{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcInterbase6Statement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZDbcInterbase6Utils,
  ZPlainFirebirdInterbaseDriver, ZCompatibility,
  ZDbcFirebirdInterbase,
  ZDbcLogging, ZVariant, ZMessages, ZDbcCachedResultSet, ZDbcUtils;

type
  {** Implements Prepared SQL Statement for Interbase or FireBird. }

  { TZAbstractInterbase6PreparedStatement }
  TZAbstractInterbase6PreparedStatement = class(TZAbstractFirebirdInterbasePreparedStatement)
  private
    FResultXSQLDA: IZSQLDA; //the out param or resultset Interface
    FIBConnection: IZInterbase6Connection; //the IB/FB connection interface
    FParamSQLData: IZParamsSQLDA;//the in param Interface
    FParamXSQLDA: PXSQLDA;
    FPlainDriver: TZInterbasePlainDriver; //the api holder object of the provider
    FStatusVector: TARRAY_ISC_STATUS; //the errorcode vector
    FStmtHandle: TISC_STMT_HANDLE; //the smt handle
    FMaxRowsPerBatch, FMemPerRow: Integer;
    procedure ExecuteInternal;
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
  protected
    procedure ReleaseConnection; override;
    function CreateResultSet: IZResultSet;
    procedure WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt); override;
    function CreateConversionError(Index: Cardinal; Current: TZSQLType): EZSQLException; override;
  public
    constructor Create(const Connection: IZInterbase6Connection; const SQL: string; Info: TStrings);
    procedure AfterClose; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  TZInterbase6PreparedStatement = class(TZAbstractInterbase6PreparedStatement,
    IZPreparedStatement);

  TZInterbase6Statement = class(TZAbstractInterbase6PreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZInterbase6Connection; Info: TStrings);
  end;

  TZInterbase6CallableStatement = class(TZAbstractInterbaseFirebirdCallableStatement)
  protected
    function InternalCreateExecutionStatement(const Connection: IZInterbaseFirebirdConnection;
      const SQL: String; Info: TStrings): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} Types, Math,
  ZSysUtils, ZFastCode, ZEncoding, ZDbcInterbase6ResultSet,
  ZDbcResultSet, ZDbcProperties;

const
  EBStart = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('EXECUTE BLOCK(');
  EBBegin =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}(')AS BEGIN'+LineEnding);
  EBSuspend =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('SUSPEND;'+LineEnding); //required for RETURNING syntax
  EBEnd = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('END');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
  cRETURNING: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('RETURNING');
function GetExecuteBlockString(const ParamsSQLDA: IZParamsSQLDA;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: TZInterbasePlainDriver;
  var MemPerRow, PreparedRowsOfArray,MaxRowsPerBatch: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: Integer): RawByteString;
var
  IndexName, ArrayName, Tmp: RawByteString;
  ParamIndex, J: Cardinal;
  I, BindCount, ParamNameLen, SingleStmtLength, LastStmLen,
  HeaderLen, FullHeaderLen, StmtLength:  Integer;
  CodePageInfo: PZCodePage;
  PStmts, PResult, P: PAnsiChar;
  ReturningFound: Boolean;

  procedure Put(const Args: array of RawByteString; var Dest: PAnsiChar);
  var I: Integer;
    L: LengthInt;
  begin
    for I := low(Args) to high(Args) do //Move data
      if Pointer(Args[i]) <> nil then begin
        L := {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^;
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Args[i])^, Dest^, L);
        Inc(Dest, L);
      end;
  end;
  procedure AddParam(const Args: array of RawByteString; var Dest: RawByteString);
  var I, L: Integer;
    P: PAnsiChar;
  begin
    Dest := ''; L := 0;
    for I := low(Args) to high(Args) do //Calc String Length
      Inc(L ,Length(Args[i]));
    SetLength(Dest, L);
    P := Pointer(Dest);
    Put(Args, P);
  end;
begin
  if Pointer(TypeTokens) = nil then
  begin
    BindCount := ParamsSQLDA.GetFieldCount;
    Assert(InParamCount=BindCount, 'ParamCount missmatch');
    SetLength(TypeTokens, BindCount);
    MemPerRow := 0;
    for ParamIndex := 0 to BindCount-1 do
    begin
      case ParamsSQLDA.GetIbSqlType(ParamIndex) and not (1) of
        SQL_VARYING, SQL_TEXT:
          begin
            CodePageInfo := PlainDriver.ValidateCharEncoding(ParamsSQLDA.GetIbSqlSubType(ParamIndex) and 255);
            AddParam([' VARCHAR(', IntToRaw(ParamsSQLDA.GetIbSqlLen(ParamIndex) div CodePageInfo.CharWidth),
            ') CHARACTER SET ', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(CodePageInfo.Name), '=?' ], TypeTokens[ParamIndex]);
          end;
        SQL_DOUBLE, SQL_D_FLOAT:
           AddParam([' DOUBLE PRECISION=?'], TypeTokens[ParamIndex]);
        SQL_FLOAT:
           AddParam([' FLOAT=?'],TypeTokens[ParamIndex]);
        SQL_LONG:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' INTEGER=?'],TypeTokens[ParamIndex])
          else begin
            Tmp := IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex));
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(9,', Tmp,')=?'], TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(9,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_SHORT:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' SMALLINT=?'],TypeTokens[ParamIndex])
          else begin
            Tmp := IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex));
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(4,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(4,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_TIMESTAMP:
           AddParam([' TIMESTAMP=?'],TypeTokens[ParamIndex]);
        SQL_BLOB:
          if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = isc_blob_text then
            AddParam([' BLOB SUB_TYPE TEXT=?'],TypeTokens[ParamIndex])
          else
            AddParam([' BLOB=?'],TypeTokens[ParamIndex]);
        //SQL_ARRAY                      = 540;
        //SQL_QUAD                       = 550;
        SQL_TYPE_TIME:
           AddParam([' TIME=?'],TypeTokens[ParamIndex]);
        SQL_TYPE_DATE:
           AddParam([' DATE=?'],TypeTokens[ParamIndex]);
        SQL_INT64: // IB7
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' BIGINT=?'],TypeTokens[ParamIndex])
          else begin
            Tmp := IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex));
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(18,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(18,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
           AddParam([' BOOLEAN=?'],TypeTokens[ParamIndex]);
        SQL_NULL{FB25}:
           AddParam([' CHAR(1)=?'],TypeTokens[ParamIndex]);
      end;
      (*Inc(MemPerRow, ParamsSQLDA.GetFieldLength(ParamIndex) +
        2*Ord((ParamsSQLDA.GetIbSqlType(ParamIndex) and not 1) = SQL_VARYING));*)
    end;
    Inc(MemPerRow, XSQLDA_LENGTH(InParamCount));
  end;
  {now let's calc length of stmt to know if we can bound all array data or if we need some more calls}
  StmtLength := 0;
  FullHeaderLen := 0;
  ReturningFound := False;
  PreparedRowsOfArray := 0;

  for J := 0 to RemainingArrayRows -1 do
  begin
    ParamIndex := 0;
    SingleStmtLength := 0;
    LastStmLen := StmtLength;
    HeaderLen := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin //calc Parameters size
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        ParamNameLen := {P}1+GetOrdinalDigits(ParamIndex)+1{_}+GetOrdinalDigits(j);
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        {inc header}
        Inc(HeaderLen, ParamNameLen+ {%H-}PLengthInt(NativeUInt(TypeTokens[ParamIndex]) - StringLenOffSet)^+Ord(not ((ParamIndex = 0) and (J=0))){,});
        {inc stmt}
        Inc(SingleStmtLength, 1+{:}ParamNameLen);
        Inc(ParamIndex);
      end else begin
        Inc(SingleStmtLength, {%H-}PLengthInt(NativeUInt(CurrentSQLTokens[i]) - StringLenOffSet)^);
        P := Pointer(CurrentSQLTokens[i]);
        if not ReturningFound and (Ord(P^) in [Ord('R'), Ord('r')]) and (Length(CurrentSQLTokens[i]) = Length(cRETURNING)) then begin
          ReturningFound := ZSysUtils.SameText(P, Pointer(cReturning), Length(cRETURNING));
          Inc(StmtLength, Ord(ReturningFound)*Length(EBSuspend));
        end;
      end;
    end;
    Inc(SingleStmtLength, 1{;}+Length(LineEnding));
    if MaxRowsPerBatch = 0 then //calc maximum batch count if not set already
      MaxRowsPerBatch := Min(Integer(XSQLDAMaxSize div MemPerRow),     {memory limit of XSQLDA structs}
        Integer(((32*1024)-LBlockLen) div (HeaderLen+SingleStmtLength)))+1; {32KB limited Also with FB3};
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if (PreparedRowsOfArray = MaxRowsPerBatch-1) or
       ((InitialStatementType = stInsert) and (PreparedRowsOfArray > 255)) or
       ((InitialStatementType <> stInsert) and (PreparedRowsOfArray > 125)) then begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      Break;
    end else
      PreparedRowsOfArray := J;
  end;

  {EH: now move our data to result ! ONE ALLOC ! of result (: }
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, StmtLength+LBlockLen);
  PResult := Pointer(Result);
  Put([EBStart], PResult);
  PStmts := PResult + FullHeaderLen+Length(EBBegin);
  for J := 0 to PreparedRowsOfArray do begin
    ParamIndex := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin
        IndexName := IntToRaw(ParamIndex);
        ArrayName := IntToRaw(J);
        Put([':P', IndexName, '_', ArrayName], PStmts);
        if (ParamIndex = 0) and (J=0)
        then Put(['P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult)
        else Put([',P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult);
        Inc(ParamIndex);
      end else
        Put([CurrentSQLTokens[i]], PStmts);
    end;
    Put([';',LineEnding], PStmts);
  end;
  Put([EBBegin], PResult);
  if ReturningFound then
    Put([EBSuspend], PStmts);
  Put([EBEnd], PStmts);
  Inc(PreparedRowsOfArray);
end;

{ TZAbstractInterbase6PreparedStatement }

type
  TCountType = (cntSel, cntIns, cntDel, cntUpd);

procedure TZAbstractInterbase6PreparedStatement.ExecuteInternal;
var iError: ISC_STATUS;
  ISC_TR_HANDLE: PISC_TR_HANDLE;
  dialect: Word;
var
  ReqInfo: AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Len, Item, Count: Integer;
  Counts: array[TCountType] of Integer;
begin
  if BatchDMLArrayCount = 0 then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcBindPrepStmt,Self);
    RestartTimer;
    ISC_TR_HANDLE := FIBConnection.GetTrHandle;
    dialect := FIBConnection.GetDialect;
    if (FStatementType = stExecProc)
    then iError := FPlainDriver.isc_dsql_execute2(@FStatusVector, ISC_TR_HANDLE,
      @FStmtHandle, Dialect, FParamXSQLDA, FResultXSQLDA.GetData) //expecting out params
    else iError := FPlainDriver.isc_dsql_execute(@FStatusVector, ISC_TR_HANDLE,
      @FStmtHandle, Dialect, FParamXSQLDA); //not expecting a result
    if (iError <> 0) or (FStatusVector[2] = isc_arg_warning) then
      FIBConnection.HandleErrorOrWarning(lcExecPrepStmt, @FStatusVector, SQL, Self);
    LastUpdateCount := -1;
    if FStatementType <> stDDL then begin
      ReqInfo := AnsiChar(isc_info_sql_records);

      if FPlainDriver.isc_dsql_sql_info(@FStatusVector, @FStmtHandle, 1,
          @ReqInfo, SizeOf(TByteBuffer), PAnsiChar(FByteBuffer)) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_sql_info'{$ELSE}''{$ENDIF}, Self);
      if FByteBuffer[0] <> isc_info_sql_records then
        Exit;

      pBufStart := @FByteBuffer[1];
      pBuf := pBufStart;
      Len := FPlainDriver.isc_vax_integer(pBuf, 2) + 2;
      Inc(pBuf, 2);
      if FByteBuffer[Len] <> isc_info_end then
        Exit;

      FillChar(Counts{%H-}, SizeOf(Counts), #0);
      while pBuf - pBufStart <= Len do
      begin
        Item := Byte(pBuf^);

        if Item = isc_info_end then
          Break;

        Inc(pBuf);
        Count := ReadInterbase6NumberWithInc(FPlainDriver, pBuf);

        case Item of
          isc_info_req_select_count: Counts[cntSel] := Count;
          isc_info_req_insert_count: Counts[cntIns] := Count;
          isc_info_req_update_count: Counts[cntUpd] := Count;
          isc_info_req_delete_count: Counts[cntDel] := Count;
          else
            raise EZSQLException.Create(SInternalError);
        end;
      end;

      { Note: Update statements could have Select counter <> 0 as well }

      case FStatementType of
        stSelect, //selectable procedure could have a update count but FB does not return them.
        stSelectForUpdate:LastUpdateCount := Counts[cntSel];
        stInsert:         LastUpdateCount := Counts[cntIns];
        stUpdate:         LastUpdateCount := Counts[cntUpd];
        stDelete:         LastUpdateCount := Counts[cntDel];
        stExecProc: begin
                          { Exec proc could have any counter... So search for the first non-zero counter }
                          LastUpdateCount := Counts[cntIns];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntUpd];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntDel];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntSel];
                        end;
        else            LastUpdateCount := -1;
      end;
    end;
  end else ExceuteBatch;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FIBConnection := nil;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FStmtHandle := 0;
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZAbstractInterbase6PreparedStatement.Create(const Connection: IZInterbase6Connection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FIBConnection := Connection;
  FPlainDriver := TZInterbasePlainDriver(FIBConnection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FMaxRowsPerBatch := 0;
end;

function TZAbstractInterbase6PreparedStatement.CreateConversionError(
  Index: Cardinal; Current: TZSQLType): EZSQLException;
var Expected: TZSQLType;
begin
  Expected := Self.FParamSQLData.GetFieldSqlType(Index);
  Result := ZDbcUtils.CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
    Current, Expected)
end;

function TZAbstractInterbase6PreparedStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZInterbase6XSQLDAResultSet;
  CachedResolver: TZInterbaseFirebirdCachedResolver;
  CachedResultSet: TZCachedResultSet;
begin
  if FOpenResultSet <> nil then
    Result := IZResultSet(FOpenResultSet)
  else begin
    NativeResultSet := TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
      FResultXSQLDA, FStatementType);
    if (GetResultSetConcurrency = rcUpdatable) or (GetResultSetType <> rtForwardOnly) then begin
      if FIBConnection.IsFirebirdLib and (FIBConnection.GetHostVersion >= 2000000) //is the SQL2003 st. IS DISTINCT FROM supported?
      then CachedResolver  := TZFirebird2upCachedResolver.Create(Self, NativeResultSet.GetMetadata)
      else CachedResolver  := TZInterbaseFirebirdCachedResolver.Create(Self, NativeResultSet.GetMetadata);
      if CachedLob
      then CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings)
      else CachedResultSet := TZInterbaseCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    NativeResultSet.TransactionResultSet := Pointer(Result);
    FOpenResultSet := Pointer(Result);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.AfterClose;
begin
  if (FStmtHandle <> 0) then begin// Free statement-handle! Otherwise: Exception!
    if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_drop) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_dsql_free_statement', FIBConnection);
    FStmtHandle := 0;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Prepare;
var
  eBlock: RawByteString;
  PreparedRowsOfArray: Integer;
  TypeItem: AnsiChar;
  Buffer: array[0..7] of AnsiChar;
  FinalChunkSize: Integer;
  L,MaxLen: LengthInt;
  Mem, Index: Cardinal;
  XSQLVAR: PXSQLVAR;
  XSQLDA: PXSQLDA;
  P: PAnsiChar;
  Status: ISC_STATUS;
label jmpEB;

  procedure PrepareArrayStmt(var Slot: TZIB_FBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        TZInterbase6PreparedStatement(Slot.Obj).BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        TZInterbase6PreparedStatement(Slot.Obj)._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZInterbase6PreparedStatement.Create(FIBConnection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      TZInterbase6PreparedStatement(Slot.Obj)._AddRef;
      {$ENDIF}
      TZInterbase6PreparedStatement(Slot.Obj).FASQL := eBlock;
      TZInterbase6PreparedStatement(Slot.Obj).BindList.Count := BindList.Count*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    eBlock := GetExecuteBlockString(FParamSQLData,
      FIsParamIndex, BindList.Count, Rows, FCachedQueryRaw,
      FPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
      FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    PrepareArrayStmt(FBatchStmts[False]);
  end;
  procedure SplitQueryIntoPieces;
  var CurrentCS_ID: Integer;
  begin
    CurrentCS_ID := FDB_CP_ID;
    try
      FDB_CP_ID := CS_NONE;
      GetRawEncodedSQL(SQL);
    finally
      FDB_CP_ID := CurrentCS_ID;
    end;
  end;
begin
  if (not Prepared) then begin
    with FIBConnection do begin
    { Allocate an sql statement }
    if FStmtHandle = 0 then
      if FPlainDriver.isc_dsql_allocate_statement(@FStatusVector, GetDBHandle, @FStmtHandle) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_dsql_allocate_statement', Self);
      { Prepare an sql statement }
      //get overlong string running:
      //see request https://zeoslib.sourceforge.io/viewtopic.php?f=40&p=147689#p147689
      //http://tracker.firebirdsql.org/browse/CORE-1117?focusedCommentId=31493&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#action_31493
      L := Length(ASQL);
      if L > High(Word) then begin//test word range overflow
        if ZFastCode.Pos(RawByteString(#0), ASQL) > 0 then
          raise EZSQLException.Create('Statements longer than 64KB may not contain the #0 character.');
        MaxLen := GetConnection.GetMetadata.GetDatabaseInfo.GetMaxStatementLength;
        if L > MaxLen then
          raise Exception.Create('Statements longer than ' + ZFastCode.IntToStr(MaxLen) + ' bytes are not supported by your database.');
        L := 0; //fall back to C-String behavior
      end;
      Status := FPlainDriver.isc_dsql_prepare(@FStatusVector, GetTrHandle, @FStmtHandle,
          Word(L), Pointer(ASQL), GetDialect, nil);
      if (Status <> 0) or (FStatusVector[2] = isc_arg_warning) then
        FIBConnection.HandleErrorOrWarning(lcPrepStmt, @FStatusVector, SQL, Self);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcPrepStmt,Self);
      if Assigned(FPlainDriver.fb_dsql_set_timeout) then begin
        Mem := StrToInt(DefineStatementParameter(Self, DSProps_StatementTimeOut, '0'));
        if Mem <> 0 then
          if FPlainDriver.fb_dsql_set_timeout(@FStatusVector, @FStmtHandle, Mem) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'fb_dsql_set_timeout', Self);
      end;
      { Set Statement Type }
      TypeItem := AnsiChar(isc_info_sql_stmt_type);

      { Get information about a prepared DSQL statement. }
      if FPlainDriver.isc_dsql_sql_info(@FStatusVector, @FStmtHandle, 1,
          @TypeItem, SizeOf(Buffer), @Buffer[0]) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcPrepStmt, @FStatusVector, SQL, Self);

      if Buffer[0] = AnsiChar(isc_info_sql_stmt_type)
      then FStatementType := TZIbSqlStatementType(ReadInterbase6Number(FPlainDriver, Buffer[1]))
      else FStatementType := stUnknown;

      if FStatementType in [stUnknown, stGetSegment, stPutSegment, stStartTrans, stCommit, stRollback] then begin
        FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE);
        raise EZSQLException.Create(SStatementIsNotAllowed);
      end else if FStatementType in [stSelect, stExecProc, stSelectForUpdate] then begin
        FResultXSQLDA := TZSQLDA.Create(Connection, ConSettings);
        { Initialise ouput param and fields }
        if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, GetDialect, FResultXSQLDA.GetData) <> 0 then
          FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe'{$ELSE}''{$ENDIF}, Self);
        if FResultXSQLDA.GetData^.sqld <> FResultXSQLDA.GetData^.sqln then begin
          FResultXSQLDA.AllocateSQLDA;
          if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, GetDialect, FResultXSQLDA.GetData) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe'{$ELSE}''{$ENDIF}, Self);
        end;
        FOutMessageCount := FResultXSQLDA.GetData.sqld;
        if FOutMessageCount > 0 then begin
          XSQLDA := FResultXSQLDA.GetData;
          Mem := 0;
          {$R-}
          for Index := 0 to FOutMessageCount -1 do begin
            XSQLVAR := @XSQLDA.sqlvar[Index];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
            Mem := mem + XSQLVAR.sqllen;
            if XSQLVAR.sqltype and not (1) = SQL_VARYING then
              Mem := mem + SizeOf(ISC_USHORT);
            if XSQLVAR.sqltype and 1 = 1 then //nullable?
              Mem := mem + SizeOf(ISC_SHORT); //null indicator;
          end;
          if Mem = 0 then //see TestTicket426
            Mem := SizeOf(Cardinal);
          GetMem(FOutData, Mem); //alloc space as one block
          P := FOutData;
          {$R-}
          for Index := 0 to FOutMessageCount -1 do begin
            XSQLVAR := @XSQLDA.sqlvar[Index];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
            XSQLVAR.sqldata := P;
            Inc(P, XSQLVAR.sqllen);
            if XSQLVAR.sqltype and not (1) = SQL_VARYING then
              Inc(P, SizeOf(ISC_SHORT));
            if XSQLVAR.sqltype and 1 = 1 then begin //nullable?
              XSQLVAR.sqlind := PISC_SHORT(P);
              Inc(P, SizeOf(ISC_SHORT));
            end else
              XSQLVAR.sqlind := nil;
          end;
        end;
      end;
    end;
    inherited Prepare; //log action and prepare params
  end;
  if BatchDMLArrayCount > 0 then begin
    //if not done already then split our query into pieces to build the
    //exceute block query
    if (FCachedQueryRaw = nil) then
      SplitQueryIntoPieces;
    if FMaxRowsPerBatch = 0 then begin //init to find out max rows per batch
jmpEB:eBlock := GetExecuteBlockString(FParamSQLData,
        FIsParamIndex, BindList.Count, BatchDMLArrayCount, FCachedQueryRaw,
        FPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    end else
      eBlock := '';
    FinalChunkSize := (BatchDMLArrayCount mod FMaxRowsPerBatch);
    if (FMaxRowsPerBatch <= BatchDMLArrayCount) and (FBatchStmts[True].Obj = nil) then begin
      if eBlock = '' then goto jmpEB;
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
    end;
    if (FinalChunkSize > 0) and ((FBatchStmts[False].Obj = nil) or
       (FinalChunkSize <> FBatchStmts[False].PreparedRowsOfArray)) then //if final chunk then
      PrepareFinalChunk(FinalChunkSize);
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
  Index, Mem: Cardinal;
  XSQLVAR: PXSQLVAR;
  CS_ID: Word;
  P: PAnsiChar;
begin
  With FIBConnection do begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection, ConSettings);
    FParamXSQLDA := FParamSQLData.GetData;
    if FParamXSQLDA.sqln < BindList.Capacity then begin
      FParamXSQLDA.sqld := BindList.Capacity;
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
    end;
    {check dynamic sql}
    if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect, FParamXSQLDA) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe_bind'{$ELSE}''{$ENDIF}, Self);

    //alloc space for lobs, arrays, param-types
    if ((FStatementType = stExecProc) and (FResultXSQLDA.GetFieldCount > 0)) or
       ((FStatementType = stSelect) and (BindList.HasOutOrInOutOrResultParam))
    then SetParamCount(FParamXSQLDA^.sqld + FResultXSQLDA.GetFieldCount)
    else SetParamCount(FParamXSQLDA^.sqld);

    { Resize XSQLDA structure if required }
    if FParamXSQLDA^.sqld <> FParamXSQLDA^.sqln then begin
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
      if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect,FParamXSQLDA) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe_bind'{$ELSE}''{$ENDIF}, Self);
    end;
    FInMessageCount := FParamXSQLDA^.sqld;
    GetMem(FInParamDescripors, FInMessageCount * SizeOf(TZInterbaseFirerbirdParam));
    if FInMessageCount > 0 then begin
      Mem := 0;
      {$R-}
      for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        XSQLVAR := @FParamXSQLDA.sqlvar[Index];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        sqltype := XSQLVAR.sqltype and not (1);
        if sqltype = SQL_TEXT then begin //length might be zero
          //we don't use the fixed char fields. We don't space padd the data nor changing the sqllen
          XSQLVAR.sqltype := SQL_VARYING;
          sqltype := SQL_VARYING;
        end;
        if sqltype = SQL_VARYING then begin
          sqllen := ((XSQLVAR.sqllen shr 2) + 1) shl 2; //4Byte align incluing 4 bytes reserved for overlongs {let fb raise the Exception}
          XSQLVAR.sqllen := sqllen;
          Mem := mem + XSQLVAR.sqllen + SizeOf(ISC_SHORT);
        end else
          Mem := mem + XSQLVAR.sqllen;
        sqlscale := XSQLVAR.sqlscale;
        if ((sqltype = SQL_BLOB) and (XSQLVAR.sqlsubtype = isc_blob_text)) or (sqltype = SQL_VARYING) then begin
          CS_ID := XSQLVAR.sqlsubtype and 255;
          CodePage := FCodePageArray[CS_ID]
        end else CodePage := zCP_Binary;
        XSQLVAR.sqltype := XSQLVAR.sqltype or 1;
        Mem := mem + SizeOf(ISC_SHORT); //null indicator;
      end;
      GetMem(FInData, Mem); //alloc space as one block
      P := FInData;
      { write offset addresses we scribble in }
      {$R-}
      for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        XSQLVAR := @FParamXSQLDA.sqlvar[Index];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        sqldata := P;
        sqllen := XSQLVAR.sqllen;
        sqlscale := XSQLVAR.sqlscale;
        XSQLVAR.sqldata := P;
        Inc(P, sqllen);
        if sqltype = SQL_VARYING then begin
          Inc(P, SizeOf(ISC_SHORT));
          codepage := FCodePageArray[XSQLVAR.sqlsubtype and 255];
        end else if (sqltype = SQL_BLOB) and (XSQLVAR.sqlsubtype = isc_blob_text) then
          codepage := ConSettings.ClientCodePage.CP;
        sqlind := PISC_SHORT(P);
        XSQLVAR.sqlind := PISC_SHORT(P);
        Inc(P, SizeOf(ISC_SHORT));
      end;
    end;
  end;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractInterbase6PreparedStatement.Unprepare;
begin
  FMaxRowsPerBatch := 0;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  SetLength(FTypeTokens, 0);
  inherited Unprepare;
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    if FPlainDriver.isc_dsql_free_statement(@fStatusVector, @FStmtHandle, DSQL_UNPREPARE) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcUnprepStmt, @FStatusVector, SQL, Self);
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcUnprepStmt,Self);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractInterbase6PreparedStatement.UnPrepareInParameters;
begin
  inherited UnPrepareInParameters;
  FParamXSQLDA := nil;
end;

procedure TZAbstractInterbase6PreparedStatement.WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt);
var
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
  CurPos, SegLen: Integer;
begin
  BlobHandle := 0;
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    { create blob handle }
    with FIBConnection do
      if FPlainDriver.isc_create_blob2(@StatusVector, GetDBHandle, GetTrHandle,
        @BlobHandle, PISC_QUAD(sqldata), 0, nil) <> 0 then
      HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'create lob', Self);

    { put data to blob }
    CurPos := 0;
    SegLen := DefaultBlobSegmentSize;
    while (NativeUint(CurPos) < Len) do begin
      if (NativeUInt(CurPos + SegLen) > Len) then
        SegLen := Len - NativeUint(CurPos);
      if FPlainDriver.isc_put_segment(@StatusVector, @BlobHandle, SegLen, P) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'write lob', Self);
      Inc(CurPos, SegLen);
      Inc(P, SegLen);
    end;

    { close blob handle }
    if FPlainDriver.isc_close_blob(@StatusVector, @BlobHandle) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'close lob', Self);
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractInterbase6PreparedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  ExecuteInternal;
  { Create ResultSet if possible else free Statement Handle }
  if (FStatementType in [stSelect, stExecProc, stSelectForUpdate]) and (FResultXSQLDA.GetFieldCount <> 0) then begin
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
  end else
    LastResultSet := nil;
  Result := LastResultSet <> nil;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;
  if (FResultXSQLDA <> nil) and (FResultXSQLDA.GetFieldCount <> 0) then begin
    if (FStatementType = stSelect) and Assigned(FOpenResultSet) and not BindList.HasOutOrInOutOrResultParam
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := Result;
  end else begin
    Result := nil;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAbstractInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  LastResultSet := nil;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;
  Result := LastUpdateCount;
  if BatchDMLArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: if BindList.HasOutParam then begin
          FOutParamResultSet := CreateResultSet;
          FOpenResultSet := nil;
        end else if FResultXSQLDA.GetFieldCount <> 0 then
          if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcExecPrepStmt, @FStatusVector, SQL, Self);
      stExecProc: { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          FOutParamResultSet := CreateResultSet;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //nothing todo
    end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{ TZInterbase6Statement }

constructor TZInterbase6Statement.Create(const Connection: IZInterbase6Connection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.InternalCreateExecutionStatement(
  const Connection: IZInterbaseFirebirdConnection; const SQL: String;
  Info: TStrings): TZAbstractPreparedStatement;
begin
  Result := TZInterbase6PreparedStatement.Create(Connection as IZInterbase6Connection, SQL, Info);
end;


{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.

