{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZDbcFirebirdStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} FmtBCD, Types, SysUtils,
  ZCompatibility, ZPlainFirebird, ZPlainFirebirdInterbaseDriver,
  ZDbcIntfs, ZDbcStatement, ZDbcFirebirdInterbase, ZDbcFirebird,
  ZDbcInterbase6Utils;

type
  {** Implements a abstract Statement for Firebird. }
  TZAbstractFirebirdStatement = Class(TZAbstractFirebirdInterbasePreparedStatement)
  private
    FMaxRowsPerBatch: Integer;
    FFBConnection: IZFirebirdConnection;
    FAttachment: IAttachment;
    FFBStatement: IStatement;
    FFBTransaction: ITransaction;
    FStatus: IStatus;
    FStatementType: TZIbSqlStatementType;
    FInMessageMetadata, FOutMessageMetadata: IMessageMetadata;
    FPlainDriver: TZFirebird3UpPlainDriver;
    FResultSet: IResultSet;
  protected
    procedure WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt); override;
    function CreateConversionError(Index: Cardinal; Current: TZSQLType): EZSQLException; override;
  protected
    function CreateResultSet: IZResultSet;
    procedure ExecuteInternal;
  protected
    procedure PrepareInParameters; override;
  public
    Constructor Create(const Connection: IZFirebirdConnection;
      const SQL: String; Info: TStrings);
    Destructor Destroy; override;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  {** Implements a regular Statement for Firebird. }

  TZFirebirdStatement = Class(TZAbstractFirebirdStatement)
  public
    Constructor Create(const Connection: IZFirebirdConnection;
      Info: TStrings);
  end;

  {** Implements IZPreparedStatement for Firebird. }
  TZFirebirdPreparedStatement = class(TZAbstractFirebirdStatement,
    IZPreparedStatement);

  {** Implements IZCallableStatement for Firebird. }
  TZFirebirdCallableStatement = class(TZAbstractInterbaseFirebirdCallableStatement)
  protected
    function InternalCreateExecutionStatement(const Connection: IZInterbaseFirebirdConnection;
      const SQL: String; Info: TStrings): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_FIREBIRD}
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD}

uses ZMessages, ZSysUtils, ZFastCode, ZEncoding, ZVariant,
  ZDbcLogging, ZDbcFirebirdResultSet, ZDbcResultSet, ZDbcCachedResultSet,
  ZDbcUtils, ZDbcProperties;

const
  EBStart = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('EXECUTE BLOCK(');
  EBBegin =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}(')AS BEGIN'+LineEnding);
  EBSuspend =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('SUSPEND;'+LineEnding); //required for RETURNING syntax
  EBEnd = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('END');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
  cRETURNING: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('RETURNING');
function GetExecuteBlockString(const Stmt: TZAbstractFirebirdStatement;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  var PreparedRowsOfArray,MaxRowsPerBatch: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: Cardinal): RawByteString;
var
  IndexName, ArrayName, tmp: RawByteString;
  ParamIndex, J: Cardinal;
  I, BindCount, ParamNameLen, SingleStmtLength, LastStmLen,
  HeaderLen, FullHeaderLen, StmtLength:  Integer;
  CodePageInfo: PZCodePage;
  PStmts, PResult, P: PAnsiChar;
  ReturningFound: Boolean;
  MemPerRow: Cardinal;

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
  BindCount := Stmt.FInMessageMetadata.GetCount(Stmt.FStatus);
  MemPerRow := XSQLDA_LENGTH(BindCount);
  if Pointer(TypeTokens) = nil then
  begin
    Assert(InParamCount=BindCount, 'ParamCount missmatch');
    SetLength(TypeTokens, BindCount);
    for ParamIndex := 0 to BindCount-1 do
    begin
      case Stmt.FInMessageMetadata.GetType(Stmt.FStatus, ParamIndex) and not (1) of
        SQL_VARYING, SQL_TEXT:
          begin
            CodePageInfo := Stmt.FPlainDriver.ValidateCharEncoding(Word(Stmt.FInMessageMetadata.GetCharSet(Stmt.FStatus, ParamIndex)) and 255);
            AddParam([' VARCHAR(', IntToRaw(Stmt.FInMessageMetadata.GetLength(Stmt.FStatus, ParamIndex) div Cardinal(CodePageInfo.CharWidth)),
            ') CHARACTER SET ', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(CodePageInfo.Name), '=?' ], TypeTokens[ParamIndex]);
          end;
        SQL_DOUBLE, SQL_D_FLOAT:
           AddParam([' DOUBLE PRECISION=?'], TypeTokens[ParamIndex]);
        SQL_FLOAT:
           AddParam([' FLOAT=?'],TypeTokens[ParamIndex]);
        SQL_LONG:
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' INTEGER=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(9,', Tmp,')=?'], TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(9,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_SHORT:
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' SMALLINT=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(4,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(4,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_TIMESTAMP:
           AddParam([' TIMESTAMP=?'],TypeTokens[ParamIndex]);
        SQL_BLOB:
          if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = isc_blob_text then
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
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' BIGINT=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(18,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(18,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
           AddParam([' BOOLEAN=?'],TypeTokens[ParamIndex]);
        SQL_NULL{FB25}:
           AddParam([' CHAR(1)=?'],TypeTokens[ParamIndex]);
      end;
    end;
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
      MaxRowsPerBatch := {Min(}Integer(XSQLDAMaxSize div MemPerRow);//,     {memory limit of XSQLDA structs}
        //Integer(((32*1024)-LBlockLen) div (HeaderLen+SingleStmtLength)))+1; {32KB limited Also with FB3};
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if //(PreparedRowsOfArray = MaxRowsPerBatch-1) or
       ((InitialStatementType = stInsert) and (PreparedRowsOfArray = 254)) or
       ((InitialStatementType <> stInsert) and (PreparedRowsOfArray = 124)) then begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      MaxRowsPerBatch := J;
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

{ TZAbstractFirebirdStatement }

constructor TZAbstractFirebirdStatement.Create(
  const Connection: IZFirebirdConnection; const SQL: String; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FFBConnection := Connection;
  FAttachment := Connection.GetAttachment;
  FAttachment.addRef;
  FStatus := Connection.GetStatus;
  FPlainDriver := FFBConnection.GetPlainDriver;
end;

{**
  destroys this object and releases all memory
}
function TZAbstractFirebirdStatement.CreateConversionError(Index: Cardinal;
  Current: TZSQLType): EZSQLException;
var Expected: TZSQLType;
    AType, ASubType: Cardinal;
    Scale: Integer;
begin
  AType := FInMessageMetadata.getType(FStatus, Index);
  ASubType := FInMessageMetadata.getSubType(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  Expected := ConvertIB_FBType2SQLType(AType, ASubType, Scale);
  Result := ZDbcUtils.CreateConversionError(Index, Current, Expected);
end;

function TZAbstractFirebirdStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZAbstractFirebirdResultSet;
  CachedResolver: TZInterbaseFirebirdCachedResolver;
  CachedResultSet: TZCachedResultSet;
begin
  if FOpenResultSet <> nil then
    Result := IZResultSet(FOpenResultSet)
  else begin
    if FResultSet <> nil
    then NativeResultSet := TZFirebirdResultSet.Create(Self, SQL, FOutMessageMetadata, FStatus, FOutData, @FResultSet)
    else NativeResultSet := TZFirebirdOutParamResultSet.Create(Self, SQL, FOutMessageMetadata, FStatus, FOutData);
    { EH: i have noticed several exception if i use a scrollable cursor ... }
    if ((GetResultSetType <> rtForwardOnly) or (GetResultSetConcurrency = rcUpdatable)) and (FResultSet <> nil) then begin
      NativeResultSet.SetType(rtForwardOnly);
      CachedResolver := TZFirebird2upCachedResolver.Create(Self, NativeResultSet.GetMetadata);
      if CachedLob
      then CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings)
      else CachedResultSet := TZFirebirdCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(rcUpdatable);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    NativeResultSet.TransactionResultSet := Pointer(Result);
    NativeResultSet.RegisterCursor;
    FOpenResultSet := Pointer(Result);
  end;
end;

destructor TZAbstractFirebirdStatement.Destroy;
begin
  inherited Destroy;
  FAttachment.release;
end;

procedure TZAbstractFirebirdStatement.ExecuteInternal;
var flags: Cardinal;
begin
  if BatchDMLArrayCount = 0 then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcBindPrepStmt,Self);
    RestartTimer;
    FFBTransaction := FFBConnection.GetActiveTransaction.GetTransaction;
    if FStatementType in [stSelect, stSelectForUpdate] then begin
      (* commented, somesting in fblclient is killing our skack/heap for some selects
       using a scrollable cursor..
      if (GetResultSetType <> rtForwardOnly) and (GetResultSetConcurrency = rcReadOnly)
      then flags := {$IFDEF WITH_CLASS_CONST}IStatement.CURSOR_TYPE_SCROLLABLE{$ELSE}IStatement_CURSOR_TYPE_SCROLLABLE{$ENDIF}
      else *)flags := 0;
      FResultSet := FFBStatement.openCursor(FStatus, FFBTransaction,
        FInMessageMetadata, FInData, FOutMessageMetadata, flags)
    end else FFBTransaction := FFBStatement.execute(FStatus, FFBTransaction,
      FInMessageMetadata, FInData, FOutMessageMetadata, FOutData);
    if ((FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
       ((FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0)  then
      FFBConnection.HandleErrorOrWarning(lcExecPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), fASQL, Self);
  end else ExceuteBatch;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractFirebirdStatement.ExecutePrepared: Boolean;
begin
  LastUpdateCount := -1;
  Prepare;
  PrepareLastResultSetForReUse;
  ExecuteInternal;
  { Create ResultSet if possible else free Statement Handle }
  if (FStatementType in [stSelect, stExecProc, stSelectForUpdate]) and (FOutMessageMetadata <> nil) then begin
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
  end else
    LastResultSet := nil;
  LastUpdateCount := FFBStatement.getAffectedRecords(FStatus);
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
function TZAbstractFirebirdStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;

  if (FOutMessageMetadata <> nil) then begin
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
  or -1 for SQL statements that return nothing
}
function TZAbstractFirebirdStatement.ExecuteUpdatePrepared: Integer;
begin
  LastUpdateCount := -1;
  Prepare;
  LastResultSet := nil;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;
  if BatchDMLArrayCount = 0 then begin
    case FStatementType of
      stSelect, stSelectForUpdate: if BindList.HasOutParam then begin
          FOutParamResultSet := CreateResultSet;
          FOpenResultSet := nil;
        end else if FResultSet <> nil then begin
          FResultSet.Close(FStatus);
          if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
            FFBConnection.HandleErrorOrWarning(lcExecPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), fASQL, Self);
          FResultSet.Release;
        end;
      stExecProc: begin{ Create ResultSet if possible }
          if FOutMessageMetadata <> nil then
            FOutParamResultSet := CreateResultSet;
          LastUpdateCount := FFBStatement.getAffectedRecords(FStatus)
        end;
      stInsert, stUpdate, stDelete: LastUpdateCount := FFBStatement.getAffectedRecords(FStatus);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end;
  Result := LastUpdateCount;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  prepares the statement on the server
}
procedure TZAbstractFirebirdStatement.Prepare;
var Transaction: ITransaction;
  flags: Cardinal;
  PreparedRowsOfArray: Integer;
  FinalChunkSize: Integer;
  TimeOut: Cardinal;
label jmpEB;
  procedure PrepareArrayStmt(var Slot: TZIB_FBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        TZFirebirdPreparedStatement(Slot.Obj).BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        TZFirebirdPreparedStatement(Slot.Obj)._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZFirebirdPreparedStatement.Create(FFBConnection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      TZFirebirdPreparedStatement(Slot.Obj)._AddRef;
      {$ENDIF}
      TZFirebirdPreparedStatement(Slot.Obj).FASQL := FRawTemp;
      TZFirebirdPreparedStatement(Slot.Obj).BindList.Count := BindList.Count*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    FRawTemp := GetExecuteBlockString(Self,
      FIsParamIndex, BindList.Count, Rows, FCachedQueryRaw,
      PreparedRowsOfArray, FMaxRowsPerBatch,
      FTypeTokens, FStatementType, FFBConnection.GetXSQLDAMaxSize);
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
  if not Prepared then begin
    RestartTimer;
    Transaction := FFBConnection.GetActiveTransaction.GetTransaction;
    if FWeakIZPreparedStatementPtr <> nil
    {$IFDEF WITH_CLASS_CONST}
    then flags := IStatement.PREPARE_PREFETCH_METADATA
    else flags := IStatement.PREPARE_PREFETCH_TYPE or IStatement.PREPARE_PREFETCH_OUTPUT_PARAMETERS;
    {$ELSE}
    then flags := IStatement_PREPARE_PREFETCH_METADATA
    else flags := IStatement_PREPARE_PREFETCH_TYPE or IStatement_PREPARE_PREFETCH_OUTPUT_PARAMETERS;
    {$ENDIF}
    FFBStatement := FAttachment.prepare(FStatus, Transaction, Length(fASQL),
      Pointer(fASQL), FDialect, flags);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FFBConnection.HandleErrorOrWarning(lcPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), fASQL, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcPrepStmt,Self);
    if FFBStatement.vTable.version > 3 then begin
      TimeOut := StrToInt(DefineStatementParameter(Self, DSProps_StatementTimeOut, '0'));
      if TimeOut <> 0 then begin
        IStatement_V4(FFBStatement).setTimeout(FStatus, TimeOut);
        if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
          FFBConnection.HandleErrorOrWarning(lcPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), fASQL, Self);
      end;
    end;
    FStatementType := TZIbSqlStatementType(FFBStatement.getType(FStatus));
    FOutMessageMetadata := FFBStatement.getOutputMetadata(FStatus);
    FOutMessageCount := FOutMessageMetadata.getCount(FStatus);
    if FOutMessageCount = 0 then begin
      FOutMessageMetadata.release;
      FOutMessageMetadata := nil;
    end else begin
      flags := FOutMessageMetadata.getMessageLength(FStatus);
      if Flags = 0 then //see TestTicket426 (even if not reproducable with FB3 client)
        Flags := SizeOf(Cardinal);
      GetMem(FOutData, flags);
    end;
    inherited Prepare;
  end;
  if BatchDMLArrayCount > 0 then begin
    //if not done already then split our query into pieces to build the
    //exceute block query
    if (FCachedQueryRaw = nil) then
      SplitQueryIntoPieces;
    if FMaxRowsPerBatch = 0 then begin //init to find out max rows per batch
jmpEB:fRawTemp := GetExecuteBlockString(Self,
        FIsParamIndex, BindList.Count, BatchDMLArrayCount, FCachedQueryRaw,
        PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FFBConnection.GetXSQLDAMaxSize);
    end else
      fRawTemp := '';
    FinalChunkSize := (BatchDMLArrayCount mod FMaxRowsPerBatch);
    if (FMaxRowsPerBatch <= BatchDMLArrayCount) and (FBatchStmts[True].Obj = nil) then begin
      if fRawTemp = '' then goto jmpEB;
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
    end;
    if (FinalChunkSize > 0) and ((FBatchStmts[False].Obj = nil) or
       (FinalChunkSize <> FBatchStmts[False].PreparedRowsOfArray)) then //if final chunk then
      PrepareFinalChunk(FinalChunkSize);
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractFirebirdStatement.PrepareInParameters;
var MessageMetadata: IMessageMetadata;
    MetadataBuilder: IMetadataBuilder;
    Index, Tmp, SubType: Cardinal;
    CS_ID: Word;
begin
  MessageMetadata := FFBStatement.getInputMetadata(FStatus);
  try
    FInMessageCount := MessageMetadata.getCount(FStatus);
    //alloc space for lobs, arrays, param-types
    if (FOutMessageMetadata <> nil) and ((FStatementType = stExecProc) or
       ((FStatementType = stSelect) and BindList.HasOutOrInOutOrResultParam))
    then BindList.SetCount(FInMessageCount + FOutMessageCount)
    else BindList.SetCount(FInMessageCount);
    if FInMessageCount > 0 then begin
      GetMem(FInParamDescripors, FInMessageCount * SizeOf(TZInterbaseFirerbirdParam));
      MetadataBuilder := MessageMetadata.getBuilder(FStatus);
      try
        {$R-}
        for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
          sqltype := MessageMetadata.getType(FStatus, Index);
          if sqltype = SQL_TEXT then //length might be zero
            sqltype := SQL_VARYING; //we don't use the fixed char fields. We don't space padd the data nor changing the sqllen
          MetadataBuilder.setType(FStatus, Index, sqltype);
          SubType := MessageMetadata.getSubType(FStatus, Index);
          MetadataBuilder.setSubType(FStatus, Index, SubType);
          sqllen := MessageMetadata.getLength(FStatus, Index);
          if sqltype = SQL_VARYING then
            sqllen := ((sqllen shr 2) + 1) shl 2; //4Byte align incluing 4 bytes reserved for overlongs {let fb raise the Exception}
          MetadataBuilder.setLength(FStatus, Index, sqllen);
          sqlscale := MessageMetadata.getScale(FStatus, Index);
          MetadataBuilder.setScale(FStatus, Index, sqlscale);
          Tmp := MessageMetadata.getCharSet(FStatus, Index);
          MetadataBuilder.setCharSet(FStatus, Index, Tmp);
          if ((sqltype = SQL_BLOB) and (SubType = isc_blob_text)) or (sqltype = SQL_VARYING) then begin
            CS_ID := Word(Tmp) and 255;
            CodePage := FCodePageArray[CS_ID]
          end else CodePage := zCP_Binary
        end;
        FInMessageMetadata := MetadataBuilder.getMetadata(FStatus);
      finally
        MetadataBuilder.release;
      end;
      Tmp := FInMessageMetadata.getMessageLength(FStatus);
      GetMem(FInData, Tmp);
      {$R-}
      for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        sqlind := PISC_SHORT(PAnsiChar(FInData)+ FInMessageMetadata.getNullOffset(FStatus, Index));
        sqldata := PAnsiChar(FInData)+ FInMessageMetadata.getOffset(FStatus, Index);
      end;
    end;
  finally
    MessageMetadata.release;
  end;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractFirebirdStatement.Unprepare;
begin
  inherited Unprepare;
  if FInMessageMetadata <> nil then begin
    FInMessageMetadata.release;
    FInMessageMetadata := nil;
  end;
  if FOutMessageMetadata <> nil then begin
    FOutMessageMetadata.release;
    FOutMessageMetadata := nil;
  end;
  if FFBStatement <> nil then begin
    FFBStatement.free(FStatus);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FFBConnection.HandleErrorOrWarning(lcUnprepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), fASQL, Self);
    FFBStatement.release;
    FFBStatement := nil;
  end;
end;

procedure TZAbstractFirebirdStatement.WriteLobBuffer(Index: Cardinal;
  P: PAnsiChar; Len: NativeUInt);
var
  CurPos: NativeUInt;
  SegLen: Cardinal;
  Attachment: IAttachment;
  Transaction: ITransaction;
  Blob: IBlob;
begin
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    { create blob handle }
    Attachment := FFBConnection.GetAttachment;
    Transaction := FFBConnection.GetActiveTransaction.GetTransaction;
    Blob := Attachment.createBlob(FStatus, Transaction, ISC_QUADPtr(sqldata), 0, nil);
    { put data to blob }
    CurPos := 0;
    SegLen := DefaultBlobSegmentSize;
    while (CurPos < Len) do begin
      if (CurPos + SegLen > Len) then
        SegLen := Len - CurPos;
      Blob.putSegment(FStatus, SegLen, P);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FFBConnection.HandleErrorOrWarning(lcBindPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.putSegment', Self);
      Inc(CurPos, SegLen);
      Inc(P, SegLen);
    end;
    { close blob handle }
    Blob.close(FStatus);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FFBConnection.HandleErrorOrWarning(lcBindPrepStmt, PARRAY_ISC_STATUS(FStatus.getErrors), 'IBlob.close', Self);
    Blob.release;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{ TZFirebirdStatement }

constructor TZFirebirdStatement.Create(const Connection: IZFirebirdConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZFirebirdCallableStatement }

function TZFirebirdCallableStatement.InternalCreateExecutionStatement(
  const Connection: IZInterbaseFirebirdConnection; const SQL: String;
  Info: TStrings): TZAbstractPreparedStatement;
begin
  Result := TZFirebirdPreparedStatement.Create(Connection as IZFirebirdConnection, SQL, Info);
end;

initialization
{$ENDIF ZEOS_DISABLE_FIREBIRD}
end.
