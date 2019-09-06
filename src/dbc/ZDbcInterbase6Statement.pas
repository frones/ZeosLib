{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, FmtBCD,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZDbcInterbase6Utils,
  ZPlainFirebirdInterbaseConstants, ZPlainFirebirdDriver, ZCompatibility,
  ZDbcLogging, ZVariant, ZMessages, ZDbcCachedResultSet;

type
  {** Implements Prepared SQL Statement for Interbase or FireBird. }
  TZInterbase6PreparedStatement = class;

  {** record for holding batch dml stmts }
  TZIBStmt = record
    Obj: TZInterbase6PreparedStatement;
    PreparedRowsOfArray: Integer;
  end;

  { TZAbstractInterbase6PreparedStatement }
  TZAbstractInterbase6PreparedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FResultXSQLDA: IZSQLDA; //the out param or resultset Interface
    FIBConnection: IZInterbase6Connection; //the IB/FB connection interface
    FParamSQLData: IZParamsSQLDA;//the in param Interface
    FParamXSQLDA: PXSQLDA;
    FPlainDriver: TZInterbasePlainDriver; //the api holder object of the provider
    FCodePageArray: TWordDynArray; //an array of codepages
    FStatusVector: TARRAY_ISC_STATUS; //the errorcode vector
    FStmtHandle: TISC_STMT_HANDLE; //the smt handle
    FStatementType: TZIbSqlStatementType; //the stmt type
    FTypeTokens: TRawByteStringDynArray;
    FBatchStmts: array[Boolean] of TZIBStmt;
    FMaxRowsPerBatch, FMemPerRow: Integer;
    FDB_CP_ID: Integer;
    procedure ExecuteInternal;
    procedure ExceuteBatch;
  protected
    procedure CheckParameterIndex(var Value: Integer); override;
    function GetInParamLogValue(Index: Integer): RawByteString; override;
    procedure ReleaseConnection; override;
    function CreateResultSet: IZResultSet;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure AfterClose; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  TZInterbase6PreparedStatement = class(TZAbstractInterbase6PreparedStatement, IZPreparedStatement)
  private
    procedure EncodePData(XSQLVAR: PXSQLVAR; Value: PAnsiChar; Len: LengthInt);
    procedure SetPAnsiChar(Index: Word; Value: PAnsiChar; Len: LengthInt);
    procedure SetPWideChar(Index: Word; Value: PWideChar; Len: LengthInt);
    procedure WriteLobBuffer(XSQLVAR: PXSQLVAR; Buffer: Pointer; Len: LengthInt);

    procedure InternalBindDouble(XSQLVAR: PXSQLVAR; const Value: Double);
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
  public //setters
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
    procedure SetNull(Index: Integer; {%H-}SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; Value: Boolean);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    procedure SetWord(Index: Integer; Value: Word);
    procedure SetSmall(Index: Integer; Value: SmallInt);
    procedure SetUInt(Index: Integer; Value: Cardinal);
    procedure SetInt(Index: Integer; Value: Integer);
    procedure SetULong(Index: Integer; const Value: UInt64);
    procedure SetLong(Index: Integer; const Value: Int64);
    procedure SetFloat(Index: Integer; Value: Single);
    procedure SetDouble(Index: Integer; const Value: Double);
    procedure SetCurrency(Index: Integer; const Value: Currency);
    procedure SetBigDecimal(Index: Integer; const Value: TBCD);

    procedure SetCharRec(Index: Integer; const Value: TZCharRec); reintroduce;
    procedure SetString(Index: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(Index: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(Index: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const Value: RawByteString); reintroduce;
    procedure SetUnicodeString(Index: Integer; const Value: ZWideString); reintroduce;

    procedure SetDate(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTime(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTimestamp(Index: Integer; const Value: TDateTime); reintroduce;

    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce;
    procedure SetGUID(Index: Integer; const Value: TGUID); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZInterbase6Statement = class(TZAbstractInterbase6PreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  end;

  TZInterbase6CallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZEncoding, ZDbcInterbase6ResultSet, ZClasses,
  ZDbcUtils, ZDbcResultSet, ZTokenizer;

procedure BindSQLDAInParameters(BindList: TZBindList;
  Stmt: TZInterbase6PreparedStatement; ArrayOffSet, ArrayItersCount: Integer);
var
  I, J, ParamIndex: Integer;
  IsNull: Boolean;
  { array DML bindings }
  ZData: Pointer; //array entry
begin
  ParamIndex := FirstDbcIndex;
  for J := ArrayOffSet to ArrayOffSet+ArrayItersCount-1 do
    for i := 0 to BindList.Count -1 do
    begin
      IsNull := IsNullFromArray(BindList[i].Value, J);
      ZData := PZArray(BindList[i].Value).VArray;
      if (ZData = nil) or (IsNull) then
        Stmt.SetNull(ParamIndex, ZDbcIntfs.stUnknown)
      else
        case TZSQLType(PZArray(BindList[i].Value).VArrayType) of
          stBoolean: Stmt.SetBoolean(ParamIndex, TBooleanDynArray(ZData)[J]);
          stByte: Stmt.SetSmall(ParamIndex, TByteDynArray(ZData)[J]);
          stShort: Stmt.SetSmall(ParamIndex, TShortIntDynArray(ZData)[J]);
          stWord: Stmt.SetInt(ParamIndex, TWordDynArray(ZData)[J]);
          stSmall: Stmt.SetSmall(ParamIndex, TSmallIntDynArray(ZData)[J]);
          stLongWord: Stmt.SetLong(ParamIndex, TLongWordDynArray(ZData)[J]);
          stInteger: Stmt.SetInt(ParamIndex, TIntegerDynArray(ZData)[J]);
          stLong: Stmt.SetLong(ParamIndex, TInt64DynArray(ZData)[J]);
          stULong: Stmt.SetLong(ParamIndex, TUInt64DynArray(ZData)[J]);
          stFloat: Stmt.SetFloat(ParamIndex, TSingleDynArray(ZData)[J]);
          stDouble: Stmt.SetDouble(ParamIndex, TDoubleDynArray(ZData)[J]);
          stCurrency: Stmt.SetCurrency(ParamIndex, TCurrencyDynArray(ZData)[J]);
          stBigDecimal: Stmt.SetBigDecimal(ParamIndex, TBCDDynArray(ZData)[J]);
          stGUID: Stmt.SetGUID(ParamIndex, TGUIDDynArray(ZData)[j]);
          stString, stUnicodeString:
                case PZArray(BindList[i].Value).VArrayVariantType of
                  vtString: Stmt.SetString(ParamIndex, TStringDynArray(ZData)[j]);
                  {$IFNDEF NO_ANSISTRING}
                  vtAnsiString: Stmt.SetAnsiString(ParamIndex, TAnsiStringDynArray(ZData)[j]);
                  {$ENDIF}
                  {$IFNDEF NO_UTF8STRING}
                  vtUTF8String: Stmt.SetUTF8String(ParamIndex, TUTF8StringDynArray(ZData)[j]);
                  {$ENDIF}
                  vtRawByteString: Stmt.SetRawByteString(ParamIndex, TRawByteStringDynArray(ZData)[j]);
                  vtUnicodeString: Stmt.SetUnicodeString(ParamIndex, TUnicodeStringDynArray(ZData)[j]);
                  vtCharRec: Stmt.SetCharRec(ParamIndex, TZCharRecDynArray(ZData)[j]);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
          stBytes:      Stmt.SetBytes(ParamIndex, TBytesDynArray(ZData)[j]);
          stDate:       Stmt.SetDate(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stTime:       Stmt.SetTime(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stTimestamp:  Stmt.SetTimestamp(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stAsciiStream,
          stUnicodeStream,
          stBinaryStream: Stmt.SetBlob(ParamIndex, TZSQLType(PZArray(BindList[i].Value).VArrayType), TInterfaceDynArray(ZData)[j] as IZBlob);
          else
            raise EZIBConvertError.Create(SUnsupportedParameterType);
        end;
      Inc(ParamIndex);
    end;
end;

{ TZAbstractInterbase6PreparedStatement }

{**
  execute the dml batch array
}
procedure TZAbstractInterbase6PreparedStatement.ExceuteBatch;
var
  AC: Boolean;
  ArrayOffSet: Integer;
begin
  AC := Connection.GetAutoCommit;
  if AC then
    Connection.SetAutoCommit(False);
  try
    ArrayOffSet := 0;
    FIBConnection.GetTrHandle; //restart transaction if required
    try
      if (FBatchStmts[True].Obj <> nil) and (BatchDMLArrayCount >= FBatchStmts[True].PreparedRowsOfArray) then
        while (ArrayOffSet+FBatchStmts[True].PreparedRowsOfArray <= BatchDMLArrayCount) do begin
          BindSQLDAInParameters(BindList, FBatchStmts[True].Obj,
            ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
          FBatchStmts[True].Obj.ExecuteInternal;
          Inc(ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
        end;
      if (FBatchStmts[False].Obj <> nil) and (ArrayOffSet < BatchDMLArrayCount) then begin
        BindSQLDAInParameters(BindList, FBatchStmts[False].Obj,
          ArrayOffSet, FBatchStmts[False].PreparedRowsOfArray);
        FBatchStmts[False].Obj.ExecuteInternal;
      end;
      if AC then
        Connection.Commit;
    except
      if AC then
        Connection.Rollback;
      raise;
    end;
  finally
    Connection.SetAutoCommit(AC);
  end;
  LastUpdateCount := BatchDMLArrayCount;
end;

procedure TZAbstractInterbase6PreparedStatement.ExecuteInternal;
var iError: ISC_STATUS;
begin
  if BatchDMLArrayCount = 0 then
    With FIBConnection do begin
      if FStatementType = stExecProc
      then iError := FPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
        @FStmtHandle, GetDialect, FParamXSQLDA, FResultXSQLDA.GetData) //expecting out params
      else iError := FPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @FStmtHandle, GetDialect, FParamXSQLDA); //not expecting a result
      if iError <> 0 then
        ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver,
          FStatusVector, Self, lcExecute, ASQL);
      LastUpdateCount := GetAffectedRows(FPlainDriver, FStmtHandle, FStatementType, Self);
    end
  else ExceuteBatch;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FIBConnection := nil;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var B: boolean;
begin
  FStmtHandle := 0;
  for B := False to True do
    if Assigned(FBatchStmts[b].Obj) then
      FBatchStmts[b].Obj.ReleaseImmediat(Sender, AError);
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZAbstractInterbase6PreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  FPlainDriver := TZInterbasePlainDriver(FIBConnection.GetIZPlainDriver.GetInstance);
  FCodePageArray := FPlainDriver.GetCodePageArray;
  FDB_CP_ID := ConSettings^.ClientCodePage^.ID;
  FCodePageArray[FDB_CP_ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FMaxRowsPerBatch := 0;
end;

function TZAbstractInterbase6PreparedStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZInterbase6XSQLDAResultSet;
  CachedResolver: TZInterbase6CachedResolver;
  CachedResultSet: TZCachedResultSet;
begin
  if FOpenResultSet <> nil then
    Result := IZResultSet(FOpenResultSet)
  else begin
    NativeResultSet := TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
      FResultXSQLDA, CachedLob, FStatementType);
    if (GetResultSetConcurrency = rcUpdatable) or (GetResultSetType <> rtForwardOnly) then
    begin
      CachedResolver  := TZInterbase6CachedResolver.Create(Self,  NativeResultSet.GetMetadata);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
    end
    else
      Result := NativeResultSet;
    NativeResultSet.TransactionResultSet := Pointer(Result);
    FOpenResultSet := Pointer(Result);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.CheckParameterIndex(
  var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then
    raise EZSQLException.Create(SInvalidInputParameterCount);
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

procedure TZAbstractInterbase6PreparedStatement.AfterClose;
begin
  if (FStmtHandle <> 0) then begin// Free statement-handle! Otherwise: Exception!
    if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_drop) <> 0 then
      CheckInterbase6Error(FPlainDriver,
          FStatusVector, Self, lcOther, 'isc_dsql_free_statement');
    FStmtHandle := 0;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Prepare;
var
  eBlock: RawByteString;
  PreparedRowsOfArray: Integer;
  TypeItem: AnsiChar;
  Buffer: array[0..7] of AnsiChar;

  procedure PrepareArrayStmt(var Slot: TZIBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        Slot.Obj.BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        Slot.Obj._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZInterbase6PreparedStatement.Create(Connection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      Slot.Obj._AddRef;
      {$ENDIF}
      Slot.Obj.FASQL := eBlock;
      Slot.Obj.BindList.Count := BindList.Count*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    eBlock := GetExecuteBlockString(FParamSQLData,
      IsParamIndex, BindList.Count, Rows, FCachedQueryRaw,
      FPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
      FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    PrepareArrayStmt(FBatchStmts[False]);
  end;
begin
  if (not Prepared) then begin
    with Self.FIBConnection do begin
    { Allocate an sql statement }
    if FStmtHandle = 0 then
      if FPlainDriver.isc_dsql_allocate_statement(@FStatusVector, GetDBHandle, @FStmtHandle) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcOther, ASQL);
      { Prepare an sql statement }
      if FPlainDriver.isc_dsql_prepare(@FStatusVector, GetTrHandle, @FStmtHandle,
          Length(ASQL), Pointer(ASQL), GetDialect, nil) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcPrepStmt, ASQL); //Check for disconnect AVZ
      { Set Statement Type }
      TypeItem := AnsiChar(isc_info_sql_stmt_type);

      { Get information about a prepared DSQL statement. }
      if FPlainDriver.isc_dsql_sql_info(@FStatusVector, @FStmtHandle, 1,
          @TypeItem, SizeOf(Buffer), @Buffer[0]) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self);

      if Buffer[0] = AnsiChar(isc_info_sql_stmt_type)
      then FStatementType := TZIbSqlStatementType(ReadInterbase6Number(FPlainDriver, Buffer[1]))
      else FStatementType := stUnknown;

      if FStatementType in [stUnknown, stGetSegment, stPutSegment, stStartTrans, stCommit, stRollback] then begin
        FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE);
        raise EZSQLException.Create(SStatementIsNotAllowed);
      end else if FStatementType in [stSelect, stExecProc, stSelectForUpdate] then begin
        FResultXSQLDA := TZSQLDA.Create(Connection);
        { Initialise ouput param and fields }
        if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, GetDialect, FResultXSQLDA.GetData) <> 0 then
          CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, ASQL);
        if FResultXSQLDA.GetData^.sqld <> FResultXSQLDA.GetData^.sqln then begin
          FResultXSQLDA.AllocateSQLDA;
          if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, GetDialect, FResultXSQLDA.GetData) <> 0 then
            CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, ASql);
        end;
        FResultXSQLDA.InitFields(False);
      end;
    end;
    inherited Prepare; //log action and prepare params
  end;
  if BatchDMLArrayCount > 0 then begin
    if FMaxRowsPerBatch = 0 then begin
      eBlock := GetExecuteBlockString(FParamSQLData,
        IsParamIndex, BindList.Count, BatchDMLArrayCount, FCachedQueryRaw,
        FPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    end else
      eBlock := '';
    if (FMaxRowsPerBatch <= BatchDMLArrayCount) and (eBlock <> '') then begin
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
      if BatchDMLArrayCount > FMaxRowsPerBatch then //final block count
        PrepareFinalChunk(BatchDMLArrayCount mod PreparedRowsOfArray);
    end else if (eBlock = '') then begin
      if (FMaxRowsPerBatch > BatchDMLArrayCount) then begin
        if (FBatchStmts[False].PreparedRowsOfArray <> BatchDMLArrayCount) then
          PrepareFinalChunk(BatchDMLArrayCount) //full block of batch
      end else
        if (BatchDMLArrayCount <> FMaxRowsPerBatch) and (FBatchStmts[False].PreparedRowsOfArray <> (BatchDMLArrayCount mod FMaxRowsPerBatch)) then
          PrepareFinalChunk(BatchDMLArrayCount mod FMaxRowsPerBatch); //final block of batch
    end else if (FBatchStmts[False].PreparedRowsOfArray <> BatchDMLArrayCount) then
      PrepareArrayStmt(FBatchStmts[False]); //full block of batch
  end;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractInterbase6PreparedStatement.Unprepare;
var b: Boolean;
begin
  for b := False to True do
    if FBatchStmts[b].Obj <> nil then begin
      FBatchStmts[b].Obj.BindList.Count := 0;
      {$IFNDEF AUTOREFCOUNT}
      FBatchStmts[b].Obj._Release;
      {$ENDIF}
      FBatchStmts[b].Obj := nil;
    end;
  FMaxRowsPerBatch := 0;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  SetLength(FTypeTokens, 0);
  inherited Unprepare;
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    if FPlainDriver.isc_dsql_free_statement(@fStatusVector, @FStmtHandle, DSQL_UNPREPARE) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcOther, 'isc_dsql_free_statement');
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
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
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
  inherited ExecutePrepared;
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
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
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

  inherited ExecuteQueryPrepared;
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
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  ExecuteInternal;
  Result := LastUpdateCount;
  if BatchDMLArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
                  CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcOther, 'isc_dsql_free_statement');
      stExecProc: { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          FOutParamResultSet := CreateResultSet;
    end;
  inherited ExecuteUpdatePrepared;
end;

function TZAbstractInterbase6PreparedStatement.GetInParamLogValue(
  Index: Integer): RawByteString;
var XSQLVAR: PXSQLVAR;
  TempDate: TZTimeStamp;
  dDT, tDT: TDateTime;
begin
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then
    Result := 'null'
  else case XSQLVAR.sqltype and not(1) of
    SQL_D_FLOAT,
    SQL_DOUBLE    : Result := FloatToRaw(PDouble(XSQLVAR.sqldata)^);
    SQL_LONG      : if XSQLVAR.sqlscale = 0
                    then Result := IntToRaw(PISC_LONG(XSQLVAR.sqldata)^)
                    else Result := FloatToRaw(PISC_LONG(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_FLOAT     : Result := FloatToRaw(PSingle(XSQLVAR.sqldata)^);
    SQL_BOOLEAN   : Result := BoolToRawEx(PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0);
    SQL_BOOLEAN_FB: Result := BoolToRawEx(PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = 0
                    then Result := IntToRaw(PISC_SHORT(XSQLVAR.sqldata)^)
                    else Result := FloatToRaw(PISC_SHORT(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_QUAD,
    SQL_INT64     : if XSQLVAR.sqlscale = 0
                    then Result := IntToRaw(PISC_INT64(XSQLVAR.sqldata)^)
                    else Result := FloatToRaw(PISC_INT64(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_TEXT      : Result := SQLQuotedStr(PAnsiChar(XSQLVAR.sqldata), XSQLVAR.sqllen, AnsiChar(#39));
    SQL_VARYING   : Result := SQLQuotedStr(PAnsiChar(@PISC_VARYING(XSQLVAR.sqldata).str[0]), PISC_VARYING(XSQLVAR.sqldata).strlen, AnsiChar(#39));
    SQL_BLOB      : Result := '(LOB)';
    SQL_TYPE_TIME : begin
                      isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
                        TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                      if TryEncodeTime(TempDate.Hour, TempDate.Minute, TempDate.Second, TempDate.Fractions div 10, tDT)
                      then Result := ZSysUtils.DateTimeToRawSQLTime(tDT, ConSettings.WriteFormatSettings, True)
                      else Result := '(time)';
                    end;
    SQL_TYPE_DATE : begin
                      isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                        TempDate.Year, TempDate.Month, Tempdate.Day);
                      if TryEncodeDate(TempDate.Year,TempDate.Month, TempDate.Day, dDT)
                      then Result := ZSysUtils.DateTimeToRawSQLDate(dDT, ConSettings.WriteFormatSettings, True)
                      else Result := '(date)';
                    end;
    SQL_TIMESTAMP : begin
                      isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                        TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                      isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                        TempDate.Year, TempDate.Month, Tempdate.Day);
                      if not TryEncodeTime(TempDate.Hour, TempDate.Minute, TempDate.Second, TempDate.Fractions div 10, tDT) then
                        tDT := 0;
                      if not TryEncodeDate(TempDate.Year,TempDate.Month, TempDate.Day, dDT) then
                        dDT := 0;
                      if dDT < 0
                      then Result := ZSysUtils.DateTimeToRawSQLTimeStamp(dDT-tDT, ConSettings.WriteFormatSettings, True)
                      else Result := ZSysUtils.DateTimeToRawSQLTimeStamp(dDT+tDT, ConSettings.WriteFormatSettings, True)
                    end;
    else Result := 'unknown'
  end;
end;

function TZAbstractInterbase6PreparedStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, ParamCnt, FirstComposePos: Integer;
  ParamFound: Boolean;
  Tokens: TZTokenList;
  {$IFNDEF UNICODE}
  Tmp: String;
  List: TStrings;
  {$ELSE}
  Tmp: RawByteString;
  RemainderIdx: Integer;

  procedure Concat(const Value: RawByteString);
  begin
    FCachedQueryRaw[RemainderIdx] := FCachedQueryRaw[RemainderIdx] + Value;
    Result := Result + Value;
  end;
  {$ENDIF}

  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    IsParamIndex[High(IsParamIndex)] := Param;
    Result := Result + Value;
  end;

begin
  ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound {$IFNDEF UNICODE}or ConSettings^.AutoEncode {$ENDIF}or (FDB_CP_ID = CS_NONE) then begin
    ParamCnt := 0;
    Result := '';
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    {$IFNDEF UNICODE}
    if ConSettings^.AutoEncode
    then List := TStringList.Create
    else List := nil; //satisfy compiler
    {$ENDIF}
    try
      FirstComposePos := 0;
      FTokenMatchIndex := -1;
      {$IFDEF UNICODE}RemainderIdx := -1;{$ENDIF}
      for I := 0 to Tokens.Count -1 do begin
        if ParamFound and Tokens.IsEqual(I, Char('?')) then begin
          if (FirstComposePos < Tokens.Count-1) then begin
            {$IFDEF UNICODE}
            Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, FClientCP);
            if RemainderIdx = -1
            then Add(Tmp)
            else begin
              Concat(Tmp);
              RemainderIdx := -1;
            end;
            {$ELSE}
            Add(Tokens.AsString(FirstComposePos, I-1));
            {$ENDIF}
          end;
          {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
          Add(ZUnicodeToRaw(Tokens.AsString(I, I), ConSettings^.ClientCodePage^.CP));
          {$ELSE}
          Add('?', True);
          {$ENDIF}
          Inc(ParamCnt);
          FirstComposePos := i +1;
        end
        {$IFNDEF UNICODE}
        else if ConSettings.AutoEncode then
          case (Tokens[i].TokenType) of
            ttQuoted, ttComment,
            ttWord: with Tokens[i]^ do begin
              if (FDB_CP_ID = CS_NONE) and ( //all identifiers collate unicode_fss if CS_NONE
                 (Tokens[i].TokenType = ttQuotedIdentifier) or
                 ((Tokens[i].TokenType = ttWord) and (Tokens[i].L > 1) and (Tokens[i].P^ = '"')))
              then Tmp := ZConvertStringToRawWithAutoEncode(Tokens.AsString(i), ConSettings^.CTRL_CP, zCP_UTF8)
              else Tmp := ConSettings^.ConvFuncs.ZStringToRaw(Tokens.AsString(i), ConSettings^.CTRL_CP, FClientCP);
              Tokens[i].P := Pointer(tmp);
              Tokens[i].L := Length(tmp);
              List.Add(Tmp); //keep alive
            end;
        end
        {$ELSE}
        else if (FDB_CP_ID = CS_NONE) and (//all identifiers collate unicode_fss if CS_NONE
                 (Tokens[i].TokenType = ttQuotedIdentifier) or
                 ((Tokens[i].TokenType = ttWord) and (Tokens[i].L > 1) and (Tokens[i].P^ = '"'))) then begin
          if (FirstComposePos < I) then begin
            Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, FClientCP);
            if RemainderIdx = -1
            then Add(Tmp)
            else Concat(Tmp);
          end;
          RemainderIdx := High(FCachedQueryRaw);
          Tmp := PUnicodeToRaw(Tokens[i].P, Tokens[i].L, zCP_UTF8);
          Concat(Tmp);
          FirstComposePos := I +1;
        end;
        {$ENDIF};
      end;
      if (FirstComposePos < Tokens.Count) then
        {$IFDEF UNICODE}
        if RemainderIdx <> -1 then begin
          I := Tokens.Count -1;
          Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, (Tokens[i].P - Tokens[FirstComposePos].P)+Tokens[i].L, FClientCP);
          Concat(Tmp);
        end else {$ENDIF}
        Add(ConSettings^.ConvFuncs.ZStringToRaw(Tokens.AsString(FirstComposePos, Tokens.Count -1), ConSettings^.CTRL_CP, FClientCP));
    finally
      Tokens.Free;
      {$IFNDEF UNICODE}
      if ConSettings^.AutoEncode then
        List.Free;
      {$ENDIF}
    end;
    SetBindCapacity(ParamCnt);
  end else
    Result := ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
end;

procedure TZInterbase6PreparedStatement.EncodePData(XSQLVAR: PXSQLVAR;
  Value: PAnsiChar; Len: LengthInt);
begin
  if Len > XSQLVAR.sqllen then begin
    FreeMem(XSQLVAR.sqldata, XSQLVAR.sqllen+SizeOf(Short));
    XSQLVAR.sqllen := ((((Len-1) shr 3)+1) shl 3);
    GetMem(XSQLVAR.sqldata, XSQLVAR.sqllen+SizeOf(Short));
  end;
  PISC_VARYING(XSQLVAR.sqldata).strlen := Len;
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, PISC_VARYING(XSQLVAR.sqldata).str[0], Len);
end;

procedure TZInterbase6PreparedStatement.InternalBindDouble(XSQLVAR: PXSQLVAR;
  const Value: Double);
var TimeStamp: TZTimeStamp;
begin
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Value;
    SQL_LONG      : if XSQLVAR.sqlscale = 0
                    then PISC_LONG(XSQLVAR.sqldata)^ := Round(Value)
                    else PISC_LONG(XSQLVAR.sqldata)^ := Round(Value*IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = 0
                    then PISC_SHORT(XSQLVAR.sqldata)^ := Round(Value)
                    else PISC_SHORT(XSQLVAR.sqldata)^ := Round(Value*IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_INT64,
    SQL_QUAD      : if XSQLVAR.sqlscale = 0
                    then PISC_INT64(XSQLVAR.sqldata)^ := Round(Value)
                    else PISC_INT64(XSQLVAR.sqldata)^ := Round(Value*IBScaleDivisor[XSQLVAR.sqlscale]);
    SQL_TYPE_DATE : begin
                      DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      isc_encode_date(PISC_DATE(XSQLVAR.sqldata)^, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                    end;
    SQL_TYPE_TIME : begin
                      DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                      TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                      isc_encode_time(PISC_TIME(XSQLVAR.sqldata)^, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                    end;
    SQL_TIMESTAMP : begin
                      DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      isc_encode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                      TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                      isc_encode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  With FIBConnection do begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection);
    FParamXSQLDA := FParamSQLData.GetData;
    if FParamXSQLDA.sqln < BindList.Capacity then begin
      FParamXSQLDA.sqld := BindList.Capacity;
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
    end;
    {check dynamic sql}
    if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect, FParamXSQLDA) <> 0 then
      ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcExecute, ASQL);

    //alloc space for lobs, arrays, param-types
    if ((FStatementType = stExecProc) and (FResultXSQLDA.GetFieldCount > 0)) or
       ((FStatementType = stSelect) and (BindList.HasOutOrInOutOrResultParam))
    then SetParamCount(FParamXSQLDA^.sqld + FResultXSQLDA.GetFieldCount)
    else SetParamCount(FParamXSQLDA^.sqld);

    { Resize XSQLDA structure if required }
    if FParamXSQLDA^.sqld <> FParamXSQLDA^.sqln then begin
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
      if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect,FParamXSQLDA) <> 0 then
        ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcExecute, ASQL);
    end;
    FParamSQLData.InitFields(True);
  end;

end;

procedure TZInterbase6PreparedStatement.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
begin
  if ParamType = pctResultSet then
    RaiseUnsupportedException;
  inherited;
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZInterbase6PreparedStatement.SetAnsiString(Index: Integer;
  const Value: AnsiString);
var XSQLVAR: PXSQLVAR;
 CS_ID: ISC_SHORT;
 L: LengthInt;
 P: PAnsiChar;
  procedure Convert(var P: PAnsiChar; Var L: LengthInt; ToCP: Word);
  begin
    FUniTemp := PRawToUnicode(P, L, ZOSCodePage); //localize it
    FRawTemp := ZUnicodeToRaw(FUniTemp, ToCP);
    P := Pointer(FRawTemp);
    if P <> nil then
      L := Length(FRawTemp)
    else begin
      L := 0;
      P := PEmptyAnsiString;
    end;
  end;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if Value <> '' then begin
    L := Length(Value);
    P := Pointer(Value);
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT,
      SQL_VARYING   : begin
                      CS_ID := XSQLVAR.sqlsubtype and 255;
                      if (CS_ID = CS_BINARY) or (FCodePageArray[CS_ID] = ZOSCodePage) then
                        EncodePData(XSQLVAR, P, L)
                      else begin
                        Convert(P, L, FCodePageArray[CS_ID]);
                        EncodePData(XSQLVAR, P, L);
                      end;
                    end;
      SQL_BLOB,
      SQL_QUAD      : if XSQLVAR.sqlsubtype = isc_blob_text then
                        if (ClientCP = ZOSCodePage) then
                          WriteLobBuffer(XSQLVAR, Pointer(Value), L)
                        else begin
                          Convert(P, L, ClientCP);
                          WriteLobBuffer(XSQLVAR, P, L)
                        end
                      else WriteLobBuffer(XSQLVAR, P, L);
      else SetPAnsiChar(Index, Pointer(Value), L);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF}

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetBigDecimal(
  Index: Integer; const Value: TBCD);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_LONG      : BCD2ScaledOrdinal(Value, XSQLVAR.sqldata, SizeOf(ISC_LONG), -XSQLVAR.sqlscale);
    SQL_SHORT     : BCD2ScaledOrdinal(Value, XSQLVAR.sqldata, SizeOf(ISC_SHORT), -XSQLVAR.sqlscale);
    SQL_INT64,
    SQL_QUAD      : BCD2ScaledOrdinal(Value, XSQLVAR.sqldata, SizeOf(ISC_INT64), -XSQLVAR.sqlscale);
    else SetDouble(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BCDToDouble(Value));
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

procedure TZInterbase6PreparedStatement.SetBlob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var
  XSQLVAR: PXSQLVAR;
  RawTemp: RawByteString;
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  SQLType := FParamSQLData.GetFieldSqlType(Index);
  if (Value = nil) then begin
    BindList.SetNull(Index, SQLType);
    P := nil;
    L := 0;//satisfy compiler
  end else begin
    BindList.Put(Index, SQLType, Value); //localize for the refcount
    if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then
      if Value.IsClob then
        P := Value.GetPAnsiChar(ConSettings^.ClientCodePage.CP)
      else begin
        RawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings);
        BindList.Put(Index, stAsciiStream, TZAbstractCLob.CreateWithData(Pointer(RawTemp),
          Length(RawTemp), ConSettings^.ClientCodePage.CP, ConSettings));
        P := IZBlob(BindList[Index].Value).GetBuffer;
      end
    else
      P := Value.GetBuffer;
    L := IZBlob(BindList[Index].Value).Length;
  end;
  if P <> nil then begin
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT,
      SQL_VARYING   : EncodePData(XSQLVAR, P, L);
      SQL_BLOB,
      SQL_QUAD      : WriteLobBuffer(XSQLVAR, P, L);
      else raise EZIBConvertError.Create(SUnsupportedDataType);
    end;
    if (XSQLVAR.sqlind <> nil) then
       XSQLVAR.sqlind^ := ISC_NOTNULL;
  end else if (XSQLVAR.sqlind <> nil) then
       XSQLVAR.sqlind^ := ISC_NULL;

end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetBoolean(
  Index: Integer; Value: Boolean);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Ord(Value);
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Ord(Value);
    SQL_LONG      : if Value
                    then PISC_LONG(XSQLVAR.sqldata)^ := IBScaleDivisor[XSQLVAR.sqlscale]
                    else PISC_LONG(XSQLVAR.sqldata)^ := 0;
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value);
    SQL_SHORT     : if Value
                    then PISC_SHORT(XSQLVAR.sqldata)^ := IBScaleDivisor[XSQLVAR.sqlscale]
                    else PISC_SHORT(XSQLVAR.sqldata)^ := 0;
    SQL_INT64,
    SQL_QUAD      : if Value
                    then PISC_INT64(XSQLVAR.sqldata)^ := IBScaleDivisor[XSQLVAR.sqlscale]
                    else PISC_INT64(XSQLVAR.sqldata)^ := 0;
    SQL_TEXT,
    SQL_VARYING   : begin
                      PByte(@fABuffer[0])^ := Ord('0')+Ord(Value);
                      EncodePData(XSQLVAR, @fABuffer[0], 1);
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetByte(Index: Integer;
  Value: Byte);
begin
  SetSmall(Index, Value);
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
var XSQLVAR: PXSQLVAR;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  L := Length(Value);
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, Pointer(Value), L);
    SQL_LONG      : if L=SizeOf(Integer)
                    then PISC_LONG(XSQLVAR.sqldata)^ := PISC_LONG(Value)^
                    else PISC_LONG(XSQLVAR.sqldata)^ := Round(RawToFloat(PAnsiChar(Pointer(Value)), AnsiChar('.')) * IBScaleDivisor[XSQLVAR.sqlscale]); //AVZ
    SQL_SHORT     : if L=SizeOf(Integer)
                    then PISC_SHORT(XSQLVAR.sqldata)^ := PSmallInt(Value)^
                    else PISC_SHORT(XSQLVAR.sqldata)^ := RawToInt(BytesToStr(Value));
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(PAnsiChar(Value), PAnsiChar(Value)+L));
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(PAnsiChar(Value), PAnsiChar(Value)+L));
    SQL_D_FLOAT,
    SQL_DOUBLE    : if L=SizeOf(Double)
                    then PDouble(XSQLVAR.sqldata)^ := PDouble(Value)^
                    else PDouble(XSQLVAR.sqldata)^ := RawToFloat(PAnsiChar(Pointer(Value)), AnsiChar('.'))  * IBScaleDivisor[XSQLVAR.sqlscale]; //AVZ
    SQL_FLOAT     : if L=SizeOf(Single)
                    then PDouble(XSQLVAR.sqldata)^ := PSingle(Value)^
                    else PSingle (XSQLVAR.sqldata)^ := RawToFloat(PAnsiChar(Pointer(Value)), AnsiChar('.')) * IBScaleDivisor[XSQLVAR.sqlscale];  //AVZ
    SQL_INT64     : if L=SizeOf(Int64)
                    then PISC_INT64(XSQLVAR.sqldata)^ := PISC_INT64(Value)^
                    else PISC_INT64(XSQLVAR.sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloat(PAnsiChar(Pointer(Value)), AnsiChar('.')) * IBScaleDivisor[XSQLVAR.sqlscale]); //AVZ - INT64 value was not recognized
    SQL_BLOB,
    SQL_QUAD      : WriteLobBuffer(XSQLVAR, Pointer(Value), L);
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetCharRec(Index: Integer;
  const Value: TZCharRec);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value.CP = zCP_UTF16 then
    SetPWideChar(Index, Value.P, Value.Len)
  else begin
    {$R-}
    XSQLVAR := @FParamXSQLDA.sqlvar[Index];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if (Value.CP = ClientCP) or ((FDB_CP_ID = CS_NONE) and
     ((XSQLVAR.sqltype and not(1) = SQL_TEXT) or (XSQLVAR.sqltype and not(1) = SQL_VARYING)) and
      (FCodePageArray[XSQLVAR.sqlsubtype and 255] = Value.CP))
    then SetPAnsiChar(Index, Value.P, Value.Len)
    else begin
      FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP); //localize it
      if FUniTemp <> ''
      then SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp))
      else SetPWideChar(Index, PEmptyUnicodeString, 0);
    end;
  end;
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var XSQLVAR: PXSQLVAR;
  i64: Int64 absolute Value;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Value;
    SQL_LONG      : if XSQLVAR.sqlscale = -4 then  //scale fits!
                      PISC_LONG(XSQLVAR.sqldata)^ := I64
                    else if XSQLVAR.sqlscale > -4 then //EH: check the modulo?
                      PISC_LONG(XSQLVAR.sqldata)^ := I64 div IBScaleDivisor[-4-XSQLVAR.sqlscale] //dec scale digits
                    else
                      PISC_LONG(XSQLVAR.sqldata)^ := I64 * IBScaleDivisor[4+XSQLVAR.sqlscale]; //inc scale digits
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = -4 then  //scale fits!
                      PISC_SHORT(XSQLVAR.sqldata)^ := I64
                    else if XSQLVAR.sqlscale > -4 then //EH: check the modulo?
                      PISC_SHORT(XSQLVAR.sqldata)^ := I64 div IBScaleDivisor[-4-XSQLVAR.sqlscale] //dec scale digits
                    else
                      PISC_SHORT(XSQLVAR.sqldata)^ := I64 * IBScaleDivisor[4+XSQLVAR.sqlscale]; //inc scale digits
    SQL_INT64,
    SQL_QUAD      : if XSQLVAR.sqlscale = -4 then //scale fits!
                      PISC_INT64(XSQLVAR.sqldata)^ := I64
                    else if XSQLVAR.sqlscale > -4 then //EH: check the modulo?
                      PISC_INT64(XSQLVAR.sqldata)^ := I64 div IBScaleDivisor[-4-XSQLVAR.sqlscale]//dec scale digits
                    else
                      PISC_INT64(XSQLVAR.sqldata)^ := I64 * IBScaleDivisor[4+XSQLVAR.sqlscale]; //inc scale digits
    SQL_TEXT,
    SQL_VARYING   : begin
                      CurrToRaw(Value, @fABuffer[0], @P);
                      EncodePData(XSQLVAR, @fABuffer[0], P-PAnsiChar(@fABuffer[0]));
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetDate(Index: Integer;
  const Value: TDateTime);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, @fABuffer,
                      DateTimeToRawSQLDate(Value, @fABuffer, ConSettings^.WriteFormatSettings, False));
    else InternalBindDouble(XSQLVAR, Value);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, @fABuffer[0], FloatToSqlRaw(Value, @fABuffer[0]));
    else InternalBindDouble(XSQLVAR, Value);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetFloat(Index: Integer;
  Value: Single);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, @fABuffer[0], FloatToSqlRaw(Value, @fABuffer[0]));
    else InternalBindDouble(XSQLVAR, Value);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetGUID(Index: Integer;
  const Value: TGUID);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : if XSQLVAR.sqlsubtype = CS_BINARY then
                      EncodePData(XSQLVAR, @Value.D1, SizeOf(TGUID))
                    else begin
                      //see https://firebirdsql.org/refdocs/langrefupd25-intfunc-uuid_to_char.html
                      GUIDToBuffer(@Value.D1, PAnsiChar(@fABuffer), []);
                      EncodePData(XSQLVAR, @fABuffer, 36)
                    end;
    SQL_BLOB,
    SQL_QUAD      : if XSQLVAR.sqlsubtype = CS_BINARY then
                      WriteLobBuffer(XSQLVAR, @Value.D1, SizeOf(TGUID))
                    else begin
                      GUIDToBuffer(@Value.D1, PAnsiChar(@fABuffer), []);
                      WriteLobBuffer(XSQLVAR, @fABuffer, 36)
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetInt(Index, Value: Integer);
var XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Value;
    SQL_LONG      : if XSQLVAR.sqlscale = 0
                    then PISC_LONG(XSQLVAR.sqldata)^ := Value
                    else PISC_LONG(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = 0
                    then PISC_SHORT(XSQLVAR.sqldata)^ := Value
                    else PISC_SHORT(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_INT64,
    SQL_QUAD      : if XSQLVAR.sqlscale = 0
                    then PISC_INT64(XSQLVAR.sqldata)^ := Value
                    else PISC_INT64(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      IntToRaw(Value, @fABuffer[0], @P);
                      EncodePData(XSQLVAR, @fABuffer[0], P-@fABuffer[0]);
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>unsigned longlong</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
var XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Value;
    SQL_LONG      : if XSQLVAR.sqlscale = 0
                    then PISC_LONG(XSQLVAR.sqldata)^ := Value
                    else PISC_LONG(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = 0
                    then PISC_SHORT(XSQLVAR.sqldata)^ := Value
                    else PISC_SHORT(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_INT64,
    SQL_QUAD      : if XSQLVAR.sqlscale = 0
                    then PISC_INT64(XSQLVAR.sqldata)^ := Value
                    else PISC_INT64(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      IntToRaw(Value, @fABuffer[0], @P);
                      EncodePData(XSQLVAR, @fABuffer[0], P-@fABuffer[0]);
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZInterbase6PreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) then
    XSQLVAR.sqlind^ := ISC_NULL;
end;

{**
   Set up parameter PAnsiChar value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZInterbase6PreparedStatement.SetPAnsiChar(Index: Word;
  Value: PAnsiChar; Len: LengthInt);
var
  TempTimeStamp: TDateTime;
  TimeStamp: TZTimeStamp;
  Failed: Boolean;
  XSQLVAR: PXSQLVAR;
begin
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT      : EncodePData(XSQLVAR, Value, Len);
    SQL_VARYING   : EncodePData(XSQLVAR, Value, Len);
    SQL_LONG      : PISC_LONG (XSQLVAR.sqldata)^ := RawToIntDef(Value, Value+Len, 0);
    SQL_SHORT     : PISC_SHORT (XSQLVAR.sqldata)^ := RawToIntDef(Value, Value+Len,0);
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_D_FLOAT,
    SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(XSQLVAR.sqldata)^, Len);
    SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (XSQLVAR.sqldata)^, Len);
    SQL_INT64     : PISC_INT64(XSQLVAR.sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[XSQLVAR.sqlscale], 0)); //AVZ - INT64 value was not recognized
    SQL_BLOB, SQL_QUAD: WriteLobBuffer(XSQLVAR, Value, Len);
    SQL_TYPE_DATE :
      begin
        if (Len = 0) or (PByte(Value+2)^ = Ord(':')) then
          TempTimeStamp := 0
        else if Len = ConSettings^.WriteFormatSettings.DateFormatLen then
          TempTimeStamp := RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := Int(RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
        DecodeDate(TempTimeStamp, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        isc_encode_date(PISC_DATE(XSQLVAR.sqldata)^, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
      end;
    SQL_TYPE_TIME:
      begin
        if Len = 0 then
          TempTimeStamp := 0
        else if PByte(Value+2)^ = Ord(':') then //possible date if Len = 10 then
          TempTimeStamp := RawSQLTimeToDateTime(Value,Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := Frac(RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
        DecodeTime(TempTimeStamp, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
        isc_encode_time(PISC_TIME(XSQLVAR.sqldata)^, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
      end;
    SQL_TIMESTAMP:
      begin
        if Len = 0 then
          TempTimeStamp := 0
        else if PByte(Value+2)^ = Ord(':') then
          TempTimeStamp := RawSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed)
        else if (ConSettings^.WriteFormatSettings.DateTimeFormatLen - Len) <= 4 then
          TempTimeStamp := RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed)
        else if PByte(Value+4)^ = Ord('-') then
          TempTimeStamp := RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := RawSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed);
        DecodeDate(TempTimeStamp, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        isc_encode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        DecodeTime(TempTimeStamp, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
        isc_encode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
      end;
    else raise EZIBConvertError.Create(SErrorConvertion);
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

procedure TZInterbase6PreparedStatement.SetPWideChar(Index: Word;
  Value: PWideChar; Len: LengthInt);
var
  TempTimeStamp: TDateTime;
  TimeStamp: TZTimeStamp;
  Failed: Boolean;
  XSQLVAR: PXSQLVAR;
begin
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : begin
                      if XSQLVAR.sqlsubtype and 255 <> CS_BINARY then
                        if (FDB_CP_ID <> CS_NONE)
                        then FRawTemp := PUnicodeToRaw(Value, Len, FClientCP)
                        else FRawTemp := PUnicodeToRaw(Value, Len, FCodePageArray[XSQLVAR.sqlsubtype and 255])
                      else FRawTemp := UnicodeStringToAscii7(Value, Len);
                      if FRawTemp <> ''
                      then EncodePData(XSQLVAR, Pointer(FRawTemp), Length(FRawTemp))
                      else EncodePData(XSQLVAR, PEmptyAnsiString, 0)
                    end;
    SQL_LONG      : PISC_LONG(XSQLVAR.sqldata)^ := UnicodeToIntDef(Value, Value+Len, 0);
    SQL_SHORT     : PISC_SHORT(XSQLVAR.sqldata)^ := UnicodeToIntDef(Value, Value+Len,0);
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_D_FLOAT,
    SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(XSQLVAR.sqldata)^, Len);
    SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (XSQLVAR.sqldata)^, Len);
    SQL_INT64     : PISC_INT64(XSQLVAR.sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[XSQLVAR.sqlscale], 0)); //AVZ - INT64 value was not recognized
    SQL_BLOB,
    SQL_QUAD      : begin
                      if XSQLVAR.sqlsubtype = isc_blob_text
                      then FRawTemp := PUnicodeToRaw(Value, Len, FClientCP)
                      else FRawTemp := UnicodeStringToAscii7(Value, Len);
                      if FRawTemp <> ''
                      then WriteLobBuffer(XSQLVAR, Pointer(FRawTemp), Length(FRawTemp))
                      else WriteLobBuffer(XSQLVAR, PEmptyAnsiString, 0)
                    end;
    SQL_TYPE_DATE :
      begin
        if (Len = 0) or (PByte(Value+2)^ = Ord(':')) then
          TempTimeStamp := 0
        else if Len = ConSettings^.WriteFormatSettings.DateFormatLen then
          TempTimeStamp := UnicodeSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := Int(UnicodeSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
        DecodeDate(TempTimeStamp, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        isc_encode_date(PISC_DATE(XSQLVAR.sqldata)^, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
      end;
    SQL_TYPE_TIME:
      begin
        if Len = 0 then
          TempTimeStamp := 0
        else if PByte(Value+2)^ = Ord(':') then //possible date if Len = 10 then
          TempTimeStamp := UnicodeSQLTimeToDateTime(Value,Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := Frac(UnicodeSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
        DecodeTime(TempTimeStamp, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
        isc_encode_time(PISC_TIME(XSQLVAR.sqldata)^, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
      end;
    SQL_TIMESTAMP:
      begin
        if Len = 0 then
          TempTimeStamp := 0
        else if PByte(Value+2)^ = Ord(':') then
          TempTimeStamp := UnicodeSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed)
        else if (ConSettings^.WriteFormatSettings.DateTimeFormatLen - Len) <= 4 then
          TempTimeStamp := UnicodeSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed)
        else if PByte(Value+4)^ = Ord('-') then
          TempTimeStamp := UnicodeSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed)
        else
          TempTimeStamp := UnicodeSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed);
        DecodeDate(TempTimeStamp, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        isc_encode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        DecodeTime(TempTimeStamp, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
        isc_encode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
      end;
    else raise EZIBConvertError.Create(SErrorConvertion);
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>raw encoded string</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetRawByteString(
  Index: Integer; const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPAnsiChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetShort(Index: Integer;
  Value: ShortInt);
begin
  SetSmall(Index, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetSmall(
  Index: Integer; Value: SmallInt);
var XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_FLOAT     : PSingle(XSQLVAR.sqldata)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(XSQLVAR.sqldata)^   := Value;
    SQL_LONG      : if XSQLVAR.sqlscale = 0
                    then PISC_LONG(XSQLVAR.sqldata)^ := Value
                    else PISC_LONG(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_BOOLEAN   : PISC_BOOLEAN(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ := Ord(Value <> 0);
    SQL_SHORT     : if XSQLVAR.sqlscale = 0
                    then PISC_SHORT(XSQLVAR.sqldata)^ := Value
                    else PISC_SHORT(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_INT64,
    SQL_QUAD      : if XSQLVAR.sqlscale = 0
                    then PISC_INT64(XSQLVAR.sqldata)^ := Value
                    else PISC_INT64(XSQLVAR.sqldata)^ := Value*IBScaleDivisor[XSQLVAR.sqlscale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      IntToRaw(Integer(Value), @fABuffer[0], @P);
                      EncodePData(XSQLVAR, @fABuffer[0], P-@fABuffer[0]);
                    end;
    else raise EZIBConvertError.Create(SUnsupportedDataType);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetString(Index: Integer;
  const Value: String);
{$IFDEF UNICODE}
begin
  SetUnicodeString(Index, Value);
{$ELSE}
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> '' then begin
    {$R-}
    XSQLVAR := @FParamXSQLDA.sqlvar[Index];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT,
      SQL_VARYING   : if not ConSettings^.AutoEncode or (XSQLVAR.sqlsubtype and 255 = CS_BINARY) then
                        EncodePData(XSQLVAR, Pointer(Value), Length(Value))
                      else if (ClientCP <> CS_NONE)
                        then SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                          ConSettings^.ConvFuncs.ZStringToRaw( Value, ConSettings^.Ctrl_CP, ClientCP))
                        else SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                          ConSettings^.ConvFuncs.ZStringToRaw( Value, ConSettings^.Ctrl_CP, FCodePageArray[XSQLVAR.sqlsubtype and 255]));
      SQL_BLOB,
      SQL_QUAD      : if not ConSettings^.AutoEncode or (XSQLVAR.sqlsubtype <> isc_blob_text)
                      then WriteLobBuffer(XSQLVAR, Pointer(Value), Length(Value))
                      else SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                          ConSettings^.ConvFuncs.ZStringToRaw( Value, ConSettings^.Ctrl_CP, ClientCP));
      else SetPAnsiChar(Index, Pointer(Value), Length(Value));
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetTime(Index: Integer;
  const Value: TDateTime);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, @fABuffer,
                      DateTimeToRawSQLTime(Value, @fABuffer, ConSettings^.WriteFormatSettings, False));
    else InternalBindDouble(XSQLVAR, Value);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetTimestamp(
  Index: Integer; const Value: TDateTime);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  XSQLVAR := @FParamXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  case (XSQLVAR.sqltype and not(1)) of
    SQL_TEXT,
    SQL_VARYING   : EncodePData(XSQLVAR, @fABuffer,
                      DateTimeToRawSQLTimeStamp(Value, @fABuffer, ConSettings^.WriteFormatSettings, False));
    else InternalBindDouble(XSQLVAR, Value);
  end;
  if (XSQLVAR.sqlind <> nil) then
     XSQLVAR.sqlind^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a Java <code>usigned int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetUInt(Index: Integer;
  Value: Cardinal);
begin
  SetLong(Index, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned longlong</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZInterbase6PreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
begin
  SetLong(Index, Value);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Java <code>UnicodeString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetUnicodeString(
  Index: Integer; const Value: ZWideString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPWideChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0);
end;

{**
  Sets the designated parameter to a Java <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZInterbase6PreparedStatement.SetUTF8String(Index: Integer;
  const Value: UTF8String);
var XSQLVAR: PXSQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> '' then begin
    {$R-}
    XSQLVAR := @FParamXSQLDA.sqlvar[Index];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT,
      SQL_VARYING   : if (ClientCP = zCP_UTF8) or (XSQLVAR.sqlsubtype and 255 = CS_BINARY)
                        or ((FDB_CP_ID = CS_NONE) and (FCodePageArray[XSQLVAR.sqlsubtype and 255] = zCP_UTF8)) then
                        EncodePData(XSQLVAR, Pointer(Value), Length(Value))
                      else begin
                        FUniTemp := PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8); //localize it
                        if (FDB_CP_ID = CS_NONE)
                        then FRawTemp := ZUnicodeToRaw(FUniTemp, FCodePageArray[XSQLVAR.sqlsubtype and 255])
                        else FRawTemp := ZUnicodeToRaw(FUniTemp, ClientCP);
                        if FRawTemp <> ''
                        then EncodePData(XSQLVAR, Pointer(FRawTemp), Length(FRawTemp))
                        else EncodePData(XSQLVAR, PEmptyAnsiString, 0);
                      end;
      SQL_BLOB,
      SQL_QUAD      : if XSQLVAR.sqlsubtype = isc_blob_text then
                        if (ClientCP = zCP_UTF8) then
                          WriteLobBuffer(XSQLVAR, Pointer(Value), Length(Value))
                        else begin
                          FUniTemp := PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8); //localize it
                          FRawTemp := ZUnicodeToRaw(FUniTemp, ClientCP);
                          if FRawTemp <> ''
                          then WriteLobBuffer(XSQLVAR, Pointer(FRawTemp), Length(FRawTemp))
                          else WriteLobBuffer(XSQLVAR, PEmptyAnsiString, 0);
                        end
                      else WriteLobBuffer(XSQLVAR, Pointer(Value), Length(Value));
      else SetPAnsiChar(Index, Pointer(Value), Length(Value));
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF NO_UTF8STRING}

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZInterbase6PreparedStatement.SetWord(Index: Integer;
  Value: Word);
begin
  SetInt(Index, Value);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZInterbase6PreparedStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then begin
    FParamSQLData.FreeParamtersValues;
    FParamXSQLDA := nil;
  end;
  inherited UnPrepareInParameters;
end;

procedure TZInterbase6PreparedStatement.WriteLobBuffer(XSQLVAR: PXSQLVAR;
  Buffer: Pointer; Len: LengthInt);
var
  BlobId: TISC_QUAD;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
  CurPos, SegLen: Integer;
  TempBuffer: PAnsiChar;
begin
  BlobHandle := 0;

  { create blob handle }
  with FIBConnection do
    if FPlainDriver.isc_create_blob2(@StatusVector, GetDBHandle, GetTrHandle,
      @BlobHandle, @BlobId, 0, nil) <> 0 then
    CheckInterbase6Error(FPlainDriver, StatusVector, Self);

  { put data to blob }
  TempBuffer := Buffer;
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < Len) do begin
    if (CurPos + SegLen > Len) then
      SegLen := Len - CurPos;
    if FPlainDriver.isc_put_segment(@StatusVector, @BlobHandle, SegLen, TempBuffer) <> 0 then
      CheckInterbase6Error(FPlainDriver, StatusVector, Self);
    Inc(CurPos, SegLen);
    Inc(TempBuffer, SegLen);
  end;

  { close blob handle }
  if FPlainDriver.isc_close_blob(@StatusVector, @BlobHandle) <> 0 then
    CheckInterbase6Error(FPlainDriver, StatusVector, Self);
  PISC_QUAD(XSQLVAR.sqldata)^ := BlobId;
end;

{ TZInterbase6Statement }

constructor TZInterbase6Statement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  P: PChar;
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
begin
  SQL := '';
  if (Connection as IZInterbase6Connection).StoredProcedureIsSelectable(StoredProcName)
  then ToBuff('SELECT * FROM ', SQL)
  else ToBuff('EXECUTE PROCEDURE ', SQL);
  ToBuff(StoredProcName, SQL);
  ToBuff('(', SQL);
  for I := 0 to BindList.Capacity -1 do
    if not (BindList.ParamTypes[I] in [pctOut,pctReturn]) then
      ToBuff('?,', SQL);
  FlushBuff(SQL);
  P := Pointer(SQL);
  if (BindList.Capacity > 0) and ((P+Length(SQL)-1)^ = ',')
  then (P+Length(SQL)-1)^ := ')' //cancel last comma
  else (P+Length(SQL)-1)^ := ' '; //cancel bracket
  Result := TZInterbase6PreparedStatement.Create(Connection, SQL, Info);
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
