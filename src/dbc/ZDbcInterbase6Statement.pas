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

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZDbcInterbase6Utils,
  ZPlainFirebirdInterbaseConstants, ZPlainFirebirdDriver, ZCompatibility,
  ZDbcLogging, ZVariant, ZMessages;

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
    FParamSQLData: IZParamsSQLDA;//the in param Interface
    FResultXSQLDA: IZSQLDA; //the out param or resultset Interface
    FIBConnection: IZInterbase6Connection; //the IB/FB connection interface
    FPlainDriver: TZInterbasePlainDriver; //the api holder object of the provider
    FCodePageArray: TWordDynArray; //an array of codepages
    FStatusVector: TARRAY_ISC_STATUS; //the errorcode vector
    FStmtHandle: TISC_STMT_HANDLE; //the smt handle
    FStatementType: TZIbSqlStatementType; //the stmt type
    FTypeTokens: TRawByteStringDynArray;
    FBatchStmts: array[Boolean] of TZIBStmt;
    FMaxRowsPerBatch, FMemPerRow: Integer;
    function ExecuteInternal: ISC_STATUS;
    function ExceuteBatch: Integer;
  protected //setters
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; {%H-}SQLType: TZSQLType; const Value: Double); override;
    procedure BindLob(Index: Integer; {%H-}SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindNull(Index: Integer; {%H-}SQLType: TZSQLType); override;
    procedure BindSignedOrdinal(Index: Integer; {%H-}SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; {%H-}SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;
  protected
    procedure CheckParameterIndex(Value: Integer); override;
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; override;
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure Close; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
  end;

  TZInterbase6PreparedStatement = class(TZAbstractInterbase6PreparedStatement, IZPreparedStatement);
  TZInterbase6Statement = class(TZAbstractInterbase6PreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  end;

  TZInterbase6CallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(Mode: TZCallExecKind; const
      StoredProcName: String): TZAbstractPreparedStatement2; override;
  end;

implementation

uses ZSysUtils, ZFastCode, ZEncoding, ZDbcInterbase6ResultSet, ZClasses;

{ TZAbstractInterbase6PreparedStatement }

{**
  execute the dml batch array
}
function TZAbstractInterbase6PreparedStatement.ExceuteBatch: Integer;
var
  AC: Boolean;
  ArrayOffSet: Integer;
begin
  Result := 0;
  AC := Connection.GetAutoCommit;
  if AC then
    Connection.SetAutoCommit(False);
  try
    ArrayOffSet := 0;
    FIBConnection.GetTrHandle; //restart transaction if required
    try
      if (FBatchStmts[True].Obj <> nil) and (ArrayCount >= FBatchStmts[True].PreparedRowsOfArray) then
        while (ArrayOffSet+FBatchStmts[True].PreparedRowsOfArray <= ArrayCount) do begin
          BindSQLDAInParameters(BindList, FBatchStmts[True].Obj.FParamSQLData,
            GetConnection.GetConSettings, FCodePageArray, ArrayOffSet,
            FBatchStmts[True].PreparedRowsOfArray);
          Result := FBatchStmts[True].Obj.ExecuteInternal;
          Inc(ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
        end;
      if (FBatchStmts[False].Obj <> nil) and (ArrayOffSet < ArrayCount) then begin
        BindSQLDAInParameters(BindList, FBatchStmts[False].Obj.FParamSQLData,
          GetConnection.GetConSettings, FCodePageArray, ArrayOffSet,
          FBatchStmts[False].PreparedRowsOfArray);
        Result := FBatchStmts[False].Obj.ExecuteInternal;
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
  LastUpdateCount := ArrayCount;
end;

function TZAbstractInterbase6PreparedStatement.ExecuteInternal: ISC_STATUS;
begin
  if ArrayCount = 0 then
    With FIBConnection do begin
      if FStatementType = stExecProc
      then Result := FPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
        @FStmtHandle, GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData) //expecting out params
      else Result := FPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @FStmtHandle, GetDialect, FParamSQLData.GetData); //not expecting a result
      if Result <> 0 then
        ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver,
          FStatusVector, Self, lcExecute, ASQL);
      LastUpdateCount := GetAffectedRows(FPlainDriver, FStmtHandle, FStatementType, Self);
    end
  else
    Result := ExceuteBatch;
end;

procedure TZAbstractInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection);
    if FParamSQLData.GetData^.sqln < BindList.Capacity then begin
      FParamSQLData.GetData^.sqld := BindList.Capacity;
      FParamSQLData.AllocateSQLDA;
    end;

    {check dynamic sql}
    if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect, FParamSQLData.GetData) <> 0 then
      ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcExecute, ASQL);

    //alloc space for lobs, arrays, param-types
    if ((FStatementType = stExecProc) and (FResultXSQLDA.GetFieldCount > 0)) or
       ((FStatementType = stSelect) and (BindList.HasOutParams))
    then BindList.SetCount(FParamSQLData.GetData^.sqld + FResultXSQLDA.GetFieldCount)
    else BindList.SetCount(FParamSQLData.GetData^.sqld);

    { Resize XSQLDA structure if required }
    if FParamSQLData.GetData^.sqld <> FParamSQLData.GetData^.sqln then begin
      FParamSQLData.AllocateSQLDA;
      if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData) <> 0 then
        ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcExecute, ASQL);
    end;
    FParamSQLData.InitFields(True);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
var
  B: boolean;
begin
  FStmtHandle := 0;
  for B := False to True do
    if Assigned(FBatchStmts[b].Obj) then
      FBatchStmts[b].Obj.ReleaseImmediat(Sender);
  inherited ReleaseImmediat(Sender);
end;

function TZAbstractInterbase6PreparedStatement.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
begin
  Result := inherited AlignParamterIndex2ResultSetIndex(Value);
  Result := Result - FParamSQLData.GetFieldCount
end;

procedure TZAbstractInterbase6PreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var RawTemp: RawByteString;
begin
  CheckParameterIndex(Index);
  if (SQLType = stGUID) and (FParamSQLData.GetFieldSqlType(Index) in [stString, stUnicodeString]) then begin
    RawTemp := GUIDToRaw(PGUID(Buf)^, False); //see https://firebirdsql.org/refdocs/langrefupd25-intfunc-uuid_to_char.html
    FParamSQLData.UpdatePAnsiChar(Index, Pointer(RawTemp), 36);
  end else
    FParamSQLData.UpdatePAnsiChar(Index, Buf, Len)
end;

procedure TZAbstractInterbase6PreparedStatement.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  CheckParameterIndex(Index);
  FParamSQLData.UpdateBoolean(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
begin
  CheckParameterIndex(Index);
  case SQLType of
    stDate: FParamSQLData.UpdateDate(Index, Value);
    stTime: FParamSQLData.UpdateTime(Index, Value);
    stTimeStamp: FParamSQLData.UpdateTimeStamp(Index, Value);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
begin
  CheckParameterIndex(Index);
  FParamSQLData.UpdateDouble(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
  inherited UnPrepareInParameters;
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
  FCodePageArray[ConSettings^.ClientCodePage^.ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FMaxRowsPerBatch := 0;
end;

procedure TZAbstractInterbase6PreparedStatement.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  CheckParameterIndex(Index);
  inherited BindLob(Index, FParamSQLData.GetFieldSqlType(Index), Value);
  if (BindList[index].Value = nil)
  then FParamSQLData.UpdateNull(Index, True)
  else FParamSQLData.WriteLobBuffer(Index, IZBlob(BindList[index].Value).GetBuffer, IZBlob(BindList[index].Value).Length);
end;

procedure TZAbstractInterbase6PreparedStatement.BindNull(Index: Integer;
  SQLType: TZSQLType);
begin
  CheckParameterIndex(Index);
  FParamSQLData.UpdateNull(Index, True);
end;

procedure TZAbstractInterbase6PreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  CheckParameterIndex(Index);
  if Value = ''
  then FParamSQLData.UpdatePAnsiChar(Index, PEmptyAnsiString, 0)
  else FParamSQLData.UpdatePAnsiChar(Index, Pointer(Value), Length(Value));
end;

procedure TZAbstractInterbase6PreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  CheckParameterIndex(Index);
  if Buf = nil
  then FParamSQLData.UpdatePAnsiChar(Index, PEmptyAnsiString, 0)
  else FParamSQLData.UpdatePAnsiChar(Index, Buf, Len);
end;

procedure TZAbstractInterbase6PreparedStatement.BindSignedOrdinal(
  Index: Integer; SQLType: TZSQLType; const Value: Int64);
begin
  CheckParameterIndex(Index);
  FParamSQLData.UpdateLong(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.BindUnsignedOrdinal(
  Index: Integer; SQLType: TZSQLType; const Value: UInt64);
begin
  CheckParameterIndex(Index);
  FParamSQLData.UpdateLong(Index, Int64(Value));
end;

procedure TZAbstractInterbase6PreparedStatement.CheckParameterIndex(
  Value: Integer);
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then
    raise EZSQLException.Create(SInvalidInputParameterCount)
end;

procedure TZAbstractInterbase6PreparedStatement.Close;
begin
  inherited Close;
  if (FStmtHandle <> 0) then begin// Free statement-handle! Otherwise: Exception!
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_drop);
    FStmtHandle := 0;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Prepare;
var
  eBlock: RawByteString;
  PreparedRowsOfArray: Integer;

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
      FStatementType := ZDbcInterbase6Utils.PrepareStatement(FPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ASQL, Self, FStmtHandle); //allocate handle if required or reuse it
      if FStatementType in [stSelect, stExecProc, stSelectForUpdate] then
      begin
        FResultXSQLDA := TZSQLDA.Create(Connection);
        PrepareResultSqlData(FPlainDriver, GetDialect,
          ASQL, FStmtHandle, FResultXSQLDA, Self);
      end;
    end;
    inherited Prepare; //log action and prepare params
  end;
  if ArrayCount > 0 then begin
    if FMaxRowsPerBatch = 0 then begin
      eBlock := GetExecuteBlockString(FParamSQLData,
        IsParamIndex, BindList.Count, ArrayCount, FCachedQueryRaw,
        FPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    end else
      eBlock := '';
    if (FMaxRowsPerBatch <= ArrayCount) and (eBlock <> '') then begin
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
      if ArrayCount > FMaxRowsPerBatch then //final block count
        PrepareFinalChunk(ArrayCount mod PreparedRowsOfArray);
    end else if (eBlock = '') then begin
      if (FMaxRowsPerBatch > ArrayCount) then begin
        if (FBatchStmts[False].PreparedRowsOfArray <> ArrayCount) then
          PrepareFinalChunk(ArrayCount) //full block of batch
      end else
        if (ArrayCount <> FMaxRowsPerBatch) and (FBatchStmts[False].PreparedRowsOfArray <> (ArrayCount mod FMaxRowsPerBatch)) then
          PrepareFinalChunk(ArrayCount mod FMaxRowsPerBatch); //final block of batch
    end else if (FBatchStmts[False].PreparedRowsOfArray <> ArrayCount) then
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
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_UNPREPARE);
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  SetLength(FTypeTokens, 0);
  inherited Unprepare;
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
var iError: Integer;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  BindInParameters;
  iError := ExecuteInternal;

  case FStatementType of
    stInsert, stDelete, stUpdate, stSelectForUpdate:
      Result := False;
    else
      Result := True;
  end;

  { Create ResultSet if possible else free Statement Handle }
  if iError <> DISCONNECT_ERROR then
    if (FStatementType in [stSelect, stExecProc, stSelectForUpdate]) and (FResultXSQLDA.GetFieldCount <> 0) then begin
      if not Assigned(LastResultSet) then
        LastResultSet := CreateIBResultSet(SQL, Self,
          TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
            FResultXSQLDA, True, CachedLob, FStatementType));
        FOpenResultSet := Pointer(LastResultSet);
      end else
    else
      LastResultSet := nil;
  inherited ExecutePrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  iError : Integer; //Check for database disconnect AVZ
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  BindInParameters;
  iError := ExecuteInternal;

  if (iError <> DISCONNECT_ERROR) then begin
    if (FStatementType in [stSelect, stExecProc]) and ( FResultXSQLDA.GetFieldCount <> 0) then
      if Assigned(FOpenResultSet) then
        Result := IZResultSet(FOpenResultSet)
      else begin
        Result := CreateIBResultSet(SQL, Self,
          TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
            FResultXSQLDA, False, CachedLob, FStatementType));
        FOpenResultSet := Pointer(Result);
      end
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
  BindInParameters;
  ExecuteInternal;
  Result := LastUpdateCount;
  if ArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: FreeStatement(FPlainDriver, FStmtHandle, DSQL_CLOSE);  //AVZ
      stExecProc: begin
        { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          LastResultSet := CreateIBResultSet(SQL, Self,
            TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
              FResultXSQLDA, True, CachedLob, FStatementType));
        FOpenResultSet := Pointer(LastResultSet);
      end;
    end;
  inherited ExecuteUpdatePrepared;
end;

function TZAbstractInterbase6PreparedStatement.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
begin
  CheckParameterIndex(ParamIndex);
  Result := FParamSQLData.GetAsLogValue(ParamIndex)
end;

{ TZInterbase6Statement }

constructor TZInterbase6Statement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.CreateExecutionStatement(
  Mode: TZCallExecKind;
  const StoredProcName: String): TZAbstractPreparedStatement2;
var
  P: PChar;
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
begin
  SQL := '';
  if mode = zcekParams
  then ToBuff('EXECUTE PROCEDURE ', SQL)
  else ToBuff('SELECT * FROM ',SQL);
  ToBuff(StoredProcName, SQL);
  ToBuff('(', SQL);
  for I := 0 to BindList.Capacity -1 do
    if not (BindList.ParamTypes[I] in [zptOutput,zptResult]) then
      ToBuff('?,', SQL);
  FlushBuff(SQL);
  P := Pointer(SQL);
  if (BindList.Capacity > 0) and ((P+Length(SQL)-1)^ = ',')
  then (P+Length(SQL)-1)^ := ')' //cancel last comma
  else (P+Length(SQL)-1)^ := ' '; //cancel bracket
  Result := TZInterbase6PreparedStatement.Create(Connection, SQL, Info);
end;

end.
