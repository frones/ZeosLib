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

  TZIBStmt = record
    Obj: TZInterbase6PreparedStatement;
    Intf: IZPreparedStatement;
    PreparedRowsOfArray: Integer;
  end;

  { TZAbstractInterbase6PreparedStatement }
  TZAbstractInterbase6PreparedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FPlainDriver: TZInterbasePlainDriver;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    FTypeTokens: TRawByteStringDynArray;
    FBatchStmts: array[Boolean, stInsert..stDelete] of TZIBStmt;
    FMaxRowsPerBatch, FMemPerRow: Integer;
    function ExecuteInternal: ISC_STATUS;
    function ExceuteBatch: Integer;
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindNull(Index: Integer; SQLType: TZSQLType); override;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;
    procedure CheckParameterIndex(Value: Integer); override;
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;


  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
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

  TZInterbase6CallableStatement = class(TZAbstractPreparedCallableStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FPlainDriver: TZInterbasePlainDriver;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    function ExecuteInternal: Integer;
  protected
    function GetProcedureSql(SelectProc: boolean): RawByteString;

    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure Close; override;

    procedure Prepare(SelectProc: Boolean); reintroduce;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses Math, ZSysUtils, ZDbcUtils, ZFastCode,
  ZDbcInterbase6ResultSet, ZDbcProperties;

{ TZAbstractInterbase6PreparedStatement }

{**
  execute the dml batch array
}
function TZAbstractInterbase6PreparedStatement.ExceuteBatch: Integer;
var
  AC: Boolean;
  ArrayOffSet: Integer;
begin
  AC := Connection.GetAutoCommit;
  Connection.SetAutoCommit(False);
  Result := 0;
  try
    ArrayOffSet := 0;
    FIBConnection.GetTrHandle; //restart transaction if required
    try
      if ArrayCount >= FBatchStmts[True][FStatementType].PreparedRowsOfArray then
        while (ArrayOffSet+FBatchStmts[True][FStatementType].PreparedRowsOfArray < ArrayCount) do begin
          BindSQLDAInParameters(BindList, FBatchStmts[True][FStatementType].Obj.FParamSQLData,
            GetConnection.GetConSettings, FCodePageArray, ArrayOffSet,
            FBatchStmts[True][FStatementType].PreparedRowsOfArray);
          Result := FBatchStmts[True][FStatementType].Obj.ExecuteInternal;
          Inc(ArrayOffSet, FBatchStmts[True][FStatementType].PreparedRowsOfArray);
        end;
      BindSQLDAInParameters(BindList, FBatchStmts[False][FStatementType].Obj.FParamSQLData,
        GetConnection.GetConSettings, FCodePageArray, ArrayOffSet,
        FBatchStmts[False][FStatementType].PreparedRowsOfArray);
      Result := FBatchStmts[False][FStatementType].Obj.ExecuteInternal;
      Connection.Commit;
    except
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

    BindList.SetCount(FParamSQLData.GetData^.sqld); //alloc space for lobs and arrays

    { Resize XSQLDA structure if required }
    if FParamSQLData.GetData^.sqld <> FParamSQLData.GetData^.sqln then
    begin
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
  StatementType: TZIbSqlStatementType;
begin
  FStmtHandle := 0;
  for B := False to True do
    for StatementType := stInsert to stDelete do
      if Assigned(FBatchStmts[b][StatementType].Obj) then
        FBatchStmts[b][StatementType].Obj.ReleaseImmediat(Sender);
  inherited ReleaseImmediat(Sender);
end;

procedure TZAbstractInterbase6PreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var RawTemp: RawByteString;
begin
  CheckParameterIndex(Index); //check index, mark io and type
  if (SQLType = stGUID) and (FParamSQLData.GetFieldSqlType(Index) in [stString, stUnicodeString]) then begin
    RawTemp := GUIDToRaw(PGUID(Buf)^, False); //see https://firebirdsql.org/refdocs/langrefupd25-intfunc-uuid_to_char.html
    FParamSQLData.UpdatePAnsiChar(Index, Pointer(RawTemp), 36);
  end else
    FParamSQLData.UpdatePAnsiChar(Index, Buf, Len);
end;

procedure TZAbstractInterbase6PreparedStatement.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  FParamSQLData.UpdateBoolean(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  case SQLType of
    stDate: FParamSQLData.UpdateDate(Index, Value);
    stTime: FParamSQLData.UpdateTime(Index, Value);
    stTimeStamp: FParamSQLData.UpdateTimeStamp(Index, Value);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  FParamSQLData.UpdateDouble(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.BindInParameters;
begin
  //if ArrayCount = 0 then
    (*BindSQLDAInParameters( ClientVarManager, InParamValues,
      InParamTypes, InParamCount, FParamSQLData, GetConnection.GetConSettings,
        FCodePageArray);*)
  inherited BindInParameters;
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
  CheckParameterIndex(Index); //check index, mark io and type
  inherited BindLob(Index, FParamSQLData.GetFieldSqlType(Index), Value);
  if (BindList[index].Value = nil)
  then FParamSQLData.UpdateNull(Index, True)
  else FParamSQLData.WriteLobBuffer(Index, IZBlob(BindList[index].Value).GetBuffer, IZBlob(BindList[index].Value).Length);
end;

procedure TZAbstractInterbase6PreparedStatement.BindNull(Index: Integer;
  SQLType: TZSQLType);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  FParamSQLData.UpdateNull(Index, True);
end;

procedure TZAbstractInterbase6PreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  if Value = ''
  then FParamSQLData.UpdatePAnsiChar(Index, PEmptyAnsiString, 0)
  else FParamSQLData.UpdatePAnsiChar(Index, Pointer(Value), Length(Value));
end;

procedure TZAbstractInterbase6PreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  if Buf = nil
  then FParamSQLData.UpdatePAnsiChar(Index, PEmptyAnsiString, 0)
  else FParamSQLData.UpdatePAnsiChar(Index, Buf, Len);
end;

procedure TZAbstractInterbase6PreparedStatement.BindSignedOrdinal(
  Index: Integer; SQLType: TZSQLType; const Value: Int64);
begin
  CheckParameterIndex(Index); //check index, mark io and type
  FParamSQLData.UpdateLong(Index, Value);
end;

procedure TZAbstractInterbase6PreparedStatement.BindUnsignedOrdinal(
  Index: Integer; SQLType: TZSQLType; const Value: UInt64);
begin
  CheckParameterIndex(Index); //check index, mark io and type
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
  if (FStmtHandle <> 0) then begin// Free statement-handle! On the other hand: Exception!
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
    if (Slot.Intf = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then
        Slot.Obj.BindList.Count := 0;
      Slot.Obj := TZInterbase6PreparedStatement.Create(Connection, '', Info);
      Slot.Intf := Slot.Obj;
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
      FPlainDriver, FMemPerRow, PreparedRowsOfArray,
      FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    PrepareArrayStmt(FBatchStmts[False][FStatementType]);
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
        FPlainDriver, FMemPerRow, PreparedRowsOfArray,
          FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    end else
       eBlock := '';
    if (FMaxRowsPerBatch < ArrayCount) and (eBlock <> '') then begin
      FMaxRowsPerBatch := PreparedRowsOfArray;
      PrepareArrayStmt(FBatchStmts[True][FStatementType]);
      //final chunk
      PrepareFinalChunk(ArrayCount mod PreparedRowsOfArray);
    end else if (eBlock = '') then begin
      if (FMaxRowsPerBatch = 0) and (FBatchStmts[False][FStatementType].PreparedRowsOfArray <> ArrayCount) then
        PrepareFinalChunk(ArrayCount)
      else if (FMaxRowsPerBatch <> 0) and (FBatchStmts[False][FStatementType].PreparedRowsOfArray <> ArrayCount mod FMaxRowsPerBatch) then
        PrepareFinalChunk(ArrayCount mod FMaxRowsPerBatch);
    end;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Unprepare;
var b: Boolean;
  st: TZIbSqlStatementType;
begin
  for b := False to True do
    for st := stInsert to stDelete do
      if FBatchStmts[b][st].Obj <> nil then begin
        FBatchStmts[b][st].Obj.BindList.Count := 0;
        FBatchStmts[b][st].Obj := nil;
        FBatchStmts[False][FStatementType].Intf := nil;
      end;
  FMaxRowsPerBatch := 0;
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_UNPREPARE); //unprepare avoids new allocation for the stmt handle
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
    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if not Assigned(LastResultSet) then
        LastResultSet := CreateIBResultSet(SQL, Self,
          TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
            FResultXSQLDA, True, CachedLob, FStatementType))
      else
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
var
  iError : Integer; //Implementation for graceful disconnect AVZ
begin
  Prepare;
  LastResultSet := nil;
  BindInParameters;
  iError := ExecuteInternal;
  Result := LastUpdateCount;
  if ArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: if (iError <> DISCONNECT_ERROR) then
        FreeStatement(FPlainDriver, FStmtHandle, DSQL_CLOSE);  //AVZ
      stExecProc:
        { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          LastResultSet := CreateIBResultSet(SQL, Self,
            TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
              FResultXSQLDA, True, CachedLob, FStatementType));
    end;
  inherited ExecuteUpdatePrepared;
end;

function TZAbstractInterbase6PreparedStatement.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
begin
  Result := FParamSQLData.GetAsLogValue(ParamIndex);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.ExecuteInternal: Integer;
begin
  With FIBConnection do
  begin
    if FStatementType =  stExecProc
    then Result := FPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
      @FStmtHandle, GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData) //expecting out params
   else Result := FPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
      @FStmtHandle, GetDialect, FParamSQLData.GetData);
   if Result <> 0 then
      ZDbcInterbase6Utils.CheckInterbase6Error(FPlainDriver,
        FStatusVector, Self, lcExecute, FProcSQL);
  end;
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6CallableStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  FPlainDriver := TZInterbasePlainDriver(FIBConnection.GetIZPlainDriver.GetInstance);
  FCodePageArray := FPlainDriver.GetCodePageArray;
  ResultSetType := rtScrollInsensitive;
  FStmtHandle := 0;
  FStatementType := stUnknown;
end;

procedure TZInterbase6CallableStatement.PrepareInParameters;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection);
    {check dynamic sql}
    if FPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,
        FParamSQLData.GetData) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, ASQL);

    { Resize XSQLDA structure if needed }
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA;
      if FPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self, lcExecute, ASQL);
    end;
    FParamSQLData.InitFields(True);
  end;
end;

procedure TZInterbase6CallableStatement.BindInParameters;
begin
  TrimInParameters;
  BindSQLDAInParameters(ClientVarManager,
    InParamValues, InParamTypes, InParamCount, FParamSQLData, ConSettings, FCodePageArray);
  inherited BindInParameters;
end;

procedure TZInterbase6CallableStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
end;

procedure TZInterbase6CallableStatement.Prepare(SelectProc: Boolean);
const
  CallableStmtType: array[Boolean] of TZIbSqlStatementType = (stExecProc, stSelect);
begin
  if CallableStmtType[SelectProc] <> FStatementType then UnPrepare;
  if not Prepared then
  begin
    FProcSql := GetProcedureSql(SelectProc);
    with FIBConnection do
    begin
      FStatementType := ZDbcInterbase6Utils.PrepareStatement(FPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, FProcSql, Self, FStmtHandle); //allocate handle if required or reuse it

      if FStatementType in [stSelect, stExecProc] then
        begin
          FResultXSQLDA := TZSQLDA.Create(Connection);
          PrepareResultSqlData(FPlainDriver, GetDialect,
            FProcSql, FStmtHandle, FResultXSQLDA, Self);
        end;
    end;
    inherited Prepare;
  end;
end;

procedure TZInterbase6CallableStatement.Unprepare;
begin
  inherited Unprepare;
  if FStmtHandle <> 0 then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_UNPREPARE);
end;

procedure TZInterbase6CallableStatement.Close;
begin
  inherited Close;
  if FStmtHandle <> 0 then begin// Free statement-handle! On the other hand: Exception!
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_DROP);
    FStmtHandle := 0;
  end;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZInterbase6CallableStatement.ExecutePrepared: Boolean;
var RS: IZResultSet;
begin
  Prepare(False);
  PrepareLastResultSetForReUse;
  PrepareOpenResultSetForReUse;
  with FIBConnection do begin
    BindInParameters;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    ExecuteInternal;

    LastUpdateCount := GetAffectedRows(FPlainDriver, FStmtHandle, FStatementType, Self);
    Result := not (FStatementType in [stInsert, stDelete, stUpdate, stSelectForUpdate]);

    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if not Assigned(LastResultSet) then
        LastResultSet := TZInterbase6XSQLDAResultSet.Create(Self, SQL,
          FStmtHandle, FResultXSQLDA, True, CachedLob, FStatementType)
      else begin
        { Fetch data and fill Output params }
        LastResultSet := nil;
        if not Assigned(FOpenResultSet) then
        begin
          RS := TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
            FResultXSQLDA, False, CachedLob, FStatementType);
          FOpenResultSet := Pointer(RS);
        end;
        AssignOutParamValuesFromResultSet(IZResultSet(FOpenResultSet),
            OutParamValues, OutParamCount , FDBParamTypes);
      end;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZInterbase6CallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  Prepare(True);
  PrepareOpenResultSetForReUse;
  with FIBConnection do
  begin
    BindInParameters;

    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, FProcSql);
    ExecuteInternal;
    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if Assigned(FOpenResultSet) then
        Result := IZResultSet(FOpenResultSet)
      else
      begin
        Result := TZInterbase6XSQLDAResultSet.Create(Self, Self.SQL,
          FStmtHandle, FResultXSQLDA, False, CachedLob, FStatementType);
        FOpenResultSet := Pointer(Result);
      end;
  end;
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
function TZInterbase6CallableStatement.ExecuteUpdatePrepared: Integer;
var RS: IZResultSet;
begin
  Prepare(False);
  PrepareOpenResultSetForReUse;
  with FIBConnection do
  begin
    BindInParameters;

    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, FProcSql);
    ExecuteInternal;

    Result := GetAffectedRows(FPlainDriver, FStmtHandle, FStatementType, Self);
    LastUpdateCount := Result;
    { Fetch data and fill Output params }
    if not Assigned(FOpenResultSet) then
    begin
      RS := TZInterbase6XSQLDAResultSet.Create(Self, SQL, FStmtHandle,
        FResultXSQLDA, False, CachedLob, FStatementType);
      FOpenResultSet := Pointer(RS);
    end;
    AssignOutParamValuesFromResultSet(IZResultSet(FOpenResultSet), OutParamValues, OutParamCount , FDBParamTypes);
  end;
end;

{**
   Create sql string for calling stored procedure.
   @param SelectProc indicate use <b>EXECUTE PROCEDURE</b> or
    <b>SELECT</b> staement
   @return a Stored Procedure SQL string
}
function TZInterbase6CallableStatement.GetProcedureSql(SelectProc: boolean): RawByteString;

  function GenerateParamsStr(Count: integer): RawByteString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if I > 0 then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: RawByteString;
begin
  //TrimInParameters;
  InParams := GenerateParamsStr(Length(InParamValues));
  if InParams <> '' then
    InParams := '(' + InParams + ')';

  if SelectProc then
    Result := 'SELECT * FROM ' + ASQL + InParams
  else
    Result := 'EXECUTE PROCEDURE ' + ASQL + InParams;
end;

{ TZInterbase6Statement }

constructor TZInterbase6Statement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

end.
