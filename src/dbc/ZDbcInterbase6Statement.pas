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
  ZPlainFirebirdInterbaseConstants, ZCompatibility,
  ZDbcLogging, ZVariant, ZMessages;

type

  {** Implements Prepared SQL Statement. }

  { TZInterbase6PreparedStatement }
  TZInterbase6PreparedStatement = class(TZAbstractPreparedStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FInitialStatementType, FStatementType: TZIbSqlStatementType;
    FMemPerRow, FArrayOffSet: Integer;
    FPreparedRowsOfArray: Integer;
    FTypeTokens: TRawByteStringDynArray;
    FZBufferSize: LongWord;
    function ExecuteInternal: Integer;
    function InternalPrepare(const SQL: RawByteString; PrepareParams: Boolean): TZIbSqlStatementType;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function CheckInterbase6Error(const Sql: RawByteString = '') : Integer;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; Info: TStrings); overload;
    procedure Close; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
  end;
  TZInterbase6Statement = class(TZInterbase6PreparedStatement);

  TZInterbase6CallableStatement = class(TZAbstractPreparedCallableStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    function ExecuteInternal: Integer;
  protected
    procedure CheckInterbase6Error(const Sql: RawByteString = '');
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

uses Math, ZSysUtils, ZDbcUtils, ZFastCode, ZPlainFirebirdDriver,
  ZDbcInterbase6ResultSet;

{ TZInterbase6PreparedStatement }
function TZInterbase6PreparedStatement.ExecuteInternal: Integer;
begin
  With FIBConnection do
  begin
    case FStatementType of
      stSelect, stSelectForUpdate: //AVZ Get many rows - only need to use execute not execute2
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData);
      stExecProc:
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData); //expecting a result
      else
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData) //not expecting a result
    end;
    Result := ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver,
      FStatusVector, ConSettings, lcExecute, ASQL);
    if Result <> DISCONNECT_ERROR then
      LastUpdateCount := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
  end;
end;

function TZInterbase6PreparedStatement.InternalPrepare(const SQL: RawByteString;
  PrepareParams: Boolean): TZIbSqlStatementType;
begin
  with Self.FIBConnection do begin
    Result := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
      GetDBHandle, GetTrHandle, GetDialect, SQL, ConSettings, FStmtHandle); //allocate handle if required or reuse it
    if PrepareParams and (Result in [stInsert, stUpdate, stDelete, stExecProc, stSelectForUpdate]) then
      PrepareInParameters;
    if Result in [stSelect, stExecProc, stSelectForUpdate] then
    begin
      FResultXSQLDA := TZSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
      PrepareResultSqlData(GetPlainDriver, GetDialect,
        SQL, FStmtHandle, FResultXSQLDA, ConSettings);
    end;
  end;
end;

procedure TZInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
    {check dynamic sql}
    GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect, FParamSQLData.GetData);
    ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, ConSettings, lcExecute, ASQL);

    { Resize XSQLDA structure if required }
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA;
      GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData);
      ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, ConSettings, lcExecute, ASQL);
    end;
    FParamSQLData.InitFields(True);
  end;
end;

procedure TZInterbase6PreparedStatement.BindInParameters;
begin
  if (ArrayCount > 0) and (FStatementType in [stExecProc]) then //newly prepared execute block is a SP
    while True do
      if (FArrayOffSet+FPreparedRowsOfArray >= ArrayCount) then
      begin
        if ArrayCount > FPreparedRowsOfArray then
          Prepare; {rebuild new block and bind ramaing params*iters}
        BindSQLDAInParameters(ClientVarManager, InParamValues,
          InParamCount, FParamSQLData, GetConnection.GetConSettings,
          FCodePageArray, FArrayOffSet, FPreparedRowsOfArray);
        Break
      end
      else
      begin
        BindSQLDAInParameters(ClientVarManager, InParamValues,
          InParamCount, FParamSQLData, GetConnection.GetConSettings,
          FCodePageArray, FArrayOffSet, FPreparedRowsOfArray);
        Inc(FArrayOffSet, FPreparedRowsOfArray);
        ExecuteInternal; {left space for last execution}
      end
  else
    BindSQLDAInParameters(ClientVarManager, InParamValues,
      InParamTypes, InParamCount, FParamSQLData, GetConnection.GetConSettings,
        FCodePageArray);
  inherited BindInParameters;
end;

procedure TZInterbase6PreparedStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
end;

{**
   Check interbase error status
   @param Sql the used sql tring

   @return Integer - Error Code to test for graceful database disconnection
}
function TZInterbase6PreparedStatement.CheckInterbase6Error(const SQL: RawByteString) : Integer;
begin
  Result := ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, ConSettings, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6PreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  FCodePageArray := (FIBConnection.GetIZPlainDriver as IZInterbasePlainDriver).GetCodePageArray;
  FCodePageArray[ConSettings^.ClientCodePage^.ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FZBufferSize := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //128KB by default
  FZBufferSize := Min(FZBufferSize, FIBConnection.GetXSQLDAMaxSize);
end;

constructor TZInterbase6PreparedStatement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  Create(Connection,'', Info);
end;

procedure TZInterbase6PreparedStatement.Close;
begin
  inherited Close;
  if (FStmtHandle <> 0) then begin// Free statement-handle! On the other hand: Exception!
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_drop);
    FStmtHandle := 0;
  end;
end;

procedure TZInterbase6PreparedStatement.Prepare;
  procedure PrepareArray(Iteration: Integer);
  begin
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE);
    FStatementType := InternalPrepare(GetExecuteBlockString(FParamSQLData,
      IsParamIndex, InParamCount, Iteration, CachedQueryRaw,
      FIBConnection.GetPlainDriver, FMemPerRow, FPreparedRowsOfArray,
        FTypeTokens, FInitialStatementType, FZBufferSize), False);
  end;
begin
  if (not Prepared) then
  begin
    FStatementType := InternalPrepare(ASQL, ArrayCount > 0);
    FInitialStatementType := FStatementType;
    if (FStatementType in [stInsert, stUpdate, stDelete]) and (ArrayCount > 0) and (FStmtHandle <> 0) then
      PrepareArray(ArrayCount);
    CheckInterbase6Error(ASQL);
    inherited Prepare;
  end else if (ArrayCount > 0 ) and ((FArrayOffSet+FPreparedRowsOfArray >= ArrayCount ) or (FArrayOffSet = 0)) then begin
    PrepareArray(ArrayCount-FArrayOffSet);
    CheckInterbase6Error(ASQL);
    inherited Prepare;
  end else if FInitialStatementType <> FStatementType then begin
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE);
    FPreparedRowsOfArray := 0;
    FStatementType := InternalPrepare(ASQL, False);
    inherited Prepare; //log action and prepare params
  end;
end;

procedure TZInterbase6PreparedStatement.Unprepare;
begin
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE); //unprepare avoids new allocation for the stmt handle
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  SetLength(FTypeTokens, 0);
  FMemPerRow := 0;
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
function TZInterbase6PreparedStatement.ExecutePrepared: Boolean;
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
function TZInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
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
      else
      begin
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
function TZInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
var
  iError : Integer; //Implementation for graceful disconnect AVZ
begin
  Prepare;
  BindInParameters;
  iError := ExecuteInternal;
  Result := LastUpdateCount;
  case FStatementType of
    stCommit, stRollback, stUnknown: Result := -1;
    stSelect: if (iError <> DISCONNECT_ERROR) then
      FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE);  //AVZ
  end;
  inherited ExecuteUpdatePrepared;
end;

procedure TZInterbase6PreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
begin
  if ParameterIndex = FirstDbcIndex then
  begin
    Self.FArrayOffSet := 0;
    Self.FPreparedRowsOfArray := 0;
  end;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
end;


{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.ExecuteInternal: Integer;
begin
  With FIBConnection do
  begin
    case FStatementType of
      stSelect: //AVZ Get many rows - only need to use execute not execute2
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData);
      stExecProc:
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData); //expecting a result
      else
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, nil) //not expecting a result
    end;
    Result := ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver,
      FStatusVector, ConSettings, lcExecute, FProcSQL);
  end;
end;

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6CallableStatement.CheckInterbase6Error(const Sql: RawByteString);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, ConSettings, lcExecute, Sql);
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
  FCodePageArray := (FIBConnection.GetIZPlainDriver as IZInterbasePlainDriver).GetCodePageArray;
  ResultSetType := rtScrollInsensitive;
  FStmtHandle := 0;
  FStatementType := stUnknown;
end;

procedure TZInterbase6CallableStatement.PrepareInParameters;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
    {check dynamic sql}
    GetPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,
      FParamSQLData.GetData);
    ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, FStatusVector, ConSettings, lcExecute, ASQL);

    { Resize XSQLDA structure if needed }
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA;
      GetPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData);
      ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, FStatusVector, ConSettings, lcExecute, ASQL);
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
      FStatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, FProcSql, ConSettings, FStmtHandle); //allocate handle if required or reuse it

      if FStatementType in [stSelect, stExecProc] then
        begin
          FResultXSQLDA := TZSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
          PrepareResultSqlData(GetPlainDriver, GetDialect,
            FProcSql, FStmtHandle, FResultXSQLDA, ConSettings);
        end;
    end;
    CheckInterbase6Error(FProcSql);
    inherited Prepare;
  end;
end;

procedure TZInterbase6CallableStatement.Unprepare;
begin
  inherited Unprepare;
  if FStmtHandle <> 0 then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE);
end;

procedure TZInterbase6CallableStatement.Close;
begin
  inherited Close;
  if FStmtHandle <> 0 then begin// Free statement-handle! On the other hand: Exception!
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_DROP);
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

    LastUpdateCount := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
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

    Result := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
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

end.

