{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql;

type

  {** Defines a PostgreSQL specific statement. }
  IZPostgreSQLStatement = interface(IZStatement)
    ['{E4FAFD96-97CC-4247-8ECC-6E0A168FAFE6}']

    function IsOidAsBlob: Boolean;
  end;

  TEICategory = (eicExecute, eicPrepStmt, eicExecPrepStmt, eicUnprepStmt);
  {** Implements Generic PostgreSQL Statement. }
  TZPostgreSQLStatement = class(TZAbstractStatement, IZPostgreSQLStatement)
  private
    FPlainDriver: IZPostgreSQLPlainDriver;
    FOidAsBlob: Boolean;
  protected
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
    function GetConnectionHandle():PZPostgreSQLConnect;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function IsOidAsBlob: Boolean;
  end;

  {$IFDEF ZEOS_TEST_ONLY}
  {** Implements Emulated Prepared SQL Statement. }
  TZPostgreSQLEmulatedPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FPlainDriver: IZPostgreSQLPlainDriver;
    Foidasblob: Boolean;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString; override;
    function GetConnectionHandle: PZPostgreSQLConnect;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings);
  end;
  {$ENDIF}

  TZPostgreSQLPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FRawPlanName: RawByteString;
    FPostgreSQLConnection: IZPostgreSQLConnection;
    FPlainDriver: IZPostgreSQLPlainDriver;
    QueryHandle: PZPostgreSQLResult;
    Foidasblob: Boolean;
    FConnectionHandle: PZPostgreSQLConnect;
    Findeterminate_datatype: Boolean;
  protected
    function CreateResultSet(QueryHandle: PZPostgreSQLResult): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; virtual; abstract;
    function PrepareAnsiSQLQuery: RawByteString;
    function GetDeallocateSQL: RawByteString; virtual; abstract;
    function GetPrepareSQLPrefix: RawByteString; virtual; abstract;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement with AnsiString usage }
  TZPostgreSQLClassicPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FExecSQL: RawByteString;
    function GetAnsiSQLQuery: RawByteString;
  protected
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer; Escaped: Boolean): RawByteString;
    procedure BindInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement based on Protocol3
    ServerVersion 7.4Up and ClientVersion 8.0Up. with C++API usage}
  TZPostgreSQLCAPIPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FPQparamValues: TPQparamValues;
    FPQparamLengths: TPQparamLengths;
    FPQparamFormats: TPQparamFormats;
    FPRawPlanName: PAnsiChar;
  protected
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
  end;

  {** Implements callable Postgresql Statement. }
  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement)
  private
    Foidasblob: Boolean;
    FPlainDriver: IZPostgreSQLPlainDriver;
    function GetProcedureSql: string;
    function FillParams(const ASql: String): RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    function GetConnectionHandle:PZPostgreSQLConnect;
    function GetPlainDriver:IZPostgreSQLPlainDriver;
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
    procedure TrimInParameters; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver, IZCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils,
  ZEncoding, ZDbcUtils;

{ TZPostgreSQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLStatement.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  { Processes connection properties. }
  FOidAsBlob := StrToBoolEx(Self.Info.Values['oidasblob'])
    or (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object.
}
function TZPostgreSQLStatement.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLStatement.CreateResultSet(const SQL: string;
  QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, SQL,
  ConnectionHandle, QueryHandle, CachedLob, ChunkSize);

  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil, ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := inherited ExecuteQuery(SQL);
  ConnectionHandle := GetConnectionHandle();
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  if QueryHandle <> nil then
    Result := CreateResultSet(Self.SQL, QueryHandle)
  else
    Result := nil;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZPostgreSQLStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := inherited ExecuteUpdate(SQL);
  ConnectionHandle := GetConnectionHandle();

  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(FPlainDriver.GetCommandTuples(QueryHandle), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZPostgreSQLStatement.Execute(const SQL: RawByteString): Boolean;
var
  QueryHandle: PZPostgreSQLResult;
  ResultStatus: TZPostgreSQLExecStatusType;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  inherited Execute(SQL);
  ConnectionHandle := GetConnectionHandle();
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(Self.SQL, QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Provides connection handle from the associated IConnection
}
function TZPostgreSQLStatement.GetConnectionHandle():PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{$IFDEF ZEOS_TEST_ONLY}
{ TZPostgreSQLEmulatedPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLEmulatedPreparedStatement.Create(
  PlainDriver: IZPostgreSQLPlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  Foidasblob := StrToBoolDef(Self.Info.Values['oidasblob'], False) or
    (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Creates a temporary statement which executes queries.
  @return a created statement object.
}
function TZPostgreSQLEmulatedPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Connection, Info);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLEmulatedPreparedStatement.PrepareAnsiSQLParam(
  ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), FPlainDriver, ChunkSize,
    InParamTypes[ParamIndex], Foidasblob, True, False, ConSettings);
end;

{**
  Provides connection handle from the associated IConnection
}
function TZPostgreSQLEmulatedPreparedStatement.GetConnectionHandle:PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;
{$ENDIF}

{ TZPostgreSQLPreparedStatement }

{**
  Creates a result set based on the current settings.
  @param QueryHandle the Postgres query handle
  @return a created result set object.
}
constructor TZPostgreSQLPreparedStatement.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  Foidasblob := StrToBoolDef(Self.Info.Values['oidasblob'], False) or
    (Connection as IZPostgreSQLConnection).IsOidAsBlob;
  FPostgreSQLConnection := Connection;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  FConnectionHandle := Connection.GetConnectionHandle;
  Findeterminate_datatype := False;
  FRawPlanName := IntToRaw(Hash(ASQL)+Cardinal(FStatementId)+{%H-}NativeUInt(FConnectionHandle));
end;

function TZPostgreSQLPreparedStatement.CreateResultSet(QueryHandle: Pointer): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, Self.SQL,
  FConnectionHandle, QueryHandle, CachedLob, ChunkSize);

  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a RawByteString SQL statement.
}
function TZPostgreSQLPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then
    begin
      if InParamCount <= ParamIndex then
        raise EZSQLException.Create(SInvalidInputParameterCount);
      Result := Result + PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
        ClientVarManager, (Connection as IZPostgreSQLConnection), FPlainDriver,
        ChunkSize, InParamTypes[ParamIndex], Foidasblob, True, False, ConSettings);
      Inc(ParamIndex);
    end
    else
      Result := Result + CachedQueryRaw[i];
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZPostgreSQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;

  Prepare;
  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);
  if QueryHandle <> nil then
  begin
    Result := CreateResultSet(QueryHandle);
    FOpenResultSet := Pointer(Result);
  end
  else
    Result := nil;
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
function TZPostgreSQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  Prepare;

  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(FPlainDriver.GetCommandTuples(QueryHandle), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;

  inherited ExecuteUpdatePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZPostgreSQLPreparedStatement.ExecutePrepared: Boolean;
var
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  Prepare;

  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;

  inherited ExecutePrepared;
end;

procedure TZPostgreSQLPreparedStatement.Prepare;
var
  TempSQL: RawByteString;
  N, I: Integer;
begin
  if not Prepared then
  begin
    TempSQL := GetPrepareSQLPrefix;
    N := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Inc(N);
        TempSQL := TempSQL + '$' + IntToRaw(N);
      end else
        TempSQL := TempSQL + CachedQueryRaw[i];

  { EgonHugeist: assume automated Prepare after third execution. That's the way
    the JDBC Drivers go too... }
    if ( N > 0 ) or ( ExecCount > 2 ) then //prepare only if Params are available or certain executions expected
    begin
      QueryHandle := ExecuteInternal(TempSQL, eicPrepStmt);
      if not (Findeterminate_datatype) then
        FPlainDriver.Clear(QueryHandle);
      inherited Prepare;
    end;
  end;
end;

procedure TZPostgreSQLPreparedStatement.Unprepare;
begin
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) then
  begin
    inherited Unprepare;
    if (not Findeterminate_datatype)  then
    begin
      QueryHandle := ExecuteInternal(GetDeallocateSQL, eicUnprepStmt);
      FPlainDriver.Clear(QueryHandle);
      FPostgreSQLConnection.UnregisterPreparedStmtName({$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
    end;
  end;
end;

{ TZPostgreSQLClassicPreparedStatement }

function TZPostgreSQLClassicPreparedStatement.GetAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then
    begin
      Result := Result + PrepareAnsiSQLParam(ParamIndex, True);
      Inc(ParamIndex);
    end
    else
      Result := Result + CachedQueryRaw[i];
end;

function TZPostgreSQLClassicPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  const Category: TEICategory): PZPostgreSQLResult;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
        Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
          FConnectionHandle, lcPrepStmt, ASQL, Result) = '42P18');
        if not Findeterminate_datatype then
          FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(FExecSQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
        begin
          Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
          CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
            lcUnprepStmt, ASQL, Result);
        end
      else Result := nil;
    else
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

function TZPostgreSQLClassicPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer;
  Escaped: Boolean): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), FPlainDriver, ChunkSize,
    InParamTypes[ParamIndex], Foidasblob, Escaped, True, ConSettings);
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLClassicPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount > 0 then
    if Prepared then
    begin
      FExecSQL := 'EXECUTE "'+FRawPlanName+'"(';
      for i := 0 to InParamCount -1 do
        if I = 0 then
          FExecSQL := FExecSQL+PrepareAnsiSQLParam(i, False)
        else
          FExecSQL := FExecSQL+','+PrepareAnsiSQLParam(i, False);
      FExecSQL := FExecSQL+');';
    end
    else
      FExecSQL := GetAnsiSQLQuery
  else
    FExecSQL := ASQL;
  inherited BindInParameters;
end;

function TZPostgreSQLClassicPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'"';
end;

function TZPostgreSQLClassicPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := 'PREPARE "'+FRawPlanName+'" AS ';
end;

{ TZPostgreSQLCAPIPreparedStatement }

function TZPostgreSQLCAPIPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  const Category: TEICategory): PZPostgreSQLResult;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.Prepare(FConnectionHandle, FPRawPlanName,
          PAnsiChar(SQL), InParamCount, nil);
        Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
          FConnectionHandle, lcPrepStmt, ASQL, Result) = '42P18');
        if not Findeterminate_datatype then
          FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
        Exit;
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.ExecPrepared(FConnectionHandle,
          FPRawPlanName, InParamCount, FPQparamValues,
          FPQparamLengths, FPQparamFormats, 0);
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecPrepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
        begin
          Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
          CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
            lcUnprepStmt, ASQL, Result);
        end
      else Result := nil;
    else
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.PrepareInParameters;
begin
  if not (Findeterminate_datatype) then
  begin
    SetLength(FPQparamValues, InParamCount);
    SetLength(FPQparamLengths, InParamCount);
    SetLength(FPQparamFormats, InParamCount);
  end;
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLCAPIPreparedStatement.BindInParameters;
var
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  ParamIndex: Integer;

  procedure UpdateNull(const Index: Integer);
  begin
    FPQparamValues[Index] := nil;
    FPQparamLengths[Index] := 0;
    FPQparamFormats[Index] := 0;
  end;

  procedure UpdatePAnsiChar(const Value: PAnsiChar; Const Index: Integer);
  begin
    UpdateNull(Index);
    FPQparamValues[Index] := Value;
    {EH: sade.., PG ignores Length settings for string even if it could speed up
      the speed by having a known size instead of checking for #0 terminator}
  end;

  procedure UpdateBinary(Value: Pointer; const Len, Index: Integer);
  begin
    UpdateNull(Index);

    FPQparamValues[Index] := Value;
    FPQparamLengths[Index] := Len;
    FPQparamFormats[Index] := 1;
  end;

begin
  if InParamCount <> High(FPQparamValues)+1 then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  for ParamIndex := 0 to InParamCount -1 do
  begin
    if ClientVarManager.IsNull(InParamValues[ParamIndex])  then
      UpdateNull(ParamIndex)
    else
      {EH: Nice advanteges of the TZVariant:
        a string(w.Type ever) needs to be localized. So i simply reuse this
        values as vars and have a constant pointer ((: }
      case InParamTypes[ParamIndex] of
        stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString:
          UpdatePAnsiChar(ClientVarManager.GetAsCharRec(InParamValues[ParamIndex], ConSettings^.ClientCodePage^.CP).P, ParamIndex);
        stBytes:
          begin
            InParamValues[ParamIndex].VBytes := ClientVarManager.GetAsBytes(InParamValues[ParamIndex]);
            UpdateBinary(Pointer(InParamValues[ParamIndex].VBytes), Length(InParamValues[ParamIndex].VBytes), ParamIndex);
          end;
        stDate:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stTime:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stTimestamp:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[ParamIndex]) as IZBlob;
            if TempBlob.IsEmpty then
              UpdateNull(ParamIndex)
            else
              case InParamTypes[ParamIndex] of
                stBinaryStream:
                  if Foidasblob then
                  begin
                    try
                      WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
                        FConnectionHandle, 0, ChunkSize);
                      WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                      InParamValues[ParamIndex].VRawByteString := IntToRaw(WriteTempBlob.GetBlobOid);
                      UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
                    finally
                      WriteTempBlob := nil;
                    end;
                  end
                  else
                    UpdateBinary(TempBlob.GetBuffer, TempBlob.Length, ParamIndex);
                stAsciiStream, stUnicodeStream:
                  if TempBlob.IsClob then
                    UpdatePAnsiChar(TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), ParamIndex)
                  else
                  begin
                    InParamValues[ParamIndex].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                      TempBlob.Length, ConSettings);
                    UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
                  end;
              end; {case..}
          end;
      end;
  end;
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.UnPrepareInParameters;
begin
  { release allocated memory }
  if not (Findeterminate_datatype) then
  begin
    SetLength(FPQparamValues, 0);
    SetLength(FPQparamLengths, 0);
    SetLength(FPQparamFormats, 0);
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'";';
end;

function TZPostgreSQLCAPIPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := '';
  FPRawPlanName := PAnsiChar(FRawPlanName);
end;

{ TZPostgreSQLCallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLCallableStatement.Create(
  Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  ResultSetType := rtScrollInsensitive;
  FPlainDriver := (Connection as IZPostgreSQLConnection).GetPlainDriver;
  Foidasblob := StrToBoolDef(Self.Info.Values['oidasblob'], False) or
    (Connection as IZPostgreSQLConnection).IsOidAsBlob;
end;

{**
  Provides connection handle from the associated IConnection
  @return a PostgreSQL connection handle.
}
function TZPostgreSQLCallableStatement.GetConnectionHandle:PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLCallableStatement.CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  NativeResultSet := TZPostgreSQLResultSet.Create(GetPlainDriver, Self, SQL,
    ConnectionHandle, QueryHandle, CachedLob, ChunkSize);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
   Returns plain draiver from connection object
   @return a PlainDriver object
}
function TZPostgreSQLCallableStatement.GetPlainDriver():IZPostgreSQLPlainDriver;
begin
  if self.Connection <> nil then
    Result := (self.Connection as IZPostgreSQLConnection).GetPlainDriver
  else
    Result := nil;
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLCallableStatement.PrepareAnsiSQLParam(
  ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), FPlainDriver, ChunkSize,
    InParamTypes[ParamIndex], Foidasblob, True, False, ConSettings);
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := nil;
  ConnectionHandle := GetConnectionHandle();
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  if QueryHandle <> nil then
  begin
    Result := CreateResultSet(Self.SQL, QueryHandle);
    AssignOutParamValuesFromResultSet(Result, OutParamValues, OutParamCount , FDBParamTypes);
  end
  else
    Result := nil;
end;

{**
  Prepares and executes an SQL statement that returns a single <code>ResultSet</code> object.
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  TrimInParameters;
  Result := ExecuteQuery(FillParams(GetProcedureSql));
end;

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZPostgreSQLCallableStatement.GetProcedureSql: string;
  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: string;
begin
  if Length(CachedQueryRaw) = 1 then  //only name in there?
  begin
    Unprepare; //reset cached query
    InParams := GenerateParamsStr(InParamCount);
    Result := Format('SELECT * FROM %s(%s)', [SQL, InParams]);
    {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := Result; //sets the cached queries again
  end;
end;

{**
   Fills the parameter (?) tokens with corresponding parameter value
   @return a prepared SQL query for execution
}
function TZPostgreSQLCallableStatement.FillParams(const ASql: String): RawByteString;
var I: Integer;
  ParamIndex: Integer;
begin
  if InParamCount > 0 then
  begin
    Result := '';
    ParamIndex := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Result := Result + PrepareAnsiSQLParam(ParamIndex);
        Inc(ParamIndex);
      end
      else
        Result := Result + CachedQueryRaw[i];
  end
  else
    Result := GetRawEncodedSQL(ASql);
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZPostgreSQLCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := -1;
  ConnectionHandle := GetConnectionHandle();
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(GetPlainDriver.GetCommandTuples(QueryHandle), 0);
    AssignOutParamValuesFromResultSet(CreateResultSet(Self.SQL, QueryHandle),
      OutParamValues, OutParamCount , FDBParamTypes);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;


function TZPostgreSQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  TrimInParameters;
  Result := Self.ExecuteUpdate(FillParams(GetProcedureSql));
end;

{**
   Function removes ptResult, ptOutput parameters from
   InParamTypes and InParamValues
}
procedure TZPostgreSQLCallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  for I := 0 to High(InParamTypes) do
  begin
    if (Self.FDBParamTypes[i] in [2, 4]) then //[ptResult, ptOutput]
      Continue;
    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;

  if ParamCount = InParamCount then
    Exit;

  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount);
end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stBinaryStream, stUnicodeStream]);
end;



end.

