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
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql;

type

  {** Defines a PostgreSQL specific statement. }
  IZPostgreSQLStatement = interface(IZStatement)
    ['{E4FAFD96-97CC-4247-8ECC-6E0A168FAFE6}']

    function IsOidAsBlob: Boolean;
  end;

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
    FPlanName: String;
    FRawPlanName: RawByteString;
    FPostgreSQLConnection: IZPostgreSQLConnection;
    FPlainDriver: IZPostgreSQLPlainDriver;
    QueryHandle: PZPostgreSQLResult;
    Foidasblob: Boolean;
    FConnectionHandle: PZPostgreSQLConnect;
    Findeterminate_datatype: Boolean;
    FCachedQuery: TStrings;
    function CreateResultSet(QueryHandle: PZPostgreSQLResult): IZResultSet;
  protected
    procedure SetPlanNames; virtual; abstract;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement with AnsiString usage }
  TZPostgreSQLClassicPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FExecSQL: RawByteString;
    function GetAnsiSQLQuery: RawByteString;
  protected
    procedure SetPlanNames; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer; Escaped: Boolean): RawByteString;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    procedure Prepare; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement based on Protocol3
    ServerVersion 7.4Up and ClientVersion 8.0Up. with C++API usage}
  TZPostgreSQLCAPIPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FPQparamValues: TPQparamValues;
    FPQparamLengths: TPQparamLengths;
    FPQparamFormats: TPQparamFormats;
    function ExectuteInternal(const SQL: RawByteString; const LogSQL: String;
      const LoggingCategory: TZLoggingCategory): PZPostgreSQLResult;
  protected
    procedure SetPlanNames; override;
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function PrepareAnsiSQLQuery: RawByteString;

  public
    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
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
    procedure FetchOutParams(ResultSet: IZResultSet);
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
  ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils, ZTokenizer,
  ZEncoding{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

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
  ConnectionHandle, QueryHandle, ChunkSize);

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
  Result := nil;
  ConnectionHandle := GetConnectionHandle();
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    SSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
  if QueryHandle <> nil then
    Result := CreateResultSet(LogSQL, QueryHandle)
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
  Result := -1;
  ConnectionHandle := GetConnectionHandle();

  ASQL := SQL; //Prepares SQL if needed
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    LogSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(String(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
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
  ASQL := SQL;
  ConnectionHandle := GetConnectionHandle();
  QueryHandle := FPlainDriver.ExecuteQuery(ConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, ConnectionHandle, lcExecute,
    LogSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(LogSQL, QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
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

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
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
  SetPlanNames;
end;

destructor TZPostgreSQLPreparedStatement.Destroy;
begin
  if Assigned(FCachedQuery) then
    FReeAndNil(FCachedQuery);
  inherited Destroy;
end;

function TZPostgreSQLPreparedStatement.CreateResultSet(QueryHandle: Pointer): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, Self.SQL,
  FConnectionHandle, QueryHandle, ChunkSize);

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

{ TZPostgreSQLClassicPreparedStatement }

function TZPostgreSQLClassicPreparedStatement.GetAnsiSQLQuery;
var
  I: Integer;
  ParamIndex: Integer;
  Tokens: TStrings;

  function TokenizeSQLQuery: TStrings;
  var
    I: Integer;
    Tokens: TStrings;
    Temp: string;
  begin
    if FCachedQuery = nil then
    begin
      FCachedQuery := TStringList.Create;
      if Pos('?', SQL) > 0 then
      begin
        Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toUnifyWhitespaces]);
        try
          Temp := '';
          for I := 0 to Tokens.Count - 1 do
          begin
            if Tokens[I] = '?' then
            begin
              FCachedQuery.Add(Temp);
              FCachedQuery.AddObject('?', Self);
              Temp := '';
            end
            else
              Temp := Temp + Tokens[I];
          end;
          if Temp <> '' then
            FCachedQuery.Add(Temp);
        finally
          Tokens.Free;
        end;
      end
      else
        FCachedQuery.Add(SQL);
    end;
    Result := FCachedQuery;
  end;
begin
  ParamIndex := 0;
  Result := '';
  Tokens := TokenizeSQLQuery;

  for I := 0 to Tokens.Count - 1 do
  begin
    if Tokens[I] = '?' then
    begin
      Result := Result + PrepareAnsiSQLParam(ParamIndex, True);
      Inc(ParamIndex);
    end
    else
      Result := Result + ZPlainString(Tokens[I]);
  end;
end;

procedure TZPostgreSQLClassicPreparedStatement.SetPlanNames;
begin
  FPlanName := '"'+IntToStr(Hash(ASQL)+Cardinal(FStatementId)+NativeUInt(FConnectionHandle))+'"';
  FRawPlanName := {$IFDEF UNICODE}RawByteString{$ENDIF}(FPlanName);
end;

function TZPostgreSQLClassicPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer;
  Escaped: Boolean): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
    (Connection as IZPostgreSQLConnection), FPlainDriver, ChunkSize,
    InParamTypes[ParamIndex], Foidasblob, Escaped, True, ConSettings);
end;

procedure TZPostgreSQLClassicPreparedStatement.PrepareInParameters;
var
  I, N: Integer;
  Tokens: TStrings;
  TempSQL: String;
  QueryHandle: PZPostgreSQLResult;
begin
  if Pos('?', SQL) > 0 then
  begin
    Tokens := Connection.GetDriver.GetTokenizer.
      TokenizeBufferToList(SQL, [toUnifyWhitespaces]);
    try
      TempSQL := 'PREPARE '+FPlanName+' AS ';
      N := 0;
      for I := 0 to Tokens.Count - 1 do
      begin
        if Tokens[I] = '?' then
        begin
          Inc(N);
          TempSQL := TempSQL + '$' + IntToStr(N);
        end else
          TempSQL := TempSQL + Tokens[I];
      end;
    finally
      Tokens.Free;
    end;
  end
  else Exit;

  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := TempSQL;
  QueryHandle := FPlainDriver.ExecuteQuery(FConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle, lcPrepStmt,
    SSQL, QueryHandle);
  DriverManager.LogMessage(lcPrepStmt, FPlainDriver.GetProtocol, SSQL);
  FPlainDriver.Clear(QueryHandle);
end;

procedure TZPostgreSQLClassicPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if Self.InParamCount > 0 then
  begin
    if Prepared then
    begin
      FExecSQL := 'EXECUTE '+FRawPlanName+'(';
      for i := 0 to InParamCount -1 do
        if I = 0 then
          FExecSQL := FExecSQL+PrepareAnsiSQLParam(i, False)
        else
          FExecSQL := FExecSQL+','+PrepareAnsiSQLParam(i, False);
      FExecSQL := FExecSQL+');';
    end
    else
      FExecSQL := GetAnsiSQLQuery;
  end
  else
    FExecSQL := ASQL;
end;

procedure TZPostgreSQLClassicPreparedStatement.UnPrepareInParameters;
begin
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) then
  begin
    ASQL := 'DEALLOCATE '+FRawPlanName+';';
    Execute(ASQL);
  end;
end;

procedure TZPostgreSQLClassicPreparedStatement.Prepare;
begin
  { EgonHugeist: assume automated Prepare after third execution. That's the way
    the JDBC Drivers go too... }
  if (not Prepared ) and ( InParamCount > 0 ) and ( ExecCount > 2 ) then
    inherited Prepare;
  BindInParameters;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLClassicPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver,
    FConnectionHandle, lcExecute, SSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, Self.SSQL);
  if QueryHandle <> nil then
    Result := CreateResultSet(QueryHandle)
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
function TZPostgreSQLClassicPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := -1;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle, lcExecute,
    SSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(String(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
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
function TZPostgreSQLClassicPreparedStatement.Execute(const SQL: RawByteString): Boolean;
var
  QueryHandle: PZPostgreSQLResult;
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.ExecuteQuery(FConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle, lcExecute,
    SSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);

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
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZPostgreSQLClassicPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  Result := ExecuteQuery(FExecSQL);
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
function TZPostgreSQLClassicPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  Result := ExecuteUpdate(FExecSQL);
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
function TZPostgreSQLClassicPreparedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  Result := Execute(FExecSQL);
  inherited ExecutePrepared;
end;

{ TZPostgreSQLCAPIPreparedStatement }

function TZPostgreSQLCAPIPreparedStatement.ExectuteInternal(const SQL: RawByteString;
  const LogSQL: String; const LoggingCategory: TZLoggingCategory): PZPostgreSQLResult;
begin
  case LoggingCategory of
    lcPrepStmt:
      begin
        Result := FPlainDriver.Prepare(FConnectionHandle, PAnsiChar(RawByteString(FPlanName)),
          PAnsiChar(SQL), InParamCount, nil);
        Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
          FConnectionHandle, LoggingCategory, LogSQL, Result) = '42P18');
        DriverManager.LogMessage(LoggingCategory, FPlainDriver.GetProtocol, LogSQL);
        if not Findeterminate_datatype then
          FPostgreSQLConnection.RegisterPreparedStmtName(FPlanName);
        Exit;
      end;
    lcExecPrepStmt:
      Result := FPlainDriver.ExecPrepared(FConnectionHandle,
        PAnsiChar(RawByteString(FPlanName)), InParamCount, FPQparamValues,
        FPQparamLengths, FPQparamFormats, 0);
    lcUnprepStmt:
      if Assigned(FConnectionHandle) then
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL))
      else Result := nil;
    else
      Result := FPlainDriver.ExecuteQuery(FConnectionHandle, PAnsiChar(SQL));
  end;
  if Assigned(FConnectionHandle) then
    CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
      LoggingCategory, LogSQL, Result);
  DriverManager.LogMessage(LoggingCategory, FPlainDriver.GetProtocol, LogSQL);
end;
procedure TZPostgreSQLCAPIPreparedStatement.SetPlanNames;
begin
  FPlanName := IntToStr(Hash(ASQL)+Cardinal(FStatementId)+NativeUInt(FConnectionHandle));
  FRawPlanName := {$IFDEF UNICODE}RawByteString{$ENDIF}(FPlanName);
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetASQL(const Value: RawByteString);
begin
  if ( ASQL <> Value ) and Prepared then
    Unprepare;
  inherited SetASQL(Value);
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if ( WSQL <> Value ) and Prepared then
    Unprepare;
  inherited SetWSQL(Value);
end;

procedure TZPostgreSQLCAPIPreparedStatement.PrepareInParameters;
begin
  if not (Findeterminate_datatype) then
  begin
    SetLength(FPQparamValues, InParamCount);
    SetLength(FPQparamLengths, InParamCount);
    SetLength(FPQparamFormats, InParamCount);
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.BindInParameters;
var
  Value: TZVariant;
  TempBlob: IZBlob;
  TempStream: TStream;
  WriteTempBlob: IZPostgreSQLBlob;
  ParamIndex: Integer;
  TempBytes: TByteDynArray;

  procedure UpdateNull(const Index: Integer);
  begin
    FreeMem(FPQparamValues[Index]);

    FPQparamValues[Index] := nil;
    FPQparamLengths[Index] := 0;
    FPQparamFormats[Index] := 0;
  end;

  procedure UpdateString(Value: RawByteString; const Index: Integer);
  begin
    UpdateNull(Index);

    FPQparamValues[ParamIndex] := AllocMem(Length(Value) + 1);
    {$IFDEF WITH_STRPCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrCopy(FPQparamValues[Index], PAnsiChar(Value));
  end;

  procedure UpdateBinary(Value: Pointer; const Len, Index: Integer);
  begin
    UpdateNull(Index);

    FPQparamValues[Index] := AllocMem(Len);
    System.Move(Value^, FPQparamValues[Index]^, Len);
    FPQparamLengths[Index] := Len;
    FPQparamFormats[Index] := 1;
  end;

begin
  if InParamCount <> High(FPQparamValues)+1 then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  for ParamIndex := 0 to InParamCount -1 do
  begin
    Value := InParamValues[ParamIndex];
    if DefVarManager.IsNull(Value)  then
      UpdateNull(ParamIndex)
    else
      case InParamTypes[ParamIndex] of
        stBoolean:
          UpdateString(RawByteString(UpperCase(BoolToStrEx(SoftVarManager.GetAsBoolean(Value)))), ParamIndex);
        stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
          UpdateString(RawByteString(SoftVarManager.GetAsString(Value)), ParamIndex);
        stBytes:
          begin
            TempBytes := SoftVarManager.GetAsBytes(Value);
            UpdateBinary(PAnsiChar(TempBytes), Length(TempBytes), ParamIndex);
          end;
        stString:
          UpdateString(ZPlainString(SoftVarManager.GetAsString(Value), GetConnection.GetEncoding), ParamIndex);
        stUnicodeString:
          UpdateString(ZPlainString(SoftVarManager.GetAsUnicodeString(Value)), ParamIndex);
        stDate:
          UpdateString(RawByteString(FormatDateTime('yyyy-mm-dd', SoftVarManager.GetAsDateTime(Value))), ParamIndex);
        stTime:
          UpdateString(RawByteString(FormatDateTime('hh":"mm":"ss"."zzz', SoftVarManager.GetAsDateTime(Value))), ParamIndex);
        stTimestamp:
          UpdateString(RawByteString(FormatDateTime('yyyy-mm-dd hh":"mm":"ss"."zzz', SoftVarManager.GetAsDateTime(Value))), ParamIndex);
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
            if not TempBlob.IsEmpty then
            begin
              case InParamTypes[ParamIndex] of
                stBinaryStream:
                  if ((GetConnection as IZPostgreSQLConnection).IsOidAsBlob) or
                    StrToBoolDef(Info.Values['oidasblob'], False) then
                  begin
                    TempStream := TempBlob.GetStream;
                    try
                      WriteTempBlob := TZPostgreSQLBlob.Create(FPlainDriver, nil, 0,
                        FConnectionHandle, 0, ChunkSize);
                      WriteTempBlob.SetStream(TempStream);
                      WriteTempBlob.WriteBlob;
                      UpdateString(RawByteString(IntToStr(WriteTempBlob.GetBlobOid)), ParamIndex);
                    finally
                      WriteTempBlob := nil;
                      TempStream.Free;
                    end;
                  end
                  else
                    UpdateBinary(TempBlob.GetBuffer, TempBlob.Length, ParamIndex);
                stAsciiStream, stUnicodeStream:
                  begin
                    UpdateString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings), ParamIndex);
                  end;
              end; {case..}
              TempBlob := nil;
            end
            else
              UpdateNull(ParamIndex);
          end; {if not TempBlob.IsEmpty then}
      end;
  end;
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.UnPrepareInParameters;
var
  I: Integer;
begin
  { release allocated memory }
  if not (Findeterminate_datatype) then
  begin
    for i := 0 to InParamCount-1 do
    begin
      FreeMem(FPQparamValues[i]);
      FPQparamValues[i] := nil;
    end;
    SetLength(FPQparamValues, 0);
    SetLength(FPQparamLengths, 0);
    SetLength(FPQparamFormats, 0);
  end;
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a prepared SQL statement.
}
function TZPostgreSQLCAPIPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
  Tokens: TStrings;
begin
  ParamIndex := 0;
  Result := '';
  Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toUnifyWhitespaces]);

  for I := 0 to Tokens.Count - 1 do
  begin
    if Tokens[I] = '?' then
    begin
      if InParamCount <= ParamIndex then
        raise EZSQLException.Create(SInvalidInputParameterCount);
      Result := Result + PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
        (Connection as IZPostgreSQLConnection), FPlainDriver, ChunkSize,
        InParamTypes[ParamIndex], Foidasblob, True, False, ConSettings);
      Inc(ParamIndex);
    end
    else
      Result := Result + ZPlainString(Tokens[I]);
  end;
  Tokens.Free;
end;

procedure TZPostgreSQLCAPIPreparedStatement.Prepare;
var
  Tokens: TStrings;
  TempSQL: String;
  N, I: Integer;
begin
  if not Prepared then
  begin
    N := 0;
    if Pos('?', SSQL) > 0 then
    begin
      TempSQL := ''; //init
      Tokens := Connection.GetDriver.GetTokenizer.
        TokenizeBufferToList(SSQL, [toUnifyWhitespaces]);
      try
        for I := 0 to Tokens.Count - 1 do
        begin
          if Tokens[I] = '?' then
          begin
            Inc(N);
            TempSQL := TempSQL + '$' + IntToStr(N);
          end else
            TempSQL := TempSQL + Tokens[I];
        end;
      finally
        Tokens.Free;
      end;
    end
    else TempSQL := SSQL;

    if ( N > 0 ) or ( ExecCount > 2 ) then //prepare only if Params are available or certain executions expected
    begin
      QueryHandle := ExectuteInternal(GetEncodedSQL(TempSQL), 'PREPARE '#39+TempSQL+#39, lcPrepStmt);
      if not (Findeterminate_datatype) then
        FPlainDriver.Clear(QueryHandle);
      inherited Prepare;
    end;
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.Unprepare;
var
  TempSQL: String;
begin
  if Prepared then
  begin
    inherited Unprepare;
    if Assigned(FPostgreSQLConnection.GetConnectionHandle) and (not Findeterminate_datatype)  then
    begin
      TempSQL := 'DEALLOCATE "'+FPlanName+'";';
      QueryHandle := ExectuteInternal(RawByteString(TempSQL), TempSQL, lcUnprepStmt);
      FPlainDriver.Clear(QueryHandle);
      FPostgreSQLConnection.UnregisterPreparedStmtName(FPlanName);
    end;
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;

  Prepare;
  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExectuteInternal(PrepareAnsiSQLQuery, SSQL, lcExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecPrepStmt);
    end
  else
    QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecute);
  if QueryHandle <> nil then
    Result := CreateResultSet(QueryHandle)
  else
    Result := nil;
  inherited ExecuteQueryPrepared;
end;

function TZPostgreSQLCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  Prepare;

  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExectuteInternal(PrepareAnsiSQLQuery, SSQL, lcExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecPrepStmt);
    end
  else
    QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecute);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(String(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;

  inherited ExecuteUpdatePrepared;
end;

function TZPostgreSQLCAPIPreparedStatement.ExecutePrepared: Boolean;
var
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  Prepare;

  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExectuteInternal(PrepareAnsiSQLQuery, SSQL, lcExecPrepStmt)
    else
    begin
      BindInParameters;
      QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecPrepStmt);
    end
  else
    QueryHandle := ExectuteInternal(ASQL, SSQL, lcExecute);

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
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(String(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;

  inherited ExecutePrepared;
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
    ConnectionHandle, QueryHandle, ChunkSize);
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

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
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
    LogSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, LogSQL);
  if QueryHandle <> nil then
  begin
    Result := CreateResultSet(SSQL, QueryHandle);
    FetchOutParams(Result);
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
  InParams := GenerateParamsStr(Length(InParamValues));
  Result := Format('SELECT * FROM %s(%s)', [SQL, InParams]);
end;

{**
   Fills the parameter (?) tokens with corresponding parameter value
   @return a prepared SQL query for execution
}
function TZPostgreSQLCallableStatement.FillParams(const ASql: String): RawByteString;
var I: Integer;
  Tokens: TStrings;
  ParamIndex: Integer;
begin
  if Pos('?', ASql) > 0 then
  begin
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(ASql, [toUnifyWhitespaces]);
    try
      ParamIndex := 0;
      for I := 0 to Tokens.Count - 1 do
        if Tokens[I] = '?' then
        begin
          Result := Result + PrepareAnsiSQLParam(ParamIndex);
          Inc(ParamIndex);
        end
        else
          Result := Result + ZPlainString(Tokens[i]);
    finally
      Tokens.Free;
    end;
  end
  else
    Result := GetEncodedSQL(ASql);
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
    SSQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SSQL);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(String(GetPlainDriver.GetCommandTuples(QueryHandle)), 0);
    FetchOutParams(CreateResultSet(SSQL, QueryHandle));
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
  Sets output parameters from a ResultSet
  @param Value a IZResultSet object.
}
procedure TZPostgreSQLCallableStatement.FetchOutParams(ResultSet: IZResultSet);
var
  ParamIndex, I: Integer;
  Temp: TZVariant;
  HasRows: Boolean;
begin
  ResultSet.BeforeFirst;
  HasRows := ResultSet.Next;

  I := 1;
  for ParamIndex := 0 to OutParamCount - 1 do
  begin
    if not (FDBParamTypes[ParamIndex] in [2, 3, 4]) then // ptOutput, ptInputOutput, ptResult
      Continue;

    if I > ResultSet.GetMetadata.GetColumnCount then
      Break;

    if (not HasRows) or (ResultSet.IsNull(I)) then
      DefVarManager.SetNull(Temp)
    else
      case ResultSet.GetMetadata.GetColumnType(I) of
      stBoolean:
        DefVarManager.SetAsBoolean(Temp, ResultSet.GetBoolean(I));
      stByte:
        DefVarManager.SetAsInteger(Temp, ResultSet.GetByte(I));
      stShort:
        DefVarManager.SetAsInteger(Temp, ResultSet.GetShort(I));
      stInteger:
        DefVarManager.SetAsInteger(Temp, ResultSet.GetInt(I));
      stLong:
        DefVarManager.SetAsInteger(Temp, ResultSet.GetLong(I));
      stFloat:
        DefVarManager.SetAsFloat(Temp, ResultSet.GetFloat(I));
      stDouble:
        DefVarManager.SetAsFloat(Temp, ResultSet.GetDouble(I));
      stBigDecimal:
        DefVarManager.SetAsFloat(Temp, ResultSet.GetBigDecimal(I));
      stString:
        DefVarManager.SetAsString(Temp, ResultSet.GetString(I));
      stUnicodeString:
        DefVarManager.SetAsUnicodeString(Temp, ResultSet.GetUnicodeString(I));
      stDate:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetDate(I));
      stTime:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetTime(I));
      stTimestamp:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetTimestamp(I));
      else
        DefVarManager.SetAsString(Temp, ResultSet.GetString(I));
      end;
    OutParamValues[ParamIndex] := Temp;
    Inc(I);
  end;
  ResultSet.BeforeFirst;
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

