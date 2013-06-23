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

uses Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZDbcInterbase6,
  ZDbcInterbase6Utils, ZDbcInterbase6ResultSet,
  ZPlainFirebirdInterbaseConstants,
  ZCompatibility, ZDbcLogging, ZVariant, ZMessages;

type

  {** Implements Generic Interbase6 Statement. }
  TZInterbase6Statement = class(TZAbstractStatement)
  private
    FCachedBlob: boolean;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    function CheckInterbase6Error(const Sql: string = '') : Integer;
  public
    constructor Create(Connection: IZConnection; Info: TStrings);

    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;
    function Execute(const SQL: ZAnsiString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }

  { TZInterbase6PreparedStatement }

  TZInterbase6PreparedStatement = class(TZAbstractPreparedStatement)
  private
    FCachedBlob: boolean;
    FParamSQLData: IZParamsSQLDA;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;

    Cursor: AnsiString;
    SQLData: IZResultSQLDA;
    StmtHandle: TISC_STMT_HANDLE;
    StatementType: TZIbSqlStatementType;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function CheckInterbase6Error(const Sql: string = '') : Integer;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Prepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZInterbase6CallableStatement = class(TZAbstractPreparedCallableStatement)
  private
    FCachedBlob: boolean;
    FParamSQLData: IZParamsSQLDA;
    FResultSQLData: IZResultSQLDA;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(const Sql: string = '');
    procedure FetchOutParams(Value: IZResultSQLDA);
    function GetProcedureSql(SelectProc: boolean): string;

    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses ZSysUtils, ZDbcUtils;

{ TZInterbase6Statement }

{**
   Check interbase error status
   @param Sql the used sql tring

   @return ErrorCode for possible Database Disconnect 
}
function TZInterbase6Statement.CheckInterbase6Error(const Sql: string = '') : Integer;
begin
  Result := ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6Statement.Create(Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
end;

{**
  Destroys this object and cleanups the memory.
}
{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6Statement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
var
  Cursor: AnsiString;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
  iError : Integer; //For closing the database //AVZ
begin
  StmtHandle := 0;
  iError := 0;
  {$IFNDEF UNICODE}ASQL := SQL;{$ENDIF} //preprepares SQL and sets AnsiSQL(ASQL)
  with FIBConnection do
  begin
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ASQL, SSQL, StmtHandle);
//      if not(StatementType in [stSelect, stSelectForUpdate]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
          SSQL, StmtHandle, SQLData);

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, SQLData.GetData);
      iError := CheckInterbase6Error(SSQL);

      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
                  @StmtHandle, PAnsiChar(Cursor), 0);
          CheckInterbase6Error(SSQL);
        end;

        Result := GetCachedResultSet(SSQL, Self,
               TZInterbase6ResultSet.Create(Self, LogSQL, StmtHandle, Cursor, SQLData, nil, FCachedBlob));
      end
        else
      begin
        if (iError <> DISCONNECT_ERROR) then
        begin
          raise EZSQLException.Create(SCanNotRetrieveResultSetData);
        end;
      end;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SSQL);
    except
      on E: Exception do
      begin
       FreeStatement(GetPlainDriver, StmtHandle, DSQL_drop);
       raise;
      end;
    end;
  end;
end;
{$HINTS OFF}

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
{$HINTS OFF}
function TZInterbase6Statement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
var
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := -1;
  StmtHandle := 0;
  with FIBConnection do
  begin
    try
      {$IFNDEF UNICODE}ASQL := SQL;{$ENDIF} //preprepares SQL and sets AnsiSQL(ASQL)
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ASQL, SSQL, StmtHandle);

//      if StatementType in [stExecProc, stSelect, stSelectForUpdate] then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, nil, nil);
      CheckInterbase6Error(SSQL);

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
      else
        begin
          Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
          LastUpdateCount := Result;
        end;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SSQL);
    finally
      FreeStatement(GetPlainDriver, StmtHandle, DSQL_drop);
    end;
  end;
end;
{$HINTS ON}

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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
{$HINTS OFF}
function TZInterbase6Statement.Execute(const SQL: ZAnsiString): Boolean;
var
  Cursor: AnsiString;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := 0;
  with FIBConnection do
  begin
    try
      Result := False;
      {$IFNDEF UNICODE}ASQL := SQL;{$ENDIF} //preprepares SQL and sets AnsiSQL(ASQL)
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ASQL, LogSQL, StmtHandle);

      { Check statement type }
//      if not (StatementType in [stExecProc]) then
//        raise EZSQLException.Create(SStatementIsNotAllowed);

      { Create Result SQLData if statement returns result }
      if StatementType in [stSelect, stExecProc] then
      begin
        SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
        PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect, LogSQL,
          StmtHandle, SQLData);
      end;

      { Execute prepared statement }
      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
              @StmtHandle, GetDialect, nil);
      CheckInterbase6Error(LogSQL);
      { Set updated rows count }
      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Stateent Handle }
      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;

          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
                  @StmtHandle, PAnsiChar(Cursor), 0);
          CheckInterbase6Error(sSQL);
        end;

        LastResultSet := GetCachedResultSet(SSQL, Self,
          TZInterbase6ResultSet.Create(Self, SSQL, StmtHandle, Cursor,
            SQLData, nil, FCachedBlob));
      end
      else
      begin
        LastResultSet := nil;
        FreeStatement(GetPlainDriver, StmtHandle, DSQL_drop);
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SSQL);
    except
      on E: Exception do
      begin
       FreeStatement(GetPlainDriver, StmtHandle, DSQL_drop);
       raise;
      end;
    end;
  end;
end;
{$HINTS ON}

{ TZInterbase6PreparedStatement }

procedure TZInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  With FIBConnection do
    begin
      {create the parameter bind structure}
      FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle, ConSettings);
      {check dynamic sql}
      GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, GetDialect,
        FParamSQLData.GetData);
      ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, lcExecute, SSQL);

      { Resize XSQLDA structure if needed }
      if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
      begin
        FParamSQLData.AllocateSQLDA;
        GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, GetDialect,FParamSQLData.GetData);
        ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, lcExecute, SSQL);
      end;

      FParamSQLData.InitFields(True);
    end;
  inherited PrepareInParameters;
end;

procedure TZInterbase6PreparedStatement.BindInParameters;
begin
  BindSQLDAInParameters(FIBConnection.GetPlainDriver, InParamValues,
    InParamTypes, InParamCount, FParamSQLData, GetConnection.GetConSettings);
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
function  TZInterbase6PreparedStatement.CheckInterbase6Error(const Sql: string) : Integer;
begin
  Result := ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6PreparedStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));

  Prepare;
end;

destructor TZInterbase6PreparedStatement.Destroy;  
begin
  FreeStatement(FIBConnection.GetPlainDriver, StmtHandle, DSQL_drop);
  inherited Destroy;
end;

procedure TZInterbase6PreparedStatement.Prepare;
begin
  StmtHandle := 0;
  with FIBConnection do
  begin
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, GetEncodedSQL(SQL), SQL, StmtHandle);
    if StatementType in [stSelect, stExecProc] then
      begin
        SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle
        , ConSettings);
        PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
          SQL, StmtHandle, SQLData);
      end;
  end;
  CheckInterbase6Error(SQL);
  LogPrepStmtMessage(lcPrepStmt, SQL);
  inherited Prepare;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  if StmtHandle = 0 then Prepare;

  with FIBConnection do
  begin
    try
      BindInParameters;     

      if (StatementType = stSelect) then     //AVZ Get many rows - only need to use execute not execute2
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @StmtHandle,
          GetDialect, FParamSQLData.GetData)
      else
      begin
        CursorName := 'ExecProc'+RandomString(12); //AVZ - Need a way to return one row so we give the cursor a name
        if (SQLData = nil) then
          GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, nil) //not expecting a result
        else
          GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, SQLData.GetData); //expecting a result
      end;

      CheckInterbase6Error(SQL);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Statement Handle }
      if (StatementType in [stSelect, stExecProc])
        and (SQLData.GetFieldCount <> 0) then
      begin
        LastResultSet := GetCachedResultSet(SQL, Self,
        TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
        SQLData, nil, FCachedBlob));
      end
        else
      begin
        LastResultSet := nil;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
    except
      on E: Exception do
      begin
       {if (CursorName <> '') then //AVZ TEST
       begin
         StmtHandle := nil;
       end;}
       FreeStatement(GetPlainDriver, StmtHandle, DSQL_CLOSE); //AVZ
       raise;
      end;
    end;
  end;
  inherited ExecutePrepared;
end;
{$HINTS ON}

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  iError : Integer; //Check for database disconnect AVZ
begin
  if StmtHandle = 0 then Prepare;

  with FIBConnection do
  begin
    try
      BindInParameters;

      if (StatementType = stSelect) then     //AVZ Get many rows - only need to use execute not execute2
      begin
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @StmtHandle,
          GetDialect, FParamSQLData.GetData);
      end
        else
      begin
        CursorName := 'ExecProc'+RandomString(12); //AVZ - Need a way to return one row so we give the cursor a name
        if (SQLData = nil) then
        begin
          GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, nil); //not expecting a result
        end
          else
        begin
          GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, FParamSQLData.GetData, SQLData.GetData); //expecting a result
        end;
      end;

      iError := CheckInterbase6Error(SQL);

      if (StatementType in [stSelect, stExecProc]) and (SQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
                  @StmtHandle, PAnsiChar(Cursor), 0);
          iError := CheckInterbase6Error(SQL);
        end;

        if (iError <> DISCONNECT_ERROR) then
          Result := GetCachedResultSet(SQL, Self, TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor, SQLData, nil, FCachedBlob));
      end
      else
        if (iError <> DISCONNECT_ERROR) then    //AVZ
          raise EZSQLException.Create(SCanNotRetrieveResultSetData)
        else
          Result := nil;
    except
      on E: Exception do
      begin
        //The cursor will be already closed for exec2
        if (Pos('ExecProc', String(CursorName)) <> 0) then
          StmtHandle := 0;

        FreeStatement(GetPlainDriver, StmtHandle, DSQL_CLOSE); //AVZ
        raise;
      end;
    end;
  end;
  inherited ExecuteQueryPrepared;
end;
{$HINTS ON}

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
{$HINTS OFF}
function TZInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
var
  iError : Integer; //Implementation for graceful disconnect AVZ
begin
  Result := -1;

  if StmtHandle = 0 then Prepare;

  with FIBConnection do
  begin
      BindInParameters;

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, FParamSQLData.GetData);
      iError := CheckInterbase6Error(SQL);

      Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
      LastUpdateCount := Result;

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
        stSelect           : FreeStatement(GetPlainDriver, StmtHandle, DSQL_CLOSE);  //AVZ
        stDelete: if (Result = 0) then Result := 1; //AVZ - A delete statement may return zero affected rows, calling procedure expects 1 as Result or Error!
        stUpdate: ; //EgonHugeist - but not a UpdateStatement!
      end;



      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
  end;
  inherited ExecuteUpdatePrepared;

  //Trail for the disconnection of the database gracefully - AVZ
  if (iError = DISCONNECT_ERROR) then
  begin
    Result := DISCONNECT_ERROR;
  end;

end;
{$HINTS ON}


{ TZInterbase6CallableStatement }

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6CallableStatement.CheckInterbase6Error(const Sql: string);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6CallableStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  with FIBConnection do
  begin
    FParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle,
      GetTrHandle, ConSettings);
    FResultSQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle,
      GetTrHandle, ConSettings);
  end;
end;

procedure TZInterbase6CallableStatement.PrepareInParameters;
begin
  with FIBConnection do
  begin
    { Prepare statement }
    FStatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
      GetDBHandle, GetTrHandle, GetDialect, ZPlainString(ProcSql), ProcSQL, FStmtHandle);
    PrepareResultSqlData(GetPlainDriver, GetDBHandle, GetDialect,
      SQL, FStmtHandle, FResultSQLData);
    PrepareParameters(GetPlainDriver, ProcSql, GetDialect, FStmtHandle, FParamSQLData);
  end;
end;

procedure TZInterbase6CallableStatement.BindInParameters;
begin
  BindSQLDAInParameters(FIBConnection.GetPlainDriver, InParamValues,
    InParamTypes, InParamCount, FParamSQLData, ConSettings);
  inherited BindInParameters;
end;

procedure TZInterbase6CallableStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
end;

procedure TZInterbase6CallableStatement.Unprepare;
begin
  inherited Unprepare;
  FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_unprepare);
  if FStmtHandle <> 0 then // Free statement-hande! On the other hand: Exception!
  begin
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_drop);
    FStmtHandle := 0;
  end;
end;

destructor TZInterbase6CallableStatement.Destroy;
begin
  inherited Destroy;
  if FStmtHandle <> 0 then
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_drop);
  FResultSQLData := nil;
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
{$HINTS OFF}
function TZInterbase6CallableStatement.ExecutePrepared: Boolean;
var
  Cursor: AnsiString;
begin
  Result := False;
  with FIBConnection do
  begin
    ProcSql := GetProcedureSql(False);
    BindInParameters;
    try
      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
            GetDialect, FParamSQLData.GetData, Self.FResultSQLData.GetData);
      CheckInterbase6Error(SQL);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType);

      case FStatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Create ResultSet if possible else free Stateent Handle, ResultSQlData and
        ParamSqlData }
      if (FStatementType in [stSelect, stExecProc])
        and (FResultSQLData.GetFieldCount <> 0) then
      begin
        Cursor := RandomString(12);
        LastResultSet := GetCachedResultSet(SQL, Self,
           TZInterbase6ResultSet.Create(Self, SQL, FStmtHandle, Cursor, FResultSQLData, nil, FCachedBlob));
      end
      else
      begin
        { Fetch data and fill Output params }
        FetchOutParams(FResultSQLData);
        FreeStatement(GetPlainDriver, FStmtHandle, DSQL_CLOSE); //AVZ
        LastResultSet := nil;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, SQL);
    except
      on E: Exception do
      begin
       FreeStatement(GetPlainDriver, FStmtHandle, DSQL_CLOSE); //AVZ
       raise;
      end;
    end;
  end;
end;
{$HINTS ON}

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZInterbase6CallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: AnsiString;
begin
  with FIBConnection do
  begin
    ProcSql := GetProcedureSql(True); //Prepares the Statement
    BindInParameters;
    try
      if (FStatementType = stSelect) then     //AVZ Get many rows - only need to use execute not execute2
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData)
      else
      begin
        CursorName := 'ExecProc'+RandomString(12); //AVZ - Need a way to return one row so we give the cursor a name
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, FResultSQLData.GetData);
      end;

      CheckInterbase6Error(ProcSql);

      if (FStatementType in [stSelect, stExecProc]) and (FResultSQLData.GetFieldCount <> 0) then
      begin
        if CursorName <> '' then
        begin
          Cursor := CursorName;
          GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector, @FStmtHandle, PAnsiChar(Cursor), 0);
          CheckInterbase6Error(ProcSql);
        end;

        Result := GetCachedResultSet(ProcSql, Self,
          TZInterbase6ResultSet.Create(Self, ProcSql, FStmtHandle, Cursor,
          FResultSQLData, nil, FCachedBlob));
      end;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, ProcSql);
    except
      on E: Exception do
      begin
        FreeStatement(GetPlainDriver, FStmtHandle, DSQL_unprepare); //AVZ
        raise;
      end;
    end;
  end;
end;
{$HINTS ON}

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
begin
  with FIBConnection do
  begin
    try
      ProcSQL := Self.GetProcedureSql(False);
      BindInParameters;
      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
        GetDialect, FParamSQLData.GetData, FResultSQLData.GetData);
      CheckInterbase6Error(ProcSql);

      Result := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType);
      LastUpdateCount := Result;
      { Fetch data and fill Output params }
      FetchOutParams(FResultSQLData);
      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, GetPlainDriver.GetProtocol, ProcSQL);
    finally
      //FreeStatement(GetPlainDriver, FStmtHandle, DSQL_unprepare); //AVZ -- unprepare the statement - not close it

    end;
  end;
end;

{**
  Set output parameters values from TZResultSQLDA.
  @param Value a TZResultSQLDA object.
}
procedure TZInterbase6CallableStatement.FetchOutParams(
  Value: IZResultSQLDA);
var
  ParamIndex, I: Integer;
  Temp: TZVariant;
begin
  I := 0;
  for ParamIndex := 0 to OutParamCount - 1 do
  begin
    if not (FDBParamTypes[ParamIndex] in [2, 3, 4]) then // ptOutput, ptInputOutput, ptResult
      Continue;

    if I >= Value.GetFieldCount then
      Break;

    if Value.IsNull(I) then
      DefVarManager.SetNull(Temp)
    else
      case Value.GetFieldSqlType(I) of
      stBoolean:
        DefVarManager.SetAsBoolean(Temp, Value.GetBoolean(I));
      stByte:
        DefVarManager.SetAsInteger(Temp, Value.GetByte(I));
      stShort:
        DefVarManager.SetAsInteger(Temp, Value.GetShort(I));
      stInteger:
        DefVarManager.SetAsInteger(Temp, Value.GetInt(I));
      stLong:
        DefVarManager.SetAsInteger(Temp, Value.GetLong(I));
      stFloat:
        DefVarManager.SetAsFloat(Temp, Value.GetFloat(I));
      stDouble:
        DefVarManager.SetAsFloat(Temp, Value.GetDouble(I));
      stBigDecimal:
        DefVarManager.SetAsFloat(Temp, Value.GetBigDecimal(I));
      stString:
        DefVarManager.SetAsString(Temp, ZDbcString(Value.GetString(I)));
      stUnicodeString:
        DefVarManager.SetAsUnicodeString(Temp, ZDbcUnicodeString(Value.GetString(I)));
      stDate:
        DefVarManager.SetAsDateTime(Temp, Value.GetDate(I));
      stTime:
        DefVarManager.SetAsDateTime(Temp, Value.GetTime(I));
      stTimestamp:
        DefVarManager.SetAsDateTime(Temp, Value.GetTimestamp(I));
    end;
    OutParamValues[ParamIndex] := Temp;
    Inc(I);
  end;
end;

{**
   Create sql string for calling stored procedure.
   @param SelectProc indicate use <b>EXECUTE PROCEDURE</b> or
    <b>SELECT</b> staement
   @return a Stored Procedure SQL string 
}
function TZInterbase6CallableStatement.GetProcedureSql(SelectProc: boolean): string;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := ''; //init Result -> FPC
    for I := 0 to Count - 1 do
    begin
      if I > 0 then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: string;
begin
  TrimInParameters;
  InParams := GenerateParamsStr(High(InParamValues) + 1);
  if InParams <> '' then
    InParams := '(' + InParams + ')';

  if SelectProc then
    Result := 'SELECT * FROM ' + SQL + InParams
  else
    Result := 'EXECUTE PROCEDURE ' + SQL + InParams;
end;

end.
