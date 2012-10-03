{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcOracleStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging,
  ZPlainOracleDriver, ZCompatibility, ZVariant, ZDbcOracleUtils,
  ZPlainOracleConstants, Types;

type

  {** Defines a Oracle specific statement. }
  IZOracleStatement = interface(IZStatement)
    ['{8644E5B6-1E0F-493F-B6AC-40D70CCEA13A}']

    function GetStatementHandle: POCIStmt;
  end;

  {** Implements Generic Oracle Statement. }
  TZOracleStatement = class(TZAbstractStatement, IZOracleStatement)
  private
    FPlainDriver: IZOraclePlainDriver;

  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function GetStatementHandle: POCIStmt;
  end;

  {** Implements Prepared SQL Statement. }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FPrepared: Boolean;
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FOracleSQL: string;
    FExecStatement: IZStatement;
    FLastStatement: IZStatement;
    FInVars: PZSQLVars;

    procedure SetLastStatement(LastStatement: IZStatement);
    function GetExecStatement: IZStatement;
    function ConvertToOracleSQLQuery(SQL: string): string;

  protected
    property Prepared: Boolean read FPrepared write FPrepared;
    property Handle: POCIStmt read FHandle write FHandle;
    property ErrorHandle: POCIError read FErrorHandle write FErrorHandle;
    property OracleSQL: string read FOracleSQL write FOracleSQL;
    property ExecStatement: IZStatement read FExecStatement write FExecStatement;
    property LastStatement: IZStatement read FLastStatement write SetLastStatement;
    property InVars: PZSQLVars read FInVars write FInVars;



  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;
    procedure Prepare; override;
    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetStatementHandle: POCIStmt;
  end;

  TZOracleParam = Record
    pName:string;
    pSQLType:Integer;
    pValue: TZVariant;
    pTypeName: String;
    pOut:boolean;
  End;

  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FErrorHandle: POCIError;
    FInVars: PZSQLVars;
    FPlainDriver:IZOraclePlainDriver;
    FOracleSQL: string;
    FPrepared:boolean;
    FHandle: POCIStmt;
    FOracleParams: array of TZOracleParam;
    FOracleParamsCount: Integer;
    procedure ArrangeInParams;
    procedure FetchOutParamsFromOracleVars;
    procedure FetchOutParamsFromResultSet(ResultSet: IZResultSet);
    procedure ExecuteInternal(const AHandle: POCIStmt; AErrorHandle: POCIError;
      OracleVars: PZSQLVars; CurrentParms: TZVariantDynArray; const ExecSQL: String);
  protected
    function GetProcedureSql(SelectProc: boolean): string;
    //function GetFunctionExecuteSql: String;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
  public
    procedure RegisterOutParameter(ParameterIndex: Integer;SQLType: Integer); override;
    procedure Prepare; override;
    function IsNull(ParameterIndex: Integer): Boolean;override;

    Function ExecuteUpdatePrepared: Integer; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    constructor Create(Connection: IZConnection; const pProcName: string; Info: TStrings);
    destructor Destroy; override;
    procedure ClearParameters; override;
  end;

implementation

uses
  ZTokenizer, ZDbcOracle;

{ TZOracleStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOracleStatement.Create(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZOracleStatement.ExecuteQuery(const SQL: string): IZResultSet;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ClientCodePage^.Encoding,
      Connection.PreprepareSQL);
    Result := CreateOracleResultSet(FPlainDriver, Self, SQL,
      Handle, ErrorHandle);
  except
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
    raise;
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
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
function TZOracleStatement.ExecuteUpdate(const SQL: string): Integer;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle,
      ErrorHandle, StrToIntDef(Info.Values['prefetch_count'], 100),
      ClientCodePage^.Encoding, Connection.PreprepareSQL);
    ExecuteOracleStatement(FPlainDriver, Connection, SQL, Handle, ErrorHandle);
    Result := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  finally
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

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
function TZOracleStatement.Execute(const SQL: string): Boolean;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
  StatementType: ub2;
begin
  Result := False;
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);

  try
    PrepareOracleStatement(FPlainDriver, SQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ClientCodePage^.Encoding,
      Connection.PreprepareSQL);

    StatementType := 0;
    FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
      OCI_ATTR_STMT_TYPE, ErrorHandle);

    if StatementType = OCI_STMT_SELECT then
    begin
      LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
        SQL, Handle, ErrorHandle);
      Result := LastResultSet <> nil;
    end
    else
    begin
      ExecuteOracleStatement(FPlainDriver, Connection, SQL,
        Handle, ErrorHandle);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
    end;
  finally
    if not Result then
    begin
      FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
    end;
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Gets statement handle.
  @return statement handle.
}
function TZOracleStatement.GetStatementHandle: POCIStmt;
begin
  Result := nil;
end;

{ TZOraclePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOraclePreparedStatement.Create(
  PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  FOracleSQL := ConvertToOracleSQLQuery(SQL);
  FPrepared := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOraclePreparedStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Sets a reference to the last statement.
  @param LastStatement the last statement interface.
}
procedure TZOraclePreparedStatement.SetLastStatement(
  LastStatement: IZStatement);
begin
  if FLastStatement <> nil then
    FLastStatement.Close;
  FLastStatement := LastStatement;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZOraclePreparedStatement.GetExecStatement: IZStatement;
begin
  if ExecStatement = nil then
  begin
    ExecStatement := TZOracleStatement.Create(FPlainDriver, Connection, Info);

    ExecStatement.SetMaxFieldSize(GetMaxFieldSize);
    ExecStatement.SetMaxRows(GetMaxRows);
    ExecStatement.SetEscapeProcessing(EscapeProcessing);
    ExecStatement.SetQueryTimeout(GetQueryTimeout);
    ExecStatement.SetCursorName(CursorName);

    ExecStatement.SetFetchDirection(GetFetchDirection);
    ExecStatement.SetFetchSize(GetFetchSize);
    ExecStatement.SetResultSetConcurrency(GetResultSetConcurrency);
    ExecStatement.SetResultSetType(GetResultSetType);
  end;
  Result := ExecStatement;
end;

{**
  Converts an SQL query into Oracle format.
  @param SQL a query with parameters defined with '?'
  @returns a query with parameters in Oracle format ':pN'.
}
function TZOraclePreparedStatement.ConvertToOracleSQLQuery(SQL: string): string;
var
  I, N: Integer;
  Tokens: TStrings;
begin
  if Pos('?', SQL) > 0 then
  begin
    Tokens := Connection.GetDriver.GetTokenizer.
      TokenizeBufferToList(SQL, [toUnifyWhitespaces]);
    try
      Result := '';
      N := 0;
      for I := 0 to Tokens.Count - 1 do
      begin
        if Tokens[I] = '?' then
        begin
          Inc(N);
          Result := Result + ':P' + IntToStr(N);
        end else
          Result := Result + Tokens[I];
      end;
    finally
      Tokens.Free;
    end;
  end else
    Result := SQL;
end;

{**
  Closes this statement and frees all resources.
}
procedure TZOraclePreparedStatement.Close;
begin
  inherited Close;
  if LastStatement <> nil then
  begin
    FLastStatement.Close;
    FLastStatement := nil;
  end;
  FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  FreeOracleSQLVars(FPlainDriver, FInVars, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle);
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
function TZOraclePreparedStatement.Execute(const SQL: string): Boolean;
begin
  LastStatement := GetExecStatement;
  Result := LastStatement.Execute(SQL);
  if Result then
    LastResultSet := LastStatement.GetResultSet
  else
    LastUpdateCount := LastStatement.GetUpdateCount;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQuery(const SQL: string): IZResultSet;
begin
  Result := GetExecStatement.ExecuteQuery(SQL);
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
function TZOraclePreparedStatement.ExecuteUpdate(const SQL: string): Integer;
begin
  Result := GetExecStatement.ExecuteUpdate(SQL);
  LastUpdateCount := Result;
end;

{**
  Prepares an SQL statement
}
procedure TZOraclePreparedStatement.Prepare;
var
  I: Integer;
  Status: Integer;
  TypeCode: ub2;
  CurrentVar: PZSQLVar;
begin
  if not Prepared then
  begin
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
    begin
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    end;

    PrepareOracleStatement(FPlainDriver, OracleSQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ClientCodePage^.Encoding,
      Connection.PreprepareSQL);

    AllocateOracleSQLVars(FInVars, InParamCount);
    InVars^.ActualNum := InParamCount;

    for I := 0 to InParamCount - 1 do
    begin
      CurrentVar := @FInVars.Variables[I + 1];
      CurrentVar.Handle := nil;

      { Artificially define Oracle internal type. }
      if InParamTypes[I] = stBinaryStream then
        TypeCode := SQLT_BLOB
      else if InParamTypes[I] = stAsciiStream then
        TypeCode := SQLT_CLOB
      else if InParamTypes[I] = stUnicodeStream then
        TypeCode := SQLT_CLOB
      else TypeCode := SQLT_STR;

      InitializeOracleVar(FPlainDriver, Connection, CurrentVar,
        InParamTypes[I], TypeCode, 1024);

      Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
        FErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
        CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil,
        OCI_DEFAULT);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, SQL);
    end;

    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);
    Prepared := True;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecutePrepared: Boolean;
var
  StatementType: ub2;
begin
  Result := False;

  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues, ChunkSize);

  StatementType := 0;
  FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
    OCI_ATTR_STMT_TYPE, ErrorHandle);

  if StatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
      SQL, Handle, ErrorHandle);
    Result := LastResultSet <> nil;
  end
  else
  begin
    { Executes the statement and gets a result. }
    ExecuteOracleStatement(FPlainDriver, Connection, OracleSQL,
      Handle, ErrorHandle);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, OracleSQL);

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);

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
function TZOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues,ChunkSize);

  { Executes the statement and gets a resultset. }
  Result := CreateOracleResultSet(FPlainDriver, Self, SQL,
    Handle, ErrorHandle);

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SQL);

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);
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
function TZOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  StatementType: ub2;
  ResultSet: IZResultSet;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, ErrorHandle,
    FInVars, InParamValues, ChunkSize);

  try
    StatementType := 0;
    FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
      OCI_ATTR_STMT_TYPE, ErrorHandle);

    if StatementType = OCI_STMT_SELECT then
    begin
      { Executes the statement and gets a resultset. }
      ResultSet := CreateOracleResultSet(FPlainDriver, Self,
        SQL, Handle, ErrorHandle);
      try
        while ResultSet.Next do;
        LastUpdateCount := ResultSet.GetRow;
      finally
        ResultSet.Close;
      end;
    end
    else
    begin
      { Executes the statement and gets a result. }
      ExecuteOracleStatement(FPlainDriver, Connection, OracleSQL,
        Handle, ErrorHandle);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
    end;
    Result := LastUpdateCount;

    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, OracleSQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;

{**
  Gets statement handle.
  @return statement handle.
}
function TZOraclePreparedStatement.GetStatementHandle: POCIStmt;
begin
  Result := FHandle;
end;



procedure TZOracleCallableStatement.Prepare;
  procedure PrepareStatmentHandles(var AHandle, AErrorHandle: Pointer;
    var OracleVars: PZSQLVars; const AParamCount: Integer; const PrepareSQL: String);
  var
    I: Integer;
    Status: Integer;
    TypeCode: ub2;
    CurrentVar: PZSQLVar;
    SQLType:TZSQLType;
  begin
    { Allocates statement handles. }
    if (AHandle = nil) or (AErrorHandle = nil) then
    begin
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        AHandle, AErrorHandle);
    end;

    PrepareOracleStatement(FPlainDriver, PrepareSQL, AHandle, AErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ClientCodePage^.Encoding,
    Connection.PreprepareSQL);
    AllocateOracleSQLVars(OracleVars, AParamCount);
    OracleVars^.ActualNum := AParamCount;

    for I := 0 to AParamCount - 1 do
    begin
      CurrentVar := @OracleVars.Variables[I + 1];
      CurrentVar.Handle := nil;
      SQLType := TZSQLType(FOracleParams[I].pSQLType);

    { Artificially define Oracle internal type. }
      if SQLType = stBinaryStream then
        TypeCode := SQLT_BLOB
      else if SQLType in [stAsciiStream, stUnicodeStream] then
        TypeCode := SQLT_CLOB
      else TypeCode := SQLT_STR;

      InitializeOracleVar(FPlainDriver, Connection, CurrentVar,
        SQLType, TypeCode, 1024);

      Status := FPlainDriver.BindByPos(AHandle, CurrentVar.BindHandle,
        AErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
        CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil,
        OCI_DEFAULT);
      CheckOracleError(FPlainDriver, AErrorHandle, Status, lcExecute, PrepareSQL);
    end;
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, PrepareSQL);
  end;

{var
  I, InCount: Integer;
  TempVar: TZVariant;}
begin
  if not FPrepared then
  begin
    FOracleSQL := GetProcedureSql(False);
      {TrimInParameters;
      FOracleParamsCount := InParamCount;
      InCount := 0;
      for i := 0 to InParamCount -1 do
        if FDBParamTypes[i] in [1,3] then
          Inc(InCount);
      if HasOutParameter then
      begin
        PrepareStatmentHandles(FExecHandle, FExecErrorHandle, FExecVars, InCount, GetFunctionExecuteSql);
        PrepareStatmentHandles(FSelectHandle, FSelectErrorHandle, FSelectVars, 0, GetFunctionSelectSql);
      end
      else
        PrepareStatmentHandles(FHandle, FErrorHandle, FInVars, FOracleParamsCount, FOracleSQL);
    end
    else}
      PrepareStatmentHandles(FHandle, FErrorHandle, FInVars, FOracleParamsCount, FOracleSQL);
    FPrepared := True;
  end;
end;

procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  if ParameterIndex > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex;

  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex);
  with FOracleParams[ParameterIndex-1] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+IntToStr(ParameterIndex);
    pSQLType := SQLType;
    pOut := true;
  end;
end;

procedure TZOracleCallableStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
var AConnection: IZConnection;
begin
  inherited SetInParam(ParameterIndex, SQLType, Value);
  if ParameterIndex > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex;

  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex);
  with FOracleParams[ParameterIndex-1] do
  begin
    AConnection := GetConnection;
    if Assigned(AConnection) and ( not AConnection.UseMetadata ) then
      pName := 'p'+IntToStr(ParameterIndex);
    pSQLType := ord(SQLType);
    pValue := Value;
    pOut := FDBParamTypes[ParameterIndex] in [2,3,4];
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
begin
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex+1);
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
end;

procedure TZOracleCallableStatement.ArrangeInParams;
var
  I: Integer;
begin
  if IsFunction then
    if Length(FOracleParams) > 1 then
    {now move Returnvalue to first position}
    begin
      SetInParamCount(Length(InParamValues)+1);
      for i := High(InParamValues) downto 1 do
        InParamValues[I] := InParamValues[I-1];
      DefVarManager.SetNull(InParamValues[0]);
    end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  CurrentVar: PZSQLVar;
  I, StartFrom, Align: integer;
  procedure SetOutParam(CurrentVar: PZSQLVar; Index: Integer);
  var
    OracleConnection :IZOracleConnection;
    Year:SmallInt;
    Month, Day:Byte; Hour, Min, Sec:ub1; MSec: ub4;
    dTmp:TDateTime;
    ps: PAnsiChar;
  begin
    case CurrentVar.TypeCode of
      SQLT_INT: DefVarManager.SetAsInteger( outParamValues[Index], PLongInt(CurrentVar.Data)^ );
      SQLT_FLT:  DefVarManager.SetAsFloat( outParamValues[Index], PDouble(CurrentVar.Data)^ );
      SQLT_STR:
        begin
          GetMem(ps,1025);
          try
            StrLCopy( ps, (CurrentVar.Data), 1024);
            DefVarManager.SetAsString( OutParamValues[Index], ZDbcString(ps) );
          finally
            FreeMem(ps);
          end;
        end;
      SQLT_TIMESTAMP:
      begin
        OracleConnection := Connection as IZOracleConnection;
        FPlainDriver.DateTimeGetDate(
          OracleConnection.GetConnectionHandle ,
          FErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
          Year, Month, Day);
        FPlainDriver.DateTimeGetTime(
          OracleConnection.GetConnectionHandle ,
          FErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
          Hour, Min, Sec,MSec);
        dTmp := EncodeDate(year,month,day )+EncodeTime(Hour,min,sec,msec) ;
        DefVarManager.SetAsDateTime( outParamValues[Index], dTmp );
      end;
    end;
  end;
begin
  if IsFunction and HasOutParameter then
  begin
    StartFrom := 1;
    Align := -1;
  end
  else
  begin
    StartFrom := 0;
    Align := 0;
  end;

  for I := StartFrom to FOracleParamsCount -1 do
    if FOracleParams[I].pOut then
    begin
      CurrentVar:= @FInVars.Variables[I+1];
      CurrentVar.Data := CurrentVar.DupData;
      SetOutParam(CurrentVar, I+Align);
    end;

  if IsFunction and HasOutParameter then
  begin
    CurrentVar:= @FInVars.Variables[1];
    CurrentVar.Data := CurrentVar.DupData;
    SetOutParam(CurrentVar, High(outParamValues));
  end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromResultSet(ResultSet: IZResultSet);
var
  ParamIndex, I: Integer;
  Temp: TZVariant;
  HasRows: Boolean;
begin
  //ResultSet.BeforeFirst;
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
      stString, stAsciiStream:
        DefVarManager.SetAsString(Temp, ResultSet.GetString(I));
      stUnicodeString, stUnicodeStream:
        DefVarManager.SetAsUnicodeString(Temp, ResultSet.GetUnicodeString(I));
      stDate:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetDate(I));
      stTime:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetTime(I));
      stTimestamp:
        DefVarManager.SetAsDateTime(Temp, ResultSet.GetTimestamp(I));
      stBinaryStream:
        DefVarManager.SetAsInterface(Temp, ResultSet.GetBlob(I));
      else
        DefVarManager.SetAsString(Temp, ResultSet.GetString(I));
      end;
    OutParamValues[ParamIndex] := Temp;
    Inc(I);
  end;
  //ResultSet.BeforeFirst;
end;

procedure TZOracleCallableStatement.ExecuteInternal(const AHandle: POCIStmt;
  AErrorHandle: POCIError; OracleVars: PZSQLVars; CurrentParms: TZVariantDynArray;
  const ExecSQL: String);
var
  StatementType: ub2;
begin
  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver , Connection, AErrorHandle,
    OracleVars, CurrentParms, ChunkSize);

  try
    StatementType := 0;
    FPlainDriver.AttrGet(AHandle, OCI_HTYPE_STMT, @StatementType, nil,
      OCI_ATTR_STMT_TYPE, AErrorHandle);

    case StatementType of
      OCI_STMT_SELECT:
        begin
          { Executes the statement and gets a resultset. }
          LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
            ExecSQL, AHandle, AErrorHandle);
          FetchOutParamsFromResultSet(LastResultSet);
        end;
      OCI_STMT_DECLARE:
        ExecuteOracleStatement(FPlainDriver, Connection, ExecSQL,
          AHandle, AErrorHandle);
      OCI_STMT_BEGIN:
        begin
          ExecuteOracleStatement(FPlainDriver, Connection, ExecSQL,
            AHandle, AErrorHandle);
          LastUpdateCount := GetOracleUpdateCount(FPlainDriver, AHandle, AErrorHandle);
          FetchOutParamsFromOracleVars;
        end;
    end;
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, ExecSQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(OracleVars);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
end;

function TZOracleCallableStatement.GetProcedureSql(SelectProc: boolean): string;
var
  sFunc: string;

  function GenerateParamsStr(Count: integer): string;
    var
      I: integer;
    begin
      for I := 0 to Count - 1 do
      begin
        if ( FDBParamTypes[I] ) = 4 then //ptResult
        begin
          sFunc := ' :'+FOracleParams[I].pName+' := ';
          continue;
        end;
        if Result <> '' then
          Result := Result + ',';
        Result := Result + ':'+FOracleParams[I].pName;
      end;
      Result := '('+Result+')'
    end;

var
  InParams: string;
begin
  sFunc := '';
  InParams := GenerateParamsStr( FOracleParamsCount );
  if IsFunction then
    Result := 'BEGIN  ' + sFunc +' "'+SQL +'"'+ InParams+'; END;'
  //  Result := 'SELECT "'+SQL+'"'+InParams+' AS '+FOracleParams[High(FOracleParams)].pName+' FROM DUAL'
  else
    {if SelectProc then
      Result := 'SELECT * FROM "' + SQL +'"'+ InParams+lineending
    else}
      Result := 'BEGIN  ' + sFunc +' '+SQL + InParams+'; END;';
end;

{function TZOracleCallableStatement.GetFunctionExecuteSql: String;
  function GenerateParamsStr: string;
  var
    I: integer;
  begin
    for I := 0 to High(FOracleParams) do
    begin
      if (Result <> '') and not (FDBParamTypes[I] = 4) then
        Result := Result + ',';
      case FDBParamTypes[I] of
        1: //ptInput
          Result := Result + ':'+FOracleParams[I].pName;
        2,3: //ptOutput, ptInputOutput
          Result := Result + FOracleParams[I].pName;
        4: //ptResult
          Continue;
      end;
    end;
    Result := '('+Result+')'
  end;

  function GetFunctionDeclarationSql: string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to High(FOracleParams) do
      if ( FDBParamTypes[I] ) in [2,4] then //ptOutput, ptResult
        if TZSQLType(FOracleParams[I].pSQLType) in [stString, stUnicodeString] then
          Result := Result + FOracleParams[I].pName+' '+FOracleParams[I].pTypeName+'(1024);'#10#13
        else
          Result := Result + FOracleParams[I].pName+' '+FOracleParams[I].pTypeName+';'#10#13
      else
        if FDBParamTypes[I] = 3 then //ptInputOutput
          if TZSQLType(FOracleParams[I].pSQLType) in [stString, stUnicodeString] then
            Result := Result + FOracleParams[I].pName+' '+FOracleParams[I].pTypeName+'(1024) := :'+FOracleParams[I].pName+';'#10#13
          else
            Result := Result + FOracleParams[I].pName+' '+FOracleParams[I].pTypeName+' := :'+FOracleParams[I].pName+';'#10#13;
    Result := 'DECLARE '+Result;
  end;
begin
  Result := 'BEGIN '+FOracleParams[High(FOracleParams)].pName+' := "'+SQL+'"'+GenerateParamsStr+'; END;';
  Result := GetFunctionDeclarationSql+ Result;
end;}

function TZOracleCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  result := inherited IsNull(ParameterIndex);
end;

procedure TZOracleCallableStatement.ClearParameters;
begin
  inherited;
  FOracleParamsCount := 0;
  SetLength(FOracleParams, 0);
end;

constructor TZOracleCallableStatement.Create(Connection: IZConnection;
  const pProcName: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FOracleParamsCount := 0;
  SQL := pProcName;
  FPlainDriver := Connection.GetIZPlainDriver as IZOraclePlainDriver;
  ResultSetType := rtForwardOnly;
  FPrepared := False;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
  {function CopyInAndInOutVars: TZVariantDynArray;
  var
    I: Integer;
  begin
    for i := 0 to high(InParamValues) do
      if FDBParamTypes[i] in [1,3] then
      begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)] := InParamValues[i];
      end;
  end;}
begin
  ArrangeInParams; //Need to sort InParams for Functions
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  {if IsFunction and HasOutParameter then
  begin
    ExecuteInternal(FExecHandle, FExecErrorHandle, FExecVars, CopyInAndInOutVars, ExecSQL);
    ExecuteInternal(FSelectHandle, FSelectErrorHandle, FSelectVars, nil, SelectSQL);
    Result := 1;
    while LastResultSet.Next do inc(Result);
  end
  else}
  ExecuteInternal(FHandle, FErrorHandle, FInVars, InParamValues, FOracleSQL);
  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  ArrangeInParams; //Need to sort InParams for Functions
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  ExecuteInternal(FHandle, FErrorHandle, FInVars, InParamValues, FOracleSQL);
  Result := nil; {temporay}
end;

end.
