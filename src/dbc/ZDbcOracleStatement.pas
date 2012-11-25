{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;
    function Execute(const SQL: ZAnsiString): Boolean; override;

    function GetStatementHandle: POCIStmt;
  end;

  {** Implements Prepared SQL Statement. }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FPrepared: Boolean;
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FExecStatement: IZStatement;
    FLastStatement: IZStatement;
    FInVars: PZSQLVars;

    procedure SetLastStatement(LastStatement: IZStatement);
    function GetExecStatement: IZStatement;
    function ConvertToOracleSQLQuery(SQL: string): ZAnsiString;

  protected
    property Prepared: Boolean read FPrepared write FPrepared;
    property Handle: POCIStmt read FHandle write FHandle;
    property ErrorHandle: POCIError read FErrorHandle write FErrorHandle;
    property ExecStatement: IZStatement read FExecStatement write FExecStatement;
    property LastStatement: IZStatement read FLastStatement write SetLastStatement;
    property InVars: PZSQLVars read FInVars write FInVars;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Close; override;
    procedure Prepare; override;
    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;
    function Execute(const SQL: ZAnsiString): Boolean; override;

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
    pType: ShortInt;
    pProcIndex: Integer;
  End;

  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FErrorHandle: POCIError;
    FInVars: PZSQLVars;
    FPlainDriver:IZOraclePlainDriver;
    FPrepared:boolean;
    FHandle: POCIStmt;
    FOracleParams: array of TZOracleParam;
    FOracleParamsCount: Integer;
    FParamNames: TStringDynArray;
    PackageIncludedList: TStrings;
    FFunctionResultOffsetsIn: array of ShortInt;
    FFunctionResultOffsetsOut: TIntegerDynArray;
    procedure ArrangeInParams;
    procedure FetchOutParamsFromOracleVars;
  protected
    function GetProcedureSql(SelectProc: boolean): ZAnsiString;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
  public
    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); override;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer); override;
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
  ZTokenizer, ZDbcOracle, ZDbcOracleResultSet;

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
function TZOracleStatement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);
  ASQL := SQL;
  try
    PrepareOracleStatement(FPlainDriver, ASQL, LogSQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ConSettings);
    Result := CreateOracleResultSet(FPlainDriver, Self, LogSQL,
      Handle, ErrorHandle);
  except
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
    raise;
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
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
function TZOracleStatement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
begin
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);
  ASQL := SQL;
  try
    PrepareOracleStatement(FPlainDriver, ASQL, LogSQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ConSettings);
    ExecuteOracleStatement(FPlainDriver, Connection, LogSQL, Handle, ErrorHandle);
    Result := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  finally
    FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

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
function TZOracleStatement.Execute(const SQL: ZAnsiString): Boolean;
var
  Handle: POCIStmt;
  ErrorHandle: POCIError;
  StatementType: ub2;
begin
  Result := False;
  AllocateOracleStatementHandles(FPlainDriver, Connection, Handle, ErrorHandle);
  ASQL := SQL;
  try
    PrepareOracleStatement(FPlainDriver, ASQL, LogSQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ConSettings);

    StatementType := 0;
    FPlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @StatementType, nil,
      OCI_ATTR_STMT_TYPE, ErrorHandle);

    if StatementType = OCI_STMT_SELECT then
    begin
      LastResultSet := CreateOracleResultSet(FPlainDriver, Self,
        LogSQL, Handle, ErrorHandle);
      Result := LastResultSet <> nil;
    end
    else
    begin
      ExecuteOracleStatement(FPlainDriver, Connection, LogSQL,
        Handle, ErrorHandle);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
    end;
  finally
    if not Result then
      FreeOracleStatementHandles(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

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
  ASQL := ConvertToOracleSQLQuery(SQL);
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
function TZOraclePreparedStatement.ConvertToOracleSQLQuery(SQL: string): ZAnsiString;
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
          Result := Result + ':P' + ZAnsiString(IntToStr(N));
        end else
          Result := Result + ZPlainString(Tokens[I]);
      end;
    finally
      Tokens.Free;
    end;
  end else
    Result := GetEncodedSQL(SQL);
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
function TZOraclePreparedStatement.Execute(const SQL: ZAnsiString): Boolean;
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
function TZOraclePreparedStatement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
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
function TZOraclePreparedStatement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
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

    PrepareOracleStatement(FPlainDriver, ASQL, LogSQL, Handle, ErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ConSettings);

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
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, LogSQL);
    end;

    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
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
    ExecuteOracleStatement(FPlainDriver, Connection, LogSQL,
      Handle, ErrorHandle);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
  end;

  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

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
      ExecuteOracleStatement(FPlainDriver, Connection, LogSQL,
        Handle, ErrorHandle);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, Handle, ErrorHandle);
    end;
    Result := LastUpdateCount;

    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
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
var
  I: Integer;
  Status: Integer;
  TypeCode: ub2;
  CurrentVar: PZSQLVar;
  SQLType:TZSQLType;
begin
  if not FPrepared then
  begin
    ASQL := GetProcedureSql(False);
    SetLength(FParamNames, FOracleParamsCount);
    for i := 0 to FOracleParamsCount -1 do
      FParamNames[I] := Self.FOracleParams[I].pName;

    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
    begin
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    end;

    PrepareOracleStatement(FPlainDriver, ASQL, LogSQL, FHandle, FErrorHandle,
      StrToIntDef(Info.Values['prefetch_count'], 100), ConSettings);
    //make sure eventual old buffers are cleaned
    FreeOracleSQLVars(FPlainDriver, FInVars, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle);
    AllocateOracleSQLVars(FInVars, FOracleParamsCount);
    FInVars^.ActualNum := FOracleParamsCount;

    for I := 0 to FOracleParamsCount - 1 do
    begin
      CurrentVar := @FInVars.Variables[I + 1];
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

      Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
        FErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
        CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil,
        OCI_DEFAULT);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, LogSQL);
    end;
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
  end;
end;


procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  if FOracleParams[ParameterIndex-1].pType = 4 then
  begin
    SetLength(FFunctionResultOffsetsOut, Length(FFunctionResultOffsetsOut)+1);
    FFunctionResultOffsetsOut[High(FFunctionResultOffsetsOut)] := ParameterIndex-1;
  end;
  with FOracleParams[ParameterIndex-1] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+IntToStr(ParameterIndex);
    pSQLType := SQLType;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamType(ParameterIndex:integer;ParamType:Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex);
  if ParameterIndex > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex;
  FOracleParams[ParameterIndex-1].pType := ParamType;
end;

procedure TZOracleCallableStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
var AConnection: IZConnection;
begin
  inherited SetInParam(ParameterIndex, SQLType, Value);
  with FOracleParams[ParameterIndex-1] do
  begin
    AConnection := GetConnection;
    if Assigned(AConnection) and ( not AConnection.UseMetadata ) then
      pName := 'p'+IntToStr(ParameterIndex);
    pSQLType := ord(SQLType);
    pValue := Value;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
var
  iPos, I: Integer;
  ProcName: String;
begin
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
  iPos := Pos('.', ParamName);
  if iPos > 0 then
  begin
    ProcName := Copy(ParamName, 1, iPos-1); //extract function or Procedure names
    FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.IndexOf(ProcName);
    if  FOracleParams[ParameterIndex].pProcIndex = -1 then //Check if exists
      FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.Add(ProcName); //Add to List if not exists
    if FOracleParams[ParameterIndex].pType = 4 then //Returnvalues are coming in first
    begin
      for i := 0 to high(FOracleParams) do //Now get First offset of function
        if FOracleParams[ParameterIndex].pProcIndex = FOracleParams[I].pProcIndex then Break;
      SetLength(FFunctionResultOffsetsIn, Length(FFunctionResultOffsetsIn)+1);
      FFunctionResultOffsetsIn[High(FFunctionResultOffsetsIn)] := I;
    end;
  end
  else //No package
    if Self.FDBParamTypes[ParameterIndex] = 4 then
    begin
      SetLength(FFunctionResultOffsetsIn, 1);
      FFunctionResultOffsetsIn[0] := 0;
    end;

end;

procedure TZOracleCallableStatement.ArrangeInParams;
var
  I, NewProcIndex, VarStartProcIndex: Integer;
  TempVars: TZVariantDynArray;

  procedure MoveAndSetNullVariant(ToPos: Integer; AVar: TZVariant);
  var
    I: Integer;
  begin
    for i := High(TempVars) downto ToPos+1 do
      TempVars[I] := TempVars[I-1];
    TempVars[ToPos] := AVar;
    DefVarManager.SetNull(TempVars[ToPos]);
  end;
begin
  if PackageIncludedList.Count > 0 then
  begin
    SetLength(TempVars, Length(FOracleParams));
    NewProcIndex := -1;
    VarStartProcIndex := 0;
    for i := 0 to high(FOracleParams) do
    begin
      if NewProcIndex <> FOracleParams[i].pProcIndex then
      begin
        NewProcIndex := FOracleParams[i].pProcIndex;
        VarStartProcIndex := i;
      end;
      if FOracleParams[I].pType in [1,2,3] then
        TempVars[i] := FOracleParams[i].pValue
      else
        MoveAndSetNullVariant(VarStartProcIndex, FOracleParams[i].pValue); //move Last to First
    end;
    InParamValues := TempVars;
  end
  else
    if IsFunction then
      if Length(FOracleParams) > 1 then
      {now move Returnvalue to first position}
      begin
        if Length(InParamValues) < Length(FOracleParams) then
          SetInParamCount(Length(InParamValues)+1);
        for i := High(InParamValues) downto 1 do
          InParamValues[I] := InParamValues[I-1];
        DefVarManager.SetNull(InParamValues[0]);
      end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  CurrentVar: PZSQLVar;
  LobLocator: POCILobLocator;
  I,Align, ResultOffSetCount: integer;
  TempBlob: IZBlob;

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
      SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          if CurrentVar.Indicator >= 0 then
            LobLocator := PPOCIDescriptor(CurrentVar.Data)^
          else
            LobLocator := nil;

          OracleConnection := Connection as IZOracleConnection;
          TempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0, OracleConnection, LobLocator,
            CurrentVar.ColType, GetChunkSize);
          (TempBlob as IZOracleBlob).ReadBlob;
          DefVarManager.SetAsInterface(outParamValues[Index], TempBlob);
          TempBlob := nil;
        end;
      SQLT_NTY:
        DefVarManager.SetAsInterface(outParamValues[Index], TZOracleBlob.CreateWithStream(nil, GetConnection));
      end;
  end;
begin
  ResultOffSetCount := 0;
  Align := 1;

  for I := 0 to FOracleParamsCount -1 do
    if ( Length(FFunctionResultOffsetsIn) > 0) and (FOracleParams[I].pType = 4) then
    begin //ptResult
      CurrentVar:= @FInVars.Variables[FFunctionResultOffsetsIn[ResultOffSetCount]+1];
      CurrentVar.Data := CurrentVar.DupData;
      SetOutParam(CurrentVar, Self.FFunctionResultOffsetsOut[ResultOffSetCount]);
      Inc(ResultOffSetCount);
      Inc(Align);
    end
    else
      if FOracleParams[I].pType in [2,3] then
      begin
        if ( Length(FFunctionResultOffsetsIn) > 0) and
           (FFunctionResultOffsetsIn[ResultOffSetCount] = i) then
          Inc(Align); //Jump over ReturnValue
        CurrentVar:= @FInVars.Variables[I+Align];
        CurrentVar.Data := CurrentVar.DupData;
        SetOutParam(CurrentVar, I-ResultOffSetCount);
      end;
end;

function TZOracleCallableStatement.GetProcedureSql(SelectProc: boolean): ZAnsiString;
var
  sFunc: string;
  I, IncludeCount: Integer;
  PackageBody: TStrings;
  TempResult: String;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    for I := 0 to Count - 1 do
    begin
      if ( FDBParamTypes[I] = 4 ) then //ptResult
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
  if PackageIncludedList.Count > 0 then
  begin
    PackageBody := TStringList.Create;
    PackageBody.Add('BEGIN');
    for IncludeCount := 0 to PackageIncludedList.Count -1 do
    begin
      InParams := '';
      sFunc := '';
      for i := 0 to high(FOracleParams) do
      begin
        if IncludeCount = FOracleParams[i].pProcIndex then
          if ( FDBParamTypes[I] = 4 ) then //ptResult
            sFunc := ' :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])+' := '
          else
            if InParams <> '' then
              InParams := InParams +', :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
            else
              InParams := InParams +':'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
      end;
      PackageBody.Add('BEGIN '+sFunc+GetConnection.GetMetadata.GetIdentifierConvertor.Quote(SQL)+
        '.'+GetConnection.GetMetadata.GetIdentifierConvertor.Quote(PackageIncludedList[IncludeCount])+'('+InParams+'); END;');
    end;
    PackageBody.Add('END;');
    TempResult := TrimRight(PackageBody.Text);
    FreeAndNil(PackageBody);
  end
  else
  begin
    InParams := GenerateParamsStr( FOracleParamsCount );
    TempResult := 'BEGIN ' + sFunc +SQL + InParams+'; END;';
  end;
  Result := ZPlainString(TempResult);
end;

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
  PackageIncludedList := TStringList.Create;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  FreeOracleSQLVars(FPlainDriver, FInVars, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle);
  PackageIncludedList.Free;
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  ArrangeInParams; //Need to sort InParams for Functions
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver , Connection, FErrorHandle,
    FInVars, InParamValues, ChunkSize);

  try
    ExecuteOracleStatement(FPlainDriver, Connection, LogSQL,
      FHandle, FErrorHandle);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;

  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  ArrangeInParams; //Need to sort InParams for Functions
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver , Connection, FErrorHandle,
    FInVars, InParamValues, ChunkSize);

  try
    ExecuteOracleStatement(FPlainDriver, Connection, LogSQL,
      FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self, LogSQL,
      FHandle, FErrorHandle, FInVars, FParamNames, FDBParamTypes, FFunctionResultOffsetsIn);
    Result := LastResultSet;
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;
end;

end.
