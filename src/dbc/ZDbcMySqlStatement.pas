{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZClasses, ZDbcIntfs, ZDbcStatement, ZDbcMySql, ZVariant,
  ZPlainMySqlDriver, ZPlainMySqlConstants, ZCompatibility, ZDbcLogging;

type

  {** Represents a MYSQL specific connection interface. }
  IZMySQLStatement = interface (IZStatement)
    ['{A05DB91F-1E40-46C7-BF2E-25D74978AC83}']

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
    function GetStmtHandle: PZMySqlPrepStmt;
  end;

  {** Represents a MYSQL prepared Statement specific connection interface. }
  IZMySQLPreparedStatement = interface (IZMySQLStatement)
    ['{A05DB91F-1E40-46C7-BF2E-25D74978AC83}']
  end;

  {** Implements Generic MySQL Statement. }
  TZMySQLStatement = class(TZAbstractStatement, IZMySQLStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;

    function CreateResultSet(const SQL: string): IZResultSet;
    function GetStmtHandle : PZMySqlPrepStmt;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);

    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;
    function Execute(const SQL: ZAnsiString): Boolean; override;

    function GetMoreResults: Boolean; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
  end;

  {** Implements Prepared SQL Statement. }
  TZMySQLEmulatedPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PZMySQLConnect);
  end;

  TZMysqlColumnBuffer = Array of PDOBindRecord2;
  { TZMySQLBindBuffer }
  {** Encapsulates a MySQL bind buffer. }
  TZMySQLAbstractBindBuffer = class(TZAbstractObject)
  protected
    FAddedColumnCount : Integer;
    FBindOffsets: MYSQL_BINDOFFSETS;
    FBindArray: Array of byte;
    FPColumnArray: ^TZMysqlColumnBuffer;
  public
    constructor Create(PlainDriver:IZMysqlPlainDriver;
      const BindCount : Integer; var ColumnArray:TZMysqlColumnBuffer);
    function GetColumnArray : TZMysqlColumnBuffer;
    function GetBufferAddress : Pointer;
    function GetBufferType(ColumnIndex: Integer) : TMysqlFieldTypes;
  end;

  {** Encapsulates a MySQL bind buffer for ResultSets. }
  TZMySQLResultSetBindBuffer = class(TZMySQLAbstractBindBuffer)
  public
    procedure AddColumn(PlainDriver: IZMysqlPlainDriver; const FieldHandle: PZMySQLField);
  end;

  {** Encapsulates a MySQL bind buffer for updates. }
  TZMySQLParamBindBuffer = class(TZMySQLAbstractBindBuffer)
  public
    procedure AddColumn(buffertype: TMysqlFieldTypes; const field_length: integer;
      const largeblobparameter: boolean);
  end;
  {** Implements Prepared SQL Statement. }

  { TZMySQLPreparedStatement }

  TZMySQLPreparedStatement = class(TZAbstractPreparedStatement,IZMySQLPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FMySQLConnection: IZMySQLConnection;
    FStmtHandle: PZMySqlPrepStmt;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;

    FColumnArray: TZMysqlColumnBuffer;
    FParamBindBuffer: TZMySQLParamBindBuffer;

    function CreateResultSet(const SQL: string): IZResultSet;

    function getFieldType (testVariant: TZVariant): TMysqlFieldTypes;
  protected
    function GetStmtHandle : PZMySqlPrepStmt;
    //procedure PrepareInParameters; override; //not supported
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    property StmtHandle: PZMySqlPrepStmt read GetStmtHandle;
    constructor Create(PlainDriver: IZMysqlPlainDriver; Connection: IZConnection;
      const SQL: string; Info: TStrings);
    procedure Prepare; override;
    destructor Destroy; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
  end;

  {** Implements callable Postgresql Statement. }
  TZMySQLCallableStatement = class(TZAbstractCallableStatement, IZMySQLStatement,
    IZParamNamedCallableStatement)
  private
    FPlainDriver: IZMysqlPlainDriver;
    FHandle: PZMySQLConnect;
    FQueryHandle: PZMySQLResult;
    FUseResult: Boolean;
    FParamNames: array [0..1024] of String;
    FParamTypeNames: array [0..1024] of String;
    function GetCallSQL: ZAnsiString;
    function GetOutParamSQL: String;
    function GetSelectFunctionSQL: ZAnsiString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString;
    function GetStmtHandle : PZMySqlPrepStmt;
  protected
    procedure ClearResultSets; override;
    procedure BindInParameters; override;
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure FetchOutParams(ResultSet: IZResultSet);
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PZMySQLConnect);

    function Execute(const SQL: ZAnsiString): Boolean; override;
    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;

    function HasMoreResultSets: Boolean; override;
    function GetFirstResultSet: IZResultSet; override;
    function GetPreviousResultSet: IZResultSet; override;
    function GetNextResultSet: IZResultSet; override;
    function GetLastResultSet: IZResultSet; override;
    function BOR: Boolean; override;
    function EOR: Boolean; override;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; override;
    function GetResultSetCount: Integer; override;
  end;

implementation

uses
  Types, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZSysUtils, ZDbcResultSetMetadata,
  ZMessages, ZDbcCachedResultSet, ZDbcUtils, DateUtils, ZEncoding, ZDbcResultSet,
  Math;

{ TZMySQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZMySQLStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: PZMySQLConnect);
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>False</code> This is not a prepared mysql statement.
}
function TZMySQLStatement.IsPreparedStatement: Boolean;
begin
  Result := False;
end;

function TZMySQLStatement.GetStmtHandle: PZMySqlPrepStmt;
begin
  Result := nil;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult, nil);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
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
function TZMySQLStatement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
begin
  ASQL := SQL;
  Result := nil;
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(ASQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
    if not FPlainDriver.ResultSetExists(FHandle) then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    Result := CreateResultSet(LogSQL);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, LogSQL);
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
function TZMySQLStatement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
var
  QueryHandle: PZMySQLResult;
  HasResultset : Boolean;
begin
  ASQL := SQL;
  Result := -1;
  if FPlainDriver.ExecQuery(FHandle, PAnsichar(ASQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
      begin
        QueryHandle := FPlainDriver.StoreResult(FHandle);
        if QueryHandle <> nil then
        begin
          Result := FPlainDriver.GetRowCount(QueryHandle);
          FPlainDriver.FreeResult(QueryHandle);
        end
        else
          Result := FPlainDriver.GetAffectedRows(FHandle);
        while(FPlainDriver.RetrieveNextRowset(FHandle) = 0) do
          begin
           QueryHandle := FPlainDriver.StoreResult(FHandle);
           if QueryHandle <> nil then
             begin
               FPlainDriver.FreeResult(QueryHandle);
             end;
           end;
      end
  { Process regular query }
    else
      Result := FPlainDriver.GetAffectedRows(FHandle);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, LogSQL);
  LastUpdateCount := Result;
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
function TZMySQLStatement.Execute(const SQL: ZAnsiString): Boolean;
var
  HasResultset : Boolean;
begin
  ASQL := SQL;
  Result := False;
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(ASQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
    begin
      Result := True;
      LastResultSet := CreateResultSet(LogSQL);
    end
    { Processes regular query. }
    else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
    end;
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, LogSQL);
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZMySQLStatement.GetMoreResults: Boolean;
var
  AStatus: integer;
begin
  Result := inherited GetMoreResults;
  if FPlainDriver.GetClientVersion >= 40100 then
  begin
    AStatus := FPlainDriver.RetrieveNextRowset(FHandle);
    if AStatus > 0 then
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SSQL)
    else
      Result := (AStatus = 0);

    if LastResultSet <> nil then
      LastResultSet.Close;
    LastResultSet := nil;
    LastUpdateCount := -1;
    if FPlainDriver.ResultSetExists(FHandle) then
      LastResultSet := CreateResultSet(SSQL)
    else
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
  end;
end;

{ TZMySQLEmulatedPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLEmulatedPreparedStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; const SQL: string; Info: TStrings; Handle: PZMySQLConnect);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZMySQLEmulatedPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZMySQLStatement.Create(FPlainDriver, Connection, Info,FHandle);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZMySQLEmulatedPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value) then
    if (InParamDefaultValues[ParamIndex] <> '') and
      StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
      Result := ZAnsiString(InParamDefaultValues[ParamIndex])
    else
      Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then
           Result := '''Y'''
        else
           Result := '''N''';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := ZAnsiString(SoftVarManager.GetAsString(Value));
      stBytes:
        Result := GetSQLHexAnsiString(PAnsiChar(ZAnsiString(SoftVarManager.GetAsString(Value))), Length(ZAnsiString(SoftVarManager.GetAsString(Value))));
      stString:
        Result := FPlainDriver.EscapeString(FHandle, ZPlainString(SoftVarManager.GetAsString(Value)), ConSettings, True);
      stUnicodeString:
        Result := FPlainDriver.EscapeString(FHandle, ZPlainString(SoftVarManager.GetAsUnicodeString(Value)), ConSettings, True);
      stDate:
        begin
          DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
            AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
          Result := ZAnsiString('''' + Format('%0.4d-%0.2d-%0.2d',
            [AYear, AMonth, ADay]) + '''');
        end;
      stTime:
        begin
          DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
            AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
          Result := ZAnsiString('''' + Format('%0.2d:%0.2d:%0.2d',
            [AHour, AMinute, ASecond]) + '''');
        end;
      stTimestamp:
        begin
          DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
            AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
          Result := ZAnsiString('''' + Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
            [AYear, AMonth, ADay, AHour, AMinute, ASecond]) + '''');
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            case InParamTypes[ParamIndex] of
              stBinaryStream:
                Result := GetSQLHexAnsiString(PAnsichar(TempBlob.GetBuffer), TempBlob.Length);
              else
                Result := FPlainDriver.EscapeString(FHandle,
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, TempBlob.WasDecoded, ConSettings),
                    ConSettings, True);
            end;
          end
          else
            Result := 'NULL';
        end;
    end;
  end;
end;

{ TZMySQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLPreparedStatement.Create(
  PlainDriver: IZMySQLPlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FMySQLConnection := Connection as IZMySQLConnection;
  FHandle := FMysqlConnection.GetConnectionHandle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));

  Prepare;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLPreparedStatement.Destroy;
begin
  FStmtHandle := FPlainDriver.ClosePrepStmt(FStmtHandle);
  inherited Destroy;
end;

procedure TZMySQLPreparedStatement.Prepare;
begin
  FStmtHandle := FPlainDriver.InitializePrepStmt(FHandle);
  if (FStmtHandle = nil) then
    begin
      CheckMySQLPrepStmtError(FPlainDriver, FStmtHandle, lcPrepStmt, SFailedtoInitPrepStmt);
      exit;
    end;
  if (FPlainDriver.PrepareStmt(FStmtHandle, PAnsiChar(ASQL), length(ASQL)) <> 0) then
    begin
      CheckMySQLPrepStmtError(FPlainDriver, FStmtHandle, lcPrepStmt, SFailedtoPrepareStmt);
      exit;
    end;
  LogPrepStmtMessage(lcPrepStmt, SQL);
  inherited Prepare;
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLPreparedStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>True</code> This is a prepared mysql statement.
}
function TZMySQLPreparedStatement.IsPreparedStatement: Boolean;
begin
  Result := True;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLPreparedStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLPreparedResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLPreparedResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, (Self as IZMysqlStatement),
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZMysqlPreparedStatement.BindInParameters;
var
  PBuffer: Pointer;
  year, month, day, hour, minute, second, millisecond: word;
  MyType: TMysqlFieldTypes;
  I, OffSet, PieceSize: integer;
  TempBlob: IZBlob;
  TempAnsi: ZAnsiString;
begin
  //http://dev.mysql.com/doc/refman/5.0/en/storage-requirements.html
  if InParamCount = 0 then
     exit;
  { Initialize Bind Array and Column Array }
  FParamBindBuffer := TZMySqlParamBindBuffer.Create(FPlainDriver,InParamCount,FColumnArray);

  For I := 0 to InParamCount - 1 do
  begin
    MyType := GetFieldType(InParamValues[I]);
    case MyType of
      FIELD_TYPE_VARCHAR:
        FParamBindBuffer.AddColumn(FIELD_TYPE_STRING, Max(1,StrLen(PAnsiChar(ZPlainString(InParamValues[I].VUnicodeString)))),false);
      FIELD_TYPE_BLOB:
        begin
          TempBlob := (InParamValues[I].VInterface as IZBlob);
          if TempBlob.IsEmpty then
            DefVarManager.SetNull(InParamValues[I])
          else
            if InParamTypes[I] = stBinaryStream then
              FParamBindBuffer.AddColumn(FIELD_TYPE_BLOB, TempBlob.Length, TempBlob.Length > ChunkSize)
            else
            begin
              TempAnsi := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                        TempBlob.Length, TempBlob.WasDecoded, ConSettings);
              TempBlob := TZAbstractBlob.CreateWithData(PAnsiChar(TempAnsi), Length(TempAnsi));
              TempBlob.SetString(TempAnsi);
              InParamValues[I].VInterface  := TempBlob;
              FParamBindBuffer.AddColumn(FIELD_TYPE_STRING, TempBlob.Length, TempBlob.Length > ChunkSize);
            end;
        end;
      FIELD_TYPE_LONGLONG:
        FParamBindBuffer.AddColumn(MyType,SizeOf(Int64),false);
      FIELD_TYPE_TINY:
        FParamBindBuffer.AddColumn(FIELD_TYPE_STRING,1,false);
      else
        FParamBindBuffer.AddColumn(MyType,Max(1, StrLen(PAnsiChar(ZPlainString(InParamValues[I].VString)))),false);
    end;
    PBuffer := @FColumnArray[I].buffer[0];

    if InParamValues[I].VType=vtNull then
      FColumnArray[I].is_null := 1
    else
      FColumnArray[I].is_null := 0;
      case FParamBindBuffer.GetBufferType(I+1) of
        FIELD_TYPE_FLOAT:    Single(PBuffer^)     := InParamValues[I].VFloat;
        FIELD_TYPE_DOUBLE:   Double(PBuffer^)     := InParamValues[I].VFloat;
        FIELD_TYPE_STRING:
          case MyType of
            FIELD_TYPE_TINY:
              if InParamValues[I].VBoolean then
                PAnsiChar(PBuffer)^ := 'Y'
              else
                PAnsiChar(PBuffer)^ := 'N';
            FIELD_TYPE_VARCHAR:
              StrCopy(PAnsiChar(PBuffer), PAnsiChar(ZPlainString(InParamValues[I].VUnicodeString)));
            FIELD_TYPE_BLOB:
              begin
                if TempBlob.Length<=ChunkSize then
                  StrCopy(PAnsiChar(PBuffer), PAnsiChar(TempBlob.GetString));
                TempBlob := nil;
              end;
            else
              StrCopy(PAnsiChar(PBuffer), PAnsiChar(ZPlainString(InParamValues[I].VString)));
          end;
        FIELD_TYPE_LONGLONG: Int64(PBuffer^) := InParamValues[I].VInteger;
        FIELD_TYPE_DATETIME:
          begin
            DecodeDateTime(InParamValues[I].VDateTime, Year, Month, Day, hour, minute, second, millisecond);
            PMYSQL_TIME(PBuffer)^.year := year;
            PMYSQL_TIME(PBuffer)^.month := month;
            PMYSQL_TIME(PBuffer)^.day := day;
            PMYSQL_TIME(PBuffer)^.hour := hour;
            PMYSQL_TIME(PBuffer)^.minute := minute;
            PMYSQL_TIME(PBuffer)^.second := second;
            PMYSQL_TIME(PBuffer)^.second_part := millisecond;
          end;
          FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
          FIELD_TYPE_BLOB:
            begin
              if TempBlob.Length<=ChunkSize then
                System.Move(TempBlob.GetBuffer^, PBuffer^, TempBlob.Length);
              TempBlob := nil;
            end;
          FIELD_TYPE_NULL:;
      end;
  end;

  if (FPlainDriver.BindParameters(FStmtHandle, FParamBindBuffer.GetBufferAddress) <> 0) then
  begin
    checkMySQLPrepStmtError (FPlainDriver, FStmtHandle, lcPrepStmt, SBindingFailure);
    exit;
  end;
  inherited BindInParameters;

  // Send large blobs in chuncks
  For I := 0 to InParamCount - 1 do
  begin
    if FParamBindBuffer.GetBufferType(I+1) in [FIELD_TYPE_STRING,FIELD_TYPE_BLOB] then
      begin
        MyType := GetFieldType(InParamValues[I]);
        if MyType = FIELD_TYPE_BLOB then
          begin
            TempBlob := (InParamValues[I].VInterface as IZBlob);
            if TempBlob.Length>ChunkSize then
            begin
              OffSet := 0;
              PieceSize := ChunkSize;
              while OffSet < TempBlob.Length do
              begin
                if OffSet+PieceSize > TempBlob.Length then
                  PieceSize := TempBlob.Length - OffSet;
                if (FPlainDriver.SendPreparedLongData(FStmtHandle, I, PAnsiChar(TempBlob.GetBuffer)+OffSet, PieceSize) <> 0) then
                begin
                  checkMySQLPrepStmtError (FPlainDriver, FStmtHandle, lcPrepStmt, SBindingFailure);
                  exit;
                end;
                Inc(OffSet, PieceSize);
              end;
            end;
            TempBlob:=nil;
          end;
      end;
  end;
end;

procedure TZMySQLPreparedStatement.UnPrepareInParameters;
begin
  // Empty : Mysql can't prepare datastructures before the actual parameters are known, because the
  // number/datatype of parameters isn't returned by the server.
  inherited UnPrepareInParameters;
end;

function TZMysqlPreparedStatement.getFieldType (testVariant: TZVariant): TMysqlFieldTypes;
begin
  case testVariant.vType of
    vtNull:      Result := FIELD_TYPE_TINY;
    vtBoolean:   Result := FIELD_TYPE_TINY;
    vtInteger:   Result := FIELD_TYPE_LONGLONG;
    vtFloat:     Result := FIELD_TYPE_DOUBLE;
    vtString:    Result := FIELD_TYPE_STRING;
    vtDateTime:  Result := FIELD_TYPE_DATETIME;
    vtUnicodeString: Result := FIELD_TYPE_VARCHAR;
    vtInterface: Result := FIELD_TYPE_BLOB;
  else
    raise EZSQLException.Create(SUnsupportedDataType);
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  BindInParameters;
  if (self.FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
    try
      checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
    except
      if Assigned(FParamBindBuffer) then
        FreeAndNil(FParamBindBuffer);
      raise;
    end;

  if Assigned(FParamBindBuffer) then
     FreeAndNil(FParamBindBuffer);

  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) = 0 then
    raise EZSQLException.Create(SCanNotOpenResultSet);
  Result := CreateResultSet(SQL);
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
function TZMySQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  BindInParameters;
  if (self.FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
    try
      checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
    except
      if Assigned(FParamBindBuffer) then
        FreeAndNil(FParamBindBuffer); //MemLeak closed
      raise;
    end;

  if Assigned(FParamBindBuffer) then
    FreeAndNil(FParamBindBuffer); //MemLeak closed

    { Process queries with result sets }
  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) > 0 then
    begin
      FPlainDriver.StorePreparedResult(FStmtHandle);
      Result := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
      if Assigned(FStmtHandle) then
        begin
          FPlainDriver.FreePreparedResult(FStmtHandle);
          while(FPlainDriver.GetPreparedNextResult(FStmtHandle) = 0) do
            FPlainDriver.FreePreparedResult(FStmtHandle);
        end;

    end
    { Process regular query }
  else
    Result := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
  LastUpdateCount := Result;
  Inherited ExecuteUpdatePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZMySQLPreparedStatement.ExecutePrepared: Boolean;
begin
  BindInParameters;
  if (FPlainDriver.ExecuteStmt(FStmtHandle) <> 0) then
    try
      checkMySQLPrepStmtError(FPlainDriver,FStmtHandle, lcExecPrepStmt, SPreparedStmtExecFailure);
    except
      if Assigned(FParamBindBuffer) then
        FreeAndNil(FParamBindBuffer); //MemLeak closed
      raise;
    end;

  if Assigned(FParamBindBuffer) then
    FreeAndNil(FParamBindBuffer); //MemLeak closed

  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) > 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(SQL);
  end
  { Processes regular query. }
  else
  begin
    Result := False;
    LastUpdateCount := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
  end;

  inherited ExecutePrepared;
end;

function TZMySQLPreparedStatement.GetStmtHandle: PZMySqlPrepStmt;
begin
  Result := FStmtHandle;
end;

{ TZMySQLCallableStatement }

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZMySQLCallableStatement.GetCallSQL: ZAnsiString;
  function GenerateParamsStr(Count: integer): ZAnsiString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count-1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      if FDBParamTypes[i] in [1, 2, 3, 4] then
        Result := Result + '@'+ZPlainString(FParamNames[I])
    end;
  end;

var
  InParams: ZAnsiString;
begin
  if HasOutParameter then
    InParams := GenerateParamsStr(OutParamCount)
  else
    InParams := GenerateParamsStr(InParamCount);
  Result := 'CALL '+ZPlainString(SQL)+'('+InParams+')';
end;

function TZMySQLCallableStatement.GetOutParamSQL: String;
  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    I := 0;
    while True do
      if (FDBParamTypes[i] = 0) or ( I = Length(FDBParamTypes)) then
        break
      else
      begin
        if FDBParamTypes[i] in [2, 3, 4] then
        begin
          if Result <> '' then
            Result := Result + ',';
          if FParamTypeNames[i] = '' then
            Result := Result + ' @'+FParamNames[I]+' AS '+FParamNames[I]
          else
            Result := Result + ' CAST(@'+FParamNames[I]+ ' AS '+FParamTypeNames[i]+') AS '+FParamNames[I];
        end;
        Inc(i);
      end;
  end;

var
  OutParams: String;
begin
  OutParams := GenerateParamsStr(Self.OutParamCount-Length(InParamValues));
  Result := 'SELECT '+ OutParams;
end;

function TZMySQLCallableStatement.GetSelectFunctionSQL: ZAnsiString;
  function GenerateInParamsStr: ZAnsiString;
  var
    I: Integer;
  begin
    Result := '';
    for i := 0 to Length(InParamValues) -1 do
      if Result = '' then
        Result := PrepareAnsiSQLParam(I)
      else
        Result := Result+', '+ PrepareAnsiSQLParam(I);
  end;
var
  InParams: ZAnsiString;
begin
  InParams := GenerateInParamsStr;
  Result := 'SELECT '+ZPlainString(SQL)+'('+InParams+')';
  Result := Result + ' AS ReturnValue';
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
function TZMySQLCallableStatement.PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value) then
    if (InParamDefaultValues[ParamIndex] <> '') and
      StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
      Result := ZAnsiString(InParamDefaultValues[ParamIndex])
    else
      Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if SoftVarManager.GetAsBoolean(Value) then
          Result := '''Y'''
        else
          Result := '''N''';
      stByte, stShort, stInteger, stLong, stBigDecimal, stFloat, stDouble:
        Result := ZAnsiString(SoftVarManager.GetAsString(Value));
      stBytes:
        Result := GetSQLHexAnsiString(PAnsiChar(AnsiString(SoftVarManager.GetAsString(Value))), Length(SoftVarManager.GetAsString(Value)));
      stString:
        Result := FPlainDriver.EscapeString(FHandle, ZPlainString(SoftVarManager.GetAsString(Value), ConSettings), ConSettings, True);
      stUnicodeString:
        Result := FPlainDriver.EscapeString(FHandle, ZPlainString(SoftVarManager.GetAsUnicodeString(Value)), ConSettings, True);
      stDate:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + ZAnsiString(Format('%0.4d-%0.2d-%0.2d',
          [AYear, AMonth, ADay])) + '''';
      end;
      stTime:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + ZAnsiString(Format('%0.2d:%0.2d:%0.2d',
          [AHour, AMinute, ASecond])) + '''';
      end;
      stTimestamp:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + ZAnsiString(Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
          [AYear, AMonth, ADay, AHour, AMinute, ASecond])) + '''';
      end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            case InParamTypes[ParamIndex] of
              stBinaryStream:
                Result := GetSQLHexAnsiString(PAnsichar(TempBlob.GetBuffer), TempBlob.Length);
              else
                Result := FPlainDriver.EscapeString(FHandle,
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, TempBlob.WasDecoded, ConSettings),
                  ConSettings, True);
            end
          else
            Result := 'NULL';
        end;
    end;
  end;
end;

function TZMySQLCallableStatement.GetStmtHandle: PZMySqlPrepStmt;
begin
  Result := nil;
end;

procedure TZMySQLCallableStatement.ClearResultSets;
begin
  inherited;
  FPlainDriver.FreeResult(FQueryHandle);
  FQueryHandle := nil;
end;

procedure TZMySQLCallableStatement.BindInParameters;
var
  I: integer;
  ExecQuery: ZAnsiString;
begin
  I := 0;
  ExecQuery := '';
  while True do
    if (i = Length(FDBParamTypes)) then
      break
    else
    begin
      if FDBParamTypes[i] in [1, 3] then //ptInputOutput
        if ExecQuery = '' then
          ExecQuery := 'SET @'+ZPlainString(FParamNames[i])+' = '+PrepareAnsiSQLParam(I)
        else
          ExecQuery := ExecQuery + ', @'+ZPlainString(FParamNames[i])+' = '+PrepareAnsiSQLParam(I);
      Inc(i);
    end;
  if not (ExecQuery = '') then
    if FPlainDriver.ExecQuery(Self.FHandle, PAnsiChar(ExecQuery)) = 0 then
      DriverManager.LogMessage(lcBindPrepStmt, FPlainDriver.GetProtocol, String(ExecQuery))
    else
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, String(ExecQuery));
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLCallableStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQLResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    FUseResult, @LastUpdateCount, not IsFunction);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) or (not IsFunction) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(rcReadOnly);
    {Need to fetch all data. The handles must be released for mutiple
      Resultsets}
    CachedResultSet.AfterLast;//Fetch all
    CachedResultSet.BeforeFirst;//Move to first pos
    NativeResultSet.ReleaseHandle; //Release the handles
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Sets output parameters from a ResultSet
  @param Value a IZResultSet object.
}
procedure TZMySQLCallableStatement.FetchOutParams(ResultSet: IZResultSet);
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
  ResultSet.BeforeFirst;
end;

procedure TZMySQLCallableStatement.RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName, ParamName: String; Const ColumnSize, Precision: Integer);
begin
  FParamNames[ParameterIndex] := ParamName;
  if ( Pos('char', LowerCase(ParamTypeName)) > 0 ) or
     ( Pos('set', LowerCase(ParamTypeName)) > 0 ) then
    FParamTypeNames[ParameterIndex] := 'CHAR('+IntToStr(ColumnSize)+')'
  else
    if ( Pos('set', LowerCase(ParamTypeName)) > 0 ) then
      FParamTypeNames[ParameterIndex] := 'CHAR('+IntToStr(ColumnSize)+')'
    else
      if ( Pos('datetime', LowerCase(ParamTypeName)) > 0 ) or
         ( Pos('timestamp', LowerCase(ParamTypeName)) > 0 ) then
        FParamTypeNames[ParameterIndex] := 'DATETIME'
      else
        if ( Pos('date', LowerCase(ParamTypeName)) > 0 ) then
          FParamTypeNames[ParameterIndex] := 'DATE'
        else
          if ( Pos('time', LowerCase(ParamTypeName)) > 0 ) then
            FParamTypeNames[ParameterIndex] := 'TIME'
          else
            if ( Pos('int', LowerCase(ParamTypeName)) > 0 ) or
               ( Pos('year', LowerCase(ParamTypeName)) > 0 ) then
              FParamTypeNames[ParameterIndex] := 'SIGNED'
            else
              if ( Pos('binary', LowerCase(ParamTypeName)) > 0 ) then
                FParamTypeNames[ParameterIndex] := 'BINARY('+IntToStr(ColumnSize)+')'
              else
                FParamTypeNames[ParameterIndex] := '';
end;

constructor TZMySQLCallableStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; const SQL: string; Info: TStrings;
  Handle: PZMySQLConnect);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
begin
  Result := nil;
  ASQL := SQL;
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(SQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);
    if not FPlainDriver.ResultSetExists(FHandle) then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    if IsFunction then
      ClearResultSets;
    FResultSets.Add(CreateResultSet(SSQL));
    if FPlainDriver.CheckAnotherRowset(FHandle) then
    begin
      while FPlainDriver.RetrieveNextRowset(FHandle) = 0 do
        if FPlainDriver.CheckAnotherRowset(FHandle) then
          FResultSets.Add(CreateResultSet(SSQL))
        else break;
      CheckMySQLError(FPlainDriver, FHandle, lcExecute, SSQL);
    end;
    FActiveResultset := FResultSets.Count-1;
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SSQL);
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
function TZMySQLCallableStatement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
begin
  Result := -1;
  ASQL := SQL;
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(ASQL)) = 0 then
  begin
    { Process queries with result sets }
    if FPlainDriver.ResultSetExists(FHandle) then
    begin
      ClearResultSets;
      FActiveResultset := 0;
      FResultSets.Add(CreateResultSet(LogSQL));
      if FPlainDriver.CheckAnotherRowset(FHandle) then
      begin
        Result := LastUpdateCount;
        while FPlainDriver.RetrieveNextRowset(FHandle) = 0 do
          if FPlainDriver.CheckAnotherRowset(FHandle) then
          begin
            FResultSets.Add(CreateResultSet(SSQL));
            inc(Result, LastUpdateCount); //LastUpdateCount will be returned from ResultSet.Open
          end
          else break;
        CheckMySQLError(FPlainDriver, FHandle, lcExecute, SSQL);
      end
      else
        Result := LastUpdateCount;
      FActiveResultset := FResultSets.Count-1;
      LastResultSet := IZResultSet(FResultSets[FActiveResultset]);
    end
    else { Process regular query }
      Result := FPlainDriver.GetAffectedRows(FHandle);
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, SSQL);
  LastUpdateCount := Result;
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
function TZMySQLCallableStatement.Execute(const SQL: ZAnsiString): Boolean;
var
  HasResultset : Boolean;
begin
  Result := False;
  {$IFNDEF UNICODE}ASQL := SQL;{$ENDIF}
  if FPlainDriver.ExecQuery(FHandle, PAnsiChar(ASQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
    begin
      Result := True;
      LastResultSet := CreateResultSet(LogSQL);
    end
    { Processes regular query. }
    else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
    end;
  end
  else
    CheckMySQLError(FPlainDriver, FHandle, lcExecute, LogSQL);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteQuery(GetSelectFunctionSQL);
  end
  else
  begin
    BindInParameters;
    ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      Result := ExecuteQuery(ZPlainString(GetOutParamSQL)) //Get the Last Resultset
    else
      Result := GetLastResultSet;
  end;
  if Assigned(Result) then
    FetchOutParams(Result);
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
function TZMySQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteUpdate(GetSelectFunctionSQL);
    FetchOutParams(LastResultSet);
  end
  else
  begin
    BindInParameters;
    Result := ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      FetchOutParams(ExecuteQuery(ZPlainString(GetOutParamSQL))); //Get the Last Resultset
    Inc(Result, LastUpdateCount);
  end;
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLCallableStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>False</code> This is not a prepared mysql statement.
}
function TZMySQLCallableStatement.IsPreparedStatement: Boolean;
begin
  Result := False;
end;

{**
  Are more resultsets retrieved?
  @result Returns <code>True</code> if more resultsets are retrieved
}
function TZMySQLCallableStatement.HasMoreResultSets: Boolean;
begin
  Result := FResultSets.Count > 1;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetNextResultSet: IZResultSet;
begin
  if ( FActiveResultset < FResultSets.Count-1) and ( FResultSets.Count > 1) then
  begin
    Inc(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetPreviousResultSet: IZResultSet;
begin
  if ( FActiveResultset > 0) and ( FResultSets.Count > 0) then
  begin
    Dec(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetFirstResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := 0;
    Result := IZResultSet(FResultSets[0]);
  end;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetLastResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := FResultSets.Count -1;
    Result := IZResultSet(FResultSets[FResultSets.Count -1]);
  end;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZMySQLCallableStatement.BOR: Boolean;
begin
  Result := FActiveResultset = 0;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZMySQLCallableStatement.EOR: Boolean;
begin
  Result := FActiveResultset = FResultSets.Count -1;
end;

{**
  Retrieves a ResultSet by his index.
  @param Integer the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
function TZMySQLCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
  if ( Index < 0 ) or ( Index > FResultSets.Count -1 ) then
    raise Exception.Create(Format(SListIndexError, [Index]))
  else
    Result := IZResultSet(FResultSets[Index]);
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZMySQLCallableStatement.GetResultSetCount: Integer;
begin
  Result := FResultSets.Count;
end;

{ TZMySQLAbstractBindBuffer }

constructor TZMySQLAbstractBindBuffer.Create(PlainDriver: IZMysqlPlainDriver;
  const BindCount: Integer; var ColumnArray: TZMysqlColumnBuffer);
begin
  inherited Create;
  FBindOffsets := PlainDriver.GetBindOffsets;
  if FBindOffsets.buffer_type=0 then
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(PlainDriver.GetClientVersion));
  FPColumnArray := @ColumnArray;
  setlength(FBindArray,0);
  setlength(ColumnArray,BindCount);
  setlength(FBindArray,BindCount*FBindOffsets.size);
end;

function TZMySQLAbstractBindBuffer.GetColumnArray: TZMysqlColumnBuffer;
begin
  result := FPColumnArray^;
end;

function TZMySQLAbstractBindBuffer.GetBufferAddress: Pointer;
begin
  result:=@FBindArray[0];
end;

function TZMySQLAbstractBindBuffer.GetBufferType(ColumnIndex: Integer): TMysqlFieldTypes;
begin
  result := PTMysqlFieldTypes(@FbindArray[NativeUInt((ColumnIndex-1)*FBindOffsets.size)+FBindOffsets.buffer_type])^;
end;


{ TZMySQLResultSetBindBuffer }

procedure TZMySQLResultSetBindBuffer.AddColumn(PlainDriver: IZMysqlPlainDriver;
  const FieldHandle: PZMySQLField);
var
  buffertype: TMysqlFieldTypes;
  ColOffset: NativeUInt;
begin
  buffertype := PlainDriver.GetFieldType(FieldHandle);
  Inc(FAddedColumnCount);
  With FPColumnArray^[FAddedColumnCount-1] do
  begin
      case buffertype of
        FIELD_TYPE_DATE:        Length := sizeOf(MYSQL_TIME);
        FIELD_TYPE_TIME:        Length := sizeOf(MYSQL_TIME);
        FIELD_TYPE_DATETIME:    Length := sizeOf(MYSQL_TIME);
        FIELD_TYPE_TIMESTAMP:   Length := sizeOf(MYSQL_TIME);
        FIELD_TYPE_TINY:        Length := 1;
        FIELD_TYPE_SHORT:       Length := 2;
        FIELD_TYPE_LONG:        Length := 4;
        FIELD_TYPE_LONGLONG:    Length := 8;
        FIELD_TYPE_FLOAT:       Length := 4;
        FIELD_TYPE_DOUBLE:      Length := 8;
        FIELD_TYPE_NEWDECIMAL:  Length := 11;
        FIELD_TYPE_BLOB,
        FIELD_TYPE_MEDIUM_BLOB,
        FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_GEOMETRY:
          Length := PlainDriver.GetFieldMaxLength(FieldHandle)+1;
        FIELD_TYPE_VARCHAR,
        FIELD_TYPE_VAR_STRING,
        FIELD_TYPE_STRING:
            Length := Min(MaxBlobSize, Max(PlainDriver.GetFieldLength(FieldHandle), PlainDriver.GetFieldMaxLength(FieldHandle)))+1;
      else
        Length := PlainDriver.GetFieldLength(FieldHandle);
      end;
    SetLength(Buffer, Length);
  end;
  ColOffset := NativeUInt((FAddedColumnCount-1)*FBindOffsets.size);
  PTMysqlFieldTypes(@FbindArray[ColOffset+FBindOffsets.buffer_type])^ := buffertype;
  PULong(@FbindArray[ColOffset+FBindOffsets.buffer_length])^ := FPColumnArray^[FAddedColumnCount-1].length;
  PByte(@FbindArray[ColOffset+FBindOffsets.is_unsigned])^:= 0;
  PPointer(@FbindArray[ColOffset+FBindOffsets.buffer])^:= @FPColumnArray^[FAddedColumnCount-1].buffer[0];
  PPointer(@FbindArray[ColOffset+FBindOffsets.length])^:= @FPColumnArray^[FAddedColumnCount-1].length;
  PPointer(@FbindArray[ColOffset+FBindOffsets.is_null])^:= @FPColumnArray^[FAddedColumnCount-1].is_null;
end;

{ TZMySQLParamBindBuffer }

// largeblobparameter: true to indicate that parameter is a blob that will be
// sent chunked. Set to false for result set columns.

procedure TZMySQLParamBindBuffer.AddColumn(buffertype: TMysqlFieldTypes;
  const field_length: integer; const largeblobparameter: boolean);
var
  tempbuffertype: TMysqlFieldTypes;
  ColOffset:NativeUInt;
begin
  Case buffertype of
    FIELD_TYPE_DECIMAL,
    FIELD_TYPE_NEWDECIMAL: tempbuffertype := FIELD_TYPE_DOUBLE;
  Else
    tempbuffertype := buffertype;
  End;
  Inc(FAddedColumnCount);
  With FPColumnArray^[FAddedColumnCount-1] do
  begin
    length := getMySQLFieldSize(tempbuffertype,field_length);
    if largeblobparameter then
    begin
      is_Null := 0;
      buffer := nil;
    end
    else if field_length = 0 then
    begin
      is_Null := 1;
      buffer := nil;
    end
    else
    begin
      if tempbuffertype in [FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
           FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING] then
      //ludob: mysql adds terminating #0 on top of data. Avoid buffer overrun.
        SetLength(buffer,length+1)
      else
        SetLength(buffer,length);
      is_null := 0;
    end;
  end;
  ColOffset:=NativeUInt((FAddedColumnCount-1)*FBindOffsets.size);
  PTMysqlFieldTypes(@FbindArray[ColOffset+FBindOffsets.buffer_type])^:=tempbuffertype;
  PULong(@FbindArray[ColOffset+FBindOffsets.buffer_length])^ := FPColumnArray^[FAddedColumnCount-1].length;
  PByte(@FbindArray[ColOffset+FBindOffsets.is_unsigned])^:= 0;
  PPointer(@FbindArray[ColOffset+FBindOffsets.buffer])^:= @FPColumnArray^[FAddedColumnCount-1].buffer[0];
  PPointer(@FbindArray[ColOffset+FBindOffsets.length])^:= @FPColumnArray^[FAddedColumnCount-1].length;
  PPointer(@FbindArray[ColOffset+FBindOffsets.is_null])^:= @FPColumnArray^[FAddedColumnCount-1].is_null;
end;

end.
