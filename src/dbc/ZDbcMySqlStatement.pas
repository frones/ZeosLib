{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

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
    function PrepareSQLParam(ParamIndex: Integer): string; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PZMySQLConnect);
  end;

  TZMysqlColumnBuffer = Array of PDOBindRecord2;
  { TZMySQLBindBuffer }
  {** Encapsulates a MySQL bind buffer. }
  TZMySQLBindBuffer = class(TZAbstractObject)
  protected
    FAddedColumnCount : Integer;
    FBindOffsets: MYSQL_BINDOFFSETS;
    FBindArray: Array of byte;
    FPColumnArray: ^TZMysqlColumnBuffer;
  public
    constructor Create(PlainDriver:IZMysqlPlainDriver; NumColumns : Integer; var ColumnArray:TZMysqlColumnBuffer);
    destructor Destroy; override;
    procedure AddColumn(buffertype:TMysqlFieldTypes; field_length:integer; largeblobparameter:boolean);
    function GetColumnArray : TZMysqlColumnBuffer;
    function GetBufferAddress : Pointer;
    function GetBufferType(ColumnIndex: Integer) : TMysqlFieldTypes;
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
    FBindBuffer: TZMysqlBindBuffer;

    function CreateResultSet(const SQL: string): IZResultSet;

    function getFieldType (testVariant: TZVariant): TMysqlFieldTypes;
  protected
    function GetStmtHandle : PZMySqlPrepStmt;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    property StmtHandle: PZMySqlPrepStmt read GetStmtHandle;
    constructor Create(PlainDriver: IZMysqlPlainDriver; Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Prepare; override;

    function ExecuteQuery(const SQL: string): IZResultSet; override;
    function ExecuteUpdate(const SQL: string): Integer; override;
    function Execute(const SQL: string): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;
  end;

implementation

uses
  Types, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZSysUtils, ZDbcResultSetMetadata,
  ZMessages, ZDbcCachedResultSet, ZDbcUtils, DateUtils;

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
    FUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ClientCodePage);
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
function TZMySQLStatement.ExecuteQuery(const SQL: string): IZResultSet;
begin
  Result := nil;
  if FPlainDriver.ExecQuery(FHandle, SQL, Connection.PreprepareSQL, Self.GetConnection.GetEncoding, LogSQL) = 0 then
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
function TZMySQLStatement.ExecuteUpdate(const SQL: string): Integer;
var
  QueryHandle: PZMySQLResult;
  HasResultset : Boolean;
begin
  Result := -1;
  if FPlainDriver.ExecQuery(FHandle, SQL, Connection.PreprepareSQL, GetConnection.GetEncoding, LogSQL) = 0 then
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
function TZMySQLStatement.Execute(const SQL: string): Boolean;
var
  HasResultset : Boolean;
begin
  Result := False;
  if FPlainDriver.ExecQuery(FHandle, SQL, Connection.PreprepareSQL, GetConnection.GetEncoding, LogSQL) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);
    HasResultSet := FPlainDriver.ResultSetExists(FHandle);
    { Process queries with result sets }
    if HasResultSet then
    begin
      Result := True;
      LastResultSet := CreateResultSet(SQL);
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
function TZMySQLEmulatedPreparedStatement.PrepareSQLParam(ParamIndex: Integer): string;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempStream,TempStreamIn: TStream;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value) then
    if (InParamDefaultValues[ParamIndex] <> '') and
      StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
      Result := InParamDefaultValues[ParamIndex]
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
        Result := SoftVarManager.GetAsString(Value);
      stBytes:
        Result := Self.GetConnection.GetBinaryEscapeString(AnsiString(SoftVarManager.GetAsString(Value)));
      stString:
        Result := Self.GetConnection.GetEscapeString(SoftVarManager.GetAsString(Value));
      stUnicodeString:
        {$IFDEF DELPHI12_UP}
          if GetConnection.PreprepareSQL then
            Result := Self.GetConnection.GetEscapeString(SoftVarManager.GetAsUnicodeString(Value)) else
        {$ENDIF}
        if (GetConnection.GetClientCodePageInformations^.Encoding = ceUTF8) then
          Result := Self.GetConnection.GetEscapeString(UTF8Encode(SoftVarManager.GetAsUnicodeString(Value)))
        else
          Result := Self.GetConnection.GetEscapeString(AnsiString(SoftVarManager.GetAsUnicodeString(Value)));
      stDate:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.4d-%0.2d-%0.2d',
          [AYear, AMonth, ADay]) + '''';
      end;
      stTime:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.2d:%0.2d:%0.2d',
          [AHour, AMinute, ASecond]) + '''';
      end;
      stTimestamp:
      begin
        DecodeDateTime(SoftVarManager.GetAsDateTime(Value),
          AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        Result := '''' + Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
          [AYear, AMonth, ADay, AHour, AMinute, ASecond]) + '''';
      end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if InParamTypes[ParamIndex] = stBinaryStream then
              Result := Self.GetConnection.GetBinaryEscapeString(TempBlob.GetString)
            else
            begin
              if (GetConnection.GetClientCodePageInformations^.Encoding = ceUTF8) then
              begin //could be equal valid for unicode if the user reads the Stream as ftMemo
                TempStreamIn:=TempBlob.GetStream;
                TempStream := GetValidatedUnicodeStream(TempStreamIn);
                TempStreamIn.Free;
                TempBlob.SetStream(TempStream);
                TempStream.Free;
              end;
              {$IFDEF DELPHI12_UP}
              if GetConnection.PreprepareSQL then
                Result := Self.GetConnection.GetEscapeString(ZDbcString(TempBlob.GetString)) else
              {$ENDIF}
              Result := GetConnection.GetEscapeString(TempBlob.GetString)
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
var
  AnsiSQL: AnsiString;
begin
  FStmtHandle := FPlainDriver.InitializePrepStmt(FHandle);
  if (FStmtHandle = nil) then
    begin
      CheckMySQLPrepStmtError(FPlainDriver, FStmtHandle, lcPrepStmt, SFailedtoInitPrepStmt);
      exit;
    end;
  AnsiSQL := GetPrepreparedSQL(SQL); //do not spit Tokens twice
  if (FPlainDriver.PrepareStmt(FStmtHandle, PAnsiChar(AnsiSQL), length(AnsiSQL)) <> 0) then
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
      CachedResolver, ClientCodePage);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZMySQLPreparedStatement.PrepareInParameters;
begin
  // Empty : Mysql can't prepare datastructures before the actual parameters are known, because the
  // number/datatype of parameters isn't returned by the server.
  inherited PrepareInParameters;
end;

procedure TZMysqlPreparedStatement.BindInParameters;
var
  PBuffer: Pointer;
  year, month, day, hour, minute, second, millisecond: word;
  MyType: TMysqlFieldTypes;
  I, OffSet, PieceSize: integer;
  TempBlob: IZBlob;

begin
  //http://dev.mysql.com/doc/refman/5.0/en/storage-requirements.html
  if InParamCount = 0 then
     exit;
  { Initialize Bind Array and Column Array }
  FBindBuffer := TZMysqlBindBuffer.Create(FPlainDriver,InParamCount,FColumnArray);

  For I := 0 to InParamCount - 1 do
  begin
    MyType := GetFieldType(InParamValues[I]);
    if MyType = FIELD_TYPE_VARCHAR then
      FBindBuffer.AddColumn(FIELD_TYPE_STRING, StrLen(PAnsiChar(UTF8Encode(InParamValues[I].VUnicodeString)))+1,false)
    else
      if MyType =FIELD_TYPE_BLOB then
      begin
        TempBlob := (InParamValues[I].VInterface as IZBlob);
        if InParamTypes[I] = stBinaryStream then
          FBindBuffer.AddColumn(FIELD_TYPE_BLOB, TempBlob.Length,TempBlob.Length>ChunkSize)
        else
          FBindBuffer.AddColumn(FIELD_TYPE_STRING, TempBlob.Length,TempBlob.Length>ChunkSize);
      end
      else
        FBindBuffer.AddColumn(MyType,StrLen(PAnsiChar(ZPlainString(InParamValues[I].VString)))+1,false);
    PBuffer := @FColumnArray[I].buffer[0];

    if InParamValues[I].VType=vtNull then
      FColumnArray[I].is_null := 1
    else
      FColumnArray[I].is_null := 0;
      case FBindBuffer.GetBufferType(I+1) of
        FIELD_TYPE_FLOAT:    Single(PBuffer^)     := InParamValues[I].VFloat;
        FIELD_TYPE_DOUBLE:   Double(PBuffer^)     := InParamValues[I].VFloat;
        FIELD_TYPE_STRING:
          begin
            if MyType = FIELD_TYPE_VARCHAR then
              StrCopy(PAnsiChar(PBuffer), PAnsiChar(UTF8Encode(InParamValues[I].VUnicodeString)))
            else
            if MyType = FIELD_TYPE_BLOB then
            begin
              if TempBlob.Length<=ChunkSize then
                StrCopy(PAnsiChar(PBuffer), PAnsiChar(TempBlob.GetString));
              TempBlob := nil;
            end
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

  if (FPlainDriver.BindParameters(FStmtHandle, FBindBuffer.GetBufferAddress) <> 0) then
    begin
      checkMySQLPrepStmtError (FPlainDriver, FStmtHandle, lcPrepStmt, SBindingFailure);
      exit;
    end;
   inherited BindInParameters;

  // Send large blobs in chuncks
  For I := 0 to InParamCount - 1 do
    begin
      if FBindBuffer.GetBufferType(I+1) in [FIELD_TYPE_STRING,FIELD_TYPE_BLOB] then
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
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZMySQLPreparedStatement.ExecuteQuery(const SQL: string): IZResultSet;
begin
  Self.SQL := SQL;
  Result := ExecuteQueryPrepared;
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
function TZMySQLPreparedStatement.ExecuteUpdate(const SQL: string): Integer;
begin
  Self.SQL := SQL;
  Result := ExecuteUpdatePrepared;
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
function TZMySQLPreparedStatement.Execute(const SQL: string): Boolean;
begin
  Self.SQL := SQL;
  Result := ExecutePrepared;
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
       FBindBuffer.Free;  //MemLeak closed
 	     raise;
     end;

  FBindBuffer.Free;

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
       FBindBuffer.Free;  //MemLeak closed
 	     raise;
     end;

  FBindBuffer.Free;

    { Process queries with result sets }
  if FPlainDriver.GetPreparedFieldCount(FStmtHandle) > 0 then
    begin
      FPlainDriver.StorePreparedResult(FStmtHandle);
      Result := FPlainDriver.GetPreparedAffectedRows(FStmtHandle);
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
       FBindBuffer.Free;  //MemLeak closed
       raise;
     end;

  FBindBuffer.Free;

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

{ TZMySQLBindBuffer }

constructor TZMySQLBindBuffer.Create(PlainDriver: IZMysqlPlainDriver; NumColumns: Integer; var ColumnArray:TZMysqlColumnBuffer);
begin
  inherited Create;
  FBindOffsets := PlainDriver.GetBindOffsets;
  if FBindOffsets.buffer_type=0 then
    raise EZSQLException.Create('Unknown dll version : '+IntToStr(PlainDriver.GetClientVersion));
  FPColumnArray := @ColumnArray;
  setlength(FBindArray,0);
  setlength(ColumnArray,NumColumns);
  setlength(FBindArray,NumColumns*FBindOffsets.size);
end;

destructor TZMySQLBindBuffer.Destroy;
begin
  inherited Destroy;
end;


// largeblobparameter: true to indicate that parameter is a blob that will be
// sent chunked. Set to false for result set columns.

procedure TZMySQLBindBuffer.AddColumn(buffertype: TMysqlFieldTypes;
  field_length: integer; largeblobparameter:boolean);
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

function TZMySQLBindBuffer.GetColumnArray: TZMysqlColumnBuffer;
begin
  result := FPColumnArray^;
end;

function TZMySQLBindBuffer.GetBufferAddress: Pointer;
begin
  result:=@FBindArray[0];
end;

function TZMySQLBindBuffer.GetBufferType(ColumnIndex: Integer): TMysqlFieldTypes;
begin
  result := PTMysqlFieldTypes(@FbindArray[NativeUInt((ColumnIndex-1)*FBindOffsets.size)+FBindOffsets.buffer_type])^;
end;

end.
