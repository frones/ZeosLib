{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
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

unit ZDbcSqLiteStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcLogging, ZVariant
  {$IFDEF WITH_WIDESTRUTILS}, WideStrUtils{$ENDIF};

type

  {** Implements Generic SQLite Statement. }
  TZSQLiteStatement = class(TZAbstractStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;

    function CreateResultSet(const SQL: string; StmtHandle: Psqlite_vm;
       ColumnCount: Integer; ColumnNames: PPAnsiChar;
       ColumnValues: PPAnsiChar): IZResultSet;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: Psqlite);

    function ExecuteQuery(const SQL: ZAnsiString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZAnsiString): Integer; override;
    function Execute(const SQL: ZAnsiString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZSQLitePreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: Psqlite);
  end;

implementation

uses
  Types, ZDbcSqLiteUtils, ZDbcSqLiteResultSet, ZSysUtils, ZEncoding,
  ZMessages, ZDbcCachedResultSet{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLiteStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native SQLite plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZSQLiteStatement.Create(PlainDriver: IZSQLitePlainDriver;
  Connection: IZConnection; Info: TStrings; Handle: Psqlite);
begin
  inherited Create(Connection, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}

function TZSQLiteStatement.CreateResultSet(const SQL: string; StmtHandle: Psqlite_vm;
   ColumnCount: Integer; ColumnNames: PPAnsiChar; ColumnValues: PPAnsiChar): IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  Self.SSQL := SQL;
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, SSQL, FHandle,
    StmtHandle, ColumnCount, ColumnNames, ColumnValues);
  NativeResultSet.SetConcurrency(rcReadOnly);

  { Creates a cached result set. }
  CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
    NativeResultSet.GetMetaData);
  CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SSQL,
    CachedResolver,GetConnection.GetConSettings);

  { Fetches all rows to prevent blocking. }
  CachedResultSet.SetType(rtScrollInsensitive);
  CachedResultSet.Last;
  CachedResultSet.BeforeFirst;
  CachedResultSet.SetConcurrency(GetResultSetConcurrency);

  Result := CachedResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZSQLiteStatement.ExecuteQuery(const SQL: ZAnsiString): IZResultSet;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQLTail: PAnsiChar;
  StmtHandle: Psqlite_vm;
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  ErrorMessage := '';
  SQLTail := '';
  ColumnCount := 0;
  ASQL := SQL; //preprepares SQL
  ErrorCode := FPlainDriver.Compile(FHandle, PAnsiChar(ASQL), Length(ASQL), SQLTail,
    StmtHandle, ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, LogSQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    raise;
  end;

  Result := CreateResultSet(SSQL, StmtHandle, ColumnCount, ColumnNames,
    ColumnValues);
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
function TZSQLiteStatement.ExecuteUpdate(const SQL: ZAnsiString): Integer;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
begin
  ErrorMessage := '';
  ASQL := SQL; //preprepares SQL
  ErrorCode := FPlainDriver.Execute(FHandle, PAnsiChar(ASQL), nil, nil,ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SSQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);
  Result := FPlainDriver.Changes(FHandle);
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
function TZSQLiteStatement.Execute(const SQL: ZAnsiString): Boolean;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
  SQLTail: PAnsiChar;
  StmtHandle: Psqlite_vm;
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  ErrorMessage := '';
  SQLTail := '';
  ColumnCount := 0;
  ColumnValues:=nil;
  ColumnNames:=nil;
  ASQL := SQL; //preprapares SQL
  ErrorCode := FPlainDriver.Compile(FHandle, PAnsiChar(ASQL), Length(ASQL), SQLTail,
    StmtHandle, ErrorMessage);
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcExecute, SSQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    raise;
  end;

  { Process queries with result sets }
  if ColumnCount <> 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(SSQL, StmtHandle, ColumnCount, ColumnNames,
      ColumnValues);
  end
  { Processes regular query. }
  else
  begin
    if assigned(ColumnValues) then
      Freemem(ColumnValues);
    if assigned(ColumnNames) then
      Freemem(ColumnNames);
    Result := False;
    LastUpdateCount := FPlainDriver.Changes(FHandle);
    ErrorCode := FPlainDriver.Finalize(StmtHandle, ErrorMessage);
    CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage, lcOther,
      'Finalize SQLite VM');
  end;
end;

{ TZSQLitePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native SQLite Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZSQLitePreparedStatement.Create(PlainDriver: IZSQLitePlainDriver;
  Connection: IZConnection; const SQL: string; Info: TStrings; Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZSQLitePreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZSQLiteStatement.Create(FPlainDriver, Connection, Info,FHandle);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZSQLitePreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer): ZAnsiString;
var
  Value: TZVariant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if DefVarManager.IsNull(Value)  then
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
        Result := EncodeString(AnsiString(SoftVarManager.GetAsString(Value)));
      stString:
        Result := ZPlainString(AnsiQuotedStr(SoftVarManager.GetAsString(Value), #39));
      stUnicodeString:
        {$IFDEF UNICODE}
        Result := ZPlainString(AnsiQuotedStr(SoftVarManager.GetAsUnicodeString(Value), #39));
        {$ELSE}
        Result := AnsiQuotedStr(ZPlainString(SoftVarManager.GetAsUnicodeString(Value)), #39);
        {$ENDIF}
      stDate:
        Result := '''' + ZAnsiString(FormatDateTime('yyyy-mm-dd',
          SoftVarManager.GetAsDateTime(Value))) + '''';
      stTime:
        Result := '''' + ZAnsiString(FormatDateTime('hh:mm:ss',
          SoftVarManager.GetAsDateTime(Value))) + '''';
      stTimestamp:
        Result := '''' + ZAnsiString(FormatDateTime('yyyy-mm-dd hh:mm:ss',
          SoftVarManager.GetAsDateTime(Value))) + '''';
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            if InParamTypes[ParamIndex] = stBinaryStream then
              Result := EncodeString(TempBlob.GetString)
            else
              Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(
                GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                TempBlob.Length, TempBlob.WasDecoded, ConSettings), #39)
          else
            Result := 'NULL';
        end;
    end;
  end;
end;

end.
