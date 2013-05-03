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

  {** Implements CAPI Prepared SQL Statement. }
  TZSQLiteCAPIPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FErrorCode: Integer;
    FHandle: Psqlite;
    FStmtHandle: Psqlite3_stmt;
    FPlainDriver: IZSQLitePlainDriver;
    function CreateResultSet(const SQL: string; StmtHandle: Psqlite_vm;
       ColumnCount: Integer; ColumnNames: PPAnsiChar;
       ColumnValues: PPAnsiChar): IZResultSet;
  protected
    procedure SetASQL(const Value: ZAnsiString); override;
    procedure SetWSQL(const Value: ZWideString); override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings; Handle: Psqlite);

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
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
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    StmtHandle, ColumnCount, ColumnNames, ColumnValues);
  NativeResultSet.SetConcurrency(rcReadOnly);

  { Creates a cached result set. }
  CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
    NativeResultSet.GetMetaData);
  CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
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
  StmtHandle: Psqlite3_stmt;
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  ColumnCount := 0;
  ASQL := SQL; //preprepares SQL
  ErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL),
    StmtHandle, nil);
  CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcExecute, LogSQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, LogSQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle);
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
  ErrorMessage: PAnsichar;
begin
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
  StmtHandle: Psqlite_vm;
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  ColumnCount := 0;
  ColumnValues:=nil;
  ColumnNames:=nil;
  ASQL := SQL; //preprapares SQL
  ErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL),
    StmtHandle, nil);
  CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcExecute, SSQL);
  DriverManager.LogMessage(lcExecute, FPlainDriver.GetProtocol, SSQL);

  try
    ErrorCode := FPlainDriver.Step(StmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
  except
    FPlainDriver.Finalize(StmtHandle);
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
    ErrorCode := FPlainDriver.Finalize(StmtHandle);
    CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther,
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
  Prepare;
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
  TempBlob: IZBlob;
begin
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


procedure BindingDestructor(Value: PAnsiChar); cdecl;
begin
  {$IFDEF DELPHI18_UP}
  SysUtils.StrDispose(Value);
  {$ELSE}
  StrDispose(Value);
  {$ENDIF}
end;

{ TZSQLiteCAPIPreparedStatement }

function TZSQLiteCAPIPreparedStatement.CreateResultSet(const SQL: string;
  StmtHandle: Psqlite_vm; ColumnCount: Integer; ColumnNames: PPAnsiChar;
  ColumnValues: PPAnsiChar): IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  Self.SSQL := SQL;
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, SSQL, FHandle,
    StmtHandle, ColumnCount, ColumnNames, ColumnValues, False);
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

procedure TZSQLiteCAPIPreparedStatement.SetASQL(const Value: ZAnsiString);
begin
  if ( ASQL <> Value ) and Prepared then
    Unprepare;
  inherited SetASQL(Value);
end;

procedure TZSQLiteCAPIPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if ( WSQL <> Value ) and Prepared then
    Unprepare;
  inherited SetWSQL(Value);
end;

procedure TZSQLiteCAPIPreparedStatement.PrepareInParameters;
begin
  if FPlainDriver.bind_parameter_count(FStmtHandle) <> InParamCount then
    raise Exception.Create('Invalid InParamCount');
end;

procedure TZSQLiteCAPIPreparedStatement.BindInParameters;
var
  Value: TZVariant;
  TempBlob: IZBlob;
  I, L: Integer;
  TempAnsi: ZAnsiString;

  Function AsPAnsiChar(Const S : ZAnsiString; Len: Integer) : PAnsiChar;
  begin
    Result := {$IFDEF UNICODE}AnsiStrAlloc{$ELSE}StrAlloc{$ENDIF}(Len);
    System.Move(PAnsiChar(S)^, Result^, Len);
  end;

begin
  FErrorcode := FPlainDriver.clear_bindings(FStmtHandle);
  CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcBindPrepStmt, SSQL);
  for i := 1 to InParamCount do
  begin
    Value := InParamValues[i-1];
    if DefVarManager.IsNull(Value)  then
      FErrorcode := FPlainDriver.bind_null(FStmtHandle, I)
    else
    begin
      case InParamTypes[I-1] of
        stBoolean:
          if SoftVarManager.GetAsBoolean(Value) then
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
              SysUtils.StrNew(PAnsiChar('Y')), 1, @BindingDestructor)
            {$ELSE}
              StrNew(PAnsiChar('Y')), 1, @BindingDestructor)
            {$ENDIF}
          else
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
              SysUtils.StrNew(PAnsichar('N')), 1, @BindingDestructor);
            {$ELSE}
              StrNew(PAnsichar('N')), 1, @BindingDestructor);
            {$ENDIF}
        stByte, stShort, stInteger:
          FErrorcode := FPlainDriver.bind_int(FStmtHandle, i,
            SoftVarManager.GetAsInteger(Value));
        stLong:
          FErrorcode := FPlainDriver.bind_int64(FStmtHandle, i,
            SoftVarManager.GetAsInteger(Value));
        stBigDecimal, stFloat, stDouble:
          FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
            SoftVarManager.GetAsFloat(Value));
        stBytes:
          begin
            TempAnsi := ZAnsiString(SoftVarManager.GetAsString(Value));
            L := Length(TempAnsi);
            FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
              AsPAnsiChar(TempAnsi, L), L, @BindingDestructor)
          end;
        stString:
          FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
            SysUtils.StrNew(PAnsichar(ZPlainString(SoftVarManager.GetAsString(Value)))),
            {$ELSE}
            StrNew(PAnsichar(ZPlainString(SoftVarManager.GetAsString(Value)))),
            {$ENDIF}
              -1, @BindingDestructor);
        stUnicodeString:
          FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
            SysUtils.StrNew(PAnsichar(ZPlainString(SoftVarManager.GetAsUnicodeString(Value)))),
            {$ELSE}
            StrNew(PAnsichar(ZPlainString(SoftVarManager.GetAsUnicodeString(Value)))),
            {$ENDIF}

               -1, @BindingDestructor);
        stDate:
          FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
            SysUtils.StrNew(PAnsichar(ZAnsiString(FormatDateTime('yyyy-mm-dd',
             {$ELSE}
            StrNew(PAnsichar(ZAnsiString(FormatDateTime('yyyy-mm-dd',
            {$ENDIF}
            SoftVarManager.GetAsDateTime(Value))))),
                -1, @BindingDestructor);
        stTime:
          FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
            SysUtils.StrNew(PAnsichar(ZAnsiString(FormatDateTime('hh:mm:ss',
            {$ELSE}
            StrNew(PAnsichar(ZAnsiString(FormatDateTime('hh:mm:ss',
            {$ENDIF}
            SoftVarManager.GetAsDateTime(Value))))),
                -1, @BindingDestructor);
        stTimestamp:
          FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            {$IFDEF DELPHI18_UP}
            SysUtils.StrNew(PAnsichar(ZAnsiString(FormatDateTime('yyyy-mm-dd hh:mm:ss',
            {$ELSE}
            StrNew(PAnsichar(ZAnsiString(FormatDateTime('yyyy-mm-dd hh:mm:ss',
            {$ENDIF}
            SoftVarManager.GetAsDateTime(Value))))),
                -1, @BindingDestructor);
        { works equal but selects from data which was written in string format
          won't match! e.G. TestQuery etc. On the other hand-> i've prepared
          this case on the resultsets too. JULIAN_DAY_PRECISION?}
        {stDate, stTime, stTimestamp:
          FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
            SoftVarManager.GetAsDateTime(Value));}
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := DefVarManager.GetAsInterface(Value) as IZBlob;
            if not TempBlob.IsEmpty then
              if InParamTypes[I-1] = stBinaryStream then
              begin
                TempAnsi := TempBlob.GetString;
                FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
                  AsPAnsiChar(TempAnsi, TempBlob.Length), TempBlob.Length,
                    @BindingDestructor)
              end
              else
              begin
                TempAnsi := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, TempBlob.WasDecoded, ConSettings);
                FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
                  {$IFDEF DELPHI18_UP}
                  SysUtils.StrNew(PAnsiChar(TempAnsi)),
                  {$ELSE}
                  StrNew(PAnsiChar(TempAnsi)),
                  {$ENDIF}
                  Length(TempAnsi), @BindingDestructor);
              end
            else
              FErrorcode := FPlainDriver.bind_null(FStmtHandle, I);
          end;
      end;
    end;
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcBindPrepStmt, SSQL);
  end;
end;

constructor TZSQLiteCAPIPreparedStatement.Create(PlainDriver: IZSQLitePlainDriver;
  Connection: IZConnection; const SQL: string; Info: TStrings; Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
end;

procedure TZSQLiteCAPIPreparedStatement.Prepare;
begin
  FErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL), FStmtHandle, nil);
  CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcPrepStmt, SSQL);
  inherited Prepare;
end;

procedure TZSQLiteCAPIPreparedStatement.Unprepare;
begin
  ClearParameters;
  if Assigned(FStmtHandle) then
    FErrorCode := FPlainDriver.Finalize(FStmtHandle)
  else
    FErrorCode := SQLITE_OK;
  FStmtHandle := nil;
  CheckSQLiteError(FPlainDriver, FErrorCode, nil,
    lcUnprepStmt, 'Unprepare SQLite Statment');
  inherited UnPrepare;
end;

function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  if Not Prepared then
     Prepare;
  { after reading the last row we reset the statment. So we don't need this here }
  ColumnValues := nil;
  ColumnNames := nil;
  ColumnCount := 0;
  try
    BindInParameters;
    FErrorCode := FPlainDriver.Step(FStmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcOther, SCanNotRetrieveResultsetData);
  except
    if ColumnValues <> nil then
      FreeMem(ColumnValues);
    ColumnValues := nil;
    if ColumnNames <> nil then
      FreeMem(ColumnNames);
    ColumnNames := nil;
    Unprepare;
    raise;
  end;

  Result := CreateResultSet(SSQL, FStmtHandle, ColumnCount, ColumnNames,
    ColumnValues);
end;

function TZSQLiteCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  if Not Prepared then
     Prepare;
  BindInParameters;

  Result := 0;
  try
    FErrorCode := FPlainDriver.Step(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcExecPrepStmt, SSQL);
    Result := FPlainDriver.Changes(FHandle);
  finally
    FErrorCode := FPlainDriver.reset(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcOther, 'Reset');
    LastUpdateCount := Result;
  end;
end;

function TZSQLiteCAPIPreparedStatement.ExecutePrepared: Boolean;
var
  ColumnCount: Integer;
  ColumnValues: PPAnsiChar;
  ColumnNames: PPAnsiChar;
begin
  if Not Prepared then
     Prepare;

  ColumnCount := 0;
  ColumnValues:=nil;
  ColumnNames:=nil;
  try
    BindInParameters;

    FErrorCode := FPlainDriver.Step(FStmtHandle, ColumnCount,
      ColumnValues, ColumnNames);
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcExecPrepStmt, 'Step');
  except
    UnPrepare;
    raise;
  end;

  { Process queries with result sets }
  if ColumnCount <> 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(SSQL, FStmtHandle, ColumnCount, ColumnNames,
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
    FErrorCode := FPlainDriver.reset(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FErrorCode, nil, lcOther, 'Reset');
  end;
end;

end.

