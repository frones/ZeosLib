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
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF WITH_WIDESTRUTILS}WideStrUtils, {$ENDIF}
  ZDbcIntfs, ZDbcStatement, ZPlainSqLiteDriver, ZCompatibility, ZDbcLogging,
  ZVariant;

type

  {** Implements Generic SQLite Statement. }
  TZSQLiteStatement = class(TZAbstractStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
    FForceNativeResultSet: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    function CreateResultSet(const StmtHandle: Psqlite_vm;
      const ErrorCode: Integer): IZResultSet;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; Info: TStrings; Handle: Psqlite);

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;
  end;

  {$IFDEF ZEOS_TEST_ONLY}
  {** Implements Prepared SQL Statement. }
  TZSQLitePreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
    FBindDoubleDateTimeValues: Boolean;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: Psqlite);
  end;
  {$ENDIF}

  {** Implements CAPI Prepared SQL Statement. }
  TZSQLiteCAPIPreparedStatement = class(TZAbstractRealPreparedStatement)
  private
    FErrorCode: Integer;
    FHandle: Psqlite;
    FStmtHandle: Psqlite3_stmt;
    FPlainDriver: IZSQLitePlainDriver;
    FForceNativeResultSet: Boolean;
    FBindDoubleDateTimeValues: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    function CreateResultSet(const StmtHandle: Psqlite_vm;
      const ErrorCode: Integer): IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(const PlainDriver: IZSQLitePlainDriver;
      const Connection: IZConnection; const SQL: string; const Info: TStrings;
      const Handle: Psqlite); overload;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;


implementation

uses
  Types{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}, ZDbcSqLiteUtils,
  ZDbcSqLiteResultSet, ZSysUtils, ZEncoding, ZMessages, ZDbcCachedResultSet,
  ZDbcUtils;

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
  FForceNativeResultSet :=  StrToBoolEx(DefineStatementParameter(Self, 'ForceNativeResultSet', 'false'));
  FUndefinedVarcharAsStringLength := StrToIntDef(DefineStatementParameter(Self, 'Undefined_Varchar_AsString_Length', '0'), 0);
end;

{**
  Creates a result set based on the current settings.
  @param SQL the select statement
  @param StmtHandle the SQLite Statement handle
  @return a created result set object.
}

function TZSQLiteStatement.CreateResultSet(const StmtHandle: Psqlite_vm;
  const ErrorCode: Integer): IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, Self.SQL, FHandle,
    StmtHandle, ErrorCode, FUndefinedVarcharAsStringLength, True);
  NativeResultSet.SetConcurrency(rcReadOnly);

  if FForceNativeResultSet then
    Result := NativeResultSet
  else
  begin
    { Creates a cached result set. }
    CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL,
      CachedResolver,GetConnection.GetConSettings);

    { Fetches all rows to prevent blocking (DataBase is locked).}
    CachedResultSet.Last;
    CachedResultSet.BeforeFirst;

    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
    Result := CachedResultSet;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZSQLiteStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
var
  ErrorCode: Integer;
  StmtHandle: Psqlite3_stmt;
begin
  ASQL := SQL; //preprepares SQL
  Result := nil;
  ErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL),
    StmtHandle, nil);
  CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, nil, lcExecute, ASQL, ConSettings);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  try
    ErrorCode := FPlainDriver.Step(StmtHandle);
    CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, nil, lcOther, 'FETCH', ConSettings);
    if FPlainDriver.column_count(StmtHandle) > 0 then
      Result := CreateResultSet(StmtHandle, ErrorCode);
  except
    FPlainDriver.Finalize(StmtHandle);
    raise;
  end;
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
function TZSQLiteStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsichar;
begin
  ASQL := SQL; //preprepares SQL
  ErrorCode := FPlainDriver.Execute(FHandle, PAnsiChar(ASQL), nil, nil,ErrorMessage);
  CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, ErrorMessage, lcExecute, ASQL, ConSettings);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
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
function TZSQLiteStatement.Execute(const SQL: RawByteString): Boolean;
var
  ErrorCode: Integer;
  StmtHandle: Psqlite_vm;
begin
  ASQL := SQL; //preprepares SQL
  ErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL),
    StmtHandle, nil);
  CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, nil, lcExecute, ASQL, ConSettings);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  try
    ErrorCode := FPlainDriver.Step(StmtHandle);
    CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, nil, lcOther, 'FETCH', ConSettings);
  except
    FPlainDriver.Finalize(StmtHandle);
    raise;
  end;

  { Process queries with result sets }
  if FPlainDriver.column_count(StmtHandle) <> 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(StmtHandle, ErrorCode);
  end
  else { Processes regular query. }
  begin
    Result := False;
    LastUpdateCount := FPlainDriver.Changes(FHandle);
    ErrorCode := FPlainDriver.Finalize(StmtHandle);
    CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, nil, lcOther,
      'Finalize SQLite VM', ConSettings);
  end;
end;

{$IFDEF ZEOS_TEST_ONLY}
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
  FBindDoubleDateTimeValues :=  StrToBoolEx(DefineStatementParameter(Self, 'BindDoubleDateTimeValues', 'false'));
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
function TZSQLitePreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
var
  Value: TZVariant;
  TempBlob: IZBlob;
  TempBytes: TBytes;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Value := InParamValues[ParamIndex];
  if ClientVarManager.IsNull(Value)  then
    Result := 'NULL'
  else
  begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if ClientVarManager.GetAsBoolean(Value) then
           Result := '''Y'''
        else
           Result := '''N''';
      stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
      stFloat, stDouble, stCurrency, stBigDecimal:
        Result := ClientVarManager.GetAsRawByteString(Value);
      stBytes:
        begin
          TempBytes := ClientVarManager.GetAsBytes(Value);
          Result := GetSQLHexAnsiString(PAnsiChar(TempBytes), Length(TempBytes));
        end;
      stString, stUnicodeString:
        Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}
          AnsiQuotedStr(PAnsiChar(ClientVarManager.GetAsRawByteString(Value)), #39);
      stDate:
        if FBindDoubleDateTimeValues then
          Result := FloatToRaw(ClientVarManager.GetAsDateTime(Value)-JulianEpoch)
        else
          Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
            ConSettings^.WriteFormatSettings, True);
      stTime:
        if FBindDoubleDateTimeValues then
          Result := FloatToRaw(ClientVarManager.GetAsDateTime(Value)-JulianEpoch)
        else
          Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
            ConSettings^.WriteFormatSettings, True);
      stTimestamp:
        if FBindDoubleDateTimeValues then
          Result := FloatToRaw(ClientVarManager.GetAsDateTime(Value)-JulianEpoch)
        else
          Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
            ConSettings^.WriteFormatSettings, True);
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            if InParamTypes[ParamIndex] = stBinaryStream then
              Result := GetSQLHexAnsiString(TempBlob.GetBuffer, TempBlob.Length)
            else
              Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(
                GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                TempBlob.Length, ConSettings), #39)
          else
            Result := 'NULL';
        end;
    end;
  end;
end;
{$ENDIF}

(* out of use now...
procedure BindingDestructor(Value: PAnsiChar); cdecl;
begin
  {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(Value);
end;*)

{ TZSQLiteCAPIPreparedStatement }

function TZSQLiteCAPIPreparedStatement.CreateResultSet(const StmtHandle: Psqlite_vm;
  const ErrorCode: Integer): IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
    NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, Self.SQL, FHandle,
      StmtHandle, ErrorCode, FUndefinedVarcharAsStringLength, False);
    NativeResultSet.SetConcurrency(rcReadOnly);

  if not FForceNativeResultSet then
  begin
    { Creates a cached result set. }
    CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL,
      CachedResolver,GetConnection.GetConSettings);

    { Fetches all rows to prevent blocking (DataBase is locked).}
    CachedResultSet.Last;
    CachedResultSet.BeforeFirst;
    
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);

    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZSQLiteCAPIPreparedStatement.PrepareInParameters;
begin
  if FPlainDriver.bind_parameter_count(FStmtHandle) <> InParamCount then
    raise Exception.Create('Invalid InParamCount');
end;

procedure TZSQLiteCAPIPreparedStatement.BindInParameters;
var
  TempBlob: IZBlob;
  I: Integer;
  Buffer: PAnsiChar;
  CharRec: TZCharRec;
begin
  FErrorcode := FPlainDriver.clear_bindings(FStmtHandle);
  CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcBindPrepStmt, ASQL, ConSettings);
  for i := 1 to InParamCount do
  begin
    if ClientVarManager.IsNull(InParamValues[i-1])  then
      FErrorcode := FPlainDriver.bind_null(FStmtHandle, I)
    else
    begin
      case InParamTypes[I-1] of
        stBoolean:
          if ClientVarManager.GetAsBoolean(InParamValues[i-1]) then
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
            PAnsiChar(AnsiString('Y')), 1, nil)
          else
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              PAnsichar(AnsiString('N')), 1, nil);
        stByte, stShort, stWord, stSmall, stInteger:
          FErrorcode := FPlainDriver.bind_int(FStmtHandle, i,
            ClientVarManager.GetAsInteger(InParamValues[i-1]));
        stLongWord, stLong, stUlong:
          FErrorcode := FPlainDriver.bind_int64(FStmtHandle, i,
            ClientVarManager.GetAsInteger(InParamValues[i-1]));
        stFloat, stDouble, stCurrency, stBigDecimal:
          FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
            ClientVarManager.GetAsFloat(InParamValues[i-1]));
        stBytes:
          begin
            InParamValues[i-1].VBytes := SoftVarManager.GetAsBytes(InParamValues[i-1]);
            FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
              @InParamValues[i-1].VBytes[0], Length(InParamValues[i-1].VBytes), nil);
          end;
        stString, stUnicodeString:
          begin
            CharRec := ClientVarManager.GetAsCharRec(InParamValues[i-1], zCP_UTF8);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              CharRec.P, CharRec.Len, nil);
          end;
        stDate:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLDate(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              PAnsiChar(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.DateFormatLen, nil);
          end;
        stTime:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLTime(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              PAnsiChar(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.TimeFormatLen, nil);
          end;
        stTimestamp:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLTimeStamp(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              PAnsiChar(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.DateTimeFormatLen, nil);
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[i-1]) as IZBlob;
            if not TempBlob.IsEmpty then
              if InParamTypes[I-1] = stBinaryStream then
              begin
                FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
                  TempBlob.GetBuffer, TempBlob.Length, nil)
              end
              else
                if TempBlob.IsClob then
                begin
                  Buffer := TempBlob.GetPAnsiChar(zCP_UTF8);
                  FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
                    Buffer, TempBlob.Length, nil);
                end
                else
                begin
                  InParamValues[I-1].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, ConSettings);
                  FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
                    PAnsiChar(InParamValues[I-1].VRawByteString),
                    Length(InParamValues[I-1].VRawByteString), nil);
                end
            else
              FErrorcode := FPlainDriver.bind_null(FStmtHandle, I);
          end;
      end;
    end;
    CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcBindPrepStmt, ASQL, ConSettings);
  end;
  inherited BindInParameters;
end;

constructor TZSQLiteCAPIPreparedStatement.Create(
  const PlainDriver: IZSQLitePlainDriver; const Connection: IZConnection;
  const SQL: string; const Info: TStrings; const Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FStmtHandle := nil;
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  FForceNativeResultSet :=  StrToBoolEx(DefineStatementParameter(Self, 'ForceNativeResultSet', 'false'));
  FBindDoubleDateTimeValues :=  StrToBoolEx(DefineStatementParameter(Self, 'BindDoubleDateTimeValues', 'false'));
  FUndefinedVarcharAsStringLength := StrToIntDef(DefineStatementParameter(Self, 'Undefined_Varchar_AsString_Length', '0'), 0);
end;

procedure TZSQLiteCAPIPreparedStatement.Prepare;
begin
  FErrorCode := FPlainDriver.Prepare(FHandle, PAnsiChar(ASQL), Length(ASQL), FStmtHandle, nil);
  CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, nil, lcPrepStmt, ASQL, ConSettings);
  inherited Prepare;
end;

procedure TZSQLiteCAPIPreparedStatement.Unprepare;
begin
  ClearParameters;
  CheckSQLiteError(FPlainDriver, FStmtHandle, FPlainDriver.Finalize(FStmtHandle),
    nil, lcUnprepStmt, 'Unprepare SQLite Statement', ConSettings);
  FStmtHandle := nil;
  inherited UnPrepare;
end;

function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if Not Prepared then
     Prepare;

  if LastResultSet <> nil then LastResultSet.Close; // reset stmt-handle
  LastResultSet := nil; //keep track we do not return a closed ResultSet
  BindInParameters;

  FErrorCode := FPlainDriver.Step(FStmtHandle); //exec prepared
  CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcOther,
    ConSettings^.ConvFuncs.ZStringToRaw(SCanNotRetrieveResultsetData,
    ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP), ConSettings);
  if FPlainDriver.column_count(FStmtHandle) = 0 then
    FPlainDriver.reset(FStmtHandle) //reset handle now!
  else //expect a resultset
    LastResultSet := CreateResultSet(FStmtHandle, FErrorCode); //resultset executes reset stmt-handle
  Result := LastResultSet;

  inherited ExecuteQueryPrepared; //Log values
end;

function TZSQLiteCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  if Not Prepared then
     Prepare;
  BindInParameters;

  Result := 0;
  try
    CheckSQLiteError(FPlainDriver, FStmtHandle, FPlainDriver.Step(FStmtHandle),
      nil, lcExecPrepStmt, ASQL, ConSettings); //exec prepared
    Result := FPlainDriver.Changes(FHandle);
    inherited ExecuteUpdatePrepared; //log values
  finally
    CheckSQLiteError(FPlainDriver, FStmtHandle, FPlainDriver.reset(FStmtHandle),
      nil, lcOther, 'Reset', ConSettings); //reset handle
    LastUpdateCount := Result;
  end;
end;

function TZSQLiteCAPIPreparedStatement.ExecutePrepared: Boolean;
begin
  if Not Prepared then
     Prepare;

  BindInParameters;

  FErrorCode := FPlainDriver.Step(FStmtHandle);
  CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcExecPrepStmt, 'Step', ConSettings);

  { Process queries with result sets }
  if FPlainDriver.column_count(FStmtHandle) <> 0 then
  begin
    Result := True;
    LastResultSet := CreateResultSet(FStmtHandle, FErrorCode);
  end
  { Processes regular query. }
  else
  begin
    Result := False;
    LastUpdateCount := FPlainDriver.Changes(FHandle);
    FErrorCode := FPlainDriver.reset(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcOther, 'Reset', ConSettings);
  end;
  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;

  inherited ExecutePrepared;
end;

end.

