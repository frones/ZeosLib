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
  ZDbcIntfs, ZDbcStatement, ZPlainSqLiteDriver, ZCompatibility, ZDbcLogging,
  ZVariant, Types;

type
  {** Implements CAPI Prepared SQL Statement. }
  TZAbstractSQLiteCAPIPreparedStatement = class(TZRawPreparedStatement)
  private
    FErrorCode: Integer;
    FHandle: Psqlite;
    FStmtHandle: Psqlite3_stmt;
    FPlainDriver: TZSQLitePlainDriver;
    FUndefinedVarcharAsStringLength: Integer;
    FBindDoubleDateTimeValues, //are DoubleValues used as Date+/time values?
    FBindOrdinalBoolValues, //do we bind 0/1 as Boolean?
    FHasLoggingListener: Boolean;
    FBindLater, //Late bindings?
    FLateBound: Boolean; //LateBound done reset is'nt called -> continue LateBindings
    function CreateResultSet: IZResultSet;
  protected
    procedure ResetCallBack;
  protected
    procedure CheckParameterIndex(Index: Integer); override;
    function GetLastErrorCodeAndHandle(var StmtHandle: Psqlite3_stmt): Integer;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; override;
  protected
    procedure BindNull(Index: Integer; SQLType: TZSQLType); override;
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; {%H-}SQLType: TZSQLType; const Value: Double); override;
    procedure BindUnsignedOrdinal(Index: Integer; {%H-}SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindSignedOrdinal(Index: Integer; {%H-}SQLType: TZSQLType; const Value: Int64); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;
  public
    constructor Create(const Connection: IZConnection;
      const SQL: string; const Info: TStrings; const Handle: Psqlite);

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure Cancel; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZSQLiteCAPIPreparedStatement = class(TZAbstractSQLiteCAPIPreparedStatement, IZPreparedStatement);

  TZSQLiteStatement = class(TZAbstractSQLiteCAPIPreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings;
      const Handle: Psqlite); overload;
  end;


implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS} AnsiStrings,{$ENDIF} ZDbcSqLiteUtils,
  ZDbcSqLiteResultSet, ZSysUtils, ZEncoding, ZMessages, ZDbcCachedResultSet,
  ZDbcUtils, ZDbcProperties, ZFastCode, ZClasses;

const DeprecatedBoolRaw: array[Boolean] of {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = ('N','Y');

(* out of use now...
procedure BindingDestructor(Value: PAnsiChar); cdecl;
begin
  {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(Value);
end;*)

{ TZAbstractSQLiteCAPIPreparedStatement }

function TZAbstractSQLiteCAPIPreparedStatement.GetLastErrorCodeAndHandle(
  var StmtHandle: Psqlite3_stmt): Integer;
begin
  Result := FErrorCode;
  StmtHandle := FStmtHandle;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.CheckParameterIndex(Index: Integer);
begin
  if not Prepared then begin
    Prepare;
    FBindLater := False;
  end;
  if (BindList.Count < Index+1) then
    raise EZSQLException.Create(SInvalidInputParameterCount);
end;

function TZAbstractSQLiteCAPIPreparedStatement.CreateResultSet: IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  NativeResultSet := TZSQLiteResultSet.Create(Self, Self.SQL, @FHandle,
    @FStmtHandle, @FErrorCode, FUndefinedVarcharAsStringLength, ResetCallBack);
  NativeResultSet.SetConcurrency(rcReadOnly);

  if (GetResultSetConcurrency = rcUpdatable)
    or (GetResultSetType <> rtForwardOnly) then
  begin
    { Creates a cached result set. }
    CachedResolver := TZSQLiteCachedResolver.Create(FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL,
      CachedResolver,GetConnection.GetConSettings);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);

    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result); //weak reference to Resultset to avoid NO decrementing of RefCount.
    //we need this reference to close the SQLite resultset and reset the stmt handle.
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.PrepareInParameters;
begin
  SetParamCount(FPlainDriver.sqlite3_bind_parameter_count(FStmtHandle));
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.ResetCallBack;
var ErrorCode: Integer;
begin
  if Assigned(FStmtHandle) then begin
    ErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle); //reset handle now!
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcExecute, 'sqlite3_reset', ConSettings);
  end;
  if not FLateBound then
    FBindLater := False;
end;

function TZAbstractSQLiteCAPIPreparedStatement.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
begin
  Result := Value;
  RaiseUnsupportedException;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var ErrorCode: Integer;
begin
  inherited BindBinary(Index, SQLType, Buf, Len);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, Buf, Len, nil);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  if fBindOrdinalBoolValues
  then BindSignedOrdinal(Index, stLong, Ord(Value))
  else BindRawStr(Index, DeprecatedBoolRaw[Value]);
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
var
  BindVal: PZBindValue;
  ErrorCode: Integer;
begin
  if FBindDoubleDateTimeValues then
    BindDouble(Index, SQLType, Value-JulianEpoch)
  else begin
    CheckParameterIndex(Index);
    BindVal := BindList[Index];
    if SQLType = stDate then
      if (BindVal.BindType <> zbtRawString) or (Length(RawByteString(BindVal.Value)) <> ConSettings^.WriteFormatSettings.DateFormatLen)
      then Bindlist.Put(Index, stString, DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False), zCP_UTF8)
      else DateTimeToRawSQLDate(Value, BindVal.Value, ConSettings^.WriteFormatSettings, False)
    else if SQLType = stTime then
      if (BindVal.BindType <> zbtRawString) or (Length(RawByteString(BindVal.Value)) <> ConSettings^.WriteFormatSettings.TimeFormatLen)
      then Bindlist.Put(Index, stString, DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False), zCP_UTF8)
      else DateTimeToRawSQLTime(Value, BindVal.Value, ConSettings^.WriteFormatSettings, False)
    else
      if (BindVal.BindType <> zbtRawString) or (Length(RawByteString(BindVal.Value)) <> ConSettings^.WriteFormatSettings.DateTimeFormatLen)
      then Bindlist.Put(Index, stString, DateTimeToRawSQLTimestamp(Value, ConSettings^.WriteFormatSettings, False), zCP_UTF8)
      else DateTimeToRawSQLTimestamp(Value, BindVal.Value, ConSettings^.WriteFormatSettings, False);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, BindVal.Value,
        Length(RawByteString(BindVal.Value)), nil);
      if ErrorCode <> SQLITE_OK then
        CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcBindPrepStmt, ASQL, ConSettings);
    end;
  end;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var ErrorCode: Integer;
begin
  if FBindLater or FHasLoggingListener
  then inherited BindDouble(Index, stDouble, Value)
  else CheckParameterIndex(Index);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, Index +1, Value);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindInParameters;
var
  I, Errorcode: Integer;
  BindVal: PZBindValue;
begin
  if FBindLater and (BindList.Count > 0) then begin
    for i := 0 to BindList.Count-1 do begin
      BindVal := BindList[i];
      if BindVal.BindType = zbtNull then
        Errorcode := FPlainDriver.sqlite3_bind_null(FStmtHandle, I +1)
      else case BindVal^.SQLType of
        stString:   if BindVal.BindType = zbtRawString
                    then Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, I +1, BindVal.Value, Length(RawByteString(BindVal.Value)), nil)
                    else Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, I +1, PZCharRec(BindVal.Value).P, PZCharRec(BindVal.Value).Len, nil);
        stLong:     Errorcode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, I +1, PInt64(BindList._8Bytes[I])^);
        stDouble:   Errorcode := FPlainDriver.sqlite3_bind_Double(FStmtHandle, I +1, PDouble(BindList._8Bytes[I])^);
        stBytes:    if BindVal.BindType = zbtBytes
                    then Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, BindVal.Value, Length(TBytes(BindVal.Value)), nil)
                    else Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, PZBufRec(BindVal.Value).Buf, PZBufRec(BindVal.Value).Len, nil);
        stAsciiStream: Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, I +1, IZBlob(BindVal.Value).GetBuffer, IZBlob(BindVal.Value).Length, nil);
        stBinaryStream,
        stUnicodeStream:Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, IZBlob(BindVal.Value).GetBuffer, IZBlob(BindVal.Value).Length, nil);
        else begin
          ErrorCode := SQLITE_ERROR; //satisfy comiler
          RaiseUnsupportedParameterTypeException(BindVal^.SQLType);
        end;
      end;
      if ErrorCode <> SQLITE_OK then
        CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
    end;
  end else
    FLateBound := False;
  if FHasLoggingListener then
    inherited BindInParameters;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var ErrorCode: Integer;
begin
  inherited; //localize lob and make clob conversion if reqired
  if not FBindLater then begin
    if (Value = nil) or Value.IsEmpty then
      Errorcode := FPlainDriver.sqlite3_bind_null(FStmtHandle, Index +1)
    else if SQLType = stBinaryStream
      then Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, Value.GetBuffer, Value.Length, nil)
      else Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Value.GetBuffer, Value.Length, nil);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end;
end;

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
  @param Handle the sqlite connection handle
}
constructor TZAbstractSQLiteCAPIPreparedStatement.Create(
  const Connection: IZConnection;
  const SQL: string; const Info: TStrings; const Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FStmtHandle := nil;
  FHandle := Handle;
  FPlainDriver := TZSQLitePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  FBindDoubleDateTimeValues :=  StrToBoolEx(DefineStatementParameter(Self, DSProps_BindDoubleDateTimeValues, 'false'));
  FUndefinedVarcharAsStringLength := StrToIntDef(DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'), 0);
  fBindOrdinalBoolValues := StrToBoolEx(DefineStatementParameter(Self, DSProps_BindOrdinalBoolValues, 'false'));
  FHasLoggingListener := DriverManager.HasLoggingListener;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractSQLiteCAPIPreparedStatement.Prepare;
var pzTail: PAnsiChar;
begin
  if not Prepared then begin
    FErrorCode := FPlainDriver.sqlite3_prepare_v2(FHandle, Pointer(ASQL), Length(ASQL), FStmtHandle, pzTail{%H-});
    if FErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcPrepStmt, ASQL, ConSettings);
    inherited Prepare;
  end;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractSQLiteCAPIPreparedStatement.Unprepare;
var ErrorCode: Integer;
begin
  { EH: do not change this sequence!: first close possbile opened resultset}
  inherited UnPrepare;
  if Assigned(FStmtHandle) then begin
    ErrorCode := FPlainDriver.sqlite3_finalize(FStmtHandle);
    FStmtHandle := nil; //Keep track we do not try to finalize the handle again on destroy or so
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode,
        lcUnprepStmt, 'sqlite3_finalize', ConSettings);
  end;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindNull(Index: Integer;
  SQLType: TZSQLType);
var ErrorCode: Integer;
begin
  inherited;
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_null(FStmtHandle, Index +1);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var ErrorCode: Integer;
begin
  if FBindLater or FHasLoggingListener
  then inherited BindRawStr(Index, Buf, Len)
  else CheckParameterIndex(Index);
  if not FBindLater then begin
    if (Buf = nil) then
      Buf := PEmptyAnsiString;
    ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Buf, Len, nil);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
var ErrorCode: Integer;
begin
  inherited BindRawStr(Index, Value); //localize -> no val destructor
  if not FBindLater then begin
    if (Pointer(Value) = nil)
    then ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, PEmptyAnsiString, 0, nil)
    else ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Pointer(Value), Length(Value), nil);
      if ErrorCode <> SQLITE_OK then
        CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: Int64);
var ErrorCode: Integer;
begin
  if FBindLater or FHasLoggingListener
  then inherited BindSignedOrdinal(Index, stLong, Value)
  else CheckParameterIndex(Index);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, Index +1, Value);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: UInt64);
begin
  BindSignedOrdinal(Index, stLong, Int64(Value));
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractSQLiteCAPIPreparedStatement.Cancel;
begin
  FPlainDriver.sqlite3_interrupt(FHandle);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  BindInParameters;
  FBindLater := False;
  FErrorCode := FPlainDriver.sqlite3_step(FStmtHandle); //exec prepared
  if not (FErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcExecPrepStmt, ASQL, ConSettings);
  if (FErrorCode <> SQLITE_ROW) and (FPlainDriver.sqlite3_column_count(FStmtHandle) = 0) then begin
    FErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    if FErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcOther, ASQL, ConSettings); //exec prepared
    Result := nil;
  end else if Assigned(FOpenResultSet) //expect a resultset
    then Result := IZResultSet(FOpenResultSet) //return allready reseted RS
    else Result := CreateResultSet; //resultset executes reset stmt-handle
  if FHasLoggingListener then
    inherited ExecuteQueryPrepared; //Log values
  FBindLater := Assigned(Result);
  FHasLoggingListener := DriverManager.HasLoggingListener;
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
function TZAbstractSQLiteCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
var ErrorCode: Integer;
begin
  Prepare;
  BindInParameters;

  Result := -1;
  FBindLater := False;
  ErrorCode := fPlainDriver.sqlite3_step(FStmtHandle);
  if (ErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then begin
    Result := FPlainDriver.sqlite3_Changes(FHandle);
    if FHasLoggingListener then
      inherited ExecuteUpdatePrepared; //log values
    ErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcExecPrepStmt, ASQL, ConSettings); //exec prepared
    LastUpdateCount := Result;
    FHasLoggingListener := DriverManager.HasLoggingListener;
  end else
    try
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcExecPrepStmt, ASQL, ConSettings); //exec prepared
    finally
      FPlainDriver.sqlite3_reset(FStmtHandle); //reset handle allways without check else -> leaking mem
      LastUpdateCount := Result;
      FHasLoggingListener := DriverManager.HasLoggingListener;
    end;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractSQLiteCAPIPreparedStatement.ExecutePrepared: Boolean;
begin
  PrepareLastResultSetForReUse;
  Prepare;
  BindInParameters;

  FBindLater := False;
  FErrorCode := FPlainDriver.sqlite3_step(FStmtHandle);
  if not (FErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcExecPrepStmt, 'Step', ConSettings);
  { Process queries with result sets }
  if (FErrorCode = SQLITE_ROW) or (FPlainDriver.sqlite3_column_count(FStmtHandle) <> 0) then begin
    Result := True;
    LastResultSet := CreateResultSet;
  end else begin { Processes regular query. }
    Result := False;
    LastUpdateCount := FPlainDriver.sqlite3_Changes(FHandle);
    FErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    LastResultSet := nil;
    if FErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcOther, 'Reset', ConSettings);
  end;
  if FHasLoggingListener then
    inherited ExecutePrepared;
  FHasLoggingListener := DriverManager.HasLoggingListener;
end;

{ TZSQLiteStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle the sqlite connection handle
}
constructor TZSQLiteStatement.Create(const Connection: IZConnection;
  const Info: TStrings; const Handle: Psqlite);
begin
  inherited Create(Connection, '', Info, Handle);
end;

end.

