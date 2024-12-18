{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD, Types,
  {$IFNDEF FPC}ZClasses,{$ENDIF} //inlined Get method of TZCustomElementList
  ZCompatibility, ZVariant, ZPlainSqLiteDriver,
  ZDbcIntfs, ZDbcStatement, ZDbcSqLite, ZDbcLogging;

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
    FSQLiteIntAffinity, //see docs
    FLateBound: Boolean; //LateBound done reset is'nt called -> continue LateBindings
    FByteBuffer: PByteBuffer;
    FSQLiteConnection: IZSQLiteConnection;
    function CreateResultSet: IZResultSet;
  protected
    procedure ResetCallBack;
  protected
    procedure CheckParameterIndex(var Index: Integer); override;
    function GetLastErrorCodeAndHandle(var StmtHandle: Psqlite3_stmt): Integer;
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;
  public
    constructor Create(const Connection: IZSQLiteConnection;
      const SQL: string; const Info: TStrings);

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure Cancel; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZSQLiteCAPIPreparedStatement = class(TZAbstractSQLiteCAPIPreparedStatement, IZPreparedStatement)
  public
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual

    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(ParameterIndex: Integer; {%H-}SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency); reintroduce;
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(ParameterIndex: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD); reintroduce;
    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); reintroduce; overload;
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
  end;

  TZSQLiteStatement = class(TZAbstractSQLiteCAPIPreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZSQLiteConnection;
      const Info: TStrings);
  end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS} AnsiStrings,{$ENDIF}
  ZDbcSqLiteResultSet, ZSysUtils, ZEncoding, ZMessages, ZDbcCachedResultSet,
  ZDbcUtils, ZDbcProperties, ZFastCode, ZExceptions;

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

procedure TZAbstractSQLiteCAPIPreparedStatement.CheckParameterIndex(var Index: Integer);
begin
  if not Prepared then begin
    Prepare;
    FBindLater := False;
  end;
  if (BindList.Count < Index+1) then begin
    {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := Format(SBindVarOutOfRange, [Index]);
    raise EZSQLException.Create({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
  end;
end;

function TZAbstractSQLiteCAPIPreparedStatement.CreateResultSet: IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
  FetchAll: Boolean;
begin
  FetchAll := StrToBoolDef(GetConnection.GetParameters.Values[ConnProps_SQLiteFetchAll], false);
  FetchAll := FetchAll or StrToBoolDef(GetParameters.Values[ConnProps_SQLiteFetchAll], false);

  { Creates a native result set. }
  NativeResultSet := TZSQLiteResultSet.Create(Self, Self.SQL,
    @FStmtHandle, @FErrorCode, FUndefinedVarcharAsStringLength, ResetCallBack,
    FSQLiteIntAffinity);
  NativeResultSet.SetConcurrency(rcReadOnly);

  if (GetResultSetConcurrency = rcUpdatable)
    or (GetResultSetType <> rtForwardOnly) or FetchAll then
  begin
    { Creates a cached result set. }
    CachedResolver := TZSQLiteCachedResolver.Create(FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZSQLiteCachedResultSet.Create(NativeResultSet, Self.SQL,
      CachedResolver,GetConnection.GetConSettings);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);

    if FetchAll then begin
      CachedResultSet.AfterLast;
      CachedResultSet.BeforeFirst;
    end;

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
      FSQLiteConnection.HandleErrorOrWarning(lcOther, ErrorCode, 'sqlite3_reset',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
  if not FLateBound then
    FBindLater := False;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var ErrorCode: Integer;
begin
  inherited BindBinary(Index, SQLType, Buf, Len);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, Buf, Len, nil);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_blob',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{**
  Binds the input parameters
}
procedure TZAbstractSQLiteCAPIPreparedStatement.BindInParameters;
var
  I, Errorcode: Integer;
  BindVal: PZBindValue;
  P: Pointer;
  L: NativeUInt;
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
        stInteger:  Errorcode := FPlainDriver.sqlite3_bind_int(FStmtHandle, I +1, PInteger(BindList._4Bytes[I])^);
        stLong,
        stCurrency: Errorcode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, I +1, PInt64(BindList._8Bytes[I])^);
        stDouble:   Errorcode := FPlainDriver.sqlite3_bind_Double(FStmtHandle, I +1, PDouble(BindList._8Bytes[I])^);
        stBytes:    if BindVal.BindType = zbtBytes
                    then Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, BindVal.Value, Length(TBytes(BindVal.Value)), nil)
                    else Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, PZBufRec(BindVal.Value).Buf, PZBufRec(BindVal.Value).Len, nil);
        stAsciiStream: begin
                    P := IZBlob(BindVal.Value).GetBuffer(FRawTemp, L);
                    Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, I +1, P, L, nil);
                  end;
        stBinaryStream: begin
                    P := IZBlob(BindVal.Value).GetBuffer(FRawTemp, L);
                    Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, P, L, nil);
                  end;
        else raise CreateUnsupportedParameterTypeException(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindVal^.SQLType);
      end;
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_xxx',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else
    FLateBound := False;
  if FHasLoggingListener then
    inherited BindInParameters;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var ErrorCode: Integer;
  P: Pointer;
  L: NativeUInt;
begin
  inherited; //localize lob and make clob conversion if reqired
  if not FBindLater then begin
    if (Value = nil) or Value.IsEmpty then
      Errorcode := FPlainDriver.sqlite3_bind_null(FStmtHandle, Index +1)
    else begin
      P := Value.GetBuffer(FRawTemp, L);
      if SQLType = stBinaryStream
      then Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, P, L, nil)
      else Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, P, L, nil);
    end;
    if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_xxx',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
end;

{**
  Constructs this object and assigns main properties.
  @param Connection a sqlite database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractSQLiteCAPIPreparedStatement.Create(
  const Connection: IZSQLiteConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FStmtHandle := nil;
  FHandle := Connection.GetConnectionHandle;
  FByteBuffer := Connection.GetByteBufferAddress;
  FPlainDriver := Connection.GetPlainDriver;
  ResultSetType := rtForwardOnly;
  FSQLiteConnection := Connection;
  FBindDoubleDateTimeValues :=  StrToBoolEx(DefineStatementParameter(Self, DSProps_BindDoubleDateTimeValues, 'false'));
  FUndefinedVarcharAsStringLength := StrToIntDef(DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'), 0);
  fBindOrdinalBoolValues := StrToBoolEx(DefineStatementParameter(Self, DSProps_BindOrdinalBoolValues, 'false'));
  FSQLiteIntAffinity := StrToBoolEx(DefineStatementParameter(Self, DSProps_SQLiteIntAffinity, 'false'));
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
    RestartTimer;
    FErrorCode := FPlainDriver.sqlite3_prepare_v2(FHandle, Pointer(ASQL), Length(ASQL), FStmtHandle, pzTail{%H-});
    if FErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcPrepStmt, FErrorCode, SQL,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    inherited Prepare;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcPrepStmt,Self);
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
      FSQLiteConnection.HandleErrorOrWarning(lcUnprepStmt, ErrorCode, 'sqlite3_finalize',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
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
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

procedure TZAbstractSQLiteCAPIPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
var ErrorCode: Integer;
  P: PAnsiChar;
begin
  inherited BindRawStr(Index, Value); //localize -> no val destructor
  if not FBindLater then begin
    P := Pointer(Value);
    if P = nil then
    P := PEmptyAnsiString;
    ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, P, Length(Value), nil);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
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
  LastUpdateCount := -1;
  PrepareOpenResultSetForReUse;
  Prepare;
  BindInParameters;
  RestartTimer;
  FBindLater := False;
  FErrorCode := FPlainDriver.sqlite3_step(FStmtHandle); //exec prepared
  if not (FErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
    FSQLiteConnection.HandleErrorOrWarning(lcExecPrepStmt, FErrorCode, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
  if FHasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
  if (FErrorCode <> SQLITE_ROW) and (FPlainDriver.sqlite3_column_count(FStmtHandle) = 0) then begin
    LastUpdateCount := FPlainDriver.sqlite3_Changes(FHandle);
    FErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    if FErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcOther, FErrorCode, '',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    Result := nil;
  end else if Assigned(FOpenResultSet) //expect a resultset
    then Result := IZResultSet(FOpenResultSet) //return allready reseted RS
    else Result := CreateResultSet; //resultset executes reset stmt-handle
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
  RestartTimer;
  LastUpdateCount := -1;
  FBindLater := False;
  ErrorCode := fPlainDriver.sqlite3_step(FStmtHandle);
  if (ErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then begin
    if ErrorCode <> SQLITE_ROW then
      LastUpdateCount := FPlainDriver.sqlite3_Changes(FHandle);
    if FHasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt,Self);
    ErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcOther, ErrorCode, SQL,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    FHasLoggingListener := DriverManager.HasLoggingListener;
  end else try
    FSQLiteConnection.HandleErrorOrWarning(lcExecPrepStmt, ErrorCode, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    FPlainDriver.sqlite3_reset(FStmtHandle); //reset handle allways without check else -> leaking mem
    FHasLoggingListener := DriverManager.HasLoggingListener;
  end;
  Result := LastUpdateCount;
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
  RestartTimer;
  FBindLater := False;
  FErrorCode := FPlainDriver.sqlite3_step(FStmtHandle);
  if not (FErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
    FSQLiteConnection.HandleErrorOrWarning(lcExecPrepStmt, FErrorCode, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
  { Process queries with result sets }
  if (FErrorCode = SQLITE_ROW) or (FPlainDriver.sqlite3_column_count(FStmtHandle) <> 0) then begin
    LastUpdateCount := -1;
    Result := True;
    LastResultSet := CreateResultSet;
  end else begin { Processes regular query. }
    Result := False;
    LastUpdateCount := FPlainDriver.sqlite3_Changes(FHandle);
    FErrorCode := FPlainDriver.sqlite3_reset(FStmtHandle);
    LastResultSet := nil;
    if FErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcOther, FErrorCode, 'Reset',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
  FHasLoggingListener := DriverManager.HasLoggingListener;
  if FHasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{ TZSQLiteStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a sqlite database connection object.
  @param Info a statement parameters.
}
constructor TZSQLiteStatement.Create(const Connection: IZSQLiteConnection;
  const Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZSQLiteCAPIPreparedStatement }

procedure TZSQLiteCAPIPreparedStatement.SetBigDecimal(ParameterIndex: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var ErrorCode, L: Integer;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener then
    BindList.Put(ParameterIndex, Value);
  if not FBindLater then begin
    P := BindList.AcquireCustomValue(ParameterIndex, stBigDecimal, MaxFmtBCDFractionSize+3{#0});
    L := BCDToRaw(Value, P, '.');
    ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, ParameterIndex+1, P, l, nil);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

procedure TZSQLiteCAPIPreparedStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  if fBindOrdinalBoolValues
  then SetLong(ParameterIndex, Ord(Value))
  else BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, DeprecatedBoolRaw[Value]);
end;

procedure TZSQLiteCAPIPreparedStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  SetInt(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZSQLiteCAPIPreparedStatement.SetBytes(Index: Integer; Value: PByte;
  Len: NativeUInt);
var ErrorCode: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  if FBindLater or FHasLoggingListener then
    BindList.Put(Index, stBytes, Value, Len);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, Value, Len, nil);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_blob',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
var ErrorCode: Integer;
  i64: Int64 absolute Value;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener then
    BindList.Put(ParameterIndex, stCurrency, P8Bytes(@Value));
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, ParameterIndex+1, i64);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_int64',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZSQLiteCAPIPreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var
  ErrorCode: Integer;
  DT: TDateTime;
  Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  if FBindDoubleDateTimeValues then begin
    ZSysUtils.TryDateToDateTime(Value, DT);
    if FBindLater or FHasLoggingListener
      then BindList.Put(Index, Value);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, Index+1, DT-JulianEpoch);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_double',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else begin
    Len := DateToRaw(Value.Year, Value.Month, Value.Day, PAnsiChar(FByteBuffer),
      ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
    Bindlist.Put(Index, stString, fRawTemp, zCP_UTF8);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Pointer(fRawTemp), Len, nil);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
var ErrorCode: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener then
    BindList.Put(ParameterIndex, stDouble, P8Bytes(@Value));
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, ParameterIndex+1, Value);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_double',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  SetDouble(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetInt(ParameterIndex, Value: Integer);
var ErrorCode: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener then
    BindList.Put(ParameterIndex, stInteger, P4Bytes(@Value));
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_int(FStmtHandle, ParameterIndex+1, Value);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_int',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
var ErrorCode: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener then
    BindList.Put(ParameterIndex, stLong, P8Bytes(@Value));
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, ParameterIndex+1, Value);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_int64',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

procedure TZSQLiteCAPIPreparedStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
var ErrorCode: Integer;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FBindLater or FHasLoggingListener
  then BindList.SetNull(ParameterIndex, SQLType);
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_null(FStmtHandle, ParameterIndex +1);
    if ErrorCode <> SQLITE_OK then
      FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_null',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end else
    FLateBound := True;
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  SetInt(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  SetInt(ParameterIndex, Value);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZSQLiteCAPIPreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var
  ErrorCode: Integer;
  DT: TDateTime;
  Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  if FBindDoubleDateTimeValues then begin
    ZSysUtils.TryTimeToDateTime(Value, DT);
    if FBindLater or FHasLoggingListener
      then BindList.Put(Index, Value);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, Index+1, DT-JulianEpoch);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_double',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else begin
    Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, False, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
    Bindlist.Put(Index, stString, fRawTemp, zCP_UTF8);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Pointer(fRawTemp), Len, nil);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZSQLiteCAPIPreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var
  ErrorCode: Integer;
  DT: TDateTime;
  Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  if FBindDoubleDateTimeValues then begin
    ZSysUtils.TryTimeStampToDateTime(Value, DT);
    if FBindLater or FHasLoggingListener
      then BindList.Put(Index, Value);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, Index+1, DT-JulianEpoch);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_double',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else begin
    Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
      Value.Hour, Value.Minute, Value.Second, Value.Fractions,
        PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat, False, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
    Bindlist.Put(Index, stString, fRawTemp, zCP_UTF8);
    if not FBindLater then begin
      ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Pointer(fRawTemp), Len, nil);
      if ErrorCode <> SQLITE_OK then
        FSQLiteConnection.HandleErrorOrWarning(lcBindPrepStmt, ErrorCode, 'sqlite3_bind_text',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLiteCAPIPreparedStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  SetLong(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZSQLiteCAPIPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  SetLong(ParameterIndex, Value);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZSQLiteCAPIPreparedStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  SetInt(ParameterIndex, Value);
end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.

