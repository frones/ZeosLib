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
  ZVariant, Types;

type
  {** Implements CAPI Prepared SQL Statement. }
  TZSQLiteCAPIPreparedStatement = class(TImplizitBindRealAndEmulationStatement_A)
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
    FByRefBound: TPointerDynArray;
    FByRefLength: TIntegerDynArray;
    function CreateResultSet: IZResultSet;
  protected
    procedure ResetCallBack;
  protected
    procedure InternalSetInParamCount(NewParamCount: Integer); override;
    function GetLastErrorCodeAndHandle(var StmtHandle: Psqlite3_stmt): Integer;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  protected
    procedure BindNull(Index: Integer; var SQLType: TZSQLType); override;
    procedure BindBinary(Index: Integer; var SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindUnsignedOrdinal(Index: Integer; var SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindSignedOrdinal(Index: Integer; var SQLType: TZSQLType; const Value: Int64); override;
    procedure BindDouble(Index: Integer; var SQLType: TZSQLType; const Value: Double); override;
    procedure BindDateTime(Index: Integer; var SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindRawStr(Index: Integer; var SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; var SQLType: TZSQLType; const Buf: RawByteString); override;

    function GetBoundValueAsLogValue(Index: Integer): RawByteString; override;
    function BoolAsString(Value: Boolean): RawByteString; override;
  public
    constructor Create(const Connection: IZConnection;
      const SQL: string; const Info: TStrings; const Handle: Psqlite); overload;
    constructor Create(const Connection: IZConnection; const Info: TStrings;
      const Handle: Psqlite); overload;

    procedure Prepare; override;
    procedure Unprepare; override;
    //procedure Close; override;
    procedure Cancel; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZSQLiteStatement = class(TZSQLiteCAPIPreparedStatement);


implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS} AnsiStrings,{$ENDIF} ZDbcSqLiteUtils,
  ZDbcSqLiteResultSet, ZSysUtils, ZEncoding, ZMessages, ZDbcCachedResultSet,
  ZDbcUtils, ZDbcProperties, ZFastCode;

var
  SQLitePreparableTokens: TPreparablePrefixTokens;
(* out of use now...
procedure BindingDestructor(Value: PAnsiChar); cdecl;
begin
  {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(Value);
end;*)

{ TZSQLiteCAPIPreparedStatement }

function TZSQLiteCAPIPreparedStatement.GetBoundValueAsLogValue(
  Index: Integer): RawByteString;
var P: Pointer;
begin
  P := FByRefBound[Index];
  if P = nil then
    Result := 'null'
  else case FInParamTypes[Index] of
    stLong:     Result := IntToRaw(PInt64(P)^);
    stString:   Result := SQLQuotedStr(PAnsichar(P), FByRefLength[Index], #39);
    stDouble:   Result := FloatToRaw(PDouble(P)^);
    stBytes:    Result := GetSQLHexAnsiString(P, FByRefLength[Index], False);
  end;
end;

function TZSQLiteCAPIPreparedStatement.GetLastErrorCodeAndHandle(
  var StmtHandle: Psqlite3_stmt): Integer;
begin
  Result := FErrorCode;
  StmtHandle := FStmtHandle;
end;

procedure TZSQLiteCAPIPreparedStatement.InternalSetInParamCount(
  NewParamCount: Integer);
begin
  inherited InternalSetInParamCount(NewParamCount);
  SetLength(FByRefBound, NewParamCount);
  SetLength(FByRefLength, NewParamCount);
  FBindLater := False;
end;

function TZSQLiteCAPIPreparedStatement.CreateResultSet: IZResultSet;
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

procedure TZSQLiteCAPIPreparedStatement.PrepareInParameters;
begin
  InternalSetInParamCount(FPlainDriver.sqlite3_bind_parameter_count(FStmtHandle));
end;

procedure TZSQLiteCAPIPreparedStatement.ResetCallBack;
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

const
  BoolArray: array[Boolean] of PAnsiChar = ('N', 'Y');

procedure TZSQLiteCAPIPreparedStatement.BindBinary(Index: Integer;
  var SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var ErrorCode: Integer;
begin
  if SQLType in [stGUID, stBytes] then begin
    ZSetString(Buf, Len, FParamEmulatedValues[Index]);
    Buf := Pointer(FParamEmulatedValues[Index]);
  end;
  SQLType := stBytes;
  if not FBindLater then begin
    {we don't need to bind again if pointers are unchanged changed!}
    //if (Buf <> FByRefBound[Index]) or (Len <> FByRefLength[Index]) then begin
      ErrorCode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, Index +1, Buf, Len, nil);
      if ErrorCode <> SQLITE_OK then
        CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcBindPrepStmt, ASQL, ConSettings);
    //end;
  end else
    FLateBound := True;
  FByRefBound[Index] := Buf;
  FByRefLength[Index] := Len;
end;

procedure TZSQLiteCAPIPreparedStatement.BindDateTime(Index: Integer;
  var SQLType: TZSQLType; const Value: TDateTime);
begin
  if FBindDoubleDateTimeValues then
    BindDouble(Index, SQLType, Value-JulianEpoch)
  else begin
    if SQLType = stDate then
      if Length(FParamEmulatedValues[Index]) <> ConSettings^.WriteFormatSettings.DateFormatLen
      then FParamEmulatedValues[Index] := DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False)
      else DateTimeToRawSQLDate(Value, Pointer(FParamEmulatedValues[Index]), ConSettings^.WriteFormatSettings, False)
    else if SQLType = stTime then
      if Length(FParamEmulatedValues[Index]) <> ConSettings^.WriteFormatSettings.DateFormatLen
      then FParamEmulatedValues[Index] := DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False)
      else DateTimeToRawSQLTime(Value, Pointer(FParamEmulatedValues[Index]), ConSettings^.WriteFormatSettings, False)
    else
      if Length(FParamEmulatedValues[Index]) <> ConSettings^.WriteFormatSettings.DateTimeFormatLen
      then FParamEmulatedValues[Index] := DateTimeToRawSQLTimestamp(Value, ConSettings^.WriteFormatSettings, False)
      else DateTimeToRawSQLTimestamp(Value, Pointer(FParamEmulatedValues[Index]), ConSettings^.WriteFormatSettings, False);
    BindRawStr(Index, SQLType, Pointer(FParamEmulatedValues[Index]), Length(FParamEmulatedValues[Index]));
  end;
end;

procedure TZSQLiteCAPIPreparedStatement.BindDouble(Index: Integer;
  var SQLType: TZSQLType; const Value: Double);
var ErrorCode: Integer;
begin
  SQLType := stDouble;
  if FBindLater or FHasLoggingListener then begin
    ZSetString(@Value, SizeOf(Double), FParamEmulatedValues[Index]);
    FByRefBound[Index] := Pointer(FParamEmulatedValues[Index]);
  end;
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_double(FStmtHandle, Index +1, Value);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZSQLiteCAPIPreparedStatement.BindInParameters;
var
  I, Errorcode: Integer;
  P: Pointer;
begin
  if InParamCount > 0 then begin
    if FBindLater then begin
      for i := 0 to InParamCount-1 do begin
        P := FByRefBound[I];
        if P = nil then
          Errorcode := FPlainDriver.sqlite3_bind_null(FStmtHandle, I +1)
        else case FInParamTypes[I] of
          stString:   Errorcode := FPlainDriver.sqlite3_bind_text(FStmtHandle, I +1, P, FByRefLength[I], nil);
          stLong:     Errorcode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, I +1, PInt64(P)^);
          stDouble:   Errorcode := FPlainDriver.sqlite3_bind_Double(FStmtHandle, I +1, PDouble(P)^);
          stBytes:    Errorcode := FPlainDriver.sqlite3_bind_blob(FStmtHandle, I +1, P, FByRefLength[I], nil);
          else begin
            ErrorCode := SQLITE_ERROR; //satisfy comiler
            RaiseUnsupportedParameterTypeException(InParamTypes[I]);
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
end;

constructor TZSQLiteCAPIPreparedStatement.Create(
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
  FEmulatePrepare := False;
end;

constructor TZSQLiteCAPIPreparedStatement.Create(
  const Connection: IZConnection; const Info: TStrings; const Handle: Psqlite);
begin
  Create(Connection, '', Info, Handle);
end;

procedure TZSQLiteCAPIPreparedStatement.Prepare;
var pzTail: PAnsiChar;
begin
  if not Prepared then begin
    FErrorCode := FPlainDriver.sqlite3_prepare_v2(FHandle, Pointer(ASQL), Length(ASQL), FStmtHandle, pzTail);
    if FErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcPrepStmt, ASQL, ConSettings);
    inherited Prepare;
  end;
end;

procedure TZSQLiteCAPIPreparedStatement.Unprepare;
var ErrorCode: Integer;
begin
  { EH: do not change this sequence!: first close possbile opened resultset}
  inherited UnPrepare;
  if Assigned(FStmtHandle) then begin
    ErrorCode := FPlainDriver.sqlite3_finalize(FStmtHandle);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode,
        lcUnprepStmt, 'sqlite3_finalize', ConSettings);
    FStmtHandle := nil; //Keep track we do not try to finalize the handle again on destroy or so
  end;
end;

procedure TZSQLiteCAPIPreparedStatement.BindNull(Index: Integer;
  var SQLType: TZSQLType);
var ErrorCode: Integer;
begin
  FByRefBound[Index] := nil;
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_null(FStmtHandle, Index +1);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZSQLiteCAPIPreparedStatement.BindRawStr(Index: Integer;
  var SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt);
var ErrorCode: Integer;
begin
  SQLType := stString;
  if not FBindLater then begin
    if Buf = nil then
      Buf := PEmptyAnsiString;
    {we don't need to bind again if pointers are unchanged!}
    //if (Buf <> FByRefBound[Index]) or (Len <> FByRefLength[Index]) then begin
      ErrorCode := FPlainDriver.sqlite3_bind_text(FStmtHandle, Index +1, Buf, Len, nil);
      if ErrorCode <> SQLITE_OK then
        CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
    //end;
  end else
    FLateBound := True;
  FByRefBound[Index] := Buf;
  FByRefLength[Index] := Len;
end;

procedure TZSQLiteCAPIPreparedStatement.BindRawStr(Index: Integer;
  var SQLType: TZSQLType; const Buf: RawByteString);
begin
  FParamEmulatedValues[Index] := Buf; //keep alive
  BindRawStr(Index, SQLType, Pointer(Buf), Length(Buf));
end;

procedure TZSQLiteCAPIPreparedStatement.BindSignedOrdinal(
  Index: Integer; var SQLType: TZSQLType; const Value: Int64);
var ErrorCode: Integer;
begin
  SQLType := stLong;
  if FBindLater or FHasLoggingListener then begin
    ZSetString(@Value, SizeOf(Int64), FParamEmulatedValues[Index]);
    FByRefBound[Index] := Pointer(FParamEmulatedValues[Index]);
  end;
  if not FBindLater then begin
    ErrorCode := FPlainDriver.sqlite3_bind_int64(FStmtHandle, Index +1, Value);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode, lcBindPrepStmt, ASQL, ConSettings);
  end else
    FLateBound := True;
end;

procedure TZSQLiteCAPIPreparedStatement.BindUnsignedOrdinal(
  Index: Integer; var SQLType: TZSQLType; const Value: UInt64);
begin
  BindSignedOrdinal(Index, SQLType, Int64(Value));
end;

const DeprecatedBoolRaw: array[Boolean] of AnsiString = ('N','Y');
const IntBoolRaw: array[Boolean] of AnsiString = ('0','1');
function TZSQLiteCAPIPreparedStatement.BoolAsString(
  Value: Boolean): RawByteString;
begin
  if fBindOrdinalBoolValues
  then Result := IntBoolRaw[Value]
  //EH: i know this is plain wrong:
  //http://www.sqlite.org/datatype3.html
  //but zeos binds this since i know zeos
  else Result := DeprecatedBoolRaw[Value];
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZSQLiteCAPIPreparedStatement.Cancel;
begin
  FPlainDriver.sqlite3_interrupt(FHandle);
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
(*procedure TZSQLiteCAPIPreparedStatement.Close;
var ErrorCode: Integer;
begin
  inherited Close; //first close LastResultSet before finalize. Otherwise -> Library routine called out of sequence.
  { we need this here too: TZTestDbcSQLiteCase.TestResultSet would raise an Error on Connection.Close if Stmt isn't freed!}
  if Assigned(FStmtHandle) then begin
    ErrorCode := FPlainDriver.sqlite3_finalize(FStmtHandle);
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode,
        lcUnprepStmt, 'sqlite3_finalize', ConSettings);
    FStmtHandle := nil; //Keep track we do not try to finalize the handle again on destroy or so
  end;
end; *)

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
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
function TZSQLiteCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
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
function TZSQLiteCAPIPreparedStatement.ExecutePrepared: Boolean;
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

initialization

SetLength(SQLitePreparableTokens, 5);
SQLitePreparableTokens[0].MatchingGroup := 'INSERT';
SQLitePreparableTokens[1].MatchingGroup := 'UPDATE';
SQLitePreparableTokens[2].MatchingGroup := 'DELETE';
SQLitePreparableTokens[3].MatchingGroup := 'SELECT';


end.

