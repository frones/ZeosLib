{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcSQLAnywhereStatement;

{$I ZDbc.inc}

interface

{$IFNDEF ZEOS_DISABLE_SQLANY}
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZDbcIntfs, ZDbcStatement, ZCompatibility, ZDbcLogging, ZVariant, ZClasses,
  ZDbcSQLAnywhere, ZPlainSQLAnywhere, ZCollections;

type
  {** Implements Prepared SQL Statement. }
  TZAbstractSQLAnywhereStatement = class(TZRawPreparedStatement)
  private
    Fa_sqlany_bind_paramArray: Pa_sqlany_bind_paramArray;
    fMoreResultsIndicator: TZMoreResultsIndicator;
    FSQLAnyConnection: IZSQLAnywhereConnection;
    FPlainDriver: TZSQLAnywherePlainDriver;
    FHasOutParams: Boolean;
    Fa_sqlany_stmt: Pa_sqlany_stmt;
    FCallResultCache: TZCollection;
    Fapi_version: Tsacapi_u32;
    FParamsDescribed: Boolean; //SQLAynwhere just describes SP's by now those params we do not change, they might be bidirectional
    procedure ClearCallResultCache;
  private
    function CreateResultSet: IZResultSet;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);

    procedure Prepare; override;
    procedure Unprepare; override;

    function GetMoreResults: Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZSQLAnywhereStatement = Class(TZAbstractSQLAnywhereStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  End;

  TZSQLAnywherePreparedStatement = class(TZAbstractSQLAnywhereStatement, IZPreparedStatement)
  private
    FIsNullArray: Psacapi_i32Array;
    FLengthArray: Psize_tArray;
    FBindParamSize: NativeInt;
    FBindAgain: Boolean;
  protected
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    function InitDataValue(Index: Integer; SQLType: TZSQLType; Length: Tsize_t): Pa_sqlany_data_value;
  protected
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure BindInParameters; override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(var Value: Integer); override;
  public
    procedure AfterConstruction; override;
  public
    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(Index: Integer; Value: Word);
    procedure SetSmall(Index: Integer; Value: SmallInt);
    procedure SetUInt(Index: Integer; Value: Cardinal);
    procedure SetInt(Index: Integer; Value: Integer);
    procedure SetULong(Index: Integer; const Value: UInt64);
    procedure SetLong(Index: Integer; const Value: Int64);
    procedure SetFloat(Index: Integer; Value: Single);
    procedure SetDouble(Index: Integer; const Value: Double);
    procedure SetCurrency(Index: Integer; const Value: Currency);
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce; overload;
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGuid(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID); reintroduce;
    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); reintroduce; overload;
  end;

  TZSQLAnywhereCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    /// <summary>creates an exceution Statement. Which wraps the call.</summary>
    /// <param>"StoredProcName" the name of the stored procedure or function to
    ///  be called.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_SQLANY}
implementation
{$IFNDEF ZEOS_DISABLE_SQLANY}

uses ZSysUtils, ZDbcUtils, ZMessages, ZDbcSQLAnywhereResultSet,
  ZDbcGenericResolver, ZEncoding, ZFastCode;

{ TZAbstractSQLAnywhereStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the query
  @param Info a statement parameters.
}
procedure TZAbstractSQLAnywhereStatement.ClearCallResultCache;
var I: Integer;
  RS: IZResultSet;
begin
  for I := 0 to FCallResultCache.Count -1 do
    if Supports(FCallResultCache[i], IZResultSet, RS) then
      RS.Close;
  FreeAndNil(FCallResultCache);
end;

constructor TZAbstractSQLAnywhereStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FSQLAnyConnection := Connection as IZSQLAnywhereConnection;
  FPlainDriver := FSQLAnyConnection.GetPlainDriver;
  ResultSetType := rtScrollInsensitive;
  Fapi_version := FSQLAnyConnection.Get_api_version;
end;

{**
  Creates as resultset.
  @return the resultset.
}
function TZAbstractSQLAnywhereStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZSQLAnywhereResultSet;
  CachedResultSet: TZSQLAnywhereCachedResultSet;
begin
  if (fMoreResultsIndicator = mriHasNoMoreResults) and (FOpenResultSet <> nil) then
    Result := IZResultSet(FOpenResultSet)
  else begin
    NativeResultSet := TZSQLAnywhereResultSet.Create(Self, SQL, @Fa_sqlany_stmt);
    if ResultSetConcurrency = rcUpdatable then begin
      CachedResultSet := TZSQLAnywhereCachedResultSet.Create(NativeResultSet, SQL, nil, ConSettings);
      CachedResultSet.SetResolver(TZGenerateSQLCachedResolver.Create(Self, NativeResultSet.GetMetadata));
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
  end;
end;

{**
  prepares the statement on the server
}
procedure TZAbstractSQLAnywhereStatement.Prepare;
begin
  if FCallResultCache <> nil then
    ClearCallResultCache;
  if not Prepared then begin
    Fa_sqlany_stmt := FplainDriver.sqlany_prepare(FSQLAnyConnection.Get_a_sqlany_connection,
      Pointer(fASQL));
    if Fa_sqlany_stmt = nil then
      FSQLAnyConnection.HandleErrorOrWarning(lcPrepStmt, SQL, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcPrepStmt,Self);
    inherited Prepare;
  end else if fMoreResultsIndicator <> mriHasNoMoreResults then
    while GetMoreResults do ;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractSQLAnywhereStatement.Unprepare;
begin
  if FCallResultCache <> nil then
    ClearCallResultCache;
  if fMoreResultsIndicator <> mriHasNoMoreResults then
    while GetMoreResults do ;
  fMoreResultsIndicator := mriUnknown;
  inherited Unprepare;
  if Fa_sqlany_stmt <> nil then begin
    FPLainDriver.sqlany_free_stmt(Fa_sqlany_stmt);
    Fa_sqlany_stmt := nil;
  end;
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
function TZAbstractSQLAnywhereStatement.GetMoreResults: Boolean;
var num_cols: Tsacapi_i32;
  procedure FromCache;
  var AnyValue: IZAnyValue;
  begin
    Result := FCallResultCache.Count > 0;
    if Result then begin
      if Supports(FCallResultCache[0], IZResultSet, FlastResultSet)
      then LastUpdateCount := -1
      else begin
        FCallResultCache[0].QueryInterface(IZAnyValue, AnyValue);
        LastUpdateCount := AnyValue.GetInteger;
      end;
      FCallResultCache.Delete(0);
    end;
  end;
begin
  if (FOpenResultSet <> nil)
  then IZResultSet(FOpenResultSet).Close;
  if FCallResultCache <> nil
  then FromCache
  else begin
    Result := FPlainDriver.sqlany_get_next_result(Fa_sqlany_stmt) = 1;
    if Result then begin
      num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
      if num_cols < 0
      then FSQLAnyConnection.HandleErrorOrWarning(lcExecPrepStmt, SQL, Self)
      else if num_cols > 0
        then LastResultSet := CreateResultSet
        else LastUpdateCount := FplainDriver.sqlany_affected_rows(Fa_sqlany_stmt);
    end;
    if fMoreResultsIndicator = mriUnknown then
       fMoreResultsIndicator := TZMoreResultsIndicator(Ord(mriHasNoMoreResults)+Ord(Result));
  end;
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

  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractSQLAnywhereStatement.ExecutePrepared: Boolean;
var num_cols: Tsacapi_i32;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  if FWeakIZPreparedStatementPtr <> nil then
    BindInParameters;
  RestartTimer;
  if FHasOutParams and (FOutParamResultSet = nil) then
    FOutParamResultSet := TZSQLAynwhereOutParamResultSet.Create(Self, SQL, @Fa_sqlany_stmt,
      Fa_sqlany_bind_paramArray, BindList);
  if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
    FSQLAnyConnection.HandleErrorOrWarning(lcExecPrepStmt, SQL, Self);
  num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if num_cols < 0
  then FSQLAnyConnection.HandleErrorOrWarning(lcExecPrepStmt, SQL, Self)
  else if num_cols > 0 then begin
    LastUpdatecount := -1;
    LastResultSet := CreateResultSet
  end else begin
    LastUpdateCount := FplainDriver.sqlany_affected_rows(Fa_sqlany_stmt);
    if FHasOutParams then
      FLastResultSet := FOutParamResultSet;
  end;
  Result := Assigned(FLastResultSet);
  { Logging SQL Command }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractSQLAnywhereStatement.ExecuteQueryPrepared: IZResultSet;
var num_cols: Tsacapi_i32;
begin
  LastUpdateCount := -1;
  PrepareOpenResultSetForReUse;
  Prepare;
  if FWeakIZPreparedStatementPtr <> nil then
    BindInParameters;
  RestartTimer;
  if FHasOutParams and (FOutParamResultSet = nil) then
    FOutParamResultSet := TZSQLAynwhereOutParamResultSet.Create(Self, SQL, @Fa_sqlany_stmt,
      Fa_sqlany_bind_paramArray, BindList);
  if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
    FSQLAnyConnection.HandleErrorOrWarning(lcExecPrepStmt, SQL, Self);
  Result := nil;
  num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if num_cols > 0
  then Result := CreateResultSet
  else if FHasOutParams then begin
    Result := FOutParamResultSet;
    FOpenResultSet := Pointer(Result);
  end else begin
    while GetMoreResults and (FLastResultSet = nil) do ;
    Result := GetResultSet;
  end;
  { Logging SQL Command and values}
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
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
function TZAbstractSQLAnywhereStatement.ExecuteUpdatePrepared: Integer;
var num_cols: Tsacapi_i32;
begin
  LastUpdateCount := -1;
  Prepare;
  if FWeakIZPreparedStatementPtr <> nil then
    BindInParameters;
  RestartTimer;
  if FHasOutParams and (FOutParamResultSet = nil) then
    FOutParamResultSet := TZSQLAynwhereOutParamResultSet.Create(Self, SQL, @Fa_sqlany_stmt,
      Fa_sqlany_bind_paramArray, BindList);
  if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
    FSQLAnyConnection.HandleErrorOrWarning(lcExecPrepStmt, SQL, Self);
  num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if num_cols = 0
  then LastUpdateCount := FplainDriver.sqlany_affected_rows(Fa_sqlany_stmt)
  else while GetMoreResults do
    if FLastResultSet = nil then begin
      LastUpdateCount := FplainDriver.sqlany_affected_rows(Fa_sqlany_stmt);
      Break;
    end;
  { Logging SQL Command and values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
  Result := LastUpdateCount;;
end;

{ TZSQLAnywherePreparedStatement }

procedure TZSQLAnywherePreparedStatement.AfterConstruction;
begin
  inherited;
  if Fapi_version >= SQLANY_API_VERSION_4
  then FBindParamSize := SizeOf(Ta_sqlany_bind_paramV4up)
  else FBindParamSize := SizeOf(Ta_sqlany_bind_param)
end;

procedure TZSQLAnywherePreparedStatement.BindInParameters;
var I: Integer;
  J: Tsacapi_u32;
begin
  if FBindAgain then begin
    J := 0;
    for i := 0 to BindList.Count -1 do
      if BindList[i].ParamType <> pctResultSet then begin
        if FPlainDriver.sqlany_bind_param(Fa_sqlany_stmt, J,
           Pointer(PAnsiChar(Fa_sqlany_bind_paramArray) + (FBindParamSize * i))) <> 1 then
          FSQLAnyConnection.HandleErrorOrWarning(lcExecute, 'sqlany_bind_param', Self);
        Inc(J);
      end;
    FBindAgain := False;
  end;
  inherited BindInParameters;
end;

procedure TZSQLAnywherePreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var data_value: Pa_sqlany_data_value;
  Stream: TStream;
begin
  inherited BindLob(Index, SQLType, Value); //else FPC raises tons of memleaks
  data_value := InitDataValue(Index, SQLType, 0);
  if (Value = nil) or Value.IsEmpty
  then data_value.is_null^ := 1
  else begin
    data_value.is_null^ := 0;
    Stream := Value.GetStream;
    try
      data_value.length^ := Stream.Size;
      data_value.buffer_size := ((data_value.length^ shr 3)+1) shl 3;
      ReallocMem(data_value.buffer, data_value.buffer_size);
      Stream.Read(data_value.buffer^, data_value.length^);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TZSQLAnywherePreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  if Pointer(Value) <> nil
  then BindRawStr(Index, Pointer(Value), Length(Value))
  else BindRawStr(Index, PEmptyAnsiString, 0);
end;

procedure TZSQLAnywherePreparedStatement.BindRawStr(Index: Integer; Buf: PAnsiChar;
  Len: LengthInt);
var data_value: Pa_sqlany_data_value;
begin
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stString, Len);
  data_value.is_null^ := 0;
  data_value.length^ := Len;
  if Len > 0 then
    Move(Buf^, data_value.buffer^, Len);
end;

procedure TZSQLAnywherePreparedStatement.CheckParameterIndex(
  var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then begin
    {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := Format(SBindVarOutOfRange, [Value]);
    raise EZSQLException.Create({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
  end;
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

function TZSQLAnywherePreparedStatement.InitDataValue(Index: Integer;
  SQLType: TZSQLType; Length: Tsize_t): Pa_sqlany_data_value;
label jmpVarLen;
var ActSize: Tsize_t;
    ActType: Ta_sqlany_data_type;
    Bind: Pa_sqlany_bind_param;
begin
  Bind := Pointer(PAnsiChar(Fa_sqlany_bind_paramArray) + (FBindParamSize * Index));
  Result := @Bind.value;
  ActSize := Result.buffer_size;
  ActType := Result._type;
  case SQLType of
    stBoolean, stByte: begin
       Result.buffer_size := SizeOf(Byte);
       Result._type := A_UVAL8;
      end;
    stShort: begin
       Result.buffer_size := SizeOf(ShortInt);
       Result._type := A_VAL8;
      end;
    stWord: begin
       Result.buffer_size := SizeOf(Word);
       Result._type := A_UVAL16;
      end;
    stSmall: begin
       Result.buffer_size := SizeOf(Word);
       Result._type := A_VAL16;
      end;
    stLongWord: begin
       Result.buffer_size := SizeOf(Cardinal);
       Result._type := A_UVAL32;
      end;
    stInteger: begin
       Result.buffer_size := SizeOf(Integer);
       Result._type := A_VAL32;
      end;
    stULong: begin
       Result.buffer_size := SizeOf(Uint64);
       Result._type := A_UVAL64;
      end;
    stLong: begin
       Result.buffer_size := SizeOf(Int64);
       Result._type := A_VAL64;
      end;
    stFloat, stDouble: begin
       Result.buffer_size := SizeOf(Double);
       Result._type := A_DOUBLE;
      end;
    stCurrency,
    stBigDecimal: begin
       Result.buffer_size := 44;
       Result._type := A_STRING;
      end;
    stDate: begin
       Result.buffer_size := 16;
       Result._type := A_STRING;
      end;
    stTime: begin
       Result.buffer_size := 24;
       Result._type := A_STRING;
      end;
    stTimestamp: begin
       Result.buffer_size := 32;
       Result._type := A_STRING;
      end;
    stGUID: begin
       Result.buffer_size := 44;
       Result._type := A_STRING;
      end;
    stString, stUnicodeString: begin
        Result._type := A_STRING;
        goto jmpVarLen;
      end;
    stBytes: begin
        Result._type := A_BINARY;
jmpVarLen:
        if length = 0 then
          length := 8
        else if Result.buffer_size < length then
          length := ((length shr 3)+1) shl 3; //8 Byte aligned incl. space for zero term
        Result.buffer_size := length;
      end;
    stAsciiStream,
    stUnicodeStream: begin
       Result.buffer_size := 0;
       Result._type := A_STRING;
      end;
    stBinaryStream: begin
       Result.buffer_size := 0;
       Result._type := A_BINARY;
      end;
    else raise EZSQLException.Create(SUnsupportedParameterType);
  end;
  if (ActSize <> Result.buffer_size) then begin
    FBindAgain := True;
    ReallocMem(Result.buffer, Result.buffer_size);
  end;
  FBindAgain := FBindAgain or (ActType <> Result._type);
end;

procedure TZSQLAnywherePreparedStatement.PrepareInParameters;
var num_params, I: Tsacapi_i32;
  Bind: Pa_sqlany_bind_param;
begin
  FBindAgain := True;
  inherited PrepareInParameters;
  num_params := FPlainDriver.sqlany_num_params(Fa_sqlany_stmt);
  if num_params = -1 then
    FSQLAnyConnection.HandleErrorOrWarning(lcBindPrepStmt, SQL, Self);
  SetBindCapacity(num_params);
  for i := 0 to num_params -1 do begin
    {$R-}
    Bind := Pointer(PAnsiChar(Fa_sqlany_bind_paramArray) + (FBindParamSize * I));
    if FPlainDriver.sqlany_describe_bind_param(Fa_sqlany_stmt, I, Bind) <> 1 then
      FSQLAnyConnection.HandleErrorOrWarning(lcBindPrepStmt, SQL, Self);
    FParamsDescribed := FParamsDescribed or (Bind.value._type <> A_INVALID_TYPE);
    FHasOutParams := FHasOutParams or (Ord(Bind.direction) >= Ord(DD_OUTPUT));
    Bind.value.length :=  @FLengthArray[i];
    Bind.value.is_null := @FIsNullArray[i];
    if Bind.value.buffer_size > 0 then
      GetMem(Bind.value.buffer, Bind.value.buffer_size);

    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  end;
end;

{**
  Sets the designated parameter to a <code>BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stBigDecimal, 0);
  data_value.is_null^ := 0;
  data_value.length^ := BcdToRaw(Value, data_value.buffer, '.');
end;

procedure TZSQLAnywherePreparedStatement.SetBindCapacity(Capacity: Integer);
var OldCapacity, I: Integer;
  Bind: Pa_sqlany_bind_param;
begin
  OldCapacity := Bindlist.Capacity;
{ that's crashing sqlany 12
  if (Fa_sqlany_stmt <> nil) and (BindList.Count > 0) then
    for i := OldCapacity -1 downto Capacity do
      FplainDriver.sqlany_bind_param(Fa_sqlany_stmt, Cardinal(I), nil);}

  inherited SetBindCapacity(Capacity);
  if OldCapacity <> Capacity then begin
    BindList.Count := Capacity;
    ReallocMem(FIsNullArray, Capacity * SizeOf(Tsacapi_i32));
    FillChar(FIsNullArray^, Capacity * SizeOf(Tsacapi_i32), #0);
    ReallocMem(FLengthArray, Capacity * SizeOf(Tsize_t));
    for i := OldCapacity-1 downto Capacity do begin
      Bind := Pointer(PAnsiChar(Fa_sqlany_bind_paramArray) + (FBindParamSize * i));
      if Bind^.value.buffer <> nil then
        FreeMem(Bind^.value.buffer);
    end;
    ReallocMem(Fa_sqlany_bind_paramArray, Capacity*FBindParamSize);
    if Capacity > 0 then
      FillChar((PAnsichar(Fa_sqlany_bind_paramArray)+OldCapacity*SizeOf(FBindParamSize))^,
        (Capacity-OldCapacity)*FBindParamSize, #0);
  end;
end;

{**
  Sets the designated parameter to a <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stBoolean, 0);
  data_value.is_null^ := 0;
  PByte(data_value.buffer)^ := Byte(Value);
end;

procedure TZSQLAnywherePreparedStatement.SetByte(Index: Integer; Value: Byte);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stByte, 0);
  data_value.is_null^ := 0;
  PByte(data_value.buffer)^ := Value;
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
procedure TZSQLAnywherePreparedStatement.SetBytes(Index: Integer; Value: PByte;
  Len: NativeUInt);
var data_value: Pa_sqlany_data_value;
begin
  if (Len = 0) or (Value = nil) then
    SetNull(Index, stBytes)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    data_value := InitDataValue(Index, stBytes, Len);
    data_value.is_null^ := 0;
    Move(Value^, data_value.buffer^, Len);
    data_value.length^ := Len;
  end;
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
var Len: LengthInt;
begin
  Len := Length(Value);
  if Len = 0
  then SetNull(Index, stBytes)
  else SetBytes(Index, Pointer(Value), Len);
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var data_value: Pa_sqlany_data_value;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stCurrency, 0);
  data_value.is_null^ := 0;
  CurrToRaw(Value, '.', data_value.buffer, @P);
  data_value.length^ := P - data_value.buffer;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stDate, 0);
  data_value.is_null^ := 0;
  data_value.length^ := ZSysUtils.DateToRaw(Value.Year, Value.Month, Value.Day,
    data_value.buffer, ConSettings.WriteFormatSettings.DateFormat, False, Value.IsNegative)
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stDouble, 0);
  data_value.is_null^ := 0;
  PDouble(data_value.buffer)^ := Value;
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetFloat(Index: Integer;
  Value: Single);
begin
  SetDouble(Index, Value);
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetGuid(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stGUID, 0);
  data_value.is_null^ := 0;
  ZSysUtils.GUIDToBuffer(@Value.D1, data_value.buffer, [guidWithBrackets]);
  data_value.length^ := 38;
end;

procedure TZSQLAnywherePreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZSQLStringWriter; var Result: SQLString);
var Bind: Pa_sqlany_bind_param;
begin
  CheckParameterIndex(ParamIndex);
  Bind := Pointer(PAnsiChar(Fa_sqlany_bind_paramArray) + (FBindParamSize * ParamIndex));
  if (Bind.value.is_null <> nil) and (Bind.value.is_null^ <> 0) then
    SQLWriter.AddText('(NULL)', Result)
  else case Bind.value._type of
    A_BINARY: if BindList[ParamIndex].SQLType = stBinaryStream
              then SQLWriter.AddText('(BLOB)', Result)
              else SQLWriter.AddHexBinary(PByte(Bind.value.buffer), Bind.value.length^, True, Result);
    A_STRING: case BindList[ParamIndex].SQLType of
                stAsciiStream: SQLWriter.AddText('(CLOB)', Result);
                stUnicodeStream: SQLWriter.AddText('(NCLOB)', Result);
                stCurrency, stBigDecimal:
                  {$IFDEF UNICODE}
                    SQLWriter.AddAscii7Text(Bind.value.buffer, Bind.value.length^, Result);
                  else begin
                    FUniTemp := PRawToUnicode(Bind.value.buffer, Bind.value.length^, FClientCP);
                    SQLWriter.AddTextQuoted(FUniTemp, #39, Result);
                    FUniTemp := '';
                  end;
                  {$ELSE}
                    SQLWriter.AddText(Bind.value.buffer, Bind.value.length^, Result);
                  else SQLWriter.AddTextQuoted(Bind.value.buffer, Bind.value.length^, AnsiChar(#39), Result);
                  {$ENDIF}
              end;
    A_DOUBLE: if Bind.value.buffer_size = SizeOf(Double)
              then SQLWriter.AddFloat(PDouble(Bind.value.buffer)^, Result)
              else SQLWriter.AddFloat(PSingle(Bind.value.buffer)^, Result);
    A_VAL64:  SQLWriter.AddOrd(PInt64(Bind.value.buffer)^, Result);
    A_UVAL64: SQLWriter.AddOrd(PUInt64(Bind.value.buffer)^, Result);
    A_VAL32:  SQLWriter.AddOrd(PInteger(Bind.value.buffer)^, Result);
    A_UVAL32: SQLWriter.AddOrd(PCardinal(Bind.value.buffer)^, Result);
    A_VAL16:  SQLWriter.AddOrd(PSmallInt(Bind.value.buffer)^, Result);
    A_UVAL16: SQLWriter.AddOrd(PWord(Bind.value.buffer)^, Result);
    A_VAL8:   SQLWriter.AddOrd(PShortInt(Bind.value.buffer)^, Result);
    A_UVAL8:  SQLWriter.AddOrd(PByte(Bind.value.buffer)^, Result);
    else SQLWriter.AddText('(UNKNOWN)', Result);
  end;
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetInt(Index, Value: Integer);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stInteger, 0);
  data_value.is_null^ := 0;
  PInteger(data_value.buffer)^ := Value;
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stLong, 0);
  data_value.is_null^ := 0;
  PInt64(data_value.buffer)^ := Value;
end;

procedure TZSQLAnywherePreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, SQLType, 0);
  data_value.is_null^ := 1;
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetShort(Index: Integer;
  Value: ShortInt);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stShort, 0);
  data_value.is_null^ := 0;
  PShortInt(data_value.buffer)^ := Value;
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetSmall(Index: Integer;
  Value: SmallInt);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stSmall, 0);
  data_value.is_null^ := 0;
  PSmallInt(data_value.buffer)^ := Value;
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stTime, 0);
  data_value.is_null^ := 0;
  data_value.length^ := ZSysUtils.TimeToRaw(Value.Hour, Value.Minute, Value.Second,
    Value.Fractions, data_value.buffer, ConSettings.WriteFormatSettings.TimeFormat,
    False, Value.IsNegative)
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stTimeStamp, 0);
  data_value.is_null^ := 0;
  data_value.length^ := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
    Value.Hour, Value.Minute, Value.Second, Value.Fractions, data_value.buffer,
    ConSettings.
    WriteFormatSettings.DateTimeFormat, False, Value.IsNegative)
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetUInt(Index: Integer;
  Value: Cardinal);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stLongWord, 0);
  data_value.is_null^ := 0;
  PCardinal(data_value.buffer)^ := Value;
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZSQLAnywherePreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stULong, 0);
  data_value.is_null^ := 0;
  PUInt64(data_value.buffer)^ := Value;
end;

procedure TZSQLAnywherePreparedStatement.SetWord(Index: Integer; Value: Word);
var data_value: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  data_value := InitDataValue(Index, stWord, 0);
  data_value.is_null^ := 0;
  PWord(data_value.buffer)^ := Value;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZSQLAnywherePreparedStatement.UnPrepareInParameters;
begin
  SetBindCapacity(0);
  FParamsDescribed := False;
  FBindAgain := True;
end;

{ TZSQLAnywhereStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZSQLAnywhereStatement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZSQLAnywhereCallableStatement }

function TZSQLAnywhereCallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
begin
  SQL := '';
  I := Length(StoredProcName);
  i := I + 6+BindList.Count shl 1;
  SQLWriter := TZSQLStringWriter.Create(I);
  SQLWriter.AddText('CALL ', SQL);
  SQLWriter.AddText(StoredProcName, SQL);
  if BindList.Count > 0 then
    SQLWriter.AddChar('(', SQL);
  for i := 0 to BindList.Count-1 do
    if BindList.ParamTypes[i] <> pctReturn then
      SQLWriter.AddText('?,', SQL);
  if BindList.Count > 0 then begin
    SQLWriter.CancelLastComma(SQL);
    SQLWriter.AddChar(')', SQL);
  end;
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := TZSQLAnywherePreparedStatement.Create(Connection , SQL, Info);
  TZSQLAnywherePreparedStatement(Result).Prepare;
end;

initialization
{$ENDIF ZEOS_DISABLE_SQLANY}
end.
