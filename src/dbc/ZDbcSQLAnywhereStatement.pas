{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcSQLAnywhereStatement;

{$I ZDbc.inc}

interface

{$IFNDEF ZEOS_DISABLE_ASA}
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZDbcIntfs, ZDbcStatement, ZCompatibility, ZDbcLogging, ZVariant, ZClasses,
  ZDbcSQLAnywhere, ZPlainSQLAnywhere, ZCollections;

type
  {** Implements Prepared SQL Statement. }
  TZAbstractSQLAnywhereStatement = class(TZRawPreparedStatement)
  private
    FSQLAnyConnection: IZSQLAnywhereConnection;
    FPlainDriver: TZSQLAnywherePlainDriver;
    FMoreResults: Boolean;
    FHasOutParams: Boolean;
    Fa_sqlany_stmt: Pa_sqlany_stmt;
    FCallResultCache: TZCollection;
    procedure ClearCallResultCache;
  private
    function CreateResultSet: IZResultSet;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure AfterClose; override;
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
    Fa_sqlany_bind_paramArray: Pa_sqlany_bind_paramArray;
    FIsNullArray: Psacapi_i32Array;
    FLengthArray: Psize_tArray;
  protected
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
  protected
    procedure PrepareInParameters; override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZRawSQLStringWriter; Var Result: RawByteString); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(var Value: Integer); override;
  public
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; Value: Boolean);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    procedure SetWord(Index: Integer; Value: Word);
    procedure SetSmall(Index: Integer; Value: SmallInt);
    procedure SetUInt(Index: Integer; Value: Cardinal);
    procedure SetInt(Index: Integer; Value: Integer);
    procedure SetULong(Index: Integer; const Value: UInt64);
    procedure SetLong(Index: Integer; const Value: Int64);
    procedure SetFloat(Index: Integer; Value: Single);
    procedure SetDouble(Index: Integer; const Value: Double);
    procedure SetCurrency(Index: Integer; const Value: Currency);
    procedure SetBigDecimal(Index: Integer; const Value: TBCD);
    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce; overload;
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGuid(Index: Integer; const Value: TGUID); reintroduce;
    procedure SetDate(Index: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; const Value: TZTimeStamp); reintroduce; overload;
  end;

  TZSQLAnywhereCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses ZSysUtils, ZDbcUtils, ZMessages, ZDbcSQLAnywhereResultSet,
  ZDbcGenericResolver, ZEncoding, ZDbcProperties, ZFastCode;

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
  ResultSetType := rtScrollSensitive;
end;

destructor TZAbstractSQLAnywhereStatement.Destroy;
begin
  inherited Destroy;
  FSQLAnyConnection := nil;
end;

function TZAbstractSQLAnywhereStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZSQLAnywhereResultSet;
  CachedResultSet: TZSQLAnywhereCachedResultSet;
begin
  With FSQLAnyConnection do begin
    NativeResultSet := TZSQLAnywhereResultSet.Create(Self, SQL, Fa_sqlany_stmt);
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

procedure TZAbstractSQLAnywhereStatement.Prepare;
begin
  if FCallResultCache <> nil then
    ClearCallResultCache;
  if not Prepared then begin
    Fa_sqlany_stmt := FplainDriver.sqlany_prepare(FSQLAnyConnection.Get_a_sqlany_connection,
      Pointer(fASQL));
    if Fa_sqlany_stmt = nil then
      FSQLAnyConnection.HandleError(lcPrepStmt, fASQL, Self);
    inherited Prepare;
  end;
end;

procedure TZAbstractSQLAnywhereStatement.Unprepare;
begin
  if FCallResultCache <> nil then
    ClearCallResultCache;
  if Fa_sqlany_stmt <> nil then begin
    FPLainDriver.sqlany_free_stmt(Fa_sqlany_stmt);
    Fa_sqlany_stmt := nil;
  end;
  inherited Unprepare;
end;

procedure TZAbstractSQLAnywhereStatement.AfterClose;
begin
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
begin
  Result := FPlainDriver.sqlany_get_next_result(Fa_sqlany_stmt) = 1;
  if Result then begin
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
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;
  if FMoreResults or FHasOutParams
  then LastResultSet := ExecuteQueryPrepared
  else begin
    if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
      FSQLAnyConnection.HandleError(lcExecute, fASQL, Self);
    num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
    if num_cols < 0
    then FSQLAnyConnection.HandleError(lcExecute, fASQL, Self)
    else if num_cols > 0 then
      LastResultSet := CreateResultSet;
  end;
  Result := Assigned(FLastResultSet);
  { Logging SQL Command and values}
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
  PrepareOpenResultSetForReUse;
  Prepare;
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;
  if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
    FSQLAnyConnection.HandleError(lcExecute, fASQL, Self);
  Result := nil;
  num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if num_cols > 0
  then Result := CreateResultSet
  else begin
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
  Prepare;
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;
  if FplainDriver.sqlany_execute(Fa_sqlany_stmt) <> 1 then
    FSQLAnyConnection.HandleError(lcExecute, fASQL, Self);
  Result := -1;
  num_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if num_cols > 0 then
    while GetMoreResults and (FLastResultSet <> nil) do
      Result := FplainDriver.sqlany_affected_rows(Fa_sqlany_stmt);
  { Logging SQL Command and values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{ TZSQLAnywherePreparedStatement }

procedure TZSQLAnywherePreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
begin
  inherited BindLob(Index, SQLType, Value); //else FPC raises tons of memleaks
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
begin
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.CheckParameterIndex(
  var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then
    raise EZSQLException.Create(SInvalidInputParameterCount);
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZSQLAnywherePreparedStatement.PrepareInParameters;
var num_params, I: Tsacapi_i32;
  Bind: Pa_sqlany_bind_param;
begin
  inherited PrepareInParameters;
  num_params := FPlainDriver.sqlany_num_params(Fa_sqlany_stmt);
  if num_params = -1 then
    FSQLAnyConnection.HandleError(lcExecute, fASQL, Self);
  SetBindCapacity(num_params);
  for i := 0 to num_params -1 do begin
    {$R-}
    Bind := @Fa_sqlany_bind_paramArray[I];
    if FPlainDriver.sqlany_describe_bind_param(Fa_sqlany_stmt, I, Bind) <> 1 then
      FSQLAnyConnection.HandleError(lcExecute, fASQL, Self);
    ReallocMem(Bind.value.buffer, Bind.value.buffer_size);
    Bind.value.length :=  @FLengthArray[i];
    Bind.value.is_null := @FIsNullArray[i];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  end;
end;

procedure TZSQLAnywherePreparedStatement.SetBigDecimal(Index: Integer;
  const Value: TBCD);
begin
  SetRawByteString(Index, BCDToSQLRaw(Value));
end;

procedure TZSQLAnywherePreparedStatement.SetBindCapacity(Capacity: Integer);
var OldCapacity, I: Integer;
  Bind: Pa_sqlany_bind_param;
begin
  OldCapacity := Bindlist.Capacity;
  inherited SetBindCapacity(Capacity);
  if OldCapacity <> Capacity then begin
    BindList.SetCount(Capacity);
    ReallocMem(FIsNullArray, Capacity * SizeOf(Tsacapi_i32));
    ReallocMem(FLengthArray, Capacity * SizeOf(Tsize_t));
    for i := OldCapacity-1 downto Capacity do begin
      {$R-}
      Bind := @Fa_sqlany_bind_paramArray[I];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      if Bind^.value.buffer <> nil then
        FreeMem(Bind^.value.buffer);
    end;
    ReallocMem(Fa_sqlany_bind_paramArray, Capacity*SizeOf(Ta_sqlany_bind_param));
  end;
end;

procedure TZSQLAnywherePreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetByte(Index: Integer; Value: Byte);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
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
begin
  if (Len = 0) or (Value = nil) then
    SetNull(Index, stBytes)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
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
  if Len = 0 then
    SetNull(Index, stBytes)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
  end;
end;

procedure TZSQLAnywherePreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
begin
  SetRawByteString(Index, CurrToRaw(Value));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywherePreparedStatement.SetDate(Index: Integer;
  const Value: TZDate);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZSQLAnywherePreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetFloat(Index: Integer;
  Value: Single);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetGuid(Index: Integer;
  const Value: TGUID);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZRawSQLStringWriter; var Result: RawByteString);
begin
  CheckParameterIndex(ParamIndex);
  (*
  if (SQLVar.sqlInd <> nil) and (SQLVar.sqlInd^ = -1) then
    SQLWriter.AddText('(NULL)', Result)
  else case SQLVar.sqlType and $FFFE of
    (*
    DT_SMALLINT         : SQLWriter.AddOrd(PSmallInt(SQLVAR.sqlData)^, Result);
    DT_INT              : SQLWriter.AddOrd(PInteger(SQLVAR.sqlData)^, Result);
    //DT_DECIMAL          : ;
    DT_FLOAT            : SQLWriter.AddFloat(PSingle(SQLVAR.sqldata)^, Result);
    DT_DOUBLE           : SQLWriter.AddFloat(PDouble(SQLVAR.sqldata)^, Result);
    DT_VARCHAR          : SQLWriter.AddTextQuoted(PAnsiChar(@PZASASQLSTRING(SQLVAR.sqldata).data[0]), PZASASQLSTRING(SQLVAR.sqldata).length, AnsiChar(#39), Result);
    DT_LONGVARCHAR      : SQLWriter.AddText('(CLOB)', Result);
    DT_TIMESTAMP_STRUCT : case PSmallInt(PAnsiChar(SQLVAR.sqlData)+SizeOf(TZASASQLDateTime))^ of
                            DT_DATE: begin
                                DT := EncodeDate(PZASASQLDateTime(SQLVAR.sqlData).Year,
                                  PZASASQLDateTime(SQLVAR.sqlData).Month +1, PZASASQLDateTime(SQLVAR.sqlData).Day);
                                SQLWriter.AddDate(DT, ConSettings.WriteFormatSettings.DateFormat, Result);
                              end;
                            DT_TIME: begin
                                DT := EncodeTime(PZASASQLDateTime(SQLVAR.sqlData).Hour,
                                  PZASASQLDateTime(SQLVAR.sqlData).Minute, PZASASQLDateTime(SQLVAR.sqlData).Second, PZASASQLDateTime(SQLVAR.sqlData).MicroSecond div 1000);
                                SQLWriter.AddTime(DT, ConSettings.WriteFormatSettings.TimeFormat, Result);
                              end
                            else {DT_TIMESTAMP} begin
                              DT := EncodeDate(PZASASQLDateTime(SQLVAR.sqlData).Year,
                                PZASASQLDateTime(SQLVAR.sqlData).Month +1, PZASASQLDateTime(SQLVAR.sqlData).Day);
                              if DT < 0
                              then DT := DT-EncodeTime(PZASASQLDateTime(SQLVAR.sqlData).Hour,
                                PZASASQLDateTime(SQLVAR.sqlData).Minute, PZASASQLDateTime(SQLVAR.sqlData).Second, PZASASQLDateTime(SQLVAR.sqlData).MicroSecond div 1000)
                              else DT := DT+EncodeTime(PZASASQLDateTime(SQLVAR.sqlData).Hour,
                                PZASASQLDateTime(SQLVAR.sqlData).Minute, PZASASQLDateTime(SQLVAR.sqlData).Second, PZASASQLDateTime(SQLVAR.sqlData).MicroSecond div 1000);
                              SQLWriter.AddDateTime(DT, ConSettings.WriteFormatSettings.DateTimeFormat, Result);
                            end;
                          end;
    DT_BINARY           : SQLWriter.AddHexBinary(@PZASASQLSTRING(SQLVAR.sqldata).data[0], PZASASQLSTRING(SQLVAR.sqldata).length, True, Result);
    DT_LONGBINARY       : SQLWriter.AddText('(BLOB)', Result);
    DT_TINYINT          : SQLWriter.AddOrd(PByte(SQLVAR.sqlData)^, Result);
    DT_BIGINT           : SQLWriter.AddOrd(PInt64(SQLVAR.sqlData)^, Result);
    DT_UNSINT           : SQLWriter.AddOrd(PCardinal(SQLVAR.sqlData)^, Result);
    DT_UNSSMALLINT      : SQLWriter.AddOrd(PWord(SQLVAR.sqlData)^, Result);
    DT_UNSBIGINT        : SQLWriter.AddOrd(PUInt64(SQLVAR.sqlData)^, Result);
    DT_BIT              : If PByte(SQLVAR.sqlData)^ = 0
                          then SQLWriter.AddText('(FALSE)', Result)
                          else SQLWriter.AddText('(TRUE)', Result);
    DT_NVARCHAR         : Result := SQLQuotedStr(PAnsiChar(@PZASASQLSTRING(SQLVAR.sqldata).data[0]), PZASASQLSTRING(SQLVAR.sqldata).length, AnsiChar(#39));
    DT_LONGNVARCHAR     : SQLWriter.AddText('(NCLOB)', Result);
    else                  SQLWriter.AddText('(UNKNOWN)', Result);
  end;*)
end;

procedure TZSQLAnywherePreparedStatement.SetInt(Index, Value: Integer);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetShort(Index: Integer;
  Value: ShortInt);
begin
  SetSmall(Index, Value);
end;

procedure TZSQLAnywherePreparedStatement.SetSmall(Index: Integer;
  Value: SmallInt);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywherePreparedStatement.SetTime(Index: Integer;
  const Value: TZTime);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywherePreparedStatement.SetTimestamp(Index: Integer;
  const Value: TZTimeStamp);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZSQLAnywherePreparedStatement.SetUInt(Index: Integer;
  Value: Cardinal);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
end;

procedure TZSQLAnywherePreparedStatement.SetWord(Index: Integer; Value: Word);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
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
{$ENDIF ZEOS_DISABLE_ASA}
end.
