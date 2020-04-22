{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASAStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZDbcIntfs, ZDbcStatement, ZCompatibility, ZDbcLogging, ZVariant, ZClasses,
  ZDbcASA, ZDbcASAUtils, ZPlainASADriver, ZPlainASAConstants;

type
  {** Implements Prepared SQL Statement. }
  TZAbstractASAStatement = class(TZRawParamDetectPreparedStatement)
  private
    FCursorOptions: SmallInt;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FPlainDriver: TZASAPlainDriver;
    FInParamSQLDA: PASASQLDA;
    FResultSQLDA: PASASQLDA;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
    FInParamSQLData: IZASASQLDA;
    FHasOutParams: Boolean;
  private
    function CreateResultSet: IZResultSet;
  protected
    procedure CheckParameterIndex(var Value: Integer); override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure AfterClose; override;
    procedure Cancel; override;
    function GetMoreResults: Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZASAStatement = Class(TZAbstractASAStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  End;

  TZASAPreparedStatement = class(TZAbstractASAStatement, IZPreparedStatement)
  private
    procedure InitBind(SQLVAR: PZASASQLVAR; ASAType: Smallint; Len: Cardinal);
  protected
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindTimeStampStruct(Index: Integer; ASAType: SmallInt; const Value: TZASASQLDateTime);
  protected
    procedure UnPrepareInParameters; override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZRawSQLStringWriter; Var Result: RawByteString); override;
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
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGuid(Index: Integer; const Value: TGUID); reintroduce;
    procedure SetDate(Index: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; const Value: TZTimeStamp); reintroduce; overload;
  end;

  TZASACallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses ZSysUtils, ZDbcUtils, ZMessages, ZDbcASAResultSet, ZDbcCachedResultSet,
  ZEncoding, ZDbcProperties, ZFastCode;

{ TZAbstractASAStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the query
  @param Info a statement parameters.
}
constructor TZAbstractASAStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FASAConnection := Connection as IZASAConnection;
  FPlainDriver := TZASAPlainDriver(FASAConnection.GetIZPlainDriver.GetInstance);
  FetchSize := BlockSize;
  ResultSetType := rtScrollSensitive;
  with ZClasses.TZRawSQLStringWriter.Create(40) do begin
    AddOrd(Pointer(FASAConnection.GetDBHandle), FCursorName);
    AddChar(AnsiChar('_'), FCursorName);
    AddOrd(FStatementId, FCursorName);
    Finalize(FCursorName);
    Free;
  end;
end;

destructor TZAbstractASAStatement.Destroy;
begin
  inherited Destroy;
  FASAConnection := nil;
end;

function TZAbstractASAStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZASANativeResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  With FASAConnection do begin
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
    FSQLData := TZASASQLDA.Create(FPlainDriver,
      FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings);
    DescribeCursor(FASAConnection, FSQLData, CursorName, ASQL);
    NativeResultSet := TZASANativeResultSet.Create(Self, SQL, FStmtNum, CursorName, FSQLData, CachedLob);
    if ResultSetConcurrency = rcUpdatable then begin
      CachedResultSet := TZASACachedResultSet.Create(NativeResultSet, SQL, nil, ConSettings);
      CachedResultSet.SetResolver(TZASACachedResolver.Create(Self, NativeResultSet.GetMetadata));
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
  end;
end;

procedure TZAbstractASAStatement.Prepare;
var DBHandle: PZASASQLCA;
begin
  if not Prepared then
  begin
    DBHandle := FASAConnection.GetDBHandle;
    if FStmtNum <> 0 then
    begin
      FPlainDriver.dbpp_dropstmt(DBHandle, nil, nil, @FStmtNum);
      FStmtNum := 0;
    end;
    if ResultSetConcurrency = rcUpdatable then
      FCursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
    else
      FCursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
    if ResultSetType = rtScrollInsensitive then
      FCursorOptions := FCursorOptions + CUR_INSENSITIVE;
    FInParamSQLData := TZASASQLDA.Create(FPlainDriver,
      FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings, FCountOfQueryParams);
    FInParamSQLDA := FInParamSQLData.GetData;
    {EH: ASA describes the StmtNum and Variable-Count only
        the first descriptor field is ignored
        also the ParamSQL MUST be given because we wanted to describe the inputparams (even if no types nor names are done)
        else the FMoreResuls indicator does not work properly }
    if Assigned(FPlainDriver.dbpp_prepare_describe_16) then
      FPlainDriver.dbpp_prepare_describe_16(DBHandle, nil, nil, @FStmtNum, Pointer(ASQL),
        FResultSQLDA, FInParamSQLDA, SQL_PREPARE_DESCRIBE_STMTNUM +
          SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0, 0)
    else if Assigned(FPlainDriver.dbpp_prepare_describe_12) then
      FPlainDriver.dbpp_prepare_describe_12(DBHandle, nil, nil, @FStmtNum, Pointer(ASQL),
        FResultSQLDA, FInParamSQLDA, SQL_PREPARE_DESCRIBE_STMTNUM +
          SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0, 0)
    else
      FPlainDriver.dbpp_prepare_describe(DBHandle, nil, nil, @FStmtNum, Pointer(ASQL),
        FResultSQLDA, FInParamSQLDA, SQL_PREPARE_DESCRIBE_STMTNUM +
          SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0);
    ZDbcASAUtils.CheckASAError(FPlainDriver, DBHandle, lcExecute, GetConSettings, ASQL);
    SetParamCount(FInParamSQLDA.sqld);
    if FInParamSQLDA.sqld <> FInParamSQLDA.sqln then begin
      FInParamSQLData.AllocateSQLDA(FInParamSQLDA.sqld);
      FInParamSQLDA := FInParamSQLData.GetData;
      {ASA describes !paramcount! and if params are outparams only}
      FPlainDriver.dbpp_describe(DBHandle, nil, nil, @FStmtNum,
        FInParamSQLDA, SQL_DESCRIBE_INPUT);
      ZDbcASAUtils.CheckASAError(FPlainDriver, DBHandle, lcExecute, GetConSettings, ASQL);
    end;
    FMoreResults := DBHandle.sqlerrd[2] = 0; //we need to know if more ResultSets can be retrieved
    if not FMoreResults then begin
      FSQLData := TZASASQLDA.Create(FPlainDriver,
        FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings, 0);
      FResultSQLDA := FSQLData.GetData;
      FPLainDriver.dbpp_describe(DBHandle, nil, nil, @FStmtNum, FResultSQLDA, SQL_DESCRIBE_OUTPUT);
      ZDbcASAUtils.CheckASAError(FPlainDriver, DBHandle, lcExecute, GetConSettings, ASQL);
      if FResultSQLDA.sqld <> FResultSQLDA.sqln then begin
        FSQLData.AllocateSQLDA(FResultSQLDA.sqld);
        FResultSQLDA := FSQLData.GetData;
        FPLainDriver.dbpp_describe(DBHandle, nil, nil, @FStmtNum, FResultSQLDA, SQL_DESCRIBE_OUTPUT);
        ZDbcASAUtils.CheckASAError(FPlainDriver, DBHandle, lcExecute, GetConSettings, ASQL);
        { test if Outparams are available: }
        FHasOutParams := FResultSQLDA.sqlVar[0].sqlInd^ and DT_PROCEDURE_OUT = DT_PROCEDURE_OUT;
        //if FHasOutParams then
          //FSQLData.InitFields;
      end;
    end;
    inherited Prepare
  end;
end;

procedure TZAbstractASAStatement.Unprepare;
begin
  if not Assigned(FOpenResultSet) then //on closing the RS we exec db_close
    FPlainDriver.dbpp_close(FASAConnection.GetDBHandle, Pointer(CursorName));
  inherited Unprepare;
end;

procedure TZAbstractASAStatement.AfterClose;
begin
  if FStmtNum <> 0 then begin
    FPlainDriver.dbpp_dropstmt(FASAConnection.GetDBHandle, nil, nil, @FStmtNum);
    FStmtNum := 0;
  end;
  FInParamSQLDA := nil;
end;

procedure TZAbstractASAStatement.Cancel;
begin
  with FASAConnection do begin
    FPlainDriver.db_cancel_request(GetDBHandle);
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
  end;
end;

procedure TZAbstractASAStatement.CheckParameterIndex(var Value: Integer);
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

function TZAbstractASAStatement.GetMoreResults: Boolean;
begin
  Result := FMoreResults;
  if FMoreResults then begin
    with FASAConnection do begin
      FPlainDriver.dbpp_resume(GetDBHandle, Pointer(CursorName));
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE
      then Result := false
      else DescribeCursor(FASAConnection, FSQLData, CursorName, EmptyRaw);
    end;
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
function TZAbstractASAStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;
  if FMoreResults or FHasOutParams
  then LastResultSet := ExecuteQueryPrepared
  else begin
    FPlainDriver.dbpp_open(FASAConnection.GetDBHandle, Pointer(CursorName),
      nil, nil, @FStmtNum, FInParamSQLDA, FetchSize, 0, CUR_OPEN_DECLARE + CUR_READONLY);  //need a way to know if a resultset can be retrieved
    if FASAConnection.GetDBHandle.sqlCode = SQLE_OPEN_CURSOR_ERROR then begin
      ExecuteUpdatePrepared;
      FLastResultSet := nil;
    end else begin
      ZDbcASAUtils.CheckASAError(FPlainDriver, FASAConnection.GetDBHandle, lcExecute, ConSettings);
      LastResultSet := CreateResultSet;
    end;
  end;
  Result := Assigned(FLastResultSet);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractASAStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;

  with FASAConnection do begin
    if not FHasOutParams then begin
      FPlainDriver.dbpp_open(GetDBHandle, Pointer(CursorName), nil, nil, @FStmtNum,
        FInParamSQLDA, FetchSize, 0, FCursorOptions);
      if Assigned(FOpenResultSet)
      then Result := IZResultSet(FOpenResultSet)
      else Result := CreateResultSet;
    end else begin
      //first create the ResultSet -> exact types are described
      if Assigned(FOpenResultSet)
      then Result := IZResultSet(FOpenResultSet)
      else begin
        Result := TZASAParamererResultSet.Create(Self, SQL, FStmtNum, CursorName, FSQLData, True);
        FOpenResultSet := Pointer(Result);
      end;
      //now fill the outparam SQLDA-Variables
      FPlainDriver.dbpp_execute_into(GetDBHandle, nil, nil, @FStmtNum, FInParamSQLDA, FResultSQLDA);
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
      FOutParamResultSet := Result;
    end;
  end;
  { Logging SQL Command and values}
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
function TZAbstractASAStatement.ExecuteUpdatePrepared: Integer;
var Handle: PZASASQLCA;
begin
  Prepare;
  if FWeakIntfPtrOfIPrepStmt <> nil then
    BindInParameters;
  if FHasOutParams and (FOpenResultSet = nil) then begin
    //first create the ResultSet -> exact types are described
    FOutParamResultSet := TZASAParamererResultSet.Create(Self, SQL, FStmtNum, CursorName, FSQLData, True);
    FOpenResultSet := Pointer(FOutParamResultSet);
  end;
  with FASAConnection do begin
    Handle := GetDBHandle;
    FPlainDriver.dbpp_execute_into(Handle, nil, nil, @FStmtNum,
      FInParamSQLDA, FResultSQLDA);
    ZDbcASAUtils.CheckASAError(FPlainDriver, Handle, lcExecute, ConSettings,
      ASQL, SQLE_TOO_MANY_RECORDS);
    Result := GetDBHandle.sqlErrd[2];
    LastUpdateCount := Result;
    { Autocommit statement.
      EH: we've a chained mode only(deprecated by sybase)
      no idea if that's correct, it's alltime code}
    if GetAutoCommit then begin
      FPlainDriver.dbpp_commit(Handle, 0);
      CheckASAError(FPlainDriver, Handle, lcTransaction, ConSettings);
    end;
  end;
  { Logging SQL Command and values }
  inherited ExecuteUpdatePrepared;
end;

{ TZASAPreparedStatement }

procedure TZASAPreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var ASAType: SmallInt;
  P: Pointer;
  L: NativeUint;
  SQLVAR: PZASASQLVAR;
begin
  inherited BindLob(Index, SQLType, Value); //else FPC raises tons of memleaks
  if (Value = nil) or Value.IsEmpty then
    SetNull(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType)
  else begin
    P := IZBlob(BindList[Index].Value).GetBuffer(FRawTemp, L);
    if SQLType = stBinaryStream
    then ASAType := DT_LONGBINARY
    else ASAType := DT_LONGVARCHAR;
    SQLVAR := @FInParamSQLDA.sqlvar[Index];
    InitBind(SQLVAR, ASAType or 1, L);
    Move(P^, PZASABlobStruct(SQLVAR.sqlData).arr[0], L);
  end;
end;

procedure TZASAPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  if Pointer(Value) <> nil
  then BindRawStr(Index, Pointer(Value), Length(Value))
  else BindRawStr(Index, PEmptyAnsiString, 0);
end;

procedure TZASAPreparedStatement.BindRawStr(Index: Integer; Buf: PAnsiChar;
  Len: LengthInt);
var SQLVAR: PZASASQLVAR;
  sqlType: SmallInt;
begin
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (Len + SizeOf(TZASASQLSTRING) > High(SmallInt)) then begin
    if fClientCP = zCP_UTF8
    then sqlType := DT_LONGNVARCHAR or 1
    else sqlType := DT_LONGVARCHAR or 1;
    InitBind(SQLVAR, sqlType, Len);
    Move(Buf^, PZASABlobStruct(SQLVAR.sqlData).arr[0], Len);
  end else begin
    if fClientCP = zCP_UTF8
    then sqlType := DT_NVARCHAR or 1
    else sqlType := DT_VARCHAR or 1;
    if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> sqlType) or (SQLVAR.SQLlen < Len+SizeOf(TZASASQLSTRING)) then
      InitBind(SQLVAR, sqlType or 1, Len);
    SQLVAR.sqlind^ := 0; //not NULL
    Move(Buf^, PZASASQLSTRING(SQLVAR.sqlData).data[0], Len);
    PZASASQLSTRING(SQLVAR.sqlData).length := Len;
  end;

end;

procedure TZASAPreparedStatement.BindTimeStampStruct(Index: Integer;
  ASAType: SmallInt; const Value: TZASASQLDateTime);
var SQLVAR: PZASASQLVAR;
begin
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_TIMESTAMP_STRUCT or 1) then
    InitBind(SQLVAR, DT_TIMESTAMP_STRUCT or 1, SizeOf(TZASASQLDateTime));
  SQLVAR.sqlind^ := 0; //not NULL
  PZASASQLDateTime(SQLVAR.sqlData)^ := Value;
  PSmallInt(PAnsiChar(SQLVAR.sqlData)+SizeOf(TZASASQLDateTime))^ := ASAType; //save declared type for the logs
end;

procedure TZASAPreparedStatement.InitBind(SQLVAR: PZASASQLVAR;
  ASAType: Smallint; Len: Cardinal);
begin
  with SQLVAR^ do begin
    if Assigned( sqlData) then
      FreeMem(SQLData);
    case ASAType and $FFFE of
        DT_LONGBINARY, DT_LONGNVARCHAR, DT_LONGVARCHAR: begin
          GetMem(sqlData, Len + SizeOf( TZASABlobStruct));
          PZASABlobStruct( sqlData).array_len := Len;
          PZASABlobStruct( sqlData).stored_len := Len;
          PZASABlobStruct( sqlData).untrunc_len := Len;
          PZASABlobStruct( sqlData).arr[0] := AnsiChar(#0);
          sqllen := SizeOf( TZASABlobStruct)-1;
        end;
      DT_BINARY, DT_VARCHAR, DT_NVARCHAR: begin
          sqllen := Len + SizeOf( TZASASQLSTRING);
          GetMem(sqlData, sqllen);
          PZASASQLSTRING( sqlData).length := 0;
        end;
      DT_DATE, DT_TIME, DT_TIMESTAMP, DT_TIMESTAMP_STRUCT: begin
          sqllen := SizeOf(TZASASQLDateTime);
          GetMem(sqlData, SizeOf(TZASASQLDateTime)+SizeOf(SmallInt));
          PSmallInt(PAnsiChar(SQLData)+SizeOf(TZASASQLDateTime))^ := ASAType; //save declared type
          ASAType := DT_TIMESTAMP_STRUCT or 1;
        end;
      else begin
          GetMem(sqlData, Len);
          sqllen := Len;
        end;
    end;
    sqlType := ASAType;
  end;
end;

procedure TZASAPreparedStatement.SetBigDecimal(Index: Integer;
  const Value: TBCD);
begin
  SetRawByteString(Index, BCDToSQLRaw(Value));
end;

procedure TZASAPreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_BIT or 1) then
    InitBind(SQLVAR, DT_BIT or 1, SizeOf(Byte));
  SQLVAR.sqlind^ := 0; //not NULL
  PByte(SQLVAR.sqlData)^ := Ord(Value);
end;

procedure TZASAPreparedStatement.SetByte(Index: Integer; Value: Byte);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_TINYINT or 1) then
    InitBind(SQLVAR, DT_TINYINT or 1, SizeOf(Byte));
  SQLVAR.sqlind^ := 0; //not NULL
  PByte(SQLVAR.sqlData)^ := Value;
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
procedure TZASAPreparedStatement.SetBytes(ParameterIndex: Integer; Value: PByte;
  Len: NativeUInt);
var SQLVAR: PZASASQLVAR;
begin
  if (Len = 0) or (Value = nil) then
    SetNull(ParameterIndex, stBytes)
  else begin
    {$IFNDEF GENERIC_INDEX}
    ParameterIndex := ParameterIndex -1;
    {$ENDIF}
    CheckParameterIndex(ParameterIndex);
    SQLVAR := @FInParamSQLDA.sqlvar[ParameterIndex];
    if (Len + SizeOf(TZASASQLSTRING) > Cardinal(High(SmallInt))) then begin
      if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_LONGBINARY or 1) then
      InitBind(SQLVAR, DT_LONGBINARY or 1, Len);
      Move(Value^, PZASABlobStruct(SQLVAR.sqlData).arr[0], Len);
    end else begin
      if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_BINARY or 1) or (SQLVAR.SQLlen < NativeInt(Len)+SizeOf(TZASASQLSTRING)) then
        InitBind(SQLVAR, DT_BINARY or 1, Len);
      SQLVAR.sqlind^ := 0; //not NULL
      Move(Value^, PZASASQLSTRING(SQLVAR.sqlData).data[0], Len);
      PZASASQLSTRING(SQLVAR.sqlData).length := Len;
    end;
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
procedure TZASAPreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
var SQLVAR: PZASASQLVAR;
  Len: LengthInt;
begin
  Len := Length(Value);
  if Len = 0 then
    SetNull(Index, stBytes)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    SQLVAR := @FInParamSQLDA.sqlvar[Index];
    if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_BINARY or 1) or (SQLVAR.SQLlen < Len+SizeOf(TZASASQLSTRING)) then
      InitBind(SQLVAR, DT_BINARY or 1, Len);
    SQLVAR.sqlind^ := 0; //not NULL
    Move(Pointer(Value)^, PZASASQLSTRING(SQLVAR.sqlData).data[0], Len);
    PZASASQLSTRING(SQLVAR.sqlData).length := Len;
  end;
end;

procedure TZASAPreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
begin
  SetRawByteString(Index, CurrToRaw(Value));
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZASAPreparedStatement.SetDate(Index: Integer;
  const Value: TZDate);
var TS: TZASASQLDateTime;
begin
  FillChar(TS, SizeOf(TZASASQLDateTime), #0);
  TS.Year := Value.Year;
  Ts.Month := Value.Month -1;
  TS.Day := Value.Day;
  if Value.IsNegative then
    TS.Year := -TS.Year;
  BindTimeStampStruct(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, DT_DATE, TS);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZASAPreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_DOUBLE or 1) then
    InitBind(SQLVAR, DT_DOUBLE or 1, SizeOf(Double));
  SQLVAR.sqlind^ := 0; //not NULL
  PDouble(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetFloat(Index: Integer;
  Value: Single);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_FLOAT or 1) then
    InitBind(SQLVAR, DT_FLOAT or 1, SizeOf(Single));
  SQLVAR.sqlind^ := 0; //not NULL
  PSingle(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetGuid(Index: Integer;
  const Value: TGUID);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_FIXCHAR or 1) or (SQLVAR.SQLlen <> 36) then
    InitBind(SQLVAR, DT_FIXCHAR or 1, 36);
  SQLVAR.sqlind^ := 0; //not NULL
  ZSysUtils.GUIDToBuffer(@Value.D1, PAnsiChar(SQLVAR.sqlData), []);
end;

procedure TZASAPreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZRawSQLStringWriter; var Result: RawByteString);
var SQLVAR: PZASASQLVAR;
  DT: TDateTime;
begin
  CheckParameterIndex(ParamIndex);
  SQLVAR := @FInParamSQLDA.sqlvar[ParamIndex];
  if (SQLVar.sqlInd <> nil) and (SQLVar.sqlInd^ = -1) then
    SQLWriter.AddText('(NULL)', Result)
  else case SQLVar.sqlType and $FFFE of
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
  end;
end;

procedure TZASAPreparedStatement.SetInt(Index, Value: Integer);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_INT or 1) then
    InitBind(SQLVAR, DT_INT or 1, SizeOf(Integer));
  SQLVAR.sqlind^ := 0; //not NULL
  PInteger(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_BIGINT or 1) then
    InitBind(SQLVAR, DT_BIGINT or 1, SizeOf(Int64));
  SQLVAR.sqlind^ := 0; //not NULL
  PInt64(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[ Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> SQLType2ASATypeMap[SQLType] or 1) then
    InitBind(SQLVAR, SQLType2ASATypeMap[SQLType] or 1, SQLType2ASASizeMap[SQLType]);
  SQLVAR.sqlind^ := -1 //NULL
end;

procedure TZASAPreparedStatement.SetShort(Index: Integer;
  Value: ShortInt);
begin
  SetSmall(Index, Value);
end;

procedure TZASAPreparedStatement.SetSmall(Index: Integer;
  Value: SmallInt);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_SMALLINT or 1) then
    InitBind(SQLVAR, DT_SMALLINT or 1, SizeOf(SmallInt));
  SQLVAR.sqlind^ := 0; //not NULL
  PSmallInt(SQLVAR.sqlData)^ := Value;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZASAPreparedStatement.SetTime(Index: Integer;
  const Value: TZTime);
var TS: TZASASQLDateTime;
begin
  FillChar(TS, SizeOf(TZASASQLDateTime), #0);
  TS.Hour := Value.Hour;
  Ts.Minute := Value.Minute;
  TS.Second := Value.Second;
  TS.MicroSecond := Value.Fractions div 1000;
  BindTimeStampStruct(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, DT_TIME, TS);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZASAPreparedStatement.SetTimestamp(Index: Integer;
  const Value: TZTimeStamp);
var TS: TZASASQLDateTime;
begin
  FillChar(TS, SizeOf(TZASASQLDateTime), #0);
  TS.Year := Value.Year;
  Ts.Month := Value.Month -1;
  TS.Day := Value.Day;
  if Value.IsNegative then
    TS.Year := -TS.Year;
  TS.Hour := Value.Hour;
  Ts.Minute := Value.Minute;
  TS.Second := Value.Second;
  TS.MicroSecond := Value.Fractions div 1000;
  BindTimeStampStruct(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, DT_TIME, TS);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZASAPreparedStatement.SetUInt(Index: Integer;
  Value: Cardinal);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_UNSINT or 1) then
    InitBind(SQLVAR, DT_UNSINT or 1, SizeOf(Cardinal));
  SQLVAR.sqlind^ := 0; //not NULL
  PCardinal(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_UNSBIGINT or 1) then
    InitBind(SQLVAR, DT_UNSBIGINT or 1, SizeOf(UInt64));
  SQLVAR.sqlind^ := 0; //not NULL
  PUInt64(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.SetWord(Index: Integer; Value: Word);
var SQLVAR: PZASASQLVAR;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLVAR := @FInParamSQLDA.sqlvar[Index];
  if (SQLVAR.sqlData = nil) or (SQLVAR.sqlType <> DT_UNSSMALLINT or 1) then
    InitBind(SQLVAR, DT_UNSSMALLINT or 1, SizeOf(Word));
  SQLVAR.sqlind^ := 0; //not NULL
  PWord(SQLVAR.sqlData)^ := Value;
end;

procedure TZASAPreparedStatement.UnPrepareInParameters;
begin
  inherited;
  FInParamSQLDA := nil;
end;

{ TZASAStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZASAStatement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZASACallableStatement }

function TZASACallableStatement.CreateExecutionStatement(
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
  Result := TZASAPreparedStatement.Create(Connection , SQL, Info);
  TZASAPreparedStatement(Result).Prepare;
end;

{$ENDIF ZEOS_DISABLE_ASA}
end.



