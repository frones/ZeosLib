{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZClasses, ZDbcIntfs, ZDbcStatement, ZDbcMySql, ZVariant, ZPlainMySqlDriver,
  ZPlainMySqlConstants, ZCompatibility, ZDbcLogging, ZDbcUtils;

type
  TMySQLPreparable = (myDelete, myInsert, myUpdate, mySelect, myCall);
  TOpenCursorCallback = procedure of Object;
  THandleStatus = (hsUnknown, hsAllocated, hsExecutedPrepared, hsExecutedOnce, hsReset);
  {** Implements Prepared MySQL Statement. }
  TZAbstractMySQLPreparedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FPMYSQL: PPMYSQL; //the connection handle
    FMySQLConnection: IZMySQLConnection;
    FMYSQL_STMT: PMYSQL_STMT; //a allocated stmt handle
    FPlainDriver: TZMySQLPlainDriver;
    FUseResult, //single row fetches with tabular streaming
    FUseDefaults, //prozess default values -> EH: this should be handled higher up (my POV)
    FMySQL_FieldType_Bit_1_IsBoolean, //self-descriptive isn't it?
    FInitial_emulate_prepare, //the user given mode
    FBindAgain, //if types or pointer locations do change(realloc f.e.) we need to bind again -> this is dead slow with mysql
    FChunkedData, //just skip the binding loop for sending long data
    FHasDefaultValues, //are default values given?
    FStmtHandleIsExecuted: Boolean; //identify state of stmt handle for flushing pending results?
    FPreparablePrefixTokens: TPreparablePrefixTokens;
    FBindOffset: PMYSQL_BINDOFFSETS;
    FPrefetchRows: Ulong; //Number of rows to fetch from server at a time when using a cursor.
    FResultsCount: Integer; //count the Results to find out if we can re-use last ResultSets
    FClientVersion: Integer; //just a local variable
    FMYSQL_BINDs: Pointer; //a buffer for N-params * mysql_bind-record size which are changing from version to version
    FMYSQL_aligned_BINDs: PMYSQL_aligned_BINDs; //offset structure to set all the mysql info's aligned to it's field-structures
    FOpenCursorCallback: TOpenCursorCallback;
    FEmulatedValues: TRawByteStringDynArray;
    FEmulatedParams: Boolean; //just use emulated String params?
    FIsFunction: Boolean; //did RegisterParameter evaluate against a ResultParameter;
    FLastWasOutParams: Boolean;
//    FHandleStatus: THandleStatus; //indicate status of MYSQL_STMT handle
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure InitBuffer(SQLType: TZSQLType; Index: Integer; Bind: PMYSQL_aligned_BIND; ActualLength: LengthInt = 0);
    procedure FlushPendingResults;
    procedure InternalRealPrepare;
    function CheckPrepareSwitchMode: Boolean;
    function ComposeRawSQLQuery: RawByteString;
    function IsOutParamResult: Boolean;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
    procedure InternalSetInParamCount(NewParamCount: Integer);
    function GetInParamLogValue(Index: Integer): RawByteString; override;
    procedure CheckParameterIndex(Value: Integer); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; override;

    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindNull(Index: Integer; SQLType: TZSQLType); override;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;
  public
    constructor Create(const Connection: IZMySQLConnection;
      const SQL: string; Info: TStrings);
  public
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZParamType; const Name: String = ''; {%H-}PrecisionOrSize: LengthInt = 0;
      {%H-}Scale: LengthInt = 0); override;

    procedure Prepare; override;
    procedure Unprepare; override;
    procedure ClearParameters; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; override;
    function GetUpdateCount: Integer; override;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); override;
  end;

  TZMySQLPreparedStatement = class(TZAbstractMySQLPreparedStatement, IZPreparedStatement);

  TZMySQLStatement = class(TZAbstractMySQLPreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZMySQLConnection; Info: TStrings);
  end;

  TZMySQLCallableStatement2 = class(TZAbstractCallableStatement_A,
    IZCallableStatement{, IZParamNamedCallableStatement})
  private
    FPlainDriver: TZMySQLPLainDriver;
  protected
    function CreateExecutionStatement(Mode: TZCallExecKind; const
      StoredProcName: String): TZAbstractPreparedStatement2; override;
    function SupportsBidirectionalParms: Boolean; override;
  public
    procedure AfterConstruction; override;
  end;

  {** Implements callable Postgresql Statement. }
  TZMySQLCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FPlainDriver: TZMysqlPlainDriver;
    FPMYSQL: PPMYSQL;
    FMYSQL_STMT: PMYSQL_STMT; //allways nil by now
    FQueryHandle: PZMySQLResult;
    FUseResult: Boolean;
    FParamNames: array [0..1024] of RawByteString;
    FParamTypeNames: array [0..1024] of RawByteString;
    FUseDefaults: Boolean;
    FOpenCursorCallback: TOpenCursorCallback;
    function GetCallSQL: RawByteString;
    function GetOutParamSQL: RawByteString;
    function GetSelectFunctionSQL: RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    procedure ClearResultSets; override;
    procedure BindInParameters; override;
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName: String; const ParamName: String; Const ColumnSize, {%H-}Precision: Integer);
  public
    constructor Create(const Connection: IZMySQLConnection;
      const SQL: string; const Info: TStrings);

    function Execute(const SQL: RawByteString): Boolean; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;

    function GetFirstResultSet: IZResultSet; override;
    function GetPreviousResultSet: IZResultSet; override;
    function GetNextResultSet: IZResultSet; override;
    function GetLastResultSet: IZResultSet; override;
    function BOR: Boolean; override;
    function EOR: Boolean; override;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; override;
    function GetResultSetCount: Integer; override;
    function GetMoreResults: Boolean; override;
  end;

implementation

uses
  Math, DateUtils, ZFastCode, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZDbcProperties,
  ZSysUtils, ZMessages, ZDbcCachedResultSet, ZEncoding, ZDbcResultSet
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

var
  MySQL41PreparableTokens: TPreparablePrefixTokens;
//  MySQL50PreparableTokens: TPreparablePrefixTokens;
//  MySQL5015PreparableTokens: TPreparablePrefixTokens;
//  MySQL5023PreparableTokens: TPreparablePrefixTokens;
//  MySQL5112PreparableTokens: TPreparablePrefixTokens;
  MySQL568PreparableTokens: TPreparablePrefixTokens;

const EnumBool: array[Boolean] of {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('N','Y');

{ TZAbstractMySQLPreparedStatement }

procedure TZAbstractMySQLPreparedStatement.BindNull(Index: Integer;
  SQLType: TZSQLType);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindNull(Index, SQLType)
    else CheckParameterIndex(Index);
    if FUseDefaults and (FInParamDefaultValues[Index] <> '')
    then FEmulatedValues[Index] := FInParamDefaultValues[Index]
    else FEmulatedValues[Index] := 'null'
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if FUseDefaults and (FInParamDefaultValues[Index] <> '') then
      BindRawStr(Index, Pointer(FInParamDefaultValues[Index]), Length(FInParamDefaultValues[Index]))
    else if (BindList.SQLTypes[Index] <> SQLType) then
      InitBuffer(SQLType, Index, Bind, 0);
    Bind^.is_null_address^ := 1;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
var
  Bind: PMYSQL_aligned_BIND;
  Len: LengthInt;
begin
  Len := Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1 then
      inherited BindRawStr(Index, Value);
    Connection.GetEscapeString(Pointer(Value), Len, FEmulatedValues[Index]);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stString) or (Bind.buffer_length_address^ < Cardinal(Len+1)) then
      InitBuffer(stString, Index, Bind, Len);
    if Len = 0
    then PByte(Bind^.buffer)^ := Ord(#0)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Bind^.buffer)^, Len+1);
    Bind^.Length[0] := Len;
    Bind^.is_null_address^ := 0;
    //BindList.SetNull(Index, stString);
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1 then
      inherited BindRawStr(Index, Buf, Len);
    Connection.GetEscapeString(Buf, Len, FEmulatedValues[Index]);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stString) or (Bind.buffer_length_address^ < Cardinal(Len+1)) then
      InitBuffer(stString, Index, Bind, Len);
    if Len = 0
    then PByte(Bind^.buffer)^ := Ord(#0)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Pointer(Bind^.buffer)^, Len+1);
    Bind^.Length[0] := Len;
    Bind^.is_null_address^ := 0;
    //BindList.SetNull(Index, stString);
  end;
end;

procedure TZAbstractMySQLPreparedStatement.CheckParameterIndex(Value: Integer);
begin
  //if not Prepared then
    //Prepare;
  if (BindList.Capacity < Value+1) then
    if FMYSQL_STMT <> nil
    then raise EZSQLException.Create(SInvalidInputParameterCount)
    else inherited CheckParameterIndex(Value);
end;

function TZAbstractMySQLPreparedStatement.CheckPrepareSwitchMode: Boolean;
begin
  Result := ((not FInitial_emulate_prepare) or (ArrayCount > 0 )) and (FMYSQL_STMT = nil) and (TokenMatchIndex <> -1) and
     ((ArrayCount > 0 ) or (ExecutionCount = MinExecCount2Prepare));
  if Result then begin
    FEmulatedParams := False;
    if (BindList.Count > 0) then
      InternalSetInParamCount(BindList.Count);
  end;
end;

procedure TZAbstractMySQLPreparedStatement.ClearParameters;
var
  array_size: UInt;
  I: Integer;
begin
  if ArrayCount > 0 then begin
    array_size := 0;
    for i := 0 to BindList.Count -1 do begin
      {$R-}
      FreeMem(FMYSQL_aligned_BINDs^[i].indicators);
      FMYSQL_aligned_BINDs^[i].indicators := nil;
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end;
    if FPlainDriver.mysql_stmt_attr_set517up(FMYSQL_STMT, STMT_ATTR_ARRAY_SIZE, @array_size) <> 0 then
      checkMySQLError (FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcPrepStmt,
        ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP), Self);
    for i := 0 to BindList.Count -1 do
      BindNull(i, TZSQLType(BindList.Arrays[i].VArrayType));
  end;
  inherited ClearParameters;
end;

function TZAbstractMySQLPreparedStatement.ComposeRawSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(FCachedQueryRaw) do
    if IsParamIndex[i] then begin
      ToBuff(FEmulatedValues[ParamIndex], Result);
      Inc(ParamIndex);
    end else
      ToBuff(FCachedQueryRaw[I], Result);
  FlushBuff(Result);
end;

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param SQL a command to execute.
  @param Info a statement parameters.
}
constructor TZAbstractMySQLPreparedStatement.Create(
  const Connection: IZMySQLConnection;
  const SQL: string; Info: TStrings);
begin
  FPlainDriver := TZMySQLPlainDriver(Connection.GetIZPlainDriver.GetInstance);
  FClientVersion := FPLainDriver.mysql_get_client_version;
  FBindOffset := GetBindOffsets(FPlainDriver.IsMariaDBDriver, FClientVersion);

  if (FPLainDriver.IsMariaDBDriver and (FClientVersion >= 100000)) or
     (not FPLainDriver.IsMariaDBDriver and (FClientVersion >= 50608))
  then FPreparablePrefixTokens := MySQL568PreparableTokens
  else FPreparablePrefixTokens := MySQL41PreparableTokens;

  FMySQLConnection := Connection;
  FPMYSQL := Connection.GetConnectionHandle;

  inherited Create(Connection, SQL, Info);

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, DSProps_UseResult, 'false'));
  if not FUseResult then
    ResultSetType := rtScrollInsensitive;
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, DSProps_Defaults, 'true'));
  FPrefetchRows := Max(1,{$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_PrefetchRows, '100'),100));

  FInitial_emulate_prepare := (FBindOffset.buffer_type=0) or (MinExecCount2Prepare < 0) or
    StrToBoolEx(DefineStatementParameter(Self, DSProps_EmulatePrepares, 'false'));
  FEmulatedParams := True;
  FMySQL_FieldType_Bit_1_IsBoolean := FMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean;
  FGUIDAsString := True;
end;

procedure TZAbstractMySQLPreparedStatement.Prepare;
begin
  FlushPendingResults;
  if not Prepared then
    inherited Prepare;
  if CheckPrepareSwitchMode then
    InternalRealPrepare;
end;

procedure TZAbstractMySQLPreparedStatement.Unprepare;
var status: Integer;
  ParamCount: Integer;
begin
  ParamCount := BindList.Count;
  inherited Unprepare;
  FlushPendingResults;
  if not FEmulatedParams and (FMYSQL_STMT <> nil) then begin
    (*if FStmtHandleIsExecuted then begin
      Status := FPlainDriver.mysql_stmt_reset(FMYSQL_STMT);
      if Status <> 0 then
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
          ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
            ConSettings^.ClientCodePage^.CP), Self);
    end;*)
    //cancel all pending results:
    //https://mariadb.com/kb/en/library/mysql_stmt_close/
    status := FPlainDriver.mysql_stmt_close(FMYSQL_STMT);
    try
      if status <> 0 then checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcUnprepStmt,
        ConvertZMsgToRaw(cSUnknownError,
        ZMessages.cCodePage, ConSettings^.ClientCodePage^.CP), Self);
    finally
      FMYSQL_STMT := nil;
      FStmtHandleIsExecuted := False;
      if ParamCount > 0 then
        ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
          ParamCount*Ord(FMYSQL_aligned_BINDs<>nil), 0, 1);
    end;
  end else if (ParamCount > 0) and (FMYSQL_BINDs <> nil) then //switch mode did alloc mem
    ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
      ParamCount*Ord(FMYSQL_aligned_BINDs<>nil), 0, 1);
  FEmulatedParams := FInitial_emulate_prepare;
  FLastWasOutParams := False;
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
function TZAbstractMySQLPreparedStatement.GetMoreResults: Boolean;
var status: Integer;
begin
  Result := False;
  if (FOpenResultSet <> nil)
  then IZResultSet(FOpenResultSet).Close;
  if FEmulatedParams or not FStmtHandleIsExecuted then begin
    if Assigned(FPlainDriver.mysql_next_result) and Assigned(FPMYSQL^) then begin
      LastUpdateCount := -1;
      if FPlainDriver.mysql_next_result(FPMYSQL^) > 0
      then CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
      FResultsCount := 0; //Reset -> user is expecting more resultsets
      if FPlainDriver.mysql_field_count(FPMYSQL^) > 0 then begin
        Result := True;
        LastResultSet := CreateResultSet(SQL);
      end else begin
        LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^);
        LastResultSet := nil;
      end;
    end;
  end else begin
    if Assigned(FPlainDriver.mysql_stmt_next_result) and Assigned(FMYSQL_STMT) then begin
      LastUpdateCount := -1;
      Status := FPlainDriver.mysql_stmt_next_result(FMYSQL_STMT);
      if Status > 0 then
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecute, ASQL, Self);
      if Status = 0 then
        if FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0 then begin
          Result := True;
          LastResultSet := CreateResultSet(SQL);
        end else begin
          LastResultSet := nil;
          LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
        end;
    end;
  end;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZAbstractMySQLPreparedStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZAbstractResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  FLastWasOutParams := IsOutParamResult;
  if FLastWasOutParams and (FOpenResultSet <> nil) then
    IZResultSet(FOpenResultSet).Close;

  if (FResultsCount = 1) and (FOpenResultSet <> nil) then begin
    Result := IZResultSet(FOpenResultSet);
    FOpenCursorCallback;
    if fUseResult and ((GetResultSetConcurrency = rcUpdatable) or
       (GetResultSetType = rtScrollInsensitive)) then begin
      Result.Last; //invoke fetch all -> note this is done on msql_strore_result too
      Result.BeforeFirst;
    end;
  end else begin
    if FUseResult and not FLastWasOutParams//server cursor?
    then NativeResultSet := TZMySQL_Use_ResultSet.Create(FPlainDriver, Self, SQL,
      False, FPMYSQL, @FMYSQL_STMT, nil, FOpenCursorCallback)
    else NativeResultSet := TZMySQL_Store_ResultSet.Create(FPlainDriver, Self, SQL,
      FLastWasOutParams, FPMYSQL, @FMYSQL_STMT, nil, FOpenCursorCallback);

    if (GetResultSetConcurrency = rcUpdatable) or
       ((GetResultSetType = rtScrollInsensitive) and FUseResult) then begin
      if (GetResultSetConcurrency = rcUpdatable) then
        if FEmulatedParams
        then CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          FPMYSQL, nil, Self, NativeResultSet.GetMetaData)
        else CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          FPMYSQL, FMYSQL_STMT, Self, NativeResultSet.GetMetaData)
      else CachedResolver := nil;
      CachedResultSet := TZCachedResultSet.CreateWithColumns(NativeResultSet.ColumnsInfo,
        NativeResultSet, SQL, CachedResolver, ConSettings);
      if fUseResult then begin
        CachedResultSet.Last; //invoke fetch all -> note this is done on msql_strore_result too
        CachedResultSet.BeforeFirst;
      end;
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
      Result.GetMetadata.IsWritable(FirstDbcIndex); //force metadata loading
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
    Inc(FResultsCount);
  end;
end;

procedure TZAbstractMySQLPreparedStatement.PrepareInParameters;
begin
  if not FEmulatedParams and (FMYSQL_STMT<> nil)
  then SetParamCount(FPlainDriver.mysql_stmt_param_count(FMYSQL_STMT))
  else InternalSetInParamCount(BindList.Capacity);
end;

procedure TZAbstractMySQLPreparedStatement.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZParamType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var
  OldCount: Integer;
begin
  OldCount := BindList.Count;
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name, PrecisionOrSize, Scale);
  FIsFunction := FIsFunction or (ParamType = zptResult);
  if not FEmulatedParams then begin
    if OldCount <> BindList.Count then
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        OldCount, BindList.Count, 1);
    {$R-}
    FMYSQL_aligned_BINDs[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null_address^ := 1;
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  end;
end;

procedure TZAbstractMySQLPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  FPMYSQL^ := nil;
  FMYSQL_STMT := nil;
  FBindAgain := True;
  FStmtHandleIsExecuted := False;
  inherited ReleaseImmediat(Sender);
end;

procedure TZAbstractMySQLPreparedStatement.SetBindCapacity(Capacity: Integer);
var OldCapacity: Integer;
begin
  OldCapacity := Bindlist.Capacity;
  inherited SetBindCapacity(Capacity);
  if OldCapacity <> BindList.Capacity then
    if (FMYSQL_STMT = nil)
    then SetLength(FEmulatedValues, BindList.Capacity)
    else ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        OldCapacity, BindList.Capacity, 1);
end;

procedure TZAbstractMySQLPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var BufferSize: ULong;
  ClientStrings: TRawByteStringDynArray;
  Bind: PMYSQL_aligned_BIND;
  I: Integer;
  ClientCP: Word;
  UniTemp: ZWideString;
  RawTemp: RawByteString;
  Lob: IZBLob;
  MySQLTime: PMYSQL_TIME;
  P: PAnsiChar;
label set_raw, move_from_temp;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if (FMYSQL_STMT = nil) then
    InternalRealPrepare;
  if (FMYSQL_STMT = nil) then
    raise EZSQLException.Create(SFailedtoPrepareStmt);
  {$R-}
  Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  ClientCP := ConSettings^.ClientCodePage.CP;
  ClientStrings := nil;
  FBindAgain := True;
  ReAllocMem(Bind^.indicators, ArrayCount);
  Bind^.indicator_address^ := Pointer(Bind^.indicators);
  FillChar(Pointer(Bind^.indicators)^, ArrayCount, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  Bind^.Iterations := ArrayCount;
  BufferSize := 0;
  case SQLType of
    stBoolean:
      if FMySQL_FieldType_Bit_1_IsBoolean then begin
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end else begin
        ReAllocMem(Bind^.length, ArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        ReAllocMem(Bind^.buffer, SizeOf(Pointer)*ArrayCount + (ArrayCount shl 1));
        Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        for i := 0 to ArrayCount -1 do begin
          PWord(PAnsiChar(Bind^.buffer)+ArrayCount*SizeOf(Pointer)+(i shl 1))^ := PWord(EnumBool[TBooleanDynArray(Value)[i]])^; //write data
          PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := PAnsiChar(Bind^.buffer)+ArrayCount*SizeOf(Pointer)+(i shl 1); //write address
          Bind^.length[i] := 1;
        end;
      end;
    stByte, stShort: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stByte));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stWord, stSmall: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stWord));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stInteger, stLongWord: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_LONG;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stLongWord));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stLong, stULong: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stULong));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stFloat: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_FLOAT;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stDouble: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stBigDecimal, stCurrency: begin
        ReAllocMem(Bind^.buffer, SizeOf(Double) *ArrayCount);
        Bind^.buffer_address^:= Pointer(Bind^.buffer);
        Bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        if SQLType = stBigDecimal then
          for i := 0 to ArrayCount -1 do
            PDouble(PAnsiChar(Bind^.buffer)+(I*SizeOf(Double)))^ := TExtendedDynArray(Value)[i]
        else
          for i := 0 to ArrayCount -1 do
            PDouble(PAnsiChar(Bind^.buffer)+(I*SizeOf(Double)))^ := TCurrencyDynArray(Value)[i];
      end;
    stDate, stTime, stTimeStamp: begin
        ReAllocMem(Bind^.buffer, (SizeOf(TMYSQL_TIME)+SizeOf(Pointer))*ArrayCount);
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
        P := PAnsiChar(Bind^.buffer)+(ArrayCount*SizeOf(Pointer));
        FillChar(P^, ArrayCount*SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
        if SQLType = stDate then begin
          Bind^.buffer_type_address^ := FIELD_TYPE_DATE;
          for i := 0 to ArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            DecodeDate(TDateTimeDynArray(Value)[i], PWord(@MySQLTime^.year)^, PWord(@MySQLTime^.month)^, PWord(@MySQLTime^.day)^);
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
            //MySQLTime.time_type := MYSQL_TIMESTAMP_DATE; //done by fillchar
          end
        end else if SQLType = stTime then begin
          Bind^.buffer_type_address^ := FIELD_TYPE_TIME;
          for i := 0 to ArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            DecodeTime(TDateTimeDynArray(Value)[i], PWord(@MySQLTime^.hour)^, PWord(@MySQLTime^.minute)^, PWord(@MySQLTime^.second)^, PWord(@MySQLTime^.second_part)^);
            MySQLTime.time_type := MYSQL_TIMESTAMP_TIME;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
          end
        end else begin
          Bind^.buffer_type_address^ := FIELD_TYPE_DATETIME;
          for i := 0 to ArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            DecodeDateTime(TDateTimeDynArray(Value)[i], PWord(@MySQLTime^.year)^, PWord(@MySQLTime^.month)^, PWord(@MySQLTime^.day)^,
              PWord(@MySQLTime^.hour)^, PWord(@MySQLTime^.minute)^, PWord(@MySQLTime^.second)^, PWord(@MySQLTime^.second_part)^);
            MySQLTime.time_type := MYSQL_TIMESTAMP_DATETIME;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
          end;
        end;
      end;
    stBytes: begin
        ReAllocMem(Bind^.buffer, SizeOf(Pointer)*ArrayCount);
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
        ReAllocMem(Bind^.length, ArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        for i := 0 to ArrayCount -1 do begin
          Bind^.length[i] := Length(TBytesDynArray(Value)[i]);
          if Bind^.length[i] > 0
          then PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := Pointer(TBytesDynArray(Value)[i]) //write address
          else PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := PEmptyAnsiString;
        end;
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
      end;
    stGUID: begin
        ReAllocMem(Bind^.length, ArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        if FGUIDAsString then begin
          ReAllocMem(Bind^.buffer, SizeOf(Pointer)*ArrayCount + (37*ArrayCount));
          Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
          P := PAnsiChar(Bind^.buffer)+ SizeOf(Pointer)*ArrayCount;
          for i := 0 to ArrayCount -1 do begin
            Bind^.length[i] := 36;
            GUIDToBuffer(@TGUIDDynArray(Value)[i].D1, P, False, True);
            PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := P; //write address
            Inc(P, 37);
          end;
        end else begin
          ReAllocMem(Bind^.buffer, SizeOf(Pointer)*ArrayCount);
          Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
          for i := 0 to ArrayCount -1 do begin
            PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := @TGUIDDynArray(Value)[i].D1; //write address
            Bind^.length[i] := SizeOf(TGUID);
          end;
        end;
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
      end;
    stString, stUnicodeString: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        ReAllocMem(Bind^.length, ArrayCount*SizeOf(Ulong));
        Bind^.length_address^ := Bind^.length;
        case VariantType of
          {$IFNDEF UNICODE}
          vtString:
            if not ConSettings.AutoEncode and ZCompatibleCodePages(ConSettings^.CTRL_CP, ClientCP)
            then goto set_raw
            else begin
              SetLength(ClientStrings, ArrayCount);
              for I := 0 to ArrayCount -1 do begin
                ClientStrings[i] := ConSettings^.ConvFuncs.ZStringToRaw(TStringDynArray(Value)[i], ConSettings^.CTRL_CP, ClientCP);
                BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
              end;
              goto move_from_temp;
            end;
          {$ENDIF}
          {$IFNDEF NO_ANSISTRING}
          vtAnsiString:
            if ZCompatibleCodePages(ZOSCodePage, ClientCP)
            then goto set_raw
            else begin
              SetLength(ClientStrings, ArrayCount);
              for I := 0 to ArrayCount -1 do begin
                ClientStrings[i] := ConSettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(Value)[i], ClientCP);
                BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
              end;
              goto move_from_temp;
            end;
          {$ENDIF}
          {$IFNDEF NO_UTF8STRING}
          vtUTF8String:
            if ZCompatibleCodePages(zCP_UTF8, ClientCP)
            then goto set_raw
            else begin
              SetLength(ClientStrings, ArrayCount);
              for I := 0 to ArrayCount -1 do begin
                ClientStrings[i] := ConSettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(Value)[i], ClientCP);
                BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
              end;
              goto move_from_temp;
            end;
          {$ENDIF}
          vtRawByteString:begin
set_raw:      //MySQL uses array of PAnsichar so we are using our buffer as pointer array
              ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*ArrayCount);
              for I := 0 to ArrayCount -1 do begin
                Bind^.length[i] := Length(TRawByteStringDynArray(Value)[i]);
                if Bind^.length[i] > 0
                then PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := Pointer(TRawByteStringDynArray(Value)[i]) //write address
                else PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := PEmptyAnsiString;
              end;
              Bind^.buffer_address^ := Pointer(Bind^.buffer);
            end;
          vtUnicodeString
          {$IFDEF UNICODE}
          ,vtString
          {$ENDIF}:       begin
              SetLength(ClientStrings, ArrayCount);
              for I := 0 to ArrayCount -1 do begin
                ClientStrings[i] := ZUnicodeToRaw(TUnicodeStringDynArray(Value)[i], ClientCP);
                BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
              end;
move_from_temp:
              ReAllocMem(Bind^.Buffer, Cardinal(SizeOf(Pointer)*ArrayCount)+BufferSize);
              P := PAnsichar(Bind^.Buffer)+ SizeOf(Pointer)*ArrayCount;
              for I := 0 to ArrayCount -1 do begin
                Bind^.length[i] := Length(ClientStrings[i]);
                if Bind^.length[i] > 0
                then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(ClientStrings[i])^, P^, Bind^.length[i]+1)  //write buffer
                else Byte(P^) := Ord(#0);
                PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := P;
                Inc(P, Bind^.length[i]+1);
              end;
              Bind^.buffer_address^ := Pointer(Bind^.buffer);
            end;
          vtCharRec:      begin
              SetLength(ClientStrings, ArrayCount);
              ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*ArrayCount); //minumum size
              for I := 0 to ArrayCount -1 do
                if ZCompatibleCodePages(TZCharRecDynArray(Value)[i].CP, ClientCP) or (TZCharRecDynArray(Value)[i].Len = 0) then begin
                  Bind^.length[i] := TZCharRecDynArray(Value)[i].Len;
                  PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := TZCharRecDynArray(Value)[i].P; //wite address
                end else if ZCompatibleCodePages(TZCharRecDynArray(Value)[i].CP, zCP_UTF16) then begin
                  ClientStrings[i] := PUnicodeToRaw(TZCharRecDynArray(Value)[i].P, TZCharRecDynArray(Value)[i].Len, ClientCP);
                  BufferSize := BufferSize + Cardinal(Length(ClientStrings[i])) +1;
                  Bind^.length[i] := Length(ClientStrings[i]);
                end else begin
                  UniTemp := PRawToUnicode(TZCharRecDynArray(Value)[i].P, TZCharRecDynArray(Value)[i].Len, TZCharRecDynArray(Value)[i].CP);
                  ClientStrings[i] := ZUnicodeToRaw(UniTemp, ClientCP);
                  BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
                  Bind^.length[i] := Length(ClientStrings[i]);
                end;
              if BufferSize > 0 then begin
                ReAllocMem(Bind^.buffer, Cardinal(ArrayCount*SizeOf(Pointer))+BufferSize);
                P := PAnsichar(Bind^.Buffer)+ SizeOf(Pointer)*ArrayCount;
                for I := 0 to ArrayCount -1 do
                  if Pointer(ClientStrings[i]) <> nil then begin
                    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(ClientStrings[i])^, P^, Bind^.length[i]); //write buffer
                    PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := P;
                    Inc(P, Bind^.length[i]+1);
                  end;
                end;
              Bind^.buffer_address^ := Pointer(Bind^.buffer);
            end;
          else raise EZSQLException.Create(SUnsupportedParameterType);
        end;
      end;
    stAsciiStream, stUnicodeStream, stBinaryStream: {in current state (mariadb 10.3) send_long_data isn't supported for batch array bindings }
      begin
        ReAllocMem(Bind^.length, ArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        Bind^.buffer_type_address^ := FIELD_TYPE_BLOB;
        ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*ArrayCount);
        for I := 0 to ArrayCount -1 do begin
          if (TInterfaceDynArray(Value)[i] = nil) or not Supports(TInterfaceDynArray(Value)[i], IZBlob, Lob) or Lob.IsEmpty then
            Bind^.indicators[i] := Ord(STMT_INDICATOR_NULL)
          else if (Lob.Length = 0) then begin
            Bind^.length[i] := 0;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := PEmptyAnsiString;
          end else begin
            if SQLType <> stBinaryStream then begin
              if not Lob.IsClob then begin
                RawTemp := GetValidatedAnsiStringFromBuffer(Lob.GetBuffer, Lob.Length, ConSettings);
                Lob := TZAbstractCLob.CreateWithData(Pointer(RawTemp), Length(RawTemp), ClientCP, ConSettings);
                TInterfaceDynArray(Value)[i] := Lob;
              end;
              Lob.GetPAnsiChar(ClientCP);
            end;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := Lob.GetBuffer;
            Bind^.length[i] := Lob.Length;
          end;
        end;
        Bind^.buffer_address^ := Pointer(Bind^.Buffer);
      end;
  end;
  Bind^.Iterations := ArrayCount;
end;

procedure TZAbstractMySQLPreparedStatement.SetDefaultValue(ParameterIndex: Integer;
  const Value: string);
var P: PChar;
begin
  P := Pointer(Value);
  if (P = nil) or FEmulatedParams or not ((P^ = #39) and ((P+Length(Value)-1)^ = #39))
  then inherited SetDefaultValue(ParameterIndex, Value)
  else inherited SetDefaultValue(ParameterIndex, Copy(Value, 2, Length(Value)-2));
end;

procedure TZAbstractMySQLPreparedStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType);
var
  Bind: PMYSQL_aligned_BIND;
  I: Integer;
  procedure SetIndicator(IsNull: Boolean; Bind: PMYSQL_aligned_BIND; Index: Integer);
  begin
    if IsNull then
      if FUseDefaults
      then Bind^.indicators[Index] := STMT_INDICATOR_DEFAULT
      else Bind^.indicators[Index] := STMT_INDICATOR_NULL
  end;
begin
  inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  {$R-}
  Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (FMYSQL_STMT = nil) then
    InternalRealPrepare;
  if (FMYSQL_STMT = nil) then
    raise EZSQLException.Create(SFailedtoPrepareStmt);
  if Bind^.Iterations <> Cardinal(ArrayCount) then begin
    ReAllocMem(Bind^.indicators, ArrayCount);
    Bind^.indicator_address^ := Pointer(Bind^.indicators);
    FBindAgain := True;
  end;
  case SQLType of
    stBoolean:      for i := 0 to ArrayCount -1 do
                      SetIndicator(TBooleanDynArray(Value)[i], Bind, I);
    stByte:         for i := 0 to ArrayCount -1 do
                      SetIndicator(TByteDynArray(Value)[i]<>0, Bind, I);
    stShort:        for i := 0 to ArrayCount -1 do
                      SetIndicator(TShortIntDynArray(Value)[i]<>0, Bind, I);
    stWord:         for i := 0 to ArrayCount -1 do
                      SetIndicator(TWordDynArray(Value)[i]<>0, Bind, I);
    stSmall:        for i := 0 to ArrayCount -1 do
                      SetIndicator(TSmallIntDynArray(Value)[i]<>0, Bind, I);
    stLongWord:     for i := 0 to ArrayCount -1 do
                      SetIndicator(TLongWordDynArray(Value)[i]<>0, Bind, I);
    stInteger:      for i := 0 to ArrayCount -1 do
                      SetIndicator(TIntegerDynArray(Value)[i]<>0, Bind, I);
    stULong:        for i := 0 to ArrayCount -1 do
                      SetIndicator(TUInt64DynArray(Value)[i]<>0, Bind, I);
    stLong:         for i := 0 to ArrayCount -1 do
                      SetIndicator(TInt64DynArray(Value)[i]<>0, Bind, I);
    stFloat:        for i := 0 to ArrayCount -1 do
                      SetIndicator(TSingleDynArray(Value)[i]<>0, Bind, I);
    stDouble:       for i := 0 to ArrayCount -1 do
                      SetIndicator(TDoubleDynArray(Value)[i]<>0, Bind, I);
    stCurrency:     for i := 0 to ArrayCount -1 do
                      SetIndicator(TCurrencyDynArray(Value)[i]<>0, Bind, I);
    stBigDecimal:   for i := 0 to ArrayCount -1 do
                      SetIndicator(TExtendedDynArray(Value)[i]<>0, Bind, I);
    stDate, stTime,
    stTimestamp:    for i := 0 to ArrayCount -1 do
                      SetIndicator(TDateTimeDynArray(Value)[i]<>0, Bind, I);
    stString, stUnicodeString:
                    case VariantType of
                      {$IFNDEF UNICODE} vtString, {$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
                      vtRawByteString:
                        for i := 0 to ArrayCount -1 do
                          SetIndicator(StrToBoolEx(TRawByteStringDynArray(Value)[i]), Bind, I);
                      {$IFDEF UNICODE} vtString, {$ENDIF}
                      vtUnicodeString:
                        for i := 0 to ArrayCount -1 do
                          SetIndicator(StrToBoolEx(TUnicodeStringDynArray(Value)[i]), Bind, I);
                      vtCharRec:
                        for i := 0 to ArrayCount -1 do
                          if ZCompatibleCodePages(TZCharRecDynArray(Value)[i].CP, zCP_UTF16)
                          then SetIndicator(StrToBoolEx(PWideChar(TZCharRecDynArray(Value)[i].P)), Bind, I)
                          else SetIndicator(StrToBoolEx(PAnsiChar(TZCharRecDynArray(Value)[i].P)), Bind, I);
                      else
                        raise EZSQLException.Create(sUnsupportedOperation);
                    end;
  end;
  Bind^.Iterations := ArrayCount;
end;

function TZAbstractMySQLPreparedStatement.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
var I: Integer;
begin
  Result := inherited AlignParamterIndex2ResultSetIndex(Value);
  for i := Value downto 0 do
    if BindList.ParamTypes[i] in [zptUnknown, zptInput] then
      Dec(Result);
end;

procedure TZAbstractMySQLPreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if (SQLType = stGUID) and FGUIDAsString then
    BindRawStr(Index, GUIDToRaw(Buf, Len, False))
  else if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindBinary(Index, SQLType, Buf, Len)
    else CheckParameterIndex(Index);
    Connection.GetBinaryEscapeString(Buf, Len, FEmulatedValues[Index])
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer_length_address^ < Cardinal(Len+1)*Byte(Ord(SQLType <> stBinaryStream))) then begin
      InitBuffer(SQLType, Index, Bind, Len);
      BindList[Index].SQLType := SQLType;
    end;
    if SQLType <> stBinaryStream then begin
      if Len = 0
      then PByte(Bind^.buffer)^ := Ord(#0)
      else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Pointer(Bind^.buffer)^, Len);
      Bind^.Length[0] := Len;
    end else begin
      FChunkedData := True;
      Bind^.Length[0] := 0;
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindInParameters;
var
  P: PAnsiChar;
  Len: NativeUInt;
  I: Integer;
  bind: PMYSQL_aligned_BIND;
  OffSet, PieceSize: LongWord;
  array_size: UInt;
begin
  if not FEmulatedParams and FBindAgain and (BindList.Count > 0) and (FMYSQL_STMT <> nil) then begin
    if (ArrayCount > 0) then begin
      //set array_size first: https://mariadb.com/kb/en/library/bulk-insert-column-wise-binding/
      array_size := ArrayCount;
      if FPlainDriver.mysql_stmt_attr_set517up(FMYSQL_STMT, STMT_ATTR_ARRAY_SIZE, @array_size) <> 0 then
        checkMySQLError (FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcPrepStmt,
          ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
          ConSettings^.ClientCodePage^.CP), Self);
    end;
    if (FPlainDriver.mysql_stmt_bind_param(FMYSQL_STMT, Pointer(FMYSQL_BINDs)) <> 0) then
      checkMySQLError (FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcPrepStmt,
        ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP), Self);
      FBindAgain := False;
  end;
  inherited BindInParameters;
  { now finlize chunked data }
  if not FEmulatedParams and FChunkedData then
    // Send large data chunked
    for I := 0 to BindList.Count - 1 do begin
      Bind := @FMYSQL_aligned_BINDs[I];
      if (Bind^.is_null_address^ = 0) and (Bind^.buffer = nil) and (BindList[i].BindType = zbtLob) then begin
        P := IZBlob(BindList[I].Value).GetBuffer;
        Len := IZBlob(BindList[I].Value).Length;
        OffSet := 0;
        PieceSize := ChunkSize;
        while (OffSet < Len) or (Len = 0) do
        begin
          if OffSet+PieceSize > Len then
            PieceSize := Len - OffSet;
          if (FPlainDriver.mysql_stmt_send_long_data(FMYSQL_STMT, I, P, PieceSize) <> 0) then begin
            checkMySQLError (FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcPrepStmt,
              ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
              ConSettings^.ClientCodePage^.CP), Self);
            exit;
          end else Inc(P, PieceSize);
          Inc(OffSet, PieceSize);
          if Len = 0 then Break;
        end;
      end;
    end;
end;

procedure TZAbstractMySQLPreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
begin
  inherited BindLob(Index, SQLType, Value); //refcounts
  if (Value = nil) or (Value.IsEmpty) then
    BindNull(Index, SQLType)
  else if FEmulatedParams then begin
    if SQLType = stBinaryStream
    then Connection.GetBinaryEscapeString(Value.GetBuffer, Value.Length, FEmulatedValues[Index])
    else Connection.GetEscapeString(Value.GetBuffer, Value.Length, FEmulatedValues[Index])
  end else
    FChunkedData := True;
end;

procedure TZAbstractMySQLPreparedStatement.UnPrepareInParameters;
begin
  inherited UnPrepareInParameters;
  FBindAgain := True;
  FChunkedData := False;
  FIsFunction := False;
end;

function TZAbstractMySQLPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := @FPreparablePrefixTokens;
end;

function TZAbstractMySQLPreparedStatement.GetInParamLogValue(
  Index: Integer): RawByteString;
var
  Bind: PMYSQL_aligned_BIND;
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  if FEmulatedParams then
    Result := FEmulatedValues[Index]
  else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if Bind^.is_unsigned_address^ = 0
        then Result := IntToRaw(PShortInt(Bind^.buffer_address^)^)
        else Result := IntToRaw(PByte(Bind^.buffer_address^)^);
      FIELD_TYPE_SHORT:
        if Bind^.is_unsigned_address^ = 0
        then Result := IntToRaw(PSmallInt(Bind^.buffer_address^)^)
        else Result := IntToRaw(PWord(Bind^.buffer_address^)^);
      FIELD_TYPE_LONG:
        if Bind^.is_unsigned_address^ = 0
        then Result := IntToRaw(PLongInt(Bind^.buffer_address^)^)
        else Result := IntToRaw(PLongWord(Bind^.buffer_address^)^);
      FIELD_TYPE_FLOAT:
        Result := FloatToSQLRaw(PSingle(Bind^.buffer_address^)^);
      FIELD_TYPE_DOUBLE:
        Result := FloatToSQLRaw(PDouble(Bind^.buffer_address^)^);
      FIELD_TYPE_NULL:
        Result := 'null';
      FIELD_TYPE_TIMESTAMP:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(Bind^.buffer_address^)^.Year,
            PMYSQL_TIME(Bind^.buffer_address^)^.Month,
            PMYSQL_TIME(Bind^.buffer_address^)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(Bind^.buffer_address^)^.Hour,
            PMYSQL_TIME(Bind^.buffer_address^)^.Minute,
            PMYSQL_TIME(Bind^.buffer_address^)^.Second,
            0{PMYSQL_TIME(Bind^.buffer_address^)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          Result := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, True);
        end;
      FIELD_TYPE_LONGLONG:
        if Bind^.is_unsigned_address^ = 0
        then Result := IntToRaw(PInt64(Bind^.buffer_address^)^)
        else Result := IntToRaw(PUInt64(Bind^.buffer_address^)^);
      FIELD_TYPE_DATE: begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(Bind^.buffer_address^)^.Year,
            PMYSQL_TIME(Bind^.buffer_address^)^.Month,
            PMYSQL_TIME(Bind^.buffer_address^)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          Result := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, True);
        end;
      FIELD_TYPE_TIME: begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(Bind^.buffer_address^)^.Hour,
            PMYSQL_TIME(Bind^.buffer_address^)^.Minute,
            PMYSQL_TIME(Bind^.buffer_address^)^.Second,
            0{PMYSQL_TIME(Bind^.buffer_address^)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          Result := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, True);
        end;
      FIELD_TYPE_YEAR:
        Result := IntToRaw(PWord(Bind^.buffer_address^)^);
      FIELD_TYPE_STRING:
          Result := SQLQuotedStr(PAnsiChar(Bind^.buffer), Bind^.length[0], {$IFDEF NO_ANSICHAR}Ord{$ENDIF}(#39));
      FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_BLOB: Result := '(Blob)'
    end;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractMySQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  RSQL: RawByteString;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  BindInParameters;
  if FEmulatedParams or (FMYSQL_STMT = nil) then begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      if FPlainDriver.mysql_field_count(FPMYSQL^) = 0 then
        if GetMoreResults
        then Result := LastResultSet
        else raise EZSQLException.Create(SCanNotOpenResultSet)
      else Result := CreateResultSet(SQL);
      FOpenResultSet := Pointer(Result);
    end else
      CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, RSQL, Self);
  end else begin
    if FplainDriver.IsMariaDBDriver and (FTokenMatchIndex = Ord(myCall)) then begin //EH: no idea why but maria db hangs if we do not reset the stmt ):
       if FPlainDriver.mysql_stmt_reset(FMYSQL_STMT) <> 0 then
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
          ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
          ConSettings^.ClientCodePage^.CP), Self);
       FBindAgain := True;
       BindInParameters;
    end;
    if (FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0) then begin
      FStmtHandleIsExecuted := True;
      if FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) = 0 then
        if GetMoreResults
        then Result := LastResultSet
        else raise EZSQLException.Create(SCanNotOpenResultSet)
      else Result := CreateResultSet(SQL);
      FOpenResultSet := Pointer(Result);
    end else
      checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
        ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP), Self);
  end;
  inherited ExecuteQueryPrepared;
  CheckPrepareSwitchMode;
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
function TZAbstractMySQLPreparedStatement.ExecuteUpdatePrepared: Integer;
var
//  QueryHandle: PZMySQLResult;
  RSQL: RawByteString;
begin
  Prepare;
  BindInParameters;
  Result := -1;
  if FEmulatedParams or (FMYSQL_STMT = nil) then begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      if (FplainDriver.mysql_field_count(FPMYSQL^) > 0) then begin
        //Result := FPlainDriver.mysql_affected_rows(FPMYSQL^); //raises av's on FPC
        //retrieve outparam
        LastResultSet := CreateResultSet(SQL);
        LastResultSet.Last;
        Result := LastResultSet.GetRow;
        LastResultSet.BeforeFirst;
      end else { Process queries with result sets }
        {if FPlainDriver.mysql_field_count(FPMYSQL^) > 0 then begin
          QueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
          Result := FPlainDriver.mysql_num_rows(QueryHandle);
          FPlainDriver.mysql_free_result(QueryHandle);
        end else { Process regular query }
          Result := FPlainDriver.mysql_affected_rows(FPMYSQL^);
    end else
      CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, RSQL, Self);
  end else begin
    if (FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0) then begin
      FStmtHandleIsExecuted := True;
      if (FplainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0) then begin
        Result := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
        //retrieve outparam
        LastResultSet := CreateResultSet(SQL);
      end else { Process queries with result sets }
        {if FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0 then begin
          FPlainDriver.mysql_stmt_store_result(FMYSQL_STMT);
          Result := FPlainDriver.mysql_stmt_num_rows(FMYSQL_STMT);
          FPlainDriver.mysql_stmt_free_result(FMYSQL_STMT);
        end else { Process regular query }
          Result := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
    end else
      checkMySQLError(FPlainDriver,FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
        ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
          ConSettings^.ClientCodePage^.CP),Self);
  end;
  Inherited ExecuteUpdatePrepared;
  CheckPrepareSwitchMode;
end;

procedure TZAbstractMySQLPreparedStatement.FlushPendingResults;
var
  FQueryHandle: PZMySQLResult;
  Status: Integer;
begin
  if FLastWasOutParams and Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  if FLastWasOutParams and Assigned(LastResultSet) then
    LastResultSet.Close;
  if (FEmulatedParams or not FStmtHandleIsExecuted) and (FPMYSQL^ <> nil) then
    while true do begin
      Status := FPlainDriver.mysql_next_result(FPMYSQL^);
      if Status = -1 then
        Break
      else if (Status = 0) {and (FPlainDriver.mysql_field_count(FPMYSQL) > 0)} then begin
        FQueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
        if FQueryHandle <> nil then begin
          FPlainDriver.mysql_free_result(FQueryHandle);
          Inc(FResultsCount);
        end;
      end else if Status > 0 then begin
        CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
        Break;
      end;
    end
  else if (FMYSQL_STMT <> nil) and FStmtHandleIsExecuted then begin
    (*if FPlainDriver.IsMariaDBDriver then begin  //mysql raises a out of sync here, even if docs saying they clear all pending results
      Status := FPlainDriver.mysql_stmt_reset(FMYSQL_STMT);
      if Status <> 0 then
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
          ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
            ConSettings^.ClientCodePage^.CP), Self);
    end else *)while true do begin //so we need to do the job by hand now
      Status := FPlainDriver.mysql_stmt_next_result(FMYSQL_STMT);
      if Status = -1 then
        Break
      else if (Status = 0) {and (FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0)} then begin
        //horray we can't store the result -> https://dev.mysql.com/doc/refman/5.7/en/mysql-stmt-store-result.html
        if FPlainDriver.mysql_stmt_free_result(FMYSQL_STMT) <> 0 then //MySQL allows this Mariadb is viny nilly now
          checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
          ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
            ConSettings^.ClientCodePage^.CP), Self);
        Inc(FResultsCount);
      end else if Status > 0 then begin
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
          ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
            ConSettings^.ClientCodePage^.CP), Self);
        Break;
      end;
    end;
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
function TZAbstractMySQLPreparedStatement.ExecutePrepared: Boolean;
var RSQL: RawByteString;
begin
  PrepareLastResultSetForReUse;
  Prepare;
  BindInParameters;
  if FEmulatedParams or (FMYSQL_STMT = nil) then begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      if FPlainDriver.mysql_field_count(FPMYSQL^) > 0
      then LastResultSet := CreateResultSet(SQL)
      else LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^)
    end else CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, RSQL, Self);
  end else begin
    if FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0 then begin
      FStmtHandleIsExecuted := True;
      if FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0
      then LastResultSet := CreateResultSet(SQL)
      else LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT)
    end else checkMySQLError(FPlainDriver,FPMYSQL^, FMYSQL_STMT, lcExecPrepStmt,
        ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
          ConSettings^.ClientCodePage^.CP), Self);
  end;
  Result := Assigned(LastResultSet);
  inherited ExecutePrepared;
  CheckPrepareSwitchMode;
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZAbstractMySQLPreparedStatement.GetUpdateCount: Integer;
begin
  Result := inherited GetUpdateCount;
  if FEmulatedParams or not FStmtHandleIsExecuted then begin
    if (Result = -1) and Assigned(FPMYSQL^) and (FPlainDriver.mysql_field_count(FPMYSQL^) = 0) then begin
      LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^);
      Result := LastUpdateCount;
    end
  end else begin
    if (Result = -1) and Assigned(FMYSQL_STMT) and (FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) = 0) then begin
      LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
      Result := LastUpdateCount;
    end;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.InitBuffer(SQLType: TZSQLType;
  Index: Integer; Bind: PMYSQL_aligned_BIND; ActualLength: LengthInt = 0);
var BuffSize: Integer;
begin
  case SQLType of
    stBoolean:      begin
                      BuffSize := 1;
                      if fMySQL_FieldType_Bit_1_IsBoolean then begin
                        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
                        Bind^.is_unsigned_address^ := 1;
                      end else Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stByte,
    stShort:        begin
                      BuffSize := SizeOf(Byte);
                      Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stByte)
                    end;
    stWord,
    stSmall:        begin
                      BuffSize := SizeOf(Word);
                      Bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stWord)
                    end;
    stLongWord,
    stInteger:      begin
                      BuffSize := SizeOf(Cardinal);
                      Bind^.buffer_type_address^ := FIELD_TYPE_LONG;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stLongWord)
                    end;
    stULong,
    stLong:         begin
                      BuffSize := SizeOf(Int64);
                      Bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stULong)
                    end;
    stFloat,
    stDouble,
    stCurrency,
    stBigDecimal:   begin
                      BuffSize := SizeOf(Double);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
                    end;
    stDate:         begin
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DATE;
                    end;
    stTime:         if true or FPlainDriver.IsMariaDBDriver then begin
                    //https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_TIME;
                    end else begin //milli/micro-second fractions are not supported
                      BuffSize := ConSettings^.WriteFormatSettings.TimeFormatLen;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stTimeStamp:    if true or FPlainDriver.IsMariaDBDriver then begin
                    //https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DATETIME;
                    end else begin //milli/micro-second fractions are not supported
                      BuffSize := ConSettings^.WriteFormatSettings.DateTimeFormatLen;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stGUID:         begin  //EH: binary(16) or char(38/36/34) ?
                      BuffSize := 38;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stString,
    stUnicodeString,
    stBytes:        begin
                      if ActualLength = 0 then
                        ActualLength := 8;
                      //ludob: mysql adds terminating #0 on top of data. Avoid buffer overrun.
                      BuffSize := Max(8, (((ActualLength-1) shr 3)+1) shl 3); //8 byte aligned including space for trailing #0
                      if SQLType <> stBytes
                      then Bind^.buffer_type_address^ := FIELD_TYPE_STRING
                      else Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
                    end;
    stAsciiStream,
    stUnicodeStream:begin
                      BuffSize := 0; //chunked
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stBinaryStream: begin
                      BuffSize := 0; //chunked
                      Bind^.buffer_type_address^ := FIELD_TYPE_BLOB;
                    end;
    else raise EZSQLException.Create(sUnsupportedOperation);
  end;
  if BuffSize > 0 then
    ReAllocMem(Bind.buffer, BuffSize+Ord(Bind^.buffer_type_address^ in [FIELD_TYPE_STRING, FIELD_TYPE_BLOB,FIELD_TYPE_TINY_BLOB] ))
  else if Bind.buffer <> nil then begin
    FreeMem(Bind.buffer);
    Bind.buffer := nil;
  end;
  Bind^.buffer_address^ := Bind.buffer;
  Bind^.buffer_length_address^ := BuffSize;
  Bind^.Iterations := 1;
  BindList[Index].SQLType := SQLType;
  fBindAgain := True;
  if (SQLType in [stDate, stTime, stTimeStamp]) and not (Bind^.buffer_type_address^ = FIELD_TYPE_STRING) then begin
    FillChar(Bind^.buffer^, SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
    if SQLType = stTime then
      PMYSQL_TIME(Bind^.buffer)^.time_type := MYSQL_TIMESTAMP_TIME
    else if SQLType = stTimeStamp then
      PMYSQL_TIME(Bind^.buffer)^.time_type := MYSQL_TIMESTAMP_DATETIME;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindBoolean(Index: Integer; Value: Boolean);
begin
  if FMySQL_FieldType_Bit_1_IsBoolean
  then BindSignedOrdinal(Index, stBoolean, Ord(Value))
  else BindRawStr(Index, EnumBool[Value]);
end;

procedure TZAbstractMySQLPreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
var
  Bind: PMYSQL_aligned_BIND;
  P: PMYSQL_TIME;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1 then
      inherited BindDateTime(Index, SQLType, Value);
    case SQLType of
      stDate: if Length(FEmulatedValues[Index])-2 < ConSettings^.WriteFormatSettings.DateFormatLen
              then FEmulatedValues[Index] := DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, True)
              else DateTimeToRawSQLDate(Value, Pointer(FEmulatedValues[Index]), ConSettings^.WriteFormatSettings, True);
      stTime: if Length(FEmulatedValues[Index])-2 < ConSettings^.WriteFormatSettings.TimeFormatLen
              then FEmulatedValues[Index] := DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, True)
              else DateTimeToRawSQLTime(Value, Pointer(FEmulatedValues[Index]), ConSettings^.WriteFormatSettings, True);
      stTimestamp: if Length(FEmulatedValues[Index])-2 < ConSettings^.WriteFormatSettings.DateTimeFormatLen
              then FEmulatedValues[Index] := DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, True)
              else DateTimeToRawSQLTimeStamp(Value, Pointer(FEmulatedValues[Index]), ConSettings^.WriteFormatSettings, True);
    end;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer = nil)  then
      InitBuffer(SQLType, Index, Bind);
    P := Pointer(bind^.buffer);
    P^.neg := Ord(Value < 0);
    if P^.time_type = MYSQL_TIMESTAMP_DATE then
      DecodeDate(Value, PWord(@P^.Year)^, PWord(@P^.Month)^, PWord(@P^.Day)^)
    else begin
      P^.second_part := 0;
      if P^.time_type = MYSQL_TIMESTAMP_TIME
      then DecodeTime(Value, PWord(@P^.hour)^, PWord(@P^.minute)^, PWord(@P^.second)^, PWord(@P^.second_part)^)
      else DecodeDateTime(Value, PWord(@P^.Year)^, PWord(@P^.Month)^, PWord(@P^.Day)^,
        PWord(@P^.hour)^, PWord(@P^.minute)^, PWord(@P^.second)^, PWord(@P^.second_part)^);
      P^.second_part := P^.second_part*1000;
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindDouble(Index, SQLType, Value)
    else CheckParameterIndex(Index);
    FEmulatedValues[Index] := FloatToSQLRaw(Value);
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer = nil)  then
      InitBuffer(SQLType, Index, Bind);
    PDouble(bind^.buffer)^ := Value;
    Bind^.is_null_address^ := 0;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: Int64);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindSignedOrdinal(Index, SQLType, Value)
    else CheckParameterIndex(Index);
    FEmulatedValues[Index] := IntToRaw(Value);
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer = nil) then
      InitBuffer(SQLType, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PLongInt(Bind^.buffer)^ := LongInt(Value)
                            else PLongWord(Bind^.buffer)^ := LongWord(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                      else PUInt64(Bind^.buffer)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractMySQLPreparedStatement.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: UInt64);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindSignedOrdinal(Index, SQLType, Value)
    else CheckParameterIndex(Index);
    FEmulatedValues[Index] := IntToRaw(Value);
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer = nil) then
      InitBuffer(SQLType, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PLongInt(Bind^.buffer)^ := LongInt(Value)
                            else PLongWord(Bind^.buffer)^ := LongWord(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
                            else PUInt64(Bind^.buffer)^ := Value;
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
    end;
    Bind^.is_null_address^ := 0;
    //BindList.SetNull(Index, SQLType);
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAbstractMySQLPreparedStatement.InternalRealPrepare;
var
  I: Integer;
  P: PansiChar;
begin
  if (FMYSQL_STMT = nil) then
    FMYSQL_STMT := FPlainDriver.mysql_stmt_init(FPMYSQL^);
  FBindAgain := True;
  FStmtHandleIsExecuted := False;
  if (FPlainDriver.mysql_stmt_prepare(FMYSQL_STMT, Pointer(ASQL), length(ASQL)) <> 0) then
    checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcPrepStmt,
      ConvertZMsgToRaw(SFailedtoPrepareStmt,
      ZMessages.cCodePage, ConSettings^.ClientCodePage^.CP), Self);
  //see user comment: http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-fetch.html
  //"If you want work with more than one statement simultaneously, anidated select,
  //for example, you must declare CURSOR_TYPE_READ_ONLY the statement after just prepared this.!"
  if FUseResult and ((TokenMatchIndex = Ord(mySelect)) or (TokenMatchIndex = Ord(myCall)) ) then
    //EH: This can be set only if results are expected else server is hanging on execute
    if (FClientVersion >= 50020 ) then //supported since 5.0.2
      if Assigned(FPlainDriver.mysql_stmt_attr_set517UP) //we need this to be able to use more than !one! stmt -> keep cached
      then FPlainDriver.mysql_stmt_attr_set517UP(FMYSQL_STMT, STMT_ATTR_CURSOR_TYPE, @CURSOR_TYPE_READ_ONLY)
      else FPlainDriver.mysql_stmt_attr_set(FMYSQL_STMT, STMT_ATTR_CURSOR_TYPE, @CURSOR_TYPE_READ_ONLY);
  if FClientVersion >= 50060 then //supported since 5.0.6
    //try achieve best performnce. No idea how to calculate it
    if Assigned(FPlainDriver.mysql_stmt_attr_set517UP) and (FPrefetchRows <> 1)
    then FPlainDriver.mysql_stmt_attr_set517UP(FMYSQL_STMT, STMT_ATTR_PREFETCH_ROWS, @FPrefetchRows)
    else FPlainDriver.mysql_stmt_attr_set(FMYSQL_STMT, STMT_ATTR_PREFETCH_ROWS, @FPrefetchRows);
  FEmulatedParams := False;
  if FHasDefaultValues then
    for I := 0 to High(FInParamDefaultValues) do begin
      P := Pointer(FInParamDefaultValues[i]);
      if (P<>nil) and (PByte(P)^ = Ord(#39)) and (PByte(P+Length(FInParamDefaultValues[i])-1)^=Ord(#39))
      then FInParamDefaultValues[i] := Copy(FInParamDefaultValues[i], 2, Length(FInParamDefaultValues[i])-2)
      else FInParamDefaultValues[i] := FInParamDefaultValues[i];
    end;
  SetLength(FEmulatedValues, 0);
  if (BindList.Capacity > 0) and (FMYSQL_BINDs = nil) then
    InternalSetInParamCount(BindList.Capacity);
end;

procedure TZAbstractMySQLPreparedStatement.InternalSetInParamCount(NewParamCount: Integer);
var I: Integer;
  V: TZVariant;
  SQLType: TZSQLType;
begin
  if not FEmulatedParams then
    if (FMYSQL_BINDs <> nil) and (NewParamCount <> BindList.Count) or ((NewParamCount > 0) and (FMYSQL_aligned_BINDs = nil)) then begin
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        BindList.Count*Ord(FMYSQL_aligned_BINDs<>nil), NewParamCount, 1);
      //init buffers and move data to buffer
      if NewParamCount > 0 then
        for i := 0 to BindList.Count -1 do begin
          if BindList.BindTypes[i] in [zbtNull, zbtLob, zbtArray] then begin
            if BindList.BindTypes[i] = zbtLob then
              FChunkedData := True;
            continue;
          end;
          V := BindList.Variants[I];
          SQLType := BindList.SQLTypes[I];
          BindList.SetNull(I, stUnknown);
          case V.VType of
            vtBoolean:  BindBoolean(I, v.VBoolean);
            vtBytes:    BindBinary(I, SQLType, Pointer(V.VBytes), Length(V.VBytes));
            vtInteger:  BindSignedOrdinal(i, SQLType, V.VInteger);
            vtUInteger: BindUnSignedOrdinal(i, SQLType, V.VUInteger);
            vtFloat:    BindDouble(i, SQLType, V.VFloat);
            vtDateTime: BindDatetime(i, SQLType, V.VDateTime);
            vtRawByteString: BindRawStr(i, V.VRawByteString);
            vtCharRec:  BindRawStr(i, V.VCharRec.P, V.VCharRec.Len);
          end;
        end;
    end;
end;


function TZAbstractMySQLPreparedStatement.IsOutParamResult: Boolean;
begin
  Result := False;
  if FPMYSQL^ <> nil then
    Result := PLongWord(NativeUInt(FPMYSQL^)+GetServerStatusOffset(FClientVersion))^ and SERVER_PS_OUT_PARAMS <> 0;
end;

{ TZMySQLCallableStatement }

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZMySQLCallableStatement.GetCallSQL: RawByteString;
  function GenerateParamsStr(Count: integer): RawByteString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count-1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      if FDBParamTypes[i] in [zptInput..zptResult] then
        Result := Result + '@'+FParamNames[i];
    end;
  end;

var
  InParams: RawByteString;
begin
  if HasOutParameter then
    InParams := GenerateParamsStr(OutParamCount)
  else
    InParams := GenerateParamsStr(InParamCount);
  Result := 'CALL '+ConSettings^.ConvFuncs.ZStringToRaw(SQL,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)+'('+InParams+')';
end;

function TZMySQLCallableStatement.GetOutParamSQL: RawByteString;
  function GenerateParamsStr: RawByteString;
  var
    I: integer;
  begin
    Result := '';
    I := 0;
    while True do
      if ( I = Length(FDBParamTypes)) or (FDBParamTypes[i] = zptUnknown) then
        break
      else begin
        if FDBParamTypes[i] in [zptOutput..zptResult] then begin
          if Result <> '' then
            Result := Result + ',';
          if FParamTypeNames[i] = '' then
            Result := Result + ' @'+FParamNames[I]+' AS '+FParamNames[I]
          else
            Result := Result + ' CAST(@'+FParamNames[I]+ ' AS '+FParamTypeNames[i]+') AS '+FParamNames[I];
        end;
        Inc(i);
      end;
  end;

var
  OutParams: RawByteString;
begin
  OutParams := GenerateParamsStr;
  Result := 'SELECT '+ OutParams;
end;

function TZMySQLCallableStatement.GetSelectFunctionSQL: RawByteString;
  function GenerateInParamsStr: RawByteString;
  var
    I: Integer;
  begin
    Result := '';
    for i := 0 to Length(InParamValues) -1 do
      if Result = '' then
        Result := PrepareAnsiSQLParam(I)
      else
        Result := Result+', '+ PrepareAnsiSQLParam(I);
  end;
var
  InParams: RawByteString;
begin
  InParams := GenerateInParamsStr;
  Result := 'SELECT '+ConSettings^.ConvFuncs.ZStringToRaw(SQL,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)+'('+InParams+')';
  Result := Result + ' AS ReturnValue';
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
function TZMySQLCallableStatement.PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := ZDbcMySQLUtils.MySQLPrepareAnsiSQLParam(GetConnection as IZMySQLConnection,
    InParamValues[ParamIndex], InParamDefaultValues[ParamIndex], ClientVarManager,
    InParamTypes[ParamIndex], FUseDefaults);
end;

procedure TZMySQLCallableStatement.ClearResultSets;
begin
  inherited;
  FPlainDriver.mysql_free_result(FQueryHandle);
  FQueryHandle := nil;
end;

procedure TZMySQLCallableStatement.BindInParameters;
var
  I: integer;
  ExecQuery: RawByteString;
begin
  I := 0;
  ExecQuery := '';
  while True do
    if (i = Length(FDBParamTypes)) then
      break
    else
    begin
      if FDBParamTypes[i] in [zptInput, zptInputOutput] then
        if ExecQuery = '' then
          ExecQuery := 'SET @'+FParamNames[i]+' = '+PrepareAnsiSQLParam(I)
        else
          ExecQuery := ExecQuery + ', @'+FParamNames[i]+' = '+PrepareAnsiSQLParam(I);
      Inc(i);
    end;
  if not (ExecQuery = '') then
    if FPlainDriver.mysql_real_query(Self.FPMYSQL^, Pointer(ExecQuery), Length(ExecQuery)) = 0 then begin
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcBindPrepStmt, ConSettings^.Protocol, ExecQuery)
    end else
      CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ExecQuery, Self);
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLCallableStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQL_Store_ResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQL_Store_ResultSet.Create(FPlainDriver, Self, SQL, True, FPMYSQL, @FMYSQL_STMT,
    @LastUpdateCount, FOpenCursorCallback);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) or (not IsFunction) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, FPMYSQL, nil,
      Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(rcReadOnly);
    {Need to fetch all data. The handles must be released for mutiple
      Resultsets}
    CachedResultSet.Last;//Fetch all
    CachedResultSet.BeforeFirst;//Move to first pos
    //if FQueryHandle <> nil then
      //FPlainDriver.mysql_free_result(FQueryHandle);
    //NativeResultSet.ResetCursor; //Release the handles
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZMySQLCallableStatement.RegisterParamTypeAndName(const ParameterIndex:integer;
  const ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
var ParamTypeNameLo: String;
begin
  FParamNames[ParameterIndex] := ConSettings^.ConvFuncs.ZStringToRaw(ParamName,
    ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  ParamTypeNameLo := LowerCase(ParamTypeName);
  if ( ZFastCode.Pos('char', ParamTypeNameLo) > 0 ) or
     ( ZFastCode.Pos('set', ParamTypeNameLo) > 0 ) then
    FParamTypeNames[ParameterIndex] := 'CHAR('+ZFastCode.IntToRaw(ColumnSize)+')'
  else
    if ( ZFastCode.Pos('set', ParamTypeNameLo) > 0 ) then
      FParamTypeNames[ParameterIndex] := 'CHAR('+ZFastCode.IntToRaw(ColumnSize)+')'
    else
      if ( ZFastCode.Pos('datetime', ParamTypeNameLo) > 0 ) or
         ( ZFastCode.Pos('timestamp', ParamTypeNameLo) > 0 ) then
        FParamTypeNames[ParameterIndex] := 'DATETIME'
      else
        if ( ZFastCode.Pos('date', ParamTypeNameLo) > 0 ) then
          FParamTypeNames[ParameterIndex] := 'DATE'
        else
          if ( ZFastCode.Pos('time', ParamTypeNameLo) > 0 ) then
            FParamTypeNames[ParameterIndex] := 'TIME'
          else
            if ( ZFastCode.Pos('int', ParamTypeNameLo) > 0 ) or
               ( ZFastCode.Pos('year', ParamTypeNameLo) > 0 ) then
              FParamTypeNames[ParameterIndex] := 'SIGNED'
            else
              if ( ZFastCode.Pos('binary', ParamTypeNameLo) > 0 ) then
                FParamTypeNames[ParameterIndex] := 'BINARY('+ZFastCode.IntToRaw(ColumnSize)+')'
              else
                FParamTypeNames[ParameterIndex] := '';
end;

constructor TZMySQLCallableStatement.Create(const Connection: IZMySQLConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPMYSQL := Connection.GetConnectionHandle;
  FPlainDriver := TZMySQLPlainDriver(Connection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtScrollInsensitive;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, DSProps_UseResult, 'false'));
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, DSProps_Defaults, 'true'))
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  ASQL := SQL;
  if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(ASQL), Length(ASQL)) = 0 then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    if FPlainDriver.mysql_field_count(FPMYSQL^) = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    if IsFunction then
      ClearResultSets;
    FResultSets.Add(CreateResultSet(Self.SQL));
    if FPlainDriver.mysql_more_results(FPMYSQL^) = 1 then begin
      while FPlainDriver.mysql_next_result(FPMYSQL^) = 0 do
        if FPlainDriver.mysql_more_results(FPMYSQL^) = 1 then
          FResultSets.Add(CreateResultSet(Self.SQL))
        else break;
      CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
    end;
    FActiveResultset := FResultSets.Count-1;
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
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
function TZMySQLCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  Result := -1;
  ASQL := SQL;
  if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(ASQL), Length(ASQL)) = 0 then
  begin
    { Process queries with result sets }
    if FPlainDriver.mysql_field_count(FPMYSQL^) > 0 then begin
      ClearResultSets;
      FActiveResultset := 0;
      FResultSets.Add(CreateResultSet(Self.SQL));
      if FPlainDriver.mysql_more_results(FPMYSQL^) = 1 then begin
        Result := LastUpdateCount;
        while FPlainDriver.mysql_next_result(FPMYSQL^) = 0 do
          if FPlainDriver.mysql_more_results(FPMYSQL^) = 1 then begin
            FResultSets.Add(CreateResultSet(Self.SQL));
            inc(Result, LastUpdateCount); //LastUpdateCount will be returned from ResultSet.Open
          end
          else break;
        CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
      end
      else
        Result := LastUpdateCount;
      FActiveResultset := FResultSets.Count-1;
      LastResultSet := IZResultSet(FResultSets[FActiveResultset]);
    end
    else { Process regular query }
      Result := FPlainDriver.mysql_affected_rows(FPMYSQL^);
  end
  else
    CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
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
function TZMySQLCallableStatement.Execute(const SQL: RawByteString): Boolean;
begin
  Result := False;
  ASQL := SQL;
  if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(ASQL), Length(ASQL)) = 0 then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    { Process queries with result sets }
    if FPlainDriver.mysql_field_count(FPMYSQL^) > 0 then begin
      Result := True;
      LastResultSet := CreateResultSet(Self.SQL);
    end else { Processes regular query. }
      LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^);
  end else
    CheckMySQLError(FPlainDriver, FPMYSQL^, nil, lcExecute, ASQL, Self);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteQuery(GetSelectFunctionSQL);
  end
  else
  begin
    BindInParameters;
    ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      Result := ExecuteQuery(GetOutParamSQL) //Get the Last Resultset
    else
      Result := GetLastResultSet;
  end;
  if Assigned(Result) then
    AssignOutParamValuesFromResultSet(Result, OutParamValues, OutParamCount , FDBParamTypes);
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
function TZMySQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteUpdate(GetSelectFunctionSQL);
    AssignOutParamValuesFromResultSet(LastResultSet, OutParamValues, OutParamCount , FDBParamTypes);
  end
  else
  begin
    BindInParameters;
    Result := ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      AssignOutParamValuesFromResultSet(ExecuteQuery(GetOutParamSQL), OutParamValues, OutParamCount , FDBParamTypes);
    Inc(Result, LastUpdateCount);
  end;
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLCallableStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>False</code> This is not a prepared mysql statement.
}
function TZMySQLCallableStatement.IsPreparedStatement: Boolean;
begin
  Result := False;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetNextResultSet: IZResultSet;
begin
  if ( FActiveResultset < FResultSets.Count-1) and ( FResultSets.Count > 1) then
  begin
    Inc(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetPreviousResultSet: IZResultSet;
begin
  if ( FActiveResultset > 0) and ( FResultSets.Count > 0) then
  begin
    Dec(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetFirstResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := 0;
    Result := IZResultSet(FResultSets[0]);
  end;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetLastResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := FResultSets.Count -1;
    Result := IZResultSet(FResultSets[FResultSets.Count -1]);
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
function TZMySQLCallableStatement.GetMoreResults: Boolean;
begin
  Result := FResultSets.Count > 0;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZMySQLCallableStatement.BOR: Boolean;
begin
  Result := FActiveResultset = 0;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZMySQLCallableStatement.EOR: Boolean;
begin
  Result := FActiveResultset = FResultSets.Count -1;
end;

{**
  Retrieves a ResultSet by his index.
  @param Integer the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
function TZMySQLCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
  if ( Index < 0 ) or ( Index > FResultSets.Count -1 ) then
    raise Exception.Create(Format(SListIndexError, [Index]))
  else
    Result := IZResultSet(FResultSets[Index]);
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZMySQLCallableStatement.GetResultSetCount: Integer;
begin
  Result := FResultSets.Count;
end;

{ TZMySQLStatement }

constructor TZMySQLStatement.Create(const Connection: IZMySQLConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FEmulatedParams := True;
  FMinExecCount2Prepare := -1;
  FInitial_emulate_prepare := True;
end;

{ TZMySQLCallableStatement2 }

procedure TZMySQLCallableStatement2.AfterConstruction;
begin
  inherited AfterConstruction;
  FPlainDriver := TZMySQLPLainDriver(Connection.GetIZPlainDriver.GetInstance);
end;

function TZMySQLCallableStatement2.CreateExecutionStatement(Mode: TZCallExecKind;
  const StoredProcName: String): TZAbstractPreparedStatement2;
var
  I: Integer;
  P: PChar;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
begin
  SQL := '';
  if IsFunction
  then ToBuff('SELECT ', SQL)
  else ToBuff('CALL ', SQL);
  ToBuff(StoredProcName, SQL);
  ToBuff('(', SQL);
  for i := 0 to BindList.Count-1 do
    if BindList.ParamTypes[i] <> zptResult then
      ToBuff('?,', SQL);
  FlushBuff(SQL);
  P := Pointer(SQL);
  if (P+Length(SQL)-1)^ = ','
  then (P+Length(SQL)-1)^ := ')' //cancel last comma
  else (P+Length(SQL)-1)^ := ' ';
  if IsFunction then
    SQL := SQL +' as ReturnValue';
  Result := TZAbstractMySQLPreparedStatement.Create(Connection as IZMySQLConnection, SQL, Info);
  Result.MinExecCount2Prepare := 0; //prepare immediately
  TZAbstractMySQLPreparedStatement(Result).InternalRealPrepare;
  FExecStatements[TZCallExecKind(not Ord(Mode) and 1)] := Result;
  TZAbstractMySQLPreparedStatement(Result)._AddRef;
end;

function TZMySQLCallableStatement2.SupportsBidirectionalParms: Boolean;
begin
  Result := True; //indicate not skipping out-values
end;

initialization

{ preparable statements: }

{ http://dev.mysql.com/doc/refman/4.1/en/sql-syntax-prepared-statements.html }
SetLength(MySQL41PreparableTokens, Ord(mySelect)+1);
MySQL41PreparableTokens[0].MatchingGroup := 'DELETE';
MySQL41PreparableTokens[1].MatchingGroup := 'INSERT';
MySQL41PreparableTokens[2].MatchingGroup := 'UPDATE';
MySQL41PreparableTokens[3].MatchingGroup := 'SELECT';

SetLength(MySQL568PreparableTokens, Ord(myCall)+1);
MySQL568PreparableTokens[Ord(myDelete)].MatchingGroup := 'DELETE';
MySQL568PreparableTokens[Ord(myInsert)].MatchingGroup := 'INSERT';
MySQL568PreparableTokens[Ord(myUpdate)].MatchingGroup := 'UPDATE';
MySQL568PreparableTokens[Ord(mySelect)].MatchingGroup := 'SELECT';
MySQL568PreparableTokens[Ord(myCall)].MatchingGroup := 'CALL';

(*EH commented all -> usually most of them are called once
SetLength(MySQL41PreparableTokens, 13);
MySQL41PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL41PreparableTokens[0].ChildMatches, 1);
  MySQL41PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL41PreparableTokens[1].MatchingGroup := 'COMMIT';
MySQL41PreparableTokens[2].MatchingGroup := 'CREATE';
  SetLength(MySQL41PreparableTokens[2].ChildMatches, 2);
  MySQL41PreparableTokens[2].ChildMatches[0] := 'INDEX';
  MySQL41PreparableTokens[2].ChildMatches[1] := 'TABLE';
MySQL41PreparableTokens[3].MatchingGroup := 'DROP';
  SetLength(MySQL41PreparableTokens[3].ChildMatches, 2);
  MySQL41PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL41PreparableTokens[3].ChildMatches[1] := 'TABLE';
MySQL41PreparableTokens[4].MatchingGroup := 'DELETE';
MySQL41PreparableTokens[5].MatchingGroup := 'DO';
MySQL41PreparableTokens[6].MatchingGroup := 'INSERT';
MySQL41PreparableTokens[7].MatchingGroup := 'RENAME';
  SetLength(MySQL41PreparableTokens[7].ChildMatches, 1);
  MySQL41PreparableTokens[7].ChildMatches[0] := 'TABLE';
MySQL41PreparableTokens[8].MatchingGroup := 'REPLACE';
MySQL41PreparableTokens[9].MatchingGroup := 'SELECT';
MySQL41PreparableTokens[10].MatchingGroup := 'SET';
MySQL41PreparableTokens[11].MatchingGroup := 'SHOW';
MySQL41PreparableTokens[12].MatchingGroup := 'UPDATE';

{ http://dev.mysql.com/doc/refman/5.0/en/sql-syntax-prepared-statements.html }
SetLength(MySQL50PreparableTokens, 15);
MySQL50PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL50PreparableTokens[0].ChildMatches, 1);
  MySQL50PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[1].MatchingGroup := 'CALL';
MySQL50PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL50PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL50PreparableTokens[3].ChildMatches, 2);
  MySQL50PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL50PreparableTokens[3].ChildMatches[1] := 'TABLE';
MySQL50PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL50PreparableTokens[4].ChildMatches, 2);
  MySQL50PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL50PreparableTokens[4].ChildMatches[1] := 'TABLE';
MySQL50PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL50PreparableTokens[6].MatchingGroup := 'DO';
MySQL50PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL50PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL50PreparableTokens[8].ChildMatches, 1);
  MySQL50PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL50PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL50PreparableTokens[11].MatchingGroup := 'SET';
MySQL50PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL50PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL50PreparableTokens[13].ChildMatches, 1);
  MySQL50PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[14].MatchingGroup := 'UPDATE';

SetLength(MySQL5015PreparableTokens, 15);
MySQL5015PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5015PreparableTokens[0].ChildMatches, 1);
  MySQL5015PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5015PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5015PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5015PreparableTokens[3].ChildMatches, 3);
  MySQL5015PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5015PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5015PreparableTokens[3].ChildMatches[2] := 'VIEW';
MySQL5015PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5015PreparableTokens[4].ChildMatches, 3);
  MySQL5015PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5015PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5015PreparableTokens[4].ChildMatches[2] := 'VIEW';
MySQL5015PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5015PreparableTokens[6].MatchingGroup := 'DO';
MySQL5015PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5015PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5015PreparableTokens[8].ChildMatches, 1);
  MySQL5015PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5015PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5015PreparableTokens[11].MatchingGroup := 'SET';
MySQL5015PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5015PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5015PreparableTokens[13].ChildMatches, 1);
  MySQL5015PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[14].MatchingGroup := 'UPDATE';

SetLength(MySQL5023PreparableTokens, 18);
MySQL5023PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5023PreparableTokens[0].ChildMatches, 1);
  MySQL5023PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5023PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5023PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5023PreparableTokens[3].ChildMatches, 3);
  MySQL5023PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5023PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5023PreparableTokens[3].ChildMatches[2] := 'VIEW';
MySQL5023PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5023PreparableTokens[4].ChildMatches, 3);
  MySQL5023PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5023PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5023PreparableTokens[4].ChildMatches[2] := 'VIEW';
MySQL5023PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5023PreparableTokens[6].MatchingGroup := 'DO';
MySQL5023PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5023PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5023PreparableTokens[8].ChildMatches, 1);
  MySQL5023PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5023PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5023PreparableTokens[11].MatchingGroup := 'SET';
MySQL5023PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5023PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5023PreparableTokens[13].ChildMatches, 1);
  MySQL5023PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL5023PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL5023PreparableTokens[15].ChildMatches, 1);
  MySQL5023PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL5023PreparableTokens[16].ChildMatches, 1);
  MySQL5023PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL5023PreparableTokens[17].ChildMatches, 1);
  MySQL5023PreparableTokens[17].ChildMatches[0] := 'TABLE';

{http://dev.mysql.com/doc/refman/5.1/en/sql-syntax-prepared-statements.html}
SetLength(MySQL5112PreparableTokens, 30);
MySQL5112PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5112PreparableTokens[0].ChildMatches, 1);
  MySQL5112PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5112PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5112PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5112PreparableTokens[3].ChildMatches, 5);
  MySQL5112PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5112PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5112PreparableTokens[3].ChildMatches[2] := 'VIEW';
  MySQL5112PreparableTokens[3].ChildMatches[3] := 'DATABASE';
  MySQL5112PreparableTokens[3].ChildMatches[4] := 'USER';
MySQL5112PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5112PreparableTokens[4].ChildMatches, 5);
  MySQL5112PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5112PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5112PreparableTokens[4].ChildMatches[2] := 'VIEW';
  MySQL5112PreparableTokens[4].ChildMatches[3] := 'DATABASE';
  MySQL5112PreparableTokens[4].ChildMatches[4] := 'USER';
MySQL5112PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5112PreparableTokens[6].MatchingGroup := 'DO';
MySQL5112PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5112PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5112PreparableTokens[8].ChildMatches, 3);
  MySQL5112PreparableTokens[8].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[8].ChildMatches[1] := 'DATABASE';
  MySQL5112PreparableTokens[8].ChildMatches[2] := 'USER';
MySQL5112PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5112PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5112PreparableTokens[11].MatchingGroup := 'SET';
MySQL5112PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5112PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5112PreparableTokens[13].ChildMatches, 1);
  MySQL5112PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL5112PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL5112PreparableTokens[15].ChildMatches, 1);
  MySQL5112PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL5112PreparableTokens[16].ChildMatches, 1);
  MySQL5112PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL5112PreparableTokens[17].ChildMatches, 1);
  MySQL5112PreparableTokens[17].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[18].MatchingGroup := 'CACHE';
  SetLength(MySQL5112PreparableTokens[18].ChildMatches, 1);
  MySQL5112PreparableTokens[18].ChildMatches[0] := 'INDEX';
MySQL5112PreparableTokens[19].MatchingGroup := 'CHANGE';
  SetLength(MySQL5112PreparableTokens[19].ChildMatches, 1);
  MySQL5112PreparableTokens[19].ChildMatches[0] := 'MASTER';
MySQL5112PreparableTokens[20].MatchingGroup := 'CHECKSUM';
  SetLength(MySQL5112PreparableTokens[20].ChildMatches, 2);
  MySQL5112PreparableTokens[20].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[20].ChildMatches[1] := 'TABLES';
MySQL5112PreparableTokens[21].MatchingGroup := 'FLUSH';
  SetLength(MySQL5112PreparableTokens[21].ChildMatches, 10);
  MySQL5112PreparableTokens[21].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[21].ChildMatches[1] := 'TABLES';
  MySQL5112PreparableTokens[21].ChildMatches[2] := 'HOSTS';
  MySQL5112PreparableTokens[21].ChildMatches[3] := 'PRIVILEGES';
  MySQL5112PreparableTokens[21].ChildMatches[4] := 'LOGS';
  MySQL5112PreparableTokens[21].ChildMatches[5] := 'STATUS';
  MySQL5112PreparableTokens[21].ChildMatches[6] := 'MASTER';
  MySQL5112PreparableTokens[21].ChildMatches[7] := 'SLAVE';
  MySQL5112PreparableTokens[21].ChildMatches[8] := 'DES_KEY_FILE';
  MySQL5112PreparableTokens[21].ChildMatches[9] := 'USER_RESOURCES';
MySQL5112PreparableTokens[22].MatchingGroup := 'GRANT';
MySQL5112PreparableTokens[23].MatchingGroup := 'INSTALL';
  SetLength(MySQL5112PreparableTokens[23].ChildMatches, 1);
  MySQL5112PreparableTokens[23].ChildMatches[0] := 'PLUGIN';
MySQL5112PreparableTokens[24].MatchingGroup := 'KILL';
MySQL5112PreparableTokens[25].MatchingGroup := 'LOAD';
  SetLength(MySQL5112PreparableTokens[25].ChildMatches, 1);
  MySQL5112PreparableTokens[25].ChildMatches[0] := 'INDEX'; //+INTO CACHE
MySQL5112PreparableTokens[26].MatchingGroup := 'RESET';
  SetLength(MySQL5112PreparableTokens[26].ChildMatches, 3);
  MySQL5112PreparableTokens[26].ChildMatches[0] := 'MASTER';
  MySQL5112PreparableTokens[26].ChildMatches[1] := 'SLAVE';
  MySQL5112PreparableTokens[26].ChildMatches[2] := 'QUERY'; //+CACHE
MySQL5112PreparableTokens[27].MatchingGroup := 'REVOKE';
MySQL5112PreparableTokens[28].MatchingGroup := 'SLAVE';
  SetLength(MySQL5112PreparableTokens[28].ChildMatches, 2);
  MySQL5112PreparableTokens[28].ChildMatches[0] := 'START';
  MySQL5112PreparableTokens[28].ChildMatches[1] := 'STOP';
MySQL5112PreparableTokens[29].MatchingGroup := 'UNINSTALL';
  SetLength(MySQL5112PreparableTokens[29].ChildMatches, 1);
  MySQL5112PreparableTokens[29].ChildMatches[0] := 'PLUGIN';

{http://dev.mysql.com/doc/refman/5.6/en/sql-syntax-prepared-statements.html}
SetLength(MySQL568PreparableTokens, 30);
MySQL568PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL568PreparableTokens[0].ChildMatches, 2);
  MySQL568PreparableTokens[0].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[0].ChildMatches[1] := 'USER';
MySQL568PreparableTokens[1].MatchingGroup := 'CALL';
MySQL568PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL568PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL568PreparableTokens[3].ChildMatches, 5);
  MySQL568PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL568PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL568PreparableTokens[3].ChildMatches[2] := 'VIEW';
  MySQL568PreparableTokens[3].ChildMatches[3] := 'DATABASE';
  MySQL568PreparableTokens[3].ChildMatches[4] := 'USER';
MySQL568PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL568PreparableTokens[4].ChildMatches, 5);
  MySQL568PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL568PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL568PreparableTokens[4].ChildMatches[2] := 'VIEW';
  MySQL568PreparableTokens[4].ChildMatches[3] := 'DATABASE';
  MySQL568PreparableTokens[4].ChildMatches[4] := 'USER';
MySQL568PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL568PreparableTokens[6].MatchingGroup := 'DO';
MySQL568PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL568PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL568PreparableTokens[8].ChildMatches, 3);
  MySQL568PreparableTokens[8].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[8].ChildMatches[1] := 'DATABASE';
  MySQL568PreparableTokens[8].ChildMatches[2] := 'USER';
MySQL568PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL568PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL568PreparableTokens[11].MatchingGroup := 'SET';
MySQL568PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL568PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL568PreparableTokens[13].ChildMatches, 1);
  MySQL568PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL568PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL568PreparableTokens[15].ChildMatches, 1);
  MySQL568PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL568PreparableTokens[16].ChildMatches, 1);
  MySQL568PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL568PreparableTokens[17].ChildMatches, 1);
  MySQL568PreparableTokens[17].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[18].MatchingGroup := 'CACHE';
  SetLength(MySQL568PreparableTokens[18].ChildMatches, 1);
  MySQL568PreparableTokens[18].ChildMatches[0] := 'INDEX';
MySQL568PreparableTokens[19].MatchingGroup := 'CHANGE';
  SetLength(MySQL568PreparableTokens[19].ChildMatches, 1);
  MySQL568PreparableTokens[19].ChildMatches[0] := 'MASTER';
MySQL568PreparableTokens[20].MatchingGroup := 'CHECKSUM';
  SetLength(MySQL568PreparableTokens[20].ChildMatches, 2);
  MySQL568PreparableTokens[20].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[20].ChildMatches[1] := 'TABLES';
MySQL568PreparableTokens[21].MatchingGroup := 'FLUSH';
  SetLength(MySQL568PreparableTokens[21].ChildMatches, 10);
  MySQL568PreparableTokens[21].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[21].ChildMatches[1] := 'TABLES';
  MySQL568PreparableTokens[21].ChildMatches[2] := 'HOSTS';
  MySQL568PreparableTokens[21].ChildMatches[3] := 'PRIVILEGES';
  MySQL568PreparableTokens[21].ChildMatches[4] := 'LOGS';
  MySQL568PreparableTokens[21].ChildMatches[5] := 'STATUS';
  MySQL568PreparableTokens[21].ChildMatches[6] := 'MASTER';
  MySQL568PreparableTokens[21].ChildMatches[7] := 'SLAVE';
  MySQL568PreparableTokens[21].ChildMatches[8] := 'DES_KEY_FILE';
  MySQL568PreparableTokens[21].ChildMatches[9] := 'USER_RESOURCES';
MySQL568PreparableTokens[22].MatchingGroup := 'GRANT';
MySQL568PreparableTokens[23].MatchingGroup := 'INSTALL';
  SetLength(MySQL568PreparableTokens[23].ChildMatches, 1);
  MySQL568PreparableTokens[23].ChildMatches[0] := 'PLUGIN';
MySQL568PreparableTokens[24].MatchingGroup := 'KILL';
MySQL568PreparableTokens[25].MatchingGroup := 'LOAD';
  SetLength(MySQL568PreparableTokens[25].ChildMatches, 1);
  MySQL568PreparableTokens[25].ChildMatches[0] := 'INDEX'; //+INTO CACHE
MySQL568PreparableTokens[26].MatchingGroup := 'RESET';
  SetLength(MySQL568PreparableTokens[26].ChildMatches, 3);
  MySQL568PreparableTokens[26].ChildMatches[0] := 'MASTER';
  MySQL568PreparableTokens[26].ChildMatches[1] := 'SLAVE';
  MySQL568PreparableTokens[26].ChildMatches[2] := 'QUERY'; //+CACHE
MySQL568PreparableTokens[27].MatchingGroup := 'REVOKE';
MySQL568PreparableTokens[28].MatchingGroup := 'SLAVE';
  SetLength(MySQL568PreparableTokens[28].ChildMatches, 2);
  MySQL568PreparableTokens[28].ChildMatches[0] := 'START';
  MySQL568PreparableTokens[28].ChildMatches[1] := 'STOP';
MySQL568PreparableTokens[29].MatchingGroup := 'UNINSTALL';
  SetLength(MySQL568PreparableTokens[29].ChildMatches, 1);
  MySQL568PreparableTokens[29].ChildMatches[0] := 'PLUGIN'; *)

end.
