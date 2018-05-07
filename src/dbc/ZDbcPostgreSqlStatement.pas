{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql, ZDbcUtils;

type
  TEICategory = (eicExecute, eicExeParam, eicPrepStmt, eicExecPrepStmt, eicUnprepStmt);

  { TZPostgreSQLPreparedStatement }

  {** PostgreSQL Prepared SQL statement interface. }
  IZPGSQLPreparedStatement = interface(IZPreparedStatement)
    ['{EED35CAA-8F36-4639-8B67-32DF237E8F6F}']
    function GetLastQueryHandle: PZPostgreSQLResult;
  end;

  TZPostgreSQLPreparedStatement = class(TZAbstractPreparedStatement,
    IZPGSQLPreparedStatement)
  private
    FRawPlanName: RawByteString;
    FPostgreSQLConnection: IZPostgreSQLConnection;
    FPlainDriver: TZPostgreSQLPlainDriver;
    QueryHandle: PZPostgreSQLResult;
    FOidAsBlob: Boolean;
    FConnectionHandle: PZPostgreSQLConnect;
    Findeterminate_datatype, fServerCursor: Boolean;
    FUseEmulatedStmtsOnly: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    fPrepareCnt: Cardinal;
    fParamSQL: RawByteString;
    Finteger_datetimes: Boolean;
  protected
    procedure FlushPendingResults; virtual;
    function CreateResultSet(QueryHandle: PZPostgreSQLResult; ServerCursor: Boolean): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PZPostgreSQLResult; virtual; abstract;
    function PrepareAnsiSQLQuery: RawByteString; virtual;
    function GetDeallocateSQL: RawByteString; virtual; abstract;
    function GetPrepareSQLPrefix: RawByteString; virtual; abstract;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; override;
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings); overload;
    constructor Create(const Connection: IZPostgreSQLConnection;
      Info: TStrings); overload;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    function GetLastQueryHandle: PZPostgreSQLResult;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement with AnsiString usage }
  TZPostgreSQLClassicPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FExecSQL: RawByteString;
    function GetAnsiSQLQuery: RawByteString;
  protected
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PZPostgreSQLResult; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer; Escaped: Boolean): RawByteString;
    procedure BindInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement based on Protocol 3.0
    ServerVersion 7.4Up and ClientVersion 8.0Up. with C++API usage
    And First with full overrides for all Setters to skip the
    TZVariant-bottleneck}
  TZPostgreSQLCAPIPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FPQparamValues: TPQparamValues;
    FPQparamLengths: TPQparamLengths; //EH: i know PG ignores this for str vals but we'll keep it for by ref logging or pqexecparams
    FPQparamFormats: TPQparamFormats;
    FPQparamBuffs: TRawByteStringDynArray;
    FDefaultValues: TRawByteStringDynArray;
    FParamOIDs: TPQparamTypes;
    FLobs: array of IZBLob;
    FStatBuf: array[0..7] of Byte; //just a fix buff for the network order
    fnParams: Integer;
    fHasOIDLobs: Boolean;
    procedure InternalSetInParamCount(NewParamCount: Integer);
  protected
    function PrepareAnsiSQLQuery: RawByteString; override;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PZPostgreSQLResult; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
    procedure SetInParamCount(const NewParamCount: Integer); override;

    Procedure BindStr(ParameterIndex: Integer; SQLType: TZSQLType; const Value: RawByteString);
    procedure BindBin(ParameterIndex: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
    procedure BindNetworkOrderBin(ParameterIndex: Integer; SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt);

    procedure InternalSetOrdinal(ParameterIndex: Integer; SQLType: TZSQLType; const Value: Int64); override;
    procedure InternalSetDouble(ParameterIndex: Integer; SQLType: TZSQLType; const Value: Double); override;
    procedure InternalSetDateTime(ParameterIndex: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    //date/time to posgres binary format
    function TimeToInt64(const Value: TDateTime): Int64;
    function TimeToDouble(const Value: TDateTime): Double;
    function DateToInt(const Value: TDateTime): Integer;
    function TimeStampToInt64(const Value: TDateTime): Int64;
    function TimeStampToDouble(const Value: TDateTime): Double;


    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;
    function OIDToSQLType({$IFNDEF GENERIC_INDEX}var {$ENDIF}ParameterIndex: Integer;
      SQLType: TZSQLType): TZSQLType;
  public
    procedure ClearParameters; override;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;

    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType); override;
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean); override;
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64); override;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); override;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); override;
    procedure SetBlob(ParameterIndex: Integer; const SQLType: TZSQLType; const Value: IZBlob); override;
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant); override;
  end;

  {** EgonHugeist: Implements prepared async SQL Statement based on Protocol 3.0
    ServerVersion 8.3Up and ClientVersion 8.0Up. with C++API usage}
  TZPostgteSQLAsyncCAPIPreparedStatement = class(TZPostgreSQLCAPIPreparedStatement)
  protected
    procedure FlushPendingResults; override;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PZPostgreSQLResult; override;
  protected
    procedure UnPrepareInParameters; override;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  end;

  {** Implements Standard Postgresql Statement.
      Only for compatibility with old dbc-based code
      Uses the 'Classic' prepared statement to be sure it still works with old postgres servers
  }
  TZPostgreSQLStatement = class(TZPostgreSQLClassicPreparedStatement);

  {** Implements callable Postgresql Statement. }
  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement)
  private
    FOidAsBlob, fServerCursor: Boolean;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FUndefinedVarcharAsStringLength: Integer;
    FConnectionHandle: PZPostgreSQLConnect;
    function GetProcedureSql: string;
    function FillParams(const ASql: String): RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
    procedure TrimInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils,
  ZEncoding, ZDbcProperties, ZTokenizer;


var
  PGPreparableTokens: TPreparablePrefixTokens;
const
  ParamFormatBin = 1;
  ParamFormatStr = 0;

function IfThen(Condition: Boolean; const TrueVal, FalseVal: RawByteString): RawByteString; overload;
begin
  if Condition
  then Result := TrueVal
  else Result := FalseVal;
end;

{ TZPostgreSQLPreparedStatement }

{**
  Creates a result set based on the current settings.
  @param QueryHandle the Postgres query handle
  @return a created result set object.
}
constructor TZPostgreSQLPreparedStatement.Create(
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPostgreSQLConnection := Connection;
  FOidAsBlob := StrToBoolEx(Self.Info.Values[DSProps_OidAsBlob]) or Connection.IsOidAsBlob;
  FPlainDriver := Connection.GetPlainDriver;
  //ResultSetType := rtScrollInsensitive;
  FConnectionHandle := Connection.GetConnectionHandle;
  Findeterminate_datatype := False;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'));
  { see http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
    the pgBouncer does not support the RealPrepareds.... }
  FUseEmulatedStmtsOnly := not Assigned(FplainDriver.PQexecParams) or not Assigned(FplainDriver.PQexecPrepared) or
    StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_EmulatePrepares, 'FALSE'));
  Finteger_datetimes := Connection.integer_datetimes;

  fPrepareCnt := 0;
end;

constructor TZPostgreSQLPreparedStatement.Create(
  const Connection: IZPostgreSQLConnection; Info: TStrings);
begin
  Create(Connection, SQL, Info);
end;

function TZPostgreSQLPreparedStatement.GetLastQueryHandle: PZPostgreSQLResult;
begin
  Result := QueryHandle;
end;

function TZPostgreSQLPreparedStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, C, N: Integer;
  Temp: RawByteString;
  Tokens: TZTokenDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens;
  P: PChar;
  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    FIsParamIndex[High(FIsParamIndex)] := Param;
    ToBuff(Value, Result);
  end;
begin
  Result := '';
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    if ((ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0) or
        (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('$', SQL) > 0)) then begin
      Tokens := Connection.GetDriver.GetTokenizer.TokenizeBuffer(SQL, [toSkipEOF]);
      ComparePrefixTokens := PGPreparableTokens;
      Temp := '';
      N := -1;
      FTokenMatchIndex := -1;
      FParamsCnt := 0;
      for I := 0 to High(Tokens) do begin
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Tokens[I].TokenType = ttWord) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens) do
              if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then begin
                if Length(ComparePrefixTokens[C].ChildMatches) = 0 then begin
                  FTokenMatchIndex := C;
                  ComparePrefixTokens := nil;
                end else
                  N := C; //save group
                Break;
              end;
            if N = -1 then //no sub-tokens ?
              ComparePrefixTokens := nil; //stop compare sequence
          end else begin //we already got a group
            FTokenMatchIndex := -1;
            for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
              if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then begin
                FTokenMatchIndex := N;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
        P := Pointer(Tokens[I].Value);
        if (P^ = '?') or ((Tokens[I].TokenType = ttWord) and (P^ = '$') and
           ({$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(P+1, -1) <> -1)) then begin
          Add(Temp);
          Add({$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[I].Value), True);
          Temp := '';
          Inc(FParamsCnt);
        end else case (Tokens[i].TokenType) of
          ttQuoted, ttComment,
          ttWord, ttQuotedIdentifier, ttKeyword:
            Temp := Temp + ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
          else
            Temp := Temp + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[i].Value);
        end;
      end;
      if (Temp <> '') then
        Add(Temp);
    end else
      Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
    FlushBuff(Result);
  end else
    Result := ASQL;
end;

function TZPostgreSQLPreparedStatement.CreateResultSet(
  QueryHandle: PZPostgreSQLResult; ServerCursor: Boolean): IZResultSet;
var
  NativeResultSet: TZAbstractPostgreSQLStringResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  {if fServerCursor
  then NativeResultSet := TZServerCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, FConnectionHandle,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else} NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, FConnectionHandle,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);

  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  if Result <> nil then
    FOpenResultSet := Pointer(Result);
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a RawByteString SQL statement.
}
function TZPostgreSQLPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then begin
      if ParamIndex > InParamCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} then
        raise EZSQLException.Create(SInvalidInputParameterCount)
      else
        ToBuff(PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
        ClientVarManager, FPostgreSQLConnection, ChunkSize,
          InParamTypes[ParamIndex], FOidAsBlob, True, False, ConSettings), Result);
      Inc(ParamIndex);
    end else
      ToBuff(CachedQueryRaw[i], Result);
  FlushBuff(Result);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZPostgreSQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  Prepare;
  PrepareOpenResultSetForReUse;
  if Prepared then
    if FUseEmulatedStmtsOnly or (Findeterminate_datatype and not Assigned(FplainDriver.PQexecParams)) then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else begin
      BindInParameters;
      if not Findeterminate_datatype
      then QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt)
      else QueryHandle := ExecuteInternal(fParamSQL, eicExeParam);
    end
  else QueryHandle := ExecuteInternal(ASQL, eicExecute);
  if QueryHandle <> nil then
    if Assigned(FOpenResultSet) then
      Result := IZResultSet(FOpenResultSet)
    else
      Result := CreateResultSet(QueryHandle, fServerCursor)
  else
    Result := nil;
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
function TZPostgreSQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  Prepare;
  if Prepared then
    if FUseEmulatedStmtsOnly or (Findeterminate_datatype and not Assigned(FplainDriver.PQexecParams)) then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else begin
      BindInParameters;
      if not Findeterminate_datatype
      then QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt)
      else QueryHandle := ExecuteInternal(fParamSQL, eicExeParam);
    end
  else QueryHandle := ExecuteInternal(ASQL, eicExecute);

  if QueryHandle <> nil then begin
    Result := RawToIntDef(FPlainDriver.PQcmdTuples(QueryHandle), 0);
    FPlainDriver.PQclear(QueryHandle);
    FlushPendingResults;
  end;

  inherited ExecuteUpdatePrepared;
end;

procedure TZPostgreSQLPreparedStatement.FlushPendingResults;
begin

end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZPostgreSQLPreparedStatement.ExecutePrepared: Boolean;
var
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  if Prepared then
    if FUseEmulatedStmtsOnly or (Findeterminate_datatype and not Assigned(FplainDriver.PQexecParams)) then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else begin
      BindInParameters;
      if not Findeterminate_datatype
      then QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt)
      else QueryHandle := ExecuteInternal(fParamSQL, eicExeParam);
    end
  else QueryHandle := ExecuteInternal(ASQL, eicExecute);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.PQresultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        if not Assigned(LastResultSet) then
          LastResultSet := CreateResultSet(QueryHandle, fServerCursor);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.PQcmdTuples(QueryHandle), 0);
        FPlainDriver.PQclear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.PQcmdTuples(QueryHandle), 0);
        FPlainDriver.PQclear(QueryHandle);
      end;
  end;

  inherited ExecutePrepared;
end;

procedure TZPostgreSQLPreparedStatement.Prepare;
var
  N, I: Integer;
begin
  if not Prepared then
  begin
    Inc(fPrepareCnt);
    FRawPlanName := IntToRaw(FStatementId)+IntToRaw({%H-}NativeUInt(FConnectionHandle))+IntToRaw(fPrepareCnt);
    fParamSQL := '';
    ToBuff(GetPrepareSQLPrefix, fParamSQL);
    N := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Inc(N);
        ToBuff('$', fParamSQL);
        ToBuff(IntToRaw(N), fParamSQL);
      end else
        ToBuff(CachedQueryRaw[i], fParamSQL);
    FlushBuff(fParamSQL);
    if (not FUseEmulatedStmtsOnly) and (TokenMatchIndex <> -1) then //detected after tokenizing the query
      QueryHandle := ExecuteInternal(fParamSQL + GetPrepareSQLPrefix, eicPrepStmt)
    else
      Findeterminate_datatype := True;
    inherited Prepare; //we need this step always for Set(A/W)SQL overloads if SQL changes
  end;
end;

function TZPostgreSQLPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := PGPreparableTokens;
end;

procedure TZPostgreSQLPreparedStatement.Unprepare;
begin
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) then begin
    if not Findeterminate_datatype
    then FPostgreSQLConnection.UnregisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
  end;
  inherited Unprepare;
end;

{ TZPostgreSQLClassicPreparedStatement }

function TZPostgreSQLClassicPreparedStatement.GetAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then
    begin
      Result := Result + PrepareAnsiSQLParam(ParamIndex, True);
      Inc(ParamIndex);
    end
    else
      Result := Result + CachedQueryRaw[i];
end;

function TZPostgreSQLClassicPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  Category: TEICategory): PZPostgreSQLResult;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(SQL));
        try
          Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
            FConnectionHandle, lcPrepStmt, ASQL, Result) = '42P18');
        except
          Unprepare; //free cached query tokens
          raise;     // handle exception
        end;
        if not Findeterminate_datatype then
        begin
          FPlainDriver.PQclear(Result);
          FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
        end;
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(FExecSQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
      begin
        Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end
      else Result := nil;
    else
      begin
        Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

function TZPostgreSQLClassicPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer;
  Escaped: Boolean): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), ChunkSize, InParamTypes[ParamIndex],
      FOidAsBlob, Escaped, True, ConSettings);
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLClassicPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount > 0 then
    if Prepared then
    begin
      FExecSQL := 'EXECUTE "'+FRawPlanName+'"(';
      for i := 0 to InParamCount -1 do
        if I = 0 then
          FExecSQL := FExecSQL+PrepareAnsiSQLParam(i, False)
        else
          FExecSQL := FExecSQL+','+PrepareAnsiSQLParam(i, False);
      FExecSQL := FExecSQL+');';
    end
    else
      FExecSQL := GetAnsiSQLQuery
  else
    FExecSQL := ASQL;
  inherited BindInParameters;
end;

function TZPostgreSQLClassicPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'"';
end;

function TZPostgreSQLClassicPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := 'PREPARE "'+FRawPlanName+'" AS ';
end;

{ TZPostgreSQLCAPIPreparedStatement }

procedure TZPostgreSQLCAPIPreparedStatement.BindStr(ParameterIndex: Integer;
  SQLType: TZSQLType; Const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex+1 >= InParamCount
  then SetInParamCount(ParameterIndex+1);

  FInParamTypes[ParameterIndex] := SQLType;
  FPQparamBuffs[ParameterIndex] := Value;
  FPQparamLengths[ParameterIndex] := Length(Value); //EH: i know PG ignores this..
  if Value <> ''
  then FPQparamValues[ParameterIndex] := Pointer(FPQparamBuffs[ParameterIndex])
  else FPQparamValues[ParameterIndex] := PEmptyAnsiString;
  FPQparamFormats[ParameterIndex] := ParamFormatStr;
end;

procedure TZPostgreSQLCAPIPreparedStatement.ClearParameters;
begin
  //Do nothing here
end;

function TZPostgreSQLCAPIPreparedStatement.DateToInt(
  const Value: TDateTime): Integer;
var y,m,d: Word;
begin
  DecodeDate(Value, y,m,d);
  Result := date2j(y,m,d) - POSTGRES_EPOCH_JDATE;
end;

function TZPostgreSQLCAPIPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  Category: TEICategory): PZPostgreSQLResult;
var
  PError: PAnsiChar;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.PQprepare(FConnectionHandle, Pointer(FRawPlanName),
          Pointer(SQL), InParamCount, Pointer(fParamOIDs));
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(FConnectionHandle);
        if (PError <> nil) and (PError^ <> #0) then
          { check for indermine datatype error}
          if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
            CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
              lcExecPrepStmt, ASQL, Result)
          else begin
            Findeterminate_datatype := True;
            Exit;
          end
        else begin
          FPlainDriver.PQclear(Result);
          Result := nil;
        end;
        FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.PQexecPrepared(FConnectionHandle,
          Pointer(FRawPlanName), InParamCount, Pointer(FPQparamValues),
          Pointer(FPQparamLengths), Pointer(FPQparamFormats), 0);
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecPrepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
        begin
          Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(SQL));
          CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
            lcUnprepStmt, ASQL, Result);
        end
      else Result := nil;
    eicExeParam: begin
        Result := FPlainDriver.PQexecParams(FConnectionHandle, Pointer(fParamSQL),
          InParamCount, Pointer(fParamOIDs), Pointer(FPQparamValues),
          Pointer(FPQparamLengths), Pointer(FPQparamFormats), 0);
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end;
    else
      begin
        Result := FPlainDriver.PQExec(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
  WriteTempBlob: IZPostgreSQLOidBlob;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then begin
      if ParamIndex > InParamCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} then
        raise EZSQLException.Create(SInvalidInputParameterCount)
      else case InParamTypes[ParamIndex] of
        stUnknown: ToBuff('null', Result);
        stAsciiStream, stUnicodeStream:
          if (FLobs[ParamIndex] = nil) or FLobs[ParamIndex].IsEmpty then
            ToBuff('null', Result)
          else if FLobs[ParamIndex].IsClob then begin
            FPQparamValues[ParamIndex] := FLobs[ParamIndex].GetPAnsiChar(ConSettings^.ClientCodePage.CP);
            ToBuff(FPostgreSQLConnection.EscapeString(FPQparamValues[ParamIndex], FLobs[ParamIndex].Length, True), Result);
          end else
            ToBuff(FPostgreSQLConnection.EscapeString(GetValidatedAnsiStringFromBuffer(FLobs[ParamIndex].GetBuffer,
                  FLobs[ParamIndex].Length, ConSettings)), Result);
        stBinaryStream:
            if (FLobs[ParamIndex] = nil) or FLobs[ParamIndex].IsEmpty
            then ToBuff('null', Result)
            else if FOIDAsBlob then
              try
                WriteTempBlob := TZPostgreSQLOidBlob.Create(
                  TZPostgreSQLPlainDriver(Connection.GetIZPlainDriver.GetInstance),
                  nil, 0, Self.FConnectionHandle, 0, ChunkSize);
                WriteTempBlob.WriteBuffer(FLobs[ParamIndex].GetBuffer, FLobs[ParamIndex].Length);
                ToBuff(IntToRaw(WriteTempBlob.GetBlobOid), Result);
              finally
                WriteTempBlob := nil;
              end
            else ToBuff(FPostgreSQLConnection.EncodeBinary(FLobs[ParamIndex].GetBuffer, FLobs[ParamIndex].Length, True), Result);
        else if FPQparamValues[ParamIndex] = nil then
          if FDefaultValues[ParamIndex] = ''
          then ToBuff('null', Result)
          else ToBuff(FDefaultValues[ParamIndex], Result)
        else ToBuff(FPQparamBuffs[ParamIndex], Result); //normal case already escaped in any kind
      end;
      Inc(ParamIndex);
    end else
      ToBuff(CachedQueryRaw[I], Result);
  FlushBuff(Result);
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.PrepareInParameters;
var
  res: PZPostgreSQLResult;
  I: Integer;
begin
  fHasOIDLobs := False;
  if FUseEmulatedStmtsOnly then
    Exit;
  if not (Findeterminate_datatype) and (CountOfQueryParams > 0) then begin
    if Assigned(FPlainDriver.PQdescribePrepared) then begin
      res := FPlainDriver.PQdescribePrepared(FConnectionHandle, Pointer(FRawPlanname));
      try
        fnParams := FplainDriver.PQnparams(res);
        if (InParamCount > 0) and (fnParams <> InParamCount) then
          raise EZSQLException.Create(SInvalidInputParameterCount)
        else begin
          if (InParamCount = 0) and (fnParams > 0) then
            InternalSetInParamCount(fnParams);
          for i := 0 to InParamCount-1 do
            FParamOIDs[i] := FplainDriver.PQparamtype(res, i);
        end;
      finally
        FPlainDriver.PQclear(res);
      end;
    end else
      for i := 0 to InParamCount-1 do
        SQLTypeToPostgreSQL(InParamTypes[i], fOIDAsBlob, FParamOIDs[i]);
    //for i := 0 to InParamCount -1 do
      //SetLength(FPQparamBuffs[i], Max(Length(FPQparamBuffs[i]),GetOIDbufferSize(FparamOIDs[i], FPQparamFormats[i])));
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetAnsiString(
  ParameterIndex: Integer; const Value: AnsiString);
begin
  SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZAnsiToRaw(Value, ConSettings^.ClientCodePage.CP));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetBlob(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value: IZBlob);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex+1 >= InParamCount then
  SetInParamCount(ParameterIndex+1);
  FLobs[ParameterIndex] := Value;
  FInParamTypes[ParameterIndex] := SQLType;
  if Value.IsEmpty then
    FPQparamValues[ParameterIndex] := nil
  else if SQLType = stBinaryStream then begin
    fHasOIDLobs := FOidAsBlob;
    if not fHasOIDLobs then begin
      FPQparamValues[ParameterIndex] := Value.GetBuffer;
      FPQparamLengths[ParameterIndex]:= Value.Length;
      FPQparamFormats[ParameterIndex]:= ParamFormatBin;
    end;
  end else if Value.IsClob then begin
    FPQparamValues[ParameterIndex] := Value.GetPAnsiChar(ConSettings^.ClientCodePage.CP);
    FPQparamLengths[ParameterIndex]:= Value.Length;
    FPQparamFormats[ParameterIndex]:= ParamFormatStr;
  end else
    BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType, GetValidatedAnsiStringFromBuffer(Value.GetBuffer,
      Value.Length, ConSettings));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  InternalSetOrdinal(ParameterIndex, stBoolean, Ord(Value));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  if (OIDToSQLType(ParameterIndex, stGUID) = stGUID) and (Length(Value) = 16)
  then if not FUseEmulatedStmtsOnly
    then BindBin(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stGUID, Pointer(Value), Length(Value))
    else BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stGUID, #39+GUIDToRaw(Value)+#39)
  else if not FUseEmulatedStmtsOnly
    then BindBin(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBytes, Pointer(Value), Length(Value))
    else BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBytes, FPostgreSQLConnection.EncodeBinary(Value, True));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
var UniTemp: ZWideString;
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex > InParamCount-1
  then SetInParamCount(ParameterIndex+1);

  FInParamTypes[ParameterIndex] := stString;
  if (Value.Len = 0) then begin
    if not FUseEmulatedStmtsOnly
    then begin
      FPQparamValues[ParameterIndex] := PEmptyAnsiString;
      FPQparamLengths[ParameterIndex] := 0;
    end else FPQparamBuffs[ParameterIndex] := #39#39;
  end else if ZCompatibleCodePages(Value.CP, ConSettings^.ClientCodePage.CP) then
    if not FUseEmulatedStmtsOnly then begin
      FPQparamValues[ParameterIndex] := Value.P;
      FPQparamLengths[ParameterIndex] := Value.Len;
    end else
      FPQparamBuffs[ParameterIndex] := FPostgreSQLConnection.EscapeString(Value.P, Value.Len, True)
  else begin
    if ZCompatibleCodePages(Value.CP, zCP_UTF16) then begin
      FInParamTypes[ParameterIndex] := stUnicodeString;
      FPQparamBuffs[ParameterIndex] := PUnicodeToRaw(Value.P, Value.Len, ConSettings^.ClientCodePage.CP)
    end else begin
      UniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
      FPQparamBuffs[ParameterIndex] := ZUnicodeToRaw(UniTemp, ConSettings^.ClientCodePage.CP)
    end;
    if not FUseEmulatedStmtsOnly then begin
      FPQparamLengths[ParameterIndex] := Length(FPQparamBuffs[ParameterIndex]);
      FPQparamValues[ParameterIndex] := Pointer(FPQparamBuffs[ParameterIndex]);
    end else FPQparamBuffs[ParameterIndex] := FPostgreSQLConnection.EscapeString(FPQparamValues[ParameterIndex], FPQparamLengths[ParameterIndex], True);
  end;
 if FUseEmulatedStmtsOnly then begin //indicate not null
   FPQparamLengths[ParameterIndex] := Length(FPQparamBuffs[ParameterIndex]);
   FPQparamValues[ParameterIndex] := Pointer(FPQparamBuffs[ParameterIndex]);
 end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetDefaultValue(
  ParameterIndex: Integer; const Value: string);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex > InParamCount-1
  then SetInParamCount(ParameterIndex+1);
  FDefaultValues[ParameterIndex] := ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetGUID(ParameterIndex: Integer;
  const Value: TGUID);
begin
  case OIDToSQLType(ParameterIndex, stGUID) of
    stGUID: BindBin(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stGUID, @Value.D1, SizeOf(TGUID));
    else if not FUseEmulatedStmtsOnly
      then BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stGUID, GUIDToRaw(Value))
      else BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stGUID, #39+GUIDToRaw(Value)+#39)
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetInParamCount(
  const NewParamCount: Integer);
begin
  if not Prepared then
    Prepare;
  if Findeterminate_datatype or FUseEmulatedStmtsOnly then begin
    Assert(NewParamCount <= CountOfQueryParams);
    InternalSetInParamCount(NewParamCount);
  end else
    Assert(NewParamCount <= FInParamCount); //<- done by PrepareInParams
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex > InParamCount-1
  then SetInParamCount(ParameterIndex+1);

  FInParamTypes[ParameterIndex] := SQLType;
  FPQparamValues[ParameterIndex] := nil;
  if (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
    fLobs[ParameterIndex] := nil;
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
begin
  if not FUseEmulatedStmtsOnly
  then BindStr(ParameterIndex, stString, Value)
  else BindStr(ParameterIndex, stString, FPostgreSQLConnection.EscapeString(Pointer(Value), Length(Value), True));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  InternalSetOrdinal(ParameterIndex, stULong, Value);
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  SetRawByteString(ParameterIndex, ZUnicodeToRaw(Value, ConSettings^.ClientCodePage.CP));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetUTF8String(
  ParameterIndex: Integer; const Value: UTF8String);
begin
  SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZUTF8ToRaw(Value, ConSettings^.ClientCodePage.CP));
end;

procedure TZPostgreSQLCAPIPreparedStatement.SetValue(ParameterIndex: Integer;
  const Value: TZVariant);
var lob: IZBlob;
begin
  case Value.VType of
    vtNull:           SetNull(ParameterIndex, stString);
    vtBoolean:        SetBoolean(ParameterIndex, Value.VBoolean);
    vtInteger:        SetLong(ParameterIndex, Value.VInteger);
    vtUInteger:       SetULong(ParameterIndex, Value.VUInteger);
    vtFloat:          SetDouble(ParameterIndex, Value.VFloat);
    vtBytes:          SetBytes(ParameterIndex, Value.VBytes);
    vtString:         SetString(ParameterIndex, Value.VString);
    vtAnsiString:     SetAnsiString(ParameterIndex, Value.VAnsiString);
    vtUTF8String:     SetUTF8String(ParameterIndex, Value.VUTF8String);
    vtRawByteString:  SetRawByteString(ParameterIndex, Value.VRawByteString);
    vtUnicodeString:  SetUnicodeString(ParameterIndex, Value.VUnicodeString);
    vtDateTime:       SetTimeStamp(ParameterIndex, Value.VDateTime);
    vtInterface:      If Supports(Value.VInterface, IZBlob, lob) then begin
                        if lob.IsClob
                        then SetBlob(ParameterIndex, stAsciiStream, lob)
                        else SetBlob(ParameterIndex, stBinaryStream, lob);
                      end else raise EZVariantException.Create(STypesMismatch);
    vtCharRec:        SetCharRec(ParameterIndex, Value.VCharRec);
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.TimeStampToDouble(
  const Value: TDateTime): Double;
var Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Date: Double; //overflow save multiply
begin
  DecodeDate(Value, Year, Month, Day);
  Date := date2j(Year, Month, Day) - POSTGRES_EPOCH_JDATE;
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := (Hour * MinsPerHour + Min) * SecsPerMin + Sec + Msec / MSecsPerSec; //time2t_in64 did not work?
  Result := Date * SecsPerDay + Result;
end;

function TZPostgreSQLCAPIPreparedStatement.TimeStampToInt64(
  const Value: TDateTime): Int64;
var Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Date: Int64; //overflow save multiply
begin
  DecodeDate(Value, Year, Month, Day);
  Date := date2j(Year, Month, Day) - POSTGRES_EPOCH_JDATE;
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //timestamp do not play with microseconds!!
  Result := ((Hour * MINS_PER_HOUR + Min) * SECS_PER_MINUTE + Sec) * MSecsPerSec + MSec;
  Result := (Date * MSecsPerDay + Result) * MSecsPerSec;
end;

function TZPostgreSQLCAPIPreparedStatement.TimeToDouble(
  const Value: TDateTime): Double;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //macro of datetime.c
  Result := (((hour * MINS_PER_HOUR) + min) * SECS_PER_MINUTE) + sec + Msec
end;

function TZPostgreSQLCAPIPreparedStatement.TimeToInt64(
  const Value: TDateTime): Int64;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //macro of datetime.c
  Result := (((((hour * MINS_PER_HOUR) + min) * SECS_PER_MINUTE) + sec) * USECS_PER_SEC) + Msec
end;

procedure TZPostgreSQLCAPIPreparedStatement.BindBin(ParameterIndex: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex+1 >= InParamCount then
  SetInParamCount(ParameterIndex+1);
  FInParamTypes[ParameterIndex] := SQLType;
  ZSetString(Buf, Len,FPQparamBuffs[ParameterIndex]);
  FPQparamFormats[ParameterIndex] := ParamFormatBin;
  FPQparamLengths[ParameterIndex] := Len;
  if Len > 0
  then FPQparamValues[ParameterIndex] := Pointer(FPQparamBuffs[ParameterIndex])
  else FPQparamValues[ParameterIndex] := PEmptyAnsiString;
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLCAPIPreparedStatement.BindInParameters;
var
  I: Integer;
  WriteTempBlob: IZPostgreSQLOidBlob;
begin
  {EH: after implizit bindings we need to finalize the OID Lobs and the
   default values only }
  if fHasOIDLobs then
    for i := 0 to InParamCount -1 do begin
      if FInParamTypes[i] = stBinaryStream then
        try
          WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
            FConnectionHandle, 0, ChunkSize);
          WriteTempBlob.WriteBuffer(Flobs[i].GetBuffer, Flobs[i].Length);
          BindStr(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBinaryStream, IntToRaw(WriteTempBlob.GetBlobOid));
        finally
          WriteTempBlob := nil;
        end
      //else if (FPQparamValues[I] = nil) and (High(fDefaultValues) >= i) and (fDefaultValues[i] <> '') then //null bound ?
        // BindStr(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, InParamTypes[i], fDefaultValues[i])
    end;
  if DriverManager.HasLoggingListener then
    inherited BindInParameters;
end;

procedure TZPostgreSQLCAPIPreparedStatement.BindNetworkOrderBin(
  ParameterIndex: Integer; SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt);
var PStart: PAnsiChar;
begin
  if ParameterIndex+1 >= FInParamCount then
  SetInParamCount(ParameterIndex+1);
  if Length(FPQparamBuffs[ParameterIndex]) < Len then
    SetLength(FPQparamBuffs[ParameterIndex], Len);
  FPQparamFormats[ParameterIndex] := ParamFormatBin;
  FPQparamLengths[ParameterIndex] := Len;
  PStart := Pointer(FPQparamBuffs[ParameterIndex]);
  FPQparamValues[ParameterIndex] := PStart;
  { reverse host-byte order to network-byte order }
  {$IFNDEF ENDIAN_BIG}
  Inc(PStart, Len-1);
  if Len > 1 then begin
    while Len > 0 do begin
      PStart^ := Buf^;
      dec(PStart);
      Inc(Buf);
      dec(Len);
    end
  end;
  {$ENDIF}
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.UnPrepareInParameters;
begin
  { release allocated memory }
  InternalSetInParamCount(0);
  fnParams := 0;
  fHasOIDLobs := False;
end;

function TZPostgreSQLCAPIPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'";';
end;

function TZPostgreSQLCAPIPreparedStatement.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
begin
  Result := '';
  if FPQparamValues[ParamIndex] = nil then
    Result := '(NULL)'
  else if (FPQparamFormats[ParamIndex] = ParamFormatStr) then
    //by ref?
    if (FPQparamValues[ParamIndex] = Pointer(FPQparamBuffs[ParamIndex])) then
      Result := #39+FPQparamBuffs[ParamIndex]+#39
    else begin
      ZSetString(FPQparamValues[ParamIndex], FPQparamLengths[ParamIndex], Result);
      Result := #39+Result+#39;
    end
  else
    Result := GetSQLHexAnsiString(FPQparamValues[ParamIndex], FPQparamLengths[ParamIndex], False);
end;

function TZPostgreSQLCAPIPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := '';
end;

procedure TZPostgreSQLCAPIPreparedStatement.InternalSetDateTime(
  ParameterIndex: Integer; SQLType: TZSQLType; const Value: TDateTime);
begin
  case OIDToSQLType(ParameterIndex, SQLType) of
    stTime:   begin
                if FPostgreSQLConnection.integer_datetimes
                then PInt64(@FStatBuf[0])^ := TimeToInt64(Value)
                else PDouble(@FStatBuf[0])^ := TimeToDouble(Value);
                BindNetworkOrderBin(ParameterIndex, stTime, @FStatBuf[0], 8);
              end;
    stDate:   begin
                PInteger(@FStatBuf[0])^ := DateToInt(Value);
                BindNetworkOrderBin(ParameterIndex, stDate, @FStatBuf[0], SizeOf(integer));
              end;
    stTimeStamp: begin
                if FPostgreSQLConnection.integer_datetimes
                then PInt64(@FStatBuf[0])^ := TimeStampToInt64(Value)
                else PDouble(@FStatBuf[0])^ := TimeStampToDouble(Value);
                BindNetworkOrderBin(ParameterIndex, stTimeStamp, @FStatBuf[0], 8);
              end;
    else case SQLType of
      stTime: BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stTimeStamp, DateTimeToRawSQLTime(Value,
        ConSettings^.WriteFormatSettings, FUseEmulatedStmtsOnly,
          IfThen(FUseEmulatedStmtsOnly,'::time','')));
      stDate: BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stTimeStamp, DateTimeToRawSQLDate(Value,
        ConSettings^.WriteFormatSettings, FUseEmulatedStmtsOnly,
          IfThen(FUseEmulatedStmtsOnly,'::date','')));
      stTimestamp: BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stTimeStamp, DateTimeToRawSQLTimeStamp(Value,
        ConSettings^.WriteFormatSettings, FUseEmulatedStmtsOnly,
          IfThen(FUseEmulatedStmtsOnly,'::timestamp','')));
    end;
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.InternalSetDouble(
  ParameterIndex: Integer; SQLType: TZSQLType; const Value: Double);
begin
  case OIDToSQLType(ParameterIndex, stFloat) of
    stBoolean:  begin
                  PByte(@FStatBuf[0])^ := Ord(Value <> 0);
                  BindBin(ParameterIndex, stBoolean, @FStatBuf[0], SizeOf(Byte));
                end;
    stSmall:    begin
                  PSmallInt(@FStatBuf[0])^ := Trunc(Value);
                  BindNetworkOrderBin(ParameterIndex, stSmall, @FStatBuf[0], SizeOf(SmallInt));
                end;
    stInteger:  begin
                  PInteger(@FStatBuf[0])^ := Trunc(Value);
                  BindNetworkOrderBin(ParameterIndex, stInteger, @FStatBuf[0], SizeOf(Integer));
                end;
    stLongWord: begin
                  PLongword(@FStatBuf[0])^ := Trunc(Value);
                  BindNetworkOrderBin(ParameterIndex, stLongWord, @FStatBuf[0], SizeOf(Longword));
                end;
    stLong:   begin
                PInt64(@FStatBuf[0])^ := Trunc(Value);
                BindNetworkOrderBin(ParameterIndex, stLong, @FStatBuf[0], SizeOf(Int64));
              end;
    stFloat:  begin
                PSingle(@FStatBuf[0])^ := Value;
                BindNetworkOrderBin(ParameterIndex, stFloat, @FStatBuf[0], SizeOf(Single));
              end;
    stDouble: BindNetworkOrderBin(ParameterIndex, stDouble, @Value, SizeOf(Double));
    stCurrency: begin
                PInt64(@FStatBuf[0])^ := Trunc(Value*100);
                BindNetworkOrderBin(ParameterIndex, stLong, @FStatBuf[0], SizeOf(Int64));
              end;
    else BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stFloat, FloatToSqlRaw(Value));
  end;
end;

procedure TZPostgreSQLCAPIPreparedStatement.InternalSetInParamCount(
  NewParamCount: Integer);
begin
  SetLength(FPQparamValues, NewParamCount);
  SetLength(FPQparamLengths, NewParamCount);
  SetLength(FPQparamFormats, NewParamCount);
  SetLength(FParamOIDs, NewParamCount);
  SetLength(FInParamTypes, NewParamCount);
  SetLength(FLobs, NewParamCount);
  SetLength(FDefaultValues, NewParamCount);
  SetLength(FPQparamBuffs, NewParamCount);
  FInParamCount := NewParamCount;
end;

procedure TZPostgreSQLCAPIPreparedStatement.InternalSetOrdinal(
  ParameterIndex: Integer; SQLType: TZSQLType; const Value: Int64);
begin
  case OIDToSQLType(ParameterIndex, stLong) of
    stBoolean:  begin
                  PByte(@FStatBuf[0])^ := Ord(Value <> 0);
                  BindBin(ParameterIndex, stBoolean, @FStatBuf[0], SizeOf(Byte));
                end;
    stSmall:    begin
                  PSmallInt(@FStatBuf[0])^ := Value;
                  BindNetworkOrderBin(ParameterIndex, stSmall, @FStatBuf[0], SizeOf(SmallInt));
                end;
    stInteger:  begin
                  PInteger(@FStatBuf[0])^ := Value;
                  BindNetworkOrderBin(ParameterIndex, stInteger, @Value, SizeOf(Integer));
                end;
    stLongWord: begin
                  PLongword(@FStatBuf[0])^ := Value;
                  BindNetworkOrderBin(ParameterIndex, stLongWord, @FStatBuf[0], SizeOf(Longword));
                end;
    stLong,
    stCurrency:  BindNetworkOrderBin(ParameterIndex, stLong, @Value, SizeOf(Int64));
    stFloat:  begin
                PSingle(@FStatBuf[0])^ := Value;
                BindNetworkOrderBin(ParameterIndex, stFloat, @FStatBuf[0], SizeOf(Single));
              end;
    stDouble: begin
                PDouble(@FStatBuf[0])^ := Value;
                BindNetworkOrderBin(ParameterIndex, stDouble, @FStatBuf[0], SizeOf(Double));
              end;
    else BindStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stLong, IntToRaw(Value));
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.OIDToSQLType(
  {$IFNDEF GENERIC_INDEX}var {$ENDIF} ParameterIndex: Integer;
  SQLType: TZSQLType): TZSQLType;
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if ParameterIndex > High(FParamOIDs) then
    SetInParamCount(ParameterIndex+1);
  if not FUseEmulatedStmtsOnly then
    if not Findeterminate_datatype then
      case FParamOIDs[ParameterIndex] of
        INVALIDOID: begin
          SQLTypeToPostgreSQL(SQLType, FOIdAsBLob, FParamOIDs[ParameterIndex]);
          Result := SQLType;
        end;
        { these types are binary supported by now }
        BOOLOID:  Result := stBoolean;
        BYTEAOID: Result := stBytes;
        INT8OID:  Result := stLong;
        INT2OID:  Result := stSmall;
        INT4OID:  Result := stInteger;
        OIDOID:   Result := stLongWord;
        FLOAT4OID:Result := stFloat;
        FLOAT8OID:Result := stDouble;
        CASHOID:  Result := stCurrency;
        DATEOID:  Result := stDate;
        TIMEOID:  Result := stTime;
        TIMESTAMPOID: Result := stTimeStamp;
        UUIDOID:  Result := stGUID;
        else Result := stUnknown;
      end
    else begin
      SQLTypeToPostgreSQL(SQLType, FOIdAsBLob, FParamOIDs[ParameterIndex]);
      Result := SQLType;
    end
  else Result := stUnknown;
end;

{ TZPostgreSQLCallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLCallableStatement.Create(
  const Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  ResultSetType := rtScrollInsensitive;
  with (Connection as IZPostgreSQLConnection) do begin
    FPlainDriver := GetPlainDriver;
    FOidAsBlob := StrToBoolEx(Self.Info.Values[DSProps_OidAsBlob]) or IsOidAsBlob;
    FConnectionHandle := GetConnectionHandle;
  end;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength , '0'));
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLCallableStatement.CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZAbstractPostgreSQLStringResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if fServerCursor then
    NativeResultSet := TZServerCursorPostgreSQLStringResultSet.Create(Self, SQL, FConnectionHandle,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else
    NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, SQL, FConnectionHandle,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLCallableStatement.PrepareAnsiSQLParam(
  ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), ChunkSize, InParamTypes[ParamIndex],
      FOidAsBlob, True, False, ConSettings);
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := nil;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.PQExec(FConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  if QueryHandle <> nil then
  begin
    Result := CreateResultSet(Self.SQL, QueryHandle);
    AssignOutParamValuesFromResultSet(Result, OutParamValues, OutParamCount , FDBParamTypes);
  end
  else
    Result := nil;
end;

{**
  Prepares and executes an SQL statement that returns a single <code>ResultSet</code> object.
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  TrimInParameters;
  Result := ExecuteQuery(FillParams(GetProcedureSql));
end;

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZPostgreSQLCallableStatement.GetProcedureSql: string;
  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: string;
begin
  if Length(CachedQueryRaw) = 1 then  //only name in there?
  begin
    Unprepare; //reset cached query
    InParams := GenerateParamsStr(InParamCount);
    Result := Format('SELECT * FROM %s(%s)', [SQL, InParams]);
    {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := Result; //sets the cached queries again
  end;
end;

{**
   Fills the parameter (?) tokens with corresponding parameter value
   @return a prepared SQL query for execution
}
function TZPostgreSQLCallableStatement.FillParams(const ASql: String): RawByteString;
var I: Integer;
  ParamIndex: Integer;
begin
  if InParamCount > 0 then
  begin
    Result := '';
    ParamIndex := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Result := Result + PrepareAnsiSQLParam(ParamIndex);
        Inc(ParamIndex);
      end
      else
        Result := Result + CachedQueryRaw[i];
  end
  else
    Result := GetRawEncodedSQL(ASql);
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
function TZPostgreSQLCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := -1;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.PQExec(FConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(FPlainDriver.PQcmdTuples(QueryHandle), 0);
    AssignOutParamValuesFromResultSet(CreateResultSet(Self.SQL, QueryHandle),
      OutParamValues, OutParamCount , FDBParamTypes);
  end;
end;


function TZPostgreSQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  TrimInParameters;
  Result := Self.ExecuteUpdate(FillParams(GetProcedureSql));
end;

{**
   Function removes ptResult, ptOutput parameters from
   InParamTypes and InParamValues
}
procedure TZPostgreSQLCallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  for I := 0 to High(InParamTypes) do
  begin
    if (Self.FDBParamTypes[i] in [2, 4]) then //[ptResult, ptOutput]
      Continue;
    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;

  if ParamCount = InParamCount then
    Exit;

  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount);
end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stBinaryStream, stUnicodeStream]);
end;

{ TZPostgteSQLAsyncCAPIPreparedStatement }

function TZPostgteSQLAsyncCAPIPreparedStatement.ExecuteInternal(
  const SQL: RawByteString; Category: TEICategory): PZPostgreSQLResult;
var
  PError: PAnsichar;
  I: Integer;
label retry;
begin
  Result := nil;
  case Category of
    eicPrepStmt: begin
retry:
        Result := FPlainDriver.PQprepare(FConnectionHandle, Pointer(FRawPlanName),
          Pointer(SQL), InParamCount, Pointer(fParamOIDs));
        PError := FPlainDriver.PQerrorMessage(FConnectionHandle);
        if (PError <> nil) and (PError^ <> #0) and (Pointer(fParamOIDs) = nil) then
          { check for indermine datatype error}
          if ZSysUtils.ZMemLComp(PError, PAnsichar('42P18'), 5) <> 0 then
            CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
              lcExecPrepStmt, ASQL, Result)
          else begin
            FPlainDriver.PQclear(Result);
            { todo:
              help postgre by setting types to the parameter markers like :$1::timestamp
              or predetermine the postgre OID's see Tip of:
              https://www.postgresql.org/docs/9.1/static/libpq-exec.html }
            if (Pointer(fParamOIDs) = nil) then begin
              SetLength(FParamOIDs, InParamCount);
              for i := 0 to InParamCount -1 do
                SQLTypeToPostgreSQL(InParamTypes[i], FOidAsBlob, FParamOIDs[i]);
              goto retry;
            end else
              Findeterminate_datatype := True;
          end
        else begin
          FPlainDriver.PQclear(Result);
          Result := nil;
        end;
      end;
    eicExecPrepStmt:
      if FPlainDriver.PQsendQueryPrepared(FConnectionHandle,
        Pointer(FRawPlanName), InParamCount, Pointer(FPQparamValues),
        Pointer(FPQparamLengths), Pointer(FPQparamFormats), 0) = Ord(PGRES_COMMAND_OK) then begin
        if FServerCursor then
          FPlainDriver.PQsetSingleRowMode(FConnectionHandle);
        Result := FPlainDriver.PQgetResult(FConnectionHandle);
      end else
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecPrepStmt, ASQL, nil);
    eicUnprepStmt:
      if FPlainDriver.PQSendQuery(FConnectionHandle, Pointer(SQL)) = Ord(PGRES_COMMAND_OK)
      then Result := FPlainDriver.PQgetResult(FConnectionHandle)
      else CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, nil);
    else if FPlainDriver.PQSendQuery(FConnectionHandle, Pointer(SQL)) = Ord(PGRES_COMMAND_OK) then begin
      if FServerCursor then
        FPlainDriver.PQsetSingleRowMode(FConnectionHandle);
      Result := FPlainDriver.PQgetResult(FConnectionHandle); //just get first result all other are pending
      CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
        lcExecute, ASQL, Result);
    end else
      CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
        lcExecute, ASQL, nil);
  end;
end;

procedure TZPostgteSQLAsyncCAPIPreparedStatement.FlushPendingResults;
var PQRes: PZPostgreSQLResult;
begin
  while True do begin
    PQRes := FPlainDriver.PQgetResult(FConnectionHandle);
    if PQRes = nil
    then break
    else FplainDriver.PQclear(PQRes);
  end;
end;

procedure TZPostgteSQLAsyncCAPIPreparedStatement.Prepare;
begin
  FlushPendingResults;
  inherited Prepare;
end;

procedure TZPostgteSQLAsyncCAPIPreparedStatement.Unprepare;
begin
  inherited UnPrepare;
  FlushPendingResults;
end;

procedure TZPostgteSQLAsyncCAPIPreparedStatement.UnPrepareInParameters;
begin
  inherited UnPrepareInParameters;
  SetLength(FParamOIDs, 0);
end;

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
SetLength(PGPreparableTokens, 5);
PGPreparableTokens[0].MatchingGroup := 'SELECT';
PGPreparableTokens[1].MatchingGroup := 'INSERT';
PGPreparableTokens[2].MatchingGroup := 'UPDATE';
PGPreparableTokens[3].MatchingGroup := 'DELETE';
PGPreparableTokens[4].MatchingGroup := 'VALUES';

end.

