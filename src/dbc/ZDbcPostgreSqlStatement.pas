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
  TEICategory = (
    eicExecute, eicExeParamV3, eicExeParamV3_Async,
    eicPrepStmtV2, eicPrepStmtV3, eicPrepStmtV3_Async,
    eicExecPrepStmtV2, eicExecPrepStmtV3, eicExecPrepStmtV3_Async,
    eicUnprepStmtV2, eicUnprepStmtV3, eicUnprepStmtV3_Async);
  TPGExecMode = (pgemV2, pgemV3, pgemV3_Async);
const
  TEIPQExecuteModes: array[TPGExecMode] of TEICategory = (
    eicExecute, eicExeParamV3, eicExeParamV3_Async);
  TEIPQPrepareModes: array[TPGExecMode] of TEICategory = (
    eicPrepStmtV2, eicPrepStmtV3, eicPrepStmtV3_Async);
  TEIPQExecPreparedModes: array[TPGExecMode] of TEICategory = (
    eicExecPrepStmtV2, eicExecPrepStmtV3, eicExecPrepStmtV3_Async);
  TEIPQUnPrepareModes: array[TPGExecMode] of TEICategory = (
    eicUnprepStmtV2, eicUnprepStmtV3, eicUnprepStmtV3_Async);
  //PGMaxParams = 32767 shr 4;
  PGMaxParams = 5000;
type
  {** PostgreSQL Prepared SQL statement interface. }
  IZPGSQLPreparedStatement = interface(IZPreparedStatement)
    ['{EED35CAA-8F36-4639-8B67-32DF237E8F6F}']
    function GetLastQueryHandle: PPGresult;
  end;

  TArrayDMLType = (dmlInsert = 1, dmlUpdate, dmlDelete);

  TZPostgreSQLPreparedStatement = class(TImplizitBindRealAndEmulationStatement_A,
    IZPGSQLPreparedStatement)
  private
    FPostgreSQLConnection: IZPostgreSQLConnection;
    FPlainDriver: TZPostgreSQLPlainDriver; //PG API
    Fconn: TPGconn; //the Connection-Handle
    QueryHandle: PPGresult; //Current query handle we'd obtained
    FRawPlanName: RawByteString; //a name we use to prepare (oddly PG still has no handle instead)
    FOidAsBlob: Boolean; //are blob's threaded as oid-lobs?
    Findeterminate_datatype, //did PG Fail to determine the datatypes? (mostly just because of bd queries)
    fAsyncQueries, //get the GetMoreResults logic with multiple results or SingleRowMode running
    fServerCursor, //PQsingleRowMode? is implizit the Async api
    fPQParamsFoundInQuery, //did the user compose it's query with the non standart §n marks already? -> skip replacing the question marks
    FUseEmulatedStmtsOnly: Boolean; //no Prepareds?
    FUndefinedVarcharAsStringLength: Integer; //compatibility: in earlier version of Zeos the fieldlength for VARCHAR without given length was 255 now we assume text instead
    fPrepareCnt: Cardinal; //we have no stmt handle! The same object might be reused with another SQL
                           //and the previous used planname did run into the oddly broken transaction issue of pg
                           //we need a secondary uniqueness idea for the planname as a fallback
    Finteger_datetimes: Boolean;  //what's the binary format to bind datetimes? Integer or Double
    FPQparamValues: TPQparamValues; //usually just for binary bindings used PG up until today loves the StrLen() instead of given lengtes for the strings (they can't store #0 bytes)
    FPQparamLengths: TPQparamLengths; //EH: i know PG ignores this for str vals but we'll keep it for by ref logging or pqexecparams
    FPQparamFormats: TPQparamFormats; //indicate binary or string format for the bindings
    FParamOIDs: TPQparamTypes; //the Parameter OID's
    FPGExecMode: TPGExecMode; //the protocol mode + user wanted mode with minimum version of protocol V3
    FPQResultFormat: Integer; //which format will the tubles have binary or string? note if any unsupported oid  given we use string format only as a fallback
    function CheckPrepareSwitchMode: Boolean;
    procedure InternalRealPrepare;
    procedure AllocBuf(Index: Integer; Len: LengthInt; ParamFormat: Integer);
    function OIDToSQLType(Index: Integer; SQLType: TZSQLType): TZSQLType;
    function ExecuteDMLBatchWithUnnest: PPGresult;
    function ExecuteDMLBatchWithUnnestVarlenaArrays: PPGresult;
  protected
    procedure BindNull(Index: Integer; var SQLType: TZSQLType); override;
    procedure BindRawStr(Index: Integer; var SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; var SQLType: TZSQLType; const Buf: RawByteString); override;
    procedure BindBinary(Index: Integer; var SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindSignedOrdinal(Index: Integer; var SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; var SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindDouble(Index: Integer; var SQLType: TZSQLType; const Value: Double); override;
    procedure BindDateTime(Index: Integer; var SQLType: TZSQLType; const Value: TDateTime); override;

    function GetBoundValueAsLogValue(Index: Integer): RawByteString; override;
    function DateTimeAsString(const Value: TDateTime; SQLType: TZSQLType): RawByteString; override;
    function BoolAsString(Value: Boolean): RawByteString; override;
  protected
    procedure InternalSetInParamCount(NewParamCount: Integer); override;
    procedure FlushPendingResults;
    function CreateResultSet(QueryHandle: PPGresult; ServerCursor: Boolean): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PPGresult; virtual;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; override;
  public
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings); overload;
    constructor Create(const Connection: IZPostgreSQLConnection;
      Info: TStrings); overload;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    function GetLastQueryHandle: PPGresult;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
  end;

  TZPostgreSQLClassicPreparedStatement = class(TZPostgreSQLPreparedStatement);
  TZPostgreSQLCAPIPreparedStatement = class(TZPostgreSQLPreparedStatement);
  TZPostgreSQLStatement = class(TZPostgreSQLClassicPreparedStatement);

  {** Implements callable Postgresql Statement. }
  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement)
  private
    FOidAsBlob, fServerCursor: Boolean;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FUndefinedVarcharAsStringLength: Integer;
    Fconn: TPGconn;
    function GetProcedureSql: string;
    function FillParams(const ASql: String): RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    function CreateResultSet(const SQL: string;
      QueryHandle: PPGresult): IZResultSet;
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
  ZEncoding, ZDbcProperties, ZTokenizer, Types, ZDbcResultSet;


var
  PGPreparableTokens: TPreparablePrefixTokens;
const
  ParamFormatBin = 1;
  ParamFormatStr = 0;

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
  Fconn := Connection.GetConnectionHandle;
  Findeterminate_datatype := False;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'));
  { see http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
    the pgBouncer does not support the RealPrepareds.... }
  FUseEmulatedStmtsOnly := not Assigned(FplainDriver.PQexecParams) or not Assigned(FplainDriver.PQexecPrepared) or
    StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_EmulatePrepares, 'FALSE'));
  Finteger_datetimes := Connection.integer_datetimes;
  FPQResultFormat := ParamFormatStr;
  fPrepareCnt := 0;
  if FUseEmulatedStmtsOnly then
    FMinExecCount2Prepare := -1;
  if Connection.GetServerMajorVersion >= 8 then begin
    FEmulatedParams := False; //indicate we bind to buffers and use PQexecParams
    fAsyncQueries := False;(* not ready yet! StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_ExexAsync, 'FALSE'))
      and Assigned(FplainDriver.PQsendQuery) and Assigned(FplainDriver.PQsendQueryParams) and
      Assigned(FplainDriver.PQsendQueryPrepared);*)
    if fAsyncQueries
    then FPGExecMode := pgemV3_Async
    else FPGExecMode := pgemV3;
    fServerCursor := fAsyncQueries and StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_SingleRowMode, 'FALSE'))
  end else
    FEmulatedParams := True;
end;

procedure TZPostgreSQLPreparedStatement.AllocBuf(Index: Integer;
  Len: LengthInt; ParamFormat: Integer);
begin
  FPQparamLengths[Index] := Len;
  if Length(FParamValues[Index]) < Len then
    ZSetString(nil, Len, FParamValues[Index]);
  FPQparamValues[Index] := Pointer(FParamValues[Index]);
  FPQparamFormats[Index] := ParamFormat;
end;

procedure TZPostgreSQLPreparedStatement.BindBinary(Index: Integer;
  var SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
begin
  if fPGExecMode <> pgemV2 then begin
    FPQparamLengths[Index] := Len;
    FPQparamFormats[Index] := ParamFormatBin;
    if SQLType = stBinaryStream then
      FPQparamValues[Index] := Buf
    else if Len > 0 then begin
      if Length(FParamValues[Index]) < Len
      then ZSetString(Buf, Len, FParamValues[Index])
      else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Pointer(FParamValues[Index])^, Len);
      FPQparamValues[Index] := Pointer(FParamValues[Index]);
    end else
      FPQparamValues[Index] := PEmptyAnsiString;
  end else
    Connection.GetBinaryEscapeString(Buf, Len, FParamValues[Index]);
end;

procedure TZPostgreSQLPreparedStatement.BindDateTime(Index: Integer;
  var SQLType: TZSQLType; const Value: TDateTime);
begin
  if fPGExecMode <> pgemV2 then begin
    SQLType := OIDToSQLType(Index, SQLType);
    case OIDToSQLType(Index, SQLType) of
      stTime:     begin
                    AllocBuf(Index, 8, ParamFormatBin);
                    if Finteger_datetimes
                    then Time2PG(Value, PInt64(FPQparamValues[Index])^)
                    else Time2PG(Value, PDouble(FPQparamValues[Index])^);
                  end;
      stDate:     begin
                    AllocBuf(Index, SizeOf(Integer), ParamFormatBin);
                    Date2PG(Value, PInteger(FPQparamValues[Index])^);
                  end;
      stTimeStamp:begin
                    AllocBuf(Index, 8, ParamFormatBin);
                    if Finteger_datetimes
                    then DateTime2PG(Value, PInt64(FPQparamValues[Index])^)
                    else DateTime2PG(Value, PDouble(FPQparamValues[Index])^);
                  end;
      else case SQLType of
        stTime: BindRawStr(Index, SQLType, DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False));
        stDate: BindRawStr(Index, SQLType, DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False));
        stTimeStamp: BindRawStr(Index, SQLType, DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, False));
      end;
    end;
  end else
    FParamValues[Index] := inherited DateTimeAsString(Value, SQLType);
end;

procedure TZPostgreSQLPreparedStatement.BindDouble(Index: Integer;
  var SQLType: TZSQLType; const Value: Double);
begin
  if fPGExecMode <> pgemV2 then begin
    SQLType := OIDToSQLType(Index, SQLType);
    case SQLType of
      stBoolean:  begin
                    AllocBuf(Index, SizeOf(Byte), ParamFormatBin);
                    PByte(FPQparamValues[Index])^ := Ord(Value <> 0);
                  end;
      stSmall:    begin
                    AllocBuf(Index, SizeOf(SmallInt), ParamFormatBin);
                    SmallInt2PG(SmallInt(Trunc(Value)), FPQparamValues[Index]);
                  end;
      stInteger:  begin
                    AllocBuf(Index, SizeOf(Integer), ParamFormatBin);
                    Integer2PG(Integer(Trunc(Value)), FPQparamValues[Index]);
                  end;
      stLongWord: begin
                    AllocBuf(Index, SizeOf(LongWord), ParamFormatBin);
                    LongWord2PG(LongWord(Trunc(Value)), FPQparamValues[Index]);
                  end;
      stLong:     begin
                    AllocBuf(Index, SizeOf(Int64), ParamFormatBin);
                    Int642PG(Trunc(Value), FPQparamValues[Index]);
                  end;
      stFloat:    begin
                    AllocBuf(Index, SizeOf(Single), ParamFormatBin);
                    Single2PG(Value, FPQparamValues[Index]);
                  end;
      stDouble:   begin
                    AllocBuf(Index, SizeOf(Double), ParamFormatBin);
                    Double2PG(Value, FPQparamValues[Index]);
                  end;
      stCurrency: begin
                    AllocBuf(Index, SizeOf(Currency), ParamFormatBin);
                    if FParamOIDs[index] = CASHOID
                    then Currency2PG(Value, FPQparamValues[Index])
                    else Double2PG(Value, FPQparamValues[Index]);
                  end;
      else BindRawStr(Index, SQLType, FloatToSqlRaw(Value));
    end;
  end else
    FParamValues[Index] := #39+FloatToSqlRaw(Value)+#39;
end;

procedure TZPostgreSQLPreparedStatement.BindNull(Index: Integer;
  var SQLType: TZSQLType);
begin
  if fPGExecMode <> pgemV2 then begin
    SQLType := OIDToSQLType(Index, SQLType);
//  if (FRawPlanName <> '') and not indeterminate_datatype and FInParamTypes[Index] <> SQLType and BindBinary then
    FPQparamFormats[Index] := ParamFormatStr;
    (*case FParamOIDs[Index] of
      BOOLOID:  FPQparamLengths[Index] := 1;
      BYTEAOID: FPQparamLengths[Index] := 0;
      INT8OID:  FPQparamLengths[Index] := 8;
      INT2OID:  FPQparamLengths[Index] := 2;
      INT4OID:  FPQparamLengths[Index] := 4;
      OIDOID:   FPQparamLengths[Index] := 4;
      FLOAT4OID:FPQparamLengths[Index] := 4;
      FLOAT8OID:FPQparamLengths[Index] := 8;
      CASHOID:  FPQparamLengths[Index] := 8;
      DATEOID:  FPQparamLengths[Index] := 4;
      TIMEOID:  FPQparamLengths[Index] := 8;
      TIMESTAMPOID: FPQparamLengths[Index] := 8;
      else FPQparamFormats[Index] := ParamFormatStr;
    end; *)
    FPQparamValues[Index] := nil
  end else FParamValues[Index] := 'null';
end;

procedure TZPostgreSQLPreparedStatement.BindRawStr(Index: Integer;
  var SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt);
begin
  if fPGExecMode <> pgemV2 then begin
    SQLType := OIDToSQLType(Index, SQLType);
    FPQparamLengths[Index] := Len;
    FPQparamFormats[Index] := ParamFormatStr;
    if SQLType in [stAsciiStream, stUnicodeStream] then
      FPQparamValues[Index] := Buf
    else if Len > 0 then begin
      ZSetString(Buf, Len, FParamValues[Index]);
      FPQparamValues[Index] := Pointer(FParamValues[Index]);
    end else
      FPQparamValues[Index] := PEmptyAnsiString;
  end else
    Connection.GetEscapeString(Buf, Len, FParamValues[Index]);
end;

procedure TZPostgreSQLPreparedStatement.BindRawStr(Index: Integer;
  var SQLType: TZSQLType; const Buf: RawByteString);
begin
  if fPGExecMode <> pgemV2 then begin
    FPQparamLengths[Index] := Length(Buf);
    FParamValues[Index] := Buf;
    if FPQparamLengths[Index] = 0
    then FPQparamValues[Index] := PEmptyAnsiString
    else FPQparamValues[Index] := Pointer(FParamValues[Index]);
    FPQparamFormats[Index] := ParamFormatStr;
  end else
    Connection.GetEscapeString(Pointer(Buf), Length(Buf), FParamValues[Index]);
end;

procedure TZPostgreSQLPreparedStatement.BindSignedOrdinal(Index: Integer;
  var SQLType: TZSQLType; const Value: Int64);
begin
  if fPGExecMode <> pgemV2 then begin
    SQLType := OIDToSQLType(Index, SQLType);
    case SQLType of
      stBoolean:  begin
                    AllocBuf(Index, SizeOf(Byte), ParamFormatBin);
                    PByte(FPQparamValues[Index])^ := Ord(Value <> 0);
                  end;
      stSmall:    begin
                    AllocBuf(Index, SizeOf(SmallInt), ParamFormatBin);
                    SmallInt2PG(SmallInt(Value), FPQparamValues[Index]);
                  end;
      stInteger:  begin
                    AllocBuf(Index, SizeOf(Integer), ParamFormatBin);
                    Integer2PG(Integer(Value), FPQparamValues[Index]);
                  end;
      stBinaryStream, //oidlobs
      stLongWord: begin
                    AllocBuf(Index, SizeOf(LongWord), ParamFormatBin);
                    LongWord2PG(LongWord(Value), FPQparamValues[Index]);
                  end;
      stLong,
      stCurrency: begin
                    AllocBuf(Index, SizeOf(Int64), ParamFormatBin);
                    Int642PG(Value, FPQparamValues[Index]);
                  end;
      stFloat:    begin
                    AllocBuf(Index, SizeOf(Single), ParamFormatBin);
                    Single2PG(Value, FPQparamValues[Index]);
                  end;
      stDouble:   begin
                    AllocBuf(Index, SizeOf(Double), ParamFormatBin);
                    Double2PG(Value, FPQparamValues[Index]);
                  end;
      else BindRawStr(Index, SQLType, IntToRaw(Value));
    end;
  end else
    FParamValues[Index] := #39+IntToRaw(Value)+#39;
end;

procedure TZPostgreSQLPreparedStatement.BindUnsignedOrdinal(Index: Integer;
  var SQLType: TZSQLType; const Value: UInt64);
begin
  BindSignedOrdinal(Index, SQLType, Int64(Value));
end;

function TZPostgreSQLPreparedStatement.BoolAsString(
  Value: Boolean): RawByteString;
begin
  Result := BoolStrIntsRaw[Value];
end;

function TZPostgreSQLPreparedStatement.CheckPrepareSwitchMode: Boolean;
begin
  Result := ((not FUseEmulatedStmtsOnly) or (ArrayCount > 0 )) and (FRawPlanName = '') and (TokenMatchIndex <> -1) and
     ((ArrayCount > 0 ) or (ExecutionCount = MinExecCount2Prepare));
  if Result then
    FEmulatedParams := False
  else if (TokenMatchIndex = -1) and (FRawPlanName = '') then
    FEmulatedParams := fPGExecMode <> pgemV2; //we do not have a PQExecParams
end;

constructor TZPostgreSQLPreparedStatement.Create(
  const Connection: IZPostgreSQLConnection; Info: TStrings);
begin
  Create(Connection, SQL, Info);
end;

function TZPostgreSQLPreparedStatement.GetLastQueryHandle: PPGresult;
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
        fPQParamsFoundInQuery := (P^ <> '?') and (fPQParamsFoundInQuery or (P^ = '$'));
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
    FlushBuff(Result);
  end else
    Result := ASQL;
end;

procedure TZPostgreSQLPreparedStatement.InternalRealPrepare;
begin
  FRawPlanName := IntToRaw(FStatementId)+IntToRaw({%H-}NativeUInt(Fconn))+IntToRaw(fPrepareCnt);
  QueryHandle := ExecuteInternal(fASQL, TEIPQPrepareModes[fPGExecMode])
end;

procedure TZPostgreSQLPreparedStatement.InternalSetInParamCount(
  NewParamCount: Integer);
var
  N, I: Integer;
begin
  if (not fPQParamsFoundInQuery or FEmulatedParams) and (Length(CachedQueryRaw) > 1) then begin
    fASQL := '';
    N := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then begin
        Inc(N);
        ToBuff('$', fASQL);
        ToBuff(IntToRaw(N), fASQL);
      end else
        ToBuff(CachedQueryRaw[i], fASQL);
    FlushBuff(fASQL);
  end;
  if (fPGExecMode <> pgemV2) and (NewParamCount <> FinParamCount) then begin
    SetLength(FParamOIDs, NewParamCount);
    SetLength(FPQparamValues, NewParamCount);
    SetLength(FPQparamLengths, NewParamCount);
    SetLength(FPQparamFormats, NewParamCount);
  end;
  inherited InternalSetInParamCount(NewParamCount);
end;

function TZPostgreSQLPreparedStatement.OIDToSQLType(Index: Integer;
  SQLType: TZSQLType): TZSQLType;
begin
  if (fPGExecMode = pgemV2) then
    Result := SQLType
  else case FParamOIDs[Index] of
    INVALIDOID: begin
      SQLTypeToPostgreSQL(SQLType, FOIdAsBLob, FParamOIDs[Index]);
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
    else if SQLType in [stString, stUnicodeString, stAsciistream, stUnicodeStream, stBinaryStream] then
      Result := SQLType
    else
      Result := stUnknown; //indicate unsupport the types as Fallback to String format
  end;
end;

function TZPostgreSQLPreparedStatement.CreateResultSet(
  QueryHandle: PPGresult; ServerCursor: Boolean): IZResultSet;
var
  NativeResultSet: TZAbstractPostgreSQLStringResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  {if fServerCursor
  then NativeResultSet := TZServerCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, Fconn,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else} NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, Fconn,
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
  FOpenResultSet := Pointer(Result);
end;

function TZPostgreSQLPreparedStatement.DateTimeAsString(const Value: TDateTime;
  SQLType: TZSQLType): RawByteString;
begin
  case SQLType of
    stTime: Result := DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, True,'::time');
    stDate: Result := DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, true,'::date');
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, True, '::timestamp');
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatement.PrepareInParameters;
var
  res: PPGresult;
  nParams, I: Integer;
  aOID: OID;
begin
  if FEmulatedParams or (fRawPlanName = '') or (fPGExecMode = pgemV2) then
    InternalSetInParamCount(CountOfQueryParams)
  else if (fRawPlanName <> '') and not (Findeterminate_datatype) and (CountOfQueryParams > 0) then begin
    if Assigned(FPlainDriver.PQdescribePrepared) then begin
      //we use the describe only if minimum one INVALIDOID was given else our bindings have been excepted by the server
      {nParams := -1;
      for i := low(FParamOIDs) to high(FParamOIDs) do
        if FParamOIDs[I] = INVALIDOID then begin
          nParams := I;
          Break;
        end;
      if nParams = -1 then
        Exit; //no describe required }
      res := FPlainDriver.PQdescribePrepared(Fconn, Pointer(FRawPlanname));
      try
        nParams := FplainDriver.PQnparams(res);
        if (InParamCount > 0) and (nParams <> InParamCount) then
          raise EZSQLException.Create(SInvalidInputParameterCount)
        else begin
          if (InParamCount = 0) and (nParams > 0) then
            InternalSetInParamCount(nParams);
          for i := 0 to InParamCount-1 do begin
            aOID := FplainDriver.PQparamtype(res, i);
            if (aOID <> FParamOIDs[i]) then begin
              //bind bin again or switch to string format if we do not support the PG Types -> else Error
              FParamOIDs[i] := aOID;
              if (FPQparamValues[I] <> nil) then
                case InParamTypes[i] of
                  stBoolean:  BindSignedOrdinal(i, InParamTypes[i], PByte(FPQparamValues[i])^);
                  stSmall:    BindSignedOrdinal(i, InParamTypes[i], PG2SmallInt(FPQparamValues[i]));
                  stInteger:  BindSignedOrdinal(i, InParamTypes[i], PG2Integer(FPQparamValues[i]));
                  stLongWord: BindSignedOrdinal(i, InParamTypes[i], PG2LongWord(FPQparamValues[i]));
                  stLong:     BindSignedOrdinal(i, InParamTypes[i], PG2Int64(FPQparamValues[i]));
                  stCurrency: BindDouble(i, InParamTypes[i], PG2Currency(FPQparamValues[i]));
                  stFloat:    BindDouble(i, InParamTypes[i], PG2Single(FPQparamValues[i]));
                  stDouble:   BindDouble(i, InParamTypes[i], PG2Double(FPQparamValues[i]));
                  stTime:     if Finteger_datetimes
                              then BindDateTime(i, InParamTypes[i], PG2Time(PInt64(FPQparamValues[i])^))
                              else BindDateTime(i, InParamTypes[i], PG2Time(PDouble(FPQparamValues[i])^));
                  stDate:     BindDateTime(i, InParamTypes[i], PG2Date(PInteger(FPQparamValues[i])^));
                  stTimeStamp:if Finteger_datetimes
                              then BindDateTime(i, InParamTypes[i], PG2DateTime(PInt64(FPQparamValues[i])^))
                              else BindDateTime(i, InParamTypes[i], PG2DateTime(PDouble(FPQparamValues[i])^));
                end;
            end;
          end;
        end;
      finally
        FPlainDriver.PQclear(res);
      end;
    end else
      for i := 0 to InParamCount-1 do
        SQLTypeToPostgreSQL(InParamTypes[i], fOIDAsBlob, FParamOIDs[i]);
  end;
end;

procedure TZPostgreSQLPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  inherited ReleaseImmediat(Sender);
  Fconn := nil;
  QueryHandle := nil;
  FRawPlanName := '';
  fPrepareCnt := 0;
  InternalSetInParamCount(0);
end;

procedure TZPostgreSQLPreparedStatement.SetBlob(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var
  WriteTempBlob: IZPostgreSQLOidBlob;
begin
  if Value.IsEmpty or not (FoidAsBlob and (SQLType = stBinaryStream)) then
    inherited SetBlob(ParameterIndex, SQLType, Value)
  else
    try
      WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
        Fconn, 0, ChunkSize);
      WriteTempBlob.WriteBuffer(Value.GetBuffer, Value.Length);
      if FEmulatedParams
      then SetRawByteString(ParameterIndex, IntToRaw(WriteTempBlob.GetBlobOid))
      else InternalSetOrdinal(ParameterIndex, stBinaryStream, WriteTempBlob.GetBlobOid);
      fParamLobs[ParameterIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] := Value; //else leaking mem we've no var to flush
    finally
      WriteTempBlob := nil;
    end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZPostgreSQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  BindInParameters;
  if (fPGExecMode = pgemV2) and ((FRawPlanName = '') or (Findeterminate_datatype)) then
    QueryHandle := ExecuteInternal(ComposeRawSQLQuery, TEIPQExecuteModes[FPGExecMode])
  else if Findeterminate_datatype or (FRawPlanName = '') then
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecuteModes[FPGExecMode])
  else
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecPreparedModes[FPGExecMode]);
  if QueryHandle <> nil then
    if Assigned(FOpenResultSet)
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet(QueryHandle, fServerCursor)
  else
    Result := nil;
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
function TZPostgreSQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  Prepare;
  BindInParameters;
  if (fPGExecMode = pgemV2) and ((FRawPlanName = '') or (Findeterminate_datatype)) then
    QueryHandle := ExecuteInternal(ComposeRawSQLQuery, TEIPQExecuteModes[FPGExecMode])
  else if Findeterminate_datatype or (FRawPlanName = '') then
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecuteModes[FPGExecMode])
  else if ArrayCount = 0 then
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecPreparedModes[FPGExecMode])
  else begin
    if true
    then ExecuteDMLBatchWithUnnest
    else ExecuteDMLBatchWithUnnestVarlenaArrays;
    Exit;
  end;
  if QueryHandle <> nil then begin
    Result := RawToIntDef(FPlainDriver.PQcmdTuples(QueryHandle), 0);
    FPlainDriver.PQclear(QueryHandle);
    FlushPendingResults;
  end;

  inherited ExecuteUpdatePrepared;
  CheckPrepareSwitchMode;
end;

procedure TZPostgreSQLPreparedStatement.FlushPendingResults;
var PQRes: PPGresult;
begin
  while True do begin
    PQRes := FPlainDriver.PQgetResult(Fconn);
    if PQRes = nil
    then break
    else FplainDriver.PQclear(PQRes);
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
function TZPostgreSQLPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  Category: TEICategory): PPGresult;
var
  PError: PAnsiChar;
  ExecSQL: RawByteString;
  I, N: Integer;
  TmpSQL: RawByteString;
label retryExecute;
begin
  Result := nil;
  if not Assigned(Fconn) then
    Exit;
  case Category of
    eicPrepStmtV2:
      begin
        TmpSQL := 'PREPARE "'+FRawPlanName+'" AS '+SQL;
        Result := FPlainDriver.PQExec(Fconn, Pointer(TmpSQL));
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(Fconn);
        if (PError <> nil) and (PError^ <> #0) then
          { check for indermine datatype error}
          if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
            HandlePostgreSQLError(Self, FPlainDriver, Fconn, lcPrepStmt, ASQL, Result)
          else begin
            FPlainDriver.PQclear(Result);
            Result := nil;
            Findeterminate_datatype := True;
            FEmulatedParams := True;
            //EH: grumple switch back to emulations and fix the bound values:
            for i := 0 to High(FInParamTypes) do begin
              if FParamValues[i] <> 'null' then
              case FInParamTypes[i] of
                stTime: FParamValues[i] := FParamValues[i]+'::time';
                stDate: FParamValues[i] := FParamValues[i]+'::date';
                stTimeStamp: FParamValues[i] := FParamValues[i]+'::timestamp';
              end;
            end;
          end;
      end;
    eicExecPrepStmtV2:
      begin
        TmpSQL := '';
        ToBuff('EXECUTE "', TmpSQL);
        ToBuff(FRawPlanName, TmpSQL);
        ToBuff('"', TmpSQL);
        if InParamCount > 0 then begin
          ToBuff('(', TmpSQL);
          for i := 0 to InParamCount -1 do begin
            ToBuff(FParamValues[i], TmpSQL);
            ToBuff(',', TmpSQL);
          end;
          FlushBuff(TmpSQL);
          TmpSQL[Length(TmpSQL)] := ')' //cancel last comma
        end else
          FlushBuff(TmpSQL);
        if Assigned(Fconn) then
          Result := FPlainDriver.PQExec(Fconn, Pointer(TmpSQL));
          if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
            HandlePostgreSQLError(Self, FPlainDriver, Fconn,
              lcExecPrepStmt, ExecSQL, Result);
      end;
    eicPrepStmtV3:
      begin
        Result := FPlainDriver.PQprepare(Fconn, Pointer(FRawPlanName),
          Pointer(SQL), InParamCount, nil{Pointer(fParamOIDs)});
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(Fconn);
        if (PError <> nil) and (PError^ <> #0) then
          { check for indermine datatype error}
          if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
            HandlePostgreSQLError(Self, FPlainDriver, Fconn,
              lcPrepStmt, ASQL, Result)
          else begin
            FPlainDriver.PQclear(Result);
            Findeterminate_datatype := True
          end
        else begin
          FPlainDriver.PQclear(Result);
          Result := nil;
          PrepareInParameters;
        end;
      end;
    eicExecPrepStmtV3: begin
        Result := FPlainDriver.PQexecPrepared(Fconn,
          Pointer(FRawPlanName), InParamCount, Pointer(FPQparamValues),
          Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
        if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
          HandlePostgreSQLError(Self, FPlainDriver, Fconn,
            lcExecPrepStmt, ASQL, Result);
      end;
    eicExecPrepStmtV3_Async:
        if FPlainDriver.PQsendQueryPrepared(Fconn,
           Pointer(FRawPlanName), InParamCount, Pointer(FPQparamValues),
           Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
          HandlePostgreSQLError(Self, FPlainDriver, Fconn,
            lcExecPrepStmt, ASQL, Result)
        else if FServerCursor then
          FPlainDriver.PQsetSingleRowMode(Fconn);
    eicUnprepStmtV2, eicUnprepStmtV3, eicUnprepStmtV3_Async: begin
        Result := FPlainDriver.PQExec(Fconn, Pointer(SQL));
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(Fconn);
        if (PError <> nil) and (PError^ <> #0) then
          { check for current transaction is aborted error}
          if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, current_transaction_is_aborted, 5) <> 0) then
            HandlePostgreSQLError(Self, FPlainDriver, Fconn, lcUnprepStmt, ASQL, Result)
          else
            FPostgreSQLConnection.RegisterTrashPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName))
        else if Result <> nil then begin
          FPlainDriver.PQclear(Result);
          Result := nil;
        end;
      end;
    eicExeParamV3: begin
        if (InParamCount > 0) then begin
          if not Findeterminate_datatype then begin
            Result := FPlainDriver.PQexecParams(Fconn, Pointer(FASQL),
              InParamCount, Pointer(fParamOIDs), Pointer(FPQparamValues),
              Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
            if Assigned(FPlainDriver.PQresultErrorField)
            then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
            else PError := FPLainDriver.PQerrorMessage(Fconn);
            if (PError <> nil) and (PError^ <> #0) then
              { check for indermine datatype error}
              if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) = 0) then begin
                FPlainDriver.PQclear(Result);
                Findeterminate_datatype := True;
                goto retryExecute;
              end;
          end else begin
retryExecute:
            TmpSQL := '';
            N := 0;
            for I := 0 to High(FCachedQueryRaw) do
              if FIsParamIndex[i] then begin
                ToBuff(GetBoundValueAsLogValue(n), TmpSQL);
                Inc(N);
              end else
                ToBuff(FCachedQueryRaw[i], TmpSQL);
              FlushBuff(TmpSQL);
            Result := FPlainDriver.PQExec(Fconn, Pointer(TmpSQL));
          end;
        end else
          Result := FPlainDriver.PQExec(Fconn, Pointer(FASQL));
        if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
          HandlePostgreSQLError(Self, FPlainDriver, Fconn,
            lcExecute, ASQL, Result);
      end;
    eicExeParamV3_Async:
        if FplainDriver.PQsendQueryParams(Fconn,
           Pointer(FASQL), InParamCount, Pointer(fParamOIDs), Pointer(FPQparamValues),
           Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
           HandlePostgreSQLError(Self, FPlainDriver, Fconn, lcExecute, ASQL, Result)
        else if FServerCursor then
          FPlainDriver.PQsetSingleRowMode(Fconn);
    eicPrepStmtV3_Async:
      if not FPlainDriver.PQsendPrepare(Fconn, Pointer(FRawPlanName),
         Pointer(FASQL), InParamCount, Pointer(fParamOIDs)) = Ord(PGRES_COMMAND_OK) then
        HandlePostgreSQLError(Self, FPlainDriver, Fconn,
          lcExecute, ASQL, nil);
    else
      begin
        Result := FPlainDriver.PQExec(Fconn, Pointer(SQL));
        if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
          HandlePostgreSQLError(Self, FPlainDriver, Fconn,
            lcExecute, ASQL, Result);
      end;
  end;
end;
{$R+}

function TZPostgreSQLPreparedStatement.ExecuteDMLBatchWithUnnest: PPGresult;
var
  OffSet, nParamsOfPGArray: Cardinal;
  UniTemp: ZWideString;
  D, P: Pointer;
  CP: word;
  AutoComm: boolean;
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  fRawTemp: RawByteString;
  function BuildStmtStr(Iters: Cardinal): RawByteString;
  var
    J, I, N: Cardinal;
    aOID: OID;
  begin
    Result := '';
    N := 1;
    OffSet := 0;
    //first build up a new string with replaced params for the full chunks
    for I := 0 to high(FCachedQueryRaw) do
      if IsParamIndex[i] then begin
        ToBuff('unnest(array[', Result);
        for J := 0 to Iters-1 do begin
          ToBuff('$',Result);
          ToBuff(IntToRaw(N), Result);
          ToBuff(',', Result);
          Inc(N);
        end;
        FlushBuff(Result);
        Result[Length(Result)] := ']'; //cancel last comma
        ToBuff('::', Result);
        SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[OffSet].VArrayType), FOidAsBlob, aOID);
        ToBuff({$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID)),Result);
        ToBuff('[])', Result);
        ToBuff(lineEnding, Result);
        Inc(OffSet);
      end else
        ToBuff(FCachedQueryRaw[i], Result);
    FlushBuff(Result);
  end;

  procedure BindAndExecute(const stmt: TZPostgreSQLPreparedStatement; Arrayoffset, Iters: Cardinal; EICategory: TEICategory);
  var J, I: Cardinal;
    N: Integer; //the ParameterIndex
    SQLType: TZSQLType;
  begin
    N := -1;
    for i := 0 to fInParamCount -1 do begin
      for J := ArrayOffSet to ArrayOffSet+Iters-1 do begin
        Inc(N);
        if not ZDbcUtils.IsNullFromArray(@FBatchArrays[I], J) then begin
          SQLType := TZSQLType(FBatchArrays[I].VArrayType);
          D := FBatchArrays[I].VArray;
          P := Pointer(stmt.FParamValues[N]);
          SQLTypeToPostgreSQL(SQLType, FOidAsBlob, stmt.FParamOIDs[N]);
          case SQLType of
            stBoolean:    if (stmt.FParamOIDs[N] = BOOLOID) and (P <> nil)
                          then PByte(P)^ := Ord(TBooleanDynArray(D)[N])
                          else stmt.BindSignedOrdinal(N, SQLType, Ord(TBooleanDynArray(D)[j]));
            stByte:       stmt.BindSignedOrdinal(N, SQLType, TByteDynArray(D)[j]);
            stShort:      stmt.BindSignedOrdinal(N, SQLType, TShortIntDynArray(D)[j]);
            stWord:       stmt.BindSignedOrdinal(N, SQLType, TWordDynArray(D)[j]);
            stSmall:      if (stmt.FParamOIDs[N] = INT2OID) and (P <> nil)
                          then SmallInt2PG(TSmallIntDynArray(D)[j], P)
                          else stmt.BindSignedOrdinal(N, SQLType, TSmallIntDynArray(D)[j]);
            stInteger:    if (stmt.FParamOIDs[N] = INT4OID) and (P <> nil)
                          then Integer2PG(TSmallIntDynArray(D)[j], P)
                          else stmt.BindSignedOrdinal(N, SQLType, TIntegerDynArray(D)[j]);
            stLongWord:   if (stmt.FParamOIDs[N] = OIDOID) and (P <> nil)
                          then LongWord2PG(TLongWordDynArray(D)[j], P)
                          else stmt.BindSignedOrdinal(N,SQLType, TLongWordDynArray(D)[j]);
            stLong:       if (stmt.FParamOIDs[N] = INT8OID) and (P <> nil)
                          then Int642PG(TInt64DynArray(D)[j], P)
                          else stmt.BindSignedOrdinal(N,SQLType, TInt64DynArray(D)[j]);
            stULong:      if (stmt.FParamOIDs[N] = INT8OID) and (P <> nil)
                          then Int642PG(TUInt64DynArray(D)[j], P)
                          else stmt.BindSignedOrdinal(N,SQLType, TUInt64DynArray(D)[j]);
            stFloat:      if (stmt.FParamOIDs[N] = FLOAT4OID) and (P <> nil)
                          then Single2PG(TSingleDynArray(D)[j], P)
                          else stmt.BindDouble(N, SQLType, TSingleDynArray(D)[j]);
            stDouble:     if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                          then Double2PG(TDoubleDynArray(D)[j], P)
                          else stmt.BindDouble(N,SQLType, TDoubleDynArray(D)[j]);
            stCurrency:   if (stmt.FParamOIDs[N] = CASHOID) and (P <> nil)
                          then Currency2PG(TCurrencyDynArray(D)[j], P)
                          else if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                          then Double2PG(TCurrencyDynArray(D)[j], P)
                          else stmt.BindDouble(N, SQLType, TCurrencyDynArray(D)[j]);
            stBigDecimal: if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                          then Double2PG(TExtendedDynArray(D)[j], P)
                          else stmt.BindDouble(N, SQLType, TExtendedDynArray(D)[j]);
            stDate:       if (stmt.FParamOIDs[N] = DATEOID) and (P <> nil)
                          then Date2PG(TDateTimeDynArray(D)[j], PInteger(P)^)
                          else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
            stTime:       if (stmt.FParamOIDs[N] = TIMEOID) and (P <> nil)
                          then Time2PG(TDateTimeDynArray(D)[j], PInt64(P)^)
                          else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
            stTimeStamp:  if (stmt.FParamOIDs[N] = TIMEOID) and (P <> nil)
                          then DateTime2PG(TDateTimeDynArray(D)[j], PInt64(P)^)
                          else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
            stGUID:       if (stmt.FParamOIDs[N] = UUIDOID) then begin
                            stmt.FPQparamValues[N] := @TGUIDDynArray(D)[j].D1;
                            stmt.FPQparamLengths[N] := SizeOf(TGUID);
                            stmt.FPQparamFormats[n] := ParamFormatBin;
                          end else
                            stmt.BindBinary(N, SQLType, @TGUIDDynArray(D)[j].D1, SizeOf(TGUID));
            stBytes:      if (stmt.FParamOIDs[N] = BYTEAOID) then begin
                            stmt.FPQparamValues[N] := Pointer(TBytesDynArray(D)[j]);
                            stmt.FPQparamLengths[N] := Length(TBytesDynArray(D)[j]);
                            stmt.FPQparamFormats[n] := ParamFormatBin;
                          end else
                            stmt.BindBinary(I, SQLType, Pointer(TBytesDynArray(D)[j]), Length(TBytesDynArray(D)[j]));
            stString, stUnicodeString: begin
                stmt.FPQparamFormats[N] := ParamFormatStr;
                case FBatchArrays[I].VArrayVariantType of
                  vtString:     stmt.FParamValues[N] := ConSettings.ConvFuncs.ZStringToRaw(TStringDynArray(D)[j], ConSettings.CTRL_CP, CP);
                  vtAnsiString: stmt.FParamValues[N] := Consettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(D)[j], CP);
                  vtUTF8String: if ZCompatibleCodePages(CP, zCP_UTF8) then begin
                                  stmt.FPQparamValues[N] := Pointer(TUTF8StringDynArray(D)[j]);
                                  if stmt.FPQparamValues[N] = nil then
                                    stmt.FPQparamValues[N] := PEmptyAnsiString;
                                  stmt.FPQparamLengths[N] := Length(TUTF8StringDynArray(D)[j]);
                                  Continue;
                                end else
                                  stmt.FParamValues[N] := Consettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(D)[j], CP);
                  vtRawByteString:begin
                                    stmt.FPQparamValues[N] := Pointer(TRawByteStringDynArray(D)[j]);
                                    if stmt.FPQparamValues[N] = nil then
                                      stmt.FPQparamValues[N] := PEmptyAnsiString;
                                    stmt.FPQparamLengths[N] := Length(TRawByteStringDynArray(D)[j]);
                                    Continue;
                                  end;
                  vtUnicodeString: stmt.FParamValues[N] := ZUnicodeToRaw(TUnicodeStringDynArray(D)[j], CP);
                  vtCharRec: if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) or (TZCharRecDynArray(D)[j].Len = 0) then begin
                                stmt.FPQparamValues[N] := TZCharRecDynArray(D)[j].P;
                                stmt.FPQparamLengths[N] := TZCharRecDynArray(D)[j].Len;
                                if stmt.FPQparamValues[N] = nil then
                                  stmt.FPQparamValues[N] := PEmptyAnsiString;
                                Continue;
                              end else if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, zCP_UTF16) then
                                stmt.FParamValues[N] := PUnicodeToRaw(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, CP)
                              else begin
                                UniTemp := PRawToUnicode(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, TZCharRecDynArray(D)[j].CP);
                                stmt.FParamValues[N] := ZUnicodeToRaw(UniTemp, CP)
                              end;
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
                stmt.FPQparamValues[N] := Pointer(stmt.FParamValues[N]);
                if stmt.FPQparamValues[N] = nil then
                  stmt.FPQparamValues[N] := PEmptyAnsiString;
                stmt.FPQparamLengths[N] := Length(stmt.FParamValues[N]);
              end;
            stAsciiStream, stUnicodeStream, stBinaryStream:
              begin
                TempBlob := TInterfaceDynArray(D)[j] as IZBlob;
                if (TempBlob <> nil) and not TempBlob.IsEmpty then begin
                  if FInParamTypes[i] in [stUnicodeStream, stAsciiStream] then
                    if TempBlob.IsClob then
                      TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP)
                    else begin
                      fRawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                      TempBlob := TZAbstractClob.CreateWithData(Pointer(fRawTemp), Length(fRawTemp), Cp, ConSettings);
                      TInterfaceDynArray(D)[j] := TempBlob;
                    end
                  else if FOidAsBlob then begin
                    WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
                      Fconn, 0, ChunkSize);
                    WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                    if (stmt.FParamOIDs[N] = OIDOID) and (P <> nil)
                    then LongWord2PG(WriteTempBlob.GetBlobOid, P)
                    else stmt.BindSignedOrdinal(N, SQLType, WriteTempBlob.GetBlobOid);
                    continue;
                  end;
                  Stmt.FPQparamValues[N] := TempBlob.GetBuffer;
                  Stmt.FPQparamLengths[N] := TempBlob.Length;
                end else
                  Stmt.FPQparamValues[N] := nil;
              end;
          end;
        end else
          Stmt.FPQparamValues[N] := nil;
      end;
    end;
    Stmt.ExecuteInternal(Stmt.FASQL, EICategory)
  end;
var
  Stmt: TZPostgreSQLPreparedStatement;
  Loops, ArrayOffSet: Cardinal;
begin
//introduction
// the posgres server support bulk ops since 8.4
// but the free version of libpq simply misses this API:
// https://www.enterprisedb.com/docs/en/9.4/eeguide/Postgres_Plus_Enterprise_Edition_Guide.1.088.html
// its made for pay customers only

//so find something we can do with the free libpq:

//diaray:
//first approach:
//pg 9.6 pipelined batch of https://github.com/2ndQuadrant/postgres/tree/dev/libpq-async-batch
//see: https://www.postgresql.org/message-id/flat/CAMsr+YFUjJytRyV4J-16bEoiZyH=4nj+sQ7JP9ajwz=B4dMMZw@mail.gmail.com#CAMsr+YFUjJytRyV4J-16bEoiZyH=4nj+sQ7JP9ajwz=B4dMMZw@mail.gmail.com

//to behonest:
// It's a bit(1.5x) faster than single executions because of the batch
// and onetime send of all queries. But we need the compiled version of 2ndQuadrant
//code goes here :
(*
  if FPlainDriver.PQbeginBatchMode(Fconn) = Ord(PGRES_COMMAND_OK) then begin
   ///bind data
      if FplainDriver.PQsendQueryPrepared(FConn, Pointer(FRawPlanName), FInParamCount,
         Pointer(FPQparamValues), Pointer(FPQparamLengths), Pointer(FPQparamFormats),
         ParamFormatStr) <> Ord(PGRES_COMMAND_OK) then
      HandlePostgreSQLError(Self, FplainDriver, Fconn, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    end;
    if not FplainDriver.PQsendEndBatch(FConn)  <> Ord(PGRES_COMMAND_OK) then
       HandlePostgreSQLError(Self, FplainDriver, Fconn, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    J := 0;
    while True do begin
      //now we've to obtain all resuls, else we can't leave the batchmode
      I := FplainDriver.PQgetNextQuery(Fconn);
      if i = Ord(PGRES_BATCH_END) then
        Break;
      P := FplainDriver.PQgetResult(Fconn);
      if P <> nil then begin
        FplainDriver.PQclear(P);
        Inc(J);
      end else
        break;
    end;
    if J <> ArrayCount+1 then
       raise Exception.Create('Fehlermeldung');
    if FplainDriver.PQisInBatchMode(Fconn) = Ord(PGRES_COMMAND_OK) then
      FplainDriver.PQEndBatchMode(FConn);
*) //->>>>> trash, worth in vain

//second approach:
//sadly postgres "DO" anonymous block does not support paramaters !

//so create a function with array params as input and execute the dml in a loop
//works ok and is portable for all kind of user queries.
//but we've a sp now which first need to be created and if we unprepare the stmt
//we also nee to drop the function again
//what about name collisions ?
//perfornmance is 4x faster than single executions
// example
{  function "sp_in_batch_example" (int[],text[]..)
returns int[] as $$
declare i INT;
begin
   FOR i IN 1 .. array_upper($1, 1)
    LOOP
       insert into "example" values(nextval('"example_ID_Seq"'),$1[i],$2[i]);
    END LOOP;

   return $1;
end;
$$ LANGUAGE plpgsql;}

//the code goes here :
(*
  if FDMLBatchFunctions[tDML] = '' then begin
    ToBuff('create or replace function "', fRawTemp);
    FDMLBatchFunctions[tDML] := FRawPlanName+IntToRaw(Ord(tDML))+InttoRaw(Hash(FASQL));
    ToBuff(FDMLBatchFunctions[tDML], fRawTemp);
    ToBuff('" (', fRawTemp);
    for i := Low(FParamOIDs) to high(FParamOIDs) do begin
      SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[i].VArrayType), FOidAsBlob, aOID);
      ToBuff({$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID)),fRawTemp);
      ToBuff('[],',fRawTemp);
    end;
    FlushBuff(fRawTemp);
    fRawTemp[Length(fRawTemp)] := ')'; //cancel last comma
    ToBuff(lineending, fRawTemp);
    ToBuff('returns void as $$',fRawTemp);
    ToBuff(lineEnding, fRawTemp);
    ToBuff('declare i int;', fRawTemp);
    ToBuff(lineEnding, fRawTemp);
    ToBuff('begin', fRawTemp);
    ToBuff(lineEnding, fRawTemp);
    ToBuff('  FOR i IN 1 .. array_upper($1, 1) loop', fRawTemp);
    ToBuff(lineEnding, fRawTemp);
    ToBuff('    ', fRawTemp);
    J := 0;
    for I := 0 to high(FCachedQueryRaw) do
      if IsParamIndex[i] then begin
        Inc(J);
        ToBuff('$', fRawTemp);
        ToBuff(IntToRaw(J), fRawTemp);
        ToBuff('[i]', fRawTemp);
      end else
        ToBuff(FCachedQueryRaw[i], fRawTemp);
    //FlushBuff(fRawTemp); //todo check if terminated already
    ToBuff(';', fRawTemp);
    ToBuff(lineEnding, fRawTemp);
    ToBuff('  end loop;', fRawTemp);
    ToBuff(LineEnding, fRawTemp);
    ToBuff('end; $$ LANGUAGE plpgsql;', fRawTemp);
    FlushBuff(fRawTemp);

    QueryHandle := FplainDriver.PQexec(Fconn, Pointer(fRawTemp));
    if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
      HandlePostgreSQLError(Self, FPlainDriver, Fconn,
        lcExecPrepStmt, fRawTemp, QueryHandle);
  end;
  //calc amount of usable parameters per roundtrip
  if PGMaxParams div InParamCount div ArrayCount > 0 then
    nParamsOfPGArray := ArrayCount
  else
    nParamsOfPGArray := (PGMaxParams div InParamCount) mod ArrayCount;

  fRawTemp := '';
  ToBuff('select "', fRawTemp);
  ToBuff(FDMLBatchFunctions[tDML], fRawTemp);
  ToBuff('"(', fRawTemp);
  N := 1;
  for J := 0 to fInParamCount -1 do begin
    ToBuff('array[', fRawTemp);
    for i := 0 to nParamsOfPGArray-1 do begin
      ToBuff('$',fRawTemp);
      ToBuff(IntToRaw(N), fRawTemp);
      ToBuff(',', fRawTemp);
      Inc(N);
    end;
    FlushBuff(fRawTemp);
    fRawTemp[Length(fRawTemp)] := ']'; //cancel last comma
    ToBuff('::', fRawTemp);
    SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[j].VArrayType), FOidAsBlob, aOID);
    ToBuff({$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID)),fRawTemp);
    ToBuff('[],', fRawTemp)
  end;
  FlushBuff(fRawTemp);
  fRawTemp[Length(fRawTemp)] := ')'; //cancel last comma
*)

// EH: my third approach ..
// using unnest(array[$1,$2.....]::bigint[]
// performance is 9x faster than single executions
// but we've to send data chunked -> using more than 5000 params lead to performance loss again even if 32k are supported

//if someone could show me how to bind the array native !with! null indicators
//i'm sure we would have the final performance boost again
//note to myselve: https://stackoverflow.com/questions/4016412/postgresqls-libpq-encoding-for-binary-transport-of-array-data

  AutoComm := Connection.GetAutoCommit;
  //calc amount of usable parameters per roundtrip
  //note using more params per loop kills the performace again
  Loops := ((ArrayCount * InParamCount) div 5000);
  if Loops <= 1 then
    nParamsOfPGArray := ArrayCount
  else
    nParamsOfPGArray := (5000 div InParamCount) mod ArrayCount;

  Result := nil;
  CP := ConSettings^.ClientCodePage.CP;
  Stmt := TZPostgreSQLPreparedStatement.Create(FPostgreSQLConnection, Info);
  Stmt.FaSQL := BuildStmtStr(nParamsOfPGArray);
  Stmt.InternalSetInParamCount(nParamsOfPGArray*Cardinal(InParamCount));
  try
    try
      if Loops > 1 then begin
        if AutoComm then
           Connection.SetAutoCommit(False);
        Stmt.InternalRealPrepare; //faster in case of multiple executions
        ArrayOffSet := 0;
        for OffSet := 0 to Loops-1 do begin
          BindAndExecute(Stmt, ArrayOffSet, nParamsOfPGArray, eicExecPrepStmtV3);
          Inc(ArrayOffSet, nParamsOfPGArray);
        end;
        //final chunk:
        nParamsOfPGArray := Cardinal(ArrayCount) - ArrayOffSet;
        if nParamsOfPGArray > 0 then begin
          Stmt.FaSQL := BuildStmtStr(nParamsOfPGArray);
          //we don't need to realloc all buffs again
          //but we need to init OIDs to initial type on start of second parameter array
          FillChar(Stmt.FParamOIDs[nParamsOfPGArray], SizeOf(OID)*((nParamsOfPGArray*Cardinal(InParamCount))-nParamsOfPGArray), #0);
          // and indicate the new paramcount
          Stmt.FInParamCount := nParamsOfPGArray*Cardinal(InParamCount);
          BindAndExecute(Stmt, ArrayOffSet, nParamsOfPGArray, eicExeParamV3);
        if AutoComm then
          Connection.Commit;
        end;
      end else
        BindAndExecute(Stmt, 0, nParamsOfPGArray, eicExeParamV3);
    except
      if AutoComm and (Loops > 1) then
        Connection.Rollback;
      raise;
    end;
  finally
    FreeAndNil(Stmt);
    if AutoComm and (Loops > 1) then
      Connection.SetAutoCommit(AutoComm);
  end;
end;

function TZPostgreSQLPreparedStatement.ExecuteDMLBatchWithUnnestVarlenaArrays: PPGresult;
var
  OffSet, J, I: Cardinal;
  UniTemp: ZWideString;
  D, P, A, pN: Pointer;
  CP: word;
  aOID: OID;
  fRawTemp: RawByteString;
  Stmt: TZPostgreSQLPreparedStatement;
  N: Integer; //the ParameterIndex
  SQLType: TZSQLType;
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  procedure AllocArray(Index: Cardinal; ElementSize: Byte; out P: Pointer);
  var
    aOID: OID;
  begin
    SetLength(stmt.FParamValues[Index], GET_ARR_SIZE(ArrayCount, ElementSize, True));
    P := Pointer(stmt.FParamValues[Index]);
    FillChar(p^, Length(stmt.FParamValues[Index]), #0);
    SET_ARR_HASNULL(P, True);
    SET_ARR_NDIM(P, 1);
    SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[I].VArrayType), FOidAsBlob, aOID);
    SET_ARR_ELEMTYPE(P,aOID);
    Integer2PG(ArrayCount, ARR_DIMS(P));
  end;
begin
  Result := nil;
  CP := ConSettings^.ClientCodePage.CP;

  fRawTemp := '';
  N := 1;
  OffSet := 0;
  //first build up a new string with unnest($n)::xyz[] surrounded params
  for I := 0 to high(FCachedQueryRaw) do
    if IsParamIndex[i] then begin
      ToBuff('unnest(', fRawTemp);
      ToBuff('$',fRawTemp);
      ToBuff(IntToRaw(N), fRawTemp);
      ToBuff(',', fRawTemp);
      Inc(N);
      FlushBuff(fRawTemp);
      (PAnsiChar(Pointer(fRawTemp))+Length(fRawTemp)-1)^ := ':'; //cancel last comma
      ToBuff(':', fRawTemp);
      SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[OffSet].VArrayType), FOidAsBlob, aOID);
      ToBuff({$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID)),fRawTemp);
      ToBuff('[])', fRawTemp);
      Inc(OffSet);
    end else
      ToBuff(FCachedQueryRaw[i], fRawTemp);
  FlushBuff(fRawTemp);
(* gives such a string:
insert into public.SampleRecord (ID,FirstName,LastName,Amount,BirthDate,LastChange,CreatedAt) values (
unnest($1::int8[])
,unnest($2::text[])
,unnest($3::text[])
,unnest($4::float8[])
,unnest($5::timestamp[])
,unnest($6::int8[])
,unnest($7::int8[]))
*)
  Stmt := TZPostgreSQLPreparedStatement.Create(FPostgreSQLConnection, Info);
  Stmt.FaSQL := fRawTemp;
  Stmt.InternalSetInParamCount(InParamCount);
  Stmt.FParamsCnt := InParamCount;
  Stmt.InternalRealPrepare; //force describe the params to get the array oids
  N := -1;
  for i := 0 to fInParamCount -1 do begin
    SQLType := TZSQLType(FBatchArrays[I].VArrayType);
    J := 0;
    D := FBatchArrays[I].VArray;
    P := nil;
    case SQLType of
      stBoolean:    begin
                      AllocArray(I, SizeOf(Byte), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        PByte(P)^ := Ord(TBooleanDynArray(D)[N]);
                        Inc(NativeUInt(P));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stByte:       begin
                      AllocArray(I, SizeOf(SmallInt), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        SmallInt2PG(TByteDynArray(D)[j], P);
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        Inc(NativeUInt(P), SizeOf(SmallInt));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stShort:      begin
                      AllocArray(I, SizeOf(SmallInt), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        SmallInt2PG(TShortIntDynArray(D)[j], P);
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        Inc(NativeUInt(P), SizeOf(SmallInt));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stWord:       begin
                      AllocArray(I, SizeOf(Integer), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        Integer2PG(TWordDynArray(D)[j], P);
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        Inc(NativeUInt(P), SizeOf(Integer));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stSmall:      begin
                      AllocArray(I, SizeOf(SmallInt), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        SmallInt2PG(TSmallIntDynArray(D)[j], P);
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        Inc(NativeUInt(P), SizeOf(SmallInt));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stInteger:    if (stmt.FParamOIDs[N] = INT4OID) and (P <> nil)
                    then Integer2PG(TSmallIntDynArray(D)[j], P)
                    else stmt.BindSignedOrdinal(N, SQLType, TIntegerDynArray(D)[j]);
      stLongWord:   if (stmt.FParamOIDs[N] = OIDOID) and (P <> nil)
                    then LongWord2PG(TLongWordDynArray(D)[j], P)
                    else stmt.BindSignedOrdinal(N,SQLType, TLongWordDynArray(D)[j]);
      stLong:       begin
                      AllocArray(I, SizeOf(Int64), A);
                      P := ARR_DATA_PTR(A);
                      pN := ARR_NULLBITMAP(A);
                      for j := 0 to ArrayCount -1 do begin
                        Int642PG(TInt64DynArray(D)[j], P);
                        PByte(pN)^ := Ord(IsNullFromArray(@FBatchArrays[I], J));
                        Inc(NativeUInt(P), SizeOf(Int64));
                        Inc(NativeUInt(pN));
                      end;
                    end;
      stULong:      if (stmt.FParamOIDs[N] = INT8OID) and (P <> nil)
                    then Int642PG(TUInt64DynArray(D)[j], P)
                    else stmt.BindSignedOrdinal(N,SQLType, TUInt64DynArray(D)[j]);
      stFloat:      if (stmt.FParamOIDs[N] = FLOAT4OID) and (P <> nil)
                    then Single2PG(TSingleDynArray(D)[j], P)
                    else stmt.BindDouble(N, SQLType, TSingleDynArray(D)[j]);
      stDouble:     if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                    then Double2PG(TDoubleDynArray(D)[j], P)
                    else stmt.BindDouble(N,SQLType, TDoubleDynArray(D)[j]);
      stCurrency:   if (stmt.FParamOIDs[N] = CASHOID) and (P <> nil)
                    then Currency2PG(TCurrencyDynArray(D)[j], P)
                    else if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                    then Double2PG(TCurrencyDynArray(D)[j], P)
                    else stmt.BindDouble(N, SQLType, TCurrencyDynArray(D)[j]);
      stBigDecimal: if (stmt.FParamOIDs[N] = FLOAT8OID) and (P <> nil)
                    then Double2PG(TExtendedDynArray(D)[j], P)
                    else stmt.BindDouble(N, SQLType, TExtendedDynArray(D)[j]);
      stDate:       if (stmt.FParamOIDs[N] = DATEOID) and (P <> nil)
                    then Date2PG(TDateTimeDynArray(D)[j], PInteger(P)^)
                    else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
      stTime:       if (stmt.FParamOIDs[N] = TIMEOID) and (P <> nil)
                    then Time2PG(TDateTimeDynArray(D)[j], PInt64(P)^)
                    else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
      stTimeStamp:  if (stmt.FParamOIDs[N] = TIMEOID) and (P <> nil)
                    then DateTime2PG(TDateTimeDynArray(D)[j], PInt64(P)^)
                    else stmt.BindDateTime(N, SQLType, TDateTimeDynArray(D)[j]);
      stGUID:       if (stmt.FParamOIDs[N] = UUIDOID) then begin
                      stmt.FPQparamValues[N] := @TGUIDDynArray(D)[j].D1;
                      stmt.FPQparamLengths[N] := SizeOf(TGUID);
                    end else
                      stmt.BindBinary(N, SQLType, @TGUIDDynArray(D)[j].D1, SizeOf(TGUID));
      stBytes:      if (stmt.FParamOIDs[N] = BYTEAOID) then begin
                      stmt.FPQparamValues[N] := Pointer(TBytesDynArray(D)[j]);
                      stmt.FPQparamLengths[N] := Length(TBytesDynArray(D)[j]);
                    end else
                      stmt.BindBinary(I, SQLType, Pointer(TBytesDynArray(D)[j]), Length(TBytesDynArray(D)[j]));
      stString, stUnicodeString: begin
          stmt.FPQparamFormats[N] := ParamFormatStr;
          case FBatchArrays[I].VArrayVariantType of
            vtString:     stmt.FParamValues[N] := ConSettings.ConvFuncs.ZStringToRaw(TStringDynArray(D)[j], ConSettings.CTRL_CP, CP);
            vtAnsiString: stmt.FParamValues[N] := Consettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(D)[j], CP);
            vtUTF8String: if ZCompatibleCodePages(CP, zCP_UTF8) then begin
                            stmt.FPQparamValues[N] := Pointer(TUTF8StringDynArray(D)[j]);
                            if stmt.FPQparamValues[N] = nil then
                              stmt.FPQparamValues[N] := PEmptyAnsiString;
                            Continue;
                          end else
                            stmt.FParamValues[N] := Consettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(D)[j], CP);
            vtRawByteString:begin
                              stmt.FPQparamValues[N] := Pointer(TRawByteStringDynArray(D)[j]);
                              if stmt.FPQparamValues[N] = nil then
                                stmt.FPQparamValues[N] := PEmptyAnsiString;
                              Continue;
                            end;
            vtUnicodeString: stmt.FParamValues[N] := ZUnicodeToRaw(TUnicodeStringDynArray(D)[j], CP);
            vtCharRec: if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) or (TZCharRecDynArray(D)[j].Len = 0) then begin
                          stmt.FPQparamValues[N] := TZCharRecDynArray(D)[j].P;
                          if stmt.FPQparamValues[N] = nil then
                            stmt.FPQparamValues[N] := PEmptyAnsiString;
                          Continue;
                        end else if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, zCP_UTF16) then
                          stmt.FParamValues[N] := PUnicodeToRaw(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, CP)
                        else begin
                          UniTemp := PRawToUnicode(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, TZCharRecDynArray(D)[j].CP);
                          stmt.FParamValues[N] := ZUnicodeToRaw(UniTemp, CP)
                        end;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
          stmt.FPQparamValues[N] := Pointer(stmt.FParamValues[N]);
          stmt.FPQparamLengths[N] := Length(stmt.FParamValues[N]);
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := TInterfaceDynArray(D)[j] as IZBlob;
          if (TempBlob <> nil) and not TempBlob.IsEmpty then begin
            if FInParamTypes[i] in [stUnicodeStream, stAsciiStream] then
              if TempBlob.IsClob then
                TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP)
              else begin
                fRawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                TempBlob := TZAbstractClob.CreateWithData(Pointer(fRawTemp), Length(fRawTemp), Cp, ConSettings);
                TInterfaceDynArray(D)[j] := TempBlob;
              end
            else if FOidAsBlob then begin
              WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
                Fconn, 0, ChunkSize);
              WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
              if (stmt.FParamOIDs[N] = OIDOID) and (P <> nil)
              then LongWord2PG(WriteTempBlob.GetBlobOid, P)
              else stmt.BindSignedOrdinal(N, SQLType, WriteTempBlob.GetBlobOid);
              continue;
            end;
            Stmt.FPQparamValues[N] := TempBlob.GetBuffer;
            Stmt.FPQparamLengths[N] := TempBlob.Length;
          end else
            Stmt.FPQparamValues[N] := nil;
        end;
    end;
  end;
  Stmt.ExecuteInternal(Stmt.FASQL, eicExeParamV3)
end;

function TZPostgreSQLPreparedStatement.ExecutePrepared: Boolean;
var
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  if (fPGExecMode = pgemV2) and ((FRawPlanName = '') or (Findeterminate_datatype)) then
    QueryHandle := ExecuteInternal(ComposeRawSQLQuery, TEIPQExecuteModes[FPGExecMode])
  else if Findeterminate_datatype or (FRawPlanName = '') then
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecuteModes[FPGExecMode])
  else
    QueryHandle := ExecuteInternal(ASQL, TEIPQExecPreparedModes[FPGExecMode]);

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
  CheckPrepareSwitchMode;
end;

procedure TZPostgreSQLPreparedStatement.Prepare;
begin
  if fPGExecMode = pgemV3_Async then
    FlushPendingResults;
  if not Prepared then begin
    Inc(fPrepareCnt);
    inherited Prepare; //we need this step always for Set(A/W)SQL overloads if SQL changes
  end;
  if CheckPrepareSwitchMode then
    InternalRealPrepare;
end;

function TZPostgreSQLPreparedStatement.GetBoundValueAsLogValue(
  Index: Integer): RawByteString;
begin
  if fPGExecMode <> pgemV2 then begin
    Result := '';
    if FPQparamValues[Index] = nil then
      Result := 'NULL'
    else if (FPQparamFormats[Index] = ParamFormatStr) then
      Connection.GetEscapeString(PAnsichar(FPQparamValues[Index]), FPQparamLengths[Index], Result)
    else case InParamTypes[Index] of
      stBoolean:    Result := ZSysUtils.BoolStrIntsRaw[PByte(FPQparamValues[Index])^ <> 0];
      stSmall:      Result := IntToRaw(PG2SmallInt(FPQparamValues[Index]));
      stInteger:    Result := IntToRaw(PG2Integer(FPQparamValues[Index]));
      stLongWord:   Result := IntToRaw(PG2LongWord(FPQparamValues[Index]));
      stLong:       Result := IntToRaw(PG2LongWord(FPQparamValues[Index]));
      stFloat:      Result := FloatToRaw(PG2Single(FPQparamValues[Index]));
      stDouble:     Result := FloatToRaw(PG2Double(FPQparamValues[Index]));
      stCurrency:   Result := FloatToRaw(PG2Currency(FPQparamValues[Index]));
      stGUID:       Result := GUIDToRaw(PGUID(FPQparamValues[Index])^);
      stTime:       if Finteger_datetimes
                    then Result := DateTimeToRawSQLTime(PG2Time(PInt64(FPQparamValues[Index])^), ConSettings^.WriteFormatSettings, True)
                    else Result := DateTimeToRawSQLTime(PG2Time(PDouble(FPQparamValues[Index])^), ConSettings^.WriteFormatSettings, True);
      stDate:       Result := DateTimeToRawSQLDate(PG2Date(PInteger(FPQparamValues[Index])^), ConSettings^.WriteFormatSettings, true);
      stTimestamp:  if Finteger_datetimes
                    then Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PInt64(FPQparamValues[Index])^), ConSettings^.WriteFormatSettings, True)
                    else Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PDouble(FPQparamValues[Index])^), ConSettings^.WriteFormatSettings, True);
      else
        Connection.GetBinaryEscapeString(FPQparamValues[Index], FPQparamLengths[Index], Result);
    end;
  end else
    Result := FParamValues[Index];
end;

function TZPostgreSQLPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := PGPreparableTokens;
end;

procedure TZPostgreSQLPreparedStatement.Unprepare;
begin
  if fPGExecMode = pgemV3_Async then
    FlushPendingResults;
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) and (FRawPlanName <> '') and not Findeterminate_datatype then
    ExecuteInternal('DEALLOCATE "'+FRawPlanName+'"', TEIPQUnPrepareModes[FPGExecMode]);
  inherited Unprepare;
  Findeterminate_datatype := False;
  FRawPlanName := '';
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatement.UnPrepareInParameters;
begin
  { release allocated memory }
  InternalSetInParamCount(0);
  Findeterminate_datatype := False;
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
    Fconn := GetConnectionHandle;
  end;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength , '0'));
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLCallableStatement.CreateResultSet(const SQL: string;
      QueryHandle: PPGresult): IZResultSet;
var
  NativeResultSet: TZAbstractPostgreSQLStringResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if fServerCursor then
    NativeResultSet := TZServerCursorPostgreSQLStringResultSet.Create(Self, SQL, Fconn,
      QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else
    NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, SQL, Fconn,
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
  QueryHandle: PPGresult;
begin
  Result := nil;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.PQExec(Fconn, PAnsiChar(ASQL));
  if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
    HandlePostgreSQLError(Self, FPlainDriver, Fconn, lcExecute,
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
  QueryHandle: PPGresult;
begin
  Result := -1;
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := FPlainDriver.PQExec(Fconn, PAnsiChar(ASQL));
  if not PGSucceeded(FPlainDriver.PQerrorMessage(Fconn)) then
    HandlePostgreSQLError(Self, FPlainDriver, Fconn, lcExecute,
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

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
SetLength(PGPreparableTokens, 5);
PGPreparableTokens[0].MatchingGroup := 'SELECT';
PGPreparableTokens[Ord(dmlInsert)].MatchingGroup := 'INSERT';
PGPreparableTokens[Ord(dmlUpdate)].MatchingGroup := 'UPDATE';
PGPreparableTokens[Ord(dmlDelete)].MatchingGroup := 'DELETE';
PGPreparableTokens[4].MatchingGroup := 'VALUES';

end.

