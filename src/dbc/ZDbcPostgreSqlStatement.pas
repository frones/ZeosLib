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
{$IFDEF FPC}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
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

  TZPostgreSQLPreparedStatement = class;
  TPGArrayDMLStmt = record
    Obj: TZPostgreSQLPreparedStatement;
    Intf: IZPreparedStatement;
  end;


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
    FPGArrayDMLStmts: array[TArrayDMLType] of TPGArrayDMLStmt;
    function CheckPrepareSwitchMode: Boolean;
    procedure InternalRealPrepare;
    procedure AllocBuf(Index: Integer; Len: LengthInt; ParamFormat: Integer);
    function OIDToSQLType(Index: Integer; SQLType: TZSQLType): TZSQLType;
    function ExecuteDMLBatchWithUnnestVarlenaArrays: PPGresult;
  protected
    procedure BindNull(Index: Integer; var SQLType: TZSQLType); override;
    procedure BindRawStr(Index: Integer; var SQLType: TZSQLType; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; var {%H-}SQLType: TZSQLType; const Buf: RawByteString); override;
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
    function CreateResultSet(QueryHandle: PPGresult; {%H-}ServerCursor: Boolean): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TEICategory): PPGresult; virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
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
  I, C, N, FirstComposePos: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  {$IFNDEF UNICODE}
  tmp: RawByteString;
  List: TStrings;
  {$ENDIF}
  ComparePrefixTokens: TPreparablePrefixTokens;
  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    FIsParamIndex[High(FIsParamIndex)] := Param;
    ToBuff(Value, Result);
  end;
  function IsNumeric(P, PEnd: PChar): Boolean;
  begin
    Result := P<= PEnd;
    repeat
      Result := Result and ((Ord(P^) >= Ord('0')) and (Ord(P^) <= Ord('9')));
      if not Result
      then Break
      else Inc(P);
    until P > PEnd;
  end;
begin
  Result := '';
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
    //{$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    {$IFNDEF UNICODE}
    if ConSettings.AutoEncode
    then List := TStringList.Create
    else List := nil; //satisfy comiler
    {$ENDIF}
    try
      ComparePrefixTokens := PGPreparableTokens;
      N := -1;
      FTokenMatchIndex := -1;
      FParamsCnt := 0;
      FirstComposePos := 0;
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Token.TokenType = ttWord) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens) do
              if Tokens.IsEqual(i, ComparePrefixTokens[C].MatchingGroup, tcInsensitive) then begin
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
              if Tokens.IsEqual(i, ComparePrefixTokens[N].ChildMatches[C], tcInsensitive) then begin
                FTokenMatchIndex := N;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
        if ((Token.P^ = '?') and (Token.L = 1)) or
           ((Token.TokenType = ttWord) and (Token.P^ = '$') and
            IsNumeric(Token.P+1, Token.P+Token.L-1)) then begin
          {$IFDEF UNICODE}
          Add(ZUnicodeToRaw(Tokens.AsString(FirstComposePos, I-1), ConSettings^.ClientCodePage^.CP));
          if (Token.P^ = '?')
          then Add('?', True)
          else Add(UnicodeStringToAscii7(Token.P, Token.L), True);
          {$ELSE}
          Add(Tokens.AsString(FirstComposePos, I-1));
          if (Token.P^ = '?')
          then Add('?', True)
          else Add(TokenAsString(Token^), True);
          {$ENDIF}
          Inc(FParamsCnt);
          fPQParamsFoundInQuery := (Token.P^ <> '?') and (fPQParamsFoundInQuery or (Token.P^ = '$'));
          FirstComposePos := i + 1;
        end {$IFNDEF UNICODE}
        else if ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword: begin
              tmp := ConSettings^.ConvFuncs.ZStringToRaw(TokenAsString(Token^), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
              Token^.P := Pointer(tmp);
              Token^.L := Length(tmp);
              List.Add(tmp); //keep alive
            end;
        end
        {$ENDIF};
      end;
      if (FirstComposePos <= Tokens.Count-1) then
        Add(ConSettings^.ConvFuncs.ZStringToRaw(Tokens.AsString(FirstComposePos, Tokens.Count -1), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
    finally
      FlushBuff(Result);
      Tokens.Free;
      {$IFNDEF UNICODE}
      if ConSettings.AutoEncode then
        List.Free;
      {$ENDIF}
    end;
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
var ArrayDMLType: TArrayDMLType;
begin
  inherited ReleaseImmediat(Sender);
  Fconn := nil;
  QueryHandle := nil;
  FRawPlanName := '';
  fPrepareCnt := 0;
  InternalSetInParamCount(0);
  for ArrayDMLType := low(TArrayDMLType) to high(ArrayDMLType) do
    (FPGArrayDMLStmts[ArrayDMLType].Intf as IImmediatelyReleasable).ReleaseImmediat(Sender);
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
    ExecuteDMLBatchWithUnnestVarlenaArrays;
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

function TZPostgreSQLPreparedStatement.ExecuteDMLBatchWithUnnestVarlenaArrays: PPGresult;
var
  Stmt: TZPostgreSQLPreparedStatement;

  procedure AllocArray(Index: Cardinal; TotalSize: Integer;
    out A: PArrayType; out Buf: Pointer);
  var
    aOID: OID;
    ArrOverhead, BuffSize: Integer;
  begin
    ArrOverhead := ARR_OVERHEAD_NONULLS(1);
    BuffSize := ArrOverhead+TotalSize;
    //alloc mem for the varlena array struct ->
    SetLength(stmt.FParamValues[Index], BuffSize);
    A := Pointer(stmt.FParamValues[Index]);
    SQLTypeToPostgreSQL(TZSQLType(FBatchArrays[Index].VArrayType), FOidAsBlob, aOID);
    //write dimension(s)
    Integer2PG(1, ARR_NDIM(A));
    //indicate nullable items
    Integer2PG(0, @A.flags);
    //Write the OID
    LongWord2PG(aOID, ARR_ELEMTYPE(A));
    //write item count
    Integer2PG(ArrayCount, ARR_DIMS(A));
    //write lower bounds
    Integer2PG(1, ARR_LBOUND(A));
    Buf := ARR_DATA_PTR(A);
    Stmt.FPQparamValues[Index] := a;
    Stmt.FPQparamFormats[Index] := ParamFormatBin;
    Stmt.FPQparamLengths[Index] := BuffSize;
  end;
var
  OffSet, J, I: Cardinal;
  UniTemp: ZWideString;
  D, P: Pointer;
  A: PArrayType;
  CP: word;
  aOID: OID;
  fRawTemp: RawByteString;
  N: Integer; //the ParameterIndex
  SQLType: TZSQLType;
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  FTempRaws: TRawByteStringDynArray;
label FromRaw;
begin
  Result := nil;
  CP := ConSettings^.ClientCodePage.CP;

//introduction
// the posgres server support bulk ops since 8.4
// but the free version of libpq/server simply misses this API:
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

// EH: my third approach ..
// using unnest(array[$1,$2.....]::bigint[]
// performance is 9x faster than single executions
// but we've to send data chunked -> using more than 5000 params lead to performance loss again even if 32/64k are supported
// and there is the final chunk of x params for the unprepared stmt
(*
insert into public.SampleRecord (ID,FirstName,LastName,Amount,BirthDate,LastChange,CreatedAt) values (
unnest(array[$1..$100]::int8[])
,unnest(array[$101..$200]::text[])
,unnest(array[$201..$300]::text[])
,unnest(array[$301..$400]::float8[])
,unnest(array[$401..$500]::timestamp[])
,unnest(array[$501..$600]::int8[])
,unnest(array[$601..$700]::int8[]))
*)
//my final and fastest approch bind binary arrays and pass data once per param and one prepared stmt:
  if FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf = nil then begin
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

  EgonHugeist:

  This approach took me ages. No description somewhere, just two (not working) example on
  Stack overflow... The Array.h ported Macros -> Trash !
  Postgres makes differences between external and internal format.
  The external format passes all items as verlena struct and uses Len=-1 as NullIndicator
  If null we've to send the length indicator only else each value is passed by length+bytes of val

  However all findings happen on debudding the postgres server -> thanks to Jan for helping me.

  Approch is
  *)
    Stmt := TZPostgreSQLPreparedStatement.Create(FPostgreSQLConnection, Info);
    Stmt.FaSQL := fRawTemp;
    Stmt.InternalSetInParamCount(InParamCount);
    Stmt.FParamsCnt := InParamCount;
    Stmt.InternalRealPrepare; //force describe the params to get the array oids
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj := Stmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf := Stmt;
  end else
    Stmt := FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj;
  N := -1;
  for i := 0 to fInParamCount -1 do begin
    SQLType := TZSQLType(FBatchArrays[I].VArrayType);
    D := FBatchArrays[I].VArray;
    P := nil;
    case SQLType of
      stBoolean:    begin
                      AllocArray(I, SizeOf(Byte)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i]);
                        end else begin
                          Integer2PG(SizeOf(Byte), P);
                          PByte(NativeUInt(P)+SizeOf(int32))^ := Ord(TBooleanDynArray(D)[j]);
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Byte));
                        end;
                    end;
      stByte:       begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TByteDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stShort:      begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TShortIntDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stWord:       begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          SmallInt2PG(TWordDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stSmall:      begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TSmallIntDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stInteger:    begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          Integer2PG(TIntegerDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stLongWord:   if (stmt.FParamOIDs[N] = OIDOID) then begin
                      AllocArray(I, SizeOf(LongWord)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(OID));
                        end else begin
                          Integer2PG(SizeOf(OID), P);
                          LongWord2PG(TLongWordDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(OID));
                        end;
                    end else begin
                      AllocArray(I, SizeOf(Int64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(LongWord), P);
                          Int642PG(TLongWordDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Int64));
                        end;
                    end;
      stLong:       begin
                      AllocArray(I, SizeOf(Int64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Int64), P);
                          Int642PG(TInt64DynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Int64));
                        end;
                    end;
      stULong:      begin
                      AllocArray(I, SizeOf(UInt64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Int64), P);
                          Int642PG(Int64(TUInt64DynArray(D)[j]), Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(UInt64));
                        end;
                    end;
      stFloat:      begin
                      AllocArray(I, SizeOf(Single)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Single));
                        end else begin
                          Integer2PG(SizeOf(Single), P);
                          Single2PG(TSingleDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Single));
                        end;
                    end;
      stDouble:     begin
                      AllocArray(I, SizeOf(Double)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Double), P);
                          Double2PG(TDoubleDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Double));
                        end;
                    end;
      stCurrency:   begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          if (stmt.FParamOIDs[i] = CASHOID)
                          then Currency2PG(TCurrencyDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)))
                          else Double2PG(TCurrencyDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+8);
                        end
                    end;
      stBigDecimal: begin
                      AllocArray(I, SizeOf(Double)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Double));
                        end else begin
                          Integer2PG(SizeOf(Double), P);
                          Double2PG(TExtendedDynArray(D)[j], Pointer(NativeUInt(P)+SizeOf(int32)));
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Double));
                        end;
                    end;
      stDate:       begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          Date2PG(TDateTimeDynArray(D)[j], PInteger(NativeUInt(P)+SizeOf(int32))^);
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stTime:       begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          if Finteger_datetimes
                          then Time2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                          else Time2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                          Inc(NativeUInt(P),SizeOf(int32)+8);
                        end
                    end;
      stTimeStamp:  begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          if Finteger_datetimes
                          then DateTime2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                          else DateTime2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                          Inc(NativeUInt(P),SizeOf(int32)+8);
                        end
                    end;
      stGUID:       begin
                      AllocArray(I, SizeOf(TGUID)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(TGUID));
                        end else begin
                          Integer2PG(SizeOf(TGUID), P);
                          //eh: Network byteOrder?
                          PGUID(NativeUInt(P)+SizeOf(int32))^ := TGUIDDynArray(D)[j];
                          Inc(NativeUInt(P),SizeOf(int32)+SizeOf(TGUID));
                        end
                    end;
      stBytes:      begin
                      N := 0;
                      for J := 0 to ArrayCount -1 do
                        if not (IsNullFromArray(@FBatchArrays[I], j) or (Pointer(TBytesDynArray(D)[j]) = nil)) then
                          Inc(N, Length(TBytesDynArray(D)[j]));
                      AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(@FBatchArrays[I], j) or (Pointer(TBytesDynArray(D)[j]) = nil) then begin
                          Integer2PG(-1, P);
                          Inc(NativeUInt(P),SizeOf(int32));
                        end else begin
                          N := Length(TBytesDynArray(D)[j]);
                          Integer2PG(N, P);
                          //eh: Network byteOrder?
                          Move(Pointer(TBytesDynArray(D)[j])^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                          Inc(NativeUInt(P),SizeOf(int32)+N);
                        end
                    end;
      stString, stUnicodeString: begin
          case FBatchArrays[I].VArrayVariantType of
            {$IFNDEF UNICODE}
            vtString:   begin
                          if not (not ConSettings^.AutoEncode and ZCompatibleCodePages(CP, ConSettings^.CTRL_CP)) then begin
                            SetLength(FTempRaws, ArrayCount);
                            for J := 0 to ArrayCount -1 do
                              FTempRaws[j] := ConSettings.ConvFuncs.ZStringToRaw(TStringDynArray(D)[j], ConSettings.CTRL_CP, CP);
                            D := Pointer(FTempRaws);
                          end;
                          goto FromRaw;
                        end;
            {$ENDIF}
            vtAnsiString: begin
                            if not ZCompatibleCodePages(CP, ZOSCodePage) then begin
                              SetLength(FTempRaws, ArrayCount);
                              for J := 0 to ArrayCount -1 do
                                FTempRaws[j] := Consettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(D)[j], CP);
                              D := Pointer(FTempRaws);
                            end;
                            goto FromRaw;
                          end;
            vtUTF8String: begin
                            if not ZCompatibleCodePages(CP, zCP_UTF8) then begin
                              SetLength(FTempRaws, ArrayCount);
                              for J := 0 to ArrayCount -1 do
                                FTempRaws[j] := Consettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(D)[j], CP);
                              D := Pointer(FTempRaws);
                            end;
                            goto FromRaw;
                          end;
            vtRawByteString:begin
FromRaw:                    N := 0;
                            for j := 0 to ArrayCount -1 do
                              if not IsNullFromArray(@FBatchArrays[I], j) then
                                Inc(N, Length(TRawByteStringDynArray(D)[j]));
                            AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
                            for j := 0 to ArrayCount -1 do
                              if IsNullFromArray(@FBatchArrays[I], j) then begin
                                Integer2PG(-1, P);
                                Inc(NativeUInt(P),SizeOf(int32));
                              end else begin
                                N := Length(TRawByteStringDynArray(D)[j]);
                                Integer2PG(N, P);
                                Move(Pointer(TRawByteStringDynArray(D)[j])^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                                Inc(NativeUInt(P),SizeOf(int32)+N);
                              end;
                            end;
            {$IFDEF UNICODE}
            vtString,
            {$ENDIF}
            vtUnicodeString: begin
                              SetLength(FTempRaws, ArrayCount);
                              for J := 0 to ArrayCount -1 do
                                FTempRaws[j] := ZUnicodeToRaw(TUnicodeStringDynArray(D)[j], CP);
                              D := Pointer(FTempRaws);
                              goto FromRaw;
                            end;
            vtCharRec:  begin
                          N := 0;
                          OffSet := ArrayCount;
                          for J := 0 to ArrayCount -1 do begin
                            Dec(OffSet, Ord(not ((ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) and (TZCharRecDynArray(D)[j].Len > 0) and not IsNullFromArray(@FBatchArrays[I], j)))));
                            Inc(N, TZCharRecDynArray(D)[j].Len*Byte(Ord(not IsNullFromArray(@FBatchArrays[I], j))));
                          end;
                          if OffSet <> Cardinal(ArrayCount) then begin
                            SetLength(FTempRaws, ArrayCount); //EH: localize all ... shit!!!
                            for J := 0 to ArrayCount -1 do
                              if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) or (TZCharRecDynArray(D)[j].Len = 0) then
                                ZSetString(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, FTempRaws[j])
                              else if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, zCP_UTF16) then
                                FTempRaws[j] := PUnicodeToRaw(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, CP)
                              else begin
                                UniTemp := PRawToUnicode(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, TZCharRecDynArray(D)[j].CP);
                                FTempRaws[j] := ZUnicodeToRaw(UniTemp, CP)
                              end;
                            D := Pointer(FTempRaws);
                            goto FromRaw;
                          end else begin
                            AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
                            for J := 0 to ArrayCount -1 do
                              if IsNullFromArray(@FBatchArrays[I], j) then begin
                                Integer2PG(-1, P);
                                Inc(NativeUInt(P),SizeOf(int32));
                              end else begin
                                N := TZCharRecDynArray(D)[j].Len;
                                Integer2PG(N, P);
                                Move(TZCharRecDynArray(D)[j].P^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                                Inc(NativeUInt(P),SizeOf(int32)+N);
                              end;
                          end;
                        end;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          N := 0;
          if (FInParamTypes[i] = stBinaryStream) and FOidAsBlob then
            SetLength(Stmt.fParamLobs, ArrayCount);
          for J := 0 to ArrayCount -1 do
            if (TInterfaceDynArray(D)[j] <> nil) and Supports(TInterfaceDynArray(D)[j], IZBlob, TempBlob) and not TempBlob.IsEmpty then
              if FInParamTypes[i] in [stUnicodeStream, stAsciiStream] then begin
                if TempBlob.IsClob then
                  TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP)
                else begin
                  fRawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                  TempBlob := TZAbstractClob.CreateWithData(Pointer(fRawTemp), Length(fRawTemp), Cp, ConSettings);
                  TInterfaceDynArray(D)[j] := TempBlob;
                end;
                Inc(N,TempBlob.Length);
              end else if FOidAsBlob then begin
                WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
                  Fconn, 0, ChunkSize);
                WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                Stmt.fParamLobs[j] := WriteTempBlob;
                Inc(N, SizeOf(OID));
              end else
                Inc(N, TempBlob.Length);
          AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
          if (FInParamTypes[i] = stBinaryStream) and FOidAsBlob then begin
            for j := 0 to ArrayCount -1 do
              if Stmt.fParamLobs[j] = nil then begin
                Integer2PG(-1, P);
                Inc(NativeUInt(P),SizeOf(int32));
              end else begin
                Integer2PG(SizeOf(OID), P);
                WriteTempBlob := Stmt.fParamLobs[j] as IZPostgreSQLOidBlob;
                LongWord2PG(WriteTempBlob.GetBlobOid, Pointer(NativeUInt(P)+SizeOf(int32)));
                Inc(NativeUInt(P),SizeOf(int32)+SizeOf(OID));
              end;
            SetLength(Stmt.fParamLobs, 0);
          end else begin
            AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
            for J := 0 to ArrayCount -1 do
              if not ((TInterfaceDynArray(D)[j] <> nil) and Supports(TInterfaceDynArray(D)[j], IZBlob, TempBlob) and not TempBlob.IsEmpty) then begin
                Integer2PG(-1, P);
                Inc(NativeUInt(P),SizeOf(int32));
              end else begin
                N := TempBlob.Length;
                Integer2PG(N, P);
                Move(TempBlob.GetBuffer^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                Inc(NativeUInt(P),SizeOf(int32)+N);
              end;
          end;
        end;
    end;
  end;
  try
    Stmt.ExecuteInternal(Stmt.FASQL, eicExecPrepStmtV3);
    FPlainDriver.PQclear(Stmt.QueryHandle);
  finally
    //flush the mem
    for i := 0 to FInParamCount -1 do
      Stmt.FParamValues[i] := '';
  end;
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

function TZPostgreSQLPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := @PGPreparableTokens;
end;

procedure TZPostgreSQLPreparedStatement.Unprepare;
var ArrayDMLType: TArrayDMLType;
begin
  if fPGExecMode = pgemV3_Async then
    FlushPendingResults;
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) and (FRawPlanName <> '') and not Findeterminate_datatype then
    ExecuteInternal('DEALLOCATE "'+FRawPlanName+'"', TEIPQUnPrepareModes[FPGExecMode]);
  inherited Unprepare;
  Findeterminate_datatype := False;
  FRawPlanName := '';
  for ArrayDMLType := low(TArrayDMLType) to high(ArrayDMLType) do begin
    FPGArrayDMLStmts[ArrayDMLType].Obj := nil;
    FPGArrayDMLStmts[ArrayDMLType].Intf := nil;
  end;
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

