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

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql, ZDbcUtils;

type
  TPQV3ExecCatagory = (pqExecute, pqExecPrepared, pqPrepare, pqUnPrepare);
  TArrayDMLType = (dmlInsert = 1, dmlUpdate, dmlDelete);

  TZPostgreSQLPreparedStatementV3 = class; //forward
  TPGArrayDMLStmt = record
    Obj: TZPostgreSQLPreparedStatementV3;
    Intf: IZPreparedStatement;
  end;

  {** implements a abstract prepared statement for PostgreSQL protocol V3+ }

  TZAbstractPostgreSQLPreparedStatementV3 = class(TZRawParamDetectPreparedStatement)
  private
    FPostgreSQLConnection: IZPostgreSQLConnection;//local connection object
    FPlainDriver: TZPostgreSQLPlainDriver; //PG API
    FconnAddress: PPGconn; //the Connection-Handle
    Fres: TPGresult; //Current query handle we'd obtained
    FRawPlanName: RawByteString; //a name we use to prepare (oddly PG still has no handle instead)
    FOidAsBlob: Boolean; //are blob's threaded as oid-lobs?
    Findeterminate_datatype, //did PG Fail to determine the datatypes? (mostly just because of bad queries)
    fAsyncQueries, //get the GetMoreResults logic with multiple results or SingleRowMode running
    fServerCursor, //PQsingleRowMode? is implizit the Async api
    FUseEmulatedStmtsOnly: Boolean; //no Prepareds?
    FUndefinedVarcharAsStringLength: Integer; //compatibility: in earlier version of Zeos the fieldlength for VARCHAR without given length was 255 now we assume text instead
    fPrepareCnt: Cardinal; //we have no stmt handle! The same object might be reused with another SQL
                           //and the previous used planname did run into the oddly broken transaction issue of pg
                           //we need a secondary uniqueness idea for the planname as a fallback
    Finteger_datetimes: Boolean;  //what's the binary format to bind datetimes? Integer or Double
    FPQparamValues: TPQparamValues; //postgres array of data
    FPQparamLengths: TPQparamLengths; //usually just for binary bindings used PG up until today loves the StrLen() instead of given lengtes for the strings (they can't store #0 bytes)
    FPQparamFormats: TPQparamFormats; //indicate binary or string format for the bindings
    FPQParamOIDs: TPQparamTypes; //the Parameter OID's
    FPQResultFormat: Integer; //which format will the tubles have binary or string? note if any unsupported oid  given we use string format only as a fallback
    FPGArrayDMLStmts: array[TArrayDMLType] of TPGArrayDMLStmt;
    FParamNames: TRawByteStringDynArray;
    FOutParamCount: Integer;
    FMinExecCount2Prepare: Integer; //how many executions must be done to fall into a real prepared mode?
    FExecCount: Integer; //How often did we execute the stmt until we reached MinExecCount2Prepare?
    FOrgSQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
    function CheckPrepareSwitchMode: Boolean;
    procedure InternalRealPrepare;
    function OIDToSQLType(Index: Integer; SQLType: TZSQLType): TZSQLType;
    function ExecuteDMLBatchWithUnnestVarlenaArrays: TPGresult;
    procedure LinkParam2PG(Index: Integer; Buf: Pointer; Len: LengthInt; ParamFormat: Integer);
  protected
    procedure SetBindCapacity(Capacity: Integer); override;

    procedure BindNull(Index: Integer; SQLType: TZSQLType); override;
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;

    procedure CheckParameterIndex(Value: Integer); override;
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; override;
  protected
    procedure FlushPendingResults;
    function CreateResultSet({%H-}ServerCursor: Boolean): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      Category: TPQV3ExecCatagory): TPGresult; virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;
  public
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = '';
      {%H-}PrecisionOrSize: LengthInt = 0; {%H-}Scale: LengthInt = 0); override;
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings);
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
  end;

  {** implements a prepared statement for PostgreSQL protocol V3+ }
  TZPostgreSQLPreparedStatementV3 = class(TZAbstractPostgreSQLPreparedStatementV3, IZPreparedStatement)
  public
    procedure SetCurrency(Index: Integer; const Value: Currency); reintroduce;
  end;

  {** implements a emulated prepared statement for PostgresSQL protocol V2 }
  TZPostgrePreparedStatementV2 = class(TZPostgreSQLPreparedStatementV3)
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings);
    procedure Unprepare; override;
  end;

  {** implements a statement for PostgresSQL }
  TZPostgreSQLStatement = class(TZPostgrePreparedStatementV2, IZStatement)
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      Info: TStrings); overload;
  end;

  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(Mode: TZCallExecKind; const
      StoredProcName: String): TZAbstractPreparedStatement2; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils,
  ZEncoding, ZDbcProperties, ZTokenizer, Types, ZDbcResultSet, ZClasses;


var
  PGPreparableTokens: TPreparablePrefixTokens;
const
  ParamFormatBin = 1;
  ParamFormatStr = 0;

{ TZAbstractPostgreSQLPreparedStatementV3 }

procedure TZAbstractPostgreSQLPreparedStatementV3.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
begin
  inherited BindBinary(Index, SQLType, Buf, Len);
  LinkParam2PG(Index, Buf, Len, ParamFormatBin);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  if OIDToSQLType(Index, stBoolean) = stBoolean then begin
    inherited BindBoolean(Index, Value);
    LinkParam2PG(Index, @BindList[Index].Value, SizeOf(Byte), ParamFormatBin);
    PByte(FPQparamValues[Index])^ := Ord(Value);
  end else BindSignedOrdinal(Index, BindList.SQLTypes[Index], Ord(Value));
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
begin
  inherited BindDateTime(Index, OIDToSQLType(Index, SQLType), Value);
  case BindList.SQLTypes[Index] of
    stTime:     begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], 8, ParamFormatBin);
                  if Finteger_datetimes
                  then Time2PG(Value, PInt64(FPQparamValues[Index])^)
                  else Time2PG(Value, PDouble(FPQparamValues[Index])^);
                end;
    stDate:     begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Integer), ParamFormatBin);
                  Date2PG(Value, PInteger(FPQparamValues[Index])^);
                end;
    stTimeStamp:begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], 8, ParamFormatBin);
                  if Finteger_datetimes
                  then DateTime2PG(Value, PInt64(FPQparamValues[Index])^)
                  else DateTime2PG(Value, PDouble(FPQparamValues[Index])^);
                end;
    else case SQLType of
      stTime: BindRawStr(Index, DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False));
      stDate: BindRawStr(Index, DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False));
      stTimeStamp: BindRawStr(Index, DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, False));
    end;
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
begin
  inherited BindDouble(Index, OIDToSQLType(Index, SQLType), Value);
  case BindList.SQLTypes[Index] of
    stBoolean:  begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Byte), ParamFormatBin);
                  PByte(FPQparamValues[Index])^ := Ord(Value <> 0);
                end;
    stSmall:    begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(SmallInt), ParamFormatBin);
                  SmallInt2PG(SmallInt(Trunc(Value)), FPQparamValues[Index]);
                end;
    stInteger:  begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Integer), ParamFormatBin);
                  Integer2PG(Integer(Trunc(Value)), FPQparamValues[Index]);
                end;
    stLongWord: begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Cardinal), ParamFormatBin);
                  Cardinal2PG(Cardinal(Trunc(Value)), FPQparamValues[Index]);
                end;
    stLong:     begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Int64), ParamFormatBin);
                  Int642PG(Trunc(Value), FPQparamValues[Index]);
                end;
    stFloat:    begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Single), ParamFormatBin);
                  Single2PG(Value, FPQparamValues[Index]);
                end;
    stDouble:   begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Double), ParamFormatBin);
                  Double2PG(Value, FPQparamValues[Index]);
                end;
    stCurrency: begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Currency), ParamFormatBin);
                  if FPQParamOIDs[index] = CASHOID
                  then Currency2PGCash(Value, FPQparamValues[Index])
                  else Double2PG(Value, FPQparamValues[Index]);
                end;
    else BindRawStr(Index, FloatToSqlRaw(Value));
  end;

end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var
  WriteTempBlob: IZPostgreSQLOidBlob;
begin
  inherited BindLob(Index, SQLType, Value); //keep alive and play with refcounts
  if (Value <> nil) and not Value.IsEmpty then
    if ((SQLType = stBinaryStream) and FOidAsBlob) then begin
      WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, 0, ChunkSize);
      WriteTempBlob.WriteBuffer(Value.GetBuffer, Value.Length);
      BindSignedOrdinal(Index, SQLType, WriteTempBlob.GetBlobOid);
      WriteTempBlob := nil;
    end else begin
      FPQparamValues[Index] := Value.GetBuffer;
      FPQparamLengths[Index] := Value.Length;
      if SQLType = stBinaryStream
      then FPQparamFormats[Index] := ParamFormatBin
      else FPQparamFormats[Index] := ParamFormatStr;
    end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindNull(Index: Integer;
  SQLType: TZSQLType);
begin
  inherited BindNull(Index, OIDToSQLType(Index, SQLType));
  FPQparamFormats[Index] := ParamFormatStr;
  FPQparamValues[Index] := nil
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  inherited BindRawStr(Index, Buf, Len);
  LinkParam2PG(Index, Buf, Len, ParamFormatStr);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  inherited BindRawStr(Index, Value); //localize
  LinkParam2PG(Index, Pointer(Value), Length(Value), ParamFormatStr);
  if Pointer(Value) = nil then
    FPQparamValues[Index] := PEmptyAnsiString;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: Int64);
begin
  inherited BindSignedOrdinal(Index, OIDToSQLType(Index, SQLType), Value);
  case BindList.SQLTypes[Index] of
    stBoolean:  begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Byte), ParamFormatBin);
                  PByte(FPQparamValues[Index])^ := Ord(Value <> 0);
                end;
    stSmall:    begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(SmallInt), ParamFormatBin);
                  SmallInt2PG(SmallInt(Value), FPQparamValues[Index]);
                end;
    stInteger:  begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Integer), ParamFormatBin);
                  Integer2PG(Integer(Value), FPQparamValues[Index]);
                end;
    stBinaryStream, //oidlobs
    stLongWord: begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Cardinal), ParamFormatBin);
                  Cardinal2PG(Cardinal(Value), FPQparamValues[Index]);
                end;
    stLong,
    stCurrency: begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Int64), ParamFormatBin);
                  Int642PG(Value, FPQparamValues[Index]);
                end;
    stFloat:    begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Single), ParamFormatBin);
                  Single2PG(Value, FPQparamValues[Index]);
                end;
    stDouble:   begin
                  LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Double), ParamFormatBin);
                  Double2PG(Value, FPQparamValues[Index]);
                end;
    else BindRawStr(Index, IntToRaw(Value));
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: UInt64);
begin
  BindSignedOrdinal(Index, SQLType, Int64(Value));
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.CheckParameterIndex(Value: Integer);
begin
  if not Prepared then
    Prepare;
  if (BindList.Capacity < Value+1) then
    if fRawPlanname <> ''
    then raise EZSQLException.Create(SInvalidInputParameterCount)
    else inherited CheckParameterIndex(Value);
end;

function TZAbstractPostgreSQLPreparedStatementV3.CheckPrepareSwitchMode: Boolean;
begin
  Result := not Findeterminate_datatype and ((not FUseEmulatedStmtsOnly) or (ArrayCount > 0 )) and
    (FRawPlanName = '') and (TokenMatchIndex <> -1) and ((ArrayCount > 0 ) or (FExecCount = FMinExecCount2Prepare));
end;

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractPostgreSQLPreparedStatementV3.Create(
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPostgreSQLConnection := Connection;
  FOidAsBlob := StrToBoolEx(Self.Info.Values[DSProps_OidAsBlob]) or Connection.IsOidAsBlob;
  FPlainDriver := Connection.GetPlainDriver;
  //ResultSetType := rtScrollInsensitive;
  FconnAddress := Connection.GetPGconnAddress;
  Findeterminate_datatype := False;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'));
  { see http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
    the pgBouncer does not support the RealPrepareds.... }
  FUseEmulatedStmtsOnly := not Assigned(FplainDriver.PQexecParams) or not Assigned(FplainDriver.PQexecPrepared) or
    StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_EmulatePrepares, 'FALSE'));
  Findeterminate_datatype := FUseEmulatedStmtsOnly;
  Finteger_datetimes := Connection.integer_datetimes;
  FPQResultFormat := ParamFormatStr;
  fPrepareCnt := 0;
  //JDBC prepares after 4th execution
  if not FUseEmulatedStmtsOnly
  then FMinExecCount2Prepare := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_MinExecCntBeforePrepare, '2'), 2)
  else FMinExecCount2Prepare := -1;
  fAsyncQueries := False;(* not ready yet! StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_ExexAsync, 'FALSE'))
    and Assigned(FplainDriver.PQsendQuery) and Assigned(FplainDriver.PQsendQueryParams) and
    Assigned(FplainDriver.PQsendQueryPrepared);*)
  fServerCursor := fAsyncQueries and StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_SingleRowMode, 'FALSE'))
end;

function TZAbstractPostgreSQLPreparedStatementV3.CreateResultSet(
  ServerCursor: Boolean): IZResultSet;
var
  NativeResultSet: TZAbstractPostgreSQLStringResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if fServerCursor
  then NativeResultSet := TZServerCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, FconnAddress,
      @Fres, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, FconnAddress,
      @Fres, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);

  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency = rcUpdatable) or fServerCursor then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL, nil,
      ConSettings);
    if (GetResultSetConcurrency = rcUpdatable) then begin
      CachedResultSet.SetConcurrency(rcUpdatable);
      CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
        Self,  NativeResultSet.GetMetadata));
    end;
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result);
end;

function TZAbstractPostgreSQLPreparedStatementV3.ExecuteDMLBatchWithUnnestVarlenaArrays: TPGresult;
var
  Stmt: TZPostgreSQLPreparedStatementV3;
  SQLType: TZSQLType;

  procedure AllocArray(Index: Cardinal; TotalSize: Integer;
    out A: PArrayType; out Buf: PAnsiChar);
  var
    aOID: OID;
    BuffSize: Integer;
  begin
    BuffSize := ARR_OVERHEAD_NONULLS(1)+TotalSize;
    //alloc mem for the varlena array struct ->
    A := Stmt.BindList.AquireCustomValue(Index, SQLType, BuffSize);
    SQLTypeToPostgreSQL(SQLType, FOidAsBlob, aOID);
    //write dimension(s)
    Integer2PG(1, ARR_NDIM(A));
    //indicate nullable items
    Integer2PG(0, @A.flags);
    //Write the OID
    Cardinal2PG(aOID, ARR_ELEMTYPE(A));
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
  D: Pointer;
  P: PAnsiChar;
  A: PArrayType;
  CP: word;
  aOID: OID;
  X: Integer;
  fRawTemp: RawByteString;
  N: Integer; //the ParameterIndex
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  FTempRaws: TRawByteStringDynArray;
  PGresult: TPGresult;
  Arr: PZArray;
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
  if FPlainDriver.PQbeginBatchMode(FconnAddress^) = Ord(PGRES_COMMAND_OK) then begin
   ///bind data
      if FplainDriver.PQsendQueryPrepared(FconnAddress^, Pointer(FRawPlanName), FInParamCount,
         Pointer(FPQparamValues), Pointer(FPQparamLengths), Pointer(FPQparamFormats),
         ParamFormatStr) <> Ord(PGRES_COMMAND_OK) then
      HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    end;
    if not FplainDriver.PQsendEndBatch(FconnAddress^)  <> Ord(PGRES_COMMAND_OK) then
       HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    J := 0;
    while True do begin
      //now we've to obtain all resuls, else we can't leave the batchmode
      I := FplainDriver.PQgetNextQuery(FconnAddress^);
      if i = Ord(PGRES_BATCH_END) then
        Break;
      P := FplainDriver.PQgetResult(FconnAddress^);
      if P <> nil then begin
        FplainDriver.PQclear(P);
        Inc(J);
      end else
        break;
    end;
    if J <> ArrayCount+1 then
       raise Exception.Create('Fehlermeldung');
    if FplainDriver.PQisInBatchMode(FconnAddress^) = Ord(PGRES_COMMAND_OK) then
      FplainDriver.PQEndBatchMode(FconnAddress^);
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
unnest(array[$1]::int8[])
,unnest(array[$2]::text[])
,unnest(array[3]::text[])
,unnest(array[$4]::numeric[])
,unnest(array[$5]::timestamp[])
,unnest(array[$6]::int8[])
,unnest(array[$7]::int8[]))
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
        SQLTypeToPostgreSQL(TZSQLType(BindList.Arrays[Offset].VArrayType), FOidAsBlob, aOID);
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

  However all findings happen on debugging the postgres server -> thanks to Jan for helping me.

  *)
    Stmt := TZPostgreSQLPreparedStatementV3.Create(FPostgreSQLConnection, '', Info);
    Stmt.FaSQL := fRawTemp;
    Stmt.SetParamCount(BindList.Count);
    Stmt.InternalRealPrepare; //force describe the params to get the array oids
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj := Stmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf := Stmt;
  end else
    Stmt := FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj;
  N := -1;
  for i := 0 to bindList.Count -1 do begin
    if BindList[I].BindType <> zbtArray then
      Continue;
    Arr := BindList[I].Value; //localize -> next steps will free the memory
    SQLType := TZSQLType(Arr.VArrayType);
    D := Arr.VArray;
    P := nil;
    case SQLType of
      stBoolean:    begin
                      AllocArray(I, SizeOf(Byte)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i]);
                        end else begin
                          Integer2PG(SizeOf(Byte), P);
                          PByte(P+SizeOf(int32))^ := Ord(TBooleanDynArray(D)[j]);
                          Inc(P,SizeOf(int32)+SizeOf(Byte));
                        end;
                    end;
      stByte:       begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TByteDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stShort:      begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TShortIntDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stWord:       begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          SmallInt2PG(TWordDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stSmall:      begin
                      AllocArray(I, SizeOf(SmallInt)*ArrayCount+SizeOf(Int32)*ArrayCount, A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(SmallInt));
                        end else begin
                          Integer2PG(SizeOf(SmallInt), P);
                          SmallInt2PG(TSmallIntDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                        end;
                    end;
      stInteger:    begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          Integer2PG(TIntegerDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stLongWord:   if (stmt.FPQParamOIDs[N] = OIDOID) then begin
                      AllocArray(I, SizeOf(OID)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(OID));
                        end else begin
                          Integer2PG(SizeOf(OID), P);
                          Cardinal2PG(TLongWordDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(OID));
                        end;
                    end else begin
                      AllocArray(I, SizeOf(Int64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(LongWord), P);
                          Int642PG(TLongWordDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Int64));
                        end;
                    end;
      stLong:       begin
                      AllocArray(I, SizeOf(Int64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Int64), P);
                          Int642PG(TInt64DynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Int64));
                        end;
                    end;
      stULong:      begin
                      AllocArray(I, SizeOf(UInt64)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Int64), P);
                          Int642PG(Int64(TUInt64DynArray(D)[j]),P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(UInt64));
                        end;
                    end;
      stFloat:      begin
                      AllocArray(I, SizeOf(Single)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Single));
                        end else begin
                          Integer2PG(SizeOf(Single), P);
                          Single2PG(TSingleDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Single));
                        end;
                    end;
      stDouble:     begin
                      AllocArray(I, SizeOf(Double)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Int64));
                        end else begin
                          Integer2PG(SizeOf(Double), P);
                          Double2PG(TDoubleDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Double));
                        end;
                    end;
      stCurrency:   if (stmt.FPQParamOIDs[i] = CASHOID) then begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          Currency2PGCash(TCurrencyDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+8);
                        end;
                    end else begin
                      AllocArray(I, MaxCurr2NumSize*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          X := SizeOf(int32);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], MaxCurr2NumSize);
                        end else begin
                          Currency2PGNumeric(TCurrencyDynArray(D)[j], P+SizeOf(int32), x);
                          Integer2PG(X, P);
                          Inc(P,SizeOf(int32)+X);
                          Dec(Stmt.FPQparamLengths[i], MaxCurr2NumSize-X);
                        end
                    end;
      stBigDecimal: begin
                      AllocArray(I, SizeOf(Double)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Double));
                        end else begin
                          Integer2PG(SizeOf(Double), P);
                          Double2PG(TExtendedDynArray(D)[j],P+SizeOf(int32));
                          Inc(P,SizeOf(int32)+SizeOf(Double));
                        end;
                    end;
      stDate:       begin
                      AllocArray(I, SizeOf(Integer)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(Integer));
                        end else begin
                          Integer2PG(SizeOf(Integer), P);
                          Date2PG(TDateTimeDynArray(D)[j], PInteger(NativeUInt(P)+SizeOf(int32))^);
                          Inc(P,SizeOf(int32)+SizeOf(Integer));
                        end;
                    end;
      stTime:       begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          if Finteger_datetimes
                          then Time2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                          else Time2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                          Inc(P,SizeOf(int32)+8);
                        end
                    end;
      stTimeStamp:  begin
                      AllocArray(I, 8*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], 8);
                        end else begin
                          Integer2PG(8, P);
                          if Finteger_datetimes
                          then DateTime2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                          else DateTime2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                          Inc(P,SizeOf(int32)+8);
                        end
                    end;
      stGUID:       begin
                      AllocArray(I, SizeOf(TGUID)*ArrayCount+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                          Dec(Stmt.FPQparamLengths[i], SizeOf(TGUID));
                        end else begin
                          Integer2PG(SizeOf(TGUID), P);
                          //eh: Network byteOrder?
                          PGUID(NativeUInt(P)+SizeOf(int32))^ := TGUIDDynArray(D)[j];
                          Inc(P,SizeOf(int32)+SizeOf(TGUID));
                        end
                    end;
      stBytes:      begin
                      N := 0;
                      for J := 0 to ArrayCount -1 do
                        if not (IsNullFromArray(Arr, j) or (Pointer(TBytesDynArray(D)[j]) = nil)) then
                          Inc(N, Length(TBytesDynArray(D)[j]));
                      AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
                      for j := 0 to ArrayCount -1 do
                        if IsNullFromArray(Arr, j) or (Pointer(TBytesDynArray(D)[j]) = nil) then begin
                          Integer2PG(-1, P);
                          Inc(P,SizeOf(int32));
                        end else begin
                          N := Length(TBytesDynArray(D)[j]);
                          Integer2PG(N, P);
                          //eh: Network byteOrder?
                          Move(Pointer(TBytesDynArray(D)[j])^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                          Inc(P,SizeOf(int32)+N);
                        end
                    end;
      stString, stUnicodeString: begin
          case Arr.VArrayVariantType of
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
            {$IFNDEF NO_ANSISTRING}
            vtAnsiString: begin
                            if not ZCompatibleCodePages(CP, ZOSCodePage) then begin
                              SetLength(FTempRaws, ArrayCount);
                              for J := 0 to ArrayCount -1 do
                                FTempRaws[j] := Consettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(D)[j], CP);
                              D := Pointer(FTempRaws);
                            end;
                            goto FromRaw;
                          end;
            {$ENDIF}
            {$IFNDEF NO_UTF8STRING}
            vtUTF8String: begin
                            if not ZCompatibleCodePages(CP, zCP_UTF8) then begin
                              SetLength(FTempRaws, ArrayCount);
                              for J := 0 to ArrayCount -1 do
                                FTempRaws[j] := Consettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(D)[j], CP);
                              D := Pointer(FTempRaws);
                            end;
                            goto FromRaw;
                          end;
            {$ENDIF}
            vtRawByteString:begin
FromRaw:                    N := 0;
                            for j := 0 to ArrayCount -1 do
                              if not IsNullFromArray(Arr, j) then
                                Inc(N, Length(TRawByteStringDynArray(D)[j]));
                            AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
                            for j := 0 to ArrayCount -1 do
                              if IsNullFromArray(Arr, j) then begin
                                Integer2PG(-1, P);
                                Inc(P,SizeOf(int32));
                              end else begin
                                N := Length(TRawByteStringDynArray(D)[j]);
                                Integer2PG(N, P);
                                Move(Pointer(TRawByteStringDynArray(D)[j])^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                                Inc(P,SizeOf(int32)+N);
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
                            Dec(OffSet, Ord(not ((ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) and (TZCharRecDynArray(D)[j].Len > 0) and not IsNullFromArray(Arr, j)))));
                            Inc(N, TZCharRecDynArray(D)[j].Len*Byte(Ord(not IsNullFromArray(Arr, j))));
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
                              if IsNullFromArray(Arr, j) then begin
                                Integer2PG(-1, P);
                                Inc(P,SizeOf(int32));
                              end else begin
                                N := TZCharRecDynArray(D)[j].Len;
                                Integer2PG(N, P);
                                Move(TZCharRecDynArray(D)[j].P^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                                Inc(P,SizeOf(int32)+N);
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
          for J := 0 to ArrayCount -1 do
            if (TInterfaceDynArray(D)[j] <> nil) and Supports(TInterfaceDynArray(D)[j], IZBlob, TempBlob) and not TempBlob.IsEmpty then
              if BindList.SQLTypes[I] in [stUnicodeStream, stAsciiStream] then begin
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
                  FconnAddress^, 0, ChunkSize);
                WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                TInterfaceDynArray(D)[j] := WriteTempBlob;
                Inc(N, SizeOf(OID));
              end else
                Inc(N, TempBlob.Length);
          AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
          if (BindList.SQLtypes[i] = stBinaryStream) and FOidAsBlob then begin
            for j := 0 to ArrayCount -1 do
              if TInterfaceDynArray(D)[j] = nil then begin
                Integer2PG(-1, P);
                Inc(P,SizeOf(int32));
              end else begin
                Integer2PG(SizeOf(OID), P);
                WriteTempBlob := TInterfaceDynArray(D)[j] as IZPostgreSQLOidBlob;
                Cardinal2PG(WriteTempBlob.GetBlobOid,P+SizeOf(int32));
                Inc(P,SizeOf(int32)+SizeOf(OID));
              end;
          end else begin
            AllocArray(I, N+(ArrayCount*SizeOf(int32)), A, P);
            for J := 0 to ArrayCount -1 do
              if not ((TInterfaceDynArray(D)[j] <> nil) and Supports(TInterfaceDynArray(D)[j], IZBlob, TempBlob) and not TempBlob.IsEmpty) then begin
                Integer2PG(-1, P);
                Inc(P,SizeOf(int32));
              end else begin
                N := TempBlob.Length;
                Integer2PG(N, P);
                Move(TempBlob.GetBuffer^, Pointer(NativeUInt(P)+SizeOf(int32))^, N);
                Inc(P,SizeOf(int32)+N);
              end;
          end;
        end;
    end;
  end;
  PGresult := Stmt.ExecuteInternal(Stmt.FASQL, pqExecPrepared);
  try
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
        lcExecute, ASQL, Result);
  finally
    FPlainDriver.PQclear(PGresult); //free postgres mem
    Stmt.BindList.FlushAll; //free allocated mem
  end;
end;

function TZAbstractPostgreSQLPreparedStatementV3.ExecuteInternal(
  const SQL: RawByteString; Category: TPQV3ExecCatagory): TPGresult;
var
  PError: PAnsiChar;
  I, N: Integer;
  TmpSQL: RawByteString;
label retryExecute;
begin
  Result := nil;
  if not Assigned(FconnAddress^) then
    Exit;
  case Category of
    pqExecute: begin
        { Logging Execution }
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcExecute,Self);
        if fAsyncQueries then begin
          if (BindList.Capacity > 0) then begin
            if FplainDriver.PQsendQueryParams(FconnAddress^,
               Pointer(FASQL), BindList.Count-FOutParamCount, Pointer(FPQParamOIDs), Pointer(FPQparamValues),
               Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
              HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^, lcExecute, ASQL, Result)
          end else begin
            if FplainDriver.PQsendQuery(FconnAddress^, Pointer(FASQL)) <> Ord(PGRES_COMMAND_OK) then
              HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^, lcExecute, ASQL, Result)
          end;
          if FServerCursor then
            FPlainDriver.PQsetSingleRowMode(FconnAddress^);
          Result := FPlainDriver.PQgetResult(FconnAddress^); //obtain the first result
        end else begin
          if (BindList.Capacity > 0) then begin
            if not Findeterminate_datatype then begin
              Result := FPlainDriver.PQexecParams(FconnAddress^, Pointer(FASQL),
                BindList.Count-FOutParamCount, Pointer(FPQParamOIDs), Pointer(FPQparamValues),
                Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
              if Assigned(FPlainDriver.PQresultErrorField)
              then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
              else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
              if (PError <> nil) and (PError^ <> #0) then begin
                { check for indermine datatype error}
                if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) = 0) then begin
                  FPlainDriver.PQclear(Result);
                  Findeterminate_datatype := True;
                  goto retryExecute;
                end;
              end else begin
                Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
                CheckPrepareSwitchMode;
              end;
            end else begin
  retryExecute:
              TmpSQL := '';
              N := 0;
              for I := 0 to High(FCachedQueryRaw) do
                if FIsParamIndex[i] then begin
                  ToBuff(GetInParamLogValue(n), TmpSQL);
                  Inc(N);
                end else
                  ToBuff(FCachedQueryRaw[i], TmpSQL);
                FlushBuff(TmpSQL);
              Result := FPlainDriver.PQExec(FconnAddress^, Pointer(TmpSQL));
            end;
          end else
            Result := FPlainDriver.PQExec(FconnAddress^, Pointer(FASQL));
          if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
            HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
              lcExecute, ASQL, Result);
        end;
      end;
    pqPrepare: begin
        { Logging Execution }
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcPrepStmt,Self);
        if fAsyncQueries then begin
          if not FPlainDriver.PQsendPrepare(FconnAddress^, Pointer(FRawPlanName),
             Pointer(FASQL), BindList.Count-FOutParamCount, Pointer(FPQParamOIDs)) = Ord(PGRES_COMMAND_OK) then
            HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
              lcExecute, ASQL, nil);
        end else begin
          Result := FPlainDriver.PQprepare(FconnAddress^, Pointer(FRawPlanName),
            Pointer(SQL), BindList.Count-FOutParamCount, nil{Pointer(fParamOIDs)});
          if Assigned(FPlainDriver.PQresultErrorField)
          then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
          else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
          if (PError <> nil) and (PError^ <> #0) then
            { check for indermine datatype error}
            if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
              HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
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
      end;
    pqExecPrepared: begin
        { Logging Execution }
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcExecPrepStmt,Self);
        if fAsyncQueries then begin
          if FPlainDriver.PQsendQueryPrepared(FconnAddress^,
             Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
             Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
            HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
              lcExecPrepStmt, ASQL, Result)
          else if FServerCursor then
            FPlainDriver.PQsetSingleRowMode(FconnAddress^);
          Result := FPlainDriver.PQgetResult(FconnAddress^); //obtain the first result
        end else begin
          Result := FPlainDriver.PQexecPrepared(FconnAddress^,
            Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
            Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
          if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
            HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
              lcExecPrepStmt, ASQL, Result);
        end;
      end;
    pqUnprepare: begin
        { Logging Execution }
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcUnprepStmt,Self);
        Result := FPlainDriver.PQExec(FconnAddress^, Pointer(SQL));
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
        if (PError <> nil) and (PError^ <> #0) then
          { check for current transaction is aborted error}
          if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, current_transaction_is_aborted, 5) <> 0) then
            HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^, lcUnprepStmt, ASQL, Result)
          else
            FPostgreSQLConnection.RegisterTrashPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName))
        else if Result <> nil then begin
          FPlainDriver.PQclear(Result);
          Result := nil;
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
function TZAbstractPostgreSQLPreparedStatementV3.ExecutePrepared: Boolean;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  if (DriverManager <> nil) and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  if Findeterminate_datatype or (FRawPlanName = '')
  then Fres := ExecuteInternal(ASQL, pqExecute)
  else Fres := ExecuteInternal(ASQL, pqExecPrepared);

  { Process queries with result sets }
  if FPlainDriver.PQresultStatus(Fres) = PGRES_TUPLES_OK then begin
    Result := True;
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet(fServerCursor);
  end else begin
    Result := False;
    LastUpdateCount := RawToIntDef(
      FPlainDriver.PQcmdTuples(Fres), 0);
    FPlainDriver.PQclear(Fres);
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractPostgreSQLPreparedStatementV3.ExecuteQueryPrepared: IZResultSet;
var
  Status: TZPostgreSQLExecStatusType;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  if (DriverManager <> nil) and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  if Findeterminate_datatype or (FRawPlanName = '')
  then Fres := ExecuteInternal(ASQL, pqExecute)
  else Fres := ExecuteInternal(ASQL, pqExecPrepared);
  Status := FPlainDriver.PQresultStatus(Fres);
  if (Fres <> nil) and (Status = PGRES_TUPLES_OK) then
    if Assigned(FOpenResultSet)
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet(fServerCursor)
  else
    Result := nil;
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
function TZAbstractPostgreSQLPreparedStatementV3.ExecuteUpdatePrepared: Integer;
begin
  PrepareLastResultSetForReuse;
  Result := -1;
  Prepare;
  if (DriverManager <> nil) and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  if Findeterminate_datatype or (FRawPlanName = '') then
    Fres := ExecuteInternal(ASQL, pqExecute)
  else if ArrayCount = 0 then
    Fres := ExecuteInternal(ASQL, pqExecPrepared)
  else begin
    ExecuteDMLBatchWithUnnestVarlenaArrays;
    Exit;
  end;
  if Fres <> nil then
    if (FPlainDriver.PQresultStatus(Fres) = PGRES_TUPLES_OK) and (FHasInOutParams or (FOutParamCount > 0)) then begin
      if not Assigned(LastResultSet) then
        LastResultSet := CreateResultSet(fServerCursor);
      LastUpdateCount := RawToIntDef(FPlainDriver.PQcmdTuples(Fres), 0);
    end else begin
      LastUpdateCount := RawToIntDef(
        FPlainDriver.PQcmdTuples(Fres), 0);
      FPlainDriver.PQclear(Fres);
    end;
  Result := LastUpdateCount;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.FlushPendingResults;
var PQRes: TPGresult;
begin
  while True do begin
    PQRes := FPlainDriver.PQgetResult(FconnAddress^);
    if PQRes = nil
    then break
    else FplainDriver.PQclear(PQRes);
  end;
end;

function TZAbstractPostgreSQLPreparedStatementV3.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := @PGPreparableTokens;
end;

function TZAbstractPostgreSQLPreparedStatementV3.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
var BindValue: PZBindValue;
begin
  BindValue := BindList[ParamIndex];
  case BindValue.BindType of
    zbtNull: Result := 'null';
    zbt8Byte:
      case FPQParamOIDs[ParamIndex] of
        BOOLOID:  Result := BoolStrIntsRaw[PInt64(BindValue)^ <> 0];
        INT8OID:  Result := IntToRaw(PG2Int64(BindValue.Value));
        INT2OID:  Result := IntToRaw(PG2SmallInt(BindValue.Value));
        INT4OID:  Result := IntToRaw(PG2Integer(BindValue.Value));
        OIDOID:   Result := IntToRaw(PG2Cardinal(BindValue.Value));
        FLOAT4OID:Result := FloatToRaw(PG2Single(BindValue.Value));
        FLOAT8OID:Result := FloatToRaw(PG2Double(BindValue.Value));
        CASHOID:  Result := FloatToRaw(PG2Int64(BindValue.Value)/100);
        DATEOID:  Result := DateTimeToRawSQLDate(PG2Date(PInteger(BindValue.Value)^), ConSettings^.WriteFormatSettings, True, '::date');
        TIMEOID:  if Finteger_datetimes
                  then Result := DateTimeToRawSQLTime(PG2Time(PInt64(BindValue.Value)^), ConSettings^.WriteFormatSettings, True, '::time')
                  else Result := DateTimeToRawSQLTime(PG2Time(PDouble(BindValue.Value)^), ConSettings^.WriteFormatSettings, True, '::time');
        TIMESTAMPOID: if Finteger_datetimes
                  then Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PInt64(BindValue.Value)^), ConSettings^.WriteFormatSettings, True, '::timestamp')
                  else Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PDouble(BindValue.Value)^), ConSettings^.WriteFormatSettings, True, '::timestamp');
      end;
    zbtRawString: Connection.GetEscapeString(PAnsiChar(BindValue.Value), Length(RawByteString(BindValue.Value)), Result);
    zbtCharByRef: Connection.GetEscapeString(PAnsiChar(PZCharRec(BindValue.Value)^.P), PZCharRec(BindValue.Value)^.Len, Result);
    zbtBinByRef: Connection.GetBinaryEscapeString(PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len, Result);
    zbtGUID:     Result := #39+GUIDToRaw(BindValue.Value, SizeOf(TGUID), True)+#39;
    zbtBytes: Connection.GetBinaryEscapeString(BindValue.Value, Length(TBytes(BindValue.Value)), Result);
    zbtLob: if BindValue.SQLType = stBinaryStream
            then Connection.GetBinaryEscapeString(IZBlob(BindValue.Value).GetBuffer, IZBlob(BindValue.Value).Length, Result)
            else Connection.GetEscapeString(IZBlob(BindValue.Value).GetBuffer, IZBlob(BindValue.Value).Length, Result);
    zbtPointer: Result := BoolStrIntsRaw[BindValue.Value <> nil];
    //zbtBCD: Result := '';
    else Result := '';
  end;
end;

function TZAbstractPostgreSQLPreparedStatementV3.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, C, N, FirstComposePos, ParamsCnt: Integer;
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
  FOrgSQL := SQL;
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
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
      ParamsCnt := 0;
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
          Inc(ParamsCnt);
          {$IFDEF UNICODE}
          Add(ZUnicodeToRaw(Tokens.AsString(FirstComposePos, I-1), ConSettings^.ClientCodePage^.CP));
          if (Token.P^ = '?')
          then Add('$'+IntToRaw(ParamsCnt), True)
          else Add(UnicodeStringToAscii7(Token.P, Token.L), True);
          {$ELSE}
          Add(Tokens.AsString(FirstComposePos, I-1));
          if (Token.P^ = '?')
          then Add('$'+IntToRaw(ParamsCnt), True)
          else Add(TokenAsString(Token^), True);
          {$ENDIF}
          FirstComposePos := i + 1;
        end {$IFNDEF UNICODE}
        else if ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier: begin
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
      SetBindCapacity(ParamsCnt);
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

procedure TZAbstractPostgreSQLPreparedStatementV3.InternalRealPrepare;
begin
  FRawPlanName := IntToRaw(FStatementId)+IntToRaw({%H-}NativeUInt(FconnAddress^))+IntToRaw(fPrepareCnt);
  Fres := ExecuteInternal(fASQL, pqPrepare)
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.SetBindCapacity(Capacity: Integer);
begin
  inherited SetBindCapacity(Capacity);
  if (Length(FPQparamValues) <> BindList.Capacity) then begin
    SetLength(FPQparamValues, BindList.Capacity);
    SetLength(FPQparamLengths, BindList.Capacity);
    SetLength(FPQparamFormats, BindList.Capacity);
    SetLength(FPQParamOIDs, BindList.Capacity);
    SetLength(FInParamDefaultValues, BindList.Capacity);
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.LinkParam2PG(Index: Integer;
  Buf: Pointer; Len: LengthInt; ParamFormat: Integer);
begin
  FPQparamLengths[Index] := Len;
  FPQparamValues[Index] := Buf;
  FPQparamFormats[Index] := ParamFormat;
end;

function TZAbstractPostgreSQLPreparedStatementV3.OIDToSQLType(Index: Integer;
  SQLType: TZSQLType): TZSQLType;
begin
  case FPQParamOIDs[Index] of
    INVALIDOID: begin
      SQLTypeToPostgreSQL(SQLType, FOIdAsBLob, FPQParamOIDs[Index]);
      if (Ord(SQLType) > Ord(stBoolean)) and (Ord(SQLType) < Ord(stLongWord)) and not Odd(Ord(SQLType)) then
        SQLType := TZSQLType(Ord(SQLType)+3);
      Result := SQLType;
    end;
    { these types are binary supported by now }
    BOOLOID:    Result := stBoolean;
    BYTEAOID:   Result := stBytes;
    INT8OID:    Result := stLong;
    INT2OID:    Result := stSmall;
    INT4OID:    Result := stInteger;
    OIDOID:     Result := stLongWord;
    FLOAT4OID:  Result := stFloat;
    FLOAT8OID:  Result := stDouble;
    CASHOID:    Result := stCurrency;
    NUMERICOID: if SQLType in [stCurrency, stBigDecimal]
                then Result := SQLType
                else Result := stCurrency;
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

function TZAbstractPostgreSQLPreparedStatementV3.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
var I: Integer;
begin
  Result := inherited AlignParamterIndex2ResultSetIndex(Value);
  for i := Value downto 0 do
    if BindList.ParamTypes[i] in [pctUnknown, pctIn] then
      Dec(Result);
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPostgreSQLPreparedStatementV3.Prepare;
begin
  if fAsyncQueries then
    FlushPendingResults;
  if not Prepared then begin
    Inc(fPrepareCnt);
    inherited Prepare; //we need this step always for Set(A/W)SQL overloads if SQL changes
  end;
  if CheckPrepareSwitchMode then
    InternalRealPrepare;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractPostgreSQLPreparedStatementV3.PrepareInParameters;
var
  res: TPGresult;
  I: Integer;
  pgOID, zOID: OID;
begin
  if (fRawPlanName <> '') and not (Findeterminate_datatype) and (BindList.Capacity > 0) then begin
    if Assigned(FPlainDriver.PQdescribePrepared) then begin
      res := FPlainDriver.PQdescribePrepared(FconnAddress^, Pointer(FRawPlanname));
      try
        BindList.SetCount(FplainDriver.PQnparams(res)+FOutParamCount);
        for i := 0 to BindList.Count-FOutParamCount-1 do begin
          pgOID := FplainDriver.PQparamtype(res, i);
          if (pgOID <> FPQParamOIDs[i]) then begin
            zOID := FPQParamOIDs[i];
            //bind bin again or switch to string format if we do not support the PG Types -> else Error
            FPQParamOIDs[i] := pgOID;
            if (FPQparamValues[I] <> nil) then
              case BindList.SQLTypes[i] of
                stBoolean:  BindBoolean(i, Boolean(PByte(FPQparamValues[i])^));
                stSmall:    BindSignedOrdinal(i, BindList.SQLTypes[i], PG2SmallInt(FPQparamValues[i]));
                stInteger:  BindSignedOrdinal(i, BindList.SQLTypes[i], PG2Integer(FPQparamValues[i]));
                stLongWord: BindSignedOrdinal(i, BindList.SQLTypes[i], PG2Cardinal(FPQparamValues[i]));
                stLong:     BindSignedOrdinal(i, BindList.SQLTypes[i], PG2Int64(FPQparamValues[i]));
                stCurrency: if zOID  = CASHOID
                            then SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGCash2Currency(FPQparamValues[i]))
                            else SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGNumeric2Currency(FPQparamValues[i]));
                stFloat:    BindDouble(i, BindList.SQLTypes[i], PG2Single(FPQparamValues[i]));
                stDouble:   BindDouble(i, BindList.SQLTypes[i], PG2Double(FPQparamValues[i]));
                stBigDecimal:BindDouble(i, BindList.SQLTypes[i], PG2Double(FPQparamValues[i]));
                stTime:     if Finteger_datetimes
                            then BindDateTime(i, BindList.SQLTypes[i], PG2Time(PInt64(FPQparamValues[i])^))
                            else BindDateTime(i, BindList.SQLTypes[i], PG2Time(PDouble(FPQparamValues[i])^));
                stDate:     BindDateTime(i, BindList.SQLTypes[i], PG2Date(PInteger(FPQparamValues[i])^));
                stTimeStamp:if Finteger_datetimes
                            then BindDateTime(i, BindList.SQLTypes[i], PG2DateTime(PInt64(FPQparamValues[i])^))
                            else BindDateTime(i, BindList.SQLTypes[i], PG2DateTime(PDouble(FPQparamValues[i])^));
              end;
          end;
        end;
      finally
        FPlainDriver.PQclear(res);
      end;
    end else
      for i := 0 to BindList.Count-1 do
        SQLTypeToPostgreSQL(BindList.SQLTypes[i], fOIDAsBlob, FPQParamOIDs[i]);
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var I: Integer;
begin
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name, PrecisionOrSize, Scale);
  if ParamType in [pctOut, pctReturn] then begin
    FOutParamCount := 0;
    for i := 0 to BindList.Count -1 do
      Inc(FOutParamCount, Ord(BindList.ParamTypes[i] in [pctOut, pctReturn]));
  end else
    FHasInOutParams := FHasInOutParams or (ParamType = pctInOut);
  if (Name = '') then
    exit;
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  SetLength(FParamNames, BindList.Capacity);
  {$IFDEF UNICODE}
  FParamNames[ParameterIndex] := ZUnicodeToRaw(Name, ConSettings.ClientCodePage^.CP);
  {$ELSE}
  FParamNames[ParameterIndex] := ConSettings.ConvFuncs.ZStringToRaw(Name, ConSettings.CTRL_CP, ConSettings.ClientCodePage^.CP);
  {$ENDIF}
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
var ArrayDMLType: TArrayDMLType;
begin
  inherited ReleaseImmediat(Sender);
  Fres := nil;
  FRawPlanName := '';
  fPrepareCnt := 0;
  FExecCount := 0;
  SetParamCount(0);
  for ArrayDMLType := low(TArrayDMLType) to high(ArrayDMLType) do
    if Assigned(FPGArrayDMLStmts[ArrayDMLType].Intf) then
      (FPGArrayDMLStmts[ArrayDMLType].Intf as IImmediatelyReleasable).ReleaseImmediat(Sender);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.Unprepare;
var ArrayDMLType: TArrayDMLType;
begin
  if fAsyncQueries then
    FlushPendingResults;
  inherited Unprepare;
  FExecCount := 0;
  if Prepared and Assigned(FconnAddress^) and (FRawPlanName <> '') and not Findeterminate_datatype then
    ExecuteInternal('DEALLOCATE "'+FRawPlanName+'"', pqUnprepare);
  Findeterminate_datatype := False;
  FRawPlanName := '';
  FOutParamCount := 0;
  FHasInOutParams := False;
  for ArrayDMLType := low(TArrayDMLType) to high(TArrayDMLType) do begin
    FPGArrayDMLStmts[ArrayDMLType].Obj := nil;
    FPGArrayDMLStmts[ArrayDMLType].Intf := nil;
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPostgreSQLPreparedStatementV3.UnPrepareInParameters;
begin
  { release allocated memory }
  SetParamCount(0);
  Findeterminate_datatype := False;
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
    and not (Metadata.GetColumnType(ColumnIndex) in [stUnknown, stBinaryStream]);
end;

{ TZPostgreSQLStatement }

constructor TZPostgreSQLStatement.Create(
  const Connection: IZPostgreSQLConnection; Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FMinExecCount2Prepare := -1;
  fAsyncQueries := False;
  Findeterminate_datatype := True;
  FUseEmulatedStmtsOnly := True;
end;

{ TZPostgrePreparedStatementV2 }

constructor TZPostgrePreparedStatementV2.Create(
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FMinExecCount2Prepare := -1;
  fAsyncQueries := False;
  Findeterminate_datatype := True;
  FUseEmulatedStmtsOnly := True;
end;

procedure TZPostgrePreparedStatementV2.Unprepare;
begin
  inherited;
  Findeterminate_datatype := True;
end;

{ TZPostgreSQLCallableStatement }

function TZPostgreSQLCallableStatement.CreateExecutionStatement(
  Mode: TZCallExecKind;
  const StoredProcName: String): TZAbstractPreparedStatement2;
var
  P: PChar;
  I, J: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
begin
  SQL := '';
  ToBuff('SELECT * FROM ',SQL);
  ToBuff(StoredProcName, SQL);
  ToBuff('(', SQL);
  J := 1;
  for I := 0 to BindList.Capacity -1 do
    if Ord(BindList.ParamTypes[I]) < Ord(pctOut) then begin
      ToBuff('$', SQL);
      ToBuff(ZFastCode.IntToStr(J), SQL);
      ToBuff(',', SQL);
      Inc(J);
    end;
  FlushBuff(SQL);
  P := Pointer(SQL);
  if (BindList.Capacity > 0) and ((P+Length(SQL)-1)^ = ',')
  then (P+Length(SQL)-1)^ := ')' //cancel last comma
  else SQL := SQL + ')';
  Result := TZPostgreSQLPreparedStatementV3.Create(Connection as IZPostgreSQLConnection, SQL, Info);
  FExecStatements[TZCallExecKind(not Ord(Mode) and 1)] := Result;
  TZPostgreSQLPreparedStatementV3(Result)._AddRef;
end;

{ TZPostgreSQLPreparedStatementV3 }

procedure TZPostgreSQLPreparedStatementV3.SetCurrency(Index: Integer;
  const Value: Currency);
var X: Integer;
  SQLType: TZSQLType;
  P: Pointer;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  SQLType := OIDToSQLType(Index, stCurrency);
  if (FPQParamOIDs[index] <> CASHOID) and (SQLType in [stCurrency, stBigDecimal]) then begin
    P := BindList.AquireCustomValue(Index, SQLType, MaxCurr2NumSize);
    Currency2PGNumeric(Value, P, X);
    LinkParam2PG(Index, P, X, ParamFormatBin)
  end else begin
    BindList.Put(Index, stCurrency, P8Bytes(@Value));
    case SQLType of
      stBoolean:  begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Byte), ParamFormatBin);
                    PByte(FPQparamValues[Index])^ := Ord(Value <> 0);
                  end;
      stSmall:    begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(SmallInt), ParamFormatBin);
                    SmallInt2PG(SmallInt(PInt64(@Value)^ div 10000), FPQparamValues[Index]);
                  end;
      stInteger:  begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Integer), ParamFormatBin);
                    Integer2PG(Integer(PInt64(@Value)^ div 10000), FPQparamValues[Index]);
                  end;
      stLongWord: begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Cardinal), ParamFormatBin);
                    Cardinal2PG(Cardinal(PInt64(@Value)^ div 10000), FPQparamValues[Index]);
                  end;
      stLong:     begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Int64), ParamFormatBin);
                    Int642PG(PInt64(@Value)^ div 10000, FPQparamValues[Index]);
                  end;
      stFloat:    begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Single), ParamFormatBin);
                    Single2PG(Value, FPQparamValues[Index]);
                  end;
      stDouble:   begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Double), ParamFormatBin);
                    Double2PG(Value, FPQparamValues[Index]);
                  end;
      stCurrency: begin
                    LinkParam2PG(Index, BindList._8Bytes[Index], SizeOf(Currency), ParamFormatBin);
                    Currency2PGCash(Value, FPQparamValues[Index]);
                  end;
      else BindRawStr(Index, CurrToRaw(Value));
    end;
  end;
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

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.

