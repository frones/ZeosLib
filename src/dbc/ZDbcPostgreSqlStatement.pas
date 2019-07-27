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
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql, ZDbcUtils;

type
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
    FOutParamCount: Integer;
    FMinExecCount2Prepare: Integer; //how many executions must be done to fall into a real prepared mode?
    FExecCount: Integer; //How often did we execute the stmt until we reached MinExecCount2Prepare?
    FOrgSQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
    FParamNames: TRawByteStringDynArray;
    function CheckPrepareSwitchMode: Boolean;
    procedure InternalRealPrepare;
    function OIDToSQLType(var Index: Integer; SQLType: TZSQLType): TZSQLType;
    function ExecuteDMLBatchWithUnnestVarlenaArrays: TPGresult;
    procedure LinkBinParam2PG(Index: Integer; Buf: Pointer; Len: LengthInt);
    procedure LinkTxtParam2PG(Index: Integer; Buf: Pointer; Len: LengthInt);
    procedure BindArray(Index: Cardinal; Stmt: TZPostgreSQLPreparedStatementV3); reintroduce;
  protected
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(var Value: Integer); override;
    procedure ReleaseConnection; override;
  protected
    procedure FlushPendingResults;
    function CreateResultSet({%H-}ServerCursor: Boolean): IZResultSet;
    function PGExecute: TPGresult; virtual;
    procedure PGExecutePrepare;
    function PGExecutePrepared: TPGresult;
    procedure PGExecuteUnPrepare;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;
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

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  {** implements a prepared statement for PostgreSQL protocol V3+ }
  TZPostgreSQLPreparedStatementV3 = class(TZAbstractPostgreSQLPreparedStatementV3, IZPreparedStatement)
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;

    procedure InternalBindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double);
    procedure InternalBindInt(Index: Integer; SQLType: TZSQLType; Value: {$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF});
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    procedure ClearParameters; reintroduce;

    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; Value: Boolean);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt); reintroduce;
    procedure SetWord(Index: Integer; Value: Word); reintroduce;
    procedure SetSmall(Index: Integer; Value: SmallInt); reintroduce;
    procedure SetUInt(Index: Integer; Value: Cardinal); reintroduce;
    procedure SetInt(Index: Integer; Value: Integer); reintroduce;
    procedure SetULong(Index: Integer; const Value: UInt64); reintroduce;
    procedure SetLong(Index: Integer; const Value: Int64); reintroduce;
    procedure SetFloat(Index: Integer; Value: Single); reintroduce;
    procedure SetDouble(Index: Integer; const Value: Double); reintroduce;
    procedure SetCurrency(Index: Integer; const Value: Currency); reintroduce;
    procedure SetBigDecimal(Index: Integer; const Value: TBCD); reintroduce;

    procedure SetDate(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTime(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTimestamp(Index: Integer; const Value: TDateTime); reintroduce;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = '';
      {%H-}PrecisionOrSize: LengthInt = 0; {%H-}Scale: LengthInt = 0); override;
    (*procedure SetNullArray(Index: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
    procedure SetDataArray(Index: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);*)
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
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement2; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

const ZSQLType2PGBindTypes: array[stUnknown..stGUID] of TZBindType = (zbtNull,
    zbt4Byte{stBoolean},
    zbt4Byte{stByte}, zbt4Byte{stShort}, zbt4Byte{stWord},
    zbt4Byte{stSmall}, zbt4Byte{stLongWord}, zbt4Byte{stInteger}, zbt8Byte{stULong}, zbt8Byte{stLong},  //ordinals
    zbt4Byte{stFloat}, zbt8Byte{stDouble}, zbtCustom{stCurrency}, zbtCustom{stBigDecimal}, //floats
    zbt4Byte{stDate}, zbt8Byte{stTime}, zbt8Byte{stTimestamp},
    zbtGUID{stGUID});
{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils,
  ZEncoding, ZDbcProperties, ZTokenizer, Types, ZDbcResultSet, ZClasses
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};


var
  PGPreparableTokens: TPreparablePrefixTokens;

{ TZAbstractPostgreSQLPreparedStatementV3 }

procedure TZAbstractPostgreSQLPreparedStatementV3.BindArray(Index: Cardinal;
  Stmt: TZPostgreSQLPreparedStatementV3);
var
  SQLType: TZSQLType;

  procedure AllocArray(Index: Cardinal; TotalSize: Integer;
    out A: PArrayType; out Buf: PAnsiChar);
  var
    aOID: OID;
    BuffSize: Integer;
  begin
    BuffSize := ARR_OVERHEAD_NONULLS(1)+TotalSize;
    //alloc mem for the varlena array struct ->
    A := Stmt.BindList.AquireMinCustomValue(Index, SQLType, BuffSize);
    SQLTypeToPostgreSQL(SQLType, FOidAsBlob, aOID);
    //write dimension(s)
    Integer2PG(1, ARR_NDIM(A));
    //indicate nullable items
    Integer2PG(0, @A.flags);
    //Write the OID
    Cardinal2PG(aOID, ARR_ELEMTYPE(A));
    //write item count
    Integer2PG(BatchDMLArrayCount, ARR_DIMS(A));
    //write lower bounds
    Integer2PG(1, ARR_LBOUND(A));
    Buf := ARR_DATA_PTR(A);
    Stmt.FPQparamValues[Index] := a;
    Stmt.FPQparamFormats[Index] := ParamFormatBin;
    Stmt.FPQparamLengths[Index] := BuffSize;
  end;
var
  OffSet, J: Cardinal;
  D: Pointer;
  P: PAnsiChar;
  A: PArrayType;
  CP: word;
  X, N, DynArrayLen: Integer;
  Arr: PZArray;
  Native: Boolean;

  procedure BindLobs;
  var J: Cardinal;
    N: Integer;
    TempBlob: IZBlob;
    WriteTempBlob: IZPostgreSQLOidBlob;
  begin
    CP := ConSettings^.ClientCodePage.CP;
    N := 0;
    for J := 0 to DynArrayLen -1 do
      if (TInterfaceDynArray(D)[j] <> nil) and Supports(TInterfaceDynArray(D)[j], IZBlob, TempBlob) and not TempBlob.IsEmpty then
        if BindList.SQLTypes[Index] in [stUnicodeStream, stAsciiStream] then begin
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
    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
    if (BindList.SQLtypes[Index] = stBinaryStream) and FOidAsBlob then begin
      for j := 0 to DynArrayLen -1 do
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
      AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
      for J := 0 to DynArrayLen -1 do
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
  procedure BindRawStrings;
  var J: Cardinal;
    N: Integer;
  begin
    N := 0;
    for j := 0 to DynArrayLen -1 do
      if not IsNullFromArray(Arr, j) then
        Inc(N, Length(TRawByteStringDynArray(D)[j]));
    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
    for j := 0 to DynArrayLen -1 do
      if IsNullFromArray(Arr, j) then begin
        Integer2PG(-1, P);
        Inc(P,SizeOf(int32));
      end else begin
        N := Length(TRawByteStringDynArray(D)[j]);
        Integer2PG(N, P);
        Move(Pointer(TRawByteStringDynArray(D)[j])^, (P+SizeOf(int32))^, N);
        Inc(P,SizeOf(int32)+N);
      end;
  end;
  procedure BindUniStrings;
  var J: Cardinal;
    N, charWidth, RawLen, MaxBytes: Integer;
  begin
    N := 0;
    charWidth := ConSettings^.ClientCodePage.CharWidth;
    for j := 0 to DynArrayLen -1 do
      if not IsNullFromArray(Arr, j) then
        Inc(N, Length(TUnicodeStringDynArray(D)[j]));
    AllocArray(Index, (N*CharWidth)+(DynArrayLen*SizeOf(int32)), A, P);
    for j := 0 to DynArrayLen -1 do
      if IsNullFromArray(Arr, j) then begin
        Integer2PG(-1, P);
        Inc(P,SizeOf(int32));
      end else begin
        N := Length(TUnicodeStringDynArray(D)[j]);
        MaxBytes := N*CharWidth;
        RawLen := ZEncoding.PUnicode2PRawBuf(Pointer(TUnicodeStringDynArray(D)[j]), (P+SizeOf(int32)), N, MaxBytes, CP);
        Integer2PG(RawLen, P);
        Inc(P,SizeOf(int32)+RawLen);
        Dec(Stmt.FPQparamLengths[Index], (MaxBytes-RawLen));
      end;
  end;
  procedure BindConvertedStrings;
  var FTempRaws: TRawByteStringDynArray;
    J: Cardinal;
  begin
    SetLength(FTempRaws, DynArrayLen);
    case Arr.VArrayVariantType of
      {$IFNDEF UNICODE}
      vtString:   for J := 0 to DynArrayLen -1 do
                    FTempRaws[j] := ConSettings.ConvFuncs.ZStringToRaw(TStringDynArray(D)[j], ConSettings.CTRL_CP, CP);
      {$ENDIF}
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString: for J := 0 to DynArrayLen -1 do
                      FTempRaws[j] := Consettings^.ConvFuncs.ZAnsiToRaw(TAnsiStringDynArray(D)[j], CP);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String: for J := 0 to DynArrayLen -1 do
                      FTempRaws[j] := Consettings^.ConvFuncs.ZUTF8ToRaw(TUTF8StringDynArray(D)[j], CP);
      {$ENDIF}
      {$IFDEF UNICODE}
      vtString,
      {$ENDIF}
      vtUnicodeString: for J := 0 to DynArrayLen -1 do
                          FTempRaws[j] := ZUnicodeToRaw(TUnicodeStringDynArray(D)[j], CP);
      vtCharRec:  for J := 0 to DynArrayLen -1 do
                    if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) or (TZCharRecDynArray(D)[j].Len = 0) then
                      ZSetString(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, FTempRaws[j])
                    else if ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, zCP_UTF16) then
                      FTempRaws[j] := PUnicodeToRaw(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, CP)
                    else begin
                      fUniTemp := PRawToUnicode(TZCharRecDynArray(D)[j].P, TZCharRecDynArray(D)[j].Len, TZCharRecDynArray(D)[j].CP);
                      FTempRaws[j] := ZUnicodeToRaw(fUniTemp, CP)
                    end;
    end;
    D := FTempRaws;
    BindRawStrings;
  end;
begin
  Arr := BindList[Index].Value; //localize -> next steps will free the memory
  SQLType := TZSQLType(Arr.VArrayType);
  D := Arr.VArray;
  P := nil;
  Native := Arr.VArrayVariantType in NativeArrayValueTypes[SQLType];
  DynArrayLen := Length(TByteDynArray(D));
  case SQLType of
    stBoolean:    begin
                    AllocArray(Index, SizeOf(Byte)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index]);
                      end else begin
                        Integer2PG(SizeOf(Byte), P);
                        if Native
                        then PByte(P+SizeOf(int32))^ := Ord(TBooleanDynArray(D)[j])
                        else PByte(P+SizeOf(int32))^ := Ord(ArrayValueToBoolean(Arr, J));
                        Inc(P,SizeOf(int32)+SizeOf(Byte));
                      end;
                  end;
    stByte:       begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TByteDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stShort:      begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TShortIntDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stWord:       begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        Integer2PG(SizeOf(Integer), P);
                        SmallInt2PG(TWordDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stSmall:      begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TSmallIntDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stInteger:    begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        Integer2PG(SizeOf(Integer), P);
                        Integer2PG(TIntegerDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stLongWord:   if (stmt.FPQParamOIDs[Index] = OIDOID) then begin
                    AllocArray(Index, SizeOf(OID)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(OID));
                      end else begin
                        Integer2PG(SizeOf(OID), P);
                        Cardinal2PG(TLongWordDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(OID));
                      end;
                  end else begin
                    AllocArray(Index, SizeOf(Int64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(LongWord), P);
                        Int642PG(TLongWordDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Int64));
                      end;
                  end;
    stLong:       begin
                    AllocArray(Index, SizeOf(Int64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Int64), P);
                        Int642PG(TInt64DynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Int64));
                      end;
                  end;
    stULong:      begin
                    AllocArray(Index, SizeOf(UInt64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Int64), P);
                        Int642PG(Int64(TUInt64DynArray(D)[j]),P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(UInt64));
                      end;
                  end;
    stFloat:      begin
                    AllocArray(Index, SizeOf(Single)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Single));
                      end else begin
                        Integer2PG(SizeOf(Single), P);
                        Single2PG(TSingleDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Single));
                      end;
                  end;
    stDouble:     begin
                    AllocArray(Index, SizeOf(Double)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Double), P);
                        Double2PG(TDoubleDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Double));
                      end;
                  end;
    stCurrency:   if (stmt.FPQParamOIDs[Index] = CASHOID) then begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        Integer2PG(8, P);
                        Currency2PGCash(TCurrencyDynArray(D)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+8);
                      end;
                  end else begin
                    AllocArray(Index, MaxCurr2NumSize*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        X := SizeOf(int32);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], MaxCurr2NumSize);
                      end else begin
                        Currency2PGNumeric(TCurrencyDynArray(D)[j], P+SizeOf(int32), x);
                        Integer2PG(X, P);
                        Inc(P,SizeOf(int32)+X);
                        Dec(Stmt.FPQparamLengths[Index], MaxCurr2NumSize-X);
                      end
                  end;
    stBigDecimal: begin
                    AllocArray(Index, MaxBCD2NumSize*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], MaxBCD2NumSize);
                      end else begin
                        BCD2PGNumeric(TBCDDynArray(D)[j], P+SizeOf(int32), x);
                        Integer2PG(X, P);
                        Inc(P,SizeOf(int32)+X);
                        Dec(Stmt.FPQparamLengths[Index], MaxBCD2NumSize-X);
                      end;
                  end;
    stDate:       begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        Integer2PG(SizeOf(Integer), P);
                        Date2PG(TDateTimeDynArray(D)[j], PInteger(NativeUInt(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stTime:       begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        Integer2PG(8, P);
                        if Finteger_datetimes
                        then Time2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                        else Time2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+8);
                      end
                  end;
    stTimeStamp:  begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        Integer2PG(8, P);
                        if Finteger_datetimes
                        then DateTime2PG(TDateTimeDynArray(D)[j], PInt64(NativeUInt(P)+SizeOf(int32))^)
                        else DateTime2PG(TDateTimeDynArray(D)[j], PDouble(NativeUInt(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+8);
                      end
                  end;
    stGUID:       begin
                    AllocArray(Index, SizeOf(TGUID)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(TGUID));
                      end else begin
                        Integer2PG(SizeOf(TGUID), P);
                        //eh: Network byteOrder?
                        PGUID(NativeUInt(P)+SizeOf(int32))^ := TGUIDDynArray(D)[j];
                        Inc(P,SizeOf(int32)+SizeOf(TGUID));
                      end
                  end;
    stBytes:      begin
                    N := 0;
                    for J := 0 to DynArrayLen -1 do
                      if not (IsNullFromArray(Arr, j) or (Pointer(TBytesDynArray(D)[j]) = nil)) then
                        Inc(N, Length(TBytesDynArray(D)[j]));
                    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
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
        CP := ConSettings^.ClientCodePage.CP;
        case Arr.VArrayVariantType of
          {$IFNDEF UNICODE}
          vtString:       if (not ConSettings^.AutoEncode and ZCompatibleCodePages(CP, ConSettings^.CTRL_CP))
                          then BindRawStrings
                          else BindConvertedStrings;
          {$ENDIF}
          {$IFNDEF NO_ANSISTRING}
          vtAnsiString:   if ZCompatibleCodePages(CP, ZOSCodePage)
                          then BindRawStrings
                          else BindConvertedStrings;
          {$ENDIF}
          {$IFNDEF NO_UTF8STRING}
          vtUTF8String:   if ZCompatibleCodePages(CP, zCP_UTF8)
                          then BindRawStrings
                          else BindConvertedStrings;
          {$ENDIF}
          vtRawByteString:BindRawStrings;
          {$IFDEF UNICODE}
          vtString,
          {$ENDIF}
          vtUnicodeString: BindUniStrings;
          vtCharRec:  begin
                        N := 0;
                        OffSet := DynArrayLen;
                        for J := 0 to DynArrayLen -1 do begin
                          Dec(OffSet, Ord(not ((ZCompatibleCodePages(TZCharRecDynArray(D)[j].CP, cp) and (TZCharRecDynArray(D)[j].Len > 0) and not IsNullFromArray(Arr, j)))));
                          Inc(N, TZCharRecDynArray(D)[j].Len*Byte(Ord(not IsNullFromArray(Arr, j))));
                        end;
                        if OffSet <> Cardinal(DynArrayLen) then
                          BindConvertedStrings
                        else begin
                          AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
                          for J := 0 to DynArrayLen -1 do
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
    stAsciiStream, stUnicodeStream, stBinaryStream: BindLobs;
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.CheckParameterIndex(var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (BindList.Capacity < Value+1) then
    if fRawPlanname <> ''
    then raise EZSQLException.Create(SInvalidInputParameterCount)
    else inherited CheckParameterIndex(Value);
  { now change the index to the !in!-param ordinal index }
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

function TZAbstractPostgreSQLPreparedStatementV3.CheckPrepareSwitchMode: Boolean;
begin
  Result := (FRawPlanName = '') and not Findeterminate_datatype and ((not FUseEmulatedStmtsOnly) or (BatchDMLArrayCount > 0 )) and
    (TokenMatchIndex <> -1) and ((BatchDMLArrayCount > 0 ) or (FExecCount = FMinExecCount2Prepare));
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
  if Assigned(FPlainDriver.PQexecParams) and StrToBoolEx(DefineStatementParameter(Self, DSProps_BinaryWireResultMode, 'TRUE'))
  then FPQResultFormat := ParamFormatBin
  else FPQResultFormat := ParamFormatStr;
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
      @Fres, @FPQResultFormat, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength)
  else NativeResultSet := TZClientCursorPostgreSQLStringResultSet.Create(Self, Self.SQL, FconnAddress,
      @Fres, @FPQResultFormat, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);

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
  I: Cardinal;
  function CreateBatchDMLStmt: TZPostgreSQLPreparedStatementV3;
  var I, OffSet: Cardinal;
    aOID: OID;
    N: Integer; //the ParameterIndex
    SQL: RawByteString;
    Buf: TRawBuff;
  begin
//EgonHugeist: introduction
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
    if J <> BatchDMLArrayCount+1 then
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
    SQL := '';
    N := 1;
    Buf.Pos := 0;
    OffSet := 0;
    //first build up a new string with unnest($n)::xyz[] surrounded params
    for I := 0 to high(FCachedQueryRaw) do
      if IsParamIndex[i] then begin
        if BindList[OffSet].BindType in [zbtArray, zbtRefArray] then begin
          ZDbcUtils.ToBuff('unnest($', buf, SQL);
          fRawTemp := IntToRaw(N);
          ZDbcUtils.ToBuff(fRawTemp, buf, SQL);
          ZDbcUtils.ToBuff('::', buf, SQL);
          SQLTypeToPostgreSQL(TZSQLType(BindList.Arrays[Offset].VArrayType), FOidAsBlob, aOID);
          ZDbcUtils.ToBuff({$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID)), buf, SQL);
          ZDbcUtils.ToBuff('[])', buf, SQL);
          Inc(OffSet);
        end else begin
          ZDbcUtils.ToBuff('$', buf, SQL);
          fRawTemp := IntToRaw(N);
          ZDbcUtils.ToBuff(fRawTemp, buf, SQL);
          ZDbcUtils.ToBuff(',', buf, SQL);
        end;
        Inc(N);
      end else
        ZDbcUtils.ToBuff(FCachedQueryRaw[i], buf, SQL);
    ZDbcUtils.FlushBuff(buf, SQL);
  (* gives such a string:
  insert into public.SampleRecord (ID,FirstName,LastName,Amount,BirthDate,LastChange,CreatedAt) values (
  unnest($1::int8[])
  ,unnest($2::text[])
  ,unnest($3::text[])
  ,unnest($4::float8[])
  ,unnest($5::timestamp[])
  ,unnest($6::int8[])
  ,unnest($7::int8[]))

  This approach took me ages. No description somewhere, just two (not working) example on
  Stack overflow... The Array.h ported Macros -> Trash !
  Postgres makes differences between external and internal format.
  The external format passes all items as verlena struct and uses Len=-1 as NullIndicator
  If null we've to send the length indicator only else each value is passed by length+bytes of val

  However all findings happen on debugging the postgres server -> thanks to Jan for helping me.

  *)
    Result := TZPostgreSQLPreparedStatementV3.Create(FPostgreSQLConnection, '', Info);
    Result.FaSQL := SQL;
    Result.SetParamCount(BindList.Count);
    Result.InternalRealPrepare; //force describe the params to get the array oids
  end;
begin
  if FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf = nil then begin
    Stmt := CreateBatchDMLStmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj := Stmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf := Stmt;
  end else
    Stmt := FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj;
  for i := 0 to bindList.Count -1 do
    if BindList[I].BindType in [zbtArray, zbtRefArray] then
      BindArray(i, stmt);
  Result := Stmt.PGExecutePrepared;
  Stmt.BindList.ClearValues; //free allocated mem
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
    HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
      lcExecute, ASQL, Result);
end;

function TZAbstractPostgreSQLPreparedStatementV3.PGExecute: TPGresult;
var
  PError: PAnsiChar;
  label ExecWithParams;
  function ExecEmulated: TPGresult;
  var TmpSQL: RawByteString;
    I, N: Integer;
  begin
    TmpSQL := '';
    N := 0;
    for I := 0 to High(FCachedQueryRaw) do
      if FIsParamIndex[i] then begin
        ToBuff(GetInParamLogValue(n), TmpSQL);
        Inc(N);
      end else
        ToBuff(FCachedQueryRaw[i], TmpSQL);
      FlushBuff(TmpSQL);
    if (FPQResultFormat = ParamFormatBin)
    then Result := FPlainDriver.PQexecParams(FconnAddress^, Pointer(TmpSQL),
        0, nil, nil, nil, nil, ParamFormatBin)
    else Result := FPlainDriver.PQExec(FconnAddress^, Pointer(TmpSQL));
  end;
begin
  Result := nil;
  if not Assigned(FconnAddress^) then
    Exit;
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
ExecWithParams:
        Result := FPlainDriver.PQexecParams(FconnAddress^, Pointer(FASQL),
          BindList.Count-FOutParamCount, Pointer(FPQParamOIDs), Pointer(FPQparamValues),
          Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
        if Assigned(FPlainDriver.PQresultErrorField)
        then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
        else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
        if (PError <> nil) and (PError^ <> #0) then begin
          { check for indermine datatype error}
          if Assigned(FPlainDriver.PQresultErrorField) and CompareMem(PError, indeterminate_datatype, 5) then begin
            FPlainDriver.PQclear(Result);
            Findeterminate_datatype := True;
            Result := ExecEmulated;
          end else if Assigned(FPlainDriver.PQresultErrorField) and (FPQResultFormat = ParamFormatBin) and
            CompareMem(PError, no_binary_output_function_available_for_type_void, 5) then begin
            FPlainDriver.PQclear(Result);
            FPQResultFormat := ParamFormatStr; //fall back to string format
            goto ExecWithParams;
          end;
        end else begin
          Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
          CheckPrepareSwitchMode;
        end;
      end else
        Result := ExecEmulated;
    end else if FPQResultFormat = ParamFormatStr
      then Result := FPlainDriver.PQExec(FconnAddress^, Pointer(FASQL))
      else goto ExecWithParams;
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
        lcExecute, ASQL, Result);
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
  then Fres := PGExecute
  else Fres := PGExecutePrepared;

  { Process queries with result sets }
  if FPlainDriver.PQresultStatus(Fres) = PGRES_TUPLES_OK then begin
    Result := True;
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet(fServerCursor);
    if BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
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
  then Fres := PGExecute
  else Fres := PGExecutePrepared;
  Status := FPlainDriver.PQresultStatus(Fres);
  if (Fres <> nil) and (Status = PGRES_TUPLES_OK) then begin
    if Assigned(FOpenResultSet)
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet(fServerCursor);
    if BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := Result;
  end else
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
  Prepare;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  if BatchDMLArrayCount > 0
  then FRes := ExecuteDMLBatchWithUnnestVarlenaArrays
  else if (FRawPlanName = '') or Findeterminate_datatype
    then Fres := PGExecute
    else Fres := PGExecutePrepared;
  if Fres <> nil then
    if (FPlainDriver.PQresultStatus(Fres) = PGRES_TUPLES_OK) and (BindList.HasOutOrInOutOrResultParam) then begin
      if not Assigned(LastResultSet) then
        FOutParamResultSet := CreateResultSet(fServerCursor);
      LastUpdateCount := RawToIntDef(FPlainDriver.PQcmdTuples(Fres), 0);
    end else begin
      LastUpdateCount := RawToIntDef(FPlainDriver.PQcmdTuples(Fres), 0);
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
var P: Pointer;
  BindValue: PZBindValue;
begin
  BindValue := BindList[ParamIndex];
  case BindValue.BindType of
    zbtNull: Result := 'null';
    zbt4Byte: begin
        P := BindList._4Bytes[ParamIndex];
        case FPQParamOIDs[ParamIndex] of
          BOOLOID:  Result := BoolStrIntsRaw[BindValue.Value <> nil];
          INT2OID:  Result := IntToRaw(PG2SmallInt(P));
          INT4OID:  Result := IntToRaw(PG2Integer(P));
          OIDOID:   Result := IntToRaw(PG2Cardinal(P));
          FLOAT4OID:Result := FloatToRaw(PG2Single(P));
          DATEOID:  Result := DateTimeToRawSQLDate(PG2Date(PInteger(P)^), ConSettings^.WriteFormatSettings, True, '::date');
        end;
      end;
    zbt8Byte: begin
        P := BindList._8Bytes[ParamIndex];
        case FPQParamOIDs[ParamIndex] of
          INT8OID:  Result := IntToRaw(PG2Int64(P));
          FLOAT8OID:Result := FloatToRaw(PG2Double(P));
          CASHOID:  Result := FloatToRaw(PG2Int64(P)/100);
          TIMEOID:  if Finteger_datetimes
                    then Result := DateTimeToRawSQLTime(PG2Time(PInt64(P)^), ConSettings^.WriteFormatSettings, True, '::time')
                    else Result := DateTimeToRawSQLTime(PG2Time(PDouble(P)^), ConSettings^.WriteFormatSettings, True, '::time');
          TIMESTAMPOID: if Finteger_datetimes
                    then Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PInt64(P)^), ConSettings^.WriteFormatSettings, True, '::timestamp')
                    else Result := DateTimeToRawSQLTimeStamp(PG2DateTime(PDouble(P)^), ConSettings^.WriteFormatSettings, True, '::timestamp');
        end;
      end;
    zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}: Connection.GetEscapeString(PAnsiChar(BindValue.Value), Length(RawByteString(BindValue.Value)), Result);
    zbtCharByRef: Connection.GetEscapeString(PAnsiChar(PZCharRec(BindValue.Value)^.P), PZCharRec(BindValue.Value)^.Len, Result);
    zbtBinByRef: Connection.GetBinaryEscapeString(PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len, Result);
    zbtGUID:     GUIDToRaw(BindValue.Value, [guidWithBrackets, guidQuoted], Result);
    zbtBytes: Connection.GetBinaryEscapeString(BindValue.Value, Length(TBytes(BindValue.Value)), Result);
    zbtLob: if BindValue.SQLType = stBinaryStream
            then Connection.GetBinaryEscapeString(IZBlob(BindValue.Value).GetBuffer, IZBlob(BindValue.Value).Length, Result)
            else Connection.GetEscapeString(IZBlob(BindValue.Value).GetBuffer, IZBlob(BindValue.Value).Length, Result);
    zbtPointer: Result := BoolStrIntsRaw[BindValue.Value <> nil];
    zbtCustom: if BindValue.SQLType = stArray
                then Result := '(Array)'
                else Result := CurrToRaw(PGNumeric2Currency(PAnsiChar(BindValue.Value)+SizeOf(LengthInt)));
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
           ((Token.TokenType = ttWord) and (Token.P^ = '$')) then begin
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
  FRawPlanName := IntToRaw(FStatementId)+'_'+IntToRaw({%H-}NativeUInt(FconnAddress^))+'_'+IntToRaw(fPrepareCnt);
  PGExecutePrepare;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.SetBindCapacity(Capacity: Integer);
var i: Integer;
begin
  inherited SetBindCapacity(Capacity);
  if (Length(FPQparamValues) <> BindList.Capacity) then begin
    SetLength(FPQparamValues, BindList.Capacity);
    SetLength(FPQparamLengths, BindList.Capacity);
    SetLength(FPQparamFormats, BindList.Capacity);
    SetLength(FPQParamOIDs, BindList.Capacity);
    SetLength(FInParamDefaultValues, BindList.Capacity);
    SetLength(FParamNames, BindList.Capacity);
    { rebind since the realloc did change the addresses of the hooked values}
    for i := 0 to Capacity -1 do
      if BindList.BindTypes[i] {$IFDEF CPU64}in [zbt4Byte, zbt8Byte]{$ELSE}= zbt4Byte{$ENDIF} then
        FPQparamValues[I] := @BindList[I].Value;
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.LinkBinParam2PG(
  Index: Integer; Buf: Pointer; Len: LengthInt);
begin
  FPQparamLengths[Index] := Len;
  FPQparamValues[Index] := Buf;
  FPQparamFormats[Index] := ParamFormatBin;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.LinkTxtParam2PG(
  Index: Integer; Buf: Pointer; Len: LengthInt);
begin
  FPQparamLengths[Index] := Len;
  FPQparamValues[Index] := Buf;
  FPQparamFormats[Index] := ParamFormatStr;
end;

function TZAbstractPostgreSQLPreparedStatementV3.OIDToSQLType(var Index: Integer;
  SQLType: TZSQLType): TZSQLType;
begin
  CheckParameterIndex(Index);
  if (FPQParamOIDs[Index] = INVALIDOID) or (FRawPlanname = '') or Findeterminate_datatype then begin
    FPQParamOIDs[Index] := ZSQLType2OID[FOidAsBlob][SQLType];
    if (Ord(SQLType) > Ord(stBoolean)) and (Ord(SQLType) < Ord(stLongWord)) and not Odd(Ord(SQLType))
    then Result := TZSQLType(Ord(SQLType)+3)
    else Result := SQLType
  end else case FPQParamOIDs[Index] of
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
    else if SQLType in [stBytes, stString, stUnicodeString, stAsciistream, stUnicodeStream, stBinaryStream] then
      Result := SQLType
    else
      Result := stUnknown; //indicate unsupport the types as Fallback to String format
  end;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPostgreSQLPreparedStatementV3.PGExecutePrepare;
var PError: PAnsiChar;
  Res: TPGresult;
begin
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcPrepStmt,Self);
  if fAsyncQueries then begin
    if not FPlainDriver.PQsendPrepare(FconnAddress^, Pointer(FRawPlanName),
       Pointer(FASQL), BindList.Count-FOutParamCount, Pointer(FPQParamOIDs)) = Ord(PGRES_COMMAND_OK) then begin
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^, lcExecute, ASQL, nil);
    end;
  end else begin
    Res := FPlainDriver.PQprepare(FconnAddress^, Pointer(FRawPlanName),
      Pointer(ASQL), BindList.Count-FOutParamCount, nil{Pointer(fParamOIDs)});
    if Assigned(FPlainDriver.PQresultErrorField)
    then PError := FPlainDriver.PQresultErrorField(Res,Ord(PG_DIAG_SQLSTATE))
    else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
    if (PError <> nil) and (PError^ <> #0) then
      { check for indermine datatype error}
      if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
        HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
          lcPrepStmt, ASQL, Res)
      else begin
        FPlainDriver.PQclear(Res);
        Findeterminate_datatype := True
      end
    else begin
      FPlainDriver.PQclear(Res);
      PrepareInParameters;
    end;
  end;
end;

function TZAbstractPostgreSQLPreparedStatementV3.PGExecutePrepared: TPGresult;
var PError: PAnsiChar;
label ReExecuteStr;
begin
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
  if fAsyncQueries then begin
    if FPlainDriver.PQsendQueryPrepared(FconnAddress^,
       Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
       Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then begin
      Result := nil; //satisfy compiler
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
        lcExecPrepStmt, ASQL, Result);
    end else if FServerCursor then
      FPlainDriver.PQsetSingleRowMode(FconnAddress^);
    Result := FPlainDriver.PQgetResult(FconnAddress^); //obtain the first result
  end else begin
ReExecuteStr:
    Result := FPlainDriver.PQexecPrepared(FconnAddress^,
      Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
      Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
    if Assigned(FPlainDriver.PQresultErrorField)
    then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
    else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
    if (PError <> nil) and (PError^ <> #0) and Assigned(FPlainDriver.PQresultErrorField) and
       (FPQResultFormat = ParamFormatBin) and CompareMem(PError, no_binary_output_function_available_for_type_void, 5) then begin
      FPlainDriver.PQclear(Result);
      FPQResultFormat := ParamFormatStr; //fall back to string format
      goto ReExecuteStr;
    end;
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^,
        lcExecPrepStmt, ASQL, Result);
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.PGExecuteUnPrepare;
var
  PError: PAnsiChar;
  Res: TPGresult;
  procedure DoOnFail;
  begin
    FPostgreSQLConnection.RegisterTrashPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName))
  end;
begin
  fRawTemp := 'DEALLOCATE "'+FRawPlanName+'"';
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcUnprepStmt,Self);
  Res := FPlainDriver.PQExec(FconnAddress^, Pointer(fRawTemp));
  if Assigned(FPlainDriver.PQresultErrorField)
  then PError := FPlainDriver.PQresultErrorField(Res,Ord(PG_DIAG_SQLSTATE))
  else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
  if (PError <> nil) and (PError^ <> #0) then
    { check for current transaction is aborted error}
    if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, current_transaction_is_aborted, 5) <> 0) then
      HandlePostgreSQLError(Self, FPlainDriver, FconnAddress^, lcUnprepStmt, ASQL, Res)
    else
      DoOnFail
  else if Res <> nil then
    FPlainDriver.PQclear(Res);
end;

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

procedure TZAbstractPostgreSQLPreparedStatementV3.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FPostgreSQLConnection := nil;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var ArrayDMLType: TArrayDMLType;
begin
  inherited ReleaseImmediat(Sender, AError);
  Fres := nil;
  FRawPlanName := '';
  fPrepareCnt := 0;
  FExecCount := 0;
  SetParamCount(0);
  for ArrayDMLType := low(TArrayDMLType) to high(ArrayDMLType) do
    if Assigned(FPGArrayDMLStmts[ArrayDMLType].Intf) then
      (FPGArrayDMLStmts[ArrayDMLType].Intf as IImmediatelyReleasable).ReleaseImmediat(Sender, AError);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.Unprepare;
var ArrayDMLType: TArrayDMLType;
begin
  if fAsyncQueries then
    FlushPendingResults;
  inherited Unprepare;
  FExecCount := 0;
  if Prepared and Assigned(FconnAddress^) and (FRawPlanName <> '') and not Findeterminate_datatype then
    PGExecuteUnPrepare;
  Findeterminate_datatype := False;
  FRawPlanName := '';
  FOutParamCount := 0;
  for ArrayDMLType := low(TArrayDMLType) to high(TArrayDMLType) do begin
    FPGArrayDMLStmts[ArrayDMLType].Obj := nil;
    FPGArrayDMLStmts[ArrayDMLType].Intf := nil;
  end;
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
  const StoredProcName: String): TZAbstractPreparedStatement2;
var
  P: PChar;
  I, J: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
begin
  SQL := '';
  ToBuff('SELECT * FROM ',SQL);
  ToBuff(StoredProcName, SQL);
  ToBuff(Char('('), SQL);
  J := 1;
  for I := 0 to BindList.Capacity -1 do
    if Ord(BindList.ParamTypes[I]) < Ord(pctOut) then begin
      ToBuff(Char('$'), SQL);
      ToBuff(ZFastCode.IntToStr(J), SQL);
      ToBuff(Char(','), SQL);
      Inc(J);
    end;
  FlushBuff(SQL);
  P := Pointer(SQL);
  if J > 1
  then (P+Length(SQL)-1)^ := ')' //cancel last comma
  else SQL := SQL + ')';
  Result := TZPostgreSQLPreparedStatementV3.Create(Connection as IZPostgreSQLConnection, SQL, Info);
end;

{ TZPostgreSQLPreparedStatementV3 }

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var InParamIdx: Integer;
begin
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  if SQLType = stBytes
  then BindList.Put(Index, SQLType, TBytes(Buf))
  else BindList.Put(Index, SQLtype, Buf, Len);
  LinkBinParam2PG(InParamIdx, Buf, Len);
end;

procedure TZPostgreSQLPreparedStatementV3.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var WriteTempBlob: IZPostgreSQLOidBlob;
  Lob_OID: OID;
  InParamIdx: Integer;
  RefCntLob: IZBlob;
begin
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  RefCntLob := Value; //inc RefCount
  if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then begin
    if Value.IsClob then
      Value.GetPAnsiChar(FClientCP)
    else begin
      FRawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings);
      RefCntLob := TZAbstractCLob.CreateWithData(Pointer(FRawTemp),
        Length(FRawTemp), ConSettings^.ClientCodePage.CP, ConSettings);
    end;
    SQLType := stAsciiStream;
  end;
  BindList.Put(Index, SQLType, RefCntLob);
  if (RefCntLob <> nil) and not RefCntLob.IsEmpty then
    if ((SQLType = stBinaryStream) and FOidAsBlob) then begin
      WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, 0, ChunkSize);
      WriteTempBlob.WriteBuffer(RefCntLob.GetBuffer, RefCntLob.Length);
      Lob_OID := WriteTempBlob.GetBlobOid;
      FPQParamOIDs[InParamIdx] := OIDOID;
      BindList.Put(Index, stBinaryStream, P4Bytes(@Lob_OID));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], SizeOf(OID));
      {$IFNDEF ENDIAN_BIG}Reverse4Bytes(FPQparamValues[Index]);{$ENDIF}
      WriteTempBlob := nil;
    end else begin
      FPQparamValues[InParamIdx] := RefCntLob.GetBuffer;
      FPQparamLengths[InParamIdx] := RefCntLob.Length;
      if SQLType = stBinaryStream
      then FPQparamFormats[InParamIdx] := ParamFormatBin
      else FPQparamFormats[InParamIdx] := ParamFormatStr;
    end;
end;

procedure TZPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var InParamIDX: Integer;
begin
  InParamIDX := Index;
  CheckParameterIndex(InParamIDX);
  if Buf = nil then begin
    Buf := PEmptyAnsiString;
    Len := 0;
  end;
  BindList.Put(Index, stString, Buf, Len, FClientCP);
  LinkTxtParam2PG(InParamIDX, Buf, Len);
end;

procedure TZPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  const Value: RawByteString);
var InParamIDX: Integer;
begin
  InParamIDX := Index;
  CheckParameterIndex(InParamIDX);
  BindList.Put(Index, stString, Value, FClientCP);
  LinkTxtParam2PG(InParamIDX, Pointer(Value), Length(Value));
  if Pointer(Value) = nil then
    FPQparamValues[InParamIDX] := PEmptyAnsiString;
end;

procedure TZPostgreSQLPreparedStatementV3.ClearParameters;
begin
  //inherited ClearParameters
  BatchDMLArrayCount := 0;
end;

{**
  Binds a double value
}
procedure TZPostgreSQLPreparedStatementV3.InternalBindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var PGSQLType: TZSQLType;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure SetAsRaw;
  begin
    case SQLType of
      stBoolean:    fRawTemp := BoolStrIntsRaw[Value <> 0];
      stSmall,
      stInteger,
      stLong:       fRawTemp := IntToRaw(Trunc(Value));
      stDate:       fRawTemp := DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False);
      stTime:       fRawTemp := DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False);
      stTimeStamp:  fRawTemp := DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, False);
      else          fRawTemp := FloatToSqlRaw(Value);
    end;
    SetRawByteString(Index, fRawTemp)
  end;
var InParamIdx: Integer;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, SQLType);
  if PGSQLType in [stCurrency, stBigDecimal] then
    if PGSQLType = stBigDecimal then begin
      Double2BCD(Value, PBCD(@fABuffer[0])^);
      SetBigDecimal(Index, PBCD(@fABuffer[0])^);
    end else
      SetCurrency(Index, Value)
  else if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
    if PGSQLType in [stBoolean, stFloat, stSmall, stInteger, stDate] then begin
      BindList.Put(Index, PGSQLType, P4Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], 4);
    end else begin
      BindList.Put(Index, PGSQLType, P8Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
    end;
    case PGSQLType of
      stBoolean:    PByte(FPQparamValues[InParamIdx])^ := Byte(Ord(Value <> 0));
      stSmall:      SmallInt2PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stInteger:    Integer2PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stLong:       Int642PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stFloat:      Single2PG(Value, FPQparamValues[InParamIdx]);
      stDouble:     {$IFNDEF ENDIAN_BIG}Reverse8Bytes(FPQparamValues[InParamIdx]){$ENDIF};
      stDate:       Date2PG(Value, PInteger(FPQparamValues[InParamIdx])^);
      stTime:       if Finteger_datetimes
                    then Time2PG(Value, PInt64(FPQparamValues[InParamIdx])^)
                    else Time2PG(Value, PDouble(FPQparamValues[InParamIdx])^);
      stTimeStamp:  if Finteger_datetimes
                    then DateTime2PG(Value, PInt64(FPQparamValues[InParamIdx])^)
                    else DateTime2PG(Value, PDouble(FPQparamValues[InParamIdx])^);
                    end;
  end else SetAsRaw;
end;

procedure TZPostgreSQLPreparedStatementV3.InternalBindInt(Index: Integer;
  SQLType: TZSQLType; Value: {$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF});
var PGSQLType: TZSQLType;
{ move the string conversions into a own proc -> no (U/L)StrClear}
procedure SetAsRaw; begin SetRawByteString(Index, IntToRaw(Value)); end;
var InParamIdx: Integer;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, SQLType);
  if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
    if (FPQparamValues[InParamIdx] = nil) or (BindList[Index].SQLType <> PGSQLType) then
      if ZSQLType2PGBindSizes[PGSQLType] <= 4 then begin
        BindList.Put(Index, PGSQLType, P4Bytes(@Value));
        LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], ZSQLType2PGBindSizes[PGSQLType]);
      end else if ZSQLType2PGBindSizes[PGSQLType] = 8 then begin
        BindList.Put(Index, PGSQLType, P8Bytes({$IFNDEF CPU64}@faBuffer[0]{$ELSE}@Value{$ENDIF}));
        LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
      end else
        LinkBinParam2PG(InParamIdx, BindList.AquireCustomValue(Index, PGSQLType,
          ZSQLType2PGBindSizes[PGSQLType]), ZSQLType2PGBindSizes[PGSQLType]);
    case PGSQLType of
      stBoolean:  PByte(FPQparamValues[InParamIdx])^ := Ord(Value);
      stSmall:    SmallInt2PG(Value, FPQparamValues[InParamIdx]);
      stInteger,
      stDate:     Integer2PG(Value, FPQparamValues[InParamIdx]);
      stFloat:    Single2PG(Value, FPQparamValues[InParamIdx]);
      stLongWord: Cardinal2PG(Value, FPQparamValues[InParamIdx]);
      stLong:     Int642PG(Value, FPQparamValues[InParamIdx]);
      stDouble:   Double2PG(Value, FPQparamValues[InParamIdx]);
      stCurrency: Currency2PGNumeric(Value, FPQparamValues[InParamIdx], FPQparamLengths[InParamIdx]);
      stBigDecimal: Begin
                      ScaledOrdinal2BCD(Value, 0, PBCD(@fABuffer[0])^);
                      BCD2PGNumeric(PBCD(@fABuffer[0])^, FPQparamValues[InParamIdx], FPQparamLengths[InParamIdx]);
                    end;
    end;
  end else SetAsRaw;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatementV3.PrepareInParameters;
var
  res: TPGresult;
  I: Integer;
  pgOID, zOID: OID;
  NewSQLType: TZSQLType;
begin
  if (fRawPlanName <> '') and not (Findeterminate_datatype) and (BindList.Capacity > 0) then begin
    if Assigned(FPlainDriver.PQdescribePrepared) then begin
      res := FPlainDriver.PQdescribePrepared(FconnAddress^, Pointer(FRawPlanname));
      try
        BindList.SetCount(FplainDriver.PQnparams(res)+FOutParamCount);
        for i := 0 to BindList.Count-FOutParamCount-1 do begin
          pgOID := FplainDriver.PQparamtype(res, i);
          NewSQLType := PostgreSQLToSQLType(ConSettings, fOIDAsBlob, pgOID, -1);
          zOID := FPQParamOIDs[i];
          FPQParamOIDs[i] := pgOID;
          if NewSQLType in [stUnicodeString, stUnicodeStream]
          then NewSQLType := TZSQLType(Ord(NewSQLType) -1)
          else if (NewSQLType = stBigDecimal) and (BindList.SQLTypes[i] = stCurrency)
            then NewSQLType := stCurrency;
          if (NewSQLType <> BindList.SQLTypes[i]) and (FPQparamValues[I] <> nil) then
          {if (pgOID <> FPQParamOIDs[i]) then begin
            zOID := FPQParamOIDs[i];}
            //bind bin again or switch to string format if we do not support the PG Types -> else Error
            //if (FPQparamValues[I] <> nil) then
              case BindList.SQLTypes[i] of
                stBoolean:  SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Boolean(PByte(FPQparamValues[i])^));
                stSmall:    SetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2SmallInt(FPQparamValues[i]));
                stInteger:  SetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Integer(FPQparamValues[i]));
                stLongWord,
                stLong:     SetLong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Int64(FPQparamValues[i]));
                stCurrency: if zOID  = CASHOID
                            then SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGCash2Currency(FPQparamValues[i]))
                            else SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGNumeric2Currency(FPQparamValues[i]));
                stFloat:    SetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Single(FPQparamValues[i]));
                stDouble:   SetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Double(FPQparamValues[i]));
                stBigDecimal:begin
                              PGNumeric2BCD(FPQparamValues[i], PBCD(@fABuffer[0])^);
                              SetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PBCD(@fABuffer[0])^);
                            end;
                stTime:     if Finteger_datetimes
                            then SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Time(PInt64(FPQparamValues[i])^))
                            else SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Time(PDouble(FPQparamValues[i])^));
                stDate:     SetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},PG2Date(PInteger(FPQparamValues[i])^));
                stTimeStamp:if Finteger_datetimes
                            then SetTimeStamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},PG2DateTime(PInt64(FPQparamValues[i])^))
                            else SetTimeStamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},PG2DateTime(PDouble(FPQparamValues[i])^));
              end;
          //end;
        end;
      finally
        FPlainDriver.PQclear(res);
      end;
    end else
      for i := 0 to BindList.Count-1 do
        SQLTypeToPostgreSQL(BindList.SQLTypes[i], fOIDAsBlob, FPQParamOIDs[i]);
  end;
end;

procedure TZPostgreSQLPreparedStatementV3.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var I: Integer;
begin
  if SQLType in [stUnicodeString, stUnicodeStream] then
    SQLType := TZSQLType(Ord(SQLType)-1);
  I := ParameterIndex;
  CheckParameterIndex(I);
  BindList.SetParamTypes(ParameterIndex , SQLType, ParamType);
  if Name <> '' then
    FParamNames[ParameterIndex] := ConSettings.ConvFuncs.ZStringToRaw(Name, ConSettings.CTRL_CP, FClientCP);

  if ParamType in [pctOut, pctReturn] then begin
    FOutParamCount := 0;
    for i := 0 to BindList.Count -1 do
      Inc(FOutParamCount, Ord(BindList.ParamTypes[i] in [pctOut, pctReturn]));
  end;
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetBigDecimal(Index: Integer;
  const Value: TBCD);
var SQLType: TZSQLType;
procedure SetAsRaw; begin SetRawByteString(Index, BcdToSQLRaw(Value)); end;
var Idx: Integer;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  SQLType := OIDToSQLType(Idx, stBigDecimal);
  if (FPQParamOIDs[Idx] = NUMERICOID) then begin
    if (FPQparamValues[Idx] = nil) or (BindList.SQLTypes[Idx] <> SQLType) then begin
      FPQparamValues[Idx] := BindList.AquireCustomValue(Idx, SQLType, MaxBCD2NumSize);
      FPQparamFormats[Idx] := ParamFormatBin;
    end;
    BCD2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
  end else case SQLType of
    stBoolean,
    stSmall,
    stInteger,
    {$IFDEF CPU64}stLong,{$ENDIF}
    stLongWord: InternalBindInt(Index, SQLType, ZSysUtils.BCD2Int64(Value));
    {$IFNDEF CPU64}
    stLong:     SetLong(Index, BCD2Int64(Value));
    {$ENDIF}
    stTime, stDate, stTimeStamp, stFloat,
    stDouble:   InternalBindDouble(Index, SQLType, BCDToDouble(Value));
    else SetAsRaw;
  end;
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  InternalBindInt(Index, stBoolean, Ord(Value));
end;

{**
  Sets the designated parameter to a Java <code>unsigned 8Bit int</code> value.
  The driver converts this
  to an SQL <code>BYTE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetByte(Index: Integer; Value: Byte);
begin
  InternalBindInt(Index, stSmall, Value);
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetCurrency(Index: Integer;
  const Value: Currency);
var SQLType: TZSQLType;
procedure SetAsRaw; begin SetRawByteString(Index, CurrToRaw(Value)); end;
var Idx: Integer;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  SQLType := OIDToSQLType(Idx, stCurrency);
  if (FPQParamOIDs[Idx] = NUMERICOID) then begin
    if (FPQparamValues[Idx] = nil) or (BindList.SQLTypes[Idx] <> SQLType) then begin
      FPQparamValues[Idx] := BindList.AquireCustomValue(Idx, SQLType, MaxCurr2NumSize);
      FPQparamFormats[Idx] := ParamFormatBin;
    end;
    Currency2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
  end else case SQLType of
    stBoolean:  InternalBindInt(Idx, stBoolean, Ord(Value <> 0));
    stSmall,
    stInteger:  InternalBindInt(Idx, SQLType, PInt64(@Value)^ div 10000);
    stLongWord: begin
                  BindList.Put(Idx, stLong, P4Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._4Bytes[Idx], SizeOf(Cardinal));
                  Cardinal2PG(Cardinal(PInt64(@Value)^ div 10000), FPQparamValues[Idx]);
                end;
    stLong:     begin
                  BindList.Put(Idx, stLong, P8Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._8Bytes[Idx], SizeOf(Int64));
                  Int642PG(PInt64(@Value)^ div 10000, FPQparamValues[Idx]);
                end;
    stTime, stDate, stTimeStamp, stFloat,
    stDouble:   InternalBindDouble(Idx, SQLType, Value);
    stCurrency: begin
                  BindList.Put(Idx, stCurrency, P8Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._8Bytes[Idx], SizeOf(Currency));
                  Currency2PGCash(Value, FPQparamValues[Idx]);
                end;
    else SetAsRaw;
  end;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetDate(Index: Integer;
  const Value: TDateTime);
begin
  InternalBindDouble(Index, stDate, Value);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetDouble(Index: Integer;
  const Value: Double);
begin
  InternalBindDouble(Index, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetFloat(Index: Integer;
  Value: Single);
begin
  InternalBindDouble(Index, stFloat, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetInt(Index, Value: Integer);
begin
  InternalBindInt(Index, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetLong(Index: Integer;
  const Value: Int64);
{$IFDEF CPU64}
begin
  InternalBindInt(Index, stLong, Value);
{$ELSE}
{ move the string conversions into a own proc -> no (U/L)StrClear}
var PGSQLType: TZSQLType;
procedure SetAsRaw; begin SetRawByteString(Index, IntToRaw(Value)); end;
var Idx: Integer;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(Idx, stLong);
  if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    if (FPQparamValues[Idx] = nil) or (BindList[Idx].SQLType <> PGSQLType) then
      if ZSQLType2PGBindSizes[PGSQLType] <= 4 then begin
        BindList.Put(Idx, PGSQLType, P4Bytes(@Value));
        LinkBinParam2PG(Idx, BindList._4Bytes[Idx], ZSQLType2PGBindSizes[PGSQLType]);
      end else if ZSQLType2PGBindSizes[PGSQLType] = 8 then begin
        BindList.Put(Idx, PGSQLType, P8Bytes(@Value));
        LinkBinParam2PG(Idx, BindList._8Bytes[Idx], 8);
      end else
        LinkBinParam2PG(Idx, BindList.AquireCustomValue(Idx, PGSQLType,
          ZSQLType2PGBindSizes[PGSQLType]), ZSQLType2PGBindSizes[PGSQLType]);
    case PGSQLtype of
      stLong:     Int642PG(Value, FPQparamValues[Idx]);
      stBoolean:  PByte(FPQparamValues[Idx])^ := Ord(Value);
      stSmall:    SmallInt2PG(Value, FPQparamValues[Idx]);
      stInteger,
      stDate:     Integer2PG(Value, FPQparamValues[Idx]);
      stLongWord: Cardinal2PG(Value, FPQparamValues[Idx]);
      stFloat:    Single2PG(Value, FPQparamValues[Idx]);
      stDouble:   Double2PG(Value, FPQparamValues[Idx]);
      stCurrency: Currency2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
    end;
  end else SetAsRaw;
{$ENDIF}
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZPostgreSQLPreparedStatementV3.SetNull(Index: Integer;
  SQLType: TZSQLType);
var InParamIdx: Integer;
begin
  InParamIdx := {$IFNDEF GENERIC_INDEX}Index -1{$ENDIF};
  SQLType := OIDToSQLType(InParamIdx, SQLType);
  BindList.SetNull(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType);
  FPQparamFormats[InParamIdx] := ParamFormatStr;
  FPQparamValues[InParamIdx] := nil
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetShort(Index: Integer;
  Value: ShortInt);
begin
  InternalBindInt(Index, stSmall, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetSmall(Index: Integer;
  Value: SmallInt);
begin
  InternalBindInt(Index, stSmall, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetTime(Index: Integer;
  const Value: TDateTime);
begin
  InternalBindDouble(Index, stTime, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetTimestamp(Index: Integer;
  const Value: TDateTime);
begin
  InternalBindDouble(Index, stTimeStamp, Value);
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetUInt(Index: Integer;
  Value: Cardinal);
begin
  {$IFDEF CPU64}
  InternalBindInt(Index, stLong, Value);
  {$ELSE}
  SetLong(Index, Value);
  {$ENDIF}
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetULong(Index: Integer;
  const Value: UInt64);
begin
  {$IFDEF CPU64}
  InternalBindInt(Index, stLong, Value);
  {$ELSE}
  SetLong(Index, Value);
  {$ENDIF}
end;

{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetWord(Index: Integer; Value: Word);
begin
  InternalBindInt(Index, stInteger, Value);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatementV3.UnPrepareInParameters;
begin
  { release allocated memory }
  SetParamCount(0);
  Findeterminate_datatype := False;
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

