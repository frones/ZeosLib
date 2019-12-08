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

unit ZDbcPostgreSqlResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$ELSE}Types{$ENDIF},
  FmtBCD, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZPlainPostgreSqlDriver, ZDbcLogging,
  ZDbcResultSetMetadata, ZCompatibility, ZDbcCache, ZDbcGenericResolver,
  ZClasses;

type
  TZPGColumnInfo = class(TZColumnInfo)
  protected
    fTableOID, FColOID: OID;
    fTableColNo: Integer;
  public
    property TableOID: OID read fTableOID write fTableOID;
    property TableColNo: Integer read fTableColNo write fTableColNo;
    property ColumnOID: OID read FColOID write FColOID;
  end;

  {** Implements Postgres ResultSet Metadata. }
  TZPostgresResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure LoadColumns; override;
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  { Postgres Error Class}
  EZPGConvertError = class(EZSQLException);

  {** Implements PostgreSQL ResultSet. }
  TZAbstractPostgreSQLStringResultSet = class(TZAbstractReadOnlyResultSet_A, IZResultSet)
  private
    FconnAddress: PPGconn;
    Fres: TPGresult;
    FresAddress: PPGresult;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
    FIs_bytea_output_hex: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FCachedLob: boolean;
    FClientCP: Word;
    FResultFormat: PInteger;
    FBinaryValues, Finteger_datetimes, FIsOidAsBlob: Boolean;
    FDecimalSeps: array[Boolean] of Char;
    procedure ClearPGResult;
    function CreatePGConvertError(ColumnIndex: Integer; DataType: OID): EZPGConvertError;
  protected
    procedure Open; override;
    procedure DefinePostgreSQLToSQLType({$IFDEF AUTOREFCOUNT}var{$ENDIF}ColumnInfo: TZPGColumnInfo;
      TypeOid: Oid; TypeModifier: Integer);
    function PGRowNo: Integer; virtual; abstract;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      connAddress: PPGconn; resAddress: PPGresult; ResultFormat: PInteger;
      const CachedLob: Boolean; const Chunk_Size, UndefinedVarcharAsStringLength: Integer);

    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); reintroduce; overload;
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer): TBytes;
    function GetBlob(ColumnIndex: Integer): IZBlob;

    function MoveAbsolute(Row: Integer): Boolean; override;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject]);
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZClientCursorPostgreSQLStringResultSet = Class(TZAbstractPostgreSQLStringResultSet)
  protected
    procedure Open; override;
    function PGRowNo: Integer; override;
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
  End;

  TZServerCursorPostgreSQLStringResultSet = Class(TZAbstractPostgreSQLStringResultSet)
  protected
    procedure Open; override;
    function PGRowNo: Integer; override;
  public
    function Next: Boolean; override;
  End;

  {** Represents an interface, specific for PostgreSQL blobs. }
  IZPostgreSQLOidBlob = interface(IZBlob)
    ['{BDFB6B80-477D-4CB1-9508-9541FEA6CD72}']
    function GetBlobOid: Oid;
    procedure WriteLob;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZPostgreSQLOidBlob = class(TZAbstractUnCachedBlob, IZPostgreSQLOidBlob, IZUnCachedLob)
  private
    FHandle: TPGconn;
    FBlobOid: Oid;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
  public
    constructor Create(const PlainDriver: TZPostgreSQLPlainDriver; const
      Data: Pointer; const Size: Integer; const Handle: TPGconn;
      const BlobOid: Oid; const Chunk_Size: Integer);

    function GetBlobOid: Oid;
    procedure ReadLob; override;
    procedure WriteLob; override;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);

    function Clone(Empty: Boolean = False): IZBlob; override;
  end;

  TZPostgreSQLByteaHexBlob = class(TZAbstractBlob)
  public
    constructor Create(Data: PAnsiChar);
  end;

  TZPostgreSQLByteaEscapedBlob = class(TZAbstractBlob)
  public
    constructor Create(const PlainDriver: TZPostgreSQLPlainDriver; Data: PAnsiChar);
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL version 7.4 and up. }
  TZPostgreSQLCachedResolverV74up = class(TZPostgreSQLCachedResolver)
  public
    procedure FormWhereClause({$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor: TZRowAccessor; var Result: SQLString); override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL version 8.0 and up. }
  TZPostgreSQLCachedResolverV8up = class(TZPostgreSQLCachedResolverV74up)
  protected
    procedure SetResolverStatementParamters(const Statement: IZStatement;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF} Params: TStrings); override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math,
  ZMessages, ZEncoding, ZFastCode, ZDbcPostgreSqlMetadata, ZDbcMetadata,
  ZDbcPostgreSql, ZDbcPostgreSqlUtils, ZDbcUtils, ZDbcProperties,
  ZVariant;


// added for suporting Infinity, -Infinity and NaN.
// See https://sourceforge.net/p/zeoslib/tickets/173/
// maybe this should be pushed into ZSysUtils.SQLStrToFloatDef?
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  var Result: Extended); overload;
begin
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
end;
{$IFEND}

procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Single;
  var Result: Single); overload;
begin
  {$IFDEF FPC2_6DOWN}
  {$R-}
  {$Q-}
  {$ENDIF}
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
  {$IFDEF FPC2_6DOWN}
    {$ifdef RangeCheckEnabled}
      {$R+}
    {$endif}
    {$ifdef OverFlowCheckEnabled}
      {$Q+}
    {$endif}
  {$ENDIF}
end;

procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Double;
  var Result: Double); overload;
begin
  {$IFDEF FPC2_6DOWN}
    {$R-}
    {$Q-}
  {$ENDIF}
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
  {$IFDEF FPC2_6DOWN}
    {$ifdef RangeCheckEnabled}
      {$R+}
    {$endif}
    {$ifdef OverFlowCheckEnabled}
      {$Q+}
    {$endif}
  {$ENDIF}
end;

{ TZAbstractPostgreSQLStringResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractPostgreSQLStringResultSet.ColumnsToJSON(
  JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
var
  C, L: Cardinal;
  P, pgBuff: PAnsiChar;
  RNo, H, I: Integer;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  UUID: TGUID absolute BCD;
  DT: TDateTime absolute BCD;
label ProcBts, jmpDate, jmpTime, jmpTS;
begin
  RNo := RowNo - 1;
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if FPlainDriver.PQgetisnull(Fres, RNo, C) <> 0 then
      if JSONWriter.Expand then begin
        if not (jcsSkipNulls in JSONComposeOptions) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[i]);
      P := FPlainDriver.PQgetvalue(Fres, RNo, C);
      with TZPGColumnInfo(ColumnsInfo[C]) do begin
        if FBinaryValues then
          case ColumnType of
            stUnknown     : JSONWriter.AddShort('null');
            stBoolean     : JSONWriter.AddShort(JSONBool[PByte(P)^<>0]);
            stSmall       : JSONWriter.Add(PG2SmallInt(P));
            stInteger     : JSONWriter.Add(PG2Integer(P));
            stLong        : JSONWriter.Add(PG2Int64(P));
            stCurrency    : if ColumnOID = CASHOID
                            then JSONWriter.AddCurr64(PGCash2Currency(P))
                            else JSONWriter.AddCurr64(PGNumeric2Currency(P));
            stFloat       : JSONWriter.AddSingle(PG2Single(P));
            stDouble      : JSONWriter.AddDouble(PG2Double(P));
            stBigDecimal  : begin
                              PGNumeric2BCD(P, BCD);
                              JSONWriter.AddNoJSONEscape(@fTinyBuffer[0], BCDToRaw(BCD, @fTinyBuffer[0], '.'));
                            end;
            stBytes,
            stBinaryStream: if ColumnOID = BYTEAOID then
                              JSONWriter.WrBase64(P, FPlainDriver.PQgetlength(Fres, RNo, C), True)
                            else begin
                              PPointer(@fTinyBuffer[0])^ := nil; //init avoid gpf
                              PIZlob(@fTinyBuffer[0])^ := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, PG2Cardinal(P), FChunk_Size);
                              JSONWriter.WrBase64(PIZlob(@fTinyBuffer[0])^.GetBuffer, PIZlob(@fTinyBuffer[0])^.Length, True);
                              PIZlob(@fTinyBuffer[0])^ := nil;
                            end;
            stGUID        : begin
                              JSONWriter.Add('"');
                              UUID.D1 := PG2Cardinal(@PGUID(P).D1); //what a sh...ttt! swapped digits!
                              UUID.D2 := PG2Word(@PGUID(P).D2);
                              UUID.D3 := PG2Word(@PGUID(P).D3);
                              PInt64(@UUID.D4)^ := PInt64(@PGUID(P).D4)^;
                              JSONWriter.Add(UUID);//
                              JSONWriter.Add('"');
                            end;
            stDate        : begin
jmpDate:                      if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              PG2Date(PInteger(P)^, TS.Year, TS.Month, TS.Day);
                              DateToIso8601PChar(@FTinyBuffer[0], True, TS.Year, TS.Month, TS.Day);
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], 10);
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stTime        : begin
jmpTime:                      if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("0000-00-00')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              if Finteger_datetimes
                              then dt2Time(PG2Int64(P), TS.Hour, TS.Minute, TS.Second, Ts.Fractions)
                              else dt2Time(PG2Double(P), TS.Hour, TS.Minute, TS.Second, Ts.Fractions);
                              TimeToIso8601PChar(@FTinyBuffer[0], True, TS.Hour, TS.Minute, TS.Second, TS.Fractions, 'T', jcoMilliseconds in JSONComposeOptions);
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], 9+4*Ord(not (jcoMongoISODate in JSONComposeOptions) and (jcoMilliseconds in JSONComposeOptions)));
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stTimestamp   : begin
jmpTS:                        if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              if Finteger_datetimes
                              then PG2DateTime(PInt64(P)^, TS.Year, TS.Month, TS.Day, Ts.Hour, TS.Minute, TS.Second, TS.Fractions)
                              else PG2DateTime(PDouble(P)^, TS.Year, TS.Month, TS.Day, Ts.Hour, TS.Minute, TS.Second, TS.Fractions);
                              DateToIso8601PChar(@FTinyBuffer[0], True, TS.Year, TS.Month, TS.Day);
                              TimeToIso8601PChar(@FTinyBuffer[10], True, TS.Hour, TS.Minute, TS.Second, TS.Fractions, 'T', jcoMilliseconds in JSONComposeOptions);
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],19+(4*Ord(not (jcoMongoISODate in JSONComposeOptions) and (jcoMilliseconds in JSONComposeOptions))));
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stString,
            stUnicodeString:if (ColumnOID = MACADDROID) then begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PGMacAddr2Raw(P, @FTinyBuffer[0]));
                              JSONWriter.Add('"');
                            end else if (ColumnOID = INETOID) or (ColumnOID = CIDROID) then begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PGInetAddr2Raw(P, @FTinyBuffer[0]));
                              JSONWriter.Add('"');
                            end else if ColumnOID = INTERVALOID then begin
                              if Finteger_datetimes
                              then DT := PG2DateTime(PInt64(P)^)
                              else DT := PG2DateTime(PDouble(P)^);
                              DT := DT + (PG2Integer(P+8)-102) * SecsPerDay + PG2Integer(P+12) * SecsPerDay * 30;
                              P := @fTinyBuffer[0];
                              if Int(DT) = 0 then begin
                                DecodeDate(DT, TS.Year, Ts.Month, Ts.Day);
                                Date2PG(DT, PInteger(P)^);
                                goto jmpDate;
                              end else begin
                                if Finteger_datetimes
                                then DateTime2PG(DT, PInt64(P)^)
                                else DateTime2PG(DT, PDouble(P)^);
                                if Frac(DT) = 0
                                then goto jmpTime
                                else goto jmpTS;
                              end;
                            end else begin
                              JSONWriter.Add('"');
                              if (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID)
                              then JSONWriter.AddJSONEscape(P, ZDbcUtils.GetAbsorbedTrailingSpacesLen(P, SynCommons.StrLen(P)))
                              else JSONWriter.AddJSONEscape(P, SynCommons.StrLen(P));
                              JSONWriter.Add('"');
                            end;
            stAsciiStream,
            stUnicodeStream:if (ColumnOID = JSONOID) or (ColumnOID = JSONBOID) then
                              JSONWriter.AddNoJSONEscape(P, SynCommons.StrLen(P))
                            else begin
                              JSONWriter.Add('"');
                              JSONWriter.AddJSONEscape(P, SynCommons.StrLen(P));
                              JSONWriter.Add('"');
                            end;
            //stArray, stDataSet,
          end
        else
          case ColumnType of
            stUnknown     : JSONWriter.AddShort('null');
            stBoolean     : JSONWriter.AddShort(JSONBool[StrToBoolEx(P, True, (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID))]);
            stByte..stDouble, stBigDecimal  : JSONWriter.AddNoJSONEscape(P, ZFastCode.StrLen(P));
            stCurrency    : JSONWriter.AddDouble(ZSysUtils.SQLStrToFloatDef(P, 0, ZFastCode.StrLen(P)));
            stBytes,
            stBinaryStream: if ColumnOID = BYTEAOID then begin
                              pgBuff := nil;
                              if FIs_bytea_output_hex then begin
                                {skip trailing /x}
                                L := (ZFastCode.StrLen(P)-2) shr 1;
                                try
                                  GetMem(pgBuff, L);
                                  HexToBin(P+2, pgBuff, L);
                                  JSONWriter.WrBase64(pgBuff, L, True);
                                finally
                                  FreeMem(pgBuff);
                                end;
                              end else if Assigned(FPlainDriver.PQUnescapeBytea) then
                                try
                                  pgBuff := FPlainDriver.PQUnescapeBytea(P, @L);
                                  JSONWriter.WrBase64(pgBuff, L, True);
                                finally
                                  FPlainDriver.PQFreemem(pgBuff);
                                end
                              else
                                JSONWriter.WrBase64(P, FPlainDriver.PQgetlength(Fres, RNo, C), True);
                            end else begin
                              PPointer(@fTinyBuffer[0])^ := nil; //init avoid gpf
                              PIZlob(@fTinyBuffer[0])^ := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, RawToIntDef(P, 0), FChunk_Size);
                              JSONWriter.WrBase64(PIZlob(@fTinyBuffer[0])^.GetBuffer, PIZlob(@fTinyBuffer[0])^.Length, True);
                              PIZlob(@fTinyBuffer[0])^ := nil;
                            end;
            stGUID        : begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(P);//
                              JSONWriter.Add('"');
                            end;
            stDate        : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('"');
                            end;
            stTime        : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("0000-00-00T');
                              JSONWriter.AddNoJSONEscape(P, 8); //mongo has no milliseconds
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              JSONWriter.AddShort('T');
                              if ((P+8)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions) //time zone ?
                              then JSONWriter.AddNoJSONEscape(P, 8)
                              else JSONWriter.AddNoJSONEscape(P, 12);
                              JSONWriter.Add('"');
                            end;
            stTimestamp   : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('T');
                              JSONWriter.AddNoJSONEscape(P+11, 8);//mongo has no milliseconds
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('T');
                              if ((P+19)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions)
                              then JSONWriter.AddNoJSONEscape(P+11, 8)
                              else JSONWriter.AddNoJSONEscape(P+11, 12);
                              JSONWriter.Add('"');
                            end;
            stString,
            stUnicodeString:begin
                              JSONWriter.Add('"');
                              if (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID)
                              then JSONWriter.AddJSONEscape(P, ZDbcUtils.GetAbsorbedTrailingSpacesLen(P, SynCommons.StrLen(P)))
                              else JSONWriter.AddJSONEscape(P, SynCommons.StrLen(P));
                              JSONWriter.Add('"');
                            end;
            stAsciiStream,
            stUnicodeStream:if (ColumnOID = JSONOID) or (ColumnOID = JSONBOID) then
                              JSONWriter.AddNoJSONEscape(P, SynCommons.StrLen(P))
                            else begin
                              JSONWriter.Add('"');
                              JSONWriter.AddJSONEscape(P, SynCommons.StrLen(P));
                              JSONWriter.Add('"');
                            end;
            //stArray, stDataSet,
          end;
      end;
      JSONWriter.Add(',');
    end;
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a PostgreSQL plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a PostgreSQL specific query handle.
}
constructor TZAbstractPostgreSQLStringResultSet.Create(const Statement: IZStatement;
  const SQL: string; connAddress: PPGconn; resAddress: PPGresult;
  ResultFormat: PInteger; const CachedLob: Boolean;
  const Chunk_Size, UndefinedVarcharAsStringLength: Integer);
var PGCon: IZPostgreSQLConnection;
begin
  inherited Create(Statement, SQL,
    TZPostgresResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FconnAddress := connAddress;
  Fres := resAddress^;
  FresAddress := resAddress;
  FPlainDriver := TZPostgreSQLPlainDriver(Statement.GetConnection.GetIZPlainDriver.GetInstance);
  ResultSetConcurrency := rcReadOnly;
  FChunk_Size := Chunk_Size; //size of read/write lob in chunks
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FIsOidAsBlob := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Statement, DSProps_OidAsBlob, 'False'));
  Statement.GetConnection.QueryInterface(IZPostgreSQLConnection, PGCon);
  FIs_bytea_output_hex := PGCon.Is_bytea_output_hex;
  Finteger_datetimes := PGCon.integer_datetimes;
  PGCon := nil;
  FResultFormat := ResultFormat;
  FBinaryValues := FResultFormat^ = ParamFormatBin;
  FCachedLob := CachedLob;
  FClientCP := ConSettings.ClientCodePage.CP;
  Open;
end;

function TZAbstractPostgreSQLStringResultSet.CreatePGConvertError(
  ColumnIndex: Integer; DataType: OID): EZPGConvertError;
begin
  Result := EZPGConvertError.Create(Format(SErrorConvertionField,
        [TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnLabel, IntToStr(DataType)]));
end;

procedure TZAbstractPostgreSQLStringResultSet.ClearPGResult;
begin
  if Fres <> nil then
  begin
    FPlainDriver.PQclear(Fres);
    Fres := nil;
  end;
end;

{**
  Converts a PostgreSQL native types into ZDBC SQL types.
  @param ColumnIndex a column index.
  @param ColumnInfo a column description object.
  @param TypeOid a type oid.
  @return a SQL undepended type.
}
procedure TZAbstractPostgreSQLStringResultSet.DefinePostgreSQLToSQLType(
  {$IFDEF AUTOREFCOUNT}var{$ENDIF}ColumnInfo: TZPGColumnInfo; TypeOid: Oid;
  TypeModifier: Integer);
var
  SQLType: TZSQLType;
  Connection: IZPostgreSQLConnection;
begin
  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  case TypeOid of
    CASHOID: ColumnInfo.Currency := True; { money }
    NAMEOID: if (Connection.GetServerMajorVersion < 7) or
           ((Connection.GetServerMajorVersion = 7) and (Connection.GetServerMinorVersion < 3))
        then ColumnInfo.Precision := 32
        else ColumnInfo.Precision := 64; { name }
    CIDROID: ColumnInfo.Precision := 100; { cidr }
    INETOID: ColumnInfo.Precision := 100{39}; { inet }
    MACADDROID: ColumnInfo.Precision := 17; { macaddr }
    INTERVALOID: ColumnInfo.Precision := 32; { interval }
    REGPROCOID: ColumnInfo.Precision := 64; { regproc } // M.A. was 10
    BYTEAOID:{ bytea }
      if TypeModifier >= VARHDRSZ then
        ColumnInfo.Precision := TypeModifier - VARHDRSZ
      else if Connection.IsOidAsBlob then
        ColumnInfo.Precision := 256;
    //see: https://www.postgresql.org/message-id/slrnd6hnhn.27a.andrew%2Bnonews%40trinity.supernews.net
    //macro:
    //numeric: this is ugly, the typmod is ((prec << 16) | scale) + VARHDRSZ,
    //i.e. numeric(10,2) is ((10 << 16) | 2) + 4
    NUMERICOID: if TypeModifier <> -1 then begin
        ColumnInfo.Precision := (TypeModifier - VARHDRSZ) shr 16 and $FFFF;
        ColumnInfo.Scale     := (TypeModifier - VARHDRSZ)        and $FFFF;
        if (ColumnInfo.Scale <= 4) and (ColumnInfo.Precision <= sAlignCurrencyScale2Precision[ColumnInfo.Scale])
        then ColumnInfo.ColumnType := stCurrency
        else ColumnInfo.ColumnType := stBigDecimal;
        Exit;
      end;
    TIMESTAMPOID, TIMESTAMPTZOID, ABSTIMEOID: begin
      ColumnInfo.ColumnType := stTimestamp; { timestamp,timestamptz/abstime. no 'datetime' any more}
      if TypeModifier <> -1 then
        ColumnInfo.Scale := TypeModifier; //milliseconds or negative if not specified
      Exit;
    end;
    TIMEOID, TIMETZOID: begin
      ColumnInfo.ColumnType := stTime;
      if TypeModifier <> -1 then
        ColumnInfo.Scale := TypeModifier;
      Exit;
    end;
  end;

  SQLType := PostgreSQLToSQLType(ConSettings, Connection.IsOidAsBlob, TypeOid, TypeModifier);

  if SQLType <> stUnknown then
    ColumnInfo.ColumnType := SQLType
  else begin
    ColumnInfo.ColumnType := stString;
    ColumnInfo.Precision := 255;
    ColumnInfo.ReadOnly := True;
  end;
end;

{**
  Opens this recordset.
}
procedure TZAbstractPostgreSQLStringResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZPGColumnInfo;
  FieldMode, FieldSize, FieldType, FieldCount: Integer;
  P: PAnsiChar;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldCount := FPlainDriver.PQnfields(Fres);
  for I := 0 to FieldCount - 1 do
  begin
    ColumnInfo := TZPGColumnInfo.Create;
    with ColumnInfo do
    begin
      TableOID := FPlainDriver.PQftable(Fres, I);
      TableColNo := FplainDriver.PQftablecol(Fres, I);
      //See: http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=20797
      if TableColNo < 1 then
        // these fields have fixed numbers in the PostgreSQL source code, they seem to not use 0
        case TableColNo of
          0: ColumnName := '';
          -1: ColumnName := 'ctid';
          -2: ColumnName := 'oid';
          -3: ColumnName := 'xmin';
          -4: ColumnName := 'cmin';
          -5: ColumnName := 'xmax';
          -6: ColumnName := 'cmax';
          -7: ColumnName := 'tableoid';
        end;
      P := FPlainDriver.PQfname(Fres, I);
      Precision := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      ColumnLabel := PRawToUnicode(P, Precision, FClientCP);
      {$ELSE}
      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(FClientCP, ConSettings^.CTRL_CP)
      then ColumnLabel := BufferToStr(P, Precision)
      else ColumnLabel := ZUnicodeToString(PRawToUnicode(P, Precision, FClientCP), ConSettings^.CTRL_CP);
      {$ENDIF}
      Nullable := ntNullableUnknown; //there is NO information about nullable
      Precision := 0;

      FieldType := FPlainDriver.PQftype(Fres, I);

      ColumnOID := FieldType;
      FieldMode := FPlainDriver.PQfmod(Fres, I);
      DefinePostgreSQLToSQLType(ColumnInfo, FieldType, FieldMode);
      if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]
      then ColumnCodePage := FClientCP
      else ColumnCodePage := High(Word);

      if Precision = 0 then begin
        FieldSize := FPlainDriver.PQfsize(Fres, I);
        Precision := Max(Max(FieldMode - 4, FieldSize), 0);

        if ColumnType in [stString, stUnicodeString] then begin
          {begin patch: varchar() is equal to text!}
          if ( FieldMode = -1 ) and ( FieldSize = -1 ) and ( FieldType = 1043) then
            if FUndefinedVarcharAsStringLength > 0
            then Precision := FUndefinedVarcharAsStringLength
            else DefinePostgreSQLToSQLType(ColumnInfo, 25, FieldMode) //assume text instead!
          else if ( (ColumnLabel = 'expr') or ( Precision = 0 ) ) then
            Precision := 255;
          if ColumnType = stString then
            CharOctedLength := Precision * ConSettings^.ClientCodePage^.CharWidth
          else if ColumnType = stUnicodeString then
            CharOctedLength := Precision shl 1;
        end;
      end;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Resets cursor position of this recordset and
  reset the prepared handles.
}
procedure TZAbstractPostgreSQLStringResultSet.ResetCursor;
begin
  if not Closed then begin
    ClearPGResult;
    inherited ResetCursor;
  end;
end;
{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractPostgreSQLStringResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := FPlainDriver.PQgetisnull(Fres, RowNo - 1,
    ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}) <> 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var L: LongWord;
  PEnd: PAnsiChar;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  UUID: TGUID absolute BCD;
  DT: TDateTime absolute BCD;
  MS: Word;
  function FromOIDLob(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
  begin
    FRawTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetString;
    Result := Pointer(FRawTemp);
    Len := Length(FRawTemp);
  end;
label JmpPEndTinyBuf, JmpStr, jmpTime, jmpDate, jmpTS;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, PGRowNo, ColumnIndex) <> 0;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Result := FPlainDriver.PQgetvalue(Fres, PGRowNo, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:  if PByte(Result)^ = 0 then begin
                      Result := Pointer(BoolStrsRaw[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsRaw[False]);
                      Len := 5;
                    end;
        stSmall:    begin
                      IntToRaw(PG2SmallInt(Result), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLongWord: begin
                      IntToRaw(PG2Cardinal(Result), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stInteger:  begin
                      IntToRaw(PG2Integer(Result), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLong:     begin
                      IntToRaw(PG2int64(Result), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stFloat:    begin
                      Len := FloatToSqlRaw(PG2Single(Result), @fTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end;
        stDouble:   begin
                      Len := FloatToSqlRaw(PG2Double(Result), @fTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end;
        stCurrency: begin
                      CurrToRaw(GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), @fTinyBuffer[0], @PEnd);
JmpPEndTinyBuf:       Result := @fTinyBuffer[0];
                      Len := PEnd - Result;
                    end;
        stBigDecimal: begin
                      PGNumeric2BCD(Result, BCD{%H-});
                      Result := @fTinyBuffer[0];
                      Len := BCDToRaw(BCD, @fTinyBuffer[0], '.');
                    end;
        stDate:     begin
                      PG2Date(PInteger(Result)^, TS.Year, TS.Month, TS.Day);
jmpDate:              Result := @fTinyBuffer[0];
                      Len := DateToRaw(TS.Year, TS.Month, TS.Day, Result,
                        ConSettings.DisplayFormatSettings.DateFormat, False, False);
                    end;
        stTime:     begin
                      if Finteger_datetimes
                      then dt2time(PG2Int64(Result), TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else dt2time(PG2Double(Result), TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTime:              Result := @fTinyBuffer[0];
                      Len := TimeToRaw(TS.Hour, TS.Minute, TS.Second, TS.Fractions,
                        Result, ConSettings.DisplayFormatSettings.TimeFormat, False, False);
                    end;
        stTimestamp:begin
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(Result)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2DateTime(PDouble(Result)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTS:                Result := @fTinyBuffer[0];
                      Len := ZSysUtils.DateTimeToRaw(TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute,
                        TS.Second, TS.Fractions, Result, ConSettings.DisplayFormatSettings.DateTimeFormat, False, False);
                    end;
        stGUID:     begin
                      UUID.D1 := PG2Cardinal(@PGUID(Result).D1); //what a sh...ttt! swapped digits!
                      UUID.D2 := PG2Word(@PGUID(Result).D2);
                      UUID.D3 := PG2Word(@PGUID(Result).D3);
                      PInt64(@UUID.D4)^ := PInt64(@PGUID(Result).D4)^;
                      ZSysUtils.GUIDToBuffer(@UUID.D1, PAnsiChar(@fTinyBuffer[0]), []); //pg does not Return brackets adopt behavior
                      for ColumnIndex := 0 to 35 do
                        fTinyBuffer[ColumnIndex] := fTinyBuffer[ColumnIndex] or $20;
                      Result := @fTinyBuffer[0];
                      Len := 36;
                    end;
        stString,
        stUnicodeString: if (ColumnOID = MACADDROID) then begin
                      Len := PGMacAddr2Raw(Result, @FTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end else if (ColumnOID = INETOID) or (ColumnOID = CIDROID) then begin
                      Len := PGInetAddr2Raw(Result, @FTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end else if ColumnOID = INTERVALOID then begin
                      if Finteger_datetimes
                      then DT := PG2DateTime(PInt64(Result)^)
                      else DT := PG2DateTime(PDouble(Result)^);
                      DT := DT + (PG2Integer(Result+8)-102) * SecsPerDay + PG2Integer(Result+12) * SecsPerDay * 30;
                      if Frac(DT) = 0 then begin
                        DecodeTime(DT, TS.Hour, Ts.Minute, Ts.Second, MS);
                        Ts.Fractions := 0;
                        goto jmpTime
                      end else if Int(DT) = 0 then begin
                        DecodeDate(DT, TS.Year, Ts.Month, Ts.Day);
                        goto jmpDate;
                      end;
                      DecodeTime(DT, TS.Hour, Ts.Minute, Ts.Second, MS);
                      DecodeDate(DT, TS.Year, Ts.Month, Ts.Day);
                      Ts.Fractions := 0;
                      goto jmpTS;
                    end else goto jmpStr;
        stAsciiStream,
        stUnicodeStream:Len := ZFastCode.StrLen(Result);
        stBytes:        Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := FromOIDLob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Len)
                        else Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
        else            begin
                          Result := PEmptyAnsiString;
                          Len := 0;
                        end;
      end
    else case ColumnOID of
      BYTEAOID: if FIs_bytea_output_hex then begin
                  {skip trailing /x}
                  SetLength(FRawTemp, (ZFastCode.StrLen(Result)-2) shr 1);
                  Len := Length(FRawTemp);
                  HexToBin(Result+2, Pointer(FRawTemp), Len);
                  Result := Pointer(FRawTemp);
                end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
                  Result := FPlainDriver.PQUnescapeBytea(Result, @L);
                  ZSetString(Result, L, FRawTemp);
                  Len := Length(FRawTemp);
                  FPlainDriver.PQFreemem(Result);
                  Result := Pointer(FRawTemp);
                end else
                  Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
      {OIDOID:   if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType = stBinaryStream then begin
                  FTempLob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
                    RawToInt64Def(Result, 0), FChunk_Size); //Localize it
                  Result := FTempLob.GetBuffer;
                  Len := FTempLob.Length;
                end else
                  Len := ZFastCode.StrLen(Result); }
      else      begin
                  {http://www.postgresql.org/docs/9.0/static/libpq-exec.html
                  PQgetlength:
                   This is the actual data length for the particular data value, that is,
                   the size of the object pointed to by PQgetvalue.
                   For text data format this is the same as strlen().
                   For binary format this is essential information.
                   Note that one should not rely on PQfsize to obtain the actual data length.}
JmpStr:           Len := ZFastCode.StrLen(Result);
                  if (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID) then
                    Len := GetAbsorbedTrailingSpacesLen(Result, Len);
                end;
    end;
  end;
end;

function TZAbstractPostgreSQLStringResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var P: PAnsiChar;
  PEnd: PWideChar;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  UUID: TGUID absolute BCD;
  DT: TDateTime absolute BCD;
  MS: Word;
  procedure FromOIDLob(ColumnIndex: Integer);
  var Lob: IZBlob;
  begin
    Lob := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    FUniTemp := Ascii7ToUnicodeString(Lob.GetBuffer, Lob.Length);
  end;
label JmpPEndTinyBuf, JmpUni, jmpStr, jmpTxt, jmpRaw, jmpBin, jmpLen, jmpTime, jmpDate, jmpTS;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, PGRowNo, ColumnIndex) <> 0;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    p := FPlainDriver.PQgetvalue(Fres, PGRowNo, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:  if PByte(P)^ = 0 then begin
                      Result := Pointer(BoolStrsW[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsW[False]);
                      Len := 5;
                    end;
        stSmall:    begin
                      IntToUnicode(PG2SmallInt(P), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLongWord: begin
                      IntToUnicode(PG2Cardinal(P), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stInteger:  begin
                      IntToUnicode(PG2Integer(P), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLong:     begin
                      IntToUnicode(PG2int64(P), @fTinyBuffer[0], @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stFloat:    begin
                      Len := FloatToSqlUnicode(PG2Single(P), @fTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end;
        stDouble:   begin
                      Len := FloatToSqlUnicode(PG2Double(P), @fTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end;
        stCurrency: begin
                      CurrToUnicode(GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), @fTinyBuffer[0], @PEnd);
JmpPEndTinyBuf:       Result := @fTinyBuffer[0];
                      Len := PEnd - Result;
                    end;
        stBigDecimal: begin
                      PGNumeric2BCD(P, BCD{%H-});
                      Result := @fTinyBuffer[0];
                      Len := BCDToUni(BCD, @fTinyBuffer[0], '.');
                    end;
        stDate:     begin
                      PG2Date(PInteger(P)^, TS.Year, TS.Month, TS.Day);
jmpDate:              Result := @fTinyBuffer[0];
                      Len := DateToUni(TS.Year, TS.Month, TS.Day,
                        Result, ConSettings.DisplayFormatSettings.DateFormat, False, False);
                    end;
        stTime:     begin
                      if Finteger_datetimes
                      then dt2time(PG2Int64(P), TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else dt2time(PG2Double(P), TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTime:              Result := @fTinyBuffer[0];
                      Len := TimeToUni(TS.Hour, TS.Minute, TS.Second, TS.Fractions,
                        Result, ConSettings.DisplayFormatSettings.TimeFormat, False, tS.IsNegative);
                    end;
        stTimestamp:begin
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(P)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2DateTime(PDouble(P)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTS:                Result := @fTinyBuffer[0];
                      Len := ZSysUtils.DateTimeToUni(TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute,
                        TS.Second, TS.Fractions, Result, ConSettings.DisplayFormatSettings.DateTimeFormat, False, False);
                    end;
        stGUID:     begin
                      UUID.D1 := PG2Cardinal(@PGUID(P).D1); //what a sh...ttt! swapped digits!
                      UUID.D2 := PG2Word(@PGUID(P).D2);
                      UUID.D3 := PG2Word(@PGUID(P).D3);
                      PInt64(@UUID.D4)^ := PInt64(@PGUID(P).D4)^;
                      ZSysUtils.GUIDToBuffer(@UUID.D1, PWideChar(@fTinyBuffer[0]), []);
                      for ColumnIndex := 0 to 35 do //to lowercase
                        PWord(@fTinyBuffer[ColumnIndex shl 1])^ := PWord(@fTinyBuffer[ColumnIndex shl 1])^ or $20;
                      Result := @fTinyBuffer[0];
                      Len := 36;
                    end;
        stUnicodeString,
        stString:   if (ColumnOID = MACADDROID) then begin
                      Len := PGMacAddr2Uni(P, @FTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end else if (ColumnOID = INETOID) or (ColumnOID = CIDROID) then begin
                      Len := PGInetAddr2Uni(P, @FTinyBuffer[0]);
                      Result := @fTinyBuffer[0];
                    end else if ColumnOID = INTERVALOID then begin
                      if Finteger_datetimes
                      then DT := PG2DateTime(PInt64(P)^)
                      else DT := PG2DateTime(PDouble(P)^);
                      DT := DT + (PG2Integer(P+8)-102) * SecsPerDay + PG2Integer(P+12) * SecsPerDay * 30;
                      if Frac(DT) = 0 then begin
                        DecodeTime(DT, TS.Hour, Ts.Minute, Ts.Second, MS);
                        Ts.Fractions := 0;
                        goto jmpTime
                      end else if Int(DT) = 0 then begin
                        DecodeDate(DT, TS.Year, Ts.Month, Ts.Day);
                        goto jmpDate;
                      end;
                      DecodeTime(DT, TS.Hour, Ts.Minute, Ts.Second, MS);
                      DecodeDate(DT, TS.Year, Ts.Month, Ts.Day);
                      Ts.Fractions := 0;
                      goto jmpTS;
                    end else goto jmpStr;
        stUnicodeStream,
        stAsciiStream: goto JmpTxt;
        stBytes:        begin
jmpBin:                   Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
                          goto jmpRaw;
                        end;
        stBinaryStream: if ColumnOID = OIDOID then begin
                          FromOIDLob(ColumnIndex);
                          goto jmpLen;
                        end else
                          goto jmpBin;
        else            begin
                          Result := PEmptyUnicodeString;
                          Len := 0;
                        end;
      end
    else begin
      if ColumnType in [stString,stUnicodeString] then
jmpStr: if (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID) then begin
          Len := GetAbsorbedTrailingSpacesLen(P, ZFastCode.StrLen(P));
          goto JmpUni;
        end else
          goto jmpTxt
      else if (ColumnType in [stAsciiStream,stUnicodeStream]) then begin
jmpTxt: Len := ZFastCode.StrLen(P);
JmpUni: FUniTemp := PRawToUnicode(P, Len, FClientCP);
      end else begin
         Len := ZFastCode.StrLen(P);
jmpRaw:  FUniTemp := Ascii7ToUnicodeString(P,Len);
      end;
jmpLen:
      Len := Length(FUniTemp);
      if (Len > 0)
      then Result := Pointer(FUniTemp)
      else Result := PEmptyUnicodeString;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractPostgreSQLStringResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := False
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^ <> 0;
        stSmall:                      Result := PWord(P)^ <> 0;
        stLongWord, stInteger, stDate:Result := PCardinal(P)^ <> 0;
        stULong, stLong:              Result := PUint64(P)^ <> 0;
        stFloat:                      Result := PSingle(P)^ <> 0;
        stDouble:                     Result := PDouble(P)^ <> 0;
        stCurrency:                   if ColumnOID = CASHOID
                                      then Result := PInt64(P)^ <> 0
                                      else Result := (PG2Word(P) > 0);//read nbasedigit count
        stBigDecimal:                 Result := PG2Word(P) > 0;//read nbasedigit count
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PInt64(P)^ <> 0
                                      else Result := PDouble(P)^ <> 0;
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := StrToBoolEx(P, True, (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID));
        //stBytes: ;
        //stBinaryStream: ;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else
      Result := StrToBoolEx(P, True, (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID));
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetInt(ColumnIndex: Integer): Integer;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(@fTinyBuffer[0])^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := PInt64(@fTinyBuffer[0])^ div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        Result := BCD2Int64(PBCD(@fTinyBuffer[0])^);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToIntDef(P, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else Result := RawToIntDef(P, 0);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetLong(ColumnIndex: Integer): Int64;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(Result)^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := Result div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        BCD2Int64(PBCD(@fTinyBuffer[0])^, Result);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToInt64Def(P, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else Result := RawToInt64Def(P, 0);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractPostgreSQLStringResultSet.GetUInt(
  ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

function TZAbstractPostgreSQLStringResultSet.GetULong(ColumnIndex: Integer): UInt64;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(Result)^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := PInt64(@Result)^ div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        BCD2UInt64(PBCD(@fTinyBuffer[0])^, Result);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToUInt64Def(P, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else Result := RawToUInt64Def(P, 0);
  end;

end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetFloat(ColumnIndex: Integer): Single;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := PG2Single(P);
        stDouble:                     Result := PG2Double(P);
        stCurrency:                   Result := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        Result := BCDToDouble(PBCD(@fTinyBuffer[0])^);
                                      end;
        stTime:                       if Finteger_datetimes
                                      then Result := PG2Time(PInt64(P)^)
                                      else Result := PG2Time(PDouble(P)^);
         stTimestamp:                 if Finteger_datetimes
                                      then Result := PG2DateTime(PInt64(P)^)
                                      else Result := PG2DateTime(PDouble(P)^);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else pgSQLStrToFloatDef(P, 0, Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>ZERO-UUID</code>
}
procedure TZAbstractPostgreSQLStringResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var
  Buffer, pgBuff: PAnsiChar;
  Len: cardinal;
  SrcUUID: PGUID absolute Buffer;
label Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if not LastWasNull then with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if ColumnOID = BYTEAOID {bytea} then begin
      if FBinaryValues then begin
        Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
        if Len = SizeOf(TGUID)
        then Move(Buffer^, Result.D1, SizeOf(TGUID))
        else goto Fail;
      end else if FIs_bytea_output_hex then begin
        {skip trailing /x}
        Len := (ZFastCode.StrLen(Buffer)-2) shr 1;
        if Len = SizeOf(TGUID)
        then {$IFDEF USE_SYNCOMMONS}SynCommons.{$ENDIF}HexToBin(Buffer+2, @Result.D1, SizeOf(TGUID))
        else goto Fail;
      end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
        pgBuff := FPlainDriver.PQUnescapeBytea(Buffer, @Len);
        if Len = SizeOf(TGUID) then begin
          Move(pgBuff^, Result.D1, SizeOf(TGUID));
          FPlainDriver.PQFreemem(pgBuff);
        end else begin
          FPlainDriver.PQFreemem(pgBuff);
          goto Fail;
        end;
      end else begin
        Len := ZFastCode.StrLen(Buffer);
        getMem(pgBuff, Len);
        Len := DecodeCString(Len, Buffer, pgBuff);
        if Len = SizeOf(TGUID) then begin
          Move(pgBuff^, Result.D1, SizeOf(TGUID));
          FreeMem(pgBuff);
        end else begin
          FreeMem(pgBuff);
          goto Fail;
        end;
      end;
    end else if ColumnOID = UUIDOID { uuid } then begin
      if FBinaryValues then begin
        Result.D1 := PG2Cardinal(@SrcUUID.D1);
        Result.D2 := PG2Word(@SrcUUID.D2);
        Result.D3 := PG2Word(@SrcUUID.D3);
        PInt64(@Result.D4)^ := PInt64(@SrcUUID.D4)^;
      end else ValidGUIDToBinary(Buffer, @Result.D1);
    end else case ColumnType of
      stString,
      stUnicodeString: if (ColumnOID = MACADDROID) or (ColumnOID = INETOID) or (ColumnOID = CIDROID) or (ColumnOID = INTERVALOID)
                      then goto Fail
                      else begin
                        Len := ZFastCode.StrLen(Buffer);
                        if (ColumnOID = CHAROID) or (ColumnOID = BPCHAROID) then
                          Len := GetAbsorbedTrailingSpacesLen(Buffer, Len);
                        if (Len = 36) or (Len = 38)
                        then ValidGUIDToBinary(Buffer, @Result.D1)
                        else goto Fail;
                      end;
      else
Fail:       raise CreatePGConvertError(ColumnIndex, ColumnOID);
    end;
  end else FillChar(Result, SizeOf(TGUID), #0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetDouble(ColumnIndex: Integer): Double;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger:                    Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := PG2Single(P);
        stDouble:                     Result := PG2Double(P);
        stCurrency:                   Result := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        Result := BCDToDouble(PBCD(@fTinyBuffer[0])^);
                                      end;
        stDate:                       Result := PG2Date(Pinteger(P)^);
        stTime:                       if Finteger_datetimes
                                      then Result := PG2Time(PInt64(P)^)
                                      else Result := PG2Time(PDouble(P)^);
        stTimestamp:                  if Finteger_datetimes
                                      then Result := PG2DateTime(PInt64(P)^)
                                      else Result := PG2DateTime(PDouble(P)^);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else pgSQLStrToFloatDef(P, 0, Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractPostgreSQLStringResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then FillChar(Result, SizeOf(TBCD), #0)
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean, stSmall,
        stInteger, stLong, stLongWord: ScaledOrdinal2BCD(GetLong(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), 0, Result);
        stFloat, stDouble,
        stDate, stTime, stTimeStamp:  Double2Bcd(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
        stCurrency:                   if ColumnOID = NUMERICOID
                                      then PGNumeric2BCD(P, Result)
                                      else ScaledOrdinal2BCD(PG2Int64(P), 2, Result);
        stBigDecimal:                 PGNumeric2BCD(P, Result);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    LastWasNull := not TryRawToBcd(P, StrLen(P), Result, '.');
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then ScaledOrdinal2BCD(PG2Cardinal(P), 0, Result, False)
                        else Result := NullBCD;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else LastWasNull := not TryRawToBcd(P, StrLen(P), Result, '.');
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  Buffer, pgBuff: PAnsiChar;
  Len: cardinal;
  TempLob: IZBLob;
  ResUUID: PGUID absolute Result;
  SrcUUID: PGUID absolute Buffer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if not LastWasNull then with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if ColumnOID = BYTEAOID {bytea} then begin
      if FBinaryValues then
        Result := BufferToBytes(Buffer, FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex))
      else if FIs_bytea_output_hex then begin
        {skip trailing /x}
        SetLength(Result, (ZFastCode.StrLen(Buffer)-2) shr 1);
        if Assigned(Result) then
          HexToBin(Buffer+2, Pointer(Result), Length(Result));
      end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
        pgBuff := FPlainDriver.PQUnescapeBytea(Buffer, @Len);
        Result := BufferToBytes(pgBuff, Len);
        FPlainDriver.PQFreemem(pgBuff);
      end else begin
        Len := ZFastCode.StrLen(Buffer);
        SetLength(Result, Len);
        SetLength(Result, DecodeCString(Len, Buffer, Pointer(Result)));
      end;
    end else if ColumnOID = UUIDOID { uuid } then begin
      SetLength(Result, SizeOf(TGUID)); //take care we've a unique dyn-array if so then this alloc happens once
      if FBinaryValues then begin
        ResUUID.D1 := PG2Cardinal(@SrcUUID.D1);
        ResUUID.D2 := PG2Word(@SrcUUID.D2);
        ResUUID.D3 := PG2Word(@SrcUUID.D3);
        PInt64(@ResUUID.D4)^ := PInt64(@SrcUUID.D4)^;
      end else ValidGUIDToBinary(Buffer, Pointer(Result));
    end else if ColumnOID = OIDOID { oid } then begin
      if FBinaryValues
      then Len := PG2Cardinal(Buffer)
      else Len := RawToIntDef(Buffer, 0);
      TempLob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
        Len, FChunk_Size);
      Result := TempLob.GetBytes
    end else if FBinaryValues then

    else Result := BufferToBytes(Buffer, ZFastCode.StrLen(Buffer))
  end else Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetCurrency(
  ColumnIndex: Integer): Currency;
var P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean, stSmall,
        stInteger, stLong, stLongWord:Result := GetLong(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stFloat, stDouble,
        stDate, stTime, stTimeStamp:  Result := GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stCurrency:                   if ColumnOID = NUMERICOID
                                      then Result := PGNumeric2Currency(P)
                                      else Result := PGCash2Currency(P);
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(@fTinyBuffer[0])^);
                                        BCDToCurr(PBCD(@fTinyBuffer[0])^, Result);
                                      end;
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if ColumnOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, ColumnOID);
      end
    else SQLStrToFloatDef(P, 0, FDecimalSeps[ColumnOID = CASHOID], Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractPostgreSQLStringResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
var
  Len: NativeUInt;
  P: PAnsiChar;
label from_str, Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then goto fill
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stDate:       begin
                        PG2Date(Pinteger(P)^, Result.Year, Result.Month, Result.Day);
                        Result.IsNegative := False;
                      end;
        stTime:       goto Fill;
        stTimestamp:  begin
                        Result.IsNegative := False;
                        if Finteger_datetimes
                        then PG2DateTime(PInt64(P)^, Result.Year, Result.Month, Result.Day,
                          PZTime(@FTinyBuffer[0])^.Hour, PZTime(@FTinyBuffer[0])^.Minute,
                          PZTime(@FTinyBuffer[0])^.Second, PZTime(@FTinyBuffer[0])^.Fractions)
                        else PG2DateTime(PDouble(P)^, Result.Year, Result.Month, Result.Day,
                          PZTime(@FTinyBuffer[0])^.Hour, PZTime(@FTinyBuffer[0])^.Minute,
                          PZTime(@FTinyBuffer[0])^.Second, PZTime(@FTinyBuffer[0])^.Fractions);
                  end;
        stBoolean, stSmall,
        stInteger, stLong, stLongWord,
        stFloat, stDouble,
        stCurrency, stBigDecimal:     DecodeDateTimeToDate(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    goto from_str;
        else raise CreatePGConvertError(ColumnIndex, FColOID);
      end
    else begin
from_str:
      Len := ZFastCode.StrLen(P);
      LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then
Fill:   PInt64(@Result.Year)^ := 0;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractPostgreSQLStringResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
var
  Len: NativeUInt;
  P: PAnsiChar;
label from_str, fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then goto Fill
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stDate:     goto Fill;
        stTime:     begin
                      Result.IsNegative := False;
                      if Finteger_datetimes
                      then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                      else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                    end;
        stTimestamp:begin
                      Result.IsNegative := False;
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(P)^, PZDate(@fTinyBuffer[0])^.Year,
                        PZDate(@fTinyBuffer[0])^.Month, PZDate(@fTinyBuffer[0])^.Day,
                        Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                      else PG2DateTime(PDouble(P)^, PZDate(@fTinyBuffer[0])^.Year,
                        PZDate(@fTinyBuffer[0])^.Month, PZDate(@fTinyBuffer[0])^.Day,
                        Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                    end;
        stBoolean, stSmall,
        stInteger, stLong, stLongWord,
        stFloat, stDouble,
        stCurrency, stBigDecimal: DecodeDateTimeToTime(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    goto from_str;
        else raise CreatePGConvertError(ColumnIndex, FColOID);
      end
    else begin
from_str:
      Len := ZFastCode.StrLen(P);
      LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then begin
Fill:   PCardinal(@Result.Hour)^ := 0;
        PInt64(@Result.Second)^ := 0;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
procedure TZAbstractPostgreSQLStringResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
var
  Len: NativeUInt;
  P: PAnsiChar;
label from_str, fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull
  then goto Fill
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stDate:     begin
                      PG2Date(Pinteger(P)^, Result.Year, Result.Month, Result.Day);
                      Result.IsNegative := False;
                      PInt64(@Result.Hour)^ := 0;
                      PInt64(@Result.Fractions)^ := 0;
                      goto Fill;
                    end;
        stTime:     begin
                      Result.IsNegative := False;
                      PInt64(@Result.Year)^ := 0;
                      PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
                      if Finteger_datetimes
                      then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                      else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                    end;
        stTimestamp:begin
                      Result.IsNegative := False;
                      PCardinal(@Result.TimeZoneHour)^ := 0;
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(P)^, Result.Year,
                        Result.Month, Result.Day, Result.Hour, Result.Minute,
                        Result.Second, Result.Fractions)
                      else PG2DateTime(PDouble(P)^, Result.Year,
                        Result.Month, Result.Day, Result.Hour, Result.Minute,
                        Result.Second, Result.Fractions);
                    end;
        stBoolean, stSmall,
        stInteger, stLong, stLongWord,
        stFloat, stDouble,
        stCurrency, stBigDecimal: DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    goto from_str;
        else raise CreatePGConvertError(ColumnIndex, FColOID);
      end
    else begin
from_str:
      Len := ZFastCode.StrLen(P);
      LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then begin
Fill:   Fillchar(Result, SizeOF(TZTimeStamp), #0);
      end;
    end;
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractPostgreSQLStringResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  P: PAnsiChar;
  Len: NativeUint;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  Result := nil;
  with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    if LastWasNull then
      if (ColumnOID = OIDOID) and FIsOidAsBlob
      then Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, 0, FChunk_Size)
      else Result := nil
    else case ColumnType of
      stBoolean, stSmall, stInteger, stLongWord, stLong,
      stDate, stTime, stTimestamp,
      stFloat, stDouble,
      stCurrency, stBigDecimal,
      stGUID,
      stString, stUnicodeString: begin
                                  P := GetPAnsiChar(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Len);
                                  Result := TZAbstractCLob.CreateWithData(P, Len, FClientCP, ConSettings);
                                end;
      stAsciiStream,
      stUnicodeStream:  begin
                          P := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
                          Result := TZAbstractCLob.CreateWithData(P, ZFastCode.StrLen(P), FClientCP, ConSettings);
                        end;
      stBytes,
      stBinaryStream: if (ColumnOID = OIDOID) and FIsOidAsBlob then
                        if FBinaryValues
                        then Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
                          PG2Cardinal(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex)), FChunk_Size)
                        else Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
                          RawToIntDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0), FChunk_Size)
                      else if FBinaryValues or (not FIs_bytea_output_hex and not Assigned(FPlainDriver.PQUnescapeBytea))
                        then Result := TZAbstractBlob.CreateWithData(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex),
                          FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex))
                        else if FIs_bytea_output_hex then
                          Result := TZPostgreSQLByteaHexBlob.Create(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex))
                        else
                          Result := TZPostgreSQLByteaEscapedBlob.Create(FPlainDriver, FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex))
    end;
  end;
end;

function TZAbstractPostgreSQLStringResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  if (Fres = nil) and (not Closed) and (RowNo=0)then
  begin
    Fres :=  FresAddress^;
    LastRowNo := FPlainDriver.PQntuples(Fres);
  end;
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
  begin
    if (ResultSetType = rtForwardOnly) then
      ClearPGResult;
    Exit;
  end;

  { Processes negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then
  begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end
    else
      Result := False;
    if not Result and (ResultSetType = rtForwardOnly) then
      ClearPGResult;
  end
  else
    RaiseForwardOnlyException;
end;

{ TZPostgreSQLOidBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a PostgreSQL connection reference.
}
constructor TZPostgreSQLOidBlob.Create(const PlainDriver: TZPostgreSQLPlainDriver;
  const Data: Pointer; const Size: Integer; const Handle: TPGconn;
  const BlobOid: Oid; const Chunk_Size: Integer);
begin
  inherited CreateWithData(Data, Size);
  FHandle := Handle;
  FBlobOid := BlobOid;
  FPlainDriver := PlainDriver;
  FChunk_Size := Chunk_Size;
end;

{**
  Gets the blob handle oid.
  @return the blob handle oid.
}
function TZPostgreSQLOidBlob.GetBlobOid: Oid;
begin
  Result := FBlobOid;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZPostgreSQLOidBlob.ReadLob;
var
  BlobHandle: Integer;
  Buffer: PAnsiChar;
  ReadNum: Integer;
  OffSet: Integer;
begin
  if not Updated and (FBlobOid > 0) then
  begin
    BlobHandle := FPlainDriver.lo_open(FHandle, FBlobOid, INV_READ);
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
      HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Read Large Object',nil);
    if BlobHandle >= 0 then
    begin
      Buffer := AllocMem(FChunk_Size+1);
      OffSet := 0;
      repeat
        ReadNum := FPlainDriver.lo_read(FHandle, BlobHandle,
          Buffer, FChunk_Size);
        Inc(OffSet, ReadNum);
        ReallocMem(FBlobData, OffSet);
        if ReadNum > 0 then
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(OffSet-ReadNum))^, ReadNum);
      until ReadNum < FChunk_Size;
      BlobSize := OffSet;
      FPlainDriver.lo_close(FHandle, BlobHandle);
      FreeMem(Buffer, FChunk_Size+1);
    end;
    inherited ReadLob; //don't forget this...
  end;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZPostgreSQLOidBlob.WriteLob;
begin
  WriteBuffer(BlobData, BlobSize);
end;

procedure TZPostgreSQLOidBlob.WriteBuffer(const Buffer: Pointer; const Len: integer);
var
  BlobHandle: Integer;
  Position: Integer;
  Size: Integer;
begin
  { Checks for empty blob. }
  if IsEmpty then
  begin
    FBlobOid := 0;
    Exit;
  end;

  { Creates a new large object. }
  if FBlobOid = 0 then begin
    FBlobOid := FPlainDriver.lo_creat(FHandle, INV_WRITE);
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
      HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Create Large Object',nil);
  end;

  { Opens and writes a large object. }
  BlobHandle := FPlainDriver.lo_open(FHandle, FBlobOid, INV_WRITE);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Open Large Object',nil);

  Position := 0;
  while Position < Len do
  begin
    if (Len - Position) < FChunk_Size then
      Size := Len - Position
    else
      Size := FChunk_Size;
    FPlainDriver.lo_write(FHandle, BlobHandle,
      {%H-}Pointer({%H-}NativeUInt(Buffer) + NativeUInt(Position)), Size);
      if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
        HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Write Large Object',nil);
    Inc(Position, Size);
  end;

  FPlainDriver.lo_close(FHandle, BlobHandle);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Close Large Object',nil);
end;
{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZPostgreSQLOidBlob.Clone(Empty: Boolean = False): IZBlob;
begin
  if Empty then
    Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
      FHandle, FBlobOid, FChunk_Size)
  else
    Result := TZPostgreSQLOidBlob.Create(FPlainDriver, BlobData, BlobSize,
      FHandle, FBlobOid, FChunk_Size);
end;

{ TZPostgreSQLByteaBlob }

constructor TZPostgreSQLByteaEscapedBlob.Create(const PlainDriver: TZPostgreSQLPlainDriver;
  Data: PAnsiChar);
var
  to_length: LongWord;
  pgBuffer: Pointer;
begin
  inherited CreateWithData(nil, 0);
  pgBuffer := PlainDriver.PQunescapeBytea(Data, @to_length);
  fBlobSize := to_length;
  if fBlobSize > 0 then begin
    System.GetMem(FBlobData, fBlobSize);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(pgBuffer^, FBlobData^, fBlobSize);
  end;
  PlainDriver.PQFreemem(pgBuffer);
end;

{ TZPostgreSQLByteaHexBlob }

constructor TZPostgreSQLByteaHexBlob.Create(Data: PAnsiChar);
begin
  inherited CreateWithData(nil, 0);
  {skip trailing /x}
  fBlobSize := (ZFastCode.StrLen(Data)-2) shr 1;
  if fBlobSize > 0 then begin
    System.GetMem(FBlobData, fBlobSize);
    HexToBin(Data+2, fBlobData, fBlobSize);
  end;
end;

{ TZPostgresResultSetMetadata }

procedure TZPostgresResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  //ColumnInfo.ColumnName := '';
end;

{**
  Initializes columns with additional data.
}
procedure TZPostgresResultSetMetadata.LoadColumns;
{$IFNDEF ZEOS_TEST_ONLY}
var
  Current: TZPGColumnInfo;
  I: Integer;
  PGMetaData: IZPGDatabaseMetadata;
  RS: IZResultSet;
{$ENDIF}
begin
  {$IFDEF ZEOS_TEST_ONLY}
  inherited LoadColumns;
  {$ELSE}
  if Metadata.GetConnection.GetDriver.GetStatementAnalyser.DefineSelectSchemaFromQuery(Metadata.GetConnection.GetDriver.GetTokenizer, SQL) <> nil then
    for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
      Current := TZPGColumnInfo(ResultSet.ColumnsInfo[i]);
      ClearColumn(Current);
      PGMetaData := MetaData as IZPGDatabaseMetadata;
      RS := PGMetaData.GetColumnsByTableOID(Current.TableOID);
      if RS <> nil then begin
        RS.BeforeFirst;
        while RS.Next do
          if RS.GetInt(TableColColumnOrdPosIndex) = Current.TableColNo then begin
            FillColumInfoFromGetColumnsRS(Current, RS, RS.GetString(ColumnNameIndex));
            Break;
          end else
            if RS.GetInt(TableColColumnOrdPosIndex) > Current.TableColNo then
              Break;
      end;
    end;
  Loaded := True;
  {$ENDIF}
end;

{ TZClientCursorPostgreSQLStringResultSet }

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZClientCursorPostgreSQLStringResultSet.MoveAbsolute(
  Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  if (Fres = nil) and (not Closed) and (RowNo=0)then
  begin
    Fres := FresAddress^;
    LastRowNo := FPlainDriver.PQntuples(Fres);
  end;
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
  begin
    if (ResultSetType = rtForwardOnly) then
      ClearPGResult;
    Exit;
  end;

  { Processes negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then
  begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end
    else
      Result := False;
    if not Result and (ResultSetType = rtForwardOnly) then
      ClearPGResult;
  end
  else
    RaiseForwardOnlyException;
end;

{**
  Opens this recordset.
}
procedure TZClientCursorPostgreSQLStringResultSet.Open;
begin
  if not Assigned(Fres) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  LastRowNo := FPlainDriver.PQntuples(Fres);
  inherited open;
end;

function TZClientCursorPostgreSQLStringResultSet.PGRowNo: Integer;
begin
  Result := RowNo-1;
end;

{ TZServerCursorPostgreSQLStringResultSet }

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZServerCursorPostgreSQLStringResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if ((MaxRows > 0) and (LastRowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;
  if RowNo = 0 then
    if not Assigned(Fres) then begin
      Fres := FresAddress^;
      if FPlainDriver.PQsetSingleRowMode(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
        HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcOther, 'open recordset', Fres);
    end else
      FplainDriver.PQclear(Fres)
end;

{**
  Opens this recordset.
}
procedure TZServerCursorPostgreSQLStringResultSet.Open;
begin
  if ResultSetType <> rtForwardOnly then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);
  if FPlainDriver.PQsetSingleRowMode(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
    HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcOther, 'open recordset', Fres);
  inherited Open;
end;

function TZServerCursorPostgreSQLStringResultSet.PGRowNo: Integer;
begin
  Result := 0;
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

{ TZPostgreSQLCachedResolverV74up }

procedure TZPostgreSQLCachedResolverV74up.FormWhereClause(
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Columns: TObjectList;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}SQLWriter: TZSQLStringWriter;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I: Integer;
  Tmp: SQLString;
begin
  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do
    with TZResolverParameter(WhereColumns[I]) do begin
      if I > 0 then
        SQLWriter.AddText(' AND ', Result);
      Tmp := IdentifierConvertor.Quote(ColumnName);
      SQLWriter.AddText(Tmp, Result);
      if (Metadata.IsNullable(ColumnIndex) = ntNullable)
      then SQLWriter.AddText(' IS NOT DISTINCT FROM ?', Result)
      else SQLWriter.AddText('=?', Result);
    end;
end;

{ TZPostgreSQLCachedResolverV8up }

procedure TZPostgreSQLCachedResolverV8up.SetResolverStatementParamters(
  const Statement: IZStatement;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TStrings);
begin
  inherited SetResolverStatementParamters(Statement, Params);
  Params.Values[ConnProps_BindDoublesAsString] := 'false';
  Params.Values[DSProps_EmulatePrepares] := 'false';
end;


{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.

