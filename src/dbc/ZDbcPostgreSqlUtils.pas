{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcPostgreSqlUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, fmtBCD,
  ZDbcIntfs, ZPlainPostgreSqlDriver, ZDbcPostgreSql, ZDbcLogging,
  ZCompatibility, ZVariant;

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const Connection: IZPostgreSQLConnection;
  const TypeName: string): TZSQLType; overload;

{**
    Another version of PostgreSQLToSQLType()
      - comparing integer should be faster than AnsiString?
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(OIDAsBlob: Boolean;
  TypeOid: OID; TypeModifier: Integer): TZSQLType; overload;

{**
   Return PostgreSQL type name from ZSQLType
   @param The ZSQLType type
   @return The Postgre TypeName
}
function SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: Boolean): string; overload;
procedure SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: Boolean; out aOID: OID); overload;

{**
  add by Perger -> based on SourceForge:
  [ 1520587 ] Fix for 1484704: bytea corrupted on post when not using utf8,
  file: 1484704.patch

  Converts a binary string into escape PostgreSQL format.
  @param Value a binary stream.
  @return a string in PostgreSQL binary string escape format.
}
function EncodeBinaryString(SrcBuffer: PAnsiChar; Len: Integer; Quoted: Boolean = False): RawByteString;

{**
  Encode string which probably consists of multi-byte characters.
  Characters ' (apostraphy), low value (value zero), and \ (back slash) are encoded. Since we have noticed that back slash is the second byte of some BIG5 characters (each of them is two bytes in length), we need a characterset aware encoding function.
  @param CharactersetCode the characterset in terms of enumerate code.
  @param Value the regular string.
  @return the encoded string.
}
function PGEscapeString(SrcBuffer: PAnsiChar; SrcLength: Integer;
    ConSettings: PZConSettings; Quoted: Boolean): RawByteString;

function PGSucceeded(ErrorMessage: PAnsiChar): Boolean; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(const Value: string): Word;

//https://www.postgresql.org/docs/9.1/static/datatype-datetime.html

//macros from datetime.c
function date2j(y, m, d: Integer): Integer;
procedure j2date(jd: Integer; out AYear, AMonth, ADay: Word);

procedure time2t(Hour, Min, Sec: Word; fsec: Cardinal; out MicroSeconds: Int64); overload;
procedure time2t(Hour, Min, Sec: Word; fsec: Cardinal; out MicroSeconds: Double); overload;


procedure dt2time(jd: Int64; out Hour, Min, Sec: Word; out MicroFractions: Cardinal); overload;
procedure dt2time(jd: Double; out Hour, Min, Sec: Word; out MicroFractions: Cardinal); overload;

procedure DateTime2PG(const Value: TDateTime; out Result: Int64); overload;
procedure DateTime2PG(const Value: TDateTime; out Result: Double); overload;

procedure TimeStamp2PG(const Value: TZTimeStamp; out MicroSeconds: Double); overload;
procedure TimeStamp2PG(const Value: TZTimeStamp; out MicroSeconds: Int64); overload;

procedure Date2PG(const Value: TDateTime; out Result: Integer); overload;
procedure Date2PG(Year, Month, Day: Word; out Result: Integer); overload;

procedure Time2PG(const Value: TDateTime; out Result: Int64); overload;
procedure Time2PG(const Hour, Min, Sec: Word; NanoFraction: Cardinal; out Result: Int64); overload;
procedure Time2PG(const Value: TDateTime; out Result: Double); overload;
procedure Time2PG(const Hour, Min, Sec: Word; NanoFraction: Cardinal; out Result: Double); overload;

function PG2DateTime(Value: Double; const TimeZoneOffset: Int64): TDateTime; overload;
procedure PG2DateTime(Value: Double; const TimeZoneOffset: Int64;
  out Year, Month, Day, Hour, Min, Sec: Word; out NanoFractions: Cardinal); overload;

function PG2DateTime(Value: Int64; const TimeZoneOffset: Int64): TDateTime; overload;
procedure PG2DateTime(Value: Int64; const TimeZoneOffset: Int64;
  out Year, Month, Day, Hour, Min, Sec: Word; out NanoFractions: Cardinal); overload;

function PG2Time(Value: Double): TDateTime; overload;
procedure PG2Time(Value: Double; Out Hour, Min, Sec: Word; out NanoFractions: Cardinal); overload;
function PG2Time(Value: Int64): TDateTime; overload;
procedure PG2Time(Value: Int64; Out Hour, Min, Sec: Word; out NanoFractions: Cardinal); overload;

function PG2Date(Value: Integer): TDateTime; overload;
procedure PG2Date(Value: Integer; out Year, Month, Day: Word); overload;

function PG2SmallInt(P: Pointer): SmallInt; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure SmallInt2PG(Value: SmallInt; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Integer(P: Pointer): Integer; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Integer2PG(Value: Integer; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Cardinal(P: Pointer): Cardinal; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Cardinal2PG(Value: Cardinal; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Int64(P: Pointer): Int64; {$IFNDEF WITH_C5242_OR_C4963_INTERNAL_ERROR} {$IFDEF WITH_INLINE}inline;{$ENDIF} {$ENDIF}
procedure Int642PG(const Value: Int64; Buf: Pointer); {$IFNDEF WITH_C5242_OR_C4963_INTERNAL_ERROR} {$IFDEF WITH_INLINE}inline;{$ENDIF} {$ENDIF}

{** written by EgonHugeist
  converts a postgres numeric into a native currency value
   @param P is a pointer to a valid numeric value
   @return a converted currency value
}
function PGNumeric2Currency(P: Pointer): Currency;

{** written by EgonHugeist
  converts a native currency value into a postgres numeric value
  the buffer must have a minimum of 4*SizeOf(Word) and maximum size of 9*SizeOf(Word) bytes
   @param Value the value which should be converted
   @param buf the numeric buffer we write into
   @param size return the number of bytes we finally used
}
procedure Currency2PGNumeric(const Value: Currency; Buf: Pointer; out Size: Integer);

{** written by EgonHugeist
  converts a native currency value into a postgres numeric value
  the buffer must have a minimum of 4*SizeOf(Word) and maximum size of 9*SizeOf(Word) bytes
   @param Value the value which should be converted
   @param buf the numeric buffer we write into
   @param size return the number of bytes we finally used
}
procedure BCD2PGNumeric(const Src: TBCD; Dst: PAnsiChar; out Size: Integer);

{** written by EgonHugeist
  converts a postgres numeric value into a bigdecimal value
  the buffer must have a minimum of 4*SizeOf(Word) and maximum size of 9*SizeOf(Word) bytes
  @param Src the pointer to a postgres numeric value
  @param Dst the result value to be converted
}
procedure PGNumeric2BCD(Src: PAnsiChar; var Dst: TBCD);

function PGCash2Currency(P: Pointer): Currency; {$IFNDEF WITH_C5242_OR_C4963_INTERNAL_ERROR} {$IFDEF WITH_INLINE}inline;{$ENDIF} {$ENDIF}
procedure Currency2PGCash(const Value: Currency; Buf: Pointer); {$IFNDEF WITH_C5242_OR_C4963_INTERNAL_ERROR} {$IFDEF WITH_INLINE}inline;{$ENDIF} {$ENDIF}

function PG2Single(P: Pointer): Single; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Single2PG(Value: Single; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Double(P: Pointer): Double; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Double2PG(const Value: Double; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PGMacAddr2Raw(Src, Dest: PAnsiChar): LengthInt;
function PGInetAddr2Raw(Src, Dest: PAnsiChar): LengthInt;

function PGMacAddr2Uni(Src: PAnsiChar; Dest: PWideChar): LengthInt;
function PGInetAddr2Uni(Src: PAnsiChar; Dest: PWideChar): LengthInt;

{$IFNDEF ENDIAN_BIG}
procedure Reverse4Bytes(P: Pointer); {$IF defined (FPC) and defined(INTEL_ASM)}assembler;{$IFEND}{$IF defined(WITH_INLINE) and (defined(FPC) or not defined(MSWINDOWS))}inline;{$IFEND}
procedure Reverse8Bytes(P: Pointer); {$IF defined (FPC) and defined(INTEL_ASM)}assembler;{$IFEND}{$IF defined(WITH_INLINE) and (defined(FPC) or not defined(MSWINDOWS))}inline;{$IFEND}
{$ENDIF}

//ported macros from array.h
function ARR_NDIM(a: PArrayType): PInteger;
function ARR_HASNULL(a: PArrayType): Boolean;
function ARR_ELEMTYPE(a: PArrayType): POID;
function ARR_DIMS(a: PArrayType): PInteger;
function ARR_LBOUND(a: PArrayType): PInteger;
function ARR_OVERHEAD_NONULLS(ndims: Integer): Integer;
function ARR_DATA_OFFSET(a: PArrayType): Int32;
function ARR_DATA_PTR(a: PArrayType): Pointer;

const
  MinPGNumSize = (1{ndigits}+1{weight}+1{sign}+1{dscale})*SizeOf(Word);
  MaxCurr2NumSize = MinPGNumSize+(5{max 5 NBASE ndigits}*SizeOf(Word));
  MaxBCD2NumSize  = MinPGNumSize+(MaxFMTBcdFractionSize div 4{max 16 NBASE ndigits}*SizeOf(Word));
  ParamFormatBin = 1;
  ParamFormatStr = 0;

const ZSQLType2PGBindSizes: array[stUnknown..stGUID] of Integer = (-1,
    SizeOf(Byte){stBoolean},
    SizeOf(SmallInt){stByte}, SizeOf(SmallInt){stShort}, SizeOf(Integer){stWord},
    SizeOf(SmallInt){stSmall}, SizeOf(Cardinal){stLongWord}, SizeOf(Integer){stInteger}, SizeOf(Int64){stULong}, SizeOf(Int64){stLong},  //ordinals
    SizeOf(Single){stFloat}, SizeOf(Double){stDouble}, MaxCurr2NumSize{stCurrency}, MaxBCD2NumSize{stBigDecimal}, //floats
    SizeOf(Integer){stDate}, 8{stTime}, 8{stTimestamp},
    SizeOf(TGUID){stGUID});

const ZSQLType2OID: array[Boolean, stUnknown..stBinaryStream] of OID = (
  (INVALIDOID, BOOLOID,
    INT2OID, INT2OID, INT4OID, INT2OID, INT8OID, INT4OID, INT8OID, INT8OID,  //ordinals
    FLOAT4OID, FLOAT8OID, NUMERICOID, NUMERICOID, //floats
    DATEOID, TIMEOID, TIMESTAMPOID, UUIDOID,
    //now varying size types in equal order
    VARCHAROID, VARCHAROID, BYTEAOID,
    TEXTOID, TEXTOID, BYTEAOID),
  (INVALIDOID, BOOLOID,
    INT2OID, INT2OID, INT4OID, INT2OID, OIDOID, INT4OID, INT8OID, INT8OID,  //ordinals
    FLOAT4OID, FLOAT8OID, CASHOID, NUMERICOID, //floats
    DATEOID, TIMEOID, TIMESTAMPOID, UUIDOID,
    //now varying size types in equal order
    VARCHAROID, VARCHAROID, BYTEAOID,
    TEXTOID, TEXTOID, OIDOID));

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses Math, ZFastCode, ZMessages, ZSysUtils, ZDbcUtils
  {$IFDEF WITH_SBCDOVERFLOW}, DBConsts{$ENDIF};

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const Connection: IZPostgreSQLConnection;
  const TypeName: string): TZSQLType;
var
  TypeNameLo: string;
  P: PChar absolute TypeNameLo;
begin
  TypeNameLo := LowerCase(TypeName);
  if (TypeNameLo = 'interval') or (TypeNameLo = 'char') or (TypeNameLo = 'bpchar')
    or (TypeNameLo = 'varchar') or (TypeNameLo = 'bit') or (TypeNameLo = 'varbit')
  then Result := stString
  else if (TypeNameLo = 'text') or (TypeNameLo = 'citext') then
    Result := stAsciiStream
  else if TypeNameLo = 'oid' then
  begin
    if Connection.IsOidAsBlob() then
      Result := stBinaryStream
    else
      Result := stLongWord;
  end
  else if TypeNameLo = 'name' then
    Result := stString
  else if TypeNameLo = 'enum' then
    Result := stString
  else if TypeNameLo = 'cidr' then
    Result := stString
  else if TypeNameLo = 'inet' then
    Result := stString
  else if TypeNameLo = 'macaddr' then
    Result := stString
  else if TypeNameLo = 'int2' then
    Result := stSmall
  else if TypeNameLo = 'int4' then
    Result := stInteger
  else if TypeNameLo = 'int8' then
    Result := stLong
  else if TypeNameLo = 'float4' then
    Result := stFloat
  else if (TypeNameLo = 'float8') then
    Result := stDouble
  else if (TypeNameLo = 'decimal') or (TypeNameLo = 'numeric') then
    Result := stBigDecimal
  else if TypeNameLo = 'money' then
    Result := stCurrency
  else if TypeNameLo = 'bool' then
    Result := stBoolean
  else if TypeNameLo = 'date' then
    Result := stDate
  else if TypeNameLo = 'time' then
    Result := stTime
  else if (TypeNameLo = 'datetime') or (TypeNameLo = 'timestamp')
    or (TypeNameLo = 'timestamptz') or (TypeNameLo = 'abstime') then
    Result := stTimestamp
  else if TypeNameLo = 'regproc' then
    Result := stString
  else if TypeNameLo = 'bytea' then
  begin
    if Connection.IsOidAsBlob then
      Result := stBytes
    else
      Result := stBinaryStream;
  end
  else if (TypeNameLo = 'int2vector') or (TypeNameLo = 'oidvector') then
    Result := stAsciiStream
  else if (TypeNameLo <> '') and (P^ = '_') then // ARRAY TYPES
    Result := stAsciiStream
  else if (TypeNameLo = 'uuid') then
    Result := stGuid
  else if StartsWith(TypeNameLo, 'json') then
    Result := stAsciiStream
  else if StartsWith(TypeNameLo, 'xml') then
    Result := stAsciiStream
  else
    Result := stUnknown;
end;

{**
   Another version of PostgreSQLToSQLType()
     - comparing integer should be faster than AnsiString.
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(OIDAsBlob: Boolean; TypeOid: OID;
  TypeModifier: Integer): TZSQLType; overload;
var Scale: Integer;
begin
  case TypeOid of
    INTERVALOID, CHAROID, BPCHAROID, VARCHAROID:  { interval/char/bpchar/varchar }
      Result := stString;
    TEXTOID: Result := stAsciiStream; { text }
    OIDOID: if OidAsBlob
            then Result := stBinaryStream
            else Result := stLongWord;
    NAMEOID: Result := stString; { name }
    INT2OID: Result := stSmall; { int2 }
    INT4OID: Result := stInteger; { int4 }
    INT8OID: Result := stLong; { int8 }
    CIDROID: Result := stString; { cidr }
    INETOID: Result := stString; { inet }
    MACADDROID: Result := stString; { macaddr }
    FLOAT4OID: Result := stFloat; { float4 }
    FLOAT8OID: Result := stDouble; { float8/numeric. no 'decimal' any more }
    NUMERICOID: begin
      Result := stBigDecimal;
      //see: https://www.postgresql.org/message-id/slrnd6hnhn.27a.andrew%2Bnonews%40trinity.supernews.net
      //macro:
      //numeric: this is ugly, the typmod is ((prec << 16) | scale) + VARHDRSZ,
      //i.e. numeric(10,2) is ((10 << 16) | 2) + 4
        if TypeModifier <> -1 then begin
          Scale := (TypeModifier - VARHDRSZ) and $FFFF;
          if (Scale <= 4) and ((TypeModifier - VARHDRSZ) shr 16 and $FFFF < sAlignCurrencyScale2Precision[Scale]) then
            Result := stCurrency
        end;
      end;
    CASHOID: Result := stCurrency; { money }
    BOOLOID: Result := stBoolean; { bool }
    DATEOID: Result := stDate; { date }
    TIMEOID: Result := stTime; { time }
    TIMESTAMPOID, TIMESTAMPTZOID, ABSTIMEOID: Result := stTimestamp; { timestamp,timestamptz/abstime. no 'datetime' any more}
    BITOID, VARBITOID: Result := stString; {bit/ bit varying string}
    REGPROCOID: Result := stString; { regproc }
    1034: Result := stAsciiStream; {aclitem[]}
    BYTEAOID: if TypeModifier >= VARHDRSZ
      then Result := stBytes
      else Result := stBinaryStream;
    UUIDOID: Result := stGUID; {uuid}
    JSONOID, JSONBOID: Result := {$IFDEF ZEOS90UP}stJSON{$ELSE}stAsciiStream{$ENDIF};
    XMLOID: Result := {$IFDEF ZEOS90UP}stXML{$ELSE}stAsciiStream{$ENDIF};
    INT2VECTOROID, OIDVECTOROID: Result := stAsciiStream; { int2vector/oidvector. no '_aclitem' }
    143,629,651,719,791,1000..OIDARRAYOID,1040,1041,1115,1182,1183,1185,1187,1231,1263,
    1270,1561,1563,2201,2207..2211,2949,2951,3643,3644,3645,3735,3770 : { other array types }
      Result := stAsciiStream;
    else
      Result := stUnknown;
  end;
end;

function SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: boolean): string;
begin
  case SQLType of
    stBoolean: Result := 'bool';
    stByte, stSmall, stInteger, stLong: Result := 'int';
    stFloat: Result := 'float4';
    stDouble: Result := 'float8';
    stCurrency, stBigDecimal: Result := 'numeric';
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: Result := 'text';
    stDate: Result := 'date';
    stTime: Result := 'time';
    stTimestamp: Result := 'timestamp';
    stGuid: Result := 'uuid';
    stBinaryStream, stBytes:
      if IsOidAsBlob then
        Result := 'oid'
      else
        Result := 'bytea';
    else Result := '';
  end;
end;

procedure SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: Boolean; out aOID: OID);
begin
  case SQLType of
    stBoolean: aOID := BOOLOID;
    stByte, stShort, stSmall: aOID := INT2OID;
    stWord, stInteger: aOID := INT4OID;
    stLongWord, stLong, stULong: aOID := INT8OID;
    stFloat: aOID := FLOAT4OID;
    stDouble: aOID := FLOAT8OID;
    stBigDecimal, stCurrency: aOID := NUMERICOID;//CASHOID;  the pg money has a scale of 2 while we've a scale of 4
    stString, stUnicodeString,//: aOID := VARCHAROID;
    stAsciiStream, stUnicodeStream: aOID := TEXTOID;
    stDate: aOID := DATEOID;
    stTime: aOID := TIMEOID;
    stTimestamp: aOID := TIMESTAMPOID;
    stGuid: aOID := UUIDOID;
    stBytes: aOID := BYTEAOID;
    stBinaryStream:
      if IsOidAsBlob
      then aOID := OIDOID
      else aOID := BYTEAOID;
    else {stUnknown, stArray} aOID := INVALIDOID;
  end;
end;

{**
  Encode string which probably consists of multi-byte characters.
  Characters ' (apostraphy), low value (value zero), and \ (back slash) are encoded.
  Since we have noticed that back slash is the second byte of some BIG5 characters
    (each of them is two bytes in length), we need a characterset aware encoding function.
  @param CharactersetCode the characterset in terms of enumerate code.
  @param Value the regular string.
  @return the encoded string.
}
function PGEscapeString(SrcBuffer: PAnsiChar; SrcLength: Integer;
    ConSettings: PZConSettings; Quoted: Boolean): RawByteString;
var
  I, LastState: Integer;
  DestLength: Integer;
  DestBuffer: PAnsiChar;

  function pg_CS_stat(stat: integer; character: integer;
          CharactersetCode: TZPgCharactersetType): integer;
  begin
    if character = 0 then
      stat := 0;

    case CharactersetCode of
      csUTF8, csUNICODE_PODBC:
        begin
          if (stat < 2) and (character >= $80) then
          begin
            if character >= $fc then
              stat := 6
            else if character >= $f8 then
              stat := 5
            else if character >= $f0 then
              stat := 4
            else if character >= $e0 then
              stat := 3
            else if character >= $c0 then
              stat := 2;
          end
          else
            if (stat > 2) and (character > $7f) then
              Dec(stat)
            else
              stat := 0;
        end;
  { Shift-JIS Support. }
      csSJIS:
        begin
      if (stat < 2)
        and (character > $80)
        and not ((character > $9f) and (character < $e0)) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
  { Chinese Big5 Support. }
      csBIG5:
        begin
      if (stat < 2) and (character > $A0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
  { Chinese GBK Support. }
      csGBK:
        begin
      if (stat < 2) and (character > $7F) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { Korian UHC Support. }
      csUHC:
        begin
      if (stat < 2) and (character > $7F) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { EUC_JP Support }
      csEUC_JP:
        begin
      if (stat < 3) and (character = $8f) then { JIS X 0212 }
        stat := 3
      else
      if (stat <> 2)
        and ((character = $8e) or
        (character > $a0)) then { Half Katakana HighByte & Kanji HighByte }
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { EUC_CN, EUC_KR, JOHAB Support }
      csEUC_CN, csEUC_KR, csJOHAB:
        begin
      if (stat < 2) and (character > $a0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
      csEUC_TW:
        begin
      if (stat < 4) and (character = $8e) then
        stat := 4
      else if (stat = 4) and (character > $a0) then
        stat := 3
      else if ((stat = 3) or (stat < 2)) and (character > $a0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
        { Chinese GB18030 support.Added by Bill Huang <bhuang@redhat.com> <bill_huanghb@ybb.ne.jp> }
      csGB18030:
        begin
      if (stat < 2) and (character > $80) then
        stat := 2
      else if stat = 2 then
      begin
        if (character >= $30) and (character <= $39) then
          stat := 3
        else
          stat := 1;
      end
      else if stat = 3 then
      begin
        if (character >= $30) and (character <= $39) then
          stat := 1
        else
          stat := 3;
      end
      else
        stat := 0;
        end;
      else
      stat := 0;
    end;
    Result := stat;
  end;

begin
  DestBuffer := SrcBuffer; //safe entry
  DestLength := Ord(Quoted) shl 1;
  LastState := 0;
  for I := 1 to SrcLength do
  begin
    LastState := pg_CS_stat(LastState,integer(SrcBuffer^),
      TZPgCharactersetType(ConSettings.ClientCodePage.ID));
    if (PByte(SrcBuffer)^ in [Ord(#0), Ord(#39)]) or ((PByte(SrcBuffer)^ = Ord('\')) and (LastState = 0))
    then Inc(DestLength, 4)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;

  SrcBuffer := DestBuffer; //restore entry
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);
  if Quoted then begin
    PByte(DestBuffer)^ := Ord(#39);
    Inc(DestBuffer);
  end;

  LastState := 0;
  for I := 1 to SrcLength do begin
    LastState := pg_CS_stat(LastState,integer(SrcBuffer^),
      TZPgCharactersetType(ConSettings.ClientCodePage.ID));
    if (PByte(SrcBuffer)^ in [Ord(#0), Ord(#39)]) or ((PByte(SrcBuffer)^ = Ord('\')) and (LastState = 0)) then begin
      PByte(DestBuffer)^ := Ord('\');
      PByte(DestBuffer+1)^ := Ord('0') + (Byte(SrcBuffer^) shr 6);
      PByte(DestBuffer+2)^ := Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07);
      PByte(DestBuffer+3)^ := Ord('0') + (Byte(SrcBuffer^) and $07);
      Inc(DestBuffer, 4);
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
  if Quoted then
    PByte(DestBuffer)^ := Ord(#39);
end;


{**
  add by Perger -> based on SourceForge:
  [ 1520587 ] Fix for 1484704: bytea corrupted on post when not using utf8,
  file: 1484704.patch

  Converts a binary string into escape PostgreSQL format.
  @param Value a binary stream.
  @return a string in PostgreSQL binary string escape format.
}
function EncodeBinaryString(SrcBuffer: PAnsiChar; Len: Integer; Quoted: Boolean = False): RawByteString;
var
  I: Integer;
  DestLength: Integer;
  DestBuffer: PAnsiChar;
begin
  DestBuffer := SrcBuffer; //save entry
  DestLength := Ord(Quoted) shl 1;
  for I := 1 to Len do
  begin
    if (Byte(SrcBuffer^) < 32) or (Byte(SrcBuffer^) > 126) or (PByte(SrcBuffer)^ in [Ord(#39), Ord('\')])
    then Inc(DestLength, 5)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;
  SrcBuffer := DestBuffer; //restore

  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);
  if Quoted then begin
    PByte(DestBuffer)^ := Ord(#39);
    Inc(DestBuffer);
  end;

  for I := 1 to Len do begin
    if (Byte(SrcBuffer^) < 32) or (Byte(SrcBuffer^) > 126) or (PByte(SrcBuffer)^ in [Ord(#39), Ord('\')]) then begin
      PByte(DestBuffer)^ := Ord('\');
      PByte(DestBuffer+1)^ := Ord('\');
      PByte(DestBuffer+2)^ := Ord('0') + (Byte(SrcBuffer^) shr 6);
      PByte(DestBuffer+3)^ := Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07);
      PByte(DestBuffer+4)^ := Ord('0') + (Byte(SrcBuffer^) and $07);
      Inc(DestBuffer, 5);
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
  if Quoted then
    DestBuffer^ := '''';
end;

function PGSucceeded(ErrorMessage: PAnsiChar): Boolean;
begin
  Result := (ErrorMessage = nil) or (ErrorMessage^ = #0);
end;

{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(const Value: string): Word;
var Buf: array[0..20] of Char;
  P, PEnd, PBuf: PChar;
begin
  P := Pointer(Value);
  PEnd := P + Length(Value);
  PBuf := @Buf[0];
  while P < PEnd do
    if (Ord(P^) in [Ord('0')..Ord('9')]) then begin
      PBuf^:= P^;
      Inc(P);
      Inc(PBuf);
    end else
      Break;
  PBuf^ := #0;
  {$IFDEF UNICODE}
  Result := UnicodeToIntDef(PWideChar(@Buf[0]), 0);
  {$ELSE}
  Result := RawToIntDef(PAnsiChar(@Buf[0]), 0);
  {$ENDIF}
end;

function date2j(y, m, d: Integer): Integer;
var
  julian: Integer;
  century: Integer;
begin
  if (m > 2) then begin
    m := m+1;
    y := y+4800;
  end else begin
    m := M + 13;
    y := y + 4799;
  end;

  century := y div 100;
  julian := y * 365 - 32167;
  julian := julian + y div 4 - century + century div 4;
  Result := julian + 7834 * m div 256 + d;
end;

{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "Int64" before doing the multiply could prevent overflow errors} {$ENDIF}
procedure j2date(jd: Integer; out AYear, AMonth, ADay: Word);
var
  julian, quad, extra: Cardinal;
  y: Integer;
begin
  julian := jd;
  julian := julian + 32044;
  quad := julian div 146097;
  extra := (julian - quad * 146097) * 4 + 3;
  julian := julian + 60 + quad * 3 + extra div 146097;
  quad := julian div 1461;
  julian := julian - quad * 1461;
  y := julian * 4 div 1461;
  if y <> 0 then
    julian := (julian + 305) mod 365
  else
    julian := (julian + 306) mod 366;
  julian := julian + 123;
  y := y + Integer(quad * 4);
  AYear := y - 4800;
  quad := julian * 2141 div 65536;
  ADay := julian - 7834 * quad div 256;
  AMonth := (quad + 10) mod 12{MONTHS_PER_YEAR} + 1;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors} {$ENDIF}
procedure time2t(Hour, Min, Sec: Word; fsec: Cardinal; out MicroSeconds: Int64);
begin
  MicroSeconds := (((((Hour * MINS_PER_HOUR) + Min) * SECS_PER_MINUTE) + Sec) * USECS_PER_SEC) + fsec;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors} {$ENDIF}
procedure time2t(Hour, Min, Sec: Word; fsec: Cardinal; out MicroSeconds: Double);
begin
  MicroSeconds := (((((Hour * MINS_PER_HOUR) + Min) * SECS_PER_MINUTE) + Sec) * USECS_PER_SEC) + fsec;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFNDEF ENDIAN_BIG}
procedure Reverse4Bytes(P: Pointer); {$IF defined(FPC) and defined(INTEL_ASM)}nostackframe; assembler;{$IFEND}
{$IFDEF INTEL_ASM}
asm
  {$IFNDEF CPU64}
  mov edx, [eax]
  {$ELSE !CPU64}
  mov edx, [rcx]
  {$ENDIF CPU64}
  bswap edx
  {$IFNDEF CPU64}
  mov [eax], edx
  {$ELSE !CPU64}
  mov [rcx], edx
  {$ENDIF CPU64}
end;
{$ELSE INTEL_ASM}
var C: Cardinal;
begin
  {$R-}
  C := ((PCardinal(P)^ shl 8) and $FF00FF00) or ((PCardinal(P)^ shr 8) and $00FF00FF);
  C :=  (C shl 16) or (C shr 16);
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  PCardinal(P)^ := C;
end;
{$ENDIF INTEL_ASM}
{$ENDIF ENDIAN_BIG}


{$IFNDEF ENDIAN_BIG}
procedure Reverse8Bytes(P: Pointer); {$IF defined(FPC) and defined(INTEL_ASM)}nostackframe; assembler;{$IFEND}
{$IFDEF INTEL_ASM}
asm
  {$IFNDEF CPU64}
  mov edx, [eax]
  mov ecx, [eax+$04]
  bswap edx
  bswap ecx
  mov [eax], ecx;
  mov [eax+$04], edx;
  {$ELSE CPU64}
  mov rdx, [rcx]
  bswap rdx
  mov [rcx], rdx;
  {$ENDIF CPU64}
end;
{$ELSE INTEL_ASM}
{$IFNDEF CPU64}
var C1, C2: Cardinal;
{$ELSE}
var u64: UInt64;
{$ENDIF}
begin
  {$R-}
  {$IFNDEF CPU64}
  C1 := ((PCardinal(          P   )^ shl 8) and $FF00FF00) or ((PCardinal(          P   )^ shr 8) and $00FF00FF);
  C1 :=  (C1 shl 16) or (C1 shr 16);
  C2 := ((PCardinal(PAnsiChar(P)+4)^ shl 8) and $FF00FF00) or ((PCardinal(PAnsiChar(P)+4)^ shr 8) and $00FF00FF);
  C2 :=  (C2 shl 16) or (C2 shr 16);
  PCardinal(          P   )^ := C2;
  PCardinal(PAnsiChar(P)+4)^ := C1;
  {$ELSE}
  u64 := ((PUInt64(p)^ shl 8 ) and $FF00FF00FF00FF00) or ((PUInt64(p)^ shr 8 ) and $00FF00FF00FF00FF);
  u64 := ((u64 shl 16) and $FFFF0000FFFF0000) or ((u64 shr 16) and $0000FFFF0000FFFF);
  u64 :=  (u64 shl 32) or ((u64 shr 32));
  PUInt64(p)^ := u64;
  {$ENDIF}
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;
{$ENDIF INTEL_ASM}
{$ENDIF}

{$IFDEF FPC} {$PUSH}
  {$WARN 4079 off : Converting the operands to "Int64" before doing the add could prevent overflow errors}
  {$WARN 4080 off : Converting the operands to "Int64" before doing the substract could prevent overflow errors}
{$ENDIF}
procedure DateTime2PG(const Value: TDateTime; out Result: Int64);
var Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Date: Int64; //overflow save multiply
begin
  DecodeDate(Value, Year, Month, Day);
  Date := date2j(Year, Month, Day) - POSTGRES_EPOCH_JDATE;
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //timestamps do not play with microseconds!!
  Result := ((Hour * MINS_PER_HOUR + Min) * SECS_PER_MINUTE + Sec) * MSecsPerSec + MSec;
  Result := (Date * MSecsPerDay + Result) * MSecsPerSec;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure DateTime2PG(const Value: TDateTime; out Result: Double);
var Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Date: Double; //overflow save multiply
begin
  DecodeDate(Value, Year, Month, Day);
  Date := date2j(Year, Month, Day) - POSTGRES_EPOCH_JDATE;
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := (Hour * MinsPerHour + Min) * SecsPerMin + Sec + Msec / MSecsPerSec;
  Result := Date * SECS_PER_DAY + Result;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

function PG2DateTime(Value: Double; const TimeZoneOffset: Int64): TDateTime;
var date: TDateTime;
  Year, Month, Day, Hour, Min, Sec: Word;
  fsec: Cardinal;
begin
  PG2DateTime(Value, TimeZoneOffset, Year, Month, Day, Hour, Min, Sec, fsec);
  TryEncodeDate(Year, Month, Day, date);
  dt2time(Value, Hour, Min, Sec, fsec);
  TryEncodeTime(Hour, Min, Sec, fsec, Result);
  if date < 0
  then Result := date - Result
  else Result := date + Result;
end;

procedure TimeStamp2PG(const Value: TZTimeStamp; out MicroSeconds: Double);
var Date: Double; //overflow save multiply
begin
  Date := date2j(Value.Year, Value.Month, Value.Day) - POSTGRES_EPOCH_JDATE;
  time2t(Value.Hour, Value.Minute, Value.Second, Value.Fractions div 1000, MicroSeconds);
  MicroSeconds := Date * SECS_PER_DAY + MicroSeconds;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@MicroSeconds);
  {$ENDIF}
end;

{$IFDEF FPC} {$PUSH}{$WARN 4080 off : Converting the operands to "Int64" before doing the substract could prevent overflow errors} {$ENDIF}
procedure TimeStamp2PG(const Value: TZTimeStamp; out MicroSeconds: Int64);
var Date: Int64; //overflow save multiply
begin
  Date := date2j(Value.Year, Value.Month, Value.Day) - POSTGRES_EPOCH_JDATE;
  time2t(Value.Hour, Value.Minute, Value.Second, Value.Fractions div 1000, MicroSeconds);
  Date := (Date * MSecsPerDay);//allign to millisecond resolution
  Date := Date*MicroSecsOfMilliSecond; //align to microseconds
  MicroSeconds := Date + MicroSeconds;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@MicroSeconds);
  {$ENDIF}
end;

procedure PG2DateTime(value: Double; const TimeZoneOffset: Int64;
  out Year, Month, Day, Hour, Min, Sec: Word; out NanoFractions: Cardinal);
var
  date: Double;
  time: Double;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  time := value + TimeZoneOffset;
  if Time < 0
  then date := Ceil(time / SecsPerDay)
  else date := Floor(time / SecsPerDay);
  if date <> 0 then
    Time := Time - Round(date * SecsPerDay);
  if Time < 0 then begin
    Time := Time + SecsPerDay;
    date := date - 1;
  end;
  date := date + POSTGRES_EPOCH_JDATE;
  j2date(Integer(Trunc(date)), Year, Month, Day);
  dt2time(Time, Hour, Min, Sec, NanoFractions);
  NanoFractions := NanoFractions * 1000;
end;

function PG2DateTime(Value: Int64; const TimeZoneOffset: Int64): TDateTime;
var d: TDateTime;
  date: Int64;
  Year, Month, Day, Hour, Min, Sec: Word;
  MicroF: Cardinal;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  if TimeZoneOffset <> 0 then
    Value := Value + TimeZoneOffset;
  date := Value div USECS_PER_DAY;
  Value := Value mod USECS_PER_DAY;
  if Value < 0 then begin
    Value := Value + USECS_PER_DAY;
    date := date - 1;
  end;
  date := date + POSTGRES_EPOCH_JDATE;
  j2date(date, Year, Month, Day);
  if not TryEncodeDate(Year, Month, Day, d) then
    D := 0;
  dt2time(Value, Hour, Min, Sec, MicroF);
  if not TryEncodeTime(Hour, Min, Sec, MicroF div 1000, Result) then
    Result := 0;
  if d < 0
  then Result := d - Result
  else Result := d + Result;
end;

procedure PG2DateTime(Value: Int64; const TimeZoneOffset: Int64;
  out Year, Month, Day, Hour, Min, Sec: Word; out NanoFractions: Cardinal);
var date: Int64;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  if TimeZoneOffset <> 0 then
    Value := Value + TimeZoneOffset;
  date := Value div USECS_PER_DAY;
  Value := Value - (date * USECS_PER_DAY);
  if Value < 0 then begin
    Value := Value + USECS_PER_DAY;
    date := date - 1;
  end;
  date := date + POSTGRES_EPOCH_JDATE;
  j2date(date, Year, Month, Day);
  dt2time(Value, Hour, Min, Sec, NanoFractions);
  NanoFractions := NanoFractions * 1000;
end;

procedure dt2time(jd: Int64; out Hour, Min, Sec: Word; out MicroFractions: Cardinal);
begin
  Hour := jd div USECS_PER_HOUR;
  jd := jd - Int64(Hour) * Int64(USECS_PER_HOUR);
  Min := jd div USECS_PER_MINUTE;
  jd := jd - Int64(Min) * Int64(USECS_PER_MINUTE);
  Sec := jd div USECS_PER_SEC;
  MicroFractions := jd - (Int64(Sec) * Int64(USECS_PER_SEC));
end;

procedure dt2time(jd: Double; out Hour, Min, Sec: Word; out MicroFractions: Cardinal);
begin
  Hour := Trunc(jd / SECS_PER_HOUR);
  jd := jd - Hour * SECS_PER_HOUR;
  Min := Trunc(jd / SECS_PER_MINUTE);
  jd := jd - Min * SECS_PER_MINUTE;
  Sec := Trunc(jd);
  MicroFractions := Trunc((jd - Sec)) * Int64(USECS_PER_SEC);
end;

procedure Time2PG(const Value: TDateTime; out Result: Int64);
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  time2t(Hour, Min, Sec, MSec * 1000, Result);
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

procedure Time2PG(const Hour, Min, Sec: Word; NanoFraction: Cardinal; out Result: Int64);
begin
  time2t(Hour, Min, Sec, NanoFraction div 1000, Result);
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

procedure Time2PG(const Value: TDateTime; out Result: Double);
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //macro of datetime.c
  time2t(Hour, Min, Sec, MSec*1000, Result);
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

procedure Time2PG(const Hour, Min, Sec: Word; NanoFraction: Cardinal; out Result: Double); overload;
begin
  //macro of datetime.c
  time2t(Hour, Min, Sec, NanoFraction div 1000, Result);
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

procedure PG2Time(Value: Double; Out Hour, Min, Sec: Word;
  out NanoFractions: Cardinal); overload;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, NanoFractions);
  NanoFractions := NanoFractions * 1000;
end;

function PG2Time(Value: Double): TDateTime;
var Hour, Min, Sec: Word; fMicro: Cardinal;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, fMicro);
  if not TryEncodeTime(Hour, Min, Sec, fMicro div 1000, Result) then
    Result := 0;
end;

procedure PG2Time(Value: Int64; Out Hour, Min, Sec: Word;
  out NanoFractions: Cardinal);
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, NanoFractions);
  NanoFractions := NanoFractions * 1000;
end;

function PG2Time(Value: Int64): TDateTime;
var Hour, Min, Sec: Word; fMicro: Cardinal;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, fMicro);
  if not TryEncodeTime(Hour, Min, Sec, fMicro div 1000, Result) then
    Result := 0;
end;

procedure Date2PG(const Value: TDateTime; out Result: Integer);
var y,m,d: Word;
begin
  DecodeDate(Value, y,m,d);
  Date2PG(y,m,d, Result);
end;

procedure Date2PG(Year, Month, Day: Word; out Result: Integer);
begin
  Result := date2j(Year,Month,Day) - POSTGRES_EPOCH_JDATE;
  {$IFNDEF ENDIAN_BIG}
  Reverse4Bytes(@Result);
  {$ENDIF}
end;

function PG2Date(Value: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse4Bytes(@Value);
  {$ENDIF}
  j2date(Value+POSTGRES_EPOCH_JDATE, Year, Month, Day);
  if not TryEncodeDate(Year, Month, Day, Result) then
    Result := 0;
end;

procedure PG2Date(Value: Integer; out Year, Month, Day: Word);
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse4Bytes(@Value);
  {$ENDIF}
  j2date(Value+POSTGRES_EPOCH_JDATE, Year, Month, Day);
end;

function PG2SmallInt(P: Pointer): SmallInt;
begin
{$IFNDEF ENDIAN_BIG}
  Word(Result) := ((PWord(P)^ and $00FF) shl 8) or ((PWord(P)^ and $FF00) shr 8);
{$ELSE}
  Result := PSmallInt(P)^;
{$ENDIF}
end;

procedure SmallInt2PG(Value: SmallInt; Buf: Pointer);
begin
{$IFNDEF ENDIAN_BIG}
  PWord(Buf)^ := ((Word(Value) and $00FF) shl 8) or ((Word(Value) and $FF00) shr 8);
{$ELSE}
  PSmallInt(Buf)^ := Value;
{$ENDIF}
end;

function PG2Integer(P: Pointer): Integer;
{$IFNDEF ENDIAN_BIG}
var C: Cardinal absolute Result;
begin
  {$IFOPT R+}
  C :=  ((PCardinal(P)^ and $000000FF) shl 24) or
        ((PCardinal(P)^ and $0000FF00) shl 8) or
        ((PCardinal(P)^ and $00FF0000) shr 8 ) or
        ((PCardinal(P)^ and $FF000000) shr 24)
  {$ELSE}
  C := ((PCardinal(P)^ shl 8) and $FF00FF00) or ((PCardinal(P)^ shr 8) and $00FF00FF);
  C := (C shl 16) or (C shr 16);
  {$ENDIF}
{$ELSE}
begin
  Result := PCardinal(P)^;
{$ENDIF}
end;

procedure Integer2PG(Value: Integer; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$IFNDEF ENDIAN_BIG}
var C: Cardinal absolute Value;
begin
  {$IFOPT R+}
  PCardinal(Buf)^ :=  ((c and $000000FF) shl 24) or
                      ((C and $0000FF00) shl 8) or
                      ((C and $00FF0000) shr 8 ) or
                      ((C and $FF000000) shr 24)
  {$ELSE}
  C := ((C shl 8) and $FF00FF00) or ((C shr 8) and $00FF00FF);
  PCardinal(Buf)^ := (C shl 16) or (C shr 16);
  {$ENDIF}
{$ELSE}
begin
  PInteger(Buf)^ :=  Value;
{$ENDIF}
end;

function PG2Cardinal(P: Pointer): Cardinal;
begin
{$IFNDEF ENDIAN_BIG}
  {$IFOPT R+}
  Result := ((PCardinal(P)^ and $000000FF) shl 24) or
            ((PCardinal(P)^ and $0000FF00) shl 8) or
            ((PCardinal(P)^ and $00FF0000) shr 8 ) or
            ((PCardinal(P)^ and $FF000000) shr 24)
  {$ELSE}
  Result := ((PCardinal(P)^ shl 8) and $FF00FF00) or ((PCardinal(P)^ shr 8) and $00FF00FF);
  Result := (Result shl 16) or (Result shr 16);
  {$ENDIF}
{$ELSE}
  Result := PCardinal(P)^;
{$ENDIF}
end;

procedure Cardinal2PG(Value: Cardinal; Buf: Pointer);
{$IFNDEF ENDIAN_BIG}
begin
  {$IFOPT R+}
  PCardinal(Buf)^ :=  ((Value and $000000FF) shl 24) or
                      ((Value and $0000FF00) shl 8) or
                      ((Value and $00FF0000) shr 8 ) or
                      ((Value and $FF000000) shr 24)
  {$ELSE}
  Value := ((Value shl 8) and $FF00FF00) or ((Value shr 8) and $00FF00FF);
  PCardinal(Buf)^ := (Value shl 16) or (Value shr 16);
  {$ENDIF}
{$ELSE}
begin
  PCardinal(Buf)^ := Value;
{$ENDIF}
end;

function PG2Int64(P: Pointer): Int64;
{$IFNDEF ENDIAN_BIG}
var {$IFNDEF CPU64}
    D64:  Int64Rec absolute Result;
    S64: PInt64Rec absolute P;
    {$ELSE}
    S64: PInt64    absolute P;
    {$ENDIF}
begin
  {$R-}
  {$IFNDEF CPU64}
  D64.Lo := ((S64.Hi shl 8) and $FF00FF00) or ((S64.Hi shr 8) and $00FF00FF);
  D64.Lo :=  (D64.Lo shl 16) or (D64.Lo shr 16);
  D64.Hi := ((S64.Lo shl 8) and $FF00FF00) or ((S64.Lo shr 8) and $00FF00FF);
  D64.Hi :=  (D64.Hi shl 16) or (D64.Hi shr 16);
  {$ELSE}
  Result := ((S64^   shl 8 ) and $FF00FF00FF00FF00) or ((S64^   shr 8 ) and $00FF00FF00FF00FF);
  Result := ((Result shl 16) and $FFFF0000FFFF0000) or ((Result shr 16) and $0000FFFF0000FFFF);
  Result :=  (Result shl 32) or ((Result shr 32));
  {$ENDIF}
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
{$ELSE}
begin
  Result := PInt64(P)^;
{$ENDIF}
end;

procedure Int642PG(const Value: Int64; Buf: Pointer);
{$IFNDEF ENDIAN_BIG}
var {$IFNDEF CPU64}
    S64:  Int64Rec absolute Value;
    D64: PInt64Rec absolute Buf;
    {$ELSE}
    D64: PInt64    absolute Buf;
    {$ENDIF}
begin
  {$R-}
  {$IFNDEF CPU64}
  D64.Lo := ((S64.Hi shl 8) and $FF00FF00) or ((S64.Hi shr 8) and $00FF00FF);
  D64.Lo :=  (D64.Lo shl 16) or (D64.Lo shr 16);
  D64.Hi := ((S64.Lo shl 8) and $FF00FF00) or ((S64.Lo shr 8) and $00FF00FF);
  D64.Hi :=  (D64.Hi shl 16) or (D64.Hi shr 16);
  {$ELSE}
  D64^ := ((Value shl 8 ) and $FF00FF00FF00FF00) or ((Value shr 8 ) and $00FF00FF00FF00FF);
  D64^ := ((D64^  shl 16) and $FFFF0000FFFF0000) or ((D64^  shr 16) and $0000FFFF0000FFFF);
  D64^ :=  (D64^  shl 32) or ((D64^ shr 32));
  {$ENDIF}
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
{$ELSE}
begin
  PInt64(Buf)^ := Value;
{$ENDIF}
end;

{$IFDEF WITH_PG_WEIGHT_OPT_BUG}{$O-}{$ENDIF}
const CurrMulTbl: array[0..4] of Int64 = (1, 10000, 100000000, 1000000000000, 10000000000000000) ;
{$R-} {$Q-} //for the endian shifts
function PGNumeric2Currency(P: Pointer): Currency;
var
  Numeric_External: PPGNumeric_External absolute P;
  NBASEDigits, Sign, NBASEDigit: Word;
  Weight, I: SmallInt;
  i64: Int64 absolute Result;
begin
  Sign := Numeric_External.sign;
  {$IFNDEF ENDIAN_BIG}Sign := ((Sign and $00FF) shl 8) or ((Sign and $FF00) shr 8);{$ENDIF ENDIAN_BIG}
  NBASEDigits := Numeric_External.NBASEDigits;
  {$IFNDEF ENDIAN_BIG}NBASEDigits := ((NBASEDigits and $00FF) shl 8) or ((NBASEDigits and $FF00) shr 8);{$ENDIF ENDIAN_BIG}
  Result := 0;
  if (NBASEDigits = 0) or (Sign = NUMERIC_NAN) or (Sign = NUMERIC_NULL) then
    Exit;
  Weight := Numeric_External.weight;
  {$IFNDEF ENDIAN_BIG}Word(Weight) := ((Word(Weight) and $00FF) shl 8) or ((Word(Weight) and $FF00) shr 8);{$ENDIF ENDIAN_BIG}
  Inc(Weight);
  for I := 0 to (NBASEDigits-1) do begin
    //NBASEDigit := Word(Numeric_External.digits[i]);
    NBASEDigit := Numeric_External.digits[i];
    {$IFNDEF ENDIAN_BIG}NBASEDigit := ((NBASEDigit and $00FF) shl 8) or ((NBASEDigit and $FF00) shr 8);{$ENDIF ENDIAN_BIG}
    I64 := I64 + NBASEDigit * CurrMulTbl[weight-i];
  end;
  if Sign <> NUMERIC_POS then
    Result := -Result;
end;
{$IFDEF WITH_PG_WEIGHT_OPT_BUG}{$O+}{$ENDIF}
{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "Int64" before doing the multiply could prevent overflow errors} {$ENDIF}
procedure Currency2PGNumeric(const Value: Currency; Buf: Pointer; out Size: Integer);
var
  U64, U64b: UInt64;
  NBASEDigits, NBASEDigit: Word;
  Numeric_External: PPGNumeric_External absolute Buf;
  {$IFDEF CPU64}
  I: SmallInt;
  {$ELSE}
  C: Cardinal;
label R3BDigit, R2BDigit, R1BDigit;  {EH: small jump table for unrolled 32 bit opt }
  {$ENDIF}
begin
  //https://doxygen.postgresql.org/backend_2utils_2adt_2numeric_8c.html#a3ae98a87bbc2d0dfc9cbe3d5845e0035
  if Value < 0 then begin
    U64 := -PInt64(@Value)^;
    {$IFNDEF ENDIAN_BIG}
    NBASEDigit := NUMERIC_NEG;
    Numeric_External.sign := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
    {$ELSE !ENDIAN_BIG}
    Numeric_External.sign := NUMERIC_NEG;
    {$ENDIF ENDIAN_BIG}
  end else if Value > 0 then begin
    U64 := PInt64(@Value)^;
    Numeric_External.sign := NUMERIC_POS
  end else begin
    PInt64(Buf)^ := 0; //clear all four 2 byte vales once
    Size := 8;
    Exit;
  end;
  NBASEDigits := (GetOrdinalDigits(U64) shr 2)+1;

  Numeric_External.NBASEDigits := {$IFNDEF ENDIAN_BIG}(NBASEDigits and $00FF shl 8) or (NBASEDigits and $FF00 shr 8){$ELSE}NBASEDigits{$ENDIF};  //write len
  Size := (4+NBASEDigits) * SizeOf(Word); //give size out
  NBASEDigit := Word(NBASEDigits-2);
  Word(Numeric_External.weight) := {$IFNDEF ENDIAN_BIG}(NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8){$ELSE}NBASEDigit{$ENDIF}; //write weight
  {$IFNDEF CPU64}
  if Int64Rec(u64).Hi = 0 then begin
    C := Int64Rec(u64).Lo div NBASE;
    NBASEDigit := Word(Int64Rec(u64).Lo-(C* NBASE)); //dividend mod 10000
    u64 := C; //next dividend
  end else begin
  {$ENDIF}
    U64b := U64 div NBASE; //get the scale digit
    NBASEDigit := Word(u64-(U64b * NBASE)); //dividend mod 10000
    u64 := U64b; //next dividend
  {$IFNDEF CPU64}
  end;
  {$ENDIF}
  if NBASEDigit = 0
  then Numeric_External.dscale := 0
  else Numeric_External.dscale := {$IFNDEF ENDIAN_BIG}(Word(4) and $00FF shl 8) or (Word(4) and $FF00 shr 8){$ELSE}4{$ENDIF};
{$IFNDEF ENDIAN_BIG}
  NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
{$ENDIF ENDIAN_BIG}
  Word(Numeric_External.digits[(NBASEDigits-1)]) := NBASEDigit; //set last scale digit
  {$IFDEF CPU64}
  if NBASEDigits > 1 then begin
    for I := NBASEDigits-2 downto 1{keep space for 1 base 10000 digit} do begin
      U64b := U64 div NBASE;
      NBASEDigit := Word(u64-(U64b * NBASE)); //dividend mod 10000
      u64 := U64b; //next dividend
      {$IFNDEF ENDIAN_BIG}
      NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
      {$ENDIF ENDIAN_BIG}
      Word(Numeric_External.digits[I]) := NBASEDigit;
    end;
    NBASEDigit := Word(Int64Rec(u64).Lo);
    {$IFNDEF ENDIAN_BIG}
    NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
    {$ENDIF ENDIAN_BIG}
    Word(Numeric_External.digits[0]) := NBASEDigit; //set first digit
  end;
  {$ELSE}
  case NBASEDigits-1 of
    4:  begin
          U64b := U64 div NBASE;
          NBASEDigit := Word(u64-(U64b * NBASE)); //dividend mod 10000
          u64 := U64b; //next dividend
          {$IFNDEF ENDIAN_BIG}
          NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
          {$ENDIF ENDIAN_BIG}
          Word(Numeric_External.digits[3]) := NBASEDigit;
          goto R3BDigit;
        end;
    3:  begin
R3BDigit: U64b := U64 div NBASE;
          NBASEDigit := Word(u64-(U64b * NBASE)); //dividend mod 10000
          u64 := U64b; //next dividend
          {$IFNDEF ENDIAN_BIG}
          NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
          {$ENDIF ENDIAN_BIG}
          Word(Numeric_External.digits[2]) := NBASEDigit;
          goto R2BDigit;
        end;
    2:  begin
R2BDigit: C := Int64Rec(u64).Lo div NBASE;
          NBASEDigit := Word(Int64Rec(u64).Lo-(C* NBASE)); //dividend mod 10000
          u64 := C; //next dividend
          {$IFNDEF ENDIAN_BIG}
          NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
          {$ENDIF ENDIAN_BIG}
          Word(Numeric_External.digits[1]) := NBASEDigit;
          goto R1BDigit;
        end;
    1:  begin
R1BDigit: NBASEDigit := Word(Int64Rec(u64).Lo);
          {$IFNDEF ENDIAN_BIG}
          NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
          {$ENDIF ENDIAN_BIG}
          Word(Numeric_External.digits[0]) := NBASEDigit;
       end;
  end;
  {$ENDIF CPU64}
end;
{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}


//each postgres num digit is a base 0...9999 digit, so we need to multiply
const WordFactors: array[0..3] of Word = (1000, 100, 10, 1);

{** written by EgonHugeist
  converts a bigdecimal value into a postgres numeric value
   @param src the value which should be converted
   @param dst the numeric buffer we write into
   @param size return the number of bytes we finally used
}
{$R-} {$Q-}
{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "Int64" before doing the multiply could prevent overflow errors} {$ENDIF}
procedure BCD2PGNumeric(const Src: TBCD; Dst: PAnsiChar; out Size: Integer);
var
  pNibble, PLastNibble, pWords: PAnsichar;
  FactorIndexOrScale, x, y: Integer;
  Precision, Scale, NBASEDigit: Word;
  Weight: SmallInt;
  GetFirstBCDHalfByte: Boolean;
begin
  //https://doxygen.postgresql.org/backend_2utils_2adt_2numeric_8c.html
  GetPacketBCDOffSets(Src, pNibble, PLastNibble, Precision, Scale, GetFirstBCDHalfByte);
  if Precision = 0 then begin//zero
    PInt64(Dst)^ := 0; //clear NBSEDigit, weight, sign, dscale  once
    Size := 8;
    Exit;
  end;
  Y := Precision -1;
  Precision := (Precision - Scale);
  { align word factor index }
  FactorIndexOrScale :=  BASE1000Digits - Precision mod 4;
  if FactorIndexOrScale = BASE1000Digits then
    FactorIndexOrScale := 0;
  PWords := Dst + 8; //set entry ptr
  pWord(PWords)^ := 0; //init first NBASE digit
  Weight := 0;
  for X := 0 to Y do begin
    if GetFirstBCDHalfByte
    then PWord(pWords)^ := PWord(pWords)^ + ((PByte(pNibble)^ shr 4) * WordFactors[FactorIndexOrScale])
    else begin
      PWord(pWords)^ := PWord(pWords)^ + ((PByte(pNibble)^ and $0f) * WordFactors[FactorIndexOrScale]);
      Inc(pNibble); //next nibble
    end;
    GetFirstBCDHalfByte := not GetFirstBCDHalfByte;
    if FactorIndexOrScale = 3 then begin
      {$IFNDEF ENDIAN_BIG}
      NBASEDigit := PWord(pWords)^;
      PWord(pWords)^ := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
      {$ENDIF !ENDIAN_BIG}
      FactorIndexOrScale := 0;
      if X < Precision then
        Inc(Weight);
      Inc(PWords, SizeOf(Word));
      if X < y then
        PWord(PWords)^ := 0;
    end else
      Inc(FactorIndexOrScale);
  end;
  {$IFNDEF ENDIAN_BIG}
  if (FactorIndexOrScale <> 0) then begin
    NBASEDigit := PWord(pWords)^;
    PWord(pWords)^ := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
  end;
  {$ENDIF !ENDIAN_BIG}
  if (Weight >= 0) or (Integer(Scale) - Y = 1) then
    Dec(Weight);
  NBASEDigit := (PWords - Dst - 8) shr 1; //calc count of nbase digits
  {$IFNDEF ENDIAN_BIG}
  NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
  {$ENDIF ENDIAN_BIG}
  //now fill static varlena values
  PWord(Dst)^ := NBASEDigit; //write NBASEDigit count
  //https://www.postgresql.org/message-id/491DC5F3D279CD4EB4B157DDD62237F404E27FE9%40zipwire.esri.com
  {$IFNDEF ENDIAN_BIG}
  NBASEDigit := (Word(Weight) and $00FF shl 8) or (Word(Weight) and $FF00 shr 8);
  PWord(Dst+2)^ := NBASEDigit; //white weight
  {$ELSE}
  PSmallInt(Dst+2)^ := Weight;
  {$ENDIF}
  if Src.SignSpecialPlaces and (1 shl 7) <> 0 then begin// write sign
  {$IFNDEF ENDIAN_BIG}
    NBASEDigit := NUMERIC_NEG;
    PWord(Dst+4)^ := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
  {$ELSE}
    PWord(Dst+4)^ := NUMERIC_NEG;
  {$ENDIF}
  end else PWord(Dst+4)^ := NUMERIC_POS;
  {$IFNDEF ENDIAN_BIG}
  Scale := (Scale and $00FF shl 8) or (Scale and $FF00 shr 8);
  {$ENDIF ENDIAN_BIG}
  PWord(Dst+6)^ := Scale; //dscale
  Size := (pWords - Dst); //return size in bytes
end;
{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF FPC} {$POP} {$ENDIF}

{** written by EgonHugeist
  converts a postgres numeric value into a bigdecimal value
  the buffer must have a minimum of 4*SizeOf(Word) and maximum size of 9*SizeOf(Word) bytes
  @param Src the pointer to a postgres numeric value
  @param Dst the result value to be converted
}
{$Q-} {$R-} //else my shift fail
{$IFDEF WITH_PG_WEIGHT_OPT_BUG}{$O-}{$ENDIF}
procedure PGNumeric2BCD(Src: PAnsiChar; var Dst: TBCD);
var
  i, NBASEDigitsCount, Precision, Scale, Digits: Integer;
  NBASEDigit, FirstNibbleDigit: Word;
  Weight: SmallInt;
  pNibble, pLastNibble: PAnsiChar;
  HalfNibbles: Boolean; //fpc compare fails in all areas if not strict left padded
label ZeroBCD, FourNibbles, Loop, Done, Final2, Final3, jmpScale;
begin
  FillChar(Dst.Fraction[0], MaxFMTBcdDigits, #0); //init fraction
  {$IFNDEF ENDIAN_BIG}
  NBASEDigit := PWord(Src)^;
  NBASEDigitsCount := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8);
  NBASEDigit := PWord(Src+4)^;
  NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8); //read sign
  {$ELSE !ENDIAN_BIG}
  NBASEDigitsCount := PWord(Src)^;
  NBASEDigit := PWord(Src+4)^; //read sign
  {$ENDIF ENDIAN_BIG}

  if ((NBASEDigitsCount = 0) and (NBASEDigit = NUMERIC_POS)) or      // zero
     ((NBASEDigit <> NUMERIC_POS) and (NBASEDigit <> NUMERIC_NEG)) then begin // NaN or NULL
ZeroBCD:
    PCardinal(@Dst.Precision)^ := ZInitZeroBCD;
    Exit;
  end;
  if NBASEDigit = NUMERIC_NEG
  then Dst.SignSpecialPlaces := $80
  else Dst.SignSpecialPlaces := 0;

  Weight := PSmallInt(Src+2)^;
  {$IFNDEF ENDIAN_BIG}Word(Weight) := (Word(Weight) and $00FF shl 8) or (Word(Weight) and $FF00 shr 8);{$ENDIF} //weight can be less than zero!
  Inc(Src, 8);
  pNibble := @Dst.Fraction[0];
  pLastNibble := pNibble + MaxFMTBcdDigits -1; //overflow control
  if Weight < 0 then begin {save absolute Weight value to I }
    I := -Weight;
    Inc(pNibble, (I - 1) shl 1); //set new bcd nibble offset
    if pNibble > pLastNibble then //overflow -> raise AV ?
      goto ZeroBCD;
  end else
    I := Weight;
  if NBASEDigitsCount <= I then begin
    Precision := (I - NBASEDigitsCount + 1) * BASE1000Digits;
    Scale := Precision * Ord(Weight < 0);
  end else if Weight < -1 then begin //scale starts with weight -1 nbase digits
    Precision := (I - 1) * BASE1000Digits;
    Scale := Precision;
  end else begin
    Precision := NBASEDigitsCount * BASE1000Digits;
    Scale := (NBASEDigitsCount-(Weight + 1)) * BASE1000Digits;
  end;
  //process first base-digit -> pack nibbles top most left  i.e. '0001' will be '1'
  NBASEDigit := {$IFNDEF ENDIAN_BIG}(PWord(Src)^ and $00FF shl 8) or (PWord(Src)^ and $FF00 shr 8){$ELSE}PWord(Src)^{$ENDIF}; //each digit is a base 10000 digit -> 0..9999
  FirstNibbleDigit := NBASEDigit div 100;
  HalfNibbles := False;
  if Weight > 0 then begin
    if FirstNibbleDigit > 0 then begin
      if NBASEDigit > 999 then begin
        I := 0;
        goto FourNibbles
      end else begin
        HalfNibbles := True;
        NBASEDigit := ZBase100Byte2BcdNibbleLookup[NBASEDigit - (FirstNibbleDigit * 100)]; //mod 100
        FirstNibbleDigit := ZBase100Byte2BcdNibbleLookup[FirstNibbleDigit];
        if I <= NBASEDigitsCount then
          Inc(pNibble);
        PByte(pNibble  )^ := Byte(FirstNibbleDigit shl 4) or Byte(NBASEDigit shr 4);
        PByte(pNibble+1)^ := Byte(NBASEDigit) shl 4;
        Inc(pNibble);
Final3: Digits := 3;
      end;
    end else if NBASEDigit > 9 then begin
      PByte(pNibble)^   := ZBase100Byte2BcdNibbleLookup[NBASEDigit];
Final2: Digits := 2;
    end else begin
      HalfNibbles := True;
      PByte(pNibble)^ := Byte(NBASEDigit) shl 4;
      Digits := 1;
    end;
    Dec(Precision, BASE1000Digits-Digits);
    if (NBASEDigitsCount = 1) or (pNibble = pLastNibble)
    then goto done;
    if not HalfNibbles then Inc(pNibble);
    I := 1;
  end else I := 0;
Loop:
  NBASEDigit := PWord(Src+(i shl 1))^;  //each digit is a base 10000 digit -> 0..9999
  {$IFNDEF ENDIAN_BIG}NBASEDigit := (NBASEDigit and $00FF shl 8) or (NBASEDigit and $FF00 shr 8){$ENDIF};
  FirstNibbleDigit := NBASEDigit div 100;
FourNibbles:
  NBASEDigit := NBASEDigit - (FirstNibbleDigit * 100); //mod 100
  NBASEDigit := ZBase100Byte2BcdNibbleLookup[NBASEDigit] {shl 8}; //move lookup 2 half bytes forward
  FirstNibbleDigit := ZBase100Byte2BcdNibbleLookup[FirstNibbleDigit];
  if HalfNibbles then begin
    PByte(pNibble  )^ := PByte(pNibble)^ or Byte(FirstNibbleDigit shr 4);
    PByte(pNibble+1)^ := Byte((FirstNibbleDigit) shl 4) or Byte(NBASEDigit shr 4);
    if pNibble < pLastNibble
    then PByte(pNibble+2)^ := Byte(NBASEDigit) shl 4
    else goto Final3;  //overflow -> raise EBcdOverflowException.Create(SBcdOverflow)
  end else if pNibble < pLastNibble
    then PWord(pNibble)^   := (NBASEDigit shl 8) or FirstNibbleDigit
    else begin
      PByte(pNibble)^   := FirstNibbleDigit;
      goto Final2;  //overflow -> raise EBcdOverflowException.Create(SBcdOverflow)
    end;
  if pNibble < pLastNibble
  then Inc(pNibble, 1+Ord(I<NBASEDigitsCount-1)) //keep offset of pNibble to lastnibble if loop end reached
  else goto Done; //overflow -> raise EBcdOverflowException.Create(SBcdOverflow)
  Inc(I);
  if I < NBASEDigitsCount then
    goto Loop;
Done:
  if (Scale > 0) then begin  //padd trailing zeroes away
    pLastNibble := PAnsiChar(@Dst.Fraction[0])+(Precision shr 1);//  pNibble + Ord(HalfNibbles);
    for I := Precision downto (Precision-Scale) do begin
      if (Scale > 0) and ((i and 1 = 1) and (PByte(pLastNibble)^ shr 4 = 0) or (i and 1 = 0) and (PByte(pLastNibble-1)^ and $0F = 0)) then begin
        Dec(Precision);
        Dec(Scale);
      end else Break;
      if (i and 1 = 0) then
        Dec(PLastNibble)
    end;
jmpScale:
    if Scale > 0 then
      if Dst.SignSpecialPlaces = $80
      then Dst.SignSpecialPlaces := Scale or $80
      else Dst.SignSpecialPlaces := Scale;
  end;
  Dst.Precision := Max(Precision, 1);
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
{$IFDEF WITH_PG_WEIGHT_OPT_BUG}{$O+}{$ENDIF}

function PGCash2Currency(P: Pointer): Currency;
var i64: Int64 absolute Result;
begin
  i64 := PG2Int64(P) * 100; //PGmoney as a scale of two but we've a scale of 4
end;

procedure Currency2PGCash(const Value: Currency; Buf: Pointer);
var i64: Int64 absolute Value;
begin
  Int642PG(i64 div 100, Buf); //PGmoney as a scale of two but we've a scale of 4
end;

function PG2Single(P: Pointer): Single;
begin
  Result := PSingle(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(@Result){$ENDIF}
end;

procedure Single2PG(Value: Single; Buf: Pointer);
begin
  PSingle(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(Buf){$ENDIF}
end;

function PG2Double(P: Pointer): Double;
{$IFNDEF ENDIAN_BIG}
var i64: Int64 absolute Result;
begin
  i64 := PG2Int64(P);
{$ELSE}
begin
  Result := PDouble(P)^;
{$ENDIF}
end;

procedure Double2PG(const Value: Double; Buf: Pointer);
{$IFNDEF ENDIAN_BIG}
var i64: Int64 absolute Value;
begin
  Int642PG(i64, Buf);
{$ELSE}
begin
  PDouble(Buf)^ := Value;
{$ENDIF}
end;

function PGMacAddr2Raw(Src, Dest: PAnsiChar): LengthInt;
begin
  PWord(Dest    )^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+0)^] or $2020;; //a
  PByte(Dest  +2)^ := Ord(':');
  PWord(Dest  +3)^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+1)^] or $2020;; //b
  PByte(Dest  +5)^ := Ord(':');
  PWord(Dest  +6)^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+2)^ ]or $2020;; //c
  PByte(Dest  +8)^ := Ord(':');
  PWord(Dest  +9)^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+3)^] or $2020;; //d
  PByte(Dest +11)^ := Ord(':');
  PWord(Dest +12)^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+4)^] or $2020;; //e
  PByte(Dest +14)^ := Ord(':');
  PWord(Dest +15)^ := ZSysUtils.TwoDigitLookupHexW[PByte(Src+5)^] or $2020;; //f
  PByte(Dest +17)^ := 0;
  Result := 17;
end;

function PGInetAddr2Raw(Src, Dest: PAnsiChar): LengthInt;
var Inet: PInetRec absolute Src;
  P: PAnsiChar;
  i, digits: Byte;
  w: word;
begin
  P := Dest;
  if Inet.nb = 4 then
    for I := 0 to 3 do begin
      digits := GetOrdinalDigits(Inet.ipaddr[i]);
      IntToRaw(Inet.ipaddr[i], P, digits);
      PByte(P+digits)^ := Ord('.');
      Inc(P, digits+1);
    end
  else for I := 0 to 7 do begin
    PWord(P)^ := ZSysUtils.TwoDigitLookupHexW[Inet.ipaddr[i shl 1]] or $2020;;
    W         := ZSysUtils.TwoDigitLookupHexW[Inet.ipaddr[i shl 1 + 1]] or $2020;
    if PByte(P)^ = Ord('0') then begin
      if PByte(P+1)^ <> Ord('0') then begin
        PByte(P)^ := PByte(P+1)^;
        PWord(P+1)^ := W;
        Inc(P, 3);
      end else begin
        PWord(P)^ := W;
        if PByte(P)^ = Ord('0') then begin
          PByte(P)^ := PByte(P+1)^;
          Inc(P);
        end else
          Inc(P,2);
      end;
    end else begin
      PWord(P+2)^ := W;
      Inc(P, 4);
    end;
    PByte(P)^ := Ord(':');
    Inc(P);
  end;
  if (Inet.nb = 4) or (Inet.is_cidr <> 0) then begin
    PByte(P-1)^ := Ord('/');
    digits := GetOrdinalDigits(Inet.bits);
    IntToRaw(Inet.bits, P, digits);
    Inc(P, digits);
  end else
    Dec(P);
  PByte(P)^ := 0;
  Result := P - Dest;
end;

function PGMacAddr2Uni(Src: PAnsiChar; Dest: PWideChar): LengthInt;
begin
  PCardinal(Dest    )^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+0)^] or $00200020; //a
  PWord(Dest      +2)^ := Ord(':');
  PCardinal(Dest  +3)^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+1)^] or $00200020; //b
  PWord(Dest      +5)^ := Ord(':');
  PCardinal(Dest  +6)^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+2)^] or $00200020; //c
  PWord(Dest      +8)^ := Ord(':');
  PCardinal(Dest  +9)^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+3)^] or $00200020; //d
  PWord(Dest     +11)^ := Ord(':');
  PCardinal(Dest +12)^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+4)^] or $00200020; //e
  PWord(Dest     +14)^ := Ord(':');
  PCardinal(Dest +15)^ := ZSysUtils.TwoDigitLookupHexLW[PByte(Src+5)^] or $00200020; //f
  PWord(Dest     +17)^ := 0;
  Result := 17;
end;

function PGInetAddr2Uni(Src: PAnsiChar; Dest: PWideChar): LengthInt;
var Inet: PInetRec absolute Src;
  P: PWideChar;
  i, digits: Byte;
  C: Cardinal;
begin
  P := Dest;
  if Inet.nb = 4 then
    for I := 0 to 3 do begin
      digits := GetOrdinalDigits(Inet.ipaddr[i]);
      IntToUnicode(Inet.ipaddr[i], P, digits);
      PWord(P+digits)^ := Ord('.');
      Inc(P, digits+1);
    end
  else for I := 0 to 7 do begin
    PCardinal(P)^ := ZSysUtils.TwoDigitLookupHexLW[Inet.ipaddr[i shl 1]] or $00200020;
    c             := ZSysUtils.TwoDigitLookupHexLW[Inet.ipaddr[i shl 1 + 1]] or $00200020;
    if PWord(P)^ = Ord('0') then begin
      if PWord(P+1)^ <> Ord('0') then begin
        PWord(P)^ := PWord(P+1)^;
        PCardinal(P+1)^ := C;
        Inc(P, 3);
      end else begin
        PCardinal(P)^ := C;
        if PWord(P)^ = Ord('0') then begin
          PWord(P)^ := PWord(P+1)^;
          Inc(P);
        end else
          Inc(P,2);
      end;
    end else begin
      PCardinal(P+2)^ := C;
      Inc(P, 4);
    end;
    PWord(P)^ := Ord(':');
    Inc(P);
  end;
  if (Inet.nb = 4) or (Inet.is_cidr <> 0) then begin
    PWord(P-1)^ := Ord('/');
    digits := GetOrdinalDigits(Inet.bits);
    IntToUnicode(Inet.bits, P, digits);
    Inc(P, digits);
  end else
    Dec(P);
  PWord(P)^ := 0;
  Result := P - Dest;
end;

function  ARR_NDIM(a: PArrayType): PInteger;
begin
  Result := @PArrayType(a).ndim;
end;

function  ARR_HASNULL(a: PArrayType): Boolean;
begin
  Result := PArrayType(a).flags <> 0;
end;

function  ARR_ELEMTYPE(a: PArrayType): POID;
begin
  Result := @PArrayType(a).elemtype;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinal and pointers is not portable} //the NativeUInt is an alias of PtrInt so it's size aligned
{$ENDIF}
function  ARR_DIMS(a: PArrayType): PInteger;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(SizeOf(TArrayType)));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinal and pointers is not portable} //the NativeUInt is an alias of PtrInt so it's size aligned
{$ENDIF}
function ARR_LBOUND(a: PArrayType): PInteger;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(SizeOf(TArrayType))+(SizeOf(Integer)*Cardinal(PG2Integer(ARR_NDIM(a)))));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

(**
  Returns the actual array data offset.
*)
function  ARR_DATA_OFFSET(a: PArrayType): Int32;
begin
  if ARR_HASNULL(a)
  then Result := PG2Integer(@PArrayType(a).flags)
  else Result := ARR_OVERHEAD_NONULLS(PG2Integer(ARR_NDIM(a)));
end;

function ARR_OVERHEAD_NONULLS(ndims: Integer): Integer;
begin
  Result := sizeof(TArrayType) + 2 * sizeof(integer) * (ndims)
end;

(**
  Returns a pointer to the actual array data.
*)
{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinal and pointers is not portable} //the NativeUInt is an alias of PtrInt so it's size aligned
{$ENDIF}
function  ARR_DATA_PTR(a: PArrayType): Pointer;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(ARR_DATA_OFFSET(a)));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
