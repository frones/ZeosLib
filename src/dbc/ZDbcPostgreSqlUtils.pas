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
function PostgreSQLToSQLType(ConSettings: PZConSettings;
  OIDAsBlob: Boolean; TypeOid: OID; TypeModifier: Integer): TZSQLType; overload;

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

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
  @param ResultHandle the Handle to the Result
}
procedure HandlePostgreSQLError(const Sender: IImmediatelyReleasable;
  const PlainDriver: TZPostgreSQLPlainDriver; conn: TPGconn;
  LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  ResultHandle: TPGresult);

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
procedure dt2time(jd: Int64; out Hour, Min, Sec: Word; out fsec: LongWord); overload;
procedure dt2time(jd: Double; out Hour, Min, Sec: Word; out fsec: LongWord); overload;

procedure DateTime2PG(const Value: TDateTime; out Result: Int64); overload;
procedure DateTime2PG(const Value: TDateTime; out Result: Double); overload;

procedure Date2PG(const Value: TDateTime; out Result: Integer);

procedure Time2PG(const Value: TDateTime; out Result: Int64); overload;
procedure Time2PG(const Value: TDateTime; out Result: Double); overload;

function PG2DateTime(Value: Double): TDateTime; overload;
procedure PG2DateTime(Value: Double; out Year, Month, Day, Hour, Min, Sec: Word;
  out fsec: LongWord); overload;

function PG2DateTime(Value: Int64): TDateTime; overload;
procedure PG2DateTime(Value: Int64; out Year, Month, Day, Hour, Min, Sec: Word;
  out fsec: LongWord); overload;

function PG2Time(Value: Double): TDateTime; overload;
function PG2Time(Value: Int64): TDateTime; overload;

function PG2Date(Value: Integer): TDateTime;

function PG2SmallInt(P: Pointer): SmallInt; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure SmallInt2PG(Value: SmallInt; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Word(P: Pointer): Word; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Word2PG(Value: Word; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Integer(P: Pointer): Integer; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Integer2PG(Value: LongInt; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Cardinal(P: Pointer): Cardinal; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Cardinal2PG(Value: Cardinal; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Int64(P: Pointer): Int64; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Int642PG(const Value: Int64; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PGNumeric2Currency(P: Pointer): Currency; //{$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Currency2PGNumeric(const Value: Currency; Buf: Pointer; out Size: Integer); //{$IFDEF WITH_INLINE}inline;{$ENDIF}

function PGCash2Currency(P: Pointer): Currency; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Currency2PGCash(const Value: Currency; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Single(P: Pointer): Single; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Single2PG(Value: Single; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function PG2Double(P: Pointer): Double; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure Double2PG(const Value: Double; Buf: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}

procedure MoveReverseByteOrder(Dest, Src: PAnsiChar; Len: LengthInt);


//ported macros from array.h
function ARR_NDIM(a: PArrayType): PInteger;
function ARR_HASNULL(a: PArrayType): Boolean;
function ARR_ELEMTYPE(a: PArrayType): POID;
function ARR_DIMS(a: PArrayType): PInteger;
function ARR_LBOUND(a: PArrayType): PInteger;
function ARR_NULLBITMAP(a: PArrayType): PByte;
function ARR_OVERHEAD_NONULLS(ndims: Integer): Integer;
function ARR_OVERHEAD_WITHNULLS(ndims, nitems: Integer): Integer;
function ARR_DATA_OFFSET(a: PArrayType): Int32;
function ARR_DATA_PTR(a: PArrayType): Pointer;

const MinPGNumSize = (1{ndigits}+1{weight}+1{sign}+1{dscale})*SizeOf(Word);
const MaxCurr2NumSize = MinPGNumSize+(5{max 5 NBASE ndigits}*SizeOf(Word));
const MaxBCD2NumSize  = MinPGNumSize+(MaxFMTBcdFractionSize div 4{max 5 NBASE ndigits}*SizeOf(Word));

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses Math, ZFastCode, ZMessages, ZSysUtils, ZClasses, ZDbcUtils;

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
begin
  TypeNameLo := LowerCase(TypeName);
  if (TypeNameLo = 'interval') or (TypeNameLo = 'char') or (TypeNameLo = 'bpchar')
    or (TypeNameLo = 'varchar') or (TypeNameLo = 'bit') or (TypeNameLo = 'varbit')
  then//EgonHugeist: Highest Priority Client_Character_set!!!!
    if (Connection.GetConSettings.CPType = cCP_UTF16) then
      Result := stUnicodeString
    else
      Result := stString
  else if TypeNameLo = 'text' then
    Result := stAsciiStream
  else if TypeNameLo = 'oid' then
  begin
    if Connection.IsOidAsBlob() then
      Result := stBinaryStream
    else
      Result := stInteger;
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
  else if (TypeNameLo = 'float8') or (TypeNameLo = 'decimal')
    or (TypeNameLo = 'numeric') then
    Result := stDouble
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
  else if (TypeNameLo <> '') and (TypeNameLo[1] = '_') then // ARRAY TYPES
    Result := stAsciiStream
  else if (TypeNameLo = 'uuid') then
    Result := stGuid
  else
    Result := stUnknown;

  if (Connection.GetConSettings.CPType = cCP_UTF16) then
    if Result = stAsciiStream then
      Result := stUnicodeStream;
end;

{**
   Another version of PostgreSQLToSQLType()
     - comparing integer should be faster than AnsiString.
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(ConSettings: PZConSettings;
  OIDAsBlob: Boolean; TypeOid: OID; TypeModifier: Integer): TZSQLType; overload;
var Scale: Integer;
begin
  case TypeOid of
    INTERVALOID, CHAROID, BPCHAROID, VARCHAROID:  { interval/char/bpchar/varchar }
      if (ConSettings.CPType = cCP_UTF16) then
          Result := stUnicodeString
        else
          Result := stString;
    TEXTOID: Result := stAsciiStream; { text }
    OIDOID: { oid }
      begin
        if OidAsBlob then
          Result := stBinaryStream
        else
          Result := stInteger;
      end;
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
          if (Scale <= 4) and ((TypeModifier - VARHDRSZ) shr 16 and $FFFF <= sAlignCurrencyScale2Precision[Scale]) then
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
    BYTEAOID: { bytea }
        if OidAsBlob
        then Result := stBytes
        else Result := stBinaryStream;
    UUIDOID: Result := stGUID; {uuid}
    INT2VECTOROID, OIDVECTOROID: Result := stAsciiStream; { int2vector/oidvector. no '_aclitem' }
    143,629,651,719,791,1000..OIDARRAYOID,1040,1041,1115,1182,1183,1185,1187,1231,1263,
    1270,1561,1563,2201,2207..2211,2949,2951,3643,3644,3645,3735,3770 : { other array types }
      Result := stAsciiStream;
    else
      Result := stUnknown;
  end;

  if (ConSettings.CPType = cCP_UTF16) then
    if Result = stAsciiStream then
      Result := stUnicodeStream;
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
  end;
end;

procedure SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: Boolean; out aOID: OID);
begin
  case SQLType of
    stUnknown: aOID := INVALIDOID;
    stBoolean: aOID := BOOLOID;
    stByte, stShort, stSmall: aOID := INT2OID;
    stWord, stInteger: aOID := INT4OID;
    stLongWord, stLong, stULong: aOID := INT8OID;
    stFloat: aOID := FLOAT4OID;
    stDouble{$IFNDEF BCD_TEST},stBigDecimal{$ENDIF}: aOID := FLOAT8OID;
    {$IFDEF BCD_TEST}stBigDecimal, {$ENDIF}stCurrency: aOID := NUMERICOID;//CASHOID;  the pg money has a scale of 2 while we've a scale of 4
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

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
  //FirmOS 22.02.06
  @param ResultHandle the Handle to the Result
}
procedure HandlePostgreSQLError(const Sender: IImmediatelyReleasable;
  const PlainDriver: TZPostgreSQLPlainDriver; conn: TPGconn;
  LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  ResultHandle: TPGresult);
var
   resultErrorFields: array[TZPostgreSQLFieldCode] of PAnsiChar;
   I: TZPostgreSQLFieldCode;
   ErrorMessage: PAnsiChar;
   ConSettings: PZConSettings;
   aMessage, aErrorStatus: String;
begin
  ErrorMessage := PlainDriver.PQerrorMessage(conn);
  if PGSucceeded(ErrorMessage) then Exit;

  for i := low(TZPostgreSQLFieldCode) to high(TZPostgreSQLFieldCode) do
    if Assigned(ResultHandle) and Assigned(PlainDriver.PQresultErrorField) {since 7.4}
    then resultErrorFields[i] := PlainDriver.PQresultErrorField(ResultHandle,TPG_DIAG_ErrorFieldCodes[i])
    else resultErrorFields[i] := nil;

  if Assigned(Sender) then begin
    ConSettings := Sender.GetConSettings;
    aMessage := '';
    for i := low(TZPostgreSQLFieldCode) to high(TZPostgreSQLFieldCode) do
       if resultErrorFields[i] <> nil then
        aMessage := aMessage + TPG_DIAG_ErrorFieldPrevixes[i]+Trim(ConSettings^.ConvFuncs.ZRawToString(resultErrorFields[i],
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP));
    if aMessage <> ''
    then aMessage := Format(SSQLError1, [aMessage])
    else aMessage := Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(
          ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]);
    aErrorStatus := ConSettings^.ConvFuncs.ZRawToString(resultErrorFields[pgdiagSQLSTATE],
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    if DriverManager.HasLoggingListener then
      DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
        0, ErrorMessage);
  end else begin
    aMessage := '';
    for i := low(TZPostgreSQLFieldCode) to high(TZPostgreSQLFieldCode) do
       if resultErrorFields[i] <> nil then
        aMessage := aMessage + TPG_DIAG_ErrorFieldPrevixes[i]+Trim(String(resultErrorFields[i]));
    if aMessage <> ''
    then aMessage := Format(SSQLError1, [aMessage])
    else aMessage := Format(SSQLError1, [String(ErrorMessage)]);
    aErrorStatus := String(resultErrorFields[pgdiagSQLSTATE]);
    if DriverManager.HasLoggingListener then
      DriverManager.LogError(LogCategory, 'postresql', LogMessage, 0, ErrorMessage);
  end;

  if ResultHandle <> nil then
    PlainDriver.PQclear(ResultHandle);
  if PlainDriver.PQstatus(conn) = CONNECTION_BAD then begin
    if Assigned(Sender) then
      Sender.ReleaseImmediat(Sender);
    raise EZSQLConnectionLost.CreateWithCodeAndStatus(Ord(CONNECTION_BAD), aErrorStatus, aMessage);
  end else if LogCategory <> lcUnprepStmt then //silence -> https://sourceforge.net/p/zeoslib/tickets/246/
    raise EZSQLException.CreateWithStatus(aErrorStatus, aMessage);
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
  Result := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(PWideChar(@Buf[0]), 0);
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

procedure j2date(jd: Integer; out AYear, AMonth, ADay: Word);
var
  julian, quad, extra: LongWord;
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

{$IFNDEF ENDIAN_BIG}
procedure Reverse2Bytes(P: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
var W: Byte;
begin
  W := PByte(P)^;
  PByteArray(P)[0] := PByteArray(P)[1];
  PByteArray(P)[1] := W;
end;
{$ENDIF}

{$IFNDEF ENDIAN_BIG}
procedure Reverse4Bytes(P: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
var W: Word;
begin
  W := PWord(P)^;
  PByteArray(P)[0] := PByteArray(P)[3];
  PByteArray(P)[1] := PByteArray(P)[2];
  PByteArray(P)[2] := PByteArray(@W)[1];
  PByteArray(P)[3] := PByteArray(@W)[0];
end;
{$ENDIF}

{$IFNDEF ENDIAN_BIG}
procedure Reverse8Bytes(P: Pointer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
var W: LongWord;
begin
  W := PLongWord(P)^;
  PByteArray(P)[0] := PByteArray(P)[7];
  PByteArray(P)[1] := PByteArray(P)[6];
  PByteArray(P)[2] := PByteArray(P)[5];
  PByteArray(P)[3] := PByteArray(P)[4];
  PByteArray(P)[4] := PByteArray(@W)[3];
  PByteArray(P)[5] := PByteArray(@W)[2];
  PByteArray(P)[6] := PByteArray(@W)[1];
  PByteArray(P)[7] := PByteArray(@W)[0];
end;
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

function PG2DateTime(Value: Double): TDateTime;
var date: TDateTime;
  Year, Month, Day, Hour, Min, Sec: Word;
  fsec: LongWord;
begin
  PG2DateTime(Value, Year, Month, Day, Hour, Min, Sec, fsec);
  TryEncodeDate(Year, Month, Day, date);
  dt2time(Value, Hour, Min, Sec, fsec);
  TryEncodeTime(Hour, Min, Sec, fsec, Result);
  Result := date + Result;
end;

procedure PG2DateTime(value: Double; out Year, Month, Day, Hour, Min, Sec: Word;
  out fsec: LongWord);
var
  date: Double;
  time: Double;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  time := value;
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
  dt2time(Time, Hour, Min, Sec, fsec);
end;

function PG2DateTime(Value: Int64): TDateTime;
var date: TDateTime;
  Year, Month, Day, Hour, Min, Sec: Word;
  fsec: LongWord;
begin
  PG2DateTime(Value, Year, Month, Day, Hour, Min, Sec, fsec);
  if not TryEncodeDate(Year, Month, Day, date) then
    Date := 0;
  if not TryEncodeTime(Hour, Min, Sec, fsec div MSecsPerSec, Result) then
    Result := 0;
  Result := date + Result;
end;

procedure PG2DateTime(Value: Int64; out Year, Month, Day, Hour, Min, Sec: Word;
  out fsec: LongWord);
var date: Int64;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  date := Value div USECS_PER_DAY;
  Value := Value mod USECS_PER_DAY;
  if Value < 0 then begin
    Value := Value + USECS_PER_DAY;
    date := date - 1;
  end;
  date := date + POSTGRES_EPOCH_JDATE;
  j2date(date, Year, Month, Day);
  dt2time(Value, Hour, Min, Sec, fsec);
end;

procedure dt2time(jd: Int64; out Hour, Min, Sec: Word; out fsec: LongWord);
begin
  Hour := jd div USECS_PER_HOUR;
  jd := jd - Int64(Hour) * Int64(USECS_PER_HOUR);
  Min := jd div USECS_PER_MINUTE;
  jd := jd - Int64(Min) * Int64(USECS_PER_MINUTE);
  Sec := jd div USECS_PER_SEC;
  Fsec := jd - (Int64(Sec) * Int64(USECS_PER_SEC));
end;

procedure dt2time(jd: Double; out Hour, Min, Sec: Word; out fsec: LongWord);
begin
  Hour := Trunc(jd / SECS_PER_HOUR);
  jd := jd - Hour * SECS_PER_HOUR;
  Min := Trunc(jd / SECS_PER_MINUTE);
  jd := jd - Min * SECS_PER_MINUTE;
  Sec := Trunc(jd);
  Fsec := Trunc(jd - Sec);
end;

procedure Time2PG(const Value: TDateTime; out Result: Int64);
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := (((((hour * MINS_PER_HOUR) + min) * SECS_PER_MINUTE) + sec) * USECS_PER_SEC) + Msec;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

procedure Time2PG(const Value: TDateTime; out Result: Double);
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  //macro of datetime.c
  Result := (((hour * MINS_PER_HOUR) + min) * SECS_PER_MINUTE) + sec + Msec;
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Result);
  {$ENDIF}
end;

function PG2Time(Value: Double): TDateTime;
var Hour, Min, Sec: Word; fsec: LongWord;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, fsec);
  if not TryEncodeTime(Hour, Min, Sec, Fsec, Result) then
    Result := 0;
end;

function PG2Time(Value: Int64): TDateTime;
var Hour, Min, Sec: Word; fsec: LongWord;
begin
  {$IFNDEF ENDIAN_BIG}
  Reverse8Bytes(@Value);
  {$ENDIF}
  dt2Time(Value, Hour, Min, Sec, fsec);
  if not TryEncodeTime(Hour, Min, Sec, Fsec, Result) then
    Result := 0;
end;

procedure Date2PG(const Value: TDateTime; out Result: Integer);
var y,m,d: Word;
begin
  DecodeDate(Value, y,m,d);
  Result := date2j(y,m,d) - POSTGRES_EPOCH_JDATE;
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

procedure MoveReverseByteOrder(Dest, Src: PAnsiChar; Len: LengthInt);
begin
  { adjust byte order of host to network  }
  {$IFNDEF ENDIAN_BIG}
  Dest := Dest+Len-1;
  while Len > 0 do begin
    Dest^ := Src^;
    dec(Dest);
    Inc(Src);
    dec(Len);
  end;
  {$ELSE}
  Move(Src^, Dest^, Len);
  {$ENDIF}
end;

function PG2SmallInt(P: Pointer): SmallInt;
begin
  Result := PSmallInt(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse2Bytes(@Result){$ENDIF}
end;

procedure SmallInt2PG(Value: SmallInt; Buf: Pointer);
begin
  PSmallInt(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse2Bytes(Buf){$ENDIF}
end;

function PG2Word(P: Pointer): Word;
begin
  Result := PWord(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse2Bytes(@Result){$ENDIF}
end;

procedure Word2PG(Value: Word; Buf: Pointer);
begin
  PWord(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse2Bytes(Buf){$ENDIF}
end;

function PG2Integer(P: Pointer): LongInt;
begin
  Result := PLongInt(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(@Result){$ENDIF}
end;

procedure Integer2PG(Value: LongInt; Buf: Pointer);
begin
  PLongInt(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(Buf){$ENDIF}
end;

function PG2Cardinal(P: Pointer): Cardinal;
begin
  Result := PCardinal(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(@Result){$ENDIF}
end;

procedure Cardinal2PG(Value: Cardinal; Buf: Pointer);
begin
  PCardinal(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse4Bytes(Buf){$ENDIF}
end;

function PG2Int64(P: Pointer): Int64;
begin
  Result := PInt64(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(@Result){$ENDIF}
end;

procedure Int642PG(const Value: Int64; Buf: Pointer);
begin
  PInt64(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(Buf){$ENDIF}
end;

function PGNumeric2Currency(P: Pointer): Currency;
var
  Numeric_External: PPGNumeric_External absolute P;
  {Scale, }Sign: Word;
  NBASEDigits, Weight, I: SmallInt;
begin
  Result := 0;
  Sign := PG2Word(@Numeric_External.sign);
  NBASEDigits := PG2Word(@Numeric_External.NBASEDigits);
  if (NBASEDigits = 0) or (Sign = NUMERIC_NAN) or (Sign = NUMERIC_NULL) then
    Exit;
//  Scale := PG2Word(@Numeric_External.dscale);
  Weight := PG2SmallInt(@Numeric_External.weight);
  for I := 0 to NBASEDigits -1 do
    Result := Result + PG2SmallInt(@Numeric_External.digits[i]) * IntPower(NBASE, Weight-i);
  if Sign = NUMERIC_NEG then
    Result := -Result;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure Currency2PGNumeric(const Value: Currency; Buf: Pointer; out Size: Integer);
var
  U64, U64b: UInt64;
  NBASEDigits, I, NBASEDigit: SmallInt;
  Numeric_External: PPGNumeric_External absolute Buf;
begin
  //https://doxygen.postgresql.org/backend_2utils_2adt_2numeric_8c.html#a3ae98a87bbc2d0dfc9cbe3d5845e0035
  if Value < 0 then begin
    U64 := -PInt64(@Value)^;
    Word2PG(NUMERIC_NEG, @Numeric_External.sign);
  end else begin
    U64 := PInt64(@Value)^;
    if Value <> 0 then
      Numeric_External.sign := NUMERIC_POS
    else begin
      PInt64(Buf)^ := 0; //clear all four 2 byte vales once
      Size := 8;
      Exit;
    end;
  end;
  NBASEDigits := (GetOrdinalDigits(U64) shr 2)+1;
  Word2PG(Word(NBASEDigits), @Numeric_External.NBASEDigits); //write len
  SmallInt2PG(NBASEDigits-2, @Numeric_External.weight); //weight
  U64b := U64 div NBASE; //get the scale digit
  NBASEDigit := SmallInt(u64-(U64b * NBASE)); //dividend mod 10000
  u64 := U64b; //next dividend
  if NBASEDigit = 0 then begin
    Numeric_External.dscale := 0;
    Numeric_External.digits[(NBASEDigits-1)] := 0;
  end else begin
    Word2PG(GetOrdinalDigits(Word(NBASEDigit)), @Numeric_External.dscale);
    SmallInt2PG(NBASEDigit, @Numeric_External.digits[(NBASEDigits-1)]); //set last scale digit
  end;
  for I := NBASEDigits-2 downto 1{keep space for 1 base 10000 digit} do begin
    U64b := U64 div NBASE;
    NBASEDigit := u64-(U64b * NBASE); //dividend mod 10000
    u64 := U64b; //next dividend
    SmallInt2PG(NBASEDigit, @Numeric_External.digits[I]);
  end;
  SmallInt2PG(SmallInt(Int64Rec(u64).Lo), @Numeric_External.digits[0]); //set first digit
  Size := (4+NBASEDigits) * SizeOf(Word);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function PGCash2Currency(P: Pointer): Currency;
begin
  PInt64(@Result)^ := PInt64(P)^; //move first
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(@Result);{$ENDIF}
  Result {%H-}:= PInt64(@Result)^ div 100;
end;

procedure Currency2PGCash(const Value: Currency; Buf: Pointer);
begin
  PInt64(Buf)^ := PInt64(@Value)^*100; //PGmoney as a scale of two but we've a scale of 4
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(Buf){$ENDIF}
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
begin
  Result := PDouble(P)^;
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(@Result){$ENDIF}
end;

procedure Double2PG(const Value: Double; Buf: Pointer);
begin
  PDouble(Buf)^ := Value;
  {$IFNDEF ENDIAN_BIG}Reverse8Bytes(Buf){$ENDIF}
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

function  ARR_DIMS(a: PArrayType): PInteger;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(SizeOf(TArrayType)));
end;

function ARR_LBOUND(a: PArrayType): PInteger;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(SizeOf(TArrayType))+(SizeOf(Integer)*Cardinal(PG2Integer(ARR_NDIM(a)))));
end;

function  ARR_NULLBITMAP(a: PArrayType): PByte;
begin
  if ARR_HASNULL(a) then
    Result := Pointer(NativeUInt(a)+NativeUInt(SizeOf(TArrayType))+
      (2*(SizeOf(Integer)*Cardinal(PG2Integer(ARR_NDIM(a))))))
  else
    Result := nil;
end;

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
  The total array header size (in bytes) for an array with the specified
  number of dimensions and total number of items.
*)
function ARR_OVERHEAD_WITHNULLS(ndims, nitems: Integer): Integer;
begin
  Result := sizeof(TArrayType) + 2 * sizeof(integer) * (ndims) + ((nitems + 7) shr 3 {div 8})
end;

(**
  Returns a pointer to the actual array data.
*)
function  ARR_DATA_PTR(a: PArrayType): Pointer;
begin
  Result := Pointer(NativeUInt(a)+NativeUInt(ARR_DATA_OFFSET(a)));
end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
