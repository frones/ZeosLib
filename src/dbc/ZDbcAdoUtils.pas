{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Specific Utilities                  }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcAdoUtils;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  FmtBCD,
  ZDbcIntfs, ZCompatibility, ZPlainAdo, ZVariant, ZPlainOleDBDriver;

type
  PDirectionTypes = ^TDirectionTypes;
  TDirectionTypes = array of TOleEnum;

const ZProcedureColumnType2AdoType: array[TZProcedureColumnType] of  TOleEnum =
  (adParamUnknown{pctUnknown}, adParamInput{pctIn}, adParamInputOutput{pctInOut},
    adParamOutput{pctOut}, adParamReturnValue{pctReturn}, adParamReturnValue{pctResultSet});
  AdoType2ZProcedureColumnType: array[adParamUnknown..adParamReturnValue] of TZProcedureColumnType =
    (pctUnknown, pctIn, pctInOut, pctOut, pctReturn);

 ZSQLTypeToAdoParamSize: array[TZSQLType] of Integer = (0,//stUnknown,
    //fixed size DataTypes first
    SizeOf(WordBool),
    SizeOf(Byte), SizeOf(ShortInt), SizeOf(Word), SizeOf(SmallInt), SizeOf(Cardinal), SizeOf(Integer), SizeOf(UInt64), SizeOf(Int64),  //ordinals
    SizeOf(Single), SizeOf(Double), SizeOf(Currency), SizeOf(tagDec),
    SizeOf(TDateTime), SizeOf(TDateTime), SizeOf(TDateTime), 38,
    //now varying size types in equal order
    15000,15000,15000,//stString, stUnicodeString, stBytes,
    8192,8192,8192,//stAsciiStream, stUnicodeStream, stBinaryStream,  -> send chunked
    //finally the object types
    0,0//stArray, stDataSet not supported yet
    );
 ZSQLTypeToAdoType: array[TZSQLType] of TDataTypeEnum = (adEmpty,//stUnknown,
    adBoolean, adUnsignedTinyInt, adTinyInt, adUnsignedSmallInt, adSmallInt,
    adUnsignedInt, adInteger, adUnsignedBigInt, adBigInt,
    adSingle, adDouble, adCurrency, adDecimal,
    adDate{no msec loss}, adDate, adDate{no msec loss}, //check Paramter Precsion & Scale for full msec-support
    adGUID,
    {adVarWChar}adBSTR,{adVarWChar}adBSTR,adVarBinary,
    {adLongVarWChar}adBSTR, {adLongVarWChar}adBSTR, adLongVarBinary,
    adArray,adIDispatch//stArray, stDataSet not supported yet
    );

{**
  Converts an ADO native types into string related.
  @param FieldType dblibc native field type.
  @return a string data type name.
}
function ConvertAdoToTypeName(FieldType: SmallInt): string;

{**
  Converts a Ado native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertAdoToSqlType(const FieldType, Precision, Scale: SmallInt): TZSQLType;

{**
  Converts a Zeos type into ADO types.
  @param FieldType zeos field type.
  @return a ADO datatype.
}
function ConvertSqlTypeToAdo(FieldType: TZSQLType): Integer;

{**
  Converts a Variant type into ADO types.
  @param VT Variant datatype.
  @return a ADO datatype.
}
function ConvertVariantToAdo(VT: TVarType): Integer;

{**
  Converts a TZResultSetType type into ADO cursor type.
  @param ResultSetType.
  @return a ADO cursor type.
}
function ConvertResultSetTypeToAdo(ResultSetType: TZResultSetType): Integer;

{**
  Converts a TZResultSetConcurrency type into ADO lock type.
  @param ResultSetConcurrency.
  @return a ADO lock type.
}
function ConvertResultSetConcurrencyToAdo(ResultSetConcurrency: TZResultSetConcurrency): Integer;

{**
  Converts a OLEDB schema guid into ADO schema ID usable with OpenSchema.
  @param OleDBSchema schema guid.
  @return a ADO schema id.
}
function ConvertOleDBToAdoSchema(OleDBSchema: TGUID): Integer;

function IsSelect(const SQL: string): Boolean;

procedure BCD2Decimal(const Value: TBCD; Dest: PDecimal);

var
{**
  Required to free memory allocated by oledb
}
  ZAdoMalloc: IMalloc;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF}, Variants,
  ZSysUtils, ZDbcUtils, ZMessages, ZEncoding, ZFastCode;

{**
  Converts an ADO native types into string related.
  @param FieldType dblibc native field type.
  @return a string data type name.
}
function ConvertAdoToTypeName(FieldType: SmallInt): string;
begin
  case FieldType of
    adChar             : Result := 'Char';
    adVarChar          : Result := 'VarChar';
    adBSTR             : Result := 'BSTR';
    adWChar            : Result := 'WChar';
    adVarWChar         : Result := 'VarWChar';
    adBoolean          : Result := 'Boolean';
    adTinyInt          : Result := 'TinyInt';
    adUnsignedTinyInt  : Result := 'UnsignedTinyInt';
    adSmallInt         : Result := 'SmallInt';
    adUnsignedSmallInt : Result := 'UnsignedSmallInt';
    adInteger          : Result := 'Integer';
    adUnsignedInt      : Result := 'UnsignedInt';
    adBigInt           : Result := 'BigInt';
    adUnsignedBigInt   : Result := 'UnsignedBigInt';
    adSingle           : Result := 'Single';
    adDouble           : Result := 'Double';
    adDecimal          : Result := 'Decimal';
    adNumeric          : Result := 'Numeric';
    adVarNumeric       : Result := 'VarNumeric';
    adCurrency         : Result := 'Currency';
    adDBDate           : Result := 'DBDate';
    adDBTime           : Result := 'DBTime';
    adDate             : Result := 'Date';
    adDBTimeStamp      : Result := 'DBTimeStamp';
    adFileTime         : Result := 'FileTime';
    adLongVarChar      : Result := 'LongVarChar';
    adLongVarWChar     : Result := 'LongVarWChar';
    adBinary           : Result := 'Binary';
    adVarBinary        : Result := 'VarBinary';
    adLongVarBinary    : Result := 'LongVarBinary';
    adGUID             : Result := 'GUID';
    adEmpty            : Result := 'Empty';
    adError            : Result := 'Error';
    adArray            : Result := 'Array';
    adChapter          : Result := 'Chapter';
    adIDispatch        : Result := 'IDispatch';
    adIUnknown         : Result := 'IUnknown';
    adPropVariant      : Result := 'PropVariant';
    adUserDefined      : Result := 'UserDefined';
    adVariant          : Result := 'Variant';
  else
    Result := 'Unknown';
  end;
end;

{**
  Converts a Ado native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertAdoToSqlType(const FieldType, Precision, Scale: SmallInt): TZSQLType;
begin
  //http://msdn.microsoft.com/en-us/library/windows/desktop/ms675318%28v=vs.85%29.aspx
  case FieldType of
    adChar, adVarChar: Result := stString;
    adWChar, adVarWChar, adBSTR: Result := stUnicodeString;
    adBoolean: Result := stBoolean;
    adTinyInt: Result := stShort;
    adUnsignedTinyInt: Result := stByte;
    adSmallInt: Result := stSmall;
    adUnsignedSmallInt: Result := stWord;
    adInteger, adError{Indicates a 32-bit error code}: Result := stInteger;
    adUnsignedInt: Result := stLongWord;
    adBigInt: Result := stLong;
    adUnsignedBigInt: Result := stULong;
    adSingle: Result := stFloat;
    adDouble: Result := stDouble;
    adDecimal, adNumeric, adVarNumeric:
        if (Scale >= 0) and (Scale <= 4) and (Precision < sAlignCurrencyScale2Precision[Scale])
        then Result := stCurrency
        else Result := stBigDecimal;
    adCurrency: Result := stCurrency;
    adDBDate: Result := stDate;
    adDBTime: Result := stTime;
    adDate : Result := stDate;
    adDBTimeStamp, adFileTime: Result := stTimestamp;
    adLongVarChar: Result := stAsciiStream;
    adLongVarWChar: Result := stUnicodeStream;
    adBinary, adVarBinary: Result := stBytes;
    adLongVarBinary: Result := stBinaryStream;
    adGUID: Result := stGUID;
    adEmpty, AdArray, adChapter,
    adPropVariant, adUserDefined: Result := stString;
    adVariant: Result := stString;
  else
    {adIDispatch, adIUnknown: reserved, nut used tpyes}Result := stUnknown
  end;
end;

{**
  Converts a Zeos type into ADO types.
  @param FieldType zeos field type.
  @return a ADO datatype.
}
function ConvertSqlTypeToAdo(FieldType: TZSQLType): Integer;
begin
  case FieldType of
    stString, stUnicodeString: Result := adVarWChar;
    stBoolean: Result := adBoolean;
    stByte: Result := adUnsignedTinyInt;
    stShort: Result := adTinyInt;
    stWord: Result := adUnsignedSmallInt;
    stSmall: Result := adSmallInt;
    stLongWord: Result := adUnsignedInt;
    stInteger: Result := adInteger;
    stULong: Result := adUnsignedBigInt;
    stLong: Result := adBigInt;
    stCurrency: Result := adCurrency;
    stFloat: Result := adSingle;
    stDouble, stBigDecimal: Result := adDouble;
    stDate, stTime, stTimestamp: Result := adDate;
    stBytes: Result := adVarBinary;
    stGUID: Result := adGUID;
    stAsciiStream, stUnicodeStream: Result := adLongVarWChar;
    stBinaryStream: Result := adLongVarBinary;
  else
    Result := adEmpty;
  end;
end;

{**
  Converts a Variant type into ADO types.
  @param VT Variant datatype.
  @return a ADO datatype.
}
function ConvertVariantToAdo(VT: TVarType): Integer;
begin
  case VT and varTypeMask of
    varEmpty: Result := adEmpty;
    varNull: Result := adVarChar;
    varSmallint: Result := adSmallInt;
    varInteger: Result := adInteger;
    varSingle: Result := adSingle;
    varDouble: Result := adDouble;
    varCurrency: Result := adCurrency;
    varDate: Result := adDate;
    varOleStr: Result := adVarWChar;
    varDispatch: Result := adIDispatch;
    varError: Result := adError;
    varBoolean: Result := adBoolean;
    varVariant: Result := adVariant;
    varUnknown: Result := adIUnknown ;
    varShortInt: Result := adTinyInt;
    varByte: if (VT and varArray) <> 0 then Result := adLongVarBinary else Result := adUnsignedTinyInt;
    varWord: Result := adUnsignedSmallInt;
    varLongWord: Result := adUnsignedInt;
    varInt64: Result := adBigInt;
    varStrArg: Result := adWChar;
    varString: Result := adVarChar;
    {$IFDEF WITH_varUString}
    varUString: Result := adVarChar;
    {$ENDIF}
    varAny: Result := adEmpty;
  else
    Result := adEmpty;
  end;
end;


{**
  Converts a TZResultSetType type into ADO cursor type.
  @param ResultSetType.
  @return a ADO cursor type.
}
function ConvertResultSetTypeToAdo(ResultSetType: TZResultSetType): Integer;
begin
  case ResultSetType of
    rtForwardOnly: Result := adOpenForwardOnly;
    rtScrollInsensitive: Result := adOpenStatic;
    else {rtScrollSensitive:} Result := adOpenDynamic;
  end
end;

{**
  Converts a TZResultSetConcurrency type into ADO lock type.
  @param ResultSetConcurrency.
  @return a ADO lock type.
}
function ConvertResultSetConcurrencyToAdo(ResultSetConcurrency: TZResultSetConcurrency): Integer;
begin
  if ResultSetConcurrency = rcReadOnly
  then Result := adLockReadOnly
  else Result := adLockOptimistic;
end;

{**
  Converts a OLEDB schema guid into ADO schema ID usable with OpenSchema.
  @param OleDBSchema schema guid.
  @return a ADO schema id.
}
function ConvertOleDBToAdoSchema(OleDBSchema: TGUID): Integer;
begin
  Result := -1;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_ASSERTIONS) then Result := 0;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CATALOGS) then Result := 1;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CHARACTER_SETS) then Result := 2;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLLATIONS) then Result := 3;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMNS) then Result := 4;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CHECK_CONSTRAINTS) then Result := 5;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CONSTRAINT_COLUMN_USAGE) then Result := 6;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_CONSTRAINT_TABLE_USAGE) then Result := 7;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_KEY_COLUMN_USAGE) then Result := 8;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_REFERENTIAL_CONSTRAINTS) then Result := 9;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLE_CONSTRAINTS) then Result := 10;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMN_DOMAIN_USAGE) then Result := 11;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_INDEXES) then Result := 12;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_COLUMN_PRIVILEGES) then Result := 13;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLE_PRIVILEGES) then Result := 14;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_USAGE_PRIVILEGES) then Result := 15;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURES) then Result := 16;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_SCHEMATA) then Result := 17;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_SQL_LANGUAGES) then Result := 18;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_STATISTICS) then Result := 19;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TABLES) then Result := 20;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_TRANSLATIONS) then Result := 21;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROVIDER_TYPES) then Result := 22;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEWS) then Result := 23;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEW_COLUMN_USAGE) then Result := 24;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_VIEW_TABLE_USAGE) then Result := 25;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURE_PARAMETERS) then Result := 26;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_FOREIGN_KEYS) then Result := 27;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PRIMARY_KEYS) then Result := 28;
  if IsEqualGuid(OleDBSchema, DBSCHEMA_PROCEDURE_COLUMNS) then Result := 29;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_CUBES) then Result := 32;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_DIMENSIONS) then Result := 33;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_HIERARCHIES) then Result := 34;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_LEVELS) then Result := 35;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_MEASURES) then Result := 36;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_PROPERTIES) then Result := 37;
  if IsEqualGuid(OleDBSchema, MDSCHEMA_MEMBERS) then Result := 38;
  if IsEqualGuid(OleDBSchema, DBPROPSET_TRUSTEE) then Result := 39;
end;

function IsSelect(const SQL: string): Boolean;
var P, PEnd: PChar;
begin
  P := Pointer(SQL);
  if P = nil then
    Result := False
  else begin
    PEnd := P + Length(SQL);
    while (P < PEnd) and (Ord(P^) <= Ord (' ')) do Inc(P); //TrimLeft
    Result := ((PEnd - P) >= 6) and SameText(P, PChar('SELECT'), 6);
  end;
  //Result := Uppercase(Copy(TrimLeft(Sql), 1, 6)) = 'SELECT';
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure BCD2Decimal(const Value: TBCD; Dest: PDecimal);
var
  LastNibbleByteIDX, BCDScale, P, I, F: Byte;
  i64: UInt64;
  Negative, LastByteIsHalfByte: boolean;
begin
  LastNibbleByteIDX := (Value.Precision-1) shr 1;
  F := Value.SignSpecialPlaces;
  BCDScale := (F and 63);
  Negative := (F and $80) = $80;
  LastByteIsHalfByte := (Value.Precision and 1 = 1) or ((BCDScale and 1 = 1) and (Value.Fraction[LastNibbleByteIDX] and $0F = 0));
  P := 0;
  i64 := 0;
  { scan for leading zeroes to skip them }
  if LastNibbleByteIDX > 0 then begin
    for I := 0 to LastNibbleByteIDX do begin
      F := Value.Fraction[i];
      if F = 0
      then Inc(P)
      else begin
        i64 := ZBcdNibble2Base100ByteLookup[F];
        Break;
      end;
    end
  end;
  { initialize the Result }
  if P < LastNibbleByteIDX then begin
    for I := P+1 to LastNibbleByteIDX-Ord(LastByteIsHalfByte) do
      i64 := i64 * 100 + ZBcdNibble2Base100ByteLookup[Value.Fraction[i]];
    { last half nibble byte}
    if LastByteIsHalfByte then begin
      i64 := i64 * 10 + Value.Fraction[P+LastNibbleByteIDX] shr 4;
      if (BCDScale and 1 = 1) and (Value.Precision and 1 = 0) then
        Dec(BCDScale);
    end;
    if negative then
      Dest.Sign := 1;
  end;
  Dest.Scale := BCDScale;
  {$IFDEF FPC}
  Dest.Lo64 := i64; //correct translated
  {$else}
  PUint64(@Dest.Lo64)^ := i64;
  {$ENDIF}
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

initialization
  OleCheck(CoGetMalloc(1, ZAdoMalloc));
finalization
  ZAdoMalloc := nil;
{$ENDIF ZEOS_DISABLE_ADO}
end.
