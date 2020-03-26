{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Connectivity Functions              }
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

unit ZDbcUtils;

interface

{$I ZDbc.inc}
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} TypInfo, FmtBcd,
  ZCompatibility, ZDbcIntfs, ZClasses, ZTokenizer, ZVariant, ZSysUtils,
  ZDbcResultSetMetadata;

const SQL_MAX_NUMERIC_LEN = 16;
type
  TPreparablePrefixToken = Record
    MatchingGroup: String;
    ChildMatches: TStringDynArray;
  end;
  PPreparablePrefixTokens = ^TPreparablePrefixTokens;
  TPreparablePrefixTokens = array of TPreparablePrefixToken;

  PRawBuff = ^TRawBuff;
  TRawBuff = record
    Pos: Word;
    Buf: array[Byte] of AnsiChar;
  end;

  TUCS2Buff = record
    Pos: Word;
    Buf: array[Byte] of WideChar;
  end;

  TBCDDynArray = array of TBCD;

  PDB_NUMERIC = ^TDB_NUMERIC;
  TDB_NUMERIC = record { oledb&odbc little enadian / dblib big endian}
    precision:  Byte;
    scale:      Byte;
    sign:       Byte; {1 if positive, 0 if negative }
    val:        array[0..SQL_MAX_NUMERIC_LEN -1] of BYTE; //fixed len
  end;

  TZVariantTypes = set of TZVariantType;

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SuupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(const Url: string;
  const SupportedProtocols: TStringDynArray): string;

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(const Statement: IZStatement;
  const ParamName: string; const Default: string): string; overload;

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param an info list for the lookups.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(const Connection: IZConnection;
  const StmtInfo: TStrings; const ParamName: string;
  const Default: string): string; overload;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);

{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Len then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}
function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;

{$IF DEFINED(ENABLE_DBLIB) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
(** written by EgonHugeist
  converts a sql numeric value into a <code>java.math.BigDecimal</code>
  @param Src the pointer to a valid oledn DB_(VAR)NUMERIC struct which to be converted
  @param Dest the <code>java.math.BigDecimal</code> value which should be filled
  @param NumericLen the count of value digits of the numeric
*)
procedure SQLNumeric2BCD(Src: PDB_NUMERIC; var Dest: TBCD; NumericLen: Integer);

(** written by EgonHugeist
  converts a <code>java.math.BigDecimal</code> value into a oledb DB_(VAR)NUMERIC
  @param Dest the pointer to a valid oledn DB_(VAR)NUMERIC struct which to be converted
  @param Src the <code>java.math.BigDecimal</code> value which should be filled
*)
procedure BCD2SQLNumeric(const Src: TBCD; Dest: PDB_NUMERIC);

(** written by EgonHugeist
  converts an oledb DB_(VAR)NUMERIC into a raw string buffer
  @param Src the DB_(VAR)NUMERIC value which should be converted
  @param Dest the pointer to the raw buffer we write in
  @param NumericLen fill the length in bytes of the converted value
*)
procedure SQLNumeric2Raw(Src: PDB_NUMERIC; Dest: PAnsiChar; var NumericLen: NativeUint);

(** written by EgonHugeist
  converts an oledb DB_(VAR)NUMERIC into a utf16 string buffer
  @param Src the DB_(VAR)NUMERIC value which should be converted
  @param Dest the pointer to the utf16 buffer we write in
  @param NumericLen fill the length in words of the converted value
*)
procedure SQLNumeric2Uni(Src: PDB_NUMERIC; Dest: PWideChar; var NumericLen: NativeUint);

(** written by EgonHugeist
  converts a DB_NUMERIC value with litte endian order value into a native currency value
  @param src the pointer to a valid DB_NUMERIC struct which to be converted
  @param NumericNegSign the value which represents a negative dbnumeric value
  @return the converted currency value
*)
function DBNumeric2Curr_LE(Src: PDB_NUMERIC; NumericNegSign: Byte): Currency;
(** written by EgonHugeist
  converts a DB_NUMERIC value with big endian order value into a native currency value
  @param src the pointer to a valid DB_NUMERIC struct which to be converted
  @param NumericNegSign the value which represents a negative dbnumeric value
  @return the converted currency value
*)
function DBNumeric2Curr_BE(Src: PDB_NUMERIC; NumericNegSign: Byte): Currency;

type TNumericSign = array[Boolean] of Byte;  //true represents a negative index
const ZeroIsNegativeOneIsPositive: TNumericSign = (1, 0);
const OneIsNegativeZeroIsPositive: TNumericSign = (0, 1);

procedure Curr2DBNumeric_LE(const Src: Currency; Dest: PDB_NUMERIC; const NumericSign: TNumericSign);
procedure Curr2DBNumeric_BE(const Src: Currency; Dest: PDB_NUMERIC; const NumericSign: TNumericSign);
{$IFEND}

procedure MoveReverseByteOrder(Dest, Src: PAnsiChar; Len: LengthInt);

function TokenizeSQLQueryRaw(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex: TBooleanDynArray;
  IsNCharIndex: PBooleanDynArray; ComparePrefixTokens: PPreparablePrefixTokens;
  var TokenMatchIndex: Integer): TRawByteStringDynArray;

function TokenizeSQLQueryUni(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex: TBooleanDynArray;
  IsNCharIndex: PBooleanDynArray; ComparePrefixTokens: PPreparablePrefixTokens;
  var TokenMatchIndex: Integer): TUnicodeStringDynArray;

function ExtractFields(const FieldNames: string; const SepChars: Array of Char): TStrings;


{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString; overload; //deprecated;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString; overload; //deprecated;

{**
  Set the string-types conversion funtion in relation to the Connection-Settings.
  The Results should be as optimal as possible to speed up the behavior
  @param ConSettings a Pointer to the ConnectionSetting
}
procedure SetConvertFunctions(ConSettings: PZConSettings);

function CreateUnsupportedParameterTypeException(Index: Integer; ParamType: TZSQLType): EZSQLException;

function IsNullFromArray(ZArray: PZArray; Index: Cardinal): Boolean;

procedure ToBuff(const Value: RawByteString; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(Value: Pointer; L: LengthInt; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(Value: AnsiChar; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(const Value: ZWideString; var Buf: TUCS2Buff; var Result: ZWideString); overload;
procedure ToBuff(Value: WideChar; var Buf: TUCS2Buff; var Result: ZWideString); overload;

procedure ReplaceOrAddLastChar(cOld, cNew: AnsiChar; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ReplaceOrAddLastChar(cOld, cNew: WideChar; var Buf: TUCS2Buff; var Result: ZWideString); overload;

procedure CancelLastChar(var Buf: TRawBuff; var Result: RawByteString); overload;
procedure CancelLastChar(var Buf: TUCS2Buff; var Result: ZWideString); overload;

procedure FlushBuff(var Buf: TRawBuff; var Result: RawByteString); overload;
procedure FlushBuff(var Buf: TUCS2Buff; var Result: ZWideString); overload;

function GetAbsorbedTrailingSpacesLen(Buf: PAnsiChar; Len: LengthInt): LengthInt; {$IFDEF WITH_INLINE}inline;{$ENDIF} overload;
function GetAbsorbedTrailingSpacesLen(Buf: PWideChar; Len: LengthInt): LengthInt; {$IFDEF WITH_INLINE}inline;{$ENDIF} overload;

procedure ReferenceArray(aArray: Pointer; Var Dest: Pointer; SQLType: TZSQLType; VariantType: TZVariantType);
procedure DeReferenceArray(Var Dest: Pointer; SQLType: TZSQLType; VariantType: TZVariantType);

function ArrayValueToInteger(ZArray: PZArray; Index: Integer): Integer;
function ArrayValueToCardinal(ZArray: PZArray; Index: Integer): Cardinal;
function ArrayValueToInt64(ZArray: PZArray; Index: Integer): Int64;
function ArrayValueToUInt64(ZArray: PZArray; Index: Integer): UInt64;
function ArrayValueToCurrency(ZArray: PZArray; Index: Integer): Currency;
function ArrayValueToDouble(ZArray: PZArray; Index: Integer): Double;
function ArrayValueToBoolean(ZArray: PZArray; Index: Integer): Boolean;
function ArrayValueToDate(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
function ArrayValueToTime(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
function ArrayValueToDateTime(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
procedure ArrayValueToGUID(ZArray: PZArray; Index: Integer; GUID: PGUID);
procedure ArrayValueToBCD(ZArray: PZArray; Index: Integer; var BCD: TBCD);

function CharRecArray2UnicodeStrArray(const Value: TZCharRecDynArray; var MaxLen: LengthInt): TUnicodeStringDynArray; overload;
function CharRecArray2UnicodeStrArray(const Value: TZCharRecDynArray): TUnicodeStringDynArray; overload;

function CreateCanNotAccessBlobRecordException(ColumnIndex: Integer; SQLType: TZSQLType): EZSQLException;
function CreateWriteOnlyException: EZSQLException;

{**
  creates an "operation is not allowed in READ ONLY mode" exception.
}
function CreateReadOnlyException: EZSQLException;
function CreateBinaryException: EZSQLException;
function CreateNonBinaryException: EZSQLException;
function CreateConversionError(ColumnIndex: Integer; Actual, Expected: TZSQLType): EZSQLException;

const
  i4SpaceRaw: Integer = Ord(#32)+Ord(#32) shl 8 + Ord(#32) shl 16 +Ord(#32) shl 24;  //integer representation of the four space chars
  i4SpaceUni: Int64 = 9007336695791648;  //integer representation of the four wide space chars
  sAlignCurrencyScale2Precision: array[0..4] of Integer = (
    15, 16, 17, 18, 19);
  ZSQLTypeToBuffSize: array[TZSQLType] of Integer = (0,//stUnknown,
    //fixed size DataTypes first
    SizeOf(WordBool),
    SizeOf(Byte), SizeOf(ShortInt), SizeOf(Word), SizeOf(SmallInt), SizeOf(Cardinal), SizeOf(Integer), SizeOf(UInt64), SizeOf(Int64),  //ordinals
    SizeOf(Single), SizeOf(Double), SizeOf(Currency), SizeOf(TBcd),
    SizeOf(TZDate), SizeOf(TZTime), SizeOf(TZTimeStamp),
    SizeOf(TGUID),
    //now varying size types in equal order
    0,0,0,//stString, stUnicodeString, stBytes,
    0,0,0,//stAsciiStream, stUnicodeStream, stBinaryStream,
    //finally the object types
    0,0//stArray, stDataSet
    );
  NativeArrayValueTypes: array[TZSQLType] of TZVariantTypes = ([],
    [vtNull, vtBoolean],
    [vtNull, vtUInteger], [vtNull, vtInteger], [vtNull, vtUInteger], [vtNull, vtInteger], [vtNull, vtUInteger], [vtNull, vtInteger], [vtNull, vtUInteger], [vtNull, vtInteger],  //ordinals
    [vtNull], [vtNull, vtDouble], [vtNull, vtCurrency], [vtNull, vtBigDecimal], //floats
    [vtNull, vtDateTime], [vtNull, vtDateTime], [vtNull, vtDateTime],
    [vtNull, vtGUID],
    //now varying size types in equal order
    [], [], [vtNull, vtBytes],
    [vtNull, vtInterface], [vtNull, vtInterface], [vtNull, vtInterface],
    //finally the object types
    [], []);

implementation

uses ZMessages, ZEncoding, ZFastCode, ZGenericSqlToken, Math;
  //{$IFNDEF NO_UNIT_CONTNRS}, ZClasses{$ENDIF};

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(const Url: string;
  const SupportedProtocols: TStringDynArray): string;
var
  I: Integer;
  Protocol: string;
  Index: Integer;
begin
  Result := '';

  Index := FirstDelimiter(':', Url);
  if Index > 0 then
    Protocol := Copy(Url, Index + 1, Length(Url) - Index)
  else
    Protocol := '';
  Index := FirstDelimiter(':', Protocol);
  if Index > 1 then
    Protocol := Copy(Protocol, 1, Index - 1)
  else
    Protocol := '';

  if Protocol = '' then
    raise EZSQLException.Create(Format(SIncorrectConnectionURL, [Url]));

  for I := Low(SupportedProtocols) to High(SupportedProtocols) do
  begin
    if SupportedProtocols[I] = Protocol then
    begin
      Result := Protocol;
      Break;
    end;
  end;

  if Result = '' then
    raise EZSQLException.Create(Format(SUnsupportedProtocol, [Protocol]));
end;

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;
begin
  case ResultType of
    stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stCurrency, stBigDecimal:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString];
    stDouble:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString,
        stTime, stDate, stTimeStamp];
    stString, stUnicodeString:
      Result := True;
    stBytes:
      Result := InitialType in [stString, stUnicodeString, stBytes, stGUID,
        stAsciiStream, stUnicodeStream, stBinaryStream];
    stTimestamp:
      Result := InitialType in [stString, stUnicodeString, stDate, stTime, stTimestamp, stDouble];
    stDate:
      Result := InitialType in [stString, stUnicodeString, stDate, stTimestamp, stDouble];
    stTime:
      Result := InitialType in [stString, stUnicodeString, stTime, stTimestamp, stDouble];
    stBinaryStream:
      Result := (InitialType in [stBinaryStream, stBytes]);
    stAsciiStream, stUnicodeStream:
      Result := (InitialType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]);
    else
      Result := (ResultType = InitialType) and (InitialType <> stUnknown);
  end;
end;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;
begin
  case ColumnType of
    stBoolean:        Result := 'Boolean';
    stByte:           Result := 'Byte';
    stShort:          Result := 'Short';
    stWord:           Result := 'Word';
    stSmall:          Result := 'Small';
    stLongWord:       Result := 'LongWord';
    stInteger:        Result := 'Integer';
    stULong:          Result := 'ULong';
    stLong:           Result := 'Long';
    stFloat:          Result := 'Float';
    stDouble:         Result := 'Double';
    stCurrency:       Result := 'Currency';
    stBigDecimal:     Result := 'BigDecimal';
    stString:         Result := 'String';
    stUnicodeString:  Result := 'UnicodeString';
    stBytes:          Result := 'Bytes';
    stGUID:           Result := 'GUID';
    stDate:           Result := 'Date';
    stTime:           Result := 'Time';
    stTimestamp:      Result := 'Timestamp';
    stAsciiStream:    Result := 'AsciiStream';
    stUnicodeStream:  Result := 'UnicodeStream';
    stBinaryStream:   Result := 'BinaryStream';
    stArray:          Result := 'Array';
    stDataSet:        Result := 'DataSet';
    else
      Result := 'Unknown';
  end;
end;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);
begin
  if E is EZSQLException then
    raise EZSQLException.CreateClone(EZSQLException(E))
  else
    raise EZSQLException.Create(E.Message);
end;

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);
var
  I: Integer;
  Current: TZColumnInfo;
  ColumnInfo: TZColumnInfo;
begin
  for I := 0 to FromList.Count - 1 do
  begin
    Current := TZColumnInfo(FromList[I]);
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.AutoIncrement := Current.AutoIncrement;
    ColumnInfo.CaseSensitive := Current.CaseSensitive;
    ColumnInfo.Searchable := Current.Searchable;
    ColumnInfo.Currency := Current.Currency;
    ColumnInfo.Nullable := Current.Nullable;
    ColumnInfo.Signed := Current.Signed;
    ColumnInfo.ColumnLabel := Current.ColumnLabel;
    ColumnInfo.ColumnName := Current.ColumnName;
    ColumnInfo.SchemaName := Current.SchemaName;
    ColumnInfo.Precision := Current.Precision;
    ColumnInfo.Scale := Current.Scale;
    ColumnInfo.TableName := Current.TableName;
    ColumnInfo.CatalogName := Current.CatalogName;
    ColumnInfo.ColumnType := Current.ColumnType;
    ColumnInfo.ReadOnly := Current.ReadOnly;
    ColumnInfo.Writable := Current.Writable;
    ColumnInfo.DefinitelyWritable := Current.DefinitelyWritable;
    ColumnInfo.ColumnCodePage := Current.ColumnCodePage;

    ToList.Add(ColumnInfo);
  end;
end;

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(const Statement: IZStatement; const ParamName: string;
  const Default: string): string;
begin
  Result := Statement.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Statement.GetConnection.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Default;
end;

function DefineStatementParameter(const Connection: IZConnection;
  const StmtInfo: TStrings; const ParamName: string;
  const Default: string): string;
begin
  Result := '';
  if StmtInfo <> nil then
    Result := StmtInfo.Values[ParamName];
  if (Result = '') then
    Result := Connection.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Default;
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);
var
  SL: TStrings;
  I: Integer;
begin
  SL := ZSysUtils.SplitString(QualifiedName, '.');
  try
    Catalog := '';
    Schema := '';
    case SL.Count of
      0, 1: ObjectName := QualifiedName;
      2: begin
          if SupportsCatalogs then begin
            if SupportsSchemas
            then Schema := SL.Strings[0]
            else Catalog := SL.Strings[0];
            ObjectName := SL.Strings[1];
          end else if SupportsSchemas then begin
            Schema := SL.Strings[0];
            ObjectName := SL.Strings[1];
          end else
            ObjectName := SL.Strings[0]+'.'+SL.Strings[1];
        end;
      3: if SupportsCatalogs then begin
          Catalog := SL.Strings[0];
          if SupportsSchemas then begin
            Schema := SL.Strings[1];
            ObjectName := SL.Strings[2]
          end else
            ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
        end else if SupportsSchemas then begin
          Schema := SL.Strings[0];
          ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
        end else
          ObjectName := SL.Strings[0]+'.'+SL.Strings[1]+'.'+SL.Strings[2];
      else if SupportsCatalogs then begin
        Catalog := SL.Strings[0];
        if SupportsSchemas then begin
          Schema := SL.Strings[1];
          for i := 2 to SL.Count-1 do
            if i = 2
            then ObjectName := SL.Strings[i]
            else ObjectName := ObjectName+'.'+SL.Strings[i];
        end else begin
          ObjectName := '';
          for i := 2 to SL.Count-1 do
            if I = 2
            then ObjectName := SL.Strings[i]
            else ObjectName := ObjectName+'.'+SL.Strings[i];
        end;
      end else if SupportsSchemas then begin
        Schema := SL.Strings[0];
        for i := 1 to SL.Count-1 do
          if i = 1
          then ObjectName := SL.Strings[i]
          else ObjectName := ObjectName+'.'+SL.Strings[i];
      end else
        for i := 0 to SL.Count-1 do
          if I = 0
          then ObjectName := SL.Strings[i]
          else ObjectName := ObjectName+'.'+SL.Strings[i];
    end;
  finally
    SL.Free;
  end;
end;
{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;
begin
  if Value = '' then
    Result := '%'
  else
    Result := Value;
end;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Length then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}

function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
var P: PWideChar;
begin
  ZSetString(nil, ((Len+1) shl 1)+Ord(not Odbc), Result{%H-});
  if ODBC then begin
    P := Pointer(Result);
    Word(P^) := Ord('0');
    Word((P+1)^) := Ord('x');
    Inc(P, 2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
  end else begin
    P := Pointer(Result);
    Word(P^) := Ord('x');
    Word((P+1)^) := Ord(#39);
    Inc(P,2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    Word(P^) := Word(#39);
  end;
end;

function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
var P: PAnsiChar;
begin
  ZSetString(nil, ((Len+1) shl 1)+Ord(not Odbc), Result{%H-});
  if ODBC then begin
    P := Pointer(Result);
    Byte(P^) := Ord('0');
    Byte((P+1)^) := Ord('x');
    Inc(P, 2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
  end else begin
    P := Pointer(Result);
    Byte(P^) := Ord('x');
    Byte((P+1)^) :=Ord(#39);
    Inc(P,2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    Byte(P^) := Ord(#39);
  end;
end;

function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;
begin
  {$IFDEF UNICODE}
  Result := GetSQLHexWideString(Value, Len, ODBC);
  {$ELSE}
  Result := GetSQLHexAnsiString(Value, Len, ODBC);
  {$ENDIF}
end;

{$IF DEFINED(ENABLE_DBLIB) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
(** EgonHugeist prolog:
  i didn't found any description/documentation how to work the the ole numerics.
  After some tests like PUInt64(@TestNum.Val[0])^:
    testNum: TDB_NUMERIC = (Precision: 18; Scale: 1; Sign: 1;
      val: (78, 243, 48, 166, 75, 155, 182, 1, 0, 0, 0, 0, 0, 0, 0, 0));
  i found out all byte are a multiple of 16 starting with 1. Byte order
  is Endian_little. Same as ordinals are stored on a Win-OS.
  But we've more than 8 Bytes. Encode it into the BCD is messy and not fast.
  Each byte need to be recalculated again because we need the modula of 100 for the nibbles.
  So i need a local copy of the bytes first.
  Also is there no precise Nibble position possible -> which means i'd to start
  from last nibble down to first niblle and move all data afterwards.
  If someone finds a faster way ... please let me know it!

  converts a oledb DB_(VAR)NUMERIC value into a <code>java.math.BigDecimal</code>
  @param Src the pointer to a valid oledn DB_(VAR)NUMERIC struct which to be converted
  @param Dest the <code>java.math.BigDecimal</code> value which should be filled
  @param NumericLen the count of value digits of the numeric
*)
procedure SQLNumeric2BCD(Src: PDB_NUMERIC; var Dest: TBCD; NumericLen: Integer);
var
  Remainder, NextDigit, Precision, Scale: Word;
  remPrecision: SmallInt absolute NextDigit;
  signPrecision: SmallInt absolute Precision;
  NumericVal: array [0..SQL_MAX_NUMERIC_LEN - 1] of Byte;
  pDigitCopy, pNumDigit, pNibble, pFirstNibble, pLastNibble: PAnsiChar;
  ValueIsOdd: Boolean;
begin
  // check for zero value and padd trailing zeroes away to reduce the main loop
  pNumDigit := @Src.val[0];
  pNibble := pNumDigit + NumericLen-1;
  pFirstNibble := @Dest.Fraction[0];
  while (pNibble >= pNumDigit) and (PByte(pNibble)^ = 0) do
    Dec(pNibble);
  if pNibble < pNumDigit then begin //zero value
    PCardinal(@Dest.Precision)^ := ZInitZeroBCD; //not the delphi default but the fastest bcd-rec to represent a zero value
    Exit;
  end;
  { prepare local buffer }
  NumericLen := (pNibble - pNumDigit);
  if NumericLen >= SQL_MAX_NUMERIC_LEN
  then GetMem(pDigitCopy, NumericLen+1)
  else pDigitCopy := @NumericVal[0];
  Move(pNumDigit^, pDigitCopy^, NumericLen+1); //localize all bytes for next calculation loop.
  Precision := Src.precision;
  Scale := Src.scale;
  { calcutate precision }
  if Src.scale > Precision then begin
    NextDigit := Src.scale - Src.precision;
    Precision := Precision + NextDigit;
    FillChar(Dest.Fraction, MaxFMTBcdDigits, #0);
  end;
  ValueIsOdd := Precision and 1 = 1; //indicate how we write into the buffer
  pLastNibble := pFirstNibble + MaxFMTBcdDigits -1;
  pNibble := pFirstNibble + ((Precision-1) shr 1); { address last bcd nibble we write in}
  if ValueIsOdd then begin
    PByte(pNibble)^ := 0; //clear last nibble
    Dec(pNibble);
  end;
  while NumericLen >= 0 do begin //outer bcd filler loop
    Remainder := 0;
    pNumDigit := pDigitCopy+Cardinal(NumericLen);
    while pNumDigit > pDigitCopy do begin //inner digit calc loop
      NextDigit := PByte(pNumDigit)^ + Remainder;
      PByte(pNumDigit)^ := NextDigit div 100;
      Remainder := (NextDigit - (PByte(pNumDigit)^ * 100) {mod 100}) shl 8;
      Dec(pNumDigit);
    end;
    NextDigit := PByte(pNumDigit)^ + Remainder;
    PByte(pNumDigit)^ := NextDigit div 100;
    Remainder := ZBase100Byte2BcdNibbleLookup[NextDigit - (PByte(pNumDigit)^ * 100){mod 100}];
    if PNibble <= PLastNibble then //overflow save
      if ValueIsOdd then begin //my new lookup version with bool algebra only
        PByte(pNibble+1)^ := PByte(pNibble+1)^ or ((Byte(Remainder) and $0F) shl 4);
        PByte(pNibble)^   := (Byte(Remainder) shr 4);
      end else
        PByte(pNibble)^   := Byte(Remainder)
    else begin
      Dec(Precision, 2);
      if Scale > 1 then
        Dec(Scale, 2)
      else if Scale > 0 then
        Dec(Scale);
    end;
    Dec(pNibble);
    if PByte(pDigitCopy+NumericLen)^ = 0 then
      Dec(NumericLen); //as long we've no zero we've to loop again
  end;
  Inc(pNibble, 1+Ord(ValueIsOdd));
  pLastNibble := pFirstNibble + ((Precision+1) shr 1)-1; { address last bcd nibble }
  Precision := (PLastNibble +1 - PNibble) shl 1 - Ord(ValueIsOdd);
  {left pack the Bcd fraction }
  remPrecision := Precision - Src.Scale;
  while (signPrecision >= remPrecision) do begin
    if ValueIsOdd and (PByte(pNibble)^ and $0F = 0) then
      Inc(pNibble)
    else if not (not ValueIsOdd and (PByte(pNibble)^ shr 4 = 0)) then
      Break;
    Dec(Precision);
    ValueIsOdd := not ValueIsOdd;
  end;
  {move or left pact the fraction}
  ValueIsOdd := (PByte(pNibble)^ shr 4 = 0);
  if (PNibble > PNumDigit) or ValueIsOdd then begin {move nibbles foreward}
    PNumDigit := PFirstNibble;
    while (pNibble <= pLastNibble) do begin
      if (PNibble > PNumDigit) then
        PByte(PNumDigit)^ := PByte(pNibble)^;
      if ValueIsOdd then begin
        if PNumDigit < pLastNibble then
          PByte(PNumDigit)^ := Byte((PByte(PNumDigit)^ and $0f) shl 4) or Byte(PByte(pNibble+1)^ shr 4);
      end;
      Inc(PNumDigit);
      Inc(pNibble);
    end;
  end;
  {right pack the Bcd scale fraction }
  pLastNibble := pFirstNibble + ((Precision+1) shr 1)-1; { address last bcd nibble }
  ValueIsOdd := Precision and 1 = 1;
  while Scale > 0 do begin
    if ValueIsOdd and (PByte(PLastNibble)^ shr 4 = 0) then
      Dec(pLastNibble)
    else if not (not ValueIsOdd and ((PByte(PLastNibble)^ and $0F) = 0)) then
      Break;
    Dec(Precision);
    Dec(Scale);
    ValueIsOdd := not ValueIsOdd;
  end;
  Dest.Precision := Precision;
  if Src.sign = 0 then //negative ?
    Scale := Scale + (1 shl 7);
  Dest.SignSpecialPlaces := Scale;
  if Pointer(pDigitCopy) <> Pointer(@NumericVal[0]) then
    FreeMem(pDigitCopy);
end;

Type
  TSQLDigit = {$IFDEF CPU64}Cardinal{$ELSE}Word{$ENDIF}; //Byte
  PSQLDigitArray = ^TSQLDigitArray;
  TSQLDigitArray = array[0..SQL_MAX_NUMERIC_LEN] of TSQLDigit;

  TDoubleSQLDigit = NativeUInt; //Word

  POleDBMultiplyLookup = ^TOleDBMultiplyLookup;
  TOleDBMultiplyLookup = record
    MultiplierCount: TSQLDigit;
    ByteCount: TSQLDigit;
    Values: array[0..(MaxFMTBcdDigits div SizeOf(TSQLDigit)) - 1] of TSQLDigit;
  end;
const
  FlushHalfDoubleDigit: TSQLDigit = TSQLDigit(-1);
  ShrSQLDigit = SizeOf(TSQLDigit) * 8;
var
  DBNumMultiplyLookup: array[Boolean, 1..MaxFMTBcdDigits-1] of TOleDBMultiplyLookup;

(** EH:
  converts a <code>java.math.BigDecimal</code> value into a oledb DB_(VAR)NUMERIC
  @param Dest the pointer to a valid oledn DB_(VAR)NUMERIC struct which to be converted
  @param Src the <code>java.math.BigDecimal</code> value which should be filled
*)
procedure BCD2SQLNumeric(const Src: TBCD; Dest: PDB_NUMERIC);
var PNibble, pLastNibble, pFirstNibble: PAnsiChar;
  Base100Digit, Carry, I: TSQLDigit;
  NextVal: TDoubleSQLDigit;
  pValues: PSQLDigitArray;
  PrecisionIsEven: Boolean;
  MultiplyLookup: POleDBMultiplyLookup;
begin
  pValues := @Dest.val[0];
  FillChar(pValues^, SQL_MAX_NUMERIC_LEN, #0);
  Dest.precision := Src.Precision;
  Dest.scale := Src.SignSpecialPlaces and $3F;
  Dest.sign := Byte(Src.SignSpecialPlaces and (1 shl 7) = 0);

  pFirstNibble := @Src.Fraction[0];
  pLastNibble := pFirstNibble+((Src.Precision -1) shr 1);
  pNibble := pLastNibble-1;
  { init first byte }
  if (Src.Precision and 1) = 0 then begin
    pValues[0] := ZBcdNibble2Base100ByteLookup[PByte(pLastNibble)^];
    PrecisionIsEven := True;
  end else begin
    pValues[0] := PByte(pLastNibble)^ shr 4;
    PrecisionIsEven := False;
  end;

  while pNibble >= pFirstNibble do begin
    Base100Digit := ZSysUtils.ZBcdNibble2Base100ByteLookup[PByte(pNibble)^];
    Carry := 0;
    MultiplyLookup := @DBNumMultiplyLookup[PrecisionIsEven][pLastNibble-pNibble];
    for I := 0 to MultiplyLookup.MultiplierCount - 1 do begin
      NextVal := pValues[I] + TDoubleSQLDigit(MultiplyLookup.Values[i]) * Base100Digit + Carry;
      pValues[I] := TSQLDigit(NextVal and FlushHalfDoubleDigit);
      Carry := NextVal shr ShrSQLDigit;
    end;
    if Carry <> 0 then
      pValues[MultiplyLookup.MultiplierCount] := Carry;
    Dec(pNibble);
  end;
end;

procedure SQLNumeric2Raw(Src: PDB_NUMERIC; Dest: PAnsiChar; var NumericLen: NativeUInt);
var
  Remainder, NextDigit: Word;
  NumericVal: array [0..SQL_MAX_NUMERIC_LEN - 1] of Byte;
  pDigit, pDigitCopy, pNumDigit, pLastDigit: PAnsiChar;
label MainLoop, Done;
begin
  pNumDigit := @Src.val[0];
  pLastDigit := pNumDigit + (NumericLen -1);
  // check for zero value and padd trailing zeroes away to reduce the main loop
  while (pLastDigit >= pNumDigit) and (PByte(pLastDigit)^ = 0) do
    Dec(pLastDigit);
  if pLastDigit < pNumDigit then begin
    PByte(Dest)^ := Ord('0');
    NumericLen := 1;
    Exit;
  end;

  { prepare local digit buffer }
  NumericLen := (pLastDigit - pNumDigit);
  if NumericLen >= SQL_MAX_NUMERIC_LEN
  then GetMem(pDigitCopy, NumericLen+1)
  else pDigitCopy := @NumericVal[0];
  Move(pNumDigit^, pDigitCopy^, NumericLen+1); //localize all bytes for next calculation loop.

  if Src.scale > Src.precision //normalize precision
  then pDigit := Dest+(Src.precision +2 + (Src.scale - Src.precision))
  else pDigit := Dest+ Src.precision +2;
  pLastDigit := pDigit;

MainLoop: //outer digit filler loop
  Remainder := 0;
  pNumDigit := pDigitCopy+NumericLen;
  while pNumDigit > pDigitCopy do begin //inner digit calc loop
    NextDigit := PByte(pNumDigit)^ + Remainder;
    PByte(pNumDigit)^ := NextDigit div 100;
    Remainder := (NextDigit - (PByte(pNumDigit)^ * 100) {mod 100}) shl 8;
    Dec(pNumDigit);
  end;
  NextDigit := PByte(pNumDigit)^ + Remainder;
  PByte(pNumDigit)^ := NextDigit div 100;
  Remainder := NextDigit - (PByte(pNumDigit)^ * 100); //mod 100
  Dec(pDigit, 2);
  PWord(pDigit)^ := TwoDigitLookupW[Remainder];
  { as long we've no zero we've to loop again }
  if PByte(pDigitCopy+NumericLen)^ = 0 then
    if NumericLen > 0
    then Dec(NumericLen)
    else goto Done;
  goto MainLoop;
Done:
  if Src.scale < Src.Precision then
    Inc(pDigit, Ord(PByte(pDigit)^ = Ord('0')))
  else if PByte(pDigit)^ <> Ord('0') then begin
    Dec(pDigit);
    PByte(pDigit)^ := Ord('0');
  end;
  if Src.sign = 0 then begin//negative ?
    Dec(pDigit);
    PByte(pDigit)^ := Ord('-');
  end;
  NumericLen := pLastDigit-pDigit;
  Move(pDigit^, Dest^, (NumericLen-Src.scale));
  if Src.scale > 0 then begin
    pByte(Dest+(NumericLen-Src.scale))^ := Ord('.');
    Move((pLastDigit-Src.scale)^, (Dest+(NumericLen-Src.scale)+1)^,Src.scale);
    Inc(NumericLen);
  end;
  //free possibly allocated mem
  if Pointer(pDigitCopy) <> Pointer(@NumericVal[0]) then
    FreeMem(pDigitCopy);
end;

procedure SQLNumeric2Uni(Src: PDB_NUMERIC; Dest: PWideChar; var NumericLen: NativeUInt);
var
  Remainder, NextDigit: Word;
  NumericVal: array [0..SQL_MAX_NUMERIC_LEN - 1] of Byte;
  pDigitCopy, pNumDigit, pLastDigit: PAnsiChar;
  pDigit: PWideChar;
label MainLoop, Done;
begin
  pNumDigit := @Src.val[0];
  pLastDigit := pNumDigit + (NumericLen -1);
  // check for zero value and padd trailing zeroes away to reduce the main loop
  while (pLastDigit >= pNumDigit) and (PByte(pLastDigit)^ = 0) do
    Dec(pLastDigit);
  if pLastDigit < pNumDigit then begin
    PWord(Dest)^ := Ord('0');
    NumericLen := 1;
    Exit;
  end;

  { prepare local digit buffer }
  NumericLen := (pLastDigit - pNumDigit);
  if NumericLen >= SQL_MAX_NUMERIC_LEN
  then GetMem(pDigitCopy, NumericLen+1)
  else pDigitCopy := @NumericVal[0];
  Move(pNumDigit^, pDigitCopy^, NumericLen+1); //localize all bytes for next calculation loop.

  if Src.scale > Src.precision //normalize precision
  then pDigit := Dest+(Src.precision + 2 + (Src.scale - Src.precision))
  else pDigit := Dest+ Src.precision + 2;
  pLastDigit := Pointer(pDigit);

MainLoop: //outer digit filler loop
  Remainder := 0;
  pNumDigit := pDigitCopy+NumericLen;
  while pNumDigit > pDigitCopy do begin //inner digit calc loop
    NextDigit := PByte(pNumDigit)^ + Remainder;
    PByte(pNumDigit)^ := NextDigit div 100;
    Remainder := (NextDigit - (PByte(pNumDigit)^ * 100) {mod 100}) shl 8;
    Dec(pNumDigit);
  end;
  NextDigit := PByte(pNumDigit)^ + Remainder;
  PByte(pNumDigit)^ := NextDigit div 100;
  Remainder := NextDigit - (PByte(pNumDigit)^ * 100); //mod 100
  Dec(pDigit, 2);
  PCardinal(pDigit)^ := TwoDigitLookupLW[Remainder];
  { as long we've no zero we've to loop again }
  if PByte(pDigitCopy+NumericLen)^ = 0 then
    if NumericLen > 0
    then Dec(NumericLen)
    else goto Done;
  goto MainLoop;
Done:
  if Src.scale < Src.Precision then
    Inc(pDigit, Ord(PWord(pDigit)^ = Ord('0')))
  else if PWord(pDigit)^ <> Ord('0') then begin
    Dec(pDigit);
    PWord(pDigit)^ := Ord('0');
  end;
  if Src.sign = 0 then begin//negative ?
    Dec(pDigit);
    PWord(pDigit)^ := Ord('-');
  end;
  NumericLen := PWideChar(pLastDigit)-pDigit;
  Move(pDigit^, Dest^, (NumericLen-Src.scale) shl 1);
  if Src.scale > 0 then begin
    pWord(Dest+(NumericLen-Src.scale))^ := Ord('.');
    Move((PWideChar(pLastDigit)-Src.scale)^, (Dest+(NumericLen-Src.scale)+1)^,Src.scale shl 1);
    Inc(NumericLen);
  end;
  //free possibly allocated mem
  if Pointer(pDigitCopy) <> Pointer(@NumericVal[0]) then
    FreeMem(pDigitCopy);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure DBNumMultiplyLookupFiller;
const bL: array[Boolean] of TSQLDigit = (10, 100);
var B: Boolean;
  I, j, cnt: Integer;
  Carry: TSQLDigit;
  bCarry: Byte;
  wNextVal: Word;
  NextVal: TDoubleSQLDigit;
  pVals: PByteArray;
  bOleDBMultiplyLookup: array[Boolean, 1..MaxFMTBcdDigits-1] of TOleDBMultiplyLookup;
begin
  { calulate amount of packed multipliers }
  for b := False to True do begin
    cnt := 1;
    FillChar(DBNumMultiplyLookup[b], SizeOf(TOleDBMultiplyLookup), #0);
    DBNumMultiplyLookup[b][1].MultiplierCount := Cnt;
    DBNumMultiplyLookup[b][1].Values[0] := bl[b];
    for I := 2 to MaxFMTBcdDigits-1 do begin
      Move(DBNumMultiplyLookup[b][I-1], DBNumMultiplyLookup[b][I], SizeOf(TOleDBMultiplyLookup));
      Carry := 0;
      for J := 0 to Cnt - 1 do begin
        NextVal := (TDoubleSQLDigit(DBNumMultiplyLookup[b][I].Values[J]) * TDoubleSQLDigit(100)) + Carry;
        DBNumMultiplyLookup[b][I].Values[J] := NextVal and FlushHalfDoubleDigit;
        Carry := NextVal shr ShrSQLDigit;
      end;
      if Carry <> 0 then begin
        DBNumMultiplyLookup[b][i].Values[Cnt] := Carry;
        Inc(Cnt);
        if Cnt > MaxFMTBcdDigits div SizeOf(TSQLDigit) then
          Break;
        DBNumMultiplyLookup[b][i].MultiplierCount := Cnt;
      end;
    end;
  end;
  { now calculate the amount of bytes for the VAR_NUMERICS }
  for b := False to True do begin
    cnt := 1;
    FillChar(bOleDBMultiplyLookup[b], SizeOf(TOleDBMultiplyLookup), #0);
    bOleDBMultiplyLookup[b][1].MultiplierCount := Cnt;
    pByte(@bOleDBMultiplyLookup[b][1].Values[0])^ := bl[b];
    for I := 2 to MaxFMTBcdDigits-1 do begin
      Move(bOleDBMultiplyLookup[b][I-1], bOleDBMultiplyLookup[b][I], SizeOf(TOleDBMultiplyLookup));
      pVals := @bOleDBMultiplyLookup[b][I].Values[0];
      bCarry := 0;
      for J := 0 to Cnt - 1 do begin
        wNextVal := (Word(pVals[J]) * Word(100)) + bCarry;
        pVals[J] := wNextVal and $FF;
        bCarry := wNextVal shr 8;
      end;
      if bCarry <> 0 then begin
        pVals[Cnt] := bCarry;
        Inc(Cnt);
        bOleDBMultiplyLookup[b][i].MultiplierCount := Cnt;
        DBNumMultiplyLookup[b][i].ByteCount := Cnt;
        if Cnt > MaxFMTBcdDigits then
          Break;
      end else
        DBNumMultiplyLookup[b][i].ByteCount := Cnt;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function DBNumeric2Curr_LE(Src: PDB_NUMERIC; NumericNegSign: Byte): Currency;
var i64: Int64 absolute Result;
{$IFDEF ENDIAN_BIG}I: Integer;{$ENDIF}
begin
  i64 := PInt64(@Src.val[0])^;
  if i64 = 0 then
    Exit;
  {$IFDEF ENDIAN_BIG}
  for I := 7 downto 0 do
    if (Src.val[I] <> 0) and (I >= 1) then begin
      MoveReverseByteOrder(@i64, @i64, i+1);
      Break;
    end;
  {$ENDIF}
  if Src.Scale < 4 then
    i64 := i64 * ZFastCode.Int64Tower[4 - Src.scale]
  else if Src.Scale > 4 then
    i64 := i64 div ZFastCode.Int64Tower[Src.scale - 4];
  if Src.Sign = NumericNegSign then
    Result := -Result;
end;

function DBNumeric2Curr_BE(Src: PDB_NUMERIC; NumericNegSign: Byte): Currency;
var i64: Int64 absolute Result;
{$IFNDEF ENDIAN_BIG}I: Integer;{$ENDIF}
begin
  i64 := PInt64(@Src.val[0])^;
  if i64 = 0 then
    Exit;
  {$IFNDEF ENDIAN_BIG}
  for I := 7 downto 0 do
    if (Src.val[I] <> 0) and (I >= 1) then begin
      MoveReverseByteOrder(@i64, @i64, i+1);
      Break;
    end;
  {$ENDIF}
  if Src.Scale < 4 then
    i64 := i64 * ZFastCode.Int64Tower[4 - Src.scale]
  else if Src.Scale > 4 then
    i64 := i64 div ZFastCode.Int64Tower[Src.scale - 4];
  if Src.Sign = NumericNegSign then
    Result := -Result;
end;

procedure Curr2DBNumeric_LE(const Src: Currency; Dest: PDB_NUMERIC; const NumericSign: TNumericSign);
var i64: Int64 absolute Src;
{$IFDEF ENDIAN_BIG}I: Integer;{$ENDIF}
begin
  Dest.precision := 19;
  Dest.scale := 4;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
  if Src < 0 then begin
    Dest.sign := NumericSign[True];
    PUInt64(@Dest.val[0])^ := -i64;
  end else begin
    Dest.sign := not NumericSign[False];
    PUInt64(@Dest.val[0])^ := i64;
  end;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  {$IFDEF ENDIAN_BIG}
  for I := 7 downto 0 do
    if (Dest.val[I] <> 0) and (I >= 1) then begin
      MoveReverseByteOrder(@Dest.val[0], @Dest.val[0], I+1);
      Break;
    end;
  {$ENDIF}
  PInt64(@Dest.val[SizeOf(Currency)])^ := 0;
end;

procedure Curr2DBNumeric_BE(const Src: Currency; Dest: PDB_NUMERIC; const NumericSign: TNumericSign);
var i64: Int64 absolute Src;
{$IFNDEF ENDIAN_BIG}I: Integer;{$ENDIF}
begin
  Dest.precision := 19;
  Dest.scale := 4;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
  if Src < 0 then begin
    Dest.sign := NumericSign[True];
    PUInt64(@Dest.val[0])^ := -i64;
  end else begin
    Dest.sign := not NumericSign[False];
    PUInt64(@Dest.val[0])^ := i64;
  end;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  {$IFNDEF ENDIAN_BIG}
  for I := 7 downto 0 do
    if (Dest.val[I] <> 0) and (I >= 1) then begin
      MoveReverseByteOrder(@Dest.val[0], @Dest.val[0], I+1);
      Break;
    end;
  {$ENDIF}
  PInt64(@Dest.val[SizeOf(Currency)])^ := 0;
end;
{$IFEND}

procedure MoveReverseByteOrder(Dest, Src: PAnsiChar; Len: LengthInt);
var B: Byte;
begin
  if (Dest = Src) then begin
    Dest := Src+Len-1;
    Len := Len shr 1;
    while Len > 0 do begin
      B := PByte(Dest)^;
      Dest^ := Src^;
      PByte(Src)^ := B;
      dec(Dest);
      Inc(Src);
      dec(Len);
    end;
  end else begin
    Dest := Dest+Len-1;
    while Len > 0 do begin
      Dest^ := Src^;
      dec(Dest);
      Inc(Src);
      dec(Len);
    end;
  end;
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryRaw(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex: TBooleanDynArray;
  IsNCharIndex: PBooleanDynArray; ComparePrefixTokens: PPreparablePrefixTokens;
  var TokenMatchIndex: Integer): TRawByteStringDynArray;
var
  I, C, N, FirstComposePos: Integer;
  CP: Word;
  NextIsNChar, ParamFound: Boolean;
  Tokens: TZTokenList;
  Token: PZToken;
  Tmp: RawByteString;
  {$IFNDEF UNICODE}
  SectionWriter: TZRawSQLStringWriter;
  Fraction: RawByteString;
  {$ENDIF}

  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    if IsNCharIndex <> nil then begin
      SetLength(IsNCharIndex^, Length(Result));
      if Param and NextIsNChar then
      begin
        IsNCharIndex^[High(IsNCharIndex^)] := True;
        NextIsNChar := False;
      end else
        IsNCharIndex^[High(IsNCharIndex^)] := False;
    end;
  end;
begin
  ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound {$IFNDEF UNICODE}or ConSettings^.AutoEncode {$ENDIF}or Assigned(ComparePrefixTokens) then begin
    Tokens := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    CP := ConSettings^.ClientCodePage^.CP;
    {$IFNDEF UNICODE}
    if ConSettings^.AutoEncode
    then SectionWriter := TZRawSQLStringWriter.Create(Length(SQL) shr 5)
    else SectionWriter := nil; //satisfy compiler
    {$ENDIF}
    try
      NextIsNChar := False;
      N := -1;
      FirstComposePos := 0;
      TokenMatchIndex := -1;
      Tmp := '';
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if (Token.TokenType = ttWord) and Assigned(ComparePrefixTokens) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens^) do
              if Tokens.IsEqual(I, ComparePrefixTokens^[C].MatchingGroup,  tcInsensitive) then begin
                if Length(ComparePrefixTokens^[C].ChildMatches) = 0
                then TokenMatchIndex := C
                else N := C; //save group
                Break;
              end;
            if N = -1 then //no sub-tokens ?
              ComparePrefixTokens := nil; //stop compare sequence
          end else begin //we already got a group
            for C := 0 to high(ComparePrefixTokens^[N].ChildMatches) do
              if Tokens.IsEqual(I, ComparePrefixTokens^[N].ChildMatches[C], tcInsensitive) then begin
                TokenMatchIndex := N;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
      if ParamFound and Tokens.IsEqual(I, Char('?')) then begin
        if (FirstComposePos < Tokens.Count-1) then
          {$IFDEF UNICODE}
          Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, Tokens[I-1].P-Tokens[FirstComposePos].P+Tokens[I-1].L, CP);
          {$ELSE}
          if Consettings.AutoEncode
          then SectionWriter.Finalize(Tmp)
          else Tmp := Tokens.AsString(FirstComposePos, I-1);
          {$ENDIF}
          Add(Tmp, False);
          {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
          Add(ZUnicodeToRaw(Tokens.AsString(I, I), ConSettings^.ClientCodePage^.CP), True);
          {$ELSE}
          Add('?', True);
          {$ENDIF}
          Tmp := EmptyRaw;
          FirstComposePos := i +1;
        end else if ParamFound and (IsNCharIndex<> nil) and Tokens.IsEqual(I, Char('N')) and
            (Tokens.Count > i) and Tokens.IsEqual(i+1, Char('?')) then
          NextIsNChar := True
        {$IFNDEF UNICODE}
        else if (FirstComposePos <= I) and ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier: begin
                Fraction := ConSettings^.ConvFuncs.ZStringToRaw(TokenAsString(Token^), ConSettings^.CTRL_CP, CP);
                SectionWriter.AddText(Fraction, Tmp);
              end;
            else SectionWriter.AddText(Token.P, Token.L, tmp);
        end
        {$ENDIF};
      end;
      I := Tokens.Count -1;
      if (FirstComposePos <= Tokens.Count-1) then begin
        {$IFDEF UNICODE}
        Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, Tokens[I].P-Tokens[FirstComposePos].P+Tokens[I].L, CP);
        {$ELSE}
        if ConSettings.AutoEncode
        then SectionWriter.Finalize(Tmp)
        else Tmp := Tokens.AsString(FirstComposePos, I);
        {$ENDIF}
        Add(Tmp, False);
      end;
    finally
      Tokens.Free;
      {$IFNDEF UNICODE}
      if ConSettings^.AutoEncode then
        SectionWriter.Free;
      {$ENDIF}
    end;
  end
  else
    Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryUni(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex: TBooleanDynArray;
  IsNCharIndex: PBooleanDynArray; ComparePrefixTokens: PPreparablePrefixTokens;
  var TokenMatchIndex: Integer): TUnicodeStringDynArray;
var
  I, C, N: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  Temp: ZWideString;
  FirstComposePos: Integer;
  {$IFNDEF UNICODE}
  SectionWriter: TZUnicodeSQLStringWriter;
  {$ENDIF}
  NextIsNChar, ParamFound: Boolean;
  procedure Add(const Value: ZWideString; Const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    if IsNCharIndex <> nil then begin
      SetLength(IsNCharIndex^, Length(Result));
      if Param and NextIsNChar then begin
        IsNCharIndex^[High(IsNCharIndex^)] := True;
        NextIsNChar := False;
      end else
        IsNCharIndex^[High(IsNCharIndex^)] := False;
    end;
  end;
begin
  ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSe}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound {$IFNDEF UNICODE}or ConSettings^.AutoEncode{$ENDIF} or Assigned(ComparePrefixTokens) then begin
    Tokens := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    {$IFNDEF UNICODE}
    if ConSettings.AutoEncode
    then SectionWriter := TZUnicodeSQLStringWriter.Create(Length(SQL))
    else SectionWriter := nil;
    {$ENDIF}
    try
      Temp := '';
      NextIsNChar := False;
      N := -1;
      TokenMatchIndex := -1;
      FirstComposePos := 0;
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if (Token.TokenType = ttWord) and Assigned(ComparePrefixTokens) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens^) do
              if Tokens.IsEqual(I, ComparePrefixTokens^[C].MatchingGroup, tcInsensitive) then begin
                if Length(ComparePrefixTokens^[C].ChildMatches) = 0 then
                  TokenMatchIndex := C
                else
                  N := C; //save group
                Break;
              end;
            if N = -1 then //no sub-tokens ?
              ComparePrefixTokens := nil; //stop compare sequence
          end else begin //we already got a group
            for C := 0 to high(ComparePrefixTokens^[N].ChildMatches) do
              if Tokens.IsEqual(I, ComparePrefixTokens^[N].ChildMatches[C], tcInsensitive) then begin
                TokenMatchIndex := N;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
        if ParamFound and Tokens.IsEqual(I, Char('?')) then begin
          {$IFDEF UNICODE}
          Temp := Tokens.AsString(FirstComposePos, I-1);
          {$ELSE}
          if ConSettings.AutoEncode
          then SectionWriter.Finalize(Temp)
          else Temp := PRawToUnicode(Tokens[FirstComposePos].P, Tokens[I-1].P-Tokens[FirstComposePos].P+Tokens[I-1].L, ConSettings^.CTRL_CP);
          {$ENDIF}
          Add(Temp, False);
          Add('?', True);
          Temp := '';
          FirstComposePos := i +1;
        end else if ParamFound and (IsNCharIndex <> nil) and Tokens.IsEqual(I, Char('N')) and
          (Tokens.Count > i) and Tokens.IsEqual(I+1, Char('?')) then
          NextIsNChar := True
        {$IFNDEF UNICODE}
        else if (FirstComposePos <= I) and ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword:
              SectionWriter.AddText(ConSettings^.ConvFuncs.ZStringToUnicode(Tokens.AsString(i), ConSettings^.CTRL_CP), Temp);
            else SectionWriter.AddAscii7Text(Token.P, Token.L, Temp);
          end;
        {$ENDIF}
      end;
      I := Tokens.Count -1;
      if (FirstComposePos <= Tokens.Count-1) then begin
        {$IFDEF UNICODE}
        Temp := Tokens.AsString(FirstComposePos, I);
        {$ELSE}
        if ConSettings.AutoEncode
        then SectionWriter.Finalize(Temp)
        else Temp := PRawToUnicode(Tokens[FirstComposePos].P, Tokens[I].P-Tokens[FirstComposePos].P+Tokens[I].L, ConSettings^.CTRL_CP);
        {$ENDIF}
        Add(Temp, False);
      end;
    finally
      Tokens.Free;
      {$IFNDEF UNICODE}
      if ConSettings^.AutoEncode then
        SectionWriter.Free;
      {$ENDIF}
    end;
  end else
    {$IFDEF UNICODE}
    Add(SQL);
    {$ELSE}
    Add(ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings^.CTRL_CP));
    {$ENDIF}
end;

{**
  Extracts list of fields from a string. Fields could be quoted, delimited by any of
  the specified delimiters and any number of whitespaces. Any other symbol or
  unexpected delimiter will raise an exception. Quoted field names will be returned
  without quotes.
  @param FieldNames a list of field names.
  @param SepChars set of field name delimiters

  @returns list of field names.
}
function ExtractFields(const FieldNames: string; const SepChars: Array of Char): TStrings;
var
  Token: PZToken;
  Tokenizer: IZTokenizer;
  procedure RaiseTokenExc;
  begin
    FreeAndNil(Result);
    raise EZSQLException.Create(Format('Unexpected token "%s" in string "%s"', [TokenAsString(Token^), FieldNames]));
  end;

  function CharInSet(P: Char; SepChars: Array of Char): Boolean;
  var I: Integer;
  begin
    for I := Low(SepChars) to High(SepChars) do
      if SepChars[i] = P then begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;
var
  Tokens: TZTokenList;
  I: Integer;
  ExpectToken: TZTokenType;
begin
  ExpectToken := ttWord;
  Tokenizer := TZGenericSQLTokenizer.Create;
  Tokens := Tokenizer.TokenizeBufferToList(FieldNames,
    [toSkipEOF, toSkipWhitespaces]);
  Result := TStringList.Create;

  try
    for I := 0 to Tokens.Count - 1 do begin
      Token := Tokens[I];
      if Token.TokenType <> ExpectToken then
        RaiseTokenExc;

      case Token.TokenType of
        ttWord:
          begin
            Result.Add(Tokenizer.GetQuoteState.DecodeToken(Token^, Token.P^));
            ExpectToken := ttSymbol;
          end;
        ttSymbol:
          begin
            if not CharInSet(Token.p^, SepChars) then
              RaiseTokenExc;
            ExpectToken := ttWord;
          end;
        else
          RaiseTokenExc;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

function TestEncoding(const Bytes: TByteDynArray; const Size: Cardinal): TZCharEncoding;
begin
  Result := ceDefault;
  {EgonHugeist:
    Step one: Findout, what's comming in! To avoid User-Bugs as good as possible
      it is possible that a PAnsiChar OR a PWideChar was written into
      the Stream!!!  And these chars could be trunced with changing the
      Stream.Size.
      I know this can lead to pain with two byte ansi chars, but what else can i do?
    step two: detect the encoding }

  if (Size mod 2 = 0) and ( ZFastCode.StrLen(Pointer(Bytes)) {%H-}< Size ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16
  else
    //if ConSettings.AutoEncode then
      case ZDetectUTF8Encoding(Pointer(Bytes), Size) of
        etUSASCII: Result := ceDefault; //Exact!
        etAnsi:
          { Sure this isn't right in all cases!
            Two/four byte WideChars causing the same result!
            Leads to pain! Is there a way to get a better test?
            I've to start from the premise the function which calls this func
            should decide wether ansi or unicode}
          Result := ceAnsi;
        etUTF8: Result := ceUTF8; //Exact!
      end
    //else
      //Result := ceDefault
end;

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString;
var
  US: ZWideString; //possible com base widestring -> prevent overflow
  Bytes: TByteDynArray;
  Encoding: TZCharEncoding;
begin
  if Size = 0 then
    Result := EmptyRaw
  else
  begin
    SetLength(Bytes, Size +2);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.move(Buffer^, Pointer(Bytes)^, Size);
    Encoding := TestEncoding(Bytes, Size);
    SetLength(Bytes, 0);
    case Encoding of
      ceDefault: ZSetString(Buffer, Size, Result);
      ceAnsi:
        if ConSettings.ClientCodePage.Encoding in [ceAnsi, ceUTF16] then
          if ( ConSettings.CTRL_CP = zCP_UTF8) or (ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP) then //second test avoids encode the string twice
            ZSetString(Buffer, Size, Result)  //should be exact
          else
          begin
            US := PRawToUnicode(Buffer, Size, ConSettings.CTRL_CP);
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
          end
        else begin  //Database expects UTF8
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            if ZOSCodePage = zCP_UTF8 then
              US := ZSysUtils.ASCII7ToUnicodeString(Buffer, Size) //Can't localize the ansi CP
            else
              US := PRawToUnicode(Buffer, Size, ZOSCodePage)
          else
            US := PRawToUnicode(Buffer, Size, ConSettings.CTRL_CP);
          Result := ZUnicodeToRaw(US, zCP_UTF8);
        end;
      ceUTF8:
        if (ConSettings.ClientCodePage.Encoding in [ceAnsi, ceUTF16]) then begin//ansi expected
          {$IFDEF WITH_LCONVENCODING}
          ZSetString(Buffer, Size, Result);
          Result := Consettings.PlainConvertFunc(Result);
          {$ELSE}
          US := PRawToUnicode(Buffer, Size, zCP_UTF8);
          Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
          {$ENDIF}
         end else //UTF8 Expected
           ZSetString(Buffer, Size, Result);  //should be exact
      ceUTF16:
        begin
          SetLength(US, Size shr 1);
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(US)^, Size);
          if ConSettings.ClientCodePage.Encoding = ceAnsi then
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(UTF8Encode(US))
            {$ELSE}
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
            {$ENDIF}
          else
            Result := ZUnicodeToRaw(US, zCP_UTF8);
        end;
      else
        Result := EmptyRaw;
    end;
  end;
end;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString;
var DB_CP: Word;
begin
  DB_CP := ConSettings.ClientCodePage.CP;
  ConSettings.ClientCodePage.CP := ToCP;
  Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
  ConSettings.ClientCodePage.CP := DB_CP;
end;

function CreateUnsupportedParameterTypeException(Index: Integer; ParamType: TZSQLType): EZSQLException;
var TypeName: String;
begin
  TypeName := GetEnumName(TypeInfo(TZSQLType), Ord(ParamType));
  Result := EZSQLException.Create(SUnsupportedParameterType + ': ' + TypeName+', Index: '+ZFastCode.IntToStr(Index));
end;

const
  null_a: PAnsiChar = 'null';
  null_w: PWideChar = 'null';
function IsNullFromArray(ZArray: PZArray; Index: Cardinal): Boolean;
var P: Pointer;
  VType: TZVariantType;
  SQLType: TZSQLType;
begin
  Result := False;
  if (ZArray <> nil) then begin
    P := ZArray^.VIsNullArray;
    SQLType := TZSQLType(ZArray.VIsNullArrayType);
    VType := ZArray^.VIsNullArrayVariantType;
    if (P = nil) and (ZArray^.VArray <> nil) and
       not (SQLType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then
      case ZArray^.VIsNullArrayVariantType of
        {$IFNDEF UNIOCDE}vtString,{$ENDIF}
        {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
        vtRawByteString:  begin
                            P := ZArray^.VArray;
                            SQLType := stString;
                            VType := vtRawByteString;
                          end;
        {$IFDEF UNIOCDE}vtString,{$ENDIF}
        vtUnicodeString: begin
                            P := ZArray^.VArray;
                            SQLType := stUnicodeString;
                            VType := vtUnicodeString;
                          end;
        vtCharRec:        begin
                            P := ZArray^.VArray;
                            SQLType := stUnicodeString;
                            VType := vtCharRec;
                          end;
        else ;//hide weird FPC warning
      end;
    if P <> nil then
      case SQLType of
        stBoolean: IsNullFromArray := TBooleanDynArray(P)[Index];
        stByte: IsNullFromArray := TByteDynArray(P)[Index] <> 0;
        stShort: IsNullFromArray := TShortIntDynArray(P)[Index] <> 0;
        stWord: IsNullFromArray := TWordDynArray(P)[Index] <> 0;
        stSmall: IsNullFromArray := TSmallIntDynArray(P)[Index] <> 0;
        stLongWord: IsNullFromArray := TLongWordDynArray(P)[Index] <> 0;
        stInteger: IsNullFromArray := TIntegerDynArray(P)[Index] <> 0;
        stLong: IsNullFromArray := TInt64DynArray(P)[Index] <> 0;
        stULong: IsNullFromArray := TUInt64DynArray(P)[Index] <> 0;
        stFloat: IsNullFromArray := TSingleDynArray(P)[Index] <> 0;
        stDouble: IsNullFromArray := TDoubleDynArray(P)[Index] <> 0;
        stCurrency: IsNullFromArray := TCurrencyDynArray(P)[Index] <> 0;
        stBigDecimal: IsNullFromArray := TExtendedDynArray(P)[Index] <> 0;
        stString, stUnicodeString:
            case VType of
              {$IFNDEF UNIOCDE}
              vtString,
              {$ENDIF}
              {$IFNDEF NO_ANSISTRING}
              vtAnsiString,
              {$ENDIF}
              {$IFNDEF NO_UTF8STRING}
              vtUTF8String,
              {$ENDIF}
              vtRawByteString: IsNullFromArray := StrToBoolEx(TRawByteStringDynArray(P)[Index]) or
                                ((Length(TRawByteStringDynArray(P)[Index]) = 4) and
                                  ZSysUtils.SameText(null_a, Pointer(TRawByteStringDynArray(P)[Index]), 4));
              {$IFDEF UNIOCDE}
              vtString,
              {$ENDIF}
              vtUnicodeString: IsNullFromArray := StrToBoolEx(TUnicodeStringDynArray(P)[Index])or
                                ((Length(TUnicodeStringDynArray(P)[Index]) = 4) and
                                  ZSysUtils.SameText(null_w, Pointer(TRawByteStringDynArray(P)[Index]), 4));
              vtCharRec:
                if (TZCharRecDynArray(P)[Index].CP = zCP_UTF16)
                then IsNullFromArray := StrToBoolEx(PWideChar(TZCharRecDynArray(P)[Index].P)) or
                                ((TZCharRecDynArray(P)[Index].Len = 4) and
                                  ZSysUtils.SameText(null_w, PWideChar(TZCharRecDynArray(P)[Index].P), 4))
                else IsNullFromArray := StrToBoolEx(PAnsiChar(TZCharRecDynArray(P)[Index].P)) or
                                ((TZCharRecDynArray(P)[Index].Len = 4) and
                                  ZSysUtils.SameText(null_a, PAnsiChar(TZCharRecDynArray(P)[Index].P), 4));
              vtNull: IsNullFromArray := True;
              else
                raise EZSQLException.Create('Unsupported String Variant');
            end;
        stBytes:
          IsNullFromArray := TBytesDynArray(P)[Index] = nil;
        stDate, stTime, stTimestamp:
          IsNullFromArray := TDateTimeDynArray(P)[Index] <> 0;
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream:
          IsNullFromArray := TInterfaceDynArray(P)[Index] = nil;
        else
          raise EZSQLException.Create(SUnsupportedParameterType);
      end
  end else Result := True;
end;

procedure ToBuff(const Value: RawByteString; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  L, LRes: LengthInt;
begin
  L := Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  if L <= 0 then Exit;
  if L < (SizeOf(Buf.Buf)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := AnsiChar(P^)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L);
    Inc(Buf.Pos, L);
  end else begin
    LRes := Length(Result)+Buf.Pos+L;
    SetLength(Result, LRes{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+LRes)^ := Ord(#0);
    {$ENDIF}
    Inc(P, LRes-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure ToBuff(Value: Pointer; L: LengthInt; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  LRes: LengthInt;
begin
  if L <= 0 then Exit;
  if L < (SizeOf(Buf.Buf)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := AnsiChar(P^)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L);
    Inc(Buf.Pos, L);
  end else begin
    LRes := Length(Result)+Buf.Pos+L;
    SetLength(Result, LRes{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+LRes)^ := Ord(#0);
    {$ENDIF}
    Inc(P, LRes-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure ToBuff(Value: AnsiChar; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  if Buf.Pos < (SizeOf(Buf.Buf)) then begin
    Buf.Buf[Buf.Pos] := Value;
    Inc(Buf.Pos);
  end else begin
    L := Length(Result)+Buf.Pos+1;
    SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+L)^ := Ord(#0);
    {$ENDIF}
    Inc(P, Length(Result)-Buf.Pos-1);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    AnsiChar(P^) := Value;
  end;
end;

procedure ToBuff(const Value: ZWideString; var Buf: TUCS2Buff; var Result: ZWideString); overload;
var
  P: PWideChar;
  L: LengthInt;
begin
  L := Length(Value);
  if L <= 0 then Exit;
  if L < ((SizeOf(Buf.Buf) shr 1)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := P^
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L shl 1);
    Inc(Buf.Pos, L);
  end else begin
    SetLength(Result, Length(Result)+Buf.Pos+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L shl 1);
  end;
end;

procedure ToBuff(Value: WideChar; var Buf: TUCS2Buff; var Result: ZWideString); overload;
var
  P: PWideChar;
  L: LengthInt;
begin
  if (Buf.Pos < (SizeOf(Buf.Buf) shr 1)) then begin
    Buf.Buf[Buf.Pos] := Value;
    Inc(Buf.Pos);
  end else begin
    L := Length(Result)+Buf.Pos+1;
    SetLength(Result, L);
    P := Pointer(Result);
    Inc(P, L-Buf.Pos-1);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    P^ := Value;
  end;
end;

procedure FlushBuff(var Buf: TRawBuff; var Result: RawByteString); overload;
var P: PAnsiChar;
  L: LengthInt;
begin
  if Buf.Pos > 0 then begin
    L := Length(Result)+Buf.Pos;
    SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+L)^ := Ord(#0);
    {$ENDIF}
    Inc(P, L-Buf.Pos);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
    Buf.Pos := 0;
  end;
end;

procedure FlushBuff(var Buf: TUCS2Buff; var Result: ZWideString); overload;
var P: PWideChar;
  L: LengthInt;
begin
  if Buf.Pos > 0 then begin
    L := Length(Result)+Buf.Pos;
    SetLength(Result, L);
    P := Pointer(Result);
    Inc(P, L-Buf.Pos);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
    Buf.Pos := 0;
  end;
end;

procedure ReplaceOrAddLastChar(cOld, cNew: AnsiChar; var Buf: TRawBuff; var Result: RawByteString);
var P: PAnsiChar;
begin
  P := nil;
  if (Buf.Pos > 0) and (Buf.Buf[Buf.Pos-1] = cOld) then
    P := @Buf.Buf[Buf.Pos-1]
  else if (Buf.Pos = 0) and (Pointer(Result) <> nil) then begin
    P := Pointer(Result);
    if PByte(P + Length(Result) -1)^ = Ord(cOld)
    then P := P+Length(Result) -1
    else P := nil;
  end;
  if P = nil
  then ToBuff(cNew, Buf, Result)
  else PByte(P)^ := Ord(cNew);
end;

procedure CancelLastChar(var Buf: TRawBuff; var Result: RawByteString);
begin
  if (Buf.Pos > 0) then
    Dec(Buf.Pos)
  else if (Buf.Pos = 0) and (Pointer(Result) <> nil) then
    Result := Copy(Result, 1, Length(Result)-1);
end;

procedure CancelLastChar(var Buf: TUCS2Buff; var Result: ZWideString); overload;
begin
  if (Buf.Pos > 0) then
    Dec(Buf.Pos)
  else if (Buf.Pos = 0) and (Pointer(Result) <> nil) then
    Result := Copy(Result, 1, Length(Result)-1);
end;

procedure ReplaceOrAddLastChar(cOld, cNew: WideChar; var Buf: TUCS2Buff; var Result: ZWideString);
var P: PWideChar;
begin
  P := nil;
  if (Buf.Pos > 0) and (Buf.Buf[Buf.Pos-1] = cOld) then
    P := @Buf.Buf[Buf.Pos-1]
  else if (Buf.Pos = 0) and (Pointer(Result) <> nil) then begin
    P := Pointer(Result);
    if PWord(P + Length(Result) -1)^ = Ord(cOld)
    then P := P+Length(Result) -1
    else P := nil;
  end;
  if P = nil
  then ToBuff(cNew, Buf, Result)
  else PWord(P)^ := Ord(cNew);
end;

function GetAbsorbedTrailingSpacesLen(Buf: PAnsiChar; Len: LengthInt): LengthInt;
var PEnd: PAnsiChar;
begin
  if Len > 4 then begin
    PEnd := Buf + Len - 4;
    while (PEnd >= Buf) and (PInteger(PEnd)^ = i4SpaceRaw) do
      Dec(PEnd, 4);
    Inc(PEnd, 4);
  end else
    PEnd := Buf+Len;
  while (PEnd > Buf) and (PByte(PEnd-1)^ = Ord(' ')) do
    Dec(PEnd);
  Result := PEnd - Buf;
end;

function GetAbsorbedTrailingSpacesLen(Buf: PWideChar; Len: LengthInt): LengthInt;
var PEnd: PWideChar;
begin
  if Len > 4 then begin
    PEnd := Buf + Len - 4;
    while (PEnd >= Buf) and (PInt64(PEnd)^ = i4SpaceUni) do
      Dec(PEnd, 4);
    Inc(PEnd, 4);
  end else
    PEnd := Buf+Len;
  while (PEnd > Buf) and (PWord(PEnd-1)^ = Ord(' ')) do
    Dec(PEnd);
  Result := PEnd- Buf;
end;

procedure ReferenceArray(aArray: Pointer; Var Dest: Pointer;
  SQLType: TZSQLType; VariantType: TZVariantType);
begin
  Dest := nil; //if aArray is equal to Dest the RefCount won't be incremented
  if aArray <> nil then
    case VariantType of
      {$IFNDEF UNICODE}vtString,{$ENDIF}
      {$IFNDEF NO_ANSISTRING}vtAnsiString,
      {$ENDIF}{$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
      vtRawByteString:  TRawByteStringDynArray(Dest) := TRawByteStringDynArray(aArray);
      {$IFDEF UNICODE}vtString,{$Endif}
      vtUnicodeString:  TUnicodeStringDynArray(Dest) := TUnicodeStringDynArray(aArray);
      vtCharRec:        TZCharRecDynArray(Dest) := TZCharRecDynArray(aArray);
      else case SQLType of
        stBoolean:      TBooleanDynArray(Dest)  := TBooleanDynArray(aArray);
        stByte:         TByteDynArray(Dest)     := TByteDynArray(aArray);
        stShort:        TShortIntDynArray(Dest) := TShortIntDynArray(aArray);
        stWord:         TShortIntDynArray(Dest) := TShortIntDynArray(aArray);
        stSmall:        TSmallIntDynArray(Dest) := TSmallIntDynArray(aArray);
        stLongWord:     TCardinalDynArray(Dest) := TCardinalDynArray(aArray);
        stInteger:      TIntegerDynArray(Dest)  := TIntegerDynArray(aArray);
        stULong:        TUInt64DynArray(Dest)   := TUint64DynArray(aArray);
        stLong:         TInt64DynArray(Dest)    := TInt64DynArray(aArray);
        stFloat:        TSingleDynArray(Dest)   := TSingleDynArray(aArray);
        stDouble:       TDoubleDynArray(Dest)   := TDoubleDynArray(aArray);
        stCurrency:     TCurrencyDynArray(Dest) := TCurrencyDynArray(aArray);
        stBigDecimal:   TBcdDynArray(Dest)      := TBcdDynArray(aArray);
        stDate,
        stTime,
        stTimestamp:    TDateTimeDynArray(Dest) := TDateTimeDynArray(aArray);
        stGUID:         TGUIDDynArray(Dest)     := TGUIDDynArray(aArray);
        stBytes:        TBytesDynArray(Dest)    := TBytesDynArray(aArray);
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream: TInterfaceDynArray(Dest):= TInterfaceDynArray(aArray);
        else Raise EZUnsupportedException.Create(SUnsupportedOperation);
      end;
    end;
end;

procedure DeReferenceArray(Var Dest: Pointer; SQLType: TZSQLType; VariantType: TZVariantType);
begin
  if Dest <> nil then
    case VariantType of
      {$IFNDEF UNICODE}vtString,{$ENDIF}
      {$IFNDEF NO_ANSISTRING}vtAnsiString,
      {$ENDIF}{$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
      vtRawByteString:  TRawByteStringDynArray(Dest) := nil;
      {$IFDEF UNICODE}vtString,{$Endif}
      vtUnicodeString:  TUnicodeStringDynArray(Dest) := nil;
      vtCharRec:        TZCharRecDynArray(Dest) := nil;
      vtDate:           TZDateDynArray(Dest) := nil;
      vtTime:           TZTimeDynArray(Dest) := nil;
      vtTimeStamp:      TZTimeStampDynArray(Dest) := nil;
      else case SQLType of
        stBoolean:      TBooleanDynArray(Dest)  := nil;
        stByte:         TByteDynArray(Dest)     := nil;
        stShort:        TShortIntDynArray(Dest) := nil;
        stWord:         TShortIntDynArray(Dest) := nil;
        stSmall:        TSmallIntDynArray(Dest) := nil;
        stLongWord:     TCardinalDynArray(Dest) := nil;
        stInteger:      TIntegerDynArray(Dest)  := nil;
        stULong:        TUInt64DynArray(Dest)   := nil;
        stLong:         TInt64DynArray(Dest)    := nil;
        stFloat:        TSingleDynArray(Dest)   := nil;
        stDouble:       TDoubleDynArray(Dest)   := nil;
        stCurrency:     TCurrencyDynArray(Dest) := nil;
        stBigDecimal:   TBcdDynArray(Dest)      := nil;
        stDate,
        stTime,
        stTimestamp:    TDateTimeDynArray(Dest) := nil;
        stGUID:         TGUIDDynArray(Dest)     := nil;
        stBytes:        TBytesDynArray(Dest)    := nil;
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream: TInterfaceDynArray(Dest):= nil;
        else raise EZUnsupportedException.Create(sUnsupportedOperation);
      end;
    end;
end;

function CharRecArray2UnicodeStrArray(const Value: TZCharRecDynArray;
  var MaxLen: LengthInt): TUnicodeStringDynArray;
var i: Integer;
begin
  SetLength(Result, Length(Value));
  MaxLen := 0;
  for I := 0 to High(Value) do
    if Value[i].CP = zCP_UTF16 then begin
      SetString(Result[i], PWideChar(Value[i].P), Value[i].Len);
      MaxLen := Max(MaxLen, LengthInt(Value[i].Len));
    end else begin
      Result[i] := PRawToUnicode(Value[i].P, Value[i].Len, Value[i].CP);
      MaxLen := Max(MaxLen, Length(Result[i]));
    end;
end;

function CharRecArray2UnicodeStrArray(const Value: TZCharRecDynArray): TUnicodeStringDynArray;
var i: Integer;
begin
  SetLength(Result, Length(Value));
  for I := 0 to High(Value) do
    if Value[i].CP = zCP_UTF16
    then SetString(Result[i], PWideChar(Value[i].P), Value[i].Len)
    else Result[i] := PRawToUnicode(Value[i].P, Value[i].Len, Value[i].CP);
end;

function ArrayValueToInteger(ZArray: PZArray; Index: Integer): Integer;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  Result := RawToIntDef(TRawByteStringDynArray(ZArray.VArray)[Index], 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString:  Result := UnicodeToIntDef(TUnicodeStringDynArray(ZArray.VArray)[Index], 0);
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then Result := UnicodeToIntDef(TZCharRecDynArray(ZArray.VArray)[Index].P, 0)
      else Result := RawToIntDef(TZCharRecDynArray(ZArray.VArray)[Index].P, 0);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := Trunc(TSingleDynArray(ZArray.VArray)[Index]);
        stDouble:     Result := Trunc(TDoubleDynArray(ZArray.VArray)[Index]);
        stCurrency:   Result := Trunc(TCurrencyDynArray(ZArray.VArray)[Index]);
        stBigDecimal: Result := Trunc(TExtendedDynArray(ZArray.VArray)[Index]);
        stTime, stDate, stTimeStamp:
          Result := Trunc(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToCardinal(ZArray: PZArray; Index: Integer): Cardinal;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  Result := RawToUInt64Def(TRawByteStringDynArray(ZArray.VArray)[Index], 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString:  Result := UnicodeToUInt64Def(TUnicodeStringDynArray(ZArray.VArray)[Index], 0);
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then Result := UnicodeToUInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0)
      else Result := RawToUInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := Trunc(TSingleDynArray(ZArray.VArray)[Index]);
        stDouble:     Result := Trunc(TDoubleDynArray(ZArray.VArray)[Index]);
        stCurrency:   Result := Trunc(TCurrencyDynArray(ZArray.VArray)[Index]);
        stBigDecimal: Result := Trunc(TExtendedDynArray(ZArray.VArray)[Index]);
        stTime, stDate, stTimeStamp:
          Result := Trunc(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToInt64(ZArray: PZArray; Index: Integer): Int64;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  Result := RawToInt64Def(TRawByteStringDynArray(ZArray.VArray)[Index], 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString:  Result := UnicodeToInt64Def(TUnicodeStringDynArray(ZArray.VArray)[Index], 0);
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then Result := UnicodeToInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0)
      else Result := RawToInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := Trunc(TSingleDynArray(ZArray.VArray)[Index]);
        stDouble:     Result := Trunc(TDoubleDynArray(ZArray.VArray)[Index]);
        stCurrency:   Result := Trunc(TCurrencyDynArray(ZArray.VArray)[Index]);
        stBigDecimal: Result := Trunc(TExtendedDynArray(ZArray.VArray)[Index]);
        stTime, stDate, stTimeStamp:
          Result := Trunc(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToUInt64(ZArray: PZArray; Index: Integer): UInt64;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  Result := RawToUInt64Def(TRawByteStringDynArray(ZArray.VArray)[Index], 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString:  Result := UnicodeToUInt64Def(TUnicodeStringDynArray(ZArray.VArray)[Index], 0);
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then Result := UnicodeToUInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0)
      else Result := RawToUInt64Def(TZCharRecDynArray(ZArray.VArray)[Index].P, 0);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := Trunc(TSingleDynArray(ZArray.VArray)[Index]);
        stDouble:     Result := Trunc(TDoubleDynArray(ZArray.VArray)[Index]);
        stCurrency:   Result := Trunc(TCurrencyDynArray(ZArray.VArray)[Index]);
        stBigDecimal: Result := Trunc(TExtendedDynArray(ZArray.VArray)[Index]);
        stTime, stDate, stTimeStamp:
          Result := Trunc(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToCurrency(ZArray: PZArray; Index: Integer): Currency;
var P: Pointer;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        SQLStrToFloatDef(PAnsiChar(P), 0, Result, Length(TRawByteStringDynArray(ZArray.VArray)[Index]));
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString: begin
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        SQLStrToFloatDef(PWideChar(P), 0, Result, Length(TUnicodeStringDynArray(ZArray.VArray)[Index]));
      end;
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then SQLStrToFloatDef(PWideChar(TZCharRecDynArray(ZArray.VArray)[Index].P), 0,
        Result, TZCharRecDynArray(ZArray.VArray)[Index].Len)
      else SQLStrToFloatDef(PAnsiChar(TZCharRecDynArray(ZArray.VArray)[Index].P), 0,
        Result, TZCharRecDynArray(ZArray.VArray)[Index].Len);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index];
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index];
        stCurrency:   Result := TCurrencyDynArray(ZArray.VArray)[Index];
        stBigDecimal:  BCDToCurr(TBCDDynArray(ZArray.VArray)[Index], Result);
        stTime, stDate, stTimeStamp:
          Result := TDateTimeDynArray(ZArray.VArray)[Index];
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToDouble(ZArray: PZArray; Index: Integer): Double;
var P: Pointer;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        SQLStrToFloatDef(PAnsiChar(P), 0, Result, Length(TRawByteStringDynArray(ZArray.VArray)[Index]));
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}vtUnicodeString: begin
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        SQLStrToFloatDef(PWideChar(P), 0, Result, Length(TUnicodeStringDynArray(ZArray.VArray)[Index]));
      end;
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then SQLStrToFloatDef(PWideChar(TZCharRecDynArray(ZArray.VArray)[Index].P), 0,
        Result, TZCharRecDynArray(ZArray.VArray)[Index].Len)
      else SQLStrToFloatDef(PAnsiChar(TZCharRecDynArray(ZArray.VArray)[Index].P), 0,
        Result, TZCharRecDynArray(ZArray.VArray)[Index].Len);
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := Ord(TBooleanDynArray(ZArray.VArray)[Index]);
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index];
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index];
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index];
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index];
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index];
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index];
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index];
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index];
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index];
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index];
        stCurrency:   Result := TCurrencyDynArray(ZArray.VArray)[Index];
        stBigDecimal: Result := TExtendedDynArray(ZArray.VArray)[Index];
        stTime, stDate, stTimeStamp:
          Result := TDateTimeDynArray(ZArray.VArray)[Index];
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToBoolean(ZArray: PZArray; Index: Integer): Boolean;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: Result := StrToBoolEx(TRawByteStringDynArray(ZArray.VArray)[Index]);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result := StrToBoolEx(TUnicodeStringDynArray(ZArray.VArray)[Index]);
    vtCharRec:
      if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
      then Result := StrToBoolEx(PWideChar(TZCharRecDynArray(ZArray.VArray)[Index].P))
      else Result := StrToBoolEx(PAnsiChar(TZCharRecDynArray(ZArray.VArray)[Index].P));
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stBoolean:    Result := TBooleanDynArray(ZArray.VArray)[Index];
        stByte:       Result := TByteDynArray(ZArray.VArray)[Index] <> 0;
        stShort:      Result := TShortIntDynArray(ZArray.VArray)[Index]  <> 0;
        stWord:       Result := TWordDynArray(ZArray.VArray)[Index] <> 0;
        stSmall:      Result := TSmallIntDynArray(ZArray.VArray)[Index] <> 0;
        stLongWord:   Result := TCardinalDynArray(ZArray.VArray)[Index] <> 0;
        stInteger:    Result := TIntegerDynArray(ZArray.VArray)[Index] <> 0;
        stLong:       Result := TInt64DynArray(ZArray.VArray)[Index] <> 0;
        stULong:      Result := TUInt64DynArray(ZArray.VArray)[Index] <> 0;
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index] <> 0;
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index] <> 0;
        stCurrency:   Result := TCurrencyDynArray(ZArray.VArray)[Index] <> 0;
        stBigDecimal: Result := TExtendedDynArray(ZArray.VArray)[Index] <> 0;
        stTime, stDate, stTimeStamp: Result := TDateTimeDynArray(ZArray.VArray)[Index]  <> 0;
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToDate(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
var P: Pointer;
  L: LengthInt;
  B: Boolean;
label Str_Conv, Fail;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        B := False;
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        L := Length(TRawByteStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
        B := True;
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        L := Length(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    vtCharRec: begin
        P := TZCharRecDynArray(ZArray.VArray)[Index].P;
        L := TZCharRecDynArray(ZArray.VArray)[Index].Len;
        B := (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16);
Str_Conv:
        if B
        then B := TryPCharToDateTime(PWideChar(P), L, formatSettings, Result{%H-})
        else B := TryPCharToDateTime(PAnsiChar(P), L, formatSettings, Result);
        if B then goto Fail;
      end;
    vtNull, vtDateTime:  case TZSQLType(ZArray.VArrayType) of
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index];
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index];
        stBigDecimal: Result := TExtendedDynArray(ZArray.VArray)[Index];
        stTime, stDate, stTimeStamp: Result := Int(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else
Fail: raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToTime(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
var P: Pointer;
  L: LengthInt;
  B: Boolean;
label Str_Conv, Fail;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        B := False;
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        L := Length(TRawByteStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
        B := True;
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        L := Length(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    vtCharRec: begin
        P := TZCharRecDynArray(ZArray.VArray)[Index].P;
        L := TZCharRecDynArray(ZArray.VArray)[Index].Len;
        B := (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16);
Str_Conv:
        if B
        then B := TryPCharToDateTime(PWideChar(P), L, FormatSettings, Result{%H-})
        else B := TryPCharToDateTime(PAnsiChar(P), L, FormatSettings, Result);
        if not B then goto Fail;
      end;
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index];
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index];
        stBigDecimal: Result := TExtendedDynArray(ZArray.VArray)[Index];
        stTime, stDate, stTimeStamp: Result := Int(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else
Fail:  raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

function ArrayValueToDateTime(ZArray: PZArray; Index: Integer; const FormatSettings: TZFormatSettings): TDateTime;
var P: Pointer;
  L: LengthInt;
  B: Boolean;
label Str_Conv, Fail;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        B := False;
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        L := Length(TRawByteStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
        B := True;
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        L := Length(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        goto Str_Conv;
      end;
    vtCharRec: begin
        P := TZCharRecDynArray(ZArray.VArray)[Index].P;
        L := TZCharRecDynArray(ZArray.VArray)[Index].Len;
        B := (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16);
Str_Conv:
        if B
        then B := TryPCharToDateTime(PWideChar(P), L, FormatSettings, Result{%H-})
        else B := TryPCharToDateTime(PAnsiChar(P), L, FormatSettings, Result);
        if not B then goto Fail;
      end;
    vtNull:  case TZSQLType(ZArray.VArrayType) of
        stFloat:      Result := TSingleDynArray(ZArray.VArray)[Index];
        stDouble:     Result := TDoubleDynArray(ZArray.VArray)[Index];
        stBigDecimal: Result := TExtendedDynArray(ZArray.VArray)[Index];
        stTime, stDate, stTimeStamp: Result := Int(TDateTimeDynArray(ZArray.VArray)[Index]);
        else raise EZSQLException.Create(IntToStr(ZArray.VArrayType)+' '+SUnsupportedParameterType);
      end;
    else
Fail:  raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

procedure ArrayValueToGUID(ZArray: PZArray; Index: Integer; GUID: PGUID);
var P: Pointer;
label W_Conv, A_Conv, DoRaise;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    vtCharRec: begin
        P := TZCharRecDynArray(ZArray.VArray)[Index].P;
        if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
        then goto W_Conv
        else goto A_Conv;
      end;
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
A_Conv: ZSysUtils.ValidGUIDToBinary(PAnsiChar(P), @GUID.D1);
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
W_Conv: ZSysUtils.ValidGUIDToBinary(PWideChar(P), @GUID.D1);
      end;
    vtBytes: case TZSQLType(ZArray.VArrayType) of
        stGUID: begin
                  Assert(Length(TBytesDynArray(ZArray.VArray)[Index]) = SizeOf(TGUID), 'wrong guid value');
                  GUID^ := PGUID(TBytesDynArray(ZArray.VArray)[Index])^;
                end;
        else goto DoRaise;
      end;
    vtNull: case TZSQLType(ZArray.VArrayType) of
        stGUID:  GUID^ := TGUIDDynArray(ZArray.VArray)[Index];
        else goto DoRaise;
      end;
    else
DoRaise: raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

procedure ArrayValueToBCD(ZArray: PZArray; Index: Integer; var BCD: TBCD);
var P: Pointer;
  L: Integer;
label W_Conv, A_Conv, DoRaise;
begin
  {$R-}
  case ZArray.VArrayVariantType of
    vtCharRec: begin
        P := TZCharRecDynArray(ZArray.VArray)[Index].P;
        L := TZCharRecDynArray(ZArray.VArray)[Index].Len;
        if (TZCharRecDynArray(ZArray.VArray)[Index].CP = zCP_UTF16)
        then goto W_Conv
        else goto A_Conv;
      end;
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(TRawByteStringDynArray(ZArray.VArray)[Index]);
        L := Length(TRawByteStringDynArray(ZArray.VArray)[Index]);
A_Conv: Assert(ZSysUtils.TryRawToBcd(PAnsiChar(P), L, BCD, '.'), 'wrong bcd value');
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
        P := Pointer(TUnicodeStringDynArray(ZArray.VArray)[Index]);
        L := Length(TUnicodeStringDynArray(ZArray.VArray)[Index]);
W_Conv: Assert(ZSysUtils.TryUniToBcd(PWideChar(P), L, BCD, '.'), 'wrong bcd value');
      end;
    vtBigDecimal, vtCurrency, vtDouble,
    vtInteger, vtUInteger,
    vtNull: case TZSQLType(ZArray.VArrayType) of
              stBoolean:    ScaledOrdinal2BCD(Word(Ord(TBooleanDynArray(ZArray.VArray)[Index])), 0, BCD);
              stByte:       ScaledOrdinal2BCD(Word(TByteDynArray(ZArray.VArray)[Index]), 0, BCD, False);
              stShort:      ScaledOrdinal2BCD(SmallInt(TShortIntDynArray(ZArray.VArray)[Index]), 0, BCD);
              stWord:       ScaledOrdinal2BCD(TWordDynArray(ZArray.VArray)[Index], 0, BCD, False);
              stSmall:      ScaledOrdinal2BCD(TSmallIntDynArray(ZArray.VArray)[Index], 0, BCD);
              stLongWord:   ScaledOrdinal2BCD(TCardinalDynArray(ZArray.VArray)[Index], 0, BCD, False);
              stInteger:    ScaledOrdinal2BCD(TIntegerDynArray(ZArray.VArray)[Index], 0, BCD);
              stLong:       ScaledOrdinal2BCD(TInt64DynArray(ZArray.VArray)[Index], 0, BCD);
              stULong:      ScaledOrdinal2BCD(TUInt64DynArray(ZArray.VArray)[Index], 0, BCD, False);
              stFloat:      DoubleToBCD(TSingleDynArray(ZArray.VArray)[Index], BCD);
              stTime, stDate, stTimeStamp,
              stDouble:     DoubleToBCD(TDoubleDynArray(ZArray.VArray)[Index], BCD);
              stCurrency:   Currency2BCD(TCurrencyDynArray(ZArray.VArray)[Index], BCD);
              stBigDecimal: BCD := TBcdDynArray(ZArray.VArray)[Index];
              else goto DoRaise;
            end;
    else
DoRaise: raise EZSQLException.Create(IntToStr(Ord(ZArray.VArrayVariantType))+' '+SUnsupportedParameterType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

procedure SetConvertFunctions(ConSettings: PZConSettings);
begin
  FillChar(ConSettings^.ConvFuncs, SizeOf(ConSettings^.ConvFuncs), #0);

  //Let's start with the AnsiTo/From types..
  if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then begin
    //last but not least the String to/from converters
    //string represents the DataSet/IZResultSet Strings

    {$IFDEF UNICODE}
    Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRaw;
    Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;

    ConSettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;
    Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicode;
    {$ELSE}
    {String To/From Raw}
    if (ConSettings^.ClientCodePage^.CP = ConSettings^.CTRL_CP) then begin
      Consettings^.ConvFuncs.ZRawToString := @ZMoveRawToString;
      if ConSettings^.AutoEncode
      then Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode
      else Consettings^.ConvFuncs.ZStringToRaw := @ZMoveStringToRaw;
    end else if ConSettings^.AutoEncode then begin
      Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;
      Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode;
    end else begin
      Consettings^.ConvFuncs.ZStringToRaw := @ZMoveStringToRaw;
      Consettings^.ConvFuncs.ZRawToString := @ZMoveRawToString;
    end;

    {String To/From Unicode}
    if ConSettings^.CTRL_CP = zCP_UTF8
    then Consettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString_CPUTF8
    else Consettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;

    if ConSettings^.AutoEncode
    then Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicodeWithAutoEncode
    else if ConSettings^.CTRL_CP = zCP_UTF8
      then Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertString_CPUTF8ToUnicode
      else Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicode;
    {$ENDIF}
  end else begin //autoencode strings is allways true
    Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode;
    Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;
    ConSettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;
    Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicodeWithAutoEncode;
  end;
end;

function CreateCanNotAccessBlobRecordException(ColumnIndex: Integer; SQLType: TZSQLType): EZSQLException;
begin
  Result := EZSQLException.Create( Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(SQLType)]));
end;

function CreateWriteOnlyException: EZSQLException;
begin
  Result := EZSQLException.Create(Format(SOperationIsNotAllowed3, ['WRITE ONLY']));
end;

{**
  Raises operation is not allowed in READ ONLY mode exception.
}
function CreateReadOnlyException: EZSQLException;
begin
  Result := EZSQLException.Create(Format(SOperationIsNotAllowed3, ['READ ONLY']));
end;

function CreateConversionError(ColumnIndex: Integer; Actual, Expected: TZSQLType): EZSQLException;
begin
  Result := EZSQLException.Create(Format(SConvertionIsNotPossible, [ColumnIndex,
     DefineColumnTypeName(Actual), DefineColumnTypeName(Expected)]));
end;

function CreateBinaryException: EZSQLException;
begin
  Result := EZSQLException.Create(Format(SOperationIsNotAllowed3, ['BINARY']));
end;

function CreateNonBinaryException: EZSQLException;
begin
  Result := EZSQLException.Create(Format(SOperationIsNotAllowed3, ['NON BINARY']));
end;

{$IF DEFINED(ENABLE_DBLIB) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
initialization
  DBNumMultiplyLookupFiller;
{$IFEND}

end.
