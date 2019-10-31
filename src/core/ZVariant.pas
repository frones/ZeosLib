{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Variant Processing Classes                }
{                                                         }
{            Originally written by Sergey Seroukhov       }
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

unit ZVariant;

interface

{$I ZCore.inc}
uses
  {$IF not defined(FPC) and defined(MSWINDOWS)}Windows,{$IFEND}  //need for inline
  Classes, SysUtils, ZCompatibility, ZClasses, FmtBCD;

const
  {** Precision for float values comparison }
  FLOAT_COMPARE_PRECISION = 1.0e-5;
  FLOAT_COMPARE_PRECISION_SINGLE = 1.5e-5;

  {FPC - Compatibility for SQLite (currently) }
  {$IF NOT DECLARED(JulianEpoch)} // sysutils/datih.inc
  const
    JulianEpoch = -2415018.5; // "julian day 0" is January 1, 4713 BC 12:00AM
  {$IFEND}

  StrFalseUp = 'FALSE';
  StrTrueUp = 'TRUE';
  BoolStrsUp: array[Boolean] of string = (StrFalseUp, StrTrueUp);
  BoolStrsUpW: array[Boolean] of ZWideString = (ZWideString(StrFalseUp), ZWideString(StrTrueUp));
{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolStrsUpRaw: array[Boolean] of RawByteString = (RawByteString(StrFalseUp), RawByteString(StrTrueUp));
{$ELSE}
var
  BoolStrsUpRaw: array[Boolean] of RawByteString;
{$ENDIF}

type
  {** Defines variant types. }
  TZVariantType = (vtNull, vtBoolean, vtInteger, vtUInteger,
    vtDouble, vtCurrency, vtBigDecimal, vtGUID, vtBytes,
    vtString, {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}{$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF} vtRawByteString, vtUnicodeString, //String Types
    vtDate, vtTime, vtTimeStamp, vtDateTime, vtPointer, vtInterface, vtCharRec,
    vtArray{a dynamic array of [vtNull ... vtCharRec]} );

  PZArray =^TZArray;
  TZArray = Record
    VArray: Pointer; { Pointer to a Dynamic Array of X}
    VArrayType: Byte; {ord of TZSQLType}
    VArrayVariantType: TZVariantType; { better way to determine vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString, vtCharRec}
    VIsNullArray: Pointer; { Pointer to a Dynamic Array of a possible NULL indicator, might be integers or boolean types}
    VIsNullArrayType: Byte; {ord of TZSQLType}
    VIsNullArrayVariantType: TZVariantType; { better way to determine vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString, vtCharRec}
  end;

  {** Defines a variant structure. }
  TZVariant = record
    VType: TZVariantType;
    VUnicodeString: ZWideString;
    VRawByteString: RawByteString;
    VInterface: IZInterface;
    case TZVariantType of
      vtBoolean: (VBoolean: Boolean);
      vtInteger: (VInteger: Int64);
      vtUInteger: (VUInteger: UInt64);
      vtDouble: (VDouble: Double);
      vtCurrency: (VCurrency: Currency);
      vtBigDecimal: (VBigDecimal: TBCD);
      vtGUID: (VGUID: TGUID);
      vtDate: (VDate: TZDate);
      vtTime: (VTime: TZTime);
      vtTimeStamp: (VTimeStamp: TZTimeStamp);
      {$IFDEF BCC32_vtDateTime_ERROR}
      vtDateTime: (VDateTime: Double);
      {$ELSE}
      vtDateTime: (VDateTime: TDateTime);
      {$ENDIF}
      vtPointer: (VPointer: Pointer);
      vtCharRec: (VCharRec: TZCharRec);
      vtArray: (VArray: TZArray);
  end;

  PZVariant = ^TZVariant;

  {** Defines an dynamic array of variants. }
  TZVariantDynArray = array of TZVariant;

  PZVariantArray = ^TZVariantArray;
  {** Defines an array of variants. }
  TZVariantArray = array[Byte] of TZVariant;

  {** Defines a variant processing exception. }
  EZVariantException = class (Exception);

  {** Defines an interface for variant data. }
  {** Defines a Variant Manager interface. }
  IZVariantManager = interface (IZInterface)
    ['{DAA373D9-1A98-4AA8-B65E-4C23167EE83F}']

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(out Value: TZVariant);

    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
    procedure Assign(const SrcValue: TZVariant; out DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TBytes;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsUInteger(const Value: TZVariant): UInt64;
    function GetAsDouble(const Value: TZVariant): Double;
    function GetAsCurrency(const Value: TZVariant): Currency;
    procedure GetAsBigDecimal(const Value: TZVariant; Var Result: TBCD);
    procedure GetAsDate(const Value: TZVariant; Var Result: TZDate);
    procedure GetAsTime(const Value: TZVariant; Var Result: TZTime);
    procedure GetAsTimeStamp(const Value: TZVariant; Var Result: TZTimeStamp);
    procedure GetAsGUID(const Value: TZVariant; Var Result: TGUID);
    function GetAsString(const Value: TZVariant): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    {$ENDIF}
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsCharRec(const Value: TZVariant): TZCharRec;
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    {$ENDIF}
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;
    function GetAsArray(const Value: TZVariant): TZArray;

    procedure SetAsBoolean(out Value: TZVariant; const Data: Boolean);
    procedure SetAsBytes(out Value: TZVariant; const Data: TBytes);
    procedure SetAsInteger(out Value: TZVariant; const Data: Int64);
    procedure SetAsUInteger(out Value: TZVariant; const Data: UInt64);
    procedure SetAsDouble(out Value: TZVariant; const Data: Double);
    procedure SetAsCurrency(out Value: TZVariant; const Data: Currency);
    procedure SetAsBigDecimal(out Value: TZVariant; const Data: TBCD);
    procedure SetAsString(out Value: TZVariant; const Data: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(out Value: TZVariant; const Data: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(out Value: TZVariant; const Data: UTF8String);
    {$ENDIF}
    procedure SetAsRawByteString(out Value: TZVariant; const Data: RawByteString);
    procedure SetAsCharRec(out Value: TZVariant; const Data: TZCharRec);
    procedure SetAsUnicodeString(out Value: TZVariant; const Data: ZWideString);
    procedure SetAsDate(out Value: TZVariant; const Data: TZDate);
    procedure SetAsDateTime(out Value: TZVariant; const Data: TDateTime);
    procedure SetAsTime(out Value: TZVariant; const Data: TZTime);
    procedure SetAsTimeStamp(out Value: TZVariant; const Data: TZTimeStamp);
    procedure SetAsPointer(out Value: TZVariant; const Data: Pointer);
    procedure SetAsInterface(out Value: TZVariant; const Data: IZInterface);
    procedure SetAsArray(out Value: TZVariant; const Data: TZArray);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with strict convertion rules. }
  TZSoftVariantManager = class (TInterfacedObject, IZVariantManager)
  private
    FFormatSettings: TZFormatSettings;
    procedure ConvertFixedTypesToUnicode(const Value: TZVariant; var Result: UnicodeString);
    procedure ConvertFixedTypesToRaw(const Value: TZVariant; var Result: RawByteString{$IFDEF WITH_RAWBYTESTRING}; CP: Word{$ENDIF});
    procedure PRawCPConvert(Src: PAnsiChar; L: LengthInt; var Dest: RawByteString; FromCP, ToCP: Word);
    {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
    procedure RawCPConvert(const Src: RawByteString; var Dest: RawByteString; FromCP, ToCP: Word);
    {$ENDIF}
    {FrOst: !!! Performance note !!!
      This method is executed VERY often so it must be fast as possible.
    But if all the code is placed as one single piece, compiler will add LOTS
    of UStrClr, FinalizeArray etc in the method's prolog. This is caused by string
    assignments so we extract cases of every string type into separate function.
      This even beats additional time for nested function call: every string type case
    adds N finalizing so single piece of code makes N*M (M - number of string types, 6 here)
    calls while variant with nested functions only makes 1 + N calls.
      For string types conversions speedup is just 2 times and for non-string is about 40 TIMES!

      Every nested function probably could be optimized further... }

    { !! Performance note: if values returned from functions are assigned directly
    to Result's field, the compiler generates lots of string clearing functions in
    the prolog. F.ex., for ProcessAnsiString overall prolog roundtrip consists of
    2*LStrArrayClr, 2*LStrClr and 3*UStrClr. Trick with local variable results in
    1*UStrArrayClr, 1*LStrClr and 1*UStrClr (eliminating 4 function calls! }
    procedure ProcessRawByteString(const Value: TZVariant; out Result: TZVariant); virtual;
    procedure ProcessString(const Value: TZVariant; out Result: TZVariant); virtual;
    procedure ProcessUnicodeString(const Value: TZVariant; out Result: TZVariant); virtual;
    {$IFNDEF NO_ANSISTRING}procedure ProcessAnsiString(const Value: TZVariant; out Result: TZVariant); virtual; {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}procedure ProcessUTF8String(const Value: TZVariant; out Result: TZVariant); virtual; {$ENDIF NO_UTF8STRING}
    procedure ProcessCharRec(const Value: TZVariant; out Result: TZVariant); virtual;
  protected
    procedure RaiseTypeMismatchError;
    procedure RaiseUnsupportedOperation;
  public
    constructor Create;
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
    procedure Assign(const SrcValue: TZVariant; out DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(out Value: TZVariant);

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TBytes;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsUInteger(const Value: TZVariant): UInt64;
    function GetAsDouble(const Value: TZVariant): Double;
    function GetAsCurrency(const Value: TZVariant): Currency;
    procedure GetAsBigDecimal(const Value: TZVariant; Var Result: TBCD);
    procedure GetAsDate(const Value: TZVariant; Var Result: TZDate);
    procedure GetAsTime(const Value: TZVariant; Var Result: TZTime);
    procedure GetAsTimeStamp(const Value: TZVariant; Var Result: TZTimeStamp);
    procedure GetAsGUID(const Value: TZVariant; Var Result: TGUID);
    function GetAsString(const Value: TZVariant): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    {$ENDIF}
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; overload; virtual;
    function GetAsCharRec(const Value: TZVariant): TZCharRec; overload;
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;
    function GetAsArray(const Value: TZVariant): TZArray;

    procedure SetAsBoolean(out Value: TZVariant; const Data: Boolean);
    procedure SetAsBytes(out Value: TZVariant; const Data: TBytes);
    procedure SetAsInteger(out Value: TZVariant; const Data: Int64);
    procedure SetAsUInteger(out Value: TZVariant; const Data: UInt64);
    procedure SetAsDouble(out Value: TZVariant; const Data: Double);
    procedure SetAsCurrency(out Value: TZVariant; const Data: Currency);
    procedure SetAsBigDecimal(out Value: TZVariant; const Data: TBCD);
    procedure SetAsGUID(out Value: TZVariant; const Data: TGUID);
    procedure SetAsString(out Value: TZVariant; const Data: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(out Value: TZVariant; const Data: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(out Value: TZVariant; const Data: UTF8String);
    {$ENDIF}
    procedure SetAsRawByteString(out Value: TZVariant; const Data: RawByteString);
    procedure SetAsCharRec(out Value: TZVariant; const Data: TZCharRec);
    procedure SetAsUnicodeString(out Value: TZVariant; const Data: ZWideString);
    procedure SetAsDate(out Value: TZVariant; const Data: TZDate);
    procedure SetAsDateTime(out Value: TZVariant; const Data: TDateTime);
    procedure SetAsTime(out Value: TZVariant; const Data: TZTime);
    procedure SetAsTimeStamp(out Value: TZVariant; const Data: TZTimeStamp);
    procedure SetAsPointer(out Value: TZVariant; const Data: Pointer);
    procedure SetAsInterface(out Value: TZVariant; const Data: IZInterface);
    procedure SetAsArray(out Value: TZVariant; const Data: TZArray);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with connection related convertion rules. }
  IZClientVariantManager = Interface(IZVariantManager)
    ['{73A1A2C7-7C38-4620-B7FE-2426BF839BE5}']
    function UseWComparsions: Boolean;
  End;

  {** Implements a variant manager with connection related convertion rules. }
  TZClientVariantManager = class (TZSoftVariantManager, IZVariantManager, IZClientVariantManager)
  private
    FConSettings: PZConSettings;
    FClientCP, FCtrlsCP: Word;
    FUseWComparsions: Boolean;
    procedure ProcessString(const Value: TZVariant; out Result: TZVariant); override;
    procedure ProcessUnicodeString(const Value: TZVariant; out Result: TZVariant); override;
    procedure ProcessRawByteString(const Value: TZVariant; out Result: TZVariant); override;
    {$IFNDEF NO_ANSISTRING}procedure ProcessAnsiString(const Value: TZVariant; out Result: TZVariant); override; {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}procedure ProcessUTF8String(const Value: TZVariant; out Result: TZVariant); override; {$ENDIF NO_UTF8STRING}
    procedure ProcessCharRec(const Value: TZVariant; out Result: TZVariant); override;
  public
    constructor Create(const ConSettings: PZConSettings{; FormatSettings: TZFormatSettings});
    function UseWComparsions: Boolean;
    function GetAsDateTime(const Value: TZVariant): TDateTime; reintroduce;
  end;

type

  {** Represents any value interface. }
  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TBytes;
    function GetInteger: Int64;
    function GetUInteger: UInt64;
    function GetDouble: Double;
    function GetCurrency: Currency;
    function GetBigDecimal: TBCD;
    function GetDate: TZDate;
    function GetTime: TZTime;
    function GetTimeStamp: TZTimeStamp;
    function GetString: String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    {$ENDIF}
    function GetUnicodeString: ZWideString;
    function GetDateTime: TDateTime;
  end;

  {** Implements an any value object. }
  TZAnyValue = class(TZAbstractObject, IZAnyValue, IZComparable)
  private
    FValue: TZVariant;
  public
    constructor Create(const Value: TZVariant);
    constructor CreateWithBoolean(Value: Boolean);
    constructor CreateWithInteger(const Value: Int64);
    constructor CreateWithUInteger(const Value: UInt64);
    constructor CreateWithDouble(const Value: Double);
    constructor CreateWithCurrency(const Value: Currency);
    constructor CreateWithBigDecimal(const Value: TBCD);
    constructor CreateWithString(const Value: String);
    {$IFDEF UNICODE}
    // unicodeType is a (dummy) default parameter to avoid
    // the problem described in https://forums.codegear.com/thread.jspa?messageID=65681
    // when dcc creates header (.hpp)-files for c++ builder. Both 'String' and
    // 'UnicodeString' translate into 'UnicodeString' in C++ builder 2009/2010, and
    // CreateWithString and CreateWithUnicodeString would result in duplicate
    // C++ constructors.
    constructor CreateWithUnicodeString(const Value: String; unicodeType: Boolean=true);
    {$ELSE}
    constructor CreateWithUnicodeString(const Value: WideString);
    {$ENDIF}
    constructor CreateWithDateTime(const Value: TDateTime);
    constructor CreateWithDate(const Value: TZDate);
    constructor CreateWithTime(const Value: TZTime);
    constructor CreateWithTimeStamp(const Value: TZTimeStamp);

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TBytes;
    function GetInteger: Int64;
    function GetUInteger: UInt64;
    function GetDouble: Double;
    function GetCurrency: Currency;
    function GetBigDecimal: TBCD;
    function GetString: String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    {$ENDIF}
    function GetUnicodeString: ZWideString;
    function GetDate: TZDate;
    function GetTime: TZTime;
    function GetTimeStamp: TZTimeStamp;
    function GetDateTime: TDateTime;

    function Equals(const Value: IZInterface): Boolean; override;
    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

{**
  Initializes a custom variant value.
  @param Value a custom variant value to be initialized.
  @param vType new variant type to be initialized.
}
procedure InitializeVariant({$IFDEF FPC}Out{$ELSE}var{$ENDIF} Value: TZVariant; vType: TZVariantType);
{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;

{**
  Encodes null into a custom variant.
  @returns an decoded custom variant.
}
function EncodeNull : TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a boolean into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBoolean(const Value: Boolean): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a Byte array into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBytes(const Value: TBytes): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a GUID into a custom variant.
  @param Value a GUID to be encoded.
  @returns an encoded custom variant.
}
function EncodeGUID(const Value: TGUID): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an integer into a custom variant.
  @param Value an intger value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInteger(const Value: Int64): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an integer into a custom variant.
  @param Value an intger value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUInteger(const Value: UInt64): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Encodes a double into a custom variant.
  @param Value a double value to be encoded.
  @returns an encoded custom variant.
}
function EncodeDouble(const Value: Double): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a Currency into a custom variant.
  @param Value a Currency value to be encoded.
  @returns an encoded custom variant.
}
function EncodeCurrency(const Value: Currency): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a BCD into a custom variant.
  @param Value a BCD value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBigDecimal(const Value: TBCD): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Encodes a String into a custom variant.
  @param Value a String value to be encoded.
  @returns an encoded custom variant.
}
function EncodeString(const Value: String): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$IFNDEF NO_ANSISTRING}
{**
  Encodes a AnsiString into a custom variant.
  @param Value a AnsiString value to be encoded.
  @returns an encoded custom variant.
}
function EncodeAnsiString(const Value: AnsiString): TZVariant;  {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}
{**
  Encodes a UTF8String into a custom variant.
  @param Value a UTF8String value to be encoded.
  @returns an encoded custom variant.
}
{$IFNDEF NO_UTF8STRING}
function EncodeUTF8String(const Value: UTF8String): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}
{**
  Encodes a RawByteString into a custom variant.
  @param Value a RawByteString value to be encoded.
  @param CP the CoodePage of the Value string.
  @returns an encoded custom variant.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a TZCharRec into a custom variant.
  @param Value a TZCharRec value to be encoded.
  @returns an encoded custom variant.
}
function EncodeCharRec(const Value: TZCharRec): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a unicodestring into a custom variant.
  @param Value a unicodestring value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUnicodeString(const Value: ZWideString): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a TDateTime into a custom variant.
  @param Value a TDateTime value to be encoded.
  @returns an encoded custom variant.
}
function EncodeDateTime(const Value: TDateTime): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Encodes a Time value into a custom variant.
  @param Value a TZTime value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZTime(const Value: TZTime): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a Date value into a custom variant.
  @param Value a TZDate value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZDate(const Value: TZDate): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a TimeStamp value into a custom variant.
  @param Value a TZTimeStamp value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZTimeStamp(const Value: TZTimeStamp): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a pointer into a custom variant.
  @param Value a pointer value to be encoded.
  @returns an encoded custom variant.
}
function EncodePointer(const Value: Pointer): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an interface into a custom variant.
  @param Value an interface value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInterface(const Value: IZInterface): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Encodes an TZArray into a custom variant.
  @param Value an interface value to be encoded.
  @returns an encoded custom variant.
}
function EncodeArray(const Value: TZArray): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

var
  {** Declares a variant manager with soft convertion rules. }
  SoftVarManager: IZVariantManager;

  {** A NULL Variant Value. }
  NullVariant: TZVariant;

implementation

uses
  Variants, Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZMessages, ZEncoding, ZFastCode, ZSysUtils;

{ TZDefaultVariantManager }

{**
  Constructs this object and assigns the main properties.
}
constructor TZSoftVariantManager.Create;
begin
  inherited;
  FFormatSettings.DateFormat :=  DefDateFormatYMD;
  FFormatSettings.DateFormatLen := Length(FFormatSettings.DateFormat);
  FFormatSettings.TimeFormat := DefTimeFormat;
  FFormatSettings.TimeFormatLen := Length(FFormatSettings.TimeFormat);
  FFormatSettings.DateTimeFormat := FFormatSettings.DateFormat+' '+FFormatSettings.TimeFormat;
  FFormatSettings.DateTimeFormatLen := Length(FFormatSettings.DateTimeFormat);
end;

{**
  Assigns one variant value to another one.
  @param SrcValue a source variant value.
  @param DstValue a destination variant value.
}
procedure TZSoftVariantManager.Assign(const SrcValue: TZVariant;
  out DstValue: TZVariant);
begin
  DstValue.VType := SrcValue.VType;
  case SrcValue.VType of
    vtBoolean: DstValue.VBoolean := SrcValue.VBoolean;
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    vtBytes: DstValue.VRawByteString := SrcValue.VRawByteString;
    vtInteger: DstValue.VInteger := SrcValue.VInteger;
    vtUInteger: DstValue.VUInteger := SrcValue.VUInteger;
    vtDouble: DstValue.VDouble := SrcValue.VDouble;
    vtCurrency: DstValue.VCurrency := SrcValue.VCurrency;
    vtBigDecimal: DstValue.VBigDecimal := SrcValue.VBigDecimal;
    vtCharRec: DstValue.VCharRec := SrcValue.VCharRec;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: DstValue.VUnicodeString := SrcValue.VUnicodeString;
    vtGUID: DstValue.VGUID := SrcValue.VGUID;
    vtDate: DstValue.VDate := SrcValue.VDate;
    vtTime: DstValue.VTime := SrcValue.VTime;
    vtTimeStamp: DstValue.VTimeStamp := SrcValue.VTimeStamp;
    vtDateTime: DstValue.VDateTime := SrcValue.VDateTime;
    vtPointer: DstValue.VPointer := SrcValue.VPointer;
    vtInterface: DstValue.VInterface := SrcValue.VInterface;
  end;
end;

{**
  Clones a variant value.
  @param Value a source variant value.
  @returns a clonned variant value.
}
function TZSoftVariantManager.Clone(const Value: TZVariant): TZVariant;
begin
  Assign(Value, Result);
end;

{**
  Raises a type mismatch exception.
}
procedure TZSoftVariantManager.RaiseTypeMismatchError;
begin
  raise EZVariantException.Create(STypesMismatch);
end;

{**
  Raises an unsupported operation exception.
}
procedure TZSoftVariantManager.RaiseUnsupportedOperation;
begin
  raise EZVariantException.Create(SUnsupportedOperation);
end;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure TZSoftVariantManager.RawCPConvert(const Src: RawByteString;
  var Dest: RawByteString; FromCP, ToCP: Word);
begin
  PRawCPConvert(Pointer(Src), Length(Src), Dest, FromCP, ToCP);
end;
{$ENDIF WITH_TBYTES_AS_RAWBYTESTRING}

{**
  Compares two variant values.
  @param Value1 the first variant value.
  @param Value2 the second variant value.
  @return <0 if Value1 < Value 2, =0 if Value1 = Value2, >0 if Value1 > Value2
}
function TZSoftVariantManager.Compare(const Value1,
  Value2: TZVariant): Integer;
var
  ABCD: TBCD;
  AGUID: TGUID absolute ABCD;
  ATimeStamp: TZTimeStamp absolute ABCD;
  ADate: TZDate absolute ABCD;
  ATime: TZTime absolute ABCD;
  i: Int64 absolute ABCD;
  U: Int64 absolute ABCD;
  TempDateTime: TDateTime absolute ABCD;
  function CompareDiff(const Diff: Double): Integer;
  begin
    Result :=  Ord(Diff > FLOAT_COMPARE_PRECISION)-Ord(Diff < -FLOAT_COMPARE_PRECISION)
  end;
  function CompareCurr(const v1, v2: Currency): Integer;
  begin
    Result := Ord(V1 > V2)-Ord(V1 < V2);
  end;
label DoWideCompare;
begin
  case Value1.VType of
    vtNull: if IsNull(Value2) then
              Result := 0 else
              Result := -1;
    vtBoolean:
      Result := Ord(Value1.VBoolean)-Ord(GetAsBoolean(Value2));
    vtInteger: begin
      //EH: aware of range overflow(result is an integer not a Int64) comparing hashes leads to pain:
        i := GetAsInteger(Value2);
        Result := Ord(Value1.VInteger > I)-Ord(Value1.VInteger < I);
      end;
    vtUInteger: begin
      //EH: aware of range overflow(result is an integer not a Int64) comparing hashes leads to pain:
        U := GetAsInteger(Value2);
        Result := Ord(Value1.VUInteger > U)-Ord(Value1.VUInteger < U);
      end;
    vtDouble: Result := CompareDiff(Value1.VDouble-GetAsDouble(Value2));
    vtCurrency: Result := CompareCurr(Value1.VCurrency, GetAsCurrency(Value2));
    vtBigDecimal: begin
                    GetAsBigDecimal(Value2, ABCD);
                    Result := BCDCompare(Value1.VBigDecimal, ABCD);
                  end;
    vtGUID:      begin
                   GetAsGUID(Value2, AGUID);
                   Result := ZSysUtils.ZMemLComp(@Value1.VGUID.D1, @AGUID.D1, SizeOf(TGUID));
                 end;
    {$IFNDEF UNICODE}
    vtString:
      Result := AnsiCompareStr(Value1.VRawByteString, GetAsString(Value2));
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Value1.VRawByteString, GetAsAnsiString(Value2));
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String,
    {$ENDIF}
    vtRawByteString:
DoWideCompare:
      {$IF defined(Delphi) and defined(UNICODE)}
      Result := AnsiCompareStr(GetAsUnicodeString(Value1), GetAsUnicodeString(Value2));
      {$ELSE}
      Result := WideCompareStr(GetAsUnicodeString(Value1), GetAsUnicodeString(Value2));
      {$IFEND}
    vtCharRec:
      if ZCompatibleCodePages(Value1.VCharRec.CP, zCP_UTF16) then
        {$IFDEF UNICODE}
        Result := AnsiStrComp(PWideChar(Value1.VCharRec.P), PWideChar(GetAsUnicodeString(Value2)))
        {$ELSE}
        Result := WideCompareStr(PWideChar(Value1.VCharRec.P), PWideChar(GetAsUnicodeString(Value2)))
        {$ENDIF}
      else
      {$IFNDEF NO_ANSISTRING}
        if ZCompatibleCodePages(Value1.VCharRec.CP, ZOSCodePage) then
          Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrComp(PAnsiChar(Value1.VCharRec.P), PAnsiChar(Pointer(GetAsAnsiString(Value2))))
        else
      {$ENDIF}
          goto DoWideCompare;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
    {$IFDEF UNICODE}
      Result := AnsiCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
    {$ELSE}
      Result := WideCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
    {$ENDIF}
    vtDate: begin
              GetAsDate(Value2, ADate);
              Result := ZCompareDate(Value1.VDate, ADate);
            end;
    vtTime: begin
              GetAsTime(Value2, ATime);
              Result := ZCompareTime(Value1.VTime, ATime);
            end;
    vtTimeStamp: begin
              GetAsTimeStamp(Value2, ATimeStamp);
              Result := ZCompareTimeStamp(Value1.VTimeStamp, ATimeStamp);
            end;
    vtDateTime:
      begin
        TempDateTime := GetAsDateTime(Value2);
        Result := ZCompareDateTime(Value1.VDateTime, TempDateTime);
      end;
    vtPointer:
      Result := sign(Int64({%H-}NativeUInt(Value1.VPointer) - GetAsUInteger(Value2)));
    else
      Result := 0;
  end;
end;

{**
  Checks is the specified value NULL.
  @param Value a value to be checked.
  @returns <code>True</code> if variant has NULL value.
}
function TZSoftVariantManager.IsNull(const Value: TZVariant): Boolean;
begin
  Result := Value.VType = vtNull;
end;

{**
  Sets the NULL value to specified variant.
  @param Value variant value to be set to NULL.
}
procedure TZSoftVariantManager.SetNull(out Value: TZVariant);
begin
  Value := EncodeNull;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsBoolean(
  const Value: TZVariant): Boolean;
begin
  case Value.VType of
    vtNull:     Result := False;
    vtBoolean:  Result := Value.VBoolean;
    vtInteger:  Result := Value.VInteger <> 0;
    vtUInteger: Result := Value.VUInteger <> 0;
    vtDouble:   Result := Value.VDouble <> 0;
    vtCurrency: Result := Value.VCurrency <> 0;
    vtBigDecimal: Result := not (Value.VBigDecimal.Precision = 10) and (Value.VBigDecimal.SignSpecialPlaces = 2);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: Result := StrToBoolEx(Value.VRawByteString);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result := StrToBoolEx(Value.VUnicodeString);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then Result := StrToBoolEx(PWideChar(Value.VCharRec.P))
      else Result := StrToBoolEx(PAnsiChar(Value.VCharRec.P));
    vtDateTime: Result := Value.VDateTime <> 0;
    vtDate: Result := PInt64(@Value.VDate.Year)^ <> 0;
    vtTime: Result := (PCardinal(@Value.VTime.Hour)^ <> 0) or (PInt64(@Value.VTime.Second)^ <> 0);
    vtTimeStamp: Result := (PInt64(@Value.VTimeStamp.Year)^ <> 0) or (PInt64(@Value.VTimeStamp.Minute)^ <> 0)
      or (PInt64(PAnsichar(@Value.VTimeStamp.TimeZoneHour)-2)^ <> 0);
    else raise EZVariantException.Create(SErrorConvertion);
  end;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsBytes(
  const Value: TZVariant): TBytes;
begin
  Result := nil;
  case Value.VType of
    vtNull: Exit;
    vtBoolean: Result := BufferToBytes(PAnsiChar(@Value.VBoolean), SizeOf(Boolean));
    vtDate,vtInteger, vtUInteger, vtDouble, vtCurrency: Result := BufferToBytes(@Value.VInteger, 8);
    vtBigDecimal: Result := BufferToBytes(@Value.VBigDecimal.Precision, SizeOf(TBCD));
    vtTime: Result := BufferToBytes(@Value.VTime.Hour, SizeOf(TZTime));
    vtTimeStamp: Result := BufferToBytes(@Value.VTimeStamp.Year, SizeOf(TZTimeStamp));
    vtBytes{$IFNDEF UNICODE}, vtString{$ENDIF},
    {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
    vtRawByteString: Result := {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}StrToBytes{$ENDIF}(Value.VRawByteString);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
                      SetLength(Result, Length(Value.VUnicodeString) shl 1);
                      if Pointer(Result) = nil then Exit;
                      Move(Pointer(Value.VUnicodeString)^, Pointer(Result)^, Length(Result));
                    end;
    vtCharRec:  begin
                  if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
                  then SetLength(Result, Value.VCharRec.Len shl 1)
                  else SetLength(Result, Value.VCharRec.Len);
                  if Pointer(Result) = nil then Exit;
                  Move(Value.VCharRec.P^, Pointer(Result)^, Length(Result));
                end;
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to integer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsInteger(
  const Value: TZVariant): Int64;
begin
  case Value.VType of
    vtNull:       Result := 0;
    vtBoolean:    Result := Ord(Value.VBoolean);
    vtInteger:    Result := Value.VInteger;
    vtUInteger:   Result := Value.VUInteger;
    vtCurrency:   Result := PInt64(@Value.VCurrency)^ div 10000;
    vtBigDecimal: BCD2Int64(Value.VBigDecimal, Result);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: Result := RawToInt64Def(Pointer(Value.VRawByteString), 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result := UnicodeToInt64Def(Value.VUnicodeString, 0);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then Result := UnicodeToInt64Def(PWideChar(Value.VCharRec.P), PWideChar(Value.VCharRec.P)+Value.VCharRec.Len, 0)
      else Result := RawToInt64Def(PAnsiChar(Value.VCharRec.P), PAnsiChar(Value.VCharRec.P)+Value.VCharRec.Len, 0);
    vtPointer:    Result := Int64({%H-}NativeUInt(Value.VPointer));
    else Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDouble(Value));
  end;
end;

{**
  Gets a variant to UInt64 value.
  @param Value a variant to be converted.
  @param a result value.
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZSoftVariantManager.GetAsUInteger(
  const Value: TZVariant): UInt64;
begin
  case Value.VType of
    vtNull:     Result := 0;
    vtBoolean:  Result := Ord(Value.VBoolean);
    vtInteger:  Result := Value.VInteger;
    vtUInteger: Result := Value.VUInteger;
    vtCurrency: Result := PInt64(@Value.VCurrency)^ div 10000;
    vtBigDecimal:BCD2UInt64(Value.VBigDecimal, Result);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: Result := RawToUInt64Def(Pointer(Value.VRawByteString), 0);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result := UnicodeToUInt64Def(Value.VUnicodeString, 0);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then Result := UnicodeToUInt64Def(PWideChar(Value.VCharRec.P), PWideChar(Value.VCharRec.P)+Value.VCharRec.Len, 0)
      else Result := RawToUInt64Def(PAnsiChar(Value.VCharRec.P), PAnsiChar(Value.VCharRec.P)+Value.VCharRec.Len, 0);
    vtPointer:  Result := {%H-}NativeUInt(Value.VPointer);
    else Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDouble(Value));
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets a variant to date value.
  @param Value a variant to be converted.
  @param a result value.
}
procedure TZSoftVariantManager.GetAsDate(const Value: TZVariant;
  var Result: TZDate);
var P: Pointer;
Label Fail;
begin
  case Value.VType of
    vtNull, vtTime: PInt64(@Result.Year)^ := 0;
    vtDate: Result := Value.VDate;
    vtTimeStamp: DateFromTimeStamp(Value.VTimeStamp, Result);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToDate(PAnsiChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
Fail:                   raise EZVariantException.Create(SErrorConvertion);
                      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToDate(PWideChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
                        goto Fail;
                      end;
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then begin
          if not TryPCharToDate(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
            goto Fail;
        end else if not TryPCharToDate(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
          goto Fail;
    else ZSysUtils.DecodeDateTimeToDate(GetAsDouble(Value), Result);
  end;
end;

{**
  Gets a variant to double value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsDouble(
  const Value: TZVariant): Double;
  label Fail;
begin
  case Value.VType of
    vtNull:     Result := 0;
    vtBoolean:  Result := Ord(Value.VBoolean);
    vtInteger:  Result := Value.VInteger;
    vtUInteger: Result := Value.VUInteger;
    vtDouble:   Result := Value.VDouble;
    vtCurrency: Result := Value.VCurrency;
    vtBigDecimal:Result := BCDToDouble(Value.VBigDecimal);
    vtDate:     if not TryDateToDateTime(Value.VDate, PDatetime(@Result)^) then
                  Goto Fail;
    vtTime:     if not TryTimeToDateTime(Value.VTime, PDatetime(@Result)^) then
                  Goto Fail;
    vtTimeStamp:if not TryTimeStampToDateTime(Value.VTimeStamp, PDatetime(@Result)^) then
Fail:             raise EZVariantException.Create(SErrorConvertion);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  SqlStrToFloatDef(PAnsiChar(Pointer(Value.VRawByteString)), 0, Result, Length(Value.VRawByteString));
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:  SqlStrToFloatDef(PWideChar(Pointer(Value.VUnicodeString)), 0, Result, Length(Value.VUnicodeString));
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then SqlStrToFloatDef(PWideChar(Value.VCharRec.P), 0, Result, Value.VCharRec.Len)
      else SqlStrToFloatDef(PAnsiChar(Value.VCharRec.P), 0, Result, Value.VCharRec.Len);
    vtDateTime: Result := Value.VDateTime;
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to GUID value.
  @param Value a variant to be converted.
  @param a result value.
}
procedure TZSoftVariantManager.GetAsGUID(const Value: TZVariant; Var Result: TGUID);
var P: Pointer;
Label Fail;
begin
  case Value.VType of
    vtNull:     FillChar(Result, SizeOf(TGUID), #0);
    vtGUID:     Result := Value.VGUID;
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  if Length(Value.VRawByteString) in [36,38] then begin
                        P := Pointer(Value.VRawByteString);
                        ZSysUtils.ValidGUIDToBinary(PAnsiChar(P), @Result.D1);
                      end else Goto Fail;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:  if Length(Value.VUnicodeString) in [36,38] then begin
                        P := Pointer(Value.VUnicodeString);
                        ZSysUtils.ValidGUIDToBinary(PWideChar(P), @Result.D1);
                      end else Goto Fail;
    vtCharRec:        if Value.VCharRec.Len in [36,38] then
                        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
                        then ZSysUtils.ValidGUIDToBinary(PWideChar(Value.VCharRec.P), @Result.D1)
                        else ZSysUtils.ValidGUIDToBinary(PWideChar(Value.VCharRec.P), @Result.D1)
                      else Goto Fail;
    vtBytes:  if Length(Value.VRawByteString) = SizeOf(TGUID)
              then Result := PGUID(Value.VRawByteString)^
              else Goto Fail;
    else
Fail: raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to currency value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsCurrency(
  const Value: TZVariant): Currency;
begin
  case Value.VType of
    vtNull:     Result := 0;
    vtBoolean:  Result := Ord(Value.VBoolean);
    vtInteger:  Result := Value.VInteger;
    vtUInteger: Result := Value.VUInteger;
    vtCurrency: Result := Value.VCurrency;
    vtBigDecimal: BCDToCurr(Value.VBigDecimal, Result);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:  SqlStrToFloatDef(PAnsiChar(Pointer(Value.VRawByteString)), 0, Result, Length(Value.VRawByteString));
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:  SqlStrToFloatDef(PWideChar(Pointer(Value.VUnicodeString)), 0, Result, Length(Value.VUnicodeString));
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then SqlStrToFloatDef(PWideChar(Value.VCharRec.P), 0, Result, Value.VCharRec.Len)
      else SqlStrToFloatDef(PAnsiChar(Value.VCharRec.P), 0, Result, Value.VCharRec.Len);
    else Result := GetAsDouble(Value);
  end;
end;

{**
  Gets a variant to currency value.
  @param Value a variant to be converted.
  @param a result value.
}
procedure TZSoftVariantManager.GetAsBigDecimal(const Value: TZVariant;
  Var Result: TBCD);
begin
  case Value.VType of
    vtNull: Result := NullBCD;
    vtBoolean:  ScaledOrdinal2BCD(Word(Ord(Value.VBoolean)), 0, Result, False);
    vtInteger:  ScaledOrdinal2BCD(Value.VInteger, 0, Result);
    vtUInteger: ScaledOrdinal2BCD(Value.VUInteger, 0, Result, False);
    vtDouble:   Double2BCD(Value.VDouble, Result);
    vtCurrency: CurrToBCD(Value.VCurrency, Result);
    vtBigDecimal: Result := Value.VBigDecimal;
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: Result := RawToBCD(Value.VRawByteString);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result := UniToBCD(Value.VUnicodeString);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then Result := UniToBCD(PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
      else Result := RawToBCD(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len);
    vtDateTime: Double2BCD(Value.VDateTime, Result);
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsString(
  const Value: TZVariant): String;
begin
  Result := Convert(Value, vtString).{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF};
end;

{**
  Gets a variant to time value.
  @param Value a variant to be converted.
  @param a result value.
}
procedure TZSoftVariantManager.GetAsTime(const Value: TZVariant;
  var Result: TZTime);
var P: Pointer;
Label Fail;
begin
  case Value.VType of
    vtNull, vtDate: begin
              PCardinal(@Result.Hour)^ := 0;
              PInt64(@Result.Second)^ := 0;
            end;
    vtTime: Result := Value.VTime;
    vtTimeStamp: ZSysUtils.TimeFromTimeStamp(Value.VTimeStamp, Result);
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToTime(PAnsiChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
Fail:                   raise EZVariantException.Create(SErrorConvertion);
                      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToTime(PWideChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
                        goto Fail;
                      end;
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then begin
          if not TryPCharToTime(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
            goto Fail;
        end else if not TryPCharToTime(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
          goto Fail;
    else ZSysUtils.DecodeDateTimeToTime(GetAsDouble(Value), Result);
  end;
end;

procedure TZSoftVariantManager.GetAsTimeStamp(const Value: TZVariant;
  var Result: TZTimeStamp);
var P: Pointer;
Label Fail;
begin
  case Value.VType of
    vtNull: FillChar(Result, SizeOf(TZTimeStamp), #0);
    vtDate: TimeStampFromDate(Value.VDate, Result);
    vtTime: TimeStampFromTime(Value.VTime, Result);
    vtTimeStamp: Result := Value.VTimeStamp;
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToTimeStamp(PAnsiChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
Fail:                   raise EZVariantException.Create(SErrorConvertion);
                      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: begin
                      P := Pointer(Value.VRawByteString);
                      if not TryPCharToTimeStamp(PWideChar(P), Length(Value.VRawByteString), FFormatSettings, Result) then
                        goto Fail;
                      end;
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then begin
          if not TryPCharToTimeStamp(PWideChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
            goto Fail;
        end else if not TryPCharToTimeStamp(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, FFormatSettings, Result) then
          goto Fail;
    else ZSysUtils.DecodeDateTimeToTimeStamp(GetAsDouble(Value), Result);
  end;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
{$IFNDEF NO_ANSISTRING}
function TZSoftVariantManager.GetAsAnsiString(
  const Value: TZVariant): AnsiString;
begin
  Result := Convert(Value, vtAnsiString).VRawByteString;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZSoftVariantManager.GetAsUTF8String(const Value: TZVariant): UTF8String;
begin
  Result := Convert(Value, vtUTF8String).VRawByteString;
end;
{$ENDIF}

function TZSoftVariantManager.GetAsRawByteString(const Value: TZVariant): RawByteString;
begin
  Result := Convert(Value, vtRawByteString).VRawByteString;
end;

{$IFDEF FPC} // parameters not used intentionally
  {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
function TZSoftVariantManager.GetAsRawByteString(const Value: TZVariant;
  const RawCP: Word): RawByteString;
begin
  RaiseUnsupportedOperation
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZSoftVariantManager.GetAsCharRec(const Value: TZVariant): TZCharRec;
begin
  Result := Convert(Value, vtCharRec).VCharRec;
end;

{**
  Gets a variant to unicode string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsUnicodeString(
  const Value: TZVariant): ZWideString;
begin
  Result := Convert(Value, vtUnicodeString).VUnicodeString;
end;

{**
  Gets a variant to date and time value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsDateTime(
  const Value: TZVariant): TDateTime;
begin
  case Value.VType of
    vtNull: Result := 0;
    {$IFNDEF UNICODE}vtString, {$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString:
      Result := AnsiSQLDateToDateTime(Value.VRawByteString);
    {$IFDEF UNICODE}vtString, {$ENDIF}
    vtUnicodeString:
      Result := AnsiSQLDateToDateTime(Value.VUnicodeString);
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then Result := AnsiSQLDateToDateTime(PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
      else Result := AnsiSQLDateToDateTime(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len);
    else Result := GetAsDouble(Value);
  end;
end;

{**
  Gets a variant to pointer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsPointer(
  const Value: TZVariant): Pointer;
begin
  case Value.VType of
    vtNull: Result := nil;
    vtInteger: Result := {%H-}Pointer(Value.VInteger);
    vtUInteger: Result := {%H-}Pointer(Value.VUInteger);
    {$IFNDEF UNICODE}vtString, {$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtBytes, vtRawByteString: Result := Pointer(Value.VRawByteString);
    vtCharRec: Result := Value.VCharRec.P;
    vtUnicodeString: Result := Pointer(Value.VUnicodeString);
    vtinterface: Result := Pointer(Value.VInterface);
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to interface value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsInterface(
  const Value: TZVariant): IZInterface;
begin
  case Value.VType of
    vtNull:
      Result := nil;
    vtInterface:
      Result := Value.VInterface;
    else raise EZVariantException.Create(STypesMismatch);
  end;
end;

{**
  Gets a variant to TZArray value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZSoftVariantManager.GetAsArray(
  const Value: TZVariant): TZArray;
begin
  Result := Convert(Value, vtArray).VArray;
end;

{**
  Assigns a boolean value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsBoolean(out Value: TZVariant;
  const Data: Boolean);
begin
  Value := EncodeBoolean(Data);
end;

{**
  Assigns a Byte array value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsBytes(out Value: TZVariant;
  const Data: TBytes);
begin
  Value := EncodeBytes(Data);
end;

{**
  Assigns an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsInteger(out Value: TZVariant;
  const Data: Int64);
begin
  Value := EncodeInteger(Data);
end;

{**
  Assigns an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsUInteger(out Value: TZVariant;
  const Data: UInt64);
begin
  Value := EncodeUInteger(Data);
end;

{**
  Assigns a double value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsDouble(out Value: TZVariant;
  const Data: Double);
begin
  Value := EncodeDouble(Data);
end;

{**
  Assigns a GUID value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsGUID(out Value: TZVariant;
  const Data: TGUID);
begin
  Value := EncodeGUID(Data);
end;

{**
  Assigns a currency value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsCurrency(out Value: TZVariant;
  const Data: Currency);
begin
  Value := EncodeCurrency(Data);
end;

{**
  Assigns a bigdecimal value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsBigDecimal(out Value: TZVariant;
  const Data: TBCD);
begin
  Value := EncodeBigDecimal(Data);
end;

{**
  Assigns a String value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsString(out Value: TZVariant;
  const Data: String);
begin
  Value := EncodeString(Data);
end;

{**
  Assigns a time value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsTime(out Value: TZVariant;
  const Data: TZTime);
begin
  Value.VType := vtTime;
  Value.VTime := Data;
end;

{**
  Assigns a timestamp value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsTimeStamp(out Value: TZVariant;
  const Data: TZTimeStamp);
begin
  Value.VType := vtTimeStamp;
  Value.VTimeStamp := Data;
end;

{**
  Assigns a AnsiString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
{$IFNDEF NO_ANSISTRING}
procedure TZSoftVariantManager.SetAsAnsiString(out Value: TZVariant;
  const Data: AnsiString);
begin
  Value := EncodeAnsiString(Data);
end;
{$ENDIF}

{**
  Assigns a UTF8string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
{$IFNDEF NO_UTF8STRING}
procedure TZSoftVariantManager.SetAsUTF8String(out Value: TZVariant;
  const Data: UTF8String);
begin
  Value := EncodeUTF8String(Data);
end;
{$ENDIF}

{**
  Assigns a RawByteString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsRawByteString(out Value: TZVariant;
  const Data: RawByteString);
begin
  Value := EncodeRawByteString(Data);
end;

{**
  Assigns a RawByteString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsCharRec(out Value: TZVariant;
  const Data: TZCharRec);
begin
  Value := EncodeCharRec(Data);
end;

{**
  Assigns a unicode string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsUnicodeString(out Value: TZVariant;
  const Data: ZWideString);
begin
  Value := EncodeUnicodeString(Data);
end;

{**
  Assigns a date value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsDate(out Value: TZVariant;
  const Data: TZDate);
begin
  Value.VType := vtDate;
  Value.VDate := Data;
end;

{**
  Assigns a datetime value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsDateTime(out Value: TZVariant;
  const Data: TDateTime);
begin
  Value := EncodeDateTime(Data);
end;

{**
  Assigns a pointer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsPointer(out Value: TZVariant;
  const Data: Pointer);
begin
  Value := EncodePointer(Data);
end;

{**
  Assigns a interface value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsInterface(out Value: TZVariant;
  const Data: IZInterface);
begin
  Value := EncodeInterface(Data);
end;

{**
  Assigns a TZArray value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZSoftVariantManager.SetAsArray(out Value: TZVariant;
  const Data: TZArray);
begin
  Value := EncodeArray(Data);
end;

{**
  Performs '+' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpAdd(const Value1,
  Value2: TZVariant): TZVariant;
var BCD: TBCD;
  i: Int64 absolute BCD;
  u: UInt64 absolute I;
  D: Double absolute I;
  C: Currency absolute i;
begin
  InitializeVariant(Result, Value1.VType);
  case Value1.VType of
    vtNull: ;
    vtInteger: begin
                i := GetAsInteger(Value2);
                Result.VInteger := Value1.VInteger + i;
              end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: begin
                  U := GetAsUInteger(Value2);
                  Result.VUInteger := Value1.VUInteger + U;
                end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtDouble:   begin
                  D := GetAsDouble(Value2);
                  Result.VDouble := Value1.VDouble + D;
                end;
    vtCurrency: begin
                  D := GetAsCurrency(Value2);
                  Result.VCurrency := Value1.VCurrency + C;
                end;
    vtBigDecimal: begin
                    GetAsBigDecimal(Value2, BCD);
                    BCDAdd(Value1.VBigDecimal, BCD, Result.VBigDecimal);
                  end;
    {$IFNDEF UNICODE}
    vtString: Result.VRawByteString := Value1.VRawByteString + GetAsString(Value2);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: Result.VRawByteString := Value1.VRawByteString + GetAsAnsiString(Value2);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: Result.VRawByteString := Value1.VRawByteString + GetAsUTF8String(Value2);
    {$ENDIF}
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    vtRawByteString: Result.VRawByteString := RawConcat([Value1.VRawByteString,GetAsRawByteString(Value2)]);
    {$ELSE}
    vtRawByteString: Result.VRawByteString := Value1.VRawByteString + GetAsRawByteString(Value2);
    {$ENDIF}
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: Result.VUnicodeString := Value1.VUnicodeString + GetAsUnicodeString(Value2);
    vtDateTime: Result := EncodeDateTime(Value1.VDateTime + GetAsDateTime(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '&' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpAnd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean and GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger and GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger and GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '/' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpDiv(const Value1,
  Value2: TZVariant): TZVariant;
var
  BCD: TBCD;
  i64: Int64 absolute BCD;
  u: UInt64 absolute I64;
  D: Double absolute I64;
  C: Currency absolute i64;
begin
  InitializeVariant(Result, Value1.VType);
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: begin
                i64 := GetAsInteger(Value2);
                Result.VInteger := Value1.VInteger div i64;
              end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: begin
                  U := GetAsUInteger(Value2);
                  Result.VUInteger := Value1.VUInteger div U;
                end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtDouble: begin
                D := GetAsDouble(Value2);
                Result.VDouble := Value1.VDouble / D;
              end;
    vtCurrency: begin
                  c := GetAsCurrency(Value2);
                  PInt64(@Result.VCurrency)^ := PInt64(@Value1.VCurrency)^ div PInt64(@C)^;
                end;
    vtBigDecimal: begin
                    InitializeVariant(Result, vtBigDecimal);
                    GetAsBigDecimal(Value2, BCD);
                    BcdDivide(Value1.vBigDecimal, BCD, Result.VBigDecimal);
                  end;
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) = 0);
end;

{**
  Performs '<' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpLess(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) < 0);
end;

{**
  Performs '<=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpLessEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) <= 0);
end;

{**
  Performs '%' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpMod(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger mod GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger mod GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpMore(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) > 0);
end;

{**
  Performs '>=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpMoreEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) >= 0);
end;

{**
  Performs '*' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpMul(const Value1,
  Value2: TZVariant): TZVariant;
var BCD: TBCD;
  i: Int64 absolute BCD;
  u: UInt64 absolute I;
  D: Double absolute I;
  C: Currency absolute i;
begin
  InitializeVariant(Result, Value1.VType);
  case Value1.VType of
    vtNull: ;
    vtInteger: begin
                I := GetAsInteger(Value2);
                Result.VInteger := Value1.VInteger * I;
              end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: begin
                  U := GetAsUInteger(Value2);
                  Result.VUInteger := Value1.VUInteger * U;
                end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtDouble:   begin
                  D := GetAsDouble(Value2);
                  Result.VDouble := Value1.VDouble * D;
                end;
    vtCurrency: begin
                  C := GetAsCurrency(Value2);
                  Result.VCurrency := Value1.VCurrency * C;
                end;
    vtBigDecimal: begin
                    GetAsBigDecimal(Value2, BCD);
                    BcdMultiply(Value1.vBigDecimal, BCD, Result.VBigDecimal);
                  end;
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs unary '-' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpNegative(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(-Value.VInteger);
    vtDouble: Result := EncodeDouble(-Value.VDouble);
    vtCurrency: Result := EncodeCurrency(-Value.VCurrency);
    vtBigDecimal: begin
                    InitializeVariant(Result, vtBigDecimal);
                    BcdMultiply(Value.VBigDecimal, StrToBCD('-1'), Result.VBigDecimal);
                  end;
    vtTime: begin
              InitializeVariant(Result, VTTime);
              Result.VTime := Value.VTime;
              Result.VTime.IsNegative := not Result.VTime.IsNegative;
            end;
    vtDate: begin
              InitializeVariant(Result, vtDate);
              Result.VDate := Value.VDate;
              Result.VDate.IsNegative := not Result.VDate.IsNegative;
            end;
    vtTimeStamp: begin
              InitializeVariant(Result, vtTimeStamp);
              Result.VTimeStamp := Value.VTimeStamp;
              Result.VTimeStamp.IsNegative := not Result.VTimeStamp.IsNegative;
            end;
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '~' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpNot(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(not Value.VBoolean);
    vtInteger: Result := EncodeInteger(not Value.VInteger);
    vtUInteger: Result := EncodeUInteger(not Value.VUInteger);
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '<>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpNotEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) <> 0);
end;

{**
  Performs '|' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpOr(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result{%H-});
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean or GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger or GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VInteger or GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpPow(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeDouble(Power(Value1.VInteger, GetAsInteger(Value2)));
    vtUInteger: Result := EncodeDouble(Power(Value1.VUInteger, GetAsUInteger(Value2)));
    vtDouble: Result := EncodeDouble(Power(Value1.VDouble, GetAsDouble(Value2)));
    vtCurrency: Result := EncodeDouble(Power(Value1.VCurrency, GetAsCurrency(Value2)));
    vtBigDecimal: Result := EncodeDouble(Power(BcdToDouble(Value1.VBigDecimal), GetAsDouble(Value2)));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '-' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpSub(const Value1,
  Value2: TZVariant): TZVariant;
var BCD: TBCD;
  I: Int64 absolute BCD;
  u: UInt64 absolute I;
  D: Double absolute I;
  C: Currency absolute I;
begin
  InitializeVariant(Result, Value1.VType);
  case Value1.VType of
    vtNull: ;
    vtInteger: begin
                I := GetAsInteger(Value2);
                Result.VInteger := Value1.VInteger - I;
              end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: begin
                  U := GetAsUInteger(Value2);
                  Result.VUInteger := Value1.VUInteger - U;
                end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtDouble:   begin
                  D := GetAsDouble(Value2);
                  Result.VDouble := Value1.VDouble - D;
                end;
    vtCurrency: begin
                  C := GetAsCurrency(Value2);
                  Result.VCurrency := Value1.VCurrency - C;
                end;
    vtBigDecimal: begin
                    GetAsBigDecimal(Value2, BCD);
                    BcdSubtract(Value1.VBigDecimal, BCD, Result.VBigDecimal);
                  end;
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZSoftVariantManager.OpXor(const Value1,
  Value2: TZVariant): TZVariant;
var
  TempBool1, TempBool2: Boolean;
  TempInteger1, TempInteger2: Int64;
  TempUInteger1, TempUInteger2: UInt64;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtBoolean:
      begin
        TempBool1 := Value1.VBoolean;
        TempBool2 := GetAsBoolean(Value2);
        Result := EncodeBoolean((TempBool1 and not TempBool2)
          or (not TempBool1 and TempBool2));
      end;
    vtInteger:
      begin
        TempInteger1 := Value1.VInteger;
        TempInteger2 := GetAsInteger(Value2);
        Result := EncodeInteger((TempInteger1 and not TempInteger2)
          or (not TempInteger1 and TempInteger2));
      end;
    vtUInteger:
      begin
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        TempUInteger1 := Value1.VUInteger;
        TempUInteger2 := GetAsUInteger(Value2);
        Result := EncodeUInteger((TempUInteger1 and not TempUInteger2)
          or (not TempUInteger1 and TempUInteger2));
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      end;
    else RaiseUnsupportedOperation;
  end;
end;

procedure TZSoftVariantManager.PRawCPConvert(Src: PAnsiChar; L: LengthInt;
  var Dest: RawByteString; FromCP, ToCP: Word);
var W: UnicodeString;
begin
  W := PRawToUnicode(Src, L, FromCP);
  Dest := ZUnicodeToRaw(W, ToCP);
end;

{$IFNDEF NO_ANSISTRING}
procedure TZSoftVariantManager.ProcessAnsiString(const Value: TZVariant;
  out Result: TZVariant);
label FromW;
var ResTmp: RawByteString;
begin
  Result.VType := vtAnsiString;
  case Value.VType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    vtAnsiString,
    vtBytes, vtRawByteString: ResTmp := Value.VRawByteString;
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: if ZCompatibleCodePages(ZOSCodePage, zCP_UTF8) then
                    ResTmp := Value.VRawByteString
                  else begin
                    Result.VUnicodeString := PRawToUnicode(Pointer(Value.VRawByteString),
                      Length(Value.VRawByteString), zCP_UTF8);
                    goto FromW;
                  end;
    {$ENDIF}
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := ZUnicodeToRaw(Value.VUnicodeString, ZOSCodePage);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
      else if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
      else begin
        Result.VUnicodeString := PRawToUnicode(Value.
          VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
FromW:    ResTmp := ZUnicodeToRaw(Result.VUnicodeString, ZOSCodePage);
        Result.VUnicodeString := '';
      end
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_ANSISTRING}

procedure TZSoftVariantManager.ProcessCharRec(const Value: TZVariant;
  out Result: TZVariant);
label {$IFDEF UNICODE}AsVCharRecFromVString, {$ENDIF}AsVCharRecFromRaw;
begin
  Result.VType := vtCharRec;
  case Value.VType of
    vtNull: begin
        Result.VCharRec.Len := 0;
        Result.VCharRec.CP := High(Word);
        Result.VCharRec.P := nil;
      end;
    vtBoolean, vtInteger, vtUInteger, vtDouble, vtCurrency, vtBigDecimal,
    vtBytes, vtDate, vtTime, vtTimeStamp, vtDateTime: begin
        {$IFDEF UNICODE}
        ConvertFixedTypesToUnicode(Value, Result.VUnicodeString);
        Goto AsVCharRecFromVString;
        {$ELSE}
        ConvertFixedTypesToRaw(Value, Result.VRawByteString{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF});
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
        {$ENDIF}
      end;
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: begin
        Result.VCharRec.CP := zCP_UTF8;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    {$IFNDEF UNICODE}
    vtString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto AsVCharRecFromRaw;
      end;
    {$ENDIF}
    vtRawByteString:
      begin
        Result.VCharRec.CP := High(Word);
AsVCharRecFromRaw:
        Result.VRawByteString := Value.VRawByteString;
        if Pointer(Result.VRawByteString) = nil then begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.P := PEmptyAnsiString;
        end else begin
          Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
          Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL call of PChar conversion
        end;
      end;
    {$IFDEF UNICODE}vtString, {$ENDIF}
    vtUnicodeString:
      begin
{$IFDEF UNICODE}AsVCharRecFromVString:{$ENDIF}
        Result.VUnicodeString := Value.VUnicodeString;
        Result.VCharRec.CP := zCP_UTF16;
        Result.VCharRec.Len := Length(Result.VUnicodeString); //don't use PLengthInt helper: VUnicodeString may be Wide/Unicode-String
        if Result.VCharRec.Len = 0
        then Result.VCharRec.P := PEmptyUnicodeString
        else Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL call of PWideChar conversion
      end;
    vtCharRec:
      Result.VCharRec := Value.VCharRec;
    else
      RaiseTypeMismatchError;
  end;
end;

procedure TZSoftVariantManager.ProcessRawByteString(const Value: TZVariant;
  out Result: TZVariant);
begin
  Result.VType := vtRawByteString;
  ConvertFixedTypesToRaw(Value, Result.VRawByteString{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF});
end;

procedure TZSoftVariantManager.ProcessString(const Value: TZVariant;
  out Result: TZVariant);
var Tmp: {$IFDEF UNICODE}UnicodeString{$ELSE}RawByteString{$ENDIF};
begin
  Result.VType := vtString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: Tmp := Value.VRawByteString;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      {$IFDEF UNICODE}
      Tmp := PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), ZOSCodePage);
      {$ELSE}
      Tmp := Value.VRawByteString;
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:
      {$IFDEF UNICODE}
      Tmp := PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), zCP_UTF8);
      {$ELSE}
      if ZCompatibleCodePages(ZOSCodePage, zCP_UTF8)
      then Tmp := Value.VRawByteString
      else RawCPConvert(Value.VRawByteString, Tmp, zCP_UTF8, ZOSCodePage);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      {$IFDEF UNICODE}
      Tmp := Value.VUnicodeString;
      {$ELSE}
      Tmp := String(Value.VUnicodeString); //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
      {$ENDIF}
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        {$IFDEF UNICODE}
        SetString(Tmp, PChar(Value.VCharRec.P), Value.VCharRec.Len)
        {$ELSE}
        Tmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
        {$ENDIF}
      else
      {$IFNDEF UNICODE}
      if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
        ZSetString(PChar(Value.VCharRec.P), Value.VCharRec.Len, Tmp)
      else
      {$ENDIF}
      begin
      {$IFNDEF UNICODE}
        Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
        Tmp := String(Result.VUnicodeString);
        Result.VUnicodeString := '';
        {$ELSE}
        Tmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
        {$ENDIF}
      end;
    else {$IFDEF UNICODE}ConvertFixedTypesToUnicode{$ELSE}ConvertFixedTypesToRaw{$ENDIF}(Value, Tmp{$IF defined(WITH_RAWBYTESTRING) and not defined(UNICODE)}, ZOSCodePage{$IFEND});
  end;
  {$IFDEF UNICODE}
  Result.VUnicodeString := Tmp;
  {$ELSE}
  Result.VRawByteString := Tmp;
  {$ENDIF}
end;

procedure TZSoftVariantManager.ProcessUnicodeString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: UnicodeString;
begin
  Result.VType := vtUnicodeString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: ResTmp := UnicodeString(Value.VRawByteString);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: ResTmp := ZRawToUnicode(Value.VRawByteString, ZOSCodePage);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: ResTmp := ZRawToUnicode(Value.VRawByteString, zCP_UTF8);
    {$ENDIF}
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := Value.VUnicodeString;
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then SetString(ResTmp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
      else ResTmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
    else ConvertFixedTypesToUnicode(Value, ResTmp);
  end;
  Result.VUnicodeString := ResTmp;
end;

{$IFNDEF NO_UTF8STRING}
procedure TZSoftVariantManager.ProcessUTF8String(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtUTF8String;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if ZOSCodePage= zCP_UTF8
              then ResTmp := Value.VRawByteString
              else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, zCP_UTF8);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: if ZOSCodePage= zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, zCP_UTF8);
    {$ENDIF}
    vtUTF8String: ResTmp := Value.VRawByteString;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := ZUnicodeToRaw(Value.VUnicodeString, zCP_UTF8);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
      then ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, zCP_UTF8)
      else if ZCompatibleCodePages(zCP_UTF8, Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
      else begin
        Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
        ResTmp := ZUnicodeToRaw(Result.VUnicodeString, zCP_UTF8);
        Result.VUnicodeString := '';
      end
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_UTF8STRING}

procedure TZSoftVariantManager.ConvertFixedTypesToRaw(const Value: TZVariant;
  var Result: RawByteString{$IFDEF WITH_RAWBYTESTRING}; CP: Word{$ENDIF});
var P: PAnsiChar;
  L: LengthInt;
  Buff: array[0..FmtBCD.MaxFMTBcdFractionSize+2] of AnsiChar;
begin
  case Value.VType of
    vtNull:     begin
                  P := nil;
                  L := 0;
                end;
    vtBoolean:  begin
                  P := Pointer(BoolStrsUpRaw[Value.VBoolean]);
                  L := 4+Ord(not Value.VBoolean)
                end;
    vtInteger:  begin
                  IntToRaw(Value.VInteger, @Buff[0], @P);
                  L := P - PAnsiChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtUInteger: begin
                  IntToRaw(Value.VUInteger, @Buff[0], @P);
                  L := P - PAnsiChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtDouble:   begin
                  P := @Buff[0];
                  L := ZSysUtils.FloatToSqlRaw(Value.VDouble, P);
                end;
    vtCurrency: begin
                  ZFastCode.CurrToRaw(Value.VDouble, @Buff[0], @P);
                  L := P - PAnsiChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtBigDecimal:begin
                  P := @Buff[0];
                  L := ZSysUtils.BcdToRaw(Value.VBigDecimal, P, '.');
                end;
    vtGUID:     begin
                  P := @Buff[0];
                  GUIDToBuffer(@Value.VGUID.D1, P, [guidWithBrackets]);
                  L := 38;
                end;
    vtTime:     begin
                  P := @Buff[0];
                  L := TimeToRaw(Value.VTime.Hour, Value.VTime.Minute,
                    Value.VTime.Second, Value.VTime.Fractions, P,
                    FFormatSettings.TimeFormat, False, Value.VDate.IsNegative)
                end;
    vtDate:     begin
                  P := @Buff[0];
                  L := DateToRaw(Value.VDate.Year, Value.VDate.Month, Value.VDate.Day,
                    P, FFormatSettings.DateFormat, False, Value.VDate.IsNegative);
                end;
    vtTimeStamp:begin
                  P := @Buff[0];
                  L := DateTimeToRaw(Value.VTimeStamp.Year, Value.VTimeStamp.Month,
                    Value.VTimeStamp.Day, Value.VTimeStamp.Hour, Value.VTimeStamp.Minute,
                    Value.VTimeStamp.Second, Value.VTimeStamp.Fractions, P,
                    FFormatSettings.DateTimeFormat, False, Value.VTimeStamp.IsNegative);
                end;
    vtDateTime: begin
                  P := @Buff[0];
                  L := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, P, FFormatSettings, False);
                end;
    else        begin
                  P := nil;
                  L := 0;
                  RaiseTypeMismatchError;
                end;
  end;
  ZSetString(P, L, Result{$IFDEF WITH_RAWBYTESTRING},CP{$ENDIF});
end;

procedure TZSoftVariantManager.ConvertFixedTypesToUnicode(const Value: TZVariant;
  var Result: UnicodeString);
var P: PWideChar;
  L: LengthInt;
  Buff: array[0..FmtBCD.MaxFMTBcdFractionSize+2] of AnsiChar;
begin
  case Value.VType of
    vtNull:     begin
                  P := nil;
                  L := 0;
                end;
    vtBoolean:  begin
                  P := Pointer(BoolStrsUpW[Value.VBoolean]);
                  L := 4+Ord(not Value.VBoolean)
                end;
    vtInteger:  begin
                  IntToUnicode(Value.VInteger, @Buff[0], @P);
                  L := P - PWideChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtUInteger: begin
                  IntToUnicode(Value.VUInteger, @Buff[0], @P);
                  L := P - PWideChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtDouble:   begin
                  P := @Buff[0];
                  L := ZSysUtils.FloatToSqlUnicode(Value.VDouble, P);
                end;
    vtCurrency: begin
                  ZFastCode.CurrToUnicode(Value.VDouble, @Buff[0], @P);
                  L := P - PWideChar(@Buff[0]);
                  P := @Buff[0];
                end;
    vtBigDecimal:begin
                  P := @Buff[0];
                  L := ZSysUtils.BcdToUni(Value.VBigDecimal, P, '.');
                end;
    vtGUID:     begin
                  P := @Buff[0];
                  GUIDToBuffer(@Value.VGUID.D1, P, [guidWithBrackets]);
                  L := 38;
                end;
    vtTime:     begin
                  P := @Buff[0];
                  L := TimeToUni(Value.VTime.Hour, Value.VTime.Minute, Value.VTime.Second,
                    Value.VTime.Fractions, P, FFormatSettings.TimeFormat, False, Value.VTime.IsNegative)
                end;
    vtDate:     begin
                  P := @Buff[0];
                  L := DateToUni(Value.VDate.Year, Value.VDate.Month, Value.VDate.Day,
                    P, FFormatSettings.DateFormat, False, Value.VDate.IsNegative);
                end;
    vtTimeStamp:begin
                  P := @Buff[0];
                  L := ZSysUtils.DateTimeToUni(Value.VTimeStamp.Year, Value.VTimeStamp.Month,
                    Value.VTimeStamp.Day, Value.VTimeStamp.Hour, Value.VTimeStamp.Minute,
                    Value.VTimeStamp.Second, Value.VTimeStamp.Fractions, P,
                    FFormatSettings.DateTimeFormat, False, Value.VTimeStamp.IsNegative);
                end;
    vtDateTime: begin
                  P := @Buff[0];
                  L := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(Value.VDateTime, P, FFormatSettings, False);
                end;
    else        begin
                  P := nil;
                  L := 0;
                  RaiseTypeMismatchError;
                end;
  end;
  System.SetString(Result, P, L);
end;

{ TZSoftVariantManager }

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZSoftVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;

  procedure ProcessBytes(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtBytes;
    case Value.VType of
      vtNull: Result.VRawByteString := EmptyRaw;
      vtInteger, vtUInteger, vtDouble: ZSetString(PAnsiChar(@Value.VInteger), 8, Result.VRawByteString);
      vtBytes{$IFNDEF UNICODE}, vtString{$ENDIF},
      {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
      {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
      vtRawByteString:
        Result.VRawByteString := Value.VRawByteString;
      {$IFDEF UNICODE}vtString,{$ENDIF}
      vtUnicodeString: ZSetString(PAnsiChar(Pointer(Value.VUnicodeString)), Length(Value.VUnicodeString) shl 1, Result.VRawByteString);
      vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
          then ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len shl 1, Result.VRawByteString)
          else ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, Result.VRawByteString);
      else
        RaiseTypeMismatchError;
    end;
  end;

begin
  InitializeVariant(Result, NewType);
  case NewType of
    vtBoolean:    Result.VBoolean := GetAsBoolean(Value);
    vtInteger:    Result.VInteger := GetAsInteger(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger:   Result.VUInteger := GetAsUInteger(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtDouble:     Result.VDouble := GetAsDouble(Value);
    vtCurrency:   Result.VCurrency := GetAsCurrency(Value);
    vtBigDecimal: GetAsBigDecimal(Value, Result.VBigDecimal);
    vtGUID:       GetAsGUID(Value, Result.VGUID);
    vtDate:       GetAsDate(Value, Result.VDate);
    vtTime:       GetAsTime(Value, Result.VTime);
    vtTimeStamp:  GetAsTimeStamp(Value, Result.VTimeStamp);
    vtBytes:      ProcessBytes(Value, Result);
    vtDateTime:   Result.VDateTime := GetAsDateTime(Value);
    vtPointer:    Result.VPointer := GetAsPointer(Value);
    vtInterface:  Result.VInterface := GetAsInterface(Value);
    vtString:     ProcessString(Value, Result);
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: ProcessAnsiString(Value, Result);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: ProcessUTF8String(Value, Result);
    {$ENDIF}
    vtRawByteString: ProcessRawByteString(Value, Result);
    vtUnicodeString: ProcessUnicodeString(Value, Result);
    vtCharRec:       ProcessCharRec(Value, Result);
  end;
end;

{ TZClientVariantManager }

{**
  Constructs this object and assigns the main properties.
  @param ClientCodePage the current ClientCodePage.
}
constructor TZClientVariantManager.Create(const ConSettings: PZConSettings{;
  FormatSettings: TZFormatSettings});
begin
  inherited Create; //Set all standard converters functions
  FConSettings := ConSettings;
  FFormatSettings := FConSettings.ReadFormatSettings;
  FClientCP := Consettings.ClientCodePage.CP;
  FCtrlsCP := Consettings.CTRL_CP;
  FUseWComparsions := (FClientCP <> ZOSCodePage) or
    (FClientCP = zCP_UTF8) or
    Consettings.ClientCodePage.IsStringFieldCPConsistent or
    (Consettings.ClientCodePage.Encoding = ceUTF16);
end;

function TZClientVariantManager.GetAsDateTime(
  const Value: TZVariant): TDateTime;
var P: Pointer;
  L: LengthInt;
label DateTimeFromRaw, DateTimeFromUnicode, Fail;
begin
  case Value.VType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(Value.VRawByteString);
        L := Length(Value.VRawByteString);
DateTimeFromRaw:
        if not ZSysUtils.TryPCharToDateTime(PAnsiChar(P), L, FConSettings^.ReadFormatSettings, Result{%H-}) then
          goto Fail;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      begin
        P := Pointer(Value.VUnicodeString);
        L := Length(Value.VUnicodeString);
DateTimeFromUnicode:
        if not ZSysUtils.TryPCharToDateTime(PWideChar(P), L, FConSettings^.ReadFormatSettings, Result) then
          goto Fail;
      end;
    vtCharRec: begin
        P := Value.VCharRec.P;
        L := Value.VCharRec.Len;
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
        then goto DateTimeFromUnicode
        else goto DateTimeFromRaw;
      end;
    else
Fail: Result := inherited GetAsDateTime(Value);
  end;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZClientVariantManager.ProcessAnsiString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtAnsiString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FConSettings.AutoEncode
              then ResTmp := ZConvertStringToAnsiWithAutoEncode(Value.VRawByteString, FCtrlsCP)
              else ResTmp := Value.VRawByteString;
    {$ENDIF}
    vtAnsiString: ResTmp := Value.VRawByteString;
    vtUTF8String: if ZOSCodePage = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, ZOSCodePage);
    vtRawByteString: if ZOSCodePage = FClientCP
                     then ResTmp := Value.VRawByteString
                     else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, ZOSCodePage);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, ZOSCodePage);
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
      else if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, ZOSCodePage);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_ANSISTRING}

procedure TZClientVariantManager.ProcessCharRec(const Value: TZVariant;
  out Result: TZVariant);
label SetRaw;
begin
  Result.VType := vtCharRec;
  case Value.VType of
    vtNull:
      begin
        Result.VCharRec.Len := 0;
        Result.VCharRec.CP := High(Word);
        Result.VCharRec.P := nil;
      end;
    {$IFNDEF UNICODE}
    vtString: begin
        Result.VCharRec.CP := FCtrlsCP;
        goto SetRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto SetRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: begin
        Result.VCharRec.CP := zCP_UTF8;
        goto SetRaw;
      end;
    {$ENDIF}
    vtRawByteString: begin
        Result.VCharRec.CP := fClientCP;
SetRaw: if Pointer(Result.VRawByteString) = nil then begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
        end else begin
          Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
          Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL conversion to PAnsiChar
        end;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      begin
        Result.VUnicodeString := Value.VUnicodeString;
        Result.VCharRec.CP := zCP_UTF16;
        Result.VCharRec.Len := Length(Result.VUnicodeString);
        if Result.VCharRec.Len = 0 then
          Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
        else
          Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PWideChar
      end;
    vtCharRec:
      Result.VCharRec := Value.VCharRec;
    else if (Ord(FConSettings^.ClientCodePage^.Encoding) < Ord(ceUTF16)) then begin
          Result.VRawByteString := Convert(Value, vtRawByteString).VRawByteString;
          Result.VCharRec.CP := FClientCP;
          if Pointer(Result.VRawByteString) = nil then begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
          end else begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL conversion to PAnsiChar
          end;
        end else begin
          Result.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
          Result.VCharRec.CP := zCP_UTF16;
          Result.VCharRec.Len := Length(Result.VUnicodeString);
          if Result.VCharRec.Len = 0
          then Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
          else Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PAnsiChar
        end;

  end;
end;

procedure TZClientVariantManager.ProcessRawByteString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtRawByteString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FConSettings.AutoEncode
              then ResTmp := ZConvertStringToRawWithAutoEncode(Value.VRawByteString, FCtrlsCP, FClientCP)
              else ResTmp := Value.VRawByteString;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: if FClientCP = ZOSCodePage
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, FClientCP);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: if FClientCP = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, FClientCP);
    {$ENDIF}
    vtRawByteString: ResTmp := Value.VRawByteString;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, zCP_UTF8);
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, FClientCP)
      else if ZCompatibleCodePages(FClientCP, Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, FClientCP);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;

procedure TZClientVariantManager.ProcessString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: {$IFDEF UNICODE}UnicodeString{$ELSE}RawByteString{$ENDIF};
begin
  Result.VType := vtString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: Result.VRawByteString := Value.VRawByteString;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, ZOSCodePage);
                  {$ELSE}
                  if FConSettings^.CTRL_CP = ZOSCodePage
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, FCtrlsCP);
                  {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, zCP_UTF8);
                  {$ELSE}
                  if FCtrlsCP = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, FCtrlsCP);
                  {$ENDIF}
    {$ENDIF}
    vtRawByteString: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, FClientCP);
                  {$ELSE}
                  if (FCtrlsCP = FClientCP) or (not FConSettings.AutoEncode)
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, FCtrlsCP);
                  {$ENDIF}
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:
      {$IFDEF UNICODE}
      ResTmp := Value.VUnicodeString;
      {$ELSE}
      //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
      //this hint means a cast instead of convert. The user should better use WideString constants!
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, FCtrlsCP);
      {$ENDIF}
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        {$IFDEF UNICODE}
        SetString(ResTmp, PChar(Value.VCharRec.P), Value.VCharRec.Len)
        {$ELSE}
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
        {$ENDIF}
      else
        {$IFNDEF UNICODE}
        if ZCompatibleCodePages(FCtrlsCP, Value.VCharRec.CP)
        then ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
        else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, FCtrlsCP);
        {$ELSE}
        ResTmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
        {$ENDIF}
    vtDateTime:
      ResTmp := ZSysUtils.{$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(Value.VDateTime, FFormatSettings, False);
    else {$IFDEF UNICODE}ConvertFixedTypesToUnicode{$ELSE}ConvertFixedTypesToRaw{$ENDIF}(Value, ResTmp{$IF defined(WITH_RAWBYTESTRING) and not defined(UNICODE)}, FCtrlsCP{$IFEND});
  end;
  Result.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF} := ResTmp;
end;

procedure TZClientVariantManager.ProcessUnicodeString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: UnicodeString;
begin
  Result.VType := vtUnicodeString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FConSettings.AutoEncode
              then ResTmp := ZConvertStringToUnicodeWithAutoEncode(Value.VRawByteString, FCtrlsCP)
              else ResTmp := ZRawToUnicode(Value.VRawByteString, FCtrlsCP);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: ResTmp := ZRawToUnicode(Value.VRawByteString, ZOSCodePage);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: ResTmp := ZRawToUnicode(Value.VRawByteString, zCP_UTF8);
    {$ENDIF}
    vtRawByteString: ResTmp := ZRawToUnicode(Value.VRawByteString, FClientCP);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := Value.VUnicodeString;
    vtDateTime:      ResTmp := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    vtCharRec: if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
        then SetString(ResTmp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
        else ResTmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
    else ConvertFixedTypesToUnicode(Value, ResTmp);
  end;
  Result.VUnicodeString := ResTmp;
end;

{$IFNDEF NO_UTF8STRING}
procedure TZClientVariantManager.ProcessUTF8String(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtUTF8String;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FConSettings.AutoEncode
              then ResTmp := ZConvertStringToUTF8WithAutoEncode(Value.VRawByteString, FCtrlsCP)
              else ResTmp := Value.VRawByteString;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: if ZOSCodePage = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, zCP_UTF8);
    {$ENDIF}
    vtUTF8String: ResTmp := Value.VRawByteString;
    vtRawByteString: if ZOSCodePage = FClientCP
                     then ResTmp := Value.VRawByteString
                     else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, ZOSCodePage);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, zCP_UTF8);
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, zCP_UTF8)
      else if ZCompatibleCodePages(zCP_UTF8, Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, zCP_UTF8);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_UTF8STRING}

function TZClientVariantManager.UseWComparsions: Boolean;
begin
  Result := FUseWComparsions;
end;

{ TZAnyValue }

{**
  Constructs this object and assigns the main properties.
  @param Value an any value.
}
constructor TZAnyValue.Create(const Value: TZVariant);
begin
  FValue := Value;
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a boolean value.
}
constructor TZAnyValue.CreateWithBoolean(Value: Boolean);
begin
  FValue := EncodeBoolean(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a date value.
}
constructor TZAnyValue.CreateWithDate(const Value: TZDate);
begin
  FValue := EncodeZDate(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a datetime value.
}
constructor TZAnyValue.CreateWithDateTime(const Value: TDateTime);
begin
  FValue := EncodeDateTime(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a double value.
}
constructor TZAnyValue.CreateWithDouble(const Value: Double);
begin
  FValue := EncodeDouble(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a currency value.
}
constructor TZAnyValue.CreateWithCurrency(const Value: Currency);
begin
  FValue := EncodeCurrency(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a bigdecimal value.
}
constructor TZAnyValue.CreateWithBigDecimal(const Value: TBCD);
begin
  FValue := EncodeBigDecimal(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a signed integer value.
}
constructor TZAnyValue.CreateWithInteger(const Value: Int64);
begin
  FValue := EncodeInteger(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a unsigned integer value.
}
constructor TZAnyValue.CreateWithUInteger(const Value: UInt64);
begin
  FValue := EncodeUInteger(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a string value.
}
constructor TZAnyValue.CreateWithString(const Value: String);
begin
  FValue := EncodeString(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a time value.
}
constructor TZAnyValue.CreateWithTime(const Value: TZTime);
begin
  FValue := EncodeZTime(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a timestamp value.
}
constructor TZAnyValue.CreateWithTimeStamp(const Value: TZTimeStamp);
begin
  FValue := EncodeZTimeStamp(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a unicode string value.
}
{$IFDEF UNICODE}
constructor TZAnyValue.CreateWithUnicodeString(const Value: String; unicodeType : Boolean = true);
{$ELSE}
constructor TZAnyValue.CreateWithUnicodeString(const Value: WideString);
{$ENDIF}
begin
  FValue := EncodeUnicodeString(Value);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAnyValue.Clone: IZInterface;
begin
  Result := TZAnyValue.Create(FValue);
end;

{**
  Compares this and another property.
  @return <code>True</code> is properties are equal.
}
function TZAnyValue.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZAnyValue;
begin
  if Value <> nil then begin
    if Value.QueryInterface(IZAnyValue, Temp) = S_OK then begin
      Result := SoftVarManager.Compare(FValue, Temp.GetValue) = 0;
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Gets a stored any value.
  @return a stored any value.
}
function TZAnyValue.GetValue: TZVariant;
begin
  Result := FValue;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAnyValue.ToString: string;
begin
  Result := GetString;
end;

{**
  Checks is the stored value contains NULL.
  @returns <code>True</code> if NULL is stored.
}
function TZAnyValue.IsNull: Boolean;
begin
  Result := SoftVarManager.IsNull(FValue);
end;

{**
  Gets a stored value converted to double.
  @return a stored value converted to double.
}
function TZAnyValue.GetDouble: Double;
begin
  Result := SoftVarManager.GetAsDouble(FValue);
end;

{**
  Gets a stored value converted to currency.
  @return a stored value converted to currency.
}
function TZAnyValue.GetCurrency: Currency;
begin
  Result := SoftVarManager.GetAsCurrency(FValue);
end;

{**
  Gets a stored value converted to bigdecimal.
  @return a stored value converted to bigdecimal.
}
function TZAnyValue.GetBigDecimal: TBCD;
begin
  SoftVarManager.GetAsBigDecimal(FValue, Result{%H-});
end;

{**
  Gets a stored value converted to integer.
  @return a stored value converted to integer.
}
function TZAnyValue.GetInteger: Int64;
begin
  Result := SoftVarManager.GetAsInteger(FValue);
end;

{**
  Gets a stored value converted to unsigned integer.
  @return a stored value converted to unsigned integer.
}
function TZAnyValue.GetUInteger: UInt64;
begin
  Result := SoftVarManager.GetAsUInteger(FValue);
end;

{**
  Gets a stored value converted to String.
  @return a stored value converted to string.
}
function TZAnyValue.GetString: String;
begin
  Result := SoftVarManager.GetAsString(FValue);
end;

{**
  Gets a stored value converted to time.
  @return a stored value converted to time.
}
{$IFDEF FPC}
  {$PUSH}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
function TZAnyValue.GetTime: TZTime;
begin
  SoftVarManager.GetAsTime(FValue, Result);
end;

{**
  Gets a stored value converted to timestamp.
  @return a stored value converted to timestamp.
}
function TZAnyValue.GetTimeStamp: TZTimeStamp;
begin
  SoftVarManager.GetAsTimeStamp(FValue, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
{$IFNDEF NO_ANSISTRING}
function TZAnyValue.GetAnsiString: AnsiString;
begin
  Result := SoftVarManager.GetAsAnsiString(FValue);
end;
{$ENDIF}

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
{$IFNDEF NO_UTF8STRING}
function TZAnyValue.GetUTF8String: UTF8String;
begin
  Result := SoftVarManager.GetAsUTF8String(FValue);
end;
{$ENDIF}

{**
  Gets a stored value converted to boolean.
  @return a stored value converted to boolean.
}
function TZAnyValue.GetBoolean: Boolean;
begin
  Result := SoftVarManager.GetAsBoolean(FValue);
end;

{**
  Gets a stored value converted to byte array.
  @return a stored value converted to a byte array.
}
function TZAnyValue.GetBytes: TBytes;
begin
  Result := SoftVarManager.GetAsBytes(FValue);
end;

{**
  Gets a stored value converted to unicode string.
  @return a stored value converted to unicode string.
}
function TZAnyValue.GetUnicodeString: ZWideString;
begin
  Result := SoftVarManager.GetAsUnicodeString(FValue);
end;

{**
  Gets a stored value converted to date.
  @return a stored value converted to date.
}
{$IFDEF FPC} // parameters not used intentionally
  {$PUSH}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
function TZAnyValue.GetDate: TZDate;
begin
  SoftVarManager.GetAsDate(FValue, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets a stored value converted to datetime.
  @return a stored value converted to datetime.
}
function TZAnyValue.GetDateTime: TDateTime;
begin
  Result := SoftVarManager.GetAsDateTime(FValue);
end;


procedure InitializeVariant({$IFDEF FPC}Out{$ELSE}var{$ENDIF} Value: TZVariant; vType: TZVariantType);
begin
  //clear me late on !!
  Value.VType := vType;
end;

{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;
begin
  case Value.VType of
    vtBoolean: Result := Value.VBoolean;
    vtBytes: Result := BytesToVar(Value.VRawByteString);
    vtInteger: if (Value.VInteger > -MaxInt) and (Value.VInteger < MaxInt) then
        Result := Integer(Value.VInteger)
      else
{$ifdef fpc}
        Result := Value.VInteger;
{$else}
        Result := ZFastCode.IntToStr(Value.VInteger);
{$endif}
    vtDouble: Result := Value.VDouble;
    vtCurrency: Result := Value.VCurrency;
    vtBigDecimal: VarFMTBcdCreate(Result, Value.VBigDecimal);
    vtString: Result := {$IFDEF UNICODE}Value.VUnicodeString{$ELSE}String(Value.VRawByteString){$ENDIF};
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: Result := AnsiString(Value.VRawByteString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: Result := UTF8String(Value.VRawByteString);
    {$ENDIF}
    vtRawByteString: Result := Value.VRawByteString;
    vtUnicodeString: Result := Value.VUnicodeString;
    vtDateTime: Result := Value.VDateTime;
    vtPointer:
    {$ifdef fpc}
        Result := {%H-}NativeUInt(Value.VPointer);
    {$else}
        Result := NativeUInt(Value.VPointer);
    {$endif}
    vtInterface: Result := Value.VInterface;
  else
    Result := Null;
  end;
end;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;
var
  I, L: Integer;
begin
  L := Length(Value);
  Result := VarArrayCreate([0, L - 1], varVariant);
  for I := 0 to L - 1 do
    Result[I] := EncodeVariant(Value[I]);
end;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;
begin
  if VarIsFMTBcd(Value) then
    Result := EncodeBigDecimal(VarToBcd(Value))
  else case VarType(Value) of
    varSmallint, varInteger, varByte:
      Result := EncodeInteger(Integer(Value));
    varBoolean: Result := EncodeBoolean(Value);
    varString: Result := EncodeString(Value);
   {$IFDEF UNICODE}
   varUString: Result := EncodeUnicodeString(Value);
   {$ENDIF}
    varSingle, varDouble:
      Result := EncodeDouble(Value);
    varCurrency:
      Result := EncodeCurrency(Value);
    varUnknown: Result := EncodeInterface(Value);
    varOleStr:
      Result := EncodeUnicodeString(Value);
    varDate: Result := EncodeDateTime(Value);
    varShortInt, varWord, varLongWord:
      Result := EncodeInteger(Value);
    varInt64{$IFDEF WITH_VARIANT_UINT64},varUInt64{$ENDIF}:
      Result := EncodeInteger(Value);
  else
    Result := EncodeNull;
  end;
end;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;
var
  I, L, H: Integer;
begin
  if VarIsArray(Value) then
  begin
    L := VarArrayLowBound(Value, 1);
    H := VarArrayHighBound(Value, 1);
    SetLength(Result, H - L + 1);
    for I := L to H do
      Result[I - L] := DecodeVariant(Value[I]);
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := DecodeVariant(Value);
  end;
end;

{**
  Creates a null variant.
}
function EncodeNull: TZVariant;
begin
  Result.VType := vtNull;
end;

{**
  Creates a boolean variant.
  @param Value a value to be assigned.
}
function EncodeBoolean(const Value: Boolean): TZVariant;
begin
  Result.VType := vtBoolean;
  Result.VBoolean := Value;
end;

{**
  Creates a bytes array variant.
  @param Value a value to be assigned.
}
function EncodeBytes(const Value: TBytes): TZVariant;
begin
  Result.VType := vtBytes;
  ZSetString(Pointer(Value), Length(Value), Result.VRawByteString);
end;

{**
  Creates a bytes array variant from a GUID.
  @param Value a value to be assigned.
}
function EncodeGUID(const Value: TGUID): TZVariant;
begin
  Result.VType := vtGUID;
  Result.VGUID := Value;
end;

{**
  Creates an integer variant.
  @param Value a value to be assigned.
}
function EncodeInteger(const Value: Int64): TZVariant;
begin
  Result.VType := vtInteger;
  Result.VInteger := Value;
end;

{**
  Creates an unsigned integer variant.
  @param Value a value to be assigned.
}
function EncodeUInteger(const Value: UInt64): TZVariant;
begin
  Result.VType := vtUInteger;
  Result.VUInteger := Value;
end;

{**
  Creates a double variant.
  @param Value a value to be assigned.
}
function EncodeDouble(const Value: Double): TZVariant;
begin
  Result.VType := vtDouble;
  Result.VDouble := Value;
end;

{**
  Creates a currency variant.
  @param Value a value to be assigned.
}
function EncodeCurrency(const Value: Currency): TZVariant;
begin
  Result.VType := vtCurrency;
  Result.VCurrency := Value;
end;

{**
  Creates a bigdecimal variant.
  @param Value a value to be assigned.
}
function EncodeBigDecimal(const Value: TBCD): TZVariant;
begin
  Result.VType := vtBigDecimal;
  Result.VBigDecimal := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
function EncodeString(const Value: String): TZVariant;
begin
  Result.VType := vtString;
  Result.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF} := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
{$IFNDEF NO_ANSISTRING}
function EncodeAnsiString(const Value: AnsiString): TZVariant;
begin
  Result.VType := vtAnsiString;
  Result.VRawByteString := Value;
end;
{$ENDIF}

{**
  Creates a UTF8String variant.
  @param Value a value to be assigned.
}
{$IFNDEF NO_UTF8STRING}
function EncodeUTF8String(const Value: UTF8String): TZVariant;
begin
  Result.VType := vtUTF8String;
  Result.VRawByteString := Value;
end;
{$ENDIF}

{**
  Creates a RawByteString variant.
  @param Value a value to be assigned.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant;
begin
  Result.VType := vtRawByteString;
  Result.VRawByteString := Value;
end;

{**
  Creates a TZCharRec variant.
  @param Value a value to be assigned.
}
function EncodeCharRec(const Value: TZCharRec): TZVariant;
begin
  Result.VType := vtCharRec;
  Result.VCharRec := Value;
end;

{**
  Creates a UnicodeString variant.
  @param Value a value to be assigned.
}
function EncodeUnicodeString(const Value: ZWideString): TZVariant;
begin
  Result.VType := vtUnicodeString;
  Result.VUnicodeString := Value;
end;

{**
  Creates a TDateTime variant.
  @param Value a value to be assigned.
}
function EncodeDateTime(const Value: TDateTime): TZVariant;
begin
  Result.VType := vtDateTime;
  Result.VDateTime := Value;
end;

{**
  Encodes a Time value into a custom variant.
  @param Value a TZTime value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZTime(const Value: TZTime): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result.VType := vtTime;
  Result.VTime := Value;
end;

{**
  Encodes a Date value into a custom variant.
  @param Value a TZDate value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZDate(const Value: TZDate): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result.VType := vtDate;
  Result.VDate := Value;
end;

{**
  Encodes a TimeStamp value into a custom variant.
  @param Value a TZTimeStamp value to be encoded.
  @returns an encoded custom variant.
}
function EncodeZTimeStamp(const Value: TZTimeStamp): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result.VType := vtTimeStamp;
  Result.VTimeStamp := Value;
end;

{**
  Creates a pointer variant.
  @param Value a value to be assigned.
}
function EncodePointer(const Value: Pointer): TZVariant;
begin
  Result.VType := vtPointer;
  Result.VPointer := Value;
end;

{**
  Creates an Interface variant.
  @param Value a value to be assigned.
}
function EncodeInterface(const Value: IZInterface): TZVariant;
begin
  Result.VType := vtInterface;
  Result.VInterface := Value;
end;

{**
  Creates an TZArray variant.
  @param Value a value to be assigned.
}
function EncodeArray(const Value: TZArray): TZVariant;
begin
  Result.VType := vtArray;
  Result.VArray := Value;
end;

{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure RawFiller;
var B: Boolean;
begin
  for B := False to True do
    BoolStrsUpRaw[b] := UnicodeStringToASCII7(BoolStrsUpW[b]);
end;
{$ENDIF}

initialization
{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  RawFiller;
{$ENDIF}
  SoftVarManager := TZSoftVariantManager.Create;
  NullVariant    := EncodeNull;

finalization
  SoftVarManager := nil;
end.
