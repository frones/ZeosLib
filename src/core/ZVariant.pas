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
  {$IFNDEF FPC}
  Windows, //need for inline
  {$ENDIF}
  Classes, SysUtils, Types, ZCompatibility, ZClasses, ZSysUtils;

const
  {** Precision for float values comparison }
  FLOAT_COMPARE_PRECISION = 1.0e-5;
  FLOAT_COMPARE_PRECISION_SINGLE = 1.5e-5;

  {FPC - Compatibility for SQLite (currently) }
  JULIAN_DAY_DISTANCE = 2415018.5; //distance from "julian day 0" (January 1, 4713 BC 12:00AM) to "1899-12-30 00:00AM"}

type
  {** Defines variant types. }
  TZVariantType = (vtNull, vtBoolean, vtInteger, vtFloat, vtBytes,
    vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString, //String Types
    vtDateTime, vtPointer, vtInterface);

  {** Defines a variant structure. }
  TZVariant = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    VType: TZVariantType;
    VString: String;
    VAnsiString: AnsiString;
    VRawByteString: RawByteString;
    VUTF8String: UTF8String;
    VUnicodeString: ZWideString;
    VBytes: TByteDynArray;
    VInterface: IZInterface;
    case TZVariantType of
      vtBoolean: (VBoolean: Boolean);
      vtInteger: (VInteger: Int64);
      vtFloat: (VFloat: Extended);
      VtDateTime: (VDateTime: Double);  // M.A. was TDateTime
      VtPointer: (VPointer: Pointer);
  end;

  PZVariant = ^TZVariant;

  {** Defines an array of variants. }
  TZVariantDynArray = array of TZVariant;

  {** Defines a variant processing exception. }
  EZVariantException = class (Exception);

  {** Defines an interface for variant data. }
  {** Defines a Variant Manager interface. }
  IZVariantManager = interface (IZInterface)
    ['{DAA373D9-1A98-4AA8-B65E-4C23167EE83F}']

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(var Value: TZVariant);

    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
    procedure Assign(const SrcValue: TZVariant; var DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TByteDynArray;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): String;
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;

    procedure SetAsBoolean(var Value: TZVariant; Data: Boolean);
    procedure SetAsBytes(var Value: TZVariant; const Data: TByteDynArray);
    procedure SetAsInteger(var Value: TZVariant; Data: Int64);
    procedure SetAsFloat(var Value: TZVariant; Data: Extended);
    procedure SetAsString(var Value: TZVariant; const Data: String);
    procedure SetAsAnsiString(var Value: TZVariant; const Data: AnsiString);
    procedure SetAsUTF8String(var Value: TZVariant; const Data: UTF8String);
    procedure SetAsRawByteString(var Value: TZVariant; const Data: RawByteString);
    procedure SetAsUnicodeString(var Value: TZVariant; const Data: ZWideString);
    procedure SetAsDateTime(var Value: TZVariant; Data: TDateTime);
    procedure SetAsPointer(var Value: TZVariant; Data: Pointer);
    procedure SetAsInterface(var Value: TZVariant; Data: IZInterface);

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
  TZDefaultVariantManager = class (TInterfacedObject, IZVariantManager)
  private
    ZAnsiToUTF8: TZAnsiToUTF8;
    ZUTF8ToAnsi: TZUTF8ToAnsi;
    ZUTF8ToString: TZUTF8ToString;
    ZStringToUTF8: TZStringToUTF8;
    FSystemCodePage: Word;
  protected
    procedure RaiseTypeMismatchError;
    procedure RaiseUnsupportedOperation;
  public
    constructor Create;
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      virtual;
    procedure Assign(const SrcValue: TZVariant; var DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(var Value: TZVariant);

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TByteDynArray;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): String;
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;

    procedure SetAsBoolean(var Value: TZVariant; Data: Boolean);
    procedure SetAsBytes(var Value: TZVariant; const Data: TByteDynArray);
    procedure SetAsInteger(var Value: TZVariant; Data: Int64);
    procedure SetAsFloat(var Value: TZVariant; Data: Extended);
    procedure SetAsString(var Value: TZVariant; const Data: String);
    procedure SetAsAnsiString(var Value: TZVariant; const Data: AnsiString);
    procedure SetAsUTF8String(var Value: TZVariant; const Data: UTF8String);
    procedure SetAsRawByteString(var Value: TZVariant; const Data: RawByteString);
    procedure SetAsUnicodeString(var Value: TZVariant; const Data: ZWideString);
    procedure SetAsDateTime(var Value: TZVariant; Data: TDateTime);
    procedure SetAsPointer(var Value: TZVariant; Data: Pointer);
    procedure SetAsInterface(var Value: TZVariant; Data: IZInterface);

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

  {** Implements a variant manager with soft convertion rules. }
  TZSoftVariantManager = class (TZDefaultVariantManager)
  public
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      override;
  end;

  IZClientVariantManager = Interface(IZVariantManager)
    ['{73A1A2C7-7C38-4620-B7FE-2426BF839BE5}']
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; overload;
  End;

  {** Implements a variant manager with connection related convertion rules. }
  TZClientVariantManager = class (TZDefaultVariantManager, IZClientVariantManager)
  private
    FConSettings: PZConSettings;
  public
    constructor Create(const ConSettings: PZConSettings);
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      override;
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; overload;
  end;

type

  {** Represents any value interface. }
  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TByteDynArray;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: String;
    function GetAnsiString: AnsiString;
    function GetUTF8String: UTF8String;
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
    constructor CreateWithInteger(Value: Int64);
    constructor CreateWithFloat(Value: Extended);
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
    constructor CreateWithDateTime(Value: TDateTime);

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TByteDynArray;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: String;
    function GetAnsiString: AnsiString;
    function GetUTF8String: UTF8String;
    function GetUnicodeString: ZWideString;
    function GetDateTime: TDateTime;

    function Equals(const Value: IZInterface): Boolean; override;
    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

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
function EncodeNull : TZVariant;
{**
  Encodes a boolean into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBoolean(const Value: Boolean): TZVariant;
{**
  Encodes a Byte array into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBytes(const Value: TByteDynArray): TZVariant;
{**
  Encodes an integer into a custom variant.
  @param Value an intger value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInteger(const Value: Int64): TZVariant;
{**
  Encodes a float into a custom variant.
  @param Value a float value to be encoded.
  @returns an encoded custom variant.
}
function EncodeFloat(const Value: Extended): TZVariant;
{**
  Encodes a String into a custom variant.
  @param Value a String value to be encoded.
  @returns an encoded custom variant.
}
function EncodeString(const Value: String): TZVariant;
{**
  Encodes a AnsiString into a custom variant.
  @param Value a AnsiString value to be encoded.
  @returns an encoded custom variant.
}
function EncodeAnsiString(const Value: AnsiString): TZVariant;
{**
  Encodes a UTF8String into a custom variant.
  @param Value a UTF8String value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUTF8String(const Value: UTF8String): TZVariant;
{**
  Encodes a RawByteString into a custom variant.
  @param Value a RawByteString value to be encoded.
  @param CP the CoodePage of the Value string.
  @returns an encoded custom variant.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant;
{**
  Encodes a unicodestring into a custom variant.
  @param Value a unicodestring value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUnicodeString(const Value: ZWideString): TZVariant;
{**
  Encodes a TDateTime into a custom variant.
  @param Value a TDateTime value to be encoded.
  @returns an encoded custom variant.
}
function EncodeDateTime(const Value: TDateTime): TZVariant;
{**
  Encodes a pointer into a custom variant.
  @param Value a pointer value to be encoded.
  @returns an encoded custom variant.
}
function EncodePointer(const Value: Pointer): TZVariant;
{**
  Encodes an interface into a custom variant.
  @param Value an interface value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInterface(const Value: IZInterface): TZVariant;

var
  {** Declares a default variant manager with strict convertion rules. }
  DefVarManager: IZVariantManager;

  {** Declares a variant manager with soft convertion rules. }
  SoftVarManager: IZVariantManager;

  {** A NULL Variant Value. }
  NullVariant: TZVariant;

implementation

uses
  Variants, Math, ZMessages, ZEncoding
  {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}, AnsiStrings{$ENDIF};

{ TZDefaultVariantManager }

{**
  Constructs this object and assignes the main properties.
}
constructor TZDefaultVariantManager.Create;
begin
  inherited;
  FSystemCodePage := ZDefaultSystemCodePage;
  if ZCompatibleCodePages(zCP_UTF8, FSystemCodePage) then
  begin
    ZAnsiToUTF8 := @ZMoveAnsiToUTF8;
    ZUTF8ToAnsi := @ZMoveUTF8ToAnsi;
    ZUTF8ToString := @ZMoveUTF8ToString;
    ZStringToUTF8 := @ZMoveStringToUTF8;
  end
  else
  begin
    ZAnsiToUTF8 := @ZConvertAnsiToUTF8;
    ZUTF8ToAnsi := @ZConvertUTF8ToAnsi;
    ZUTF8ToString := @ZConvertUTF8ToString;
    ZStringToUTF8 := @ZConvertStringToUTF8;
  end;
end;

{**
  Assignes one variant value to another one.
  @param SrcValue a source variant value.
  @param DstValue a destination variant value.
}
procedure TZDefaultVariantManager.Assign(const SrcValue: TZVariant;
  var DstValue: TZVariant);
begin
  DstValue.VType := SrcValue.VType;
  case SrcValue.VType of
    vtBoolean: DstValue.VBoolean := SrcValue.VBoolean;
    vtBytes: DstValue.VBytes := SrcValue.VBytes;
    vtInteger: DstValue.VInteger := SrcValue.VInteger;
    vtFloat: DstValue.VFloat := SrcValue.VFloat;
    vtString: DstValue.VString := SrcValue.VString;
    vtAnsiString: DstValue.VAnsiString := SrcValue.VAnsiString;
    vtRawByteString: DstValue.VRawByteString := SrcValue.VRawByteString;
    vtUTF8String: DstValue.VUTF8String := SrcValue.VUTF8String;
    vtUnicodeString: DstValue.VUnicodeString := SrcValue.VUnicodeString;
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
function TZDefaultVariantManager.Clone(const Value: TZVariant): TZVariant;
begin
  Assign(Value, Result);
end;

{**
  Raises a type mismatch exception.
}
procedure TZDefaultVariantManager.RaiseTypeMismatchError;
begin
  raise EZVariantException.Create(STypesMismatch);
end;

{**
  Raises an unsupported operation exception.
}
procedure TZDefaultVariantManager.RaiseUnsupportedOperation;
begin
  raise EZVariantException.Create(SUnsupportedOperation);
end;

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZDefaultVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        else
          RaiseTypeMismatchError;
      end;
    vtBytes:
      case Value.VType of
        vtNull:
          Result.VBytes := nil;
        vtBytes:
          Result.VBytes := Value.VBytes;
        vtString:
          Result.VBytes := StrToBytes(Value.VString);
        vtAnsiString:
          Result.VBytes := StrToBytes(Value.VAnsiString);
        vtRawByteString:
          Result.VBytes := StrToBytes(Value.VRawByteString);
        vtUTF8String:
          Result.VBytes := StrToBytes(Value.VUTF8String);
        vtUnicodeString:
          Result.VBytes := StrToBytes(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else
            Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        else
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else
            Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        else
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtBytes:
          Result.VString := {$IFDEF UNICODE}String{$ENDIF}(BytesToStr(Value.VBytes));
        vtString:
          Result.VString := Value.VString;
        vtAnsiString:
          Result.VString := {$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString);
        vtUTF8String:
          Result.VString := ZUTF8ToString(Value.VUTF8String, FSystemCodePage);
        vtUnicodeString:
          Result.VString := {$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtAnsiString:
      case Value.VType of
        vtNull:
          Result.VAnsiString := '';
        vtBytes:
          Result.VAnsiString := BytesToStr(Value.VBytes);
        vtString:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(Value.VString);
        vtAnsiString:
          Result.VAnsiString := Value.VAnsiString;
        vtUTF8String:
          Result.VAnsiString := ZUTF8ToAnsi(Value.VUTF8String);
        vtUnicodeString:
          Result.VAnsiString := AnsiString(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtUTF8String:
      case Value.VType of
        vtNull:
          Result.VUTF8String := '';
        vtBytes:
          ZSetString(PAnsiChar(Value.VBytes), Length(Value.VBytes), Result.VUTF8String);
        vtString:
          Result.VUTF8String := ZStringToUTF8(Value.VString, FSystemCodePage);
       vtAnsiString:
          Result.VUTF8String := ZAnsiToUTF8(Value.VAnsiString);
        vtUTF8String:
          Result.VUTF8String := Value.VUTF8String;
        vtUnicodeString:
          {$IFDEF WITH_RAWBYTESTRING}
          Result.VUTF8String := UTF8String(Value.VUnicodeString);
          {$ELSE}
          Result.VUTF8String := UTF8Encode(Value.VUnicodeString);
          {$ENDIF}
        else
          RaiseTypeMismatchError;
      end;
    vtRawByteString:
      case Value.VType of
        vtNull:
          Result.VRawByteString := '';
        vtBytes:
          ZSetString(PAnsiChar(Value.VBytes), Length(Value.VBytes), Result.VRawByteString);
        vtRawByteString:
          Result.VRawByteString := Value.VRawByteString;
        else
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtString:
          Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(Value.VString); //Cast ansi to Wide/Unicode
        vtAnsiString:
          Result.VUnicodeString := ZWideString(Value.VAnsiString); //Cast ansi to Wide/Unicode
        vtUTF8String:
          Result.VUnicodeString :=
            {$IFDEF WITH_RAWBYTESTRING}
            ZWideString(Value.VUTF8String);
            {$ELSE}
            UTF8ToString(PAnsiChar(Value.VUTF8String));
            {$ENDIF}
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtPointer:
          Result.VPointer := Value.VPointer;
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else
          RaiseTypeMismatchError;
      end;
  end;
end;

{**
  Compares two variant values.
  @param Value1 the first variant value.
  @param Value2 the second variant value.
  @return <0 if Value1 < Value 2, =0 if Value1 = Value2, >0 if Value1 > Value2
}
function TZDefaultVariantManager.Compare(const Value1,
  Value2: TZVariant): Integer;
var
  TempFloat: Extended;
  TempDateTime: TDateTime;
begin
  case Value1.VType of
    vtNull:
      begin
        if IsNull(Value2) then
          Result := 0
        else
          Result := -1;
      end;
    vtBoolean:
      begin
        if GetAsBoolean(Value2) then
        begin
          if Value1.VBoolean then
            Result := 0
          else
            Result := -1;
        end
        else
        begin
          if Value1.VBoolean then
            Result := 1
          else
            Result := 0;
        end;
      end;
    vtInteger:
      Result := Value1.VInteger - GetAsInteger(Value2);
    vtFloat:
      begin
        TempFloat := GetAsFloat(Value2);
        if Value1.VFloat - TempFloat < -FLOAT_COMPARE_PRECISION then
          Result := -1
        else if Value1.VFloat - TempFloat > FLOAT_COMPARE_PRECISION then
          Result := 1
        else
          Result := 0;
      end;
{ TODO -oEgonHugeist -cOptimierung :
  String typed needs to be reviewed for a more optimal way.
  Simple ByteCompare instead of functions which are codepage dependent should be faster, thought. }
    vtString:
      Result := AnsiStrComp(PChar(Value1.VString), PChar(GetAsString(Value2)));
    vtAnsiString:
      Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrComp(PAnsiChar(Value1.VAnsiString), PAnsiChar(GetAsAnsiString(Value2)));
    vtUTF8String:
      Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrComp(PAnsiChar(GetAsAnsiString(Value1)), PAnsiChar(GetAsAnsiString(Value2)));
    vtRawByteString:
      Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrComp(PAnsiChar(GetAsAnsiString(Value1)), PAnsiChar(GetAsAnsiString(Value2)));
    vtUnicodeString:
{$IFNDEF FPC}
   {$IFDEF UNICODE}
      Result := AnsiCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
   {$ELSE}
      Result := WideCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
   {$ENDIF}
{$ELSE}
      Result := AnsiCompareStr(AnsiString(Value1.VUnicodeString), GetAsString(Value2));
{$ENDIF}
    vtDateTime:
      begin
        TempDateTime := GetAsDateTime(Value2);
        if Value1.VDateTime < TempDateTime then
          Result := -1
        else if Value1.VDateTime > TempDateTime then
          Result := 1
        else
          Result := 0;
      end;
    vtPointer:
      Result := sign(NativeInt(Value1.VPointer) - GetAsInteger(Value2));
    else
      Result := 0;
  end;
end;

{**
  Checks is the specified value NULL.
  @param Value a value to be checked.
  @returns <code>True</code> if variant has NULL value.
}
function TZDefaultVariantManager.IsNull(const Value: TZVariant): Boolean;
begin
  Result := Value.VType = vtNull;
end;

{**
  Sets the NULL value to specified variant.
  @param Value variant value to be set to NULL.
}
procedure TZDefaultVariantManager.SetNull(var Value: TZVariant);
begin
  Value := EncodeNull;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsBoolean(
  const Value: TZVariant): Boolean;
begin
  Result := Convert(Value, vtBoolean).VBoolean;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsBytes(
  const Value: TZVariant): TByteDynArray;
begin
  Result := Convert(Value, vtBytes).VBytes;
end;
{**
  Gets a variant to integer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsInteger(
  const Value: TZVariant): Int64;
begin
  Result := Convert(Value, vtInteger).VInteger;
end;

{**
  Gets a variant to float value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsFloat(
  const Value: TZVariant): Extended;
begin
  Result := Convert(Value, vtFloat).VFloat;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsString(
  const Value: TZVariant): String;
begin
  Result := Convert(Value, vtString).VString;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsAnsiString(
  const Value: TZVariant): AnsiString;
begin
  Result := Convert(Value, vtAnsiString).VAnsiString;
end;

function TZDefaultVariantManager.GetAsUTF8String(const Value: TZVariant): UTF8String;
begin
  Result := Convert(Value, vtUTF8String).VUTF8String;
end;

function TZDefaultVariantManager.GetAsRawByteString(const Value: TZVariant): RawByteString;
begin
  Result := Convert(Value, vtRawByteString).VRawByteString;
end;

{**
  Gets a variant to unicode string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsUnicodeString(
  const Value: TZVariant): ZWideString;
begin
  Result := Convert(Value, vtUnicodeString).VUnicodeString;
end;

{**
  Gets a variant to date and time value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsDateTime(
  const Value: TZVariant): TDateTime;
begin
  Result := Convert(Value, vtDateTime).VDateTime;
end;

{**
  Gets a variant to pointer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsPointer(
  const Value: TZVariant): Pointer;
begin
  Result := Convert(Value, vtPointer).VPointer;
end;

{**
  Gets a variant to interface value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsInterface(
  const Value: TZVariant): IZInterface;
begin
  Result := Convert(Value, vtInterface).VInterface;
end;

{**
  Assignes a boolean value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsBoolean(var Value: TZVariant;
  Data: Boolean);
begin
  Value := EncodeBoolean(Data);
end;

{**
  Assignes a Byte array value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsBytes(var Value: TZVariant;
  const Data: TByteDynArray);
begin
  Value := EncodeBytes(Data);
end;

{**
  Assignes an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsInteger(var Value: TZVariant;
  Data: Int64);
begin
  Value := EncodeInteger(Data);
end;

{**
  Assignes a float value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsFloat(var Value: TZVariant;
  Data: Extended);
begin
  Value := EncodeFloat(Data);
end;

{**
  Assignes a String value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsString(var Value: TZVariant;
  const Data: String);
begin
  Value := EncodeString(Data);
end;

{**
  Assignes a AnsiString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsAnsiString(var Value: TZVariant;
  const Data: AnsiString);
begin
  Value := EncodeAnsiString(Data);
end;

{**
  Assignes a UTF8string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsUTF8String(var Value: TZVariant;
  const Data: UTF8String);
begin
  Value := EncodeUTF8String(Data);
end;

{**
  Assignes a RawByteString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
  @param CP the CodePage of the Data string
}
procedure TZDefaultVariantManager.SetAsRawByteString(var Value: TZVariant;
  const Data: RawByteString);
begin
  Value := EncodeRawByteString(Data);
end;

{**
  Assignes a unicode string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsUnicodeString(var Value: TZVariant;
  const Data: ZWideString);
begin
  Value := EncodeUnicodeString(Data);
end;

{**
  Assignes a datetime value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsDateTime(var Value: TZVariant;
  Data: TDateTime);
begin
  Value := EncodeDateTime(Data);
end;

{**
  Assignes a pointer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsPointer(var Value: TZVariant;
  Data: Pointer);
begin
  Value := EncodePointer(Data);
end;

{**
  Assignes a interface value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsInterface(var Value: TZVariant;
  Data: IZInterface);
begin
  Value := EncodeInterface(Data);
end;

{**
  Performs '+' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpAdd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger + GetAsInteger(Value2));
    vtFloat: Result := EncodeFloat(Value1.VFloat + GetAsFloat(Value2));
    vtString: Result := EncodeString(Value1.VString + GetAsString(Value2));
    vtAnsiString: Result := EncodeAnsiString(Value1.VAnsiString + GetAsAnsiString(Value2));
    vtUTF8String: Result := EncodeUTF8String(Value1.VUTF8String + GetAsUTF8String(Value2));
    vtRawByteString: Result := EncodeRawByteString(Value1.VRawByteString + GetAsRawByteString(Value2));
    vtUnicodeString: Result := EncodeUnicodeString(Value1.VUnicodeString + GetAsUnicodeString(Value2));
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
function TZDefaultVariantManager.OpAnd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean and GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger and GetAsInteger(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '/' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpDiv(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger div GetAsInteger(Value2));
    vtFloat: Result := EncodeFloat(Value1.VFloat / GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpEqual(const Value1,
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
function TZDefaultVariantManager.OpLess(const Value1,
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
function TZDefaultVariantManager.OpLessEqual(const Value1,
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
function TZDefaultVariantManager.OpMod(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger mod GetAsInteger(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpMore(const Value1,
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
function TZDefaultVariantManager.OpMoreEqual(const Value1,
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
function TZDefaultVariantManager.OpMul(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger * GetAsInteger(Value2));
    vtFloat: Result := EncodeFloat(Value1.VFloat * GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs unary '-' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNegative(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(-Value.VInteger);
    vtFloat: Result := EncodeFloat(-Value.VFloat);
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '~' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNot(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(not Value.VBoolean);
    vtInteger: Result := EncodeInteger(not Value.VInteger);
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '<>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNotEqual(const Value1,
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
function TZDefaultVariantManager.OpOr(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean or GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger or GetAsInteger(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpPow(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeFloat(Power(Value1.VInteger, GetAsInteger(Value2)));
    vtFloat: Result := EncodeFloat(Power(Value1.VFloat, GetAsFloat(Value2)));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '-' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpSub(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger - GetAsInteger(Value2));
    vtFloat: Result := EncodeFloat(Value1.VFloat - GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpXor(const Value1,
  Value2: TZVariant): TZVariant;
var
  TempBool1, TempBool2: Boolean;
  TempInteger1, TempInteger2: Int64;
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
    else RaiseUnsupportedOperation;
  end;
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
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        vtInteger:
          Result.VBoolean := Value.VInteger <> 0;
        vtFloat:
          Result.VBoolean := Value.VFloat <> 0;
        vtString:
          Result.VBoolean := StrToBoolEx(Value.VString);
        vtAnsiString:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString));
        vtUTF8String:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String));
        vtRawByteString:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VRawByteString));
        vtUnicodeString:
          Result.VBoolean := StrToBoolEx({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString));
        vtDateTime:
          Result.VBoolean := Value.VDateTime <> 0;
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtBytes:
      case Value.VType of
        vtNull:
          Result.VBytes := nil;
        vtBytes:
          Result.VBytes := Value.VBytes;
        vtString:
          Result.VBytes := StrToBytes(Value.VString);
        vtAnsiString:
          Result.VBytes := StrToBytes(Value.VAnsiString);
        vtRawByteString:
          Result.VBytes := StrToBytes(Value.VRawByteString);
        vtUTF8String:
          Result.VBytes := StrToBytes(Value.VUTF8String);
        vtUnicodeString:
          Result.VBytes := StrToBytes(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else
            Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        vtFloat:
          Result.VInteger := Trunc(Value.VFloat);
        vtString:
          Result.VInteger := StrToInt64Def(Value.VString, 0);
        vtAnsiString:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString), 0);
        vtUTF8String:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String), 0);
        vtRawByteString:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VRawByteString), 0);
        vtUnicodeString:
          Result.VInteger := StrToInt64Def({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString), 0);
        vtDateTime:
          Result.VInteger := Trunc(Value.VDateTime);
        vtPointer:
          Result.VInteger := NativeInt(Value.VPointer);
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else
            Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        vtString:
          Result.VFloat := SqlStrToFloatDef(Value.VString, 0);
        vtAnsiString:
          Result.VFloat := SqlStrToFloatDef({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString), 0);
        vtUTF8String:
          Result.VFloat := SqlStrToFloatDef({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String), 0);
        vtRawByteString:
          Result.VFloat := SqlStrToFloatDef(Value.VRawByteString, 0);
        vtUnicodeString:
          Result.VFloat := SqlStrToFloatDef(AnsiString(Value.VUnicodeString), 0);
        vtDateTime:
          Result.VFloat := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VString := 'TRUE'
          else
            Result.VString := 'FALSE';
        vtBytes:
          ZSetString(PAnsiChar(Value.VBytes), Length(Value.VBytes), Result.VString);
        vtInteger:
          Result.VString := IntToStr(Value.VInteger);
        vtFloat:
          Result.VString := FloatToSqlStr(Value.VFloat);
        vtString:
          Result.VString := Value.VString;
        vtAnsiString:
          Result.VString := {$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString);
        vtUTF8String:
          Result.VString := ZUTF8ToString(Value.VUTF8String, FSystemCodePage);
        vtUnicodeString:
          Result.VString := Value.VUnicodeString; //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
        vtDateTime:
          Result.VString := DateTimeToAnsiSQLDate(Value.VDateTime);
          // gto: Not a real threat, as it's converting dates (unicode safe)
        else
          RaiseTypeMismatchError;
      end;
    vtAnsiString:
      case Value.VType of
        vtNull:
          Result.VAnsiString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VAnsiString := 'TRUE'
          else
            Result.VAnsiString := 'FALSE';
        vtInteger:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(Value.VInteger));
        vtFloat:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(Value.VString);
        vtAnsiString:
          Result.VAnsiString := Value.VAnsiString;
        vtUTF8String:
          Result.VAnsiString := ZUTF8ToAnsi(Value.VUTF8String);
        vtUnicodeString:
          Result.VAnsiString := AnsiString(Value.VUnicodeString);
        vtDateTime:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtUTF8String:
      case Value.VType of
        vtNull:
          Result.VUTF8String := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VUTF8String := 'TRUE'
          else
            Result.VUTF8String := 'FALSE';
        vtInteger:
          Result.VUTF8String := UTF8String(IntToStr(Value.VInteger));
        vtFloat:
          Result.VUTF8String := UTF8String(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VUTF8String := ZStringToUTF8(Value.VString, FSystemCodePage);
        vtAnsiString:
          Result.VUTF8String := ZAnsiToUTF8(Value.VAnsiString);
        vtUTF8String:
          Result.VUTF8String := Value.VUTF8String;
        vtUnicodeString:
          {$IFDEF WITH_RAWBYTESTRING}
          Result.VUTF8String := UTF8String(Value.VUnicodeString);
          {$ELSE}
          Result.VUTF8String := UTF8Encode(Value.VUnicodeString);
          {$ENDIF}
        vtDateTime:
          Result.VUTF8String := UTF8String(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VUnicodeString := 'TRUE'
          else
            Result.VUnicodeString := 'FALSE';
        vtInteger:
          Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(IntToStr(Value.VInteger));
        vtFloat:
          Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(Value.VString);
        vtAnsiString:
          Result.VUnicodeString := ZWideString(Value.VAnsiString);
        vtUTF8String:
          Result.VUnicodeString := {$IFDEF UNICODE}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(Value.VUTF8String));
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        vtDateTime:
          Result.VUnicodeString := ZWideString(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VDateTime := Value.VInteger;
        vtFloat:
          Result.VDateTime := Value.VFloat;
        vtString:
          Result.VDateTime := AnsiSQLDateToDateTime(Value.VString);
        vtAnsiString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString));
        vtUTF8String:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String));
        vtRawByteString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}String{$ENDIF}(Value.VRawByteString));
        vtUnicodeString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString));
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VPointer := Pointer(Value.VInteger);
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else
      end;
  end;
end;

{ TZClientVariantManager }

{**
  Constructs this object and assignes the main properties.
  @param ClientCodePage the current ClientCodePage.
}
constructor TZClientVariantManager.Create(const ConSettings: PZConSettings);
begin
  inherited Create; //Set all standart converters functions

  FConSettings := ConSettings;
end;

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZClientVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        vtInteger:
          Result.VBoolean := Value.VInteger <> 0;
        vtFloat:
          Result.VBoolean := Value.VFloat <> 0;
        vtString:
          Result.VBoolean := StrToBoolEx(Value.VString);
        vtAnsiString:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString));
        vtUTF8String:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String));
        vtRawByteString:
          Result.VBoolean := StrToBoolEx({$IFDEF UNICODE}String{$ENDIF}(Value.VRawByteString));
        vtUnicodeString:
          Result.VBoolean := StrToBoolEx({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString));
        vtDateTime:
          Result.VBoolean := Value.VDateTime <> 0;
        else
          RaiseTypeMismatchError;
      end;
    vtBytes:
      case Value.VType of
        vtNull:
          Result.VBytes := nil;
        vtBytes:
          Result.VBytes := Value.VBytes;
        vtString:
          Result.VBytes := StrToBytes(Value.VString);
        vtAnsiString:
          Result.VBytes := StrToBytes(Value.VAnsiString);
        vtRawByteString:
          Result.VBytes := StrToBytes(Value.VRawByteString);
        vtUTF8String:
          Result.VBytes := StrToBytes(Value.VUTF8String);
        vtUnicodeString:
          Result.VBytes := StrToBytes(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else
            Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        vtFloat:
          Result.VInteger := Trunc(Value.VFloat);
        vtString:
          Result.VInteger := StrToInt64Def(Value.VString, 0);
        vtAnsiString:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString), 0);
        vtUTF8String:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String), 0);
        vtRawByteString:
          Result.VInteger := StrToInt64Def({$IFDEF UNICODE}String{$ENDIF}(Value.VRawByteString), 0);
        vtUnicodeString:
          Result.VInteger := StrToInt64Def({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString), 0);
        vtDateTime:
          Result.VInteger := Trunc(Value.VDateTime);
        vtPointer:
          Result.VInteger := NativeInt(Value.VPointer);
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else
            Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        vtString:
          Result.VFloat := SqlStrToFloatDef(Value.VString, 0);
        vtAnsiString:
          Result.VFloat := SqlStrToFloatDef({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString), 0);
        vtUTF8String:
          Result.VFloat := SqlStrToFloatDef({$IFDEF UNICODE}String{$ENDIF}(Value.VUTF8String), 0);
        vtRawByteString:
          Result.VFloat := SqlStrToFloatDef(Value.VRawByteString, 0);
        vtUnicodeString:
          Result.VFloat := SqlStrToFloatDef({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString), 0);
        vtDateTime:
          Result.VFloat := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VString := 'TRUE'
          else
            Result.VString := 'FALSE';
        vtBytes:
          Result.VString := {$IFDEF UNICODE}String{$ENDIF}(BytesToStr(Value.VBytes));
        vtInteger:
          Result.VString := IntToStr(Value.VInteger);
        vtFloat:
          Result.VString := FloatToSqlStr(Value.VFloat);
        vtString:
          Result.VString := Value.VString;
        vtAnsiString:
          Result.VString := FConSettings^.ConvFuncs.ZAnsiToString(Value.VAnsiString, FConSettings^.CTRL_CP);
        vtUTF8String:
          Result.VString := FConSettings^.ConvFuncs.ZUTF8ToString(Value.VUTF8String, FConSettings^.CTRL_CP);
        vtRawByteString:
          Result.VString := FConSettings^.ConvFuncs.ZRawToString(Value.VRawByteString, FConSettings^.ClientCodePage^.CP, FConSettings^.CTRL_CP);
        vtUnicodeString:
          //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
          //this hint means a cast instead of convert. The user should better use WideString constants!
          Result.VString := FConSettings^.ConvFuncs.ZUnicodeToString(Value.VUnicodeString, FConSettings^.CTRL_CP);
        vtDateTime:
          Result.VString := DateTimeToAnsiSQLDate(Value.VDateTime);
        else
          RaiseTypeMismatchError;
      end;
    vtAnsiString:
      case Value.VType of
        vtNull:
          Result.VAnsiString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VAnsiString := 'TRUE'
          else
            Result.VAnsiString := 'FALSE';
        vtInteger:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(Value.VInteger));
        vtFloat:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VAnsiString := FConSettings^.ConvFuncs.ZStringToAnsi(Value.VString, FConSettings^.CTRL_CP);
        vtAnsiString:
          Result.VAnsiString := Value.VAnsiString;
        vtUTF8String:
          Result.VAnsiString := FConSettings^.ConvFuncs.ZUTF8ToAnsi(Value.VUTF8String);
        vtRawByteString:
          Result.VAnsiString := FConSettings^.ConvFuncs.ZRawToAnsi(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
        vtUnicodeString:
          Result.VAnsiString := AnsiString(Value.VUnicodeString);
        vtDateTime:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtUTF8String:
      case Value.VType of
        vtNull:
          Result.VUTF8String := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VUTF8String := 'TRUE'
          else
            Result.VUTF8String := 'FALSE';
        vtInteger:
          Result.VUTF8String := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ENDIF}(IntToStr(Value.VInteger));
        vtFloat:
          Result.VUTF8String := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ENDIF}(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VUTF8String := FConSettings^.ConvFuncs.ZStringToUTF8(Value.VString, FConSettings^.CTRL_CP);
        vtAnsiString:
          Result.VUTF8String := FConSettings^.ConvFuncs.ZAnsiToUTF8(Value.VAnsiString);
        vtUTF8String:
          Result.VUTF8String := Value.VUTF8String;
        vtRawByteString:
          Result.VUTF8String := FConSettings^.ConvFuncs.ZRawToUTF8(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
        vtUnicodeString:
          {$IFDEF WITH_RAWBYTESTRING}
          Result.VUTF8String := UTF8String(Value.VUnicodeString);
          {$ELSE}
          Result.VUTF8String := UTF8Encode(Value.VUnicodeString);
          {$ENDIF}
        vtDateTime:
          Result.VUTF8String := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtRawByteString:
      case Value.VType of
        vtNull:
          Result.VRawByteString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VRawByteString := 'TRUE'
          else
            Result.VRawByteString := 'FALSE';
        vtInteger:
          Result.VRawByteString := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(IntToStr(Value.VInteger));
        vtFloat:
          Result.VRawByteString := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VRawByteString := FConSettings^.ConvFuncs.ZStringToRaw(Value.VString, FConSettings^.CTRL_CP, FConSettings^.ClientCodePage^.CP);
        vtAnsiString:
          Result.VRawByteString := FConSettings^.ConvFuncs.ZAnsiToRaw(Value.VAnsiString, FConSettings^.ClientCodePage^.CP);
        vtUTF8String:
          Result.VRawByteString := FConSettings^.ConvFuncs.ZUTF8ToRaw(Value.VUTF8String, FConSettings^.ClientCodePage^.CP);
        vtRawByteString:
          Result.VRawByteString := Value.VRawByteString;
        vtUnicodeString:
          Result.VRawByteString := FConSettings^.ConvFuncs.ZUnicodeToRaw(Value.VUnicodeString, FConSettings^.ClientCodePage^.CP);
        vtDateTime:
          Result.VRawByteString := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VUnicodeString := 'TRUE'
          else
            Result.VUnicodeString := 'FALSE';
        vtInteger:
          Result.VUnicodeString := ZWideString(IntToStr(Value.VInteger));
        vtFloat:
          Result.VUnicodeString := ZWideString(FloatToSqlStr(Value.VFloat));
        vtString:
          Result.VUnicodeString := FConSettings^.ConvFuncs.ZStringToUnicode(Value.VString, FConSettings^.CTRL_CP);
        vtAnsiString:
          Result.VUnicodeString := ZWideString(Value.VAnsiString);
        vtUTF8String:
          Result.VUnicodeString := {$IFDEF UNICODE}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(Value.VUTF8String));
        vtRawByteString:
          Result.VUnicodeString := FConSettings^.ConvFuncs.ZRawToUnicode(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        vtDateTime:
          Result.VUnicodeString := ZWideString(DateTimeToAnsiSQLDate(Value.VDateTime));
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtInteger:
          Result.VDateTime := Value.VInteger;
        vtFloat:
          Result.VDateTime := Value.VFloat;
        vtString:
          Result.VDateTime := AnsiSQLDateToDateTime(Value.VString);
        vtAnsiString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString));
        vtUTF8String:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF WITH_RAWBYTESTRING}String{$ENDIF}(Value.VUTF8String));
        vtRawByteString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF WITH_RAWBYTESTRING}String{$ENDIF}(Value.VRawByteString));
        vtUnicodeString:
          Result.VDateTime := AnsiSQLDateToDateTime({$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString));
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VPointer := Pointer(Value.VInteger);
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else RaiseTypeMismatchError;
      end;
  end;
end;

{$WARNINGS OFF} //suppress [Pascal Warning] ZVariant.pas(1926): W1035 Return value of function 'TZClientVariantManager.GetAsRawByteString' might be undefined
function TZClientVariantManager.GetAsRawByteString(const Value: TZVariant;
  const RawCP: Word): RawByteString;
var US: ZWideString;
begin
  case Value.VType of
    vtNull:
      Result := '';
    vtBoolean:
      if Value.VBoolean then
        Result := 'TRUE'
      else
        Result := 'FALSE';
    vtBytes:
      ZSetString(PAnsiChar(Value.VBytes), Length(Value.VBytes), Result);
    vtInteger:
      Result := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(IntToStr(Value.VInteger));
    vtFloat:
      Result := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(FloatToSqlStr(Value.VFloat));
    vtString:
      Result := ZConvertStringToRawWithAutoEncode(Value.VString, FConSettings^.CTRL_CP, RawCP);
    vtAnsiString:
      if ZCompatibleCodePages(ZDefaultSystemCodePage, RawCP) then
        Result := ZMoveAnsiToRaw(Value.VAnsiString, RawCP)
      else
        Result := ZConvertAnsiToRaw(Value.VAnsiString, RawCP);
    vtUTF8String:
      if ZCompatibleCodePages(zCP_UTF8, RawCP) then
        Result := ZMoveUTF8ToRaw(Value.VUTF8String, RawCP)
      else
        Result := ZConvertUTF8ToRaw(Value.VUTF8String, RawCP);
    vtRawByteString:
      if ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, RawCP) then
        Result := Value.VRawByteString
      else
      begin
        US := ZRawToUnicode(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
        Result := ZUnicodeToRaw(US, RawCP);
      end;
    vtUnicodeString:
      Result := ZUnicodeToRaw(Value.VUnicodeString, RawCP);
    vtDateTime:
      Result := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
    else
      RaiseTypeMismatchError;
  end;
end;
{$WARNINGS ON}

{ TZAnyValue }

{**
  Constructs this object and assignes the main properties.
  @param Value an any value.
}
constructor TZAnyValue.Create(const Value: TZVariant);
begin
  FValue := Value;
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a boolean value.
}
constructor TZAnyValue.CreateWithBoolean(Value: Boolean);
begin
  FValue := EncodeBoolean(Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a datetime value.
}
constructor TZAnyValue.CreateWithDateTime(Value: TDateTime);
begin
  FValue := EncodeDateTime(Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a float value.
}
constructor TZAnyValue.CreateWithFloat(Value: Extended);
begin
  FValue := EncodeFloat(Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a integer value.
}
constructor TZAnyValue.CreateWithInteger(Value: Int64);
begin
  FValue := EncodeInteger(Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a string value.
}
constructor TZAnyValue.CreateWithString(const Value: String);
begin
  FValue := EncodeString(Value);
end;

{**
  Constructs this object and assignes the main properties.
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
  if Value <> nil then
  begin
    if Value.QueryInterface(IZAnyValue, Temp) = 0 then
    begin
      Result := SoftVarManager.Compare(FValue, Temp.GetValue) = 0;
      Temp := nil;
    end
    else
      Result := inherited Equals(Value);
  end
  else
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
function TZAnyValue.GetFloat: Extended;
begin
  Result := SoftVarManager.GetAsFloat(FValue);
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
  Gets a stored value converted to String.
  @return a stored value converted to string.
}
function TZAnyValue.GetString: String;
begin
  Result := SoftVarManager.GetAsString(FValue);
end;

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
function TZAnyValue.GetAnsiString: AnsiString;
begin
  Result := SoftVarManager.GetAsAnsiString(FValue);
end;

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
function TZAnyValue.GetUTF8String: UTF8String;
begin
  Result := SoftVarManager.GetAsUTF8String(FValue);
end;

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
function TZAnyValue.GetBytes: TByteDynArray;
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
  Gets a stored value converted to datetime.
  @return a stored value converted to datetime.
}
function TZAnyValue.GetDateTime: TDateTime;
begin
  Result := SoftVarManager.GetAsDateTime(FValue);
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
    vtBytes: Result := BytesToVar(Value.VBytes);
    vtInteger:
      if (Value.VInteger > -MaxInt) and (Value.VInteger < MaxInt) then
        Result := Integer(Value.VInteger)
      else
{$ifdef fpc}
        Result := Value.VInteger;
{$else}
        Result := IntToStr(Value.VInteger);
{$endif}
    vtFloat: Result := Value.VFloat;
    vtString: Result := Value.VString;
    vtAnsiString: Result := Value.VAnsiString;
    vtUTF8String: Result := Value.VUTF8String;
    vtRawByteString: Result := Value.VRawByteString;
    vtUnicodeString: Result := Value.VUnicodeString;
    vtDateTime: Result := Value.VDateTime;
    vtPointer:
    {$ifdef fpc}
        Result := NativeInt(Value.VPointer);
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
  case VarType(Value) of
    varSmallint, varInteger, varByte:
      Result := EncodeInteger(Integer(Value));
    varBoolean: Result := EncodeBoolean(Value);
    varString: Result := EncodeString(Value);
   {$IFDEF UNICODE}
   varUString: Result := EncodeUnicodeString(Value);
   {$ENDIF}
    varSingle, varDouble, varCurrency:
      Result := EncodeFloat(Value);
    varUnknown: Result := EncodeInterface(Value);
    varOleStr:
      Result := EncodeUnicodeString(Value);
    varDate: Result := EncodeDateTime(Value);
    varShortInt, varWord, varLongWord:
      Result := EncodeInteger(Value);
    varInt64{$IFDEF BDS5_UP},varUInt64{$ENDIF}:
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
function EncodeBytes(const Value: TByteDynArray): TZVariant;
begin
  Result.VType := vtBytes;
  Result.VBytes := Value;
end;

{**
  Creates a integer variant.
  @param Value a value to be assigned.
}
function EncodeInteger(const Value: Int64): TZVariant;
begin
  Result.VType := vtInteger;
  Result.VInteger := Value;
end;

{**
  Creates a float variant.
  @param Value a value to be assigned.
}
function EncodeFloat(const Value: Extended): TZVariant;
begin
  Result.VType := vtFloat;
  Result.VFloat := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
function EncodeString(const Value: String): TZVariant;
begin
  Result.VType := vtString;
  Result.VString := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
function EncodeAnsiString(const Value: AnsiString): TZVariant;
begin
  Result.VType := vtAnsiString;
  Result.VAnsiString := Value;
end;

{**
  Creates a UTF8String variant.
  @param Value a value to be assigned.
}
function EncodeUTF8String(const Value: UTF8String): TZVariant;
begin
  Result.VType := vtUTF8String;
  Result.VUTF8String := Value;
end;

{**
  Creates a UTF8String variant.
  @param Value a value to be assigned.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant;
begin
  Result.VType := vtRawByteString;
  Result.VRawByteString := Value;
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

initialization
  DefVarManager  := TZDefaultVariantManager.Create;
  SoftVarManager := TZSoftVariantManager.Create;
  NullVariant    := EncodeNull;
finalization
  DefVarManager  := nil;
  SoftVarManager := nil;
end.


