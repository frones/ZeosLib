{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZSysUtils;

interface

{$I ZCore.inc}

uses
  Variants, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToText
  ZMessages, ZCompatibility, FmtBCD;

type
  /// <summary>Declares a TZClientFormatSettings record.</summary>
  TZClientFormatSettings = Record
    DateFormat: String;
    DateFormatLen: Byte;
    TimeFormat: String;
    TimeFormatLen: Byte;
    DateTimeFormat: String;
    DateTimeFormatLen: Byte;
  End;

  /// <summary>Implements a EArgumentException if not known by compiler</summary>
  {$IF NOT DECLARED(EArgumentException)}
  EArgumentException = Class(Exception);
  {$IFEND}

const
  StrFalse = 'False';
  StrTrue = 'True';
  BoolStrInts: array[Boolean] of string = ('0', '1');
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolStrIntsRaw: array[Boolean] of RawByteString = ('0', '1');
  BoolStrsRaw: array[Boolean] of RawByteString = (RawByteString(StrFalse), RawByteString(StrTrue));
  YesNoStrsRaw: array[Boolean] of RawByteString = ('NO', 'YES');
  {$ENDIF}
  BoolStrs: array[Boolean] of string = (StrFalse, StrTrue);
  BoolStrsW: array[Boolean] of UnicodeString = (UnicodeString(StrFalse), UnicodeString(StrTrue));
  SQLDateTimeFmt = 'YYYY"-"MM"-"DD HH":"NN":"SS';
  SQLDateTimeFmtMSecs = 'YYYY"-"MM"-"DD HH":"NN":"SS"."ZZZ';
  YesNoStrs: array[Boolean] of string = ('NO', 'YES');

var
  TwoDigitLookupHexW: packed array[Byte] of Word;
  TwoDigitLookupHexLW: packed array[Byte] of Cardinal;
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING} //can not be initialized ...
  BoolStrIntsRaw: array[Boolean] of RawByteString;
  BoolStrsRaw: array[Boolean] of RawByteString;
  YesNoStrsRaw: array[Boolean] of RawByteString;
  {$ENDIF}

/// <summary>Determines a position of a first delimiter.</summary>
/// <param>"Delimiters" a string with possible delimiters</param>
/// <param>"Str" a string to be checked.</param>
/// <returns>a position of the first found delimiter or
///  <c>0</c> if no delimiters was found</returns>
function FirstDelimiter(const Delimiters, Str: string): Integer;

/// <summary>Determines a position of a last delimiter.</summary>
/// <param>"Delimiters" a string with possible delimiters</param>
/// <param>"Str" a string to be checked.</param>
/// <returns>a position of the first found delimiter or
///  <c>0</c> if no delimiters was found</returns>
function LastDelimiter(const Delimiters, Str: string): Integer;

/// <author>EgonHugeist</author>
/// <summary>Compares two Buffers with fixed length.</summary>
/// <param>"P1" the first Pointer to be compared</param>
/// <param>"P2" the second Pointer to be compared</param>
/// <param>"Len" the length in bytes of the given buffers</param>
/// <returns><c>0</c> if the memory equals else return byte diff on first
///  failing postion</returns>
function ZMemLComp(P1, P2: PAnsiChar; Len: Cardinal): Integer;

/// <author>EgonHugeist</author>
/// <summary>Compares two WideChar Buffers with fixed length.</summary>
/// <param>"P1" the first Pointer to be compared</param>
/// <param>"P2" the second Pointer to be compared</param>
/// <param>"Len" the length in words of the given buffers</param>
/// <returns><c>True</c> if the memory equals</returns>
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Compares two raw Buffers with fixed length.</summary>
/// <param>"P1" the first Pointer to be compared</param>
/// <param>"P2" the second Pointer to be compared</param>
/// <param>"Len" the length in bytes of the given buffers</param>
/// <returns><c>True</c> if the memory equals</returns>
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;

/// <summary>Checks is the string starts with substring.</summary>
/// <param>"Str" a UnicodeString to be compared</param>
/// <param>"SubStr">a UnicodeString to test at the start of the Str</param>
/// <returns><c>True</c> if Str starts with SubStr</returns>
function StartsWith(const Str, SubStr: UnicodeString): Boolean; overload;

/// <summary>Checks is the string starts with substring.</summary>
/// <param>"Str">a RawByteString to be compared</param>
/// <param>"SubStr">a RawByteString to test at the start of the Str</param>
/// <returns><c>True</c> if Str starts with SubStr</returns>
function StartsWith(const Str, SubStr: RawByteString): Boolean; overload;

/// <summary>Checks is the string ends with substring.</summary>
/// <param>"Str">a RawByteString the String to be compared</param>
/// <param>"SubStr">a RawByteString to test at the end of the Str</param>
/// <returns><c>True</c> if Str ends with SubStr</returns>
function EndsWith(const Str, SubStr: RawByteString): Boolean; overload;

/// <summary>Checks is the string ends with substring.</summary>
/// <param>"Str">a UnicodeString the String to be compared</param>
/// <param>"SubStr">a UnicodeString to test at the end of the Str</param>
/// <returns><c>True</c> if Str ends with SubStr</returns>
function EndsWith(const Str, SubStr: UnicodeString): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a PAnsiChar into an Extendend float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
/// <returns>a converted value or Def if conversion fails.</returns>
function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; Len: NativeUInt = 0): Extended; overload;

/// <author>EgonHugeist</author>
/// <summary>convert PAnsiChar into an Extendend float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; out Result: Extended; Len: NativeUInt = 0); overload;

/// <author>EgonHugeist</author>
/// <summary>convert PAnsiChar into a Currency value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; out Result: Currency; Len: NativeUInt = 0); overload;

/// <author>EgonHugeist</author>
/// <summary>convert PAnsiChar into a Currency value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"DecimalSep" a reference to decimal seperator to be used.
///  We will initialize this sep with '.' or replace it with the Sep we found</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: NativeUInt = 0); overload;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
/// <author>EgonHugeist</author>
/// <summary>convert PAnsiChar into a Double value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double; out Result: Double; Len: NativeUInt = 0); overload;
{$IFEND}
/// <author>EgonHugeist</author>
/// <summary>convert PAnsiChar into a Single value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a PAnsiChar of a raw character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in bytes of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single; out Result: Single; Len: NativeUInt = 0); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a PWideChar into an Extendend float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0#0 terminated.</param>
/// <returns>a converted value or Def if conversion fails.</returns>
function SQLStrToFloatDef(Value: PWideChar; const Def: Extended; Len: NativeUInt = 0): Extended; overload;

/// <author>EgonHugeist</author>
/// <summary>convert PWideChar into an Extendend float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Extended; out Result: Extended; Len: NativeUInt = 0); overload;

/// <author>EgonHugeist</author>
/// <summary>convert PWideChar into an Extendend float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; out Result: Currency; Len: NativeUInt = 0); overload;

/// <author>EgonHugeist</author>
/// <summary>convert PWideChar into a Currency value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"DecimalSep" DecimalSep a reference to decimal seperator to be used.
///  We will initialize this sep with '.' or replace it if with the Sep was found</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: NativeUInt = 0); overload;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
/// <author>EgonHugeist</author>
/// <summary>convert PWideChar into a Double float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double; out Result: Double; Len: NativeUInt = 0); overload;
{$IFEND}
/// <author>EgonHugeist</author>
/// <summary>convert PWideChar into a Single float value.
///  Supported string formats are SQLFloat, Float, Hex, Money+Suffix</summary>
/// <param>"Value" a Pointer to an UTF16 character buffer we want to convert.</param>
/// <param>"Def" a default value if the value can not be converted.</param>
/// <param>"Result" return a converted value or Def if conversion fails.</param>
/// <param>"Len" the length in words of the given buffer. If Len is zero the buffer should be #0 terminated.</param>
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single; out Result: Single; Len: NativeUInt = 0); overload;

{$IFDEF UNICODE}
/// <summary>convert a character buffer into pascal string.</summary>
/// <param>"Buffer" a character buffer pointer.</param>
/// <param>"Length" the buffer length.</param>
/// <returns>a string retrived from the buffer.</returns>
function BufferToStr(Buffer: PWideChar; Length: Integer): string;
{$ELSE}
/// <summary>convert a character buffer into pascal string.</summary>
/// <param>"Buffer" a character buffer pointer.</param>
/// <param>"Length" the buffer length.</param>
/// <returns>a string retrived from the buffer.</returns>
function BufferToStr(Buffer: PAnsiChar; Length: Integer): string;
{$ENDIF}

/// <summary>convert a buffer into an array of bytes.</summary>
/// <param>"Buffer" a buffer pointer.</param>
/// <param>"Length" the buffer length.</param>
/// <returns>a TBytes retrieved from the buffer.</returns>
function BufferToBytes(Buffer: Pointer; Length: Integer): TBytes; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <summary>Case insensitive conversion of a RawByteString into boolean value.</summary>
/// <param>"Str" a RawByteString value to be converted.</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <returns><c>True</c> if Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Case insensitive conversion of a Raw zero terminated buffer
///  into boolean value.</summary>
/// <param>"Str" a RawByteString buffer to be converted.</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <param>"IgnoreTrailingSaces" Ignore trailing spaces for fixed char fields f.e.</param>
/// <returns><c>True</c> if Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Case insensitive conversion of a Raw buffer into boolean value.</summary>
/// <param>"Buf" a RawByteString buffer to be converted.</param>
/// <param>"PEnd" the end of then buffer. Usually the zero byte</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <param>"IgnoreTrailingSaces" Ignore trailing spaces for fixed char fields f.e.</param>
/// <returns><c>True</c> if Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(Buf, PEnd: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

/// <summary>Case insensitive conversion of a UnicodeString into boolean value.</summary>
/// <param>"Str" a UnicodeString value to be converted.</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <returns><c>True</c> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(const Str: UnicodeString; const CheckInt: Boolean = True): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Case insensitive conversion of a UTF16 zero terminated buffer
///  into boolean value.</summary>
/// <param>"Str" a UTF16 buffer to be converted.</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <param>"IgnoreTrailingSaces" Ignore trailing spaces for fixed char fields f.e.</param>
/// <returns><c>True</c> if Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Case insensitive conversion of a UTF16 buffer into boolean value.</summary>
/// <param>"Buf" a UTF16 buffer to be converted.</param>
/// <param>"PEnd" the end of then buffer. Usually the zero word</param>
/// <param>"CheckInt" Check for "0" char too? Default is True</param>
/// <param>"IgnoreTrailingSaces" Ignore trailing spaces for fixed char fields f.e.</param>
/// <returns><c>True</c> if Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/!=0</returns>
function StrToBoolEx(Buf, PEnd: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

/// <summary>convert a boolean into a UnicodeString value.</summary>
/// <param>"Value" a boolean value</param>
/// <returns><c>"True"</c> or <c>"False"</c></returns>
function BoolToUnicodeEx(Value: Boolean): UnicodeString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <summary>convert a boolean into a RawByteString value.</summary>
/// <param>"Value" a boolean value</param>
/// <returns><c>"True"</c> or <c>"False"</c></returns>
function BoolToRawEx(Value: Boolean): RawByteString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <summary>convert a boolean into a String value.</summary>
/// <param>"Value" a boolean value</param>
/// <returns><c>"True"</c> or <c>"False"</c></returns>
function BoolToStrEx(Value: Boolean): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{$IFDEF ENABLE_POSTGRESQL}
/// <summary>Checks if the specified string can represent an IP address.</summary>
/// <param>"Str" a string value.</param>
/// <returns><c>True</c> if the string can represent an IP address
///  or <c>False</c> otherwise.</returns>
function IsIpAddr(const Str: string): Boolean;
{$ENDIF}

/// <summary>Splits a string using the multiple chars.</summary>
/// <param>"Str" the source string.</param>
/// <param>"Delimiters" Str the delimiters string.</param>
/// <returns>result list where plased delimited string.</returns>
function SplitString(const Str, Delimiters: string): TStrings;

/// <summary>Puts to list a splitted string using the multiple chars which
///  replaces the previous list content.</summary>
/// <param>"List" a list with strings.</param>
/// <param>"Str" the source string</param>
/// <param>"Delimiters" the delimiters string</param>
procedure PutSplitString(List: TStrings; const Str, Delimiters: string);

/// <summary>Appends to list a splitted string using the multiple chars.</summary>
/// <param>"List" a list with strings.</param>
/// <param>"Str" the source string</param>
/// <param>"Delimiters" the delimiters string</param>
procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);

/// <summary>Composes a string from the specified strings list delimited with
///  a special character.</summary>
/// <param>"List" a list of strings.</param>
/// <param>"Delimiter" a delimiters string.</param>
/// <returns>a composed string from the list.</returns>
function ComposeString(List: TStrings; const Delimiter: string): string;

/// <summary>convert a float value into SQL string with '.' decimal seperator.</summary>
/// <param>"Value" a float value to be converted.</param>
/// <returns>a converted string value.</returns>
function FloatToSQLStr(Value: Extended): string;

/// <summary>convert SQL string with '.' decimal seperator into a float value.</summary>
/// <param>"Str" a string value to be converted.</param>
/// <returns>a converted float value.</returns>
function SQLStrToFloat(const Str: String): Extended;

/// <summary>Puts to list a splitted string using the delimiter string which replaces
///  the previous list content.</summary>
/// <param>"List" a list with strings.</param>
/// <param>"Str" the source string</param>
/// <param>"Delimiters" the delimiters string</param>
procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);

/// <summary>Splits string using the delimiter string.</summary>
/// <param>"Str" the source string</param>
/// <param>"Delimiters" the delimiters string.</param>
/// <returns>the result list where placed delimited string.</returns>
function SplitStringEx(const Str, Delimiter: string): TStrings;

/// <summary>Appends to list a splitted string using the delimeter string.</summary>
/// <param>"List" a list with strings.</param>
/// <param>"Str" the source string</param>
/// <param>"Delimiters" the delimiters string.</param>
/// <returns>the result list where placed delimited string.</returns>
procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);

/// <summary>convert bytes into a RawByteString representation.</summary>
/// <param>"Value" an array of bytes to be converted.</param>
/// <returns>a converted RawByteString.</returns>
function BytesToStr(const Value: TBytes): RawByteString;

/// <summary>convert a RawByteString into an array of bytes.</summary>
/// <param>"Value" a RawByteString to be converted.</param>
/// <returns>an array of bytes.</returns>
function StrToBytes(const Value: RawByteString): TBytes; overload;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
/// <summary>convert an array of bytes into a variant representation.</summary>
/// <param>"Value" an array of bytes to be converted.</param>
/// <returns>a converted variant.</returns>
function BytesToVar(const Value: TBytes): Variant; overload;
{$ENDIF}

/// <summary>convert the RawByteString containing bytes into a byte-array
///  variant representation.</summary>
/// <param>"Value" a RawByteString to be converted.</param>
/// <returns>a byte-array converted variant.</returns>
function BytesToVar(const Value: RawByteString): Variant; {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}overload;{$ENDIF}

/// <summary>convert variant into an array of bytes.</summary>
/// <param>"Value" a varaint to be converted.</param>
/// <returns>a converted array of bytes.</returns>
function VarToBytes(const Value: Variant): TBytes;

/// <author>EgonHugeist</author>
/// <summary>convert a raw SQL Date (DateFormat) to TZDate value.
///  We do not check if the date is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  The year may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" a pointer to the raw encoded date string buffer.</param>
/// <param>"Len" the length of the buffer.</param>
/// <param>"Format" a DateFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZDate-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryRawToDate(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var Date: TZDate): Boolean;

/// <author>EgonHugeist</author>
/// <summary>convert an UTF16 SQL Date to TZDate value.
///  We do not check if the date is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  The year may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" a pointer to the UTF16 encoded date string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a DateFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZDate-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryUniToDate(Value: PWideChar; Len: Cardinal;
  const Format: String; var Date: TZDate): Boolean;

/// <author>EgonHugeist</author>
/// <summary>convert a raw SQL Date to TZTime value.
///  We do not check if the time is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second, '.','F','f','Z'/'z' for fractions.
///  The hour may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/','.',':'.</summary>
/// <param>"Value" a pointer to the raw encoded date string buffer.</param>
/// <param>"Len" the length of the buffer.</param>
/// <param>"Format" a TimeFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZTime-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryRawToTime(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var Time: TZTime): Boolean;

/// <author>EgonHugeist</author>
/// <summary>convert an UTF16 SQL Date to TZTime value.
///  We do not check if the time is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second, '.','F','f','Z'/'z' for fractions.
///  The hour may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/','.',':'.</summary>
/// <param>"Value" a pointer to the UTF16 encoded date string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a TimeFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZTime-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryUniToTime(Value: PWideChar; Len: Cardinal;
  const Format: String; var Time: TZTime): Boolean;

/// <author>EgonHugeist</author>
/// <summary>convert a raw SQL TimeStamp to TZTimeStamp value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'.</summary>
/// <param>"Value" a pointer to the raw encoded date string buffer.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TZTimeStamp-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryRawToTimeStamp(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var TimeStamp: TZTimeStamp): Boolean;

/// <author>EgonHugeist</author>
/// <summary>convert an UTF16 SQL TimeStamp to TZTimeStamp value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'.</summary>
/// <param>"Value" a pointer to the UTF16 encoded date string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TZTimeStamp-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryUniToTimeStamp(Value: PWideChar; Len: Cardinal;
  const Format: String; var TimeStamp: TZTimeStamp): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Trys to convert a TZDate-Value into a pascal TDateTime representation</summary>
/// <param>"Value" a TZDate value to be converted.</param>
/// <param>"DT" a reference to the TDateTime-Value we try to convert in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryDateToDateTime(const Value: TZDate; var DT: TDateTime): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Trys to convert a TZTime-Value into a pascal TDateTime representation</summary>
/// <param>"Value" a TZTime value to be converted.</param>
/// <param>"DT" a reference to the TDateTime-Value we try to convert in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryTimeToDateTime(const Value: TZTime; var DT: TDateTime): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Trys to convert a TZTimeStamp-Value into a pascal TDateTime representation</summary>
/// <param>"Value" a TZTimeStamp value to be converted.</param>
/// <param>"DT" a reference to the TDateTime-Value we try to convert in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryTimeStampToDateTime(const Value: TZTimeStamp; var DT: TDateTime): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Decodes a pascal TDateTime vlaue into the TZDate reprensentation</summary>
/// <param>"Value" a TDateTime value to be converted.</param>
/// <param>"Date" a reference to the TZDate-Value we decode in.</param>
procedure DecodeDateTimeToDate(const Value: TDateTime; var Date: TZDate);

/// <author>EgonHugeist</author>
/// <summary>Decodes a pascal TDateTime vlaue into the TZTime reprensentation</summary>
/// <param>"Value" a TDateTime value to be converted.</param>
/// <param>"Time" a reference to the TZTime-Value we decode in.</param>
procedure DecodeDateTimeToTime(const Value: TDateTime; var Time: TZTime);

/// <author>EgonHugeist</author>
/// <summary>Decodes a pascal TDateTime vlaue into the TZTimeStamp reprensentation</summary>
/// <param>"Value" a TDateTime value to be converted.</param>
/// <param>"TimeStamp" a reference to the TZTimeStamp-Value we decode in.</param>
procedure DecodeDateTimeToTimeStamp(const Value: TDateTime; var TimeStamp: TZTimeStamp);

/// <author>EgonHugeist</author>
/// <summary>convert a TZTime-Value into the TZTimeStamp reprensentation</summary>
/// <param>"Time" a TZTime value to be converted.</param>
/// <param>"TS" a reference to the TZTimeStamp-Value we convert in.</param>
procedure TimeStampFromTime(const Time: TZTime; var TS: TZTimeStamp);

/// <author>EgonHugeist</author>
/// <summary>convert a TZDate-Value into the TZTimeStamp reprensentation</summary>
/// <param>"Date" a TZDate value to be converted.</param>
/// <param>"TS" a reference to the TZTimeStamp-Value we convert in.</param>
procedure TimeStampFromDate(const Date: TZDate; var TS: TZTimeStamp);

/// <author>EgonHugeist</author>
/// <summary>convert a TZTimeStamp-Value into the TZTime reprensentation</summary>
/// <param>"TS" a TZTimeStamp value to be converted.</param>
/// <param>"Time" a reference to the TZTime-Value we convert in.</param>
procedure TimeFromTimeStamp(const TS: TZTimeStamp; var Time: TZTime);

/// <author>EgonHugeist</author>
/// <summary>convert a TZTimeStamp-Value into the TZDate reprensentation</summary>
/// <param>"TS" a TZTimeStamp value to be converted.</param>
/// <param>"Date" a reference to the TZDate-Value we convert in.</param>
procedure DateFromTimeStamp(const TS: TZTimeStamp; var Date: TZDate);

/// <author>EgonHugeist</author>
/// <summary>compares two pascal TDateTime-Values</summary>
/// <param>"Value1" the first value to be compared.</param>
/// <param>"Value2" the second value to be compared.</param>
/// <returns><c>0</c> if both values are equal, -1 if Value1 is less than Value2,
///  1 otherwise</returns>
function ZCompareDateTime(const Value1, Value2: TDateTime): Integer;

/// <author>EgonHugeist</author>
/// <summary>compares two TZDate-Values</summary>
/// <param>"Value1" the first value to be compared.</param>
/// <param>"Value2" the second value to be compared.</param>
/// <returns><c>0</c> if both values are equal, -1 if Value1 is less than Value2,
///  1 otherwise</returns>
function ZCompareDate(const Value1, Value2: TZDate): Integer;

/// <author>EgonHugeist</author>
/// <summary>compares two TZTime-Values</summary>
/// <param>"Value1" the first value to be compared.</param>
/// <param>"Value2" the second value to be compared.</param>
/// <returns><c>0</c> if both values are equal, -1 if Value1 is less than Value2,
///  1 otherwise</returns>
function ZCompareTime(const Value1, Value2: TZTime): Integer;

/// <author>EgonHugeist</author>
/// <summary>compares two TZTimeStamp-Values</summary>
/// <param>"Value1" the first value to be compared.</param>
/// <param>"Value2" the second value to be compared.</param>
/// <returns><c>0</c> if both values are equal, -1 if Value1 is less than Value2,
///  1 otherwise</returns>
function ZCompareTimeStamp(const Value1, Value2: TZTimeStamp): Integer;

/// <author>EgonHugeist</author>
/// <summary>Trys to convert a character buffer into a TZDate-representation
///  We do not check if the date is valid. We just convert numbers into the
///  Result record using the TZClientFormatSettings as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  The year may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported. If conversion fails, a second approach using
///  ISO8601 format is made</summary>
/// <param>"P" the pointer to a raw encoded buffer to be converted.</param>
/// <param>"Len" the length of the buffer.</param>
/// <param>"FormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Date" a reference to the TZDate-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToDate(P: PAnsiChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var Date: TZDate): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to convert a character buffer into a TZDate-representation
///  We do not check if the date is valid. We just convert numbers into the
///  Result record using the TZClientFormatSettings as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  The year may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/'. Long names are not supported.
///  If conversion fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" the pointer to a UTF16 encoded buffer to be converted.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"FormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Date" a reference to the TZDate-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToDate(P: PWideChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var Date: TZDate): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a raw SQL Time to TZTime value.
///  We do not check if the time is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second, '.','F','f','Z'/'z' for fractions.
///  The hour may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/','.',':'. If conversion
///  fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the raw encoded time string buffer.</param>
/// <param>"Len" the length of the buffer.</param>
/// <param>"Format" a TimeFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZTime-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise.</returns>
function TryPCharToTime(P: PAnsiChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var Time: TZTime): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a UTF16 SQL Time to TZTime value.
///  We do not check if the time is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second, '.','F','f','Z'/'z' for fractions.
///  The hour may be negative using explicit '-' to sign as is.
///  Valid delimiters (if given) are ' ','-','\','/','.',':'. If conversion
///  fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the UTF16 encoded time string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a TimeFormat string used for the convertion.</param>
/// <param>"Result" a reference to the TZTime-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise.</returns>
function TryPCharToTime(P: PWideChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var Time: TZTime): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Try to convert a raw SQL TimeStamp to TZTimeStamp value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'. If
///  conversion fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the raw encoded timestamp string buffer.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TZTimeStamp-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToTimeStamp(P: PAnsiChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var TimeStamp: TZTimeStamp): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Try to convert a UTF16 SQL TimeStamp to TZTimeStamp value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'. If
///  conversion fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the UTF16 encoded timestamp string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TZTimeStamp-record we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToTimeStamp(P: PWideChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var TimeStamp: TZTimeStamp): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Try to convert a raw SQL TimeStamp to pascal TDateTime value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'. If
///  conversion fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the raw encoded timestamp string buffer.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TDateTime value we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToDateTime(P: PAnsiChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var DateTime: TDateTime): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Try to convert a UTF16 SQL TimeStamp to pascal TDateTime value.
///  We do not check if the timestamp is valid. We just convert numbers into the
///  Result record using the Formatstring as match pattern. Valid format tokens are:
///  'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day, 'H'/'h' for hour,
///  'N'/'n' for minute, 'S'/'s' for second, '.','F','f','Z'/'z' for fractions,
///  'P'/'p' for the UTC offset. The year may be negative using explicit '-' to
///  sign as is. Valid delimiters (if given) are ' ','-','\','/','.',':'. If
///  conversion fails, a second approach using ISO8601 format is performed</summary>
/// <param>"P" a pointer to the UTF16 encoded timestamp string buffer.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Format" a Timestamp-Format string used for the convertion.</param>
/// <param>"Result" a reference to the TDateTime value we write in.</param>
/// <returns><c>True</c> if the conversion was successful, <c>False</c> otherwise</returns>
function TryPCharToDateTime(P: PWideChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var DateTime: TDateTime): Boolean; overload;

/// <summary>convert an ISO8601 formated String into a pascal TDateTime value.</summary>
/// <param>"Value" a UnicodeString to be converted.</param>
/// <returns>a converted value or zero if conversion fails.</returns>
function AnsiSQLDateToDateTime(const Value: UnicodeString): TDateTime; overload;

/// <summary>convert an ISO8601 formated String into a pascal TDateTime value.</summary>
/// <param>"P" a pointer to a UTF16 buffer to be converted.</param>
/// <param>"L" the length in words of the buffer.</param>
/// <returns>a converted value or zero if conversion fails.</returns>
function AnsiSQLDateToDateTime(P: PWideChar; L: LengthInt): TDateTime; overload;

/// <summary>convert an ISO8601 formated String into a pascal TDateTime value.</summary>
/// <param>"Value" a RawByteString to be converted.</param>
/// <returns>a converted value or zero if conversion fails.</returns>
function AnsiSQLDateToDateTime(const Value: RawByteString): TDateTime; overload;

/// <summary>convert an ISO8601 formated String into a pascal TDateTime value.</summary>
/// <param>"P" a pointer to a raw encoded buffer to be converted.</param>
/// <param>"L" the length in bytes of the buffer.</param>
/// <returns>a converted value or zero if conversion fails.</returns>
function AnsiSQLDateToDateTime(P: PAnsiChar; L: LengthInt): TDateTime; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string. Valid format tokens
///  are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::date.</param>
/// <returns>a formated RawByteString in Date-Format pattern</returns>
function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string buffer. Valid format
///  tokens are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"Buf" the raw buffer we write in.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::date.</param>
/// <returns>the length in bytes of written value.</returns>
function DateTimeToRawSQLDate(const Value: TDateTime; Buf: PAnsichar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a Year, Month, Day values into a string buffer. Valid format
///  tokens are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Year" the year of the date string.</param>
/// <param>"Month" the month of the date string.</param>
/// <param>"Day" the day of the date string.</param>
/// <param>"Buf" the raw buffer we write in.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Neagtive" if the year is negative.</param>
/// <returns>the length in bytes of written value.</returns>
function DateToRaw(Year, Month, Day: Word; Buf: PAnsichar;
  const Format: String; Quoted, Negative: Boolean): Byte;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string. Valid format tokens
///  are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::date.</param>
/// <returns>a formated UnicodeString in Date-Format pattern</returns>
function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string buffer. Valid format
///  tokens are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"Buf" the UTF16 buffer we write in.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::date.</param>
/// <returns>the length in words of written value.</returns>
function DateTimeToUnicodeSQLDate(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a Year, Month, Day values into a string buffer. Valid format
///  tokens are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Year" the year of the date string.</param>
/// <param>"Month" the month of the date string.</param>
/// <param>"Day" the day of the date string.</param>
/// <param>"Buf" the UTF16 buffer we write in.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Neagtive" if the year is negative.</param>
/// <returns>the length in words of written value.</returns>
function DateToUni(Year, Month, Day: Word; Buf: PWideChar;
  const Format: String; Quoted, Negative: Boolean): Byte; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a Year, Month, Day values into a string buffer. Valid format
///  tokens are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Year" the year of the date string.</param>
/// <param>"Month" the month of the date string.</param>
/// <param>"Day" the day of the date string.</param>
/// <param>"Format" the format template</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Neagtive" if the year is negative.</param>
/// <returns>a converted string.</returns>
function DateToUni(Year, Month, Day: Word; const Format, Suffix: String;
  Quoted, Negative: Boolean): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string. Valid format tokens
///  are: 'Y'/'y' for year,'M'/'m' for month,'D'/'d' for day.
///  Valid delimiters (if given) are ' ','-','\','/'.
///  Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::date.</param>
/// <returns>a formated String in Date-Format pattern</returns>
function DateTimeToSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: string = ''): string;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string. Valid format tokens
///  are 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::time.</param>
/// <returns>a formated RawByteString in Time-Format pattern</returns>
function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a raw string buffer. Valid
///  format tokens are 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for
///  second, '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::time.</param>
/// <returns>the length in bytes of written value.</returns>
function DateTimeToRawSQLTime(const Value: TDateTime; Buffer: PAnsichar;
  const ConFormatSettings: TZClientFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a time fractions into a raw string buffer. Valid
///  format tokens are 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for
///  second, '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Hour" the hour to be converted.</param>
/// <param>"Minute" the minute to be converted.</param>
/// <param>"Second" the second to be converted.</param>
/// <param>"Fractions" the second-fractions to be converted.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"IsNegative" if the value time value is negative</param>
/// <returns>the length in bytes of written value.</returns>
function TimeToRaw(Hour, Minute, Second: Word; Fractions: Cardinal;
  Buf: PAnsichar; const Format: String; Quoted: Boolean; IsNegative: Boolean): Byte; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a string. Valid format tokens
///  are 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" ia suffix string which can be appendened to the result
///  String i.e. Postgres ::time.</param>
/// <returns>a formated UnicodeString in Time-Format pattern</returns>
function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a UTF16 string buffer. Valid
///  format tokens are 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for
///  second, '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::time.</param>
/// <returns>the length in words of written value.</returns>
function DateTimeToUnicodeSQLTime(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a time fractions into a UTF16 string buffer. Valid
///  format tokens are: 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for
///  second, '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Hour" the hour to be converted.</param>
/// <param>"Minute" the minute to be converted.</param>
/// <param>"Second" the second to be converted.</param>
/// <param>"Fractions" the second-fractions to be converted.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"IsNegative" if the value time value is negative</param>
/// <returns>the length in words of written value.</returns>
function TimeToUni(Hour, Minute, Second: Word; Fractions: Cardinal;
  Buf: PWideChar; const Format: String; Quoted, IsNegative: Boolean): Byte; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into s String. Valid
///  format tokens are: 'H'/'h' for hour,'M'/'m'/'N'/'n' for minute,'S'/'s' for
///  second, '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. </summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::time.</param>
/// <returns>a formated String in Time-Format pattern</returns>
function DateTimeToSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: string = ''): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::timestamp.</param>
/// <returns>a formated RawByteString in DateTime-Format pattern</returns>
function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"Buf" the raw buffer we write in.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::timestamp.</param>
/// <returns>the length in bytes we wrote into the buffer</returns>
function DateTimeToRawSQLTimeStamp(const Value: TDateTime; Buf: PAnsiChar;
  const ConFormatSettings: TZClientFormatSettings; Quoted: Boolean;
  const Suffix: RawByteString = EmptyRaw): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert DateTime values into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. Long names are not supported.</summary>
/// <param>"Year" the year to be converted.</param>
/// <param>"Month" the month to be converted.</param>
/// <param>"Day" the day to be converted.</param>
/// <param>"Hour" the hour to be converted.</param>
/// <param>"Minute" the minute to be converted.</param>
/// <param>"Second" the second to be converted.</param>
/// <param>"Fractions" the second-fractions to be converted.</param>
/// <param>"Buf" the raw buffer we write in.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Negative" if the year should be negative</param>
/// <returns>the length in bytes we wrote into the buffer</returns>
function DateTimeToRaw(Year, Month, Day, Hour, Minute, Second: Word;
  Fractions: Cardinal; Buf: PAnsiChar; const Format: String; Quoted, Negative: Boolean): Byte;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::timestamp.</param>
/// <returns>a formated UnicodeString in DateTime-Format pattern</returns>
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a pascal TDateTime value into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':'. Long names are not supported.</summary>
/// <param>"Value" the value to be converted.</param>
/// <param>"Buf" the UTF16 buffer we write in.</param>
/// <param>"ConFormatSettings" the TZClientFormatSettings to be used as format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Suffix" a suffix string which can be appendened to the result
///  String i.e. Postgres ::timestamp.</param>
/// <returns>the length in words we wrote into the buffer</returns>
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): Word; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert DateTime values into a String. Valid
///  format tokens are: 'Y'/'y' for year,'N'/'n' for month,'D'/'d' for day,
///  'H'/'h' for hour,'M'/'m' for minute,'S'/'s' for second,
///  '.','F','f','Z'/'z' for fractions. Valid delimiters (if given) are
///  ' ','-','\','/','.',':','T'. Long names are not supported.</summary>
/// <param>"Year" the year to be converted.</param>
/// <param>"Month" the month to be converted.</param>
/// <param>"Day" the day to be converted.</param>
/// <param>"Hour" the hour to be converted.</param>
/// <param>"Minute" the minute to be converted.</param>
/// <param>"Second" the second to be converted.</param>
/// <param>"Fractions" the second-fractions to be converted.</param>
/// <param>"Buf" the UTF16 buffer we write in.</param>
/// <param>"Format" the format template</param>
/// <param>"Quoted" if the result should be quoted with a #39 char.</param>
/// <param>"Negative" if the year should be negative</param>
/// <returns>the length in words we wrote into the buffer</returns>
function DateTimeToUni(Year, Month, Day, Hour, Minute, Second: Word;
  Fractions: Cardinal; Buf: PWideChar; const Format: String;
  Quoted, Negative: Boolean): Byte;

/// <summary>convert a string into escape PostgreSQL format.</summary>
/// <param>"Value" a regular UnicodeString.</param>
/// <returns>a UnicodeString in PostgreSQL escape format.</returns>
function EncodeCString(const Value: UnicodeString): UnicodeString; overload;

/// <summary>convert a string into escape PostgreSQL format.</summary>
/// <param>"Value" a regular RawByteString.</param>
/// <returns>a RawByteString in PostgreSQL escape format.</returns>
function EncodeCString(const Value: RawByteString): RawByteString; overload;

/// <summary>convert a string from escape PostgreSQL format.</summary>
/// <param>"Value" Value a UnicodeString in PostgreSQL escape format.</param>
/// <returns>a regular UnicodeString.</returns>
function DecodeCString(const Value: UnicodeString): UnicodeString; overload;

/// <summary>convert a string from escape PostgreSQL format.</summary>
/// <param>"Value" Value a RawByteString in PostgreSQL escape format.</param>
/// <returns>a regular RawByteString.</returns>
function DecodeCString(const Value: RawByteString): RawByteString; overload;

/// <summary>convert a UTF16 buffer from escape PostgreSQL format.</summary>
/// <param>"SrcLength" the length in words of the buffer.</param>
/// <param>"SrcBuffer" a UTF16 buffer in PostgreSQL escape format.</param>
/// <param>"Result" returns a regular UnicodeString.</param>
procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PWideChar; out Result: UnicodeString); overload;

/// <summary>convert a raw buffer from escape PostgreSQL format.</summary>
/// <param>"SrcLength" the length in bytes of the buffer.</param>
/// <param>"SrcBuffer" a raw buffer in PostgreSQL escape format.</param>
/// <param>"Result" returns a regular RawByteString.</param>
procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PAnsiChar; out Result: RawByteString); overload;

/// <summary>convert a UTF16 buffer from escape PostgreSQL format.</summary>
/// <param>"SrcLength" the length in words of the buffer.</param>
/// <param>"SrcBuffer" a UTF16 buffer in PostgreSQL escape format.</param>
/// <param>"DestBuffer" the UTF16 buffer we wite the regular result in.</param>
/// <returns>the length in words we wrote in</returns>
function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PWideChar): LengthInt; overload;

/// <summary>convert a raw buffer from escape PostgreSQL format.</summary>
/// <param>"SrcLength" the length in words of the buffer.</param>
/// <param>"SrcBuffer" a raw buffer in PostgreSQL escape format.</param>
/// <param>"DestBuffer" the raw buffer we wite the regular result in.</param>
/// <returns>the length in bytes we wrote in</returns>
function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PAnsiChar): LengthInt; overload;

/// <author>Fr0sT</author>
/// <summary>Replace chars in the string.</summary>
/// <param>"Source" a char to search.</param>
/// <param>"Target" Target a char to replace.</param>
/// <param>"Str" a source string.</param>
/// <returns>a string with replaced chars.</returns>
function ReplaceChar(const Source, Target: Char; const Str: string): string; overload;

/// <author>EgonHugeist</author>
/// <summary>Replace chars in the string.</summary>
/// <param>"Str" a reference to the string with replaced chars.</param>
/// <param>"Source" a char to search.</param>
/// <param>"Target" Target a char to replace.</param>
procedure ReplaceChar(Var Str: string; const Source, Target: Char); overload;

/// <author>Fr0sT</author>
/// <summary>Remove chars in the string. More obvious and ~35 times faster
///  than StringReplace(Str, ToRemove, '')</summary>
/// <param>"ToRemove" a char to search and remove.</param>
/// <param>"Str" a source string.</param>
/// <returns>a string with removed chars.</returns>
function RemoveChar(ToRemove: Char; const Str: string): string;

/// <author>Fr0sT</author>
/// <summary>Append a string to another string separating the added string with
///  delimiter. Correctly processes cases where any of the arguments is empty</summary>
/// <param>"Str" source string to append to. If empty, resulting Str value will be AddStr.</param>
/// <param>"AddStr" string to append. If empty, Str won't be changed.</param>
/// <param>"Delimiter" string to separate AddStr from Str.</param>
procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);

/// <author>Fr0sT</author>
/// <summary>Break a string into two parts according to appearance of Delimiter.</summary>
/// <param>"Str" source string.</param>
/// <param>"Delimiter" separator string; Str=Left+Delimiter+Right.</param>
/// <param>"Left" left part of Str from the start to the first Delimiter.
///  Equals to Str if Str doesn't contain Delimiter.</param>
/// <param>"Right" right part of Str from the first Delimiter to the end.
///  Empty if Str doesn't contain Delimiter.</param>
procedure BreakString(const Str, Delimiter: String; var Left, Right: String);

/// <summary>Decodes a Full Version Value encoded with the format:
///  (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
///  into separated major, minor and subversion values</summary>
/// <param>"FullVersion" an integer containing the Full Version to decode.</param>
/// <param>"MajorVersion" an integer containing the Major Version decoded.</param>
/// <param>"MinorVersion" an integer containing the Minor Version decoded.</param>
/// <param>"SubVersion" an integer contaning the Sub Version (revision) decoded.</param>
procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);

/// <summary>Encodes major, minor and subversion (revision) values in this format:
///  (major_version * 1,000,000) + (minor_version * 1,000) + sub_version</summary>
/// <param>"MajorVersion" an integer containing the Major Version.</param>
/// <param>"MinorVersion" an integer containing the Minor Version.</param>
/// <param>"SubVersion" an integer containing the Sub Version (revision).</param>
/// <returns>an integer containing the full version.</returns>
function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;

/// <summary>Formats a Zeos SQL Version format to X.Y.Z where:
///  X = major_version, Y = minor_version, Z = sub_version</summary>
/// <param>"SQLVersion" an integer.</param>
/// <returns>Formated Zeos SQL Version Value.</returns>
function FormatSQLVersion( const SQLVersion: Integer ): String;

/// <summary>Appends WHERE clause condition. Returns ' and '+Condition if
///  Condition is not empty and empty string otherwise. This allows short
///  constructions like 'WHERE 1=1'+AppendCondition(Cond1)+
/// AppendCondition(Cond2)...</summary>
/// <param>"Condition" the condition to add.</param>
/// <returns>' and '+Condition or an empty string.</returns>
function AppendCondition(const Condition: string): string;

/// <author>EgonHugeist</author>
/// <summary>convert a raw string into an UnicodeString by widening the
///  bytes to words. This is safe only if all bytes are less or equal to 127</summary>
/// <param>"Src" the raw String to be converted.</param>
/// <returns>a converted UnicodeString</returns>
function ASCII7ToUnicodeString(const Src: RawByteString): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a raw buffer into an UnicodeString by widening the
///  bytes to words. This is safe only if all bytes are less or equal to 127</summary>
/// <param>"Src" the raw buffer to be converted.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <returns>a converted UnicodeString</returns>
function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a raw buffer into an UnicodeString by widening the
///  bytes to words. This is safe only if all bytes are less or equal to 127</summary>
/// <param>"Src" the raw buffer to be converted.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <param>"Result" a referenc to the converted UnicodeString</param>
procedure ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt; var Result: UnicodeString); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a UnicodeString into an RawbyteString by shorten the
///  words to bytes. This is safe only if all words are less or equal to 127</summary>
/// <param>"Src" the UnicodeString to be converted.</param>
/// <returns>a converted RawByteString</returns>
function UnicodeStringToASCII7(const Src: UnicodeString): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a UTF16 buffer into an RawbyteString by shorten the
///  words to bytes. This is safe only if all words are less or equal to 127</summary>
/// <param>"Src" the UTF16 buffer to be converted.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <returns>a converted RawByteString</returns>
function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a UTF16 buffer into an RawbyteString by shorten the
///  words to bytes. This is safe only if all words are less or equal to 127</summary>
/// <param>"Src" the UTF16 buffer to be converted.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Result" a reference to the converted RawByteString</returns>
procedure UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt; var Result: RawByteString); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a RawByteString using ffGeneral format.
///  DecimalSeparator is from global formatsettings</summary>
/// <param>"Value" the Value to be converted.</param>
/// <returns>a converted RawByteString</returns>
function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a raw buffer using ffGeneral format.
///  DecimalSeparator is from global formatsettings</summary>
/// <param>"Value" the Value to be converted.</param>
/// <param>"Buf" the buffer we write in.</param>
/// <returns>the length in bytes we wrote.</returns>
function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a RawByteString using ffGeneral format.
///  DecimalSeparator is always the '.'(dot)</summary>
/// <param>"Value" the Value to be converted.</param>
/// <returns>a converted RawByteString</returns>
function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a raw buffer using ffGeneral format.
///  DecimalSeparator is always the '.'(dot)</summary>
/// <param>"Value" the Value to be converted.</param>
/// <param>"Buf" the buffer we write in.</param>
/// <returns>the length in bytes we wrote.</returns>
function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a UnicodeString using ffGeneral format.
///  DecimalSeparator is from global formatsettings</summary>
/// <param>"Value" the Value to be converted.</param>
/// <returns>a converted UnicodeString</returns>
function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): UnicodeString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a UTF16 buffer using ffGeneral format.
///  DecimalSeparator is from global formatsettings</summary>
/// <param>"Value" the Value to be converted.</param>
/// <param>"Buf" the buffer we write in.</param>
/// <returns>the length in words we wrote.</returns>
function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a UnicodeString using ffGeneral format.
///  DecimalSeparator is always the '.'(dot)</summary>
/// <param>"Value" the Value to be converted.</param>
/// <returns>a converted UnicodeString</returns>
function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): UnicodeString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a float value into a UTF16 buffer using ffGeneral format.
///  DecimalSeparator is always the '.'(dot)</summary>
/// <param>"Value" the Value to be converted.</param>
/// <param>"Buf" the buffer we write in.</param>
/// <returns>the length in words we wrote.</returns>
function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer into it's hexadecimal representation</summary>
/// <param>"Buffer" the buffer to be converted.</param>
/// <param>"Text" the raw buffer we write in. Size of the buffer must be Lenx2.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
procedure ZBinToHex(Buffer, Text: PAnsiChar; const Len: LengthInt); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer into it's hexadecimal representation</summary>
/// <param>"Buffer" the buffer to be converted.</param>
/// <param>"Text" the UTF16 buffer we write in. Length in words of the buffer must be Lenx2.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
procedure ZBinToHex(Buffer: PAnsiChar; Text: PWideChar; const Len: LengthInt); overload;

type
  /// <summary>defines GUID conversion options</summary>
  TGUIDConvOption = (guidWithBrackets, guidQuoted, guidSet0Term);
  /// <summary>defines a set of GUID conversion options</summary>
  TGUIDConvOptions = set of TGUIDConvOption;

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer 16 byte long buffer into it's human
///  readable representation.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Dest" the raw buffer we write in.</param>
/// <param>"Options" the conversion options.</param>
procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar; const Options: TGUIDConvOptions); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer 16 byte long buffer into it's human
///  readable representation.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Dest" the raw buffer we write in.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <param>"SetTerm" write a trailing #0 byte?</param>
procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar; WithBrackets, SetTerm: Boolean); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer 16 byte long buffer into it's human
///  readable representation.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Dest" the UTF16 buffer we write in.</param>
/// <param>"Options" the conversion options.</param>
procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const Options: TGUIDConvOptions); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a binary buffer 16 byte long buffer into it's human
///  readable representation.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Dest" the UTF16 buffer we write in.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <param>"SetTerm" write a trailing #0#0 word?</param>
procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; WithBrackets, SetTerm: Boolean); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>GUID reference into it's human readable representation.</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <param>"Result" a reference to the RawByteString we fill.</param>
procedure GUIDToRaw(Value: PGUID; const Options: TGUIDConvOptions; var Result: RawByteString); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a GUID into it's human readable representation</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <returns>the RawByteString we've filled.</returns>
function GUIDToRaw(const GUID: TGUID; const Options: TGUIDConvOptions): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a GUID into it's human readable representation.</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the RawByteString we've filled.</returns>
function GUIDToRaw(const GUID: TGUID; WithBrackets: Boolean = True): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a 16byte long byte array into it's human readable representation.</summary>
/// <param>"Bts" a dynamic byte-array. If length of this array !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the RawByteString we've filled.</returns>
function GUIDToRaw(const Bts: TBytes; WithBrackets: Boolean = True): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a buffer into it's human readable representation.</summary>
/// <param>"Buffer" a buffer we read from.</param>
/// <param>"Len" the length of the buffer we read from. If this value !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the RawByteString we've filled.</returns>
function GUIDToRaw(Buffer: Pointer; Len: Byte; WithBrackets: Boolean = True): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>GUID reference into it's human readable representation.</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <param>"Result" a reference to the UnicodeString we fill.</param>
procedure GUIDToUnicode(Value: PGUID; const Options: TGUIDConvOptions; var Result: UnicodeString); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a GUID into it's human readable representation</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <returns>the UnicodeString we've filled.</returns>
function GUIDToUnicode(const GUID: TGUID; const Options: TGUIDConvOptions): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a GUID into it's human readable representation.</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the UnicodeString we've filled.</returns>
function GUIDToUnicode(const GUID: TGUID; WithBrackets: Boolean = True): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a 16byte long byte array into it's human readable representation.</summary>
/// <param>"Bts" a dynamic byte-array. If length of this array !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the UnicodeString we've filled.</returns>
function GUIDToUnicode(const Bts: TBytes; WithBrackets: Boolean = True): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>convert a buffer into it's human readable representation.</summary>
/// <param>"Buffer" a buffer we read from.</param>
/// <param>"Len" the length of the buffer we read from. If this value !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the UnicodeString we've filled.</returns>
function GUIDToUnicode(Buffer: Pointer; Len: Byte; WithBrackets: Boolean = True): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>GUID reference into it's human readable representation.</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <param>"Result" a reference to the String we fill.</param>
function GUIDToStr(Value: PGUID; const Options: TGUIDConvOptions): String; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a GUID into it's human readable representation</summary>
/// <param>"Value" a reference the TGUID to be converted.</param>
/// <param>"Options" the conversion options.</param>
/// <returns>the String we've filled.</returns>
function GUIDToStr(const GUID: TGUID; WithBrackets: Boolean = True): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a 16byte long byte array into it's human readable representation.</summary>
/// <param>"Bts" a dynamic byte-array. If length of this array !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the String we've filled.</returns>
function GUIDToStr(const Bts: TBytes; WithBrackets: Boolean = True): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a buffer into it's human readable representation.</summary>
/// <param>"Buffer" a buffer we read from.</param>
/// <param>"Len" the length of the buffer we read from. If this value !=16
///  a EArgumentException is raised.</param>
/// <param>"WithBrackets" should the value start with '{' and end with '}'?</param>
/// <returns>the String we've filled.</returns>
function GUIDToStr(Buffer: Pointer; Len: Byte; WithBrackets: Boolean = True): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>convert a raw human readable representation of a guid into
///  it's binary 16 format. Conversion is 1,5x faster than compiler's</summary>
/// <param>"Src" a raw buffer we read from.</param>
/// <param>"Dest" the buffer we write in.</param>
procedure ValidGUIDToBinary(Src, Dest: PAnsiChar); overload;

/// <author>EgonHugeist</author>
/// <summary>convert a UTF16 human readable representation of a guid into
///  it's binary 16 format. Conversion is 1,5x faster than compiler's</summary>
/// <param>"Src" a UTF16 buffer we read from.</param>
/// <param>"Dest" the buffer we write in.</param>
procedure ValidGUIDToBinary(Src: PWideChar; Dest: PAnsiChar); overload;

/// <author>EgonHugeist</author>
/// <summary>Standard quoting. Result starts and ends with a quote, each half
///  quote get's a second quote.</summary>
/// <param>"S" a String to be quoted.</param>
/// <param>"Quote" the wide quote-char to be used</param>
/// <returns>the quoted UnicodeString.</returns>
function SQLQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>Standard quoting. Result starts and ends with a quote, each half
///  quote get's a second quote.</summary>
/// <param>"Src" a UTF16 buffer to be quoted.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Quote" the wide quote-char to be used</param>
/// <returns>the quoted UnicodeString.</returns>
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Standard quoting. Result starts and ends with a quote, each half
///  quote get's a second quote.</summary>
/// <param>"S" a String to be quoted.</param>
/// <param>"Quote" the quote-char to be used</param>
/// <returns>the quoted RawByteString.</returns>
function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>Standard quoting. Result starts and ends with a quote, each half
///  quote get's a second quote.</summary>
/// <param>"Src" a raw buffer to be quoted.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <param>"Quote" the quote-char to be used</param>
/// <returns>the quoted RawByteString.</returns>
function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString; overload;

/// <author>Fr0sT</author>
/// <summary>Standard quoting with 2 quoting chars: Result := QuoteLeft +
///  Double_Quotes(Src, QuoteLeft, QuoteRight) + QuoteRight. This version is a
///  bit slower than that with one quoting char so use it only when
///  QuoteLeft!=QuoteRight</summary>
/// <param>"S" a String to be quoted.</param>
/// <param>"QuoteLeft" the left quote-char to be used.</param>
/// <param>"QuoteRight" the right quote-char to be used.</param>
/// <returns>the quoted String.</returns>
function SQLQuotedStr(const S: string; QuoteLeft, QuoteRight: Char): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <author>Fr0sT</author>
/// <summary>Standard quoting with 2 quoting chars: Result := QuoteLeft +
///  Double_Quotes(Src, QuoteLeft, QuoteRight) + QuoteRight. This version is a
///  bit slower than that with one quoting char so use it only when
///  QuoteLeft!=QuoteRight</summary>
/// <param>"Src" a String buffer to be quoted.</param>
/// <param>"Len" the length of the buffer.</param>
/// <param>"QuoteLeft" the left quote-char to be used.</param>
/// <param>"QuoteRight" the right quote-char to be used.</param>
/// <returns>The quoted String.</returns>
function SQLQuotedStr(Src: PChar; Len: LengthInt; QuoteLeft, QuoteRight: Char): string; overload;

/// <author>EgonHugeist</author>
/// <summary>Standard dequoting of a String.</summary>
/// <param>"S" a String to be dequoted.</param>
/// <param>"QuoteChar" the quote-char to be used.</param>
/// <returns>The dequoted String. If Quotes are not given or quote-count
///  dosn't match the Quote rules, then the original string is returned.</returns>
function SQLDequotedStr(const S: string; QuoteChar: Char): string; overload;

/// <author>EgonHugeist</author>
/// <summary>Standard dequoting of a string buffer.</summary>
/// <param>"Src" a buffer to be dequoted.</param>
/// <param>"Len" the buffer length. Either count of bytes for Ansi-Compiler or
///  words for Unicode comiler.</param>
/// <param>"QuoteChar" the quote-char to be used.</param>
/// <returns>The dequoted String. If Quotes are not given or quote-count
///  dosn't match the Quote rules, then the original string is returned.</returns>
function SQLDequotedStr(Src: PChar; Len: LengthInt; QuoteChar: Char): string; overload;

/// <author>Fr0sT</author>
/// <summary>Dequoting of a string buffer using different quote chars for State&End.</summary>
/// <param>"S" a String to be dequoted.</param>
/// <param>"QuoteLeft" the left quote-char to be used.</param>
/// <param>"QuoteRight" the rigth quote-char to be used.</param>
/// <returns>The dequoted String. If Quotes are not matching the dequoting rules
///  a EArgumentException is raised (Decison by author). </returns>
function SQLDequotedStr(const S: string; QuoteLeft, QuoteRight: Char): string; overload;

/// <author>EgonHugeist</author>
/// <summary>Standard dequoting of a raw buffer into another raw buffer.</summary>
/// <param>"pSrc" a String buffer to be dequoted.</param>
/// <param>"pDst" a String buffer to we write in.</param>
/// <param>"Len" a reference to a lenght in bytes. <c>In</c> the actual length
///  of the source buffer. <c>Out</c> the length which was written into the dest
///  buffer.</param>
/// <param>"QuoteChar" the quote-char to be used.</param>
procedure SQLDequotedStr(pSrc, pDst: PAnsiChar; var Len: LengthInt; QuoteChar: AnsiChar); overload;

/// <author>EgonHugeist</author>
/// <summary>ASCII7 case insensitive compare of two buffer with minimum same
///  length.</summary>
/// <param>"Val1" the first raw buffer to be compared.</param>
/// <param>"Val2" the first raw buffer to be compared.</param>
/// <param>"Len" the lenght in bytes of the buffers.</param>
/// <returns><c>True</c> if the comparsion was successful, <c>False</c> otherwise</returns>
function SameText(Val1, Val2: PAnsiChar; Len: LengthInt): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>ASCII7 case insensitive compare of two buffer with minimum same
///  length.</summary>
/// <param>"Val1" the first UTF16 buffer to be compared.</param>
/// <param>"Val2" the first UTF16 buffer to be compared.</param>
/// <param>"Len" the lenght in words of the buffers.</param>
/// <returns><c>True</c> if the comparsion was successful, <c>False</c> otherwise</returns>
function SameText(Val1, Val2: PWideChar; Len: LengthInt): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trims raw buffer. Each starting and trailing character smaller
///  than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"L" <c>In</c> the length in bytes of the buffer. <c>Out</c> the
///  length in bytes the final buffer have after right-trimming.</param>
/// <param>"P" <c>In</c> a reference to the input buffer which should be
///  trimmed. <c>Out</c> the new refernece of the input buffer after left
///  trimming was done.</param>
procedure Trim(var L: NativeUInt; var P: PAnsiChar); overload;

/// <author>EgonHugeist</author>
/// <summary>Trims UTF16 buffer. Each starting and trailing character smaller
///  than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"L" <c>In</c> the length in words of the buffer. <c>Out</c> than
///  length in words the final buffer have after right-trimming.</param>
/// <param>"P" <c>In</c> a reference to the input buffer which should be
///  trimmed. <c>Out</c> the new refernece of the input buffer after left
///  trimming was done.</param>
procedure Trim(var L: NativeUInt; var P: PWideChar); overload;

/// <author>EgonHugeist</author>
/// <summary>Trims raw buffer into a raw string. Each starting and trailing
///  character smaller than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"P" the input buffer which should be trimmed. </param>
/// <param>"L" the length in bytes of the buffer.</param>
/// <returns>a trimmed raw encoded string.</returns>
function Trim(P: PAnsiChar; L: LengthInt): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Trims raw #0 terminated buffer into a raw string. Each starting and
/// trailing character smaller then ordinal(' '/space) get's trimmed away.</summary>
/// <param>"P" the input buffer which should be trimmed. </param>
/// <returns>a trimmed raw encoded string.</returns>
function Trim(P: PAnsiChar): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Trims a UTF16 buffer. Each starting and trailing character smaller
///  than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"P" <c>In</c>the input buffer which should be trimmed.</param>
/// <param>"L" <c>In</c> the length in words of the buffer.</param>
/// <returns>a trimmed UnicodeString.</returns>
function Trim(P: PWideChar; L: LengthInt): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Left trims a raw buffer. Each starting and trailing character
///  smaller than ordinal (' '/space) get's trimmed away.</summary>
/// <param>"P" <c>In</c>the input buffer which should be left trimmed.</param>
/// <param>"L" <c>In</c> the length in bytes of the buffer.</param>
/// <returns>a trimmed RawByteString.</returns>
function LTrim(P: PAnsiChar; L: LengthInt): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Right trims a raw buffer. Each starting and trailing character
///  smaller than ordinal (' '/space) get's trimmed away.</summary>
/// <param>"P" <c>In</c>the input buffer which should be right trimmed.</param>
/// <param>"L" <c>In</c> the length in bytes of the buffer.</param>
/// <returns>a trimmed RawByteString.</returns>
function RTrim(P: PAnsiChar; L: LengthInt): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Left trims a UTF16 buffer. Each starting and trailing character
///  smaller than ordinal (' '/space) get's trimmed away.</summary>
/// <param>"P" <c>In</c>the input buffer which should be right trimmed.</param>
/// <param>"L" <c>In</c> the length in words of the buffer.</param>
/// <returns>a trimmed UnicodeString.</returns>
function LTrim(P: PWideChar; L: LengthInt): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Right trims a UTF16 buffer. Each starting and trailing character
///  smaller than ordinal (' '/space) get's trimmed away.</summary>
/// <param>"P" <c>In</c>the input buffer which should be right trimmed.</param>
/// <param>"L" <c>In</c> the length in words of the buffer.</param>
/// <returns>a trimmed UnicodeString.</returns>
function RTrim(P: PWideChar; L: LengthInt): UnicodeString; overload;

{$IFNDEF UNICODE}
/// <author>EgonHugeist</author>
/// <summary>Trims a UnicodeString. Each starting and trailing character smaller
///  than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"Value" the UTF16 string to be trimmed.</param>
/// <returns>a trimmed UnicodeString.</returns>
function Trim(const Value: UnicodeString): UnicodeString; overload;
{$ENDIF}

{$IF defined(UNICODE) and not defined(WITH_UNITANSISTRINGS)}
/// <author>EgonHugeist</author>
/// <summary>Trims a rawbytestring. Each starting and trailing character smaller
///  than ordinal(' '/space) get's trimmed away.</summary>
/// <param>"Value" the raw string to be trimmed.</param>
/// <returns>a trimmed RawByteString.</returns>
function Trim(const Value: RawByteString): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>ASCII7 lowercase a raw encoded string.</summary>
/// <param>"Value" the raw string.</param>
/// <returns>a lowercase RawByteString.</returns>
function LowerCase(const Value: RawByteString): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>ASCII7 uppercase a raw encoded string.</summary>
/// <param>"Value" the raw string.</param>
/// <returns>a uppercase RawByteString.</returns>
function UpperCase(const Value: RawByteString): RawByteString; overload;
{$IFEND}

{$IFDEF NO_RAW_HEXTOBIN}
/// <author>Arnaud Bouchez</author>
/// <summary>fast conversion from hexa chars into binary data. Using this
///  function with Bin^ as an integer value will decode in big-endian order
///  (most-signignifican byte first)</summary>
/// <param>"Hex" the buffer must contain at least BinBytes*2 chars to be converted.</param>
/// <param>"Bin" the buffer must reserve BinBytes space. if Bin=nil, no output
///  data is written, but the Hex^ format is checked.</param>
/// <returns><c>False</c> if any invalid (non hexa) char is found in Hex^,
///  <c>True</c> otherwise.</returns>
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): Boolean;
{$ENDIF}

/// <summary>Creates a memory stream with copy of data in buffer. If buffer
///  contains no data, creates an empty stream.</summary>
/// <param>"Buffer" the buffer copied into the result stream.</param>
/// <param>"Size" the size in bytes of the buffer.</param>
/// <returns>a TMemoryStream object.</returns>
function StreamFromData(Buffer: Pointer; Size: Integer): TMemoryStream; overload;

/// <summary>Creates a memory stream with copy of a UnicodeString</summary>
/// <param>"AString" the source string to be copied.</param>
/// <returns>a TMemoryStream object.</returns>
function StreamFromData(const AString: UnicodeString): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

/// <summary>Creates a memory stream with copy of a byte array.</summary>
/// <param>"Bytes" the source bytes to be copied.</param>
/// <returns>a TMemoryStream object.</returns>
function StreamFromData(const Bytes: TBytes): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
/// <summary>Creates a memory stream with copy of a RawByteString</summary>
/// <param>"AString" the source string to be copied.</param>
/// <returns>a TMemoryStream object.</returns>
function StreamFromData(const AString: RawByteString): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
{$ENDIF}

/// <author>EgonHugeist</author>
/// <summary>Encode a currency value to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
procedure Currency2Bcd(const Value: Currency; var Result: TBCD);

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled Int64 to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
procedure ScaledOrdinal2Bcd(const Value: Int64; Scale: Byte; var Result: TBCD); overload;

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled UInt64 to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
/// <param>"Negative" if Result should be negative.</param>
procedure ScaledOrdinal2Bcd(Value: UInt64; Scale: Byte; var Result: TBCD; Negative: Boolean); overload;

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled Integer to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
procedure ScaledOrdinal2Bcd(Value: Integer; Scale: Byte; var Result: TBCD); overload;

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled Cardinal to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
/// <param>"Negative" if Result should be negative.</param>
procedure ScaledOrdinal2Bcd(Value: Cardinal; Scale: Byte; var Result: TBCD; Negative: Boolean); overload;

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled SmallInt to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
procedure ScaledOrdinal2Bcd(Value: SmallInt; Scale: Byte; var Result: TBCD); overload;

/// <author>EgonHugeist</author>
/// <summary>Encode a scaled Word to a TBCD.</summary>
/// <param>"Value" the source value to be encoded.</param>
/// <param>"Scale" the the scale digits.</param>
/// <param>"Result" a TBCD-record reference to write in.</param>
/// <param>"Negative" if Result should be negative.</param>
procedure ScaledOrdinal2Bcd(Value: Word; Scale: Byte; var Result: TBCD; Negative: Boolean); overload;

/// <author>EgonHugeist</author>
/// <summary>Case sensitive string replace all. Length of NewPattern must be
///  smaller or equal to NewPattern. Otherwise if Assertiation are turned off
///  the Results my be unexpected.</summary>
/// <param>"Source" the raw source value we replace from.</param>
/// <param>"OldPattern" the raw search string.</param>
/// <param>"NewPattern" the raw new string.</param>
/// <returns>a RawByteString.</returns>
function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Case sensitive string replace all. Length of NewPattern must be
///  smaller or equal to NewPattern. Otherwise if Assertiation are turned off
///  the Results my be unexpected.</summary>
/// <param>"Source" the UTF16 source value we replace from.</param>
/// <param>"OldPattern" the UTF16 search string.</param>
/// <param>"NewPattern" the UTF16 new string.</param>
/// <returns>a UnicodeString.</returns>
function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: UnicodeString): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Case sensitive string replace all. Length of NewPattern must be
///  greater or equal to NewPattern. Otherwise if Assertiation are turned off
///  the Results my be unexpected.</summary>
/// <param>"Source" the raw source value we replace from.</param>
/// <param>"OldPattern" the raw search string.</param>
/// <param>"NewPattern" the raw new string.</param>
/// <returns>a RawByteString.</returns>
function StringReplaceAll_CS_GToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Case sensitive string replace all. Length of NewPattern must be
///  greater or equal to NewPattern. Otherwise if Assertiation are turned off
///  the Results my be unexpected.</summary>
/// <param>"Source" the UTF16 source value we replace from.</param>
/// <param>"OldPattern" the UTF16 search string.</param>
/// <param>"NewPattern" the UTF16 new string.</param>
/// <returns>a UnicodeString.</returns>
function StringReplaceAll_CS_GToEQ(const Source, OldPattern, NewPattern: UnicodeString): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a TBCD value into a string. Decimal-seperator is allways
///  the '.'(dot). Trailing and leading zeroes are padded away, means then
///  result is allways the shortes representation.</summary>
/// <param>"Value" the value to be converted.</param>
/// <returns>a converted RawByteString.</returns>
function BcdToSQLRaw({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a TBCD value into a string. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.</summary>
/// <param>"Bcd" the value to be converted.</param>
/// <param>"Buf" a raw buffer we write in. Buffer must have reserve enough space.</param>
/// <param>"DecimalSep" the Decimal-seperator to be used.</param>
/// <returns>the count of bytes we wrote into the buffer.</returns>
function BcdToRaw({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Bcd: TBcd; Buf: PAnsiChar; DecimalSep: Char): LengthInt; overload;

/// <author>EgonHugeist</author>
/// <summary>Encodes a TBCD value from raw buffer. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.
///  Decimal-Seperator is the '.'(dot). If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the value buffer we read from.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <returns>Encoded BCD-record.</returns>
function RawToBCD(Value: PAnsiChar; Len: LengthInt): TBCD; overload;

/// <author>EgonHugeist</author>
/// <summary>Encodes a TBCD value from a string. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.
///  Decimal-Seperator is the '.'(dot). If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the raw string to be used for the conversion.</param>
/// <returns>Encoded BCD-record.</returns>
function RawToBCD(const Value: RawByteString): TBCD; overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a TBCD value into a string. Decimal-seperator is allways
///  the '.'(dot). Trailing and leading zeroes are padded away, means then
///  result is allways the shortes representation.</summary>
/// <param>"Value" the value to be converted.</param>
/// <returns>a converted UnicodeString.</returns>
function BcdToSQLUni({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): UnicodeString;

/// <author>EgonHugeist</author>
/// <summary>Decodes a TBCD value into a string. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.</summary>
/// <param>"Bcd" the value to be converted.</param>
/// <param>"Buf" a UTF16 buffer we write in. Buffer must have reserve enough space.</param>
/// <param>"DecimalSep" the Decimal-seperator to be used.</param>
/// <returns>the count of words we wrote into the buffer.</returns>
function BcdToUni({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Bcd: TBcd; Buf: PWideChar; DecimalSep: Char): LengthInt;

/// <author>EgonHugeist</author>
/// <summary>Encodes a TBCD value from UTF16 buffer. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.
///  Decimal-Seperator is the '.'(dot). If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the value buffer we read from.</param>
/// <param>"Len" the length in words of the buffer.</param>
/// <returns>Encoded BCD-record.</returns>
function UniToBCD(Value: PWideChar; Len: LengthInt): TBCD; overload;

/// <author>EgonHugeist</author>
/// <summary>Encodes a TBCD value from a string. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.
///  Decimal-Seperator is the '.'(dot). If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the UniocdeString to be used for the conversion.</param>
/// <returns>Encoded BCD-record.</returns>
function UniToBCD(const Value: UnicodeString): TBCD; overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a BCD to a Int64. If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the value to be decoded.</param>
/// <param>"Result" the Int64 decoded.</param>
procedure BCD2Int64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD; out Result: Int64); overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a BCD to a Int64. If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the value to be decoded.</param>
/// <returns>the Int64 decoded.</returns>
function BCD2Int64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): Int64; overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a BCD to a UInt64</summary>
/// <param>"Value" the value to be decoded.</param>
/// <param>"Result" the UInt64 decoded.</param>
procedure BCD2UInt64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD; out Result: UInt64); overload;

/// <author>EgonHugeist</author>
/// <summary>Decodes a BCD to a UInt64. If conversion fails a EBcdException
///  get's raised.</summary>
/// <param>"Value" the value to be decoded.</param>
/// <returns>the UInt64 decoded.</returns>
function BCD2UInt64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): UInt64; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a String.</summary>
/// <param>"Value" the UnicodeString to be used.</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"FormatSettings" the FormatSettings to be used for the
///  decimal-seperator.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryUniToBcd(const Value: UnicodeString; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a String.</summary>
/// <param>"Value" the UnicodeString to be used.</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"FormatSettings" the FormatSettings to be used for the
///  decimal-seperator.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryUniToBcd(const Value: UnicodeString; var Bcd: TBcd; DecimalSep: Char): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a UTF16 String buffer.</summary>
/// <param>"Buf" the UTF16 buffer to be used.</param>
/// <param>"Len" the length in words of the buffer</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"DecimalSep" the decimal-seperator to be used.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryUniToBcd(Buf: PWideChar; Len: LengthInt; var Bcd: TBcd; DecimalSep: Char): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a String.</summary>
/// <param>"Value" the RawByteString to be used.</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"FormatSettings" the FormatSettings to be used for the
///  decimal-seperator.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryRawToBcd(const Value: RawByteString; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a String.</summary>
/// <param>"Value" the RawByteString to be used.</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"DecimalSep" the decimal-seperator to be used.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryRawToBcd(const Value: RawByteString; var Bcd: TBcd; DecimalSep: Char): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a raw String buffer.</summary>
/// <param>"Buf" the raw buffer to be used.</param>
/// <param>"Len" the length in words of the buffer</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"DecimalSep" the decimal-seperator to be used.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryRawToBcd(Buf: PAnsiChar; Len: LengthInt; var Bcd: TBcd; DecimalSep: Char): Boolean; overload;

/// <author>EgonHugeist</author>
/// <summary>Trys to encode a BCD from a String.</summary>
/// <param>"Value" the String to be used.</param>
/// <param>"Bcd" the reference to the BCD we write in.</param>
/// <param>"FormatSettings" the FormatSettings to be used for the
///  decimal-seperator.</param>
/// <returns><c>True</c> if conversion was successfull;
///  <c>False</c> otherwise.</returns>
function TryStr2BCD(const Value: String; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean;

/// <author>EgonHugeist</author>
/// <summary>Encodes a TBCD value from a string. Trailing and leading zeroes
///  are padded away, means then result is allways the shortes representation.
///  If conversion fails a EBcdException get's raised.</summary>
/// <param>"Value" the raw string to be used for the conversion.</param>
/// <param>"FormatSettings" the FormatSettings to be used for the
///  decimal-seperator.</param>
/// <returns>Encoded BCD-record.</returns>
function Str2BCD(const Value: String{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): TBCD;

/// <author>EgonHugeist</author>
/// <summary>Encode a Double Value into it's TBCD representation.</summary>
/// <param>"Value" the floiting value be used for the conversion.</param>
/// <param>"Result" the BCD-record reference we write in.</param>
procedure Double2BCD(const Value: Double; var Result: TBCD);

/// <author>EgonHugeist</author>
/// <summary>get reading offsets of the bcd, including packet Precsion and Scale</summary>
/// <param>"Value" a const reference to the BCD.</param>
/// <param>"PNibble" a pointer to the first significant nibble.</param>
/// <param>"PLastNibble" a pointer to the last significant nibble.</param>
/// <param>"Precision" the packed result precision.</param>
/// <param>"Scale" the packed result scale.</param>
/// <param>"GetFirstBCDHalfByte" indicate if the first halfbyte of PNibble counts.</param>
/// <returns><c>True</c> if packing is possible; <c>False</c> otherwise.</returns>
function GetPacketBCDOffSets({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD;
  out PNibble, PLastNibble: PAnsiChar; out Precision, Scale: Word; out GetFirstBCDHalfByte: Boolean): Boolean;

/// <author>EgonHugeist</author>
/// <summary>pack the bcd to top of data, remove useless trailing or leading zeros.</summary>
/// <param>"Value" a reference to the TBCD record which should be packed.</param>
/// <param>"PNibble" a reference to the first nibble containing significant data.</param>
/// <param>"PLastNibble" a reference to the last nibble containing significant data.</param>
/// <param>"Precision" the new Precision we write.</param>
/// <param>"Scale" the new Scale we write.</param>
/// <param>"GetFirstBCDHalfByte" does the first half-byte count?</param>
procedure ZPackBCDToLeft(var Value: TBCD; var PNibble, PLastNibble: PAnsiChar;
  Precision, Scale: Word; GetFirstBCDHalfByte: Boolean);

/// <author>EgonHugeist</author>
/// <summary>Compares and packs two TBCD records.</summary>
/// <param>"Value1" a reference to the first BCD that should pe compared.</param>
/// <param>"Value2" a reference to the second BCD that should pe compared.</param>
/// <returns><c>0</c> if both values are equal, -1 if Value1 is less than Value2,
///  1 otherwise</returns>
function ZBCDCompare(var Value1, Value2: TBCD): Integer;

/// <summary>defines a currency fraction rounding scale.</summary>
Type TCurrRoundToScale = 0..4;

/// <author>EgonHugeist</author>
/// <summary>round a currency value half away from zero to it's exact scale
///  digits.</summary>
/// <param>"Value" the value to be rounded.</param>
/// <param>"Scale" the exact scale digt we want to round valid is 0..4. even
///  if 4 is a noop</param>
/// <returns>a rounded value</returns>
function RoundCurrTo(const Value: Currency; Scale: TCurrRoundToScale): Currency;

/// <summary>defines a nano second fraction rounding scale.</summary>
Type TFractionRoundToScale = 0..9;

/// <author>EgonHugeist</author>
/// <summary>round a fraction cardinal value half away from zero to it's exact
///  scale digits.</summary>
/// <param>"Value" the value to be rounded.</param>
/// <param>"Scale" the exact scale digt we want to round valid is 0..9. even
///  if 9 is a noop</param>
/// <returns>a rounded value</returns>
function RoundNanoFractionTo(const Value: Cardinal;
  Scale: TFractionRoundToScale): Cardinal;

/// <author>EgonHugeist</author>
/// <summary>Find a position of a Char using ASCII7 case insensitive
///  comparsion.</summary>
/// <param>"C" the Char to be searched.</param>
/// <param>"Str" the String we search in.</param>
/// <returns><c>0</c> if the char could not be found; The 1-based offset
///  otherwise</returns>
function CharPos_CI(C: Char; const Str: string): NativeInt;

/// <author>EgonHugeist</author>
/// <summary>round a nano second fraction value up to milliseconds.</summary>
/// <param>"Value" the value to be rounded.</param>
/// <returns>a rounded value with millisecond precision</returns>
function RoundNanoFractionToMillis(const Value: Cardinal): Word;

Type
  TZBCDScale = 0..MaxFMTBcdFractionSize;

/// <author>EgonHugeist</author>
/// <summary>rounds a TBCD half away from zero to it's given scale.</summary>
/// <param>"Value" the value to be rounded.</param>
/// <param>"Scale" the TZBCDScale used for rounding.</param>
/// <param>"Precision" the new Precision after rounding.</param>
procedure ZRoundBCD(var Value: TBCD; Scale: TZBCDScale; Out Precision: Word);

var
  /// <summary>defines a lookup table for Byte to Nibble conversions.</summary>
  ZBase100Byte2BcdNibbleLookup: array[0..99] of Byte;
  /// <summary>defines a lookup table for Nibble to Byte conversions.</summary>
  ZBcdNibble2Base100ByteLookup: array[0..153] of Byte;

const
  /// <summary>defines a local copy of current FormatSettings with '.' as
  ///  DecimalSeparator and empty other fields</summary>
  FmtSettFloatDot: TFormatSettings = ( DecimalSeparator: {%H-}'.' );
  /// <summary>defines a millisecond multiply table.</summary>
  MSecMulTable: array[1..3] of Word = (100,10,1);
  /// <summary>defines a fraction to nanosecond multiply table.</summary>
  FractionLength2NanoSecondMulTable: array[0..9] of Cardinal = (
    0,
    100000000,
    10000000,
    1000000,
    100000,
    10000,
    1000,
    100,
    10,
    1);

  NanoSecsPerMSec: Cardinal = 1000000;
  MicroSecsPerSec: Cardinal = 1000000;
  MSecsOfSecond = 1000;
  MicroSecsOfMilliSecond = 1000;
  MSecsOfMinute = 60 * MSecsOfSecond;
  MSecsOfHour = 60 * MSecsOfMinute;
  MSecsOfDay = MSecsOfHour * 24;

  cPascalIntegralDatePart: TZDate = (Year: 1899; Month: 12; Day: 30; IsNegative: False);

  cMaxDateLen = 1{neg sign}+5{high word}+1{delim}+2{month}+1{delim}+2{day}+1{T};
  cMaxDateLenQuoted = cMaxDateLen+2;
  cMaxTimeLen = 1{T}+1{neg sign}+2{hour}+1{delim}+2{minute}+1{delim}+2{second}+1{dot}+9{nano fractions};
  cMaxTimeLenQuoted = cMaxTimeLen +2;
  cMaxTimeStampLen = cMaxDateLen+cMaxTimeLen{Z is replace by dbl T delim }+6{-/+)14:45};
  cMaxTimeStampLenQuoted = cMaxTimeStampLen+2;
implementation

uses DateUtils, Math,
  {$IF defined(WITH_RTLCONSTS_SInvalidGuidArray) or defined(TLIST_IS_DEPRECATED)}RTLConsts,{$IFEND}
  SysConst,{keep it after RTLConst -> deprecated warning}
  {$IFDEF WITH_DBCONSTS}DBConsts,{$ENDIF}
  ZFastCode;

const
  u4Zeros: UnicodeString = '0000';
{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  r8Zeros: RawByteString = '00000000';
{$ENDIF}
var
  ZBcdNibble2DwoDigitLookupW:   array[0..153] of Word;
  ZBcdNibble2DwoDigitLookupLW:  array[0..153] of Cardinal;
{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  r8Zeros: RawByteString;
{$ENDIF}

function FirstDelimiter(const Delimiters, Str: string): Integer;
var P: PChar absolute Str;
  PStart, PEnd, PDStart, PDEnd: PChar;
begin
  Result := 0;
  PDStart := Pointer(Delimiters);
  if PDStart = nil then Exit;
  PDEnd := PDStart+Length(Delimiters);
  PStart := P;
  PEnd := PStart+Length(Str);
  while (PStart < PEnd) do begin
    while PDStart < PDEnd do
      if PStart^ = PDStart^ then begin
        Result := PStart-P+1;
        Exit;
      end else
        Inc(PDStart);
    PDStart := Pointer(Delimiters);
    Inc(PStart);
  end;
end;

function LastDelimiter(const Delimiters, Str: string): Integer;
var P: PChar absolute Str;
  PStart, PEnd, PDStart, PDEnd: PChar;
begin
  Result := 0;
  PDStart := Pointer(Delimiters);
  if PDStart = nil then Exit;
  PDEnd := PDStart+Length(Delimiters);
  PStart := P;
  PEnd := PStart+Length(Str)-1;
  while (PEnd >= PStart) do begin
    while PDStart < PDEnd do
      if PEnd^ = PDStart^ then begin
        Result := PEnd-P+1;
        Exit;
      end else
        Inc(PDStart);
    PDStart := Pointer(Delimiters);
    Dec(PEnd);
  end;
end;

function ZMemLComp(P1, P2: PAnsiChar; Len: Cardinal): Integer;
Label Fail;
var
  PEnd: PAnsiChar;
begin
  Result := 0;
  PEnd := P1 + Len;
  while P1+32 < PEnd do //compare 32 Bytes per loop
  begin
    {$IFDEF CPUX64} //32Bit targets a less optimal comparing 8Byte values
    if (PUInt64(P1)^ <> PUInt64(P2)^) then goto Fail;
    if (PUInt64(P1+8)^ <> PUInt64(P2+8)^) then goto Fail;
    if (PUInt64(P1+16)^ <> PUInt64(P2+16)^) then goto Fail;
    if (PUInt64(P1+24)^ <> PUInt64(P2+24)^) then goto Fail;
    {$ELSE}
    if (PCardinal(P1)^ <> PCardinal(P2)^) then goto Fail;
    if (PCardinal(P1+4)^ <> PCardinal(P2+4)^) then goto Fail;
    if (PCardinal(P1+8)^ <> PCardinal(P2+8)^) then goto Fail;
    if (PCardinal(P1+12)^ <> PCardinal(P2+12)^) then goto Fail;
    if (PCardinal(P1+16)^ <> PCardinal(P2+16)^) then goto Fail;
    if (PCardinal(P1+20)^ <> PCardinal(P2+20)^) then goto Fail;
    if (PCardinal(P1+24)^ <> PCardinal(P2+24)^) then goto Fail;
    if (PCardinal(P1+28)^ <> PCardinal(P2+28)^) then goto Fail;
    {$ENDIF}
    Inc(P1, 32); Inc(P2, 32);
  end;
  while P1+8 < PEnd do //compare 8 Bytes per loop
  begin
    {$IFDEF CPUX64}
    if (PUInt64(P1)^ <> PUInt64(P2)^) then goto Fail; //not overflow save so let's check the bytes
    {$ELSE}
    if (PCardinal(P1)^ <> PCardinal(P2)^) then goto Fail;
    if (PCardinal(P1+4)^ <> PCardinal(P2+4)^) then goto Fail;
    {$ENDIF}
    Inc(P1, 8); Inc(P2, 8);
  end;
Fail:
  while P1 < PEnd do
  begin
    Result := PByte(P1)^ - PByte(P2)^; //overflow save
    if Result = 0 then
    begin
      Inc(P1); Inc(P2);
    end
    else
      Exit;
  end;
end;

function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(Pointer(P1), Pointer(P2), Len shl 1) = 0;
end;

function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(P1, P2, Len) = 0;
end;

function StartsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
begin
  if SubStr = EmptyRaw
  then Result := True
  else begin
    LenSubStr := Length(SubStr);
    if LenSubStr <= Length(Str)
    then Result := MemLCompAnsi(PAnsiChar(Str), PAnsiChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

function StartsWith(const Str, SubStr: UnicodeString): Boolean;
var
  LenSubStr: Integer;
begin
  if SubStr = ''
  then Result := True
  else begin
    LenSubStr := Length(SubStr);
    if LenSubStr <= Length(Str)
    then Result := MemLCompUnicode(PWideChar(Str), PWideChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

function EndsWith(const Str, SubStr: UnicodeString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = ''
  then Result := False // act like Delphi's AnsiEndsStr()
  else begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr
    then Result := MemLCompUnicode(PWideChar(Str) + LenStr - LenSubStr,
         PWidechar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

function EndsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = EmptyRaw
  then Result := False // act like Delphi's AnsiEndsStr()
  else begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr
    then Result := MemLCompAnsi(PAnsiChar(Str) + LenStr - LenSubStr,
         PAnsiChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  Len: NativeUInt = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result, Len);
end;

function CurrToRawBuff(Value: PAnsiChar; Buf: PByteArray; Len: NativeUInt): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
  if Len = 0 then goto fail;
  Result := True;
  DotPos := 0; CommaPos := 0; ValidCount := 0; InvalidPos := 0;
  FillChar(Buf^, Len+1, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  for i := 0 to Len-1 do
    case Ord((Value+i)^) of
      Ord('0')..Ord('9'):
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      Ord(','):
        if ((I-InvalidPos-DotPos) = 3) or ((DotPos=0) and (ValidCount > 0)) then //all others are invalid!
        begin
          CommaPos := I;
          if DotPos = 0 then
            Inc(ValidCount)
          else //align result four Byte block and overwrite last ThousandSeparator
            PCardinal(@Buf[DotPos-1])^ := PCardinal(@Buf[DotPos])^;
          Buf[ValidCount-1] := Ord('.');
        end
        else
          Goto Fail;
      Ord('-'), Ord('+'):
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      Ord('.'):
        begin
          if DotPos > 0 then //previously init so commapos can't be an issue here
          begin
            if (I-InvalidPos-DotPos) = 3 then //all others are invalid!
            begin
              PCardinal(@Buf[DotPos-1])^ := PCardinal(@Buf[DotPos])^;
              Buf[ValidCount-1] := Ord('.');
              Inc(InvalidPos);
            end
            else
              Goto Fail;
          end
          else
            if I < CommaPos then
              Goto Fail
            else
            begin
              Buf[ValidCount] := Ord('.');
              Inc(ValidCount);
            end;
          DotPos := ValidCount;
        end;
      else
        if (ValidCount > 0) then
          if Ord((Value+i)^) = Ord(' ') then //641,22 $ f.e.
            Break
          else
            Goto Fail
        else
          InvalidPos := i+1;
    end;
  Exit;
Fail:
  Result := False;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  out Result: Extended; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then begin
    Result := ValRawExt(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        Trim(Len, Value);
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len)
        then RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency;
  out Result: Currency; Len: NativeUInt);
var InvalidPos: Integer;
  C: Cardinal absolute InvalidPos;
begin
  Result := Def;
  if Assigned(Value) then begin
    if Len = 0 then
      Len := ZFastCode.StrLen(Value);
    InvalidPos := Len;
    Result := ValRawCurr(PByteArray(Value), '.', InvalidPos);
    if C = Len then Exit;
    if C < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        C := Len;
        Result := ValRawCurr(PByteArray(Value), ',', InvalidPos);
        if C = Len then
          Exit;
      end
    end;
    if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
  end;
  Result := Def;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: NativeUInt = 0); overload;
var InvalidPos: Integer;
  C: Cardinal absolute InvalidPos;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      Len := ZFastCode.StrLen(Value);
    if DecimalSep = #0 then
      DecimalSep := '.';
    InvalidPos := Len;
    Result := ValRawCurr(PByteArray(Value), DecimalSep, InvalidPos);
    if C = Len then Exit;
    if C < Len then begin //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) in [Ord('.'), Ord(',')]) and (Ord((Value+InvalidPos-1)^) <> Ord(DecimalSep)) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        C := Len;
        if DecimalSep = '.'
        then DecimalSep := ','
        else DecimalSep := '.';
        Result := ValRawCurr(PByteArray(Value), DecimalSep, InvalidPos);
        if C = Len then
          Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double;
  out Result: Double; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawDbl(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else begin
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        Trim(Len, Value);
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single;
  out Result: Single; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawSin(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else begin
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        Trim(Len, Value);
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len)
        then RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else Result := Def;
      end;
  end;
end;

function SQLStrToFloatDef(Value: PWideChar; const Def: Extended;
  Len: NativeUInt = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result, Len);
end;

function CurrToUnicodeBuf(Value: PWideChar; Buffer: PWordArray; CodePoints: Integer): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
  if CodePoints = 0 then goto Fail;
  Result := True;
  DotPos := 0; CommaPos := 0; ValidCount := 0; InvalidPos := 0;
  FillChar(Pointer(Buffer)^, (CodePoints+1) shl 1, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  for i := 0 to CodePoints-1 do
    case (Value+i)^ of
      '0'..'9':
        begin
          Buffer[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      ',':
        if ((I-InvalidPos-DotPos) = 3) or ((DotPos=0) and (ValidCount > 0)) then //all others are invalid!
        begin
          CommaPos := I;
          if DotPos = 0 then
            Inc(ValidCount)
          else //align result eight Byte block and overwrite last ThousandSeparator
          begin
            PCardinal(@Buffer[DotPos-1])^ := PCardinal(@Buffer[DotPos])^; //Move first four byte block
            PCardinal(@Buffer[DotPos+1])^ := PCardinal(@Buffer[DotPos+2])^; //Move second four byte block
          end;
          Buffer[ValidCount-1] := Ord('.');
        end
        else
          goto Fail;
      '-', '+':
        begin
          Buffer[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      '.':
        begin
          if DotPos > 0 then //previously init so commapos can't be an issue here
            if (I-InvalidPos-DotPos) = 3 then //all others are invalid!
            begin
              PCardinal(@Buffer[DotPos-1])^ := PCardinal(@Buffer[DotPos])^; //Move first four byte block
              PCardinal(@Buffer[DotPos+1])^ := PCardinal(@Buffer[DotPos+2])^; //Move second four byte block
              Buffer[ValidCount-1] := Ord('.');
              Inc(InvalidPos);
            end
            else
              goto Fail
          else
            if I < CommaPos then
              goto Fail
            else
            begin
              Buffer[ValidCount] := Ord('.');
              Inc(ValidCount);
            end;
          DotPos := ValidCount;
        end;
      else
        if (ValidCount > 0) then
          if (Value+i)^ = ' ' then //641,22 $ f.e. (PostgreSQL)
            Break
          else
            goto Fail
        else
          InvalidPos := i+1;
    end;
  Exit;
Fail:
  Result := False;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Extended;
  out Result: Extended; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Word;
  PBuf: PWordArray;
  C: Cardinal absolute InvalidPos;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeExt(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value)
          {$ELSE}
          Len := Length(Value)
          {$ENDIF}
        else
          if (Len < C) and ((Value+InvalidPos-1)^ = ' ') then Exit;//fixed width str
        Trim(Len, Value);
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency;
  out Result: Currency; Len: NativeUInt);
var InvalidPos: Integer;
  C: Cardinal absolute InvalidPos;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      {$IFDEF WITH_PWIDECHAR_STRLEN}
      Len := SysUtils.StrLen(Value);
      {$ELSE}
      Len := Length(Value);
      {$ENDIF}
    InvalidPos := Len;
    Result := ValUnicodeCurr(PWordArray(Value), '.', InvalidPos);
    if C = Len then Exit;
    if C < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        Result := ValUnicodeCurr(PWordArray(Value), ',', InvalidPos);
        if C = Len then
          Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; Var DecimalSep: Char;
  out Result: Currency; Len: NativeUInt); overload;
var InvalidPos: Integer;
  C: Cardinal absolute InvalidPos;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      {$IFDEF WITH_PWIDECHAR_STRLEN}
      Len := SysUtils.StrLen(Value);
      {$ELSE}
      Len := Length(Value);
      {$ENDIF}
    if DecimalSep = #0 then
      DecimalSep := '.';
    InvalidPos := Len;
    Result := ValUnicodeCurr(PWordArray(Value), DecimalSep, InvalidPos);
    if C = Len then Exit;
    if C < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) in [Ord('.'), Ord(',')]) and (Ord((Value+InvalidPos-1)^) <> Ord(DecimalSep)) and
         (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        if DecimalSep = '.'
        then DecimalSep := ','
        else DecimalSep := '.';
        Result := ValUnicodeCurr(PWordArray(Value), DecimalSep, InvalidPos);
        if C = Len then Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double;
  out Result: Double; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeDbl(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value);
          {$ELSE}
          Len := Length(Value);
          {$ENDIF}
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        Trim(Len, Value);
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result);
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single;
  out Result: Single; Len: NativeUInt);
var
  InvalidPos: Integer;
  StatBuf: Array[Byte] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeSin(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Byte(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value);
          {$ELSE}
          Len := Length(Value);
          {$ENDIF}
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        Result := Def;
        Trim(Len, Value);
        if Len > SizeOf(StatBuf)-1 then begin
          Result := Def;
          Exit;
        end;
        PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(Value), WideChar('.'), Def, Result);
      end;
  end;
end;

{$IFDEF UNICODE}
function BufferToStr(Buffer: PWideChar; Length: Integer): string;
{$ELSE}
function BufferToStr(Buffer: PAnsiChar; Length: Integer): string;
{$ENDIF}
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

function BufferToBytes(Buffer: Pointer; Length: Integer): TBytes;
begin
  Result := nil;
  SetLength(Result, Length);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Result)^, Length);
end;

function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PAnsiChar(Pointer(Str)), CheckInt, False);
end;

function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Ord(Str^)  or $20 of //lower
      Ord('t'): //Check mixed case of 'true' or 't' string
        if PByte(Str+1)^ = Ord(#0) then
          Result := True
        else if (PByte(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PByte(Str+1)^ or $20 = Ord('r')) and (PByte(Str+2)^ or $20 = Ord('u'))
                and (PByte(Str+3)^ or $20 = Ord('e')) then
          if PByte(Str+4)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+4)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,4);
            goto SkipSpaces;
          end;
      Ord('y'): //Check mixed case of 'Yes' or 'y' string
        if PByte(Str+1)^ = Ord(#0) then
          Result := True
        else if (PByte(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PByte(Str+1)^ or $20 = Ord('e')) and (PByte(Str+2)^ or $20 = Ord('s')) then
          if PByte(Str+3)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+3)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,3);
SkipSpaces: while PByte(Str)^ = Ord(' ') do Inc(Str);
            Result := PByte(Str)^ = Ord(#0);
          end;
      Ord('o'): //Check mixed case of 'ON' or 'on' string
        if PByte(Str+1)^ or $20 = Ord('n') then
          if PByte(Str+2)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+2)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,2);
            goto SkipSpaces;
          end;
      else
        Result := CheckInt and (RawToIntDef(Str, 0) <> 0);
    end;
end;

function StrToBoolEx(Buf, PEnd: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;
var P: PAnsiChar;
begin
  Result := False; //init
  if (Buf = nil) or (PEnd = nil) or (Buf = PEnd) then
    Exit;
  if IgnoreTrailingSaces then
    while Ord((PEnd-1)^) = Ord(' ') do
      Dec(PEnd);
  case (Ord(Buf^) or $20) of
    { test lowercase "YES" / "Y"}
    Ord('y'): if (Pend-Buf) = 1 then
                Result := True
              else if (Pend-Buf) = 3 then
                Result := (Ord((Buf+1)^) or $20 = Ord('e')) and (Ord((Buf+1)^) or $20 = Ord('s'));
    { test lowercase "ON" }
    Ord('o'): Result := (Pend-Buf = 2) and (Ord((Buf+1)^) or $20 = Ord('n'));
    { test lowercase "TRUE" / "T"}
    Ord('t'): if Pend-Buf = 1 then
                Result := True
              else Result := (Pend-Buf = 4) and (Ord((Buf+1)^) or $20 = Ord('r')) and
                (Ord((Buf+1)^) or $20 = Ord('u')) and (Ord((Buf+1)^) or $20 = Ord('e'));
    else begin
      P := PEnd;
      Result := CheckInt and (ValRawInt(Buf, P) <> 0) and (P = PEnd);
    end;
  end;
end;

function StrToBoolEx(const Str: UnicodeString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PWideChar(Pointer(Str)), CheckInt, False);
end;

function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Ord(Str^)  or $20 of //lower
      Ord('t'): //Check mixed case of 'true' or 't' string
        if PWord(Str+1)^ = Ord(#0) then
          Result := True
        else if (PWord(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PWord(Str+1)^ or $20 = Ord('r')) and (PWord(Str+2)^ or $20 = Ord('u'))
                and (PWord(Str+3)^ or $20 = Ord('e')) then
          if PWord(Str+4)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+4)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,4);
            goto SkipSpaces;
          end;
      Ord('y'): //Check mixed case of 'Yes' or 'y' string
        if PWord(Str+1)^ = Ord(#0) then
          Result := True
        else if (PWord(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PWord(Str+1)^ or $20 = Ord('e')) and (PWord(Str+2)^ or $20 = Ord('s')) then
          if PWord(Str+3)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+3)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,3);
SkipSpaces: while PWord(Str)^ = Ord(' ') do Inc(Str);
            Result := PWord(Str)^ = Ord(#0);
          end;
      Ord('o'): //Check mixed case of 'ON' or 'on' string
        if PWord(Str+1)^ or $20 = Ord('n') then
          if PWord(Str+2)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+2)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,2);
            goto SkipSpaces;
          end;
      else
        Result := CheckInt and (UnicodeToIntDef(Str, 0) <> 0);
    end;
end;

function StrToBoolEx(Buf, PEnd: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;
var P: PWideChar;
begin
  Result := False; //init
  if (Buf = nil) or (PEnd = nil) or (Buf = PEnd) then
    Exit;
  if IgnoreTrailingSaces then
    while Ord((PEnd-1)^) = Ord(' ') do
      Dec(PEnd);
  case (Ord(Buf^) or $20) of
    { test lowercase "YES" / "Y"}
    Ord('y'): if (Pend-Buf) = 1 then
                Result := True
              else if (Pend-Buf) = 3 then
                Result := (Ord((Buf+1)^) or $20 = Ord('e')) and (Ord((Buf+1)^) or $20 = Ord('s'));
    { test lowercase "ON" }
    Ord('o'): Result := (Pend-Buf = 2) and (Ord((Buf+1)^) or $20 = Ord('n'));
    { test lowercase "TRUE" / "T"}
    Ord('t'): if Pend-Buf = 1 then
                Result := True
              else Result := (Pend-Buf = 4) and (Ord((Buf+1)^) or $20 = Ord('r')) and
                (Ord((Buf+1)^) or $20 = Ord('u')) and (Ord((Buf+1)^) or $20 = Ord('e'));
    else begin
      P := PEnd;
      Result := CheckInt and (ValUnicodeInt(Buf, P) <> 0) and (P = PEnd);
    end;
  end;
end;

function BoolToUnicodeEx(Value: Boolean): UnicodeString;
begin
  Result := BoolStrsW[Value];
end;

function BoolToRawEx(Value: Boolean): RawByteString;
begin
  Result := BoolStrsRaw[Value];
end;

function BoolToStrEx(Value: Boolean): string;
begin
  Result := BoolStrs[Value];
end;

{$IFDEF ENABLE_POSTGRESQL}
function IsIpAddr(const Str: string): Boolean;
LAbel LExit;
var
  I, N: Integer;
  Splited: TStrings;
begin
  Result := False;
  Splited := SplitString(Str, '.');
  if Splited.Count <> 4 then
    goto LExit
  else
    for i := 0 to 3 do
    begin
      N := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Splited[i], -1);
      if (N < 0) or (N > 255) then
        goto LExit;
    end;
  Result := True;
  LExit: Splited.Free;
end;
{$ENDIF}

procedure SplitToStringList(List: TStrings; const AStr, Delimiters: string);
var
  PStart, PCurr, PEnd, PDelim: PChar;
  S: String;
begin
  //EH: 5x faster version of SplitToStringList
  if AStr = ''
  then Exit
  else if Delimiters = '' then begin
    List.Add(AStr);
    Exit;
  end;
  PStart := Pointer(AStr);
  PCurr := Pointer(AStr);
  PEnd := Pointer(AStr);
  Inc(PEnd, Length(AStr));
  while PCurr < PEnd do begin
    PDelim := Pointer(Delimiters);
    while PDelim^ <> #0 do
      if PDelim^ = PCurr^
      then Break
      else Inc(PDelim);
    if PDelim^ <> #0 then
      if PCurr > PStart then begin
        SetString(S, PStart, PCurr-PStart);
        List.Add(S);
        PStart := PCurr+1;
      end else
        inc(PStart, Ord(PStart^ = PDelim^));
    inc(PCurr)
  end;
  if PCurr > PStart then begin
    SetString(S, PStart, PCurr-PStart);
    List.Add(S);
  end;
end;

(*procedure SplitToStringList(List: TStrings; Str: string; const Delimiters: string);
var
  DelimPos: Integer;
  Str: string;
begin
  Str := AStr;
  repeat
    DelimPos := FirstDelimiter(Delimiters, Str);
    if DelimPos > 0 then
    begin
      if DelimPos > 1 then
        List.Add(Copy(Str, 1, DelimPos - 1));
      Str := Copy(Str, DelimPos + 1, Length(Str) - DelimPos);
      end
      else
      Break;
  until DelimPos <= 0;
  if Str <> '' then
    List.Add(Str);
end;*)

function SplitString(const Str, Delimiters: string): TStrings;
var Succeeded: Boolean;
begin
  Result := TStringList.Create;
  Succeeded := False;
  try
    SplitToStringList(Result, Str, Delimiters);
    Succeeded := True;
  finally
    if not Succeeded then
      FreeAndNil(Result);
  end;
  {except
    Result.Free;
    raise;
  end;}
end;

procedure PutSplitString(List: TStrings; const Str, Delimiters: string);
begin
  List.Clear;
  SplitToStringList(List, Str, Delimiters);
end;

procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);
begin
  SplitToStringList(List, Str, Delimiters);
end;

function ComposeString(List: TStrings; const Delimiter: string): string;
var
  i, Len, DelimLen: Integer;
  S: string;
  P: PChar;
begin
  DelimLen := Length(Delimiter);
  Len := 0;
  if List.Count > 0 then begin
    Inc(Len, Length(List[0]));
    for i := 1 to List.Count - 1 do
      Inc(Len, DelimLen + Length(List[i]));
  end;
  {$IFDEF FPC}Result := '';{$ENDIF}
  SetLength(Result, Len);
  P := Pointer(Result);
  for i := 0 to List.Count - 1 do begin
    if (i > 0) and (DelimLen > 0) then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Delimiter)^, P^, DelimLen * SizeOf(Char));
      Inc(P, DelimLen);
    end;
    S := List[i];
    Len := Length(S);
    if Len > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(S)^, P^, Len * SizeOf(Char));
      Inc(P, Len);
    end;
  end;
end;

function FloatToSQLStr(Value: Extended): string;
begin
  Result := FloatToStr(Value, FmtSettFloatDot);
end;

function SQLStrToFloat(const Str: String): Extended;
begin
  Result := StrToFloat(Str, FmtSettFloatDot);
end;

procedure SplitToStringListEx(List: TStrings; const Str, Delimiter: string);
var P: PChar absolute Str;
  temp: string;
  i, OffSet: integer;
  PD: PChar;
  L, LD: LengthInt;
begin
  if Str = '' then Exit;
  L := Length(Str);
  PD := Pointer(Delimiter);
  if PD = nil then begin
    List.Add(Str);
    Exit;
  end;
  LD := Length(Delimiter);
  OffSet := 1;
  I := ZFastCode.PosEx(PD, P, LD, L, OffSet);
  while I > 0 do begin
    SetString(temp, (P+OffSet-1), (i-OffSet));
    if (temp <> '') or (List.Count > 0) then
      List.Add(temp);
    OffSet := I+LD;
    I := ZFastCode.PosEx(PD, P, LD, L, OffSet);
  end;
  if OffSet < L then
    if OffSet = 1
    then List.Add(Str)
    else begin
      SetString(temp, (P+OffSet-1), (L-OffSet+1));
      List.Add(temp);
    end;
end;

procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  List.Clear;
  SplitToStringListEx(List, Str, Delimiter);
end;

function SplitStringEx(const Str, Delimiter: string): TStrings;
var Succeeded: Boolean;
begin
  Result := TStringList.Create;
  Succeeded := False;
  try
    SplitToStringListEx(Result, Str, Delimiter);
    Succeeded := True;
  finally
    if not Succeeded then
      FreeAndNil(Result);
  end;
  {except
    Result.Free;
    raise;
  end;}
end;

procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  SplitToStringListEx(List, Str, Delimiter);
end;

function BytesToStr(const Value: TBytes): RawByteString;
{$IF defined(MISS_RBS_SETSTRING_OVERLOAD) and not defined(WITH_TBYTES_AS_RAWBYTESTRING)}
var L: Integer;
{$IFEND}
begin
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  Result := Value;
  {$ELSE}
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := EmptyRaw;
    L := Length(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
    {$ELSE}
    SetString(Result, PAnsiChar(@Value[0]), Length(Value))
    {$ENDIF}
  {$ENDIF}
end;

function StrToBytes(const Value: RawByteString): TBytes;
var L: Integer;
begin
  L := Length(Value);
  {$IFDEF FPC} Result := nil;{$ENDIF}
  SetLength(Result, L);
  if Value <> EmptyRaw then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operant..." marked as inline is not inlined}{$ENDIF}
function BytesToVar(const Value: TBytes): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  for I := 0 to Length(Value) - 1 do
    Result[I] := Value[I];
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator := (const Source: Byte): Variant;" marked as inline is not inlined}{$ENDIF}
function BytesToVar(const Value: RawByteString): Variant;
var
  I: Integer;
  P: PByte;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  P := Pointer(Value);
  for I := 0 to Length(Value) - 1 do begin
    Result[I] := P^;
    Inc(P);
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}
{$ENDIF WITH_TBYTES_AS_RAWBYTESTRING}

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operant..." marked as inline is not inlined}{$ENDIF}
function VarToBytes(const Value: Variant): TBytes;
var
  I: Integer;
begin
  {$IFDEF FPC}Result := nil;{$ENDIF}
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create(SInvalidVarByteArray);

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function TryRawToDate(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var Date: TZDate): Boolean;
var VEnd: PAnsiChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, Fmt;
begin
  Result := False;
  PF := Pointer(Format);
  PInt64(@Date.Year)^ := 0;
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  while (PF < FEnd) and (Value < VEnd) do begin
    B := PByte(Value)^ or $20;
Fmt:F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('y'):  Date.Year := Date.Year * 10 + B;
            Byte('m'):  Date.Month := Date.Month * 10 + B;
            Byte('d'):  Date.Day := Date.Day * 10 + B;
            else if (F in [Byte('-'),Byte('/'),Byte('\'),Byte(' ')]) then begin
              Dec(PF); //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
              goto Fmt;
            end else begin
              Inc(PF);
              Continue;
            end;
          end;
          goto Next;
        end;
      Byte('+'),
      Byte('-'):  if F = Byte('y') then begin
                    Date.IsNegative := PWord(Value)^ = Byte('-');
                    Inc(Value);
                  end else if (B = F) or (F in [Ord('/'),Ord('\'),Ord(' ')])
                    then goto Next
                    else Exit;
      else if (B = F) or ((B = Byte('t')) and (Value+1 = VEnd)) then begin //delimiter?
Next:   Inc(Value);
        Inc(PF);
      end else if (B in [Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
        Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
      end else
        Exit;
    end;
  end;
  Result := True;
end;

function TryUniToDate(Value: PWideChar; Len: Cardinal;
  const Format: String; var Date: TZDate): Boolean;
var VEnd: PWideChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, IncF, Fmt;
begin
  Result := False;
  PF := Pointer(Format);
  PInt64(@Date.Year)^ := 0;
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  while (PF < FEnd) and (Value < VEnd) do begin
    if PWord(Value)^ > High(Byte) then Exit;
    B := Byte(PWord(Value)^ or $0020);
Fmt:F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('y'):  Date.Year := Date.Year * 10 + B;
            Byte('m'):  Date.Month := Date.Month * 10 + B;
            Byte('d'):  Date.Day := Date.Day * 10 + B;
            else if (F in [Byte('-'),Byte('/'),Byte('\'),Byte(' ')]) then begin
                Dec(PF); //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
                goto Fmt;
              end else
                goto IncF;
          end;
          goto Next;
        end;
      Byte('+'),
      Byte('-'):  if F = Byte('y') then begin
                    Date.IsNegative := PWord(Value)^ = Byte('-');
                    Inc(Value);
                  end else if (B = F) or (F in [Ord('/'),Ord('\'),Ord(' ')])
                    then goto Next
                    else Exit;
      else if (B = F) or ((B = Byte('t')) and (Value+1 = VEnd)) then begin //delimiter?
Next:   Inc(Value);
IncF:   Inc(PF);
      end else if (Byte(B) in [Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
        Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
      end else
        Exit;
    end;
  end;
  Result := True;
end;

function TryRawToTime(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var Time: TZTime): Boolean;
var VEnd, PFDot: PAnsiChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, zFlush, jmpFrac;
begin
  Result := False;
  PF := Pointer(Format);
  PCardinal(@Time.Hour)^ := 0;
  PInt64(@Time.Second)^ := 0;
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  Len := 0;
  PFDot := nil;
  while (PF < FEnd) and (Value < VEnd) do begin
    B := PByte(Value)^;
    F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('h'):  Time.Hour := Time.Hour * 10 + B;
            Byte('m'),
            Byte('n'):  Time.Minute := Time.Minute * 10 + B;
            Byte('s'):  Time.Second := Time.Second * 10 + B;
            Byte('.'):  begin
                          if PFDot = nil then begin
                            PFDot := Value+1;
                            while PFDot < VEnd do
                              if PByte(PFDot)^ = Byte('.')
                              then Break
                              else Inc(PFDot);
                            if PFDot = VEnd then
                              PFDot := Value;
                          end;
                          if PFDot > Value then begin
                            Dec(PF); //we still have a number of a second
                            Continue;
                          end else goto jmpFrac
                        end;
            Byte('f'),
            Byte('z'):  begin //fractions start here(delphi logic) -> is this correct? ISO uses the '.' indicator and Z represents the timezone!
jmpFrac:        Time.Fractions := Time.Fractions * 10 + B;
                inc(Len);
                if ((Value+1) < VEnd) and (PByte(Value+1)^ in [Byte('0')..Byte('9')]) then begin
                  Inc(Value);  {if a server returns micro or nanoseconds and the format with the z indicators is to short ...}
                  Continue;
                end else
                  goto zFlush;
              end;
            else begin
              if (F in [Byte(':'),Byte('-'),Byte('/'),Byte('\'),Byte(' '), Byte('t')])
              then Dec(PF) //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
              else Inc(PF);
              Continue;
            end;
          end;
          goto Next;
        end;
      Byte('+'),
      Byte('-'): if F = Byte('h') then begin
                  Time.IsNegative := B = Byte('-');
                  Inc(Value);
                end else if B = {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^
                  then goto Next
                  else Exit;
      Byte('.'): if (PF = FEnd -1)
                then Inc(Value)
                else if F = Byte('.')
                  then goto next
                  else goto zFlush;
      else if (B = F) or ((B or $20 = Byte('t')) and (F = Byte(' '))) or
            ((B = Byte(' ')) and (F = Byte('t'))) then begin //delimiter?
Next:   Inc(Value);
        Inc(PF);
      end else if (PByte(Value)^ in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
zFlush: Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
      end else
        Exit;
    end;
  end;
  if (Len > 0) and (Len < 9) then
    Time.Fractions := Time.Fractions * FractionLength2NanoSecondMulTable[Len];
  Result := True;
end;

function TryUniToTime(Value: PWideChar; Len: Cardinal;
  const Format: String; var Time: TZTime): Boolean;
var VEnd, PFDot: PWideChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, zFlush, jmpFrac;
begin
  Result := False;
  PF := Pointer(Format);
  PCardinal(@Time.Hour)^ := 0;
  PInt64(@Time.Second)^ := 0;
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  Len := 0;
  PFDot := nil;
  while (PF < FEnd) and (Value < VEnd) do begin
    if PWord(Value)^ > High(Byte) then Exit;
    B := Byte(PWord(Value)^);
    F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('h'):  Time.Hour := Time.Hour * 10 + B;
            Byte('m'),
            Byte('n'):  Time.Minute := Time.Minute * 10 + B;
            Byte('s'):  Time.Second := Time.Second * 10 + B;
            Byte('.'):  begin
                          if PFDot = nil then begin
                            PFDot := Value+1;
                            while PFDot < VEnd do
                              if PByte(PFDot)^ = Byte('.')
                              then Break
                              else Inc(PFDot);
                            if PFDot = VEnd then
                              PFDot := Value;
                          end;
                          if PFDot > Value then begin
                            Dec(PF); //we still have a number of a second
                            Continue;
                          end else goto jmpFrac
                        end;
            Byte('f'),
            Byte('z'):  begin //fractions start here(delphi logic) -> is this correct? ISO uses the '.' indicator and Z represents the timezone!
jmpFrac:        Time.Fractions := Time.Fractions * 10 + B;
                inc(Len);
                if ((Value+1) < VEnd) and (PByte(Value+1)^ in [Byte('0')..Byte('9')]) then begin
                  Inc(Value);  {if a server returns micro or nanoseconds and the format with the z indicators is to short ...}
                  Continue;
                end else
                  goto zFlush;
              end;
            else begin
              if (F in [Byte(':'),Byte('-'),Byte('/'),Byte('\'),Byte(' '), Byte('t')])
              then Dec(PF) //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
              else Inc(PF);
              Continue;
            end;
          end;
          goto Next;
        end;
      Byte('+'),
      Byte('-'): if F = Byte('h') then begin
                  Time.IsNegative := B = Byte('-');
                  Inc(Value);
                end else if B = {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^
                  then goto Next
                  else Exit;
      Byte('.'): if (PF = FEnd -1)
                then Inc(Value)
                else if F = Byte('.')
                  then goto next
                  else goto zFlush;
      else if (B = F) or ((B or $20 = Byte('t')) and (F = Byte(' '))) or
            ((B = Byte(' ')) and (F = Byte('t'))) then begin //delimiter?
Next:   Inc(Value);
        Inc(PF);
      end else if (PByte(Value)^ in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
zFlush: Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
      end else
        Exit;
    end;
  end;
  if (Len > 0) and (Len < 9) then
    Time.Fractions := Time.Fractions * FractionLength2NanoSecondMulTable[Len];
  Result := True;
end;

function TryRawToTimeStamp(Value: PAnsiChar; Len: Cardinal;
  const Format: String; var TimeStamp: TZTimeStamp): Boolean;
var VEnd{End of value +1}, PTZ{remander of TimeZone sign},
  PFDot{Dot position of Fraction part}: PAnsiChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, zFlush, TimeZ, jmpFrac;
begin
  Result := False;
  PF := Pointer(Format);
  FillChar(TimeStamp, SizeOf(TZTimeStamp), #0);
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  Len := 0;
  PFDot := nil;
  while (PF < FEnd) and (Value < VEnd) do begin
    B := PByte(Value)^;
    F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('y'):  TimeStamp.Year := TimeStamp.Year * 10 + B;
            Byte('m'):  TimeStamp.Month := TimeStamp.Month * 10 + B;
            Byte('d'):  TimeStamp.Day := TimeStamp.Day * 10 + B;
            Byte('h'):  TimeStamp.Hour := TimeStamp.Hour * 10 + B;
            Byte('n'):  TimeStamp.Minute := TimeStamp.Minute * 10 + B;
            Byte('s'):  TimeStamp.Second := TimeStamp.Second * 10 + B;
            Byte('.'):  begin
                          if PFDot = nil then begin
                            PFDot := Value+1;
                            while PFDot < VEnd do
                              if PByte(PFDot)^ = Byte('.')
                              then Break
                              else Inc(PFDot);
                            if PFDot = VEnd then
                              PFDot := Value;
                          end;
                          if PFDot > Value then begin
                            Dec(PF); //we still have a number of a second
                            Continue;
                          end else goto jmpFrac
                        end;
            Byte('z'):  begin //fractions start here(delphi logic) -> is this correct? ISO uses the '.' indicator and Z represents the timezone!
jmpFrac:        TimeStamp.Fractions := TimeStamp.Fractions * 10 + B;
                inc(Len);
                if ((Value+1) < VEnd) and (PByte(Value+1)^ in [Byte('0')..Byte('9')]) then begin
                  Inc(Value);  {if a server returns micro or nanoseconds and the format with the z indicators is to short ...}
                  Continue;
                end else
                  goto zFlush;
              end;
            Byte('p'):  begin
                TimeStamp.TimeZoneHour := B;
                goto TimeZ;
              end;
            else begin
              if (F in [Byte(':'),Byte('-'),Byte('/'),Byte('\'),Byte(' '), Byte('t')])
              then Dec(PF) //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
              else Inc(PF);
              Continue;
            end;
          end;
          goto Next;
        end;
      Byte('+'), Byte('-'): begin
          case F of
            Byte('y'): TimeStamp.IsNegative := B = Byte('-');
            Byte('p'): begin
TimeZ:          PTZ := Value;
                Inc(Value);
                while (Value < VEnd) do begin
                  B := PByte(Value)^;
                  if (B in [Byte('0')..Byte('9')]) then begin
                    TimeStamp.TimeZoneHour := TimeStamp.TimeZoneHour * 10 + (B - Byte('0'));
                    Inc(Value);
                  end else if B = Byte(':') then begin
                    if ((Value+1) < VEnd) then
                      case PByte(Value+1)^ of
                        Byte('1'): {not possible} TimeStamp.TimeZoneMinute := 15;
                        Byte('3'): TimeStamp.TimeZoneMinute := 30;
                        Byte('4'): TimeStamp.TimeZoneMinute := 45;
                        else Break;
                      end
                    else Exit;
                    Inc(Value, 2);
                  end;
                end;
                if {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PTZ)^ = Byte('-') then
                  TimeStamp.TimeZoneHour := -TimeStamp.TimeZoneHour;
              end
            else if (B = F) or (F in [Ord('/'),Ord('\'),Ord(' ')])
            then goto Next
            else Exit;
          end;
          Inc(Value);
        end;
      Byte('.'): if (PF = FEnd -1) or ({$IFDEF UNICODE}PWord(PF+1)^ or $0020{$ELSE}PByte(PF+1)^ or $20{$ENDIF} = Ord('p'))
                then Inc(Value)
                else if F = Byte('.')
                  then goto next
                  else goto zFlush;
      else if (B = F) or ((B or $20 = Byte('t')) and (F = Byte(' '))) or
            ((B = Byte(' ')) and (F = Byte('t'))) then begin //delimiter?
Next:   Inc(Value);
        Inc(PF);
      end else if (B in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
zFlush: Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
        F := {$IFDEF UNICODE}Byte(PWord(PF)^ or $0020){$ELSE}PByte(PF)^ or $20{$ENDIF};
        B := PByte(Value-1)^ or $20;
        if (F in [Byte(' '), Byte('t')]) and (B in [Byte(' '), Byte('t')]) then
          Exit;
      end else
        Exit;
    end;
  end;
  if (Len > 0) and (Len < 9) then
    TimeStamp.Fractions := TimeStamp.Fractions * FractionLength2NanoSecondMulTable[Len];
  Result := True;
end;

function TryUniToTimeStamp(Value: PWideChar; Len: Cardinal;
  const Format: String; var TimeStamp: TZTimeStamp): Boolean;
var VEnd{End of value +1}, PTZ{remander of TimeZone sign},
  PFDot{Dot position of Fraction part}: PWideChar;
  PF, FEnd: PChar;
  F, B: Byte;
label Next, zFlush, TimeZ, jmpFrac;
begin
  Result := False;
  PF := Pointer(Format);
  FillChar(TimeStamp, SizeOf(TZTimeStamp), #0);
  if (PF = nil) or (Value = nil) or (Len=0) then Exit;
  FEnd := PF+Length(Format);
  VEnd := Value + Len;
  Len := 0;
  PFDot := nil;
  while (PF < FEnd) and (Value < VEnd) do begin
    if PWord(Value)^ > High(Byte) then Exit;
    B := Byte(PWord(Value)^ or $0020);
    F := {$IFDEF UNICODE}PWord(PF)^ or $0020{$ELSE}PByte(PF)^ or $20{$ENDIF};
    case B of
      Byte('0')..Byte('9'): begin
          B := B - Byte('0');
          case Byte(F) of //lower() of
            Byte('y'):  TimeStamp.Year := TimeStamp.Year * 10 + B;
            Byte('m'):  TimeStamp.Month := TimeStamp.Month * 10 + B;
            Byte('d'):  TimeStamp.Day := TimeStamp.Day * 10 + B;
            Byte('h'):  TimeStamp.Hour := TimeStamp.Hour * 10 + B;
            Byte('n'):  TimeStamp.Minute := TimeStamp.Minute * 10 + B;
            Byte('s'):  TimeStamp.Second := TimeStamp.Second * 10 + B;
            Byte('.'):  begin
                          if PFDot = nil then begin
                            PFDot := Value+1;
                            while PFDot < VEnd do
                              if PWord(PFDot)^ = Word('.')
                              then Break
                              else Inc(PFDot);
                            if PFDot = VEnd then
                              PFDot := Value;
                          end;
                          if PFDot > Value then begin
                            Dec(PF); //we still have a number of a second
                            Continue;
                          end else goto jmpFrac
                        end;
            Byte('z'):  begin //fractions start here(delphi logic) -> is this correct? ISO uses the '.' indicator and Z represents the timezone!
jmpFrac:        TimeStamp.Fractions := TimeStamp.Fractions * 10 + B;
                inc(Len);
                if ((Value+1) < VEnd) and (PWord(Value+1)^<High(Byte)) and (Byte(PWord(Value+1)^) in [Byte('0')..Byte('9')]) then begin
                  Inc(Value);  {if a server returns micro or nanoseconds and the format with the z indicators is to short ...}
                  Continue;
                end else
                  goto zFlush;
              end;
            Byte('p'):  begin
                TimeStamp.TimeZoneHour := B;
                goto TimeZ;
              end;
            else begin
              if (F in [Byte(':'),Byte('-'),Byte('/'),Byte('\'),Byte(' '), Byte('t')])
              then Dec(PF) //we have a numeric value found. As long we have delimiters we sort the num back to the last format char
              else Inc(PF);
              Continue;
            end;
          end;
          goto Next;
        end;
      Byte('+'), Byte('-'): begin
          case F of
            Byte('y'): TimeStamp.IsNegative := PWord(Value)^ = Byte('-');
            Byte('p'): begin
TimeZ:          PTZ := Value;
                Inc(Value);
                while (Value < VEnd) do begin
                  if PWord(Value)^ > High(Byte) then Exit;
                  B := Byte(PWord(Value)^);
                  if (B in [Byte('0')..Byte('9')]) then begin
                    TimeStamp.TimeZoneHour := TimeStamp.TimeZoneHour * 10 + (B - Byte('0'));
                    Inc(Value);
                  end else if B = Byte(':') then begin
                    if ((Value+1) < VEnd) and (PWord(Value+1)^ <= High(Byte)) then
                      case Byte(PWord(Value+1)^) of
                        Byte('1'): {not possible} TimeStamp.TimeZoneMinute := 15;
                        Byte('3'): TimeStamp.TimeZoneMinute := 30;
                        Byte('4'): TimeStamp.TimeZoneMinute := 45;
                        else Break;
                      end
                    else Exit;
                    Inc(Value, 2);
                  end;
                  if {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PTZ)^ = Ord('-') then
                    TimeStamp.TimeZoneHour := -TimeStamp.TimeZoneHour;
                end
              end
            else if (B = F) or (F in [Ord('/'),Ord('\'),Ord(' ')])
            then goto Next
            else Exit;
          end;
          Inc(Value);
        end;
      Byte('.'): if (PF = FEnd -1) or ({$IFDEF UNICODE}PWord(PF+1)^ or $0020{$ELSE}PByte(PF+1)^ or $20{$ENDIF} = Ord('p'))
                then Inc(Value)
                else if F = Byte('.')
                  then goto next
                  else goto zFlush;
      else if (B = F) or
          ((B = Byte('t')) and (F = Byte(' '))) or
          ((B = Byte(' ')) and (F = Ord('t'))) then begin //delimiter?
Next:   Inc(Value);
        Inc(PF);
      end else if (B in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' ')]) then begin
zFlush: Inc(Value);
        if Value = VEnd then Break;
        B := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^;
        while ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PF)^ = B) and (PF < FEnd) do Inc(PF);
        F := {$IFDEF UNICODE}Byte(PWord(PF)^ or $0020){$ELSE}PByte(PF)^ or $20{$ENDIF};
        B := Byte(PWord(Value-1)^ or $0020);
        if (F in [Byte(' '), Byte('t')]) and (B in [Byte(' '), Byte('t')]) then
          Exit;
      end else
        Exit;
    end;
  end;
  if (Len > 0) and (Len < 9) then
    TimeStamp.Fractions := TimeStamp.Fractions * FractionLength2NanoSecondMulTable[Len];
  Result := True;
end;

function TryDateToDateTime(const Value: TZDate; var DT: TDateTime): Boolean;
begin
  Result := not Value.IsNegative and TryEncodeDate(Value.Year, Value.Month, Value.Day, DT);
  if not Result then
    DT := 0;
end;

function TryTimeToDateTime(const Value: TZTime; var DT: TDateTime): Boolean;
begin
  Result := not Value.IsNegative and TryEncodeTime(Value.Hour, Value.Minute, Value.Second, Value.Fractions div NanoSecsPerMSec, DT);
  if not Result then
    DT := 0;
end;

function TryTimeStampToDateTime(const Value: TZTimeStamp; var DT: TDateTime): Boolean;
var d: TDatetime;
begin
  if not Value.IsNegative then begin
    if TryEncodeDate(Value.Year, Value.Month, Value.Day, d) then begin
      Result := True;
      if TryEncodeTime(Value.Hour, Value.Minute, Value.Second, Value.Fractions div NanoSecsPerMSec, DT) then begin
        if d < 0
        then DT := D - DT
        else DT := D + DT
      end else
        DT := D;
    end else
      Result := TryEncodeTime(Value.Hour, Value.Minute, Value.Second, Value.Fractions div NanoSecsPerMSec, DT);
  end else
    Result := False;
  if not Result then
    DT := 0;
end;

procedure DecodeDateTimeToDate(const Value: TDateTime; var Date: TZDate);
begin
  DecodeDate(Value, Date.Year, Date.Month, Date.Day);
  Date.IsNegative := False;
end;

procedure DecodeDateTimeToTime(const Value: TDateTime; var Time: TZTime);
var MS: Word;
begin
  DecodeTime(Value, Time.Hour, Time.Minute, Time.Second, MS);
  Time.Fractions := MS * NanoSecsPerMSec;
  Time.IsNegative := False;
end;

procedure DecodeDateTimeToTimeStamp(const Value: TDateTime; var TimeStamp: TZTimeStamp);
var MS: Word;
begin
  DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
  DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, MS);
  TimeStamp.Fractions := MS * NanoSecsPerMSec;
  TimeStamp.IsNegative := False;
  TimeStamp.TimeZoneHour := 0;
  TimeStamp.TimeZoneMinute := 0;
end;

procedure TimeStampFromTime(const Time: TZTime; var TS: TZTimeStamp);
begin
  PInt64(@TS.Year)^ := PInt64(@cPascalIntegralDatePart.Year)^;
  PInt64(@TS.Hour)^ := PInt64(@Time.Hour)^;
  PInt64(@TS.Fractions)^ := 0;
  TS.Fractions := Time.Fractions;
  TS.IsNegative := Time.IsNegative;
end;

procedure TimeStampFromDate(const Date: TZDate; var TS: TZTimeStamp);
begin
  PInt64(@TS.Year)^ := PInt64(@Date.Year)^;
  PInt64(@TS.Hour)^ := 0;
  PInt64(@TS.Fractions)^ := 0;
  TS.IsNegative := Date.IsNegative;
end;

procedure TimeFromTimeStamp(const TS: TZTimeStamp; var Time: TZTime);
begin
  PZTime(@Time.Hour)^ := PZTime(@TS.Hour)^;
  Time.IsNegative :=  TS.IsNegative;
end;

procedure DateFromTimeStamp(const TS: TZTimeStamp; var Date: TZDate);
begin
  PZDate(@Date.Year)^ := PZDate(@TS.Year)^;
  Date.IsNegative := TS.IsNegative;
end;

function ZCompareDateTime(const Value1, Value2: TDateTime): Integer;
var
  TS1, TS2: TTimeStamp;
begin
  TS1 := DateTimeToTimeStamp(Value1);
  TS2 := DateTimeToTimeStamp(Value2);
  Result := Ord(TS1.Date > TS2.Date)-Ord(TS1.Date < TS2.Date);
  if Result = 0 then
    Result := Ord(TS1.Time > TS2.Time)-Ord(TS1.Time < TS2.Time);
end;

function ZCompareDate(const Value1, Value2: TZDate): Integer;
begin
  Result := Ord(Value1.IsNegative)-Ord(Value2.IsNegative);
  if Result = 0 then begin
    Result := Value1.Year-Value2.Year;
    if Result = 0 then begin
      Result := Value1.Month - Value2.Month;
      if Result = 0 then
        Result := Value1.Day - Value2.Day;
    end;
  end;
end;

function ZCompareTime(const Value1, Value2: TZTime): Integer;
begin
  Result := Ord(Value1.IsNegative)-Ord(Value2.IsNegative);
  if Result = 0 then begin
    Result := Value1.Hour - Value2.Hour;
    if Result = 0 then begin
      Result := Value1.Minute - Value2.Minute;
      if Result = 0 then begin
        Result := Value1.Second - Value2.Second;
        if Result = 0 then
          Result := Ord(Value1.Fractions > Value2.Fractions)-Ord(Value1.Fractions < Value2.Fractions);
      end;
    end;
  end;
end;

function ZCompareTimeStamp(const Value1, Value2: TZTimeStamp): Integer;
begin
  Result := Ord(Value1.IsNegative)-Ord(Value2.IsNegative);
  if Result = 0 then begin
    Result := Value1.Year-Value2.Year;
    if Result = 0 then begin
      Result := Value1.Month - Value2.Month;
      if Result = 0 then begin
        Result := Value1.Day - Value2.Day;
        if Result = 0 then begin
          Result := (SmallInt(Value1.Hour) + Value1.TimeZoneHour) - (SmallInt(Value2.Hour) + Value2.TimeZoneHour);
          if Result = 0 then begin
            Result := Integer(Value1.Minute + Value1.TimeZoneMinute) - Integer(Value2.Minute + Value2.TimeZoneMinute);
            if Result = 0 then begin
              Result := Value1.Second - Value2.Second;
              if Result = 0 then
                Result := Ord(Value1.Fractions > Value2.Fractions)-Ord(Value1.Fractions < Value2.Fractions);
            end;
          end;
        end;
      end;
    end;
  end;
end;

function FormatCompare(const F1, F2: String; MaxLen: Integer): Boolean;
var P1, P2, PEnd1, PEnd2: PChar;
  C: Byte;
begin
  P1 := Pointer(F1);
  P2 := Pointer(F2);
  Result := False;
  if (P1 = nil) or (P2 = nil) then Exit;
  PEnd1 := P1 + Math.Max(Length(F1), MaxLen);
  PEnd2 := P2 + Math.Max(Length(F2), MaxLen);
  while (P1 < PEnd1) and (P2 < PEnd2) do begin
    {$IFDEF UNICODE}
    C := PWord(P1)^ or $0020;
    {$ELSE}
    C := PByte(P1)^ or $20;
    {$ENDIF}
    if ({$IFDEF UNICODE}PWord(P2)^ or $0020{$ELSE}PByte(P2)^ or $20{$ENDIF} = C) then begin
      Inc(P1);
      Inc(P2);
      while (P1 < PEnd1) and ({$IFDEF UNICODE}PWord(P1)^ or $0020{$ELSE}PByte(P1)^ or $20{$ENDIF} = C ) do
        Inc(P1);
      while (P2 < PEnd2) and ({$IFDEF UNICODE}PWord(P2)^ or $0020{$ELSE}PByte(P2)^ or $20{$ENDIF} = C ) do
        Inc(P2);
    end else if C in [Ord('t'), Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord(' ')] then begin
      Inc(P1);
      Inc(P2);
    end else
      Exit;
  end;
  Exit;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToDate(P: PAnsiChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var Date: TZDate): Boolean;
var TS: TZTimeStamp;
begin
  if Len <= FormatSettings.DateFormatLen then begin
    Result := TryRawToDate(P,  Len, FormatSettings.DateFormat, Date);
    if not Result and not FormatCompare(FormatSettings.DateFormat, DefDateFormatYMD, FormatSettings.DateFormatLen) then
      Result := TryRawToDate(P,  Len, DefDateFormatYMD, Date);
  end else begin
    Result := TryPCharToTimeStamp(P, Len, FormatSettings, TS);
    if Result then begin
      Date.Year := TS.Year;
      Date.Month := Ts.Month;
      Date.Day := TS.Day;
      Date.IsNegative := TS.IsNegative;
    end else
      PInt64(@Date.Year)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToDate(P: PWideChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var Date: TZDate): Boolean;
var TS: TZTimeStamp;
begin
  if Len <= FormatSettings.DateFormatLen then begin
    Result := TryUniToDate(P,  Len, FormatSettings.DateFormat, Date);
    if not Result and not FormatCompare(FormatSettings.DateFormat, DefDateFormatYMD, FormatSettings.DateFormatLen) then
      Result := TryUniToDate(P,  Len, DefDateFormatYMD, Date);
  end else begin
    Result := TryPCharToTimeStamp(P, Len, FormatSettings, TS);
    if Result then begin
      Date.Year := TS.Year;
      Date.Month := Ts.Month;
      Date.Day := TS.Day;
      Date.IsNegative := TS.IsNegative;
    end else
      PInt64(@Date.Year)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToTime(P: PAnsiChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var Time: TZTime): Boolean;
var TS: TZTimeStamp;
begin
  Result := False;
  if (Len > 2) and (P <> nil) then
    if (PByte(P+2)^ = Ord(':')) or (Len = FormatSettings.TimeFormatLen)
    then Result := TryRawToTime(P, Len, FormatSettings.TimeFormat, Time)
    else begin
      Result := TryPCharToTimeStamp(P, Len, FormatSettings, TS);
      if Result then begin
        Time.Hour := TS.Hour;
        Time.Minute := TS.Minute;
        Time.Second := TS.Second;
        Time.Fractions := TS.Fractions;
        Time.IsNegative := Ts.IsNegative;
      end;
    end;
  if not Result then begin
    PCardinal(@Time.Hour)^ := 0;
    PInt64(@Time.Second)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToTime(P: PWideChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var Time: TZTime): Boolean;
var TS: TZTimeStamp;
begin
  Result := False;
  if (Len > 2) and (P <> nil) then
    if (PWord(P+2)^ = Ord(':')) or (Len = FormatSettings.TimeFormatLen)
    then Result := TryUniToTime(P, Len, FormatSettings.TimeFormat, Time)
    else begin
      Result := TryPCharToTimeStamp(P, Len, FormatSettings, TS);
      if Result then begin
        Time.Hour := TS.Hour;
        Time.Minute := TS.Minute;
        Time.Second := TS.Second;
        Time.Fractions := TS.Fractions;
        Time.IsNegative := Ts.IsNegative;
      end;
    end;
  if not Result then begin
    PCardinal(@Time.Hour)^ := 0;
    PInt64(@Time.Second)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TryPCharToTimeStamp(P: PAnsiChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var TimeStamp: TZTimeStamp): Boolean;
begin
  if (Len > 2) and (P <> nil) then
    if PByte(P+2)^ = Ord(':') then begin
      PInt64(@TimeStamp.Year)^ := PInt64(@cPascalIntegralDatePart.Year)^;
      PCardinal(@TimeStamp.TimeZoneHour)^ := 0;
      Result := TryRawToTime(P, Len,
        FormatSettings.TimeFormat, PZTime(@TimeStamp.Hour)^)
    end else if (Len >= FormatSettings.DateTimeFormatLen) or
       ((FormatSettings.DateTimeFormatLen - Len ) <= 4) then begin
      Result := TryRawToTimeStamp(P, Len,
        FormatSettings.DateTimeFormat, TimeStamp);
      if not Result and not FormatCompare(FormatSettings.DateTimeFormat, DefDateFormatYMD, 10) then
        Result := TryRawToTimeStamp(P,  Len, DefDateTimeFormatMsecsYMD, TimeStamp);
    end else begin
      PInt64(@TimeStamp.Hour)^ := 0;
      PInt64(@TimeStamp.Fractions)^ := 0;
      Result := TryRawToDate(P, Len, FormatSettings.DateFormat, PZDate(@TimeStamp.Year)^);
      if not Result and not FormatCompare(FormatSettings.DateFormat, DefDateFormatYMD, 10) then
        Result := TryRawToDate(P, Len, DefDateFormatYMD, PZDate(@TimeStamp.Year)^);
      TimeStamp.IsNegative := PZDate(@TimeStamp.Year).IsNegative;
      PZDate(@TimeStamp.Year).IsNegative := False;
    end
  else Result := False;
end;

function TryPCharToTimeStamp(P: PWideChar; Len: Cardinal;
  const FormatSettings: TZClientFormatSettings; var TimeStamp: TZTimeStamp): Boolean;
begin
  if (Len > 2) and (P <> nil) then
    if PByte(P+2)^ = Ord(':') then begin
      PInt64(@TimeStamp.Year)^ := PInt64(@cPascalIntegralDatePart.Year)^;
      PCardinal(@TimeStamp.TimeZoneHour)^ := 0;
      Result := TryUniToTime(P, Len,
        FormatSettings.TimeFormat, PZTime(@TimeStamp.Hour)^)
    end else if (Len >= FormatSettings.DateTimeFormatLen) or
       ((FormatSettings.DateTimeFormatLen - Len ) <= 4) then begin
      Result := TryUniToTimeStamp(P, Len, FormatSettings.DateTimeFormat, TimeStamp);
      if not Result and not FormatCompare(FormatSettings.DateTimeFormat, DefDateFormatYMD, 10) then
        Result := TryUniToTimeStamp(P,  Len, DefDateTimeFormatMsecsYMD, TimeStamp);
    end else begin
      PInt64(@TimeStamp.Hour)^ := 0;
      PInt64(@TimeStamp.Fractions)^ := 0;
      Result := TryUniToDate(P, Len, FormatSettings.DateFormat, PZDate(@TimeStamp.Year)^);
      if not Result and not FormatCompare(FormatSettings.DateFormat, DefDateFormatYMD, 10) then
        Result := TryUniToDate(P, Len, DefDateFormatYMD, PZDate(@TimeStamp.Year)^);
      PZDate(@TimeStamp.Year).IsNegative := False;
      TimeStamp.IsNegative := PZDate(@TimeStamp.Year).IsNegative;
    end
  else Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToDateTime(P: PAnsiChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var DateTime: TDateTime): Boolean;
var TS: TZTimeStamp;
begin
  if TryPCharToTimeStamp(P, Len, FormatSettings, TS) then
    Result := TryTimeStampToDateTime(TS, DateTime)
  else begin
    Result := False;
    Datetime := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "TS" does not seem to be intialized} {$ENDIF}
function TryPCharToDateTime(P: PWideChar; Len: Cardinal; const FormatSettings: TZClientFormatSettings; var DateTime: TDateTime): Boolean;
var TS: TZTimeStamp;
begin
  if TryPCharToTimeStamp(P, Len, FormatSettings, TS) then
    Result := TryTimeStampToDateTime(TS, DateTime)
  else begin
    Result := False;
    Datetime := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function AnsiSQLDateToDateTime(P: PWideChar; L: LengthInt): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  DateFound: Boolean;
  tmp: TDateTime;
  procedure ExtractTime(P: PWideChar; L: LengthInt);
  begin
    Hour := UnicodeToIntDef(P, P+2, 0);
    Min := UnicodeToIntDef((P+3), (P+5), 0);
    Sec := UnicodeToIntDef((P+6), (p+8), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    MSec := 0;
    if (L > 8) and ((P+8)^ = '.') then begin
      MSec := UnicodeToIntDef(P+9, (P+L), 0);
      L := L-9;
      if L < 3
      then MSec := MSec * MSecMulTable[L]
      else if L > 3 then
        MSec := MSec div MSecMulTable[3-L]
    end;
  end;
begin
  Result := 0;
  DateFound := False;
  if L >= 10 then begin
    Year := UnicodeToIntDef(P, (P+4), 0);
    Month := UnicodeToIntDef((P+5), (P+7), 0);
    Day := UnicodeToIntDef((P+8), (P+10), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then begin
      if TryEncodeDate(Year, Month, Day, tmp) then begin
        Result := tmp;
        DateFound := True;
      end;
    end;
  end;

  if (L >= 18) or ( not DateFound ) then begin
    if DateFound
    then ExtractTime(P+11, L-11)
    else ExtractTime(P, L);
    if TryEncodeTime(Hour, Min, Sec, MSec, tmp) then
      if Result >= 0
      then Result := Result + Tmp
      else Result := Result - Tmp
  end;
end;

function AnsiSQLDateToDateTime(const Value: UnicodeString): TDateTime;
var P: PWideChar;
begin
  P := Pointer(Value);
  if P = nil
  then Result := 0
  else Result := AnsiSQLDateToDateTime(P, Length(Value));
end;

function AnsiSQLDateToDateTime(P: PAnsiChar; L: LengthInt): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  DateFound: Boolean;
  tmp: TDateTime;
  procedure ExtractTime(P: PAnsiChar; L: LengthInt);
  begin
    Hour := RawToIntDef(P, P+2, 0);
    Min := RawToIntDef((P+3), (P+5), 0);
    Sec := RawToIntDef((P+6), (p+8), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    MSec := 0;
    if (L > 8) and ((P+8)^ = '.') then begin
      MSec := RawToIntDef(P+9, (P+L), 0);
      L := L-9;
      if L < 3
      then MSec := MSec * MSecMulTable[L]
      else if L > 3 then
        MSec := MSec div MSecMulTable[3-L]
    end;
  end;
begin
  Result := 0;
  DateFound := False;
  if L >= 10 then begin
    Year := RawToIntDef(P, (P+4), 0);
    Month := RawToIntDef((P+5), (P+7), 0);
    Day := RawToIntDef((P+8), (P+10), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then begin
      if TryEncodeDate(Year, Month, Day, tmp) then begin
        Result := tmp;
        DateFound := True;
      end;
    end;
  end;

  if (L >= 18) or ( not DateFound ) then begin
    if DateFound
    then ExtractTime(P+11, L-11)
    else ExtractTime(P, L);
    if TryEncodeTime(Hour, Min, Sec, MSec, tmp) then
      if Result >= 0
      then Result := Result + Tmp
      else Result := Result - Tmp
  end;
end;

function AnsiSQLDateToDateTime(const Value: RawByteString): TDateTime;
var P: PAnsiChar;
begin
  P := Pointer(Value);
  if P = nil
  then Result := 0
  else Result := AnsiSQLDateToDateTime(P, Length(Value));
end;

function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var L, L2, Year, Month, Day: Word;
  Buffer: array[0..cMaxDateLenQuoted] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeDate(Value, Year, Month, Day);
  L := DateToRaw(Year, Month, Day, @Buffer[0],
    ConFormatSettings.DateFormat, Quoted, False);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil , L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

function DateTimeToRawSQLDate(const Value: TDateTime; Buf: PAnsichar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word;
var L, Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := DateToRaw(Year, Month, Day, Buf,
    ConFormatSettings.DateFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L);
    Result := Result+L;
  end;
end;

function DateToRaw(Year, Month, Day: Word; Buf: PAnsichar;
  const Format: String; Quoted, Negative: Boolean): Byte;
var PStart: PAnsiChar;
  PFormat, PEnd, FStart: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean; //equals to C1?
  B: Byte absolute EQ2;
label inc_dbl, next4; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PByte(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  FStart := PFormat+1+Ord(EQ2);
                  if EQ2 then
                    while (FStart < PEnd) and
                      {$IFDEF UNICODE}
                      (PWord(FStart)^ or $0020 = C1) do Inc(FStart);
                      {$ELSE}
                      (PByte(FStart)^ or $20 = C1) do Inc(FStart);
                      {$ENDIF}
                  b := (FStart - PFormat);
                  if (B = 5) or (Year >= 10000) then begin
                    if (Year >= 10000) then begin
                      Result := Year div 10000;
                      PByte(Buf)^ := Ord('0')+Result;
                      Year := Year-(Result*10000);
                    end else
                      PByte(Buf)^ := Ord('0');
                    Inc(Buf);
                    goto next4;
                  end else if (B = 4) or (Year >= 1000) then begin
next4:              {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := TwoDigitLookupW[Result];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := TwoDigitLookupW[Year div 100];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                  end else if (B=3) or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+Year div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3);
                  end else if EQ2 or (Year >= 10) then begin
                    PWord(Buf)^ := TwoDigitLookupW[Year];
                    Inc(Buf, 2);
                  end else begin
                    PByte(Buf)^ := Ord('0') + Year;
                    Inc(Buf);
                  end;
                  PFormat := FStart;
                  Continue;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Month];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Day];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  continue;
                end else
                  PByte(Buf)^ := Ord('0') + Day;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateTimeToUnicodeSQLDate(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): word;
var L, Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := DateToUni(Year, Month, Day, Buf,
    ConFormatSettings.DateFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

function DateToUni(Year, Month, Day: Word; Buf: PWideChar;
  const Format: String; Quoted, Negative: Boolean): Byte;
var PStart: PWideChar;
  PFormat, PEnd, FStart: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean; //equals to C1?
  B: Byte absolute EQ2;
label inc_dbl, next4; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PWord(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  FStart := PFormat+1+Ord(EQ2);
                  if EQ2 then
                    while (FStart < PEnd) and
                      {$IFDEF UNICODE}
                      (PWord(FStart)^ or $0020 = C1) do Inc(FStart);
                      {$ELSE}
                      (PByte(FStart)^ or $20 = C1) do Inc(FStart);
                      {$ENDIF}
                  b := (FStart - PFormat);
                  if (B = 5) or (Year >= 10000) then begin
                    if (Year >= 10000) then begin
                      Result := Year div 10000;
                      PWord(Buf)^ := Ord('0')+Result;
                      Year := Year-(Result*10000);
                    end else
                      PWord(Buf)^ := Ord('0');
                    Inc(Buf);
                    goto next4;
                  end else if (B = 4) or (Year >= 1000) then begin
next4:              {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PCardinal(Buf)^   := TwoDigitLookupLW[Result];
                    PCardinal(Buf+2)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PCardinal(Buf)^   := TwoDigitLookupLW[Year div 100];
                    PCardinal(Buf+2)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                  end else if (B = 3) or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := Ord('0')+Result;
                    PCardinal(Buf+1)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := Ord('0')+Year div 100;
                    PCardinal(Buf+1)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3);
                  end else if (B = 2) or (Year >= 10) then begin
                    PCardinal(Buf)^ := TwoDigitLookupLW[Year];
                    Inc(Buf, 2);
                  end else begin
                    PWord(Buf)^ := Ord('0') + Year;
                    Inc(Buf);
                  end;
                  PFormat := FStart;
                  Continue;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Month];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Day];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  continue;
                end else
                  PWord(Buf)^ := Ord('0') + Day;
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateToUni(Year, Month, Day: Word; const Format, Suffix: String;
  Quoted, Negative: Boolean): UnicodeString; overload;
var L, L2: Word;
  Buffer: array[0..cMaxDateLenQuoted] of WideChar;
  P: PWideChar;
begin
  L := DateToUni(Year, Month, Day, @Buffer[0], Format, Quoted, False);
  l2 := Length(Suffix);
  Result := '';
  System.SetLength(Result, L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString): UnicodeString;
var L, L2, Year, Month, Day: Word;
  Buffer: array[0..cMaxDateLenQuoted] of WideChar;
  P: PWideChar;
begin
  DecodeDate(Value, Year, Month, Day);
  L := DateToUni(Year, Month, Day, @Buffer[0],
    ConFormatSettings.DateFormat, Quoted, False);
  l2 := Length(Suffix);
  System.SetString(Result, nil , L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

function DateTimeToSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: string): string;
begin
  Result := {$IFDEF UNICODE} DateTimeToUnicodeSQLDate {$ELSE} DateTimeToRawSQLDate {$ENDIF} (Value, ConFormatSettings, Quoted, Suffix);
end;

function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var l, l2, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeLenQuoted] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  L := TimeToRaw(Hour, Minute, Second, MSec * NanoSecsPerMSec, @Buffer[0],
    ConFormatSettings.TimeFormat, Quoted, False);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil , L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

function DateTimeToRawSQLTime(const Value: TDateTime; Buffer: PAnsichar;
  const ConFormatSettings: TZClientFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word;
var l, Hour, Minute, Second, MSec: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  Result := TimeToRaw(Hour, Minute, Second, MSec * NanoSecsPerMSec, Buffer,
    ConFormatSettings.TimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buffer+Result)^, L);
    Result := Result+L;
  end;
end;

function TimeToRaw(Hour, Minute, Second: Word; Fractions: Cardinal;
  Buf: PAnsichar; const Format: String; Quoted, IsNegative: Boolean): Byte;
var PStart, ZStart: PAnsiChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean;
  B: Byte absolute EQ2;
label inc_dbl; //keep code tiny
begin
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(IsNegative));
  if IsNegative then
    PByte(Buf-1)^ := Ord('-');
  PFormat := Pointer(Format);
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1;
    case C1 of
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Hour];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  Continue;
                end else
                  PByte(Buf)^ := Ord('0') + Hour;
      Ord('n'),
      Ord('m'): if EQ2 or (Minute >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Minute];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Second];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Second;
      Ord('f'),
      Ord('z'): begin
                  ZStart := Buf;
                  PInt64(Buf)^ := PInt64(r8Zeros)^;
                  if Fractions = 0 then begin
                    Inc(Buf, 8);
                    PByte(Buf)^ := Byte('0');
                  end else begin
                    B := GetOrdinalDigits(Fractions);
                    Inc(Buf, 9-B);
                    IntToRaw(Fractions, Buf, B);
                    Inc(Buf, B-1);
                  end;
                  {$IFDEF UNICODE}
                  while PWord(PFormat+1)^ or $0020 = C1 do begin
                  {$ELSE}
                  while PByte(PFormat+1)^ or $20 = C1 do begin
                  {$ENDIF}
                    Inc(PFormat);
                    if (ZStart<Buf) then
                      Inc(ZStart);
                  end;
                  while (PByte(Buf)^ = Byte('0')) and (Buf>ZStart) do
                    Dec(Buf);
                end;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString): UnicodeString;
var l, l2, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeLenQuoted] of WideChar;
  P: PWideChar;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  L := TimeToUni(Hour, Minute, Second, MSec * NanoSecsPerMSec, @Buffer[0],
    ConFormatSettings.TimeFormat, Quoted, False);
  l2 := Length(Suffix);
  System.SetString(Result, nil , L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

function DateTimeToUnicodeSQLTime(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): Word;
var l, Hour, Minute, Second, MSec: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  Result := TimeToUni(Hour, Minute, Second, MSec * NanoSecsPerMSec, Buf,
    ConFormatSettings.TimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

function TimeToUni(Hour, Minute, Second: Word; Fractions: Cardinal;
  Buf: PWideChar; const Format: String; Quoted, IsNegative: Boolean): Byte;
var PStart, ZStart: PWideChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean;
  B: Byte absolute EQ2;
label inc_dbl; //keep code tiny
begin
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(IsNegative));
  if IsNegative then
    PWord(Buf-1)^ := Ord('-');
  PFormat := Pointer(Format);
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1;
    case C1 of
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Hour];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  continue;
                end else
                  PWord(Buf)^ := Ord('0') + Hour;
      Ord('m'),
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Minute];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Second];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Second;
      Ord('f'),
      Ord('z'): begin
                  ZStart := Buf;
                  PInt64(Buf)^ := PInt64(u4Zeros)^;
                  PInt64(Buf+4)^ := PInt64(u4Zeros)^;
                  if Fractions = 0 then begin
                    Inc(Buf, 8);
                    PWord(Buf)^ := Byte('0');
                  end else begin
                    B := GetOrdinalDigits(Fractions);
                    Inc(Buf, 9-B);
                    IntToUnicode(Fractions, Buf, B);
                    Inc(Buf, B-1);
                  end;
                  {$IFDEF UNICODE}
                  while PWord(PFormat+1)^ or $0020 = C1 do begin
                  {$ELSE}
                  while PByte(PFormat+1)^ or $20 = C1 do begin
                  {$ENDIF}
                    Inc(PFormat);
                    if (ZStart<Buf) then
                      Inc(ZStart);
                  end;
                  while (PWord(Buf)^ = Word('0')) and (Buf>ZStart) do
                    Dec(Buf);
                end;
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat)
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateTimeToSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: string): string;
begin
  Result := {$IFDEF UNICODE} DateTimeToUnicodeSQLTime {$ELSE} DateTimeToRawSQLTime {$ENDIF} (Value, ConFormatSettings, Quoted, Suffix);
end;

function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var l, l2, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeStampLenQuoted] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L := DateTimeToRaw(Year, Month, Day, Hour, Minute, Second, MSec*NanoSecsPerMSec,
    @Buffer[0], ConFormatSettings.DateTimeFormat, Quoted, False);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil, L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

function DateTimeToRawSQLTimeStamp(const Value: TDateTime; Buf: PAnsiChar;
  const ConFormatSettings: TZClientFormatSettings; Quoted: Boolean;
  const Suffix: RawByteString = EmptyRaw): Word;
var l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  Result := DateTimeToRaw(Year, Month, Day, Hour, Minute, Second,
    MSec*NanoSecsPerMSec, Buf, ConFormatSettings.DateTimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L);
    Result := Result+L;
  end;
end;

function DateTimeToRaw(Year, Month, Day, Hour, Minute, Second: Word;
  Fractions: Cardinal; Buf: PAnsiChar; const Format: String;
  Quoted, Negative: Boolean): Byte;
var PStart, ZStart: PAnsiChar;
  PFormat, PEnd{$IFDEF UNICODE}, FStart{$ENDIF}: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean; //equals to C1?
  B: Byte absolute EQ2;
label inc_dbl, next4; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PByte(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  {$IFDEF UNICODE}FStart{$ELSE}ZStart{$ENDIF} := PFormat+1+Ord(EQ2);
                  if EQ2 then
                    while ({$IFDEF UNICODE}FStart{$ELSE}ZStart{$ENDIF} < PEnd) and
                      {$IFDEF UNICODE}
                      (PWord(FStart)^ or $0020 = C1) do Inc(FStart);
                      {$ELSE}
                      (PByte(ZStart)^ or $20 = C1) do Inc(ZStart);
                      {$ENDIF}
                  b := ({$IFDEF UNICODE}FStart{$ELSE}ZStart{$ENDIF} - PFormat);
                  if (B = 5) or (Year >= 10000) then begin
                    if (Year >= 10000) then begin
                      Result := Year div 10000;
                      PByte(Buf)^ := Ord('0')+Result;
                      Year := Year-(Result*10000);
                    end else
                      PByte(Buf)^ := Ord('0');
                    Inc(Buf);
                    goto next4;
                  end else if (B = 4) or (Year >= 1000) then begin
next4:              {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := TwoDigitLookupW[Result];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := TwoDigitLookupW[Year div 100];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                  end else if (B=3) or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+Year div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3);
                  end else if EQ2 or (Year >= 10) then begin
                    PWord(Buf)^ := TwoDigitLookupW[Year];
                    Inc(Buf, 2);
                  end else begin
                    PByte(Buf)^ := Ord('0') + Year;
                    Inc(Buf);
                  end;
                  PFormat := {$IFDEF UNICODE}FStart{$ELSE}ZStart{$ENDIF};
                  Continue;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Month];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  Continue;
                end else
                  PByte(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Day];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Day;
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Hour];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Hour;
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Minute];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Second];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Second;
      Ord('f'),
      Ord('z'): begin
                  ZStart := Buf;
                  PInt64(Buf)^ := PInt64(r8Zeros)^;
                  if Fractions = 0 then begin
                    Inc(Buf, 8);
                    PByte(Buf)^ := Byte('0');
                  end else begin
                    B := GetOrdinalDigits(Fractions);
                    Inc(Buf, 9-B);
                    IntToRaw(Fractions, Buf, B);
                    Inc(Buf, B-1);
                  end;
                  {$IFDEF UNICODE}
                  while PWord(PFormat+1)^ or $0020 = C1 do begin
                  {$ELSE}
                  while PByte(PFormat+1)^ or $20 = C1 do begin
                  {$ENDIF}
                    Inc(PFormat);
                    if (ZStart<Buf) then
                      Inc(ZStart);
                  end;
                  while (PByte(Buf)^ = Byte('0')) and (Buf>ZStart) do
                    Dec(Buf);
                end;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): Word;
var l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  Result := DateTimeToUni(Year, Month, Day, Hour, Minute, Second,
    MSec*NanoSecsPerMSec, Buf, ConFormatSettings.DateTimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

function DateTimeToUni(Year, Month, Day, Hour, Minute, Second: Word;
  Fractions: Cardinal; Buf: PWideChar; const Format: String;
  Quoted, Negative: Boolean): Byte;
var PStart, ZStart: PWideChar;
  PFormat, PEnd{$IFNDEF UNICODE}, FStart{$ENDIF}: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2: Boolean; //equals to C1?
  B: Byte absolute EQ2;
label inc_dbl, next4; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PWord(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  {$IFDEF UNICODE}ZStart{$ELSE}FStart{$ENDIF} := PFormat+1+Ord(EQ2);
                  if EQ2 then
                    while ({$IFDEF UNICODE}ZStart{$ELSE}FStart{$ENDIF} < PEnd) and
                      {$IFDEF UNICODE}
                      (PWord(ZStart)^ or $0020 = C1) do Inc(ZStart);
                      {$ELSE}
                      (PByte(FStart)^ or $20 = C1) do Inc(FStart);
                      {$ENDIF}
                  b := ({$IFDEF UNICODE}ZStart{$ELSE}FStart{$ENDIF} - PFormat);
                  if (B = 5) or (Year >= 10000) then begin
                    if (Year >= 10000) then begin
                      Result := Year div 10000;
                      PWord(Buf)^ := Ord('0')+Result;
                      Year := Year-(Result*10000);
                    end else
                      PWord(Buf)^ := Ord('0');
                    Inc(Buf);
                    goto next4;
                  end else if (B = 4) or (Year >= 1000) then begin
next4:              {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PCardinal(Buf)^   := TwoDigitLookupLW[Result];
                    PCardinal(Buf+2)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PCardinal(Buf)^   := TwoDigitLookupLW[Year div 100];
                    PCardinal(Buf+2)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                  end else if (B = 3) or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := Ord('0')+Result;
                    PCardinal(Buf+1)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := Ord('0')+Year div 100;
                    PCardinal(Buf+1)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3);
                  end else if (B = 2) or (Year >= 10) then begin
                    PCardinal(Buf)^ := TwoDigitLookupLW[Year];
                    Inc(Buf, 2);
                  end else begin
                    PWord(Buf)^ := Ord('0') + Year;
                    Inc(Buf);
                  end;
                  PFormat := {$IFDEF UNICODE}ZStart{$ELSE}FStart{$ENDIF};
                  Continue;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Month];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  Continue;
                end else
                  PWord(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Day];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Day;
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Hour];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Hour;
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Minute];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PCardinal(Buf)^ := TwoDigitLookupLW[Second];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Second;
      Ord('f'),
      Ord('z'): begin
                  ZStart := Buf;
                  PInt64(Buf)^ := PInt64(u4Zeros)^;
                  PInt64(Buf+4)^ := PInt64(u4Zeros)^;
                  if Fractions = 0 then begin
                    Inc(Buf, 8);
                    PWord(Buf)^ := Byte('0');
                  end else begin
                    B := GetOrdinalDigits(Fractions);
                    Inc(Buf, 9-B);
                    IntToUnicode(Fractions, Buf, B);
                    Inc(Buf, B-1);
                  end;
                  {$IFDEF UNICODE}
                  while PWord(PFormat+1)^ or $0020 = C1 do begin
                  {$ELSE}
                  while PByte(PFormat+1)^ or $20 = C1 do begin
                  {$ENDIF}
                    Inc(PFormat);
                    if (ZStart<Buf) then
                      Inc(ZStart);
                  end;
                  while (PWord(Buf)^ = Word('0')) and (Buf>ZStart) do
                    Dec(Buf);
                end;
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZClientFormatSettings;
  const Quoted: Boolean; const Suffix: UnicodeString = ''): UnicodeString;
var l, l2, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..cMaxTimeStampLenQuoted] of WideChar;
  P: PWideChar;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L := DateTimeToUni(Year, Month, Day, Hour, Minute,
    Second, MSec*NanoSecsPerMSec, @Buffer[0], ConFormatSettings.DateTimeFormat, Quoted, False);
  l2 := Length(Suffix);
  System.SetString(Result, nil, L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

function EncodeCString(const Value: UnicodeString): UnicodeString;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PWideChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  DestLength := 0;
  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ = #0
    then Inc(DestLength, 4)
    else case PWord(SrcBuffer)^ of
      Ord('"'), Ord(''''), Ord('\'):
       Inc(DestLength, 2)
      else
       Inc(DestLength);
    end;
    Inc(SrcBuffer);
  end;
  if SrcLength = DestLength then begin
    Result := Value;
    Exit;
  end;

  SrcBuffer := Pointer(Value);
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);

  for I := 1 to SrcLength do begin
    case PWord(SrcBuffer)^ of
      Ord(#0): begin
          PWord(DestBuffer)^ := Ord('\');
          PWord(DestBuffer+1)^ := Ord('0') + (Byte(PWord(SrcBuffer)^) shr 6);
          PWord(DestBuffer+2)^ := Ord('0') + ((Byte(PWord(SrcBuffer)^) shr 3) and $07);
          PWord(DestBuffer+3)^ := Ord('0') + (Byte(PWord(SrcBuffer)^) and $07);
          Inc(DestBuffer, 4);
        end;
      Ord('"'), Ord(''''), Ord('\'): begin
          PWord(DestBuffer)^ := Ord('\');
          PWord(DestBuffer+1)^ := Ord(SrcBuffer^);
          Inc(DestBuffer, 2);
        end;
      else begin
          DestBuffer^ := SrcBuffer^;
          Inc(DestBuffer);
        end;
    end;
    Inc(SrcBuffer);
  end;
end;

function EncodeCString(const Value: RawByteString): RawByteString;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PAnsiChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  DestLength := 0;
  for I := 1 to SrcLength do begin
    if PByte(SrcBuffer)^ = Ord(#0)
    then Inc(DestLength, 4)
    else case PByte(SrcBuffer)^ of
      Ord('"'), Ord(''''), Ord('\'):
       Inc(DestLength, 2)
      else
       Inc(DestLength);
    end;
    Inc(SrcBuffer);
  end;
  if SrcLength = DestLength then begin
    Result := Value;
    Exit;
  end;

  SrcBuffer := Pointer(Value);
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);

  for I := 1 to SrcLength do begin
    case PByte(SrcBuffer)^ of
      Ord(#0): begin
          PByte(DestBuffer)^ := Ord('\');
          PByte(DestBuffer+1)^ := Ord('0') + (PByte(SrcBuffer)^ shr 6);
          PByte(DestBuffer+2)^ := Ord('0') + ((PByte(SrcBuffer)^ shr 3) and $07);
          PByte(DestBuffer+3)^ := Ord('0') + (PByte(SrcBuffer)^ and $07);
          Inc(DestBuffer, 4);
        end;
      Ord('"'), Ord(''''), Ord('\'): begin
          PByte(DestBuffer)^ := Ord('\');
          PByte(DestBuffer+1)^ := Ord(SrcBuffer^);
          Inc(DestBuffer, 2);
        end;
      else begin
          DestBuffer^ := SrcBuffer^;
          Inc(DestBuffer);
        end;
    end;
    Inc(SrcBuffer);
  end;
end;

function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PWideChar): LengthInt; overload;
begin
  Result := 0;
  while SrcLength > 0 do begin
    if SrcBuffer^ = '\' then begin
      Inc(SrcBuffer);
      case SrcBuffer^ of
        '0'..'9':
          begin
            DestBuffer^ := WideChar(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
              or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
              or ((Byte(SrcBuffer[2]) - Ord('0'))));
            Inc(SrcBuffer, 3);
            Dec(SrcLength, 4);
          end
        else
          begin
            case SrcBuffer^ of
              'r': DestBuffer^ := #13;
              'n': DestBuffer^ := #10;
              't': DestBuffer^ := #9;
              else
                DestBuffer^ := SrcBuffer^;
            end;
            Inc(SrcBuffer);
            Dec(SrcLength, 2);
          end
      end;
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(Result);
  end;
end;

function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PAnsiChar): LengthInt; overload;
begin
  Result := 0;
  while SrcLength > 0 do begin
    if Ord(SrcBuffer^) = Ord('\') then begin
      Inc(SrcBuffer);
      case Ord(SrcBuffer^) of
        Ord('0')..Ord('9'):
          begin
            AnsiChar(DestBuffer^) := AnsiChar(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
              or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
              or ((Byte(SrcBuffer[2]) - Ord('0'))));
            Inc(SrcBuffer, 3);
            Dec(SrcLength, 4);
          end
        else
          begin
            case Ord(SrcBuffer^) of
              Ord('r'): PByte(DestBuffer)^ := Ord(#13);
              Ord('n'): PByte(DestBuffer)^ := Ord(#10);
              Ord('t'): PByte(DestBuffer)^ := Ord(#9);
              else
                DestBuffer^ := SrcBuffer^;
            end;
            Inc(SrcBuffer);
            Dec(SrcLength, 2);
          end
      end;
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(Result);
  end;
end;

procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PWideChar; out Result: UnicodeString);
begin
  Result := '';
  SetLength(Result, SrcLength);
  SetLength(Result, DecodeCString(SrcLength, SrcBuffer, Pointer(Result)));
end;

procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PAnsiChar; out Result: RawByteString);
begin
  Result := EmptyRaw;
  SetLength(Result, SrcLength);
  SetLength(Result, DecodeCString(SrcLength, SrcBuffer, Pointer(Result)));
end;

function DecodeCString(const Value: RawByteString): RawByteString;
begin
  DecodeCString(Length(Value), Pointer(Value), Result);
end;

function DecodeCString(const Value: UnicodeString): UnicodeString;
begin
  DecodeCString(Length(Value), Pointer(Value), Result);
end;

function ReplaceChar(const Source, Target: Char; const Str: string): string;
var
  P: PChar;
  I:Integer;
begin
  Result := Str;
  UniqueString(Result);
  P := Pointer(Result);
  for i := 0 to Length(Str) - 1 do
  begin
    if P^ = Source then
      P^ := Target;
    Inc(P);
  end;
end;

procedure ReplaceChar(Var Str: string; const Source, Target: Char);
var P, PEnd: PChar;
begin
  UniqueString(Str);
  P := Pointer(Str);
  PEnd := P + Length(Str);
  while P < PEnd do begin
    if P^ = Source then
      P^ := Target;
    Inc(P);
  end;
end;

function RemoveChar(ToRemove: Char; const Str: string): string;
var
  PSrc, PSrcEnd, PDest: PChar;
  Len: Integer;
begin
  Len := Length(Str);
  {$IFDEF FPC} Result := '';{$ENDIF}
  SetLength(Result, Len);
  if Len = 0 then
    Exit;
  PSrc := Pointer(Str);
  PSrcEnd := PSrc+Len; //address the term
  PDest := Pointer(Result);

  while PSrc < PSrcEnd do
  begin
    if PSrc^ <> ToRemove then
    begin
      PDest^ := PSrc^;
      Inc(PDest);
    end
    else
      Dec(Len);
    Inc(PSrc);
  end;
  SetLength(Result, Len);
end;

procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);
begin
  if AddStr <> '' then
    if Str = '' then
      Str := AddStr
    else
      Str := Str + Delimiter + AddStr;
end;

procedure BreakString(const Str, Delimiter: String; var Left, Right: String);
var
  DelimPos, DelimLen: Integer;
  StrSave: string;
begin
  DelimPos := ZFastCode.Pos(Delimiter, Str);
  if DelimPos > 0 then begin
    DelimLen := Length(Delimiter);
    StrSave := Str; // allow one variable both as Str and Left
    Left := Copy(StrSave, 1, DelimPos-1);
    Right := Copy(StrSave, DelimPos + DelimLen, MaxInt);
  end else begin
    Left := Str;
    Right := '';
  end;
end;


procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);
begin
 MajorVersion := FullVersion div 1000000;
 MinorVersion := (FullVersion - (MajorVersion * 1000000)) div 10000;
 SubVersion   := FullVersion-(MajorVersion*1000000)-(MinorVersion*10000);
end;

function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;
begin
  Result := (MajorVersion * 1000000) + (MinorVersion * 10000) + SubVersion;
end;

function FormatSQLVersion(const SQLVersion: Integer): string;
var
   MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeSQLVersioning(SQLVersion, MajorVersion, MinorVersion, SubVersion);
 Result := ZFastCode.IntToStr(MajorVersion)+'.'+
           ZFastCode.IntToStr(MinorVersion)+'.'+
           ZFastCode.IntToStr(SubVersion);
end;

function AppendCondition(const Condition: string): string;
begin
  if Condition = ''
    then Result := ''
    else Result := ' AND ' + Condition;
end;

function ASCII7ToUnicodeString(const Src: RawByteString): UnicodeString;
begin
  Result := Ascii7ToUnicodeString(Pointer(Src), Length(Src));
end;

function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): UnicodeString;
var Source: PByteArray absolute Src;
  PEnd: PAnsiChar;
  Dest: PWordArray;
begin
  if Src = nil then begin
    Result := '';
    Exit;
  end;
  System.SetString(Result, nil, Len);
  Dest := Pointer(Result);
  PEnd := PAnsiChar(Source)+Len-8;
  while PAnsiChar(Source) < PEnd do begin//making a octed processing loop
    Dest[0] := Source[0];
    Dest[1] := Source[1];
    Dest[2] := Source[2];
    Dest[3] := Source[3];
    Dest[4] := Source[4];
    Dest[5] := Source[5];
    Dest[6] := Source[6];
    Dest[7] := Source[7];
    Inc(PWideChar(Dest), 8);
    Inc(PAnsiChar(Src), 8);
  end;
  Inc(PEnd, 8);
  while PAnsiChar(Source) < PEnd do begin//processing final bytes
    Dest[0] := Source[0];
    inc(PAnsiChar(Source));
    inc(PWideChar(Dest));
  end;
end;

procedure ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt; var Result: UnicodeString);
var Source: PByteArray absolute Src;
  PEnd: PAnsiChar;
  Dest: PWordArray;
begin
  if Src = nil then begin
    Result := '';
    Exit;
  end;
  System.SetString(Result, nil, Len);
  Dest := Pointer(Result);
  PEnd := PAnsiChar(Source)+Len-8;
  while PAnsiChar(Source) < PEnd do begin//making a octed processing loop
    Dest[0] := Source[0];
    Dest[1] := Source[1];
    Dest[2] := Source[2];
    Dest[3] := Source[3];
    Dest[4] := Source[4];
    Dest[5] := Source[5];
    Dest[6] := Source[6];
    Dest[7] := Source[7];
    Inc(PWideChar(Dest), 8);
    Inc(PAnsiChar(Src), 8);
  end;
  Inc(PEnd, 8);
  while PAnsiChar(Source) < PEnd do begin//processing final bytes
    Dest[0] := Source[0];
    inc(PAnsiChar(Source));
    inc(PWideChar(Dest));
  end;
end;

function UnicodeStringToASCII7(const Src: UnicodeString): RawByteString;
begin
  Result := UnicodeStringToASCII7(Pointer(Src), Length(Src));
end;

function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString;
var i: integer;
  BArr: PByteArray absolute Result;
  WArr: PWordArray absolute Src;
begin
  if (Src = nil) or (Len = 0) then
    Result := EmptyRaw
  else begin
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := EmptyRaw; //speeds up SetLength x2
    SetLength(Result, len);
    {$ELSE}
    System.SetString(Result,nil, Len);
    {$ENDIF}
{$R-}
    for i := 0 to len-1 do
      BArr[i] := WArr[i]; //0..127 equals to widechars
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

procedure UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt; var Result: RawByteString);
var i: integer;
  BArr: PByteArray absolute Result;
  WArr: PWordArray absolute Src;
begin
  if (Src = nil) or (Len = 0) then
    Result := EmptyRaw
  else begin
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := EmptyRaw; //speeds up SetLength x2
    SetLength(Result, len);
    {$ELSE}
    System.SetString(Result,nil, Len);
    {$ENDIF}
{$R-}
    for i := 0 to len-1 do
      BArr[i] := WArr[i]; //0..127 equals to widechars
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "FloatToRaw" marked as inline is not inlined}{$ENDIF}
function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  Result := '';
  System.SetString(Result, PAnsiChar(@Buffer[0]), FloatToRaw(Value, @Buffer[0]));
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt;
{$IFDEF NEXTGEN}
var
  Buffer: array[0..63] of WideChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := FloatToText(PWideChar(@Buffer[0]), Value, fvExtended, ffGeneral, 15, 0);
  for I := 0 to Result -1 do
    PByte(Buf+I)^ := Ord(Buffer[i]);
  {$ELSE}
  Result := {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(
    Buf, Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0);
  {$ENDIF}
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "FloatToSqlRaw" marked as inline is not inlined}{$ENDIF}
function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  Result := '';
  System.SetString(Result, PAnsiChar(@Buffer[0]), FloatToSqlRaw(Value, @Buffer[0]));
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt;
{$IFDEF NEXTGEN}
var
  Buffer: array[0..63] of WideChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := FloatToText(PWideChar(@Buffer[0]), Value, fvExtended, ffGeneral, 15, 0, FmtSettFloatDot);
  for I := 0 to Result -1 do
    PByte(Buf+I)^ := Ord(Buffer[i]);
  {$ELSE}
  Result := {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(
    Buf, Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0, FmtSettFloatDot);
  {$ENDIF}
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "FloatToUnicode" marked as inline is not inlined}{$ENDIF}
function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): UnicodeString;
var
  Buffer: array[0..63] of WideChar;
begin
  Result := '';
  System.SetString(Result, PWideChar(@Buffer[0]), FloatToUnicode(Value, @Buffer[0]));
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt;
{$IFNDEF UNICODE}
var
  Buffer: array[0..63] of AnsiChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := FloatToText(Buf, Value, fvExtended, ffGeneral, 15, 0);
  {$ELSE}
  Result := FloatToText(PAnsiChar(@Buffer[0]), Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0);
  for I := 0 to Result -1 do
    PWord(Buf+I)^ := Ord(Buffer[i]);
  {$ENDIF}
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "FloatToSqlUnicode" marked as inline is not inlined}{$ENDIF}
function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): UnicodeString;
var
  Buffer: array[0..63] of WideChar;
begin
  Result := '';
  System.SetString(Result, PWideChar(@Buffer[0]), FloatToSqlUnicode(Value, @Buffer[0]));
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt;
{$IFNDEF UNICODE}
var
  Buffer: array[0..63] of AnsiChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := FloatToText(Buf, Value, fvExtended, ffGeneral, 15, 0, FmtSettFloatDot);
  {$ELSE}
  Result := FloatToText(PAnsiChar(@Buffer[0]), Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0, FmtSettFloatDot);
  for I := 0 to Result -1 do
    PWord(Buf+I)^ := Ord(Buffer[i]);
  {$ENDIF}
end;

//EgonHugeist: my RTL replacemnet is 2x faster
procedure ZBinToHex(Buffer, Text: PAnsiChar; const Len: LengthInt);
var
  PEnd: PAnsiChar;
begin
  PEnd := Buffer+Len-8;
  while Buffer < PEnd do //try to convert 8Byte per loop
  begin
    PWord(Text)^ := TwoDigitLookupHexW[Ord(Buffer^)];
    PWord(Text+2)^ := TwoDigitLookupHexW[Ord((Buffer+1)^)];
    PWord(Text+4)^ := TwoDigitLookupHexW[Ord((Buffer+2)^)];
    PWord(Text+6)^ := TwoDigitLookupHexW[Ord((Buffer+3)^)];
    PWord(Text+8)^ := TwoDigitLookupHexW[Ord((Buffer+4)^)];
    PWord(Text+10)^ := TwoDigitLookupHexW[Ord((Buffer+5)^)];
    PWord(Text+12)^ := TwoDigitLookupHexW[Ord((Buffer+6)^)];
    PWord(Text+14)^ := TwoDigitLookupHexW[Ord((Buffer+7)^)];
    Inc(Buffer, 8);
    Inc(Text, 16);
  end;
  Inc(PEnd, 8);
  while Buffer < PEnd do
  begin
    PWord(Text)^ := TwoDigitLookupHexW[Ord(Buffer^)];
    Inc(Buffer);
    Inc(Text, 2);
  end;
end;

//EgonHugeist: my RTL replacemnet is 5x faster (BinToHex+Wide-Conv)
procedure ZBinToHex(Buffer: PAnsiChar; Text: PWideChar; const Len: LengthInt);
var
  PEnd: PAnsiChar;
begin
  PEnd := Buffer+Len-8;
  while Buffer < PEnd do  //try to convert 8Byte per loop
  begin
    PCardinal(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    PCardinal(Text+2)^ := TwoDigitLookupHexLW[Ord((Buffer+1)^)];
    PCardinal(Text+4)^ := TwoDigitLookupHexLW[Ord((Buffer+2)^)];
    PCardinal(Text+6)^ := TwoDigitLookupHexLW[Ord((Buffer+3)^)];
    PCardinal(Text+8)^ := TwoDigitLookupHexLW[Ord((Buffer+4)^)];
    PCardinal(Text+10)^ := TwoDigitLookupHexLW[Ord((Buffer+5)^)];
    PCardinal(Text+12)^ := TwoDigitLookupHexLW[Ord((Buffer+6)^)];
    PCardinal(Text+14)^ := TwoDigitLookupHexLW[Ord((Buffer+7)^)];
    Inc(Buffer, 8);
    Inc(Text, 16);
  end;
  Inc(PEnd, 8);
  while Buffer < PEnd do
  begin
    PCardinal(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    Inc(Buffer);
    Inc(Text, 2);
  end;
end;

{$IFDEF NO_RAW_HEXTOBIN}
  /// a conversion table from hexa chars into binary data
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  // - is defined globally, since may be used from an inlined function
var
  ConvertHexToBin: array[byte] of byte;
type
  PNormTableByte = ^TNormTableByte;
  TNormTableByte = packed array[byte] of byte;

function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): Boolean; overload;
var I: Integer;
    B,C: NativeUInt;//PtrUInt;
    tab: {$ifdef CPUX86}TNormTableByte absolute ConvertHexToBin{$else}PNormTableByte{$endif};
begin
  result := false; // return false if any invalid char
  if Hex=nil then
    exit;
  {$ifndef CPUX86}tab := @ConvertHexToBin;{$endif} // faster on PIC an x86_64
  if Bin<>nil then
    for I := 1 to BinBytes do begin
      B := tab[Ord(Hex^)];
      inc(Hex);
      if B>15 then exit;
      B := B shl 4;
      C := tab[Ord(Hex^)];
      inc(Hex);
      if C>15 then exit;
      Bin^ := B+C;
      inc(Bin);
    end else
    for I := 1 to BinBytes do begin // Bin=nil -> validate Hex^ input
      B := tab[Ord(Hex^)];
      inc(Hex);
      if B>15 then exit;
      C := tab[Ord(Hex^)];
      inc(Hex);
      if C>15 then exit;
    end;
  result := true; // conversion OK
end;
{$ENDIF}

procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar;
  const Options: TGUIDConvOptions);
var
  D1: Cardinal;
  W: Word;
  I: Integer;
begin
  if guidQuoted in Options then begin
    PByte(Dest)^ := Ord(#39);
    if guidWithBrackets in Options then begin
      PByte(Dest+1)^ := Ord('{');
      Inc(Dest, 2);
    end else
      Inc(Dest);
  end else if guidWithBrackets in Options then begin
    PByte(Dest)^ := Ord('{');
    Inc(Dest);
  end;
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do begin
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(@D1)^];
    D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  PByte(Dest)^ := Ord('-');
  // Source is binary data in fact, we're using PAnsiChar only to allow
  // pointer math for older Delphis
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  PByte(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  PByte(Dest)^ := Ord('-'); //Process D4
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+8)^];
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+9)^];
  PByte(Dest+5)^ := Ord('-');
  Inc(Dest, 6);
  for i := 0 to 5 do
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+10+i)^];
  if guidWithBrackets in Options then
    PByte(Dest+12)^ := Ord('}');
  if guidQuoted in Options then
    PByte(Dest+12+Ord(guidWithBrackets in Options))^ := Ord(#39);
  if guidSet0Term in Options then
    PByte(Dest+12+Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options))^ := Ord(#0); //set trailing term
end;

//EgonHugeist: my conversion is 10x faster than IDE's
procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar;
  WithBrackets, SetTerm: Boolean);
var Options: TGUIDConvOptions;
begin
  Options := [];
  if WithBrackets then Include(Options, guidWithBrackets);
  if SetTerm then Include(Options, guidSet0Term);
  GUIDToBuffer(Source, Dest, Options);
end;

procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const Options: TGUIDConvOptions);
var
  I: Integer;
  D1: Cardinal;
  W: Word;
begin
  if guidQuoted in Options then begin
    PWord(Dest)^ := Ord(#39);
    if guidWithBrackets in Options then begin
      PWord(Dest+1)^ := Ord('{');
      Inc(Dest, 2);
    end else
      Inc(Dest);
  end else if guidWithBrackets in Options then begin
    PWord(Dest)^ := Ord('{');
    Inc(Dest);
  end;
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do begin
    PCardinal(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(@D1)^];
    if D1 > 0 then D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  PWord(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PCardinal(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PCardinal(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  PWord(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PCardinal(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PCardinal(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  PWord(Dest)^ := Ord('-'); //Process D4
  PCardinal(Dest+1)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+8)^];
  PCardinal(Dest+3)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+9)^];
  PWord(Dest+5)^ := Ord('-');
  Inc(Dest, 6);
  for i := 0 to 5 do
    PCardinal(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+10+i)^];
  if guidWithBrackets in Options then
    PWord(Dest+12)^ := Ord('}');
  if guidQuoted in Options then
    PWord(Dest+12+Ord(guidWithBrackets in Options))^ := Ord(#39);
  if guidSet0Term in Options then
    PWord(Dest+12+Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options))^ := Ord(#0); //set trailing term
end;

procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar;
  WithBrackets, SetTerm: Boolean);
var Options: TGUIDConvOptions;
begin
  Options := [];
  if WithBrackets then Include(Options, guidWithBrackets);
  if SetTerm then Include(Options, guidSet0Term);
  GUIDToBuffer(Source, Dest, Options);
end;

procedure GUIDToRaw(Value: PGUID; const Options: TGUIDConvOptions; var Result: RawByteString);
var P: PAnsiChar;
begin
  ZSetString(nil, 36+((Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options)) shl 1), Result{%H-});
  P := Pointer(Result);
  GUIDToBuffer(Value, P, Options);
end;

function GUIDToRaw(const GUID: TGUID; const Options: TGUIDConvOptions): RawByteString;
var P: PAnsiChar;
begin
  ZSetString(nil, 36+((Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options)) shl 1), Result{%H-});
  P := Pointer(Result);
  GUIDToBuffer(@GUID.D1, P, Options);
end;

function GUIDToRaw(const GUID: TGUID; WithBrackets: Boolean): RawByteString;
var Options: TGUIDConvOptions;
  P: PAnsiChar;
begin
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  ZSetString(nil, 36+(Ord(WithBrackets) shl 1), Result{%H-});
  P := Pointer(Result);
  GUIDToBuffer(@GUID.D1, P, Options);
end;

function GUIDToRaw(Buffer: Pointer; Len: Byte; WithBrackets: Boolean = True): RawByteString;
var Options: TGUIDConvOptions;
  P: PAnsiChar;
begin
  if Len <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  ZSetString(nil, 36+(Ord(WithBrackets) shl 1), Result{%H-});
  P := Pointer(Result);
  GUIDToBuffer(Buffer, P, Options);
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "GUIDToBuffer" marked as inline is not inlined}{$ENDIF}
function GUIDToRaw(const Bts: TBytes; WithBrackets: Boolean): RawByteString;
var Options: TGUIDConvOptions;
  P: PAnsiChar;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  Result := '';
  SetLength(Result, 36+(Ord(WithBrackets) shl 1));
  P := Pointer(Result);
  GUIDToBuffer(Pointer(Bts), P, Options);
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function GUIDToUnicode(const GUID: TGUID; const Options: TGUIDConvOptions): UnicodeString;
var P: PWideChar;
begin
  ZSetString(nil, 36+((Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options)) shl 1), Result{%H-});
  P := Pointer(Result);
  GUIDToBuffer(@GUID.D1, P, Options);
end;

procedure GUIDToUnicode(Value: PGUID; const Options: TGUIDConvOptions; var Result: UnicodeString);
var P: PWideChar;
begin
  Result := '';
  SetLength(Result, 36+((Ord(guidWithBrackets in Options)+Ord(guidQuoted in Options)) shl 1));
  P := Pointer(Result);
  GUIDToBuffer(Value, P, Options);
end;

function GUIDToUnicode(const GUID: TGUID; WithBrackets: Boolean): UnicodeString;
var Options: TGUIDConvOptions;
  P: PWideChar;
begin
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  Result := '';
  SetLength(Result, 36+(Ord(WithBrackets) shl 1));
  P := Pointer(Result);
  GUIDToBuffer(@GUID.D1, P, Options);
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "GUIDToBuffer" marked as inline is not inlined}{$ENDIF}
function GUIDToUnicode(const Bts: TBytes; WithBrackets: Boolean): UnicodeString;
var Options: TGUIDConvOptions;
  P: PWideChar;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  Result := '';
  SetLength(Result, 36+(Ord(WithBrackets) shl 1));
  P := Pointer(Result);
  GUIDToBuffer(Pointer(Bts), P, Options);
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function GUIDToUnicode(Buffer: Pointer; Len: Byte; WithBrackets: Boolean = True): UnicodeString; overload;
var Options: TGUIDConvOptions;
  P: PWideChar;
begin
  if Len <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Options := [];
  if WithBrackets then include(Options, guidWithBrackets);
  Result := '';
  SetLength(Result, 36+(Ord(WithBrackets) shl 1));
  P := Pointer(Result);
  GUIDToBuffer(Buffer, P, Options);
end;

function GUIDToStr(const GUID: TGUID; WithBrackets: Boolean): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (GUID, WithBrackets);
end;

function GUIDToStr(const Bts: TBytes; WithBrackets: Boolean): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (Bts, WithBrackets);
end;

function GUIDToStr(Value: PGUID; const Options: TGUIDConvOptions): string;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF}(Value, Options, {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}(Result));
end;

function GUIDToStr(Buffer: Pointer; Len: Byte; WithBrackets: Boolean): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (Buffer, Len, WithBrackets);
end;

procedure InvalidGUID(C: Char);
begin
  raise EArgumentException.CreateResFmt(@SInvalidGUID, [String(C)]);
end;

procedure ValidGUIDToBinary(Src, Dest: PAnsiChar);
  function HexChar(c: AnsiChar): Byte;
  begin
    case Ord(c) of
      Ord('0')..Ord('9'):  Result := Byte(c) - Byte('0');
      Ord('a')..Ord('f'):  Result := (Byte(c) - Byte('a')) + 10;
      Ord('A')..Ord('F'):  Result := (Byte(c) - Byte('A')) + 10;
    else
      begin
        Result := 0; //satisfy compiler!
        InvalidGUID(Char(C));
      end;
    end;
  end;

  function HexByte(p: PAnsiChar): Byte;
  begin
    Result := Byte((HexChar(AnsiChar(p^)) shl 4) + HexChar(AnsiChar((p+1)^)));
  end;
var
  i: Integer;
begin
  Inc(Src, Ord(Ord(Src^) = Ord('{')));
  for i := 0 to 3 do //process D1
    PByte(dest+I)^ := HexByte(Src+(3-i) shl 1);
  if PByte(Src+8)^ <> Ord('-') then InvalidGUID(Char((Src+8)^));
  Inc(src, 9);
  Inc(dest, 4);
  for i := 0 to 1 do //D2, D3
  begin
    PByte(dest)^ := HexByte(src+2);
    PByte(dest+1)^ := HexByte(src);
    Inc(dest, 2);
    if PByte(Src+4)^ <> Ord('-') then InvalidGUID(Char((Src+4)^));
    Inc(src, 5);
  end;
  PByte(dest)^ := HexByte(src);
  PByte(dest+1)^ := HexByte(src+2);
  Inc(dest, 2);
  if PByte(Src+4)^ <> Ord('-') then InvalidGUID(Char((Src+4)^));
  Inc(src, 5);
  for i := 0 to 5 do
  begin
    PByte(dest)^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
  if not ((Ord(Src^) = Ord('}')) or (Ord(Src^) = Ord(#0))) then InvalidGUID(Char(Src^));
end;

procedure ValidGUIDToBinary(Src: PWideChar; Dest: PAnsiChar);
  function HexChar(c: WideChar): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      begin
        InvalidGUID(Char(C));
        Result := 0; //satisfy compiler
      end;
    end;
  end;

  function HexByte(p: PWideChar): Byte;
  begin
    Result := Byte((HexChar(p^) shl 4) + HexChar((p+1)^));
  end;
var
  i: Integer;
begin
  Inc(Src, Ord(Src^ = '{'));
  for i := 0 to 3 do //process D1
    PByte(dest+I)^ := HexByte(Src+(3-i) shl 1);
  if (Src+8)^ <> '-' then InvalidGUID(Char((Src+8)^));
  Inc(src, 9);
  Inc(dest, 4);
  for i := 0 to 1 do //D2, D3
  begin
    PByte(dest)^ := HexByte(src+2);
    PByte(dest+1)^ := HexByte(src);
    Inc(dest, 2);
    if (Src+4)^ <> '-' then InvalidGUID(Char((Src+4)^));
    Inc(src, 5);
  end;
  PByte(dest)^ := HexByte(src);
  PByte(dest+1)^ := HexByte(src+2);
  Inc(dest, 2);
  if (Src+4)^ <> '-' then InvalidGUID(Char((Src+4)^));
  Inc(src, 5);
  for i := 0 to 5 do
  begin
    PByte(dest)^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
  if not ((Src^ = '}') or (Src^ = #0)) then InvalidGUID(Char(Src^));
end;

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): UnicodeString; overload;
var
  P, Dest, PEnd, PFirst: PWideChar;
begin
  Dest := Nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    if (P^=Quote) then begin
      if Dest = nil then
        PFirst := P;
      Inc(NativeUInt(Dest));
    end;
    Inc(P);
  end;
  if Dest = nil then begin
    Result := '';
    System.SetLength(Result, Len+2);
    Dest := Pointer(Result);
    Dest^ := Quote;
    if Len > 0 then begin
      System.Move(Src^, (Dest+1)^, Len shl 1);
      Inc(Dest, Len+1);
    end else
      Inc(Dest);
    Dest^ := Quote;
    Exit;
  end;
  SetLength(Result, Len + NativeInt(NativeUint(Dest)) + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  P := PFirst;
  repeat
    Inc(P);
    Move(Src^, Dest^, (NativeUInt(P) - NativeUInt(Src)));
    Inc(Dest, (P - Src));
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if P^=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (NativeUInt(PEnd) - NativeUInt(Src)));
  Inc(Dest, PEnd - Src);
  Dest^ := Quote;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function SQLQuotedStr(const S: UnicodeString; Quote: WideChar): UnicodeString;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString;
var
  P, Dest, PEnd, PFirst: PAnsiChar;
begin
  Dest := nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    if (AnsiChar(P^)=Quote) then begin
      if Dest = nil then
        PFirst := P;
      Inc(NativeUInt(Dest));
    end;
    Inc(P);
  end;
  if Dest = nil then begin
    {$IFDEF FPC} Result := '';{$ENDIF}
    System.SetLength(Result, Len+2);
    Dest := Pointer(Result);
    AnsiChar(Dest^) := Quote;
    if Len > 0 then begin
      System.Move(Src^, (Dest+1)^, Len);
      Inc(Dest, Len+1);
    end else
      Inc(Dest);
    AnsiChar(Dest^) := Quote;
    Exit;
  end;
  SetLength(Result, Len + NativeInt(NativeUint(Dest)) + 2);
  Dest := Pointer(Result);
  AnsiChar(Dest^) := Quote;
  Inc(Dest);
  P := PFirst;
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src));
    Inc(Dest, P - Src);
    AnsiChar(Dest^) := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if AnsiChar(P^)=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (PEnd - Src));
  Inc(Dest, PEnd - Src);
  AnsiChar(Dest^) := Quote;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

function SQLQuotedStr(Src: PChar; Len: LengthInt; QuoteLeft, QuoteRight: Char): string;
var
  EscChars: LengthInt;
  pSrc, pSrcEnd, pDest: PChar;
begin
  // Src must not be empty!
  if Len = 0 then
  begin
    // Str = Char+Char compiles to three (!) internal functions so we've to use pointers
    {$IFDEF FPC} Result := '';{$ENDIF}
    SetLength(Result, 2);
    pDest := Pointer(Result);
    pDest^ := QuoteLeft;
    (pDest+1)^ := QuoteRight;
    Exit;
  end;

  EscChars := 0;
  pSrc := Src;
  pSrcEnd := Src + Len - 1;
  // Count chars that should be escaped
  while pSrc <= pSrcEnd do
  begin
    if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      Inc(EscChars);
    Inc(pSrc);
  end;

  pSrc := Src;
  SetLength(Result, Len + EscChars + 2);
  pDest := Pointer(Result);
  pDest^ := QuoteLeft;
  Inc(pDest);

  if EscChars > 0 then
  begin
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      begin
        pDest^ := pSrc^;
        (pDest+1)^ := pSrc^;
        Inc(pDest, 2);
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
      end;
      Inc(pSrc);
    end;
  end
  else
  begin
    Move(pSrc^, pDest^, Len*SizeOf(Char));
    Inc(pDest, Len);
  end;
  pDest^ := QuoteRight;
end;

function SQLQuotedStr(const S: string; QuoteLeft, QuoteRight: Char): string;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), QuoteLeft, QuoteRight);
end;

const
  SUnescapedChar = 'Unescaped char in string "%s"';
  SUnescapedCharAtEnd = 'Unescaped char at the end of the string "%s"';

function SQLDequotedStr(const S: string; QuoteChar: Char): string;
var
  L: LengthInt;
  P: PChar;
begin
  L := Length(S);
  if L <= 1 then
    Result := S
  else begin
    P := Pointer(S);
    if L = 2 then
      if (P^ = QuoteChar) and ((P+1)^ = QuoteChar) then // just quotes
        Result := ''
      else
        Result := S
    else
      Result := SQLDequotedStr(P, L, QuoteChar);
  end;
end;

function SQLDequotedStr(Src: PChar; Len: LengthInt; QuoteChar: Char): string;
var
  EscChars: LengthInt;
  pSrcBegin, pSrcEnd, pSrc, pDest: PChar;
  function CreateEArgumentException: EArgumentException;
  begin
    result := EArgumentException.CreateFmt(SUnescapedChar, [pSrc]);
  end;
begin
  pSrcBegin := Pointer(Src);
  // Input must have at least 2 chars, otherwise it is considered unquoted
  // so return as is
  if Len <= 1 then
  begin
    SetString(Result, Src, Len);
    Exit;
  end;

  pSrcEnd := pSrcBegin + Len - 1;
  // Check if input is quoted and return input as is if not
  if (pSrcBegin^ = QuoteChar) and (pSrcEnd^ = QuoteChar) then
  begin
    // just quotes
    if Len = 2 then
    begin
      Result := '';
      Exit;
    end;
    Inc(pSrcBegin);
    Dec(pSrcEnd);
  end
  else
  begin
    SetString(Result, Src, Len);
    Exit;
  end;

  // Count chars that should be escaped
  pSrc := pSrcBegin;
  EscChars := 0;
  while pSrc < pSrcEnd do
  begin
    if (pSrc^ = QuoteChar) then
      if pSrc^ = (pSrc+1)^ then
      begin
        Inc(EscChars);
        Inc(pSrc, 2);
      end
      else
        raise CreateEArgumentException
    else
      Inc(pSrc);
  end;
  // Check last char (pSrc = pSrcEnd is true here only if previous char wasn't
  // quote or was escaped quote)
  if (pSrc = pSrcEnd) and (pSrc^ = QuoteChar) then
    raise CreateEArgumentException;

  // Input contains some escaped quotes
  if EscChars > 0 then
  begin
    SetLength(Result, Len - EscChars - 2);
    pSrc := pSrcBegin;
    pDest := Pointer(Result);
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteChar) then
        Inc(pSrc);
      pDest^ := pSrc^;
      Inc(pSrc);
      Inc(pDest);
    end;
  end
  else
  // Input contains no escaped quotes
  begin
    SetLength(Result, Len - 2); // Result Length always > 2 here!
    Move(pSrcBegin^, Pointer(Result)^, (Len - 2)*SizeOf(Char));
  end;
end;

procedure SQLDequotedStr(pSrc, pDst: PAnsiChar; var Len: LengthInt; QuoteChar: AnsiChar);
var
  EscChars: LengthInt;
  pSrcBegin, pSrcEnd: PAnsiChar;
  function CreateEArgumentException: EArgumentException;
  begin
    result := EArgumentException.CreateFmt(SUnescapedChar, [pSrc]);
  end;
begin
  pSrcBegin := pSrc;
  // Input must have at least 2 chars, otherwise it is considered unquoted
  // so return as is
  if Len <= 1 then begin
    if Len = 1 then
      pDst^ := pSrc^;
    Exit;
  end;
  pSrcEnd := pSrcBegin + Len - 1;
  // Check if input is quoted and return input as is if not
  if (PByte(pSrc)^ = Byte(QuoteChar)) and (PByte(pSrcEnd)^ = Byte(QuoteChar)) then begin
    // just quotes
    if Len = 2 then begin
      Len := 0;
      Exit;
    end;
    Inc(pSrcBegin);
    Dec(pSrcEnd);
  end else Exit;

  // Count chars that should be escaped
  pSrc := pSrcBegin;
  EscChars := 0;
  while pSrc < pSrcEnd do
    if (PByte(pSrc)^ = Byte(QuoteChar)) then
      if pSrc^ = (pSrc+1)^ then begin
        Inc(EscChars);
        Inc(pSrc, 2);
      end else
        raise CreateEArgumentException
    else
      Inc(pSrc);
  // Check last char (pSrc = pSrcEnd is true here only if previous char wasn't
  // quote or was escaped quote)
  if (pSrc = pSrcEnd) and (PByte(pSrc)^ = Byte(QuoteChar)) then
    raise CreateEArgumentException;
  Len := Len -2;
  // Input contains some escaped quotes
  if EscChars > 0 then begin
    Len := Len - EscChars;
    pSrc := pSrcBegin;
    while pSrc <= pSrcEnd do begin
      if (PByte(pSrc)^ = Byte(QuoteChar)) then
        Inc(pSrc);
      pDst^ := pSrc^;
      Inc(pSrc);
      Inc(pDst);
    end;
  end else  // Input contains no escaped quotes
    Move(pSrcBegin^, pDst^, Len);
end;

function SQLDequotedStr(const S: string; QuoteLeft, QuoteRight: Char): string;
var
  SrcLen, EscChars: LengthInt;
  pSrcBegin, pSrcEnd, pSrc, pDest: PChar;
  function CreateEArgumentException: EArgumentException;
  begin
    result := EArgumentException.CreateFmt(SUnescapedChar, [pSrc]);
  end;
  function CreateEArgumentExceptionAtEnd: EArgumentException;
  begin
    result := EArgumentException.CreateFmt(SUnescapedCharAtEnd, [pSrc]);
  end;
begin
  SrcLen := Length(S);
  pSrcBegin := Pointer(S);
  // Input must have at least 2 chars, otherwise it is considered unquoted
  // so return as is
  if SrcLen <= 1 then
  begin
    Result := S;
    Exit;
  end;

  pSrcEnd := pSrcBegin + SrcLen - 1;
  // Check if input is quoted and return input as is if not
  if (pSrcBegin^ = QuoteLeft) and (pSrcEnd^ = QuoteRight) then
  begin
    // just quotes
    if SrcLen = 2 then
    begin
      Result := '';
      Exit;
    end;
    Inc(pSrcBegin);
    Dec(pSrcEnd);
  end
  else
  begin
    Result := S;
    Exit;
  end;

  // Count chars that should be escaped
  pSrc := pSrcBegin;
  EscChars := 0;
  while pSrc < pSrcEnd do
  begin
    if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      if pSrc^ = (pSrc+1)^ then
      begin
        Inc(EscChars);
        Inc(pSrc, 2);
      end
      else raise CreateEArgumentException
    else
      Inc(pSrc);
  end;
  // Check last char (pSrc = pSrcEnd is true here only if previous char wasn't
  // quote or was escaped quote)
  if (pSrc = pSrcEnd) and ((pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight)) then
    raise CreateEArgumentExceptionAtEnd;

  // Input contains some escaped quotes
  if EscChars > 0 then
  begin
    SetLength(Result, SrcLen - EscChars - 2);
    pSrc := pSrcBegin;
    pDest := Pointer(Result);
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
        Inc(pSrc);
      pDest^ := pSrc^;
      Inc(pSrc);
      Inc(pDest);
    end;
  end
  else
  // Input contains no escaped quotes
  begin
    SetLength(Result, SrcLen - 2); // Result Length always > 2 here!
    Move(pSrcBegin^, Pointer(Result)^, (SrcLen - 2)*SizeOf(Char));
  end;
end;

{$Q-}
{$R-}
function SameText(Val1, Val2: PAnsiChar; Len: LengthInt): Boolean;
var  PEnd: PAnsiChar;
  B: Byte;
begin
  Result := (Len = 0) or (Val1 = Val2);
  if Result then
    Exit;
  PEnd := Val1 + Len -4;
  while Val1 < PEnd do begin//compare 4 Bytes per loop
    if (PCardinal(Val1)^ <> PCardinal(Val2)^) then //equal?
      if PCardinal(Val1)^ and $80808080<>0 then begin //no Ascii quad?
        for B := 0 to 3 do
          if PByteArray(Val1)[B] <> PByteArray(Val2)[B] then
            if (PByteArray(Val1)[B] or $80 <> 0) or (PByteArray(Val2)[B] or $80 <> 0) then //one of both not in ascii range?
              Exit
            else if (PByteArray(Val1)[B] or $20) <> (PByteArray(Val2)[B] or $20) then
              Exit;
      end else if PCardinal(Val1)^ or $20202020 <> PCardinal(Val2)^ or $20202020 then
          Exit;
    Inc(Val1, 4);
    Inc(Val2, 4);
  end;
  Inc(PEnd, 4);
  while Val1 < PEnd do begin
    if (PByte(Val1)^ <> PByte(Val2)^) then //equal binary?
      if (PByte(Val1)^ and $80 <> 0) or (PByte(Val2)^ and $80 <> 0) then //no Ascii byte?
        Exit
      else if PByte(Val1)^ or $20 <> PByte(Val2)^ or $20 then
        Exit;
    Inc(Val1);
    Inc(Val2);
  end;
  Result := True;
end;

function SameText(Val1, Val2: PWideChar; Len: LengthInt): Boolean;
var  PEnd: PWideChar;
  B: Boolean;
begin
  Result := (Len = 0) or (Val1 = Val2);
  if Result then
    Exit;
  PEnd := Val1 + Len -2;
  while Val1 < PEnd do begin//compare 4 Bytes per loop
    if (PCardinal(Val1)^ <> PCardinal(Val2)^) then //equal binary?
      if PCardinal(Val1)^ and $00800080<>0 then begin//no Ascii pair?
        for B := False to True do
          if PWordArray(Val1)[Ord(B)] <> PWordArray(Val2)[Ord(B)] then
            if (PWordArray(Val1)[Ord(B)] or $0080 <> 0) or (PWordArray(Val2)[Ord(B)] or $0080 <> 0) then //one of both not in ascii range?
              Exit
            else if (PWordArray(Val1)[Ord(B)] or $0020) <> (PWordArray(Val2)[Ord(B)] or $0020) then
              Exit;
      end else if PCardinal(Val1)^ or $00200020 <> PCardinal(Val2)^ or $00200020 then
        Exit;
    Inc(Val1, 2);
    Inc(Val2, 2);
  end;
  Inc(PEnd, 2);
  while Val1 < PEnd do begin
    if (PWord(Val1)^ <> PWord(Val2)^) then //equal?
      if (PWord(Val1)^ and $0080 <> 0) or (PWord(Val2)^ and $0080 <> 0) then //no Ascii char?
        Exit
      else if PWord(Val1)^ or $0020 <> PWord(Val2)^ or $0020 then
        Exit;
    Inc(Val1);
    Inc(Val2);
  end;
  Result := True;
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

procedure Trim(var L: NativeUInt; var P: PAnsiChar);
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  L := (PEnd+1)-P;
end;

procedure Trim(var L: NativeUInt; var P: PWideChar);
var PEnd: PWideChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  L := (PEnd+1)-P;
end;

function Trim(P: PAnsiChar; L: LengthInt): RawByteString;
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(P, PEnd-P+1, Result);
end;

function LTrim(P: PAnsiChar; L: LengthInt): RawByteString;
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (PByte(P)^ <= Byte(' ')) do
    Inc(P);
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(P, PEnd-P+1, Result);
end;

function RTrim(P: PAnsiChar; L: LengthInt): RawByteString;
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (PEnd >= P) and (PByte(PEnd)^ <= Byte(' ')) do
    Dec(PEnd);
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(P, PEnd-P+1, Result);
end;

function Trim(P: PAnsiChar): RawByteString;
begin
  Result := Trim(P, StrLen(P));
end;

function Trim(P: PWideChar; L: LengthInt): UnicodeString;
var PEnd: PWideChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  {$IFDEF FPC}Result := '';{$ENDIF}
  System.SetString(Result, P, PEnd-P+1);
end;

function LTrim(P: PWideChar; L: LengthInt): UnicodeString;
var PEnd: PWideChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (PWord(P)^ <= Word(' ')) do
    Inc(P);
  {$IFDEF FPC}Result := '';{$ENDIF}
  System.SetString(Result, P, PEnd-P+1);
end;

function RTrim(P: PWideChar; L: LengthInt): UnicodeString;
var PEnd: PWideChar;
begin
  PEnd := P + L -1;
  while (PEnd >= P) and (PWord(PEnd)^ <= Word(' ')) do
    Dec(PEnd);
  {$IFDEF FPC}Result := '';{$ENDIF}
  System.SetString(Result, P, PEnd-P+1);
end;

{$IF defined(UNICODE) and not defined(WITH_UNITANSISTRINGS)}
function Trim(const Value: RawByteString): RawByteString;
var P, PEnd: PAnsiChar;
begin
  if Pointer(Value) = nil then begin
    Result := Value;
    Exit;
  end;
  P := Pointer(Value);
  PEnd := P + Length(Value) -1;
  if (Ord(P^) > Ord(' ')) and (Ord(PEnd^) > Ord(' ')) then
    Result := Value
  else begin
    while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
      Inc(P);
    while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
      Dec(PEnd);
    ZSetString(P, PEnd-P+1, Result);
  end;
end;

function LowerCase(const Value: RawByteString): RawByteString;
var Len: Integer;
  Dst, Src, PEnd: PByte;
  Ch: Byte;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  if Len > 0 then begin
    Dst := Pointer(Result);
    Src := Pointer(Value);
    PEnd := Dst+Len;
    while Dst < Pend do begin
      Ch := PByte(Src)^;
      if (ch >= Ord('A')) and (ch <= Ord('Z')) then
        Ch := Ch or $20;
      Dst^ := Ch;
      Inc(Dst);
      Inc(Src);
    end;
  end;
end;

function UpperCase(const Value: RawByteString): RawByteString;
var Len: Integer;
  Dst, Src, PEnd: PByte;
  Ch: Byte;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  if Len > 0 then begin
    Dst := Pointer(Result);
    Src := Pointer(Value);
    PEnd := Dst+Len;
    while Dst < Pend do begin
      Ch := PByte(Src)^;
      if (ch >= Ord('a')) and (ch <= Ord('z')) then
        Ch := Ch xor $20;
      Dst^ := Ch;
      Inc(Dst);
      Inc(Src);
    end;
  end;
end;

{$IFEND}

{$IFNDEF UNICODE}
function Trim(const Value: UnicodeString): UnicodeString; overload;
var
  P, PEnd: PWideChar;
begin
  if Pointer(Value) = nil then begin
    Result := '';
    Exit;
  end;
  P := Pointer(Value);
  PEnd := P + Length(Value) -1;
  if (Ord(P^) > Ord(' ')) and (Ord(PEnd^) > Ord(' ')) then
    Result := Value
  else begin
    while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
      Inc(P);
    while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
      Dec(PEnd);
    System.SetString(Result, P, PEnd-P+1);
  end;
end;
{$ENDIF}

function StreamFromData(Buffer: Pointer; Size: Integer): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(Buffer^, Size);
  Result.Position := 0;
end;

function StreamFromData(const AString: UnicodeString): TMemoryStream;
begin
  Result := StreamFromData(Pointer(AString), Length(AString)*SizeOf(WideChar));
end;

function StreamFromData(const Bytes: TBytes): TMemoryStream;
begin
  Result := StreamFromData(Pointer(Bytes), Length(Bytes));
end;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
function StreamFromData(const AString: RawByteString): TMemoryStream;
begin
  Result := StreamFromData(Pointer(AString), Length(AString));
end;
{$ENDIF}

{$R-} {$Q-}
procedure Currency2Bcd(const Value: Currency; var Result: TBCD);
var V2: UInt64;
  iRec: Int64Rec absolute V2;
  Negative: Boolean;
begin
  Negative := Value < 0;
  if Negative
  then V2 := UInt64(-PInt64(@Value)^)
  else V2 := UInt64(PInt64(@Value)^);
  if IRec.Hi = 0
  then ScaledOrdinal2Bcd(iRec.Lo, 4, Result, Negative)
  else ScaledOrdinal2Bcd(V2,      4, Result, Negative);
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

const
  SignSpecialPlacesArr: Array[Boolean] of Byte = ($00, $80);

procedure ScaledOrdinal2Bcd(const Value: Int64; Scale: Byte; var Result: TBCD); overload;
var V2: UInt64;
  iRec: Int64Rec absolute V2;
  Negative: Boolean;
begin
  {$R-} {$Q-}
  Negative := Value < 0;
  if Negative
  then V2 := UInt64(-Value)
  else V2 := UInt64(Value);
  if IRec.Hi = 0
  then ScaledOrdinal2Bcd(iRec.Lo, Scale, Result, Negative)
  else ScaledOrdinal2Bcd(V2,      Scale, Result, Negative);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

 {$R-} {$Q-}
procedure ScaledOrdinal2Bcd(Value: UInt64; Scale: Byte; var Result: TBCD; Negative: Boolean);
var V2: UInt64;
  B, Precision, Digits, FirstPlace, LastPlace, Place: Cardinal; //D7 int overflow -> reason unknown
  LastWasZero: Boolean;
label Done;
begin
  Digits := GetOrdinalDigits(Value);
  if (Digits = 1) and (byte(Value) = 0) then begin
    PCardinal(@Result.Precision)^ := ZInitZeroBCD;
    Exit;
  end;
  if Digits < Scale then begin
    PInt64(@Result.Fraction[0])^ := 0; //clear these nibbles
    PInt64(@Result.Fraction[7])^ := 0;
    FirstPlace := (Scale - Digits) shr 1;
    Precision := Scale;
  end else begin
    Precision := Digits;
    FirstPlace := 0;
  end;
  LastWasZero := True;
  LastPlace := ((Precision - 1) shr 1);
  PCardinal(@Result.Fraction[LastPlace+1])^ := 0; //some compilers read over lastnibble (FPC+XE10.3x64 f.e.)
  if (Precision and 1) = 1 then
    if Digits = 1 then begin
      Result.Fraction[LastPlace] := Byte(Value) shl 4;
      goto Done;
    end else begin
      v2 := Value div 10;
      B := (Value-(V2*10));
      if (Scale > 0) and (B = 0) then begin //special case 4FPC where BCD compare fails
        Dec(Scale);
        Dec(Precision);
        Dec(LastPlace);
      end else begin
        Result.Fraction[LastPlace] := Byte(B) shl 4;
        LastWasZero := False;
      end;
      Value := V2;
    end;
  for Place := LastPlace-(Precision and 1) downto FirstPlace +1 do begin
    v2 := Value div 100;
    B := Value-(V2*100);
    LastWasZero := LastWasZero and (B = 0);
    if (Place = LastPlace) and (Scale >= 2) and LastWasZero then begin //special case 4FPC where BCD compare fails
      Dec(Scale, 2);
      Dec(Precision, 2);
      Dec(LastPlace);
    end else
      Result.Fraction[Place] := ZBase100Byte2BcdNibbleLookup[Byte(B)];
    Value := V2;
  end;
  Result.Fraction[FirstPlace] := ZBase100Byte2BcdNibbleLookup[Byte(Value)];
  if (Scale >= 1) and (LastPlace > FirstPlace) and (
       (Precision and 1 = 1) and ((Result.Fraction[LastPlace] shr 4  ) = 0) or
       (Precision and 1 = 0) and ((Result.Fraction[LastPlace] and $0F) = 0)) then begin
      Dec(Scale);
      Dec(Precision);
    end;
Done:
  Result.SignSpecialPlaces := SignSpecialPlacesArr[Negative] or Scale;
  Result.Precision := Precision;
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$R-} {$Q-}
procedure ScaledOrdinal2Bcd(Value: Integer; Scale: Byte; var Result: TBCD);
begin
  if Value < 0
  then ScaledOrdinal2Bcd(Cardinal(-Value), Scale, Result, True)
  else ScaledOrdinal2Bcd(Cardinal(Value), Scale, Result, False);
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$R-} {$Q-}
procedure ScaledOrdinal2Bcd(Value: Cardinal; Scale: Byte; var Result: TBCD; Negative: Boolean);
var Precision, V2, B, LastPlace, FirstPlace, Place, Digits: Cardinal; //B: D7 int overflow -> reason unknown
  LastWasZero: Boolean;
label Done;
begin
  Digits := GetOrdinalDigits(Value);
  if (Digits = 1) and (byte(Value) = 0) then begin
    PCardinal(@Result.Precision)^ := ZInitZeroBCD;
    Exit;
  end;
  if Digits < Scale then begin
    PInt64(@Result.Fraction[0])^ := 0; //clear these nibbles
    FirstPlace := (Scale - Digits) shr 1;
    Precision := Scale;
  end else begin
    Precision := Digits;
    FirstPlace := 0;
  end;
  LastWasZero := True;
  LastPlace := ((Precision - 1) shr 1);
  PCardinal(@Result.Fraction[LastPlace+1])^ := 0; //some compilers read over lastnibble (FPC+XE10.3x64 f.e.)
  if Precision and 1 = 1 then
    if Digits = 1 then begin
      Result.Fraction[LastPlace] := Byte(Value) shl 4;
      goto done;
    end else begin
      v2 := Value div 10;
      B := Value{%H-}-(V2*10);
      if (Scale > 0) and (B = 0) then begin //special case 4FPC where BCD compare fails
        Dec(Scale);
        Dec(Precision);
        Dec(LastPlace);
      end else begin
        Result.Fraction[LastPlace] := Byte(B) shl 4;
        LastWasZero := False;
      end;
      Value := V2;
    end;
  for Place := LastPlace-(Precision and 1) downto FirstPlace +1 do begin
    v2 := Value div 100;
    B := Value{%H-}-(V2*100);
    LastWasZero := LastWasZero and (B = 0);
    if (Place = LastPlace) and (Scale >= 2) and LastWasZero then begin //special case 4FPC where BCD compare fails
      Dec(Scale, 2);
      Dec(Precision, 2);
      Dec(LastPlace);
    end else
      Result.Fraction[Place] := ZBase100Byte2BcdNibbleLookup[Byte(B)];
    Value := V2;
  end;
  Result.Fraction[FirstPlace] := ZBase100Byte2BcdNibbleLookup[Byte(Value)];
  if (Scale >= 1) and (LastPlace > FirstPlace) and (
       (Precision and 1 = 1) and ((Result.Fraction[LastPlace] shr 4  ) = 0) or
       (Precision and 1 = 0) and ((Result.Fraction[LastPlace] and $0F) = 0)) then begin
      Dec(Scale);
      Dec(Precision);
    end;
Done:
  Result.SignSpecialPlaces := SignSpecialPlacesArr[Negative] or Scale;
  Result.Precision := Precision;
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

procedure ScaledOrdinal2Bcd(Value: SmallInt; Scale: Byte; var Result: TBCD); overload;
begin
  {$R-} {$Q-}
  if Value < 0
  then ScaledOrdinal2Bcd(Word(-Value), Scale, Result, True)
  else ScaledOrdinal2Bcd(Word(Value), Scale, Result, False);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{$R-} {$Q-}
procedure ScaledOrdinal2Bcd(Value: Word; Scale: Byte; var Result: TBCD; Negative: Boolean);
var Precision, V2, B, LastPlace, FirstPlace, Digits: Word; //B: D7 int overflow -> reason unknown
  LastWasZero: Boolean;
label Done;
begin
  Digits := GetOrdinalDigits(Value);
  if (Digits = 1) and (byte(Value) = 0) then begin
    PCardinal(@Result.Precision)^ := ZInitZeroBCD;
    Exit;
  end;
  if Digits < Scale then begin
    PCardinal(@Result.Fraction[0])^ := 0; //clear these nibbles
    FirstPlace := (Scale - Digits) shr 1;
    Precision := Scale;
  end else begin
    Precision := Digits;
    FirstPlace := 0;
  end;
  LastPlace := ((Precision - 1) shr 1);
  PCardinal(@Result.Fraction[LastPlace+1])^ := 0; //some compilers read over lastnibble (FPC+XE10.3x64 f.e.)
  LastWasZero := True;
  if Precision and 1 = 1 then
    if Digits = 1 then begin
      Result.Fraction[LastPlace] := Byte(Value) shl 4;
      goto done;
    end else begin
      v2 := Value div 10;
      B := Value{%H-}-(V2*10);
      if (Scale > 0) and (B = 0) then begin //special case 4FPC where BCD compare fails
        Dec(Scale);
        Dec(Precision);
        Dec(LastPlace);
      end else begin
        Result.Fraction[LastPlace] := Byte(B) shl 4;
        LastWasZero := False;
      end;
      Value := V2;
    end;
  //unrolled version we're comming from a smallInt/word with max precision of 5
  if Digits >= 4 then begin
    v2 := Value div 100;
    B := Value-(V2*100);
    LastWasZero := LastWasZero and (B = 0);
    if LastWasZero and (Scale >= 2) then begin
      Result.Fraction[FirstPlace] := ZBase100Byte2BcdNibbleLookup[Byte(V2)];
      Dec(Scale, 2);
      Dec(Precision, 2);
      Dec(LastPlace);
    end else
      PWord(@Result.Fraction[FirstPlace])^ := ZBase100Byte2BcdNibbleLookup[Byte(V2)]+ZBase100Byte2BcdNibbleLookup[Byte(B)] shl 8;
  end else
    Result.Fraction[FirstPlace] := ZBase100Byte2BcdNibbleLookup[Byte(Value)];
  if (Scale >= 1) and (LastPlace > FirstPlace) and (
       (Precision and 1 = 1) and ((Result.Fraction[LastPlace] shr 4  ) = 0) or
       (Precision and 1 = 0) and ((Result.Fraction[LastPlace] and $0F) = 0)) then begin
      Dec(Scale);
      Dec(Precision);
    end;
Done:
  Result.SignSpecialPlaces := SignSpecialPlacesArr[Negative] or Scale;
  Result.Precision := Precision;
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString;
var PSrc, PEnd: PAnsiChar;
  POld: PAnsiChar absolute OldPattern;
  PNew: PAnsiChar absolute NewPattern;
  PRes, PResEnd: PAnsiChar;
  L, iPos, LOld, LNew: Integer;
begin
  L := Length(Source);
  LOld := Length(OldPattern);
  PSrc := Pointer(Source);
  iPos := PosEx(POld, PSrc, LOld, L, 1);
  if iPos = 0 then begin
    Result := Source;
    Exit;
  end;
  PEnd := PSrc+L-1;
  if PNew = nil
  then LNew := 0
  else LNew := Length(NewPattern);
  Assert(LNew <= LOld);
  SetLength(Result, L);
  PRes := Pointer(Result);
  PResEnd := PRes+L;
  repeat
    Move(PSrc^, PRes^, iPos-LOld+1);
    Inc(PRes, (iPos-LOld+1));
    if LNew > 0 then begin
      Move(PNew^, PRes^, LNew);
      Inc(PRes, LNew);
    end;
    Inc(PSrc, (iPos-1+LOld));
    IPos := PosEx(POld, PSrc, LOld, (PEnd-PSrc)+1, 1);
  until IPos = 0;
  if (PSrc <= PEnd) then begin
    Move(PSrc^, PRes^, (PEnd-PSrc+1));
    Inc(Pres, (PEnd-PSrc+1));
  end;
  SetLength(Result, L-(PResEnd-PRes));
end;

function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: UnicodeString): UnicodeString;
var PSrc, PEnd: PWideChar;
  POld: PWideChar absolute OldPattern;
  PNew: PWideChar absolute NewPattern;
  PRes, PResEnd: PWideChar;
  L, iPos, LOld, LNew: Integer;
begin
  L := Length(Source);
  LOld := Length(OldPattern);
  PSrc := Pointer(Source);
  iPos := PosEx(POld, PSrc, LOld, L, 1);
  if iPos = 0 then begin
    Result := Source;
    Exit;
  end;
  PEnd := PSrc+L-1;
  if PNew = nil
  then LNew := 0
  else LNew := Length(NewPattern);
  Assert(LNew <= LOld);
  SetLength(Result, L);
  PRes := Pointer(Result);
  PResEnd := PRes+L;
  repeat
    Move(PSrc^, PRes^, (iPos-LOld+1) shl 1);
    Inc(PRes, (iPos-LOld+1));
    if LNew > 0 then begin
      Move(PNew^, PRes^, LNew shl 1);
      Inc(PRes, LNew);
    end;
    Inc(PSrc, (iPos-1+LOld));
    IPos := PosEx(POld, PSrc, LOld, (PEnd-PSrc)+1, 1);
  until IPos = 0;
  if (PSrc <= PEnd) then begin
    Move(PSrc^, PRes^, (PAnsiChar(PEnd)-PAnsiChar(PSrc)+2));
    Inc(Pres, (PEnd-PSrc+1));
  end;
  SetLength(Result, L-(PResEnd-PRes));
end;

function StringReplaceAll_CS_GToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString; overload;
var Cnt, LOld, LNew, LSrc, I, OffSet: Integer;
  PSrc, PNewP, PDest, PEnd: PAnsiChar;
begin
  Cnt := 0;
  OffSet := 1;
  LOld := Length(OldPattern);
  while true do begin
    OffSet := ZFastCode.PosEx(OldPattern, Source, OffSet);
    if OffSet = 0 then
      Break;
    Inc(Cnt);
    Inc(OffSet, LOld);
  end;
  if Cnt = 0 then begin
    Result := Source;
    Exit;
  end;
  LNew := Length(NewPattern);
  Assert(LNew >= LOld);
  Result := EmptyRaw;
  I := (LNew - LOld);
  I := ((I * Cnt) + Length(Source));
  SetLength(Result, I);
  PDest := Pointer(Result);
  PSrc  := Pointer(Source);
  LSrc := Length(Source);
  PEnd := PSrc+LSrc;
  PNewP := Pointer(NewPattern);
  OffSet := ZFastCode.PosEx(OldPattern, Source, 1);
  I := OffSet-1;
  repeat
    Move(PSrc^, PDest^, I);
    Inc(PDest, Cardinal(I));
    Inc(PSrc, Cardinal(I+LOld));
    Move(PNewP^, PDest^, LNew);
    Inc(PDest, Cardinal(LNew));
    OffSet := OffSet + LOld;
    I := OffSet;
    OffSet := ZFastCode.PosEx(OldPattern, Source, OffSet);
    I := OffSet - I;
  until OffSet = 0;
  if (PSrc <= PEnd) then
    Move(PSrc^, PDest^, (PAnsiChar(PEnd)-PAnsiChar(PSrc)));
end;

function StringReplaceAll_CS_GToEQ(const Source, OldPattern, NewPattern: UnicodeString): UnicodeString;
var Cnt, LOld, LNew, LSrc, I, OffSet: Integer;
  PSrc, PNewP, PDest, PEnd: PWideChar;
begin
  Cnt := 0;
  OffSet := 1;
  LOld := Length(OldPattern);
  while true do begin
    OffSet := ZFastCode.PosEx(OldPattern, Source, OffSet);
    if OffSet = 0 then
      Break;
    Inc(Cnt);
    Inc(OffSet, LOld);
  end;
  if Cnt = 0 then begin
    Result := Source;
    Exit;
  end;
  LNew := Length(NewPattern);
  Assert(LNew >= LOld);
  Result := '';
  I := (LNew - LOld);
  I := ((I * Cnt) + Length(Source));
  SetLength(Result, I);
  PDest := Pointer(Result);
  PSrc  := Pointer(Source);
  LSrc := Length(Source);
  PEnd := PSrc+LSrc;
  PNewP := Pointer(NewPattern);
  OffSet := ZFastCode.PosEx(OldPattern, Source, 1);
  I := (OffSet-1);
  repeat
    Move(PSrc^, PDest^, I shl 1);
    Inc(PDest, Cardinal(I));
    Inc(PSrc, Cardinal(I+LOld));
    Move(PNewP^, PDest^, LNew shl 1);
    Inc(PDest, Cardinal(LNew));
    OffSet := OffSet + LOld;
    I := OffSet;
    OffSet := ZFastCode.PosEx(OldPattern, Source, OffSet);
    I := OffSet - I;
  until OffSet = 0;
  if (PSrc <= PEnd) then
    Move(PSrc^, PDest^, (PAnsiChar(PEnd)-PAnsiChar(PSrc)));
end;

procedure RaiseBcd2OrdException(P, PEnd: PChar);
var S: String;
begin
  SetString(s, P, PEnd-P);
  raise EConvertError.CreateResFmt(@SInvalidInteger, [S])
end;

procedure BCD2Int64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD; out Result: Int64);
var Buf: array[0..MaxFMTBcdFractionSize+2] of Char;
 P, PEnd, PFail: PChar;
begin
  P := @Buf[0];
  PEnd := P +{$IFDEF UNICODE}BCDToUni{$ELSE}BcdToRaw{$ENDIF}(Value, P, '.');
  PFail := PEnd; //save
  Result := {$IFDEF UNICODE}ValUnicodeInt64{$ELSE}ValRawInt64{$ENDIF}(P, PFail);
  if (PFail <> PEnd) and (PFail^ <> '.') then //BCD truncation ? or overrun
    RaiseBcd2OrdException(P, PEnd);
end;

function BCD2Int64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): Int64;
begin
  BCD2Int64(Value, Result);
end;

procedure BCD2UInt64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD; out Result: UInt64);
begin
  Result := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(BcdToStr(Value), Uint64(0));
end;

function BCD2UInt64({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): UInt64;
var Buf: array[0..MaxFMTBcdFractionSize+2] of Char;
 P, PEnd, PFail: PChar;
begin
  P := @Buf[0];
  PEnd := P +{$IFDEF UNICODE}BcdToUni{$ELSE}BcdToRaw{$ENDIF}(Value, P, '.');
  PFail := PEnd; //save
  Result := {$IFDEF UNICODE}ValUnicodeUInt64{$ELSE}ValRawUInt64{$ENDIF}(P, PFail);
  if (PFail <> PEnd) and (PFail^ <> '.') then //BCD truncation ? or overrun
    RaiseBcd2OrdException(P, PEnd);
end;

function TryUniToBcd(const Value: UnicodeString; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean; overload;
begin
  Result := TryUniToBCD(Value, BCD, {$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}FormatSettings.DecimalSeparator{$ELSE}'.'{$ENDIF});
end;

function TryUniToBcd(const Value: UnicodeString; var Bcd: TBcd; DecimalSep: Char): Boolean;
begin
  if Value = ''
  then Result := False
  else Result := TryUniToBcd(Pointer(Value), Length(Value), BCD, DecimalSep);
end;

function TryUniToBcd(Buf: PWideChar; Len: LengthInt; var Bcd: TBcd; DecimalSep: Char): Boolean;
var
  Negative: Boolean;
  DecimalPos, Exp: Integer;
  Pos: Byte;
  P, PEnd: PWideChar;
label Fail, CompExp, Finalize, CheckPos;
begin
  FillChar(Bcd, SizeOf(Bcd), #0);
  PEnd := Buf+Len;
  // Skip leading white chars
  while (Buf < PEnd) and ((PWord(Buf)^ <= Ord(' ')) and (Byte(PWord(Buf)^) in [Ord(' '), Ord(#6), Ord(#9), Ord(#10), Ord(#13), Ord(#14)])) do Inc(Buf);
  Negative := Buf^ = '-';
  if Negative or (PWord(Buf)^ = Ord('+')) then Inc(Buf);
  // Skip trailing white chars
  while (PEnd > Buf) and ((PWord(PEnd-1)^ <= Ord(' ')) and (Byte(PWord(PEnd-1)^) in [Ord(' '), Ord(#6), Ord(#9), Ord(#10), Ord(#13), Ord(#14), Ord('0')]))  do Dec(PEnd);
  P := PEnd; //remainder for Exponent or if no decimal sep is given
  Pos := 0;
  DecimalPos := -1;
  if Buf = PEnd then
    if PByte(PEnd)^ = Ord('0')
    then goto Finalize
    else goto Fail
  else Inc(PEnd, Ord(PWord(PEnd)^ = Ord('0')));
  // Skip leading zeroes
  while (Buf < PEnd) and (PWord(Buf)^ = Ord('0')) do Inc(Buf);
  while Buf < PEnd do begin
    if PWord(Buf)^ = Ord(DecimalSep) then begin
      if DecimalPos <> -1 then
        goto Fail;
      if Pos = 0 then
        Inc(Pos)
      else if (Pos = 1) and (PWord(Buf-1)^ = Ord('0')) then //strict left padd the bcd for fpc else BCDCompare fails even if the value is correct
        Dec(Pos);
      DecimalPos := Pos;
      while (PEnd > Buf) and (PWord(PEnd-1)^ = Ord('0')) do Dec(PEnd); //and pad trailing zeroes away 123.90000000000
      Inc(Buf);
      if (Buf = PEnd) then
        goto Finalize;
    end else if (PWord(Buf)^ or $20 = Ord('e')) then
      goto CompExp;

    if (PWord(Buf)^ < Ord('0')) or (PWord(Buf)^ > Ord('9')) or
       ((Pos = 64) and (DecimalPos = -1)) then
      goto Fail;
    if Pos < 64 then begin
      if (Pos and 1) = 0
      then Bcd.Fraction[Pos shr 1] := Byte(Ord(Buf^) - Ord('0')) * $10
      else Bcd.Fraction[Pos shr 1] := (Bcd.Fraction[Pos shr 1] and $F0) + Byte(Ord(Buf^) - Ord('0'));
      Inc(Pos);
    end;
    Inc(Buf);
  end;
  goto CheckPos;
CompExp:
  PEnd := P;
  Exp := ValUnicodeInt(Buf+1, p);
  if pEnd <> P
  then goto Fail;
  if DecimalPos < 0 then begin
    DecimalPos := Pos;
    Inc(Pos);
  end;
  if Exp < 0 then begin
    if DecimalPos < -Exp then begin
      bcd.Precision := Pos;
      bcd.SignSpecialPlaces := Pos -1;
      Exp := Pos - Exp;
      Pos := Pos - DecimalPos;
      if Exp > MaxFMTBcdFractionSize then
        goto Fail;
      if not NormalizeBcd(bcd, bcd, Exp, Pos)
      then goto Fail;
      Pos := Exp;
    end else
      Inc(DecimalPos, Exp);
  end else begin
    inc(DecimalPos, Exp);
    if DecimalPos > Pos then begin
      Pos := DecimalPos;
      DecimalPos := -1;
    end;
  end;
  goto Finalize;
CheckPos:
  if Buf <> PEnd then
    goto Fail;
Finalize:
  if Pos = 0 then begin //zero
    Bcd.Precision := 1;//10 = delphi/FPC defaults for a zero BCD are disturbing the FPC users
    Bcd.SignSpecialPlaces := 0; //2 = delphi/FPC defaults for a zero BCD are disturbing the FPC users
  end else begin
    if Pos > MaxFMTBcdFractionSize
    then goto Fail;
    Bcd.Precision := Pos;
    if DecimalPos = -1
    then Bcd.SignSpecialPlaces := 0
    else Bcd.SignSpecialPlaces := Byte(Pos - DecimalPos);
    if Negative then
      Bcd.SignSpecialPlaces := Bcd.SignSpecialPlaces or $80;
  end;
  Result := True;
  Exit;
Fail:
  Result := False;
end;

function TryRawToBcd(const Value: RawByteString; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean;
begin
  Result := TryRawToBCD(Value, BCD, {$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}FormatSettings.DecimalSeparator{$ELSE}'.'{$ENDIF});
end;

function TryRawToBcd(const Value: RawByteString; var Bcd: TBcd; DecimalSep: Char): Boolean; overload;
begin
  if Value = EmptyRaw
  then Result := False
  else Result := TryRawToBcd(Pointer(Value), Length(Value), BCD, DecimalSep);
end;

function TryRawToBcd(Buf: PAnsiChar; Len: LengthInt; var Bcd: TBcd; DecimalSep: Char): Boolean;
var
  Negative: Boolean;
  DecimalPos, Exp: Integer;
  Pos: Byte;
  P, PEnd: PAnsiChar;
label Fail, CompExp, Finalize, CheckPos;
begin
  FillChar(Bcd, SizeOf(Bcd), #0);
  PEnd := Buf+Len;
  // Skip leading white chars
  while (Buf < PEnd) and (Byte(PByte(Buf)^) in [Ord(' '), Ord(#6), Ord(#9), Ord(#10), Ord(#13), Ord(#14)]) do Inc(Buf);
  Negative := Buf^ = '-';
  if Negative or (PByte(Buf)^ = Ord('+')) then Inc(Buf);
  Pos := 0;
  DecimalPos := -1;
  // Skip trailing white chars
  while (PEnd > Buf) and (PByte(PEnd-1)^ in [Ord(' '), Ord(#6), Ord(#9), Ord(#10), Ord(#13), Ord(#14)])  do Dec(PEnd);
  P := PEnd; //remainder for Exponent or if no decimal sep is given
  if Buf = PEnd then
    if PByte(PEnd)^ = Ord('0')
    then goto Finalize
    else goto Fail
  else Inc(PEnd, Ord(PByte(PEnd)^ = Ord('0')));
  while Buf < PEnd do begin
    if PByte(Buf)^ = Ord(DecimalSep) then begin
      if DecimalPos <> -1 then
        goto Fail;
      if Pos = 0 then
        Inc(Pos)
      else if (Pos = 1) and (PByte(Buf-1)^ = Ord('0')) then //strict left padd the bcd for fpc else BCDCompare fails even if the value is correct
        Dec(Pos);
      DecimalPos := Pos;
      while (PEnd > Buf) and (PByte(PEnd-1)^ = Ord('0')) do Dec(PEnd); //and pad trailing zeroes away 123.90000000000
      Inc(Buf);
      if (Buf = PEnd) then
        goto Finalize;
    end else if (PByte(Buf)^ or $20 = Ord('e')) then
      goto CompExp;

    if (PByte(Buf)^ < Ord('0')) or (PByte(Buf)^ > Ord('9')) or
       ((Pos = 64) and (DecimalPos = -1)) then
      goto Fail;
    if Pos < 64 then begin
      if (Pos and 1) = 0
      then Bcd.Fraction[Pos shr 1] := (PByte(Buf)^ - Ord('0')) * $10
      else Bcd.Fraction[Pos shr 1] := (Bcd.Fraction[Pos shr 1] and $F0) + (PByte(Buf)^ - Ord('0'));
      Inc(Pos);
    end;
    Inc(Buf);
  end;
  goto CheckPos;
CompExp:
  PEnd := P;
  Exp := ValRawInt(Buf+1, p);
  if pEnd <> P
  then goto Fail;
  if DecimalPos < 0 then begin
    DecimalPos := Pos;
    Inc(Pos);
  end;
  if Exp < 0 then begin
    if DecimalPos < -Exp then begin
      bcd.Precision := Pos;
      bcd.SignSpecialPlaces := Pos -1;
      Exp := Pos - Exp;
      Pos := Pos - DecimalPos;
      if Exp > MaxFMTBcdFractionSize then //bcd overflow?
        goto Fail;
      if not NormalizeBcd(bcd, bcd, Exp, Pos)
      then goto Fail;
      Pos := Exp;
    end else
      Inc(DecimalPos, Exp);
  end else begin
    inc(DecimalPos, Exp);
    if DecimalPos > Pos then begin
      Pos := DecimalPos;
      DecimalPos := -1;
    end;
  end;
  goto Finalize;
CheckPos:
  if Buf <> PEnd then
    goto Fail;
Finalize:
  if Pos = 0 then begin //zero
    Bcd.Precision := 1;//10 = delphi/FPC defaults for a zero BCD are disturbing the FPC users
    Bcd.SignSpecialPlaces := 0; //2 = delphi/FPC defaults for a zero BCD are disturbing the FPC users
  end else begin
    if Pos > MaxFMTBcdFractionSize
    then goto Fail;
    Bcd.Precision := Pos;
    if DecimalPos = -1
    then Bcd.SignSpecialPlaces := 0
    else Bcd.SignSpecialPlaces := Pos - DecimalPos;
    if Negative then
      Bcd.SignSpecialPlaces := Bcd.SignSpecialPlaces or $80;
  end;
  Result := True;
  Exit;
Fail:
  Result := False;
end;

function TryStr2BCD(const Value: String; var Bcd: TBcd{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): Boolean;
begin
  if Value <> ''
  then Result := {$IFDEF UNICODE}TryUniToBcd{$ELSE}TryRawToBcd{$ENDIF}(
      Pointer(Value), Length(Value), BCD, {$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}FormatSettings.DecimalSeparator{$ELSE}'.'{$ENDIF})
  else begin
    FillChar(BCD, SizeOf(BCD), #0);
    Result := False;
  end;
end;

function Str2BCD(const Value: String{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}; const FormatSettings: TFormatSettings{$ENDIF}): TBCD;
begin
  if not TryStr2BCD(Value, Result{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}{%H-},FormatSettings{$ENDIF}) then
    raise EBcdException.CreateFmt(SInvalidBcdValue, [Value]);
end;

procedure Double2BCD(const Value: Double; var Result: TBCD);
var Buffer: array[0..63] of Char;
begin
  {$IFDEF UNICODE}
  TryUniToBCD(@Buffer[0], FloatToSqlUnicode(Value, @Buffer[0]), Result, '.');
  {$ELSE}
  TryRawToBCD(@Buffer[0], FloatToSqlRaw(Value, @Buffer[0]), Result, '.');
  {$ENDIF}
end;

function GetPacketBCDOffSets({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
  Value: TBCD; out PNibble, PLastNibble: PAnsiChar;
  out Precision, Scale: Word; out GetFirstBCDHalfByte: Boolean): Boolean;
var Digit: Word;
  B: Boolean;
begin
  pNibble := @Value.Fraction[0];
  Precision := Value.Precision;
  B := Precision and 1 = 1;
  Scale :=  Value.SignSpecialPlaces and 63;
  pLastNibble := pNibble + (Precision-1) shr 1;
  GetFirstBCDHalfByte := True;
  Result := False;
  { padd leading zeroes away }
  while (Precision > 1) and (Precision > Scale) do begin
    if PByte(pNibble)^ = 0 then begin
      Dec(Precision);
      Result := True;
      if (Precision > 1) and (Precision > Scale) then begin
        Inc(PNibble);
        Dec(Precision);
        Continue;
      end else GetFirstBCDHalfByte := False;
    end else if (PByte(pNibble)^ shr 4) = 0 then begin
      GetFirstBCDHalfByte := False;
      Dec(Precision);
      Result := True;
    end;
    Break;
  end;
  { padd trailing zeroes away }
  while (pLastNibble >= pNibble ) and (Scale > 0) do begin
    if B
    then Digit := (PByte(pLastNibble)^ shr 4)
    else Digit := (PByte(pLastNibble)^ and $0F);
    if Digit = 0 then begin
      if B then Dec(pLastNibble);
      Dec(Scale);
      Dec(Precision);
      Result := True;
      if Precision = 0 then
        Precision := 1;
    end else
      Break;
    B := not B;
  end;
end;

procedure ZPackBCDToLeft(var Value: TBCD; var PNibble, PLastNibble: PAnsiChar;
  Precision, Scale: Word; GetFirstBCDHalfByte: Boolean);
var PFirstNibble: PAnsiChar;
  B: Byte absolute GetFirstBCDHalfByte;
begin
  PFirstNibble := @Value.Fraction[0];
  if (PFirstNibble < PNibble) or not GetFirstBCDHalfByte then begin
    if GetFirstBCDHalfByte then //byte move
      while PNibble <= PLastNibble do begin
        PFirstNibble^ := PNibble^;
        Inc(PNibble);
        Inc(PFirstNibble);
      end
    else begin {move half nibbles:
       /\/\/\/\/\/\...
      /\/\/\/\/\/... }
      B := PByte(PNibble)^ and $0F; //save second half byte
      while (PNibble < PLastNibble) do begin
        PByte(PFirstNibble)^ := (B shl 4) or (PByte(PNibble+1)^ shr 4);
        Inc(PNibble);
        B := PByte(PNibble)^ and $0F; //second half byte
        Inc(PFirstNibble);
      end;
      PByte(PFirstNibble)^ := (B shl 4);
    end;
  end;
  PNibble := @Value.Fraction;
  PLastNibble := PNibble + (Precision shr 1);
  Value.Precision := Precision;
  GetFirstBCDHalfByte := Value.SignSpecialPlaces and (1 shl 7) <> 0;
  Value.SignSpecialPlaces := Scale or SignSpecialPlacesArr[GetFirstBCDHalfByte];
end;

function ZBCDCompare(var Value1, Value2: TBCD): Integer;
var PNibble1, PNibble2, PLastNibble1, PLastNibble2, PNibble, PLastNibble: PAnsiChar;
    Prec1, Prec2, Scale1, Scale2: Word;
    GetFB1, GetFB2, Negative: Boolean;
    s1, s2: Integer;
begin
  Result := Ord(Value2.SignSpecialPlaces and (1 shl 7) <> 0) - Ord(Value1.SignSpecialPlaces and (1 shl 7) <> 0);
  if Result = 0 then begin //both positve or negative
    Negative := Value1.SignSpecialPlaces and (1 shl 7) <> 0;
    if GetPacketBCDOffSets(Value1, pNibble1, pLastNibble1, Prec1, Scale1, GetFB1) then
      ZPackBCDToLeft(Value1, pNibble1, pLastNibble1, Prec1, Scale1, GetFB1);
    if GetPacketBCDOffSets(Value2, pNibble2, pLastNibble2, Prec2, Scale2, GetFB2) then
      ZPackBCDToLeft(Value2, pNibble2, pLastNibble2, Prec2, Scale2, GetFB2);
    {determine digits before fractions start: }
    s1 := Integer(Prec1)-Scale1;
    s2 := Integer(Prec2)-Scale2;
    if Negative
    then Result := Ord(s1 < s2) - Ord(s1 > s2)
    else Result := Ord(s1 > s2) - Ord(s1 < s2);
    GetFB1 := Result = 0;
    s1 := S1 + Ord(Prec1=Scale1);
    s2 := s2 + Ord(Prec2=Scale2);
    GetFB2 := S1 = s2;
    if (Result = 0) or GetFB2 then begin //both have same amount of digits before a comma(if there is one)
      if Prec1 <= Prec2 then begin
        PNibble := PNibble1;
        PLastNibble := PLastNibble1;
      end else begin
        PNibble := PNibble2;
        PLastNibble := PLastNibble2;
      end;
      while PNibble <= PLastNibble do begin
        if GetFB1 = GetFB2 then begin
          s1 := ZBcdNibble2Base100ByteLookup[PByte(PNibble1)^];
          s2 := ZBcdNibble2Base100ByteLookup[PByte(PNibble2)^];
        end else begin
          if (Prec1 = Scale1) then begin
            s1 := (PByte(PNibble1)^ shr 4);
            s2 := ZBcdNibble2Base100ByteLookup[PByte(PNibble2)^];
          end;
          if (Prec2 = Scale2) then begin
            s1 := ZBcdNibble2Base100ByteLookup[PByte(PNibble1)^];
            s2 := (PByte(PNibble2)^ shr 4);
          end;
        end;
        if Negative
        then Result := Ord(s1 < s2) - Ord(s1 > s2)
        else Result := Ord(s1 > s2) - Ord(s1 < s2);
        if Result <> 0 then
          Exit;
        Inc(PNibble);
        Inc(pNibble1);
        Inc(pNibble2);
      end;
      {both of them have equal digits for the smalles comparable range
       so finally compare which one has more digits total }
      Result := Ord(Prec1 > Prec2) - Ord(Prec1 < Prec2);
    end;
  end;
end;

function BcdToSQLRaw({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): RawByteString;
var Digits: array[0..MaxFMTBcdFractionSize-1+1{sign}+1{dot}] of AnsiChar;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(PAnsiChar(@Digits[0]), BcdToRaw(Value, @Digits[0], '.'),Result)
end;

function BcdToRaw({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Bcd: TBcd;
  Buf: PAnsiChar; DecimalSep: Char): LengthInt;
var
  PBuf, pCN{current nibble}, pDN{decimal nibble}, pLN{last bibble}, pFN{firts nibble}: PAnsiChar;
  Scale, Precision, i: Integer;
label zero;
begin
  PByte(Buf)^ := Ord('0');
  Precision := Bcd.Precision;
  if Precision = 0 then goto zero;
  Scale := Bcd.SignSpecialPlaces and $3F;
  if (Precision > MaxFMTBcdFractionSize) or (Scale > Precision) then
    raise EBcdOverflowException.Create(SBcdOverflow);
  pCN := @Bcd.Fraction[0]; //set first byte
  pLN := pCN + ((Precision -1) shr 1); //pin last nibble byte
  pDN := pLN+1-((Scale+Ord(Scale > 0)+(Precision and 1)) shr 1); //pin middle nibble or after decimal sep
  while (pLN > pDN) and (PByte(pLN)^ = 0) do Dec(pLN);//skip trailing zeroes
  while (pCN < pDN) and (PByte(pCN)^ = 0) do Inc(pCN);//skip leading zeroes
  if ((pCN = pLN) and (PByte(pCN)^ = 0)) or ((pCN > pLN) and (PByte(pLN)^ = 0)) then begin
zero: Result := 1;
    Exit;
  end;
  PBuf := Buf;
  pFN := pCN; //remainder
  if (Bcd.SignSpecialPlaces and $80) = $80 then begin
    pWord(Buf)^ := Ord('-')+Ord('0') shl 8;
    Inc(Buf);
  end;
  for i := Ord(Scale = 0) to Ord(pFN < pDN) do begin
    if (pCN = pDN) then begin //decimal byte in midde or before of next nibbles
      if (Precision and 1) = (Scale and 1) then begin
        Inc(Buf, Ord(pCN = pFN));
        PByte(Buf)^ := Ord(DecimalSep);
        pWord(Buf+1)^ := ZBcdNibble2DwoDigitLookupW[PByte(pCN)^];
        Inc(Buf, 3);
      end else begin
        pWord(Buf)^ := ZBcdNibble2DwoDigitLookupW[PByte(pCN)^];
        if (pDN = pLN) and (PByte(Buf+1)^ = Ord('0'))
        then Inc(Buf)
        else begin
          PByte(Buf+2)^ := PByte(Buf+1)^;
          PByte(Buf+1)^ := Ord(DecimalSep);
          Inc(Buf, 3);
        end;
      end;
      if (pCN <> pLN)
      then pDN := pLN+1
      else Break;
    end else if (pCN > pFN) or ((PByte(pCN)^ shr 4) <> 0) then begin //regulare double digit
      pWord(Buf)^ := ZBcdNibble2DwoDigitLookupW[PByte(pCN)^];
      Inc(Buf, 2);
    end else begin  // first half nibble
      PByte(Buf)^ := Ord('0') + (PByte(pCN)^ and $0f);
      Inc(Buf);
    end;
    inc(pCN);
    while (pCN < pDN) do begin
      pWord(Buf)^ := ZBcdNibble2DwoDigitLookupW[PByte(pCN)^];
      Inc(Buf, 2);
      inc(pCN);
    end;
  end;
  if (pByte(Buf-1)^ = Ord ('0')) and ((Scale > 0) or ((Precision and 1 = 1) and (PByte(pLN)^ and $0f = 0))) then Dec(Buf);
  Result := (Buf-PBuf);
end;

function BcdToUni({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Bcd: TBcd; Buf: PWideChar; DecimalSep: Char): LengthInt;
var
  pCN{current nibble}, pDN{decimal nibble}, pLN{last bibble}, pFN{firts nibble}: PAnsiChar;
  PBuf: PWideChar;
  Scale, Precision, i: Integer;
label zero;
begin
  PWord(Buf)^ := Ord('0');
  Precision := Bcd.Precision;
  if Precision = 0 then goto zero;
  Scale := Bcd.SignSpecialPlaces and $3F;
  if (Precision > MaxFMTBcdFractionSize) or (Scale > Precision) then
    raise EBcdOverflowException.Create(SBcdOverflow);
  pCN := @Bcd.Fraction[0]; //set first byte
  pLN := pCN + ((Precision -1) shr 1); //pin last nibble byte
  pDN := pLN+1-((Scale+Ord(Scale > 0)+(Precision and 1)) shr 1); //pin middle nibble or after decimal sep
  while (pLN > pDN) and (PByte(pLN)^ = 0) do Dec(pLN);//skip trailing zeroes
  while (pCN < pDN) and (PByte(pCN)^ = 0) do Inc(pCN);//skip leading zeroes
  if ((pCN = pLN) and (PByte(pCN)^ = 0)) or ((pCN > pLN) and (PByte(pLN)^ = 0)) then begin
zero: Result := 1;
    Exit;
  end;
  PBuf := Buf;
  pFN := pCN; //remainder
  if (Bcd.SignSpecialPlaces and $80) = $80 then begin
    PCardinal(Buf)^ := Ord('-')+Ord('0') shl 16;
    Inc(Buf);
  end;
  for i := Ord(Scale = 0) to Ord(pFN < pDN) do begin
    if (pCN = pDN) then begin //decimal byte in midde or before of next nibbles
      if (Precision and 1) = (Scale and 1) then begin
        Inc(Buf, Ord(pCN = pFN));
        PWord(Buf)^ := Ord(DecimalSep);
        PCardinal(Buf+1)^ := ZBcdNibble2DwoDigitLookupLW[PByte(pCN)^];
        Inc(Buf, 3);
      end else begin
        PCardinal(Buf)^ := ZBcdNibble2DwoDigitLookupLW[PByte(pCN)^];
        if (pDN = pLN) and (PWord(Buf+1)^ = Ord('0'))
        then Inc(Buf)
        else begin
          PWord(Buf+2)^ := PWord(Buf+1)^;
          PWord(Buf+1)^ := Ord(DecimalSep);
          Inc(Buf, 3);
        end;
      end;
      if (pCN <> pLN)
      then pDN := pLN+1
      else Break;
    end else if (pCN > pFN) or ((PByte(pCN)^ shr 4) <> 0) then begin //regulare double digit
      PCardinal(Buf)^ := ZBcdNibble2DwoDigitLookupLW[PByte(pCN)^];
      Inc(Buf, 2);
    end else begin  // first half nibble
      PWord(Buf)^ := Ord('0') + (PByte(pCN)^ and $0f);
      Inc(Buf);
    end;
    inc(pCN);
    while (pCN < pDN) do begin
      PCardinal(Buf)^ := ZBcdNibble2DwoDigitLookupLW[PByte(pCN)^];
      Inc(Buf, 2);
      inc(pCN);
    end;
  end;
  if (pByte(Buf-1)^ = Ord ('0')) and ((Scale > 0) or ((Precision and 1 = 1) and (PByte(pLN)^ and $0f = 0))) then Dec(Buf);
  Result := (Buf-PBuf);
end;

function RawToBCD(Value: PAnsiChar; Len: LengthInt): TBCD;
begin
  if not TryRawToBCD(Value, Len, Result{%H-}, '.') then
    raise EBcdException.CreateFmt(SInvalidBcdValue, [Value]);
end;

function RawToBCD(const Value: RawByteString): TBCD;
begin
  Result := RawToBCD(Pointer(Value), Length(Value));
end;

function BcdToSQLUni({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD): UnicodeString;
var Digits: array[0..MaxFMTBcdFractionSize-1+1{sign}+1{dot}] of WideChar;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  System.SetString(Result, PWideChar(@Digits[0]), BcdToUni(Value, @Digits[0], '.'));
end;

function UniToBCD(Value: PWideChar; Len: LengthInt): TBCD;
begin
  if not TryUniToBCD(Value, Len, Result{%H-}, '.') then
    raise EBcdException.CreateFmt(SInvalidBcdValue, [Value]);
end;

function UniToBCD(const Value: UnicodeString): TBCD;
begin
  Result := UniToBCD(Pointer(Value), Length(Value));
end;


{ for a better code align -> move out of method }
{$IFNDEF CPU64}
const CIntTable: array[TCurrRoundToScale]      of Cardinal = (10000, 1000, 100, 10, 1);
{$ENDIF}
const CInt64Table:    array[TCurrRoundToScale]  of Int64   = (10000, 1000, 100, 10, 1);
const PosHalfModulos: array [TCurrRoundToScale] of Integer = ( 4445,  445,  45,  5, 0);
const NegHalfModulos: array [TCurrRoundToScale] of Integer = (-4445, -445, -45, -5, 0);

function RoundCurrTo(const Value: Currency; Scale: TCurrRoundToScale): Currency;
var Modulo: Integer;
  {$IFNDEF CPU64}
  sI64Rec: Int64Rec absolute Value;
  dI64Rec: Int64Rec absolute Result;
  {$ENDIF}
  s64: Int64 absolute Value;
  d64: Int64 absolute Result;
begin
  if Scale < 4 then begin
    {$IFNDEF CPU64} //push trunc performance of positive tiny values
    if sI64Rec.Hi = 0 then begin
      dI64Rec.hi := sI64Rec.Lo div CIntTable[Scale];
      dI64Rec.lo := dI64Rec.hi *   CIntTable[Scale];
      Modulo := sI64Rec.Lo - dI64Rec.lo;
      dI64Rec.Hi := 0;
    end else {$ENDIF} begin
      d64 := s64 div CInt64Table[Scale];
      d64 := d64 *   CInt64Table[Scale];
      Modulo := s64 - d64;
    end;
    if Scale > 0 then
      if Modulo < 0 then begin
        if Modulo <= NegHalfModulos[Scale] then
          d64 := d64 - CInt64Table[Scale];
      end else if Modulo >= PosHalfModulos[Scale] then
        d64 := d64 + CInt64Table[Scale];
  end else
    Result := Value
end;

const HalfFractModulos:     array [TFractionRoundToScale] of Cardinal = ( 444444445, 44444445,  4444445,  444445,  44445,  4445,  445, 45, 5,0);
const FractionRoundSummant: array [TFractionRoundToScale] of Cardinal = (1000000000,100000000, 10000000, 1000000, 100000, 10000, 1000,100,10,1);

function RoundNanoFractionTo(const Value: Cardinal; Scale: TFractionRoundToScale): Cardinal;
var Modulo: Cardinal;
begin
  if Scale = 0 then
    Result := 0
  else if (Scale < 9) and (Value > 0) then begin
    Result := Value div FractionLength2NanoSecondMulTable[Scale];
    Result := Result * FractionLength2NanoSecondMulTable[Scale];
    Modulo := Value - Result;
    if (Scale > 0) and (Modulo >= HalfFractModulos[Scale]) then
      Result := Result + FractionRoundSummant[Scale];
  end else Result := Value;
end;

function RoundNanoFractionToMillis(const Value: Cardinal): Word;
var F, Modulo: Cardinal;
begin
  Result := Value div NanoSecsPerMSec;
  F :=  Result * NanoSecsPerMSec;
  Modulo := Value - F;
  if Modulo >= HalfFractModulos[3] then
    Result := Result + 1;
end;

procedure ZRoundBCD(var Value: TBCD; Scale: TZBCDScale; Out Precision: Word);
var PNibble, PLastNibble: PAnsiChar;
  BcdScale, BcdPrecision: Word;
  Negative: Boolean;
  Current, Prior, Remainder: Byte;
begin
  BcdScale := Scale;
  if GetPacketBCDOffSets(Value, PNibble, PLastNibble, Precision, BcdScale, Negative) then
    ZPackBCDToLeft(Value, pNibble, pLastNibble, Precision, BcdScale, Negative);
  if BcdScale <= Scale then Exit; //left packing did the job
  Negative := (Value.SignSpecialPlaces and $80) = $80;
  Remainder := 0;
  Value.SignSpecialPlaces := 9;
  BcdPrecision := Precision;
  if Precision < MaxFMTBcdFractionSize-1 then
    PByte(PLastNibble+1)^ := 0; //clear overlong nibble in case Remainder is not zero after loop
  while (BcdPrecision > 0) and ((BcdScale > Scale) or Boolean(Remainder)) do begin
    if Odd(BcdPrecision) then begin
      Current := PByte(PLastNibble)^ shr 4;
      PByte(PLastNibble)^ := 0;
      Dec(PLastNibble);
      Prior := PByte(PLastNibble)^ and $0F;
    end else begin
      Current := PByte(PLastNibble)^ and $0F;
      Prior := PByte(PLastNibble)^ shr 4;
    end;
    Current := Current + Remainder;
    if (Current >= 5) then
      if (Prior = 9)
      then Remainder := 1
      else begin
        Remainder := 0;
        Inc(Prior);
        if Odd(BcdPrecision) then begin
          Current := PByte(PLastNibble)^ shr 4;
          PByte(PLastNibble)^ := (Current shl 4) or Prior;
        end else
          PByte(PLastNibble)^ := Prior shl 4;
      end;
    if (BcdScale > 0) and (Precision >= BcdScale) then begin
      Dec(Precision);
      Dec(BcdScale);
    end;
    Dec(BcdPrecision)
  end;
  if Remainder <> 0 then begin//all walues have been rounded to 10
    BcdScale := 0;
    PByte(PNibble)^ := 1 shl 4;
    Inc(Precision);
  end;
  Value.Precision := Precision;
  Value.SignSpecialPlaces := SignSpecialPlacesArr[Negative] or BcdScale;
end;

function CharPos_CI(C: Char; const Str: string): NativeInt;
var P, PStart, PEnd: PChar;
  L: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
begin
  Result := 0;
  PStart := Pointer(Str);
  P := PStart;
  PEnd := P + Length(Str);
  L := Ord(C) or $20; //lower
  while P < PEnd do begin
    if ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(P)^ or $20) = L then begin
      Result := (P-PStart)+1;
      Break;
    end;
    Inc(P);
  end;
end;

{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure BoolConstFiller;
var B: Boolean;
begin
  for B := False to True do begin
    BoolStrIntsRaw[B] := UnicodeStringToASCII7(BoolStrInts[B]);
    BoolStrsRaw[B] := UnicodeStringToASCII7(BoolStrsW[B]);
    YesNoStrsRaw[B] := UnicodeStringToASCII7(YesNoStrs[b]);
  end;
  SetLength(r8Zeros, 8);
  r8Zeros[0] := Ord('0');
  r8Zeros[1] := Ord('0');
  r8Zeros[2] := Ord('0');
  r8Zeros[3] := Ord('0');
  r8Zeros[4] := Ord('0');
  r8Zeros[5] := Ord('0');
  r8Zeros[6] := Ord('0');
  r8Zeros[7] := Ord('0');
  r8Zeros[8] := Ord('0');
  r8Zeros[9] := Ord('0');
end;
{$ENDIF}

procedure HexFiller;
var
  I{$IFDEF NO_RAW_HEXTOBIN}, v{$ENDIF}: Byte;
  Hex: String;
begin
  for i := Low(Byte) to High(Byte) do
  begin
    Hex := IntToHex(I, 2);
    {$IFDEF UNICODE}
    TwoDigitLookupHexLW[i] := PCardinal(Pointer(Hex))^;
    TwoDigitLookupHexW[i] := PWord(Pointer(RawByteString(Hex)))^;
    {$ELSE}
    TwoDigitLookupHexW[i] := PWord(Pointer(Hex))^;
    TwoDigitLookupHexLW[i] := PCardinal(Pointer(UnicodeString(Hex)))^;
    {$ENDIF}
  end;
  {$IFDEF NO_RAW_HEXTOBIN}
  //copy from Arnaud Bouchez syncommons.pas
  Fillchar(ConvertHexToBin[0],SizeOf(ConvertHexToBin),255); // all to 255
  V := 0;
  for i := ord('0') to ord('9') do begin
    ConvertHexToBin[i] := v;
    inc(v);
  end;
  for i := ord('A') to ord('F') do begin
    ConvertHexToBin[i] := v;
    ConvertHexToBin[i+(ord('a')-ord('A'))] := v;
    inc(v);
  end;
  {$ENDIF}
end;

procedure BcdNibbleLookupFiller;
var i, n: Byte;
begin
  for i := 0 to 99 do begin
    N := ((i div 10) shl 4) + (i mod 10);
    ZBase100Byte2BcdNibbleLookup[i] := N;
    ZBcdNibble2Base100ByteLookup[N] := i;
    ZBcdNibble2DwoDigitLookupW[N] := ZFastCode.TwoDigitLookupW[I];
    ZBcdNibble2DwoDigitLookupLW[N] := ZFastCode.TwoDigitLookupLW[I];
  end;
end;

initialization;
  BcdNibbleLookupFiller;
  HexFiller;  //build up lookup table
{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolConstFiller; //build bool consts
{$ENDIF}
end.
