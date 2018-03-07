{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZSysUtils;

interface

{$I ZCore.inc}

uses
  Variants, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZMessages, ZCompatibility;

type
  {** Modified comaprison function. }
  TZListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  {** Modified list of pointers. }
  TZSortedList = class (TList)
  public
    procedure Sort(Compare: TZListSortCompare);
  end;

const
  StrFalse = 'False';
  StrTrue = 'True';
  BoolStrInts: array[Boolean] of string = ('0', '1');
  BoolStrIntsRaw: array[Boolean] of RawByteString = ('0', '1');
  BoolStrs: array[Boolean] of string = (StrFalse, StrTrue);
  BoolStrsRaw: array[Boolean] of RawByteString = (RawByteString(StrFalse), RawByteString(StrTrue));
  BoolStrsW: array[Boolean] of ZWideString = (ZWideString(StrFalse), ZWideString(StrTrue));

var
  TwoDigitLookupHexW: packed array[Byte] of Word;
  TwoDigitLookupHexLW: packed array[Byte] of LongWord;

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(const Delimiters, Str: string): Integer;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(const Delimiters, Str: string): Integer;

{**
  Compares two Buffers with fixed length
  @param P1 first Pointer
  @param P2 seconds Pointer
  @return <code>Integer</code> if the memory equals else return PByte(P1)-PByte(B2)
}
function ZMemLComp(P1, P2: PAnsiChar; Len: Cardinal): Integer;

{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChars
  @param P2 seconds PWideChars
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;

{**
  Checks is the string starts with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: ZWidestring): Boolean; overload;

{**
  Checks is the string starts with substring.
  @param Str a AnsiString/RawByteString to be checked.
  @param SubStr a AnsiString/RawByteString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}

function StartsWith(const Str, SubStr: RawByteString): Boolean; overload;

{**
  Checks is the string ends with substring.
  @param Str a AnsiString/RawByteString to be checked.
  @param SubStr a AnsiString/RawByteString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: RawByteString): Boolean; overload;

{**
  Checks is the string ends with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: ZWideString): Boolean; overload;

{**
  Converts SQL PAnsiChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix and ThousandSeparators
  @param Str an SQL PAnsiChar with comma or dot delimiter.
  @param Def a default value if the PAnsiChar can not be converted.
  @return a converted value or Def if conversion did fail.
}
function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; Len: Integer = 0): Extended; overload;
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; var Result: Extended; Len: Integer = 0); overload;
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; var Result: Currency; Len: Integer = 0); overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double; var Result: Double; Len: Integer = 0); overload;
{$IFEND}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single; var Result: Single; Len: Integer = 0); overload;

{**
  Converts SQL PWideChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix and ThousandSeparators
  @param Str an SQL PWideChar with comma or dot delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion did fail.
}
function SQLStrToFloatDef(Value: PWideChar; const Def: Extended; Len: Integer = 0): Extended; overload;
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Extended; var Result: Extended; Len: Integer = 0); overload;
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; var Result: Currency; Len: Integer = 0); overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double; var Result: Double; Len: Integer = 0); overload;
{$IFEND}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single; var Result: Single; Len: Integer = 0); overload;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
function BufferToStr(Buffer: PWideChar; Length: LongInt): string; overload;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
function BufferToStr(Buffer: PAnsiChar; Length: LongInt): string; overload;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a TBytes retrived from the buffer.
}
function BufferToBytes(Buffer: Pointer; Length: LongInt): TBytes; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @param CheckInt Check for "0" char too?
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean; overload;

{**
  Converts a string into boolean value.
  @param Str a PAnsiChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: ZWideString; const CheckInt: Boolean = True): Boolean; overload;

{**
  Converts a string into boolean value.
  @param Str a PWideChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToUnicodeEx(Value: Boolean): ZWideString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts a boolean into RawByteString value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToRawEx(Value: Boolean): RawByteString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{$IFDEF ENABLE_POSTGRESQL}
{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(const Str: string): Boolean;
{$ENDIF}

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(const Str, Delimiters: string): TStrings;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; const Str, Delimiters: string);

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; const Delimiter: string): string;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;

{**
  Converts SQL string with '.' delimiter into a float value.
  @param Str a string value to be converted.
  @return a converted float value.
}
function SQLStrToFloat(const Str: String): Extended;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(const Str, Delimiter: string): TStrings;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);

{**
  Converts bytes into a AnsiString representation.
  @param Value an array of bytes to be converted.
  @return a converted AnsiString.
}
function BytesToStr(const Value: TBytes): RawByteString;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: AnsiString): TBytes; overload;

{$IFDEF WITH_RAWBYTESTRING}
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: UTF8String): TBytes; overload;
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: RawByteString): TBytes; overload;
{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
{$ENDIF}
function StrToBytes(const Value: WideString): TBytes; overload;
{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TBytes; overload;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TBytes): Variant;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(const Value: Variant): TBytes;

{**
  Converts Ansi SQL Date/Time to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(const Value: string): TDateTime;

{**
  Converts Ansi SQL Date (DateFormat) to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat. May be nil;
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL Date (DateFormat) to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat. May be nil;
  @return a decoded TDateTime value.
}
function UnicodeSQLDateToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL Time (hh:nn:ss or hh:mm:nn.zzz or TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL Time (hh:nn:ss or hh:mm:nn.zzz or TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeStampToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeStampToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts DateTime value to a rawbyteString
  @param Value a TDateTime value.
  @param DateFormat the result format.
  @param DateFromatLen the length of the format pattern
  @return a formated RawByteString with DateFormat pattern.
}
function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;

{**
  Converts DateTime value to a WideString/UnicodeString
  @param Value a TDateTime value.
  @param DateFormat the result format.
  @param DateFromatLen the length of the format pattern
  @return a formated RawByteString with DateFormat pattern.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;

{**
  Converts DateTime value into a RawByteString with format pattern
  @param Value a TDateTime value.
  @param TimeFormat the result format.
  @return a formated RawByteString with Time-Format pattern.
}
function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;

{**
  Converts DateTime value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param TimeFormat the result format.
  @return a formated WideString/UnicodeString with Time-Format pattern.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;

{**
  Converts DateTime value to a RawByteString
  @param Value a TDateTime value.
  @param TimeStampFormat the result format.
  @return a formated RawByteString in TimeStamp-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;

{**
  Converts DateTime value to a WideString/UnicodeString
  @param Value a TDateTime value.
  @param TimeStampFormat the result format.
  @return a formated WideString/UnicodeString in TimeStamp-Format pattern.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;

{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime; WithMMSec: Boolean = False): string;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(const Value: string): string;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(const Value: string): string;

{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
function ReplaceChar(const Source, Target: Char; const Str: string): string;

{**
  Remove chars in the string.
  More obvious and ~35 times faster than StringReplace(Str, ToRemove, '')
  @param ToRemove a char to search and remove.
  @param Str a source string.
  @return a string with removed chars.
}
function RemoveChar(ToRemove: Char; const Str: string): string;

{**
  Append a string to another string separating the added string with delimiter.
  Correctly processes cases where any of the arguments is empty
  @param Str source string to append to. If empty, resulting Str value will be AddStr
  @param AddStr string to append. If empty, Str won't be changed
  @param Delimiter string to separate AddStr from Str
}
procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);

{**
  Break a string into two parts according to appearance of Delimiter.
  @param Str source string
  @param Delimiter separator string; Str=Left+Delimiter+Right
  @param Left left part of Str from the start to the first Delimiter.
    Equals to Str if Str doesn't contain Delimiter
  @param Right left part of Str from the first Delimiter to the end.
    Empty if Str doesn't contain Delimiter
}
procedure BreakString(const Str, Delimiter: String; var Left, Right: String);

{**
  Decodes a Full Version Value encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);

{**
  Encodes major, minor and subversion (revision) values in this format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  For example, 4.1.12 is returned as 4001012.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;

{**
  Formats a Zeos SQL Version format to X.Y.Z where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param SQLVersion an integer
  @return Formated Zeos SQL Version Value.
}
function FormatSQLVersion( const SQLVersion: Integer ): String;

function ASCII7ToUnicodeString(const Src: RawByteString): ZWideString; overload;
function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): ZWideString; overload;
function UnicodeStringToASCII7(const Src: ZWideString): RawByteString; overload;
function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString; overload;

function FloatToRaw(const Value: Extended): RawByteString;
function FloatToSqlRaw(const Value: Extended): RawByteString;
function FloatToUnicode(const Value: Extended): ZWideString;
function FloatToSqlUnicode(const Value: Extended): ZWideString;

procedure ZBinToHex(Buffer, Text: PAnsiChar; const Len: LengthInt); overload;
procedure ZBinToHex(Buffer: PAnsiChar; Text: PWideChar; const Len: LengthInt); overload;

procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar; const SetTerm: Boolean = False); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const SetTerm: Boolean = False); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

function GUIDToRaw(const GUID: TGUID): RawByteString; overload;
function GUIDToRaw(const Bts: TBytes): RawByteString; overload;
function GUIDToRaw(Buffer: Pointer; Len: NativeInt): RawByteString; overload;

function GUIDToUnicode(const GUID: TGUID): ZWideString; overload;
function GUIDToUnicode(const Bts: TBytes): ZWideString; overload;
function GUIDToUnicode(Buffer: Pointer; Len: NativeInt): ZWideString; overload;

procedure ValidGUIDToBinary(Src, Dest: PAnsiChar); overload;
procedure ValidGUIDToBinary(Src: PWideChar; Dest: PAnsiChar); overload;

function SQLQuotedStr(const S: ZWideString; Quote: WideChar): ZWidestring; overload;
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): ZWidestring; overload;
function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString; overload;
function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString; overload;

implementation

uses DateUtils, StrUtils, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF WITH_RTLCONSTS_SInvalidGuidArray}RTLConsts,{$ENDIF}SysConst,
  ZFastCode;

var
  // Local copy of current FormatSettings with '.' as DecimalSeparator and
  // ',' as ThousandSeparator
  FSSqlFloat: TFormatSettings;

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(const Delimiters, Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := 1 to Length(Delimiters) do
  begin
    Index := ZFastCode.Pos(Delimiters[I], Str);
    if (Index > 0) and ((Index < Result) or (Result = 0)) then
      Result := Index;
  end;
end;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(const Delimiters, Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := Length(Str) downto 1 do
  begin
    Index := ZFastCode.Pos(Str[I], Delimiters);
    if (Index > 0) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChar
  @param P2 seconds PWideChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(Pointer(P1), Pointer(P2), Len shl 1) = 0;
end;

{**  EH:
  Compares two Pointers with a maximum len
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>Integer</code> 0 if the memory at P1 and P2 are equal, otherwise
    return the byte difference
}
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
    if (PLongWord(P1)^ <> PLongWord(P2)^) then goto Fail;
    if (PLongWord(P1+4)^ <> PLongWord(P2+4)^) then goto Fail;
    if (PLongWord(P1+8)^ <> PLongWord(P2+8)^) then goto Fail;
    if (PLongWord(P1+12)^ <> PLongWord(P2+12)^) then goto Fail;
    if (PLongWord(P1+16)^ <> PLongWord(P2+16)^) then goto Fail;
    if (PLongWord(P1+20)^ <> PLongWord(P2+20)^) then goto Fail;
    if (PLongWord(P1+24)^ <> PLongWord(P2+24)^) then goto Fail;
    if (PLongWord(P1+28)^ <> PLongWord(P2+28)^) then goto Fail;
    {$ENDIF}
    Inc(P1, 32); Inc(P2, 32);
  end;
  while P1+8 < PEnd do //compare 8 Bytes per loop
  begin
    {$IFDEF CPUX64}
    if (PUInt64(P1)^ <> PUInt64(P2)^) then goto Fail; //not overflow save so let's check the bytes
    {$ELSE}
    if (PLongWord(P1)^ <> PLongWord(P2)^) then goto Fail;
    if (PLongWord(P1+4)^ <> PLongWord(P2+4)^) then goto Fail;
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

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(P1, P2, Len) = 0;
end;

{**
  Checks is the string starts with substring.
  @param Str a AnsiString/RaweByteString to be checked.
  @param SubStr a AnsiString/RaweByteString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
begin
  LenSubStr := Length(SubStr);
  if SubStr = '' then
    Result := True
   else
    if LenSubStr <= Length(Str) then
      Result := MemLCompAnsi(PAnsiChar(Str), PAnsiChar(SubStr), LenSubStr)
    else
      Result := False;
end;

{**
  Checks is the string starts with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: ZWideString): Boolean;
var
  LenSubStr: Integer;
begin
  LenSubStr := Length(SubStr);
  if SubStr = '' then
    Result := True
  else
    if LenSubStr <= Length(Str) then
      Result := MemLCompUnicode(PWideChar(Str), PWideChar(SubStr), LenSubStr)
    else
      Result := False;
end;

{**
  Checks is the string ends with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: ZWideString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = '' then
    Result := False // act like Delphi's AnsiEndsStr()
  else
  begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr then
      Result := MemLCompUnicode(PWideChar(Str) + LenStr - LenSubStr,
         PWidechar(SubStr), LenSubStr)
    else
      Result := False;
  end;
end;

{**
  Checks is the string ends with substring.
  @param Str a AnsiString/RawbyteString to be checked.
  @param SubStr a AnsiString/RawbyteString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = '' then
    Result := False // act like Delphi's AnsiEndsStr()
  else
  begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr then
      Result := MemLCompAnsi(PAnsiChar(Str) + LenStr - LenSubStr,
         PAnsiChar(SubStr), LenSubStr)
    else
      Result := False;
  end;
end;

{**
  Converts SQL AnsiString/RawByteString into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix and ThousandSeparators
  @param Str an SQL AnsiString/RawByteString with comma or dot delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  Len: Integer = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result{%H-}, Len);
end;

function CurrToRawBuff(Value: PAnsiChar; Buf: PByteArray; Len: Integer): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
  Result := True;
  DotPos := 0; CommaPos := 0; ValidCount := 0; InvalidPos := 0;
  FillChar(Buf^, Len+1, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  for i := 0 to Len-1 do
    case (Value+i)^ of
      '0'..'9':
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      ',':
        if ((I-InvalidPos-DotPos) = 3) or ((DotPos=0) and (ValidCount > 0)) then //all others are invalid!
        begin
          CommaPos := I;
          if DotPos = 0 then
            Inc(ValidCount)
          else //align result four Byte block and overwrite last ThousandSeparator
            PLongWord(@Buf[DotPos-1])^ := PLongWord(@Buf[DotPos])^;
          Buf[ValidCount-1] := Ord('.');
        end
        else
          Goto Fail;
      '-', '+':
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      '.':
        begin
          if DotPos > 0 then //previously init so commapos can't be an issue here
          begin
            if (I-InvalidPos-DotPos) = 3 then //all others are invalid!
            begin
              PLongWord(@Buf[DotPos-1])^ := PLongWord(@Buf[DotPos])^;
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
          if (Value+i)^ = ' ' then //641,22 $ f.e.
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
  var Result: Extended; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawExt(Pointer(Value), '.', InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and ((Value+Len*Ord(Len>0)-1)^ in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, ',', Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), '.', Def, Result)
        else
          Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency;
  var Result: Currency; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawDbl(Pointer(Value), '.', InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and ((Value+Len*Ord(Len>0)-1)^ in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, ',', Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), '.', Def, Result)
        else
          Result := Def;
      end;
  end;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double;
  var Result: Double; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawDbl(Pointer(Value), '.', InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and ((Value+Len*Ord(Len>0)-1)^ in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, ',', Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), '.', Def, Result)
        else
          Result := Def;
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single;
  var Result: Single; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawSin(Pointer(Value), '.', InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and ((Value+Len*Ord(Len>0)-1)^ in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, ',', Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), '.', Def, Result)
        else
          Result := Def;
      end;
  end;
end;

{**
  Converts SQL Unicode String into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix and ThousandSeparators
  @param Str an SQL AnsiString/RawByteString with comma or dot delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloatDef(Value: PWideChar; const Def: Extended;
  Len: Integer = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result{%H-}, Len);
end;

function CurrToUnicodeBuf(Value: PWideChar; Buffer: PWordArray; CodePoints: Integer): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
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
            PLongWord(@Buffer[DotPos-1])^ := PLongWord(@Buffer[DotPos])^; //Move first four byte block
            PLongWord(@Buffer[DotPos+1])^ := PLongWord(@Buffer[DotPos+2])^; //Move second four byte block
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
              PLongWord(@Buffer[DotPos-1])^ := PLongWord(@Buffer[DotPos])^; //Move first four byte block
              PLongWord(@Buffer[DotPos+1])^ := PLongWord(@Buffer[DotPos+2])^; //Move second four byte block
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
  var Result: Extended; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeExt(PWordArray(Value), WideChar('.'), InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (AnsiChar((Value+Len*Ord(Len>0)-1)^) in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value)
          {$ELSE}
          Len := Length(Value)
          {$ENDIF}
        else
          if (Len < InvalidPos) and ((Value+InvalidPos-1)^ = ' ') then Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency;
  var Result: Currency; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeDbl(PWordArray(Value), WideChar('.'), InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (AnsiChar((Value+Len*Ord(Len>0)-1)^) in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
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
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double;
  var Result: Double; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeDbl(PWordArray(Value), WideChar('.'), InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (AnsiChar((Value+Len*Ord(Len>0)-1)^) in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
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
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result);
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single;
  var Result: Single; Len: Integer = 0);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeSin(PWordArray(Value), WideChar('.'), InvalidPos{%H-});
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (AnsiChar((Value+Len*Ord(Len>0)-1)^) in ['0'..'9']) then  //nope no money. Just a comma instead of dot.
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
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(Value), WideChar('.'), Def, Result);
      end;
  end;
end;

{ Convert string buffer into pascal string }

function BufferToStr(Buffer: PWideChar; Length: LongInt): string;
var s : ZWidestring;
begin
   Result := '';
   if Assigned(Buffer) then
   begin
      SetString(s, Buffer, Length div SizeOf(Char));
      Result := String(s);
   end;
end;

{ Convert string buffer into pascal string }

function BufferToStr(Buffer: PAnsiChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

function BufferToBytes(Buffer: Pointer; Length: LongInt): TBytes;
begin
  SetLength(Result, Length);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Result)^, Length);
end;

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PAnsiChar(Pointer(Str)), CheckInt, False);
end;

{**
  Converts a string into boolean value.
  @param Str a PAnsiChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Str^ of
      'T', 't': //Check mixed case of 'true' or 't' string
        begin
          Inc(Str);
          case Str^ of
            #0: Result := True;
            'R', 'r':
              begin
                Inc(Str);
                case Str^ of
                  'U', 'u':
                    begin
                      Inc(Str);
                      case Str^ of
                        'E', 'e':
                          begin
                            inc(Str);
                            case Str^ of
                              #0: Result := True;
                              ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                            end;
                          end;
                      end;
                    end;
                end;
              end;
            ' ': if IgnoreTrailingSaces then goto SkipSpaces;
          end;
        end;
      'Y', 'y': //Check mixed case of 'Yes' or 'y' string
        begin
          Inc(Str);
          case Str^ of
            #0: Result := True;
            'E', 'e':
              begin
                Inc(Str);
                case Str^ of
                  'S', 's':
                    begin
                      Inc(Str);
                      case Str^ of
                        #0: Result := True;
                        ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                      end;
                    end;
                end;
              end;
            ' ':
              if IgnoreTrailingSaces then
              begin
                SkipSpaces:
                while Str^ = ' ' do Inc(Str);
                Result := Str^ = #0;
              end;
          end;
        end;
      'O', 'o': //Check mixed case of 'ON' or 'on' string
        begin
          Inc(Str);
          case Str^ of
            'N', 'n': begin
                Inc(Str);
                case Str^ of
                  #0: Result := True;
                  ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                end;
              end;
          end;
        end;
      else
        Result := CheckInt and (RawToIntDef(Str, 0) <> 0);
    end;
end;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(const Str: ZWideString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PWideChar(Pointer(Str)), CheckInt, False);
end;

{**
  Converts a string into boolean value.
  @param Str a PWideChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Str^ of
      'T', 't': //Check mixed case of 'true' or 't' string
        begin
          Inc(Str);
          case Str^ of
            #0: Result := True;
            'R', 'r':
              begin
                Inc(Str);
                case Str^ of
                  'U', 'u':
                    begin
                      Inc(Str);
                      case Str^ of
                        'E', 'e':
                          begin
                            inc(Str);
                            case Str^ of
                              #0: Result := True;
                              ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                            end;
                          end;
                      end;
                    end;
                end;
              end;
            ' ': if IgnoreTrailingSaces then goto SkipSpaces;
          end;
        end;
      'Y', 'y': //Check mixed case of 'Yes' or 'y' string
        begin
          Inc(Str);
          case Str^ of
            #0: Result := True;
            'E', 'e':
              begin
                Inc(Str);
                case Str^ of
                  'S', 's':
                    begin
                      Inc(Str);
                      case Str^ of
                        #0: Result := True;
                        ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                      end;
                    end;
                end;
              end;
            ' ':
              if IgnoreTrailingSaces then
              begin
                SkipSpaces:
                while Str^ = ' ' do Inc(Str);
                Result := Str^ = #0;
              end;
          end;
        end;
      'O', 'o': //Check mixed case of 'ON' or 'on' string
        begin
          Inc(Str);
          case Str^ of
            'N', 'n': begin
                Inc(Str);
                case Str^ of
                  #0: Result := True;
                  ' ': if IgnoreTrailingSaces then goto SkipSpaces;
                end;
              end;
          end;
        end;
      else
        Result := CheckInt and (UnicodeToIntDef(Str, 0) <> 0);
    end;
end;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToUnicodeEx(Value: Boolean): ZWideString;
begin
  Result := BoolStrsW[Value];
end;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToRawEx(Value: Boolean): RawByteString;
begin
  Result := BoolStrsRaw[Value];
end;

{$IFDEF ENABLE_POSTGRESQL}
{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
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

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(const Str, Delimiters: string): TStrings;
begin
  Result := TStringList.Create;
  try
    SplitToStringList(Result, Str, Delimiters);
  except
    Result.Free;
    raise;
  end;
end;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; const Str, Delimiters: string);
begin
  List.Clear;
  SplitToStringList(List, Str, Delimiters);
end;

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);
begin
  SplitToStringList(List, Str, Delimiters);
end;

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; const Delimiter: string): string;
var
  i, Len, DelimLen: Integer;
  S: string;
  P: PChar;
begin
  DelimLen := Length(Delimiter);
  Len := 0;
  if List.Count > 0 then
  begin
    Inc(Len, Length(List[0]));
    for i := 1 to List.Count - 1 do
      Inc(Len, DelimLen + Length(List[i]));
  end;
  SetLength(Result, Len);
  P := Pointer(Result);
  for i := 0 to List.Count - 1 do
  begin
    if (i > 0) and (DelimLen > 0) then
    begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Delimiter)^, P^, DelimLen * SizeOf(Char));
      Inc(P, DelimLen);
    end;
    S := List[i];
    Len := Length(S);
    if Len > 0 then
    begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(S)^, P^, Len * SizeOf(Char));
      Inc(P, Len);
    end;
  end;
end;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;
begin
  Result := FloatToStr(Value, FSSqlFloat);
  end;

function SQLStrToFloat(const Str: String): Extended;
begin
  Result := StrToFloat(Str, FSSqlFloat);
end;

{**
  Split a single string using the delimiter, appending the resulting strings
  to the List. (gto: New version, now unicode safe and without the bug which
  adds a blank line before the last found string)
  @param List a list to append the result.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure SplitToStringListEx(List: TStrings; const Str, Delimiter: string);
var
   temp: string;
   i: integer;
begin
   temp := Str + Delimiter;
   repeat
      i := List.Add(Copy(temp, 0, AnsiPos(Delimiter, temp) - 1));
      Delete(temp, 1, Length(List[i] + Delimiter));
   until
      temp = '';
end;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  List.Clear;
  SplitToStringListEx(List, Str, Delimiter);
end;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(const Str, Delimiter: string): TStrings;
begin
  Result := TStringList.Create;
  try
    SplitToStringListEx(Result, Str, Delimiter);
  except
    Result.Free;
    raise;
  end;
end;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  SplitToStringListEx(List, Str, Delimiter);
end;

{**
  Converts bytes into a AnsiString representation.
  @param Value an array of bytes to be converted.
  @return a converted AnsiString.
}
function BytesToStr(const Value: TBytes): RawByteString;
{$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
var L: Integer;
{$ENDIF}
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Result := '';
  L := Length(Value);
  SetLength(Result, L);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
  {$ELSE}
  SetString(Result, PAnsiChar(@Value[0]), Length(Value))
  {$ENDIF}
end;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: AnsiString): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L*SizeOf(AnsiChar));
end;

{$IFDEF WITH_RAWBYTESTRING}
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: UTF8String): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value[1], Result[0], L)
end;

{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: RawByteString): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
end;
{$ENDIF}
{**
  Converts a WideString into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: WideString): TBytes;
var
  L: Integer;
  RBS: RawByteString;
begin
  L := Length(Value);
  if L = 0 then
    Result := nil
  else
  begin
    RBS := UnicodeStringToASCII7(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(RBS)^, Pointer(Result)^, L)
  end;
end;
{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TBytes;
var
  L: Integer;
  RBS: RawByteString;
begin
  L := Length(Value);
  if L = 0 then
    Result := nil
  else
  begin
    RBS := UnicodeStringToASCII7(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(RBS)^, Pointer(Result)^, L)
  end;
end;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TBytes): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  for I := 0 to Length(Value) - 1 do
    Result[I] := Value[I];
end;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(const Value: Variant): TBytes;
var
  I: Integer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create(SInvalidVarByteArray);

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;

{**
  Converts Ansi SQL Date/Time (yyyy-mm-dd hh:nn:ss or yyyy-mm-dd hh:nn:ss.zzz)
  to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(const Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Temp: string;
  DateFound: Boolean;

  procedure ExtractTime(const AString: String);
  var dotPos: Integer;
  begin
    Hour := StrToIntDef(Copy(AString, 1, 2), 0);
    Min := StrToIntDef(Copy(AString, 4, 2), 0);
    Sec := StrToIntDef(Copy(AString, 7, 2), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    dotPos := 0;
    MSec := 0;
    if Length(AString) > 8 then
      dotPos := ZFastCode.Pos('.', AString);

    //if the dot are found, milliseconds are present.
    if dotPos > 0 then begin
      MSec := StrToIntDef(LeftStr(RightStr(AString,Length(AString)-dotPos)+'000',3),0);
    end;
  end;
begin
  Temp := Value;
  Result := 0;
  DateFound := False;

  if Length(Temp) >= 10 then
  begin
    Year := StrToIntDef(Copy(Temp, 1, 4), 0);
    Month := StrToIntDef(Copy(Temp, 6, 2), 0);
    Day := StrToIntDef(Copy(Temp, 9, 2), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then
    begin
      try
        Result := EncodeDate(Year, Month, Day);
        DateFound := True;
      except
      end;
    end;
    Temp := RightStr(Temp, Length(Temp)-11);
  end;

  if (Length(Temp) >= 8) or ( not DateFound ) then
  begin
    if DateFound then
      ExtractTime(Temp)
    else
      ExtractTime(Value);
    try
      if Result >= 0 then
        Result := Result + EncodeTime(Hour, Min, Sec, MSec)
      else
        Result := Result - EncodeTime(Hour, Min, Sec, MSec)
    except
    end;
  end;
end;

function CheckNumberRange(Value: AnsiChar; out Failed: Boolean): Byte; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Failed := not ((Value >= '0') and (Value <= '9'));
  if Failed then
    Result := 0
  else
    Result := Ord(Value) - Ord('0');
end;

function CheckNumberRange(Value: WideChar; out Failed: Boolean): Word; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Failed := not ((Value >= '0') and (Value <= '9'));
  if Failed then
    Result := 0
  else
    Result := Ord(Value) - Ord('0');
end;

function CheckNumberRange(Value: AnsiChar): Boolean; overload; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ((Value >= '0') and (Value <= '9'));
end;

function CheckNumberRange(Value: WideChar): Boolean; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ((Value >= '0') and (Value <= '9'));
end;

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Year, Month: Int64;
  Day: Word;
  DateFormat: PChar;

  procedure TryExtractDateFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Result := 0;
    Failed := ZFormatSettings.DateFormatLen = 0;
    if not Failed then
    begin
      Year := 0; Month := 0; Day := 0;
      for i := 0 to ZFormatSettings.DateFormatLen-1 do
      begin
        case DateFormat^ of
          'Y', 'y':
            begin
              Year := Year * 10 + CheckNumberRange(Value^, Failed);
              if Failed then Exit;
            end;
          'M', 'm':
            begin
              Month := Month * 10 + CheckNumberRange(Value^, Failed);
              if Failed then Exit;
            end;
          'D', 'd':
            begin
              Day := Day * 10 + CheckNumberRange(Value^, Failed);
              if Failed then Exit;
            end;
        end;
        Inc(DateFormat);
        Inc(Value);
        if I+1 = vallen then Break;
      end;
      Failed := not ((Year <> 0) and (Month <> 0) and (Day <> 0));
      if not Failed then
        try
          Result := EncodeDate(Year, Month, Day);
        except
        end;
    end;
  end;

  procedure TryExtractDateFromUnknownSize;
  var
    DateLenCount: Cardinal;
    YPos, MPos, Code: Integer;
  begin
    Result := 0;
    Failed := False;
    if not (Value = '') then
    begin
      Year := 0; Month := 0; Day := 0;
      YPos := 0; MPos := 0; DateLenCount := 0;
      while ( DateLenCount < ValLen ) and (not ((Value+DateLenCount)^ in ['-','/','\','.']) ) do
      begin
        Year := Year * 10 + CheckNumberRange((Value+DateLenCount)^, Failed);
        if Failed then Exit;
        Inc(DateLenCount);
        Inc(YPos);
      end;
      while ( DateLenCount < ValLen ) and (not CheckNumberRange((Value+DateLenCount)^)) do
        Inc(DateLenCount);
      while ( DateLenCount < ValLen ) and (not ((Value+DateLenCount)^ in ['-','/','\']) ) do
      begin
        Month := Month * 10 + CheckNumberRange((Value+DateLenCount)^, Failed);
        if Failed then Exit;
        Inc(DateLenCount);
        Inc(MPos);
      end;
      while ( DateLenCount < ValLen ) and (not CheckNumberRange((Value+DateLenCount)^)) do
        Inc(DateLenCount);
      while ( DateLenCount < ValLen ) and (not ((Value+DateLenCount)^ in ['-','/','\']) ) do
      begin
        Day := Day * 10 + CheckNumberRange((Value+DateLenCount)^, Failed);
        if Failed then Exit
        else Inc(DateLenCount);
      end;
      if MPos > 2 then //float ValueTmp
      begin
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ValRawExt(Pointer(Value), '.', Code{%H-}));
        Failed := Code <> 0;
        if Failed then  Exit;
      end;
      if YPos > 4 then //We've a problem! No date delimiters found? -> YYYYMMDD or YYMMDD or ....Float!
        if YPos = 8 then
        begin
          //Let's start from the premise we've LongDateFormat YYYYMMDD
          Day := Year mod 100;
          Month := (Year mod 10000) div 100;
          Year := Year div 10000;
        end
        else
          if YPos = 6 then
          //Let's start from the premise we've ShortDateFormat YYMMDD
          begin
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
          begin
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ValRawExt(Pointer(Value), '.', Code));
            if Code <> 0 then
              Result := 0;
            Exit;
          end;
      Failed := not ( (Year <> 0) and (Month <> 0) and (Day <> 0) );
      if not Failed then
        try
          Result := EncodeDate(Year, Month, Day);
        except
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ValRawExt(Pointer(Value), '.', Code));
          Failed := Code <> 0;
          if Failed then Result := 0;
        end;
    end;
  end;
begin
  DateFormat := Pointer(ZFormatSettings.DateFormat);
  Failed := False;
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TryExtractDateFromFormat(Value);
    if Failed and ( ZFormatSettings.DateFormatLen = 0 )then
      TryExtractDateFromUnknownSize;
  end;
end;

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLDateToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLDateToDateTime(Pointer(UnicodeStringToASCII7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed);
end;

{**
  Converts Ansi SQL Time (TimeFormat)
  to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Hour, Minute: Int64;
  Sec, MSec: Word;
  TimeFormat: PChar;

  procedure TryExtractTimeFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Result := 0;
    Failed := ( ZFormatSettings.TimeFormatLen = 0 );
    if not Failed then
    begin
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      Failed := ( ZFormatSettings.TimeFormatLen = 0 ) and not (ValLen <= Byte(ZFormatSettings.TimeFormatLen-4));
      if not Failed then
      begin
        for i := 0 to ZFormatSettings.TimeFormatLen-1 do
        begin
          case TimeFormat^ of
            'H', 'h':
              begin
                Hour := Hour * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'N', 'n':
              begin
                Minute := Minute * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'S', 's':
              begin
                Sec := Sec * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'Z', 'z':
              begin
                MSec := MSec * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
          end;
          Inc(TimeFormat);
          Inc(Value);
          if i+1 = ValLen then Break;
        end;
        try
          Result := EncodeTime(Hour, Minute, Sec, MSec);
        except
          Failed := True;
        end;
      end;
    end;
  end;

  procedure TryExtractTimeFromVaryingSize;
  var
    HPos, NPos, Code: Integer;
    TimeLenCount: Cardinal;
  begin
    Result := 0;
    Failed := False;
    if not (Value = '') then
    begin
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      TimeLenCount := 0; HPos := 0; NPos := 0;
      while ( TimeLenCount < ValLen ) and (not ((Value+TimeLenCount)^ in [':','-','/','\','.']) ) do
      begin
        Hour := Hour * 10 + CheckNumberRange((Value+TimeLenCount)^, Failed);
        if Failed then Exit;
        Inc(HPos); Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange((Value+TimeLenCount)^)) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not ((Value+TimeLenCount)^ in [':','-','/','\','.']) ) do
      begin
        Minute := Minute * 10 + CheckNumberRange((Value+TimeLenCount)^, Failed);
        if Failed then Exit;
        Inc(NPos); Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange((Value+TimeLenCount)^)) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not ((Value+TimeLenCount)^ in [':','-','/','\','.']) ) do
      begin
        Sec := Sec * 10 + CheckNumberRange((Value+TimeLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange((Value+TimeLenCount)^) ) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not ((Value+TimeLenCount)^ in [':','-','/','\','.']) ) do
      begin
        MSec := MSec * 10 + CheckNumberRange((Value+TimeLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeLenCount);
      end;
      if NPos > 2 then //float value
      begin
        Result := Frac(ValRawExt(Pointer(Value), '.', Code));
        Failed := Code <> 0;
        if Failed then
          Result := 0;
        Exit;
      end;
      if HPos > 4 then //We've a problem! No date delimiters found? -> HHNNSSZZZ or HHNNSS or ....Float!
        case HPos of
          9:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZZZ
              MSec :=     Hour mod 1000;
              Sec :=     (Hour mod 100000)   div 1000;
              Minute :=  (Hour mod 10000000) div 100000;
              Hour := Hour div 10000000;
            end;
          8:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZZ
              MSec :=    Hour mod 100;
              Sec :=    (Hour mod 10000)   div 100;
              Minute := (Hour mod 1000000) div 10000;
              Hour := Hour div 1000000;
            end;
          7:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZ
              MSec :=    Hour mod 10;
              Sec :=    (Hour mod 1000)   div 10;
              Minute := (Hour mod 100000) div 1000;
              Hour := Hour div 100000;
            end;
          6:
            begin//Let's start from the premise we've ShortTimeFormat HHNNSS
              Sec := Hour mod 100;
              Minute := (Hour mod 10000) div 100;
              Hour := Hour div 10000;
            end
            else
            begin
              Result := Frac(ValRawExt(Pointer(Value), '.', Code));
              Failed := Code <> 0;
              if Failed then Result := 0;
              Exit;
            end;
        end;
      try
        Result := EncodeTime(Hour, Minute, Sec, MSec);
      except
        Result := Frac(ValRawExt(Pointer(Value), '.', Code));
        Failed := Code <> 0;
        if Failed then Result := 0;
      end;
    end;
  end;
begin
  Failed := False;
  TimeFormat := Pointer(ZFormatSettings.TimeFormat);
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TryExtractTimeFromFormat(Value); //prefered. Adapts to given Format-Mask
    if Failed and ( ZFormatSettings.TimeFormatLen = 0 )then
      TryExtractTimeFromVaryingSize;
  end;
end;

{**
  Converts Unicode SQL Time (TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLTimeToDateTime(Pointer(UnicodeStringToAscii7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed);
end;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeStampToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Year, Month: Int64;
  Day, Hour, Minute, Sec, MSec: Word;
  YPos, MPos, HPos: Integer;
  TimeStampFormat: PChar;

  procedure CheckFailAndEncode;
  begin
    if ( (Year <> 0) and (Month <> 0) and (Day <> 0) ) then
      try
        Result := EncodeDate(Year, Month, Day);
      except
        Result := 0;
        Failed := True;
      end
    else
      Failed := (Hour or Minute or Sec or MSec) = 0;;
    if not Failed then
      try
        if Result >= 0 then
          Result := Result + EncodeTime(Hour, Minute, Sec, MSec)
        else
          Result := Result - EncodeTime(Hour, Minute, Sec, MSec)
      except
        Result := 0;
        Failed := True;
      end
    else
      Failed := True;
  end;

  procedure TryExtractTimeStampFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Failed := ZFormatSettings.DateTimeFormatLen = 0;
    if not Failed then
    begin
      Failed  := (ValLen <= Byte(ZFormatSettings.DateTimeFormatLen-4));
      if not Failed then
      begin
        Year := 0; Month := 0; Day := 0;
        Hour := 0; Minute := 0; Sec := 0; MSec := 0;
        for i := 0 to ZFormatSettings.DateTimeFormatLen -1 do
        begin
          case TimeStampFormat^ of
            'Y', 'y':
              begin
                Year := Year * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'M', 'm':
              begin
                Month := Month * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'D', 'd':
              begin
                Day := Day * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'H', 'h':
              begin
                Hour := Hour * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'N', 'n':
              begin
                Minute := Minute * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'S', 's':
              begin
                Sec := Sec * 10 + CheckNumberRange(Value^, Failed);
                if Failed then Exit;
              end;
            'Z', 'z':
              begin
                MSec := MSec * 10 + CheckNumberRange(Value^, Failed);
                if Failed then
                begin
                  Failed := not (Value^ = '+'); //postgres 2013-10-23 12:31:52.48+02 f.e.
                  if Failed then
                    Exit
                  else
                  begin
                    Msec := Msec div 10; //align result again
                    Break;
                  end;
                end;
              end;
            '.':
              if (Value^ = '+') then Break; //postgres 1997-02-25 00:00:00+01 f.e.
          end;
          Inc(TimeStampFormat);
          Inc(Value);
          if (i+1) = ValLen then Break;
        end;
        CheckFailAndEncode;
      end;
    end;
  end;

  procedure TryExtractTimeStampFromVaryingSize;
  var
    DotCount, Code: Integer;
    TimeStampLenCount: Cardinal;

    procedure ReadDate;
    begin
      { read date}
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        Year := Year * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(YPos); Inc(TimeStampLenCount);
      end;
      if (Value+TimeStampLenCount)^ = '.' then
        Inc(DotCount); //possible float
      if ((Value+TimeStampLenCount)^ = ':') and ( YPos < 3) then
      begin
        Hour := Year;
        Year := 0;
        Exit;
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        Month := Month * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
        Inc(MPos);
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        Day := Day * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
    end;

    procedure ReadTime;
    begin
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        if HPos = 2 then //hour can't have 3 digits, date was aligned previously instead of time  > let's fix it
        begin
          MSec := Hour * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
          if Failed then Exit;
          Sec := Day; Day := 0;
          Minute := Month; Month := 0;
          Hour := Year; Year := 0;
          exit;
        end;
        Hour := Hour * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(HPos); Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        Minute := Minute * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) ) do
      begin
        Sec := Sec * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and ((Value+TimeStampLenCount)^ in [':','-','/','\','.',' ']) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Value^ in [':','-','/','\','.',' ']) ) do
      begin
        MSec := MSec * 10 + CheckNumberRange((Value+TimeStampLenCount)^, Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
    end;

  begin
    Result := 0;
    Failed := False;
    if not (Value = '') then
    begin
      Year := 0; Month := 0; Day := 0;
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      YPos := 0; MPos := 0; HPos := 0; TimeStampLenCount := 0; DotCount := 0;
      ReadDate;
      {read time}
      if Failed then Exit
      else ReadTime;
      if Failed then Exit;

      if (MPos > 2) and ( DotCount = 1) then //float value
      begin
        Result := ValRawExt(Pointer(Value), '.', Code{%H-});
        Failed := Code <> 0;
        if Failed then
          Result := 0;
        Exit;
      end;
      if YPos > 4 then //We've a problem! No date delimiters found? -> YYYYMMDDHHNNSSZZZ or ... or .. HHNNSS or ....Float!
        if YPos >= 14 then //YYYYMMDDHHNNSS +ZZZ?
        begin //Let's start from the premise we've LongTimeFormat HHNNSSZZ
          case YPos of
            17: //YYYYMMDDHHNNSSZZZ
              begin
                MSec := Year mod 1000;
                Year := Year div 1000;
              end;
            16: //YYYYMMDDHHNNSSZZ
              begin
                MSec := Year mod 100;
                Year := Year div 100;
              end;
            15: //YYYYMMDDHHNNSSZ
              begin
                MSec := Year mod 10;
                Year := Year div 10;
              end;
          end;
          //YYYYMMDDHHNNSS
          Sec := Year mod 100;
          Minute := (Year mod 10000) div 100;
          Hour := (Year mod 1000000) div 10000;
          Day := (Year mod 100000000) div 1000000;
          Month := (Year mod 10000000000) div 100000000;
          Year := Year div 10000000000;
        end
        else
          if YPos = 8 then //Date?
          begin
            //Let's start from the premise we've LongDateFormat YYYYMMDD
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
          if YPos = 6 then
          //Let's start from the premise we've ShortTimeFormat HHNNSS
          begin
            if MPos > 5 then
            begin
              case MPos of
                9: //HHNNSSZZZ
                  begin
                    MSec := Month mod 1000;
                    Month := Month div 1000;
                  end;
                8: //HHNNSSZZ
                  begin
                    MSec := Month mod 100;
                    Month := Month div 100;
                  end;
                7: //HHNNSSZ
                  begin
                    MSec := Month mod 100;
                    Month := Month div 100;
                  end;
              end;
              Sec := Month mod 100;
              Minute := (Month mod 10000) div 100;
              Hour := (Month mod 1000000) div 10000;
              Month := 0;
            end;
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
            if (DotCount = 1) or (DotCount = 0 ) then
            begin
              Result := ValRawExt(Pointer(Value), '.', Code);
              Failed := ( Code <> 0 );
              if Failed then Result := 0;
              Exit;
            end
            else
            begin
              Failed := True;
              Exit;
            end;
      CheckFailAndEncode;
    end;
  end;
begin
  Failed := False;
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TimeStampFormat := Pointer(ZFormatSettings.DateTimeFormat);
    TryExtractTimeStampFromFormat(Value);
    if Failed then
      TryExtractTimeStampFromVaryingSize;
  end;
end;

{**
  Converts Unicode SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeStampToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLTimeStampToDateTime(Pointer(UnicodeStringToAscii7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed)
end;

procedure PrepareDateTimeStr(const Quoted: Boolean; const Suffix: ZWideString;
  const Len: LengthInt; var Value: ZWideString); overload;
var
  SLen: LengthInt;
  P: PWideChar;
begin
  SLen := Length(Suffix);
  { prepare Value if required }
  ZSetString(nil, len+(2*Ord(Quoted))+Slen, Value);
  P := Pointer(Value);
  if Quoted then begin
    P^ := #39; //starting quote
    (P+Len+1)^ := #39; //leading quote
    if SLen > 0 then //move suffix after leading quote
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Suffix)^, (P+Len+2)^, Slen shl 1);
  end else
  if SLen > 0 then //move suffix after leading quote
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Suffix)^, (P+Len)^, Slen shl 1);
end;

procedure PrepareDateTimeStr(const Quoted: Boolean; const Suffix: RawByteString;
  const Len: LengthInt; var Value: RawByteString); overload;
var
  SLen: LengthInt;
  P: PAnsiChar;
begin
  SLen := Length(Suffix);
  { prepare Value if required }
  ZSetString(nil, len+(2*Ord(Quoted))+Slen, Value);
  P := Pointer(Value);
  if Quoted then begin
    P^ := #39; //starting quote
    (P+Len+1)^ := #39; //leading quote
    if SLen > 0 then //move suffix after leading quote
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Suffix)^, (P+Len+2)^, Slen);
  end
  else
    if SLen > 0 then
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Suffix)^, (P+Len)^, Slen);
end;
{**
  Converts DateTime value to a rawbyteString
  @param Value a TDateTime value.
  @param DateFormat the result format.
  @return a formated RawByteString with DateFormat pattern.
}
function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  DateFormat: PChar;
  PA: PAnsiChar;
  YearSet: Boolean;
begin
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  YearSet := False;
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.DateFormatLen, Result{%H-});
  PA := Pointer(Result);
  Inc(PA, Ord(Quoted));

  I := ConFormatSettings.DateFormatLen-1;
  DateFormat := Pointer(ConFormatSettings.DateFormat);
  while I > 0 do
    case (DateFormat+i)^ of
      'Y', 'y':
        begin
          if YearSet then  //Year has either two or four digits
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AYear div 100]
          else
          begin
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AYear mod 100];
            YearSet := True;
          end;
          Dec(i,2);
        end;
      'M', 'm':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMonth];
          Dec(I, 2);
        end;
      'D', 'd':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[ADay];
          Dec(I, 2);
        end;
      else
      begin
        PByte(PA+i)^ := Ord((DateFormat+i)^);
        Dec(i);
      end;
  end;
end;

{**
  Converts DateTime value to a rawbyteString
  @param Value a TDateTime value.
  @param DateFormat the result format.
  @return a formated RawByteString with DateFormat pattern.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  DateFormat: PChar;
  PW: PWideChar;
  YearSet: Boolean;
begin
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  YearSet := False;
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.DateFormatLen, Result{%H-});
  PW := Pointer(Result);
  Inc(PW, Ord(Quoted));

  I := ConFormatSettings.DateFormatLen-1;
  DateFormat := Pointer(ConFormatSettings.DateFormat);
  while I > 0 do
    case (DateFormat+i)^ of
      'Y', 'y':
        begin
          if YearSet then //Year has either two or four digits
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AYear div 100]
          else
          begin
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AYear mod 100];
            YearSet := True;
          end;
          Dec(i,2);
        end;
      'M', 'm':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMonth];
          Dec(I, 2);
        end;
      'D', 'd':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[ADay];
          Dec(I, 2);
        end;
      else
      begin
        PWord(PW+i)^ := Ord((DateFormat+i)^); //instead of conversion with WideChar -> FPC rocks!
        Dec(i);
      end;
  end;
end;

{**
  Converts DateTime value into a RawByteString with format pattern
  @param Value a TDateTime value.
  @param TimeFormat the result format.
  @return a formated RawByteString with Time-Format pattern.
}
{$WARNINGS OFF} //suppress D2007 Waring for undefined result
function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  TimeFormat: PChar;
  PA: PAnsiChar;
  ZSet: Boolean;
begin
  {need fixed size to read from back to front}
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.TimeFormatLen, Result{%H-});
  ZSet := False;
  PA := Pointer(Result);
  Inc(PA, Ord(Quoted));

  I := ConFormatSettings.TimeFormatLen-1;
  TimeFormat := Pointer(ConFormatSettings.TimeFormat);
  while I > 0 do
    case (TimeFormat+i)^ of
      'H', 'h':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AHour];
          Dec(I, 2);
        end;
      'N', 'n':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMinute];
          Dec(I, 2);
        end;
      'S', 's':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[ASecond];
          Dec(I, 2);
        end;
      'Z', 'z':
        begin
          Dec(I);
          if ZSet then
            Continue
          else
          begin
            (PWord(@PByteArray(PA)[i]))^ := TwoDigitLookupW[AMilliSecond mod 100];
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMilliSecond div 10];
            ZSet := True;
          end;
        end;
      else
      begin
        PByte(PA+i)^ := Ord((TimeFormat+i)^);
        Dec(i);
      end;
    end;
end;

{**
  Converts DateTime value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param TimeFormat the result format.
  @return a formated WideString/UnicodeString with Time-Format pattern.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  TimeFormat: PChar;
  PW: PWideChar;
  ZSet: Boolean;
begin
  {need fixed size to read from back to front}
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.TimeFormatLen, Result{%H-});
  ZSet := False;
  PW := Pointer(Result);
  Inc(PW, Ord(Quoted));

  I := ConFormatSettings.TimeFormatLen-1;
  TimeFormat := Pointer(ConFormatSettings.TimeFormat);
  while I > 0 do
    case (TimeFormat+i)^ of
      'H', 'h':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AHour];
          Dec(I, 2);
        end;
      'N', 'n':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMinute];
          Dec(I, 2);
        end;
      'S', 's':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[ASecond];
          Dec(I, 2);
        end;
      'Z', 'z':
        begin
          Dec(I);
          if ZSet then
            Continue
          else
          begin
            (PLongWord(@PWordArray(PW)[i]))^ := TwoDigitLookupLW[AMilliSecond mod 100];
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMilliSecond div 10];
            ZSet := True;
          end;
        end;
      else
      begin
        PWord(PW+i)^ := Ord((TimeFormat+i)^); //instead of conversion with WideChar -> FPC rocks!
        Dec(i);
      end;
    end;
end;


{**
  Converts DateTime value to a RawByteString
  @param Value a TDateTime value.
  @param TimeStampFormat the result format.
  @return a formated RawByteString in TimeStamp-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = ''): RawByteString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  TimeStampFormat: PChar;
  ZSet, YearSet: Boolean;
  PA: PAnsiChar;
begin
  {need fixed size to read from back to front}
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.DateTimeFormatLen, Result{%H-});
  ZSet := False;
  YearSet := False;
  PA := Pointer(Result);
  Inc(PA, Ord(Quoted));

  I := ConFormatSettings.DateTimeFormatLen-1;
  TimeStampFormat := Pointer(ConFormatSettings.DateTimeFormat);
  while I > 0 do
    case (TimeStampFormat+i)^ of
      'Y', 'y':
        begin
          if YearSet then  //Year has either two or four digits
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AYear div 100]
          else
          begin
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AYear mod 100];
            YearSet := True;
          end;
          Dec(i,2);
        end;
      'M', 'm':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMonth];
          Dec(i, 2);
        end;
      'D', 'd':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[ADay];
          Dec(i, 2);
        end;
      'H', 'h':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AHour];
          Dec(i, 2);
        end;
      'N', 'n':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMinute];
          Dec(i, 2);
        end;
      'S', 's':
        begin
          (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[ASecond];
          Dec(i, 2);
        end;
      'Z', 'z':
        begin
          Dec(I);
          if ZSet then
            Continue
          else
          begin
            (PWord(@PByteArray(PA)[i]))^ := TwoDigitLookupW[AMilliSecond mod 100];
            (PWord(@PByteArray(PA)[i-1]))^ := TwoDigitLookupW[AMilliSecond div 10];
            ZSet := True;
          end;
        end;
      else
      begin
        PByte(PA+i)^ := Ord((TimeStampFormat+i)^);
        Dec(i);
      end;
    end;
end;

{**
  Converts DateTime value to a WideString/UnicodeString
  @param Value a TDateTime value.
  @param TimeStampFormat the result format.
  @return a formated WideString/UnicodeString in TimeStamp-Format pattern.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  I: Integer;
  TimeStampFormat: PChar;
  ZSet, YearSet: Boolean;
  PW: PWideChar;
begin
  {need fixed size to read from back to front}
  DecodeDateTime(Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  PrepareDateTimeStr(Quoted, Suffix, ConFormatSettings.DateTimeFormatLen, Result{%H-});
  ZSet := False;
  YearSet := False;
  PW := Pointer(Result);
  Inc(PW, Ord(Quoted));

  I := ConFormatSettings.DateTimeFormatLen-1;
  TimeStampFormat := Pointer(ConFormatSettings.DateTimeFormat);
  while I > 0 do
    case (TimeStampFormat+i)^ of
      'Y', 'y':
        begin
          if YearSet then  //Year has either two or four digits
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AYear div 100]
          else
          begin
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AYear mod 100];
            YearSet := True;
          end;
          Dec(i,2);
        end;
      'M', 'm':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMonth];
          Dec(i, 2);
        end;
      'D', 'd':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[ADay];
          Dec(i, 2);
        end;
      'H', 'h':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AHour];
          Dec(i, 2);
        end;
      'N', 'n':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMinute];
          Dec(i, 2);
        end;
      'S', 's':
        begin
          (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[ASecond];
          Dec(i, 2);
        end;
      'Z', 'z':
        begin
          Dec(I);
          if ZSet then
            Continue
          else
          begin
            (PLongWord(@PWordArray(PW)[i]))^ := TwoDigitLookupLW[AMilliSecond mod 100];
            (PLongWord(@PWordArray(PW)[i-1]))^ := TwoDigitLookupLW[AMilliSecond div 10];
            ZSet := True;
          end;
        end;
      else
      begin
        PWord(PW+i)^ := Ord((TimeStampFormat+i)^); //instead of conversion with WideChar -> FPC rocks!
        Dec(i);
      end;
    end;
end;
{$WARNINGS ON} //suppress D2007 Warning for undefined result


{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime; WithMMSec: Boolean = False): string;
var
  a, MSec:Word;
begin
  if WithMMSec then
  begin
    DecodeTime(Value,a,a,a,MSec);
    if MSec=0 then
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value)
    else
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value);
  end
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
end;

{ TZSortedList }

{**
  Origial Autor: Aleksandr Sharahov
  see http://guildalfa.ru/alsha/
  Performs hybrid sort algorithm for the list.
  changes by EgonHugeist:
  Replace cardinal casts by using our NativeUInt to make it 64Bit compatible too
  Note Alexandr wrote: For max of speed it is very impotant to use procedures
    QuickSort_0AA and HybridSort_0AA as is (not in class, not included
    in other procedure, and not changed parameters and code).
}
//~1.57 times faster than Delphi QuickSort on E6850
{$UNDEF SaveQ} {$IFOPT Q+} {$Q-} {$DEFINE SaveQ} {$ENDIF}
{$UNDEF SaveR} {$IFOPT R+} {$R-} {$DEFINE SaveR} {$ENDIF}
const
  InsCount = 35; //33..49;
  InsLast = InsCount-1;
  SOP = SizeOf(pointer);
  MSOP = NativeUInt(-SOP);
{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure QuickSortSha_0AA(L, R: NativeUInt; Compare: TZListSortCompare);
var
  I, J, P, T: NativeUInt;
begin;
  while true do
  begin
    I := L;
    J := R;
    if J-I <= InsLast * SOP then break;
    T := (J-I) shr 1 and MSOP + I;

    if Compare(PPointer(J)^, PPointer(I)^)<0 then
    begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(J)^;
      PNativeUInt(J)^ := P;
    end;
    P := PNativeUInt(T)^;
    if Compare(Pointer(P), PPointer(I)^)<0 then
    begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(T)^;
      PNativeUInt(T)^ := P;
    end
    else
      if Compare(PPointer(J)^, Pointer(P)) < 0 then
      begin
        P := PNativeUInt(J)^;
        PNativeUInt(J)^ := PNativeUInt(T)^;
        PNativeUInt(T)^ := P;
      end;

    repeat
      Inc(I,SOP);
    until not (Compare(PPointer(I)^, Pointer(P)) < 0);
    repeat
      Dec(J,SOP)
    until not (Compare(pointer(P), PPointer(J)^) < 0);
    if I < J then
      repeat
        T := PNativeUInt(I)^;
        PNativeUInt(I)^ := PNativeUInt(J)^;
        PNativeUInt(J)^ := T;
        repeat
          Inc(I,SOP);
        until not (Compare(PPointer(I)^, pointer(P)) < 0 );
        repeat
          Dec(J,SOP);
        until not (Compare(pointer(P), PPointer(J)^) < 0);
      until I >= J;
    Dec(I,SOP); Inc(J,SOP);

    if I-L <= R-J then
    begin
      if L + InsLast * SOP < I then
        QuickSortSha_0AA(L, I, Compare);
      L := J;
    end
    else
    begin
      if J + InsLast * SOP < R
        then QuickSortSha_0AA(J, R, Compare);
      R := I;
    end;
  end;
end;

procedure HybridSortSha_0AA(List: PPointerList; Count: integer; Compare: TZListSortCompare);
var
  I, J, {$IFDEF WITH_IE200706094}J2,{$ENDIF} L, R: NativeUInt;
begin;
  if (List<>nil) and (Count>1) then
  begin
    L := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[0]);
    R := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[Count-1]);
    J := R;
    if Count-1 > InsLast then
    begin
      J:=NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[InsLast]);
      QuickSortSha_0AA(L, R, Compare);
    end;

    I := L;
    repeat;
      if Compare(PPointer(J)^, PPointer(I)^) < 0 then I:=J;
      dec(J,SOP);
    until J <= L;

    if I > L then
    begin
      J := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(L)^;
      PNativeUInt(L)^ := J;
    end;

    J := L + SOP;
    while true do
    begin
      repeat;
        if J >= R then exit;
        inc(J,SOP);
      {$IFDEF WITH_IE200706094} //FPC 64Bit raises an internal Error 200706094!
        J2 := J+MSOP;
      until Compare(PPointer(J)^,PPointer(J2)^) < 0;
      {$ELSE}
      until Compare(PPointer(J)^,PPointer(J+MSOP)^) < 0;
      {$ENDIF}
      I := J - SOP;
      L := PNativeUInt(J)^;
      repeat;
        PNativeUInt(I+SOP)^ := PNativeUInt(I)^;
        dec(I,SOP);
      until not (Compare(Pointer(L),PPointer(I)^) < 0);
      PNativeUInt(I + SOP)^ := L;
    end;
  end;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}
{$IFDEF SaveQ} {$Q+} {$UNDEF SaveQ} {$ENDIF}
{$IFDEF SaveR} {$R+} {$UNDEF SaveR} {$ENDIF}

{**
  Performs sorting for this list.
  @param Compare a comparison function.
}
procedure TZSortedList.Sort(Compare: TZListSortCompare);
begin
  {$IFDEF TLIST_ISNOT_PPOINTERLIST}
  HybridSortSha_0AA(@List, Count, Compare);
  {$ELSE}
  HybridSortSha_0AA(List, Count, Compare);
  {$ENDIF}
end;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(const Value: string): string;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  DestLength := 0;
  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ = #0 then
       Inc(DestLength, 4)
    else
      case SrcBuffer^ of
        '"', '''', '\':
         Inc(DestLength, 2)
        else
         Inc(DestLength);
      end;
    Inc(SrcBuffer);
  end;

  SrcBuffer := Pointer(Value);
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);

  for I := 1 to SrcLength do begin
    case SrcBuffer^ of
      #0: begin
          DestBuffer[0] := '\';
          DestBuffer[1] := Chr(Ord('0') + (Byte(SrcBuffer^) shr 6));
          DestBuffer[2] := Chr(Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07));
          DestBuffer[3] := Chr(Ord('0') + (Byte(SrcBuffer^) and $07));
          Inc(DestBuffer, 4);
        end;
      '"', '''', '\':
        begin
          DestBuffer[0] := '\';
          DestBuffer[1] := SrcBuffer^;
          Inc(DestBuffer, 2);
        end;
      else
        begin
          DestBuffer^ := SrcBuffer^;
          Inc(DestBuffer);
        end;
    end;
    Inc(SrcBuffer);
  end;
end;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(const Value: string): string;
var
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  SetLength(Result, SrcLength);
  DestLength := 0;
  DestBuffer := Pointer(Result);


  while SrcLength > 0 do begin
    if SrcBuffer^ = '\' then begin
      Inc(SrcBuffer);
      case SrcBuffer^ of
        '0'..'9':
          begin
            DestBuffer^ := Chr(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
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
    Inc(DestLength);
  end;
  SetLength(Result, DestLength);
end;


{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
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


{**
  Remove chars in the string.
  More obvious and ~35 times faster than StringReplace(Str, ToRemove, '')
  @param ToRemove a char to search and remove.
  @param Str a source string.
  @return a string with removed chars.
}
function RemoveChar(ToRemove: Char; const Str: string): string;
var
  PSrc, PSrcEnd, PDest: PChar;
  Len: Integer;
begin
  Len := Length(Str);
  SetLength(Result, Len);
  if Len = 0 then
    Exit;
  PSrc := Pointer(Str);
  PSrcEnd := @Str[Len];
  PDest := Pointer(Result);

  while PSrc <= PSrcEnd do
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

{**
  Append a string to another string separating the added string with delimiter.
  Correctly processes cases where any of the arguments could be empty
  @param Str source string to append to. If empty, resulting Str value will be AddStr
  @param AddStr string to append. If empty, Str won't be changed
  @param Delimiter string to separate AddStr from Str
}
procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);
begin
  if AddStr <> '' then
    if Str = '' then
      Str := AddStr
    else
      Str := Str + Delimiter + AddStr;
end;

{**
  Break a string into two parts according to appearance of Delimiter.
  @param Str source string
  @param Delimiter separator string; Str=Left+Delimiter+Right
  @param Left left part of Str from the start to the first Delimiter.
    Equals to Str if Str doesn't contain Delimiter
  @param Right left part of Str from the first Delimiter to the end.
    Empty if Str doesn't contain Delimiter

  NB: "var" modifier here allows using the same variable both as source and dest,
  for ex. in a loop like
    while Str <> '' do
    begin
      BreakString(Str, Delim, Fragment, Str);
      ...
    end;
  "out" modifier will clear the value at the entry of the proc!
}
procedure BreakString(const Str, Delimiter: String; var Left, Right: String);
var
  DelimPos, DelimLen: Integer;
  StrSave: string;
begin
  DelimPos := ZFastCode.Pos(Delimiter, Str);
  if DelimPos > 0 then
  begin
    DelimLen := Length(Delimiter);
    StrSave := Str; // allow one variable both as Str and Left
    Left := Copy(StrSave, 1, DelimPos - 1);
    Right := Copy(StrSave, DelimPos + DelimLen, MaxInt);
  end
  else
  begin
    Left := Str;
    Right := '';
  end;
end;


{**
  Decodes a full version value encoded with Zeos SQL format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);
begin
 MajorVersion := FullVersion div 1000000;
 MinorVersion := (FullVersion - (MajorVersion * 1000000)) div 1000;
 SubVersion   := FullVersion-(MajorVersion*1000000)-(MinorVersion*1000);
end;

{**
  Encodes major, minor and subversion (revision) values in Zeos SQL format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  For example, 4.1.12 is returned as 4001012.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;
begin
 Result := (MajorVersion * 1000000) + (MinorVersion * 1000) + SubVersion;
end;

{**
  Formats a Zeos SQL Version format to X.Y.Z where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param SQLVersion an integer
  @return Formated Zeos SQL Version Value.
}

function FormatSQLVersion(const SQLVersion: Integer): string;
var
   MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeSQLVersioning(SQLVersion, MajorVersion, MinorVersion, SubVersion);
 Result := ZFastCode.IntToStr(MajorVersion)+'.'+
           ZFastCode.IntToStr(MinorVersion)+'.'+
           ZFastCode.IntToStr(SubVersion);
end;

function ASCII7ToUnicodeString(const Src: RawByteString): ZWideString;
var I: Integer;
begin
  if Pointer(Src) = nil then
    Result := ''
  else begin
    System.SetString(Result, nil, {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^);
    for i := 0 to {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^-1 do
      PWordArray(Result)[i] := PByteArray(Src)[i]; //0..255 equals to widechars
  end;
end;

function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): ZWideString;
begin
  ZSetString(Src, Len, Result{%H-});
end;

function UnicodeStringToASCII7(const Src: ZWideString): RawByteString;
var i, l: integer;
begin
  L := System.Length(Src); //temp l speeds x2
  if L = 0 then
    Result := ''
  else
  begin
    if (Pointer(Result) = nil) or //empty ?
      ({%H-}PRefCntInt(NativeUInt(Result) - StringRefCntOffSet)^ <> 1) or { unique string ? }
      (LengthInt(l) <> {%H-}PLengthInt(NativeUInt(Result) - StringLenOffSet)^) then { length as expected ? }
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Result := ''; //speeds up SetLength x2
      SetLength(Result, l);
    end;
    {$ELSE}
    System.SetString(Result,nil, l);
    {$ENDIF}
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..255 equals to widechars
  end;
end;


function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString;
var i: integer;
begin
  if (Src = nil) or (Len = 0) then
    Result := ''
  else
  begin
    if (Pointer(Result) = nil) or //empty ?
      ({%H-}PRefCntInt(NativeUInt(Result) - StringRefCntOffSet)^ <> 1) or { unique string ? }
      (LengthInt(len) <> {%H-}PLengthInt(NativeUInt(Result) - StringLenOffSet)^) then { length as expected ? }
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Result := ''; //speeds up SetLength x2
      SetLength(Result, len);
    end;
    {$ELSE}
    System.SetString(Result,nil, Len);
    {$ENDIF}
    for i := 0 to len-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..255 equals to widechars
  end;
end;

function FloatToRaw(const Value: Extended): RawByteString;
{$IFNDEF FPC}
var
  Buffer: array[0..63] of AnsiChar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := FloatToStr(Value);
  {$ELSE}
  SetString(Result, Buffer, {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(PAnsiChar(@Buffer), Value, fvExtended,
    ffGeneral, 15, 0));
  {$ENDIF}
end;

function FloatToSqlRaw(const Value: Extended): RawByteString;
{$IFNDEF FPC}
var
  Buffer: array[0..63] of AnsiChar;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := FloatToStr(Value, FSSqlFloat);
  {$ELSE}
  SetString(Result, Buffer, {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(PAnsiChar(@Buffer), Value, fvExtended,
    ffGeneral, 15, 0, FSSqlFloat));
  {$ENDIF}
end;

function FloatToUnicode(const Value: Extended): ZWideString;
begin
  Result := ASCII7ToUnicodeString(FloatToRaw(Value));
end;

function FloatToSqlUnicode(const Value: Extended): ZWideString;
begin
  Result := ASCII7ToUnicodeString(FloatToSqlRaw(Value));
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
    PLongWord(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    PLongWord(Text+2)^ := TwoDigitLookupHexLW[Ord((Buffer+1)^)];
    PLongWord(Text+4)^ := TwoDigitLookupHexLW[Ord((Buffer+2)^)];
    PLongWord(Text+6)^ := TwoDigitLookupHexLW[Ord((Buffer+3)^)];
    PLongWord(Text+8)^ := TwoDigitLookupHexLW[Ord((Buffer+4)^)];
    PLongWord(Text+10)^ := TwoDigitLookupHexLW[Ord((Buffer+5)^)];
    PLongWord(Text+12)^ := TwoDigitLookupHexLW[Ord((Buffer+6)^)];
    PLongWord(Text+14)^ := TwoDigitLookupHexLW[Ord((Buffer+7)^)];
    Inc(Buffer, 8);
    Inc(Text, 16);
  end;
  Inc(PEnd, 8);
  while Buffer < PEnd do
  begin
    PLongWord(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    Inc(Buffer);
    Inc(Text, 2);
  end;
end;

procedure HexFiller;
var
  I: Byte;
  Hex: String;
begin
  for i := Low(Byte) to High(Byte) do
  begin
    Hex := IntToHex(I, 2);
    {$IFDEF UNICODE}
    TwoDigitLookupHexLW[i] := PLongWord(Pointer(Hex))^;
    TwoDigitLookupHexW[i] := PWord(Pointer(AnsiString(Hex)))^;
    {$ELSE}
    TwoDigitLookupHexW[i] := PWord(Pointer(Hex))^;
    TwoDigitLookupHexLW[i] := PCardinal(Pointer(ZWideString(Hex)))^;
    {$ENDIF}
  end;
end;

//EgonHugeist: my conversion is 10x faster than IDE's
procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar;
  const SetTerm: Boolean = False);
var
  D1: Cardinal;
  W: Word;
  I: Integer;
begin
  Dest^ := '{';
  Inc(Dest);
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do
  begin
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(@D1)^];
    D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  Dest^ := '-';
  // Source is binary data in fact, we're using PAnsiChar only to allow
  // pointer math for older Delphis
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  Dest^ := '-';
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  Dest^ := '-'; //Process D4
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+8)^];
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+9)^];
  (Dest+5)^ := '-';
  Inc(Dest, 6);
  for i := 0 to 5 do
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+10+i)^];
  (Dest+12)^ := '}';
  if SetTerm then (Dest+13)^ := #0; //set trailing term
end;

procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const SetTerm: Boolean = False);
var
  I: Integer;
  D1: Cardinal;
  W: Word;
begin
  Dest^ := '{';
  Inc(Dest);
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do
  begin
    PLongWord(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(@D1)^];
    if D1 > 0 then D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  Dest^ := '-';
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  Dest^ := '-';
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  Dest^ := '-'; //Process D4
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+8)^];
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+9)^];
  (Dest+5)^ := '-';
  Inc(Dest, 6);
  for i := 0 to 5 do
    PLongWord(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+10+i)^];
  (Dest+12)^ := '}';
  if SetTerm then (Dest+13)^ := #0; //set trailing term
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(const GUID: TGUID): RawByteString; overload;
begin
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(@GUID.D1, PAnsiChar(Pointer(Result)));
end;

{$IFNDEF WITH_EARGUMENTEXCEPTION}     // EArgumentException is supported
type
  EArgumentException = Class(Exception);
{$ENDIF}

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(const Bts: TBytes): RawByteString; overload;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Pointer(Bts), PAnsiChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(Buffer: Pointer; Len: NativeInt): RawByteString; overload;
begin
  if (Buffer = Nil) or (Len <> 16) then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Buffer, PAnsiChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(const GUID: TGUID): ZWideString; overload;
begin
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(@GUID.D1, PWideChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(const Bts: TBytes): ZWideString; overload;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Pointer(Bts), PWideChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(Buffer: Pointer; Len: NativeInt): ZWideString; overload;
begin
  if (Buffer = Nil) or (Len <> 16) then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Buffer, PWideChar(Pointer(Result)));
end;

procedure InvalidGUID(C: Char);
begin
  raise EArgumentException.CreateResFmt(@SInvalidGUID, [String(C)]);
end;
{**
  EgonHugeist: my conversion is 1,5x faster than IDE's
  converty hex-dezimal guid-string into a binary format
  @param Src a Pointer to the ansi Source-String
  @param Dest a Pointer to a 16-Bytes buffer
    Note it works with TGUID using @GUID.D1 and all other Buffers
}
procedure ValidGUIDToBinary(Src, Dest: PAnsiChar);
  function HexChar(c: AnsiChar): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      begin
        Result := 0; //satisfy compiler!
        InvalidGUID(Char(C));
      end;
    end;
  end;

  function HexByte(p: PAnsiChar): Byte;
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

{**
  EgonHugeist: my conversion is 1,5x faster than IDE's
  converty hex-dezimal unicode guid-string into a binary format
  @param Src a Pointer to the Unicode Source-String
  @param Dest a Pointer to a 16-Bytes buffer
    Note it works with TGUID using @GUID.D1 and all other Buffers
}
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

function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): ZWidestring; overload;
var
  P, Dest, PEnd, PFirst: PWideChar;
begin
  Dest := Nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    Inc({%H-}NativeUInt(Dest), Ord(P^=Quote));
    if Dest = nil then
      PFirst := P;
    Inc(P);
  end;
  if Dest = nil then begin
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
  SetLength(Result, Len + {%H-}NativeInt(Dest) + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  P := PFirst+1;
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src) shl 1);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if P^=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (PEnd - Src) shl 1);
  Inc(Dest, PEnd - Src);
  Dest^ := Quote;
end;

function SQLQuotedStr(const S: ZWideString; Quote: WideChar): ZWidestring;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString;
var
  P, Dest, PEnd, PFirst: PAnsiChar;
begin
  Dest := nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    Inc({%H-}NativeUInt(Dest), Ord(P^=Quote));
    if Dest = nil then
      PFirst := P;
    Inc(P);
  end;
  if Dest = nil then begin
    System.SetLength(Result, Len+2);
    Dest := Pointer(Result);
    Dest^ := Quote;
    if Len > 0 then begin
      System.Move(Src^, (Dest+1)^, Len);
      Inc(Dest, Len+1);
    end else
      Inc(Dest);
    Dest^ := Quote;
    Exit;
  end;
  SetLength(Result, Len + {%H-}NativeInt(Dest) + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  P := PFirst+1;
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if P^=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (PEnd - Src));
  Inc(Dest, PEnd - Src);
  Dest^ := Quote;
end;

function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

initialization

HexFiller;  //build up lookup table
  {$IFDEF WITH_FORMATSETTINGS}
  FSSqlFloat := FormatSettings;
  {$ELSE}
  FSSqlFloat.CurrencyString := CurrencyString;
  FSSqlFloat.CurrencyFormat := CurrencyFormat;
  FSSqlFloat.NegCurrFormat := NegCurrFormat;
  {$ENDIF}
  FSSqlFloat.DecimalSeparator := '.';
  FSSqlFloat.ThousandSeparator := ',';

end.
