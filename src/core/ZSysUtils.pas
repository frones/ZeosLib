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
  Variants, ZMessages, ZCompatibility, Classes, SysUtils, Types;

type
  {** Modified comaprison function. }
  TZListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  {** Modified list of pointers. }
  TZSortedList = class (TList)
  protected
    procedure QuickSort(SortList: PPointerList; L, R: Integer;
      SCompare: TZListSortCompare);
  public
    procedure Sort(Compare: TZListSortCompare);
  end;

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


{$IFDEF UNICODE}
{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChars
  @param P2 seconds PWideChars
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PChar; Len: Integer): Boolean;
{$ENDIF}

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: string): Boolean; {$IFDEF UNICODE} overload;
function StartsWith(const Str, SubStr: RawByteString): Boolean; overload; {$ENDIF}
{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: string): Boolean;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
{$IFDEF WITH_RAWBYTESTRING}
function SQLStrToFloatDef(Str: RawByteString; Def: Extended): Extended; overload;
{$ENDIF}
function SQLStrToFloatDef(Str: String; Def: Extended): Extended; overload;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloat(const Str: AnsiString): Extended;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
function BufferToStr(Buffer: PWideChar; Length: LongInt): string; overload;
function BufferToStr(Buffer: PAnsiChar; Length: LongInt): string; overload;

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: RawByteString): Boolean; overload;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: ZWideString): Boolean; overload;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToStrEx(Bool: Boolean): String;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(const Str: string): Boolean;

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
function BytesToStr(const Value: TByteDynArray): RawByteString;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: AnsiString): TByteDynArray; overload;

{$IFDEF WITH_RAWBYTESTRING}
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: UTF8String): TByteDynArray; overload;
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: RawByteString): TByteDynArray; overload;
{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
{$ENDIF}
function StrToBytes(const Value: WideString): TByteDynArray; overload;
{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TByteDynArray; overload;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TByteDynArray): Variant;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(const Value: Variant): TByteDynArray;

{**
  Converts Ansi SQL Date/Time to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(const Value: string): TDateTime;

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeWithProperFormat(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL Date (yyyy-mm-dd or DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeFixedSize(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL Date (y*?m*?d* d* or float or  no DateFormat) to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeVaringSize(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL Time (hh:nn:ss or hh:mm:nn.zzz or TimeFormat)
  to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(const Value, TimeFormat: RawByteString;
  var Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateTimeToDateTime(const Value, DateTimeFormat: RawByteString;
  var Failed: Boolean): TDateTime;

{**
  Converts Timestamp String to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function TimestampStrToDateTime(const Value: string): TDateTime;

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
   Copy buffer to the pascal string
   @param Buffer a buffer with data
   @param Length a buffer length
   @return a buffer content
}
function MemPas(Buffer: PChar; Length: LongInt): string;

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

{**
  Arranges thousand and decimal separator to a System-defaults
  @param the value which has to be converted and arranged
  @return a valid floating value
}
function ZStrToFloat(Value: PAnsiChar): Extended; overload;

{**
  Arranges thousand and decimal separator to a System-defaults
  @param the value which has to be converted and arranged
  @return a valid floating value
}
function ZStrToFloat(Value: RawByteString): Extended; overload;

procedure ZSetString(const Src: PAnsiChar; var Dest: AnsiString); overload;
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: AnsiString); overload;
procedure ZSetString(const Src: PAnsiChar; var Dest: UTF8String); overload;
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String); overload;
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: ZWideString); overload;
{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(const Src: PAnsiChar; var Dest: RawByteString); overload;
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString); overload;
{$ENDIF}

function NotEmptyASCII7ToString(const Src: RawByteString): string; overload;
function NotEmptyASCII7ToString(Src: PAnsiChar; Len: integer): string; overload;
function NotEmptyStringToASCII7(const Src: string): RawByteString; overload;
function NotEmptyStringToASCII7(Src: PChar): RawByteString; overload;

function PosEmptyASCII7ToString(const Src: RawByteString): string; overload;
function PosEmptyASCII7ToString(Src: PAnsiChar; Len: integer): string; overload;
function PosEmptyStringToASCII7(const Src: string): RawByteString; overload;
function PosEmptyStringToASCII7(Src: PChar): RawByteString; overload;

function NotEmptyASCII7ToUnicodeString(const Src: RawByteString): ZWideString; overload;
function NotEmptyASCII7ToUnicodeString(Src: PAnsiChar; Len: integer): ZWideString; overload;
function NotEmptyUnicodeStringToASCII7(const Src: ZWideString): RawByteString; overload;
function NotEmptyUnicodeStringToASCII7(Src: PWideChar): RawByteString; overload;

function PosEmptyASCII7ToUnicodeString(const Src: RawByteString): ZWideString; overload;
function PosEmptyASCII7ToUnicodeString(Src: PAnsiChar; Len: integer): ZWideString; overload;
function PosEmptyUnicodeStringToASCII7(const Src: ZWideString): RawByteString; overload;
function PosEmptyUnicodeStringToASCII7(Src: PWideChar): RawByteString; overload;

{ Integer convertion in Raw and Unicode Format}
function IntToRaw(const Value: Integer): RawByteString; overload;
function IntToRaw(const Value: Int64): RawByteString; overload;

function IntToUnicode(const Value: Integer): ZWideString; overload;
function IntToUnicode(const Value: Int64): ZWideString; overload;

function IntToString(const Value: Integer): String; overload;
function IntToString(const Value: Int64): String; overload;

function RawToInt(const Value: RawbyteString): Integer;
function UnicodeToInt(const Value: ZWideString): Integer;
function RawToIntDef(const S: RawByteString; const Default: Integer) : Integer;
function UnicodeToIntDef(const S: ZWideString; const Default: Integer) : Integer;
function RawToInt64Def(const S: RawByteString; const Default: Integer) : Int64;
function UnicodeToInt64Def(const S: ZWideString; const Default: Integer) : Int64;

{ Float convertion in Raw and Unicode Format}
function RawToFloat(const s: RawByteString; const DecimalSep: AnsiChar): Extended;
function RawToFloatDef(const s: RawByteString; const DecimalSep: AnsiChar; const Default: Extended): Extended;
function ValRawExt(const s: RawByteString; const DecimalSep: AnsiChar; var code: Integer): Extended;
function ValRawInt(const s: RawByteString; var code: Integer): Integer;

implementation

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} StrUtils, SysConst,
  ZMatchPattern, Math;


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
    Index := Pos(Delimiters[I], Str);
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
    Index := Pos(Str[I], Delimiters);
    if (Index > 0) then
    begin
      Result := I;
      Break;
    end;
  end;
end;


{$IFDEF UNICODE}
{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChar
  @param P2 seconds PWideChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;
begin
  while (Len > 0) and (P1^ = P2^) do
  begin
    Inc(P1);
    Inc(P2);
    Dec(Len);
  end;
  Result := Len = 0;
end;
{$ENDIF}

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;
begin
  while (Len > 0) and (P1^ = P2^) do
  begin
    Inc(P1);
    Inc(P2);
    Dec(Len);
  end;
  Result := Len = 0;
end;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: string): Boolean;
var
  LenSubStr: Integer;
begin
  LenSubStr := Length(SubStr);
  if SubStr = '' then
    Result := True
   else if LenSubStr <= Length(Str) then
    //Result := Copy(Str, 1, Length(SubStr)) = SubStr;
   {$IFDEF UNICODE}
   Result := MemLCompUnicode(PChar(Str), PChar(SubStr), LenSubStr)
   {$ELSE}
   Result := MemLCompAnsi(PChar(Str), PChar(SubStr), LenSubStr)
   {$ENDIF}
  else
    Result := False;
end;

{$IFDEF UNICODE}
function StartsWith(const Str, SubStr: RawByteString): Boolean; overload;
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
{$ENDIF}

{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: string): Boolean;
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
      //Result := Copy(Str, LenStr - LenSubStr + 1, LenSubStr) = SubStr
    {$IFDEF UNICODE}
      Result := MemLCompUnicode(PChar(Pointer(Str)) + LenStr - LenSubStr,
         Pointer(SubStr), LenSubStr)
    {$ELSE}
      Result := MemLCompAnsi(PChar(Pointer(Str)) + LenStr - LenSubStr,
         Pointer(SubStr), LenSubStr)
    {$ENDIF}
    else
      Result := False;
  end;
end;

function ConvertMoneyToFloat(MoneyString: String): String;
var
  I: Integer;
begin
  if MoneyString = '' then
    Result := ''
  else
  begin
    if CharInSet(Char(MoneyString[1]), ['0'..'9', '-']) then
      Result := MoneyString
    else
      for i := 1 to Length(MoneyString) do
        if CharInSet(Char(MoneyString[I]), ['0'..'9', '-']) then
        begin
          if I > 1 then
          begin //Money type
            Result := Copy(MoneyString, I, Length(MoneyString)-i+1);
            if Pos(',', Result) > 0 then
              if Pos('.', Result) > 0  then
              begin
                Result := Copy(Result, 1, Pos(',', Result)-1);
                while Pos('.', Result) > 0  do
                  Result := Copy(Result, 1, Pos('.', Result)-1)+Copy(Result, Pos('.', Result)+1, Length(Result)); //remove ThousandSeparator
                Result := Result + '.'+Copy(MoneyString, Pos(',', MoneyString)+1, Length(MoneyString));
              end
              else
                Result[Pos(',', Result)] := '.';
          end;
          Break;
        end;
  end;
end;
{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
{$IFDEF WITH_RAWBYTESTRING}
function SQLStrToFloatDef(Str: RawByteString; Def: Extended): Extended;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  AString: String;
begin
  if Str = '' then
    Result := Def
  else
  begin
    OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
    OldThousandSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator;
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := ',';
    if not CharInSet(Char(NotEmptyASCII7ToString(Str)[1]), ['0'..'9', '-']) then
      AString := ConvertMoneyToFloat(NotEmptyASCII7ToString(Str))
    else
      AString := NotEmptyASCII7ToString(Str);
    Result := StrToFloatDef(AString, Def);
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldThousandSeparator;
  end;
end;
{$ENDIF}

function SQLStrToFloatDef(Str: String; Def: Extended): Extended;
var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  AString: String;
begin
  if Str = '' then
    Result := Def
  else
  begin
    OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
    OldThousandSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator;
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := ',';
    if not CharInSet(Char(Str[1]), ['0'..'9', '-']) then
      AString := ConvertMoneyToFloat(Str)
    else
      AString := Str;
    Result := StrToFloatDef(AString, Def);
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldThousandSeparator;
  end;
end;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloat(const Str: AnsiString): Extended;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
  try
    Result := StrToFloat(String(Str));
  finally
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
  end;
end;

{ Convert string buffer into pascal string }

function BufferToStr(Buffer: PWideChar; Length: LongInt): string;
var s : Widestring;
begin
   Result := '';
   if Assigned(Buffer) then
   begin
      SetString(s, Buffer, Length div SizeOf(Char));
      Result := s;
   end;
end;

{ Convert string buffer into pascal string }

function BufferToStr(Buffer: PAnsiChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: RawByteString): Boolean;
begin
  Str := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}UpperCase(Str);
  Result := (Str = 'Y') or (Str = 'YES') or (Str = 'T') or (Str = 'TRUE')
    or (RawToIntDef(Str, 0) <> 0);
end;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: ZWideString): Boolean;
var tmp: String;
begin
  tmp := UpperCase({$IFNDEF UNICODE}String{$ENDIF}(Str));
  Result := (tmp = 'Y') or (tmp = 'YES') or (tmp = 'T') or (tmp = 'TRUE')
    or (StrToIntDef(tmp, 0) <> 0);
end;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToStrEx(Bool: Boolean): String;
begin
  if Bool then
    Result := 'True'
  else
    Result := 'False';
end;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(const Str: string): Boolean;
var
  I, N, M, Pos: Integer;
begin
  if IsMatch('*.*.*.*', Str) then
  begin
    N := 0;
    M := 0;
    Pos := 1;
    for I := 1 to Length(Str) do
    begin
      if I - Pos > 3 then
        Break;
      if Str[I] = '.' then
      begin
       if StrToInt(Copy(Str, Pos, I - Pos)) > 255 then
         Break;
       Inc(N);
       Pos := I + 1;
      end;
      if CharInSet(Str[I], ['0'..'9']) then
        Inc(M);
    end;
    Result := (M + N = Length(Str)) and (N = 3);
  end
  else
    Result := False;
end;

procedure SplitToStringList(List: TStrings; Str: string; const Delimiters: string);
var
  DelimPos: Integer;
begin
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
end;

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
      Move(Pointer(Delimiter)^, P^, DelimLen * SizeOf(Char));
      Inc(P, DelimLen);
    end;
    S := List[i];
    Len := Length(S);
    if Len > 0 then
    begin
      Move(Pointer(S)^, P^, Len * SizeOf(Char));
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
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
  try
    Result := FloatToStr(Value);
  finally
    {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
  end;
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
function BytesToStr(const Value: TByteDynArray): RawByteString;
{$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
var L: Integer;
{$ENDIF}
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Result := '';
  L := Length(Value);
  SetLength(Result, L);
  System.Move(Pointer(Value)^, PAnsiChar(Result)^, L);
  {$ELSE}
  SetString(Result, PAnsiChar(@Value[0]), Length(Value))
  {$ENDIF}
end;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: AnsiString): TByteDynArray;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    Move(Value[1], Result[0], L)
end;

{$IFDEF WITH_RAWBYTESTRING}
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: UTF8String): TByteDynArray;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    Move(Value[1], Result[0], L)
end;
{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: RawByteString): TByteDynArray;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    Move(Value[1], Result[0], L)
end;
{$ENDIF}
{**
  Converts a WideString into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
function StrToBytes(const Value: WideString): TByteDynArray;
var
  L: Integer;
  RBS: RawByteString;
begin
  RBS := RawByteString(Value);
  L := Length(RBS);
  SetLength(Result, L);
  if Value <> '' then
    Move(RBS[1], Result[0], L)
end;
{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TByteDynArray;
var L: Integer;
begin
  L := Length(Value) * SizeOf(Char);
  SetLength(Result, L);
  if Value <> '' then
    Move(Value[1], Result[0], L)
end;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TByteDynArray): Variant;
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
function VarToBytes(const Value: Variant): TByteDynArray;
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

  procedure ExtractTime(AString: String);
  var dotPos: Integer;
  begin
    Hour := StrToIntDef(Copy(AString, 1, 2), 0);
    Min := StrToIntDef(Copy(AString, 4, 2), 0);
    Sec := StrToIntDef(Copy(AString, 7, 2), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    dotPos := 0;
    MSec := 0;
    if Length(AString) > 8 then
      dotPos :=Pos ('.', AString);

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

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeWithProperFormat(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;
var
  Year, Month, Day: Word;
  I, LenDateFormat, Code, YPos, MPos, DPos: Integer;
  PDateFormat: PAnsiChar;
  rYear: Array [0..3] of AnsiChar;
  rMonth, rDay: Array [0..1] of AnsiChar;
begin
  Result := 0;
  LenDateFormat := Length(DateFormat);
  Failed := False;
  if not (Value = '') then
  begin
    Failed := LenDateFormat = 0;
    if not Failed then
    begin
      if DateFormat = 'FLOAT' then
      begin
        Result := ValRawExt(Value, '.', Code);
        Failed := Code <> 0;
        if Failed then Result := 0;
      end
      else
      begin
        YPos := 0; MPos := 0; DPos := 0;
        PDateFormat := PAnsiChar(DateFormat);
        FillChar(rYear, 4, 0);
        for i := 1 to LenDateFormat do
        begin
          case PDateFormat^ of
            'Y', 'y':
              begin
                rYear[YPos] := Value[i];
                Inc(YPos);
              end;
            'M', 'm':
              begin
                rMonth[MPos] := Value[i];
                Inc(MPos);
              end;
            'D', 'd':
              begin
                rDay[DPos] := Value[i];
                Inc(DPos);
              end;
          end;
          Inc(PDateFormat);
        end;
        Year := RawToIntDef(rYear, 0);
        Month := RawToIntDef(rMonth, 0);
        Day := RawToIntDef(rDay, 0);
        Failed := not ((Year <> 0) and (Month <> 0) and (Day <> 0));
        if not Failed then
          try
            Result := EncodeDate(Year, Month, Day);
          except
          end;
      end;
    end;
  end;
end;

{**
  Converts Ansi SQL Date (yyyy-mm-dd or DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeFixedSize(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;
var
  Year, Month, Day: Word;
begin
  Result := 0;
  Failed := False;
  if not (Value = '') then
  begin
    Failed := not ( Length(Value) = 10 );
    if not Failed then
    begin
      Year := RawToIntDef(Copy(Value, 1, 4), 0);
      Month := RawToIntDef(Copy(Value, 6, 2), 0);
      Day := RawToIntDef(Copy(Value, 9, 2), 0);
      Failed := not ( (Year <> 0) and (Month <> 0) and (Day <> 0) ) ;
      if not Failed then
        try
          Result := EncodeDate(Year, Month, Day);
        except
        end;
    end;
  end;
end;

{**
  Converts Ansi SQL Date (y*?m*?d* or float or no DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTimeVaringSize(const Value, DateFormat: RawByteString;
  var Failed: Boolean): TDateTime;
var
  Year, Month, Day: Word;
  DateLenCount, DateLen, MonthLen, YPos, MPos, DPos, Code: Integer;
  PDateValue: PAnsiChar;
  rYear, rMonth, rDay: Array [0..10] of AnsiChar;
  Tmp: RawByteString;
begin
  Result := 0;
  Failed := False;
  if not (Value = '') then
  begin
    DateLen := Length(Value);
    YPos := 0; MPos := 0; DPos := 0; DateLenCount := 0;
    PDateValue := PAnsiChar(Value);
    FillChar(rYear, 11, 0);
    FillChar(rMonth, 11, 0);
    FillChar(rDay, 11, 0);
    while ( DateLenCount < DateLen ) and (not (PDateValue^ in ['-','/','\']) ) do
    begin
      rYear[YPos] := PDateValue^;
      Inc(YPos);
      Inc(PDateValue);
      Inc(DateLenCount);
    end;
    while (not (PDateValue^ in ['0'..'9']) ) and ( DateLenCount < DateLen ) do
    begin
      Inc(PDateValue);
      Inc(DateLenCount);
    end;
    while ( DateLenCount < DateLen ) and (not (PDateValue^ in ['-','/','\']) ) do
    begin
      rMonth[MPos] := PDateValue^;
      Inc(MPos);
      Inc(PDateValue);
      Inc(DateLenCount);
    end;
    while (not (PDateValue^ in ['0'..'9']) ) and ( DateLenCount < DateLen ) do
    begin
      Inc(PDateValue);
      Inc(DateLenCount);
    end;
    while ( DateLenCount < DateLen ) and (not (PDateValue^ in ['-','/','\']) ) do
    begin
      rDay[DPos] := PDateValue^;
      Inc(DPos);
      Inc(PDateValue);
      Inc(DateLenCount);
    end;
    Tmp := rYear;
    DateLenCount := Length(Tmp);
    Tmp := rMonth;
    MonthLen := Length(Tmp);
    if MonthLen > 2 then //float value
    begin
      Result := Trunc(ValRawExt(Value, '.', Code));
      if Code <> 0 then
        Result := 0;
      Exit;
    end;
    if DateLenCount > 4 then //We've a problem! No date delimiters found? -> YYYYMMDD or YYMMDD or ....Float!
      if DateLenCount = 8 then
      begin //Let's start from the premise we've LongDateFormat YYYYMMDD
        rMonth[0] := rYear[4];
        rMonth[1] := rYear[5];
        rDay[0] := rYear[6];
        rDay[1] := rYear[7];
        FillChar((PAnsiChar(@rYear)+4)^, 4, 0); //Reset useless chars
      end
      else
        if DateLenCount = 6 then
        //Let's start from the premise we've ShortDateFormat YYMMDD
        begin
          rMonth[0] := rYear[2];
          rMonth[1] := rYear[3];
          rDay[0] := rYear[4];
          rDay[1] := rYear[5];
          FillChar((PAnsiChar(@rYear)+2)^, 4, 0);
        end
        else
        begin
          Result := Trunc(ValRawExt(Value, '.', Code));
          if Code <> 0 then
            Result := 0;
          Exit;
        end;
    Year := RawToIntDef(rYear, 0);
    Month := RawToIntDef(rMonth, 0);
    Day := RawToIntDef(rDay, 0);
    Failed := not ( (Year <> 0) and (Month <> 0) and (Day <> 0) );
    if not Failed then
      try
        Result := EncodeDate(Year, Month, Day); //Reset useless chars
      except
        Result := Trunc(ValRawExt(Value, '.', Code));
        if Code <> 0 then
          Result := 0;
      end
    else
    begin
      Result := Trunc(ValRawExt(Value, '.', Code));
      if Code <> 0 then
        Result := 0;
    end;
  end;
end;

{**
  Converts Ansi SQL Time (hh-nn-ss or hh:mm:nn.zzz or TimeFormat)
  to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(const Value, TimeFormat: RawByteString;
  var Failed: Boolean): TDateTime;
var
  Hour, Minute, Sec, MSec: Word;
  I, HPos, NPos, SPos, MPos, CodeH, CodeN, CodeS, CodeM, LVal, LFormat: Integer;
  PTimeFormat: PAnsiChar;
  rHour, rMin, rSec: Array [0..1] of AnsiChar;
  rMSec: Array [0..2] of AnsiChar;
begin
  Result := 0;
  Failed := False;
  if not (Value = '') then
  begin
    LVal := Length(Value);
    LFormat := Length(TimeFormat);
    if (TimeFormat = '') or ( LFormat <> LVal) then
    begin
      Hour := ValRawInt(Copy(Value, 1, 2), CodeH);
      Minute := ValRawInt(Copy(Value, 4, 2), CodeN);
      Sec := ValRawInt(Copy(Value, 7, 2), CodeS);
      if LVal > 8 then
        MSec := ValRawInt(Copy(Value, 10, 3), CodeM)
      else
        MSec := 0;
    end
    else
    begin
      HPos := 0; NPos := 0; SPos := 0; MPos := 0;
      PTimeFormat := PAnsiChar(TimeFormat);
      for i := 1 to LFormat do
      begin
        case PTimeFormat^ of
          'H', 'h':
            begin
              rHour[HPos] := Value[i];
              Inc(HPos);
            end;
          'N', 'n':
            begin
              rMin[NPos] := Value[i];
              Inc(NPos);
            end;
          'S', 's':
            begin
              rSec[SPos] := Value[i];
              Inc(SPos);
            end;
          'Z', 'z':
            begin
              rMSec[MPos] := Value[i];
              Inc(MPos);
            end;
        end;
        Inc(PTimeFormat);
      end;
      Hour := ValRawInt(rHour, CodeH);
      Minute := ValRawInt(rMin, CodeN);
      Sec := ValRawInt(rSec, CodeS);
      MSec := ValRawInt(rMSec, CodeM);
    end;
    Failed := ( CodeH or CodeN or CodeS or CodeM) <> 0;
    if (not Failed) then
      try
        Result := EncodeTime(Hour, Minute, Sec, MSec);
      except
      end;
  end;
end;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateTimeToDateTime(const Value, DateTimeFormat: RawByteString;
  var Failed: Boolean): TDateTime;
var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;
  I, LVal, LFormat,
  YPos, MPos, DPos, HPos, NPos, SPos, MSPos,
  CodeY, CodeM, CodeD, CodeH, CodeN, CodeS, CodeMS: Integer;
  PDateTimeFormat: PAnsiChar;
  rYear: Array [0..3] of AnsiChar;
  rMonth, rDay, rHour, rMin, rSec: Array [0..1] of AnsiChar;
  rMSec: Array [0..2] of AnsiChar;
begin
  Result := 0;
  Failed := False;
  MSec := 0;
  if not (Value = '') then
  begin
    LVal := Length(Value);
    LFormat := Length(DateTimeFormat);
    if ( LFormat = 0 ) or ( LVal <> LFormat ) then
    begin
      Year := ValRawInt(Copy(Value, 1, 4), CodeY);
      Month := ValRawInt(Copy(Value, 6, 2), CodeM);
      Day := ValRawInt(Copy(Value, 9, 2), CodeD);
      Hour := ValRawInt(Copy(Value, 11, 2), CodeH);
      Minute := ValRawInt(Copy(Value, 14, 2), CodeN);
      Sec := ValRawInt(Copy(Value, 17, 2), CodeS);
      if Length(Value) > 19 then
        MSec := ValRawInt(Copy(Value, 20, 3), CodeM);
    end
    else
    begin
      YPos := 0; MPos := 0; DPos := 0; HPos := 0; NPos := 0; SPos := 0; MSPos := 0;
      PDateTimeFormat := PAnsiChar(DateTimeFormat);
      for i := 1 to Length(PDateTimeFormat) do
      begin
        case PDateTimeFormat^ of
          'Y', 'y':
            begin
              rYear[YPos] := Value[i];
              Inc(YPos);
            end;
          'M', 'm':
            begin
              rMonth[MPos] := Value[i];
              Inc(MPos);
            end;
          'D', 'd':
            begin
              rDay[DPos] := Value[i];
              Inc(DPos);
            end;
          'H', 'h':
            begin
              rHour[HPos] := Value[i];
              Inc(HPos);
            end;
          'N', 'n':
            begin
              rMin[NPos] := Value[i];
              Inc(NPos);
            end;
          'S', 's':
            begin
              rSec[SPos] := Value[i];
              Inc(SPos);
            end;
          'Z', 'z':
            begin
              rMSec[MSPos] := Value[i];
              Inc(MSPos);
            end;
        end;
        Inc(PDateTimeFormat);
      end;
      Year := ValRawInt(rYear, CodeY);
      Month := ValRawInt(rMonth, CodeM);
      Day := ValRawInt(rMonth, CodeD);
      Hour := ValRawInt(rHour, CodeH);
      Minute := ValRawInt(rMin, CodeN);
      Sec := ValRawInt(rSec, CodeS);
      MSec := ValRawInt(rMSec, CodeMS);
    end;
    Failed := ( ( CodeY or CodeM or CodeD or CodeH or CodeN or CodeS or CodeMS) <> 0) and
      ( (Year <> 0) and (Month <> 0) and (Day <> 0) );
    if not Failed then
    begin
      try
        Result := EncodeDate(Year, Month, Day);
      except end;
      try
        if Result >= 0 then
          Result := Result + EncodeTime(Hour, Minute, Sec, MSec)
        else
          Result := Result - EncodeTime(Hour, Minute, Sec, MSec)
      except end;
    end;
  end;
end;

{**
  Converts Timestamp String to TDateTime
  @param Value a timestamp string.
  @return a decoded TDateTime value.
}
function TimestampStrToDateTime(const Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Integer;
  StrLength, StrPos, StrPosPrev: Integer;
  //
  function CharMatch( matchchars: string ): boolean;
  // try to match as much characters as possible
  begin
    StrPosPrev:= StrPos;
    Result:= false;
    while StrPos<=StrLength do
       if pos(Value[StrPos], matchchars) > 0 then
         begin
            inc(StrPos);
            Result := true;
         end
       else
         break;
  end;
begin
  Result := 0;
  StrPos:= 1;
  StrLength := Length(Value);

  if not CharMatch('1234567890') then
     exit; // year
  Year := StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  if not CharMatch('-/\') then
     exit;
  if not CharMatch('1234567890') then
     exit; // month
  Month:= StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  if not CharMatch('-/\') then
     exit;
  if not CharMatch('1234567890') then
     exit; // day
  Day:= StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  try
    Result := EncodeDate(Year, Month, Day);
  except
  end;
  //
  if not CharMatch(' ') then
     exit;
  if not CharMatch('1234567890') then
     exit; // hour
  Hour := StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  if not CharMatch('-/\') then
     exit;
  if not CharMatch('1234567890') then
     exit; // minute
  Min:= StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  if not CharMatch('-/\') then
     exit;
  if not CharMatch('1234567890') then
     exit; // second
  Sec:= StrToIntDef(Copy(Value, StrPosPrev, StrPos-StrPosPrev), 0);
  try
    Result := REsult + EncodeTime(Hour, Min, Sec,0);
  except
  end;

end;


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
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value)
end;

{ TZSortedList }

{**
  Performs quick sort algorithm for the list.
}
procedure TZSortedList.QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TZListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (I < R) And (SCompare(SortList^[I], P) < 0) do //check I against R too since the pointer can be nil
        Inc(I);
      while (J > L) And (SCompare(SortList^[J], P) > 0) do //check j against L too since the pointer can be nil
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

{**
  Performs sorting for this list.
  @param Compare a comparison function.
}
procedure TZSortedList.Sort(Compare: TZListSortCompare);
begin
  if (List <> nil) and (Count > 0) then
    {$IFDEF DELPHI16_UP}
    QuickSort(@List, 0, Count - 1, Compare);
    {$ELSE}
    QuickSort(List, 0, Count - 1, Compare);
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
  SrcBuffer := PChar(Value);
  DestLength := 0;
  for I := 1 to SrcLength do
  begin
    if CharInSet(SrcBuffer^, [#0]) then
       Inc(DestLength, 4)
    else if CharInSet(SrcBuffer^, ['"', '''', '\']) then
       Inc(DestLength, 2)
    else
       Inc(DestLength);
    Inc(SrcBuffer);
  end;

  SrcBuffer := PChar(Value);
  SetLength(Result, DestLength);
  DestBuffer := PChar(Result);

  for I := 1 to SrcLength do
  begin
    if CharInSet(SrcBuffer^, [#0]) then
    begin
      DestBuffer[0] := '\';
      DestBuffer[1] := Chr(Ord('0') + (Byte(SrcBuffer^) shr 6));
      DestBuffer[2] := Chr(Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07));
      DestBuffer[3] := Chr(Ord('0') + (Byte(SrcBuffer^) and $07));
      Inc(DestBuffer, 4);
    end
    else if CharInSet(SrcBuffer^, ['"', '''', '\']) then
    begin
      DestBuffer[0] := '\';
      DestBuffer[1] := SrcBuffer^;
      Inc(DestBuffer, 2);
    end
    else
    begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
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
  SrcBuffer := PChar(Value);
  SetLength(Result, SrcLength);
  DestLength := 0;
  DestBuffer := PChar(Result);

  while SrcLength > 0 do
  begin
    if SrcBuffer^ = '\' then
    begin
      Inc(SrcBuffer);
      if CharInSet(SrcBuffer^, ['0'..'9']) then
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
    end
    else
    begin
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
   Copy buffer to the pascal string
   @param Buffer a buffer with data
   @param Length a buffer length
   @return a buffer content
}
function MemPas(Buffer: PChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
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
 Result := IntToString(MajorVersion)+'.'+IntToString(MinorVersion)+'.'+IntToString(SubVersion);
end;

{**
  Arranges thousand and decimal separator to a System-defaults
  @param the value which has to be converted and arranged
  @return a valid floating value
}
function ZStrToFloat(Value: PAnsiChar): Extended;
var
  OldDecimalSeparator, OldThousandSeparator: Char;
begin
  OldDecimalSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  OldThousandSeparator := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator;

  if AnsiStrPos(PAnsiChar(Value), PAnsiChar(AnsiString(OldDecimalSeparator))) = nil then
    if AnsiStrPos(PAnsiChar(Value), PAnsiChar(AnsiString(OldThousandSeparator))) = nil then
      //No DecimalSeparator and no ThousandSeparator
      Result := StrToFloat(NotEmptyASCII7ToString(Value))
    else
    begin
      //wrong DecimalSepartor
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldThousandSeparator;
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldDecimalSeparator;
      Result := StrToFloat({$IFDEF UNICODE}NotEmptyASCII7ToString{$ENDIF}(Value));
    end
  else
    if AnsiStrPos(PAnsiChar(Value), PAnsiChar(AnsiString(OldThousandSeparator))) = nil then
      //default DecimalSepartor
      Result := StrToFloat({$IFDEF UNICODE}NotEmptyASCII7ToString{$ENDIF}(Value))
    else
      if StrLen(AnsiStrPos(PAnsiChar(Value), PAnsiChar(AnsiString(OldDecimalSeparator)))) <
        StrLen(AnsiStrPos(PAnsiChar(Value), PAnsiChar(AnsiString(OldThousandSeparator)))) then
          //default DecimalSepartor and ThousandSeparator
        Result := StrToFloat({$IFDEF UNICODE}NotEmptyASCII7ToString{$ENDIF}(Value))
      else
      begin
        //wrong DecimalSepartor and ThousandSeparator
        {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldThousandSeparator;
        {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldDecimalSeparator;
        Result := StrToFloat({$IFDEF UNICODE}NotEmptyASCII7ToString{$ENDIF}(Value));
      end;

  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator := OldThousandSeparator;
end;

{**
  Arranges thousand and decimal separator to a System-defaults
  @param the value which has to be converted and arranged
  @return a valid floating value
}
function ZStrToFloat(Value: RawByteString): Extended;
begin
  Result := ZStrToFloat(PAnsiChar(Value));
end;

procedure ZSetString(const Src: PAnsiChar; var Dest: AnsiString);
begin
  SetString(Dest, Src, StrLen(Src));
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: AnsiString);
begin
  SetString(Dest, Src, Len);
end;

procedure ZSetString(const Src: PAnsiChar; var Dest: UTF8String);
{$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
var Len: Integer;
{$ENDIF}
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Dest := '';
  Len := StrLen(Src);
  SetLength(Dest, Len);
  Move(Src^, PAnsiChar(Dest)^, Len);
  {$ELSE}
  SetString(Dest, Src, StrLen(Src));
  {$ENDIF}
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String);
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Dest := '';
  SetLength(Dest, Len);
  Move(Src^, PAnsiChar(Dest)^, Len);
  {$ELSE}
  SetString(Dest, Src, Len);
  {$ENDIF}
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: ZWideString); overload;
begin
  Dest := ''; //speeds up for SetLength
  if ( Len = 0 ) or ( Src = nil ) then
    Exit
  else
  begin
    SetLength(Dest, Len div 2);
    Move(Src^, PWideChar(Dest)^, Len);
  end;
end;

{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(const Src: PAnsiChar; var Dest: RawByteString);
{$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
var Len: Integer;
{$ENDIF}
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Dest := '';
  Len := StrLen(Src);
  SetLength(Dest, Len);
  Move(Src^, PAnsiChar(Dest)^, Len);
  {$ELSE}
  SetString(Dest, Src, StrLen(Src));
  {$ENDIF}
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString);
begin
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Dest := '';
  SetLength(Dest, Len);
  Move(Src^, PAnsiChar(Dest)^, Len);
  {$ELSE}
  SetString(Dest, Src, Len);
  {$ENDIF}
end;
{$ENDIF}

function NotEmptyASCII7ToString(const Src: RawByteString): string;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  l := Length(Src); //temp l speeds x2
  SetString(result,nil,l);
  for i := 0 to l-1 do
    PWordArray(result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function NotEmptyASCII7ToString(Src: PAnsiChar; Len: integer): string;
{$IFDEF UNICODE}
var i: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  System.SetString(result, nil, Len);
  for i := 0 to Len-1 do
    PWordArray(Result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  {$ELSE}
  System.SetString(Result, PAnsiChar(Src), Len);
  {$ENDIF}
end;

function NotEmptyStringToASCII7(const Src: string): RawByteString;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  L := System.Length(Src); //temp l speeds x2
  System.SetString(Result,nil, l);
  for i := 0 to l-1 do
    PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;


function NotEmptyStringToASCII7(Src: PChar): RawByteString;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  L := System.Length(Src); //temp l speeds x2
  System.SetString(Result,nil, l);
  for i := 0 to l-1 do
    PByteArray(Result)[i] := PWordArray(Src)[i];
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function PosEmptyASCII7ToString(const Src: RawByteString): string;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  l := Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyASCII7ToString but is 2x faster if L = 0
    Result := ''
  else
  begin
    SetString(result,nil,l);
    for i := 0 to l-1 do
      PWordArray(result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  end;
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function PosEmptyASCII7ToString(Src: PAnsiChar; Len: integer): string;
{$IFDEF UNICODE}
var i: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  if Len = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyASCII7ToString but is 2x faster if Len = 0
    Result := ''
  else
  begin
    System.SetString(result, nil, Len);
    for i := 0 to Len-1 do
      PWordArray(Result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  end;
  {$ELSE}
  System.SetString(Result, PAnsiChar(Src), Len);
  {$ENDIF}
end;

function PosEmptyStringToASCII7(const Src: string): RawByteString;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  L := System.Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyStringToASCII7 but is 2x faster if L = 0
    Result := ''
  else
  begin
    System.SetString(Result,nil, l);
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
  end;
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;


function PosEmptyStringToASCII7(Src: PChar): RawByteString;
{$IFDEF UNICODE}
var i, l: integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  L := system.Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyStringToASCII7 but is 2x faster if L = 0
    Result := ''
  else
  begin
    System.SetString(Result,nil, l);
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
  end;
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function NotEmptyASCII7ToUnicodeString(const Src: RawByteString): ZWideString;
var i, l: integer;
begin
  l := Length(Src); //temp l speeds x2
  SetString(result,nil,l);
  for i := 0 to l-1 do
    PWordArray(result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
end;

function NotEmptyASCII7ToUnicodeString(Src: PAnsiChar; Len: integer): ZWideString;
var i: integer;
begin
  System.SetString(result, nil, Len);
  for i := 0 to Len-1 do
    PWordArray(Result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
end;

function NotEmptyUnicodeStringToASCII7(const Src: ZWideString): RawByteString;
var i, l: integer;
begin
  L := System.Length(Src); //temp l speeds x2
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Result := ''; //speeds up SetLength x2
  SetLength(Result, l);
  {$ELSE}
  System.SetString(Result,nil, l);
  {$ENDIF}
  for i := 0 to l-1 do
    PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
end;


function NotEmptyUnicodeStringToASCII7(Src: PWideChar): RawByteString;
var i, l: integer;
begin
  L := System.Length(Src); //temp l speeds x2
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  Result := ''; //speeds up SetLength x2
  SetLength(Result, l);
  {$ELSE}
  System.SetString(Result,nil, l);
  {$ENDIF}
  for i := 0 to l-1 do
    PByteArray(Result)[i] := PWordArray(Src)[i];
end;

function PosEmptyASCII7ToUnicodeString(const Src: RawByteString): ZWideString;
var i, l: integer;
begin
  l := Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyASCII7ToString but is 2x faster if L = 0
    Result := ''
  else
  begin
    SetString(result,nil,l);
    for i := 0 to l-1 do
      PWordArray(result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  end;
end;

function PosEmptyASCII7ToUnicodeString(Src: PAnsiChar; Len: integer): ZWideString;
var i: integer;
begin
  if Len = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyASCII7ToString but is 2x faster if Len = 0
    Result := ''
  else
  begin
    System.SetString(result, nil, Len);
    for i := 0 to Len-1 do
      PWordArray(Result)[i] := PByteArray(Src)[i]; //0..254 equals to widechars
  end;
end;

function PosEmptyUnicodeStringToASCII7(const Src: ZWideString): RawByteString;
var i, l: integer;
begin
  L := System.Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyStringToASCII7 but is 2x faster if L = 0
    Result := ''
  else
  begin
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := ''; //speeds up SetLength x2
    SetLength(Result, l);
    {$ELSE}
    System.SetString(Result,nil, l);
    {$ENDIF}
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
  end;
end;


function PosEmptyUnicodeStringToASCII7(Src: PWideChar): RawByteString;
var i, l: integer;
begin
  L := system.Length(Src); //temp l speeds x2
  if L = 0 then   //this line eats 30ms in average of exec count 10.000.000x against NotEmptyStringToASCII7 but is 2x faster if L = 0
    Result := ''
  else
  begin
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := ''; //speeds up SetLength x2
    SetLength(Result, l);
    {$ELSE}
    System.SetString(Result,nil, l);
    {$ENDIF}
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..254 equals to widechars
  end;
end;

{$IF defined (WIN32) and not defined(FPC)}
procedure StrToIntError(const S: string);
begin
  raise EConvertError.CreateResFmt(@SInvalidInteger, [S]);
end;
{$IFEND}

function RawToInt(const Value: RawbyteString): Integer;
{$IF defined (WIN32) and not defined(FPC)}
//originally wrtten by John O'Harrow
//http://fastcode.sourceforge.net/
//function StrToInt32_JOH_IA32_7_b(const S: string) : Integer;
asm
  test  eax, eax
  jz    @@Failed
  push  eax
  push  ebx
  push  edi
  push  esi
  mov   edx, eax            {String Pointer}
  xor   ebx, ebx            {Clear Sign Flag (top bit) and Valid Flag}
  xor   eax, eax            {Clear Result}
  mov   edi, '0'
  mov   esi, 9
@@Trim:                     {Strip Leading Spaces}
  movzx ecx, [edx]
  inc   edx
  cmp   cl, ' '
  je    @@Trim
  cmp   ecx, edi            {cl <= '0'?}
  jle   @@CheckFirstChar    {Yes, Check +, -, $, 0x, 0X}
  test  cl, not 'x'
  jz    @@CheckX            {May start with 'x' or 'X'}

@@Numeric:
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@TestValid         {Not '0'..'9'}
  mov   eax, ecx            {Result := Digit}

  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+1]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+2]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+3]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+4]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+5]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+6]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+7]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

@@NumLoop:
  movzx ecx, [edx+8]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax, MaxInt/10
  ja    @@Error
  inc   edx
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  jmp   @@NumLoop

@@TestValid:
  test  bl, bl              {Got Valid Number?}
  jz    @@Error             {No, Error}
@@CheckDone:
  add   ecx, edi            {Last Character = Null Terminator?}
  jnz   @@Error             {No, Error}
  sar   ebx, 31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax, ebx            {Negate Result if Necessary}
  sub   eax, ebx
  pop   esi
  pop   edi
  pop   ebx
  pop   ecx
  ret

@@NumDone:
  cmp   eax, $80000000
  jb    @@CheckDone         {No Overflow}
  jne   @@Error             {Overflow}
  test  ebx, ebx            {Sign Flag Set?}
  js    @@CheckDone         {Yes, Result is Valid (-MaxInt-1)}
@@Error:
  pop   esi
  pop   edi
  pop   ebx
  pop   eax
@@Failed:
  jmp   StrToIntError

@@CheckFirstChar:           {First Char <= '0'}
  cmp   cl, '$'
  je    @@Hex
  cmp   cl, '-'
  je    @@Minus
  cmp   cl, '+'
  je    @@Plus
  cmp   ecx, edi            {Starts with '0'?}
  jne   @@Error             {No, Error}
  movzx ecx, [edx]          {Character after '0'}
  mov   bl, 1               {Valid := True}
  inc   edx
  jmp   @@CheckX
@@Minus:
  mov   ebx, $80000000      {Set Sign Flag (Top Bit), Valid := False}
@@Plus:
  movzx ecx, [edx]          {Character after '+' or '-'}
  inc   edx
  cmp   cl, '$'
  je    @@Hex               {Starts with '+$' or '-$'}
  cmp   ecx, edi            {Starts with '+0' or '-0'?}
  jne   @@CheckAlpha        {No, May start with '+x', '-x', '+X' or '-X'}
  movzx ecx, [edx]          {Character after '+0' or '-0'}
  inc   ebx                 {Starts with '+0' or '-0', Valid := True}
  inc   edx
@@CheckAlpha:
  test  cl, not 'x'         {Could Char be 'x' or 'X'?}
  jnz   @@Numeric           {No, Assume Numeric}
@@CheckX:
  or    cl, $20             {'X' -> 'x'}
  cmp   cl, 'x'             {Char = 'X' or 'x'?}
  movzx ecx, [edx-1]        {Reload Character}
  jne   @@Numeric           {Does Not start with +/-('x', 'X', '0x' or '0X')}
  mov   bl, 0               {Reset Valid to False}
@@Hex:
  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex1              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@TestValid         {Check for Valid and Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex1:
  mov   eax, ecx            {Result = Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex2              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex2:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex3              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex3:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex4              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex4:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex5              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex5:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex6              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex6:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex7              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex7:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex8              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex8:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

@@HexLoop:
  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@CheckRange        {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@CheckRange:
  cmp   eax, MaxInt/8       {High(ULONG) div 16}
  ja    @@Error             {Overflow}
  shl   eax, 4
(*
  shl   eax, 4              //Using this instead of the above 3 lines wrongly
  jc    @@Error             //  passes validation with S='$200000000000000'
*)
  add   eax, ecx            {Result = Result * 16 + Digit}
  inc   edx
  jmp   @@HexLoop
{$ELSE}
  {$IFDEF UNICODE}
  //function StrToInt32_JOH_PAS_7_c(const s: string): Longint;
  //originally wrtten by John O'Harrow
  //http://fastcode.sourceforge.net/
  const
    AdjustLowercase = Ord('a') - 10;
    AdjustUppercase = Ord('A') - 10;
  var
    Digit: Integer;
    Neg, Hex, Valid: Boolean;
    P: PAnsiChar;
  begin
    P := Pointer(Value);
    if not Assigned(P) then
      raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
    Neg   := False;
    Hex   := False;
    Valid := False;
    while P^ = ' ' do
      Inc(P);
    if P^ in ['+', '-'] then
      begin
        Neg := (P^ = '-');
        inc(P);
      end;
    if P^ = '$' then
      begin
        Hex := True;
        inc(P);
      end
    else
      begin
        if P^ = '0' then
          begin
            Valid := True;
            inc(P);
          end;
        if (Ord(P^) or $20) = ord('x') then
          begin {Upcase(P^) = 'X'}
            Hex := True;
            inc(P);
          end;
      end;
    Result := 0;
    if Hex then
      begin
        Valid := False;
        while True do
          begin
            case P^ of
              '0'..'9': Digit := Ord(P^) - Ord('0');
              'a'..'f': Digit := Ord(P^) - AdjustLowercase;
              'A'..'F': Digit := Ord(P^) - AdjustUppercase;
              else      Break;
            end;
            if Cardinal(Result) > MaxInt div 8 then
              Break;
            Result := (Result shl 4) + Digit;
            Valid := True;
            inc(P);
          end;
      end
    else
      begin
        while True do
          begin
            if not (P^ in ['0'..'9']) then
              Break;
            if Result > (MaxInt div 10) then
              Break;
            Result := (Result * 10) + Ord(P^) - Ord('0');
            Valid := True;
            inc(P);
          end;
        if Result < 0 then {Possible Overflow}
          if (Cardinal(Result) <> $80000000) or (not neg) then
            begin {Min(LongInt) = $80000000 is a Valid Result}
              Dec(P);
              Valid := False;
            end;
      end;
    if Neg then
      Result := -Result;
    if (not Valid) or (P^ <> #0) then
      raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
  {$ELSE}
  begin
    Result := StrToInt(Value);
  {$ENDIF}
{$IFEND}
end;

{$WARNINGS OFF}
function UnicodeToInt(const Value: ZWideString): Integer;
//function StrToInt32_JOH_PAS_7_c(const s: string): Longint;
//originally wrtten by John O'Harrow
//http://fastcode.sourceforge.net/
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PWideChar;
begin
  P := Pointer(Value);
  if not Assigned(P) then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if CharInSet(P^, ['+', '-']) then
    begin
      Neg := (P^ = WideChar('-'));
      inc(P);
    end;
  if P^ = WideChar('$') then
    begin
      Hex := True;
      inc(P);
    end
  else
    begin
      if P^ = WideChar('0') then
        begin
          Valid := True;
          inc(P);
        end;
      if (Ord(P^) or $20) = ord('x') then
        begin {Upcase(P^) = 'X'}
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - AdjustLowercase;
            'A'..'F': Digit := Ord(P^) - AdjustUppercase;
            else      Break;
          end;
          if Cardinal(Result) > MaxInt div 8 then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not (CharInSet(P^, ['0'..'9'])) then
            Break;
          if Result > (MaxInt div 10) then
            Break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(LongInt) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;
{$WARNINGS ON}

const
  TwoDigitLookupA : packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

  TwoDigitLookupW : packed array[0..99] of array[1..2] of WideChar =
    (ZWideString('00'),ZWideString('01'),ZWideString('02'),ZWideString('03'),ZWideString('04'),ZWideString('05'),ZWideString('06'),ZWideString('07'),ZWideString('08'),ZWideString('09'),
     ZWideString('10'),ZWideString('11'),ZWideString('12'),ZWideString('13'),ZWideString('14'),ZWideString('15'),ZWideString('16'),ZWideString('17'),ZWideString('18'),ZWideString('19'),
     ZWideString('20'),ZWideString('21'),ZWideString('22'),ZWideString('23'),ZWideString('24'),ZWideString('25'),ZWideString('26'),ZWideString('27'),ZWideString('28'),ZWideString('29'),
     ZWideString('30'),ZWideString('31'),ZWideString('32'),ZWideString('33'),ZWideString('34'),ZWideString('35'),ZWideString('36'),ZWideString('37'),ZWideString('38'),ZWideString('39'),
     ZWideString('40'),ZWideString('41'),ZWideString('42'),ZWideString('43'),ZWideString('44'),ZWideString('45'),ZWideString('46'),ZWideString('47'),ZWideString('48'),ZWideString('49'),
     ZWideString('50'),ZWideString('51'),ZWideString('52'),ZWideString('53'),ZWideString('54'),ZWideString('55'),ZWideString('56'),ZWideString('57'),ZWideString('58'),ZWideString('59'),
     ZWideString('60'),ZWideString('61'),ZWideString('62'),ZWideString('63'),ZWideString('64'),ZWideString('65'),ZWideString('66'),ZWideString('67'),ZWideString('68'),ZWideString('69'),
     ZWideString('70'),ZWideString('71'),ZWideString('72'),ZWideString('73'),ZWideString('74'),ZWideString('75'),ZWideString('76'),ZWideString('77'),ZWideString('78'),ZWideString('79'),
     ZWideString('80'),ZWideString('81'),ZWideString('82'),ZWideString('83'),ZWideString('84'),ZWideString('85'),ZWideString('86'),ZWideString('87'),ZWideString('88'),ZWideString('89'),
     ZWideString('90'),ZWideString('91'),ZWideString('92'),ZWideString('93'),ZWideString('94'),ZWideString('95'),ZWideString('96'),ZWideString('97'),ZWideString('98'),ZWideString('99'));

const
  MinInt64Uni : WideString = '-9223372036854775808';
  MinInt64Raw : RawByteString = '-9223372036854775808';

function IntToRaw(const Value: Integer): RawbyteString;
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr_JOH_PAS_5(Value: Integer): string;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
var
  Negative       : Boolean;
  I, J, K        : Cardinal;
  Digits         : Integer;
  P              : PByte;
  NewLen, OldLen : Integer;
begin
  Negative := (Value < 0);
  I := Abs(Value);
  if I >= 10000 then
    if I >= 1000000 then
      if I >= 100000000 then
        Digits := 9 + Ord(I >= 1000000000)
      else
        Digits := 7 + Ord(I >= 10000000)
    else
      Digits := 5 + Ord(I >= 100000)
  else
    if I >= 100 then
      Digits := 3 + Ord(I >= 1000)
    else
      Digits := 1 + Ord(I >= 10);
  NewLen  := Digits + Ord(Negative);
  if Result = '' then
    OldLen := 0
  else
    if PInteger(PInteger(@Result)^ - 8)^ = 1 then {Ref Count}
      OldLen := (PInteger(PInteger(@Result)^ - 4)^)
    else
      OldLen := 0;
  if NewLen <> OldLen then
    SetLength(Result, NewLen);
  P := Pointer(Result);
  P^ := Byte('-');
  Inc(P, Ord(Negative));
  if Digits > 2 then
    repeat
      J  := I div 100;           {Dividend div 100}
      K  := J * 100;
      K  := I - K;               {Dividend mod 100}
      I  := J;                   {Next Dividend}
      Dec(Digits, 2);
      PWord(@PByteArray(P)[Digits])^ := Word(TwoDigitLookupA[K]);
    until Digits <= 2;
  if Digits = 2 then
    PWord(@PByteArray(P)[Digits-2])^ := Word(TwoDigitLookupA[I])
  else
    P^ := I or ord('0');
end;

function IntToRaw(const Value: Int64): RawByteString;
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr64_JOH_PAS_5(Value: Int64): string;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
var
  Negative           : Boolean;
  I64, J64, K64      : Int64;
  I32, J32, K32, L32 : Cardinal;
  Digits             : Byte;
  P                  : PByte;
  OldLen, NewLen     : Integer;
  C                  : AnsiChar;
begin
  if Value = $8000000000000000 then
    begin {Special RawByteString since ABS($8000000000000000) Fails}
      Result := MinInt64Raw;
      Exit;
    end;
  if (Value >= -MaxInt-1) and (Value <= MaxInt) then
    begin {Within Integer Range - Use Faster Integer Version}
      Result := IntToRaw(Integer(Value));
      Exit;
    end;
  Negative := Value < 0;
  I64 := Abs(Value);
  if I64 >= 100000000000000 then
    if I64 >= 10000000000000000 then
      if I64 >= 1000000000000000000 then
        Digits := 19
      else
        Digits := 17 + Ord(I64 >= 100000000000000000)
    else
      Digits := 15 + Ord(I64 >= 1000000000000000)
  else
    if I64 >= 1000000000000 then
      Digits := 13 + Ord(I64 >= 10000000000000)
    else
      if I64 >= 10000000000 then
        Digits := 11 + Ord(I64 >= 100000000000)
      else
        Digits := 10;
  NewLen  := Digits + Ord(Negative);
  if Result = '' then
    OldLen := 0
  else
    if PInteger(PInteger(@Result)^ - 8)^ = 1 then {Ref Count}
      OldLen := (PInteger(PInteger(@Result)^ - 4)^)
    else
      OldLen := 0;
  if NewLen <> OldLen then
    SetLength(Result, NewLen);
  P := Pointer(Result);
  P^ := Byte('-');
  Inc(P, Ord(Negative));
  if Digits > 17 then
  begin {18 or 19 Digits}
    if Digits = 19 then
    begin
      C := '0';
      while I64 >= 1000000000000000000 do
        begin
          Dec(I64, 1000000000000000000);
          Inc(C);
        end;
      P^ := Ord(C);
      Inc(P);
    end;
    C := '0';
    while I64 >= 100000000000000000 do
      begin
        Dec(I64, 100000000000000000);
        Inc(C);
      end;
    P^ := Ord(C);
    Inc(P);
    Digits := 17;
  end;
  J64 := I64 div 100000000; {Very Slow prior to Delphi 2005}
  K64 := I64 - (J64 * 100000000); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  Dec(Digits, 4);
  J32 := (Word(TwoDigitLookupA[K32]) shl 16) + Word(TwoDigitLookupA[L32]);
  PInteger(@PByteArray(P)[Digits])^ := J32;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  Dec(Digits, 4);
  I32 := (Word(TwoDigitLookupA[K32]) shl 16) + Word(TwoDigitLookupA[J32]);
  PInteger(@PByteArray(P)[Digits])^ := I32;
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
  if Digits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(Digits, 2);
      PWord(@PByteArray(P)[Digits])^ := Word(TwoDigitLookupA[K32]);
    until Digits <= 2;
  if Digits = 2 then
    PWord(@PByteArray(P)[Digits-2])^ := Word(TwoDigitLookupA[I32])
  else
    P^ := I32 or ord('0');
end;

function IntToUnicode(const Value: Integer): ZWideString;
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr_JOH_PAS_5(Value: Integer): string;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
var
  Negative       : Boolean;
  I, J, K        : Cardinal;
  Digits         : Integer;
  P              : PByte;
  NewLen, OldLen : Integer;
begin
  Negative := (Value < 0);
  I := Abs(Value);
  if I >= 10000 then
    if I >= 1000000 then
      if I >= 100000000 then
        Digits := 9 + Ord(I >= 1000000000)
      else
        Digits := 7 + Ord(I >= 10000000)
    else
      Digits := 5 + Ord(I >= 100000)
  else
    if I >= 100 then
      Digits := 3 + Ord(I >= 1000)
    else
      Digits := 1 + Ord(I >= 10);
  NewLen  := Digits + Ord(Negative);
  if Result = '' then
    OldLen := 0
  else
    if PInteger(PInteger(@Result)^ - 8)^ = 1 then {Ref Count}
      OldLen := (PInteger(PInteger(@Result)^ - 4)^)
    else
      OldLen := 0;
  if NewLen <> OldLen then
    SetLength(Result, NewLen);
  P := Pointer(Result);
  PWord(P)^ := Word('-');
  Inc(P, Ord(Negative)*2);
  if Digits > 2 then
    repeat
      J  := I div 100;           {Dividend div 100}
      K  := J * 100;
      K  := I - K;               {Dividend mod 100}
      I  := J;                   {Next Dividend}
      Dec(Digits, 2);
      PLongWord(@PWordArray(P)[Digits])^ := LongWord(TwoDigitLookupW[K]);
    until Digits <= 2;
  if Digits = 2 then
    PLongWord(@PWordArray(P)[Digits-2])^ := LongWord(TwoDigitLookupW[I])
  else
    PWord(P)^ := I or Word('0');
end;

function IntToUnicode(const Value: Int64): ZWideString;
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr64_JOH_PAS_5(Value: Int64): string;
{$IFNDEF WITH_FASTCODE_INTTOSTR}
var
  Negative           : Boolean;
  I64, J64, K64      : Int64;
  I32, J32, K32, L32 : Cardinal;
  Digits             : Byte;
  P                  : PWideChar;
  NewLen             : Integer;
{$ENDIF}
begin
  {$IFDEF WITH_FASTCODE_INTTOSTR}
  Result := IntToStr(Value);
  {$ELSE}
  if Value = $8000000000000000 then
    begin {Special WideString since ABS($8000000000000000) Fails}
      Result := MinInt64Uni;
      Exit;
    end;
  if (Value >= -MaxInt-1) and (Value <= MaxInt) then
    begin {Within Integer Range - Use Faster Integer Version}
      Result := IntToUnicode(Integer(Value));
      Exit;
    end;
  Negative := Value < 0;
  I64 := Value;
  if I64 >= 100000000000000 then
    if I64 >= 10000000000000000 then
      if I64 >= 1000000000000000000 then
        Digits := 19
      else
        Digits := 17 + Ord(I64 >= 100000000000000000)
    else
      Digits := 15 + Ord(I64 >= 1000000000000000)
  else
    if I64 >= 1000000000000 then
      Digits := 13 + Ord(I64 >= 10000000000000)
    else
      if I64 >= 10000000000 then
        Digits := 11 + Ord(I64 >= 100000000000)
      else
        Digits := 10;
  NewLen  := Digits + Ord(Negative);
  SetLength(Result, NewLen);
  P := PWideChar(Result);
  P^ := '-';
  Inc(P, Ord(Negative));
  if Digits > 17 then
  begin {18 or 19 Digits}
    if Digits = 19 then
    begin
      P^ := WideChar('0');
      while I64 >= 1000000000000000000 do
      begin
        Dec(I64, 1000000000000000000);
        Inc(P^);
      end;
      Inc(P);
    end;
    P^ := WideChar('0');
    while I64 >= 100000000000000000 do
    begin
      Dec(I64, 100000000000000000);
      Inc(P^);
    end;
    Inc(P);
    Digits := 17;
  end;
  J64 := I64 div 100000000;
  K64 := I64 - (J64 * 100000000); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PLongWord(P + Digits - 2)^ := LongWord(TwoDigitLookupW[K32]);
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  PLongWord(P + Digits - 4)^ := LongWord(TwoDigitLookupw[L32]);
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PLongWord(P + Digits - 6)^ := LongWord(TwoDigitLookupW[K32]);
  PLongWord(P + Digits - 8)^ := LongWord(TwoDigitLookupW[J32]);
  Dec(Digits, 8);
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
  if Digits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(Digits, 2);
      PLongWord(P + Digits)^ := LongWord(TwoDigitLookupW[K32]);
    until Digits <= 2;
  if Digits = 2 then
    PLongWord(P + Digits-2)^ := LongWord(TwoDigitLookupW[I32])
  else
    P^ := WideChar(I32 or ord('0'));
  {$ENDIF}
end;

function IntToString(const Value: Integer): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

function IntToString(const Value: Int64): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

{$IF defined(WIN32) and not defined(FPC)}
function ValLong_JOH_IA32_8_a(const s: RawByteString; var code: Integer): Longint;
//fast asm by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
asm
  test  eax, eax
  jz    @@Null
  push  ebx
  push  esi
  push  eax                 {Save String Pointer}
  mov   esi, eax            {String Pointer}
  xor   ebx, ebx            {Clear Valid Flag and Sign Flag}
  xor   eax, eax            {Clear Result}
  jmp   @@TrimEntry
@@Null:
  mov   [edx], eax
  inc   [edx]               {Code = 1}
  ret
@@Trim:                     {Strip Leading Spaces}
  inc   esi
@@TrimEntry:
  movzx ecx, [esi]
  cmp   cl, ' '
  je    @@Trim
  cmp   cl, '0'
  jle   @@CheckFirstChar
@@CheckAlpha:
  test  cl, not 'x'
  jz    @@CheckX            {May be 'x' or 'X'}
@@NumLoop:
  sub   cl, '0'
  cmp   cl, 9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax, MaxInt/10
  ja    @@SetSign
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  inc   esi
  mov   bl, 1               {Valid := True}
  movzx ecx, [esi]
  jmp   @@NumLoop
@@NumDone:
  cmp   eax, $80000000
  jb    @@SetSign           {No Overflow}
  jne   @@Overflow
  test  ebx, ebx            {Sign Flag}
  js    @@SetSign           {Result is Valid (-MaxInt-1)}
@@Overflow:
  dec   esi
  mov   bl, 0               {Valid := False}
  jmp   @@SetSign
@@CheckFirstChar:
  cmp   cl, '+'
  je    @@PlusMinus
  cmp   cl, '-'
  jne   @@SignSet
@@PlusMinus:                {Starts with '+' or '-'}
  mov   bl, '+'+1
  sub   ebx, ecx            {Set Sign Flag: '+' -> +1, '-' -> -1}
  inc   esi
  mov   bl, 0               {Valid := False}
  movzx ecx, [esi]          {Character after '+' or '-'}
@@SignSet:
  cmp   cl, '$'
  je    @@Hex               {Hexadecimal}
  cmp   cl, '0'
  jne   @@CheckAlpha        {May start with 'x' or 'X'}
  inc   esi
  mov   bl, 1               {Assume Valid = True}
  movzx ecx, [esi]          {Character after '0'}
  jmp   @@CheckAlpha        {May start with '0x' or '0X'}
@@CheckX:
  mov   bh, cl
  or    bh, $20             {'X' -> 'x'}
  cmp   bh, 'x'
  jne   @@NumLoop
@@Hex:
  mov   bl, 0               {Valid := False}
@@HexLoop:
  inc   esi
  movzx ecx, [esi]
  cmp   cl, 'a'
  jb    @@CheckNum
  sub   cl, 'a'-'A'         {'a' > 'A'}
@@CheckNum:
  sub   cl, '0'
  cmp   cl, 9
  jna   @@CheckHexRange     {'0'..'9'}
  sub   cl, 'A'-'0'
  cmp   cl, 5               {Valid Hex Character?}
  ja    @@NotHex            {No, Invalid}
  add   cl, 10              {Yes, Adjust Digit}
@@CheckHexRange:
  cmp   eax, MaxInt/8       {High(ULONG) div 16}
  ja    @@SetSign           {Overflow}
  shl   eax, 4              {Result = Result * 16}
  mov   bl, 1               {Valid := True}
  add   eax, ecx
  jmp   @@HexLoop
@@NotHex:
  add   cl, 'A'-'0'         {Restore Char-'0'}
@@SetSign:
  mov   ch, bl              {Save Valid Flag}
  sar   ebx, 31             {Set Each Bit to Top Bit}
  dec   ch                  {0 if Valid, -1 if Invalid}
  xor   eax, ebx            {Negate Result if Necessary}
  sub   eax, ebx
  or    cl, ch              {If Invalid, Force CL = -1}
  cmp   cl, -'0'            {Last Character = #0?}
  jne   @@Error             {Not Valid or Not End of String}
  xor   esi, esi            {Code := 0}
  pop   ecx                 {Dump String Pointer}
@@Finished:
  mov   [edx], esi          {Set Error Code}
  pop   esi
  pop   ebx
  ret
@@Error:
  inc   esi
  pop   ecx                 {String Pointer}
  sub   esi, ecx
  jmp   @@Finished          {Exit Setting Error Code}
end;
{$ELSE}
{$WARNINGS OFF} {Prevent False Compiler Warning on Digit not being Initialized}
function ValLong_JOH_PAS_4_b(const s; var code: Integer): Longint;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PAnsiChar;
begin
  Code := 0;
  P    := @S;
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if P^ in ['+', '-'] then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else Break;
          end;
          if (Result < 0) or (Result > $0FFFFFFF) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not (P^ in ['0'..'9']) then
            break;
          if Result > (MaxInt div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(LongInt) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;
{$WARNINGS ON}
{$IFEND}

function RawToIntDef(const S: RawByteString; const Default: Integer) : Integer;
var
  E: Integer;
begin
//  Val(S, Result, E); {when system._ValLong updated}
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValLong_JOH_IA32_8_a(S, E);
  {$ELSE}
  Result := ValLong_JOH_PAS_4_b(Pointer(S)^, E);
  {$IFEND}
  if E <> 0 then Result := Default;
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValLong_JOH_PAS_4_b_unicode(const s; var code: Integer): Longint;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PWideChar;
  {$IFNDEF UNICODE}
  function UpCase(Ch: WideChar): WideChar;
  begin
    Result := Ch;
    case Ch of
      'a'..'z':
        Result := WideChar(Word(Ch) and $FFDF);
    end;
  end;
  {$ENDIF}
begin
  Code := 0;
  P    := @S;
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if CharInSet(P^, ['+', '-']) then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else Break;
          end;
          if (Result < 0) or (Result > $0FFFFFFF) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not CharInSet(P^, ['0'..'9']) then
            break;
          if Result > (MaxInt div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(LongInt) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;
{$WARNINGS ON}

function UnicodeToIntDef(const S: ZWideString; const Default: Integer) : Integer;
var
  E: Integer;
begin
  Result := ValLong_JOH_PAS_4_b_unicode(Pointer(S)^, E);
  if E <> 0 then Result := Default;
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValInt64_JOH_PAS_4_b(const s; var code: Integer): Int64;
//function ValLong_JOH_PAS_4_b(const s; var code: Integer): LongInt;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for Int64
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PAnsiChar;
begin
  Code := 0;
  P    := @S;
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if P^ in ['+', '-'] then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else Break;
          end;
          if (Result < 0) or (Result > (High(Int64) shr 3)) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not (P^ in ['0'..'9']) then
            break;
          if Result > (High(Int64) div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if (Result <> 0) and (Neg <> (Result < 0)) then
          begin {Possible Overflow}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;
{$WARNINGS ON}

{$IF defined(WIN32) and not defined(FPC)}
function ValInt64_JOH_IA32_8_a(const s: RawByteString; var code: Integer): Int64;
asm
  test  eax, eax
  jz    @@Null
  push  ebx
  push  esi
  push  edi
  push  edx                 {Save Code Address}
  push  eax                 {Save String Pointer}
  mov   esi, eax            {String Pointer}
  xor   ebx, ebx            {Clear Valid Flag and Sign Flag}
  xor   eax, eax            {Clear Result}
  xor   edx, edx
  jmp   @@TrimEntry
@@Null:
  mov   [edx], eax
  inc   [edx]               {Code = 1}
  xor   edx, edx            {Result = 0}
  ret
@@Trim:                     {Strip Leading Spaces}
  inc   esi
@@TrimEntry:
  movzx ecx, [esi]
  cmp   cl, ' '
  je    @@Trim
  cmp   cl, '0'
  jle   @@CheckFirstChar
@@CheckAlpha:
  test  cl, $87
  jz    @@CheckX            {May be 'x' or 'X'}
@@NumLoop:
  sub   ecx, '0'
  cmp   ecx, 9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   edx, $0ccccccc
  jae   @@CheckNumRange     {May be Out of Range}
@@InRange:
  test  edx, edx
  jnz   @@LargeNum
  cmp   eax, MaxInt/10-9    {MaxInt div 10)-9}
  ja    @@LargeNum
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  jmp   @@DoneNumMul
@@LargeNum:
  mov   bh, cl              {Save Digit}
  add   eax, eax
  adc   edx, edx
  mov   ecx, eax
  mov   edi, edx            {edi:ecx = Result * 2}
  shld  edx, eax, 2
  add   eax, eax
  add   eax, eax            {edx:eax = Result * 8}
  add   eax, ecx
  adc   edx, edi            {Result = Result * 10}
  movzx ecx, bh             {Restore Digit}
  add   eax, ecx            {Add Digit to Result}
  adc   edx, 0
@@DoneNumMul:
  inc   esi
  mov   bl, 1               {Valid := True}
  movzx ecx, [esi]
  jmp   @@NumLoop
@@CheckNumRange:
  ja    @@SetSign           {Out of Range}
  cmp   eax, $cccccccc
  jna   @@InRange           {Within Range}
  jmp   @@SetSign
@@NumDone:
  cmp   edx, $80000000      {Check for Overflow}
  jb    @@SetSign
  jne   @@Overflow
  test  eax, eax
  jnz   @@Overflow
  test  ebx, ebx            {Sign Flag}
  js    @@Setsign           {Result is Valid (-MaxInt64-1)}
@@Overflow:
  dec   esi
  mov   bl, 0               {Valid := False}
  jmp   @@SetSign
@@CheckFirstChar:
  cmp   cl, '-'
  je    @@PlusMinus
  cmp   cl, '+'
  jne   @@SignSet
@@PlusMinus:                {Starts with '+' or '-'}
  mov   bl, '+'+1
  sub   ebx, ecx            {Set Sign Flag: '+' -> +1, '-' -> -1}
  inc   esi
  mov   bl, 0               {Valid := False}
  movzx ecx, [esi]          {Character after '+' or '-'}
@@SignSet:
  cmp   cl, '$'
  je    @@Hex               {Hexadecimal}
  cmp   cl, '0'
  jne   @@CheckAlpha        {May start with 'x' or 'X'}
  inc   esi
  mov   bl, 1               {Assume Valid = True}
  movzx ecx, [esi]          {Character after '0'}
  jmp   @@CheckAlpha        {May start with '0x' or '0X'}
@@CheckX:
  mov   bh, cl
  or    bh, $20             {'X' -> 'x'}
  cmp   bh, 'x'
  jne   @@NumLoop
@@Hex:
  mov   bl, 0               {Valid := False}
@@HexLoop:
  inc   esi
  movzx ecx, [esi]
  cmp   cl, 'a'
  jb    @@CheckNum
  sub   cl, 'a'-'A'         {'a' > 'A'}
@@CheckNum:
  sub   cl, '0'
  cmp   cl, 9
  jna   @@CheckHexRange     {'0'..'9'}
  sub   cl, 'A'-'0'
  cmp   cl, 5               {Valid Hex Character?}
  ja    @@NotHex            {No, Invalid}
  add   cl, 10              {Yes, Adjust Digit}
@@CheckHexRange:
  cmp   edx, $10000000
  jae   @@SetSign           {Overflow}
  shld  edx, eax, 4         {Result := Result * 16}
  shl   eax, 4
  add   eax, ecx            {Add Digit}
  adc   edx, 0
  mov   bl, 1               {Valid := True}
  jmp   @@HexLoop
@@NotHex:
  add   cl, 'A'-'0'         {Restore Char-'0'}
@@SetSign:
  mov   ch, bl              {Save Valid Flag}
  sar   ebx, 31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax, ebx            {Negate Result if Necessary}
  xor   edx, ebx
  sub   eax, ebx
  sbb   edx, ebx
  dec   ch                  {0 if Valid, -1 if Invalid}
  or    cl, ch              {If Invalid, Force CL = -1}
  cmp   cl, -'0'
  jne   @@Error             {Not Valid or Not End of String}
  xor   esi, esi            {Code := 0}
  pop   ebx                 {Dump String Pointer}
@@Finished:
  pop   ecx
  mov   [ecx], esi          {Set Error Code}
  pop   edi
  pop   esi
  pop   ebx
  ret
@@Error:
  inc   esi
  pop   ecx                 {String Pointer}
  sub   esi, ecx
  jmp   @@Finished
end;
{$IFEND}

function RawToInt64Def(const S: RawByteString; const Default: Integer) : Int64;
var
  E: Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValInt64_JOH_IA32_8_a(s, E);
  {$ELSE}
  Result := ValInt64_JOH_PAS_4_b(Pointer(S)^, E);
  {$IFEND}
  if E <> 0 then Result := Default;
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValInt64_JOH_PAS_4_b_unicode(const s; var code: Integer): Int64;
//function ValLong_JOH_PAS_4_b(const s; var code: Integer): LongInt;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for Int64
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PWideChar;
  {$IFNDEF UNICODE}
  function UpCase(Ch: WideChar): WideChar;
  begin
    Result := Ch;
    case Ch of
      'a'..'z':
        Result := WideChar(Word(Ch) and $FFDF);
    end;
  end;
  {$ENDIF}
begin
  Code := 0;
  P    := @S;
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if CharInSet(P^, ['+', '-']) then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else Break;
          end;
          if (Result < 0) or (Result > (High(Int64) shr 3)) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not CharInSet(P^, ['0'..'9']) then
            break;
          if Result > (High(Int64) div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if (Result <> 0) and (Neg <> (Result < 0)) then
          begin {Possible Overflow}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;
{$WARNINGS ON}

function UnicodeToInt64Def(const S: ZWideString; const Default: Integer) : Int64;
var
  E: Integer;
begin
  Result := ValInt64_JOH_PAS_4_b_unicode(Pointer(S)^, E);
  if E <> 0 then Result := Default;
end;

function RawToFloatDef(const s: RawByteString; const DecimalSep: AnsiChar;
  const Default: Extended): Extended;
var
  E: Integer;
begin
  Result :=  ValRawExt(s, DecimalSep, E);
  if E <> 0 then Result := Default;
end;

function RawToFloat(const s: RawByteString; const DecimalSep: AnsiChar): Extended;
var
  E: Integer;
begin
  Result :=  ValRawExt(s, DecimalSep, E);
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

function ValRawExt(const s: RawByteString; const DecimalSep: AnsiChar; var code: Integer): Extended;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; var code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator
var
  Digits, ExpValue: Integer;
  Ch: AnsiChar;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if s = '' then
    begin
      inc(Code);
      Exit;
    end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code+1] = ' ' do
    Inc(Code);
  Ch := S[code+1];
  if Ch in ['+', '-'] then
  begin
    inc(Code);
    Neg := (Ch = '-');
  end;
  while true do
  begin
    Ch := S[code+1];
    inc(Code);
    if not (Ch in ['0'..'9']) then
      break;
    Result := (Result * 10) + Ord(Ch) - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = DecimalSep then
  begin
    while true do
      begin
        Ch := S[code+1];
        inc(Code);
        if not (Ch in ['0'..'9']) then
        begin
          if not valid then {Starts with '.'}
          begin
            if Ch = #0 then
              dec(code); {s = '.'}
          end;
          break;
        end;
        Result := (Result * 10) + Ord(Ch) - Ord('0');
        Dec(Digits);
        Valid := true;
      end;
    end;
  ExpValue := 0;
  if (Ord(Ch) or $20) = ord('e') then
    begin {Ch in ['E','e']}
      Valid := false;
      Ch := S[code+1];
      if Ch in ['+', '-'] then
        begin
          inc(Code);
          NegExp := (Ch = '-');
          Ch := S[code+1];
        end;
      while true do
        begin
          Ch := S[code+1];
          inc(Code);
          if not (Ch in ['0'..'9']) then
            break;
          ExpValue := (ExpValue * 10) + Ord(Ch) - Ord('0');
          Valid := true;
        end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    Result := Result * IntPower(10, Digits);
  if Neg then
    Result := -Result;
  if Valid and (ch = #0) then
    code := 0;
end;

function ValRawInt(const s: RawByteString; var code: Integer): Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValInt64_JOH_IA32_8_a(s, Code);
  {$ELSE}
  Result := ValInt64_JOH_PAS_4_b(Pointer(S)^, Code);
  {$IFEND}
end;


end.
