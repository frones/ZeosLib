{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZCompatibility;

interface

{$I ZCore.inc}

uses
  Variants,
{$IFDEF FPC}
  {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
  LConvEncoding,
  {.$ELSE}
  //cwstring,
  {$IFEND}
  {$IFDEF UNIX}
    dynlibs,
  {$endif}
{$ENDIF}
  {$IFDEF WITH_WIDESTRUTILS}
  WideStrUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils;

type
{$IFDEF FPC}
  ULong                 = {$IFDEF WIN64}LongWord{$ELSE}PTRUINT{$ENDIF};
                            // EgonHugeist: Use always a 4Byte Integer as long the PlainDriver dll's are 32Bit for Windows64
                            //on the other hand MySQL64 and FB64 have problems on Win64!
  ULongLong             = QWord;
  NativeInt             = PtrInt;
  NativeUInt            = PtrUInt;
{$ELSE}
  {$IFNDEF DELPHI16_UP}
  NativeInt             = Integer;
  NativeUInt            = LongWord;
  PWord                 = ^Word; // M.A.
  {$ENDIF}
  ULong                 = LongWord;
  ULongLong             = Int64; //delphi don´t have Unsigned Int64 type
{$ENDIF}
  PULong                = ^ULong;
  PULongLong            = ^ULongLong;

  UInt                  = LongWord;
  PUInt                 = ^UInt;

  TObjectDynArray       = array of TObject;

{$IFDEF FPC}
type
  TDBScreenCursor = (dcrDefault, dcrHourGlass, dcrSQLWait, dcrOther);

  IDBScreen = interface
    ['{29A1C508-6ADC-44CD-88DE-4F51B25D5995}']
    function GetCursor: TDBScreenCursor;
    procedure SetCursor(Cursor: TDBScreenCursor);

    property Cursor: TDBScreenCursor read GetCursor write SetCursor;
  end;

var
  LoginDialogProc: function (const ADatabaseName: string; var AUserName,
    APassword: string): Boolean;
  DBScreen: IDBScreen;
{$ENDIF}

{$IFNDEF FPC} //delphi and windows
const
  LineEnding = #13#10;
  Brackets = ['(',')','[',']','{','}'];
  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;

function Hash(S : AnsiString) : LongWord;
function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;

{$ENDIF}

{$IFDEF WINDOWS}
const SharedSuffix='.dll';
{$ELSE}
  {$IFDEF DARWIN}
  const SharedSuffix='.dylib';
  {$ELSE}
    {$IFDEF UNIX}
      const SharedSuffix='.so';
    {$ELSE}
      const SharedSuffix='.dll'; //Delphi
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
const
  RTLD_GLOBAL = $101;
  INVALID_HANDLE_VALUE = 0;

type
  HMODULE = PtrInt;

function LoadLibrary(ModuleName: PChar): HMODULE;
function FreeLibrary(Module: HMODULE): LongBool;
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
  {$ENDIF}
{$ENDIF}

{EgonHugeist:}
type
  ZAnsiString = {.$IFDEF WITH_RAWBYTESTRING}{$IFDEF DELPHI12_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
  ZWideString = {$IFDEF DELPHI12_UP}String{$ELSE}WideString{$ENDIF};

  {** Defines the Target Ansi codepages for the Controls }
  TZControlsCodePage = ({$IFDEF DELPHI12_UP}cCP_UTF16, cCP_UTF8, cGET_ACP{$ELSE}{$IFDEF FPC}cCP_UTF8, cCP_UTF16, cGET_ACP{$ELSE}cGET_ACP, cCP_UTF8, cCP_UTF16{$ENDIF}{$ENDIF});

  TZCharEncoding = (
    ceDefault,  //Internal switch for the two Functions below do not use it as a CodePage-declaration!
    ceAnsi,     //Base Ansi-String: prefered CodePage
    ceUTF8,     //UTF8_Unicode: 1-4Byte/Char
    ceUTF16,    //reserved
    ceUTF32);   //reserved

    {Here it's possible to add some more, to handle the Ansi->Unicode-Translations}

  PZCodePage = ^TZCodePage;
  TZCodePage = record
    Name: String;             //Name of Client-CharacterSet
    ID:  Integer;             //may be an ordinal value of predefined Types or the database used id}
    CharWidth: Integer;       //count of Bytes per char
    Encoding: TZCharEncoding; //The Type of String-Translation handling
    CP:  Word;                //The CodePage the AnsiString must have to
    ZAlias: String;           //A possible (saver?) CharacterSet which is more Zeos compatible... If it's empty it will be ignored!!!
    IsSupported: Boolean;     //Is the choosen CP supported?
  end;

  PZConSettings = ^TZConSettings;
  TZConSettings = record
    AutoEncode: Boolean;        //Check Encoding and or convert string with FromCP ToCP
    UTF8AsWideString: Boolean;  //If DB encoding is UTF8 then assume Wide-Fields? -> what do the constrols expect?
    CPType: TZControlsCodePage; //the CP-Settings type the controls do expect
    OS_CP: Word;                //Target CP of string conversations (CP_ACP/CP_UPF8)
    ClientCodePage: PZCodePage; //The codepage informations of the current characterset
    {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
    PlainConvertFunc: TConvertEncodingFunction;
    DbcConvertFunc: TConvertEncodingFunction;
    {$IFEND}
  end;

  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    function ZDbcString(const Ansi: ZAnsiString; ConSettings: PZConSettings): String; overload;
    function ZDbcString(const Ansi: ZAnsiString; const Encoding: TZCharEncoding = ceDefault): String; overload;
    function ZPlainString(const AStr: String; ConSettings: PZConSettings): ZAnsiString; overload;
    function ZPlainString(const AStr: String; const Encoding: TZCharEncoding = ceDefault): ZAnsiString; overload;
    function ZStringFromUnicode(const ws: ZWideString; const Encoding: TZCharEncoding = ceDefault): String;
    function ZUnicodeFromString(const AStr: String; const Encoding: TZCharEncoding = ceDefault): ZWideString;
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  public
    destructor Destroy; override;
  end;

{$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
function NoConvert(const s: string): string;
{$IFEND}

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; CharWidth: 1; Encoding: ceAnsi;
      CP: $ffff; ZAlias: ''; IsSupported: True);

  ConSettingsDummy: TZConSettings =
    (AutoEncode: False; UTF8AsWideString: False;
     CPType: {$IFDEF DELPHI}{$IFDEF DELPHI12_UP}cCP_UTF16{$ELSE}cGET_ACP{$ENDIF}{$ELSE}cCP_UTF8{$ENDIF};
     OS_CP: $ffff;
     ClientCodePage: nil;
     {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
     PlainConvertFunc: @NoConvert;
     DbcConvertFunc: @NoConvert
     {$IFEND});

  {$IF defined(DELPHI) or defined (MSWINDOWS)}
  ZFullMultiByteCodePages: array[0..21] of Word = (50220, 50221, 50222, 50225,
    50227, 50229, 52936, 54936, 54936, 57002, 57003, 57004, 57005, 57006,
    57007, 57008, 57009, 57010, 57011, 65000, 65001, 42);
  {$IFEND}

  {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
  ZLConvCodepages: array[0..16] of Word = (
    28591,  //ISO_8859_1
    28592,  //ISO_8859_2
    1250,   //WIN1250
    1251,   //WIN1251
    1252,   //WIN1252
    1253,   //WIN1253
    1254,   //WIN1254
    1255,   //WIN1255
    1256,   //WIN1256
    1257,   //WIN1257
    1258,   //WIN1258
    437,    //CP437
    850,    //CP850
    852,    //CP852
    866,    //CP866
    874,    //CP874
    20866   //KOI8 (Russian)
    );
  {$IFEND}

function AnsiToStringEx(const s: ZAnsiString; const FromCP{$IFNDEF DELPHI12_UP}, ToCP{$ENDIF}: Word): String;
function StringToAnsiEx(const s: String; const {$IFNDEF DELPHI12_UP}FromCP, {$ENDIF} ToCP: Word): ZAnsiString;

{$IF defined(DELPHI) or defined (MSWINDOWS)}
function IsFullMultiByteCodePage(CP: Word): Boolean;
{$IFEND}

{$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
procedure SetConvertFunctions(const OS_CP, DB_CP: Word;
  out PlainConvert, DbcConvert: TConvertEncodingFunction);
function IsLConvEncodingCodePage(const CP: Word): Boolean;
{$IFEND}

{$IF not Declared(DetectUTF8Encoding)}
{$DEFINE ZDetectUTF8Encoding}
Type
  TEncodeType = (etUSASCII, etUTF8, etANSI);

function DetectUTF8Encoding(Ansi: AnsiString): TEncodeType;
{$IFEND}

{$IFNDEF WITH_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: AnsiString): WideString;
{$IFEND}

implementation

{$IFDEF ZDetectUTF8Encoding}
function DetectUTF8Encoding(Ansi: AnsiString): TEncodeType; //EgonHugeist: Detect a valid UTF8Sequence
var
  I, Len: Integer;
  Source: PAnsiChar;

  function P(Pos: Integer = 0): Byte;
  begin
    Result := Byte(Source[Pos]);
  end;

  procedure IncPos(X: Integer = 1);
  begin
    inc(Source, X);
    inc(i, X);
  end;
begin
  Result := etUSASCII;
  if Ansi = '' then Exit;

  Len := Length(Ansi);
  Source := PAnsiChar(Ansi);

  // skip US-ASCII Chars they are allways valid.
  I := 0;
  while ( I <= Len ) do
  begin
    if P >= $80 then break;
    IncPos;
  end;

  if i > Len then exit; //US ACII

  //No US-Ascii at all.
  while I < Len do
  begin
    case p of
      $00..$7F: //Ascii
        IncPos;

      $C2..$DF: // non-overlong 2-byte
        if (I+1 < Len)
            and (P(1) in [$80..$BF]) then
          IncPos(2)
        else
          break;

      $E0: // excluding overlongs
        if (I+2 < Len)
            and (P(1) in [$A0..$BF])
            and (P(2) in [$80..$BF]) then
          IncPos(3)
        else
          break;

      $E1..$EF: // straight 3-byte & excluding surrogates
        if (i+2 < Len)
            and (P(1) in [$80..$BF])
            and (P(2) in [$80..$BF]) then
          IncPos(3)
        else
          break;

      $F0: // planes 1-3
        if (i+3 < Len)
            and (P(1) in [$90..$BF])
            and (P(2) in [$80..$BF])
            and (P(3) in [$80..$BF]) then
          IncPos(4)
        else
          break;

      $F1..$F3: // planes 4-15
        if (i+3 < Len)
            and (P(1) in [$80..$BF])
            and (P(2) in [$80..$BF])
            and (P(3) in [$80..$BF]) then
          IncPos(4)
        else
          break;

      $F4: // plane 16
        if (i+3 < Len)
            and (P(1) in [$80..$8F])
            and (P(2) in [$80..$BF])
            and (P(3) in [$80..$BF]) then
          IncPos(4)
        else
          break;
    else
      break;
    end;
  end;

  if i = Len then
    Result := etUTF8  //UTF8
  else
    Result := etANSI; //Ansi
end;
{$ENDIF}

{$IF defined(DELPHI) or defined (MSWINDOWS)}
function IsFullMultiByteCodePage(CP: Word): Boolean;
var
  I: Integer;
begin
  for i := 0 to High(ZFullMultiByteCodePages) do
  begin
    Result := CP = ZFullMultiByteCodePages[i];
    if Result then Break;
  end;
end;
{$IFEND}

{$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
function NoConvert(const s: string): string;
begin
  Result := S;
end;

function IsLConvEncodingCodePage(const CP: Word): Boolean;
var
  I: Integer;
begin
  for i := 0 to High(ZLConvCodepages) do
  begin
    Result := CP = ZLConvCodepages[i];
    if Result then Break;
  end;
end;

procedure SetConvertFunctions(const OS_CP, DB_CP: Word;
  out PlainConvert, DbcConvert: TConvertEncodingFunction);
begin
  if OS_CP = DB_CP then
  begin
    PlainConvert := @NoConvert;
    DbcConvert := @NoConvert;
  end
  else
  begin
    case DB_CP of
      28591: //ISO_8859_1
        begin
          DbcConvert := @ISO_8859_1ToUTF8;
          PlainConvert := @UTF8ToISO_8859_1;
        end;
      28592:  //ISO_8859_2
        begin
          DbcConvert := @ISO_8859_2ToUTF8;
          PlainConvert := @UTF8ToISO_8859_2;
        end;
      1250: //WIN1250
        begin
          DbcConvert := @CP1250ToUTF8;
          PlainConvert := @UTF8ToCP1250;
        end;
      1251: //WIN1251
        begin
          DbcConvert := @CP1251ToUTF8;
          PlainConvert := @UTF8ToCP1251;
        end;
      1252: //WIN1252
        begin
          DbcConvert := @CP1252ToUTF8;
          PlainConvert := @UTF8ToCP1252;
        end;
      1253: //WIN1253
        begin
          DbcConvert := @CP1253ToUTF8;
          PlainConvert := @UTF8ToCP1253;
        end;
      1254: //WIN1254
        begin
          DbcConvert := @CP1254ToUTF8;
          PlainConvert := @UTF8ToCP1254;
        end;
      1255: //WIN1255
        begin
          DbcConvert := @CP1255ToUTF8;
          PlainConvert := @UTF8ToCP1255;
        end;
      1256: //WIN1256
        begin
          DbcConvert := @CP1256ToUTF8;
          PlainConvert := @UTF8ToCP1256;
        end;
      1257: //WIN1257
        begin
          DbcConvert := @CP1257ToUTF8;
          PlainConvert := @UTF8ToCP1257;
        end;
      1258: //WIN1258
        begin
          DbcConvert := @CP1258ToUTF8;
          PlainConvert := @UTF8ToCP1258;
        end;
      437: //CP437
        begin
          DbcConvert := @CP437ToUTF8;
          PlainConvert := @UTF8ToCP437;
        end;
      850: //CP850
        begin
          DbcConvert := @CP850ToUTF8;
          PlainConvert := @UTF8ToCP850;
        end;
      852: //CP852
        begin
          DbcConvert := @CP852ToUTF8;
          PlainConvert := @UTF8ToCP852;
        end;
      866: //CP866
        begin
          DbcConvert := @CP866ToUTF8;
          PlainConvert := @UTF8ToCP866;
        end;
      874: //CP874
        begin
          DbcConvert := @CP874ToUTF8;
          PlainConvert := @UTF8ToCP874;
        end;
      20866: //KOI8 (Russian)
        begin
          DbcConvert := @KOI8ToUTF8;
          PlainConvert := @UTF8ToKOI8;
        end
      else
        begin
          DbcConvert := @NoConvert;
          PlainConvert := @NoConvert;
        end;
    end;
  end;
end;
{$IFEND}

function AnsiToWide(const S: ZAnsiString; const CP: Word): ZWideString;
var
  {$IFDEF DELPHI15_UP}wlen, ulen{$ELSE}l{$ENDIF}: Integer;
begin
  Result := '';
  if CP = 65001 then
    Result := UTF8ToString(s)
  else
    {$IF defined(DELPHI) or defined(MSWINDOWS)}
    if ( IsFullMultiByteCodePage(CP) ) then //dwFlags must be set to 0!!!
    begin
      {$IFDEF DELPHI15_UP}
      ulen := Length(s);
      wlen := UnicodeFromLocaleChars(cp, 0, PAnsiChar(S), ulen, NIL, 0); // wlen is the number of UCS2 without NULL terminater.
      if wlen = 0 then exit;
      SetLength(result, wlen);
      UnicodeFromLocaleChars(cp, 0, PAnsiChar(S), ulen, PWideChar(Result), wlen);
      {$ELSE}
      l := MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]), - 1, nil, 0); //Checkout the Result-Lengh
      if l = 0 then Exit;
      SetLength(Result, l - 1); //Set Result-Length
      MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]),
        - 1, PWideChar(@Result[1]), l - 1); //Convert Ansi to Wide with supported Chars
      {$ENDIF}
    end
    else
    begin
      {$IFDEF DELPHI15_UP}
      ulen := Length(s);
      wlen := UnicodeFromLocaleChars(cp, MB_PRECOMPOSED, PAnsiChar(S), ulen, NIL, 0); // wlen is the number of UCS2 without NULL terminater.
      if wlen = 0 then exit;
      SetLength(result, wlen);
      UnicodeFromLocaleChars(cp, MB_PRECOMPOSED, PAnsiChar(S), ulen, PWideChar(Result), wlen);
      {$ELSE}
      l := MultiByteToWideChar(CP, MB_PRECOMPOSED, PAnsiChar(@s[1]), - 1, nil, 0); //Checkout the Result-Lengh
      if l = 0 then Exit;
      SetLength(Result, l - 1);
      MultiByteToWideChar(CP, MB_PRECOMPOSED, PAnsiChar(@s[1]),
        - 1, PWideChar(@Result[1]), l - 1); //Convert Ansi to Wide with supported Chars
      {$ENDIF}
    end;
    {$ELSE}
    Result := ZWideString(s);
    {now we can look for a better way to implement a solution for FPC like FPC2.7+ can do
    or we implement the libiconv.dll eg. OSX/*nix systems}
    {$IFEND}
end;

function WideToAnsi(const ws: ZWideString; CP: Word): ZAnsiString;
var
  {$IFDEF DELPHI15_UP}wlen, ulen{$ELSE}l{$ENDIF}: Integer;
begin
  Result := '';
  if CP = 65001 then
    Result := UTF8Encode(ws)
  else
    {$IF defined(DELPHI) or defined(MSWINDOWS)}
    if ( IsFullMultiByteCodePage(CP)) then //dwFlags MUST be Set to 0!!!!
    begin
      {$IFDEF DELPHI15_UP}
      wlen := Length(ws);
      ulen := LocaleCharsFromUnicode(CP, 0, PWideChar(WS), wlen, NIL, 0, NIL, NIL);
      setlength(Result, ulen);
      LocaleCharsFromUnicode(CP, 0, PWideChar(WS), wlen, PAnsiChar(Result), ulen, NIL, NIL);
      {$ELSE}
      l := WideCharToMultiByte(CP,0, @ws[1], - 1, nil, 0, nil, nil); //Checkout the result length
      if l = 0 then Exit;
      SetLength(Result, l - 1); //SetResult Length
      WideCharToMultiByte(CP,0, @ws[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
      {$ENDIF}
    end
    else
    begin
      {$IFDEF DELPHI15_UP}
      wlen := Length(ws);
      ulen := LocaleCharsFromUnicode(CP, WC_COMPOSITECHECK or
        WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        PWideChar(WS), wlen, NIL, 0, NIL, NIL);
      setlength(Result, ulen);
      LocaleCharsFromUnicode(CP, WC_COMPOSITECHECK or
        WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        PWideChar(WS), wlen, PAnsiChar(Result), ulen, NIL, NIL);
      {$ELSE}
      l := WideCharToMultiByte(CP,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, nil, 0, nil, nil); //Checkout the result length
      if l = 0 then Exit;
      SetLength(Result, l - 1); //SetResult Length
      WideCharToMultiByte(CP,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
      {$ENDIF}
    end;
    {$ELSE}
    {now we can look for a better way to implement a solution for FPC like FPC2.7+ can do
    or we implement the libiconv.dll eg. OSX/*nix systems}
    Result := ZAnsiString(Ws);
    {$IFEND}
end;

function AnsiToStringEx(const s: ZAnsiString; const FromCP{$IFNDEF DELPHI12_UP}, ToCP{$ENDIF}: Word): String;
begin
  if s = '' then
    Result := ''
  else
    if ( FromCP = $ffff ) {$IFNDEF DELPHI12_UP} or ( FromCP = ToCP ){$ENDIF}then
      Result := String(s)
    else
      {$IFDEF DELPHI12_UP}
      if FromCP = 65001 then
        result := UTF8ToString(s)
      else
        Result := AnsiToWide(s, FromCP);
      {$ELSE} //Ansi-Compiler
        {$IFDEF FPC}
          {$IFDEF WINDOWS}
          Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
          {$ELSE}
          Result := String(S);
          {now we can look for a better way to implement a solution for FPC like FPC2.7+ can do
          or we implement the libiconv.dll eg. OSX/*nix systems}
          {$ENDIF}
        {$ELSE} // <= Delphi2007
          Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
        {$ENDIF}
      {$ENDIF}
end;

function StringToAnsiEx(const s: String; const {$IFNDEF DELPHI12_UP}FromCP, {$ENDIF} ToCP: Word): ZAnsiString;
begin
  if s = '' then
    Result := ''
  else
    if ( ToCP = $ffff ) {$IFNDEF DELPHI12_UP} or ( FromCP = ToCP ){$ENDIF}then
      Result := ZAnsiString(s)
    else
      {$IFDEF DELPHI12_UP}
      if ToCP = 65001 then
        result := UTF8Encode(s)
      else
          Result := WideToAnsi(s, ToCP);
      {$ELSE} //Ansi-Compiler
        {$IFDEF FPC}
          {$IFDEF WINDOWS}
          Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
          {$ELSE}
          Result := ZAnsiString(S);
          {now we can look for a better way to implement a solution for FPC like FPC2.7+ can do
          or we implement the libiconv.dll eg. OSX/*nix systems}
          {$ENDIF}
        {$ELSE} // <= Delphi2007
          Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
        {$ENDIF}
      {$ENDIF}
end;

{**
  EgonHugeist:
  Now use the new Functions to get encoded Strings instead of
  hard-coded Compiler-Directives or UTF8Encode/Decode:

  function ZDbcString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
  function ZPlainString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;

  These functions do auto arrange the in/out-coming AnsiStrings in
  dependency of the used CharacterSet and the used Compiler whithout
  String-DataLoss!!.
  So my thouths where use only these two function for all
  String/Ansi/Unicode-handlings of DBC-layer. Which means in full effect
  no more directives in Zeos Source-Code then here to do this handling.
  @param Ansi: the String which has to be handled.
  @param Encoding is set to Default-Character-Set we've choosen bevor (on conecting)
    Change this if you need some Transtations to a specified Encoding.
    Example: CharacterSet was set to Latin1 and some "special"-String MUST BE
     UTF8 instead of Latin1. (SSL-Keys eventualy)
  @param Convert ignored for Delphi means if the Chararacters should be propper
    to the specified codepage


  IS there a need for it? AnsiEncoded adaps automaticaly to WideString
  So what about coming UTF16/32????
}
function TZCodePagedObject.ZDbcString(const Ansi: ZAnsiString;
  ConSettings: PZConSettings): String;
begin
  case ConSettings.ClientCodePage.Encoding of
    ceUTF8:
      {$IFDEF DELPHI12_UP}
        Result := UTF8ToString(Ansi);
      {$ELSE}
        if ( ConSettings.CPType in [cCP_UTF8, cCP_UTF16] ) or (not ConSettings.AutoEncode) then
          Result := Ansi
        else
          {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
          Result := ConSettings.DbcConvertFunc(Ansi);
          {$ELSE}
          Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.OS_CP);
          {$IFEND}
      {$ENDIF}
    else
      {$IFDEF DELPHI12_UP}
        Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP);
      {$ELSE}
        if ConSettings.AutoEncode then
          {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
          Result := ConSettings.DbcConvertFunc(Ansi)
          {$ELSE}
          Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.OS_CP)
          {$IFEND}
        else
          Result := Ansi;
      {$ENDIF}
    end;
end;

function TZCodePagedObject.ZDbcString(const Ansi: ZAnsiString;
  const Encoding: TZCharEncoding = ceDefault): String;
var
  TempEncoding, UseEncoding: TZCharEncoding;
begin
  if Encoding = ceDefault then
    if not Assigned(FConSettings.ClientCodePage) then
      raise Exception.Create('CodePage-Informations not Assigned!')
    else
      UseEncoding := FConSettings.ClientCodePage^.Encoding
  else
    UseEncoding := Encoding;

  {$IFNDEF DELPHI12_UP}
  if not FConSettings.AutoEncode and ( FConSettings.ClientCodePage^.Encoding = UseEncoding ) then
    Result := Ansi
  else
  {$ENDIF}
  begin
    TempEncoding := FConSettings.ClientCodePage^.Encoding;
    FConSettings.ClientCodePage^.Encoding := UseEncoding;
    Result := ZDbcString(Ansi, FConSettings);
    FConSettings.ClientCodePage^.Encoding := TempEncoding;
  end;
end;

{**
EgonHugeist:
  Now use the new Functions to get encoded Strings instead of
  hard-Coded Compiler-Directives or UTF8Encode/Decode:

  function ZDbcString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
  function ZPlainString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;

  These functions do auto arrange the in/out-coming AnsiStrings in
  dependency of the used CharacterSet and the used Compiler whithout
  String-DataLoss!!.
  So my thouths where Use only these two function for all
  String/Ansi/Unicode-handlings. Which means in full effect no more Directives
  in Zeos Source-Code then here to do this Handling
  @Str: the String which has to be handled.
  @Encoding is set to Default-Character-Set we've choosen bevor (on conecting)
    Change this if you need some Transtations to a specified Encoding.
    Example: CharacterSet was set to Latin1 and some "special"-String MUST BE
     UTF8 instead of Latin1. (SSL-Keys eventualy)
}
function TZCodePagedObject.ZPlainString(const AStr: String;
  ConSettings: PZConSettings): ZAnsiString;
begin
  case ConSettings.ClientCodePage.Encoding of
    ceUTF8:
      {$IFDEF DELPHI12_UP}
      Result := UTF8Encode(AStr);
      {$ELSE}
        if ConSettings.AutoEncode then
          if DetectUTF8Encoding(AStr) in [etUTF8, etUSASCII] then
            Result := AStr
          else
            if ( ConSettings.OS_CP = 65001 ) then //avoid "no success" for expected Codepage UTF8 of the Controls
              Result := AnsiToUTF8(AStr)
            else
              Result := StringToAnsiEx(AStr, ConSettings.OS_CP, 65001)
        else
          Result := AStr;
      {$ENDIF}
    else
      begin
      {$IFDEF DELPHI12_UP}
        Result := StringToAnsiEx(AStr, ConSettings.ClientCodePage.CP);
      {$ELSE}
        if ConSettings.AutoEncode then
          case DetectUTF8Encoding(AStr) of
            etUSASCII: Result := AStr;
            etAnsi:
              {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
              if ConSettings.OS_CP = ConSettings.ClientCodePage.CP then
                Result := AStr
              else
                Result := ConSettings.PlainConvertFunc(AnsiToUTF8(AStr));
              {$ELSE}
              Result := Astr;
              {$IFEND}
            else
              {$IF defined(WITH_LCONVENCODING) and not defined(MSWINDOWS)}
              Result := ConSettings.PlainConvertFunc(AStr);
              {$ELSE}
              Result := StringToAnsiEx(AStr, 65001, ConSettings.ClientCodePage.CP);
              {$IFEND}
          end
        else
          Result := AStr;
      {$ENDIF}
    end;
  end;
end;

function TZCodePagedObject.ZPlainString(const AStr: String;
  const Encoding: TZCharEncoding = ceDefault): ZAnsiString;
var
  TempEncoding, UseEncoding: TZCharEncoding;
begin
  if Encoding = ceDefault then
    if not Assigned(FConSettings.ClientCodePage) then
      raise Exception.Create('CodePage-Informations not Assigned!')
    else
      UseEncoding := FConSettings.ClientCodePage^.Encoding
  else
    UseEncoding := Encoding;

  {$IFNDEF DELPHI12_UP}
  if not FConSettings.AutoEncode and ( FConSettings.ClientCodePage^.Encoding = UseEncoding ) then
    Result := AStr
  else
  {$ENDIF}
  begin
    TempEncoding := FConSettings.ClientCodePage.Encoding;
    FConSettings.ClientCodePage.Encoding := UseEncoding;
    Result := ZPlainString(AStr, FConSettings);
    FConSettings.ClientCodePage.Encoding := TempEncoding;
  end;
end;

function TZCodePagedObject.ZStringFromUnicode(const ws: ZWideString; const Encoding: TZCharEncoding = ceDefault): String;
{$IFNDEF DELPHI12_UP}
var
  UseEncoding: TZCharEncoding;
{$ENDIF}
begin
  {$IFDEF DELPHI12_UP}
  Result := WS;
  {$ELSE}
  if Encoding = ceDefault then
    if not Assigned(FConSettings.ClientCodePage) then
      raise Exception.Create('CodePage-Informations not Assigned!')
    else
      UseEncoding := FConSettings.ClientCodePage^.Encoding
  else
    UseEncoding := Encoding;

  case UseEncoding of
    ceUTF8:
      Result := UTF8Encode(WS);
    else
      Result := WideToAnsi(ws, FConSettings.OS_CP);
  end;
  {$ENDIF}
end;

function TZCodePagedObject.ZUnicodeFromString(const AStr: String;
  const Encoding: TZCharEncoding = ceDefault): ZWideString;
{$IFNDEF DELPHI12_UP}
var
  UseEncoding: TZCharEncoding;
{$ENDIF}
begin
  {$IFDEF DELPHI12_UP}
  Result := AStr;
  {$ELSE}
  if Encoding = ceDefault then
    if not Assigned(FConSettings.ClientCodePage) then
      raise Exception.Create('CodePage-Informations not Assigned!')
    else
      UseEncoding := FConSettings.ClientCodePage^.Encoding
  else
    UseEncoding := Encoding;

  case UseEncoding of
    ceUTF8:
      Result := UTF8ToString(AStr);
    else
      {$IFDEF DELPHI12_UP} //later for FPC 2.8 too eventual
      Result := Astr;
      {$ELSE}
      Result := AnsiToWide(Astr, FConSettings.ClientCodePage.CP);
      {$ENDIF}
  end;
  {$ENDIF}
end;

destructor TZCodePagedObject.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF UNIX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := dynlibs.LoadLibrary(ModuleName);
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := dynlibs.FreeLibrary(Module);
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  Result := dynlibs.GetProcAddress(Module,Proc)
end;
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
function Hash(S : AnsiString) : LongWord;
Var
  thehash,g,I : LongWord;
begin
   thehash:=0;
   For I:=1 to Length(S) do { 0 terminated }
     begin
     thehash:=thehash shl 4;
     inc(theHash,Ord(S[i]));
     g:=thehash and LongWord($f shl 28);
     if g<>0 then
       begin
       thehash:=thehash xor (g shr 24);
       thehash:=thehash xor g;
       end;
     end;
   If theHash=0 then
     Hash:=$ffffffff
   else
     Hash:=TheHash;
end;

function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
var
  P,PE : PChar;

begin
  Result:=AnsiLowerCase(S);
  P:=PChar(pointer(Result));
  PE:=P+Length(Result);
  while (P<PE) do
    begin
    while (P<PE) and CharInSet(P^, WordDelims) do
      inc(P);
    if (P<PE) then
      P^:=UpCase(P^);
    while (P<PE) and not (CharInSet(P^, WordDelims)) do
      inc(P);
    end;
end;
{$ENDIF}

{$IFNDEF WITH_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  result := C in Charset;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  result := Char(C) in Charset;
end;
{$ENDIF}

{$IFDEF  ZUTF8ToString}
function UTF8ToString(const s: AnsiString): WideString;
begin
  Result := UTF8Decode(s);
end;
{$UNDEF ZUTF8ToString}
{$ENDIF}


end.




