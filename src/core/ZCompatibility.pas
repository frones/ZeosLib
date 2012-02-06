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
{$IFDEF UNIX}
  {$IFDEF FPC}
    dl,
  {$ELSE}
    libc,
  {$ENDIF}
{$ENDIF}
  {$IFDEF DELPHI15_UP}
  WideStrUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
  LConvEncoding,
  {$ENDIF}
  Classes, SysUtils;

type

{$IFDEF FPC}
  TVariantDynArray      = array of Variant;
  {$IFDEF CPU64}
  ULong                 = QWord;
  {$ELSE}
  ULong                 = LongWord;
  {$ENDIF}
  ULongLong             = QWord;
{$ELSE}
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

{$IFDEF CHECK_CLIENT_CODE_PAGE}
{EgonHugeist:}
type
  TZClientCodePageOption = (coShowSupportedsOnly, coDisableSupportWarning,
    coSetCodePageToConnection, coPreprepareSQL);
  TZClientCodePageOptions = set of TZClientCodePageOption;

  TZCharEncoding = (
    ceDefault, //Internal switch for the two Functions below do not use it as a CodePage-decaration!
    ceUnsupported,  //may be Realy Unsupported CodePages {This must be testet before}
    ceAnsi, //Base Ansi-String: prefered CodePage
    ceUTF8, //UTF8_Unicode: 1-4Byte/Char
    ceUTF16 //UTF16/USC2 Unicode: 2-4 Byte/Char
    {$IFNDEF MSWINDOWS}
    ,ceUTF32 //UTF32 Unicode 4Bytes per Char actual Windows-unsupported!!
    {$ENDIF});
    {Here it possible to add some more, to handle the Ansi->Unicode-Translations}

  TZCodePage = {packed to slow..} record
    Name: String; //Name of Client-CharacterSet
    ID:  Integer; {may be an ordinal value of predefined Types...}
    Encoding: TZCharEncoding; //The Type of String-Translation handling
    CP:  Word; //The CodePage the AnsiString must have to
    ZAlias: String; //A possible (saver?) CharacterSet which is more Zeos compatible...
                    //If it's empty it will be ignored!!!
  end;
  PZCodePage = ^TZCodePage;

  TAbstractCodePagedInterfacedObject = Class(TInterfacedObject)
  private
    FCodePage: PZCodePage;
  protected
    {EgonHugeist:
      Now use the new Functions to get encoded Strings instead of
      hard-Coded Compiler-Directives or UTF8Encode/Decode:

      function ZString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
      function ZAnsiString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;
    These functions do auto arrange the in/out-coming AnsiStrings in
    dependency of the used CharacterSet and the used Compiler}
    function ZString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
    function ZAnsiString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;
    function ZStringW(const ws: WideString; const Encoding: TZCharEncoding = ceDefault): String;
    property ClientCodePage: PZCodePage read FCodePage write FCodePage;
  public
    destructor Destroy; override;
  end;

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; Encoding: ceAnsi; CP: 0; ZAlias: '');

function ZWideToAnsiString(const ws: WideString; codePage: Word): AnsiString;
function ZAnsiToWideString(const s: AnsiString; codePage: Word): WideString;
function ZCPWideString(const ws: WideString; codePage: Word): WideString;
function ZCPCheckedAnsiString(const s: AnsiString; codepage: Word): AnsiString;
function ZCPToAnsiString(const s: AnsiString; codepage: Word): AnsiString;
function ZAnsiStringToCP(const s: AnsiString; codepage: Word): AnsiString;

const
  zCP_ACP = 0; {ASCII US}
  zCP_EBC037 = 37; {EBCDIC Codepage 037}
  zCP_EBC273 = 273; {EBCDIC Code Page 273/1 8-bit Austrian German}
  zCP_EBC277 = 277; {EBCDIC Code Page 277/1 8-bit Danish}
  zCP_EBC278 = 278; {EBCDIC Code Page 278/1 8-bit Swedish}
  zCP_EBC280 = 280; {EBCDIC Code Page 280/1 8-bit Italian}
  zCP_EBC284 = 284; {EBCDIC Code Page 284 8-bit Latin American/Spanish}

  zCP_DOS437 = 437; {MS-DOS odepage 437 (US)}
  zCP_DOS737 = 737; {MS-DOS Codepage 737 (Greek IBM PC defacto Standard)}
  zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
  zCP_DOS850 = 850;	{MS-DOS Codepage 850 (Multilingual Latin 1)}
  zCP_DOS851 = 851; {MS-DOS Codepage 851 (Greece) - obsolete}
  zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
  zCP_DOS853 = 853;	{MS-DOS Codepage 853 (Multilingual Latin 3)}
  zCP_DOS855 = 855;	{MS-DOS Codepage 855 (Russia) - obsolete}
  zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
  zCP_DOS858 = 858; {MS-DOS Codepage 858  {Latin I + Euro symbol}
  zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
  zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
  zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
  zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
  zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
  zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
  zCP_DOS866 = 866; {ibm866	866	Cyrl (DOS)}
  zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}
  zCP_DOS874 = 874; {MS-DOS Codepage 874 (Thai)}
  zCP_EBC875 = 875;	{EBCDIC Codepage 875 (Greek)}
  zCP_EBC924 = 924; {Latin 9 EBCDIC 924}
  zCP_DOS895 = 895; {MS-DOS Codepage 895 (Kamenicky CS)}
  zCP_SHIFTJS = 932; {csshiftjis	932	Shift-JIS, ms_kanji}
  zCP_GB2312 = 936; {csiso58gb231280	936	Chinesisch - VR (GB2312), iso-ir-58}
  zCP_EUCKR = 949; {cseuckr Korean, iso-ir-149, ks-c-5601, ks-c-5601-1987, ks_c_5601-1989}
  zCP_Big5 = 950; {big5, csbig5}
  zCP_EBC1026 = 1026; {EBCDIC Code Page 1026 8-bit Turkish}
  zCP_UNICODE = 1200; {Indicates the Unicode character set, Windows code page 1200}
  zCP_WIN1250 = 1250; {Microsoft Windows Codepage 1250 (EE)}
  zCP_WIN1251 = 1251; {Microsoft Windows Codepage 1251 (Cyrl)}
  zCP_WIN1252 = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
  zCP_WIN1253 = 1253; {Microsoft Windows Codepage 1253 (Greek)}
  zCP_WIN1254 = 1254; {Microsoft Windows Codepage 1254 (Turk)}
  zCP_WIN1255 = 1255; {Microsoft Windows Codepage 1255 (Hebrew)}
  cCP_WIN1256 = 1256; {Microsoft Windows Codepage 1256 (Arab)}
  zCP_WIN1257 = 1257; {Microsoft Windows Codepage 1257 (BaltRim)}
  zCP_WIN1258 = 1258; {Microsoft Windows Codepage 1258 (Viet), TCVN-5712}
  zCP_Latin2 = 28592; {latin2	east european (ISO), iso-8859-2, iso-ir-101,}
  zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}
  zCP_KOI8R = 20866; {cskoi8r	20866	Kyrillisch (KOI8-R)}
  zCP_ISO2022JPSIO = 50222; {Indicates the Internet character set ISO-2022-JP-SIO.}
  zCP_ISO2022JPESC = 50221; {Indicates the Internet character set ISO-2022-JP-ESC.}
  zCP_JAUTODETECT = 50932; {Indicates Japanese auto-detect (50932). }
  zCP_KAUTODETECT = 50949; {Indicates Korean auto-detect (50949).}
  zCP_UTF8 = 65001;
  zCP_UTF7 = 65000;
  zCP_2022kr = 50225; {csiso2022kr	50225	Koreanisch (ISO) }
  zCP_EBC1047 = 1047;	{EBCDIC Codepage 1047}
  zCP_EBC500 = 500;	{EBCDIC Codepage 500}

  {$IFDEF FPC}
  FPCSupportedCodePages: array[0..19] of Word = (
    zCP_DOS850, zCP_DOS858, zCP_DOS866, zCP_DOS874, zCP_SHIFTJS, zCP_GB2312,
    zCP_EUCKR, zCP_Big5, zCP_WIN1250, zCP_WIN1251,zCP_WIN1252, zCP_WIN1253,
    zCP_WIN1254, zCP_WIN1255, cCP_WIN1256, zCP_WIN1257, zCP_WIN1258,
    zCP_Latin2, zCP_KOI8R, zCP_UTF8);
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHI12_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: AnsiString): WideString;
{$IFEND}


implementation

{$IFDEF CHECK_CLIENT_CODE_PAGE}

{$IFDEF FPC}
function UTF8ToCodePagedString(const s: String; CP: Word): String;
begin
  case CP of
    //zCP_ACP = 0; {ASCII US}
    //zCP_DOS437: Reuslt := UTF8ToCP437(S);
    //zCP_DOS737 = 737; {MS-DOS Codepage 737 (Greek IBM PC defacto Standard)}
    //zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
    zCP_DOS850: Result := UTF8ToCP850(s);
    //zCP_DOS851 = 851; {MS-DOS Codepage 851 (Greece) - obsolete}
    //zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
    //zCP_DOS853 = 853;	{MS-DOS Codepage 853 (Multilingual Latin 3)}
    //zCP_DOS855 = 855;	{MS-DOS Codepage 855 (Russia) - obsolete}
    //zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
    zCP_DOS858: Result := UTF8ToCP1252(s); {MS-DOS Codepage 858  {Latin I + Euro symbol}
    //zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
    //zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
    //zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
    //zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
    //zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
    //zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
    zCP_DOS866: Result := UTF8ToCP866(s);
    //zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}
    zCP_DOS874: Result := UTF8ToCP874(s);
    //zCP_EBC875 = 875;	{EBCDIC Codepage 875 (Greek)}
    //zCP_DOS895 = 895; {MS-DOS Codepage 895 (Kamenicky CS)}
    zCP_SHIFTJS: Result := UTF8ToCP932(s);
    zCP_GB2312: Result := UTF8ToCP936(s);
    zCP_EUCKR: Result := UTF8ToCP949(s);
    zCP_Big5: Result := UTF8ToCP950(s);
    //zCP_UNICODE = 1200; {Indicates the Unicode character set, Windows code page 1200}
    zCP_WIN1250: Result := UTF8ToCP1250(S);
    zCP_WIN1251: Result := UTF8ToCP1251(S);
    zCP_WIN1252: Result := UTF8ToCP1252(s);
    zCP_WIN1253: Result := UTF8ToCP1253(s);
    zCP_WIN1254: Result := UTF8ToCP1254(s);
    zCP_WIN1255: Result := UTF8ToCP1255(s);
    cCP_WIN1256: Result := UTF8ToCP1256(s);
    zCP_WIN1257: Result := UTF8ToCP1257(s);
    zCP_WIN1258: Result := UTF8ToCP1258(s);
    zCP_Latin2: Result := UTF8ToISO_8859_2(s);
    //zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}
    zCP_KOI8R: Result := UTF8ToKOI8(s);
    //zCP_ISO2022JPSIO = 50222; {Indicates the Internet character set ISO-2022-JP-SIO.}
    //zCP_ISO2022JPESC = 50221; {Indicates the Internet character set ISO-2022-JP-ESC.}
    //zCP_JAUTODETECT = 50932; {Indicates Japanese auto-detect (50932). }
    //zCP_KAUTODETECT = 50949; {Indicates Korean auto-detect (50949).}
    zCP_UTF8: Result := s;
    //zCP_UTF7 = 65000;
    //zCP_2022kr = 50225; {csiso2022kr	50225	Koreanisch (ISO) }
    //zCP_EBC037 = 37; {EBCDIC Codepage 037}
    //zCP_EBC1026 = 1026;	{EBCDIC Codepage 1026 (Turkish)}
    //zCP_EBC1047 = 1047;	{EBCDIC Codepage 1047}
    //zCP_EBC500 = 500;	{EBCDIC Codepage 500}
    else
      Result := s;
  end;
end;

function CodePagedStringToUTF8(const s: String; CP: Word): String;
begin
  case CP of
    //zCP_ACP = 0; {ASCII US}
    //zCP_DOS437: Reuslt := UTF8ToCP437(S);
    //zCP_DOS737 = 737; {MS-DOS Codepage 737 (Greek IBM PC defacto Standard)}
    //zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
    zCP_DOS850: Result := CP850ToUTF8(s);
    //zCP_DOS851 = 851; {MS-DOS Codepage 851 (Greece) - obsolete}
    //zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
    //zCP_DOS853 = 853;	{MS-DOS Codepage 853 (Multilingual Latin 3)}
    //zCP_DOS855 = 855;	{MS-DOS Codepage 855 (Russia) - obsolete}
    //zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
    zCP_DOS858: Result := CP1252ToUTF8(s); {MS-DOS Codepage 858  {Latin I + Euro symbol}
    //zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
    //zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
    //zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
    //zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
    //zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
    //zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
    zCP_DOS866: Result := CP866ToUTF8(s);
    //zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}
    zCP_DOS874: Result := CP874ToUTF8(s);
    //zCP_EBC875 = 875;	{EBCDIC Codepage 875 (Greek)}
    //zCP_DOS895 = 895; {MS-DOS Codepage 895 (Kamenicky CS)}
    zCP_SHIFTJS: Result := CP932ToUTF8(s);
    zCP_GB2312: Result := CP936ToUTF8(s);
    zCP_EUCKR: Result := CP949ToUTF8(s);
    zCP_Big5: Result := CP950ToUTF8(s);
    //zCP_UNICODE = 1200; {Indicates the Unicode character set, Windows code page 1200}
    zCP_WIN1250: Result := CP1250ToUTF8(S);
    zCP_WIN1251: Result := CP1251ToUTF8(S);
    zCP_WIN1252: Result := CP1252ToUTF8(s);
    zCP_WIN1253: Result := CP1253ToUTF8(s);
    zCP_WIN1254: Result := CP1254ToUTF8(s);
    zCP_WIN1255: Result := CP1255ToUTF8(s);
    cCP_WIN1256: Result := CP1256ToUTF8(s);
    zCP_WIN1257: Result := CP1257ToUTF8(s);
    zCP_WIN1258: Result := UTF8ToCP1258(s);
    zCP_Latin2: Result := CP1258ToUTF8(s);
    //zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}
    zCP_KOI8R: Result := KOI8ToUTF8(s);
    //zCP_ISO2022JPSIO = 50222; {Indicates the Internet character set ISO-2022-JP-SIO.}
    //zCP_ISO2022JPESC = 50221; {Indicates the Internet character set ISO-2022-JP-ESC.}
    //zCP_JAUTODETECT = 50932; {Indicates Japanese auto-detect (50932). }
    //zCP_KAUTODETECT = 50949; {Indicates Korean auto-detect (50949).}
    zCP_UTF8: Result := s;
    //zCP_UTF7 = 65000;
    //zCP_2022kr = 50225; {csiso2022kr	50225	Koreanisch (ISO) }
    //zCP_EBC037 = 37; {EBCDIC Codepage 037}
    //zCP_EBC1026 = 1026;	{EBCDIC Codepage 1026 (Turkish)}
    //zCP_EBC1047 = 1047;	{EBCDIC Codepage 1047}
    //zCP_EBC500 = 500;	{EBCDIC Codepage 500}
    else
      Result := s;
  end;
end;
{$ENDIF}
{**
  Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}

function ZWideToAnsiString(const ws: WideString; codePage: Word): AnsiString;
{$IFNDEF FPC}
var
  {$IFNDEF DELPHI12_UP}
  l: integer;
  {$ENDIF}
  AnsiTemp: {$IFDEF DELPHI14_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
{$ENDIF}
begin
  if ws = '' then
    Result := ''
  else
  begin
    {$IFDEF FPC}
    Result := UTF8Encode(ws)
    {$ELSE}
      {$IFDEF DELPHI12_UP} //use Delphi-RTL cause of possible Marvin-Mode for XE2
      AnsiTemp := UTF8Encode(ws); //total Char-transport. may be server-unsupported
      if ( AnsiTemp <> '' ) and ( codePage <> $ffff )  then
        SetCodePage(AnsiTemp, codePage, True); //Server-Codepage supported!
      Result := AnsiTemp;
      {$ELSE}
      if ( codePage <> $ffff ) and ( ws <> '' ) then
      begin
        l := WideCharToMultiByte(codePage,
          WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
          @ws[1], - 1, nil, 0, nil, nil);
        SetLength(Result, l - 1);
        if l > 1 then
          WideCharToMultiByte(codePage,
            WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
            @ws[1], - 1, @Result[1], l - 1, nil, nil);
      end
      else
        Result := UTF8Encode(ws); //toal chars
      {$ENDIF}
    {$ENDIF}
  end;
end; { ZWAnsiString }

{**
  Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
}
function ZAnsiToWideString(const s: AnsiString; codePage: Word): WideString;
{$IFNDEF DELPHI12_UP AND IFNDEF FPC}
var
  l: integer;
{$ENDIF}
begin
  if s = '' then
    Result := ''
  else
  {$IFDEF FPC}
   Result := UTF8ToString(S)
  {$ELSE}
    {$IFDEF DELPHI14_UP} //possible MARVIN mode...
    if ( codePage <> $ffff ) and ( s <> '' ) then
      Result := UTF8ToString(AnsiToUTF8Ex(s, codePage))
    else
      Result := WideString(s);
    {$ELSE}
    if ( codePage <> $ffff ) and ( s <> '' ) then //Older Delphi-Version can use Allways the Win-APi
    begin
      l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PAnsiChar(@s[1]), - 1, nil, 0);
      SetLength(Result, l - 1);
      if l > 1 then
        MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@s[1]),
          - 1, PWideChar(@Result[1]), l - 1);
    end
    else
      Result := WideString(s);
    {$ENDIF}
  {$ENDIF}
end; { ZWideString }

function ZCPWideString(const ws: WideString; codePage: Word): WideString;
begin
  Result := ZAnsiToWideString(ZWideToAnsiString(ws, codePage), codePage);
end;

{**
  Egonhugeist
  This little function picks unsupported chars out. So the data is 100%
  Server-supported and no Errors where raised
  }
function ZCPCheckedAnsiString(const s: AnsiString; codepage: Word): AnsiString;
begin
  {$IFDEF FPC}
  Result := S;
  {$ELSE}
  Result := ZWideToAnsiString(ZAnsiToWideString(s, CodePage), CodePage);
  {$ENDIF}
end;

function ZCPToAnsiString(const s: AnsiString; codepage: Word): AnsiString;
begin
  {$IFDEF FPC}
  Result := CodePagedStringToUTF8(s, codePage);
  {$ELSE}
  Result := ZWideToAnsiString(ZAnsiToWideString(s, CodePage), CodePage);
  {$ENDIF}
end;

function ZAnsiStringToCP(const s: AnsiString; codepage: Word): AnsiString;
begin
  {$IFDEF FPC}
  Result := UTF8ToCodePagedString(s, codePage);
  {$ELSE}
  Result := ZWideToAnsiString(ZAnsiToWideString(s, CodePage), CodePage);
  {$ENDIF}
end;

{**
  EgonHugeist:
  Now use the new Functions to get encoded Strings instead of
  hard-coded Compiler-Directives or UTF8Encode/Decode:

  function ZString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
  function ZAnsiString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;

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

function TAbstractCodePagedInterfacedObject.ZString(const Ansi: AnsiString;
  const Encoding: TZCharEncoding = ceDefault): String;
var
  UseEncoding: TZCharEncoding;
begin
  if not Assigned(FCodePage) then
    raise Exception.Create('CodePage-Informations not Assigned!');
  if Encoding = ceDefault then
    UseEncoding := FCodePage.Encoding
  else
    UseEncoding := Encoding;

  case UseEncoding of
    ceUTF8, ceUTF16{$IFNDEF MSWINDOWS}, ceUTF32{$ENDIF}:
    //ceUTF16: ;//not done yet, may be interesting for SQLite which supports Execute&Open_16-Functions
    //ceUTF32: //not done yet
    {$IFDEF DELPHI12_UP}
    Result := UTF8ToString(Ansi);
    {$ELSE}
    Result := Ansi;
    {$ENDIF}
  else
    {$IFDEF DELPHI12_UP}
    Result := ZAnsiToWideString(Ansi, FCodePage^.CP);
    {$ELSE}
      {$IFDEF FPC}
      Result := Ansi; //Ansi to Ansi is no Problem!!!
      {$ELSE}
      Result := ZCPCheckedAnsiString(Ansi, FCodePage^.CP);
      {$ENDIF}
    {$ENDIF}
  end;
end;

{**
EgonHugeist:
  Now use the new Functions to get encoded Strings instead of
  hard-Coded Compiler-Directives or UTF8Encode/Decode:

  function ZString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
  function ZAnsiString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;

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
function TAbstractCodePagedInterfacedObject.ZAnsiString(const Str: String;
  const Encoding: TZCharEncoding = ceDefault): AnsiString;
var
  UseEncoding: TZCharEncoding;
begin
  if not Assigned(FCodePage) then
    raise Exception.Create('CodePage-Informations not Assigned!');
  if Encoding = ceDefault then
    UseEncoding := FCodePage.Encoding
  else
    UseEncoding := Encoding;

  case UseEncoding of
    ceUTF8, ceUTF16:
        {$IFDEF DELPHI12_UP}
        Result := AnsiString(UTF8Encode(Str));
        {$ELSE}
        Result := Str;
        {$ENDIF}
    //ceUTF16: ;//not done yet
    //ceUTF32
  else
    begin
      { EgonHugeist:
        To Delphi12_UP and ev. (comming) FPC 2.8 Users:
        This function Result an Ansi-String with default DB CodePage
        Possible Problems:
          if you've CodePage 1252 and add some Chinese Letters the CodePage
          turns to 1200(Windows Unicode) which is able to pick up this 2Byte letters
          So on we've to add an additional Param to my encodingRecord
          like an !SAVE!-Alias, we've to use and the CodePage we must have
          here if it's not UTFx

          BE WARNED!! This is a string-helper Function to handle data loss in
          dependency of the choosen Character-Codepage not
          a solution to enable all chars for your spezified CharacterSet of your
          Connection.}
      {$IFDEF DELPHI12_UP} //later for FPC 2.8 too eventual
        Result := ZWideToAnsiString(Str, FCodePage^.CP);
      {$ELSE}
        {$IFDEF FPC}
        { EgonHugeist:
          Actual the FPC uses CodePage of UTF8 generally.
          FPC doesn't support CodePage-informations so a save prepreparing
          is'nt possible in the Resultsets. So we only can hope the data is
          valid. Or we need a changed/addidtional Cached-Resultset which
          is only for the user-data and NOT for Metadata. While testing these
          prepreparations the Metadata-informations where changed. Or on the
          other hand it's possible thate Lazarus-functions are not right there..}
          Result := Str; //Ansi to Ansi is no Problem!!!
        {$ELSE} //Delphi7=>?<2009
        Result := ZCPCheckedAnsiString(Str, FCodePage^.CP);
        {$ENDIF}
      {$ENDIF}
    end;
  end;
end;

function TAbstractCodePagedInterfacedObject.ZStringW(const ws: WideString; const Encoding: TZCharEncoding = ceDefault): String;
var
  UseEncoding: TZCharEncoding;
begin
  if not Assigned(FCodePage) then
    raise Exception.Create('CodePage-Informations not Assigned!');
  if Encoding = ceDefault then
    UseEncoding := FCodePage.Encoding
  else
    UseEncoding := Encoding;

  case UseEncoding of
    ceUTF8, ceUTF16:
        {$IFDEF DELPHI12_UP}
        Result := WS;
        {$ELSE}
        Result := UTF8Encode(WS);
        {$ENDIF}
    //ceUTF16: ;//not done yet
    //ceUTF32
  else
    begin
      { EgonHugeist:
        To Delphi12_UP and ev. (comming) FPC 2.8 Users:
        This function Result an Ansi-String with default DB CodePage
        Possible Problems:
          if you've CodePage 1252 and add some Chinese Letters the CodePage
          turns to 1200(Windows Unicode) which is able to pick up this 2Byte letters
          So on we've to add an additional Param to my encodingRecord
          like an !SAVE!-Alias, we've to use and the CodePage we must have
          here if it's not UTFx

          BE WARNED!! This is a string-helper Function to handle data loss in
          dependency of the choosen Character-Codepage not
          a solution to enable all chars for your spezified CharacterSet of your
          Connection.}
      {$IFDEF DELPHI12_UP} //later for FPC 2.8 too eventual
        Result := WS;
      {$ELSE}
        {$IFDEF FPC}
        { EgonHugeist:
          Actual the FPC uses CodePage of UTF8 generally.
          FPC doesn't support CodePage-informations so a save prepreparing
          is'nt possible in the Resultsets. So we only can hope the data is
          valid. Or we need a changed/addidtional Cached-Resultset which
          is only for the user-data and NOT for Metadata. While testing these
          prepreparations the Metadata-informations where changed. Or on the
          other hand it's possible thate Lazarus-functions are not right there..}
          Result := UTF8Encode(WS);
        {$ELSE} //Delphi7=>?<2009
        Result := ZWideToAnsiString(Str, FCodePage^.CP);
        {$ENDIF}
      {$ENDIF}
    end;
  end;
end;

destructor TAbstractCodePagedInterfacedObject.Destroy;
begin
  Self.FCodePage := nil;
  inherited Destroy;
end;

{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := HMODULE(dlopen(Modulename, RTLD_GLOBAL));
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := longbool(dlclose(pointer(Module)));
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  Result := dlsym(pointer(Module), Proc);
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

{$IFNDEF DELPHI12_UP}
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


