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
const
  zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
  zCP_DOS866 = 866; {ibm866	866	Kyrillisch (DOS)}
  zCP_WIN874 = 874; {windows-874	874	Thailändisch}
  zCP_SHIFTJS = 932; {csshiftjis	932	Shift-JIS, ms_kanji}
  zCP_GB2312 = 936; {csiso58gb231280	936	Chinesisch - VR (GB2312), iso-ir-58}
  zCP_EUCKR = 949; {cseuckr Korean, iso-ir-149, ks-c-5601, ks-c-5601-1987, ks_c_5601-1989}
  zCP_Big5 = 950; {big5, csbig5}
  zCP_WIN1250 = 1250; {x-cp1250	1250	Osteuropäisch (Windows), Window-Unicode}
  zCP_WIN1251 = 1251; {x-cp1251	1251	Kyrillisch (Windows)}
  zCP_Latin1 = 1252; {cp367, cp819, ansi_x3.4-1968, ansi_x3.4-1986, ascii, csascii, iso-8859-1, iso646-us, ibm367, ibm819, Latin1, iso-ir-100, iso-ir-6, iso_646.irv}
  zCP_WIN1253 = 1253; {windows-1253	1253	Greck (Windows) }
  zCP_WIN1254 = 1254; { windows-1254	1254	Türkisch (Windows) }
  zCP_WIN1255 = 1255; {  csisolatinhebrew	1255	Hebräisch (ISO-Visual), so-ir-138, iso-8859-8}
  cCP_WIN1256 = 1256; { windows-1256	1256	Arabisch}
  zCP_WIN1257 = 1257; {windows-1257	1257	Baltic (Windows)}
  zCP_WIN1285 = 1285; {windows-1258	1258	Vietnamese}
  zCP_Latin2 = 28592; {latin2	east european (ISO), iso-8859-2, iso-ir-101}
  zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}
  zCP_KOI8R = 20866; {cskoi8r	20866	Kyrillisch (KOI8-R)}
  zCP_UTF8 = 65001;
  zCP_UTF7 = 65000;
  zCP_2022kr = 50225; {csiso2022kr	50225	Koreanisch (ISO) }
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
    property ClientCodePage: PZCodePage read FCodePage write FCodePage;
  public
    destructor Destroy; override;
  end;

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; Encoding: ceAnsi; CP: 0; ZAlias: '');

{$ENDIF}

{$IFNDEF DELPHI12_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: AnsiString): WideString;
{$IFEND}

{$IFDEF MSWINDOWS}
function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
{$ENDIF}

implementation

{$IFDEF CHECK_CLIENT_CODE_PAGE}

{$IFDEF MSWINDOWS}
{**
  Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}

function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], - 1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }

{**
  Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
}
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: integer;
begin
  if s = '' then
    Result := ''
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PAnsiChar(@s[1]), - 1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(@s[1]),
        - 1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString }
{$ENDIF}

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
    {$IFDEF FPC}
    Result := Ansi; //Ansi to Ansi is no Problem!!!
    {$ELSE}
    Result := UTF8ToString(Ansi);
    {$ENDIF}
  else
    {$IFDEF DELPHI12_UP}
    Result := String(Ansi); //Ansi to Wide/Unicode is no Problem!!!
    {$ELSE}
    //CodepageCheck of incoming Ansi?
    Result := Ansi; //Systemdefaults no CP aviable Ansi to Ansi is no Problem!!!
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
        {$IFDEF FPC}
        Result := Str;
        {$ELSE}
        Result := AnsiString(UTF8Encode(Str));
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

          BE WARNED!! This is a string-helper Function to handle DataLoss not
          a solution to Enable all chars for your spezified CharacterSet of your
          Connection -> trying this may result an Exception if Chars are not
          supported}


      {$IFDEF DELPHI12_UP} //later for FPC 2.8 too eventual
        {$IFDEF DELPHI15_UP}
          if  FCodePage^.CP <> $ffff then
            Result := Utf8ToAnsiEx(UTF8Encode(Str), FCodePage^.CP) //possible Dataloss for unsupported Chars else the Database raises errors
          else
            Result := Copy(UTF8Encode(Str), 1, Length(UTF8Encode(Str))); //total Char-transport. may be server-unsupported
        {$ELSE}
          Result := Copy(UTF8Encode(Str), 1, Length(UTF8Encode(Str))); //total Char-transport. may be server-unsupported
          if ( Result <> '' ) and ( FCodePage^.CP <> $ffff ) then
            PWord(Integer(Result) - 12)^ := FCodePage^.CP;
        {$ENDIF}
      {$ELSE}
        {$IFDEF FPC} //Lazarus -> FPC 2.6
        { EgonHugeist:
          Actual the FPC uses CodePage of UTF8 generally (or am i wrong?)
          So we need to switch to Result to OS- or better Database-Used CodePage first!
          If this this is correct we have to Copy the String like in Delphi12_UP.
          Maybe if there is somebody who know's a better solution then me,
          please do it!}
          Result := Str; //so this must be testet please!!!!!!!
        {$ELSE} //Delphi7-2005
        {Uses Alway OS-Default Ansi-CodePage so check if we've to switch here too }
        Result := Str;
        if Result <> '' and FCodePage^.CP <> $ffff then
          PWord(Integer(Result) - 12)^ := FCodePage^.CP;
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

