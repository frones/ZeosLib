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
    dynlibs,
  {$endif}
{$ENDIF}
  {$IFDEF WITH_WIDESTRUTILS}
  WideStrUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
    Types,
  {$endif}
  Classes, SysUtils;

type
{$IFDEF FPC}
  ULong                 = {$IFDEF WIN64}LongWord{$ELSE}PTRUINT{$ENDIF};
                            // EgonHugeist: Use always a 4Byte Integer as long the PlainDriver dll's are 32Bit for Windows64
                            //on the other hand MySQL64 and FB64 have problems on Win64!
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

{EgonHugeist:}
type
  TZCharEncoding = (
    ceDefault, //Internal switch for the two Functions below do not use it as a CodePage-decaration!
    //ceUnsupported,  //may be Realy Unsupported CodePages {This must be testet before}
    ceAnsi, //Base Ansi-String: prefered CodePage
    ceUTF8, //UTF8_Unicode: 1-4Byte/Char
    ceUTF16, ceUTF32);

    {Here it's possible to add some more, to handle the Ansi->Unicode-Translations}

  TZCodePage = {packed to slow..} record
    Name: String; //Name of Client-CharacterSet
    ID:  Integer; {may be an ordinal value of predefined Types...}
    Encoding: TZCharEncoding; //The Type of String-Translation handling
    {$IFDEF WITH_CHAR_CONTROL} CP:  Word; {$ENDIF} //The CodePage the AnsiString must have to
    ZAlias: String; //A possible (saver?) CharacterSet which is more Zeos compatible...
                    //If it's empty it will be ignored!!!
    IsSupported: Boolean;
  end;
  PZCodePage = ^TZCodePage;

  TAbstractCodePagedInterfacedObject = Class(TInterfacedObject)
  private
    FCodePage: PZCodePage;
  protected
    function ZDbcString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
    function ZPlainString(const AStr: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;
    function ZStringW(const ws: WideString; const Encoding: TZCharEncoding = ceDefault): String;
    property ClientCodePage: PZCodePage read FCodePage write FCodePage;
  public
    destructor Destroy; override;
  end;

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; Encoding: ceAnsi; {$IFDEF WITH_CHAR_CONTROL}
      CP: 0;{$ENDIF} ZAlias: '');

{$IFDEF WITH_CHAR_CONTROL}
  ZFullMultiByteCodePages: array[0..21] of Word = (50220, 50221, 50222, 50225,
    50227, 50229, 52936, 54936, 54936, 57002, 57003, 57004, 57005, 57006,
    57007, 57008, 57009, 57010, 57011, 65000, 65001, 42);

function ZWideToAnsiString(const ws: WideString; CP: Word): AnsiString;
function ZAnsiToWideString(const s: AnsiString; CP: Word): WideString;
function ZCPWideString(const ws: WideString; CP: Word): WideString;
function ZCPCheckedAnsiString(const s: AnsiString; CP: Word): AnsiString;
function ZCPToAnsiString(const s: AnsiString; CP: Word): AnsiString;
function ZAnsiStringToCP(const s: AnsiString; CP: Word): AnsiString;
{$ENDIF}

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

{$IFDEF WITH_CHAR_CONTROL}
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
  {**
    Converts Unicode string to Ansi string using specified code page.
    @param   ws       Unicode string.
    @param   codePage Code page to be used in conversion.
    @returns Converted ansi string.
  }

  function ZWideToAnsiString(const ws: WideString; CP: Word): AnsiString;
  {$IFNDEF FPC}
  var
    {$IFNDEF DELPHI12_UP}
    l: integer;
    {$ELSE}
    AnsiTemp: {$IFDEF DELPHI14_UP}RawByteString{$ELSE}AnsiString{$ENDIF};
    {$ENDIF}
  {$ENDIF}
  begin
    if ws = '' then
      Result := ''
    else
      if CP = 65001 then
        Result := UTF8Encode(WS)
      else
      begin
        {$IFDEF FPC}
        Result := Result;
        {$ELSE}
          {$IFDEF DELPHI12_UP} //use Delphi-RTL cause of possible Marvin-Mode for XE2
          AnsiTemp := UTF8Encode(ws); //total Char-transport. may be server-unsupported
          if ( AnsiTemp <> '' ) and ( CP <> $ffff )  then
            SetCodePage(AnsiTemp, CP, True); //Server-CP supported!
          Result := AnsiTemp;
          {$ELSE} //Here we can use faster Delphi-RTL if somebode knows how!
          if ( CP <> $ffff ) then //EgonHugeist
          begin
            if ( IsFullMultiByteCodePage(CP)) then //dwFlags MUST be Set to 0!!!!
            begin
              l := WideCharToMultiByte(CP,0, @ws[1], - 1, nil, 0, nil, nil); //Checkout the result length
              SetLength(Result, l - 1); //SetResult Length
              if l > 1 then
                WideCharToMultiByte(CP,0, @ws[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
            end
            else
            begin
              l := WideCharToMultiByte(CP,
                WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
                @ws[1], - 1, nil, 0, nil, nil); //Checkout the result length
              SetLength(Result, l - 1); //SetResult Length
              if l > 1 then
                WideCharToMultiByte(CP,
                  WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
                  @ws[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
            end;
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
    @param   CP Code page to be used in conversion.
    @returns Converted wide string.
  }
  function ZAnsiToWideString(const s: AnsiString; CP: Word): WideString;
  {$IFNDEF FPC}
    {$IFNDEF DELPHI14_UP}
  var
      {$IFNDEF DELPHI12_UP}
      l: integer;
      {$ELSE}
      AnsiTemp: AnsiString;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  begin
    if s = '' then
      Result := ''
    else
      if CP = 65001 then
        result := UTF8ToString(s)
      else
        {$IFDEF FPC}
        Result := WideString(S)
        {$ELSE}
          {$IFDEF DELPHI12_UP}
            if ( CP <> $ffff ) then
            {$IFDEF DELPHI14_UP} //possible MARVIN mode...
              Result := UTF8ToString(AnsiToUTF8Ex(s, CP))
            {$ELSE}
              begin
                AnsiTemp := s;
                SetCodePage(AnsiTemp, CP);
                Result := WideString(AnsiTemp);
              end
            {$ENDIF}
            else
              Result := WideString(s);
          {$ELSE} //Here we can use faster Delphi-RTL if somebode knows how!
            if ( CP <> $ffff ) then //Older Delphi-Version can use Allways the Win-APi
            begin
              if ( IsFullMultiByteCodePage(CP) ) then //dwFlags must be set to 0!!!
              begin
                l := MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]), - 1, nil, 0); //Checkout the Result-Lengh
                SetLength(Result, l - 1); //Set Result-Length
                if l > 1 then
                  MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]),
                    - 1, PWideChar(@Result[1]), l - 1); //Convert Ansi to Wide with supported Chars
              end
              else
              begin
                l := MultiByteToWideChar(CP, MB_PRECOMPOSED, PAnsiChar(@s[1]), - 1, nil, 0); //Checkout the Result-Lengh
                SetLength(Result, l - 1);
                if l > 1 then
                  MultiByteToWideChar(CP, MB_PRECOMPOSED, PAnsiChar(@s[1]),
                    - 1, PWideChar(@Result[1]), l - 1); //Convert Ansi to Wide with supported Chars
              end;
            end
            else
              Result := WideString(s);
          {$ENDIF}
        {$ENDIF}
  end; { ZWideString }

  function ZCPWideString(const ws: WideString; CP: Word): WideString;
  begin
    Result := ZAnsiToWideString(ZWideToAnsiString(ws, CP), CP);
  end;

  {**
    Egonhugeist
    This little function picks unsupported chars out. So the data is 100%
    Server-supported and no Errors where raised
    }
  function ZCPCheckedAnsiString(const s: AnsiString; CP: Word): AnsiString;
  begin
    {$IFDEF FPC}
    Result := S;
    {$ELSE}
    Result := ZWideToAnsiString(ZAnsiToWideString(s, CP), CP);
    {$ENDIF}
  end;

  function ZCPToAnsiString(const s: AnsiString; CP: Word): AnsiString;
  begin
    {$IFDEF FPC}
    Result := S;
    {$ELSE}
    Result := ZWideToAnsiString(ZAnsiToWideString(s, CP), CP);
    {$ENDIF}
  end;

  function ZAnsiStringToCP(const s: AnsiString; CP: Word): AnsiString;
  begin
    {$IFDEF FPC}
    Result := S;
    {$ELSE}
    Result := ZWideToAnsiString(ZAnsiToWideString(s, CP), CP);
    {$ENDIF}
  end;
{$ENDIF}
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

function TAbstractCodePagedInterfacedObject.ZDbcString(const Ansi: AnsiString;
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
    ceUTF8:
    {$IFDEF DELPHI12_UP}
    if DetectUTF8Encoding(Ansi) = etUTF8 then
      Result := UTF8ToString(Ansi) //Decode the AnsiString
    else
      Result := String(Ansi); //Reference the AnsiString and move 'em up to UnicodeString
    {$ELSE}
      {.$IFDEF FPC}
      Result := Ansi;
      {.$ELSE}
      {if DetectUTF8Encoding(Ansi) = etUTF8 then //Take care we've rael ansi as result
        Result := UTF8ToAnsi(Ansi)
      else
        Result := Ansi;
      {$ENDIF}
    {$ENDIF}
  else
    {$IFDEF DELPHI12_UP}
      {$IFDEF WITH_CHAR_CONTROL}
      Result := ZAnsiToWideString(Ansi, FCodePage^.CP);
      {$ELSE}
      Result := String(Ansi);
      {$ENDIF}
    {$ELSE}
      {$IFDEF FPC}
      Result := Ansi; //Ansi to Ansi is no Problem!!!
      {$ELSE}
        {$IFDEF WITH_CHAR_CONTROL}
        Result := ZCPCheckedAnsiString(Ansi, FCodePage^.CP);
        {$ELSE}
        Result := Ansi;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
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
function TAbstractCodePagedInterfacedObject.ZPlainString(const AStr: String;
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
    ceUTF8:
      {$IFDEF DELPHI12_UP}
      Result := AnsiString(UTF8Encode(AStr));
      {$ELSE}
        {$IFDEF FPC}
        Result := AStr;
        {$ELSE}
        if DetectUTF8Encoding(AStr) in [etUTF8, etUSASCII] then
          Result := AStr
        else
          Result := AnsiToUTF8(AStr);
        {$ENDIF}
      {$ENDIF}
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
        {$IFDEF WITH_CHAR_CONTROL}
        Result := ZWideToAnsiString(AStr, FCodePage^.CP);
        {$ELSE}
        Result := AnsiString(AStr);
        {$ENDIF}
      {$ELSE}
        {$IFDEF FPC}
        { EgonHugeist:
          Actual the FPC uses CodePage of UTF8 generally.
          FPC doesn't support CodePage-informations so a save prepreparing
          is'nt possible in the Resultsets. So we only can hope the data is
          valid. Or we need a changed/addidtional Cached-Resultset which
          is only for the user-data and NOT for Metadata. While testing these
          prepreparations the Metadata-informations where changed. Or on the
          other hand it's possible that the Lazarus-functions are not right there..}
          Result := AStr; //Ansi to Ansi is no Problem!!!
        {$ELSE} //Delphi7=>?<2009
          {$IFDEF WITH_CHAR_CONTROL}
          Result := ZCPCheckedAnsiString(AStr, FCodePage^.CP);
          {$ELSE}
          Result := AStr;
          {$ENDIF}
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
    ceUTF8:
        {$IFDEF DELPHI12_UP}
        Result := WS;
        {$ELSE}
        Result := UTF8Encode(WS);
        {$ENDIF}
    else
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
        {$IFDEF WITH_CHAR_CONTROL}
        Result := ZWideToAnsiString(ws, FCodePage^.CP);
        {$ELSE}
        Result := UTF8Encode(WS);
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;
end;

destructor TAbstractCodePagedInterfacedObject.Destroy;
begin
  Self.FCodePage := nil;
  inherited Destroy;
end;

{$IFDEF UNIX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  //Result := HMODULE(dlopen(Modulename, RTLD_GLOBAL));
  Result := dynlibs.LoadLibrary(ModuleName);
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  //Result := longbool(dlclose(pointer(Module)));
  Result := dynlibs.FreeLibrary(Module);
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  //Result := dlsym(pointer(Module), Proc);
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




