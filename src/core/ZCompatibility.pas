{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
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

unit ZCompatibility;

interface

{$I ZCore.inc}

uses
  Variants,
  {$IFDEF WITH_LCONVENCODING}
  LConvEncoding,
  {$ENDIF}
{$IFDEF FPC}
  {$IFDEF UNIX}
    dynlibs,
  {$endif}
{$ENDIF}
  {$IFDEF WITH_WIDESTRUTILS}
  WideStrUtils,
  {$ENDIF}
  {$If defined(MSWINDOWS) and not defined(FPC)}
  Windows,
  {$IFEND}
  Classes, SysUtils;

type
{$IFDEF FPC}
  ULong                 = {$IFDEF WIN64}LongWord{$ELSE}PTRUINT{$ENDIF};
                            // EgonHugeist: Use always a 4Byte Integer as long the PlainDriver dll's are 32Bit for Windows64
                            //on the other hand MySQL64 and FB64 have problems on Win64!
  ULongLong             = QWord;
  NativeInt             = PtrInt;
  NativeUInt            = PtrUInt;
  PNativeUInt           = ^NativeUInt;
{$ELSE}
  {$IFNDEF DELPHI16_UP}
  NativeInt             = Integer;
  NativeUInt            = LongWord;
  PNativeUInt           = ^NativeUInt;
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
  ZAnsiString = {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ELSE}AnsiString{$ENDIF};
  ZWideString = {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF};

  {** Defines the Target Ansi codepages for the Controls }
  TZControlsCodePage = ({$IFDEF UNICODE}cCP_UTF16, cCP_UTF8, cGET_ACP{$ELSE}{$IFDEF FPC}cCP_UTF8, cCP_UTF16, cGET_ACP{$ELSE}cGET_ACP, cCP_UTF8, cCP_UTF16{$ENDIF}{$ENDIF});

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
  end;

  PZConSettings = ^TZConSettings;
  TZConSettings = record
    AutoEncode: Boolean;        //Check Encoding and or convert string with FromCP ToCP
    CPType: TZControlsCodePage; //the CP-Settings type the controls do expect
    CTRL_CP: Word;                //Target CP of string conversations (CP_ACP/CP_UPF8)
    ClientCodePage: PZCodePage; //The codepage informations of the current characterset
    {$IFDEF WITH_LCONVENCODING}
    PlainConvertFunc: TConvertEncodingFunction;
    DbcConvertFunc: TConvertEncodingFunction;
    {$ENDIF}
    DateFormat: String;
  end;

  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    function ZDbcString(const Ansi: ZAnsiString; ConSettings: PZConSettings): String; overload;
    function ZDbcString(const Ansi: ZAnsiString; FromCP: Word): String; overload;
    function ZDbcString(const Ansi: ZAnsiString; const Encoding: TZCharEncoding = ceDefault): String; overload;
    function ZDbcString(const AStr: ZWideString; const Encoding: TZCharEncoding = ceDefault): String; overload;
    function ZDbcUnicodeString(const AStr: ZAnsiString): ZWideString; overload;
    function ZDbcUnicodeString(const AStr: ZAnsiString; const FromCP: Word): ZWideString; overload;
    {$IFDEF WITH_RAWBYTESTRING}
    function ZDbcUnicodeString(const AStr: String; const FromCP: Word): ZWideString; overload;
    {$ENDIF}
    function ZPlainString(const AStr: String; ConSettings: PZConSettings): ZAnsiString; overload;
    function ZPlainString(const AStr: String; ConSettings: PZConSettings; const ToCP: Word): ZAnsiString; overload;
    function ZPlainString(const AStr: String; const Encoding: TZCharEncoding = ceDefault): ZAnsiString; overload;
    function ZPlainString(const AStr: WideString; const Encoding: TZCharEncoding = ceDefault): ZAnsiString; overload;
    function ZPlainString(const AStr: WideString; ConSettings: PZConSettings): ZAnsiString; overload;
    function ZPlainString(const AStr: WideString; ConSettings: PZConSettings; const ToCP: Word): ZAnsiString; overload;
    function ZPlainUnicodeString(const AStr: String): WideString;
    procedure SetConSettingsFromInfo(Info: TStrings);
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  public
    destructor Destroy; override;
  end;

  {$IFDEF WITH_LCONVENCODING}
  function NoConvert(const s: string): string;
  {$ENDIF}

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; CharWidth: 1; Encoding: ceAnsi;
      CP: $ffff; ZAlias: '');

  ConSettingsDummy: TZConSettings =
    (AutoEncode: False;
      CPType: {$IFDEF DELPHI}{$IFDEF UNICODE}cCP_UTF16{$ELSE}cGET_ACP{$ENDIF}{$ELSE}cCP_UTF8{$ENDIF};
      CTRL_CP: $ffff;
      ClientCodePage: @ClientCodePageDummy;
      {$IFDEF WITH_LCONVENCODING}
      PlainConvertFunc: @NoConvert;
      DbcConvertFunc: @NoConvert;
      {$ENDIF}
    );

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
function UTF8ToString(const s: ZAnsiString): ZWideString;
{$IFEND}

implementation

uses ZEncoding;

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
{$IFDEF WITH_FPC_STRING_CONVERSATION}
var TempAnsi: ZAnsiString;
{$ENDIF}
begin
  {$IFNDEF UNICODE}
  if not ConSettings.AutoEncode then
    Result := Ansi
  else
  {$ENDIF}
    case ConSettings.ClientCodePage.Encoding of
      ceUTF8:
        {$IFDEF UNICODE}
          Result := UTF8ToString(Ansi);
        {$ELSE}
          if ( ConSettings.CPType in [cCP_UTF8, cCP_UTF16] ) then
            Result := Ansi
          else
            {$IFDEF WITH_LCONVENCODING}
            Result := ConSettings.DbcConvertFunc(Ansi);
            {$ELSE}
              {$IFDEF WITH_FPC_STRING_CONVERSATION}
              begin
                //avoid string conversations -> move memory
                TempAnsi := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP);
                SetLength(Result, Length(TempAnsi));
                Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
              end;
              {$ELSE}
              Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP);
              {$ENDIF}
            {$ENDIF}
        {$ENDIF}
      else
        {$IFDEF UNICODE}
        Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP);
        {$ELSE}
          if ConSettings.AutoEncode then
            if ConSettings.ClientCodePage.CP = zCP_NONE then //that's not nice it slows down the incoming strings! Find a way to determine allways the current server CP
              case DetectUTF8Encoding(Ansi) of
                etUSASCII: Result := Ansi;
                etAnsi:
                  if ConSettings.CTRL_CP = zCP_UTF8 then
                    {$IFDEF WITH_FPC_STRING_CONVERSATION}
                    begin
                      //avoid string conversations -> move memory
                      TempAnsi := AnsiToUTF8(Ansi); //hope we've compatible results ))):
                      SetLength(Result, Length(TempAnsi));
                      Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
                    end
                    {$ELSE}
                    Result := AnsiToUTF8(Ansi) //hope we've compatible results ))):
                    {$ENDIF}
                  else
                    Result := Ansi;
                else
                  if ConSettings.CTRL_CP = zCP_UTF8 then
                    Result := Ansi
                  else
                    {$IFDEF WITH_FPC_STRING_CONVERSATION}
                    begin
                      //avoid string conversations -> move memory
                      TempAnsi := AnsiToStringEx(Ansi, zCP_UTF8, ConSettings.CTRL_CP);
                      SetLength(Result, Length(TempAnsi));
                      Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
                    end;
                    {$ELSE}
                      {$IFDEF WITH_LCONVENCODING}
                      Result := Ansi;
                      {$ELSE}
                      Result := AnsiToStringEx(Ansi, zCP_UTF8, ConSettings.CTRL_CP);
                      {$ENDIF}
                    {$ENDIF}
              end
            else
              {$IFDEF WITH_LCONVENCODING}
              Result := ConSettings.DbcConvertFunc(Ansi)
              {$ELSE}
                {$IFDEF WITH_FPC_STRING_CONVERSATION}
                begin
                  //avoid string conversations -> move memory
                  TempAnsi := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP);
                  SetLength(Result, Length(TempAnsi));
                  Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
                end
                {$ELSE}
                Result := AnsiToStringEx(Ansi, ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP)
                {$ENDIF}
              {$ENDIF}
          else
            Result := Ansi;
        {$ENDIF}
      end;
end;

function TZCodePagedObject.ZDbcString(const Ansi: ZAnsiString; FromCP: Word): String;
var
  CurrentCP: Word;
  CurrentEncoding: TZCharEncoding;
begin
  if FromCP = FConsettings.ClientCodePage.CP then
    Result := ZDbcString(Ansi, ConSettings)
  else
  begin
    CurrentCP := FConsettings.ClientCodePage.CP;
    CurrentEncoding := FConSettings.ClientCodePage.Encoding;
    if ( FromCP = zCP_UTF8 ) then
      FConSettings.ClientCodePage.Encoding := ceUTF8
    else
      FConSettings.ClientCodePage.Encoding := ceAnsi;
    FConsettings.ClientCodePage.CP := FromCP;
    Result := ZDbcString(Ansi, FConSettings);
    FConsettings.ClientCodePage.CP := CurrentCP;
    FConSettings.ClientCodePage.Encoding := CurrentEncoding;
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

  {$IFNDEF UNICODE}
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

function TZCodePagedObject.ZDbcUnicodeString(const AStr: ZAnsiString): ZWideString;
begin
  {$IFNDEF WITH_LCONVENCODING}
  Result := AnsiToWide(AStr, FConSettings.ClientCodePage.CP);
  {$ELSE}
    case Consettings.ClientCodePage.Encoding of
      ceAnsi:
        Result := UTF8Decode(ConSettings.DbcConvertFunc(AStr)); //!!!!SLOW Job don twice (Ansi up to wide to UTF8 to Wide)
      else
        Result := UTF8ToString(AStr)
    end;
  {$ENDIF}
end;

function TZCodePagedObject.ZDbcString(const AStr: ZWideString; const Encoding: TZCharEncoding = ceDefault): String;
{$IFDEF WITH_FPC_STRING_CONVERSATION}
var
  TempAnsi: ZAnsiString;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := AStr;
  {$ELSE}
    {$IFDEF WITH_LCONVENCODING}
    Result := UTF8Encode(AStr);
    {$ELSE}
      {$IFDEF WITH_FPC_STRING_CONVERSATION}
      begin
        //avoid string conversations -> move memory
        TempAnsi := WideToAnsi(AStr, FConSettings.CTRL_CP);
        SetLength(Result, Length(TempAnsi));
        Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
      end
      {$ELSE}
      Result := WideToAnsi(AStr, FConSettings.CTRL_CP);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TZCodePagedObject.ZDbcUnicodeString(const AStr: ZAnsiString;
  const FromCP: Word): ZWideString;
begin
  {$IFNDEF WITH_LCONVENCODING}
  Result := AnsiToWide(AStr, FromCP);
  {$ELSE}
  if FromCP = zCP_UTF8 then
    Result := UTF8Decode(AStr)
  else
    if FromCP = ConSettings.ClientCodePage.CP then
      Result := UTF8Decode(ConSettings.DbcConvertFunc(AStr))
    else
      Result := WideString(AStr); //default WideString cast, can't convert
  {$ENDIF}
end;

{$IFDEF WITH_RAWBYTESTRING}
function TZCodePagedObject.ZDbcUnicodeString(const AStr: String; const FromCP: Word): ZWideString;
begin
  {$IFDEF UNICODE}
  Result := AStr;
  {$ELSE}
    {$IFNDEF WITH_LCONVENCODING}
    Result := AnsiToWide(AStr, FromCP);
    {$ELSE}
    if FromCP = zCP_UTF8 then
      Result := UTF8Decode(AStr)
    else
      if FromCP = ConSettings.ClientCodePage.CP then
        Result := UTF8Decode(ConSettings.DbcConvertFunc(AStr))
      else
        Result := WideString(AStr); //default WideString cast, can't convert
    {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}

{**
EgonHugeist:
  Now use the new Functions to get encoded Strings instead of
  hard-Coded Compiler-Directives or UTF8Encode/Decode:

  function ZPlainString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;

  These functions do auto arrange the in/out-coming AnsiStrings in
  dependency of the used CharacterSet and the database uses whithout
  String-DataLoss!! (if possible -> UTF8 is save).
  @param AStr: the String which has to be handled.
  @param Encoding is set to Default-Character-Set we've choosen bevor (on conecting)
    Change this if you need some Transtations to a specified Encoding.
    Example: CharacterSet was set to Latin1 and some "special"-String MUST BE
     UTF8 instead of Latin1. (SSL-Keys eventualy)
}
function TZCodePagedObject.ZPlainString(const AStr: String;
  ConSettings: PZConSettings): ZAnsiString;
{$IFDEF WITH_FPC_STRING_CONVERSATION}
var
  TempAnsi: ZAnsiString;
{$ENDIF}
begin
  case ConSettings.ClientCodePage.Encoding of
    ceUTF8:
      {$IFDEF UNICODE}
      Result := UTF8Encode(AStr);
      {$ELSE}
        if ConSettings.AutoEncode then
          if DetectUTF8Encoding(AStr) in [etUTF8, etUSASCII] then
            Result := AStr
          else
            if ( ConSettings.CTRL_CP = zCP_UTF8 ) or (ConSettings.CTRL_CP = zCP_UTF8) then //avoid "no success" for expected Codepage UTF8 of the Controls
              {$IFDEF WITH_FPC_STRING_CONVERSATION}
              begin
                //avoid string conversations -> move memory
                TempAnsi := AnsiToUTF8(AStr);
                SetLength(Result, Length(TempAnsi));
                Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
              end
              {$ELSE}
              Result := AnsiToUTF8(AStr)
              {$ENDIF}
            else
              {$IFDEF WITH_FPC_STRING_CONVERSATION}
              begin
                //avoid string conversations -> move memory
                TempAnsi := StringToAnsiEx(AStr, ConSettings.CTRL_CP, zCP_UTF8);
                SetLength(Result, Length(TempAnsi));
                Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
              end
              {$ELSE}
                {$IFDEF WITH_LCONVENCODING}
                Result := AnsiToUTF8(AStr)
                {$ELSE}
                Result := StringToAnsiEx(AStr, ConSettings.CTRL_CP, zCP_UTF8)
                {$ENDIF}
              {$ENDIF}
        else
          Result := AStr;
      {$ENDIF}
    else
      begin
      {$IFDEF UNICODE}
        Result := StringToAnsiEx(AStr, ConSettings.ClientCodePage.CP);
      {$ELSE}
        if ConSettings.AutoEncode then
          case DetectUTF8Encoding(AStr) of
            etUSASCII: Result := AStr;
            etAnsi:
              {$IFDEF WITH_LCONVENCODING}
              if ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP then
                Result := AStr
              else
                Result := ConSettings.PlainConvertFunc(AnsiToUTF8(AStr));
              {$ELSE}
              Result := Astr;
              {$ENDIF}
            else
              {$IFDEF WITH_LCONVENCODING}
              Result := ConSettings.PlainConvertFunc(AStr);
              {$ELSE}
                {$IFDEF WITH_FPC_STRING_CONVERSATION}
                begin
                  if ConSettings.ClientCodePage.CP = zCP_NONE then
                    TempAnsi := UTF8ToAnsi(AStr) //hope it's compatible we don't know the server CP here!!
                  else
                    TempAnsi := StringToAnsiEx(AStr, zCP_UTF8, ConSettings.ClientCodePage.CP);
                  //avoid string conversations -> move memory
                  SetLength(Result, Length(TempAnsi));
                  Move(PAnsiChar(TempAnsi)^, PAnsiChar(Result)^, Length(TempAnsi));
                end;
                {$ELSE}
                if ConSettings.ClientCodePage.CP = zCP_NONE then
                  Result := UTF8ToAnsi(AStr) //hope it's compatible we don't know the server CP here!!
                else
                  Result := StringToAnsiEx(AStr, zCP_UTF8, ConSettings.ClientCodePage.CP);
                {$ENDIF}
              {$ENDIF}
          end
        else
          Result := AStr;
      {$ENDIF}
    end;
  end;
end;

function TZCodePagedObject.ZPlainString(const AStr: String; ConSettings: PZConSettings; const ToCP: Word): ZAnsiString;
var
  CurrentCP: Word;
  CurrentEncoding: TZCharEncoding;
begin
  if ToCP = ConSettings.ClientCodePage.CP then
    Result := ZPlainString(AStr, ConSettings)
  else
  begin
    CurrentCP := ConSettings.ClientCodePage.CP;
    CurrentEncoding := ConSettings.ClientCodePage.Encoding;
    ConSettings.ClientCodePage.CP := ToCP;
    if ( ToCP = zCP_UTF8 ) then
      ConSettings.ClientCodePage.Encoding := ceUTF8
    else
      ConSettings.ClientCodePage.Encoding := ceAnsi;
    Result := ZPlainString(AStr, ConSettings);
    ConSettings.ClientCodePage.CP := CurrentCP;
    ConSettings.ClientCodePage.Encoding := CurrentEncoding;
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

  {$IFNDEF UNICODE}
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

function TZCodePagedObject.ZPlainString(const AStr: WideString;
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

  TempEncoding := FConSettings.ClientCodePage.Encoding;
  FConSettings.ClientCodePage.Encoding := UseEncoding;
  Result := ZPlainString(AStr, FConSettings);
  FConSettings.ClientCodePage.Encoding := TempEncoding;
end;

function TZCodePagedObject.ZPlainString(const AStr: WideString;
  ConSettings: PZConSettings): ZAnsiString;
begin
  {$IFDEF WITH_LCONVENCODING}
  Result := ConSettings.PlainConvertFunc(UTF8Encode(AStr));
  {$ELSE}
  Result := WideToAnsi(AStr, ConSettings.ClientCodePage.CP);
  {$ENDIF}
end;

function TZCodePagedObject.ZPlainString(const AStr: WideString; ConSettings: PZConSettings; const ToCP: Word): ZAnsiString;
var
  CurrentCP: Word;
  CurrentEncoding: TZCharEncoding;
begin
  if ToCP = ConSettings.ClientCodePage.CP then
    Result := ZPlainString(AStr, ConSettings)
  else
  begin
    CurrentCP := ConSettings.ClientCodePage.CP;
    CurrentEncoding := ConSettings.ClientCodePage.Encoding;
    ConSettings.ClientCodePage.CP := ToCP;
    if ( ToCP = zCP_UTF8 ) then
      ConSettings.ClientCodePage.Encoding := ceUTF8
    else
      ConSettings.ClientCodePage.Encoding := ceAnsi;
    Result := ZPlainString(AStr, ConSettings);
    ConSettings.ClientCodePage.CP := CurrentCP;
    ConSettings.ClientCodePage.Encoding := CurrentEncoding;
  end;
end;

function TZCodePagedObject.ZPlainUnicodeString(const AStr: String): WideString;
begin
  {$IFDEF UNICODE}
  Result := AStr;
  {$ELSE}
    {$IFDEF WITH_LCONVENCODING}
    Result := UTF8ToString(AStr);
    {$ELSE}
    Result := AnsiToWide(AStr, FConSettings.CTRL_CP);
    {$ENDIF}
  {$ENDIF}
end;

procedure TZCodePagedObject.SetConSettingsFromInfo(Info: TStrings);
begin
  if Assigned(Info) and Assigned(FConSettings) then
  begin
    {$IFDEF UNICODE}
    ConSettings.CTRL_CP := ZDefaultSystemCodePage;
    if Info.values['controls_cp'] = 'GET_ACP' then
      ConSettings.CPType := cGET_ACP
    else
      ConSettings.CPType := cCP_UTF16;
    ConSettings.AutoEncode := True;
    {$ELSE}
      {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) or defined(WITH_LCONVENCODING)}
      ConSettings.AutoEncode := Info.Values['AutoEncodeStrings'] = 'ON'; //compatibitity Option for existing Applications;
      {$ELSE}
      ConSettings.AutoEncode := False;
      {$IFEND}
    if Info.values['controls_cp'] = 'GET_ACP' then
    begin
      ConSettings.CPType := cGET_ACP;
      ConSettings.CTRL_CP := ZDefaultSystemCodePage;
    end
    else
      if Info.values['controls_cp'] = 'CP_UTF8' then
      begin
        ConSettings.CPType := cCP_UTF8;
        ConSettings.CTRL_CP := zCP_UTF8;
      end
      else
        if Info.values['controls_cp'] = 'CP_UTF16' then
        begin
          {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) or defined(WITH_LCONVENCODING)}
          ConSettings.CPType := {$IFDEF WITH_WIDEFIELDS}cCP_UTF16{$ELSE}cCP_UTF8{$ENDIF};
          ConSettings.CTRL_CP := ZDefaultSystemCodePage;
          ConSettings.AutoEncode := True;
          {$ELSE}
          if ConSettings.ClientCodePage.Encoding = ceUTF8 then
          begin
            ConSettings.CPType := {$IFDEF WITH_WIDEFIELDS}cCP_UTF16{$ELSE}cCP_UTF8{$ENDIF};
            ConSettings.CTRL_CP := zCP_UTF8;
            ConSettings.AutoEncode := True;
          end
          else
          begin
            ConSettings.CPType := cCP_UTF8;
            ConSettings.CTRL_CP := zCP_UTF8;
            ConSettings.AutoEncode := False;
          end;
          {$IFEND}
        end
        else // nothing was found set defaults
        begin
          {$IFDEF FPC}
          ConSettings.CPType := cCP_UTF8;
          ConSettings.CTRL_CP := zCP_UTF8;
          {$ELSE}
          ConSettings.CPType := cGET_ACP;
          ConSettings.CTRL_CP := GetACP;
          {$ENDIF}
        end;
    {$ENDIF}
  end;
end;

destructor TZCodePagedObject.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF WITH_LCONVENCODING}
function NoConvert(const s: string): string;
begin
  Result := S;
end;
{$ENDIF}


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
function UTF8ToString(const s: ZAnsiString): ZWideString;
begin
  Result := UTF8Decode(s);
end;
{$UNDEF ZUTF8ToString}
{$ENDIF}


end.




