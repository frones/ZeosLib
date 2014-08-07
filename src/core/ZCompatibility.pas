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
{$IFDEF FPC}
  {$IFDEF UNIX}
    dynlibs,
  {$endif}
{$ENDIF}
  {$If defined(MSWINDOWS) and not defined(FPC)}
  Windows,
  {$IFEND}
  Classes,
  {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF WITH_LCONVENCODING} LConvEncoding,{$ENDIF}
  Types,
  SysUtils;

type
  {$IF not declared(UInt64)}
  UInt64                = QWord;
  {$IFEND}
  {$IF not declared(PUInt64)}
  PUInt64               = {$IFDEF FPC}PQWord{$ELSE}^UInt64{$ENDIF};
  {$IFEND}
  {$IF not declared(PPLongWord)}
  PPLongWord            = ^PLongWord;
  {$IFEND}
{$IFDEF FPC}
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
{$ENDIF}
  // EgonHugeist: Use always a 4Byte unsigned Integer for Windows otherwise MySQL64 has problems on Win64!
  // don't know anything about reported issues on other OS's
  ULong                 = {$IFDEF WIN64}LongWord{$ELSE}NativeUInt{$ENDIF};
  ULongLong             = UInt64;
  PULong                = ^ULong;
  PULongLong            = ^ULongLong;

  UInt                  = LongWord;
  PUInt                 = ^UInt;
  ZPPWideChar           = ^PWideChar;//BCB issue: PPWideChar is not part of system

  {EH: just a clear type/IDE to get the length of String or Array by reading back from
    initial entry(X) - SizeOf(LengthInt) = Length}

  PLengthInt            = ^LengthInt;
  LengthInt             = {$IFDEF FPC}SizeInt{$ELSE}LongInt{$ENDIF};
  PRefCntInt            = ^RefCntInt;
  RefCntInt             = {$IFDEF FPC}SizeInt{$ELSE}LongInt{$ENDIF};
const
  {$IFDEF FPC}
  { ustrings.inc/astrings.inc:
  ....
  @-8  : SizeInt for reference count;
  @-4  : SizeInt for size;
  @    : String + Terminating #0;
  .... }
  StringLenOffSet             = SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Len};
  StringRefCntOffSet          = SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Ref}+SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Len};
  {$ELSE} //system.pas
  StringLenOffSet             = SizeOf(LongInt); {PStrRec.Len}
  StringRefCntOffSet          = SizeOf(LongInt){PStrRec.RefCnt}+SizeOf(LongInt){PStrRec.Len};
  {$ENDIF}
type
  {EH: Keep the Len, Pointer, x.... order in next three records! New field -> add it @the end!}
  PZAnsiRec = ^TZAnsiRec;
  TZAnsiRec = Record
    Len: Cardinal;
    P: PAnsiChar;
  end;

  PZWideRec = ^TZWideRec;
  TZWideRec = Record
    Len: Cardinal;
    P: PWideChar;
  end;

  TZCharRec = Record
    Len: Cardinal; //Length of String
    P: Pointer;    //Allocated Mem of String including #0 terminator
    CP: Word;      //CodePage of the String
  end;

  {$IFNDEF HAVE_TBYTES}
  TBytes = TByteDynArray;
  {$ENDIF}

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

function Hash(S : AnsiString) : LongWord; overload;
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
  {$IFNDEF WITH_RAWBYTESTRING}
  RawByteString = AnsiString;
  {$ENDIF}

  ZWideString = {$IFDEF PWIDECHAR_IS_PUNICODECHAR}UnicodeString{$ELSE}WideString{$ENDIF};

  {$IF not declared(TBooleanDynArray)}
  TBooleanDynArray        = array of Boolean;
  {$IFEND}
  {$IF not declared(TByteDynArray)}
  TByteDynArray           = array of Byte;
  {$IFEND}
  {$IF not declared(TShortIntDynArray)}
  TShortIntDynArray       = array of ShortInt;
  {$IFEND}
  {$IF not declared(TWordDynArray)}
  TWordDynArray           = array of Word;
  {$IFEND}
  {$IF not declared(TSmallIntDynArray)}
  TSmallIntDynArray       = array of SmallInt;
  {$IFEND}
  {$IF not declared(TLongWordDynArray)}
  TLongWordDynArray       = array of LongWord;
  {$IFEND}
  {$IF not declared(TIntegerDynArray)}
  TIntegerDynArray        = array of LongInt;
  {$IFEND}
  {$IF not declared(TCardinalDynArray)}
  TCardinalDynArray       = array of Cardinal;
  {$IFEND}
  {$IF not declared(TUInt64DynArray)}
  TUInt64DynArray         = array of UInt64;
  {$IFEND}
  {$IF not declared(TInt64DynArray)}
  TInt64DynArray          = array of Int64;
  {$IFEND}
  {$IF not declared(TSingleDynArray)}
  TSingleDynArray         = array of Single;
  {$IFEND}
  {$IF not declared(TDoubleDynArray)}
  TDoubleDynArray         = array of Double;
  {$IFEND}
  {$IF not declared(TCurrencyDynArray)}
  TCurrencyDynArray       = array of Currency;
  {$IFEND}
  {$IF not declared(TExtendedDynArray)}
  TExtendedDynArray       = array of Extended;
  {$IFEND}
  {$IF not declared(TDateTimeDynArray)}
  TDateTimeDynArray       = array of TDateTime;
  {$IFEND}
  {$IF not declared(TUTF8StringDynArray)}
  TUTF8StringDynArray     = array of UTF8String;
  {$IFEND}
  {$IF not declared(TAnsiStringDynArray)}
  TAnsiStringDynArray     = array of AnsiString;
  {$IFEND}
  {$IF not declared(TRawByteStringDynArray)}
  TRawByteStringDynArray  = array of RawByteString;
  {$IFEND}
  {$IF not declared(TUnicodeStringDynArray)}
  TUnicodeStringDynArray  = array of ZWideString;
  {$IFEND}
  {$IF not declared(TStringDynArray)}
  TStringDynArray  = array of String;
  {$IFEND}
  {$IF not declared(TBytesDynArray)}
  TBytesDynArray  = array of TBytes;
  {$IFEND}
  {$IF not declared(TInterfaceDynArray)}
  TInterfaceDynArray  = array of IInterface;
  {$IFEND}
  {$IF not declared(TGUIDDynArray)}
  TGUIDDynArray  = array of TGUID;
  {$IFEND}
  {$IF not declared(TPointerDynArray)}
  TPointerDynArray  = array of Pointer;
  {$IFEND}
  TZCharRecDynArray = array of TZCharRec;
type
  {declare move or converter functions for the String Types}
  TZAnsiRecToUTF8 = function(const Src: TZAnsiRec; const RawCP: Word): UTF8String;
  TZAnsiToRaw = function (const Src: AnsiString; const RawCP: Word): RawByteString;
  TZRawToAnsi = function (const Src: RawByteString; const RawCP: Word): AnsiString;
  TZAnsiToUTF8 = function (const Src: AnsiString): UTF8String;
  TZUTF8ToAnsi = function (const Src: UTF8String): AnsiString;
  TZRawToUTF8 = function (const Src: RawByteString; const CP: Word): UTF8String;
  TZUTF8ToRaw = function (const Src: UTF8String; const CP: Word): RawByteString;
  TZRawToString = function (const Src: RawByteString; const RawCP, StringCP: Word): String;
  TZStringToRaw = function (const Src: String; const StringCP, RawCP: Word): RawByteString;
  TZUTF8ToString = function (const Src: UTF8String; const StringCP: Word): String;
  TZStringToUTF8 = function (const Src: String; const StringCP: Word): UTF8String;
  TZAnsiToString = function (const Src: AnsiString; const StringCP: Word): String;
  TZStringToAnsi = function (const Src: String; const StringCP: Word): AnsiString;
  TZRawToUnicode = function (const S: RawByteString; const CP: Word): ZWideString;
  TZUnicodeToRaw = function (const US: ZWideString; CP: Word): RawByteString;
  TZUnicodeToString = function (const Src: ZWideString; const StringCP: Word): String;
  TZStringToUnicode = function (const Src: String; const StringCP: Word): ZWideString;
  TZAnsiRecToString = function (const Value: TZAnsiRec; const StringCP: Word): String;
  TZWideRecToString = function (const Value: TZWideRec; const StringCP: Word): String;

  {** Defines the Target Ansi codepages for the Controls }
  TZControlsCodePage = ({$IFDEF UNICODE}cCP_UTF16, cCP_UTF8, cGET_ACP{$ELSE}{$IFDEF FPC}cCP_UTF8, cCP_UTF16, cGET_ACP{$ELSE}cGET_ACP, cCP_UTF8, cCP_UTF16{$ENDIF}{$ENDIF});

  TZCharEncoding = (
    ceDefault,  //Internal switch for the two Functions below do not use it as a CodePage-declaration!
    ceAnsi,     //Base Ansi-String: prefered CodePage
    ceUTF8,     //UTF8_Unicode: 1-4Byte/Char
    ceUTF16,    //Wide or Unicode string encoding for Field-mapping
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
    IsStringFieldCPConsistent: Boolean; //Is the current client characterset codepage consistent for all codepages?
  end;

  TConvertEncodingFunctions = record
    ZAnsiToUTF8: TZAnsiToUTF8;
    ZUTF8ToAnsi: TZUTF8ToAnsi;
    ZUTF8ToString: TZUTF8ToString;
    ZStringToUTF8: TZStringToUTF8;
    ZAnsiToRaw: TZAnsiToRaw;
    ZRawToAnsi: TZRawToAnsi;
    ZRawToUTF8: TZRawToUTF8;
    ZUTF8ToRaw: TZUTF8ToRaw;
    ZStringToRaw: TZStringToRaw;
    ZRawToString: TZRawToString;
    ZAnsiToString: TZAnsiToString;
    ZStringToAnsi: TZStringToAnsi;
    ZUnicodeToRaw: TZUnicodeToRaw;
    ZRawToUnicode: TZRawToUnicode;
    ZUnicodeToString: TZUnicodeToString;
    ZStringToUnicode: TZStringToUnicode;
    ZAnsiRecToString: TZAnsiRecToString;
    ZWideRecToString: TZWideRecToString;
    ZAnsiRecToUTF8: TZAnsiRecToUTF8;
  end;

  TZFormatSettings = Record
    DateFormat: RawByteString;
    DateFormatLen: Cardinal;
    PDateFormat: PAnsiChar;
    TimeFormat: RawByteString;
    TimeFormatLen: Cardinal;
    PTimeFormat: PAnsiChar;
    DateTimeFormat: RawByteString;
    DateTimeFormatLen: Cardinal;
    PDateTimeFormat: PAnsiChar;
  End;

  PZConSettings = ^TZConSettings;
  TZConSettings = record
    AutoEncode: Boolean;        //Check Encoding and or convert string with FromCP ToCP
    CPType: TZControlsCodePage; //the CP-Settings type the controls do expect
    CTRL_CP: Word;              //Target CP of string conversion (CP_ACP/CP_UPF8)
    ConvFuncs: TConvertEncodingFunctions; //a rec for the Convert functions used by the objects
    ClientCodePage: PZCodePage; //The codepage informations of the current characterset
    DisplayFormatSettings: TZFormatSettings;
    ReadFormatSettings: TZFormatSettings;
    WriteFormatSettings: TZFormatSettings;
    {$IFDEF WITH_LCONVENCODING}
    PlainConvertFunc: TConvertEncodingFunction;
    DbcConvertFunc: TConvertEncodingFunction;
    {$ENDIF}
    DataBaseSettings: Pointer;
    Protocol, Database, User: RawByteString;
  end;

  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    procedure SetConSettingsFromInfo(Info: TStrings);
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  end;

  {$IFDEF WITH_LCONVENCODING}
  function NoConvert(const s: string): string;
  {$ENDIF}


{$IFNDEF WITH_CHARINSET}
function CharInSet(const C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function CharInSet(const C: WideChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
{$IFEND}

{$IFDEF UNICODE}
function Hash(const Key : ZWideString) : Cardinal; {$IFNDEF FPC}overload;{$ENDIF}
{$ENDIF}

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: AnsiString); overload;
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String); overload;
procedure ZSetString(const Src: Pointer; const Len: Cardinal; var Dest: ZWideString); overload;
{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString); overload;
{$ENDIF}

var
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; CharWidth: 1; Encoding: ceAnsi;
      CP: $ffff; ZAlias: ''{%H-});

  ConSettingsDummy: TZConSettings =
    (AutoEncode: False;
      CPType: {$IFDEF DELPHI}{$IFDEF UNICODE}cCP_UTF16{$ELSE}cGET_ACP{$ENDIF}{$ELSE}cCP_UTF8{$ENDIF};
      ClientCodePage: {%H-}@ClientCodePageDummy;
      DisplayFormatSettings:
        (DateFormat: 'DD-MM-YYYY';
          DateFormatLen: 10;
          PDateFormat: 'DD-MM-YYYY';
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          PTimeFormat: 'HH:NN:SS.ZZZ';
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS';
          DateTimeFormatLen: 23;
          PDateTimeFormat: 'DD-MM-YYYY HH:NN:SS');
      ReadFormatSettings:
          (DateFormat: 'DD-MM-YYYY';
          DateFormatLen: 10;
          PDateFormat: 'DD-MM-YYYY';
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          PTimeFormat: 'HH:NN:SS.ZZZ';
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ';
          DateTimeFormatLen: 23;
          PDateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ');
      WriteFormatSettings:
          (DateFormat: 'DD-MM-YYYY';
          DateFormatLen: 10;
          PDateFormat: 'DD-MM-YYYY';
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          PTimeFormat: 'HH:NN:SS.ZZZ';
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ';
          DateTimeFormatLen: 23;
          PDateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ');
      {$IFDEF WITH_LCONVENCODING}
      PlainConvertFunc: @NoConvert;
      DbcConvertFunc: @NoConvert;
      {$ENDIF}
    {%H-});

var
  ZDefaultSystemCodePage: Word;

implementation

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
        ConSettings.CTRL_CP := 65001;
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
            ConSettings.CTRL_CP := 65001;
            ConSettings.AutoEncode := True;
          end
          else
          begin
            ConSettings.CPType := cCP_UTF8;
            ConSettings.CTRL_CP := 65001;
            ConSettings.AutoEncode := False;
          end;
          {$IFEND}
        end
        else // nothing was found set defaults
        begin
          {$IFDEF FPC}
          ConSettings.CPType := cCP_UTF8;
          ConSettings.CTRL_CP := 65001;
          {$ELSE}
          ConSettings.CPType := cGET_ACP;
          ConSettings.CTRL_CP := GetACP;
          {$ENDIF}
        end;
    {$ENDIF}
  end;
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

{$IFDEF UNICODE}
function Hash(const key: ZWideString): Cardinal;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to length(key) do
  begin
    Result := (Result shl 5) or (Result shr 27);
    Result := Result xor Cardinal(key[I]);
  end;
end; { Hash }
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
     {$IFOPT Q+}
       {$DEFINE OverFlowCheckEnabled}
       {$OVERFLOWCHECKS OFF}
     {$ENDIF}
     inc(theHash,Ord(S[i]));
     {$IFDEF OverFlowCheckEnabled}
       {$OVERFLOWCHECKS ON}
     {$ENDIF}
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
function CharInSet(const C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  result := C in Charset;
end;

function CharInSet(const C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  result := Char(C) in Charset;
end;
{$ENDIF}

{$IFDEF  ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
begin
  Result := UTF8Decode(s);
end;
{$UNDEF ZUTF8ToString}
{$ENDIF}

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: AnsiString);
begin
  if ( Len = 0 ) or ( Src = nil ) then
    Dest := ''
  else
    {$IFNDEF FPC}
    if (Pointer(Dest) <> nil) and //Empty?
       (PLongInt(NativeInt(Dest) - 8)^ = 1) {refcount} and
       (PLongInt(NativeInt(Dest) - 4)^ = LongInt(Len)) {length} then
      Move(Src^, Pointer(Dest)^, Len)
    else
    {$ENDIF}
      SetString(Dest, Src, Len);
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String);
begin
  if ( Len = 0 ) or ( Src = nil ) then
    Dest := ''
  else
    {$IFNDEF FPC}
    if (Pointer(Dest) <> nil) and //Empty?
       (PLongInt(NativeInt(Dest) - 8)^ = 1) {refcount} and
       (PLongInt(NativeInt(Dest) - 4)^ = LongInt(Len)) {length} then
      Move(Src^, Pointer(Dest)^, Len)
    else
    {$ENDIF}
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := '';
        SetLength(Dest, Len);
        Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;

procedure ZSetString(const Src: Pointer; const Len: Cardinal; var Dest: ZWideString); overload;
begin
  Dest := ''; //speeds up for SetLength
  if ( Len = 0 ) or ( Src = nil ) then
    Exit
  else
  begin
    SetLength(Dest, Len div 2);
    Move(Src^, Pointer(Dest)^, Len);
  end;
end;

{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString);
begin
  if ( Len = 0 ) or ( Src = nil ) then
    Dest := ''
  else
    {$IFNDEF FPC}
    if (NativeUInt(Dest) <> 0) and //Empty?
       (PLongInt(NativeInt(Dest) - 8)^ = 1) {refcount} and
       (PLongInt(NativeInt(Dest) - 4)^ = LongInt(Len)) {length} then
      Move(Src^, Pointer(Dest)^, Len)
    else
    {$ENDIF}
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := '';
        SetLength(Dest, Len);
        Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;
{$ENDIF}


initialization
  case ConSettingsDummy.CPType of
    cCP_UTF16, cGET_ACP: ConSettingsDummy.CTRL_CP := ZDefaultSystemCodePage;
    cCP_UTF8: ConSettingsDummy.CTRL_CP := 65001;
  end;
end.




