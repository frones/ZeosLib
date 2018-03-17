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
{$IFDEF FPC}
  {$IFDEF UNIX}
    dynlibs,
  {$endif}
{$ENDIF}
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
  {$IF not declared(NativeInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  NativeInt             = PtrInt;
  {$IFEND}
  {$IF not declared(NativeUInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  NativeUInt            = PtrUInt;
  {$IFEND}
  {$IF not declared(PNativeUInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  PNativeUInt           = ^NativeUInt;
  {$IFEND}
{$ELSE}
  {$IFNDEF HAVE_TRUE_NATIVE_TYPES}  //introduced since D2007 but "stable" since XE2
  NativeInt             = Integer;
  NativeUInt            = LongWord;
  PNativeUInt           = ^NativeUInt;
  PWord                 = ^Word; // M.A.
  {$ENDIF}
{$ENDIF}
  // EgonHugeist: Use always a 4Byte unsigned Integer for Windows otherwise MySQL64 has problems on Win64!
  // don't know anything about reported issues on other OS's
  ULong                 = {$IFDEF MSWINDOWS}LongWord{$ELSE}NativeUInt{$ENDIF};
  ULongLong             = UInt64;
  PULong                = ^ULong;
  PULongLong            = ^ULongLong;

  UInt                  = LongWord;
  PUInt                 = ^UInt;
  ZPPWideChar           = ^PWideChar;//BCB issue: PPWideChar is not part of system

  {EH: just a clear type/IDE to get the length of String reading back from
    initial entry(X) - SizeOf(LengthInt) = Length}

  PLengthInt            = ^LengthInt;
  LengthInt             = {$IFDEF FPC}SizeInt{$ELSE}LongInt{$ENDIF};
  PRefCntInt            = ^RefCntInt;
  RefCntInt             = {$IFDEF FPC}SizeInt{$ELSE}LongInt{$ENDIF};
  {EH: just two types for determination DynArray Length if ever something changes we just need a define here.}
  ArrayLenInt           = NativeInt;
  PArrayLenInt          = ^ArrayLenInt;

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
  ArrayLenOffSet              = SizeOf(ArrayLenInt);
type
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
  TPRawToUTF8 = function(const Src: PAnsiChar; Len: NativeUInt; const RawCP: Word): UTF8String;
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
  TPRawToString = function (Src: PAnsiChar; Len: LengthInt; const RawCP, StringCP: Word): String;
  TPUnicodeToString = function (Src: PWideChar; CodePoints: NativeUInt; const StringCP: Word): String;

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
    ZPRawToString: TPRawToString;
    ZPUnicodeToString: TPUnicodeToString;
    ZPRawToUTF8: TPRawToUTF8;
  end;

  TZFormatSettings = Record
    DateFormat: String;
    DateFormatLen: Byte;
    TimeFormat: String;
    TimeFormatLen: Byte;
    DateTimeFormat: String;
    DateTimeFormatLen: Byte;
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
  public
    function GetConSettings: PZConSettings;
  end;

  {$IFDEF WITH_LCONVENCODING}
  function NoConvert(const s: string): string;
  {$ENDIF}


{$IFNDEF WITH_CHARINSET}
function CharInSet(const C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function CharInSet(const C: WideChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function CharInSet(const C: Word; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
{$IFEND}

function Hash(const S : RawByteString) : LongWord; overload;
function Hash(const Key : ZWideString) : Cardinal; overload;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}); overload;// {$IFDEF WITH_INLINE}Inline;{$ENDIF}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String); overload;// {$IFDEF WITH_INLINE}Inline;{$ENDIF}
procedure ZSetString(Src: PAnsiChar; const Len: LengthInt; var Dest: ZWideString); overload;// {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString); overload;// {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

{$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
function Min(const A, B: NativeUInt): NativeUInt; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function Max(const A, B: NativeUInt): NativeUInt; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
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
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS';
          DateTimeFormatLen: 23);
      ReadFormatSettings:
          (DateFormat: 'DD-MM-YYYY';
          DateFormatLen: 10;
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ';
          DateTimeFormatLen: 23);
      WriteFormatSettings:
          (DateFormat: 'DD-MM-YYYY';
          DateFormatLen: 10;
          TimeFormat: 'HH:NN:SS.ZZZ';
          TimeFormatLen: 12;
          DateTimeFormat: 'DD-MM-YYYY HH:NN:SS.ZZZ';
          DateTimeFormatLen: 23);
      {$IFDEF WITH_LCONVENCODING}
      PlainConvertFunc: @NoConvert;
      DbcConvertFunc: @NoConvert;
      {$ENDIF}
    {%H-});

const
  PEmptyUnicodeString: PWideChar = '';
  PEmptyAnsiString: PAnsiChar = '';

var
  ZOSCodePage: Word;

implementation

{$IFDEF FAST_MOVE}
uses ZFastCode;
{$ENDIF}

function TZCodePagedObject.GetConSettings: PZConSettings;
begin
  Result := FConSettings;
end;

procedure TZCodePagedObject.SetConSettingsFromInfo(Info: TStrings);
begin
  if Assigned(Info) and Assigned(FConSettings) then
  begin
    {$IFDEF UNICODE}
    ConSettings.CTRL_CP := DefaultSystemCodePage;
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
      ConSettings.CTRL_CP := ZOSCodePage;
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
          {$IFDEF WITH_WIDEFIELDS}
          ConSettings.CPType := cCP_UTF16;
            {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
            ConSettings.CTRL_CP := DefaultSystemCodePage;
            {$ELSE}
            ConSettings.CTRL_CP := ZOSCodePage;
            {$ENDIF}
          {$ELSE}
          ConSettings.CPType := cCP_UTF8;
          ConSettings.CTRL_CP := 65001;
          {$ENDIF}
          ConSettings.AutoEncode := True;
        end
        else // nothing was found set defaults
        begin
          {$IFDEF LCL}
          ConSettings.CPType := cCP_UTF8;
          ConSettings.CTRL_CP := 65001;
          {$ELSE}
          ConSettings.CPType := cGET_ACP;
            {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
            ConSettings.CTRL_CP := DefaultSystemCodePage;
            {$ELSE}
            ConSettings.CTRL_CP := ZOSCodePage;
            {$ENDIF}
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

{$IFOPT Q+}
  {$DEFINE OverFlowCheckEnabled}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE RangeCheckEnabled}
  {$R-}
{$ENDIF}

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

(*function Hash(const S: RawByteString): Cardinal; //perform the FPC used ELF Hash algorithm -> pretty slow(byte hashed) but tiny
Var
  thehash,g,I : LongWord;
begin
  thehash:=0;
  For I:=1 to Length(S) do { 0 terminated }
  begin
    thehash:=thehash shl 4;
    inc(theHash,Ord(S[i]));
    g:=thehash and $f0000000;;
    if g<>0 then
    begin
      thehash:=thehash xor (g shr 24);
      thehash:=thehash xor g;
    end;
  end;
  If theHash=0 then
     Result := $ffffffff
   else
     Result :=TheHash;
end;*)

{ ported from http://stofl.org/questions/3690608/simple-string-hashing-function}
//perform a MurmurHash2 algorithm by Austin Appleby loads faster (4Byte aligned)
//Changes by EgonHugeist:
//use PAnsiChar instead of S[] to inc the 4Byte blocks -> faster!
//note: we can also use 64Bit versions: http://factgrabber.com/index.php?q=MurmurHash&lcid=xrnmicaJ5olmGUbBZJlkwWah5plkiYaJJpkGgSexJhkm
//function MurmurHash2(const S: RawByteString; const Seed: LongWord=$9747b28c): LongWord;
function Hash(const S: RawByteString): LongWord;
var
  k: LongWord;
  Len: LongWord;
  P, PEnd: PAnsiChar;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
begin
  //The default seed, $9747b28c, is from the original C library
  P := Pointer(S);
  if P = nil then
    Result := $ffffffff
  else
  begin
    Len := {%Result-}PLengthInt(P - StringLenOffSet)^;
    // Initialize the hash to a 'random' value
    Result := $9747b28c xor len;

    // Mix 4 bytes at a time into the hash
    PEnd := P + Len - 4;
    while P < PEnd do
    begin
      k := PLongWord(P)^;

      k := k * m;
      k := k xor (k shr r);
      k := k * m;

      Result := Result * m;
      Result := Result xor k;

      Inc(P, 4);
    end;
    Inc(PEnd, 4);
    Len := PEnd-P;

    {   Handle the last few bytes of the input array
            P: ... $69 $18 $2f
    }
    if len = 3 then
      Result := Result xor (LongWord((P+2)^) shl 16);
    if len >= 2 then
      Result := Result xor (LongWord((P+1)^) shl 8);
    if len >= 1 then
    begin
      Result := Result xor (LongWord(P^));
      Result := Result * m;
    end;

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    Result := Result xor (Result shr 13);
    Result := Result * m;
    Result := Result xor (Result shr 15);

    Result := Result;
  end;
end;

{$IFDEF RangeCheckEnabled}
  {$R+}
{$ENDIF}
{$IFDEF OverFlowCheckEnabled}
  {$OVERFLOWCHECKS ON}
{$ENDIF}

{$IFNDEF FPC}
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
  result := CharInSet(Word(C), CharSet);
end;

function CharInSet(const C: Word; const CharSet: TSysCharSet): Boolean;
begin
  result := (C <= High(Byte)) and (AnsiChar(Byte(C)) in Charset);
end;

{$ENDIF}

{$IFDEF  ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
begin
  Result := UTF8Decode(s);
end;
{$UNDEF ZUTF8ToString}
{$ENDIF}

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
begin
  if ( Len = 0 ) then
    Dest := ''
  else
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len)
    end else
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Dest := '';
      SetLength(Dest, Len);
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end;
    {$ELSE}
      SetString(Dest, Src, Len);
    {$ENDIF}
end;

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String);
begin
  if ( Len = 0 ) then
    Dest := ''
  else
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then
    begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end
    else
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := '';
        SetLength(Dest, Len);
        if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;

//EgonHugeist: my fast ByteToWord shift without encoding maps and/or alloc a ZWideString
procedure ZSetString(Src: PAnsiChar; const Len: LengthInt; var Dest: ZWideString); overload;
var
  PEnd: PAnsiChar;
  PW: PWideChar;
begin
  if ( Len = 0 ) then
    Dest := ''
  else
  begin
    {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
    if (Pointer(Dest{%H-}) = nil) or//empty
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
       (Len <> {%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^) then { length as expected ? }
    {$ELSE}
    if Length(Dest) <> Len then //WideString isn't ref counted
    {$ENDIF}
    SetLength(Dest, Len);
    if Src <> nil then
    begin
      PW := Pointer(Dest);
      PEnd := Src+Len-4;
      while Src < PEnd do //quad conversion per loop
      begin
        PWord(PW)^ := PByte(Src)^;
        PWord(PW+1)^ := PByte(Src+1)^;
        PWord(PW+2)^ := PByte(Src+2)^;
        PWord(PW+3)^ := PByte(Src+3)^;
        Inc(Src, 4);
        Inc(PW, 4);
      end;
      Inc(PEnd, 4);
      while Src < PEnd do
      begin
        PWord(PW)^ := PByte(Src)^;
        Inc(Src);
        Inc(PW);
      end;
    end;
  end;
end;

{$IFDEF WITH_RAWBYTESTRING}

procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString);
begin
  if ( Len = 0 ) then
    Dest := ''
  else
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len)
    end else
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := '';
        SetLength(Dest, Len);
        if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;
{$ENDIF}

{$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
function Min(const A, B: NativeUInt): NativeUInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: NativeUInt): NativeUInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
{$ENDIF}

initialization
  case ConSettingsDummy.CPType of
    cCP_UTF16, cGET_ACP: ConSettingsDummy.CTRL_CP := ZOSCodePage;
    cCP_UTF8: ConSettingsDummy.CTRL_CP := 65001;
  end;
end.
