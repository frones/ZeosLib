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
    function ZAnsiString(const Str: String; const Encoding: TZCharEncoding = ceDefault; const CP:  Word = 0): AnsiString;
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
function UTF8ToString(const s: AnsiString): WideString;
{$IFEND}

implementation

{$IFDEF CHECK_CLIENT_CODE_PAGE}

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
    ceUTF8, ceUTF16:
      Result := UTF8ToString(Ansi);
    //ceUTF16: ;//not done yet, may be interesting for SQLite which supports Execute&Open_16-Functions
    //ceUTF32: //not done yet
  else
    Result := String(Ansi); //Ansi to Wide/Unicode is no Problem!!!
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
  const Encoding: TZCharEncoding = ceDefault; const CP:  Word = 0): AnsiString;
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
    ceUTF8, ceUTF16: Result := AnsiString(UTF8Encode(Str));
    //ceUTF16: ;//not done yet
    //ceUTF32
  else
    { EgonHugeist:
      To Delphi12_UP and (comming) FPC 2.8 Users:
      This function Result an Ansi-String with default OS CodePage
      Possible Problems:
        if you've CodePage 1252 and add some Chinese Letters the CodePage
        turns to 1200 which is able to pick up this 2Byte letters
        So on we've to add an additional Param to my encodingRecord
        like an !SAVE!-Alias, we've to use and the CodePage we must have
        here if it's not UTFx

        BE WARNED!! This is a string-helper Function to handle DataLoss not
        a solution to Enable all chars for your spezified CharacterSet of your
        Connection -> trying this may result an Exception if Chars are not
        supported}

    {$IFDEF DELPHI12_UP} //later for FPC 2.8 too
    if CP <> 0 then SetCodePage({$IFDEF DELPHI14_UP}RawByteString{$ENDIF}(Result), CP, True);
    Result := Copy(UTF8Encode(Str), 1, Length(UTF8Encode(Str)));
    //Check function Utf8ToAnsiEx(const S: UTF8String; const cp : integer): AnsiString;
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
      {$ENDIF}
    {$ENDIF}
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
    while (P<PE) and {$IFDEF DELPHI12_UP} CharInSet(P^, WordDelims) {$ELSE} (P^ in WordDelims){$ENDIF}do
      inc(P);
    if (P<PE) then
      P^:=UpCase(P^);
    while (P<PE) and not {$IFDEF DELPHI12_UP}(CharInSet(P^, WordDelims)){$ELSE} (P^ in WordDelims){$ENDIF} do
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

{$IF not Declared(UTF8ToString)}
function UTF8ToString(const s: AnsiString): WideString;
begin
  Result := UTF8Decode(s);
end;
{$IFEND}


end.

