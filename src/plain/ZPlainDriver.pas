{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainLoader
  {$IFDEF CHECK_CLIENT_CODE_PAGE}, ZCompatibility, Types{$ENDIF};

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    {$IFDEF CHECK_CLIENT_CODE_PAGE}
    {EgonHugeist:
      Why this here? -> No one else then Plaindriver knows which Characterset
      is supported. Here i've made a intervention in dependency of used Compiler..
    }
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage; //Egonhugeist
    {$ENDIF}
    procedure Initialize;
  end;

  {ADDED by EgonHugeist 20-01-2011}
  {** implements a generic base class of a generic plain driver.
   to make the CodePage-handling tranparency for all Plain-Drivers}

  {$IFDEF CHECK_CLIENT_CODE_PAGE}
  TZGenericAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
  private
    FCodePages: array of TZCodePage;
  protected
    procedure LoadCodePages; virtual; abstract;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff; const ZAlias: String = ''); virtual;
    procedure ResetCodePage(const OldID: Integer; const Name: String;
      const ID:  Integer; {may be an ordinal value of predefined Types...}
      Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff; const ZAlias: String = '');
    function GetCompilerSaveCodePageName: String; virtual;
  public
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    procedure Initialize; virtual; abstract;
    destructor Destroy; override;

    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage;
  end;
  {$ENDIF}

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver with TZNativeLibraryLoader-object. }

  { TZAbstractPlainDriver }

  TZAbstractPlainDriver = class({$IFDEF CHECK_CLIENT_CODE_PAGE}
    TZGenericAbstractPlainDriver{$ELSE}TZAbstractObject{$ENDIF}, IZPlainDriver)
  protected
    FLoader: TZNativeLibraryLoader;
    procedure LoadApi; virtual;
  public
    constructor CreateWithLibrary(const LibName : String);
    property Loader: TZNativeLibraryLoader read FLoader;
    function GetProtocol: string; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF}abstract;
    function GetDescription: string; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF} abstract;
    procedure Initialize; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF}
    destructor Destroy; override;
  end;
  {END ADDED by fduenas 15-06-2006}

{$IFDEF CHECK_CLIENT_CODE_PAGE}
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
  zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}
  zCP_KOI8R = 20866; {cskoi8r	20866	Kyrillisch (KOI8-R)}
  zCP_KOI8U = 21866; {KOI8-U is an 8-bit character encoding, designed to cover Ukrainian, which uses the Cyrillic alphabet.}
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets — Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2	east european (ISO), 8-bit single-byte coded graphic character sets — Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}

  zCP_ISO_8859_5 = 28595; {-bit single-byte coded graphic character sets — Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_ISO2022JPSIO = 50222; {Indicates the Internet character set ISO-2022-JP-SIO.}
  zCP_ISO2022JPESC = 50221; {Indicates the Internet character set ISO-2022-JP-ESC.}
  zCP_JAUTODETECT = 50932; {Indicates Japanese auto-detect (50932). }
  zCP_KAUTODETECT = 50949; {Indicates Korean auto-detect (50949).}
  zCP_UTF8 = 65001;
  zCP_UTF7 = 65000;
  zCP_2022kr = 50225; {csiso2022kr	50225	Koreanisch (ISO) }
  zCP_EBC1047 = 1047;	{EBCDIC Codepage 1047}
  zCP_EBC500 = 500;	{EBCDIC Codepage 500}
{$ENDIF}

implementation

uses ZSysUtils{$IFDEF CHECK_CLIENT_CODE_PAGE}, SysUtils{$ENDIF};

{$IFDEF CHECK_CLIENT_CODE_PAGE}

{TZGenericAbstractPlainDriver}

function TZGenericAbstractPlainDriver.GetCompilerSaveCodePageName: String;
begin
  Result := '';
end;

procedure TZGenericAbstractPlainDriver.AddCodePage(const Name: String;
      const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff; const ZAlias: String = '');
begin
  SetLength(FCodePages, Length(FCodePages)+1);
  FCodePages[High(FCodePages)].Name := Name;
  FCodePages[High(FCodePages)].ID := ID;
  FCodePages[High(FCodePages)].Encoding := Encoding;
  FCodePages[High(FCodePages)].CP := CP;
  {$IFDEF FPC}
  if not ( FCodePages[High(FCodePages)].Encoding = ceUTF8 ) then
  begin
    FCodePages[High(FCodePages)].ZAlias := GetCompilerSaveCodePageName;
    FCodePages[High(FCodePages)].IsSupported := False;
  {$ELSE}
  if CP = $ffff then
  begin
    FCodePages[High(FCodePages)].ZAlias := GetCompilerSaveCodePageName;
    FCodePages[High(FCodePages)].IsSupported := False;
  {$ENDIF}
  end
  else
    FCodePages[High(FCodePages)].IsSupported := True;
end;

procedure TZGenericAbstractPlainDriver.ResetCodePage(const OldID: Integer;
      const Name: String; const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff; const ZAlias: String = '');
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if OldID = FCodePages[I].ID then
    begin
      FCodePages[I].ID := ID;
      FCodePages[I].Name := Name;
      FCodePages[I].Encoding := Encoding;
      FCodePages[I].CP := CP;
      FCodePages[I].ZAlias := ZAlias;
      {$IFDEF FPC}
      if not ( FCodePages[I].Encoding = ceUTF8 ) then
      begin
        FCodePages[i].ZAlias := GetCompilerSaveCodePageName;
        FCodePages[i].IsSupported := False;
      {$ELSE}
      if CP = $ffff then
      begin
        FCodePages[i].ZAlias := GetCompilerSaveCodePageName;
        FCodePages[i].IsSupported := False;
      {$ENDIF}
      end
      else
        FCodePages[High(FCodePages)].IsSupported := True;
      Break;
    end;
end;

function TZGenericAbstractPlainDriver.GetSupportedClientCodePages(
  const IgnoreUnsupported: Boolean): TStringDynArray;
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if ( FCodePages[i].IsSupported ) or ( IgnoreUnsupported ) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := FCodePages[i].Name;
    end;
end;

destructor TZGenericAbstractPlainDriver.Destroy;
begin
  SetLength(FCodePages, 0);
  inherited Destroy;
end;

{**
   Checks if the given CharacterSet is Unicode-Save!
   @param ClientCharacterSet the Value wich hast to be compared
   @result True if ClientCharacterSet supports Unicode
}
function TZGenericAbstractPlainDriver.GetClientCodePageInformations(
  const ClientCharacterSet: String): PZCodePage;
var
  I: Integer;
begin
  {now check for PlainDriver-Informations...}
  for i := Low(FCodePages) to high(FCodePages) do
    if UpperCase(FCodePages[i].Name) = UpperCase(ClientCharacterSet) then
    begin
      Result := @FCodePages[i];
      Exit;
    end;
  Result := GetClientCodePageInformations(GetCompilerSaveCodePageName); //recalls em selve -> switch to supported
end;

{$ENDIF}

{ TZAbstractPlainDriver }

{ADDED by fduenas 15-06-2006}
{**
  Gets the clients's full version number. Initially this should be 0.
  @return the clients's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    client_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetClientVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Client's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetClientVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetClientVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}
{**
  Gets the servers's full version number. Initially this should be 0.
  @return the server's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    server_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetServerVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Server's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetServerVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetServerVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}

procedure TZAbstractPlainDriver.LoadApi;
begin

end;

constructor TZAbstractPlainDriver.CreateWithLibrary(const LibName: String);
begin
  Inherited Create;
  Loader.ClearLocations;
  Loader.AddLocation(LibName);
end;

procedure TZAbstractPlainDriver.Initialize;
begin
  If Assigned(Loader) and not Loader.Loaded then
    If Loader.LoadNativeLibrary then
      LoadApi;
end;

destructor TZAbstractPlainDriver.Destroy;
begin
  FLoader.Free;
  inherited Destroy;
end;

end.

