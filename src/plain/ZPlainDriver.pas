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

uses ZClasses, ZPlainLoader, ZCompatibility, Types;

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    {EgonHugeist:
      Why this here? -> No one else then Plaindriver knows which Characterset
      is supported. Here i've made a intervention in dependency of used Compiler..
    }
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage; //Egonhugeist
    procedure Initialize;
  end;

  {ADDED by EgonHugeist 20-01-2011}
  {** implements a generic base class of a generic plain driver.
   to make the CodePage-handling tranparency for all Plain-Drivers}

  TZGenericAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
  private
    FCodePages: array of TZCodePage;
  protected
    procedure LoadCodePages; virtual; abstract;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi;
      {$IFDEF WITH_CHAR_CONTROL}const CP: Word = $ffff; {$ENDIF}
      const ZAlias: String = ''); virtual;
    procedure ResetCodePage(const OldID: Integer; const Name: String;
      const ID:  Integer; {may be an ordinal value of predefined Types...}
      Encoding: TZCharEncoding = ceAnsi;
      {$IFDEF WITH_CHAR_CONTROL}const CP: Word = $ffff; {$ENDIF}
      const ZAlias: String = '');
    function GetCompilerSaveCodePageName: String; virtual;
  public
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    procedure Initialize; virtual; abstract;
    destructor Destroy; override;

    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage;
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver with TZNativeLibraryLoader-object. }

  { TZAbstractPlainDriver }

  TZAbstractPlainDriver = class(TZGenericAbstractPlainDriver, IZPlainDriver)
  protected
    FLoader: TZNativeLibraryLoader;
    procedure LoadApi; virtual;
  public
    constructor CreateWithLibrary(const LibName : String);
    property Loader: TZNativeLibraryLoader read FLoader;
    function GetProtocol: string; override; abstract;
    function GetDescription: string; override; abstract;
    procedure Initialize; override;
    destructor Destroy; override;
  end;
  {END ADDED by fduenas 15-06-2006}


{$IFDEF WITH_CHAR_CONTROL}
const
  zCP_ACP = 0; {ASCII US}
  zCP_EBC037 = 37; {IBM EBCDIC US-Canada}
  zCP_EBC273 = 273; {EBCDIC Code Page 273/1 8-bit Austrian German}
  zCP_EBC277 = 277; {EBCDIC Code Page 277/1 8-bit Danish}
  zCP_EBC278 = 278; {EBCDIC Code Page 278/1 8-bit Swedish}
  zCP_EBC280 = 280; {EBCDIC Code Page 280/1 8-bit Italian}
  zCP_EBC284 = 284; {EBCDIC Code Page 284 8-bit Latin American/Spanish}

  zCP_DOS437 = 437; {IBM437/MS-DOS odepage 437 (US)}
  zCP_DOS500 = 500; {IBM EBCDIC International}
  zCP_DOS708 = 708; {Arabic (ASMO 708)}
  zCP_DOS709 = 709; {Arabic (ASMO-449+, BCON V4)}
  zCP_DOS710 = 710; {Arabic - Transparent Arabic}
  zCP_DOS720 = 720; {Arabic (Transparent ASMO); Arabic (DOS)}
  zCP_DOS737 = 737; {OEM Greek (formerly 437G); Greek (DOS)}
  zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
  zCP_DOS850 = 850;	{MS-DOS Codepage 850 (Multilingual Latin 1)}
  zCP_DOS851 = 851; {MS-DOS Codepage 851 (Greece) - obsolete}
  zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
  zCP_DOS853 = 853;	{MS-DOS Codepage 853 (Multilingual Latin 3)}
  zCP_DOS855 = 855;	{MS-DOS Codepage 855 (Russia) - obsolete}
  zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
  zCP_DOS858 = 858; {MS-DOS Codepage 858  {Latin I + Euro symbol}
  zCP_DOS895 = 895; {MS-DOS Codepage 895 (Kamenicky CS)}
  zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
  zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
  zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
  zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
  zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
  zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
  zCP_DOS866 = 866; {ibm866	866	Cyrl (DOS)}
  zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}
  zCP_DOS870 = 870; {IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2}
  zCP_DOS874 = 874; {ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)}
  zCP_EBC875 = 875;	{EBCDIC Codepage 875 (Greek)}
  zCP_EBC924 = 924; {Latin 9 EBCDIC 924}
  zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
  zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
  zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
  zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}
  zCP_IBM1026 = 1026; {EBCDIC Code Page 1026 8-bit Turkish}
  zCP_IBM01047 = 1047; {IBM EBCDIC Latin 1/Open System}
  zCP_IBM01140 = 1140; {IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro)}
  zCP_IBM01141 = 1141; {IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro)}
  zCP_IBM01142 = 1142; {IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro)}
  zCP_IBM01143 = 1143; {IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro)}
  zCP_IBM01144 = 1144; {IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro)}
  zCP_IBM01145 = 1145; {IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro)}
  zCP_IBM01146 = 1146; {IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro)}
  zCP_IBM01147 = 1147; {IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro)}
  zCP_IBM01148 = 1148; {IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro)}
  zCP_IBM01149 = 1149; {IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro)}

  zCP_UTF16 = 1200; {utf-16; Indicates the Unicode character set, Windows code page 1200}
  zCP_UTF16BE = 1201; {Unicode UTF-16, big endian byte order; available only to managed applications}
  zCP_WIN1250 = 1250; {Microsoft Windows Codepage 1250 (East European)}
  zCP_WIN1251 = 1251; {Microsoft Windows Codepage 1251 (Cyrl)}
  zCP_WIN1252 = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
  zCP_WIN1253 = 1253; {Microsoft Windows Codepage 1253 (Greek)}
  zCP_WIN1254 = 1254; {Microsoft Windows Codepage 1254 (Turk)}
  zCP_WIN1255 = 1255; {Microsoft Windows Codepage 1255 (Hebrew)}
  cCP_WIN1256 = 1256; {Microsoft Windows Codepage 1256 (Arab)}
  zCP_WIN1257 = 1257; {Microsoft Windows Codepage 1257 (BaltRim)}
  zCP_WIN1258 = 1258; {Microsoft Windows Codepage 1258 (Viet), TCVN-5712}
  ZCP_JOHAB = 1361; {Korean (Johab)}
  zCP_KOREAN = 2022; {iso-2022-kr	50225	Koreanisch (ISO)}

  zCP_macintosh = 10000; {MAC Roman; Western European (Mac)}
  zCP_x_mac_japanese = 10001; {Japanese (Mac)}
  zCP_x_mac_chinesetrad = 10002; {MAC Traditional Chinese (Big5); Chinese Traditional (Mac)}
  zCP_x_mac_korean = 10003; {Korean (Mac)}
  zCP_x_mac_arabic = 10004;	{Arabic (Mac)}
  zCP_x_mac_hebrew = 10005; {Hebrew (Mac)}
  zCP_x_mac_greek = 10006;	{Greek (Mac)}
  zCP_x_mac_cyrillic = 10007; {Cyrillic (Mac)}
  zCP_x_mac_chinesesimp = 10008; {MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac)}
  zCP_x_mac_romanian = 10010; {Romanian (Mac)}
  zCP_x_mac_ukrainian = 10017; {Ukrainian (Mac)}
  zCP_x_mac_thai = 10021; {Thai (Mac)}
  zCP_x_mac_ce = 10029; {MAC Latin 2; Central European (Mac)}
  zCP_x_mac_icelandic = 10079;	{Icelandic (Mac)}
  zCP_x_mac_turkish = 10081;	{Turkish (Mac)}
  zCP_x_mac_croatian = 10082; {Croatian (Mac)}
  zCP_utf32 = 12000; {Unicode UTF-32, little endian byte order; available only to managed applications}
  zCP_utf32BE = 12001; {Unicode UTF-32, big endian byte order; available only to managed applications}

  zCP_x_Chinese_CNS = 20000; {CNS Taiwan; Chinese Traditional (CNS)}
  zCP_x_cp20001 = 20001; {TCA Taiwan}
  zCP_x_Chinese_Eten = 20002; {Eten Taiwan; Chinese Traditional (Eten)}
  zCP_x_cp20003 = 20003; {IBM5550 Taiwan}
  zCP_x_cp20004 = 20004; {TeleText Taiwan}
  zCP_x_cp20005 = 20005; {Wang Taiwan}
  zCP_x_IA5 = 20105; {IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5)}
  zCP_x_IA5_German = 20106; {IA5 German (7-bit)}
  zCP_x_IA5_Swedish = 20107; {IA5 Swedish (7-bit)}
  zCP_x_IA5_Norwegian = 20108; {IA5 Norwegian (7-bit)}
  zCP_us_ascii = 20127; {US-ASCII (7-bit)}
  zCP_x_cp20261 = 20261; {T.61}
  zCP_x_cp20269 = 20269; {ISO 6937 Non-Spacing Accent}
  zCP_IBM273 = 20273; {IBM EBCDIC Germany}
  zCP_IBM277 = 20277; {IBM EBCDIC Denmark-Norway}
  zCP_IBM278 = 20278; {IBM EBCDIC Finland-Sweden}
  zCP_IBM280 = 20280; {IBM EBCDIC Italy}
  zCP_IBM284 = 20284; {IBM EBCDIC Latin America-Spain}
  zCP_IBM285 = 20285; {IBM EBCDIC United Kingdom}
  zCP_IBM290 = 20290; {IBM EBCDIC Japanese Katakana Extended}
  zCP_IBM297 = 20297; {IBM EBCDIC France}
  zCP_IBM420 = 20420; {IBM EBCDIC Arabic}
  zCP_IBM423 = 20423; {IBM EBCDIC Greek}
  zCP_IBM424 = 20424; {IBM EBCDIC Hebrew}
  zCP_x_EBCDIC_KoreanExtended = 20833; {IBM EBCDIC Korean Extended}
  zCP_IBM_Thai = 20838; {IBM EBCDIC Thai}
  zCP_KOI8R = 20866; {cskoi8r	20866	Cyrillic (KOI8-R)}
  zCP_IBM871 = 20871; {IBM EBCDIC Icelandic}
  zCP_IBM880 = 20880; {IBM EBCDIC Cyrillic Russian}
  zCP_IBM905 = 20905; {IBM EBCDIC Turkish}
  zCP_IBM00924 = 20924; {IBM EBCDIC Latin 1/Open System (1047 + Euro symbol)}
  zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}
  zCP_x_cp20936 = 20936;	{Simplified Chinese (GB2312); Chinese Simplified (GB2312-80)}
  zCP_x_cp20949 = 20949;	{Korean Wansung}
  zCP_cp1025 = 21025;	{IBM EBCDIC Cyrillic Serbian-Bulgarian}
  //21027 (deprecated)}}
  zCP_KOI8U = 21866; {KOI8-U is an 8-bit character encoding, designed to cover Ukrainian, which uses the Cyrillic alphabet.}
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets — Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2	east european (ISO), 8-bit single-byte coded graphic character sets — Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L3_ISO_8859_3 = 28593; {ISO 8859-3 Latin 3}
  zCP_L4_ISO_8859_4 = 28594; {ISO 8859-4 Baltic}
  zCP_L5_ISO_8859_5 = 28595; {-bit single-byte coded graphic character sets — Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L6_ISO_8859_6 = 28596; {ISO 8859-6 Arabic}
  zCP_L7_ISO_8859_7 = 28597; {ISO 8859-7 Greek}
  zCP_L8_ISO_8859_8 = 28598; {ISO 8859-8 Hebrew; Hebrew (ISO-Visual)}
  zCP_ISO_8859_9 = 28599; {ISO 8859-9 Turkish}
  zCP_ISO_8859_13 = 28603; {ISO 8859-13 Estonian}
  zCP_L9_ISO_8859_15 = 28605; {ISO 8859-15 Latin 9}
  zCP_x_Europa = 29001; {Europa 3}
  zCP_iso_8859_8_i = 38598;	{ISO 8859-8 Hebrew; Hebrew (ISO-Logical)}

  zCP_iso_2022_jp = 50220;	{ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)}
  zCP_csISO2022JP = 50221;	{ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
  zCP_x_iso_2022_jp = 50222;	{ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI)}
  zCP_iso_2022_kr = 50225; {ISO 2022 Korean}
  zCP_x_cp50227 = 50227;	{ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022)}
  zCP_EUC_TC_ISO220 = 50229; {ISO 2022 Traditional Chinese}
  zCP_EBCDIC_euc_jpe = 50930;	{EBCDIC Japanese (Katakana) Extended}
  zCP_EBCDIC_euc_jp = 50931; {EBCDIC US-Canada and Japanese}
  zCP_euc_jp_auto = 50932; {EUC Japanese, Indicates Japanese auto-detect (50932). }
  zCP_EBCDIC_euc_kr = 50933; {EBCDIC Korean Extended and Korean}
  zCP_EBCDIC_euc_cn = 50935; {EBCDIC Simplified Chinese Extended and Simplified Chinese}
  zCP_EBCDIC_euc_sc = 50936; {EBCDIC Simplified Chinese}
  zCP_EBCDIC_USC_TC = 50937; {EBCDIC US-Canada and Traditional Chinese}
  zCP_euc_cn_auto = 50939; {EBCDIC Japanese (Latin) Extended and Japanese}
  zCP_euc_kr_auto = 50949; {EUC Korean, Indicates Korean auto-detect (50949).}
  zCP_euc_JP_win = 51932; {EUC Japanese}
  zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
  zCP_euc_kr = 51949; {EUC Korean}
  zCP_euc_tc = 51950; {EUC Traditional Chinese}
  zCP_hz_gb_2312 = 52936; {HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ)}
  zCP_GB18030 = 54936;	{Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
  zCP_x_iscii_de = 57002;	{ISCII Devanagari}
  zCP_x_iscii_be = 57003; {ISCII Bengali}
  zCP_x_iscii_ta = 57004; {ISCII Tamil}
  zCP_x_iscii_te = 57005; {ISCII Telugu}
  zCP_x_iscii_as = 57006; {ISCII Assamese}
  zCP_x_iscii_or = 57007; {ISCII Oriya}
  zCP_x_iscii_ka = 57008; {ISCII Kannada}
  zCP_x_iscii_ma = 57009; {ISCII Malayalam}
  zCP_x_iscii_gu = 57010; {ISCII Gujarati}
  zCP_x_iscii_pa = 57011; {ISCII Punjabi}
  zCP_UTF8 = 65001;
  zCP_UTF7 = 65000;
{$ENDIF}


implementation

uses ZSysUtils, SysUtils;


{TZGenericAbstractPlainDriver}

function TZGenericAbstractPlainDriver.GetCompilerSaveCodePageName: String;
begin
  Result := '';
end;

procedure TZGenericAbstractPlainDriver.AddCodePage(const Name: String;
      const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      {$IFDEF WITH_CHAR_CONTROL}const CP: Word = $ffff; {$ENDIF}
      const ZAlias: String = '');
begin
  SetLength(FCodePages, Length(FCodePages)+1);
  FCodePages[High(FCodePages)].Name := Name;
  FCodePages[High(FCodePages)].ID := ID;
  FCodePages[High(FCodePages)].Encoding := Encoding;
  {$IFDEF WITH_CHAR_CONTROL} FCodePages[High(FCodePages)].CP := CP; {$ENDIF}

  {$IFDEF FPC}
    if not ( FCodePages[High(FCodePages)].Encoding = ceUTF8 ) then
    begin
      FCodePages[High(FCodePages)].ZAlias := GetCompilerSaveCodePageName;
      FCodePages[High(FCodePages)].IsSupported := False;
    end
    else
      FCodePages[High(FCodePages)].IsSupported := True;
  {$ELSE}
    {$IFDEF WITH_CHAR_CONTROL}
      if CP = $ffff then
      begin
        FCodePages[High(FCodePages)].ZAlias := GetCompilerSaveCodePageName;
        FCodePages[High(FCodePages)].IsSupported := False;
      end
      else
    {$ENDIF}
        FCodePages[High(FCodePages)].IsSupported := True;
  {$ENDIF}
end;

procedure TZGenericAbstractPlainDriver.ResetCodePage(const OldID: Integer;
      const Name: String; const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      {$IFDEF WITH_CHAR_CONTROL}const CP: Word = $ffff; {$ENDIF}
      const ZAlias: String = '');
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if OldID = FCodePages[I].ID then
    begin
      FCodePages[I].ID := ID;
      FCodePages[I].Name := Name;
      FCodePages[I].Encoding := Encoding;
      {$IFDEF WITH_CHAR_CONTROL}FCodePages[I].CP := CP;{$ENDIF}
      FCodePages[I].ZAlias := ZAlias;

      {$IFDEF FPC}
        if not ( FCodePages[I].Encoding = ceUTF8 ) then
        begin
          FCodePages[i].ZAlias := GetCompilerSaveCodePageName;
          FCodePages[i].IsSupported := False;
        end
        else
          FCodePages[High(FCodePages)].IsSupported := True;
      {$ELSE}
        {$IFDEF WITH_CHAR_CONTROL}
        if CP = $ffff then
        begin
          FCodePages[i].ZAlias := GetCompilerSaveCodePageName;
          FCodePages[i].IsSupported := False;
        end
        else
        {$ENDIF}
          FCodePages[High(FCodePages)].IsSupported := True;
      {$ENDIF}
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
  {$IFDEF FPC}
  GetClientCodePageInformations(GetCompilerSaveCodePageName); //recalls em selve -> switch to supported (UTF8)
  {$ELSE}
  Result := @ClientCodePageDummy;
  {$ENDIF}
end;

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

