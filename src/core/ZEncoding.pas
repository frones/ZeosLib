{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZEncoding;

interface

{$I ZCore.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} Math,
  {$IFDEF WITH_LCONVENCODING}
  {$MACRO ON}
   LCLVersion, LConvEncoding,
  {$ENDIF}
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZCompatibility;

const
  //zCP_ACP = 0; {ASCII US}
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
  zCP_DOS852 = 852; {ibm852 852 east european(DOS)}
  zCP_DOS853 = 853;	{MS-DOS Codepage 853 (Multilingual Latin 3)}
  zCP_DOS855 = 855;	{MS-DOS Codepage 855 (Russia) - obsolete}
  zCP_DOS856 = 856;
  zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
  zCP_DOS858 = 858; {MS-DOS Codepage 858  Latin I + Euro symbol}
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
  zCP_MSWIN921 = 921;
  zCP_MSWIN923 = 923;
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
  zCP_KOREAN = 2022; {iso-2022-kr	50225	Korean (ISO)}

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
  zCP_IBM_Thai = 20838; {IBM EBCDIC Thai / TIS-620}
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
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2	east european (ISO), 8-bit single-byte coded graphic character sets - Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L3_ISO_8859_3 = 28593; {ISO 8859-3 Latin 3}
  zCP_L4_ISO_8859_4 = 28594; {ISO 8859-4 Baltic}
  zCP_L5_ISO_8859_5 = 28595; {8bit single-byte coded graphic character sets - Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L6_ISO_8859_6 = 28596; {ISO 8859-6 Arabic}
  zCP_L7_ISO_8859_7 = 28597; {ISO 8859-7 Greek}
  zCP_L8_ISO_8859_8 = 28598; {ISO 8859-8 Hebrew; Hebrew (ISO-Visual)}
  zCP_L5_ISO_8859_9 = 28599; {ISO 8859-9 Turkish}
  zCP_L6_ISO_8859_10 = 28600; { ISO 8859-10, ECMA 144 	Nordic }
  zCP_L7_ISO_8859_13 = 28603; {ISO 8859-13 Estonian}
  zCP_L8_ISO_8859_14 = 28604; { ISO 8859-14 	Celtic }
  zCP_L9_ISO_8859_15 = 28605; {ISO 8859-15 Latin 9}
  zCP_L10_ISO_8859_16 = 28606;  { ISO 8859-16, ASRO SR 14111 	Romanian }
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
  zCP_NONE = $ffff;

{$IFDEF WITH_LCONVENCODING}
const
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

function IsLConvEncodingCodePage(const CP: Word): Boolean;
procedure SetConvertFunctions(const CTRL_CP, DB_CP: Word;
  out PlainConvert, DbcConvert: TConvertEncodingFunction); overload;
{$ELSE}

function StringToAnsiEx(const s: String; const {$IFNDEF UNICODE}FromCP,{$ENDIF} ToCP: Word): RawByteString; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function AnsiToStringEx(const s: RawByteString; const FromCP{$IFNDEF UNICODE}, ToCP{$ENDIF}: Word): String; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}

function ZRawToUnicode(const S: RawByteString; const CP: Word): ZWideString;
function ZUnicodeToRaw(const US: ZWideString; CP: Word): RawByteString;

{converter functions for the String-types}
function ZConvertAnsiToRaw(const Src: AnsiString; const RawCP: Word): RawByteString;
function ZConvertRawToAnsi(const Src: RawByteString; const RawCP: Word): AnsiString;
function ZConvertAnsiToUTF8(const Src: AnsiString): UTF8String;
function ZConvertUTF8ToAnsi(const Src: UTF8String): AnsiString;
function ZConvertRawToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRawToString(const Src: RawByteString; const RawCP, StringCP: Word): String;
function ZConvertStringToRaw(const Src: String; const StringCP, RawCP: Word): RawByteString;
function ZConvertStringToRawWithAutoEncode(const Src: String; const StringCP, RawCP: Word): RawByteString;
function ZConvertUTF8ToString(const Src: UTF8String; const StringCP: Word): String;
function ZConvertStringToUTF8(const Src: String; const StringCP: Word): UTF8String;
function ZConvertStringToUTF8WithAutoEncode(const Src: String; const StringCP: Word): UTF8String;
function ZConvertStringToAnsi(const Src: String; const StringCP: Word): AnsiString;
function ZConvertStringToAnsiWithAutoEncode(const Src: String; const StringCP: Word): AnsiString;
function ZConvertAnsiToString(const Src: AnsiString; const StringCP: Word): String;
function ZConvertUnicodeToString(const Src: ZWideString; const StringCP: Word): String;
function ZConvertUnicodeToString_CPUTF8(const Src: ZWideString; const StringCP: Word): String;
function ZConvertStringToUnicode(const Src: String; const StringCP: Word): ZWideString;
function ZConvertString_CPUTF8ToUnicode(const Src: String; const StringCP: Word): ZWideString;
function ZConvertStringToUnicodeWithAutoEncode(const Src: String; const StringCP: Word): ZWideString;
{move functions for the String types}
function ZMoveAnsiToRaw(const Src: AnsiString; const RawCP: Word): RawByteString;
function ZMoveRawToAnsi(const Src: RawByteString; const RawCP: Word): AnsiString;
function ZMoveAnsiToUTF8(const Src: AnsiString): UTF8String;
function ZMoveUTF8ToAnsi(const Src: UTF8String): AnsiString;
function ZMoveRawToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZMoveUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
function ZMoveStringToAnsi(Const Src: String; const StringCP: Word): AnsiString;
function ZMoveAnsiToString(const Src: AnsiString; const StringCP: Word): String;
function ZMoveRawToString(const Src: RawByteString; const RawCP, StringCP: Word): String;
function ZMoveStringToRaw(const Src: String; const StringCP, RawCP: Word): RawByteString;
function ZMoveUTF8ToString(const Src: UTF8String; StringCP: Word): String;
function ZMoveStringToUTF8(const Src: String; const StringCP: Word): UTF8String;

function ZUnknownRawToUnicode(const S: RawByteString; const CP: Word): ZWideString;
function ZUnknownRawToUnicodeWithAutoEncode(const S: RawByteString;
  const CP: Word): ZWideString;
function ZUnicodeToUnknownRaw(const US: ZWideString; CP: Word): RawByteString;

{**
  Get the current system codepage of AnsiString
  @return current system codepage of AnsiString
}
function ZDefaultSystemCodePage: Word;

{**
  Is the codepage equal or compatible?
  @param CP1 word the first codepage to compare
  @param CP2 word the second codepage to compare
  @returns Boolean True if codepage is equal or compatible
}
function ZCompatibleCodePages(const CP1, CP2: Word): Boolean; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Set the string-types conversion funtion in relation to the Connection-Settings.
  The Results should be as optimal as possible to speed up the behavior
  @param ConSettings a Pointer to the ConnectionSetting
}
procedure SetConvertFunctions(ConSettings: PZConSettings); {$IFDEF WITH_LCONVENCODING}overload;{$ENDIF}

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  WasDecoded: Boolean; ConSettings: PZConSettings): RawByteString; overload;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString; overload;

function GetValidatedAnsiString(const Uni: ZWideString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString; overload;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Buffer the pointer to the Data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

function GetValidatedUnicodeStream(const Ansi: RawByteString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

implementation

uses SysUtils, Types {$IFDEF WITH_WIDESTRUTILS},WideStrUtils{$ENDIF},
  ZSysUtils{$IFDEF WITH_STRLEN_DEPRECATED}, AnsiStrings{$ENDIF};

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZUnknownRawToUnicode(const S: RawByteString;
  const CP: Word): ZWideString;
begin
  Result := ZWideString(S);
end;

function ZUnknownRawToUnicodeWithAutoEncode(const S: RawByteString;
  const CP: Word): ZWideString;
begin
  case DetectUTF8Encoding(S) of
    etUSASCII, etUTF8: Result := UTF8ToString(S);
    else
      Result := ZWideString(S);
  end;
end;

function ZUnicodeToUnknownRaw(const US: ZWideString; CP: Word):
  RawByteString;
begin
  Result := RawByteString(US);
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

function ZRawToUnicode(const S: RawByteString; const CP: Word): ZWideString;
{$IFDEF WITH_LCONVENCODING}
begin
  case CP of
    28591: //ISO_8859_1
      Result := UTF8Decode(ISO_8859_1ToUTF8(PAnsiChar(S)));
    28592:  //ISO_8859_2
      Result := UTF8Decode(ISO_8859_2ToUTF8(PAnsiChar(S)));
    1250: //WIN1250
      Result := UTF8Decode(CP1250ToUTF8(PAnsiChar(S)));
    1251: //WIN1251
      Result := UTF8Decode(CP1251ToUTF8(PAnsiChar(S)));
    1252: //WIN1252
      Result := UTF8Decode(CP1252ToUTF8(PAnsiChar(S)));
    1253: //WIN1253
      Result := UTF8Decode(CP1253ToUTF8(PAnsiChar(S)));
    1254: //WIN1254
      Result := UTF8Decode(CP1254ToUTF8(PAnsiChar(S)));
    1255: //WIN1255
      Result := UTF8Decode(CP1255ToUTF8(PAnsiChar(S)));
    1256: //WIN1256
      Result := UTF8Decode(CP1256ToUTF8(PAnsiChar(S)));
    1257: //WIN1257
      Result := UTF8Decode(CP1257ToUTF8(PAnsiChar(S)));
    1258: //WIN1258
      Result := UTF8Decode(CP1258ToUTF8(PAnsiChar(S)));
    437: //CP437
      Result := UTF8Decode(CP437ToUTF8(PAnsiChar(S)));
    850: //CP850
      Result := UTF8Decode(CP850ToUTF8(PAnsiChar(S)));
    {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
    852: //CP852
      Result := UTF8Decode(CP852ToUTF8(PAnsiChar(S)));
    {$ENDIF}
    866: //CP866
      Result := UTF8Decode(CP866ToUTF8(PAnsiChar(S)));
    874: //CP874
      Result := UTF8Decode(CP874ToUTF8(PAnsiChar(S)));
    20866: //KOI8 (Russian)
      Result := UTF8Decode(KOI8ToUTF8(PAnsiChar(S)));
    65001: //UTF8
      Result := UTF8Decode(PAnsiChar(s));
    else
      Result := ZWideString(S); //random success!
  end;
end;
{$ELSE}
{$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
var
  {$IFDEF WITH_UNICODEFROMLOCALECHARS}
  wlen, ulen: Integer;
  {$ELSE}
  l: Integer;
  US: WideString;
  {$ENDIF}
{$IFEND}
begin
  Result := '';
  if CP = zCP_NONE  then
    Result := ZUnknownRawToUnicode(s, CP)
  else
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    begin
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      ulen := Length(s);
      wlen := UnicodeFromLocaleChars(cp, 0, PAnsiChar(S), ulen, NIL, 0); // wlen is the number of UCS2 without NULL terminater.
      if wlen = 0 then exit;
      SetLength(result, wlen);
      UnicodeFromLocaleChars(cp, 0, PAnsiChar(S), ulen, PWideChar(Result), wlen);
      {$ELSE}
      l := MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]), - 1, nil, 0); //Checkout the Result-Lengh
      if l = 0 then Exit;
      SetLength(US, l - 1); //Set Result-Length
      MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]),
        - 1, PWideChar(@US[1]), l - 1); //Convert Ansi to Wide with supported Chars
      Result := US;
      {$ENDIF}
    end;
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Ansi2WideMoveProc(PAnsiChar(s), CP, Result, Length(s));
      {$ELSE}
      if ZCompatibleCodePages(CP, zCP_UTF8) then
        Result := UTF8Encode(s)
      else
        Result := ZWideString(s);
      {$ENDIF}
    {$IFEND}
end;
{$ENDIF}

function ZUnicodeToRaw(const US: ZWideString; CP: Word): RawByteString;
{$IFDEF WITH_LCONVENCODING}
begin
  case CP of
    28591: //ISO_8859_1
      Result := UTF8ToISO_8859_1(UTF8Encode(US));
    28592:  //ISO_8859_2
      Result := UTF8ToISO_8859_2(UTF8Encode(US));
    1250: //WIN1250
      Result := UTF8ToCP1250(UTF8Encode(US));
    1251: //WIN1251
      Result := UTF8ToCP1251(UTF8Encode(US));
    1252: //WIN1252
      Result := UTF8ToCP1252(UTF8Encode(US));
    1253: //WIN1253
      Result := UTF8ToCP1253(UTF8Encode(US));
    1254: //WIN1254
      Result := UTF8ToCP1254(UTF8Encode(US));
    1255: //WIN1255
      Result := UTF8ToCP1255(UTF8Encode(US));
    1256: //WIN1256
      Result := UTF8ToCP1256(UTF8Encode(US));
    1257: //WIN1257
      Result := UTF8ToCP1257(UTF8Encode(US));
    1258: //WIN1258
      Result := UTF8ToCP1258(UTF8Encode(US));
    437: //CP437
      Result := UTF8ToCP437(UTF8Encode(US));
    850: //CP850
      Result := UTF8ToCP850(UTF8Encode(US));
    {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
    852: //CP852
      Result := UTF8ToCP852(UTF8Encode(US));
    {$ENDIF}
    866: //CP866
      Result := UTF8ToCP866(UTF8Encode(US));
    874: //CP874
      Result := UTF8ToCP874(UTF8Encode(US));
    20866: //KOI8 (Russian)
      Result := UTF8ToKOI8(UTF8Encode(US));
    65001: //UTF8
      Result := UTF8Encode(US);
    else
      Result := RawByteString(US); //random success!
  end;
end;
{$ELSE}
{$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
var
  {$IFDEF WITH_UNICODEFROMLOCALECHARS}
  wlen, ulen: Integer;
  {$ELSE}
  l: Integer;
  WS: WideString;
  {$ENDIF}
{$IFEND}
begin
  Result := '';
  if CP = zCP_NONE then
    Result := RawByteString(US) //random success
  else
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    begin
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      wlen := Length(US);
      ulen := LocaleCharsFromUnicode(CP, 0, PWideChar(US), wlen, NIL, 0, NIL, NIL);
      setlength(Result, ulen);
      LocaleCharsFromUnicode(CP, 0, PWideChar(US), wlen, PAnsiChar(Result), ulen, NIL, NIL);
      {$ELSE}
      WS := US;
      l := WideCharToMultiByte(CP,0, @WS[1], - 1, nil, 0, nil, nil); //Checkout the result length
      if l = 0 then Exit;
      SetLength(Result, l - 1); //SetResult Length
      WideCharToMultiByte(CP,0, @WS[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
      {$ENDIF}
    end;
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Unicode2AnsiMoveProc(PWideChar(US), Result, CP, Length(US));
      {$ELSE}
      if ZCompatibleCodePages(CP, zCP_UTF8) then
        Result := UTF8Encode(US)
      else
        Result := RawByteString(US); //random success
      {$ENDIF}
    {$IFEND}
end;
{$ENDIF}

{$IFNDEF WITH_LCONVENCODING}
function AnsiToStringEx(const s: RawByteString;
  const FromCP{$IFNDEF UNICODE}, ToCP{$ENDIF}: Word): String;
begin
  if s = '' then
    Result := ''
  else
    if ( FromCP = zCP_NONE ) {$IFNDEF UNICODE} or ( FromCP = ToCP ){$ENDIF}then
      Result := String(s)
    else
      {$IFDEF UNICODE}
      if FromCP = zCP_UTF8 then
        result := UTF8ToString(s)
      else
        Result := ZRawToUnicode(s, FromCP);
      {$ELSE} //Ansi-Compiler
        Result := ZUnicodeToRaw(ZRawToUnicode(s, FromCP), ToCP);
      {$ENDIF}
end;

function StringToAnsiEx(const s: String; const {$IFNDEF UNICODE}FromCP, {$ENDIF} ToCP: Word): RawByteString;
begin
  if s = '' then
    Result := ''
  else
    if ( ToCP = zCP_NONE ) {$IFNDEF UNICODE} or ( FromCP = ToCP ){$ENDIF}then
      Result := RawByteString(s)
    else
      {$IFDEF UNICODE}
      if ToCP = zCP_UTF8 then
        result := UTF8Encode(s)
      else
        Result := ZUnicodeToRaw(s, ToCP);
      {$ELSE} //Ansi-Compiler
        Result := ZUnicodeToRaw(ZRawToUnicode(s, FromCP), ToCP);
      {$ENDIF}
end;

{$ELSE}

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

function NoConvert(const s: string): string;
begin
  Result := s;
end;

procedure SetConvertFunctions(const CTRL_CP, DB_CP: Word;
  out PlainConvert, DbcConvert: TConvertEncodingFunction);
begin
  if CTRL_CP = DB_CP then
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
      {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
      852: //CP852
        begin
          DbcConvert := @CP852ToUTF8;
          PlainConvert := @UTF8ToCP852;
        end;
      {$ENDIF}
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
{$ENDIF}

function ZDefaultSystemCodePage: Word;
begin
  {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
  Result := Word(DefaultSystemCodePage);
  {$ELSE}
    {$IFDEF MSWINDOWS}
    Result := GetACP; //available for Windows and WinCE
    {$ELSE}
    Result := zCP_UTF8; //how to determine the current OS CP?
    {$ENDIF}
  {$ENDIF}
end;

{**
  Is the codepage equal or compatible?
  @param CP1 word the first codepage to compare
  @param CP2 word the second codepage to compare
  @returns Boolean True if codepage is equal or compatible
}
function ZCompatibleCodePages(const CP1, CP2: Word): Boolean;
begin
  Result := (CP1 = CP2) or (CP1 = zCP_us_ascii) or (CP2 = zCP_us_ascii);
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function TestEncoding(const Bytes: TByteDynArray; const Size: Cardinal;
  const ConSettings: PZConSettings): TZCharEncoding;
begin
  Result := ceDefault;
  {EgonHugeist:
    Step one: Findout, wat's comming in! To avoid User-Bugs as good as possible
      it is possible that a PAnsiChar OR a PWideChar was written into
      the Stream!!!  And these chars could be trunced with changing the
      Stream.Size.
      I know this can lead to pain with two byte ansi chars, but what else can i do?
    step two: detect the encoding }

  if ( {$IFDEF WITH_STRLEN_DEPRECATED}AnsiStrings.{$ENDIF}StrLen(PAnsiChar(Bytes)) < Size ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16
  else
    if ConSettings.AutoEncode then
      case DetectUTF8Encoding(PAnsichar(Bytes)) of
        etUSASCII: Result := ceDefault; //Exact!
        etAnsi:
          { Sure this isn't right in all cases!
            Two/four byte WideChars causing the same result!
            Leads to pain! Is there a way to get a better test?
            I've to start from the premise the function which calls this func
            should decide wether ansi or unicode}
          Result := ceAnsi;
        etUTF8: Result := ceUTF8; //Exact!
      end
    else
      Result := ceDefault
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

function ZConvertAnsiToRaw(const Src: AnsiString; const RawCP: Word): RawByteString;
var US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := ZWideString(Src);
    Result := ZUnicodeToRaw(US, RawCP);
  end;
end;

function ZConvertRawToAnsi(const Src: RawByteString; const RawCP: Word): AnsiString;
var US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := ZRawToUnicode(Src, RawCP);
    Result := AnsiString(US); //use compiler convertation
  end;
end;

function ZConvertAnsiToUTF8(const Src: AnsiString): UTF8String;
var US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := ZWideString(Src);
    Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(US);
  end;
end;

function ZConvertUTF8ToAnsi(const Src: UTF8String): AnsiString;
var US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := {$IFDEF WITH_RAWBYTESTRING}ZWideString{$ELSE}UTF8Decode{$ENDIF}(Src);
    Result := AnsiString(US);
  end;
end;

function ZConvertRawToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
var
  {$IFDEF WITH_LCONVENCODING}
  sUTF8: String;
  {$ELSE}
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
  {$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  begin
    {$IFDEF WITH_LCONVENCODING}
    Result := '';
    case CP of
      28591: //ISO_8859_1
        sUTF8 := ISO_8859_1ToUTF8(PAnsiChar(Src));
      28592:  //ISO_8859_2
        sUTF8 := ISO_8859_2ToUTF8(PAnsiChar(Src));
      1250: //WIN1250
        sUTF8 := CP1250ToUTF8(PAnsiChar(Src));
      1251: //WIN1251
        sUTF8 := CP1251ToUTF8(PAnsiChar(Src));
      1252: //WIN1252
        sUTF8 := CP1252ToUTF8(PAnsiChar(Src));
      1253: //WIN1253
        sUTF8 := CP1253ToUTF8(PAnsiChar(Src));
      1254: //WIN1254
        sUTF8 := CP1254ToUTF8(PAnsiChar(Src));
      1255: //WIN1255
        sUTF8 := CP1255ToUTF8(PAnsiChar(Src));
      1256: //WIN1256
        sUTF8 := CP1256ToUTF8(PAnsiChar(Src));
      1257: //WIN1257
        sUTF8 := CP1257ToUTF8(PAnsiChar(Src));
      1258: //WIN1258
        sUTF8 := CP1258ToUTF8(PAnsiChar(Src));
      437: //CP437
        sUTF8 := CP437ToUTF8(PAnsiChar(Src));
      850: //CP850
        sUTF8 := CP850ToUTF8(PAnsiChar(Src));
      852: //CP852
        sUTF8 := CP852ToUTF8(PAnsiChar(Src));
      866: //CP866
        sUTF8 := CP866ToUTF8(PAnsiChar(Src));
      874: //CP874
        sUTF8 := CP874ToUTF8(PAnsiChar(Src));
      20866: //KOI8 (Russian)
        sUTF8 := KOI8ToUTF8(PAnsiChar(Src));
      65001: //utf8
        sUTF8 := PAnsiChar(Src);
      else
        sUTF8 := PAnsiChar(Src);
    end;
    ZSetString(PAnsichar(sUTF8), Result);
  {$ELSE}
    US := ZRawToUnicode(Src, CP);
    Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(US);
  {$ENDIF}
  end;
end;

function ZConvertUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
var
  {$IFDEF WITH_LCONVENCODING}
  sUTF8: String;
  {$ELSE}
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
  {$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  begin
  {$IFDEF WITH_LCONVENCODING}
    case CP of
      28591: //ISO_8859_1
        sUTF8 := UTF8ToISO_8859_1(PAnsiChar(Src));
      28592:  //ISO_8859_2
        sUTF8 := UTF8ToISO_8859_2(PAnsiChar(Src));
      1250: //WIN1250
        sUTF8 := UTF8ToCP1250(PAnsiChar(Src));
      1251: //WIN1251
        sUTF8 := UTF8ToCP1251(PAnsiChar(Src));
      1252: //WIN1252
        sUTF8 := UTF8ToCP1252(PAnsiChar(Src));
      1253: //WIN1253
        sUTF8 := UTF8ToCP1253(PAnsiChar(Src));
      1254: //WIN1254
        sUTF8 := UTF8ToCP1254(PAnsiChar(Src));
      1255: //WIN1255
        sUTF8 := UTF8ToCP1255(PAnsiChar(Src));
      1256: //WIN1256
        sUTF8 := UTF8ToCP1256(PAnsiChar(Src));
      1257: //WIN1257
        sUTF8 := UTF8ToCP1257(PAnsiChar(Src));
      1258: //WIN1258
        sUTF8 := UTF8ToCP1258(PAnsiChar(Src));
      437: //CP437
        sUTF8 := UTF8ToCP437(PAnsiChar(Src));
      850: //CP850
        sUTF8 := UTF8ToCP850(PAnsiChar(Src));
      852: //CP852
        sUTF8 := UTF8ToCP852(PAnsiChar(Src));
      866: //CP866
        sUTF8 := UTF8ToCP866(PAnsiChar(Src));
      874: //CP874
        sUTF8 := UTF8ToCP874(PAnsiChar(Src));
      20866: //KOI8 (Russian)
        sUTF8 := UTF8ToKOI8(PAnsiChar(Src));
      65001: //UTF8
        sUTF8 := PAnsiChar(Src);
      else
        sUTF8 := PAnsiChar(Src);
    end;
    Result := ''; //Makes compler happy
    ZSetString(PAnsiChar(sUTF8), Result);
  {$ELSE}
    US := UTF8ToString(PAnsiChar(Src));
    Result := ZUnicodeToRaw(US, CP);
  {$ENDIF}
  end;
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZConvertRawToString(const Src: RawByteString;
  const RawCP, StringCP: Word): String;
{$IFNDEF UNICODE}
var
  {$IFDEF WITH_LCONVENCODING}
  sUTF8: String;
  {$ELSE}
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
  {$ENDIF}
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  begin
    {$IFDEF WITH_LCONVENCODING}
    sUTF8 := ''; //Makes Compiler happy
    ZSetString(PAnsichar(Src), sUTF8);
    case RawCP of
      28591: //ISO_8859_1
        Result := ISO_8859_1ToUTF8(PAnsiChar(sUTF8));
      28592:  //ISO_8859_2
        Result := ISO_8859_2ToUTF8(PAnsiChar(sUTF8));
      1250: //WIN1250
        Result := CP1250ToUTF8(PAnsiChar(sUTF8));
      1251: //WIN1251
        Result := CP1251ToUTF8(PAnsiChar(sUTF8));
      1252: //WIN1252
        Result := CP1252ToUTF8(PAnsiChar(sUTF8));
      1253: //WIN1253
        Result := CP1253ToUTF8(PAnsiChar(sUTF8));
      1254: //WIN1254
        Result := CP1254ToUTF8(PAnsiChar(sUTF8));
      1255: //WIN1255
        Result := CP1255ToUTF8(PAnsiChar(sUTF8));
      1256: //WIN1256
        Result := CP1256ToUTF8(PAnsiChar(sUTF8));
      1257: //WIN1257
        Result := CP1257ToUTF8(PAnsiChar(sUTF8));
      1258: //WIN1258
        Result := CP1258ToUTF8(PAnsiChar(sUTF8));
      437: //CP437
        Result := CP437ToUTF8(PAnsiChar(sUTF8));
      850: //CP850
        Result := CP850ToUTF8(PAnsiChar(sUTF8));
      {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
      852: //CP852
        Result := CP852ToUTF8(PAnsiChar(sUTF8));
      {$ENDIF}
      866: //CP866
        Result := CP866ToUTF8(PAnsiChar(sUTF8));
      874: //CP874
        Result := CP874ToUTF8(PAnsiChar(sUTF8));
      20866: //KOI8 (Russian)
        Result := KOI8ToUTF8(PAnsiChar(sUTF8));
      65001: //utf8
        Result := PAnsiChar(sUTF8);
      else
        Result := PAnsiChar(sUTF8);
    end;
  {$ELSE}
    {$IFDEF UNICODE}
    Result := ZRawToUnicode(Src, RawCP);
    {$ELSE}
    US := ZRawToUnicode(Src, RawCP);
    ZSetString(PAnsiChar(ZUnicodeToRaw(US, StringCP)), Result);
    {$ENDIF}
  {$ENDIF}
  end;
end;

function ZConvertStringToRaw(const Src: String; const StringCP, RawCP: Word): RawByteString;
{$IFNDEF UNICODE}
var
  {$IFDEF WITH_LCONVENCODING}
  sUTF8: String;
  {$ELSE}
  US: ZWideString; //COM based, so let's localize the value to avoid Buffer overrun
  {$ENDIF}
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  {$IFDEF WITH_LCONVENCODING}
  begin
    case RawCP of
      28591: //ISO_8859_1
        sUTF8 := UTF8ToISO_8859_1(Src);
      28592:  //ISO_8859_2
        sUTF8 := UTF8ToISO_8859_2(Src);
      1250: //WIN1250
        sUTF8 := UTF8ToCP1250(Src);
      1251: //WIN1251
        sUTF8 := UTF8ToCP1251(Src);
      1252: //WIN1252
        sUTF8 := UTF8ToCP1252(Src);
      1253: //WIN1253
        sUTF8 := UTF8ToCP1253(Src);
      1254: //WIN1254
        sUTF8 := UTF8ToCP1254(Src);
      1255: //WIN1255
        sUTF8 := UTF8ToCP1255(Src);
      1256: //WIN1256
        sUTF8 := UTF8ToCP1256(Src);
      1257: //WIN1257
        sUTF8 := UTF8ToCP1257(Src);
      1258: //WIN1258
        sUTF8 := UTF8ToCP1258(Src);
      437: //CP437
        sUTF8 := UTF8ToCP437(Src);
      850: //CP850
        sUTF8 := UTF8ToCP850(Src);
      {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
      852: //CP852
        sUTF8 := UTF8ToCP852(Src);
      {$ENDIF}
      866: //CP866
        sUTF8 := UTF8ToCP866(Src);
      874: //CP874
        sUTF8 := UTF8ToCP874(Src);
      20866: //KOI8 (Russian)
        sUTF8 := UTF8ToKOI8(Src);
      65001: //UTF8
        sUTF8 := Src;
      else
        sUTF8 := Src;
    end;
    Result := ''; //Makes compler happy
    ZSetString(PAnsiChar(sUTF8), Result);
  end;
  {$ELSE}
    {$IFDEF UNICODE}
    Result := ZUnicodeToRaw(Src, RawCP);
    {$ELSE}
    begin
      US := ZRawToUnicode(Src, StringCP);
      Result := ZUnicodeToRaw(US, RawCP);
    end;
    {$ENDIF}
  {$ENDIF}
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

{$WARNINGS OFF}
function ZConvertStringToRawWithAutoEncode(const Src: String;
  const StringCP, RawCP: Word): RawByteString;
begin
  {$IFDEF UNICODE}
  Result := ZUnicodeToRaw(Src, RawCP);
  {$ELSE}
  Result := ''; //init for FPC
  case DetectUTF8Encoding(Src) of
    etUSASCII: ZSetString(PAnsiChar(Src), Result);
    etAnsi:
      if (RawCP = zCP_UTF8) then
        if ZCompatibleCodePages(StringCP, zCP_UTF8 ) then
          Result := ZUnicodeToRaw(ZWideString(Src), RawCP) //Random success unknown String CP
        else
          Result := ZConvertStringToRaw(Src, StringCP, RawCP)
      else
        ZSetString(PAnsiChar(Src), Result);
    etUTF8:
      if (RawCP = zCP_UTF8) then
        ZSetString(PAnsiChar(Src), Result)
      else
        Result := ZConvertStringToRaw(Src, zCP_UTF8, RawCP);
  end;
  {$ENDIF}
end;
{$WARNINGS ON}

function ZConvertUTF8ToString(const Src: UTF8String;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var
  US: ZWideString; //COM based. Localize the Value to avoid buffer overrun
  S: RawByteString;
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := UTF8ToString(PAnsiChar(Src));
    {$ELSE}
    begin
      Result := ''; //Makes Compiler happy
      US := UTF8ToString(PAnsiChar(Src));
      S := ZUnicodeToRaw(US, StringCP);
      ZSetString(PAnsiChar(S), Result);
    end;
    {$ENDIF}
end;

function ZConvertStringToUTF8(const Src: String;
  const StringCP: Word): UTF8String;
{$IFNDEF UNICODE}
var
  US: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := UTF8String(Src);
    {$ELSE}
    begin
      US := ZRawToUnicode(Src, StringCP);
      {$IFDEF WITH_RAWBYTESTRING}
      Result := UTF8String(US);
      {$ELSE}
      Result := UTF8Encode(US);
      {$ENDIF}
    end;
    {$ENDIF}
end;

function ZConvertStringToUTF8WithAutoEncode(const Src: String;
  const StringCP: Word): UTF8String;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := UTF8String(Src);
  {$ELSE}
  Result := '';
  If DetectUTF8Encoding(PAnsiChar(Src)) in [etUSASCII, etUTF8] then
    ZSetString(PAnsiChar(Src), Result)
  else //Ansi
  begin
    if ZCompatibleCodePages(StringCP, zCP_UTF8)  then
      Tmp := ZWideString(Src)
    else
      Tmp := ZRawToUnicode(PAnsiChar(Src), StringCP);
    {$IFDEF WITH_RAWBYTESTRING}
    Result := UTF8String(Tmp);
    {$ELSE}
    Result := UTF8Encode(Tmp);
    {$ENDIF}
  end;
  {$ENDIF}
end;

function ZConvertStringToAnsi(const Src: String;
  const StringCP: Word): AnsiString;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := AnsiString(Src);
  {$ELSE}
  Tmp := ZRawToUnicode(PAnsiChar(Src), StringCP);
  Result := AnsiString(Tmp);
  {$ENDIF}
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZConvertStringToAnsiWithAutoEncode(const Src: String;
  const StringCP: Word): AnsiString;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := AnsiString(Src);
  {$ELSE}
  Result := '';
  If DetectUTF8Encoding(PAnsiChar(Src)) in [etUSASCII, etAnsi] then
    ZSetString(PAnsiChar(Src), Result)
  else
  begin
    Tmp := UTF8ToString(PAnsiChar(Src));
    Result := AnsiString(Tmp);
  end;
  {$ENDIF}
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

function ZConvertAnsiToString(const Src: AnsiString;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := String(Src);
  {$ELSE}
  Result := '';
  Tmp := ZRawToUnicode(PAnsiChar(Src), ZDefaultSystemCodePage);
  ZSetString(PAnsiChar(ZUnicodeToRaw(Tmp, StringCP)), Result);
  {$ENDIF}
end;

function ZConvertUnicodeToString(const Src: ZWideString;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  Tmp := ZUnicodeToRaw(Src, StringCP);
  ZSetString(PAnsiChar(Tmp), Result);
  {$ENDIF}
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZConvertUnicodeToString_CPUTF8(const Src: ZWideString;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  Tmp := UTF8Encode(Src);
  ZSetString(PAnsiChar(Tmp), Result);
  {$ENDIF}
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

function ZConvertStringToUnicode(const Src: String;
  const StringCP: Word): ZWideString;
{$IFNDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  Tmp := ''; //Makes compiler Happy
  ZSetString(PAnsiChar(Src), Tmp);
  Result := ZRawToUnicode(Tmp, StringCP);
  {$ENDIF}
end;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZConvertString_CPUTF8ToUnicode(const Src: String;
  const StringCP: Word): ZWideString;
{$IFNDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  Tmp := ''; //Makes Compiler happy
  ZSetString(PAnsiChar(Src), Tmp);
  Result := UTF8ToString(Tmp);
  {$ENDIF}
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}


function ZConvertStringToUnicodeWithAutoEncode(const Src: String;
  const StringCP: Word): ZWideString;
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  If DetectUTF8Encoding(PAnsiChar(Src)) in [etUSASCII, etUTF8] then
    Result := UTF8ToString(PAnsiChar(Src))
  else
    if ZCompatibleCodePages(StringCP, zCP_UTF8)  then
      Result := ZWideString(Src)
    else
      Result := ZRawToUnicode(PAnsiChar(Src), StringCP);
  {$ENDIF}
end;


{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function ZMoveAnsiToRaw(const Src: AnsiString; const RawCP: Word): RawByteString;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveRawToAnsi(const Src: RawByteString; const RawCP: Word): AnsiString;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveAnsiToUTF8(const Src: AnsiString): UTF8String;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveUTF8ToAnsi(const Src: UTF8String): AnsiString;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveRawToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  ZSetString(PAnsiChar(Src), Result);
end;

function ZMoveStringToAnsi(Const Src: String; const StringCP: Word): AnsiString;
begin
  {$IFDEF UNICODE}
  Result := AnsiString(Src);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;

function ZMoveAnsiToString(const Src: AnsiString; const StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := String(Src);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;


function ZMoveRawToString(const Src: RawByteString;
  const RawCP, StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := ZRawToUnicode(Src, RawCP);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;

function ZMoveStringToRaw(const Src: String;
  const StringCP, RawCP: Word): RawByteString;
begin
  {$IFDEF UNICODE}
  Result := ZUnicodeToRaw(Src, RawCP);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;

function ZMoveUTF8ToString(const Src: UTF8String; StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := String(Src);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;

function ZMoveStringToUTF8(const Src: String; const StringCP: Word): UTF8String;
begin
  {$IFDEF UNICODE}
  Result := UTF8String(Src);
  {$ELSE}
  ZSetString(PAnsiChar(Src), Result);
  {$ENDIF}
end;

{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
{$WARNINGS OFF}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString;
var
  US: ZWideString;
  Bytes: TByteDynArray;
begin
  if Size = 0 then
    Result := ''
  else
  begin
    SetLength(Bytes, Size +2);
    System.move(Buffer^, Pointer(Bytes)^, Size);
    case TestEncoding(Bytes, Size, ConSettings) of
      ceDefault: Result := PAnsiChar(Bytes);
      ceAnsi:
        if ConSettings.ClientCodePage.Encoding = ceAnsi then
          if ( ConSettings.CTRL_CP = zCP_UTF8) or (ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP) then //second test avoids encode the string twice
            Result := PAnsiChar(Bytes)  //should be exact
          else
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(AnsiToUTF8(PAnsiChar(Bytes)))  //no other possibility
            {$ELSE}
            Result := ZUnicodeToRaw(ZRawToUnicode(PAnsiChar(Bytes), ConSettings.CTRL_CP), ConSettings.ClientCodePage.CP)
            {$ENDIF}
        else  //Database expects UTF8
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            Result := AnsiToUTF8(String(PAnsiChar(Bytes))) //Can't localize the ansi CP
          else
            {$IFDEF WITH_LCONVENCODING}
            Result := AnsiToUTF8(PAnsiChar(Bytes));
            {$ELSE}
            Result := UTF8Encode(ZRawToUnicode(PAnsiChar(Bytes), ConSettings.CTRL_CP));
            {$ENDIF}
      ceUTF8:
        if ConSettings.ClientCodePage.Encoding = ceAnsi then //ansi expected
          {$IFDEF WITH_LCONVENCODING}
          Result := Consettings.PlainConvertFunc(String(PAnsiChar(Bytes)))
          {$ELSE}
          Result := ZUnicodeToRaw(UTF8ToString(PAnsiChar(Bytes)), ConSettings.ClientCodePage.CP)
          {$ENDIF}
         else //UTF8 Expected
           Result := PAnsiChar(Bytes);
      ceUTF16:
        begin
          SetLength(US, Size div 2);
          System.Move(PWideChar(Bytes)^, PWideChar(US)^, Size);
          if ConSettings.ClientCodePage.Encoding = ceAnsi then
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(UTF8Encode(US))
            {$ELSE}
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
            {$ENDIF}
          else
            Result := UTF8Encode(US);
        end;
      else
        Result := '';
    end;
  end;
end;
{$WARNINGS ON}

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString;
var DB_CP: Word;
begin
  DB_CP := ConSettings.ClientCodePage.CP;
  ConSettings.ClientCodePage.CP := ToCP;
  Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
  ConSettings.ClientCodePage.CP := DB_CP;
end;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString;
begin
  if FromDB then
    if ( ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP ) or not ConSettings.AutoEncode then
      Result := Ansi
    else
      {$IFDEF WITH_LCONVENCODING}
      Result := Consettings.DbcConvertFunc(Ansi)
      {$ELSE}
      Result := ZUnicodeToRaw(ZRawToUnicode(Ansi, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP)
      {$ENDIF}
  else
    Result := ''; // not done yet  and not needed. Makes the compiler happy
end;

function GetValidatedAnsiString(const Uni: ZWideString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString;
begin
  if FromDB then
    {$IFDEF WITH_LCONVENCODING}
    Result := UTF8Encode(Uni)
    {$ELSE}
    Result := ZUnicodeToRaw(Uni, ConSettings^.CTRL_CP)
    {$ENDIF}
  else
    Result := ''; // not done yet  and not needed. Makes the compiler happy
end;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  WasDecoded: Boolean; ConSettings: PZConSettings): RawByteString;
var
  US: ZWideString;
begin
  if WasDecoded then
  begin
    SetLength(US, Size div 2);
    System.Move(Buffer^, PWideChar(US)^, Size);
    Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP);
  end
  else
    Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
end;
{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Stream the Stream with the unknown format and data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  Len: Integer;
  US: ZWideString;
  Bytes: TByteDynArray;

  procedure SetFromWide;
  begin
    SetLength(US, Size div 2);
    System.Move(PWideChar(Bytes)^, PWideChar(US)^, Size);
  end;
begin
  Result := nil;
  US := '';
  if Assigned(Buffer) and ( Size > 0 ) then
  begin
    SetLength(Bytes, Size +2);
    System.move(Buffer^, Pointer(Bytes)^, Size);
    if FromDB then //do not check encoding twice
      Result := GetValidatedUnicodeStream(PAnsiChar(Bytes), ConSettings, FromDB)
    else
      case TestEncoding(Bytes, Size, ConSettings) of
        ceDefault:
          case Consettings.ClientCodePage.Encoding of
            ceUTF8: US := UTF8ToString(PAnsiChar(Bytes));
            ceAnsi:
              {$IFDEF WITH_LCONVENCODING}
              US := ZWideString(PAnsiChar(Bytes)); //cast means random success
              {$ELSE}
              if ( ConSettings.CTRL_CP = zCP_UTF8) then
                US := ZWideString(PAnsiChar(Bytes)) //random success
              else
                US := ZRawToUnicode(PAnsiChar(Bytes), ConSettings.CTRL_CP);
             {$ENDIF}
            end;
        ceAnsi: //We've to start from the premisse we've got a Unicode string i there
          begin
            SetLength(US, Size div 2);
            System.Move(PWideChar(Bytes)^, PWideChar(US)^, Size);
          end;
        ceUTF8: US := UTF8ToString(PAnsiChar(Bytes));
        ceUTF16:
          begin
            SetLength(US, Size div 2);
            System.Move(PWideChar(Bytes)^, PWideChar(US)^, Size);
          end;
      end;

    Len := Length(US)*2;
    if not Assigned(Result) and (Len > 0) then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(PWideChar(US)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
    SetLength(Bytes, 0);
  end;
end;

function GetValidatedUnicodeStream(const Ansi: RawByteString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  Len: Integer;
  US: ZWideString;
begin
  Result := nil;
  if Ansi <> '' then
  begin
    if FromDB then
      {$IFDEF WITH_LCONVENCODING}
      US := UTF8ToString(Consettings.DbcConvertFunc(Ansi))
      {$ELSE}
      US := ZRawToUnicode(Ansi, ConSettings.ClientCodePage.CP)
      {$ENDIF}
    else
      case DetectUTF8Encoding(Ansi) of
        etUSASCII, etUTF8: US := UTF8ToString(Ansi);
        etAnsi:
          {$IFDEF WITH_LCONVENCODING}
          US := ZWideString(Ansi); //random success
          {$ELSE}
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            US := ZWideString(Ansi) //random success
          else
            US := ZRawToUnicode(Ansi, ConSettings.CTRL_CP);
         {$ENDIF}
      end;

    Len := Length(US)*2;
    if Len > 0 then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(PWideChar(US)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
  end;
end;

procedure SetConvertFunctions(ConSettings: PZConSettings);
begin
  ConSettings^.ConvFuncs.ZAnsiToUTF8 := nil;
  ConSettings^.ConvFuncs.ZUTF8ToAnsi:= nil;
  ConSettings^.ConvFuncs.ZUTF8ToString:= nil;
  ConSettings^.ConvFuncs.ZStringToUTF8:= nil;
  ConSettings^.ConvFuncs.ZAnsiToRaw:= nil;
  ConSettings^.ConvFuncs.ZRawToAnsi:= nil;
  ConSettings^.ConvFuncs.ZRawToUTF8:= nil;
  ConSettings^.ConvFuncs.ZUTF8ToRaw:= nil;
  ConSettings^.ConvFuncs.ZStringToRaw:= nil;
  ConSettings^.ConvFuncs.ZAnsiToString := nil;
  ConSettings^.ConvFuncs.ZStringToAnsi := nil;
  ConSettings^.ConvFuncs.ZRawToString:= nil;
  ConSettings^.ConvFuncs.ZUnicodeToRaw:= nil;
  ConSettings^.ConvFuncs.ZRawToUnicode:= nil;
  ConSettings^.ConvFuncs.ZUnicodeToString:= nil;
  ConSettings^.ConvFuncs.ZStringToUnicode:= nil;

  //Let's start with the AnsiTo/From types..
  // Ansi to/from UTF8String
  if ZCompatibleCodePages(ZDefaultSystemCodePage, zCP_UTF8) then
  begin
    ConSettings^.ConvFuncs.ZAnsiToUTF8 := @ZMoveAnsiToUTF8;
    ConSettings^.ConvFuncs.ZUTF8ToAnsi := @ZMoveUTF8ToAnsi;
  end
  else
  begin
    ConSettings^.ConvFuncs.ZAnsiToUTF8 := @ZConvertAnsiToUTF8;
    ConSettings^.ConvFuncs.ZUTF8ToAnsi := @ZConvertUTF8ToAnsi;
  end;

  // Ansi to/from String
  if ZCompatibleCodePages(ZDefaultSystemCodePage, ConSettings^.CTRL_CP) then
  begin
    ConSettings^.ConvFuncs.ZAnsiToString := @ZMoveAnsiToString;
    if ConSettings^.AutoEncode then
      ConSettings^.ConvFuncs.ZStringToAnsi := @ZConvertStringToAnsiWithAutoEncode
    else
      ConSettings^.ConvFuncs.ZStringToAnsi := @ZMoveStringToAnsi;
  end
  else
  begin
    ConSettings^.ConvFuncs.ZAnsiToString := @ZConvertAnsiToString;
    if ConSettings^.AutoEncode then
      ConSettings^.ConvFuncs.ZStringToAnsi := @ZConvertStringToAnsiWithAutoEncode
    else
      ConSettings^.ConvFuncs.ZStringToAnsi := @ZConvertStringToAnsi;
  end;

  if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
  begin
    // raw to/from UTF8
    if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, zCP_UTF8) then
    begin
      ConSettings^.ConvFuncs.ZRawToUTF8 := @ZMoveRawToUTF8;
      ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZMoveUTF8ToRaw;
    end
    else
    begin
      ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRawToUTF8;
      ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw;
    end;

    // raw to/from ansi
    if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ZDefaultSystemCodePage) then
    begin
      ConSettings^.ConvFuncs.ZAnsiToRaw := @ZMoveAnsiToRaw;
      ConSettings^.ConvFuncs.ZRawToAnsi := @ZMoveRawToAnsi;
    end
    else
    begin
      ConSettings^.ConvFuncs.ZAnsiToRaw := @ZConvertAnsiToRaw;
      ConSettings^.ConvFuncs.ZRawToAnsi := @ZConvertRawToAnsi;
    end;

    // raw to/from unicode
    if ConSettings^.ClientCodePage^.CP = zCP_NONE then
    begin
      if ConSettings^.AutoEncode then
        ConSettings^.ConvFuncs.ZRawToUnicode := @ZUnknownRawToUnicodeWithAutoEncode
      else
        ConSettings^.ConvFuncs.ZRawToUnicode := @ZUnknownRawToUnicode;
      ConSettings^.ConvFuncs.ZUnicodeToRaw := @ZUnicodeToUnknownRaw;
    end
    else
    begin
      ConSettings^.ConvFuncs.ZRawToUnicode := @ZRawToUnicode;
      ConSettings^.ConvFuncs.ZUnicodeToRaw := @ZUnicodeToRaw;
    end;

    //last but not least the String to/from converters
    //string represents the DataSet/IZResultSet Strings
    if ZCompatibleCodePages(ConSettings^.CTRL_CP, zCP_UTF8) then
    begin
      ConSettings^.ConvFuncs.ZUTF8ToString := @ZMoveUTF8ToString;
      if ConSettings^.AutoEncode then
        ConSettings^.ConvFuncs.ZStringToUTF8 := @ZConvertStringToUTF8WithAutoEncode
      else
        ConSettings^.ConvFuncs.ZStringToUTF8 := @ZMoveStringToUTF8;
    end
    else
    begin
      ConSettings^.ConvFuncs.ZUTF8ToString := @ZConvertUTF8ToString;
      if ConSettings^.AutoEncode then
        ConSettings^.ConvFuncs.ZStringToUTF8 := @ZConvertStringToUTF8WithAutoEncode
      else
        ConSettings^.ConvFuncs.ZStringToUTF8 := @ZConvertStringToUTF8
    end;

    {$IFDEF UNICODE}
    Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRaw;
    Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;

    ConSettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;
    Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicode;
    {$ELSE}
      {String To/From Raw}
      if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      begin
        Consettings^.ConvFuncs.ZRawToString := @ZMoveRawToString;
        if ConSettings^.AutoEncode then
          Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode
        else
          Consettings^.ConvFuncs.ZStringToRaw := @ZMoveStringToRaw;
      end
      else
        if ConSettings^.AutoEncode then
        begin
          Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;
          Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode;
        end
        else
        begin
          Consettings^.ConvFuncs.ZStringToRaw := @ZMoveStringToRaw;
          Consettings^.ConvFuncs.ZRawToString := @ZMoveRawToString;
        end;

      {String To/From Unicode}
      if ConSettings^.CTRL_CP = zCP_UTF8 then
        Consettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString_CPUTF8
      else
        Consettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;

      if ConSettings^.AutoEncode then
        Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicodeWithAutoEncode
      else
        if ConSettings^.CTRL_CP = zCP_UTF8 then
          Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertString_CPUTF8ToUnicode
        else
          Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicode;
    {$ENDIF}
  end
  else //autoencode strings is allways true
  begin
    ConSettings^.ConvFuncs.ZUTF8ToString := @ZConvertUTF8ToString;
    ConSettings^.ConvFuncs.ZStringToUTF8 := @ZConvertStringToUTF8WithAutoEncode;
    ConSettings^.ConvFuncs.ZAnsiToRaw := @ZConvertAnsiToRaw;
    ConSettings^.ConvFuncs.ZRawToAnsi := @ZConvertRawToAnsi;
    ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRawToUTF8;
    ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw;
    Consettings^.ConvFuncs.ZStringToRaw := @ZConvertStringToRawWithAutoEncode;
    Consettings^.ConvFuncs.ZRawToString := @ZConvertRawToString;
    Consettings^.ConvFuncs.ZUnicodeToRaw := @ZUnicodeToRaw;
    Consettings^.ConvFuncs.ZRawToUnicode := @ZRawToUnicode;
    ConSettings^.ConvFuncs.ZUnicodeToString := @ZConvertUnicodeToString;
    Consettings^.ConvFuncs.ZStringToUnicode := @ZConvertStringToUnicodeWithAutoEncode;
  end;
end;

end.

