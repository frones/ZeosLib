{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 charcter encoding unit                  }
{                                                         }
{        Originally written by EgonHugeist                }
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
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF WITH_LCONVENCODING}
  {$MACRO ON}
   LCLVersion, LConvEncoding,
  {$ENDIF}
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
  Math,
  {$IFEND}
  ZCompatibility;

const
  {code page identifiers http://msdn.microsoft.com/en-us/library/bb643325.aspx}
  zCP_Binary = -1;
  zCP_DOS437 = 437; {IBM437/MS-DOS odepage 437 (US)}
  zCP_DOS708 = 708; {Arabic (ASMO 708)}
  zCP_DOS720 = 720; {Arabic (Transparent ASMO); Arabic (DOS)}
  zCP_DOS737 = 737; {OEM Greek (formerly 437G); Greek (DOS)}
  zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
  zCP_DOS850 = 850;	{MS-DOS Codepage 850 (Multilingual Latin 1)}
  zCP_DOS852 = 852; {ibm852 852 east european(DOS)}
  zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
  zCP_DOS858 = 858; {MS-DOS Codepage 858  Latin I + Euro symbol}
  zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
  zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
  zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
  zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
  zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
  zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
  zCP_DOS866 = 866; {ibm866	866	Cyrl (DOS)}
  zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}

  zCP_WIN874 = 874; {ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)}
  zCP_MSWIN921 = 921;
  zCP_MSWIN923 = 923;

  zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
  zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
  zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
  zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}

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

  zCP_macintosh = 10000; {MAC Roman; Western European (Mac)}
  zCP_x_mac_ce = 10029; {MAC Latin 2; Central European (Mac)}
  zCP_utf32 = 12000; {Unicode UTF-32, little endian byte order; available only to managed applications}
  zCP_utf32BE = 12001; {Unicode UTF-32, big endian byte order; available only to managed applications}

  zCP_x_IA5_Swedish = 20107; {IA5 Swedish (7-bit)}
  zCP_us_ascii = 20127; {US-ASCII (7-bit)}
  zCP_KOI8R = 20866; {cskoi8r 20866 Cyrillic (KOI8-R)}
  zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}
  zCP_KOI8U = 21866; {KOI8-U is an 8-bit character encoding, designed to cover Ukrainian, which uses the Cyrillic alphabet.}
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2 east european (ISO), 8-bit single-byte coded graphic character sets - Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L3_ISO_8859_3 = 28593; {ISO 8859-3 Latin 3}
  zCP_L4_ISO_8859_4 = 28594; {ISO 8859-4 Baltic}
  zCP_L5_ISO_8859_5 = 28595; {8bit single-byte coded graphic character sets - Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L6_ISO_8859_6 = 28596; {ISO 8859-6 Arabic}
  zCP_L7_ISO_8859_7 = 28597; {ISO 8859-7 Greek}
  zCP_L8_ISO_8859_8 = 28598; {ISO 8859-8 Hebrew; Hebrew (ISO-Visual)}
  zCP_L5_ISO_8859_9 = 28599; {ISO 8859-9 Turkish}
  zCP_L6_ISO_8859_10 = 28600; { ISO 8859-10, ECMA 144 Nordic }
  zCP_L7_ISO_8859_13 = 28603; {ISO 8859-13 Estonian}
  zCP_L8_ISO_8859_14 = 28604; { ISO 8859-14 Celtic }
  zCP_L9_ISO_8859_15 = 28605; {ISO 8859-15 Latin 9}
  zCP_L10_ISO_8859_16 = 28606;  { ISO 8859-16, ASRO SR 14111 Romanian }

  zCP_csISO2022JP = 50221; {ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
  zCP_euc_JP_win = 51932; {EUC Japanese}
  zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
  zCP_euc_kr = 51949; {EUC Korean}
  zCP_GB18030 = 54936; {Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
  zCP_UTF7 = 65000;
  zCP_UTF8 = 65001;
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

function ZRawToUnicode(const S: RawByteString; const CP: Word): ZWideString; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}
function PRawToUnicode(Source: PAnsiChar; const SourceBytes: NativeUInt; CP: Word): ZWideString; //{$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}
function ZUnicodeToRaw(const US: ZWideString; CP: Word): RawByteString; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}
function PUnicodeToRaw(Source: PWideChar; CodePoints: NativeUInt; CP: Word): RawByteString; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}
{$IFNDEF UNICODE}
function PUnicodeToString(Source: PWideChar; CodePoints: NativeUInt; CP: Word): String; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}
{$ENDIF}

{converter functions for the String-types}
{$IFDEF WITH_LCONVENCODING}
function ZConvertRaw28591ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw28591(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw28592ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw28592(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1250ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1250(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1251ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1251(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1252ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1252(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1253ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1253(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1254ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1254(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1255ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1255(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1256ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1256(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1257ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1257(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw1258ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw1258(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw437ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw437(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw850ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw850(Const Src: UTF8String; const CP: Word): RawByteString;
{$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
function ZConvertRaw852ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw852(Const Src: UTF8String; const CP: Word): RawByteString;
{$ENDIF}
function ZConvertRaw866ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw866(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw874ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw874(Const Src: UTF8String; const CP: Word): RawByteString;
function ZConvertRaw20866ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
function ZConvertUTF8ToRaw20866(Const Src: UTF8String; const CP: Word): RawByteString;
{$ENDIF}
function ZConvertPRawToUTF8(const Src: PAnsiChar; Len: NativeUInt; const RawCP: Word): UTF8String;
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
function ZConvertStringToAnsiWithAutoEncode(const Src: String; const {%H-}StringCP: Word): AnsiString;
function ZConvertAnsiToString(const Src: AnsiString; const StringCP: Word): String;
function ZConvertUnicodeToString(const Src: ZWideString; const StringCP: Word): String;
function ZConvertUnicodeToString_CPUTF8(const Src: ZWideString; const {%H-}StringCP: Word): String;
function ZConvertStringToUnicode(const Src: String; const StringCP: Word): ZWideString;
function ZConvertString_CPUTF8ToUnicode(const Src: String; const {%H-}StringCP: Word): ZWideString;
function ZConvertStringToUnicodeWithAutoEncode(const Src: String; const StringCP: Word): ZWideString;
{move functions for the String types}
function ZMovePRawToUTF8(const Src: PAnsiChar; Len: NativeUInt; const {%H-}RawCP: Word): UTF8String;
function ZMoveAnsiToRaw(const Src: AnsiString; const {%H-}RawCP: Word): RawByteString;
function ZMoveRawToAnsi(const Src: RawByteString; const {%H-}RawCP: Word): AnsiString;
function ZMoveAnsiToUTF8(const Src: AnsiString): UTF8String;
function ZMoveUTF8ToAnsi(const Src: UTF8String): AnsiString;
function ZMoveRawToUTF8(const Src: RawByteString; const {%H-}CP: Word): UTF8String;
function ZMoveUTF8ToRaw(Const Src: UTF8String; const {%H-}CP: Word): RawByteString;
function ZMoveStringToAnsi(Const Src: String; const {%H-}StringCP: Word): AnsiString;
function ZMoveAnsiToString(const Src: AnsiString; const {%H-}StringCP: Word): String;
function ZMoveRawToString(const Src: RawByteString; const {%H-}RawCP, {%H-}StringCP: Word): String;
function ZMoveStringToRaw(const Src: String; const {%H-}StringCP, {%H-}RawCP: Word): RawByteString;
function ZMoveUTF8ToString(const Src: UTF8String; {%H-}StringCP: Word): String;
function ZMoveStringToUTF8(const Src: String; const {%H-}StringCP: Word): UTF8String;

function ZUnknownRawToUnicode(const S: RawByteString; const CP: Word): ZWideString;
function ZUnknownRawToUnicodeWithAutoEncode(const S: RawByteString;
  const CP: Word): ZWideString;
function ZUnicodeToUnknownRaw(const US: ZWideString; CP: Word): RawByteString;

{**
  Is the codepage equal or compatible?
  @param CP1 word the first codepage to compare
  @param CP2 word the second codepage to compare
  @returns Boolean True if codepage is equal or compatible
}
function ZCompatibleCodePages(const CP1, CP2: Word): Boolean; {$IF defined (WITH_INLINE) and not defined(WITH_C11389_ERROR)}inline;{$IFEND}

{**
  Set the string-types conversion funtion in relation to the Connection-Settings.
  The Results should be as optimal as possible to speed up the behavior
  @param ConSettings a Pointer to the ConnectionSetting
}
procedure SetConvertFunctions(ConSettings: PZConSettings); {$IFDEF WITH_LCONVENCODING}overload;{$ENDIF}

Type
  TEncodeType = (etUSASCII, etUTF8, etANSI);
  TSBCSMapProc = procedure(Source: PAnsichar; SourceBytes: NativeUInt; var Dest: PWideChar);
  TMBCSMapProc = function(Source: PAnsichar; SourceBytes: NativeUInt; Dest: PWideChar): LengthInt;
  PSBCS_MAP = ^TSBCS_MAP;
  TSBCS_MAP = packed array[$00..$FF] of Word;

function ZDetectUTF8Encoding(Source: PAnsiChar; Len: NativeUInt): TEncodeType; overload;
function ZDetectUTF8Encoding(const Source: RawByteString): TEncodeType; overload;
function USASCII7ToUnicodeString(Source: PAnsiChar; Len: NativeUInt): ZWideString; overload;
function USASCII7ToUnicodeString(const Source: RawByteString): ZWideString; overload;

{ Message-Helpers }
function ConvertZMsgToRaw(const AMessage: String; {$IFNDEF LCL}Const{$ENDIF}MsgCP, RawCP: Word): RawByteString;
function ConvertEMsgToRaw(const AMessage: String; {$IFNDEF LCL}Const{$ENDIF} RawCP: Word): RawByteString;


{SBCS codepages $00..FF}
procedure AnsiSBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  var Dest: ZWideString; const SBCS_MAP: PSBCS_MAP); overload;
procedure AnsiSBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  const MapProc: TSBCSMapProc; var Dest: ZWideString); overload;
procedure MapByteToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  var dest: PWideChar); {$IF defined (WITH_INLINE)}inline;{$IFEND}

{MBCS codepages }
procedure AnsiMBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  const MapProc: TMBCSMapProc; var Dest: ZWideString);
function UTF8ToWideChar(Source: PAnsichar; SourceBytes: NativeUInt; Dest: PWideChar): LengthInt;

const
  {$IFDEF USE_RAW2WIDE_PROCS} //compiler related: D7 is less optimal use MultibultToWidechar instead for known CP's
  CP437ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP708ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2502, $2524, $00E9, $00E2, $2561, $00E0, $2562, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $2556, $2555, $2563,
    $2551, $2557, $255D, $00F4, $255C, $255B, $00FB, $00F9, $2510, $2514, $009A, $009B, $009C, $009D, $009E, $009F,
    $F8C1, $2534, $252C, $251C, $00A4, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $060C, $2566, $00AB, $00BB,
    $2591, $2592, $2593, $2560, $2550, $256C, $2567, $2568, $2564, $2565, $2559, $061B, $2558, $2552, $2553, $061F,
    $256B, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637, $0638, $0639, $063A, $2588, $2584, $258C, $2590, $2580,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $F8C2, $F8C3, $F8C4, $F8C5, $F8C6, $F8C7, $256A, $2518, $250C, $00B5, $00A3, $25A0, $00A0);
  CP720ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $00E9, $00E2, $0084, $00E0, $0086, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $008D, $008E, $008F,
    $0090, $0651, $0652, $00F4, $00A4, $0640, $00FB, $00F9, $0621, $0622, $0623, $0624, $00A3, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F, $0630, $0631, $0632, $0633, $0634, $0635, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $0636, $0637, $0638, $0639, $063A, $0641, $00B5, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A,
    $2261, $064B, $064C, $064D, $064E, $064F, $0650, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP737ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F, $03A0,
    $03A1, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8,
    $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $03C5, $03C6, $03C7, $03C8,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03C9, $03AC, $03AD, $03AE, $03CA, $03AF, $03CC, $03CD, $03CB, $03CE, $0386, $0388, $0389, $038A, $038C, $038E,
    $038F, $00B1, $2265, $2264, $03AA, $03AB, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP775ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0106, $00FC, $00E9, $0101, $00E4, $0123, $00E5, $0107, $0142, $0113, $0156, $0157, $012B, $0179, $00C4, $00C5,
    $00C9, $00E6, $00C6, $014D, $00F6, $0122, $00A2, $015A, $015B, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $00A4,
    $0100, $012A, $00F3, $017B, $017C, $017A, $201D, $00A6, $00A9, $00AE, $00AC, $00BD, $00BC, $0141, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $0104, $010C, $0118, $0116, $2563, $2551, $2557, $255D, $012E, $0160, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0172, $016A, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $017D,
    $0105, $010D, $0119, $0117, $012F, $0161, $0173, $016B, $017E, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $00D3, $00DF, $014C, $0143, $00F5, $00D5, $00B5, $0144, $0136, $0137, $013B, $013C, $0146, $0112, $0145, $2019,
    $00AD, $00B1, $201C, $00BE, $00B6, $00A7, $00F7, $201E, $00B0, $2219, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP850ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00F0, $00D0, $00CA, $00CB, $00C8, $0131, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE, $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
    $00AD, $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP852ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $016F, $0107, $00E7, $0142, $00EB, $0150, $0151, $00EE, $0179, $00C4, $0106,
    $00C9, $0139, $013A, $00F4, $00F6, $013D, $013E, $015A, $015B, $00D6, $00DC, $0164, $0165, $0141, $00D7, $010D,
    $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017D, $017E, $0118, $0119, $00AC, $017A, $010C, $015F, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $011A, $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0102, $0103, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $0111, $0110, $010E, $00CB, $010F, $0147, $00CD, $00CE, $011B, $2518, $250C, $2588, $2584, $0162, $016E, $2580,
    $00D3, $00DF, $00D4, $0143, $0144, $0148, $0160, $0161, $0154, $00DA, $0155, $0170, $00FD, $00DD, $0163, $00B4,
    $00AD, $02DD, $02DB, $02C7, $02D8, $00A7, $00F7, $00B8, $00B0, $00A8, $02D9, $0171, $0158, $0159, $25A0, $00A0);
  CP857ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $0131, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $0130, $00D6, $00DC, $00F8, $00A3, $00D8, $015E, $015F,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $011E, $011F, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00BA, $00AA, $00CA, $00CB, $00C8, $F8BB, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $F8BC, $00D7, $00DA, $00DB, $00D9, $00EC, $00FF, $00AF, $00B4,
    $00AD, $00B1, $F8BD, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP858ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00F0, $00D0, $00CA, $00CB, $00C8, $20AC, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE, $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
    $00AD, $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP860ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E3, $00E0, $00C1, $00E7, $00EA, $00CA, $00E8, $00CD, $00D4, $00EC, $00C3, $00C2,
    $00C9, $00C0, $00C8, $00F4, $00F5, $00F2, $00DA, $00F9, $00CC, $00D5, $00DC, $00A2, $00A3, $00D9, $20A7, $00D3,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00D2, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP861ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00D0, $00F0, $00DE, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00FE, $00FB, $00DD, $00FD, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00C1, $00CD, $00D3, $00DA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP862ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $00A2, $00A3, $00A5, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP863ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00C2, $00E0, $00B6, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $2017, $00C0, $00A7,
    $00C9, $00C8, $00CA, $00F4, $00CB, $00CF, $00FB, $00F9, $00A4, $00D4, $00DC, $00A2, $00A3, $00D9, $00DB, $0192,
    $00A6, $00B4, $00F3, $00FA, $00A8, $00B8, $00B3, $00AF, $00CE, $2310, $00AC, $00BD, $00BC, $00BE, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP864ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00B0, $00B7, $2219, $221A, $2592, $2500, $2502, $253C, $2524, $252C, $251C, $2534, $2510, $250C, $2514, $2518,
    $03B2, $221E, $03C6, $00B1, $00BD, $00BC, $2248, $00AB, $00BB, $FEF7, $FEF8, $009B, $009C, $FEFB, $FEFC, $009F,
    $00A0, $00AD, $FE82, $00A3, $00A4, $FE84, $F8BE, $F8BF, $FE8E, $FE8F, $FE95, $FE99, $060C, $FE9D, $FEA1, $FEA5,
    $0660, $0661, $0662, $0663, $0664, $0665, $0666, $0667, $0668, $0669, $FED1, $061B, $FEB1, $FEB5, $FEB9, $061F,
    $00A2, $FE80, $FE81, $FE83, $FE85, $FECA, $FE8B, $FE8D, $FE91, $FE93, $FE97, $FE9B, $FE9F, $FEA3, $FEA7, $FEA9,
    $FEAB, $FEAD, $FEAF, $FEB3, $FEB7, $FEBB, $FEBF, $FEC1, $FEC5, $FECB, $FECF, $00A6, $00AC, $00F7, $00D7, $FEC9,
    $0640, $FED3, $FED7, $FEDB, $FEDF, $FEE3, $FEE7, $FEEB, $FEED, $FEEF, $FEF3, $FEBD, $FECC, $FECE, $FECD, $FEE1,
    $FE7D, $0651, $FEE5, $FEE9, $FEEC, $FEF0, $FEF2, $FED0, $FED5, $FEF5, $FEF6, $FEDD, $FED9, $FEF1, $25A0, $F8C0);
  CP865ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00A4,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP866ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $0401, $0451, $0404, $0454, $0407, $0457, $040E, $045E, $00B0, $2219, $00B7, $221A, $2116, $00A4, $25A0, $00A0);
  CP869ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0386, $0087, $00B7, $00AC, $00A6, $2018, $2019, $0388, $2015, $0389,
    $038A, $03AA, $038C, $0093, $0094, $038E, $03AB, $00A9, $038F, $00B2, $00B3, $03AC, $00A3, $03AD, $03AE, $03AF,
    $03CA, $0390, $03CC, $03CD, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $00BD, $0398, $0399, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $039A, $039B, $039C, $039D, $2563, $2551, $2557, $255D, $039E, $039F, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $03A0, $03A1, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $03A3,
    $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2, $03B3, $2518, $250C, $2588, $2584, $03B4, $03B5, $2580,
    $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $0384,
    $00AD, $00B1, $03C5, $03C6, $03C7, $00A7, $03C8, $0385, $00B0, $00A8, $03C9, $03CB, $03B0, $03CE, $25A0, $00A0);
  CP870ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $009C, $0009, $0086, $007F, $0097, $008D, $008E, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $009D, $0085, $0008, $0087, $0018, $0019, $0092, $008F, $001C, $001D, $001E, $001F,
    $0080, $0081, $0082, $0083, $0084, $000A, $0017, $001B, $0088, $0089, $008A, $008B, $008C, $0005, $0006, $0007,
    $0090, $0091, $0016, $0093, $0094, $0095, $0096, $0004, $0098, $0099, $009A, $009B, $0014, $0015, $009E, $001A,
    $0020, $00A0, $00E2, $00E4, $0163, $00E1, $0103, $010D, $00E7, $0107, $005B, $002E, $003C, $0028, $002B, $0021,
    $0026, $00E9, $0119, $00EB, $016F, $00ED, $00EE, $013E, $013A, $00DF, $005D, $0024, $002A, $0029, $003B, $005E,
    $002D, $002F, $00C2, $00C4, $02DD, $00C1, $0102, $010C, $00C7, $0106, $007C, $002C, $0025, $005F, $003E, $003F,
    $02C7, $00C9, $0118, $00CB, $016E, $00CD, $00CE, $013D, $0139, $0060, $003A, $0023, $0040, $0027, $003D, $0022,
    $02D8, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $015B, $0148, $0111, $00FD, $0159, $015F,
    $00B0, $006A, $006B, $006C, $006D, $006E, $006F, $0070, $0071, $0072, $0142, $0144, $0161, $00B8, $02DB, $00A4,
    $0105, $007E, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $015A, $0147, $0110, $00DD, $0158, $015E,
    $02D9, $0104, $017C, $0162, $017B, $00A7, $017E, $017A, $017D, $0179, $0141, $0143, $0160, $00A8, $00B4, $00D7,
    $007B, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $00AD, $00F4, $00F6, $0155, $00F3, $0151,
    $007D, $004A, $004B, $004C, $004D, $004E, $004F, $0050, $0051, $0052, $011A, $0171, $00FC, $0165, $00FA, $011B,
    $005C, $00F7, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $010F, $00D4, $00D6, $0154, $00D3, $0150,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $010E, $0170, $00DC, $0164, $00DA, $009F);
  CP874ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $0082, $0083, $0084, $2026, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0E01, $0E02, $0E03, $0E04, $0E05, $0E06, $0E07, $0E08, $0E09, $0E0A, $0E0B, $0E0C, $0E0D, $0E0E, $0E0F,
    $0E10, $0E11, $0E12, $0E13, $0E14, $0E15, $0E16, $0E17, $0E18, $0E19, $0E1A, $0E1B, $0E1C, $0E1D, $0E1E, $0E1F,
    $0E20, $0E21, $0E22, $0E23, $0E24, $0E25, $0E26, $0E27, $0E28, $0E29, $0E2A, $0E2B, $0E2C, $0E2D, $0E2E, $0E2F,
    $0E30, $0E31, $0E32, $0E33, $0E34, $0E35, $0E36, $0E37, $0E38, $0E39, $0E3A, $F8C1, $F8C2, $F8C3, $F8C4, $0E3F,
    $0E40, $0E41, $0E42, $0E43, $0E44, $0E45, $0E46, $0E47, $0E48, $0E49, $0E4A, $0E4B, $0E4C, $0E4D, $0E4E, $0E4F,
    $0E50, $0E51, $0E52, $0E53, $0E54, $0E55, $0E56, $0E57, $0E58, $0E59, $0E5A, $0E5B, $F8C5, $F8C6, $F8C7, $F8C8);
  CP875ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $009C, $0009, $0086, $007F, $0097, $008D, $008E, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $009D, $0085, $0008, $0087, $0018, $0019, $0092, $008F, $001C, $001D, $001E, $001F,
    $0080, $0081, $0082, $0083, $0084, $000A, $0017, $001B, $0088, $0089, $008A, $008B, $008C, $0005, $0006, $0007,
    $0090, $0091, $0016, $0093, $0094, $0095, $0096, $0004, $0098, $0099, $009A, $009B, $0014, $0015, $009E, $001A,
    $0020, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $005B, $002E, $003C, $0028, $002B, $0021,
    $0026, $039A, $039B, $039C, $039D, $039E, $039F, $03A0, $03A1, $03A3, $005D, $0024, $002A, $0029, $003B, $005E,
    $002D, $002F, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $007C, $002C, $0025, $005F, $003E, $003F,
    $00A8, $0386, $0388, $0389, $00A0, $038A, $038C, $038E, $038F, $0060, $003A, $0023, $0040, $0027, $003D, $0022,
    $0385, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6,
    $00B0, $006A, $006B, $006C, $006D, $006E, $006F, $0070, $0071, $0072, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC,
    $00B4, $007E, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3,
    $00A3, $03AC, $03AD, $03AE, $03CA, $03AF, $03CC, $03CD, $03CB, $03CE, $03C2, $03C4, $03C5, $03C6, $03C7, $03C8,
    $007B, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $00AD, $03C9, $0390, $03B0, $2018, $2015,
    $007D, $004A, $004B, $004C, $004D, $004E, $004F, $0050, $0051, $0052, $00B1, $00BD, $001A, $0387, $2019, $00A6,
    $005C, $001A, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00B2, $00A7, $001A, $001A, $00AB, $00AC,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $00B3, $00A9, $001A, $001A, $00BB, $009F);
  CP1250ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0083, $201E, $2026, $2020, $2021, $0088, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
    $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7, $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
    $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7, $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);
  CP1251ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021, $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7, $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7, $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F);
  CP1252ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $008D, $017D, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $009D, $017E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);
  CP1253ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $0088, $2030, $008A, $2039, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $009A, $203A, $009C, $009D, $009E, $009F,
    $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $F8F9, $00AB, $00AC, $00AD, $00AE, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $F8FA, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $F8FB);
  CP1254ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $009D, $009E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);
  CP1255ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $008A, $2039, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $009A, $203A, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
    $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7, $05B8, $05B9, $05BA, $05BB, $05BC, $05BD, $05BE, $05BF,
    $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $05F2, $05F3, $05F4, $F88D, $F88E, $F88F, $F890, $F891, $F892, $F893,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $F894, $F895, $200E, $200F, $F896);
  CP1256ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
    $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
    $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $06BE, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
    $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7, $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
    $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7, $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
    $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7, $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2);
  CP1257ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0083, $201E, $2026, $2020, $2021, $0088, $2030, $008A, $2039, $008C, $00A8, $02C7, $00B8,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $009A, $203A, $009C, $00AF, $02DB, $009F,
    $00A0, $F8FC, $00A2, $00A3, $00A4, $F8FD, $00A6, $00A7, $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112, $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7, $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113, $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7, $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9);
  CP1258ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $008A, $2039, $0152, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $009A, $203A, $0153, $009D, $009E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $0102, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $0300, $00CD, $00CE, $00CF,
    $0110, $00D1, $0309, $00D3, $00D4, $01A0, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $01AF, $0303, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $0301, $00ED, $00EE, $00EF,
    $0111, $00F1, $0323, $00F3, $00F4, $01A1, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $01B0, $20AB, $00FF);
  CP10000ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C4, $00C5, $00C7, $00C9, $00D1, $00D6, $00DC, $00E1, $00E0, $00E2, $00E4, $00E3, $00E5, $00E7, $00E9, $00E8,
    $00EA, $00EB, $00ED, $00EC, $00EE, $00EF, $00F1, $00F3, $00F2, $00F4, $00F6, $00F5, $00FA, $00F9, $00FB, $00FC,
    $2020, $00B0, $00A2, $00A3, $00A7, $2022, $00B6, $00DF, $00AE, $00A9, $2122, $00B4, $00A8, $2260, $00C6, $00D8,
    $221E, $00B1, $2264, $2265, $00A5, $00B5, $2202, $2211, $220F, $03C0, $222B, $00AA, $00BA, $2126, $00E6, $00F8,
    $00BF, $00A1, $00AC, $221A, $0192, $2248, $2206, $00AB, $00BB, $2026, $00A0, $00C0, $00C3, $00D5, $0152, $0153,
    $2013, $2014, $201C, $201D, $2018, $2019, $00F7, $25CA, $00FF, $0178, $2044, $20AC, $2039, $203A, $FB01, $FB02,
    $2021, $00B7, $201A, $201E, $2030, $00C2, $00CA, $00C1, $00CB, $00C8, $00CD, $00CE, $00CF, $00CC, $00D3, $00D4,
    $F8FF, $00D2, $00DA, $00DB, $00D9, $0131, $02C6, $02DC, $00AF, $02D8, $02D9, $02DA, $00B8, $02DD, $02DB, $02C7);
  CP10029ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C4, $0100, $0101, $00C9, $0104, $00D6, $00DC, $00E1, $0105, $010C, $00E4, $010D, $0106, $0107, $00E9, $0179,
    $017A, $010E, $00ED, $010F, $0112, $0113, $0116, $00F3, $0117, $00F4, $00F6, $00F5, $00FA, $011A, $011B, $00FC,
    $2020, $00B0, $0118, $00A3, $00A7, $2022, $00B6, $00DF, $00AE, $00A9, $2122, $0119, $00A8, $2260, $0123, $012E,
    $012F, $012A, $2264, $2265, $012B, $0136, $2202, $2211, $0142, $013B, $013C, $013D, $013E, $0139, $013A, $0145,
    $0146, $0143, $00AC, $221A, $0144, $0147, $2206, $00AB, $00BB, $2026, $00A0, $0148, $0150, $00D5, $0151, $014C,
    $2013, $2014, $201C, $201D, $2018, $2019, $00F7, $25CA, $014D, $0154, $0155, $0158, $2039, $203A, $0159, $0156,
    $0157, $0160, $201A, $201E, $0161, $015A, $015B, $00C1, $0164, $0165, $00CD, $017D, $017E, $016A, $00D3, $00D4,
    $016B, $016E, $00DA, $016F, $0170, $0171, $0172, $0173, $00DD, $00FD, $0137, $017B, $0141, $017C, $0122, $02C7);
  CP20107ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $00A4, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $00C9, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00C4, $00D6, $00C5, $00DC, $005F,
    $00E9, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $00E4, $00F6, $00E5, $00FC, $007F,
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $00A4, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $00C9, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00C4, $00D6, $00C5, $00DC, $005F,
    $00E9, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $00E4, $00F6, $00E5, $00FC, $007F);
  CP20866ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556, $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
    $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565, $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);
  CP20127ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F);
  CP21866ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457, $2557, $2558, $2559, $255A, $255B, $0491, $045E, $255E,
    $255F, $2560, $2561, $0401, $0404, $2563, $0406, $0407, $2566, $2567, $2568, $2569, $256A, $0490, $040E, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);
  CP28592ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7, $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7, $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);
  CP28593ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0126, $02D8, $00A3, $00A4, $F7F5, $0124, $00A7, $00A8, $0130, $015E, $011E, $0134, $00AD, $F7F6, $017B,
    $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7, $00B8, $0131, $015F, $011F, $0135, $00BD, $F7F7, $017C,
    $00C0, $00C1, $00C2, $F7F8, $00C4, $010A, $0108, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $F7F9, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7, $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
    $00E0, $00E1, $00E2, $F7FA, $00E4, $010B, $0109, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $F7FB, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7, $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9);
  CP28594ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7, $00A8, $0160, $0112, $0122, $0166, $00AD, $017D, $00AF,
    $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7, $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E, $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
    $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7, $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
    $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7, $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9);
  CP28595ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407, $0408, $0409, $040A, $040B, $040C, $00AD, $040E, $040F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457, $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F);
  CP28596ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $F7C8, $F7C9, $F7CA, $00A4, $F7CB, $F7CC, $F7CD, $F7CE, $F7CF, $F7D0, $F7D1, $060C, $00AD, $F7D2, $F7D3,
    $F7D4, $F7D5, $F7D6, $F7D7, $F7D8, $F7D9, $F7DA, $F7DB, $F7DC, $F7DD, $F7DE, $061B, $F7DF, $F7E0, $F7E1, $061F,
    $F7E2, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637, $0638, $0639, $063A, $F7E3, $F7E4, $F7E5, $F7E6, $F7E7,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $F7E8, $F7E9, $F7EA, $F7EB, $F7EC, $F7ED, $F7EE, $F7EF, $F7F0, $F7F1, $F7F2, $F7F3, $F7F4);
  CP28597ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $02BD, $02BC, $00A3, $F7C2, $F7C3, $00A6, $00A7, $00A8, $00A9, $F7C4, $00AB, $00AC, $00AD, $F7C5, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $F7C6, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $F7C7);
  CP28598ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $F79C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $203E,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $F79D,
    $F79E, $F79F, $F7A0, $F7A1, $F7A2, $F7A3, $F7A4, $F7A5, $F7A6, $F7A7, $F7A8, $F7A9, $F7AA, $F7AB, $F7AC, $F7AD,
    $F7AE, $F7AF, $F7B0, $F7B1, $F7B2, $F7B3, $F7B4, $F7B5, $F7B6, $F7B7, $F7B8, $F7B9, $F7BA, $F7BB, $F7BC, $2017,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $F7BD, $F7BE, $F7BF, $F7C0, $F7C1);
  CP28599ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);
  CP28603ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $201D, $00A2, $00A3, $00A4, $201E, $00A6, $00A7, $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $201C, $00B5, $00B6, $00B7, $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112, $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7, $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113, $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7, $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $2019);
  CP28605ToUnicodeMap: packed array[$00..$FF] of Word = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AC, $00A5, $0160, $00A7, $0161, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $017D, $00B5, $00B6, $00B7, $017E, $00B9, $00BA, $00BB, $0152, $0153, $0178, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);
{$ENDIF}
{windows unsupported CP's}
  CP28600ToUnicodeMap: packed array[$00..$FF] of Word = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7, $013B, $0110, $0160, $0166, $017D, $00AD, $016A, $014A,
    $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7, $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E, $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,
    $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168, $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169, $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138);
  CP28604ToUnicodeMap: packed array[$00..$FF] of Word = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7, $1E80, $00A9, $1E82, $1E0B, $1EF2, $00AD, $00AE, $0178,
    $1E1E, $1E1F, $0120, $0121, $1E40, $1E41, $00B6, $1E56, $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84, $1E85, $1E61,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0174, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $0176, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $0177, $00FF);
  CP28606ToUnicodeMap: packed array[$00..$FF] of Word = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0105, $0141, $20AC, $201E, $0160, $00A7, $0161, $00A9, $0218, $00AB, $0179, $00AD, $017A, $017B,
    $00B0, $00B1, $010C, $0142, $017D, $201D, $00B6, $00B7, $017E, $010D, $0219, $00BB, $0152, $0153, $0178, $017C,
    $00C0, $00C1, $00C2, $0102, $00C4, $0106, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0110, $0143, $00D2, $00D3, $00D4, $0150, $00D6, $015A, $0170, $00D9, $00DA, $00DB, $00DC, $0118, $021A, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $0107, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0111, $0144, $00F2, $00F3, $00F4, $0151, $00F6, $015B, $0171, $00F9, $00FA, $00FB, $00FC, $0119, $021B, $00FF);

implementation

uses SysUtils;

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
  case ZDetectUTF8Encoding(S) of
    etUSASCII: Result := USASCII7ToUnicodeString(S);
    etUTF8: Result := UTF8ToString(S);
    else
      Result := ZWideString(S); //random success, we don't know the CP here
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
begin
  if Pointer(S) = nil then
    Result := ''
  else
    Result := PRawToUnicode(Pointer(S), {%H-}PLengthInt(NativeUInt(S) - StringLenOffSet)^, CP);
end;

procedure MapByteToUCS2(Source: PAnsichar; SourceBytes: NativeUInt; var dest: PWideChar);
var
  PEnd: PAnsiChar;
begin
  PEnd := Source+SourceBytes-8;
  {all Bytes[0..255] have a equivalient Word expression }
  while Source < PEnd do
  begin
    PWord(Dest)^ := PByte(Source)^;
    PWord(Dest+1)^ := PByte(Source+1)^;
    PWord(Dest+2)^ := PByte(Source+2)^;
    PWord(Dest+3)^ := PByte(Source+3)^;
    PWord(Dest+4)^ := PByte(Source+4)^;
    PWord(Dest+5)^ := PByte(Source+5)^;
    PWord(Dest+6)^ := PByte(Source+6)^;
    PWord(Dest+7)^ := PByte(Source+7)^;
    Inc(Dest, 8);
    Inc(Source, 8);
  end;
  Inc(PEnd, 8);
  while Source < PEnd do
  begin
    PWord(Dest)^ := Byte(Source^); //Shift Byte to Word
    inc(Source);
    inc(Dest);
  end;
end;

procedure AnsiSBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  var Dest: ZWideString; const SBCS_MAP: PSBCS_MAP);
var
  P: PWideChar;
  C: LongWord;
  PEnd: PAnsiChar;
begin
  {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
  if (Pointer(Dest) = nil) or//empty
     ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
     (SourceBytes <> NativeUInt({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^)) then { length as expected ? }
  {$ENDIF}
    System.SetLength(Dest, SourceBytes);
  P := Pointer(Dest);
  PEnd := Source+SourceBytes-4;
  while Source < PEnd do //convert remaining characters with codepage agnostic
  begin
    { quad ASCII conversion by SHA }
    if (PLongWord(Source)^ and $80808080 = 0) then  //all in range $00..$79 ASCII7
    begin
      C := PLongWord(Source)^;
      PLongWord(P)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      PLongWord(P+2)^ := (c shl 8 or c) and $00ff00ff;
    end
    else
    begin //we need a lookup here
      PWord(P)^ := SBCS_MAP^[PByte(Source)^];
      PWord(P+1)^ := SBCS_MAP^[PByte(Source+1)^];
      PWord(P+2)^ := SBCS_MAP^[PByte(Source+2)^];
      PWord(P+3)^ := SBCS_MAP^[PByte(Source+3)^];
    end;
    inc(Source,4);
    inc(P,4);
  end;
  Inc(PEnd, 4);
  while Source < PEnd do
  begin
    PWord(P)^ := SBCS_MAP^[PByte(Source)^];
    inc(P);
    inc(Source);
  end;
end;

procedure AnsiSBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  const MapProc: TSBCSMapProc; var Dest: ZWideString);
var
  P: PWideChar;
begin
  SetLength(Dest, SourceBytes);
  P := Pointer(Dest);
  MapProc(Source, SourceBytes, P);
end;

procedure AnsiMBCSToUCS2(Source: PAnsichar; SourceBytes: NativeUInt;
  const MapProc: TMBCSMapProc; var Dest: ZWideString);
var
  P: PWideChar;
begin
  SetLength(Dest, SourceBytes);
  P := Pointer(Dest);
  SetLength(Dest, MapProc(Source, SourceBytes, P));
end;

{ UTF8ToWideChar and its's used constant original written by Arnaud Bouchez
  see: syncommons.pas in mORMot framework www.synopse.info
  Changes:
    - replace PUTF8Char to PAnsichar
    - replace Returning bytes by WideChars and a LengthInt-Type
    - replace Dest^ := WideChar(c) casts by using PWord(Dest)^ := C; expresions
      which is imbelievable faster with FPC (they are spooling all such casts
      through the WideStringManager -> horrable performance drop )
    - omit StrLen determination we don't need here
    - add 4Byte ASCII quads with SHA optimization again if 00..7F was found in iteration loop
    - omit nil check of Value.p and dest since we use it only if we've real data
    - change arrays to packed arrays which are faster handled by ide
  Performance: faster than UTF8Decode but not faster than MultiByteToWideChar
  with D7 $ FPCso we exclude this function here
}
// some constants used for UTF-8 conversion, including surrogates
const
  UTF16_HISURROGATE_MIN = $d800;
  //UTF16_HISURROGATE_MAX = $dbff;
  UTF16_LOSURROGATE_MIN = $dc00;
  //UTF16_LOSURROGATE_MAX = $dfff;
  UTF8_EXTRABYTES: packed array[$80..$ff] of byte = (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,0,0);

  UTF8_EXTRA: packed array[0..6] of record
    offset, minimum: cardinal;
  end = ( // http://floodyberry.wordpress.com/2007/04/14/utf-8-conversion-tricks
    (offset: $00000000;  minimum: $00010000),
    (offset: $00003080;  minimum: $00000080),
    (offset: $000e2080;  minimum: $00000800),
    (offset: $03c82080;  minimum: $00010000),
    (offset: $fa082080;  minimum: $00200000),
    (offset: $82082080;  minimum: $04000000),
    (offset: $00000000;  minimum: $04000000));
  //UTF8_EXTRA_SURROGATE = 3;
  //UTF8_FIRSTBYTE: packed array[2..6] of byte = ($c0,$e0,$f0,$f8,$fc);

function UTF8ToWideChar(Source: PAnsichar; SourceBytes: NativeUInt; Dest: PWideChar): LengthInt;
{$IF defined (WIN32) and not (defined(FPC) or defined(UNICODE))}
//new Delphi's make same code -> terrific wheres FPC instruction set is twice longer!
//but pure pascal is almost faster than System.UTFDecode or MultiByteToWideChar
asm
  push ebx
  push esi
  push edi
  add esp,-$0c
  mov edi,ecx
//begin
  mov [esp],edi //begd := dest;
  add edx,eax //endSource := Source+SourceBytes;
  mov [esp+$04],edx
@@Loop:
  mov edx,[eax] //PCardinal(Source)^
  test edx,$80808080 //if (PCardinal(Source)^ and $80808080=0)
  jnz @@CheckByte //nope -> jump to byte compare
  lea ecx,[eax+$04] //Source+4
  cmp ecx,[esp+$04] //(Source+4<endSource)
  jnb @@CheckByte //nope -> jump to byte compare
  add eax,$04 //inc(Source,4)
//pCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
  mov ecx,edx
  shl ecx,$08 //c shl 8
  mov ebx,edx
  and ebx,$000000ff //(c and $FF)
  or ecx,ebx  //or
  and ecx,$00ff00ff //and $00ff00ff
  mov [edi],ecx
  shr edx,$10 //c := c shr 16;
  mov ecx,edx
  shl ecx,$08 //c shl 8
  or edx,ecx //or c
  and edx,$00ff00ff //and $00ff00ff
  lea ecx,[edi+$04] //pCardinal(dest+2)^
  mov [ecx],edx //save
  add edi,$08 //inc(dest,4);
  jmp @@Loop //start Loop again
@@CheckByte:
  movzx edx,[eax] //c := byte(Source^);
  inc eax //inc(Source)
  test dl,$80 //if c and $80=0
  jnz @@Extra //nope no ASCII7 goto Extra
  mov [edi],dx //PWord(dest)^ := c;
  add edi,$02 //inc(dest);
  cmp eax,[esp+$04] //if Source<endsource then
  jb @@Loop //start Loop again
  jmp @@Quit //quit
@@Extra:
//extra := UTF8_EXTRABYTES[c];
  movzx ecx, [edx+UTF8_EXTRABYTES-$80]
  mov [esp+$08],ecx
//if (extra=0) or (Source+extra>endSource) then break;
  cmp dword ptr [esp+$08],$00
  jz @@Quit
  mov ecx,[esp+$08]
  add ecx,eax
  cmp ecx,[esp+$04]
  jnbe @@Quit //break
//for i := 1 to extra do begin
  mov esi,[esp+$08]
  test esi,esi
  jle @@WITH_UTF8_EXTRA
//if byte(Source^) and $c0<>$80 then
@@CompareByte:
  movzx ecx,[eax]
  mov ebx,ecx
  and bl,$c0
  cmp bl,$80
  jnz @@Quit //nope invalid input content
//c := c shl 6+byte(Source^);
  shl edx,$06 //c shl 6
  movzx ecx,cl //save byte(Source^)
  inc eax //inc(Source)
  add edx,ecx //process c := c +byte
//for i := 1 to extra do begin
  dec esi
  jnz @@CompareByte
@@WITH_UTF8_EXTRA:
//with UTF8_EXTRA[extra] do begin
  mov ecx,[esp+$08]
  lea ecx,[ecx*8+UTF8_EXTRA]
  //lea ecx, dword ptr [UTF8_EXTRA+ebx*8]
  sub edx,[ecx] //dec(c,offset);
  cmp edx,[ecx+$04] //if c<minimum then
  jb @@Quit //nope wrong input content
  cmp edx,$0000ffff //if c<=$ffff then
  jnbe @@StoreSurrogate //nope goto Surrogate processing
  mov [edi],dx //PWord(dest)^ := c;
  add edi,$02 //inc(dest);
  cmp eax,[esp+$04] //if Source<endsource then
  jb @@Loop //start loop again
  jmp @@Quit //ready
@@StoreSurrogate:
  sub edx,$00010000 //dec(c,$10000); // store as UTF-16 surrogates
//PWord(dest)^ := c shr 10  +UTF16_HISURROGATE_MIN;
  mov ecx,edx
  shr ecx,$0a //c shr 10
  add cx,UTF16_HISURROGATE_MIN
  mov [edi],cx //PWord(dest)^ :=
  and dx,$03ff //c and $3FF
  add dx,UTF16_LOSURROGATE_MIN
  lea ecx,[edi+$02] //dest+1
  mov [ecx],dx //save to Dest
  add edi,$04 //inc(dest,2)
  cmp eax,[esp+$04] //if Source>=endsource then
  jb @@Loop //start loop again
@@Quit:
//result := dest-begd; // dest-begd return widechar length
  mov eax,edi
  sub eax,[esp]
  shr eax,1
@@NoSource:
  add esp,$0c
  pop edi
  pop esi
  pop ebx
  ret {do not remove, this is for the alignment }
  nop {do not remove, this is for the alignment }
end;
{$else}
// faster than System.UTF8Decode()
var c: cardinal;
    begd: pWideChar;
    endSource: PAnsiChar;
    i,extra: integer;
label Quit;
begin
  begd := dest;
  endSource := Source+SourceBytes;
  repeat
    // first handle 7 bit ASCII chars, by quad (Sha optimization)
    if (PCardinal(Source)^ and $80808080=0) and (Source+4<endSource) then
    begin
      c := pCardinal(Source)^;
      inc(Source,4);
      pCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      pCardinal(dest+2)^ := (c shl 8 or c) and $00ff00ff;
      inc(dest,4);
    end
    else
    begin
      c := byte(Source^);
      inc(Source);
      if c and $80=0 then begin
        PWord(dest)^ := c;
        inc(dest);
        if Source<endsource then
          continue else
          break;
      end;
      //handle UTF8 byte sequences by A.Bouches optimization
      extra := UTF8_EXTRABYTES[c];
      if (extra=0) or (Source+extra>endSource) then break;
      for i := 1 to extra do begin
        if byte(Source^) and $c0<>$80 then
          goto Quit; // invalid input content
        c := c shl 6+byte(Source^);
        inc(Source);
      end;
      with UTF8_EXTRA[extra] do begin
        dec(c,offset);
        if c<minimum then
          break; // invalid input content
      end;
      if c<=$ffff then begin
        PWord(dest)^ := c;
        inc(dest);
        if Source<endsource then
          continue else
          break;
      end;
      dec(c,$10000); // store as UTF-16 surrogates
      PWord(dest)^ := c shr 10  +UTF16_HISURROGATE_MIN;
      PWord(dest+1)^ := c and $3FF+UTF16_LOSURROGATE_MIN;
      inc(dest,2);
      if Source>=endsource then
        break;
    end;
  until false;
Quit:
  result := ({%H-}NativeUInt(dest)-{%H-}NativeUInt(begd)) shr 1; // dest-begd return codepoint length
end;
{$IFEND}

function PRawToUnicode(Source: PAnsiChar; const SourceBytes: NativeUInt;
  CP: Word): ZWideString;
var
  S: RawByteString;
  {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
  C: LongWord;
  PEnd: PAnsiChar;
  Dest: PWideChar;
  wlen: LengthInt;
  {$IFEND}
label A2U;
begin
  if SourceBytes = 0 then
    Result := ''
  else
A2U:
    case CP of
      zCP_NONE:
        case ZDetectUTF8Encoding(Source, SourceBytes) of
          etUSASCII: Result := USASCII7ToUnicodeString(Source, SourceBytes);
          etUTF8: AnsiMBCSToUCS2(Source, SourceBytes, UTF8ToWideChar, Result);
          else
            if ZCompatibleCodePages(ZDefaultSystemCodePage,zCP_UTF8) then
            begin
              ZSetString(Source, SourceBytes, S{%H-});
              Result := ZWideString(S); //random success, we don't know ANY proper CP here
            end
            else
            begin
              CP := ZDefaultSystemCodePage; //still a random success here!
              goto A2U;
            end;
        end;
      {$IFDEF USE_RAW2WIDE_PROCS} //compiler related: D7 is less optimal use MultibultToWidechar instead for known CP's
      //FPC code is less optimal ... very slow. Wasn't able to get a asm code running
      //so i left the code for Non-Mindows OS's which will benefit here. No LIBICONV link is required.
      //let's see what comming FPC 2.7.1 is able to do (if they have it's own codepage maps)
      //Delphi7 needs 10% of time the FPC requires whereas the UnicodeIDE are 60% faster than D7
      zCP_DOS437:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP437ToUnicodeMap);
      zCP_DOS708:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP708ToUnicodeMap);
      zCP_DOS720:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP720ToUnicodeMap);
      zCP_DOS737:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP737ToUnicodeMap);
      zCP_DOS775:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP775ToUnicodeMap);
      zCP_DOS850:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP850ToUnicodeMap);
      zCP_DOS852:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP852ToUnicodeMap);
      zCP_DOS857:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP857ToUnicodeMap);
      zCP_DOS858:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP858ToUnicodeMap);
      zCP_DOS860:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP860ToUnicodeMap);
      zCP_DOS861:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP861ToUnicodeMap);
      zCP_DOS862:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP862ToUnicodeMap);
      zCP_DOS863:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP863ToUnicodeMap);
      zCP_DOS864:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP864ToUnicodeMap);
      zCP_DOS865:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP865ToUnicodeMap);
      zCP_DOS866:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP866ToUnicodeMap);
      zCP_DOS869:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP869ToUnicodeMap);
      zCP_WIN874:   AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP874ToUnicodeMap);
      zCP_WIN1250:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1250ToUnicodeMap);
      zCP_WIN1251:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1251ToUnicodeMap);
      zCP_WIN1252:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1252ToUnicodeMap);
      zCP_WIN1253:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1253ToUnicodeMap);
      zCP_WIN1254:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1254ToUnicodeMap);
      zCP_WIN1255:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1255ToUnicodeMap);
      cCP_WIN1256:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1256ToUnicodeMap);
      zCP_WIN1257:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1257ToUnicodeMap);
      zCP_WIN1258:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP1258ToUnicodeMap);
      zCP_macintosh: AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP10000ToUnicodeMap);
      zCP_x_mac_ce: AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP10029ToUnicodeMap);
      zCP_x_IA5_Swedish:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP20107ToUnicodeMap);
      zCP_KOI8R:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP20866ToUnicodeMap);
      zCP_us_ascii: AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP20127ToUnicodeMap);
      zCP_KOI8U:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP21866ToUnicodeMap);
      zCP_L1_ISO_8859_1: AnsiSBCSToUCS2(Source, SourceBytes, MapByteToUCS2, Result);
      zCP_L2_ISO_8859_2:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28592ToUnicodeMap);
      zCP_L3_ISO_8859_3:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28593ToUnicodeMap);
      zCP_L4_ISO_8859_4:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28594ToUnicodeMap);
      zCP_L5_ISO_8859_5:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28595ToUnicodeMap);
      zCP_L6_ISO_8859_6:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28596ToUnicodeMap);
      zCP_L7_ISO_8859_7:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28597ToUnicodeMap);
      zCP_L8_ISO_8859_8:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28598ToUnicodeMap);
      zCP_L5_ISO_8859_9:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28599ToUnicodeMap);
      zCP_L7_ISO_8859_13:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28603ToUnicodeMap);
      zCP_L9_ISO_8859_15:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28605ToUnicodeMap);
      {$ENDIF USE_RAW2WIDE_PROCS}
      {not supported codepages by Windows MultiByteToWideChar}
      zCP_L6_ISO_8859_10:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28600ToUnicodeMap);
      zCP_L8_ISO_8859_14:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28604ToUnicodeMap);
      zCP_L10_ISO_8859_16:  AnsiSBCSToUCS2(Source, SourceBytes, Result, @CP28606ToUnicodeMap);
      (* remaing fast conversion for MBCS encodings
      zCP_MSWIN921 = 921;
      zCP_MSWIN923 = 923;
      zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
      zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
      zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
      zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}
      ZCP_JOHAB = 1361; {Korean (Johab)}
      zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}

      zCP_csISO2022JP = 50221;	{ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
      zCP_euc_JP_win = 51932; {EUC Japanese}
      zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
      zCP_euc_kr = 51949; {EUC Korean}
      zCP_GB18030 = 54936;	{Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
      zCP_UTF7 = 65000;
      *)
      zCP_UTF8: AnsiMBCSToUCS2(Source, SourceBytes, UTF8ToWideChar, Result);
      else
        {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
        begin
          PEnd := Source+SourceBytes-4;
          Result := ''; //speeds up SetLength x2
          SetLength(result, SourceBytes);
          Dest := Pointer(Result);
          {first handle leading ASCII if possible }
          while (Source < PEnd ) and (PLongWord(Source)^ and $80808080 = 0) do
          begin
            C := PLongWord(Source)^;
            PLongWord(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
            c := c shr 16;
            PLongWord(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
            inc(Source,4);
            inc(Dest,4);
          end;
          inc(PEnd, 4);
          while (Source < PEnd) and (PByte(Source)^ and $80 = 0) do
          begin
            PWord(Dest)^ := Byte(Source^); //Shift Byte to Word
            inc(Source);
            inc(Dest);
          end;
          if Source < PEnd then //convert remaining characters with codepage agnostic
          begin
            wlen :=  PEnd-Source; //elimainate already processed chars
            {$IFDEF WITH_UNICODEFROMLOCALECHARS}
            SetLength(Result, LengthInt(SourceBytes) - wlen + UnicodeFromLocaleChars(CP, 0, Source, wlen, Dest, wlen));
            {$ELSE}
            SetLength(Result, LengthInt(SourceBytes) - wlen + MultiByteToWideChar(CP, 0, Source, wlen, Dest, wlen)); //Convert Ansi to Wide with supported Chars
            {$ENDIF}
          end;
        end;
        {$ELSE}
          {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
          WidestringManager.Ansi2UnicodeMoveProc(Source, CP, Result, SourceBytes);
          {$ELSE}
          begin
            ZSetString(Source, SourceBytes, S);
            if ZCompatibleCodePages(CP, zCP_UTF8) then
              Result := UTF8Decode(S)
            else
              Result := ZWideString(S); //random success
          end;
          {$ENDIF}
        {$IFEND}
    end;
end;

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
begin
  {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER)}
  Result := PUnicodeToRaw(Pointer(US), Length(US), CP);
  {$ELSE}
    if ZCompatibleCodePages(CP, zCP_UTF8) then
      Result := UTF8Encode(US)
    else
      Result := RawByteString(US); //random success
  {$IFEND}
end;
{$ENDIF}

function PUnicodeToRaw(Source: PWideChar; CodePoints: NativeUInt; CP: Word): RawByteString;
{$IFDEF WITH_LCONVENCODING}
var
  US: ZWideString;
begin
  SetString(US, Value.P, Value.Len);
  Result := ZUnicodeToRaw(US, CP);
end;
{$ELSE}
{$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
var
  ulen: Integer;
{$IFEND}
begin
  Result := ''; //speed up setlength *2
  if CodePoints = 0 then
    Exit
  else
  begin
    if CP = zCP_NONE then
      CP := ZDefaultSystemCodePage; //random success
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    ULen := Min(Integer(CodePoints) * 4, High(Integer)-1);
    setlength(Result, ulen); //oversized
    {$IFDEF WITH_UNICODEFROMLOCALECHARS}
    SetLength(Result, LocaleCharsFromUnicode(CP, 0, Source, CodePoints, PAnsiChar(Result), ulen, NIL, NIL)); // Convert Unicode down to Ansi
    {$ELSE}
    SetLength(Result, WideCharToMultiByte(CP,0, Source, CodePoints, PAnsiChar(Result), ulen, nil, nil)); // Convert Wide down to Ansi
    {$ENDIF}
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
        WidestringManager.Unicode2AnsiMoveProc(Source, Result, CP, CodePoints);
      {$ELSE}
        if ZCompatibleCodePages(CP, zCP_UTF8) then
          Result := UTF8Encode(ZWideString(Source))
        else
          Result := RawByteString(Source); //random success
      {$ENDIF}
    {$IFEND}
  end;
end;
{$ENDIF}

{$IFNDEF UNICODE}
function PUnicodeToString(Source: PWideChar; CodePoints: NativeUInt; CP: Word): String;
{$IFDEF WITH_LCONVENCODING}
var
  US: ZWideString;
begin
  SetString(US, Source, CodePoints);
  Result := ZUnicodeToRaw(US, CP);
end;
{$ELSE}
  {$IFDEF MSWINDOWS}
  var
    ulen: Integer;
  {$ELSE}             //2013-10-13 mse
    {$IF defined(PWIDECHAR_IS_PUNICODECHAR) and not defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER)}
    var
      WS: UnicodeString;
    {$IFEND}
  {$ENDIF}
  begin
    if CP = zCP_NONE then
      CP := ZDefaultSystemCodePage; //random success
    Result := '';
    {$IFDEF MSWINDOWS}
    if CodePoints = 0 then Exit;
    ULen := Min(Integer(CodePoints) * 4, High(Integer)-1);
    setlength(Result, ulen); //oversized
    setlength(Result, WideCharToMultiByte(CP,0, Source, CodePoints, Pointer(Result), ulen, nil, nil)); // Convert Wide down to Ansi
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Unicode2AnsiMoveProc(Source, Result, CP, CodePoints);
      {$ELSE} //FPC 2.6 down
      SetString(WS, Source, CodePoints);
      if ZCompatibleCodePages(CP, zCP_UTF8) then
        Result := UTF8Encode(WS)
      else
        Result := String(WS); //random success according the CP
      {$ENDIF FPC_HAS_BUILTIN_WIDESTR_MANAGER}
    {$ENDIF MSWINDOWS}
  end;
  {$ENDIF WITH_LCONVENCODING}
{$ENDIF UNICODE}

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

function ZConvertRaw28591ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := ISO_8859_1ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw28591(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToISO_8859_1(PAnsiChar(Src));
end;

function ZConvertRaw28592ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := ISO_8859_2ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw28592(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToISO_8859_2(PAnsiChar(Src));
end;

function ZConvertRaw1250ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1250ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1250(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1250(PAnsiChar(Src));
end;

function ZConvertRaw1251ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1251ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1251(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1251(PAnsiChar(Src));
end;

function ZConvertRaw1252ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1252ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1252(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1252(PAnsiChar(Src));
end;

function ZConvertRaw1253ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1253ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1253(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1253(PAnsiChar(Src));
end;

function ZConvertRaw1254ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1254ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1254(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1254(PAnsiChar(Src));
end;

function ZConvertRaw1255ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1255ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1255(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1255(PAnsiChar(Src));
end;

function ZConvertRaw1256ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1256ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1256(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1256(PAnsiChar(Src));
end;

function ZConvertRaw1257ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1257ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1257(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1257(PAnsiChar(Src));
end;

function ZConvertRaw1258ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP1258ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw1258(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP1258(PAnsiChar(Src));
end;

function ZConvertRaw437ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP437ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw437(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP437(PAnsiChar(Src));
end;

function ZConvertRaw850ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP850ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw850(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP850(PAnsiChar(Src));
end;

{$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
function ZConvertRaw852ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP852ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw852(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP852(PAnsiChar(Src));
end;
{$ENDIF}

function ZConvertRaw866ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP866ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw866(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP866(PAnsiChar(Src));
end;

function ZConvertRaw874ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := CP874ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw874(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToCP874(PAnsiChar(Src));
end;

function ZConvertRaw20866ToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  Result := KOI8ToUTF8(PAnsiChar(Src));
end;

function ZConvertUTF8ToRaw20866(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  Result := UTF8ToKOI8(PAnsiChar(Src));
end;

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

procedure SetDefaultSystemCodePage;
begin
  {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
  ZDefaultSystemCodePage := Word(DefaultSystemCodePage);
  {$ELSE}
    {$IFDEF MSWINDOWS}
    ZDefaultSystemCodePage := GetACP; //available for Windows and WinCE
    {$ELSE}
    ZDefaultSystemCodePage := zCP_UTF8; //how to determine the current OS CP?
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
  Result := (CP1 = CP2) or ((CP1 = zCP_us_ascii) or (CP2 = zCP_us_ascii)) or
    (((CP1 = zCP_UTF16) or (CP1 = zCP_UTF16BE)) and ((CP2 = zCP_UTF16) or (CP2 = zCP_UTF16BE)));
end;

function ZConvertPRawToUTF8(const Src: PAnsiChar; Len: NativeUInt; const RawCP: Word): UTF8String;
var US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if (Src = nil) or (Len = 0) then
    Result := ''
  else
  begin
    US := PRawToUnicode(Src, Len, RawCP);
    {$IFDEF WITH_RAWBYTESTRING}
    Result := UTF8String(US);
    {$ELSE}
    Result := UTF8Encode(US);
    {$ENDIF}
  end;
end;

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
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := ZRawToUnicode(Src, CP);
    Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(US);
  end;
end;

function ZConvertUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
var
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
begin
  if Src = '' then
    Result := ''
  else
  begin
    US := {$IFDEF WITH_RAWBYTESTRING}ZWideString{$ELSE}UTF8Decode{$ENDIF}(Src);
    Result := ZUnicodeToRaw(US, CP);
  end;
end;

function ZConvertRawToString(const Src: RawByteString;
  const RawCP, StringCP: Word): String;
{$IF not defined(UNICODE) and not defined(WITH_LCONVENCODING)}
var
  US: ZWideString; //COM based. So localize the String to avoid Buffer overrun
  {$IFDEF WITH_RAWBYTESTRING}
  Raw: RawByteString;
  {$ENDIF}
{$IFEND}
begin
  if Src = '' then
    Result := ''
  else
  begin
    {$IFDEF WITH_LCONVENCODING}
    case RawCP of
      28591: //ISO_8859_1
        Result := ISO_8859_1ToUTF8(Src);
      28592:  //ISO_8859_2
        Result := ISO_8859_2ToUTF8(Src);
      1250: //WIN1250
        Result := CP1250ToUTF8(Src);
      1251: //WIN1251
        Result := CP1251ToUTF8(Src);
      1252: //WIN1252
        Result := CP1252ToUTF8(Src);
      1253: //WIN1253
        Result := CP1253ToUTF8(Src);
      1254: //WIN1254
        Result := CP1254ToUTF8(Src);
      1255: //WIN1255
        Result := CP1255ToUTF8(Src);
      1256: //WIN1256
        Result := CP1256ToUTF8(Src);
      1257: //WIN1257
        Result := CP1257ToUTF8(Src);
      1258: //WIN1258
        Result := CP1258ToUTF8(Src);
      437: //CP437
        Result := CP437ToUTF8(Src);
      850: //CP850
        Result := CP850ToUTF8(Src);
      {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
      852: //CP852
        Result := CP852ToUTF8(Src);
      {$ENDIF}
      866: //CP866
        Result := CP866ToUTF8(Src);
      874: //CP874
        Result := CP874ToUTF8(Src);
      20866: //KOI8 (Russian)
        Result := KOI8ToUTF8(Src);
      65001: //utf8
        Result := Src;
      else
        Result := Src;
    end;
    {$ELSE}
      {$IFDEF UNICODE}
      Result := ZRawToUnicode(Src, RawCP);
      {$ELSE}
        US := ZRawToUnicode(Src, RawCP);
        {$IFDEF WITH_RAWBYTESTRING}
        Raw := ZUnicodeToRaw(US, StringCP);
        ZSetString(Pointer(Raw), {%H-}PLengthInt(NativeUInt(Raw) - StringLenOffSet)^, Result);
        {$ELSE}
        Result := ZUnicodeToRaw(US, StringCP);
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;
end;

function ZConvertStringToRaw(const Src: String; const StringCP, RawCP: Word): RawByteString;
{$IF not defined(UNICODE) and not defined(WITH_LCONVENCODING)}
var
  US: ZWideString; //COM based, so let's localize the value to avoid Buffer overrun
{$IFEND}
begin
  if Src = '' then
    Result := ''
  else
  {$IFDEF WITH_LCONVENCODING}
  begin
    case RawCP of
      28591: //ISO_8859_1
        Result := UTF8ToISO_8859_1(Src);
      28592:  //ISO_8859_2
        Result := UTF8ToISO_8859_2(Src);
      1250: //WIN1250
        Result := UTF8ToCP1250(Src);
      1251: //WIN1251
        Result := UTF8ToCP1251(Src);
      1252: //WIN1252
        Result := UTF8ToCP1252(Src);
      1253: //WIN1253
        Result := UTF8ToCP1253(Src);
      1254: //WIN1254
        Result := UTF8ToCP1254(Src);
      1255: //WIN1255
        Result := UTF8ToCP1255(Src);
      1256: //WIN1256
        Result := UTF8ToCP1256(Src);
      1257: //WIN1257
        Result := UTF8ToCP1257(Src);
      1258: //WIN1258
        Result := UTF8ToCP1258(Src);
      437: //CP437
        Result := UTF8ToCP437(Src);
      850: //CP850
        Result := UTF8ToCP850(Src);
      {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
      852: //CP852
        Result := UTF8ToCP852(Src);
      {$ENDIF}
      866: //CP866
        Result := UTF8ToCP866(Src);
      874: //CP874
        Result := UTF8ToCP874(Src);
      20866: //KOI8 (Russian)
        Result := UTF8ToKOI8(Src);
      65001: //UTF8
        Result := Src;
      else
        Result := Src;
    end;
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

function ZConvertStringToRawWithAutoEncode(const Src: String;
  const StringCP, RawCP: Word): RawByteString;
{$IFNDEF UNICODE}
var WS: ZWideString; //prevent possible overflow for COM based WideString
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  {$IFDEF UNICODE}
  Result := ZUnicodeToRaw(Src, RawCP);
  {$ELSE !UNICODE}
  case ZDetectUTF8Encoding(Src) of
    etUSASCII:
      {$IFDEF WITH_RAWBYTESTRING}
      ZSetString(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^ , Result);
      {$ELSE !WITH_RAWBYTESTRING}
      Result := Src;
      {$ENDIF WITH_RAWBYTESTRING}
    etAnsi:
      if (RawCP = zCP_UTF8) then
        if ZCompatibleCodePages(StringCP, zCP_UTF8 ) then
        begin
          WS := ZWideString(Src);
          Result := ZUnicodeToRaw(WS, RawCP) //Random success unknown String CP
        end
        else
          Result := ZConvertStringToRaw(Src, StringCP, RawCP)
      else
        {$IFDEF WITH_RAWBYTESTRING}
        ZSetString(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, Result);
        {$ELSE !WITH_RAWBYTESTRING}
        Result := Src;
        {$ENDIF WITH_RAWBYTESTRING}
    else //etUTF8:
      if (RawCP = zCP_UTF8) then
        {$IFDEF WITH_RAWBYTESTRING}
        ZSetString(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, Result)
        {$ELSE !WITH_RAWBYTESTRING}
        Result := Src
        {$ENDIF WITH_RAWBYTESTRING}
      else
        Result := ZConvertStringToRaw(Src, zCP_UTF8, RawCP);
  end;
  {$ENDIF UNICODE}
end;

function ZConvertUTF8ToString(const Src: UTF8String;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var
  US: ZWideString; //COM based. Localize the Value to avoid buffer overrun
  {$IFDEF WITH_RAWBYTESTRING}
  S: RawByteString;
  {$ENDIF}
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := String(Src);
    {$ELSE}
    begin
      Result := ''; //Makes Compiler happy
      {$IFDEF WITH_RAWBYTESTRING}
      US := ZWideString(Src);
      S := ZUnicodeToRaw(US, StringCP);
      ZSetString(Pointer(S), {%H-}PLengthInt(NativeUInt(S) - StringLenOffSet)^, Result);
      {$ELSE}
      US := UTF8Decode(Src);
      Result := ZUnicodeToRaw(US, StringCP);
      {$ENDIF}
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
      Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(US);
    end;
    {$ENDIF}
end;

function ZConvertStringToUTF8WithAutoEncode(const Src: String;
  const StringCP: Word): UTF8String;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
  {$IFDEF UNICODE}
    Result := UTF8String(Src);
 {$ELSE}
    If ZDetectUTF8Encoding(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^) in [etUSASCII, etUTF8] then
      {$IFDEF WITH_RAWBYTESTRING}
      ZSetString(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, Result)
      {$ELSE}
      Result := Src
      {$ENDIF}
    else //Ansi
    begin
      if ZCompatibleCodePages(StringCP, zCP_UTF8)  then
        Tmp := ZWideString(Src)
      else
        Tmp := PRawToUnicode(Pointer(Src),
          {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, StringCP);
      Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(Tmp);
    end;
  {$ENDIF}
end;

function ZConvertStringToAnsi(const Src: String;
  const StringCP: Word): AnsiString;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := AnsiString(Src);
    {$ELSE}
    Tmp := PRawToUnicode(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, StringCP);
    Result := AnsiString(Tmp);
    {$ENDIF}
end;

function ZConvertStringToAnsiWithAutoEncode(const Src: String;
  const StringCP: Word): AnsiString;
{$IFNDEF UNICODE}
var Tmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := AnsiString(Src);
    {$ELSE}
    case ZDetectUTF8Encoding(Pointer(Src), {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^) of
      etUSASCII: Result := Src;
      etAnsi:
        if ZDefaultSystemCodePage = zCP_UTF8 then
        begin
          Tmp := ZWideString(Src);
          Result := UTF8Encode(Src);
        end
        else
          Result := Src;
      else
        if ZDefaultSystemCodePage = zCP_UTF8 then
          Result := Src
        else
        begin
          Tmp := PRawToUnicode(Pointer(Src),
            {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, zCP_UTF8);
          Result := AnsiString(Tmp);
        end;
    end;
    {$ENDIF}
end;

function ZConvertAnsiToString(const Src: AnsiString;
  const StringCP: Word): String;
{$IFNDEF UNICODE}
var
  UniTmp: ZWideString; //COM based. Localize the Value to avoid buffer overrun
  {$IFDEF WITH_RAWBYTESTRING}
  RawTemp: RawByteString;
  {$ENDIF}
{$ENDIF}
begin
  if Src = '' then
    Result := ''
  else
    {$IFDEF UNICODE}
    Result := String(Src);
    {$ELSE}
    begin
      UniTmp := PRawToUnicode(Pointer(Src),
        {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^, ZDefaultSystemCodePage);
      {$IFDEF WITH_RAWBYTESTRING}
      RawTemp := ZUnicodeToRaw(UniTmp, StringCP);
      ZSetString(Pointer(RawTemp), {%H-}PLengthInt(NativeUInt(RawTemp) - StringLenOffSet)^, Result);
      {$ELSE !WITH_RAWBYTESTRING}
      Result := ZUnicodeToRaw(UniTmp, StringCP);
      {$ENDIF WITH_RAWBYTESTRING}
    end;
    {$ENDIF}
end;

function ZConvertUnicodeToString(const Src: ZWideString;
  const StringCP: Word): String;
{$IF not defined(UNICODE) and defined (WITH_RAWBYTESTRING)}
var Tmp: RawByteString;
{$IFEND}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  if Src = '' then
    Result := ''
  else
    {$IFDEF WITH_RAWBYTESTRING}
    begin
      Tmp := ZUnicodeToRaw(Src, StringCP);
      ZSetString(Pointer(Tmp), {%H-}PLengthInt(NativeUInt(tmp) - StringLenOffSet)^, Result{%H-});
    end;
    {$ELSE WITH_RAWBYTESTRING}
    Result := ZUnicodeToRaw(Src, StringCP);
    {$ENDIF WITH_RAWBYTESTRING}
  {$ENDIF}
end;

function ZConvertUnicodeToString_CPUTF8(const Src: ZWideString;
  const StringCP: Word): String;
{$IF not defined(UNICODE) and defined (WITH_RAWBYTESTRING)}
var Tmp: RawByteString;
{$IFEND}
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  if Src = '' then
    Result := ''
  else
    {$IFDEF WITH_RAWBYTESTRING}
    begin
      Tmp := UTF8Encode(Src);
      ZSetString(Pointer(Tmp), {%H-}PLengthInt(NativeUInt(tmp) - StringLenOffSet)^, Result);
    end;
    {$ELSE !WITH_RAWBYTESTRING}
    Result := UTF8Encode(Src);
    {$ENDIF}
  {$ENDIF}
end;

function ZConvertStringToUnicode(const Src: String;
  const StringCP: Word): ZWideString;
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  if Src = '' then
    Result := ''
  else
  begin
    Result := PRawToUnicode(Pointer(Src), {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^, StringCP);
  end;
  {$ENDIF}
end;

function ZConvertString_CPUTF8ToUnicode(const Src: String;
  const StringCP: Word): ZWideString;
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  if Src = '' then
    Result := ''
  else
    Result := PRawToUnicode(Pointer(Src),
      {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^, zCP_UTF8);
  {$ENDIF}
end;


function ZConvertStringToUnicodeWithAutoEncode(const Src: String;
  const StringCP: Word): ZWideString;
begin
  {$IFDEF UNICODE}
  Result := Src;
  {$ELSE}
  if Src = '' then
    Result := ''
  else
  begin
    case ZDetectUTF8Encoding(Pointer(Src), {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^) of
      etUSASCII: Result := USASCII7ToUnicodeString(Pointer(Src),
        {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^);
      etUTF8: Result := PRawToUnicode(Pointer(Src),
        {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^, zCP_UTF8);
      else
        if ZCompatibleCodePages(StringCP, zCP_UTF8)  then
          Result := ZWideString(Src) //random success!
        else
          Result := PRawToUnicode(Pointer(Src), {%H-}PLengthInt(NativeUInt(src) - StringLenOffSet)^, StringCP);
    end;
  end;
  {$ENDIF}
end;


function ZMovePRawToUTF8(const Src: PAnsiChar; Len: NativeUInt; const RawCP: Word): UTF8String;
begin
  ZSetString(Src, Len, Result{%H-});
end;

function ZMoveAnsiToRaw(const Src: AnsiString; const RawCP: Word): RawByteString;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  ZSetString(Pointer(Src), Length(Src), Result{%H-});
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveRawToAnsi(const Src: RawByteString; const RawCP: Word): AnsiString;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  System.SetString(Result, PAnsiChar(Pointer(Src)), Length(Src));
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveAnsiToUTF8(const Src: AnsiString): UTF8String;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  ZSetString(Pointer(Src), Length(Src), Result{%H-});
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveUTF8ToAnsi(const Src: UTF8String): AnsiString;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  System.SetString(Result, PAnsiChar(Src), Length(Src));
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveRawToUTF8(const Src: RawByteString; const CP: Word): UTF8String;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  ZSetString(Pointer(Src), Length(Src), Result{%H-});
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveUTF8ToRaw(Const Src: UTF8String; const CP: Word): RawByteString;
begin
  {$IFDEF WITH_RAWBYTESTRING}
  ZSetString(Pointer(Src), Length(Src), Result{%H-});
  {$ELSE}
  Result := Src;
  {$ENDIF}
end;

function ZMoveStringToAnsi(Const Src: String; const StringCP: Word): AnsiString;
begin
  {$IFDEF UNICODE}
  Result := AnsiString(Src);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
end;

function ZMoveAnsiToString(const Src: AnsiString; const StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := String(Src);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
end;


function ZMoveRawToString(const Src: RawByteString;
  const RawCP, StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := ZRawToUnicode(Src, RawCP);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
end;

function ZMoveStringToRaw(const Src: String;
  const StringCP, RawCP: Word): RawByteString;
begin
  {$IFDEF UNICODE}
  Result := ZUnicodeToRaw(Src, RawCP);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
end;

function ZMoveUTF8ToString(const Src: UTF8String; StringCP: Word): String;
begin
  {$IFDEF UNICODE}
  Result := String(Src);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
end;

function ZMoveStringToUTF8(const Src: String; const StringCP: Word): UTF8String;
begin
  {$IFDEF UNICODE}
  Result := UTF8String(Src);
  {$ELSE}
    {$IFDEF WITH_RAWBYTESTRING}
    ZSetString(Pointer(Src), Length(Src), Result{%H-});
    {$ELSE}
    Result := Src;
    {$ENDIF}
  {$ENDIF}
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
  ConSettings^.ConvFuncs.ZPRawToUTF8 := nil;

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

  { PRaw to UTTF8 string}
  if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, zCP_UTF8) and
    ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
    ConSettings^.ConvFuncs.ZPRawToUTF8 := @ZMovePRawToUTF8
  else
    ConSettings^.ConvFuncs.ZPRawToUTF8 := @ZConvertPRawToUTF8;

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
      {$IFDEF WITH_LCONVENCODING}
      case ConSettings^.ClientCodePage^.CP of
        28591:  //ISO_8859_1
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw28591ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw28591;
          end;
        28592:  //ISO_8859_2
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw28592ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw28592;
          end;
        1250:   //WIN1250
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1250ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1250;
          end;
        1251:   //WIN1251
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1251ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1251;
          end;
        1252:   //WIN1252
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1252ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1252;
          end;
        1253:   //WIN1253
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1253ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1253;
          end;
        1254:   //WIN1254
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1254ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1254;
          end;
        1255:   //WIN1255
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1255ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1255;
          end;
        1256:   //WIN1256
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1256ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1256;
          end;
        1257:   //WIN1257
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1257ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1257;
          end;
        1258:   //WIN1258
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw1258ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw1258;
          end;
        437:    //CP437
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw437ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw437;
          end;
        850:    //CP850
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw850ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw850;
          end;
        {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
        852:    //CP852
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw852ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw852;
          end;
        {$ENDIF}
        866:    //CP866
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw866ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw866;
          end;
        874:    //CP874
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw874ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw874;
          end;
        20866:   //KOI8 (Russian)
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRaw20866ToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw20866;
          end;
        else
          begin
            ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRawToUTF8;
            ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw;
          end;
      end;
      {$ELSE}
      ConSettings^.ConvFuncs.ZRawToUTF8 := @ZConvertRawToUTF8;
      ConSettings^.ConvFuncs.ZUTF8ToRaw := @ZConvertUTF8ToRaw;
      {$ENDIF}
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

function ZDetectUTF8Encoding(Source: PAnsiChar; Len: NativeUInt): TEncodeType;
var
  c : Byte;
  EndPtr: PAnsichar;
begin
  Result := etUSASCII;
  if (Source = nil) or (Len = 0) then Exit;

  EndPtr := Source + Len;

  // skip leading US-ASCII part.
  while Source+SizeOf(UInt64) <= EndPtr do //Check first Octet block
  begin
    if PUInt64(Source)^ and $8080808080808080<>0 then Break; //break on first non USASCII sequence
    inc(Source, SizeOf(UInt64));
  end;

  while Source+SizeOf(LongWord) <= EndPtr do //Check next quad
  begin
    if PLongWord(Source)^ and $80808080<>0 then Break; //break on first non USASCII sequence
    inc(Source, SizeOf(LongWord));
  end;

  while Source < EndPtr do //Check bytes
  begin
    if Byte(Source^) >= $80 then break; //break on first non USASCII sequence
    inc(Source);
  end;

  // If all character is US-ASCII, done.
  if Source = EndPtr then exit;

  while Source < EndPtr do
  begin
    c := Byte(Source^);
    case c of
      $00..$7F:  //Ascii7
        if EndPtr - Source > SizeOf(UInt64) then
          if PUInt64(Source)^ and $8080808080808080 = 0 then //Check octet block ASCII again
            inc(Source, SizeOf(UInt64))
          else if PLongWord(Source)^ and $80808080 = 0 then //Check quad block ASCII again
            inc(Source, SizeOf(PLongWord))
          else Inc(Source)
        else
          if EndPtr - Source > SizeOf(PLongWord) then
            if PLongWord(Source)^ and $80808080 = 0 then //Check quad block ASCII again
              inc(Source, SizeOf(PLongWord))
            else
              Inc(Source)
          else
            inc(Source);
      $C2..$DF:  // non-overlong 2-byte
        if (Source+1 < EndPtr)
            and (Byte((Source+1)^) in [$80..$BF]) then
          Inc(Source, 2)
        else
          break;

      $E0: // excluding overlongs
        if (Source+2 < EndPtr)
            and (Byte((Source+1)^) in [$A0..$BF])
            and (Byte((Source+2)^) in [$80..$BF]) then
          Inc(Source, 3)
        else
          break;

      $E1..$EF: // straight 3-byte & excluding surrogates
        if (Source+2 < EndPtr)
            and (Byte((Source+1)^) in [$80..$BF])
            and (Byte((Source+2)^) in [$80..$BF]) then
          Inc(Source, 3)
        else
          break;

      $F0: // planes 1-3
        if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$90..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF]) then
          Inc(Source, 4)
        else
          break;

      $F1..$F3:
        if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$80..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF]) then
          Inc(Source, 4)
        else
          break;

      $F4:
        if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$80..$8F])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF]) then
          Inc(Source, 4)
        else
          break;
    else
      break;
    end;
  end;

  if Source = EndPtr then Result := etUTF8
  else Result := etANSI;
end;

function ZDetectUTF8Encoding(const Source: RawByteString): TEncodeType;
begin
  Result := ZDetectUTF8Encoding(Pointer(Source), Length(Source));
end;

function USASCII7ToUnicodeString(Source: PAnsiChar; Len: NativeUInt): ZWideString; overload;
var C: LongWord;
  Dest: PWideChar;
begin
  SetString(Result, nil, Len);
  Dest := Pointer(Result);
  {fast quad conversion from SHA}
  while Len >= 4 do
  begin
    C := PLongWord(Source)^;
    dec(Len,4);
    inc(Source,4);
    PLongWord(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
    c := c shr 16;
    PLongWord(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
    inc(Dest,4);
  end;
  while Len > 0 do
  begin
    dec(Len);
    PWord(Dest)^ := Byte(Source^); //Shift Byte to Word
    inc(Source);
    inc(Dest);
  end;
end;

function USASCII7ToUnicodeString(const Source: RawByteString): ZWideString; overload;
begin
  Result := USASCII7ToUnicodeString(Pointer(Source), Length(Source));
end;
{$IFDEF UNICODE}
function ConvertZMsgToRaw(const AMessage: String; Const MsgCP, RawCP: Word): RawByteString;
begin
  Result := ZUnicodeToRaw(AMessage, RawCP);
end;

function ConvertEMsgToRaw(const AMessage: String; Const RawCP: Word): RawByteString;
begin
  Result := ZUnicodeToRaw(AMessage, RawCP);
end;
{$ELSE !UNICODE}
function ConvertZMsgToRaw(const AMessage: String; {$IFNDEF LCL}Const{$ENDIF}MsgCP, RawCP: Word): RawByteString;
begin
  {$IFDEF LCL}
  RawCP := zCP_UTF8;
  {$ENDIF}
  if ZCompatibleCodePages(RawCP, MsgCP) then
  {$IFDEF WITH_RAWBYTESTRING} //fpc2.7up
  begin
    Result := ''; //satisfy compiler
    ZSetString(PAnsiChar(AMessage), Length(AMessage), Result);
  end
  {$ELSE !WITH_RAWBYTESTRING}
  Result := AMessage
  {$ENDIF WITH_RAWBYTESTRING}
  else
    Result := ZUnicodeToRaw(PRawToUnicode(Pointer(AMessage),
      {%H-}PLengthInt(NativeUInt(AMessage) - StringLenOffSet)^, MsgCP), RawCP);
end;

function ConvertEMsgToRaw(const AMessage: String; {$IFNDEF LCL}Const{$ENDIF}RawCP: Word): RawByteString;
begin
  {$IFDEF LCL}
  RawCP := zCP_UTF8;
  {$ENDIF}
  if ZCompatibleCodePages(RawCP, ZDefaultSystemCodePage) then
  {$IFDEF WITH_RAWBYTESTRING} //fpc2.7up
  begin
    Result := ''; //satisfy compiler
    ZSetString(PAnsiChar(AMessage), Length(AMessage), Result);
  end
  {$ELSE !WITH_RAWBYTESTRING}
  Result := AMessage
  {$ENDIF WITH_RAWBYTESTRING}
  else
    Result := ZUnicodeToRaw(PRawToUnicode(Pointer(AMessage),
      {%H-}PLengthInt(NativeUInt(AMessage) - StringLenOffSet)^, ZDefaultSystemCodePage), RawCP);
end;
{$ENDIF UNICODE}

initialization
  SetDefaultSystemCodePage;
  SetConvertFunctions(@ConSettingsDummy);
end.


