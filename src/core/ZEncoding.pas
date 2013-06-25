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
  Classes,
  {$IFDEF WITH_LCONVENCODING}
  LConvEncoding,
  {$ENDIF}
  {$IF defined(MSWINDOWS) and not (defined(WITH_UNICODEFROMLOCALECHARS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER))}
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
  zCP_DOS852 = 852; {ibm852	852	Osteuropäisch (DOS)}
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
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets — Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2	east european (ISO), 8-bit single-byte coded graphic character sets — Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L3_ISO_8859_3 = 28593; {ISO 8859-3 Latin 3}
  zCP_L4_ISO_8859_4 = 28594; {ISO 8859-4 Baltic}
  zCP_L5_ISO_8859_5 = 28595; {-bit single-byte coded graphic character sets — Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
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
  out PlainConvert, DbcConvert: TConvertEncodingFunction);

{$ELSE}

function AnsiToWide(const S: ZAnsiString;
  const CP: Word): {$IFDEF WITH_UNICODEFROMLOCALECHARS}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function WideToAnsi(const ws: {$IFDEF WITH_UNICODEFROMLOCALECHARS}UnicodeString{$ELSE}WideString{$ENDIF}; CP: Word):
  ZAnsiString; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function StringToAnsiEx(const s: String; const {$IFNDEF UNICODE}FromCP,{$ENDIF} ToCP: Word): ZAnsiString; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function AnsiToStringEx(const s: ZAnsiString; const FromCP{$IFNDEF UNICODE}, ToCP{$ENDIF}: Word): String; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}

{**
  Returns the current system codepage of AnsiString
  @return current system codepage of AnsiString
}
function ZDefaultSystemCodePage: Word;

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): ZAnsiString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): ZAnsiString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  WasDecoded: Boolean; ConSettings: PZConSettings): ZAnsiString; overload;

function GetValidatedAnsiString(const Ansi: ZAnsiString;
  ConSettings: PZConSettings; const FromDB: Boolean): ZAnsiString;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Buffer the pointer to the Data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

function GetValidatedUnicodeStream(const Ansi: ZAnsiString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

implementation

uses SysUtils, Types {$IFDEF WITH_WIDESTRUTILS},WideStrUtils{$ENDIF};

{$IFNDEF WITH_LCONVENCODING}
function AnsiToWide(const S: ZAnsiString;
  const CP: Word): {$IFDEF WITH_UNICODEFROMLOCALECHARS}UnicodeString{$ELSE}WideString{$ENDIF};
{$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER}
var
  {$IFDEF WITH_UNICODEFROMLOCALECHARS}wlen, ulen{$ELSE}l{$ENDIF}: Integer;
{$ENDIF}
begin
  Result := '';
  case CP of
    zCP_UTF8: Result := UTF8ToString(s);
    zCP_NONE: Result := {$IFDEF WITH_UNICODEFROMLOCALECHARS}UnicodeString{$ELSE}WideString{$ENDIF}(s);
    else
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Ansi2WideMoveProc(PAnsiChar(s), CP, Result, Length(s));
      {$ELSE}
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
          SetLength(Result, l - 1); //Set Result-Length
          MultiByteToWideChar(CP, 0, PAnsiChar(@s[1]),
            - 1, PWideChar(@Result[1]), l - 1); //Convert Ansi to Wide with supported Chars
          {$ENDIF}
        end
        {$ELSE} //FPC.6-
          Result := ZWideString(s); //random success!
        {$IFEND}
      {$ENDIF}
  end;
end;

function WideToAnsi(const ws: {$IFDEF WITH_UNICODEFROMLOCALECHARS}UnicodeString{$ELSE}WideString{$ENDIF}; CP: Word):
  ZAnsiString;
{$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER}
var
  {$IFDEF WITH_UNICODEFROMLOCALECHARS}wlen, ulen{$ELSE}l{$ENDIF}: Integer;
{$ENDIF}
begin
  Result := '';
  case CP of
    zCP_UTF8: Result := UTF8Encode(ws);
    zCP_NONE: Result := ZAnsiString(WS);
    else
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Wide2AnsiMoveProc(PWideChar(WS), Result, CP, Length(WS));
      {$ELSE}
        {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
        begin
          {$IFDEF WITH_UNICODEFROMLOCALECHARS}
          wlen := Length(ws);
          ulen := LocaleCharsFromUnicode(CP, 0, PWideChar(WS), wlen, NIL, 0, NIL, NIL);
          setlength(Result, ulen);
          LocaleCharsFromUnicode(CP, 0, PWideChar(WS), wlen, PAnsiChar(Result), ulen, NIL, NIL);
          {$ELSE}
          l := WideCharToMultiByte(CP,0, @ws[1], - 1, nil, 0, nil, nil); //Checkout the result length
          if l = 0 then Exit;
          SetLength(Result, l - 1); //SetResult Length
          WideCharToMultiByte(CP,0, @ws[1], - 1, @Result[1], l - 1, nil, nil); // Convert Wide down to Ansi
          {$ENDIF}
        end;
        {$ELSE} //FPC2.6-
        Result := ZAnsiString(WS); //random success
        {$IFEND}
      {$ENDIF}
  end;
end;

function AnsiToStringEx(const s: ZAnsiString;
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
        Result := AnsiToWide(s, FromCP);
      {$ELSE} //Ansi-Compiler
        Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
      {$ENDIF}
end;

function StringToAnsiEx(const s: String; const {$IFNDEF UNICODE}FromCP, {$ENDIF} ToCP: Word): ZAnsiString;
begin
  if s = '' then
    Result := ''
  else
    if ( ToCP = zCP_NONE ) {$IFNDEF UNICODE} or ( FromCP = ToCP ){$ENDIF}then
      Result := ZAnsiString(s)
    else
      {$IFDEF UNICODE}
      if ToCP = zCP_UTF8 then
        result := UTF8Encode(s)
      else
        Result := WideToAnsi(s, ToCP);
      {$ELSE} //Ansi-Compiler
        Result := WideToAnsi(AnsiToWide(s, FromCP), ToCP);
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
      852: //CP852
        begin
          DbcConvert := @CP852ToUTF8;
          PlainConvert := @UTF8ToCP852;
        end;
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
    {$IF defined(MSWINDOWS) and not defined(WinCE)}
    Result := GetACP;
    {$ELSE}
    Result := zCP_UTF8;
    {$IFEND}
  {$ENDIF}
end;

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

  if ( StrLen(PAnsiChar(Bytes)) < Size ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
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

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): ZAnsiString;
var
  WS: ZWideString;
  Bytes: TByteDynArray;
begin
  Result := '';

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
            Result := WideToAnsi(AnsiToWide(PAnsiChar(Bytes), ConSettings.CTRL_CP), ConSettings.ClientCodePage.CP)
            {$ENDIF}
        else  //Database expects UTF8
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            Result := AnsiToUTF8(String(PAnsiChar(Bytes))) //Can't localize the ansi CP
          else
            {$IFDEF WITH_LCONVENCODING}
            Result := AnsiToUTF8(PAnsiChar(Bytes));
            {$ELSE}
            Result := UTF8Encode(AnsiToWide(PAnsiChar(Bytes), ConSettings.CTRL_CP));
            {$ENDIF}
      ceUTF8:
        if ConSettings.ClientCodePage.Encoding = ceAnsi then //ansi expected
          {$IFDEF WITH_LCONVENCODING}
          Result := Consettings.PlainConvertFunc(String(PAnsiChar(Bytes)))
          {$ELSE}
          Result := WideToAnsi(UTF8ToString(PAnsiChar(Bytes)), ConSettings.ClientCodePage.CP)
          {$ENDIF}
         else //UTF8 Expected
           Result := PAnsiChar(Bytes);
      ceUTF16:
        begin
          SetLength(WS, Size div 2);
          System.Move(PWideChar(Bytes)^, PWideChar(WS)^, Size);
          if ConSettings.ClientCodePage.Encoding = ceAnsi then
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(UTF8Encode(WS))
            {$ELSE}
            Result := WideToAnsi(WS, ConSettings.ClientCodePage.CP)
            {$ENDIF}
          else
            Result := UTF8Encode(WS);
        end;
      else
        Result := '';
    end;
    SetLength(Bytes, 0);
  end;
end;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): ZAnsiString;
var DB_CP: Word;
begin
  DB_CP := ConSettings.ClientCodePage.CP;
  ConSettings.ClientCodePage.CP := ToCP;
  Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
  ConSettings.ClientCodePage.CP := DB_CP;
end;

function GetValidatedAnsiString(const Ansi: ZAnsiString;
  ConSettings: PZConSettings; const FromDB: Boolean): ZAnsiString;
begin
  if FromDB then
    if ( ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP ) or not ConSettings.AutoEncode then
      Result := Ansi
    else
      {$IFDEF WITH_LCONVENCODING}
      Result := Consettings.DbcConvertFunc(Ansi)
      {$ELSE}
      Result := WideToAnsi(AnsiToWide(Ansi, ConSettings.ClientCodePage.CP), ConSettings.CTRL_CP)
      {$ENDIF}
  else
    Result := ''; // not done yet  and not needed. Makes the compiler happy
end;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  WasDecoded: Boolean; ConSettings: PZConSettings): ZAnsiString;
var
  WS: ZWideString;
begin
  if WasDecoded then
  begin
    SetLength(WS, Size div 2);
    System.Move(Buffer^, PWideChar(WS)^, Size);

    {$IFDEF WITH_LCONVENCODING}
    Result := Consettings.PlainConvertFunc(UTF8Encode(WS));
    {$ELSE}
    Result := WideToAnsi(WS, ConSettings.ClientCodePage.CP);
    {$ENDIF}
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
  WS: ZWideString;
  Bytes: TByteDynArray;

  procedure SetFromWide;
  begin
    SetLength(WS, Size div 2);
    System.Move(PWideChar(Bytes)^, PWideChar(WS)^, Size);
  end;
begin
  Result := nil;
  WS := '';
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
            ceUTF8: WS := UTF8ToString(PAnsiChar(Bytes));
            ceAnsi:
              {$IFDEF WITH_LCONVENCODING}
              WS := ZWideString(PAnsiChar(Bytes)); //cast means random success
              {$ELSE}
              if ( ConSettings.CTRL_CP = zCP_UTF8) then
                WS := ZWideString(PAnsiChar(Bytes)) //random success
              else
                WS := AnsiToWide(PAnsiChar(Bytes), ConSettings.CTRL_CP);
             {$ENDIF}
            end;
        ceAnsi: //We've to start from the premisse we've got a Unicode string i there
          begin
            SetLength(WS, Size div 2);
            System.Move(PWideChar(Bytes)^, PWideChar(WS)^, Size);
          end;
        ceUTF8: WS := UTF8ToString(PAnsiChar(Bytes));
        ceUTF16:
          begin
            SetLength(WS, Size div 2);
            System.Move(PWideChar(Bytes)^, PWideChar(WS)^, Size);
          end;
      end;

    Len := Length(WS)*2;
    if not Assigned(Result) and (Len > 0) then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(PWideChar(WS)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
    SetLength(Bytes, 0);
  end;
end;

function GetValidatedUnicodeStream(const Ansi: ZAnsiString;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  Len: Integer;
  WS: ZWideString;
begin
  Result := nil;
  if Ansi <> '' then
  begin
    if FromDB then
      {$IFDEF WITH_LCONVENCODING}
      WS := UTF8ToString(Consettings.DbcConvertFunc(Ansi))
      {$ELSE}
      WS := AnsiToWide(Ansi, ConSettings.ClientCodePage.CP)
      {$ENDIF}
    else
      case DetectUTF8Encoding(Ansi) of
        etUSASCII, etUTF8: WS := UTF8ToString(Ansi);
        etAnsi:
          {$IFDEF WITH_LCONVENCODING}
          WS := ZWideString(Ansi); //random success
          {$ELSE}
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            WS := ZWideString(Ansi) //random success
          else
            WS := AnsiToWide(Ansi, ConSettings.CTRL_CP);
         {$ENDIF}
      end;

    Len := Length(WS)*2;
    if Len > 0 then
    begin
      Result := TMemoryStream.Create;
      Result.Size := Len;
      System.Move(PWideChar(WS)^, TMemoryStream(Result).Memory^, Len);
      Result.Position := 0;
    end;
  end;
end;

end.
