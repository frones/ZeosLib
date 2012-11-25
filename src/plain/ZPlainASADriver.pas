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

unit ZPlainASADriver;

interface

{$I ZPlain.inc}

uses Classes, ZCompatibility, ZPlainDriver, ZPlainASAConstants;

{***************** Plain API Constants definition ****************}

type

  {** Represents a generic interface to ASA native API. }
  IZASAPlainDriver = interface (IZPlainDriver)
    ['{86AFDDD6-D401-4A30-B3BE-4AC5095E13F0}']

    function sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
       MaxSize: Integer): PAnsiChar;
    function db_init( sqlca: PZASASQLCA): Integer;
    function db_fini( sqlca: PZASASQLCA): Integer;
    function db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar): Integer;
    function db_string_disconnect(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
    function db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;

    function db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
    function db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
    function db_fill_s_sqlda( Parameter: PASASQLDA; MaxLength: Integer):
      PASASQLDA;
    procedure db_free_sqlda( Parameter: PASASQLDA);
    procedure db_free_sqlda_noind( Parameter: PASASQLDA);
    procedure db_free_filled_sqlda( Parameter: PASASQLDA);

    procedure db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_setoption( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);

    procedure db_describe_cursor(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_into(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar; Descriptor: PASASQLDA;
      WhatToDesc: LongWord; LongNames: Word);
    procedure db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
      Options: Word);
    procedure db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
    procedure db_dropstmt(sqlca: PZASASQLCA; StatementName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt);
    procedure db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
     ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
     BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
    procedure db_close( sqlca: PZASASQLCA; CursorName: PAnsiChar);

    procedure db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar; Offset: Word;
      RelPositon: Integer; Descriptor: PASASQLDA; BlockSize: SmallInt;
      Options: Word);
    procedure db_fetch_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
      BlockSize: SmallInt; Options, ArrayWidth: Word);
    procedure db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
    procedure db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    procedure db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
    procedure db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);

    procedure db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SQLDescriptor, ResultDescriptor: PASASQLDA);
    procedure db_execute_into(sqlca: PZASASQLCA; Statement: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      ResultDescriptor: PASASQLDA);
    procedure db_execute_imm(sqlca: PZASASQLCA; Statement: PAnsiChar);

    procedure db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_rollback( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_register_callback( sqlca: PZASASQLCA;
      CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
    procedure db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    function db_cancel_request( sqlca: PZASASQLCA): Integer;
    function db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
    function db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
  end;

  {** Implements a driver for ASA 7.0-9.0}
  TZASABasePlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZASAPlainDriver)
  private
    ASA_API: TASA_API;
  protected
    procedure LoadApi; override;
    function GetUnicodeCodePageName: String; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
       MaxSize: Integer): PAnsiChar;
    function db_init( sqlca: PZASASQLCA): Integer;
    function db_fini( sqlca: PZASASQLCA): Integer;
    function db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar): Integer;
    function db_string_disconnect(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
    function db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;

    function db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
    function db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
    function db_fill_s_sqlda( Parameter: PASASQLDA; MaxLength: Integer):
      PASASQLDA;
    procedure db_free_sqlda( Parameter: PASASQLDA);
    procedure db_free_sqlda_noind( Parameter: PASASQLDA);
    procedure db_free_filled_sqlda( Parameter: PASASQLDA);

    procedure db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_setoption( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);

    procedure db_describe_cursor(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_into(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar; Descriptor: PASASQLDA;
      WhatToDesc: LongWord; LongNames: Word);
    procedure db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
      Options: Word);
    procedure db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
    procedure db_dropstmt(sqlca: PZASASQLCA; StatementName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt);
    procedure db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
    procedure db_close(sqlca: PZASASQLCA; CursorName: PAnsiChar);

    procedure db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar; Offset: Word;
      RelPositon: Integer; Descriptor: PASASQLDA; BlockSize: SmallInt;
      Options: Word);
    procedure db_fetch_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
      BlockSize: SmallInt; Options, ArrayWidth: Word);
    procedure db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
    procedure db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    procedure db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
    procedure db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);

    procedure db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor, ResultDescriptor: PASASQLDA);
    procedure db_execute_into(sqlca: PZASASQLCA; Statement: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      ResultDescriptor: PASASQLDA);
    procedure db_execute_imm(sqlca: PZASASQLCA; Statement: PAnsiChar);

    procedure db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_rollback( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_register_callback( sqlca: PZASASQLCA;
      CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
    procedure db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    function db_cancel_request( sqlca: PZASASQLCA): Integer;
    function db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
    function db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
  end;

  TZASA7PlainDriver = class(TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  TZASA8PlainDriver = class(TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for ASA 9.0 }
  TZASA9PlainDriver = class (TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for ASA 12.0 }
  TZASA12PlainDriver = class (TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses SysUtils, ZPlainLoader, ZEncoding;

procedure TZASABasePlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @ASA_API.sqlerror_message       := GetAddress('sqlerror_message');
    @ASA_API.db_init                := GetAddress('db_init');
    @ASA_API.db_fini                := GetAddress('db_fini');
    @ASA_API.db_string_connect      := GetAddress('db_string_connect');
    @ASA_API.db_string_disconnect   := GetAddress('db_string_disconnect');
    @ASA_API.db_find_engine         := GetAddress('db_find_engine');
    @ASA_API.db_start_engine        := GetAddress('db_start_engine');
    @ASA_API.db_stop_engine         := GetAddress('db_stop_engine');
    @ASA_API.db_start_database      := GetAddress('db_start_database');
    @ASA_API.db_stop_database       := GetAddress('db_stop_database');
    @ASA_API.alloc_sqlda            := GetAddress('alloc_sqlda');
    @ASA_API.fill_sqlda             := GetAddress('fill_sqlda');
    @ASA_API.fill_s_sqlda           := GetAddress('fill_s_sqlda');
    @ASA_API.free_filled_sqlda      := GetAddress('free_filled_sqlda');
    @ASA_API.free_sqlda             := GetAddress('free_sqlda');
    @ASA_API.free_sqlda_noind       := GetAddress('free_sqlda_noind');
    @ASA_API.dbpp_setConnect        := GetAddress('dbpp_setconnect');
    @ASA_API.dbpp_disconnect        := GetAddress('dbpp_disconnect');
    @ASA_API.dbpp_prepare_into      := GetAddress('dbpp_prepare_into');
    @ASA_API.dbpp_describe_cursor   := GetAddress('dbpp_describe_cursor');
    @ASA_API.dbpp_prepare_describe  := GetAddress('dbpp_prepare_describe');
    @ASA_API.dbpp_prepare_describe_12  := GetAddress('dbpp_prepare_describe_12');
    @ASA_API.dbpp_select            := GetAddress('dbpp_select');
    @ASA_API.dbpp_open              := GetAddress('dbpp_open');
    @ASA_API.dbpp_close             := GetAddress('dbpp_close');
    @ASA_API.dbpp_fetch             := GetAddress('dbpp_fetch');
    @ASA_API.dbpp_declare           := GetAddress('dbpp_declare');
    @ASA_API.dbpp_dropstmt          := GetAddress('dbpp_dropstmt');
    @ASA_API.dbpp_describe          := GetAddress('dbpp_describe');
    @ASA_API.dbpp_delete            := GetAddress('dbpp_delete');
    @ASA_API.dbpp_update            := GetAddress('dbpp_update');
    @ASA_API.dbpp_put_into          := GetAddress('dbpp_put_into');
    @ASA_API.dbpp_put_array         := GetAddress('dbpp_put_array');
    @ASA_API.dbpp_execute_imm       := GetAddress('dbpp_execute_imm');
    @ASA_API.dbpp_commit            := GetAddress('dbpp_commit');
    @ASA_API.dbpp_rollback          := GetAddress('dbpp_rollback');
    @ASA_API.dbpp_execute_into      := GetAddress('dbpp_execute_into');
    @ASA_API.dbpp_get_data          := GetAddress('dbpp_get_data');
    @ASA_API.dbpp_explain           := GetAddress('dbpp_explain');
    @ASA_API.dbpp_setoption         := GetAddress('dbpp_setoption');
    @ASA_API.dbpp_fetch_array       := GetAddress('dbpp_fetch_array');
    @ASA_API.db_register_a_callback := GetAddress('db_register_a_callback');
    @ASA_API.dbpp_resume            := GetAddress('dbpp_resume');
    @ASA_API.db_cancel_request      := GetAddress('db_cancel_request');
    @ASA_API.db_change_char_charset := GetAddress('db_change_char_charset');
    @ASA_API.db_change_nchar_charset:= GetAddress('db_change_nchar_charset');
  end;
end;

function TZASABasePlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8';
end;

procedure TZASABasePlainDriver.LoadCodePages;
begin
  { MultiByte }
  //AddCodePage('874THAIBIN', 1, ceAnsi, 874); {Windows Thailändisch, ISO8859-11, binäre Sortierung}
  //AddCodePage('932JPN', 2, ceAnsi, 932 {Japanese Shift-JIS mit Microsoft-Erweiterungen}
  //AddCodePage('936ZHO', 3, ceAnsi, 936); {Vereinfachtes Chinesisch, PRC GBK}
  //AddCodePage('949KOR', 4, ceAnsi, 949); {Korean KS C 5601-1987-Codierung, Wansung}
  //AddCodePage('950ZHO_HK', 5, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  //AddCodePage('950ZHO_TW', 6, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung}
  //AddCodePage('EUC_CHINA', 21, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  //AddCodePage('EUC_JAPAN', 22, ceAnsi, zCP_SHIFTJS); {Japanisch EUC JIS X 0208-1990 und JIS X 0212-1990-Zeichensatz}
  //AddCodePage('EUC_KOREA', 23, ceAnsi, 1361); { Koreanisch KS C 5601-1992 8-Bit-Zeichensatz, Johab}
  //AddCodePage('EUC_TAIWAN', 24, ceAnsi, 964); {EUC-TW-Kodierung}
  //AddCodePage('UCA', 29, ceUTF16, zCP_UTF16, 'utf8'); {UCA	UCA-Standardkollatierung}
  AddCodePage('UTF-8', 30, ceUTF8, zCP_UTF8, '', 3); {UTF-8, 8-Bit-Mehrbyte-Zeichensatz für Unicode, binäre Reihenfolge}

  { SingleByte }
  //AddCodePage('1250LATIN2', 7, ceAnsi, 1250); {Windows Latin 2, Mittel- und Osteuropa}
  //AddCodePage('1250POL', 8, ceAnsi, 1250); {Windows Latin 2, Polnisch}
  //AddCodePage('1251CYR', 9, ceAnsi, 1251); {Windows Kyrillisch}
  //AddCodePage('1252LATIN1', 10, ceAnsi, 1252); { Windows Latin 1, Western}
  //AddCodePage('1252LT1ACC', 11, ceAnsi, 1252); {Windows-Spezial Latin 1, Western, Zeichen mit Akzent nicht gleich}
  //AddCodePage('1252NOR', 12, ceAnsi, 1252); {Windows Latin 1, Norwegisch}
  //AddCodePage('1252SPA', 13, ceAnsi, 1252); {Windows Latin 1, Spanisch}
  //AddCodePage('1252SWEFIN', 14, ceAnsi, 1252); {Windows Latin 1, Schwedisch/Finnisch}
  //AddCodePage('1253ELL', 15, ceAnsi, 1253); {Windows Griechisch, ISO8859-7 mit Erweiterungen}
  //AddCodePage('1254TRK', 16, ceAnsi, 1254); {Windows Türkisch, ISO8859-9 mit Erweiterungen}
  //AddCodePage('1254TRKALT', 17, ceAnsi, 1254); {Windows Türkisch, ISO8859-9 mit Erweiterungen, I mit I-Punkt gleich I ohne I-Punkt}
  //AddCodePage('1255HEB', 18, ceAnsi, 1255); {Windows Hebräisch, ISO8859-8 mit Erweiterungen}
  //AddCodePage('1256ARA', 19, ceAnsi, 1256); {Windows Arabisch, ISO8859-6 mit Erweiterungen}
  //AddCodePage('1257LIT', 20, ceAnsi, 1257); {Windows Baltische Staaten, Litauisch}
  //AddCodePage('ISO1LATIN1', 25, ceAnsi, zCP_L1_ISO_8859_1); {ISO8859-1, ISO Latin 1, Western, Latin 1-Sortierreihenfolge}
  //AddCodePage('ISO9LATIN1', 26, ceAnsi, zCP_ISO_8859_9); {	ISO8859-15, ISO Latin 9, Western, Latin 1-Sortierreihenfolge}
  //AddCodePage('ISO_1', 27, ceAnsi, zCP_L1_ISO_8859_1); {ISO8859-1, ISO Latin 1, Western}
  //AddCodePage('ISO_BINENG', 28, ceAnsi, zCP_ACP); {Binäre Sortierreihenfolge, Englisch ISO/ASCII 7-Bit-Zuordnung nach Groß- und Kleinschreibung}
end;

constructor TZASABasePlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  LoadCodePages;
end;

function TZASABasePlainDriver.sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
  MaxSize: Integer): PAnsiChar;
begin
  Result := ASA_API.sqlerror_message( sqlca, Buffer, MaxSize);
end;

function TZASABasePlainDriver.db_init( sqlca: PZASASQLCA): Integer;
begin
  Result := ASA_API.db_init( sqlca);
end;

function TZASABasePlainDriver.db_fini( sqlca: PZASASQLCA): Integer;
begin
  Result := ASA_API.db_fini( sqlca);
end;

function TZASABasePlainDriver.db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar):
  Integer;
begin
  Result := ASA_API.db_string_connect( sqlca, Params);
end;

function TZASABasePlainDriver.db_string_disconnect( sqlca: PZASASQLCA;
  Params: PAnsiChar): LongWord;
begin
  Result := ASA_API.db_string_disconnect( sqlca, Params)
end;

function TZASABasePlainDriver.db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  Word;
begin
  Result := ASA_API.db_find_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := ASA_API.db_start_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := ASA_API.db_stop_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := ASA_API.db_start_database( sqlca, Params);
end;

function TZASABasePlainDriver.db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := ASA_API.db_stop_database( sqlca, Params);
end;

function TZASABasePlainDriver.db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
begin
  Result := ASA_API.alloc_sqlda( NumVar);
end;

function TZASABasePlainDriver.db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
begin
  Result := ASA_API.fill_sqlda( Parameter);
end;

function TZASABasePlainDriver.db_fill_s_sqlda( Parameter: PASASQLDA;
  MaxLength: Integer): PASASQLDA;
begin
  Result := ASA_API.fill_s_sqlda( Parameter, MaxLength);
end;

procedure TZASABasePlainDriver.db_free_sqlda( Parameter: PASASQLDA);
begin
  ASA_API.free_sqlda( Parameter);
end;

procedure TZASABasePlainDriver.db_free_sqlda_noind( Parameter: PASASQLDA);
begin
  ASA_API.free_sqlda_noind( Parameter);
end;

procedure TZASABasePlainDriver.db_free_filled_sqlda( Parameter: PASASQLDA);
begin
  ASA_API.free_filled_sqlda( Parameter);
end;

procedure TZASABasePlainDriver.db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
begin
  ASA_API.dbpp_setconnect( sqlca, ConnStr);
end;

procedure TZASABasePlainDriver.db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
begin
  ASA_API.dbpp_disconnect( sqlca, ConnStr);
end;

procedure TZASABasePlainDriver.db_setoption( sqlca: PZASASQLCA; Temporary: Integer;
   User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);
begin
  ASA_API.dbpp_setoption( sqlca, Temporary, User, Option, Descriptor);
end;

procedure TZASABasePlainDriver.db_describe_cursor( sqlca: PZASASQLCA;
  CursorName: PAnsiChar; Descriptor: PASASQLDA; WhatToDesc: LongWord);
begin
  ASA_API.dbpp_describe_cursor( sqlca, CursorName, Descriptor, WhatToDesc);
end;

procedure TZASABasePlainDriver.db_prepare_into( sqlca: PZASASQLCA;
  ProgName: PAnsiChar; StatementNum: PSmallInt; SqlStatement: PAnsiChar;
  Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
begin
  ASA_API.dbpp_prepare_into( sqlca, nil, ProgName, StatementNum, SqlStatement,
    Descriptor1, Descriptor2, WhatToDesc);
end;

procedure TZASABasePlainDriver.db_prepare_describe( sqlca: PZASASQLCA;
   ProgName: PAnsiChar; StatementNum: PSmallInt; SqlStatement: PAnsiChar;
  Descriptor: PASASQLDA; WhatToDesc: LongWord; LongNames: Word);
var
  U1: LongWord;
begin
  U1 := 0;
  if Assigned(ASA_API.dbpp_prepare_describe) then
    ASA_API.dbpp_prepare_describe( sqlca, nil, ProgName, StatementNum,
      SqlStatement, nil, Descriptor, WhatToDesc, LongNames)
  else
    if Assigned(ASA_API.dbpp_prepare_describe_12) then
    ASA_API.dbpp_prepare_describe_12(sqlca, nil, ProgName, StatementNum,
        SqlStatement, nil, Descriptor, WhatToDesc, LongNames, U1);
end;

procedure TZASABasePlainDriver.db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
  Options: Word);
begin
  ASA_API.dbpp_declare( sqlca, CursorName, StatementName, ProgName,
    StatementNum, Options);
end;

procedure TZASABasePlainDriver.db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
begin
  ASA_API.dbpp_describe( sqlca, nil, ProgName, StatementNum, Descriptor,
    WhatToDesc);
end;

procedure TZASABasePlainDriver.db_dropstmt( sqlca: PZASASQLCA;
  StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt);
begin
  ASA_API.dbpp_dropstmt( sqlca, StatementName, ProgName, StatementNum);
end;

procedure TZASABasePlainDriver.db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
  BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
begin
  ASA_API.dbpp_open( sqlca, CursorName, nil, ProgName, StatementNum,
    Descriptor, BlockSize, IsolationLvl, CursorOptions);
end;

procedure TZASABasePlainDriver.db_close(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  ASA_API.dbpp_close( sqlca, CursorName);
end;

procedure TZASABasePlainDriver.db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
  BlockSize: SmallInt; Options: Word);
begin
  ASA_API.dbpp_fetch( sqlca, CursorName, Offset, RelPositon, Descriptor,
    BlockSize, Options);
end;

procedure TZASABasePlainDriver.db_fetch_array( sqlca: PZASASQLCA;
  CursorName: PAnsiChar; Offset: Word; RelPositon: Integer;
  Descriptor: PASASQLDA; BlockSize: SmallInt; Options, ArrayWidth: Word);
begin
  ASA_API.dbpp_fetch_array( sqlca, CursorName, Offset, RelPositon, Descriptor,
    BlockSize, Options, ArrayWidth);
end;

procedure TZASABasePlainDriver.db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
begin
  ASA_API.dbpp_get_data( sqlca, CursorName, ColumnNumber, Offset, Descriptor,
    0);
end;

procedure TZASABasePlainDriver.db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  ASA_API.dbpp_delete( sqlca, CursorName, nil, nil);
end;

procedure TZASABasePlainDriver.db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA);
begin
  ASA_API.dbpp_update( sqlca, CursorName, Descriptor);
end;

procedure TZASABasePlainDriver.db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
begin
  ASA_API.dbpp_put_into( sqlca, CursorName, Descriptor, ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);
begin
  ASA_API.dbpp_put_array( sqlca, CursorName, Descriptor, ResultDescriptor,
    Rows);
end;

procedure TZASABasePlainDriver.db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
  StatementNum: PSmallInt; Descriptor, ResultDescriptor: PASASQLDA);
begin
  ASA_API.dbpp_select( sqlca, nil, ProgName, StatementNum, Descriptor,
    ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_execute_into( sqlca: PZASASQLCA;
  Statement: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
begin
  ASA_API.dbpp_execute_into( sqlca, Statement, ProgName, StatementNum,
    Descriptor, ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_execute_imm( sqlca: PZASASQLCA;
  Statement: PAnsiChar);
begin
  ASA_API.dbpp_execute_imm( sqlca, Statement, 2);
end;

procedure TZASABasePlainDriver.db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
begin
  ASA_API.dbpp_commit( sqlca, TransLevel);
end;

procedure TZASABasePlainDriver.db_rollback( sqlca: PZASASQLCA;
  TransLevel: LongWord);
begin
  ASA_API.dbpp_rollback( sqlca, TransLevel);
end;

procedure TZASABasePlainDriver.db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA);
begin
  ASA_API.dbpp_explain( sqlca, CursorName, 0, Descriptor);
end;

procedure TZASABasePlainDriver.db_register_callback( sqlca: PZASASQLCA;
  CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
begin
  ASA_API.db_register_a_callback( sqlca, Integer( CBIdx), Proc);
end;

procedure TZASABasePlainDriver.db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  ASA_API.dbpp_resume( sqlca, CursorName);
end;

function TZASABasePlainDriver.db_cancel_request( sqlca: PZASASQLCA): Integer;
begin
  Result := ASA_API.db_cancel_request( sqlca);
end;

function TZASABasePlainDriver.db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
begin
  Result := ASA_API.db_change_char_charset(sqlca, CharSet);
end;

function TZASABasePlainDriver.db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
begin
  Result := ASA_API.db_change_nchar_charset(sqlca, CharSet);
end;

{TZASA7PlainDriver}

function TZASA7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA7PlainDriver.Create;
end;

constructor TZASA7PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA7_WINDOWS_DLL_LOCATION{$ELSE}ASA7_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA7PlainDriver.GetProtocol: string;
begin
  Result := 'ASA7';
end;

function TZASA7PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ASA 7.0 DBLib';
end;

{TZASA8PlainDriver}

constructor TZASA8PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA8_WINDOWS_DLL_LOCATION{$ELSE}ASA8_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA8PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA8PlainDriver.Create;
end;

function TZASA8PlainDriver.GetProtocol: string;
begin
  Result := 'ASA8';
end;

function TZASA8PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ASA 8.0 DBLib';
end;

{TZASA9PlainDriver}

constructor TZASA9PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA9_WINDOWS_DLL_LOCATION{$ELSE}ASA9_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA9PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA9PlainDriver.Create;
end;

function TZASA9PlainDriver.GetProtocol: string;
begin
  Result := 'ASA9';
end;

function TZASA9PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ASA 9.0 DBLib';
end;

{TZASA12PlainDriver}

constructor TZASA12PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA12_WINDOWS_DLL_LOCATION{$ELSE}ASA12_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA12PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA12PlainDriver.Create;
end;

function TZASA12PlainDriver.GetProtocol: string;
begin
  Result := 'ASA12';
end;

function TZASA12PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ASA 9.0 DBLib';
end;

end.

