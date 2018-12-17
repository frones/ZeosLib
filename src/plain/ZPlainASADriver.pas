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

{$IFNDEF ZEOS_DISABLE_ASA}

uses Classes, ZCompatibility, ZPlainDriver, ZPlainASAConstants;

{***************** Plain API Constants definition ****************}

type

  {** Represents a generic interface to ASA native API. }
  IZASAPlainDriver = interface (IZPlainDriver)
    ['{86AFDDD6-D401-4A30-B3BE-4AC5095E13F0}']
  end;

  {** Implements a driver for ASA 7.0-9.0}
  TZASAPlainDriver = class (TZAbstractPlainDriver, IZASAPlainDriver)
  public
    db_init: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_fini: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_string_connect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_string_disconnect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_start_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_stop_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_start_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_stop_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_find_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Alloc_sqlda: function( NumVar: LongWord): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    fill_sqlda: function( Parameter: PASASQLDA): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    free_filled_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    fill_s_sqlda: function( Parameter: PASASQLDA; MaxLength: Integer): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    free_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    free_sqlda_noind: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_setConnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_disconnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_describe_cursor: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_prepare_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_prepare_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    {ASA12 dbpp_prepare_describe_12, (SQLCA *,char *,char *,short int *,char *,struct sqlda *,struct sqlda *,unsigned int, unsigned short int, a_sql_uint32 ))}
    dbpp_prepare_describe_12: procedure (SQLCA: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word; UnknownUint2: Longword)
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_select: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1,
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_open: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Descriptor1: PASASQLDA; BlockSize: SmallInt; IsolationLvl: SmallInt;
      Options : Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_close: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_fetch: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_declare: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_dropstmt: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor: PASASQLDA;
      WhatToDesc: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_delete: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown1: PAnsiChar; UnKnown2: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_update: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_execute_imm: procedure(sqlca: PZASASQLCA; SqlRecordStatement:
      PAnsiChar;
      UnKnown1: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_put_into: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; UnKnown1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_put_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; Into_sqlda: PASASQLDA; Rows: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_commit: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_rollback: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_execute_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1: PASASQLDA;
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_get_data: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: LongInt; Descriptor1: PASASQLDA;
      Unknown: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_explain: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      SomeNumber1: Word; Descriptor1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_register_a_callback: procedure( sqlca: PZASASQLCA;
      CBIdx: integer; Proc: TZASASQLCallback);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_setoption: procedure( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_fetch_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word; ArrayWidth: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    sqlerror_message: function(sqlca: PZASASQLCA; Buffer: PAnsiChar;
      MaxSize: Integer): PAnsiChar;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_resume: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_cancel_request: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_change_char_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_change_nchar_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  protected
    procedure LoadApi; override;
    function GetUnicodeCodePageName: String; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function Clone: IZPlainDriver; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}

implementation

{$IFNDEF ZEOS_DISABLE_ASA}

uses SysUtils, ZPlainLoader, ZEncoding;

procedure TZASAPlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @sqlerror_message       := GetAddress('sqlerror_message');
    @db_init                := GetAddress('db_init');
    @db_fini                := GetAddress('db_fini');
    @db_string_connect      := GetAddress('db_string_connect');
    @db_string_disconnect   := GetAddress('db_string_disconnect');
    @db_find_engine         := GetAddress('db_find_engine');
    @db_start_engine        := GetAddress('db_start_engine');
    @db_stop_engine         := GetAddress('db_stop_engine');
    @db_start_database      := GetAddress('db_start_database');
    @db_stop_database       := GetAddress('db_stop_database');
    @alloc_sqlda            := GetAddress('alloc_sqlda');
    @fill_sqlda             := GetAddress('fill_sqlda');
    @fill_s_sqlda           := GetAddress('fill_s_sqlda');
    @free_filled_sqlda      := GetAddress('free_filled_sqlda');
    @free_sqlda             := GetAddress('free_sqlda');
    @free_sqlda_noind       := GetAddress('free_sqlda_noind');
    @dbpp_setConnect        := GetAddress('dbpp_setconnect');
    @dbpp_disconnect        := GetAddress('dbpp_disconnect');
    @dbpp_prepare_into      := GetAddress('dbpp_prepare_into');
    @dbpp_describe_cursor   := GetAddress('dbpp_describe_cursor');
    @dbpp_prepare_describe  := GetAddress('dbpp_prepare_describe');
    @dbpp_prepare_describe_12  := GetAddress('dbpp_prepare_describe_12');
    @dbpp_select            := GetAddress('dbpp_select');
    @dbpp_open              := GetAddress('dbpp_open');
    @dbpp_close             := GetAddress('dbpp_close');
    @dbpp_fetch             := GetAddress('dbpp_fetch');
    @dbpp_declare           := GetAddress('dbpp_declare');
    @dbpp_dropstmt          := GetAddress('dbpp_dropstmt');
    @dbpp_describe          := GetAddress('dbpp_describe');
    @dbpp_delete            := GetAddress('dbpp_delete');
    @dbpp_update            := GetAddress('dbpp_update');
    @dbpp_put_into          := GetAddress('dbpp_put_into');
    @dbpp_put_array         := GetAddress('dbpp_put_array');
    @dbpp_execute_imm       := GetAddress('dbpp_execute_imm');
    @dbpp_commit            := GetAddress('dbpp_commit');
    @dbpp_rollback          := GetAddress('dbpp_rollback');
    @dbpp_execute_into      := GetAddress('dbpp_execute_into');
    @dbpp_get_data          := GetAddress('dbpp_get_data');
    @dbpp_explain           := GetAddress('dbpp_explain');
    @dbpp_setoption         := GetAddress('dbpp_setoption');
    @dbpp_fetch_array       := GetAddress('dbpp_fetch_array');
    @db_register_a_callback := GetAddress('db_register_a_callback');
    @dbpp_resume            := GetAddress('dbpp_resume');
    @db_cancel_request      := GetAddress('db_cancel_request');
    @db_change_char_charset := GetAddress('db_change_char_charset');
    @db_change_nchar_charset:= GetAddress('db_change_nchar_charset');
  end;
end;

function TZASAPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ASA';
end;

function TZASAPlainDriver.GetProtocol: string;
begin
  Result := 'ASA';
end;

function TZASAPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8';
end;

procedure TZASAPlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('TIS-620', 1, ceAnsi, 874); {Windows Thailändisch, ISO8859-11, binäre Sortierung}
  AddCodePage('Windows-31J', 2, ceAnsi, 932); {Japanese Shift-JIS mit Microsoft-Erweiterungen}
  AddCodePage('GBK', 3, ceAnsi, 936); {GB2312-80 Simplified Chinese}
  AddCodePage('IBM949', 4, ceAnsi, 949); {Korean KS C 5601-1987-Codierung, Wansung}
  AddCodePage('BIG5', 5, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('EUC_CHINA', 6, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  AddCodePage('UTF-8', 7, ceUTF8, zCP_UTF8, '', 3); {UTF-8, 8-Bit-Mehrbyte-Zeichensatz für Unicode, binäre Reihenfolge}

  { SingleByte }
  AddCodePage('Windows-1250', 8, ceAnsi, 1250); {Windows Latin 2, Polnisch}
  AddCodePage('Windows-1251', 9, ceAnsi, 1251); {Windows Kyrillisch}
  AddCodePage('Windows-1252', 10, ceAnsi, 1252); { Windows Latin 1, Western}
  AddCodePage('Windows-1253', 11, ceAnsi, 1253); {Windows Griechisch, ISO8859-7 mit Erweiterungen}
  AddCodePage('Windows-1254', 12, ceAnsi, 1254); {Windows Türkisch, ISO8859-9 mit Erweiterungen}
  AddCodePage('Windows-1255', 13, ceAnsi, 1255); {Windows Hebräisch, ISO8859-8 mit Erweiterungen}
  AddCodePage('Windows-1256', 14, ceAnsi, 1256); {Windows Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('Windows-1257', 15, ceAnsi, 1257); {Windows Baltische Staaten, Litauisch}
  AddCodePage('Windows-1258', 16, ceAnsi, 1258); {Windows }

  {*nix}
  AddCodePage('ISO_8859-6:1987', 17, ceAnsi, 1256); {Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('ISO_8859-2:1987', 18, ceAnsi, 1251); {Zentral- und Osteuropäisch}
  //ISO-8859-15 //ISO9LATIN1
  //ISO_8859-7:1987 //Griechisch
  //ISO_8859-8:1988 //Hebräisch
  //ISO-8859-15 //Italienisch
  //EUC-JP //Japanisch
  //EUC-KR //Koreanisch
  //ISO_8859-5:1988 //Russisch
  AddCodePage('GB2312', 19, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  //EUC-TW //Traditionelles Chinesisch - Taiwan
  AddCodePage('Big5-HKSCS', 20, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('ISO_8859-9:1989', 21, ceAnsi, 920); //Türkisch
end;

function TZASAPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASAPlainDriver.Create;
end;

constructor TZASAPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  FLoader.AddLocation({$IFNDEF LINUX}ASA7_WINDOWS_DLL_LOCATION{$ELSE}ASA7_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA8_WINDOWS_DLL_LOCATION{$ELSE}ASA8_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA9_WINDOWS_DLL_LOCATION{$ELSE}ASA9_LINUX_DLL_LOCATION{$ENDIF});
  LoadCodePages;
end;

{$ENDIF ZEOS_DISABLE_ASA}

end.

