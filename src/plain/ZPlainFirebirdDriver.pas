{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for Firebird             }
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

unit ZPlainFirebirdDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE}

{$IFDEF UNIX}
{$IFDEF INTERBASE_CRYPT}
{$DEFINE ENABLE_INTERBASE_CRYPT}
{$ENDIF}
{$ENDIF}

uses Types,
  {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZCompatibility, ZPlainDriver, ZPlainLoader,
  ZPlainFirebirdInterbaseConstants;

const

  WINDOWSIB6_DLL_LOCATION   = 'gds32.dll';
  LINUXIB6_DLL_LOCATION   = 'libgds'+SharedSuffix;

  WINDOWS_DLL_LOCATION   = 'gds32.dll';
  LINUX_DLL_LOCATION   = 'libgds32'+SharedSuffix;
  LINUX_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix;

  WINDOWS2_DLL_LOCATION   = 'fbclient.dll';
  WINDOWS2_DLL_LOCATION_EMBEDDED = 'fbclientd.dll';

  LINUX2_DLL_LOCATION   = 'libfbclient'+SharedSuffix;
  LINUX2_DLL_LOCATION_EMBEDDED = 'libfbembed'+SharedSuffix;
  LINUX2_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix;

  WINDOWS15_DLL_LOCATION   = 'fbclient15.dll';
  WINDOWS15_DLL_LOCATION_EMBEDDED = 'fbclientd15.dll';
  LINUX15_DLL_LOCATION   = 'libfbclient'+SharedSuffix+'.15';
  LINUX15_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix+'.15';
  LINUX15_DLL_LOCATION_EMBEDDED = 'libfbembed'+SharedSuffix+'.15';

  WINDOWS20_DLL_LOCATION   = 'fbclient20.dll';
  WINDOWS20_DLL_LOCATION_EMBEDDED = 'fbclientd20.dll';
  LINUX2_DLL_LOCATION2   = 'libfbclient'+SharedSuffix+'.2';
  LINUX20_DLL_LOCATION   = 'libfbclient'+SharedSuffix+'.20';
  LINUX20_DLL_LOCATION_EMBEDDED = 'libfbembed'+SharedSuffix+'.20';
  LINUX20_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix+'.20';

  WINDOWS21_DLL_LOCATION   = 'fbclient21.dll';
  WINDOWS21_DLL_LOCATION_EMBEDDED = 'fbclientd21.dll';
  LINUX21_DLL_LOCATION   = 'libfbclient'+SharedSuffix+'.21';
  LINUX21_DLL_LOCATION_EMBEDDED = 'libfbembed'+SharedSuffix+'.21';
  LINUX21_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix+'.21';

  WINDOWS25_DLL_LOCATION   = 'fbclient25.dll';
  WINDOWS25_DLL_LOCATION_EMBEDDED = 'fbclientd25.dll';
  LINUX25_DLL_LOCATION   = 'libfbclient'+SharedSuffix+'.25';
  LINUX25_DLL_LOCATION_EMBEDDED = 'libfbembed'+SharedSuffix+'.25';
  LINUX25_IB_CRYPT_LOCATION = 'libcrypt'+SharedSuffix+'.25';

type

  {** Represents a generic interface to Interbase native API. }
  IZInterbasePlainDriver = interface (IZPlainDriver)
    ['{AE2C4379-4E47-4752-BC01-D405ACC337F5}']
    function GetCodePageArray: TWordDynArray;
  end;

  {** Implements a base driver for Firebird}

  { TZFirebirdBaseDriver }

  TZInterbasePlainDriver = class (TZAbstractPlainDriver, IZInterbasePlainDriver)
  private
    FCodePageArray: TWordDynArray;
  protected
    FPreLoader : TZNativeLibraryLoader;
    procedure FillCodePageArray;
    procedure LoadCodePages; override;
    function GetUnicodeCodePageName: String; override;
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    procedure Initialize(const Location: String = ''); virtual;
    {$ENDIF}
    procedure LoadApi; override;
  public
    constructor Create;
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    destructor Destroy; override;
    {$ENDIF}

    function GetCodePageArray: TWordDynArray;

  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  public
    { General database routines }

    isc_attach_database: function(status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_detach_database: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_drop_database: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_create_database: function(status_vector: PISC_STATUS; db_name_len: Smallint;
      db_name: PAnsiChar; handle: PISC_DB_HANDLE; dpb_len: Smallint; dpb: PAnsiChar;
      db_type: Smallint{UNUSED}): ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_database_info: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PByte; result_buffer_length: Short;
      result_buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_get_client_major_version: function(): NativeInt;
    {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_get_client_minor_version: function(): NativeInt;
    {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Array processing routines }
    isc_array_gen_sdl: function(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PAnsiChar; isc_arg5: PShort): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_array_get_slice: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_array_lookup_bounds: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_array_lookup_desc: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_array_set_desc: function(status_vector: PISC_STATUS;
      table_name: PAnsiChar; column_name: PAnsiChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_array_put_slice: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_free: function(isc_arg1: PAnsiChar): ISC_LONG;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_sqlcode: function(status_vector: PISC_STATUS): ISC_LONG;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_sql_interprete: procedure(sqlcode: Short; buffer: PAnsiChar;
      buffer_length: Short); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_interprete: function(buffer: PAnsiChar; status_vector: PPISC_STATUS):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    fb_interpret: function(buffer: PAnsiChar;  bufsize: integer; status_vector: PPISC_STATUS):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Transaction support routines }

    isc_start_transaction: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PAnsiChar):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_start_multiple: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_rollback_transaction: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_rollback_retaining: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_commit_retaining: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_commit_transaction: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_transaction_info: function(status_vector: PISC_STATUS;
      tr_handle: PISC_TR_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short;
      result_buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Dynamic SQL routines }

    isc_dsql_allocate_statement: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_alloc_statement2: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_describe: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_describe_bind: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_execute: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_execute2: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_execute_immediate: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_fetch: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_free_statement: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_prepare: function(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_set_cursor_name: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PAnsiChar; _type: Word): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_dsql_sql_info: function(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PAnsiChar;
      buffer_length: Short; buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Blob processing routines }

    isc_open_blob2: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_create_blob2: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_blob_info: function(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short; result_buffer: PAnsiChar):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_close_blob: function(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_cancel_blob: function(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_get_segment: function(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PAnsiChar): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_put_segment: function(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PAnsiChar):
      ISC_STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Event processing routines }

    isc_event_block: function(event_buffer: PPAnsiChar; result_buffer: PPAnsiChar;
      id_count: ISC_USHORT;
      v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): ISC_LONG;
      cdecl; // ! always cdecl

    isc_event_counts: procedure(event_counts: PARRAY_ISC_EVENTCOUNTS;
      buffer_length: Short; event_buffer: PAnsiChar; result_buffer: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_cancel_events: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_que_events: function(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PAnsiChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Types convertion routines }

    //isc_decode_date: procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
      //{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_encode_date: procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
      //{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Interbase Version 6 routines }
    //isc_decode_sql_date: procedure(ib_date: PISC_DATE;
      //tm_date: PCTimeStructure); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_decode_sql_time: procedure(ib_time: PISC_TIME;
      //tm_date: PCTimeStructure); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_decode_timestamp: procedure(ib_timestamp: PISC_TIMESTAMP;
      //tm_date: PCTimeStructure); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_encode_sql_date: procedure(tm_date: PCTimeStructure;
      //ib_date: PISC_DATE); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_encode_sql_time: procedure(tm_date: PCTimeStructure;
      //ib_time: PISC_TIME); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    //isc_encode_timestamp: procedure(tm_date: PCTimeStructure;
      //ib_timestamp: PISC_TIMESTAMP);
      //{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_vax_integer: function(buffer: PAnsiChar; length: Short): ISC_LONG;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    isc_portable_integer: function(ptr: pbyte; length: Smallint): ISC_INT64;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    isc_get_client_version: procedure(version: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  end;

  {** Implements a native driver for Firebird }
  TZFirebirdPlainDriver = class (TZInterbasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

function XSQLDA_LENGTH(Value: LongInt): LongInt;

{$ENDIF ZEOS_DISABLE_INTERBASE}

implementation

{$IFNDEF ZEOS_DISABLE_INTERBASE}

uses SysUtils, ZEncoding, ZFastCode{$IFDEF UNICODE},ZSysUtils{$ENDIF};

function XSQLDA_LENGTH(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

{ IZFirebirdPlainDriver }

function TZInterbasePlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF8';
end;

procedure TZInterbasePlainDriver.FillCodePageArray;
var I: Integer;
begin
  SetLength(FCodePageArray, 70);
  for i := 0 to High(FCodePages) do
    FCodePageArray[FCodePages[i].ID] := FCodePages[i].CP;
end;

procedure TZInterbasePlainDriver.LoadCodePages;
begin
  AddCodePage('ASCII', CS_ASCII, ceAnsi, zCP_WIN1252); {English}
  AddCodePage('BIG_5', CS_BIG_5, ceAnsi, zCP_Big5); {Chinese, Vietnamese, Korean}
  AddCodePage('CYRL', CS_CYRL, ceAnsi, zCP_WIN1251, '', 2);  {Russian}
  AddCodePage('DOS437', CS_DOS437, ceAnsi, zCP_DOS437); {English (USA)}
  AddCodePage('DOS850', CS_DOS850, ceAnsi, zCP_DOS850); {Latin I (no Euro symbol)}
  AddCodePage('DOS852', CS_DOS852, ceAnsi, {$IFDEF MSWINDOWS}zCP_L2_ISO_8859_2{$ELSE}zCP_DOS852{$ENDIF}); {Latin II} //need a crack for windows. Don't know why but it seems Win converts cp852 false see: http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=4779&sid=a143d302f1f967b844bea2bee9eb39b8
  AddCodePage('DOS857', CS_DOS857, ceAnsi, zCP_DOS857); {Turkish}
  AddCodePage('DOS860', CS_DOS860, ceAnsi, zCP_DOS860); {Portuguese}
  AddCodePage('DOS861', CS_DOS861, ceAnsi, zCP_DOS861); {Icelandic}
  AddCodePage('DOS863', CS_DOS863, ceAnsi, zCP_DOS863); {French (Canada)}
  AddCodePage('DOS865', CS_DOS865, ceAnsi, zCP_DOS865); {Nordic}
  AddCodePage('EUCJ_0208', CS_EUCJ_0208, ceAnsi, zCP_EUC_JP, '', 2); {EUC Japanese}
  AddCodePage('GB_2312', CS_GB_2312, ceAnsi, zCP_GB2312, '', 2); {Simplified Chinese (Hong Kong, PRC)}
  AddCodePage('ISO8859_1', CS_ISO8859_1, ceAnsi, zCP_L1_ISO_8859_1); {Latin 1}
  AddCodePage('KSC_5601', CS_KSC_5601, ceAnsi, zCP_EUCKR, '', 2); {Korean (Unified Hangeul)}
  AddCodePage('NEXT', CS_NEXT);  {apple NeXTSTEP encoding}
  AddCodePage('NONE', CS_NONE, ceAnsi, ZOSCodePage, '', 1, False); {Codepage-neutral. Uppercasing limited to ASCII codes 97-122}
  AddCodePage('OCTETS', CS_BINARY, ceAnsi, $fffd); {Binary character}
  AddCodePage('SJIS_0208', CS_SJIS_0208, ceAnsi, zCP_SHIFTJS, '', 2); {Japanese} //fixed: https://sourceforge.net/p/zeoslib/tickets/115/
  AddCodePage('UNICODE_FSS', CS_UNICODE_FSS, ceUTF8, zCP_UTF8, '', 3); {UNICODE}
  AddCodePage('WIN1250', CS_WIN1250, ceAnsi, zCP_WIN1250); {ANSI — Central European}
  AddCodePage('WIN1251', CS_WIN1251, ceAnsi, zCP_WIN1251); {ANSI — Cyrillic}
  AddCodePage('WIN1252', CS_WIN1252, ceAnsi, zCP_WIN1252); {ANSI — Latin I}
  AddCodePage('WIN1253', CS_WIN1253, ceAnsi, zCP_WIN1253); {ANSI Greek}
  AddCodePage('WIN1254', CS_WIN1254, ceAnsi, zCP_WIN1254); {ANSI Turkish}
  //FB 1.5
  AddCodePage('DOS737', CS_DOS737, ceAnsi, zCP_DOS737); {Greek}
  AddCodePage('DOS775', CS_DOS775, ceAnsi, zCP_DOS775); {Baltic}
  AddCodePage('DOS858', CS_DOS858, ceAnsi, zCP_DOS858); {Latin I + Euro symbol}
  AddCodePage('DOS862', CS_DOS862, ceAnsi, zCP_DOS862); {Hebrew}
  AddCodePage('DOS864', CS_DOS864, ceAnsi, zCP_DOS864); {Arabic}
  AddCodePage('DOS866', CS_DOS866, ceAnsi, zCP_DOS866); {Russian}
  AddCodePage('DOS869', CS_DOS869, ceAnsi, zCP_DOS869); {Modern Greek}
  AddCodePage('ISO8859_2', CS_ISO8859_2, ceAnsi, zCP_L2_ISO_8859_2); {Latin 2 —  Latin3 — Southern European (Maltese, Esperanto)}
  AddCodePage('ISO8859_3', CS_ISO8859_3, ceAnsi, zCP_L3_ISO_8859_3); {Latin 1}
  AddCodePage('ISO8859_4', CS_ISO8859_4, ceAnsi, zCP_L4_ISO_8859_4); {Latin 4 — Northern European (Estonian, Latvian, Lithuanian, Greenlandic, Lappish)}
  AddCodePage('ISO8859_5', CS_ISO8859_5, ceAnsi, zCP_L5_ISO_8859_5); {Cyrillic (Russian)}
  AddCodePage('ISO8859_6', CS_ISO8859_6, ceAnsi, zCP_L6_ISO_8859_6); {Arabic}
  AddCodePage('ISO8859_7', CS_ISO8859_7, ceAnsi, zCP_L7_ISO_8859_7); {Greek}
  AddCodePage('ISO8859_8', CS_ISO8859_8, ceAnsi, zCP_L8_ISO_8859_8); {Hebrew}
  AddCodePage('ISO8859_9', CS_ISO8859_9, ceAnsi, zCP_L5_ISO_8859_9); {Latin 5}
  AddCodePage('ISO8859_13', CS_ISO8859_13, ceAnsi, zCP_L7_ISO_8859_13); {Latin 7 — Baltic Rim}
  AddCodePage('WIN1255', CS_WIN1255, ceAnsi, zCP_WIN1255); {ANSI Hebrew}
  AddCodePage('WIN1256', CS_WIN1256, ceAnsi, zCP_WIN1256); {ANSI Arabic}
  AddCodePage('WIN1257', CS_WIN1257, ceAnsi, zCP_WIN1257); {ANSI Baltic}
  //FB 2.0
  AddCodePage('WIN1258', CS_WIN1258, ceAnsi, zCP_WIN1258); {Vietnamese}
  AddCodePage('KOI8R', CS_KOI8R, ceAnsi, zCP_KOI8R); {Russian}
  AddCodePage('KOI8U', CS_KOI8U, ceAnsi, zCP_KOI8U); {Ukrainian}
  AddCodePage('UTF8', CS_UTF8, ceUTF8, zCP_UTF8, '', 4); {All}
  //FB 2.1
  AddCodePage('CP943C', CS_CP943C, ceAnsi, 943, '', 2); {Japanese}
  AddCodePage('GBK', CS_GBK, ceAnsi, zCP_GB2312, '', 2); {Chinese}
  AddCodePage('TIS620', CS_TIS620, ceAnsi, zCP_WIN874); {Thai}
  //FB 2.5
  ResetCodePage(CS_BIG_5, 'BIG_5', CS_BIG_5, ceAnsi, zCP_BIG5, '', 2); {Chinese, Vietnamese, Korean} //Changed Bytes
  AddCodePage('GB18030', CS_GB18030, ceAnsi, zCP_GB18030, '', 4); {Chinese}
end;

{$IFDEF ENABLE_INTERBASE_CRYPT}
procedure TZInterbasePlainDriver.Initialize(const Location: String = '');
begin
  If Assigned(FPreLoader) and not FPreLoader.Loaded then
    FPreLoader.LoadNativeLibrary;
  inherited Initialize(Location);
end;
{$ENDIF}

procedure TZInterbasePlainDriver.LoadApi;
begin
  inherited LoadApi;
  with Loader do
  begin
    @isc_sqlcode         := GetAddress('isc_sqlcode');
    @isc_sql_interprete  := GetAddress('isc_sql_interprete');
    @isc_interprete      := GetAddress('isc_interprete');
    @isc_vax_integer     := GetAddress('isc_vax_integer');
    @isc_portable_integer:= GetAddress('isc_portable_integer');

    @isc_array_gen_sdl   := GetAddress( 'isc_array_gen_sdl');
    @isc_array_get_slice := GetAddress( 'isc_array_get_slice');
    @isc_array_lookup_bounds := GetAddress( 'isc_array_lookup_bounds');
    @isc_array_lookup_desc := GetAddress( 'isc_array_lookup_desc');
    @isc_array_set_desc  := GetAddress( 'isc_array_set_desc');
    @isc_array_put_slice := GetAddress( 'isc_array_put_slice');

    @isc_blob_info       := GetAddress('isc_blob_info');
    @isc_open_blob2      := GetAddress('isc_open_blob2');
    @isc_close_blob      := GetAddress('isc_close_blob');
    @isc_cancel_blob     := GetAddress('isc_cancel_blob');
    @isc_get_segment     := GetAddress('isc_get_segment');
    @isc_put_segment     := GetAddress('isc_put_segment');
    @isc_create_blob2    := GetAddress('isc_create_blob2');
    //@isc_decode_date     := GetAddress('isc_decode_date');
    //@isc_encode_date     := GetAddress('isc_encode_date');
    @isc_dsql_free_statement := GetAddress('isc_dsql_free_statement');
    @isc_dsql_execute2   := GetAddress('isc_dsql_execute2');
    @isc_dsql_execute    := GetAddress('isc_dsql_execute');
    @isc_dsql_set_cursor_name := GetAddress('isc_dsql_set_cursor_name');
    @isc_dsql_fetch      := GetAddress('isc_dsql_fetch');
    @isc_dsql_sql_info   := GetAddress('isc_dsql_sql_info');
    @isc_dsql_allocate_statement := GetAddress('isc_dsql_allocate_statement');
    @isc_dsql_alloc_statement2 := GetAddress('isc_dsql_alloc_statement2');
    @isc_dsql_prepare    := GetAddress('isc_dsql_prepare');
    @isc_dsql_describe_bind := GetAddress('isc_dsql_describe_bind');
    @isc_dsql_describe   := GetAddress('isc_dsql_describe');
    @isc_dsql_execute_immediate := GetAddress('isc_dsql_execute_immediate');
    @isc_drop_database   := GetAddress('isc_drop_database');
    @isc_detach_database := GetAddress('isc_detach_database');
    @isc_attach_database := GetAddress('isc_attach_database');
    @isc_database_info   := GetAddress('isc_database_info');
    @isc_create_database := GetAddress('isc_create_database');
    @isc_transaction_info   := GetAddress('isc_transaction_info');
    @isc_start_multiple  := GetAddress('isc_start_multiple');
    @isc_start_transaction := GetAddress('isc_start_transaction');
    @isc_commit_transaction := GetAddress('isc_commit_transaction');

    @isc_commit_retaining := GetAddress('isc_commit_retaining');
    @isc_rollback_transaction := GetAddress('isc_rollback_transaction');
    @isc_cancel_events   := GetAddress('isc_cancel_events');
    @isc_que_events      := GetAddress('isc_que_events');
    @isc_event_counts    := GetAddress('isc_event_counts');
    @isc_event_block     := GetAddress('isc_event_block');
    @isc_free            := GetAddress('isc_free');

    @isc_rollback_retaining := GetAddress( 'isc_rollback_retaining');
    //@isc_decode_sql_date := GetAddress('isc_decode_sql_date');
    //@isc_decode_sql_time := GetAddress('isc_decode_sql_time');
    //@isc_decode_timestamp := GetAddress('isc_decode_timestamp');
    //@isc_encode_sql_date := GetAddress('isc_encode_sql_date');
    //@isc_encode_sql_time := GetAddress('isc_encode_sql_time');
    //@isc_encode_timestamp := GetAddress('isc_encode_timestamp');

    @fb_interpret        := GetAddress('fb_interpret');

    @isc_get_client_version := GetAddress('isc_get_client_version');
    @isc_get_client_major_version := GetAddress('isc_get_client_major_version');
    @isc_get_client_minor_version := GetAddress('isc_get_client_minor_version');
  end;
end;

function TZInterbasePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZInterbasePlainDriver.Create;
end;

constructor TZInterbasePlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF ENABLE_INTERBASE_CRYPT}
  FPreLoader := TZNativeLibraryLoader.Create([LINUX_IB_CRYPT_LOCATION]);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FLoader.AddLocation(WINDOWSIB6_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS2_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS2_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS15_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS15_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS20_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS20_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS21_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS21_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS25_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS25_DLL_LOCATION_EMBEDDED);
  {$ELSE}
  FLoader.AddLocation(LINUXIB6_DLL_LOCATION);
  FLoader.AddLocation(LINUX_DLL_LOCATION);
  FLoader.AddLocation(LINUX_IB_CRYPT_LOCATION);
  FLoader.AddLocation(LINUX2_DLL_LOCATION);
  FLoader.AddLocation(LINUX2_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX2_IB_CRYPT_LOCATION);
  FLoader.AddLocation(LINUX15_DLL_LOCATION);
  FLoader.AddLocation(LINUX15_IB_CRYPT_LOCATION);
  FLoader.AddLocation(LINUX15_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX2_DLL_LOCATION2);
  FLoader.AddLocation(LINUX20_DLL_LOCATION);
  FLoader.AddLocation(LINUX20_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX20_IB_CRYPT_LOCATION);
  FLoader.AddLocation(LINUX21_DLL_LOCATION);
  FLoader.AddLocation(LINUX21_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX21_IB_CRYPT_LOCATION);
  FLoader.AddLocation(LINUX25_DLL_LOCATION);
  FLoader.AddLocation(LINUX25_DLL_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX25_IB_CRYPT_LOCATION);
  {$ENDIF}
  LoadCodePages;
  FillCodePageArray;
end;

{$IFDEF ENABLE_INTERBASE_CRYPT}
destructor TZInterbasePlainDriver.Destroy;
begin
  FPreLoader.Free;
  inherited Destroy;
end;
{$ENDIF}

function TZInterbasePlainDriver.GetCodePageArray: TWordDynArray;
begin
  Result := FCodePageArray;
end;

function TZInterbasePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Interbase';
end;

function TZInterbasePlainDriver.GetProtocol: string;
begin
  Result := 'interbase';
end;

{ TZFirebirdPlainDriver }

function TZFirebirdPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFirebirdPlainDriver.Create;
end;

function TZFirebirdPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird';
end;

function TZFirebirdPlainDriver.GetProtocol: string;
begin
  Result := 'firebird';
end;

{$ENDIF ZEOS_DISABLE_INTERBASE}

end.
