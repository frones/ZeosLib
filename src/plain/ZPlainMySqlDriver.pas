{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZPlainDriver, ZCompatibility, ZPlainMySqlConstants;

const
  MARIADB_LOCATION = 'libmariadb'+ SharedSuffix;
  DLL_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix;
{$IFDEF MSWINDOWS}
  WINDOWS_DLL_LOCATION = 'libmysql.dll';
  WINDOWS_DLL41_LOCATION = 'libmysql41.dll';
  WINDOWS_DLL41_LOCATION_EMBEDDED = 'libmysqld41.dll';
  WINDOWS_DLL50_LOCATION = 'libmysql50.dll';
  WINDOWS_DLL50_LOCATION_EMBEDDED = 'libmysqld50.dll';
  WINDOWS_DLL51_LOCATION = 'libmysql51.dll';
  WINDOWS_DLL51_LOCATION_EMBEDDED = 'libmysqld51.dll';
  WINDOWS_DLL55_LOCATION = 'libmysql55.dll';
  WINDOWS_DLL55_LOCATION_EMBEDDED = 'libmysqld55.dll';
  WINDOWS_DLL56_LOCATION = 'libmysql56.dll';
  WINDOWS_DLL56_LOCATION_EMBEDDED = 'libmysqld56.dll';
  WINDOWS_DLL57_LOCATION = 'libmysql57.dll';
  WINDOWS_DLL57_LOCATION_EMBEDDED = 'libmysqld57.dll';
{$ELSE}
  LINUX_DLL_LOCATION = 'libmysqlclient'+SharedSuffix;
  LINUX_DLL41_LOCATION = 'libmysqlclient'+SharedSuffix+'.14';
  LINUX_DLL41_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.14';
  LINUX_DLL50_LOCATION = 'libmysqlclient'+SharedSuffix+'.15';
  LINUX_DLL50_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.15';
  LINUX_DLL51_LOCATION = 'libmysqlclient'+SharedSuffix+'.16';
  LINUX_DLL51_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.16';
  LINUX_DLL55_LOCATION = 'libmysqlclient'+SharedSuffix+'.18';
  LINUX_DLL55_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.18';
  LINUX_DLL56_LOCATION = 'libmysqlclient'+SharedSuffix+'.19';
  LINUX_DLL56_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.19';
  LINUX_DLL57_LOCATION = 'libmysqlclient'+SharedSuffix+'.20';
  LINUX_DLL57_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.20';
{$ENDIF}

type
  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    function IsMariaDBDriver: Boolean;

    function Init(const mysql: PMYSQL): PMYSQL;

    {non API functions}
    procedure SetDriverOptions(Options: TStrings); // changed by tohenk, 2009-10-11
  end;

  {** Implements a base driver for MySQL}

  { TZMySQLPlainDriver }

  TZMySQLPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZMySQLPlainDriver)
  public
    FIsMariaDBDriver: Boolean;
    { ************** Plain API Function types definition ************* }
    { Functions to get information from the MYSQL and MYSQL_RES structures
      Should definitely be used if one uses shared libraries. }
    //mysql_get_character_set_info: procedure(mysql: PMYSQL; cs: PMY_CHARSET_INFO); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_affected_rows:          function( mysql: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_character_set_name:     function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_close:                  procedure(mysql: PMYSQL); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_connect:                function(mysql: PMYSQL; const Host, User, Passwd: PAnsiChar): PMYSQL;   {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_create_db:              function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_data_seek:              procedure(Result: PMYSQL_RES; Offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_debug:                  procedure(Debug: PAnsiChar); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_drop_db:                function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_dump_debug_info:        function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_eof:                    function(Result: PMYSQL_RES): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_errno:                  function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_error:                  function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_escape_string:          function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field:            function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field_direct:     function(Result: PMYSQL_RES; FieldNo: UInt): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_fields:           function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_lengths:          function(Result: PMYSQL_RES): PULongArray; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_row:              function(Result: PMYSQL_RES): PMYSQL_ROW; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_seek:             function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_tell:             function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_free_result:            procedure(Result: PMYSQL_RES); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_info:        function: PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_host_info:          function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_get_proto_info:         function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_info:        function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_info:                   function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_init:                   function(mysql: PMYSQL): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_insert_id:              function(mysql: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_kill:                   function(mysql: PMYSQL; Pid: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_list_dbs:               function(mysql: PMYSQL; Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_list_fields:            function(mysql: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_list_processes:         function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_list_tables:            function(mysql: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_fields:             function(Result: PMYSQL_RES): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_rows:               function(Result: PMYSQL_RES): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_options:                function(mysql: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ping:                   function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_query:                  function(mysql: PMYSQL; const Query: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_connect:           function(mysql: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: UInt; const UnixSocket: PAnsiChar; ClientFlag: ULong): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_escape_string:     function(mysql: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_query:             function(mysql: PMYSQL; const Query: PAnsiChar; Length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_refresh:                function(mysql: PMYSQL; Options: UInt): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_row_seek:               function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_row_tell:               function(Result: PMYSQL_RES): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_select_db:              function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ssl_set:                function(mysql: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_stat:                   function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_store_result:           function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_thread_id:              function(mysql: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_use_result:             function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Set up and bring down a thread; these function should be called for each thread in an application which
      opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }
    my_init:                      procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_init:            function: Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_end:             procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_safe:            function: UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Set up and bring down the server; to ensure that applications will work when linked against either the
      standard client library or the embedded server library, these functions should be called. }
    mysql_server_init:            function(Argc: Integer; Argv, Groups: Pointer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //deprecated
    mysql_library_init:           function(Argc: Integer; Argv, Groups: Pointer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_server_end:             procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //deprecated
    mysql_library_end:            procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_change_user:            function(mysql: PMYSQL; const user: PAnsiChar; const passwd: PAnsiChar; const db: PAnsiChar): Byte;
    mysql_field_count:            function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_version:     function: ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_send_query:             function(mysql: PMYSQL; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_read_query_result:      function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_autocommit:             function(mysql: PMYSQL; mode: my_bool): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_commit:                 function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_version:     function(mysql: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_hex_string:             function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_more_results:           function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_next_result:            function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_rollback:               function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_character_set:      function(mysql: PMYSQL; const csname: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_server_option:      function(mysql: PMYSQL; Option: TMysqlSetOption): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_shutdown:               function(mysql: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_sqlstate:               function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_warning_count:          function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    {BELOW are new PREPARED STATEMENTS}
    mysql_stmt_affected_rows:     function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_get:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; attr: Pointer): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set517UP:     function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const attr: Pointer): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const attr: Pointer): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_param:        function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_result:       function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_close:             function(stmt: PMYSQL_STMT): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_data_seek:         procedure(stmt: PMYSQL_STMT; offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_errno:             function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_error:             function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_execute:           function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch:             function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch_column:      function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_field_count:       function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_free_result:       function(stmt: PMYSQL_STMT): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_init:              function(mysql: PMYSQL): PMYSQL_STMT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_insert_id:         function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_next_result:       function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_num_rows:          function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_count:       function(stmt: PMYSQL_STMT): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_metadata:    function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_prepare:           function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_reset:             function(stmt: PMYSQL_STMT): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_result_metadata:   function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_stmt_row_seek:          function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    //mysql_stmt_row_tell:          function(stmt: PMYSQL_STMT): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_send_long_data:    function(stmt: PMYSQL_STMT; param_number: UInt; const data: PAnsiChar; length: ULong): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_sqlstate:          function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_store_result:      function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_more_results:      function(stmt: PMYSQL_STMT): my_bool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mariadb_stmt_execute_direct:  function(stmt: PMYSQL_STMT; query: PAnsiChar; Length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  protected
    ServerArgs: array of PAnsiChar;
    ServerArgsRaw: array of RawByteString;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
    procedure BuildServerArguments(const Options: TStrings);
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    destructor Destroy; override;

    function IsMariaDBDriver: Boolean;
    function Init(const mysql: PMYSQL): PMYSQL; virtual;

    procedure SetDriverOptions(Options: TStrings); virtual; // changed by tohenk, 2009-10-11
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}

implementation

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses SysUtils, ZPlainLoader, ZEncoding, ZFastCode, ZConnProperties
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZMySQLPlainBaseDriver }
function TZMySQLPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL or MariaDB';
end;

function TZMySQLPlainDriver.GetProtocol: string;
begin
  Result := 'mysql'
end;

function TZMySQLPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'utf8';
end;

procedure TZMySQLPlainDriver.LoadCodePages;
begin
  {MySQL 3.23-4.1}
  { MultiByte }
  AddCodePage('big5', 1, ceAnsi, zCP_Big5, '', 2); {Big5 Traditional Chinese}
  AddCodePage('ujis', 10, ceAnsi, zCP_EUC_JP, '', 3); {EUC-JP Japanese}
  AddCodePage('sjis', 11, ceAnsi, zCP_SHIFTJS, '', 2); {Shift-JIS Japanese}
  AddCodePage('gbk', 19, ceAnsi, zCP_GB2312, '', 2); {GBK Simplified Chinese}
  AddCodePage('utf8', 22, ceUTF8, zCP_UTF8, '', 3); {UTF-8 Unicode}
  AddCodePage('ucs2', 23, ceUTF16, zCP_UTF16, 'utf8', 2); {UCS-2 Unicode}
  AddCodePage('euckr', 14, ceAnsi, zCP_EUCKR, '', 2); {EUC-KR Korean}
  AddCodePage('gb2312', 16, ceAnsi, zCP_GB2312, '', 2); {GB2312 Simplified Chinese}
  AddCodePage('cp932', 35, ceAnsi, zCP_SHIFTJS, '', 2); {SJIS for Windows Japanese}
  AddCodePage('eucjpms', 36, ceAnsi, $ffff, '', 3); {UJIS for Windows Japanese}
  { SingleChar }
  AddCodePage('dec8', 2); {DEC West European}
  AddCodePage('cp850', 3, ceAnsi, zCP_DOS850); {DOS West European}
  AddCodePage('hp8', 4); {HP West European}
  AddCodePage('koi8r', 5, ceAnsi, zCP_KOI8R); {KOI8-R Relcom Russian}
  AddCodePage('latin1', 6, ceAnsi, zCP_WIN1252); {cp1252 West European}
  AddCodePage('latin2', 7, ceAnsi, zCP_L2_ISO_8859_2); {ISO 8859-2 Central European}
  AddCodePage('swe7', 8, ceAnsi, zCP_x_IA5_Swedish); {7bit Swedish}
  AddCodePage('ascii', 9, ceAnsi, zCP_us_ascii); {US ASCII}
  AddCodePage('hebrew', 12, ceAnsi, zCP_L8_ISO_8859_8); {ISO 8859-8 Hebrew}
  AddCodePage('tis620', 13, ceAnsi, zCP_WIN874); {TIS620 Thai}
  AddCodePage('koi8u', 15, ceAnsi, zCP_KOI8U); {KOI8-U Ukrainian}
  AddCodePage('greek', 17, ceAnsi, zCP_L7_ISO_8859_7); {ISO 8859-7 Greek}
  AddCodePage('cp1250', 18, ceAnsi, zCP_WIN1250); {Windows Central European}
  AddCodePage('latin5', 20, ceAnsi, zCP_L5_ISO_8859_9); {ISO 8859-9 Turkish}
  AddCodePage('armscii8', 21, ceAnsi, zCP_us_ascii); {ARMSCII-8 Armenian}
  AddCodePage('cp866', 24, ceAnsi, zCP_DOS866); {DOS Russian}
  AddCodePage('keybcs2', 25); {DOS Kamenicky Czech-Slovak}
  AddCodePage('macce', 26, ceAnsi, zCP_x_mac_ce); {Mac Central European}
  AddCodePage('macroman', 27, ceAnsi, zCP_macintosh); {Mac West European}
  AddCodePage('cp852', 28, ceAnsi, zCP_DOS852); {DOS Central European}
  AddCodePage('latin7', 29, ceAnsi, zCP_L7_ISO_8859_13); {ISO 8859-13 Baltic}
  AddCodePage('cp1251', 30, ceAnsi, zCP_WIN1251); {Windows Cyrillic}
  AddCodePage('cp1256', 31, ceAnsi, zCP_WIN1256); {Windows Arabic}
  AddCodePage('cp1257', 32, ceAnsi, zCP_WIN1257); {Windows Baltic}
  AddCodePage('binary', 33); {Binary pseudo charset}
  AddCodePage('geostd8', 34); {GEOSTD8 Georgian}
  {MySQL 4.1-5.5}
  { MultiByte }
  AddCodePage('utf8mb4', 37, ceUTF8, zCP_UTF8, '', 4); {UTF-8 Unicode}
  //AddCodePage('utf16', 38, ceUTF16, zCP_UTF16, 'utf8', 4); {UTF-16 Unicode}
  //AddCodePage('utf32', 39, ceUTF16, zCP_utf32, 'utf8', 4); {UTF-32 Unicode} //Egonhugeist improved
end;

procedure TZMySQLPlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do begin
  @mysql_affected_rows          := GetAddress('mysql_affected_rows');
  @mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @mysql_close                  := GetAddress('mysql_close');
  //@mysql_connect                := GetAddress('mysql_connect');
  @mysql_create_db              := GetAddress('mysql_create_db');
  @mysql_data_seek              := GetAddress('mysql_data_seek');
  //@mysql_debug                  := GetAddress('mysql_debug');
  @mysql_drop_db                := GetAddress('mysql_drop_db');
  //@mysql_dump_debug_info        := GetAddress('mysql_dump_debug_info');
  @mysql_eof                    := GetAddress('mysql_eof');
  @mysql_errno                  := GetAddress('mysql_errno');
  @mysql_error                  := GetAddress('mysql_error');
  @mysql_escape_string          := GetAddress('mysql_escape_string');
  @mysql_fetch_field            := GetAddress('mysql_fetch_field');
  @mysql_fetch_field_direct     := GetAddress('mysql_fetch_field_direct');
  @mysql_fetch_fields           := GetAddress('mysql_fetch_fields');
  @mysql_fetch_lengths          := GetAddress('mysql_fetch_lengths');
  @mysql_fetch_row              := GetAddress('mysql_fetch_row');
  @mysql_field_seek             := GetAddress('mysql_field_seek');
  @mysql_field_tell             := GetAddress('mysql_field_tell');
  @mysql_free_result            := GetAddress('mysql_free_result');
  @mysql_get_client_info        := GetAddress('mysql_get_client_info');
  @mysql_get_host_info          := GetAddress('mysql_get_host_info');
  //@mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @mysql_get_server_info        := GetAddress('mysql_get_server_info');
  @mysql_info                   := GetAddress('mysql_info');
  @mysql_init                   := GetAddress('mysql_init');
  @mysql_insert_id              := GetAddress('mysql_insert_id');
  //@mysql_kill                   := GetAddress('mysql_kill');
  //@mysql_list_dbs               := GetAddress('mysql_list_dbs');
  //@mysql_list_fields            := GetAddress('mysql_list_fields');
  //@mysql_list_processes         := GetAddress('mysql_list_processes');
  //@mysql_list_tables            := GetAddress('mysql_list_tables');
  @mysql_num_fields             := GetAddress('mysql_num_fields');
  @mysql_num_rows               := GetAddress('mysql_num_rows');
  @mysql_options                := GetAddress('mysql_options');
  @mysql_ping                   := GetAddress('mysql_ping');
  //@mysql_query                  := GetAddress('mysql_query');
  @mysql_real_connect           := GetAddress('mysql_real_connect');
  @mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @mysql_real_query             := GetAddress('mysql_real_query');
  //@mysql_refresh                := GetAddress('mysql_refresh');
  //@mysql_row_seek               := GetAddress('mysql_row_seek');
  @mysql_row_tell               := GetAddress('mysql_row_tell');
  @mysql_select_db              := GetAddress('mysql_select_db');
  //@mysql_shutdown               := GetAddress('mysql_shutdown');
  @mysql_ssl_set                := GetAddress('mysql_ssl_set');
  //@mysql_stat                   := GetAddress('mysql_stat');
  @mysql_store_result           := GetAddress('mysql_store_result');
  //@mysql_thread_id              := GetAddress('mysql_thread_id');
  @mysql_use_result             := GetAddress('mysql_use_result');

  @my_init                      := GetAddress('my_init');
  @mysql_thread_init            := GetAddress('mysql_thread_init');
  @mysql_thread_end             := GetAddress('mysql_thread_end');
  @mysql_thread_safe            := GetAddress('mysql_thread_safe');

  @mysql_server_init            := GetAddress('mysql_server_init'); //deprecated
  @mysql_library_init           := GetAddress('mysql_library_init');
  @mysql_server_end             := GetAddress('mysql_server_end');  //deprecated
  @mysql_library_end            := GetAddress('mysql_library_end');

  @mysql_change_user            := GetAddress('mysql_change_user');
  @mysql_field_count            := GetAddress('mysql_field_count');

  @mysql_get_client_version     := GetAddress('mysql_get_client_version');

  @mysql_send_query             := GetAddress('mysql_send_query');
  @mysql_read_query_result      := GetAddress('mysql_read_query_result');

  @mysql_autocommit             := GetAddress('mysql_autocommit');
  @mysql_commit                 := GetAddress('mysql_commit');
  @mysql_get_server_version     := GetAddress('mysql_get_server_version');
  @mysql_hex_string             := GetAddress('mysql_hex_string');
  @mysql_more_results           := GetAddress('mysql_more_results');
  @mysql_next_result            := GetAddress('mysql_next_result');
  @mysql_rollback               := GetAddress('mysql_rollback');
  @mysql_set_character_set      := GetAddress('mysql_set_character_set');
  @mysql_set_server_option      := GetAddress('mysql_set_server_option');
  //@mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @mysql_warning_count          := GetAddress('mysql_warning_count');
  {API for PREPARED STATEMENTS}
  @mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
  @mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
  @mariadb_stmt_execute_direct  := GetAddress('mariadb_stmt_execute_direct');

  //http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-attr-set.html
  //http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-attr-set.html
  if Assigned(mariadb_stmt_execute_direct) or (mysql_get_client_version >= 50107) //avoid heap crashs !
  then @mysql_stmt_attr_set517UP := GetAddress('mysql_stmt_attr_set') //uses mybool
  else @mysql_stmt_attr_set      := GetAddress('mysql_stmt_attr_set'); //uses ulong

  @mysql_stmt_bind_param        := GetAddress('mysql_stmt_bind_param');
  @mysql_stmt_bind_result       := GetAddress('mysql_stmt_bind_result');
  @mysql_stmt_close             := GetAddress('mysql_stmt_close');
  @mysql_stmt_data_seek         := GetAddress('mysql_stmt_data_seek');
  @mysql_stmt_errno             := GetAddress('mysql_stmt_errno');
  @mysql_stmt_error             := GetAddress('mysql_stmt_error');
  @mysql_stmt_execute           := GetAddress('mysql_stmt_execute');
  @mysql_stmt_fetch             := GetAddress('mysql_stmt_fetch');
  @mysql_stmt_fetch_column      := GetAddress('mysql_stmt_fetch_column');
  @mysql_stmt_field_count       := GetAddress('mysql_stmt_field_count');
  @mysql_stmt_free_result       := GetAddress('mysql_stmt_free_result');
  @mysql_stmt_init              := GetAddress('mysql_stmt_init');
  @mysql_stmt_insert_id         := GetAddress('mysql_stmt_insert_id');
  @mysql_stmt_num_rows          := GetAddress('mysql_stmt_num_rows');
  @mysql_stmt_param_count       := GetAddress('mysql_stmt_param_count');
  @mysql_stmt_param_metadata    := GetAddress('mysql_stmt_param_metadata');
  @mysql_stmt_prepare           := GetAddress('mysql_stmt_prepare');
  @mysql_stmt_reset             := GetAddress('mysql_stmt_reset');
  @mysql_stmt_result_metadata   := GetAddress('mysql_stmt_result_metadata');
  //@mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
  //@mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
  @mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
  @mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
  @mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');
  @mysql_stmt_more_results      := GetAddress('mysql_stmt_more_results');
  //@mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
  @mysql_stmt_next_result       := GetAddress('mysql_stmt_next_result');
  end;
end;

procedure TZMySQLPlainDriver.BuildServerArguments(const Options: TStrings);
var
  TmpList: TStringList;
  i: Integer;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Add(ParamStr(0));
    for i := 0 to Options.Count - 1 do
      if SameText(SERVER_ARGUMENTS_KEY_PREFIX,
                  Copy(Options.Names[i], 1,
                       Length(SERVER_ARGUMENTS_KEY_PREFIX))) then
        TmpList.Add(Options.ValueFromIndex[i]);
    //Check if DataDir is specified, if not, then add it to the Arguments List
    if TmpList.Values[ConnProps_Datadir] = '' then
       TmpList.Values[ConnProps_Datadir] := EMBEDDED_DEFAULT_DATA_DIR;

    SetLength(ServerArgs, TmpList.Count);
    SetLength(ServerArgsRaw, TmpList.Count);
    for i := 0 to TmpList.Count - 1 do begin
      {$IFDEF UNICODE}
      ServerArgsRaw[i] := ZUnicodeToRaw(TmpList[i], ZOSCodePage);
      {$ELSE}
      ServerArgsRaw[i] := TmpList[i];
      {$ENDIF}
      ServerArgs[i] :=  Pointer(TmpList[i]);
    end;
  finally
    {$IFDEF AUTOREFCOUNT}
    TmpList := nil;
    {$ELSE}
    TmpList.Free;
    {$ENDIF}
  end;
end;

function TZMySQLPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQLPlainDriver.Create;
end;

constructor TZMySQLPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  FLoader.AddLocation(MARIADB_LOCATION);
  FLoader.AddLocation(DLL_LOCATION_EMBEDDED);
{$IFDEF MSWINDOWS}
  FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL41_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL41_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS_DLL50_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL50_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS_DLL51_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL51_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS_DLL55_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL55_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS_DLL56_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL56_LOCATION_EMBEDDED);
  FLoader.AddLocation(WINDOWS_DLL57_LOCATION);
  FLoader.AddLocation(WINDOWS_DLL57_LOCATION_EMBEDDED);
{$ELSE}
  FLoader.AddLocation(LINUX_DLL_LOCATION);
  FLoader.AddLocation(LINUX_DLL41_LOCATION);
  FLoader.AddLocation(LINUX_DLL41_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX_DLL50_LOCATION);
  FLoader.AddLocation(LINUX_DLL50_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX_DLL51_LOCATION);
  FLoader.AddLocation(LINUX_DLL51_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX_DLL55_LOCATION);
  FLoader.AddLocation(LINUX_DLL55_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX_DLL56_LOCATION);
  FLoader.AddLocation(LINUX_DLL56_LOCATION_EMBEDDED);
  FLoader.AddLocation(LINUX_DLL57_LOCATION);
  FLoader.AddLocation(LINUX_DLL57_LOCATION_EMBEDDED);
{$ENDIF}
  LoadCodePages;
end;

destructor TZMySQLPlainDriver.Destroy;
begin
  SetLength(ServerArgs, 0);
  SetLength(ServerArgsRaw, 0);

  if (FLoader.Loaded) then
    if Assigned(mysql_library_end) then
      mysql_library_end //since 5.0.3
    else if Assigned(mysql_server_end) then
      mysql_server_end; //deprected since 5.0.3
  inherited Destroy;
end;

function TZMySQLPlainDriver.IsMariaDBDriver: Boolean;
begin
  Result := FIsMariaDBDriver;
end;

function TZMySQLPlainDriver.Init(const mysql: PMYSQL): PMYSQL;
var
  ClientInfo: PAnsiChar;
  L: LengthInt;
  ErrorNo: Integer;
begin
  if (Assigned(mysql_server_init) or Assigned(mysql_library_init)){ and (ServerArgsLen > 0) }then begin
    ErrorNo := Length(ServerArgs);
    if Assigned(mysql_library_init) then //http://dev.mysql.com/doc/refman/5.7/en/mysql-library-init.html
      ErrorNo := mysql_library_init(ErrorNo, ServerArgs, @SERVER_GROUPS) //<<<-- Isn't threadsafe
    else //http://dev.mysql.com/doc/refman/5.7/en/mysql-server-init.html
      ErrorNo := mysql_server_init(ErrorNo, ServerArgs, @SERVER_GROUPS); //<<<-- Isn't threadsafe
    if ErrorNo <> 0 then
      raise Exception.Create('Could not initialize the MySQL / MariaDB client library. Error No: ' + ZFastCode.IntToStr(ErrorNo));  // The manual says nothing else can be called until this call succeeds. So lets just throw the error number...
  end;
  Result := mysql_init(mysql);
  if not Assigned(Result) then
    raise Exception.Create('Could not finish the call to mysql_init. Not enough memory?');
  ClientInfo := mysql_get_client_info;
  L := ZFastCode.StrLen(ClientInfo);
  FIsMariaDBDriver := Assigned(mariadb_stmt_execute_direct) or CompareMem(ClientInfo+L-7, PAnsiChar('MariaDB'), 7);
end;

procedure TZMySQLPlainDriver.SetDriverOptions(Options: TStrings);
var
  PreferedLibrary: String;
begin
  PreferedLibrary := Options.Values[ConnProps_Library];
  if PreferedLibrary <> '' then
    Loader.AddLocation(PreferedLibrary);
  if Assigned(mysql_library_init) and Assigned(mysql_library_end) then
    BuildServerArguments(Options);
end;

{$ENDIF ZEOS_DISABLE_MYSQL}

end.


