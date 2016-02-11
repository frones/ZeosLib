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

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZPlainDriver, ZCompatibility, ZPlainMySqlConstants;

const
  MARIADB_LOCATION = 'libmariadb'+ SharedSuffix;
{$IFNDEF UNIX}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  WINDOWS_DLL_LOCATION = 'libmysql.dll';
  WINDOWS_DLL_LOCATION_EMBEDDED = 'libmysqld.dll';
  {$ENDIF}
  WINDOWS_DLL41_LOCATION = 'libmysql41.dll';
  WINDOWS_DLL41_LOCATION_EMBEDDED = 'libmysqld41.dll';
  WINDOWS_DLL50_LOCATION = 'libmysql50.dll';
  WINDOWS_DLL50_LOCATION_EMBEDDED = 'libmysqld50.dll';
  WINDOWS_DLL51_LOCATION = 'libmysql51.dll';
  WINDOWS_DLL51_LOCATION_EMBEDDED = 'libmysqld51.dll';
  WINDOWS_DLL55_LOCATION = 'libmysql55.dll';
  WINDOWS_DLL55_LOCATION_EMBEDDED = 'libmysqld55.dll';
{$ELSE}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  LINUX_DLL_LOCATION = 'libmysqlclient'+SharedSuffix;
  LINUX_DLL_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix;
  {$ENDIF}
  LINUX_DLL41_LOCATION = 'libmysqlclient'+SharedSuffix+'.14';
  LINUX_DLL41_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.14';
  LINUX_DLL50_LOCATION = 'libmysqlclient'+SharedSuffix+'.15';
  LINUX_DLL50_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.15';
  LINUX_DLL51_LOCATION = 'libmysqlclient'+SharedSuffix+'.16';
  LINUX_DLL51_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.16';
  LINUX_DLL55_LOCATION = 'libmysqlclient'+SharedSuffix+'.18';
  LINUX_DLL55_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.18';
{$ENDIF}

type
  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    function IsMariaDBDriver: Boolean;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}

    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    function character_set_name(Handle: PMYSQL): PAnsiChar;// char_set_name
    procedure Close(Handle: PZMySQLConnect);
    function Connect(Handle: PZMySQLConnect; const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function CreateDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    procedure Debug(Debug: PAnsiChar);
    function DropDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    // eof
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function FetchField(Res: PZMySQLResult): PZMySQLField;
    // fetch_field_direct
    // fetch_fields
    function FetchLengths(Res: PZMySQLResult): PMySQLLengthArray;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    // field_tell
    procedure FreeResult(Res: PZMySQLResult);
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
    // info
    function Init(const Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function GetBindOffsets: MYSQL_BINDOFFSETS;
    function GetListDatabases(Handle: PZMySQLConnect; Wild: PAnsiChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect; const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect; const Wild: PAnsiChar): PZMySQLResult;
    // num_fields
    function GetNumRows(Res: PZMySQLResult): Int64;
    function SetOptions(Handle: PZMySQLConnect; Option: TMySQLOption; const Arg: Pointer): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer; overload;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar; Length: Integer): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // row_tell
    function SelectDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function use_result(Handle: PZMySQLConnect): PZMySQLResult;

    // thread_init
    // thread_end
    // thread_safe

    // server_init
    // server_end

    // change_user
    // field_count
    // function GetClientVersion: AnsiString;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!

    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    //function GetServerVersion (Handle: PZMySQLConnect): AnsiString;
    // hex_string
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    {ADDED by EgonHugeist}
    function set_character_set(Handle: PMYSQL; const csname: PAnsiChar): Integer; // set_character_set returns 0 if valid
    // set_server_option
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;
    // warning_count

    function EscapeString(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong;
    function stmt_affected_rows(Handle: PZMySqlPrepStmt): Int64;
    // stmt_attr_get
    function stmt_attr_set(stmt: PZMySqlPrepStmt; option: TMysqlStmtAttrType;
                                  arg: Pointer): Byte;
    function stmt_bind_param(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function stmt_bind_result(Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function stmt_close(PrepStmtHandle: PZMySqlPrepStmt): Byte;
    procedure stmt_data_seek(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function stmt_errno(Handle: PZMySqlPrepStmt): Integer;
    function stmt_error(Handle: PZMySqlPrepStmt): AnsiString;
    function stmt_execute(Handle: PZMySqlPrepStmt): Integer;
    function stmt_fetch(Handle: PZMySqlPrepStmt): Integer;
    function stmt_fetch_column(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer;
    function stmt_field_count(Handle: PZMySqlPrepStmt): Integer;
    function stmt_free_result(Handle: PZMySqlPrepStmt): Byte;
    function stmt_init(Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function stmt_insert_id(Handle: PZMySqlPrepStmt): Int64;
    function stmt_next_result(Handle: PZMySqlPrepStmt): Integer;
    function stmt_num_rows(Handle: PZMySqlPrepStmt): Int64;
    function stmt_param_count(Handle: PZMySqlPrepStmt): Cardinal; // param_count


    function stmt_param_metadata(PrepStmtHandle: PZMySqlPrepStmt): PZMySQLResult;
    function stmt_prepare(PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    function stmt_reset(PrepStmtHandle: PZMySqlPrepStmt): Byte;
    function stmt_result_metadata(Handle: PZMySqlPrepStmt): PZMySQLResult;
    function stmt_row_seek(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // stmt_row_tell
    function stmt_send_long_data(Handle: PZMySqlPrepStmt; parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
    function stmt_sqlstate(Handle: PZMySqlPrepStmt): PAnsiChar;
    function stmt_store_result(Handle: PZMySqlPrepStmt): Integer;

    procedure GetCharacterSetInfo(Handle: PZMySQLConnect; CharSetInfo: PMY_CHARSET_INFO);// get_character_set_info since 5.0.10

    {non API functions}
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure SetDriverOptions(Options: TStrings); // changed by tohenk, 2009-10-11
  end;

  {** Implements a base driver for MySQL}

  { TZMySQLBaseDriver }

  TZMySQLBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZMySQLPlainDriver)
  private
    FIsMariaDBDriver: Boolean;
    { ************** Plain API Function types definition ************* }
    { Functions to get information from the MYSQL and MYSQL_RES structures
      Should definitely be used if one uses shared libraries. }
    mysql_get_character_set_info: procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_affected_rows:          function( Handle: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_character_set_name:     function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_close:                  procedure(Handle: PMYSQL); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_connect:                function(Handle: PMYSQL; const Host, User, Passwd: PAnsiChar): PMYSQL;   {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_create_db:              function(Handle: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_data_seek:              procedure(Result: PMYSQL_RES; Offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_debug:                  procedure(Debug: PAnsiChar); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_drop_db:                function(Handle: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_dump_debug_info:        function(Handle: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_eof:                    function(Result: PMYSQL_RES): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_errno:                  function(Handle: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_error:                  function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_escape_string:          function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field:            function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field_direct:     function(Result: PMYSQL_RES; FieldNo: UInt): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_fields:           function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_lengths:          function(Result: PMYSQL_RES): PMySQLLengthArray; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_row:              function(Result: PMYSQL_RES): PMYSQL_ROW; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_seek:             function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_tell:             function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_free_result:            procedure(Result: PMYSQL_RES); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_info:        function: PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_host_info:          function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_proto_info:         function(Handle: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_info:        function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_info:                   function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_init:                   function(Handle: PMYSQL): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_insert_id:              function(Handle: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_kill:                   function(Handle: PMYSQL; Pid: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_dbs:               function(Handle: PMYSQL; Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_fields:            function(Handle: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_processes:         function(Handle: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_tables:            function(Handle: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_fields:             function(Result: PMYSQL_RES): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_rows:               function(Result: PMYSQL_RES): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_options:                function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ping:                   function(Handle: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_query:                  function(Handle: PMYSQL; const Query: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_connect:           function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: UInt; const UnixSocket: PAnsiChar; ClientFlag: ULong): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_escape_string:     function(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_query:             function(Handle: PMYSQL; const Query: PAnsiChar; Length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_refresh:                function(Handle: PMYSQL; Options: UInt): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_row_seek:               function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_row_tell:               function(Result: PMYSQL_RES): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_select_db:              function(Handle: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ssl_set:                function(Handle: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stat:                   function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_store_result:           function(Handle: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_id:              function(Handle: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_use_result:             function(Handle: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

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
    mysql_field_count:            function(Handle: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_version:     function: ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_send_query:             function(mysql: PMYSQL; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_read_query_result:      function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_autocommit:             function(Handle: PMYSQL; const mode: Byte): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_commit:                 function(Handle: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_version:     function(Handle: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_hex_string:             function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_more_results:           function(Handle: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_next_result:            function(Handle: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_rollback:               function(Handle: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_character_set:      function(Handle: PMYSQL; const csname: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_server_option:      function(Handle: PMYSQL; Option: TMysqlSetOption): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_shutdown:               function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_sqlstate:               function(Handle: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_warning_count:          function(Handle: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    {BELOW are new PREPARED STATEMENTS}
    mysql_stmt_affected_rows:     function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_get:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; arg: PAnsiChar): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set517UP:     function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: Pointer): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: Pointer): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_param:        function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_result:       function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_close:             function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_data_seek:         procedure(stmt: PMYSQL_STMT; offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_errno:             function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_error:             function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_execute:           function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch:             function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch_column:      function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_field_count:       function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_free_result:       function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_init:              function(Handle: PMYSQL): PMYSQL_STMT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_insert_id:         function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_next_result:       function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_num_rows:          function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_count:       function(stmt: PMYSQL_STMT): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_metadata:    function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_prepare:           function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_reset:             function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_result_metadata:   function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_row_seek:          function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_row_tell:          function(stmt: PMYSQL_STMT): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_send_long_data:    function(stmt: PMYSQL_STMT; parameter_number: UInt; const data: PAnsiChar; length: ULong): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_sqlstate:          function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_store_result:      function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  protected
    ServerArgs: array of PAnsiChar;
    ServerArgsLen: Integer;
    IsEmbeddedDriver: Boolean;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
    procedure BuildServerArguments(Options: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    function IsMariaDBDriver: Boolean;
    procedure Debug(Debug: PAnsiChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(const Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PAnsiChar; Port: Cardinal;
      UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer; overload;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function stmt_attr_set(stmt: PZMySqlPrepStmt; option: TMysqlStmtAttrType;
                                  arg: Pointer): Byte;
    function stmt_affected_rows(Handle: PZMySqlPrepStmt): Int64;
    function stmt_bind_param(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function stmt_bind_result(Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function stmt_close(PrepStmtHandle: PZMySqlPrepStmt): Byte;
    procedure stmt_data_seek(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function stmt_errno(Handle: PZMySqlPrepStmt): Integer;
    function stmt_error(Handle: PZMySqlPrepStmt): AnsiString;
    function stmt_execute(Handle: PZMySqlPrepStmt): Integer;
    function stmt_fetch(Handle: PZMySqlPrepStmt): Integer;
    function stmt_fetch_column(stmt: PMYSQL_STMT; bind: Pointer{BIND record};
      column: UInt; offset: ULong): Integer;
    function stmt_field_count(Handle: PZMySqlPrepStmt): Integer;
    function stmt_free_result(Handle: PZMySqlPrepStmt): Byte;
    function stmt_init(Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function stmt_insert_id(Handle: PZMySqlPrepStmt): Int64;
    function stmt_next_result(Handle: PZMySqlPrepStmt): Integer;
    function stmt_num_rows(Handle: PZMySqlPrepStmt): Int64;
    function stmt_param_count(Handle: PZMySqlPrepStmt): Cardinal;
    function stmt_param_metadata(PrepStmtHandle: PZMySqlPrepStmt): PZMySQLResult;
    function stmt_prepare(PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    function stmt_reset(PrepStmtHandle: PZMySqlPrepStmt): Byte;
    function stmt_result_metadata(Handle: PZMySqlPrepStmt): PZMySQLResult;
    function stmt_row_seek(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function stmt_send_long_data(Handle: PZMySqlPrepStmt; parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
    function stmt_sqlstate(Handle: PZMySqlPrepStmt): PAnsiChar;
    function stmt_store_result(Handle: PZMySqlPrepStmt): Integer;
    procedure GetCharacterSetInfo(Handle: PZMySQLConnect; CharSetInfo: PMY_CHARSET_INFO);

    function GetBindOffsets: MYSQL_BINDOFFSETS;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TMySQLOption;
      const Arg: Pointer): Integer;
    function EscapeString(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong;
    function GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(Handle: PZMySQLConnect;
      Wild: PAnsiChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;
      const Wild: PAnsiChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(Handle: PZMySQLConnect;
      const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;

    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function use_result(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    {ADDED by EgonHugeist}
    function character_set_name(Handle: PMYSQL): PAnsiChar;// char_set_name
    function set_character_set(Handle: PMYSQL; const csname: PAnsiChar): Integer; // set_character_set Returns 0 if valid

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PMySQLLengthArray;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    function GetFieldCount(Res: PZMySQLResult): Integer;

    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure SetDriverOptions(Options: TStrings); virtual; // changed by tohenk, 2009-10-11
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQL41PlainDriver }

  TZMySQL41PlainDriver = class (TZMysqlBaseDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQLD41PlainDriver }

  TZMySQLD41PlainDriver = class (TZMySQL41PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQL5PlainDriver }

  TZMySQL5PlainDriver = class (TZMysqlBaseDriver)
  protected
    function Clone: IZPlainDriver; override;
  protected
    procedure LoadApi; override;
    procedure LoadCodePages; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQLD5PlainDriver }

  TZMySQLD5PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZMariaDB5PlainDriver }

  TZMariaDB5PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZMariaDB10PlainDriver }
  TZMariaDB10PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses SysUtils, ZPlainLoader, ZEncoding, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZMySQLPlainBaseDriver }
function TZMySQLBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'utf8';
end;

procedure TZMySQLBaseDriver.LoadCodePages;
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
  AddCodePage('cp1256', 31, ceAnsi, cCP_WIN1256); {Windows Arabic}
  AddCodePage('cp1257', 32, ceAnsi, zCP_WIN1257); {Windows Baltic}
  AddCodePage('binary', 33); {Binary pseudo charset}
  AddCodePage('geostd8', 34); {GEOSTD8 Georgian}
end;

procedure TZMySQLBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @mysql_affected_rows          := GetAddress('mysql_affected_rows');
  @mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @mysql_close                  := GetAddress('mysql_close');
  @mysql_connect                := GetAddress('mysql_connect');
  @mysql_create_db              := GetAddress('mysql_create_db');
  @mysql_data_seek              := GetAddress('mysql_data_seek');
  @mysql_debug                  := GetAddress('mysql_debug');
  @mysql_drop_db                := GetAddress('mysql_drop_db');
  @mysql_dump_debug_info        := GetAddress('mysql_dump_debug_info');
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
  @mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @mysql_get_server_info        := GetAddress('mysql_get_server_info');
  @mysql_info                   := GetAddress('mysql_info');
  @mysql_init                   := GetAddress('mysql_init');
  @mysql_insert_id              := GetAddress('mysql_insert_id');
  @mysql_kill                   := GetAddress('mysql_kill');
  @mysql_list_dbs               := GetAddress('mysql_list_dbs');
  @mysql_list_fields            := GetAddress('mysql_list_fields');
  @mysql_list_processes         := GetAddress('mysql_list_processes');
  @mysql_list_tables            := GetAddress('mysql_list_tables');
  @mysql_num_fields             := GetAddress('mysql_num_fields');
  @mysql_num_rows               := GetAddress('mysql_num_rows');
  @mysql_options                := GetAddress('mysql_options');
  @mysql_ping                   := GetAddress('mysql_ping');
  @mysql_query                  := GetAddress('mysql_query');
  @mysql_real_connect           := GetAddress('mysql_real_connect');
  @mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @mysql_real_query             := GetAddress('mysql_real_query');
  @mysql_refresh                := GetAddress('mysql_refresh');
  @mysql_row_seek               := GetAddress('mysql_row_seek');
  @mysql_row_tell               := GetAddress('mysql_row_tell');
  @mysql_select_db              := GetAddress('mysql_select_db');
  @mysql_shutdown               := GetAddress('mysql_shutdown');
  @mysql_ssl_set                := GetAddress('mysql_ssl_set');
  @mysql_stat                   := GetAddress('mysql_stat');
  @mysql_store_result           := GetAddress('mysql_store_result');
  @mysql_thread_id              := GetAddress('mysql_thread_id');
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
  @mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @mysql_warning_count          := GetAddress('mysql_warning_count');
  {API for PREPARED STATEMENTS}
  @mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
  @mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
  @mysql_stmt_attr_set          := GetAddress('mysql_stmt_attr_set'); //uses ulong
  @mysql_stmt_attr_set517UP     := GetAddress('mysql_stmt_attr_set'); //uses mybool
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
  @mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
  @mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
  @mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
  @mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
  @mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');
  end;
end;

procedure TZMySQLBaseDriver.BuildServerArguments(Options: TStrings);
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
    if TmpList.Values['--datadir'] = '' then
       TmpList.Add('--datadir='+EMBEDDED_DEFAULT_DATA_DIR);

    for i := 0 to ServerArgsLen - 1 do
      {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(ServerArgs[i]);
    ServerArgsLen := TmpList.Count;
    SetLength(ServerArgs, ServerArgsLen);
    for i := 0 to ServerArgsLen - 1 do
      {$IFDEF UNICODE}
      ServerArgs[i] := {$IFDEF WITH_STRNEW_DEPRECATED}AnsiStrings.{$ENDIF}StrNew(PAnsiChar(UTF8String(TmpList[i])));
      {$ELSE}
      ServerArgs[i] := StrNew(PAnsiChar(TmpList[i]));
      {$ENDIF}
  finally
    TmpList.Free;
  end;
end;

constructor TZMySQLBaseDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
  ServerArgsLen := 0;
  SetLength(ServerArgs, ServerArgsLen);
  IsEmbeddedDriver := False;
  LoadCodePages;
end;

destructor TZMySQLBaseDriver.Destroy;
var
  i : integer;
begin
  for i := 0 to ServerArgsLen - 1 do
    {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(ServerArgs[i]);

  if (FLoader.Loaded) then
    if Assigned(mysql_library_end) then
      mysql_library_end //since 5.0.3
    else
      if Assigned(mysql_server_end) then
        mysql_server_end; //deprected since 5.0.3
  inherited Destroy;
end;

function TZMySQLBaseDriver.IsMariaDBDriver: Boolean;
begin
  Result := FIsMariaDBDriver;
end;

procedure TZMySQLBaseDriver.Close(Handle: PZMySQLConnect);
begin
  mysql_close(Handle);
end;

function TZMySQLBaseDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PAnsiChar): PZMySQLConnect;
begin
  Result := mysql_connect(Handle, Host, User, Password);
end;

function TZMySQLBaseDriver.SslSet(Handle: PZMySQLConnect;
  const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
begin
  Result := mysql_ssl_set(Handle, Key, Cert, Ca, Capath, Cipher);
end;

function TZMySQLBaseDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_create_db(Handle, Database);
end;

procedure TZMySQLBaseDriver.Debug(Debug: PAnsiChar);
begin
  mysql_debug(Debug);
end;

function TZMySQLBaseDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_drop_db(Handle, Database);
end;

function TZMySQLBaseDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := mysql_dump_debug_info(Handle);
end;

function TZMySQLBaseDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar): Integer;
begin
  Result := mysql_query(Handle, Query);
end;

function TZMySQLBaseDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar; Length: Integer): Integer;
begin
  Result := mysql_real_query(Handle, Query, Length);
end;

function TZMySQLBaseDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := mysql_fetch_field(Res);
end;

function TZMySQLBaseDriver.FetchLengths(Res: PZMySQLResult): PMySQLLengthArray;
begin
  Result := mysql_fetch_lengths(Res);
end;

function TZMySQLBaseDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := mysql_fetch_row(Res);
end;

procedure TZMySQLBaseDriver.FreeResult(Res: PZMySQLResult);
begin
  mysql_free_result(Res);
end;

function TZMySQLBaseDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := mysql_affected_rows(Handle);
end;

{**
  EgonHugeist: Get CharacterSet of current Connection
  Returns the default character set name for the current connection.
}
function TZMySQLBaseDriver.character_set_name(Handle: PMYSQL): PAnsiChar;// char_set_name
begin
  if Assigned(mysql_character_set_name) then
    Result := mysql_character_set_name(Handle)
  else
    Result := nil;
end;

{**
  EgonHugeist: This function is used to set the default character set for the
  current connection. The string csname specifies a valid character set name.
  The connection collation becomes the default collation of the character set.
  This function works like the SET NAMES statement, but also sets the value
  of mysql->charset, and thus affects the character set
  used by mysql_real_escape_string()
}
function TZMySQLBaseDriver.set_character_set(Handle: PMYSQL;
  const csname: PAnsiChar): Integer; // set_character_set Returns 0 if valid
begin
  if Assigned(mysql_set_character_set) then
    Result := mysql_set_character_set(Handle, csName)
  else
    Result := 1;
end;

function TZMySQLBaseDriver.GetClientInfo: PAnsiChar;
begin
  Result := mysql_get_client_info;
end;

function TZMySQLBaseDriver.EscapeString(Handle: PMYSQL; PTo: PAnsiChar;
  const PFrom: PAnsiChar; length: ULong): ULong;
begin
  if Handle = nil then
    Result := mysql_escape_string(PTo, PFrom, Length)
  else
    Result := mysql_real_escape_string(Handle, PTo, PFrom, Length);
end;

function TZMySQLBaseDriver.GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := mysql_get_host_info(Handle);
end;

function TZMySQLBaseDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_dbs(Handle, Wild);
end;

function TZMySQLBaseDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQLBaseDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := mysql_list_processes(Handle);
end;

function TZMySQLBaseDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_tables(Handle, Wild);
end;

function TZMySQLBaseDriver.GetNumRows(Res: PZMySQLResult): Int64;
begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  mysql_num_rows (Res);
end;

function TZMySQLBaseDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := mysql_get_proto_info(Handle);
end;

function TZMySQLBaseDriver.GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := mysql_get_server_info(Handle);
end;

function TZMySQLBaseDriver.GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := mysql_stat(Handle);
end;

function TZMySQLBaseDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := mysql_thread_id(Handle);
end;

function TZMySQLBaseDriver.Init(const Handle: PZMySQLConnect): PZMySQLConnect;
var
  ClientInfo: PAnsiChar;
  L: LengthInt;
begin
  if (Assigned(mysql_server_init) or Assigned(mysql_library_init)){ and (ServerArgsLen > 0) }then
    if Assigned(mysql_library_init) then
      //http://dev.mysql.com/doc/refman/5.7/en/mysql-library-init.html
      mysql_library_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS) //<<<-- Isn't threadsafe
    else
      //http://dev.mysql.com/doc/refman/5.7/en/mysql-server-init.html
      mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS); //<<<-- Isn't threadsafe
  Result := mysql_init(Handle);
  ClientInfo := GetClientInfo;
  L := ZFastCode.StrLen(ClientInfo);
  FIsMariaDBDriver := CompareMem(ClientInfo+L-7, PAnsiChar('MariaDB'), 7);
end;

function TZMySQLBaseDriver.GetLastInsertID(Handle: PZMySQLConnect): Int64;
begin
  Result := mysql_insert_id(PMYSQL(Handle));
end;

function TZMySQLBaseDriver.Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
begin
  Result := mysql_kill(Handle, Pid);
end;

function TZMySQLBaseDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := mysql_ping(Handle);
end;

function TZMySQLBaseDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQLBaseDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := mysql_refresh(Handle, Options);
end;

procedure TZMySQLBaseDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  mysql_data_seek(Res, Offset);
end;

function TZMySQLBaseDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := mysql_field_seek(Res, Offset);
end;

function TZMySQLBaseDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := mysql_row_seek(Res, Row);
end;

function TZMySQLBaseDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_select_db(Handle, Database);
end;

function TZMySQLBaseDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TMySQLOption; const Arg: Pointer): Integer;
begin
  Result := mysql_options(Handle,TMySqlOption(Option), Arg);
end;

function TZMySQLBaseDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQLBaseDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
begin
  Result := mysql_autocommit(PMYSQL(Handle), Byte(Ord(Mode))) = 0;
end;

function TZMySQLBaseDriver.Commit(Handle: PZMySQLConnect): Boolean;
begin
  Result := mysql_commit(PMYSQL(Handle)) = 0;
end;

function TZMySQLBaseDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
begin
  Result := mysql_more_results (PMYSQL(Handle)) <> 0;
end;

function TZMySQLBaseDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
begin
    Result := mysql_next_result (PMYSQL(Handle));
end;

function TZMySQLBaseDriver.Rollback (Handle: PZMySQLConnect): Boolean;
begin
  Result := mysql_rollback(PMYSQL(Handle)) = 0;
end;

function TZMySQLBaseDriver.GetSQLState(Handle: PZMySQLConnect): AnsiString;
begin
  Result := mysql_sqlstate (PMYSQL(Handle));
end;

function TZMySQLBaseDriver.stmt_attr_set(stmt: PZMySqlPrepStmt;
  option: TMysqlStmtAttrType; arg: Pointer): Byte;
begin
  //http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-attr-set.html
  //http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-attr-set.html
  if mysql_get_client_version >= 50107 then
    Result :=  mysql_stmt_attr_set517up(PMYSQL_STMT(stmt),option,arg)
  else //avoid stack crashs !
    Result := mysql_stmt_attr_set(PMYSQL_STMT(stmt),option,arg);
end;

function TZMySQLBaseDriver.stmt_affected_rows(Handle: PZMySqlPrepStmt): Int64;
begin
  Result :=  mysql_stmt_affected_rows(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_bind_param(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := mysql_stmt_bind_param(PMYSQL_STMT(Handle), pointer(bindArray));
end;

function TZMySQLBaseDriver.stmt_bind_result(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
  Result := mysql_stmt_bind_result(PMYSQL_STMT(Handle), pointer(bindArray));
end;

function TZMySQLBaseDriver.stmt_close(PrepStmtHandle: PZMySqlPrepStmt): Byte;
begin
  Result := mysql_stmt_close(PMYSQL_STMT(PrepStmtHandle));
end;

procedure TZMySQLBaseDriver.stmt_data_seek(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
begin
  mysql_stmt_data_seek(PMYSQL_STMT(PrepStmtHandle), Offset);
end;

function TZMySQLBaseDriver.stmt_errno(Handle: PZMySqlPrepStmt):Integer;
begin
    Result := mysql_stmt_errno(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_error(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := mysql_stmt_error(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_execute(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := mysql_stmt_execute(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_fetch(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := mysql_stmt_fetch(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_fetch_column(stmt: PZMySqlPrepStmt;
  bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer;
begin
  if (@mysql_stmt_fetch_column <> nil) then
    Result := mysql_stmt_fetch_column(stmt, bind, column, offset)
  else
    Result := -1; //indicate an error: http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-fetch-column.html
end;

function TZMySQLBaseDriver.stmt_field_count(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := mysql_stmt_field_count(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_free_result(Handle: PZMySqlPrepStmt): Byte;
begin
   Result := mysql_stmt_free_result(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_init(Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := mysql_stmt_init(PMYSQL(Handle));
end;

function TZMySQLBaseDriver.stmt_insert_id(Handle: PZMySqlPrepStmt): Int64;
begin
    Result := mysql_stmt_insert_id(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_next_result(Handle: PZMySqlPrepStmt): Integer;
begin
    if (@mysql_stmt_next_result = nil) then
        Result := -1  // Successful and there are no more results
    else
        Result :=  mysql_stmt_next_result(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_num_rows(Handle: PZMySqlPrepStmt): Int64;
begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  mysql_stmt_num_rows(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_param_count(Handle: PZMySqlPrepStmt): Cardinal;
begin
    Result := mysql_stmt_param_count(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_param_metadata(PrepStmtHandle: PZMySqlPrepStmt): PZMySQLResult;
begin
  Result := mysql_stmt_param_metadata(PMYSQL_STMT(PrepStmtHandle));
end;

function TZMySQLBaseDriver.stmt_prepare(PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
begin
    Result := mysql_stmt_prepare(PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQLBaseDriver.stmt_reset(PrepStmtHandle: PZMySqlPrepStmt): Byte;
begin
  Result := mysql_stmt_reset(PrepStmtHandle);
end;

function TZMySQLBaseDriver.stmt_result_metadata(Handle: PZMySqlPrepStmt): PZMySQLResult;
begin
    Result := mysql_stmt_result_metadata(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_row_seek(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
    Result := mysql_stmt_row_seek(PMYSQL_STMT(Handle), Row);
end;

function TZMySQLBaseDriver.stmt_send_long_data(Handle: PZMySqlPrepStmt;
  parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
begin
  Result := mysql_stmt_send_long_data(PMYSQL_STMT(Handle), parameter_number, data, length);
end;

function TZMySQLBaseDriver.stmt_sqlstate(Handle: PZMySqlPrepStmt): PAnsiChar;
begin
  Result := mysql_stmt_sqlstate(PMYSQL_STMT(Handle));
end;

function TZMySQLBaseDriver.stmt_store_result(Handle: PZMySqlPrepStmt): Integer;
begin
  Result := mysql_stmt_store_result(PMYSQL_STMT(Handle));
end;

procedure TZMySQLBaseDriver.GetCharacterSetInfo(Handle: PZMySQLConnect; CharSetInfo: PMY_CHARSET_INFO);
begin
    mysql_get_character_set_info(Handle, CharSetInfo);
end;

function TZMySQLBaseDriver.GetBindOffsets: MYSQL_BINDOFFSETS;
var
  DriverVersion : Integer;
begin
  DriverVersion:=GetClientVersion;
  case DriverVersion of
    40100..40199 : begin
                     result.buffer_type   := {%H-}NativeUint(@(PMYSQL_BIND41(nil).buffer_type));
                     result.buffer_length := {%H-}NativeUint(@(PMYSQL_BIND41(nil).buffer_length));
                     result.is_unsigned   := {%H-}NativeUint(@(PMYSQL_BIND41(nil).is_unsigned));
                     result.buffer        := {%H-}NativeUint(@(PMYSQL_BIND41(nil).buffer));
                     result.length        := {%H-}NativeUint(@(PMYSQL_BIND41(nil).length));
                     result.is_null       := {%H-}NativeUint(@(PMYSQL_BIND41(nil).is_null));
                     result.size          := Sizeof(MYSQL_BIND41);
                   end;
    50000..50099 : begin
                     result.buffer_type   := {%H-}NativeUint(@(PMYSQL_BIND50(nil).buffer_type));
                     result.buffer_length := {%H-}NativeUint(@(PMYSQL_BIND50(nil).buffer_length));
                     result.is_unsigned   := {%H-}NativeUint(@(PMYSQL_BIND50(nil).is_unsigned));
                     result.buffer        := {%H-}NativeUint(@(PMYSQL_BIND50(nil).buffer));
                     result.length        := {%H-}NativeUint(@(PMYSQL_BIND50(nil).length));
                     result.is_null       := {%H-}NativeUint(@(PMYSQL_BIND50(nil).is_null));
                     result.size          := Sizeof(MYSQL_BIND50);
                   end;
    50100..59999 : begin
                     result.buffer_type   := {%H-}NativeUint(@(PMYSQL_BIND51(nil).buffer_type));
                     result.buffer_length := {%H-}NativeUint(@(PMYSQL_BIND51(nil).buffer_length));
                     result.is_unsigned   := {%H-}NativeUint(@(PMYSQL_BIND51(nil).is_unsigned));
                     result.buffer        := {%H-}NativeUint(@(PMYSQL_BIND51(nil).buffer));
                     result.length        := {%H-}NativeUint(@(PMYSQL_BIND51(nil).length));
                     result.is_null       := {%H-}NativeUint(@(PMYSQL_BIND51(nil).is_null));
                     result.size          := Sizeof(MYSQL_BIND51);
                   end;
    60000..60099 : begin
                     result.buffer_type   := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer_type));
                     result.buffer_length := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer_length));
                     result.is_unsigned   := {%H-}NativeUint(@(PMYSQL_BIND60(nil).is_unsigned));
                     result.buffer        := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer));
                     result.length        := {%H-}NativeUint(@(PMYSQL_BIND60(nil).length));
                     result.is_null       := {%H-}NativeUint(@(PMYSQL_BIND60(nil).is_null));
                     result.size          := Sizeof(MYSQL_BIND60);
                   end;
  else
    if FIsMariaDBDriver and (DriverVersion >= 100000) then //MariaDB 10
    begin
      result.buffer_type   := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer_type));
      result.buffer_length := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer_length));
      result.is_unsigned   := {%H-}NativeUint(@(PMYSQL_BIND60(nil).is_unsigned));
      result.buffer        := {%H-}NativeUint(@(PMYSQL_BIND60(nil).buffer));
      result.length        := {%H-}NativeUint(@(PMYSQL_BIND60(nil).length));
      result.is_null       := {%H-}NativeUint(@(PMYSQL_BIND60(nil).is_null));
      result.size          := Sizeof(MYSQL_BIND60);
    end
    else
      result.buffer_type:=0;
  end;
end;

function TZMySQLBaseDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := mysql_store_result(Handle);
end;

function TZMySQLBaseDriver.use_result(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := mysql_use_result(Handle);
end;

function TZMySQLBaseDriver.GetLastError(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := mysql_error(Handle);
end;

function TZMySQLBaseDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := mysql_num_rows(Res);
end;

function TZMySQLBaseDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQLBaseDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := mysql_num_fields(Res);
end;

function TZMySQLBaseDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PAnsiChar;
begin
  Result := PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQLBaseDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := mysql_errno(PMYSQL(Handle));
end;

function TZMySQLBaseDriver.GetClientVersion: Integer;
begin
 Result := mysql_get_client_version;
end;

function TZMySQLBaseDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := mysql_get_server_version(Handle);
end;

procedure TZMySQLBaseDriver.SetDriverOptions(Options: TStrings);
var
  PreferedLibrary: String;
begin
  PreferedLibrary := Options.Values['Library'];
  if PreferedLibrary <> '' then
    Loader.AddLocation(PreferedLibrary);
  if IsEmbeddedDriver then
    BuildServerArguments(Options);
end;

{ TZMySQL41PlainDriver }

function TZMySQL41PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQL41PlainDriver.Create;
end;

constructor TZMySQL41PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION);
  {$ENDIF}
end;

function TZMySQL41PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.1';
end;

function TZMySQL41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

{ TZMySQLD41PlainDriver }

function TZMySQLD41PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQLD41PlainDriver.Create;
end;

constructor TZMySQLD41PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD41PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.1';
end;

function TZMySQLD41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.1+';
end;

{ TZMySQL5PlainDriver }

function TZMySQL5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQL5PlainDriver.Create;
end;

procedure TZMySQL5PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
    @mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
    @mysql_stmt_next_result       := GetAddress('mysql_stmt_next_result');
  end;
end;

procedure TZMySQL5PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  {MySQL 4.1-5.5}
  { MultiByte }
  AddCodePage('utf8mb4', 37, ceUTF8, zCP_UTF8, '', 4); {UTF-8 Unicode}
  AddCodePage('utf16', 38, ceUTF16, zCP_UTF16, 'utf8', 4); {UTF-16 Unicode}
  AddCodePage('utf32', 39, ceUTF16, zCP_utf32, 'utf8', 4); {UTF-32 Unicode} //Egonhugeist improved
end;

constructor TZMySQL5PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
    FLoader.AddLocation(MARIADB_LOCATION);
  {$ENDIF}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION);
  {$ELSE}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
    FLoader.AddLocation(MARIADB_LOCATION);
  {$ENDIF}
    FLoader.AddLocation(LINUX_DLL50_LOCATION);
    FLoader.AddLocation(LINUX_DLL51_LOCATION);
    FLoader.AddLocation(LINUX_DLL55_LOCATION);
  {$ENDIF}
end;

function TZMySQL5PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-5';
end;

function TZMySQL5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 5.0+';
end;

{ TZMySQLD5PlainDriver }

function TZMySQLD5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQLD5PlainDriver.Create
end;

constructor TZMySQLD5PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL51_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL55_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD5PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-5';
end;

function TZMySQLD5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 5+';
end;

{ TZMariaDB5PlainDriver }
function TZMariaDB5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMariaDB5PlainDriver.Create
end;

constructor TZMariaDB5PlainDriver.Create;
begin
  inherited Create;
  FLoader.ClearLocations;
  FLoader.AddLocation(MARIADB_LOCATION);
end;

function TZMariaDB5PlainDriver.GetProtocol: string;
begin
  Result := 'MariaDB-5';
end;

function TZMariaDB5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MariaDB-5.x';
end;

{ TZMariaDB5PlainDriver }
function TZMariaDB10PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMariaDB10PlainDriver.Create
end;

function TZMariaDB10PlainDriver.GetProtocol: string;
begin
  Result := 'MariaDB-10';
end;

function TZMariaDB10PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MariaDB-10';
end;

end.


