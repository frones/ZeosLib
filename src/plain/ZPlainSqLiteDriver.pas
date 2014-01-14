{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for SQLite             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZPlainSqLiteDriver;

interface

{$I ZPlain.inc}

uses SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} Types,
  ZClasses, ZCompatibility, ZPlainDriver;

const
  WINDOWS_DLL_LOCATION = 'sqlite.dll';
  WINDOWS_DLL3_LOCATION = 'sqlite3.dll';
  LINUX_DLL_LOCATION = 'libsqlite'+SharedSuffix;
  LINUX_DLL3_LOCATION = 'libsqlite3'+SharedSuffix;

  SQLITE_ISO8859   = 1;
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';

  { Return values for sqlite_exec() and sqlite_step() }
  SQLITE_OK           = 0;   // Successful result
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // An internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  _SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // (Internal Only) Table or record not found
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // (Internal Only) Database table is empty
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // Too much data for one row of a table
  SQLITE_CONSTRAINT   = 19;  // Abort due to contraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Auxiliary database format error
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_ROW          = 100;  // sqlite_step() has another row ready
  SQLITE_DONE         = 101;  // sqlite_step() has finished executing

  SQLITE_NUMERIC      = -1;
  SQLITE_TEXT         = -2;
  SQLITE_ARGS         = -3;

  {
    The second parameter to the access authorization function above will
    be one of the values below.  These values signify what kind of operation
    is to be authorized.  The 3rd and 4th parameters to the authorization
    function will be parameters or NULL depending on which of the following
    codes is used as the second parameter.  The 5th parameter is the name
    of the database ("main", "temp", etc.) if applicable.  The 6th parameter
    is the name of the inner-most trigger or view that is responsible for
    the access attempt or NULL if this access attempt is directly from
    input SQL code.

                                             Arg-3           Arg-4
  }
  SQLITE_COPY                  = 0;  // Table Name      File Name
  SQLITE_CREATE_INDEX          = 1;  // Index Name      Table Name
  SQLITE_CREATE_TABLE          = 2;  // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX     = 3;  // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE     = 4;  // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER   = 5;  // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW      = 6;  // View Name       NULL
  SQLITE_CREATE_TRIGGER        = 7;  // Trigger Name    Table Name
  SQLITE_CREATE_VIEW           = 8;  // View Name       NULL
  SQLITE_DELETE                = 9;  // Table Name      NULL
  SQLITE_DROP_INDEX            = 10; // Index Name      Table Name
  SQLITE_DROP_TABLE            = 11; // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX       = 12; // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE       = 13; // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER     = 14; // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW        = 15; // View Name       NULL
  SQLITE_DROP_TRIGGER          = 16; // Trigger Name    Table Name
  SQLITE_DROP_VIEW             = 17; // View Name       NULL
  SQLITE_INSERT                = 18; // Table Name      NULL
  SQLITE_PRAGMA                = 19; // Pragma Name     1st arg or NULL
  SQLITE_READ                  = 20; // Table Name      Column Name
  SQLITE_SELECT                = 21; // NULL            NULL
  SQLITE_TRANSACTION           = 22; // NULL            NULL
  SQLITE_UPDATE                = 23; // Table Name      Column Name
  SQLITE_ATTACH                = 24; // Filename        NULL
  SQLITE_DETACH                = 25; // Database Name   NULL

  { The return value of the authorization function should be one of the
    following constants: }
  SQLITE_DENY    = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE3_TEXT   = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

type
  Psqlite = Pointer;
  Psqlite_func = Pointer;
  Psqlite_vm = Pointer;
  Psqlite3_stmt = Pointer;
  Psqlite3_value = Pointer;

  Tsqlite3_destructor_type = procedure(user: pointer); cdecl;


  SQLITE_STATIC = procedure(User: Pointer = Nil); cdecl;
  SQLITE_TRANSIENT = procedure(User: pointer = Pointer(-1)); cdecl;

type
{ ************** Plain API Function types definition ************* }

  Tsqlite_callback = function(p1: Pointer; p2: Integer; var p3: PAnsiChar;
    var p4: PAnsiChar): Integer; cdecl;
  Tsqlite_simple_callback = function(p1: Pointer): Integer; cdecl;
  Tsqlite_simple_callback0 = function(p1: Pointer): Pointer; cdecl;
  Tsqlite_busy_callback = function(p1: Pointer; const p2: PAnsiChar;
    p3: Integer): Integer; cdecl;

  Tsqlite_function_callback = procedure(p1: Psqlite_func; p2: Integer;
    const p3: PPAnsiChar); cdecl;
  Tsqlite_finalize_callback = procedure(p1: Psqlite_func); cdecl;
  Tsqlite_auth_callback = function(p1: Pointer; p2: Integer; const p3: PAnsiChar;
    const p4: PAnsiChar; const p5: PAnsiChar; const p6: PAnsiChar): Integer; cdecl;
  Tsqlite_trace_callback = procedure(p1: Pointer; const p2: PAnsiChar); cdecl;

  Tsqlite_open = function(const filename: PAnsiChar;var Qsqlite: Psqlite): Integer; cdecl;
  Tsqlite_close = function(db: Psqlite): Integer; cdecl;

  { prepared statment api}
  Tsqlite3_prepare = function(
    db: Psqlite;                // Database handle
    const zSql: PAnsiChar;      // SQL statement, UTF-8 encoded
    nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
    out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
    out pzTail: PPAnsichar      // OUT: Pointer to unused portion of zSql
  ): Integer; cdecl;
  Tsqlite3_prepare_v2 = function(
    db: Psqlite;                // Database handle
    const zSql: PAnsiChar;      // SQL statement, UTF-8 encoded
    nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
    out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
    out pzTail: PPAnsichar      // OUT: Pointer to unused portion of zSql
  ): Integer; cdecl;
  Tsqlite3_prepare16 = function(
    db: Psqlite;                // Database handle
    const zSql: PWideChar;      // SQL statement, UTF-16 encoded
    nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
    out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
    out pzTail: ZPPWideChar      // OUT: Pointer to unused portion of zSql
  ): Integer; cdecl;
  Tsqlite3_prepare16_v2 = function(
    db: Psqlite;                // Database handle
    const zSql: PWideChar;      // SQL statement, UTF-16 encoded
    nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
    out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
    out pzTail: ZPPWideChar      // OUT: Pointer to unused portion of zSql
  ): Integer; cdecl;

  Tsqlite3_bind_parameter_count = function(pStmt: Psqlite3_stmt): Integer; cdecl;
  Tsqlite3_bind_parameter_name = function(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar; cdecl;
  Tsqlite3_bind_parameter_index = function(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer; cdecl;

  Tsqlite3_clear_bindings = function(pStmt: Psqlite3_stmt): Integer; cdecl;
  Tsqlite3_column_count = function(pStmt: Psqlite3_stmt): Integer; cdecl;
  Tsqlite3_column_name = function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_name16 = function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;

  Tsqlite3_column_database_name = function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_database_name16 = function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
  Tsqlite3_column_table_name = function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_table_name16 = function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
  Tsqlite3_column_origin_name = function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_origin_name16 = function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;

  Tsqlite3_column_decltype = function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_decltype16 = function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;

  Tsqlite3_step = function(pStmt: Psqlite3_stmt): Integer; cdecl;
  Tsqlite3_data_count = function (pStmt: Psqlite3_stmt): Integer; cdecl;

  Tsqlite3_bind_blob = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
  Tsqlite3_bind_double = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer; cdecl;
  Tsqlite3_bind_int = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer; cdecl;
  Tsqlite3_bind_int64 = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer; cdecl;
  Tsqlite3_bind_null = function(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer; cdecl;
  Tsqlite3_bind_text = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
  Tsqlite3_bind_text16 = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
  Tsqlite3_bind_value = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer; cdecl;
  Tsqlite3_bind_zeroblob = function(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer; cdecl;

  Tsqlite3_finalize = function(pStmt: Psqlite3_stmt): Integer; cdecl;
  Tsqlite3_reset = function(pStmt: Psqlite3_stmt): Integer; cdecl;

  Tsqlite3_column_blob = function(Stmt: Psqlite3_stmt; iCol:integer): Pointer; cdecl;
  Tsqlite3_column_bytes = function(Stmt: Psqlite3_stmt; iCol: Integer): integer; cdecl;
  Tsqlite3_column_bytes16 = function(Stmt: Psqlite3_stmt; iCol: Integer): integer; cdecl;
  Tsqlite3_column_double = function(Stmt: Psqlite3_stmt; iCol: Integer): Double; cdecl;
  Tsqlite3_column_int = function(Stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  Tsqlite3_column_int64 = function(Stmt: Psqlite3_stmt; iCol: Integer): Int64; cdecl;
  Tsqlite3_column_text = function(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
  Tsqlite3_column_text16 = function(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
  Tsqlite3_column_type = function(Stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
  Tsqlite3_column_value = function(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value; cdecl;
  Tsqlite3_last_insert_rowid = function(db: Psqlite): Int64; cdecl;

  //gets the column type
  Tsqlite_column_type = function(db:PSqlite;iCol:integer):Integer; cdecl;

  Tsqlite_exec = function(db: Psqlite; const sql: PAnsiChar;
    sqlite_callback: Tsqlite_callback; arg: Pointer;
    var errmsg: PAnsiChar): Integer; cdecl;
  Tsqlite_errmsg = function(db: Psqlite): PAnsiChar; cdecl;
  Tsqlite_errstr = function(code: Integer): PAnsiChar; cdecl;
  //Tsqlite_last_insert_rowid = function(db: Psqlite): Integer; cdecl;
  Tsqlite_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_last_statement_changes = function(db: Psqlite): Integer; cdecl;
  Tsqlite_interrupt = procedure(db: Psqlite); cdecl;
  Tsqlite_complete = function(const sql: PAnsiChar): Integer; cdecl;
  Tsqlite_busy_handler = procedure(db: Psqlite;
    callback: Tsqlite_busy_callback; ptr: Pointer); cdecl;
  Tsqlite_busy_timeout = procedure(db: Psqlite; ms: Integer); cdecl;
  Tsqlite_get_table = function(db: Psqlite; const sql: PAnsiChar;
    var resultp: PPAnsiChar; var nrow: Integer; var ncolumn: Integer;
    var errmsg: PAnsiChar): Integer; cdecl;
  Tsqlite_free_table = procedure(var result: PAnsiChar); cdecl;
  Tsqlite_freemem = procedure(ptr: Pointer); cdecl;
  Tsqlite_libversion = function: PAnsiChar; cdecl;
  Tsqlite_libencoding = function: PAnsiChar; cdecl;

  Tsqlite_create_function = function(db: Psqlite; const zName: PAnsiChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    pUserData: Pointer): Integer; cdecl;
  Tsqlite_create_aggregate = function(db: Psqlite; const zName: PAnsiChar;
    nArg: Integer; callback: Tsqlite_function_callback;
    finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_function_type = function(db: Psqlite; const zName: PAnsiChar;
    datatype: Integer): Integer; cdecl;

  Tsqlite_set_result_string = function(func: Psqlite_func; const arg: PAnsiChar;
    len: Integer; UN: Tsqlite_simple_callback): PAnsiChar; cdecl;

  Tsqlite_set_result_int = procedure(func: Psqlite_func; arg: Integer); cdecl;
  Tsqlite_set_result_double = procedure(func: Psqlite_func; arg: Double); cdecl;
  Tsqlite_set_result_error = procedure(func: Psqlite_func; const arg: PAnsiChar;
    len: Integer); cdecl;
  Tsqlite_user_data = function(func: Psqlite_func): Pointer; cdecl;
  Tsqlite_aggregate_context = function(func: Psqlite_func;
    nBytes: Integer): Pointer; cdecl;
  Tsqlite_aggregate_count = function(func: Psqlite_func): Integer; cdecl;

  Tsqlite_set_authorizer = function(db: Psqlite;
    callback: Tsqlite_auth_callback; pUserData: Pointer): Integer; cdecl;
  Tsqlite_trace = function(db: Psqlite; callback: Tsqlite_trace_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_progress_handler = procedure(db: Psqlite; p1: Integer;
    callback: Tsqlite_simple_callback; ptr: Pointer); cdecl;
  Tsqlite_commit_hook = function(db: Psqlite; callback: Tsqlite_simple_callback;
    ptr: Pointer): Pointer; cdecl;

  Tsqlite_open_encrypted = function(const zFilename: PAnsiChar;
    const pKey: PAnsiChar; nKey: Integer; var pErrcode: Integer;
    var pzErrmsg: PAnsiChar): Psqlite; cdecl;
  Tsqlite_rekey = function(db: Psqlite; const pKey: Pointer;
    nKey: Integer): Integer; cdecl;
  Tsqlite_key = function(db: Psqlite; const pKey: Pointer;
    nKey: Integer): Integer; cdecl;

{ ************* Plain API Function variables definition ************ }
TZSQLite_API = record
  sqlite_open: Tsqlite_open;
  sqlite_close: Tsqlite_close;

  { prepared statement api }
  sqlite_prepare: Tsqlite3_prepare;
  sqlite_prepare_v2: Tsqlite3_prepare_v2;
  sqlite_prepare16: Tsqlite3_prepare16;
  sqlite_prepare16_v2: Tsqlite3_prepare16_v2;

  sqlite_bind_parameter_count: Tsqlite3_bind_parameter_count;
  sqlite_bind_parameter_name: Tsqlite3_bind_parameter_name;
  sqlite_bind_parameter_index: Tsqlite3_bind_parameter_index;

  sqlite_clear_bindings: Tsqlite3_clear_bindings;
  sqlite_column_count: Tsqlite3_column_count;
  sqlite_column_name: Tsqlite3_column_name;
  sqlite_column_name16: Tsqlite3_column_name16;

  sqlite_column_database_name: Tsqlite3_column_database_name;
  sqlite_column_database_name16: Tsqlite3_column_database_name16;
  sqlite_column_table_name: Tsqlite3_column_table_name;
  sqlite_column_table_name16: Tsqlite3_column_table_name16;
  sqlite_column_origin_name: Tsqlite3_column_origin_name;
  sqlite_column_origin_name16: Tsqlite3_column_origin_name16;

  sqlite_column_decltype: Tsqlite3_column_decltype;
  sqlite_column_decltype16: Tsqlite3_column_decltype16;

  sqlite_step: Tsqlite3_step;
  sqlite_data_count: Tsqlite3_data_count;

  sqlite_bind_blob: Tsqlite3_bind_blob;
  sqlite_bind_double: Tsqlite3_bind_double;
  sqlite_bind_int: Tsqlite3_bind_int;
  sqlite_bind_int64: Tsqlite3_bind_int64;
  sqlite_bind_null: Tsqlite3_bind_null;
  sqlite_bind_text: Tsqlite3_bind_text;
  sqlite_bind_text16: Tsqlite3_bind_text16;
  sqlite_bind_value: Tsqlite3_bind_value;
  sqlite_bind_zeroblob: Tsqlite3_bind_zeroblob;

  sqlite_finalize: Tsqlite3_finalize;
  sqlite_reset: Tsqlite3_reset;

  sqlite_column_blob: Tsqlite3_column_blob;
  sqlite_column_bytes: Tsqlite3_column_bytes;
  sqlite_column_bytes16: Tsqlite3_column_bytes16;
  sqlite_column_double: Tsqlite3_column_double;
  sqlite_column_int: Tsqlite3_column_int;
  sqlite_column_int64: Tsqlite3_column_int64;
  sqlite_column_text: Tsqlite3_column_text;
  sqlite_column_text16: Tsqlite3_column_text16;
  sqlite_column_type: Tsqlite3_column_type;
  sqlite_column_value: Tsqlite3_column_value;

  sqlite_exec: Tsqlite_exec;
  sqlite_errmsg: Tsqlite_errmsg;
  sqlite_errstr: Tsqlite_errstr;
  sqlite_last_insert_rowid: Tsqlite3_last_insert_rowid;
  sqlite_changes: Tsqlite_changes;
  sqlite_last_statement_changes: Tsqlite_last_statement_changes;
  sqlite_interrupt: Tsqlite_interrupt;
  sqlite_complete: Tsqlite_complete;
  sqlite_busy_handler: Tsqlite_busy_handler;
  sqlite_busy_timeout: Tsqlite_busy_timeout;
  sqlite_get_table: Tsqlite_get_table;
  sqlite_free_table: Tsqlite_free_table;
  sqlite_freemem: Tsqlite_freemem;
  sqlite_libversion: Tsqlite_libversion;
  sqlite_libencoding: Tsqlite_libencoding;
  sqlite_create_function: Tsqlite_create_function;
  sqlite_create_aggregate: Tsqlite_create_aggregate;
  sqlite_function_type: Tsqlite_function_type;
  sqlite_set_result_string: Tsqlite_set_result_string;
  sqlite_set_result_int: Tsqlite_set_result_int;
  sqlite_set_result_double: Tsqlite_set_result_double;
  sqlite_set_result_error: Tsqlite_set_result_error;
  sqlite_user_data: Tsqlite_user_data;
  sqlite_aggregate_context: Tsqlite_aggregate_context;
  sqlite_aggregate_count: Tsqlite_aggregate_count;
  sqlite_set_authorizer: Tsqlite_set_authorizer;
  sqlite_trace: Tsqlite_trace;
  sqlite_progress_handler: Tsqlite_progress_handler;
  sqlite_commit_hook: Tsqlite_commit_hook;
  sqlite_open_encrypted: Tsqlite_open_encrypted;
  sqlite_rekey: Tsqlite_rekey;
  sqlite_key: Tsqlite_key;
end;

type

  {** Represents a generic interface to SQLite native API. }
  IZSQLitePlainDriver = interface (IZPlainDriver)
    ['{B931C952-3076-4ECB-9630-D900E8DB9869}']

    function Open(const filename: PAnsiChar; mode: Integer;
      var errmsg: PAnsiChar): Psqlite;
    function Close(db: Psqlite): Integer;
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Int64;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(db: Psqlite; code: Integer): String;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PAnsiChar; var resultp: PPAnsiChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PAnsiChar): Integer;
    procedure FreeTable(var result: PAnsiChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;
    function LibEncoding: PAnsiChar;

    function CreateFunction(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer;
    function CreateAggregate(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PAnsiChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    { Prepared statmenet api }
    function Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
    function Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
    function Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;
    function Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;

    function bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
    function bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
    function bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;

    function clear_bindings(pStmt: Psqlite3_stmt): Integer;
    function column_count(pStmt: Psqlite3_stmt): Integer;
    function column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_database_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_table_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_origin_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_decltype16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function Step(Stmt: Psqlite3_stmt; var pN: Integer;
      var pazValue, pazColName: PPAnsiChar): Integer; overload;
    function Step(Stmt: Psqlite3_stmt): Integer; overload;
    function data_count(pStmt: Psqlite3_stmt): Integer;

    function bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
    function bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
    function bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
    function bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
    function bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
    function bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;

    function finalize(pStmt: Psqlite3_stmt): Integer;
    function reset(pStmt: Psqlite3_stmt): Integer;

    function column_blob(Stmt: Psqlite3_stmt; iCol:integer): TStream;
    function column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
    function column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
    function column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_type(Stmt: Psqlite3_stmt; iCol: Integer): String;
    function column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PAnsiChar; const pKey: PAnsiChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
  end;

  {** Implements a base driver for SQLite}
  TZSQLiteBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZSQLitePlainDriver)
  protected
    SQLite_API : TZSQLite_API;
    // procedure LoadApi; override; ->completely done in version dependent child classes
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
  public
    constructor Create;

    function Open(const filename: PAnsiChar; mode: Integer;
      var errmsg: PAnsiChar): Psqlite;
    function Close(db: Psqlite): Integer;
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Int64;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(db: Psqlite; code: Integer): String;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    function GetTable(db: Psqlite; const sql: PAnsiChar; var resultp: PPAnsiChar;
      var nrow: Integer; var ncolumn: Integer; var errmsg: PAnsiChar): Integer;
    procedure FreeTable(var result: PAnsiChar);
    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;
    function LibEncoding: PAnsiChar;

    function CreateFunction(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      pUserData: Pointer): Integer; virtual;
    function CreateAggregate(db: Psqlite; const zName: PAnsiChar;
      nArg: Integer; callback: Tsqlite_function_callback;
      finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
    function FunctionType(db: Psqlite; const zName: PAnsiChar;
      datatype: Integer): Integer;
    function SetResultString(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    { Prepared statmenet api }
    function Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
    function Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
    function Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;
    function Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;

    function bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
    function bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
    function bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;

    function clear_bindings(pStmt: Psqlite3_stmt): Integer;
    function column_count(pStmt: Psqlite3_stmt): Integer;
    function column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_database_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_table_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_origin_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_decltype16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;

    function Step(Stmt: Psqlite3_stmt; var pN: Integer;
      var pazValue, pazColName: PPAnsiChar): Integer; overload;
    function Step(Stmt: Psqlite3_stmt): Integer; overload;
    function data_count(pStmt: Psqlite3_stmt): Integer;

    function bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
    function bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
    function bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
    function bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
    function bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PAnsiChar; N: Integer;
      ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PWideChar; N: Integer;
      ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
    function bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;

    function finalize(pStmt: Psqlite3_stmt): Integer;
    function reset(pStmt: Psqlite3_stmt): Integer;

    function column_blob(Stmt: Psqlite3_stmt; iCol:integer): TStream;
    function column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
    function column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
    function column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_type(Stmt: Psqlite3_stmt; iCol: Integer): String;
    function column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function OpenEncrypted(const zFilename: PAnsiChar; const pKey: PAnsiChar;
      nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;

  end;

  {** Implements a driver for SQLite 3 }
  TZSQLite3PlainDriver = class (TZSQLiteBaseDriver, IZPlainDriver, IZSQLitePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses ZPlainLoader, ZEncoding;

{ TZSQLiteBaseDriver }

function TZSQLiteBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8'
end;

procedure TZSQLiteBaseDriver.LoadCodePages;  //Egonhugeist
begin
  { MultiByte }
  AddCodePage('UTF-8', 1, ceUTF8, zCP_UTF8);
  AddCodePage('UTF-16le', 2, ceUTF16, zCP_UTF16, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
  AddCodePage('UTF-16be', 3, ceUTF16, zCP_UTF16BE, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
  AddCodePage('UTF-16', 4, ceUTF16, zCP_UTF16, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
end;

constructor TZSQLiteBaseDriver.Create;
begin
   inherited create;
   FLoader := TZNativeLibraryLoader.Create([]);
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    FLoader.AddLocation(LINUX_DLL_LOCATION+'.0');
  {$ENDIF}
end;

function TZSQLiteBaseDriver.AggregateContext(func: Psqlite_func;
  nBytes: Integer): Pointer;
begin
  Result := SQLite_API.sqlite_aggregate_context(func, nBytes);
end;

function TZSQLiteBaseDriver.AggregateCount(func: Psqlite_func): Integer;
begin
  Result := SQLite_API.sqlite_aggregate_count(func);
end;

procedure TZSQLiteBaseDriver.BusyHandler(db: Psqlite;
  callback: Tsqlite_busy_callback; ptr: Pointer);
begin
  SQLite_API.sqlite_busy_handler(db, callback, ptr);
end;

procedure TZSQLiteBaseDriver.BusyTimeout(db: Psqlite; ms: Integer);
begin
  SQLite_API.sqlite_busy_timeout(db, ms);
end;

function TZSQLiteBaseDriver.Changes(db: Psqlite): Integer;
begin
  Result := SQLite_API.sqlite_changes(db);
end;

function TZSQLiteBaseDriver.CommitHook(db: Psqlite;
  callback: Tsqlite_simple_callback; ptr: Pointer): Pointer;
begin
  Result := SQLite_API.sqlite_commit_hook(db, callback, ptr);
end;

function TZSQLiteBaseDriver.Complete(const sql: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_complete(sql);
end;

function TZSQLiteBaseDriver.CreateAggregate(db: Psqlite;
  const zName: PAnsiChar; nArg: Integer; callback: Tsqlite_function_callback;
  finalize: Tsqlite_finalize_callback; pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.CreateFunction(db: Psqlite;
  const zName: PAnsiChar; nArg: Integer; callback: Tsqlite_function_callback;
  pUserData: Pointer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.ErrorString(db: Psqlite; code: Integer): String;
var
  ErrorMessagePointer: PAnsiChar;
  ErrorMessage: String;
  ErrorString: String;
begin
  if code = SQLITE_OK then
  begin
    Result := 'not an error';
    Exit;
  end;

  if code = SQLITE_NOMEM then
  begin
    Result := 'out of memory';
    Exit;
  end;

  if ( db = nil ) or ( @SQLite_API.sqlite_errstr = nil ) then
  begin
    case code of
      SQLITE_OK:         Result := 'not an error';
      SQLITE_ERROR:      Result := 'SQL logic error or missing database';
      SQLITE_INTERNAL:   Result := 'internal SQLite implementation flaw';
      SQLITE_PERM:       Result := 'access permission denied';
      SQLITE_ABORT:      Result := 'callback requested query abort';
      SQLITE_BUSY:       Result := 'database is locked';
      SQLITE_LOCKED:     Result := 'database table is locked';
      SQLITE_NOMEM:      Result := 'out of memory';
      SQLITE_READONLY:   Result := 'attempt to write a readonly database';
      _SQLITE_INTERRUPT:  Result := 'interrupted';
      SQLITE_IOERR:      Result := 'disk I/O error';
      SQLITE_CORRUPT:    Result := 'database disk image is malformed';
      SQLITE_NOTFOUND:   Result := 'table or record not found';
      SQLITE_FULL:       Result := 'database is full';
      SQLITE_CANTOPEN:   Result := 'unable to open database file';
      SQLITE_PROTOCOL:   Result := 'database locking protocol failure';
      SQLITE_EMPTY:      Result := 'table contains no data';
      SQLITE_SCHEMA:     Result := 'database schema has changed';
      SQLITE_TOOBIG:     Result := 'too much data for one table row';
      SQLITE_CONSTRAINT: Result := 'constraint failed';
      SQLITE_MISMATCH:   Result := 'datatype mismatch';
      SQLITE_MISUSE:     Result := 'library routine called out of sequence';
      SQLITE_NOLFS:      Result := 'kernel lacks large file support';
      SQLITE_AUTH:       Result := 'authorization denied';
      SQLITE_FORMAT:     Result := 'auxiliary database format error';
      SQLITE_RANGE:      Result := 'bind index out of range';
      SQLITE_NOTADB:     Result := 'file is encrypted or is not a database';
    else
      Result := 'unknown error';
    end;

    exit;
  end
  else
  begin
    ErrorMessagePointer := Self.SQLite_API.sqlite_errstr(code);
    {$IFDEF UNICODE}
    ErrorString := Trim(UTF8ToUnicodeString(ErrorMessagePointer));
    {$ELSE}
      {$IFNDEF FPC}
      ErrorString := Trim(UTF8ToAnsi(StrPas(ErrorMessagePointer)));
      {$ELSE}
      ErrorString := Trim(ErrorMessagePointer);
      {$ENDIF}
    {$ENDIF}

    ErrorMessagePointer := Self.SQLite_API.sqlite_errmsg(db);
    {$IFDEF UNICODE}
    ErrorMessage := Trim(UTF8ToUnicodeString(ErrorMessagePointer));
    {$ELSE}
      {$IFNDEF FPC}
      ErrorMessage := Trim(UTF8ToAnsi(ErrorMessagePointer));
      {$ELSE}
      ErrorMessage := Trim(ErrorMessagePointer);
      {$ENDIF}
    {$ENDIF}

    Result := ErrorString + ': ' + ErrorMessage;
  end;
end;

function TZSQLiteBaseDriver.Execute(db: Psqlite; const sql: PAnsiChar;
  sqlite_callback: Tsqlite_callback; arg: Pointer;
  var errmsg: PAnsiChar): Integer;
begin
  errmsg:= nil;
  Result := SQLite_API.sqlite_exec(db, sql, sqlite_callback, arg, errmsg);
end;

procedure TZSQLiteBaseDriver.FreeMem(ptr: Pointer);
begin
  SQLite_API.sqlite_freemem(ptr);
end;

procedure TZSQLiteBaseDriver.FreeTable(var result: PAnsiChar);
begin
  SQLite_API.sqlite_free_table(result);
end;

function TZSQLiteBaseDriver.FunctionType(db: Psqlite;
  const zName: PAnsiChar; datatype: Integer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.GetTable(db: Psqlite; const sql: PAnsiChar;
  var resultp: PPAnsiChar; var nrow, ncolumn: Integer;
  var errmsg: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_get_table(db, sql, resultp, nrow, ncolumn,
    errmsg);
end;

procedure TZSQLiteBaseDriver.Interrupt(db: Psqlite);
begin
  SQLite_API.sqlite_interrupt(db);
end;

function TZSQLiteBaseDriver.LastInsertRowId(db: Psqlite): Int64;
begin
  Result := SQLite_API.sqlite_last_insert_rowid(db);
end;

function TZSQLiteBaseDriver.LastStatementChanges(db: Psqlite): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.LibEncoding: PAnsiChar;
begin
  Result := nil;
end;

function TZSQLiteBaseDriver.LibVersion: PAnsiChar;
begin
  Result := SQLite_API.sqlite_libversion;
end;

function TZSQLiteBaseDriver.Open(const filename: PAnsiChar; mode: Integer;
  var errmsg: PAnsiChar): Psqlite;
var
  Result0: Psqlite;
{$IFNDEF UNICODE}
  Version: string;
  FileNameString: String;
{$ENDIF}
begin
  Result0:= nil;
  (*Note to Windows users: The encoding used for the filename argument of
    sqlite3_open() and sqlite3_open_v2() must be UTF-8, not whatever codepage
    is currently defined. Filenames containing international characters must
    be converted to UTF-8 prior to passing them into sqlite3_open() or
    sqlite3_open_v2(). *)

{$IFDEF UNICODE}
  SQLite_API.sqlite_open(filename, Result0);
{$ELSE}
  Version := LibVersion;
  FileNameString := filename;
  if (Version > '3.2.5') then
    {$IFDEF FPC}
      SQLite_API.sqlite_open(PAnsiChar(FileNameString), Result0)
    {$ELSE}
      SQLite_API.sqlite_open(PAnsiChar(AnsiToUTF8(FileNameString)), Result0)
    {$ENDIF}
  else
    SQLite_API.sqlite_open(filename, Result0);
{$ENDIF}
  Result := Result0;
end;

function TZSQLiteBaseDriver.OpenEncrypted(const zFilename, pKey: PAnsiChar;
  nKey: Integer; var pErrcode: Integer; var pzErrmsg: PAnsiChar): Psqlite;
begin
  pErrcode := SQLITE_MISUSE;
  pzErrmsg := 'function is not used in the current version of the library';
  Result:= nil;
end;

procedure TZSQLiteBaseDriver.ProgressHandler(db: Psqlite; p1: Integer;
  callback: Tsqlite_simple_callback; ptr: Pointer);
begin
  SQLite_API.sqlite_progress_handler(db, p1, callback, ptr);
end;

function TZSQLiteBaseDriver.ReKey(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @SQLite_API.sqlite_rekey = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := SQLite_API.sqlite_rekey(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.Key(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @SQLite_API.sqlite_key = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := SQLite_API.sqlite_key(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.SetAuthorizer(db: Psqlite;
  callback: Tsqlite_auth_callback; pUserData: Pointer): Integer;
begin
  Result := SQLite_API.sqlite_set_authorizer(db, callback, pUserData);
end;

procedure TZSQLiteBaseDriver.SetResultDouble(func: Psqlite_func;
  arg: Double);
begin
  SQLite_API.sqlite_set_result_double(func, arg);
end;

procedure TZSQLiteBaseDriver.SetResultError(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer);
begin
  SQLite_API.sqlite_set_result_error(func, arg, len);
end;

procedure TZSQLiteBaseDriver.SetResultInt(func: Psqlite_func;
  arg: Integer);
begin
  SQLite_API.sqlite_set_result_int(func, arg);
end;

function TZSQLiteBaseDriver.SetResultString(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_set_result_string(func, arg, len, nil);
end;

{ Prepared statmenet api }
function TZSQLiteBaseDriver.Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
begin
  Result := SQLite_API.sqlite_prepare(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; pzTail: PPAnsichar): Integer;
begin
  Result := SQLite_API.sqlite_prepare_v2(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;
begin
  Result := SQLite_API.sqlite_prepare16(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; pzTail: ZPPWideChar): Integer;
begin
  Result := SQLite_API.sqlite_prepare16_v2(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_bind_parameter_count(pStmt);
end;

function TZSQLiteBaseDriver.bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
begin
  Result := SQLite_API.sqlite_bind_parameter_name(pStmt, ParamIndex);
end;

function TZSQLiteBaseDriver.bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;
begin
  Result := SQLite_API.sqlite_bind_parameter_index(pStmt, ZName);
end;

function TZSQLiteBaseDriver.clear_bindings(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_clear_bindings(pStmt);
end;

function TZSQLiteBaseDriver.column_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_column_count(pStmt);
end;

function TZSQLiteBaseDriver.column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_name(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_name16(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_database_name(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_database_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_database_name16(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_table_name(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_table_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_table_name16(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_origin_name(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_origin_name16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_origin_name16(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_decltype(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_decltype16(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_decltype16(pStmt, iCol);
end;

function TZSQLiteBaseDriver.Step(Stmt: Psqlite3_stmt; var pN: Integer;
  var pazValue, pazColName: PPAnsiChar): Integer;
var
  i: Integer;
  val, cname,ctype: PAnsiChar;
  pazValue0, pazColName0, pazColType: PPAnsiChar;
begin
  pazValue0 := nil; // satisfy compiler
  Result := SQLite_API.sqlite_step(Stmt);
  if (Result = SQLITE_ROW) or (Result = SQLITE_DONE) then
  begin
    pN:= SQLite_API.sqlite_column_count(Stmt);
    if Result = SQLITE_ROW then
    begin
      pazValue:= AllocMem(SizeOf(PPAnsiChar)*(pN+1));
      pazValue0:= pazValue;
    end;
    pazColName:= AllocMem(SizeOf(PPAnsiChar)*(pN+1)*2);
    pazColName0:= pazColName;
    pazColType:= pazColName;

    Inc(pazColType, pN);
    for i := 0 to pN - 1 do
    begin
      if Result = SQLITE_ROW then
      begin
        cname:= SQLite_API.sqlite_column_name(Stmt, i);
        ctype:= SQLite_API.sqlite_column_decltype(Stmt, i);
        val  := SQLite_API.sqlite_column_text(Stmt, i);
        pazValue0^ := val;
        inc(pazValue0);
      end
      else
      begin
        cname:= SQLite_API.sqlite_column_name(Stmt, i);
        ctype:= SQLite_API.sqlite_column_decltype(Stmt, i);
      end;
      pazColName0^:= cname;
      pazColType^ := ctype;
      inc(pazColName0);
      inc(pazColType);
    end;
    if Result = SQLITE_ROW then
         pazValue0^ := nil;
    pazColType^:= nil;
    if Result = SQLITE_DONE then
         pazValue := nil;
  end;
end;

function TZSQLiteBaseDriver.Step(Stmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_step(Stmt);
end;

function TZSQLiteBaseDriver.data_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_data_count(pStmt);
end;

function TZSQLiteBaseDriver.bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := SQLite_API.sqlite_bind_blob(pStmt, ParamIndex, Buffer, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
begin
  Result := SQLite_API.sqlite_bind_double(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
begin
  Result := SQLite_API.sqlite_bind_int(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
begin
  Result := SQLite_API.sqlite_bind_int64(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
begin
  Result := SQLite_API.sqlite_bind_null(pStmt, ParamIndex);
end;

function TZSQLiteBaseDriver.bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := SQLite_API.sqlite_bind_text(pStmt, ParamIndex, Text, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := SQLite_API.sqlite_bind_text16(pStmt, ParamIndex, Text, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
begin
  Result := SQLite_API.sqlite_bind_value(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;
begin
  Result := SQLite_API.sqlite_bind_zeroblob(pStmt, ParamIndex, N);
end;

function TZSQLiteBaseDriver.finalize(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_finalize(pStmt);
end;

function TZSQLiteBaseDriver.reset(pStmt: Psqlite3_stmt): Integer;
begin
  Result := SQLite_API.sqlite_reset(pStmt);
end;

function TZSQLiteBaseDriver.column_blob(Stmt: Psqlite3_stmt; iCol:integer): TStream;
var
  P : Pointer;
  len : integer;
begin
  result := TMemoryStream.Create;
  P := SQLite_API.sqlite_column_blob(Stmt, iCol-1);
  len := SQLite_API.sqlite_column_bytes(Stmt, iCol-1);
  result.WriteBuffer(P^,len);
end;

function TZSQLiteBaseDriver.column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
begin
  Result := SQLite_API.sqlite_column_bytes(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
begin
  Result := SQLite_API.sqlite_column_bytes16(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
begin
  Result := SQLite_API.sqlite_column_double(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
begin
  Result := SQLite_API.sqlite_column_int(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
begin
  Result := SQLite_API.sqlite_column_int64(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := SQLite_API.sqlite_column_text(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := SQLite_API.sqlite_column_text16(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_type(Stmt: Psqlite3_stmt; iCol: Integer): String;
begin
  if Assigned(SQLite_API.sqlite_column_type) then
    case SQLite_API.sqlite_column_type(stmt, iCol) of
      SQLITE_INTEGER:
         Result:='INT(19)';
      SQLITE_FLOAT:
         Result:='FLOAT(16)';
      SQLITE3_TEXT:
         //RESULT := 'CHAR'; //EgonHugeist: Need to boil down this type  !
                           //Else Metadatainformations are not readable !
         Result:='VARCHAR';
      SQLITE_BLOB:
         Result:='BLOB';
      SQLITE_NULL: Result := '';
    end
  else
    Result:='';
end;

function TZSQLiteBaseDriver.column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;
begin
  Result := SQLite_API.sqlite_column_value(stmt, iCol);
end;

function TZSQLiteBaseDriver.Trace(db: Psqlite;
  callback: Tsqlite_trace_callback; ptr: Pointer): Pointer;
begin
  Result := SQLite_API.sqlite_trace(db, callback, ptr);
end;

function TZSQLiteBaseDriver.UserData(func: Psqlite_func): Pointer;
begin
  Result := SQLite_API.sqlite_user_data(func);
end;

function TZSQLiteBaseDriver.Close(db: Psqlite): Integer;
begin
  Result := SQLite_API.sqlite_close(db);
end;

{ TZSQLite3PlainDriver }

function TZSQLite3PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZSQLite3PlainDriver.Create;
end;

procedure TZSQLite3PlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @SQLite_API.sqlite_open                   := GetAddress('sqlite3_open');
  @SQLite_API.sqlite_close                  := GetAddress('sqlite3_close');

  { prepared Statment api }
  { prepared statement api }
  @SQLite_API.sqlite_prepare                := GetAddress('sqlite3_prepare');
  @SQLite_API.sqlite_prepare_v2             := GetAddress('sqlite3_prepare_v2');
  @SQLite_API.sqlite_prepare16              := GetAddress('sqlite3_prepare16');
  @SQLite_API.sqlite_prepare16_v2           := GetAddress('sqlite3_prepare16_v2');

  @SQLite_API.sqlite_bind_parameter_count   := GetAddress('sqlite3_bind_parameter_count');
  @SQLite_API.sqlite_bind_parameter_name    := GetAddress('sqlite3_bind_parameter_name');
  @SQLite_API.sqlite_bind_parameter_index   := GetAddress('sqlite3_bind_parameter_index');

  @SQLite_API.sqlite_clear_bindings         := GetAddress('sqlite3_clear_bindings');

  @SQLite_API.sqlite_column_count           := GetAddress('sqlite3_column_count');
  @SQLite_API.sqlite_column_bytes           := GetAddress('sqlite3_column_bytes');
  @SQLite_API.sqlite_column_bytes16         := GetAddress('sqlite3_column_bytes16');
  @SQLite_API.sqlite_Column_blob            := GetAddress('sqlite3_column_blob');
  @SQLite_API.sqlite_column_double          := GetAddress('sqlite3_column_double');
  @SQLite_API.sqlite_column_int             := GetAddress('sqlite3_column_int');
  @SQLite_API.sqlite_column_int64           := GetAddress('sqlite3_column_int64');
  @SQLite_API.sqlite_column_text            := GetAddress('sqlite3_column_text');
  @SQLite_API.sqlite_column_text16          := GetAddress('sqlite3_column_text16');
  @SQLite_API.sqlite_column_type            := GetAddress('sqlite3_column_type');
  @SQLite_API.sqlite_column_value           := GetAddress('sqlite3_column_value');
  @SQLite_API.sqlite_column_name            := GetAddress('sqlite3_column_name');
  @SQLite_API.sqlite_column_name16          := GetAddress('sqlite3_column_name16');

  @SQLite_API.sqlite_column_database_name   := GetAddress('sqlite3_column_database_name');
  @SQLite_API.sqlite_column_database_name16 := GetAddress('sqlite3_column_database_name16');
  @SQLite_API.sqlite_column_table_name      := GetAddress('sqlite3_column_table_name');
  @SQLite_API.sqlite_column_table_name16    := GetAddress('sqlite3_column_table_name16');
  @SQLite_API.sqlite_column_origin_name     := GetAddress('sqlite3_column_origin_name');
  @SQLite_API.sqlite_column_origin_name16   := GetAddress('sqlite3_column_origin_name16');

  @SQLite_API.sqlite_column_decltype        := GetAddress('sqlite3_column_decltype');
  @SQLite_API.sqlite_column_decltype16      := GetAddress('sqlite3_column_decltype16');

  @SQLite_API.sqlite_step                   := GetAddress('sqlite3_step');
  @SQLite_API.sqlite_data_count             := GetAddress('sqlite3_data_count');

  @SQLite_API.sqlite_bind_blob              := GetAddress('sqlite3_bind_blob');
  @SQLite_API.sqlite_bind_double            := GetAddress('sqlite3_bind_double');
  @SQLite_API.sqlite_bind_int               := GetAddress('sqlite3_bind_int');
  @SQLite_API.sqlite_bind_int64             := GetAddress('sqlite3_bind_int64');
  @SQLite_API.sqlite_bind_null              := GetAddress('sqlite3_bind_null');
  @SQLite_API.sqlite_bind_text              := GetAddress('sqlite3_bind_text');
  @SQLite_API.sqlite_bind_text16            := GetAddress('sqlite3_bind_text16');
  @SQLite_API.sqlite_bind_value             := GetAddress('sqlite3_bind_value');
  @SQLite_API.sqlite_bind_zeroblob          := GetAddress('sqlite3_bind_zeroblob');

  @SQLite_API.sqlite_finalize               := GetAddress('sqlite3_finalize');
  @SQLite_API.sqlite_reset                  := GetAddress('sqlite3_reset');

  @SQLite_API.sqlite_exec                   := GetAddress('sqlite3_exec');
  @SQLite_API.sqlite_last_insert_rowid      := GetAddress('sqlite3_last_insert_rowid');
  @SQLite_API.sqlite_changes                := GetAddress('sqlite3_changes');
//  @SQLite_API.sqlite_last_statement_changes := GetAddress('sqlite3_last_statement_changes');
  @SQLite_API.sqlite_errmsg                 := GetAddress('sqlite3_errmsg');
  @SQLite_API.sqlite_errstr                 := GetAddress('sqlite3_errstr');
  @SQLite_API.sqlite_interrupt              := GetAddress('sqlite3_interrupt');
  @SQLite_API.sqlite_complete               := GetAddress('sqlite3_complete');
  @SQLite_API.sqlite_busy_handler           := GetAddress('sqlite3_busy_handler');
  @SQLite_API.sqlite_busy_timeout           := GetAddress('sqlite3_busy_timeout');
  @SQLite_API.sqlite_get_table              := GetAddress('sqlite3_get_table');
  @SQLite_API.sqlite_free_table             := GetAddress('sqlite3_free_table');
  @SQLite_API.sqlite_freemem                := GetAddress('sqlite3_free');
  @SQLite_API.sqlite_libversion             := GetAddress('sqlite3_libversion');
//  @SQLite_API.sqlite_libencoding            := GetAddress('sqlite3_libencoding');
//  @SQLite_API.sqlite_create_function        := GetAddress('sqlite3_create_function');
//  @SQLite_API.sqlite_create_aggregate       := GetAddress('sqlite3_create_aggregate');
//  @SQLite_API.sqlite_function_type          := GetAddress('sqlite3_function_type');
  @SQLite_API.sqlite_set_result_string      := GetAddress('sqlite3_result_string');
  @SQLite_API.sqlite_set_result_int         := GetAddress('sqlite3_result_int');
  @SQLite_API.sqlite_set_result_double      := GetAddress('sqlite3_result_double');
  @SQLite_API.sqlite_set_result_error       := GetAddress('sqlite3_result_error');
  @SQLite_API.sqlite_user_data              := GetAddress('sqlite3_user_data');
  @SQLite_API.sqlite_aggregate_context      := GetAddress('sqlite3_aggregate_context');
  @SQLite_API.sqlite_aggregate_count        := GetAddress('sqlite3_aggregate_count');
  @SQLite_API.sqlite_set_authorizer         := GetAddress('sqlite3_set_authorizer');
  @SQLite_API.sqlite_trace                  := GetAddress('sqlite3_trace');
  @SQLite_API.sqlite_progress_handler       := GetAddress('sqlite3_progress_handler');
  @SQLite_API.sqlite_commit_hook            := GetAddress('sqlite3_commit_hook');
//  @SQLite_API.sqlite_open_encrypted         := GetAddress('sqlite3_open_encrypted');
  @SQLite_API.sqlite_rekey                  := GetAddress('sqlite3_rekey');
  @SQLite_API.sqlite_key                    := GetAddress('sqlite3_key');
  end;
end;

constructor TZSQLite3PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL3_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL3_LOCATION);
    FLoader.AddLocation(LINUX_DLL3_LOCATION+'.0');
  {$ENDIF}
  LoadCodePages;
end;

function TZSQLite3PlainDriver.GetProtocol: string;
begin
  Result := 'sqlite-3';
end;

function TZSQLite3PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for SQLite 3';
end;

end.

