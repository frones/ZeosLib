{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 4.0                         }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}
{*********************************************************}
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
{                                                         }
{*********************************************************}

unit ZPlainMySql40;

interface

{$I ZPlain.inc}

{$J+}

uses Classes, ZPlainLoader, ZCompatibility, ZPlainMySqlConstants;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION = 'libmysql40.dll';

{ General Declarations }
//  PROTOCOL_VERSION     = 10;
//  FRM_VER              = 6;

{ Enum Field Types }
  FIELD_TYPE_DECIMAL   = 0;
  FIELD_TYPE_TINY      = 1;
  FIELD_TYPE_SHORT     = 2;
  FIELD_TYPE_LONG      = 3;
  FIELD_TYPE_FLOAT     = 4;
  FIELD_TYPE_DOUBLE    = 5;
  FIELD_TYPE_NULL      = 6;
  FIELD_TYPE_TIMESTAMP = 7;
  FIELD_TYPE_LONGLONG  = 8;
  FIELD_TYPE_INT24     = 9;
  FIELD_TYPE_DATE      = 10;
  FIELD_TYPE_TIME      = 11;
  FIELD_TYPE_DATETIME  = 12;
  FIELD_TYPE_YEAR      = 13;
  FIELD_TYPE_NEWDATE   = 14;
  FIELD_TYPE_ENUM      = 247;
  FIELD_TYPE_SET       = 248;
  FIELD_TYPE_TINY_BLOB = 249;
  FIELD_TYPE_MEDIUM_BLOB = 250;
  FIELD_TYPE_LONG_BLOB = 251;
  FIELD_TYPE_BLOB      = 252;
  FIELD_TYPE_VAR_STRING = 253;
  FIELD_TYPE_STRING    = 254;
  FIELD_TYPE_GEOMETRY  = 255;

{ For Compatibility }
  FIELD_TYPE_CHAR      = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL  = FIELD_TYPE_ENUM;

  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;

  MANAGER_OK           = 200;
  MANAGER_INFO         = 250;
  MANAGER_ACCESS       = 401;
  MANAGER_CLIENT_ERR   = 450;
  MANAGER_INTERNAL_ERR = 500;

{ ****************** Plain API Types definition ***************** }

type
  TClientCapabilities = (
    CLIENT_LONG_PASSWORD,
    CLIENT_FOUND_ROWS,
    CLIENT_LONG_FLAG,
    CLIENT_CONNECT_WITH_DB,
    CLIENT_NO_SCHEMA,
    CLIENT_COMPRESS,
    CLIENT_ODBC,
    CLIENT_LOCAL_FILES,
    CLIENT_IGNORE_SPACE
  );

  TSetClientCapabilities = set of TClientCapabilities;

  TRefreshOptions = (
    _REFRESH_GRANT,
    _REFRESH_LOG,
    _REFRESH_TABLES,
    _REFRESH_HOSTS,
    _REFRESH_FAST
  );
  TSetRefreshOptions = set of TRefreshOptions;

  TMySqlStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

  TMySqlOption = (
    MYSQL_OPT_CONNECT_TIMEOUT,
    MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,
    MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,
    MYSQL_SET_CHARSET_NAME,
    MYSQL_OPT_LOCAL_INFILE
  );

  TMySqlRplType = (
    MYSQL_RPL_MASTER,
    MYSQL_RPL_SLAVE,
    MYSQL_RPL_ADMIN
  );

  PUSED_MEM=^USED_MEM;
  USED_MEM = packed record
    next:       PUSED_MEM;
    left:       Integer;
    size:       Integer;
  end;

  PERR_PROC = ^ERR_PROC;
  ERR_PROC = procedure;

  PMEM_ROOT = ^MEM_ROOT;
  MEM_ROOT = packed record
    free:          PUSED_MEM;
    used:          PUSED_MEM;
    pre_alloc:     PUSED_MEM;
    min_malloc:    Integer;
    block_size:    Integer;
    block_num:     Integer;
    first_block_usage: Integer;
    error_handler: PERR_PROC;
  end;

  NET = packed record
    hPipe:         Pointer;
    buff:          PChar;
    buff_end:      PChar;
    write_pos:     PChar;
    read_pos:      PChar;
    fd:            Integer;
    max_packet:    Cardinal;
    max_packet_size: Cardinal;
    last_errno:    Cardinal;
    pkt_nr:        Cardinal;
    compress_pkt_nr: Cardinal;
    write_timeout:  Cardinal;
    read_timeout:   Cardinal;
    retry_count:  Cardinal;
    fcntl:         Integer;
    last_error:    array[01..MYSQL_ERRMSG_SIZE] of Char;
    error:         Char;
    return_errno:  Byte;
    compress:      Byte;
    remain_in_buf: LongInt;
    length:        LongInt;
    buf_length:    LongInt;
    where_b:       LongInt;
    return_status: Pointer;
    reading_or_writing: Char;
    save_char:     Char;
    no_send_ok:    Byte;
    query_cache_query: Pointer;
  end;

  PMYSQL_FIELD = ^MYSQL_FIELD;
  MYSQL_FIELD = record
    name:       PChar;   // Name of column
    table:      PChar;   // Table of column if column was a field
    org_table:  PChar;   // Org table name if table was an alias
    db:         PChar;   // Database for table
    def:        PChar;   // Default value (set by mysql_list_fields)
    length:     LongInt; // Width of column
    max_length: LongInt; // Max width of selected set
    flags:      Integer; // Div flags
    decimals:   Integer; // Number of decimals in field
    _type:      Byte;    // Type of field. Se mysql_com.h for types
  end;

  MYSQL_FIELD_OFFSET = Cardinal;

  MYSQL_ROW = array[00..$ff] of PChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_ROW_OFFSET = PMYSQL_ROWS;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  _MYSQL_OPTIONS = record
    connect_timeout: Cardinal;
    clientFlag:      Cardinal;
    port:            Cardinal;
    host:            PChar;
    init_command:    PChar;
    user:            PChar;
    password:        PChar;
    unix_socket:     PChar;
    db:              PChar;
    my_cnf_file:     PChar;
    my_cnf_group:    PChar;
    charset_dir:     PChar;
    charset_name:    PChar;
    ssl_key:         PChar;
    ssl_cert:        PChar;
    ssl_ca:          PChar;
    ssl_capath:      PChar;
    ssl_cipher:      PChar;
    max_allowed_packet: LongInt;
    use_ssl:         Byte;
    compress:        Byte;
    named_pipe:      Byte;
    rpl_probe:       Byte;
    rpl_parse:       Byte;
    no_master_reads: Byte;
  end;

  PMYSQL_OPTIONS = ^_MYSQL_OPTIONS;

  PMYSQL = ^MYSQL;
  MYSQL = record
    _net:            NET;
    connector_fd:    Pointer;
    host:            PChar;
    user:            PChar;
    passwd:          PChar;
    unix_socket:     PChar;
    server_version:  PChar;
    host_info:       PChar;
    info:            PChar;
    db:              PChar;
    charset:         PChar;
    fields:          PMYSQL_FIELD;
    field_alloc:     MEM_ROOT;
    affected_rows:   Int64;
    insert_id:       Int64;
    extra_info:      Int64;
    thread_id:       LongInt;
    packet_length:   LongInt;
    port:            Cardinal;
    client_flag:     Cardinal;
    server_capabilities: Cardinal;
    protocol_version: Cardinal;
    field_count:     Cardinal;
    server_status:   Cardinal;
    server_language: Cardinal;
    options:         _mysql_options;
    status:          TMySqlStatus;
    free_me:         Byte;
    reconnect:       Byte;
    scramble_buff:   array[0..8] of Char;
    rpl_pivot:       Byte;
    master:          PMYSQL;
    next_slave:      PMYSQL;
    last_used_slave: PMYSQL;
    last_used_con:   PMYSQL;
  end;

  MYSQL_RES = packed record
    row_count:       Int64;
    fields:          PMYSQL_FIELD;
    data:            PMYSQL_DATA;
    data_cursor:     PMYSQL_ROWS;
    lengths:         PLongInt;
    handle:          PMYSQL;
    field_alloc:     MEM_ROOT;
    field_count:     Integer;
    current_field:   Integer;
    row:             PMYSQL_ROW;
    current_row:     PMYSQL_ROW;
    eof:             Byte;
  end;
  PMYSQL_RES = ^MYSQL_RES;

  TModifyType = (MODIFY_INSERT, MODIFY_UPDATE, MODIFY_DELETE);
  TQuoteOptions = (QUOTE_STRIP_CR,QUOTE_STRIP_LF);
  TQuoteOptionsSet = set of TQuoteOptions;

  MYSQL_MANAGER = record
    _net:               NET;
    host:               PChar;
    user:               PChar;
    passwd:             PChar;
    port:               Cardinal;
    free_me:            Byte;
    eof:                Byte;
    cmd_status:         Integer;
    last_errno:         Integer;
    net_buf:            PChar;
    net_buf_pos:        PChar;
    net_data_end:       PChar;
    net_buf_size:       Integer;
    last_error:         array[1..MAX_MYSQL_MANAGER_ERR] of Char;
  end;
  PMYSQL_MANAGER = ^MYSQL_MANAGER;

{ ************** Plain API Function types definition ************* }

  { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }

  Tmysql_affected_rows          = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  //Tmysql_autocommit             = function(Handle: PMYSQL; const mode: Byte): Byte;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_change_user            = function(mysql: PMYSQL; const user: PChar; const passwd: PChar; const db: PChar): Byte;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_character_set_name     = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_close                  = procedure(Handle: PMYSQL);                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  //Tmysql_commit                 = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_connect                = function(Handle: PMYSQL; const Host, User, Passwd: PChar): PMYSQL;   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_create_db              = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_data_seek              = procedure(Result: PMYSQL_RES; Offset: Int64);                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_debug                  = procedure(Debug: PChar);                                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_drop_db                = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_dump_debug_info        = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_eof                    = function(Result: PMYSQL_RES): Byte;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_errno                  = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_error                  = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_escape_string          = function(PTo, PFrom: PChar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field            = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field_direct     = function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD;       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_fields           = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_lengths          = function(Result: PMYSQL_RES): PLongInt;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_row              = function(Result: PMYSQL_RES): PMYSQL_ROW;                            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_count            = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_seek             = function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_tell             = function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_free_result            = procedure(Result: PMYSQL_RES);                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_get_character_set_info = procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO);
  Tmysql_get_client_info        = function: PChar;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_client_version     = function: Cardinal;                                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_host_info          = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_proto_info         = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_info        = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_get_server_version     = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_hex_string             = function(PTo, PFrom: Pchar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_info                   = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_init                   = function(Handle: PMYSQL): PMYSQL;                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_insert_id              = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_kill                   = function(Handle: PMYSQL; Pid: LongInt): Integer;                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_dbs               = function(Handle: PMYSQL; Wild: PChar): PMYSQL_RES;                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_fields            = function(Handle: PMYSQL; const Table, Wild: PChar): PMYSQL_RES;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_processes         = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_tables            = function(Handle: PMYSQL; const Wild: PChar): PMYSQL_RES;             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_more_results           = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_next_result            = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_fields             = function(Result: PMYSQL_RES): Cardinal;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_rows               = function(Result: PMYSQL_RES): Int64;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_options                = function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PChar): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ping                   = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_query                  = function(Handle: PMYSQL; const Query: PChar): Integer;               {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_connect           = function(Handle: PMYSQL; const Host, User, Passwd, Db: PChar;
                                           Port: Cardinal; const UnixSocket: PChar; ClientFlag: Cardinal): PMYSQL;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_escape_string     = function(Handle: PMYSQL; PTo: PChar; const PFrom: PChar; length: Cardinal): Cardinal;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_query             = function(Handle: PMYSQL; const Query: PChar; Length: Cardinal): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_refresh                = function(Handle: PMYSQL; Options: Cardinal): Integer;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_rollback               = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_seek               = function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_tell               = function(Result: PMYSQL_RES): PMYSQL_ROWS;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_select_db              = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_set_character_set      = function(Handle: PMYSQL; csname: PChar): Integer;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_set_server_option      = function(Handle: PMYSQL; Option: TMysqlSetOption): Integer;          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_shutdown               = function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel):
  Tmysql_shutdown = function(Handle: PMYSQL): Integer;    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//                                  Integer;                                                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_sqlstate               = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ssl_set                = function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:
                                  PChar): Byte;                                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stat                   = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_store_result           = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_id              = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_use_result             = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_warning_count          = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  { Set up and bring down a thread; these function should be called for each thread in an application which
    opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }

  Tmy_init                      = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_init            = function: Byte;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_safe            = function: Cardinal;                                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  { Set up and bring down the server; to ensure that applications will work when linked against either the
    standard client library or the embedded server library, these functions should be called. }

  Tmysql_server_init            = function(Argc: Integer; Argv, Groups: Pointer): Integer;            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_server_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


{BELOW are new PREPARED STATEMENTS}

//  Tmysql_stmt_affected_rows     = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_attr_get          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
//                                  arg: PChar): Integer;                                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_attr_set          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
//                                 const arg: PChar): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_bind_param        = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_bind_result       = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_close             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_data_seek         = procedure(stmt: PMYSQL_STMT; offset: Int64);                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_errno             = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_error             = function(stmt: PMYSQL_STMT): PChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_execute           = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_fetch             = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_fetch_column      = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND; column: Cardinal;
//                                  offset: Cardinal): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_field_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_free_result       = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_init              = function(Handle: PMYSQL): PMYSQL_STMT;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_insert_id         = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_num_rows          = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_param_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_param_metadata    = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_prepare           = function(stmt: PMYSQL_STMT; const query: PChar; length: Cardinal):
//                                  Integer;                                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_reset             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_result_metadata   = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_row_seek          = function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS;     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_row_tell          = function(stmt: PMYSQL_STMT): PMYSQL_ROWS;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_send_long_data    = function(stmt: PMYSQL_STMT; parameter_number: Cardinal; const
//                                  data: PChar; length: Cardinal): Byte;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_sqlstate          = function(stmt: PMYSQL_STMT): PChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tmysql_stmt_store_result      = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{BELOW are functions not implemented in PDO}
  Tmysql_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_read_query_result = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on master }
  Tmysql_master_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_master_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on slave }
  Tmysql_slave_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_slave_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable parsing of all queries to decide
    if they go on master or slave }
  Tmysql_enable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the parse flag }
  Tmysql_rpl_parse_enabled = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable reads from master }
  Tmysql_enable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the master read flag }
  Tmysql_reads_from_master_enabled = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_rpl_query_type = function(const query: PChar; len: Integer):
    TMySqlRplType; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Discover the master and its slaves }
  Tmysql_rpl_probe = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set the master, close/free the old one, if it is not a pivot }
  Tmysql_set_master = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_add_slave = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_init = function(con: PMYSQL_MANAGER): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_connect = function(con: PMYSQL_MANAGER; const host: PChar;
    const user: PChar; const passwd: PChar; port: Cardinal): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_close = procedure(con: PMYSQL_MANAGER);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_command = function(con: PMYSQL_MANAGER; const cmd: PChar;
    cmd_len: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_fetch_line = function(con: PMYSQL_MANAGER; res_buf: PChar;
    res_buf_size: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

var

{ ************* Plain API Function variables definition ************ }

  mysql_affected_rows:          Tmysql_affected_rows;	        {mysql 3.2}
//  mysql_autocommit:             Tmysql_autocommit;              {mysql 4.1}
  mysql_change_user:            Tmysql_change_user;             {mysql 3.23}
  mysql_character_set_name:     Tmysql_character_set_name;      {mysql 3.2}
  mysql_close:                  Tmysql_close;                   {mysql 3.2}
//  mysql_commit:                 Tmysql_commit;                  {mysql 4.1}
  mysql_connect:                Tmysql_connect;                 {mysql 3.2} {deprecated for mysql_real_connect}
  mysql_create_db:              Tmysql_create_db;               {mysql 3.2} {deprecated for mysql_query}
  mysql_data_seek:              Tmysql_data_seek;               {mysql 3.2}
  mysql_debug:                  Tmysql_debug;                   {mysql 3.2}
  mysql_drop_db:                Tmysql_drop_db;                 {mysql 3.2} {deprecated for mysql_query}
  mysql_dump_debug_info:        Tmysql_dump_debug_info;	        {mysql 3.2}
  mysql_eof:                    Tmysql_eof;                     {mysql 3.2} {deprecated for mysql_error/mysql_errno}
  mysql_errno:                  Tmysql_errno;	                {mysql 3.2}
  mysql_error:                  Tmysql_error;                   {mysql 3.2}
  mysql_escape_string:          Tmysql_escape_string;           {mysql 3.2} {deprecated for mysql_real_escape_string}
  mysql_fetch_field:            Tmysql_fetch_field;             {mysql 3.2}
  mysql_fetch_field_direct:     Tmysql_fetch_field_direct;      {mysql 3.2}
  mysql_fetch_fields:           Tmysql_fetch_fields;            {mysql 3.2}
  mysql_fetch_lengths:          Tmysql_fetch_lengths;           {mysql 3.2}
  mysql_fetch_row:              Tmysql_fetch_row;               {mysql 3.2}
  mysql_field_count:            Tmysql_field_count;             {mysql 3.22}
  mysql_field_seek:             Tmysql_field_seek;              {mysql 3.2}
  mysql_field_tell:             Tmysql_field_tell;              {mysql 3.2}
  mysql_free_result:            Tmysql_free_result;             {mysql 3.2}
//  mysql_get_character_set_info: Tmysql_get_character_set_info; {mysql 5.0.10}
  mysql_get_client_info:        Tmysql_get_client_info;         {mysql 3.2}
  mysql_get_client_version:     Tmysql_get_client_version;      {mysql 4.0}
  mysql_get_host_info:          Tmysql_get_host_info;           {mysql 3.2}
  mysql_get_proto_info:         Tmysql_get_proto_info;          {mysql 3.2}
  mysql_get_server_info:        Tmysql_get_server_info;         {mysql 3.2}
//  mysql_get_server_version:     Tmysql_get_server_version;      {mysql 4.1}
//  mysql_hex_string:             Tmysql_hex_string;              {mysql 4.1.8}
  mysql_info:                   Tmysql_info;                    {mysql 3.2}
  mysql_init:                   Tmysql_init;                    {mysql 3.2}
  mysql_insert_id:              Tmysql_insert_id;               {mysql 3.2}
  mysql_kill:                   Tmysql_kill;                    {mysql 3.2}
  mysql_list_dbs:               Tmysql_list_dbs;                {mysql 3.2}
  mysql_list_fields:            Tmysql_list_fields;             {mysql 3.2}
  mysql_list_processes:         Tmysql_list_processes;          {mysql 3.2}
  mysql_list_tables:            Tmysql_list_tables;             {mysql 3.2}
//  mysql_more_results:           Tmysql_more_results;            {mysql 4.1}
//  mysql_next_result:            Tmysql_next_result;             {mysql 4.1}
  mysql_num_fields:             Tmysql_num_fields;              {mysql 3.2}
  mysql_num_rows:               Tmysql_num_rows;                {mysql 3.2}
  mysql_options:                Tmysql_options;                 {mysql 3.2}
  mysql_ping:                   Tmysql_ping;	                {mysql 3.2}
  mysql_query:                  Tmysql_query;                   {mysql 3.2} {deprecated for mysql_real_query}
  mysql_real_connect:           Tmysql_real_connect;            {mysql 3.2}
  mysql_real_escape_string:     Tmysql_real_escape_string;      {mysql 3.2}
  mysql_real_query:             Tmysql_real_query;              {mysql 3.2}
  mysql_refresh:                Tmysql_refresh;                 {mysql 3.2}
//  mysql_rollback:               Tmysql_rollback;                {mysql 4.1}
  mysql_row_seek:               Tmysql_row_seek;                {mysql 3.2}
  mysql_row_tell:               Tmysql_row_tell;                {mysql 3.2}
  mysql_select_db:              Tmysql_select_db;               {mysql 3.2}
//  mysql_set_character_set:      Tmysql_set_character_set;       {mysql 4.1.13}
//  mysql_set_server_option:      Tmysql_set_server_option;       {mysql 4.1}
//  mysql_shutdown:               Tmysql_shutdown;                {mysql 3.2} {new argument 4.1}
  mysql_shutdown:       Tmysql_shutdown;
//  mysql_sqlstate:               Tmysql_sqlstate;                {mysql 4.1}
  mysql_ssl_set:                Tmysql_ssl_set;                 {mysql 3.2}
  mysql_stat:                   Tmysql_stat;                    {mysql 3.2}
  mysql_store_result:           Tmysql_store_result;            {mysql 3.2}
  mysql_thread_id:              Tmysql_thread_id;               {mysql 3.2}
  mysql_use_result:             Tmysql_use_result;              {mysql 3.2}
//  mysql_warning_count:          Tmysql_warning_count;           {mysql 4.1}


  {API for THREADED FUNCTIONS }

  my_init:                      Tmy_init;                       {mysql 3.2}
  mysql_thread_init:            Tmysql_thread_init;             {mysql 3.2}
  mysql_thread_end:             Tmysql_thread_end;              {mysql 3.2}
  mysql_thread_safe:            tmysql_thread_safe;             {mysql 3.2}


  {API for EMBEDDED SERVER  }

  mysql_server_init:            Tmysql_server_init;             {mysql 3.2}
  mysql_server_end:             Tmysql_server_end;              {mysql 3.2}


  {API for PREPARED STATEMENTS}

//  mysql_stmt_affected_rows:     Tmysql_stmt_affected_rows;      {mysql 4.1.0}
//  mysql_stmt_attr_get:          Tmysql_stmt_attr_get;           {mysql 4.1.2}
//  mysql_stmt_attr_set:          Tmysql_stmt_attr_set;           {mysql 4.1.2} {augmented 5.0.2/6}
//  mysql_stmt_bind_param:        Tmysql_stmt_bind_param;         {mysql 4.1.2}
//  mysql_stmt_bind_result:       Tmysql_stmt_bind_result;        {mysql 4.1.2}
//  mysql_stmt_close:             Tmysql_stmt_close;              {mysql 4.1.0}
//  mysql_stmt_data_seek:         Tmysql_stmt_data_seek;          {mysql 4.1.1}
//  mysql_stmt_errno:             Tmysql_stmt_errno;              {mysql 4.1.0}
//  mysql_stmt_error:             Tmysql_stmt_error;              {mysql 4.1.0}
//  mysql_stmt_execute:           Tmysql_stmt_execute;            {mysql 4.1.2}
//  mysql_stmt_fetch:             Tmysql_stmt_fetch;              {mysql 4.1.2}
//  mysql_stmt_fetch_column:      Tmysql_stmt_fetch_column;       {mysql 4.1.2}
//  mysql_stmt_field_count:       Tmysql_stmt_field_count;        {mysql 4.1.3}
//  mysql_stmt_free_result:       Tmysql_stmt_free_result;        {mysql 4.1.1}
//  mysql_stmt_init:              Tmysql_stmt_init;               {mysql 4.1.2}
//  mysql_stmt_insert_id:         Tmysql_stmt_insert_id;          {mysql 4.1.2}
//  mysql_stmt_num_rows:          Tmysql_stmt_num_rows;           {mysql 4.1.1}
//  mysql_stmt_param_count:       Tmysql_stmt_param_count;        {mysql 4.1.2}
//  mysql_stmt_param_metadata:    Tmysql_stmt_param_metadata;     {mysql 4.1.2}
//  mysql_stmt_prepare:           Tmysql_stmt_prepare;            {mysql 4.1.2}
//  mysql_stmt_reset:             Tmysql_stmt_reset;              {mysql 4.1.1}
//  mysql_stmt_result_metadata:   Tmysql_stmt_result_metadata;    {mysql 4.1.2}
//  mysql_stmt_row_seek:          Tmysql_stmt_row_seek;           {mysql 4.1.1}
//  mysql_stmt_row_tell:          Tmysql_stmt_row_tell;           {mysql 4.1.1}
//  mysql_stmt_send_long_data:    Tmysql_stmt_send_long_data;     {mysql 4.1.2}
//  mysql_stmt_sqlstate:          Tmysql_stmt_sqlstate;           {mysql 4.1.1}
//  mysql_stmt_store_result:      Tmysql_stmt_store_result;       {mysql 4.1.0}

  {BELOW are functions not implemented in PDO}

  mysql_send_query:     Tmysql_send_query;
  mysql_read_query_result: Tmysql_read_query_result;
  mysql_master_query:   Tmysql_master_query;
  mysql_master_send_query: Tmysql_master_send_query;
  mysql_slave_query:    Tmysql_slave_query;
  mysql_slave_send_query: Tmysql_slave_send_query;
  mysql_enable_rpl_parse: Tmysql_enable_rpl_parse;
  mysql_disable_rpl_parse: Tmysql_disable_rpl_parse;
  mysql_rpl_parse_enabled: Tmysql_rpl_parse_enabled;
  mysql_enable_reads_from_master: Tmysql_enable_reads_from_master;
  mysql_disable_reads_from_master: Tmysql_disable_reads_from_master;
  mysql_reads_from_master_enabled: Tmysql_reads_from_master_enabled;
  mysql_rpl_query_type: Tmysql_rpl_query_type;
  mysql_rpl_probe:      Tmysql_rpl_probe;
  mysql_set_master:     Tmysql_set_master;
  mysql_add_slave:      Tmysql_add_slave;
  mysql_manager_init:   Tmysql_manager_init;
  mysql_manager_connect: Tmysql_manager_connect;
  mysql_manager_close:  Tmysql_manager_close;
  mysql_manager_command: Tmysql_manager_command;
  mysql_manager_fetch_line: Tmysql_manager_fetch_line;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for MySQL native library. }
  TZMySQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZMySQLNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZMySQLNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

  @mysql_affected_rows          := GetAddress('mysql_affected_rows');
//  @mysql_autocommit             := GetAddress('mysql_autocommit');
  @mysql_change_user            := GetAddress('mysql_change_user');
  @mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @mysql_close                  := GetAddress('mysql_close');
//  @mysql_commit                 := GetAddress('mysql_commit');
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
  @mysql_field_count            := GetAddress('mysql_field_count');
  @mysql_field_seek             := GetAddress('mysql_field_seek');
  @mysql_field_tell             := GetAddress('mysql_field_tell');
  @mysql_free_result            := GetAddress('mysql_free_result');
//  @mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
  @mysql_get_client_info        := GetAddress('mysql_get_client_info');
  @mysql_get_client_version     := GetAddress('mysql_get_client_version');
  @mysql_get_host_info          := GetAddress('mysql_get_host_info');
  @mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @mysql_get_server_info        := GetAddress('mysql_get_server_info');
//  @mysql_get_server_version     := GetAddress('mysql_get_server_version');
//  @mysql_hex_string             := GetAddress('mysql_hex_string');
  @mysql_info                   := GetAddress('mysql_info');
  @mysql_init                   := GetAddress('mysql_init');
  @mysql_insert_id              := GetAddress('mysql_insert_id');
  @mysql_kill                   := GetAddress('mysql_kill');
  @mysql_list_dbs               := GetAddress('mysql_list_dbs');
  @mysql_list_fields            := GetAddress('mysql_list_fields');
  @mysql_list_processes         := GetAddress('mysql_list_processes');
  @mysql_list_tables            := GetAddress('mysql_list_tables');
//  @mysql_more_results           := GetAddress('mysql_more_results');
//  @mysql_next_result            := GetAddress('mysql_next_result');
  @mysql_num_fields             := GetAddress('mysql_num_fields');
  @mysql_num_rows               := GetAddress('mysql_num_rows');
  @mysql_options                := GetAddress('mysql_options');
  @mysql_ping                   := GetAddress('mysql_ping');
  @mysql_query                  := GetAddress('mysql_query');
  @mysql_real_connect           := GetAddress('mysql_real_connect');
  @mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @mysql_real_query             := GetAddress('mysql_real_query');
  @mysql_refresh                := GetAddress('mysql_refresh');
//  @mysql_rollback               := GetAddress('mysql_rollback');
  @mysql_row_seek               := GetAddress('mysql_row_seek');
  @mysql_row_tell               := GetAddress('mysql_row_tell');
  @mysql_select_db              := GetAddress('mysql_select_db');
//  @mysql_set_character_set      := GetAddress('mysql_set_character_set');
//  @mysql_set_server_option      := GetAddress('mysql_set_server_option');
//  @mysql_shutdown               := GetAddress('mysql_shutdown');
//  @mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @mysql_ssl_set                := GetAddress('mysql_ssl_set');
  @mysql_stat                   := GetAddress('mysql_stat');
  @mysql_store_result           := GetAddress('mysql_store_result');
  @mysql_thread_id              := GetAddress('mysql_thread_id');
  @mysql_use_result             := GetAddress('mysql_use_result');
//  @mysql_warning_count          := GetAddress('mysql_warning_count');

  @my_init                      := GetAddress('my_init');
  @mysql_thread_init            := GetAddress('mysql_thread_init');
  @mysql_thread_end             := GetAddress('mysql_thread_end');
  @mysql_thread_safe            := GetAddress('mysql_thread_safe');

  @mysql_server_init            := GetAddress('mysql_server_init');
  @mysql_server_end             := GetAddress('mysql_server_end');

//  @mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
//  @mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
//  @mysql_stmt_attr_set          := GetAddress('mysql_stmt_attr_set');
//  @mysql_stmt_bind_param        := GetAddress('mysql_stmt_bind_param');
//  @mysql_stmt_bind_result       := GetAddress('mysql_stmt_bind_result');
//  @mysql_stmt_close             := GetAddress('mysql_stmt_close');
//  @mysql_stmt_data_seek         := GetAddress('mysql_stmt_data_seek');
//  @mysql_stmt_errno             := GetAddress('mysql_stmt_errno');
//  @mysql_stmt_error             := GetAddress('mysql_stmt_error');
//  @mysql_stmt_execute           := GetAddress('mysql_stmt_execute');
//  @mysql_stmt_fetch             := GetAddress('mysql_stmt_fetch');
//  @mysql_stmt_fetch_column      := GetAddress('mysql_stmt_fetch_column');
//  @mysql_stmt_field_count       := GetAddress('mysql_stmt_field_count');
//  @mysql_stmt_free_result       := GetAddress('mysql_stmt_free_result');
//  @mysql_stmt_init              := GetAddress('mysql_stmt_init');
//  @mysql_stmt_insert_id         := GetAddress('mysql_stmt_insert_id');
//  @mysql_stmt_num_rows          := GetAddress('mysql_stmt_num_rows');
//  @mysql_stmt_param_count       := GetAddress('mysql_stmt_param_count');
//  @mysql_stmt_param_metadata    := GetAddress('mysql_stmt_param_metadata');
//  @mysql_stmt_prepare           := GetAddress('mysql_stmt_prepare');
//  @mysql_stmt_reset             := GetAddress('mysql_stmt_reset');
//  @mysql_stmt_result_metadata   := GetAddress('mysql_stmt_result_metadata');
//  @mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
//  @mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
//  @mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
//  @mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
//  @mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');




  {BELOW are functions not implemented in PDO}
  @mysql_send_query      := GetAddress('mysql_send_query');
  @mysql_read_query_result := GetAddress('mysql_read_query_result');
  @mysql_master_query    := GetAddress('mysql_master_query');
  @mysql_master_send_query := GetAddress('mysql_master_send_query');
  @mysql_slave_query     := GetAddress('mysql_slave_query');
  @mysql_slave_send_query := GetAddress('mysql_slave_send_query');
  @mysql_enable_rpl_parse := GetAddress('mysql_enable_rpl_parse');
  @mysql_disable_rpl_parse := GetAddress('mysql_disable_rpl_parse');
  @mysql_rpl_parse_enabled := GetAddress('mysql_rpl_parse_enabled');
  @mysql_enable_reads_from_master := GetAddress('mysql_enable_reads_from_master');
  @mysql_disable_reads_from_master := GetAddress('mysql_disable_reads_from_master');
  @mysql_reads_from_master_enabled := GetAddress('mysql_reads_from_master_enabled');
  @mysql_rpl_query_type  := GetAddress('mysql_rpl_query_type');
  @mysql_rpl_probe       := GetAddress('mysql_rpl_probe');
  @mysql_set_master      := GetAddress('mysql_set_master');
  @mysql_add_slave       := GetAddress('mysql_add_slave');
  @mysql_manager_init    := GetAddress('mysql_manager_init');
  @mysql_manager_connect := GetAddress('mysql_manager_connect');
  @mysql_manager_close   := GetAddress('mysql_manager_close');
  @mysql_manager_command := GetAddress('mysql_manager_command');
  @mysql_manager_fetch_line := GetAddress('mysql_manager_fetch_line');
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION
{$ENDIF}
    ]);
{$ELSE}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.
