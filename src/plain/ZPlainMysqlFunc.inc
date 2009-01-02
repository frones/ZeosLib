{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

{$IFDEF MYSQL_5_API}
  {$DEFINE MYSQL_5_UP}
  {$DEFINE MYSQL_41_UP}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}
{$IFDEF MYSQL_41_API}
  {$DEFINE MYSQL_41_UP}
  {$DEFINE MYSQL_41_DOWN}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}

{ ************** Plain API Function types definition ************* }

  { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }
  Tmysql_affected_rows          = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_character_set_name     = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_close                  = procedure(Handle: PMYSQL);                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_connect                = function(Handle: PMYSQL; const Host, User, Passwd: PAnsiChar): PMYSQL;   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_create_db              = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_data_seek              = procedure(Result: PMYSQL_RES; Offset: Int64);                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_debug                  = procedure(Debug: PAnsiChar);                                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_drop_db                = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_dump_debug_info        = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_eof                    = function(Result: PMYSQL_RES): Byte;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_errno                  = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_error                  = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_escape_string          = function(PTo, PFrom: PAnsiChar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field            = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field_direct     = function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD;       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_fields           = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_lengths          = function(Result: PMYSQL_RES): PLongInt;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_row              = function(Result: PMYSQL_RES): PMYSQL_ROW;                            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_seek             = function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_tell             = function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_free_result            = procedure(Result: PMYSQL_RES);                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_client_info        = function: PAnsiChar;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_host_info          = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_proto_info         = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_info        = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_info                   = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_init                   = function(Handle: PMYSQL): PMYSQL;                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_insert_id              = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_kill                   = function(Handle: PMYSQL; Pid: LongInt): Integer;                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_dbs               = function(Handle: PMYSQL; Wild: PAnsiChar): PMYSQL_RES;                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_fields            = function(Handle: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_processes         = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_tables            = function(Handle: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES;             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_fields             = function(Result: PMYSQL_RES): Cardinal;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_rows               = function(Result: PMYSQL_RES): Int64;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_options                = function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ping                   = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_query                  = function(Handle: PMYSQL; const Query: PAnsiChar): Integer;               {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_connect           = function(Handle: PMYSQL; const Host, User, Passwd, Db: PAnsiChar;
                                           Port: Cardinal; const UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_escape_string     = function(Handle: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: Cardinal): Cardinal;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_query             = function(Handle: PMYSQL; const Query: PAnsiChar; Length: Cardinal): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_refresh                = function(Handle: PMYSQL; Options: Cardinal): Integer;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_seek               = function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_tell               = function(Result: PMYSQL_RES): PMYSQL_ROWS;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_select_db              = function(Handle: PMYSQL; const Db: PAnsiChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ssl_set                = function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:
                                  PAnsiChar): Byte;                                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stat                   = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_store_result           = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_id              = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_use_result             = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

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

  Tmysql_change_user            = function(mysql: PMYSQL; const user: PAnsiChar; const passwd: PAnsiChar; const db: PAnsiChar): Byte;
  Tmysql_field_count            = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  Tmysql_get_client_version     = function: Cardinal;                                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_send_query = function(mysql: PMYSQL; const query: PAnsiChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_read_query_result = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};


  Tmysql_autocommit             = function(Handle: PMYSQL; const mode: Byte): Byte;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_commit                 = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_version     = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_hex_string             = function(PTo, PFrom: PAnsiChar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_more_results           = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_next_result            = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_rollback               = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_character_set      = function(Handle: PMYSQL; csname: PAnsiChar): Integer;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_server_option      = function(Handle: PMYSQL; Option: TMysqlSetOption): Integer;          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_shutdown               = function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_sqlstate               = function(Handle: PMYSQL): PAnsiChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_warning_count          = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
{BELOW are new PREPARED STATEMENTS}
  Tmysql_stmt_affected_rows     = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_get          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  arg: PAnsiChar): Integer;                                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_set          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  const arg: PAnsiChar): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_param        = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_result       = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_close             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_data_seek         = procedure(stmt: PMYSQL_STMT; offset: Int64);                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_errno             = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_error             = function(stmt: PMYSQL_STMT): PAnsiChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_execute           = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch             = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch_column      = function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: Cardinal;
                                  offset: Cardinal): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_field_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_free_result       = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_init              = function(Handle: PMYSQL): PMYSQL_STMT;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_insert_id         = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_num_rows          = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_metadata    = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_prepare           = function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: Cardinal):
                                  Integer;                                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_reset             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_result_metadata   = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_seek          = function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS;     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_tell          = function(stmt: PMYSQL_STMT): PMYSQL_ROWS;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_send_long_data    = function(stmt: PMYSQL_STMT; parameter_number: Cardinal; const
                                  data: PAnsiChar; length: Cardinal): Byte;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_sqlstate          = function(stmt: PMYSQL_STMT): PAnsiChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_store_result      = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  {$IFDEF MYSQL_5_UP}
  Tmysql_get_character_set_info = procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO);                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

{****************  UNDEFINE EVERYTHING SET IN THIS INCLUDE *************}
  {$UNDEF MYSQL_5_UP}
  {$UNDEF MYSQL_5_DOWN}
  {$UNDEF MYSQL_41_UP}
  {$UNDEF MYSQL_41_DOWN}

