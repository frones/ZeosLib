{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{               John Marino, www.synsport.com             }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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
  ZCompatibility, Types,
  ZPlainDriver;

//{$A-} //pack the records!   EH: nope this is wrong!
{$Z+} //enum to DWORD
const
{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';

  { Field's flags }
  NOT_NULL_FLAG          = 1;     { Field can't be NULL }
  PRI_KEY_FLAG           = 2;     { Field is part of a primary key }
  UNIQUE_KEY_FLAG        = 4;     { Field is part of a unique key }
  MULTIPLE_KEY_FLAG      = 8;     { Field is part of a key }
  BLOB_FLAG              = 16;    { Field is a blob }
  UNSIGNED_FLAG          = 32;    { Field is unsigned }
  ZEROFILL_FLAG          = 64;    { Field is zerofill }
  BINARY_FLAG            = 128;   { Field is binary }
  ENUM_FLAG              = 256;   { Field is an enum }
  AUTO_INCREMENT_FLAG    = 512;   { Field is a autoincrement field }
  TIMESTAMP_FLAG         = 1024;  { Field is a timestamp }
  SET_FLAG               = 2048;  { Field is a set }
  NUM_FLAG               = 32768; { Field is num (for clients) }
  PART_KEY_FLAG	         = 16384; { Intern; Part of some key }
  GROUP_FLAG	           = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = $20000; { Intern: Used by sql_yacc }
  GET_FIXED_FIELDS_FLAG  = $40000; { Used to get fields in item tree }
  FIELD_IN_PART_FUNC_FLAG= $80000; { Field part of partition func }
  FIELD_IN_ADD_INDEX     = $100000; { Intern: Field used in ADD INDEX }
  FIELD_IS_RENAMED       = $200000; { Intern: Field is being renamed}

{THD: Killable}
  MYSQL_SHUTDOWN_KILLABLE_CONNECT    = 1;
  MYSQL_SHUTDOWN_KILLABLE_TRANS      = 2;
  MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE = 4;
  MYSQL_SHUTDOWN_KILLABLE_UPDATE     = 8;

{prepared fetch results}
  STMT_FETCH_OK         = 0;
  STMT_FETCH_ERROR      = 1;
  STMT_FETCH_NO_DATA    = 100;
  STMT_FETCH_DATA_TRUNC = 101;

  {status codes}
const
   MYSQL_NO_DATA = 100;
   MYSQL_DATA_TRUNCATED  = 101;

{$MINENUMSIZE 4}
type
  TMySqlOption = (
    MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS, MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND, MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME, MYSQL_OPT_LOCAL_INFILE,
    MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH,
    MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT,
    MYSQL_OPT_SSL_VERIFY_SERVER_CERT, MYSQL_PLUGIN_DIR, MYSQL_DEFAULT_AUTH,
    MYSQL_OPT_BIND,
    MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT,
    MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER,
    MYSQL_OPT_SSL_CRL, MYSQL_OPT_SSL_CRLPATH,
    MYSQL_OPT_CONNECT_ATTR_RESET, MYSQL_OPT_CONNECT_ATTR_ADD,
    MYSQL_OPT_CONNECT_ATTR_DELETE,
    MYSQL_SERVER_PUBLIC_KEY,
    MYSQL_ENABLE_CLEARTEXT_PLUGIN,
    MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
    MYSQL_OPT_SSL_ENFORCE,
    MYSQL_OPT_MAX_ALLOWED_PACKET, MYSQL_OPT_NET_BUFFER_LENGTH,
    MYSQL_OPT_TLS_VERSION,
    MYSQL_OPT_SSL_MODE,
    {MySQL 8:}
    MYSQL_OPT_GET_SERVER_PUBLIC_KEY,
    MYSQL_OPT_RETRY_COUNT,
    MYSQL_OPT_OPTIONAL_RESULTSET_METADATA,
    MYSQL_OPT_SSL_FIPS_MODE,
    MYSQL_OPT_TLS_CIPHERSUITES,
    MYSQL_OPT_COMPRESSION_ALGORITHMS,
    MYSQL_OPT_ZSTD_COMPRESSION_LEVEL
  );
  TMariaDBOption = (
    { MariaDB specific }
    MYSQL_PROGRESS_CALLBACK=5999,
    MYSQL_OPT_NONBLOCK);
  TMariaDBConnectorOption = (
    { MariaDB Connector/C specific }
    MYSQL_DATABASE_DRIVER=7000,
    MARIADB_OPT_SSL_FP,             // deprecated, use MARIADB_OPT_TLS_PEER_FP instead
    MARIADB_OPT_SSL_FP_LIST,        // deprecated, use MARIADB_OPT_TLS_PEER_FP_LIST instead
    MARIADB_OPT_TLS_PASSPHRASE,     // passphrase for encrypted certificates
    MARIADB_OPT_TLS_CIPHER_STRENGTH,
    MARIADB_OPT_TLS_VERSION,
    MARIADB_OPT_TLS_PEER_FP,            // single finger print for server certificate verification
    MARIADB_OPT_TLS_PEER_FP_LIST,       // finger print white list for server certificate verification
    MARIADB_OPT_CONNECTION_READ_ONLY,
    MYSQL_OPT_CONNECT_ATTRS,        // for mysql_get_optionv
    MARIADB_OPT_USERDATA,
    MARIADB_OPT_CONNECTION_HANDLER,
    MARIADB_OPT_PORT,
    MARIADB_OPT_UNIXSOCKET,
    MARIADB_OPT_PASSWORD,
    MARIADB_OPT_HOST,
    MARIADB_OPT_USER,
    MARIADB_OPT_SCHEMA,
    MARIADB_OPT_DEBUG,
    MARIADB_OPT_FOUND_ROWS,
    MARIADB_OPT_MULTI_RESULTS,
    MARIADB_OPT_MULTI_STATEMENTS,
    MARIADB_OPT_INTERACTIVE,
    MARIADB_OPT_PROXY_HEADER,
    MARIADB_OPT_IO_WAIT
  );
const
  TMySqlOptionMinimumVersion: array[TMySqlOption] of Integer =
    (
      {MYSQL_OPT_CONNECT_TIMEOUT}               0,
      {MYSQL_OPT_COMPRESS}                      0,
      {MYSQL_OPT_NAMED_PIPE}                    0,
      {MYSQL_INIT_COMMAND}                      0,
      {MYSQL_READ_DEFAULT_FILE}                 0,
      {MYSQL_READ_DEFAULT_GROUP}                0,
      {MYSQL_SET_CHARSET_DIR}                   0,
      {MYSQL_SET_CHARSET_NAME}                  0,
      {MYSQL_OPT_LOCAL_INFILE}                  0,
      {MYSQL_OPT_PROTOCOL}                      40100,
      {MYSQL_SHARED_MEMORY_BASE_NAME}           40100,
      {MYSQL_OPT_READ_TIMEOUT}                  40101,
      {MYSQL_OPT_WRITE_TIMEOUT}                 40101,
      {MYSQL_OPT_USE_RESULT}                    40101,
      {MYSQL_OPT_USE_REMOTE_CONNECTION}         40101,
      {MYSQL_OPT_USE_EMBEDDED_CONNECTION}       40101,
      {MYSQL_OPT_GUESS_CONNECTION}              40101,
      {MYSQL_SET_CLIENT_IP}                     40101,
      {MYSQL_SECURE_AUTH}                       40101,
      {MYSQL_REPORT_DATA_TRUNCATION}            40101,
      {MYSQL_OPT_RECONNECT}                     50013,
      {MYSQL_OPT_SSL_VERIFY_SERVER_CERT}        50023,
      {MYSQL_PLUGIN_DIR}                        50507,
      {MYSQL_DEFAULT_AUTH}                      50507,
      {MYSQL_OPT_BIND}                          50601,
      {MYSQL_OPT_SSL_KEY}                       50603,
      {MYSQL_OPT_SSL_CERT}                      50603,
      {MYSQL_OPT_SSL_CA}                        50603,
      {MYSQL_OPT_SSL_CAPATH}                    50603,
      {MYSQL_OPT_SSL_CIPHER}                    50603,
      {MYSQL_OPT_SSL_CRL}                       50603,
      {MYSQL_OPT_SSL_CRLPATH}                   50603,
      {MYSQL_OPT_CONNECT_ATTR_RESET}            50606,
      {MYSQL_OPT_CONNECT_ATTR_ADD}              50606,
      {MYSQL_OPT_CONNECT_ATTR_DELETE}           50606,
      {MYSQL_SERVER_PUBLIC_KEY}                 50606,
      {MYSQL_ENABLE_CLEARTEXT_PLUGIN}           50607,
      {MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS}  50610,
      {MYSQL_OPT_SSL_ENFORCE}                   50703,
      {MYSQL_OPT_MAX_ALLOWED_PACKET}            50703,
      {MYSQL_OPT_NET_BUFFER_LENGTH}             50703,
      {MYSQL_OPT_SSL_MODE}                      50703,
      {MYSQL_OPT_TLS_VERSION}                   60111,
      {MYSQL_OPT_GET_SERVER_PUBLIC_KEY}         60111,
      {MYSQL_OPT_RETRY_COUNT}                   60111,
      {MYSQL_OPT_OPTIONAL_RESULTSET_METADATA}   60111,
      {MYSQL_OPT_SSL_FIPS_MODE}                 60111,
      {MYSQL_OPT_TLS_CIPHERSUITES}              60111,
      {MYSQL_OPT_COMPRESSION_ALGORITHMS}        60111,
      {MYSQL_OPT_ZSTD_COMPRESSION_LEVEL}        60111
    );

  STMT_INDICATOR_NTS=-1;      //String is null terminated
  STMT_INDICATOR_NONE=0;      //No semantics
  STMT_INDICATOR_NULL=1;      //NULL value
  STMT_INDICATOR_DEFAULT=2;   //Use columns default value
  STMT_INDICATOR_IGNORE=3;    //Skip update of column
  STMT_INDICATOR_IGNORE_ROW=4; //Skip update of row

type
  Tmysql_protocol_type = ( MYSQL_PROTOCOL_DEFAULT, MYSQL_PROTOCOL_TCP, MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE, MYSQL_PROTOCOL_MEMORY);
  // EgonHugeist: Use always a 4Byte unsigned Integer for Windows otherwise MySQL64 has problems on Win64!
  // don't know anything about reported issues on other OS's
  ULong                 = {$IFDEF MSWINDOWS}LongWord{$ELSE}NativeUInt{$ENDIF};
  ULongLong             = UInt64;
  PULong                = ^ULong;
  PULongLong            = ^ULongLong;

  Pmy_bool = ^my_bool;
  my_bool = byte;

  PUSED_MEM=^USED_MEM;
  USED_MEM = record
    next:       PUSED_MEM;
    left:       Integer;
    size:       Integer;
  end;

  PERR_PROC = ^ERR_PROC;
  ERR_PROC = procedure;

  PMEM_ROOT = ^MEM_ROOT;
  MEM_ROOT = record
    free:          PUSED_MEM;
    used:          PUSED_MEM;
    pre_alloc:     PUSED_MEM;
    min_malloc:    Integer;
    block_size:    Integer;
    block_num:     Integer;
    first_block_usage: Integer;
    error_handler: PERR_PROC;
  end;

  MYSQL_ROW = array[00..$ff] of PAnsiChar;
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

  MYSQL_FIELD_OFFSET = UInt;

  PMYSQL_OPTIONS   = ^TMYSQL_OPTIONS;
  TMYSQL_OPTIONS = record
    connect_timeout:          UInt;
    read_timeout:             UInt;
    write_timeout:            UInt;
    port:                     UInt;
    protocol:                 UInt;
    client_flag:              ULong;
    host:                     PAnsiChar;
    user:                     PAnsiChar;
    password:                 PAnsiChar;
    unix_socket:              PAnsiChar;
    db:                       PAnsiChar;
    init_commands:            Pointer;
    my_cnf_file:              PAnsiChar;
    my_cnf_group:             PAnsiChar;
    charset_dir:              PAnsiChar;
    charset_name:             PAnsiChar;
    ssl_key:                  PAnsiChar;
    ssl_cert:                 PAnsiChar;
    ssl_ca:                   PAnsiChar;
    ssl_capath:               PAnsiChar;
    ssl_cipher:               PAnsiChar;
    shared_memory_base_name:  PAnsiChar;
    max_allowed_packet:       ULong;
    use_ssl:                  Byte;
    compress:                 Byte;
    named_pipe:               Byte;
    unused1:                  Byte;
    unused2:                  Byte;
    unused3:                  Byte;
    unused4:                  Byte;
    methods_to_use:           TMySqlOption;
    client_ip:                PAnsiChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;

  PZMySQLResult = Pointer;
  PZMySQLRow = Pointer;
  PZMySQLRowOffset = Pointer;
  PZMysqlBindArray = Pointer;

{ Enum Field Types from binary_log_types.h }
  PMysqlFieldType = ^TMysqlFieldType;
  TMysqlFieldType = (
    FIELD_TYPE_DECIMAL   = 0,
    FIELD_TYPE_TINY      = 1,
    FIELD_TYPE_SHORT     = 2,
    FIELD_TYPE_LONG      = 3,
    FIELD_TYPE_FLOAT     = 4,
    FIELD_TYPE_DOUBLE    = 5,
    FIELD_TYPE_NULL      = 6,
    FIELD_TYPE_TIMESTAMP = 7,
    FIELD_TYPE_LONGLONG  = 8,
    FIELD_TYPE_INT24     = 9,
    FIELD_TYPE_DATE      = 10,
    FIELD_TYPE_TIME      = 11,
    FIELD_TYPE_DATETIME  = 12,
    FIELD_TYPE_YEAR      = 13,
    FIELD_TYPE_NEWDATE   = 14,
    FIELD_TYPE_VARCHAR   = 15, //<--ADDED by fduenas 20-06-2006
    FIELD_TYPE_BIT       = 16, //<--ADDED by fduenas 20-06-2006
    MYSQL_TYPE_JSON      = 245,
    FIELD_TYPE_NEWDECIMAL = 246, //<--ADDED by fduenas 20-06-2006
    FIELD_TYPE_ENUM      = 247,
    FIELD_TYPE_SET       = 248,
    FIELD_TYPE_TINY_BLOB = 249,
    FIELD_TYPE_MEDIUM_BLOB = 250,
    FIELD_TYPE_LONG_BLOB = 251,
    FIELD_TYPE_BLOB      = 252,
    FIELD_TYPE_VAR_STRING = 253,
    FIELD_TYPE_STRING    = 254,
    FIELD_TYPE_GEOMETRY  = 255);

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );

  TMysqlStmtAttrType = (
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS,
    { mariadb 10.2.7up}
    STMT_ATTR_PREBIND_PARAMS=200,
    STMT_ATTR_ARRAY_SIZE,
    STMT_ATTR_ROW_SIZE
  );

  TMysqlShutdownLevel = (
    SHUTDOWN_DEFAULT = 0,
    SHUTDOWN_WAIT_CONNECTIONS = MYSQL_SHUTDOWN_KILLABLE_CONNECT,
    SHUTDOWN_WAIT_TRANSACTIONS = MYSQL_SHUTDOWN_KILLABLE_TRANS,
    SHUTDOWN_WAIT_UPDATES = MYSQL_SHUTDOWN_KILLABLE_UPDATE,
    SHUTDOWN_WAIT_ALL_BUFFERS = (MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1),
    SHUTDOWN_WAIT_CRITICAL_BUFFERS,
    KILL_QUERY = 254,
    KILL_CONNECTION = 255
  );

TMYSQL_CLIENT_OPTIONS =
( CLIENT_LONG_PASSWORD,	{  = 1;	   new more secure passwords }
  CLIENT_FOUND_ROWS ,	{	  = 2;	   Found instead of affected rows }
  CLIENT_LONG_FLAG	 ,	{ = 4;	   Get all column flags }
  CLIENT_CONNECT_WITH_DB ,	{ = 8;	   One can specify db on connect }
  CLIENT_NO_SCHEMA	 ,	{  = 16;	   Don't allow database.table.column }
  CLIENT_COMPRESS	 ,	{  = 32;	   Can use compression protcol }
  CLIENT_ODBC		 ,	{  = 64;	   Odbc client }
  CLIENT_LOCAL_FILES	  ,	{ = 128;   Can use LOAD DATA LOCAL }
  CLIENT_IGNORE_SPACE	 ,	{  = 256;   Ignore spaces before '(' }
  CLIENT_CHANGE_USER    ,	{  = 512;   Support the mysql_change_user() }
  CLIENT_INTERACTIVE    ,	{  = 1024;  This is an interactive client }
  CLIENT_SSL     ,	{         = 2048;  Switch to SSL after handshake }
  CLIENT_IGNORE_SIGPIPE  ,	{ = 4096;  IGNORE sigpipes }
  CLIENT_TRANSACTIONS    ,	{ = 8196;  Client knows about transactions }
  CLIENT_RESERVED     ,	{    = 16384;  Old flag for 4.1 protocol  }
  CLIENT_SECURE_CONNECTION  ,	{= 32768;  New 4.1 authentication }
  CLIENT_MULTI_STATEMENTS  ,	{= 65536;  Enable/disable multi-stmt support }
  CLIENT_MULTI_RESULTS  ,	{  = 131072;  Enable/disable multi-results }
  CLIENT_PS_MULTI_RESULTS,  {2^18 = 262144; Enable Multi-results in PS-protocol}
  CLIENT_PLUGIN_AUTH,{2^19 = 524288}
  CLIENT_OPT_20,  {2^20 = 1048576}
  CLIENT_OPT_21,   {2^21 = 2097152 }
  CLIENT_OPT_22,  {2^22 = 4194304}
  CLIENT_OPT_23,  {2^23 = 8388608 }
  CLIENT_OPT_24,   {2^24 = 16777216 }
  CLIENT_OPT_25,   {2^25 = 33554432}
  CLIENT_OPT_26,    {2^26 = 67108864}
  CLIENT_OPT_27,    {2^27 = 134217728}
  CLIENT_OPT_28,    {2^28 = 268435456}
  CLIENT_OPT_29,    {2^29 = 536870912}
  CLIENT_SSL_VERIFY_SERVER_CERT,    {2^30 = 1073741824}
  CLIENT_REMEMBER_OPTIONS	{ = 2147483648; Enable/disable multi-results });

  TMysqlStmtState = (
    MYSQL_STMT_INIT_DONE = 1,
    MYSQL_STMT_PREPARE_DONE,
    MYSQL_STMT_EXECUTE_DONE,
    MYSQL_STMT_FETCH_DONE
  );

  mysql_timestamp_type = (
    MYSQL_TIMESTAMP_NONE = -2,
    MYSQL_TIMESTAMP_ERROR = -1,
    MYSQL_TIMESTAMP_DATE = 0,
    MYSQL_TIMESTAMP_DATETIME = 1,
    MYSQL_TIMESTAMP_TIME = 2
  );

  TMYSQL_TIME = record
    year:                UInt;
    month:               UInt;
    day:                 UInt;
    hour:                UInt;
    minute:              UInt;
    second:              UInt;
    second_part:         ULong; {max microseconds}
    neg:                 my_bool;
    time_type:           mysql_timestamp_type;
  end;
  PMYSQL_TIME = ^TMYSQL_TIME;

  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  PMYSQL_FIELD = Pointer;

  PMYSQL_FIELD51 = ^TMYSQL_FIELD51;
  TMYSQL_FIELD51 = record
    name:             PAnsiChar;   // Name of column
    org_name:         PAnsiChar;   // Original column name, if an alias
    table:            PAnsiChar;   // Table of column if column was a field
    org_table:        PAnsiChar;   // Org table name if table was an alias
    db:               PAnsiChar;   // Database for table
    catalog:          PAnsiChar;   // Catalog for table
    def:              PAnsiChar;   // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    catalog_length:   UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType; // Type of field. Se mysql_com.h for types
    extension:        Pointer //added in 4.1
  end;

  PMYSQL_FIELD41 = ^MYSQL_FIELD41;
  MYSQL_FIELD41 = record
    name:             PAnsiChar; // Name of column
    org_name:         PAnsiChar; // Original column name, if an alias
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    catalog:          PAnsiChar; // Catalog for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    catalog_length:   UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType;     // Type of field. Se enum_field_types.
  end;
  PMYSQL_FIELD401 = ^MYSQL_FIELD401;
  MYSQL_FIELD401 = record
    name:             PAnsiChar; // Name of column
    org_name:         PAnsiChar; // Original column name, if an alias
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
  end;
  PMYSQL_FIELD40 = ^MYSQL_FIELD40;
  MYSQL_FIELD40 = record
    name:             PAnsiChar; // Name of column
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
  end;
  PMYSQL_FIELD32 = ^MYSQL_FIELD32;
  MYSQL_FIELD32 = record
    name:             PAnsiChar; // Name of column
    table:            PAnsiChar; // Table of column if column was a field
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
    length:           UInt; // Width of column
    max_length:       UInt; // Max width of selected set
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
  end;

  // offsets to used MYSQL_FIELDxx members.
  // a negative entry means the field does not exits in the record
  PMYSQL_FIELDOFFSETS = ^TMYSQL_FIELDOFFSETS;
  TMYSQL_FIELDOFFSETS = record
    name            : NativeUInt;
    name_length     : NativeInt;
    org_table       : NativeInt;
    org_table_length: NativeInt;
    org_name        : NativeInt;
    org_name_length : NativeInt;
    max_length      : NativeUInt;
    db              : NativeInt;
    db_length       : NativeInt;
    charsetnr       : NativeInt;
    _type           : NativeUInt;
    flags           : NativeUInt;
    length          : NativeUInt;
    decimals        : NativeUInt;
  end;

  PMYSQL_BIND041 = ^MYSQL_BIND041;
  MYSQL_BIND041 = record
    length: PLongWord;              // output length pointer
    is_null: Pmy_bool;              // Pointer to null indicators
    buffer: PByte;                  // buffer to get/put data
    buffer_type: TMysqlFieldType;  // buffer type
    buffer_length: LongWord;        // buffer length
    param_number: LongWord;         // For null count and error messages
    long_data_used: my_bool;        // If used with mysql_send_long_data
  end;

  PMYSQL_BIND411 = ^TMYSQL_BIND411;
  TMYSQL_BIND411 =  record
    // 4.1.22 definition
    length:           PULong;
    is_null:          Pmy_bool;
    buffer:           Pointer;
    buffer_type:      TMysqlFieldType;
    buffer_length:    ULong;
    //internal fields
    inter_buffer:     PByte;
    offset:           ULong;
    internal_length:  ULong;
    param_number:     UInt;
    pack_length:      UInt;
    is_unsigned:      my_bool;
    long_data_used:   my_bool;
    internal_is_null: my_bool;
    store_param_func: Pointer;
    fetch_result:     Pointer;
    skip_result:      Pointer;
  end;

  PMYSQL_BIND506 = ^TMYSQL_BIND506;
  TMYSQL_BIND506 =  record
    // 5.0.67 up definition
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             PByte;
    buffer_type:       TMysqlFieldType;
    buffer_length:     ULong;
    row_ptr:           PByte;
    offset:            ULong;
    length_value:      ULong;
    param_number:      ULong;
    pack_length:       ULong;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
  end;

  PMYSQL_BIND51 = ^TMYSQL_BIND51;
  TMYSQL_BIND51 =  record
    // 5.1.30 up and 6.x definition
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             Pmy_bool;
    row_ptr:           PByte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldType;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    extension:         Pointer;
  end;

  PMARIADB_BIND1027 = ^TMARIADB_BIND1027;
  TMARIADB_BIND1027 =  record
    // MariaDB 10.2.7 up
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             Pmy_bool;
    u:                 record
                          case Boolean of
                          False: (row_ptr: PByte);
                          True: (indicator: PShortInt);
                        end;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldType;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    extension:         Pointer;
  end;


  PULongArray = ^TULongArray;
  TULongArray = array[0..4095] of Ulong; //https://dev.mysql.com/doc/refman/8.0/en/column-count-limit.html

  Pmy_bool_array = ^Tmy_bool_array;
  Tmy_bool_array = array[0..High(Byte)] of my_bool; //just 4 debugging

  TIndicator = ShortInt;
  Pmysql_indicator_types = ^Tmysql_indicator_types;
  Tmysql_indicator_types = array[0..High(Byte)] of TIndicator;

  PPMYSQL = ^PMYSQL;
  PMYSQL  = pointer;

  PMY_CHARSET_INFO = ^MY_CHARSET_INFO;
  MY_CHARSET_INFO = record
    number:         UInt;
    state:          UInt;
    csname:         PAnsiChar;
    name:           PAnsiChar;
    comment:        PAnsiChar;
    dir:            PAnsiChar;
    mbminlen:       UInt;
    mbmaxlen:       UInt;
  end;
  // Structure of the MYSQL_RES record isn't used anymore.
  // Access to the fields should be done using library functions
  // Reason : the structure of these records tend to change now and then.
  PMYSQL_RES = Pointer;

  PREP_STMT_STATE=(
    MY_ST_UNKNOWN,
    MY_ST_PREPARE,
    MY_ST_EXECUTE);

  PPMYSQL_STMT = ^PMYSQL_STMT;
  PMYSQL_STMT = Pointer;

  //decimal.h
  Tdecimal_digit_t = LongInt;
  Pdecimal_digit_ts = ^Tdecimal_digit_ts;
//see https://docs.oracle.com/cd/E17952_01/mysql-5.0-en/precision-math-decimal-characteristics.html
  Tdecimal_digit_ts = array[Byte{max of old mysql before 5.0.3}] of Tdecimal_digit_t;
  // -> "Leading “+” and “0” characters are not stored."
  Pdecimal_t = ^Tdecimal_t;
  Tdecimal_t = record
    TruncPrecision{intg}: Integer; //is the number of *decimal* digits (NOT number of decimal_digit_t's !) before the point
    Scale{frac}: Integer; //is the number of decimal digits after the point
    Decimal_digit_ts{len}:  integer; //is the length of buf (length of allocated space) in decimal_digit_t's, not in bytes
    Negative{sign}: my_bool; //false means positive, true means negative
    buf:  Pdecimal_digit_ts;  //is an array of decimal_digit_t's
  end;

  /// <summary>
  ///   Enum for specifying a MySQL fork. Possible values:
  ///   fUnknown, fMySQL, fMariaDB, fSphinx, fPercona, fDrizzle, WebScaleSQL, OurDelta
  /// </summary>
  TMySQLFork = (fUnknown, fMySQL, fMariaDB, fSphinx, fPercona, fDrizzle, WebScaleSQL, OurDelta);

const
  MySQLForkName: array[TMySQLFork] of String = ('Unknown', 'MySQL', 'MariaDB',
    'Sphinx', 'Percona', 'Drizzle', 'WebScaleSQL', 'OurDelta');
  DIG_PER_DEC1 = 9;
  Powers10: array[0..DIG_PER_DEC1] of Tdecimal_digit_t = (
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
  EMBEDDED_DEFAULT_DATA_DIR = {$IFDEF WINDOWS}'.\data\'{$ELSE}'./data/'{$ENDIF};
  SERVER_ARGUMENTS_KEY_PREFIX = 'ServerArgument';
  SERVER_GROUPS : array [0..2] of PAnsiChar = ('embedded'#0, 'server'#0, nil);

  DEFAULT_PARAMS : array [0..2] of PAnsiChar = ('not_used'#0,
                                            '--datadir='+EMBEDDED_DEFAULT_DATA_DIR+#0,
                                            '--set-variable=key_buffer_size=32M'#0);

const
  MaxBlobSize = 1000000;
//some error codes:
  CR_SERVER_GONE_ERROR = 2006;
  CR_SERVER_LOST = 2013;
  CR_INVALID_PARAMETER_NO = 2034;
  CR_NO_DATA = 2051;
//the warnings (mysqld_error.h)
  WARN_DATA_TRUNCATED = 1265;
  WARN_NO_MASTER_INFO = 1617;
  WARN_OPTION_IGNORED = 1618;
  WARN_PLUGIN_BUSY = 1620;
  WARN_NON_ASCII_SEPARATOR_NOT_IMPLEMENTED = 1638;
  WARN_COND_ITEM_TRUNCATED = 1647;
  WARN_OPTION_BELOW_LIMIT = 1708;
  WARN_ON_BLOCKHOLE_IN_RBR = 1870;
  WARN_DEPRECATED_MAXDB_SQL_MODE_FOR_TIMESTAMP = 3226;

  //http://eclipseclp.org/doc/bips/lib/dbi/cursor_next_execute-3.html
  //"Only one active cursor of type no_cursor is allowed per session,
  //and this active cursor must be closed before another query can be issued to the DBMS server.
  //read_only cursor does not have this restriction,
  //and several such cursors can be active at the same time "

  CURSOR_TYPE_NO_CURSOR:  ULong = 0;
  CURSOR_TYPE_READ_ONLY:  ULong = 1;
  CURSOR_TYPE_FOR_UPDATE: ULong = 2;
  CURSOR_TYPE_SCROLLABLE: ULong = 4;

{** offet of MYSSQL.server_status field:
  The struct of the record tends to change to often and we don't need all the
  definitions

  Value := NativeUInt(@(MYSQLx(Nil).server_status
}

  MYSQL5up_server_status_offset: NativeUInt = {$IFDEF CPU64}852{$ELSE}748{$ENDIF};
  MYSQL41_server_status_offset: NativeUInt = {$IFDEF CPU64}540{$ELSE}436{$ENDIF};
  MYSQL323_server_status_offset: NativeUInt = {$IFDEF CPU64}396{$ELSE}328{$ENDIF};

  //mysql_com.h
  SERVER_PS_OUT_PARAMS = LongWord(4096); //To mark ResultSet containing output parameter values.
  SERVER_MORE_RESULTS_EXIST = LongWord(8); //Multi query - next query exists

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
  end;

  {** Implements a base driver for MySQL}

  { TZMySQLPlainDriver }

  TZMySQLPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZMySQLPlainDriver)
  public
    FIsMariaDBDriver: Boolean;
    fIsInitialized: Boolean;
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
    mysql_sqlstate:               function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
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
    mysql_thread_id:              function(mysql: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
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
    /// <summary>From MariaDB-Docs:
    ///  Immediately aborts a connection by making all subsequent
    ///  read/write operations fail. mariadb_cancel() does not invalidate memory
    ///  used for mysql structure, nor close any communication channels. To free
    ///  the memory, mysql_close() must be called. mariadb_cancel() is useful to
    ///  break long queries in situations where sending KILL is not possible.
    /// </summary>
    /// <param>"mysql" mysql handle, which was previously allocated
    ///  by mysql_init() or mysql_real_connect().</param>
    /// <returns>???</returns>
    mariadb_cancel: function(mysql: PMYSQL): integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    /// <summary>From MariaDB-Docs:
    ///  mariadb_reconnect() tries to reconnect to a server in case the
    ///  connection died due to timeout or other errors. It uses the same
    ///  credentials which were specified in mysql_real_connect().
    /// </summary>
    /// <param>"mysql" mysql handle, which was previously allocated
    ///  by mysql_init() or mysql_real_connect().</param>
    /// <returns>0 on success; an error, if the option MYSQL_OPT_RECONNECT
    ///  wasn't specified before.</returns>
    mariadb_reconnect: function(mysql: PMYSQL): my_bool;
    //see http://docwiki.embarcadero.com/RADStudio/Sydney/de/E2591_Nur_cdecl-Funktionen_d%C3%BCrfen_varargs_verwenden_(Delphi)
    //so we can't use it because a dynamic loading wouldn't be possilbe anymore
    //cdecl calling convention is always required to align the result in first place
    //mysql_optionsv: function(mysql: PMYSQL; Option: TMySqlOption; const Arg: Pointer): Integer; varargs; cdecl;
    //cirumvent that issue by defining multiple versions with multiple Args
    //on loading on loading we use always the same address
    mysql_optionsv_1: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1: Pointer): Integer; cdecl;
    mysql_optionsv_2: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2: Pointer): Integer; cdecl;
    mysql_optionsv_3: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3: Pointer): Integer; cdecl;
    mysql_optionsv_4: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4: Pointer): Integer; cdecl;
    mysql_optionsv_5: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5: Pointer): Integer; cdecl;
    mysql_optionsv_6: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: Pointer): Integer; cdecl;
    mysql_optionsv_7: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: Pointer): Integer; cdecl;
    mysql_optionsv_8: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: Pointer): Integer; cdecl;
    mysql_optionsv_9: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: Pointer): Integer; cdecl;
    mysql_optionsv_10: function(mysql: PMYSQL; Option: TMySqlOption; const Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: Pointer): Integer; cdecl;
  protected
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    destructor Destroy; override;

    function IsMariaDBDriver: Boolean;
    property IsInitialized: Boolean read fIsInitialized write fIsInitialized;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}

implementation

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses SysUtils, ZPlainLoader, ZEncoding, ZFastCode
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
var
  ClientInfo: PAnsiChar;
  L: LengthInt;
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
    @mysql_sqlstate               := GetAddress('mysql_sqlstate');
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
    //@mysql_sqlstate               := GetAddress('mysql_sqlstate');
    @mysql_warning_count          := GetAddress('mysql_warning_count');
    {API for PREPARED STATEMENTS}
    @mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
    @mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
    @mariadb_stmt_execute_direct  := GetAddress('mariadb_stmt_execute_direct');

    //http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-attr-set.html
    //http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-attr-set.html
    if Assigned(mariadb_stmt_execute_direct) or (mysql_get_client_version >= 50107)
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

    @mariadb_cancel               := GetAddress('mariadb_cancel');
    @mariadb_reconnect            := GetAddress('mariadb_reconnect');
    @mysql_optionsv_1             := GetAddress('mysql_optionsv');
    @mysql_optionsv_2             := @mysql_optionsv_1;
    @mysql_optionsv_3             := @mysql_optionsv_1;
    @mysql_optionsv_4             := @mysql_optionsv_1;
    @mysql_optionsv_5             := @mysql_optionsv_1;
    @mysql_optionsv_6             := @mysql_optionsv_1;
    @mysql_optionsv_7             := @mysql_optionsv_1;
    @mysql_optionsv_8             := @mysql_optionsv_1;
    @mysql_optionsv_9             := @mysql_optionsv_1;
    @mysql_optionsv_10            := @mysql_optionsv_1;
  end;
  ClientInfo := mysql_get_client_info;
  L := ZFastCode.StrLen(ClientInfo);
  FIsMariaDBDriver := Assigned(mariadb_stmt_execute_direct) or CompareMem(ClientInfo+L-7, PAnsiChar('MariaDB'), 7);
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

{$ENDIF ZEOS_DISABLE_MYSQL}

end.


