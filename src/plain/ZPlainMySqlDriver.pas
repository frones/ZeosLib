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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZPlainDriver, ZCompatibility, ZPlainMysqlConstants,
     ZPlainMySql41, ZPlainMySql5;

const

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
  FIELD_TYPE_VARCHAR   = 15; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_BIT       = 16; //<--ADDED by fduenas 20-06-2006
  FIELD_TYPE_NEWDECIMAL = 246; //<--ADDED by fduenas 20-06-2006
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

{ Client Connection Options }
  _CLIENT_LONG_PASSWORD	  = 1;	  { new more secure passwords }
  _CLIENT_FOUND_ROWS	  = 2;	  { Found instead of affected rows }
  _CLIENT_LONG_FLAG	  = 4;	  { Get all column flags }
  _CLIENT_CONNECT_WITH_DB = 8;	  { One can specify db on connect }
  _CLIENT_NO_SCHEMA	  = 16;	  { Don't allow database.table.column }
  _CLIENT_COMPRESS	  = 32;	  { Can use compression protcol }
  _CLIENT_ODBC		  = 64;	  { Odbc client }
  _CLIENT_LOCAL_FILES	  = 128;  { Can use LOAD DATA LOCAL }
  _CLIENT_IGNORE_SPACE	  = 256;  { Ignore spaces before '(' }
  _CLIENT_CHANGE_USER     = 512;  { Support the mysql_change_user() }
  _CLIENT_INTERACTIVE     = 1024; { This is an interactive client }
  _CLIENT_SSL             = 2048; { Switch to SSL after handshake }
  _CLIENT_IGNORE_SIGPIPE  = 4096; { IGNORE sigpipes }
  _CLIENT_TRANSACTIONS    = 8196; { Client knows about transactions }
  _CLIENT_RESERVED        = 16384; { Old flag for 4.1 protocol  }
  _CLIENT_SECURE_CONNECTION = 32768; { New 4.1 authentication }
  _CLIENT_MULTI_STATEMENTS = 65536; { Enable/disable multi-stmt support }
  _CLIENT_MULTI_RESULTS   = 131072; { Enable/disable multi-results }
  _CLIENT_REMEMBER_OPTIONS = 2147483648; {Enable/disable multi-results }

type
  PZMySQLConnect = Pointer;
  PZMySQLResult = Pointer;
  PZMySQLRow = Pointer;
  PZMySQLField = Pointer;
  PZMySQLRowOffset = Pointer;

  TZMySQLOption = (
    MYSQL_OPT_CONNECT_TIMEOUT,
    MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,
    MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,
    MYSQL_SET_CHARSET_NAME
  );

  TZMySQLStatus = (
    MYSQL_STATUS_READY,
    MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT
  );

  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    function GetDescription: string;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(Handle: PZMySQLConnect): Integer;
    {END ADDED by fduenas 15-06-2006}
    procedure Despose(var Handle: PZMySQLConnect);

    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    // char_set_name
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
    function GetEscapeString(StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    function FetchField(Res: PZMySQLResult): PZMySQLField;
    // fetch_field_direct
    // fetch_fields
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    // field_tell
    procedure FreeResult(Res: PZMySQLResult);
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
    // info
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function GetListDatabases(Handle: PZMySQLConnect; Wild: PAnsiChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect; const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect; const Wild: PAnsiChar): PZMySQLResult;
    // num_fields
    function GetNumRows(Res: PZMySQLResult): Int64;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption; const Arg: PAnsiChar): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar; Length: Integer): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // row_tell
    function SelectDatabase(Handle: PZMySQLConnect; const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;

    // my_init
    // thread_init
    // thread_end
    // thread_safe

    // server_init
    // server_end

    // change_user
    // field_count
    // function GetClientVersion: AnsiString;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!

    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    //function GetServerVersion (Handle: PZMySQLConnect): AnsiString;
    // hex_string
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    // set_character_set
    // set_server_option
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;
    // warning_count

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    // stmt_attr_get
    // stmt_attr_set
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    procedure SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    // stmt_fetch_column
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    // stmt_free_result
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    // stmt_param_metadata
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    // stmt_reset
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // stmt_row_tell
    // stmt_send_long_data
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PAnsiChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    // get_character_set_info

    {non API functions}
    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PAnsiChar;
    function GetFieldTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure BuildArguments(Options: TStrings);
  end;

  {** Implements a driver for MySQL 4.1 }
  TZMySQL41PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql41.mysql41_API;
  public
    constructor Create;

    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual;

    procedure Debug(Debug: PAnsiChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PAnsiChar; Port: Cardinal;
      UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    procedure SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PAnsiChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PAnsiChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;

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
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PAnsiChar;
    function GetFieldTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure BuildArguments(Options: TStrings); virtual;
  end;

  {** Implements a driver for MySQL 4.1 }
  TZMySQLD41PlainDriver = class (TZMySQL41PlainDriver)
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; override;
    procedure BuildArguments(Options: TStrings); override;
  end;

  TZMySQL5PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZMySQLPlainDriver)
  protected
    MYSQL_API : ZPlainMysql5.mysql5_API;
  public
    constructor Create;

    function GetProtocol: string; virtual;
    function GetDescription: string; virtual;
    procedure Initialize; virtual;

    procedure Debug(Debug: PAnsiChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PAnsiChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; virtual;
    function GetLastInsertID (Handle: PZMySQLConnect): Int64;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PAnsiChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PAnsiChar; Port: Cardinal;
      UnixSocket: PAnsiChar; ClientFlag: Cardinal): PZMySQLConnect;
    function GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PAnsiChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PAnsiChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function SslSet(Handle: PZMySQLConnect; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PAnsiChar): Integer;

    function Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (Handle: PZMySQLConnect; mode: Boolean): Boolean;
    function Commit (Handle: PZMySQLConnect): Boolean;
    function CheckAnotherRowset   (Handle: PZMySQLConnect): Boolean;
    function RetrieveNextRowset   (Handle: PZMySQLConnect): Integer;
    function Rollback (Handle: PZMySQLConnect): Boolean;
    function GetSQLState (Handle: PZMySQLConnect): AnsiString;

    function GetPreparedAffectedRows (Handle: PZMySqlPrepStmt): Int64;
    function BindParameters (Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
    function BindResult (Handle: PZMySqlPrepStmt;  bindArray: PZMysqlBindArray): Byte;
    function ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
    procedure SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
    function GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt): Integer;
    function GetLastPreparedError(Handle: PZMySqlPrepStmt): AnsiString;
    function ExecuteStmt (Handle: PZMySqlPrepStmt): Integer;
    function FetchBoundResults (Handle: PZMySqlPrepStmt): Integer;
    function GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
    function InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
    function GetPreparedInsertID (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedNumRows (Handle: PZMySqlPrepStmt): Int64;
    function GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal; // param_count
    function PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
    function GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
    function SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function GetPreparedSQLState (Handle: PZMySqlPrepStmt): PAnsiChar;
    function StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;

    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PAnsiChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PAnsiChar; Length: Cardinal): Cardinal;

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
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;

    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PAnsiChar;
    function GetFieldTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
    function GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PAnsiChar;
    procedure BuildArguments(Options: TStrings); virtual;
  end;

  TZMySQLD5PlainDriver = class (TZMySQL5PlainDriver)
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize; override;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect; override;
    procedure BuildArguments(Options: TStrings); override;
  end;

implementation
uses SysUtils, ZSysUtils;

var
  ServerArgs: array of PAnsiChar;
  ServerArgsLen: Integer;

procedure BuildServerArguments(Options: TStrings);
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
{$IFDEF VER140}
        TmpList.Add(Options.Values[Options.Names[i]]);
{$ELSE}
        TmpList.Add(Options.ValueFromIndex[i]);
{$ENDIF}
    //Check if DataDir is specified, if not, then add it to the Arguments List
    if TmpList.Values['--datadir'] = '' then
       TmpList.Add('--datadir='+EMBEDDED_DEFAULT_DATA_DIR);
    ServerArgsLen := TmpList.Count;
    SetLength(ServerArgs, ServerArgsLen);
    for i := 0 to ServerArgsLen - 1 do
      {$IFDEF DELPHI12_UP}
      ServerArgs[i] := StrNew(PAnsiChar(UTF8String(TmpList[i])));
      {$ELSE}
      ServerArgs[i] := StrNew(PAnsiChar(TmpList[i]));
      {$ENDIF}
  finally
    TmpList.Free;
  end;
end;

{ TZMySQL41PlainDriver }

constructor TZMySQL41PlainDriver.Create;
begin
end;

function TZMySQL41PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.1';
end;

function TZMySQL41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

procedure TZMySQL41PlainDriver.Initialize;
begin
  ZPlainMySql41.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql41.LibraryLoader.api_rec;
end;

procedure TZMySQL41PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL41PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PAnsiChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL41PlainDriver.SslSet(Handle: PZMySQLConnect;
  const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_ssl_set(Handle, Key, Cert, Ca, Capath, Cipher);
end;

function TZMySQL41PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL41PlainDriver.Debug(Debug: PAnsiChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL41PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL41PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL41PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL41PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL41PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL41PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL41PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL41PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

function TZMySQL41PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL41PlainDriver.GetClientInfo: PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL41PlainDriver.GetEscapeString(StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_escape_string(StrTo, StrFrom, Length);
end;

function TZMySQL41PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL41PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL41PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL41PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
end;

function TZMySQL41PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL41PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL41PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL41PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL41PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

function TZMySQL41PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql41.PMYSQL(Handle));
end;

procedure TZMySQL41PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  Handle := nil;
end;

function TZMySQL41PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL41PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL41PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL41PlainDriver.GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL41PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL41PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL41PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL41PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql41.TMySqlOption(Option), Arg);
end;

function TZMySQL41PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQL41PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    my_bool, my_mode: Byte;
begin
    if (mode = True) then
        my_mode := 1
    else
        my_mode := 0;
    my_bool := MYSQL_API.mysql_autocommit(ZPlainMySql41.PMYSQL(Handle), my_mode);
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_commit(ZPlainMySql41.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool :=  MYSQL_API.mysql_more_results (ZPlainMySql41.PMYSQL(Handle));
    if (my_bool = 0) then
        Result := False
    else
        Result := True;
end;

function TZMySQL41PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
begin
    Result := MYSQL_API.mysql_next_result (ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_rollback(ZPlainMySql41.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL41PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Result := MYSQL_API.mysql_sqlstate (ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
begin
    Result :=  MYSQL_API.mysql_stmt_affected_rows (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_param (ZPlainMySql41.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
end;

function TZMySQL41PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_result (ZPlainMySql41.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
end;

function TZMySQL41PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_stmt_close(ZPlainMySql41.PMYSQL_STMT(PrepStmtHandle));
    if (my_bool = 0) then
        Result := nil
    else
        Result := PrepStmtHandle;
end;

procedure TZMySQL41PlainDriver.SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
begin
  MYSQL_API.mysql_stmt_data_seek(ZPlainMySql41.PMYSQL_STMT(PrepStmtHandle), Offset);
end;

function TZMySQL41PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
begin
    Result := MYSQL_API.mysql_stmt_errno(ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := MYSQL_API.mysql_stmt_error(ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_execute (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_fetch (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_field_count(ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := MYSQL_API.mysql_stmt_init(ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
begin
    Result := MYSQL_API.mysql_stmt_insert_id (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_stmt_num_rows (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
begin
    Result := MYSQL_API.mysql_stmt_param_count (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
begin
    Result := MYSQL_API.mysql_stmt_prepare(ZPlainMySql41.PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQL41PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
begin
    Result := MYSQL_API.mysql_stmt_result_metadata (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
    Result := MYSQL_API.mysql_stmt_row_seek (ZPlainMySql41.PMYSQL_STMT(Handle), Row);
end;

function TZMySQL41PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PAnsiChar;
begin
    Result := MYSQL_API.mysql_stmt_sqlstate (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_store_result (ZPlainMySql41.PMYSQL_STMT(Handle));
end;

function TZMySQL41PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL41PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL41PlainDriver.GetLastError(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_error(Handle);
end;

function TZMySQL41PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL41PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL41PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := MYSQL_API.mysql_num_rows(Res);
end;

function TZMySQL41PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL41PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := MYSQL_API.mysql_num_fields(Res);
end;

function TZMySQL41PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL41PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL41PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL41PlainDriver.GetFieldName(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL41PlainDriver.GetFieldTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL41PlainDriver.GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.org_table;
end;

function TZMySQL41PlainDriver.GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.org_name;
end;

function TZMySQL41PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PAnsiChar;
begin
  Result := ZPlainMySql41.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL41PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_errno(ZPlainMySql41.PMYSQL(Handle));
end;

function TZMySQL41PlainDriver.GetClientVersion: Integer;
begin
 Result := MYSQL_API.mysql_get_client_version;
end;

function TZMySQL41PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := MYSQL_API.mysql_get_server_version(Handle);
end;

procedure TZMySQL41PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQLD41PlainDriver }

function TZMySQLD41PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.1';
end;

function TZMySQLD41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.1+';
end;

function TZMySQLD41PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
//    MYSQL_API.mysql_server_init(3, @DEFAULT_PARAMS, @SERVER_GROUPS);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQLD41PlainDriver.Initialize;
begin
  ZPlainMySql41.LibraryLoaderEmbedded.LoadIfNeeded;
  MYSQL_API := ZPlainMySql41.LibraryLoaderEmbedded.api_rec;
end;

procedure TZMySQLD41PlainDriver.BuildArguments(Options: TStrings);
begin
  BuildServerArguments(Options);
end;

{ TZMySQL5PlainDriver }

constructor TZMySQL5PlainDriver.Create;
begin
end;

function TZMySQL5PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-5';
end;

function TZMySQL5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 5.0+';
end;

procedure TZMySQL5PlainDriver.Initialize;
begin
  ZPlainMySql5.LibraryLoader.LoadIfNeeded;
  MYSQL_API := ZPlainMySql5.LibraryLoader.api_rec;
end;

procedure TZMySQL5PlainDriver.Close(Handle: PZMySQLConnect);
begin
  MYSQL_API.mysql_close(Handle);
end;

function TZMySQL5PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PAnsiChar): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL5PlainDriver.SslSet(Handle: PZMySQLConnect;
  Const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_ssl_set(Handle, Key, Cert, Ca, Capath, Cipher);
end;

function TZMySQL5PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_create_db(Handle, Database);
end;

procedure TZMySQL5PlainDriver.Debug(Debug: PAnsiChar);
begin
  MYSQL_API.mysql_debug(Debug);
end;

function TZMySQL5PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_drop_db(Handle, Database);
end;

function TZMySQL5PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_dump_debug_info(Handle);
end;

function TZMySQL5PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_query(Handle, Query);
end;

function TZMySQL5PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PAnsiChar; Length: Integer): Integer;
begin
  Result := MYSQL_API.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL5PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := MYSQL_API.mysql_fetch_field(Res);
end;

function TZMySQL5PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := MYSQL_API.mysql_fetch_lengths(Res);
end;

function TZMySQL5PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := MYSQL_API.mysql_fetch_row(Res);
end;

procedure TZMySQL5PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  MYSQL_API.mysql_free_result(Res);
end;

function TZMySQL5PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := MYSQL_API.mysql_affected_rows(Handle);
end;

function TZMySQL5PlainDriver.GetClientInfo: PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_client_info;
end;

function TZMySQL5PlainDriver.GetEscapeString(StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_escape_string(StrTo, StrFrom, Length);
end;

function TZMySQL5PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_host_info(Handle);
end;

function TZMySQL5PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL5PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_processes(Handle);
end;

function TZMySQL5PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PAnsiChar): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_list_tables(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_num_rows (Res);
end;

function TZMySQL5PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_get_proto_info(Handle);
end;

function TZMySQL5PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_get_server_info(Handle);
end;

function TZMySQL5PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_stat(Handle);
end;

function TZMySQL5PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := MYSQL_API.mysql_thread_id(Handle);
end;

function TZMySQL5PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(1, nil, nil);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

function TZMySQL5PlainDriver.getLastInsertID (Handle: PZMySQLConnect): Int64;
begin
    Result := MYSQL_API.mysql_insert_id(ZPlainMySql5.PMYSQL(Handle));
end;

procedure TZMySQL5PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
  Handle := nil;
end;

function TZMySQL5PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := MYSQL_API.mysql_kill(Handle, Pid);
end;

function TZMySQL5PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_ping(Handle);
end;

function TZMySQL5PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := MYSQL_API.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL5PlainDriver.GetRealEscapeString(Handle: PZMySQLConnect; StrTo, StrFrom: PAnsiChar;
  Length: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_real_escape_string(Handle, StrTo, StrFrom, Length);
end;

function TZMySQL5PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := MYSQL_API.mysql_refresh(Handle, Options);
end;

procedure TZMySQL5PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  MYSQL_API.mysql_data_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := MYSQL_API.mysql_field_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := MYSQL_API.mysql_row_seek(Res, Row);
end;

function TZMySQL5PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_select_db(Handle, Database);
end;

function TZMySQL5PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PAnsiChar): Integer;
begin
  Result := MYSQL_API.mysql_options(Handle,
    ZPlainMySql5.TMySqlOption(Option), Arg);
end;

function TZMySQL5PlainDriver.Shutdown(Handle: PZMySQLConnect; shutdown_level: TMysqlShutdownLevel = ZPlainMysqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := MYSQL_API.mysql_shutdown(Handle,shutdown_level);
end;

function TZMySQL5PlainDriver.SetAutocommit(Handle: PZMySQLConnect; mode: Boolean): Boolean;
var
    my_bool, my_mode: Byte;
begin
    if (mode = True) then
        my_mode := 1
    else
        my_mode := 0;
    my_bool := MYSQL_API.mysql_autocommit(ZPlainMySql5.PMYSQL(Handle), my_mode);
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.Commit(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_commit(ZPlainMySql5.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.CheckAnotherRowset(Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool :=  MYSQL_API.mysql_more_results (ZPlainMySql5.PMYSQL(Handle));
    if (my_bool = 0) then
        Result := False
    else
        Result := True;
end;

function TZMySQL5PlainDriver.RetrieveNextRowset(Handle: PZMySQLConnect): Integer;
begin
    Result := MYSQL_API.mysql_next_result (ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.Rollback (Handle: PZMySQLConnect): Boolean;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_rollback(ZPlainMySql5.PMYSQL(Handle));
    Result := (my_bool = 0);
end;

function TZMySQL5PlainDriver.getSQLState (Handle: PZMySQLConnect): AnsiString;
begin
    Result := MYSQL_API.mysql_sqlstate (ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedAffectedRows(Handle: PZMySqlPrepStmt): Int64;
begin
    Result :=  MYSQL_API.mysql_stmt_affected_rows (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.BindParameters(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_param (ZPlainMySql5.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
end;

function TZMySQL5PlainDriver.BindResult(Handle: PZMySqlPrepStmt; bindArray: PZMysqlBindArray): Byte;
begin
    Result := MYSQL_API.mysql_stmt_bind_result (ZPlainMySql5.PMYSQL_STMT(Handle), PMYSQL_BIND2(bindArray));
end;

function TZMySQL5PlainDriver.ClosePrepStmt (PrepStmtHandle: PZMySqlPrepStmt): PZMySqlPrepStmt;
var
    my_bool: Byte;
begin
    my_bool := MYSQL_API.mysql_stmt_close(ZPlainMySql5.PMYSQL_STMT(PrepStmtHandle));
    if (my_bool = 0) then
        Result := nil
    else
        Result := PrepStmtHandle;
end;

procedure TZMySQL5PlainDriver.SeekPreparedData(PrepStmtHandle: PZMySqlPrepStmt; Offset: Cardinal);
begin
  MYSQL_API.mysql_stmt_data_seek(ZPlainMySql5.PMYSQL_STMT(PrepStmtHandle), Offset);
end;

function TZMySQL5PlainDriver.GetLastPreparedErrorCode(Handle: PZMySqlPrepStmt):Integer;
begin
    Result := MYSQL_API.mysql_stmt_errno(ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.GetLastPreparedError(Handle: PZMySqlPrepStmt):AnsiString;
begin
    Result := MYSQL_API.mysql_stmt_error(ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.ExecuteStmt(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_execute (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.FetchBoundResults(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_fetch (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedFieldCount(Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_field_count(ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.InitializePrepStmt (Handle: PZMySQLConnect): PZMySqlPrepStmt;
begin
    Result := MYSQL_API.mysql_stmt_init(ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedInsertID(Handle: PZMySqlPrepStmt): Int64;
begin
    Result := MYSQL_API.mysql_stmt_insert_id (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedNumRows(Handle: PZMySqlPrepStmt): Int64;
begin
    if (Handle = nil) then
        Result := 0
    else
        Result :=  MYSQL_API.mysql_stmt_num_rows (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.GetPreparedBindMarkers (Handle: PZMySqlPrepStmt): Cardinal;
begin
    Result := MYSQL_API.mysql_stmt_param_count (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.PrepareStmt (PrepStmtHandle: PZMySqlPrepStmt; const Query: PAnsiChar; Length: Integer): Integer;
begin
    Result := MYSQL_API.mysql_stmt_prepare(ZPlainMySql5.PMYSQL_STMT(PrepStmtHandle), Query, Length);
end;

function TZMySQL5PlainDriver.GetPreparedMetaData (Handle: PZMySqlPrepStmt): PZMySQLResult;
begin
    Result := MYSQL_API.mysql_stmt_result_metadata (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.SeekPreparedRow(Handle: PZMySqlPrepStmt; Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
    Result := MYSQL_API.mysql_stmt_row_seek (ZPlainMySql5.PMYSQL_STMT(Handle), Row);
end;

function TZMySQL5PlainDriver.GetPreparedSQLState(Handle: PZMySqlPrepStmt): PAnsiChar;
begin
    Result := MYSQL_API.mysql_stmt_sqlstate (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.StorePreparedResult (Handle: PZMySqlPrepStmt): Integer;
begin
    Result := MYSQL_API.mysql_stmt_store_result (ZPlainMySql5.PMYSQL_STMT(Handle));
end;

function TZMySQL5PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_store_result(Handle);
end;

function TZMySQL5PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := MYSQL_API.mysql_use_result(Handle);
end;

function TZMySQL5PlainDriver.GetLastError(Handle: PZMySQLConnect): PAnsiChar;
begin
  Result := MYSQL_API.mysql_error(Handle);
end;

function TZMySQL5PlainDriver.GetFieldType(Field: PZMySQLField): Byte;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^._type;
end;

function TZMySQL5PlainDriver.GetFieldFlags(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.flags;
end;

function TZMySQL5PlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := MYSQL_API.mysql_num_rows(Res);
end;

function TZMySQL5PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := MYSQL_API.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

function TZMySQL5PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := MYSQL_API.mysql_num_fields(Res);
end;

function TZMySQL5PlainDriver.GetFieldDecimals(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.decimals;
end;

function TZMySQL5PlainDriver.GetFieldLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.length;
end;

function TZMySQL5PlainDriver.GetFieldMaxLength(Field: PZMySQLField): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.max_length;
end;

function TZMySQL5PlainDriver.GetFieldName(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL5PlainDriver.GetFieldTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL5PlainDriver.GetFieldOrigTable(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.org_table;
end;

function TZMySQL5PlainDriver.GetFieldOrigName(Field: PZMySQLField): PAnsiChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.org_name;
end;

function TZMySQL5PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PAnsiChar;
begin
  Result := ZPlainMySql5.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL5PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := MYSQL_API.mysql_errno(ZPlainMySql5.PMYSQL(Handle));
end;

function TZMySQL5PlainDriver.GetClientVersion: Integer;
begin
 Result := MYSQL_API.mysql_get_client_version;
end;

function TZMySQL5PlainDriver.GetServerVersion(
  Handle: PZMySQLConnect): Integer;
begin
 Result := MYSQL_API.mysql_get_server_version(Handle);
end;

procedure TZMySQL5PlainDriver.BuildArguments(Options: TStrings);
begin

end;

{ TZMySQLD5PlainDriver }

function TZMySQLD5PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-5';
end;

function TZMySQLD5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 5+';
end;

function TZMySQLD5PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
  if @MYSQL_API.mysql_server_init <> nil then
    MYSQL_API.mysql_server_init(ServerArgsLen, ServerArgs, @SERVER_GROUPS);
//    MYSQL_API.mysql_server_init(3, @DEFAULT_PARAMS, @SERVER_GROUPS);
  Handle := MYSQL_API.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQLD5PlainDriver.Initialize;
begin
  ZPlainMySql5.LibraryLoaderEmbedded.LoadIfNeeded;
  MYSQL_API := ZPlainMySql5.LibraryLoaderEmbedded.api_rec;
end;

procedure TZMySQLD5PlainDriver.BuildArguments(Options: TStrings);
begin
  BuildServerArguments(Options);
end;

end.


