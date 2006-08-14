{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainDriver, ZCompatibility;

const
{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 200;
  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';
  NAME_LEN             = 64;
  PROTOCOL_VERSION     = 10;
  FRM_VER              = 6;

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
  GROUP_FLAG	         = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }

{ Server Administration Refresh Options }
  REFRESH_GRANT	         = 1;     { Refresh grant tables }
  REFRESH_LOG		 = 2;     { Start on new log file }
  REFRESH_TABLES	 = 4;     { close all tables }
  REFRESH_HOSTS	         = 8;     { Flush host cache }
  REFRESH_STATUS         = 16;    { Flush status variables }
  REFRESH_THREADS        = 32;    { Flush status variables }
  REFRESH_SLAVE          = 64;    { Reset master info abd restat slave thread }
  REFRESH_MASTER         = 128;   { Remove all bin logs in the index and truncate the index }
  REFRESH_READ_LOCK      = 16384; { Lock tables for read }
  REFRESH_FAST		 = 32768; { Intern flag }

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

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    procedure Despose(var Handle: PZMySQLConnect);

    function Connect(Handle: PZMySQLConnect;
      const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect;
      const Host, User, Password, Db: PChar; Port: Cardinal;
      UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);

    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;
      Length: Integer): Integer;

    function SelectDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;
      const Database: PChar): Integer;

    function Shutdown(Handle: PZMySQLConnect): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;

    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;
      const Arg: PChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;

    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;

    function GetListDatabases(Handle: PZMySQLConnect; Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect; const Wild: PChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect; const Table, Wild: PChar): PZMySQLResult;
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
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
  end;

  {** Implements a driver for MySQL 4.1 }
  TZMySQL41PlainDriver = class (TZAbstractObject, IZPlainDriver,IZMySQLPlainDriver)
  private
    constructor Create;
    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;
    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    procedure Despose(var Handle: PZMySQLConnect);
    function Connect(Handle: PZMySQLConnect; const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PChar; Port: Cardinal;    UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);
    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar; Length: Integer): Integer;
    function SelectDatabase(Handle: PZMySQLConnect; const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;const Database: PChar): Integer;
    function Shutdown(Handle: PZMySQLConnect): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;const Arg: PChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function GetListDatabases(Handle: PZMySQLConnect;Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;const Wild: PChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect;const Table, Wild: PChar): PZMySQLResult;
    function GetListProcesses(Handle: PZMySQLConnect): PZMySQLResult;
    function StoreResult(Handle: PZMySQLConnect): PZMySQLResult;
    function UseResult(Handle: PZMySQLConnect): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(Handle: PZMySQLConnect): Int64;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PLongInt;
    function FetchField(Res: PZMySQLResult): PZMySQLField;
    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    function GetFieldType(Field: PZMySQLField): Byte;
    function GetFieldFlags(Field: PZMySQLField): Integer;
    function ResultSetExists(Handle: PZMySQLConnect):Boolean;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function GetFieldCount(Res: PZMySQLResult): Integer;
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
  end;


  TZMySQL5PlainDriver = class (TZAbstractObject, IZPlainDriver,IZMySQLPlainDriver)
  public
    constructor Create;
  private
    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    procedure Debug(Debug: PChar);
    function DumpDebugInfo(Handle: PZMySQLConnect): Integer;
    function GetLastError(Handle: PZMySQLConnect): PChar;
    function GetLastErrorCode(Handle: PZMySQLConnect): Integer;
    function Init(var Handle: PZMySQLConnect): PZMySQLConnect;
    procedure Despose(var Handle: PZMySQLConnect);
    function Connect(Handle: PZMySQLConnect; const Host, User, Password: PChar): PZMySQLConnect;
    function RealConnect(Handle: PZMySQLConnect; const Host, User, Password, Db: PChar; Port: Cardinal;UnixSocket: PChar; ClientFlag: Cardinal): PZMySQLConnect;
    procedure Close(Handle: PZMySQLConnect);
    function ExecQuery(Handle: PZMySQLConnect; const Query: PChar): Integer;
    function ExecRealQuery(Handle: PZMySQLConnect; const Query: PChar;Length: Integer): Integer;
    function SelectDatabase(Handle: PZMySQLConnect;const Database: PChar): Integer;
    function CreateDatabase(Handle: PZMySQLConnect;const Database: PChar): Integer;
    function DropDatabase(Handle: PZMySQLConnect;const Database: PChar): Integer;
    function Shutdown(Handle: PZMySQLConnect): Integer;
    function Refresh(Handle: PZMySQLConnect; Options: Cardinal): Integer;
    function Kill(Handle: PZMySQLConnect; Pid: LongInt): Integer;
    function Ping(Handle: PZMySQLConnect): Integer;
    function GetStatInfo(Handle: PZMySQLConnect): PChar;
    function SetOptions(Handle: PZMySQLConnect; Option: TZMySQLOption;const Arg: PChar): Integer;
    function GetEscapeString(StrTo, StrFrom: PChar; Length: Cardinal): Cardinal;
    function GetServerInfo(Handle: PZMySQLConnect): PChar;
    function GetClientInfo: PChar;
    function GetHostInfo(Handle: PZMySQLConnect): PChar;
    function GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
    function GetThreadId(Handle: PZMySQLConnect): Cardinal;
    function GetListDatabases(Handle: PZMySQLConnect;Wild: PChar): PZMySQLResult;
    function GetListTables(Handle: PZMySQLConnect;const Wild: PChar): PZMySQLResult;
    function GetListFields(Handle: PZMySQLConnect;const Table, Wild: PChar): PZMySQLResult;
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
    function GetFieldName(Field: PZMySQLField): PChar;
    function GetFieldTable(Field: PZMySQLField): PChar;
    function GetFieldLength(Field: PZMySQLField): Integer;
    function GetFieldMaxLength(Field: PZMySQLField): Integer;
    function GetFieldDecimals(Field: PZMySQLField): Integer;
    function GetFieldData(Row: PZMySQLRow; Offset: Cardinal): PChar;
  end;


implementation

uses SysUtils, ZPlainMySql320, ZPlainMySql323, ZPlainMySql40, ZPlainMySql41,ZPlainMySql5;

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
end;

procedure TZMySQL41PlainDriver.Close(Handle: PZMySQLConnect);
begin
  ZPlainMySql41.mysql_close(Handle);
end;

function TZMySQL41PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := ZPlainMySql41.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL41PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql41.mysql_create_db(Handle, Database);
end;

procedure TZMySQL41PlainDriver.Debug(Debug: PChar);
begin
  ZPlainMySql41.mysql_debug(Debug);
end;

function TZMySQL41PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql41.mysql_drop_db(Handle, Database);
end;

function TZMySQL41PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql41.mysql_dump_debug_info(Handle);
end;

function TZMySQL41PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := ZPlainMySql41.mysql_query(Handle, Query);
end;

function TZMySQL41PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := ZPlainMySql41.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL41PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := ZPlainMySql41.mysql_fetch_field(Res);
end;

function TZMySQL41PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := ZPlainMySql41.mysql_fetch_lengths(Res);
end;

function TZMySQL41PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := ZPlainMySql41.mysql_fetch_row(Res);
end;

procedure TZMySQL41PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  ZPlainMySql41.mysql_free_result(Res);
end;

function TZMySQL41PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := ZPlainMySql41.mysql_affected_rows(Handle);
end;

function TZMySQL41PlainDriver.GetClientInfo: PChar;
begin
  Result := ZPlainMySql41.mysql_get_client_info;
end;

function TZMySQL41PlainDriver.GetEscapeString(StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := ZPlainMySql41.mysql_escape_string(StrTo, StrFrom, Length);
end;

function TZMySQL41PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql41.mysql_get_host_info(Handle);
end;

function TZMySQL41PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL41PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_list_processes(Handle);
end;

function TZMySQL41PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_list_tables(Handle, Wild);
end;

function TZMySQL41PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := ZPlainMySql41.mysql_get_proto_info(Handle);
end;

function TZMySQL41PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql41.mysql_get_server_info(Handle);
end;

function TZMySQL41PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql41.mysql_stat(Handle);
end;

function TZMySQL41PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := ZPlainMySql41.mysql_thread_id(Handle);
end;

function TZMySQL41PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
//  Handle := AllocMem(SizeOf(ZPlainMySql41.MYSQL));
//  Result := ZPlainMySql41.mysql_init(Handle);
  Handle := ZPlainMySql41.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQL41PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
//  if Handle <> nil then
//    FreeMem(Handle);
  Handle := nil;
end;

function TZMySQL41PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := ZPlainMySql41.mysql_kill(Handle, Pid);
end;

function TZMySQL41PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql41.mysql_ping(Handle);
end;

function TZMySQL41PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := ZPlainMySql41.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL41PlainDriver.Refresh(Handle: PZMySQLConnect;Options: Cardinal): Integer;
begin
  Result := ZPlainMySql41.mysql_refresh(Handle, Options);
end;

function TZMySQL41PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
begin
 result := ZPlainMySql41.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

procedure TZMySQL41PlainDriver.SeekData(Res: PZMySQLResult;Offset: Cardinal);
begin
  ZPlainMySql41.mysql_data_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := ZPlainMySql41.mysql_field_seek(Res, Offset);
end;

function TZMySQL41PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := ZPlainMySql41.mysql_row_seek(Res, Row);
end;

function TZMySQL41PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql41.mysql_select_db(Handle, Database);
end;

function TZMySQL41PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := ZPlainMySql41.mysql_options(Handle,
    ZPlainMySql41.TMySqlOption(Option), Arg);
end;

function TZMySQL41PlainDriver.Shutdown(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql41.mysql_shutdown(Handle);
end;

function TZMySQL41PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_store_result(Handle);
end;

function TZMySQL41PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql41.mysql_use_result(Handle);
end;

function TZMySQL41PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql41.mysql_error(Handle);
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
  Result := ZPlainMySql41.PMYSQL_RES(Res).row_count;
end;


function TZMySQL41PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql41.PMYSQL_RES(Res).field_count;
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

function TZMySQL41PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL41PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL41PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql41.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL41PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql41.mysql_errno(ZPlainMySql41.PMYSQL(Handle));
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
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

procedure TZMySQL5PlainDriver.Initialize;
begin
  ZPlainMySql5.LibraryLoader.LoadIfNeeded;
end;

procedure TZMySQL5PlainDriver.Close(Handle: PZMySQLConnect);
begin
  ZPlainMySql5.mysql_close(Handle);
end;

function TZMySQL5PlainDriver.Connect(Handle: PZMySQLConnect; const Host,
  User, Password: PChar): PZMySQLConnect;
begin
  Result := ZPlainMySql5.mysql_connect(Handle, Host, User, Password);
end;

function TZMySQL5PlainDriver.CreateDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql5.mysql_create_db(Handle, Database);
end;

procedure TZMySQL5PlainDriver.Debug(Debug: PChar);
begin
  ZPlainMySql5.mysql_debug(Debug);
end;

function TZMySQL5PlainDriver.DropDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql5.mysql_drop_db(Handle, Database);
end;

function TZMySQL5PlainDriver.DumpDebugInfo(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql5.mysql_dump_debug_info(Handle);
end;

function TZMySQL5PlainDriver.ExecQuery(Handle: PZMySQLConnect;
  const Query: PChar): Integer;
begin
  Result := ZPlainMySql5.mysql_query(Handle, Query);
end;

function TZMySQL5PlainDriver.ExecRealQuery(Handle: PZMySQLConnect;
  const Query: PChar; Length: Integer): Integer;
begin
  Result := ZPlainMySql5.mysql_real_query(Handle, Query, Length);
end;

function TZMySQL5PlainDriver.FetchField(Res: PZMySQLResult): PZMySQLField;
begin
  Result := ZPlainMySql5.mysql_fetch_field(Res);
end;

function TZMySQL5PlainDriver.FetchLengths(Res: PZMySQLResult): PLongInt;
begin
  Result := ZPlainMySql5.mysql_fetch_lengths(Res);
end;

function TZMySQL5PlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := ZPlainMySql5.mysql_fetch_row(Res);
end;

procedure TZMySQL5PlainDriver.FreeResult(Res: PZMySQLResult);
begin
  ZPlainMySql5.mysql_free_result(Res);
end;

function TZMySQL5PlainDriver.GetAffectedRows(Handle: PZMySQLConnect): Int64;
begin
  Result := ZPlainMySql5.mysql_affected_rows(Handle);
end;

function TZMySQL5PlainDriver.GetClientInfo: PChar;
begin
  Result := ZPlainMySql5.mysql_get_client_info;
end;

function TZMySQL5PlainDriver.GetEscapeString(StrTo, StrFrom: PChar;
  Length: Cardinal): Cardinal;
begin
  Result := ZPlainMySql5.mysql_escape_string(StrTo, StrFrom, Length);
end;

function TZMySQL5PlainDriver.GetHostInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql5.mysql_get_host_info(Handle);
end;

function TZMySQL5PlainDriver.GetListDatabases(Handle: PZMySQLConnect;
  Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_list_dbs(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetListFields(Handle: PZMySQLConnect;
  const Table, Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_list_fields(Handle, Table, Wild);
end;

function TZMySQL5PlainDriver.GetListProcesses(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_list_processes(Handle);
end;

function TZMySQL5PlainDriver.GetListTables(Handle: PZMySQLConnect;
  const Wild: PChar): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_list_tables(Handle, Wild);
end;

function TZMySQL5PlainDriver.GetProtoInfo(Handle: PZMySQLConnect): Cardinal;
begin
  Result := ZPlainMySql5.mysql_get_proto_info(Handle);
end;

function TZMySQL5PlainDriver.GetServerInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql5.mysql_get_server_info(Handle);
end;

function TZMySQL5PlainDriver.GetStatInfo(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql5.mysql_stat(Handle);
end;

function TZMySQL5PlainDriver.GetThreadId(Handle: PZMySQLConnect): Cardinal;
begin
  Result := ZPlainMySql5.mysql_thread_id(Handle);
end;

function TZMySQL5PlainDriver.Init(var Handle: PZMySQLConnect): PZMySQLConnect;
begin
//  Handle := AllocMem(SizeOf(ZPlainMySql5.MYSQL));
//  Result := ZPlainMySql5.mysql_init(Handle);
  Handle := ZPlainMySql5.mysql_init(nil);
  Result := Handle;
end;

procedure TZMySQL5PlainDriver.Despose(var Handle: PZMySQLConnect);
begin
//  if Handle <> nil then
//    FreeMem(Handle);
  Handle := nil;
end;

function TZMySQL5PlainDriver.Kill(Handle: PZMySQLConnect;
  Pid: Integer): Integer;
begin
  Result := ZPlainMySql5.mysql_kill(Handle, Pid);
end;

function TZMySQL5PlainDriver.Ping(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql5.mysql_ping(Handle);
end;

function TZMySQL5PlainDriver.RealConnect(Handle: PZMySQLConnect;
  const Host, User, Password, Db: PChar; Port: Cardinal; UnixSocket: PChar;
  ClientFlag: Cardinal): PZMySQLConnect;
begin
  Result := ZPlainMySql5.mysql_real_connect(Handle, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQL5PlainDriver.Refresh(Handle: PZMySQLConnect;
  Options: Cardinal): Integer;
begin
  Result := ZPlainMySql5.mysql_refresh(Handle, Options);
end;

function TZMySQL5PlainDriver.ResultSetExists(Handle: PZMySQLConnect): Boolean;
var nr:integer;
begin
 result := ZPlainMySql5.mysql_field_count(Handle)<>0;
 // True If statement should return a resultset
end;

procedure TZMySQL5PlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  ZPlainMySql5.mysql_data_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := ZPlainMySql5.mysql_field_seek(Res, Offset);
end;

function TZMySQL5PlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := ZPlainMySql5.mysql_row_seek(Res, Row);
end;

function TZMySQL5PlainDriver.SelectDatabase(Handle: PZMySQLConnect;
  const Database: PChar): Integer;
begin
  Result := ZPlainMySql5.mysql_select_db(Handle, Database);
end;

function TZMySQL5PlainDriver.SetOptions(Handle: PZMySQLConnect;
  Option: TZMySQLOption; const Arg: PChar): Integer;
begin
  Result := ZPlainMySql5.mysql_options(Handle,
    ZPlainMySql5.TMySqlOption(Option), Arg);
end;

function TZMySQL5PlainDriver.Shutdown(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql5.mysql_shutdown(Handle);
end;

function TZMySQL5PlainDriver.StoreResult(
  Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_store_result(Handle);
end;

function TZMySQL5PlainDriver.UseResult(Handle: PZMySQLConnect): PZMySQLResult;
begin
  Result := ZPlainMySql5.mysql_use_result(Handle);
end;

function TZMySQL5PlainDriver.GetLastError(Handle: PZMySQLConnect): PChar;
begin
  Result := ZPlainMySql5.mysql_error(Handle);
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
  Result := ZPlainMySql5.PMYSQL_RES(Res).row_count;
end;


function TZMySQL5PlainDriver.GetFieldCount(Res: PZMySQLResult): Integer;
begin
  Result := ZPlainMySql5.PMYSQL_RES(Res).field_count;
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

function TZMySQL5PlainDriver.GetFieldName(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.name;
end;

function TZMySQL5PlainDriver.GetFieldTable(Field: PZMySQLField): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_FIELD(Field)^.table;
end;

function TZMySQL5PlainDriver.GetFieldData(Row: PZMySQLRow;
  Offset: Cardinal): PChar;
begin
  Result := ZPlainMySql5.PMYSQL_ROW(ROW)[Offset];
end;

function TZMySQL5PlainDriver.GetLastErrorCode(Handle: PZMySQLConnect): Integer;
begin
  Result := ZPlainMySql5.mysql_errno(ZPlainMySql5.PMYSQL(Handle));
end;




end.

