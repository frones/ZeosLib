{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6Utils;

interface

{$I ZDbc.inc}
uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} Types,
  ZDbcIntfs, ZDbcStatement, ZPlainFirebirdDriver, ZCompatibility,
  ZPlainFirebirdInterbaseConstants, ZDbcCachedResultSet, ZDbcLogging, ZMessages,
  ZVariant;

type
  { Interbase Statement Type }
  TZIbSqlStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProc, stStartTrans, stCommit,
    stRollback, stSelectForUpdate, stSetGenerator, stDisconnect);

  { Interbase Error Class}
  EZIBConvertError = class(Exception);

  { Paparameter string name and it value}
  TZIbParam = record
    Name: String;
    Number: word;
  end;
  PZIbParam = ^TZIbParam;

  { Interbase blob Information structure
    contain iformation about blob size in bytes,
    segments count, segment size in bytes and blob type
    Note: blob type can be text and binary }
  TIbBlobInfo = record
    NumSegments: Word;
    MaxSegmentSize: Word;
    BlobType: SmallInt;
    TotalSize: LongInt;
  end;

  { Base interface for sqlda }
  IZSQLDA = interface
    ['{2D0D6029-B31C-4E39-89DC-D86D20437C35}']
    procedure InitFields(Parameters: boolean);
    procedure AllocateSQLDA;
    procedure FreeParamtersValues;

    function GetData: PXSQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldIndex(const Name: AnsiString): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;
  end;

  { parameters interface sqlda}
  IZParamsSQLDA = interface(IZSQLDA)
    ['{D2C3D5E1-F3A6-4223-9A6E-3048B99A06C4}']
    procedure WriteLobBuffer(const Index: Integer; const Buffer: Pointer; const Len: Integer);
    procedure UpdateNull(const Index: Integer; const Value: boolean);
    procedure UpdateBoolean(const Index: Integer; const Value: boolean);
    procedure UpdateSmall(const Index: Integer; const Value: SmallInt);
    procedure UpdateInt(const Index: Integer; const Value: Integer);
    procedure UpdateLong(const Index: Integer; const Value: Int64);
    procedure UpdateFloat(const Index: Integer; const Value: Single);
    procedure UpdateDouble(const Index: Integer; const Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; const Value: Extended);
    procedure UpdatePAnsiChar(const Index: Integer; const Value: PAnsiChar; const Len: Cardinal);
    procedure UpdateBytes(const Index: Integer; const Value: TBytes);
    procedure UpdateDate(const Index: Integer; const Value: TDateTime);
    procedure UpdateTime(const Index: Integer; const Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; const Value: TDateTime);
    procedure UpdateQuad(const Index: Word; const Value: TISC_QUAD);
    procedure UpdateArray(const Index: Word; const Value; const SQLType: TZSQLType;
      const VariantType: TZVariantType = vtNull);
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }
  TZSQLDA = class (TZCodePagedObject, IZSQLDA)
  private
    FHandle: PISC_DB_HANDLE;
    FTransactionHandle: PISC_TR_HANDLE;
    FXSQLDA: PXSQLDA;
    FPlainDriver: IZInterbasePlainDriver;
    FDecribedLengthArray: TSmallIntDynArray;
    FDecribedScaleArray: TSmallIntDynArray;
    FDecribedTypeArray: TSmallIntDynArray;
    procedure CheckRange(const Index: Word);
    procedure IbReAlloc(var P; OldSize, NewSize: Integer);
    procedure SetFieldType(const Index: Word; Size: Integer; Code: Smallint;
      Scale: Smallint);
  public
    constructor Create(PlainDriver: IZInterbasePlainDriver;
      Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE;
      ConSettings: PZConSettings);
    destructor Destroy; override;
    procedure InitFields(Parameters: boolean);
    procedure AllocateSQLDA;
    procedure FreeParamtersValues;

    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldIndex(const Name: AnsiString): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetData: PXSQLDA;

    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;
  end;

  { Parameters class for sqlda structure.
    It clas can only write data to parameters/fields }
  TZParamsSQLDA = class (TZSQLDA, IZParamsSQLDA)
  private
    procedure EncodeString(const Code: Smallint; const Index: Word; const Str: RawByteString);
    procedure EncodePData(const Code: Smallint; const Index: Word;
      const Value: PAnsiChar; const Len: LengthInt);
    procedure UpdateDateTime(const Index: Integer; Value: TDateTime);
  public
    procedure WriteLobBuffer(const Index: Integer; const Buffer: Pointer; const Len: Integer);

    procedure UpdateNull(const Index: Integer; const Value: boolean);
    procedure UpdateBoolean(const Index: Integer; const Value: boolean);
    procedure UpdateSmall(const Index: Integer; const Value: SmallInt);
    procedure UpdateInt(const Index: Integer; const Value: Integer);
    procedure UpdateLong(const Index: Integer; const Value: Int64);
    procedure UpdateFloat(const Index: Integer; const Value: Single);
    procedure UpdateDouble(const Index: Integer; const Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; const Value: Extended);
    procedure UpdatePAnsiChar(const Index: Integer; const Value: PAnsiChar; const Len: Cardinal);
    procedure UpdateBytes(const Index: Integer; const Value: TBytes);
    procedure UpdateDate(const Index: Integer; const Value: TDateTime);
    procedure UpdateTime(const Index: Integer; const Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; const Value: TDateTime);
    procedure UpdateQuad(const Index: Word; const Value: TISC_QUAD);
    procedure UpdateArray(const Index: Word; const Value; const SQLType: TZSQLType;
      const VariantType: TZVariantType = vtNull);
  end;

function RandomString(Len: integer): RawByteString;
function CreateIBResultSet(const SQL: string; const Statement: IZStatement;
  const NativeResultSet: IZResultSet): IZResultSet;

{Interbase6 Connection Functions}
function GenerateDPB(Info: TStrings; var FDPBLength, Dialect: Word): PAnsiChar;
function GenerateTPB(Params: TStrings; var Handle: TISC_DB_HANDLE): PISC_TEB;
function GetInterbase6DatabaseParamNumber(const Value: String): word;
function GetInterbase6TransactionParamNumber(const Value: String): word;

{ Interbase6 errors functions }
function GetNameSqlType(Value: Word): RawByteString;
function CheckInterbase6Error(const PlainDriver: IZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS; const ConSettings: PZConSettings;
  const LoggingCategory: TZLoggingCategory = lcOther;
  SQL: RawByteString = '') : Integer;

{ Interbase information functions}
function GetISC_StringInfo(const PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; ConSettings: PZConSettings): String;
function GetFB_ISC_IntegerInfo(const PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; ConSettings: PZConSettings): LongInt;
function GetDBImplementationNo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): LongInt;
function GetDBImplementationClass(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): LongInt;
function GetLongDbInfo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const DatabaseInfoCommand: Integer;
  const ConSettings: PZConSettings): LongInt;
function GetStringDbInfo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const DatabaseInfoCommand: Integer;
  const ConSettings: PZConSettings): AnsiString;
function GetDBSQLDialect(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): Integer;

{ Interbase statement functions}
function PrepareStatement(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TrHandle: PISC_TR_HANDLE;
  const Dialect: Word; const SQL: RawByteString; const ConSettings: PZConSettings;
  var StmtHandle: TISC_STMT_HANDLE): TZIbSqlStatementType;
procedure PrepareResultSqlData(const PlainDriver: IZInterbasePlainDriver;
  const Dialect: Word; const SQL: RawByteString;
  var StmtHandle: TISC_STMT_HANDLE; const SqlData: IZSQLDA;
  const ConSettings: PZConSettings); overload;
procedure PrepareParameters(const PlainDriver: IZInterbasePlainDriver;
  const SQL: RawByteString; const Dialect: Word; var StmtHandle: TISC_STMT_HANDLE;
  const ParamSqlData: IZParamsSQLDA; const ConSettings: PZConSettings);
procedure BindSQLDAInParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray;  const InParamCount: Integer;
  const ParamSqlData: IZParamsSQLDA; const ConSettings: PZConSettings;
  const CodePageArray: TWordDynArray; ArrayOffSet, ArrayItersCount: Integer); overload;
procedure BindSQLDAInParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const InParamTypes: TZSQLTypeArray;
  const InParamCount: Integer; const ParamSqlData: IZParamsSQLDA;
  const ConSettings: PZConSettings; const CodePageArray: TWordDynArray); overload;
procedure FreeStatement(PlainDriver: IZInterbasePlainDriver;
  StatementHandle: TISC_STMT_HANDLE; Options : Word);
function GetStatementType(const PlainDriver: IZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const ConSettings: PZConSettings): TZIbSqlStatementType;
function GetAffectedRows(const PlainDriver: IZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const StatementType: TZIbSqlStatementType;
  const ConSettings: PZConSettings): integer;

function ConvertInterbase6ToSqlType(const SqlType, SqlSubType, Scale: Integer;
  const CtrlsCPType: TZControlsCodePage): TZSqlType;

{ interbase blob routines }
procedure GetBlobInfo(const PlainDriver: IZInterbasePlainDriver;
  const BlobHandle: TISC_BLOB_HANDLE; var BlobInfo: TIbBlobInfo;
  const ConSettings: PZConSettings);
procedure ReadBlobBufer(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TransactionHandle: PISC_TR_HANDLE;
  const BlobId: TISC_QUAD; var Size: Integer; var Buffer: Pointer;
  const Binary: Boolean; const ConSettings: PZConSettings);

function GetExecuteBlockString(const ParamsSQLDA: IZParamsSQLDA;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: IZInterbasePlainDriver;
  var MemPerRow, PreparedRowsOfArray: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: LongWord): RawByteString;

const
  { Default Interbase blob size for reading }
  DefaultBlobSegmentSize = 16 * 1024;

  IBScaleDivisor: array[-18..-1] of Int64 = (
    {sqldialect 3 range 1..18}
    1000000000000000000,
    100000000000000000,
    10000000000000000,
    {sqldialect 1 range 1..15}
    1000000000000000,
    100000000000000,
    10000000000000,
    1000000000000,
    100000000000,
    10000000000,
    1000000000,
    100000000,
    10000000,1000000,100000,10000,1000,100,10);

  { count database parameters }
  MAX_DPB_PARAMS = 67;
  { prefix database parameters names it used in paramters scann procedure }
  BPBPrefix = RawByteString('isc_dpb_');
  { list database parameters and their apropriate numbers }
  DatabaseParams: array [0..MAX_DPB_PARAMS]of TZIbParam = (
    (Name:'isc_dpb_version1';         Number: isc_dpb_version1),
    (Name:'isc_dpb_cdd_pathname';     Number: isc_dpb_cdd_pathname),
    (Name:'isc_dpb_allocation';       Number: isc_dpb_allocation),
    (Name:'isc_dpb_journal';          Number: isc_dpb_journal),
    (Name:'isc_dpb_page_size';        Number: isc_dpb_page_size),
    (Name:'isc_dpb_num_buffers';      Number: isc_dpb_num_buffers),
    (Name:'isc_dpb_buffer_length';    Number: isc_dpb_buffer_length),
    (Name:'isc_dpb_debug';            Number: isc_dpb_debug),
    (Name:'isc_dpb_garbage_collect';  Number: isc_dpb_garbage_collect),
    (Name:'isc_dpb_verify';           Number: isc_dpb_verify),
    (Name:'isc_dpb_sweep';            Number: isc_dpb_sweep),
    (Name:'isc_dpb_enable_journal';   Number: isc_dpb_enable_journal),
    (Name:'isc_dpb_disable_journal';  Number: isc_dpb_disable_journal),
    (Name:'isc_dpb_dbkey_scope';      Number: isc_dpb_dbkey_scope),
    (Name:'isc_dpb_number_of_users';  Number: isc_dpb_number_of_users),
    (Name:'isc_dpb_trace';            Number: isc_dpb_trace),
    (Name:'isc_dpb_no_garbage_collect'; Number: isc_dpb_no_garbage_collect),
    (Name:'isc_dpb_damaged';          Number: isc_dpb_damaged),
    (Name:'isc_dpb_license';          Number: isc_dpb_license),
    (Name:'isc_dpb_sys_user_name';    Number: isc_dpb_sys_user_name),
    (Name:'isc_dpb_encrypt_key';      Number: isc_dpb_encrypt_key),
    (Name:'isc_dpb_activate_shadow';  Number: isc_dpb_activate_shadow),
    (Name:'isc_dpb_sweep_interval';   Number: isc_dpb_sweep_interval),
    (Name:'isc_dpb_delete_shadow';    Number: isc_dpb_delete_shadow),
    (Name:'isc_dpb_force_write';      Number: isc_dpb_force_write),
    (Name:'isc_dpb_begin_log';        Number: isc_dpb_begin_log),
    (Name:'isc_dpb_quit_log';         Number: isc_dpb_quit_log),
    (Name:'isc_dpb_no_reserve';       Number: isc_dpb_no_reserve),
    (Name:'isc_dpb_username';         Number: isc_dpb_user_name),
    (Name:'isc_dpb_password';         Number: isc_dpb_password),
    (Name:'isc_dpb_password_enc';     Number: isc_dpb_password_enc),
    (Name:'isc_dpb_sys_user_name_enc';  Number: isc_dpb_sys_user_name_enc),
    (Name:'isc_dpb_interp';           Number: isc_dpb_interp),
    (Name:'isc_dpb_online_dump';      Number: isc_dpb_online_dump),
    (Name:'isc_dpb_old_file_size';    Number: isc_dpb_old_file_size),
    (Name:'isc_dpb_old_num_files';    Number: isc_dpb_old_num_files),
    (Name:'isc_dpb_old_file';         Number: isc_dpb_old_file),
    (Name:'isc_dpb_old_start_page';   Number: isc_dpb_old_start_page),
    (Name:'isc_dpb_old_start_seqno';  Number: isc_dpb_old_start_seqno),
    (Name:'isc_dpb_old_start_file';   Number: isc_dpb_old_start_file),
    (Name:'isc_dpb_drop_walfile';     Number: isc_dpb_drop_walfile),
    (Name:'isc_dpb_old_dump_id';      Number: isc_dpb_old_dump_id),
    (Name:'isc_dpb_wal_backup_dir';   Number: isc_dpb_wal_backup_dir),
    (Name:'isc_dpb_wal_chkptlen';     Number: isc_dpb_wal_chkptlen),
    (Name:'isc_dpb_wal_numbufs';      Number: isc_dpb_wal_numbufs),
    (Name:'isc_dpb_wal_bufsize';      Number: isc_dpb_wal_bufsize),
    (Name:'isc_dpb_wal_grp_cmt_wait'; Number: isc_dpb_wal_grp_cmt_wait),
    (Name:'isc_dpb_lc_messages';      Number: isc_dpb_lc_messages),
    (Name:'isc_dpb_lc_ctype';         Number: isc_dpb_lc_ctype),
    (Name:'isc_dpb_cache_manager';    Number: isc_dpb_cache_manager),
    (Name:'isc_dpb_shutdown';         Number: isc_dpb_shutdown),
    (Name:'isc_dpb_online';           Number: isc_dpb_online),
    (Name:'isc_dpb_shutdown_delay';   Number: isc_dpb_shutdown_delay),
    (Name:'isc_dpb_reserved';         Number: isc_dpb_reserved),
    (Name:'isc_dpb_overwrite';        Number: isc_dpb_overwrite),
    (Name:'isc_dpb_sec_attach';       Number: isc_dpb_sec_attach),
    (Name:'isc_dpb_disable_wal';      Number: isc_dpb_disable_wal),
    (Name:'isc_dpb_connect_timeout';  Number: isc_dpb_connect_timeout),
    (Name:'isc_dpb_dummy_packet_interval'; Number: isc_dpb_dummy_packet_interval),
    (Name:'isc_dpb_gbak_attach';      Number: isc_dpb_gbak_attach),
    (Name:'isc_dpb_sql_role_name';    Number: isc_dpb_sql_role_name),
    (Name:'isc_dpb_set_page_buffers'; Number: isc_dpb_set_page_buffers),
    (Name:'isc_dpb_working_directory';  Number: isc_dpb_working_directory),
    (Name:'isc_dpb_sql_dialect';      Number: isc_dpb_SQL_dialect),
    (Name:'isc_dpb_set_db_readonly';  Number: isc_dpb_set_db_readonly),
    (Name:'isc_dpb_set_db_sql_dialect'; Number: isc_dpb_set_db_SQL_dialect),
    (Name:'isc_dpb_gfix_attach';      Number: isc_dpb_gfix_attach),
    (Name:'isc_dpb_gstat_attach';     Number: isc_dpb_gstat_attach)
  );

  { count transaction parameters }
  MAX_TPB_PARAMS = 24;
  { prefix transaction parameters names it used in paramters scann procedure }
  TPBPrefix = 'isc_tpb_';
  { list transaction parameters and their apropriate numbers }
  TransactionParams: array [0..MAX_TPB_PARAMS-1]of TZIbParam = (
    (Name:'isc_tpb_version1';         Number: isc_tpb_version1),
    (Name:'isc_tpb_version3';         Number: isc_tpb_version3),
    (Name:'isc_tpb_consistency';      Number: isc_tpb_consistency),
    (Name:'isc_tpb_concurrency';      Number: isc_tpb_concurrency),
    (Name:'isc_tpb_shared';           Number: isc_tpb_shared),
    (Name:'isc_tpb_protected';        Number: isc_tpb_protected),
    (Name:'isc_tpb_exclusive';        Number: isc_tpb_exclusive),
    (Name:'isc_tpb_wait';             Number: isc_tpb_wait),
    (Name:'isc_tpb_nowait';           Number: isc_tpb_nowait),
    (Name:'isc_tpb_read';             Number: isc_tpb_read),
    (Name:'isc_tpb_write';            Number: isc_tpb_write),
    (Name:'isc_tpb_lock_read';        Number: isc_tpb_lock_read),
    (Name:'isc_tpb_lock_write';       Number: isc_tpb_lock_write),
    (Name:'isc_tpb_verb_time';        Number: isc_tpb_verb_time),
    (Name:'isc_tpb_commit_time';      Number: isc_tpb_commit_time),
    (Name:'isc_tpb_ignore_limbo';     Number: isc_tpb_ignore_limbo),
    (Name:'isc_tpb_read_committed';   Number: isc_tpb_read_committed),
    (Name:'isc_tpb_autocommit';       Number: isc_tpb_autocommit),
    (Name:'isc_tpb_rec_version';      Number: isc_tpb_rec_version),
    (Name:'isc_tpb_no_rec_version';   Number: isc_tpb_no_rec_version),
    (Name:'isc_tpb_restart_requests'; Number: isc_tpb_restart_requests),
    (Name:'isc_tpb_no_auto_undo';     Number: isc_tpb_no_auto_undo),
    (Name:'isc_tpb_no_savepoint';     Number: isc_tpb_no_savepoint),// Since IB75+
    (Name:'isc_tpb_lock_timeout';     Number: isc_tpb_lock_timeout) // Since FB20+
    );

implementation

uses
  ZFastCode, Variants, ZSysUtils, Math, ZDbcInterbase6, ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
   Generate specific length random string and return it
   @param Len a length result string
   @return random string
}
function RandomString(Len: integer): RawByteString;
begin
  Result := '';
  while Length(Result) < Len do
    Result := Result + IntToRaw({$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Random(High(Integer))));
  if Length(Result) > Len then
    Result := Copy(Result, 1, Len);
end;

{**
  Create CachedResultSet with using TZCachedResultSet and return it.
  @param SQL a sql query command
  @param Statement a zeos statement object
  @param NativeResultSet a native result set
  @return cached ResultSet if rcReadOnly <> rcReadOnly
}
function CreateIBResultSet(const SQL: string; const Statement: IZStatement;
  const NativeResultSet: IZResultSet): IZResultSet;
var
  CachedResolver: TZInterbase6CachedResolver;
  CachedResultSet: TZCachedResultSet;
begin
  if (Statement.GetResultSetConcurrency = rcUpdatable)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResolver  := TZInterbase6CachedResolver.Create(Statement,  NativeResultSet.GetMetadata);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, Statement.GetConnection.GetConSettings);
    CachedResultSet.SetConcurrency(Statement.GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Generate database connection string by connection information
  @param DPB - a database connection string
  @param Dialect - a sql dialect number
  @param Info - a list connection interbase parameters
  @return a generated string length
}
function GenerateDPB(Info: TStrings; var FDPBLength, Dialect: Word): PAnsiChar;
var
  I, Pos, PValue: Integer;
  ParamNo: Word;
  ParamName, Buffer: String;
  DPB, ParamValue: RawByteString;
begin
  FDPBLength := 1;
  DPB := AnsiChar(isc_dpb_version1);

  for I := 0 to Info.Count - 1 do
  begin
    Buffer := Info.Strings[I];
    Pos := FirstDelimiter(' ='#9#10#13, Buffer);
    ParamName := Copy(Buffer, 1, Pos - 1);
    Delete(Buffer, 1, Pos);
    ParamValue := RawByteString(Buffer);
    ParamNo := GetInterbase6DatabaseParamNumber(ParamName);

    case ParamNo of
      0: Continue;
      isc_dpb_set_db_SQL_dialect:
        Dialect := RawToIntDef(ParamValue, 0);
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_sql_role_name,
	  isc_dpb_connect_timeout:
        begin
          DPB := DPB + AnsiChar(ParamNo) + AnsiChar(Length(ParamValue)) + ParamValue;
          Inc(FDPBLength, 2 + Length(ParamValue));
        end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
        begin
          DPB := DPB + AnsiChar(ParamNo) + #1 + AnsiChar(ZFastCode.RawToInt(ParamValue));
          Inc(FDPBLength, 3);
        end;
      isc_dpb_sweep:
        begin
          DPB := DPB + AnsiChar(ParamNo) + #1 + AnsiChar(isc_dpb_records);
          Inc(FDPBLength, 3);
        end;
      isc_dpb_sweep_interval:
        begin
          PValue := ZFastCode.RawToInt(ParamValue);
          DPB := DPB + AnsiChar(ParamNo) + #4 + PAnsiChar(@PValue)[0] +
                 PAnsiChar(@PValue)[1] + PAnsiChar(@PValue)[2] + PAnsiChar(@PValue)[3];
          Inc(FDPBLength, 6);
        end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
        begin
          DPB := DPB + AnsiChar(ParamNo) + #1 + #0;
          Inc(FDPBLength, 3);
        end;
    end;
  end;

  {$IFDEF UNICODE}
  Result := AnsiStrAlloc(FDPBLength + 1);
  {$ELSE}
  Result := StrAlloc(FDPBLength + 1);
  {$ENDIF}


  {$IFDEF WITH_STRPCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrPCopy(Result, DPB);
end;

{**
   Generate transaction structuer by connection information
   @param Params - a transaction parameters list
   @param Dialect - a database connection handle
   @return a transaction ISC structure
}
function GenerateTPB(Params: TStrings; var Handle: TISC_DB_HANDLE): PISC_TEB;
var
  I: Integer;
  TPBLength,ParamNo: Word;
  TempStr, ParamValue: AnsiString;
  TPB: PAnsiChar;
  IsolationLevel: Boolean;
begin
  TPBLength := 0;
  TempStr := '';
  IsolationLevel := False;

  { Prepare transaction parameters string }
  for I := 0 to Params.Count - 1 do
  begin
    ParamNo := GetInterbase6TransactionParamNumber(Params.Strings[I]);

    case ParamNo of
      0: Continue;
      isc_tpb_lock_read, isc_tpb_lock_write:
        begin
          ParamValue := {$IFDEF UNICODE}RawbyteString{$ENDIF}(Params.Strings[I]);
          TempStr := TempStr + AnsiChar(ParamNo) + AnsiChar(Length(ParamValue)) + ParamValue;
          Inc(TPBLength, Length(ParamValue) + 2);
        end;
      else
        begin
          TempStr := TempStr + AnsiChar(ParamNo);
          Inc(TPBLength, 1);
        end;
    end;

    { Check what was set use transaction isolation level }
    if not IsolationLevel then
      case ParamNo of
        isc_tpb_concurrency, isc_tpb_consistency,
        isc_tpb_read_committed:
          IsolationLevel := True
        else
          IsolationLevel := False;
      end;

  end;

   { Allocate transaction parameters PAnsiChar buffer
    if temporally parameters string is empty the set null pointer for
    default database transaction}
  if (TPBLength > 0) and (IsolationLevel) then
  begin
    {$IFDEF UNICODE}
    TPB := AnsiStrAlloc(TPBLength + 1);
    {$ELSE}
    TPB := StrAlloc(TPBLength + 1);
    {$ENDIF}
    TPB := {$IFDEF WITH_STRPCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrPCopy(TPB, TempStr);

  end
  else
    TPB := nil;

  { Allocate transaction structure }
  Result := AllocMem(SizeOf(TISC_TEB));
  with Result^ do
  begin
    db_handle := @Handle;
    tpb_length := TPBLength;
    tpb_address := TPB;
  end;
end;

{**
  Return interbase connection parameter number by it name
  @param Value - a connection parameter name
  @return - connection parameter number
}
function GetInterbase6DatabaseParamNumber(const Value: String): Word;
var
 I: Integer;
 ParamName: String;
begin
  ParamName := AnsiLowerCase(Value);
  Result := 0;
  if ZFastCode.Pos(BPBPrefix, ParamName) = 1 then
    for I := 1 to MAX_DPB_PARAMS do
    begin
      if ParamName = DatabaseParams[I].Name then
      begin
        Result := DatabaseParams[I].Number;
        Break;
      end;
    end;
end;

{**
  Return interbase transaction parameter number by it name
  @param Value - a transaction parameter name
  @return - transaction parameter number
}
function GetInterbase6TransactionParamNumber(const Value: String): Word;
var
 I: Integer;
 ParamName: String;
begin
  ParamName := AnsiLowerCase(Value);
  Result := 0;
  if ZFastCode.Pos(TPBPrefix, ParamName) = 1 then
    for I := 1 to MAX_TPB_PARAMS do
    begin
      if ParamName = TransactionParams[I].Name then
      begin
        Result := TransactionParams[I].Number;
        Break;
      end;
    end;
end;

{**
  Converts a Interbase6 native types into ZDBC SQL types.
  @param the interbase type
  @param the interbase subtype
  @return a SQL undepended type.

  <b>Note:</b> The interbase type and subtype get from RDB$TYPES table
}
function ConvertInterbase6ToSqlType(const SqlType, SqlSubType, Scale: Integer;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
label numeric;
begin
  Result := ZDbcIntfs.stUnknown;

  case SqlType of
    blr_bool, blr_not_nullable: Result := stBoolean;
    blr_varying2, blr_varying, blr_cstring, blr_cstring2, blr_domain_name,
    blr_domain_name2, blr_column_name, blr_column_name2:
      Result := stString;
    blr_text, blr_text2:
      case SqlSubType of
        CS_BINARY: Result := stBytes;
      else
        Result := stString;
      end;
    blr_d_float: Result := stDouble;
    blr_float: Result := stFloat;
    blr_double: Result := stDouble;
    blr_blob_id, blr_quad: Result := stLong;
    blr_int64:
      case SqlSubType of
        RDB_NUMBERS_NONE:
          { weired bug! We need to check scale too!
            see: http://sourceforge.net/p/zeoslib/tickets/106/ }
          if Scale = 0 then
            Result := stLong
          else
            goto numeric;
        RDB_NUMBERS_NUMERIC: Result := stDouble;
        RDB_NUMBERS_DECIMAL:
          if Scale = 0 then
            Result := stLong
          else
            if Abs(Scale) <= 4 then
              Result := stCurrency
            else
              Result := stBigDecimal;
      end;
    blr_long:
      case SqlSubType of
        RDB_NUMBERS_NONE: Result := stInteger;
        RDB_NUMBERS_NUMERIC: Result := stDouble;
        RDB_NUMBERS_DECIMAL:
          if Scale = 0 then
            Result := stInteger
          else
numeric:    if Abs(Scale) <= 4 then
              Result := stCurrency
            else
              Result := stBigDecimal;
      end;
    blr_short:
      case SqlSubType of
        RDB_NUMBERS_NONE: Result := stSmall;
        RDB_NUMBERS_NUMERIC: Result := stDouble;
        RDB_NUMBERS_DECIMAL: Result := stDouble;
      end;
    blr_sql_date: Result := stDate;
    blr_sql_time: Result := stTime;
    blr_timestamp: Result := stTimestamp;
    blr_blob, blr_blob2:
      case SqlSubType of
        { Blob Subtypes }
        { types less than zero are reserved for customer use }
        isc_blob_untyped: Result := stBinaryStream;

        { internal subtypes }
        isc_blob_text: Result := stAsciiStream;
        isc_blob_blr: Result := stBinaryStream;
        isc_blob_acl: Result := stAsciiStream;
        isc_blob_ranges: Result := stBinaryStream;
        isc_blob_summary: Result := stBinaryStream;
        isc_blob_format: Result := stAsciiStream;
        isc_blob_tra: Result := stAsciiStream;
        isc_blob_extfile: Result := stAsciiStream;
        isc_blob_debug_info: Result := stBinaryStream;
        else //http://sourceforge.net/p/zeoslib/tickets/111/
          Result := stBinaryStream;
      end;
    else
      Result := ZDbcIntfs.stUnknown;
  end;
  if ( CtrlsCPType = cCP_UTF16) then
    case result of
      stString: Result := stUnicodeString;
      stAsciiStream: Result := stUnicodeStream;
    end;
end;

{**
   Return Interbase SqlType by it number
   @param Value the SqlType number
}
function GetNameSqlType(Value: Word): RawByteString;
begin
  case Value of
    SQL_VARYING: Result := 'SQL_VARYING';
    SQL_TEXT: Result := 'SQL_TEXT';
    SQL_DOUBLE: Result := 'SQL_DOUBLE';
    SQL_FLOAT: Result := 'SQL_FLOAT';
    SQL_LONG: Result := 'SQL_LONG';
    SQL_SHORT: Result := 'SQL_SHORT';
    SQL_TIMESTAMP: Result := 'SQL_TIMESTAMP';
    SQL_BLOB: Result := 'SQL_BLOB';
    SQL_D_FLOAT: Result := 'SQL_D_FLOAT';
    SQL_ARRAY: Result := 'SQL_ARRAY';
    SQL_QUAD: Result := 'SQL_QUAD';
    SQL_TYPE_TIME: Result := 'SQL_TYPE_TIME';
    SQL_TYPE_DATE: Result := 'SQL_TYPE_DATE';
    SQL_INT64: Result := 'SQL_INT64';
    SQL_BOOLEAN: Result := 'SQL_BOOLEAN';
  else
    Result := 'Unknown';
  end
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a Interbase Plain drver
  @param StatusVector a status vector. It contain information about error
  @param Sql a sql query commend

  @Param Integer Return is the ErrorCode that happened - for disconnecting the database
}
function CheckInterbase6Error(const PlainDriver: IZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS; const ConSettings: PZConSettings;
  const LoggingCategory: TZLoggingCategory = lcOther;
  SQL: RawByteString = '') : Integer;
var
  Msg: array[0..1024] of AnsiChar;
  PStatusVector: PISC_STATUS;
  ErrorMessage, ErrorSqlMessage: RawByteString;
  ErrorCode: LongInt;
begin
  Result := 0;
  if (StatusVector[0] = 1) and (StatusVector[1] > 0) then
  begin
    ErrorMessage := '';
    PStatusVector := @StatusVector;
    while PlainDriver.isc_interprete(Msg, @PStatusVector) > 0 do
      ErrorMessage := ErrorMessage + ' ' + Msg;

    ErrorCode := PlainDriver.isc_sqlcode(@StatusVector);
    PlainDriver.isc_sql_interprete(ErrorCode, Msg, 1024);
    ErrorSqlMessage := Msg;

    if SQL <> '' then
      SQL := ' The SQL: '+SQL+'; ';

    if ErrorMessage <> '' then
    begin
      DriverManager.LogError(LoggingCategory, ConSettings^.Protocol,
        ErrorMessage, ErrorCode, ErrorSqlMessage + SQL);

      raise EZSQLException.CreateWithCode(ErrorCode,
        ConSettings^.ConvFuncs.ZRawToString('SQL Error: '+ErrorMessage+'. Error Code: '+IntToRaw(ErrorCode)+
        '. '+ErrorSqlMessage + SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
    end;
  end;
end;

{**
   Prepare statement and create statement handle.
   @param PlainDriver a interbase plain driver
   @param Handle a interbase connection handle
   @param TrHandle a transaction handle
   @param Dialect a interbase sql dialect number
   @param Sql a sql query
   @param ConSettings the connection settings
   @param StmtHandle a statement handle
   @return sql statement type
}
function PrepareStatement(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TrHandle: PISC_TR_HANDLE;
  const Dialect: Word; const SQL: RawByteString; const ConSettings: PZConSettings;
  var StmtHandle: TISC_STMT_HANDLE): TZIbSqlStatementType;
var
  StatusVector: TARRAY_ISC_STATUS;
  iError : Integer; //Error for disconnect
begin
  { Allocate an sql statement }
  if StmtHandle = 0 then
  begin
    PlainDriver.isc_dsql_allocate_statement(@StatusVector, Handle, @StmtHandle);
    CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcOther, SQL);
  end;
  { Prepare an sql statement }
  PlainDriver.isc_dsql_prepare(@StatusVector, TrHandle, @StmtHandle,
    Length(SQL), Pointer(SQL), Dialect, nil);

  iError := CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcPrepStmt, SQL); //Check for disconnect AVZ

  { Set Statement Type }
  if (iError <> DISCONNECT_ERROR) then //AVZ
    Result := GetStatementType(PlainDriver, StmtHandle, ConSettings)
  else
    Result := stDisconnect;

  if Result in [stUnknown, stGetSegment, stPutSegment, stStartTrans] then
  begin
    FreeStatement(PlainDriver, StmtHandle, DSQL_CLOSE);  //AVZ
    raise EZSQLException.Create(SStatementIsNotAllowed);
  end;
end;

{**
   Describe SQLDA and allocate memory for result values.
   @param PlainDriver a interbase plain driver
   @param Handle a interbase connection handle
   @param Dialect a interbase sql dialect number
   @param Sql a sql query
   @param StmtHandle a statement handle
   @param SqlData a interbase sql result data
}
procedure PrepareResultSqlData(const PlainDriver: IZInterbasePlainDriver;
  const Dialect: Word; const SQL: RawByteString;
  var StmtHandle: TISC_STMT_HANDLE; const SqlData: IZSQLDA;
  const ConSettings: PZConSettings);
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  { Initialise ouput param and fields }
  PlainDriver.isc_dsql_describe(@StatusVector, @StmtHandle, Dialect,
    SqlData.GetData);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcExecute, SQL);

  if SqlData.GetData^.sqld > SqlData.GetData^.sqln then
  begin
    SqlData.AllocateSQLDA;
    PlainDriver.isc_dsql_describe(@StatusVector, @StmtHandle,
      Dialect, SqlData.GetData);
    CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcExecute, Sql);
  end;
  SqlData.InitFields(False);
end;

{**
   Return interbase statement type by statement handle
   @param PlainDriver a interbase plain driver
   @param StmtHandle a statement handle
   @return interbase statement type
}
function GetStatementType(const PlainDriver: IZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const ConSettings: PZConSettings): TZIbSqlStatementType;
var
  TypeItem: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  StatementLength: integer;
  StatementBuffer: array[0..7] of AnsiChar;
begin
  Result := stUnknown;
  TypeItem := AnsiChar(isc_info_sql_stmt_type);

  { Get information about a prepared DSQL statement. }
  PlainDriver.isc_dsql_sql_info(@StatusVector, @StmtHandle, 1,
    @TypeItem, SizeOf(StatementBuffer), StatementBuffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);

  if StatementBuffer[0] = AnsiChar(isc_info_sql_stmt_type) then
  begin
    StatementLength := PlainDriver.isc_vax_integer(
      @StatementBuffer[1], 2);
    Result := TZIbSqlStatementType(PlainDriver.isc_vax_integer(
      @StatementBuffer[3], StatementLength));
  end;
end;

{**
   Free interbse allocated statement and SQLDA for input and utput parameters
   @param  the interbase plain driver
   @param  the interbse statement handle
}
procedure FreeStatement(PlainDriver: IZInterbasePlainDriver; StatementHandle: TISC_STMT_HANDLE; Options: Word);
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  if StatementHandle <> 0  then
    PlainDriver.isc_dsql_free_statement(@StatusVector, @StatementHandle, Options);
  //CheckInterbase6Error(PlainDriver, StatusVector); //raises an unwanted exception if Connection was reopened  See: http://sourceforge.net/p/zeoslib/tickets/40/
end;

{**
   Get affected rows.
   <i>Note:<i> it function may call after statement execution
   @param PlainDriver a interbase plain driver
   @param StmtHandle a statement handle
   @param StatementType a statement type
   @return affected rows
}
function GetAffectedRows(const PlainDriver: IZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const StatementType: TZIbSqlStatementType;
  const ConSettings: PZConSettings): integer;
var
  ReqInfo: AnsiChar;
  OutBuffer: array[0..255] of AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
begin
  Result := -1;
  ReqInfo := AnsiChar(isc_info_sql_records);

  if PlainDriver.isc_dsql_sql_info(@StatusVector, @StmtHandle, 1,
    @ReqInfo, SizeOf(OutBuffer), OutBuffer) > 0 then
    Exit;
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  if OutBuffer[0] = AnsiChar(isc_info_sql_records) then
  begin
    case StatementType of
      stUpdate: Result := PlainDriver.isc_vax_integer(@OutBuffer[6], 4);
      stDelete: Result := PlainDriver.isc_vax_integer(@OutBuffer[13], 4);
      stSelect: Result := PlainDriver.isc_vax_integer(@OutBuffer[20], 4);
      stInsert: Result := PlainDriver.isc_vax_integer(@OutBuffer[27], 4);
    else
       Result := -1;
    end;
  end;
end;

{**
   Prepare sql statement parameters and fill parameters by values
   @param PlainDriver a interbase plain driver
   @param Dialect a interbase sql dialect number
   @param StmtHandle a statement handle
   @param SqlData a interbase sql result data
}
procedure PrepareParameters(const PlainDriver: IZInterbasePlainDriver;
  const SQL: RawByteString; const Dialect: Word; var StmtHandle: TISC_STMT_HANDLE;
  const ParamSqlData: IZParamsSQLDA; const ConSettings: PZConSettings);
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  {check dynamic sql}
  PlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, Dialect,
    ParamSqlData.GetData);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcExecute, SQL);

  { Resize XSQLDA structure if needed }
  if ParamSqlData.GetData^.sqld > ParamSqlData.GetData^.sqln then
  begin
    ParamSqlData.AllocateSQLDA;
    PlainDriver.isc_dsql_describe_bind(@StatusVector, @StmtHandle, Dialect,
      ParamSqlData.GetData);
    CheckInterbase6Error(PlainDriver, StatusVector, ConSettings, lcExecute, SQL);
  end;

  ParamSqlData.InitFields(True);
end;

procedure BindSQLDAInParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const InParamTypes: TZSQLTypeArray;
  const InParamCount: Integer; const ParamSqlData: IZParamsSQLDA;
  const ConSettings: PZConSettings; const CodePageArray: TWordDynArray);
var
  I, CP: Integer;
  TempBlob: IZBlob;
  Buffer: Pointer;
  Len: Integer;
  RawTemp: RawByteString;
  CharRec: TZCharRec;
begin
  {$R-}
  if InParamCount <> ParamSqlData.GetFieldCount then
    raise EZSQLException.Create(SInvalidInputParameterCount);
  for I := 0 to ParamSqlData.GetFieldCount - 1 do
  begin
    ParamSqlData.UpdateNull(I, SoftVarManager.IsNull(InParamValues[I]));
    if SoftVarManager.IsNull(InParamValues[I])then
      Continue
    else
    case InParamTypes[I] of
      stBoolean:
        ParamSqlData.UpdateBoolean(I,
          ClientVarManager.GetAsBoolean(InParamValues[I]));
      stByte, stShort, stSmall:
        ParamSqlData.UpdateSmall(I,
          ClientVarManager.GetAsInteger(InParamValues[I]));
      stWord, stInteger:
        ParamSqlData.UpdateInt(I,
          ClientVarManager.GetAsInteger(InParamValues[I]));
      stLongWord, stLong, stULong:
        ParamSqlData.UpdateLong(I,
          ClientVarManager.GetAsInteger(InParamValues[I]));
      stFloat:
        ParamSqlData.UpdateFloat(I,
          ClientVarManager.GetAsFloat(InParamValues[I]));
      stDouble:
        ParamSqlData.UpdateDouble(I,
          ClientVarManager.GetAsFloat(InParamValues[I]));
      stBigDecimal, stCurrency:
        ParamSqlData.UpdateBigDecimal(I,
          ClientVarManager.GetAsFloat(InParamValues[I]));
      stString, stUnicodeString:
        begin
          CP := ParamSqlData.GetIbSqlType(I);
          case CP of
            SQL_TEXT, SQL_VARYING:
              begin
                CP := ParamSqlData.GetIbSqlSubType(I);  //get code page
                if CP > High(CodePageArray) then
                  CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP)
                else
                  CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], CodePageArray[CP]);
              end;
            else
              CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP)
          end;
          ParamSqlData.UpdatePAnsiChar(I, CharRec.P, CharRec.Len);
        end;
      stBytes:
        ParamSqlData.UpdateBytes(I, ClientVarManager.GetAsBytes(InParamValues[I]));
      stDate:
        ParamSqlData.UpdateDate(I,
          ClientVarManager.GetAsDateTime(InParamValues[I]));
      stTime:
        ParamSqlData.UpdateTime(I,
          ClientVarManager.GetAsDateTime(InParamValues[I]));
      stTimestamp:
        ParamSqlData.UpdateTimestamp(I,
          ClientVarManager.GetAsDateTime(InParamValues[I]));
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream:
        begin
          TempBlob := SoftVarManager.GetAsInterface(InParamValues[I]) as IZBlob;
          if not TempBlob.IsEmpty then
          begin
            if (ParamSqlData.GetFieldSqlType(i) in [stUnicodeStream, stAsciiStream] ) then
              if TempBlob.IsClob then
              begin
                Buffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                Len := TempBlob.Length;
              end
              else
              begin
                RawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                Len := Length(RawTemp);
                if Len = 0 then
                  Buffer := PEmptyAnsiString
                else
                  Buffer := @RawTemp[1];
              end
            else
            begin
              Buffer := TempBlob.GetBuffer;
              Len := TempBlob.Length;
            end;
            if Buffer <> nil then
              ParamSqlData.WriteLobBuffer(i, Buffer, Len);
          end;
        end
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
    end;
  end;
   {$IFOPT D+}
  {$ENDIF}
end;

procedure BindSQLDAInParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray;  const InParamCount: Integer;
  const ParamSqlData: IZParamsSQLDA; const ConSettings: PZConSettings;
  const CodePageArray: TWordDynArray; ArrayOffSet, ArrayItersCount: Integer);
var
  I, J, ParamIndex, CP: Integer;
  TempBlob: IZBlob;
  Buffer: Pointer;
  Len: Integer;
  RawTemp: RawByteString;
  CharRec: TZCharRec;
  Value: TZVariant;
  IsNull: Boolean;

  { array DML bindings }
  ZData: Pointer; //array entry
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;
  label ProcString;
begin
  ParamIndex := 0;
  for J := ArrayOffSet to ArrayOffSet+ArrayItersCount-1 do
    for i := 0 to InParamCount -1 do
    begin
      ZData := InParamValues[I].VArray.VIsNullArray;
      if (ZData = nil) then
        IsNull := False
      else
        case TZSQLType(InParamValues[I].VArray.VIsNullArrayType) of
          stBoolean: IsNull := ZBooleanArray[J];
          stByte: IsNull := ZByteArray[J] <> 0;
          stShort: IsNull := ZShortIntArray[J] <> 0;
          stWord: IsNull := ZWordArray[J] <> 0;
          stSmall: IsNull := ZSmallIntArray[J] <> 0;
          stLongWord: IsNull := ZLongWordArray[J] <> 0;
          stInteger: IsNull := ZIntegerArray[J] <> 0;
          stLong: IsNull := ZInt64Array[J] <> 0;
          stULong: IsNull := ZUInt64Array[J] <> 0;
          stFloat: IsNull := ZSingleArray[J] <> 0;
          stDouble: IsNull := ZDoubleArray[J] <> 0;
          stCurrency: IsNull := ZCurrencyArray[J] <> 0;
          stBigDecimal: IsNull := ZExtendedArray[J] <> 0;
          stGUID:
            IsNull := True;
          stString, stUnicodeString:
            begin
              case InParamValues[i].VArray.VIsNullArrayVariantType of
                vtString: IsNull := StrToBoolEx(ZStringArray[j]);
                vtAnsiString: IsNull := StrToBoolEx(ZAnsiStringArray[j]);
                vtUTF8String: IsNull := StrToBoolEx(ZUTF8StringArray[j]);
                vtRawByteString: IsNull := StrToBoolEx(ZRawByteStringArray[j]);
                vtUnicodeString: IsNull := StrToBoolEx(ZUnicodeStringArray[j]);
                vtCharRec:
                  if ZCompatibleCodePages(ZCharRecArray[j].CP, zCP_UTF16) then
                    IsNull := StrToBoolEx(PWideChar(ZCharRecArray[j].P))
                  else
                    IsNull := StrToBoolEx(PAnsiChar(ZCharRecArray[j].P));
                vtNull: IsNull := True;
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
            end;
          stBytes:
            IsNull := ZBytesArray[j] = nil;
          stDate, stTime, stTimestamp:
            IsNull := ZDateTimeArray[j] <> 0;
          stAsciiStream,
          stUnicodeStream,
          stBinaryStream:
            IsNull := ZInterfaceArray[j] = nil;
          else
            raise EZIBConvertError.Create(SUnsupportedParameterType);
        end;

      ZData := InParamValues[I].VArray.VArray;
      if (ZData = nil) or (IsNull) then
        ParamSqlData.UpdateNull(ParamIndex, True)
      else
        case TZSQLType(InParamValues[I].VArray.VArrayType) of
          stBoolean: ParamSqlData.UpdateBoolean(ParamIndex, ZBooleanArray[J]);
          stByte: ParamSqlData.UpdateSmall(ParamIndex, ZByteArray[J]);
          stShort: ParamSqlData.UpdateSmall(ParamIndex, ZShortIntArray[J]);
          stWord: ParamSqlData.UpdateInt(ParamIndex, ZWordArray[J]);
          stSmall: ParamSqlData.UpdateSmall(ParamIndex, ZSmallIntArray[J]);
          stLongWord: ParamSqlData.UpdateLong(ParamIndex, ZLongWordArray[J]);
          stInteger: ParamSqlData.UpdateInt(ParamIndex, ZIntegerArray[J]);
          stLong: ParamSqlData.UpdateLong(ParamIndex, ZInt64Array[J]);
          stULong: ParamSqlData.UpdateLong(ParamIndex, ZUInt64Array[J]);
          stFloat: ParamSqlData.UpdateFloat(ParamIndex, ZSingleArray[J]);
          stDouble: ParamSqlData.UpdateDouble(ParamIndex, ZDoubleArray[J]);
          stCurrency: ParamSqlData.UpdateBigDecimal(ParamIndex, ZCurrencyArray[J]);
          stBigDecimal: ParamSqlData.UpdateBigDecimal(ParamIndex, ZExtendedArray[J]);
          stGUID:
            begin
              Value := EncodeRawByteString({$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(GUIDToString(ZGUIDArray[j])));
              goto ProcString;
            end;
          stString, stUnicodeString:
            begin
              case InParamValues[i].VArray.VArrayVariantType of
                vtString: Value := EncodeString(ZStringArray[j]);
                vtAnsiString: Value := EncodeAnsiString(ZAnsiStringArray[j]);
                vtUTF8String: Value := EncodeUTF8String(ZUTF8StringArray[j]);
                vtRawByteString: Value := EncodeRawByteString(ZRawByteStringArray[j]);
                vtUnicodeString: Value := EncodeUnicodeString(ZUnicodeStringArray[j]);
                vtCharRec: Value := EncodeCharRec(ZCharRecArray[j]);
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
ProcString:     CP := ParamSqlData.GetIbSqlType(ParamIndex);
              case CP of
                SQL_TEXT, SQL_VARYING:
                  begin
                    CP := ParamSqlData.GetIbSqlSubType(ParamIndex);  //get code page
                    if CP = CS_BINARY then
                      CharRec := ClientVarManager.GetAsCharRec(Value)
                    else
                      if CP > High(CodePageArray) then
                        CharRec := ClientVarManager.GetAsCharRec(Value, ConSettings^.ClientCodePage^.CP)
                      else
                        CharRec := ClientVarManager.GetAsCharRec(Value, CodePageArray[CP]);
                  end
                else
                  CharRec := ClientVarManager.GetAsCharRec(Value);
              end;
              ParamSqlData.UpdatePAnsiChar(ParamIndex, CharRec.P, CharRec.Len);
            end;
          stBytes:
            ParamSqlData.UpdateBytes(ParamIndex, ZBytesArray[j]);
          stDate:
            ParamSqlData.UpdateDate(ParamIndex, ZDateTimeArray[j]);
          stTime:
            ParamSqlData.UpdateTime(ParamIndex, ZDateTimeArray[j]);
          stTimestamp:
            ParamSqlData.UpdateTimestamp(ParamIndex, ZDateTimeArray[j]);
          stAsciiStream,
          stUnicodeStream,
          stBinaryStream:
            begin
              TempBlob := ZInterfaceArray[j] as IZBlob;
              if not TempBlob.IsEmpty then
              begin
                if (ParamSqlData.GetFieldSqlType(ParamIndex) in [stUnicodeStream, stAsciiStream] ) then
                  if TempBlob.IsClob then
                  begin
                    Buffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                    Len := TempBlob.Length;
                  end
                  else
                  begin
                    RawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                    Len := Length(RawTemp);
                    if Len = 0 then
                      Buffer := PEmptyAnsiString
                    else
                      Buffer := Pointer(RawTemp);
                  end
                else
                begin
                  Buffer := TempBlob.GetBuffer;
                  Len := TempBlob.Length;
                end;
                if Buffer <> nil then
                  ParamSqlData.WriteLobBuffer(ParamIndex, Buffer, Len);
              end;
            end
          else
            raise EZIBConvertError.Create(SUnsupportedParameterType);
        end;
      Inc(ParamIndex);
    end;
end;
{**
   Read blob information by it handle such as blob segment size, segments count,
   blob size and type.
   @param PlainDriver
   @param BlobInfo the blob information structure
}
procedure GetBlobInfo(const PlainDriver: IZInterbasePlainDriver;
  const BlobHandle: TISC_BLOB_HANDLE; var BlobInfo: TIbBlobInfo;
  const ConSettings: PZConSettings);
var
  Items: array[0..3] of AnsiChar;
  Results: array[0..99] of AnsiChar;
  I, ItemLength: Integer;
  Item: Integer;
  StatusVector: TARRAY_ISC_STATUS;
begin
  I := 0;
  Items[0] := AnsiChar(isc_info_blob_num_segments);
  Items[1] := AnsiChar(isc_info_blob_max_segment);
  Items[2] := AnsiChar(isc_info_blob_total_length);
  Items[3] := AnsiChar(isc_info_blob_type);

  if PlainDriver.isc_blob_info(@StatusVector, @BlobHandle, 4, @items[0],
    SizeOf(Results), @Results[0]) > 0 then
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);

  while (I < SizeOf(Results)) and (Results[I] <> AnsiChar(isc_info_end)) do
  begin
    Item := Integer(Results[I]);
    Inc(I);
    ItemLength := PlainDriver.isc_vax_integer(@results[I], 2);
    Inc(I, 2);
    case Item of
      isc_info_blob_num_segments:
        BlobInfo.NumSegments := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_max_segment:
        BlobInfo.MaxSegmentSize := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_total_length:
        BlobInfo.TotalSize := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
      isc_info_blob_type:
        BlobInfo.BlobType := PlainDriver.isc_vax_integer(@Results[I], ItemLength);
    end;
    Inc(i, ItemLength);
  end;
end;

{**
   Read blob field data to stream by it ISC_QUAD value
   Note: DefaultBlobSegmentSize constant used for limit segment size reading
   @param Handle the database connection handle
   @param TransactionHandle the transaction handle
   @param BlobId the ISC_QUAD structure
   @param Size the result buffer size
   @param Buffer the pointer to result buffer

   Note: Buffer must be nill. Function self allocate memory for data
    and return it size
}
procedure ReadBlobBufer(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TransactionHandle: PISC_TR_HANDLE;
  const BlobId: TISC_QUAD; var Size: Integer; var Buffer: Pointer;
  const Binary: Boolean; const ConSettings: PZConSettings);
var
  TempBuffer: PAnsiChar;
  BlobInfo: TIbBlobInfo;
  BlobSize, CurPos: LongInt;
  BytesRead, SegmentLenght: UShort;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
begin
  BlobHandle := 0;
  CurPos := 0;
//  SegmentLenght := UShort(DefaultBlobSegmentSize);

  { open blob }
  PlainDriver.isc_open_blob2(@StatusVector, Handle,
         TransactionHandle, @BlobHandle, @BlobId, 0 , nil);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);

  { get blob info }
  GetBlobInfo(PlainDriver, BlobHandle, BlobInfo{%H-}, ConSettings);
  BlobSize := BlobInfo.TotalSize;
  Size := BlobSize;

  SegmentLenght := BlobInfo.MaxSegmentSize;

  { Allocates a blob buffer }
  Buffer := AllocMem(BlobSize+Ord(not Binary)); //left space for leading #0 terminator

  TempBuffer := Buffer;

  { Copies data to blob buffer }
  while CurPos < BlobSize do
  begin
    if (CurPos + SegmentLenght > BlobSize) then
      SegmentLenght := BlobSize - CurPos;
    if not(PlainDriver.isc_get_segment(@StatusVector, @BlobHandle,
           @BytesRead, SegmentLenght, TempBuffer) = 0) or
          (StatusVector[1] <> isc_segment) then
      CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
    Inc(CurPos, BytesRead);
    Inc(TempBuffer, BytesRead);
    BytesRead := 0;
  end;
  if not Binary then
    (PAnsiChar(Buffer)+Size)^ := #0;

  { close blob handle }
  PlainDriver.isc_close_blob(@StatusVector, @BlobHandle);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
end;

{**
   Return interbase server version string
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @param isc_info a ISC_INFO_XXX number
   @param ConSettings then PZConSettings of active connection
   @return ISC_INFO string
}
function GetISC_StringInfo(const PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; ConSettings: PZConSettings): String;
var
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
  PlainDriver.isc_database_info(@StatusVector, Handle, 1, @isc_info,
    IBBigLocalBufferLength, Buffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  {$IFDEF UNICODE}
  Result := PRawToUnicode(PAnsiChar(@Buffer[5]),Integer(Buffer[4]), ConSettings^.ClientCodePage^.CP);
  {$ELSE}
  SetString(Result, PAnsiChar(@Buffer[5]),Integer(Buffer[4]));
  {$ENDIF}
  //Buffer[5 + Integer(Buffer[4])] := #0;
  //result := ConSettings^.ConvFuncs.ZRawToString(PAnsiChar(@Buffer[5]), ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
end;

{**
   Return interbase server version string
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @param isc_info a ISC_INFO_XXX number
   @param ConSettings then PZConSettings of active connection
   @return ISC_INFO Integer
}
function GetFB_ISC_IntegerInfo(const PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; ConSettings: PZConSettings): LongInt;
var
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..63] of AnsiChar;
  Len: Integer;
begin
  PlainDriver.isc_database_info(@StatusVector, Handle, 1, @isc_info,
    IBLocalBufferLength, Buffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  Len := Integer(PlainDriver.isc_portable_integer(@Buffer[1], 2));
  Result := Integer(PlainDriver.isc_portable_integer(@Buffer[3], Smallint(Len)));
end;

{**
   Return interbase database implementation
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @return interbase database implementation
}
function GetDBImplementationNo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): LongInt;
var
  DatabaseInfoCommand: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
  DatabaseInfoCommand := AnsiChar(isc_info_implementation);
  PlainDriver.isc_database_info(@StatusVector, Handle, 1, @DatabaseInfoCommand,
    IBLocalBufferLength, Buffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  result := PlainDriver.isc_vax_integer(@Buffer[3], 1);
end;

{**
   Return interbase database implementation class
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @return interbase database implementation class
}
function GetDBImplementationClass(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): LongInt;
var
  DatabaseInfoCommand: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
  DatabaseInfoCommand := AnsiChar(isc_info_implementation);
  PlainDriver.isc_database_info(@StatusVector, Handle, 1, @DatabaseInfoCommand,
    IBLocalBufferLength, Buffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  result := PlainDriver.isc_vax_integer(@Buffer[4], 1);
end;

{**
   Return interbase database info
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @param DatabaseInfoCommand a database information command
   @return interbase database info
}
function GetLongDbInfo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const DatabaseInfoCommand: Integer;
  const ConSettings: PZConSettings): LongInt;
var
  Length: Integer;
  DatabaseInfoCommand1: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
  DatabaseInfoCommand1 := AnsiChar(DatabaseInfoCommand);
  PlainDriver.isc_database_info(@StatusVector, Handle, 1, @DatabaseInfoCommand1,
    IBLocalBufferLength, Buffer);
  CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
  Length := PlainDriver.isc_vax_integer(@Buffer[1], 2);
  Result := PlainDriver.isc_vax_integer(@Buffer[4], Length);
end;

{**
   Return interbase database info string
   @param PlainDriver a interbase plain driver
   @param Handle a database connection handle
   @param DatabaseInfoCommand a database information command
   @return interbase database info string
}
function GetStringDbInfo(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const DatabaseInfoCommand: Integer;
  const ConSettings: PZConSettings): AnsiString;
var
  DatabaseInfoCommand1: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
   DatabaseInfoCommand1 := AnsiChar(DatabaseInfoCommand);
   PlainDriver.isc_database_info(@StatusVector, Handle, 1, @DatabaseInfoCommand1,
     IBLocalBufferLength, Buffer);
   CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
   Buffer[4 + Integer(Buffer[3])] := #0;
   Result := AnsiString(PAnsiChar(@Buffer[4]));
end;

{**
   Return interbase database dialect
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @return interbase database dialect
}
function GetDBSQLDialect(const PlainDriver: IZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ConSettings: PZConSettings): Integer;
var
  Length: Integer;
  DatabaseInfoCommand1: AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
   DatabaseInfoCommand1 := AnsiChar(isc_info_db_SQL_Dialect);
   PlainDriver.isc_database_info(@StatusVector, Handle, 1, @DatabaseInfoCommand1,
     IBLocalBufferLength, Buffer);
   CheckInterbase6Error(PlainDriver, StatusVector, ConSettings);
   if (Buffer[0] <> AnsiChar(isc_info_db_SQL_dialect)) then
     Result := 1
   else
   begin
     Length := PlainDriver.isc_vax_integer(@Buffer[1], 2);
     Result := PlainDriver.isc_vax_integer(@Buffer[3], Length);
   end;
end;

{ TSQLDA }
constructor TZSQLDA.Create(PlainDriver: IZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; TransactionHandle: PISC_TR_HANDLE;
  ConSettings: PZConSettings);
begin
  Self.ConSettings := ConSettings;
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FTransactionHandle := TransactionHandle;

  GetMem(FXSQLDA, XSQLDA_LENGTH(0));
  FillChar(FXSQLDA^, XSQLDA_LENGTH(0), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  FXSQLDA.sqln := 0;
  FXSQLDA.sqld := 0;

  FXSQLDA.version := SQLDA_VERSION1;
end;

{**
   Free allocated memory and free object
}
destructor TZSQLDA.Destroy;
begin
  FreeParamtersValues;
  FreeMem(FXSQLDA);
  inherited Destroy;
end;
{**
   Allocate memory for SQLVar in SQLDA structure for every
   fields by it length.
}
procedure TZSQLDA.InitFields(Parameters: boolean);
var
  I: Integer;
  SqlVar: PXSQLVAR;
begin
  {$R-}
  for I := 0 to FXSQLDA.sqld - 1 do
  begin
    SqlVar := @FXSQLDA.SqlVar[I];
    FDecribedLengthArray[i] := SqlVar.sqllen;
    FDecribedScaleArray[i] := SqlVar.sqlscale;
    FDecribedTypeArray[i] := SqlVar.sqltype;
    case SqlVar.sqltype and (not 1) of
      SQL_BOOLEAN, SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_DATE,
      SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
      SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        IbReAlloc(SqlVar.sqldata, 0, Max(1, SqlVar.sqllen));
      SQL_VARYING:
        IbReAlloc(SqlVar.sqldata, 0, SqlVar.sqllen + 2)
    end;

    if Parameters then
    begin
      //This code used when allocated sqlind parameter for Param SQLDA
      SqlVar.sqltype := SqlVar.sqltype or 1;
      IbReAlloc(SqlVar.sqlind, 0, SizeOf(Short))
    end
    else
    begin
      //This code used when allocated sqlind parameter for Result SQLDA
      if (SqlVar.sqltype and 1) <> 0 then
        ReallocMem(SqlVar.sqlind, SizeOf(Short))
      else
        SqlVar.sqlind := nil;
    end;
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Clear allocated data for SQLDA paramters
}
procedure TZSQLDA.FreeParamtersValues;
var
  I: Integer;
  SqlVar: PXSQLVAR;
begin
  {$R-}
  for I := 0 to FXSQLDA.sqln - 1 do
  begin
    SqlVar := @FXSQLDA.SqlVar[I];
    FreeMem(SqlVar.sqldata);
    FreeMem(SqlVar.sqlind);
    SqlVar.sqldata := nil;
    SqlVar.sqlind := nil;
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Chech reange count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZSQLDA.CheckRange(const Index: Word);
begin
  Assert(Index < Word(FXSQLDA.sqln), 'Out of Range.');
end;

{**
   Return alias name for field
   @param Index the index fields
   @return the alias name
}
function TZSQLDA.GetFieldAliasName(const Index: Word): String;
begin
  CheckRange(Index);
  {$R-}
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FXSQLDA.sqlvar[Index].aliasname[0], FXSQLDA.sqlvar[Index].aliasname_length, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FXSQLDA.sqlvar[Index].aliasname[0]), FXSQLDA.sqlvar[Index].aliasname_length)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FXSQLDA.sqlvar[Index].aliasname[0],
        FXSQLDA.sqlvar[Index].aliasname_length, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
  {$ENDIF}
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Return pointer to SQLDA structure
}
function TZSQLDA.GetData: PXSQLDA;
begin
  result := FXSQLDA;
end;

{**
   Get fields count not allocated.
   @return fields count
}
function TZSQLDA.GetFieldCount: Integer;
begin
  Result := FXSQLDA.sqld;
end;

{**
   Return field index by it name
   @param Index the index fields
   @return the index field
}
function TZSQLDA.GetFieldIndex(const Name: AnsiString): Word;
begin
  {$R-}
  for Result := 0 to GetFieldCount - 1 do
    if FXSQLDA.sqlvar[Result].aliasname_length = Length(name) then
      if {$IFDEF WITH_STRLICOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrLIComp(@FXSQLDA.sqlvar[Result].aliasname, PAnsiChar(Name), FXSQLDA.sqlvar[Result].aliasname_length) = 0 then
        Exit;
  raise Exception.Create(Format(SFieldNotFound1, [name]));
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Return field length
   @param Index the index fields
   @return the field lenth
}
function TZSQLDA.GetFieldLength(const Index: Word): SmallInt;
begin
  Result := GetIbSqlLen(Index);
end;

{**
   Return field scale
   @param Index the index fields
   @return the field scale
}
function TZSQLDA.GetFieldScale(const Index: Word): integer;
begin
  CheckRange(Index);
  {$R-}
  Result := Abs(FXSQLDA.sqlvar[Index].sqlscale);
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Convert Interbase sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZSQLDA.GetFieldSqlType(const Index: Word): TZSQLType;
var
  SqlScale: Integer;
  SqlSubType: Integer;
begin
  SqlScale := GetFieldScale(Index);
  SqlSubType := GetIbSqlSubType(Index);

  case GetIbSqlType(Index) of
    SQL_VARYING, SQL_TEXT:
      case SqlSubType of
        1: {Octets} Result := stBytes;
        else
          Result := stString;
      end;
    SQL_LONG:
      begin
        if SqlScale = 0 then
          Result := stInteger
        else
          Result := stDouble;
      end;
    SQL_SHORT:
      begin
        if SqlScale = 0 then
          Result := stSmall
        else
          Result := stFloat; //Numeric with low precision
       end;
    SQL_FLOAT:
      Result := stFloat;
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_DATE: Result := stTimestamp;
    SQL_TYPE_TIME: Result := stTime;
    SQL_TYPE_DATE: Result := stDate;
    SQL_INT64:
      begin
        if SqlScale = 0 then
          Result := stLong
        else if SqlScale <= 4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
      end;
    SQL_QUAD, SQL_BLOB:
      begin
        if SqlSubType = isc_blob_text then
          Result := stAsciiStream
        else
          Result := stBinaryStream;
      end;
    SQL_ARRAY: Result := stArray;
  else
      Result := stString;
  end;
  if ( ConSettings.CPType = cCP_UTF16 ) then
    case result of
      stString: Result := stUnicodeString;
      stAsciiStream: Result := stUnicodeStream;
    end;
end;

{**
   Return own name for field
   @param Index the index fields
   @return the own name
}
function TZSQLDA.GetFieldOwnerName(const Index: Word): String;
begin
  CheckRange(Index);
  {$R-}
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FXSQLDA.sqlvar[Index].OwnName[0], FXSQLDA.sqlvar[Index].OwnName_length, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FXSQLDA.sqlvar[Index].OwnName[0]), FXSQLDA.sqlvar[Index].OwnName_length)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FXSQLDA.sqlvar[Index].OwnName[0],
        FXSQLDA.sqlvar[Index].OwnName_length, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
  {$ENDIF}
  {$IFOPT D+}
    {$R+}
{$ENDIF}
end;

{**
   Return real name for field
   @param Index the index fields
   @return the real name
}
function TZSQLDA.GetFieldRelationName(const Index: Word): String;
begin
  CheckRange(Index);
  {$R-}
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FXSQLDA.sqlvar[Index].RelName[0], FXSQLDA.sqlvar[Index].RelName_length, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FXSQLDA.sqlvar[Index].RelName[0]), FXSQLDA.sqlvar[Index].RelName_length)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FXSQLDA.sqlvar[Index].RelName[0],
        FXSQLDA.sqlvar[Index].RelName_length, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
  {$ENDIF}
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Get Interbase sql fields lenth
   @param Index the index fields
   @return Interbase sql fields lenth
}
function TZSQLDA.GetIbSqlLen(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$R-}
  result := FXSQLDA.sqlvar[Index].sqllen;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Return sql name for field
   @param Index the index fields
   @return the sql name
}
function TZSQLDA.GetFieldSqlName(const Index: Word): String;
begin
  CheckRange(Index);
  {$R-}
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FXSQLDA.sqlvar[Index].sqlname[0], FXSQLDA.sqlvar[Index].sqlname_length, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FXSQLDA.sqlvar[Index].sqlname[0]), FXSQLDA.sqlvar[Index].sqlname_length)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FXSQLDA.sqlvar[Index].sqlname[0],
        FXSQLDA.sqlvar[Index].sqlname_length, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
  {$ENDIF}
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Get Interbase subsql type
   @param Index the index fields
   @return the Interbase subsql
}
function TZSQLDA.GetIbSqlSubType(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$R-}
  result := FXSQLDA.sqlvar[Index].sqlsubtype;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Get Interbase sql type
   @param Index the index fields
   @return the interbase sql type
}
function TZSQLDA.GetIbSqlType(const Index: Word): Smallint;
begin
  CheckRange(Index);
  {$R-}
  result := FXSQLDA.sqlvar[Index].sqltype and not (1);
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Reallocate memory and fill memory by #0
   @param pointer to memory block
   @param old size of memory block
   @param new size of memory block
}
procedure TZSQLDA.IbReAlloc(var P; OldSize, NewSize: Integer);
begin
  ReallocMem(Pointer(P), NewSize);
  if NewSize > OldSize then
      Fillchar((PAnsiChar(P) + OldSize)^, NewSize - OldSize, #0);
end;

procedure TZSQLDA.SetFieldType(const Index: Word; Size: Integer; Code: Smallint;
  Scale: Smallint);
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    sqltype := Code;
    if Scale <= 0 then
      sqlscale := Scale;
    sqllen := Size;
    if (Size > 0) then
      IbReAlloc(sqldata, 0, Size)
    else
    begin
      FreeMem(sqldata);
      sqldata := nil;
    end;
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZSQLDA.IsBlob(const Index: Word): boolean;
begin
  CheckRange(Index);
  {$R-}
  result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_BLOB);
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if field nullable overwise false
}
function TZSQLDA.IsNullable(const Index: Word): boolean;
begin
  CheckRange(Index);
  {$R-}
  Result := FXSQLDA.sqlvar[Index].sqltype and 1 = 1
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
procedure TZSQLDA.AllocateSQLDA;
begin
  IbReAlloc(FXSQLDA, XSQLDA_LENGTH(FXSQLDA.sqln), XSQLDA_LENGTH(FXSQLDA.sqld));
  FXSQLDA.sqln := FXSQLDA.sqld;
  SetLength(FDecribedLengthArray, FXSQLDA.sqld);
  SetLength(FDecribedScaleArray, FXSQLDA.sqld);
  SetLength(FDecribedTypeArray, FXSQLDA.sqld);
end;

{ TParamsSQLDA }

{**
   Encode pascal string to Interbase paramter buffer
   @param Code the Interbase data type
   @param Index the index target filed
   @param Str the source string
}

procedure TZParamsSQLDA.EncodeString(const Code: Smallint; const Index: Word;
  const Str: RawByteString);
begin
  if Pointer(Str) = nil then //let's avoid RTL conversion!
    EncodePData(Code, Index, PEmptyAnsiString, 0)
  else
    EncodePData(Code, Index, Pointer(Str), {%H-}PLengthInt(NativeUInt(Str) - StringLenOffSet)^);
end;

procedure TZParamsSQLDA.EncodePData(const Code: Smallint; const Index: Word;
  const Value: PAnsiChar; const Len: LengthInt);
begin
  {$R-}
  //EH: Hint it seems we don't need a #0 term here, sqlen is the indicator
   with FXSQLDA.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        begin
          sqllen := Min(Len, FDecribedLengthArray[Index]);
          if Len > 0 then
            Move(Value^, sqldata^, sqllen);
        end;
      SQL_VARYING:
        begin
          PISC_VARYING(sqldata).strlen :=  Min(Len, FDecribedLengthArray[Index]);
          sqllen := Len+SizeOf(Short);
          if sqllen > 0 then
            Move(Value^, PISC_VARYING(sqldata).str, PISC_VARYING(sqldata).strlen);
        end;
    end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter BigDecimal value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBigDecimal(const Index: Integer; const Value: Extended);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);

  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);

    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin //http://code.google.com/p/fbclient/wiki/DatatypeMapping
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(Value * IBScaleDivisor[sqlscale], 0));
        SQL_LONG   : PInteger(sqldata)^  := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(Value * IBScaleDivisor[sqlscale], 0));
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(Value * IBScaleDivisor[sqlscale], 0));
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_INT64     : PInt64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Boolean value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBoolean(const Index: Integer; const Value: boolean);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);
    {if (sqlind <> nil) and (sqlind^ = -1) then Exit;}
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := ord(Value) * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := ord(Value);
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := ord(Value);
        SQL_LONG      : PInteger(sqldata)^ := ord(Value);
        SQL_FLOAT     : PSingle(sqldata)^ := ord(Value);
        SQL_BOOLEAN   : PWordBool(sqldata)^ := Value;
        SQL_BOOLEAN_FB: PByte(sqldata)^ := Ord(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := ord(Value);
        SQL_INT64     : PInt64(sqldata)^ := ord(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToRaw(ord(Value)));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToRaw(ord(Value)));
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateBytes(const Index: Integer; const Value: TBytes);
var
 SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);
    SQLCode := (sqltype and not(1));
    case SQLCode of
      SQL_TEXT      : EncodePData(SQL_TEXT, Index, Pointer(Value), Length(Value));
      SQL_VARYING   : EncodePData(SQL_VARYING, Index, Pointer(Value), Length(Value));
      SQL_LONG      : PInteger (sqldata)^ := Round(RawToFloat(PAnsiChar(Pointer(Value)), '.') * IBScaleDivisor[sqlscale]); //AVZ
      SQL_SHORT     : PSmallint(sqldata)^ := RawToInt(BytesToStr(Value));
      SQL_TYPE_DATE : EncodeString(SQL_DATE, Index, BytesToStr(Value));
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble (sqldata)^ := RawToFloat(PAnsiChar(Pointer(Value)), '.')  * IBScaleDivisor[sqlscale]; //AVZ
      SQL_FLOAT     : PSingle (sqldata)^ := RawToFloat(PAnsiChar(Pointer(Value)), '.') * IBScaleDivisor[sqlscale];  //AVZ
      SQL_INT64     : PInt64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloat(PAnsiChar(Pointer(Value)), '.') * IBScaleDivisor[sqlscale]); //AVZ - INT64 value was not recognized
      SQL_BLOB,
      SQL_QUAD      : WriteLobBuffer(Index, Pointer(Value), Length(Value));
    else
      raise EZIBConvertError.Create(SErrorConvertion);
    end;
    if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Date value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateDate(const Index: Integer; const Value: TDateTime);
begin
  SetFieldType(Index, sizeof(Integer), SQL_TYPE_DATE + 1, 0);
  UpdateDateTime(Index, Value);
end;

{**
   Set up parameter DateTime value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateDateTime(const Index: Integer;
  Value: TDateTime);
var
  y, m, d: word;
  hr, min, sec, msec: word;
  SQLCode: SmallInt;
  TmpDate: TCTimeStructure;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    DecodeDate(Value, y, m, d);
    DecodeTime(Value, hr, min, sec, msec);
    TmpDate.tm_year := y - 1900;
    TmpDate.tm_mon := m - 1;
    TmpDate.tm_mday := d;
    TmpDate.tm_hour := hr;
    TmpDate.tm_min := min;
    TmpDate.tm_sec := sec;
    TmpDate.tm_wday := 0;
    TmpDate.tm_yday := 0;
    TmpDate.tm_isdst := 0;

    {if (sqlind <> nil) and (sqlind^ = -1) then Exit;}
    SQLCode := (sqltype and not(1));

    case SQLCode of
      SQL_TYPE_DATE : FPlainDriver.isc_encode_sql_date(@TmpDate, PISC_DATE(sqldata));
      SQL_TYPE_TIME : begin
                        FPlainDriver.isc_encode_sql_time(@TmpDate, PISC_TIME(sqldata));
                        PISC_TIME(sqldata)^ := PISC_TIME(sqldata)^ {%H-}+ msec*10;
                      end;
      SQL_TIMESTAMP : begin
                        FPlainDriver.isc_encode_timestamp(@TmpDate,PISC_TIMESTAMP(sqldata));
                        PISC_TIMESTAMP(sqldata).timestamp_time :=PISC_TIMESTAMP(sqldata).timestamp_time {%H-}+ msec*10;
                      end;
      else
        raise EZIBConvertError.Create(SInvalidState);
    end;
    if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Double value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateDouble(const Index: Integer; const Value: Double);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  //SetFieldType(Index, sizeof(double), SQL_DOUBLE + 1, 0);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);

    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      //EH: Double is within Round(x: Real=Double) precision.
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Round(Value * IBScaleDivisor[sqlscale]);
        SQL_LONG   : PInteger(sqldata)^  := Round(Value * IBScaleDivisor[sqlscale]);
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Round(Value * IBScaleDivisor[sqlscale]);
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Round(Value);
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Round(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := Round(Value);
        SQL_INT64     : PInt64(sqldata)^ := Round(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
      if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Float value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateFloat(const Index: Integer; const Value: Single);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);

  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);

    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_LONG   : PInteger(sqldata)^  := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value * IBScaleDivisor[sqlscale]);
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
        SQL_D_FLOAT,
        SQL_FLOAT  : PSingle(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_SHORT     : PSmallint(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_INT64     : PInt64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, FloatToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, FloatToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
      if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter integer value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateInt(const Index: Integer; const Value: Integer);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
      if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Long value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateLong(const Index: integer; const Value: Int64);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);

    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN   : PSmallint(sqldata)^ := Value;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedDataType);
      end;
      if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter null value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateNull(const Index: Integer; const Value: boolean);
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
    if (sqlind <> nil) then
       sqlind^ := -Ord(Value);
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter PAnsiChar value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdatePAnsiChar(const Index: Integer; const Value: PAnsiChar; const Len: Cardinal);
var
 SQLCode: SmallInt;
 TempTimeStamp: TDateTime;
 Failed: Boolean;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);

    SQLCode := (sqltype and not(1));
    case SQLCode of
      SQL_TEXT      :
        begin
          sqllen := Min(Len, FDecribedLengthArray[Index]);
          Move(Value^, sqldata^, sqllen);
        end;
      SQL_VARYING   :
        begin
          PISC_VARYING(sqldata).strlen :=  Min(Len, FDecribedLengthArray[Index]);
          sqllen := Len+SizeOf(Short);
          Move(Value^, PISC_VARYING(sqldata).str, PISC_VARYING(sqldata).strlen);
        end;
      SQL_LONG      : PInteger (sqldata)^ := RawToIntDef(Value, 0);
      SQL_SHORT     : PSmallint (sqldata)^ := RawToIntDef(Value, 0);
      SQL_D_FLOAT,
      SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(sqldata)^);
      SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (sqldata)^);
      SQL_INT64     : PInt64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0)); //AVZ - INT64 value was not recognized
      SQL_BLOB, SQL_QUAD: WriteLobBuffer(Index, Value, Len);
      SQL_TYPE_DATE :
        begin
          if (Len = 0) or ((Value+2)^ = ':') then
            TempTimeStamp := 0
          else
            if Len = ConSettings^.WriteFormatSettings.DateFormatLen then
              TempTimeStamp := RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed{%H-})
            else
              TempTimeStamp := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
          UpdateDateTime(Index, TempTimeStamp);
        end;
      SQL_TYPE_TIME:
        begin
          if Len = 0 then
            TempTimeStamp := 0
          else
            if (Value+2)^ = ':' then //possible date if Len = 10 then
              TempTimeStamp := RawSQLTimeToDateTime(Value,Len, ConSettings^.WriteFormatSettings, Failed{%H-})
            else
              TempTimeStamp := Frac(RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed));
          UpdateDateTime(Index, TempTimeStamp);
        end;
      SQL_TIMESTAMP:
        begin
          if Len = 0 then
            TempTimeStamp := 0
          else
            if (Value+2)^ = ':' then
              TempTimeStamp := RawSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed{%H-})
            else
              if (ConSettings^.WriteFormatSettings.DateTimeFormatLen - Len) <= 4 then
                TempTimeStamp := RawSQLTimeStampToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed)
              else
                if (Value+4)^ = '-' then
                  TempTimeStamp := RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed{%H-})
                else
                  TempTimeStamp := RawSQLTimeToDateTime(Value, Len, ConSettings^.WriteFormatSettings, Failed);
          UpdateDateTime(Index, TempTimeStamp);
        end;
    else
      raise EZIBConvertError.Create(SErrorConvertion);
    end;
    if (sqlind <> nil) then
         sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Interbase QUAD value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateQuad(const Index: Word; const Value: TISC_QUAD);
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
    if not ((sqlind <> nil) and (sqlind^ = -1)) then
      case (sqltype and not(1)) of
        SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY:
          begin
            PISC_QUAD(sqldata)^ := Value;
            sqlind^ := 0; // not null
          end;
      else
        raise EZIBConvertError.Create(SErrorConvertion);
      end
    else
      if (sqlind <> nil) then
        case (sqltype and not(1)) of
          SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY:
            begin
              sqlind^ := 0; // not null
              PISC_QUAD(sqldata)^ := Value
            end
          else
            raise EZIBConvertError.Create(SErrorConvertion);
        end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

procedure TZParamsSQLDA.UpdateArray(const Index: Word; const Value; const SQLType: TZSQLType;
  const VariantType: TZVariantType = vtNull);
//var
  //SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, SizeOf(TISC_QUAD), SQL_ARRAY, 0);
    if (sqlind <> nil) and (sqlind^ = -1) then
       Exit;
//    SQLCode := (sqltype and not(1));
    (*
    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN:
                     begin
                       if FPlainDriver.GetProtocol <> 'interbase-7' then
                         raise EZIBConvertError.Create(SUnsupportedDataType);
                       PSmallint(sqldata)^ := Value;
                     end;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;*)
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
  {$IFOPT D+} {$R+} {$ENDIF}
end;
{**
   Set up parameter Byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateSmall(const Index: Integer; const Value: SmallInt);
var
  SQLCode: SmallInt;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  begin
    if not FDecribedTypeArray[Index] = sqltype then
      SetFieldType(Index, FDecribedLengthArray[Index], FDecribedTypeArray[Index], FDecribedScaleArray[Index]);
    SQLCode := (sqltype and not(1));

    if (sqlscale < 0)  then
    begin
      case SQLCode of
        SQL_SHORT  : PSmallInt(sqldata)^ := Value * IBScaleDivisor[sqlscale];
        SQL_LONG   : PInteger(sqldata)^  := Value * IBScaleDivisor[sqlscale];
        SQL_INT64,
        SQL_QUAD   : PInt64(sqldata)^    := Value * IBScaleDivisor[sqlscale];
        SQL_DOUBLE : PDouble(sqldata)^   := Value;
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
    end
    else
      case SQLCode of
        SQL_D_FLOAT,
        SQL_DOUBLE    : PDouble(sqldata)^   := Value;
        SQL_LONG      : PInteger(sqldata)^ := Value;
        SQL_FLOAT     : PSingle(sqldata)^ := Value;
        SQL_BOOLEAN:
                     begin
                       if FPlainDriver.GetProtocol <> 'interbase-7' then
                         raise EZIBConvertError.Create(SUnsupportedDataType);
                       PSmallint(sqldata)^ := Value;
                     end;
        SQL_SHORT     : PSmallint(sqldata)^ := Value;
        SQL_INT64     : PInt64(sqldata)^ := Value;
        SQL_TEXT      : EncodeString(SQL_TEXT, Index, IntToRaw(Value));
        SQL_VARYING   : EncodeString(SQL_VARYING, Index, IntToRaw(Value));
      else
        raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Set up parameter Time value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateTime(const Index: Integer; const Value: TDateTime);
begin
  SetFieldType(Index, sizeof(Cardinal), SQL_TYPE_TIME + 1, 0);
  UpdateDateTime(Index, Value);
end;

{**
   Set up parameter Timestamp value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZParamsSQLDA.UpdateTimestamp(const Index: Integer; const Value: TDateTime);
begin
  SetFieldType(Index, sizeof(TISC_QUAD), SQL_TIMESTAMP + 1, 0);
  UpdateDateTime(Index, Value);
end;

procedure TZParamsSQLDA.WriteLobBuffer(const Index: Integer;
  const Buffer: Pointer; const Len: Integer);
var
  BlobId: TISC_QUAD;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
  CurPos, SegLen: Integer;
begin
  BlobHandle := 0;

  { create blob handle }
  FPlainDriver.isc_create_blob2(@StatusVector, FHandle, FTransactionHandle,
    @BlobHandle, @BlobId, 0, nil);
  CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);

  { put data to blob }
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < Len) do
  begin
    if (CurPos + SegLen > Len) then
      SegLen := Len - CurPos;
    if FPlainDriver.isc_put_segment(@StatusVector, @BlobHandle, SegLen,
      {%H-}Pointer({%H-}NativeUInt(Buffer)+NativeUInt(CurPos))) > 0 then
      CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);
    Inc(CurPos, SegLen);
  end;

  { close blob handle }
  FPlainDriver.isc_close_blob(@StatusVector, @BlobHandle);
  CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);

  UpdateQuad(Index, BlobId);
end;

function GetExecuteBlockString(const ParamsSQLDA: IZParamsSQLDA;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: IZInterbasePlainDriver;
  var MemPerRow, PreparedRowsOfArray: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: LongWord): RawByteString;
const
  EBStart = AnsiString('EXECUTE BLOCK(');
  EBBegin =  AnsiString(')AS BEGIN'+LineEnding);
  EBSuspend =  AnsiString('SUSPEND;'+LineEnding); //required for RETURNING synatax
  EBEnd = AnsiString('end');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
var
  IndexName, ArrayName: RawByteString;
  I, j, BindCount, ParamIndex, ParamNameLen, SingleStmtLength, LastStmLen,
  HeaderLen, FullHeaderLen, StmtLength, StmtMem, NewParamCount:  Integer;
  CodePageInfo: PZCodePage;
  PStmts, PResult: PAnsiChar;
  ReturningFound: Boolean;

  procedure Put(const Args: array of RawByteString; var Dest: PAnsiChar);
  var I: Integer;
  begin
    for I := low(Args) to high(Args) do //Move data
    begin
      System.Move(Pointer(Args[i])^, Dest^, {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^);
      Inc(Dest, {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^);
    end;
  end;
  procedure AddParam(const Args: array of RawByteString; var Dest: RawByteString);
  var I: Integer;
  begin
    Dest := '';
    for I := low(Args) to high(Args) do //Calc String Length
      Dest := Dest + Args[i];
  end;
  function GetIntDigits(Value: Integer): Integer;
  begin
    if Value >= 10000 then
      if Value >= 1000000 then
        if Value >= 100000000 then
          Result := 9 + Ord(Value >= 1000000000)
        else
          Result := 7 + Ord(Value >= 10000000)
      else
        Result := 5 + Ord(Value >= 100000)
    else
      if Value >= 100 then
        Result := 3 + Ord(Value >= 1000)
      else
        Result := 1 + Ord(Value >= 10);
  end;
begin
  if Pointer(TypeTokens) = nil then
  begin
    BindCount := ParamsSQLDA.GetFieldCount;
    Assert(InParamCount=BindCount, 'ParamCount missmatch');
    SetLength(TypeTokens, BindCount);
    MemPerRow := 0;
    for ParamIndex := 0 to BindCount-1 do
    begin
      case ParamsSQLDA.GetIbSqlType(ParamIndex) and not (1) of
        SQL_VARYING:
          begin
            CodePageInfo := PlainDriver.ValidateCharEncoding(ParamsSQLDA.GetIbSqlSubType(ParamIndex));
            AddParam([' VARCHAR(', IntToRaw(ParamsSQLDA.GetIbSqlLen(ParamIndex) div CodePageInfo.CharWidth),
            ') CHARACTER SET ', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(CodePageInfo.Name), ' = ?' ], TypeTokens[ParamIndex]);
          end;
        SQL_TEXT:
          begin
            CodePageInfo := PlainDriver.ValidateCharEncoding(ParamsSQLDA.GetIbSqlSubType(ParamIndex));
            AddParam([' CHAR(', IntToRaw(ParamsSQLDA.GetIbSqlLen(ParamIndex) div CodePageInfo.CharWidth),
            ') CHARACTER SET ', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(CodePageInfo.Name), ' = ?' ], TypeTokens[ParamIndex]);
          end;
        SQL_DOUBLE, SQL_D_FLOAT:
           AddParam([' DOUBLE PRECISION = ?'], TypeTokens[ParamIndex]);
        SQL_FLOAT:
           AddParam([' FLOAT = ?'],TypeTokens[ParamIndex]);
        SQL_LONG:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' INTEGER = ?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(9,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'], TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(9', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)), ',', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'],TypeTokens[ParamIndex]);
        SQL_SHORT:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' SMALLINT = ?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(4,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(4', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)), ',', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'],TypeTokens[ParamIndex]);
        SQL_TIMESTAMP:
           AddParam([' TIMESTAMP = ?'],TypeTokens[ParamIndex]);
        SQL_BLOB:
          if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = isc_blob_text then
            AddParam([' BLOB  SUB_TYPE TEXT = ?'],TypeTokens[ParamIndex])
          else
            AddParam([' BLOB = ?'],TypeTokens[ParamIndex]);
        //SQL_ARRAY                      = 540;
        //SQL_QUAD                       = 550;
        SQL_TYPE_TIME:
           AddParam([' TIME = ?'],TypeTokens[ParamIndex]);
        SQL_TYPE_DATE:
           AddParam([' DATE = ?'],TypeTokens[ParamIndex]);
        SQL_INT64: // IB7
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' BIGINT = ?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(18,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(18,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),') = ?'],TypeTokens[ParamIndex]);
        SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
           AddParam([' BOOLEAN = ?'],TypeTokens[ParamIndex]);
        SQL_NULL{FB25}:
           AddParam([' CHAR(1) = ?'],TypeTokens[ParamIndex]);
      end;
      Inc(MemPerRow, ParamsSQLDA.GetFieldLength(ParamIndex) +
        2*Ord((ParamsSQLDA.GetIbSqlType(ParamIndex) and not (1)) = SQL_VARYING));
    end;
    Inc(MemPerRow, XSQLDA_LENGTH(InParamCount));
  end;
  {now let's calc length of stmt to know if we can bound all array data or if we need some more calls}
  StmtLength := 0;
  FullHeaderLen := 0;
  StmtMem := 0;
  ReturningFound := False;
  NewParamCount := 0;
  for J := 0 to RemainingArrayRows -1 do
  begin
    ParamIndex := 0;
    SingleStmtLength := 0;
    LastStmLen := StmtLength;
    HeaderLen := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do
    begin
      if IsParamIndexArray[i] then //calc Parameters size
      begin
        ParamNameLen := {P}1+GetIntDigits(ParamIndex)+1{_}+GetIntDigits(j);
        {inc header}
        Inc(HeaderLen, ParamNameLen+ {%H-}PLengthInt(NativeUInt(TypeTokens[ParamIndex]) - StringLenOffSet)^+Ord(not ((ParamIndex = 0) and (J=0))){,});
        {inc stmt}
        Inc(SingleStmtLength, 1+{:}ParamNameLen);
        Inc(ParamIndex);
      end
      else
      begin
        Inc(SingleStmtLength, {%H-}PLengthInt(NativeUInt(CurrentSQLTokens[i]) - StringLenOffSet)^);
        if not ReturningFound and (CurrentSQLTokens[i][1] in ['R', 'r']) then
        begin
          ReturningFound := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}UpperCase(CurrentSQLTokens[i]) = 'RETUNRING';
          Inc(StmtLength, Ord(ReturningFound)*Length(EBSuspend));
        end;
      end;
    end;
    Inc(SingleStmtLength, 1{;}+Length(LineEnding));
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    Inc(NewParamCount, InParamCount);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if (LongWord(StmtLength+LBlockLen) > 32*1024{32KB limited Also with FB3}) or
       (LongWord(StmtMem + MemPerRow) > XSQLDAMaxSize) or
      ((InitialStatementType <> stInsert) and (NewParamCount > 255)) then
    begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      Break;
    end
    else
    begin
      PreparedRowsOfArray := J;
      Inc(StmtMem, MemPerRow);
    end;
  end;
  {EH: now move our data to result ! ONE ALLOC ! of result (: }
  SetLength(Result, StmtLength+LBlockLen);
  PResult := Pointer(Result);
  Put([EBStart], PResult);
  PStmts := PResult + FullHeaderLen+Length(EBBegin);
  for J := 0 to PreparedRowsOfArray do
  begin
    ParamIndex := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do
    begin
      if IsParamIndexArray[i] then
      begin
        IndexName := IntToRaw(ParamIndex);
        ArrayName := IntToRaw(J);
        Put([':P', IndexName, '_', ArrayName], PStmts);
        if (ParamIndex = 0) and (J=0) then
          Put(['P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult)
        else
          Put([',P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult);
        Inc(ParamIndex);
      end
      else
        Put([CurrentSQLTokens[i]], PStmts);
    end;
    Put([';',LineEnding], PStmts);
  end;
  Put([EBBegin], PResult);
  if ReturningFound then
    Put([EBSuspend], PStmts);
  Put([EBEnd], PStmts);
  Inc(PreparedRowsOfArray);
end;

end.



