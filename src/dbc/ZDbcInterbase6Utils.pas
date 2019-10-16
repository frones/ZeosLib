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
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} Types,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZPlainFirebirdDriver, ZCompatibility,
  ZPlainFirebirdInterbaseConstants, ZDbcLogging, ZMessages,
  ZVariant, ZClasses, FmtBCD;

type
  { Interbase Statement Type }
  TZIbSqlStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProc, stStartTrans, stCommit,
    stRollback, stSelectForUpdate, stSetGenerator, stDisconnect);

  { Interbase Error Class}
  EZIBConvertError = class(Exception);

  { Full info about single Interbase status entry}
  TZIBStatus = record
    IBDataType: Integer; // one of isc_arg_* constants
    IBDataInt: Integer;  // int data (error code)
    IBDataStr: string;   // string data
    IBMessage: string;   // result of isc_interpret
    SQLCode: Integer;    // result of isc_sqlcode
    SQLMessage: string;  // result of isc_sql_interprete
  end;
  PZIBStatus = ^TZIBStatus;

  TZIBStatusVector = array of TZIBStatus;

  { IB/FB-specific data}
  TZIBSpecificData = class(TZExceptionSpecificData)
  protected
    FStatusVector: TZIBStatusVector;
    FSQL: string;
    FIBErrorCode: Integer;
    FIBStatusCode: String;
  public
    function Clone: TZExceptionSpecificData; override;

    property IBErrorCode: Integer read FIBErrorCode;
    property IBStatusCode: string read FIBStatusCode;
    property StatusVector: TZIBStatusVector read FStatusVector;
    property SQL: string read FSQL;
  end;

  { Interbase SQL Error Class}
  EZIBSQLException = class(EZSQLException)
  public
    constructor Create(const Msg: string; const StatusVector: TZIBStatusVector; const SQL: string);
  end;

  TZIbParamValueType = (
    pvtNotImpl,  // unsupported
    pvtNone,     // no value
    pvtByteZ,    // 1-byte int that always = 0 (value ignored)
    pvtNum,      // 1/2/4-byte int, depending on a value
    pvtString    // raw byte string
  );

  { Paparameter string name and it value}
  TZIbParam = record
    Name: String;
    ValueType: TZIbParamValueType;
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
    TotalSize: Integer;
  end;

  { Base interface for sqlda }
  IZSQLDA = interface
    ['{2D0D6029-B31C-4E39-89DC-D86D20437C35}']
    procedure InitFields(Fixed2VariableSize: Boolean);
    function AllocateSQLDA: PXSQLDA;
    procedure FreeParamtersValues;

    function GetData: PXSQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }

  TZSQLDA = class (TZCodePagedObject, IZSQLDA, IImmediatelyReleasable)
  private
    FXSQLDA: PXSQLDA;
    FPlainDriver: TZInterbasePlainDriver;
    FConnection: IZConnection;
    procedure CheckRange(const Index: Word); {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure IbReAlloc(var P; OldSize, NewSize: Integer);
  public
    constructor Create(const Connection: IZConnection);
    destructor Destroy; override;
    procedure InitFields(Fixed2VariableSize: Boolean);
    function AllocateSQLDA: PXSQLDA;
    procedure FreeParamtersValues;

    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetData: PXSQLDA;

    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
  end;

  { parameters interface sqlda}
  IZParamsSQLDA = interface(IZSQLDA)
    ['{D2C3D5E1-F3A6-4223-9A6E-3048B99A06C4}']
  end;

  { Parameters class for sqlda structure.
    It clas can only write data to parameters/fields }
  TZParamsSQLDA = class (TZSQLDA, IZParamsSQLDA);

{Interbase6 Connection Functions}
function GenerateDPB(PlainDriver: TZInterbasePlainDriver; Info: TStrings;
  ConSettings: PZConSettings; CP: Word): RawByteString;
function GenerateTPB(PlainDriver: TZInterbasePlainDriver; Params: TStrings;
  ConSettings: PZConSettings; CP: Word): RawByteString;
procedure GenerateTEB(PHandle: PISC_DB_HANDLE; const TPB: RawByteString; var TEB: TISC_TEB);
function GetInterbase6DatabaseParamNumber(const Value: String): word;
function GetInterbase6TransactionParamNumber(const Value: String): word;

{ Interbase6 errors functions }
function GetNameSqlType(Value: Word): RawByteString;
function InterpretInterbaseStatus(const PlainDriver: TZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS;
  const ConSettings: PZConSettings) : TZIBStatusVector;
procedure CheckInterbase6Error(const PlainDriver: TZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS; const ImmediatelyReleasable: IImmediatelyReleasable;
  const LoggingCategory: TZLoggingCategory = lcOther;
  const SQL: RawByteString = '');

{ Interbase information functions}
function GetDBStringInfo(const PlainDriver: TZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; const ImmediatelyReleasable: IImmediatelyReleasable): String;
function GetDBIntegerInfo(const PlainDriver: TZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; const ImmediatelyReleasable: IImmediatelyReleasable): Integer;
function GetDBSQLDialect(const PlainDriver: TZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ImmediatelyReleasable: IImmediatelyReleasable): Integer;

{ Interbase statement functions}
function GetAffectedRows(const PlainDriver: TZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const StatementType: TZIbSqlStatementType;
  const ImmediatelyReleasable: IImmediatelyReleasable): integer;

function ConvertInterbase6ToSqlType(SqlType, SqlSubType, Scale, Precision: Integer;
  const CtrlsCPType: TZControlsCodePage): TZSqlType;

{ interbase blob routines }
procedure GetBlobInfo(const PlainDriver: TZInterbasePlainDriver;
  const BlobHandle: TISC_BLOB_HANDLE; out BlobInfo: TIbBlobInfo;
  const ImmediatelyReleasable: IImmediatelyReleasable);
procedure ReadBlobBufer(const PlainDriver: TZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TransactionHandle: PISC_TR_HANDLE;
  const BlobId: TISC_QUAD; out Size: Integer; out Buffer: Pointer;
  const Binary: Boolean; const ImmediatelyReleasable: IImmediatelyReleasable);

function GetExecuteBlockString(const ParamsSQLDA: IZParamsSQLDA;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: TZInterbasePlainDriver;
  var MemPerRow, PreparedRowsOfArray, MaxRowsPerBatch: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: Cardinal): RawByteString;

const
  { Default Interbase blob size for reading }
  DefaultBlobSegmentSize = 16 * 1024;

  IBScaleDivisor: array[-18..0] of Int64 = (
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
    10000000,
    1000000,
    100000,
    10000,
    1000,
    100,
    10,
    1);

  { count database parameters }
  MAX_DPB_PARAMS = 90;
  { prefix database parameters names it used in paramters scann procedure }
  DPBPrefix = 'isc_dpb_';
  { list database parameters and their apropriate numbers }
  DatabaseParams: array [0..MAX_DPB_PARAMS-1] of TZIbParam =
  (
    (Name: 'isc_dpb_cdd_pathname';          ValueType: pvtNotImpl; Number: isc_dpb_cdd_pathname),
    (Name: 'isc_dpb_allocation';            ValueType: pvtNotImpl; Number: isc_dpb_allocation),
    (Name: 'isc_dpb_journal';               ValueType: pvtNotImpl; Number: isc_dpb_journal),
    (Name: 'isc_dpb_page_size';             ValueType: pvtNum;     Number: isc_dpb_page_size),
    (Name: 'isc_dpb_num_buffers';           ValueType: pvtNum;     Number: isc_dpb_num_buffers),
    (Name: 'isc_dpb_buffer_length';         ValueType: pvtNotImpl; Number: isc_dpb_buffer_length),
    (Name: 'isc_dpb_debug';                 ValueType: pvtNum;     Number: isc_dpb_debug),
    (Name: 'isc_dpb_garbage_collect';       ValueType: pvtNone;    Number: isc_dpb_garbage_collect),
    (Name: 'isc_dpb_verify';                ValueType: pvtNum;     Number: isc_dpb_verify),    // Bitmask
    (Name: 'isc_dpb_sweep';                 ValueType: pvtNum;     Number: isc_dpb_sweep),
    (Name: 'isc_dpb_enable_journal';        ValueType: pvtString;  Number: isc_dpb_enable_journal),
    (Name: 'isc_dpb_disable_journal';       ValueType: pvtNone;    Number: isc_dpb_disable_journal),
    (Name: 'isc_dpb_dbkey_scope';           ValueType: pvtNum;     Number: isc_dpb_dbkey_scope),
    (Name: 'isc_dpb_number_of_users';       ValueType: pvtNotImpl; Number: isc_dpb_number_of_users),
    (Name: 'isc_dpb_trace';                 ValueType: pvtNone;    Number: isc_dpb_trace),
    (Name: 'isc_dpb_no_garbage_collect';    ValueType: pvtNone;    Number: isc_dpb_no_garbage_collect),
    (Name: 'isc_dpb_damaged';               ValueType: pvtNum;     Number: isc_dpb_damaged),
    (Name: 'isc_dpb_license';               ValueType: pvtString;  Number: isc_dpb_license),
    (Name: 'isc_dpb_sys_user_name';         ValueType: pvtString;  Number: isc_dpb_sys_user_name),
    (Name: 'isc_dpb_encrypt_key';           ValueType: pvtString;  Number: isc_dpb_encrypt_key),
    (Name: 'isc_dpb_activate_shadow';       ValueType: pvtByteZ;   Number: isc_dpb_activate_shadow),
    (Name: 'isc_dpb_sweep_interval';        ValueType: pvtNum;     Number: isc_dpb_sweep_interval),
    (Name: 'isc_dpb_delete_shadow';         ValueType: pvtByteZ;   Number: isc_dpb_delete_shadow),
    (Name: 'isc_dpb_force_write';           ValueType: pvtNum;     Number: isc_dpb_force_write),
    (Name: 'isc_dpb_begin_log';             ValueType: pvtString;  Number: isc_dpb_begin_log),
    (Name: 'isc_dpb_quit_log';              ValueType: pvtNone;    Number: isc_dpb_quit_log),
    (Name: 'isc_dpb_no_reserve';            ValueType: pvtNum;     Number: isc_dpb_no_reserve),
    (Name: 'isc_dpb_username';              ValueType: pvtString;  Number: isc_dpb_user_name),
    (Name: 'isc_dpb_password';              ValueType: pvtString;  Number: isc_dpb_password),
    (Name: 'isc_dpb_password_enc';          ValueType: pvtString;  Number: isc_dpb_password_enc),
    (Name: 'isc_dpb_sys_user_name_enc';     ValueType: pvtString;  Number: isc_dpb_sys_user_name_enc),
    (Name: 'isc_dpb_interp';                ValueType: pvtNum;     Number: isc_dpb_interp),
    (Name: 'isc_dpb_online_dump';           ValueType: pvtNum;     Number: isc_dpb_online_dump),
    (Name: 'isc_dpb_old_file_size';         ValueType: pvtNum;     Number: isc_dpb_old_file_size),
    (Name: 'isc_dpb_old_num_files';         ValueType: pvtNum;     Number: isc_dpb_old_num_files),
    (Name: 'isc_dpb_old_file';              ValueType: pvtString;  Number: isc_dpb_old_file),
    (Name: 'isc_dpb_old_start_page';        ValueType: pvtNum;     Number: isc_dpb_old_start_page),
    (Name: 'isc_dpb_old_start_seqno';       ValueType: pvtNum;     Number: isc_dpb_old_start_seqno),
    (Name: 'isc_dpb_old_start_file';        ValueType: pvtNum;     Number: isc_dpb_old_start_file),
    (Name: 'isc_dpb_drop_walfile';          ValueType: pvtNum;     Number: isc_dpb_drop_walfile),
    (Name: 'isc_dpb_old_dump_id';           ValueType: pvtNum;     Number: isc_dpb_old_dump_id),
    (Name: 'isc_dpb_wal_backup_dir';        ValueType: pvtString;  Number: isc_dpb_wal_backup_dir),
    (Name: 'isc_dpb_wal_chkptlen';          ValueType: pvtNum;     Number: isc_dpb_wal_chkptlen),
    (Name: 'isc_dpb_wal_numbufs';           ValueType: pvtNum;     Number: isc_dpb_wal_numbufs),
    (Name: 'isc_dpb_wal_bufsize';           ValueType: pvtNum;     Number: isc_dpb_wal_bufsize),
    (Name: 'isc_dpb_wal_grp_cmt_wait';      ValueType: pvtNum;     Number: isc_dpb_wal_grp_cmt_wait),
    (Name: 'isc_dpb_lc_messages';           ValueType: pvtString;  Number: isc_dpb_lc_messages),
    (Name: 'isc_dpb_lc_ctype';              ValueType: pvtString;  Number: isc_dpb_lc_ctype),
    (Name: 'isc_dpb_cache_manager';         ValueType: pvtNotImpl; Number: isc_dpb_cache_manager),
    (Name: 'isc_dpb_shutdown';              ValueType: pvtNum;     Number: isc_dpb_shutdown), // Bitmask
    (Name: 'isc_dpb_online';                ValueType: pvtNone;    Number: isc_dpb_online),
    (Name: 'isc_dpb_shutdown_delay';        ValueType: pvtNum;     Number: isc_dpb_shutdown_delay),
    (Name: 'isc_dpb_reserved';              ValueType: pvtNone;    Number: isc_dpb_reserved),
    (Name: 'isc_dpb_overwrite';             ValueType: pvtNone;    Number: isc_dpb_overwrite),
    (Name: 'isc_dpb_sec_attach';            ValueType: pvtNone;    Number: isc_dpb_sec_attach),
    (Name: 'isc_dpb_disable_wal';           ValueType: pvtNone;    Number: isc_dpb_disable_wal),
    (Name: 'isc_dpb_connect_timeout';       ValueType: pvtNum;     Number: isc_dpb_connect_timeout),
    (Name: 'isc_dpb_dummy_packet_interval'; ValueType: pvtNum;     Number: isc_dpb_dummy_packet_interval),
    (Name: 'isc_dpb_gbak_attach';           ValueType: pvtNone;    Number: isc_dpb_gbak_attach),
    (Name: 'isc_dpb_sql_role_name';         ValueType: pvtString;  Number: isc_dpb_sql_role_name),
    (Name: 'isc_dpb_set_page_buffers';      ValueType: pvtNum;     Number: isc_dpb_set_page_buffers),
    (Name: 'isc_dpb_working_directory';     ValueType: pvtString;  Number: isc_dpb_working_directory),
    (Name: 'isc_dpb_sql_dialect';           ValueType: pvtNum;     Number: isc_dpb_SQL_dialect),
    (Name: 'isc_dpb_set_db_readonly';       ValueType: pvtNone;    Number: isc_dpb_set_db_readonly),
    (Name: 'isc_dpb_set_db_sql_dialect';    ValueType: pvtNum;     Number: isc_dpb_set_db_SQL_dialect),
    (Name: 'isc_dpb_gfix_attach';           ValueType: pvtNone;    Number: isc_dpb_gfix_attach),
    (Name: 'isc_dpb_gstat_attach';          ValueType: pvtNone;    Number: isc_dpb_gstat_attach),
    (Name: 'isc_dpb_set_db_charset';        ValueType: pvtString;  Number: isc_dpb_set_db_charset),
    (Name: 'isc_dpb_gsec_attach';           ValueType: pvtNone;    Number: isc_dpb_gsec_attach),
    (Name: 'isc_dpb_address_path';          ValueType: pvtString;  Number: isc_dpb_address_path),
    (Name: 'isc_dpb_process_id';            ValueType: pvtNum;     Number: isc_dpb_process_id),
    (Name: 'isc_dpb_no_db_triggers';        ValueType: pvtNone;    Number: isc_dpb_no_db_triggers),
    (Name: 'isc_dpb_trusted_auth';          ValueType: pvtNone;    Number: isc_dpb_trusted_auth),
    (Name: 'isc_dpb_process_name';          ValueType: pvtString;  Number: isc_dpb_process_name),
    (Name: 'isc_dpb_trusted_role';          ValueType: pvtString;  Number: isc_dpb_trusted_role),
    (Name: 'isc_dpb_org_filename';          ValueType: pvtString;  Number: isc_dpb_org_filename),
    (Name: 'isc_dpb_utf8_filename';         ValueType: pvtNone;    Number: isc_dpb_utf8_filename),
    (Name: 'isc_dpb_ext_call_depth';        ValueType: pvtNum;     Number: isc_dpb_ext_call_depth),
    (Name: 'isc_dpb_auth_block';            ValueType: pvtString; Number: isc_dpb_auth_block), // Bytes
    (Name: 'isc_dpb_client_version';        ValueType: pvtString; Number: isc_dpb_client_version),
    (Name: 'isc_dpb_remote_protocol';       ValueType: pvtString; Number: isc_dpb_remote_protocol),
    (Name: 'isc_dpb_host_name';             ValueType: pvtString; Number: isc_dpb_host_name),
    (Name: 'isc_dpb_os_user';               ValueType: pvtString; Number: isc_dpb_os_user),
    (Name: 'isc_dpb_specific_auth_data';    ValueType: pvtString; Number: isc_dpb_specific_auth_data),  
    (Name: 'isc_dpb_auth_plugin_list';      ValueType: pvtString; Number: isc_dpb_auth_plugin_list),  
    (Name: 'isc_dpb_auth_plugin_name';      ValueType: pvtString; Number: isc_dpb_auth_plugin_name),
    (Name: 'isc_dpb_config';                ValueType: pvtString; Number: isc_dpb_config),
    (Name: 'isc_dpb_nolinger';              ValueType: pvtNone; Number: isc_dpb_nolinger),
    (Name: 'isc_dpb_reset_icu';             ValueType: pvtNone; Number: isc_dpb_reset_icu),
    (Name: 'isc_dpb_map_attach';            ValueType: pvtNone; Number: isc_dpb_map_attach)
  );

  { count transaction parameters }
  MAX_TPB_PARAMS = 23;
  { prefix transaction parameters names it used in paramters scann procedure }
  TPBPrefix = 'isc_tpb_';
  { list transaction parameters and their apropriate numbers }
  TransactionParams: array [0..MAX_TPB_PARAMS-1] of TZIbParam =
  (
    (Name: 'isc_tpb_consistency';      ValueType: pvtNone;    Number: isc_tpb_consistency),
    (Name: 'isc_tpb_concurrency';      ValueType: pvtNone;    Number: isc_tpb_concurrency),
    (Name: 'isc_tpb_shared';           ValueType: pvtNone;    Number: isc_tpb_shared),
    (Name: 'isc_tpb_protected';        ValueType: pvtNone;    Number: isc_tpb_protected),
    (Name: 'isc_tpb_exclusive';        ValueType: pvtNone;    Number: isc_tpb_exclusive),
    (Name: 'isc_tpb_wait';             ValueType: pvtNone;    Number: isc_tpb_wait),
    (Name: 'isc_tpb_nowait';           ValueType: pvtNone;    Number: isc_tpb_nowait),
    (Name: 'isc_tpb_read';             ValueType: pvtNone;    Number: isc_tpb_read),
    (Name: 'isc_tpb_write';            ValueType: pvtNone;    Number: isc_tpb_write),
    (Name: 'isc_tpb_lock_read';        ValueType: pvtString;  Number: isc_tpb_lock_read),
    (Name: 'isc_tpb_lock_write';       ValueType: pvtString;  Number: isc_tpb_lock_write),
    (Name: 'isc_tpb_verb_time';        ValueType: pvtNotImpl; Number: isc_tpb_verb_time),
    (Name: 'isc_tpb_commit_time';      ValueType: pvtNotImpl; Number: isc_tpb_commit_time),
    (Name: 'isc_tpb_ignore_limbo';     ValueType: pvtNone;    Number: isc_tpb_ignore_limbo),
    (Name: 'isc_tpb_read_committed';   ValueType: pvtNone;    Number: isc_tpb_read_committed),
    (Name: 'isc_tpb_autocommit';       ValueType: pvtNone;    Number: isc_tpb_autocommit),
    (Name: 'isc_tpb_rec_version';      ValueType: pvtNone;    Number: isc_tpb_rec_version),
    (Name: 'isc_tpb_no_rec_version';   ValueType: pvtNone;    Number: isc_tpb_no_rec_version),
    (Name: 'isc_tpb_restart_requests'; ValueType: pvtNone;    Number: isc_tpb_restart_requests),
    (Name: 'isc_tpb_no_auto_undo';     ValueType: pvtNone;    Number: isc_tpb_no_auto_undo),
    // IB75+
    (Name: 'isc_tpb_no_savepoint';     ValueType: pvtNone;    Number: isc_tpb_no_savepoint),
    // FB20+
    (Name: 'isc_tpb_lock_timeout';     ValueType: pvtNum;     Number: isc_tpb_lock_timeout),
    // FB40+
    (Name: 'isc_tpb_read_consistency'; ValueType: pvtNone;    Number: isc_tpb_read_consistency)
  );

//ported  from NoThrowTimeStamp.cpp

procedure isc_decode_time(ntime: ISC_TIME; out hours, minutes, seconds: Word; out fractions: Cardinal);
procedure isc_encode_time(var ntime: ISC_TIME; hours, minutes, seconds: Word; fractions: Cardinal);
procedure isc_decode_date(nday: ISC_DATE; out year, month, day: Word);
procedure isc_encode_date(out nday: ISC_DATE; year, month, day: Word);

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes]
  and increments buffer pointer skipping read data.
  @param PlainDriver a Interbase Plain drver
  @param pBuf - pointer to a buffer returned by driver. After the function it points to the next block.
  @return - a number read
}
function ReadInterbase6NumberWithInc(const PlainDriver: TZInterbasePlainDriver; var pBuf: PAnsiChar): Integer;

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes].
  Function accepts constant pointer for easier usage with single reads.
  @param PlainDriver a Interbase Plain drver
  @param Buffer - a buffer returned by driver
  @return - a number read
}
function ReadInterbase6Number(const PlainDriver: TZInterbasePlainDriver; const Buffer): Integer; {$IFDEF WITH_INLINE} inline;{$ENDIF}

procedure ScaledOrdinal2Raw(const Value: Int64; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(const Value: UInt64; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(Value: Integer; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(Value: Cardinal; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;

procedure ScaledOrdinal2Unicode(const Value: Int64; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(const Value: UInt64; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(Value: Integer; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(Value: Cardinal; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;

procedure BCD2ScaledOrdinal(const Value: TBCD; Dest: Pointer; DestSize, Scale: Byte);

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses
  ZFastCode, Variants, ZSysUtils, Math, ZDbcInterbase6, ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

function FindPBParam(const ParamName: string; const ParamArr: array of TZIbParam): PZIbParam;
var
  I: Integer;
begin
  for I := Low(ParamArr) to High(ParamArr) do
    if ParamName = ParamArr[I].Name then
    begin
      Result := @ParamArr[I];
      Exit;
    end;
  Result := nil;
end;

{**
  Build parameter block string

  @param PlainDriver - a Interbase Plain drver
  @param Info - a list connection interbase parameters
  @param VersionCode - isc_dpb_version1 for TPB or isc_dpb_version3 for DPB
  @param FilterPrefix - TPBPrefix for TPB or DPBPrefix for DPB
  @param ParamArr - array of parameter properties

  @return generated string
}
function BuildPB(PlainDriver: TZInterbasePlainDriver; Info: TStrings; VersionCode: Byte;
  const FilterPrefix: string; const ParamArr: array of TZIbParam;
  ConSettings: PZConSettings; CP: Word): RawByteString;
var Buf: TRawBuff;

  procedure ExtractParamNameAndValue(const S: string; out ParamName: String; out ParamValue: String);
  var
    Pos: Integer;
  begin
    Pos := FirstDelimiter(' ='#9#10#13, S);
    if Pos = 0 then
    begin
      ParamName := S;
      ParamValue := '';
    end
    else
    begin
      ParamName := Trim(LowerCase(Copy(S, 1, Pos - 1)));
      ParamValue := Trim(Copy(S, Pos + 1, MaxInt));
    end;
  end;

  procedure NumToPB(Value: Cardinal);
  var Len: Smallint;
  begin
    case Value of
      0..High(Byte):
        begin
          Len := 1;
          ToBuff(AnsiChar(Len), Buf, Result);
          ToBuff(AnsiChar(Byte(Value)), Buf, Result);
        end;
      High(Byte)+1..High(Word):
        begin
          Len := 2;
          ToBuff(AnsiChar(Len), Buf, Result);
          PWord(@Value)^ := Word(Value);
          PWord(@Value)^ := Word(PlainDriver.isc_vax_integer(@Value, Len));
          ToBuff(@Value, Len, Buf, Result);
        end;
      else
        begin
          Len := 4;
          ToBuff(AnsiChar(Len), Buf, Result);
          Value := Cardinal(PlainDriver.isc_vax_integer(@Value, Len));
          ToBuff(@Value, Len, Buf, Result);
        end;
    end;
  end;

var
  I, IntValue: Integer;
  ParamName: String;
  ParamValue: String;
  tmp: RawByteString;
  PParam: PZIbParam;
begin
  Buf.Buf[0] := AnsiChar(VersionCode);
  Buf.Pos := 1;
  Result := EmptyRaw;
  for I := 0 to Info.Count - 1 do
  begin
    ExtractParamNameAndValue(Info.Strings[I], ParamName, ParamValue);
    if ZFastCode.Pos(FilterPrefix, ParamName) <> 1 then
      Continue;
    PParam := FindPBParam(ParamName, ParamArr);
    if PParam = nil then
      raise EZSQLException.CreateFmt('Unknown PB parameter "%s"', [ParamName]);

    case PParam.ValueType of
      pvtNone:
        if VersionCode = isc_tpb_version3 then
          ToBuff(AnsiChar(PParam.Number), Buf, Result)
        else
        begin
          ToBuff(AnsiChar(PParam.Number), Buf, Result);
          ToBuff(AnsiChar(#0), Buf, Result);
        end;
      pvtByteZ:
        begin
          ToBuff(AnsiChar(PParam.Number), Buf, Result);
          ToBuff(AnsiChar(#1), Buf, Result);
          ToBuff(AnsiChar(#0), Buf, Result);
        end;
      pvtNum:
        begin
          ToBuff(AnsiChar(PParam.Number), Buf, Result);
          IntValue := StrToInt(ParamValue);
          NumToPB(IntValue);
        end;
      pvtString:
        begin
          {$IFDEF UNICODE}
          tmp := ZUnicodeToRaw(ParamValue, CP);
          {$ELSE}
          tmp := ZConvertStringToRawWithAutoEncode(ParamValue, ConSettings^.CTRL_CP, CP);
          {$ENDIF}
          ToBuff(AnsiChar(PParam.Number), Buf, Result);
          ToBuff(AnsiChar(Length(tmp)), Buf, Result);
          ToBuff(tmp, Buf, Result);
        end;
    end;
  end;
  FlushBuff(Buf, Result);
end;

{**
  Generate database connection string by connection information

  @param PlainDriver - a Interbase Plain drver
  @param Info - a list connection interbase parameters
  @return a generated string
}
function GenerateDPB(PlainDriver: TZInterbasePlainDriver; Info: TStrings;
  ConSettings: PZConSettings; CP: Word): RawByteString;
begin
  Result := BuildPB(PlainDriver, Info, isc_dpb_version1, DPBPrefix, DatabaseParams, ConSettings, CP);
end;

{**
  Generate transaction string by connection information

  @param PlainDriver - a Interbase Plain drver
  @param Params - a transaction parameters list
  @return a generated string
}
function GenerateTPB(PlainDriver: TZInterbasePlainDriver; Params: TStrings;
  ConSettings: PZConSettings; CP: Word): RawByteString;
begin
  Result := BuildPB(PlainDriver, Params, isc_tpb_version3, TPBPrefix, TransactionParams, ConSettings, CP);
end;

{**
  Generate transaction structure by connection information

  @param PHandle - pointer to database connection handle
  @param TPB - transaction parameter string
  @param TEB a transaction ISC structure
}
procedure GenerateTEB(PHandle: PISC_DB_HANDLE; const TPB: RawByteString; var TEB: TISC_TEB);
begin
  TEB.db_handle := PHandle;
  TEB.tpb_length := Length(TPB);
  TEB.tpb_address := Pointer(TPB);
end;

function GetPBNumber(const FilterPrefix, ParamName: string; const ParamArr: array of TZIbParam): Word;
var
  pParam: PZIbParam;
  ParamNameLO: String;
begin
  ParamNameLO := LowerCase(ParamName);
  Result := 0;
  if ZFastCode.Pos(FilterPrefix, ParamNameLO) = 1 then
  begin
    pParam := FindPBParam(ParamNameLO, ParamArr);
    if pParam <> nil then
      Result := pParam^.Number;
  end;
end;

{**
  Return interbase connection parameter number by it name
  @param Value - a connection parameter name
  @return - connection parameter number
}
function GetInterbase6DatabaseParamNumber(const Value: String): Word;
begin
  Result := GetPBNumber(DPBPrefix, Value, DatabaseParams);
end;

{**
  Return interbase transaction parameter number by it name
  @param Value - a transaction parameter name
  @return - transaction parameter number
}
function GetInterbase6TransactionParamNumber(const Value: String): Word;
begin
  Result := GetPBNumber(TPBPrefix, Value, TransactionParams);
end;

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes]
  and increments buffer pointer skipping read data.
  @param PlainDriver a Interbase Plain drver
  @param pBuf - pointer to a buffer returned by driver. After the function it points to the next block.
  @return - a number read
}
function ReadInterbase6NumberWithInc(const PlainDriver: TZInterbasePlainDriver; var pBuf: PAnsiChar): Integer;
var
  Len: Integer;
begin
  Len := PlainDriver.isc_vax_integer(pBuf, 2);
  Inc(pBuf, 2);
  Result := PlainDriver.isc_vax_integer(pBuf, Len);
  Inc(pBuf, Len);
end;

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes].
  Function accepts constant pointer for easier usage with single reads.
  @param PlainDriver a Interbase Plain drver
  @param Buffer - a buffer returned by driver
  @return - a number read
}
function ReadInterbase6Number(const PlainDriver: TZInterbasePlainDriver; const Buffer): Integer; {$IFDEF WITH_INLINE} inline;{$ENDIF}
var
  pBuf: PAnsiChar;
begin
  pBuf := @Buffer;
  Result := ReadInterbase6NumberWithInc(PlainDriver, pBuf);
end;

procedure ScaledOrdinal2Raw(const Value: Int64; Buf: PAnsiChar; PEnd: PPAnsiChar;
  Scale: Byte);
var
  i64: UInt64;
  Negative: Boolean;
begin
  {$R-} {$Q-}
  Negative := Value < 0;
  if Negative then begin
    i64 := UInt64(-Value);
    PByte(Buf)^ := Ord('-');
    Inc(Buf);
  end else
    i64 := UInt64(Value);
  ScaledOrdinal2Raw(i64, Buf, PEnd, Scale);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Raw(const Value: UInt64; Buf: PAnsiChar;
  PEnd: PPAnsiChar; Scale: Byte);
var
  Precision: Byte;
  rEnd: PAnsiChar;//tmp Remainder
  I: ShortInt;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    PByte(Buf)^ := ord('0');
    rEnd := Buf+1;
  end else begin
    Precision := GetOrdinalDigits(Value);
    if Precision <= Scale then begin
      PWord(Buf)^ := Ord('0')+Ord('.') shl 8; //write "0."
      Inc(Buf, 2);
      for i := 0 to Scale-Precision-1 do begin  //eh: opt? 2/4 digit's per loop?
        PByte(Buf)^ := Ord('0');
        Inc(Buf);
      end;
      IntToRaw(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while (PByte(rEnd-1)^ = Ord('0')) do
        dec(rEnd);
    end else begin
      IntToRaw(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while true do begin
        if (Scale = 0) or (PByte(rEnd-1)^ <> Ord('0')) then
          Break;
        Dec(Scale); Dec(rEnd);
      end;
      {backward move is now required to set the missing dot char}
      if Scale > 0 then begin
        System.Move((rend-Scale)^, (rend+1-Scale)^, Scale); //eh: move call(min 50cycles) required? numbers are not so big...
        PByte(rend-Scale)^ := Ord('.');
        Inc(rEnd);
      end;
    end;
  end;
  if PEnd <> nil
  then PEnd^ := rEnd
  else PByte(rEnd)^ := Ord(#0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Raw(Value: Integer; Buf: PAnsiChar; PEnd: PPAnsiChar;
  Scale: Byte);
begin
  {$R-} {$Q-}
  if Value < 0 then begin
    PByte(Buf)^ := Ord('-');
    ScaledOrdinal2Raw(Cardinal(-Value), Buf+1, PEnd, Scale)
  end else
    ScaledOrdinal2Raw(Cardinal(Value), Buf, PEnd, Scale);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Raw(Value: Cardinal; Buf: PAnsiChar; PEnd: PPAnsiChar;
  Scale: Byte);
var
  Precision: Byte;
  rEnd: PAnsiChar;//tmp Remainder
  I: ShortInt;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    PByte(Buf)^ := ord('0');
    rEnd := Buf+1;
  end else begin
    Precision := GetOrdinalDigits(Value);
    if Precision <= Scale then begin
      PWord(Buf)^ := Ord('0')+Ord('.') shl 8; //write "0."
      Inc(Buf, 2);
      for i := 0 to Scale-Precision-1 do begin  //eh: opt? 2/4 digit's per loop?
        PByte(Buf)^ := Ord('0');
        Inc(Buf);
      end;
      IntToRaw(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while (PByte(rEnd-1)^ = Ord('0')) do
        dec(rEnd);
    end else begin
      IntToRaw(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while true do begin
        if (Scale = 0) or (PByte(rEnd-1)^ <> Ord('0')) then
          Break;
        Dec(Scale); Dec(rEnd);
      end;
      {backward move is now required to set the missing dot char}
      if Scale > 0 then begin
        System.Move((rend-Scale)^, (rend+1-Scale)^, Scale); //eh: move call(min 50cycles) required? numbers are not so big...
        PByte(rend-Scale)^ := Ord('.');
        Inc(rEnd);
      end;
    end;
  end;
  if PEnd <> nil
  then PEnd^ := rEnd
  else PByte(rEnd)^ := Ord(#0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;


procedure ScaledOrdinal2Unicode(const Value: Int64; Buf: PWideChar;
  PEnd: ZPPWideChar; Scale: Byte);
var
  i64: UInt64;
  I64Rec: Int64Rec absolute i64;
  Negative: Boolean;
begin
  {$R-} {$Q-}
  Negative := Value < 0;
  if Negative then begin
    i64 := UInt64(-Value);
    PWord(Buf)^ := Ord('-');
    Inc(Buf);
  end else
    i64 := UInt64(Value);
  if i64Rec.Hi = 0
  then ScaledOrdinal2Unicode(i64Rec.Lo, Buf, PEnd, Scale)
  else ScaledOrdinal2Unicode(i64,       Buf, PEnd, Scale);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Unicode(const Value: UInt64; Buf: PWideChar;
  PEnd: ZPPWideChar; Scale: Byte);
var
  Precision: Byte;
  rEnd: PWideChar;//tmp Remainder
  I: ShortInt;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    PWord(Buf)^ := ord('0');
    rEnd := Buf+1;
  end else begin
    Precision := GetOrdinalDigits(Value);
    if Precision <= Scale then begin
      PLongWord(Buf)^ := Ord('0')+Ord('.') shl 16; //write "0."
      Inc(Buf, 2);
      for i := 0 to Scale-Precision-1 do begin  //eh: opt? 2/4 digit's per loop?
        PWord(Buf)^ := Ord('0');
        Inc(Buf);
      end;
      IntToUnicode(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while (PWord(rEnd-1)^ = Ord('0')) do
        dec(rEnd);
    end else begin
      IntToUnicode(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while true do begin
        if (Scale = 0) or (PWord(rEnd-1)^ <> Ord('0')) then
          Break;
        Dec(Scale); Dec(rEnd);
      end;
      {backward move is now required to set the missing dot char}
      if Scale > 0 then begin
        System.Move((rend-Scale)^, (rend+1-Scale)^, Scale shl 1); //eh: move call(min 50cycles) required? numbers are not so big...
        PWord(rend-Scale)^ := Ord('.');
        Inc(rEnd);
      end;
    end;
  end;
  if PEnd <> nil
  then PEnd^ := rEnd
  else PWord(rEnd)^ := Ord(#0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Unicode(Value: Integer; Buf: PWideChar;
  PEnd: ZPPWideChar; Scale: Byte);
begin
  {$R-} {$Q-}
  if Value < 0 then begin
    PByte(Buf)^ := Ord('-');
    ScaledOrdinal2Unicode(Cardinal(-Value), Buf+1, PEnd, Scale)
  end else
    ScaledOrdinal2Unicode(Cardinal(Value), Buf, PEnd, Scale);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

procedure ScaledOrdinal2Unicode(Value: Cardinal; Buf: PWideChar;
  PEnd: ZPPWideChar; Scale: Byte);
var
  Precision: Byte;
  rEnd: PWideChar;//tmp Remainder
  I: ShortInt;
begin
  {$R-} {$Q-}
  if Value = 0 then begin
    PWord(Buf)^ := ord('0');
    rEnd := Buf+1;
  end else begin
    Precision := GetOrdinalDigits(Value);
    if Precision <= Scale then begin
      PLongWord(Buf)^ := Ord('0')+Ord('.') shl 16; //write "0."
      Inc(Buf, 2);
      for i := 0 to Scale-Precision-1 do begin  //eh: opt? 2/4 digit's per loop?
        PWord(Buf)^ := Ord('0');
        Inc(Buf);
      end;
      IntToUnicode(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while (PWord(rEnd-1)^ = Ord('0')) do
        dec(rEnd);
    end else begin
      IntToUnicode(Value, Buf, Precision);
      rEnd := Buf+Precision;
      while true do begin
        if (Scale = 0) or (PWord(rEnd-1)^ <> Ord('0')) then
          Break;
        Dec(Scale); Dec(rEnd);
      end;
      {backward move is now required to set the missing dot char}
      if Scale > 0 then begin
        System.Move((rend-Scale)^, (rend+1-Scale)^, Scale shl 1); //eh: move call(min 50cycles) required? numbers are not so big...
        PWord(rend-Scale)^ := Ord('.');
        Inc(rEnd);
      end;
    end;
  end;
  if PEnd <> nil
  then PEnd^ := rEnd
  else PWord(rEnd)^ := Ord(#0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

{**
  Converts a Interbase6 native types into ZDBC SQL types.
  @param the interbase type
  @param the interbase subtype
  @return a SQL undepended type.

  <b>Note:</b> The interbase type and subtype get from RDB$TYPES table
}
function ConvertInterbase6ToSqlType(SqlType, SqlSubType, Scale, Precision: Integer;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
label testBCD;
begin
  case SqlType of
    blr_bool, blr_not_nullable: Result := stBoolean;
    blr_domain_name, blr_domain_name2,
    blr_column_name, blr_column_name2:
      Result := stString;
    blr_varying2, blr_varying,
    blr_text, blr_text2,
    blr_cstring, blr_cstring2:
      if SqlSubType = CS_BINARY
      then Result := stBytes
      else Result := stString;
    blr_float: Result := stFloat;
    blr_double: Result := stDouble;
    blr_blob_id, blr_quad: Result := stLong;
    blr_short:
      if Scale = 0
      then Result := stSmall
      else goto testBCD;
    blr_long:
      if Scale = 0
      then Result := stInteger
      else goto testBCD;
    blr_int64:
        if Scale = 0 then
          Result := stLong
        else begin
testBCD:  Scale := Abs(Scale);
          if (Scale <= 4) and (Precision <= sAlignCurrencyScale2Precision[Scale])
          then Result := stCurrency
          else Result := stBigDecimal;
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
    SQL_BOOLEAN_FB: Result := 'SQL_BOOLEAN_FB';
  else
    Result := 'Unknown';
  end
end;

{**
   Convert raw database string to compiler-native string
}
function ConvertConnRawToString(ConSettings: PZConSettings; const Src: RawByteString): string; overload;
begin
  if ConSettings <> nil then
    Result := ConSettings^.ConvFuncs.ZRawToString(Src, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
  else
    Result := string(Src);
end;

{**
   Convert pointer to raw database string to compiler-native string
}
function ConvertConnRawToString(ConSettings: PZConSettings; Buffer: Pointer; BufLen: Integer): string; overload;
var
  RawStr: RawByteString;
begin
  // TODO: having ZPRawToString we could convert the string directly without SetString
  ZSetString(PAnsiChar(Buffer), BufLen, RawStr);
  if ConSettings <> nil then
    Result := ConSettings^.ConvFuncs.ZRawToString(RawStr, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
  else
    Result := string(RawStr);
end;

{**
   Convert zero-terminated raw database string to compiler-native string
}
function ConvertConnRawToString(ConSettings: PZConSettings; Buffer: Pointer): string; overload;
begin
  Result := ConvertConnRawToString(ConSettings, Buffer, StrLen(Buffer));
end;

function ConvertStringToConnRaw(ConSettings: PZConSettings; const Src: string): RawByteString;
begin
  if ConSettings <> nil then
    Result := ConSettings^.ConvFuncs.ZStringToRaw(Src, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
  else
    Result := RawByteString(Src);
end;

{**
  Processes Interbase status vector and returns array of status data.
  @param PlainDriver a Interbase Plain drver
  @param StatusVector a status vector. It contain information about error
  @param ConSettings pointer to connection settings containing codepage info

  @return array of TInterbaseStatus records
}
function InterpretInterbaseStatus(const PlainDriver: TZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS;
  const ConSettings: PZConSettings) : TZIBStatusVector;
var
  Buffer: array[0..IBBigLocalBufferLength] of AnsiChar;
  PStatusVector: PISC_STATUS;
  StatusIdx: Integer;
  pCurrStatus: PZIBStatus;
begin
  Result := nil;
  PStatusVector := @StatusVector; StatusIdx := 0;
  repeat
    SetLength(Result, Length(Result) + 1);
    pCurrStatus := @Result[High(Result)]; // save pointer to avoid multiple High() calls
    // SQL code and status
    pCurrStatus.SQLCode := PlainDriver.isc_sqlcode(PStatusVector);
    PlainDriver.isc_sql_interprete(pCurrStatus.SQLCode, @Buffer, SizeOf(Buffer));
    pCurrStatus.SQLMessage := ConvertConnRawToString(ConSettings, @Buffer);
    // IB data
    pCurrStatus.IBDataType := StatusVector[StatusIdx];
    case StatusVector[StatusIdx] of
      isc_arg_end:  // end of argument list
        Break;
      isc_arg_gds,  // Long int code
      isc_arg_number,
      isc_arg_vms,
      isc_arg_unix,
      isc_arg_domain,
      isc_arg_dos,
      isc_arg_mpexl,
      isc_arg_mpexl_ipc,
      isc_arg_next_mach,
      isc_arg_netware,
      isc_arg_win32:
        begin
          pCurrStatus.IBDataInt := StatusVector[StatusIdx + 1];
          Inc(StatusIdx, 2);
        end;
      isc_arg_string,  // pointer to string
      isc_arg_interpreted,
      isc_arg_sql_state:
        begin
          pCurrStatus.IBDataStr := ConvertConnRawToString(ConSettings, Pointer(StatusVector[StatusIdx + 1]));
          Inc(StatusIdx, 2);
        end;
      isc_arg_cstring: // length and pointer to string
        begin
          pCurrStatus.IBDataStr := ConvertConnRawToString(ConSettings, Pointer(StatusVector[StatusIdx + 2]), StatusVector[StatusIdx + 1]);
          Inc(StatusIdx, 3);
        end;
      isc_arg_warning: // must not happen for error vector
        Break;
      else
        Break;
    end; // case

    // isc_interprete is deprecated so use fb_interpret instead if available
    if Assigned(PlainDriver.fb_interpret) then
      if PlainDriver.fb_interpret(@Buffer, Length(Buffer), @PStatusVector) = 0 then
        Break
      else
    else
    if PlainDriver.isc_interprete(@Buffer, @PStatusVector) = 0 then
      Break;
    pCurrStatus.IBMessage := ConvertConnRawToString(ConSettings, @Buffer);
  until False;
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a Interbase Plain drver
  @param StatusVector a status vector. It contain information about error
  @param Sql a sql query commend

  @Param Integer Return is the ErrorCode that happened - for disconnecting the database
}
procedure CheckInterbase6Error(const PlainDriver: TZInterbasePlainDriver;
  const StatusVector: TARRAY_ISC_STATUS; const ImmediatelyReleasable: IImmediatelyReleasable;
  const LoggingCategory: TZLoggingCategory = lcOther;
  const SQL: RawByteString = '');
var
  ErrorMessage, ErrorSqlMessage, sSQL: string;
  ErrorCode: Integer;
  i: Integer;
  InterbaseStatusVector: TZIBStatusVector;
  ConSettings: PZConSettings;
  ConLostError: EZSQLConnectionLost;
begin
  if not ((StatusVector[0] = 1) and (StatusVector[1] > 0)) then Exit;
  ConSettings := ImmediatelyReleasable.GetConSettings;
  InterbaseStatusVector := InterpretInterbaseStatus(PlainDriver, StatusVector, ConSettings);

  ErrorMessage := '';
  for i := Low(InterbaseStatusVector) to High(InterbaseStatusVector) do
    AppendSepString(ErrorMessage, InterbaseStatusVector[i].IBMessage, '; ');

  ErrorCode := InterbaseStatusVector[0].SQLCode;
  ErrorSqlMessage := InterbaseStatusVector[0].SQLMessage;

  sSQL := ConvertConnRawToString(ConSettings, SQL);
  if sSQL <> '' then
    ErrorSqlMessage := ErrorSqlMessage + ' The SQL: '+sSQL+'; ';

  if ErrorMessage <> '' then
  begin
    DriverManager.LogError(LoggingCategory, ConSettings^.Protocol,
      ConvertStringToConnRaw(ConSettings, ErrorMessage), ErrorCode,
      ConvertStringToConnRaw(ConSettings, ErrorSqlMessage));
    if ErrorCode = {isc_network_error..isc_net_write_err,} isc_lost_db_connection then begin
      ConLostError := EZSQLConnectionLost.CreateWithCode(ErrorCode,
      Format(SSQLError1, [sSQL]));
      ImmediatelyReleasable.ReleaseImmediat(ImmediatelyReleasable, ConLostError);
      if ConLostError <> nil then raise ConLostError;
    end else raise EZIBSQLException.Create(
      Format(SSQLError1, [ErrorMessage]), InterbaseStatusVector, sSQL);
  end;
end;

{**
   Get affected rows.
   <i>Note:<i> it function may call after statement execution
   @param PlainDriver a interbase plain driver
   @param StmtHandle a statement handle
   @param StatementType a statement type
   @return affected rows
}
function GetAffectedRows(const PlainDriver: TZInterbasePlainDriver;
  const StmtHandle: TISC_STMT_HANDLE; const StatementType: TZIbSqlStatementType;
  const ImmediatelyReleasable: IImmediatelyReleasable): integer;
type
  TCountType = (cntSel, cntIns, cntDel, cntUpd);
var
  ReqInfo: AnsiChar;
  Buffer: array[0..IBLocalBufferLength-1] of AnsiChar;
  StatusVector: TARRAY_ISC_STATUS;
  pBuf, pBufStart: PAnsiChar;
  Len, Item, Count: Integer;
  Counts: array[TCountType] of Integer;
begin
  Result := -1;
  ReqInfo := AnsiChar(isc_info_sql_records);

  if PlainDriver.isc_dsql_sql_info(@StatusVector, @StmtHandle, 1,
      @ReqInfo, SizeOf(Buffer), @Buffer[0]) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);

  if Buffer[0] <> AnsiChar(isc_info_sql_records) then
    Exit;

  pBufStart := @Buffer[1];
  pBuf := pBufStart;
  Len := PlainDriver.isc_vax_integer(pBuf, 2) + 2;
  Inc(pBuf, 2);
  if Buffer[Len] <> AnsiChar(isc_info_end) then
    Exit;

  FillChar(Counts{%H-}, SizeOf(Counts), #0);
  while pBuf - pBufStart <= Len do
  begin
    Item := Byte(pBuf^);

    if Item = isc_info_end then
      Break;

    Inc(pBuf);
    Count := ReadInterbase6NumberWithInc(PlainDriver, pBuf);

    case Item of
      isc_info_req_select_count: Counts[cntSel] := Count;
      isc_info_req_insert_count: Counts[cntIns] := Count;
      isc_info_req_update_count: Counts[cntUpd] := Count;
      isc_info_req_delete_count: Counts[cntDel] := Count;
      else
        raise EZSQLException.Create(SInternalError);
    end;
  end;

  { Note: Update statements could have Select counter <> 0 as well }

  case StatementType of
    stSelect, //selectable procedure could have a update count but FB does not return them.
    stSelectForUpdate: Result := Counts[cntSel];
    stInsert:          Result := Counts[cntIns];
    stUpdate:          Result := Counts[cntUpd];
    stDelete:          Result := Counts[cntDel];
    stExecProc:
      begin
        { Exec proc could have any counter... So search for the first non-zero counter }
        Result := Counts[cntIns];
        if Result > 0 then Exit;
        Result := Counts[cntUpd];
        if Result > 0 then Exit;
        Result := Counts[cntDel];
        if Result > 0 then Exit;
        Result := Counts[cntSel];
      end;
    else
      Result := -1;
  end;
end;

{**
   Read blob information by it handle such as blob segment size, segments count,
   blob size and type.
   @param PlainDriver
   @param BlobInfo the blob information structure
}
procedure GetBlobInfo(const PlainDriver: TZInterbasePlainDriver;
  const BlobHandle: TISC_BLOB_HANDLE; out BlobInfo: TIbBlobInfo;
  const ImmediatelyReleasable: IImmediatelyReleasable);
var
  Items: array[0..3] of AnsiChar;
  Results: array[0..99] of AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Item, ItemVal: Integer;
  StatusVector: TARRAY_ISC_STATUS;
begin
  Items[0] := AnsiChar(isc_info_blob_num_segments);
  Items[1] := AnsiChar(isc_info_blob_max_segment);
  Items[2] := AnsiChar(isc_info_blob_total_length);
  Items[3] := AnsiChar(isc_info_blob_type);

  if PlainDriver.isc_blob_info(@StatusVector, @BlobHandle, 4, @items[0],
      SizeOf(Results), @Results[0]) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);

  FillChar(BlobInfo{%H-}, SizeOf(BlobInfo), #0);

  pBufStart := @Results[0];
  pBuf := pBufStart;
  while pBuf - pBufStart <= SizeOf(Results) do
  begin
    Item := Byte(pBuf^);
    if Item = isc_info_end then
      Break;

    Inc(pBuf);
    ItemVal := ReadInterbase6NumberWithInc(PlainDriver, pBuf);

    case Item of
      isc_info_blob_num_segments:
        BlobInfo.NumSegments := ItemVal;
      isc_info_blob_max_segment:
        BlobInfo.MaxSegmentSize := ItemVal;
      isc_info_blob_total_length:
        BlobInfo.TotalSize := ItemVal;
      isc_info_blob_type:
        BlobInfo.BlobType := ItemVal;
    end;
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
procedure ReadBlobBufer(const PlainDriver: TZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const TransactionHandle: PISC_TR_HANDLE;
  const BlobId: TISC_QUAD; out Size: Integer; out Buffer: Pointer;
  const Binary: Boolean; const ImmediatelyReleasable: IImmediatelyReleasable);
var
  TempBuffer: PAnsiChar;
  BlobInfo: TIbBlobInfo;
  CurPos: Integer;
  BytesRead, SegLen: ISC_USHORT;
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
begin
  BlobHandle := 0;
  CurPos := 0;
//  SegmentLenght := UShort(DefaultBlobSegmentSize);

  { open blob }
  if PlainDriver.isc_open_blob2(@StatusVector, Handle,
         TransactionHandle, @BlobHandle, @BlobId, 0 , nil) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);

  { get blob info }
  GetBlobInfo(PlainDriver, BlobHandle, BlobInfo, ImmediatelyReleasable);
  Size := BlobInfo.TotalSize;
  SegLen := BlobInfo.MaxSegmentSize;

  { Allocates a blob buffer }
  Buffer := AllocMem(BlobInfo.TotalSize+Ord(not Binary)); //left space for leading #0 terminator

  TempBuffer := Buffer;

  { Copies data to blob buffer }
  while CurPos < BlobInfo.TotalSize do begin
    if (CurPos + SegLen > BlobInfo.TotalSize) then
      SegLen := BlobInfo.TotalSize - CurPos;
    if not(PlainDriver.isc_get_segment(@StatusVector, @BlobHandle,
           @BytesRead, SegLen, TempBuffer) = 0) or
          (StatusVector[1] <> isc_segment) then
      CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);
    Inc(CurPos, BytesRead);
    Inc(TempBuffer, BytesRead);
  end;
  if not Binary then
    PByte(PAnsiChar(Buffer)+Size)^ := Ord(#0);

  { close blob handle }
  if PlainDriver.isc_close_blob(@StatusVector, @BlobHandle) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);
end;

{**
   Return interbase server version string
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @param isc_info a ISC_INFO_XXX number
   @param ConSettings then PZConSettings of active connection
   @return ISC_INFO string
}
function GetDBStringInfo(const PlainDriver: TZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; const ImmediatelyReleasable: IImmediatelyReleasable): String;
var
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..IBBigLocalBufferLength - 1] of AnsiChar;
begin
  if PlainDriver.isc_database_info(@StatusVector, Handle, 1, @isc_info,
      SizeOf(Buffer), @Buffer[0]) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);

  { Buffer:
      0     - type of info
      1..2  - total data length
      3     - #1
      4     - string length
      5..N  - string
      N+1   - #1 }
  if Buffer[0] = AnsiChar(isc_info) then
    Result := ConvertConnRawToString(ImmediatelyReleasable.GetConSettings, @Buffer[5], Integer(Buffer[4]))
  else
    Result := '';
end;

{**
   Return interbase server version string
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @param isc_info a ISC_INFO_XXX number
   @param ConSettings then PZConSettings of active connection
   @return ISC_INFO Integer
}
function GetDBIntegerInfo(const PlainDriver: TZInterbasePlainDriver;
  Handle: PISC_DB_HANDLE; isc_info: Byte; const ImmediatelyReleasable: IImmediatelyReleasable): Integer;
var
  StatusVector: TARRAY_ISC_STATUS;
  Buffer: array[0..31] of AnsiChar; // this should be enough for any number
begin
  if PlainDriver.isc_database_info(@StatusVector, Handle, 1, @isc_info,
      SizeOf(Buffer), @Buffer[0]) <> 0 then
    CheckInterbase6Error(PlainDriver, StatusVector, ImmediatelyReleasable);

  { Buffer:
      0     - type of info
      1..2  - number length
      3..N  - number
      N+1   - #1 }
  if Buffer[0] = AnsiChar(isc_info)
    then Result := ReadInterbase6Number(PlainDriver, Buffer[1])
    else Result := -1;
end;

{**
   Return interbase database dialect
   @param PlainDriver a interbase plain driver
   @param Handle the database connection handle
   @return interbase database dialect
}
function GetDBSQLDialect(const PlainDriver: TZInterbasePlainDriver;
  const Handle: PISC_DB_HANDLE; const ImmediatelyReleasable: IImmediatelyReleasable): Integer;
begin
  Result := GetDBIntegerInfo(PlainDriver, Handle, isc_info_db_SQL_Dialect, ImmediatelyReleasable);
  if Result = -1 then
    Result := SQL_DIALECT_V5;
end;

{ TZFBSpecificData }

function TZIBSpecificData.Clone: TZExceptionSpecificData;
begin
  Result := TZIBSpecificData.Create;
  TZIBSpecificData(Result).FStatusVector := StatusVector;
  TZIBSpecificData(Result).FSQL := SQL;
  TZIBSpecificData(Result).FIBErrorCode := IBErrorCode;
  TZIBSpecificData(Result).FIBStatusCode := IBStatusCode;
end;

{ EZIBSQLException }

constructor EZIBSQLException.Create(const Msg: string; const StatusVector: TZIBStatusVector; const SQL: string);
var
  i, SQLErrCode, IBErrorCode: Integer;
  IBStatusCode: String;
begin
  SQLErrCode := 0; IBErrorCode := 0;
  // find main IB code
  for i := Low(StatusVector) to High(StatusVector) do
    if StatusVector[i].IBDataType = isc_arg_gds then
    begin
      IBErrorCode := StatusVector[i].IBDataInt;
      IBStatusCode := StatusVector[i].IBMessage;
      SQLErrCode := StatusVector[i].SQLCode;
      Break;
    end;

  inherited CreateWithCode(SQLErrCode, Msg);
  FSpecificData := TZIBSpecificData.Create;
  TZIBSpecificData(FSpecificData).FStatusVector := StatusVector;
  TZIBSpecificData(FSpecificData).FSQL := SQL;
  TZIBSpecificData(FSpecificData).FIBErrorCode := IBErrorCode;
  TZIBSpecificData(FSpecificData).FIBStatusCode := IBStatusCode;
end;

{ TSQLDA }
constructor TZSQLDA.Create(const Connection: IZConnection);
begin
  FConnection := Connection;
  Self.ConSettings := Connection.GetConSettings;
  FPlainDriver := TZInterbasePlainDriver(Connection.GetIZPlainDriver.GetInstance);

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
procedure TZSQLDA.InitFields(Fixed2VariableSize: Boolean);
var
  I,m: Integer;
  SqlVar: PXSQLVAR;
begin
  {$R-}
  for I := 0 to FXSQLDA.sqld - 1 do begin
    SqlVar := @FXSQLDA.SqlVar[I];
    M := SqlVar.sqllen;
    if SqlVar.sqltype and (not 1) = SQL_VARYING then
      Inc(M, 2);
    if SqlVar.sqldata <> nil then
      FreeMem(SqlVar.sqldata, M);
    if (SqlVar.sqltype and (not 1) = SQL_TEXT) and Fixed2VariableSize then begin
      SqlVar.sqltype := (SQL_VARYING or 1);
      Inc(M, 2);
    end;
    GetMem(SqlVar.sqldata, m);
    if Fixed2VariableSize then begin {Praremeters}
      //This code used when allocated sqlind parameter for Param SQLDA
      SqlVar.sqltype := SqlVar.sqltype or 1;
      IbReAlloc(SqlVar.sqlind, 0, SizeOf(Short));
    end else
      //This code used when allocated sqlind parameter for Result SQLDA
      if (SqlVar.sqltype and 1) <> 0
      then ReallocMem(SqlVar.sqlind, SizeOf(Short))
      else SqlVar.sqlind := nil;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
   Chech range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZSQLDA.CheckRange(const Index: Word);
begin
  Assert(Index < Word(FXSQLDA.sqln), 'Out of Range.');
end;

function ConvertConnRawToStringWithOpt(ConSettings: PZConSettings; Buffer: Pointer; BufLen: Integer): string;
begin
  {$IFDEF UNICODE}
  if ConSettings^.ClientCodePage^.ID = CS_NONE
  then Result := PRawToUnicode(Buffer, BufLen, zCP_UTF8)
  else Result := PRawToUnicode(Buffer, BufLen, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
      SetString(Result, PChar(Buffer), BufLen)
    else if ConSettings^.ClientCodePage^.ID = CS_NONE
      then Result := ZUnicodeToString(PRawToUnicode(Buffer, BufLen, zCP_UTF8), ConSettings^.CTRL_CP)
      else Result := ZUnicodeToString(PRawToUnicode(Buffer, BufLen, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
  {$ENDIF}
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
  Result := ConvertConnRawToStringWithOpt(ConSettings,
    @FXSQLDA.sqlvar[Index].aliasname[0], FXSQLDA.sqlvar[Index].aliasname_length);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
function TZSQLDA.GetFieldIndex(const Name: String): Word;
var S: String;
  P1, P2: PChar;
begin
  {$R-}
  for Result := 0 to GetFieldCount - 1 do
  begin
    S := ConvertConnRawToString(ConSettings, @FXSQLDA.sqlvar[Result].aliasname[0], FXSQLDA.sqlvar[Result].aliasname_length);
    if Length(S) = Length(name) then
    begin
      P1 := Pointer(Name);
      P2 := Pointer(S);
      if StrLIComp(P1, P2, Length(S)) = 0 then
        Exit;
    end;
  end;
  raise Exception.Create(Format(SFieldNotFound1, [name]));
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
   Convert Interbase sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZSQLDA.GetFieldSqlType(Index: Word): TZSQLType;
var
  XSQLVAR: PXSQLVAR;
begin
  CheckRange(Index);
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}

  case XSQLVAR.sqltype and not (1) of
    SQL_VARYING, SQL_TEXT:
      if XSQLVAR.SqlSubType = CS_BINARY {Octets}
      then Result := stBytes
      else Result := stString;
    SQL_LONG:
        if XSQLVAR.sqlscale = 0 then
          Result := stInteger
        else if XSQLVAR.SqlScale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_SHORT:
        if XSQLVAR.SqlScale = 0 then
          Result := stSmall
        else if XSQLVAR.SqlScale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_FLOAT:
      Result := stFloat;
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_BOOLEAN, SQL_BOOLEAN_FB:
      Result := stBoolean;
    SQL_DATE: Result := stTimestamp;
    SQL_TYPE_TIME: Result := stTime;
    SQL_TYPE_DATE: Result := stDate;
    SQL_INT64:
        //https://firebirdsql.org/file/documentation/reference_manuals/fblangref25-en/html/fblangref25-datatypes-fixedtypes.html
        if XSQLVAR.SqlScale = 0 then
          Result := stLong
        else if XSQLVAR.SqlScale = -4 then //EH firebird supports a max precision of 18 only
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_QUAD, SQL_BLOB:
        if XSQLVAR.SqlSubType = isc_blob_text
        then Result := stAsciiStream
        else Result := stBinaryStream;
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
  Result := ConvertConnRawToStringWithOpt(ConSettings,
    @FXSQLDA.sqlvar[Index].OwnName[0], FXSQLDA.sqlvar[Index].OwnName_length);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  Result := ConvertConnRawToStringWithOpt(ConSettings,
    @FXSQLDA.sqlvar[Index].RelName[0], FXSQLDA.sqlvar[Index].RelName_length);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  Result := FXSQLDA.sqlvar[Index].sqllen;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  Result := ConvertConnRawToStringWithOpt(ConSettings,
    @FXSQLDA.sqlvar[Index].sqlname[0], FXSQLDA.sqlvar[Index].sqlname_length);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  Result := FXSQLDA.sqlvar[Index].sqlsubtype;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  Result := FXSQLDA.sqlvar[Index].sqltype and not (1);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZSQLDA.IsBlob(const Index: Word): boolean;
begin
  CheckRange(Index);
  {$R-}
  Result := ((FXSQLDA.sqlvar[Index].sqltype and not(1)) = SQL_BLOB);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

procedure TZSQLDA.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  if Sender <> (FConnection as IImmediatelyReleasable) then
    (FConnection as IImmediatelyReleasable).ReleaseImmediat(Sender, AError);
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
function TZSQLDA.AllocateSQLDA: PXSQLDA;
begin
  IbReAlloc(FXSQLDA, XSQLDA_LENGTH(FXSQLDA.sqln), XSQLDA_LENGTH(FXSQLDA.sqld));
  FXSQLDA.sqln := FXSQLDA.sqld;
  Result := FXSQLDA;
end;


const
  EBStart = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('EXECUTE BLOCK(');
  EBBegin =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}(')AS BEGIN'+LineEnding);
  EBSuspend =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('SUSPEND;'+LineEnding); //required for RETURNING syntax
  EBEnd = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('END');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
  cRETURNING: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('RETURNING');
function GetExecuteBlockString(const ParamsSQLDA: IZParamsSQLDA;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: TZInterbasePlainDriver;
  var MemPerRow, PreparedRowsOfArray,MaxRowsPerBatch: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: Cardinal): RawByteString;
var
  IndexName, ArrayName: RawByteString;
  ParamIndex, J: Cardinal;
  I, BindCount, ParamNameLen, SingleStmtLength, LastStmLen,
  HeaderLen, FullHeaderLen, StmtLength:  Integer;
  CodePageInfo: PZCodePage;
  PStmts, PResult, P: PAnsiChar;
  ReturningFound: Boolean;

  procedure Put(const Args: array of RawByteString; var Dest: PAnsiChar);
  var I: Integer;
  begin
    for I := low(Args) to high(Args) do //Move data
    begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Args[i])^, Dest^, {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^);
      Inc(Dest, {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^);
    end;
  end;
  procedure AddParam(const Args: array of RawByteString; var Dest: RawByteString);
  var I, L: Integer;
    P: PAnsiChar;
  begin
    Dest := ''; L := 0;
    for I := low(Args) to high(Args) do //Calc String Length
      Inc(L ,Length(Args[i]));
    SetLength(Dest, L);
    P := Pointer(Dest);
    Put(Args, P);
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
           AddParam([' DOUBLE PRECISION=?'], TypeTokens[ParamIndex]);
        SQL_FLOAT:
           AddParam([' FLOAT=?'],TypeTokens[ParamIndex]);
        SQL_LONG:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' INTEGER=?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(9,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'], TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(9', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)), ',', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'],TypeTokens[ParamIndex]);
        SQL_SHORT:
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' SMALLINT=?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(4,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(4', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)), ',', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'],TypeTokens[ParamIndex]);
        SQL_TIMESTAMP:
           AddParam([' TIMESTAMP=?'],TypeTokens[ParamIndex]);
        SQL_BLOB:
          if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = isc_blob_text then
            AddParam([' BLOB SUB_TYPE TEXT=?'],TypeTokens[ParamIndex])
          else
            AddParam([' BLOB=?'],TypeTokens[ParamIndex]);
        //SQL_ARRAY                      = 540;
        //SQL_QUAD                       = 550;
        SQL_TYPE_TIME:
           AddParam([' TIME=?'],TypeTokens[ParamIndex]);
        SQL_TYPE_DATE:
           AddParam([' DATE=?'],TypeTokens[ParamIndex]);
        SQL_INT64: // IB7
          if ParamsSQLDA.GetFieldScale(ParamIndex) = 0 then
            AddParam([' BIGINT=?'],TypeTokens[ParamIndex])
          else
            if ParamsSQLDA.GetIbSqlSubType(ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(18,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(18,', IntToRaw(ParamsSQLDA.GetFieldScale(ParamIndex)),')=?'],TypeTokens[ParamIndex]);
        SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
           AddParam([' BOOLEAN=?'],TypeTokens[ParamIndex]);
        SQL_NULL{FB25}:
           AddParam([' CHAR(1)=?'],TypeTokens[ParamIndex]);
      end;
      Inc(MemPerRow, ParamsSQLDA.GetFieldLength(ParamIndex) +
        2*Ord((ParamsSQLDA.GetIbSqlType(ParamIndex) and not 1) = SQL_VARYING));
    end;
    Inc(MemPerRow, XSQLDA_LENGTH(InParamCount));
  end;
  {now let's calc length of stmt to know if we can bound all array data or if we need some more calls}
  StmtLength := 0;
  FullHeaderLen := 0;
  ReturningFound := False;
  PreparedRowsOfArray := 0;

  for J := 0 to RemainingArrayRows -1 do
  begin
    ParamIndex := 0;
    SingleStmtLength := 0;
    LastStmLen := StmtLength;
    HeaderLen := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin //calc Parameters size
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        ParamNameLen := {P}1+GetOrdinalDigits(ParamIndex)+1{_}+GetOrdinalDigits(j);
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        {inc header}
        Inc(HeaderLen, ParamNameLen+ {%H-}PLengthInt(NativeUInt(TypeTokens[ParamIndex]) - StringLenOffSet)^+Ord(not ((ParamIndex = 0) and (J=0))){,});
        {inc stmt}
        Inc(SingleStmtLength, 1+{:}ParamNameLen);
        Inc(ParamIndex);
      end else begin
        Inc(SingleStmtLength, {%H-}PLengthInt(NativeUInt(CurrentSQLTokens[i]) - StringLenOffSet)^);
        P := Pointer(CurrentSQLTokens[i]);
        if not ReturningFound and (Ord(P^) in [Ord('R'), Ord('r')]) and (Length(CurrentSQLTokens[i]) = Length(cRETURNING)) then begin
          ReturningFound := ZSysUtils.SameText(P, Pointer(cReturning), Length(cRETURNING));
          Inc(StmtLength, Ord(ReturningFound)*Length(EBSuspend));
        end;
      end;
    end;
    Inc(SingleStmtLength, 1{;}+Length(LineEnding));
    if MaxRowsPerBatch = 0 then //calc maximum batch count if not set already
      MaxRowsPerBatch := Min((XSQLDAMaxSize div Int64(MemPerRow)),     {memory limit of XSQLDA structs}
        (((32*1024)-LBlockLen) div Int64(HeaderLen+SingleStmtLength)))+1; {32KB limited Also with FB3};
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if (PreparedRowsOfArray = MaxRowsPerBatch-1) or
       ((InitialStatementType <> stInsert) and (PreparedRowsOfArray > 255)) then begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      Break;
    end else
      PreparedRowsOfArray := J;
  end;

  {EH: now move our data to result ! ONE ALLOC ! of result (: }
  SetLength(Result, StmtLength+LBlockLen);
  PResult := Pointer(Result);
  Put([EBStart], PResult);
  PStmts := PResult + FullHeaderLen+Length(EBBegin);
  for J := 0 to PreparedRowsOfArray do begin
    ParamIndex := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin
        IndexName := IntToRaw(ParamIndex);
        ArrayName := IntToRaw(J);
        Put([':P', IndexName, '_', ArrayName], PStmts);
        if (ParamIndex = 0) and (J=0)
        then Put(['P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult)
        else Put([',P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult);
        Inc(ParamIndex);
      end else
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

procedure isc_decode_time(ntime: ISC_TIME; out hours, minutes, seconds: Word; out fractions: Cardinal);
begin
  hours := ntime div (SecsPerHour * ISC_TIME_SECONDS_PRECISION);
  ntime := ntime mod (SecsPerHour * ISC_TIME_SECONDS_PRECISION);
  minutes := ntime div (SecsPerMin * ISC_TIME_SECONDS_PRECISION);
  ntime := ntime mod (SecsPerMin * ISC_TIME_SECONDS_PRECISION);
  seconds := ntime div ISC_TIME_SECONDS_PRECISION;
  fractions := ntime mod ISC_TIME_SECONDS_PRECISION;
end;

{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "$1" before doing the multiply could prevent overflow errors.} {$ENDIF} // overflow means error so just disable hint
procedure isc_encode_time(var ntime: ISC_TIME; hours, minutes, seconds: Word; fractions: Cardinal);
begin
  ntime := ((hours * MinsPerHour + minutes) * SecsPerMin + seconds) * ISC_TIME_SECONDS_PRECISION + fractions;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

const
  //see https://stackoverflow.com/questions/5248827/convert-datetime-to-julian-date-in-c-sharp-tooadate-safe
  JD_Offset             = 1721119; //This is the Julian Date of March 2nd, 1 BC. Since we moved the 'start' of the calendar from January to March, we use this as our offset
  JDZeroFromGMT         = 2400001; //Julian Date Zero (from noon GMT)
  IB_BaseDateToDay0Diff  = (JDZeroFromGMT-JD_Offset); //number of days from 0/0/0000 to IB/FB base date
  Day0ToIB_BaseDateDiff  = (JD_Offset-JDZeroFromGMT); //number of days from IB/FB base date to 0/0/0000
  DaysOf4YearCycle      = 1461;
  DaysOf400YearsCycle   = 146097; //400 years contain 146097 https://wiki.osdev.org/Julian_Day_Number
  Aug8th                = 153; //8. August

//This formula is taken from the 1939 edition of Funk & Wagnall's College Standard Dictionary (entry for the word "calendar").
//so there is no IB/FB "hokuspokus" to play with encode/decode
{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "$1" before doing the multiply could prevent overflow errors.} {$ENDIF} // overflow means error so just disable hint
procedure isc_decode_date(nday: ISC_DATE; out year, month, day: Word);
var century: integer;
begin
  nday := nday + IB_BaseDateToDay0Diff;
  century := (4 * nday - 1) div DaysOf400YearsCycle;

  nday := 4 * nday - 1 - DaysOf400YearsCycle * century;
  day := nday div 4;

  nday := (4 * day + 3) div DaysOf4YearCycle;
  day  := 4 * day + 3 - DaysOf4YearCycle * nday;
  day := (day + 4) div 4;

  month := (5 * day - 3) div Aug8th;
  day := 5 * day - 3 - Aug8th * month;
  day := (day + 5) div 5;

  year := 100 * century + nday;

  if (month < 10) then
    month := month +3
  else begin
    month := month-9;
    year := year +1;
  end;
end;

procedure isc_encode_date(out nday: ISC_DATE; year, month, day: word);
var century, year_anno: Integer;
begin
  if (month > 2) then
    month := month -3
  else begin
    month := month + 9;
    year := year -1;
  end;

  century := year div 100;
  year_anno := year - 100 * century;
  nday := ((Int64(DaysOf400YearsCycle * century)) div 4 +
           (DaysOf4YearCycle * year_anno) div 4 +
           (Aug8th * month + 2) div 5 + day + Day0ToIB_BaseDateDiff);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure BCD2ScaledOrdinal(const Value: TBCD; Dest: Pointer; DestSize, Scale: Byte);
var
  LastNibbleByteIDX, BCDScale, P, I, F: Byte;
  i64: Int64;
  Negative, LastByteIsHalfByte: boolean;
begin
  LastNibbleByteIDX := (Value.Precision-1) shr 1;
  F := Value.SignSpecialPlaces;
  BCDScale := (F and 63);
  Negative := (F and $80) = $80;
  LastByteIsHalfByte := (Value.Precision and 1 = 1) or ((BCDScale and 1 = 1) and (Value.Fraction[LastNibbleByteIDX] and $0F = 0));
  P := 0;
  i64 := 0;
  { scan for leading zeroes to skip them }
  if LastNibbleByteIDX > 0 then begin
    for I := 0 to LastNibbleByteIDX do begin
      F := Value.Fraction[i];
      if F = 0
      then Inc(P)
      else begin
        i64 := ZBcdNibble2Base100ByteLookup[F];
        Break;
      end;
    end
  end;
  { initialize the Result }
  if P < LastNibbleByteIDX then begin
    for I := P+1 to LastNibbleByteIDX-Ord(LastByteIsHalfByte) do
      i64 := i64 * 100 + ZBcdNibble2Base100ByteLookup[Value.Fraction[i]];
    { last half nibble byte}
    if LastByteIsHalfByte then begin
      i64 := i64 * 10 + Value.Fraction[P+LastNibbleByteIDX] shr 4;
      if (BCDScale and 1 = 1) and (Value.Precision and 1 = 0) then
        Dec(BCDScale);
    end;
    if negative then
      i64 := -i64;
    if BCDScale < Scale then
      i64 := i64 * IBScaleDivisor[BCDScale-scale]
    else if BCDScale > Scale then
      i64 := i64 div IBScaleDivisor[scale-BCDScale]
  end;
  case DestSize of
    8: PInt64(Dest)^ := i64;
    4: PInteger(Dest)^ := i64;
    2: PSmallInt(Dest)^ := i64;
    else PShortInt(Dest)^ := i64;
  end;
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
