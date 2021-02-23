{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6Utils;

interface

{$I ZDbc.inc}
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} FmtBCD,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZPlainFirebirdInterbaseDriver, ZCompatibility, ZMessages, ZVariant,
  ZDbcLogging, ZDbcProperties;

type
  { Interbase Statement Type }
  TZIbSqlStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProc, stStartTrans, stCommit,
    stRollback, stSelectForUpdate, stSetGenerator, stDisconnect);

  PZInterbaseFirerbirdParam = ^TZInterbaseFirerbirdParam;
  TZInterbaseFirerbirdParam = record
    sqltype:            Cardinal;      { datatype of field (normalized) }
    sqlsubtype:         Cardinal;      { subtype of field (normalized) }
    sqlscale:           Integer;       { scale factor }
    codepage:           word;          { the codepage of the field }
    sqllen:             Cardinal;      { length of data area }
    sqldata:            PAnsiChar;     { address of data }
    sqlind:             PISC_SHORT;    { address of indicator }
    QMarkPosition:      Cardinal;      { the position if the Question Mark in the raw SQL string}
  end;
  PZInterbaseFirerbirdParamArray = ^TZInterbaseFirerbirdParamArray;
  TZInterbaseFirerbirdParamArray = array[byte] of TZInterbaseFirerbirdParam;

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

  { Paparameter string name and it's value}
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
  PIbBlobInfo = ^TIbBlobInfo;
  TIbBlobInfo = record
    NumSegments: Word;
    MaxSegmentSize: Word;
    BlobType: SmallInt;
    TotalSize: Integer;
  end;

  { Base interface for sqlda }
  IZSQLDA = interface(IImmediatelyReleasable)
    ['{2D0D6029-B31C-4E39-89DC-D86D20437C35}']
    function AllocateSQLDA: PXSQLDA;

    function GetData: PXSQLDA;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }

  TZSQLDA = class (TZCodePagedObject, IZSQLDA)
  private
    FXSQLDA: PXSQLDA;
    FPlainDriver: TZInterbasePlainDriver;
    FConnection: IZConnection;
    procedure CheckRange(const Index: Word); {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure IbReAlloc(var P; OldSize, NewSize: Integer);
  public
    constructor Create(const Connection: IZConnection; ConSettings: PZConSettings);
    destructor Destroy; override;
    function AllocateSQLDA: PXSQLDA;

    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldSqlName(const Index: Word): String;
    function GetFieldOwnerName(const Index: Word): String;
    function GetFieldRelationName(const Index: Word): String;
    function GetFieldAliasName(const Index: Word): String;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): SmallInt;
    function GetData: PXSQLDA;

    function GetIbSqlType(const Index: Word): Smallint;
    function GetIbSqlSubType(const Index: Word): Smallint;
    function GetIbSqlLen(const Index: Word): Smallint;
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
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
function BuildPB(PlainDriver: TZInterbaseFirebirdPlainDriver; Info: TStrings; VersionCode: Byte;
  const FilterPrefix: string; const ParamArr: array of TZIbParam
  {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
function GenerateDPB(PlainDriver: TZInterbaseFirebirdPlainDriver; Info: TStrings
  {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
function GenerateTPB(PlainDriver: TZInterbaseFirebirdPlainDriver; Params: TStrings
  {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
procedure GenerateTEB(PHandle: PISC_DB_HANDLE; const TPB: RawByteString; var TEB: TISC_TEB);
function GetInterbase6DatabaseParamNumber(const Value: String): word;
function GetInterbase6TransactionParamNumber(const Value: String): word;

{ Interbase6 errors functions }
function StatusSucceeded(const StatusVector: TARRAY_ISC_STATUS): Boolean; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{ Interbase information functions}
function ConvertInterbase6ToSqlType(SqlType, SqlSubType, Scale, Precision: Integer): TZSqlType;

function GetNameSqlType(Value: Word): RawByteString;

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
  MAX_DPB_PARAMS = 95;
  { prefix database parameters names it used in paramters scann procedure }
  DPBPrefix = 'isc_dpb_';
  { list database parameters and their apropriate numbers }
  DatabaseParams: array [0..MAX_DPB_PARAMS-1] of TZIbParam =
  (
    (Name: 'isc_dpb_cdd_pathname';                  ValueType: pvtNotImpl; Number: isc_dpb_cdd_pathname),
    (Name: 'isc_dpb_allocation';                    ValueType: pvtNotImpl; Number: isc_dpb_allocation),
    (Name: 'isc_dpb_journal';                       ValueType: pvtNotImpl; Number: isc_dpb_journal),
    (Name: ConnProps_isc_dpb_page_size;             ValueType: pvtNum;     Number: isc_dpb_page_size),
    (Name: ConnProps_isc_dpb_num_buffers;           ValueType: pvtNum;     Number: isc_dpb_num_buffers),
    (Name: 'isc_dpb_buffer_length';                 ValueType: pvtNotImpl; Number: isc_dpb_buffer_length),
    (Name: ConnProps_isc_dpb_debug;                 ValueType: pvtNum;     Number: isc_dpb_debug),
    (Name: ConnProps_isc_dpb_garbage_collect;       ValueType: pvtNone;    Number: isc_dpb_garbage_collect),
    (Name: ConnProps_isc_dpb_verify;                ValueType: pvtNum;     Number: isc_dpb_verify),    // Bitmask
    (Name: ConnProps_isc_dpb_sweep;                 ValueType: pvtNum;     Number: isc_dpb_sweep),
    (Name: ConnProps_isc_dpb_enable_journal;        ValueType: pvtString;  Number: isc_dpb_enable_journal),
    (Name: ConnProps_isc_dpb_disable_journal;       ValueType: pvtNone;    Number: isc_dpb_disable_journal),
    (Name: ConnProps_isc_dpb_dbkey_scope;           ValueType: pvtNum;     Number: isc_dpb_dbkey_scope),
    (Name: 'isc_dpb_number_of_users';               ValueType: pvtNotImpl; Number: isc_dpb_number_of_users),
    (Name: ConnProps_isc_dpb_trace;                 ValueType: pvtNone;    Number: isc_dpb_trace),
    (Name: ConnProps_isc_dpb_no_garbage_collect;    ValueType: pvtNone;    Number: isc_dpb_no_garbage_collect),
    (Name: ConnProps_isc_dpb_damaged;               ValueType: pvtNum;     Number: isc_dpb_damaged),
    (Name: ConnProps_isc_dpb_license;               ValueType: pvtString;  Number: isc_dpb_license),
    (Name: ConnProps_isc_dpb_sys_user_name;         ValueType: pvtString;  Number: isc_dpb_sys_user_name),
    (Name: ConnProps_isc_dpb_encrypt_key;           ValueType: pvtString;  Number: isc_dpb_encrypt_key),
    (Name: ConnProps_isc_dpb_activate_shadow;       ValueType: pvtByteZ;   Number: isc_dpb_activate_shadow),
    (Name: ConnProps_isc_dpb_sweep_interval;        ValueType: pvtNum;     Number: isc_dpb_sweep_interval),
    (Name: ConnProps_isc_dpb_delete_shadow;         ValueType: pvtByteZ;   Number: isc_dpb_delete_shadow),
    (Name: ConnProps_isc_dpb_force_write;           ValueType: pvtNum;     Number: isc_dpb_force_write),
    (Name: ConnProps_isc_dpb_begin_log;             ValueType: pvtString;  Number: isc_dpb_begin_log),
    (Name: ConnProps_isc_dpb_quit_log;              ValueType: pvtNone;    Number: isc_dpb_quit_log),
    (Name: ConnProps_isc_dpb_no_reserve;            ValueType: pvtNum;     Number: isc_dpb_no_reserve),
    (Name: ConnProps_isc_dpb_user_name;             ValueType: pvtString;  Number: isc_dpb_user_name),
    (Name: ConnProps_isc_dpb_password;              ValueType: pvtString;  Number: isc_dpb_password),
    (Name: ConnProps_isc_dpb_password_enc;          ValueType: pvtString;  Number: isc_dpb_password_enc),
    (Name: ConnProps_isc_dpb_sys_user_name_enc;     ValueType: pvtString;  Number: isc_dpb_sys_user_name_enc),
    (Name: ConnProps_isc_dpb_interp;                ValueType: pvtNum;     Number: isc_dpb_interp),
    (Name: ConnProps_isc_dpb_online_dump;           ValueType: pvtNum;     Number: isc_dpb_online_dump),
    (Name: ConnProps_isc_dpb_old_file_size;         ValueType: pvtNum;     Number: isc_dpb_old_file_size),
    (Name: ConnProps_isc_dpb_old_num_files;         ValueType: pvtNum;     Number: isc_dpb_old_num_files),
    (Name: ConnProps_isc_dpb_old_file;              ValueType: pvtString;  Number: isc_dpb_old_file),
    (Name: ConnProps_isc_dpb_old_start_page;        ValueType: pvtNum;     Number: isc_dpb_old_start_page),
    (Name: ConnProps_isc_dpb_old_start_seqno;       ValueType: pvtNum;     Number: isc_dpb_old_start_seqno),
    (Name: ConnProps_isc_dpb_old_start_file;        ValueType: pvtNum;     Number: isc_dpb_old_start_file),
    (Name: ConnProps_isc_dpb_drop_walfile;          ValueType: pvtNum;     Number: isc_dpb_drop_walfile),
    (Name: ConnProps_isc_dpb_old_dump_id;           ValueType: pvtNum;     Number: isc_dpb_old_dump_id),
    (Name: ConnProps_isc_dpb_wal_backup_dir;        ValueType: pvtString;  Number: isc_dpb_wal_backup_dir),
    (Name: ConnProps_isc_dpb_wal_chkptlen;          ValueType: pvtNum;     Number: isc_dpb_wal_chkptlen),
    (Name: ConnProps_isc_dpb_wal_numbufs;           ValueType: pvtNum;     Number: isc_dpb_wal_numbufs),
    (Name: ConnProps_isc_dpb_wal_bufsize;           ValueType: pvtNum;     Number: isc_dpb_wal_bufsize),
    (Name: ConnProps_isc_dpb_wal_grp_cmt_wait;      ValueType: pvtNum;     Number: isc_dpb_wal_grp_cmt_wait),
    (Name: ConnProps_isc_dpb_lc_messages;           ValueType: pvtString;  Number: isc_dpb_lc_messages),
    (Name: ConnProps_isc_dpb_lc_ctype;              ValueType: pvtString;  Number: isc_dpb_lc_ctype),
    (Name: 'isc_dpb_cache_manager';                 ValueType: pvtNotImpl; Number: isc_dpb_cache_manager),
    (Name: ConnProps_isc_dpb_shutdown;              ValueType: pvtNum;     Number: isc_dpb_shutdown), // Bitmask
    (Name: ConnProps_isc_dpb_online;                ValueType: pvtNone;    Number: isc_dpb_online),
    (Name: ConnProps_isc_dpb_shutdown_delay;        ValueType: pvtNum;     Number: isc_dpb_shutdown_delay),
    (Name: ConnProps_isc_dpb_reserved;              ValueType: pvtNone;    Number: isc_dpb_reserved),
    (Name: ConnProps_isc_dpb_overwrite;             ValueType: pvtNone;    Number: isc_dpb_overwrite),
    (Name: ConnProps_isc_dpb_sec_attach;            ValueType: pvtNone;    Number: isc_dpb_sec_attach),
    (Name: ConnProps_isc_dpb_disable_wal;           ValueType: pvtNone;    Number: isc_dpb_disable_wal),
    (Name: ConnProps_isc_dpb_connect_timeout;       ValueType: pvtNum;     Number: isc_dpb_connect_timeout),
    (Name: ConnProps_isc_dpb_dummy_packet_interval; ValueType: pvtNum;     Number: isc_dpb_dummy_packet_interval),
    (Name: ConnProps_isc_dpb_gbak_attach;           ValueType: pvtNone;    Number: isc_dpb_gbak_attach),
    (Name: ConnProps_isc_dpb_sql_role_name;         ValueType: pvtString;  Number: isc_dpb_sql_role_name),
    (Name: ConnProps_isc_dpb_set_page_buffers;      ValueType: pvtNum;     Number: isc_dpb_set_page_buffers),
    (Name: ConnProps_isc_dpb_working_directory;     ValueType: pvtString;  Number: isc_dpb_working_directory),
    (Name: ConnProps_isc_dpb_sql_dialect;           ValueType: pvtNum;     Number: isc_dpb_SQL_dialect),
    (Name: ConnProps_isc_dpb_set_db_readonly;       ValueType: pvtNone;    Number: isc_dpb_set_db_readonly),
    (Name: ConnProps_isc_dpb_set_db_sql_dialect;    ValueType: pvtNum;     Number: isc_dpb_set_db_SQL_dialect),
    (Name: ConnProps_isc_dpb_gfix_attach;           ValueType: pvtNone;    Number: isc_dpb_gfix_attach),
    (Name: ConnProps_isc_dpb_gstat_attach;          ValueType: pvtNone;    Number: isc_dpb_gstat_attach),
    (Name: ConnProps_isc_dpb_set_db_charset;        ValueType: pvtString;  Number: isc_dpb_set_db_charset),
    (Name: ConnProps_isc_dpb_gsec_attach;           ValueType: pvtNone;    Number: isc_dpb_gsec_attach),
    (Name: ConnProps_isc_dpb_address_path;          ValueType: pvtString;  Number: isc_dpb_address_path),
    (Name: ConnProps_isc_dpb_process_id;            ValueType: pvtNum;     Number: isc_dpb_process_id),
    (Name: ConnProps_isc_dpb_no_db_triggers;        ValueType: pvtNone;    Number: isc_dpb_no_db_triggers),
    (Name: ConnProps_isc_dpb_trusted_auth;          ValueType: pvtNone;    Number: isc_dpb_trusted_auth),
    (Name: ConnProps_isc_dpb_process_name;          ValueType: pvtString;  Number: isc_dpb_process_name),
    (Name: ConnProps_isc_dpb_trusted_role;          ValueType: pvtString;  Number: isc_dpb_trusted_role),
    (Name: ConnProps_isc_dpb_org_filename;          ValueType: pvtString;  Number: isc_dpb_org_filename),
    (Name: ConnProps_isc_dpb_utf8_filename;         ValueType: pvtNone;    Number: isc_dpb_utf8_filename),
    (Name: ConnProps_isc_dpb_ext_call_depth;        ValueType: pvtNum;     Number: isc_dpb_ext_call_depth),
    (Name: ConnProps_isc_dpb_auth_block;            ValueType: pvtString;  Number: isc_dpb_auth_block), // Bytes
    (Name: ConnProps_isc_dpb_client_version;        ValueType: pvtString;  Number: isc_dpb_client_version),
    (Name: ConnProps_isc_dpb_remote_protocol;       ValueType: pvtString;  Number: isc_dpb_remote_protocol),
    (Name: ConnProps_isc_dpb_host_name;             ValueType: pvtString;  Number: isc_dpb_host_name),
    (Name: ConnProps_isc_dpb_os_user;               ValueType: pvtString;  Number: isc_dpb_os_user),
    (Name: ConnProps_isc_dpb_specific_auth_data;    ValueType: pvtString;  Number: isc_dpb_specific_auth_data),
    (Name: ConnProps_isc_dpb_auth_plugin_list;      ValueType: pvtString;  Number: isc_dpb_auth_plugin_list),
    (Name: ConnProps_isc_dpb_auth_plugin_name;      ValueType: pvtString;  Number: isc_dpb_auth_plugin_name),
    (Name: ConnProps_isc_dpb_config;                ValueType: pvtString;  Number: isc_dpb_config),
    (Name: ConnProps_isc_dpb_nolinger;              ValueType: pvtNone;    Number: isc_dpb_nolinger),
    (Name: ConnProps_isc_dpb_reset_icu;             ValueType: pvtNone;    Number: isc_dpb_reset_icu),
    (Name: ConnProps_isc_dpb_map_attach;            ValueType: pvtNone;    Number: isc_dpb_map_attach),
    (Name: ConnProps_isc_dpb_session_time_zone;     ValueType: pvtString;  Number: isc_dpb_session_time_zone), // this is an assumption and needs to be tested!
    (Name: ConnProps_isc_dpb_set_db_replica;        ValueType: pvtNone;    Number: isc_dpb_set_db_replica),      // I have no clue how to use that
    (Name: ConnProps_isc_dpb_set_bind;              ValueType: pvtString;  Number: isc_dpb_set_bind),
    (Name: ConnProps_isc_dpb_decfloat_round;        ValueType: pvtString;  Number: isc_dpb_decfloat_round),
    (Name: ConnProps_isc_dpb_decfloat_traps;        ValueType: pvtString;  Number: isc_dpb_decfloat_traps)
  );

  { count transaction parameters }
  MAX_TPB_PARAMS = 23;
  { prefix transaction parameters names it used in paramters scann procedure }
  TPBPrefix = 'isc_tpb_';
  { list transaction parameters and their apropriate numbers }
  TransactionParams: array [0..MAX_TPB_PARAMS-1] of TZIbParam =
  (
    (Name: TxnProps_isc_tpb_consistency;      ValueType: pvtNone;    Number: isc_tpb_consistency),
    (Name: TXnProps_isc_tpb_concurrency;      ValueType: pvtNone;    Number: isc_tpb_concurrency),
    (Name: TXnProps_isc_tpb_shared;           ValueType: pvtNone;    Number: isc_tpb_shared),
    (Name: TxnProps_isc_tpb_protected;        ValueType: pvtNone;    Number: isc_tpb_protected),
    (Name: TxnProps_isc_tpb_exclusive;        ValueType: pvtNone;    Number: isc_tpb_exclusive),
    (Name: TxnProps_isc_tpb_wait;             ValueType: pvtNone;    Number: isc_tpb_wait),
    (Name: TxnProps_isc_tpb_nowait;           ValueType: pvtNone;    Number: isc_tpb_nowait),
    (Name: TxnProps_isc_tpb_read;             ValueType: pvtNone;    Number: isc_tpb_read),
    (Name: TxnProps_isc_tpb_write;            ValueType: pvtNone;    Number: isc_tpb_write),
    (Name: TxnProps_isc_tpb_lock_read;        ValueType: pvtString;  Number: isc_tpb_lock_read),
    (Name: TxnProps_isc_tpb_lock_write;       ValueType: pvtString;  Number: isc_tpb_lock_write),
    (Name: TxnProps_isc_tpb_verb_time;        ValueType: pvtNotImpl; Number: isc_tpb_verb_time),
    (Name: TxnProps_isc_tpb_commit_time;      ValueType: pvtNotImpl; Number: isc_tpb_commit_time),
    (Name: TxnProps_isc_tpb_ignore_limbo;     ValueType: pvtNone;    Number: isc_tpb_ignore_limbo),
    (Name: TxnProps_isc_tpb_read_committed;   ValueType: pvtNone;    Number: isc_tpb_read_committed),
    (Name: TxnProps_isc_tpb_autocommit;       ValueType: pvtNone;    Number: isc_tpb_autocommit),
    (Name: TxnProps_isc_tpb_rec_version;      ValueType: pvtNone;    Number: isc_tpb_rec_version),
    (Name: TxnProps_isc_tpb_no_rec_version;   ValueType: pvtNone;    Number: isc_tpb_no_rec_version),
    (Name: TxnProps_isc_tpb_restart_requests; ValueType: pvtNone;    Number: isc_tpb_restart_requests),
    (Name: TxnProps_isc_tpb_no_auto_undo;     ValueType: pvtNone;    Number: isc_tpb_no_auto_undo),
    // IB75+
    (Name: TxnProps_isc_tpb_no_savepoint;     ValueType: pvtNone;    Number: isc_tpb_no_savepoint),
    // FB20+
    (Name: TxnProps_isc_tpb_lock_timeout;     ValueType: pvtNum;     Number: isc_tpb_lock_timeout),
    // FB40+
    (Name: TxnProps_isc_tpb_read_consistency; ValueType: pvtNone;    Number: isc_tpb_read_consistency)
  );

//ported  from NoThrowTimeStamp.cpp

procedure isc_decode_time(ntime: TISC_TIME; out hours, minutes, seconds: Word; out fractions: Cardinal);
procedure isc_encode_time(var ntime: TISC_TIME; hours, minutes, seconds: Word; fractions: Cardinal);
procedure isc_decode_date(nday: TISC_DATE; out year, month, day: Word);
procedure isc_encode_date(out nday: TISC_DATE; year, month, day: Word);

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes]
  and increments buffer pointer skipping read data.
  @param PlainDriver a Interbase Plain drver
  @param pBuf - pointer to a buffer returned by driver. After the function it points to the next block.
  @return - a number read
}
function ReadInterbase6NumberWithInc(const PlainDriver: TZInterbaseFirebirdPlainDriver; var pBuf: PAnsiChar): Integer;

{**
  Read Interbase number (1..4 bytes) from buffer in standard format: [Len * 2 bytes][Number * Len bytes].
  Function accepts constant pointer for easier usage with single reads.
  @param PlainDriver a Interbase Plain drver
  @param Buffer - a buffer returned by driver
  @return - a number read
}
function ReadInterbase6Number(const PlainDriver: TZInterbasePlainDriver; const Buffer): Integer; {$IFDEF WITH_INLINE} inline;{$ENDIF}

//procedure ScaledOrdinal2Raw(const Value: Int128; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
//procedure ScaledOrdinal2Raw(const Value: UInt128; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(const Value: Int64; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(const Value: UInt64; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(Value: Integer; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;
procedure ScaledOrdinal2Raw(Value: Cardinal; Buf: PAnsiChar; PEnd: PPAnsiChar; Scale: Byte); overload;

procedure ScaledOrdinal2Unicode(const Value: Int64; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(const Value: UInt64; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(Value: Integer; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;
procedure ScaledOrdinal2Unicode(Value: Cardinal; Buf: PWideChar; PEnd: ZPPWideChar; Scale: Byte); overload;

procedure BCD2ScaledOrdinal({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD;
  Dest: Pointer; DestSize, Scale: Byte);

function XSQLDA_LENGTH(Value: LongInt): LongInt;
function XSQLDA_LENGTH_V2(Value: LongInt): LongInt;

{**
   Convert pointer to raw database string to compiler-native string
}
function ConvertConnRawToString({$IFDEF UNICODE}ConSettings: PZConSettings;{$ENDIF}
  Buffer: Pointer; BufLen: Integer): string; overload;
function ConvertConnRawToString({$IFDEF UNICODE}ConSettings: PZConSettings;{$ENDIF}
  Buffer: Pointer): string; overload;

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit

uses
  Variants, Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZSysUtils, ZDbcUtils, ZEncoding, ZClasses,
  ZDbcInterbase6;

function XSQLDA_LENGTH(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

function XSQLDA_LENGTH_V2(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA_V2) + ((Value - 1) * SizeOf(TXSQLVAR_V2));
end;

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
function BuildPB(PlainDriver: TZInterbaseFirebirdPlainDriver; Info: TStrings;
  VersionCode: Byte; const FilterPrefix: string; const ParamArr: array of TZIbParam
  {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
var Writer: TZRawSQLStringWriter;

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
var
  I, IntValue: Integer;
  ParamName: String;
  ParamValue: String;
  tmp: RawByteString;
  PParam: PZIbParam;
begin
  Result := EmptyRaw;
  Writer := TZRawSQLStringWriter.Create(1024);
  try
    Writer.AddChar(AnsiChar(VersionCode), Result);
    for I := 0 to Info.Count - 1 do begin
      ExtractParamNameAndValue(Info.Strings[I], ParamName, ParamValue);
      if ZFastCode.Pos(FilterPrefix, ParamName) <> 1 then
        Continue;
      PParam := FindPBParam(ParamName, ParamArr);
      if PParam = nil then
        raise EZSQLException.CreateFmt('Unknown PB parameter "%s"', [ParamName]);

      case PParam.ValueType of
        pvtNone: begin
            Writer.AddChar(AnsiChar(PParam.Number), Result);
            if VersionCode < isc_tpb_version3 then
              Writer.AddChar(AnsiChar(#0), Result);
          end;
        pvtByteZ: begin
            Writer.AddChar(AnsiChar(PParam.Number), Result);
            Writer.AddChar(AnsiChar(#1), Result);
            Writer.AddChar(AnsiChar(#0), Result);
          end;
        pvtNum:
          begin
            Writer.AddChar(AnsiChar(PParam.Number), Result);
            IntValue := StrToInt(ParamValue);
            case IntValue of
              0..High(Byte): begin
                  Writer.AddChar(AnsiChar(Byte(1)), Result);
                  Writer.AddChar(AnsiChar(Byte(IntValue)), Result);
                end;
              High(Byte)+1..High(Word): begin
                  Writer.AddChar(AnsiChar(Byte(2)), Result);
                  PWord(@IntValue)^ := Word(IntValue);
                  PWord(@IntValue)^ := Word(PlainDriver.isc_vax_integer(@IntValue, 2));
                  Writer.AddText(@IntValue, 2, Result);
                end;
              else begin
                  Writer.AddChar(AnsiChar(Byte(4)), Result);
                  IntValue := Cardinal(PlainDriver.isc_vax_integer(@IntValue, 4));
                  Writer.AddText(@IntValue, 4, Result);
                end;
            end;
          end;
        pvtString:
          begin
            {$IFDEF UNICODE}
            tmp := ZUnicodeToRaw(ParamValue, CP);
            {$ELSE}
            tmp := ParamValue;
            {$ENDIF}
            Writer.AddChar(AnsiChar(PParam.Number), Result);
            Writer.AddChar(AnsiChar(Length(tmp)), Result);
            Writer.AddText(tmp, Result);
          end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //pvtUnimpl
      end;
    end;
    Writer.Finalize(Result);
  finally
    FreeAndNil(Writer);
  end;
end;

{**
  Generate database connection string by connection information

  @param PlainDriver - a Interbase Plain drver
  @param Info - a list connection interbase parameters
  @return a generated string
}
function GenerateDPB(PlainDriver: TZInterbaseFirebirdPlainDriver; Info: TStrings
  {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
begin
  Result := BuildPB(PlainDriver, Info, isc_dpb_version1, DPBPrefix,
    DatabaseParams {$IFDEF UNICODE},CP{$ENDIF});
end;

{**
  Generate transaction string by connection information

  @param PlainDriver - a Interbase Plain drver
  @param Params - a transaction parameters list
  @return a generated string
}
function GenerateTPB(PlainDriver: TZInterbaseFirebirdPlainDriver;
  Params: TStrings {$IFDEF UNICODE};CP: Word{$ENDIF}): RawByteString;
begin
  Result := BuildPB(PlainDriver, Params, isc_tpb_version3, TPBPrefix, TransactionParams
  {$IFDEF UNICODE},CP{$ENDIF});
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
function ReadInterbase6NumberWithInc(const PlainDriver: TZInterbaseFirebirdPlainDriver; var pBuf: PAnsiChar): Integer;
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
function ConvertInterbase6ToSqlType(SqlType, SqlSubType, Scale, Precision: Integer): TZSQLType;
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
    blr_sql_time_tz,
    blr_ex_time_tz,
    blr_sql_time: Result := stTime;
    blr_timestamp_tz,
    blr_ex_timestamp_tz,
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
    blr_dec64: Result := stDouble; //assume the DEC16 has 64 bit and an 16digit mantissa whereas a double has 15digit mantissa..Exaptable?
    blr_dec128: Result := stString; //yet defined, we have no Soft-128Bit Decimal128 IEEE 754
    blr_int128: Result := stBigDecimal;
    else
      Result := ZDbcIntfs.stUnknown;
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
   Convert pointer to raw database string to compiler-native string
}
function ConvertConnRawToString({$IFDEF UNICODE}ConSettings: PZConSettings;{$ENDIF}
  Buffer: Pointer; BufLen: Integer): string; overload;
{$IFDEF UNICODE}
var CP: Word;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  if (ConSettings <> nil) and (ConSettings.ClientCodePage <> nil)
  then CP := ConSettings^.ClientCodePage^.CP
  else CP := DefaultSystemCodePage;
  Result := PRawToUnicode(Buffer, BufLen, CP);
  {$ELSE}
  Result := '';
  System.SetString(Result, PAnsiChar(Buffer), BufLen);
  {$ENDIF}
end;

{**
   Convert zero-terminated raw database string to compiler-native string
}
function ConvertConnRawToString({$IFDEF UNICODE}ConSettings: PZConSettings;{$ENDIF}
  Buffer: Pointer): string; overload;
begin
  Result := ConvertConnRawToString({$IFDEF UNICODE} ConSettings,{$ENDIF}
    Buffer, StrLen(Buffer));
end;

{**
  Checks if Interbase status vector indicates successful operation.
  @param StatusVector a status vector

  @return flag of success
}
function StatusSucceeded(const StatusVector: TARRAY_ISC_STATUS): Boolean;
begin
  Result := not ((StatusVector[0] = isc_arg_gds) and (StatusVector[1] > isc_arg_end));
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
constructor TZSQLDA.Create(const Connection: IZConnection; ConSettings: PZConSettings);
begin
  FConnection := Connection;
  Self.ConSettings := ConSettings;
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
  FreeMem(FXSQLDA);
  inherited Destroy;
end;

{**
   Chech range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZSQLDA.CheckRange(const Index: Word);
begin
  Assert(Index < Word(FXSQLDA.sqln), 'Out of Range.');
end;

{$IFDEF UNICODE}
function ConvertConnRawToStringWithOpt(ConSettings: PZConSettings; Buffer: Pointer; BufLen: Integer): string;
{$ELSE}
function ConvertConnRawToStringWithOpt(Buffer: Pointer; BufLen: Integer): string;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  if ConSettings^.ClientCodePage^.ID = CS_NONE
  then Result := PRawToUnicode(Buffer, BufLen, zCP_UTF8)
  else Result := PRawToUnicode(Buffer, BufLen, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
  Result := '';
  SetString(Result, PChar(Buffer), BufLen)
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
  Result := ConvertConnRawToStringWithOpt({$IFDEF UNICODE}ConSettings,{$ENDIF}
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
    SQL_DEC16, SQL_DEC34,
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_BOOLEAN, SQL_BOOLEAN_FB:
      Result := stBoolean;
    SQL_TIMESTAMP_TZ_EX,
    SQL_TIMESTAMP_TZ,
    SQL_DATE: Result := stTimestamp;
    SQL_TIME_TZ_EX,
    SQL_TIME_TZ,
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
    SQL_DEC_FIXED, SQL_INT128: Result := stBigDecimal;
    else  Result := stString;
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
  Result := ConvertConnRawToStringWithOpt({$IFDEF UNICODE}ConSettings,{$ENDIF}
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
  Result := ConvertConnRawToStringWithOpt({$IFDEF UNICODE)}ConSettings,{$ENDIF}
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
  Result := ConvertConnRawToStringWithOpt({$IFDEF UNICODE}ConSettings,{$ENDIF}
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

procedure isc_decode_time(ntime: TISC_TIME; out hours, minutes, seconds: Word; out fractions: Cardinal);
begin
  hours := ntime div (SecsPerHour * ISC_TIME_SECONDS_PRECISION);
  ntime := ntime mod (SecsPerHour * ISC_TIME_SECONDS_PRECISION);
  minutes := ntime div (SecsPerMin * ISC_TIME_SECONDS_PRECISION);
  ntime := ntime mod (SecsPerMin * ISC_TIME_SECONDS_PRECISION);
  seconds := ntime div ISC_TIME_SECONDS_PRECISION;
  fractions := ntime mod ISC_TIME_SECONDS_PRECISION;
end;

{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting the operands to "$1" before doing the multiply could prevent overflow errors.} {$ENDIF} // overflow means error so just disable hint
procedure isc_encode_time(var ntime: TISC_TIME; hours, minutes, seconds: Word; fractions: Cardinal);
begin
  ntime := ((hours * Word(MinsPerHour) + minutes) * Word(SecsPerMin) + seconds) * Word(ISC_TIME_SECONDS_PRECISION) + fractions;
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
{$IFDEF FPC} {$PUSH}
  {$WARN 4080 off : Converting the operands to "$1" before doing the substract could prevent overflow errors.}
  {$WARN 4081 off : Converting the operands to "$1" before doing the multiply could prevent overflow errors.}
{$ENDIF} // overflow means error so just disable hint
procedure isc_decode_date(nday: TISC_DATE; out year, month, day: Word);
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

procedure isc_encode_date(out nday: TISC_DATE; year, month, day: word);
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

procedure BCD2ScaledOrdinal({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD;
  Dest: Pointer; DestSize, Scale: Byte);
var
  LastNibbleByteIDX, BCDScale, P, I, F: Byte;
  i64: Int64;
  Negative, LastByteIsHalfByte: boolean;
label finalize;
begin
  LastNibbleByteIDX := (Value.Precision-1) shr 1;
  F := Value.SignSpecialPlaces;
  BCDScale := (F and 63);
  Negative := (F and $80) = $80;
  LastByteIsHalfByte := (Value.Precision and 1 = 1) or ((BCDScale and 1 = 1) and (Value.Fraction[LastNibbleByteIDX] and $0F = 0));
  P := 0;
  i64 := 0;
  { scan for leading zeroes to skip them }
  for I := 0 to LastNibbleByteIDX do begin
    F := Value.Fraction[i];
    if F = 0
    then Inc(P)
    else begin
      i64 := ZBcdNibble2Base100ByteLookup[F];
      if P = LastNibbleByteIDX
      then goto finalize
      else Break;
    end;
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
finalize:
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

initialization
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
end.
