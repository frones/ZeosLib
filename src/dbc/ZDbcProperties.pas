{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Constant property names used by connections, datasets   }
{ and transactions. Common dataset and driver-specific    }
{ properties  written by Fr0sT                            }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcProperties;

interface

{$I ZDbc.inc}

uses ZDbcIntfs, ZCompatibility;

Type
  TZPropertyValueType = (
    pvtEmpty,
    pvtBool,
    pvtEnum,
    pvtNumber,
    pvtString,
    pvtBoolOrString);

  TZPropertyLevelTypes = set of (pltConnection, pltTransaction, pltStatement,
    pltResolver);

  PZPropertyProvider = ^TZPropertyProvider;
  TZPropertyProvider = record
    Provider: TZServerProvider;
    MinimumServerVersion: Integer;
    MinimumClientVersion: Integer;
    MinimumProtocolVersion: Integer;
  end;
  PZPropertyProviderArray = ^TZPropertyProviderArray;
  TZPropertyProviderArray = array[Byte] of TZPropertyProvider;

  TZPropertyProviders = record
    Count: Cardinal;
    Items: PZPropertyProviderArray;
  end;

  PProtocolArray = ^TProtocolArray;
  TProtocolArray = array[Byte] of String;

  TProtocols = record
    Count: Cardinal;
    Items: PProtocolArray;
  end;

  TZPropertyProtocols = record
    Count: Cardinal;
    Items: PZPropertyProviderArray;
  end;

const
  cProptertyTypeDesc: Array[TZPropertyValueType] of String = (
    'no value expected',
    'boolean expresson like ''Y''/''YES''/''T''/''TRUE''/''ON''/<>0 in any case to enable, any other',
    'any enumeration value as described',
    'any ordinal number',
    'any string value',
    'either BOOL expression or string value');
type
  PZProperty = ^TZProperty;
  TZProperty = Record
    Name: String;
    Purpose: String;
    ValueType: TZPropertyValueType;
    LevelTypes: TZPropertyLevelTypes;
    Values, Default, Alias: String;
    Providers: TZPropertyProviders;
    Protocols: TProtocols;
  End;

  PZPropertyRefDynArray = ^TZPropertyRefDynArray;
  TZPropertyRefDynArray = array of PZProperty;

{ WARNING! Some of the parameter values are used directly in DBC API, so they
  must not be changed. }

{ Types of parameters:
    BOOLEAN - 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0 in any case to enable, any other
      value to disable (StrToBoolEx is used to convert)
    INT     - number
    STR     - string }

  { Parameters common for all DBC's }
const
  // Type: STR
  // Same as User property
  ConnProps_UID = 'UID';
  ConnProps_Username = 'username';
  // Type: STR
  // Same as Password property
  ConnProps_PWD = 'PWD';
  ConnProps_Password = 'password';
  // Type: STR
  // Same as TZConnection.LibraryLocation property, path to client lib
  ConnProps_LibLocation = 'LibLocation';
  // Type: STR, like CP_UTF8
  // Codepage to interact with driver
  ConnProps_CodePage = 'codepage';
  // Type: BOOLEAN
  // Same as TZConnection.AutoEncodeStrings property
  ConnProps_AutoEncodeStrings = 'AutoEncodeStrings';
  ConnProps_Transliterate = 'Transliterate';
  // Type: CP_UTF16 | CP_UTF8 | GET_ACP
  // Same as ControlsCodePage property
  ConnProps_ControlsCP = 'controls_cp';
  // Type: CP_UTF8 | GET_ACP
  // Same as ControlsCodePage property
  ConnProps_RawStringEncoding = 'RawStringEncoding';
  // Type: INT
  // The login timeout to use in seconds.
  ConnProps_Timeout = 'timeout';
  // Type: STR
  // Format to display date, like YYYY-MM-DD
  ConnProps_DateDisplayFormat = 'DateDisplayFormat'; //deprecated not used anymore
  // Type: STR
  // Format to read date
  ConnProps_DateReadFormat = 'DateReadFormat';
  // Type: STR
  // Format to write date
  ConnProps_DateWriteFormat = 'DateWriteFormat';
  // Type: STR, like HH:MM:SS
  // Format to display time
  ConnProps_TimeDisplayFormat = 'TimeDisplayFormat'; //deprected not used anymore
  // Type: STR
  // Format to read time
  ConnProps_TimeReadFormat = 'TimeReadFormat';
  // Type: STR
  // Format to write time
  ConnProps_TimeWriteFormat = 'TimeWriteFormat';
  // Type: STR
  // Format to display date & time
  ConnProps_DateTimeDisplayFormat = 'DatetimeDisplayFormat'; //deprected not used anymore
  // Type: STR
  // Format to read date & time
  ConnProps_DateTimeReadFormat = 'DatetimeReadFormat';
  // Type: STR
  // Format to write date & time
  ConnProps_DateTimeWriteFormat = 'DatetimeWriteFormat';
  // Type: STR
  // Sets TZAbstractDatabaseInfo.IdentifierQuotes property, refer to Zeos manual for details
  ConnProps_IdentifierQuotes = 'identifier_quotes';

  { Parameters common for all DBC's }

  { Following parameters are for datasets and statements but could be set for
    connections to influence all linked objects.
    Values of all these parameters are being determined via DefineStatementParameter
    (value from DS/Stmt properties retrieved first; if it is empty, then value
    from Connection properties retrieved. If it is empty too, the default value
    is returned (usually empty string for options of type STR) }

  // Type: all | changed
  // Same as Dataset.UpdateMode property
  DSProps_Update = 'update';
  // Type: all | keyonly
  // Same as Dataset.WhereMode property
  DSProps_Where = 'where';
  // Type: BOOLEAN
  // Same as TZDatasetOptions.doCalcDefaults in Dataset.Options property
  DSProps_Defaults = 'defaults';
  // Type: BOOLEAN
  // Same as TZDatasetOptions.doPreferPrepared in Dataset.Options property
  DSProps_PreferPrepared = 'PreferPrepared';

  { Parameters for datasets }

  // Type: STR, like Field1[, Field2, ...] (separators: "," or ";")
  // List of fields; if defined, they are used for locating and, if WhereMode = KeyOnly,
  // for constructing a WHERE clause
  DSProps_KeyFields = 'KeyFields';
  // Type: BOOLEAN (if not defined: TRUE)
  // Check number of rows affected after executing a statement
  DSProps_ValidateUpdateCount = 'ValidateUpdateCount';

  { Parameters common for several drivers }

{$IF DEFINED(ENABLE_DBLIB) OR DEFINED(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: STR, like CP_UTF8
  // ?
  DSProps_ResetCodePage = 'ResetCodePage';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: INT
  // Size of buffer for results
  DSProps_InternalBufSize = 'internal_buffer_size';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD) OR DEFINED(ENABLE_POSTGRES)}
  // Type: BOOLEAN
  // Same as TZDatasetOptions.doCachedLobs in Dataset.Options property
  DSProps_CachedLobs = 'CachedLob';
  // Type: INT
{$IFEND}

{$IF DEFINED(ENABLE_SQLITE) OR DEFINED(ENABLE_POSTGRESQL)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // Treat varchar fields without a length limit as if they had a length limit
  // of <maxlength> thus making these fields usable with TDBEdit components.
  DSProps_UndefVarcharAsStringLength = 'Undefined_Varchar_AsString_Length';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_POSTGRESQL) OR DEFINED(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  // Type: STR, like Field1[, Field2, ...] (separators: "," or ";")
  // List of fields which will get their values on INSERT
  // (by INSERT...RETURNING) construction.
  DSProps_InsertReturningFields = 'InsertReturningFields';
{$IFEND}

{$IF DEFINED(ENABLE_ADO) OR DEFINED(ENABLE_OLEDB)}
  // Type: STR
  // the ole provider
  ConnProps_Provider = 'Provider';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB) OR DEFINED(ENABLE_ADO)}
  // Type: BOOLEAN
  // Use trusted connection
  ConnProps_TrustedConnection = 'Trusted_Connection';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB) OR DEFINED(ENABLE_FIREBIRD) or DEFINED(ZEOS_DISABLE_INTERBASE)}
  // Type: INT
  // Execution timeout in seconds/milliseconds for FireBird
  DSProps_StatementTimeOut = 'StatementTimeOut'; //since FB4 also
{$IFEND}

{$IF defined (ENABLE_MYSQL) or defined (ENABLE_POSTGRESQL)}
  // Type: INT
  // how many executions must be done to realy prepare the statement?
  // JDBC does prepare on after 4 executions.
  // A negative value means never prepare.
  // actually default is 2 executions before prepare the stmt on the server
  DSProps_MinExecCntBeforePrepare = 'MinExecCountBeforePrepare';
  // Type: BOOLEAN
  // http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
  DSProps_EmulatePrepares = 'emulate_prepares';
{$IFEND}

  { Parameters specific to a single DBC }

{$IFDEF ENABLE_DBLIB}
  // Type: INTEGER
  // If set, the TDS version will be set on connect
  // or dbinit (sybase-lib only)
  ConnProps_TDSVersion = 'TDSVersion';
  // Type: String
  // If set, the TDS version will be set on connect. Lower precedence than 'TDSVersion'
  // or dbinit (sybase-lib only)
  // see: https://www.freetds.org/userguide/choosingtdsprotocol.htm
  ConnProps_TDSProtocolVersion = 'TDSProtocolVersion';
  // Type: BOOLEAN
  // If set, executes 'SET ANSI_PADDING ON' on connect
  ConnProps_AnsiPadding = 'ANSI_PADDING';
  // Type: STR
  // The application name to send to the server
  ConnProps_AppName = 'AppName';
  // Type: STR
  // The language the server should use for messages
  ConnProps_Language = 'language';
  // Type: STR
  // The workstation name to send to the server
  ConnProps_Workstation = 'workstation';
  // Type: BOOLEAN
  // Write log file
  ConnProps_Log = 'log';
  ConnProps_Logging = 'logging';
  ConnProps_TDSDump = 'tds_dump';
  // Type: STR
  // Path to log file. If not set, the <AppPath>\<AppName>.tdslog will be used.
  ConnProps_LogFile = 'logfile';
  ConnProps_Log_File = 'log_file';
  ConnProps_TDSDumpFile = 'tds_dump_file';
  // Type: BOOLEAN
  // Use Windows auth when connecting to server
  ConnProps_NTAuth = 'NTAuth';
  ConnProps_Secure = 'secure';
  ConnProps_Trusted = 'trusted';
{$ENDIF}

{$IFDEF ENABLE_MYSQL}

  // Type: BOOLEAN
  // Use SSL
  ConnProps_MYSQLSSL = 'MYSQL_SSL';
  // Type: BOOLEAN
  // Same as MYSQL_OPT_COMPRESS, refer to MySql manual for details
  ConnProps_Compress = 'compress';
  // Type: BOOLEAN
  // Same as CLIENT_CONNECT_WITH_DB, refer to MySql manual for details
  ConnProps_DBLess = 'dbless';
  // Type: INT
  // Value used in 'SET GLOBAL max_allowed_packet' statement, refer to MySql manual for details
  ConnProps_MaxLobSize = 'MaxLobSize';
  // Type: BOOLEAN
  // Value used to identify BIT(1) as Boolean instead of ENUM('Y','N')
  ConnProps_MySQL_FieldType_Bit_1_IsBoolean = 'MySQL_FieldType_Bit_1_IsBoolean';
  // Type: STR
  // Refer to MySql manual for details
  ConnProps_Datadir = '--datadir';
  // Type: STR
  // Path to library
  ConnProps_Library = 'Library';

  { In addition, any server parameter prefixed by value of
    ZPlainMySqlConstants.SERVER_ARGUMENTS_KEY_PREFIX constant and all members from
    ZPlainMySqlConstants.TMYSQL_CLIENT_OPTIONS and ZPlainMySqlConstants.TMySqlOption
    could be used as well. }

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // Fetching rows one by one using UseResult instead of StoreResult
  // this reduces the memory-consumtion of libmysql.
  // Note mysql is tabular streamed! ->
  // So you can't use it within using metainformations or multiple active
  // resultsets!
  DSProps_UseResult = 'UseResult';
  // Type: INT
  // Sets STMT_ATTR_PREFETCH_ROWS option, refer to MySql manual for details
  DSProps_PrefetchRows = 'prefetch_rows';
  // Type: INT
  // Same as Statement.ChunkSize, size of chunks for retrieving/sending long data
  // depends to your network speed
  DSProps_ChunkSize = 'chunk_size'; //default is a very low value of 4KB
{$ENDIF}

{$IFDEF ENABLE_POSTGRESQL}
  // Type: BOOLEAN
  // If set, metadata query will check if fields are visible (by 'AND pg_table_is_visible (c.oid)')
  // http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=11174
  // http://http://zeoslib.sourceforge.net/viewtopic.php?p=16646&sid=130
  ConnProps_CheckFieldVisibility = 'CheckFieldVisibility';
  // Type: BOOLEAN
  // If not set, use cached table info
  ConnProps_NoTableInfoCache = 'NoTableInfoCache';
  // Type: STR
  // Value used in 'SET standard_conforming_strings = <Value>' query on connect,
  // refer to Postgre manual for details
  ConnProps_StdConformingStrings = 'standard_conforming_strings';
  ConnProps_integer_datetimes = 'integer_datetimes';
  { Parameters used for constructing ConnectStr.
    Refer to Postgre manual for types and acceptable values of these parameters }
  ConnProps_ApplicationName = 'application_name';
  ConnProps_RequireSSL = 'requiressl';
  ConnProps_SSLMode = 'sslmode';
  ConnProps_SSLCert = 'sslcert';
  ConnProps_SSLCompression = 'sslcompression';
  ConnProps_SSLCrl = 'sslcrl';
  ConnProps_SSLKey = 'sslkey';
  ConnProps_SSLRootcert = 'sslrootcert';
  { keepalives by Luca Olivetti }
  ConnProps_keepalives = 'keepalives';
  ConnProps_keepalives_idle = 'keepalives_idle';
  ConnProps_keepalives_interval = 'keepalives_interval';
  ConnProps_keepalives_count = 'keepalives_count';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // compatibility option for users who bind double values to the params
  // even if it should by a NUMERIC complient such as Currency or TBCD
  // If set, we'll bind the Doubles as string with then Unknown OID e.g. 0
  ConnProps_BindDoublesAsString = 'BindDoubleAsString';
  // Type: BOOLEAN
  // Is Oid type treated as Large Object handle (blob) or as a regular integer
  DSProps_OidAsBlob = 'OidAsBlob';
  // Type: BOOLEAN
  // If set, queries will be executed async-ly
  DSProps_ExecAsync = 'execute_async';
  // Type: BOOLEAN
  // fetch row by row from Server -> do not cache the results in libpq
  DSProps_SingleRowMode = 'SingleRowMode';
  // Type: BOOLEAN
  // force binary results to be retieved from server. supported since Protocol V3
  // except libs like pgbouncer which have no pqexecparams/pqexecprepared
  DSProps_BinaryWireResultMode = 'BinaryWireResultMode';
{$ENDIF}

{$IF defined(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  // Type: BOOLEAN
  // If not enabled: all commits are 'soft' (retaining), i.e. transaction isn't closed
  ConnProps_HardCommit = 'hard_commit';
  // Type: 1 | 3, default: 3
  // Dialect of API communication
  ConnProps_Dialect = 'dialect';
  // Type: STR
  // Name of the role the user connects with
  ConnProps_Rolename = 'rolename';
  // Type: BOOLEAN
  // Enable the wire compression in Firebird 3.0 and above.
  // This option generates isc_dpb_config string
  ConnProps_WireCompression = 'WireCompression';
  // Type: STR
  // Create new DB on the given path on connect
  ConnProps_CreateNewDatabase = 'CreateNewDatabase';
  // Type: BOOLEAN
  // Set a type of **all** CHAR(16) CHAR SET OCTETS fields to GUID.
  ConnProps_SetGUIDByType = 'SetGUIDByType';
  // Type: STR, like Domain1[, Domain2, ...] (separators: "," or ";")
  // List of domains; if defined, fields of that domains will get GUID type
  ConnProps_GUIDDomains = 'GUIDDomains';
  // Type: enum, <INET | WNET | XNET | LOCAL>
  // can be used to define the firebird protocol to be used
  // for FB 3.0 this will enable the construction of url style connection strings
  // see firebird 3.0 release notes
  ConnProps_FBProtocol = 'fb_protocol';

  // Type: STR
  // identify the charset "NONE" codepage
  ConnProps_Charset_NONE_CodePage = 'Charset_NONE_CodePage';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: STR, like Field1[, Field2, ...] (separators: "," or ";")
  // List of fields; if defined, fields with these names will get GUID type
  // Be careful using this option on connection level.
  DSProps_GUIDFields = 'GUIDFields';

  { Parameters for datasets }

  // Type: BOOLEAN
  // Set a type of **all** CHAR(16) CHAR SET OCTETS fields to GUID.
  // The effective value of this parameter is also determined via
  // DefineStatementParameter but declared as separate constant to logically
  // distinguish DS-level and DB-level options.
  // In addition, DB-level option affects things besides datasets.
  DSProps_SetGUIDByType = ConnProps_SetGUIDByType;

  { In addition, all isc_dpb_* (connection level) and isc_tpb_* (transaction level)
    parameters could be used as well, refer to Firebird manual for details.
    isc_dpb_config parameter could be used to set several DB or connection options,
    refer to https://firebirdsql.org/file/documentation/release_notes/html/en/3_0/rnfb30-fbconf.html
    for available parameters and values }

  { These parameters are analogs of general ones:
      'isc_dpb_username'        = ConnProps_Username
      'isc_dpb_password'        = ConnProps_Password
      'isc_dpb_lc_ctype'        = ConnProps_CodePage
      'isc_dpb_sql_role_name'   = ConnProps_Rolename
      'isc_dpb_sql_dialect'     = ConnProps_Dialect
      'isc_dpb_connect_timeout' = ConnProps_Timeout }

  { Some of the isc_tpb_* parameters are added internally according to
    Connection.TransactIsolationLevel property }

  // Type: NONE
  TxnProps_isc_tpb_consistency = 'isc_tpb_consistency';
  TxnProps_isc_tpb_concurrency = 'isc_tpb_concurrency';
  TxnProps_isc_tpb_shared = 'isc_tpb_shared';
  TxnProps_isc_tpb_protected = 'isc_tpb_protected';
  TxnProps_isc_tpb_exclusive = 'isc_tpb_exclusive';
  TxnProps_isc_tpb_wait = 'isc_tpb_wait';
  TxnProps_isc_tpb_nowait = 'isc_tpb_nowait';
  TxnProps_isc_tpb_read = 'isc_tpb_read';
  TxnProps_isc_tpb_write = 'isc_tpb_write';
  // Type: String
  TxnProps_isc_tpb_lock_read = 'isc_tpb_lock_read';
  TxnProps_isc_tpb_lock_write = 'isc_tpb_lock_write';
  //not implemented
  TxnProps_isc_tpb_verb_time = 'isc_tpb_verb_time';
  TxnProps_isc_tpb_commit_time = 'isc_tpb_commit_time';
  //Type: None
  TxnProps_isc_tpb_ignore_limbo = 'isc_tpb_ignore_limbo';
  TxnProps_isc_tpb_read_committed = 'isc_tpb_read_committed';
  TxnProps_isc_tpb_autocommit = 'isc_tpb_autocommit';
  TxnProps_isc_tpb_rec_version = 'isc_tpb_rec_version';
  TxnProps_isc_tpb_no_rec_version = 'isc_tpb_no_rec_version';
  TxnProps_isc_tpb_restart_requests = 'isc_tpb_restart_requests';
  TxnProps_isc_tpb_no_auto_undo = 'isc_tpb_no_auto_undo';
  TxnProps_isc_tpb_no_savepoint = 'isc_tpb_no_savepoint';
  //Type: Int
  TxnProps_isc_tpb_lock_timeout = 'isc_tpb_lock_timeout';
  //Type: None
  TxnProps_isc_tpb_read_consistency = 'isc_tpb_read_consistency';
{$IFEND}

{$IFDEF ENABLE_FIREBIRD}
  // Type: INT
  // Session idle timeout in seconds
  ConnProps_SessionIdleTimeOut = 'SesssionIdleTimeOut'; //since FB4
  // Type: INT
  // Execution timeout in seconds
  ConnProps_StatementTimeOut = DSProps_StatementTimeOut;
{$ENDIF ENABLE_FIREBIRD}

{$IFDEF ENABLE_SQLITE}
  // Type: BOOLEAN
  // Encrypt connection
  ConnProps_Encrypted = 'encrypted';
  // Type: INT
  // Sets sqlite3_busy_timeout(), refer to SQLite manual for details
  ConnProps_BusyTimeout = 'busytimeout';

  { Parameters directly executed with PRAGMA statement.
    Refer to SQLite manual for types and acceptable values of these parameters }
  ConnProps_CacheSize = 'cache_size';
  ConnProps_Synchronous = 'synchronous';
  ConnProps_LockingMode = 'locking_mode';
  ConnProps_ForeignKeys = 'foreign_keys';
  ConnProps_journal_mode = 'journal_mode';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // If set, directly use value of date/time/datetime fields. Otherwise, use intermediate string
  DSProps_BindDoubleDateTimeValues = 'BindDoubleDateTimeValues';
  // Type: BOOLEAN
  // If set, directly use value of boolean fields. Otherwise, use intermediate string
  DSProps_BindOrdinalBoolValues = 'BindOrdinalBoolValues';
  // Type: String
  // see sqlite manuals
  // if Value is 'EXCLUSIVE' we're assuming you want emulate a ReadCommitted transaction
  // which blocks read transactions while the transaction is underway
  DSProps_TransactionBehaviour = 'TransactionBehaviour';
{$ENDIF}

{$IFDEF ENABLE_ORACLE}
  // Type: BOOLEAN
  // If enabled or not specified, sets StatementMode to OCI_STMT_CACHE (refer to Oracle manual for details)
  ConnProps_ServerCachedStmts = 'ServerCachedStmts';

  // Type: INT
  // Sets value for OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE option, refer to Oracle manual for details
  ConnProps_BlobPrefetchSize = 'BlobPrefetchSize';
  // Type: INT
  // Sets value for OCI_ATTR_STMTCACHESIZE option, refer to Oracle manual for details
  ConnProps_StatementCache = 'StatementCache';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: INT
  // Sets value for OCI_ATTR_PREFETCH_MEMORY option, refer to Oracle manual for details
  DSProps_RowPrefetchSize = 'row_prefetch_size';

{$ENDIF}

{$IFDEF ENABLE_ASA}
  { Parameters used for constructing ConnectionString.
    Refer to ASA manual for types and acceptable values of these parameters }
    //see: http://infocenter.sybase.com/help/topic/com.sybase.help.sqlanywhere.12.0.1/dbadmin/how-introduction-connect.html
  ConnProps_APP = 'APP';
  ConnProps_AppInfo = 'AppInfo';
  ConnProps_AutoStart = 'AutoStart';
  ConnProps_ASTART = 'ASTART';
  ConnProps_AutoStop = 'AutoStop';
  ConnProps_ASTOP = 'ASTOP';
  {$IFNDEF ENABLE_ODBC}
  ConnProps_CharSet = 'CharSet';
  {$ENDIF}
  ConnProps_CS = 'CS';
  ConnProps_CommBufferSize = 'CommBufferSize';
  ConnProps_CBSIZE = 'CBSIZE';
  ConnProps_CommLinks = 'CommLinks';
  ConnProps_LINKS = 'LINKS';
  {$IFNDEF ENABLE_MYSQL}
  ConnProps_Compress = 'Compress';
  {$ENDIF}
  ConnProps_COMP = 'COMP';
  ConnProps_CompressionThreshold = 'CompressionThreshold';
  ConnProps_COMPTH = 'COMPTH';
  ConnProps_ConnectionName = 'ConnectionName';
  ConnProps_CON = 'CON';
  ConnProps_ConnectionPool = 'ConnectionPool';
  ConnProps_CPOOL = 'CPOOL';
  ConnProps_DatabaseFile = 'DatabaseFile';
  ConnProps_DBF = 'DBF';
  ConnProps_DatabaseKey = 'DatabaseKey';
  ConnProps_DBKEY = 'DBKEY';
  ConnProps_DatabaseName = 'DatabaseName';
  ConnProps_DBN = 'DBN';
  ConnProps_DatabaseSwitches = 'DatabaseSwitches';
  ConnProps_DBS = 'DBS';
  ConnProps_DataSourceName = 'DataSourceName';
  ConnProps_DSN = 'DSN';
  ConnProps_DisableMultiRowFetch = 'DisableMultiRowFetch';
  ConnProps_DMRF = 'DMRF';
  ConnProps_Elevate = 'Elevate';
  ConnProps_EncryptedPassword = 'EncryptedPassword';
  ConnProps_ENP = 'ENP';
  ConnProps_Encryption = 'Encryption';
  ConnProps_ENC = 'ENC';
  ConnProps_EngineName = 'EngineName';
  ConnProps_ENG = 'ENG';
  ConnProps_FileDataSourceName = 'FileDataSourceName';
  ConnProps_FILEDSN = 'FILEDSN';
  ConnProps_ForceStart = 'ForceStart';
  ConnProps_FORCE = 'FORCE';
  ConnProps_Host = 'Host';
  ConnProps_Idle = 'Idle';
  ConnProps_Integrated = 'Integrated';
  ConnProps_INT = 'INT';
  ConnProps_Kerberos = 'Kerberos';
  ConnProps_KRB = 'KRB';
  {$IFNDEF ENABLE_DBLIB}
  ConnProps_Language = 'Language';
  {$ENDIF}
  ConnProps_LANG = 'LANG';
  ConnProps_LazyClose = 'LazyClose';
  ConnProps_LCLOSE = 'LCLOSE';
  ConnProps_LivenessTimeout = 'LivenessTimeout';
  ConnProps_LTO = 'LTO';
  {$IFNDEF ENABLE_DBLIB}
  ConnProps_LogFile = 'LogFile';
  ConnProps_LOG = 'LOG';
  {$ENDIF ENABLE_DBLIB}
  ConnProps_NewPassword = 'NewPassword';
  ConnProps_NEWPWD = 'NEWPWD';
  ConnProps_MatView = 'MatView';
  ConnProps_NodeType = 'NodeType';
  ConnProps_NODE = 'NODE';
  //ConnProps_Password  = 'Password';
  //ConnProps_PWD = 'PWD';
  ConnProps_PrefetchBuffer = 'PrefetchBuffer';
  ConnProps_PBUF = 'PBUF';
  ConnProps_PrefetchOnOpen = 'PrefetchOnOpen';
  ConnProps_PrefetchRows = 'PrefetchRows';
  ConnProps_PROWS = 'PROWS';
  ConnProps_RetryConnectionTimeout = 'RetryConnectionTimeout';
  ConnProps_RetryConnTO = 'RetryConnTO';
  ConnProps_ServerName = 'ServerName';
  ConnProps_Server = 'Server';
  ConnProps_StartLine = 'StartLine';
  ConnProps_START = 'START';
  ConnProps_Unconditional = 'Unconditional';
  ConnProps_UNC = 'UNC';
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }
{$ENDIF}

{$IFDEF ENABLE_OLEDB}
  // Type: INT
  // ?
  ConnProps_TDSPacketSize = 'tds_packed_size';
  // Type: BOOLEAN
  // Support MARS connection
  ConnProps_MarsConn = 'MarsConn';
  // Type: STR
  // User name
  ConnProps_UserId = 'User Id';
  // Type: STR
  // ?
  ConnProps_Initial_Catalog = 'Initial Catalog';
{$ENDIF}

{$IFDEF ENABLE_ODBC}
  // Type: STR, like CP_UTF8
  // Codepage to use (same as ConnProps_CodePage)
  ConnProps_Charset = 'characterset';
  // Type: SQL_DRIVER_COMPLETE | SQL_DRIVER_PROMPT | SQL_DRIVER_COMPLETE_REQUIRED
  // Refer to ODBC manual for details
  ConnProps_DriverCompletion = 'DriverCompletion';
  // Type: BOOLEAN
  // If set, more info about columns will be retrieved
  DSProps_EnhancedColumnInfo = 'enhanced_column_info';
{$ENDIF}

{$IFDEF ENABLE_POOLED}
  { These parameters set the same-named properties of TConnectionPool object,
    refer to Zeos manual for details }
  ConnProps_ConnectionTimeout = 'ConnectionTimeout';
  ConnProps_MaxConnections = 'MaxConnections';
  ConnProps_Wait = 'Wait';
{$ENDIF}


procedure RegisterZProperty(Value: PZProperty);
function GetZProperties: PZPropertyRefDynArray;

implementation

//which of Jan@EH: instead of a constant array, use a dynamic array,
//so others can easaly add it's own properties
var ZPropertyArray: TZPropertyRefDynArray;

function GetZProperties: PZPropertyRefDynArray;
begin
  Result := @ZPropertyArray;
end;

procedure RegisterZProperty(Value: PZProperty);
begin
  SetLength(ZPropertyArray, Length(ZPropertyArray)+1);
  ZPropertyArray[High(ZPropertyArray)] := Value;
end;

const
  ZProp_UID: TZProperty = (
    Name: ConnProps_UID; Purpose: 'the login username (same as username)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Username;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_Username: TZProperty = (
    Name: ConnProps_Username; Purpose: 'the login username (same as UID)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_UID;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_PWD: TZProperty = (
    Name: ConnProps_PWD; Purpose: 'the login password';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Password;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_Password : TZProperty = (
    Name: ConnProps_Password; Purpose: 'the login password';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_PWD;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_LibLocation : TZProperty = (
    Name: ConnProps_LibLocation;
    Purpose: 'the client lib name with full path(optional)';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_CodePage : TZProperty = (
    Name: ConnProps_CodePage;
    Purpose: 'Codepage to interact with driver'+LineEnding+
      'for odbc_a it''s implemented as:'+LineEnding+
      'set a custom codepage to notify zeos about conversion routines note: cp must be equal for all fields else use the W driver.'+LineEnding+
      'first place in a name, second use '':'' for the codepage, third use ''/'' for the maximum amount of bytes per character equal to database defined charset'+LineEnding+
      'example: codepage=latin1:1252/1 or characterset=utf8:65001/4';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_AutoEncodeStrings : TZProperty = (
    Name: ConnProps_AutoEncodeStrings;
    Purpose: 'transliterate between client-characterset and RawStringEncoding'+LineEnding+
      'this option might be interesting for Ansi-Compilers and drivers like SQLite'+LineEnding+
      'the more you can work with ut8 encoding and the database encoding is ansi  or vice versa'+LineEnding+
      '(siltent character conversion with expected character/accedent loss'+LineEnding+
      '2. deprected might be omitted in future(it''s a guesswork). Test the raw encoded strings against UTF8/Ansi encoding';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_Transliterate;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  {ZProp_Transliterate: TZProperty = (
    Name: ConnProps_Transliterate;
    Purpose: 'transliterate between client-characterset and RawStringEncoding'+LineEnding+
      'this option might be interesting for !Ansi!-Compilers and drivers like SQLite'+LineEnding+
      'the more you can work with ut8 encoding and the database encoding is ansi or vice versa';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: 'ConnProps_Transliterate';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );}
  ZProp_ControlsCP : TZProperty = (
    Name: ConnProps_ControlsCP;
    Purpose: //'deprecated use RawStringEncoding instead'+LineEnding+
             'determine the stringField-Types of the dataset and/or '+LineEnding+
             'identify the raw string codepage for W-drivers on non unicode compilers for:'+LineEnding+
             'GetString()/SetString()/GetRawByteString()/SetRawByteString()'+LineEnding+
             'it''s also used for non A-Drivers if String-Translitation is enabled';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'CP_UTF8|GET_ACP'; Default: {$IFDEF LCL}'CP_UTF8'{$ELSE}'GET_ACP'{$ENDIF}; Alias: ConnProps_RawStringEncoding;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  {ZProp_RawStringEncoding : TZProperty = (
    Name: ConnProps_RawStringEncoding;
    Purpose: 'deprecated use RawStringEncoding instead'+LineEnding+
             'determine the stringField-Types of the dataset and/or '+LineEnding+
             'identify the raw string codepage for W-drivers on non unicode compilers for:'+LineEnding+
             'GetString()/SetString()/GetRawByteString()/SetRawByteString()'+LineEnding+
             'it''s also used for non A-Drivers if String-Translitation is enabled';
    ValueType: pvtEnum; LevelTypes: [pltConnection];
    Values: 'CP_UTF8|GET_ACP'; Default: 'false'; Alias: ConnProps_ControlsCP;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );}
  ZProp_Timeout : TZProperty = (
    Name: ConnProps_Timeout;
    Purpose: 'The login timeout to use in seconds.';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '10'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateReadFormat : TZProperty = (
    Name: ConnProps_DateReadFormat;
    Purpose: 'Format to read a date, like YYYY-MM-DD. Just simple Formats are supported.'+LineEnding+
             'Neither centuries nor weekdays and so on.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateWriteFormat : TZProperty = (
    Name: ConnProps_DateWriteFormat;
    Purpose: 'Format to write a date, like YYYY-MM-DD. Just simple formats are supported.'+LineEnding+
             'Neither centuries nor weekdays and so on.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_TimeReadFormat : TZProperty = (
    Name: ConnProps_TimeReadFormat;
    Purpose: 'Format to read time, like HH:MM:SS. Just simple formats are supported.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'HH:MM:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_TimeWriteFormat : TZProperty = (
    Name: ConnProps_TimeWriteFormat;
    Purpose: 'Format to write time, like HH:MM:SS. Just simple formats are supported.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'HH:MM:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateTimeReadFormat : TZProperty = (
    Name: ConnProps_DateTimeReadFormat;
    Purpose: 'Format to read date & time, like YYYY-MM-DD HH:NN:SS.F'+LineEnding+
       'Just simple formats are supported. ISO 8601 is prefered.'+LineEnding+
       'If the driver(f.e.SQLite) supports the ''T''delimiter do not hasitate to use!';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD HH:NN:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_DateTimeWriteFormat : TZProperty = (
    Name: ConnProps_DateTimeWriteFormat;
    Purpose: 'Format to read date & time, like YYYY-MM-DD HH:NN:SS.F'+LineEnding+
       'Just simple formats are supported. ISO 8601 is prefered.'+LineEnding+
       'If the driver(f.e.SQLite) supports the ''T''delimiter do not hasitate to use!';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: 'YYYY-MM-DD HH:NN:SS.F'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_IdentifierQuotes : TZProperty = (
    Name: ConnProps_IdentifierQuotes;
    Purpose: 'Overwrites TZAbstractDatabaseInfo.IdentifierQuotes property, used for Identifier quoting. '+LineEnding+
             'i.e. ADO, OleDB, ODBC, SQLite or Postgres dollar quotes...'+LineEnding+
             'skip it if you don''t use such driver''s'+LineEnding+
             'SQL standart 2003: "" ... wondering about SQL-Server....';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: '""'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_UpdateMode : TZProperty = (
    Name: DSProps_Update;
    Purpose: 'Determine how to genearate the update statement params.'+LineEnding+
      'Using All brings best performance(minior stmt-caching) for tables whith a low field-count -> normailization dude!'+LineEnding+
      'Otherwise the behavior might be vice versa. Same as TZDataset.UpdateMode property';
    ValueType: pvtEnum; LevelTypes: [pltResolver];
    Values: 'all|changed'; Default: 'changed'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_WhereMode : TZProperty = (
    Name: DSProps_Where;
    Purpose: 'Determine how to genearate the where clause. Same as TZDataset.WhereMode property';
    ValueType: pvtEnum; LevelTypes: [pltResolver];
    Values: 'all|keyonly'; Default: 'keyonly'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_CalcDefauls : TZProperty = (
    Name: DSProps_Defaults;
    Purpose: 'Calc defaults for empty columns? It will decrease your performance using it.'+LineEnding+
             'If your table has no default values declared, turn it off!';
    ValueType: pvtBool; LevelTypes: [pltResolver];
    Values: 'false|true'; Default: 'true'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IF declared(DSProps_PreferPrepared)}
  ZProp_PreferPrepared : TZProperty = (
    Name: DSProps_PreferPrepared;
    Purpose: 'Use prepared statements? We recommend you to use this.'+LineEnding+
      'The performance is much better then. Same as TZDatasetOptions.doPreferPrepared in Dataset.Options property'+LineEnding+
      'Drivers like Oracle, SQLite, ASE, Firebird, and Interbase always do prepare the stmt -> this property is ignored.'+LineEnding+
      'For drivers like ODBC, OleDb the property is used as "DEFERPREPARE" see manuals..'+LineEnding+
      'Some servers might fail to prepare the statments(MS-products are master of fails including unknown exceptions) -> turn it off on DataSet/Statement level if you run into that issue';
    ValueType: pvtBool; LevelTypes: [pltStatement];
    Values: 'false|true'; Default: 'true'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IFEND}
  ZProp_KeyFields : TZProperty = (
    Name: DSProps_KeyFields;
    Purpose: 'like Field1[, Field2, ...] (valid separators are: "," or ";")'+LineEnding+
       'List of fields; if defined, they are used for locating and, if WhereMode = KeyOnly,'+LineEnding+
       'for constructing a WHERE clause';
    ValueType: pvtString; LevelTypes: [pltResolver];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_AffectedRows : TZProperty = (
    Name: DSProps_ValidateUpdateCount;
    Purpose: 'Check number of rows affected after executing a statement.'+LineEnding+
      'If the value is different to one an error is raised. Reason is we just update !one! record,'+LineEnding+
      ' and we do not expect to change many or zero rows the stmt did affect! Use a valid primary key!';
    ValueType: pvtBool; LevelTypes: [pltResolver];
    Values: 'false|true'; Default: 'true'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );

{$IF declared(DSProps_InternalBufSize)}
  ZProp_InternalBufSize : TZProperty = (
    Name: DSProps_InternalBufSize;
    Purpose: 'Drivers like ODBC, OleDB, Oracle, ASE(SACAPI) do allow block-fetches to reduce roundtrips.'+LineEnding+
      'Define memory in bytes for the block buffer. Default is 128Kb'+LineEnding+
      'Zeos will do a minimum allocation for one row.';
    ValueType: pvtNumber; LevelTypes: [pltStatement];
    Values: ''; Default: '131072'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IFEND}
{$IF declared(DSProps_CachedLobs)}
  ZProp_CachedLobs : TZProperty = (
    Name: DSProps_CachedLobs;
    Purpose: 'Cache the Lob-Streams? Used for Oracle-Lobs, All IB/FB-lob''s, '+
      'Postgre-OID-lob''s only. All other providers do not support a good '+
      'locator API. Servers like MySQL, ASE do support late-fetching methods '+
      'but we need to refetch the whole row first if the cursor postion changes';
    ValueType: pvtBool; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: 'false'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
{$IFEND}
{$IF declared(DSProps_UndefVarcharAsStringLength)}
  const All_Postgres_SQLite: array[0..1] of String = ('postrgres', 'sqlite');
  ZProp_UndefVarcharAsStringLength : TZProperty = (
    Name: DSProps_UndefVarcharAsStringLength;
    Purpose: 'Treat varchar fields without a length limit as if they had a '+
      'length limit of <maxlength> thus making these fields usable with '+
      'TDBEdit components.';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @All_Postgres_SQLite);
  );
{$IFEND}
{$IF declared(ConnProps_Provider)}
  const AllOleDBAndADO: array[0..1] of String =
    ('OleDB','ADO');
  ZProp_OleDBProvider : TZProperty = (
    Name: ConnProps_Provider;
    Purpose: 'The OleDB-Provider if not spezified in the DataBase-String.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllOleDBAndADO);
  );
{$IFEND}
{$IF declared(ConnProps_TrustedConnection)}
  const AllODBC_OleDB_ADO: array[0..2] of String =
    ('odbc','OleDB','ADO');
  ZProp_TrustedConnection : TZProperty = (
    Name: ConnProps_TrustedConnection;
    Purpose: 'Use trusted connection?';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllODBC_OleDB_ADO);
  );
{$IFEND}
{$IF declared(DSProps_StatementTimeOut)}
  const AllODBC_OleDB_Firebird_Interbase: array[0..3] of String =
    ('odbc','OleDB','firebird','interbase');
  ZProp_StatementTimeOut : TZProperty = (
    Name: DSProps_StatementTimeOut;
    Purpose: 'Execution timeout of a statement.'+LineEnding+
      'Seconds for OleDB and ODBC, Milliseconds for Firebird and Interbase';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: '0'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 4; Items: @AllODBC_OleDB_Firebird_Interbase);
  );
{$IFEND}

{$IF defined (ENABLE_MYSQL) or defined (ENABLE_POSTGRESQL)}
  const AllMySQL_MariaDB_Postgre: array[0..2] of String =
    ('mysql','mariadb','postgres');
  ZProp_MinExecCntBeforePrepare : TZProperty = (
    Name: DSProps_MinExecCntBeforePrepare;
    Purpose: 'How many executions must be done to realy prepare the statement '+
      'on the Server? JDBC does prepare on after 4 executions. A negative '+
      'value means never prepare. Zero means prepare immediately. '+
      'Actually default is 2 executions before prepare the stmt on the server';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: '2'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllMySQL_MariaDB_Postgre);
  );
  ZProp_EmulatePrepares : TZProperty = (
    Name: DSProps_EmulatePrepares;
    Purpose: 'Old postgres before protocol V3 can''t bind paramters. '+
      'MySQL may have known issuse(resolved inbetween? unkown for MariaDB) like: '+
      'https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html '+
      'If enabled turn of parameter bindings and send composed strings instead.'+
      'That''s definitelly killing the performance so have a good reason like:'+LineEnding+
      'http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151';
    ValueType: pvtBool; LevelTypes: [pltConnection, pltStatement];
    Values: ''; Default: 'False'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 3; Items: @AllMySQL_MariaDB_Postgre);
  );
{$IFEND}

{$IFDEF ENABLE_DBLIB}
  const AllSybaseMSSQL: array[0..1] of String = ('sybase','mssql');
  ZProp_TDSVersion : TZProperty = (
    Name: ConnProps_TDSVersion;
    Purpose: '(DBLIB) If set, the TDS version will be set on connect or dbinit '+
      '(sybase-lib only) see ZPlainDbLibDriver.pas "TDSDBVERSION_?"s also see:'+ LineEnding+
      'https://www.freetds.org/userguide/ChoosingTdsProtocol.html'+LineEnding+
      'By default we set the latest protocol version';
    ValueType: pvtNumber; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSProtocolVersion : TZProperty = (
    Name: ConnProps_TDSProtocolVersion;
    Purpose: '(DBLIB) It''s the documtented TDS Protocol Version like ''7.2''. '+
      'Purpose is equal to param '+ConnProps_TDSVersion+'. If set, the TDS '+
      'version will be set on connect or dbinit (sybase-lib only) see:'+ LineEnding+
      'https://www.freetds.org/userguide/ChoosingTdsProtocol.html'+LineEnding+
      'By default we set the latest protocol version';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_AnsiPadding : TZProperty = (
    Name: ConnProps_AnsiPadding;
    Purpose: 'Turn Ansi-Padding on/off. See Server-documentation.'+
      'If set, executes ''SET ANSI_PADDING ON'' on connect';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'OFF|ON'; Default: 'ON'; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_AppName : TZProperty = (
    Name: ConnProps_AppName;
    Purpose: 'The application name to send to the server on connect.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_Language : TZProperty = (
    Name: ConnProps_Language;
    Purpose: 'The language the server should use for messages';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_Workstation : TZProperty = (
    Name: ConnProps_Workstation;
    Purpose: 'The workstation name to send to the server';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLog : TZProperty = (
    Name: ConnProps_Log;
    Purpose: 'Write a TDS log file';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_Logging+','+ConnProps_TDSDump;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLogging : TZProperty = (
    Name: ConnProps_Logging;
    Purpose: 'Write a TDS log file';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_Log+','+ConnProps_TDSDump;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSDump : TZProperty = (
    Name: ConnProps_TDSDump;
    Purpose: 'Write a TDS log file';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_Log+','+ConnProps_Logging;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLogFile : TZProperty = (
    Name: ConnProps_LogFile;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Log_File+','+ConnProps_TDSDumpFile;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSLog_File : TZProperty = (
    Name: ConnProps_Log_File;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_LogFile+','+ConnProps_TDSDumpFile;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSDumpFile : TZProperty = (
    Name: ConnProps_TDSDumpFile;
    Purpose: 'Path to log file. If not set and log/dump is active, '+
      'the <AppPath>\<AppName>.tdslog will be used.';
    ValueType: pvtString; LevelTypes: [pltConnection];
    Values: ''; Default: ''; Alias: ConnProps_Log_File+','+ConnProps_Log_File;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSNTAuth : TZProperty = (
    Name: ConnProps_NTAuth;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_Secure+','+ConnProps_Trusted;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSSecure : TZProperty = (
    Name: ConnProps_Secure;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_NTAuth+','+ConnProps_Trusted;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
  ZProp_TDSTrusted : TZProperty = (
    Name: ConnProps_Trusted;
    Purpose: 'Use Windows auth when connecting to server';
    ValueType: pvtBool; LevelTypes: [pltConnection];
    Values: 'false|true'; Default: 'false'; Alias: ConnProps_NTAuth+','+ConnProps_Secure;
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 2; Items: @AllSybaseMSSQL);
  );
{$ENDIF}
{$IF defined(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  const AllInterbaseAndFireBirebirdProtocols: array[0..1] of String =
    ('firebird','interbase');
  const cInterbaseAndFireBirebirdProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 0;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_InsertReturningFields : TZProperty = (
    Name: DSProps_InsertReturningFields;
    Purpose: 'Field1[, Field2, ...] (allowed separators: "," or ";") '+
      'It''s a list of fields which will get their values on INSERT '+
      '(by INSERT...RETURNING/OUTPUT). Results are set by IZCachedResolver to '+
      'the inserted row '+LineEnding+'Yet implemented for Firebird only.';
    ValueType: pvtString; LevelTypes: [pltResolver];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 0; Items: nil);
    Protocols: (Count: 0; Items: nil);
  );
  ZProp_HardCommit: TZProperty = (
    Name: ConnProps_HardCommit;
    Purpose: 'Don''t use isc_commit_retaining or isc_rollback_retaining call.'+
      'If enabled all record-streams of the txn are closed.'+LineEnding+
      'Note since 7.2.6 ZeosLib uses short-transactions but keeps the retaining '+
      'design by default. If a retained commit/rollback is done the transaction '+
      'is removed from the transaction-manger, and is alive until: no more row '+
      'of a opened IZResultset can be fetched and if there are no more Lob''s '+
      'to read. ZeosLib automatically will try to perform a fetchall and loads '+
      'all data, if possible. You can use cached lob''s to guarantiee all '+
      'lob''s can be read. Then the transaction will end up with a committed '+
      'or rollback as requested. However a new request will create a new '+
      'transaction.';
    ValueType: pvtBool; LevelTypes: [pltConnection, pltTransaction];
    Values: 'false|true'; Default: 'false'; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );

  ZProp_isc_tpb_consistency: TZProperty = (
    Name: TxnProps_isc_tpb_consistency;
    Purpose: 'Table-locking transaction model'+LineEnding+
      'This parameter is used for tiReadCommitted and tiSeriaizable. '+
      'You manually can use this param only if isolation level tiNone is specified.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_concurrency: TZProperty = (
    Name: TxnProps_isc_tpb_concurrency;
    Purpose:  'High throughput, high concurrency transaction with acceptable '+
      'consistency; use of this parameter takes full advantage of the InterBase or Firebird '+
      'multi-generational transaction model [Default]'+LineEnding+
      'This parameter is used for isolationlevel tiRepeatableRead and default for tiNone.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_shared: TZProperty = (
    Name: TXnProps_isc_tpb_shared;
    Purpose: 'Concurrent, shared access of a specified table among all transactions; use '+
      'in conjunction with isc_tpb_lock_read and isc_tpb_lock_write to '+
      'establish the lock option [Default]';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_protected: TZProperty = (
    Name: TxnProps_isc_tpb_protected;
    Purpose: 'Concurrent, restricted access of a specified table; use in conjunction with '+
      'isc_tpb_lock_read and isc_tpb_lock_write to establish the lock option';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_exclusive: TZProperty = (
    Name: TxnProps_isc_tpb_exclusive;
    Purpose: 'Used to specify exclusive table access when calling '+
      'isc_start_transaction() at the API level.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_wait: TZProperty = (
    Name: TxnProps_isc_tpb_wait;
    Purpose: 'Lock resolution specifies that the transaction is to wait until locked '+
      'resources are released before retrying an operation [Default]'+LineEnding+
      'This parameter is default using isolation level tiNone.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_nowait: TZProperty = (
    Name: TxnProps_isc_tpb_nowait;
    Purpose: 'Lock resolution specifies that the transaction is not to wait for '+
      'locks to be released, but instead, a lock conflict error should be '+
      'returned immediately.'+LineEnding+
      'This parameter is used for isolationlevel tiReadCommitted and tiRepeatableRead.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_read: TZProperty = (
    Name: TxnProps_isc_tpb_read;
    Purpose: 'Read-only access mode that allows a transaction only to select '+
      'data from tables'+LineEnding+
      'This parameter is set if your transaction is ReadOnly';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_write: TZProperty = (
    Name: TxnProps_isc_tpb_write;
    Purpose: 'Read-write access mode of that allows a transaction to select, insert, '+
      'update, and delete table data [Default]'+LineEnding+
      'This parameter is set if your transaction is not ReadOnly';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_lock_read: TZProperty = (
    Name: TxnProps_isc_tpb_lock_read;
    Purpose: 'Read-only access of a specified table. Use in conjunction with '+
      'isc_tpb_shared, isc_tpb_protected, and isc_tpb_exclusive to establish the '+
      'lock option.';
    ValueType: pvtString; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_lock_write: TZProperty = (
    Name: TxnProps_isc_tpb_lock_write;
    Purpose: 'Read-write access of a specified table. Use in conjunction with '+LineEnding+
      'isc_tpb_shared, isc_tpb_protected, and isc_tpb_exclusive to establish the '+LineEnding+
      'lock option [Default]';
    ValueType: pvtString; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  cFireBirebird2upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 2000000;
    MinimumClientVersion: 2000000; MinimumProtocolVersion: 0;);
  ZProp_isc_ignore_limbo: TZProperty = (
    Name: TxnProps_isc_tpb_ignore_limbo;
    Purpose: 'With this option, records created by limbo transactions are '+
      'ignored. Transactions are in limbo if the second stage of a two-phase '+
      'commit fails.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFireBirebird2upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_read_committed: TZProperty = (
    Name: TxnProps_isc_tpb_read_committed;
    Purpose: 'With this option, records created by limbo transactions are '+
      'ignored. Transactions are in limbo if the second stage of a two-phase '+
      'commit fails.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_autocommit: TZProperty = (
    Name: TxnProps_isc_tpb_autocommit;
    Purpose: 'This parameter is set if your transaction is set to AutoCommit.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_rec_version: TZProperty = (
    Name: TxnProps_isc_tpb_rec_version;
    Purpose: 'Enables an isc_tpb_read_committed transaction to read the most '+
      'recently committed version of a record even if other, uncommitted '+
      'versions are pending.'+LineEnding+
      'This parameter is set if your isolation level is tiReadCommitted.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_no_rec_version: TZProperty = (
    Name: TxnProps_isc_tpb_no_rec_version;
    Purpose: 'Enables an isc_tpb_read_committed transaction to read only the '+
      'latest committed version of a record. If an uncommitted version of a '+
      'record is pending and isc_tpb_wait is also specified, then the '+
      'transaction waits for the pending record to be committed or rolled back '+
      'before proceeding.'+LineEnding+
      'Otherwise, a lock conflict error is reported at once.';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_restart_requests: TZProperty = (
    Name: TxnProps_isc_tpb_restart_requests;
    Purpose: '<undocumented>';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 2; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  ZProp_isc_tpb_no_auto_undo: TZProperty = (
    Name: TxnProps_isc_tpb_no_auto_undo;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbaseAndFireBirebirdProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  cInterbase7_5upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 7005000;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_isc_no_savepoint: TZProperty = (
    Name: TxnProps_isc_tpb_no_savepoint;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cInterbase7_5upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFireBirebirdProtocols[1]);
  );
  ZProp_isc_tpb_lock_timeout: TZProperty = (
    Name: TxnProps_isc_tpb_lock_timeout;
    Purpose: 'With NO AUTO UNDO, the transaction refrains from keeping the log '+
      'that is normally used to undo changes in the event of a rollback. '+
      'Should the transaction be rolled back after all, other transactions '+
      'will pick up the garbage (eventually). This option can be useful for '+
      'massive insertions that don''t need to be rolled back. For transactions '+
      'that don''t perform any mutations, NO AUTO UNDO makes no difference at all';
    ValueType: pvtNumber; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFireBirebird2upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
  cFireBirebird4upProvider: TZPropertyProvider = (
    Provider: spIB_FB; MinimumServerVersion: 4000000;
    MinimumClientVersion: 0; MinimumProtocolVersion: 0;);
  ZProp_isc_tpb_read_consistency: TZProperty = (
    Name: TxnProps_isc_tpb_read_consistency;
    Purpose: ' Firebird 4.0 release notes pages 26 ("Read Consistency for '+
      'Statements in Read-Committed Transactions") and 28 ("New API Constant '+
      'in the TPB")'+LineEnding+
      'This new isolation level should be the default isolation level for read '+
      'committed transactions on Firebird 4.0';
    ValueType: pvtEmpty; LevelTypes: [pltConnection, pltTransaction];
    Values: ''; Default: ''; Alias: '';
    Providers: (Count: 1; Items: @cFireBirebird4upProvider);
    Protocols: (Count: 1; Items: @AllInterbaseAndFireBirebirdProtocols);
  );
{$IFEND}

initialization
  RegisterZProperty(@ZProp_UID);
  RegisterZProperty(@ZProp_Username);
  RegisterZProperty(@ZProp_PWD);
  RegisterZProperty(@ZProp_Password);
  RegisterZProperty(@ZProp_LibLocation);
  RegisterZProperty(@ZProp_CodePage);
  RegisterZProperty(@ZProp_AutoEncodeStrings);
  //RegisterZProperty(@ZProp_Transliterate);
  RegisterZProperty(@ZProp_ControlsCP);
  //RegisterZProperty(@ZProp_RawStringEncoding);
  RegisterZProperty(@ZProp_Timeout);
  RegisterZProperty(@ZProp_DateReadFormat);
  RegisterZProperty(@ZProp_DateWriteFormat);
  RegisterZProperty(@ZProp_TimeReadFormat);
  RegisterZProperty(@ZProp_TimeWriteFormat);
  RegisterZProperty(@ZProp_DateTimeReadFormat);
  RegisterZProperty(@ZProp_DateTimeWriteFormat);
  RegisterZProperty(@ZProp_IdentifierQuotes);
  RegisterZProperty(@ZProp_UpdateMode);
  RegisterZProperty(@ZProp_WhereMode);
  RegisterZProperty(@ZProp_CalcDefauls);
{$IF declared(DSProps_PreferPrepared)}
  RegisterZProperty(@ZProp_PreferPrepared);
{$IFEND}
  RegisterZProperty(@ZProp_KeyFields);
  RegisterZProperty(@ZProp_AffectedRows);
{$IF declared(ZProp_InternalBufSize)}
  RegisterZProperty(@ZProp_InternalBufSize);
{$IFEND}
{$IF declared(ZProp_CachedLobs)}
  RegisterZProperty(@ZProp_CachedLobs);
{$IFEND}
{$IF declared(ZProp_UndefVarcharAsStringLength)}
  RegisterZProperty(@ZProp_UndefVarcharAsStringLength);
{$IFEND}
{$IF declared(ConnProps_Provider)}
  RegisterZProperty(@ZProp_OleDBProvider);
{$IFEND}
{$IF declared(ZProp_StatementTimeOut)}
  RegisterZProperty(@ZProp_StatementTimeOut);
{$IFEND}
{$IF declared(ZProp_TrustedConnection)}
  RegisterZProperty(@ZProp_TrustedConnection);
{$IFEND}
{$IF declared(ZProp_MinExecCntBeforePrepare)}
  RegisterZProperty(@ZProp_MinExecCntBeforePrepare);
{$IFEND}
{$IF declared(ZProp_MinExecCntBeforePrepare)}
  RegisterZProperty(@ZProp_EmulatePrepares);
{$IFEND}
{$IFDEF ENABLE_DBLIB}
  RegisterZProperty(@ZProp_TDSVersion);
  RegisterZProperty(@ZProp_TDSProtocolVersion);
  RegisterZProperty(@ZProp_AnsiPadding);
  RegisterZProperty(@ZProp_AppName);
  RegisterZProperty(@ZProp_Language);
  RegisterZProperty(@ZProp_Workstation);
  RegisterZProperty(@ZProp_TDSLog);
  RegisterZProperty(@ZProp_TDSLogging);
  RegisterZProperty(@ZProp_TDSDump);
  RegisterZProperty(@ZProp_TDSLogFile);
  RegisterZProperty(@ZProp_TDSLog_File);
  RegisterZProperty(@ZProp_TDSDumpFile);
  RegisterZProperty(@ZProp_TDSNTAuth);
  RegisterZProperty(@ZProp_TDSSecure);
  RegisterZProperty(@ZProp_TDSTrusted);
{$ENDIF}
{$IF defined(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD)}
  RegisterZProperty(@ZProp_InsertReturningFields);
  RegisterZProperty(@ZProp_HardCommit);
  RegisterZProperty(@ZProp_isc_tpb_consistency);
  RegisterZProperty(@ZProp_isc_tpb_concurrency);
  RegisterZProperty(@ZProp_isc_tpb_shared);
  RegisterZproperty(@ZProp_isc_tpb_protected);
  RegisterZProperty(@ZProp_isc_tpb_exclusive);
  RegisterZProperty(@ZProp_isc_tpb_wait);
  RegisterZProperty(@ZProp_isc_tpb_nowait);
  RegisterZProperty(@ZProp_isc_tpb_read);
  RegisterZProperty(@ZProp_isc_tpb_write);
  RegisterZProperty(@ZProp_isc_tpb_lock_read);
  RegisterZProperty(@ZProp_isc_tpb_lock_write);
  RegisterZProperty(@ZProp_isc_ignore_limbo);
  RegisterZProperty(@ZProp_isc_tpb_read_committed);
  RegisterZProperty(@ZProp_isc_tpb_autocommit);
  RegisterZProperty(@ZProp_isc_tpb_rec_version);
  RegisterZProperty(@ZProp_isc_tpb_no_rec_version);
  RegisterZProperty(@ZProp_isc_tpb_restart_requests);
  RegisterZProperty(@ZProp_isc_tpb_no_auto_undo);
  RegisterZProperty(@ZProp_isc_no_savepoint);
  RegisterZProperty(@ZProp_isc_tpb_lock_timeout);
  RegisterZProperty(@ZProp_isc_tpb_read_consistency);
{$IFEND}

end.
