{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Constant property names used by connections, datasets   }
{ and transactions. Common dataset and driver-specific    }
{ properties                                              }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2017 Zeos Development Group       }
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

{ WARNING! Some of the parameter values are used directly in DBC API, so they
  must not be changed. }

{ Types of parameters:
    BOOLEAN - 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0 in any case to enable, any other
      value to disable (StrToBoolEx is used to convert)
    INT     - number
    STR     - string }

const
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
  // Type: BOOLEAN
  // Same as TZDatasetOptions.doCachedLobs in Dataset.Options property
  DSProps_CachedLobs = 'CachedLob';
  // Type: INT
  // Same as Statement.ChunkSize, size of chunks for retrieving/sending long data
  // depends to your network speed
  DSProps_ChunkSize = 'chunk_size'; //default is a very low value of 4KB

  { Parameters for datasets }

  // Type: STR, like Field1[, Field2, ...] (separators: "," or ";")
  // List of fields; if defined, they are used for locating and, if WhereMode = KeyOnly,
  // for constructing a WHERE clause
  DSProps_KeyFields = 'KeyFields';
  // Type: BOOLEAN (if not defined: TRUE)
  // Check number of rows affected after executing a statement
  DSProps_ValidateUpdateCount = 'ValidateUpdateCount';

  { Parameters common for several drivers }

{$IF DEFINED(ENABLE_DBLIB) OR DEFINED(ENABLE_INTERBASE)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: STR, like CP_UTF8
  // ?
  DSProps_ResetCodePage = 'ResetCodePage';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_INTERBASE) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_ADO) OR DEFINED(ENABLE_OLEDB)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: INT
  // Size of buffer for results
  DSProps_InternalBufSize = 'internal_buffer_size';
{$IFEND}

{$IF DEFINED(ENABLE_SQLITE) OR DEFINED(ENABLE_POSTGRESQL)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // Treat varchar fields without a length limit as if they had a length limit
  // of <maxlength> thus making these fields usable with TDBEdit components.
  DSProps_UndefVarcharAsStringLength = 'Undefined_Varchar_AsString_Length';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_POSTGRESQL) OR DEFINED(ENABLE_INTERBASE)}
  // Type: STR, like Field1[, Field2, ...] (separators: "," or ";")
  // List of fields which will get their values on INSERT
  // (by INSERT...RETURNING) construction.
  DSProps_InsertReturningFields = 'InsertReturningFields';
{$IFEND}

{$IF DEFINED(ENABLE_ADO) OR DEFINED(ENABLE_OLEDB)}
  // Type: STR
  // ?
  ConnProps_Provider = 'Provider';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
  // Type: BOOLEAN
  // Use trusted connection
  ConnProps_TrustedConnection = 'Trusted_Connection';
  // Type: INT
  // Execution timeout in seconds
  DSProps_StatementTimeOut = 'StatementTimeOut';
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

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

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

{$IFDEF ENABLE_INTERBASE}
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
{$ENDIF}

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
{$ENDIF}

{$IFDEF ENABLE_ORACLE}
  // Type: BOOLEAN
  // If enabled or not specified, sets StatementMode to OCI_STMT_CACHE (refer to Oracle manual for details)
  ConnProps_ServerCachedStmts = 'ServerCachedStmts';
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
  ConnProps_CommLinks = 'CommLinks';
  ConnProps_Links = 'LINKS';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // ?
  DSProps_CachedBlob = 'CachedBlob';
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

{$IFDEF ENABLE_ADO}
  // Type: BOOLEAN
  // ?
  DSProps_UseOLEUpdateParams = 'use_ole_update_params';
{$ENDIF}

implementation

end.