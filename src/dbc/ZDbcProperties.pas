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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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

  { Parameters common for all DBC's }
const
  /// <type>String</type>
  /// <Alias>username</Alias>
  /// <Associates>ConnProps_Username,ConnProps_TrustedConnection</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_UID]=userid</syntax>
  /// <summary>Specifies the user ID used to log in to the database.</summary>
  /// <remarks>
  ///  You must always supply a user ID when connecting to a database, unless
  ///  you are using an integrated/trusted or Kerberos login.
  /// </remarks>
  ConnProps_UID = 'UID';
  /// <type>String</type>
  /// <alias>ConnProps_UID</alias>
  /// <Associates>ConnProps_UID,ConnProps_TrustedConnection</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_Username]=username</syntax>
  /// <summary>Specifies the user name used to log in to the database.</summary>
  /// <remarks>
  ///  You must always supply a user name when connecting to a database, unless
  ///  you are using an integrated/trusted or Kerberos login.
  /// </remarks>
  ConnProps_Username = 'username';
  /// <type>String</type>
  /// <alias>ConnProps_Password</alias>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_PWD]=password</syntax>
  /// <summary>
  ///  Provides a password for a connection.
  /// </summary>
  /// <remarks>
  ///  The Password (PWD) connection parameter is not encrypted. An application
  ///  can include the password in the connection string. If both the Password
  ///  (PWD) connection parameter and the EncryptedPassword (ENP) (if supported)
  ///  connection parameter are specified, the Password (PWD) connection
  ///  parameter takes precedence.
  /// </remarks>
  ConnProps_PWD = 'PWD';
  /// <type>String</type>
  /// <alias>ConnProps_PWD</alias>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_Password]=password</syntax>
  /// <summary>
  ///  Provides a password for a connection.
  /// </summary>
  /// <remarks>
  ///  The Password (PWD) connection parameter is not encrypted. An application
  ///  can include the password in the connection string. If both the Password
  ///  (PWD) connection parameter and the EncryptedPassword (ENP) (if supported)
  ///  connection parameter are specified, the Password (PWD) connection
  ///  parameter takes precedence.
  /// </remarks>
  ConnProps_Password = 'password';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_LibLocation]=filename</syntax>
  /// <summary>Provides a filename for a client library.</summary>
  /// <remarks>If provided then we'll try to load the library otherwise the
  ///  default lib-names will be used for.
  /// </remarks>
  ConnProps_LibLocation = 'LibLocation';
  /// <Type>String</Type>
  /// <usage>Connection</usage>
  /// <Associates>ConnProps_Charset_NONE_Alias</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_CodePage]={value}</syntax>
  /// <summary>Specifies the character set to interact with driver.</summary>
  /// <remarks>If you're accessing a CharacterSet "NONE" Firebird/Interbase
  ///  database you should always use the CharacterSet "NONE" as connection
  ///  characterset. In addition it's recommented to specify which characterset
  ///  the "NONE" represents by adding:
  ///  <c>Properties.Values['Charset_NONE_Alias'] := 'WIN1251'</c> f.e.
  ///  For odbc_a and ole_db(raw longvarchar only) it's implemented as:
  ///  set a custom characterset to notify zeos about conversion routines.
  ///  <c>Note for ODBC_A</c>: This CodePage must be equal for all fields(ODBC).
  ///  Otherwise use the ODBC_W driver. It's defined as:
  ///  First place in a name as an charset alias, second add ':'+(codepage),
  ///  third add '/'+(maximum amount of bytes per character). The definition
  ///  must equal to database defined charset.
  ///  Example: CharacterSet=latin1:1252/1 or CharacterSet=utf8:65001/4
  /// </remarks>
  ConnProps_Characterset = 'CharacterSet';
  /// <type>String</type>
  /// <Alias>CharacterSet</Alias>
  /// <Associates>ConnProps_Charset_NONE_Alias</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_CodePage]={value}</syntax>
  /// <summary>Deprecated use ConnProps_Characterset instead. Specifies the
  ///  character set to interact with driver.</summary>
  /// <remarks>If you're accessing a CharacterSet "NONE" Firebird/Interbase
  ///  database you should always use the CharacterSet "NONE" as connection
  ///  characterset. In addition it's recommented to specify which characterset
  ///  the "NONE" represents by adding:
  ///  <c>Properties.Values['Charset_NONE_Alias'] := 'WIN1251'</c> f.e.
  ///  For odbc_a and ole_db(raw longvarchar only) it's implemented as:
  ///  set a custom characterset to notify zeos about conversion routines.
  ///  <c>Note for ODBC_A</c>: This CodePage must be equal for all fields(ODBC).
  ///  Otherwise use the ODBC_W driver. It's defined as:
  ///  First place in a name as an charset alias, second add ':'+(codepage),
  ///  third add '/'+(maximum amount of bytes per character). The definition
  ///  must equal to database defined charset.
  ///  Example: codepage=latin1:1252/1 or codepage=utf8:65001/4
  /// </remarks>
  ConnProps_CodePage = 'codepage';
  /// <type>Enumerator</type>
  /// <Values>DB_CP|CP_UTF8|DefaultSystemCodePage</Values>
  /// <Alias>RawStringEncoding</Alias>
  /// <Associates>ConnProps_RawStringEncoding</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_ControlsCP]={value}</syntax>
  /// <summary>dreprecaded use ConnProps_RawStringEncoding  instead;
  ///  See ConnProps_RawStringEncoding</summary>
  ConnProps_ControlsCP = 'controls_cp';
  /// <type>Enumerator</type>
  /// <Values>DB_CP|CP_UTF8|DefaultSystemCodePage</Values>
  /// <Alias>RawStringEncoding</Alias>
  /// <Associates>ConnProps_RawStringEncoding</Associates>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_ControlsCP]={value}</syntax>
  /// <summary>Defines how Zeos treads the SQL and the
  ///  <c>Get/SetString(Ansi-Compilers)</c> and <c>Get/SetRawByteString</c> for
  ///  UTF16 columns/parameters/connections on zdbc. These enum-names are mapped
  ///  the the W2A2WEncodingSource of the connection settings records.</summary>
  ConnProps_RawStringEncoding = 'RawStringEncoding';
  /// <type>Number</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_Timeout]={value}</syntax>
  /// <summary>Defines the connection/login timeout in seconds. A value smaller
  ///  than 1 means infinate.</summary>
  ConnProps_Timeout = 'timeout';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_DateReadFormat]={value}</syntax>
  /// <summary>Format to read a date from a database, like YYYY-MM-DD</summary>
  /// <remarks>Just simple formats are supported. ISO 8601 is prefered.
  ///  Weekdays, Monthnames and so on are not supported</remarks>
  ConnProps_DateReadFormat = 'DateReadFormat';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_DateWriteFormat]={value}</syntax>
  /// <summary>Format to write a date into a database, like YYYY-MM-DD</summary>
  /// <remarks>Just simple formats are supported. ISO 8601 is prefered.
  ///  Weekdays, Monthnames and so on are not supported</remarks>
  ConnProps_DateWriteFormat = 'DateWriteFormat';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_TimeReadFormat]={value}</syntax>
  /// <summary>Format to read a time value from a database, like HH:MM:SS.FFF</summary>
  ConnProps_TimeReadFormat = 'TimeReadFormat';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_TimeReadFormat]={value}</syntax>
  /// <summary>Format to write a time value into a database, like HH:MM:SS.FFF</summary>
  ConnProps_TimeWriteFormat = 'TimeWriteFormat';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_DateTimeReadFormat]={value}</syntax>
  /// <summary>Format to read a timestamp from database, like YYYY-MM-DD HH:NN:SS.F</summary>
  /// <remarks>Just simple formats are supported. ISO 8601 is prefered.
  ///  If the driver(f.e. SQLite) supports the 'T' delimiter do not hasitate to
  ///  use it! Weekdays, Monthnames and so on are not supported</remarks>
  ConnProps_DateTimeReadFormat = 'DatetimeReadFormat';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_DateTimeWriteFormat]={value}</syntax>
  /// <summary>Format to write a timestamp into database, like YYYY-MM-DD HH:NN:SS.F</summary>
  /// <remarks>Just simple formats are supported. ISO 8601 is prefered.
  ///  If the driver(f.e. SQLite) supports the 'T' delimiter do not hasitate to
  ///  use it! Weekdays, Monthnames and so on are not supported</remarks>
  ConnProps_DateTimeWriteFormat = 'DatetimeWriteFormat';
  /// <type>String</type>
  /// <summary>Sets TZAbstractDatabaseInfo.IdentifierQuotes property. The quote
  ///  chars are relevant for Postgres dollar quoting, multi drivers like ado,
  ///  odbc, OleDB same as SQLite where all known identifier quotes are allowed
  /// </summary>
  /// <default>""</default>
  /// <remarks>the quote char(s) count can contain a single char like sql standard " or
  ///  maximum two characters</remarks>
  ConnProps_IdentifierQuotes = 'identifier_quotes';

  { Parameters common for all DBC's }

  { Following parameters are for datasets and statements but could be set for
    connections to influence all linked objects.
    Values of all these parameters are being determined via DefineStatementParameter
    (value from DS/Stmt properties retrieved first; if it is empty, then value
    from Connection properties retrieved. If it is empty too, the default value
    is returned (usually empty string for options of type STR) }

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

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: INT
  // Size of buffer for results
  DSProps_InternalBufSize = 'internal_buffer_size';
{$IFEND}

{$IF DEFINED(ENABLE_ORACLE) OR DEFINED(ENABLE_INTERBASE) OR DEFINED(ENABLE_FIREBIRD) OR DEFINED(ENABLE_POSTGRES)}
  /// <type>Enumerator</type>
  /// <Values>None|OnLoad|OnAccess</Values>
  /// <usage>Connection,DataSet</usage>
  /// <syntax>Properties.Values[DSProps_LobCacheMode]={value}</syntax>
  /// <summary>How to cache lob types. OnLoad caches lobs on record fetch. OnAccess caches lobs only when accessed.</summary>
  DSProps_LobCacheMode = 'LobCacheMode';
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
  /// <summary>Defines the driver Provider as a String. This property is used
  ///  for the OleDB connection only. Example:
  ///  Properties.Values[ConnProps_Provider]=SQLNCLI11.1</summary>
  ConnProps_Provider = 'Provider';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB) OR DEFINED(ENABLE_ADO)}
  // Type: BOOLEAN
  // Use trusted connection
  ConnProps_TrustedConnection = 'Trusted_Connection';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB)}
  // Type: BOOLEAN
  // Defer the prepare?
  DSProps_DeferPrepare = 'DeferPrepare';
{$IFEND}

{$IF DEFINED(ENABLE_ODBC) OR DEFINED(ENABLE_OLEDB) OR DEFINED(ENABLE_FIREBIRD) or DEFINED(ENABLE_INTERBASE)}
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
    could be used as well. }
  (* refer MySQL manuals *)
  ConnProps_MYSQL_OPT_CONNECT_TIMEOUT             = 'MYSQL_OPT_CONNECT_TIMEOUT';
  ConnProps_MYSQL_OPT_COMPRESS                    = 'MYSQL_OPT_COMPRESS';
  ConnProps_MYSQL_OPT_NAMED_PIPE                  = 'MYSQL_OPT_NAMED_PIPE';
  ConnProps_MYSQL_INIT_COMMAND                    = 'MYSQL_INIT_COMMAND';
  ConnProps_MYSQL_READ_DEFAULT_FILE               = 'MYSQL_READ_DEFAULT_FILE';
  ConnProps_MYSQL_READ_DEFAULT_GROUP              = 'MYSQL_READ_DEFAULT_GROUP';
  ConnProps_MYSQL_SET_CHARSET_DIR                 = 'MYSQL_SET_CHARSET_DIR';
  ConnProps_MYSQL_SET_CHARSET_NAME                = 'MYSQL_SET_CHARSET_NAME';
  ConnProps_MYSQL_OPT_LOCAL_INFILE                = 'MYSQL_OPT_LOCAL_INFILE';
  ConnProps_MYSQL_OPT_PROTOCOL                    = 'MYSQL_OPT_PROTOCOL';
  ConnProps_MYSQL_SHARED_MEMORY_BASE_NAME         = 'MYSQL_SHARED_MEMORY_BASE_NAME';
  ConnProps_MYSQL_OPT_READ_TIMEOUT                = 'MYSQL_OPT_READ_TIMEOUT';
  ConnProps_MYSQL_OPT_WRITE_TIMEOUT               = 'MYSQL_OPT_WRITE_TIMEOUT';
  ConnProps_MYSQL_OPT_USE_RESULT                  = 'MYSQL_OPT_USE_RESULT';
  ConnProps_MYSQL_OPT_USE_REMOTE_CONNECTION       = 'MYSQL_OPT_USE_REMOTE_CONNECTION';
  ConnProps_MYSQL_OPT_USE_EMBEDDED_CONNECTION     = 'MYSQL_OPT_USE_EMBEDDED_CONNECTION';
  ConnProps_MYSQL_OPT_GUESS_CONNECTION            = 'MYSQL_OPT_GUESS_CONNECTION';
  ConnProps_MYSQL_SET_CLIENT_IP                   = 'MYSQL_SET_CLIENT_IP';
  ConnProps_MYSQL_SECURE_AUTH                     = 'MYSQL_SECURE_AUTH';
  ConnProps_MYSQL_REPORT_DATA_TRUNCATION          = 'MYSQL_REPORT_DATA_TRUNCATION';
  ConnProps_MYSQL_OPT_RECONNECT                   = 'MYSQL_OPT_RECONNECT';
  ConnProps_MYSQL_OPT_SSL_VERIFY_SERVER_CERT      = 'MYSQL_OPT_SSL_VERIFY_SERVER_CERT';
  ConnProps_MYSQL_PLUGIN_DIR                      = 'MYSQL_PLUGIN_DIR';
  ConnProps_MYSQL_DEFAULT_AUTH                    = 'MYSQL_DEFAULT_AUTH';
  ConnProps_MYSQL_OPT_BIND                        = 'MYSQL_OPT_BIND';
  ConnProps_MYSQL_OPT_SSL_KEY                     = 'MYSQL_OPT_SSL_KEY';
  ConnProps_MYSQL_OPT_SSL_CERT                    = 'MYSQL_OPT_SSL_CERT';
  ConnProps_MYSQL_OPT_SSL_CA                      = 'MYSQL_OPT_SSL_CA';
  ConnProps_MYSQL_OPT_SSL_CAPATH                  = 'MYSQL_OPT_SSL_CAPATH';
  ConnProps_MYSQL_OPT_SSL_CIPHER                  = 'MYSQL_OPT_SSL_CIPHER';
  ConnProps_MYSQL_OPT_SSL_CRL                     = 'MYSQL_OPT_SSL_CRL';
  ConnProps_MYSQL_OPT_SSL_CRLPATH                 = 'MYSQL_OPT_SSL_CRLPATH';
  ConnProps_MYSQL_OPT_CONNECT_ATTR_RESET          = 'MYSQL_OPT_CONNECT_ATTR_RESET';
  ConnProps_MYSQL_OPT_CONNECT_ATTR_ADD            = 'MYSQL_OPT_CONNECT_ATTR_ADD';
  ConnProps_MYSQL_OPT_CONNECT_ATTR_DELETE         = 'MYSQL_OPT_CONNECT_ATTR_DELETE';
  ConnProps_MYSQL_SERVER_PUBLIC_KEY               = 'MYSQL_SERVER_PUBLIC_KEY';
  ConnProps_MYSQL_ENABLE_CLEARTEXT_PLUGIN         = 'MYSQL_ENABLE_CLEARTEXT_PLUGIN';
  ConnProps_MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS= 'MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS';
  ConnProps_MYSQL_OPT_SSL_ENFORCE                 = 'MYSQL_OPT_SSL_ENFORCE';
  ConnProps_MYSQL_OPT_MAX_ALLOWED_PACKET          = 'MYSQL_OPT_MAX_ALLOWED_PACKET';
  ConnProps_MYSQL_OPT_NET_BUFFER_LENGTH           = 'MYSQL_OPT_NET_BUFFER_LENGTH';
  ConnProps_MYSQL_OPT_TLS_VERSION                 = 'MYSQL_OPT_TLS_VERSION';
  ConnProps_MYSQL_OPT_SSL_MODE                    = 'MYSQL_OPT_SSL_MODE';
    {MySQL 8:}
  ConnProps_MYSQL_OPT_GET_SERVER_PUBLIC_KEY       = 'MYSQL_OPT_GET_SERVER_PUBLIC_KEY';
  ConnProps_MYSQL_OPT_RETRY_COUNT                 = 'MYSQL_OPT_RETRY_COUNT';
  ConnProps_MYSQL_OPT_OPTIONAL_RESULTSET_METADATA = 'MYSQL_OPT_OPTIONAL_RESULTSET_METADATA';
  ConnProps_MYSQL_OPT_SSL_FIPS_MODE               = 'MYSQL_OPT_SSL_FIPS_MODE';
  ConnProps_MYSQL_OPT_TLS_CIPHERSUITES            = 'MYSQL_OPT_TLS_CIPHERSUITES';
  ConnProps_MYSQL_OPT_COMPRESSION_ALGORITHMS      = 'MYSQL_OPT_COMPRESSION_ALGORITHMS';
  ConnProps_MYSQL_OPT_ZSTD_COMPRESSION_LEVEL      = 'MYSQL_OPT_ZSTD_COMPRESSION_LEVEL';
  {MariaDB Connector specific }
  ConnProps_MYSQL_DATABASE_DRIVER                 = 'MYSQL_DATABASE_DRIVER';
  ConnProps_MARIADB_OPT_SSL_FP                    = 'MARIADB_OPT_SSL_FP';
  ConnProps_MARIADB_OPT_SSL_FP_LIST               = 'MARIADB_OPT_SSL_FP_LIST';
  ConnProps_MARIADB_OPT_TLS_PASSPHRASE            = 'MARIADB_OPT_TLS_PASSPHRASE';
  ConnProps_MARIADB_OPT_TLS_CIPHER_STRENGTH       = 'MARIADB_OPT_TLS_CIPHER_STRENGTH';
  ConnProps_MARIADB_OPT_TLS_VERSION               = 'MARIADB_OPT_TLS_VERSION';
  ConnProps_MARIADB_OPT_TLS_PEER_FP               = 'MARIADB_OPT_TLS_PEER_FP';
  ConnProps_MARIADB_OPT_TLS_PEER_FP_LIST          = 'MARIADB_OPT_TLS_PEER_FP_LIST';
  ConnProps_MARIADB_OPT_CONNECTION_READ_ONLY      = 'MARIADB_OPT_CONNECTION_READ_ONLY';
  ConnProps_MYSQL_OPT_CONNECT_ATTRS               = 'MYSQL_OPT_CONNECT_ATTRS';
  ConnProps_MARIADB_OPT_USERDATA                  = 'MARIADB_OPT_USERDATA';
  ConnProps_MARIADB_OPT_CONNECTION_HANDLER        = 'MARIADB_OPT_CONNECTION_HANDLER';
  ConnProps_MARIADB_OPT_PORT                      = 'MARIADB_OPT_PORT';
  ConnProps_MARIADB_OPT_UNIXSOCKET                = 'MARIADB_OPT_UNIXSOCKET';
  ConnProps_MARIADB_OPT_PASSWORD                  = 'MARIADB_OPT_PASSWORD';
  ConnProps_MARIADB_OPT_HOST                      = 'MARIADB_OPT_HOST';
  ConnProps_MARIADB_OPT_USER                      = 'MARIADB_OPT_USER';
  ConnProps_MARIADB_OPT_SCHEMA                    = 'MARIADB_OPT_SCHEMA';
  ConnProps_MARIADB_OPT_DEBUG                     = 'MARIADB_OPT_DEBUG';
  ConnProps_MARIADB_OPT_FOUND_ROWS                = 'MARIADB_OPT_FOUND_ROWS';
  ConnProps_MARIADB_OPT_MULTI_RESULTS             = 'MARIADB_OPT_MULTI_RESULTS';
  ConnProps_MARIADB_OPT_MULTI_STATEMENTS          = 'MARIADB_OPT_MULTI_STATEMENTS';
  ConnProps_MARIADB_OPT_INTERACTIVE               = 'MARIADB_OPT_INTERACTIVE';
  ConnProps_MARIADB_OPT_PROXY_HEADER              = 'MARIADB_OPT_PROXY_HEADER';
  ConnProps_MARIADB_OPT_IO_WAIT                   = 'MARIADB_OPT_IO_WAIT';

  { Parameters that are for datasets and statements but could be set for connections
    (see comment above) }

  // Type: BOOLEAN
  // Fetching rows one by one using UseResult instead of StoreResult
  // this reduces the memory-consumtion of libmysql.
  // Note mysql is tabular streamed! ->
  // So you can't use it within using metainformations or multiple active
  // resultsets!
  DSProps_UseResult = 'UseResult';
  // Type: BOOLEAN
  DSProps_MySQLUseDefaults = 'UseDefaults';
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
  // force binary results to be retrieved from server. supported since Protocol
  // V3 except libs like pgbouncer which have no pqexecparams/pqexecprepared
  DSProps_BinaryWireResultMode = 'BinaryWireResultMode';
  /// <type>Integer</type>
  /// <summary>Sets Listener interval in milliseconds.</summary>
  /// <default>250</default>
  ELProps_ListernerInterval = 'ListernerInterval';
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
  /// <type>Enumerator</type>
  /// <values>INET|WNET|XNET|LOCAL</values>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_FBProtocol]={value}</syntax>
  /// <summary>can be used to define the firebird protocol to be used
  /// for FB 3.0 this will enable the construction of url style connection
  /// strings see firebird 3.0 release notes</summary>
  ConnProps_FBProtocol = 'fb_protocol';

  /// <type>String</type>
  /// <Associates>ConnProps_Characterset</Associates>
  /// <protocols>firebird,interbase</protocols>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_Charset_NONE_Alias]={value}</syntax>
  /// <summary>Specifies the character set "NONE" codepage to interact with
  ///  driver.</summary>
  /// <remarks>If you're accessing a CharacterSet "NONE" Firebird/Interbase
  ///  database you should always use the CharacterSet "NONE" as connection
  ///  characterset.</remarks>
  ConnProps_Charset_NONE_Alias = 'Charset_NONE_Alias';

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

  ConnProps_isc_dpb_page_size               = 'isc_dpb_page_size';
  ConnProps_isc_dpb_num_buffers             = 'isc_dpb_num_buffers';
  ConnProps_isc_dpb_debug                   = 'isc_dpb_debug';
  ConnProps_isc_dpb_garbage_collect         = 'isc_dpb_garbage_collect';
  ConnProps_isc_dpb_verify                  = 'isc_dpb_verify';
  ConnProps_isc_dpb_sweep                   = 'isc_dpb_sweep';
  ConnProps_isc_dpb_enable_journal          = 'isc_dpb_enable_journal';
  ConnProps_isc_dpb_disable_journal         = 'isc_dpb_disable_journal';
  ConnProps_isc_dpb_dbkey_scope             = 'isc_dpb_dbkey_scope';
  ConnProps_isc_dpb_trace                   = 'isc_dpb_trace';
  ConnProps_isc_dpb_no_garbage_collect      = 'isc_dpb_no_garbage_collect';
  ConnProps_isc_dpb_damaged                 = 'isc_dpb_damaged';
  ConnProps_isc_dpb_license                 = 'isc_dpb_license';
  ConnProps_isc_dpb_sys_user_name           = 'isc_dpb_sys_user_name';
  ConnProps_isc_dpb_encrypt_key             = 'isc_dpb_encrypt_key';
  ConnProps_isc_dpb_activate_shadow         = 'isc_dpb_activate_shadow';
  ConnProps_isc_dpb_sweep_interval          = 'isc_dpb_sweep_interval';
  ConnProps_isc_dpb_delete_shadow           = 'isc_dpb_delete_shadow';
  ConnProps_isc_dpb_force_write             = 'isc_dpb_force_write';
  ConnProps_isc_dpb_begin_log               = 'isc_dpb_begin_log';
  ConnProps_isc_dpb_quit_log                = 'isc_dpb_quit_log';
  ConnProps_isc_dpb_no_reserve              = 'isc_dpb_no_reserve';
  ConnProps_isc_dpb_user_name               = 'isc_dpb_user_name';
  ConnProps_isc_dpb_password                = 'isc_dpb_password';
  ConnProps_isc_dpb_password_enc            = 'isc_dpb_password_enc';
  ConnProps_isc_dpb_sys_user_name_enc       = 'isc_dpb_sys_user_name_enc';
  ConnProps_isc_dpb_interp                  = 'isc_dpb_interp';
  ConnProps_isc_dpb_online_dump             = 'isc_dpb_online_dump';
  ConnProps_isc_dpb_old_file_size           = 'isc_dpb_old_file_size';
  ConnProps_isc_dpb_old_num_files           = 'isc_dpb_old_num_files';
  ConnProps_isc_dpb_old_file                = 'isc_dpb_old_file';
  ConnProps_isc_dpb_old_start_page          = 'isc_dpb_old_start_page';
  ConnProps_isc_dpb_old_start_seqno         = 'isc_dpb_old_start_seqno';
  ConnProps_isc_dpb_old_start_file          = 'isc_dpb_old_start_file';
  ConnProps_isc_dpb_drop_walfile            = 'isc_dpb_drop_walfile';
  ConnProps_isc_dpb_old_dump_id             = 'isc_dpb_old_dump_id';
  ConnProps_isc_dpb_wal_backup_dir          = 'isc_dpb_wal_backup_dir';
  ConnProps_isc_dpb_wal_chkptlen            = 'isc_dpb_wal_chkptlen';
  ConnProps_isc_dpb_wal_numbufs             = 'isc_dpb_wal_numbufs';
  ConnProps_isc_dpb_wal_bufsize             = 'isc_dpb_wal_bufsize';
  ConnProps_isc_dpb_wal_grp_cmt_wait        = 'isc_dpb_wal_grp_cmt_wait';
  ConnProps_isc_dpb_lc_messages             = 'isc_dpb_lc_messages';
  ConnProps_isc_dpb_lc_ctype                = 'isc_dpb_lc_ctype';
  ConnProps_isc_dpb_shutdown                = 'isc_dpb_shutdown';
  ConnProps_isc_dpb_online                  = 'isc_dpb_online';
  ConnProps_isc_dpb_shutdown_delay          = 'isc_dpb_shutdown_delay';
  ConnProps_isc_dpb_reserved                = 'isc_dpb_reserved';
  ConnProps_isc_dpb_overwrite               = 'isc_dpb_overwrite';
  ConnProps_isc_dpb_sec_attach              = 'isc_dpb_sec_attach';
  ConnProps_isc_dpb_disable_wal             = 'isc_dpb_disable_wal';
  ConnProps_isc_dpb_connect_timeout         = 'isc_dpb_connect_timeout';
  ConnProps_isc_dpb_dummy_packet_interval   = 'isc_dpb_dummy_packet_interval';
  ConnProps_isc_dpb_gbak_attach             = 'isc_dpb_gbak_attach';
  ConnProps_isc_dpb_sql_role_name           = 'isc_dpb_sql_role_name';
  ConnProps_isc_dpb_set_page_buffers        = 'isc_dpb_set_page_buffers';
  ConnProps_isc_dpb_working_directory       = 'isc_dpb_working_directory';
  ConnProps_isc_dpb_sql_dialect             = 'isc_dpb_sql_dialect';
  ConnProps_isc_dpb_set_db_readonly         = 'isc_dpb_set_db_readonly';
  ConnProps_isc_dpb_set_db_sql_dialect      = 'isc_dpb_set_db_sql_dialect';
  ConnProps_isc_dpb_gfix_attach             = 'isc_dpb_gfix_attach';
  ConnProps_isc_dpb_gstat_attach            = 'isc_dpb_gstat_attach';
  ConnProps_isc_dpb_set_db_charset          = 'isc_dpb_set_db_charset';
  ConnProps_isc_dpb_gsec_attach             = 'isc_dpb_gsec_attach';
  ConnProps_isc_dpb_address_path            = 'isc_dpb_address_path';
  ConnProps_isc_dpb_process_id              = 'isc_dpb_process_id';
  ConnProps_isc_dpb_no_db_triggers          = 'isc_dpb_no_db_triggers';
  ConnProps_isc_dpb_trusted_auth            = 'isc_dpb_trusted_auth';
  ConnProps_isc_dpb_process_name            = 'isc_dpb_process_name';
  ConnProps_isc_dpb_trusted_role            = 'isc_dpb_trusted_role';
  ConnProps_isc_dpb_org_filename            = 'isc_dpb_org_filename';
  ConnProps_isc_dpb_utf8_filename           = 'isc_dpb_utf8_filename';
  ConnProps_isc_dpb_ext_call_depth          = 'isc_dpb_ext_call_depth';
  ConnProps_isc_dpb_auth_block              = 'isc_dpb_auth_block';
  ConnProps_isc_dpb_client_version          = 'isc_dpb_client_version';
  ConnProps_isc_dpb_remote_protocol         = 'isc_dpb_remote_protocol';
  ConnProps_isc_dpb_host_name               = 'isc_dpb_host_name';
  ConnProps_isc_dpb_os_user                 = 'isc_dpb_os_user';
  ConnProps_isc_dpb_specific_auth_data      = 'isc_dpb_specific_auth_data';
  ConnProps_isc_dpb_auth_plugin_list        = 'isc_dpb_auth_plugin_list';
  ConnProps_isc_dpb_auth_plugin_name        = 'isc_dpb_auth_plugin_name';
  ConnProps_isc_dpb_config                  = 'isc_dpb_config';
  ConnProps_isc_dpb_nolinger                = 'isc_dpb_nolinger';
  ConnProps_isc_dpb_reset_icu               = 'isc_dpb_reset_icu';
  ConnProps_isc_dpb_map_attach              = 'isc_dpb_map_attach';
  ConnProps_isc_dpb_session_time_zone       = 'isc_dpb_session_time_zone';
  ConnProps_isc_dpb_set_db_replica          = 'isc_dpb_set_db_replica';
  ConnProps_isc_dpb_set_bind                = 'isc_dpb_set_bind';
  ConnProps_isc_dpb_decfloat_round          = 'isc_dpb_decfloat_round';
  ConnProps_isc_dpb_decfloat_traps          = 'isc_dpb_decfloat_traps';
  { Some of the isc_tpb_* parameters are added internally according to
    Connection.TransactIsolationLevel property }
  // Type: NONE
  TxnProps_isc_tpb_consistency              = 'isc_tpb_consistency';
  TxnProps_isc_tpb_concurrency              = 'isc_tpb_concurrency';
  TxnProps_isc_tpb_shared                   = 'isc_tpb_shared';
  TxnProps_isc_tpb_protected                = 'isc_tpb_protected';
  TxnProps_isc_tpb_exclusive                = 'isc_tpb_exclusive';
  TxnProps_isc_tpb_wait                     = 'isc_tpb_wait';
  TxnProps_isc_tpb_nowait                   = 'isc_tpb_nowait';
  TxnProps_isc_tpb_read                     = 'isc_tpb_read';
  TxnProps_isc_tpb_write                    = 'isc_tpb_write';
  // Type: String
  TxnProps_isc_tpb_lock_read                = 'isc_tpb_lock_read';
  TxnProps_isc_tpb_lock_write               = 'isc_tpb_lock_write';
  //not implemented
  TxnProps_isc_tpb_verb_time                = 'isc_tpb_verb_time';
  TxnProps_isc_tpb_commit_time              = 'isc_tpb_commit_time';
  //Type: None
  TxnProps_isc_tpb_ignore_limbo             = 'isc_tpb_ignore_limbo';
  TxnProps_isc_tpb_read_committed           = 'isc_tpb_read_committed';
  TxnProps_isc_tpb_autocommit               = 'isc_tpb_autocommit';
  TxnProps_isc_tpb_rec_version              = 'isc_tpb_rec_version';
  TxnProps_isc_tpb_no_rec_version           = 'isc_tpb_no_rec_version';
  TxnProps_isc_tpb_restart_requests         = 'isc_tpb_restart_requests';
  TxnProps_isc_tpb_no_auto_undo             = 'isc_tpb_no_auto_undo';
  TxnProps_isc_tpb_no_savepoint             = 'isc_tpb_no_savepoint';
  //Type: Int
  TxnProps_isc_tpb_lock_timeout             = 'isc_tpb_lock_timeout';
  //Type: None
  TxnProps_isc_tpb_read_consistency         = 'isc_tpb_read_consistency';
{$IFEND}

{$IFDEF ENABLE_FIREBIRD}
  // Type: INT
  // Session idle timeout in seconds
  ConnProps_SessionIdleTimeOut = 'SesssionIdleTimeOut'; //since FB4
  // Type: INT
  // Execution timeout in seconds
  ConnProps_StatementTimeOut = DSProps_StatementTimeOut;
  /// <type>Enum</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_FirebirdAPI]={legacy|interface}</syntax>
  /// <values>legacy|interface</syntax>
  /// <summary>
  ///  defines the Firebird API which is used used for. The default for firebird
  ///  3+ is object API. If "legacy" is set the old firebird legacy API is used.
  /// </summary>
  /// <default>interface</default>
  /// <remarks>
  ///  If the library version is lower then 3.0 the parameter is ignored
  ///  and the legacy API is always used
  /// </remarks>
  ConnProps_FirebirdAPI = 'FirebirdAPI';
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
  TxnProps_TransactionBehaviour = 'TransactionBehaviour';
  /// <type>Enum</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[DSProps_SQLiteIntAffinity]={False|True}</syntax>
  /// <values>true|False</values>
  /// <summary>
  ///  Treat "INT" fields in any kind as Int64, means ignore all subtypes like
  ///  [smallint, int32, MEDIUMINT]</summary>
  /// <default>False</default>
  DSProps_SQLiteIntAffinity = 'SQLiteIntAffinity';
  /// <type>Integer</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_SQLiteOpenFlags]=value</syntax>
  /// <values>are defined in ZPlainSqLiteDriver.pas</values>
  /// <summary>see: https://www.sqlite.org/c3ref/open.html</summary>
  ConnProps_SQLiteOpen_Flags = 'SQLiteOpen_Flags';
  /// <type>String</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_SQLiteOpen_zVfs]=value</syntax>
  /// <summary>see: https://www.sqlite.org/c3ref/open.html</summary>
  ConnProps_SQLiteOpen_zVfs = 'SQLiteOpen_zVfs';
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

  /// <type>Int</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_OCIAuthenticateMode]=mode</syntax>
  /// <summary>
  ///  Specifies the various modes of operation.
  ///  The constants are defined in ZPlainOracleDriver.pas
  /// </summary>
  /// <remarks>
  ///  Valid modes are:
  ///  OCI_DEFAULT - in this mode, the user session context returned may only ever be set with the same server context specified in svchp. For encoding, the server handle uses the setting in the environment handle.
  ///  OCI_MIGRATE - in this mode, the new user session context may be set in a service handle with a different server handle. This mode establishes the user session context. To create a migratable session, the service handle must already be set with a non-migratable user session, which becomes the "creator" session of the migratable session. That is, a migratable session must have a non-migratable parent session.
  ///  OCI_SYSDBA - in this mode, the user is authenticated for SYSDBA access.
  ///  OCI_SYSOPER - in this mode, the user is authenticated for SYSOPER access.
  ///  OCI_PRELIM_AUTH - this mode may only be used with OCI_SYSDBA or OCI_SYSOPER to authenticate for certain administration tasks.
  /// </remarks>
  ConnProps_OCIAuthenticateMode = 'OCIAuthenticateMode';

  /// <type>Boolean</type>
  /// <usage>Connection</usage>
  /// <syntax>Properties.Values[ConnProps_OCIMultiThreaded]=True/False</syntax>
  /// <summary>
  ///  If set to true, OCI_THREADED will also be used for initializing the connection environment
  /// </summary>
  /// <remarks>
  ///  If set to true, OCI_THREADED will also be used for initializing the connection environment
  /// </remarks>
  ConnProps_OCIMultiThreaded = 'OCIMultiThreaded';
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
  ConnProps_CharSet = 'CharSet';
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
  // Type: SQL_DRIVER_COMPLETE | SQL_DRIVER_PROMPT | SQL_DRIVER_COMPLETE_REQUIRED
  // Refer to ODBC manual for details
  ConnProps_DriverCompletion = 'DriverCompletion';
  // Type: BOOLEAN
  // If set, more info about columns will be retrieved
  DSProps_EnhancedColumnInfo = 'enhanced_column_info';
  /// <summary>Defines the driver as a String. This property is used
  ///  for the ODBC connection only. Example:
  ///  Properties.Values[ConnProps_DRIVER]={SQL Server Native Client 11.0}</summary>
  ConnProps_DRIVER = 'DRIVER';
  /// <summary>Defines the server as a String. This property is used
  ///  for the ODBC connection only. Example:
  ///  Properties.Values[ConnProps_Server]=(localdb)\ZeosLib</summary>
  ConnProps_Server = 'Server';
{$ENDIF}

{$IFNDEF ZEOS_DISABLE_PROXY}
  /// <summary>
  ///   Type: http | https
  ///   Defines which protocol the Webservicde Proxy driver uses for connecting to the server.
  ///   If empty, https will be used.
  ///   Behavior is undefined if an undefined value is set.
  /// </summary>
  ConnProps_ProxyProtocol = 'ProxyProtocol';
{$ENDIF}

{$IFDEF ENABLE_POOLED}
  { These parameters set the same-named properties of TConnectionPool object,
    refer to Zeos manual for details }
  ConnProps_ConnectionTimeout = 'ConnectionTimeout';
  ConnProps_MaxConnections = 'MaxConnections';
  ConnProps_Wait = 'Wait';
{$ENDIF}

implementation

end.
