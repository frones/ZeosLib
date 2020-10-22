{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Database Connectivity Interfaces              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcIntfs;

interface

{$I ZDbc.inc}
{$Z-}

uses
  {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS}
  {$IFNDEF DO_NOT_DERIVE_FROM_EDATABASEERROR}DB, {$ENDIF}
  FmtBcd, Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}SysUtils,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZClasses, ZCollections, ZCompatibility, ZTokenizer, ZSelectSchema, ZSysUtils,
  ZGenericSqlAnalyser, ZDbcLogging, ZVariant, ZPlainDriver;

const
  /// <summary>generic constant for first column/parameter index.
  ///  Note in zeos 8.1+ we use zero based index. Means the <c>GENERIC_INDEX</c>
  ///  will be removed.</summary>
  FirstDbcIndex = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  { generic constant for invalid column/parameter index }
  /// <summary>generic constant for an invalid column/parameter index.
  ///  Note in zeos 8.1+ we use based zero index. Means the <c>GENERIC_INDEX</c>
  ///  will be removed.</summary>
  InvalidDbcIndex = {$IFDEF GENERIC_INDEX}-1{$ELSE}0{$ENDIF};
const
  { Constants from JDBC DatabaseMetadata }
  TypeSearchable            = 3;
  procedureResultUnknown    = 0;
  procedureNoResult         = 1;
  ProcedureReturnsResult    = 2;

// Exceptions
type
  TZExceptionSpecificData = class
  public
    function Clone: TZExceptionSpecificData; virtual; abstract;
  end;

  {** Abstract SQL exception. }
  EZSQLThrowable = class({$IFDEF DO_NOT_DERIVE_FROM_EDATABASEERROR}Exception{$ELSE}EDATABASEERROR{$ENDIF})
  private
    FErrorCode: Integer;
    FStatusCode: String;
  protected
    FSpecificData: TZExceptionSpecificData;
  public
    constructor Create(const Msg: string);
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    constructor CreateWithCodeAndStatus(ErrorCode: Integer; const StatusCode: String; const Msg: string);
    constructor CreateClone(const E:EZSQLThrowable);
    destructor Destroy; override;

    property ErrorCode: Integer read FErrorCode;
    property StatusCode: string read FStatuscode; // The "String" Errocode // FirmOS
    property SpecificData: TZExceptionSpecificData read FSpecificData; // Engine-specific data
  end;

  EZSQLThrowableClass = class of EZSQLThrowable;

  {** Generic SQL exception. }
  EZSQLException = class(EZSQLThrowable);

  {** Generic SQL warning. }
  EZSQLWarning = class(EZSQLThrowable);

  {** Reqquested operation is not (yet) supported by Zeos }
  EZUnsupportedException = class(EZSQLException);

  /// <summary>
  ///   Generic connection lost exception.
  /// </summary>
  EZSQLConnectionLost = class(EZSQLException);

  TOnConnectionLostError = procedure(var AError: EZSQLConnectionLost) of Object;
  TOnConnect = procedure of Object;

  TZW2A2WEncodingSource = (encDB_CP, encUTF8, encDefaultSystemCodePage);
  {** hold some connection parameters }
  PZConSettings = ^TZConSettings;
  TZConSettings = record
    W2A2WEncodingSource: TZW2A2WEncodingSource; //Target CP of raw string conversion (GET_ACP/UTF8/DefaultSytemCodePage)
    ClientCodePage: PZCodePage; //The codepage informations of the current characterset
    ReadFormatSettings: TZFormatSettings;
    WriteFormatSettings: TZFormatSettings;
  end;

  {** a base class for most dbc-layer objects }
  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    procedure SetConSettingsFromInfo(Info: TStrings);
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  public
    /// <summary>Get a reference to the actual connection settings.</summary>
    /// <returns>the TZConSettings record refrence.</returns>
    function GetConSettings: PZConSettings;
  end;

  {** a base class for most dbc-layer objects }

  { TZImmediatelyReleasableObject }

  TZImmediatelyReleasableObject = Class(TZCodePagedObject)
  protected
    FWeakImmediatRelPtr: Pointer;
  public
    procedure AfterConstruction; override;
  end;

  // List of URL properties that could operate with URL-escaped strings
  TZURLStringList = Class(TStringList)
  protected
    function GetURLText: String;
    procedure SetURLText(const Value: string);
  public
    property URLText: String read GetURLText write SetURLText;
  end;

  TZURL = class
  private
    FPrefix: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FLibLocation: String;
    FProperties: TZURLStringList;
    FOnPropertiesChange: TNotifyEvent;
    procedure SetPrefix(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetLibLocation: String;
    procedure SetLibLocation(const Value: String);
    function GetURL: string;
    procedure SetURL(const Value: string);
    procedure DoOnPropertiesChange(Sender: TObject);
    procedure AddValues(Values: TStrings);
  public
    constructor Create; overload;
    constructor Create(const AURL: String); overload;
    constructor Create(const AURL: String; Info: TStrings); overload;
    constructor Create(const AURL: TZURL); overload;
    constructor Create(Const AURL, AHostName: string; const APort: Integer;
      const ADatabase, AUser, APassword: string; Info: TStrings); overload;

    destructor Destroy; override;
    property Prefix: string read FPrefix write SetPrefix;
    property Protocol: string read FProtocol write SetProtocol;
    property HostName: string read FHostName write SetHostName;
    property Port: Integer read FPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property LibLocation: string read GetLibLocation write SetLibLocation;
    property Properties: TZURLStringList read FProperties;
    property URL: string read GetURL write SetURL;

    property OnPropertiesChange: TNotifyEvent read FOnPropertiesChange write FOnPropertiesChange;
  end;

  // Data types
type
  /// <summary>Defines supported SQL types.</summary>
  TZSQLType = (stUnknown,
    //fixed size DataTypes first
    stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,  //ordinals
    stFloat, stDouble, stCurrency, stBigDecimal, //floats
    stDate, stTime, stTimestamp,
    stGUID,
    //now varying size types in equal order
    stString, {should be used for raw strings only}
    stUnicodeString{should be used for national strings only}, stBytes,
    //EH: these 3 enums below should be used for real streamed data only (FB/PG(OIDLobs)/MySQL/ODBC/OleDB/Oracle)
    //otherwise stString...stBytes are sufficent (SQLite, Postgres Varchar/Text/byteea f.e.)
    //our RowAccassor eats all long data since i added the ptr ref/deref technic years ago
    //it's not worth it handling all obselete TDataSet incompatibilities on Dbc layer
    //that's a problem of component layer only
    stAsciiStream{should be used for raw streams only}, stUnicodeStream{should be used for unicode streams only}, stBinaryStream,
    //finally the object types
    stArray, stDataSet);

  TZSQLTypeArray = array of TZSQLType;

  /// <summary>Defines a transaction isolation level.</summary>
  TZTransactIsolationLevel = (tiNone, tiReadUncommitted, tiReadCommitted,
    tiRepeatableRead, tiSerializable);

  /// <summary>Defines a set of transaction isolation level.</summary>
  TZSupportedTransactIsolationLevels = set of TZTransactIsolationLevel;

  /// <summary>Defines a resultset fetch direction.</summary>
  TZFetchDirection = (fdForward, fdReverse, fdUnknown);

  /// <summary>Defines a type of result set.</summary>
  TZResultSetType = (rtForwardOnly, rtScrollInsensitive, rtScrollSensitive);

  /// <summary>Defines a result set concurrency type.</summary>
  TZResultSetConcurrency = (rcReadOnly, rcUpdatable);

  /// <summary>Defines a nullable type for the column.</summary>
  TZColumnNullableType = (ntNoNulls, ntNullable, ntNullableUnknown);

  /// <summary>Defines a nullable type for the column.</summary>
  TZProcedureResultType = (prtUnknown, prtNoResult, prtReturnsResult);

  /// <summary>Defines a column type for the procedures.</summary>
  TZProcedureColumnType = (pctUnknown, pctIn, pctInOut, pctOut, pctReturn,
    pctResultSet);

  /// <summary>Defines a dynamic array of column types for the procedures.
  /// </summary>
  TZProcedureColumnTypeDynArray = array of TZProcedureColumnType;

  /// <summary>Defines a best row identifier.</summary>
  TZBestRowIdentifier = (brUnknown, brNotPseudo, brPseudo);

  /// <summary>Defines a scope best row identifier.</summary>
  TZScopeBestRowIdentifier = (sbrTemporary, sbrTransaction, sbrSession);

  /// <summary>Defines a version column.</summary>
  TZVersionColumn = (vcUnknown, vcNotPseudo, vcPseudo);

  /// <summary>Defines a imported key type enumerator.</summary>
  TZImportedKey = (ikCascade, ikRestrict, ikSetNull, ikNoAction, ikSetDefault,
    ikInitiallyDeferred, ikInitiallyImmediate, ikNotDeferrable);

  /// <summary>Defines a table index type enumerator.</summary>
  TZTableIndex = (tiStatistic, tiClustered, tiHashed, tiOther);

  /// <summary>Defines a post update mode.</summary>
  TZPostUpdatesMode = (poColumnsAll, poColumnsChanged);

  /// <summary>Defines a locate mode.</summary>
  TZLocateUpdatesMode = (loWhereAll, loWhereChanged, loWhereKeyOnly);

  /// <summary>Defines a MoreResults state.</summary>
  TZMoreResultsIndicator = (mriUnknown, mriHasNoMoreResults, mriHasMoreResults);

  /// <summary>Defines the server type.</summary>
  TZServerProvider = (spUnknown, spMSSQL, spMSJet, spOracle, spASE, spASA,
    spPostgreSQL, spIB_FB, spMySQL, spNexusDB, spSQLite, spDB2, spAS400,
    spInformix, spCUBRID, spFoxPro);

  /// <summary>Defines a LOB stream mode.</summary>
  TZLobStreamMode = (lsmRead, lsmWrite, lsmReadWrite);

  /// <summary>Defines a reference to the static TByteBuffer.</summary>
  PByteBuffer = ^TByteBuffer;
  /// <summary>Defines a static TByteBuffer.</summary>
  TByteBuffer = array[0..1024] of Byte;

  /// <summary>Defines a reference to the static TWordBuffer.</summary>
  PWordBuffer = ^TWordBuffer;
  /// <summary>Defines a static TWordBuffer.</summary>
  TWordBuffer = array[0..512] of Word;


// Interfaces
type
  // Forward declarations
  IZDriverManager = interface;
  IZDriver = interface;
  IZConnection = interface;
  IZDatabaseMetadata = interface;
  IZDatabaseInfo = interface;
  IZStatement = interface;
  IZPreparedStatement = interface;
  IZCallableStatement = interface;
  IZResultSet = interface;
  IZResultSetMetadata = interface;
  IZBlob = interface;
  IZClob = interface;
  IZNotification = interface;
  IZSequence = interface;
  IZDataSet = interface;

  /// <summary>Defines the Driver Manager interface.</summary>
  IZDriverManager = interface(IZInterface)
    ['{8874B9AA-068A-4C0C-AE75-9DB1EA9E3720}']
    /// <summary>Locates a required driver and opens a connection to the
    ///  specified database.</summary>
    /// <param>"Url" a database connection Url.</param>
    /// <returns>an created connection interface.</returns>
    function GetConnection(const Url: string): IZConnection;
    /// <summary>Locates a required driver and opens a connection to the
    ///  specified database.</summary>
    /// <param>"Url" a database connection Url.</param>
    /// <param>="Info" a list of extra connection parameters.</param>
    /// <returns>an created connection.</returns>
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    /// <summary>Locates a required driver and opens a connection to the
    ///  specified database.</summary>
    /// <param>="Url" a database connection url.</param>
    /// <param>="User" a user's name.</param>
    /// <param>"Password">a user's password.</param>
    /// <returns>an created connection.</returns>
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;
    /// <summary>Gets a driver which accepts the specified url.</summary>
    /// <param>"Url" a database connection url.</param>
    /// <returns>a found driver or <c>nil</c> otherwise.</returns>
    function GetDriver(const Url: string): IZDriver;
    /// <summary>Locates a required driver and returns the client library
    ///  version number.</summary>
    /// <param>"Url"a database connection Url.</param>
    /// <returns>client library version number.</returns>
    function GetClientVersion(const Url: string): Integer;
    /// <summary>Registers a driver for specific database.</summary>
    /// <param>"Driver" a driver to be registered.</param>
    procedure RegisterDriver(const Driver: IZDriver);
    /// <summary>Unregisters a driver for specific database.</summary>
    /// <param>"Driver" a driver to be unregistered.</param>
    procedure DeregisterDriver(const Driver: IZDriver);
    /// <summary>Gets a collection of registered drivers.</summary>
    /// <returns>an unmodifiable collection with registered drivers.</returns>
    function GetDrivers: IZCollection;
    /// <summary>Adds a logging listener to log SQL events.</summary>
    /// <param>"Listener" a logging interface to be added.</param>
    procedure AddLoggingListener(const Listener: IZLoggingListener);
    /// <summary>Removes a logging listener from the list.</summary>
    /// <param>"Listener" a logging interface to be removed.</param>
    procedure RemoveLoggingListener(const Listener: IZLoggingListener);
    /// <summary>Is a listener registerd?</summary>
    /// <returns><c>true</c> if a listener is available; <c>false</c> otherwise.
    /// </returns>
    function HasLoggingListener: Boolean;
    /// <summary>Logs a message about event with normal result code.</summary>
    /// <param>"Category" a category of the message.</param>
    /// <param>"Protocol" a name of the protocol.</param>
    /// <param>"Msg" a description message.</param>
    procedure LogMessage(Category: TZLoggingCategory; const Protocol: String;
      const Msg: SQLString); overload;
    /// <summary>Logs a message about event with normal result code.</summary>
    /// <param>"Category" a category of the message.</param>
    /// <param>"Sender" a IZLoggingObject.</param>
    procedure LogMessage(const Category: TZLoggingCategory; const Sender: IZLoggingObject); overload;
    /// <summary>Logs a message about event with error result code.</summary>
    /// <param>"Category" the category of the message.</param>
    /// <param>"Protocol" the name of the protocol.</param>
    /// <param>"Msg" a description message.</param>
    /// <param>"ErrorCode" an error code.</param>
    /// <param>"Error" an error message string.</param>
    procedure LogError(Category: TZLoggingCategory; const Protocol: String;
      const Msg: SQLString; ErrorCode: Integer; const Error: SQLString);
    /// <summary>Constructs a valid URL</summary>
    /// <param>"Protocol" the Driver-protocol (must be assigned).</param>
    /// <param>"HostName" the hostname (could be empty).</param>
    /// <param>"Database" the connection-database (could be empty).</param>
    /// <param>"UserName" the username (could be empty).</param>
    /// <param>"Password" the password(could be empty).</param>
    /// <param>"Port" the Server-Port (could be 0).</param>
    /// <param>"Properties" the Database-Properties (could be empty).</param>
    /// <param>"LibLocation" optional. The library name with optional absolute
    ///  path.</param>
    /// <returns>a valid URL</returns>
    function ConstructURL(const Protocol, HostName, Database,
      UserName, Password: String; const Port: Integer;
      const Properties: TStrings = nil; const LibLocation: String = ''): String;
    /// <summary>Adds garbage interfaces to keep them alive for a while.</summary>
    /// <param>"Value" a garbage interface.</param>
    procedure AddGarbage(const Value: IZInterface);
    /// <summary>Clears the garbage list. All collected interfaces get
    ///  dereferenced.</summary>
    procedure ClearGarbageCollector;
  end;

  /// <summary>Database Driver interface.</summary>
  IZDriver = interface(IZInterface)
    ['{2157710E-FBD8-417C-8541-753B585332E2}']
    /// <summary>Get an array of protocols supported by the driver.</summary>
    /// <returns>an array of protocol names.</returns>
    function GetSupportedProtocols: TStringDynArray;
    /// <summary>Get an array of character sets supported by the driver.</summary>
    /// <returns>an array of character set names.</returns>
    function GetClientCodePages(const Url: TZURL): TStringDynArray;
    /// <summary>Attempts to create a database connection to the given URL.
    ///  The driver should return "null" if it realizes it is the wrong kind
    ///  of driver to connect to the given URL. This will be common, as when
    ///  the zeos driver manager is asked to connect to a given URL it passes
    ///  the URL to each loaded driver in turn.
    ///  The driver should raise a EZSQLException if it is the right
    ///  driver to connect to the given URL, but has trouble loading the
    ///  library.</summary>
    /// <param>"url" the connection url to find the Driver.</param>
    /// <param>"Info" a Connection parameter list.</param>
    /// <returns>a <c>IZConnection</c> interface that represents a
    ///  connection to the URL</returns>
    function Connect(const Url: string; Info: TStrings): IZConnection; overload;
    /// <summary>Attempts to create a database connection to the given URL.
    ///  The driver should return "null" if it realizes it is the wrong kind
    ///  of driver to connect to the given URL. This will be common, as when
    ///  the zeos driver manager is asked to connect to a given URL it passes
    ///  the URL to each loaded driver in turn.
    ///  The driver should raise a EZSQLException if it is the right
    ///  driver to connect to the given URL, but has trouble loading the
    ///  library.</summary>
    /// <param>"url" the TZURL Object used to find the Driver, it's library and
    ///  assigns the connection properties.</param>
    /// <returns>a <c>IZConnection</c> interface that represents a
    ///  connection to the URL</returns>
    function Connect(const Url: TZURL): IZConnection; overload;
    /// <summary>Returns the version of the plain driver library that will be
    ///  used to open a connection to the given URL.</summary>
    /// <param>"url" the URL of the databaseparam</param>
    /// <returns>the version number of the plain driver library for the give
    ///  URL.</returns>
    function GetClientVersion(const Url: string): Integer;
    /// <summary>Returns true if the driver thinks that it can open a connection
    ///  to the given URL.  Typically drivers will return true if they
    ///  understand the subprotocol specified in the URL and false if they
    ///  don't.</summary>
    /// <param>"url" the URL of the database</param>
    /// <returns>true if this driver can connect to the given URL.</returns>
    function AcceptsURL(const Url: string): Boolean;
    /// <summary>Gets plain driver for selected protocol.</summary>
    /// <param>"url" the URL of the driver.</param>
    /// <returns>a selected plaindriver interface.</returns>
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver;
    /// <summary>Not yet implemented.
    ///  Gets information about the possible properties for this driver.
    ///  The getPropertyInfo method is intended to allow a generic GUI tool to
    ///  discover what properties it should prompt a human for in order to get
    ///  enough information to connect to a database.  Note that depending on
    ///  the values the human has supplied so far, additional values may become
    ///  necessary, so it may be necessary to iterate though several calls
    ///  to getPropertyInfo.</summary>
    /// <param>"url" the URL of the database to which to connect.</param>
    /// <param>"info" a proposed list of tag/value pairs that will be sent on
    ///  connect open.</param>
    /// <returns>an array of DriverPropertyInfo objects describing possible
    ///  properties.  This array may be an empty array if no properties
    ///  are required.</returns>
    function GetPropertyInfo(const Url: string; Info: TStrings): TStrings;
    /// <summary>Gets the driver's major version number. Initially this should
    ///  be 1.</summary>
    /// <returns>this driver's major version number.</returns>
    function GetMajorVersion: Integer;
    /// <summary>Gets the driver's minor version number. Initially this should
    ///  be 0.</summary>
    /// <returns>this driver's minor version number.</returns>
    function GetMinorVersion: Integer;
    /// <summary>Gets the driver's sub version (revision) number. Initially
    ///  this should be 0.</summary>
    /// <returns>this driver's minor version number.</returns>
    function GetSubVersion: Integer;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an immediately releasable interface.</summary>
  IImmediatelyReleasable = interface(IZInterface)
    ['{7AA5A5DA-5EC7-442E-85B0-CCCC71C13169}']
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
    /// <summary>Get a reference to the actual connection settings.</summary>
    /// <returns>the TZConSettings record refrence.</returns>
    function GetConSettings: PZConSettings;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a transaction interface.</summary>
  IZTransaction = interface(IImmediatelyReleasable)
    ['{501FDB3C-4D44-4BE3-8BB3-547976E6500E}']
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  released. Otherwise makes all changes made since the previous commit/
    ///  rollback permanent and releases any database locks currently held by
    ///  the Connection. This method should be used only when auto-commit mode
    ///  has been disabled. See setAutoCommit.</summary>
    procedure Commit;
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  rolled back. Otherwise drops all changes made since the previous
    ///  commit/rollback and releases any database locks currently held by this
    ///  Connection. This method should be used only when auto-commit has been
    ///  disabled. See setAutoCommit.</summary>
    procedure Rollback;
    /// <summary>Starts transaction support or saves the current transaction.
    ///  If the connection is closed, the connection will be opened.
    ///  If a transaction is underway a nested transaction or a savepoint will
    ///  be spawned. While the tranaction(s) is/are underway the AutoCommit
    ///  property is set to False. Ending up the transaction with a
    ///  commit/rollback the autocommit property will be restored if changing
    ///  the autocommit mode was triggered by a starttransaction call.</summary>
    /// <returns>The current txn-level. 1 means a expicit transaction
    ///  was started. 2 means the transaction was saved. 3 means the previous
    ///  savepoint got saved too and so on.</returns>
    function StartTransaction: Integer;
    /// <summary>Get's the owner connection that produced that object instance.
    /// </summary>
    /// <returns>the connection object interface.</returns>
    function GetConnection: IZConnection;
    /// <summary>Get the nested transaction level. -1 means no active
    ///  transaction, 0 means the txn is in AutoCommit-Mode, 1 means a expicit
    ///  transaction was started. 2 means the transaction was saved. 3 means the
    ///  previous savepoint got saved too and so on.</summary>
    /// <returns>Returns the current txn-level. </returns>
    function GetTransactionLevel: Integer;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    /// <summary>Gets the current auto-commit state. See setAutoCommit.</summary>
    /// <returns><c>True</c> if the transaction is in autocommit mode;
    ///  <c>False</c> otherwise.</returns>
    function GetAutoCommit: Boolean;
    /// <summary>Check if the current transaction is readonly. See setReadonly.
    ///  </summary>
    /// <returns><c>True</c> if the transaction is readonly; <c>False</c>
    ///  otherwise.</returns>
    function IsReadOnly: Boolean;
    /// <summary>Puts this transaction in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    /// <summary>Releases a transaction and resources immediately
    ///  instead of waiting for them to be automatically released. If the
    ///  transaction is underway a rollback will be done. Note: A
    ///  Transaction is automatically closed when the Conenction closes or it is
    ///  garbage collected. Certain fatal errors also result in a closed
    //// Transaction.</summary>
    procedure Close;
    /// <summary>Test if this <c>Transaction</c> object is closed.</summary>
    /// <returns><c>True</c> if the transaction is closed; <c>False</c>
    ///  otherwise.</returns>
    function IsClosed: Boolean;
  end;

  IZTransactionManager = interface(IImmediatelyReleasable)
    ['{BF61AD03-1072-473D-AF1F-67F90DFB4E6A}']
    /// <summary>Creates a <c>Transaction</c></summary>
    /// <param>"AutoCommit" the AutoCommit mode.</param>
    /// <param>"ReadOnly" the ReadOnly mode.</param>
    /// <param>"TransactIsolationLevel" the TransactIsolationLevel one of the
    ///  TRANSACTION_* isolation values with the exception of TRANSACTION_NONE;
    ///  some databases may not support other values see
    ///  DatabaseInfo.supportsTransactionIsolationLevel</param>
    /// <param>"Params" a list of properties used for the transaction.</param>
    /// <returns>returns the Transaction interface.</returns>
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    procedure ReleaseTransaction(const Value: IZTransaction);
    function IsTransactionValid(const Value: IZTransaction): Boolean;
    procedure ClearTransactions;
  end;

  {** Implements a variant manager with connection related convertion rules. }
  IZClientVariantManager = Interface(IZVariantManager)
    ['{73A1A2C7-7C38-4620-B7FE-2426BF839BE5}']
    function UseWComparsions: Boolean;
  End;

  /// <summary>
  ///   Database Connection interface.
  /// </summary>
  IZConnection = interface(IImmediatelyReleasable)
    ['{8EEBBD1A-56D1-4EC0-B3BD-42B60591457F}']
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload;
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload;

    procedure SetOnConnectionLostErrorHandler(Handler: TOnConnectionLostError);
    procedure SetAddLogMsgToExceptionOrWarningMsg(Value: Boolean);
    procedure SetRaiseWarnings(Value: Boolean);

    procedure RegisterStatement(const Value: IZStatement);
    procedure DeregisterStatement(const Statement: IZStatement);

    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    /// <summary>Creates a <code>CallableStatement</code> object for calling
    ///  database stored procedures. The <code>CallableStatement</code> object
    ///  provides methods for setting up its IN and OUT parameters, and methods
    ///  for executing the call to a stored procedure. Note: This method is
    ///  optimized for handling stored procedure call statements. Some drivers
    ///  may send the call statement to the database when the method
    ///  <c>prepareCall</c> is done; others may wait until the
    ///  <c>CallableStatement</c> object is executed. This has no direct effect
    ///  on users; however, it does affect which method throws certain
    ///  EZSQLExceptions. Result sets created using the returned
    ///  IZCallableStatement will have forward-only type and read-only
    ///  concurrency, by default.</summary>
    /// <param>"Name" a procedure or function name.</param>
    /// <param>"Params" a statement parameters list.</param>
    /// <returns> a new IZCallableStatement interface containing the
    ///  pre-compiled SQL statement <returns>
    function PrepareCallWithParams(const Name: string; Params: TStrings):
      IZCallableStatement;

    function CreateNotification(const Event: string): IZNotification;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;

    function NativeSQL(const SQL: string): string;
    /// <summary>Sets this connection's auto-commit mode. If a connection is in
    ///  auto-commit mode, then all its SQL statements will be executed and
    ///  committed as individual transactions. Otherwise, its SQL statements are
    ///  grouped into transactions that are terminated by a call to either the
    ///  method <c>commit</c> or the method <c>rollback</c>. By default, new
    ///  connections are in auto-commit mode. The commit occurs when the
    ///  statement completes or the next execute occurs, whichever comes first.
    ///  In the case of statements returning a ResultSet, the statement
    ///  completes when the last row of the ResultSet has been retrieved or the
    ///  ResultSet has been closed. In advanced cases, a single statement may
    ///  return multiple results as well as output parameter values. In these
    ///  cases the commit occurs when all results and output parameter values
    ///  have been retrieved. It is not recommented setting autoCommit to false
    ///  because a call to either the method <c>commit</c> or the method
    ///  <c>rollback</c> will restart the transaction. It's use full only if
    ///  repeately many opertions are done and no startTransaction is intended
    ///  to use. If you change mode to true the current Transaction and it's
    ///  nested SavePoints are committed then.</summary>
    /// <param>"Value" true enables auto-commit; false disables auto-commit.</param>
    procedure SetAutoCommit(Value: Boolean);
    /// <summary>Gets the current auto-commit state. See setAutoCommit.</summary>
    /// <returns>the current state of auto-commit mode.</returns>
    function GetAutoCommit: Boolean;
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  released. Otherwise makes all changes made since the previous commit/
    ///  rollback permanent and releases any database locks currently held by
    ///  the Connection. This method should be used only when auto-commit mode
    ///  has been disabled. See setAutoCommit.</summary>
    procedure Commit;
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  rolled back. Otherwise drops all changes made since the previous
    ///  commit/rollback and releases any database locks currently held by this
    ///  Connection. This method should be used only when auto-commit has been
    ///  disabled. See setAutoCommit.</summary>
    procedure Rollback;
    /// <summary>Starts transaction support or saves the current transaction.
    ///  If the connection is closed, the connection will be opened.
    ///  If a transaction is underway a nested transaction or a savepoint will
    ///  be spawned. While the tranaction(s) is/are underway the AutoCommit
    ///  property is set to False. Ending up the transaction with a
    ///  commit/rollback the autocommit property will be restored if changing
    ///  the autocommit mode was triggered by a starttransaction call.</summary>
    /// <returns>The current txn-level. 1 means a expicit transaction
    ///  was started. 2 means the transaction was saved. 3 means the previous
    ///  savepoint got saved too and so on.</returns>
    function StartTransaction: Integer;
    function GetConnectionTransaction: IZTransaction;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);
    procedure CommitPrepared(const transactionid: string);
    procedure RollbackPrepared(const transactionid: string);

    //Ping Server Support (firmos) 27032006

    function PingServer: Integer;
    function AbortOperation: Integer;
    function EscapeString(const Value: RawByteString): RawByteString;

    procedure Open;
    procedure Close;
    function IsClosed: Boolean;

    function GetDriver: IZDriver;
    function GetIZPlainDriver: IZPlainDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    function GetClientVersion: Integer;
    function GetHostVersion: Integer;

    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    /// <summary>Check if the current conenction is readonly. See setReadonly.
    ///  </summary>
    /// <returns><c>True</c> if the conenction is readonly; <c>False</c>
    ///  otherwise.</returns>
    function IsReadOnly: Boolean;

    procedure SetCatalog(const Value: string);
    function GetCatalog: string;

    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    function GetTransactionIsolation: TZTransactIsolationLevel;

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean);

    function GetBinaryEscapeString(const Value: TBytes): String; overload;

    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload;

    function GetEncoding: TZCharEncoding;
    function GetClientVariantManager: IZClientVariantManager;
    function GetURL: String;
    function GetServerProvider: TZServerProvider;
  end;

  /// <summary>
  ///   Database metadata interface.
  /// </summary>
  IZDatabaseMetadata = interface(IZInterface)
    ['{FE331C2D-0664-464E-A981-B4F65B85D1A8}']

    function GetURL: string;
    function GetUserName: string;

    function GetDatabaseInfo: IZDatabaseInfo;
    function GetTriggers(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet; //EgonHugeist 30.03.2011
    function GetCollationAndCharSet(const Catalog, Schema, TableName, ColumnName: String): IZResultSet; //EgonHugeist 10.01.2012
    function GetCharacterSets: IZResultSet; //EgonHugeist 19.01.2012
    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet;
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet;

    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet;
    function GetSchemas: IZResultSet;
    function GetCatalogs: IZResultSet;
    function GetTableTypes: IZResultSet;
    function GetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet;
    function GetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet;

    function GetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet;
    function GetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
    function GetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;

    function GetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet;

    function GetTypeInfo: IZResultSet;

    function GetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet;

    function GetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet;

    function GetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;

    function GetConnection: IZConnection;
    function GetIdentifierConvertor: IZIdentifierConvertor; deprecated;
    function GetIdentifierConverter: IZIdentifierConverter; //typo fixed

    procedure ClearCache; overload;
    procedure ClearCache(const Key: string); overload;

    function AddEscapeCharToWildcards(const Pattern: string): string;
    function NormalizePatternCase(const Pattern: String): string;
    function CloneCachedResultSet(const ResultSet: IZResultSet): IZResultSet;
  end;

  /// <summary>
  ///  Database information interface. Used to describe the database as a whole
  ///  (version, capabilities, policies, etc).
  /// </summary>
  IZDatabaseInfo = interface(IZInterface)
    ['{107CA354-F594-48F9-8E08-CD797F151EA0}']

    // database/driver/server info:
    function GetDatabaseProductName: string;
    function GetDatabaseProductVersion: string;
    function GetDriverName: string;
    function GetDriverVersion: string;
    function GetDriverMajorVersion: Integer;
    function GetDriverMinorVersion: Integer;
    function GetServerVersion: string;

    // capabilities (what it can/cannot do):
    function AllProceduresAreCallable: Boolean;
    function AllTablesAreSelectable: Boolean;
    function SupportsMixedCaseIdentifiers: Boolean;
    function SupportsMixedCaseQuotedIdentifiers: Boolean;
    function SupportsAlterTableWithAddColumn: Boolean;
    function SupportsAlterTableWithDropColumn: Boolean;
    function SupportsColumnAliasing: Boolean;
    function SupportsConvert: Boolean;
    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
      Boolean;
    function SupportsTableCorrelationNames: Boolean;
    function SupportsDifferentTableCorrelationNames: Boolean;
    function SupportsExpressionsInOrderBy: Boolean;
    function SupportsOrderByUnrelated: Boolean;
    function SupportsGroupBy: Boolean;
    function SupportsGroupByUnrelated: Boolean;
    function SupportsGroupByBeyondSelect: Boolean;
    function SupportsLikeEscapeClause: Boolean;
    function SupportsMultipleResultSets: Boolean;
    function SupportsMultipleTransactions: Boolean;
    function SupportsNonNullableColumns: Boolean;
    function SupportsMinimumSQLGrammar: Boolean;
    function SupportsCoreSQLGrammar: Boolean;
    function SupportsExtendedSQLGrammar: Boolean;
    function SupportsANSI92EntryLevelSQL: Boolean;
    function SupportsANSI92IntermediateSQL: Boolean;
    function SupportsANSI92FullSQL: Boolean;
    function SupportsIntegrityEnhancementFacility: Boolean;
    function SupportsOuterJoins: Boolean;
    function SupportsFullOuterJoins: Boolean;
    function SupportsLimitedOuterJoins: Boolean;
    function SupportsSchemasInDataManipulation: Boolean;
    function SupportsSchemasInProcedureCalls: Boolean;
    function SupportsSchemasInTableDefinitions: Boolean;
    function SupportsSchemasInIndexDefinitions: Boolean;
    function SupportsSchemasInPrivilegeDefinitions: Boolean;
    function SupportsCatalogsInDataManipulation: Boolean;
    function SupportsCatalogsInProcedureCalls: Boolean;
    function SupportsCatalogsInTableDefinitions: Boolean;
    function SupportsCatalogsInIndexDefinitions: Boolean;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean;
    function SupportsOverloadPrefixInStoredProcedureName: Boolean;
    function SupportsParameterBinding: Boolean;
    function SupportsPositionedDelete: Boolean;
    function SupportsPositionedUpdate: Boolean;
    function SupportsSelectForUpdate: Boolean;
    function SupportsStoredProcedures: Boolean;
    function SupportsSubqueriesInComparisons: Boolean;
    function SupportsSubqueriesInExists: Boolean;
    function SupportsSubqueriesInIns: Boolean;
    function SupportsSubqueriesInQuantifieds: Boolean;
    function SupportsCorrelatedSubqueries: Boolean;
    function SupportsUnion: Boolean;
    function SupportsUnionAll: Boolean;
    function SupportsOpenCursorsAcrossCommit: Boolean;
    function SupportsOpenCursorsAcrossRollback: Boolean;
    function SupportsOpenStatementsAcrossCommit: Boolean;
    function SupportsOpenStatementsAcrossRollback: Boolean;
    function SupportsTransactions: Boolean;
    function SupportsTransactionIsolationLevel(const Level: TZTransactIsolationLevel):
      Boolean;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
    function SupportsDataManipulationTransactionsOnly: Boolean;
    function SupportsResultSetType(const _Type: TZResultSetType): Boolean;
    function SupportsResultSetConcurrency(const _Type: TZResultSetType;
      const Concurrency: TZResultSetConcurrency): Boolean;
    function SupportsBatchUpdates: Boolean;
    function SupportsNonEscapedSearchStrings: Boolean;
    function SupportsMilliseconds: Boolean;
    function SupportsUpdateAutoIncrementFields: Boolean;
    function SupportsArrayBindings: Boolean;

    // maxima:
    function GetMaxBinaryLiteralLength: Integer;
    function GetMaxCharLiteralLength: Integer;
    function GetMaxColumnNameLength: Integer;
    function GetMaxColumnsInGroupBy: Integer;
    function GetMaxColumnsInIndex: Integer;
    function GetMaxColumnsInOrderBy: Integer;
    function GetMaxColumnsInSelect: Integer;
    function GetMaxColumnsInTable: Integer;
    function GetMaxConnections: Integer;
    function GetMaxCursorNameLength: Integer;
    function GetMaxIndexLength: Integer;
    function GetMaxSchemaNameLength: Integer;
    function GetMaxProcedureNameLength: Integer;
    function GetMaxCatalogNameLength: Integer;
    function GetMaxRowSize: Integer;
    function GetMaxStatementLength: Integer;
    function GetMaxStatements: Integer;
    function GetMaxTableNameLength: Integer;
    function GetMaxTablesInSelect: Integer;
    function GetMaxUserNameLength: Integer;

    // policies (how are various data and operations handled):
    function IsReadOnly: Boolean;
    function IsCatalogAtStart: Boolean;
    function DoesMaxRowSizeIncludeBlobs: Boolean;
    function NullsAreSortedHigh: Boolean;
    function NullsAreSortedLow: Boolean;
    function NullsAreSortedAtStart: Boolean;
    function NullsAreSortedAtEnd: Boolean;
    function NullPlusNonNullIsNull: Boolean;
    function UsesLocalFiles: Boolean;
    function UsesLocalFilePerTable: Boolean;
    function StoresUpperCaseIdentifiers: Boolean;
    function StoresLowerCaseIdentifiers: Boolean;
    function StoresMixedCaseIdentifiers: Boolean;
    function StoresUpperCaseQuotedIdentifiers: Boolean;
    function StoresLowerCaseQuotedIdentifiers: Boolean;
    function StoresMixedCaseQuotedIdentifiers: Boolean;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel;
    function DataDefinitionCausesTransactionCommit: Boolean;
    function DataDefinitionIgnoredInTransactions: Boolean;

    // interface details (terms, keywords, etc):
    function GetIdentifierQuoteString: string;
    function GetIdentifierQuoteKeywordsSorted: TStringList;
    function GetSchemaTerm: string;
    function GetProcedureTerm: string;
    function GetCatalogTerm: string;
    function GetCatalogSeparator: string;
    function GetSQLKeywords: string;
    function GetNumericFunctions: string;
    function GetStringFunctions: string;
    function GetSystemFunctions: string;
    function GetTimeDateFunctions: string;
    function GetSearchStringEscape: string;
    function GetExtraNameCharacters: string;
  end;

  /// <summary>
  ///  Generic SQL statement interface.
  /// </summary>
  IZStatement = interface(IImmediatelyReleasable)
    ['{22CEFA7E-6A6D-48EC-BB9B-EE66056E90F1}']

    /// <summary>
    ///  Executes an SQL statement that returns a single <c>ResultSet</c> object.
    /// </summary>
    /// <param name="SQL">
    ///  typically this is a static SQL <c>SELECT</c> statement
    /// </param>
    /// <returns>
    ///  a <c>ResultSet</c> object that contains the data produced by the
    ///  given query; never <c>nil</c>
    /// </returns>
    function ExecuteQuery(const SQL: UnicodeString): IZResultSet; overload;
    /// <summary>
    ///  Executes an SQL <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statement. In addition,
    ///  SQL statements that return nothing, such as SQL DDL statements,
    ///  can be executed.
    /// </summary>
    /// <param name="SQL">
    ///  an SQL <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statement or an SQL statement that returns nothing
    /// </param>
    /// <returns>
    ///  either the row count for <c>INSERT</c>, <c>UPDATE</c>
    ///  or <c>DELETE</c> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function ExecuteUpdate(const SQL: UnicodeString): Integer; overload;
    /// <summary>
    ///  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
    ///  <code>DELETE</code> statement. In addition,
    ///  SQL statements that return nothing, such as SQL DDL statements,
    ///  can be executed.
    /// </summary>
    /// <param name="SQL">
    ///  an SQL <code>INSERT</code>, <code>UPDATE</code> or
    ///  <code>DELETE</code> statement or an SQL statement that returns nothing
    /// </param>
    /// <returns>
    ///  either the row count for <code>INSERT</code>, <code>UPDATE</code>
    ///  or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function Execute(const SQL: UnicodeString): Boolean; overload;
    /// <summary>
    ///  Executes an SQL statement that returns a single <c>ResultSet</c> object.
    /// </summary>
    /// <param name="SQL">
    ///  typically this is a static SQL <c>SELECT</c> statement
    /// </param>
    /// <returns>
    ///  a <c>ResultSet</c> object that contains the data produced by the
    ///  given query; never <c>nil</c>
    /// </returns>
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload;
    /// <summary>
    ///  Executes an SQL <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statement. In addition,
    ///  SQL statements that return nothing, such as SQL DDL statements,
    ///  can be executed.
    /// </summary>
    /// <param name="SQL">
    ///  an SQL <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statement or an SQL statement that returns nothing
    /// </param>
    /// <returns>
    ///  either the row count for <c>INSERT</c>, <c>UPDATE</c>
    ///  or <c>DELETE</c> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload;
    /// <summary>
    ///  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
    ///  <code>DELETE</code> statement. In addition,
    ///  SQL statements that return nothing, such as SQL DDL statements,
    ///  can be executed.
    /// </summary>
    /// <param name="SQL">
    ///  an SQL <code>INSERT</code>, <code>UPDATE</code> or
    ///  <code>DELETE</code> statement or an SQL statement that returns nothing
    /// </param>
    /// <returns>
    ///  either the row count for <code>INSERT</code>, <code>UPDATE</code>
    ///  or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function Execute(const SQL: RawByteString): Boolean; overload;

    /// <summary>
    ///  get the current SQL string
    /// </summary>
    function GetSQL : String;

    /// <summary>
    ///  Releases this <c>Statement</c> object's database
    ///  resources immediately instead of waiting for
    ///  this to happen when it is automatically closed.
    ///  It is generally good practice to release resources as soon as
    ///  you are finished with them to avoid tying up database
    ///  resources.
    ///  <para><b>Note:</b> A <c>Statement</c> object is automatically closed when its
    ///    reference counter becomes zero. When a <c>Statement</c> object is closed, its current
    ///    <c>ResultSet</c> object, if one exists, is also closed.
    ///  </para>
    /// </summary>
    procedure Close;
    function IsClosed: Boolean;

    /// <summary>
    ///  Returns the maximum number of bytes allowed
    ///  for any column value.
    ///  This limit is the maximum number of bytes that can be
    ///  returned for any column value.
    ///  The limit applies only to <c>BINARY</c>,
    ///  <c>VARBINARY</c>, <c>LONGVARBINARY</c>, <c>CHAR</c>, <c>VARCHAR</c>, and <c>LONGVARCHAR</c>
    ///  columns.  If the limit is exceeded, the excess data is silently
    ///  discarded.
    /// </summary>
    /// <returns>
    ///  the current max column size limit; zero means unlimited
    /// </returns>
    function GetMaxFieldSize: Integer;
    /// <summary>
    ///  Sets the limit for the maximum number of bytes in a column to
    ///  the given number of bytes.  This is the maximum number of bytes
    ///  that can be returned for any column value.  This limit applies
    ///  only to <c>BINARY</c>, <c>VARBINARY</c>,
    ///  <c>LONGVARBINARY</c>, <c>CHAR</c>, <c>VARCHAR</c>, and
    ///  <c>LONGVARCHAR</c> fields.  If the limit is exceeded, the excess data
    ///  is silently discarded. For maximum portability, use values
    ///  greater than 256.
    /// </summary>
    /// <param name="Value">
    ///  the new max column size limit; zero means unlimited
    /// </param>
    procedure SetMaxFieldSize(Value: Integer);
    /// <summary>
    ///  Retrieves the maximum number of rows that a
    ///  <c>ResultSet</c> object can contain.  If the limit is exceeded, the excess
    ///  rows are silently dropped.
    /// </summary>
    /// <returns>
    ///  the current max row limit; zero means unlimited
    /// </returns>
    function GetMaxRows: Integer;
    /// <summary>
    ///  Sets the limit for the maximum number of rows that any
    ///  <c>ResultSet</c> object can contain to the given number.
    ///  If the limit is exceeded, the excess rows are silently dropped.
    /// </summary>
    /// <param name="Value">
    ///  the new max rows limit; zero means unlimited
    /// </param>
    procedure SetMaxRows(Value: Integer);
    /// <summary>
    ///  Retrieves the number of seconds the driver will
    ///  wait for a <c>Statement</c> object to execute. If the limit is exceeded, a
    ///  <c>SQLException</c> is thrown.
    /// </summary>
    /// <returns>
    ///  the current query timeout limit in seconds; zero means unlimited
    /// </returns>
    function GetQueryTimeout: Integer;
    /// <summary>
    ///  Sets the number of seconds the driver will
    ///  wait for a <c>Statement</c> object to execute to the given number of seconds.
    ///  If the limit is exceeded, an <c>SQLException</c> is thrown.
    /// </summary>
    /// <param name="Value">
    ///  the new query timeout limit in seconds; zero means unlimited
    /// </param>
    procedure SetQueryTimeout(Value: Integer);
    /// <summary>
    ///  Cancels this <c>Statement</c> object if both the DBMS and
    ///  driver support aborting an SQL statement.
    ///  This method can be used by one thread to cancel a statement that
    ///  is being executed by another thread.
    /// </summary>
    procedure Cancel;
    /// <summary>
    ///  Defines the SQL cursor name that will be used by
    ///  subsequent <c>Statement</c> object <c>execute</c> methods.
    ///  This name can then be
    ///  used in SQL positioned update / delete statements to identify the
    ///  current row in the <c>ResultSet</c> object generated by this statement.  If
    ///  the database doesn't support positioned update/delete, this
    ///  method is a noop.  To insure that a cursor has the proper isolation
    ///  level to support updates, the cursor's <c>SELECT</c> statement should be
    ///  of the form 'select for update ...'. If the 'for update' phrase is
    ///  omitted, positioned updates may fail.
    ///  <note>
    ///   <para><B>Note:</B> By definition, positioned update/delete
    ///   execution must be done by a different <c>Statement</c> object than the one
    ///   which generated the <c>ResultSet</c> object being used for positioning. Also,
    ///   cursor names must be unique within a connection.</para>
    ///  </note>
    /// </summary>
    /// <param name="Value">
    ///    the new cursor name, which must be unique within a connection
    /// </param>
    procedure SetCursorName(const Value: String);

    /// <summary>
    ///  Returns the current result as a <c>ResultSet</c> object.
    ///  This method should be called only once per result.
    /// </summary>
    /// <returns>
    ///  the current result as a <c>ResultSet</c> object;
    ///  <c>nil</c> if the result is an update count or there are no more results
    /// </returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetResultSet: IZResultSet;
    /// <summary>
    ///  Returns the current result as an update count;
    ///  if the result is a <c>ResultSet</c> object or there are no more results, -1
    ///  is returned. This method should be called only once per result.
    /// </summary>
    /// <returns>
    ///  the current result as an update count; -1 if the current result is a
    ///  <c>ResultSet</c> object or there are no more results
    /// </returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetUpdateCount: Integer;
    /// <summary>
    ///  Moves to a <c>Statement</c> object's next result.  It returns
    ///  <c>true</c> if this result is a <c>ResultSet</c> object.
    ///  This method also implicitly closes any current <c>ResultSet</c>
    ///  object obtained with the method <c>getResultSet</c>.
    ///
    ///  <para>There are no more results when the following is true:
    ///  <code>(not getMoreResults and (getUpdateCount = -1)</code>
    ///  </para>
    /// </summary>
    /// <returns>
    ///  <c>true</c> if the next result is a <c>ResultSet</c> object;
    ///  <c>false</c> if it is an update count or there are no more results
    /// </returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetMoreResults: Boolean;

    /// <summary>
    ///  Gives the driver a hint as to the direction in which
    ///  the rows in a result set
    ///  will be processed. The hint applies only to result sets created
    ///  using this <c>Statement</c> object.  The default value is
    ///  <c>fdForward</c>.
    ///  <para>Note that this method sets the default fetch direction for
    ///  result sets generated by this <c>Statement</c> object.
    ///  Each result set has its own methods for getting and setting
    ///  its own fetch direction.</para>
    /// </summary>
    /// <param name="Value">
    ///  the initial direction for processing rows
    /// </param>
    procedure SetFetchDirection(Value: TZFetchDirection);
    /// <summary>
    ///  Retrieves the direction for fetching rows from
    ///  database tables that is the default for result sets
    ///  generated from this <c>Statement</c> object.
    ///  If this <c>Statement</c> object has not set
    ///  a fetch direction by calling the method <c>setFetchDirection</c>,
    ///  the return value is implementation-specific.
    /// </summary>
    /// <returns>
    ///  the default fetch direction for result sets generated
    ///  from this <c>Statement</c> object
    /// </returns>
    function GetFetchDirection: TZFetchDirection;

    /// <summary>
    ///  Gives the DBC driver a hint as to the number of rows that should
    ///  be fetched from the database when more rows are needed.  The number
    ///  of rows specified affects only result sets created using this
    ///  statement. If the value specified is zero, then the hint is ignored.
    ///  The default value is zero.
    ///  <para><b>Note:</b> Most drivers will ignore this.</para>
    /// </summary>
    /// <param name="Value">
    ///  the number of rows to fetch
    /// </param>
    procedure SetFetchSize(Value: Integer);
    /// <summary>
    ///  Retrieves the number of result set rows that is the default
    ///  fetch size for result sets
    ///  generated from this <c>Statement</c> object.
    ///  If this <c>Statement</c> object has not set
    ///  a fetch size by calling the method <c>setFetchSize</c>,
    ///  the return value is implementation-specific.
    ///  <para><b>Note:</b> Most drivers will ignore this.</para>
    /// </summary>
    /// <returns>
    ///  the default fetch size for result sets generated
    ///  from this <c>Statement</c> object
    /// </returns>
    function GetFetchSize: Integer;
    /// <summary>
    ///  Sets a result set concurrency for <c>ResultSet</c> objects
    ///  generated by this <c>Statement</c> object.
    /// </summary>
    /// <param name="Value">
    ///  either <c>rcReadOnly</code> or
    ///  <code>rcUpdateable</code>
    /// </param>
    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency);
    /// <summary>
    ///  Retrieves the result set concurrency for <c>ResultSet</c> objects
    ///  generated by this <c>Statement</c> object.
    /// </summary>
    /// <returns>
    ///  either <c>rcReadOnly</c> or
    ///  <c>rcUpdateable</c>
    /// </returns>
    function GetResultSetConcurrency: TZResultSetConcurrency;
    /// <summary>
    ///  Sets a result set type for <c>ResultSet</c> objects
    ///  generated by this <c>Statement</c> object.
    /// </summary>
    /// <param name="Value">
    ///  one of <c>rtForwardOnly</c>,
    ///  <c>rtScrollInsensitive</c>, or
    ///  <c>rtScrollSensitive</c>
    /// </param>
    procedure SetResultSetType(Value: TZResultSetType);
    /// <summary>
    ///  Retrieves the result set type for <c>ResultSet</c> objects
    ///  generated by this <c>Statement</c> object.
    /// </summary>
    /// <returns>
    ///  one of <c>rcForwardOnly</c>,
    ///  <c>rcScrollInsensitive</c>, or
    ///  <c>rcScrollSensitive</c>
    /// </returns>
    function GetResultSetType: TZResultSetType;

    /// <summary>
    ///  Sets a new value for post updates.
    /// </summary>
    /// <param name="Value">
    ///  a new value for post updates.
    /// </param>
    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    /// <summary>
    ///  Gets the current value for post updates.
    /// </summary>
    /// <returns>
    ///  the current value for post updates.
    /// </returns>
    function GetPostUpdates: TZPostUpdatesMode;
    /// <summary>
    ///  Sets a new value for locate updates.
    /// </summary>
    /// <param name="Value">
    ///  Value a new value for locate updates.
    /// </param>
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    /// <summary>
    ///  Gets the current value for locate updates.
    /// </summary>
    /// <returns>
    ///  the current value for locate updates.
    /// </returns>
    function GetLocateUpdates: TZLocateUpdatesMode;

    /// <summary>Not yet implemented.
    ///  Adds an SQL command to the current batch of commmands for this
    ///  <c>Statement</c> object. This method is optional.
    /// </summary>
    /// <param name="SQL">
    ///  typically this is a static SQL <c>INSERT</c> or
    ///  <c>UPDATE</c> statement
    /// </param>
    procedure AddBatch(const SQL: string);
    /// <summary>Not yet implemented.
    ///  Adds an SQL command to the current batch of commmands for this
    ///  <c>Statement</c> object. This method is optional.
    /// </summary>
    /// <param name="SQL">
    ///  typically this is a static SQL <c>INSERT</c> or
    ///  <c>UPDATE</c> statement
    /// </param>
    procedure AddBatchRequest(const SQL: string);

    /// <summary> Not yet implemented.
    ///  Makes the set of commands in the current batch empty.
    ///  This method is optional.
    /// </summary>
    procedure ClearBatch;
    /// <summary> Not yet implemented.
    ///  Submits a batch of commands to the database for execution and if all
    ///  commands execute successfully, returns an array of update counts. The
    ///  <c>int</c> elements of the array that is returned are ordered to
    ///  correspond to the commands in the batch, which are ordered according to
    ///  the order in which they were added to the batch. The elements in the
    ///  array returned by the method <c>executeBatch</c> may be one of the
    ///  following: A number greater than or equal to zero -- indicates that the
    ///  command was processed successfully and is an update count giving the
    ///  number of rows in the database that were affected by the command's
    ///  execution. A value of <c>-2</c> -- indicates that the command was
    ///  processed successfully but that the number of rows affected is unknown
    ///  If one of the commands in a batch update fails to execute properly,
    ///  this method throws a <c>BatchUpdateException</c>, and a driver may or
    ///  may not continue to process the remaining commands in the batch.
    ///  However, the driver's behavior must be consistent with a particular
    ///  DBMS, either always continuing to process commands or never continuing
    ///  to process commands. If the driver continues processing after a
    ///  failure, the array returned by the method
    ///  <c>BatchUpdateException.getUpdateCounts</c> will contain as many
    ///  elements as there are commands in the batch, and at least one of the
    ///  elements will be the following: A value of <c>-3</c> -- indicates that
    ///  the command failed to execute successfully and occurs only if a driver
    ///  continues to process commands after a command fails. A driver is not
    ///  required to implement this method. The possible implementations and
    ///  return values have been modified to accommodate the option of
    ///  continuing to proccess commands in a batch update after a
    ///  <c>BatchUpdateException</c> obejct has been thrown.</summary>
    /// <returns>An array of update counts containing one element for each
    ///  command in the batch. The elements of the array are ordered according
    ///  to the order in which commands were added to the batch.</returns>
    function ExecuteBatch: TIntegerDynArray;
    /// <summary>
    ///  Returns the <c>Connection</c> object
    ///  that produced this <c>Statement</c> object.
    /// </summary>
    /// <returns>
    /// <see cref="IZConnection"></see>
    ///  the connection that produced this statement
    /// </returns>
    function GetConnection: IZConnection;

    /// <summary>
    ///  Gets statement parameters.
    /// </summary>
    /// <returns>
    ///  a list with statement parameters.
    /// </returns>
    function GetParameters: TStrings;
    /// <summary>
    ///  Returns the ChunkSize for reading/writing large lobs
    /// </summary>
    /// <returns>the chunksize in bytes.</returns>
    function GetChunkSize: Integer;

    /// <summary>
    ///  Retrieves the first warning reported by calls on this <c>Statement</c> object.
    ///  Subsequent <c>Statement</c> object warnings will be chained to this
    ///  <c>SQLWarning</c> object.
    ///  <para>The warning chain is automatically cleared each time
    ///  a statement is (re)executed.</para>
    ///  <para><b>Note:</b> If you are processing a <c>ResultSet</c> object, any
    ///  warnings associated with reads on that <c>ResultSet</c> object
    ///  will be chained on it.</para>
    /// </summary>
    /// <returns>
    ///  the first <c>SQLWarning</c> object or <c>nil</c>
    /// </returns>
    function GetWarnings: EZSQLWarning;

    /// <summary>Clears all the warnings reported on this <c>Statement</c>
    ///  object. After a call to this method,the method <c>getWarnings</c> will
    ///  return <c>nil</c> until a new warning is reported for this
    ///  <c>Statement</c> object.</summary>
    procedure ClearWarnings;
    /// <summary>The sender resultsets get's closed. Notify owner
    ///  <c>Statement</c> about it. The statment, if rexecuted, will
    ///  create a new resultset interface.</summary>
    /// <param>"Sender" the closing resultset.</param>
    procedure FreeOpenResultSetReference(const Sender: IZResultSet);
    /// <summary>Result a unique internal Id per class.</summary>
    /// <returns>the the new class id.</returns>
    function GetStatementId: NativeUInt;
  end;

  /// <summary>Prepared SQL statement interface.</summary>
  IZPreparedStatement = interface(IZStatement)
    ['{990B8477-AF11-4090-8821-5B7AFEA9DD70}']
    /// <summary>Executes the SQL query in this <c>PreparedStatement</c> object
    ///  and returns the result set generated by the query.
    /// <returns>a <c>IZResultSet</c> interface that contains the data produced
    ///  by the query; never <c>nil</c></returns>
    function ExecuteQueryPrepared: IZResultSet;
    /// <summary>Executes the SQL INSERT, UPDATE or DELETE statement in this
    ///  <c>PreparedStatement</c> object. In addition, SQL statements that
    ///  return nothing, such as SQL DDL statements, can be executed.
    /// <returns>either the row count for INSERT, UPDATE or DELETE statements;
    ///  or -1 for SQL statements that return nothing</returns>
    function ExecuteUpdatePrepared: Integer;
    /// <summary>Executes any kind of SQL statement. Some prepared statements
    ///  return multiple results; the <c>ExecutePrepared</c> method handles these
    ///  complex statements as well as the simpler form of statements handled
    ///  by the methods <c>ExecuteQuery</c> and <c>ExecuteUpdate</c>.
    ///  see IStatement.execute
    /// <returns>True if a ResultSet is available otherwise false.</returns>
    function ExecutePrepared: Boolean;

    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD);
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: UnicodeString); //AVZ
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); overload;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); overload;
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); overload;
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); overload;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); overload;
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); overload;
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob);
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant);
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      {%H-}Scale: LengthInt = 0);

    //======================================================================
    // Methods for accessing out parameters by index
    //======================================================================
    function IsNull(Index: Integer): Boolean;
    function GetBoolean(ParameterIndex: Integer): Boolean;
    function GetByte(ParameterIndex: Integer): Byte;
    function GetShort(ParameterIndex: Integer): ShortInt;
    function GetWord(ParameterIndex: Integer): Word;
    function GetSmall(ParameterIndex: Integer): SmallInt;
    function GetUInt(ParameterIndex: Integer): Cardinal;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetULong(ParameterIndex: Integer): UInt64;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double;
    function GetCurrency(ParameterIndex: Integer): Currency;
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
    procedure GetGUID(Index: Integer; var Result: TGUID);
    function GetBytes(ParameterIndex: Integer): TBytes; overload;
    function GetDate(ParameterIndex: Integer): TDateTime; overload;
    procedure GetDate(ParameterIndex: Integer; Var Result: TZDate); overload;
    function GetTime(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTime(ParameterIndex: Integer; Var Result: TZTime); overload;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTimeStamp(Index: Integer; var Result: TZTimeStamp); overload;
    function GetValue(ParameterIndex: Integer): TZVariant;

    function GetString(ParameterIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): UnicodeString;

    function GetBLob(ParameterIndex: Integer): IZBlob;
    //function GetCLob(ParameterIndex: Integer): IZClob;

    procedure ClearParameters;
  end;

  /// <summary>
  ///   Callable SQL statement interface.
  /// </summary>
  IZCallableStatement = interface(IZPreparedStatement)
    ['{E6FA6C18-C764-4C05-8FCB-0582BDD1EF40}']
    { Multiple ResultSet support API }
    function GetFirstResultSet: IZResultSet;
    function GetPreviousResultSet: IZResultSet;
    function GetNextResultSet: IZResultSet;
    function GetLastResultSet: IZResultSet;
    function BOR: Boolean;
    function EOR: Boolean;
    function GetResultSetByIndex(Index: Integer): IZResultSet;
    function GetResultSetCount: Integer;

    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); //deprecated;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer); //deprecated;
  end;

  /// <summary>Defines a compare method</summary>
  TCompareFunc = function(const Null1, Null2: Boolean; const V1, V2): Integer;
  /// <summary>Defines an array of compare methods.</summary>
  TCompareFuncs = Array of TCompareFunc;

  /// <summary>Defines Column-Comparison kinds</summary>
  TComparisonKind = (ckAscending{greater than}, ckDescending{less than}, ckEquals);
  /// <summary>Defines an array of compaison kinds.</summary>
  TComparisonKindArray = Array of TComparisonKind;

  {$IFDEF USE_SYNCOMMONS}
  TZJSONComposeOption = (jcoEndJSONObject, jcoDATETIME_MAGIC, jcoMongoISODate,
    jcoMilliseconds, jcsSkipNulls);
  TZJSONComposeOptions = set of TZJSONComposeOption;
  {$ENDIF USE_SYNCOMMONS}

  /// <summary>
  ///   Rows returned by SQL query.
  /// </summary>
  IZResultSet = interface(IImmediatelyReleasable)
    ['{8F4C4D10-2425-409E-96A9-7142007CC1B2}']
    /// <summary>Releases this <c>ResultSet</c> object's database and resources
    ///  immediately instead of waiting for this to happen when it is
    ///  automatically closed. Note: A <c>ResultSet</c> object is automatically
    ///  closed by the <c>Statement</c> object that generated it when that
    ///  <c>Statement</c> object is closed, or is used to retrieve the next
    ///  result from a sequence of multiple results. A <c>ResultSet</c> object
    ///  is also automatically closed when it is garbage collected.</summary>
    procedure Close;
    /// <summary>Resets the Cursor position to beforeFirst, releases server and
    ///  client resources but keeps buffers or Column-Informations alive.</summary>
    procedure ResetCursor;
    /// <summary>Reports whether the last column read had a value of SQL
    ///  <c>NULL</c>. Note that you must first call one of the <c>getXXX</c>
    ///  methods on a column to try to read its value and then call the method
    ///  <c>wasNull</c> to see if the value read was SQL <c>NULL</c>.</summary>
    /// <returns><c>true</c> if the last column value read was SQL <c>NULL</c>
    ///  and <c>false</c> otherwise.</returns>
    function WasNull: Boolean;
    /// <summary>Indicates whether the this <c>ResultSet</c> is closed.</summary>
    /// <returns><c>true</c> if closed; <c>false</c> otherwise.</returns>
    function IsClosed: Boolean;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function GetString(ColumnIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetUnicodeString(ColumnIndex: Integer): UnicodeString;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetByte(ColumnIndex: Integer): Byte;
    function GetShort(ColumnIndex: Integer): ShortInt;
    function GetWord(ColumnIndex: Integer): Word;
    function GetSmall(ColumnIndex: Integer): SmallInt;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer): TBytes; overload;
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    function GetDate(ColumnIndex: Integer): TDateTime; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    function GetTime(ColumnIndex: Integer): TDateTime; overload;
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); overload;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;
    function GetAsciiStream(ColumnIndex: Integer): TStream;
    function GetAnsiStream(ColumnIndex: Integer): TStream;
    function GetUTF8Stream(ColumnIndex: Integer): TStream;
    function GetUnicodeStream(ColumnIndex: Integer): TStream;
    function GetBinaryStream(ColumnIndex: Integer): TStream;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    function GetDataSet(ColumnIndex: Integer): IZDataSet;
    function GetValue(ColumnIndex: Integer): TZVariant;
    function GetDefaultExpression(ColumnIndex: Integer): string;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    function IsNullByName(const ColumnName: string): Boolean;
    function GetPAnsiCharByName(const ColumnName: string; out Len: NativeUInt): PAnsiChar;
    function GetStringByName(const ColumnName: string): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiStringByName(const ColumnName: string): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8StringByName(const ColumnName: string): UTF8String;
    {$ENDIF}
    function GetRawByteStringByName(const ColumnName: string): RawByteString;
    function GetUnicodeStringByName(const ColumnName: string): UnicodeString;
    function GetPWideCharByName(const ColumnName: string; out Len: NativeUInt): PWideChar;
    function GetBooleanByName(const ColumnName: string): Boolean;
    function GetByteByName(const ColumnName: string): Byte;
    function GetShortByName(const ColumnName: string): ShortInt;
    function GetWordByName(const ColumnName: string): Word;
    function GetSmallByName(const ColumnName: string): SmallInt;
    function GetUIntByName(const ColumnName: string): Cardinal;
    function GetIntByName(const ColumnName: string): Integer;
    function GetULongByName(const ColumnName: string): UInt64;
    function GetLongByName(const ColumnName: string): Int64;
    function GetFloatByName(const ColumnName: string): Single;
    function GetDoubleByName(const ColumnName: string): Double;
    function GetCurrencyByName(const ColumnName: string): Currency;
    procedure GetBigDecimalByName(const ColumnName: string; var Result: TBCD);
    procedure GetGUIDByName(const ColumnName: string; var Result: TGUID);
    function GetBytesByName(const ColumnName: string): TBytes;
    function GetDateByName(const ColumnName: string): TDateTime; overload;
    procedure GetDateByName(const ColumnName: string; var Result: TZDate); overload;
    function GetTimeByName(const ColumnName: string): TDateTime; overload;
    procedure GetTimeByName(const ColumnName: string; Var Result: TZTime); overload;
    function GetTimestampByName(const ColumnName: string): TDateTime; overload;
    procedure GetTimeStampByName(const ColumnName: string; var Result: TZTimeStamp); overload;
    function GetAsciiStreamByName(const ColumnName: string): TStream;
    function GetAnsiStreamByName(const ColumnName: string): TStream;
    function GetUTF8StreamByName(const ColumnName: string): TStream;
    function GetUnicodeStreamByName(const ColumnName: string): TStream;
    function GetBinaryStreamByName(const ColumnName: string): TStream;
    function GetBlobByName(const ColumnName: string; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    function GetDataSetByName(const ColumnName: String): IZDataSet;
    function GetValueByName(const ColumnName: string): TZVariant;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    function GetCursorName: String;
    function GetMetadata: IZResultSetMetadata;
    function FindColumn(const ColumnName: string): Integer;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean;
    /// <summary>Indicates whether the cursor is before the first row in this
    ///  <c>ResultSet</c> object.</summary>
    /// <returns><c>true</c> if the cursor is before the first row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsBeforeFirst: Boolean;
    /// <summary>Indicates whether the cursor is after the last row in this
    ///  <c>ResultSet</c> object.</summary>
    /// <returns><c>true</c> if the cursor is after the last row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsAfterLast: Boolean;
    /// <summary>Indicates whether the cursor is on the first row of this
    ///  <c>ResultSet</c> object.<summary>
    /// <returns><c>true</c> if the cursor is on the first row;
    ///  <c>false</c> otherwise.</returns>
    function IsFirst: Boolean;
    /// <summary>Indicates whether the cursor is on the last row of this
    ///  <c>ResultSet</c> object. Note: Calling the method <c>isLast</c> may be
    ///  expensive because the driver might need to fetch ahead one row in order
    ///  to determine whether the current row is the last row in the result set.
    /// </summary>
    /// <returns><c>true</c> if the cursor is on the last row;
    ///  <c>false</c> otherwise.</returns>
    function IsLast: Boolean;
    /// <summary>Moves the cursor to the top of this <c>ResultSet</c> interface,
    ///  just before the first row.</summary>
    procedure BeforeFirst;
    /// <summary>Moves the cursor to the end of this <c>ResultSet</c> interface,
    ///  just after the last row. This method has no effect if the result set
    ///  contains no rows.</summary>
    procedure AfterLast;
    /// <summary>Moves the cursor to the first row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the resultset</returns>
    function First: Boolean;
    /// <summary>Moves the cursor to the last row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the result set </returns>
    function Last: Boolean;
    /// <summary>Retrieves the current row number. The first row is number 1,
    ///  the second number 2, and so on.
    /// <returns>the current row number; <c>0</c> if there is no current row
    /// <returns>
    function GetRow: NativeInt;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean;
    /// <summary>Moves the cursor a relative number of rows, either positive
    ///  or negative. Attempting to move beyond the first/last row in the
    ///  result set positions the cursor before/after the the first/last row.
    ///  Calling <c>relative(0)</c> is valid, but does not change the cursor
    ///  position. Note: Calling the method <c>relative(1)</c> is different
    ///  from calling the method <c>next()</c> because is makes sense to call
    ///  <c>next()</c> when there is no current row, for example, when the
    ///  cursor is positioned before the first row or after the last row of the
    ///  result set. </summary>
    /// <param>"Rows" the relative number of rows to move the cursor.</param>
    /// <returns><c>true</c> if the cursor is on a row;<c>false</c> otherwise
    /// </returns>
    function MoveRelative(Rows: Integer): Boolean;
    /// <summary>Moves the cursor to the previous row in this <c>ResultSet</c>
    ///  interface. Note: Calling the method <c>previous()</c> is not the same
    ///  as calling the method <c>relative(-1)</c> because it makes sense to
    ///  call<c>previous()</c> when there is no current row.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if it
    ///  is off the result set</returns>
    function Previous: Boolean;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    procedure SetFetchDirection(Value: TZFetchDirection);
    function GetFetchDirection: TZFetchDirection;

    procedure SetFetchSize(Value: Integer);
    function GetFetchSize: Integer;

    function GetType: TZResultSetType;
    function GetConcurrency: TZResultSetConcurrency;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean;
    function RowInserted: Boolean;
    function RowDeleted: Boolean;

    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; Value: Byte);
    procedure UpdateShort(ColumnIndex: Integer; Value: ShortInt);
    procedure UpdateWord(ColumnIndex: Integer; Value: Word);
    procedure UpdateSmall(ColumnIndex: Integer; Value: SmallInt);
    procedure UpdateUInt(ColumnIndex: Integer; Value: Cardinal);
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer);
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64);
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double);
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: TBCD);
    procedure UpdateGUID(ColumnIndex: Integer; const Value: TGUID);
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; var Len: NativeUInt);
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; var Len: NativeUInt);
    procedure UpdateString(ColumnIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: UnicodeString);
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes); overload;
    procedure UpdateBytes(ColumnIndex: Integer; Value: PByte; var Len: NativeUInt); overload;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TZDate); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TZTime); overload;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant);
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob);

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string);
    procedure UpdateBooleanByName(const ColumnName: string; Value: Boolean);
    procedure UpdateByteByName(const ColumnName: string; Value: Byte);
    procedure UpdateShortByName(const ColumnName: string; Value: ShortInt);
    procedure UpdateWordByName(const ColumnName: string; Value: Word);
    procedure UpdateSmallByName(const ColumnName: string; Value: SmallInt);
    procedure UpdateUIntByName(const ColumnName: string; Value: Cardinal);
    procedure UpdateIntByName(const ColumnName: string; Value: Integer);
    procedure UpdateULongByName(const ColumnName: string; const Value: UInt64);
    procedure UpdateLongByName(const ColumnName: string; const Value: Int64);
    procedure UpdateFloatByName(const ColumnName: string; Value: Single);
    procedure UpdateCurrencyByName(const ColumnName: string; const Value: Currency);
    procedure UpdateDoubleByName(const ColumnName: string; const Value: Double);
    procedure UpdateBigDecimalByName(const ColumnName: string; const Value: TBCD);
    procedure UpdateGUIDByName(const ColumnName: string; const Value: TGUID);
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar; var Len: NativeUInt);
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar; var Len: NativeUInt);
    procedure UpdateStringByName(const ColumnName: string; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiStringByName(const ColumnName: string; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8StringByName(const ColumnName: string; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteStringByName(const ColumnName: string; const Value: RawByteString);
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: UnicodeString);
    procedure UpdateBytesByName(const ColumnName: string; const Value: TBytes);
    procedure UpdateDateByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateDateByName(const ColumnName: string; const Value: TZDate); overload;
    procedure UpdateTimeByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateTimeByName(const ColumnName: string; const Value: TZTime); overload;
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateUnicodeStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateBinaryStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant);

    procedure InsertRow;
    procedure UpdateRow;
    procedure DeleteRow;
    procedure RefreshRow;
    procedure CancelRowUpdates;
    procedure MoveToInsertRow;
    procedure MoveToCurrentRow;

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs;

    function GetStatement: IZStatement;

    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  end;

  /// <summary>implements DataSet interface. Just prepare and will be omitted
  ///  in future releases</summary>
  IZDataSet = interface(IZInterface)
    ['{DBC24011-EF26-4FD8-AC8B-C3E01619494A}']
    //function GetDataSet: TDataSet;
    function IsEmpty: Boolean;
  end;

  /// <summary>ResultSet metadata interface.</summary>
  IZResultSetMetadata = interface(IZInterface)
    ['{47CA2144-2EA7-42C4-8444-F5154369B2D7}']
    /// <summary>Maps the given <c>Metadata</c> column name to its
    ///  <c>Metadata</c> column index. First searches with case-sensivity then,
    ///  if nothing matches, a case.insensitive search is performed.
    /// <param>"ColumnName" the name of the column</param>
    /// <returns>the column index of the given column name or an
    ///  InvalidDbcIndex if nothing was found</returns>
    function FindColumn(const ColumnName: string): Integer;
    /// <summary>get the number of columns in this <c>ResultSet</c> interface.</summary>
    /// <returns>the number of columns</returns>
    function GetColumnCount: Integer;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean;
    function IsCaseSensitive(ColumnIndex: Integer): Boolean;
    function IsSearchable(ColumnIndex: Integer): Boolean;
    procedure SetSearchable(ColumnIndex: Integer; Value: Boolean);
    function IsCurrency(ColumnIndex: Integer): Boolean;
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType;

    function IsSigned(ColumnIndex: Integer): Boolean;
    function GetColumnLabel(ColumnIndex: Integer): string;
    function GetOrgColumnLabel(ColumnIndex: Integer): string;
    function GetColumnName(ColumnIndex: Integer): string;
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    function GetSchemaName(ColumnIndex: Integer): string;
    function GetPrecision(ColumnIndex: Integer): Integer;
    function GetScale(ColumnIndex: Integer): Integer;
    function GetTableName(ColumnIndex: Integer): string;
    function GetCatalogName(ColumnIndex: Integer): string;
    function GetColumnType(ColumnIndex: Integer): TZSQLType;
    function GetColumnTypeName(ColumnIndex: Integer): string;
    function IsReadOnly(ColumnIndex: Integer): Boolean;
    /// <summary>Set the readonly state of a field. The value will be ignored
    ///  if the field is not writable.</summary>
    /// <param>"ColumnIndex" the columnnumber of the field.</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the dml's.</param>
    procedure SetReadOnly(ColumnIndex: Integer; Value: Boolean);
    function IsWritable(ColumnIndex: Integer): Boolean;
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean;
    function GetDefaultValue(ColumnIndex: Integer): string;
    function HasDefaultValue(ColumnIndex: Integer): Boolean;
  end;

  TOnLobUpdate = procedure(Field: NativeInt) of object;
  /// <summary>
  ///   External or internal blob wrapper object.
  /// </summary>
  IZLob = interface(IZInterface)
    ['{DCF816A4-F21C-4FBB-837B-A12DCF886A6F}']
    function IsEmpty: Boolean;
    function IsUpdated: Boolean;
    procedure SetUpdated(Value: Boolean);
    function Length: Integer; //deprecated;
    procedure Open(LobStreamMode: TZLobStreamMode);
    procedure Clear;
    procedure SetOnUpdateHandler(Handler: TOnLobUpdate; AField: NativeInt);  //this is for the datasets only
  end;

  { IZBlob }

  IZBlob = interface(IZLob)
    ['{47D209F1-D065-49DD-A156-EFD1E523F6BF}']
    function IsClob: Boolean;

    function GetString: RawByteString;
    procedure SetString(const Value: RawByteString);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetStream: TStream; overload;
    procedure SetStream(const Value: TStream); overload;
    function GetBuffer(var LocalBuffer: RawByteString; Out Len: NativeUInt): Pointer;
    procedure SetBuffer(Buffer: Pointer; Length: NativeUInt);

    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    {Clob operations}
    function GetRawByteString(CodePage: Word): RawByteString;
    procedure SetRawByteString(Const Value: RawByteString; const CodePage: Word);
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    procedure SetAnsiString(Const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    procedure SetUTF8String(Const Value: UTF8String);
    {$ENDIF}
    procedure SetUnicodeString(const Value: UnicodeString);
    function GetUnicodeString: UnicodeString;
    //the clob stream implementation
    //procedure SetStream(const Value: TStream; CodePage: Word); overload;
    function GetRawByteStream: TStream;
    function GetAnsiStream: TStream;
    function GetUTF8Stream: TStream;
    function GetUnicodeStream: TStream;
    //the clob buff implementation
    function GetPAnsiChar(CodePage: Word; var LocalBuffer: RawByteString; out Len: NativeUInt): PAnsiChar;
    procedure SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word; Len: NativeUInt);
    function GetPWideChar(var LocalBuffer: UnicodeString; out Len: NativeUint): PWideChar;
    procedure SetPWideChar(Buffer: PWideChar; Len: NativeUInt);
    procedure SetCodePageTo(Value: Word);
  end;

  IZClob = interface(IZBlob)
    ['{2E0ED2FE-5F9F-4752-ADCB-EFE92E39FF94}']
    function GetStream(CodePage: Word): TStream; overload;
    procedure SetStream(const Value: TStream; CodePage: Word); overload;
  end;

  PIZLob = ^IZBlob;
  IZLobDynArray = array of IZBLob;

  /// <summary>
  ///   Database notification interface.
  /// </summary>
  IZNotification = interface(IZInterface)
    ['{BF785C71-EBE9-4145-8DAE-40674E45EF6F}']

    function GetEvent: string;
    procedure Listen;
    procedure Unlisten;
    procedure DoNotify;
    function CheckEvents: string;

    function GetConnection: IZConnection;
  end;

  /// <summary>
  ///   Database sequence generator interface.
  /// </summary>
  IZSequence = interface(IZInterface)
    ['{A9A54FE5-0DBE-492F-8DA6-04AC5FCE779C}']
    function  GetName: string;
    function  GetBlockSize: Integer;
    procedure SetName(const Value: string);
    procedure SetBlockSize(const Value: Integer);
    /// <summary>
    ///  Gets the current value of the sequence
    /// </summary>
    /// <returns>
    ///  the current unique key
    /// </returns>
    function  GetCurrentValue: Int64;
    /// <summary>
    ///  Gets the next unique key generated by this sequence
    /// </summary>
    /// <returns>
    ///  the next generated unique key
    /// </returns>
    function  GetNextValue: Int64;
    function  GetCurrentValueSQL: string;
    function  GetNextValueSQL: string;
    function  GetConnection: IZConnection;
  end;

var
  /// <summary>
  ///   The common driver manager object.
  /// </summary>
  DriverManager: IZDriverManager;
  GlobalCriticalSection: TCriticalSection;
implementation

uses ZMessages, ZEncoding, ZDbcProperties, ZFastCode;

type

  { TZDriverManager }

  /// <summary>
  ///   Driver Manager interface.
  /// </summary>
  TZDriverManager = class(TInterfacedObject, IZDriverManager)
  private
    FDriversCS: TCriticalSection; // thread-safety for FDrivers collection. Not the drivers themselves!
    FLogCS: TCriticalSection;     // thread-safety for logging listeners
    FDrivers: IZCollection;
    FLoggingListeners: IZCollection;
    FGarbageCollector: IZCollection;
    FHasLoggingListener: Boolean;
    procedure InternalLogEvent(const Event: TZLoggingEvent);
    function InternalGetDriver(const Url: string): IZDriver;
  public
    constructor Create;
    destructor Destroy; override;

    function GetConnection(const Url: string): IZConnection;
    function GetConnectionWithParams(const Url: string; Info: TStrings): IZConnection;
    function GetConnectionWithLogin(const Url: string; const User: string;
      const Password: string): IZConnection;

    function GetDriver(const Url: string): IZDriver;
    procedure RegisterDriver(const Driver: IZDriver);
    procedure DeregisterDriver(const Driver: IZDriver);

    function GetDrivers: IZCollection;

    function GetClientVersion(const Url: string): Integer;

    procedure AddLoggingListener(const Listener: IZLoggingListener);
    procedure RemoveLoggingListener(const Listener: IZLoggingListener);
    function HasLoggingListener: Boolean;

    procedure LogMessage(Category: TZLoggingCategory; const Protocol: String;
      const Msg: SQLString); overload;
    procedure LogMessage(const Category: TZLoggingCategory; const Sender: IZLoggingObject); overload;
    procedure LogError(Category: TZLoggingCategory; const Protocol: String;
      const Msg: SQLString; ErrorCode: Integer; const Error: SQLString);

    function ConstructURL(const Protocol, HostName, Database,
      UserName, Password: String; const Port: Integer;
      const Properties: TStrings = nil; const LibLocation: String = ''): String;
    procedure AddGarbage(const Value: IZInterface);
    procedure ClearGarbageCollector;
  end;

{ TZImmediatelyReleasableObject }

procedure TZImmediatelyReleasableObject.AfterConstruction;
var imm: IImmediatelyReleasable;
begin
  QueryInterface(IImmediatelyReleasable, imm);
  FWeakImmediatRelPtr := Pointer(imm);
  imm := nil;
  inherited AfterConstruction; //release constructor RefCnt
end;

{ TZDriverManager }

{**
  Constructs this object with default properties.
}
constructor TZDriverManager.Create;
begin
  FDriversCS := TCriticalSection.Create;
  FLogCS := TCriticalSection.Create;
  FDrivers := TZCollection.Create;
  FLoggingListeners := TZCollection.Create;
  FGarbageCollector := TZCollection.Create;
  FHasLoggingListener := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZDriverManager.Destroy;
begin
  FDrivers := nil;
  FLoggingListeners := nil;
  FreeAndNil(FDriversCS);
  FreeAndNil(FLogCS);
  inherited Destroy;
end;

function TZDriverManager.GetDrivers: IZCollection;
begin
  FDriversCS.Enter;
  try
    Result := TZUnmodifiableCollection.Create(FDrivers);
  finally
    FDriversCS.Leave;
  end;
end;

procedure TZDriverManager.RegisterDriver(const Driver: IZDriver);
begin
  FDriversCS.Enter;
  try
    if not FDrivers.Contains(Driver) then
      FDrivers.Add(Driver);
  finally
    FDriversCS.Leave;
  end;
end;

procedure TZDriverManager.DeregisterDriver(const Driver: IZDriver);
begin
  FDriversCS.Enter;
  try
    FDrivers.Remove(Driver);
  finally
    FDriversCS.Leave;
  end;
end;

function TZDriverManager.GetDriver(const Url: string): IZDriver;
begin
  FDriversCS.Enter;
  Result := nil;
  try
    Result := InternalGetDriver(URL);
  finally
    FDriversCS.Leave;
  end;
end;

function TZDriverManager.GetConnectionWithParams(const Url: string; Info: TStrings):
  IZConnection;
var
  Driver: IZDriver;
begin
  FDriversCS.Enter;
  Driver := nil;
  try
    Driver := InternalGetDriver(URL);
    if Driver = nil then
      raise EZSQLException.Create(SDriverWasNotFound);
    Result := Driver.Connect(Url, Info);
  finally
    FDriversCS.Leave;
  end;
end;

function TZDriverManager.GetClientVersion(const Url: string): Integer;
var
  Driver: IZDriver;
begin
  {$IFNDEF WITH_TRYFINALLY_RESULT_EXCEPTION_CHECK}
  Result := -1;
  {$ENDIF}
  FDriversCS.Enter;
  try
    Driver := InternalGetDriver(URL);
    if Driver = nil then
      raise EZSQLException.Create(SDriverWasNotFound);
    Result := GetClientVersion(Url);
  finally
    FDriversCS.Leave;
  end;
end;

function TZDriverManager.GetConnectionWithLogin(const Url: string; const User: string;
  const Password: string): IZConnection;
var
  Info: TStrings;
  Driver: IZDriver;
begin
  FDriversCS.Enter;
  Info := TStringList.Create;
  Result := nil;
  try
    Info.Values[ConnProps_Username] := User;
    Info.Values[ConnProps_Password] := Password;
    Driver := InternalGetDriver(URL);
    if Driver = nil then
      raise EZSQLException.Create(SDriverWasNotFound);
    Result := Driver.Connect(Url, Info);
  finally
    FreeAndNil(Info);
    FDriversCS.Leave;
  end;
end;

function TZDriverManager.GetConnection(const Url: string): IZConnection;
begin
  Result := GetConnectionWithParams(Url, nil);
end;

procedure TZDriverManager.AddGarbage(const Value: IZInterface);
begin
  FDriversCS.Enter;
  try
    FGarbageCollector.Add(Value);
  finally
    FDriversCS.Leave;
  end;
end;

procedure TZDriverManager.AddLoggingListener(const Listener: IZLoggingListener);
begin
  FLogCS.Enter;
  try
    FLoggingListeners.Add(Listener);
    FHasLoggingListener := True;
  finally
    FLogCS.Leave;
  end;
end;

procedure TZDriverManager.RemoveLoggingListener(const Listener: IZLoggingListener);
begin
  FLogCS.Enter;
  try
    FLoggingListeners.Remove(Listener);
    FHasLoggingListener := (FLoggingListeners.Count>0);
  finally
    FLogCS.Leave;
  end;
end;

function TZDriverManager.HasLoggingListener: Boolean;
begin
  Result := FHasLoggingListener;
end;

function TZDriverManager.InternalGetDriver(const Url: string): IZDriver;
var I: Integer;
begin
  Result := nil;
  for I := 0 to FDrivers.Count - 1 do
    if (FDrivers[I].QueryInterface(IZDriver, Result) = S_OK) and Result.AcceptsURL(Url) then
      Exit;
  Result := nil;
end;

{**
  Logs an error message about event with error result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
procedure TZDriverManager.LogError(Category: TZLoggingCategory;
  const Protocol: String; const Msg: SQLString; ErrorCode: Integer;
  const Error: SQLString);
var
  Event: TZLoggingEvent;
begin
  Event := nil;
  FLogCS.Enter;
  try
    if not FHasLoggingListener then
      Exit;
    Event := TZLoggingEvent.Create(Category, Protocol, Msg, ErrorCode, Error);
    InternalLogEvent(Event);
  finally
    FreeAndNil(Event);
    FLogCS.Leave;
  end;
end;

procedure TZDriverManager.InternalLogEvent(const Event: TZLoggingEvent);
var
  I: Integer;
  Listener: IZLoggingListener;
begin
  for I := 0 to FLoggingListeners.Count - 1 do
    if FLoggingListeners[I].QueryInterface(IZLoggingListener, Listener) = S_OK then
      Listener.LogEvent(Event);
end;

{**
  Logs a message about event with error result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZDriverManager.LogMessage(Category: TZLoggingCategory;
  const Protocol: String; const Msg: SQLString);
var
  Event: TZLoggingEvent;
begin
  Event := nil;
  FLogCS.Enter;
  try
    if not FHasLoggingListener then
      Exit;
    Event := TZLoggingEvent.Create(Category, Protocol, Msg, 0, '');
    InternalLogEvent(Event);
  finally
    FreeAndNil(Event);
    FLogCS.Leave;
  end;
end;

procedure TZDriverManager.LogMessage(const Category: TZLoggingCategory;
  const Sender: IZLoggingObject);
var
  Event: TZLoggingEvent;
begin
  Event := nil;
  FLogCS.Enter;
  try
    if not FHasLoggingListener then
      Exit;
    Event := Sender.CreateLogEvent(Category);
    if Event <> nil then
      InternalLogEvent(Event);
  finally
    FreeAndNil(Event);
    FLogCS.Leave;
  end;
end;

procedure TZDriverManager.ClearGarbageCollector;
begin
  if (FGarbageCollector.Count > 0) {$IFDEF HAVE_CS_TRYENTER}and FDriversCS.TryEnter{$ENDIF} then begin
  {$IFNDEF HAVE_CS_TRYENTER}
    FDriversCS.Enter;
  {$ENDIF}
    try
      FGarbageCollector.Clear;
    finally
      FDriversCS.Leave;
    end;
  end;
end;

function TZDriverManager.ConstructURL(const Protocol, HostName, Database,
  UserName, Password: String; const Port: Integer;
  const Properties: TStrings = nil; const LibLocation: String = ''): String;
var ZURL: TZURL;
begin
  FDriversCS.Enter;
  ZURL := TZURL.Create;
  try
    ZURL.Protocol := Protocol;
    ZURL.HostName := HostName;
    ZURL.Database := DataBase;
    ZURL.UserName := UserName;
    ZURL.Password := Password;
    ZURL.Port := Port;
    if Assigned(Properties) then
      ZURL.Properties.AddStrings(Properties);
    ZURL.LibLocation := LibLocation;
    Result := ZURL.URL;
  finally
    FDriversCS.Leave;
    FreeAndNil(ZURL);
  end;
end;

{ EZSQLThrowable }

constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode := E.ErrorCode;
  FStatusCode := E.Statuscode;
  if E.SpecificData <> nil then
    FSpecificData := E.SpecificData.Clone;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
}
constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
  @param ErrorCode a native server error code.
}
constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

{**
  Creates an exception with message string.
  @param ErrorCode a native server error code.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithCodeAndStatus(ErrorCode: Integer;
  const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
  FStatusCode := StatusCode;
end;

{**
  Creates an exception with message string.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

destructor EZSQLThrowable.Destroy;
begin
  FreeAndNil(FSpecificData);
  inherited;
end;

{ TZCodePagedObject }

function TZCodePagedObject.GetConSettings: PZConSettings;
begin
  Result := FConSettings;
end;

procedure TZCodePagedObject.SetConSettingsFromInfo(Info: TStrings);
var S: String;
begin
  if Assigned(Info) and Assigned(FConSettings) then begin
    S := Info.Values[ConnProps_RawStringEncoding];
    if S = '' then
      S := Info.Values[ConnProps_ControlsCP]; //left for backward compatibility
    S := UpperCase(S);
    if S = 'DB_CP'
    then ConSettings.W2A2WEncodingSource := encDB_CP
    else if S = 'CP_UTF8'
    then ConSettings.W2A2WEncodingSource := encUTF8
    else ConSettings.W2A2WEncodingSource := encDefaultSystemCodePage;
  end;
end;

// escape the ';' char to #9 and LineEnding to ';'
function Escape(const S: string): string; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ReplaceChar(';', #9, S);
  Result := StringReplace(Result, LineEnding, ';', [rfReplaceAll]);
end;

// unescape the ';' to LineEnding and #9 char to ';'
function UnEscape(const S: string): string; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := StringReplace(S, ';', LineEnding, [rfReplaceAll]);
  Result := ReplaceChar(#9, ';', Result);
end;

{TZURLStringList}

function TZURLStringList.GetURLText: String;
var P: PChar absolute Result;
begin
  Result := Escape(Text);
  if (P+Length(Result)-1)^ = ';' then
    SetLength(Result, Length(Result)-1);
end;

procedure TZURLStringList.SetURLText(const Value: string);
begin
  Text := UnEscape(Value);
end;

{ TZURL }

constructor TZURL.Create;
begin
  inherited;

  FPrefix := 'zdbc';
  FProperties := TZURLStringList.Create;
  FProperties.CaseSensitive := False;
  FProperties.NameValueSeparator := '=';
  FProperties.OnChange := DoOnPropertiesChange;
end;

constructor TZURL.Create(const AURL: String);
begin
  Create;
  Self.URL := AURL;
end;

// Values from Info overwrite those from URL
constructor TZURL.Create(const AURL: String; Info: TStrings);
begin
  Create(AURL);
  if Assigned(Info) then
    AddValues(Info);
end;

constructor TZURL.Create(const AURL: TZURL);
begin
  Create(AURL.URL);
end;

// Values from parameters overwrite those from URL and values from Info overwrite both
// TODO: this method is odd... properties of URL, except protocol, get overridden
// with parameters. Likely AProtocol should go here instead of AURL
constructor TZURL.Create(Const AURL, AHostName: string; const APort: Integer;
  const ADatabase, AUser, APassword: string; Info: TStrings);
begin
  Create(AURL);
  Self.HostName := AHostName;
  Self.Port := APort;
  Self.Database := ADataBase;
  Self.UserName := AUser;
  Self.Password := APassword;
  if Assigned(Info) then
    AddValues(Info);
end;

destructor TZURL.Destroy;
begin
  FProperties.Free;

  inherited;
end;

procedure TZURL.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TZURL.SetProtocol(const Value: string);
begin
  FProtocol := Value;
end;

procedure TZURL.SetHostName(const Value: string);
begin
  FHostName := Escape(Value);
end;

procedure TZURL.SetConnPort(const Value: Integer);
begin
  FPort := Value;
end;

function TZURL.GetDatabase: string;
begin
  Result := UnEscape(FDatabase);
end;

procedure TZURL.SetDatabase(const Value: string);
begin
  FDatabase := Escape(Value);
end;

function TZURL.GetUserName: string;
begin
  Result := UnEscape(FUserName);
end;

procedure TZURL.SetUserName(const Value: string);
begin
  FUserName := Escape(Value);
end;

function TZURL.GetPassword: string;
begin
  Result := UnEscape(FPassword);
end;

procedure TZURL.SetPassword(const Value: string);
begin
  FPassword := Escape(Value);
end;

function TZURL.GetLibLocation: String;
begin
  Result := UnEscape(FLibLocation);
end;

procedure TZURL.SetLibLocation(const Value: String);
begin
  FLibLocation := Escape(Value);
end;

function TZURL.GetURL: string;
var
  Params: string;
begin
  // Prefix, Protocol and always set the doubleslash to avoid unix '/' path issues if host is empty
  Result := Prefix + ':' + Protocol + ':' + '//';

  // HostName/Port
  if HostName <> '' then
  begin
    Result := Result + HostName;
    if Port <> 0 then
      Result := Result + ':' + ZFastCode.IntToStr(Port);
  end;

  // Database
  if Database <> '' then
    Result := Result + '/' + FDatabase;

  // Join the params

  Params := '';

  if FUserName <> '' then
    AppendSepString(Params, 'username=' + FUserName, ';');
  if FPassword <> '' then
    AppendSepString(Params, 'password=' + FPassword, ';');
  if Properties.Count > 0 then
    AppendSepString(Params, Properties.URLText, ';'); //Adds the escaped string
  if FLibLocation <> '' then
    AppendSepString(Params, 'LibLocation='+ FLibLocation, ';');

  // Construct the final string

  if Params <> '' then
    Result := Result + '?' + Params;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5091 off : Local variable "AValue" of managed type does not seem to be initialized}
{$ENDIF}
procedure TZURL.SetURL(const Value: string);
var
  APrefix: string;
  AProtocol: string;
  AHostName: string;
  APort: string;
  ADatabase: string;
  AProperties: string;
  AValue: string;
  I: Integer;
begin
  APrefix := '';
  AProtocol := '';
  AHostName := '';
  APort := '';
  ADatabase := '';
  AProperties := '';

  // Strip out the parameters
  BreakString(Value, '?', AValue, AProperties);

  // APrefix
  I := ZFastCode.Pos(':', AValue);
  if I = 0 then
    raise Exception.Create('TZURL.SetURL - The prefix is missing');
  BreakString(AValue, ':', APrefix, AValue);

  // AProtocol
  I := ZFastCode.Pos(':', AValue);
  if I = 0 then
    raise Exception.Create('TZURL.SetURL - The protocol is missing');
  BreakString(AValue, ':', AProtocol, AValue);

  if StartsWith(AValue, '//') then
  begin
    Delete(AValue, 1, Length('//'));
    // Strip "hostname[:port]" out of "/database"
    BreakString(AValue, '/', AValue, ADatabase);
    // AHostName, APort
    BreakString(AValue, ':', AHostName, APort);
  end
  else
  begin
    // Likely a database delimited by / so remove the /
    if StartsWith(AValue, '/') then
      Delete(AValue, 1, Length('/'));
    // ADatabase
    ADatabase := AValue;
  end;

  FPrefix := APrefix;
  FProtocol := AProtocol;
  FHostName := AHostName;
  FPort := StrToIntDef(APort, 0);
  FDatabase := ADatabase;

  // Clear fields that MUST be assigned from properties even if empty.
  // LibLocation should remain uncleared
  FUserName := '';
  FPassword := '';
  FProperties.URLText := AProperties; // will launch DoOnPropertiesChange
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZURL.DoOnPropertiesChange(Sender: TObject);

  // Return a value named ValueName from FProperties and delete the item
  function ExtractValueFromProperties(const ValueName: string): string;
  var I: Integer;
  begin
    Result := '';
    I := FProperties.IndexOfName(ValueName);
    if I = -1 then Exit;
    Result := FProperties.ValueFromIndex[I];
    FProperties.Delete(I);
  end;

var
  S: string;
begin
  FProperties.OnChange := nil; // prevent re-entering

  S := ExtractValueFromProperties(ConnProps_UID);
  if S <> '' then
    UserName := S;

  S := ExtractValueFromProperties(ConnProps_Username);
  if S <> '' then
    UserName := S;

  S := ExtractValueFromProperties(ConnProps_PWD);
  if S <> '' then
    Password := S;

  S := ExtractValueFromProperties(ConnProps_Password);
  if S <> '' then
    Password := S;

  S := ExtractValueFromProperties(ConnProps_LibLocation);
  if S <> '' then
    LibLocation := S;

  FProperties.OnChange := DoOnPropertiesChange;

  if Assigned(FOnPropertiesChange) then
    FOnPropertiesChange(Sender);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "Param" does not seem to be initialized}
  {$WARN 5091 off : Local variable "Param" of managed type does not seem to be initialized}
{$ENDIF}
procedure TZURL.AddValues(Values: TStrings);
var
  I: Integer;
  Param, Value: String;
begin
  FProperties.BeginUpdate; // prevent calling OnChange on every iteration
  for I := 0 to Values.Count -1 do
  begin
    BreakString(Values[I], '=', Param, Value);
    if Value <> '' then
      FProperties.Values[Param] := Value
    else
      if FProperties.IndexOf(Values[I]) = -1 then //add unique params only!
        FProperties.Add(Values[I]);
  end;
  FProperties.EndUpdate;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


initialization
  DriverManager := TZDriverManager.Create;
  GlobalCriticalSection := TCriticalSection.Create;
finalization
  DriverManager := nil;
  FreeAndNil(GlobalCriticalSection);
end.

