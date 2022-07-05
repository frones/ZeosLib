{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcConnection;

interface

{$I ZDbc.inc}

uses
{$IFDEF WITH_LCONVENCODING}
  LConvEncoding,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF TLIST_IS_DEPRECATED}ZSysUtils,{$ENDIF}
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZClasses, ZDbcIntfs, ZTokenizer, ZCompatibility, ZGenericSqlToken, ZVariant,
  ZGenericSqlAnalyser, ZPlainDriver, ZCollections, ZDbcLogging, ZExceptions;

type

  /// <summary>Implements an abstract Database Driver object. </summary>
  TZAbstractDriver = class(TInterfacedObject, IZDriver)
  protected
    FCachedPlainDrivers: IZHashMap;
    FSupportedProtocols: TStringDynArray;
    procedure AddSupportedProtocol(const AProtocol: String);
    function AddPlainDriverToCache(const PlainDriver: IZPlainDriver; const Protocol: string = ''; const LibLocation: string = ''): String;
    function GetPlainDriverFromCache(const Protocol, LibLocation: string): IZPlainDriver;
    /// <summary>Gets plain driver for selected protocol.</summary>
    /// <param>"url" the URL of the driver.</param>
    /// <returns>a selected plaindriver interface.</returns>
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; virtual;
  public
    /// <summary>Constructs this object with default properties.</summary>
    constructor Create; virtual;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
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
    function Connect(const Url: string; Info: TStrings = nil): IZConnection; overload;// deprecated;
    /// <summary>Attempts to create a database connection to the given URL.
    ///  The driver should return "null" if it realizes it is the wrong kind
    ///  of driver to connect to the given URL. This will be common, as when
    ///  the zeos driver manager is asked to connect to a given URL it passes
    ///  the URL to each loaded driver in turn.
    ///  The driver should raise a SQLException if it is the right
    ///  driver to connect to the given URL, but has trouble loading the
    ///  library. </summary>
    ///  <param> url the TZURL Object used to find the Driver, it's library and
    ///    assigns the connection properties.</param>
    ///  <returns> a <c>IZConnection</c> interface that represents a
    ///    connection to the URL</returns>
    function Connect(const {%H-}Url: TZURL): IZConnection; overload; virtual;
    /// <summary>Returns true if the driver thinks that it can open a connection
    ///  to the given URL.  Typically drivers will return true if they
    ///  understand the subprotocol specified in the URL and false if they
    ///  don't.</summary>
    /// <param>"url" the URL of the database</param>
    /// <returns>true if this driver can connect to the given URL.</returns>
    function AcceptsURL(const Url: string): Boolean; virtual;
    /// <summary>Gets information about the possible properties for this driver.
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
    function GetPropertyInfo(const {%H-}Url: string; {%H-}Info: TStrings): TStrings; virtual;
    /// <summary>Gets the driver's major version number. Initially this should
    ///  be 1.</summary>
    /// <returns>this driver's major version number.</returns>
    function GetMajorVersion: Integer; virtual;
    /// <summary>Gets the driver's minor version number. Initially this should
    ///  be 0.</summary>
    /// <returns>this driver's minor version number.</returns>
    function GetMinorVersion: Integer; virtual;
    /// <summary>Gets the driver's sub version (revision) number. Initially
    ///  this should be 0.</summary>
    /// <returns>this driver's minor version number.</returns>
    function GetSubVersion: Integer; virtual;
    /// <summary>Creates a generic tokenizer object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetTokenizer: IZTokenizer; virtual;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser; virtual;
    /// <summary>Returns the version of the plain driver library that will be
    ///  used to open a connection to the given URL.</summary>
    /// <param>"url" the URL of the databaseparam</param>
    /// <returns>the version number of the plain driver library for the give
    ///  URL.</returns>
    function GetClientVersion(const {%H-}Url: string): Integer; virtual;
  end;

  /// <summary>Implements an abstract Database Connection object</summary>
  TZAbstractDbcConnection = class(TZImmediatelyReleasableObject, IImmediatelyReleasable)
  private
    FOnConnectionLostError: TOnConnectionLostError; //error handle which can be registered
    FDriver: IZDriver;
    FDriverManager: IZDriverManager; //just keep refcount high until last conection is gone e.g. Logging
    FIZPlainDriver: IZPlainDriver;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FClosed: Boolean;
    FURL: TZURL;
    FUseMetadata: Boolean;
    FClientVarManager: IZClientVariantManager;
    fRegisteredStatements: TZSortedList; //weak reference to pending stmts
    fAddLogMsgToExceptionOrWarningMsg: Boolean;
    fRaiseWarnings: Boolean;
    function GetHostName: string;
    procedure SetHostName(const Value: String);
    function GetPort: Integer;
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: String);
    function GetUser: string;
    procedure SetUser(const Value: String);
    function GetPassword: string;
    procedure SetPassword(const Value: String);
    function GetInfo: TStrings;
  protected
    FByteBuffer: TByteBuffer; //have a static buffer for any conversion oslt
    fWeakReferenceOfSelfInterface: Pointer;
    FWeakEventListenerSelfPtr, FCreatedWeakEventListenerPtr: Pointer;
    FRestartTransaction: Boolean;
    FDisposeCodePage: Boolean;
    FClientCodePage: String;
    FMetadata: TContainedObject;
    {$IFDEF ZEOS_TEST_ONLY}
    FTestMode: Byte;
    {$ENDIF}
    FLogMessage: SQLString;
    /// <summary>Releases a Connection's database and resources immediately
    ///  instead of waiting for them to be automatically released.</summary>
    ///  Note: A Connection is automatically closed when it is garbage
    ///  collected. Certain fatal errors also result in a closed Connection.</summary>
    procedure InternalClose; virtual; abstract;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a raw encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; virtual;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a UTF16 encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; virtual;
    procedure SetDateTimeFormatProperties(DetermineFromInfo: Boolean = True);
    procedure ResetCurrentClientCodePage(const Name: String;
      IsStringFieldCPConsistent: Boolean);
    function GetEncoding: TZCharEncoding;
    function GetClientVariantManager: IZClientVariantManager;
    /// <author>EgonHugeist</author>
    /// <summary>Check if the given Charset for Compiler/Database-Support.
    ///  Not supported means if there is a possible String-DataLoss.
    ///  So it raises an Exception if case of settings. This handling
    ///  is an improofment to inform Zeos-Users about the troubles the given
    ///  CharacterSet may have.</summary>
    /// <param>"CharSet" the CharacterSet which has to be proofed</param>
    /// <param>"DoArrange" represents a switch to check and set a aternative
    ///  ZAlias as default. This means it ignores the choosen
    ///  Client-CharacterSet and sets a "more" Zeos-Compatible
    ///  Client-CharacterSet if known.</param>
    procedure CheckCharEncoding(const CharSet: String; const DoArrange: Boolean = False);
    procedure OnPropertiesChange({%H-}Sender: TObject); virtual;
    procedure LogError(const Category: TZLoggingCategory; ErrorCode: Integer;
      const Sender: IImmediatelyReleasable; const Msg, Error: String);
    procedure SetOnConnectionLostErrorHandler(Handler: TOnConnectionLostError);
    procedure SetAddLogMsgToExceptionOrWarningMsg(Value: Boolean);
    procedure SetRaiseWarnings(Value: Boolean);
    procedure RegisterStatement(const Value: IZStatement);
    procedure DeregisterStatement(const Value: IZStatement);
    procedure CloseRegisteredStatements;

    property Driver: IZDriver read FDriver write FDriver;
    property PlainDriver: IZPlainDriver read FIZPlainDriver write FIZPlainDriver;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Info: TStrings read GetInfo;
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property URL: TZURL read FURL;
    property TransactIsolationLevel: TZTransactIsolationLevel
      read FTransactIsolationLevel write FTransactIsolationLevel;
    property DriverManager: IZDriverManager read FDriverManager;
  public
    /// <author>EgonHugeist and MDeams</author>
    /// <summary>Constructs this object and assignes the main properties.</summary>
    /// <param>"Driver" the parent ZDBC driver.</param>
    /// <param>"Url" a connection URL.</param>
    /// <param>"PlainDriver" a versioned ZPlainDriver object interface.</param>
    /// <param>"HostName" a name of the host.</param>
    /// <param>"Port" a port number (0 for default port).</param>
    /// <param>"Database" a name of the database.</param>
    /// <param>"User" a user name.</param>
    /// <param>"Password" a user password.</param>
    /// <param>"Info" a string list with extra connection parameters.</param>
    /// <remarks>The old deprecadet constructor which was used
    ///  from the descendant classes. We left him here for compatibility reasons
    ///  to existing projects which using the DbcConnections directly.</remarks>
    constructor Create(const {%H-}Driver: IZDriver; const Url: string;
      const {%H-}PlainDriver: IZPlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string;
      Info: TStrings); overload;
    /// <summary>Constructs this object and assignes the main properties.</summary>
    /// <param>"ZUrl§ a connection ZURL-object which exports all connection
    ///  parameters.</param>
    constructor Create(const ZUrl: TZURL); overload;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    procedure AfterConstruction; override;

    /// <summary>Creates a <c>Statement</c> interface for sending SQL statements
    ///  to the database. SQL statements without parameters are normally
    ///  executed using Statement objects. If the same SQL statement
    ///  is executed many times, it is more efficient to use a
    ///  <c>PreparedStatement</c> object. Result sets created using the returned
    ///  <c>Statement</c> interface will by default have forward-only type and
    ///  read-only concurrency.</summary>
    /// <returns>A new Statement interface</returns>
    function CreateStatement: IZStatement;
    /// <summary>Creates a <c>PreparedStatement</c> interface for sending
    ///  parameterized SQL statements to the database. A SQL statement with
    ///  or without IN parameters can be pre-compiled and stored in a
    ///  PreparedStatement object. This object can then be used to efficiently
    ///  execute this statement multiple times.
    ///  Note: This method is optimized for handling parametric SQL statements
    ///  that benefit from precompilation. If the driver supports
    ///  precompilation, the method <c>prepareStatement</c> will send the
    ///  statement to the database for precompilation. Some drivers may not
    ///  support precompilation. In this case, the statement may not be sent to
    ///  the database until the <c>PreparedStatement</c> is executed. This has
    ///  no direct effect on users; however, it does affect which method throws
    ///  certain SQLExceptions. Result sets created using the returned
    ///  PreparedStatement will have forward-only type and read-only
    ///  concurrency, by default.</summary>
    /// <param>"SQL" a SQL statement that may contain one or more '?' IN
    ///  parameter placeholders.</param>
    /// <param> Info a statement parameter list.</param>
    /// <returns>a new PreparedStatement object containing the
    ///  optional pre-compiled statement</returns>
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    /// <summary>Creates a <c>CallableStatement</c> object for calling
    ///  database stored procedures. The <c>CallableStatement</c> object
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
    /// <returns> a new IZCallableStatement interface containing the
    ///  pre-compiled SQL statement </returns>
    function PrepareCall(const Name: string): IZCallableStatement;
    /// <summary>Creates a sequence generator object.</summary>
    /// <param>"Sequence" a name of the sequence generator.</param>
    /// <param>"BlockSize" a number of unique keys requested in one trip to SQL
    ///  server.</param>
    /// <returns>returns a created sequence object.</returns>
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; virtual;
    /// <summary>Converts the given SQL statement into the system's native SQL
    ///  grammar. A driver may convert the JDBC sql grammar into its system's
    ///  native SQL grammar prior to sending it; this method returns the
    ///  native form of the statement that the driver would have sent.</summary>
    /// <param>"SQL" a SQL statement that may contain one or more '?' parameter
    ///  placeholders</param>
    /// <return>the native form of this statement</returns>
    function NativeSQL(const SQL: string): string; virtual;
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
    procedure SetAutoCommit(Value: Boolean); virtual;
    /// <summary>Gets the current auto-commit state. See setAutoCommit.</summary>
    /// <returns>the current state of auto-commit mode.</returns>
    function GetAutoCommit: Boolean; virtual;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const {%H-}transactionid: string);virtual;
    procedure CommitPrepared(const {%H-}transactionid: string);virtual;
    procedure RollbackPrepared(const {%H-}transactionid: string);virtual;
    /// <author>firmos (initially for MySQL 27032006)</author>
    /// <summary>Ping Current Connection's server, if client was disconnected,
    ///  the connection is resumed.</summary>
    /// <returns>0 if succesfull or error code if any error occurs</returns>
    function PingServer: Integer; virtual;
    /// <author>aehimself</author>
    /// <summary>Immediately abort any kind of queries.</summary>
    /// <returns>0 if the operation is aborted; Non zero otherwise.</returns>
    function AbortOperation: Integer; virtual;
    /// <summary>Escape a string so it's acceptable for the Connection's server.</summary>
    /// <param>"value" a string that should be escaped</param>
    /// <returns>the escaped string</returns>
    function EscapeString(const Value: RawByteString): RawByteString; overload; virtual;
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open; virtual;
    /// <summary>Releases a Connection's database and resources immediately
    ///  instead of waiting for them to be automatically released. Note: A
    ///  Connection is automatically closed when it is garbage collected.
    ///  Certain fatal errors also result in a closed Connection.</summary>
    procedure Close;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
    /// <summary>Tests to see if a Connection is closed.</summary>
    /// <returns><c>True</c> if the connection is closed; <c>False</c> if it's
    ///  still open</returns>
    function IsClosed: Boolean; virtual;

    function GetDriver: IZDriver;
    /// <summary>Gets the plain driver.</summary>
    /// <returns>the plain driver interface.</returns>
    function GetIZPlainDriver: IZPlainDriver;
    /// <summary>Gets the metadata regarding this connection's database.
    ///  A Connection's database is able to provide information
    ///  describing its tables, its supported SQL grammar, its stored
    ///  procedures, the capabilities of this connection, and so on. This
    ///  information is made available through a DatabaseMetaData object.</summary>
    /// <returns>a DatabaseMetaData object for this Connection.</returns>
    function GetMetadata: IZDatabaseMetadata;
    /// <summary>Gets a connection parameters.</summary>
    /// <returns>a list with connection parameters.</returns>
    function GetParameters: TStrings;
    /// <author>fduenas</author>
    /// <summary>Gets the client's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this clients's full version number</returns>
    function GetClientVersion: Integer; virtual;
    /// <author>fduenas</author>
    /// <summary>Gets the host's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this server's full version number</returns>
    function GetHostVersion: Integer; virtual;
    /// <summary>Gets the PlainDriver description.</summary>
    /// <returns>a the description.</returns>
    function GetDescription: String;
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean); virtual;
    /// <summary>Check if the current conenction is readonly. See setReadonly.
    ///  </summary>
    /// <returns><c>True</c> if the conenction is readonly; <c>False</c>
    ///  otherwise.</returns>
    function IsReadOnly: Boolean; virtual;
    /// <summary>Get's the escaped URL used for establishing this connection.</summary>
    /// <returns>the escaped URL.</returns>
    function GetURL: String;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const {%H-}Catalog: string); virtual;
    /// <summary>Returns the Connection's current catalog name.</summary>
    /// <returns>the current catalog name or an empty string.</returns>
    function GetCatalog: string; virtual;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); virtual;
    /// <summary>Gets this Connection's current transaction isolation level.</summary>
    /// <returns>the current TRANSACTION_* mode value.</returns>
    function GetTransactionIsolation: TZTransactIsolationLevel; virtual;
    /// <summary>Returns the first warning reported by calls on this Connection.</summary>
    /// <remarks>Subsequent warnings will be chained to this EZSQLWarning.</remarks>
    /// <returns>the first SQLWarning or nil.</returns>
    function GetWarnings: EZSQLWarning; virtual;
    /// <summary>Clears all warnings reported for this <c>Connection</c> object.
    ///  After a call to this method, the method <c>getWarnings</c> returns nil
    ///  until a new warning is reported for this Connection.</summary>
    procedure ClearWarnings; virtual;

    function GetBinaryEscapeString(const Value: TBytes): String; overload; virtual;

    /// <summary>Escape a string so it's acceptable for the Connection's server.</summary>
    /// <param>"value" a string that should be escaped</param>
    /// <returns>an escaped string</returns>
    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload; virtual;
    /// <summary>Escape a string so it's acceptable for the Connection's server.</summary>
    /// <param>"value" a string that should be escaped</param>
    /// <returns>an escaped string</returns>
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; virtual;
    /// <summary>Are metadata used? If <c>True</c> then we can determine if
    ///  columns are writeable or not. This is required for generating automatic
    ///  updates.</summary>
    /// <returns><c>True</c> if metainformations are turned on; <c>False</c>
    ///  otherwise.</returns>
    function UseMetadata: boolean;
    /// <summary>Sets the use of metadata informations. This is required for
    ///  generating automatic updates.</summary>
    /// <param>"Value" enables or disables the metadata loading.</param>
    procedure SetUseMetadata(Value: Boolean); virtual;
    /// <summary>Returns the ServicerProvider for this connection. For some
    ///  drivers the connection must be opened to determine the provider.</summary>
    /// <returns>the ServerProvider or spUnknown if not known.</returns>
    function GetServerProvider: TZServerProvider; virtual;
    /// <summary>Get a generic event alerter object.</summary>
    /// <param>"Handler" an event handler which gets triggered if the event is received.</param>
    /// <param>"CloneConnection" if <c>True</c> a new connection will be spawned.</param>
    /// <returns>a the generic event alerter object as interface or nil.</returns>
    function GetEventListener(Handler: TZOnEventHandler; CloneConnection: Boolean;
      Options: TStrings): IZEventListener; virtual;
    /// <summary>Check if the connection supports an event Listener.</summary>
    /// <returns><c>true</c> if the connection supports an event Listener;
    /// <c>false</c> otherwise.</returns>
    function SupportsEventListener: Boolean;
    /// <summary>Closes the event alerter.</summary>
    /// <param>"Value" a reference to the previously created alerter to be released.</param>
    procedure CloseEventListener(var Value: IZEventListener);
  protected
    /// <summary>Get the refrence to a fixed TByteBuffer.</summary>
    /// <returns>the address of the TByteBuffer</returns>
    function GetByteBufferAddress: PByteBuffer;
  protected
    property Closed: Boolean read IsClosed write FClosed;
    property AddLogMsgToExceptionOrWarningMsg: Boolean read
      fAddLogMsgToExceptionOrWarningMsg write fAddLogMsgToExceptionOrWarningMsg;
    property RaiseWarnings: Boolean read fRaiseWarnings write fRaiseWarnings;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract Database Connection object for
  ///  driveres having no simultaneous transaction support</summary>
  TZAbstractSingleTxnConnection = class(TZAbstractDbcConnection,
    IZTransactionManager)
  protected
    FSavePoints: TStrings;
    FTransactionLevel: Integer;
    fTransactions: IZCollection;
    fActiveTransaction: IZTransaction;
    fWeakTxnPtr: Pointer;
  public //implement IZTransaction
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
  public //implement IZTransactionManager
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    /// <summary>Remove the given transaction interface from the manager list.
    ///  This method will be called from the Transaction interface when the
    ///  Transaction gets closed. If the interface is unknown an SQLException
    ///  will be raised.</summary>
    /// <param>"Value" the Transaction interface which should be removed.</param>
    procedure ReleaseTransaction(const Value: IZTransaction);
    /// <summary>Test if the interface is known in the Transaction manager.
    ///  This is usefull if the txn interface was managed, the connection was
    ///  lost and the txn interface is in destruction.</summary>
    /// <param>"Value" the Transaction interface which should be checked.</param>
    /// <returns><c>True</c> if the transaction is known; <c>False</c>
    ///  otherwise.</returns>
    function IsTransactionValid(const Value: IZTransaction): Boolean;
    /// <summary>Clears all transactions.</summary>
    procedure ClearTransactions;
    /// <summary>Get the active transaction interces of the connection.</summary>
    /// <returns>The transaction object</returns>
    function GetConnectionTransaction: IZTransaction;
  public //implement IImmediatelyReleasable
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  ///<summary>Implements an Abstract Sequence generator.</summary>
  TZAbstractSequence = class(TInterfacedObject, IZSequence)
  private
    FConnection: IZConnection;
    FNextValRS, FCurrValRS: IZResultSet;
    FNextValStmt, FCurrValStmt: IZPreparedStatement;
  protected
    FName: string;
    FBlockSize: Integer;
    /// <summary>Sets a name of the sequence generator..</summary>
    /// <param>"Value" a name of this sequence generator.</param>
    procedure SetName(const Value: string); virtual;
    /// <summary>Sets the block size for this sequence.</summary>
    /// <param>Value the block size.</param>
    procedure SetBlockSize(const Value: Integer); virtual;
    property Connection: IZConnection read FConnection write FConnection;
    /// <summary>Flushs open statements and resultsets.</summary>
    /// <param>Value the block size.</param>
    procedure FlushResults;
  public
    /// <summary>Creates this sequence object.</summary>
    /// <param>"Connection" an SQL connection object.</param>
    /// <param>"Name" a name of the sequence generator.</param>
    /// <param>"BlockSize" a number of unique keys requested in one trip to server.</param>
    constructor Create(const Connection: IZConnection; const Name: string;
      BlockSize: Integer);

    /// <summary>Gets the current value of the sequence</summary>
    /// <returns>the current unique key</returns>
    function GetCurrentValue: Int64;
    /// <summary>Gets the next unique key generated by this sequence</summary>
    /// <returns>the next generated unique key</returns>
    function GetNextValue: Int64;
    /// <summary>Get the name of the sequence generator.</summary>
    /// <returns> a name of this sequence generator.</returns>
    function GetName: string;
    /// <summary>Get the blocksize/increment_by value of the sequence generator.</summary>
    /// <returns>the assigned increment_by value.</returns>
    function GetBlockSize: Integer;
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; virtual; abstract;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; virtual; abstract;
    /// <summary>Returns the <c>Connection</c> interface
    ///  that produced this <c>Sequence</c> object.</summary>
    /// <returns>the connection that produced this sequence.</returns>
    function GetConnection: IZConnection;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract identifier sequence object.</summary>
  TZIdentifierSequence = Class(TZAbstractSequence)
  protected
    /// <summary>Sets a name of the sequence generator..</summary>
    /// <param>"Value" a name of this sequence generator.</param>
    procedure SetName(const Value: string); override;
  End;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract MSSQL sequence object.</summary>
  TZMSSQLSequence = class(TZAbstractSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract sequence using the <Name>.CURRVAL/NEXTVAL Syntax</summary>
  TZDotCurrvalNextvalSequence = class(TZIdentifierSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a Sybase SQL Anywhere sequence.</summary>
  TZSybaseASASequence = class(TZDotCurrvalNextvalSequence);

  /// <author>EgonHugeist</author>
  /// <summary>Implements an Informix sequence.</summary>
  TZInformixSequence = class(TZDotCurrvalNextvalSequence);

  /// <author>EgonHugeist</author>
  /// <summary>Implements an DB2 sequence.</summary>
  TZDB2Sequence = class(TZDotCurrvalNextvalSequence);

  /// <author>EgonHugeist</author>
  /// <summary>Implements an CUBRID sequence.</summary>
  TZCubridSequence = class(TZDotCurrvalNextvalSequence);

  /// <author>EgonHugeist</author>
  /// <summary>Implements an Oracle sequence.</summary>
  TZOracleSequence = class(TZDotCurrvalNextvalSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a FireBird2+ sequence.</summary>
  TZFirebird2UpSequence = class(TZIdentifierSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a postresql sequence.</summary>
  TZPostgreSQLSequence = class(TZAbstractSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>defines a class of an TZAbstractSequence</summary>
  TZAbstractSequenceClass = class of TZAbstractSequence;

  /// <summary>Implements a variant manager with connection related convertion rules.</summary>
  TZClientVariantManager = class (TZSoftVariantManager, IZVariantManager, IZClientVariantManager)
  protected
    FConSettings: PZConSettings;
    FClientCP, FStringCP: Word;
    FUseWComparsions: Boolean;
    procedure ProcessString(const Value: TZVariant; out Result: TZVariant); override;
    procedure ProcessUnicodeString(const Value: TZVariant; out Result: TZVariant); override;
    procedure ProcessRawByteString(const Value: TZVariant; out Result: TZVariant); override;
    {$IFNDEF NO_ANSISTRING}procedure ProcessAnsiString(const Value: TZVariant; out Result: TZVariant); override; {$ENDIF NO_ANSISTRING}
    {$IFNDEF NO_UTF8STRING}procedure ProcessUTF8String(const Value: TZVariant; out Result: TZVariant); override; {$ENDIF NO_UTF8STRING}
    procedure ProcessCharRec(const Value: TZVariant; out Result: TZVariant); override;
  public
    /// <summary>Constructs this object and assigns the main properties.</summary>
    /// <param>"ConSettings" a connection settings record reference</summary>.
    constructor Create(const ConSettings: PZConSettings);
    function UseWComparsions: Boolean;
    function GetAsDateTime(const Value: TZVariant): TDateTime; reintroduce;
  end;

  PZEvent = ^TZEvent;
  TZEvent = record
    Name: SQLString;
    Handler: TZOnEventHandler;
  end;

  TZEventList = class(TZCustomElementList)
  private
    FHandler: TZOnEventHandler;
  protected
    class function GetElementSize: Cardinal; virtual;
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(Handler: TZOnEventHandler);
    procedure Add(const Name: String; Handler: TZOnEventHandler);
    procedure Remove(const Name: String);
    function Get(const Index: NativeInt): PZEvent;
    function GetByName(const Name: String): PZEvent;
    property Handler: TZOnEventHandler read FHandler;
  end;

type
  TZSavePointQueryType = (spqtSavePoint, spqtCommit, spqtRollback);

const
  cSavePoint = 'SAVEPOINT ';
  cSaveTrans = 'SAVE TRANSACTION ';
  cReleaseSP = 'RELEASE SAVEPOINT ';
  cEmpty = '';
  cRollbackTran = 'ROLLBACK TRANSACTION ';
  cRollbackTo = 'ROLLBACK TO ';
  cRollbackToSP = 'ROLLBACK TO SAVEPOINT ';
  cUnknown = '';

  cSavePointSyntaxW: array[TZServerProvider, TZSavePointQueryType] of UnicodeString =
    ( ({spUnknown}    cUnknown,   cUnknown,   cUnknown),
      ({spMSSQL}      cSaveTrans, cEmpty,     cRollbackTran),
      ({spMSJet}      cUnknown,   cUnknown,   cUnknown),
      ({spOracle}     cSavePoint, cEmpty,     cRollbackTo),
      ({spASE}        cSaveTrans, cEmpty,     cRollbackTran),
      ({spASA}        cSavePoint, cReleaseSP, cRollbackToSP),
      ({spPostgreSQL} cSavePoint, cReleaseSP, cRollbackTo),
      ({spIB_FB}      cSavePoint, cReleaseSP, cRollbackTo),
      ({spMySQL}      cSavePoint, cReleaseSP, cRollbackTo),
      ({spNexusDB}    cUnknown,   cUnknown,   cUnknown),
      ({spSQLite}     cSavePoint, cReleaseSP, cRollbackTo),
      ({spDB2}        cUnknown,   cUnknown,   cUnknown),
      ({spAS400}      cUnknown,   cUnknown,   cUnknown),
      ({spInformix}   cUnknown,   cUnknown,   cUnknown),
      ({spCUBRID}     cUnknown,   cUnknown,   cUnknown),
      ({spFoxPro}     cUnknown,   cUnknown,   cUnknown)
    );
  cSavePointSyntaxA: array[TZServerProvider, TZSavePointQueryType] of RawByteString =
    ( ({spUnknown}    cUnknown,   cUnknown,   cUnknown),
      ({spMSSQL}      cSaveTrans, cEmpty,     cRollbackTran),
      ({spMSJet}      cUnknown,   cUnknown,   cUnknown),
      ({spOracle}     cSavePoint, cEmpty,     cRollbackTo),
      ({spASE}        cSaveTrans, cEmpty,     cRollbackTran),
      ({spASA}        cSavePoint, cReleaseSP, cRollbackToSP),
      ({spPostgreSQL} cSavePoint, cReleaseSP, cRollbackTo),
      ({spIB_FB}      cSavePoint, cReleaseSP, cRollbackTo),
      ({spMySQL}      cSavePoint, cReleaseSP, cRollbackTo),
      ({spNexusDB}    cUnknown,   cUnknown,   cUnknown),
      ({spSQLite}     cSavePoint, cReleaseSP, cRollbackTo),
      ({spDB2}        cUnknown,   cUnknown,   cUnknown),
      ({spAS400}      cUnknown,   cUnknown,   cUnknown),
      ({spInformix}   cUnknown,   cUnknown,   cUnknown),
      ({spCUBRID}     cUnknown,   cUnknown,   cUnknown),
      ({spFoxPro}     cUnknown,   cUnknown,   cUnknown)
    );

  sCommitMsg = 'COMMIT TRANSACTION';
  sRollbackMsg = 'ROLLBACK TRANSACTION';

  sSessionTransactionIsolation: array[TZTransactIsolationLevel] of
    String = (
    'SET TRANSACTION ISOLATION LEVEL UNDEFINED',
    'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
    'SET TRANSACTION ISOLATION LEVEL READ COMMITTED',
    'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');

  cCommit_A: RawByteString = sCommitMsg;
  cCommit_W: UnicodeString = sCommitMsg;
  cRollback_A: RawByteString = sRollbackMsg;
  cRollback_W: UnicodeString = sRollbackMsg;

implementation

uses ZMessages,{$IFNDEF TLIST_IS_DEPRECATED}ZSysUtils, {$ENDIF}
  ZEncoding, ZFastCode, ZSelectSchema,
  ZDbcMetadata, ZDbcUtils, ZDbcProperties,
  StrUtils, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

const
  TZDefaultProviderSequenceClasses: array[TZServerProvider] of TZAbstractSequenceClass = (
    {spUnknown}   nil,
    {spMSSQL}     TZMSSQLSequence,
    {spMSJet}     nil,
    {spOracle}    TZOracleSequence,
    {spASE}       nil,
    {spASA}       TZDotCurrvalNextvalSequence,
    {spPostgreSQL}TZPostgreSQLSequence,
    {spIB_FB}     TZFirebird2UpSequence,
    {spMySQL}     nil,
    {spNexusDB}   nil,
    {spSQLite}    nil,
    {spDB2}       TZDB2Sequence,
    {spAS400}     nil,
    {spInformix}  TZInformixSequence,
    {spCUBRID}    TZCubridSequence,
    {spFoxPro}    nil
    );

{ TZAbstractDriver }

constructor TZAbstractDriver.Create;
begin
  FCachedPlainDrivers := TZHashMap.Create;
end;

destructor TZAbstractDriver.Destroy;
begin
  FCachedPlainDrivers.Clear;
  FCachedPlainDrivers := nil;
  inherited Destroy;
end;

function TZAbstractDriver.GetSupportedProtocols: TStringDynArray;
begin
  Result := FSupportedProtocols;
end;

function TZAbstractDriver.GetClientCodePages(const Url: TZURL): TStringDynArray;
var
  Plain: IZPlainDriver;
begin
  Plain := GetPlainDriverFromCache(Url.Protocol, '');
  if Assigned(Plain) then
  Result := Plain.GetClientCodePages;
end;

{$WARN SYMBOL_DEPRECATED OFF}
function TZAbstractDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempURL:  TZURL;
begin
  TempURL := TZURL.Create(Url, Info);
  try
    Result := Connect(TempURL);
  finally
    TempUrl.Free;
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}

function TZAbstractDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := nil;
end;

function TZAbstractDriver.AcceptsURL(const Url: string): Boolean;
var
  I: Integer;
  Protocols: TStringDynArray;
begin
  Result := False;
  Protocols := GetSupportedProtocols;
  for I := Low(Protocols) to High(Protocols) do
  begin
    Result := StartsWith(LowerCase(Url), Format('zdbc:%s:', [LowerCase(Protocols[I])]));
    if Result then
      Break;
  end;
end;

procedure TZAbstractDriver.AddSupportedProtocol(const AProtocol: String);
begin
  SetLength(FSupportedProtocols, Length(FSupportedProtocols)+1);
  FSupportedProtocols[High(FSupportedProtocols)] := AProtocol;
end;

function TZAbstractDriver.AddPlainDriverToCache(const PlainDriver: IZPlainDriver;
  const Protocol: string = ''; const LibLocation: string = ''): String;
var
  TempKey: IZAnyValue;
begin
  if Protocol = '' then begin
    Result := PlainDriver.GetProtocol;
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(PlainDriver.GetProtocol))
  end else begin
    Result := Protocol;
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol+LibLocation));
  end;
  FCachedPlainDrivers.Put(TempKey, PlainDriver);
end;

function TZAbstractDriver.GetPlainDriverFromCache(const Protocol, LibLocation: string): IZPlainDriver;
var
  TempKey: IZAnyValue;
  TempPlain: IZPlainDriver;
begin
  TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol+LibLocation));
  Result := FCachedPlainDrivers.Get(TempKey) as IZPlainDriver;
  if Result = nil then begin
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol));
    TempPlain := FCachedPlainDrivers.Get(TempKey) as IZPlainDriver;
    if Assigned(TempPlain) then begin
      Result := TempPlain.Clone;
      AddPlainDriverToCache(Result, Protocol, LibLocation);
    end;
  end;
end;

function TZAbstractDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean): IZPlainDriver;
begin
  Result := GetPlainDriverFromCache(Url.Protocol, Url.LibLocation);
  if Assigned(Result) and InitDriver then begin
    GlobalCriticalSection.Enter;
    try
      Result.Initialize(Url.LibLocation);
    finally
      GlobalCriticalSection.Leave;
    end;
  end;
end;

function TZAbstractDriver.GetPropertyInfo(const Url: string; Info: TStrings): TStrings;
begin
  Result := nil;
end;

function TZAbstractDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

function TZAbstractDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

function TZAbstractDriver.GetSubVersion: Integer;
begin
  Result := 0;
end;

function TZAbstractDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZGenericStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

function TZAbstractDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZGenericSQLTokenizer.Create;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "Event" not used}
{$ENDIF}
function TZAbstractDriver.GetClientVersion(const Url: string): Integer;
begin
  Result := 0;
end;

{ TZAbstractDbcConnection }

function TZAbstractDbcConnection.GetHostName: string;
begin
  Result := FURL.HostName;
end;

procedure TZAbstractDbcConnection.SetHostName(const Value: String);
begin
  FURL.HostName := Value;
end;

function TZAbstractDbcConnection.GetPort: Integer;
begin
  Result := FURL.Port;
end;

function TZAbstractDbcConnection.GetServerProvider: TZServerProvider;
begin
  Result := spUnknown;
end;

procedure TZAbstractDbcConnection.SetConnPort(const Value: Integer);
begin
  FURL.Port := Value;
end;

function TZAbstractDbcConnection.GetDatabase: string;
begin
  Result := FURL.Database;
end;

procedure TZAbstractDbcConnection.SetDatabase(const Value: String);
begin
  FURL.Database := Value;
end;

function TZAbstractDbcConnection.GetUser: string;
begin
  Result := FURL.UserName;
end;

procedure TZAbstractDbcConnection.SetUser(const Value: String);
begin
  FURL.UserName := Value;
end;

function TZAbstractDbcConnection.SupportsEventListener: Boolean;
begin
  Result := FWeakEventListenerSelfPtr <> nil;
end;

function TZAbstractDbcConnection.GetPassword: string;
begin
  Result := FURL.Password;
end;

procedure TZAbstractDbcConnection.SetPassword(const Value: String);
begin
  FURL.Password := Value;
end;

function TZAbstractDbcConnection.GetInfo: TStrings;
begin
  Result := FURL.Properties;
end;

procedure TZAbstractDbcConnection.SetDateTimeFormatProperties(DetermineFromInfo: Boolean);

  procedure SetNotEmptyFormat(const FmtFromValues, FmtDefault: string; out ResultFmt: string);
  begin
    if FmtFromValues = '' then
      ResultFmt := FmtDefault
    else
      ResultFmt := UpperCase(FmtFromValues);
  end;

begin
  if DetermineFromInfo then begin
    {date formats}
    SetNotEmptyFormat(Info.Values[ConnProps_DateWriteFormat],
      DefDateFormatYMD,
      ConSettings^.WriteFormatSettings.DateFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateReadFormat],
      DefDateFormatYMD,
      ConSettings^.ReadFormatSettings.DateFormat);
    {time formats}
    SetNotEmptyFormat(Info.Values[ConnProps_TimeWriteFormat],
      IfThen(GetMetaData.GetDatabaseInfo.SupportsMilliseconds, DefTimeFormatMsecs, DefTimeFormat),
      ConSettings^.WriteFormatSettings.TimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_TimeReadFormat],
      IfThen(GetMetaData.GetDatabaseInfo.SupportsMilliseconds, DefTimeFormatMsecs, DefTimeFormat),
      ConSettings^.ReadFormatSettings.TimeFormat);

    {timestamp formats}
    SetNotEmptyFormat(Info.Values[ConnProps_DateTimeWriteFormat],
      ConSettings^.WriteFormatSettings.DateFormat+' '+ConSettings^.WriteFormatSettings.TimeFormat,
      ConSettings^.WriteFormatSettings.DateTimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateTimeReadFormat],
      ConSettings^.ReadFormatSettings.DateFormat+' '+ConSettings^.ReadFormatSettings.TimeFormat,
      ConSettings^.ReadFormatSettings.DateTimeFormat);

  end;

  ConSettings^.WriteFormatSettings.DateFormatLen := Length(ConSettings^.WriteFormatSettings.DateFormat);
  ConSettings^.ReadFormatSettings.DateFormatLen := Length(ConSettings^.ReadFormatSettings.DateFormat);

  ConSettings^.WriteFormatSettings.TimeFormatLen := Length(ConSettings^.WriteFormatSettings.TimeFormat);
  ConSettings^.ReadFormatSettings.TimeFormatLen := Length(ConSettings^.ReadFormatSettings.TimeFormat);

  ConSettings^.WriteFormatSettings.DateTimeFormatLen := Length(ConSettings^.WriteFormatSettings.DateTimeFormat);
  ConSettings^.ReadFormatSettings.DateTimeFormatLen := Length(ConSettings^.ReadFormatSettings.DateTimeFormat);
end;

procedure TZAbstractDbcConnection.SetOnConnectionLostErrorHandler(
  Handler: TOnConnectionLostError);
var
  OldAssigned: Boolean;
  NewAssigned: Boolean;
begin
  OldAssigned := Assigned(FOnConnectionLostError);
  NewAssigned := Assigned(Handler);

  if Not NewAssigned then // if the new handler is unassigned we always assign it.
    FOnConnectionLostError := Handler
  else if OldAssigned then // new handler and old handler are assigined
    raise EZSQLException.Create('Connection lost eError handler registered already!')
  else //new handler is assigned and old handler is not assigned.
    FOnConnectionLostError := Handler;
end;

procedure TZAbstractDbcConnection.RegisterStatement(
  const Value: IZStatement);
begin
  if fRegisteredStatements.IndexOf(Pointer(Value)) = -1 then
    fRegisteredStatements.Add(Pointer(Value))
end;

procedure TZAbstractDbcConnection.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var I: Integer;
  ImmediatelyReleasable: IImmediatelyReleasable;
  FError: EZSQLConnectionLost;
begin
  FAutoCommit := True;
  if not Closed and Assigned(FOnConnectionLostError) and Assigned(AError) then begin
    FError := AError;
    AError := nil;
  end else
    FError := nil; //satisfy compiler
  fClosed := True;
  for I := fRegisteredStatements.Count-1 downto 0 do
    If Supports(IZStatement(fRegisteredStatements[I]), IImmediatelyReleasable, ImmediatelyReleasable)
      and (Sender <> ImmediatelyReleasable) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
  if FDisposeCodePage then
  begin
    Dispose(ConSettings^.ClientCodePage);
    ConSettings^.ClientCodePage := nil;
    FDisposeCodePage := False;
  end;
  if Assigned(FOnConnectionLostError) and (FError <> nil) then
    FOnConnectionLostError(FError);
end;

procedure TZAbstractDbcConnection.ResetCurrentClientCodePage(const Name: String;
  IsStringFieldCPConsistent: Boolean);
var NewCP, tmp: PZCodePage;
begin
  FDisposeCodePage := True;
  Tmp := ConSettings^.ClientCodePage;
  ConSettings^.ClientCodePage := New(PZCodePage);
  NewCP := GetIZPlainDriver.ValidateCharEncoding(Name);
  ConSettings^.ClientCodePage^.Name := Tmp^.Name;
  ConSettings^.ClientCodePage^.ID := NewCP^.ID;
  ConSettings^.ClientCodePage^.CharWidth := NewCP^.CharWidth;
  ConSettings^.ClientCodePage^.Encoding := NewCP^.Encoding;
  ConSettings^.ClientCodePage^.CP := NewCP^.CP;
  ConSettings^.ClientCodePage^.ZAlias := '';
  ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := IsStringFieldCPConsistent;
  {Also reset the MetaData ConSettings}
  (FMetadata as TZAbstractDatabaseMetadata).ConSettings := ConSettings;
  FClientVarManager := TZClientVariantManager.Create(ConSettings);
end;

function TZAbstractDbcConnection.GetEncoding: TZCharEncoding;
begin
  Result := ConSettings.ClientCodePage^.Encoding;
end;

function TZAbstractDbcConnection.GetClientVariantManager: IZClientVariantManager;
begin
  Result := TZClientVariantManager.Create(ConSettings);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZAbstractDbcConnection.AbortOperation: Integer;
begin
//  Would this work...?
//  for i := fRegisteredStatements.Count-1 downto 0 do
//   IZStatement(fRegisteredStatements[i]).Cancel;
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractDbcConnection.AfterConstruction;
var iCon: IZConnection;
    iListener: IZEventListener;
begin
  if QueryInterface(IZConnection, ICon) = S_OK then begin
    fWeakReferenceOfSelfInterface := Pointer(iCon);
    iCon := nil;
  end;
  if QueryInterface(IZEventListener, iListener) = S_OK then begin
    FWeakEventListenerSelfPtr := Pointer(iListener);
    iListener := nil;
  end;
  inherited AfterConstruction;
  FURL.OnPropertiesChange := OnPropertiesChange;
end;

procedure TZAbstractDbcConnection.CheckCharEncoding(const CharSet: String;
  const DoArrange: Boolean = False);
begin
  ConSettings.ClientCodePage := GetIZPlainDriver.ValidateCharEncoding(CharSet, DoArrange);
  FClientCodePage := ConSettings.ClientCodePage^.Name; //resets the developer choosen ClientCodePage
  FClientVarManager := TZClientVariantManager.Create(ConSettings);
  if (ConSettings.ClientCodePage.Encoding = ceUTF16) and
    (Info.Values[ConnProps_RawStringEncoding] = '')
    {$IF declared(ConnProps_ControlsCP)}and (Info.Values[ConnProps_ControlsCP] = ''){$IFEND}
    then ConSettings.W2A2WEncodingSource := encUTF8;

end;

{$WARN SYMBOL_DEPRECATED OFF}
constructor TZAbstractDbcConnection.Create(const Driver: IZDriver; const Url: string;
  const PlainDriver: IZPlainDriver;
  const HostName: string; Port: Integer; const Database: string;
  const User: string; const Password: string; Info: TStrings);
var
  TempURL: TZURL;
begin
  TempURL := TZURL.Create(Url, HostName, Port, Database, User, Password, Info);
  Create(TempURL);
  TempURL.Free;
end;
{$WARN SYMBOL_DEPRECATED ON}

constructor TZAbstractDbcConnection.Create(const ZUrl: TZURL);
begin
  FClosed := True;
  FDisposeCodePage := False;
  if not assigned(ZUrl) then
    raise EZSQLException.Create('ZUrl is not assigned!')
  else
    FURL := TZURL.Create(ZURL);
  FDriverManager := ZDbcIntfs.DriverManager; //just keep refcount high
  FDriver := FDriverManager.GetDriver(ZURL.URL);
  FIZPlainDriver := FDriver.GetPlainDriver(ZUrl);
  FAutoCommit := True;
  FReadOnly := False; //EH: Changed! We definitelly did newer ever open a ReadOnly connection by default!
  FTransactIsolationLevel := tiNone;
  FUseMetadata := True;
  fAddLogMsgToExceptionOrWarningMsg := True;
  fRaiseWarnings := False;
  fRegisteredStatements := TZSortedList.Create;
  {$IFDEF ZEOS_TEST_ONLY}
  FTestMode := 0;
  {$ENDIF}
  FClientCodePage := Info.Values[ConnProps_CodePage];
  ConSettings := New(PZConSettings);
  CheckCharEncoding(FClientCodePage, True);
  SetConSettingsFromInfo(Info);
end;

destructor TZAbstractDbcConnection.Destroy;
begin
  if not FClosed then
    Close;
  FreeAndNil(FMetadata);
  FreeAndNil(FURL);
  FreeAndNil(fRegisteredStatements);
  FIZPlainDriver := nil;
  FDriver := nil;
  if Assigned(ConSettings) then begin
    Dispose(ConSettings);
    ConSettings := nil;
  end;
  FClientVarManager := nil;
  inherited Destroy;
end;

procedure TZAbstractDbcConnection.Open;
begin
  FClosed := False;
  SetDateTimeFormatProperties;
end;

function TZAbstractDbcConnection.CreateStatement: IZStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).CreateStatementWithParams(nil);
end;

function TZAbstractDbcConnection.PrepareStatement(const SQL: string): IZPreparedStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).PrepareStatementWithParams(SQL, nil);
end;

procedure TZAbstractDbcConnection.PrepareTransaction(const transactionid: string);
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

function TZAbstractDbcConnection.PrepareCall(
  const Name: string): IZCallableStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).PrepareCallWithParams(Name, nil);
end;

function TZAbstractDbcConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  if TZDefaultProviderSequenceClasses[GetServerProvider] <> nil then
    Result := TZDefaultProviderSequenceClasses[GetServerProvider].Create(
      IZConnection(fWeakReferenceOfSelfInterface), Sequence, BlockSize)
  else
    Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

function TZAbstractDbcConnection.NativeSQL(const SQL: string): string;
begin
  Result := SQL;
end;

procedure TZAbstractDbcConnection.SetAddLogMsgToExceptionOrWarningMsg(
  Value: Boolean);
begin
  fAddLogMsgToExceptionOrWarningMsg := Value;
end;

procedure TZAbstractDbcConnection.SetAutoCommit(Value: Boolean);
begin
  FAutoCommit := Value;
end;

function TZAbstractDbcConnection.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

procedure TZAbstractDbcConnection.CommitPrepared(const transactionid: string);
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

procedure TZAbstractDbcConnection.RollbackPrepared(const transactionid: string);
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZAbstractDbcConnection.PingServer: Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractDbcConnection.EscapeString(const Value : RawByteString) : RawByteString;
begin
  Result := EncodeCString(Value);
end;

procedure TZAbstractDbcConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var CP: Word;
begin
  if ConSettings.ClientCodePage.Encoding = ceUTF16
  then CP := zCP_UTF8
  else CP := ConSettings.ClientCodePage.CP;
  ExecuteImmediat(ZRawToUnicode(SQL, CP), LoggingCategory);
end;

procedure TZAbstractDbcConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
begin
  ExecuteImmediat(ZUnicodeToRaw(SQL, ConSettings.ClientCodePage.CP), LoggingCategory);
end;

procedure TZAbstractDbcConnection.Close;
var RefCountAdded: Boolean;
begin
  //while killing pending statements which keep the Connection.RefCount greater than 0
  //we need to take care about calling Destroy which calls Close again.
  DriverManager.ClearGarbageCollector;
  if RefCount > 0 then begin //manual close called
    RefCountAdded := True;
    _AddRef;
  end else
    RefCountAdded := False; //destructor did call close;
  try
    try
      ClearWarnings;
      CloseRegisteredStatements;
    finally
      InternalClose;
    end;
  finally
    FClosed := True;
    if FDisposeCodePage then
    begin
      Dispose(ConSettings^.ClientCodePage);
      ConSettings^.ClientCodePage := nil;
      FDisposeCodePage := False;
    end;
    if RefCountAdded then
      _Release; //destructor will call close again
  end;
end;

procedure TZAbstractDbcConnection.CloseEventListener(var Value: IZEventListener);
var con: IZConnection;
begin
  if (FCreatedWeakEventListenerPtr <> nil) and IZEventListener(FCreatedWeakEventListenerPtr).IsListening then
    IZEventListener(FCreatedWeakEventListenerPtr).UnListen;

  if FCreatedWeakEventListenerPtr <> Pointer(Value) then begin
    Con := Value.GetConnection;
    Con.CloseEventListener(Value);
    Con := nil;
  end else
    FCreatedWeakEventListenerPtr := nil;
  Value := nil;
end;

procedure TZAbstractDbcConnection.CloseRegisteredStatements;
var I: Integer;
begin
  for i := fRegisteredStatements.Count-1 downto 0 do begin
    //try
      IZStatement(fRegisteredStatements[i]).Close;
    //except end;
  end;
end;

function TZAbstractDbcConnection.IsClosed: Boolean;
begin
  Result := FClosed;
  DriverManager.ClearGarbageCollector;
end;

function TZAbstractDbcConnection.GetDriver: IZDriver;
begin
  Result := FDriver;
end;

function TZAbstractDbcConnection.GetIZPlainDriver: IZPlainDriver;
begin
  result := FIZPlainDriver;
end;

function TZAbstractDbcConnection.GetMetadata: IZDatabaseMetadata;
begin
  if Closed then
    Open;
  Result := FMetadata as IZDatabaseMetadata;
end;

function TZAbstractDbcConnection.GetParameters: TStrings;
begin
  Result := Info;
end;

function TZAbstractDbcConnection.GetClientVersion: Integer;
begin
  Result := 0;
end;

function TZAbstractDbcConnection.GetHostVersion: Integer;
begin
  Result := 0;
end;

function TZAbstractDbcConnection.GetDescription: String;
begin
  Result := PlainDriver.GetDescription;
end;

procedure TZAbstractDbcConnection.SetRaiseWarnings(Value: Boolean);
begin
  fRaiseWarnings := Value;
end;

procedure TZAbstractDbcConnection.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

function TZAbstractDbcConnection.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TZAbstractDbcConnection.LogError(const Category: TZLoggingCategory;
  ErrorCode: Integer; const Sender: IImmediatelyReleasable; const Msg,
  Error: String);
var Stmt: IZStatement;
    AMessage: String;
begin
  if (Sender <> nil) and (Sender.QueryInterface(IZStatement, Stmt) = S_OK) then begin
    AMessage := 'Statement '+{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Stmt.GetStatementId);
    if Msg <> '' then
      AMessage := AMessage+' : ';
  end else AMessage := '';
  AMessage := AMessage + Msg;
  DriverManager.LogError(Category, URL.Protocol, AMessage, ErrorCode, Error);
end;

function TZAbstractDbcConnection.GetURL: String;
begin
  Result := FURL.URL
end;

procedure TZAbstractDbcConnection.SetCatalog(const Catalog: string);
begin
end;

function TZAbstractDbcConnection.GetCatalog: string;
begin
  Result := '';
end;

procedure TZAbstractDbcConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  FTransactIsolationLevel := Level;
end;

function TZAbstractDbcConnection.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := FTransactIsolationLevel;
end;

function TZAbstractDbcConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

procedure TZAbstractDbcConnection.ClearWarnings;
begin
end;

procedure TZAbstractDbcConnection.DeregisterStatement(
  const Value: IZStatement);
var
  I: Integer;
begin
  I := fRegisteredStatements.IndexOf(Pointer(Value));
  if I > -1 then fRegisteredStatements.Delete(I);
end;

function TZAbstractDbcConnection.UseMetadata: boolean;
begin
  result := FUseMetadata;
end;

procedure TZAbstractDbcConnection.SetUseMetadata(Value: Boolean);
begin
  FUseMetadata := Value;
end;

function TZAbstractDbcConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  Result := GetSQLHexString(Pointer(Value), Length(Value), GetServerProvider in [spMSSQL, spASE, spASA, spDB2]);
end;

function TZAbstractDbcConnection.GetByteBufferAddress: PByteBuffer;
begin
  Result := @FByteBuffer[0];
end;

function TZAbstractDbcConnection.GetEscapeString(const Value: UnicodeString): UnicodeString;
var P: PWideChar;
    L: LengthInt;
begin
  P := Pointer(Value);
  L := Length(Value);
  if (P <> nil) and ((PWord(P)^=Word(#39)) and (PWord(P+L-1)^=Word(#39)))
  then Result := Value
  else Result := SQLQuotedStr(P, L, WideChar(#39));
end;

function TZAbstractDbcConnection.GetEscapeString(const Value: RawByteString): RawByteString;
var P: PAnsiChar;
    L: LengthInt;
begin
  P := Pointer(Value);
  L := Length(Value);
  if (P <> nil) and ((PByte(P)^=Byte(#39)) and (PByte(P+L-1)^=Byte(#39)))
  then Result := Value
  else Result := SQLQuotedStr(P, L, AnsiChar(#39));
end;

function TZAbstractDbcConnection.GetEventListener(Handler: TZOnEventHandler;
  CloneConnection: Boolean; Options: TStrings): IZEventListener;
var Con: IZConnection;
begin
  Result := nil;
  if (FWeakEventListenerSelfPtr = nil) then
    raise EZSQLException.Create('Listener is not supported');
  if (FCreatedWeakEventListenerPtr <> nil) and not CloneConnection then
    raise EZSQLException.Create('Listener alredy retrieved');
  if CloneConnection then begin
    Con := FDriverManager.GetConnection(FURL.URL);
    Result := Con.GetEventListener(Handler, False, Options);
    Con := nil;
  end else begin
    Result := IZEventListener(FWeakEventListenerSelfPtr);
    FCreatedWeakEventListenerPtr := Pointer(Result);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Sender" not used} {$ENDIF}
procedure TZAbstractDbcConnection.OnPropertiesChange(Sender: TObject);
begin
  // do nothing in base class
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZAbstractSequence }

constructor TZAbstractSequence.Create(const Connection: IZConnection;
  const Name: string; BlockSize: Integer);
begin
  FConnection := Connection;
  FName := Name;
  FBlockSize := BlockSize;
end;

function TZAbstractSequence.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

function TZAbstractSequence.GetName: string;
begin
  Result := FName;
end;

procedure TZAbstractSequence.FlushResults;
begin
  if FNextValRS <> nil then begin
    FNextValRS.Close;
    FNextValRS := nil;
  end;
  if FNextValStmt <> nil then begin
    FNextValStmt.Close;
    FNextValStmt := nil;
  end;
  if FCurrValRS <> nil then begin
    FCurrValRS.Close;
    FCurrValRS := nil;
  end;
  if FCurrValStmt <> nil then begin
    FCurrValStmt.Close;
    FCurrValStmt := nil;
  end;
end;

function TZAbstractSequence.GetBlockSize: Integer;
begin
  Result := FBlockSize;
end;

function TZAbstractSequence.GetCurrentValue: Int64;
begin
  if (FCurrValStmt = nil) or FCurrValStmt.IsClosed then
    FCurrValStmt := FConnection.PrepareStatement(GetCurrentValueSQL);
  FCurrValRS := FCurrValStmt.ExecuteQueryPrepared;
  if (FCurrValRS = nil) or (not FCurrValRS.Next) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  Result := FCurrValRS.GetLong(FirstDbcIndex);
  FCurrValRS.ResetCursor;
end;

function TZAbstractSequence.GetNextValue: Int64;
begin
  if (FNextValStmt = nil) or FNextValStmt.IsClosed then
    FNextValStmt := FConnection.PrepareStatement(GetNextValueSQL);
  FNextValRS := FNextValStmt.ExecuteQueryPrepared;
  if (FNextValRS = nil) or (not FNextValRS.Next) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  Result := FNextValRS.GetLong(FirstDbcIndex);
  FNextValRS.ResetCursor;
end;

procedure TZAbstractSequence.SetBlockSize(const Value: Integer);
begin
  FBlockSize := Value;
end;

procedure TZAbstractSequence.SetName(const Value: string);
begin
  if FName <> Value then begin
    FlushResults;
    FName := Value;
  end;
end;

{ TZIdentifierSequence }

procedure TZIdentifierSequence.SetName(const Value: string);
var QuotedName: String;
begin
  QuotedName := FConnection.GetMetadata.GetIdentifierConverter.Quote(Value, iqSequence);
  inherited SetName(QuotedName);
end;

{ TZMSSQLSequence }

function TZMSSQLSequence.GetCurrentValueSQL: string;
begin
  Result := 'select next value for '+FConnection.GetMetadata.GetIdentifierConverter.Quote(FName, iqSequence);
end;

function TZMSSQLSequence.GetNextValueSQL: string;
begin
  Result := 'SELECT current_value FROM sys.sequences WHERE name = '+FName;
end;

{ TZDotCurrvalNextvalSequence }

function TZDotCurrvalNextvalSequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT '+FName+'.CURRVAL';
end;

function TZDotCurrvalNextvalSequence.GetNextValueSQL: string;
begin
  Result := 'SELECT '+FName+'.NEXTVAL'
end;

{ TZOracleSequence }

function TZOracleSequence.GetCurrentValueSQL: string;
begin
  Result := inherited GetCurrentValueSQL+' FROM DUAL';
end;

function TZOracleSequence.GetNextValueSQL: string;
begin
  Result := inherited GetNextValueSQL+' FROM DUAL';
end;

{ TZFirebird2UpSequence }

function TZFirebird2UpSequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT GEN_ID('+FName+', 0) FROM RDB$DATABASE';
end;

function TZFirebird2UpSequence.GetNextValueSQL: string;
begin
  Result := 'SELECT NEXT VALUE FOR '+FName+' FROM RDB$DATABASE';
end;

{ TZPostgreSQLSequence }

function TZPostgreSQLSequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT CURRVAL('''+FName+''')';
end;

function TZPostgreSQLSequence.GetNextValueSQL: string;
begin
  Result := 'SELECT NEXTVAL('''+FName+''')';
end;

{ TZClientVariantManager }

constructor TZClientVariantManager.Create(const ConSettings: PZConSettings);
begin
  inherited Create; //Set all standard converters functions
  FConSettings := ConSettings;
  FFormatSettings := FConSettings.ReadFormatSettings;
  FClientCP := Consettings.ClientCodePage.CP;
  FStringCP := GetW2A2WConversionCodePage(ConSettings);
  FUseWComparsions := (FClientCP <> ZOSCodePage) or
    (FClientCP = zCP_UTF8) or
    not Consettings.ClientCodePage.IsStringFieldCPConsistent or
    (Consettings.ClientCodePage.Encoding = ceUTF16);
end;

function TZClientVariantManager.GetAsDateTime(
  const Value: TZVariant): TDateTime;
var P: Pointer;
  L: LengthInt;
label DateTimeFromRaw, DateTimeFromUnicode, Fail;
begin
  case Value.VType of
    {$IFNDEF UNICODE}vtString,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
    {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
    vtRawByteString: begin
        P := Pointer(Value.VRawByteString);
        L := Length(Value.VRawByteString);
DateTimeFromRaw:
        if not ZSysUtils.TryPCharToDateTime(PAnsiChar(P), L, FConSettings^.ReadFormatSettings, Result{%H-}) then
          goto Fail;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      begin
        P := Pointer(Value.VUnicodeString);
        L := Length(Value.VUnicodeString);
DateTimeFromUnicode:
        if not ZSysUtils.TryPCharToDateTime(PWideChar(P), L, FConSettings^.ReadFormatSettings, Result) then
          goto Fail;
      end;
    vtCharRec: begin
        P := Value.VCharRec.P;
        L := Value.VCharRec.Len;
        if (Value.VCharRec.CP = zCP_UTF16)
        then goto DateTimeFromUnicode
        else goto DateTimeFromRaw;
      end;
    else
Fail: Result := inherited GetAsDateTime(Value);
  end;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZClientVariantManager.ProcessAnsiString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  ResTmp := '';
  Result.VType := vtAnsiString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FStringCP = ZOSCodePage
              then ResTmp := Value.VRawByteString
              else PRawToRawConvert(Pointer(Value.VRawByteString), Length(Value.VRawByteString), FStringCP, ZOSCodePage, ResTmp);
    {$ENDIF}
    vtAnsiString: ResTmp := Value.VRawByteString;
    vtUTF8String: if ZOSCodePage = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, ZOSCodePage);
    vtRawByteString: if ZOSCodePage = FClientCP
                     then ResTmp := Value.VRawByteString
                     else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, ZOSCodePage);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, ZOSCodePage);
    vtCharRec:
      if (Value.VCharRec.CP = zCP_UTF16) then
        PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage, ResTmp)
      else if (ZOSCodePage = Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, ZOSCodePage);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, ZOSCodePage{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_ANSISTRING}

procedure TZClientVariantManager.ProcessCharRec(const Value: TZVariant;
  out Result: TZVariant);
label SetRaw;
begin
  Result.VType := vtCharRec;
  case Value.VType of
    vtNull:
      begin
        Result.VCharRec.Len := 0;
        Result.VCharRec.CP := High(Word);
        Result.VCharRec.P := nil;
      end;
    {$IFNDEF UNICODE}
    vtString: begin
        Result.VCharRec.CP := FStringCP;
        goto SetRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: begin
        Result.VCharRec.CP := ZOSCodePage;
        goto SetRaw;
      end;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: begin
        Result.VCharRec.CP := zCP_UTF8;
        goto SetRaw;
      end;
    {$ENDIF}
    vtRawByteString: begin
        Result.VCharRec.CP := FClientCP;
SetRaw: if Pointer(Result.VRawByteString) = nil then begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
        end else begin
          Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
          Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL conversion to PAnsiChar
        end;
      end;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      begin
        Result.VUnicodeString := Value.VUnicodeString;
        Result.VCharRec.CP := zCP_UTF16;
        Result.VCharRec.Len := Length(Result.VUnicodeString);
        if Result.VCharRec.Len = 0 then
          Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
        else
          Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PWideChar
      end;
    vtCharRec:
      Result.VCharRec := Value.VCharRec;
    else if (Ord(FConSettings^.ClientCodePage^.Encoding) < Ord(ceUTF16)) then begin
          Result.VRawByteString := Convert(Value, vtRawByteString).VRawByteString;
          Result.VCharRec.CP := FClientCP;
          if Pointer(Result.VRawByteString) = nil then begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
          end else begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL conversion to PAnsiChar
          end;
        end else begin
          Result.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
          Result.VCharRec.CP := zCP_UTF16;
          Result.VCharRec.Len := Length(Result.VUnicodeString);
          if Result.VCharRec.Len = 0
          then Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
          else Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PAnsiChar
        end;

  end;
end;

procedure TZClientVariantManager.ProcessRawByteString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtRawByteString;
  ResTmp := '';
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FStringCP = FClientCP
              then ResTmp := Value.VRawByteString
              else RawCPConvert(Value.VRawByteString, ResTmp, FStringCP, FClientCP);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: if FClientCP = ZOSCodePage
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, FClientCP);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: if FClientCP = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, FClientCP);
    {$ENDIF}
    vtRawByteString: ResTmp := Value.VRawByteString;
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: PUnicodeToRaw(Pointer(Value.VUnicodeString), Length(Value.VUnicodeString), FClientCP, ResTmp);
    vtCharRec:
      if (Value.VCharRec.CP = zCP_UTF16) then
        PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, FClientCP, ResTmp)
      else if (FClientCP = Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, FClientCP);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;

procedure TZClientVariantManager.ProcessString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: {$IFDEF UNICODE}UnicodeString{$ELSE}RawByteString{$ENDIF};
begin
  Result.VType := vtString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: Result.VRawByteString := Value.VRawByteString;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, ZOSCodePage);
                  {$ELSE}
                  if FStringCP = ZOSCodePage
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, FStringCP);
                  {$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, zCP_UTF8);
                  {$ELSE}
                  if FStringCP = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, zCP_UTF8, FStringCP);
                  {$ENDIF}
    {$ENDIF}
    vtRawByteString: {$IFDEF UNICODE}
                  ResTmp := ZRawToUnicode(Value.VRawByteString, FClientCP);
                  {$ELSE}
                  if (FStringCP = FClientCP)
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, FStringCP);
                  {$ENDIF}
   {$IFDEF UNICODE}vtString,{$ENDIF}
   vtUnicodeString:
      {$IFDEF UNICODE}
      ResTmp := Value.VUnicodeString;
      {$ELSE}
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, FStringCP);
      {$ENDIF}
    vtCharRec: if (Value.VCharRec.CP = zCP_UTF16) then
        {$IFDEF UNICODE}
        SetString(ResTmp, PChar(Value.VCharRec.P), Value.VCharRec.Len)
        {$ELSE}
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, FStringCP)
        {$ENDIF}
      else
        {$IFNDEF UNICODE}
        if (FStringCP = Value.VCharRec.CP)
        then ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_DEFAULTSYSTEMCODEPAGE},FStringCP{$ENDIF})
        else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, FStringCP);
        {$ELSE}
        ResTmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
        {$ENDIF}
    vtDateTime:
      ResTmp := ZSysUtils.{$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(Value.VDateTime, FFormatSettings, False);
    else {$IFDEF UNICODE}ConvertFixedTypesToUnicode{$ELSE}ConvertFixedTypesToRaw{$ENDIF}(Value, ResTmp{$IF defined(WITH_RAWBYTESTRING) and not defined(UNICODE)}, DefaultSystemCodePage{$IFEND});
  end;
  Result.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF} := ResTmp;
end;

procedure TZClientVariantManager.ProcessUnicodeString(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: UnicodeString;
begin
  ResTmp := '';
  Result.VType := vtUnicodeString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), FStringCP, ResTmp);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), ZOSCodePage, ResTmp);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), zCP_UTF8, ResTmp);
    {$ENDIF}
    vtRawByteString: PRawToUnicode(Pointer(Value.VRawByteString), Length(Value.VRawByteString), FClientCP, ResTmp);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := Value.VUnicodeString;
    vtDateTime:      ResTmp := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
        then SetString(ResTmp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
        else PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP, ResTmp);
    else ConvertFixedTypesToUnicode(Value, ResTmp);
  end;
  Result.VUnicodeString := ResTmp;
end;

{$IFNDEF NO_UTF8STRING}
procedure TZClientVariantManager.ProcessUTF8String(const Value: TZVariant;
  out Result: TZVariant);
var ResTmp: RawByteString;
begin
  Result.VType := vtUTF8String;
  ResTmp := '';
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString:     if FStringCP = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, FStringCP, zCP_UTF8);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: if ZOSCodePage = zCP_UTF8
                  then ResTmp := Value.VRawByteString
                  else RawCPConvert(Value.VRawByteString, ResTmp, ZOSCodePage, zCP_UTF8);
    {$ENDIF}
    vtUTF8String: ResTmp := Value.VRawByteString;
    vtRawByteString: if FClientCP = zCP_UTF8
                     then ResTmp := Value.VRawByteString
                     else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, zCP_UTF8);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      PUnicodeToRaw(Pointer(Value.VUnicodeString), Length(Value.VUnicodeString), zCP_UTF8, ResTmp);
    vtCharRec:
      if (Value.VCharRec.CP = zCP_UTF16) then
        PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, zCP_UTF8, ResTmp)
      else if (zCP_UTF8 = Value.VCharRec.CP) then
        ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF})
      else PRawCPConvert(Value.VCharRec.P, Value.VCharRec.Len, ResTmp, Value.VCharRec.CP, zCP_UTF8);
    vtDateTime: ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    else ConvertFixedTypesToRaw(Value, ResTmp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
  end;
  Result.VRawByteString := ResTmp;
end;
{$ENDIF NO_UTF8STRING}

function TZClientVariantManager.UseWComparsions: Boolean;
begin
  Result := FUseWComparsions;
end;

{ TZAbstractSingleTxnConnection }

procedure TZAbstractSingleTxnConnection.AfterConstruction;
var Trans: IZTransaction;
begin
  QueryInterface(IZTransaction, Trans);
  fWeakTxnPtr := Pointer(Trans);
  Trans := nil;
  inherited AfterConstruction;
  FSavePoints := TStringList.Create;
  fTransactions := TZCollection.Create;
end;

procedure TZAbstractSingleTxnConnection.ClearTransactions;
var I: Integer;
    Txn: IZTransaction;
begin
  for i := 0 to fTransactions.Count -1 do
    if (fTransactions[i] <> nil) and (fTransactions[i].QueryInterface(IZTransaction, Txn) = S_OK) then
      Txn.Close;
  fTransactions.Clear;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "AutoCommit" not used} {$ENDIF}
function TZAbstractSingleTxnConnection.CreateTransaction(AutoCommit,
  ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
var URL: TZURL;
  Connection: IZConnection;
begin
  URL := TZURL.Create;
  Result := nil;
  try
    URL.URL := FURL.URL;
    if Params <> nil then
      URL.Properties.AddStrings(Params);
    Connection := DriverManager.GetConnection(URL.URL);
    Connection.SetAutoCommit(True); //do not automatically start a explicit txn that should be done by StartTransaction
    Connection.SetReadOnly(ReadOnly);
    Connection.SetTransactionIsolation(TransactIsolationLevel);
    Result := Connection as IZTransaction; //if not supported we get an invalid interface exc.
    fTransactions.Add(Result)
  finally
    FreeAndNil(URL);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZAbstractSingleTxnConnection.Destroy;
begin
  try
    inherited Destroy;
  finally
    FreeAndNil(FSavePoints);
    fTransactions := nil;
  end;
end;

function TZAbstractSingleTxnConnection.GetConnection: IZConnection;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface);
end;

function TZAbstractSingleTxnConnection.GetConnectionTransaction: IZTransaction;
begin
  {$IFDEF ZEOSDEBUG}
  if fWeakTxnPtr = nil then
    raise EZUnsupportedException.Create(SUnsupportedOperation);
  {$ENDIF}
  Result := IZTransaction(fWeakTxnPtr);
end;

function TZAbstractSingleTxnConnection.GetTransactionLevel: Integer;
begin
  Result := Ord(not AutoCommit);
  if FSavePoints <> nil then
    Result := Result + FSavePoints.Count;
end;

function TZAbstractSingleTxnConnection.IsTransactionValid(
  const Value: IZTransaction): Boolean;
begin
  if Value = nil then
    Result := False
  else if Pointer(Value) = fWeakTxnPtr then
    Result := True
  else
    Result := fTransactions.IndexOf(Value) >= 0;
end;

procedure TZAbstractSingleTxnConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var I: Integer;
  Imm: IImmediatelyReleasable;
begin
  inherited ReleaseImmediat(Sender, AError);
  if (fActiveTransaction <> nil) and (fActiveTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and (imm <> Sender) then begin
    imm.ReleaseImmediat(Sender, AError);
    fActiveTransaction := nil;
  end;
  if fTransactions <> nil then begin
    for I := fTransactions.Count -1 downto 0 do
      if (fTransactions[i] <> nil) and (fTransactions[i].QueryInterface(IImmediatelyReleasable, imm) = S_OK) and (imm <> Sender) then
        imm.ReleaseImmediat(Sender, AError);
    fTransactions.Clear;
  end;
end;

procedure TZAbstractSingleTxnConnection.ReleaseTransaction(
  const Value: IZTransaction);
begin
  if Pointer(Value) = fWeakTxnPtr
  then raise EZUnsupportedException.Create(SUnsupportedOperation)
  else fTransactions.Delete(fTransactions.IndexOf(Value));
end;

{ TZEventList }

procedure TZEventList.Add(const Name: String; Handler: TZOnEventHandler);
var Index: NativeInt;
    Event: PZEvent;
begin
  if not Assigned(Handler) then
    Handler := FHandler;
  if (Name = '') or not Assigned(Handler) then
    raise EZSQLException.Create('Name or Handler not set');
  Event := GetByName(Name);
  if Event <> nil then
    raise EZSQLException.Create('Event registered already');
  Event := inherited Add(Index);
  Event.Name := Name;
  Event.Handler := Handler;
end;

constructor TZEventList.Create(Handler: TZOnEventHandler);
begin
  inherited Create(GetElementSize, True);
  FHandler := Handler;
end;

function TZEventList.Get(const Index: NativeInt): PZEvent;
begin
  Result := inherited Get(Index);
end;

function TZEventList.GetByName(const Name: String): PZEvent;
var I: NativeInt;
begin
  for i := 0 to Count -1 do begin
    Result := inherited Get(I);
    if Result.Name = Name then
      Exit;
  end;
  Result := nil;
end;

class function TZEventList.GetElementSize: Cardinal;
begin
  Result := SizeOf(TZEvent)
end;

procedure TZEventList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    PZEvent(Ptr).Name := '';
  inherited Notify(Ptr, Action);
end;

procedure TZEventList.Remove(const Name: String);
var I: NativeInt;
  Event: PZEvent;
begin
  for i := 0 to Count -1 do begin
    Event := inherited Get(I);
    if Event.Name = Name then begin
      Delete(I);
      Exit;
    end;
  end;
  raise EZSQLException.Create('Event is not registered');
end;

end.
