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
  /// <author>EgonHugeist</author>
  /// <summary>generic constant for first column/parameter index.</summary>
  /// <remarks>Since zeos 8.0up we use zero based index. Means the
  ///  <c>GENERIC_INDEX</c> will be removed in future releases.</remarks>
  FirstDbcIndex = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  /// <author>EgonHugeist</author>
  /// <summary>generic constant for an invalid column/parameter index.</summary>
  /// <remarks>Since zeos 8.0up we use zero based index. Means the
  ///  <c>GENERIC_INDEX</c> will be removed in future releases.</remarks>
  InvalidDbcIndex = {$IFDEF GENERIC_INDEX}-1{$ELSE}0{$ENDIF};
const
  { Constants from JDBC DatabaseMetadata }
  TypeSearchable            = 3;
  procedureResultUnknown    = 0;
  procedureNoResult         = 1;
  ProcedureReturnsResult    = 2;

// Exceptions
type
  /// <author>Fr0st</author>
  /// <summary>Defines an Abstract exception data object.</summary>
  TZExceptionSpecificData = class
  public
    function Clone: TZExceptionSpecificData; virtual; abstract;
  end;

  /// <summary>Defines an Abstract SQL exception.</summary>
  EZSQLThrowable = class({$IFDEF DO_NOT_DERIVE_FROM_EDATABASEERROR}Exception{$ELSE}EDatabaseError{$ENDIF})
  private
    FErrorCode: Integer;
    FStatusCode: String;
  protected
    FSpecificData: TZExceptionSpecificData;
  public
    /// <summary>Creates an exception with message string.</summary>
    /// <param>"Msg" a error description.</param>
    constructor Create(const Msg: string);
    /// <summary>Creates an exception with message string and an ErrorCode.</summary>
    /// <param>"Msg" a error description.</param>
    /// <param>"ErrorCode" a native server error code.</param>
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    /// <summary>Creates an exception with message string and a StatusCode.</summary>
    /// <param>"StatusCode" a server status code.</param>
    /// <param>"Msg" a error description.</param>
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    /// <summary>Creates an exception with and ErrorCode, StatusCode and a
    ///  message string.</summary>
    /// <param>"ErrorCode" a native server error code.</param>
    /// <param>"StatusCode" a server status code.</param>
    /// <param>"Msg" a error description.</param>
    constructor CreateWithCodeAndStatus(ErrorCode: Integer; const StatusCode: String; const Msg: string);
    /// <summary>Creates an exception cloned from an EZSQLThrowable.</summary>
    /// <param>"E" the source we clone from.</param>
    constructor CreateClone(const E:EZSQLThrowable);
    /// <summary>Destroys this object and releases all resources.</summary>
    destructor Destroy; override;
  public
    /// <summary>Specifies the ErrorCode value of the EZSQLThrowable.</summary>
    property ErrorCode: Integer read FErrorCode;
    /// <author>FirmOS</summary>
    /// <summary>Specifies the StatusCode value of the EZSQLThrowable.</summary>
    property StatusCode: string read FStatuscode;
    /// <author>Fr0sT</summary>
    /// <summary>Specifies the Provider SpecificData of the EZSQLThrowable.</summary>
    property SpecificData: TZExceptionSpecificData read FSpecificData; // Engine-specific data
  end;

  /// <author>EgonHugeist</summary>
  /// <summary>Specifies a class of EZSQLThrowable.</summary>
  EZSQLThrowableClass = class of EZSQLThrowable;

  /// <summary>Generic SQL exception.</summary>
  EZSQLException = class(EZSQLThrowable);

  /// <summary>Generic SQL warning.</summary>
  EZSQLWarning = class(EZSQLThrowable);

  /// <summary>Requested operation is not (yet) supported by Zeos.</summary>
  EZUnsupportedException = class(EZSQLException);

  /// <summary>Generic connection lost exception.</summary>
  EZSQLConnectionLost = class(EZSQLException);

  /// <summary>A on connection Lost event methode.</summary>
  TOnConnectionLostError = procedure(var AError: EZSQLConnectionLost) of Object;
  /// <summary>A on connect event.</summary>
  TOnConnect = procedure of Object;

  /// <author>EgonHugeist</author>
  /// <summary>Defines an enumerator for UTF16 to raw or vice verca encodings.
  ///  <c>encDB_CP</c> defines the raw string encoding is the characterset of
  ///  the database, <c>encUTF8</c> defines the raw string encoding as UTF8,
  ///  <c>encDefaultSystemCodePage</c> defines the raw string encoding is
  ///  the DefaultSystemCodePage.</summary>
  TZW2A2WEncodingSource = (encDB_CP,encUTF8, encDefaultSystemCodePage);
  /// <author>EgonHugeist</author>
  /// <summary>defines a reference to the connection settings record.</summary>
  PZConSettings = ^TZConSettings;
  /// <author>EgonHugeist</author>
  /// <summary>defines the connection settings record.</summary>
  TZConSettings = record
    /// <summary>Target/Source CP of raw string conversion.</summary>
    W2A2WEncodingSource: TZW2A2WEncodingSource; //
    /// <summary>A reference to the database characterset information.</summary>
    ClientCodePage: PZCodePage;
    /// <summary>The database ReadFormatSettings.</summary>
    ReadFormatSettings: TZFormatSettings;
    /// <summary>The database WriteFormatSettings.</summary>
    WriteFormatSettings: TZFormatSettings;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>defines an interfaced object containing the connection settings
  ///  reference.
  /// </summary>
  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    /// <summary>Fills the ConSettings record from a given parameter list.</summary>
    /// <param>"Info" the Parameter list.</param>
    procedure SetConSettingsFromInfo(Info: TStrings);
    /// <summary>Specifies the Connection settings reference.</summary>
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  public
    /// <summary>Get a reference to the actual connection settings.</summary>
    /// <returns>the TZConSettings record refrence.</returns>
    function GetConSettings: PZConSettings;
  end;

  {** a base class for most dbc-layer objects }

  { TZImmediatelyReleasableObject }

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract immediately releasable object.</summary>
  TZImmediatelyReleasableObject = Class(TZCodePagedObject)
  protected
    FWeakImmediatRelPtr: Pointer;
  public
    /// <summary>Responds after the last constructor has executed.
    ///  AfterConstruction is called automatically after the object's last
    ///  constructor has executed. Do not call it explicitly in your applications.
    ///  The AfterConstruction method implemented in TInterfacedObject
    ///  decrements the class constructors RefCount. So don't forget to call
    ///  the inherited Afterconstruction which is triggered as an OnCreate event.
    /// </summary>
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

type
  /// <summary>Defines ZDBC supported SQL types.</summary>
  TZSQLType = (stUnknown,
    //fixed size DataTypes first
    stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong, //ordinals
    stFloat, stDouble, {$IFDEF ZEOS90UP}stDecimal128,{$ENDIF} //floating types
    stCurrency, stBigDecimal, //ExactTypes
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
    {$IFDEF ZEOS90UP}
    stJSON, stXML, stVariant,
    {$ENDIF ZEOS90UP}
    //finally the object types
    stArray, stResultSet{$IFDEF ZEOS90UP}, stStatement{$ENDIF});

  /// <summary>Defines a dynamic array of TZSQLType(s).</summary>
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
    ///  possible transaction isolation levels. </summary>
    /// <remarks>This method cannot be called while a explicit transaction is
    ///  started.</remarks>
    /// <param>"Value" one of <c>tiNone, tiReadUncommitted, tiReadCommitted,
    ///  tiRepeatableRead, tiSerializable</c> isolation values with the
    ///  exception of <c>tiNone</c>; some databases may not support other
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
    ///  database optimizations.</summary>
    /// <remarks>This method cannot be called while a explicit transaction is
    ///  started.</remarks>
    /// <param>"Value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    /// <summary>Releases a transaction and resources immediately
    ///  instead of waiting for them to be automatically released. If the
    ///  transaction is underway a rollback will be done. Note: A
    ///  Transaction is automatically closed when the connection closes or it is
    ///  garbage collected. Certain fatal errors also result in a closed
    //// Transaction.</summary>
    procedure Close;
    /// <summary>Test if this <c>Transaction</c> object is closed.</summary>
    /// <returns><c>True</c> if the transaction is closed; <c>False</c>
    ///  otherwise.</returns>
    function IsClosed: Boolean;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a transaction manager interface.</summary>
  IZTransactionManager = interface(IImmediatelyReleasable)
    ['{BF61AD03-1072-473D-AF1F-67F90DFB4E6A}']
    /// <summary>Creates a <c>Transaction</c></summary>
    /// <param>"AutoCommit" the AutoCommit mode.</param>
    /// <param>"ReadOnly" the ReadOnly mode.</param>
    /// <param>"TransactIsolationLevel" the TransactIsolationLevel one of of
    ///  <c>tiNone, tiReadUncommitted, tiReadCommitted, tiRepeatableRead,
    ///  tiSerializable</c> isolation values with the exception of <c>tiNone</c>;
    ///  some databases may not support other values see
    ///  DatabaseInfo.SupportsTransactionIsolationLevel</param>
    /// <param>"Params" a list of properties used for the transaction.</param>
    /// <returns>returns the Transaction interface.</returns>
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
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a variant manager with connection related convertion
  ///  rules.</summary>
  IZClientVariantManager = Interface(IZVariantManager)
    ['{73A1A2C7-7C38-4620-B7FE-2426BF839BE5}']
    function UseWComparsions: Boolean;
  End;

  /// <summary>Defines the Database Connection interface.</summary>
  IZConnection = interface(IImmediatelyReleasable)
    ['{8EEBBD1A-56D1-4EC0-B3BD-42B60591457F}']
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a raw encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a UTF16 encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload;
    /// <summary>Sets a OnConnectionLost event.</summary>
    /// <param>"Handler" the event to be called on triggering the connection
    ///  lost.</param>
    /// <remarks> The handler must raise an exception to abort
    ///  the code serialization.</remarks>
    procedure SetOnConnectionLostErrorHandler(Handler: TOnConnectionLostError);
    /// <summary>Add the SQL to the EZSQLException?</summary>
    /// <param>"Value" if <c>True</c> the SQL will be added to the exception.</param>
    procedure SetAddLogMsgToExceptionOrWarningMsg(Value: Boolean);
    /// <summary>Should SQL-Warnings be raised?</summary>
    /// <param>"Value" if <c>True</c> the EZSQLWarning will be thrown.</param>
    procedure SetRaiseWarnings(Value: Boolean);
    /// <summary>Register an active statement on the connection.</summary>
    /// <param>"Value" the statement interface to be registered.</param>
    /// <remarks>This refrence is stored as a weak reference to avoid circular
    ///  refrences.</remarks>
    procedure RegisterStatement(const Value: IZStatement);
    /// <summary>Deregister a tatement from the connection if it's closing.</summary>
    /// <param>"Value" the statement interface to be deregistered.</param>
    procedure DeregisterStatement(const Statement: IZStatement);
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
    /// <returns> a new PreparedStatement object containing the
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
    /// <summary>Creates a <c>Statement</c> interface for sending SQL statements
    ///  to the database. SQL statements without parameters are normally
    ///  executed using Statement objects. If the same SQL statement
    ///  is executed many times, it is more efficient to use a
    ///  <c>PreparedStatement</c> object. Result sets created using the returned
    ///  <c>Statement</c> interface will by default have forward-only type and
    ///  read-only concurrency.</summary>
    /// <param>Info a statement parameters.</param>
    /// <returns>A new Statement interface</returns>
    function CreateStatementWithParams(Info: TStrings): IZStatement;
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
    /// <returns> a new PreparedStatement object containing the
    ///  optional pre-compiled statement</returns>
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
    ///  pre-compiled SQL statement </returns>
    function PrepareCallWithParams(const Name: string; Params: TStrings):
      IZCallableStatement;
    /// <summary>Creates an object to send/recieve notifications from SQL
    ///  server. An unsupported operation exception will be raised if the driver
    ///  doesn't support it, </summary>
    /// <param>"Event" an event name.</param>
    /// <returns>a created notification object.</returns>
    function CreateNotification(const Event: string): IZNotification;
    /// <summary>Creates a sequence generator object.</summary>
    /// <param>"Sequence" a name of the sequence generator.</param>
    /// <param>"BlockSize" a number of unique keys requested in one trip to SQL
    ///  server.</param>
    /// <returns>returns a created sequence object.</returns>
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;
    /// <summary>Converts the given SQL statement into the system's native SQL
    ///  grammar. A driver may convert the ZDBC sql grammar into its system's
    ///  native SQL grammar prior to sending it; this method returns the
    /// native form of the statement that the driver would have sent.</summary>
    /// <param>"SQL" a SQL statement that may contain one or more '?'/
    /// questionmark parameter placeholders</param>
    /// <returns>the native form of this statement</returns>
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
    /// <author>EgonHugeist</author>
    /// <summary>Starts transaction support or saves the current transaction.
    ///  If the connection is closed, the connection will be opened.
    ///  If a transaction is underway a nested transaction or a savepoint will
    ///  be spawned. While the tranaction(s) is/are underway the AutoCommit
    ///  property is set to False. Ending up the transaction with a
    ///  commit/rollback the autocommit property will be restored if changing
    ///  the autocommit mode was triggered by a starttransaction call.</summary>
    /// <returns>The current txn-level. 1 means a explicit transaction
    ///  was started. 2 means the transaction was saved. 3 means the previous
    ///  savepoint got saved too and so on.</returns>
    function StartTransaction: Integer;
    /// <author>EgonHugeist</author>
    /// <summary>Get the active transaction interces of the connection.</summary>
    /// <returns>The transaction object</returns>
    function GetConnectionTransaction: IZTransaction;
    /// <author>firmos</author>
    /// <summary>Prepares a two phase transaction.</summary>
    procedure PrepareTransaction(const transactionid: string);
    /// <author>firmos</author>
    /// <summary>commits the two phase transaction.</summary>
    procedure CommitPrepared(const transactionid: string);
    /// <author>firmos</author>
    /// <summary>Rolls back the two phase transaction.</summary>
    procedure RollbackPrepared(const transactionid: string);
    /// <author>firmos</author>
    /// <summary>Pings the server.</summary>
    /// <returns>0 if the connection is OK; non zero otherwise.</returns>
    function PingServer: Integer;
    /// <author>aehimself</author>
    /// <summary>Immediately abort any kind of queries.</summary>
    /// <returns>0 if the operation is aborted; Non zero otherwise.</returns>
    function AbortOperation: Integer;
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open;
    /// <summary>Releases a Connection's database and resources immediately
    ///  instead of waiting for them to be automatically released. Note: A
    ///  Connection is automatically closed when it is garbage collected.
    ///  Certain fatal errors also result in a closed Connection.</summary>
    procedure Close;
    /// <summary>Is the connection object closed?</summary>
    /// <returns><c>True</c> if Closed; <c>False</c> otherwise</returns>
    function IsClosed: Boolean;
    /// <summary>Gets the parent ZDBC driver.</summary>
    /// <returns>the parent ZDBC driver interface.</returns>
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
    function GetClientVersion: Integer;
    /// <author>fduenas</author>
    /// <summary>Gets the host's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this server's full version number</returns>
    function GetHostVersion: Integer;
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations.</summary>
    /// <remarks>This method cannot be called while a explicit transaction is
    ///  started.</remarks>
    /// <param>"Value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    /// <summary>Check if the current connection is readonly. See setReadonly.
    ///  </summary>
    /// <returns><c>True</c> if the connection is readonly; <c>False</c>
    ///  otherwise.</returns>
    function IsReadOnly: Boolean;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Value: string);
    /// <summary>Returns the Connection's current catalog name.</summary>
    /// <returns>the current catalog name or an empty string.</returns>
    function GetCatalog: string;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of <c>tiNone, tiReadUncommitted, tiReadCommitted,
    ///  tiRepeatableRead, tiSerializable</c> isolation values with the
    ///  exception of <c>tiNone</c>; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    /// <summary>Gets this Connection's current transaction isolation level.</summary>
    /// <returns>the current TRANSACTION_* mode value.</returns>
    function GetTransactionIsolation: TZTransactIsolationLevel;
    /// <summary>Returns the first warning reported by calls on this Connection.</summary>
    /// <remarks>Subsequent warnings will be chained to this EZSQLWarning.</remarks>
    /// <returns>the first SQLWarning or nil.</returns>
    function GetWarnings: EZSQLWarning;
    /// <summary>Clears all warnings reported for this <c>Connection</c> object.
    ///  After a call to this method, the method <c>getWarnings</c> returns nil
    ///  until a new warning is reported for this Connection.</summary>
    procedure ClearWarnings;

    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean);

    function GetBinaryEscapeString(const Value: TBytes): String; overload;

    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload;
    function EscapeString(const Value: RawByteString): RawByteString;


    function GetEncoding: TZCharEncoding;
    function GetClientVariantManager: IZClientVariantManager;
    function GetURL: String;
    /// <summary>Returns the ServicerProvider for this connection. For some
    ///  drivers the connection mist be opened to determine the provider.</summary>
    /// <returns>the ServerProvider or spUnknown if not known.</returns>
    function GetServerProvider: TZServerProvider;
  end;

  /// <summary>Defines the database metadata interface.</summary>
  IZDatabaseMetadata = interface(IZInterface)
    ['{FE331C2D-0664-464E-A981-B4F65B85D1A8}']

    //function GetURL: string;
    //function GetUserName: string;

    /// <author>technobot</author>
    /// <summary>Returns general information about the database (version,
    ///  capabilities,  policies, etc).</summary>
    /// <returns>the database information object as interface.</returns>
    function GetDatabaseInfo: IZDatabaseInfo;
    /// <author>EgonHugeist</author>
    function GetTriggers(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet;
    /// <author>EgonHugeist</author>
    function GetCollationAndCharSet(const Catalog, Schema, TableName, ColumnName: String): IZResultSet;
    /// <author>EgonHugeist</author>
    function GetCharacterSets: IZResultSet;
    /// <summary>Gets a description of the stored procedures available in a
    ///  catalog.
    ///  Only procedure descriptions matching the schema and procedure name
    ///  criteria are returned. They are ordered by
    ///  PROCEDURE_SCHEM, and PROCEDURE_NAME.
    ///  Each procedure description has the the following columns:
    ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
    ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
    ///  <c>PROCEDURE_NAME</c> String => procedure name
    ///  <c>PROCEDURE_OVERLOAD</c> => a overload indicator (may be null)
    ///  <c>RESERVED1</c> => for future use
    ///  <c>RESERVED2</c> => for future use
    ///  <c>REMARKS</c> String => explanatory comment on the procedure
    ///  <c>PROCEDURE_TYPE</c> short => kind of procedure:
    ///   procedureResultUnknown - May return a result
    ///   procedureNoResult - Does not return a result
    ///   procedureReturnsResult - Returns a result</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a procedure description.</returns>
    /// <remarks>see getSearchStringEscape</remarks>
    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet;
    /// <summary>Gets a description of a catalog's stored procedure parameters
    ///  and result columns.
    ///  Only descriptions matching the schema, procedure and
    ///  parameter name criteria are returned.  They are ordered by
    ///  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
    ///  if any, is first. Next are the parameter descriptions in call
    ///  order. The column descriptions follow in column number order.
    ///  Each row in the <c>ResultSet</c> is a parameter description or
    ///  column description with the following fields:
 	  ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
 	  ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
 	  ///  <c>PROCEDURE_NAME</c> String => procedure name
 	  ///  <c>COLUMN_NAME</c> String => column/parameter name
 	  ///  <c>COLUMN_TYPE</c> Short => kind of column/parameter:
    ///      Ord(pctUnknown) - nobody knows
    ///      Ord(pctIn) - IN parameter
    ///      Ord(pctInOut) - INOUT parameter
    ///      Ord(pctOut) - OUT parameter
    ///      Ord(pctReturn) - procedure return value
    ///      Ord(pctResultSet) - result column in <c>ResultSet</c>
    ///  <c>DATA_TYPE</c> short => ZDBC SQL type
 	  ///  <c>TYPE_NAME</c> String => SQL type name, for a UDT type the
    ///   type name is fully qualified
 	  ///  <c>PRECISION</c> int => precision
 	  ///  <c>LENGTH</c> int => length in bytes of data
 	  ///  <c>SCALE</c> short => scale, second fractions
 	  ///  <c>RADIX</c> short => radix
 	  ///  <c>NULLABLE</c> short => can it contain NULL?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
 	  ///  <c>REMARKS</c> String => comment describing parameter/column</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop
    ///  schema pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <param>"columnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row describes a stored procedure
    ///  parameter or column</returns>
    /// <remarks>Some databases may not return the column descriptions for a
    ///  procedure. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet;
    /// <summary>Gets a description of tables available in a catalog.
    ///  Only table descriptions matching the catalog, schema, table
    ///  name and type criteria are returned.  They are ordered by
    ///  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.
    ///  Each table description has the following columns:
 	  ///  <c>TABLE_CAT</c> String => table catalog (may be null)
 	  ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
 	  ///  <c>TABLE_NAME</c> String => table name
 	  ///  <c>TABLE_TYPE</c> String => table type.  Typical types are "TABLE",
 	  ///  		"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 	  ///  		"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	  ///  <c>REMARKS</c> String => explanatory comment on the table</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"Types" an array of table types to include; nil returns all
    ///  types.</param>
    /// <returns><c>ResultSet</c> - each row is a table description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet;
    /// <summary>Gets the schema names available in this database. The results
    ///  are ordered by schema name.
    ///  The schema column is:
    ///  <C>TABLE_SCHEM</C> String => schema name</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a schema name</returns>
    function GetSchemas: IZResultSet;
    /// <summary>Gets the catalog names available in this database. The results
    ///  are ordered by catalog name.
    ///  The catalog column is:
    ///  <C>TABLE_CAT</C> String => catalog name</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a catalog name</returns>
    function GetCatalogs: IZResultSet;
    /// <summary>Gets the table types available in this database. The results
    ///  are ordered by table type.
    ///  The table type is:
    ///  <c>TABLE_TYPE</c> String => table type. Typical types are "TABLE",
    ///  "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY","LOCAL TEMPORARY", "ALIAS",
    ///  "SYNONYM".</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a table type</returns>
    function GetTableTypes: IZResultSet;
    /// <summary>Gets a description of table columns available in
    ///  the specified catalog.
    ///  Only column descriptions matching the catalog, schema pattern, table
    ///  name pattern and column name criteria are returned. They are ordered by
    ///  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.
    ///  Each column description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>DATA_TYPE</c> short => SQL type from java.sql.Types
    ///  <c>TYPE_NAME</c> String => Data source dependent type name,
    ///   for a UDT the type name is fully qualified
    ///  <c>COLUMN_SIZE</c> int => column size.  For char or date
    ///      types this is the maximum number of characters, for numeric or
    ///      decimal types this is precision.
    ///  <c>BUFFER_LENGTH</c> is not used.
    ///  <c>DECIMAL_DIGITS</c> int => the number of fractional digits
    ///  <c>NUM_PREC_RADIX</c> int => Radix (typically either 10 or 2)
    ///  <c>NULLABLE</c> int => is NULL allowed?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
    ///  <c>REMARKS</c> String => comment describing column (may be null)
    ///  <c>COLUMN_DEF</c> String => default value (may be null)
    ///  <c>SQL_DATA_TYPE</c> int => unused
    ///  <c>SQL_DATETIME_SUB</c> int => unused
    ///  <c>CHAR_OCTET_LENGTH</c> int => for char types the
    ///        maximum number of bytes in the column
    ///  <c>ORDINAL_POSITION</c> int	=> index of column in table
    ///       (starting at 1)
    ///  <c>IS_NULLABLE</c> String => "NO" means column definitely
    ///       does not allow NULL values; "YES" means the column might
    ///       allow NULL values. An empty string means nobody knows.</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" retrieves those
    ///  without a schema</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"ColumnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a column description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond IS_NULLABLE can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
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
    /// <summary>Get's the owner connection that produced that object instance.
    /// </summary>
    /// <returns>the connection object interface.</returns>
    function GetConnection: IZConnection;
    function GetIdentifierConvertor: IZIdentifierConvertor; deprecated;
    function GetIdentifierConverter: IZIdentifierConverter; //typo fixed

    procedure ClearCache; overload;
    procedure ClearCache(const Key: string); overload;

    function AddEscapeCharToWildcards(const Pattern: string): string;
    function NormalizePatternCase(const Pattern: String): string;
    function CloneCachedResultSet(const ResultSet: IZResultSet): IZResultSet;
  end;

  /// <author>technobot</author>
  /// <summary>Defines the Database information interface. Used to describe the
  ///  database as a whole (version, capabilities, policies, etc).</summary>
  IZDatabaseInfo = interface(IZInterface)
    ['{107CA354-F594-48F9-8E08-CD797F151EA0}']
    // database/driver/server info:
    /// <summary>What's the name of this database product?</summary>
    /// <returns>database product name</returns>
    function GetDatabaseProductName: string;
    /// <summary>What's the version of this database product?</summary>
    /// <returns>database version</returns>
    function GetDatabaseProductVersion: string;
    /// <summary>What's the name of this ZDBC driver?
    /// <returns>ZDBC driver name</returns>
    function GetDriverName: string;
    /// <summary>What's the version of this ZDBC driver?</summary>
    /// <returns>the ZDBC driver version as string.</returns>
    function GetDriverVersion: string;
    /// <summary>What's this ZDBC driver's major version number?</summary>
    /// <returns>ZDBC driver major version</returns>
    function GetDriverMajorVersion: Integer;
    /// <summary>What's this ZDBC driver's minor version number?</summary>
    /// <returns>The ZDBC driver minor version number as Integer.</returns>
    function GetDriverMinorVersion: Integer;
    /// <summary>What's the server version?</summary>
    /// <returns>The server version string.</returns>
    function GetServerVersion: string;

    // capabilities (what it can/cannot do):

    /// <summary>Can all the procedures returned by getProcedures be called by
    ///  the current user?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function AllProceduresAreCallable: Boolean;
    /// <summary>Can all the tables returned by getTable be SELECTed by the
    ///  current user?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function AllTablesAreSelectable: Boolean;
    /// <summary>Does the database treat mixed case unquoted SQL identifiers as
    ///  case sensitive and as a result store them in mixed case?
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsMixedCaseIdentifiers: Boolean;
    /// <summary>Does the database treat mixed case quoted SQL identifiers as
    ///  case sensitive and as a result store them in mixed case?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsMixedCaseQuotedIdentifiers: Boolean;
    /// <summary>Is "ALTER TABLE" with add column supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsAlterTableWithAddColumn: Boolean;
    /// <summary>Is "ALTER TABLE" with drop column supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsAlterTableWithDropColumn: Boolean;
    /// <summary>Is column aliasing supported? If so, the SQL AS clause can be
    ///  used to provide names for computed columns or to provide alias names
    ///  for columns as required.<summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsColumnAliasing: Boolean;
    /// <summary>Is the CONVERT function between SQL types supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsConvert: Boolean;
    /// <summary>Not Yet implemented. Is CONVERT between the given SQL types
    ///  supported?</summary>
    /// <param><c>"FromType"</c> the type to convert from</param>
    /// <param><c>"ToType"</c> the type to convert to</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
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

  /// <summary>Generic SQL statement interface.</summary>
  IZStatement = interface(IImmediatelyReleasable)
    ['{22CEFA7E-6A6D-48EC-BB9B-EE66056E90F1}']
    /// <summary> Executes an SQL statement that returns a single
    ///  <c>ResultSet</c> object.</summary>
    /// <param>"SQL" typically this is a static SQL <c>SELECT</c> statement</param>
    /// <returns>a <c>ResultSet</c> object that contains the data produced by
    ///  the given query; never <c>nil</c></returns>
    function ExecuteQuery(const SQL: UnicodeString): IZResultSet; overload;
    /// <summary>Executes an SQL <c>INSERT</c>, <c>UPDATE</c> or <c>DELETE</c>
    ///  statement. In addition, SQL statements that return nothing, such as SQL
    ///  DDL statements, can be executed. </summary>
    /// <param>"SQL" an SQL <c>INSERT</c>, <c>UPDATE</c> or <c>DELETE</c>
    /// statement or an SQL statement that returns nothing.</param>
    /// <returns>either the row count for <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function ExecuteUpdate(const SQL: UnicodeString): Integer; overload;
    /// <summary>Executes an SQL statement that may return multiple results.
    ///  Under some (uncommon) situations a single SQL statement may return
    ///  multiple result sets and/or update counts.  Normally you can ignore
    ///  this unless you are (1) executing a stored procedure that you know may
    ///  return multiple results or (2) you are dynamically executing an
    ///  unknown SQL string.  The  methods <c>execute</c>, <c>getMoreResults</c>
    ///  , <c>getResultSet</c>, and <c>getUpdateCount</c> let you navigate
    ///  through multiple results. The <c>execute</c> method executes an SQL
    ///  statement and indicates the form of the first result. You can then use
    ///  the methods <c>getResultSet</c> or <c>getUpdateCount</c> to retrieve
    ///  the result, and <c>getMoreResults</c> to move to any subsequent
    ///  result(s).</summary>
    /// <param>"SQL" any SQL statement.</param>
    /// <returns><c>true</c> if the next result is a <c>ResultSet</c> object;
    ///  <c>false</c> if it is an update count or there are no more results.</returns>
    function Execute(const SQL: UnicodeString): Boolean; overload;
    /// <summary> Executes an SQL statement that returns a single
    ///  <c>ResultSet</c> object.</summary>
    /// <param>"SQL" typically this is a static SQL <c>SELECT</c> statement</param>
    /// <returns>a <c>ResultSet</c> object that contains the data produced by
    ///  the given query; never <c>nil</c></returns>
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload;
    /// <summary>Executes an SQL <c>INSERT</c>, <c>UPDATE</c> or <c>DELETE</c>
    ///  statement. In addition, SQL statements that return nothing, such as SQL
    ///  DDL statements, can be executed. </summary>
    /// <param>"SQL" an SQL <c>INSERT</c>, <c>UPDATE</c> or <c>DELETE</c>
    /// statement or an SQL statement that returns nothing.</param>
    /// <returns>either the row count for <c>INSERT</c>, <c>UPDATE</c> or
    ///  <c>DELETE</c> statements, or 0 for SQL statements that return nothing
    /// </returns>
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload;
    /// <summary>Executes an SQL statement that may return multiple results.
    ///  Under some (uncommon) situations a single SQL statement may return
    ///  multiple result sets and/or update counts.  Normally you can ignore
    ///  this unless you are (1) executing a stored procedure that you know may
    ///  return multiple results or (2) you are dynamically executing an
    ///  unknown SQL string.  The  methods <c>execute</c>, <c>getMoreResults</c>
    ///  , <c>getResultSet</c>, and <c>getUpdateCount</c> let you navigate
    ///  through multiple results. The <c>execute</c> method executes an SQL
    ///  statement and indicates the form of the first result. You can then use
    ///  the methods <c>getResultSet</c> or <c>getUpdateCount</c> to retrieve
    ///  the result, and <c>getMoreResults</c> to move to any subsequent
    ///  result(s).</summary>
    /// <param>"SQL" any SQL statement.</param>
    /// <returns><c>true</c> if the next result is a <c>ResultSet</c> object;
    ///  <c>false</c> if it is an update count or there are no more results.</returns>
    function Execute(const SQL: RawByteString): Boolean; overload;
    /// <summary>get the current SQL string</summary>
    function GetSQL : String;
    /// <summary>Releases this <c>Statement</c> object's database
    ///  resources immediately instead of waiting for this to happen when it is
    ///  automatically closed. It is generally good practice to release
    ///  resources as soon as you are finished with them to avoid tying up
    ///  database resources. <b>Note:</b> A <c>Statement</c> object is
    ///  automatically closed when its reference counter becomes zero. When a
    ///  <c>Statement</c> object is closed, its current <c>ResultSet</c> object,
    ///  if one exists, is also closed.</summary>
    procedure Close;
    /// <summary>Test if the <c>Statement</c></summary>
    /// <returns><c>true</c> if the <c>Statement</c> is closed; <c>false</c>
    ///  otherwise.</returns>
    function IsClosed: Boolean;
    /// <summary>Returns the maximum number of bytes allowed for any column
    ///  value. This limit is the maximum number of bytes that can be returned
    ///  for any column value. The limit applies only to <c>BINARY</c>,
    ///  <c>VARBINARY</c>, <c>LONGVARBINARY</c>, <c>CHAR</c>, <c>VARCHAR</c>,
    ///  and <c>LONGVARCHAR</c> columns.  If the limit is exceeded, the excess
    ///  data is silently discarded. </summary>
    /// <returns>the current max column size limit; zero means unlimited.</returns>
    function GetMaxFieldSize: Integer;
    /// <summary>Sets the limit for the maximum number of bytes in a column to
    ///  the given number of bytes.  This is the maximum number of bytes that
    ///  can be returned for any column value.  This limit applies only to
    ///  <c>BINARY</c>, <c>VARBINARY</c>, <c>LONGVARBINARY</c>, <c>CHAR</c>,
    ///  <c>VARCHAR</c>, and <c>LONGVARCHAR</c> fields.  If the limit is
    ///  exceeded, the excess data is silently discarded. For maximum
    ///  portability, use values greater than 256.</summary>
    /// <param>"Value" the new max column size limit; zero means unlimited</param>
    procedure SetMaxFieldSize(Value: Integer);
    /// <summary>Retrieves the maximum number of rows that a <c>ResultSet</c>
    ///  object can contain.  If the limit is exceeded, the excess rows are
    ///  silently dropped.</summary>
    /// <returns>the current max row limit; zero means unlimited</returns>
    function GetMaxRows: Integer;
    /// <summary>Sets the limit for the maximum number of rows that any
    ///  <c>ResultSet</c> object can contain to the given number. If the limit
    ///  is exceeded, the excess rows are silently dropped.</summary>
    /// <param>"Value" the new max rows limit; zero means unlimited</param>
    procedure SetMaxRows(Value: Integer);
    /// <summary>Retrieves the number of seconds the driver will wait for a
    ///  <c>Statement</c> object to execute. If the limit is exceeded, a
    ///  <c>SQLException</c> is thrown.</summary>
    /// <returns>the current query timeout limit in seconds; zero means
    ///  unlimited.</returns>
    function GetQueryTimeout: Integer;
    /// <summary>Sets the number of seconds the driver will wait for a
    ///  <c>Statement</c> object to execute to the given number of seconds. If
    ///  the limit is exceeded, an <c>SQLException</c> is thrown.</summary>
    /// <param>"Value" the new query timeout limit in seconds; zero means
    ///  unlimited.</param>
    procedure SetQueryTimeout(Value: Integer);
    /// <summary>
    ///  Cancels this <c>Statement</c> object if both the DBMS and
    ///  driver support aborting an SQL statement.
    ///  This method can be used by one thread to cancel a statement that
    ///  is being executed by another thread.
    /// </summary>
    procedure Cancel;
    /// <summary>Defines the SQL cursor name that will be used by
    ///  subsequent <c>Statement</c> object <c>execute</c> methods.
    ///  This name can then be used in SQL positioned update / delete statements
    ///  to identify the current row in the <c>ResultSet</c> object generated by
    ///  this statement. If the database doesn't support positioned update/
    ///  delete, this method is a noop. To insure that a cursor has the proper
    ///  isolation level to support updates, the cursor's <c>SELECT</c>
    ///  statement should be of the form 'select for update ...'. If the
    ///  'for update' phrase is omitted, positioned updates may fail.
    ///  <B>Note:</B> By definition, positioned update/delete execution must be
    ///  done by a different <c>Statement</c> object than the one which
    ///  generated the <c>ResultSet</c> object being used for positioning. Also,
    ///  cursor names must be unique within a connection.</summary>
    /// <param>"Value" the new cursor name, which must be unique within a
    ///  connection</param>
    procedure SetCursorName(const Value: String);
    /// <summary>Returns the current result as a <c>ResultSet</c> object.
    ///  This method should be called only once per result.</summary>
    /// <returns>the current result as a <c>ResultSet</c> object; <c>nil</c> if
    ///  the result is an update count or there are no more results.</returns>
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
    /// <summary>Moves to a <c>Statement</c> object's next result.  It returns
    ///  <c>true</c> if this result is a <c>ResultSet</c> object.
    ///  This method also implicitly closes any current <c>ResultSet</c>
    ///  object obtained with the method <c>getResultSet</c>.
    ///
    ///  There are no more results when the following is true:
    ///  <code>(not getMoreResults and (getUpdateCount = -1)</code>
    /// </summary>
    /// <returns><c>true</c> if the next result is a <c>ResultSet</c> object;
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
    /// <param>"Value" the initial direction for processing rows</param>
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
    /// <param>"Value" the number of rows to fetch</param>
    procedure SetFetchSize(Value: Integer);
    /// <summary>Retrieves the number of result set rows that is the default
    ///  fetch size for result sets
    ///  generated from this <c>Statement</c> object.
    ///  If this <c>Statement</c> object has not set
    ///  a fetch size by calling the method <c>setFetchSize</c>,
    ///  the return value is implementation-specific.
    ///  <b>Note:</b> Most drivers will ignore this.</summary>
    /// <returns>
    ///  the default fetch size for result sets generated
    ///  from this <c>Statement</c> object
    /// </returns>
    function GetFetchSize: Integer;
    /// <summary>Sets a result set concurrency for <c>ResultSet</c> objects
    ///  generated by this <c>Statement</c> object.</summary>
    /// <param>"Value"either <c>rcReadOnly</c> or <c>rcUpdateable</c></param>
    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency);
    /// <summary>Retrieves the result set concurrency for <c>ResultSet</c>
    ///  objects generated by this <c>Statement</c> object.</summary>
    /// <returns>either <c>rcReadOnly</c> or <c>rcUpdateable</c></returns>
    function GetResultSetConcurrency: TZResultSetConcurrency;
    /// <summary>Sets a result set type for <c>ResultSet</c> objects generated
    ///  by this <c>Statement</c> object.</summary>
    /// <param>"Value" one of <c>rtForwardOnly</c>, <c>rtScrollInsensitive</c>,
    ///  or <c>rtScrollSensitive</c></param>
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
    /// <summary>Note yet implemented. Propably omitted in future.
    ///  Sets a new value for post updates.</summary>
    /// <param>"Value" a new value for post updates.</param>
    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    /// <summary>Note yet implemented. Propably omitted in future.
    ///  Gets the current value for post updates.</summary>
    /// <returns>the current value for post updates.</returns>
    function GetPostUpdates: TZPostUpdatesMode;
    /// <summary>Note yet implemented. Propably omitted in future.
    ///  Sets a new value for locate updates.</summary>
    /// <param>"Value" a new value for locate updates.</param>
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    /// <summary>Note yet implemented. Propably omitted in future.
    ///  Gets the current value for locate updates.</summary>
    /// <returns>the current value for locate updates.</returns>
    function GetLocateUpdates: TZLocateUpdatesMode;
    /// <summary>Not yet implemented. Propably omitted in future.
    ///  Adds an SQL command to the current batch of commmands for this
    ///  <c>Statement</c> object. This method is optional.</summary>
    /// <param>"SQL" typically this is a static SQL <c>INSERT</c> or
    ///  <c>UPDATE</c> statement</param>
    procedure AddBatch(const SQL: string);
    /// <summary>Not yet implemented. Propably omitted in future.
    ///  Adds an SQL command to the current batch of commmands for this
    ///  <c>Statement</c> object. This method is optional.</summary>
    /// <param>"SQL" typically this is a static SQL <c>INSERT</c> or
    ///  <c>UPDATE</c> statement </param>
    procedure AddBatchRequest(const SQL: string);
    /// <summary> Not yet implemented. Propably omitted in future.
    ///  Makes the set of commands in the current batch empty.
    ///  This method is optional.
    /// </summary>
    procedure ClearBatch;
    /// <summary> Not yet implemented. Propably omitted in future.
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
    /// <summary>Returns the <c>Connection</c> object that produced this
    ///  <c>Statement</c> object.</summary>
    /// <returns>
    /// <see cref="IZConnection"></see>
    ///  the connection that produced this statement
    /// </returns>
    function GetConnection: IZConnection;
    /// <summary>Gets statement parameters.</summary>
    /// <returns>a list with statement parameters.</returns>
    function GetParameters: TStrings;
    /// <summary>Returns the ChunkSize for reading/writing large lobs</summary>
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
    /// <summary>The sender resultset get's closed. Notify owner
    ///  <c>Statement</c> about to drop the weak reference. The statement,
    ///  if executed, will create a new resultset object.</summary>
    /// <param>"Sender" the closing resultset.</param>
    procedure FreeOpenResultSetReference(const Sender: IZResultSet);
    /// <summary>Result a unique internal Id per class.</summary>
    /// <returns>the the new class id.</returns>
    function GetStatementId: NativeUInt;
  end;

  /// <summary>Defines prepared SQL statement interface.</summary>
  IZPreparedStatement = interface(IZStatement)
    ['{990B8477-AF11-4090-8821-5B7AFEA9DD70}']
    /// <summary>Executes the SQL query in this <c>PreparedStatement</c> object
    ///  and returns the result set generated by the query.</summary>
    /// <returns>a <c>IZResultSet</c> interface that contains the data produced
    ///  by the query; never <c>nil</c></returns>
    function ExecuteQueryPrepared: IZResultSet;
    /// <summary>Executes the SQL INSERT, UPDATE or DELETE statement in this
    ///  <c>PreparedStatement</c> object. In addition, SQL statements that
    ///  return nothing, such as SQL DDL statements, can be executed.</summary>
    /// <returns>either the row count for INSERT, UPDATE or DELETE statements;
    ///  or -1 for SQL statements that return nothing</returns>
    function ExecuteUpdatePrepared: Integer;
    /// <summary>Executes any kind of SQL statement. Some prepared statements
    ///  return multiple results; the <c>ExecutePrepared</c> method handles these
    ///  complex statements as well as the simpler form of statements handled
    ///  by the methods <c>ExecuteQuery</c> and <c>ExecuteUpdate</c>.
    ///  see IStatement.execute</summary>
    /// <returns>True if a ResultSet is available otherwise false.</returns>
    function ExecutePrepared: Boolean;
    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    /// <summary>Sets the designated parameter to a <c>ShortInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    /// <summary>Sets the designated parameter to a <c>SmallInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    /// <summary>Sets the designated parameter to a <c>Cardinal</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    /// <summary>Sets the designated parameter to a <c>Integer</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    /// <summary>Sets the designated parameter to a <c>UInt64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    /// <summary>Sets the designated parameter to a <c>Int64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    /// <summary>Sets the designated parameter to a <c>Single</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    /// <summary>Sets the designated parameter to a <c>Double</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    /// <summary>Sets the designated parameter to a <c>Currency</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency);
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD);
    /// <summary>Sets the designated parameter to a <c>TZCharRec</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    /// <summary>Sets the designated parameter to a <c>String</c> value.
    ///  This method equals to SetUnicodeString on Unicode-Compilers. For
    ///  Raw-String compilers the encoding is defined by W2A2WEncodingSource of
    ///  the ConnectionSettings record. The driver will convert the string to
    ///  the Client-Characterset.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetString(ParameterIndex: Integer; const Value: String);
    /// <summary>Sets the designated parameter to a <c>UnicodeString</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: UnicodeString);
    /// <summary>Sets the designated parameter to a <c>byte array</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); overload;
    /// <summary>Sets the designated parameter to a <c>ByteArray reference</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value reference.</param>
    /// <param>"Len" the Length of the bytes buffer.</param>
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); overload;
    /// <summary>Sets the designated parameter to a <c>TGUID</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID);
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Sets the designated parameter to a <c>AnsiString</c> value.
    ///  The string must be GET_ACP encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Sets the designated parameter to a <c>AnsiString</c> value.
    ///  The string must be UTF8 encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    /// <summary>Sets the designated parameter to a <c>AnsiString</c> value.
    ///  The string must be DB-CodePage encoded. If the driver uses an UTF16
    ///  encoding, the driver will convert the value using the conversion rules
    ///  given by W2A2WEncodingSource of the ConnectionSettings record.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    /// <summary>Sets the designated parameter to a <c>Date(TDateTime)</c> value.
    ///  This method is obsolate and left for compatibility. The method always
    ///  decodes the value and calls the <c>SetDate(Index: Integer; Value: TZDate)</c>
    ///  overload.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); overload;
    /// <summary>Sets the designated parameter to a <c>TZDate</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); overload;
    /// <summary>Sets the designated parameter to a <c>Time(TDateTime)</c> value.
    ///  This method is obsolate and left for compatibility. The method always
    ///  decodes the value and calls the <c>SetTime(Index: Integer; Value: TZtime)</c>
    ///  overload.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); overload;
    /// <summary>Sets the designated parameter to a <c>TZTime</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); overload;
    /// <summary>Sets the designated parameter to a <c>TDateTime</c> value.
    ///  This method is obsolate and left for compatibility. The method always
    ///  decodes the value and calls the
    ///  <c>SetTimestamp(Index: Integer; Value: TZTimestamp)</c>overload.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); overload;
    /// <summary>Sets the designated parameter to a <c>TZTimestamp</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); overload;
    /// <summary>Sets the designated parameter to a raw character stream value.
    ///  The stream must be DB-CodePage encoded. If the driver uses an UTF16
    ///  encoding, the driver will convert the value using the conversion rules
    ///  given by W2A2WEncodingSource of the ConnectionSettings record.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream);
    /// <summary>Sets the designated parameter to a UTF16 character stream value.
    ///  The driver will convert the value to the raw encoding if the driver
    ///  does not support UTF16 streams.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream);
    /// <summary>Sets the designated parameter to the given input stream, which
    ///  will have the specified number of bytes.
    ///  When a very large binary value is input to a <c>LONGVARBINARY</c>
    ///  parameter, it may be more practical to send it via a
    ///  <c>TStream</c> object. The data will be read from the stream
    ///  as needed until end-of-file is reached.
    ///  <B>Note:</B> This stream object can either be a standard
    ///  TStream object or your own subclass that implements the
    ///  standard interface.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream);
    /// <summary>Sets the designated parameter to the given blob wrapper object.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" defines the lob constent. Valid values are:
    ///  stAsciiStream(raw encoded text), stUnicodeStream(UTF16 encoded text)
    ///  and stBinaryStream(binary data), stJSON, stXML</param>
    /// <param>"Value" the parameter blob wrapper object to be set.</param>
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob);
    /// <summary>Sets the designated parameter to the value. The value content
    ///  will be decoded and the associated setter will be called.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter blob wrapper object to be set.</param>
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant);
    /// <summary>Sets the designated parameter to a null array value. A null
    ///  array can not be bound if not data array has been bound before. So
    ///  SetDataArray() needs to be called first.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQLType of the array. Valid value is stBoolean.</param>
    /// <param>"Value" the parameter null array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"VariantType" the VariantType of the array. Valid value is vtNull.</param>
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
    /// <summary>Sets the designated parameter to a data array value. This
    ///  method usually initializes the BatchArray DML mode unless the parameter
    ///  was registered as a PLSQLTable ( in (?) )before.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"SQLType" the SQLType of the array</param>
    /// <param>"VariantType" the VariantType of the array. It is used as a
    ///  subtype like:
    ///  (SQLType = stString, VariantType = vtUTF8String) or
    ///  (SQLType = stDate, VariantType = vtDate or vtDateTime) </param>
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
    /// <summary>Register the parameter properties. This method is required for
    ///  all InOut, Out or Result parameters to access them afterwards. It's not
    ///  requiered to register In params.</summary>
    /// <param>"ParameterIndex" the first parameter is 0, the second is 1, ...
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the parameters SQLType.</param>
    /// <param>"ParamType" the TZProcedureColumnType of the parameter.</param>
    /// <param>"PrecisionOrSize" either the Precision for Numeric types or the
    ///  Length for strings or bytes. The value is ignored for all other types.</param>
    /// <param>"Scale" the numeric or second-fraction scale of the parameter.</param>
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      {%H-}Scale: LengthInt = 0);

    //======================================================================
    // Methods for accessing out parameters by index
    //======================================================================

    /// <summary>Indicates if the value of the designated paramert is Null.</summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>true</c>. <c>false</c> otherwise.</returns>
    function IsNull(ParameterIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated parameter as a Booelan value.
    ///  The driver will try to convert the value if it's not a Boolean value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>False</c>. The value otherwise.</returns>
    function GetBoolean(ParameterIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated parameter as a Byte value.
    ///  The driver will try to convert the value if it's not a Byte value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetByte(ParameterIndex: Integer): Byte;
    /// <summary>Gets the value of the designated parameter as a ShortInt value.
    ///  The driver will try to convert the value if it's not a ShortInt value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetShort(ParameterIndex: Integer): ShortInt;
    /// <summary>Gets the value of the designated parameter as a Word value.
    ///  The driver will try to convert the value if it's not a Word value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetWord(ParameterIndex: Integer): Word;
    /// <summary>Gets the value of the designated parameter as a SmallInt value.
    ///  The driver will try to convert the value if it's not a SmallInt value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetSmall(ParameterIndex: Integer): SmallInt;
    /// <summary>Gets the value of the designated parameter as a Cardinal value.
    ///  The driver will try to convert the value if it's not a Cardinal value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetUInt(ParameterIndex: Integer): Cardinal;
    /// <summary>Gets the value of the designated parameter as a Integer value.
    ///  The driver will try to convert the value if it's not a Integer value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetInt(ParameterIndex: Integer): Integer;
    /// <summary>Gets the value of the designated parameter as a UInt64 value.
    ///  The driver will try to convert the value if it's not a UInt64 value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetULong(ParameterIndex: Integer): UInt64;
    /// <summary>Gets the value of the designated parameter as a Int64 value.
    ///  The driver will try to convert the value if it's not a Int64 value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetLong(ParameterIndex: Integer): Int64;
    /// <summary>Gets the value of the designated parameter as a Single value.
    ///  The driver will try to convert the value if it's not a Single value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetFloat(ParameterIndex: Integer): Single;
    /// <summary>Gets the value of the designated parameter as a Double value.
    ///  The driver will try to convert the value if it's not a Double value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDouble(ParameterIndex: Integer): Double;
    /// <summary>Gets the value of the designated parameter as a Currency value.
    ///  The driver will try to convert the value if it's not a Currency value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetCurrency(ParameterIndex: Integer): Currency;
    /// <summary>Gets the value of the designated parameter as a TBCD value.
    ///  The driver will try to convert the value if it's not a TBCD value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-BCD</c>. The value otherwise.</returns>
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
    /// <summary>Gets the value of the designated parameter as a TGUID value.
    ///  The driver will try to convert the value if it's not a TGUID value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-GUID</c>. The value otherwise.</returns>
    procedure GetGUID(Index: Integer; var Result: TGUID);
    /// <summary>Gets the value of the designated parameter as a TBytes value.
    ///  The driver will try to convert the value if it's not a TBytes value.
    /// </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>nil</c>. The value otherwise.</returns>
    function GetBytes(ParameterIndex: Integer): TBytes; overload;
    /// <summary>Obsolate use overload instead. Gets the value of the designated
    ///  parameter as a TDate value. The driver will try to convert the value if
    ///  it's not a TDateTime value. </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDate(ParameterIndex: Integer): TDateTime; overload;
    procedure GetDate(ParameterIndex: Integer; Var Result: TZDate); overload;
    /// <summary>Obsolate use overload instead. Gets the value of the designated
    ///  parameter as a TTime value. The driver will try to convert the value if
    ///  it's not a TDateTime value. </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTime(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTime(ParameterIndex: Integer; Var Result: TZTime); overload;
    /// <summary>Obsolate use overload instead. Gets the value of the designated
    ///  parameter as a TDateTime value. The driver will try to convert the
    ///  value if it's not a TDateTime value. </summary>
    /// <param>"ParameterIndex" the first Parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first Parameter is 0,
    ///  the second is 1. This will change in future to a zero based index. It's
    ///  recommented to use an incrementation of FirstDbcIndex. <c>Note</c> only
    ///  as InOut,Out,Result registered parameters can be accessed after the
    ///  statement has been executed and the out params are available.
    ///  Otherwise an EZSQLException is thrown.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
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
    function GetCLob(ParameterIndex: Integer): IZClob;

    procedure ClearParameters;
  end;

  /// <summary>Callable SQL statement interface.</summary>
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
  /// <summary>Defines json compose options.</summary>
  TZJSONComposeOption = (jcoEndJSONObject, jcoDATETIME_MAGIC, jcoMongoISODate,
    jcoMilliseconds, jcsSkipNulls);
  /// <summary>Defines a aet of json compose options.
  ///  - if jcoEndJSONObject is included the row gets the closing '}'
  ///  (curly bracket) to terminate the object.
  ///  - if jcoDATETIME_MAGIC is included the each date string starts with the
  ///   SynCommons.pas DataTime-Magic value.
  ///  - if jcoMongoISODate is included the each date string starts with
  ///   <c>ISODate(</c> and ends with <c>Z)</c>
  ///  - if jcoMilliseconds is included the each date value uses millisecond
  ///   precision
  ///  - if jcsSkipNulls is included each SQL <c>NULL</c> null field will be
  ///   skipped. This keeps the JSON tiny.</summary>
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
    /// <summary>get the number of columns in this <c>ResultSet</c> interface.</summary>
    /// <returns>the number of columns</returns>
    function GetColumnCount: Integer;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    /// <summary>Indicates if the value of the designated column in the current
    ///  row of this <c>ResultSet</c> object is Null.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>true</c>. <c>false</c> otherwise.</returns>
    function IsNull(ColumnIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>String</c>. This method equals to
    ///  GetUnicodeString on Unicode-Compilers. For Raw-String compilers the
    ///  string encoding is defined by W2A2WEncodingSource of the
    ///  ConnectionSettings record. The driver will try to convert the
    ///  value if it's necessary.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetString(ColumnIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>AnsiString</c>. The driver will
    ///  try to convert the value if it's not a raw value in operating system
    ///  encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UTF8String</c>. The driver will
    ///  try to convert the value if it's not a raw value in UTF8 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>RawByteString</c>.
    ///  The driver will try to convert the value if it's not a raw value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UnicodeString</c> in
    ///  the pascal programming language. The driver will try to convert the
    ///  value if it's not a value in UTF16 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUnicodeString(ColumnIndex: Integer): UnicodeString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PAnsiChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver try convert the value if it's not a
    ///  raw text value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PWideChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver will try to convert the value if it's
    ///  not a UTF16 text value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in words.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Boolean</c> value.The driver will
    ///  try to convert the value if it's not a Boolean value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>False</c>. The value otherwise.</returns>
    function GetBoolean(ColumnIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Byte</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetByte(ColumnIndex: Integer): Byte;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>ShortInt</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetShort(ColumnIndex: Integer): ShortInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Word</c> value.The driver will
    ///  try to convert the value if it's not a Word value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetWord(ColumnIndex: Integer): Word;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>SmallInt</c> value.The driver will
    ///  try to convert the value if it's not a SmallInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetSmall(ColumnIndex: Integer): SmallInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Cardinal</c> value.The driver will
    ///  try to convert the value if it's not a Cardinal value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetUInt(ColumnIndex: Integer): Cardinal;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Integer</c> value.The driver will
    ///  try to convert the value if it's not a Integer value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetInt(ColumnIndex: Integer): Integer;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UInt64</c> value.The driver will
    ///  try to convert the value if it's not a UInt64 value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetULong(ColumnIndex: Integer): UInt64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Int64</c> value.The driver will
    ///  try to convert the value if it's not a Int64 value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetLong(ColumnIndex: Integer): Int64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Single</c> value.The driver will
    ///  try to convert the value if it's not a Single value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetFloat(ColumnIndex: Integer): Single;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Double</c> value.The driver will
    ///  try to convert the value if it's not a Double value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDouble(ColumnIndex: Integer): Double;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Currency</c> value.The driver will
    ///  try to convert the value if it's not a Currency value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetCurrency(ColumnIndex: Integer): Currency;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBCD</c> value.The driver will
    ///  try to convert the value if it's not a TBCD value. The value will be
    ///  filled with the minimum of digits and precision.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-BCD</c>. The value otherwise.</param>
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TGUID</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-UID</c>. The value otherwise.</param>
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBytes</c> value.The driver will
    ///  try to convert the value if it's not a TBytes value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>nil</c>. The value otherwise.</returns>
    function GetBytes(ColumnIndex: Integer): TBytes; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PByte</c> binary reference.
    ///  Live time is per call. It's not guaranteed the address is valid after
    ///  the row position changed, or another column of same row has been
    ///  accessed. It is an error to write into the buffer. The driver will try
    ///  to convert the value if it's not a binary value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Date value. Note this method
    ///  is obsolate. It always calls the GetDate using the TZDate overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDate(ColumnIndex: Integer): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZDate</c> value. The driver will
    ///  try to convert the value if it's not a Date value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZDATE</c>. The value otherwise.</param>
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value. Note this method
    ///  is obsolate. It always calls the GetTime using the TZTime overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTime(ColumnIndex: Integer): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTime</c>. The value otherwise.</returns>
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Timestamp value. Note this method
    ///  is obsolate. It always calls the GetTimestamp using the TZTimestamp
    ///  overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimestamp(ColumnIndex: Integer): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTimestamp</c> value. The driver
    ///  will try to convert the value if it's not a Timestamp value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTimestamp</c>. The value otherwise.</param>
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into ASCII.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAsciiStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  operating system encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAnsiStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  UTF8 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUTF8Stream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of UTF16 characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGNVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into UTF16
    ///  encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUnicodeStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of a column in the current row as a stream of
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a binary stream of uninterpreted bytes. The
    ///  value can then be read in chunks from the stream. This method is
    ///  particularly suitable for retrieving large <c>LONGVARBINARY</c> values.
    /// </summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetBinaryStream(ColumnIndex: Integer): TStream;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZBlob</c> object.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>Blob</c> object representing the SQL <c>BLOB</c> value in
    ///  the specified column otherwise</returns>
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZResultSet</c> object.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>ResultSet</c> object representing the SQL
    ///  <c>ResultSet</c> value in the specified column otherwise</returns>
    function GetResultSet(ColumnIndex: Integer): IZResultSet;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a TZVariant record.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-Variant</c>. The variable value otherwise.</returns>
    function GetValue(ColumnIndex: Integer): TZVariant;
    /// <summary>Gets the DefaultExpression value of the designated column in
    /// the current row of this <c>ResultSet</c> object as a <c>String</c>.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the DefaultExpression value</returns>
    function GetDefaultExpression(ColumnIndex: Integer): string;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    /// <summary>Indicates if the value of the designated column in the current
    ///  row of this <c>ResultSet</c> object is Null.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>true</c>. <c>false</c> otherwise.</returns>
    function IsNullByName(const ColumnName: string): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PAnsiChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver try convert the value if it's not a
    ///  raw text value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPAnsiCharByName(const ColumnName: string; out Len: NativeUInt): PAnsiChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>String</c>. This method equals to
    ///  GetUnicodeString on Unicode-Compilers. For Raw-String compilers the
    ///  string encoding is defined by W2A2WEncodingSource of the
    ///  ConnectionSettings record. The driver will try to convert the
    ///  value if it's necessary.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetStringByName(const ColumnName: string): String;
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>AnsiString</c>. The driver will
    ///  try to convert the value if it's not a raw value in operating system
    ///  encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetAnsiStringByName(const ColumnName: string): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UTF8String</c>. The driver will
    ///  try to convert the value if it's not a raw value in UTF8 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUTF8StringByName(const ColumnName: string): UTF8String;
    {$ENDIF}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>RawByteString</c>.
    ///  The driver will try to convert the value if it's not a raw value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetRawByteStringByName(const ColumnName: string): RawByteString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UnicodeString</c> in
    ///  the pascal programming language. The driver will try to convert the
    ///  value if it's not a value in UTF16 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUnicodeStringByName(const ColumnName: string): UnicodeString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PWideChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver will try to convert the value if it's
    ///  not a UTF16 text value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in words.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPWideCharByName(const ColumnName: string; out Len: NativeUInt): PWideChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Boolean</c> value.The driver will
    ///  try to convert the value if it's not a Boolean value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>False</c>. The value otherwise.</returns>
    function GetBooleanByName(const ColumnName: string): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Byte</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetByteByName(const ColumnName: string): Byte;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>ShortInt</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetShortByName(const ColumnName: string): ShortInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Word</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetWordByName(const ColumnName: string): Word;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>SmallInt</c> value.The driver will
    ///  try to convert the value if it's not a SmallInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetSmallByName(const ColumnName: string): SmallInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Cardinal</c> value.The driver will
    ///  try to convert the value if it's not a Cardinal value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetUIntByName(const ColumnName: string): Cardinal;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Integer</c> value.The driver will
    ///  try to convert the value if it's not a Integer value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetIntByName(const ColumnName: string): Integer;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UInt64</c> value.The driver will
    ///  try to convert the value if it's not a UInt64 value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetULongByName(const ColumnName: string): UInt64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Int64</c> value.The driver will
    ///  try to convert the value if it's not a Int64 value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetLongByName(const ColumnName: string): Int64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Single</c> value.The driver will
    ///  try to convert the value if it's not a Single value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetFloatByName(const ColumnName: string): Single;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Double</c> value.The driver will
    ///  try to convert the value if it's not a Double value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDoubleByName(const ColumnName: string): Double;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Currency</c> value.The driver will
    ///  try to convert the value if it's not a Currency value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetCurrencyByName(const ColumnName: string): Currency;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBCD</c> value.The driver will
    ///  try to convert the value if it's not a TBCD value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Resuls" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-BCD</c>. The value otherwise.</param>
    procedure GetBigDecimalByName(const ColumnName: string; var Result: TBCD);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TGUID</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-UID</c>. The value otherwise.</param>
    procedure GetGUIDByName(const ColumnName: string; var Result: TGUID);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBytes</c> value.The driver will
    ///  try to convert the value if it's not a TBytes value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>nil</c>. The value otherwise.</returns>
    function GetBytesByName(const ColumnName: string): TBytes; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PByte</c> binary reference.
    ///  Live time is per call. It's not guaranteed the address is valid after
    ///  the row position changed, or another column of same row has been
    ///  accessed. It is an error to write into the buffer. The driver will try
    ///  to convert the value if it's not a binary value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetBytesByName(const ColumnName: string; out Len: NativeUInt): PByte; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Date value. Note this method
    ///  is obsolate. It always calls the GetDate using the TZDate overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDateByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZDate</c> value. The driver will
    ///  try to convert the value if it's not a Date value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZDate</c>. The value otherwise.</param>
    procedure GetDateByName(const ColumnName: string; var Result: TZDate); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value. Note this method
    ///  is obsolate. It always calls the GetTime using the TZTime overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimeByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTime</c>. The value otherwise.</returns>
    procedure GetTimeByName(const ColumnName: string; Var Result: TZTime); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Timestamp value. Note this method
    ///  is obsolate. It always calls the GetTimestamp using the TZTimestamp
    ///  overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimestampByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTimestamp</c> value. The driver
    ///  will try to convert the value if it's not a Timestamp value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTimestamp</c>. The value otherwise.</param>
    procedure GetTimeStampByName(const ColumnName: string; var Result: TZTimeStamp); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into ASCII.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAsciiStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  operating system encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAnsiStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  UTF8 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUTF8StreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of UTF16 characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGNVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into UTF16
    ///  encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUnicodeStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of a column in the current row as a stream of
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a binary stream of uninterpreted bytes. The
    ///  value can then be read in chunks from the stream. This method is
    ///  particularly suitable for retrieving large <c>LONGVARBINARY</c> values.
    /// </summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetBinaryStreamByName(const ColumnName: string): TStream;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZBlob</c> object.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>Blob</c> object representing the SQL <c>BLOB</c> value in
    ///  the specified column otherwise</returns>
    function GetBlobByName(const ColumnName: string; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZResultSet</c> object.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>ResultSet</c> object representing the SQL
    ///  <c>ResultSet</c> value in the specified column otherwise</returns>
    function GetResultSetByName(const ColumnName: String): IZResultSet;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a TZVariant record.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-Variant</c>. The variable value otherwise.</returns>
    function GetValueByName(const ColumnName: string): TZVariant;
    /// <summary>Gets the DefaultExpression value of the designated column in
    /// the current row of this <c>ResultSet</c> object as a <c>String</c>.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the DefaultExpression value</returns>
    function GetDefaultExpressionByName(const ColumnName: string): string;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;

    /// <summary>Not yet implpemented. Gets the name of the SQL cursor used by
    ///  this <c>ResultSet</c> object. In SQL, a result table is retrieved
    ///  through a cursor that is named. The current row of a result set can be
    ///  updated or deleted using a positioned update/delete statement that
    ///  references the cursor name. To insure that the cursor has the proper
    ///  isolation level to support update, the cursor's <c>select</c> statement
    ///  should be of the form 'select for update'. If the 'for update' clause
    ///  is omitted, the positioned updates may fail.
    ///  The ZDBC API supports this SQL feature by providing the name of the
    ///  SQL cursor used by a <c>ResultSet</c> object. The current row of a
    ///  <c>ResultSet</c> object is also the current row of this SQL cursor.
    ///  <B>Note:</B> If positioned update is not supported, a
    ///  <c>EZSQLException</c> is thrown.</summary>
    /// <returns>the SQL name for this <c>ResultSet</c> object's cursor</returns>
    function GetCursorName: String;
    /// <summary>Retrieves the IZResultSetMetadata interface containing all
    ///  Informations of the <c>ResultSet</c> object's columns.</summary>
    /// <returns>the description interface of this <c>ResultSet</c> object's
    /// columns.</returns>
    function GetMetadata: IZResultSetMetadata;
    /// <summary>Maps the given <c>Metadata</c> column name to its
    ///  <c>Metadata</c> column index. First searches with case-sensivity then,
    ///  if nothing matches, a case.insensitive search is performed.
    /// <param>"ColumnName" the name of the column</param>
    /// <returns>the column index of the given column name or an
    ///  InvalidDbcIndex if nothing was found</returns>
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

    /// <summary>Yet not implemented.
    ///  Gives a hint as to the direction in which the rows in this
    ///  <c>ResultSet</c> object will be processed. Default is fdForward.
    ///  The initial value is determined by the
    ///  <c>Statement</c> object
    ///  that produced this <c>ResultSet</c> object.
    ///  The fetch direction may be changed at any time.</summary>
    /// <param>"Value" one of <c>fdForward, fdReverse, fdUnknown</c>.</param>
    procedure SetFetchDirection(Value: TZFetchDirection);
    /// <summary>Returns the fetch direction for this <c>ResultSet</c> object.
    /// </summary>
    /// <returns>the current fetch direction for this <c>ResultSet</c> object</returns>
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

  /// <summary>Defines the ResultSet metadata interface.</summary>
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
    /// <summary>Indicates whether the designated column is automatically
    ///  numbered, thus read-only.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsAutoIncrement(ColumnIndex: Integer): Boolean;
    /// <summary>Indicates whether a column's case matters.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsCaseSensitive(ColumnIndex: Integer): Boolean;
    /// <summary>Indicates whether the designated column can be used in a where
    ///  clause.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsSearchable(ColumnIndex: Integer): Boolean;
    /// <summary>Set if the column can be used in a where
    ///  clause.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Value" <c>true</c> if the column is searchable;
    ///  <c>False</c> otherwise.</param>
    procedure SetSearchable(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Indicates whether the designated column is a cash value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsCurrency(ColumnIndex: Integer): Boolean;
    /// <summary>Indicates the nullability of values in the designated column.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the nullability status of the given column; one of
    ///  <c>ntNoNulls</c>, <c>ntNullable</c> or <c>ntNullableUnknown</c></returns>
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType;
    /// <summary>Indicates whether values in the designated column are signed
    ///  numbers.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsSigned(ColumnIndex: Integer): Boolean;
    /// <summary>Gets the designated column's suggested title for use in
    ///  printouts and displays.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>a case sensitive unique column title.</returns>
    function GetColumnLabel(ColumnIndex: Integer): string;
    /// <summary>Gets the designated column's original title for use in
    ///  printouts and displays returned by the server.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the server given column title.</returns>
    function GetOrgColumnLabel(ColumnIndex: Integer): string;
    /// <summary>Get the designated column's name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>return column name or "" if not applicable</returns>
    function GetColumnName(ColumnIndex: Integer): string;
    /// <summary>Get the designated column's codepage.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the column codepage.</returns>
    /// <remarks>If the column is a [var,long,fixed]binary the returned value is
    ///  zero. If the column is a text/character column the returned value is
    ///  depends to the connection characterset or if the Charset is vairable
    ///  like IB/FB characterset "NONE" it's the column-characterset. Otherwise
    ///  the value is High(Word) and indicates a zCP_NONE codepage. See
    ///  ZEncoding.pas.</remarks>
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    /// <summary>Get the designated column's table's schema.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>schema name or "" if not applicable</returns>
    function GetSchemaName(ColumnIndex: Integer): string;
    /// <summary>Get the designated column's number of decimal digits for
    ///  numeric or decimal types or or the number of bytes for binary columns
    ///  or the number of display characters any other column type</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>precision/bytes/visible characters</returns>
    function GetPrecision(ColumnIndex: Integer): Integer;
    /// <summary>Gets the designated column's number of digits to right of the
    ///  decimal point for Numeric or Decimal types or the second fractions for
    ///  time/timestamp types or the minimum chars/bytes for fixed
    ///  binary/char/nchar columns, zero otherwise.
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>scale</returns>
    function GetScale(ColumnIndex: Integer): Integer;
    /// <summary>Gets the designated column's table name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>table name or "" if not applicable.</returns>
    function GetTableName(ColumnIndex: Integer): string;
    /// <summary>Gets the designated column's catalog name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>catalog name or "" if not applicable.</returns>
    function GetCatalogName(ColumnIndex: Integer): string;
    /// <summary>Retrieves the designated column's SQL type.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the ZDBC SQL type</returns>
    function GetColumnType(ColumnIndex: Integer): TZSQLType;
    /// <summary>Retrieves the designated column's database-specific type name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>type name used by the database or "" if not applicable. If the
    ///  column type is a user-defined type, then a fully-qualified type name is
    ///  returned.</returns>
    function GetColumnTypeName(ColumnIndex: Integer): string;
    /// <summary>Indicates whether the designated column is definitely not
    ///  writable.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsReadOnly(ColumnIndex: Integer): Boolean;
    /// <summary>Set the readonly state of a field. The value will be ignored
    ///  if the field is not writable.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the dml's.</param>
    procedure SetReadOnly(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Indicates whether it is possible for a write on the designated
    ///  column to succeed.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsWritable(ColumnIndex: Integer): Boolean;
    /// <summary>Indicates whether a write on the designated column will
    ///  definitely succeed.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean;
    /// <summary>Gets a default value for this field.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>a default value for this field.</returns>
    function GetDefaultValue(ColumnIndex: Integer): string;
    /// <summary>Finds whether this field has a default value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if this field has a default value; <c>false</c>
    ///  otherwise.</returns>
    function HasDefaultValue(ColumnIndex: Integer): Boolean;
  end;

  TOnLobUpdate = procedure(Field: NativeInt) of object;
  /// <author>EgonHugeist</author>
  /// <summary>External or internal blob wrapper object.</summary>
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

  /// <summary>Defines the BLob interface.</summary>
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

    /// <summary>Clones this blob object.</summary>
    /// <param>"LobStreamMode" the mode the cloned object is used for is one of:
    ///  <c>lsmRead, lsmWrite, lsmReadWrite</c></param>
    /// <returns> a cloned blob object.</returns>
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

  /// <author>EgonHugeist</author>
  /// <summary>Defines the CLob interface.</summary>
  IZClob = interface(IZBlob)
    ['{2E0ED2FE-5F9F-4752-ADCB-EFE92E39FF94}']
    function GetStream(CodePage: Word): TStream; overload;
    procedure SetStream(const Value: TStream; CodePage: Word); overload;
  end;

  PILob = ^IZlob;
  IZLobDynArray = array of IZLob;

  PIZLob = ^IZBlob;
  IZBLobDynArray = array of IZBLob;

  PICLob = ^IZClob;
  IZCLobDynArray = array of IZCLob;

  /// <summary>Defines the Database notification interface.</summary>
  IZNotification = interface(IZInterface)
    ['{BF785C71-EBE9-4145-8DAE-40674E45EF6F}']

    function GetEvent: string;
    procedure Listen;
    procedure Unlisten;
    procedure DoNotify;
    function CheckEvents: string;
    /// <summary>Returns the <c>Connection</c> interface
    ///  that produced this <c>Notification</c> object.</summary>
    /// <returns>the connection that produced this Notification.</returns>
    function GetConnection: IZConnection;
  end;

  /// <summary>Defines the Database sequence generator interface.</summary>
  IZSequence = interface(IZInterface)
    ['{A9A54FE5-0DBE-492F-8DA6-04AC5FCE779C}']
    /// <summary>Get the name of the sequence generator.</summary>
    /// <returns> a name of this sequence generator.</returns>
    function  GetName: string;
    /// <summary>Get the blocksize/increment_by value of the sequence generator.</summary>
    /// <returns>the increment_by value.</returns>
    function  GetBlockSize: Integer;
    /// <summary>Sets a name of the sequence generator..</summary>
    /// <param>"Value" a name of this sequence generator.</param>
    procedure SetName(const Value: string);
    /// <summary>Sets the block size for this sequence.</summary>
    /// <param>Value the block size.</param>
    procedure SetBlockSize(const Value: Integer);
    /// <summary>Gets the current value of the sequence</summary>
    /// <returns>the current unique key</returns>
    function  GetCurrentValue: Int64;
    /// <summary>Gets the next unique key generated by this sequence</summary>
    /// <returns>the next generated unique key</returns>
    function  GetNextValue: Int64;
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function  GetCurrentValueSQL: string;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function  GetNextValueSQL: string;
    /// <summary>Returns the <c>Connection</c> interface
    ///  that produced this <c>Sequence</c> object.</summary>
    /// <returns>the connection that produced this sequence.</returns>
    function  GetConnection: IZConnection;
  end;

var
  /// <summary>The common driver manager object.</summary>
  DriverManager: IZDriverManager;
  /// <summary>The global critical section.</summary>
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

constructor TZDriverManager.Create;
begin
  FDriversCS := TCriticalSection.Create;
  FLogCS := TCriticalSection.Create;
  FDrivers := TZCollection.Create;
  FLoggingListeners := TZCollection.Create;
  FGarbageCollector := TZCollection.Create;
  FHasLoggingListener := False;
end;

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
    Result := Driver.GetClientVersion(Url);
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

constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor EZSQLThrowable.CreateWithCodeAndStatus(ErrorCode: Integer;
  const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
  FStatusCode := StatusCode;
end;

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

