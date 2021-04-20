{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(DELPHI) and defined(MSWINDOWS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcConnection, ZPlainPostgreSqlDriver, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZCompatibility, ZClasses, ZSysUtils;

type

  {** Implements PostgreSQL Database Driver. }
  TZPostgreSQLDriver = class(TZAbstractDriver)
  public
    /// <summary>Constructs this object with default properties.</summary>
    constructor Create; override;
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
    function Connect(const Url: TZURL): IZConnection; override;
    /// <summary>Gets the driver's minor version number. Initially this should
    ///  be 0.</summary>
    /// <returns>this driver's minor version number.</returns>
    function GetMinorVersion: Integer; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer; override;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Defines a PostgreSQL specific connection. }
  IZPostgreSQLConnection = interface(IZConnection)
    ['{8E62EA93-5A49-4F20-928A-0EA44ABCE5DB}']
    /// <summary>Checks is oid should be treated as Large Object.</summary>
    /// <returns><c>True</c> if oid should represent a Large Object.</returns>
    function IsOidAsBlob: Boolean;
    function integer_datetimes: Boolean;
    /// <summary>Checks is bytea_output hex.</summary>
    /// <returns><c>True</c> if hex is set.</returns>
    function Is_bytea_output_hex: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: TZPostgreSQLPlainDriver;
    function GetPGconnAddress: PPGconn;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function EncodeBinary(Buf: Pointer; Len: Integer; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString; overload;
    function EscapeString(const FromChar: PAnsiChar; len: NativeUInt; Quoted: Boolean): RawByteString; overload;
    procedure RegisterTrashPreparedStmtName(const value: String);
    function ClientSettingsChanged: Boolean;
    function GetUndefinedVarcharAsStringLength: Integer;
    /// <summary>Checks if DataBaseMetaData should check FieldVisibility too.</summary>
    /// <returns><c>True</c> if user did set it.</returns>
    function CheckFieldVisibility: Boolean;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    procedure AddDomain2BaseTypeIfNotExists(DomainOID, BaseTypeOID: OID);
    function FindDomainBaseType(DomainOID: OID; out BaseTypeOID: OID): Boolean;
    procedure FillUnknownDomainOIDs;
    function GetTimeZoneOffset: Int64;
    procedure UpdateTimestampOffset;

    procedure GetBinaryEscapeString(Buf: Pointer; Len: LengthInt; out Result: RawByteString);
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString);
    function GetByteBufferAddress: PByteBuffer;
    /// <summary>Handle an error, a warning or a notice. Note: this method
    ///  should be called only if the status is in error or warning range.</summary>
    /// <param>"Status" the current status received by a call of any pq
    ///  interface method</param>
    /// <param>"LoggingCategory" the logging category used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"LogMessage" the logging message used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"Sender" the calling interface which may release the resources if
    ///  a connection loss happens</param>
    /// <param>"ResultHandle" a postgres result handle to optiain the error
    ///  informations and free the allocated memory before raising the exception.</param>
    procedure HandleErrorOrWarning(Status: TZPostgreSQLExecStatusType;
      LogCategory: TZLoggingCategory; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable; ResultHandle: TPGresult);
  end;

  PZPGDomain2BaseTypeMap = ^TZPGDomain2BaseTypeMap;
  TZPGDomain2BaseTypeMap = record
    DomainOID: OID; //the domain oid
    BaseTypeOID: OID; //the native underlaing oid
    Known: WordBool;
  end;

  TZOID2OIDMapList = class(TZSortedList)
  private
    fUnkownCount: Integer;
    function SortCompare(Item1, Item2: Pointer): Integer;
  public
    procedure AddIfNotExists(DomainOID, BaseTypeOID: OID);
    function GetOrAddBaseTypeOID(DomainOID: OID; out BaseTypeOID: OID): Boolean;
    procedure Clear; override;
  public
    property UnkownCount: Integer read fUnkownCount;
  end;

  TZPgNotificationEvent = procedure(const Event: string;
    ProcessID: Integer; Payload: string) of object;

  /// <summary>Defines a prostgres specific evnt listener</summary>
  IZPostgresEventListener = Interface(IZEventListener)
    ['{FBB89249-7C7B-4777-B64C-AFCDAAB50F66}']
    /// <summary>Set a on Notify event</summary>
    /// <param>"Value" the event to be used</param>
    procedure SetOnPgNotifyEvent(const Value: TZPgNotificationEvent);
    /// <summary>Set a timer interval to check the notifications in milli
    ///  seconds</summary>
    /// <param>"Value" the interval to be used</param>
    procedure SetListenerInterval(const Value: Cardinal);
  End;

  TZPostgresEventList = class(TZEventList)
  private
    FTimer: TZThreadTimer;
    FPgNotifyEvent: TZPgNotificationEvent;
  public
    constructor Create(Handler: TZOnEventHandler; TimerTick: TThreadMethod);
    destructor Destroy; override;
    property Timer: TZThreadTimer read FTimer;
  end;

  /// <summary>Implements PostgreSQL Database Connection.</summary>
  TZPostgreSQLConnection = class(TZAbstractSingleTxnConnection,
    IZConnection, IZPostgreSQLConnection, IZTransaction, IZEventListener,
    IZPostgresEventListener)
  private
    FUndefinedVarcharAsStringLength: Integer;
    Fconn: TPGconn;
    FTypeList: TStrings;
    FDomain2BaseTypMap: TZOID2OIDMapList;
    FOidAsBlob, Finteger_datetimes: Boolean;
    FServerMajorVersion: Integer;
    FServerMinorVersion: Integer;
    FServerSubVersion: Integer;
    FTimeZoneOffset: Int64;
    FNoticeProcessor: TPQnoticeProcessor;
    FNoticeReceiver: TPQnoticeReceiver;
    //a collection of statement handles that are not used anymore. These can be
    //safely deallocated upon the next transaction start or immediately if we
    //are in autocommit mode. See SF#137:
    FPreparedStatementTrashBin: TStrings;
    FProcedureTypesCache: TStrings;
    FClientSettingsChanged: Boolean;
    FIs_bytea_output_hex: Boolean;
    FCheckFieldVisibility: Boolean;
    fPlainDriver: TZPostgreSQLPlainDriver;
    FLastWarning: EZSQLWarning;
    FEventList: TZPostgresEventList;
    //FCreatedEventAllerterPtr: Pointer; //weak ref to self ptr
    function HasMinimumServerVersion(MajorVersion, MinorVersion, SubVersion: Integer): Boolean;
  protected { implement IZPostgreSQLConnection }
    function GetUndefinedVarcharAsStringLength: Integer;
    /// <summary>Builds a connection string for PostgreSQL.</summary>
    /// <returns>a built connection string.</returns>
    function BuildConnectStr: RawByteString;
    /// <summary>Deallocates pending prepared statements. This procedure is
    ///  intended for driver internal use only and should normally only be
    ///  called when in auto-commit mode. This either happens when unregistering
    ///  a prepared statement and being in auto-commit mode or when committing
    ///  or rolling back a transaction and before staring the next transaction
    ///  block.<summary>
    procedure DeallocatePreparedStatements;
    /// <summary>Loads a server major and minor version numbers.</summary>
    procedure LoadServerVersion;
    function EncodeBinary(const Value: RawByteString; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(Buf: Pointer; Len: Integer; Quoted: Boolean): RawByteString; overload;
    function EscapeString(const FromChar: PAnsiChar; len: NativeUInt; Quoted: Boolean): RawByteString; overload;
    procedure RegisterTrashPreparedStmtName(const value: String);
    function ClientSettingsChanged: Boolean;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
    procedure InternalClose; override;
    function GetServerSetting(const AName: RawByteString): string;
    procedure SetServerSetting(const AName, AValue: RawbyteString);
    procedure UpdateTimestampOffset;
    procedure CheckEvents;
  public
    procedure AfterConstruction; override;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
  public
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
    function PrepareCallWithParams(const Name: String; Params: TStrings):
      IZCallableStatement;
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
    /// <author>aehimself</author>
    /// <summary>Immediately abort any kind of operations on the server.</summary>
    /// <returns>0 if the operation is aborted; Non zero otherwise.</returns>
    function AbortOperation: Integer; override;
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
    procedure SetAutoCommit(Value: Boolean); override;
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean); override;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    /// <summary>Starts transaction support or saves the current transaction.
    ///  If the connection is closed, the connection will be opened.
    ///  If a transaction is underway a nested transaction or a savepoint will
    ///  be spawned. While the tranaction(s) is/are underway the AutoCommit
    ///  property is set to False. Ending up the transaction with a
    ///  commit/rollback the autocommit property will be restored if changing
    ///  the autocommit mode was triggered by a starttransaction call.</summary>
    /// <returns>Returns the current txn-level. 1 means a expicit transaction
    ///  was started. 2 means the transaction was saved. 3 means the previous
    ///  savepoint got saved too and so on.</returns>
    function StartTransaction: Integer;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);override;
    procedure CommitPrepared(const transactionid:string);override;
    procedure RollbackPrepared(const transactionid:string);override;
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  public  //implement IZPostgreSQLConnection
    /// <summary>Checks is oid should be treated as Large Object.</summary>
    /// <returns><c>True</c> if oid should represent a Large Object.</returns>
    function IsOidAsBlob: Boolean;
    /// <summary>Checks is bytea_output hex.</summary>
    /// <returns><c>True</c> if hex is set.</returns>
    function Is_bytea_output_hex: Boolean;
    function integer_datetimes: Boolean;
    /// <summary>Checks if DataBaseMetaData should check FieldVisibility too.</summary>
    /// <returns><c>True</c> if user did set it.</returns>
    function CheckFieldVisibility: Boolean;

    procedure AddDomain2BaseTypeIfNotExists(DomainOID, BaseTypeOID: OID);
    function FindDomainBaseType(DomainOID: OID; out BaseTypeOID: OID): Boolean;
    procedure FillUnknownDomainOIDs;
    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: TZPostgreSQLPlainDriver;
    function GetPGconnAddress: PPGconn;
    procedure HandleErrorOrWarning(Status: TZPostgreSQLExecStatusType;
      LogCategory: TZLoggingCategory; const LogMsg: SQLString;
      const Sender: IImmediatelyReleasable; ResultHandle: TPGresult);

    function GetHostVersion: Integer; override;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function GetServerSubVersion: Integer;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetTimeZoneOffset: Int64;
  public
    function PingServer: Integer; override;

    function EscapeString(const Value: RawByteString): RawByteString; overload; override;
    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    procedure GetBinaryEscapeString(Buf: Pointer; Len: LengthInt; out Result: RawByteString); overload;
    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload; override;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString); overload;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; override;

    {$IFDEF ZEOS_TEST_ONLY}
    constructor Create(const ZUrl: TZURL);
    {$ENDIF}
    /// <summary>Returns the first warning reported by calls on this Connection.</summary>
    /// <remarks>Subsequent warnings will be chained to this EZSQLWarning.</remarks>
    /// <returns>the first SQLWarning or nil.</returns>
    function GetWarnings: EZSQLWarning; override;
    /// <summary>Clears all warnings reported for this <c>Connection</c> object.
    ///  After a call to this method, the method <c>getWarnings</c> returns nil
    ///  until a new warning is reported for this Connection.</summary>
    procedure ClearWarnings; override;
    /// <summary>Returns the ServicerProvider for this connection.</summary>
    /// <returns>the ServerProvider</returns>
    function GetServerProvider: TZServerProvider; override;
    /// <summary>Get a generic event alerter object.</summary>
    /// <param>"Handler" an event handler which gets triggered if the event is received.</param>
    /// <param>"CloneConnection" if <c>True</c> a new connection will be spawned.</param>
    /// <param>"Options" a list of options, to setup the event alerter.</param>
    /// <returns>a the generic event alerter object as interface or nil.</returns>
    function GetEventListener(Handler: TZOnEventHandler; CloneConnection: Boolean;
      Options: TStrings): IZEventListener; override;
  public { implement IZEventListener }
    /// <summary>Returns the <c>Connection</c> object
    ///  that produced this <c>Notification</c> object.</summary>
    /// <returns>the connection that produced this EventListener.</returns>
    function GetConnection: IZConnection;
    /// <summary>Test if the <c>EventAllerter</c> is active</summary>
    /// <returns><c>true</c> if the EventAllerter is active.</returns>
    function IsListening: Boolean;
    /// <summary>Starts listening the events.</summary>
    /// <param>"EventNames" a list of event name to be listened.</param>
    /// <param>"Handler" an event handler which gets triggered if the event is received.</param>
    procedure Listen(const EventNames: TStrings; Handler: TZOnEventHandler);
    /// <summary>Triggers an event.</summary>
    procedure TriggerEvent(const Name: String);
    /// <summary>Stop listening the events and cleares the registered events.</summary>
    procedure Unlisten;
  public { implement IZPostgresEventListener}
    /// <summary>Set a on Notify event</summary>
    /// <param>"Value" the event to be used</param>
    procedure SetOnPgNotifyEvent(const Value: TZPgNotificationEvent);
    /// <summary>Set a timer interval to check the notifications in milli
    ///  seconds</summary>
    /// <param>"Value" the interval to be used</param>
    procedure SetListenerInterval(const Value: Cardinal);
  end;

  TZPostgresEventData = class(TZEventData)
  private
    fPayload: SQLString;
    FProcessID: Integer;
  public
    function ToString: string; override;
  public
    property Payload: String read fPayLoad;
    property ProcessID: Integer read FProcessID;
  end;

var
  {** The common driver manager object. }
  PostgreSQLDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  ZFastCode, ZMessages, ZDbcPostgreSqlStatement, ZDbcUtils,
  ZDbcPostgreSqlUtils, ZDbcPostgreSqlMetadata, ZPostgreSqlToken, ZDbcProperties,
  ZPostgreSqlAnalyser, ZEncoding, ZDbcMetadata;

const
  cBegin: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'BEGIN';
  cCommit: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'COMMIT';
  cRollback: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'ROLLBACK';

procedure NoticeProcessorDispatcher(arg: Pointer; message: PAnsiChar); cdecl;
var L: NativeUInt;
    Msg: SQLString;
begin
  if message = nil then
    Exit;
  L := StrLen(message);
  {$IFDEF UNICODE}
  Msg := PRawToUnicode(message, L, TZPostgreSQLConnection(Arg).ConSettings.ClientCodePage.CP);
  {$ELSE}
  Msg := '';
  ZSetString(message, L, Msg {$IFDEF WITH_RAWBYTESTRING}, TZPostgreSQLConnection(Arg).ConSettings.ClientCodePage.CP{$ENDIF});
  {$ENDIF}
  TZPostgreSQLConnection(Arg).HandleErrorOrWarning(PGRES_NONFATAL_ERROR,
    lcOther, Msg, nil, nil);
end;

procedure NoticeReceiverDispatcher(arg: Pointer; res: TPGResult); cdecl;
begin
  TZPostgreSQLConnection(Arg).HandleErrorOrWarning(PGRES_NONFATAL_ERROR,
    lcOther, 'Postgres Notice or warning', nil, res);
  TZPostgreSQLConnection(Arg).FPlainDriver.PQclear(res);
end;

{ TZPostgreSQLDriver }

constructor TZPostgreSQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZPostgreSQLPlainDriver.Create));
end;

function TZPostgreSQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZPostgreSQLConnection.Create(Url);
end;

function TZPostgreSQLDriver.GetMinorVersion: Integer;
begin
  Result := 3;
end;

function TZPostgreSQLDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZPostgreSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

function TZPostgreSQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZPostgreSQLStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZPostgreSQLConnection }

function TZPostgreSQLConnection.GetUndefinedVarcharAsStringLength: Integer;
begin
  Result := FUndefinedVarcharAsStringLength;
end;

function TZPostgreSQLConnection.GetWarnings: EZSQLWarning;
begin
  Result := FLastWarning;
end;

const
  TPG_DIAG_ErrorFieldPrevixes: Array[TZPostgreSQLFieldCode] of RawByteString = (
    '', ' ', LineEnding, LineEnding+'detail: ', '', LineEnding+'position: ', ' ',
    ' ', ' ', LineEnding+'source file: ',
    LineEnding+'line: ', lineEnding+'funtion: '
  );
procedure TZPostgreSQLConnection.HandleErrorOrWarning(
  Status: TZPostgreSQLExecStatusType;
  LogCategory: TZLoggingCategory; const LogMsg: SQLString;
  const Sender: IImmediatelyReleasable; ResultHandle: TPGresult);
var I: TZPostgreSQLFieldCode;
    aErrorStatus, FormatStr: String;
    rawMsg: RawByteString;
    P: PAnsiChar;
    L: NativeUInt;
    SQLWriter: TZRawSQLStringWriter;
    {$IFNDEF UNICODE}excCP,{$ENDIF}msgCP: Word;
    Error: EZSQLThrowable;
begin
  P := FPlainDriver.PQerrorMessage(Fconn);
  if (P = nil) or (P^ = #0) then Exit;
  L := ZFastCode.StrLen(P);
  rawMsg := '';
  SQLWriter := TZRawSQLStringWriter.Create(1024+Length(LogMsg));
  if (ConSettings <> nil) and (ConSettings.ClientCodePage <> nil)
  then msgCP := ConSettings.ClientCodePage.CP
  else msgCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
{$IFNDEF UNICODE}
  excCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
      {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
{$ENDIF}
  try
    SQLWriter.AddText(P, L, rawMsg);
    aErrorStatus := '';
    if Assigned(ResultHandle) then begin
      if Assigned(FPlainDriver.PQresultErrorField) then //since 7.3
        for i := low(TZPostgreSQLFieldCode) to {$IFDEF DEBUG}high(TZPostgreSQLFieldCode){$ELSE}pgdiagCONTEXT{$ENDIF} do begin
          P := FPlainDriver.PQresultErrorField(ResultHandle,TPG_DIAG_ErrorFieldCodes[i]);
          if P <> nil then begin
            L := ZFastCode.StrLen(P);
            ZSysUtils.Trim(L, P);
            if L > 0 then begin
              SQLWriter.AddText(TPG_DIAG_ErrorFieldPrevixes[I], rawMsg);
              SQLWriter.AddText(P, L, rawMsg);
              if i = pgdiagSQLSTATE then
                {$IFDEF UNICODE}
                aErrorStatus := ASCII7ToUnicodeString(P, L);
                {$ELSE}
                System.SetString(aErrorStatus, P, L);
                {$ENDIF}
            end;
          end;
        end;
      if Status <> PGRES_NONFATAL_ERROR then //just a warning
        FPlainDriver.PQclear(ResultHandle);
    end;
    SQLWriter.Finalize(rawMsg);
  finally
    FreeAndNil(SQLWriter);
  end;
  Error := nil;
  if (rawMsg <> '') then
    {$IFDEF UNICODE}
    FLogMessage := ZRawToUnicode(rawMsg, msgCP);
    {$ELSE}
    if excCP <> msgCP
    then PRawToRawConvert(Pointer(rawMsg), Length(rawMsg), msgCP, excCP, FLogMessage)
    else FLogMessage := rawMsg;
    {$ENDIF}
  if DriverManager.HasLoggingListener then
    if (Status = PGRES_FATAL_ERROR)
    then LogError(LogCategory, 0, Sender, LogMsg, FLogMessage)
    else DriverManager.LogMessage(LogCategory, URL.Protocol, FLogMessage);
  if AddLogMsgToExceptionOrWarningMsg and (LogMsg <> '') then
    if LogCategory in [lcExecute, lcTransaction, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;

  if AddLogMsgToExceptionOrWarningMsg and (LogMsg <> '')
  then FLogMessage := Format(FormatStr, [FLogMessage, Ord(Status), LogMsg])
  else FLogMessage := Format(FormatStr, [FLogMessage, Ord(Status)]);

  if (FPlainDriver.PQstatus(Fconn) = CONNECTION_BAD) and (LogCategory <> lcConnect) then begin
    Error := EZSQLConnectionLost.CreateWithCodeAndStatus(Ord(CONNECTION_BAD), aErrorStatus, FLogMessage);
    if Assigned(Sender)
    then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
    else ReleaseImmediat(Sender, EZSQLConnectionLost(Error));
  end else if LogCategory <> lcUnprepStmt then //silence -> https://sourceforge.net/p/zeoslib/tickets/246/
    if Status = PGRES_FATAL_ERROR
    then Error := EZSQLException.CreateWithStatus(aErrorStatus, FLogMessage)
    else begin //that's a notice propably
      ClearWarnings;
      Error := EZSQLWarning.CreateWithStatus(aErrorStatus, FLogMessage);
      if not RaiseWarnings then begin
        FLastWarning := EZSQLWarning(Error);
        Error := nil;
      end;
    end;
  if Error <> nil then
    raise Error;
end;

function TZPostgreSQLConnection.HasMinimumServerVersion(MajorVersion,
  MinorVersion, SubVersion: Integer): Boolean;
begin
  Result := MajorVersion <= GetServerMajorVersion;
  if Result and (MajorVersion = GetServerMajorVersion) then begin
    Result := MinorVersion <= GetServerMinorVersion;
    if Result and (MinorVersion = GetServerMinorVersion) then
      Result := SubVersion <= GetServerSubVersion;
  end;
end;

function TZPostgreSQLConnection.integer_datetimes: Boolean;
begin
  Result := Finteger_datetimes;
end;

destructor TZPostgreSQLConnection.Destroy;
begin
  if FTypeList <> nil then
    FreeAndNil(FTypeList);
  if FEventList <> nil then
    FreeAndNil(FEventList);
  inherited Destroy;
  FreeAndNil(FPreparedStatementTrashBin);
  FreeAndNil(FProcedureTypesCache);
  FreeAndNil(FDomain2BaseTypMap);
end;

function TZPostgreSQLConnection.AbortOperation: Integer;
var
  pCancel: PGCancel;
  L: NativeUInt;
  P: PAnsiChar;
begin
  {.$MESSAGE '.AbortOperation with PostgreSQL is untested and might cause unexpected results!'}
  // https://www.postgresql.org/docs/9.2/libpq-cancel.html
  Result := 1;
  pCancel := FPlainDriver.PQgetCancel(FConn);
  if pCancel <> nil then try
    // 0 - error, 1 - success
    P := @FByteBuffer[0];
    PByte(P)^ := 0;
    if FPlainDriver.PQcancel(pCancel, P, SizeOf(TByteBuffer)-1) = 1 then
      Result := 0
    else begin
      L := ZFastCode.StrLen(P);
      //as documented if cancel fails, because the server was ready inbetween,
      //no visible Result is returned so we tag this fail as an success
      if L = 0
      then Result := 0
      else begin
        {$IFDEF UNICODE}
        PRawToUnicode(P,L,ConSettings.ClientCodePage.CP, FLogMessage);
        {$ELSE}
        ZSetString(P, L, FLogMessage);
        {$ENDIF}
        HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, FLogMessage, nil, Self);
      end;
    end;
  finally
    FPlainDriver.PQfreeCancel(pcancel);
  end;
end;

procedure TZPostgreSQLConnection.AddDomain2BaseTypeIfNotExists(DomainOID,
  BaseTypeOID: OID);
begin
  FDomain2BaseTypMap.AddIfNotExists(DomainOID, BaseTypeOID);
end;

procedure TZPostgreSQLConnection.AfterConstruction;
begin
  FMetaData := TZPostgreSQLDatabaseMetadata.Create(Self, Url);
  FPlainDriver := PlainDriver.GetInstance as TZPostgreSQLPlainDriver;
  inherited AfterConstruction;
  FProcedureTypesCache := TStringList.Create;
  FPreparedStatementTrashBin := nil;
  FDomain2BaseTypMap := TZOID2OIDMapList.Create;
  { Sets a default PostgreSQL port }
  if Self.Port = 0 then
     Self.Port := 5432;
  inherited SetTransactionIsolation(tiReadCommitted);

  { Processes connection properties. }
  FOidAsBlob := StrToBoolEx(Info.Values[DSProps_OidAsBlob]);
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values[DSProps_UndefVarcharAsStringLength], 0);
  FCheckFieldVisibility := StrToBoolEx(Info.Values[ConnProps_CheckFieldVisibility]);

  FNoticeProcessor := NoticeProcessorDispatcher;
  FNoticeReceiver  := NoticeReceiverDispatcher;
end;

function TZPostgreSQLConnection.BuildConnectStr: RawByteString;
var
  ConnectTimeout, Cnt: Integer;
  SQLWriter: TZRawSQLStringWriter;
  //parameters should be separated by whitespace
  procedure AddParamToResult(const AParam: RawByteString;
    const AValue: String);
  begin
    if Cnt > 0 then
      SQLWriter.AddChar(AnsiChar(' '), Result);
    SQLWriter.AddText(AParam, Result);
    SQLWriter.AddChar(AnsiChar('='), Result);
    // backslashes and single quotes must be escaped with backslashes
    SQLWriter.AddTextQuoted(EncodeCString({$IFDEF UNICODE}RawByteString{$ENDIF}(AValue)), AnsiChar(#39), Result);
    Inc(Cnt);
  end;
begin
  //Init the result to empty string.
  Result := '';
  Cnt := 0;
  SQLWriter := TZRawSQLStringWriter.Create(512);
  //Entering parameters from the ZConnection
  If IsIpAddr(HostName) then
    AddParamToResult('hostaddr', HostName)
  else
    AddParamToResult('host', HostName);

  AddParamToResult('port', ZFastCode.IntToStr(Port));
  AddParamToResult('dbname', Database);
  if user <> '' then begin
    AddParamToResult('user', User);
    AddParamToResult('password', Password);
  end;

  If Info.Values[ConnProps_SSLMode] <> ''
    // the client (>= 7.3) sets the ssl mode for this connection
    // (possible values are: require, prefer, allow, disable)
  then AddParamToResult(ConnProps_SSLMode, Info.Values[ConnProps_SSLMode])
  else if Info.Values[ConnProps_RequireSSL] <> ''
    // the client (< 7.3) sets the ssl encription for this connection
    // (possible values are: 0,1)
  then AddParamToResult(ConnProps_RequireSSL, Info.Values[ConnProps_RequireSSL]);

  if Info.Values[ConnProps_SSLCompression] <> '' then AddParamToResult(ConnProps_SSLCompression, Info.Values[ConnProps_SSLCompression]);
  if Info.Values[ConnProps_SSLCert] <> '' then AddParamToResult(ConnProps_SSLCert, Info.Values[ConnProps_SSLCert]);
  if Info.Values[ConnProps_SSLKey] <> '' then AddParamToResult(ConnProps_SSLKey, Info.Values[ConnProps_SSLKey]);
  if Info.Values[ConnProps_SSLRootcert] <> '' then AddParamToResult(ConnProps_SSLRootcert, Info.Values[ConnProps_SSLRootcert]);
  if Info.Values[ConnProps_SSLCrl] <> '' then AddParamToResult(ConnProps_SSLCrl, Info.Values[ConnProps_SSLCrl]);
  { tcp keepalives by Luca Olivetti }
  if Info.Values[ConnProps_keepalives] <> '' then AddParamToResult(ConnProps_keepalives,Info.Values[ConnProps_keepalives]);
  if Info.Values[ConnProps_keepalives_idle] <> '' then AddParamToResult(ConnProps_keepalives_idle,Info.Values[ConnProps_keepalives_idle]);
  if Info.Values[ConnProps_keepalives_interval] <> '' then AddParamToResult(ConnProps_keepalives_interval,Info.Values[ConnProps_keepalives_interval]);
  if Info.Values[ConnProps_keepalives_count] <> '' then AddParamToResult(ConnProps_keepalives_count,Info.Values[ConnProps_keepalives_count]);

  { Sets a connection timeout. }
  ConnectTimeout := StrToIntDef(Info.Values[ConnProps_Timeout], -1);
  if ConnectTimeout >= 0 then
    AddParamToResult('connect_timeout', ZFastCode.IntToStr(ConnectTimeout));

  { Sets the application name }
  if Info.Values[ConnProps_ApplicationName] <> '' then
    AddParamToResult(ConnProps_ApplicationName, Info.Values[ConnProps_ApplicationName]);
  SQLWriter.Finalize(Result);
  FreeAndNil(SQLWriter);
end;

function TZPostgreSQLConnection.IsListening: Boolean;
begin
  Result := not IsClosed and (FCreatedWeakEventListenerPtr <> nil) and (FEventList.Count > 0) and FEventList.Timer.Enabled;
end;

function TZPostgreSQLConnection.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

function TZPostgreSQLConnection.Is_bytea_output_hex: Boolean;
begin
  Result := FIs_bytea_output_hex;
end;

procedure TZPostgreSQLConnection.CheckEvents;
var Notify: PZPostgreSQLNotify;
    i: NativeInt;
    ListenEvent: PZEvent;
    AEvent: TZPostgresEventData;
    Handler: TZOnEventHandler;
    {$IF defined(UNICODE) or defined(WITH_RAWBYTESTRING)}
    CP: Word;
    {$IFEND}
    PayLoad: String;
    relname: String;
    ProcessID: Integer;
begin
  if not IsClosed and (FCreatedWeakEventListenerPtr <> nil) then begin
    {$IF defined(UNICODE) or defined(WITH_RAWBYTESTRING)}
    CP := Consettings.ClientCodePage.CP;
    {$IFEND}
    PayLoad := '';
    relname := '';
    while FPlainDriver.PQisBusy(Fconn) = 1 do //see: https://sourceforge.net/p/zeoslib/tickets/475/
      Sleep(1);
    if FPlainDriver.PQconsumeInput(Fconn)=1 then
      while True do begin
        Notify := PZPostgreSQLNotify(FPlainDriver.PQnotifies(Fconn));
        if Notify = nil then
          Break;
        try
          if Notify.relname <> nil then
            {$IFDEF UNICODE}
            PRawToUnicode(Notify.relname, StrLen(Notify.relname), CP, relname)
            {$ELSE}
            ZSetString(Notify.relname, StrLen(Notify.relname), RawByteString(relname){$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
            {$ENDIF}
          else relname := '';
          if Notify.Payload <> nil then
            {$IFDEF UNICODE}
            PRawToUnicode(Notify.payload, StrLen(Notify.payload), CP, Payload)
            {$ELSE}
            ZSetString(Notify.payload, StrLen(Notify.payload), RawByteString(Payload){$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
            {$ENDIF}
          else payload := '';
          ProcessID := Notify.be_pid;
        finally
          FPlainDriver.PQFreemem(Notify);
        end;
        if (FEventList <> nil) then
          if Assigned(FEventList.FPgNotifyEvent)
          then FEventList.FPgNotifyEvent(relname, ProcessID, Payload)
          else try
            AEvent := TZPostgresEventData.Create;
            AEvent.FProcessID := ProcessID;
            AEvent.fName := relname;
            AEvent.fPayload := Payload;
            AEvent.fEventState := esSignaled;
            Handler := nil;
            if FEventList <> nil then begin
              Handler := nil;
              for I := 0 to FEventList.Count -1 do begin
                ListenEvent := FEventList[i];
                if ListenEvent.Name = AEvent.fName then begin
                  Handler := ListenEvent.Handler;
                  Break;
                end;
              end;
              if not Assigned(Handler) then
                Handler := FEventList.Handler;
              Handler(TZEventData(AEvent));
            end;
          finally
            if AEvent <> nil then
              FreeAndNil(AEvent);
          end;
      end;
  end;
end;

function TZPostgreSQLConnection.CheckFieldVisibility: Boolean;
begin
  Result := FCheckFieldVisibility;
end;

procedure TZPostgreSQLConnection.DeallocatePreparedStatements;
var
  SQL: RawByteString;
  x: Integer;
begin
  if Assigned(FPreparedStatementTrashBin) and (Fconn <> nil) and (FPreparedStatementTrashBin.Count > 0) then
    try
      for x := FPreparedStatementTrashBin.Count - 1 downto 0 do begin
        SQL := 'DEALLOCATE "' + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FPreparedStatementTrashBin.Strings[x]) + '";';;
        ExecuteImmediat(SQL, lcUnprepStmt);
      end;
    finally
      FPreparedStatementTrashBin.Clear;
    end;
end;

{**
  Encodes a Binary-AnsiString to a PostgreSQL format
  @param Value the Binary String
  @result the encoded String
}
function TZPostgreSQLConnection.EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString;
begin
  Result := EncodeBinary(Pointer(Value), Length(Value), Quoted);
end;

{**
  Encodes a Binary-AnsiString to a PostgreSQL format
  @param Value the Binary String
  @result the encoded String
}
function TZPostgreSQLConnection.EncodeBinary(const Value: RawByteString; Quoted: Boolean): RawByteString;
begin
  Result := EncodeBinary(Pointer(Value), Length(Value), Quoted);
end;

procedure TZPostgreSQLConnection.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  if Assigned(FPreparedStatementTrashBin) then
    FPreparedStatementTrashBin.Clear;
  Fconn := nil;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZPostgreSQLConnection.RegisterTrashPreparedStmtName(const value: String);
begin
  if FPreparedStatementTrashBin.IndexOf(Value) = -1 then
    FPreparedStatementTrashBin.Add(Value);
end;

procedure TZPostgreSQLConnection.ClearWarnings;
begin
  FreeAndNil(FLastWarning);
end;

function TZPostgreSQLConnection.ClientSettingsChanged: Boolean;
begin
  Result := FClientSettingsChanged;
end;

procedure TZPostgreSQLConnection.Open;

var
  SCS, Temp: string;
  {aport, apwd, ahost, aUser,} adb: RawByteString;
begin
  if not Closed then
    Exit;

  { Connect to PostgreSQL database. }
  adb := BuildConnectStr;
  Fconn := FPlainDriver.PQconnectdb(Pointer(adb));
  FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);
  try
    if FPlainDriver.PQstatus(Fconn) = CONNECTION_BAD then
      HandleErrorOrWarning(PGRES_FATAL_ERROR, lcConnect, FLogMessage, Self, nil)
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);

    if Assigned(FPlainDriver.PQsetNoticeReceivcer)
    { Set the notice processor (default = nil)}
    then FPlainDriver.PQsetNoticeReceivcer(Fconn, FNoticeReceiver, Pointer(Self))
    { Set the notice processor (default = nil)}
    else FPlainDriver.PQsetNoticeProcessor(Fconn,FNoticeProcessor, Pointer(Self));

    { Gets the current codepage }
    Temp := GetPlainDriver.ValidateCharEncoding(FPlainDriver.PQclientEncoding(Fconn)).Name;

    { Sets a client codepage if necessary }
    if ( FClientCodePage <> '' ) and (Temp <> FClientCodePage) then
      SetServerSetting('CLIENT_ENCODING', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FClientCodePage));

    inherited Open;

    SetTransactionIsolation(GetTransactionIsolation);
    if ReadOnly then begin
      ReadOnly := False;
      SetReadOnly(True);
    end;
    if not AutoCommit then begin
      AutoCommit := True;
      SetAutoCommit(False);
    end;

    { Gets the current codepage if it wasn't set..}
    if ( FClientCodePage = '') then
      CheckCharEncoding(Temp)
    else
    begin
      CheckCharEncoding(FClientCodePage);
      FClientSettingsChanged := True;
    end;

    if FPreparedStatementTrashBin = nil then
      FPreparedStatementTrashBin := TStringList.Create;

    { sets standard_conforming_strings according to Properties if available }
    SCS := Info.Values[ConnProps_StdConformingStrings];
    if (SCS <> '') then begin
      SetServerSetting(ConnProps_StdConformingStrings, {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SCS));
      FClientSettingsChanged := True;
    end;
    {$IFDEF USE_SYNCOMMONS}
    SetServerSetting('DateStyle', 'ISO');
    {$ENDIF}
    Finteger_datetimes := StrToBoolEx(GetServerSetting(#39+ConnProps_integer_datetimes+#39));
    FIs_bytea_output_hex := UpperCase(GetServerSetting('''bytea_output''')) = 'HEX';
    UpdateTimestampOffset;
  finally
    if self.IsClosed and (Self.Fconn <> nil) then
    begin
      FPlainDriver.PQFinish(Fconn);
      Fconn := nil;
    end;
  end;
end;

function TZPostgreSQLConnection.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZPostgreSQLCallableStatement.Create(Self, Name, Params);
end;

function TZPostgreSQLConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if (GetServerMajorVersion >= 8) and Assigned(FplainDriver.PQexecParams)
  then Result := TZPostgreSQLPreparedStatementV3.Create(Self, SQL, Info)
  else Result := TZPostgrePreparedStatementV2.Create(Self, SQL, Info)
end;

procedure TZPostgreSQLConnection.PrepareTransaction(const transactionid: string);
var SQL: SQLString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  SQL :='PREPARE TRANSACTION '''+copy(transactionid,1,200)+'''';
  ExecuteImmediat(SQL, lcTransaction);
  ExecuteImmediat(cBegin, lcTransaction);
  AutoCommit := False;
end;

function TZPostgreSQLConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZPostgreSQLStatement.Create(Self, Info);
end;

procedure TZPostgreSQLConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      ExecuteImmediat(cCommit, lcTransaction);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

procedure TZPostgreSQLConnection.SetListenerInterval(const Value: Cardinal);
begin
  if FEventList <> nil then
    FEventList.FTimer.Interval := Value
  else raise EZSQLException.Create('No active listener acquired');
end;

procedure TZPostgreSQLConnection.SetOnPgNotifyEvent(
  const Value: TZPgNotificationEvent);
begin
  if FEventList <> nil then
    FEventList.FPgNotifyEvent := Value
  else raise EZSQLException.Create('No active listener acquired');
end;

procedure TZPostgreSQLConnection.Commit;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  if FSavePoints.Count > 0 then begin
    S := cReleaseSP+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
    DeallocatePreparedStatements;
  end else begin
    ExecuteImmediat(cCommit, lcTransaction);
    AutoCommit := True;
    DeallocatePreparedStatements;
    if FRestartTransaction then
      StartTransaction;
  end
end;

{**
  Commits a prepared transaction in a 2-Phase commit.
  This method should be used only when in auto-commit mode.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.CommitPrepared(const transactionid: string);
var SQL: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  SQL := 'COMMIT PREPARED '''+copy(RawByteString(transactionid),1,200)+'''';
  ExecuteImmediat(SQL, lcTransaction);
end;

procedure TZPostgreSQLConnection.Rollback;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := cRollbackTo+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
    DeallocatePreparedStatements;
  end else begin
    ExecuteImmediat(cRollback, lcTransaction);
    AutoCommit := True;
    DeallocatePreparedStatements;
    if FRestartTransaction then
      StartTransaction;
  end;
end;


{**
  Rolls back a transaction that was prepared for 2-Phase commit.
  This method can only be used when auto-commit is enabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.RollbackPrepared(const transactionid: string);
var SQL: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  SQL := 'ROLLBACK PREPARED '''+copy(RawByteString(transactionid),1,200)+'''';
  ExecuteImmediat(SQL, lcTransaction);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZPostgreSQLConnection.InternalClose;
var
  LogMessage: SQLString;
  QueryHandle: TPGresult;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  //see https://sourceforge.net/p/zeoslib/tickets/246/
  FSavePoints.Clear;
  if FEventList <> nil then
    FreeAndNil(FEventList);

  try
    if not AutoCommit then begin //try to rollback
      AutoCommit := not FRestartTransaction;
      QueryHandle := FPlainDriver.PQexec(Fconn, Pointer(cRollBack));
      if QueryHandle <> nil then
        FPlainDriver.PQclear(QueryHandle); //raise no exception
    end;
  finally
    try
      DeallocatePreparedStatements;
    finally
      if Fconn <> nil then
        FPlainDriver.PQFinish(Fconn);
      Fconn := nil;
      LogMessage := 'DISCONNECT FROM "'+URL.Database+'"';
      DriverManager.LogMessage(lcDisconnect, URL.Protocol, LogMessage);
    end;
  end;
end;

const cROTxn: array[Boolean] of RawByteString = (' READ WRITE', ' READ ONLY');
{**
  Sets a new transact isolation level. tiNone, tiReadUncommitted
  will be mapped to tiReadCommitted since PostgreSQL will treat
  them the same anyway.
  For Versions earlier than 8.0 tiRepeatableRead will be mapped
  to tiSerializable since versions priot to 8.0 don't support it.
  @param Level a new transact isolation level.
}
procedure TZPostgreSQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var SQL: RawByteString;
begin
  if (Level = tiNone) or (Level = tiReadUncommitted) then
    Level := tiReadCommitted;

  if Level <>  TransactIsolationLevel then begin
    if not IsClosed then begin
      SQL := RawByteString('SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL ');
      case level of
        tiRepeatableRead: if (GetServerMajorVersion >= 8)
              then SQL := SQL + RawByteString('REPEATABLE READ')
              else SQL := SQL + RawByteString('SERIALIZABLE');
        tiSerializable:
              SQL := SQL + RawByteString('SERIALIZABLE');
        else  SQL := SQL + RawByteString('READ COMMITTED');
      end;
      SQL := SQL + cROTxn[ReadOnly];
      ExecuteImmediat(SQL, lcTransaction);
    end;
    TransactIsolationLevel := Level;
  end;
end;

function TZPostgreSQLConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    ExecuteImmediat(cBegin, lcTransaction);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count); //PG also has problems with numbered tokens..
    ExecuteImmediat(cSavePoint+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
end;

function TZPostgreSQLConnection.StoredProcedureIsSelectable(
  const ProcName: String): Boolean;
var I: Integer;
  function AddToCache(const ProcName: String): Boolean;
  var RS: IZResultSet;
    //Stmt: IZStatement;
    Catalog, Schema, ObjName: String;
  begin
    Result := True;
    if GetServerMajorVersion < 11 then
      Exit;
    with GetMetadata do begin
      with GetDatabaseInfo do
      SplitQualifiedObjectName(ProcName, SupportsCatalogsInProcedureCalls,
        SupportsSchemasInProcedureCalls, Catalog, Schema, ObjName);
      Schema := AddEscapeCharToWildcards(Schema);
      ObjName := AddEscapeCharToWildcards(ObjName);
      if UseMetadata then with GetMetadata do begin
        RS := GetProcedures(Catalog, Schema, ObjName);
        if RS.Next then
          Result := RS.GetInt(ProcedureTypeIndex) = ProcedureReturnsResult;
      end;
    end;
    FProcedureTypesCache.AddObject(ProcName, TObject(Ord(Result)));
  end;
begin
  I := FProcedureTypesCache.IndexOf(ProcName);
  if I = -1
  then Result := AddToCache(ProcName)
  else Result := FProcedureTypesCache.Objects[I] <> nil;
end;

procedure TZPostgreSQLConnection.TriggerEvent(const Name: String);
var RawTemp: RawByteString;
begin
  {$IFDEF UNICODE}
  RawTemp := 'notify '+ZUnicodeToRaw(Name, ConSettings.ClientCodePage.CP);
  {$ELSE}
  RawTemp := 'notify '+Name;
  {$ENDIF}
  ExecuteImmediat(RawTemp, lcExecute);
end;

procedure TZPostgreSQLConnection.Unlisten;
var I: NativeInt;
    RawTemp: RawByteString;
    Event: PZEvent;
begin
  if (FEventList <> nil) and (FEventList.Count > 0) then begin
    for i := FEventList.Count -1 downto 0 do begin
      Event := FEventList[I];
      {$IFDEF UNICODE}
      RawTemp := 'unlisten '+ZUnicodeToRaw(Event.Name, ConSettings.ClientCodePage.CP);
      {$ELSE}
      RawTemp := 'unlisten '+Event.Name;
      {$ENDIF}
      ExecuteImmediat(RawTemp, lcExecute);
      FEventList.Delete(I);
    end;
    FEventList.Timer.Enabled := False;
  end else
    raise EZSQLException.Create('no events registered');
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZPostgreSQLConnection.UpdateTimestampOffset;
var ANow: TDateTime;
    SQL: RawByteString;
    L: LengthInt;
    QueryHandle: TPGresult;
    Status: TZPostgreSQLExecStatusType;
    TS: TZTimestamp;
    P: Pointer;
    i64, i64Tz: Int64;
    Dbl, dblTz: Double;
begin
  if Closed then
    Exit;
  if not Assigned(FPlainDriver.PQexecParams) then begin
    FTimeZoneOffset := 0;
    Exit;
  end;
  ANow := Now;
  DecodeDateTimeToTimeStamp(ANow, TS);
  TS.Second := 0;
  TS.Fractions := 0;
  L := ZSysUtils.DateTimeToRaw(TS.Year, TS.Month, TS.Day, TS.Hour,
    TS.Minute, 0, 0, @fByteBuffer[0], DefDateTimeFormatYMD, True, False);
  SQL := '';
  System.SetString(SQL, PAnsiChar(@fByteBuffer[0]), L);

  SQL := 'SELECT '+SQL+'::TIMESTAMPTZ';
  {$IFDEF UNICODE}
  Ascii7ToUnicodeString(Pointer(SQL), Length(SQL), FLogMessage);
  {$ENDIF}
  QueryHandle := FPlainDriver.PQexecParams(Fconn, Pointer(SQL), 0,
    nil, nil, nil, nil, ParamFormatBin);
  try
    Status := FPlainDriver.PQresultStatus(QueryHandle);
    if Status = PGRES_TUPLES_OK then begin
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute, URL.Protocol, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF});
      P := FPlainDriver.PQgetvalue(QueryHandle, 0, 0);
      if Finteger_datetimes then begin
        TimeStamp2PG(TS, i64);
        {$IFNDEF ENDIAN_BIG}
        Reverse8Bytes(@i64);
        {$ENDIF}
        i64tz := PG2Int64(P);
        FTimeZoneOffSet := i64-i64tz;
      end else begin
        TimeStamp2PG(TS, dbl);
        {$IFNDEF ENDIAN_BIG}
        Reverse8Bytes(@dbl);
        {$ENDIF}
        dblTz := PG2Double(P);
        FTimeZoneOffSet := Trunc(dbl-dblTz);
      end;
    end else
      HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecute, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self, QueryHandle);
  finally
    if QueryHandle <> nil then
      FPlainDriver.PQclear(QueryHandle);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZPostgreSQLConnection.GetBinaryEscapeString(Buf: Pointer;
  Len: LengthInt; out Result: RawByteString);
begin
  Result := EncodeBinary(Buf, Len, True)
end;

function TZPostgreSQLConnection.GetConnection: IZConnection;
begin
  Result := Self;
end;

{**
  Gets a reference to PostgreSQL connection handle.
  @return a reference to PostgreSQL connection handle.
}
function TZPostgreSQLConnection.GetPGconnAddress: PPGconn;
begin
  Result := @Fconn;
end;

procedure TZPostgreSQLConnection.GetEscapeString(Buf: PAnsichar; Len: LengthInt;
  out Result: RawByteString);
begin
  Result := EscapeString(Buf, Len, True)
end;

{**
  Gets a PostgreSQL plain driver interface.
  @return a PostgreSQL plain driver interface.
}
function TZPostgreSQLConnection.GetPlainDriver: TZPostgreSQLPlainDriver;
begin
  Result := FPlainDriver;
end;

function TZPostgreSQLConnection.GetTimeZoneOffset: Int64;
begin
  Result := FTimeZoneOffset;
end;

function TZPostgreSQLConnection.GetTokenizer: IZTokenizer;
begin
  Result := TZPostgreSQLTokenizer.Create;
end;

{**
  Gets a type name by it's oid number.
  @param Id a type oid number.
  @return a type name or empty string if there was no such type found.
}
function TZPostgreSQLConnection.GetTypeNameByOid(Id: Oid): string;
var
  I, Index: Integer;
  QueryHandle: TPGresult;
  SQL: RawByteString;
  TypeCode, BaseTypeCode: Integer;
  TypeName: string;
  LastVersion: boolean;
  P: PAnsiChar;
  Status: TZPostgreSQLExecStatusType;
begin
  if Closed then
     Open;

  LastVersion := (GetServerMajorVersion < 7 ) or
    ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3));
  { Fill the list with existed types }
  if not Assigned(FTypeList) then begin
    if LastVersion
    then SQL := 'SELECT oid, typname FROM pg_type WHERE oid<10000'
    else SQL := 'SELECT oid, typname, typbasetype, typtype FROM pg_type' +
             ' WHERE (typtype = ''b'' and oid < 10000) OR typtype = ''p'' OR typtype = ''e'' OR typbasetype<>0 ORDER BY oid';

    QueryHandle := FPlainDriver.PQexec(Fconn, Pointer(SQL));
    Status := FPlainDriver.PQresultStatus(QueryHandle);
    {$IFDEF UNICODE}
    if DriverManager.HasLoggingListener then
      FLogMessage := ASCII7ToUnicodeString(SQL);
    {$ENDIF}
    if Status = PGRES_TUPLES_OK then begin
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute, URL.Protocol,
          {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF});

      FTypeList := TStringList.Create;
      for I := 0 to FPlainDriver.PQntuples(QueryHandle)-1 do begin
        TypeCode := RawToIntDef(FPlainDriver.PQgetvalue(QueryHandle, I, 0), 0);
        P := FPlainDriver.PQgetvalue(QueryHandle, I, 3);
        if (PByte(P)^ or $20) = ord('e') //lower 'E'
        then TypeName := 'enum'
        else begin
          P := FPlainDriver.PQgetvalue(QueryHandle, I, 1);
          {$IFDEF UNICODE}
          TypeName := ZSysUtils.ASCII7ToUnicodeString(P, ZFastCode.StrLen(P));
          {$ELSE}
          TypeName := '';
          ZSetString(P, ZFastCode.StrLen(P), TypeName);
          {$ENDIF}
        end;
        if LastVersion
        then BaseTypeCode := 0
        else BaseTypeCode := RawToIntDef(FPlainDriver.PQgetvalue(QueryHandle, I, 2), 0);

        if BaseTypeCode <> 0 then begin
          Index := FTypeList.IndexOfObject(TObject(BaseTypeCode));
          if Index >= 0
          then TypeName := FTypeList[Index]
          else TypeName := '';
        end;
        FTypeList.AddObject(TypeName, TObject(TypeCode));
      end;
      GetPlainDriver.PQclear(QueryHandle);
    end else
      HandleErrorOrWarning(Status, lcExecute,
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self, QueryHandle);
  end;

  I := FTypeList.IndexOfObject(TObject(Id));
  if I >= 0
  then Result := FTypeList[I]
  else Result := '';
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZPostgreSQLConnection.GetHostVersion: Integer;
begin
  Result := ZSysUtils.EncodeSQLVersioning(GetServerMajorVersion, GetServerMinorversion, GetServerSubversion)
end;

{**
  Gets a server major version.
  @return a server major version number.
}
function TZPostgreSQLConnection.GetServerMajorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMajorVersion;
end;

{**
  Gets a server minor version.
  @return a server minor version number.
}
function TZPostgreSQLConnection.GetServerMinorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMinorVersion;
end;

function TZPostgreSQLConnection.GetServerProvider: TZServerProvider;
begin
  Result := spPostgreSQL;
end;

{**
  Gets a server sub version.
  @return a server sub version number.
}
function TZPostgreSQLConnection.GetServerSubVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerSubVersion;
end;

function TZPostgreSQLConnection.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZPostgreSQLStatementAnalyser.Create;
end;

procedure TZPostgreSQLConnection.Listen(const EventNames: TStrings;
  Handler: TZOnEventHandler);
var I: Integer;
    RawTemp: RawByteString;
begin
  if (FEventList <> nil) then begin
    if (FEventList.Count > 0) then
      Unlisten;
    for i := 0 to EventNames.Count -1 do begin
      FEventList.Add(EventNames[i], Handler);
      {$IFDEF UNICODE}
      RawTemp := 'listen '+ZUnicodeToRaw(EventNames[i], ConSettings.ClientCodePage.CP);
      {$ELSE}
      RawTemp := 'listen '+EventNames[i];
      {$ENDIF}
      ExecuteImmediat(RawTemp, lcExecute);
    end;
    if FEventList.Count > 0 then
      FEventList.Timer.Enabled := True;
  end else
    raise EZSQLException.Create('no events registered');
end;

procedure TZPostgreSQLConnection.LoadServerVersion;
var
  Temp: string;
  List: TStrings;
  QueryHandle: TPGresult;
  SQL: RawByteString;
  P: PAnsichar;
  Status: TZPostgreSQLExecStatusType;
begin
  if Closed then
    Open;
  SQL := 'SELECT version()';
  QueryHandle := FPlainDriver.PQExec(Fconn, Pointer(SQL));
  Status := FPlainDriver.PQresultStatus(QueryHandle);
  if Status = PGRES_TUPLES_OK then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, URL.Protocol, 'SELECT version()');
    P := FPlainDriver.PQgetvalue(QueryHandle, 0, 0);
    {$IFDEF UNICODE}
    Temp := ZSysUtils.ASCII7ToUnicodeString(P, ZFastCode.StrLen(P));
    {$ELSE}
    Temp := '';
    ZSetString(P, ZFastCode.StrLen(P), Temp);
    {$ENDIF}
    FPlainDriver.PQclear(QueryHandle);

    List := TStringList.Create;
    try
      { Splits string by space }
      PutSplitString(List, Temp, ' ');
      { first - PostgreSQL, second X.Y.Z}
      Temp := List.Strings[1];
      { Splits string by dot }
      PutSplitString(List, Temp, '.');

      FServerMajorVersion := StrToIntDef(List.Strings[0], 0);
      if List.Count > 1 then
        FServerMinorVersion := GetMinorVersion(List.Strings[1])
      else
        FServerMinorVersion := 0;
      if List.Count > 2 then
        FServerSubVersion := GetMinorVersion(List.Strings[2])
      else
        FServerSubVersion := 0;
    finally
      List.Free;
    end;
  end else
    HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecute, 'SELECT version()', Self, QueryHandle);
end;

{**
Ping Current Connection's server, if client was disconnected,
the connection is resumed.
@return 0 if succesfull or error code if any error occurs
}
function TZPostgreSQLConnection.PingServer: Integer;
const
  PING_ERROR_ZEOSCONNCLOSED = -1;
var
  res: TPGresult;
  isset: boolean;
begin
  Result := PING_ERROR_ZEOSCONNCLOSED;
  if Not Closed and (Fconn <> nil) then begin
    res := FPlainDriver.PQExec(Fconn,'');
    isset := assigned(res);
    GetPlainDriver.PQclear(res);
    if isset and (FPlainDriver.PQstatus(Fconn) = CONNECTION_OK) then
      Result := 0
    else
      try
        FPlainDriver.PQreset(Fconn);
        res := FPlainDriver.PQExec(Fconn,'');
        isset := assigned(res);
        GetPlainDriver.PQclear(res);
        if isset and (FPlainDriver.PQstatus(Fconn) = CONNECTION_OK) then
          Result := 0;
      except
        Result := 1;
      end;
  end;
end;

const cRWSession: Array[Boolean] of RawByteString = (
  'SET SESSION CHARACTERISTICS AS TRANSACTION READ WRITE',
  'SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY');
procedure TZPostgreSQLConnection.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then begin
    if not Closed and HasMinimumServerVersion(7,4,0) then
      ExecuteImmediat(cRWSession[Value], lcTransaction);
    ReadOnly := Value;
  end;
end;

function TZPostgreSQLConnection.EscapeString(const Value: RawByteString): RawByteString;
begin
  Result := EscapeString(Pointer(Value), Length(Value), True)
end;

procedure TZPostgreSQLConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var QueryHandle: TPGresult;
    Status: TZPostgreSQLExecStatusType;
begin
  if Closed then
    Open;
  QueryHandle := FPlainDriver.PQexec(Fconn, Pointer(SQL));
  Status := FPlainDriver.PQresultStatus(QueryHandle);
  {$IFDEF UNICODE}
  if ((Status <> PGRES_COMMAND_OK) and (Status = PGRES_EMPTY_QUERY)) or DriverManager.HasLoggingListener then
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if (Status = PGRES_COMMAND_OK) or (Status = PGRES_EMPTY_QUERY) then begin
    FPlainDriver.PQclear(QueryHandle);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LoggingCategory, URL.Protocol, {$IFDEF UNICODE}FlogMessage{$ELSE}SQL{$ENDIF});
  end else
    HandleErrorOrWarning(Status, LoggingCategory, {$IFDEF UNICODE}FlogMessage{$ELSE}SQL{$ENDIF}, Self, QueryHandle);
end;

procedure TZPostgreSQLConnection.FillUnknownDomainOIDs;
var Stmt: IZStatement;
  I: Integer;
  RS: IZResultSet;
  SQLWriter: TZSQLStringWriter;
  SQL: SQLString;
begin
  if FDomain2BaseTypMap.UnkownCount = 0 then
    Exit;
  SQLWriter := TZSQLStringWriter.Create(512);
  SQL := 'select oid, typbasetype from pg_catalog.pg_type where oid in (';
  for i := 0 to FDomain2BaseTypMap.Count -1 do
    if not PZPGDomain2BaseTypeMap(FDomain2BaseTypMap.Items[I]).Known then begin
      SQLWriter.AddOrd(PZPGDomain2BaseTypeMap(FDomain2BaseTypMap.Items[I]).DomainOID, SQL);
      SQLWriter.AddChar(',', SQL);
    end;
  SQLWriter.ReplaceOrAddLastChar(',', ')', SQL);
  SQLWriter.Finalize(SQL);
  SQLWriter.Free;
  Stmt := CreateStatement;
  RS := CreateStatement.ExecuteQuery(SQL);
  while RS.Next do
    FDomain2BaseTypMap.AddIfNotExists(Rs.GetUInt(FirstDbcIndex), Rs.GetUInt(FirstDbcIndex+1));
  RS.Close;
  Stmt.Close;
end;

function TZPostgreSQLConnection.FindDomainBaseType(DomainOID: OID;
  out BaseTypeOID: OID): Boolean;
begin
  Result := FDomain2BaseTypMap.GetOrAddBaseTypeOID(DomainOID, BaseTypeOID);
end;

function TZPostgreSQLConnection.GetBinaryEscapeString(const Value: TBytes): String;
{$IFDEF UNICODE}
var Tmp: RawByteString;
{$ENDIF UNICODE}
begin
  {$IFDEF UNICODE}Tmp{$ELSE}Result{$ENDIF} := EncodeBinary(Pointer(Value), Length(Value), True);
  {$IFDEF UNICODE}
  Result := ASCII7ToUnicodeString(Tmp);
  {$ENDIF}
end;

{**
  EgonHugeist:
  Returns a String in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result = BinaryString
  @param Value represents the String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Postrgres-compatible String
}
function TZPostgreSQLConnection.GetEscapeString(const Value: UnicodeString): UnicodeString;
begin
  Result := ZRawToUnicode(EscapeString(ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP)), ConSettings^.ClientCodePage^.CP);
end;

function TZPostgreSQLConnection.GetEscapeString(const Value: RawByteString): RawByteString;
begin
  Result := EscapeString(Value);
end;

function TZPostgreSQLConnection.GetEventListener(Handler: TZOnEventHandler;
  CloneConnection: Boolean; Options: TStrings): IZEventListener;
begin
  Result := inherited GetEventListener(Handler, CloneConnection, Options);
  FEventList := TZPostgresEventList.Create(Handler, CheckEvents);
  if Options <> nil
  then FEventList.Timer.Interval := StrToIntDef(Options.Values[ELProps_ListernerInterval], 250)
  else FEventList.Timer.Interval := 250;
end;

{**
  Gets a current setting of run-time parameter.
  @param AName a parameter name.
  @result a parmeter value retrieved from server.
}
function TZPostgreSQLConnection.GetServerSetting(const AName: RawByteString): string;
var
  SQL: RawByteString;
  QueryHandle: TPGresult;
  P: PAnsichar;
  Status: TZPostgreSQLExecStatusType;
begin
  Result := '';
  SQL := 'select setting from pg_settings where name = '+AName;
  QueryHandle := FPlainDriver.PQExec(Fconn, Pointer(SQL));
  Status := FPlainDriver.PQresultStatus(QueryHandle);
  {$IFDEF UNICODE}
  if (Status <> PGRES_TUPLES_OK) or DriverManager.HasLoggingListener
  then FLogMessage := USASCII7ToUnicodeString(SQL)
  else FLogMessage := '';
  {$ENDIF}
  if Status = PGRES_TUPLES_OK then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, URL.Protocol, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF});
    P := FPlainDriver.PQgetvalue(QueryHandle, 0, 0);
    {$IFDEF UNICODE}
    PRawToUnicode(P, ZFastCode.StrLen(P), ConSettings^.ClientCodePage^.CP, Result);
    {$ELSE}
    ZSetString(P, ZFastCode.StrLen(P), RawByteString(Result){$IFDEF WITH_RAWBYTESTRING},ConSettings^.ClientCodePage^.CP{$ENDIF});
    {$ENDIF}
    FPlainDriver.PQclear(QueryHandle);
  end else
    HandleErrorOrWarning(Status, lcExecute, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self, QueryHandle);
end;

{**
  Sets current setting of run-time parameter.
  String values should be already quoted.
  @param AName a parameter name.
  @param AValue a new parameter value.
}
procedure TZPostgreSQLConnection.SetServerSetting(const AName,
  AValue: RawbyteString);
var
  SQL: RawByteString;
  QueryHandle: TPGresult;
  status: TZPostgreSQLExecStatusType;
begin
  SQL := 'SET '+AName+' = '+AValue;
  QueryHandle := FPlainDriver.PQExec(Fconn, Pointer(SQL));
  status := FPlainDriver.PQresultStatus(QueryHandle);
  {$IFDEF UNICODE}
  if (Status <> PGRES_COMMAND_OK) or DriverManager.HasLoggingListener
  then FLogMessage := USASCII7ToUnicodeString(SQL)
  else FLogMessage := '';
  {$ENDIF}
  if (Status = PGRES_COMMAND_OK) then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, URL.Protocol, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF});
    FPlainDriver.PQclear(QueryHandle);
  end else
    HandleErrorOrWarning(Status, lcExecute, {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self, QueryHandle);
end;

{$IFDEF ZEOS_TEST_ONLY}
constructor TZPostgreSQLConnection.Create(const ZUrl: TZURL);
begin
 inherited Create(ZUrl);
end;
{$ENDIF}

function TZPostgreSQLConnection.EncodeBinary(Buf: Pointer;
  Len: Integer; Quoted: Boolean): RawByteString;
var
  escapedLen: LongWord;
  procedure SetResult(escapedBuf: PAnsichar; var Result: RawByteString);
  var P: PAnsiChar;
  begin
    escapedLen := escapedLen -1; //return length including #0
    ZSetString(nil, escapedLen+Byte(Ord(Quoted) shl 1), Result);
    P := Pointer(Result);
    if Quoted then begin
      P^ := #39;
      Inc(P);
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(escapedBuf^, P^, escapedLen);
    FPlainDriver.PQFreemem(escapedBuf);
    if Quoted then
      (P+escapedLen)^ := #39;
  end;
begin
  Result := '';
  if (Buf = nil) or (Len = 0) then begin
    if Quoted then
       Result := ''''''
  end else if Assigned(Fconn) and Assigned(FPlainDriver.PQescapeByteaConn) then
    SetResult(FPlainDriver.PQescapeByteaConn(Fconn, Buf, Len, @escapedLen), Result)
  else if Assigned(FPlainDriver.PQescapeBytea) then
    SetResult(FPlainDriver.PQescapeBytea(Buf,Len,@escapedLen), Result)
  else
    Result := ZDbcPostgreSqlUtils.EncodeBinaryString(Buf, Len, Quoted);
end;

function TZPostgreSQLConnection.EscapeString(const FromChar: PAnsiChar;
  len: NativeUInt; Quoted: Boolean): RawByteString;
var
  Buf: Array[0..2048] of AnsiChar;
  iError: Integer;
  P: PAnsiChar;
  EscapedLen: NativeUInt;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  if Assigned(FPlainDriver.PQescapeStringConn) or Assigned(FPlainDriver.PQescapeString) then begin
    if (Len+Byte(Ord(Quoted))) shl 1 > (SizeOf(Buf)-1) then begin
      SetLength(Result, (Len+Byte(Ord(Quoted))) shl 1);
      P := Pointer(Result);
    end else
      P := @Buf[0];
    if Quoted then
      P^ := #39;
    iError := 0;
    if Assigned(Fconn) and Assigned(FPlainDriver.PQescapeStringConn)
    then EscapedLen := FPlainDriver.PQescapeStringConn(Fconn, P+Ord(Quoted), FromChar, Len, @iError)
    else EscapedLen := FPlainDriver.PQescapeString(P+Ord(Quoted), FromChar, Len);
    if iError <> 0 then
      raise Exception.Create('Wrong string escape behavior!');
    if Quoted then
      (P+EscapedLen+(Byte(Ord(Quoted))))^ := #39;
    if P = @Buf[0]
    then ZSetString(@Buf[0], EscapedLen+(Byte(Ord(Quoted) shl 1)), Result)
    else SetLength(Result, EscapedLen+(Byte(Ord(Quoted)) shl 1));
  end else
    Result := ZDbcPostgreSqlUtils.PGEscapeString(FromChar, Len, ConSettings, Quoted);
end;

{ TZOID2OIDMapList }

procedure TZOID2OIDMapList.AddIfNotExists(DomainOID, BaseTypeOID: OID);
var FoundOID: OID;
  I: Integer;
begin
  if not GetOrAddBaseTypeOID(DomainOID, FoundOID) then
    for i := 0 to Count-1 do
      if (PZPGDomain2BaseTypeMap(Items[i]).DomainOID = DomainOID) then begin
        PZPGDomain2BaseTypeMap(Items[i]).BaseTypeOID := BaseTypeOID;
        PZPGDomain2BaseTypeMap(Items[i]).Known := True;
        Dec(fUnkownCount);
      end;
end;

procedure TZOID2OIDMapList.Clear;
var i: Integer;
begin
  for i := Count-1 downto 0 do
    FreeMem(Items[i], SizeOf(TZPGDomain2BaseTypeMap));
  inherited;
  fUnkownCount := 0;
end;

function TZOID2OIDMapList.GetOrAddBaseTypeOID(DomainOID: OID; out BaseTypeOID: OID): Boolean;
var i: Integer;
  Val: PZPGDomain2BaseTypeMap;
begin
  Result := False;
  BaseTypeOID := InvalidOID;
  for i := 0 to Count-1 do
    if (PZPGDomain2BaseTypeMap(Items[i]).DomainOID = DomainOID) and PZPGDomain2BaseTypeMap(Items[i]).Known then begin
      Result := PZPGDomain2BaseTypeMap(Items[i]).Known;
      BaseTypeOID := PZPGDomain2BaseTypeMap(Items[i]).BaseTypeOID;
      Exit;
    end;
  GetMem(Val, SizeOf(TZPGDomain2BaseTypeMap));
  Val.DomainOID := DomainOID;
  Val.BaseTypeOID := InvalidOID;
  Val.Known := False;
  Add(Val);
  Sort(SortCompare);
  Inc(fUnkownCount);
end;

function TZOID2OIDMapList.SortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := Ord(PZPGDomain2BaseTypeMap(Item1)^.DomainOID > PZPGDomain2BaseTypeMap(Item2)^.DomainOID)-
            Ord(PZPGDomain2BaseTypeMap(Item1)^.DomainOID < PZPGDomain2BaseTypeMap(Item2)^.DomainOID);
end;

{ TZPostgresEventList }

constructor TZPostgresEventList.Create(Handler: TZOnEventHandler;
  TimerTick: TThreadMethod);
begin
  inherited Create(Handler);
  FTimer := TZThreadTimer.Create;
  FTimer.OnTimer := TimerTick;
end;

destructor TZPostgresEventList.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ TZPostgresEventData }

function TZPostgresEventData.ToString: string;
begin
  Result := inherited ToString +'; ProcessID: '+ZFastCode.IntToStr(FProcessID);
  if fPayload <> '' then
    Result := Result +'; Payload: '+fPayload;
end;

initialization
  PostgreSQLDriver := TZPostgreSQLDriver.Create;
  DriverManager.RegisterDriver(PostgreSQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(PostgreSQLDriver);
  PostgreSQLDriver := nil;
{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
