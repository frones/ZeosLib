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
  ZGenericSqlAnalyser, ZPlainDriver, ZCollections, ZDbcLogging;

type

  {** Implements Abstract Database Driver. }
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
    constructor Create; virtual;
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

  {** Implements Abstract Database Connection. }

  { TZAbstractDbcConnection }

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
    FRestartTransaction: Boolean;
    FDisposeCodePage: Boolean;
    FClientCodePage: String;
    FMetadata: TContainedObject;
    {$IFDEF ZEOS_TEST_ONLY}
    FTestMode: Byte;
    {$ENDIF}
    FLogMessage: SQLString;
    procedure InternalClose; virtual; abstract;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; virtual;
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; virtual;
    procedure SetDateTimeFormatProperties(DetermineFromInfo: Boolean = True);
    procedure ResetCurrentClientCodePage(const Name: String;
      IsStringFieldCPConsistent: Boolean);
    function GetEncoding: TZCharEncoding;
    function GetClientVariantManager: IZClientVariantManager;
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
    constructor Create(const {%H-}Driver: IZDriver; const Url: string;
      const {%H-}PlainDriver: IZPlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string;
      Info: TStrings); overload;
    constructor Create(const ZUrl: TZURL); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateNotification(const Event: string): IZNotification; virtual;
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; virtual;

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

    //Ping Support initially for MySQL 27032006 (firmos)
    function PingServer: Integer; virtual;
    function AbortOperation: Integer; virtual;
    function EscapeString(const Value: RawByteString): RawByteString; overload; virtual;

    procedure Open; virtual;
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
    function IsClosed: Boolean; virtual;

    function GetDriver: IZDriver;
    function GetIZPlainDriver: IZPlainDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; virtual;
    function GetHostVersion: Integer; virtual;
    {END ADDED by fduenas 15-06-2006}
    function GetDescription: String;
    procedure SetReadOnly(Value: Boolean); virtual;
    function IsReadOnly: Boolean; virtual;
    function GetURL: String;

    procedure SetCatalog(const {%H-}Catalog: string); virtual;
    function GetCatalog: string; virtual;

    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); virtual;
    function GetTransactionIsolation: TZTransactIsolationLevel; virtual;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;

    function GetBinaryEscapeString(const Value: TBytes): String; overload; virtual;

    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload; virtual;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; virtual;

    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean); virtual;
    function GetServerProvider: TZServerProvider; virtual;
  protected
    function GetByteBufferAddress: PByteBuffer;
    property Closed: Boolean read IsClosed write FClosed;
    property AddLogMsgToExceptionOrWarningMsg: Boolean read
      fAddLogMsgToExceptionOrWarningMsg write fAddLogMsgToExceptionOrWarningMsg;
    property RaiseWarnings: Boolean read fRaiseWarnings write fRaiseWarnings;
  end;

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
    procedure ReleaseTransaction(const Value: IZTransaction);
    function IsTransactionValid(const Value: IZTransaction): Boolean;
    procedure ClearTransactions;
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

  {** Implements Abstract Database notification. }
  TZAbstractNotification = class(TInterfacedObject, IZNotification)
  private
    FEventName: string;
    FConnection: IZConnection;
  protected
    property EventName: string read FEventName write FEventName;
    property Connection: IZConnection read FConnection write FConnection;
  public
    constructor Create(const Connection: IZConnection; const EventName: string);
    function GetEvent: string;
    procedure Listen; virtual;
    procedure Unlisten; virtual;
    procedure DoNotify; virtual;
    function CheckEvents: string; virtual;

    /// <summary>Get's the owner connection that produced that object instance.
    /// </summary>
    /// <returns>the connection object interface.</returns>
    function GetConnection: IZConnection; virtual;
  end;

  {** Implements Abstract Sequence generator. }
  TZAbstractSequence = class(TInterfacedObject, IZSequence)
  private
    FConnection: IZConnection;
    FNextValRS, FCurrValRS: IZResultSet;
    FNextValStmt, FCurrValStmt: IZPreparedStatement;
  protected
    FName: string;
    FBlockSize: Integer;
    procedure SetName(const Value: string); virtual;
    procedure SetBlockSize(const Value: Integer); virtual;
    property Connection: IZConnection read FConnection write FConnection;
    procedure FlushResults;
  public
    constructor Create(const Connection: IZConnection; const Name: string;
      BlockSize: Integer);

    function GetCurrentValue: Int64;
    function GetNextValue: Int64;

    function GetName: string;
    function GetBlockSize: Integer;
    function GetCurrentValueSQL: string; virtual; abstract;
    function GetNextValueSQL: string; virtual; abstract;

    function GetConnection: IZConnection;
  end;

  TZIdentifierSequence = Class(TZAbstractSequence)
  protected
    procedure SetName(const Value: string); override;
  End;

  {** Implements a MSSQL sequence. }
  TZMSSQLSequence = class(TZAbstractSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;

  {** Implements an abstract sequence using the <Name>.CURRVAL/NEXTVAL Syntax}
  TZDotCurrvalNextvalSequence = class(TZIdentifierSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;

  {** Implements a Sybase SQL Anywhere sequence. }
  TZSybaseASASquence = class(TZDotCurrvalNextvalSequence);

  {** Implements an Informix sequence. }
  TZInformixSquence = class(TZDotCurrvalNextvalSequence);

  {** Implements an DB2 sequence. }
  TZDB2Squence = class(TZDotCurrvalNextvalSequence);

  {** Implements an CUBRID sequence. }
  TZCubridSquence = class(TZDotCurrvalNextvalSequence);

  {** Implements an Oracle sequence. }
  TZOracleSequence = class(TZDotCurrvalNextvalSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;

  {** Implements a FireBird2+ sequence. }
  TZFirebird2UpSequence = class(TZIdentifierSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;

  {** Implements a postresql sequence. }
  TZPostgreSQLSequence = class(TZAbstractSequence)
  public
    function GetCurrentValueSQL: string; override;
    function GetNextValueSQL: string; override;
  end;

  TZAbstractSequenceClass = class of TZAbstractSequence;

  {** Implements a variant manager with connection related convertion rules. }
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
    constructor Create(const ConSettings: PZConSettings{; FormatSettings: TZFormatSettings});
    function UseWComparsions: Boolean;
    function GetAsDateTime(const Value: TZVariant): TDateTime; reintroduce;
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
  //(*
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
    {spDB2}       TZDB2Squence,
    {spAS400}     nil,
    {spInformix}  TZInformixSquence,
    {spCUBRID}    TZCubridSquence,
    {spFoxPro}    nil
    );

  //*)
{ TZAbstractDriver }

{**
  Constructs this object with default properties.
}
constructor TZAbstractDriver.Create;
begin
  FCachedPlainDrivers := TZHashMap.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
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

{**
  EgonHugeist:
  Get names of the supported CharacterSets.
  For example: ASCII, UTF8...
}
function TZAbstractDriver.GetClientCodePages(const Url: TZURL): TStringDynArray;
var
  Plain: IZPlainDriver;
begin
  Plain := GetPlainDriverFromCache(Url.Protocol, '');
  if Assigned(Plain) then
  Result := Plain.GetClientCodePages;
end;

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
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

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the TZURL of the database to which to connect
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
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
  if Protocol = '' then
  begin
    Result := PlainDriver.GetProtocol;
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(PlainDriver.GetProtocol))
  end
  else
  begin
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
  if Result = nil then
  begin
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol));
    TempPlain := FCachedPlainDrivers.Get(TempKey) as IZPlainDriver;
    if Assigned(TempPlain) then
    begin
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
begin
  if Assigned(FOnConnectionLostError) then
    FOnConnectionLostError := Handler
  else if Assigned(FOnConnectionLostError) then
    raise EZSQLException.Create('Error handler registered already!')
  else FOnConnectionLostError := Handler;
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
begin
  if QueryInterface(IZConnection, ICon) = S_OK then begin
    fWeakReferenceOfSelfInterface := Pointer(iCon);
    iCon := nil;
  end;
  inherited AfterConstruction;
  FURL.OnPropertiesChange := OnPropertiesChange;
end;

{**
  EgonHugeist: Check if the given Charset for Compiler/Database-Support!!
    Not supported means if there is a possible String-DataLoss.
    So it raises an Exception if case of settings. This handling
    is an improofment to inform Zeos-Users about the troubles the given
    CharacterSet may have.
  @param CharSet the CharacterSet which has to be proofed
  @param DoArrange represents a switch to check and set a aternative ZAlias as
    default. This means it ignores the choosen Client-CharacterSet and sets a
    "more" Zeos-Compatible Client-CharacterSet if known.
}
procedure TZAbstractDbcConnection.CheckCharEncoding(const CharSet: String;
  const DoArrange: Boolean = False);
begin
  ConSettings.ClientCodePage := GetIZPlainDriver.ValidateCharEncoding(CharSet, DoArrange);
  FClientCodePage := ConSettings.ClientCodePage^.Name; //resets the developer choosen ClientCodePage
  FClientVarManager := TZClientVariantManager.Create(ConSettings);
end;

{**
  EgonHugeist and MDeams: The old deprecadet constructor which was used
  from the descendant classes. We left him here for compatibility reasons to
  exesting projects which using the DbcConnections directly

  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param Url a connection URL.
  @param PlainDriver a versioned ZPlainDriver object interface.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
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

{**
  Constructs this object and assignes the main properties.
  @param Url a connection ZURL-class which exports all connection parameters.
}
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

{**
  Destroys this object and cleanups the memory.
}
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

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAbstractDbcConnection.Open;
begin
  FClosed := False;
  SetDateTimeFormatProperties;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @return a new Statement object
}
function TZAbstractDbcConnection.CreateStatement: IZStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).CreateStatementWithParams(nil);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZAbstractDbcConnection.PrepareStatement(const SQL: string): IZPreparedStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).PrepareStatementWithParams(SQL, nil);
end;

procedure TZAbstractDbcConnection.PrepareTransaction(const transactionid: string);
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}

function TZAbstractDbcConnection.PrepareCall(
  const SQL: string): IZCallableStatement;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface).PrepareCallWithParams(SQL, nil);
end;

{**
  Creates an object to send/recieve notifications from SQL server.
  @param Event an event name.
  @returns a created notification object.
}
{$IFDEF FPC} {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "Event" not used}
{$ENDIF}
function TZAbstractDbcConnection.CreateNotification(
  const Event: string): IZNotification;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZAbstractDbcConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  if TZDefaultProviderSequenceClasses[GetServerProvider] <> nil then
    Result := TZDefaultProviderSequenceClasses[GetServerProvider].Create(
      IZConnection(fWeakReferenceOfSelfInterface), Sequence, BlockSize)
  else
    Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

{**
  Converts the given SQL statement into the system's native SQL grammar.
  A driver may convert the JDBC sql grammar into its system's
  native SQL grammar prior to sending it; this method returns the
  native form of the statement that the driver would have sent.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders
  @return the native form of this statement
}
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

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZAbstractDbcConnection.PingServer: Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Escape a string so it's acceptable for the Connection's server.
  @param value string that should be escaped
  @return Escaped string
}
function TZAbstractDbcConnection.EscapeString(const Value : RawByteString) : RawByteString;
begin
  Result := EncodeCString(Value);
end;

{**
  Executes any statement immediataly
  @param SQL the raw encoded sql which should be executed
  @param LoggingCategory a category for the LoggingListeners
}
procedure TZAbstractDbcConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var CP: Word;
begin
  if ConSettings.ClientCodePage.Encoding = ceUTF16
  then CP := zCP_UTF8
  else CP := ConSettings.ClientCodePage.CP;
  ExecuteImmediat(ZRawToUnicode(SQL, CP), LoggingCategory);
end;

{**
  Executes any statement immediataly
  @param SQL the UTF16 encoded sql which should be executed
  @param LoggingCategory a category for the LoggingListeners
}
procedure TZAbstractDbcConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
begin
  ExecuteImmediat(ZUnicodeToRaw(SQL, ConSettings.ClientCodePage.CP), LoggingCategory);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}

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

procedure TZAbstractDbcConnection.CloseRegisteredStatements;
var I: Integer;
begin
  for i := fRegisteredStatements.Count-1 downto 0 do begin
    //try
      IZStatement(fRegisteredStatements[i]).Close;
    //except end;
  end;
end;

{**
  Tests to see if a Connection is closed.
  @return true if the connection is closed; false if it's still open
}
function TZAbstractDbcConnection.IsClosed: Boolean;
begin
  Result := FClosed;
  DriverManager.ClearGarbageCollector;
end;

{**
  Gets the parent ZDBC driver.
  @returns the parent ZDBC driver interface.
}
function TZAbstractDbcConnection.GetDriver: IZDriver;
begin
  Result := FDriver;
end;

{**
  Gets the plain driver.
  @returns the plain driver interface.
}
function TZAbstractDbcConnection.GetIZPlainDriver: IZPlainDriver;
begin
  result := FIZPlainDriver;
end;

{**
  Gets the metadata regarding this connection's database.
  A Connection's database is able to provide information
  describing its tables, its supported SQL grammar, its stored
  procedures, the capabilities of this connection, and so on. This
  information is made available through a DatabaseMetaData
  object.

  @return a DatabaseMetaData object for this Connection
}
function TZAbstractDbcConnection.GetMetadata: IZDatabaseMetadata;
begin
  if Closed then
    Open;
  Result := FMetadata as IZDatabaseMetadata;
end;

{**
  Gets a connection parameters.
  @returns a list with connection parameters.
}
function TZAbstractDbcConnection.GetParameters: TStrings;
begin
  Result := Info;
end;

{**
  Gets the client's full version number. Initially this should be 0.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractDbcConnection.GetClientVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZAbstractDbcConnection.GetHostVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the PlainDriver description.
  @returns a the description.
}
function TZAbstractDbcConnection.GetDescription: String;
begin
  Result := PlainDriver.GetDescription;
end;

{END ADDED by fduenas 15-06-2006}

procedure TZAbstractDbcConnection.SetRaiseWarnings(Value: Boolean);
begin
  fRaiseWarnings := Value;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZAbstractDbcConnection.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

{**
  Tests to see if the connection is in read-only mode.
  @return true if connection is read-only and false otherwise
}
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

{**
  get current connection URL from TZURL. Nice to clone the connection by using
  the IZDriverManager
  @return true if connection is read-only and false otherwise
}
function TZAbstractDbcConnection.GetURL: String;
begin
  Result := FURL.URL
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZAbstractDbcConnection.SetCatalog(const Catalog: string);
begin
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAbstractDbcConnection.GetCatalog: string;
begin
  Result := '';
end;

procedure TZAbstractDbcConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  FTransactIsolationLevel := Level;
end;

{**
  Gets this Connection's current transaction isolation level.
  @return the current TRANSACTION_* mode value
}
function TZAbstractDbcConnection.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := FTransactIsolationLevel;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZAbstractDbcConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
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

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Sender" not used} {$ENDIF}
procedure TZAbstractDbcConnection.OnPropertiesChange(Sender: TObject);
begin
  // do nothing in base class
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZAbstractNotification }

{**
  Creates this object and assignes the main properties.
  @param Connection a database connection object.
  @param EventName a name of the SQL event.
}
constructor TZAbstractNotification.Create(const Connection: IZConnection;
  const EventName: string);
begin
  FConnection := Connection;
  FEventName := EventName;
end;

{**
  Gets an event name.
  @return an event name for this notification.
}
function TZAbstractNotification.GetEvent: string;
begin
  Result := FEventName;
end;

{**
  Sets a listener to the specified event.
}
procedure TZAbstractNotification.Listen;
begin
end;

{**
  Removes a listener to the specified event.
}
procedure TZAbstractNotification.Unlisten;
begin
end;

{**
  Checks for any pending events.
  @return a string with incoming events??
}
function TZAbstractNotification.CheckEvents: string;
begin
  Result := '';
end;

{**
  Sends a notification string.
}
procedure TZAbstractNotification.DoNotify;
begin
end;

function TZAbstractNotification.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{ TZAbstractSequence }

{**
  Creates this sequence object.
  @param Connection an SQL connection interface.
  @param Name a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to server.
}
constructor TZAbstractSequence.Create(const Connection: IZConnection;
  const Name: string; BlockSize: Integer);
begin
  FConnection := Connection;
  FName := Name;
  FBlockSize := BlockSize;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractSequence.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{**
  Returns a name of the sequence generator.
  @return a name of this sequence generator.
}
function TZAbstractSequence.GetName: string;
begin
  Result := FName;
end;

{**
  Returns the assigned block size for this sequence.
  @return the assigned block size.
}
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

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
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

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
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

{**
  Sets the block size for this sequence.
  @param Value the block size.
}
procedure TZAbstractSequence.SetBlockSize(const Value: Integer);
begin
  FBlockSize := Value;
end;

{**
  Sets a name of the sequence generator.
  @param Value a name of this sequence generator.
}
procedure TZAbstractSequence.SetName(const Value: string);
begin
  if FName <> Value then begin
    FlushResults;
    FName := FName;
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

{**
  Constructs this object and assigns the main properties.
  @param ClientCodePage the current ClientCodePage.
}
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
  Result.VType := vtAnsiString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: if FClientCP = ZOSCodePage
              then ResTmp := Value.VRawByteString
              else begin
                ResTmp := '';
                PRawToRawConvert(Pointer(Value.VRawByteString), Length(Value.VRawByteString), FClientCP, GetW2A2WConversionCodePage(FConSettings), ResTmp);
              end;
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
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
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
        Result.VCharRec.CP := fClientCP;
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
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: ResTmp := Value.VRawByteString;
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
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, zCP_UTF8);
    vtCharRec:
      if (Value.VCharRec.CP = zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, FClientCP)
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
      //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
      //this hint means a cast instead of convert. The user should better use WideString constants!
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF});
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
  Result.VType := vtUnicodeString;
  case Value.VType of
    {$IFNDEF UNICODE}
    vtString: ResTmp := ZRawToUnicode(Value.VRawByteString, FStringCP);
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: ResTmp := ZRawToUnicode(Value.VRawByteString, ZOSCodePage);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: ResTmp := ZRawToUnicode(Value.VRawByteString, zCP_UTF8);
    {$ENDIF}
    vtRawByteString: ResTmp := ZRawToUnicode(Value.VRawByteString, FClientCP);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString: ResTmp := Value.VUnicodeString;
    vtDateTime:      ResTmp := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(Value.VDateTime, FFormatSettings, False);
    vtCharRec: if (Value.VCharRec.CP = zCP_UTF16)
        then SetString(ResTmp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
        else ResTmp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
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
    vtRawByteString: if ZOSCodePage = FClientCP
                     then ResTmp := Value.VRawByteString
                     else RawCPConvert(Value.VRawByteString, ResTmp, FClientCP, ZOSCodePage);
    {$IFDEF UNICODE}vtString,{$ENDIF}
    vtUnicodeString:
      ResTmp := ZUnicodeToRaw(Value.VUnicodeString, zCP_UTF8);
    vtCharRec:
      if (Value.VCharRec.CP = zCP_UTF16) then
        ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, zCP_UTF8)
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
  {$IFDEF DEBUG}
  if fWeakTxnPtr = nil then
    raise EZSQLException.Create(SUnsupportedOperation);
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
  then raise EZSQLException.Create(SUnsupportedOperation)
  else fTransactions.Delete(fTransactions.IndexOf(Value));
end;

end.
