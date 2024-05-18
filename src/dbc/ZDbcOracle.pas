{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

unit ZDbcOracle;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZClasses, ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainOracleDriver,
  ZDbcLogging, ZTokenizer, ZDbcGenericResolver, ZGenericSqlAnalyser, ZDbcCache,
  ZExceptions;

type
  {$IFNDEF ZEOS_DISABLE_ORACLE}

  /// <summary>Implements Oracle Database Driver.</summary>
  TZOracleDriver = class(TZAbstractDriver)
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
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer; override;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  /// <summary>Defines an Oracle specific connection interface.</summary>
  IZOracleConnection = interface (IZConnection)
    ['{C7F36FDF-8A64-477B-A0EB-3E8AB7C09F8D}']
    /// <summary>Gets a reference to Oracle connection handle.</summary>
    /// <returns>a reference to Oracle connection handle.</returns>
    function GetConnectionHandle: POCIEnv;
    /// <summary>Gets a reference to Oracle context handle.</summary>
    /// <returns>a reference to Oracle context handle.</returns>
    function GetServiceContextHandle: POCISvcCtx;
    /// <summary>Gets a reference to Oracle error handle.</summary>
    /// <returns>a reference to Oracle error handle.</returns>
    function GetErrorHandle: POCIError;
    /// <summary>Gets a reference to Oracle server handle.</summary>
    /// <returns>a reference to Oracle server handle.</returns>
    function GetServerHandle: POCIServer;
    /// <summary>Gets a reference to Oracle session handle.</summary>
    /// <returns>a reference to Oracle session handle.</returns>
    function GetSessionHandle: POCISession;
    /// <summary>Gets a reference to Oracle transaction handle.</summary>
    /// <returns>a reference to Oracle transacton handle.</returns>
    function GetTransactionHandle: POCITrans;
    /// <summary>Gets a reference to Oracle describe handle.</summary>
    /// <returns>a reference to Oracle describe handle.</returns>
    function GetDescribeHandle: POCIDescribe;
    /// <summary>Gets the oracle specific plaindriver object.</summary>
    /// <returns>the plaindriver object.</returns>
    function GetPlainDriver: TZOraclePlainDriver;
    /// <summary>Get the refrence to a fixed TByteBuffer.</summary>
    /// <returns>the address of the TByteBuffer</returns>
    function GetByteBufferAddress: PByteBuffer;

    procedure HandleErrorOrWarning(ErrorHandle: POCIError; Status: sword;
      LogCategory: TZLoggingCategory; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
  end;

  IZOracleTransaction = interface(IZTransaction)
    ['{07C8E090-BE86-4CA9-B2CB-1583DA94AFA6}']
    function GetTrHandle: POCITrans;
  end;

  /// <summary>implements an oracle OCI connection.</summary>
  TZOracleConnection = class(TZAbstractSingleTxnConnection, IZConnection,
    IZOracleConnection, IZTransaction)
  private
    FCatalog: string;
    FOCIEnv: POCIEnv;
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FServerHandle: POCIServer;
    FSessionHandle: POCISession;
    FDescibeHandle: POCIDescribe;
    FStatementPrefetchSize: Integer;
    FBlobPrefetchSize: Integer;
    FStmtMode: ub4;
    FPlainDriver: TZOraclePlainDriver;
    fGlobalTransactions: array[Boolean] of IZCollection; //simultan global read-Only/Write transaction container
    fLocalTransaction, //oracle allows just one Local transaction
    fAttachedTransaction: IZOracleTransaction;
    fcharset: UB2;
    fWarning: EZSQLWarning;
    procedure ExecuteImmediat(const SQL: RawByteString; var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory); overload;
    procedure ExecuteImmediat(const SQL: UnicodeString; var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory); overload;
    procedure InternalSetCatalog(const Catalog: String);
    procedure FreeAllocatedHandles;
  protected
    /// <summary>Releases a Connection's database and resources immediately
    ///  instead of waiting for them to be automatically released.</summary>
    ///  Note: A Connection is automatically closed when it is garbage
    ///  collected. Certain fatal errors also result in a closed Connection.</summary>
    procedure InternalClose; override;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a raw encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a UTF16 encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; override;
  public
    procedure AfterConstruction; override;
  public { IZTransactionManager }
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    /// <summary>Remove the given transaction interface from the manager list.
    ///  This method will be called from the Transaction interface when the
    ///  Transaction gets closed. If the interface is unknown an SQLException
    ///  will be raised.</summary>
    /// <param>"Value" the Transaction interface which should be removed.</param>
    procedure ReleaseTransaction(const Transaction: IZTransaction);
    procedure SetActiveTransaction(const Value: IZTransaction);
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
    /// <summary>Creates a sequence generator object.</summary>
    /// <param>"Sequence" a name of the sequence generator.</param>
    /// <param>"BlockSize" a number of unique keys requested in one trip to SQL
    ///  server.</param>
    /// <returns>returns a created sequence object.</returns>
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; override;
  public { txn support }
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
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean); override;
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
  public
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
    /// <summary>Ping Current Connection's server, if client was disconnected,
    ///  the connection is resumed.</summary>
    /// <returns>0 if succesfull or error code if any error occurs</returns>
    function PingServer: Integer; override;
    /// <author>aehimself</author>
    /// <summary>Immediately abort any kind of queries.</summary>
    /// <returns>0 if the operation is aborted; Non zero otherwise.</returns>
    function AbortOperation: Integer; override;
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open; override;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Value: string); override;
    /// <summary>Returns the Connection's current catalog name.</summary>
    /// <returns>the current catalog name or an empty string.</returns>
    function GetCatalog: string; override;
    /// <summary>Clears all warnings reported for this <c>Connection</c> object.
    ///  After a call to this method, the method <c>getWarnings</c> returns nil
    ///  until a new warning is reported for this Connection.</summary>
    procedure ClearWarnings; override;
  public { IZOracleConnection }
    /// <summary>Gets a reference to Oracle connection handle.</summary>
    /// <returns>a reference to Oracle connection handle.</returns>
    function GetConnectionHandle: POCIEnv;
    /// <summary>Gets a reference to Oracle context handle.</summary>
    /// <returns>a reference to Oracle context handle.</returns>
    function GetServiceContextHandle: POCISvcCtx;
    /// <summary>Gets a reference to Oracle error handle.</summary>
    /// <returns>a reference to Oracle error handle.</returns>
    function GetErrorHandle: POCIError;
    /// <summary>Gets a reference to Oracle server handle.</summary>
    /// <returns>a reference to Oracle server handle.</returns>
    function GetServerHandle: POCIServer;
    /// <summary>Gets a reference to Oracle session handle.</summary>
    /// <returns>a reference to Oracle session handle.</returns>
    function GetSessionHandle: POCISession;
    /// <summary>Gets a reference to Oracle transaction handle.</summary>
    /// <returns>a reference to Oracle transacton handle.</returns>
    function GetTransactionHandle: POCITrans;
    /// <summary>Gets a reference to Oracle describe handle.</summary>
    /// <returns>a reference to Oracle describe handle.</returns>
    function GetDescribeHandle: POCIDescribe;
    /// <summary>Gets the oracle specific plaindriver object.</summary>
    /// <returns>the plaindriver object.</returns>
    function GetPlainDriver: TZOraclePlainDriver;

    procedure HandleErrorOrWarning(ErrorHandle: POCIError; Status: sword;
      LogCategory: TZLoggingCategory; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
  public
    /// <summary>Gets the client's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this clients's full version number</returns>
    function GetClientVersion: Integer; override;
    /// <summary>Gets the host's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this server's full version number</returns>
    function GetHostVersion: Integer; override;
    function GetBinaryEscapeString(const Value: TBytes): String; override;
    /// <summary>Returns the ServicerProvider for this connection.</summary>
    /// <returns>the ServerProvider</returns>
    function GetServerProvider: TZServerProvider; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
 end;

  /// <summary>
  ///  Defines a oracle transaction mode.
  /// </summary>
  TZOCITxnMode = (tmDefault, tmReadOnly, tmReadWrite, tmSerializable);

  /// <summary>
  ///  Defines a oracle transaction spawning mode.
  /// </summary>
  TZOCITxnSpawnMode = (smNew, smJoin{not supported by OCI}, smResume);

  /// <summary>
  ///  Defines a oracle transaction couple mode.
  /// </summary>
  TZOCITxnCoupleMode = (cmTightly, cmLoosely);

  /// <author>EgonHugeist</author>
  /// <summary>implements an oracle transaction object</summary>
  TZOracleTransaction = class(TZCodePagedObject, IImmediatelyReleasable,
    IZTransaction, IZOracleTransaction)
  private
    fSavepoints: TStrings;
    fBranches: IZCollection;
    fDoLog, FStarted, fLocal: Boolean;
    FOCITrans: POCITrans;
    FTxnMode: TZOCITxnMode;
    FCoupleMode: TZOCITxnCoupleMode;
    FSpawnMode: TZOCITxnSpawnMode;
    //FTXID: TXID;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZOracleConnection;
  public { IZTransaction }
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  released. Otherwise makes all changes made since the previous commit/
    ///  rollback permanent and releases any database locks currently held by
    ///  the Connection. This method should be used only when auto-commit mode
    ///  has been disabled. See setAutoCommit.</summary>
    procedure Commit;
    /// <summary>Releases a transaction and resources immediately
    ///  instead of waiting for them to be automatically released. If the
    ///  transaction is underway a rollback will be done. Note: A
    ///  Transaction is automatically closed when the Conenction closes or it is
    ///  garbage collected. Certain fatal errors also result in a closed
    //// Transaction.</summary>
    procedure Close;
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
    /// <returns>Returns the current txn-level. 1 means a expicit transaction
    ///  was started. 2 means the transaction was saved. 3 means the previous
    ///  savepoint got saved too and so on.</returns>
    function StartTransaction: Integer;
    /// <summary>Check if the current transaction is readonly. See setReadonly.
    ///  </summary>
    /// <returns><c>True</c> if the transaction is readonly; <c>False</c>
    ///  otherwise.</returns>
    function IsReadOnly: Boolean;
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    /// <summary>Gets the current auto-commit state. See setAutoCommit.</summary>
    /// <returns>the current state of auto-commit mode.</returns>
    function GetAutoCommit: Boolean;
    /// <summary>Test if this <c>Transaction</c> object is closed.</summary>
    /// <returns><c>True</c> if the transaction is closed; <c>False</c>
    ///  otherwise.</returns>
    function IsClosed: Boolean;
  public { IZOracleTransaction }
    function GetTrHandle: POCITrans;
  public { IImmediatelyReleasable }
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
  public
    constructor CreateLocal(const Owner: TZOracleConnection);
    constructor CreateGlobal(const Owner: TZOracleConnection; TxnMode: TZOCITxnMode;
      SpawnMode: TZOCITxnSpawnMode; CoupleMode: TZOCITxnCoupleMode);
    procedure BeforeDestruction; override;
  end;

 {$ENDIF ZEOS_DISABLE_ORACLE}

  {** Implements a specialized cached resolver for Oracle. }
  TZOracleCachedResolver = class(TZGenerateSQLCachedResolver)
  public
    /// <author>Michael Seeger</author>
    /// <summary>Forms a SELECT statements to calculate default values.</summary>
    /// <param>"RowAccessor" an accessor object to column values.</param>
    /// <param>"ColumnsLookup" an TZIndexPairList which holds the NULL columns.</param>
    /// <returns>the composed SELECT SQL.</returns>
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): string; override;
  end;

{$IFNDEF ZEOS_DISABLE_ORACLE}


const
  CommitMode: array[Boolean] of ub4 = (OCI_DEFAULT, OCI_COMMIT_ON_SUCCESS);

var
  {** The common driver manager object. }
  OracleDriver: IZDriver;
{$ENDIF ZEOS_DISABLE_ORACLE}

implementation

uses {$IFNDEF UNICODE}ZDbcUtils,{$ENDIF}
  ZMessages, ZCollections, ZEncoding, ZSysUtils, ZFastCode,
  ZGenericSqlToken, ZOracleToken, ZOracleAnalyser,
  ZDbcOracleStatement, ZDbcOracleUtils, ZDbcOracleMetadata, ZDbcProperties;

{$IFNDEF ZEOS_DISABLE_ORACLE}

{ TZOracleDriver }

constructor TZOracleDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZOraclePlainDriver.Create));
end;

function TZOracleDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOracleConnection.Create(Url);
end;

function TZOracleDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOracleTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

function TZOracleDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZOracleStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZOracleConnection }

procedure TZOracleConnection.InternalSetCatalog(const Catalog: String);
begin
  ExecuteImmediat('ALTER SESSION SET CURRENT_SCHEMA = '+Catalog, lcOther);
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var Stmt: POCIStmt;
  Status: sword;
begin
  if IsClosed then
    Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then inherited ExecuteImmediat(SQL, LoggingCategory)
  else begin
    Stmt := nil;
    try
      ExecuteImmediat(SQL, Stmt, LoggingCategory);
    finally
      if Stmt <> nil then begin
        Status := FPlainDriver.OCIHandleFree(Stmt, OCI_HTYPE_STMT);
        if Status <> OCI_SUCCESS then
          HandleErrorOrWarning(FErrorHandle, Status, lcOther,
            {$IFDEF ZEOSDEBUG}'OCIHandleFree'{$ELSE}''{$ENDIF}, Self);
      end;
    end;
  end;
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: RawByteString;
  var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory);
var Status: sword;
begin
  if Pointer(SQL) = nil then
    Exit;
  if Stmt = nil then begin
    Status := FPlainDriver.OCIHandleAlloc(GetConnectionHandle,
      Stmt, OCI_HTYPE_STMT, 0, nil);
    if Status <> OCI_SUCCESS then
      HandleErrorOrWarning(FErrorHandle, Status, lcOther,
        'OCIHandleAlloc(OCIStmt-Handle)', Self);
    Status := FPlainDriver.OCIStmtPrepare(Stmt, FErrorHandle, Pointer(SQL),
      Length(SQL){$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF}, OCI_NTV_SYNTAX, OCI_DEFAULT);
    if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then begin
      {$IFDEF UNICODE}
      FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(FErrorHandle, Status, LoggingCategory,
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self);
    end;
  end;
  Status := FPlainDriver.OCIStmtExecute(FContextHandle,
      Stmt, FErrorHandle, 1, 0, nil, nil, CommitMode[AutoCommit]);
  if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then begin
    {$IFDEF UNICODE}
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
    {$ENDIF}
    HandleErrorOrWarning(FErrorHandle, Status, LoggingCategory,
      {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self);
  end;
end;

procedure TZOracleConnection.Open;
var
  Status: Integer;
  ncharset: ub2;
  Succeeded: Boolean;
  {$IFNDEF UNICODE}
  US: UnicodeString;
  {$ELSE}
  R: RawByteString;
  CP: Word;
  {$ENDIF}
  S: String;
  mode: ub4;
  procedure GetRawCharacterSet;
  {$IFNDEF UNICODE}
  var P: PWidechar;
      L: NativeUInt;
  {$ENDIF}
  begin
    With CreateStatement.ExecuteQuery('select VALUE from nls_database_parameters where parameter=''NLS_CHARACTERSET''') do begin

      if Next then begin
        {$IFDEF UNICODE}
        FLogMessage := GetUnicodeString(FirstDbcIndex);
        {$ELSE}
        P := GetPWideChar(FirstDbcIndex, L);
        FLogMessage := UnicodeStringToASCII7(P, L);
        {$ENDIF UNICODE}
        ResetCurrentClientCodePage(FLogMessage, True);
        { keep the w-encoding infos alive, just identify the raw CP}
        ConSettings.ClientCodePage.Encoding := ceUTF16;
        ConSettings.ClientCodePage.ID := OCI_UTF16ID;
      end;
      Close;
    end;
  end;
begin
  if not Closed then
     Exit;

  FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);

  { Sets a default port number. }
  if Port = 0 then
     Port := 1521;

  { Sets a client codepage. }
  if ConSettings.ClientCodePage = nil then begin
    FClientCodePage := Info.Values[ConnProps_CodePage];
    if FClientCodePage <> '' then
      CheckCharEncoding(FClientCodePage, True);
  end;
  if ConSettings.ClientCodePage = nil then begin
    fcharset := 0;
    {$IFDEF UNICODE} CP := ZOSCodePage; {$ENDIF}
  end else begin
    fcharset := ConSettings.ClientCodePage^.ID;
    {$IFDEF UNICODE} CP := ConSettings.ClientCodePage^.CP; {$ENDIF}
  end;
  //EH: do NOT use OCI_CLIENT_NCHARSET_ID if OCI_CLIENT_CHARSET_ID is zero!!
  if fcharset = 0
  then ncharset := 0
  else ncharset := OCI_UTF16ID;

  { Connect to Oracle database. }
  FErrorHandle := nil;
  mode := OCI_OBJECT;
  S := Info.Values[ConnProps_OCIMultiThreaded];
  if StrToBoolEx(S, False) Then
    mode := mode + OCI_THREADED;
  Status := FPlainDriver.OCIEnvNlsCreate(FOCIEnv, mode, nil, nil, nil,
    nil, 0, nil, fcharset, ncharset);
  if Status <> OCI_SUCCESS then
    HandleErrorOrWarning(FErrorHandle, Status, lcOther, 'EnvNlsCreate failed.', Self);
  FErrorHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  FServerHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FServerHandle, OCI_HTYPE_SERVER, 0, nil);
  FContextHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FContextHandle, OCI_HTYPE_SVCCTX, 0, nil);
  FDescibeHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FDescibeHandle, OCI_HTYPE_DESCRIBE, 0, nil);
  Succeeded := False;
  if fcharset = OCI_UTF16ID then begin
    {$IFDEF UNICODE}
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(URL.Database), Length(URL.Database) shl 1, 0);
    {$ELSE}
    US := ZRawToUnicode(URL.Database, GetW2A2WConversionCodePage(ConSettings));
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(US), Length(US) shl 1, 0);
    {$ENDIF}
  end else {$IFDEF UNICODE} begin
    R := ZUnicodeToRaw(URL.Database, CP);
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(R), Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, 0);
   end; {$ELSE}
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(URL.Database), Length(URL.Database), 0);
   {$ENDIF}
  if Status <> OCI_SUCCESS then try
    HandleErrorOrWarning(FErrorHandle, Status, lcConnect, FLogMessage, Self);
    Succeeded := True;
  finally
    if not Succeeded then
      FreeAllocatedHandles;
  end;

  if fcharset = 0 then begin
    FPlainDriver.OCIAttrGet(FOCIEnv, OCI_HTYPE_ENV, @fcharset,
      nil, OCI_NLS_CHARSET_ID, FErrorHandle); //Get Server default CodePage
    CheckCharEncoding(PlainDriver.ValidateCharEncoding(fcharset)^.Name);
    FPlainDriver.OCIAttrGet(FOCIEnv, OCI_HTYPE_ENV, @ncharset,
      nil, OCI_NLS_NCHARSET_ID, FErrorHandle);
    if ncharset <> OCI_UTF16ID then begin
      FreeAllocatedHandles;
      Open;//recursive call we can not patch the env varibles using OCIAttrSet
      Exit;
    end;
  end;
  FPlainDriver.OCINlsNumericInfoGet(FOCIEnv, FErrorHandle,
    @ConSettings^.ClientCodePage^.CharWidth, OCI_NLS_CHARSET_MAXBYTESZ);

  FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FServerHandle, 0,
    OCI_ATTR_SERVER, FErrorHandle);
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FSessionHandle, OCI_HTYPE_SESSION, 0, nil);
  if fcharset = OCI_UTF16ID then begin
    {$IFDEF UNICODE}
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(URL.UserName),
      Length(URL.UserName) shl 1, OCI_ATTR_USERNAME, FErrorHandle);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION,
      Pointer(Password), Length(Password) shl 1, OCI_ATTR_PASSWORD, FErrorHandle);
    {$ELSE}
    US := ZRawToUnicode(URL.UserName, GetW2A2WConversionCodePage(ConSettings));
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(US),
      Length(US) shl 1, OCI_ATTR_USERNAME, FErrorHandle);
    US := ZRawToUnicode(URL.Password, GetW2A2WConversionCodePage(ConSettings));
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(US),
      Length(US) shl 1, OCI_ATTR_PASSWORD, FErrorHandle);
    {$ENDIF}
  end else begin
    {$IFDEF UNICODE}
    R := ZUnicodeToRaw(URL.Password, CP);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(R),
      Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, OCI_ATTR_PASSWORD, FErrorHandle);
    R := ZUnicodeToRaw(URL.UserName, CP);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(R),
      Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, OCI_ATTR_USERNAME, FErrorHandle);
    {$ELSE}
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION,
      Pointer(Password), Length(Password), OCI_ATTR_PASSWORD, FErrorHandle);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(URL.UserName),
      Length(URL.UserName), OCI_ATTR_USERNAME, FErrorHandle);
    {$ENDIF}
  end;
  FPlainDriver.OCIAttrSet(FSessionHandle,OCI_HTYPE_SESSION,@fBlobPrefetchSize,0,
    OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE,FErrorHandle);
  Succeeded := False;
  S := Info.Values[ConnProps_OCIAuthenticateMode];
  Mode := {$IFDEF UNICODE}UnicodeToUInt32Def{$ELSE}RawToUInt32Def{$ENDIF}(S, OCI_DEFAULT);
  Status := FPlainDriver.OCISessionBegin(FContextHandle, FErrorHandle,
    FSessionHandle, OCI_CRED_RDBMS, Mode);
  if Status <> OCI_SUCCESS then try
    HandleErrorOrWarning(FErrorHandle, Status, lcConnect, FLogMessage, Self);
    Succeeded := True;
  finally
    if not Succeeded then
      FreeAllocatedHandles;
  end;
  FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FSessionHandle, 0,
    OCI_ATTR_SESSION, FErrorHandle);
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  fLocalTransaction := TZOracleTransaction.CreateLocal(Self);
  SetActiveTransaction(fLocalTransaction);
  fLocalTransaction.StartTransaction;
  inherited Open;
  if FCatalog <> '' then
    InternalSetCatalog(FCatalog);
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID then
    GetRawCharacterSet;
end;

function TZOracleConnection.StartTransaction: Integer;
begin
  if Closed then
    Open;
  Result := fAttachedTransaction.StartTransaction;
  AutoCommit := False;
end;

Function TZOracleConnection.AbortOperation: Integer;
Begin
  // https://docs.oracle.com/cd/B10501_01/appdev.920/a96584/oci16m96.htm
  Result := FPlainDriver.OCIBreak(FContextHandle, FErrorHandle);
  if Result <> OCI_SUCCESS then
    HandleErrorOrWarning(FErrorHandle, Result, lcOther, 'Abort operation', Self);
  Result := 0; //only possible if CheckOracleError dosn't raise an exception
End;

procedure TZOracleConnection.AfterConstruction;
begin
  FPlainDriver := PlainDriver.GetInstance as TZOraclePlainDriver;
  FMetaData := TZOracleDatabaseMetadata.Create(Self, URL);
  inherited AfterConstruction;
  fGlobalTransactions[False] := TZCollection.Create;
  fGlobalTransactions[True ] := TZCollection.Create;
  TransactIsolationLevel := tiReadCommitted;

  { Sets a default properties }
  if Self.Port = 0 then
    Self.Port := 1521;
  FLogMessage := Info.Values[ConnProps_ServerCachedStmts];
  if (FLogMessage = '') or StrToBoolEx(FLogMessage, False)
  then FStmtMode := OCI_STMT_CACHE //use by default
  else FStmtMode := OCI_DEFAULT;
  FLogMessage := Info.Values[ConnProps_StatementCache];
  FStatementPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(FLogMessage, 30); //default = 20
  FLogMessage := '';
  FBlobPrefetchSize := StrToIntDef(Info.Values[ConnProps_BlobPrefetchSize], 8*1024);
end;

procedure TZOracleConnection.FreeAllocatedHandles;
begin
  if FSessionHandle <> nil then begin
    FPlainDriver.OCIHandleFree(FSessionHandle, OCI_HTYPE_SESSION);
    FSessionHandle := nil;
  end;
  if FDescibeHandle <> nil then begin
    FPlainDriver.OCIHandleFree(FDescibeHandle, OCI_HTYPE_DESCRIBE);
    FDescibeHandle := nil;
  end;
  if FContextHandle <> nil then begin
    FPlainDriver.OCIHandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
    FContextHandle := nil;
  end;
  if FErrorHandle <> nil then begin
    FPlainDriver.OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
  end;
  if FServerHandle <> nil then begin
    FPlainDriver.OCIHandleFree(FServerHandle, OCI_HTYPE_SERVER);
    FServerHandle := nil;
  end;
  if FOCIEnv <> nil then begin
    FPlainDriver.OCIHandleFree(FOCIEnv, OCI_HTYPE_ENV);
    FOCIEnv := nil;
  end;
end;

procedure TZOracleConnection.ClearWarnings;
begin
   FreeAndNil(fWarning);
end;

procedure TZOracleConnection.Commit;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  fAttachedTransaction.Commit;
  if fAttachedTransaction.IsClosed then begin
    fAttachedTransaction.StartTransaction;
    AutoCommit := not FRestartTransaction;
  end;
end;

procedure TZOracleConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
  B: Boolean;
  I: Integer;
begin
  if (fAttachedTransaction <> nil) and (fAttachedTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and (imm <> Sender) then begin
    imm.ReleaseImmediat(Sender, AError);
    fAttachedTransaction := nil;
  end;
  if (fLocalTransaction <> nil) and (fLocalTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and (imm <> Sender) then begin
    imm.ReleaseImmediat(Sender, AError);
    fLocalTransaction := nil;
  end;
  for b := false to true do
    if fGlobalTransactions[b] <> nil then begin
      for i := fGlobalTransactions[b].Count -1 downto 0 do
        if (fGlobalTransactions[b] <> nil) and (fGlobalTransactions[b].QueryInterface(IImmediatelyReleasable, imm) = S_OK) and (imm <> Sender) then
          imm.ReleaseImmediat(Sender, AError);
      fGlobalTransactions[b].Clear;
    end;
  inherited ReleaseImmediat(Sender, AError);
  FreeAllocatedHandles;
end;

procedure TZOracleConnection.ReleaseTransaction(
  const Transaction: IZTransaction);
var OraTxn: IZOracleTransaction;
  I: Integer;
  B: Boolean;
begin
  if (Transaction<> nil) then begin
    if (Transaction.QueryInterface(IZOracleTransaction, OraTxn) = S_OK) then begin
      if (fAttachedTransaction = OraTxn) then
        fAttachedTransaction := nil;
      if (fLocalTransaction = OraTxn) then
        fLocalTransaction := nil;
      for B := False to True do begin
        I := fGlobalTransactions[b].IndexOf(Transaction);
        if I <> -1 then
          fGlobalTransactions[b].Delete(I);
      end;
    end else raise EZSQLException.Create('unknown transaction');
  end;
end;

procedure TZOracleConnection.Rollback;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  fAttachedTransaction.Rollback;
  if fAttachedTransaction.IsClosed then begin
    fAttachedTransaction.StartTransaction;
    AutoCommit := not FRestartTransaction;
  end;
end;

function TZOracleConnection.PingServer: Integer;
begin
  if Closed or (FContextHandle = nil) Or (FErrorHandle = nil)
    then Result := -1
  else begin
    Result := FPlainDriver.OCIPing(FContextHandle, FErrorHandle, OCI_DEFAULT);
    if Result <> OCI_SUCCESS then
      HandleErrorOrWarning(FErrorHandle, Result, lcOther, 'PingServer', Self);
    Result := 0; //only possible if no exception is raised
  end;
end;

function TZOracleConnection.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOracleCallableStatement_W.Create(Self, Name, Params)
  else Result := TZOracleCallableStatement_A.Create(Self, Name, Params);
end;

function TZOracleConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOraclePreparedStatement_W.Create(Self, SQL, Info)
  else Result := TZOraclePreparedStatement_A.Create(Self, SQL, Info);
end;

procedure TZOracleConnection.InternalClose;
var
  LogMessage: String;
  B: Boolean;
  Status: sword;
begin
  if Closed or not Assigned(PlainDriver) then
    Exit;
  LogMessage := 'DISCONNECT FROM "'+URL.Database+'"';
  try
    for B := False to True do
      fGlobalTransactions[b].Clear;
    fAttachedTransaction := nil;
    fLocalTransaction := nil;
  finally
    try
      if FSessionHandle <> nil then begin
        { Closes the session }
        Status := FPlainDriver.OCISessionEnd(FContextHandle, FErrorHandle, FSessionHandle,
          OCI_DEFAULT);
        if Status <> OCI_SUCCESS then
          HandleErrorOrWarning(FErrorHandle, Status, lcDisconnect, LogMessage, Self);
      end;
      if FServerHandle <> nil then begin
        { Detaches from the server }
        Status := FPlainDriver.OCIServerDetach(FServerHandle, FErrorHandle, OCI_DEFAULT);
        if Status <> OCI_SUCCESS then
          HandleErrorOrWarning(FErrorHandle, Status, lcDisconnect, LogMessage, Self);
      end;
    finally
      FreeAllocatedHandles;
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, URL.Protocol, LogMessage);
    end;
  end;
end;

function TZOracleConnection.GetCatalog: string;
begin
  if not Closed and (FCatalog = '') then
    with CreateStatementWithParams(nil).ExecuteQuery('SELECT SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'') FROM DUAL') do begin
      if Next then
        FCatalog := GetString(FirstDBCIndex);
      Close;
    end;
  Result := FCatalog;
end;

procedure TZOracleConnection.SetActiveTransaction(const Value: IZTransaction);
var OCITA: IZOracleTransaction;
  Status: sword;
begin
  OCITA := Value as IZOracleTransaction;
  { set new transaction handle to service context }
  Status := FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, OCITA.GetTrHandle, 0,
    OCI_ATTR_TRANS, FErrorHandle);
  if Status <> OCI_SUCCESS then
    HandleErrorOrWarning(FErrorHandle, Status, lcTransaction, 'OCIAttrSet(OCI_ATTR_TRANS)', Self);
  if FStmtMode = OCI_STMT_CACHE then begin
    Status := FPlainDriver.OCIAttrSet(FContextHandle,OCI_HTYPE_SVCCTX,
      @FStatementPrefetchSize, 0, OCI_ATTR_STMTCACHESIZE, FErrorHandle);
    if Status <> OCI_SUCCESS then
    HandleErrorOrWarning(FErrorHandle, Status, lcTransaction, 'OCIAttrSet(OCI_ATTR_STMTCACHESIZE)', Self);
  end;
  fAttachedTransaction := OCITA;
end;

procedure TZOracleConnection.SetAutoCommit(Value: Boolean);
begin
  AutoCommit := Value;
  FRestartTransaction := not Value;
end;

procedure TZOracleConnection.SetCatalog(const Value: string);
begin
  if Value <> FCatalog then begin
    FCatalog := Value;
    if not Closed and (Value <> '') then
      InternalSetCatalog(Value);
  end;
end;

procedure TZOracleConnection.SetReadOnly(Value: Boolean);
begin
   if (Value and (TransactIsolationLevel = tiSerializable)) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  ReadOnly := Value;
end;

procedure TZOracleConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone
  then Level := tiReadCommitted
  else if (Level = tiReadUncommitted) or (Level = tiRepeatableRead) or
    (Readonly and (Level = tiSerializable)) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  if TransactIsolationLevel <> Level then begin
    if not Closed then
      if AutoCommit
      then fLocalTransaction := nil //reastart new TA
      else raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
    TransactIsolationLevel := Level;
  end;
end;

function TZOracleConnection.CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;
begin
  Result := TZOracleSequence.Create(Self, Sequence, BlockSize);
end;

function TZOracleConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOracleStatement_W.Create(Self, Info)
  else Result := TZOracleStatement_A.Create(Self, Info);
end;

function ZDbc2OCITxnMode(ReadOnly: Boolean; TIL: TZTransactIsolationLevel): TZOCITxnMode;
begin
  if (ReadOnly and (TIL = tiSerializable)) or
      (TIL = tiReadUncommitted) or
      (TIL = tiRepeatableRead) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  if ReadOnly then
    Result := tmReadOnly
  else if TIL = tiSerializable then
    Result := tmSerializable
  else if TIL = tiReadCommitted then
    Result := tmReadWrite
  else Result := tmDefault;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "AutoCommit,Params" not used} {$ENDIF}
function TZOracleConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
var TxnMode: TZOCITxnMode;
begin
  //2Phase Txn/global?
  TxnMode := ZDbc2OCITxnMode(ReadOnly, TransactIsolationLevel);
  Result := TZOracleTransaction.CreateGlobal(Self, TxnMode, smNew, cmLoosely);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOracleConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var Stmt: POCIStmt;
  Status: sword;
begin
  if IsClosed then
    Open;

  if ConSettings.ClientCodePage.ID <> OCI_UTF16ID
  then inherited ExecuteImmediat(SQL, LoggingCategory)
  else begin
    Stmt := nil;
    try
      ExecuteImmediat(SQL, Stmt, LoggingCategory);
    finally
      if Stmt <> nil then begin
        Status := FPlainDriver.OCIHandleFree(Stmt, OCI_HTYPE_STMT);
        if Status <> OCI_SUCCESS then
          HandleErrorOrWarning(FErrorHandle, Status, lcOther, 'OCIHandleFree(OCIStmt-Handle)', Self);
      end;
    end;
  end;
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: UnicodeString;
  var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory);
var Status: sword;
begin
  if Pointer(SQL) = nil then
    Exit;
  if Stmt = nil then begin
    Status := FPlainDriver.OCIHandleAlloc(GetConnectionHandle,
      Stmt, OCI_HTYPE_STMT, 0, nil);
    if Status <> OCI_SUCCESS then
      HandleErrorOrWarning(FErrorHandle, Status, lcOther, 'OCIHandleAlloc(OCIStmt-Handle)', Self);
    Status := FPlainDriver.OCIStmtPrepare(Stmt, FErrorHandle, Pointer(SQL),
      (Length(SQL)+1) shl 1, OCI_NTV_SYNTAX, OCI_DEFAULT);
    if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then begin
      {$IFNDEF UNICODE}
      FLogMessage := ZUnicodeToRaw(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(FErrorHandle, Status, lcPrepStmt,
        {$IFNDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self);
    end;
  end;
  Status := FPlainDriver.OCIStmtExecute(FContextHandle,
      Stmt, FErrorHandle, 1, 0, nil, nil, CommitMode[AutoCommit]);
  if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then begin
    {$IFNDEF UNICODE}
    FLogMessage := ZUnicodeToRaw(SQL, ConSettings.ClientCodePage.CP);
    {$ENDIF}
    HandleErrorOrWarning(FErrorHandle, Status, LoggingCategory,
      {$IFNDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, Self);
  end;
end;

function TZOracleConnection.GetConnectionHandle: POCIEnv;
begin
  Result := FOCIEnv;
end;

function TZOracleConnection.GetServiceContextHandle: POCISvcCtx;
begin
  Result := FContextHandle;
end;

function TZOracleConnection.GetErrorHandle: POCIError;
begin
  Result := FErrorHandle;
end;

function TZOracleConnection.GetServerHandle: POCIServer;
begin
  Result := FServerHandle;
end;

function TZOracleConnection.GetServerProvider: TZServerProvider;
begin
  Result := spOracle;
end;

function TZOracleConnection.GetSessionHandle: POCISession;
begin
  Result := FSessionHandle;
end;

function TZOracleConnection.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZOracleStatementAnalyser.Create;
end;

function TZOracleConnection.GetTokenizer: IZTokenizer;
begin
  Result := TZOracleTokenizer.Create;
end;

function TZOracleConnection.GetTransactionHandle: POCITrans;
begin
  Result := fAttachedTransaction.GetTrHandle;
end;

procedure TZOracleConnection.HandleErrorOrWarning(ErrorHandle: POCIError;
  Status: sword; LogCategory: TZLoggingCategory; const LogMessage: SQLString;
  const Sender: IImmediatelyReleasable);
var
  {$IFDEF UNICODE}
  ErrorMessageA: RawByteString;
  {$ELSE}
  ErrorMessageW: UnicodeString;
  {$ENDIF}
  CP: Word;
  WriterA: TZRawSQLStringWriter;
  WriterW: TZUnicodeSQLStringWriter;
  FirstErrorCode, ErrorCode: SB4;
  ErrorMessage: SQLString;
  FormatStr: String;
  L: NativeUInt;
  I: ub4;
  NewStatus: sword;
  AExceptionClass: EZSQLThrowableClass;
  AException: EZSQLThrowable;
  ErrorString: String;
  label jmpConcat;
begin
  if Status = OCI_SUCCESS then Exit;
  ErrorCode := Status;
  FirstErrorCode := Status;
  AException := nil;
  AExceptionClass := EZSQLException;
  ErrorMessage := '';
  if ConSettings.ClientCodePage <> nil
  then CP := ConSettings.ClientCodePage.CP
  else CP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
  case Status of
    OCI_SUCCESS_WITH_INFO: begin
        AExceptionClass := EZSQLWarning;
        goto JmpConcat;
      end;
    OCI_NEED_DATA:  ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:    ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
JmpConcat:
      if ConSettings.ClientCodePage.ID = OCI_UTF16ID then begin
        WriterW := TZUnicodeSQLStringWriter.Create(1024);
        I := 1;
        {$IFNDEF UNICODE}ErrorMessageW := '';{$ENDIF}
        try
          while true do begin
            NewStatus := FPlainDriver.OCIErrorGet(ErrorHandle, I, nil, ErrorCode,
              @FByteBuffer[0], SizeOf(TByteBuffer)-1, OCI_HTYPE_ERROR);
            if NewStatus = OCI_NO_DATA  then
              Break;
            if (i > 1)
            then WriterW.AddLineFeedIfNotEmpty({$IFDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageW{$ENDIF})
            else begin
              FirstErrorCode := ErrorCode;
              if (FirstErrorCode = ORA_03113_end_of_file_on_communication_channel) or
                 (FirstErrorCode = ORA_03135_connection_lost_contact) Or
                 (FirstErrorCode = ORA_01089_immediate_shutdown_or_close_in_progress) Or
                 (FirstErrorCode = ORA_03114_not_connected_to_ORACLE) and (LogCategory <> lcConnect) then //disconnect
                AExceptionClass := EZSQLConnectionLost;
            end;
            L := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(PWideChar(@fByteBuffer[0]));
            If (NewStatus = OCI_ERROR) And (L = 0) and (Logmessage <> '') Then
            Begin
              {$IFDEF UNICODE}
              WriterW.AddText(Logmessage, ErrorMessage);
              {$ELSE !UNICODE}
              WriterW.AddText(ZRawToUnicode(Logmessage, CP), ErrorMessageW);
              {$ENDIF !UNICODE}
              Break;
            End;
            WriterW.AddText(@FByteBuffer[0], L, {$IFDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageW{$ENDIF});
            Inc(I);
          end;
          WriterW.Finalize({$IFDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageW{$ENDIF});
        finally
          FreeAndNil(WriterW);
        end;
        {$IFNDEF UNICODE}
        CP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
        ErrorMessage := ZUnicodeToRaw(ErrorMessageW, CP);
        ErrorMessageW := '';
        {$ENDIF}
      end else begin
        I := 1;
        WriterA := TZRawSQLStringWriter.Create(1024);
        {$IFDEF UNICODE}ErrorMessageA := EmptyRaw;{$ENDIF}
        try
          while true do begin
            NewStatus := FPlainDriver.OCIErrorGet(ErrorHandle, I, nil, ErrorCode,
              @FByteBuffer[0], SizeOf(TByteBuffer)-1, OCI_HTYPE_ERROR);
            if NewStatus = OCI_NO_DATA then
              Break;
            if (i > 1)
            then WriterA.AddLineFeedIfNotEmpty({$IFNDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageA{$ENDIF})
            else begin
              FirstErrorCode := ErrorCode;
              if (FirstErrorCode = ORA_03113_end_of_file_on_communication_channel) or
                 (FirstErrorCode = ORA_03135_connection_lost_contact) Or
                 (FirstErrorCode = ORA_01089_immediate_shutdown_or_close_in_progress) Or
                 ((FirstErrorCode = ORA_03114_not_connected_to_ORACLE) and (LogCategory <> lcConnect)) then //disconnect
                AExceptionClass := EZSQLConnectionLost;
            end;
            L := StrLen(PAnsiChar(@fByteBuffer[0]));

            If (NewStatus = OCI_ERROR) And (L = 0) and (Logmessage <> '') Then
            Begin
              {$IFDEF UNICODE}
              WriterA.AddText(ZUnicodeToRaw(Logmessage, CP), ErrorMessageA);
              {$ELSE !UNICODE}
              WriterA.AddText(Logmessage, ErrorMessage);
              {$ENDIF !UNICODE}
              Break;
            End;
            WriterA.AddText(@FByteBuffer[0], L, {$IFNDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageA{$ENDIF});
            Inc(I);
          end;
          WriterA.Finalize({$IFNDEF UNICODE}ErrorMessage{$ELSE}ErrorMessageA{$ENDIF});
        finally
          FreeAndNil(WriterA);
        end;
        {$IFDEF UNICODE}
        ErrorMessage := ZRawToUnicode(ErrorMessageA, CP);
        ErrorMessageA := EmptyRaw;
        {$ENDIF}
      end;
    OCI_INVALID_HANDLE:   ErrorMessage := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:  ErrorMessage := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:         ErrorMessage := 'OCI_CONTINUE';
    else                  ErrorMessage := '';
  end;
  if ErrorMessage = '' then Exit;
  if DriverManager.HasLoggingListener then
    LogError(LogCategory, FirstErrorCode, Sender, LogMessage, ErrorMessage);
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
    if LogCategory in [lcExecute, lcTransaction, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
  then ErrorString := Format(FormatStr, [ErrorMessage, FirstErrorCode, LogMessage])
  else ErrorString := Format(FormatStr, [ErrorMessage, FirstErrorCode]);
  AException := AExceptionClass.CreateWithCode(FirstErrorCode, ErrorString);
  if (Status = OCI_SUCCESS_WITH_INFO) then begin
    ClearWarnings;
    if not RaiseWarnings or (LogCategory = lcConnect) then begin
      FWarning := EZSQLWarning(AException);
      AException := nil;
    end;
  end else if AExceptionClass = EZSQLConnectionLost then
    if Sender <> nil
    then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(aException))
    else ReleaseImmediat(Sender, EZSQLConnectionLost(aException));
  if AException <> nil then
    raise AException;
end;

function TZOracleConnection.GetDescribeHandle: POCIDescribe;
begin
  Result := FDescibeHandle;
end;

function TZOracleConnection.GetClientVersion: Integer;
var
  major_version, minor_version, update_num,
      patch_num, port_update_num: sword;
begin
  FPlainDriver.OCIClientVersion(@major_version, @minor_version, @update_num,
      @patch_num, @port_update_num);
  Result := EncodeSQLVersioning(major_version,minor_version,update_num);
end;

function TZOracleConnection.GetHostVersion: Integer;
var
  buf:text;
  version:ub4;
begin
  result:=0;
  getmem(buf,1024);
  if FPlainDriver.OCIServerRelease(FServerHandle,FErrorHandle,buf,1024,OCI_HTYPE_SERVER,@version)=OCI_SUCCESS then
    Result := EncodeSQLVersioning((version shr 24) and $ff,(version shr 20) and $f,(version shr 12) and $ff);
  freemem(buf);
end;

function TZOracleConnection.GetPlainDriver: TZOraclePlainDriver;
begin
  Result := FPlainDriver;
end;

function TZOracleConnection.GetBinaryEscapeString(const Value: TBytes): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Value);
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, L*2+2);
  P := PChar(Result);
  P^ := #39;
  Inc(p);
  ZBinToHex(PAnsiChar(Value), P, L);
  (P+L)^ := #39;
end;

{ TZOracleTransaction }

procedure TZOracleTransaction.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Close;
  fSavepoints.Free;
end;

procedure TZOracleTransaction.Close;
var Status: sword;
begin
  if FOCITrans <> nil then try
    fSavepoints.Clear;
    if FStarted then
      RollBack;
  finally
    if FOCITrans <> nil then begin
      Status := FOwner.FPlainDriver.OCIHandleFree(FOCITrans, OCI_HTYPE_TRANS);
      FOCITrans := nil;
      if (Status <> OCI_SUCCESS) and (FRefCount > 0) then
        FOwner.HandleErrorOrWarning(FOwner.FErrorHandle, Status, lcTransaction, 'OCIHandleFree', Self);
    end;
  end;
end;

procedure TZOracleTransaction.Commit;
var Status: sword;
begin
  if fSavepoints.Count > 0
  then fSavepoints.Delete(fSavepoints.Count -1) //oracle,sybase,sqlserver do not support a release savepoint syntax
  else if not FStarted then
    raise EZSQLException.Create(SCannotUseCommit)
  else try
    Status := FOwner.FPlainDriver.OCITransCommit(FOwner.FContextHandle,
      FOwner.FErrorHandle, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      FOwner.HandleErrorOrWarning(FOwner.FErrorHandle, Status, lcTransaction, 'TRANSACTION COMMIT', Self);
  finally
    FStarted := False;
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, FOwner.URL.Protocol, 'TRANSACTION COMMIT');
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Owner" not used} {$ENDIF}
constructor TZOracleTransaction.CreateGlobal(const Owner: TZOracleConnection;
  TxnMode: TZOCITxnMode; SpawnMode: TZOCITxnSpawnMode;
  CoupleMode: TZOCITxnCoupleMode);
begin
  inherited Create;
  FTxnMode := TxnMode;
  FSpawnMode := SpawnMode;
  FCoupleMode := CoupleMode;
  fDoLog := True;
  fBranches := TZCollection.Create;
  FOwner.FPlainDriver.OCIHandleAlloc(FOwner.FOCIEnv, FOCITrans, OCI_HTYPE_TRANS, 0, nil);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

constructor TZOracleTransaction.CreateLocal(const Owner: TZOracleConnection);
begin
  inherited Create;
  { alloc transaction handle }
  FOwner := Owner;
  FOwner.FPlainDriver.OCIHandleAlloc(FOwner.FOCIEnv, FOCITrans, OCI_HTYPE_TRANS, 0, nil);
  fSavepoints := TStringList.Create;
  ConSettings := Owner.ConSettings;
  fLocal := True;
end;

function TZOracleTransaction.GetAutoCommit: Boolean;
begin
  if FOwner <> nil
  then Result := FOwner.AutoCommit
  else Result := True;
end;

function TZOracleTransaction.GetConnection: IZConnection;
begin
  Result := FOwner;
end;

function TZOracleTransaction.GetTransactionLevel: Integer;
begin
  if FStarted
  then Result := Ord(not FOwner.AutoCommit)+fSavepoints.Count
  else Result := -1;
end;

function TZOracleTransaction.GetTrHandle: POCITrans;
begin
  Result := FOCITrans
end;

function TZOracleTransaction.IsClosed: Boolean;
begin
  Result := not FStarted;
end;

function TZOracleTransaction.IsReadOnly: Boolean;
begin
  if FOwner <> nil
  then Result := FOwner.ReadOnly
  else Result := True;
end;

procedure TZOracleTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var imm: IImmediatelyReleasable;
    Status: sword;
begin
  fSavepoints.Clear;
  FStarted := False;
  if fBranches <> nil then
    fBranches.Clear;
  if FOCITrans <> nil then begin
    Status := FOwner.FPlainDriver.OCIHandleFree(FOCITrans, OCI_HTYPE_TRANS);
    FOCITrans := nil;
    if Status <> OCI_SUCCESS then
      FOwner.HandleErrorOrWarning(FOwner.FErrorHandle, Status, lcTransaction, 'OCIHandleFree', Self);
  end;
  if (FOwner <> nil) then begin
    FOwner.QueryInterface(IImmediatelyReleasable, imm);
    if (imm <> Sender) then
      imm.ReleaseImmediat(Self, AError);
  end;
end;

procedure TZOracleTransaction.Rollback;
var Status: sword;
begin
  if fSavepoints.Count > 0 then begin
    FOwner.ExecuteImmediat('ROLLBACK TO '+fSavepoints[fSavepoints.Count-1], lcTransaction);
    fSavepoints.Delete(fSavepoints.Count -1);
  end else if not FStarted then
    raise EZSQLException.Create(SCannotUseRollback)
  else try
    Status := FOwner.FPlainDriver.OCITransRollback(FOwner.FContextHandle,
      FOwner.FErrorHandle, OCI_DEFAULT);
    if (Status <> OCI_SUCCESS) and (FRefCount > 0) then
      FOwner.HandleErrorOrWarning(FOwner.FErrorHandle, Status, lcTransaction, 'TRANSACTION ROLLBACK', Self);
  finally
    FStarted := False;
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, FOwner.URL.Protocol, 'TRANSACTION ROLLBACK');
  end;
end;

const
  OCITransStartFlagsLog: Array[TZOCITxnMode] of String = (
    'SET TRANSACTION ISOLATION LEVEL DEFAULT',
    'SET TRANSACTION ISOLATION LEVEL READONLY',
    'SET TRANSACTION ISOLATION LEVEL READWRITE',
    'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');

  OCITransStartTILFlags: Array[TZOCITxnMode] of ub4 = (
    OCI_DEFAULT, OCI_TRANS_READONLY, OCI_TRANS_READWRITE, OCI_TRANS_SERIALIZABLE);
  OCITransStartCoupleFlags: Array[TZOCITxnCoupleMode] of ub4 = (
    OCI_TRANS_TIGHT, OCI_TRANS_LOOSE);
  OCITransStartSpawnFlags: Array[TZOCITxnSpawnMode] of ub4 = (
    OCI_TRANS_NEW, OCI_TRANS_JOIN, OCI_TRANS_RESUME);

procedure TZOracleTransaction.SetReadOnly(Value: Boolean);
begin
  if FOwner <> nil then
    FOwner.SetReadOnly(Value);
end;

procedure TZOracleTransaction.SetTransactionIsolation(
  Value: TZTransactIsolationLevel);
begin
  if FOwner <> nil then
    FOwner.SetTransactionIsolation(Value);
end;

function TZOracleTransaction.StartTransaction: Integer;
var
  Status: Integer;
  TxnFlags: ub4;
  S: String;
begin
  if FStarted then
    if FOwner.AutoCommit
    then Result := 1
    else begin
      //just numbered names do disturb the oracle .. you'll get invalid statement error
      S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
      FOwner.ExecuteImmediat('SAVEPOINT '+S, lcTransaction);
      Result := FSavePoints.Add(S)+2;
    end
  else begin
    if FLocal then
      FTxnMode := ZDbc2OCITxnMode(FOwner.ReadOnly, FOwner.TransactIsolationLevel);
    TxnFlags := OCITransStartTILFlags[FTxnMode];
    if not FLocal then begin
      TxnFlags := TxnFlags or OCITransStartCoupleFlags[FCoupleMode];
      TxnFlags := TxnFlags or OCITransStartSpawnFlags[FSpawnMode];
    end;
    Status := FOwner.FPlainDriver.OCITransStart(FOwner.FContextHandle,
      FOwner.FErrorHandle, 0, TxnFlags);
    if Status <> OCI_SUCCESS then
      FOwner.HandleErrorOrWarning(FOwner.FErrorHandle, Status, lcTransaction, OCITransStartFlagsLog[FTxnMode], Self);
    FStarted := True;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, FOwner.URL.Protocol, OCITransStartFlagsLog[FTxnMode]);
    Result := Ord(not FOwner.AutoCommit);
  end;
end;

{$ENDIF ZEOS_DISABLE_ORACLE}

{ TZOracleCachedResolver }

function TZOracleCachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): string;
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(RowAccessor, ColumnsLookup);
  if Result <> '' then
  begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0 then
    begin
      Result := copy(Result, 1, iPos+3) + ' DUAL';
    end
    else
    begin
      Result := Result + ' FROM DUAL';
    end;
  end;
end;

{$IFNDEF ZEOS_DISABLE_ORACLE}

initialization
  OracleDriver := TZOracleDriver.Create;
  DriverManager.RegisterDriver(OracleDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OracleDriver);
  OracleDriver := nil;
{$ENDIF ZEOS_DISABLE_ORACLE}


end.
