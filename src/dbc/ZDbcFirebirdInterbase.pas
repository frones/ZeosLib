{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{             Originally written by EgonHugeist           }
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

unit ZDbcFirebirdInterbase;

interface

{$I ZDbc.inc}

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD, Math, Types,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$IFDEF WITH_INFINATE_DECLARED_IN_UNIT_WINDOWS}Windows,{$ENDIF}//old delphi <= 2010 declared const INFINITE in windows unit
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZCollections, ZClasses, ZCompatibility,
  ZPlainFirebirdInterbaseDriver, ZDbcInterbase6Utils, ZDbcLogging,
  ZDbcIntfs, ZDbcConnection, ZTokenizer, ZGenericSqlAnalyser, ZDbcCache,
  ZDbcGenericResolver, ZDbcResultSetMetadata, ZDbcCachedResultSet, ZDbcUtils,
  ZDbcResultSet, ZDbcStatement;

type
  /// <summary>Implements Interbase or Firebird Database Driver</summary>
  TZInterbaseFirebirdDriver = class(TZAbstractDriver)
  public
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer; override;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  /// <author>Fr0sT</author>
  /// <summary>Forward implementation of a Interbase or Firebird GUID properties
  ///  object</summary>
  TZInterbaseFirebirdConnectionGUIDProps = class;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a Interbase or Firebird transaction interface.</summary>
  IZInterbaseFirebirdTransaction = interface(IZTransaction)
    ['{A30246BA-AFC0-43FF-AB56-AB272281A3C2}']
    /// <summary>Immediat start an active transaction.</summary>
    procedure DoStartTransaction;
    /// <summary>Registers an open cursor. As long the resultset isn't fetched
    ///  completely, the transaction is referenced the ResultSet object. If the
    ///  fetch is complete, an DeRegisterOpenCursor call will be done and the
    ///  possible orphan transaction will be "hard" commited or rolled back as
    ///  requested by user.</summary>
    /// <param>"CursorRS" the resultset object which requires this transaction
    ///  object.</param>
    procedure RegisterOpencursor(const CursorRS: IZResultSet);
    /// <summary>Registers an open LOB. As long the LOB isn't released,
    ///  the transaction is referenced the LOB object. If the
    ///  object is released, an DeRegisterOpenUnCachedLob call will be done and
    ///  the possible orphan transaction will be "hard" commited or rolled back
    ///  as requested by user.</summary>
    /// <param>"Lob" the LOB object which requires this transaction object.</param>
    procedure RegisterOpenUnCachedLob(const Lob: IZlob);
    procedure DeRegisterOpenCursor(const CursorRS: IZResultSet);
    procedure DeRegisterOpenUnCachedLob(const Lob: IZlob);
    function GetOpenCursorCount: Integer;
    function GetTPB: RawByteString;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a Interbase or Firebird specific connection interface.
  ///  Usualy all suplimentary methods of the IZConnection interface should be
  ///  visible for the "generic" user. Querying that interface should be done by
  ///  an "advanced" user.</summary>
  IZInterbaseFirebirdConnection = interface (IZConnection)
    ['{B4B4136F-3692-454A-8F22-6C5EEE247BC0}']
    /// <summary>Return Interbase/Firebird dialect number. The dialect  must be
    ///  1 or 2 or 3.</summary>
    /// <returns>the dialect number</returns>
    function GetDialect: Word;
    function GetXSQLDAMaxSize: Cardinal;
    function GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetSubTypeTextCharSetID(const TableName, ColumnName: String): Integer;
    /// <summary>Determine if the Client lib-module is a Firebird lib</summary>
    /// <returns><c>True</c>If it's a Firebird client lib; <c>False</c>
    ///  otherwise</returns>
    function IsFirebirdLib: Boolean;
    /// <summary>Determine if the Client lib-module is a Interbase lib</summary>
    /// <returns><c>True</c>If it's a Interbase client lib; <c>False</c>
    ///  otherwise</returns>
    function IsInterbaseLib: Boolean;
    function GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
    function GetByteBufferAddress: PByteBuffer;
    /// <summary>Handles possible sql errors or warnings. Each error will raise
    ///  an EZSQLException. Warnings may be suppressed, see RaiseWarnings.</summary>
    /// <param>"LogCategory" a logging category if LoggingListeners are
    ///  registeted on the DriverManager</param>
    /// <param>"StatusVector" a status vector reference. It contain information
    ///  about errors or warnings</param>
    /// <param>"LogMessage" a sql query command or another message which may be
    ///  logged or added to the Exception string. See
    ///  AddLogMsgToExceptionOrWarningMsg</param>
    /// <param>"Sender" the calling object to be used on connection loss</param>
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      StatusVector: PARRAY_ISC_STATUS; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
  end;

  TZFirebirdInterbaseEventList = class;

  TZFirebirdInterbaseEventListClass = class of TZFirebirdInterbaseEventList;

  TZFirebirdInterbaseEventAlert = procedure(Sender: TObject; EventName: string;
    EventCount: longint; var CancelAlerts: boolean) of object;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a TZInterbaseFirebirdConnection connection object.</summary>
  TZInterbaseFirebirdConnection = Class(TZAbstractDbcConnection)
  private
    FWeakTransactionManagerPtr: Pointer;
  protected
    FIsFirebirdLib: Boolean; // never use this directly, always use IsFirbirdLib
    FIsInterbaseLib: Boolean; // never use this directly, always use IsInterbaseLib
    FHardCommit: boolean;
    FDialect: Word;
    FProcedureTypesCache, FSubTypeTestCharIDCache: TStrings;
    FGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    fTransactions: IZCollection; //simultan (not nested) readonly/readwrite transaction container
    fActiveTransaction: IZInterbaseFirebirdTransaction;
    FPB_CP: Word; //the parameter buffer codepage
    FClientVersion: Integer;
    FHostVersion: Integer;
    FXSQLDAMaxSize: Cardinal;
    FInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
    FLastWarning: EZSQLWarning;
    FEventList: TZFirebirdInterbaseEventList;
    procedure DetermineClientTypeAndVersion;
    procedure AssignISC_Parameters;
    procedure TransactionParameterPufferChanged;
    function GetActiveTransaction: IZInterbaseFirebirdTransaction;
    procedure OnPropertiesChange(Sender: TObject); override;
    /// <summary>Constructs the connection string for the current connection</summary>
    /// <returns>the composed connection string</returns>
    function ConstructConnectionString: SQLString;
    class function GetEventListClass: TZFirebirdInterbaseEventListClass; virtual; abstract;
  protected
    procedure InternalClose; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public { IZTransactionManager }
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
    procedure ClearTransactions;
  public { implement IZInterbaseFirebirdConnection }
    /// <summary>Determine if the Client lib-module is a Firebird lib</summary>
    /// <returns><c>True</c>If it's a Firebird client lib; <c>False</c>
    ///  otherwise</returns>
    function IsFirebirdLib: Boolean; virtual; abstract;
    /// <summary>Determine if the Client lib-module is a Interbase lib</summary>
    /// <returns><c>True</c>If it's a Interbase client lib; <c>False</c>
    ///  otherwise</returns>
    function IsInterbaseLib: Boolean; virtual; abstract;
    function GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    function GetSubTypeTextCharSetID(const TableName, ColumnName: String): Integer;
    /// <summary>Return Interbase/Firebird dialect number. The dialect  must be
    ///  1 or 2 or 3.</summary>
    /// <returns>the dialect number</returns>
    function GetDialect: Word;
    function GetXSQLDAMaxSize: Cardinal;
    function GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
    /// <summary>Handles possible sql errors or warnings. Each error will raise
    ///  an EZSQLException. Warnings may be suppressed, see RaiseWarnings.</summary>
    /// <param>"LogCategory" a logging category if LoggingListeners are
    ///  registeted on the DriverManager</param>
    /// <param>"StatusVector" a status vector reference. It contain information
    ///  about errors or warnings</param>
    /// <param>"LogMessage" a sql query command or another message which may be
    ///  logged or added to the Exception string. See
    ///  AddLogMsgToExceptionOrWarningMsg</param>
    /// <param>"Sender" the calling object to be used on connection loss</param>
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      StatusVector: PARRAY_ISC_STATUS; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
    /// <summary>Processes Interbase or Firebird status vector and returns array
    ///  of status data.</summary>
    /// <param>"StatusVector" a TARRAY_ISC_STATUS vector reference. It contains
    ///  information about an error or warnings.</param>
    /// <returns>an array of TZIBStatusVector records</returns>
    function InterpretInterbaseStatus(var StatusVector: PARRAY_ISC_STATUS): TZIBStatusVector;
    procedure SetActiveTransaction(const Value: IZTransaction);
    function GenerateTPB(AutoCommit, ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
      Info: TStrings): RawByteString;
  public
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  released. Otherwise makes all changes made since the previous commit/
    ///  rollback permanent and releases any database locks currently held by
    ///  the Connection. This method should be used only when auto-commit mode
    ///  has been disabled. If Option "Hard_Commit" is set to true or
    ///  TestCachedResultsAndForceFetchAll returns <c>True</c> the transaction
    ///  is committed. Otherwise if "Hard_Commit" isn't set to true a
    ///  retained_commit is performed, and the txn get's removed from the
    ///  transaction manger. Later if all streams are closed a final
    ///  commit is called to release the garbage.</summary>
    procedure Commit;
    /// <summary>If the current transaction is saved the current savepoint get's
    ///  rolled back. Otherwise drops all changes made since the previous
    ///  commit/rollback and releases any database locks currently held
    ///  by this Connection. This method should be used only when auto-
    ///  commit has been disabled. If Option "Hard_Commit" is set to true
    ///  or TestCachedResultsAndForceFetchAll returns <c>True</c> the
    ///  transaction is rolled back. Otherwise if "Hard_Commit" isn't set
    ///  to true a retained_rollback is performed, and the txn get's removed
    ///  from the transaction manger. Later if all streams are closed a final
    ///  rollback is called to release the garbage.</summary>
    procedure Rollback;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
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
    /// <author>EgonHugeist</author>
    /// <summary>Get the active transaction interces of the connection.</summary>
    /// <returns>The transaction object</returns>
    function GetConnectionTransaction: IZTransaction;
  public
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
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
    function GetBinaryEscapeString(const Value: TBytes): String; override;
    /// <author>fduenas</author>
    /// <summary>Gets the host's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this server's full version number</returns>
    function GetHostVersion: Integer; override;
    /// <author>fduenas</author>
    /// <summary>Gets the client's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this clients's full version number</returns>
    function GetClientVersion: Integer; override;
    /// <summary>Creates a sequence generator object.</summary>
    /// <param>"Sequence" a name of the sequence generator.</param>
    /// <param>"BlockSize" a number of unique keys requested in one trip to SQL
    ///  server.</param>
    /// <returns>returns a created sequence object.</returns>
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;
    /// <summary>Get a generic event alerter object.</summary>
    /// <param>"Handler" an event handler which gets triggered if the event is received.</param>
    /// <param>"CloneConnection" if <c>True</c> a new connection will be spawned.</param>
    /// <param>"Options" a list of options, to setup the event alerter.</param>
    /// <returns>a the generic event alerter object as interface or nil.</returns>
    function GetEventListener(Handler: TZOnEventHandler; CloneConnection: Boolean; Options: TStrings): IZEventListener; override;
  public { implement IZEventListener}
    /// <summary>Returns the <c>Connection</c> interface
    ///  that produced this <c>Sequence</c> object.</summary>
    /// <returns>the connection that produced this sequence.</returns>
    function GetConnection: IZConnection;
    /// <returns><c>true</c> if the EventListener is active.</returns>
    function IsListening: Boolean;
    /// <summary>Stop listening the events and cleares the registered events.</summary>
    procedure Unlisten;
    /// <summary>Triggers an event.</summary>
    procedure TriggerEvent(const Name: String);
  public { implement IZFirebirdInterbaseEventAlerter}
    procedure SetOnEventAlert(Value: TZFirebirdInterbaseEventAlert);
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a generic IB/FB transaction</summary>
  TZInterbaseFirebirdTransaction = class(TZImmediatelyReleasableObject,
    IImmediatelyReleasable)
  protected
    FWeakIZTransactionPtr: Pointer;
    fSavepoints: TStrings;
    fDoCommit, fDoLog: Boolean;
    FOpenCursors, FOpenUncachedLobs: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FReadOnly, FAutoCommit: Boolean;
    FTransactionIsolation: TZTransactIsolationLevel;
    FTPB: RawByteString;
    FProperties: TStrings;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZInterbaseFirebirdConnection;
  protected
    function TxnIsStarted: Boolean; virtual; abstract;
    function TestCachedResultsAndForceFetchAll: Boolean; virtual; abstract;
  public { IZInterbaseFirebirdTransaction }
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
    /// <summary>Registers an open cursor. As long the resultset isn't fetched
    ///  completely, the transaction is referenced the the object. If the
    ///  fetch is complete, an DeRegisterOpenCursor call will be done and the
    ///  possible orphan transaction will be "hard" commited or rolled back as
    ///  requested by user.</summary>
    /// <param>"CursorRS" the resultset object which requires this transaction
    ///  object.</param>
    procedure RegisterOpencursor(const CursorRS: IZResultSet);
    procedure RegisterOpenUnCachedLob(const Lob: IZlob);
    procedure DeRegisterOpenCursor(const CursorRS: IZResultSet);
    procedure DeRegisterOpenUnCachedLob(const Lob: IZlob);
    function GetOpenCursorCount: Integer;
    function GetTPB: RawByteString;
  public // implement IZTransaction
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
    /// <summary>Gets the current auto-commit state. See setAutoCommit.</summary>
    /// <returns>the current state of auto-commit mode.</returns>
    function GetAutoCommit: Boolean;
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
  public
    constructor Create(const Owner: TZInterbaseFirebirdConnection; AutoCommit, ReadOnly: Boolean;
      TransactionIsolation: TZTransactIsolationLevel; Properties: TStrings);
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
  end;

  /// <summary>Implements a Interbase6/Firebird sequence.</summary>
  TZInterbaseFirebirdSequence = class(TZIdentifierSequence)
  public
    /// <summary>Returns the SQL to be get the current value.</summary>
    /// <returns>The SQL string</returns>
    function GetCurrentValueSQL: string; override;
    /// <summary>Returns the SQL to be get the next value.</summary>
    /// <returns>The SQL string</returns>
    function GetNextValueSQL: string; override;
    /// <summary>Sets the block size for this sequence.</summary>
    /// <param>Value the block size.</param>
    procedure SetBlockSize(const Value: Integer); override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a generic Interbase ResultSetMetadata object.</summary>
  TZInterbaseFirebirdResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
    procedure SetColumnTypeFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  End;

  /// <summary>Implements a generic Firebird 3+ ResultSetMetadata object.</summary>
  TZFirebird3upResultSetMetadata = Class(TZInterbaseFirebirdResultSetMetadata)
  public
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  End;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a specialized cached resolver for Interbase/Firebird.</summary>
  TZInterbaseFirebirdCachedResolver = class(TZGenerateSQLCachedResolver)
  private
    FInsertReturningFields: TStrings;
    FHasAutoIncrementColumns, FHasWritableAutoIncrementColumns: Boolean;
    FReturningPairs: TZIndexPairList;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;
  public
    /// <summary>Forms an INSERT statements.</summary>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <returns>the composed insert SQL.</returns>
    function FormInsertStatement(NewRowAccessor: TZRowAccessor): SQLString; override;
    /// <author>Michael Seeger</author>
    /// <summary>Forms a SELECT statements to calculate default values.</summary>
    /// <param>"RowAccessor" an accessor object to column values.</param>
    /// <param>"ColumnsLookup" an TZIndexPairList which holds the NULL columns.</param>
    /// <returns>the composed SELECT SQL.</returns>
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): string; override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a specialized cached resolver for Firebird 2+.</summary>
  TZFirebird2upCachedResolver = class(TZInterbaseFirebirdCachedResolver)
  public
    /// <summary>Forms a where clause for UPDATE or DELETE DML statements.</summary>
    /// <param>"SQLWriter" a TZSQLStringWriter object used for buffered writes</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"Result" a reference to the Result String the SQLWriter uses
    ///  for the buffered writes.</param>
    procedure FormWhereClause(const SQLWriter: TZSQLStringWriter;
      const OldRowAccessor: TZRowAccessor; var Result: SQLString); override;
  end;

  IZInterbaseFirebirdLob = Interface(IZLob)
    ['{5CD97968-D804-43FB-94CF-4794277D7198}']
    function GetBlobId: TISC_QUAD;
  End;

  TZInterbaseFirebirdColumnInfo = Class(TZColumnInfo)
  public
    sqldata: Pointer; //points to data in our buffer
    sqlind: PISC_SHORT; //null indicator, nil if not nullable
    sqltype: Cardinal;
    sqlsubtype: Cardinal;
    sqlscale: Integer;
  public
    function GetPCharFromTextVar(out Len: NativeUInt): PAnsiChar; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  End;

  TZInterbaseFirebirdStatementGUIDProps = class;

  TZAbstractInterbaseFirebirdResultSet = Class(TZAbstractReadOnlyResultSet)
  protected
    FGUIDProps: TZInterbaseFirebirdStatementGUIDProps;
    FIsMetadataResultSet: Boolean;
    FByteBuffer: PByteBuffer;
  public
    //EH: this field is a weak resultset reference
    //it may be an address of a cached resultset which owns this instance.
    //this pointer should be registered as open cursor on the TA
    //aim is: if a transaction commit is called the TA checks if
    //all open resultsets a scollable. If so a fetchall will be done by TA.
    //finally the TA can commit the handle (i.e. changing the AutoCommit mode)
    //which would ususally close all pending cursors
    TransactionResultSet: Pointer;
    procedure AfterClose; override;
    constructor Create(const Statement: IZStatement; const SQL: string);
  public
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
    function GetInt(ColumnIndex: Integer): Integer;
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
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
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
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
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
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
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
    {$IFDEF WITH_COLUMNS_TO_JSON}
    /// <summary>Fill the JSONWriter with column data</summary>
    /// <param>"JSONComposeOptions" the TZJSONComposeOptions used for composing
    ///  the JSON contents</param>
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF WITH_COLUMNS_TO_JSON}
  end;

  TZAbstractFirebirdInterbasePreparedStatement = class;

  {** record for holding batch dml stmts }
  TZIB_FBStmt = record
    Obj: TZAbstractFirebirdInterbasePreparedStatement;
    PreparedRowsOfArray: Integer;
  end;

  /// <summary>Implements a List for IB and FB containing original sqltype,
  ///  sqlscale and nullable infos</summary>
  TZIBFBOrgSqlTypeAndScaleList = class(TZCustomElementList)
  public
    function Add(sqltype: Cardinal; scale: Integer; Nullable: Boolean): NativeInt;
  public
    constructor Create;
  end;

  {** Implements IZPreparedStatement for Firebird and Interbase. }
  TZAbstractFirebirdInterbasePreparedStatement = class(TZAbstractPreparedStatement)
  private
    procedure InternalBindDouble(Index: Cardinal; const Value: Double);
    procedure SetPAnsiChar(Index: Cardinal; Value: PAnsiChar; Len: LengthInt);
    procedure SetPWideChar(Index: Cardinal; Value: PWideChar; Len: LengthInt);
  protected
    FInParamDescripors: PZInterbaseFirerbirdParamArray;
    FBatchStmts: array[Boolean] of TZIB_FBStmt;
    FDB_CP_ID: Word;
    FStatementType: TZIbSqlStatementType;
    FOutMessageCount, FInMessageCount, FMemPerRow: Cardinal;
    FDialect, FTypeTokensLen: Cardinal;
    FQuerySplitted, FReturningFound: Boolean;
    FInData, FOutData: Pointer;
    FCodePageArray: TWordDynArray;
    FByteBuffer: PByteBuffer;
    FOrgTypeList: TZIBFBOrgSqlTypeAndScaleList;
    procedure ExecuteBatchDml; virtual;
    function SplittQuery(const SQL: SQLString): RawByteString;
    function GetExecuteBlockString(RemainingArrayRows: Integer;
      XSQLDAMaxSize: Cardinal; var PreparedRowsOfArray, MaxRowsPerBatch: Integer;
      PlainDriver: TZInterbaseFirebirdPlainDriver): RawByteString;
    procedure WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt); virtual; abstract;
    function CreateConversionError(Index: Cardinal; Current: TZSQLType): EZSQLException; virtual; abstract;
  protected
    procedure UnPrepareInParameters; override;
    procedure CheckParameterIndex(var Value: Integer); override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
    class function GetBindListClass: TZBindListClass; override;
  public
    function GetRawEncodedSQL(const SQL: SQLString): RawByteString; override;
    procedure Unprepare; override;
  public
    Constructor Create(const Connection: IZInterbaseFirebirdConnection;
      const SQL: String; Info: TStrings);
    Destructor Destroy; override;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  public //setters
    /// <summary>Clears the current parameter values immediately.
    ///  In general, parameter values remain in force for repeated use of a
    ///  statement. Setting a parameter value automatically clears its
    ///  previous value.  However, in some cases it is useful to immediately
    ///  release the resources used by the current parameter values; this can
    ///  be done by calling the method <c>ClearParameters</c>.</summary>
    procedure ClearParameters; override;
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
      Scale: LengthInt = 0); override;
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual

    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(Index: Integer; Value: Byte);
    /// <summary>Sets the designated parameter to a <c>ShortInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetShort(Index: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(Index: Integer; Value: Word);
    /// <summary>Sets the designated parameter to a <c>SmallInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetSmall(Index: Integer; Value: SmallInt);
    /// <summary>Sets the designated parameter to a <c>Cardinal</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUInt(Index: Integer; Value: Cardinal);
    /// <summary>Sets the designated parameter to a <c>Integer</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetInt(Index: Integer; Value: Integer);
    /// <summary>Sets the designated parameter to a <c>UInt64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetULong(Index: Integer; const Value: UInt64);
    /// <summary>Sets the designated parameter to a <c>Int64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetLong(Index: Integer; const Value: Int64);
    /// <summary>Sets the designated parameter to a <c>Single</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetFloat(Index: Integer; Value: Single);
    /// <summary>Sets the designated parameter to a <c>Double</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDouble(Index: Integer; const Value: Double);
    /// <summary>Sets the designated parameter to a <c>Currency</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCurrency(Index: Integer; const Value: Currency);
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
    /// <summary>Sets the designated parameter to a <c>TZCharRec</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCharRec(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZCharRec); reintroduce;
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
    procedure SetString(Index: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Sets the designated parameter to a <c>RawByteString</c> value.
    ///  The string must be UTF8 encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUTF8String(Index: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Sets the designated parameter to a <c>AnsiString</c> value.
    ///  The string must be GET_ACP encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetAnsiString(Index: Integer; const Value: AnsiString); reintroduce;
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
    procedure SetRawByteString(Index: Integer; const Value: RawByteString); reintroduce;
    /// <summary>Sets the designated parameter to a <c>UnicodeString</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUnicodeString(Index: Integer; const Value: UnicodeString); reintroduce;
    /// <summary>Sets the designated parameter to a <c>TZDate</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>TZTime</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>TZTimestamp</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>byte array</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>ByteArray reference</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value reference.</param>
    /// <param>"Len" the Length of the bytes buffer.</param>
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>TGUID</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetGUID(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID); reintroduce;
    /// <summary>Sets the designated parameter to the given blob wrapper object.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" defines the lob constent. Valid values are:
    ///  stAsciiStream(raw encoded text), stUnicodeStream(UTF16 encoded text)
    ///  and stBinaryStream(binary data), stJSON, stXML</param>
    /// <param>"Value" the parameter blob wrapper object to be set.</param>
    procedure SetBlob(Index: Integer; ASQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZAbstractInterbaseFirebirdCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    /// <summary>creates an exceution Statement</summary>
    /// <param>"Connection" the owner firebird/interbase-connection interface.</param>
    /// <param>"SQL" the SQL to be prepared and executed.</param>
    /// <param>"Params" a parameter list to setup behaviors.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function InternalCreateExecutionStatement(const Connection: IZInterbaseFirebirdConnection;
      const SQL: String; Info: TStrings): TZAbstractPreparedStatement; virtual; abstract;
    /// <summary>creates an exceution Statement. Which wraps the call.</summary>
    /// <param>"StoredProcName" the name of the stored procedure or function to
    ///  be called.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

  {** EH: implements a Firebird/Interbase large object stream }
  TZAbstractInterbaseFirebirdLobStream = class(TZImmediatelyReleasableLobStream,
    IImmediatelyReleasable)
  protected
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FLobIsOpen: Boolean;
    FPosition: NativeInt;
  public
    BlobId: TISC_QUAD;
    Updated: Boolean;
    BlobInfo: PIbBlobInfo;
  end;

  TGUIDDetectFlag = (gfByType, gfByDomain, gfByFieldName);
  TGUIDDetectFlags = set of TGUIDDetectFlag;

  {** Implements GUID detection options/properties }
  TZInterbaseFirebirdAbstractGUIDProps = class //constributed by Fr0st
  private
    FDetectFlags: TGUIDDetectFlags;
    FDomains: TStrings;
    FFields: TStrings;
  public // to access from descendants
    procedure InternalInit(const OptionByType, OptionDomains, OptionFields: string);
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  public
    destructor Destroy; override;
    function ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
  end;

  // Reusable object intended for use on Connection level. Allows re-initialization
  // if Connection properties are changed. Uses Connection properties only
  //constributed by Fr0st
  TZInterbaseFirebirdConnectionGUIDProps = class(TZInterbaseFirebirdAbstractGUIDProps)
  public // to access from other classes of the unit
    procedure InitFromProps(Properties: TStrings);
  public
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
  end;

  // Temporary object intended for use on Statement level. Should be re-created
  // whenever a Statement is opened. Uses Statement & Connection properties.
  // Doesn't consider domain info (there's no domains on Statement level)
   //constributed by Fr0st
  TZInterbaseFirebirdStatementGUIDProps = class(TZInterbaseFirebirdAbstractGUIDProps)
  public
    constructor Create(const Statement: IZStatement); overload;
    function ColumnIsGUID(SQLType: TZSQLType; DataSize: Integer; const ColumnName: string): Boolean;
  end;

  PZIBFBOrgSqlTypeAndScale = ^TZIBFBOrgSqlTypeAndScale;
  TZIBFBOrgSqlTypeAndScale = record
    sqltype: cardinal;
    scale: Integer;
    Nullable: Boolean;
  end;

  PZIB_FBBindValue = ^TZIB_FBBindValue;
  TZIB_FBBindValue = record
    BindValue: TZBindValue;
    TypeToken: RawByteString;
  end;

  TZInterbaseFirebirdBindList = class(TZBindList)
  protected
    /// <summary>Get the size of the custom element of this class.</summary>
    /// <returns>the size of the custom element.</returns>
    class function GetElementSize: Integer; override;
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  /// <summary>Defines a reference of an event block for firebird and interbase.</summary>
  PZInterbaseFirebirdEventBlock = ^TZInterbaseFirebirdEventBlock;

  TZProcessEvents = procedure (EventBlock: PZInterbaseFirebirdEventBlock) of Object;
  TZAsyncQueEvents = procedure (EventBlock: PZInterbaseFirebirdEventBlock) of Object;

  /// <summary>Defines an event block slice for firebird and interbase.
  ///  A slice is a chunk of max 15 events to be registered on fbclient.</summary>
  TZInterbaseFirebirdEventBlock = record
    /// <summary>Address of a raw character pointer; isc_event_block allocates
    ///  and initializes an event parameter buffer and stores its address into
    ///  the character pointer. Up to 15 null-terminated strings registered with
    ///  isc_event_block that each name an event</summary>
    event_buffer: PAnsiChar;
    /// <summary>Address of a raw character pointer; isc_event_block allocates
    ///  an event parameter buffer, and stores its address into the character
    ///  pointer. Up to 15 null-terminated strings that each name an event
    ///  addressed in the buffer</summary>
    result_buffer: PAnsiChar;
    /// <summary>the evenbuffer length</summary>
    buffer_length: ISC_LONG;
    /// <summary>Number of event identifier strings that get registered per eventblock.</summary>
    EventCount: ISC_USHORT;
    /// <summary>Caller to process the events</summary>
    ProcessEvents: TZProcessEvents;
    /// <summary>Caller to que the events</summary>
    AsyncQueEvents: TZAsyncQueEvents;
    /// <summary>register and unregister an event triggers an received event.
    ///  But we want to listen to real events only</summary>
    FirstTime: Boolean;
    /// <summary>The offset in the envent names list we listen</summary>
    NamesOffSet: Integer;
    /// <summary>The returned id after register the event block</summary>
    event_id: PISC_LONG;
    /// <summary>Weak reference of the IEvents interface</summary>
    FBEvent: Pointer;
    /// <summary>Weak reference of the IEventsCallback interface</summary>
    FBEventsCallback: Pointer;
  end;

  IZFirebirdInterbaseEventAlerter = interface(IZEventListener)
    ['{57556AD1-F8D3-4AF9-B25C-89835DB4E4B1}']
    procedure SetOnEventAlert(Value: TZFirebirdInterbaseEventAlert);
  end;

  TZEventBlocks = array of TZInterbaseFirebirdEventBlock;
  /// <summary>Implements Firebird/Interbase EventList</summary>
  TZFirebirdInterbaseEventList = Class(TZEventList)
  private
    {$IFDEF AUTOREFCOUNT}[WEAK]{$ENDIF}FConnection: TZInterbaseFirebirdConnection;
  protected
    FEventBlocks: TZEventBlocks;
    FOnEventAlert: TZFirebirdInterbaseEventAlert;
  public
    constructor Create(Handler: TZOnEventHandler; AConnection: TZInterbaseFirebirdConnection);
    destructor Destroy; override;
  public
    procedure AsyncQueEvents(EventBlock: PZInterbaseFirebirdEventBlock); virtual; abstract;
    procedure ProcessEvents(EventBlock: PZInterbaseFirebirdEventBlock);
    procedure RegisterEvents;
    procedure UnregisterEvents; virtual; abstract;
  public
    property Connection: TZInterbaseFirebirdConnection read FConnection;
  End;

  TZFirebirdInterbaseEventData = class(TZEventData)
  private
    FCountForEvent: ISC_ULONG;
  public
    function ToString: string; override;
  public
    property CountForEvent: ISC_ULONG read FCountForEvent;
  end;

procedure BindSQLDAInParameters(BindList: TZBindList;
  Stmt: TZAbstractFirebirdInterbasePreparedStatement; ArrayOffSet, ArrayItersCount: Integer);

const
  sCS_NONE = 'NONE';
  DS_Props_IsMetadataResultSet = 'IsMetadataResultSet';
  sStartTxn = RawByteString('TRANSACTION STARTED.');
  sCommitMsg = RawByteString('TRANSACTION COMMIT');
  sRollbackMsg = RawByteString('TRANSACTION ROLLBACK');

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

uses ZSysUtils, ZFastCode, ZEncoding, ZMessages, ZVariant,
  ZInterbaseToken, ZInterbaseAnalyser, ZSelectSchema,
  ZDbcMetadata, ZDbcProperties,
  ZDbcInterbaseFirebirdMetadata;

{ TZInterbaseFirebirdDriver }

function TZInterbaseFirebirdDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZInterbaseStatementAnalyser.Create;
end;

function TZInterbaseFirebirdDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZInterbaseTokenizer.Create;
end;

{ TZInterbaseFirebirdConnection }

procedure TZInterbaseFirebirdConnection.AfterConstruction;
var TAManager: IZTransactionManager;
begin
  QueryInterface(IZTransactionManager, TAManager);
  FWeakTransactionManagerPtr := Pointer(TAManager);
  TAManager := nil;
  // ! Create the object before parent's constructor because it is used in
  // TZAbstractDbcConnection.Create > Url.OnPropertiesChange
  FGUIDProps := TZInterbaseFirebirdConnectionGUIDProps.Create;
  FClientVersion := -1;
  FMetadata := TZInterbase6DatabaseMetadata.Create(Self, Url);
  fTransactions := TZCollection.Create;
  Ftransactions.Add(nil);
  { set default sql dialect it can be overriden }
  FDialect := StrToIntDef(Info.Values[ConnProps_Dialect], SQL_DIALECT_CURRENT);
  FProcedureTypesCache := TStringList.Create;
  FSubTypeTestCharIDCache := TStringList.Create;
  FInterbaseFirebirdPlainDriver := PlainDriver.GetInstance as TZInterbaseFirebirdPlainDriver;
  inherited AfterConstruction;
end;

procedure TZInterbaseFirebirdConnection.AssignISC_Parameters;
var
  RoleName: string;
  ConnectTimeout, Idx: integer;
  WireCompression: Boolean;
  procedure ParseCreateNewDBStringToISC(const Value: String);
  var CreateDB: String;
      I: Integer;
      P, PEnd: PChar;
  begin
    CreateDB := UpperCase(Value);
    I := PosEx('CHARACTER', CreateDB);
    if I > 0 then begin
      I := PosEx('SET', CreateDB, I);
      P := Pointer(CreateDB);
      Inc(I, 3); Inc(P, I); Inc(I);
      While P^ = ' ' do begin
        Inc(I); Inc(P);
      end;
      PEnd := P;
      While ((Ord(PEnd^) >= Ord('A')) and (Ord(PEnd^) <= Ord('Z'))) or
            ((Ord(PEnd^) >= Ord('0')) and (Ord(PEnd^) <= Ord('9'))) do
        Inc(PEnd);
      Info.Values[ConnProps_isc_dpb_lc_ctype] := Copy(CreateDB, I, (PEnd-P));
    end;
    if Info.Values[ConnProps_isc_dpb_lc_ctype] = '' then
      Info.Values[ConnProps_isc_dpb_lc_ctype] := FClientCodePage;
    if Info.Values[ConnProps_isc_dpb_lc_ctype] = '' then
      Info.Values[ConnProps_isc_dpb_lc_ctype] := Info.Values[ConnProps_isc_dpb_set_db_charset];
    FClientCodePage := Info.Values[ConnProps_isc_dpb_lc_ctype];

    I := PosEx('PAGE', CreateDB);
    if I > 0 then begin
      I := PosEx('SIZE', CreateDB, I);
      P := Pointer(CreateDB);
      Inc(I, 4); Inc(P, I); Inc(I);
      While P^ = ' ' do begin
        Inc(I); Inc(P);
      end;
      PEnd := P;
      While ((Ord(PEnd^) >= Ord('0')) and (Ord(PEnd^) <= Ord('9'))) do
        Inc(PEnd);
      Info.Values[ConnProps_isc_dpb_page_size] :=  Copy(CreateDB, I, (PEnd-P));
    end else Info.Values[ConnProps_isc_dpb_page_size] := '4096'; //default
  end;
begin
  { set default sql dialect it can be overriden }
  FDialect := StrToIntDef(Info.Values[ConnProps_isc_dpb_sql_dialect], StrToIntDef(Info.Values[ConnProps_Dialect], SQL_DIALECT_CURRENT));
  Info.BeginUpdate; // Do not call OnPropertiesChange every time a property changes
  RoleName := Info.Values[ConnProps_CreateNewDatabase];

  if (GetClientVersion >= 2005000) and IsFirebirdLib and (Length(RoleName) > 5) then begin
    ParseCreateNewDBStringToISC(RoleName);
    Info.Values[ConnProps_CreateNewDatabase] := 'true';
  end;

  { Processes connection properties. }
  if Info.Values[ConnProps_isc_dpb_user_name] = '' then
    Info.Values[ConnProps_isc_dpb_user_name] := Url.UserName;
  if Info.Values[ConnProps_isc_dpb_password] = '' then
    Info.Values[ConnProps_isc_dpb_password] := Url.Password;

  if FClientCodePage = '' then begin //was set on inherited Create(...)
    FClientCodePage := Info.Values[ConnProps_isc_dpb_lc_ctype];
    if FClientCodePage = '' then
      FClientCodePage := Info.Values[ConnProps_isc_dpb_set_db_charset];
    if FClientCodePage <> '' then
      CheckCharEncoding(FClientCodePage, True);
  end;
  Info.Values[ConnProps_isc_dpb_lc_ctype] := FClientCodePage;

  if Info.Values[ConnProps_isc_dpb_sql_role_name] = '' then begin
    RoleName := Trim(Info.Values[ConnProps_Rolename]);
    if RoleName <> '' then
      Info.Values[ConnProps_isc_dpb_sql_role_name] := UpperCase(RoleName);
  end;
  // EH: that's a bug: even if this property is named as "connnect timeout"
  // it's used as idle time out after n seconds and has nothing todo with
  // opening a connection in elepsed seconds...
  if StrToIntDef(Info.Values[ConnProps_isc_dpb_connect_timeout], -1) = -1 then begin
    ConnectTimeout := StrToIntDef(Info.Values[ConnProps_Timeout], -1);
    if ConnectTimeout >= 0 then
      Info.Values[ConnProps_isc_dpb_connect_timeout] := ZFastCode.IntToStr(ConnectTimeout);
  end;

  WireCompression := StrToBoolEx(Info.Values[ConnProps_WireCompression]);
  if WireCompression then
    Info.Values[ConnProps_isc_dpb_config] :=
      Info.Values[ConnProps_isc_dpb_config] + LineEnding + 'WireCompression=true';

  if Info.IndexOf(ConnProps_isc_dpb_sql_dialect) = -1 then
    Info.Values[ConnProps_isc_dpb_sql_dialect] := ZFastCode.IntToStr(FDialect);

  Idx := Info.IndexOf('isc_dpb_utf8_filename');
  if (GetClientVersion >= 2005000) and IsFirebirdLib then begin
    if (Idx = -1) and ((FClientCodePage = 'UTF8') or (FClientCodePage = 'UNICODE_FSS')) then
      Info.Add('isc_dpb_utf8_filename');
    FPB_CP := zCP_UTF8;
  end else if Idx <> -1 then begin
    Info.Delete(Idx);
    FPB_CP := ConSettings.ClientCodePage.CP;
  end;
  Info.EndUpdate;
end;

procedure TZInterbaseFirebirdConnection.ClearTransactions;
begin
  fTransactions.Clear;
  fTransactions.Add(nil);
end;

procedure TZInterbaseFirebirdConnection.ClearWarnings;
begin
  if FLastWarning <> nil then
    FreeAndNil(FLastWarning);
end;

procedure TZInterbaseFirebirdConnection.Commit;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  with GetActiveTransaction do begin
    Commit;
    if (not FRestartTransaction) and (GetTransactionLevel <= 0) then
      SetAutoCommit(True);
  end;
end;

function TZInterbaseFirebirdConnection.ConstructConnectionString: SQLString;
var
  Protocol: String;
  Writer: TZSQLStringWriter;
begin
  Protocol := Info.Values[ConnProps_FBProtocol];
  Protocol := LowerCase(Protocol);
  Writer := TZSQLStringWriter.Create(512);
  Result := '';
  try
    if ((Protocol = 'inet') or (Protocol = 'wnet') or (Protocol = 'xnet') or (Protocol = 'local')) then begin
      if (GetClientVersion >= 3000000) and IsFirebirdLib then begin
        if protocol = 'inet' then begin
          Writer.AddText('inet://', Result);
          Writer.AddText(HostName, Result);
          if Port <> 0 then begin
            Writer.AddChar(':', Result);
            Writer.AddOrd(Port, Result);
          end;
          Writer.AddChar('/', Result);
        end else if Protocol = 'wnet' then begin
          Writer.AddText('wnet://', Result);
          if HostName <> '' then begin
            Writer.AddText(HostName, Result);
            Writer.AddChar('/', Result);
          end; //EH@Jan isn't the port missing here ? or just not required?
        end else if Protocol = 'xnet' then
          Writer.AddText('xnet://', Result);
      end else if protocol = 'inet' then begin
        if HostName = ''
        then Writer.AddText('localhost', Result)
        else Writer.AddText(HostName, Result);
        if Port <> 0 then begin
          Writer.AddChar('/', Result);
          Writer.AddOrd(Port, Result);
        end;
        Writer.AddChar(':', Result);
      end else if Protocol = 'wnet' then begin
        Writer.AddText('\\', Result);
        if HostName = ''
        then Writer.AddChar('.', Result)
        else Writer.AddText(HostName, Result);
        if Port <> 0 then begin
          Writer.AddChar('@', Result);
          Writer.AddOrd(Port, Result);
        end;
        Writer.AddChar('\', Result);
      end;
    end else if HostName <> '' then begin
      Writer.AddText(HostName, Result);
      if Port <> 0 then begin
        Writer.AddChar('/', Result);
        Writer.AddOrd(Port, Result);
      end;
      Writer.AddChar(':', Result);
    end;
    Writer.AddText(Database, Result);
    Writer.Finalize(Result);
  finally
    FreeAndNil(Writer);
  end;
end;

function TZInterbaseFirebirdConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZInterbaseFirebirdSequence.Create(
    IZConnection(fWeakReferenceOfSelfInterface), Sequence, BlockSize);
end;

destructor TZInterbaseFirebirdConnection.Destroy;
begin
  FreeAndNil(FProcedureTypesCache);
  FreeAndNil(FGUIDProps);
  FreeAndNil(FSubTypeTestCharIDCache);
  if FEventList <> nil then
    FreeAndNil(FEventList);
  inherited Destroy;
end;

procedure TZInterbaseFirebirdConnection.DetermineClientTypeAndVersion;
var
  FbPos, Major, Minor, Release: Integer;
  Buff: array[0..50] of AnsiChar;
  P, PDot, PEnd: PAnsiChar;
begin
  P := @Buff[0];
  PDot := P;
  if Assigned(FInterbaseFirebirdPlainDriver.isc_get_client_version) then begin
    FbPos := 0;
    FInterbaseFirebirdPlainDriver.isc_get_client_version(P);
    PEnd := P+ZFastCode.StrLen(P);
    while P < PEnd do begin
      if (FbPos < 2) and (PByte(P)^ = Byte('.')) then begin
        PDot := P +1; //mark start of fake minior version
        Inc(FbPos);
      end else
        PByte(P)^ := PByte(P)^ or $20; //Lowercase the buffer for the PosEx scan
      Inc(P);
    end;
    P := PDot;
  end else
    Exit;
  FbPos := ZFastCode.PosEx(RawByteString('firebird'), P, PEnd-P);
  if FbPos > 0 then begin
    FIsFirebirdLib := true;
    { to get relase version read to next dot }
    while (PDot < PEnd) and (PByte(PDot)^ >= Byte('0')) and (PByte(PDot)^ <= Byte('9')) do
      Inc(PDot);
    Release := ZFastCode.RawToIntDef(P, PDot, 0);
    { skip the Firebird brand including the space }
    Inc(P, FbPos + 8);
    PDot := P; { read to next dot }
    while (PDot < PEnd) and (PByte(PDot)^ >= Byte('0')) and (PByte(PDot)^ <= Byte('9')) do
      Inc(PDot);
    Major := ZFastCode.RawToIntDef(P, PDot, 0);
    Inc(PDot); //skip the dot
    P := PDot; { read to end or next non numeric char }
    while (PDot < PEnd) and (PByte(PDot)^ >= Byte('0')) and (PByte(PDot)^ <= Byte('9')) do
      Inc(PDot);
    Minor := ZFastCode.RawToIntDef(P, PDot, 0);
  end else begin
    Release := 0;
    if Assigned(FInterbaseFirebirdPlainDriver.isc_get_client_major_version)
    then Major := FInterbaseFirebirdPlainDriver.isc_get_client_major_version()
    else Major := 0;
    if Assigned(FInterbaseFirebirdPlainDriver.isc_get_client_minor_version)
    then Minor := FInterbaseFirebirdPlainDriver.isc_get_client_minor_version()
    else Minor := 0;
    FIsInterbaseLib := Major <> 0;
  end;
  FClientVersion := Major * 1000000 + Minor * 1000 + Release;
end;

const
  Tpb_Access: array[boolean] of String = (TxnProps_isc_tpb_write,TxnProps_isc_tpb_read);
  tpb_AutoCommit: array[boolean] of String = ('',TxnProps_isc_tpb_autocommit);
function TZInterbaseFirebirdConnection.GenerateTPB(AutoCommit,
  ReadOnly: Boolean; TransactIsolationLevel: TZTransactIsolationLevel;
  Info: TStrings): RawByteString;
var
  Params: TStrings;
{ List of parameters that are assigned according to values of properties but
  could be overwritten by user.
  These parameters are all simple flags having no value so no splitting is required. }
type
  TOverwritableParams = (parTIL, parRW, parRecVer, parWait, parAutoCommit);
  TOverwritableParamValues = array[TOverwritableParams] of string;

  { Add all items from Src to Dest except those which define overwritable params.
    Value of these params are returned in OverwritableParams array. }
  procedure AddStrings(Dest, Src: TStrings; var OverwritableParams: TOverwritableParamValues);
  var
    I: Integer;
    SrcPar: string;
  begin
    for I := 0 to Src.Count - 1 do
    begin
      SrcPar := LowerCase(Src[I]);
      if (SrcPar = TxnProps_isc_tpb_consistency) or
         (SrcPar = TXnProps_isc_tpb_concurrency) or
         (SrcPar = TxnProps_isc_tpb_read_committed) then
        OverwritableParams[parTIL] := SrcPar
      else
      if (SrcPar = TxnProps_isc_tpb_wait) or
         (SrcPar = TxnProps_isc_tpb_nowait) then
        OverwritableParams[parWait] := SrcPar
      else
      if (SrcPar = TxnProps_isc_tpb_read) or
         (SrcPar = TxnProps_isc_tpb_write) then
        OverwritableParams[parRW] := SrcPar
      else
      if (SrcPar = TxnProps_isc_tpb_rec_version) or
         (SrcPar = TxnProps_isc_tpb_no_rec_version) then
        OverwritableParams[parRecVer] := SrcPar
      else
      if (SrcPar = TxnProps_isc_tpb_autocommit) then
        OverwritableParams[parAutoCommit] := SrcPar
      else if StartsWith(SrcPar, TPBPrefix) then  //skip all non isc_tpb params
        Dest.Add(Src[I]);
    end;
  end;
var
  OverwritableParams: TOverwritableParamValues;
begin
  Params := TStringlist.Create;
  try
    Params.Capacity := Ord(High(TOverwritableParams))+Info.Count;
    OverwritableParams[parAutoCommit] := tpb_AutoCommit[AutoCommit];

    { Set transaction parameters by TransactIsolationLevel }
    case TransactIsolationLevel of
      tiReadCommitted:
        begin
          if GetHostVersion >= 4000000
          then OverwritableParams[parRecVer] := TxnProps_isc_tpb_read_consistency
          else OverwritableParams[parRecVer] := TxnProps_isc_tpb_rec_version;
          OverwritableParams[parWait] := TxnProps_isc_tpb_nowait;
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := TxnProps_isc_tpb_read_committed;
        end;
      tiRepeatableRead:
        begin
          OverwritableParams[parWait] := TxnProps_isc_tpb_nowait;
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := TXnProps_isc_tpb_concurrency;
        end;
      tiSerializable:
        begin
          AddStrings(Params, Info, OverwritableParams);
          OverwritableParams[parRW] := tpb_Access[ReadOnly];
          OverwritableParams[parTIL] := TxnProps_isc_tpb_consistency;
        end;
      else begin
        OverwritableParams[parRW] := tpb_Access[ReadOnly]; //eh: why is this done before AddStrings is called?
        { FB default values for non-standard TIL }
        OverwritableParams[parTIL] := TXnProps_isc_tpb_concurrency;
        OverwritableParams[parWait] := TxnProps_isc_tpb_wait;
        AddStrings(Params, Info, OverwritableParams);
      end;
    end;
    { EH: as long we allow to overwrite the two params we need to sync our settings: }
    if OverwritableParams[parRW] <> tpb_Access[ReadOnly] then //overwrite seems to be allowed for tiNone
      inherited SetReadOnly(not ReadOnly);
    if OverwritableParams[parAutoCommit] <> tpb_AutoCommit[AutoCommit] then //overwrite seems to be allowed allways
      inherited SetAutoCommit(not AutoCommit);


    { Add overwitable parameters to the beginning of list }
    if OverwritableParams[parRW] <> '' then
      Params.Insert(0, OverwritableParams[parRW]);
    if OverwritableParams[parWait] <> '' then
      Params.Insert(0, OverwritableParams[parWait]);
    if OverwritableParams[parRecVer] <> '' then
      Params.Insert(0, OverwritableParams[parRecVer]);
    if OverwritableParams[parTIL] <> '' then
      Params.Insert(0, OverwritableParams[parTIL]);
    if OverwritableParams[parAutoCommit] <> '' then
      Params.Insert(0, OverwritableParams[parAutoCommit]);

    Result := BuildPB(FInterbaseFirebirdPlainDriver, Params, isc_tpb_version3,
      TPBPrefix, TransactionParams{$IFDEF UNICODE},FPB_CP{$ENDIF});
  finally
    FreeAndNil(Params);
  end;
end;

function TZInterbaseFirebirdConnection.GetActiveTransaction: IZInterbaseFirebirdTransaction;
var TA: IZTransaction;
  I: Integer;
begin
  if not IsClosed then begin
    if fActiveTransaction = nil then begin
      TA := IZTransactionManager(FWeakTransactionManagerPtr).CreateTransaction(
        AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      if Ftransactions.Count > 1 then begin
        I := Ftransactions.IndexOf(TA);
        Ftransactions.Exchange(I, 0);
        Ftransactions.Delete(I);
      end;
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction);
    end;
    Result := fActiveTransaction;
  end else
    Result := nil;
end;

function TZInterbaseFirebirdConnection.GetBinaryEscapeString(
  const Value: TBytes): String;
begin
  //http://tracker.firebirdsql.org/browse/CORE-2789
  if (GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsBinaryInSQL
  then Result := GetSQLHexString(PAnsiChar(Value), Length(Value))
  else raise EZSQLException.Create('Your Firebird-Version does''t support Binary-Data in SQL-Statements! Use parameters!');
end;

function TZInterbaseFirebirdConnection.GetClientVersion: Integer;
begin
  if FClientVersion = -1 then DetermineClientTypeAndVersion;
  Result := FClientVersion;
end;

function TZInterbaseFirebirdConnection.GetDialect: Word;
begin
  Result := FDialect;
end;

function TZInterbaseFirebirdConnection.GetEventListener(
  Handler: TZOnEventHandler; CloneConnection: Boolean;
  Options: TStrings): IZEventListener;
begin
  Result := inherited GetEventListener(Handler, CloneConnection, Options);
  FEventList := GetEventListClass.Create(Handler, Self);
end;

function TZInterbaseFirebirdConnection.GetGUIDProps: TZInterbaseFirebirdConnectionGUIDProps;
begin
  Result := FGUIDProps;
end;

function TZInterbaseFirebirdConnection.GetHostVersion: Integer;
begin
  Result := FHostVersion;
end;

function TZInterbaseFirebirdConnection.GetInterbaseFirebirdPlainDriver: TZInterbaseFirebirdPlainDriver;
begin
  Result := FInterbaseFirebirdPlainDriver;
end;

function TZInterbaseFirebirdConnection.GetServerProvider: TZServerProvider;
begin
  Result := spIB_FB;
end;

function TZInterbaseFirebirdConnection.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZInterbaseStatementAnalyser.Create;
end;

function TZInterbaseFirebirdConnection.GetSubTypeTextCharSetID(const TableName,
  ColumnName: String): Integer;
var S: String;
  function GetFromMetaData: Integer;
  var Stmt: IZStatement;
    RS: IZResultSet;
  begin
    Stmt := CreateStatement;
    RS := Stmt.ExecuteQuery('SELECT F.RDB$CHARACTER_SET_ID '+LineEnding+
      'FROM RDB$RELATION_FIELDS R'+LineEnding+
      'INNER JOIN RDB$FIELDS F on R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME'+LineEnding+
      'WHERE R.RDB$RELATION_NAME = '+QuotedStr(TableName)+' and R.RDB$FIELD_NAME = '+QuotedStr(ColumnName));
    if RS.Next
    then Result := RS.GetInt(FirstDbcIndex)
    else Result := ConSettings.ClientCodePage.ID;
    RS.Close;
    RS := nil;
    Stmt.Close;
    Stmt := Nil;
  end;
begin
  S := TableName+'/'+ColumnName;
  Result := FSubTypeTestCharIDCache.IndexOf(S);
  if Result < 0 then begin
    Result := GetFromMetaData;
    FSubTypeTestCharIDCache.AddObject(S, TObject(Result));
  end else
    Result := Integer(FSubTypeTestCharIDCache.Objects[Result]);
end;

function TZInterbaseFirebirdConnection.GetTokenizer: IZTokenizer;
begin
  Result := TZInterbaseTokenizer.Create;
end;

function TZInterbaseFirebirdConnection.GetConnection: IZConnection;
begin
  Result := IZConnection(fWeakReferenceOfSelfInterface);
end;

function TZInterbaseFirebirdConnection.GetConnectionTransaction: IZTransaction;
begin
  Result := nil;
  if not Closed then
    if ((fTransactions.Count = 0) or (fTransactions[0] = nil)) then begin
      fActiveTransaction := nil;
      fActiveTransaction := GetActiveTransaction;
    end else
      fTransactions[0].QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction);
  if fActiveTransaction <> nil then
    fActiveTransaction.QueryInterface(IZTransaction, Result);;
end;

function TZInterbaseFirebirdConnection.GetWarnings: EZSQLWarning;
begin
  Result := FlastWarning;
end;

function TZInterbaseFirebirdConnection.GetXSQLDAMaxSize: Cardinal;
begin
  Result := FXSQLDAMaxSize;
end;

procedure TZInterbaseFirebirdConnection.HandleErrorOrWarning(
  LogCategory: TZLoggingCategory; StatusVector: PARRAY_ISC_STATUS;
  const LogMessage: SQLString; const Sender: IImmediatelyReleasable);
var
  FormatStr, ErrorString: string;
  isc_sqlcode, error_code: Integer;
  StatusArg, WarningArg: ISC_STATUS;
  i: Integer;
  InterbaseStatusVector: TZIBStatusVector;
  Error: EZSQLThrowable;
  ExeptionClass: EZSQLThrowableClass;
  OrgStatusVector: PARRAY_ISC_STATUS; //remainder for initialization
begin
  { usually first isc_status is gds_arg_gds .. }
  StatusArg := StatusVector[1];
  WarningArg := StatusVector[2];
  if (StatusArg = isc_arg_end) and (WarningArg = isc_arg_end) then begin
    Exit; //neither Warning nor an Error
  end;
  OrgStatusVector := StatusVector;
  InterbaseStatusVector := InterpretInterbaseStatus(StatusVector);
  isc_sqlcode := InterbaseStatusVector[0].SQLCode;
  error_code := InterbaseStatusVector[0].IBDataInt;
  ErrorString := '';
  for i := Low(InterbaseStatusVector) to High(InterbaseStatusVector) do begin
    AppendSepString(ErrorString, InterbaseStatusVector[i].IBMessage, '; ');
    if AddLogMsgToExceptionOrWarningMsg {and (InterbaseStatusVector[i].IBMessage <> '')} then // Why do we do that?
      AppendSepString(ErrorString, InterbaseStatusVector[i].SQLMessage, '; ');
  end;

  if DriverManager.HasLoggingListener then
    LogError(LogCategory, isc_sqlcode, Sender, LogMessage, ErrorString);
  { in case second isc_status is zero(no error) and third is tagged as a warning it's a /are multiple warning(s)
    otoh it's an error with a possible warning(s)}
  if (WarningArg = isc_arg_warning)
  then ExeptionClass := EZSQLWarning
  else if LogCategory = lcConnect
    then ExeptionClass := EZIBSQLException
    else case error_code of
      isc_network_error..isc_net_write_err, isc_lost_db_connection, isc_att_shut_idle,
      isc_att_shut_db_down, isc_att_shut_engine: ExeptionClass := EZSQLConnectionLost
      else ExeptionClass := EZIBSQLException;
    end;
  //used for clearing the current status vector, OTH, we permanently need a new IStatus, or IStatus.Init.
  FillChar(Pointer(OrgStatusVector)^, (PAnsiChar(StatusVector) - PAnsiChar(OrgStatusVector))+SizeOf(ISC_STATUS), #0);//init the vector again for FB3+
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
    if LogCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;//changed by Fr0st SSQLError2;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
  then FLogMessage := Format(FormatStr, [ErrorString, isc_sqlcode, LogMessage])
  else FLogMessage := Format(FormatStr, [ErrorString, isc_sqlcode]);
  if error_code <> 0 then
    FLogMessage := FLogMessage +  '; GDS Code: ' + SysUtils.IntToStr(error_code);
  if ExeptionClass = EZIBSQLException //added by Fr0st
  then Error := EZIBSQLException.Create(FLogMessage, InterbaseStatusVector, LogMessage)
  else begin
    Error := ExeptionClass.CreateWithCode(isc_sqlcode, FlogMessage);
    if ExeptionClass = EZSQLWarning then begin
      ClearWarnings;
      if not RaiseWarnings then begin
        FLastWarning := EZSQLWarning(Error);
        Error := nil;
      end;
    end else if (Sender <> nil)
      then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
      else ReleaseImmediat(Self, EZSQLConnectionLost(Error))
  end;
  FLogMessage := '';
  if Error <> nil then
    raise Error;
end;

function TZInterbaseFirebirdConnection.IsListening: Boolean;
begin
  Result := not IsClosed and (FCreatedWeakEventListenerPtr <> nil) and (FEventList <> nil) and (FEventList.FEventBlocks <> nil);
end;

function TZInterbaseFirebirdConnection.IsTransactionValid(
  const Value: IZTransaction): Boolean;
begin
  Result := fTransactions.IndexOf(Value) >= 0;
end;

procedure TZInterbaseFirebirdConnection.InternalClose;
var I: Integer;
  Txn: IZTransaction;
begin
  AutoCommit := not FRestartTransaction;
  if fTransactions <> nil then begin
    for I := fTransactions.Count -1 downto 0 do
      if (fTransactions[i] <> nil) and (fTransactions[i].QueryInterface(IZTransaction, Txn) = S_OK) then
        Txn.Close;
    fTransactions.Clear;
    fActiveTransaction := nil;
  end;
  if FEventList <> nil then begin
    if FEventList.FEventBlocks <> nil then
      UnListen;
    FreeAndNil(FEventList);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZInterbaseFirebirdConnection.InterpretInterbaseStatus(
  var StatusVector: PARRAY_ISC_STATUS): TZIBStatusVector;
var //StatusIdx: Integer; EH: that leads to ugly rangecheck issues, since FP_Interpret is incrementing the ptr -> dead memory
    pCurrStatus: PZIBStatus;
    {$IF defined(Unicode) or defined(WITH_RAWBYTESTRING)}
    CP: Word;
    {$IFEND}
    NextStatusVector{EH: that should mimic Fr0st's vector array, but does not overrun the memory}: PARRAY_ISC_STATUS;
begin
  Result := nil;
  //StatusIdx := 0;
  {$IF defined(Unicode) or defined(WITH_RAWBYTESTRING)}
  if ConSettings.ClientCodePage = nil
  then CP := DefaultSystemCodePage
  else CP := ConSettings.ClientCodePage.CP;
  {$IFEND}
  repeat
    SetLength(Result, Length(Result) + 1);
    pCurrStatus := @Result[High(Result)]; // save pointer to avoid multiple High() calls
    // SQL code and status
    pCurrStatus.SQLCode := FInterbaseFirebirdPlainDriver.isc_sqlcode(PISC_STATUS(StatusVector));
    FInterbaseFirebirdPlainDriver.isc_sql_interprete(pCurrStatus.SQLCode, @FByteBuffer[0], SizeOf(TByteBuffer)-1);
    if FByteBuffer[0] <> 0 then
      {$IFDEF UNICODE}
      pCurrStatus.SQLMessage := PRawToUnicode(PAnsiChar(@FByteBuffer[0]), ZFastCode.StrLen(@FByteBuffer[0]), CP);
      {$ELSE}
      ZSetString(PAnsiChar(@FByteBuffer[0]), ZFastCode.StrLen(@FByteBuffer[0]), RawByteString(pCurrStatus.SQLMessage){$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF});
      {$ENDIF}
    //older compile would gangle about possibly unassigned, newers gangle about assigned but never used
    //so let's use the variable in next cast line and all are happy
    NextStatusVector := StatusVector;
    // IB data
    pCurrStatus.IBDataType := PISC_STATUS(NextStatusVector)^;//StatusVector[StatusIdx];
    case PISC_STATUS(StatusVector)^ {StatusVector[StatusIdx]} of
      isc_arg_end:  // end of argument list
        Break;
      isc_arg_gds,  // Long int code
      isc_arg_number,
      isc_arg_vms,
      isc_arg_unix,
      isc_arg_domain,
      isc_arg_dos,
      isc_arg_mpexl,
      isc_arg_mpexl_ipc,
      isc_arg_next_mach,
      isc_arg_netware,
      isc_arg_win32:
        begin
          pCurrStatus.IBDataInt := StatusVector[{StatusIdx + }1];
          NextStatusVector := @StatusVector[2];
          //Inc(StatusIdx, 2);
        end;
      isc_arg_string,  // pointer to string
      isc_arg_interpreted,
      isc_arg_sql_state:
        begin
          pCurrStatus.IBDataStr := ConvertConnRawToString({$IFDEF UNICODE}
            ConSettings,{$ENDIF}Pointer(StatusVector[{StatusIdx + }1]));
          NextStatusVector := @StatusVector[2];
          //Inc(StatusIdx, 2);
        end;
      isc_arg_cstring: // length and pointer to string
        begin
          pCurrStatus.IBDataStr := ConvertConnRawToString({$IFDEF UNICODE}
            ConSettings,{$ENDIF}Pointer(StatusVector[{StatusIdx + }2]), StatusVector[{StatusIdx + }1]);
          NextStatusVector := @StatusVector[3];
          //Inc(StatusIdx, 3);
        end;
      isc_arg_warning: begin// must not happen for error vector
        Break; //how to handle a warning? I just need an example
      end
      else
        Break;
    end; // case
    if Assigned(FInterbaseFirebirdPlainDriver.fb_interpret) then begin
      if FInterbaseFirebirdPlainDriver.fb_interpret(@FByteBuffer[0], SizeOf(TByteBuffer)-1, @StatusVector) = 0 then
        Break;
    end else if FInterbaseFirebirdPlainDriver.isc_interprete(@FByteBuffer[0], @StatusVector) = 0 then
      Break;
    if PAnsiChar(StatusVector) < PAnsiChar(NextStatusVector) then
      StatusVector := NextStatusVector;
    if PISC_Status(StatusVector)^ = isc_arg_end then //EH: otoh in some
      //cirumstances we add a empty status see test TestDbcTransaction
      Break;
    pCurrStatus.IBMessage := ConvertConnRawToString({$IFDEF UNICODE}
            ConSettings,{$ENDIF}@FByteBuffer[0]);
  until False;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Sender" not used} {$ENDIF}
procedure TZInterbaseFirebirdConnection.OnPropertiesChange(Sender: TObject);
var HC: Boolean;
    TPB: RawByteString;
begin
  HC := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
  FGUIDProps.InitFromProps(Info);
  TPB := GenerateTPB(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
  if (fActiveTransaction <> nil) and ((HC <> FHardCommit) or
     (fActiveTransaction.GetTPB <> TPB)) then//*** ADDED THIS CHECK by EMartin ***
    TransactionParameterPufferChanged;
  FHardCommit := HC;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZInterbaseFirebirdConnection.ReleaseTransaction(
  const Value: IZTransaction);
var idx: Integer;
  Trans: IZTransaction;
begin
  if (fActiveTransaction <> nil) then begin
    fActiveTransaction.QueryInterface(IZTransaction, Trans);
    if (Trans = Value) then begin
      fActiveTransaction := nil;
    end;
  end;
  Idx := fTransactions.IndexOf(Value);
  if Idx <> -1 then begin
    fTransactions.Delete(Idx);
    Exit;
  end;
  raise EZSQLException.Create('release an invalid Transaction');
end;

procedure TZInterbaseFirebirdConnection.Rollback;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  with GetActiveTransaction do begin
    Rollback;
    if (not FRestartTransaction) and (GetTransactionLevel <= 0) then
      SetAutoCommit(True)
  end;
end;

procedure TZInterbaseFirebirdConnection.SetActiveTransaction(
  const Value: IZTransaction);
var Transaction: IZInterbaseFirebirdTransaction;
begin
  Transaction := nil;
  if (Value = nil) or (Value.QueryInterface(IZInterbaseFirebirdTransaction, Transaction) <> S_OK) then
    raise EZSQLException.Create('invalid IB/FB transaction');
  fActiveTransaction := Transaction;
end;

procedure TZInterbaseFirebirdConnection.SetAutoCommit(Value: Boolean);
begin
  FRestartTransaction := not Value;
  if (Value <> AutoCommit) then begin
    TransactionParameterPufferChanged;
    AutoCommit := Value;
    //restart automatically happens on GetTrHandle
  end;
end;

procedure TZInterbaseFirebirdConnection.SetOnEventAlert(
  Value: TZFirebirdInterbaseEventAlert);
begin
  if FEventList <> nil then
    FEventList.FOnEventAlert := Value
  else raise EZSQLException.Create('No active listener acquired');
end;

procedure TZInterbaseFirebirdConnection.SetReadOnly(Value: Boolean);
begin
  if (ReadOnly <> Value) then begin
    TransactionParameterPufferChanged;
    ReadOnly := Value;
    //restart automatically happens on GetTrHandle
  end;
end;

procedure TZInterbaseFirebirdConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if (Level <> TransactIsolationLevel) then begin
    TransactionParameterPufferChanged;
    TransactIsolationLevel := Level;
    //restart automatically happens on GetTrHandle
  end;
end;

function TZInterbaseFirebirdConnection.StartTransaction: Integer;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    AutoCommit := False;
    TransactionParameterPufferChanged;
  end else GetActiveTransaction.DoStartTransaction;
  Result := GetActiveTransaction.StartTransaction;
end;

function TZInterbaseFirebirdConnection.StoredProcedureIsSelectable(
  const ProcName: String): Boolean;
var I: Integer;
  function AddToCache(const ProcName: String): Boolean;
  var RS: IZResultSet;
    Stmt: IZStatement;
    DbInfo: IZInterbaseDatabaseInfo;
  begin
    Result := False;
    Supports(GetMetadata.GetDatabaseInfo, IZInterbaseDatabaseInfo, DbInfo);

    if Assigned(DbInfo) and DbInfo.HostIsFireBird and (DbInfo.GetHostVersion >= 1005000) then begin
      Stmt := IZConnection(fWeakReferenceOfSelfInterface).CreateStatementWithParams(Info);
      RS := Stmt.ExecuteQuery('SELECT RDB$PROCEDURE_TYPE FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = '+QuotedStr(ProcName));
      if RS <> nil then try
        if RS.Next then begin
          Result := RS.GetShort(FirstDbcIndex)=1; //Procedure type 2 has no suspend
          FProcedureTypesCache.AddObject(ProcName, TObject(Ord(Result)));
        end else
          Raise EZUnsupportedException.Create(SUnsupportedOperation);
      finally
        RS.Close;
        RS := nil;
        Stmt := nil;
      end;
    end;
  end;
begin
  I := FProcedureTypesCache.IndexOf(ProcName);
  if I = -1
  then Result := AddToCache(ProcName)
  else Result := FProcedureTypesCache.Objects[I] <> nil;
end;

procedure TZInterbaseFirebirdConnection.TransactionParameterPufferChanged;
begin
  if (fActiveTransaction <> nil) then begin
    if (FTransactions.IndexOf(fActiveTransaction) = 0) then
      FTransactions.Insert(0, nil); //zero index is for main txn...
    ReleaseTransaction(fActiveTransaction);
  end;
end;

procedure TZInterbaseFirebirdConnection.TriggerEvent(const Name: String);
var RawTemp: RawByteString;
begin
  {$IFDEF UNICODE}
  RawTemp := 'EXECUTE BLOCK AS BEGIN POST_EVENT '+SQLQuotedStr(ZUnicodeToRaw(Name, ConSettings.ClientCodePage.CP), AnsiChar(#39))+'; END';
  {$ELSE}
  RawTemp := 'POST_EVENT '+SQLQuotedStr(Name, AnsiChar(#39))+'; END';
  {$ENDIF}
  ExecuteImmediat(RawTemp, lcExecute);
end;

procedure TZInterbaseFirebirdConnection.Unlisten;
begin
  if (FEventList <> nil) and (FEventList.Count > 0) then begin
    if FEventList.FEventBlocks <> nil then
      FEventList.UnregisterEvents;
    FEventList.Clear;
  end else
    raise EZSQLException.Create('no events registered');
end;

{ TZInterbaseFirebirdTransaction }

procedure TZInterbaseFirebirdTransaction.AfterConstruction;
var Trans: IZTransaction;
begin
  QueryInterface(IZTransaction, Trans);
  FWeakIZTransactionPtr := Pointer(Trans);
  Trans := nil;
  inherited AfterConstruction;
end;

procedure TZInterbaseFirebirdTransaction.BeforeDestruction;
begin
  FOpenCursors.Clear;
  FOpenUncachedLobs.Clear;
  fSavepoints.Clear;
  if TxnIsStarted then
    if fDoCommit
    then IZTransaction(FWeakIZTransactionPtr).Commit
    else IZTransaction(FWeakIZTransactionPtr).RollBack;
  if Assigned(fSavepoints) then
    FreeAndNil(fSavepoints);
  if Assigned(FOpenCursors) then
    FreeAndNil(FOpenCursors);
  if Assigned(FOpenUncachedLobs) then
    FreeAndNil(FOpenUncachedLobs);
  if Assigned(FProperties) then
    FreeAndNil(FProperties);
  inherited BeforeDestruction;
end;

constructor TZInterbaseFirebirdTransaction.Create(const Owner: TZInterbaseFirebirdConnection;
  AutoCommit, ReadOnly: Boolean; TransactionIsolation: TZTransactIsolationLevel;
  Properties: TStrings);
begin
  FOwner := Owner;
  FOpenCursors := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FOpenUncachedLobs := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  fSavepoints := TStringList.Create;
  fDoLog := True;
  FReadOnly := ReadOnly;
  FAutoCommit := AutoCommit;
  ConSettings := Owner.ConSettings;
  FProperties := TStringList.Create;
  FTransactionIsolation := TransactionIsolation;
  if Properties <> nil
  then FProperties.AddStrings(Properties)
  else FProperties.AddStrings(FOwner.Info);
end;

procedure TZInterbaseFirebirdTransaction.DeRegisterOpencursor(const CursorRS: IZResultSet);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenCursors <> nil, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  I := FOpenCursors.IndexOf(Pointer(CursorRS));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenCursor beahvior'); {$ENDIF DEBUG}
  FOpenCursors.Delete(I);
end;

procedure TZInterbaseFirebirdTransaction.DeRegisterOpenUnCachedLob(const Lob: IZlob);
var I: Integer;
begin
  {$IFDEF DEBUG}Assert(FOpenUncachedLobs <> nil, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  I := FOpenUncachedLobs.IndexOf(Pointer(Lob));
  {$IFDEF DEBUG}Assert(I > -1, 'Wrong DeRegisterOpenUnCachedLob beahvior'); {$ENDIF DEBUG}
  FOpenUncachedLobs.Delete(I);
end;

function TZInterbaseFirebirdTransaction.GetTransactionLevel: Integer;
begin
  if TxnIsStarted
  then Result := Ord(not FAutoCommit)+FSavePoints.Count
  else Result := -1;
end;

function TZInterbaseFirebirdTransaction.GetOpenCursorCount: Integer;
begin
  Result := FOpenCursors.Count;
end;

function TZInterbaseFirebirdTransaction.GetTPB: RawByteString;
begin
  if FTPB = EmptyRaw then
    FTPB := FOwner.GenerateTPB(FAutoCommit, FReadOnly, FTransactionIsolation, FProperties);
  Result := FTPB;
end;

function TZInterbaseFirebirdTransaction.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

function TZInterbaseFirebirdTransaction.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TZInterbaseFirebirdTransaction.RegisterOpencursor(const CursorRS: IZResultSet);
begin
  FOpenCursors.Add(Pointer(CursorRS));
end;

procedure TZInterbaseFirebirdTransaction.RegisterOpenUnCachedLob(const Lob: IZlob);
begin
  FOpenUncachedLobs.Add(Pointer(Lob));
end;

procedure TZInterbaseFirebirdTransaction.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var I: Integer;
  ImmediatelyReleasable: IImmediatelyReleasable;
begin
  fSavepoints.Clear;
  I := FOwner.FTransactions.IndexOf(Self);
  if I > -1 then
    FOwner.FTransactions.Delete(I);
  for i := FOpenUncachedLobs.Count -1 downto 0 do
    if Supports(IZBlob(FOpenUncachedLobs[i]), IImmediatelyReleasable, ImmediatelyReleasable) and
       (Sender <> ImmediatelyReleasable) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
end;

procedure TZInterbaseFirebirdTransaction.SetAutoCommit(Value: Boolean);
begin
  if Value <> FAutoCommit then begin
    if TxnIsStarted then
      if not FAutoCommit
      then raise EZSQLException.Create(SInvalidOpInNonAutoCommit)
      else {if FOwner.IsFirebirdLib And (FOwner.GetHostVersion >= 4000000) then begin
      end else }begin
        IZTransaction(FWeakIZTransactionPtr).Close;
        FTPB := EmptyRaw;
      end;
    FAutoCommit := Value;
  end;
end;

procedure TZInterbaseFirebirdTransaction.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then begin
    if TxnIsStarted then
      if not FAutoCommit
      then raise EZSQLException.Create(SInvalidOpInNonAutoCommit)
      else IZTransaction(FWeakIZTransactionPtr).Close;
    FReadOnly := Value;
    FTPB := EmptyRaw;
  end;
end;

procedure TZInterbaseFirebirdTransaction.SetTransactionIsolation(
  Value: TZTransactIsolationLevel);
begin
  if Value <> FTransactionIsolation then begin
    if TxnIsStarted then
      if not FAutoCommit
      then raise EZSQLException.Create(SInvalidOpInNonAutoCommit)
      else IZTransaction(FWeakIZTransactionPtr).Close;
    FTransactionIsolation := Value;
    FTPB := EmptyRaw;
  end;
end;

{ TZInterbaseFirebirdSequence }

function TZInterbaseFirebirdSequence.GetCurrentValueSQL: string;
begin
  Result := 'SELECT GEN_ID('+FName+', 0) FROM RDB$DATABASE';
end;

function TZInterbaseFirebirdSequence.GetNextValueSQL: string;
begin
  with Connection.GetMetadata do begin
    if (GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsNextValueFor and (FBlockSize = 1)
    then Result := ' NEXT VALUE FOR '+FName
    else Result := ' GEN_ID('+FName+', '+ZFastcode.IntToStr(FBlockSize)+') ';
    Result := 'SELECT '+Result+' FROM RDB$DATABASE';
  end;
end;

procedure TZInterbaseFirebirdSequence.SetBlockSize(const Value: Integer);
begin
  if Value <> fBlockSize then begin
    FlushResults;
    inherited SetBlockSize(Value);
  end;
end;

{ TZInterbaseFirebirdResultSetMetadata }

procedure TZInterbaseFirebirdResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ColumnIndex" not used} {$ENDIF}
function TZInterbaseFirebirdResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZInterbaseFirebirdResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ColumnIndex" not used} {$ENDIF}
function TZInterbaseFirebirdResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZInterbaseFirebirdResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ColumnIndex" not used} {$ENDIF}
function TZInterbaseFirebirdResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := False; //not supported by FB<3/IB
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Initializes columns with additional data.
}
procedure TZInterbaseFirebirdResultSetMetadata.LoadColumns;
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  IdentifierConverter: IZIdentifierConverter;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
begin
  Connection := Metadata.GetConnection;
  Analyser := Connection.GetStatementAnalyser;
  Tokenizer := Connection.GetTokenizer;
  IdentifierConverter := Metadata.GetIdentifierConverter;
  try
    if Analyser.DefineSelectSchemaFromQuery(Tokenizer, SQL) <> nil then
      for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
        Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
        ClearColumn(Current);
        if Current.TableName = '' then
          continue;
        TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(IdentifierConverter.Quote(Current.TableName, iqTable)),'');
        if TableColumns <> nil then begin
          TableColumns.BeforeFirst;
          while TableColumns.Next do
            if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
              FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
              Break;
            end;
        end;
      end;
  finally
    Connection := nil;
    Analyser := nil;
    Tokenizer := nil;
    IdentifierConverter := nil;
  end;
  Loaded := True;
end;

procedure TZInterbaseFirebirdResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
end;

procedure TZInterbaseFirebirdResultSetMetadata.SetColumnTypeFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
var Precision: Integer;
begin
  //FB native ResultSet can't give use users choosen precision for the Numeric/Decimal Fields
  //so a ISC_INT64 type with scale smaller then four will always be
  //mapped to stBigDecimal while it could be a stCurrency type. Let's test it!
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then begin
    Precision := TableColumns.GetInt(TableColColumnSizeIndex);
    if (ColumnInfo.ColumnType = stBigDecimal) and (ColumnInfo.Scale <= 4) and
       (Precision < sAlignCurrencyScale2Precision[ColumnInfo.Scale]) then
      ColumnInfo.ColumnType := stCurrency;
  end else
    inherited SetColumnTypeFromGetColumnsRS(ColumnInfo, TableColumns);
end;

{ TZInterbaseFirebirdCachedResolver }

constructor TZInterbaseFirebirdCachedResolver.Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  Fields: string;
begin
  inherited;
  Fields := Statement.GetParameters.Values[DSProps_InsertReturningFields];
  if Fields <> '' then
    FInsertReturningFields := ExtractFields(Fields, [';', ',']);
  FReturningPairs := TZIndexPairList.Create;
end;

destructor TZInterbaseFirebirdCachedResolver.Destroy;
begin
  inherited;
  FreeAndNil(FInsertReturningFields);
  FreeAndNil(FReturningPairs);
end;

function TZInterbaseFirebirdCachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): string;
// --> ms, 30/10/2005
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(RowAccessor, ColumnsLookup);
  if Result <> '' then begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0
    then Result := copy(Result, 1, iPos+3) + ' RDB$DATABASE'
    else Result := Result + ' FROM RDB$DATABASE';
  end;
// <-- ms
end;

function TZInterbaseFirebirdCachedResolver.FormInsertStatement(
  NewRowAccessor: TZRowAccessor): SQLString;
var
  I, ColumnIndex: Integer;
  Tmp: SQLString;
  SQLWriter: TZSQLStringWriter;
  {$IF DECLARED(DSProps_InsertReturningFields)}
  Fields: TStrings;
  {$IFEND}
begin
  I := MetaData.GetColumnCount;
  SQLWriter := TZSQLStringWriter.Create(512+(I shl 5));
  {$IF DECLARED(DSProps_InsertReturningFields)}
  if FInsertReturningFields <> nil then begin
    Fields := TStringList.Create;
    Fields.Assign(FInsertReturningFields);
  end else Fields := nil;
  {$IFEND}
  Result := 'INSERT INTO ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    SQLWriter.AddChar(' ', Result);
    SQLWriter.AddChar('(', Result);
    if (FInsertStatements.Count > 0) and FHasWritableAutoIncrementColumns then
      FInsertColumns.Clear;
    if (FInsertColumns.Count = 0) then
      FillInsertColumnsPairList(NewRowAccessor);
    if (FInsertColumns.Count = 0) and not
       {test for generated always cols }
       ((Metadata.GetColumnCount > 0) and Metadata.IsAutoIncrement(FirstDbcIndex)) then begin
      Result := '';
      Exit;
    end;
    for I := 0 to FInsertColumns.Count-1 do begin
      ColumnIndex := PZIndexPair(FInsertColumns[i])^.ColumnIndex;
      Tmp := Metadata.GetColumnName(ColumnIndex);
      Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);
    SQLWriter.AddText(' VALUES (', Result);
    for I := 0 to FInsertColumns.Count - 1 do begin
      SQLWriter.AddChar('?', Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);
    FReturningPairs.Clear;
    for i := FirstDbcIndex to MetaData.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if Metadata.IsAutoIncrement(I) then begin
        FHasAutoIncrementColumns := True;
        if Metadata.IsWritable(I) then begin
          FHasWritableAutoIncrementColumns := True;
          if not NewRowAccessor.IsNull(I) then
            Continue;
        end;
        if FReturningPairs.Count = 0 then
          SQLWriter.AddText(' RETURNING ', Result);
        FReturningPairs.Add(I, FReturningPairs.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        Tmp := Metadata.GetColumnName(I);
        {$IF DECLARED(DSProps_InsertReturningFields)}
        if (Fields <> nil) then begin
          ColumnIndex := Fields.IndexOf(Tmp);
          if ColumnIndex > -1 then
            Fields.Delete(ColumnIndex); { avoid duplicates }
        end;
        {$IFEND}
        Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
        SQLWriter.AddText(Tmp, Result);
        SQLWriter.AddChar(',', Result);
      end;
    {$IF DECLARED(DSProps_InsertReturningFields)}
    if (Fields <> nil) and (Fields.Count > 0) then begin
      if FReturningPairs.Count = 0 then
        SQLWriter.AddText(' RETURNING ', Result);
      for I := 0 to Fields.Count - 1 do begin
        Tmp := Fields[I];
        ColumnIndex := MetaData.FindColumn(Tmp);
        if ColumnIndex = InvalidDbcIndex then
          raise CreateColumnWasNotFoundException(Tmp);
        FReturningPairs.Add(ColumnIndex, FReturningPairs.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
        SQLWriter.AddText(Tmp, Result);
        SQLWriter.AddChar(',', Result);
      end;
    end;
    {$IFEND}
    SQLWriter.CancelLastComma(Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
    {$IF DECLARED(DSProps_InsertReturningFields)}
    FreeAndNil(Fields);
    {$IFEND}
  end;
end;

procedure TZInterbaseFirebirdCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);
  if (UpdateType = utInserted) then begin
    if (FReturningPairs.Count >0) then
      UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
    if FHasWritableAutoIncrementColumns then
      InsertStatement := nil;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "?" not used} {$ENDIF}
procedure TZInterbaseFirebirdCachedResolver.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; const
  OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
var
  I: Integer;
  RS: IZResultSet;
begin
  RS := InsertStatement.GetResultSet;
  if (RS <> nil) then try
    if RS.Next then
      for i := 0 to FReturningPairs.Count -1 do with PZIndexPair(FReturningPairs[I])^ do
        NewRowAccessor.SetValue(SrcOrDestIndex, RS.GetValue(ColumnIndex));
  finally
    RS.Close; { Without Close RS keeps circular ref to Statement causing mem leak }
    RS := nil;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{ TZFirebird2upCachedResolver }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "OldRowAccessor" not used} {$ENDIF}
procedure TZFirebird2upCachedResolver.FormWhereClause(
  const SQLWriter: TZSQLStringWriter; const OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, idx: Integer;
  Tmp, S: SQLString;
begin
  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do begin
    idx := PZIndexPair(WhereColumns[I]).ColumnIndex;
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    S := MetaData.GetColumnName(idx);
    Tmp := IdentifierConverter.Quote(S, iqColumn);
    SQLWriter.AddText(Tmp, Result);
    if (Metadata.IsNullable(Idx) = ntNullable)
    then SQLWriter.AddText(' IS NOT DISTINCT FROM ?', Result)
    else SQLWriter.AddText('=?', Result);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{ TZFirebird3upResultSetMetadata }

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZFirebird3upResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).AutoIncrement;
end;

{ TZInterbaseFirebirdColumnInfo }

function TZInterbaseFirebirdColumnInfo.GetPCharFromTextVar(
  out Len: NativeUInt): PAnsiChar;
begin
  if SQLType = SQL_TEXT then begin
    Result := sqldata;
    if ColumnCodePage = zCP_Binary
    then Len := CharOctedLength
    else Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, CharOctedLength);
  end else begin
    Result := @PISC_VARYING(sqldata).str[0];
    Len := PISC_VARYING(sqldata).strlen;
  end;
end;

{ TZAbstractInterbaseFirebirdResultSet }

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAbstractInterbaseFirebirdResultSet.AfterClose;
begin
  FreeAndNil(FGUIDProps);
  inherited AfterClose;
end;

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TZAbstractInterbaseFirebirdResultSet.ColumnsToJSON(
  JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
var L, H, I: Integer;
    P: Pointer;
    C: ISC_SHORT;
    TempDate: TZTimeStamp;//TCTimeStructure;
  procedure WConvert(P: PAnsiChar; L: ISC_SHORT; CP: word); //no _UStrClr in method
  begin
    FUniTemp := PRawToUnicode(P, L, CP);
    JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp))
  end;
  procedure ReadUTF8CLob(Index: Integer);
  var Stream: TStream;
    IbStream: TZAbstractInterbaseFirebirdLobStream;
    Clob: IZCLob;
    Blob: IZBlob;
    Buf: PAnsiChar;
    L, R, Size: NativeInt;
  begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Blob.QueryInterface(IZClob, CLob);
    Blob := nil;
    Stream := Clob.GetStream(zCP_UTF8);
    IbStream := Stream as TZAbstractInterbaseFirebirdLobStream;
    Buf := nil;
    try
      Size := Stream.Size;
      if Size = 0 then Exit;
      { read chunked as firebird supports it }
      L := IbStream.BlobInfo.MaxSegmentSize;
      GetMem(Buf, L);
      repeat
        R := Stream.Read(Buf^, L);
        JSONWriter.AddJSONEscape(Buf, R); //is not #0 terminated
        Dec(Size, R);
        if L > Size then
          L := Size;
      until (R = 0){should not happen} or
            (Size = 0){if segmentsize < total};
    finally
      Stream.Free;
      FreeMem(Buf);
      Clob := nil;
    end;
  end;
  procedure ReadAsWCLob(Index: Integer);
  var Clob: IZCLob;
    Blob: IZBlob;
    PW: Pointer;
    L: NativeUInt;
  begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    Blob.QueryInterface(IZClob, CLob);
    Blob := nil;
    try
      PW := CLob.GetPWideChar(FUniTemp, L);
      JSONWriter.AddJSONEscapeW(PW, L); //is not #0 terminated
      FUniTemp := '';
    finally
      Clob := nil;
    end;
  end;
  procedure ReadBLob(Index: Integer);
  var Blob: IZBLob;
    P: Pointer;
    L: NativeUInt;
  begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    try
      P := Blob.GetBuffer(FRawTemp, L); //base 64 can not be added in chunks ):
      JSONWriter.WrBase64(P, L, True);
      FRawTemp := '';
    finally
      Blob := nil;
    end;
  end;
begin
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    {$R-}
    with TZInterbaseFirebirdColumnInfo(ColumnsInfo[c]) do
      if (sqlind <> nil) and (sqlind^ = ISC_NULL) then
        if JSONWriter.Expand then begin
          if not (jcsSkipNulls in JSONComposeOptions) then begin
            JSONWriter.AddString(JSONWriter.ColNames[I]);
            JSONWriter.AddShort('null,')
          end;
        end else
          JSONWriter.AddShort('null,')
      else begin
        if JSONWriter.Expand then
          JSONWriter.AddString(JSONWriter.ColNames[I]);
        case sqltype of
          SQL_VARYING   : if ColumnCodePage = zCP_Binary {octets} then
                            JSONWriter.WrBase64(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, True)
                          else begin
                            JSONWriter.Add('"');
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen)
                            else WConvert(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_TEXT      : if ColumnCodePage = zCP_Binary then
                            JSONWriter.WrBase64(sqldata, CharOctedLength, True)
                          else begin
                            JSONWriter.Add('"');
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(sqldata, L)
                            else WConvert(sqldata, L, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_D_FLOAT,
          SQL_DOUBLE    : JSONWriter.AddDouble(PDouble(sqldata)^);
          SQL_FLOAT     : JSONWriter.AddSingle(PSingle(sqldata)^);
          SQL_SHORT     : if sqlscale = 0 then
                            JSONWriter.Add(PISC_SHORT(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(FByteBuffer), @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer), PAnsiChar(P)-PAnsiChar(FByteBuffer));
                          end;
          SQL_LONG      : if sqlscale = 0 then
                            JSONWriter.Add(PISC_LONG(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(PISC_LONG(sqldata)^, PAnsiChar(FByteBuffer), @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer), PAnsiChar(P)-PAnsiChar(FByteBuffer));
                          end;
          SQL_INT64     : if (sqlscale = 0) then
                            JSONWriter.Add(PISC_INT64(sqldata)^)
                          else if sqlScale = -4 then
                            JSONWriter.AddCurr64({$IFDEF MORMOT2}PInt64(sqldata){$ELSE}PISC_INT64(sqldata)^{$ENDIF})
                          else begin
                            ScaledOrdinal2Raw(PISC_INT64(sqldata)^, PAnsiChar(FByteBuffer), @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer), PAnsiChar(P)-PAnsiChar(FByteBuffer));
                          end;
          SQL_TIMESTAMP : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                              TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(Pointer(FByteBuffer), True, TempDate.Year, TempDate.Month, TempDate.Day);
                            isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                              TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(Pointer(PAnsiChar(FByteBuffer)+10), True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second, TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer),19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_QUAD,
          SQL_BLOB      : begin
                            if ColumnCodePage <> zCP_Binary then begin
                              JSONWriter.Add('"');
                              if ColumnCodePage = zCP_UTF8
                              then ReadUTF8CLob(C)
                              else ReadAsWCLob(C);
                              JSONWriter.Add('"');
                            end else ReadBlob(C);
                          end;
          //SQL_ARRAY     : JSONWriter.AddShort('"Array"');
          SQL_TYPE_TIME : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            end else
                              JSONWriter.Add('"');
                            isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                              TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(Pointer(FByteBuffer), True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second,  TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer),9+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
          SQL_TYPE_DATE : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_DATE(sqldata)^, TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(Pointer(FByteBuffer), True, TempDate.Year, TempDate.Month, Tempdate.Day);
                            JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_BOOLEAN   : JSONWriter.AddShort(JSONBool[PISC_BOOLEAN(sqldata)^ <> 0]);
          SQL_BOOLEAN_FB: JSONWriter.AddShort(JSONBool[PISC_BOOLEAN_FB(sqldata)^ <> 0]);
          else          raise ZDbcUtils.CreateConversionError(C, ColumnType, stString);
        end;
        JSONWriter.Add(',');
      end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF WITH_COLUMNS_TO_JSON}

constructor TZAbstractInterbaseFirebirdResultSet.Create(
  const Statement: IZStatement; const SQL: string);
var Connection: IZConnection;
  RSMetadata: TContainedObject;
  Metadata: IZDatabaseMetadata;
  IB_FB_Connection: IZInterbaseFirebirdConnection;
begin
  Connection := Statement.GetConnection;
  IB_FB_Connection := Connection as IZInterbaseFirebirdConnection;
  FByteBuffer := IB_FB_Connection.GetByteBufferAddress;
  Metadata := Connection.GetMetadata;
  if IB_FB_Connection.IsFirebirdLib and (IB_FB_Connection.GetHostVersion >= 3000000)
  then RSMetadata := TZFirebird3upResultSetMetadata.Create(Metadata, SQL, Self)
  else RSMetadata := TZInterbaseFirebirdResultSetMetadata.Create(Metadata, SQL, Self);
  inherited Create(Statement, SQL, RSMetadata, Connection.GetConSettings);
  FGUIDProps := TZInterbaseFirebirdStatementGUIDProps.Create(Statement);
  FIsMetadataResultSet := (ConSettings.ClientCodePage.ID = CS_NONE) and
    (Statement.GetParameters.Values[DS_Props_IsMetadataResultSet] = 'True');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in operating system encoding.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractInterbaseFirebirdResultSet.GetAnsiString(
  ColumnIndex: Integer): AnsiString;
var
  P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: AnsiString);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(ZOSCodePage, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if not LastWasNull then case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := CharOctedLength;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(P), CharOctedLength);
                          if (ColumnCodePage = ZOSCodePage)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
                        if (ColumnCodePage = ZOSCodePage) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, ZOSCodePage, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(ColumnIndex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_ANSISTRING}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>BigDecimal</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      LastWasNull := True;
      PCardinal(@Result.Precision)^ := 0;
    end else case sqltype of
      SQL_BOOLEAN   : ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN(sqldata)^ <> 0), 0, Result);
      SQL_BOOLEAN_FB: ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN_FB(sqldata)^ <> 0), 0, Result);
      SQL_SHORT     : ScaledOrdinal2Bcd(PISC_SHORT(sqldata)^, Byte(Scale), Result);
      SQL_LONG      : ScaledOrdinal2Bcd(PISC_LONG(sqldata)^, Byte(Scale), Result);
      SQL_INT64     : ScaledOrdinal2Bcd(PISC_INT64(sqldata)^, Byte(Scale), Result);
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        LastWasNull := not TryRawToBCD(P, Len, Result, '.');
                      end;
      SQL_D_FLOAT,
      SQL_DOUBLE,
      SQL_FLOAT,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIME : Double2BCD(GetDouble(ColumnIndex), Result);
      (*SQL_DEC_FIXED, SQL_INT128: begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryRawToBCD(P, Len, Result, '.');
        end;*)
      else raise CreateConversionError(ColumnIndex, ColumnType, stBigDecimal);
    end;
  end;
end;

function TZAbstractInterbaseFirebirdResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then
      Result := False
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^ <> 0;
      SQL_FLOAT     : Result := PSingle(sqldata)^ <> 0;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^ <> 0;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^ <> 0;
      SQL_LONG      : Result := PISC_LONG(sqldata)^ <> 0;
      SQL_SHORT     : Result := PISC_SHORT(sqldata)^ <> 0;
      SQL_QUAD,
      SQL_INT64     : Result := PISC_INT64(sqldata)^ <> 0;
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := StrToBoolEx(P, P+Len);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stBoolean);
    end;
  end;
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
  function FromLob(ColumnIndex: Integer; out Len: NativeUInt): PByte;
  var Lob: IZBlob;
  begin
    Lob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    Result := Lob.GetBuffer(FRawTemp, Len);
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Result := nil;
      Len := 0;
    end else case sqltype of
      SQL_TEXT: begin
          Result := sqldata;
          Len := CharOctedLength;
        end;
      SQL_VARYING: begin
          Result := @PISC_VARYING(sqldata).str[0];
          Len := PISC_VARYING(sqldata).strlen;
        end;
      SQL_QUAD,
      SQL_BLOB: Result := FromLob(ColumnIndex, Len);
      else begin
        Result := PByte(sqldata);
        Len := CharOctedLength;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZDate</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>zero-padded</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var
  Len: NativeUInt;
  P: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then PInt64(@Result.Year)^ := 0
    else case sqltype of
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          Result.Year, Result.Month, Result.Day);
                        Result.IsNegative := False;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^, Result.Year, Result.Month, Result.Day);
                        Result.IsNegative := False;
                      end;
      SQL_TYPE_TIME : PInt64(@Result.Year)^ := 0;
      SQL_TEXT,
      SQL_VARYING: begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result)
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stDate);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^    / IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stDate);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  P: PAnsiChar;
  Len: NativeUInt;
  I64: Int64 absolute Result;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = -4 then
                        I64 := PISC_LONG(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_LONG(sqldata)^ * IBScaleDivisor[-4-sqlscale]
                      else
                        I64 := PISC_LONG(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := Ord(PISC_BOOLEAN(sqldata)^ <> 0);
      SQL_BOOLEAN_FB: Result := Ord(PISC_BOOLEAN_FB(sqldata)^ <> 0);
      SQL_SHORT     : if sqlscale = -4 then
                        I64 := PISC_SHORT(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_SHORT(sqldata)^ * IBScaleDivisor[(-4-sqlscale)]
                      else
                        I64 := PISC_SHORT(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_INT64     : if sqlscale = -4 then
                        I64 := PISC_INT64(sqldata)^
                      else if sqlscale > -4  then
                        I64 := PISC_INT64(sqldata)^ * IBScaleDivisor[-4-sqlscale]
                      else
                        I64 := PISC_INT64(sqldata)^ div IBScaleDivisor[-4-sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stCurrency);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_FLOAT     : Result := PSingle(sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ / IBScaleDivisor[sqlscale];
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^    / IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stFloat);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TGUID</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var P: PAnsiChar;
  Len: NativeUint;
label SetFromPChar, Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      PInt64(@Result.D1)^ := 0;
      PInt64(@Result.D4)^ := 0;
    end else case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodepage = zCP_Binary)
                        then Len := CharOctedLength
                        else Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                        goto SetFromPChar;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
SetFromPChar:           if (ColumnCodepage = zCP_Binary) and (Len = SizeOf(TGUID))
                        then Move(P^, Result.D1, SizeOf(TGUID))
                        else if (ColumnCodepage <> zCP_Binary) and ((Len = 36) or (Len = 38))
                          then ValidGUIDToBinary(P, @Result.D1)
                          else goto Fail;
                      end;
      else
Fail:     raise CreateConversionError(ColumnIndex, ColumnType, stGUID);

    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPCharFromTextVar(Len);
                        Result := RawToIntDef(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stInteger);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_D_FLOAT,
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToInt64Def(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stLong);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTime</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>zero padded</code>
}
procedure TZAbstractInterbaseFirebirdResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var
  P: PAnsiChar;
  Len: NativeUInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
Fill: PCardinal(@Result.Hour)^ := 0;
      PInt64(@Result.Second)^ := 0;
    end else case sqltype of
      SQL_TIMESTAMP : begin
            isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TYPE_DATE : goto Fill;
      SQL_TYPE_TIME : begin
            isc_decode_time(PISC_TIME(sqldata)^,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TEXT, SQL_VARYING: begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stTime);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTimestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero padded</code>
  @exception SQLException if a database access error occurs
}
procedure TZAbstractInterbaseFirebirdResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Pint64(@Result.Year)^ := 0;
      PInt64(@Result.Minute)^ := 0;
      PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
    end else case sqltype of
      SQL_TIMESTAMP :
        begin
          isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
            Result.Year, Result.Month, Result.Day);
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TYPE_DATE :
        begin
          PInt64(@Result.Hour)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_date(PISC_DATE(sqldata)^,
            Result.Year, Result.Month, Result.Day);
          Result.IsNegative := False;
        end;
      SQL_TYPE_TIME :
        begin
          PInt64(@Result.Year)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIME(sqldata)^,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          P := GetPCharFromTextVar(Len);
          LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stTimeStamp);
    end;
  end;
end;

function TZAbstractInterbaseFirebirdResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  Assert((ColumnIndex >= FirstDbcIndex) and (ColumnIndex <= ColumnsInfo.Count {$IFDEF GENERIC_INDEX}-1{$ENDIF}), 'Index out of Range.');
  {$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    Result := (sqlind <> nil) and (sqlind^ = ISC_NULL)
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as          8
  a <code>TZAnsiRec</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length in bytes of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractInterbaseFirebirdResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  TempDate: TZTimeStamp;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): Pointer;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := IZResultSet(FWeakIZResultSetPtr).GetBlob( ColumnIndex);
    Result := BlobTemp.GetBuffer(fRawTemp, Len);
  end;
  label set_Results;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Len := 0;
      Result := nil;
    end else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PDouble(sqldata)^, Result);
                      end;
      SQL_LONG      : if sqlscale = 0 then begin
                        IntToRaw(PISC_LONG(sqldata)^, PAnsiChar(FByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_LONG(sqldata)^, PAnsiChar(FByteBuffer), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_FLOAT     : begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PSingle(sqldata)^, Result);
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if sqlscale = 0 then begin
                        IntToRaw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(FByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(FByteBuffer), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0 then begin
                        IntToRaw(PISC_INT64(sqldata)^, PAnsiChar(FByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_INT64(sqldata)^, PAnsiChar(FByteBuffer), @Result, Byte(-sqlscale));
set_Results:            Len := Result - PAnsiChar(FByteBuffer);
                        Result := PAnsiChar(FByteBuffer);
                      end;
      SQL_TEXT,
      SQL_VARYING   : Result := GetPCharFromTextVar(Len);
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := PAnsiChar(FByteBuffer);
                        Len := DateTimeToRaw(TempDate.Year, TempDate.Month,
                          TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := PAnsiChar(FByteBuffer);
                        Len := DateToRaw(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := PAnsiChar(FByteBuffer);
                        Len := TimeToRaw(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else Raise CreateConversionError(ColumnIndex, ColumnType, stString);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length in words of UTF16 string buffer
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  label set_Results;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex, lsmRead);
    if BlobTemp.IsClob then begin
      Result := BlobTemp.GetPWideChar(FUniTemp, Len);
    end else begin
      Result := BlobTemp.GetBuffer(FRawTemp, Len);
      FUniTemp := Ascii7ToUnicodeString(Pointer(Result), Len);
      FRawTemp := '';
      Result := Pointer(FUniTemp);
      Len := Length(FUniTemp);
    end;
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull then begin
      Len := 0;
      Result := nil;
    end else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Result := PWideChar(FByteBuffer);
                        Len := FloatToSQLUnicode(PDouble(sqldata)^, Result);
                      end;
      SQL_FLOAT     : begin
                        Result := PWideChar(FByteBuffer);
                        Len := FloatToSQLUnicode(PSingle(sqldata)^, Result);
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if sqlscale = 0 then begin
                        IntToUnicode(Integer(PISC_SHORT(sqldata)^), PWideChar(FByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(Integer(PISC_SHORT(sqldata)^), PWideChar(FByteBuffer), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_LONG      : if sqlscale = 0 then begin
                        IntToUnicode(PISC_LONG(sqldata)^, PWideChar(FByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_LONG(sqldata)^, PWideChar(FByteBuffer), @Result, Byte(-sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0 then begin
                        IntToUnicode(PISC_INT64(sqldata)^, PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_INT64(sqldata)^, PWideChar(fByteBuffer), @Result, Byte(-sqlscale));
set_Results:            Len := Result - PWideChar(FByteBuffer);
                        Result := PWideChar(FByteBuffer);
                      end;
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPCharFromTextVar(Len);
                        if ColumnCodePage = CS_BINARY
                        then fUniTemp := Ascii7ToUnicodeString(P, Len)
                        else fUniTemp := PRawToUnicode(P, Len, ColumnCodePage);
                        Len := Length(fUniTemp);
                        if Len <> 0
                        then Result := Pointer(fUniTemp)
                        else Result := PEmptyUnicodeString;
                      end;
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := PWideChar(fByteBuffer);
                        Len := DateTimeToUni(TempDate.Year,
                          TempDate.Month, TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := PWideChar(fByteBuffer);
                        Len := DateToUni(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := PWideChar(fByteBuffer);
                        Len := TimeToUni(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 100000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stUnicodeString);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>usigned 32bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractInterbaseFirebirdResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>unsigned 64bit integer</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractInterbaseFirebirdResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := 0
    else case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(sqldata)^;
      SQL_LONG      : if sqlscale = 0
                      then Result := PISC_LONG(sqldata)^
                      else Result := PISC_LONG(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_SHORT     : if sqlscale = 0
                      then Result := PISC_SHORT(sqldata)^
                      else Result := PISC_SHORT(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then Result := PISC_INT64(sqldata)^
                      else Result := PISC_INT64(sqldata)^ div IBScaleDivisor[sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToUInt64Def(P, P+Len, 0);
                      end;
      else raise CreateConversionError(ColumnIndex, ColumnType, stULong);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractInterbaseFirebirdResultSet.GetUTF8String(
  ColumnIndex: Integer): UTF8String;
var P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: UTF8String);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(zCP_UTF8, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := (sqlind <> nil) and (sqlind^ = ISC_NULL);
    if LastWasNull
    then Result := ''
    else case sqltype of
      SQL_TEXT      : begin
                        P := sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := CharOctedLength;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                          if (ColumnCodePage = zCP_UTF8)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(sqldata).str[0];
                        Len := PISC_VARYING(sqldata).strlen;
                        if (ColumnCodePage = zCP_UTF8) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, zCP_UTF8, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(Columnindex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_UTF8STRING}

{ TZAbstractFirebirdInterbasePreparedStatement }

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractFirebirdInterbasePreparedStatement.AddParamLogValue(
  ParamIndex: Integer; SQLWriter: TZSQLStringWriter;
  var Result: SQLString);
var TS: TZTimeStamp;
    T: TZTime absolute TS;
    D: TZDate absolute TS;
    P: PAnsiChar;
begin
  CheckParameterIndex(ParamIndex);
  {$R-}
  with FInParamDescripors[ParamIndex] do begin
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if (sqlind <> nil) and (sqlind^ = ISC_NULL) then
      SQLWriter.AddText('(NULL)', Result)
    else case sqltype of
      SQL_ARRAY     : SQLWriter.AddText('(ARRAY)', Result);
      SQL_D_FLOAT,
      SQL_DOUBLE    : SQLWriter.AddFloat(PDouble(sqldata)^, Result);
      SQL_FLOAT     : SQLWriter.AddFloat(PSingle(sqldata)^, Result);
      SQL_BOOLEAN   : if PISC_BOOLEAN(sqldata)^ <> 0
                      then SQLWriter.AddText('(TRUE)', Result)
                      else SQLWriter.AddText('(FALSE)', Result);
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(sqldata)^ <> 0
                      then SQLWriter.AddText('(TRUE)', Result)
                      else SQLWriter.AddText('(FALSE)', Result);
      SQL_SHORT     : if sqlscale = 0
                      then SQLWriter.AddOrd(PISC_SHORT(sqldata)^, Result)
                      else begin
                        ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), PAnsiChar(FByteBuffer), @P, Byte(-IBScaleDivisor[sqlscale]));
                        SQLWriter.{$IFDEF UNICODE}AddAscii7Text{$ELSE}AddText{$ENDIF}(PAnsiChar(FByteBuffer), P - PAnsiChar(FByteBuffer), Result);
                      end;
      SQL_LONG      : if sqlscale = 0
                      then SQLWriter.AddOrd(PISC_LONG(sqldata)^, Result)
                      else begin
                        ScaledOrdinal2Raw(PISC_LONG(sqldata)^, PAnsiChar(FByteBuffer), @P, Byte(-IBScaleDivisor[sqlscale]));
                        SQLWriter.{$IFDEF UNICODE}AddAscii7Text{$ELSE}AddText{$ENDIF}(PAnsiChar(FByteBuffer), P - PAnsiChar(FByteBuffer), Result);
                      end;
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then SQLWriter.AddOrd(PInt64(sqldata)^, Result)
                      else begin
                        ScaledOrdinal2Raw(PInt64(sqldata)^, PAnsiChar(FByteBuffer), @P, Byte(-IBScaleDivisor[sqlscale]));
                        SQLWriter.{$IFDEF UNICODE}AddAscii7Text{$ELSE}AddText{$ENDIF}(PAnsiChar(FByteBuffer), P - PAnsiChar(FByteBuffer), Result);
                      end;
      SQL_TEXT      : if codepage = zCP_Binary
                      then SQLWriter.AddHexBinary(PByte(sqldata), sqllen, False, Result)
                      {$IFDEF UNICODE} else begin
                        PRawToUnicode(PAnsiChar(sqldata), sqllen, codepage, FUniTemp);
                        SQLWriter.AddTextQuoted(FUniTemp, #39, Result);
                        FUniTemp := '';
                      end;
                      {$ELSE}
                      else SQLWriter.AddTextQuoted(PAnsiChar(sqldata), sqllen, AnsiChar(#39), Result);
                      {$ENDIF}
      SQL_VARYING   : if codepage = zCP_Binary
                      then SQLWriter.AddHexBinary(PByte(@PISC_VARYING(sqldata).str[0]), PISC_VARYING(sqldata).strlen, False, Result)
                      {$IFDEF UNICODE} else begin
                        PRawToUnicode(PAnsiChar(@PISC_VARYING(sqldata).str[0]), PISC_VARYING(sqldata).strlen, codepage, FUniTemp);
                        SQLWriter.AddTextQuoted(FUniTemp, #39, Result);
                        FUniTemp := '';
                      end;
                      {$ELSE}
                      else SQLWriter.AddTextQuoted(PAnsiChar(@PISC_VARYING(sqldata).str[0]), PISC_VARYING(sqldata).strlen, AnsiChar(#39), Result);
                      {$ENDIF}
      SQL_BLOB      : if codepage <> zCP_Binary
                      then SQLWriter.AddText('(CLOB)', Result)
                      else SQLWriter.AddText('(BLOB)', Result);
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(sqldata)^,
                          T.Hour, T.Minute, T.Second, T.Fractions);
                        T.IsNegative := False;
                        T.Fractions := T.Fractions * 100000;
                        SQLWriter.AddTime(T, ConSettings.WriteFormatSettings.TimeFormat, Result);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(sqldata)^,
                          D.Year, D.Month, D.Day);
                        D.IsNegative := False;
                        SQLWriter.AddDate(D, ConSettings.WriteFormatSettings.DateFormat, Result)
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                          TS.Hour, TS.Minute, TS.Second, TS.Fractions);
                        isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          TS.Year, TS.Month, TS.Day);
                        TS.Fractions := TS.Fractions * 100000;
                        TS.IsNegative := False;
                        SQLWriter.AddTimeStamp(TS, ConSettings.WriteFormatSettings.DateTimeFormat, Result)
                      end;
      else            SQLWriter.AddText('(UNKNOWN)', Result);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAbstractFirebirdInterbasePreparedStatement.CheckParameterIndex(var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then begin
    {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := Format(SBindVarOutOfRange, [Value]);
    raise EZSQLException.Create({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
  end;
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.ClearParameters;
var B: Boolean;
begin
  inherited ClearParameters;
  for B := False to True do
    if FBatchStmts[B].Obj <> nil then begin
      FBatchStmts[B].Obj._Release;
      FBatchStmts[B].Obj := nil;
    end;
end;

constructor TZAbstractFirebirdInterbasePreparedStatement.Create(
  const Connection: IZInterbaseFirebirdConnection; const SQL: String;
  Info: TStrings);
begin
  Self.ConSettings := Connection.GetConSettings;
  FDB_CP_ID := ConSettings.ClientCodePage.ID;
  FByteBuffer := Connection.GetByteBufferAddress;
  inherited Create(Connection, SQL, Info);
  FDialect := Connection.GetDialect;
  FCodePageArray := Connection.GetInterbaseFirebirdPlainDriver.GetCodePageArray;
  FCodePageArray[FDB_CP_ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  FOrgTypeList := TZIBFBOrgSqlTypeAndScaleList.Create;
end;

destructor TZAbstractFirebirdInterbasePreparedStatement.Destroy;
begin
  inherited;
  if FOrgTypeList <> nil then begin
    FOrgTypeList.Clear;
    FreeAndNil(FOrgTypeList);
  end;
end;

procedure BindSQLDAInParameters(BindList: TZBindList;
  Stmt: TZAbstractFirebirdInterbasePreparedStatement; ArrayOffSet, ArrayItersCount: Integer);
var
  I, J, ParamIndex: Integer;
  { array DML bindings }
  ZData: Pointer; //array entry
  ZArray: PZArray;
begin
  ParamIndex := FirstDbcIndex;
  for J := ArrayOffSet to ArrayOffSet+ArrayItersCount-1 do
    for i := 0 to BindList.Count -1 do begin
      ZArray := BindList[i].Value;
      ZData := ZArray.VArray;
      if (ZData = nil) or IsNullFromArray(ZArray, J) then
        Stmt.SetNull(ParamIndex, ZDbcIntfs.stUnknown)
      else case TZSQLType(ZArray.VArrayType) of
        stBoolean: Stmt.SetBoolean(ParamIndex, TBooleanDynArray(ZData)[J]);
        stByte: Stmt.SetSmall(ParamIndex, TByteDynArray(ZData)[J]);
        stShort: Stmt.SetSmall(ParamIndex, TShortIntDynArray(ZData)[J]);
        stWord: Stmt.SetInt(ParamIndex, TWordDynArray(ZData)[J]);
        stSmall: Stmt.SetSmall(ParamIndex, TSmallIntDynArray(ZData)[J]);
        stLongWord: Stmt.SetLong(ParamIndex, TLongWordDynArray(ZData)[J]);
        stInteger: Stmt.SetInt(ParamIndex, TIntegerDynArray(ZData)[J]);
        stLong: Stmt.SetLong(ParamIndex, TInt64DynArray(ZData)[J]);
        stULong: Stmt.SetLong(ParamIndex, TUInt64DynArray(ZData)[J]);
        stFloat: Stmt.SetFloat(ParamIndex, TSingleDynArray(ZData)[J]);
        stDouble: Stmt.SetDouble(ParamIndex, TDoubleDynArray(ZData)[J]);
        stCurrency: Stmt.SetCurrency(ParamIndex, TCurrencyDynArray(ZData)[J]);
        stBigDecimal: Stmt.SetBigDecimal(ParamIndex, TBCDDynArray(ZData)[J]);
        stGUID: Stmt.SetGUID(ParamIndex, TGUIDDynArray(ZData)[j]);
        stString, stUnicodeString:
              case ZArray.VArrayVariantType of
                vtString: Stmt.SetString(ParamIndex, TStringDynArray(ZData)[j]);
                {$IFNDEF NO_ANSISTRING}
                vtAnsiString: Stmt.SetAnsiString(ParamIndex, TAnsiStringDynArray(ZData)[j]);
                {$ENDIF}
                {$IFNDEF NO_UTF8STRING}
                vtUTF8String: Stmt.SetUTF8String(ParamIndex, TUTF8StringDynArray(ZData)[j]);
                {$ENDIF}
                vtRawByteString: Stmt.SetRawByteString(ParamIndex, TRawByteStringDynArray(ZData)[j]);
                vtUnicodeString: Stmt.SetUnicodeString(ParamIndex, TUnicodeStringDynArray(ZData)[j]);
                vtCharRec: Stmt.SetCharRec(ParamIndex, TZCharRecDynArray(ZData)[j]);
                else
                  raise Exception.Create('Unsupported String Variant');
              end;
        stBytes:      Stmt.SetBytes(ParamIndex, TBytesDynArray(ZData)[j]);
        stDate:       if ZArray.VArrayVariantType = vtDate
                      then Stmt.SetDate(ParamIndex, TZDateDynArray(ZData)[j])
                      else Stmt.SetDate(ParamIndex, TDateTimeDynArray(ZData)[j]);
        stTime:       if ZArray.VArrayVariantType = vtTime
                      then Stmt.SetTime(ParamIndex, TZTimeDynArray(ZData)[j])
                      else Stmt.SetTime(ParamIndex, TDateTimeDynArray(ZData)[j]);
        stTimestamp:  if ZArray.VArrayVariantType = vtTimeStamp
                      then Stmt.SetTimestamp(ParamIndex, TZTimeStampDynArray(ZData)[j])
                      else Stmt.SetTimestamp(ParamIndex, TDateTimeDynArray(ZData)[j]);
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream: Stmt.SetBlob(ParamIndex, TZSQLType(ZArray.VArrayType), TInterfaceDynArray(ZData)[j] as IZBlob);
        else raise EZIBConvertError.Create(SUnsupportedParameterType);
      end;
      Inc(ParamIndex);
    end;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.ExecuteBatchDml;
var ArrayOffSet: Integer;
  Succeeded: Boolean;
begin
  Connection.StartTransaction;
  ArrayOffSet := 0;
  Succeeded := False;
  try
    if (FBatchStmts[True].Obj <> nil) and (BatchDMLArrayCount >= FBatchStmts[True].PreparedRowsOfArray) then
      while (ArrayOffSet+FBatchStmts[True].PreparedRowsOfArray <= BatchDMLArrayCount) do begin
        BindSQLDAInParameters(BindList, FBatchStmts[True].Obj,
          ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
        FBatchStmts[True].Obj.ExecuteUpdatePrepared;
        Inc(ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
      end;
    if (FBatchStmts[False].Obj <> nil) and (ArrayOffSet < BatchDMLArrayCount) then begin
      BindSQLDAInParameters(BindList, FBatchStmts[False].Obj,
        ArrayOffSet, FBatchStmts[False].PreparedRowsOfArray);
      FBatchStmts[False].Obj.ExecuteUpdatePrepared;
    end;
    Succeeded := True;
  finally
    if Succeeded
    then Connection.Commit
    else Connection.Rollback;
  end;
  LastUpdateCount := BatchDMLArrayCount;
end;

{$IF not declared(PRawByteString)}
type
  PRawByteString = ^RawByteString;
{$IFEND}


const
  sRETURNING = 'RETURNING';
  EBStart = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('EXECUTE BLOCK(');
  EBBegin =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}(')AS BEGIN'+LineEnding);
  EBSuspend =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('SUSPEND;'+LineEnding); //required for RETURNING syntax
  EBEnd = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('END');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
  //cRETURNING: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (sRETURNING);

class function TZAbstractFirebirdInterbasePreparedStatement.GetBindListClass: TZBindListClass;
begin
  Result := TZInterbaseFirebirdBindList;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZAbstractFirebirdInterbasePreparedStatement.GetExecuteBlockString(
  RemainingArrayRows: Integer; XSQLDAMaxSize: Cardinal; var PreparedRowsOfArray,
  MaxRowsPerBatch: Integer; PlainDriver: TZInterbaseFirebirdPlainDriver): RawByteString;
var
  Digits: Byte;
  Row, ParamIndex, LastPos, InitialStmtLen, ParamNameLen, SingleStmtLength,
  LastStmLen, HeaderLen, FullHeaderLen, StmtLength: Cardinal;
  PStmts, PResult, P: PAnsiChar;
  TypeToken: PRawByteString;
  procedure ComposeTypeTokens;
  var TypeToken: PRawByteString;
    ParamIndex, j: Cardinal;
    SQLWriter: TZRawSQLStringWriter;
    CodePageInfo: PZCodePage;
  begin
    FTypeTokensLen := 0;
    SQLWriter := TZRawSQLStringWriter.Create(100);
    Try
      {$R-}
      for ParamIndex := 0 to Cardinal(BindList.Count-1) do with FInParamDescripors[ParamIndex] do begin
        TypeToken := @PZIB_FBBindValue(BindList[ParamIndex]).TypeToken;
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        case sqltype and not (1) of
          SQL_VARYING, SQL_TEXT:
            begin
              CodePageInfo := PlainDriver.ValidateCharEncoding(SqlSubType and 255);
              SQLWriter.AddText(' VARCHAR(', TypeToken^);
              J := sqllen - Byte(CodePageInfo.CharWidth);
              SQLWriter.AddOrd(J div Byte(CodePageInfo.CharWidth), TypeToken^);
              SQLWriter.AddText(') CHARACTER SET ', TypeToken^);
              SQLWriter.{$IFDEF UNICODE}AddAscii7UTF16Text{$ELSE}AddText{$ENDIF}(CodePageInfo.Name, TypeToken^);
              SQLWriter.AddText('=?', TypeToken^);
            end;
          SQL_DOUBLE, SQL_D_FLOAT: SQLWriter.AddText(' DOUBLE PRECISION=?', TypeToken^);
          SQL_FLOAT: SQLWriter.AddText(' FLOAT=?', TypeToken^);
          SQL_LONG:
            if sqlscale = 0
            then SQLWriter.AddText(' INTEGER=?',TypeToken^)
            else begin
              if SqlSubType = RDB_NUMBERS_NUMERIC
              then SQLWriter.AddText(' NUMERIC(9,', TypeToken^)
              else SQLWriter.AddText(' DECIMAL(9,', TypeToken^);
              SQLWriter.AddOrd(Cardinal(-sqlscale), TypeToken^);
              SQLWriter.AddText(')=?', TypeToken^);
            end;
          SQL_SHORT:
            if sqlscale = 0
            then SQLWriter.AddText(' SMALLINT=?',TypeToken^)
            else begin
              if SqlSubType = RDB_NUMBERS_NUMERIC
              then SQLWriter.AddText(' NUMERIC(4,', TypeToken^)
              else SQLWriter.AddText(' DECIMAL(4,', TypeToken^);
              SQLWriter.AddOrd(Cardinal(-sqlscale), TypeToken^);
              SQLWriter.AddText(')=?', TypeToken^);
            end;
          SQL_TIMESTAMP: SQLWriter.AddText(' TIMESTAMP=?', TypeToken^);
          SQL_BLOB: if sqlsubtype = isc_blob_text
            then SQLWriter.AddText(' BLOB SUB_TYPE TEXT=?',TypeToken^)
            else SQLWriter.AddText(' BLOB=?',TypeToken^);
          //SQL_ARRAY                      = 540;
          //SQL_QUAD                       = 550;
          SQL_TYPE_TIME: SQLWriter.AddText(' TIME=?', TypeToken^);
          SQL_TYPE_DATE: SQLWriter.AddText(' DATE=?', TypeToken^);
          SQL_INT64: // IB7
            if sqlscale = 0
            then SQLWriter.AddText(' BIGINT=?',TypeToken^)
            else begin
              if SqlSubType = RDB_NUMBERS_NUMERIC
              then SQLWriter.AddText(' NUMERIC(18,', TypeToken^)
              else SQLWriter.AddText(' DECIMAL(18,', TypeToken^);
              SQLWriter.AddOrd(Cardinal(-sqlscale), TypeToken^);
              SQLWriter.AddText(')=?', TypeToken^);
            end;
          SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
             SQLWriter.AddText(' BOOLEAN=?',TypeToken^);
          SQL_NULL{FB25}:
             SQLWriter.AddText(' CHAR(1)=?',TypeToken^);
          else raise ZDbcUtils.CreateUnsupportedParameterTypeException(ParamIndex, stUnknown);
        end;
        SQLWriter.Finalize(TypeToken^);
        FTypeTokensLen := FTypeTokensLen + Cardinal(PLengthInt(NativeUInt(TypeToken^) - StringLenOffSet)^);
      end;
    Finally
      FreeAndNil(SQLWriter);
    End;
  end;
begin
  if (BindList.Count = 0) or (RemainingArrayRows = 0) then
    raise EZSQLException.Create(SInvalidInputParameterCount);
  Result := '';
  if Pointer(PZIB_FBBindValue(BindList[0]).TypeToken) = nil then
    ComposeTypeTokens;
  {now let's calc length of stmt to know if we can bound all array data or if we need some more calls}
  StmtLength := 0;
  FullHeaderLen := 0;
  PreparedRowsOfArray := 0;
  //the question marks will be !replaced! by 'P' but we skip the QMarks while composing so let's add them
  //each param gets a row num and a param num delimited by '_' so let's add them
  //each row is terminated by a ';' and a LineBreak let'so s add it too
  InitialStmtLen := (Length(FASQL)+(BindList.Count*2))+{$IFDEF MSWINDOWS}3{#13#10;}{$ELSE}2{#10;}{$ENDIF};
  for Row := 0 to RemainingArrayRows -1 do begin
    LastStmLen := StmtLength;
    HeaderLen := FTypeTokensLen;
    Inc(HeaderLen, Cardinal(BindList.Count) * 3); //add all "P","_",";";
    //Inc(HeaderLen, Cardinal(BindList.Count)); //add all "," even if one to much
    SingleStmtLength := InitialStmtLen;
    for ParamIndex := 0 to BindList.Count-1 do begin
      //calc Parameters length: 'P123_234' -> just the numbers as described above
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      ParamNameLen := GetOrdinalDigits(ParamIndex)+GetOrdinalDigits(Row);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      {inc header}
      Inc(HeaderLen, ParamNameLen);
      {inc stmt}
      Inc(SingleStmtLength, ParamNameLen);
    end;
    if MaxRowsPerBatch = 0 then begin//calc maximum batch count if not set already
      // statment limits http://www.firebirdfaq.org/faq197/
      if Connection.GetHostVersion >= 3000000 //FBVersion => 3
      then ParamIndex := 10*1024*1024 //since FB3
      else ParamIndex := 32*1024;
      ParamIndex := ParamIndex - LBlockLen;
      If FReturningFound then
        ParamIndex := ParamIndex - 9;///Length(cRETURNING);
      MaxRowsPerBatch := Min(Integer(Cardinal(XSQLDAMaxSize) div FMemPerRow),     {memory limit of XSQLDA structs}
        Integer(ParamIndex div (HeaderLen+SingleStmtLength)))+1; {10MB limited Also with FB3};
    end;
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if (PreparedRowsOfArray = MaxRowsPerBatch-1) or
       ((FStatementType = stInsert) and (PreparedRowsOfArray > 255)) or
       ((FStatementType <> stInsert) and (PreparedRowsOfArray > 125)) then begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      Break;
    end else
      PreparedRowsOfArray := Row;
  end;

  {EH: now move our data to result ! ONE ALLOC ! of result (: }
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, LengthInt(StmtLength)+LBlockLen-1{limit the one comma which is to much});
  PResult := Pointer(Result);
  Move(PAnsiChar(EBStart)^, PResult^, 18);
  Inc(PResult,14);
  PStmts := PResult + FullHeaderLen+Length(EBBegin)-1{imit the one comma which is to much};
  for Row := 0 to PreparedRowsOfArray do begin
    LastPos := 0;
    P := Pointer(FASQL);
    {$R-}
    for ParamIndex := 0 to BindList.Count-1 do with FInParamDescripors[ParamIndex] do begin
      TypeToken := @PZIB_FBBindValue(BindList[ParamIndex]).TypeToken;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      // we already marked the positions if the QuestionMarks
      ParamNameLen := QMarkPosition - LastPos;
      Move(P^, PStmts^, ParamNameLen);
      Inc(P, ParamNameLen+1);
      Inc(PStmts, ParamNameLen);
      LastPos := QMarkPosition+1;
      PByte(PStmts)^ := Byte(':');
      PByte(PStmts+1)^ := Byte('P');
      Inc(PStmts, 2);
      if (ParamIndex <> 0) or (Row <> 0) then begin
        PByte(PResult)^ := Byte(',');
        Inc(PResult);
      end;
      PByte(PResult)^ := Byte('P');
      Inc(PResult);
      Digits := ZFastCode.GetOrdinalDigits(Cardinal(ParamIndex));
      IntToRaw(Cardinal(ParamIndex), PResult, Digits);
      Inc(PResult, Digits);
      IntToRaw(Cardinal(ParamIndex), PStmts, Digits);
      Inc(PStmts, Digits);
      PByte(PResult)^ := Byte('_');
      Inc(PResult);
      PByte(PStmts)^ := Byte('_');
      Inc(PStmts);

      Digits := ZFastCode.GetOrdinalDigits(Cardinal(Row));
      IntToRaw(Cardinal(Row), PResult, Digits);
      Inc(PResult, Digits);
      IntToRaw(Cardinal(Row), PStmts, Digits);
      Inc(PStmts, Digits);
      LastStmLen := Cardinal(PLengthInt(NativeUInt(TypeToken^) - StringLenOffSet)^);
      Move(Pointer(TypeToken^)^, PResult^, LastStmLen);
      Inc(PResult, LastStmLen);
    end;
    InitialStmtLen := Cardinal(PLengthInt(NativeUInt(fASQL) - StringLenOffSet)^) - LastPos;
    Move(P^, PStmts^, Integer(InitialStmtLen));
    Inc(PStmts, InitialStmtLen);
    PByte(PStmts)^ := Byte(';');
    {$IFDEF MSWINDOWS}
    PAnsiChar(PStmts+1)^ := AnsiChar(#13);
    Inc(PStmts);
    {$ENDIF}
    PByte(PStmts+1)^ := Byte(#10);
    Inc(PStmts, 2);
  end;
  Move(PAnsiChar(EBBegin)^, PResult^, {$IFDEF MSWINDOWS}11{$ELSE}10{$ENDIF});
  if FReturningFound then begin
    Move(PAnsiChar(EBSuspend)^, PStmts^, 8);
    Inc(PStmts, 8);
  end;
  //Move(PAnsiChar(EBEnd)^, PStmts^, 3);  //the fpc can't compile the code ??
  PByte(PStmts)^ := Byte('E');
  PByte(PStmts+1)^ := Byte('N');
  PByte(PStmts+2)^ := Byte('D');
  Inc(PreparedRowsOfArray);
end;
{$IFDEF FPC}{$POP}{$ENDIF}

function TZAbstractFirebirdInterbasePreparedStatement.GetRawEncodedSQL(
  const SQL: SQLString): RawByteString;
begin
  {$IFDEF UNICODE}
  if (FDB_CP_ID = CS_NONE)
  then Result := SplittQuery(SQL)
  else Result := ZUnicodeToRaw(SQL, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
  Result := SQL;
  {$ENDIF}
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.InternalBindDouble(Index: Cardinal;
  const Value: Double);
var TimeStamp: TZTimeStamp;
begin
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := Round(Value)
                      else PISC_LONG(sqldata)^ := Round(Value*IBScaleDivisor[sqlscale]);
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value <> 0);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value <> 0);
      SQL_SHORT     : if sqlscale = 0
                      then PISC_SHORT(sqldata)^ := Round(Value)
                      else PISC_SHORT(sqldata)^ := Round(Value*IBScaleDivisor[sqlscale]);
      SQL_INT64,
      SQL_QUAD      : if sqlscale = 0
                      then PISC_INT64(sqldata)^ := Round(Value)
                      else PISC_INT64(sqldata)^ := Round(Value*IBScaleDivisor[sqlscale]);
      SQL_TYPE_DATE : begin
                        DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                        isc_encode_date(PISC_DATE(sqldata)^, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      end;
      SQL_TYPE_TIME : begin
                        DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                        isc_encode_time(PISC_TIME(sqldata)^, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                      end;
      SQL_TIMESTAMP : begin
                        DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                        DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                        TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                        isc_encode_time(PISC_TIMESTAMP(sqldata).timestamp_time, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                      end;
      else raise CreateConversionError(Index, stDouble);
    end;
  end;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
begin
  if ParamType = pctResultSet then
    Raise EZUnsupportedException.Create(SUnsupportedOperation);
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name,
    PrecisionOrSize, Scale);;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var B: Boolean;
begin
  inherited;
  for B := False to True do
    if Assigned(FBatchStmts[b].Obj) then begin
      FBatchStmts[b].Obj.ReleaseImmediat(Sender, AError);
      FBatchStmts[b].Obj._Release;
      FBatchStmts[b].Obj := nil;
    end;
end;

{**
  Sets the designated parameter to a <code>AnsiString</code> value.
  The parameter must have operating-system encoding. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetAnsiString(Index: Integer;
  const Value: AnsiString);
var Len: LengthInt;
    P: PAnsiChar;
label jmpWriteLob;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  if Value <> '' then with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    Len := Length(Value);
    P := Pointer(Value);
    case sqltype of
      SQL_TEXT,
      SQL_VARYING   : begin
                        if (codepage <> zOSCodePage) and (codepage <> zCP_Binary) then begin
                          PRawToRawConvert(P, Len, zOSCodePage, codepage, FRawTemp);
                          Len := Length(FRawTemp);
                          P := Pointer(FRawTemp);
                        end;
                        if LengthInt(sqllen) < Len then
                          Len := LengthInt(sqllen);
                        Move(P^, PISC_VARYING(sqldata).str[0], Len);
                        PISC_VARYING(sqldata).strlen := Len;
                        sqlind^ := ISC_NOTNULL;
                      end;
      SQL_BLOB      : if (codepage = zOSCodePage) or (codepage = zCP_Binary)
                      then goto jmpWriteLob
                      else begin
                        PRawToRawConvert(P, Len, zOSCodePage, codepage, FRawTemp);
                        Len := Length(FRawTemp);
                        if Len = 0
                        then P := PEmptyAnsiString
                        else P := Pointer(FRawTemp);
jmpWriteLob:            WriteLobBuffer(Index, P, Len);
                      end;
      else SetPAnsiChar(Index, P, Len);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF NO_ANSISTRING}

procedure TZAbstractFirebirdInterbasePreparedStatement.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_LONG      : BCD2ScaledOrdinal(Value, sqldata, SizeOf(ISC_LONG), -sqlscale);
      SQL_SHORT     : BCD2ScaledOrdinal(Value, sqldata, SizeOf(ISC_SHORT), -sqlscale);
      SQL_INT64,
      SQL_QUAD      : BCD2ScaledOrdinal(Value, sqldata, SizeOf(ISC_INT64), -sqlscale);
      SQL_TEXT,
      SQL_VARYING   : begin
                        L := BcdToRaw(Value, PAnsiChar(FByteBuffer), '.');
                        if sqllen < L then
                          L := sqllen;
                        Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                      end;
      else InternalBindDouble(Index, BCDToDouble(Value));
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.SetBlob(Index: Integer;
  ASQLType: TZSQLType; const Value: IZBlob);
var P: PAnsiChar;
  L: NativeUInt;
  IBLob: IZInterbaseFirebirdLob;
label jmpNotNull;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    BindList.Put(Index, ASQLType, Value); //localize for the refcount
    if (Value = nil) or Value.IsEmpty then begin
      BindList.SetNull(Index, ASQLType);
      P := nil;
      L := 0;//satisfy compiler
    end else if Supports(Value, IZInterbaseFirebirdLob, IBLob) and ((sqltype = SQL_QUAD) or (sqltype = SQL_BLOB)) then begin
      //sqldata := GetMemory(SizeOf(TISC_QUAD)); EH@Jan what's that? we have one big buffer as described by FB/IB.
	  //Jan@EH: See commit message for Rev. 6595 + Ticket 429
      PISC_QUAD(sqldata)^ := IBLob.GetBlobId;
      goto jmpNotNull;
    end else if (Value <> nil) and (codepage <> zCP_Binary) then
      if Value.IsClob then begin
        Value.SetCodePageTo(codepage);
        P := Value.GetPAnsiChar(codepage, FRawTemp, L)
      end else raise CreateConversionError(Index, stBinaryStream)
    else P := Value.GetBuffer(FRawTemp, L);
    if P <> nil then begin
      case sqltype of
        SQL_TEXT,
        SQL_VARYING   : begin
                          if sqllen < L then
                            L := sqllen;
                          Move(P^, PISC_VARYING(sqldata).str[0], L);
                          PISC_VARYING(sqldata).strlen := L;
                        end;
        SQL_BLOB,
        SQL_QUAD      : WriteLobBuffer(Index, P, L);
        else raise CreateConversionError(Index, ASQLType);
      end;
  jmpNotNull:
      sqlind^ := ISC_NOTNULL;
    end else
      sqlind^ := SQL_NULL;
  end;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Ord(Value);
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Ord(Value);
      SQL_LONG      : if Value
                      then PISC_LONG(sqldata)^ := IBScaleDivisor[sqlscale]
                      else PISC_LONG(sqldata)^ := 0;
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value);
      SQL_SHORT     : if Value
                      then PISC_SHORT(sqldata)^ := IBScaleDivisor[sqlscale]
                      else PISC_SHORT(sqldata)^ := 0;
      SQL_INT64,
      SQL_QUAD      : if Value
                      then PISC_INT64(sqldata)^ := IBScaleDivisor[sqlscale]
                      else PISC_INT64(sqldata)^ := 0;
      SQL_VARYING   : begin
                        PISC_VARYING(sqldata).strlen := 1;
                        Byte(PISC_VARYING(sqldata).str) := Byte('0')+Ord(Value);
                      end;
      else raise CreateConversionError(Index, stBoolean);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.SetByte(Index: Integer; Value: Byte);
begin
  SetSmall(Index, Value);
end;

{**
  Sets the designated parameter to an array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
var Len: NativeUInt;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Len := Length(Value);
  if Len = 0
  then P := PEmptyAnsiString
  else P := Pointer(Value);
  SetPAnsiChar(Index, P, Len);
end;

{**
  Sets the designated parameter to an array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetBytes(Index: Integer;
  Value: PByte; Len: NativeUInt);
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  if Value = nil then
    Value := Pointer(PEmptyAnsiString);
  SetPAnsiChar(Index, PAnsiChar(Value), Len);
end;

{**
  Sets the designated parameter to a <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetCharRec(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZCharRec);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if Value.CP = zCP_UTF16 then
      SetPWideChar(Index, Value.P, Value.Len)
    else if (codepage = Value.CP) or (codepage = zCP_Binary)
      then SetPAnsiChar(Index, Value.P, Value.Len)
      else begin
        FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP); //localize it
        if FUniTemp <> ''
        then SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp))
        else SetPWideChar(Index, PEmptyUnicodeString, 0);
      end;
  end;
end;

{**
  Sets the designated parameter to a <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var L: Integer;
  i64: Int64 absolute Value;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_LONG      : if sqlscale = -4 then  //sqlscale fits!
                        PISC_LONG(sqldata)^ := I64
                      else if sqlscale > -4 then //EH: check the modulo?
                        PISC_LONG(sqldata)^ := I64 div IBScaleDivisor[-4-sqlscale] //dec sqlscale digits
                      else
                        PISC_LONG(sqldata)^ := I64 * IBScaleDivisor[4+sqlscale]; //inc sqlscale digits
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value <> 0);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value <> 0);
      SQL_SHORT     : if sqlscale = -4 then  //sqlscale fits!
                        PISC_SHORT(sqldata)^ := I64
                      else if sqlscale > -4 then //EH: check the modulo?
                        PISC_SHORT(sqldata)^ := I64 div IBScaleDivisor[-4-sqlscale] //dec sqlscale digits
                      else
                        PISC_SHORT(sqldata)^ := I64 * IBScaleDivisor[4+sqlscale]; //inc sqlscale digits
      SQL_INT64,
      SQL_QUAD      : if sqlscale = -4 then //sqlscale fits!
                        PISC_INT64(sqldata)^ := I64
                      else if sqlscale > -4 then //EH: check the modulo?
                        PISC_INT64(sqldata)^ := I64 div IBScaleDivisor[-4-sqlscale]//dec sqlscale digits
                      else
                        PISC_INT64(sqldata)^ := I64 * IBScaleDivisor[4+sqlscale]; //inc sqlscale digits
      SQL_TEXT,
      SQL_VARYING   : begin
                        CurrToRaw(Value, '.', PAnsiChar(FByteBuffer), @P);
                        L := P - PAnsiChar(FByteBuffer);
                        if LengthInt(sqllen) < L then
                          L := LengthInt(sqllen);
                        Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                      end;
      else raise CreateConversionError(Index, stCurrency);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Sets the designated parameter to a <code<TZDate</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_VARYING   : begin
                        L := DateToRaw(Value.Year, Value.Month, Value.Day, PAnsiChar(FByteBuffer),
                            ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
                        if LengthInt(sqllen) < L then
                          L := LengthInt(sqllen);
                        Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                      end;
      SQL_TYPE_TIME : PISC_TIME(sqldata)^ := 0;
      SQL_TYPE_DATE : isc_encode_date(PISC_DATE(sqldata)^, Value.Year, Value.Month, Value.Day);
      SQL_TIMESTAMP : begin
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date, Value.Year, Value.Month, Value.Day);
                        PISC_TIMESTAMP(sqldata).timestamp_time := 0;
                      end;
      else begin
        ZSysUtils.TryDateToDateTime(Value, DT);
        InternalBindDouble(Index, DT);
      end;
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_TEXT,
      SQL_VARYING   : begin
          L := FloatToSqlRaw(Value, PAnsiChar(FByteBuffer));
          if LengthInt(sqllen) < L then
            L := LengthInt(sqllen);
          Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
          PISC_VARYING(sqldata).strlen := L;
        end;
      else InternalBindDouble(Index, Value);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Sets the designated parameter to a <code>single</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetFloat(Index: Integer; Value: Single);
var L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_TEXT,
      SQL_VARYING   : begin
          L := FloatToSqlRaw(Value, PAnsiChar(FByteBuffer));
          if LengthInt(sqllen) < L then
            L := LengthInt(sqllen);
          Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
          PISC_VARYING(sqldata).strlen := L;
        end;
      else InternalBindDouble(Index, Value);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetGUID(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID);
var P: PAnsiChar;
  L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_VARYING   : begin
                        if codepage = zCP_Binary then begin
                          P := @Value.D1;
                          L := SizeOf(TGUID);
                        end else begin
                          //see https://firebirdsql.org/refdocs/langrefupd25-intfunc-uuid_to_char.html
                          P := PAnsiChar(FByteBuffer);
                          GUIDToBuffer(@Value.D1, P, []);
                          L := 36;
                        end;
                        if sqllen < L then
                          L := sqllen;
                        Move(P^, PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                        sqlind^ := ISC_NOTNULL;
                      end;
      SQL_BLOB,
      SQL_QUAD      : if codepage = zCP_Binary then
                        WriteLobBuffer(Index, @Value.D1, SizeOf(TGUID))
                      else begin
                        P := PAnsiChar(FByteBuffer);
                        GUIDToBuffer(@Value.D1, P, []);
                        WriteLobBuffer(Index, P, 36)
                      end;
      else raise CreateConversionError(Index, stGUID);
    end;
  end;
end;

{**
  Sets the designated parameter to a <code>signed 32bit integer</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetInt(Index, Value: Integer);
var P: PAnsiChar;
    Digits: Byte;
    IsNegative: Boolean;
    C: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := Value
                      else PISC_LONG(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value <> 0);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value <> 0);
      SQL_SHORT     : if sqlscale = 0
                      then PISC_SHORT(sqldata)^ := Value
                      else PISC_SHORT(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then PISC_INT64(sqldata)^ := Value
                      else PISC_INT64(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_TEXT,
      SQL_VARYING   : begin
                        Digits := GetOrdinalDigits(Value, C, IsNegative);
                        if (Digits+Byte(IsNegative)) > sqllen then begin
                          PISC_VARYING(sqldata).strlen := sqllen;
                          Digits := Byte(sqllen - Byte(IsNegative));
                        end;
                        P := @PISC_VARYING(sqldata).str[0];
                        if IsNegative then begin
                          PByte(P)^ := Byte('-');
                          Inc(P);
                        end;
                        IntToRaw(C, P, Digits);
                      end;
      else raise CreateConversionError(Index, stInteger);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Sets the designated parameter to a <code>signed 64Bit integer</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "GetOrdinalDigits" marked as inline is not inlined}{$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
var U: UInt64;
  P: PAnsiChar;
  Digits: Byte;
  IsNegative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := Value
                      else PISC_LONG(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value <> 0);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value <> 0);
      SQL_SHORT     : if sqlscale = 0
                      then PISC_SHORT(sqldata)^ := Value
                      else PISC_SHORT(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then PISC_INT64(sqldata)^ := Value
                      else PISC_INT64(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_TEXT,
      SQL_VARYING   : begin
                        Digits := GetOrdinalDigits(Value, U, IsNegative);
                        if (Digits+Byte(IsNegative)) > sqllen then begin
                          PISC_VARYING(sqldata).strlen := sqllen;
                          Digits := Byte(sqllen - Byte(IsNegative));
                        end;
                        P := @PISC_VARYING(sqldata).str[0];
                        if IsNegative then begin
                          PByte(P)^ := Byte('-');
                          Inc(P);
                        end;
                        IntToRaw(U, P, Digits);
                      end;
      else raise CreateConversionError(Index, stLong);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "SQLType" not used} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  FInParamDescripors[Index].sqlind^ := ISC_NULL;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
   Set up parameter PAnsiChar value
   @param Index the target parameter index
   @param Value the source value
   @param Len the length in bytes of the source value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetPAnsiChar(Index: Cardinal;
  Value: PAnsiChar; Len: LengthInt);
var TS: TZTimeStamp;
    D: TZDate absolute TS;
    T: TZTime absolute TS;
Label Fail;
begin
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_VARYING   : begin
                        if LengthInt(sqllen) < Len then
                          Len := LengthInt(sqllen);
                        Move(Value^, PISC_VARYING(sqldata).str[0], Len);
                        PISC_VARYING(sqldata).strlen := Len;
                      end;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG (sqldata)^ := RawToIntDef(Value, Value+Len, 0)
                      else PISC_LONG (sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0));
      SQL_SHORT     : if sqlscale = 0
                      then PISC_LONG (sqldata)^ := RawToIntDef(Value, Value+Len, 0)
                      else PISC_SHORT (sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0));
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
      SQL_D_FLOAT,
      SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(sqldata)^, Len);
      SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (sqldata)^, Len);
      SQL_INT64     : if sqlscale = 0
                      then PISC_LONG (sqldata)^ := RawToInt64Def(Value, Value+Len, 0)
                      else PISC_INT64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0)); //AVZ - INT64 value was not recognized
      SQL_BLOB, SQL_QUAD: WriteLobBuffer(Index, Value, Len);
      SQL_TYPE_DATE : if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D)
                      then isc_encode_date(PISC_DATE(sqldata)^, D.Year, D.Month, D.Day)
                      else goto Fail;
      SQL_TYPE_TIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T)
                      then isc_encode_time(PISC_TIME(sqldata)^, T.Hour, T.Minute, T.Second, T.Fractions div 100000)
                      else goto Fail;
      SQL_TIMESTAMP:  if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date, TS.Year, TS.Month, TS.Day);
                        isc_encode_time(PISC_TIMESTAMP(sqldata).timestamp_time, TS.Hour, TS.Minute, TS.Second, TS.Fractions div 100000);
                      end else goto Fail;
      else
  Fail: raise CreateConversionError(Index, stString);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
   Set up parameter PWideChar value
   @param Index the target parameter index
   @param Value the source value
   @param Len the length in words of the source value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetPWideChar(Index: Cardinal;
  Value: PWideChar; Len: LengthInt);
var TS: TZTimeStamp;
    D: TZDate absolute TS;
    T: TZTime absolute TS;
    P: Pointer;
Label Fail;
begin
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_TEXT,
      SQL_VARYING   : begin
                        if codepage <> zCP_Binary
                        then FRawTemp := PUnicodeToRaw(Value, Len, codepage)
                        else FRawTemp := UnicodeStringToAscii7(Value, Len);
                        Len := Length(FRawTemp);
                        if LengthInt(sqllen) < Len then
                          Len := LengthInt(sqllen);
                        if Len > 0 then
                          Move(Pointer(FRawTemp)^, PISC_VARYING(sqldata).str[0], Len);
                        PISC_VARYING(sqldata).strlen := Len;
                      end;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := UnicodeToIntDef(Value, Value+Len, 0)
                      else PISC_LONG(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0));
      SQL_SHORT     : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := UnicodeToIntDef(Value, Value+Len, 0)
                      else PISC_SHORT(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0));
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(StrToBoolEx(Value, Value+Len));
      SQL_D_FLOAT,
      SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(sqldata)^, Len);
      SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (sqldata)^, Len);
      SQL_INT64     : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := UnicodeToInt64Def(Value, Value+Len, 0)
                      else PISC_INT64(sqldata)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[sqlscale], 0)); //AVZ - INT64 value was not recognized
      SQL_BLOB,
      SQL_QUAD      : begin
                        if Codepage <> zCP_Binary
                        then FRawTemp := PUnicodeToRaw(Value, Len, codepage)
                        else FRawTemp := UnicodeStringToAscii7(Value, Len);
                        if FRawTemp <> ''
                        then P := Pointer(FRawTemp)
                        else P := PEmptyAnsiString;
                        Len := Length(FRawTemp);
                        WriteLobBuffer(Index, P, Len)
                      end;
      SQL_TYPE_DATE : if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D)
                      then isc_encode_date(PISC_DATE(sqldata)^, D.Year, D.Month, D.Day)
                      else goto Fail;
      SQL_TYPE_TIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T)
                      then isc_encode_time(PISC_TIME(sqldata)^, T.Hour, T.Minute, T.Second, T.Fractions div 100000)
                      else goto Fail;
      SQL_TIMESTAMP:  if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date, TS.Year, TS.Month, TS.Day);
                        isc_encode_time(PISC_TIMESTAMP(sqldata).timestamp_time, TS.Hour, TS.Minute, TS.Second, TS.Fractions div 100000);
                      end else goto Fail;
      else
  Fail:   raise CreateConversionError(Index, stUnicodeString);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to <code>raw database encoded string</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetRawByteString(Index: Integer;
  const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPAnsiChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.SetShort(Index: Integer; Value: ShortInt);
begin
  SetSmall(Index, Value);
end;

procedure TZAbstractFirebirdInterbasePreparedStatement.SetSmall(Index: Integer; Value: SmallInt);
var P: PAnsiChar;
    W: Word;
    Digits: Byte;
    IsNegative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_FLOAT     : PSingle(sqldata)^   := Value;
      SQL_D_FLOAT,
      SQL_DOUBLE    : PDouble(sqldata)^   := Value;
      SQL_LONG      : if sqlscale = 0
                      then PISC_LONG(sqldata)^ := Value
                      else PISC_LONG(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_BOOLEAN   : PISC_BOOLEAN(sqldata)^ := Ord(Value <> 0);
      SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(sqldata)^ := Ord(Value <> 0);
      SQL_SHORT     : if sqlscale = 0
                      then PISC_SHORT(sqldata)^ := Value
                      else PISC_SHORT(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_QUAD,
      SQL_INT64     : if sqlscale = 0
                      then PISC_INT64(sqldata)^ := Value
                      else PISC_INT64(sqldata)^ := Value*IBScaleDivisor[sqlscale];
      SQL_TEXT,
      SQL_VARYING   : begin
                        Digits := GetOrdinalDigits(Value, W, IsNegative);
                        if (Digits+Byte(IsNegative)) > sqllen then begin
                          PISC_VARYING(sqldata).strlen := sqllen;
                          Digits := Byte(sqllen - Byte(IsNegative));
                        end;
                        P := @PISC_VARYING(sqldata).str[0];
                        if IsNegative then begin
                          PByte(P)^ := Byte('-');
                          Inc(P);
                        end;
                        IntToRaw(W, P, Digits);
                      end;
      else raise CreateConversionError(Index, stSmall);
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;

{**
  Sets the designated parameter to a <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetString(Index: Integer;
  const Value: String);
{$IFDEF UNICODE}
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPWideChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0);
{$ELSE}
var L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  if Value <> '' then with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    L := Length(Value);
    case sqltype of
      SQL_VARYING   : begin
                        if L > LengthInt(sqllen) then
                          L := LengthInt(sqllen);
                        Move(Pointer(Value)^, PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                        sqlind^ := ISC_NOTNULL;
                      end;
      SQL_BLOB      : WriteLobBuffer(Index, Pointer(Value), L);
      else SetPAnsiChar(Index, Pointer(Value), L);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>TZTime</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_TEXT,
      SQL_VARYING   : begin
                        L := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                            PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, False, False);
                        if LengthInt(sqllen) < L then
                          L := LengthInt(sqllen);
                        Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                      end;
      SQL_TYPE_DATE : isc_encode_date(PISC_DATE(sqldata)^, 1899, 12, 31);
      SQL_TYPE_TIME : isc_encode_time(PISC_TIME(sqldata)^, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
      SQL_TIMESTAMP : begin
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                          cPascalIntegralDatePart.Year, cPascalIntegralDatePart.Month, cPascalIntegralDatePart.Day);
                        isc_encode_time(PISC_TIMESTAMP(sqldata).timestamp_time, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
                      end;
      else            begin
                        ZSysUtils.TryTimeToDateTime(Value, DT);
                        InternalBindDouble(Index, DT);
                      end;
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>TZTimestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case sqltype of
      SQL_TEXT,
      SQL_VARYING   : begin
                        L := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
                            Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                            PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat,
                            False, Value.IsNegative);
                        if LengthInt(sqllen) < L then
                          L := LengthInt(sqllen);
                        Move(FByteBuffer[0], PISC_VARYING(sqldata).str[0], L);
                        PISC_VARYING(sqldata).strlen := L;
                      end;
      SQL_TYPE_DATE : isc_encode_date(PISC_DATE(sqldata)^, Value.Year, Value.Month, Value.Day);
      SQL_TYPE_TIME : isc_encode_time(PISC_TIME(sqldata)^, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
      SQL_TIMESTAMP : begin
                        isc_encode_date(PISC_TIMESTAMP(sqldata).timestamp_date, Value.Year, Value.Month, Value.Day);
                        isc_encode_time(PISC_TIMESTAMP(sqldata).timestamp_time, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
                      end;
      else begin
        ZSysUtils.TryTimeStampToDateTime(Value, DT);
        InternalBindDouble(Index, DT);
      end;
    end;
    sqlind^ := ISC_NOTNULL;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>usigned 32bit integer</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetUInt(Index: Integer; Value: Cardinal);
begin
  SetLong(Index, Value);
end;

{**
  Sets the designated parameter to a <code>unsigned 64Bit integer</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
begin
  SetLong(Index, Value);
end;

{**
  Sets the designated parameter to a <code>UnicodeString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetUnicodeString(Index: Integer;
  const Value: UnicodeString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPWideChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0);
end;

{**
  Sets the designated parameter to a <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractFirebirdInterbasePreparedStatement.SetUTF8String(Index: Integer;
  const Value: UTF8String);
var Len: LengthInt;
    P: PAnsiChar;
label jmpWriteLob;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  if Value <> '' then with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    Len := Length(Value);
    P := Pointer(Value);
    case sqltype of
      SQL_TEXT,
      SQL_VARYING   : begin
                        if (codepage <> zCP_UTF8) and (codepage <> zCP_Binary) then begin
                          PRawToRawConvert(P, Len, zCP_UTF8, codepage, FRawTemp);
                          Len := Length(FRawTemp);
                          P := Pointer(FRawTemp);
                        end;
                        if LengthInt(sqllen) < Len then
                          Len := LengthInt(sqllen);
                        Move(P^, PISC_VARYING(sqldata).str[0], Len);
                        PISC_VARYING(sqldata).strlen := Len;
                        sqlind^ := ISC_NOTNULL;
                      end;
      SQL_BLOB      : if (codepage = zCP_UTF8) or (codepage = zCP_Binary)
                      then goto jmpWriteLob
                      else begin
                        PRawToRawConvert(P, Len, zCP_UTF8, codepage, FRawTemp);
                        Len := Length(FRawTemp);
                        if Len = 0
                        then P := PEmptyAnsiString
                        else P := Pointer(FRawTemp);
jmpWriteLob:            WriteLobBuffer(Index, P, Len);
                      end;
      else SetPAnsiChar(Index, P, Len);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF NO_UTF8STRING}

procedure TZAbstractFirebirdInterbasePreparedStatement.SetWord(Index: Integer; Value: Word);
begin
  SetInt(Index, Value);
end;

function TZAbstractFirebirdInterbasePreparedStatement.SplittQuery(
  const SQL: SQLString): RawByteString;
var
  I, InParamCount: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  DoRealloc: Boolean;
  Tokenizer: IZTokenizer;
  {$IFDEF UNICODE}
  L: Cardinal;
  FirstComposeToken: PZToken;
  ResultWriter: TZRawSQLStringWriter;
  {$ELSE}
  P: PAnsiChar;
  {$ENDIF}
begin
  Result := '';
  if SQL = '' then Exit;
  Tokenizer := Connection.GetTokenizer;
  Tokens := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
  {$IFDEF UNICODE}
  ResultWriter := TZRawSQLStringWriter.Create(Length(SQL) shl 1);
  {$ELSE}
  P := Pointer(SQL);
  Result := SQL;
  {$ENDIF}
  InParamCount := 0;
  DoRealloc := FInParamDescripors = nil;
  try
    Token := nil;
    {$IFDEF UNICODE}
    FirstComposeToken := Tokens[0];
    {$ENDIF}
    for I := 0 to Tokens.Count -1 do begin
      Token := Tokens[I];
      if (Token.L = 1) and (Token.P^ = Char('?')) then begin
        if DoRealloc then
          ReallocMem(FInParamDescripors, SizeOf(TZInterbaseFirerbirdParam)*(InParamCount+1));
        {$IFDEF UNICODE}
        if (FirstComposeToken <> nil) then begin
          Token := Tokens[I-1];
          L := (Token.P-FirstComposeToken.P)+ Token.L;
          if (L = 1) and (Ord(FirstComposeToken.P^) <= 127) //micro optimization if previous token is just a ',' f.e.
          then ResultWriter.AddChar(AnsiChar(FirstComposeToken.P^), Result)
          else begin
            PUnicodeToRaw(FirstComposeToken.P, L, FClientCP, FRawTemp);
            ResultWriter.AddText(FRawTemp, Result);
          end;
          if I < Tokens.Count-1
          then FirstComposeToken := Tokens[I+1]
          else FirstComposeToken := nil;
        end;
        {$R-}FInParamDescripors[InParamCount].QMarkPosition := ResultWriter.GetCurrentLength(Result);{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        ResultWriter.AddChar(AnsiChar('?'), Result);
        {$ELSE}
        {$R-}FInParamDescripors[InParamCount].QMarkPosition := Token.P-P;{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        {$ENDIF}
        Inc(InParamCount);
      end else if (Token.TokenType = ttWord) and (Token.L = 9) and SameText(Token.P, PChar(sRETURNING), 9) then
        FReturningFound := True
      {$IFDEF UNICODE}
      else if (FDB_CP_ID = CS_NONE) and (//all identifiers collate unicode_fss if CS_NONE
               (Token.TokenType = ttQuotedIdentifier) or
               ((Token.TokenType = ttWord) and (Token.L > 1) and (Token.P^ = '"'))) then begin
        if (FirstComposeToken <> nil) then begin
          PUnicodeToRaw(FirstComposeToken.P, (Tokens[I-1].P-FirstComposeToken.P)+ Tokens[I-1].L, FClientCP, FRawTemp);
          ResultWriter.AddText(FRawTemp, Result);
        end;
        PUnicodeToRaw(Token.P, Token.L, zCP_UTF8, FRawTemp);
        ResultWriter.AddText(FRawTemp, Result);
        if I < Tokens.Count-1
        then FirstComposeToken := Tokens[I+1]
        else FirstComposeToken := nil;
      end;
      {$ENDIF};
    end;
   {$IFDEF UNICODE}
    if (FirstComposeToken <> nil) then begin
      PUnicodeToRaw(FirstComposeToken.P, (Token.P-FirstComposeToken.P)+Token.L, FClientCP, FRawTemp);
      ResultWriter.AddText(FRawTemp, Result);
    end;
    ResultWriter.Finalize(Result);
    {$ENDIF}
  finally
    Tokens.Free;
    {$IFDEF UNICODE}
    FreeAndNil(ResultWriter);
    FRawTemp := '';
    {$ENDIF}
    Tokenizer := nil;
  end;
  SetBindCapacity(InParamCount);
  FQuerySplitted := True;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractFirebirdInterbasePreparedStatement.Unprepare;
var B: Boolean;
begin
  inherited Unprepare;
  FQuerySplitted := False;
  FReturningFound := False;
  for B := False to True do
    if FBatchStmts[B].Obj <> nil then begin
      FBatchStmts[B].Obj._Release;
      FBatchStmts[B].Obj := nil;
    end;
  if FOutData <> nil then begin
    FreeMem(FOutData);
    FOutData := nil;
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractFirebirdInterbasePreparedStatement.UnPrepareInParameters;
begin
  SetBindCapacity(0);
  if FInParamDescripors <> nil then begin
    FreeMem(FInParamDescripors);
    FInParamDescripors := nil;
  end;
  if FInData <> nil then begin
    FreeMem(FInData);
    FInData := nil;
  end;
end;


{ TZInterbaseFirebirdAbstractGUIDProps }

destructor TZInterbaseFirebirdAbstractGUIDProps.Destroy;
begin
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  inherited;
end;

procedure TZInterbaseFirebirdAbstractGUIDProps.InternalInit(const OptionByType, OptionDomains, OptionFields: string);
begin
  // Cleanup
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  FDetectFlags := [];

  if StrToBoolEx(OptionByType) then
    Include(FDetectFlags, gfByType);
  if OptionDomains <> '' then
  begin
    Include(FDetectFlags, gfByDomain);
    FDomains := ExtractFields(OptionDomains, [';', ',']);
  end;
  if OptionFields <> '' then
  begin
    Include(FDetectFlags, gfByFieldName);
    FFields := ExtractFields(OptionFields, [';', ',']);
  end;
end;

{**
  Determines if a column could have GUID / UUID type (SQLType = stBytes and DataSize = 16)
  @param  SQL column type
  @param  length of column data
  @return True if domain could have GUID type
}
function TZInterbaseFirebirdAbstractGUIDProps.ColumnCouldBeGUID(SQLType: TZSQLType; DataSize: Integer): Boolean;
begin
  Result := (SQLType = stBytes) and (DataSize = 16);
end;

{**
  Determines if a column has GUID / UUID type.
  @param  SQL column type             (used if GUID is determined by type)
  @param  length of column data       (used if GUID is determined by type)
  @param  domain name                 (used if GUID is determined by domain name)
  @param  column name                 (used if GUID is determined by field name)
  @return True if column must have GUID type according to connection properties.
}
function TZInterbaseFirebirdAbstractGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
begin
  if ColumnCouldBeGUID(SQLType, DataSize) then
  begin
    // Perform checking by descending importance, the first positive result breaks the chain
    if (gfByFieldName in FDetectFlags) and (ColumnName <> '') then
    begin
      Result := (FFields.IndexOf(ColumnName) <> -1);
      if Result then Exit;
    end;
    if (gfByDomain in FDetectFlags) and (ColumnDomain <> '') then
    begin
      Result := (FDomains.IndexOf(ColumnDomain) <> -1);
      if Result then Exit;
    end;
    Result := (gfByType in FDetectFlags);
  end
  else
    Result := False;
end;

{ TZInterbaseFirebirdConnectionGUIDProps }

{**
  For use from this unit only.
  Reads GUID-related values from Properties and inits internal fields.
}
procedure TZInterbaseFirebirdConnectionGUIDProps.InitFromProps(Properties: TStrings);
begin
  InternalInit(
    Properties.Values[ConnProps_SetGUIDByType],
    Properties.Values[ConnProps_GUIDDomains],
    Properties.Values[DSProps_GUIDFields]
  );
end;

function TZInterbaseFirebirdConnectionGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnDomain, ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, ColumnDomain, ColumnName);
end;

{ TZInterbaseFirebirdStatementGUIDProps }

{**
  For use from outside the unit.
  Creates an object based on Statement and Connection properties.
  The object should be re-created every time a Statement is opened.
}
constructor TZInterbaseFirebirdStatementGUIDProps.Create(const Statement: IZStatement);
begin
  inherited Create;
  InternalInit(
    DefineStatementParameter(Statement, DSProps_SetGUIDByType, ''),
    '', // Domain info is useless when object is created based on Statement
    DefineStatementParameter(Statement, DSProps_GUIDFields, '') );
end;

function TZInterbaseFirebirdStatementGUIDProps.ColumnIsGUID(SQLType: TZSQLType;
  DataSize: Integer; const ColumnName: string): Boolean;
begin
  Result := inherited ColumnIsGUID(SQLType, DataSize, '', ColumnName);
end;

{ TZAbstractInterbaseFirebirdCallableStatement }

function TZAbstractInterbaseFirebirdCallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I, C: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
  Conn: IZInterbaseFirebirdConnection;
begin
  SQL := '';
  Conn := Connection as IZInterbaseFirebirdConnection;
  I := Length(StoredProcName);
  i := I + 6+BindList.Count shl 1;
  SQLWriter := TZSQLStringWriter.Create(I);
  if Conn.StoredProcedureIsSelectable(StoredProcName)
  then SQLWriter.AddText('SELECT * FROM ', SQL)
  else SQLWriter.AddText('EXECUTE PROCEDURE ', SQL);
  SQLWriter.AddText(StoredProcName, SQL);
  if BindList.Capacity >0 then
    SQLWriter.AddChar('(', SQL);
  C := 0;
  for I := 0 to BindList.Capacity -1 do
    if not (BindList.ParamTypes[I] in [pctOut,pctReturn]) then begin
      SQLWriter.AddText('?,', SQL);
      Inc(C);
    end;
  if BindList.Capacity > 0 then begin
    if C > 0 then begin
      SQLWriter.CancelLastComma(SQL);
      SQLWriter.AddChar(')', SQL);
    end
    else
      SQLWriter.CancelLastCharIfExists('(', SQL);
  end;
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := InternalCreateExecutionStatement(Conn, SQL, Info);
end;

{ TZIBFBOrgSqlTypeAndScaleList }

function TZIBFBOrgSqlTypeAndScaleList.Add(sqltype: Cardinal; scale: Integer;
  Nullable: Boolean): NativeInt;
var P: PZIBFBOrgSqlTypeAndScale;
begin
  P := inherited Add(Result);
  P.sqltype := sqltype;
  P.scale := scale;
  p.Nullable := Nullable;
end;

constructor TZIBFBOrgSqlTypeAndScaleList.Create;
begin
  inherited Create(SizeOf(TZIBFBOrgSqlTypeAndScale), False);
end;

{ TZInterbaseFirebirdBindList }

class function TZInterbaseFirebirdBindList.GetElementSize: Integer;
begin
  Result := SizeOf(TZIB_FBBindValue);
end;

procedure TZInterbaseFirebirdBindList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if Action = lnDeleted then
    PZIB_FBBindValue(Ptr).TypeToken := '';
  inherited Notify(Ptr, Action);
end;

{ TZFirebirdInterbaseEventList }

constructor TZFirebirdInterbaseEventList.Create(Handler: TZOnEventHandler;
  AConnection: TZInterbaseFirebirdConnection);
begin
  inherited Create(Handler);
  FConnection := AConnection;
end;

destructor TZFirebirdInterbaseEventList.Destroy;
begin
  inherited;
  if FEventBlocks <> nil then try
    UnregisterEvents except end;
end;

procedure TZFirebirdInterbaseEventList.ProcessEvents(
  EventBlock: PZInterbaseFirebirdEventBlock);
var i: integer;
    EventCounts: TARRAY_ISC_EVENTCOUNTS;
    Event: TZFirebirdInterbaseEventData;
    CancelAlerts: Boolean;
begin
  FConnection.FInterbaseFirebirdPlainDriver.isc_event_counts(@EventCounts,
    EventBlock.buffer_length, EventBlock.event_buffer, EventBlock.result_buffer);

  if not EventBlock.FirstTime then begin
    CancelAlerts := False;
    for i := 0 to (EventBlock.EventCount - 1) do
      if (EventCounts[i] > 0) then
        if Assigned(FOnEventAlert) then begin
          FOnEventAlert(Self, PZEvent(Get(EventBlock.NamesOffSet+I)).Name, EventCounts[i], CancelAlerts);
          if CancelAlerts then
            Break;
        end else begin
          Event := TZFirebirdInterbaseEventData.Create;
          Event.fName := PZEvent(Get(EventBlock.NamesOffSet+I)).Name;
          Event.fKind := 'Event';
          Event.fEventState := esSignaled;
          Event.FCountForEvent := EventCounts[i];
          Handler(TZEventData(Event));
          if Event <> nil then
            FreeAndNil(Event);
        end;
    if CancelAlerts then
      UnregisterEvents;
  end;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5057 off : Local variable "EPB" does not seem to be initialized}
  {$WARN 5091 off : Local variable "EPBRawArray" of manged type does not seem to be initialized}
{$ENDIF}
procedure TZFirebirdInterbaseEventList.RegisterEvents;
var
  EPBRawArray: array[0..IB_MAX_EVENT_BLOCK-1] of RawByteString;
  EPB: array[0..IB_MAX_EVENT_BLOCK-1] of PAnsiChar;
  Blocks, EventCount, OffSet, EventGroup, i: Integer;
  EB: PZInterbaseFirebirdEventBlock;
  Event: PZEvent;
begin
  if FEventBlocks <> nil then
    raise EZSQLException.Create('Listener active already');
  FillChar(EPBRawArray, SizeOf(EPBRawArray), #0);
  EventCount := Count;
  Blocks := (EventCount + (IB_MAX_EVENT_BLOCK-1)) div IB_MAX_EVENT_BLOCK;
  SetLength(FEventBlocks, Blocks);
  OffSet := 0;

  for EventGroup := 0 to Blocks -1 do begin
    FillChar(EPB, SizeOf(EPB), #0);
    EB := @FEventBlocks[EventGroup];
    EventCount := (Count - OffSet);
    if (EventCount > IB_MAX_EVENT_BLOCK) then
      EventCount := IB_MAX_EVENT_BLOCK;
    EB.EventCount := EventCount;
    EB.FirstTime := True;
    EB.NamesOffSet := OffSet;
    EB.ProcessEvents := ProcessEvents;
    EB.AsyncQueEvents := AsyncQueEvents;
    for i := 0 to EventCount -1 do begin
      Event := Get(Offset+i);
      {$IFDEF UNICODE}
      EPBRawArray[I] := ZUnicodeToRaw(Event.Name, FConnection.ConSettings.ClientCodePage.CP);
      {$ELSE}
      EPBRawArray[I] := Event.Name;
      {$ENDIF}
      EPB[I] := Pointer(EPBRawArray[I]);
    end;
    EB.buffer_length := FConnection.FInterbaseFirebirdPlainDriver.isc_event_block(
      @EB.event_buffer, @EB.result_buffer, EventCount, EPB[0],
      EPB[1], EPB[2], EPB[3],  EPB[4],  EPB[5],  EPB[6],  EPB[7],
      EPB[8], EPB[9], EPB[10], EPB[11], EPB[12], EPB[13], EPB[14]);
    AsyncQueEvents(EB);
    Inc(OffSet, IB_MAX_EVENT_BLOCK);
  end;
  for I := 0 to IB_MAX_EVENT_BLOCK-1 do
    EPBRawArray[i] := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZFirebirdInterbaseEventData }

function TZFirebirdInterbaseEventData.ToString: string;
begin
  Result := inherited ToString + '; Count: '+IntToStr(FCountForEvent);
end;

initialization
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
end.
