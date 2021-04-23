{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZDbcFirebird;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZDbcUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainFirebirdInterbaseDriver, ZSysUtils, ZDbcLogging, ZDbcInterbase6Utils,
  ZGenericSqlAnalyser, ZClasses, ZDbcFirebirdInterbase,
  ZPlainFirebird;

type
  /// <summary>Implements Firebird Database Driver.</summary>
  TZFirebirdDriver = class(TZInterbaseFirebirdDriver)
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
    /// <param>Url the TZURL Object used to find the Driver, it's library and
    ///  assigns the connection properties.</param>
    /// <returns>a <c>IZConnection</c> interface that represents a
    ///  connection to the URL</returns>
    function Connect(const Url: TZURL): IZConnection; override;
  end;

  /// <summary>Defines a special Firebird Transaction interface.</summary>
  IZFirebirdTransaction = interface(IZInterbaseFirebirdTransaction)
    ['{CBCAE412-34E8-489A-A022-EAE325F6D460}']
    /// <summary>Get the Firebird ITransaction corba interface.</summary>
    /// <returns>The Firebird ITransaction corba interface.</returns>
    function GetTransaction: ITransaction;
  end;

  /// <summary>Defines a special Firebird Connection interface.</summary>
  IZFirebirdConnection = interface(IZInterbaseFirebirdConnection)
    ['{C986AC0E-BC37-44B5-963F-65646333AF7C}']
    /// <summary>Get the active IZFirebirdTransaction com interface.</summary>
    /// <returns>The IZFirebirdTransaction com interface.</returns>
    function GetActiveTransaction: IZFirebirdTransaction;
    /// <summary>Get the Firebird IAttachment corba interface.</summary>
    /// <returns>The Firebird IAttachment corba interface.</returns>
    function GetAttachment: IAttachment;
    /// <summary>Get the Firebird IStatus corba interface.</summary>
    /// <returns>The Firebird IStatus corba interface.</returns>
    function GetStatus: IStatus;
    /// <summary>Get the Firebird IUtil corba interface.</summary>
    /// <returns>The Firebird IUtil corba interface.</returns>
    function GetUtil: IUtil;
    /// <summary>Get the TZFirebirdPlainDriver object.</summary>
    /// <returns>The TZFirebirdPlainDriver object.</returns>
    function GetPlainDriver: TZFirebirdPlainDriver;
  end;

  /// <summary>Implements a special Firebird Transaction object.</summary>
  TZFirebirdTransaction = class(TZInterbaseFirebirdTransaction,
    IZTransaction, IZFirebirdTransaction, IZInterbaseFirebirdTransaction)
  private
    FTransaction: ITransaction;
  protected
    /// <summary>Is the transaction is started?</summary>
    /// <returns><c>True</c> if so otherwise <c>False</c>.</returns>
    function TxnIsStarted: Boolean; override;
    /// <summary>This method is called when a Txn should end.
    ///  It tests if we can fetch all data to the cached resultset and if no
    ///  uncached lob's are underway.</summary>
    /// <returns><c>True</c> if so otherwise <c>False</c>.</returns>
    function TestCachedResultsAndForceFetchAll: Boolean; override;
  public { implement ITransaction}
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
    /// <summary>Perform a "hard" Commit or Rollback as retained done by User
    ///  before. Removes this interface from Parent-Transaction manager.
    ///  Releases a transaction and resources immediately
    ///  instead of waiting for them to be automatically released. If the
    ///  transaction is underway a rollback will be done. Note: A
    ///  Transaction is automatically closed when the Conenction closes or it is
    ///  garbage collected. Certain fatal errors also result in a closed
    //// Transaction.</summary>
    procedure Close;
    /// <summary>Test if the transaction is underway on the server.</summary>
    /// <returns><c>True</c> if so otherwise <c>False</c>.</returns>
    function IsClosed: Boolean;
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
    /// <summary>Get's the owner connection that produced that object instance.
    /// </summary>
    /// <returns>the connection object interface.</returns>
    function GetConnection: IZConnection;
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
  public { implement IZIBTransaction }
    /// <summary>Immediat start an active transaction.</summary>
    procedure DoStartTransaction;
  public { implement IZFirebirdTransaction }
    /// <summary>Get the underlaying OO Corba interface firebird transaction.</summary>
    /// <returns>Returns the CORBA firebird transaction interface</returns>
    function GetTransaction: ITransaction;
  end;

  /// <summary>Implements a special Firebird Connection object.</summary>
  TZFirebirdConnection = class(TZInterbaseFirebirdConnection, IZConnection,
    IZTransactionManager, IZFirebirdConnection, IZInterbaseFirebirdConnection,
    IZEventListener, IZFirebirdInterbaseEventAlerter)
  private
    FMaster: IMaster;
    FProvider: IProvider;
    FAttachment: IAttachment;
    FStatus: IStatus;
    FUtil: IUtil;
    FPlainDriver: TZFirebirdPlainDriver;
  protected
    /// <summary>Closes the connection internaly and frees all server resources</summary>
    procedure InternalClose; override;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a raw encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    class function GetEventListClass: TZFirebirdInterbaseEventListClass; override;
  public
    procedure AfterConstruction; override;
    Destructor Destroy; override;
  public { IZFirebirdConnection }
    /// <summary>Get the active IZFirebirdTransaction com interface.</summary>
    /// <returns>The IZFirebirdTransaction com interface.</returns>
    function GetActiveTransaction: IZFirebirdTransaction;
    /// <summary>Get the Firebird IAttachment corba interface.</summary>
    /// <returns>The Firebird IAttachment corba interface.</returns>
    function GetAttachment: IAttachment;
    /// <summary>Get the Firebird IStatus corba interface.</summary>
    /// <returns>The Firebird IStatus corba interface.</returns>
    function GetStatus: IStatus;
    /// <summary>Get the TZFirebirdPlainDriver object.</summary>
    /// <returns>The TZFirebirdPlainDriver object.</returns>
    function GetPlainDriver: TZFirebirdPlainDriver;
    /// <summary>Get the Firebird IUtil corba interface.</summary>
    /// <returns>The Firebird IUtil corba interface.</returns>
    function GetUtil: IUtil;
  public { implement IZTransactionManager }
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
    function CreateStatementWithParams(Params: TStrings): IZStatement;
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
    function PrepareStatementWithParams(const SQL: string; Params: TStrings):
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
    /// <author>firmos (initially for MySQL 27032006)</author>
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
  public { implement IZInterbaseFirebirdConnection }
    /// <summary>Determine if the Client lib-module is a Firebird lib</summary>
    /// <returns><c>True</c>If it's a firebird client lib; <c>False</c>
    ///  otherwise</returns>
    function IsFirebirdLib: Boolean; override;
    /// <summary>Determine if the Client lib-module is a Interbase lib</summary>
    /// <returns><c>True</c>If it's a Interbase client lib; <c>False</c>
    ///  otherwise</returns>
    function IsInterbaseLib: Boolean; override;
  public { implement IZEventListener}
    /// <summary>Starts listening the events.</summary>
    /// <param>"EventNames" a list of event name to be listened.</param>
    /// <param>"Handler" an event handler which gets triggered if the event is received.</param>
    procedure Listen(const EventNames: TStrings; Handler: TZOnEventHandler);
  end;

  TZFirebirdEventList = class(TZFirebirdInterbaseEventList)
  public
    procedure AsyncQueEvents(EventBlock: PZInterbaseFirebirdEventBlock); override;
    procedure UnregisterEvents; override;
  end;

  TZFBEventCallback = class(IEventCallbackImpl)
  private
    FEventBlock: PZInterbaseFirebirdEventBlock;
    FOwner: TZFirebirdEventList;
    FRefCnt: integer; //for refcounting
  public
    constructor Create(aOwner: TZFirebirdEventList; AEventBlock: PZInterbaseFirebirdEventBlock);
    procedure addRef;  override;
    function release: Integer; override;
    procedure eventCallbackFunction(length: Cardinal; events: PByte); override;
 end;

{$ENDIF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses ZFastCode, ZDbcFirebirdStatement, ZDbcInterbaseFirebirdMetadata, ZEncoding,
  ZDbcMetadata, ZMessages, ZPlainDriver,
  {$IFNDEF ZEOS_DISABLE_INTERBASE}ZDbcInterbase6,{$ENDIF}
  ZDbcProperties, Math
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES},System.Types{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZFirebirdDriver }

function TZFirebirdDriver.Connect(const Url: TZURL): IZConnection;
var iPlainDriver: IZPlainDriver;
    FirebirdPlainDriver: TZFirebirdPlainDriver;
    S: String;
begin
  iPlainDriver := GetPlainDriver(URL, True);
  S := URL.Properties.Values[ConnProps_FirebirdAPI];
  if S <> '' then
    S := LowerCase(S);
  FirebirdPlainDriver := iPlainDriver.GetInstance as TZFirebirdPlainDriver;
  if Assigned(FirebirdPlainDriver.fb_get_master_interface) and (S <> 'legacy') then
    Result := TZFirebirdConnection.Create(URL)
  else
    {$IFDEF ZEOS_DISABLE_INTERBASE}
    raise EZSQLException.Create('couldn''t find fb_get_master_interface in client library!');
    {$ELSE ZEOS_DISABLE_INTERBASE}
    Result := TZInterbase6Connection.Create(Url);
    {$ENDIF ZEOS_DISABLE_INTERBASE}
end;

constructor TZFirebirdDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZFirebirdPlainDriver.Create));
end;

{ TZFirebirdConnection }

function TZFirebirdConnection.AbortOperation: Integer;
begin
  if FAttachment = nil
  then Result := 0
  else begin
    FAttachment.cancelOperation(FStatus, fb_cancel_abort);
    Result := Ord((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF}) <> 0);
    if Result <> 0 then
      FStatus.init;
  end;
end;

procedure TZFirebirdConnection.AfterConstruction;
begin
  FPlainDriver := PlainDriver.GetInstance as TZFirebirdPlainDriver;
  FMaster := IMaster(FPlainDriver.fb_get_master_interface);
  FStatus := FMaster.getStatus;
  FUtil := FMaster.getUtilInterface;
  inherited AfterConstruction;
end;

function TZFirebirdConnection.CreateStatementWithParams(
  Params: TStrings): IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZFirebirdStatement.Create(Self, Params);
end;

function TZFirebirdConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
begin
  if Params = nil then
    Params := Info;
  Result := TZFirebirdTransaction.Create(Self, AutoCommit, ReadOnly, TransactIsolationLevel, Params);
  fTransactions.Add(Result);
end;

destructor TZFirebirdConnection.Destroy;
begin
  inherited;
  if FStatus <> nil then begin
    FStatus.Dispose;
    FStatus := nil;
  end;
  //How to free IMaster?
end;

procedure TZFirebirdConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var ZTrans: IZFirebirdTransaction;
    FBTrans: ITransaction;
    Stmt: IStatement;
    FStatementType: TZIbSqlStatementType;
    State: Cardinal;
begin
  ZTrans := GetActiveTransaction;
  FBTrans := ZTrans.GetTransaction;
  {$IFDEF UNICODE}
  if DriverManager.HasLoggingListener then
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if LoggingCategory = lcTransaction then begin
    FAttachment.execute(FStatus, FBTrans, Length(SQL), Pointer(SQL),
      FDialect, nil, nil, nil, nil);
    State := Fstatus.getState;
    if ((State and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
       ((State and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
      {$IFDEF UNICODE}
      if not DriverManager.HasLoggingListener then
        FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else begin
    Stmt := FAttachment.prepare(FStatus, FBTrans, Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Pointer(SQL), FDialect, 0);
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
       ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
      {$IFDEF UNICODE}
      if not DriverManager.HasLoggingListener then
        FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
    FStatementType := TZIbSqlStatementType(Stmt.getType(FStatus));
    try
      if FStatementType in [stGetSegment, stPutSegment, stStartTrans..stRollback]
      then raise EZSQLException.Create(SStatementIsNotAllowed)
      else begin
        Stmt.execute(FStatus, FBTrans, nil, nil, nil, nil);
        if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
           ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
          {$IFDEF UNICODE}
          if not DriverManager.HasLoggingListener then
            FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
          {$ENDIF}
          HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
            {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
        end;
      end;
    finally
      Stmt.free(FStatus);
      Stmt.release;
    end;
  end;
end;

function TZFirebirdConnection.GetActiveTransaction: IZFirebirdTransaction;
var TA: IZTransaction;
begin
  if not Closed then begin
    if fActiveTransaction = nil then begin
      TA := CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction);
    end;
    fActiveTransaction.QueryInterface(IZFirebirdTransaction, Result);
  end else
    Result := nil;
end;

function TZFirebirdConnection.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

class function TZFirebirdConnection.GetEventListClass: TZFirebirdInterbaseEventListClass;
begin
  Result := TZFirebirdEventList;
end;

function TZFirebirdConnection.GetPlainDriver: TZFirebirdPlainDriver;
begin
  Result := FPlainDriver;
end;

function TZFirebirdConnection.GetStatus: IStatus;
begin
  Result := FStatus;
end;

function TZFirebirdConnection.GetUtil: IUtil;
begin
  Result := FUtil;
end;

procedure TZFirebirdConnection.InternalClose;
begin
  inherited InternalClose;
  if FAttachment <> nil then begin
    FAttachment.detach(FStatus);
    FAttachment.release;
    FAttachment := nil;
  end;
  if FProvider <> nil then begin
    FProvider.release;
    FProvider := nil;
  end;
end;

function TZFirebirdConnection.IsFirebirdLib: Boolean;
begin
  Result := True;
end;

function TZFirebirdConnection.IsInterbaseLib: Boolean;
begin
  Result := False;
end;

procedure TZFirebirdConnection.Listen(const EventNames: TStrings;
  Handler: TZOnEventHandler);
var I: Integer;
begin
  if (FEventList <> nil) then begin
    if (FEventList.Count > 0) then
      Unlisten;
    if IsClosed then
      Open;
    for i := 0 to EventNames.Count -1 do
      FEventList.Add(EventNames[I], Handler);
    FEventList.RegisterEvents;
  end else
    raise EZSQLException.Create('no events registered');
end;

procedure TZFirebirdConnection.Open;
var
  ti: IZFirebirdTransaction;
  DPB: RawByteString;
  DBCP, CSNoneCP, CreateDB: String;
  ConnectionString: SQLString;
  DBCreated: Boolean;
  Statement: IZStatement;
  {$IFDEF UNICODE}
  CP: Word;
  {$ENDIF}
  TimeOut: Cardinal;
  procedure PrepareDPB;
  var
    R: RawByteString;
    P: PAnsiChar;
    L: LengthInt;
  begin
    {$IFDEF UNICODE}
    if (Info.IndexOf('isc_dpb_utf8_filename') = -1)
    then CP := zOSCodePage
    else CP := zCP_UTF8;
    {$ENDIF}
    {$IFDEF UNICODE}
    R := ZUnicodeToRaw(ConnectionString, CP);
    {$ELSE}
    R :=  ConnectionString;
    {$ENDIF}
    DPB := GenerateDPB(FPlainDriver, Info {$IFDEF UNICODE},CP{$ENDIF});
    P := Pointer(R);
    L := Min(1024, Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
    if L > 0 then
      Move(P^, FByteBuffer[0], L);
    PByte(PAnsiChar(@FByteBuffer[0])+L)^ := 0;
  end;
label reconnect;
begin
  if not Closed then
    Exit;
  FProvider := FMaster.getDispatcher;
  DBCP := '';
  if TransactIsolationLevel = tiReadUncommitted then
    raise EZSQLException.Create('Isolation level do not capable');
  if ConSettings^.ClientCodePage = nil then
    CheckCharEncoding(FClientCodePage, True);

  AssignISC_Parameters;
  CSNoneCP := Info.Values[ConnProps_Charset_NONE_Alias];
  ConnectionString := ConstructConnectionString;

  DBCreated := False;
  reconnect:
  try
    CreateDB := Info.Values[ConnProps_CreateNewDatabase];
    if (CreateDB <> '') and StrToBoolEx(CreateDB) and not DBCreated then begin
      if (Info.Values[ConnProps_isc_dpb_lc_ctype] <> '') and (Info.Values[ConnProps_isc_dpb_set_db_charset] = '') then
        Info.Values[ConnProps_isc_dpb_set_db_charset] := Info.Values[ConnProps_isc_dpb_lc_ctype];
      DBCP := Info.Values[ConnProps_isc_dpb_set_db_charset];
      PrepareDPB;
      FAttachment := FProvider.createDatabase(FStatus, @FByteBuffer[0], Smallint(Length(DPB)),Pointer(DPB));
      Info.Values[ConnProps_CreateNewDatabase] := ''; //prevent recreation on open
      DBCreated := True;
      FLogMessage := 'CREATE DATABASE "'+URL.Database+'" AS USER "'+ URL.UserName+'"';
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      try
        HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
          FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
      finally
        FProvider.release;
        FProvider := nil;
      end;
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    end;
    if FProvider = nil then
      FProvider := FMaster.getDispatcher;
    if FStatus  = nil then
      FStatus := FMaster.getStatus;
    if FAttachment = nil then begin
      PrepareDPB;
      FLogMessage := Format(SConnect2AsUser, [ConnectionString, URL.UserName]);;
      FAttachment := FProvider.attachDatabase(FStatus, @FByteBuffer[0], Length(DPB), Pointer(DPB));
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
        HandleErrorOrWarning(lcConnect, PARRAY_ISC_STATUS(FStatus.getErrors),
          FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
      { Logging connection action }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    end;
    { Dialect could have changed by isc_dpb_set_db_SQL_dialect command }
    FByteBuffer[0] := isc_info_db_SQL_Dialect;
    FAttachment.getInfo(FStatus, 1, @FByteBuffer[0], SizeOf(TByteBuffer)-1, @FByteBuffer[1]);
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IAttachment.getInfo', Self);
    if FByteBuffer[1] = isc_info_db_SQL_Dialect
    then FDialect := ReadInterbase6Number(FPlainDriver, @FByteBuffer[2])
    else FDialect := SQL_DIALECT_V5;
    inherited SetAutoCommit(AutoCommit or (Info.IndexOf(TxnProps_isc_tpb_autocommit) <> -1));
    FRestartTransaction := not AutoCommit;

    FHardCommit := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
    inherited Open;
  finally
    if Closed then begin
      if FAttachment <> nil then begin
        FAttachment.Release;
        FAttachment := nil;
      end;
      if FProvider <> nil then begin
        FProvider.Release;
        FProvider := nil;
      end;
    end;
  end;
  with GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo do
  begin
    CollectServerInformations; //keep this one first!
    FHostVersion := GetHostVersion;
    FXSQLDAMaxSize := GetMaxSQLDASize;
  end;
  if (DBCP = '') and not DBCreated then begin
    {Check for ClientCodePage: if empty switch to database-defaults
      and/or check for charset 'NONE' which has a different byte-width
      and no convertions where done except the collumns using collations}
    if not DBCreated then begin
      Statement := CreateStatementWithParams(nil);
      try
        with Statement.ExecuteQuery('SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE') do begin
          if Next then DBCP := GetString(FirstDbcIndex);
          Close;
        end;
      finally
        Statement := nil;
      end;
      ti := GetActiveTransaction;
      try
        ti.Close;
      finally
        ti := nil;
      end;
    end;
    if DBCP = 'NONE' then begin { SPECIAL CASE CHARCTERSET "NONE":
      EH: the server makes !NO! charset conversion if CS_NONE.
      Attaching a CS "NONE" db with a characterset <> CS_NONE has this effect:
      All field codepages are retrieved as the given client-characterset.
      This works nice as long the fields have it's own charset definition.
      But what's the encoding of the CS_NONE fields? The more what about CLOB encoding?

      If we're attaching the db with CS "NONE" all userdefined field CP's are
      returned gracefully. Zeos can convert everything to your Controls-CP.
      Except the CP_NONE fields. And the text lob's where encoding is unknown too.
      For the Unicode-IDE's this case is a nightmare. Jan's suggestion is to use
      the fields as Byte/BlobFields only. My idea is to use such fields with the
      CPWIN1252 Charset which maps each byte to words and vice versa.
      So no information is lost and the data is still readable "somehow".

      Side-note: see: https://firebirdsql.org/rlsnotesh/str-charsets.html
      Any DDL/DML in non ASCII7 range will give a maleformed string if encoding is
      different to UTF8/UNICODE_FSS because the RDB$-Tables have a
      UNICODE_FSS collation}

      {test if charset is not given or is CS_NONE }
      if CSNoneCP = ''
      then CSNoneCP := FClientCodePage
      else if FCLientCodePage <> ''
        then CSNoneCP := FCLientCodePage
        else CSNoneCP := 'WIN1252'; {WIN1252 would be optimal propably}
      ResetCurrentClientCodePage(CSNoneCP, False);
      ConSettings^.ClientCodePage^.ID := 0;
      //Now notify our metadata object all fields are retrieved in utf8 encoding
      (FMetadata as TZInterbase6DatabaseMetadata).SetUTF8CodePageInfo;
      if (FCLientCodePage <> DBCP) then begin
        Info.Values[ConnProps_isc_dpb_lc_ctype] := DBCP;
        InternalClose;
        goto reconnect; //build new TDB and reopen in SC_NONE mode
      end;
    end else if FClientCodePage = '' then
      CheckCharEncoding(DBCP);
  end;
  if (FAttachment.vTable.version >= 4) and (FHostVersion >= 4000000) then begin
    TimeOut := StrToIntDef(Info.Values[ConnProps_StatementTimeOut], 0);
    if TimeOut > 0 then begin
      FAttachment.SetStatementTimeOut(Fstatus, TimeOut);
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
        HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
          'IAttachment.SetStatmentTimeOut', IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
    TimeOut := StrToIntDef(Info.Values[ConnProps_SessionIdleTimeOut], 0);
    if TimeOut > 16 then begin
      FAttachment.setIdleTimeout(Fstatus, TimeOut);
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
        HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
          'IAttachment.setIdleTimeout', IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
  if (FHostVersion >= 4000000) then begin
    if (Info.Values[ConnProps_isc_dpb_session_time_zone] = '') then
      ExecuteImmediat('SET TIME ZONE LOCAL', lcExecute);
    ExecuteImmediat('SET BIND OF TIME ZONE TO LEGACY', lcExecute);
    ExecuteImmediat('SET BIND OF DECFLOAT TO LEGACY', lcExecute);
    ti := GetActiveTransaction;
    try
      ti.Close;
    finally
      ti := nil;
    end;
  end;
end;

function TZFirebirdConnection.PingServer: Integer;
begin
  if (FAttachment <> nil) and (FStatus <> nil) then begin
    FAttachment.Ping(FStatus);
    Result := -Ord((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF}) <> 0);
    FStatus.init;
  end else Result := -1;
end;

function TZFirebirdConnection.PrepareCallWithParams(const Name: string;
  Params: TStrings): IZCallableStatement;
begin
  if IsClosed then
    Open;
  Result := TZFirebirdCallableStatement.Create(Self, Name, Params);
end;

function TZFirebirdConnection.PrepareStatementWithParams(const SQL: string;
  Params: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  {if Self.FHostVersion >= 4000000
  then Result := TZFirebird4upPreparedStatement.Create(Self, SQL, Params)
  else }Result := TZFirebirdPreparedStatement.Create(Self, SQL, Params);
end;

var
  FireBirdDriver: IZDriver;

{ TZFirebirdTransaction }

procedure TZFirebirdTransaction.Close;
begin
  if FTransaction <> nil then with TZFirebirdConnection(FOwner) do begin
    if fDoCommit
    then FTransaction.commit(FStatus)
    else FTransaction.rollback(FStatus);
    FTransaction.release;
    FTransaction := nil;
    fSavepoints.Clear;
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sCommitMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
    FOwner.ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
  end;
end;

procedure TZFirebirdTransaction.Commit;
var S: RawByteString;
begin
  with TZFirebirdConnection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'RELEASE SAVEPOINT '+ {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTransaction <> nil then try
    if TZFirebirdConnection(FOwner).FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then begin
      FTransaction.commit(FStatus);
      {$IFDEF DEBUG}Assert({$ENDIF}FTransaction.Release{$IFDEF DEBUG} = 0){$ENDIF};
      FTransaction := nil;
    end else begin
      fDoCommit := True;
      fDoLog := False;
      FTransaction.commitRetaining(FStatus);
      ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
    end;
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sCommitMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    if fDoLog and (ZDbcIntfs.DriverManager <> nil) and
       ZDbcIntfs.DriverManager.HasLoggingListener then //don't use the local DriverManager of ZDbcConnection
      ZDbcIntfs.DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
  end;
end;

procedure TZFirebirdTransaction.DoStartTransaction;
begin
  GetTransaction;
end;

function TZFireBirdTransaction.GetConnection: IZConnection;
begin
  Result := FOwner as TZFirebirdConnection;
  FOwner.SetActiveTransaction(Self);
end;

function TZFireBirdTransaction.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
    StartTransaction;
  Result := FTransaction;
end;

function TZFirebirdTransaction.IsClosed: Boolean;
begin
  Result := FTransaction = nil;
end;

procedure TZFirebirdTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  try
    if FTransaction <> nil then begin
      FTransaction.release;
      FTransaction := nil;
    end;
  finally
    inherited;
  end;
end;

procedure TZFirebirdTransaction.Rollback;
var S: RawByteString;
begin
  with TZFirebirdConnection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'ROLLBACK TO '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTransaction <> nil then with TZFirebirdConnection(FOwner) do try
    if FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then begin
      FTransaction.rollback(FStatus);
      {$IFDEF DEBUG}Assert({$ENDIF}FTransaction.Release{$IFDEF DEBUG} = 0){$ENDIF};
      FTransaction := nil;
    end else begin
      fDoCommit := True;
      fDoLog := False;
      FTransaction.rollbackRetaining(FStatus);
      ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
    end;
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sRollbackMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    if fDoLog and (ZDbcIntfs.DriverManager <> nil) and
       ZDbcIntfs.DriverManager.HasLoggingListener then //don't use the local DriverManager of ZDbcConnection
      ZDbcIntfs.DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
  end;
end;

function TZFirebirdTransaction.StartTransaction: Integer;
var S: String;
begin
  with TZFirebirdConnection(FOwner) do begin
    if FTransaction = nil then begin
      if FTPB = EmptyRaw then
        FTPB := FOwner.GenerateTPB(FAutoCommit, FReadOnly, FTransactionIsolation, FProperties);
      FTransaction := FAttachment.startTransaction(FStatus,
        Length(FTPB){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Pointer(FTPB));
      FTransaction.AddRef;
      Result := Ord(not Self.FAutoCommit);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, URL.Protocol, 'TRANSACTION STARTED.');
    end else begin
      Result := FSavePoints.Count+2;
      S := 'SP'+ZFastcode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result);
      ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
      Result := FSavePoints.Add(S)+2;
    end;
  end;
end;

function TZFirebirdTransaction.TestCachedResultsAndForceFetchAll: Boolean;
var I, RowNo: Integer;
  P: Pointer;
begin
  Result := False;
  for I := 0 to FOpenCursors.Count -1 do
    if IZResultSet(FOpenCursors[i]).GetConcurrency <> rcUpdatable then
      Exit;
  Result := True;
  while FOpenCursors.Count > 0 do begin
    P := FOpenCursors[FOpenCursors.Count-1];
    RowNo := IZResultSet(P).GetRow;
    IZResultSet(P).Last; //now the pointer will be removed from the open cursor list
    IZResultSet(P).MoveAbsolute(RowNo); //restore current position
  end;
end;

function TZFirebirdTransaction.TxnIsStarted: Boolean;
begin
  Result := FTransaction <> nil;
end;

{ TZFBEventCallback }

procedure TZFBEventCallback.addRef;
begin
  Inc(FRefCnt);
end;

constructor TZFBEventCallback.Create(aOwner: TZFirebirdEventList;
  AEventBlock: PZInterbaseFirebirdEventBlock);
begin
  inherited Create;
  FEventBlock := AEventBlock;
  FOwner := aOwner;
  FRefCnt := 1;
end;

procedure TZFBEventCallback.eventCallbackFunction(length: Cardinal;
  events: PByte);
begin
  if (events <> nil) and (Length > 0) then begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(events^, FEventBlock.result_buffer^, length);
    FEventBlock.ProcessEvents(FEventBlock);
    FEventBlock.AsyncQueEvents(FEventBlock);
    FEventBlock.FirstTime := False
  end;
end;

function TZFBEventCallback.release: Integer;
begin
  Dec(FRefCnt);
  Result := FRefCnt;
  if FRefCnt = 0 then begin
    FEventBlock.FBEventsCallback := nil;
    {$IFDEF AUTOREFCOUNT}
    Destroy;
    {$ELSE}
    Free;
    {$ENDIF}
  end;
end;

{ TZFirebirdEventList }

procedure TZFirebirdEventList.AsyncQueEvents(
  EventBlock: PZInterbaseFirebirdEventBlock);
begin
  if EventBlock.FBEventsCallback = nil then
    EventBlock.FBEventsCallback := TZFBEventCallback.Create(Self, EventBlock);
  if EventBlock.FBEvent <> nil then begin
    IEvents(EventBlock.FBEvent).Release;
    EventBlock.FBEvent := nil;
  end;
  with TZFirebirdConnection(Connection) do
  EventBlock.FBEvent := FAttachment.queEvents(FStatus, TZFBEventCallback(EventBlock.FBEventsCallback),
    EventBlock.buffer_length, PByte(EventBlock.event_buffer));
end;

procedure TZFirebirdEventList.UnregisterEvents;
var EventBlockIdx: NativeInt;
    EventBlock: PZInterbaseFirebirdEventBlock;
begin
  try
    for EventBlockIdx := 0 to High(FEventBlocks) do begin
      EventBlock := @FEventBlocks[EventBlockIdx];
      EventBlock.FirstTime := True;
      try
        if EventBlock.FBEvent <> nil then with TZFirebirdConnection(Connection) do begin
          IEvents(EventBlock.FBEvent).Cancel(FStatus);
          if ((FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
            HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IAttachment.queEvents', IImmediatelyReleasable(FWeakImmediatRelPtr));
        end;
      finally
        if EventBlock.FBEvent <> nil then begin
          IEvents(EventBlock.FBEvent).Release;
          EventBlock.FBEvent := nil;
        end;
        if EventBlock.FBEventsCallback <> nil then begin
          TZFBEventCallback(EventBlock.FBEventsCallback).release;
          EventBlock.FBEventsCallback := nil;
        end;
      end;
    end;
  finally
    SetLength(FEventBlocks, 0)
  end;
end;

initialization
  FireBirdDriver := TZFirebirdDriver.Create;
  DriverManager.RegisterDriver(FireBirdDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(FireBirdDriver);
  FireBirdDriver := nil;
{$ENDIF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
end.
