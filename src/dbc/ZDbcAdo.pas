{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               ADO Connectivity Classes                  }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcAdo;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcConnection, ZDbcIntfs, ZCompatibility, ZPlainAdoDriver,
  ZPlainAdo, ZTokenizer, ZDbcLogging;

type
  {** Implements Ado Database Driver. }
  TZAdoDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetTokenizer: IZTokenizer; override;
  end;

  {** Represents an Ado specific connection interface. }
  IZAdoConnection = interface (IZConnection)
    ['{50D1AF76-0174-41CD-B90B-4FB770EFB14F}']
    function GetAdoConnection: ZPlainAdo.Connection;
    function GetByteBufferAddress: PByteBuffer;
    procedure HandleErrorOrWarning(LoggingCategory: TZLoggingCategory;
      const E: Exception; const Sender: IImmediatelyReleasable;
      const LogMsg: SQLString);
  end;

  {** Implements a generic Ado Connection. }
  TZAdoConnection = class(TZAbstractSingleTxnConnection, IZConnection,
    IZAdoConnection, IZTransaction)
  private
    fServerProvider: TZServerProvider;
    FTransactionLevel: Integer;
    FHostVersion: Integer;
  protected
    FAdoConnection: ZPlainAdo.Connection;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a UTF16 encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure InternalClose; override;
  public
    function GetAdoConnection: ZPlainAdo.Connection;
    procedure HandleErrorOrWarning(LoggingCategory: TZLoggingCategory;
      const E: Exception; const Sender: IImmediatelyReleasable;
      const LogMsg: SQLString);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
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

    procedure Open; override;

    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    function GetHostVersion: Integer; override;
    /// <summary>Returns the ServicerProvider for this connection. For ODBC
    ///  the connection must be opened to determine the provider. Otherwise
    ///  the provider is tested against the driver names</summary>
    /// <returns>the ServerProvider or spUnknown if not known.</returns>
    function GetServerProvider: TZServerProvider; override;
  end;

var
  {** The common driver manager object. }
  AdoDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, ActiveX,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZFastCode,
  ZPlainOleDBDriver,
  ZDbcUtils, ZAdoToken, ZSysUtils, ZMessages, ZDbcProperties, ZDbcAdoStatement,
  ZDbcAdoMetadata, ZEncoding, ZDbcOleDBUtils, ZDbcOleDBMetadata, ZDbcAdoUtils;

const                                                //adXactUnspecified
  IL: array[TZTransactIsolationLevel] of TOleEnum = (adXactChaos, adXactReadUncommitted, adXactReadCommitted, adXactRepeatableRead, adXactSerializable);

{ TZAdoDriver }

{**
  Constructs this object with default properties.
}
constructor TZAdoDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZAdoPlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
}
function TZAdoDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZAdoConnection.Create(Url);
end;

function TZAdoDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZAdoSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

var //eh: was threadvar but this defintely does not work! we just need !one! value
  AdoCoInitialized: integer;

procedure CoInit;
begin
  inc(AdoCoInitialized);
  if AdoCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(AdoCoInitialized>0);
  dec(AdoCoInitialized);
  if AdoCoInitialized=0 then
    CoUninitialize;
end;
{ TZAdoConnection }

{**
  Destroys this object and cleanups the memory.
}
destructor TZAdoConnection.Destroy;
begin
  try
    inherited Destroy;
  finally
    FAdoConnection := nil;
    CoUninit;
  end;
end;

procedure TZAdoConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var
  RowsAffected: OleVariant;
begin
  try
    FAdoConnection.Execute(SQL, RowsAffected, adExecuteNoRecords);
    {$IFNDEF UNICODE}FlogMessage := ZUnicodeToRaw(SQL, zCP_UTF8);{$ENDIF}
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LoggingCategory, URL.Protocol, {$IFDEF UNICODE}SQL{$ELSE}FlogMessage{$ENDIF});
  except
    on E: Exception do begin
      {$IFNDEF UNICODE}FlogMessage := ZUnicodeToRaw(SQL, zCP_UTF8);{$ENDIF}
      HandleErrorOrWarning(LoggingCategory, E, Self, {$IFDEF UNICODE}SQL{$ELSE}FlogMessage{$ENDIF});
    end;
  end;
end;

{**
  Just return the Ado Connection
}
function TZAdoConnection.GetAdoConnection: ZPlainAdo.Connection;
begin
  Result := FAdoConnection;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAdoConnection.Open;
var
  ConnectStrings: TStrings;
  DBInitialize: IDBInitialize;
  Command: ZPlainAdo.Command;
  DBCreateCommand: IDBCreateCommand;
  GetDataSource: IGetDataSource;
begin
  if not Closed then Exit;

  FLogMessage := Format(SConnect2AsUser,  [URL.Database, URL.UserName]);
  try
    if ReadOnly then
      FAdoConnection.Set_Mode(adModeRead)
    else
      FAdoConnection.Set_Mode(adModeUnknown);

    ConnectStrings := SplitString(DataBase, ';');
    FServerProvider := ProviderNamePrefix2ServerProvider(ConnectStrings.Values[ConnProps_Provider]);
    FreeAndNil(ConnectStrings);

    FAdoConnection.Open(WideString(Database), WideString(User), WideString(Password), -1{adConnectUnspecified});
    FAdoConnection.Set_CursorLocation(adUseClient);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    //ConSettings^.AutoEncode := {$IFDEF UNICODE}False{$ELSE}True{$ENDIF};
    CheckCharEncoding('CP_UTF16');
  except
    on E: Exception do
      HandleErrorOrWarning(lcConnect, E, Self, FlogMessage);
  end;

  inherited Open;

  {EH: the only way to get back to generic Ole is using the command ... }
  Command := CoCommand.Create;
  Command.Set_ActiveConnection(FAdoConnection);
  if Succeeded(((Command as ADOCommandConstruction).OLEDBCommand as ICommand).GetDBSession(IID_IDBCreateCommand, IInterface(DBCreateCommand))) then
    if DBCreateCommand.QueryInterface(IID_IGetDataSource, GetDataSource) = S_OK then
      if Succeeded(GetDataSource.GetDataSource(IID_IDBInitialize, IInterFace(DBInitialize))) then
        (GetMetadata.GetDatabaseInfo as IZOleDBDatabaseInfo).InitilizePropertiesFromDBInfo(DBInitialize, ZAdoMalloc);

  if not GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(GetTransactionIsolation) then
    inherited SetTransactionIsolation(GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation);
  FAdoConnection.IsolationLevel := IL[GetTransactionIsolation];
  if not AutoCommit then begin
    AutoCommit := True;
    SetAutoCommit(False);
  end;
end;

function TZAdoConnection.PrepareCallWithParams(const Name: string;
  Params: TStrings): IZCallableStatement;
begin
  if IsClosed then Open;
  Result := TZAdoCallableStatement2.Create(Self, Name, Params);
end;

function TZAdoConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZAdoPreparedStatement.Create(Self, SQL, Info)
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

  @param Info a statement parameters.
  @return a new Statement object
}
function TZAdoConnection.CreateStatementWithParams(Info: TStrings): IZStatement;
begin
  if IsClosed then Open;
  Result := TZAdoStatement.Create(Self, Info);
end;

procedure TZAdoConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      while FTransactionLevel > 0 do begin
        FAdoConnection.CommitTrans;
        Dec(FTransactionLevel);
      end;
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, URL.Protocol, 'COMMIT');
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

procedure TZAdoConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if TransactIsolationLevel = Level then Exit;
  if not Closed then begin
    FAdoConnection.IsolationLevel := IL[Level];
    if not AutoCommit then
      StartTransaction;
  end;
  TransactIsolationLevel := Level;
end;

function TZAdoConnection.StartTransaction: Integer;
var S: String;
{$IFNDEF UNICODE}
LogMessage: UnicodeString;
{$ENDIF}
begin
  if Closed then
    Open;
  AutoCommit := False;
  FLogMessage := 'START TRANSACTION';
  if FTransactionLevel = 0 then try
    FTransactionLevel := FAdoConnection.BeginTrans;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, FLogMessage);
    Result := FTransactionLevel;
  except
    on E: Exception do begin
      HandleErrorOrWarning(lcTransaction, E, Self, FLogMessage);
      Result := 0; //Satisfy compiler
    end;
  end else begin
    if cSavePointSyntaxW[fServerProvider][spqtSavePoint] = '' then
      raise EZSQLException.Create(SUnsupportedOperation);
    S := 'SP'+{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(NativeUint(Self))+'_'+{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(FSavePoints.Count);
    {$IFDEF UNICODE}FLogMessage{$ELSE}LogMessage{$ENDIF} :=
      cSavePointSyntaxW[fServerProvider][spqtSavePoint]+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(S);
    ExecuteImmediat({$IFDEF UNICODE}FLogMessage{$ELSE}LogMessage{$ENDIF}, lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.AfterConstruction;
begin
  CoInit;
  FAdoConnection := CoConnection.Create;
  FMetadata := TZAdoDatabaseMetadata.Create(Self, URL);
  fHostVersion := -1;
  inherited AfterConstruction;
end;

procedure TZAdoConnection.Commit;
var S: UnicodeString;
  Cnt: Integer;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SInvalidOpInAutoCommit);
  if Closed then Exit;
  Cnt := FSavePoints.Count;
  if Cnt > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtCommit];
    if S <> '' then begin
      S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      if fServerProvider = spMSSQL then
        S := '"'+S+'"';
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else try
    FAdoConnection.CommitTrans;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
    FTransactionLevel := 0;
    AutoCommit := True;
  except
    on E: Exception do
      HandleErrorOrWarning(lcTransaction, E, Self, sCommitMsg);
  end;
  if (Cnt = 0) and FRestartTransaction then
    StartTransaction;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.Rollback;
var S: UnicodeString;
  Cnt: Integer;
begin
  if AutoCommit then
    raise EZSQLException.Create(SInvalidOpInAutoCommit);
  Cnt := FSavePoints.Count;
  if Cnt > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtRollback];
    if S <> '' then begin
      S := S+UnicodeString(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else try
    FAdoConnection.RollbackTrans;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
    FTransactionLevel := 0;
    AutoCommit := True;
  except
    on E: Exception do
      HandleErrorOrWarning(lcTransaction, E, Self, sRollbackMsg);
  end;
  if (Cnt = 0) and FRestartTransaction then
    StartTransaction;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZAdoConnection.InternalClose;
var LogMessage: SQLString;
begin
  if Closed or (not Assigned(PlainDriver)) then
    Exit;

  LogMessage := 'CLOSE CONNECTION TO "'+URL.Database+'"';
  FSavePoints.Clear;
  if not AutoCommit then begin
    AutoCommit := not FRestartTransaction;
    FTransactionLevel := 0;
    try
      FAdoConnection.RollbackTrans;
    except end;
  end;
  try
    if FAdoConnection.State = adStateOpen then
      FAdoConnection.Close;
//      FAdoConnection := nil;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcDisconnect, URL.Protocol, LogMessage);
  except
    on E: Exception do
      HandleErrorOrWarning(lcDisconnect, E, Self, LogMessage);
  end;
end;

procedure TZAdoConnection.SetCatalog(const Catalog: string);
begin
  if Closed then Exit;

  FLogMessage := 'SET CATALOG '+Catalog;
  try
    if Catalog <> '' then //see https://sourceforge.net/p/zeoslib/tickets/117/
      FAdoConnection.DefaultDatabase := WideString(Catalog);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcOther, URL.Protocol, FLogMessage);
  except
    on E: Exception do
      HandleErrorOrWarning(lcOther, E, Self, FLogMessage);
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAdoConnection.GetCatalog: string;
begin
  Result := String(FAdoConnection.DefaultDatabase);
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZAdoConnection.GetHostVersion: Integer;
  procedure DetermineProductVersion;
  var ProductVersion: String;
  begin
    ProductVersion := GetMetadata.GetDatabaseInfo.GetDatabaseProductVersion;
    fHostVersion := SQLServerProductToHostVersion(ProductVersion);
  end;
begin
  if (fHostVersion = -1) then
    DetermineProductVersion;
  Result := fHostVersion;
end;

function TZAdoConnection.GetServerProvider: TZServerProvider;
begin
  Result := fServerProvider;
end;

procedure TZAdoConnection.HandleErrorOrWarning(
  LoggingCategory: TZLoggingCategory; const E: Exception;
  const Sender: IImmediatelyReleasable; const LogMsg: SQLString);
var FormatStr, ErrorString: String;
begin
  if E is EOleException then with E as EOleException do begin
    if DriverManager.HasLoggingListener then
      LogError(LoggingCategory, ErrorCode, Sender, LogMsg, Message);
    if AddLogMsgToExceptionOrWarningMsg and (LogMsg <> '') then
      if LoggingCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
      then FormatStr := SSQLError3
      else FormatStr := SSQLError4
    else FormatStr := SSQLError2;
    if AddLogMsgToExceptionOrWarningMsg and (LogMsg <> '')
    then ErrorString := Format(FormatStr, [Message, ErrorCode, LogMsg])
    else ErrorString := Format(FormatStr, [Message, ErrorCode]);
    raise EZSQLException.CreateWithCode(ErrorCode, ErrorString);
  end else raise Exception(E.ClassType).Create(E.Message);
end;

initialization
  AdoCoInitialized := 0;
  AdoDriver := TZAdoDriver.Create;
  DriverManager.RegisterDriver(AdoDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(AdoDriver);
  AdoDriver := nil;
{$ENDIF ZEOS_DISABLE_ADO}
end.
