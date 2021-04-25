{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

(*
constributors:
  Mark Ford(MJFShark)
  Miab3
*)

unit ZDbcOleDB;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX, Windows,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZDbcIntfs, ZDbcConnection, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZCompatibility, ZDbcOleDBUtils,
  ZPlainOleDBDriver, ZOleDBToken;

type
  /// <summary>Implements an OleDB Database Driver.</summary>
  TZOleDBDriver = class(TZAbstractDriver)
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
  end;

  /// <summary>Defines an OleDB specific connection interface</summary>
  IZOleDBConnection = interface(IZConnection)
    ['{35A72582-F758-48B8-BBF7-3267EEBC9750}']
    /// <summary>Get the current session interface as IUnknown.</summary>
    /// <returns>the session interface or nil if not connected.</returns>
    function GetSession: IUnknown;
    /// <summary>Creates a new command as ICommandText.</summary>
    /// <returns>the created command interface.</returns>
    function CreateCommand: ICommandText;
    /// <summary>Get the current MAlloc interface as IMalloc.</summary>
    /// <returns>the Malloc interface.</returns>
    function GetMalloc: IMalloc;
    /// <summary>Check if multiple active resultsets are supported</summary>
    /// <returns><c>true</c> if MARS is supported.</returns>
    function SupportsMARSConnection: Boolean;
    /// <summary>Get the address of the static connecetion TByteBuffer</summary>
    /// <returns>the address of the connection buffer</returns>
    function GetByteBufferAddress: PByteBuffer;
    /// <summary>Handle an error or a warning. Note: this method should be
    ///  called only if the status is in error or warning range.</summary>
    /// <param>"Status" the current status received by a call of any OleDB
    ///  interface method</param>
    /// <param>"LoggingCategory" the logging category used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"LogMessage" the logging message used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"Sender" the calling interface which may release the resources if
    ///  a connection loss happens</param>
    /// <param>"aStatus" a binding status array used for extended binding
    /// failures or nil.</param>
    procedure HandleErrorOrWarning(Status: HRESULT; LoggingCategory: TZLoggingCategory;
      const LogMessage: SQLString; const Sender: IImmediatelyReleasable;
      const aStatus: TDBBINDSTATUSDynArray = nil);
  end;

  /// <summary>Implements an OleDB specific connection object.</summary>
  TZOleDBConnection = class(TZAbstractSingleTxnConnection, IZConnection,
    IZOleDBConnection, IZTransaction)
  private
    FMalloc: IMalloc;
    FDBInitialize: IDBInitialize;
    FDBCreateCommand: IDBCreateCommand;
    FRetaining: Boolean;
    FpulTransactionLevel: ULONG;
    FSupportsMARSConnnection: Boolean;
    FServerProvider: TZServerProvider;
    fTransaction: ITransactionLocal;
    fCatalog: String;
    FAutoCommitTIL: ISOLATIONLEVEL;
    FRestartTransaction: Boolean;
    FLastWarning: EZSQLWarning;
    FHostVersion: Integer;
    /// <summary>Sets provider properties</summary>
    procedure SetProviderProps(DBinit: Boolean);
  protected
    function OleDbGetDBPropValue(const APropIDs: array of DBPROPID): string; overload;
    function OleDbGetDBPropValue(APropID: DBPROPID): Integer; overload;
    procedure InternalSetTIL(Level: TZTransactIsolationLevel);
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a UTF16 encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; override;
    /// <summary>Releases a Connection's database and resources immediately
    ///  instead of waiting for them to be automatically released.</summary>
    ///  Note: A Connection is automatically closed when it is garbage
    ///  collected. Certain fatal errors also result in a closed Connection.</summary>
    procedure InternalClose; override;
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
    function CreateStatementWithParams(Params: TStrings): IZStatement;
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
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open; override;
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
    {procedure SetReadOnly(ReadOnly: Boolean); override; }
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
    /// <summary>Returns the Connection's current catalog name.</summary>
    /// <returns>the current catalog name or an empty string.</returns>
    function GetCatalog: string; override;
    /// <summary>Returns the first warning reported by calls on this Connection.</summary>
    /// <remarks>Subsequent warnings will be chained to this EZSQLWarning.</remarks>
    /// <returns>the first SQLWarning or nil.</returns>
    function GetWarnings: EZSQLWarning; override;
    /// <summary>Clears all warnings reported for this <c>Connection</c> object.
    ///  After a call to this method, the method <c>getWarnings</c> returns nil
    ///  until a new warning is reported for this Connection.</summary>
    procedure ClearWarnings; override;
    /// <summary>Returns the ServicerProvider for this connection. For OLEDB
    ///  the connection must be opened to determine the provider. Otherwise
    ///  the provider is tested against the driver names</summary>
    /// <returns>the ServerProvider or spUnknown if not known.</returns>
    function GetServerProvider: TZServerProvider; override;
    /// <author>fduenas</author>
    /// <summary>Gets the host's full version number. Initially this should be 0.
    ///  The format of the version returned must be XYYYZZZ where
    ///  X   = Major version
    ///  YYY = Minor version
    ///  ZZZ = Sub version</summary>
    /// <returns>this server's full version number</returns>
    function GetHostVersion: Integer; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
  public { implement IZOleDBConnection }
    /// <summary>Get the current session interface as IUnknown.</summary>
    /// <returns>the session interface or nil if not connected.</returns>
    function GetSession: IUnknown;
    /// <summary>Creates a new command as ICommandText.</summary>
    /// <returns>the created command interface.</returns>
    function CreateCommand: ICommandText;
    /// <summary>Get the current MAlloc interface as IMalloc.</summary>
    /// <returns>the Malloc interface.</returns>
    function GetMalloc: IMalloc;
    /// <summary>Check if multiple active resultsets are supported</summary>
    /// <returns><c>true</c> if MARS is supported.</returns>
    function SupportsMARSConnection: Boolean;
    /// <summary>Handle an error or a warning. Note: this method should be
    ///  called only if the status is in error or warning range.</summary>
    /// <param>"Status" the current status received by a call of any OleDB
    ///  interface method</param>
    /// <param>"LoggingCategory" the logging category used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"LogMessage" the logging message used to log the error or
    ///  warning if a listenter is registered on the driver manager</param>
    /// <param>"Sender" the calling interface which may release the resources if
    ///  a connection loss happens</param>
    /// <param>"aStatus" a binding status array used for extended binding
    /// failures or nil.</param>
    procedure HandleErrorOrWarning(Status: HRESULT; LoggingCategory:  TZLoggingCategory;
      const LogMessage: SQLString; const Sender: IImmediatelyReleasable;
      const aStatus: TDBBINDSTATUSDynArray = nil);
  end;

var
  OleDBDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses TypInfo,
  ZSysUtils, ZDbcUtils, ZEncoding, ZMessages, ZFastCode, ZClasses,
  ZPostgreSqlAnalyser, ZPostgreSqlToken, ZSybaseAnalyser, ZSybaseToken,
  ZInterbaseAnalyser, ZInterbaseToken, ZMySqlAnalyser, ZMySqlToken,
  ZOracleAnalyser, ZOracleToken,
  ZDbcOleDBMetadata, ZDbcOleDBStatement, ZDbcProperties;

{ TZOleDBDriver }

constructor TZOleDBDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZOleDBPlainDriver.Create));
end;

function TZOleDBDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOleDBConnection.Create(Url);
end;

function TZOleDBDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOleDBTokenizer.Create;
end;

var
  OleDBCoinitialized: integer;

procedure CoInit;
begin
  inc(OleDBCoInitialized);
  if OleDBCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(OleDBCoinitialized>0);
  dec(OleDBCoinitialized);
  if OleDBCoinitialized=0 then
    CoUninitialize;
end;

{ TZOleDBConnection }

const
  TIL: array[TZTransactIsolationLevel] of ISOLATIONLEVEL =
   ( ISOLATIONLEVEL_CHAOS,
     ISOLATIONLEVEL_READUNCOMMITTED,
     ISOLATIONLEVEL_READCOMMITTED,
     ISOLATIONLEVEL_REPEATABLEREAD,
     ISOLATIONLEVEL_SERIALIZABLE);

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const sourc:Longword):OleVariant" marked as inline is not inlined}{$ENDIF}
procedure TZOleDBConnection.InternalSetTIL(Level: TZTransactIsolationLevel);
var
  rgDBPROPSET_DBPROPSET_SESSION: TDBProp;
  prgPropertySets: TDBPROPSET;
  SessionProperties: ISessionProperties;
  Status: HResult;
begin
  SessionProperties := nil;
  if (FDBCreateCommand.QueryInterface(IID_ISessionProperties, SessionProperties) = S_OK) then begin
    prgPropertySets.cProperties     := 1;
    prgPropertySets.guidPropertySet := DBPROPSET_SESSION;
    prgPropertySets.rgProperties    := @rgDBPROPSET_DBPROPSET_SESSION;
    rgDBPROPSET_DBPROPSET_SESSION.dwPropertyID := DBPROP_SESS_AUTOCOMMITISOLEVELS;
    rgDBPROPSET_DBPROPSET_SESSION.dwOptions    := DBPROPOPTIONS_REQUIRED;
    rgDBPROPSET_DBPROPSET_SESSION.colid        := DB_NULLID;
    rgDBPROPSET_DBPROPSET_SESSION.vValue       := TIL[Level];
    Status := SessionProperties.SetProperties(1, @prgPropertySets);
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcTransaction, 'SET TRANSACTION ISOLATION LEVEL', Self);
    FAutoCommitTIL := TIL[Level];
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

destructor TZOleDBConnection.Destroy;
begin
  try
    inherited Destroy; // call Disconnect;
  finally
    FDBCreateCommand := nil;
    fDBInitialize := nil;
    fMalloc := nil;
    CoUninit;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "pParams" does not seem to be initialized} {$ENDIF}
procedure TZOleDBConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var Cmd: ICommandText;
  pParams: TDBPARAMS;
  Status: HResult;
begin
  Cmd := CreateCommand;
  FillChar(pParams, SizeOf(TDBParams), #0);
  Status := Cmd.SetCommandText(DBGUID_DEFAULT, Pointer(SQL));
  {$IFNDEF UNICODE}
  if (Status <> S_OK) or DriverManager.HasLoggingListener then
    FLogMessage := ZUnicodeToRaw(SQL, zCP_UTF8);
  {$ENDIF}
  if Status <> S_OK then
    HandleErrorOrWarning(Status, LoggingCategory, {$IFDEF UNICODE}SQL{$ELSE}FLogMessage{$ENDIF}, Self);
  Status := Cmd.Execute(nil, DB_NULLGUID,pParams,nil,nil);
  if Status <> S_OK then
     HandleErrorOrWarning(Status, LoggingCategory, {$IFDEF UNICODE}SQL{$ELSE}FLogMessage{$ENDIF}, Self, nil)
  else if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, URL.Protocol, {$IFDEF UNICODE}SQL{$ELSE}FLogMessage{$ENDIF});
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOleDBConnection.SetAutoCommit(Value: Boolean);
var Status: HResult;
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      while FpulTransactionLevel > 0 do begin
        Status := fTransaction.Abort(nil, FRetaining, False);
        if Status <> S_OK then
          HandleErrorOrWarning(Status, lcTransaction, 'Rollback Transaction', Self);
        Dec(FpulTransactionLevel);
      end;
      fTransaction := nil;
      if FAutoCommitTIL <> TIL[TransactIsolationLevel] then
        InternalSetTIL(TransactIsolationLevel);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

procedure TZOleDBConnection.SetCatalog(const Catalog: string);
begin
  if Catalog <> '' then
    if GetServerProvider in [spASE,spMSSQL, spMySQL] then begin
      CreateStatementWithParams(info).ExecuteUpdate('use '+Catalog);
      fCatalog := Catalog;
    end;
end;

procedure TZOleDBConnection.SetProviderProps(DBinit: Boolean);
const
  DBPROPSET_SQLSERVERDBINIT:      TGUID = '{5cf4ca10-ef21-11d0-97e7-00c04fc2ad98}';
  //{%H-}DBPROPSET_SQLSERVERDATASOURCE:  TGUID = '{28efaee4-2d2c-11d1-9807-00c04fc2ad98}'; unused
  SSPROP_INIT_PACKETSIZE	       = 9;
var
  DBProps: IDBProperties;
  rgDBPROPSET: array[0..10] of TDBProp;
  rgDBPROPSET_SQLSERVERDBINIT: TDBProp;
  rgDBPROPSET_DATASOURCE: TDBProp;
  PropertySets: array[0..2] of TDBPROPSET;
  cPropertySets: ULONG;
  Status: HResult;
  {$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const source:smallInt):OleVariant" marked as inline is not inlined}{$ENDIF}
  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
  {$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}
begin
//some examples: https://blogs.msdn.microsoft.com/sqlnativeclient/2009/05/06/sql-server-native-client-connection-strings-and-ole-db/
  DBProps := nil; //init
  if Succeeded(FDBInitialize.QueryInterface(IID_IDBProperties, DBProps)) then
  begin
    if DBinit then
    begin
      cPropertySets := 2;
      PropertySets[0].cProperties     := 0; //init
      PropertySets[0].guidPropertySet := DBPROPSET_DBINIT;
      PropertySets[0].rgProperties    := @rgDBPROPSET[0];
      PropertySets[1].cProperties     := 0; //init
      PropertySets[1].guidPropertySet := DBPROPSET_DATASOURCE;
      PropertySets[1].rgProperties    := @rgDBPROPSET_DATASOURCE;
      PropertySets[2].cProperties     := 0; //init
      PropertySets[2].guidPropertySet := DBPROPSET_SQLSERVERDBINIT;
      PropertySets[2].rgProperties    := @rgDBPROPSET_SQLSERVERDBINIT;
      //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
      //Indicates the number of seconds before the source initialization times out
      SetProp(PropertySets[0], DBPROP_INIT_TIMEOUT,       StrToIntDef(Info.Values[ConnProps_Timeout], 0));
      //Indicates the number of seconds before a request and command execution, times out
      SetProp(PropertySets[0], DBPROP_INIT_GENERALTIMEOUT,StrToIntDef(Info.Values[ConnProps_Timeout], 0));
      //Force Multiple connections -> prevent transactional issues with IDBSchemaRowSet etc
      //http://support2.microsoft.com/default.aspx?scid=kb;en-us;272358
      SetProp(PropertySets[1], DBPROP_MULTIPLECONNECTIONS,ZVARIANT_TRUE);
      //supported for MSSQL only!!!
      if (Info.Values[ConnProps_TDSPacketSize] <> '') then
      begin
        SetProp(PropertySets[2], SSPROP_INIT_PACKETSIZE, StrToIntDef(Info.Values[ConnProps_TDSPacketSize], 0));
        cPropertySets := 3;
      end;
    end
    else
      // don't work? Bad sequence when to call?
      if (FServerProvider = spMSSQL) then
      begin
        PropertySets[0].cProperties     := 0; //init
        PropertySets[0].guidPropertySet := DBPROPSET_DATASOURCE;
        PropertySets[0].rgProperties    := @rgDBPROPSET[0];
        SetProp(PropertySets[0], DBPROP_MULTIPLECONNECTIONS,ZVARIANT_FALSE);
        cPropertySets := 1;
      end
      else
        cPropertySets := 0;
    try
      Status := DBProps.SetProperties(cPropertySets,@PropertySets[0]);
      if Failed(Status) then
        HandleErrorOrWarning(Status, lcOther, 'SetProperties', Self);
    finally
      DBProps := nil;
    end;
  end;
end;

function TZOleDBConnection.StartTransaction: Integer;
var Status: HResult;
  S: String;
begin
  if Closed then
    Open;
  AutoCommit := False;
  if FpulTransactionLevel = 0 then begin
    if not Assigned(fTransaction) then
      OleCheck(FDBCreateCommand.QueryInterface(IID_ITransactionLocal,fTransaction));
    Status := fTransaction.StartTransaction(TIL[TransactIsolationLevel],0,nil,@FpulTransactionLevel);
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcTransaction, 'Start Transaction', Self);
    Result := FpulTransactionLevel;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    if cSavePointSyntaxW[fServerProvider][spqtSavePoint] = '' then
      raise EZSQLException.Create(SUnsupportedOperation);
    ExecuteImmediat(cSavePointSyntaxW[fServerProvider][spqtSavePoint]+ {$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
end;

// returns property value(-s) from Data Source Information group as string,
//where values are delimited using space
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const source:OleVariant):AnsiString" marked as inline is not inlined}{$ENDIF}
function TZOleDBConnection.OleDbGetDBPropValue(const APropIDs: array of DBPROPID): string;
var
  DBProperties: IDBProperties;
  PropIDSet: TDBPROPIDSET;
  prgPropertySets: PDBPropSet;
  PropSet: TDBPropSet;
  nPropertySets: ULONG;
  i: Integer;
  s: string;
  Status: HResult;
begin
  Result := '';
  DBProperties := nil;
  OleCheck(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties));
  try
    PropIDSet.rgPropertyIDs   := @APropIDs;
    PropIDSet.cPropertyIDs    := High(APropIDs)+1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    Status := DBProperties.GetProperties(1, @PropIDSet, nPropertySets, prgPropertySets );
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcOther, 'IID_IDBProperties.GetProperties', Self, nil);
    PropSet := prgPropertySets^;
    for i := 0 to PropSet.cProperties-1 do begin
      if PropSet.rgProperties^[i].dwStatus <> DBPROPSTATUS(DBPROPSTATUS_OK) then
        Continue;
      if Result <> '' then
        if PropSet.rgProperties^[i].dwPropertyID = DBPROP_DBMSVER then
          Result := Result + ' Release '
        else
          Result := Result + ' ';
      s := PropSet.rgProperties^[i].vValue;
      Result := Result + s;
    end;
    // free and clear elements of PropIDSet
    for i := 0 to PropSet.cProperties-1 do
      VariantClear(PropSet.rgProperties^[i].vValue);
    FMAlloc.Free(PropSet.rgProperties);
    FMAlloc.Free(prgPropertySets); //free prgPropertySets
  finally
    DBProperties := nil;
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function TZOleDBConnection.GetCatalog: string;
begin
  Result := fCatalog;
end;

function TZOleDBConnection.GetHostVersion: Integer;
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

function TZOleDBConnection.CreateStatementWithParams(
  Params: TStrings): IZStatement;
begin
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, '', Params);
end;

function TZOleDBConnection.GetServerProvider: TZServerProvider;
begin
  Result := FServerProvider;
end;

function TZOleDBConnection.GetSession: IUnknown;
begin
  Result := FDBCreateCommand;
end;

function TZOleDBConnection.GetStatementAnalyser: IZStatementAnalyser;
begin
  case FServerProvider of
    //spUnknown, spMSSQL, spMSJet,
    spOracle: Result := TZOracleStatementAnalyser.Create;
    spMSSQL, spASE, spASA: Result := TZSybaseStatementAnalyser.Create;
    spPostgreSQL: Result := TZPostgreSQLStatementAnalyser.Create;
    spIB_FB: Result := TZInterbaseStatementAnalyser.Create;
    spMySQL: Result := TZMySQLStatementAnalyser.Create;
    //spNexusDB, spSQLite, spDB2, spAS400,
    //spInformix, spCUBRID, spFoxPro
    else Result := TZGenericStatementAnalyser.Create;
  end;
end;

function TZOleDBConnection.GetTokenizer: IZTokenizer;
begin
  case FServerProvider of
    //spUnknown, spMSJet,
    spOracle: Result := TZOracleTokenizer.Create;
    spMSSQL, spASE, spASA: Result := TZSybaseTokenizer.Create;
    spPostgreSQL: Result := TZPostgreSQLTokenizer.Create;
    spIB_FB: Result := TZInterbaseTokenizer.Create;
    spMySQL: Result := TZMySQLTokenizer.Create;
    //spNexusDB, spSQLite, spDB2, spAS400,
    //spInformix, spCUBRID, spFoxPro
    else Result := TZOleDBTokenizer.Create;
  end;
end;

function TZOleDBConnection.GetWarnings: EZSQLWarning;
begin
  Result := FLastWarning;
end;

procedure TZOleDBConnection.HandleErrorOrWarning(Status: HRESULT;
  LoggingCategory: TZLoggingCategory; const LogMessage: SQLString;
  const Sender: IImmediatelyReleasable; const aStatus: TDBBINDSTATUSDynArray);
var
  OleDBErrorMessage: UnicodeString;
  SQLState: SQLString;
  ErrorInfo, ErrorInfoDetails: IErrorInfo;
  SQLErrorInfo: ISQLErrorInfo;
  MSSQLErrorInfo: ISQLServerErrorInfo;
  ErrorRecords: IErrorRecords;
  SSErrorPtr: PMSErrorInfo;
  i, ErrorCode, FirstErrorCode: Integer;
  ErrorCount: ULONG;
  WS: WideString;
  StringsBufferPtr: PWideChar;
  Writer: TZUnicodeSQLStringWriter;
  MessageAdded: Boolean;
  Error: EZSQLThrowable;
  ExeptionClass: EZSQLThrowableClass;
  FormatStr: String;
begin
  if Status = S_OK then Exit;
  // get OleDB specific error information
  Writer := TZUnicodeSQLStringWriter.Create(1024);
  ErrorInfo := nil;
  Error := nil;
  try
    if IsError(Status) then
      if Status < 0 //avoid range check error for some negative unknown errors
      then OleDBErrorMessage := 'OLEDB Error '
      else OleDBErrorMessage := UnicodeString(SysErrorMessage(Status))
    else OleDBErrorMessage := 'OLEDB Warning ';
    SQLState := '';
    FirstErrorCode := Status;
    GetErrorInfo(0,ErrorInfo);
    if Assigned(ErrorInfo) then begin
      ErrorRecords := ErrorInfo as IErrorRecords;
      ErrorRecords.GetRecordCount(ErrorCount);
      for i := 0 to ErrorCount-1 do begin
        MessageAdded := False;
        { get all error interface we know }
        SQLErrorInfo := nil;
        if Failed(ErrorRecords.GetCustomErrorObject(i, IID_ISQLErrorInfo, IUnknown(SQLErrorInfo))) then
          SQLErrorInfo := nil;
        ErrorInfoDetails := nil;
        if Failed(ErrorRecords.GetErrorInfo(i,GetSystemDefaultLCID,ErrorInfoDetails)) then
          ErrorInfoDetails := nil;
        MSSQLErrorInfo := nil;
        if Failed(ErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, IUnknown(MSSQLErrorInfo))) then
          MSSQLErrorInfo := nil;

        { get common error info: }
        if (SQLErrorInfo <> nil) then try
          WS := '';
          SQLErrorInfo.GetSQLInfo(WS, ErrorCode );
          if I = 0 then begin
            FirstErrorCode := ErrorCode;
            SQLState := {$IFNDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(WS);
          end;
          Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
          Writer.AddText('SQLState: ', OleDBErrorMessage);
          Writer.AddText(Pointer(WS), Length(WS), OleDBErrorMessage);
          Writer.AddText(' Native Error: ', OleDBErrorMessage);
          Writer.AddOrd(ErrorCode, OleDBErrorMessage);
          WS := '';
        finally
          SQLErrorInfo := nil;
        end;
        if (ErrorInfoDetails <> nil) then try
          if Succeeded(ErrorInfoDetails.GetDescription(WS)) and (WS <> '') then begin
            Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
            Writer.AddText('Error message: ', OleDBErrorMessage);
            Writer.AddText(Pointer(WS), Length(WS), OleDBErrorMessage);
            MessageAdded := True;
          end;
          if Succeeded(ErrorInfoDetails.GetSource(WS)) and (WS <> '') then begin
            Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
            Writer.AddText('Source: ', OleDBErrorMessage);
            Writer.AddText(Pointer(WS), Length(WS), OleDBErrorMessage);
            WS := '';
          end;
        finally
          ErrorInfoDetails := nil;
          //OleCheck(SetErrorInfo(0, ErrorInfoDetails));
          WS := '';
        end;
        if Assigned(MSSQLErrorInfo) then try
          Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
          Writer.AddText('SQLServer details: ', OleDBErrorMessage);
          SSErrorPtr := nil;
          StringsBufferPtr:= nil;
          try //try use a SQL Server error interface
            if Succeeded(MSSQLErrorInfo.GetErrorInfo(SSErrorPtr, StringsBufferPtr)) and Assigned(SSErrorPtr) then begin
              if not MessageAdded and (SSErrorPtr^.pwszMessage <> nil) and (PWord(SSErrorPtr^.pwszMessage)^ <> 0) then begin
                Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
                Writer.AddText('Error message: ', OleDBErrorMessage);
                Writer.AddText(SSErrorPtr^.pwszMessage, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(SSErrorPtr^.pwszMessage), OleDBErrorMessage);
              end;
              if (SSErrorPtr^.pwszServer <> nil) and (PWord(SSErrorPtr^.pwszServer)^ <> 0) then begin
                Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
                Writer.AddText('Server: ', OleDBErrorMessage);
                Writer.AddText(SSErrorPtr^.pwszServer, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(SSErrorPtr^.pwszServer), OleDBErrorMessage);
              end;
              if (SSErrorPtr^.pwszProcedure <> nil) and (PWord(SSErrorPtr^.pwszProcedure)^ <> 0) then begin
                Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
                Writer.AddText('Procedure: ', OleDBErrorMessage);
                Writer.AddText(SSErrorPtr^.pwszProcedure, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(SSErrorPtr^.pwszProcedure), OleDBErrorMessage);
              end;
              Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
              Writer.AddText('Line: ', OleDBErrorMessage);
              Writer.AddOrd(SSErrorPtr^.wLineNumber, OleDBErrorMessage);
              Writer.AddText(', Error state: ', OleDBErrorMessage);
              Writer.AddOrd(SSErrorPtr^.bState, OleDBErrorMessage);
              Writer.AddText(', Severity: ', OleDBErrorMessage);
              Writer.AddOrd(SSErrorPtr^.bClass, OleDBErrorMessage);
            end;
          finally
            if Assigned(SSErrorPtr) then CoTaskMemFree(SSErrorPtr);
            if Assigned(StringsBufferPtr) then CoTaskMemFree(StringsBufferPtr);
          end
        finally
          MSSQLErrorInfo := nil;
        end;
      end;
    end;
    // retrieve binding information from Status[]
    if aStatus <> nil then begin
      MessageAdded := False;
      for i := 0 to high(aStatus) do
        if aStatus[i]<>ZPlainOleDBDriver.DBBINDSTATUS_OK then Begin
          if not MessageAdded then begin
            Writer.AddLineFeedIfNotEmpty(OleDBErrorMessage);
            Writer.AddText('Binding failure(s): ', OleDBErrorMessage);
            MessageAdded := True;
          end;
          Writer.AddText('Status[', OleDBErrorMessage);
          Writer.AddOrd(i{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, OleDBErrorMessage);
          Writer.AddText(']="', OleDBErrorMessage);
          if aStatus[i]<=cardinal(high(TOleDBBindStatus)) then begin
            FlogMessage := GetEnumName(TypeInfo(TOleDBBindStatus),aStatus[i]);
            {$IFDEF UNICODE}
            Writer.AddText(FlogMessage, OleDBErrorMessage);
            {$ELSE}
            Writer.AddAscii7Text(Pointer(FlogMessage), Length(FlogMessage), OleDBErrorMessage);
            {$ENDIF}
          end else
            Writer.AddOrd(aStatus[i], OleDBErrorMessage);
          Writer.AddChar('"', OleDBErrorMessage);
          Writer.AddChar(',', OleDBErrorMessage);
        end;
      Writer.CancelLastComma(OleDBErrorMessage);
    end;
    Writer.Finalize(OleDBErrorMessage);
    {$IFNDEF UNICODE}
    FlogMessage := ZUnicodeToRaw(OleDBErrorMessage, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
      {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF});
    {$ELSE}
    FlogMessage := OleDBErrorMessage;
    {$ENDIF}
    OleDBErrorMessage := '';
    if SQLState = '' then begin
      FirstErrorCode := Status;
      SQLState := IntToHex(Status,8);
    end;
    if DriverManager.HasLoggingListener then
      LogError(LoggingCategory, ErrorCode, Sender, LogMessage, FLogMessage);
    if IsError(Status) then
      if (SQLState = '08S01') and (GetServerProvider = spMSSQL)
      then ExeptionClass := EZSQLConnectionLost
      else ExeptionClass := EZSQLException
    else ExeptionClass := EZSQLWarning;
    if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
      if LoggingCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
      then FormatStr := SSQLError3
      else FormatStr := SSQLError4
    else FormatStr := SSQLError2;
    if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
    then FLogMessage := Format(FormatStr, [FLogMessage, FirstErrorCode, LogMessage])
    else FLogMessage := Format(FormatStr, [FLogMessage, FirstErrorCode]);
    Error := ExeptionClass.CreateWithCodeAndStatus(ErrorCode, SQLState, FLogMessage);
    FLogMessage := '';
    if ExeptionClass = EZSQLWarning then begin//that's a Warning
      ClearWarnings;
      if not RaiseWarnings or (LoggingCategory = lcConnect) then begin
        FLastWarning := EZSQLWarning(Error);
        Error := nil;
      end;
    end else if ExeptionClass = EZSQLConnectionLost then begin
      if (Sender <> nil)
      then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
      else ReleaseImmediat(Self, EZSQLConnectionLost(Error));
    end;
  finally
    FreeAndNil(Writer);
    OleCheck(SetErrorInfo(0, nil));
  end;
  if Error <> nil then
    raise Error;
end;

function TZOleDBConnection.CreateCommand: ICommandText;
var Status: HResult;
begin
  Result := nil;
  Status := FDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(Result));
  if Status <> S_OK then
    HandleErrorOrWarning(Status, lcOther, 'create command', Self, nil);
end;

function TZOleDBConnection.GetMalloc: IMalloc;
begin
  Result := FMalloc;
end;

function TZOleDBConnection.SupportsMARSConnection: Boolean;
begin
  Result := FSupportsMARSConnnection;
end;

procedure TZOleDBConnection.SetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  if (TransactIsolationLevel <> Level) then begin
    if not Closed then begin
      if not AutoCommit then
        raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
      InternalSetTIL(Level);
    end;
    TransactIsolationLevel := Level;
  end;
end;

procedure TZOleDBConnection.AfterConstruction;
begin
  CoInit;
  OleCheck(CoGetMalloc(1,fMalloc));
  FMetadata := TOleDBDatabaseMetadata.Create(Self, URL);
  Inherited SetAutoCommit(True);
  FHostVersion := -1;
  inherited AfterConstruction;
end;

procedure TZOleDBConnection.ClearWarnings;
begin
  if FLastWarning <> nil then
    FreeAndNil(FLastWarning);
end;

procedure TZOleDBConnection.Commit;
var S: UnicodeString;
  Status: HResult;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  if FSavePoints.Count > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtCommit];
    if S <> '' then begin
      S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    Status := fTransaction.Commit(FRetaining,XACTTC_SYNC,0);
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcTransaction, 'Commit Transaction', Self);
    Dec(FpulTransactionLevel);
    if (FpulTransactionLevel = 0) and not FRetaining then begin
      fTransaction := nil;
      AutoCommit := True;
      if FRestartTransaction then
        StartTransaction;
    end;
  end;
end;

procedure TZOleDBConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  FpulTransactionLevel := 0;
  fTransaction := nil;
  FMalloc := nil;
  FDBInitialize := nil;
  FDBCreateCommand := nil;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZOleDBConnection.Rollback;
var S: UnicodeString;
  Status: HResult;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtRollback];
    if S <> '' then begin
      S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    Status := fTransaction.Abort(nil, FRetaining, False);
    if Status < S_OK then
      HandleErrorOrWarning(Status, lcTransaction, 'Rollback Transaction', Self);
    Dec(FpulTransactionLevel);
    if (FpulTransactionLevel = 0) and not FRetaining then begin
      fTransaction := nil;
      AutoCommit := True;
      if FRestartTransaction then
        StartTransaction;
    end;
  end;
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator:=(const source:OleVariant):LongInt" marked as inline is not inlined}{$ENDIF}
function TZOleDBConnection.OleDbGetDBPropValue(APropID: DBPROPID): Integer;
var
  DBProperties: IDBProperties;
  PropIDSet: TDBPROPIDSET;
  prgPropertySets: PDBPropSet;
  PropSet: TDBPropSet;
  nPropertySets: ULONG;
  i: Integer;
  Status: HResult;
begin
  Result := 0;
  DBProperties := nil;
  if FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties) = S_OK then try
    PropIDSet.rgPropertyIDs   := @APropID;
    PropIDSet.cPropertyIDs    := 1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    Status := DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets );
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcOther, 'IID_IDBProperties.GetProperties', Self, nil);
    PropSet := prgPropertySets^;
    for i := 0 to PropSet.cProperties-1 do begin
      if PropSet.rgProperties^[i].dwStatus <> DBPROPSTATUS(DBPROPSTATUS_OK) then
        Continue;
      Result := PropSet.rgProperties^[i].vValue;
    end;
    // free and clear elements of PropIDSet
    for i := 0 to PropSet.cProperties-1 do
      VariantClear(PropSet.rgProperties^[i].vValue);
    FMAlloc.Free(PropSet.rgProperties);
    FMAlloc.Free(prgPropertySets); //free prgPropertySets
  finally
    DBProperties := nil;
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

procedure TZOleDBConnection.Open;
var
  DataInitialize : IDataInitialize;
  ConnectStrings: TStrings;
  ConnectString: UnicodeString;
  FDBCreateSession: IDBCreateSession;
  Status: HResult;
begin
  if not Closed then
    Exit;
  try
    // retrieve initialization parameters from connection string
    DataInitialize := CreateComObject(CLSID_DataLinks) as IDataInitialize;
    ConnectStrings := SplitString(DataBase, ';');
    //https://msdn.microsoft.com/de-de/library/ms131686%28v=sql.120%29.aspx
    FSupportsMARSConnnection := StrToBoolEx(ConnectStrings.Values[ConnProps_MarsConn]);
    if StrToBoolEx(ConnectStrings.Values[ConnProps_TrustedConnection]) then
      ConnectString := {$IFNDEF UNICODE}UnicodeString{$ENDIF}(DataBase)
    else
    begin
      ConnectStrings.Values[ConnProps_UserId] := User;
      ConnectStrings.Values[ConnProps_Password] := PassWord;
      ConnectString := {$IFNDEF UNICODE}UnicodeString{$ENDIF}(ComposeString(ConnectStrings, ';'));
    end;
    FServerProvider := ProviderNamePrefix2ServerProvider(ConnectStrings.Values[ConnProps_Provider]);
    fCatalog := ConnectStrings.Values[ConnProps_Initial_Catalog];
    ConnectStrings.Free;
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      Pointer(ConnectString), IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil; //no longer required!
    if FServerProvider <> spMSJet then
      SetProviderProps(True); //set's timeout values
    // open the connection to the DB
    Status := fDBInitialize.Initialize;
    if Status <> S_OK then
      HandleErrorOrWarning(Status, lcConnect, 'IID_IDBInitialize.Initialize', Self, nil);
    OleCheck(fDBInitialize.QueryInterface(IID_IDBCreateSession, FDBCreateSession));
    //some Providers do NOT support commands, so let's check if we can use it
    OleCheck(FDBCreateSession.CreateSession(nil, IID_IDBCreateCommand, IUnknown(FDBCreateCommand)));
    FDBCreateSession := nil; //no longer required!
    //if FServerProvider = spMSSQL then
      //SetProviderProps(False); //provider properties -> don't work??
    inherited Open;
    if TransactIsolationLevel = tiNone then
      Inherited SetTransactionIsolation(GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation)
    else if TransactIsolationLevel <> GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation then
      InternalSetTIL(TransactIsolationLevel);
    FAutoCommitTIL := TIL[TransactIsolationLevel];
    CheckCharEncoding('CP_UTF16'); //do this by default!
    With (GetMetadata.GetDatabaseInfo as IZOleDBDatabaseInfo) do begin
      InitilizePropertiesFromDBInfo(fDBInitialize, fMalloc);
      DBProviderName2ServerProvider(GetDatabaseProductName, FServerProvider);
    end;
    if (FServerProvider = spMSSQL) then begin
      if (Info.Values[ConnProps_DateWriteFormat] = '') or (Info.Values[ConnProps_DateTimeWriteFormat] = '') then begin
        if (Info.Values[ConnProps_DateWriteFormat] = '') then begin
          ConSettings^.WriteFormatSettings.DateFormat := 'YYYYMMDD';  //ISO format which always is accepted by SQLServer
          ConSettings^.WriteFormatSettings.DateFormatLen := 8;
        end;
        if (Info.Values[ConnProps_DateTimeWriteFormat] = '') then begin
          ConSettings^.WriteFormatSettings.DateTimeFormat := 'YYYY-MM-DDTHH:NN:SS'; //ISO format which always is accepted by SQLServer
          ConSettings^.WriteFormatSettings.DateTimeFormatLen := 19;
        end;
      end;
      { find out which encoding the raw columns do have }
      with CreateStatement.ExecuteQuery(
        'SELECT DATABASEPROPERTYEX('+QuotedStr(fCatalog)+', ''Collation'') as DatabaseCollation, '+
        '  COLLATIONPROPERTY(CAST(DATABASEPROPERTYEX('+QuotedStr(fCatalog)+', ''Collation'') as NVARCHAR(255)), ''Codepage'') as Codepage') do begin
        if Next and not IsNull(FirstDbcIndex) then begin
          ConSettings.ClientCodePage := New(PZCodePage);
          ConSettings.ClientCodePage.Encoding := ceUTF16;//well a "mixed" encoding i have not prepared yet...
          ConSettings.ClientCodePage.IsStringFieldCPConsistent := False;
          ConSettings.ClientCodePage.CP := GetInt(FirstDbcIndex + 1);
          ConSettings.ClientCodePage.Name := GetString(FirstDbcIndex); //@least
          //see Appendix G DBCS/Unicode Mapping Tables
          case ConSettings.ClientCodePage.CP of
            932 {Japanese},
            936 {Simplified Chinese},
            949 {Korean},
            950 {Traditional Chinese}: ConSettings.ClientCodePage.CharWidth := 2;
            else ConSettings.ClientCodePage.CharWidth := 1;
          end;
          FDisposeCodePage := True;
        end;
        Close;
      end;
    end;
    if DriverManager.HasLoggingListener then begin
      FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
      FLogMessage := '';
    end;
    if not AutoCommit then begin
      AutoCommit := True;
      SetAutoCommit(False);;
    end;
  except
    on E: Exception do
    begin
      FDBCreateSession := nil; // mark not connected
      FDBCreateCommand := nil; // mark not connected
      fDBInitialize := nil;
      DataInitialize := nil;
      raise;
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZOleDBConnection.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  if Closed then Open;
  if (FServerProvider = spMSSQL)
  then Result := TZOleDBCallableStatementMSSQL.Create(Self, Name, Params)
  else Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZOleDBConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, SQL, Info);
end;


procedure TZOleDBConnection.InternalClose;
var Status: HResult;
begin
  if Closed or not Assigned(fDBInitialize) then
    Exit;

  FSavePoints.Clear;
  FDBCreateCommand := nil;
  try
    FpulTransactionLevel := 0;
    if not AutoCommit then begin
      AutoCommit := not FRestartTransaction;
      if fTransaction <> nil then begin
        Status := fTransaction.Abort(nil, False, False);
        fTransaction := nil;
        if Status < S_OK then
          HandleErrorOrWarning(Status, lcTransaction, 'Rollback Transaction', Self);
      end;
    end;
    Status := fDBInitialize.Uninitialize;
    if Failed(Status) then
      HandleErrorOrWarning(Status, lcDisconnect, 'DBInitialize.Uninitialize', Self, nil);
  finally
    fDBInitialize := nil;
    FLogMessage := 'DISCONNECT FROM "'+URL.Database+'"';
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcDisconnect, URL.Protocol, FLogMessage);
  end;
end;

initialization
  OleDBDriver := TZOleDBDriver.Create;
  DriverManager.RegisterDriver(OleDBDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OleDBDriver);
  OleDBDriver := nil;
{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
