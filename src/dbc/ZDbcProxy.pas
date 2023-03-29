{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcProxy;

interface

{$I ZDbc.inc}

{$IFDEF ENABLE_PROXY} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcConnection, ZPlainProxyDriver, ZPlainProxyDriverIntf,
  ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser, {$IFNDEF ZEOS73UP}ZURL,{$ENDIF} ZCompatibility;

type

  {** Implements DBC Proxy Driver. }
  TZDbcProxyDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a DBC Proxy specific connection interface. }
  IZDbcProxyConnection = interface (IZConnection)
    ['{C6ACB283-1126-426B-9637-B4CFD430BB33}']
    function GetPlainDriver: IZProxyPlainDriver;
    function GetConnectionInterface: IZDbcProxy;
    function GetDbInfoStr: ZWideString;
  end;

  {** Implements DBC Proxy Database Connection. }

  { TZProxyConnection }

  TZDbcProxyConnection = class({$IFNDEF ZEOS73UP}TZAbstractConnection{$ELSE}TZAbstractSingleTxnConnection{$ENDIF},
    IZConnection, IZTransaction, IZDbcProxyConnection)
  private
    FPlainDriver: IZProxyPlainDriver;
    FConnIntf: IZDbcProxy;
    FDbInfo: ZWideString;

    //shadow properties - they just mirror the values that are set on the server
    FCatalog: String;
    FServerProvider: TZServerProvider;
  protected
    procedure transferProperties(PropName, PropValue: String);
    procedure applyProperties(const Properties: String);
    function encodeProperties(PropName, PropValue: String): String;
  public
    procedure AfterConstruction; override;
    {$IFNDEF ZEOS73UP}
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings): IZPreparedStatement; override;
    {$ELSE}
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
    function PrepareStatementWithParams(const SQL: string; Info: TStrings): IZPreparedStatement;
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
    function PrepareCallWithParams(const SQL: string; Info: TStrings): IZCallableStatement;
    {$ENDIF}
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
    {$IFDEF ZEOS73UP}
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
    function GetTransactionLevel: Integer; override;
    {$ENDIF}

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetAutoCommit(Value: Boolean); override;

    //todo: Get- und SetCatalog implementieren, sowie setter f�r andere Properties:
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetUseMetadata(Value: Boolean); override;
    // AutoEncodeStrings is not supported

    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;

    function GetPlainDriver: IZProxyPlainDriver;
    function GetConnectionInterface: IZDbcProxy;

    function GetServerProvider: TZServerProvider; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
    function GetDbInfoStr: ZWideString;

    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); override;
  end;

var
  {** The common driver manager object. }
  ProxyDriver: IZDriver;

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
implementation
{$IFDEF ENABLE_PROXY} //if set we have an empty unit

uses
  ZSysUtils, ZFastCode, ZEncoding,
  ZDbcProxyMetadata, ZDbcProxyStatement, ZDbcProperties,
  ZPostgreSqlAnalyser, ZPostgreSqlToken, ZSybaseAnalyser, ZSybaseToken,
  ZInterbaseAnalyser, ZInterbaseToken, ZMySqlAnalyser, ZMySqlToken,
  ZOracleAnalyser, ZOracleToken, ZGenericSqlToken,
  ZMessages, Typinfo, ZExceptions
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

const
  ReadOnlyStr = 'readonly';
  CatalogStr = 'catalog';
  TransactionIsolationStr = 'transactionisolation';
  UseMetadataStr = 'usemetadata';
  ServerProviderStr = 'serverprovider';
//  AutoEncodeStr = 'autoencodestrings';

{ TZDbcProxyDriver }

{**
  Constructs this object with default properties.
}
constructor TZDbcProxyDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZProxyBaseDriver.Create));
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
function TZDbcProxyDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZDbcProxyConnection.Create(Url) as IZConnection;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZDbcProxyDriver.GetMajorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZDbcProxyDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZDbcProxyDriver.GetTokenizer: IZTokenizer;
begin
  // todo: return a tokenizer that matches the underlying database -> do we really need to do that?
  // note: this can't work - we don't know which provider will be in use.
  Result := TZTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZDbcProxyDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  // todo: return an analyzer that matches the underlying database -> do we really need to do that?
  // note: this can't work - we don't know which provider will be in use.
  Result := TZGenericStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZDbcProxyConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZDbcProxyConnection.AfterConstruction;
begin
  FTransactionLevel := 0;
  FMetadata := TZProxyDatabaseMetadata.Create(Self, Url);
  FConnIntf := GetPlainDriver.GetLibraryInterface;
  if not assigned(FConnIntf) then
    raise EZSQLException.Create(String(GetPlainDriver.GetLastErrorStr));
  inherited AfterConstruction;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZDbcProxyConnection.Open;
var
  LogMessage: String;
  PropList: WideString;
  MyDbInfo: WideString;
  WsUrl: String; // Webservice URL
begin
  if not Closed then
    Exit;

  WsUrl := URL.Properties.Values[ConnProps_ProxyProtocol];
  if WsUrl = '' then
    WsUrl := 'https';
  WsUrl := WsUrl + '://' + HostName;
  if Port <> 0 then
    WsUrl := WsUrl + ':' + ZFastCode.IntToStr(Port);
  WsUrl := WsUrl + '/services/IZeosProxy';

  LogMessage := 'CONNECT TO "'+ URL.Database + '" AS USER "' + URL.UserName + '"';

  PropList := WideString(encodeProperties('autocommit', BoolToStr(GetAutoCommit, True)));
  FConnIntf.Connect(WideString(User), WideString(Password), WideString(WsUrl), WideString(Database), PropList, MyDbInfo);

  DriverManager.LogMessage(lcConnect, URL.Protocol , LogMessage);
  FDbInfo := MyDbInfo;
  inherited Open;
  applyProperties(String(PropList));
  New(ConSettings.ClientCodePage);
  ConSettings.ClientCodePage.Name := 'UTF16';
  ConSettings.ClientCodePage.Encoding := ceUTF16;
  ConSettings.ClientCodePage.CharWidth := 2;
  ConSettings.ClientCodePage.CP := zCP_UTF16;
end;

{$IFNDEF ZEOS73UP}
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
function TZDbcProxyConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;

  Result := TZDbcProxyPreparedStatement.Create((self as IZConnection), '', Info);
end;

{$ELSE}

function TZDbcProxyConnection.CreateStatementWithParams(Info: TStrings): IZStatement;
begin
  if IsClosed then
    Open;

  Result := TZDbcProxyPreparedStatement.Create((self as IZConnection), '', Info);
end;

{$ENDIF}

{$IFNDEF ZEOS73UP}

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
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZDbcProxyConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;

  Result := TZDbcProxyPreparedStatement.Create((self as IZConnection), SQL, Info);
end;

{$ELSE}

function TZDbcProxyConnection.PrepareStatementWithParams(const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;

  Result := TZDbcProxyPreparedStatement.Create((self as IZConnection), SQL, Info);
end;

{$ENDIF}

{$IFDEF ZEOS73UP}
function TZDbcProxyConnection.PrepareCallWithParams(const SQL: string; Info: TStrings): IZCallableStatement;
begin
  {$IFDEF FPC}
  Result := nil;
  {$ENDIF}
  raise EZSQLException.Create('PrepareCallWithParams is not supported!');
end;
{$ENDIF}

procedure TZDbcProxyConnection.Commit;
begin
  if not Closed then
    if not GetAutoCommit then begin
      FConnIntf.Commit;
      Dec(FTransactionLevel);
      AutoCommit := FTransactionLevel = 0;
    end else
      raise EZSQLException.Create(SInvalidOpInAutoCommit);
end;

procedure TZDbcProxyConnection.Rollback;
begin
  if not Closed then
    if not GetAutoCommit then begin
      FConnIntf.Rollback;
      Dec(FTransactionLevel);
      AutoCommit := FTransactionLevel = 0;
    end else
      raise EZSQLException.Create(SInvalidOpInAutoCommit);
end;

{$IFDEF ZEOS73UP}
function TZDbcProxyConnection.StartTransaction: Integer;
begin
  Result := FConnIntf.StartTransaction;
  AutoCommit := False;
  FTransactionLevel := Result;
end;

function TZDbcProxyConnection.GetTransactionLevel: Integer;
begin
  Result := FTransactionLevel;
end;

{$ENDIF}

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZDbcProxyConnection.InternalClose;
var
  LogMessage: String;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  LogMessage := 'DISCONNECT FROM "' + URL.Database + '"';

  FConnIntf.Disconnect;

  if Assigned(DriverManager) and DriverManager.HasLoggingListener then //thread save
    DriverManager.LogMessage(lcDisconnect, URL.Protocol, LogMessage);
  Dispose(ConSettings.ClientCodePage);
end;

function TZDbcProxyConnection.GetClientVersion: Integer;
begin
  Result := 1000;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZDbcProxyConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> GetAutoCommit then begin
    if not Closed then
      FConnIntf.SetAutoCommit(Value);
      if Value then
        FTransactionLevel := 0
      else
        FTransactionLevel := 1;
    inherited;
  end;
end;

{**
  Gets a SQLite plain driver interface.
  @return a SQLite plain driver interface.
}
function TZDbcProxyConnection.GetPlainDriver: IZProxyPlainDriver;
begin
  if not Assigned(PlainDriver) then
    raise EZSQLException.Create('The f*****g plain driver is not assigned.');
  if fPlainDriver = nil then
    fPlainDriver := PlainDriver as IZProxyPlainDriver;
  Result := fPlainDriver;
end;

function TZDbcProxyConnection.GetConnectionInterface: IZDbcProxy;
begin
  Result := FConnIntf;
end;

function TZDbcProxyConnection.GetHostVersion: Integer;
begin
  Result := 0;
end;

procedure TZDbcProxyConnection.applyProperties(const Properties: String);
var
  List: TStringList;
  TempStr: String;
  TransactionIsolation: TZTransactIsolationLevel;
begin
  List := TStringList.Create;
  try
    List.Text := Properties;
    //- Set-/IsReadOnly
    TempStr := List.Values[ReadOnlyStr];
    if TempStr <> '' then
      inherited SetReadOnly(StrToBool(TempStr));
    //- Set-/GetCatalog
    TempStr := List.Values[CatalogStr];
    FCatalog := TempStr;
    //- Set-/GetTransactionIsolation
    TempStr := List.Values[TransactionIsolationStr];
    if TempStr <> '' then begin
      TransactionIsolation := TZTransactIsolationLevel(GetEnumValue(TypeInfo(TZTransactIsolationLevel), TempStr));
      inherited SetTransactionIsolation(TransactionIsolation);
    end;
    //- (Set)UseMetaData
    TempStr := List.Values[UseMetadataStr];
    if TempStr <> '' then
      inherited SetUseMetadata(StrToBool(TempStr));
//    //- Get-/SetAutoEncodeStrings
//    TempStr := List.Values[AutoEncodeStr];
//    if TempStr <> '' then
//      inherited SetAutoEncodeStrings(StrToBool(TempStr));
    TempStr := List.Values[ServerProviderStr];
    if TempStr <> '' then begin
      FServerProvider := TZServerProvider(GetEnumValue(TypeInfo(TZServerProvider), TempStr));
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TZDbcProxyConnection.encodeProperties(PropName, PropValue: String): String;
var
  List: TStringList;
  TransactionIsolation: TZTransactIsolationLevel;
  TempStr: String;
begin
  PropName := LowerCase(PropName);
  List := TStringList.Create;
  try
    if PropName <> '' then
      List.Values[PropName] := PropValue;


    if PropName <> ReadOnlyStr then
      List.Values[ReadOnlyStr] := BoolToStr(IsReadOnly, true);
    if PropName <> CatalogStr then
      List.Values[CatalogStr] := GetCatalog;
    if PropName <> TransactionIsolationStr then begin
      TransactionIsolation := GetTransactionIsolation;
      TempStr := GetEnumName(TypeInfo(TZTransactIsolationLevel), Ord(TransactionIsolation));
      List.Values[TransactionIsolationStr] := TempStr;
    end;
    if PropName <> UseMetadataStr then
      List.Values[UseMetadataStr] := BoolToStr(UseMetadata, true);
//    if PropName <> AutoEncodeStr then
//      List.Values[AutoEncodeStr] := BoolToStr(GetAutoEncodeStrings, true);
    Result := List.Text;
  finally
    FreeAndNil(List);
  end;
end;

procedure TZDbcProxyConnection.transferProperties(PropName, PropValue: String);
var
  PropList: ZWideString;
begin
  if not Closed then begin
    PropList := ZWideString(encodeProperties(PropName, PropValue));
    PropList := FConnIntf.SetProperties(PropList);
  end;
  applyProperties(String(PropList));
end;

procedure TZDbcProxyConnection.SetCatalog(const Catalog: string);
begin
  transferProperties(CatalogStr, Catalog);
end;

function TZDbcProxyConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

procedure TZDbcProxyConnection.SetTransactionIsolation(Level: TZTransactIsolationLevel);
var
  TempStr: String;
begin
  TempStr := GetEnumName(TypeInfo(TZTransactIsolationLevel), Ord(Level));
  transferProperties(TransactionIsolationStr, TempStr);
end;

procedure TZDbcProxyConnection.SetUseMetadata(Value: Boolean);
var
  TempStr: String;
begin
  TempStr := BoolToStr(Value, True);
  transferProperties(UseMetadataStr, TempStr);
end;

function TZDbcProxyConnection.GetServerProvider: TZServerProvider;
begin
  Result := FServerProvider;
end;

function TZDbcProxyConnection.GetStatementAnalyser: IZStatementAnalyser;
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

function TZDbcProxyConnection.GetTokenizer: IZTokenizer;
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
    else Result := TZGenericSQLTokenizer.Create;
  end;
end;

function TZDbcProxyConnection.GetDbInfoStr: ZWideString;
begin
  Result := FDbInfo;
end;

procedure TZDbcProxyConnection.ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory);
var
  Statement: IZStatement;
begin
  Statement := CreateStatementWithParams(nil);
  Statement.Execute(SQL);
  Statement.Close;

end;

initialization
  ProxyDriver := TZDbcProxyDriver.Create;
  DriverManager.RegisterDriver(ProxyDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(ProxyDriver);
  ProxyDriver := nil;

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
end.
