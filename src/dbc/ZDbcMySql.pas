{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainMySqlDriver, ZPlainDriver,
  ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser, ZExceptions;
type

  {** Implements MySQL Database Driver. }

  { TZMySQLDriver }
  TZMySQLDriver = class(TZAbstractDriver)
  private
    FServerArgs: array of PAnsiChar;
    FServerArgsRaw: array of RawByteString;
  protected
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; override;
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer; override;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser; override;
    function GetClientVersion(const Url: string): Integer; override;
  end;

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']
    function GetConnectionHandleAddress: PPMYSQL;
    function GetDatabaseName: String;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
    function GetPlainDriver: TZMySQLPlainDriver;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString);
    function GetByteBufferAddress: PByteBuffer;
    procedure SetSilentError(Value: Boolean);
    function IsSilentError: Boolean;
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      MYSQL_STMT: PMYSQL_STMT; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
  end;

  {** Implements MySQL Database Connection. }

  { TZMySQLConnection }

  TZMySQLConnection = class(TZAbstractSingleTxnConnection, IZConnection,
    IZMySQLConnection, IZTransaction)
  private
    FCatalog: string;
    FHandle: PMySQL;
    FDatabaseName: String;
    FIKnowMyDatabaseName, FMySQL_FieldType_Bit_1_IsBoolean,
    FSupportsBitType, FSupportsReadOnly: Boolean;
    FPlainDriver: TZMySQLPlainDriver;
    FLastWarning: EZSQLWarning;
    FSilentError: Boolean;
    FHostVersion: Integer;
  protected
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
  public
    procedure AfterConstruction; override;
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
    function PrepareCallWithParams(const Name: String; Params: TStrings):
      IZCallableStatement;
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
    /// <summary>Attempts to change the transaction isolation level to the one
    ///  given. The constants defined in the interface <c>Connection</c> are the
    ///  possible transaction isolation levels. Note: This method cannot be
    ///  called while in the middle of a transaction.
    /// <param>"value" one of the TRANSACTION_* isolation values with the
    ///  exception of TRANSACTION_NONE; some databases may not support other
    ///  values. See DatabaseInfo.SupportsTransactionIsolationLevel</param>
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
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
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean); override;

    function PingServer: Integer; override;
    function AbortOperation: Integer; override;

    procedure Open; override;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;

    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    {END ADDED by fduenas 15-06-2006}
    function GetConnectionHandleAddress: PPMYSQL;
    function EscapeString(const Value: RawByteString): RawByteString; overload; override;
    function GetEscapeString(const Value: UnicodeString): UnicodeString; overload; override;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString); overload;

    function GetDatabaseName: String;
    /// <summary>Returns the ServicerProvider for this connection.</summary>
    /// <returns>the ServerProvider</returns>
    function GetServerProvider: TZServerProvider; override;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
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
    function GetPlainDriver: TZMySQLPlainDriver;
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      MYSQL_STMT: PMYSQL_STMT; const LogMessage: SQLString;
      const Sender: IImmediatelyReleasable);
    procedure SetSilentError(Value: Boolean);
    function IsSilentError: Boolean;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
  end;

var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  ZMessages, ZSysUtils, ZDbcMySqlStatement, ZMySqlToken, ZFastCode,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser, TypInfo,
  ZEncoding, ZDbcProperties, ZDbcUtils,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF};

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLPlainDriver.Create));
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
function TZMySQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZMySQLConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZMySQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZMySQLDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZMySQLDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZMySQLTokenizer.Create;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZMySQLStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected plaindriver.
}
function TZMySQLDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean = True): IZPlainDriver;
var I: Integer;
  PlainDriver: TZMySQLPLainDriver;
  TmpList: TStrings;
  ErrorNo: Integer;
begin
  //if libLocation is not in cache the driver will return a clone
  if URL.Properties.Values[ConnProps_Library] <> '' then
    URL.LibLocation := URL.Properties.Values[ConnProps_Library];

  // added by tohenk, 2009-10-11
  // before PlainDriver is initialized, we can perform pre-library loading
  // requirement check here, e.g. Embedded server argument params
  Result := inherited GetPlainDriver(URL, True);
  if Assigned(Result) then begin
    PlainDriver := Result.GetInstance AS TZMySQLPLainDriver;
    if (Assigned(PlainDriver.mysql_server_init) or Assigned(PlainDriver.mysql_library_init)) and
      not PlainDriver.IsInitialized and InitDriver and (Url.Properties.Count >0) then begin
      GlobalCriticalSection.Enter;
      TmpList := TStringList.Create;
      try
        if Url.Properties.Values[ConnProps_Datadir] = ''
        then TmpList.Add(EMBEDDED_DEFAULT_DATA_DIR)
        else TmpList.Add('--datadir=' + Url.Properties.Values[ConnProps_Datadir]);
        for i := 0 to Url.Properties.Count -1 do
          if StartsWith(Url.Properties[i], SERVER_ARGUMENTS_KEY_PREFIX) and
            (Length(Url.Properties[i])>Length(SERVER_ARGUMENTS_KEY_PREFIX)+1) then
            TmpList.Add(Url.Properties.ValueFromIndex[i]);
        SetLength(FServerArgs, TmpList.Count);
        SetLength(FServerArgsRaw, TmpList.Count);
        for i := 0 to TmpList.Count - 1 do begin
          {$IFDEF UNICODE}
          FServerArgsRaw[i] := ZUnicodeToRaw(TmpList[i], ZOSCodePage);
          {$ELSE}
          FServerArgsRaw[i] := TmpList[i];
          {$ENDIF}
          FServerArgs[i] :=  Pointer(FServerArgsRaw[i]);
        end;
        I := TmpList.Count;
        if Assigned(PlainDriver.mysql_library_init)
        then ErrorNo := PlainDriver.mysql_library_init(i, Pointer(FServerArgs), @SERVER_GROUPS) //<<<-- Isn't threadsafe
        else ErrorNo := PlainDriver.mysql_server_init(I, Pointer(FServerArgs), @SERVER_GROUPS); //<<<-- Isn't threadsafe
        if ErrorNo <> 0 then
          raise EZSQLException.Create('Could not initialize the MySQL / MariaDB client library. Error No: ' + ZFastCode.IntToStr(ErrorNo));  // The manual says nothing else can be called until this call succeeds. So lets just throw the error number...
        PlainDriver.IsInitialized := True;
      finally
        FreeAndNil(TmpList);
        GlobalCriticalSection.Leave;
      end;
    end;
  end
  else
    raise EZSQLException.Create('Can''t receive Plaindriver!');
end;

{**
  Returns the version of the plain driver library that will be used to open a connection
  to the given URL.
  @param url the URL of the database
  @return the version number of the plain driver library for the give URL
}
function TZMySQLDriver.GetClientVersion(const Url: string): Integer;
var
  TempURL: TZURL;
  iPlainDriver: IZPlainDriver;
  mPlainDriver: TZMySQLPlainDriver;
begin
  TempURL := TZURL.Create(Url);
  {$IFDEF FPC}
  Result := -1;
  {$ENDIF}
  try
    //FPC can't compile that code after adding try finally... circumvent it
    //Result := ConvertMySQLVersionToSQLVersion(((GetPlainDriver(TempUrl).GetInstance) AS TZMySQLPlainDriver).mysql_get_client_version);
    iPlainDriver := GetPlainDriver(TempUrl);
    mPlainDriver := iPlainDriver.GetInstance as TZMySQLPlainDriver;
    Result := mPlainDriver.mysql_get_client_version;
  finally
    TempUrl.Free;
  end;
end;

{ TZMySQLConnection }

function TZMySQLConnection.IsSilentError: Boolean;
begin
  Result := FSilentError;
end;

const
  MySQLSessionTransactionIsolation: array[TZTransactIsolationLevel] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE');

  MySQLSessionTransactionReadOnly: array[Boolean] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION READ ONLY',
    'SET SESSION TRANSACTION READ WRITE');

  MySQLCommitMsg: array[Boolean] of SQLString = (
    'SET autocommit=0', 'SET autocommit=1');

const
  cStartTransaction: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'START TRANSACTION';
  cCommit: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'COMMIT';
  cRollback: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'ROLLBACK';

function TZMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean: Boolean;
begin
  Result := FMySQL_FieldType_Bit_1_IsBoolean;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  UIntOpt: UInt;
  MyBoolOpt: Byte;
  ClientFlag : Cardinal;
  myopt: TMySQLOption;
  sMyOpt: string;
  my_client_Opt:TMYSQL_CLIENT_OPTIONS;
  sMy_client_Opt, sMy_client_Char_Set:String;
  ClientVersion, OptionRequiredVersion: Integer;
  SQL: RawByteString;
  P: PAnsiChar;
  S: String;
  procedure mysql_ssl_set;
  var
    pSslCa, pSslCaPath, pSslKey, pSslCert, pSslCypher: PAnsiChar;
    SslCa, SslCaPath, SslKey, SslCert, SslCypher: RawByteString;
  begin
    S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_KEY)];
    if S <> '' then begin
      SslKey := {$IFDEF UNICODE}ZUnicodeToRaw(S, ZOSCodePage){$ELSE}S{$ENDIF};
      pSslKey := Pointer(SslKey);
    end else pSslKey := nil;
    S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CERT)];
    if S <> '' then begin
      SslCert := {$IFDEF UNICODE}ZUnicodeToRaw(S, ZOSCodePage){$ELSE}S{$ENDIF};
      pSslCert := Pointer(SslCert);
    end else pSslCert := nil;
    S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CA)];
    if S <> '' then begin
      SslCa := {$IFDEF UNICODE}ZUnicodeToRaw(S, ZOSCodePage){$ELSE}S{$ENDIF};
      pSslCa := Pointer(SslCa);
    end else pSslCa := nil;
    S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CAPATH)];
    if S <> '' then begin
      SslCaPath := {$IFDEF UNICODE}ZUnicodeToRaw(S, ZOSCodePage){$ELSE}S{$ENDIF};
      pSslCaPath := Pointer(SslCaPath);
    end else pSslCaPath := nil;
    S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CIPHER)];
    if S <> '' then begin
      SslCypher := {$IFDEF UNICODE}ZUnicodeToRaw(S, ZOSCodePage){$ELSE}S{$ENDIF};
      pSslCypher := Pointer(SslCypher);
    end else pSslCypher := nil;
    FPlainDriver.mysql_ssl_set(FHandle, pSslKey, pSslCert, pSslCa, pSslCaPath, pSslCypher);
   if DriverManager.HasLoggingListener then
     DriverManager.LogMessage(lcOther, URL.Protocol, 'SSL options set');
  end;
label setuint;
begin
  if not Closed then
    Exit;
  FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);;
  if (FHandle <> nil) then begin
    if (PingServer = 0) then begin
      inherited Open;
      Exit;
    end;
  end else begin
    GlobalCriticalSection.Enter;
    try
      FHandle := FPlainDriver.mysql_init(FHandle); //is not threadsave!
    finally
      GlobalCriticalSection.Leave;
    end;
  end;
  {EgonHugeist: get current characterset first }
  if Assigned(FPlainDriver.mysql_character_set_name) then begin
    P := FPlainDriver.mysql_character_set_name(FHandle);
    {$IFDEF UNICODE}
    sMy_client_Char_Set := ASCII7ToUnicodeString(P, StrLen(P));
    {$ELSE}
    System.SetString(sMy_client_Char_Set, P, StrLen(P));
    {$ENDIF}
    if (sMy_client_Char_Set <> '') {mysql 4down doesn't have this function } and
     (sMy_client_Char_Set <> FClientCodePage) then begin
      ConSettings^.ClientCodePage := FPlainDriver.ValidateCharEncoding(sMy_client_Char_Set);
    end;
  end;
  try
    { Sets a default port number. }
    if Port = 0 then
       Port := MYSQL_PORT;

    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values[ConnProps_Compress]) and
      (Info.Values[GetMySQLOptionValue(MYSQL_OPT_COMPRESS)] = '') then //check if user allready did set the value!
      Info.Values[GetMySQLOptionValue(MYSQL_OPT_COMPRESS)] := Info.Values[ConnProps_Compress];
    { Sets connection timeout. }
    if (StrToIntDef(Info.Values[ConnProps_Timeout], 0) >= 0) and
       (Info.Values[GetMySQLOptionValue(MYSQL_OPT_CONNECT_TIMEOUT)] = '') then //check if user allready did set the value!
      Info.Values[GetMySQLOptionValue(MYSQL_OPT_CONNECT_TIMEOUT)] := Info.Values[ConnProps_Timeout];

   (*Added lines to handle option parameters 21 november 2007 marco cotroneo*)
    ClientVersion := FPlainDriver.mysql_get_client_version;
    for myopt := low(TMySQLOption) to high(TMySQLOption) do
    begin
      sMyOpt:= GetMySQLOptionValue(myOpt);
      if FPLainDriver.IsMariaDBDriver then
        OptionRequiredVersion := TMariaDBOptionMinimumVersion[myopt]
      else
        OptionRequiredVersion := TMySqlOptionMinimumVersion[myopt];

      if ClientVersion >= OptionRequiredVersion then //version checked (:
        case myopt of
          {unsigned int options ...}
          MYSQL_OPT_CONNECT_TIMEOUT,
          MYSQL_OPT_PROTOCOL,
          MYSQL_OPT_READ_TIMEOUT,
          MYSQL_OPT_WRITE_TIMEOUT,
          MYSQL_OPT_MAX_ALLOWED_PACKET,
          MYSQL_OPT_NET_BUFFER_LENGTH,
          MYSQL_OPT_SSL_MODE,
          MYSQL_OPT_RETRY_COUNT,
          MYSQL_OPT_SSL_FIPS_MODE,
          MYSQL_OPT_ZSTD_COMPRESSION_LEVEL:
            if Info.Values[sMyOpt] <> '' then begin
setuint:      UIntOpt := {$IFDEF UNICODE}UnicodeToUInt32Def{$ELSE}RawToUInt32Def{$ENDIF}(Info.Values[sMyOpt], 0);
              FPlainDriver.mysql_options(FHandle, myopt, @UIntOpt);
            end;
          MYSQL_OPT_LOCAL_INFILE: {optional empty or unsigned int}
            if Info.Values[sMyOpt] <> ''
            then goto setuint
            else if Info.IndexOf(sMyOpt) > -1 then
              FPlainDriver.mysql_options(FHandle, myopt, nil);
          { no value options }
          MYSQL_OPT_COMPRESS,
          MYSQL_OPT_GUESS_CONNECTION,
          MYSQL_OPT_NAMED_PIPE,
          MYSQL_OPT_USE_REMOTE_CONNECTION,
          MYSQL_OPT_USE_EMBEDDED_CONNECTION,
          MYSQL_OPT_USE_RESULT,
          MYSQL_OPT_CONNECT_ATTR_RESET:
            if (Info.Values[sMyOpt] <> '') or (Info.IndexOf(sMyOpt) > -1) then
              FPlainDriver.mysql_options(FHandle, myopt, nil);
          { my_bool * options}
          MYSQL_REPORT_DATA_TRUNCATION,
          MYSQL_SECURE_AUTH,
          MYSQL_OPT_RECONNECT,
          MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
          MYSQL_ENABLE_CLEARTEXT_PLUGIN,
          MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
          MYSQL_OPT_SSL_ENFORCE,
          MYSQL_OPT_GET_SERVER_PUBLIC_KEY,
          MYSQL_OPT_OPTIONAL_RESULTSET_METADATA:
            if Info.Values[sMyOpt] <> '' then begin
              MyBoolOpt := Ord(StrToBoolEx(Info.Values[sMyOpt]));
              FPlainDriver.mysql_options(FHandle, myopt, @MyBoolOpt);
            end;
          { unsigned char * options }
          MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT,
          MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER,
          MYSQL_OPT_TLS_CIPHERSUITES,
          MYSQL_OPT_COMPRESSION_ALGORITHMS: ;//skip, processed down below
          else  begin
                  S := Info.Values[sMyOpt];
                  if S <> '' then begin
                    {$IFDEF UNICODE}
                    SQL := ZUnicodeToRaw(S, ZOSCodePage);
                    P := Pointer(SQL);
                    {$ELSE}
                    P := Pointer(S);
                    {$ENDIF}
                    FPlainDriver.mysql_options(FHandle, myopt, P);
                  end;
                end;
        end;
    end;

    { Set ClientFlag }
    ClientFlag := 0;
    if not StrToBoolEx(Info.Values[ConnProps_DBLess]) then
      ClientFlag := (1 shl Integer(CLIENT_CONNECT_WITH_DB));

    for my_client_Opt := Low(TMYSQL_CLIENT_OPTIONS) to High(TMYSQL_CLIENT_OPTIONS) do
    begin
      sMy_client_Opt := GetEnumName(TypeInfo(TMYSQL_CLIENT_OPTIONS), Integer(my_client_Opt));
      if StrToBoolEx(Info.Values[sMy_client_Opt]) then
        ClientFlag := ClientFlag or Cardinal(1 shl Integer(my_client_Opt));
    end;

    { Set SSL properties before connect}
    S := Info.Values[ConnProps_MYSQLSSL];
    if StrToBoolEx(S) then mysql_ssl_set;

    { Connect to MySQL database. }
    if FPlainDriver.mysql_real_connect(FHandle,
    {$IFDEF UNICODE}
        PAnsiChar(ZUnicodeToRaw(URL.HostName, zOSCodePage)),
        PAnsiChar(ZUnicodeToRaw(URL.UserName, zOSCodePage)),
        PAnsiChar(ZUnicodeToRaw(URL.Password, zOSCodePage)),
        PAnsiChar(ZUnicodeToRaw(URL.Database, zOSCodePage)),
    {$ELSE}
        PAnsiChar(URL.HostName), PAnsiChar(URL.UserName),
        PAnsiChar(URL.Password), PAnsiChar(URL.Database),
    {$ENDIF}
        Port, nil,ClientFlag) = nil then begin
      HandleErrorOrWarning(lcConnect, nil, FLogMessage,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
      if DriverManager.HasLoggingListener then
        DriverManager.LogError(lcConnect, URL.Protocol,
          FLogMessage, 0, SUnknownError);
      raise EZSQLException.Create(SCanNotConnectToServer);
    end;
    DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);

    { Fix Bugs in certain Versions where real_conncet resets the Reconnect flag }
    s := Info.Values[GetMySQLOptionValue(MYSQL_OPT_RECONNECT)];
    if (S <> '') and
      ((ClientVersion>=50013) and (ClientVersion<50019)) or
      ((ClientVersion>=50100) and (ClientVersion<50106)) then begin
      S := Info.Values[GetMySQLOptionValue(MYSQL_OPT_RECONNECT)];
      MyBoolOpt := Ord(StrToBoolEx(S));
      FPlainDriver.mysql_options(FHandle, MYSQL_OPT_RECONNECT, @MyBoolOpt);
    end;
    if (FClientCodePage = '') and (sMy_client_Char_Set <> '') then
      FClientCodePage := sMy_client_Char_Set;

    //EH: MariaDB needs a explizit set of charset to be synced on Client<>Server!
    if (FClientCodePage <> sMy_client_Char_Set) or (FPlainDriver.IsMariaDBDriver and (FClientCodePage <> '')) then begin
      //http://dev.mysql.com/doc/refman/5.7/en/mysql-set-character-set.html
      //take care mysql_real_escape_string works like expected!
      SQL := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage);
      if not Assigned(FPlainDriver.mysql_set_character_set) or
            (FPlainDriver.mysql_set_character_set(FHandle, Pointer(SQL)) <> 0) then begin //failed? might be possible the function does not exists
        SQL := 'SET NAMES '+SQL;
        ExecuteImmediat(SQL, lcOther);
      end;
      CheckCharEncoding(FClientCodePage);
    end;
    inherited Open;
  finally
    if Closed then begin
      FPlainDriver.mysql_close(FHandle);
      FHandle := nil;
    end;
  end;
  if TMySqlOptionMinimumVersion[MYSQL_OPT_MAX_ALLOWED_PACKET] < GetHostVersion then begin
    S := Info.Values[ConnProps_MYSQL_OPT_MAX_ALLOWED_PACKET];
    UIntOpt := {$IFDEF UNICODE}UnicodeToUInt32Def{$ELSE}RawToUInt32Def{$ENDIF}(S, 0);
    if (UIntOpt <> 0) then begin
      SQL := 'SET GLOBAL max_allowed_packet='+IntToRaw(UIntOpt);
      ExecuteImmediat(SQL, lcOther);
    end;
  end;
  //no real version check required -> the user can simply switch off treading
  //enum('Y','N')
  S := Info.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean];
  FMySQL_FieldType_Bit_1_IsBoolean := StrToBoolEx(S);
  FSupportsBitType := (
    (    FPlainDriver.IsMariaDBDriver and (ClientVersion >= 100109) ) or
    (not FPlainDriver.IsMariaDBDriver and (ClientVersion >=  50003) ) ) and (GetHostVersion >= EncodeSQLVersioning(5,0,3));
  //if not explizit !un!set -> assume as default since Zeos 7.3
  FMySQL_FieldType_Bit_1_IsBoolean := FMySQL_FieldType_Bit_1_IsBoolean or (FSupportsBitType and (S = ''));
  with (GetMetadata as IZMySQLDatabaseMetadata) do begin
    SetMySQL_FieldType_Bit_1_IsBoolean(FMySQL_FieldType_Bit_1_IsBoolean);
    FSupportsReadOnly := ( IsMariaDB and (GetHostVersion >= EncodeSQLVersioning(10,0,0))) or
                         ( IsMySQL and (GetHostVersion >= EncodeSQLVersioning( 5,6,0)));
    SetDataBaseName(GetDatabaseName);
    with CreateStatement.ExecuteQuery('show variables like "lower_case_table_names"') do begin
      if next
      then Set_lower_case_table_names(GetByte(FirstDBCIndex))
      else Set_lower_case_table_names({$IFDEF WINDOWS}1{$ELSE}{$IFDEF DARWIN}2{$ELSE}0{$ENDIF}{$ENDIF});
      Close;
    end;
    sMyOpt := LowerCase(FClientCodePage);
    if (sMyOpt = 'utf8') and (IsMariaDB or (GetHostVersion >= EncodeSQLVersioning(4,1,0))) then begin
      CheckCharEncoding('utf8mb4');
      //EH: MariaDB needs a explizit set of charset to be synced on Client<>Server!
      SQL := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage);
      if not Assigned(FPlainDriver.mysql_set_character_set) or
            (FPlainDriver.mysql_set_character_set(FHandle, Pointer(SQL)) <> 0) then begin //failed? might be possible the function does not exists
        SQL := 'SET NAMES '+SQL;
        ExecuteImmediat(SQL, lcOther);
      end;
    end;
  end;

  { Sets transaction isolation level. }
  if not (TransactIsolationLevel in [tiNone,tiRepeatableRead]) then
    ExecuteImmediat(MySQLSessionTransactionIsolation[TransactIsolationLevel], lcTransaction);
  if (FSupportsReadOnly and ReadOnly) then
    ExecuteImmediat(MySQLSessionTransactionReadOnly[ReadOnly], lcTransaction);

  { Sets an auto commit mode. }
  if not AutoCommit then begin
    AutoCommit := True;
    SetAutoCommit(False);
  end;
  if FSupportsReadOnly and ReadOnly then begin
    ReadOnly := False;
    SetReadOnly(True);
  end;


  if FClientCodePage = '' then begin //workaround for MySQL 4 down
    with CreateStatement.ExecuteQuery('show variables like "character_set_database"') do begin
      if Next then
        FClientCodePage := GetString(FirstDbcIndex+1);
      Close;
    end;
    ConSettings^.ClientCodePage := FPlainDriver.ValidateCharEncoding(FClientCodePage);
  end;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZMySQLConnection.PingServer: Integer;
const
   PING_ERROR_ZEOSCONNCLOSED = -1;
begin
   if Closed or (FHandle = nil)
   then Result := PING_ERROR_ZEOSCONNCLOSED
   else Result := FPlainDriver.mysql_ping(FHandle);
end;

function TZMySQLConnection.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  if (FPLainDriver.IsMariaDBDriver and (FPLainDriver.mysql_get_client_version >= 100000)) or
     (not FPLainDriver.IsMariaDBDriver and (FPLainDriver.mysql_get_client_version >= 50608))
  then Result := TZMySQLCallableStatement56up.Create(Self, Name, Params)
  else Result := TZMySQLCallableStatement56down.Create(Self, Name, Params);
end;

function TZMySQLConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if FPlainDriver.IsMariaDBDriver and (FPlainDriver.mysql_get_client_version >= 100207) and
     (GetHostVersion >= EncodeSQLVersioning(10,3,0))
  then Result := TZMariaDBBatchDMLPreparedStatement.Create(Self, SQL, Info)
  else Result := TZMySQLEmulatedBatchPreparedStatement.Create(Self, SQL, Info);
end;

function TZMySQLConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLStatement.Create(Self, Info);
end;

function TZMySQLConnection.EscapeString(
  const Value: RawByteString): RawByteString;
begin
  GetEscapeString(Pointer(Value), Length(Value), Result);
end;

procedure TZMySQLConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var Status: Integer;
begin
  if Pointer(SQL) = nil then
    Exit;
  if Closed then Open;

  Status := FPlainDriver.mysql_real_query(FHandle,
    Pointer(SQL), Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
  if (Status <> 0) or DriverManager.HasLoggingListener then
    {$IFDEF UNICODE}
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
    {$ELSE}
    FLogMessage := SQL;
    {$ENDIF}
  if Status <> 0
  then HandleErrorOrWarning(LoggingCategory, nil, FLogMessage,
    IImmediatelyReleasable(FWeakImmediatRelPtr))
  else if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, URL.Protocol, FLogMessage);
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
function TZMySQLConnection.AbortOperation: Integer;
Var
 killquery: SQLString;
 izc: IZConnection;
Begin
  { EH untested, just prepared
  if Assigned(FPlainDriver.mariadb_cancel) then begin
    Result := FPlainDriver.mariadb_cancel(FHandle);
    FPlainDriver.mariadb_reconnect(FHandle)
  end else }begin
    // https://dev.mysql.com/doc/refman/5.7/en/mysql-kill.html
    killquery := 'KILL QUERY ' + IntToStr(FPlainDriver.mysql_thread_id(FHandle));
    izc := DriverManager.GetConnection(GetURL);
    Result := izc.CreateStatement.ExecuteUpdate(killquery);
  end;
End;

procedure TZMySQLConnection.Commit;
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
  end else begin
(* //see: https://github.com/mariadb-corporation/mariadb-connector-c/blob/3.1/libmariadb/mariadb_lib.c
    my_bool STDCALL mysql_commit(MYSQL *mysql)
{
  return((my_bool)mysql_real_query(mysql, "COMMIT", (unsigned long)strlen("COMMIT")));
}*)
    ExecuteImmediat(cCOMMIT, lcTransaction);
    AutoCommit := not FRestartTransaction;
  end
end;

procedure TZMySQLConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FHandle := nil;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZMySQLConnection.Rollback;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := cRollbackToSP+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    (* https://github.com/mariadb-corporation/mariadb-connector-c/blob/3.1/libmariadb/mariadb_lib.c
my_bool STDCALL mysql_rollback(MYSQL *mysql)
{
  return((my_bool)mysql_real_query(mysql, "ROLLBACK", (unsigned long)strlen("ROLLBACK")));
}*)
    ExecuteImmediat(cROLLBACK, lcTransaction);
    AutoCommit := not FRestartTransaction;
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZMySQLConnection.InternalClose;
begin
  if ( Closed ) or (not Assigned(FPlainDriver)) then
    Exit;
  FSavePoints.Clear;
  try
    if not AutoCommit then begin
      AutoCommit := not FRestartTransaction;
      if FPlainDriver.mysql_rollback(FHandle) <> 0 then
        HandleErrorOrWarning(lcTransaction, nil, sRollbackMsg,
          IImmediatelyReleasable(FWeakImmediatRelPtr))
    end;
  finally
    FPlainDriver.mysql_close(FHandle);
    FHandle := nil;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcDisconnect, URL.Protocol,
        'DISCONNECT FROM "'+URL.Database+'"');
    FHostVersion := 0;
  end;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZMySQLConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZMySQLConnection.GetWarnings: EZSQLWarning;
begin
  Result := FLastWarning;
end;

procedure TZMySQLConnection.AfterConstruction;
begin
  FPlainDriver := PlainDriver.GetInstance as TZMySQLPlainDriver;
  FIKnowMyDatabaseName := False;
  if Self.Port = 0 then
     Self.Port := MYSQL_PORT;
  FMetaData := TZMySQLDatabaseMetadata.Create(Self, Url);
  inherited AfterConstruction;
  inherited SetTransactionIsolation(tiRepeatableRead);
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZMySQLConnection.ClearWarnings;
begin
  FreeAndNil(FLastWarning);
end;

procedure TZMySQLConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

procedure TZMySQLConnection.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then begin
    if not Closed then begin
      if not FSupportsReadOnly then
        raise EZUnsupportedException.Create(SUnsupportedOperation);
      ExecuteImmediat(MySQLSessionTransactionReadOnly[ReadOnly], lcTransaction);
    end;
    ReadOnly := Value;
  end;
end;

procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone then
    Level := tiRepeatableRead;
  if TransactIsolationLevel <> Level then begin
    if not Closed then
      ExecuteImmediat(MySQLSessionTransactionIsolation[Level], lcTransaction);
    TransactIsolationLevel := Level;
  end;
end;

function TZMySQLConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    ExecuteImmediat(cStartTransaction, lcTransaction);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    ExecuteImmediat(cSavePoint+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S) +2;
  end;
end;

function TZMySQLConnection.SupportsFieldTypeBit: Boolean;
begin
  if Closed then
    Open;
  Result := FSupportsBitType;
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZMySQLConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    {else if Value then begin
      FSavePoints.Clear;
      if FPlainDriver.mysql_autocommit(FHandle, 1) <> 0 then
        HandleErrorOrWarning(lcTransaction, nil, MySQLCommitMsg[True],
          IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, URL.Protocol, MySQLCommitMsg[True]);
      AutoCommit := True;
    end else
      StartTransaction;}
    else begin
      if Value then
        FSavePoints.Clear;
      if FPlainDriver.mysql_autocommit(FHandle, Ord(Value)) <> 0 then
        HandleErrorOrWarning(lcTransaction, nil, MySQLCommitMsg[Value],
          IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, URL.Protocol, MySQLCommitMsg[Value]);
      FRestartTransaction := AutoCommit;
      AutoCommit := Value;
    end;
  end;
end;

{**
  Gets client's full version number.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetClientVersion: Integer;
begin
  Result := ConvertMySQLVersionToSQLVersion(FPlainDriver.mysql_get_client_version);
end;

{**
  Gets server's full version number.
  The format of the returned version must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetHostVersion: Integer;
begin
  if FHostVersion = 0
  then Result := ConvertMySQLVersionToSQLVersion(FPlainDriver.mysql_get_server_version(FHandle) )
  else Result := FHostVersion;
end;

function TZMySQLConnection.GetPlainDriver: TZMySQLPlainDriver;
begin
  Result := FPlainDriver;
end;

procedure TZMySQLConnection.HandleErrorOrWarning(
  LogCategory: TZLoggingCategory; MYSQL_STMT: PMYSQL_STMT;
  const LogMessage: SQLString; const Sender: IImmediatelyReleasable);
var
  FormatStr, SQLState: String;
  C: Cardinal;
  ErrorCode: Integer absolute C;
  P, S: PAnsiChar;
  L: NativeUInt;
  Error: EZSQLThrowable;
  AExceptionClass: EZSQLThrowableClass;
  msgCP: Word;
  {$IFNDEF UNICODE}
  excCP: Word;
  {$ENDIF}
label jmpErr;
begin
  S := nil;
  if Assigned(MYSQL_STMT) then begin
    C := FPlainDriver.mysql_stmt_errno(MYSQL_STMT);
    P := FPlainDriver.mysql_stmt_error(MYSQL_STMT);
    if Assigned(FPlainDriver.mysql_stmt_sqlstate) then
      S := FPlainDriver.mysql_stmt_sqlstate(MYSQL_STMT);
  end else begin
    C := FPlainDriver.mysql_errno(FHandle);
    P := FPlainDriver.mysql_error(FHandle);
    if Assigned(FPlainDriver.mysql_stmt_sqlstate) then
      S := FPlainDriver.mysql_sqlstate(FHandle);
  end;
  if (ErrorCode > 0) then begin
    if (ConSettings <> nil) and (ConSettings.ClientCodePage <> nil)
    then msgCP := ConSettings.ClientCodePage.CP
    else msgCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
      {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
  {$IFNDEF UNICODE}
    excCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
        {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
  {$ENDIF}
    FLogMessage := '';
    if P <> nil then begin
      L := StrLen(P);
      Trim(L, P);
      {$IFDEF UNICODE}
      FLogMessage := PRawToUnicode(P, L, msgCP);
      {$ELSE}
      if excCP <> msgCP
      then PRawToRawConvert(P, l, msgCP, excCP, FLogMessage)
      else System.SetString(FLogMessage, P, l);
      {$ENDIF}
    end;
    SQLState := '';
    if S <> nil then begin
      L := StrLen(S);
      Trim(L, S);
      {$IFDEF UNICODE}
      SQLState := ASCII7ToUnicodeString(S, L);
      {$ELSE}
      System.SetString(SQLState, s, l);
      {$ENDIF}
    end;
    if (FLogMessage = '') then
      FLogMessage := SUnknownError;

    Error := nil;
    case ErrorCode of
      CR_SERVER_GONE_ERROR,
      CR_SERVER_LOST: if LogCategory <> lcConnect
                      then AExceptionClass := EZSQLConnectionLost
                      else goto jmpErr;
      WARN_DATA_TRUNCATED,
      WARN_NO_MASTER_INFO,
      WARN_OPTION_IGNORED,
      WARN_PLUGIN_BUSY,
      WARN_NON_ASCII_SEPARATOR_NOT_IMPLEMENTED,
      WARN_COND_ITEM_TRUNCATED,
      WARN_OPTION_BELOW_LIMIT,
      WARN_ON_BLOCKHOLE_IN_RBR,
      WARN_DEPRECATED_MAXDB_SQL_MODE_FOR_TIMESTAMP:
          AExceptionClass := EZSQLWarning;
      else
jmpErr:   if not FSilentError
          then AExceptionClass := EZSQLException
          else AExceptionClass := nil;
    end;
    if DriverManager.HasLoggingListener then
      LogError(LogCategory, ErrorCode, Sender, LogMessage, FLogMessage);
    if AExceptionClass <> nil then begin
      if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
        if LogCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
        then FormatStr := SSQLError3
        else FormatStr := SSQLError4
      else FormatStr := SSQLError2;
      if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
      then FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode, LogMessage])
      else FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode]);
      Error := AExceptionClass.CreateWithCodeAndStatus(ErrorCode, SQLState, FLogMessage);
      FLogMessage := '';
      if AExceptionClass = EZSQLConnectionLost then
        if Sender <> nil
        then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
        else ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
      else if AExceptionClass = EZSQLWarning then begin
        ClearWarnings;
        if not RaiseWarnings then begin
          FLastWarning := EZSQLWarning(Error);
          Error := nil;
        end;
      end;
      if Error <> nil then
        raise Error;
    end;
  end;
end;

procedure TZMySQLConnection.SetSilentError(Value: Boolean);
begin
  FSilentError := Value;
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandleAddress: PPMYSQL;
begin
  Result := @FHandle;
end;

function TZMySQLConnection.GetServerProvider: TZServerProvider;
begin
  Result := spMySQL;
end;

function TZMySQLConnection.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZMySQLStatementAnalyser.Create;
end;

function TZMySQLConnection.GetTokenizer: IZTokenizer;
begin
  Result := TZMySQLTokenizer.Create;
end;

function TZMySQLConnection.GetDatabaseName: String;
var
  ResultSet: IZResultSet;
begin
  if not FIKnowMyDatabaseName then begin
    ResultSet := CreateStatement.ExecuteQuery('select database() as ''DATABASE''');
    if ResultSet.Next
    then FDatabaseName := ResultSet.GetStringByName('DATABASE');
    FIKnowMyDatabaseName := True;
    ResultSet.Close;
    ResultSet := nil;
  end;
  Result := FDatabaseName;
end;

function TZMySQLConnection.GetEscapeString(
  const Value: UnicodeString): UnicodeString;
var tmp: RawByteString;
begin
  tmp := ZUnicodeToRaw(Value, ConSettings.ClientcodePage.CP);
  GetEscapeString(Pointer(tmp), Length(Tmp), tmp);
  Result := ZRawToUnicode(tmp, ConSettings.ClientcodePage.CP);
end;

procedure TZMySQLConnection.GetEscapeString(Buf: PAnsichar; Len: LengthInt;
  out Result: RawByteString);
var
  StatBuf: array[0..2048] of AnsiChar;
  P: PAnsichar;
  EscapedLen: ULong;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  if ((Len+1) shl 1) > (SizeOf(Buf)-1) then begin
    SetLength(Result, (Len+1) shl 1);
    P := Pointer(Result);
  end else
    P := @StatBuf[0];
  PByte(P)^ := Ord(#39);
  if FHandle = nil
  then EscapedLen := FPlainDriver.mysql_escape_string(P+1, Buf, Len)
  else EscapedLen := FPlainDriver.mysql_real_escape_string(FHandle, P+1, Buf, Len);
  PByte(P+EscapedLen+1)^ := Ord(#39);
  if P = @StatBuf[0]
  then ZSetString(@StatBuf[0], EscapedLen+2, Result)
  else SetLength(Result, EscapedLen+2);
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;

  {$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.
