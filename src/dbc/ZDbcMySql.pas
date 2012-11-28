{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
{   http://zeos.firmos.at  (FORUM)                        }
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

uses
  Types, ZCompatibility, Classes, SysUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainMySqlDriver, ZPlainDriver, ZURL, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZPlainMySqlConstants;

type

  {** Implements MySQL Database Driver. }

  { TZMySQLDriver }
  {$WARNINGS OFF}
  TZMySQLDriver = class(TZAbstractDriver)
  protected
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; override;
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
    function GetClientVersion(const Url: string): Integer; override;
  end;
  {$WARNINGS ON}

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']

    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
  end;

  {** Implements MySQL Database Connection. }
  TZMySQLConnection = class(TZAbstractConnection, IZMySQLConnection)
  private
    FCatalog: string;
    FHandle: PZMySQLConnect;
  protected
    procedure InternalCreate; override;
  public
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    function PingServer: Integer; override;
    function EscapeString(Value: ZAnsiString): ZAnsiString; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetAutoCommit(AutoCommit: Boolean); override;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    {END ADDED by fduenas 15-06-2006}
    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PZMySQLConnect;
    function GetEscapeString(const Value: ZWideString): ZWideString; override;
    function GetEscapeString(const Value: ZAnsiString): ZAnsiString; override;
  end;


var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

implementation

uses
  ZMessages, ZSysUtils, ZDbcUtils, ZDbcMySqlStatement, ZMySqlToken,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser, TypInfo, Math;

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL5PlainDriver.Create(Self.GetTokenizer), 'mysql'));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL41PlainDriver.Create(Self.GetTokenizer)));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL5PlainDriver.Create(Self.GetTokenizer)));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLD41PlainDriver.Create(Self.GetTokenizer)));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLD5PlainDriver.Create(Self.GetTokenizer)));
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
{$WARNINGS OFF}
function TZMySQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZMySQLConnection.Create(Url);
end;
{$WARNINGS ON}

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
  if Tokenizer = nil then
    Tokenizer := TZMySQLTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZMySQLStatementAnalyser.Create;
  Result := Analyser;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected plaindriver.
}
function TZMySQLDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean = True): IZPlainDriver;
begin
  // added by tohenk, 2009-10-11
  // before PlainDriver is initialized, we can perform pre-library loading
  // requirement check here, e.g. Embedded server argument params
  Result := inherited GetPlainDriver(URL, False);
  if Assigned(Result) then
  begin
    if Url.Properties.Count >0  then
      (Result as IZMySQLPlainDriver).SetDriverOptions(Url.Properties);
    // end added by tohenk, 2009-10-11
    if InitDriver then Result.Initialize(Url.LibLocation);
  end
  else
    raise Exception.Create('Can''t receive Plaindriver!');
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
begin
  TempURL := TZURL.Create(Url);
  Result := ConvertMySQLVersionToSQLVersion((GetPlainDriver(TempUrl) as IZMySQLPlainDriver).GetClientVersion);
  TempUrl.Free
end;

{ TZMySQLConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZMySQLConnection.InternalCreate;
begin
  FMetaData := TZMySQLDatabaseMetadata.Create(Self, Url);
  if Self.Port = 0 then
     Self.Port := MYSQL_PORT;
  AutoCommit := True;
  TransactIsolationLevel := tiNone;

  { Processes connection properties. }
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLConnection.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  LogMessage: string;
  OldLevel: TZTransactIsolationLevel;
  OldAutoCommit: Boolean;
  ConnectTimeout: Integer;
  ClientFlag : Cardinal;
  SslCa, SslCaPath, SslKey, SslCert, SslCypher: PAnsiChar;
  myopt: TMySQLOption;
  sMyOpt: string;
  my_client_Opt:TMYSQL_CLIENT_OPTIONS;
  sMy_client_Opt, sMy_client_Char_Set:String;
  ClientVersion: Integer;
  SQL: PAnsiChar;
begin
   if not Closed then
      Exit;

  LogMessage := Format('CONNECT TO "%s" AS USER "%s"', [Database, User]);

  GetPlainDriver.Init(FHandle);
  {EgonHugeist: Arrange Client-CodePage/CharacterSet first
    Now we know if UTFEncoding is neccessary or not}
  sMy_client_Char_Set := String(GetPlainDriver.GetConnectionCharacterSet(FHandle));
  ConSettings.ClientCodePage := GetPlainDriver.ValidateCharEncoding(sMy_client_Char_Set); //This sets the internal use of Encodings..
  {EgonHugeist:
    Now we know in which kind of CharacterSet we have to send the next Connection-Properties
    before we can change to the CharacterSet we want to have here..
    This sets also all environment-variables to the Codepaged Object.
    Now the compatibility-functions ZString/ZPlainString working like
    Database-expected Data has to be!!. }

  try
    { Sets a default port number. }
    if Port = 0 then
       Port := MYSQL_PORT;

    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values['compress']) then
      GetPlainDriver.SetOptions(FHandle, MYSQL_OPT_COMPRESS, nil);
    { Sets connection timeout. }
    ConnectTimeout := StrToIntDef(Info.Values['timeout'], 0);
    if ConnectTimeout >= 0 then
      GetPlainDriver.SetOptions(FHandle, MYSQL_OPT_CONNECT_TIMEOUT, PAnsiChar(@ConnectTimeout));

   (*Added lines to handle option parameters 21 november 2007 marco cotroneo*)
    for myopt := low(TMySQLOption) to high(TMySQLOption) do
    begin
      sMyOpt:= GetEnumName(typeInfo(TMySQLOption), integer(myOpt));
      if Info.Values[sMyOpt] <> '' then
      begin
        GetPlainDriver.SetOptions(FHandle, myopt, PAnsiChar(AnsiString(Info.Values[sMyOpt])));
      end;
    end;

    { Set ClientFlag }
    ClientFlag := 0;
    if Not StrToBoolEx(Info.Values['dbless'])
       then ClientFlag := trunc(power(2, GetEnumValue(   TypeInfo(TMYSQL_CLIENT_OPTIONS),'_CLIENT_CONNECT_WITH_DB')));

    for my_client_Opt := low(TMYSQL_CLIENT_OPTIONS) to high(TMYSQL_CLIENT_OPTIONS) do
    begin
      sMy_client_Opt:= GetEnumName(typeInfo(TMYSQL_CLIENT_OPTIONS), integer(my_client_Opt));
      if StrToBoolEx(Info.Values[sMy_client_Opt]) then
          ClientFlag:= ClientFlag or trunc(power(2, GetEnumValue(TypeInfo(TMYSQL_CLIENT_OPTIONS),sMy_client_Opt)));
    end;

  { Set SSL properties before connect}
  SslKey := nil;
  SslCert := nil;
  SslCa := nil;
  SslCaPath := nil;
  SslCypher := nil;
  {EgonHugeist: If these Paramters MUST BE UTF8 then leave Param ceUTF8 in the
    ZPlainString-function like else remove it and it adapts to default codepage}
  if StrToBoolEx(Info.Values['MYSQL_SSL']) then
    begin
       if Info.Values['MYSQL_SSL_KEY'] <> '' then
          SslKey := PAnsiChar(ZPlainString(Info.Values['MYSQL_SSL_KEY'], ceUTF8));
       if Info.Values['MYSQL_SSL_CERT'] <> '' then
          SslCert := PAnsiChar(ZPlainString(Info.Values['MYSQL_SSL_CERT'], ceUTF8));
       if Info.Values['MYSQL_SSL_CA'] <> '' then
          SslCa := PAnsiChar(ZPlainString(Info.Values['MYSQL_SSL_CA'], ceUTF8));
       if Info.Values['MYSQL_SSL_CAPATH'] <> '' then
          SslCaPath := PAnsiChar(ZPlainString(Info.Values['MYSQL_SSL_CAPATH'], ceUTF8));
       if Info.Values['MYSQL_SSL_CYPHER'] <> '' then
          SslCypher := PAnsiChar(ZPlainString(Info.Values['MYSQL_SSL_CYPHER'], ceUTF8));
       GetPlainDriver.SslSet(FHandle, SslKey, SslCert, SslCa, SslCaPath,
          SslCypher);
       DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol,
          'SSL options set');
    end;

    { Connect to MySQL database. }
    if GetPlainDriver.RealConnect(FHandle, PAnsiChar(ZPlainString(HostName)),
                              PAnsiChar(ZPlainString(User)), PAnsiChar(ZPlainString(Password)),
                              PAnsiChar(ZPlainString(Database)), Port, nil,
                              ClientFlag) = nil then

    begin
      CheckMySQLError(GetPlainDriver, FHandle, lcConnect, LogMessage);
      DriverManager.LogError(lcConnect, PlainDriver.GetProtocol, LogMessage,
        0, SUnknownError);
      raise EZSQLException.Create(SCanNotConnectToServer);
    end;
    DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol, LogMessage);

    { Fix Bugs in certain Versions where real_conncet resets the Reconnect flag }
    if StrToBoolEx(Info.Values['MYSQL_OPT_RECONNECT']) then
    begin
      ClientVersion := GetPlainDriver.GetClientVersion;
      if ((ClientVersion>=50013) and (ClientVersion<50019)) or
         ((ClientVersion>=50100) and (ClientVersion<50106)) then
        GetPlainDriver.SetOptions(FHandle, MYSQL_OPT_RECONNECT, 'true');
    end;

    if FClientCodePage = '' then //was set on inherited Create(...)
      FClientCodePage := sMy_client_Char_Set;


    if (FClientCodePage <> sMy_client_Char_Set) then
    begin
      SQL := PAnsiChar(ZPlainString(Format('SET NAMES %s', [FClientCodePage])));
      GetPlainDriver.ExecQuery(FHandle, SQL);
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, String(SQL));
      DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, String(SQL));
    end;
    Self.CheckCharEncoding(FClientCodePage);
    {EgonHugeist:
      CheckCharEncoding(FClientCharacterSet);
      This has to be done before any Query was send to Driver.
      This procedure sets also the internal TZCharEncoding to ZCompatibility-Objects...
      Now use the new Functions to get encoded Strings:
        function ZString(const Ansi: AnsiString; const Encoding: TZCharEncoding = ceDefault): String;
        function ZPlainString(const Str: String; const Encoding: TZCharEncoding = ceDefault): AnsiString;
      These functions do auto arrange the in/out-coming AnsiStrings in
      dependency of the used CharacterSet and Compiler
      they defend the StringDataLoss}
    { Sets a client codepage. }

    { Sets transaction isolation level. }
    OldLevel := TransactIsolationLevel;
    TransactIsolationLevel := tiNone;
    SetTransactionIsolation(OldLevel);

    { Sets an auto commit mode. }
    OldAutoCommit := AutoCommit;
    AutoCommit := True;
    SetAutoCommit(OldAutoCommit);
  except
    GetPlainDriver.Close(FHandle);
    GetPlainDriver.Despose(FHandle);
    FHandle := nil;
    raise;
  end;

  inherited Open;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZMySQLConnection.PingServer: Integer;
const
   PING_ERROR_ZEOSCONNCLOSED = -1;
var
   Closing: boolean;
begin
   Closing := FHandle = nil;
   if Closed or Closing then
      Result := PING_ERROR_ZEOSCONNCLOSED
   else
      Result := GetPlainDriver.Ping(FHandle);
end;

{**
  Escape a string so it's acceptable for the Connection's server.
  @param value string that should be escaped
  @return Escaped string
}
function TZMySQLConnection.EscapeString(Value: ZAnsiString): ZAnsiString;
begin
  Result := PlainDriver.EscapeString(Self.FHandle,  Value, ConSettings);
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
function TZMySQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLStatement.Create(GetPlainDriver, Self, Info, FHandle);
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
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZMySQLConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if Assigned(Info) then
    if StrToBoolEx(Info.Values['preferprepared']) then
      Result := TZMySQLPreparedStatement.Create(GetPlainDriver, Self, SQL, Info)
    else
      Result := TZMySQLEmulatedPreparedStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle)
  else
    Result := TZMySQLEmulatedPreparedStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle);
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
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZMySQLConnection.CreateCallableStatement(const SQL: string; Info: TStrings):
  IZCallableStatement;
begin
  Result := TZMySQLCallableStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Commit;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    If not GEtPlaindriver.Commit(FHandle) then
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native Commit call');
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, 'Native Commit call');
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Rollback;
begin
  if (TransactIsolationLevel <> tiNone) and (AutoCommit <> True)
    and not Closed then
  begin
    If not GetPlaindriver.Rollback(FHandle) then
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native Rollback call');
    DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, 'Native Rollback call');
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
procedure TZMySQLConnection.Close;
var
  LogMessage: string;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;

  GetPlainDriver.Close(FHandle);
  GetPlainDriver.Despose(FHandle);
  FHandle := nil;
  LogMessage := Format('DISCONNECT FROM "%s"', [Database]);
  DriverManager.LogMessage(lcDisconnect, GetPlainDriver.GetProtocol, LogMessage);
  inherited Close;
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
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZMySQLConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  SQL: PAnsiChar;
  testResult: Integer;
begin
  if TransactIsolationLevel <> Level then
  begin
    inherited SetTransactionIsolation(Level);
    testResult := 1;
    if not Closed then
    begin
      case TransactIsolationLevel of
        tiNone, tiReadUncommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
            testResult := GetPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiReadCommitted:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED';
            testResult := GetPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiRepeatableRead:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ';
            testResult := GetPlainDriver.ExecQuery(FHandle, SQL);
          end;
        tiSerializable:
          begin
            SQL := 'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE';
            testResult := GetPlainDriver.ExecQuery(FHandle, SQL);
          end;
        else
          SQL := '';
      end;
      if (testResult <> 0) then
          CheckMySQLError(GetPlainDriver, FHandle, lcExecute, String(SQL));
      if SQL <> '' then
        DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, String(SQL));
    end;
  end;
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
procedure TZMySQLConnection.SetAutoCommit(AutoCommit: Boolean);
begin
  if AutoCommit <> Self.AutoCommit then
  begin
    inherited SetAutoCommit(AutoCommit);

    if not Closed then
    begin
      if not GetPlaindriver.SetAutocommit(FHandle, AutoCommit) then
        CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native SetAutoCommit '+BoolToStrEx(AutoCommit)+'call');
      DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, 'Native SetAutoCommit '+BoolToStrEx(AutoCommit)+'call');
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
 Result := ConvertMySQLVersionToSQLVersion( GetPlainDriver.GetClientVersion );
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
 Result := ConvertMySQLVersionToSQLVersion( GetPlainDriver.GetServerVersion(FHandle) );
 CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'mysql_get_server_version()');
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandle: PZMySQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a MySQL plain driver interface.
  @return a MySQL plain driver interface.
}
function TZMySQLConnection.GetPlainDriver: IZMySQLPlainDriver;
begin
  Result := Self.PlainDriver as IZMySQLPlainDriver;
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result := BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZMySQLConnection.GetEscapeString(const Value: ZWideString): ZWideString;
begin
  Result := inherited GetEscapeString(GetPlainDriver.EscapeString(FHandle, Value, ConSettings));
end;

function TZMySQLConnection.GetEscapeString(const Value: ZAnsiString): ZAnsiString;
begin
  Result := inherited GetEscapeString(GetPlainDriver.EscapeString(FHandle, Value, ConSettings));
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;
end.

