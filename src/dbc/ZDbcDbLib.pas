{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               DBLib Connectivity Classes                }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLib;

interface

{$I ZDbc.inc}

uses
{$IFDEF FPC}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  Types, Classes, ZDbcConnection, ZDbcIntfs, ZCompatibility, ZDbcLogging,
  ZPlainDbLibDriver, ZPlainDbLibConstants, ZTokenizer, ZGenericSqlAnalyser,
  ZURL, ZPlainDriver;

type
  TDBLibProvider = (dpMsSQL, dpSybase);

  {** Implements DBLib Database Driver. }
  {$WARNINGS OFF}
  TZDBLibDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;
  {$WARNINGS ON}

  {** Represents a DBLib specific connection interface. }
  IZDBLibConnection = interface (IZConnection)
    ['{6B0662A2-FF2A-4415-B6B0-AAC047EA0671}']

    function FreeTDS: Boolean;
    function GetProvider: TDBLibProvider;
    function GetPlainDriver: IZDBLibPlainDriver;
    function GetConnectionHandle: PDBPROCESS;
    procedure InternalExecuteStatement(const SQL: string);
    procedure CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: string);
  end;

  {** Implements a generic DBLib Connection. }
  TZDBLibConnection = class(TZAbstractConnection, IZDBLibConnection)
  private
    FProvider: TDBLibProvider;
    FFreeTDS: Boolean;
    FDisposeCodePage: Boolean;
    function FreeTDS: Boolean;
    function GetProvider: TDBLibProvider;
    procedure ReStartTransactionSupport;
    procedure InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
    procedure DetermineMSDateFormat;
    function DetermineMSServerCollation: String;
    function DetermineMSServerCodePage(const Collation: String): Word;
  protected
    FHandle: PDBPROCESS;
    procedure InternalCreate; override;
    procedure InternalExecuteStatement(const SQL: string); virtual;
    procedure InternalLogin; virtual;
    function GetPlainDriver: IZDBLibPlainDriver;
    function GetConnectionHandle: PDBPROCESS;
    procedure CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: string); virtual;
    procedure StartTransaction; virtual;
  public
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    function NativeSQL(const SQL: string): string; override;

    procedure SetAutoCommit(AutoCommit: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    procedure SetReadOnly(ReadOnly: Boolean); override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
  end;

var
  {** The common driver manager object. }
  DBLibDriver: IZDriver;

implementation

uses
  SysUtils, ZSysUtils, ZMessages, ZDbcUtils, ZDbcDbLibStatement, ZEncoding,
  ZDbcDbLibMetadata, ZSybaseToken, ZSybaseAnalyser{$IFDEF FPC}, ZClasses{$ENDIF};

{ TZDBLibDriver }

{**
  Constructs this object with default properties.
}
constructor TZDBLibDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZDBLibMSSQL7PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZDBLibSybaseASE125PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS42MsSQLPlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS42SybasePlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS50PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS70PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS71PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS72PlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
}
{$WARNINGS OFF}
function TZDBLibDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZDBLibConnection.Create(Url);
end;
{$WARNINGS ON}

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZDBLibDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZDBLibDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZDBLibDriver.GetTokenizer: IZTokenizer;
begin
  if Tokenizer = nil then
    Tokenizer := TZSybaseTokenizer.Create;
  Result := Tokenizer;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZDBLibDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  if Analyser = nil then
    Analyser := TZSybaseStatementAnalyser.Create;
  Result := Analyser;
end;

{ TZDBLibConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZDBLibConnection.InternalCreate;
begin
  FDisposeCodePage := False;
  if Pos('mssql', LowerCase(Url.Protocol)) > 0  then
  begin
    FMetadata := TZMsSqlDatabaseMetadata.Create(Self, Url);
    FProvider := dpMsSQL;
  end
  else
    if Pos('sybase', LowerCase(Url.Protocol)) > 0 then
    begin
      FMetadata := TZSybaseDatabaseMetadata.Create(Self, Url);
      FProvider := dpSybase;
    end
    else
      FMetadata := nil;
  FFreeTDS := Pos('FreeTDS', Url.Protocol) > 0;

  FHandle := nil;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZDBLibConnection.Destroy;
begin
  Close;
  if FDisposeCodePage then
    Dispose(ConSettings^.ClientCodePage);
  inherited Destroy;
end;

function TZDBLibConnection.FreeTDS: Boolean;
begin
  Result := FFreeTDS;
end;

function TZDBLibConnection.GetProvider: TDBLibProvider;
begin
  Result := FProvider;
end;

{**
  Executes simple statements internally.
}
procedure TZDBLibConnection.InternalExecuteStatement(const SQL: string);
var
  LSQL: string;
  ASQL: ZAnsiString;
begin
  FHandle := GetConnectionHandle;
  if GetPlainDriver.dbCancel(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcExecute, SQL);
  if FProvider = dpMsSQL then
    LSQL := StringReplace(Sql, '\'#13, '\\'#13, [rfReplaceAll])
  else
    LSQL := SQL;

  ASQL := ZPlainString(LSQL);
    if GetPlainDriver.dbcmd(FHandle, PAnsiChar(ASQL)) <> DBSUCCEED then
      CheckDBLibError(lcExecute, LSQL);
  if GetPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcExecute, LSQL);
  repeat
    GetPlainDriver.dbresults(FHandle);
    GetPlainDriver.dbcanquery(FHandle);
  until GetPlainDriver.dbmorecmds(FHandle) = DBFAIL;
  CheckDBLibError(lcExecute, LSQL);
  DriverManager.LogMessage(lcExecute, PlainDriver.GetProtocol, LSQL);
end;

{**
  Login procedure can be overriden for special settings.
}
procedure TZDBLibConnection.InternalLogin;
var
  Loginrec: PLOGINREC;
  LogMessage: string;
  S: string;
  lLogFile  : String;
begin
  LogMessage := Format('CONNECT TO "%s"', [HostName]);
  LoginRec := GetPLainDriver.dbLogin;
  try
//Common parameters
    S := Info.Values['workstation'];
    if S <> '' then
      GetPlainDriver.dbSetLHost(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['appname'];
    if S <> '' then
      GetPlainDriver.dbSetLApp(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['language'];
    if S <> '' then
      GetPlainDriver.dbSetLNatLang(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['timeout'];
    if S <> '' then
      GetPlainDriver.dbSetLoginTime(StrToIntDef(S, 60));

    if FFreeTDS then
    begin
      if StrToBoolEx(Info.Values['log']) or StrToBoolEx(Info.Values['logging']) or
         StrToBoolEx(Info.Values['tds_dump']) then begin
           lLogFile := Info.Values['logfile'];
           if lLogFile = '' then
            lLogFile := Info.Values['log_file'];
           if lLogFile = '' then
            lLogFile := Info.Values['tds_dump_file'];
           if lLogFile = '' then
            lLogFile := ChangeFileExt(ParamStr(0), '.tdslog');
           (GetPlainDriver as IZFreeTDSPlainDriver).tdsDump_Open(lLogFile);
         end;
    end;


    //mssql specific parameters
    if ( FProvider = dpMsSQL ) then
    begin
      if ( StrToBoolEx(Info.Values['NTAuth']) or StrToBoolEx(Info.Values['trusted'])
        or StrToBoolEx(Info.Values['secure']) ) and ( not FFreeTDS ) then
      begin
        GetPlainDriver.dbsetlsecure(LoginRec);
        LogMessage := LogMessage + ' USING WINDOWS AUTHENTICATION';
      end
      else
      begin
        GetPlainDriver.dbsetluser(LoginRec, PAnsiChar(AnsiString(User)));
        GetPlainDriver.dbsetlpwd(LoginRec, PAnsiChar(AnsiString(Password)));
        LogMessage := LogMessage + Format(' AS USER "%s"', [User]);
      end;
    end;

    //sybase specific parameters
    if FProvider = dpSybase then
    begin
      S := Info.Values['codepage'];
      if S <> '' then
        GetPlainDriver.dbSetLCharSet(LoginRec, PAnsiChar(ZPlainString(S)));
      GetPlainDriver.dbsetluser(LoginRec, PAnsiChar(ZPlainString(User)));
      GetPlainDriver.dbsetlpwd(LoginRec, PAnsiChar(ZPlainString(Password)));
      LogMessage := LogMessage + Format(' AS USER "%s"', [User]);
    end;

    CheckDBLibError(lcConnect, LogMessage);
    FHandle := GetPlainDriver.dbOpen(LoginRec, PAnsiChar(AnsiString(HostName)));
    CheckDBLibError(lcConnect, LogMessage);
    DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol, LogMessage);
  finally
    GetPLainDriver.dbLoginFree(LoginRec);
  end;
end;

function TZDBLibConnection.GetPlainDriver: IZDBLibPlainDriver;
begin
  Result := PlainDriver as IZDBLibPlainDriver;
end;

function TZDBLibConnection.GetConnectionHandle: PDBPROCESS;
begin
  if FProvider = dpMsSQL then
    if GetPlainDriver.dbDead(FHandle) then
    begin
      Closed := True;
      Open;
    end;
  Result := FHandle;
end;

procedure TZDBLibConnection.CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: string);
begin
  try
    GetPlainDriver.CheckError(FHandle);
  except
    on E: Exception do
    begin
      DriverManager.LogError(LogCategory, PlainDriver.GetProtocol, LogMessage, 0, E.Message);
      raise;
    end;
  end;
end;

{**
  Starts a transaction support.
}
procedure TZDBLibConnection.ReStartTransactionSupport;
begin
  if Closed then
    Exit;

  if not (AutoCommit or (GetTransactionIsolation = tiNone)) then
    StartTransaction;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZDBLibConnection.Open;
var
  LogMessage: string;
begin
   if not Closed then
      Exit;

  InternalLogin;

  LogMessage := Format('USE %s', [Database]);
  if FProvider = dpMsSQL then
  begin
    if GetPlainDriver.dbUse(FHandle, PAnsiChar(AnsiString(Database))) <> DBSUCCEED then
      CheckDBLibError(lcConnect, LogMessage);
  end
  else
    if GetPlainDriver.dbUse(FHandle, PAnsiChar(ZPlainString(Database))) <> DBSUCCEED then
      CheckDBLibError(lcConnect, LogMessage);
  DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol, LogMessage);

  LogMessage := 'set textlimit=2147483647';
  if GetPlainDriver.dbsetopt(FHandle, GetPlainDriver.GetVariables.dboptions[Z_TEXTLIMIT] , '2147483647') <> DBSUCCEED then
    CheckDBLibError(lcConnect, LogMessage);
  DriverManager.LogMessage(lcConnect, PlainDriver.GetProtocol, LogMessage);

  InternalExecuteStatement('set textsize 2147483647 set quoted_identifier on');

  inherited Open;

  if FProvider = dpMsSQL then
  begin
    if FClientCodePage = '' then
    begin
      FDisposeCodePage := True;
      ConSettings^.ClientCodePage := New(PZCodePage);
      ConSettings^.ClientCodePage^.CP := ZDefaultSystemCodePage; //need a tempory CP for the SQL preparation
      ConSettings^.ClientCodePage^.Encoding := ceAnsi;
      ConSettings^.ClientCodePage^.Name := DetermineMSServerCollation;
      //ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := True;//???
      ConSettings^.ClientCodePage^.CP := DetermineMSServerCodePage(ConSettings^.ClientCodePage^.Name);
    end;
    DetermineMSDateFormat;
  end
  else
    ConSettings.DateFormat := 'yyyy/mm/dd';

  InternalSetTransactionIsolation(GetTransactionIsolation);
  ReStartTransactionSupport;
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
function TZDBLibConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibStatement.Create(Self, Info);
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
function TZDBLibConnection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibPreparedStatementEmulated.Create(Self, SQL, Info);
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
function TZDBLibConnection.CreateCallableStatement(
  const SQL: string; Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibCallableStatement.Create(Self, SQL, Info);
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
function TZDBLibConnection.NativeSQL(const SQL: string): string;
begin
  Result := SQL;
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
procedure TZDBLibConnection.SetAutoCommit(AutoCommit: Boolean);
begin
  if GetAutoCommit = AutoCommit then  Exit;
  if not Closed and AutoCommit then InternalExecuteStatement('commit');
  inherited;
  ReStartTransactionSupport;
end;

procedure TZDBLibConnection.InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
const
  IL: array[TZTransactIsolationLevel, 0..1] of string = (('READ COMMITTED', '1'), ('READ UNCOMMITTED', '0'), ('READ COMMITTED', '1'), ('REPEATABLE READ', '2'), ('SERIALIZABLE', '3'));
var
  Index: Integer;
  S: string;
begin
  Index := -1;
  if FProvider = dpMsSQL then Index := 0;
  if FProvider = dpSybase then Index := 1;

  S := 'SET TRANSACTION ISOLATION LEVEL ' + IL[GetTransactionIsolation, Index];
  InternalExecuteStatement(S);
  if not (AutoCommit) then
    InternalExecuteStatement('BEGIN TRANSACTION');
end;

procedure TZDBLibConnection.DetermineMSDateFormat;
var
  Stmt: IZStatement;
  Rs: IZResultSet;
begin
  Stmt := CreateRegularStatement(Self.Info);
  RS := Stmt.ExecuteQuery('SELECT dateformat FROM sys.syslanguages WHERE name = @@LANGUAGE');
  if RS.Next then
    ConSettings.DateFormat := RS.GetString(1);
  RS := nil;
  Stmt.close;
  Stmt := nil;
  if ConSettings.DateFormat = 'dmy' then
    ConSettings.DateFormat := 'dd/mm/yyyy'
  else if ConSettings.DateFormat = 'mdy' then
    ConSettings.DateFormat := 'mm/dd/yyyy'
  else
    ConSettings.DateFormat := 'yyyy/mm/dd'
end;

function TZDBLibConnection.DetermineMSServerCollation: String;
var
  Stmt: IZStatement;
  Rs: IZResultSet;
begin
  Stmt := CreateRegularStatement(Self.Info);
  try
    RS := Stmt.ExecuteQuery('SELECT DATABASEPROPERTYEX('+AnsiQuotedStr(DataBase, #39)+', ''Collation'') as DatabaseCollation');
    if RS.Next then
      Result := RS.GetString(1)
    else
      Result := '';
    RS := nil;
  except
    Result := '';
  end;
  Stmt.close;
  Stmt := nil;
end;

function TZDBLibConnection.DetermineMSServerCodePage(const Collation: String): Word;
var
  Stmt: IZStatement;
  Rs: IZResultSet;
begin
  Stmt := CreateRegularStatement(Self.Info);
  try
    RS := Stmt.ExecuteQuery('SELECT COLLATIONPROPERTY('+AnsiQuotedStr(Collation, #39)+', ''Codepage'') AS CodePage');
    if RS.Next then
      Result := RS.GetInt(1)
    else
      Result := High(Word);
    RS := nil;
  except
    Result := High(Word);
  end;
  Stmt.close;
  Stmt := nil;
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZDBLibConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if GetTransactionIsolation = Level then
    Exit;

  if not Closed and not AutoCommit and (GetTransactionIsolation <> tiNone) then
    InternalExecuteStatement('commit');

  inherited;

  if not Closed then
    InternalSetTransactionIsolation(Level);

  RestartTransactionSupport;
end;

{**
  Starts a new transaction. Used internally.
}
procedure TZDBLibConnection.StartTransaction;
begin
  InternalExecuteStatement('begin transaction');
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Commit;
begin
  if AutoCommit then
    raise Exception.Create(SCannotUseCommit);
  InternalExecuteStatement('commit');
  StartTransaction;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Rollback;
begin
  if AutoCommit then
    raise Exception.Create(SCannotUseRollBack);
  InternalExecuteStatement('rollback');
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
procedure TZDBLibConnection.Close;
var
  LogMessage: string;
begin
  if Closed or (Not Assigned(PlainDriver) )then
    Exit;

  if not GetPlainDriver.dbDead(FHandle) then
    InternalExecuteStatement('if @@trancount > 0 rollback');

  LogMessage := Format('CLOSE CONNECTION TO "%s" DATABASE "%s"', [HostName, Database]);
  if GetPlainDriver.dbclose(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcDisConnect, LogMessage);
  DriverManager.LogMessage(lcDisconnect, PlainDriver.GetProtocol, LogMessage);

  FHandle := nil;
  inherited;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZDBLibConnection.SetReadOnly(ReadOnly: Boolean);
begin
{ TODO -ofjanos -cAPI : I think it is not supported in this way }
  inherited;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZDBLibConnection.SetCatalog(const Catalog: string);
var
  LogMessage: string;
begin
  if (Catalog <> '') and not Closed then
  begin
    LogMessage := Format('SET CATALOG %s', [Catalog]);
    if FProvider = dpMsSQL then
    begin
      if GetPLainDriver.dbUse(FHandle, PAnsiChar(AnsiString(Catalog))) <> DBSUCCEED then
        CheckDBLibError(lcOther, LogMessage);
    end
    else
      if GetPLainDriver.dbUse(FHandle, PAnsiChar(ZPlainString(Catalog))) <> DBSUCCEED then
        CheckDBLibError(lcOther, LogMessage);
    DriverManager.LogMessage(lcOther, PLainDriver.GetProtocol, LogMessage);
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZDBLibConnection.GetCatalog: string;
begin
  Result := String(StrPas(GetPlainDriver.dbName(FHandle)));
  CheckDBLibError(lcOther, 'GETCATALOG');
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZDBLibConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZDBLibConnection.ClearWarnings;
var
  LogMessage: string;
begin
  if Closed then
    Exit;

  if not GetPlainDriver.dbDead(FHandle) then
    InternalExecuteStatement('if @@trancount > 0 rollback');

  LogMessage := Format('CLOSE CONNECTION TO "%s" DATABASE "%s"', [HostName, Database]);
  if GetPlainDriver.dbclose(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcDisConnect, LogMessage);
  DriverManager.LogMessage(lcDisconnect, GetPlainDriver.GetProtocol, LogMessage);

  FHandle := nil;
  inherited;
end;

initialization
  DBLibDriver := TZDBLibDriver.Create;
  DriverManager.RegisterDriver(DBLibDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(DBLibDriver);
  DBLibDriver := nil;
end.
