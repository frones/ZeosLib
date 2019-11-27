{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2019 Zeos Development Group       }
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

unit ZDbcSqLite;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcConnection, ZPlainSqLiteDriver, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZURL, ZCompatibility, ZClasses;

type

  {** Implements SQLite Database Driver. }
  TZSQLiteDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;


  {** Represents a SQLite specific connection interface. }
  IZSQLiteConnection = interface (IZConnection)
    ['{A4B797A9-7CF7-4DE9-A5BB-693DD32D07D2}']
    function GetConnectionHandle: Psqlite;
    function GetUndefinedVarcharAsStringLength: Integer;
    function enable_load_extension(OnOff: Integer): Integer;
    function load_extension(zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;
  end;

  IZSQLiteSavePoint = interface(IZTransaction)
    ['{37210672-F9DD-45D7-843D-6259FCD2AD6D}']
    function GetOwnerSession: IZSQLiteConnection;
  end;

  TZSQLiteConnection = class;

  TSQLite3TransactionBehavior = (tbDEFERRED, tbIMMEDIATE, tbEXCLUSIVE);

  TZSQLiteSavePoint = class(TZCodePagedObject, IZTransaction, IZSQLiteSavePoint)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZSQLiteConnection;
    fName: RawByteString;
  public //IZTransaction
    procedure Commit;
    procedure Rollback;
    function SavePoint(const AName: String): IZTransaction;
    function StartTransaction: Integer;
  public
    function GetOwnerSession: IZSQLiteConnection;
  public
    Constructor Create(const Name: String; Owner: TZSQLiteConnection);
  end;

  {** Implements SQLite Database Connection. }

  TSQLite3TransactionAction = (traBeginDEFERRED, traBeginIMMEDIATE, traBeginEXCLUSIVE, traCommit, traRollBack);
  TSQLite3TransactionStmt = record
    Stmt: Psqlite3_stmt;
    SQL: RawByteString;
  end;
  { TZSQLiteConnection }
  TZSQLiteConnection = class(TZAbstractDbcConnection, IZSQLiteConnection)
  private
    FUndefinedVarcharAsStringLength: Integer;
    FCatalog: string;
    FHandle: Psqlite;
    FPlainDriver: TZSQLitePlainDriver;
    FTransactionStmts: array[TSQLite3TransactionAction] of TSQLite3TransactionStmt;
    FSavePoints: IZCollection;
    procedure InternalExecute(const SQL: RawByteString); overload;
    procedure InternalExecute(const SQL: RawByteString; var Stmt: Psqlite3_stmt); overload;
  protected
    procedure InternalCreate; override;
  public //IZSQLiteConnection
    function GetUndefinedVarcharAsStringLength: Integer;
    function enable_load_extension(OnOff: Integer): Integer;
    function load_extension(zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;
  public
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    function AbortOperation: Integer; override;

    function StartTransaction: Integer;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetAutoCommit(Value: Boolean); override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;

    function GetConnectionHandle: Psqlite;

    function ReKey(const Key: string): Integer;
    function Key(const Key: string): Integer;

    function GetServerProvider: TZServerProvider; override;
  end;

var
  {** The common driver manager object. }
  SQLiteDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses
  ZSysUtils, ZDbcSqLiteStatement, ZSqLiteToken, ZFastCode, ZDbcProperties,
  ZDbcSqLiteUtils, ZDbcSqLiteMetadata, ZSqLiteAnalyser, ZEncoding, ZMessages,
  ZCollections, ZDbcUtils
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLiteDriver }

{**
  Constructs this object with default properties.
}
constructor TZSQLiteDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create, 'sqlite'));
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create));
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
function TZSQLiteDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZSQLiteConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZSQLiteDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZSQLiteDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZSQLiteDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZSQLiteTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZSQLiteDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZSQLiteStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZSQLiteConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZSQLiteConnection.InternalCreate;
begin
  FPlainDriver := TZSQLitePlainDriver(PlainDriver.GetInstance);
  FMetadata := TZSQLiteDatabaseMetadata.Create(Self, Url);
  //https://sqlite.org/pragma.html#pragma_read_uncommitted
  inherited SetTransactionIsolation(tiSerializable);
  FSavePoints := TZCollection.Create;
  CheckCharEncoding('UTF-8');
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values[DSProps_UndefVarcharAsStringLength], 0);
  FTransactionStmts[traBeginDEFERRED].SQL := 'BEGIN DEFERRED TRANSACTION';
  FTransactionStmts[traBeginIMMEDIATE].SQL := 'BEGIN IMMEDIATE TRANSACTION';
  FTransactionStmts[traBeginEXCLUSIVE].SQL := 'BEGIN EXCLUSIVE TRANSACTION';
  FTransactionStmts[traCommit].SQL := 'COMMIT TRANSACTION';
  FTransactionStmts[traRollBack].SQL := 'ROLLBACK TRANSACTION';
end;

procedure TZSQLiteConnection.InternalExecute(const SQL: RawByteString);
var Stmt: Psqlite3_stmt;
  Status: Integer;
begin
  Stmt := nil;
  try
    InternalExecute(SQL, Stmt);
  finally
    if Stmt <> nil then begin
      Status := FPlainDriver.sqlite3_finalize(Stmt);
        CheckSQLiteError(FPlainDriver, FHandle, Status, lcTransaction, SQL, ConSettings);
    end;
  end;
end;

procedure TZSQLiteConnection.InternalExecute(const SQL: RawByteString;
  var Stmt: Psqlite3_stmt);
var PZTail: PAnsiChar;
  Status: Integer;
begin
  if Pointer(SQL) = nil then
    Exit;
  if Stmt = nil then begin
    Status := FPlainDriver.sqlite3_prepare_v2(FHandle,
      Pointer(SQL), Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Stmt, pZTail);
    if not Status in [SQLITE_OK, SQLITE_DONE] then
      CheckSQLiteError(FPlainDriver, FHandle, Status, lcPrepStmt,
        SQL, ConSettings)
  end;
  Status := FPlainDriver.sqlite3_step(Stmt);
  try
    if not Status in [SQLITE_OK, SQLITE_DONE] then
      CheckSQLiteError(FPlainDriver, FHandle, Status, lcTransaction, SQL, ConSettings)
  finally
    FPlainDriver.sqlite3_reset(Stmt);
    if Assigned(DriverManager) and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Set encryption key for a database
  @param Key the key used to encrypt your database.
  @return error code from SQLite Key function.
}
function TZSQLiteConnection.Key(const Key: string):Integer;
var
  ErrorCode: Integer;
  {$IFDEF UNICODE}
  Raw: UTF8String;
  {$ENDIF}
begin
  {$IFDEF UNICODE}
  Raw := UTF8String(Key);
  ErrorCode := FPlainDriver.sqlite3_key(FHandle, Pointer(Raw), Length(Raw));
  {$ELSE}
  ErrorCode := FPlainDriver.sqlite3_key(FHandle, Pointer(Key), Length(Key));
  {$ENDIF}
  Result := ErrorCode;
end;

function TZSQLiteConnection.load_extension(zFile: PAnsiChar; zProc: Pointer;
  var pzErrMsg: PAnsiChar): Integer;
begin
  Result := FPlainDriver.sqlite3_load_extension(FHandle, zFile, zProc, pzErrMsg);
end;

{**
  Reencrypt a database with a new key. The old/current key needs to be
  set before calling this function.
  @param Key the new key used to encrypt your database.
  @return error code from SQLite ReKey function.
}
function TZSQLiteConnection.ReKey(const Key: string):Integer;
var
  ErrorCode: Integer;
  {$IFDEF UNICODE}
  Raw: UTF8String;
  {$ENDIF}
begin
  {$IFDEF UNICODE}
  Raw := UTF8String(Key);
  ErrorCode := FPlainDriver.sqlite3_rekey(FHandle, Pointer(Raw), Length(Raw));
  {$ELSE}
  ErrorCode := FPlainDriver.sqlite3_rekey(FHandle, Pointer(Key), Length(Key));
  {$ENDIF}
  Result := ErrorCode;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZSQLiteConnection.Open;
var
  LogMessage: RawByteString;
  SQL: RawByteString;
  TmpInt: Integer;
  Stmt: IZStatement;
begin
  if not Closed then
    Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
  {$IFDEF UNICODE}
  SQL := ZUnicodeToRaw(DataBase, zCP_UTF8);
  {$ELSE}
    {$IFDEF LCL}
    SQL := DataBase;
    {$ELSE}
    if ZEncoding.ZDetectUTF8Encoding(Pointer(DataBase), Length(DataBase)) = etANSI
    then PRawToRawConvert(Pointer(DataBase), Length(DataBase), zOSCodePage, zCP_UTF8, SQL)
    else SQL := DataBase;
    {$ENDIF}
  {$ENDIF}
  //patch by omaga software see https://sourceforge.net/p/zeoslib/tickets/312/
  TmpInt := FPlainDriver.sqlite3_open(Pointer(SQL), FHandle);
  if TmpInt <> SQLITE_OK then
    CheckSQLiteError(FPlainDriver, FHandle, TmpInt, lcConnect, LogMessage, ConSettings);
  DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

  { Turn on encryption if requested }
  if StrToBoolEx(Info.Values[ConnProps_Encrypted]) and Assigned(FPlainDriver.sqlite3_key) and (Password <> '') then begin
    SQL := {$IFDEF UNICODE}UTF8String{$ENDIF}(Password);
    CheckSQLiteError(FPlainDriver, FHandle,
      FPlainDriver.sqlite3_key(FHandle, Pointer(SQL), Length(SQL)),
      lcConnect, 'SQLite.Key', ConSettings);
  end;

  { Set busy timeout if requested }
  TmpInt := StrToIntDef(Info.Values[ConnProps_BusyTimeout], -1);
  if TmpInt >= 0 then
    FPlainDriver.sqlite3_busy_timeout(FHandle, TmpInt);

  inherited Open;

  Stmt := TZSQLiteStatement.Create(Self, Info, FHandle);
  { pimp performance }
  Stmt.ExecuteUpdate('PRAGMA cache_size = '+IntToRaw(StrToIntDef(Info.Values[ConnProps_CacheSize], 10000)));

  //see http://www.sqlite.org/pragma.html#pragma_synchronous
  //0 brings best performance
  if Info.Values[ConnProps_Synchronous] <> '' then
    Stmt.ExecuteUpdate('PRAGMA synchronous = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_Synchronous]));

  //see http://www.sqlite.org/pragma.html#pragma_locking_mode
  //EXCLUSIVE brings best performance
  if Info.Values[ConnProps_LockingMode] <> '' then
    Stmt.ExecuteUpdate('PRAGMA locking_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_LockingMode]));

  if Info.Values[ConnProps_journal_mode] <> '' then
    Stmt.ExecuteUpdate('PRAGMA journal_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_journal_mode]));

  if ( FClientCodePage <> '' ) and (FClientCodePage <> 'UTF-8') then
    Stmt.ExecuteUpdate('PRAGMA encoding = '''+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage)+'''');

  Stmt.ExecuteUpdate('PRAGMA show_datatypes = ON');

  if Info.Values[ConnProps_ForeignKeys] <> '' then
    Stmt.ExecuteUpdate('PRAGMA foreign_keys = '+BoolStrIntsRaw[StrToBoolEx(Info.Values[ConnProps_ForeignKeys])] );
  if not GetAutoCommit then
    StartTransaction;
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
function TZSQLiteConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;

  Result := TZSQLiteStatement.Create(Self, Info, FHandle);
end;

function TZSQLiteConnection.enable_load_extension(OnOff: Integer): Integer;
begin
  Result := FPlainDriver.sqlite3_enable_load_extension(FHandle, OnOff);
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
function TZSQLiteConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZSQLiteCAPIPreparedStatement.Create(Self, SQL, Info, FHandle);
end;

{**
  Starts a transaction support.
}
function TZSQLiteConnection.GetUndefinedVarcharAsStringLength: Integer;
begin
  Result := FUndefinedVarcharAsStringLength;
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
function TZSQLiteConnection.AbortOperation: Integer;
begin
  {$MESSAGE '.AbortOperation with SQLite is untested and might cause unexpected results!'}
  // https://sqlite.org/c3ref/interrupt.html
  FPlainDriver.sqlite3_interrupt(FHandle);
  Result := 1;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Commit;
begin
  if not Closed then
    if not AutoCommit then begin
      InternalExecute(FTransactionStmts[traCommit].SQL, FTransactionStmts[traCommit].Stmt);
      AutoCommit := True;
      StartTransaction;
    end else
      raise Exception.Create(SInvalidOpInAutoCommit);
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Rollback;
begin
  if not Closed then
    if not AutoCommit then begin
      InternalExecute(FTransactionStmts[traRollBack].SQL, FTransactionStmts[traRollBack].Stmt);
      AutoCommit := True;
      StartTransaction;
    end else
      raise Exception.Create(SInvalidOpInAutoCommit);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZSQLiteConnection.InternalClose;
var
  LogMessage: RawByteString;
  ErrorCode: Integer;
  TransactionAction: TSQLite3TransactionAction;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
  for TransactionAction := low(TSQLite3TransactionAction) to high(TSQLite3TransactionAction) do
    if FTransactionStmts[TransactionAction].Stmt <> nil then begin
      FPlainDriver.sqlite3_finalize(FTransactionStmts[TransactionAction].Stmt);
      FTransactionStmts[TransactionAction].Stmt := nil;
    end;
  ErrorCode := FPlainDriver.sqlite3_close(FHandle);
  FHandle := nil;
  CheckSQLiteError(FPlainDriver, FHandle, ErrorCode,
    lcOther, LogMessage, ConSettings);
  if Assigned(DriverManager) and DriverManager.HasLoggingListener then //thread save
    DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZSQLiteConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

function TZSQLiteConnection.GetClientVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(FPlainDriver.sqlite3_libversion);
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
procedure TZSQLiteConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> GetAutoCommit then begin
    if not GetAutoCommit and not Closed then begin
      FSavePoints.Clear;
      InternalExecute(FTransactionStmts[traRollBack].SQL, FTransactionStmts[traRollBack].Stmt);
    end;
    inherited SetAutoCommit(Value);
    if not Value and not Closed then
      StartTransaction;
  end;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZSQLiteConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZSQLiteConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level <> GetTransactionIsolation then begin
    if not GetAutoCommit and not Closed then
      InternalExecute(FTransactionStmts[traRollBack].SQL, FTransactionStmts[traRollBack].Stmt);
    inherited SetTransactionIsolation(Level);
    if not GetAutoCommit and not Closed then
      StartTransaction;
  end;
end;

function TZSQLiteConnection.StartTransaction: Integer;
var SavePoint: IZSQLiteSavePoint;
  TransactionAction: TSQLite3TransactionAction;
  S: String;
  P: PChar;
begin
  if ReadOnly
  then raise EZSQLException.Create('useless savepoints for readonly transaction')
  else begin
    if AutoCommit then begin
      AutoCommit := False;
      S := ZDbcUtils.DefineStatementParameter(Self, Info, DSProps_TransactionBehaviour, 'DEFERRED');
      P := Pointer(S);
      case {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(P)^ or $20 of
        Ord('e'): TransactionAction := traBeginEXCLUSIVE;
        Ord('i'): TransactionAction := traBeginIMMEDIATE;
        else      TransactionAction := traBeginDEFERRED;
      end;
      InternalExecute(FTransactionStmts[TransactionAction].SQL, FTransactionStmts[TransactionAction].Stmt);
      Result := 1;
    end else begin
      S := ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count+1);
      SavePoint := TZSQLiteSavePoint.Create(S, Self);
      Result := SavePoint.StartTransaction;
      FSavePoints.Add(SavePoint);
    end;
  end;
end;

{**
  Gets a reference to SQLite connection handle.
  @return a reference to SQLite connection handle.
}
function TZSQLiteConnection.GetConnectionHandle: Psqlite;
begin
  Result := FHandle;
end;

function TZSQLiteConnection.GetServerProvider: TZServerProvider;
begin
  Result := spSQLite;
end;

function TZSQLiteConnection.GetHostVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(fPlainDriver.sqlite3_libversion);
end;


{ TZSQLiteSavePoint }

procedure TZSQLiteSavePoint.Commit;
begin
  try
    FOwner.InternalExecute('RELEASE SAVEPOINT '+FName);
  finally
   // ReleaseSavePoint(Self);
  end;
end;

constructor TZSQLiteSavePoint.Create(const Name: String;
  Owner: TZSQLiteConnection);
begin
  inherited Create;
  ConSettings := Owner.ConSettings;
  {$IFDEF UNICODE}
  fName := ZUnicodeToRaw(Name, zCP_UTF8);
  {$ELSE}
  fName := Name;
  {$ENDIF}
  FOwner := Owner;
end;

function TZSQLiteSavePoint.GetOwnerSession: IZSQLiteConnection;
begin
  Result := FOwner;
end;

procedure TZSQLiteSavePoint.Rollback;
begin
  try
    FOwner.InternalExecute('ROLLBACK TO '+FName);
  finally
    //FOwner.ReleaseSavePoint(Self);
  end;
end;

function TZSQLiteSavePoint.SavePoint(const AName: String): IZTransaction;
begin
  Result := TZSQLiteSavePoint.Create(AName, FOwner);
end;

function TZSQLiteSavePoint.StartTransaction: Integer;
begin
  FOwner.InternalExecute('SAVE POINT '+FName);
  Result := FOwner.FSavePoints.Count+1;
end;

initialization
  SQLiteDriver := TZSQLiteDriver.Create;
  DriverManager.RegisterDriver(SQLiteDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(SQLiteDriver);
  SQLiteDriver := nil;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.

