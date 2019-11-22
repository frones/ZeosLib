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

  IZSQLite3Transaction = interface(IZTransaction)
    ['{75E0BA9A-2BD4-487F-8891-9435D900B729}']
    function GetConnectionHandle: Psqlite;
    function IsReadOnly: Boolean;
    procedure ReleaseSavePoint(const Transaction: IZTransaction);
  end;

  IZSQLiteSavePoint = interface
    ['{37210672-F9DD-45D7-843D-6259FCD2AD6D}']
    function GetOwnerTransaction: IZSQLite3Transaction;
  end;

  {** Represents a SQLite specific connection interface. }
  IZSQLiteConnection = interface (IZConnection)
    ['{A4B797A9-7CF7-4DE9-A5BB-693DD32D07D2}']
    function GetConnectionHandle: Psqlite;
    function GetUndefinedVarcharAsStringLength: Integer;
    function enable_load_extension(OnOff: Integer): Integer;
    function load_extension(zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;
  end;

  TZSQLiteConnection = class;

  TSQLite3TransactionBehavior = (tbDEFERRED, tbIMMEDIATE, tbEXCLUSIVE);
  TZAbstractSQLiteTransaction = class(TZCodePagedObject, IImmediatelyReleasable)
  private
    FHandle: PSQLite;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZSQLiteConnection;
  public //IZSQLite3Transaction
    function GetConnectionHandle: Psqlite;
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
  public
    Constructor Create(Handle: PSQLite; Owner: TZSQLiteConnection);
    procedure BeforeDestruction; override;
  end;

  TZSQLiteTransaction = class(TZAbstractSQLiteTransaction, IZTransaction,
    IZSQLite3Transaction, IImmediatelyReleasable)
  private
    FAutoCommit: Boolean;
    fTransactionBehavior: TSQLite3TransactionBehavior;
    FCommitStmt, FRollBackStmt, fBeginStmt: Psqlite3_stmt;
    FSavePoints: IZCollection;
    FReadOnly: Boolean;
  public //IZTransaction
    procedure Commit;
    procedure Rollback;
    function SavePoint(const AName: String): IZTransaction;
  public //IZSQLite3Transaction
    function StartTransaction: Integer;
    function IsReadOnly: Boolean;
    procedure ReleaseSavePoint(const Transaction: IZTransaction);
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
  public
    Constructor Create(Handle: PSQLite; Owner: TZSQLiteConnection;
      ReadOnly: Boolean; TransactionBehavior: TSQLite3TransactionBehavior);
    procedure BeforeDestruction; override;
  end;

  TZSQLiteSavePoint = class(TZCodePagedObject, IZTransaction)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZSQLiteTransaction;
    fName: RawByteString;
  public //IZTransaction
    procedure Commit;
    procedure Rollback;
    function SavePoint(const AName: String): IZTransaction;
    function StartTransaction: Integer;
  public
    Constructor Create(const Name: String; Owner: TZSQLiteTransaction);
  end;

  {** Implements SQLite Database Connection. }

  { TZSQLiteConnection }
  TZSQLiteConnection = class(TZAbstractDbcConnection, IZSQLiteConnection,
    IZTransactionManager)
  private
    FUndefinedVarcharAsStringLength: Integer;
    FCatalog: string;
    FPlainDriver: TZSQLitePlainDriver;
    fRWTransaction: IZSQLite3Transaction; //sqlite support one active write transaction only
    fROTransactions: IZCollection; //simultan (not nested) readonly transaction container
    fActiveTransaction: array[Boolean] of IZSQLite3Transaction;
    procedure InternalExecute(const SQL: RawByteString); overload;
    procedure InternalExecute(Handle: Psqlite; const SQL: RawByteString); overload;
    procedure InternalExecute(Handle: Psqlite; const SQL: RawByteString; var Stmt: Psqlite3_stmt); overload;
  protected
    procedure InternalCreate; override;
  public //IZTransactionManager
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    procedure ReleaseTransaction(const Transaction: IZTransaction);
    procedure SetActiveTransaction(const Value: IZTransaction);
    function GetActiveTransaction: IZTransaction;
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
    procedure SetReadOnly(Value: Boolean); override;

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
  ZCollections
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
  CheckCharEncoding('UTF-8');
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values[DSProps_UndefVarcharAsStringLength], 0);
end;

procedure TZSQLiteConnection.InternalExecute(Handle: Psqlite;
  const SQL: RawByteString; var Stmt: Psqlite3_stmt);
var PZTail: PAnsiChar;
  Status: Integer;
begin
  if Pointer(SQL) = nil then
    Exit;
  if Stmt = nil then begin
    Status := FPlainDriver.sqlite3_prepare_v2(Handle,
      Pointer(SQL), Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Stmt, pZTail);
    if not Status in [SQLITE_OK, SQLITE_DONE] then
      CheckSQLiteError(FPlainDriver, Handle, Status, lcPrepStmt,
        SQL, ConSettings)
  end;
  Status := FPlainDriver.sqlite3_step(Stmt);
  try
    if not Status in [SQLITE_OK, SQLITE_DONE] then
      CheckSQLiteError(FPlainDriver, Handle, Status, lcTransaction, SQL, ConSettings)
  finally
    FPlainDriver.sqlite3_reset(Stmt);
    if Assigned(DriverManager) and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
  end;
end;

procedure TZSQLiteConnection.InternalExecute(const SQL: RawByteString);
begin
  InternalExecute(fActiveTransaction[ReadOnly].GetConnectionHandle, SQL);
end;

procedure TZSQLiteConnection.InternalExecute(Handle: Psqlite;
  const SQL: RawByteString);
var Stmt: Psqlite3_stmt;
  Status: Integer;
begin
  Stmt := nil;
  try
    InternalExecute(Handle, SQL, Stmt);
  finally
    if Stmt <> nil then begin
      Status := FPlainDriver.sqlite3_finalize(Stmt);
        CheckSQLiteError(FPlainDriver, Handle, Status, lcTransaction, SQL, ConSettings);
    end;
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
  ErrorCode := FPlainDriver.sqlite3_key(fActiveTransaction[ReadOnly].GetConnectionHandle, Pointer(Raw), Length(Raw));
  {$ELSE}
  ErrorCode := FPlainDriver.sqlite3_key(fActiveTransaction[ReadOnly].GetConnectionHandle, Pointer(Key), Length(Key));
  {$ENDIF}
  Result := ErrorCode;
end;

function TZSQLiteConnection.load_extension(zFile: PAnsiChar; zProc: Pointer;
  var pzErrMsg: PAnsiChar): Integer;
begin
  Result := FPlainDriver.sqlite3_load_extension(fActiveTransaction[ReadOnly].GetConnectionHandle, zFile, zProc, pzErrMsg);
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
  ErrorCode := FPlainDriver.sqlite3_rekey(fActiveTransaction[ReadOnly].GetConnectionHandle, Pointer(Raw), Length(Raw));
  {$ELSE}
  ErrorCode := FPlainDriver.sqlite3_rekey(fActiveTransaction[ReadOnly].GetConnectionHandle, Pointer(Key), Length(Key));
  {$ENDIF}
  Result := ErrorCode;
end;

procedure TZSQLiteConnection.ReleaseTransaction(
  const Transaction: IZTransaction);
var idx: Integer;
  Trans: IZTransaction;
  B: Boolean;
begin
  idx := fROTransactions.IndexOf(Transaction);
  if (fRWTransaction <> nil) or (fRWTransaction <> nil) then begin
    if Idx <> -1
    then fROTransactions.Delete(Idx)
    else if (fRWTransaction.QueryInterface(IZTransaction, Trans) = S_OK) and (Trans = Transaction)
      then fRWTransaction := nil
      else fRWTransaction.ReleaseSavePoint(Transaction);
    for B := False to True do
      if (fActiveTransaction[B] <> nil) then begin
        fActiveTransaction[B].QueryInterface(IZTransaction, Trans);
        if (Trans = Transaction) then
          fActiveTransaction[B] := nil;
      end;
  end else
    raise EZSQLException.Create('release an invalid Transaction');
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZSQLiteConnection.Open;
begin
  if not Closed then
    Exit;
  if fROTransactions = nil then
    fROTransactions := TZCollection.Create;
  CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info).QueryInterface(IZSQLite3Transaction, fActiveTransaction[ReadOnly]);
  inherited Open;
  if not GetAutoCommit then
    fActiveTransaction[ReadOnly].StartTransaction;
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
  Result := TZSQLiteStatement.Create(Self, Info, fActiveTransaction[ReadOnly].GetConnectionHandle);
end;

const OpenMassage: array[Boolean] of RawByteString = ('CREATE READWRITE CONNECTION',
  'CREATE READONLY CONNECTION');

function TZSQLiteConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
var Handle: Psqlite;
  TmpInt: Integer;
  LogMessage, SQL: RawByteString;
  TransactionBehavior: TSQLite3TransactionBehavior;
begin
  if Readonly or (fRWTransaction = nil) then begin
    Handle := nil;
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
    TmpInt := FPlainDriver.sqlite3_open(Pointer(SQL), Handle);
    if TmpInt <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, Handle, TmpInt, lcConnect, LogMessage, ConSettings);
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);
    try
      { Turn on encryption if requested }
      if StrToBoolEx(Info.Values[ConnProps_Encrypted]) and Assigned(FPlainDriver.sqlite3_key) and (Password <> '') then begin
        SQL := {$IFDEF UNICODE}UTF8String{$ENDIF}(Password);
        CheckSQLiteError(FPlainDriver, Handle,
          FPlainDriver.sqlite3_key(Handle, Pointer(SQL), Length(SQL)),
          lcConnect, 'sqlite3_key', ConSettings);
      end;
      { Set busy timeout if requested }
      TmpInt := StrToIntDef(Info.Values[ConnProps_BusyTimeout], -1);
      if TmpInt >= 0 then
        FPlainDriver.sqlite3_busy_timeout(Handle, TmpInt);
      { pimp performance }
      InternalExecute(Handle, 'PRAGMA cache_size = '+IntToRaw(StrToIntDef(Info.Values[ConnProps_CacheSize], 10000)));

      //see http://www.sqlite.org/pragma.html#pragma_synchronous
      //0 brings best performance
      if Info.Values[ConnProps_Synchronous] <> '' then
        InternalExecute(Handle, 'PRAGMA synchronous = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_Synchronous]));

      //see http://www.sqlite.org/pragma.html#pragma_locking_mode
      //EXCLUSIVE brings best performance
      if Info.Values[ConnProps_LockingMode] <> '' then
        InternalExecute(Handle, 'PRAGMA locking_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_LockingMode]));

      if Info.Values[ConnProps_journal_mode] <> '' then
        InternalExecute(Handle, 'PRAGMA journal_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_journal_mode]));

      if ( FClientCodePage <> '' ) and (FClientCodePage <> 'UTF-8') then
        InternalExecute(Handle, 'PRAGMA encoding = '''+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage)+'''');

      InternalExecute(Handle, 'PRAGMA show_datatypes = ON');

      if Info.Values[ConnProps_ForeignKeys] <> '' then
        InternalExecute(Handle, 'PRAGMA foreign_keys = '+BoolStrIntsRaw[StrToBoolEx(Info.Values[ConnProps_ForeignKeys])] );
    finally
      if ReadOnly
      then TransactionBehavior := tbDEFERRED
      else if (TransactIsolationLevel = tiReadCommitted)
        then TransactionBehavior := tbEXCLUSIVE //emulate a readblocking transaction
        else if UpperCase(Params.Values[DSProps_TransactionBehaviour]) = 'IMMEDIATE'
          then TransactionBehavior := tbIMMEDIATE
          else if UpperCase(Params.Values[DSProps_TransactionBehaviour]) = 'EXCLUSIVE' then begin
            inherited SetTransactionIsolation(TransactIsolationLevel);
            TransactionBehavior := tbEXCLUSIVE;
          end else TransactionBehavior := tbDEFERRED;
      Result := TZSQLiteTransaction.Create(Handle, Self, ReadOnly, TransactionBehavior);
      if ReadOnly
      then fROTransactions.Add(Result)
      else Result.QueryInterface(IZSQLite3Transaction, fRWTransaction);
    end;
  end else
    raise EZSQLException.Create('SQLite3 does not support multiple simultaneous write transactions');
end;

function TZSQLiteConnection.enable_load_extension(OnOff: Integer): Integer;
begin
  Result := FPlainDriver.sqlite3_enable_load_extension(fActiveTransaction[ReadOnly].GetConnectionHandle, OnOff);
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
  Result := TZSQLiteCAPIPreparedStatement.Create(Self, SQL, Info, fActiveTransaction[ReadOnly].GetConnectionHandle);
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
  FPlainDriver.sqlite3_interrupt(fActiveTransaction[ReadOnly].GetConnectionHandle);
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
    if not AutoCommit
    then with fActiveTransaction[readOnly] do begin
      Commit;
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
    if not AutoCommit
    then with fActiveTransaction[readOnly] do begin
      Rollback;
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
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  fRWTransaction := nil;
  if fROTransactions <> nil then
    fROTransactions.Clear;
  fActiveTransaction[False] := nil;
  fActiveTransaction[True] := nil;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZSQLiteConnection.GetActiveTransaction: IZTransaction;
begin
  Result := fActiveTransaction[ReadOnly]
end;

function TZSQLiteConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

function TZSQLiteConnection.GetClientVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(FPlainDriver.sqlite3_libversion);
end;

procedure TZSQLiteConnection.SetActiveTransaction(const Value: IZTransaction);
var Transaction: IZSQLite3Transaction;
  SavePoint: IZSQLiteSavePoint;
begin
  SavePoint := nil;
  if (Value = nil) or (Value.QueryInterface(IZSQLite3Transaction, Transaction) <> S_OK) or
     (Value.QueryInterface(IZSQLiteSavePoint, SavePoint) <> S_OK) then
    raise EZSQLException.Create('invalid sqlite transaction');
  if SavePoint <> nil then
    Transaction := SavePoint.GetOwnerTransaction;
  fActiveTransaction[Transaction.IsReadOnly] := Transaction;
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
    if not AutoCommit and not Closed then begin
      if ReadOnly then
        raise Exception.Create(SInvalidOpInAutoCommit);
      if fRWTransaction <> nil then
        fRWTransaction.Rollback;
    end;
    inherited SetAutoCommit(Value);
    if not Value and not Closed then
      fRWTransaction.StartTransaction;
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
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZSQLiteConnection.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then begin
    if not Closed then
      if (Value and (fRWTransaction = nil)) or
         (not Value and (fROTransactions.Count = 0)) then
        CreateTransaction(AutoCommit, Value, TransactIsolationLevel, Info).QueryInterface(IZSQLite3Transaction, fActiveTransaction)
    else if Value
      then fROTransactions[fROTransactions.Count -1].QueryInterface(IZSQLite3Transaction, fActiveTransaction)
      else fRWTransaction.QueryInterface(IZSQLite3Transaction, fActiveTransaction);
    inherited SetReadOnly(Value);
  end;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZSQLiteConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone then
    Level := tiSerializable;
  {if Level <> tiSerializable then
    raise EZSQLException.Create('unsupported transaction isolation level');}
  if Level <> GetTransactionIsolation then
    inherited SetTransactionIsolation(Level);
end;

function TZSQLiteConnection.StartTransaction: Integer;
begin
  if ReadOnly
  then raise EZSQLException.Create('useless savepoints for readonly transaction')
  else begin
    if fRWTransaction = nil then
      CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info).QueryInterface(IZSQLite3Transaction, fActiveTransaction[False]);
    Result := 1+fRWTransaction.StartTransaction;
  end;
end;

{**
  Gets a reference to SQLite connection handle.
  @return a reference to SQLite connection handle.
}
function TZSQLiteConnection.GetConnectionHandle: Psqlite;
begin
  if fActiveTransaction[ReadOnly] = nil
  then Result := nil
  else Result := fActiveTransaction[ReadOnly].GetConnectionHandle
end;

function TZSQLiteConnection.GetServerProvider: TZServerProvider;
begin
  Result := spSQLite;
end;

function TZSQLiteConnection.GetHostVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(fPlainDriver.sqlite3_libversion);
end;

{ TZAbstractSQLiteTransaction }

procedure TZAbstractSQLiteTransaction.BeforeDestruction;
var
  LogMessage: RawByteString;
  ErrorCode: Integer;
begin
  inherited;
  if FHandle <> nil then begin
    LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
    ErrorCode := FOwner.FPlainDriver.sqlite3_close(FHandle);
    FHandle := nil;
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FOwner.FPlainDriver, FHandle, ErrorCode,
        lcOther, LogMessage, ConSettings);
    if Assigned(DriverManager) and DriverManager.HasLoggingListener then //thread save
      DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
  end;
end;

constructor TZAbstractSQLiteTransaction.Create(Handle: PSQLite;
  Owner: TZSQLiteConnection);
begin
  inherited Create;
  fOwner := Owner;
  FHandle := Handle;
end;

function TZAbstractSQLiteTransaction.GetConnectionHandle: Psqlite;
begin
  Result := FHandle;
end;

procedure TZAbstractSQLiteTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FHandle := Nil;
end;

{ TZSQLiteTransaction }

procedure TZSQLiteTransaction.BeforeDestruction;
  procedure Finalize(var Stmt: Psqlite3_stmt);
  var Status: Integer;
  begin
    Status := FOwner.FPlainDriver.sqlite3_finalize(Stmt);
    Stmt := nil;
    if Status <> SQLITE_OK then
      CheckSQLiteError(FOwner.FPlainDriver, FHandle, Status,
        lcOther, 'sqlite3_finalize', ConSettings);
  end;
begin
  try
    if not FAutoCommit then
      Rollback;
  finally
    if FHandle <> nil then try
      if FCommitStmt <> nil then Finalize(FCommitStmt);
      if FRollBackStmt <> nil then finalize(FRollBackStmt);
      if fBeginStmt <> nil then finalize(fBeginStmt);
    finally
      inherited BeforeDestruction;
    end;
  end;
end;

procedure TZSQLiteTransaction.Commit;
var SavePoint: IZTransaction;
begin
  if FAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);
  if FSavePoints.Count > 0 then begin
    FSavePoints[FSavePoints.Count-1].QueryInterface(IZTransaction, SavePoint);
    SavePoint.Commit;
  end else
    FOwner.InternalExecute(FHandle, 'COMMIT TRANSACTION', FCommitStmt);
  FAutoCommit := True;
end;

constructor TZSQLiteTransaction.Create(Handle: PSQLite;
  Owner: TZSQLiteConnection; ReadOnly: Boolean;
  TransactionBehavior: TSQLite3TransactionBehavior);
begin
  inherited Create(Handle, Owner);
  ConSettings := Owner.ConSettings;
  FHandle := Handle;
  FAutoCommit := True;
  FReadOnly := ReadOnly;
  fTransactionBehavior := TransactionBehavior;
  FSavePoints := TZCollection.Create;
end;

function TZSQLiteTransaction.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TZSQLiteTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FCommitStmt := nil;
  FRollBackStmt := nil;
  fBeginStmt := nil;
  FSavePoints.Clear;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZSQLiteTransaction.ReleaseSavePoint(
  const Transaction: IZTransaction);
var idx, j: Integer;
begin
  idx := FSavePoints.IndexOf(Transaction);
  if Idx <> -1 then
    for J := FSavePoints.Count -1 downto idx do //flush all savepoints after
      FSavePoints.Delete(J);
  {$IFDEF DEBUG}Assert(idx > -1, 'Wrong remove transaction behavior');{$ENDIF}
end;

procedure TZSQLiteTransaction.Rollback;
var SavePoint: IZTransaction;
begin
  if FAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);
  if FSavePoints.Count > 0 then begin
    FSavePoints[FSavePoints.Count-1].QueryInterface(IZTransaction, SavePoint);
    SavePoint.Commit;
  end else
    FOwner.InternalExecute(FHandle, 'ROLLBACK TRANSACTION', FRollbackStmt);
  FAutoCommit := True;
end;

function TZSQLiteTransaction.SavePoint(const AName: String): IZTransaction;
begin
  if FAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);
  Result := TZSQLiteSavePoint.Create(AName, Self);
  Result.StartTransaction;
  FSavePoints.Add(Result);
end;

const TransBehaviors: array[TSQLite3TransactionBehavior] of RawByteString = (
  'DEFERRED', 'IMMEDIATE', 'EXCLUSIVE');

function TZSQLiteTransaction.StartTransaction: Integer;
var Transaction: IZTransaction;
  SQLiteTransaction: IZSQLite3Transaction;
begin
  if FAutoCommit and not FreadOnly then begin
    FOwner.InternalExecute(FHandle, 'BEGIN '+TransBehaviors[fTransactionBehavior]+' TRANSACTION', fBeginStmt);
    Result := 1;
    FAutoCommit := False;
  end else begin
    Result := FSavePoints.Count+2;
    Transaction := SavePoint(ZFastCode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result));
    QueryInterface(IZSQLite3Transaction, SQLiteTransaction);
    if (FOwner.fActiveTransaction[fReadOnly] = SQLiteTransaction) or (
       (FSavePoints.Count > 1) and (FSavePoints[FSavePoints.Count-2].QueryInterface(IZSQLite3Transaction, SQLiteTransaction) = S_OK) and
       (FOwner.fActiveTransaction[fReadOnly] = SQLiteTransaction)) then
      Transaction.QueryInterface(IZSQLite3Transaction, FOwner.fActiveTransaction[fReadOnly])
  end;
end;

{ TZSQLiteSavePoint }

procedure TZSQLiteSavePoint.Commit;
begin
  try
    FOwner.FOwner.InternalExecute(FOwner.FHandle, 'RELEASE SAVEPOINT '+FName);
  finally
    FOwner.ReleaseSavePoint(Self);
  end;
end;

constructor TZSQLiteSavePoint.Create(const Name: String;
  Owner: TZSQLiteTransaction);
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

procedure TZSQLiteSavePoint.Rollback;
begin
  try
    FOwner.FOwner.InternalExecute(FOwner.FHandle, 'ROLLBACK TO '+FName);
  finally
    FOwner.ReleaseSavePoint(Self);
  end;
end;

function TZSQLiteSavePoint.SavePoint(const AName: String): IZTransaction;
begin
  Result := TZSQLiteSavePoint.Create(AName, FOwner);
end;

function TZSQLiteSavePoint.StartTransaction: Integer;
begin
  FOwner.FOwner.InternalExecute(FOwner.FHandle, 'SAVE POINT '+FName);
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
