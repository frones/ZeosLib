{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
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
  ZGenericSqlAnalyser, ZCompatibility;

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
    function GetByteBufferAddress: PByteBuffer;
    function GetPlainDriver: TZSQLitePlainDriver;
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      ErrorCode: Integer; const LogMessage: String;
      const Sender: IImmediatelyReleasable);
  end;

  TSQLite3TransactionAction = (traBeginDEFERRED, traBeginIMMEDIATE, traBeginEXCLUSIVE, traCommit, traRollBack);

  {** Implements SQLite Database Connection. }

  { TZSQLiteConnection }
  TZSQLiteConnection = class(TZAbstractSuccedaneousTxnConnection, IZConnection,
    IZSQLiteConnection, IZTransaction)
  private
    FUndefinedVarcharAsStringLength: Integer;
    FCatalog: string;
    FHandle: Psqlite;
    FLastWarning: EZSQLWarning;
    FPlainDriver: TZSQLitePlainDriver;
    FTransactionStmts: array[TSQLite3TransactionAction] of Psqlite3_stmt;
  protected
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory; var Stmt: Psqlite3_stmt); overload;
  public //IZSQLiteConnection
    function GetUndefinedVarcharAsStringLength: Integer;
    function enable_load_extension(OnOff: Integer): Integer;
    function load_extension(zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;
    function GetPlainDriver: TZSQLitePlainDriver;
    procedure HandleErrorOrWarning(LogCategory: TZLoggingCategory;
      ErrorCode: Integer; const LogMessage: String;
      const Sender: IImmediatelyReleasable);
  public
    procedure AfterConstruction; override;
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;

    function AbortOperation: Integer; override;
  public
    procedure Open; override;

    procedure Commit;
    procedure Rollback;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function StartTransaction: Integer;


    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;


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
  ZSysUtils, ZClasses, ZEncoding, ZMessages, ZFastCode,
  ZSqLiteToken, ZSqLiteAnalyser,
  ZDbcSqLiteStatement, ZDbcProperties, ZDbcSqLiteUtils, ZDbcSqLiteMetadata,
  ZDbcUtils;

{ TZSQLiteDriver }

{**
  Constructs this object with default properties.
}
constructor TZSQLiteDriver.Create;
begin
  inherited Create;
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

const cTransactionActionStmt: Array[TSQLite3TransactionAction] of RawByteString = (
  'BEGIN DEFERRED TRANSACTION',
  'BEGIN IMMEDIATE TRANSACTION',
  'BEGIN EXCLUSIVE TRANSACTION',
  'COMMIT TRANSACTION',
  'ROLLBACK TRANSACTION');

{ TZSQLiteConnection }

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
  SQL: RawByteString;
  TmpInt: Integer;
begin
  if not Closed then
    Exit;

  FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);
  {$IFDEF UNICODE}
  SQL := ZUnicodeToRaw(DataBase, zCP_UTF8);
  {$ELSE}
    {$IFDEF LCL}
    SQL := DataBase;
    {$ELSE}
    if ZEncoding.ZDetectUTF8Encoding(Pointer(DataBase), Length(DataBase)) = etANSI then begin
      SQL := '';
      PRawToRawConvert(Pointer(DataBase), Length(DataBase), zOSCodePage, zCP_UTF8, SQL)
    end else SQL := DataBase;
    {$ENDIF}
  {$ENDIF}
  //patch by omaga software see https://sourceforge.net/p/zeoslib/tickets/312/
  TmpInt := FPlainDriver.sqlite3_open(Pointer(SQL), FHandle);
  if TmpInt <> SQLITE_OK then
    HandleErrorOrWarning(lcConnect, TmpInt, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  { Turn on encryption if requested }
  if StrToBoolEx(Info.Values[ConnProps_Encrypted]) and Assigned(FPlainDriver.sqlite3_key) and (Password <> '') then begin
    SQL := {$IFDEF UNICODE}UTF8String{$ENDIF}(Password);
    TmpInt := FPlainDriver.sqlite3_key(FHandle, Pointer(SQL), Length(SQL));
    if TmpInt <> SQLITE_OK then
      HandleErrorOrWarning(lcConnect, TmpInt, 'SQLite.Key', IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;

  { Set busy timeout if requested }
  TmpInt := StrToIntDef(Info.Values[ConnProps_BusyTimeout], -1);
  if TmpInt >= 0 then
    FPlainDriver.sqlite3_busy_timeout(FHandle, TmpInt);

  inherited Open;

  { pimp performance }
  ExecuteImmediat('PRAGMA cache_size = '+IntToRaw(StrToIntDef(Info.Values[ConnProps_CacheSize], 10000)), lcExecute);

  //see http://www.sqlite.org/pragma.html#pragma_synchronous
  //0 brings best performance
  if Info.Values[ConnProps_Synchronous] <> '' then
    ExecuteImmediat('PRAGMA synchronous = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_Synchronous]), lcExecute);

  //see http://www.sqlite.org/pragma.html#pragma_locking_mode
  //EXCLUSIVE brings best performance
  if Info.Values[ConnProps_LockingMode] <> '' then
    ExecuteImmediat('PRAGMA locking_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_LockingMode]), lcExecute);

  if Info.Values[ConnProps_journal_mode] <> '' then
    ExecuteImmediat('PRAGMA journal_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values[ConnProps_journal_mode]), lcExecute);

  if ( FClientCodePage <> '' ) and (FClientCodePage <> 'UTF-8') then
    ExecuteImmediat('PRAGMA encoding = '''+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage)+'''', lcExecute);

  ExecuteImmediat(RawByteString('PRAGMA show_datatypes = ON'), lcExecute);

  if Info.Values[ConnProps_ForeignKeys] <> '' then
    ExecuteImmediat('PRAGMA foreign_keys = '+BoolStrIntsRaw[StrToBoolEx(Info.Values[ConnProps_ForeignKeys])], lcExecute);
  if not AutoCommit then begin
    AutoCommit := True;
    SetAutoCommit(False);
  end;
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

  @param Name a procedure or function identifier
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
{$IFDEF FPC} {$PUSH}
  {$WARN 5024 off : Parameter "Name,Info" not used}
  {$WARN 5033 off : Function result does not seem to be set}
{$ENDIF}
function TZSQLiteConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
function TZSQLiteConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZSQLiteCAPIPreparedStatement.Create(Self, SQL, Info);
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
function TZSQLiteConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
    Open;

  Result := TZSQLiteStatement.Create(Self, Info);
end;

function TZSQLiteConnection.enable_load_extension(OnOff: Integer): Integer;
begin
  Result := FPlainDriver.sqlite3_enable_load_extension(FHandle, OnOff);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "PZTail" does not seem to be initialized} {$ENDIF}
procedure TZSQLiteConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory; var Stmt: Psqlite3_stmt);
var PZTail: PAnsiChar;
  Status: Integer;
  LogSQL: String;
begin
  {$IFDEF UNICODE}
  LogSQL := ZRawToUnicode(SQL, zCP_UTF8);
  {$ELSE}
  LogSQL := SQL;
  {$ENDIF}
  if Pointer(SQL) = nil then
    Exit;
  if Stmt = nil then begin
    Status := FPlainDriver.sqlite3_prepare_v2(FHandle,
      Pointer(SQL), Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Stmt, pZTail);
    if (Status <> SQLITE_OK) and (Status <> SQLITE_DONE) then
      HandleErrorOrWarning(lcPrepStmt, Status, LogSQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
  Status := FPlainDriver.sqlite3_step(Stmt);
  try
    if (Status <> SQLITE_OK) and (Status <> SQLITE_DONE) then
      HandleErrorOrWarning(LoggingCategory, Status, LogSQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    FPlainDriver.sqlite3_reset(Stmt);
  end;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, URL.Protocol, LogSQL);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZSQLiteConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var Stmt: Psqlite3_stmt;
  Status: Integer;
begin
  Stmt := nil;
  try
    ExecuteImmediat(SQL, LoggingCategory, Stmt);
  finally
    if Stmt <> nil then begin
      Status := FPlainDriver.sqlite3_finalize(Stmt);
      if Status <> SQLITE_OK then
        HandleErrorOrWarning(lcUnprepStmt, Status, 'sqlite3_finalize',
          IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
end;

{**
  Starts a transaction support.
}
function TZSQLiteConnection.GetUndefinedVarcharAsStringLength: Integer;
begin
  Result := FUndefinedVarcharAsStringLength;
end;

{**
  Checks for possible sql errors.
  @param LogCategory a logging category.
  @param ErrorCode an error code.
  @param LogMessage a logging message.
}
procedure TZSQLiteConnection.HandleErrorOrWarning(
  LogCategory: TZLoggingCategory; ErrorCode: Integer; const LogMessage: String;
  const Sender: IImmediatelyReleasable);
var
  ErrorStr: RawByteString;
  P: PAnsiChar;
  L: NativeUInt;
  Writer: TZRawSQLStringWriter;
  AExceptionClass: EZSQLThrowableClass;
  AException: EZSQLThrowable;
  FormatStr: String;
begin
  if (ErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then Exit;
  ErrorStr := EmptyRaw;
  Writer := TZRawSQLStringWriter.Create(1024);
  try
    if Assigned(FPlainDriver.sqlite3_extended_errcode) then
      ErrorCode := FPlainDriver.sqlite3_extended_errcode(FHandle);
    if ( FHandle <> nil ) and ( Assigned(FPlainDriver.sqlite3_errstr) ) then begin
      P := FPlainDriver.sqlite3_errstr(ErrorCode);
      if P <> nil then begin
        L := StrLen(P);
        ZSysUtils.Trim(L, P);
      end else L := 0;
      if L > 0 then
      Writer.AddText(P, L, ErrorStr);
    end else case ErrorCode of
      SQLITE_ERROR:       Writer.AddText('SQL logic error or missing database', ErrorStr);
      SQLITE_INTERNAL:    Writer.AddText('internal SQLite implementation flaw', ErrorStr);
      SQLITE_PERM:        Writer.AddText('access permission denied', ErrorStr);
      SQLITE_ABORT:       Writer.AddText('callback requested query abort', ErrorStr);
      SQLITE_BUSY:        Writer.AddText('database is locked', ErrorStr);
      SQLITE_LOCKED:      Writer.AddText('database table is locked', ErrorStr);
      SQLITE_NOMEM:       Writer.AddText('out of memory', ErrorStr);
      SQLITE_READONLY:    Writer.AddText('attempt to write a readonly database', ErrorStr);
      SQLITE_INTERRUPT:   Writer.AddText('interrupted', ErrorStr);
      SQLITE_IOERR:       Writer.AddText('disk I/O error', ErrorStr);
      SQLITE_CORRUPT:     Writer.AddText('database disk image is malformed', ErrorStr);
      SQLITE_NOTFOUND:    Writer.AddText('table or record not found', ErrorStr);
      SQLITE_FULL:        Writer.AddText('database is full', ErrorStr);
      SQLITE_CANTOPEN:    Writer.AddText('unable to open database file', ErrorStr);
      SQLITE_PROTOCOL:    Writer.AddText('database locking protocol failure', ErrorStr);
      SQLITE_EMPTY:       Writer.AddText('table contains no data', ErrorStr);
      SQLITE_SCHEMA:      Writer.AddText('database schema has changed', ErrorStr);
      SQLITE_TOOBIG:      Writer.AddText('too much data for one table row', ErrorStr);
      SQLITE_CONSTRAINT:  Writer.AddText('constraint failed', ErrorStr);
      SQLITE_MISMATCH:    Writer.AddText('datatype mismatch', ErrorStr);
      SQLITE_MISUSE:      Writer.AddText('library routine called out of sequence', ErrorStr);
      SQLITE_NOLFS:       Writer.AddText('kernel lacks large file support', ErrorStr);
      SQLITE_AUTH:        Writer.AddText('authorization denied', ErrorStr);
      SQLITE_FORMAT:      Writer.AddText('auxiliary database format error', ErrorStr);
      SQLITE_RANGE:       Writer.AddText('bind index out of range', ErrorStr);
      SQLITE_NOTADB:      Writer.AddText('file is encrypted or is not a database', ErrorStr);
      else                Writer.AddText('unknown error', ErrorStr);
    end;
    if ( FHandle <> nil ) and ( Assigned(FPlainDriver.sqlite3_errmsg) ) then begin
      P := FPlainDriver.sqlite3_errmsg(FHandle);
      if P <> nil then begin
        L := StrLen(P);
        ZSysUtils.Trim(L, P);
      end else L := 0;
      if L > 0 then begin
        Writer.AddLineFeedIfNotEmpty(ErrorStr);
        Writer.AddText('Message : ', ErrorStr);
        Writer.AddText(P, L, ErrorStr);
      end;
    end;
    Writer.Finalize(ErrorStr);
  finally
    FreeAndNil(Writer);
  end;
  if ErrorStr = EmptyRaw then Exit;
  {$IFDEF UNICODE}
  FLogMessage := ZRawToUnicode(ErrorStr, zCP_UTF8);
  {$ELSE}
  FLogMessage := ErrorStr;
  {$ENDIF}
  if DriverManager.HasLoggingListener then
    LogError(LogCategory, ErrorCode, Sender, LogMessage, FLogMessage);
  if ErrorCode = SQLITE_WARNING
  then AExceptionClass := EZSQLWarning
  else if (ErrorCode = SQLITE_IOERR)
    then AExceptionClass := EZSQLConnectionLost
    else AExceptionClass := EZSQLException;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
    if LogCategory in [lcExecute, lcExecPrepStmt, lcPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
  then FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode, LogMessage])
  else FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode]);
  AException := AExceptionClass.CreateWithCode(ErrorCode, FLogMessage);
  if ErrorCode = SQLITE_WARNING then begin
    ClearWarnings;
    if not RaiseWarnings then begin
      FLastWarning := EZSQLWarning(AException);
      AException := nil;
    end;
  end else if (AExceptionClass = EZSQLConnectionLost) then begin
    if (Sender <> nil)
    then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(AException))
    else ReleaseImmediat(Self, EZSQLConnectionLost(AException));
  end;
  if AException <> nil then
     raise AException;
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
function TZSQLiteConnection.AbortOperation: Integer;
begin
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
procedure TZSQLiteConnection.AfterConstruction;
begin
  FPlainDriver := PlainDriver.GetInstance as TZSQLitePlainDriver;
  FMetadata := TZSQLiteDatabaseMetadata.Create(Self, Url);
  inherited AfterConstruction;
  //https://sqlite.org/pragma.html#pragma_read_uncommitted
  inherited SetTransactionIsolation(tiSerializable);
  CheckCharEncoding('UTF-8');
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values[DSProps_UndefVarcharAsStringLength], 0);
end;

procedure TZSQLiteConnection.Commit;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  if FSavePoints.Count > 0 then begin
    S := 'RELEASE SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    ExecuteImmediat(cTransactionActionStmt[traCommit], lcTransaction, FTransactionStmts[traCommit]);
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
  end
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Rollback;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := 'ROLLBACK TO '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    ExecuteImmediat(cTransactionActionStmt[traRollBack], lcTransaction, FTransactionStmts[traRollBack]);
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
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
procedure TZSQLiteConnection.InternalClose;
var
  ErrorCode: Integer;
  TransactionAction: TSQLite3TransactionAction;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  FSavePoints.Clear;
  try
    if not AutoCommit then begin
      AutoCommit := not FRestartTransaction;
      ExecuteImmediat(cTransactionActionStmt[traRollBack], lcTransaction, FTransactionStmts[traRollBack]);
    end;
  finally
    FLogMessage := 'DISCONNECT FROM "'+URL.Database+'"';
    for TransactionAction := low(TSQLite3TransactionAction) to high(TSQLite3TransactionAction) do
      if FTransactionStmts[TransactionAction] <> nil then begin
        FPlainDriver.sqlite3_finalize(FTransactionStmts[TransactionAction]);
        FTransactionStmts[TransactionAction] := nil;
      end;
    ErrorCode := FPlainDriver.sqlite3_close(FHandle);
    FHandle := nil;
    if ErrorCode <> SQLITE_OK then
      HandleErrorOrWarning(lcConnect, ErrorCode, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    if DriverManager.HasLoggingListener then //thread save
      DriverManager.LogMessage(lcDisconnect, URL.Protocol, FLogMessage);
  end;
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
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      ExecuteImmediat(cTransactionActionStmt[traCommit], lcTransaction, FTransactionStmts[traCommit]);
      AutoCommit := True;
    end else
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
  if Level = tiNone then
    Level := tiSerializable;
  if Level <> TransactIsolationLevel then begin
    if not AutoCommit and not Closed then
      raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
    TransactIsolationLevel := Level;
  end;
end;

function TZSQLiteConnection.StartTransaction: Integer;
var TransactionAction: TSQLite3TransactionAction;
  S: String;
  P: PChar;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    S := ZDbcUtils.DefineStatementParameter(Self, Info, TxnProps_TransactionBehaviour, 'DEFERRED');
    P := Pointer(S);
    case {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(P)^ or $20 of
      Ord('e'): TransactionAction := traBeginEXCLUSIVE;
      Ord('i'): TransactionAction := traBeginIMMEDIATE;
      else      TransactionAction := traBeginDEFERRED;
    end;
    ExecuteImmediat(cTransactionActionStmt[TransactionAction], lcTransaction, FTransactionStmts[TransactionAction]);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S) + 2;
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

function TZSQLiteConnection.GetPlainDriver: TZSQLitePlainDriver;
begin
  Result := FPlainDriver;
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

