{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes           }
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

unit ZDbcODBCCon;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZClasses, ZCompatibility, ZTokenizer,
  ZPlainODBCDriver,
  ZDbcIntfs, ZDbcConnection, ZGenericSqlAnalyser, ZDbcLogging;


type
  {** Implements OleDB Database Driver. }
  TZODBCDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetTokenizer: IZTokenizer; override;
  end;

  IZODBCConnection = Interface(IZConnection)
    ['{D149ABA3-AD8B-404F-A804-77608C596394}']
    procedure HandleErrorOrWarning(RETCODE: SQLRETURN; Handle: SQLHANDLE;
      HandleType: SQLSMALLINT; const LogMessage: SQLString;
      LoggingCategory: TZLoggingCategory; const Sender: IImmediatelyReleasable);
    function GetArrayRowSupported: Boolean;
    function GetArraySelectSupported: Boolean;
    function GetPlainDriver: TZODBC3PlainDriver;
    procedure SetLastWarning(Warning: EZSQLWarning);
    function ODBCVersion: SQLUSMALLINT;
    function GetByteBufferAddress: PByteBuffer;
  End;

  TZAbstractODBCConnection = class(TZAbstractSingleTxnConnection,
    IZTransaction)
  private
    fODBCPlainDriver: TZODBC3PlainDriver;
    fHDBC: SQLHDBC;
    fHENV: SQLHENV;
    fRetaining: Boolean;
    fLastWarning: EZSQLWarning;
    fCatalog: String; //cached
    fODBCVersion: SQLUSMALLINT;
    fArraySelectSupported, fArrayRowSupported: Boolean;
    fServerProvider: TZServerProvider;
    FRestartTransaction: Boolean;
    FWeakODBCConRefOfSelf: Pointer;
    procedure DetermineAttachmentCharset;
  protected
    procedure InternalClose; override;
    function SavePoint(const AName: String): Integer; virtual; abstract;
    procedure ReleaseSavePoint(Index: Integer); virtual; abstract;
    procedure RollBackTo(Index: Integer); virtual; abstract;
    function ComposeMessageString(RETCODE: SQLRETURN; Handle: SQLHANDLE;
      HandleType: SQLSMALLINT; Out SQLState: SQLString;
      out ErrorCode: SQLINTEGER): SQLString; virtual; abstract;
  public
    function GetArrayRowSupported: Boolean;
    function GetArraySelectSupported: Boolean;
    function GetPlainDriver: TZODBC3PlainDriver;
    procedure SetLastWarning(Warning: EZSQLWarning);
    function ODBCVersion: Word;
  public
    procedure HandleErrorOrWarning(RETCODE: SQLRETURN; Handle: SQLHANDLE;
      HandleType: SQLSMALLINT; const LogMessage: SQLString;
      LoggingCategory: TZLoggingCategory; const Sender: IImmediatelyReleasable);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;

    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean); override;
    /// <summary>Returns the Connection's current catalog name.</summary>
    /// <returns>the current catalog name or an empty string.</returns>
    function GetCatalog: string; override;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
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
    /// <summary>Opens a connection to database server with specified parameters.</summary>
    procedure Open; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    /// <summary>Returns the ServicerProvider for this connection. For ODBC
    ///  the connection must be opened to determine the provider. Otherwise
    ///  the provider is tested against the driver names</summary>
    /// <returns>the ServerProvider or spUnknown if not known.</returns>
    function GetServerProvider: TZServerProvider; override;
    /// <summary>Creates a generic tokenizer interface.</summary>
    /// <returns>a created generic tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Creates a generic statement analyser object.</summary>
    /// <returns>a created generic tokenizer object as interface.</returns>
    function GetStatementAnalyser: IZStatementAnalyser;
  end;

  TZODBCConnectionW = class(TZAbstractODBCConnection, IZODBCConnection,
    IZConnection)
  protected
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); override;
    function SavePoint(const AName: String): Integer; override;
    procedure ReleaseSavePoint(Index: Integer); override;
    procedure RollBackTo(Index: Integer); override;
    function ComposeMessageString(RETCODE: SQLRETURN; Handle: SQLHANDLE;
      HandleType: SQLSMALLINT; Out SQLState: SQLString;
      out ErrorCode: SQLINTEGER): SQLString; override;
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

    function NativeSQL(const SQL: string): string; override;
    function GetCatalog: string; override;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
  end;

  TZODBCConnectionA = class(TZAbstractODBCConnection, IZODBCConnection,
    IZConnection)
  protected
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
    function SavePoint(const AName: String): Integer; override;
    procedure ReleaseSavePoint(Index: Integer); override;
    procedure RollBackTo(Index: Integer); override;
    function ComposeMessageString(RETCODE: SQLRETURN; Handle: SQLHANDLE;
      HandleType: SQLSMALLINT; Out SQLState: SQLString;
      out ErrorCode: SQLINTEGER): SQLString; override;
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

    function NativeSQL(const SQL: string): string; override;
    function GetCatalog: string; override;
    /// <summary>Sets a catalog name in order to select a subspace of this
    ///  Connection's database in which to work. If the driver does not support
    ///  catalogs, it will silently ignore this request.</summary>
    /// <param>"value" new catalog name to be used.</param>
    procedure SetCatalog(const Catalog: string); override;
  end;

  TIZBlobDynArray = array of IZBlob;
  TIZBlobsDynArray = array of TIZBlobDynArray;

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}{$IFDEF NO_INLINE_SIZE_CHECK}Math,{$ENDIF}
  ZSysUtils, ZEncoding, ZFastCode, ZMessages,
  ZPlainDriver, ZODBCToken,
  ZPostgreSqlAnalyser, ZPostgreSqlToken, ZSybaseAnalyser, ZSybaseToken,
  ZInterbaseAnalyser, ZInterbaseToken, ZMySqlAnalyser, ZMySqlToken,
  ZOracleAnalyser, ZOracleToken,
  ZDbcODBCMetadata, ZDbcODBCStatement, ZDbcUtils, ZDbcProperties;

{ TZODBCDriver }

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

  @param url the TZURL of the database to which to connect
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZODBCDriver.Connect(const Url: TZURL): IZConnection;
begin
  if Url.Protocol = 'odbc_w' then
    Result := TZODBCConnectionW.Create(URL)
  else
    Result := TZODBCConnectionA.Create(URL)
end;

{**
  Constructs this object with default properties.
}
constructor TZODBCDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TODBC3UnicodePlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TODBC3RawPlainDriver.Create));
end;

{**
  Creates a ODBC tokenizer object.
  @returns a created tokenizer object.
}
function TZODBCDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZODBCTokenizer.Create;
end;

{ TZAbstractODBCConnection }

procedure TZAbstractODBCConnection.AfterConstruction;
var ODBCConnection: IZODBCConnection;
begin
  QueryInterface(IZODBCConnection, ODBCConnection);
  FWeakODBCConRefOfSelf := Pointer(ODBCConnection);
  ODBCConnection :=  nil;
  fODBCPlainDriver := PlainDriver.GetInstance as TZODBC3PlainDriver;
  fHENV := nil;
  fHDBC := nil;
  if Supports(fODBCPlainDriver, IODBC3UnicodePlainDriver) then
    FMetaData := TODBCDatabaseMetadataW.Create(Self, Url, fHDBC)
  else
    FMetaData := TODBCDatabaseMetadataA.Create(Self, Url, fHDBC);
  fCatalog := '';
  inherited AfterConstruction; //dec constructors RefCnt
  if fODBCPlainDriver.SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), fHENV) <> SQL_SUCCESS then
    raise EZSQLException.Create('Couldn''t allocate an Environment handle');
  //Try to SET Major Version 3 and minior Version 8
  if fODBCPlainDriver.SQLSetEnvAttr(fHENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3_80, 0) = SQL_SUCCESS then
    fODBCVersion := {%H-}Word(SQL_OV_ODBC3_80)
  else begin
    //set minimum Major Version 3
    if fODBCPlainDriver.SQLSetEnvAttr(fHENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0) <> SQL_SUCCESS then
      raise EZSQLException.Create('Failed to set minimum ODBC version 3');
    fODBCVersion := {%H-}Word(SQL_OV_ODBC3) * 100;
  end;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZAbstractODBCConnection.ClearWarnings;
begin
  FreeAndNil(fLastWarning);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZAbstractODBCConnection.InternalClose;
var RET: SQLRETURN;
begin
  if Closed or not Assigned(fODBCPlainDriver) then
    Exit;
  FSavePoints.Clear;
  try
    if not AutoCommit then begin
      AutoCommit := not FRestartTransaction;
      Ret := fODBCPlainDriver.SQLEndTran(SQL_HANDLE_DBC,fHDBC,SQL_ROLLBACK);
      if (Ret <> SQL_SUCCESS) then
        HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC,
          'ROLLBACK TRANSACTION', lcTransaction, Self);
    end;
  finally
    try
      if fHDBC <> nil then begin
        Ret := fODBCPlainDriver.SQLDisconnect(fHDBC);
        if (Ret <> SQL_SUCCESS) then
          HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC,
            'DISCONNECT DATABASE', lcDisconnect, Self);
      end;
    finally
      if Assigned(fHDBC) then begin
        fODBCPlainDriver.SQLFreeHandle(SQL_HANDLE_DBC, fHDBC);
        fHDBC := nil;
      end;
    end;
  end;
end;

procedure TZAbstractODBCConnection.Commit;
var Ret: SQLRETURN;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SInvalidOpInAutoCommit);
  if FSavePoints.Count > 0
  then ReleaseSavePoint(FSavePoints.Count-1)
  else begin
    Ret := fODBCPlainDriver.SQLEndTran(SQL_HANDLE_DBC,fHDBC,SQL_COMMIT);
    if (Ret <> SQL_SUCCESS) then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC,
        'COMMIT TRANSACTION', lcTransaction, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
    if not FRestartTransaction then
      SetAutoCommit(True);
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractODBCConnection.Destroy;
begin
  inherited Destroy;
  if Assigned(fHENV) then
    fODBCPlainDriver.SQLFreeHandle(SQL_HANDLE_ENV, fHENV);
  ClearWarnings;
end;

procedure TZAbstractODBCConnection.DetermineAttachmentCharset;
var
  CodePageName: String;
  IPos, BytesPerChar, CodePage: Integer;
const CPEncodings: array[Boolean] of TZCharEncoding = (ceAnsi, ceUTF8);
label fail;
begin
  if (Info.Values[ConnProps_Codepage] <> '') or (Info.Values[ConnProps_Charset] <> '') then begin
    //set a custom codepage to notify zeos about conversion routines note: cp must be equal for all fields else use the W driver
    //first place in a name
    //second use ':' for the codepage
    //third use '/' for the maximum amount of bytes / character equal to database defined charset
    //example: codepage=latin1:1252/1
    //example: characterset=utf8:65001/4
    CodePageName := Info.Values[ConnProps_Codepage];
    if CodePageName = '' then
      CodePageName := Info.Values[ConnProps_Charset];
    IPos := ZFastCode.Pos('/', CodePageName);
    if IPos > 0 then
      CodePageName[IPos] := #0 else
      goto fail;
    BytesPerChar := ZFastCode.{$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(PChar(@CodePageName[IPos+1]), 1);
    IPos := ZFastCode.Pos(':', CodePageName);
    if IPos > 0 then
      CodePageName[IPos] := #0 else
      goto fail;
    CodePage := ZFastCode.{$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(PChar(@CodePageName[IPos+1]), ZOSCodePage);
    CodePageName := Copy(CodePageName, 1, iPos-1);
    if Supports(fODBCPlainDriver, IODBC3UnicodePlainDriver) then
      fODBCPlainDriver.AddCodePage(CodePageName, 0, ceUTF16, CodePage, '', BytesPerChar)
    else
      fODBCPlainDriver.AddCodePage(CodePageName, 0, CPEncodings[CodePage = 65001], CodePage, '', BytesPerChar);
    CheckCharEncoding(CodePageName);
  end else
fail:
    if Supports(fODBCPlainDriver, IODBC3UnicodePlainDriver)
    then CheckCharEncoding('CP_UTF16')
    else CheckCharEncoding('CP_ACP');
end;

function TZAbstractODBCConnection.GetArrayRowSupported: Boolean;
begin
  Result := fArrayRowSupported;
end;

function TZAbstractODBCConnection.GetArraySelectSupported: Boolean;
begin
  Result := fArraySelectSupported;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAbstractODBCConnection.GetCatalog: string;
begin
  Result := fCatalog;
end;

function TZAbstractODBCConnection.GetPlainDriver: TZODBC3PlainDriver;
begin
  Result := fODBCPlainDriver;
end;

function TZAbstractODBCConnection.GetServerProvider: TZServerProvider;
begin
  Result := fServerProvider;
end;

function TZAbstractODBCConnection.GetStatementAnalyser: IZStatementAnalyser;
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

function TZAbstractODBCConnection.GetTokenizer: IZTokenizer;
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
    else Result := TZODBCTokenizer.Create;
  end;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZAbstractODBCConnection.GetWarnings: EZSQLWarning;
begin
  Result := fLastWarning;
end;

const
  ConnLost = '01000';
procedure TZAbstractODBCConnection.HandleErrorOrWarning(RETCODE: SQLRETURN;
  Handle: SQLHANDLE; HandleType: SQLSMALLINT; const LogMessage: SQLString;
  LoggingCategory: TZLoggingCategory; const Sender: IImmediatelyReleasable);
var ErrorCode: SQLINTEGER;
    SQLState: SQLString;
    Error: EZSQLThrowable;
    ExeptionClass: EZSQLThrowableClass;
    FormatStr: String;
begin
  if RETCODE = SQL_SUCCESS then
    Exit;
  FLogMessage := ComposeMessageString(RETCODE, Handle, HandleType, SQLState, ErrorCode);
  if DriverManager.HasLoggingListener then
    LogError(LoggingCategory, ErrorCode, Sender, LogMessage, FLogMessage);
  if RETCODE = SQL_SUCCESS_WITH_INFO
  then ExeptionClass := EZSQLWarning
  else if SQLState = ConnLost
    then ExeptionClass := EZSQLConnectionLost
    else ExeptionClass := EZSQLException;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '') then
    if LoggingCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;
  if AddLogMsgToExceptionOrWarningMsg and (LogMessage <> '')
  then FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode, LogMessage])
  else FLogMessage := Format(FormatStr, [FLogMessage, ErrorCode]);
  Error := ExeptionClass.CreateWithCodeAndStatus(ErrorCode, SQLState, FLogMessage);
  FLogMessage := '';
  if RETCODE = SQL_SUCCESS_WITH_INFO then begin//that's a Warning
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
  if Error <> nil then
     raise Error;
end;

function TZAbstractODBCConnection.ODBCVersion: Word;
begin
  Result := fODBCVersion;
end;

procedure AssignPropertiesToConnectionStrings(PropertyStrings, ConnectionStrings: TStrings);
const KnownOdbcProperties: array[0..3] of String = (
  ConnProps_DRIVER,
  ConnProps_Server,
  ConnProps_CharacterSet,
  ConnProps_TrustedConnection);

var ps, cs: String;
  I: Integer;
begin
  if (PropertyStrings = nil) or (ConnectionStrings = nil) then Exit;
  for i := low(KnownOdbcProperties) to high(KnownOdbcProperties) do begin
    ps := PropertyStrings.Values[KnownOdbcProperties[i]];
    if PS = '' then Continue;
    cs := ConnectionStrings.Values[KnownOdbcProperties[i]];
    if CS = '' then
      ConnectionStrings.Values[KnownOdbcProperties[i]] := PS;
  end;
end;


{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinal and pointers is not portable} {$ENDIF}
procedure TZAbstractODBCConnection.Open;
type
  TDriverNameAndServerProvider = record
    DriverName: String;
    Provider: TZServerProvider;
  end;
const
  KnownDriverName2TypeMap: array[0..22] of TDriverNameAndServerProvider = (
    (DriverName: 'SQLNCLI';     Provider: spMSSQL),
    (DriverName: 'SQLSRV';      Provider: spMSSQL),
    (DriverName: 'LIBTDSODBC';  Provider: spMSSQL),
    (DriverName: 'IVSS';        Provider: spMSSQL),
    (DriverName: 'IVMSSS';      Provider: spMSSQL),
    (DriverName: 'PBSS';        Provider: spMSSQL),
    (DriverName: 'DB2CLI';      Provider: spDB2),
    (DriverName: 'LIBDB2';      Provider: spDB2),
    (DriverName: 'IVDB2';       Provider: spDB2),
    (DriverName: 'PBDB2';       Provider: spDB2),
    (DriverName: 'MSDB2';       Provider: spDB2),
    (DriverName: 'CWBODBC';     Provider: spDB2),
    (DriverName: 'MYODBC';      Provider: spMySQL),
    (DriverName: 'SQORA';       Provider: spOracle),
    (DriverName: 'MSORCL';      Provider: spOracle),
    (DriverName: 'PBOR';        Provider: spOracle),
    (DriverName: 'IVOR';        Provider: spOracle),
    (DriverName: 'ODBCFB';      Provider: spIB_FB),
    (DriverName: 'IB';          Provider: spIB_FB),
    (DriverName: 'SQLITE';      Provider: spSQLite),
    (DriverName: 'PSQLODBC';    Provider: spPostgreSQL),
    (DriverName: 'NXODBCDRIVER';Provider: spNexusDB),
    (DriverName: 'ICLIT09B';    Provider: spInformix)
    );
var
  tmp, OutConnectString: String;
  TimeOut: NativeUInt;
  aLen: SQLSMALLINT;
  ConnectStrings: TStrings;
  DriverCompletion: SQLUSMALLINT;
  InfoValue: SQLUINTEGER;
  Ret: SQLRETURN;
begin
  if not Closed then
    Exit;
  DetermineAttachmentCharset; //do this by default!
  Ret := fODBCPlainDriver.SQLAllocHandle(SQL_HANDLE_DBC,fHENV,fHDBC);
  if Ret <> SQL_SUCCESS then
    raise EZSQLException.Create('Failed to create an environment handle');
  if Info.Values[ConnProps_Timeout] <> '' then begin
    TimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Info.Values[ConnProps_Timeout],0);
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_CONNECTION_TIMEOUT, SQLPOINTER(TimeOut), 0);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'SET CONNECTION TIMEOUT', lcConnect, Self);
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_LOGIN_TIMEOUT, SQLPOINTER(TimeOut), SQL_LOGIN_TIMEOUT_DEFAULT);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'SET LOGIN TIMEOUT', lcConnect, Self);
  end;

  DriverCompletion := SQL_DRIVER_NOPROMPT;
  tmp := Info.Values[ConnProps_DriverCompletion];
  if tmp <> '' then
    if tmp = 'SQL_DRIVER_PROMPT' then
      DriverCompletion := SQL_DRIVER_PROMPT
    else if tmp = 'SQL_DRIVER_COMPLETE' then
      DriverCompletion := SQL_DRIVER_COMPLETE
    else if tmp = 'SQL_DRIVER_COMPLETE_REQUIRED' then
      DriverCompletion := SQL_DRIVER_COMPLETE_REQUIRED;

  ConnectStrings := SplitString(DataBase, ';');
  AssignPropertiesToConnectionStrings(Info, ConnectStrings);
  if StrToBoolEx(ConnectStrings.Values[ConnProps_TrustedConnection]) then
    tmp := DataBase
  else
  begin
    ConnectStrings.Values[ConnProps_UID] := User;
    ConnectStrings.Values[ConnProps_PWD] := PassWord;
    tmp := ComposeString(ConnectStrings, ';');
  end;
  {$IFDEF WITH_VAR_INIT_WARNING}OutConnectString := '';{$ENDIF}
  SetLength(OutConnectString, 1024);
  FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);
  try
    Ret := fODBCPlainDriver.SQLDriverConnect(fHDBC,
      {$IFDEF MSWINDOWS}SQLHWND(GetDesktopWindow){$ELSE}nil{$ENDIF},
      Pointer(tmp), Length(tmp), Pointer(OutConnectString),
      Length(OutConnectString), @aLen, DriverCompletion);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, FLogMessage, lcConnect, Self)
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
    SetLength(OutConnectString, aLen);
    Ret := fODBCPlainDriver.SQLGetInfo(fHDBC, SQL_PARAM_ARRAY_SELECTS, @InfoValue, SizeOf(SQLUINTEGER), nil);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'GET ATTRIBUTE SQL_PARAM_ARRAY_SELECTS', lcOther, Self);
    fArraySelectSupported := InfoValue = SQL_PAS_BATCH;
    Ret := fODBCPlainDriver.SQLGetInfo(fHDBC, SQL_PARAM_ARRAY_ROW_COUNTS, @InfoValue, SizeOf(SQLUINTEGER), nil);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'GET ATTRIBUTE SQL_PARAM_ARRAY_ROW_COUNTS', lcOther, Self);
    fArrayRowSupported := InfoValue = SQL_PARC_BATCH;
  finally
    FreeAndNil(ConnectStrings)
  end;
  inherited Open;
  inherited SetTransactionIsolation(GetMetaData.GetDatabaseInfo.GetDefaultTransactionIsolation);
  inherited SetReadOnly(GetMetaData.GetDatabaseInfo.IsReadOnly);
  if not AutoCommit then begin
    AutoCommit := True;
    SetAutoCommit(False);
  end;
  fServerProvider := spUnknown;
  with GetMetaData.GetDatabaseInfo do begin
    fRetaining := SupportsOpenCursorsAcrossCommit and
                  SupportsOpenCursorsAcrossRollback;
    tmp := UpperCase(GetDriverName);
    for aLen := low(KnownDriverName2TypeMap) to high(KnownDriverName2TypeMap) do
      if StartsWith(tmp, KnownDriverName2TypeMap[aLen].DriverName) then begin
        fServerProvider := KnownDriverName2TypeMap[aLen].Provider;
        Break;
      end;
    DBProviderName2ServerProvider(GetDatabaseProductName, FServerProvider);
  end;
  if fServerProvider = spMSSQL then begin
    { find out which encoding the raw columns do have }
    with CreateStatement.ExecuteQuery(
      'SELECT DATABASEPROPERTYEX('+QuotedStr(GetCatalog)+', ''Collation'') as DatabaseCollation, '+
      '  COLLATIONPROPERTY(CAST(DATABASEPROPERTYEX('+QuotedStr(GetCatalog)+', ''Collation'') as NVARCHAR(255)), ''Codepage'') as Codepage') do begin
      if Next and not IsNull(FirstDbcIndex) then begin
        ConSettings.ClientCodePage := New(PZCodePage);
        if Supports(fODBCPlainDriver, IODBC3UnicodePlainDriver)
        then ConSettings.ClientCodePage.Encoding := ceUTF16//well a "mixed" encoding i have not prepared yet...
        else ConSettings.ClientCodePage.Encoding := ceAnsi;
        ConSettings.ClientCodePage.IsStringFieldCPConsistent := True;
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
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractODBCConnection.Rollback;
var Ret: SQLRETURN;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SInvalidOpInAutoCommit);
  if FSavePoints.Count > 0
  then RollbackTo(FSavePoints.Count-1)
  else begin
    Ret := fODBCPlainDriver.SQLEndTran(SQL_HANDLE_DBC,fHDBC,SQL_ROLLBACK);
    if (Ret <> SQL_SUCCESS) then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'ROLLBACK TRANSACTION', lcTransaction, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
    if not FRestartTransaction then
      SetAutoCommit(True);
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
const CommitMode: Array[Boolean] of Pointer = (SQL_AUTOCOMMIT_OFF, SQL_AUTOCOMMIT_ON);
procedure TZAbstractODBCConnection.SetAutoCommit(Value: Boolean);
var Ret: SQLRETURN;
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    FSavePoints.Clear;
    if not Closed then begin
      Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC,SQL_ATTR_AUTOCOMMIT,CommitMode[Value],0);
      if (Ret <> SQL_SUCCESS) then
        HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'SET AUTOCOMMIT', lcTransaction, Self);
    end;
    AutoCommit := Value;
  end;
end;

procedure TZAbstractODBCConnection.SetCatalog(const Catalog: string);
begin
  fCatalog := Catalog;
end;

procedure TZAbstractODBCConnection.SetLastWarning(Warning: EZSQLWarning);
begin
  ClearWarnings;
  fLastWarning := Warning;
end;

const AccessMode: array[Boolean] of Pointer = (SQL_MODE_READ_WRITE, SQL_MODE_READ_ONLY);
procedure TZAbstractODBCConnection.SetReadOnly(Value: Boolean);
var Ret: SQLRETURN;
begin
  if Value <> ReadOnly then begin
    if not Closed then begin
      Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC,SQL_ATTR_ACCESS_MODE,AccessMode[Value],0);
      if (Ret <> SQL_SUCCESS) then
        HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'SET READONLY', lcTransaction, Self);
    end;
    inherited SetReadOnly(Value);
  end;
end;

const ODBCTIL: array[TZTransactIsolationLevel] of Pointer =
  ( nil,
    Pointer(SQL_TRANSACTION_READ_UNCOMMITTED),
    Pointer(SQL_TRANSACTION_READ_COMMITTED),
    Pointer(SQL_TRANSACTION_REPEATABLE_READ),
    Pointer(SQL_TRANSACTION_SERIALIZABLE)
  );
procedure TZAbstractODBCConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var Ret: SQLRETURN;
begin
  if (TransactIsolationLevel <> Level) then begin
    if not Closed then begin
      Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC,SQL_ATTR_TXN_ISOLATION,ODBCTIL[Level],0);
      if (Ret <> SQL_SUCCESS) then
        HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC,
          'SET TRANSACTION ISOLATION LEVEL', lcTransaction, self);
    end;
    inherited SetTransactionIsolation(Level);
  end;
end;

function TZAbstractODBCConnection.StartTransaction: Integer;
var S: String;
    Ret: SQLRETURN;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC,SQL_ATTR_AUTOCOMMIT,SQL_AUTOCOMMIT_OFF,0);
    if (Ret <> SQL_SUCCESS) then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC,
        'START TRANSACTION', lcTransaction, Self);
    AutoCommit := False;
    Result := 1;
  end else begin
    Result := FSavePoints.Count+2;
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(Result);
    SavePoint(S);
  end;
end;

{ TZODBCConnectionW }

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
function TZODBCConnectionW.ComposeMessageString(RETCODE: SQLRETURN;
  Handle: SQLHANDLE; HandleType: SQLSMALLINT; Out SQLState: SQLString;
  Out ErrorCode: SQLINTEGER): SQLString;
var
  SqlstateBuf: TSQLSTATE_W;
  MessageBuffer: array[0..SQL_MAX_MESSAGE_LENGTH] of WideChar;
  RecNum, NativeError: SQLINTEGER;
  ErrorString: UnicodeString;
  TextLength: SQLSMALLINT;
  MsgWriter: TZUnicodeSQLStringWriter;
begin
  ErrorCode := RetCode;
  if (Handle=nil) or (RETCODE=SQL_INVALID_HANDLE) then begin
    SQLState := 'HY000';
    Result := 'Invalid handle';
  end else begin
    SQLState := '';
    Result := '';
    ErrorString := '';
    MsgWriter := TZUnicodeSQLStringWriter.Create(SQL_SQLSTATE_SIZE+SQL_MAX_MESSAGE_LENGTH+12);
    try
      RecNum := 1;
      while TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLGetDiagRecW(HandleType,Handle,RecNum, @SqlstateBuf[0],
          @NativeError,@MessageBuffer[0],SQL_MAX_MESSAGE_LENGTH,@TextLength) and (not 1)=0 do begin
        while (TextLength>0) and (MessageBuffer[TextLength-1]<=' ') do //trim trailing lineending and spaces
          dec(TextLength);
        if RecNum = 1 then begin
          {$IFDEF UNICODE}
          System.SetString(SQLState, PWideChar(@SqlstateBuf[0]), 5);
          {$ELSE}
          SQLState := UnicodeStringToAscii7(PWideChar(@SqlstateBuf[0]), 5);
          {$ENDIF}
          ErrorCode := NativeError;
        end;
        MsgWriter.AddText(@SqlstateBuf[0], 5, ErrorString);
        MsgWriter.AddChar('[', ErrorString);
        MsgWriter.AddOrd(NativeError, ErrorString);
        MsgWriter.AddChar(']', ErrorString);
        MsgWriter.AddChar(':', ErrorString);
        if TextLength = 0
        then MsgWriter.AddText(UnicodeString('Unidentified error'), ErrorString)
        else MsgWriter.AddText(@MessageBuffer[0], TextLength, ErrorString);
        MsgWriter.AddText(UnicodeString(LineEnding), ErrorString);
        inc(RecNum);
      end;
      MsgWriter.Finalize(ErrorString);
      if RecNum = 1 then begin //no error returned?
        SQLState := 'HY000';
        Result := SUnknownError;
      end else
      {$IFNDEF UNICODE}
      Result := ZUnicodeToRaw(ErrorString, {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
        {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF});
      {$ELSE}
      Result := ErrorString;
      {$ENDIF}
      ErrorString := '';
    finally
      FreeAndNil(MsgWriter);
    end;
  end;
end;

function TZODBCConnectionW.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  Result := TZODBCStatementW.Create(Self, fHDBC, Info);
end;

procedure TZODBCConnectionW.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var STMT: SQLHSTMT;
    Ret: SQLRETURN;
begin
  if SQL = '' then
    Exit;
  if Closed then
    Open;
  STMT := nil;
  Ret := fODBCPlainDriver.SQLAllocHandle(SQL_HANDLE_STMT, fHDBC, STMT);
  if (Ret <> SQL_SUCCESS) then
    HandleErrorOrWarning(Ret, Stmt, SQL_HANDLE_STMT, 'SQLAllocHandle', lcOther, Self);
  try
    Ret := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLExecDirectW(STMT,
      Pointer(SQL), Length(SQL));
    {$IFNDEF UNICODE}
    if (Ret <> SQL_SUCCESS) or DriverManager.HasLoggingListener then
      FLogMessage := ZUnicodeToRaw(SQL, zCP_UTF8);
    {$ENDIF}
    if (Ret <> SQL_NO_DATA) and (Ret <> SQL_SUCCESS) then
      HandleErrorOrWarning(Ret, Stmt, SQL_HANDLE_STMT, {$IFDEF UNICODE}SQL{$ELSE}FLogMessage{$ENDIF}, LoggingCategory, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LoggingCategory, URL.Protocol, {$IFDEF UNICODE}SQL{$ELSE}FLogMessage{$ENDIF});
  finally
    if STMT <> nil then
      fODBCPlainDriver.SQLFreeHandle(SQL_HANDLE_STMT, STMT);
  end;
end;

{**  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZODBCConnectionW.GetCatalog: string;
var
  {$IFNDEF UNICODE}
  Buf: UnicodeString;
  {$ENDIF}
  aLen: SQLINTEGER;
  Ret: SQLRETURN;
begin
  Result := inherited GetCatalog;
  if Result = '' then begin
    RET := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLGetConnectAttrW(fHDBC,
      SQL_ATTR_CURRENT_CATALOG, nil, 0, @aLen);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'GET CATALOG', lcOther, Self);
    if aLen > 0 then begin
      {$IFDEF UNICODE}
      SetLength(Result, aLen shr 1);
      Ret := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLGetConnectAttrW(fHDBC,
        SQL_ATTR_CURRENT_CATALOG, Pointer(Result), aLen+2, @aLen);
      {$ELSE}
      {$IFDEF WITH_VAR_INIT_WARNING}Buf := '';{$ENDIF}
      SetLength(Buf, aLen shr 1);
      Ret := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLGetConnectAttrW(fHDBC,
        SQL_ATTR_CURRENT_CATALOG, Pointer(Buf), aLen+2, @aLen);
      Result := PUnicodeToRaw(Pointer(Buf), aLen shr 1, GetW2A2WConversionCodePage(ConSettings));
      {$ENDIF}
      if Ret <> SQL_SUCCESS then
        HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'GET CATALOG', lcOther, Self);
      inherited SetCatalog(Result);
    end;
  end;
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
function TZODBCConnectionW.NativeSQL(const SQL: string): string;
var NewLength: SQLINTEGER;
  RET: SQLRETURN;
{$IFNDEF UNICODE}
  aSQL, nSQL: UnicodeString;
{$ENDIF}
begin
  if SQL <> '' then begin
    {$IFNDEF UNICODE}
    aSQL := PRawToUnicode(Pointer(SQL), Length(SQL), GetW2A2WConversionCodePage(ConSettings));
    {$IFDEF WITH_VAR_INIT_WARNING}nSQL := '';{$ENDIF}
    SetLength(nSQL, Length(aSQL) shl 1);
    Ret := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLNativeSqlW(fHDBC,
      Pointer(aSQL), Length(aSQL), Pointer(nSQL), Length(nSQL), @NewLength);
    Result := PUnicodeToRaw(Pointer(nSQL), NewLength, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    SetLength(Result, Length(SQL) shl 1);
    Ret := TODBC3UnicodePlainDriver(fODBCPlainDriver).SQLNativeSqlW(fHDBC,
      Pointer(SQL), Length(SQL), Pointer(Result), Length(Result), @NewLength);
    SetLength(Result, NewLength);
    {$ENDIF}
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'NATIVE SQL',
        lcOther, Self);
  end else Result := '';
end;

function TZODBCConnectionW.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  Result := TZODBCCallableStatementW.Create(Self, fHDBC, Name, Params);
end;

function TZODBCConnectionW.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if Closed then Open;
  Result := TZODBCPreparedStatementW.Create(Self, fHDBC, SQL, Info);
end;

procedure TZODBCConnectionW.ReleaseSavePoint(Index: Integer);
var S: UnicodeString;
begin
  S := cSavePointSyntaxW[fServerProvider][spqtCommit];
  if S <> '' then begin
    S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(Self.FSavePoints[Index]);
    ExecuteImmediat(S, lcTransaction);
  end;
  FSavePoints.Delete(Index);
end;

procedure TZODBCConnectionW.RollBackTo(Index: Integer);
var S: UnicodeString;
begin
  S := cSavePointSyntaxW[fServerProvider][spqtRollback];
  if S <> '' then begin
    S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(Self.FSavePoints[Index]);
    ExecuteImmediat(S, lcTransaction);
  end;
  FSavePoints.Delete(Index);
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
function TZODBCConnectionW.SavePoint(const AName: String): Integer;
var S: UnicodeString;
begin
  S := cSavePointSyntaxW[fServerProvider][spqtSavePoint];
  if S = '' then
    raise EZSQLException.Create(SUnsupportedOperation);
  S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(AName);
  ExecuteImmediat(S, lcTransaction);
  Result := FSavePoints.Add(AName)+2;
end;

procedure TZODBCConnectionW.SetCatalog(const Catalog: string);
var Ret: SQLReturn;
  {$IFNDEF UNICODE}aCatalog: UnicodeString;{$ENDIF}
begin
  if Catalog <> inherited GetCatalog then begin
    {$IFNDEF UNICODE}
    aCatalog := PRawToUnicode(Pointer(Catalog), Length(Catalog), ZOSCodePage);
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_CURRENT_CATALOG,
      Pointer(aCatalog), Length(aCatalog) shl 1);
    {$ELSE}
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_CURRENT_CATALOG,
      Pointer(Catalog), Length(Catalog) shl 1);
    {$ENDIF}
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(RET, fHDBC, SQL_HANDLE_DBC, 'SET CATALOG',
        lcOther, Self);
    inherited SetCatalog(Catalog);
  end;
end;

{ TZODBCConnectionA }

function TZODBCConnectionA.ComposeMessageString(RETCODE: SQLRETURN;
  Handle: SQLHANDLE; HandleType: SQLSMALLINT; out SQLState: SQLString;
  out ErrorCode: SQLINTEGER): SQLString;
var
  SqlstateBuf: TSQLSTATE;
  MessageBuffer: array[0..SQL_MAX_MESSAGE_LENGTH] of AnsiChar;
  RecNum, NativeError: SQLINTEGER;
  TextLength: SQLSMALLINT;
  ErrorString: RawByteString;
  {$IFNDEF UNICODE}excCP,{$ENDIF}msgCP: Word;
  MsgWriter: TZRawSQLStringWriter;
begin
  ErrorCode := RETCODE;
  if (Handle=nil) or (RETCODE=SQL_INVALID_HANDLE) then begin
    Result := 'Invalid handle';
    SQLState := 'HY000';
  end else begin
    MsgWriter := TZRawSQLStringWriter.Create(SQL_SQLSTATE_SIZE+SQL_MAX_MESSAGE_LENGTH+10);
    SQLState := '';
    Result := '';
    try
      RecNum := 1;
      ErrorString := '';
      while TODBC3RawPlainDriver(fODBCPlainDriver).SQLGetDiagRec(HandleType,Handle,RecNum, @SqlstateBuf[0],
          @NativeError,@MessageBuffer[0],SQL_MAX_MESSAGE_LENGTH,@TextLength) and (not 1)=0 do begin
        while (TextLength>0) and (PByte(PAnsiChar(@MessageBuffer[0])+TextLength-1)^ <= Ord(' ')) do //trim trailing lineending and spaces
          dec(TextLength);
        if RecNum = 1 then begin
          {$IFDEF UNICODE}
          SQLState := Ascii7ToUnicodeString(PAnsiChar(@SqlstateBuf[0]), 5);
          {$ELSE}
          ZSetString(PAnsiChar(@SqlstateBuf[0]), 5, SQLState);
          {$ENDIF}
          ErrorCode := NativeError;
        end;
        MsgWriter.AddText(@SqlstateBuf[0], 5, ErrorString);
        MsgWriter.AddChar(AnsiChar('['), ErrorString);
        MsgWriter.AddOrd(NativeError, ErrorString);
        MsgWriter.AddChar(AnsiChar(']'), ErrorString);
        MsgWriter.AddChar(AnsiChar(':'), ErrorString);
        if TextLength = 0
        then MsgWriter.AddText(RawByteString('Unidentified error'), ErrorString)
        else MsgWriter.AddText(@MessageBuffer[0], TextLength, ErrorString);
        MsgWriter.AddText(RawByteString(LineEnding), ErrorString);
        inc(RecNum);
      end;
      MsgWriter.Finalize(ErrorString);
      if RecNum = 1 then begin //no error returned?
        SQLState := 'HY000';
        Result := SUnknownError;
      end else begin
        if (ConSettings <> nil) and (ConSettings.ClientCodePage <> nil)
        then msgCP := ConSettings.ClientCodePage.CP
        else msgCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
        {$IFDEF UNICODE}
        Result := ZRawToUnicode(ErrorString, msgCP);
        {$ELSE}
        excCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
            {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
        if excCP <> msgCP
        then PRawToRawConvert(Pointer(ErrorString), Length(ErrorString), msgCP, excCP, Result)
        else Result := ErrorString;
        {$ENDIF}
      end;
    finally
      FreeAndNil(MsgWriter);
    end;
  end;
end;

function TZODBCConnectionA.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  Result := TZODBCStatementA.Create(Self, fHDBC, Info);
end;

procedure TZODBCConnectionA.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var STMT: SQLHSTMT;
  Ret: SQLRETURN;
begin
  if SQL = '' then
    Exit;
  if Closed then
    Open;
  STMT := nil;
  Ret := fODBCPlainDriver.SQLAllocHandle(SQL_HANDLE_STMT, fHDBC, STMT);
  if (Ret <> SQL_SUCCESS) then
    HandleErrorOrWarning(Ret, Stmt, SQL_HANDLE_STMT, 'SQLAllocHandle', lcOther, Self);
  try
    Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLExecDirect(STMT,
      Pointer(SQL), Length(SQL));
    if (Ret <> SQL_SUCCESS) or DriverManager.HasLoggingListener then
      {$IFDEF UNICODE}
      FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ELSE}
      FLogMessage := SQL;
      {$ENDIF}
    if (Ret <> SQL_NO_DATA) and (Ret <> SQL_SUCCESS) then
      HandleErrorOrWarning(Ret, Stmt, SQL_HANDLE_STMT, FLogMessage, LoggingCategory, Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LoggingCategory, URL.Protocol, FLogMessage);
  finally
    if STMT <> nil then
      fODBCPlainDriver.SQLFreeHandle(SQL_HANDLE_STMT, STMT);
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}

function TZODBCConnectionA.GetCatalog: string;
var
  {$IFDEF UNICODE}
  Buf: RawByteString;
  {$ENDIF}
  aLen: SQLINTEGER;
  Ret: SQLRETURN;
begin
  Result := inherited GetCatalog;
  if Result = '' then begin
    {Test Length}
    Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLGetConnectAttr(fHDBC,
      SQL_ATTR_CURRENT_CATALOG, nil, 0, @aLen);
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(RET, fHDBC, SQL_HANDLE_DBC, 'GET CATALOG',
        lcOther, Self);
    if aLen > 0 then begin //move data to buffer
      {$IFNDEF UNICODE}
      SetLength(Result, aLen shr 1);
      Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLGetConnectAttr(fHDBC,
        SQL_ATTR_CURRENT_CATALOG, Pointer(Result), aLen+1, @aLen);
      {$ELSE}
      SetLength(Buf, aLen shr 1);
      Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLGetConnectAttr(fHDBC,
        SQL_ATTR_CURRENT_CATALOG, Pointer(Buf), aLen+1, @aLen);
      Result := PRawToUnicode(Pointer(Buf), aLen, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      if Ret <> SQL_SUCCESS then
        HandleErrorOrWarning(RET, fHDBC, SQL_HANDLE_DBC, 'GET CATALOG',
          lcOther, Self);
      inherited SetCatalog(Result);
    end;
  end;
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
function TZODBCConnectionA.NativeSQL(const SQL: string): string;
var Ret: SQLRETURN;
    NewLength: SQLINTEGER;
{$IFDEF UNICODE}
  aSQL, nSQL: RawByteString;
{$ENDIF}
begin
  if SQL <> '' then begin
    {$IFDEF UNICODE}
    aSQL := PUnicodeToRaw(Pointer(SQL), Length(SQL), ZOSCodePage);
    SetLength(nSQL, Length(aSQL) shl 1); //
    Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLNativeSql(fHDBC,
      Pointer(aSQL), Length(aSQL), Pointer(nSQL), Length(nSQL), @NewLength);
    Result := PRawToUnicode(Pointer(nSQL), NewLength, ZOSCodePage);
    {$ELSE}
    {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
    SetLength(Result, Length(SQL) shl 1); //
    Ret := TODBC3RawPlainDriver(fODBCPlainDriver).SQLNativeSql(fHDBC,
      Pointer(SQL), Length(SQL), Pointer(Result), Length(Result), @NewLength);
    SetLength(Result, NewLength);
    {$ENDIF}
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(Ret, fHDBC, SQL_HANDLE_DBC, 'NATIVE SQL',
        lcOther, Self);
  end else Result := '';
end;

function TZODBCConnectionA.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  Result := TZODBCCallableStatementA.Create(Self, fHDBC, Name, Params);
end;

function TZODBCConnectionA.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if Closed then Open;
  Result := TZODBCPreparedStatementA.Create(Self, fHDBC, SQL, Info);
end;

procedure TZODBCConnectionA.ReleaseSavePoint(Index: Integer);
var S: RawByteString;
begin
  S := cSavePointSyntaxA[fServerProvider][spqtCommit];
  if S <> '' then begin
    S := S+{$IFDEF UNICODE}UnicodeStringtoAscii7{$ENDIF}(FSavePoints[Index]);
    ExecuteImmediat(S, lcTransaction);
  end;
  FSavePoints.Delete(Index);
end;

procedure TZODBCConnectionA.RollBackTo(Index: Integer);
var S: RawByteString;
begin
  S := cSavePointSyntaxA[fServerProvider][spqtRollback];
  if S <> '' then begin
    S := S+{$IFDEF UNICODE}UnicodeStringtoAscii7{$ENDIF}(FSavePoints[Index]);
    ExecuteImmediat(S, lcTransaction);
  end;
  FSavePoints.Delete(Index);
end;

function TZODBCConnectionA.SavePoint(const AName: String): Integer;
var S: RawByteString;
begin
  S := cSavePointSyntaxA[fServerProvider][spqtSavePoint];
  if S = '' then
    raise EZSQLException.Create(SUnsupportedOperation);
  S := S+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(AName);
  ExecuteImmediat(S, lcTransaction);
  Result := FSavePoints.Add(AName)+2;
end;

procedure TZODBCConnectionA.SetCatalog(const Catalog: string);
var Ret: SQLRETURN;
  {$IFDEF UNICODE}aCatalog: RawByteString; {$ENDIF}
begin
  if Catalog <> inherited GetCatalog then begin
    {$IFDEF UNICODE}
    aCatalog := PUnicodeToRaw(Pointer(Catalog), Length(Catalog), ZOSCodePage);
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_CURRENT_CATALOG,
      Pointer(aCatalog), Length(aCatalog));
    {$ELSE}
    Ret := fODBCPlainDriver.SQLSetConnectAttr(fHDBC, SQL_ATTR_CURRENT_CATALOG,
      Pointer(Catalog), Length(Catalog));
    {$ENDIF}
    if Ret <> SQL_SUCCESS then
      HandleErrorOrWarning(RET, fHDBC, SQL_HANDLE_DBC, 'SET CATALOG',
        lcOther, Self);
    inherited SetCatalog(Catalog);
  end;
end;

var
  ODBCDriver: IZDriver;

initialization
  ODBCDriver := TZODBCDriver.Create;
  DriverManager.RegisterDriver(ODBCDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(ODBCDriver);
  ODBCDriver := nil;

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
end.
