{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASA;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}
uses
  ZCompatibility, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  SysUtils,
  ZDbcIntfs, ZDbcConnection, ZPlainASADriver, ZTokenizer, ZDbcGenericResolver,
  ZGenericSqlAnalyser, ZDbcLogging;

type
  {** Implements a ASA Database Driver. }
  TZASADriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a ASA specific connection interface. }
  IZASAConnection = interface (IZConnection)
    ['{FAAAFCE0-F550-4098-96C6-580145813EBF}']
    function GetDBHandle: PZASASQLCA;
    function GetPlainDriver: TZASAPlainDriver;
    function GetByteBufferAddress: PByteBuffer;
    procedure HandleErrorOrWarning(LoggingCategory: TZLoggingCategory;
      const Msg: SQLString; const Sender: IImmediatelyReleasable);
//    procedure CreateNewDatabase(const SQL: String);
  end;

  {** Implements ASA Database Connection. }

  { TZASAConnection }

  TZASAConnection = class(TZAbstractSingleTxnConnection, IZConnection,
    IZASAConnection, IZTransaction)
  private
    FSQLCA: TZASASQLCA;
    FHandle: PZASASQLCA;
    FPlainDriver: TZASAPlainDriver;
    FHostVersion: Integer;
    FLastWarning: EZSQLWarning;
    FClientLanguageCP: Word;
  private
    function DetermineASACharSet: String;
    procedure DetermineHostVersion;
    procedure DetermineClientLanguageCP;
    procedure SetOption(Temporary: Integer; const LogMsg: String;
      const Option, Value: RawByteString; LoggingCategory: TZLoggingCategory);
  protected
    procedure InternalClose; override;
    /// <summary>Immediately execute a query and do nothing with the results.</summary>
    /// <remarks>A new driver needs to implement one of the overloads.</remarks>
    /// <param>"SQL" a raw encoded query to be executed.</param>
    /// <param>"LoggingCategory" the LoggingCategory for the Logging listeners.</param>
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
  public
    function GetDBHandle: PZASASQLCA;
    function GetPlainDriver: TZASAPlainDriver;
    procedure HandleErrorOrWarning(LoggingCategory: TZLoggingCategory;
      const Msg: SQLString; const Sender: IImmediatelyReleasable);
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

    procedure Open; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    /// <summary>Returns the ServicerProvider for this connection.</summary>
    /// <returns>the ServerProvider</returns>
    function GetServerProvider: TZServerProvider; override;
    function GetHostVersion: Integer; override;
  end;

  {** Implements a specialized cached resolver for ASA. }
  TZASACachedResolver = class(TZGenerateSQLCachedResolver)
  end;

var
  {** The common driver manager object. }
  ASADriver: IZDriver;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses
  ZFastCode, ZDbcASAMetadata, ZDbcASAStatement, ZSybaseToken,
  ZSybaseAnalyser, ZSysUtils, ZDbcProperties, ZEncoding, ZMessages
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZASADriver }

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
function TZASADriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZASAConnection.Create(Url);
end;

{**
  Constructs this object with default properties.
}
constructor TZASADriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZASAPlainDriver.Create));
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZASADriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZASADriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZASADriver.GetTokenizer: IZTokenizer;
begin
  Result := TZSybaseTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZASADriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZSybaseStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

const
  ASATIL: array[TZTransactIsolationLevel] of RawByteString = ('1','0','1','2','3');
  SQLDA_sqldaid: PAnsiChar = 'SQLDA   ';
var
  PInt64_SQLDA_sqldaid: PInt64 absolute SQLDA_sqldaid;

{ TZASAConnection }

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZASAConnection.InternalClose;
begin
  if Closed or (not Assigned(PlainDriver))then
    Exit;
  FSavePoints.Clear;
  try
    if AutoCommit
    then FPlainDriver.dbpp_commit(FHandle, 0)
    else begin
      AutoCommit := not FRestartTransaction;
      FPlainDriver.dbpp_rollback(FHandle, 0);
    end;
    if FHandle.sqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, 'Close transaction', IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    FPlainDriver.db_string_disconnect(FHandle, nil);
    if FHandle.sqlCode <> SQLE_NOERROR then
       HandleErrorOrWarning(lcDisconnect, 'Disconnect from database', IImmediatelyReleasable(FWeakImmediatRelPtr));
    FHandle := nil;
    if FPlainDriver.db_fini(@FSQLCA) = 0 then begin
      DriverManager.LogError(lcConnect, URL.Protocol, 'Finalizing SQLCA',
        0, 'Error closing SQLCA');
      raise EZSQLException.CreateWithCode(0, 'Error closing SQLCA');
    end;
    if DriverManager.HasLoggingListener then
       DriverManager.LogMessage(lcDisconnect, URL.Protocol,
        'DISCONNECT FROM "'+URL.Database+'"');
  end;
end;

procedure TZASAConnection.Commit;
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
    FPlainDriver.dbpp_commit(FHandle, 0);
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, sCommitMsg, IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
    //SetOption(1, nil, 'CHAINED', 'ON');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
  end;
end;

function TZASAConnection.CreateStatementWithParams(Info: TStrings): IZStatement;
begin
  if IsClosed then Open;
  Result := TZASAStatement.Create(Self, Info);
end;

{**
   Get database connection handle.
   @return database handle
}
function TZASAConnection.GetDBHandle: PZASASQLCA;
begin
  Result := FHandle;
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZASAConnection.GetHostVersion: Integer;
begin
  Result := FHostVersion;
end;

function TZASAConnection.GetPlainDriver: TZASAPlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Checks for possible sql errors or warings.
  @param LogCategory a logging category.
  @param Msg a logging message.
  @param Sender the calling object to handle connection loss
}
procedure TZASAConnection.HandleErrorOrWarning(
  LoggingCategory: TZLoggingCategory; const Msg: SQLString;
  const Sender: IImmediatelyReleasable);
var err_len: Integer;
  SQLState, FormatStr: String;
  ErrCode: an_sql_code;
  Error: EZSQLThrowable;
  ExeptionClass: EZSQLThrowableClass;
  P: PAnsiChar;
  {$IFNDEF UNICODE}excCP: Word;{$ENDIF}
begin
  ErrCode := FHandle.SqlCode;
  if (ErrCode = SQLE_NOERROR) or //Nothing todo
     (ErrCode = SQLE_NOTFOUND) then //no line found
    Exit;
  P := @FByteBuffer[0];
  PByte(P)^ := 0;
{$IFNDEF UNICODE}
  excCP := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}
      {$IFDEF LCL}zCP_UTF8{$ELSE}zOSCodePage{$ENDIF}{$ENDIF};
{$ENDIF}
  P := FPlainDriver.sqlError_Message(FHandle, P, SizeOf(TByteBuffer)-1);
  err_len := ZFastCode.StrLen(P);
  {$IFDEF UNICODE}
  SQLState := USASCII7ToUnicodeString(@FHandle.sqlState[0], 5);
  FLogMessage := PRawToUnicode(P, err_Len, FClientLanguageCP);
  {$ELSE}
  SQLState := '';
  ZSetString(PAnsiChar(@FHandle.sqlState[0]), 5, SQLState);
  FLogMessage := '';
  if excCP <> FClientLanguageCP
  then PRawToRawConvert(P, err_len, FClientLanguageCP, excCP, FLogMessage)
  else System.SetString(FLogMessage, P, err_Len);
  {$ENDIF}
  if DriverManager.HasLoggingListener then
    LogError(LoggingCategory, ErrCode, Sender, Msg, FLogMessage);
  if ErrCode > 0 //that's a Warning
  then ExeptionClass := EZSQLWarning
  else if (ErrCode = SQLE_CONNECTION_NOT_FOUND) or
    (ErrCode = SQLE_CONNECTION_TERMINATED) or (ErrCode = SQLE_COMMUNICATIONS_ERROR)
    then ExeptionClass := EZSQLConnectionLost
    else ExeptionClass := EZSQLException;
  if AddLogMsgToExceptionOrWarningMsg and (Msg <> '') then
    if LoggingCategory in [lcExecute, lcPrepStmt, lcExecPrepStmt]
    then FormatStr := SSQLError3
    else FormatStr := SSQLError4
  else FormatStr := SSQLError2;
  if AddLogMsgToExceptionOrWarningMsg and (Msg <> '')
  then FLogMessage := Format(FormatStr, [FLogMessage, ErrCode, Msg])
  else FLogMessage := Format(FormatStr, [FLogMessage, ErrCode]);
  Error := ExeptionClass.CreateWithCodeAndStatus(ErrCode, SQLState, FLogMessage);
  FLogMessage := '';
  if ErrCode > 0 then begin//that's a Warning
    ClearWarnings;
    if not RaiseWarnings then begin
      FLastWarning := EZSQLWarning(Error);
      Error := nil;
    end;
  end else if (ErrCode = SQLE_CONNECTION_NOT_FOUND) or (ErrCode = SQLE_CONNECTION_TERMINATED) or
     (ErrCode = SQLE_COMMUNICATIONS_ERROR) then begin
    if (Sender <> nil)
    then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Error))
    else ReleaseImmediat(Self, EZSQLConnectionLost(Error));
  end;
  if Error <> nil then
     raise Error;
end;

function TZASAConnection.GetServerProvider: TZServerProvider;
begin
  Result := spASA;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZASAConnection.Open;
var
  ConnectionString, Links: string;
  {$IFDEF UNICODE}
  RawTemp: RawByteString;
  {$ENDIF UNICODE}
begin
  if not Closed then
     Exit;

  FClientLanguageCP := ZOSCodePage; //init
  FHandle := nil;
  ConnectionString := '';
  try
    if FPlainDriver.db_init(@FSQLCA) = 0 then
    begin
      DriverManager.LogError(lcConnect, URL.Protocol, 'Inititalizing SQLCA',
        0, 'Error initializing SQLCA');
      raise EZSQLException.Create('Error initializing SQLCA');
    end;
    FHandle := @FSQLCA;

    if HostName <> '' then
      ConnectionString := ConnectionString + 'ENG="' + HostName + '"; ';
    if User <> '' then
      ConnectionString := ConnectionString + 'UID="' + User + '"; ';
    if Password <> '' then
      ConnectionString := ConnectionString + 'PWD="' + Password + '"; ';
    if Database <> '' then
    begin
      if CompareText(ExtractFileExt(Database), '.db') = 0 then
        ConnectionString := ConnectionString + 'DBF="' + Database + '"; '
      else
        ConnectionString := ConnectionString + 'DBN="' + Database + '"; ';
    end;

    Links := '';
    if Info.Values[ConnProps_CommLinks] <> ''
      then Links := 'CommLinks=' + Info.Values[ConnProps_CommLinks];
    if Info.Values[ConnProps_Links] <> ''
      then Links := 'LINKS=' + Info.Values[ConnProps_Links];
    if (Links = '') and (Port <> 0)
      then Links := 'LINKS=tcpip(PORT=' + ZFastCode.IntToStr(Port) + ')';
    if Links <> ''
      then ConnectionString := ConnectionString + Links + '; ';

    {$IFDEF UNICODE}
    RawTemp := ZUnicodeToRaw(ConnectionString, ZOSCodePage);
    if FPlainDriver.db_string_connect(FHandle, Pointer(RawTemp)) <> 0 then
    {$ELSE}
    if FPlainDriver.db_string_connect(FHandle, Pointer(ConnectionString)) <> 0 then
    {$ENDIF}
    FLogMessage := Format(SConnect2AsUser,  [URL.Database, URL.UserName]);
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);

    if (FClientCodePage <> '' ) then begin
      FLogMessage := 'Set client characterset to: '+FClientCodePage;
      {$IFDEF UNICODE}
      RawTemp := ZUnicodeToRaw(FClientCodePage, ZOSCodePage);
      Move(Pointer(RawTemp)^, FByteBuffer[0], Length(RawTemp)+1);
      if (FPlainDriver.db_change_char_charset(FHandle, @FByteBuffer[0]) = 0 ) or
         (FPlainDriver.db_change_nchar_charset(FHandle, @FByteBuffer[0]) = 0 ) then
      {$ELSE}
      if (FPlainDriver.db_change_char_charset(FHandle, Pointer(FClientCodePage)) = 0 ) or
         (FPlainDriver.db_change_nchar_charset(FHandle, Pointer(FClientCodePage)) = 0 ) then
      {$ENDIF}
        HandleErrorOrWarning(lcConnect, FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcOther, URL.Protocol, FLogMessage);
    end;
    inherited Open;
  finally
    if Closed and (FHandle <> nil) then begin
      FPlainDriver.db_fini(FHandle);
      FHandle := nil;
    end;
  end;

  if FClientCodePage = ''  then
    CheckCharEncoding(DetermineASACharSet);
  DetermineClientLanguageCP;
  DetermineHostVersion;
  if FHostVersion >= 17000000 then //chained is deprecated On is comparable with AutoCommit=off
    SetOption(1, 'SET OPTION chained = "on"', 'chained', 'On', lcTransaction);
  { Sets an auto commit mode. }
  AutoCommit := not AutoCommit;
  SetAutoCommit(not AutoCommit);
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZASAConnection.GetWarnings: EZSQLWarning;
begin
  Result := FLastWarning;
end;

procedure TZASAConnection.AfterConstruction;
begin
  FPlainDriver := PlainDriver.GetInstance as TZASAPlainDriver;
  FMetadata := TZASADatabaseMetadata.Create(Self, URL);
  inherited AfterConstruction;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZASAConnection.ClearWarnings;
begin
  FreeAndNil(FLastWarning);
end;

function TZASAConnection.PrepareCallWithParams(const Name: String;
  Params: TStrings): IZCallableStatement;
begin
  if IsClosed then Open;
  Result := TZASACallableStatement.Create(Self, Name, Params);
end;

function TZASAConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZASAPreparedStatement.Create(Self, SQL, Info);
end;

procedure TZASAConnection.Rollback;
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
    FPlainDriver.dbpp_rollback(FHandle, 0);
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, sRollbackMsg, IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
   // SetOption(1, nil, 'CHAINED', 'ON');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
  end;
end;

procedure TZASAConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      if FHostVersion >= 17000000
      then SetOption(1, 'SET OPTION <USER>.AUTO_COMMIT = On', 'AUTO_COMMIT', 'On', lcTransaction)
      else SetOption(1, 'SET OPTION <USER>.chained = Off', 'chained', 'Off', lcTransaction);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

procedure TZASAConnection.SetOption(Temporary: Integer; const LogMsg: String;
  const Option, Value: RawByteString; LoggingCategory: TZLoggingCategory);
var
  SQLDA: PASASQLDA;
  Sz: Integer;
begin
  if Assigned(FHandle) then
  begin
    Sz := SizeOf(TASASQLDA) - (32767 * SizeOf(TZASASQLVAR));
    SQLDA := AllocMem(Sz);
    try
      PInt64(@SQLDA.sqldaid[0])^ := PInt64_SQLDA_sqldaid^;
      SQLDA.sqldabc := Sz;
      SQLDA.sqln := 1;
      SQLDA.sqld := 1;
      SQLDA.sqlVar[0].sqlType := DT_STRING;
      SQLDA.sqlVar[0].sqlLen := Length(Value){$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF};
      SQLDA.sqlVar[0].sqlData := Pointer(Value);
      FPlainDriver.dbpp_setoption(FHandle, Temporary, nil, Pointer(Option), SQLDA);
      if FHandle.SqlCode <> SQLE_NOERROR then
        HandleErrorOrWarning(LoggingCategory, LogMsg, IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(LoggingCategory, URL.Protocol, LogMsg);
    finally
      FreeMem(SQLDA);
    end;
  end;
end;

procedure TZASAConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if (Level = tiNone) then
    Level := tiReadUnCommitted;
  if Level <>  TransactIsolationLevel then begin
    if not IsClosed then
      SetOption(1, 'SET OPTION <USER>.ISOLATION_LEVEL = '+ZFastCode.IntToStr(Ord(Level)),
        'ISOLATION_LEVEL', ASATIL[Level], lcTransaction);
    TransactIsolationLevel := Level;
  end;
end;

function TZASAConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    if FHostVersion >= 17000000
    then SetOption(1, 'SET OPTION <USER>.AUTO_COMMIT = Off', 'AUTO_COMMIT', 'Off', lcTransaction)
    else SetOption(1, 'SET OPTION <USER>.chained = On', 'chained', 'On', lcTransaction);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := '"SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count)+'"';
    ExecuteImmediat(cSavePoint+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S) +2;
  end;
end;

function TZASAConnection.DetermineASACharSet: String;
var
  Stmt: IZStatement;
  RS: IZResultSet;
begin
  Stmt := CreateStatementWithParams(Info);
  RS := Stmt.ExecuteQuery('SELECT DB_PROPERTY(''CharSet'')');
  if RS.Next then
    Result := RS.GetString(FirstDbcIndex)
  else
    Result := '';
  RS := nil;
  Stmt.Close;
  Stmt := nil;
end;

procedure TZASAConnection.DetermineClientLanguageCP;
var
  Stmt: IZStatement;
  RS: IZResultSet;
  S: String;
begin
  Stmt := CreateStatementWithParams(Info);
  RS := Stmt.ExecuteQuery('SELECT CONNECTION_PROPERTY(''Language'')');
  if RS.Next
  then S := RS.GetString(FirstDbcIndex)
  else S := '';
  RS := nil;
  Stmt.Close;
  Stmt := nil;
  if S = 'arabic' then
    FClientLanguageCP := zCP_WIN1256
  else if S = 'czech' then
    FClientLanguageCP := zCP_WIN1250
  else if S = 'danish' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'dutch' then
    FClientLanguageCP := zCP_WIN1252
  else if (S = 'us_english') or (S = 'english') then
    FClientLanguageCP := zCP_us_ascii
  else if S = 'finnish' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'french' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'german' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'greek' then
    FClientLanguageCP := zCP_WIN1253
  else if S = 'hebrew' then
    FClientLanguageCP := zCP_WIN1255
  else if S = 'hungarian' then
    FClientLanguageCP := zCP_WIN1250
  else if S = 'italian' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'japanese' then
    FClientLanguageCP := zCP_SHIFTJS
  else if S = 'korean' then
    FClientLanguageCP := zCP_EUCKR
  else if S = 'lithuanian' then
    FClientLanguageCP := zCP_WIN1257
  else if (S = 'norwegian') or (s = 'norweg') then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'polish' then
    FClientLanguageCP := zCP_WIN1251
  else if (S = 'portuguese') or (S = 'portugue') then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'russian' then
    FClientLanguageCP := zCP_WIN1251
  else if (S = 'chinese') or (S = 'simpchin') then
    FClientLanguageCP := zCP_GB2312
  else if S = 'spanish' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'swedish' then
    FClientLanguageCP := zCP_WIN1252
  else if S = 'thai' then
    FClientLanguageCP := zCP_WIN874
  else if (S = 'tchinese') or (S = 'tradchin') then
    FClientLanguageCP := zCP_Big5
  else if S = 'turkish' then
    FClientLanguageCP := zCP_WIN1254
  else if S = 'ukrainian' then
    FClientLanguageCP := zCP_WIN1251
  else FClientLanguageCP := zOSCodePage;
end;

procedure TZASAConnection.DetermineHostVersion;
var
  Stmt: IZStatement;
  RS: IZResultSet;
  P: PAnsiChar;
  L: NativeUint;
  Code, Major, Minior, Sub: Integer;
begin
  Stmt := CreateStatementWithParams(Info);
  RS := Stmt.ExecuteQuery('SELECT PROPERTY(''ProductVersion'')');
  if RS.Next
  then P := RS.GetPAnsiChar(FirstDbcIndex, L)
  else P := nil;
  if P <> nil then begin
    Major := ValRawInt(P, Code);
    Inc(P, Code);
    Minior := ValRawInt(P, Code);
    Inc(P, Code);
    Sub := ValRawInt(P, Code);
    FHostVersion := ZSysUtils.EncodeSQLVersioning(Major, Minior, Sub);
  end;
  RS := nil;
  Stmt.Close;
  Stmt := nil;
end;

procedure TZASAConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var LogSQL: String;
begin
  FPlainDriver.dbpp_execute_imm(FHandle, Pointer(SQL), 0);
  if (FHandle.SqlCode <> SQLE_NOERROR) or DriverManager.HasLoggingListener then begin
    {$IFDEF UNICODE}
    LogSQL := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
    {$ELSE}
    LogSQL := SQL;
    {$ENDIF}
  end else LogSQL := '';
  if FHandle.SqlCode <> SQLE_NOERROR then
    HandleErrorOrWarning(LoggingCategory, LogSQL, IImmediatelyReleasable(FWeakImmediatRelPtr))
  else if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, URL.Protocol, LogSQL);
end;

initialization
  ASADriver := TZASADriver.Create;
  DriverManager.RegisterDriver(ASADriver);

finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(ASADriver);
  ASADriver := nil;
{$ENDIF ZEOS_DISABLE_ASA}
end.
