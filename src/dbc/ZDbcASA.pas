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
{   http://zeos.firmos.at  (FORUM)                        }
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
      const Msg: RawByteString; const Sender: IImmediatelyReleasable);
//    procedure CreateNewDatabase(const SQL: String);
  end;

  {** Implements ASA Database Connection. }

  { TZASAConnection }

  TZASAConnection = class(TZAbstractDbcSingleTransactionConnection, IZConnection,
    IZASAConnection, IZTransaction)
  private
    FSQLCA: TZASASQLCA;
    FHandle: PZASASQLCA;
    FPlainDriver: TZASAPlainDriver;
    FHostVersion: Integer;
    FLastWarning: EZSQLWarning;
  private
    function DetermineASACharSet: String;
    procedure DetermineHostVersion;
    procedure SetOption(Temporary: Integer; User: PAnsiChar;
      const Option, Value: RawByteString; LoggingCategory: TZLoggingCategory);
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
  public
    function GetDBHandle: PZASASQLCA;
    function GetPlainDriver: TZASAPlainDriver;
    procedure HandleErrorOrWarning(LoggingCategory: TZLoggingCategory;
      const Msg: RawByteString; const Sender: IImmediatelyReleasable);
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;

    procedure Commit;
    procedure Rollback;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function StartTransaction: Integer;

    procedure Open; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;

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

  try
    if AutoCommit
    then FPlainDriver.dbpp_commit(FHandle, 0)
    else FPlainDriver.dbpp_rollback(FHandle, 0);
    if FHandle.sqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, 'Close transaction', IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    FPlainDriver.db_string_disconnect(FHandle, nil);
    if FHandle.sqlCode <> SQLE_NOERROR then
       HandleErrorOrWarning(lcDisconnect, 'Disconnect from database', IImmediatelyReleasable(FWeakImmediatRelPtr));
    FHandle := nil;
    if FPlainDriver.db_fini(@FSQLCA) = 0 then begin
      DriverManager.LogError(lcConnect, ConSettings^.Protocol, 'Finalizing SQLCA',
        0, 'Error closing SQLCA');
      raise EZSQLException.CreateWithCode(0, 'Error closing SQLCA');
    end;
    if DriverManager.HasLoggingListener then
       DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol,
        'DISCONNECT FROM "'+ConSettings^.Database+'"');
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZASAConnection.Commit;
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
    FPlainDriver.dbpp_commit(FHandle, 0);
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, 'TRANSACTION COMMIT', IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol,
        'TRANSACTION COMMIT');
    //SetOption(1, nil, 'CHAINED', 'ON');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
  end;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZASAConnection.InternalCreate;
begin
  FPlainDriver := TZASAPlainDriver(GetIZPlainDriver.GetInstance);
  Self.FMetadata := TZASADatabaseMetadata.Create(Self, URL);
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
  LoggingCategory: TZLoggingCategory; const Msg: RawByteString;
  const Sender: IImmediatelyReleasable);
var err_len: Integer;
  ErrBuf: RawByteString;
  State, ErrMsg: String;
  ErrCode: an_sql_code;
  Exception: EZSQLException;
  P: PAnsiChar;
begin
  ErrCode := FHandle.SqlCode;
  if (ErrCode = SQLE_NOERROR) or //Nothing todo
     (ErrCode = SQLE_NOTFOUND) then //no line found
  Exit;
  P := @FByteBuffer[0];
  PByte(P)^ := 0;
  P := FPlainDriver.sqlError_Message(FHandle, P, SizeOf(TByteBuffer)-1);
  err_len := ZFastCode.StrLen(P);
  ErrBuf := '';
  ZSetString(P, err_len, errBuf);
  State := '';
  {$IFDEF UNICODE}
  State := USASCII7ToUnicodeString(@FHandle.sqlState[0], 5);
  ErrMsg := PRawToUnicode(P, err_Len, ConSettings.ClientCodePage.CP);
  {$ELSE}
  {$IFDEF WITH_VAR_INIT_WARNING} State := ''; {$ENDIF}
  ZSetString(PAnsiChar(@FHandle.sqlState[0]), 5, State);
  {$IFDEF WITH_VAR_INIT_WARNING} ErrMsg := ''; {$ENDIF}
  System.SetString(ErrMsg, P, err_Len);
  {$ENDIF}
  ErrMsg := ErrMsg + ' The SQL: ';
  {$IFDEF UNICODE}
  ErrMsg := ErrMsg + ZRawToUnicode(Msg, ConSettings.ClientCodePage.CP);
  {$ELSE}
  ErrMsg := ErrMsg + Msg;
  {$ENDIF}
  if ErrCode > 0 then begin//that's a Warning
    ClearWarnings;
    FLastWarning := EZSQLWarning.CreateWithCodeAndStatus(ErrCode, State, ErrMsg);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, ErrBuf);
  end else begin  //that's an error
    if DriverManager.HasLoggingListener then
      DriverManager.LogError(LoggingCategory, ConSettings^.Protocol, Msg, ErrCode, ErrBuf);
    if (ErrCode = SQLE_CONNECTION_NOT_FOUND) or (ErrCode = SQLE_CONNECTION_TERMINATED) or
       (ErrCode = SQLE_COMMUNICATIONS_ERROR) then begin
      Exception := EZSQLConnectionLost.CreateWithCodeAndStatus(ErrCode, State, ErrMsg);
      if (Sender <> nil)
      then Sender.ReleaseImmediat(Sender, EZSQLConnectionLost(Exception))
      else ReleaseImmediat(Self, EZSQLConnectionLost(Exception));
    end else
      Exception := EZSQLException.CreateWithCodeAndStatus(ErrCode, State, ErrMsg);
    if Exception <> nil then
       raise Exception;
  end;
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
  RawTemp: RawByteString;
begin
  if not Closed then
     Exit;

  FHandle := nil;
  ConnectionString := '';
  try
    if FPlainDriver.db_init(@FSQLCA) = 0 then
    begin
      DriverManager.LogError(lcConnect, ConSettings^.Protocol, 'Inititalizing SQLCA',
        0, 'Error initializing SQLCA');
      raise EZSQLException.CreateWithCode(0,
        'Error initializing SQLCA');
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
    RawTemp := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcConnect, RawTemp, IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, RawTemp);

    if (FClientCodePage <> '' ) then
      {$IFDEF UNICODE}
      RawTemp := ZUnicodeToRaw(FClientCodePage, ZOSCodePage);
      Move(Pointer(RawTemp)^, FByteBuffer[0], Length(RawTemp)+1);
      RawTemp := 'Set client characterset to: '+RawTemp;
      if (FPlainDriver.db_change_char_charset(FHandle, @FByteBuffer[0]) = 0 ) or
         (FPlainDriver.db_change_nchar_charset(FHandle, @FByteBuffer[0]) = 0 ) then
      {$ELSE}
      RawTemp := 'Set client characterset to: '+FClientCodePage;
      if (FPlainDriver.db_change_char_charset(FHandle, Pointer(FClientCodePage)) = 0 ) or
         (FPlainDriver.db_change_nchar_charset(FHandle, Pointer(FClientCodePage)) = 0 ) then
      {$ENDIF}
        HandleErrorOrWarning(lcConnect, RawTemp, IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, RawTemp);
  except
    on E: Exception do
    begin
      if Assigned(FHandle) then
        FPlainDriver.db_fini(FHandle);
      FHandle := nil;
      raise;
    end;
  end;

  inherited Open;
  if FClientCodePage = ''  then
    CheckCharEncoding(DetermineASACharSet);
  DetermineHostVersion;
  if FHostVersion >= 17000000 then
    SetOption(1, nil, 'chained', 'On', lcTransaction); //chained is deprecated On is comparable with AutoCommit=off
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

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZASAConnection.ClearWarnings;
begin
  FreeAndNil(FLastWarning);
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
function TZASAConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then Open;
  Result := TZASACallableStatement.Create(Self, Name, Info);
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
function TZASAConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  Result := TZASAPreparedStatement.Create(Self, SQL, Info);
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZASAConnection.Rollback;
var S: RawByteString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := 'ROLLBACK TO SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    FPlainDriver.dbpp_rollback(FHandle, 0);
    if FHandle.SqlCode <> SQLE_NOERROR then
      HandleErrorOrWarning(lcTransaction, 'TRANSACTION ROLLBACK', IImmediatelyReleasable(FWeakImmediatRelPtr))
    else if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol,
        'TRANSACTION ROLLBACK');
   // SetOption(1, nil, 'CHAINED', 'ON');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
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
procedure TZASAConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      if FHostVersion >= 17000000
      then SetOption(1, nil, 'AUTO_COMMIT', 'On', lcTransaction)
      else SetOption(1, nil, 'chained', 'Off', lcTransaction);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

procedure TZASAConnection.SetOption(Temporary: Integer; User: PAnsiChar;
  const Option, Value: RawByteString; LoggingCategory: TZLoggingCategory);
var
  SQLDA: PASASQLDA;
  Sz: Integer;
  logStr: RawByteString;
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
      FPlainDriver.dbpp_setoption(FHandle, Temporary, User, Pointer(Option), SQLDA);
      logstr := 'SET OPTION '+ConSettings.User+'.'+Option+' = '+Value;
      if FHandle.SqlCode <> SQLE_NOERROR then
        HandleErrorOrWarning(LoggingCategory, logstr, IImmediatelyReleasable(FWeakImmediatRelPtr))
      else if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, logstr);
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
      SetOption(1, nil, 'ISOLATION_LEVEL', ASATIL[TransactIsolationLevel], lcTransaction);
    TransactIsolationLevel := Level;
  end;
end;

{**
   Start transaction
}
function TZASAConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    if FHostVersion >= 17000000
    then SetOption(1, nil, 'AUTO_COMMIT', 'Off', lcTransaction)
    else SetOption(1, nil, 'chained', 'On', lcTransaction);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := '"SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count)+'"';
    ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
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
begin
  FPlainDriver.dbpp_execute_imm(FHandle, Pointer(SQL), 0);
  if FHandle.SqlCode <> SQLE_NOERROR then
    HandleErrorOrWarning(LoggingCategory, SQL, IImmediatelyReleasable(FWeakImmediatRelPtr))
  else if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, SQL);
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
