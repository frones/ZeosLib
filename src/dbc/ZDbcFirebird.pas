{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZDbcFirebird;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZDbcUtils, ZDbcIntfs, ZDbcConnection,
  ZPlainFirebirdInterbaseDriver, ZSysUtils, ZDbcLogging, ZDbcInterbase6Utils,
  ZGenericSqlAnalyser, ZClasses, ZDbcFirebirdInterbase,
  ZPlainFirebird;

type

  {** Implements Interbase6 Database Driver. }
  TZFirebirdDriver = class(TZInterbaseFirebirdDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
  end;

  IZFirebirdTransaction = interface(IZInterbaseFirebirdTransaction)
    ['{CBCAE412-34E8-489A-A022-EAE325F6D460}']
    function GetTransaction: ITransaction;
  end;

  IZFirebirdConnection = interface(IZInterbaseFirebirdConnection)
    ['{C986AC0E-BC37-44B5-963F-65646333AF7C}']
    function GetActiveTransaction: IZFirebirdTransaction;
    function GetAttachment: IAttachment;
    function GetStatus: IStatus;
    function GetPlainDriver: TZFirebirdPlainDriver;
  end;

  TZFirebirdTransaction = class(TZInterbaseFirebirdTransaction,
    IZTransaction, IZFirebirdTransaction, IZInterbaseFirebirdTransaction)
  private
    FTransaction: ITransaction;
  protected
    function TxnIsStarted: Boolean; override;
    function TestCachedResultsAndForceFetchAll: Boolean; override;
  public { implement ITransaction}
    procedure Commit;
    procedure Close;
    function IsClosed: Boolean;
    procedure Rollback;
    function GetConnection: IZConnection;
    function StartTransaction: Integer;
  public
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); override;
  public { implement IZIBTransaction }
    procedure DoStartTransaction;
  public { implement IZFirebirdTransaction }
    function GetTransaction: ITransaction;
  end;

  { TZFirebirdConnection }
  TZFirebirdConnection = class(TZInterbaseFirebirdConnection, IZConnection,
    IZTransactionManager, IZFirebirdConnection, IZInterbaseFirebirdConnection)
  private
    FMaster: IMaster;
    FProvider: IProvider;
    FAttachment: IAttachment;
    FStatus: IStatus;
    FUtil: IUtil;
    FPlainDriver: TZFirebirdPlainDriver;
    function ConstructConnectionString: String;
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
  public { IZFirebirdConnection }
    function GetActiveTransaction: IZFirebirdTransaction;
    function GetAttachment: IAttachment;
    function GetStatus: IStatus;
    function GetPlainDriver: TZFirebirdPlainDriver;
  public { IZTransactionManager }
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const Name: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;
  public
    function IsFirebirdLib: Boolean; override;
    function IsInterbaseLib: Boolean; override;
  public
    function AbortOperation: Integer; override;
    procedure Open; override;
  end;

{$ENDIF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses ZFastCode, ZDbcFirebirdStatement, ZDbcInterbaseFirebirdMetadata, ZEncoding,
  ZDbcMetadata, ZMessages, ZPlainDriver,
  {$IFNDEF ZEOS_DISABLE_INTERBASE}ZDbcInterbase6,{$ENDIF}
  ZDbcProperties, Math
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES},System.Types{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZFirebirdDriver }

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the zeos driver manager is asked to connect to a given URL it passes
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
function TZFirebirdDriver.Connect(const Url: TZURL): IZConnection;
var iPlainDriver: IZPlainDriver;
    FirebirdPlainDriver: TZFirebirdPlainDriver;
begin
  iPlainDriver := GetPlainDriver(URL, True);
  FirebirdPlainDriver := iPlainDriver.GetInstance as TZFirebirdPlainDriver;
  if Assigned(FirebirdPlainDriver.fb_get_master_interface) then
    Result := TZFirebirdConnection.Create(URL)
  else
    {$IFDEF ZEOS_DISABLE_INTERBASE}
    raise EZSQLException.Create('couldn''t find fb_get_master_interface in client library!');
    {$ELSE ZEOS_DISABLE_INTERBASE}
    Result := TZInterbase6Connection.Create(Url);
    {$ENDIF ZEOS_DISABLE_INTERBASE}
end;

{**
  Constructs this object with default properties.
}
constructor TZFirebirdDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZFirebirdPlainDriver.Create));
end;

{ TZFirebirdConnection }

function TZFirebirdConnection.AbortOperation: Integer;
begin
  if FAttachment = nil
  then Result := 0
  else begin
    FAttachment.cancelOperation(FStatus, fb_cancel_abort);
    Result := Ord((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF}) <> 0);
    if Result <> 0 then
      FStatus.init;
  end;
end;

{**
  Constructs the connection string for the current connection
}
function TZFirebirdConnection.ConstructConnectionString: String;
var Protocol: String;
begin
  Protocol := Info.Values[ConnProps_FBProtocol];
  Protocol := LowerCase(Protocol);
  if ((Protocol = 'inet') or (Protocol = 'wnet') or (Protocol = 'xnet')) then begin
    Result := Protocol+'://';
    if protocol = 'inet' then begin
      Result := Result + HostName;
      if Port <> 0 then
        Result := Result + ':' + ZFastCode.IntToStr(Port);
      Result := Result + '/' + Database
    end else if Protocol = 'wnet' then begin
      if HostName <> '' then
        Result := Result + HostName;
      Result := Result + '/' + Database
    end else
      Result := Result + Database;
  end else if (Protocol <> 'local') and (HostName <> 'localhost') and (HostName <> '') then
    if Port <> 0
      then Result := HostName + '/' + ZFastCode.IntToStr(Port) + ':' + Database
      else Result := HostName + ':' + Database
    else if StrToBoolEx(Info.Values[ConnProps_CreateNewDatabase])
      then Result := Database
      else Result := '';
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
function TZFirebirdConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
    Open;
  Result := TZFirebirdStatement.Create(Self, Info);
end;

{**
  Creates a <code>Transaction</code>
  @param AutoCommit the AutoCommit mode.
  @param ReadOnly the ReadOnly mode.
  @param ReadOnly the ReadOnly mode.
  @param TransactIsolationLevel one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
  @param Value returns the Transaction object.
  @see IZTransaction
  @return the index in the manager list of the new transaction
}
function TZFirebirdConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
begin
  if Params = nil then
    Params := Info;
  Result := TZFirebirdTransaction.Create(Self, AutoCommit, ReadOnly, TransactIsolationLevel, Params);
  fTransactions.Add(Result);
end;

procedure TZFirebirdConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var ZTrans: IZFirebirdTransaction;
    FBTrans: ITransaction;
    Stmt: IStatement;
    FStatementType: TZIbSqlStatementType;
    State: Cardinal;
begin
  ZTrans := GetActiveTransaction;
  FBTrans := ZTrans.GetTransaction;
  {$IFDEF UNICODE}
  if DriverManager.HasLoggingListener then
    FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if LoggingCategory = lcTransaction then begin
    FAttachment.execute(FStatus, FBTrans, Length(SQL), Pointer(SQL),
      FDialect, nil, nil, nil, nil);
    State := Fstatus.getState;
    if ((State and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
       ((State and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
      {$IFDEF UNICODE}
      if not DriverManager.HasLoggingListener then
        FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end else begin
    Stmt := FAttachment.prepare(FStatus, FBTrans, Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Pointer(SQL), FDialect, 0);
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
       ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
      {$IFDEF UNICODE}
      if not DriverManager.HasLoggingListener then
        FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
      {$ENDIF}
      HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
        {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
    FStatementType := TZIbSqlStatementType(Stmt.getType(FStatus));
    try
      if FStatementType in [stGetSegment, stPutSegment, stStartTrans..stRollback]
      then raise EZSQLException.Create(SStatementIsNotAllowed)
      else begin
        Stmt.execute(FStatus, FBTrans, nil, nil, nil, nil);
        if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) or
           ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_WARNINGS{$ELSE}IStatus_STATE_WARNINGS{$ENDIF}) <> 0) then begin
          {$IFDEF UNICODE}
          if not DriverManager.HasLoggingListener then
            FLogMessage := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
          {$ENDIF}
          HandleErrorOrWarning(LoggingCategory, PARRAY_ISC_STATUS(FStatus.getErrors),
            {$IFDEF UNICODE}FLogMessage{$ELSE}SQL{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
        end;
      end;
    finally
      Stmt.free(FStatus);
      Stmt.release;
    end;
  end;
end;

function TZFirebirdConnection.GetActiveTransaction: IZFirebirdTransaction;
var TA: IZTransaction;
begin
  if not Closed then begin
    if fActiveTransaction = nil then begin
      TA := CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction);
    end;
    fActiveTransaction.QueryInterface(IZFirebirdTransaction, Result);
  end else
    Result := nil;
end;

function TZFirebirdConnection.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TZFirebirdConnection.GetPlainDriver: TZFirebirdPlainDriver;
begin
  Result := FPlainDriver;
end;

function TZFirebirdConnection.GetStatus: IStatus;
begin
  Result := FStatus;
end;

procedure TZFirebirdConnection.InternalClose;
begin
  inherited InternalClose;
  if FAttachment <> nil then begin
    FAttachment.detach(FStatus);
    FAttachment.release;
    FAttachment := nil;
  end;
  if FProvider <> nil then begin
    FProvider.release;
    FProvider := nil;
  end;
  if FStatus <> nil then begin
    FStatus.dispose;
    FStatus := nil;
  end;
end;

{**
  Constructs this object and assignes the main properties.
}
procedure TZFirebirdConnection.InternalCreate;
begin
  FPlainDriver := PlainDriver.GetInstance as TZFirebirdPlainDriver;
  inherited InternalCreate;
  { set default sql dialect it can be overriden }
  FMaster := FPlainDriver.fb_get_master_interface;
end;

function TZFirebirdConnection.IsFirebirdLib: Boolean;
begin
  Result := True;
end;

function TZFirebirdConnection.IsInterbaseLib: Boolean;
begin
  Result := False;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZFirebirdConnection.Open;
var
  ti: IZFirebirdTransaction;
  DPB{$IFDEF UNICODE},R{$ENDIF}: RawByteString;
  DBCP, ConnectionString, CSNoneCP, CreateDB: String;
  DBName: array[0..512] of AnsiChar;
  P: PAnsiChar;
  DBCreated: Boolean;
  Statement: IZStatement;
  CP: Word;
  TimeOut: Cardinal;
  procedure PrepareDPB;
  var
    R: RawByteString;
    P: PAnsiChar;
    L: LengthInt;
  begin
    if (Info.IndexOf('isc_dpb_utf8_filename') = -1)
    then CP := zOSCodePage
    else CP := zCP_UTF8;
    {$IFDEF UNICODE}
    R := ZUnicodeToRaw(ConnectionString, CP);
    {$ELSE}
    R := ZConvertStringToRawWithAutoEncode(ConnectionString, ConSettings^.CTRL_CP, CP);
    {$ENDIF}
    DPB := GenerateDPB(FPlainDriver, Info, ConSettings, CP);
    P := Pointer(R);
    L := Min(SizeOf(DBName)-1, Length(R){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
    if P <> nil then
      Move(P^, DBName[0], L);
    AnsiChar((PAnsiChar(@DBName[0])+L)^) := AnsiChar(#0);
  end;
label reconnect, jmpTimeOuts;
begin
  if not Closed then
    Exit;
  FProvider := FMaster.getDispatcher;
  FStatus := FMaster.getStatus;
  FUtil := FMaster.getUtilInterface;
  DBCP := '';
  if TransactIsolationLevel = tiReadUncommitted then
    raise EZSQLException.Create('Isolation level do not capable');
  if ConSettings^.ClientCodePage = nil then
    CheckCharEncoding(FClientCodePage, True);

  AssignISC_Parameters;
  CSNoneCP := Info.Values[ConnProps_Charset_NONE_Alias];
  ConnectionString := ConstructConnectionString;

  DBCreated := False;
  CreateDB := Info.Values[ConnProps_CreateNewDatabase];
  if (CreateDB <> '') and StrToBoolEx(CreateDB) then begin
    DBCP := Info.Values[ConnProps_isc_dpb_set_db_charset];
    PrepareDPB;
    FAttachment := FProvider.createDatabase(FStatus, @DBName[0], Smallint(Length(DPB)),Pointer(DPB));
    Info.Values[ConnProps_CreateNewDatabase] := ''; //prevent recreation on open
    DBCreated := True;
    FLogMessage := 'CREATE DATABASE "'+URL.Database+'" AS USER "'+ URL.UserName+'"';
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
        FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  end;
reconnect:
  if FProvider = nil then
    FProvider := FMaster.getDispatcher;
  if FStatus  = nil then
    FStatus := FMaster.getStatus;
  if FAttachment = nil then begin
    PrepareDPB;
    FLogMessage := Format(SConnect2AsUser, [URL.Database, URL.UserName]);;
    {$IFDEF UNICODE}
    R := ZUnicodeToRaw(URL.Database, CP);
    P := Pointer(R);
    {$ELSE}
    P := Pointer(URL.Database);
    {$ENDIF}
    FAttachment := FProvider.attachDatabase(FStatus, PAnsichar(P), Length(DPB), Pointer(DPB));
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcConnect, PARRAY_ISC_STATUS(FStatus.getErrors),
        FLogMessage, IImmediatelyReleasable(FWeakImmediatRelPtr));
    { Logging connection action }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, URL.Protocol, FLogMessage);
  end;
  { Dialect could have changed by isc_dpb_set_db_SQL_dialect command }
  DBName[0] := AnsiChar(isc_info_db_SQL_Dialect);
  FAttachment.getInfo(FStatus, 1, @DBName[0], SizeOf(DBName)-1, @DBName[1]);
  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
    HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors), 'IAttachment.getInfo', Self);
  if DBName[1] = AnsiChar(isc_info_db_SQL_Dialect)
  then FDialect := ReadInterbase6Number(FPlainDriver, DBName[2])
  else FDialect := SQL_DIALECT_V5;
  inherited SetAutoCommit(AutoCommit or (Info.IndexOf(TxnProps_isc_tpb_autocommit) <> -1));
  FRestartTransaction := not AutoCommit;

  FHardCommit := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
  if (DBCP <> '') and not DBCreated then
    goto jmpTimeOuts;
  inherited Open;
  with GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo do
  begin
    CollectServerInformations; //keep this one first!
    FHostVersion := GetHostVersion;
    FXSQLDAMaxSize := GetMaxSQLDASize;
  end;

  {Check for ClientCodePage: if empty switch to database-defaults
    and/or check for charset 'NONE' which has a different byte-width
    and no convertions where done except the collumns using collations}
  if not DBCreated then begin
    Statement := CreateStatementWithParams(nil);
    try
      with Statement.ExecuteQuery('SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE') do begin
        if Next then DBCP := GetString(FirstDbcIndex);
        Close;
      end;
    finally
      Statement := nil;
    end;
    ti := GetActiveTransaction;
    try
      ti.Close;
    finally
      ti := nil;
    end;
  end;
  if DBCP = 'NONE' then begin { SPECIAL CASE CHARCTERSET "NONE":
    EH: the server makes !NO! charset conversion if CS_NONE.
    Attaching a CS "NONE" db with a characterset <> CS_NONE has this effect:
    All field codepages are retrieved as the given client-characterset.
    This works nice as long the fields have it's own charset definition.
    But what's the encoding of the CS_NONE fields? The more what about CLOB encoding?

    If we're attaching the db with CS "NONE" all userdefined field CP's are
    returned gracefully. Zeos can convert everything to your Controls-CP.
    Except the CP_NONE fields. And the text lob's where encoding is unknown too.
    For the Unicode-IDE's this case is a nightmare. Jan's suggestion is to use
    the fields as Byte/BlobFields only. My idea is to use such fields with the
    CPWIN1252 Charset which maps each byte to words and vice versa.
    So no information is lost and the data is still readable "somehow".

    Side-note: see: https://firebirdsql.org/rlsnotesh/str-charsets.html
    Any DDL/DML in non ASCII7 range will give a maleformed string if encoding is
    different to UTF8/UNICODE_FSS because the RDB$-Tables have a
    UNICODE_FSS collation}

    {test if charset is not given or is CS_NONE }
    if CSNoneCP = ''
    then CSNoneCP := FClientCodePage
    else if FCLientCodePage <> ''
      then CSNoneCP := FCLientCodePage
      else CSNoneCP := 'WIN1252'; {WIN1252 would be optimal propably}
    ResetCurrentClientCodePage(CSNoneCP, False);
    ConSettings^.ClientCodePage^.ID := 0;
    //Now notify our metadata object all fields are retrieved in utf8 encoding
    (FMetadata as TZInterbase6DatabaseMetadata).SetUTF8CodePageInfo;
    if (FCLientCodePage <> DBCP) then begin
      Info.Values[ConnProps_isc_dpb_lc_ctype] := DBCP;
      InternalClose;
      goto reconnect; //build new TDB and reopen in SC_NONE mode
    end;
  end else if FClientCodePage = '' then
    CheckCharEncoding(DBCP);
jmpTimeOuts:
  if (FAttachment.vTable.version >= 4) and (FHostVersion >= 4000000) then begin
    TimeOut := StrToIntDef(Info.Values[ConnProps_StatementTimeOut], 0);
    if TimeOut > 0 then begin
      IAttachment_V4(FAttachment).SetStatementTimeOut(Fstatus, TimeOut);
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
        HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
          'IAttachment.SetStatmentTimeOut', IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
    TimeOut := StrToIntDef(Info.Values[ConnProps_SessionIdleTimeOut], 0);
    if TimeOut > 16 then begin
      IAttachment_V4(FAttachment).setIdleTimeout(Fstatus, TimeOut);
      if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
        HandleErrorOrWarning(lcOther, PARRAY_ISC_STATUS(FStatus.getErrors),
          'IAttachment.setIdleTimeout', IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;

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
function TZFirebirdConnection.PrepareCallWithParams(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
    Open;
  Result := TZFirebirdCallableStatement.Create(Self, SQL, Info);
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
function TZFirebirdConnection.PrepareStatementWithParams(const Name: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZFirebirdPreparedStatement.Create(Self, Name, Info);
end;

var
  FireBirdDriver: IZDriver;

{ TZFirebirdTransaction }

procedure TZFirebirdTransaction.Close;
begin
  if FTransaction <> nil then with TZFirebirdConnection(FOwner) do begin
    if fDoCommit
    then FTransaction.commit(FStatus)
    else FTransaction.rollback(FStatus);
    FTransaction.release;
    FTransaction := nil;
    fSavepoints.Clear;
	  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sCommitMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
    FOwner.ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZFirebirdTransaction.Commit;
var S: RawByteString;
begin
  with TZFirebirdConnection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'RELEASE SAVEPOINT '+ {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTransaction <> nil then try
    if TZFirebirdConnection(FOwner).FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then begin
      FTransaction.commit(FStatus);
      {$IFDEF DEBUG}Assert({$ENDIF}FTransaction.Release{$IFDEF DEBUG} = 0){$ENDIF};
      FTransaction := nil;
    end else begin
      fDoCommit := True;
      fDoLog := False;
      FTransaction.commitRetaining(FStatus);
      ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
    end;
	  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sCommitMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sCommitMsg);
  end;
end;

procedure TZFirebirdTransaction.DoStartTransaction;
begin
  GetTransaction;
end;

function TZFireBirdTransaction.GetConnection: IZConnection;
begin
  Result := FOwner as TZFirebirdConnection;
  FOwner.SetActiveTransaction(Self);
end;

function TZFireBirdTransaction.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
    StartTransaction;
  Result := FTransaction;
end;

function TZFirebirdTransaction.IsClosed: Boolean;
begin
  Result := FTransaction = nil;
end;

procedure TZFirebirdTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  try
    if FTransaction <> nil then begin
      FTransaction.release;
      FTransaction := nil;
    end;
  finally
    inherited;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZFirebirdTransaction.Rollback;
var S: RawByteString;
begin
  with TZFirebirdConnection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'ROLLBACK TO '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
  end else if FTransaction <> nil then with TZFirebirdConnection(FOwner) do try
    if FHardCommit or
      ((FOpenCursors.Count = 0) and (FOpenUncachedLobs.Count = 0)) or
      ((FOpenUncachedLobs.Count = 0) and TestCachedResultsAndForceFetchAll)
    then begin
      FTransaction.rollback(FStatus);
      {$IFDEF DEBUG}Assert({$ENDIF}FTransaction.Release{$IFDEF DEBUG} = 0){$ENDIF};
      FTransaction := nil;
    end else begin
      fDoCommit := True;
      fDoLog := False;
      FTransaction.rollbackRetaining(FStatus);
      ReleaseTransaction(IZTransaction(FWeakIZTransactionPtr));
    end;
	  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleErrorOrWarning(lcTransaction, PARRAY_ISC_STATUS(FStatus.getErrors),
        sRollbackMsg, IImmediatelyReleasable(FWeakImmediatRelPtr));
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, URL.Protocol, sRollbackMsg);
  end;
end;

{**
  Starts transaction support or saves the current transaction.
  If the connection is closed, the connection will be opened.
  If a transaction is underway a nested transaction or a savepoint will be
  spawned. While the tranaction(s) is/are underway the AutoCommit property is
  set to False. Ending up the transaction with a commit/rollback the autocommit
  property will be restored if changing the autocommit mode was triggered by a
  starttransaction call.
  @return the current txn-level. 1 means a transaction was started.
  2 means the transaction was saved. 3 means the previous savepoint got saved
  too and so on
}
function TZFirebirdTransaction.StartTransaction: Integer;
var S: String;
begin
  with TZFirebirdConnection(FOwner) do begin
    if FTransaction = nil then begin
      if FTPB = EmptyRaw then
        FTPB := FOwner.GenerateTPB(FAutoCommit, FReadOnly, FTransactionIsolation, FProperties);
      FTransaction := FAttachment.startTransaction(FStatus,
        Length(FTPB){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Pointer(FTPB));
      FTransaction.AddRef;
      Result := Ord(not Self.FAutoCommit);
      DriverManager.LogMessage(lcTransaction, URL.Protocol, 'TRANSACTION STARTED.');
    end else begin
      Result := FSavePoints.Count+2;
      S := 'SP'+ZFastcode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result);
      ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
      Result := FSavePoints.Add(S)+2;
    end;
  end;
end;

function TZFirebirdTransaction.TestCachedResultsAndForceFetchAll: Boolean;
var I, RowNo: Integer;
  P: Pointer;
begin
  Result := False;
  for I := 0 to FOpenCursors.Count -1 do
    if IZResultSet(FOpenCursors[i]).GetConcurrency <> rcUpdatable then
      Exit;
  Result := True;
  while FOpenCursors.Count > 0 do begin
    P := FOpenCursors[FOpenCursors.Count-1];
    RowNo := IZResultSet(P).GetRow;
    IZResultSet(P).Last; //now the pointer will be removed from the open cursor list
    IZResultSet(P).MoveAbsolute(RowNo); //restore current position
  end;
end;

function TZFirebirdTransaction.TxnIsStarted: Boolean;
begin
  Result := FTransaction <> nil;
end;

initialization
  FireBirdDriver := TZFirebirdDriver.Create;
  DriverManager.RegisterDriver(FireBirdDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(FireBirdDriver);
  FireBirdDriver := nil;
{$ENDIF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
end.
