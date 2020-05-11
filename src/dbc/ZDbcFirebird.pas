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
    procedure HandleError(Status: IStatus; const msg: RawByteString;
      Sender: IImmediatelyReleasable; LoggingCategory: TZLoggingCategory);
    function GetPlainDriver: TZFirebird3UpPlainDriver;
  end;

  TZFireBirdTransaction = class(TZInterbaseFirebirdTransaction,
    IZTransaction, IZFirebirdTransaction, IZInterbaseFirebirdTransaction)
  private
    FTransaction: ITransaction;
  protected
    function TxnIsStarted: Boolean; override;
    function TestCachedResultsAndForceFetchAll: Boolean; override;
  public { implement ITransaction}
    procedure Commit;
    procedure Rollback;
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
    FPlainDriver: TZFirebird3UpPlainDriver;
    function ConstructConnectionString(out LocalDB: Boolean): String;
  protected
    procedure DetermineClientTypeAndVersion; override;
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
  public { IZFirebirdConnection }
    function GetActiveTransaction: IZFirebirdTransaction;
    function GetAttachment: IAttachment;
    function GetStatus: IStatus;
    function GetPlainDriver: TZFirebird3UpPlainDriver;
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
    procedure HandleError(Status: IStatus; const msg: RawByteString; Sender: IImmediatelyReleasable;
      LoggingCategory: TZLoggingCategory);
  public
    procedure Open; override;
  end;

{$ENDIF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses ZFastCode, ZDbcFirebirdStatement, ZDbcInterbaseFirebirdMetadata, ZEncoding,
  ZDbcMetadata, ZMessages,
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
begin
  Result := TZFirebirdConnection.Create(URL);
end;

{**
  Constructs this object with default properties.
}
constructor TZFirebirdDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZFirebird3UpPlainDriver.Create));
end;

{ TZFirebirdConnection }

{**
  Constructs the connection string for the current connection
}
function TZFirebirdConnection.ConstructConnectionString(out LocalDB: Boolean): String;
var Protocol: String;
begin
  Protocol := Info.Values[ConnProps_FBProtocol];
  Protocol := LowerCase(Protocol);
  LocalDB := False;
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
  end else if (Protocol <> 'local') and (HostName <> 'localhost') then
    if Port <> 0
      then Result := HostName + '/' + ZFastCode.IntToStr(Port) + ':' + Database
      else Result := HostName + ':' + Database
    else begin
      Result := Database;
      LocalDB := True;
    end;
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
  Result := TZFirebirdStatement.Create(Self, Info);
end;

function TZFirebirdConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
begin
  if FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel] = EmptyRaw then
    FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel] := GenerateTPB(AutoCommit, ReadOnly, TransactIsolationLevel, Params);
  Result := TZFirebirdTransaction.Create(Self, AutoCommit, ReadOnly, FTPBs[AutoCommit][ReadOnly][TransactIsolationLevel]);
end;

procedure TZFirebirdConnection.DetermineClientTypeAndVersion;
begin
  FClientVersion := 3000000 //just a dummy by now
end;

procedure TZFirebirdConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var ZTrans: IZFirebirdTransaction;
    FBTrans: ITransaction;
begin
  ZTrans := GetActiveTransaction;
  FBTrans := ZTrans.GetTransaction;
  FAttachment.execute(FStatus, FBTrans, Length(SQL), Pointer(SQL),
    FDialect, nil, nil, nil, nil);
  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
    HandleError(FStatus, SQL, Self, LoggingCategory);
end;

function TZFirebirdConnection.GetActiveTransaction: IZFirebirdTransaction;
var TA: IZTransaction;
begin
  if not Closed then begin
    if fActiveTransaction[ReadOnly] = nil then begin
      TA := CreateTransaction(AutoCommit, ReadOnly, TransactIsolationLevel, Info);
      TA.QueryInterface(IZInterbaseFirebirdTransaction, fActiveTransaction[ReadOnly]);
      fTransactions[ReadOnly].Add(TA);
    end;
    fActiveTransaction[ReadOnly].QueryInterface(IZFirebirdTransaction, Result);
  end else
    Result := nil;
end;

function TZFirebirdConnection.GetAttachment: IAttachment;
begin
  Result := FAttachment;
end;

function TZFirebirdConnection.GetPlainDriver: TZFirebird3UpPlainDriver;
begin
  Result := FPlainDriver;
end;

function TZFirebirdConnection.GetStatus: IStatus;
begin
  Result := FStatus;
end;

procedure TZFirebirdConnection.HandleError(Status: IStatus;
  const msg: RawByteString; Sender: IImmediatelyReleasable;
  LoggingCategory: TZLoggingCategory);
var
	statusVector: PARRAY_ISC_STATUS;
begin
  if ((status.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) = 0) then
    Exit;
  statusVector := PARRAY_ISC_STATUS(Status.getErrors);
  try
    CheckInterbase6Error(FPlainDriver, statusVector^, Sender, LoggingCategory, Msg);
  finally
    Status.init;
  end;
end;

procedure TZFirebirdConnection.InternalClose;
begin
  inherited;
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
  FPlainDriver := TZFirebird3UpPlainDriver(PlainDriver.GetInstance);
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
  DPB: RawByteString;
  DBCP, ConnectionString, CSNoneCP, CreateDB: String;
  DBName: array[0..512] of AnsiChar;
  LocaleDB: Boolean;
  P: PAnsiChar;
  DBCreated: Boolean;
  Statement: IZStatement;
  procedure PrepareDPB;
  var
    R: RawByteString;
    P: PAnsiChar;
    L: LengthInt;
    CP: Word;
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
label reconnect;
begin
  if not Closed then
    Exit;
  FProvider := FMaster.getDispatcher;
  FStatus := FMaster.getStatus;
  DBCP := '';
  if TransactIsolationLevel = tiReadUncommitted then
    raise EZSQLException.Create('Isolation level do not capable');
  if ConSettings^.ClientCodePage = nil then
    CheckCharEncoding(FClientCodePage, True);

  AssignISC_Parameters;
  CSNoneCP := Info.Values[DSProps_ResetCodePage];
  ConnectionString := ConstructConnectionString(LocaleDB);

  DBCreated := False;
  CreateDB := Info.Values[ConnProps_CreateNewDatabase];
  if (CreateDB <> '') then begin
    if (Info.Values['isc_dpb_lc_ctype'] <> '') and (Info.Values['isc_dpb_set_db_charset'] = '') then
      Info.Values['isc_dpb_set_db_charset'] := Info.Values['isc_dpb_lc_ctype'];
    DBCP := Info.Values['isc_dpb_set_db_charset'];
    PrepareDPB;
    FAttachment := FProvider.createDatabase(FStatus, @DBName[0], Smallint(Length(DPB)),Pointer(DPB));
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleError(FStatus, 'IProvider.createDatabase', Self, lcConnect);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
        'CREATE DATABASE "'+ConSettings.Database+'" AS USER "'+ ConSettings^.User+'"');
    DBCreated := True;
  end;
reconnect:
  if FAttachment = nil then begin
    PrepareDPB;
    if LocaleDB
    then P := Pointer(ConSettings.Database)
    else P := nil;
    FAttachment := FProvider.attachDatabase(FStatus, P, Length(DPB), Pointer(DPB));
    if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleError(FStatus, 'IProvider.attachDatabase', Self, lcConnect);
    { Logging connection action }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
        'CONNECT TO "'+ConSettings^.DataBase+'" AS USER "'+ConSettings^.User+'"');
  end;
  { Dialect could have changed by isc_dpb_set_db_SQL_dialect command }
  DBName[0] := AnsiChar(isc_info_db_SQL_Dialect);
  FAttachment.getInfo(FStatus, 1, @DBName[0], SizeOf(DBName)-1, @DBName[1]);
  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
    HandleError(FStatus, 'IAttachment.getInfo', Self, lcConnect);
  if DBName[1] = AnsiChar(isc_info_db_SQL_Dialect)
  then FDialect := ReadInterbase6Number(FPlainDriver, DBName[2])
  else FDialect := SQL_DIALECT_V5;
  inherited SetAutoCommit(AutoCommit or (Info.IndexOf('isc_tpb_autocommit') <> -1));
  FRestartTransaction := not AutoCommit;

  FHardCommit := StrToBoolEx(Info.Values[ConnProps_HardCommit]);
  if (DBCP <> '') and not DBCreated then
    Exit;
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
      ti.CloseTransaction;
      ReleaseTransaction(ti);
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
      Info.Values['isc_dpb_lc_ctype'] := DBCP;
      InternalClose;
      goto reconnect; //build new TDB and reopen in SC_NONE mode
    end;
  end else if FClientCodePage = '' then
    CheckCharEncoding(DBCP);
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
  Result := TZFirebirdPreparedStatement.Create(Self, Name, Info);
end;

var
  FireBirdDriver: IZDriver;

{ TZFireBirdTransaction }

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZFireBirdTransaction.Commit;
var S: RawByteString;
begin
  with TZFirebirdConnection(FOwner) do
  if fSavepoints.Count > 0 then begin
    S := 'RELEASE SAVEPOINT '+ {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
    ExecuteImmediat(S, lcTransaction);
    FSavePoints.Delete(FSavePoints.Count-1);
    FExplicitTransactionCounter := fSavepoints.Count +1;
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
      ReleaseTransaction(Self);
    end;
    FExplicitTransactionCounter := 0;
	  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleError(FStatus, sCommitMsg, self, lcTransaction);
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, sCommitMsg);
  end;
end;

procedure TZFireBirdTransaction.DoStartTransaction;
begin
  GetTransaction;
end;

function TZFireBirdTransaction.GetTransaction: ITransaction;
begin
  if FTransaction = nil then
    StartTransaction;
  Result := FTransaction;
end;

procedure TZFireBirdTransaction.ReleaseImmediat(
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
procedure TZFireBirdTransaction.Rollback;
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
      ReleaseTransaction(Self);
    end;
    FExplicitTransactionCounter := 0;
	  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      HandleError(FStatus, sRollbackMsg, self, lcTransaction);
  finally
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, sRollbackMsg);
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
function TZFireBirdTransaction.StartTransaction: Integer;
var S: String;
begin
  with TZFirebirdConnection(FOwner) do
  if FTransaction = nil then begin
    FTransaction := FAttachment.startTransaction(FStatus, Length(FTPB), Pointer(FTPB));
    FTransaction.AddRef;
    Result := Ord(not AutoCommit);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION STARTED.');
  end else begin
    Result := FSavePoints.Count+2;
    S := 'SP'+ZFastcode.IntToStr(NativeUInt(Self))+'_'+ZFastCode.IntToStr(Result);
    ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
  end;
  FExplicitTransactionCounter := Result;
end;

function TZFireBirdTransaction.TestCachedResultsAndForceFetchAll: Boolean;
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

function TZFireBirdTransaction.TxnIsStarted: Boolean;
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
