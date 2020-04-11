{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

unit ZDbcOracle;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZClasses, ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainOracleDriver,
  ZDbcLogging, ZTokenizer, ZDbcGenericResolver, ZGenericSqlAnalyser, ZDbcCache,
  ZPlainOracleConstants;

type

  {** Implements Oracle Database Driver. }
  TZOracleDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a Oracle specific connection interface. }
  IZOracleConnection = interface (IZConnection)
    ['{C7F36FDF-8A64-477B-A0EB-3E8AB7C09F8D}']

    function GetConnectionHandle: POCIEnv;
    function GetServiceContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
    function GetDescribeHandle: POCIDescribe;
    function GetPlainDriver: TZOraclePlainDriver;
  end;

  IZOracleTransaction = interface(IZTransaction)
    ['{07C8E090-BE86-4CA9-B2CB-1583DA94AFA6}']
    procedure CloseTransaction;
    function GetTrHandle: POCITrans;
    function IsStarted: Boolean;
    function StartTransaction: Integer;
  end;

  /// <summary>
  ///  implements an oracle OCI connection.
  /// </summary>
  {** Implements Oracle Database Connection. }
  TZOracleConnection = class(TZAbstractDbcConnection, IZConnection, IZOracleConnection)
  private
    FCatalog: string;
    FOCIEnv: POCIEnv;
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FServerHandle: POCIServer;
    FSessionHandle: POCISession;
    FDescibeHandle: POCIDescribe;
    FStatementPrefetchSize: Integer;
    FBlobPrefetchSize: Integer;
    FStmtMode: ub4;
    FPlainDriver: TZOraclePlainDriver;
    fGlobalTransactions: array[Boolean] of IZCollection; //simultan global read-Only/Write transaction container
    fLocalTransaction, //oracle allows just one Local transaction
    fAttachedTransaction: IZOracleTransaction;
    fcharset: UB2;
    procedure ExecuteImmediat(const SQL: RawByteString; var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory); overload;
    procedure ExecuteImmediat(const SQL: UnicodeString; var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory); overload;
    procedure InternalSetCatalog(const Catalog: String);
    procedure LogW(LoggingCategory: TZLoggingCategory; const logMessage: UnicodeString);
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; override;
  public { IZTransactionManager }
    function CreateTransaction(AutoCommit, ReadOnly: Boolean;
      TransactIsolationLevel: TZTransactIsolationLevel; Params: TStrings): IZTransaction;
    procedure ReleaseTransaction(const Transaction: IZTransaction);
    procedure SetActiveTransaction(const Value: IZTransaction);
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;

    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; override;
  public { txn support }
    procedure Commit;
    procedure Rollback;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function StartTransaction: Integer;
  public
    function PingServer: Integer; override;
    function AbortOperation: Integer; override;

    procedure Open; override;

    procedure SetCatalog(const Value: string); override;
    function GetCatalog: string; override;

  public { IZOracleConnection }
    function GetConnectionHandle: POCIEnv;
    function GetServiceContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
    function GetDescribeHandle: POCIDescribe;
    function GetPlainDriver: TZOraclePlainDriver;
  public
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;
    function GetServerProvider: TZServerProvider; override;
  end;

  {** Implements a specialized cached resolver for Oracle. }
  TZOracleCachedResolver = class(TZGenerateSQLCachedResolver)
  public
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): string; override;
  end;

  /// <summary>
  ///  Defines a oracle transaction mode.
  /// </summary>
  TZOCITxnMode = (tmDefault, tmReadOnly, tmReadWrite, tmSerializable);

  /// <summary>
  ///  Defines a oracle transaction spawning mode.
  /// </summary>
  TZOCITxnSpawnMode = (smNew, smJoin{not supported by OCI}, smResume);

  /// <summary>
  ///  Defines a oracle transaction couple mode.
  /// </summary>
  TZOCITxnCoupleMode = (cmTightly, cmLoosely);

  {** EH: implements an oracle transaction }
  TZOracleTransaction = class(TZCodePagedObject, IImmediatelyReleasable,
    IZTransaction, IZOracleTransaction)
  private
    fSavepoints: TStrings;
    fBranches: IZCollection;
    fDoLog, FStarted, fLocal: Boolean;
    FOCITrans: POCITrans;
    FTxnMode: TZOCITxnMode;
    FCoupleMode: TZOCITxnCoupleMode;
    FSpawnMode: TZOCITxnSpawnMode;
    //FTXID: TXID;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwner: TZOracleConnection;
  public { IZTransaction }
    procedure Commit;
    procedure Rollback;
    function StartTransaction: Integer;
  public { IZOracleTransaction }
    procedure CloseTransaction;
    function GetTrHandle: POCITrans;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    function IsStarted: Boolean;
  public
    constructor CreateLocal(const Owner: TZOracleConnection);
    constructor CreateGlobal(const Owner: TZOracleConnection; TxnMode: TZOCITxnMode;
      SpawnMode: TZOCITxnSpawnMode; CoupleMode: TZOCITxnCoupleMode);
    procedure BeforeDestruction; override;
  end;

var
  {** The common driver manager object. }
  OracleDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  ZMessages, ZGenericSqlToken, ZDbcOracleStatement, ZSysUtils, ZFastCode,
  ZDbcOracleUtils, ZDbcOracleMetadata, ZOracleToken, ZOracleAnalyser, ZDbcProperties,
  ZCollections, ZEncoding;

{ TZOracleDriver }

{**
  Constructs this object with default properties.
}
constructor TZOracleDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZOraclePlainDriver.Create));
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
function TZOracleDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOracleConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZOracleDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZOracleDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZOracleDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOracleTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZOracleDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZOracleStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZOracleConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZOracleConnection.InternalCreate;
var S: String;
begin
  FPlainDriver := TZOraclePlainDriver(PlainDriver.GetInstance);
  FMetaData := TZOracleDatabaseMetadata.Create(Self, URL);
  fGlobalTransactions[False] := TZCollection.Create;
  fGlobalTransactions[True ] := TZCollection.Create;
  TransactIsolationLevel := tiReadCommitted;

  { Sets a default properties }
  if Self.Port = 0 then
      Self.Port := 1521;
  S := Info.Values[ConnProps_ServerCachedStmts];
  if (S = '') or StrToBoolEx(S, False)
  then FStmtMode := OCI_STMT_CACHE //use by default
  else FStmtMode := OCI_DEFAULT;
  S := Info.Values[ConnProps_StatementCache];
  FStatementPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(S, 30); //default = 20
  FBlobPrefetchSize := FChunkSize;
end;

procedure TZOracleConnection.InternalSetCatalog(const Catalog: String);
begin
  ExecuteImmediat('ALTER SESSION SET CURRENT_SCHEMA = '+Catalog, lcOther);
end;

procedure TZOracleConnection.LogW(LoggingCategory: TZLoggingCategory;
  const logMessage: UnicodeString);
var R: RawByteString;
begin
  R := ZUnicodeToRaw(logMessage, ConSettings.CTRL_CP);
  DriverManager.LogMessage(LoggingCategory, ConSettings.Protocol, R);
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
var Stmt: POCIStmt;
  Status: sword;
  {$IFDEF UNICODE}
  S: UnicodeString;
  {$ENDIF UNICODE}
begin
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then inherited ExecuteImmediat(SQL, LoggingCategory)
  else begin
    Stmt := nil;
    try
      ExecuteImmediat(SQL, Stmt, LoggingCategory);
    finally
      if Stmt <> nil then begin
        Status := FPlainDriver.OCIHandleFree(Stmt, OCI_HTYPE_STMT);
        if Status <> OCI_SUCCESS then begin
          {$IFDEF UNICODE}
          S := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
          CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, S, ConSettings);
          {$ELSE}
          CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, SQL, ConSettings);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: RawByteString;
  var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory);
var Status: sword;
  {$IFDEF UNICODE}
  S: UnicodeString;
  {$ENDIF}
begin
  if Pointer(SQL) = nil then
    Exit;
  {$IFDEF UNICODE}
  S := ZRawToUnicode(SQL, ConSettings.ClientCodePage.CP);
  {$ENDIF}
  if Stmt = nil then begin
    Status := FPlainDriver.OCIHandleAlloc(GetConnectionHandle,
      Stmt, OCI_HTYPE_STMT, 0, nil);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory,
        'OCIHandleAlloc(OCIStmt-Handle)', ConSettings);
    Status := FPlainDriver.OCIStmtPrepare(Stmt, FErrorHandle, Pointer(SQL),
      Length(SQL){$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF}, OCI_NTV_SYNTAX, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, {$IFDEF UNICODE}S{$ELSE}SQL{$ENDIF}, ConSettings);
  end;
  try
    Status := FPlainDriver.OCIStmtExecute(FContextHandle,
        Stmt, FErrorHandle, 1, 0, nil, nil, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, {$IFDEF UNICODE}S{$ELSE}SQL{$ENDIF}, ConSettings);
  finally
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZOracleConnection.Open;
var
  Status: Integer;
  LogMessage: String;
  ncharset: ub2;
  Succeeded: Boolean;
  {$IFNDEF UNICODE}
  US: UnicodeString;
  {$ELSE}
  R: RawByteString;
  {$ENDIF}
  procedure CleanupOnFail;
  begin
    FPlainDriver.OCIHandleFree(FDescibeHandle, OCI_HTYPE_DESCRIBE);
    FDescibeHandle := nil;
    FPlainDriver.OCIHandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
    FContextHandle := nil;
    FPlainDriver.OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
    FPlainDriver.OCIHandleFree(FServerHandle, OCI_HTYPE_SERVER);
    FServerHandle := nil;
    FPlainDriver.OCIHandleFree(FOCIEnv, OCI_HTYPE_ENV);
    FOCIEnv := nil;
  end;
begin
  if not Closed then
     Exit;

  LogMessage := 'CONNECT TO "'+URL.Database+'" AS USER "'+URL.UserName+'"';

  { Sets a default port number. }
  if Port = 0 then
     Port := 1521;

  { Sets a client codepage. }
  fcharset := ConSettings.ClientCodePage^.ID;
  //EH: do NOT use OCI_CLIENT_NCHARSET_ID if OCI_CLIENT_CHARSET_ID is zero!!
  if fcharset = 0
  then ncharset := 0
  else ncharset := OCI_UTF16ID;

  { Connect to Oracle database. }
  FErrorHandle := nil;
  Status := FPlainDriver.OCIEnvNlsCreate(FOCIEnv, OCI_OBJECT, nil, nil, nil,
    nil, 0, nil, fcharset, ncharset);
  if Status <> OCI_SUCCESS then
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'EnvNlsCreate failed.', ConSettings);
  FErrorHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  FServerHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FServerHandle, OCI_HTYPE_SERVER, 0, nil);
  FContextHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FContextHandle, OCI_HTYPE_SVCCTX, 0, nil);
  FDescibeHandle := nil;
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FDescibeHandle, OCI_HTYPE_DESCRIBE, 0, nil);
  Succeeded := False;
  if fcharset = OCI_UTF16ID then begin
    {$IFDEF UNICODE}
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(URL.Database), Length(URL.Database) shl 1, 0);
    {$ELSE}
    US := ZRawToUnicode(URL.Database, ConSettings.CTRL_CP);
    Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(US), Length(US), 0);
    {$ENDIF}
  end else Status := FPlainDriver.OCIServerAttach(FServerHandle, FErrorHandle,
      Pointer(ConSettings^.Database), Length(ConSettings^.Database), 0);
  if Status <> OCI_SUCCESS then try
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcConnect, LogMessage, ConSettings);
    Succeeded := True;
  finally
    if not Succeeded then
      CleanupOnFail;
  end;

  if fcharset = 0 then begin
    FPlainDriver.OCIAttrGet(FOCIEnv, OCI_HTYPE_ENV, @fcharset,
      nil, OCI_NLS_CHARSET_ID, FErrorHandle); //Get Server default CodePage
    CheckCharEncoding(PlainDriver.ValidateCharEncoding(fcharset)^.Name);
    FPlainDriver.OCIAttrGet(FOCIEnv, OCI_HTYPE_ENV, @ncharset,
      nil, OCI_NLS_NCHARSET_ID, FErrorHandle);
    if ncharset <> OCI_UTF16ID then begin
      CleanupOnFail;
      Open;//recursive call we can not patch the env varibles using OCIAttrSet
      Exit;
    end;
  end;
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.OCINlsNumericInfoGet(FOCIEnv, FErrorHandle,
      @ConSettings^.ClientCodePage^.CharWidth, OCI_NLS_CHARSET_MAXBYTESZ),
    lcConnect, LogMessage, ConSettings);

  FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FServerHandle, 0,
    OCI_ATTR_SERVER, FErrorHandle);
  FPlainDriver.OCIHandleAlloc(FOCIEnv, FSessionHandle, OCI_HTYPE_SESSION, 0, nil);
  if fcharset = OCI_UTF16ID then begin
    {$IFDEF UNICODE}
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(URL.UserName),
      Length(URL.UserName) shl 1, OCI_ATTR_USERNAME, FErrorHandle);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION,
      Pointer(Password), Length(Password) shl 1, OCI_ATTR_PASSWORD, FErrorHandle);
    {$ELSE}
    US := ZRawToUnicode(URL.UserName, ConSettings.CTRL_CP);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(US),
      Length(US) shl 1, OCI_ATTR_USERNAME, FErrorHandle);
    US := ZRawToUnicode(URL.Password, ConSettings.CTRL_CP);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(US),
      Length(US) shl 1, OCI_ATTR_PASSWORD, FErrorHandle);
    {$ENDIF}
  end else begin
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, Pointer(ConSettings^.User),
      Length(ConSettings^.User), OCI_ATTR_USERNAME, FErrorHandle);
    {$IFDEF UNICODE}
    R := UnicodeStringToAscii7(Password);
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION,
      Pointer(R), Length(R), OCI_ATTR_PASSWORD, FErrorHandle);
    {$ELSE}
    FPlainDriver.OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION,
      Pointer(Password), Length(Password), OCI_ATTR_PASSWORD, FErrorHandle);
    {$ENDIF}
  end;
  FPlainDriver.OCIAttrSet(FSessionHandle,OCI_HTYPE_SESSION,@fBlobPrefetchSize,0,
    OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE,FErrorHandle);
  Succeeded := False;
  Status := FPlainDriver.OCISessionBegin(FContextHandle, FErrorHandle,
    FSessionHandle, OCI_CRED_RDBMS, OCI_DEFAULT);
  if Status <> OCI_SUCCESS then try
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcConnect, LogMessage, ConSettings);
    Succeeded := True;
  finally
    if not Succeeded then
      CleanupOnFail;
  end;
  FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FSessionHandle, 0,
    OCI_ATTR_SESSION, FErrorHandle);
  if DriverManager.HasLoggingListener then
    {$IFDEF UNICODE}
    LogW(lcConnect, LogMessage);
    {$ELSE}
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);
    {$ENDIF}
  fLocalTransaction := TZOracleTransaction.CreateLocal(Self);
  SetActiveTransaction(fLocalTransaction);
  fLocalTransaction.StartTransaction;
  inherited Open;
  if FCatalog <> '' then
    InternalSetCatalog(FCatalog);
end;

{**
  Starts a transaction support.
}
function TZOracleConnection.StartTransaction: Integer;
begin
  if Closed then
    Open;
  Result := fAttachedTransaction.StartTransaction;
  AutoCommit := False;
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
Function TZOracleConnection.AbortOperation: Integer;
Begin
  // https://docs.oracle.com/cd/B10501_01/appdev.920/a96584/oci16m96.htm
  Result := FPlainDriver.OCIBreak(FContextHandle, FErrorHandle);
  CheckOracleError(FPlainDriver, FErrorHandle, Result, lcExecute, 'Abort operation', ConSettings);
  Result := 0; //only possible if CheckOracleError dosn't raise an exception
End;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOracleConnection.Commit;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  fAttachedTransaction.Commit;
  if not fAttachedTransaction.IsStarted then begin
    fAttachedTransaction.StartTransaction;
    AutoCommit := not FRestartTransaction;
  end;
end;

procedure TZOracleConnection.ReleaseTransaction(
  const Transaction: IZTransaction);
var OraTxn: IZOracleTransaction;
  I: Integer;
  B: Boolean;
begin
  if (Transaction<> nil) then begin
    if (Transaction.QueryInterface(IZOracleTransaction, OraTxn) = S_OK) then begin
      if (fAttachedTransaction = OraTxn) then
        fAttachedTransaction := nil;
      if (fLocalTransaction = OraTxn) then
        fLocalTransaction := nil;
      for B := False to True do begin
        I := fGlobalTransactions[b].IndexOf(Transaction);
        if I <> -1 then
          fGlobalTransactions[b].Delete(I);
      end;
    end else raise EZSQLException.Create('unknown transaction');
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZOracleConnection.Rollback;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  fAttachedTransaction.Rollback;
  if not fAttachedTransaction.IsStarted then begin
    fAttachedTransaction.StartTransaction;
    AutoCommit := not FRestartTransaction;
  end;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZOracleConnection.PingServer: Integer;
begin
  Result := FPlainDriver.OCIPing(FContextHandle, FErrorHandle, OCI_DEFAULT);
  CheckOracleError(FPlainDriver, FErrorHandle, Result, lcExecute, 'Ping Server', ConSettings);
  Result := 0; //only possible if CheckOracleError dosn't raise an exception
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
function TZOracleConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOracleCallableStatement_W.Create(Self, Name, Info)
  else Result := TZOracleCallableStatement_A.Create(Self, Name, Info);
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
function TZOracleConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOraclePreparedStatement_W.Create(Self, SQL, Info)
  else Result := TZOraclePreparedStatement_A.Create(Self, SQL, Info);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZOracleConnection.InternalClose;
var
  LogMessage: String;
  B: Boolean;
begin
  if Closed or not Assigned(PlainDriver) then
    Exit;
  LogMessage := 'DISCONNECT FROM "'+URL.Database+'"';
  try
    for B := False to True do
      fGlobalTransactions[b].Clear;
    fAttachedTransaction := nil;
    fLocalTransaction := nil;
  finally
    try
      if FSessionHandle <> nil then
        { Closes the session }
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.OCISessionEnd(FContextHandle, FErrorHandle, FSessionHandle,
          OCI_DEFAULT), lcDisconnect, LogMessage, ConSettings);

      if FServerHandle <> nil then
        { Detaches from the server }
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.OCIServerDetach(FServerHandle, FErrorHandle, OCI_DEFAULT),
          lcDisconnect, LogMessage, ConSettings);
    finally
      { Frees all handles }
      if FDescibeHandle <> nil then begin
        FPlainDriver.OCIHandleFree(FDescibeHandle, OCI_HTYPE_DESCRIBE);
        FDescibeHandle := nil;
      end;
      if FSessionHandle <> nil then begin
        FPlainDriver.OCIHandleFree(FSessionHandle, OCI_HTYPE_SESSION);
        FSessionHandle := nil;
      end;
      if FContextHandle <> nil then begin
        FPlainDriver.OCIHandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
        FContextHandle := nil;
      end;
      if FServerHandle <> nil then begin
        FPlainDriver.OCIHandleFree(FServerHandle, OCI_HTYPE_SERVER);
        FServerHandle := nil;
      end;
      if FErrorHandle <> nil then begin
        FPlainDriver.OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
        FErrorHandle := nil;
      end;
      if FOCIEnv <> nil then
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.OCIHandleFree(FOCIEnv, OCI_HTYPE_ENV),
           lcDisconnect, LogMessage, ConSettings);
      if DriverManager.HasLoggingListener then
        {$IFDEF UNICODE}
        LogW(lcConnect, LogMessage);
        {$ELSE}
        DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);
        {$ENDIF}
    end;
  end;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZOracleConnection.GetCatalog: string;
begin
  if not Closed and (FCatalog = '') then
    with CreateStatementWithParams(nil).ExecuteQuery('SELECT SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'') FROM DUAL') do begin
      if Next then
        FCatalog := GetString(FirstDBCIndex);
      Close;
    end;
  Result := FCatalog;
end;

procedure TZOracleConnection.SetActiveTransaction(const Value: IZTransaction);
var OCITA: IZOracleTransaction;
  Status: sword;
begin
  OCITA := Value as IZOracleTransaction;
  { set new transaction handle to service context }
  Status := FPlainDriver.OCIAttrSet(FContextHandle, OCI_HTYPE_SVCCTX, OCITA.GetTrHandle, 0,
    OCI_ATTR_TRANS, FErrorHandle);
  if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, 'OCIAttrSet(OCI_ATTR_TRANS)', ConSettings);
  if FStmtMode = OCI_STMT_CACHE then begin
    Status := FPlainDriver.OCIAttrSet(FContextHandle,OCI_HTYPE_SVCCTX,
      @FStatementPrefetchSize, 0, OCI_ATTR_STMTCACHESIZE, FErrorHandle);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, 'OCIAttrSet(OCI_ATTR_STMTCACHESIZE)', ConSettings);
  end;
  fAttachedTransaction := OCITA;
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
procedure TZOracleConnection.SetAutoCommit(Value: Boolean);
begin
  AutoCommit := Value;
  FRestartTransaction := not Value;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZOracleConnection.SetCatalog(const Value: string);
begin
  if Value <> FCatalog then begin
    FCatalog := Value;
    if not Closed and (Value <> '') then
      InternalSetCatalog(Value);
  end;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZOracleConnection.SetReadOnly(Value: Boolean);
begin
   if (Value and (TransactIsolationLevel = tiSerializable)) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  ReadOnly := Value;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZOracleConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone
  then Level := tiReadCommitted
  else if (Level = tiReadUncommitted) or (Level = tiRepeatableRead) or
    (Readonly and (Level = tiSerializable)) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  if TransactIsolationLevel <> Level then begin
    if not Closed then
      if AutoCommit
      then fLocalTransaction := nil //reastart new TA
      else raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
    TransactIsolationLevel := Level;
  end;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZOracleConnection.CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;
begin
  Result := TZOracleSequence.Create(Self, Sequence, BlockSize);
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
function TZOracleConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  if ConSettings.ClientCodePage.ID = OCI_UTF16ID
  then Result := TZOracleStatement_W.Create(Self, Info)
  else Result := TZOracleStatement_A.Create(Self, Info);

  Result := TZOracleStatement_A.Create(Self, Info);
end;

function ZDbc2OCITxnMode(ReadOnly: Boolean; TIL: TZTransactIsolationLevel): TZOCITxnMode;
begin
  if (ReadOnly and (TIL = tiSerializable)) or
      (TIL = tiReadUncommitted) or
      (TIL = tiRepeatableRead) then
    raise EZSQLException.Create(SIsolationIsNotSupported);
  if ReadOnly then
    Result := tmReadOnly
  else if TIL = tiSerializable then
    Result := tmSerializable
  else if TIL = tiReadCommitted then
    Result := tmReadWrite
  else Result := tmDefault;
end;

function TZOracleConnection.CreateTransaction(AutoCommit, ReadOnly: Boolean;
  TransactIsolationLevel: TZTransactIsolationLevel;
  Params: TStrings): IZTransaction;
var TxnMode: TZOCITxnMode;
begin
  //2Phase Txn/global?
  TxnMode := ZDbc2OCITxnMode(ReadOnly, TransactIsolationLevel);
  Result := TZOracleTransaction.CreateGlobal(Self, TxnMode, smNew, cmLoosely);
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var Stmt: POCIStmt;
  Status: sword;
  {$IFNDEF UNICODE}
  R: RawByteString;
  {$ENDIF}
begin
  if ConSettings.ClientCodePage.ID <> OCI_UTF16ID
  then inherited ExecuteImmediat(SQL, LoggingCategory)
  else begin
    Stmt := nil;
    try
      ExecuteImmediat(SQL, Stmt, LoggingCategory);
    finally
      if Stmt <> nil then begin
        Status := FPlainDriver.OCIHandleFree(Stmt, OCI_HTYPE_STMT);
        if Status <> OCI_SUCCESS then begin
          {$IFNDEF UNICODE}
          R := ZUnicodeToRaw(SQL, ConSettings.CTRL_CP);
          CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, R, ConSettings);
          {$ELSE}
          CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, SQL, ConSettings);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

procedure TZOracleConnection.ExecuteImmediat(const SQL: UnicodeString;
  var Stmt: POCIStmt; LoggingCategory: TZLoggingCategory);
var Status: sword;
  {$IFNDEF UNICODE}
  R: RawByteString;
  {$ENDIF}
begin
  if Pointer(SQL) = nil then
    Exit;
  {$IFNDEF UNICODE}
  R := ZUnicodeToRaw(SQL, ConSettings.CTRL_CP);
  {$ENDIF}
  if Stmt = nil then begin
    Status := FPlainDriver.OCIHandleAlloc(GetConnectionHandle,
      Stmt, OCI_HTYPE_STMT, 0, nil);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory,
        'OCIHandleAlloc(OCIStmt-Handle)', ConSettings);
    Status := FPlainDriver.OCIStmtPrepare(Stmt, FErrorHandle, Pointer(SQL),
      (Length(SQL)+1) shl 1, OCI_NTV_SYNTAX, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, {$IFNDEF UNICODE}R{$ELSE}SQL{$ENDIF}, ConSettings);
  end;
  try
    Status := FPlainDriver.OCIStmtExecute(FContextHandle,
        Stmt, FErrorHandle, 1, 0, nil, nil, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, LoggingCategory, {$IFNDEF UNICODE}R{$ELSE}SQL{$ENDIF}, ConSettings);
  finally
    if DriverManager.HasLoggingListener then
      LogW(LoggingCategory, SQL);
  end;
end;

{**
  Gets a reference to Oracle connection handle.
  @return a reference to Oracle connection handle.
}
function TZOracleConnection.GetConnectionHandle: POCIEnv;
begin
  Result := FOCIEnv;
end;

{**
  Gets a reference to Oracle context handle.
  @return a reference to Oracle context handle.
}
function TZOracleConnection.GetServiceContextHandle: POCISvcCtx;
begin
  Result := FContextHandle;
end;

{**
  Gets a reference to Oracle error handle.
  @return a reference to Oracle error handle.
}
function TZOracleConnection.GetErrorHandle: POCIError;
begin
  Result := FErrorHandle;
end;

{**
  Gets a reference to Oracle server handle.
  @return a reference to Oracle server handle.
}
function TZOracleConnection.GetServerHandle: POCIServer;
begin
  Result := FServerHandle;
end;

function TZOracleConnection.GetServerProvider: TZServerProvider;
begin
  Result := spOracle;
end;

{**
  Gets a reference to Oracle session handle.
  @return a reference to Oracle session handle.
}
function TZOracleConnection.GetSessionHandle: POCISession;
begin
  Result := FSessionHandle;
end;

{**
  Gets a reference to Oracle transaction handle.
  @return a reference to Oracle transacton handle.
}
function TZOracleConnection.GetTransactionHandle: POCITrans;
begin
  Result := fAttachedTransaction.GetTrHandle;
end;

{**
  Gets a reference to Oracle describe handle.
  @return a reference to Oracle describe handle.
}
function TZOracleConnection.GetDescribeHandle: POCIDescribe;
begin
  Result := FDescibeHandle;
end;

function TZOracleConnection.GetClientVersion: Integer;
var
  major_version, minor_version, update_num,
      patch_num, port_update_num: sword;
begin
  FPlainDriver.OCIClientVersion(@major_version, @minor_version, @update_num,
      @patch_num, @port_update_num);
  Result := EncodeSQLVersioning(major_version,minor_version,update_num);
end;

function TZOracleConnection.GetHostVersion: Integer;
var
  buf:text;
  version:ub4;
begin
  result:=0;
  getmem(buf,1024);
  if FPlainDriver.OCIServerRelease(FServerHandle,FErrorHandle,buf,1024,OCI_HTYPE_SERVER,@version)=OCI_SUCCESS then
    Result := EncodeSQLVersioning((version shr 24) and $ff,(version shr 20) and $f,(version shr 12) and $ff);
  freemem(buf);
end;

function TZOracleConnection.GetPlainDriver: TZOraclePlainDriver;
begin
  Result := FPlainDriver;
end;

function TZOracleConnection.GetBinaryEscapeString(const Value: TBytes): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Value);
  SetLength(Result, L*2+2);
  P := PChar(Result);
  P^ := #39;
  Inc(p);
  ZBinToHex(PAnsiChar(Value), P, L);
  (P+L)^ := #39;
end;

function TZOracleConnection.GetBinaryEscapeString(const Value: RawByteString): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Value);
  SetLength(Result, L*2+2);
  P := PChar(Result);
  P^ := #39;
  Inc(p);
  ZBinToHex(PAnsiChar(Value), P, L);
  (P+L)^ := #39;
end;

{ TZOracleCachedResolver }

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZOracleCachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): string;
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(RowAccessor, ColumnsLookup);
  if Result <> '' then
  begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0 then
    begin
      Result := copy(Result, 1, iPos+3) + ' DUAL';
    end
    else
    begin
      Result := Result + ' FROM DUAL';
    end;
  end;
end;

{ TZOracleTransaction }

procedure TZOracleTransaction.BeforeDestruction;
var Status: sword;
begin
  inherited;
  if FOCITrans <> nil then begin
    try
      fSavepoints.Clear;
      if FStarted then
        RollBack;
      Status := FOwner.FPlainDriver.OCIHandleFree(FOCITrans, OCI_HTYPE_TRANS);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FOwner.FPlainDriver, FOwner.FErrorHandle, Status,
          lcTransaction, 'OCIHandleFree', ConSettings);
    finally
      FOCITrans := nil;
    end;
  end;
  fSavepoints.Free;
end;

procedure TZOracleTransaction.CloseTransaction;
begin
end;

procedure TZOracleTransaction.Commit;
var Status: sword;
begin
  if fSavepoints.Count > 0
  then fSavepoints.Delete(fSavepoints.Count-1)
  else if not FStarted then
    raise EZSQLException.Create(SCannotUseCommit)
  else try
    Status := FOwner.FPlainDriver.OCITransCommit(FOwner.FContextHandle,
      FOwner.FErrorHandle, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FOwner.FPlainDriver, FOwner.FErrorHandle, Status,
        lcTransaction, 'TRANSACTION COMMIT', ConSettings);
  finally
    FStarted := False;
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION COMMIT');
  end;
end;

constructor TZOracleTransaction.CreateGlobal(const Owner: TZOracleConnection;
  TxnMode: TZOCITxnMode; SpawnMode: TZOCITxnSpawnMode;
  CoupleMode: TZOCITxnCoupleMode);
begin
  inherited Create;
  FTxnMode := TxnMode;
  FSpawnMode := SpawnMode;
  FCoupleMode := CoupleMode;
  fDoLog := True;
  fBranches := TZCollection.Create;
  FOwner.FPlainDriver.OCIHandleAlloc(FOwner.FOCIEnv, FOCITrans, OCI_HTYPE_TRANS, 0, nil);
end;

constructor TZOracleTransaction.CreateLocal(const Owner: TZOracleConnection);
begin
  inherited Create;
  { alloc transaction handle }
  FOwner := Owner;
  FOwner.FPlainDriver.OCIHandleAlloc(FOwner.FOCIEnv, FOCITrans, OCI_HTYPE_TRANS, 0, nil);
  fSavepoints := TStringList.Create;
  ConSettings := Owner.ConSettings;
  fLocal := True;
end;

function TZOracleTransaction.GetTrHandle: POCITrans;
begin
  Result := FOCITrans
end;

function TZOracleTransaction.IsStarted: Boolean;
begin
  Result := FStarted;
end;

procedure TZOracleTransaction.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin

end;

procedure TZOracleTransaction.Rollback;
var Status: sword;
begin
  if fSavepoints.Count > 0 then begin
    FOwner.ExecuteImmediat('ROLLBACK TO '+fSavepoints[fSavepoints.Count-1], lcTransaction);
    fSavepoints.Delete(fSavepoints.Count -1);
  end else if not FStarted then
    raise EZSQLException.Create(SCannotUseRollback)
  else try
    Status := FOwner.FPlainDriver.OCITransRollback(FOwner.FContextHandle,
      FOwner.FErrorHandle, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FOwner.FPlainDriver, FOwner.FErrorHandle, Status,
        lcTransaction, 'TRANSACTION ROLLBACK', ConSettings);
  finally
    FStarted := False;
    if fDoLog and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, 'TRANSACTION ROLLBACK');
  end;
end;

const
  OCITransStartFlagsLog: Array[TZOCITxnMode] of String = (
    'SET TRANSACTION ISOLATION LEVEL DEFAULT',
    'SET TRANSACTION ISOLATION LEVEL READONLY',
    'SET TRANSACTION ISOLATION LEVEL READWRITE',
    'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE');

  OCITransStartTILFlags: Array[TZOCITxnMode] of ub4 = (
    OCI_DEFAULT, OCI_TRANS_READONLY, OCI_TRANS_READWRITE, OCI_TRANS_SERIALIZABLE);
  OCITransStartCoupleFlags: Array[TZOCITxnCoupleMode] of ub4 = (
    OCI_TRANS_TIGHT, OCI_TRANS_LOOSE);
  OCITransStartSpawnFlags: Array[TZOCITxnSpawnMode] of ub4 = (
    OCI_TRANS_NEW, OCI_TRANS_JOIN, OCI_TRANS_RESUME);

function TZOracleTransaction.StartTransaction: Integer;
var
  Status: Integer;
  TxnFlags: ub4;
  S: String;
begin
  if FStarted then
    if FOwner.AutoCommit
    then Result := 1
    else begin
      //just numbered names do disturb the oracle .. you'll get invalid statement error
      S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
      FOwner.ExecuteImmediat('SAVEPOINT '+S, lcTransaction);
      Result := FSavePoints.Add(S)+2;
    end
  else begin
    if FLocal then
      FTxnMode := ZDbc2OCITxnMode(FOwner.ReadOnly, FOwner.TransactIsolationLevel);
    TxnFlags := OCITransStartTILFlags[FTxnMode];
    if not FLocal then begin
      TxnFlags := TxnFlags or OCITransStartCoupleFlags[FCoupleMode];
      TxnFlags := TxnFlags or OCITransStartSpawnFlags[FSpawnMode];
    end;
    Status := FOwner.FPlainDriver.OCITransStart(FOwner.FContextHandle,
      FOwner.FErrorHandle, 0, TxnFlags);
    CheckOracleError(FOwner.FPlainDriver, FOwner.FErrorHandle, Status, lcExecute, OCITransStartFlagsLog[FTxnMode], ConSettings);
    FStarted := True;
    if DriverManager.HasLoggingListener then
      {$IFDEF UNICODE}
      FOwner.LogW(lcConnect, OCITransStartFlagsLog[FTxnMode]);
      {$ELSE}
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, OCITransStartFlagsLog[FTxnMode]);
      {$ENDIF}
    Result := Ord(not FOwner.AutoCommit);
  end;
end;

initialization
  OracleDriver := TZOracleDriver.Create;
  DriverManager.RegisterDriver(OracleDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OracleDriver);
  OracleDriver := nil;
{$ENDIF ZEOS_DISABLE_ORACLE}
end.
