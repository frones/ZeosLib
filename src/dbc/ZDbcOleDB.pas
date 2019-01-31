{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcOleDB;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZDbcIntfs, ZDbcConnection, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZURL, ZCompatibility, ZDbcOleDBUtils,
  ZOleDB, ZPlainOleDBDriver, ZOleDBToken;

type
  {** Implements OleDB Database Driver. }
  TZOleDBDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetTokenizer: IZTokenizer; override;
  end;

  {** Defines a PostgreSQL specific connection. }
  IZOleDBConnection = interface(IZConnection)
    ['{35A72582-F758-48B8-BBF7-3267EEBC9750}']
    function GetSession: IUnknown;
    function CreateCommand: ICommandText;
    function GetMalloc: IMalloc;
    function SupportsMARSConnection: Boolean;
  end;

  TOleCheckAction = (ocaOther, ocaStartTransaction, ocaCommit, ocaRollback);
  {** Implements a generic OleDB Connection. }
  TZOleDBConnection = class(TZAbstractDbcConnection, IZOleDBConnection)
  private
    FMalloc: IMalloc;
    FDBInitialize: IDBInitialize;
    FDBCreateCommand: IDBCreateCommand;
    FRetaining: Boolean;
    FpulTransactionLevel: ULONG;
    FSupportsMARSConnnection: Boolean;
    FServerProvider: TZServerProvider;
    fTransaction: ITransactionLocal;
    fCatalog: String;
    procedure StopTransaction;
    procedure SetProviderProps(DBinit: Boolean);
    procedure CheckError(Status: HResult; Action: TOleCheckAction; DoLog: Boolean);
  protected
    procedure InternalCreate; override;
    function OleDbGetDBPropValue(const APropIDs: array of DBPROPID): string; overload;
    function OleDbGetDBPropValue(APropID: DBPROPID): Integer; overload;
  public
    destructor Destroy; override;

    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure StartTransaction;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;

    {procedure SetReadOnly(ReadOnly: Boolean); override; }

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    {function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;}

    function GetServerProvider: TZServerProvider; override;
  public
    { OleDB speziffic }
    function GetSession: IUnknown;
    function CreateCommand: ICommandText;
    function GetMalloc: IMalloc;
    function SupportsMARSConnection: Boolean;
  end;

var
  {** The common driver manager object. }
  OleDBDriver: IZDriver;
const
  ROleCheckActions: array[TOleCheckAction] of RawByteString = ('',
    'Start Transaction', 'Commit Transaction', 'Rollback Transaction');
  {$IFDEF UNICODE}
  UOleCheckActions: array[TOleCheckAction] of UnicodeString = ('',
    'Start Transaction', 'Commit Transaction', 'Rollback Transaction');
  {$ENDIF}

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses ZDbcOleDBMetadata, ZDbcOleDBStatement, ZSysUtils, ZDbcUtils,
  ZMessages, ZFastCode, ZConnProperties, ZDbcProperties;

{ TZOleDBDriver }

{**
  Constructs this object with default properties.
}
constructor TZOleDBDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZOleDBPlainDriver.Create));
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
function TZOleDBDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOleDBConnection.Create(Url);
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZOleDBDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOleDBTokenizer.Create;
end;

var
  OleDBCoinitialized: integer;

procedure CoInit;
begin
  inc(OleDBCoInitialized);
  if OleDBCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(OleDBCoinitialized>0);
  dec(OleDBCoinitialized);
  if OleDBCoinitialized=0 then
    CoUninitialize;
end;

{ TZOleDBConnection }
procedure TZOleDBConnection.InternalCreate;
begin
  CoInit;
  OleCheck(CoGetMalloc(1,fMalloc));
  FMetadata := TOleDBDatabaseMetadata.Create(Self, URL);
  FRetaining := False; //not StrToBoolEx(URL.Properties.Values['hard_commit']);
  CheckCharEncoding('CP_UTF16');
  fTransaction := nil;
  Inherited SetAutoCommit(True);
  //Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOleDBConnection.Destroy;
begin
  try
    inherited Destroy; // call Disconnect;
  finally
    FDBCreateCommand := nil;
    fDBInitialize := nil;
    fMalloc := nil;
    CoUninit;
  end;
end;

procedure TZOleDBConnection.StopTransaction;
begin
  if Assigned(fTransaction) then begin
    CheckError(fTransaction.Abort(nil, False, False), ocaRollback, True);
    fTransaction := nil;
    Dec(FpulTransactionLevel, Ord(FpulTransactionLevel > 0));
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
procedure TZOleDBConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    StopTransaction;
    inherited SetAutoCommit(Value);
    StartTransaction;
  end;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZOleDBConnection.SetCatalog(const Catalog: string);
begin
  if Catalog <> '' then
    if GetServerProvider in [spSybase,spMSSQL] then begin
      CreateRegularStatement(info).ExecuteUpdate('use '+Catalog);
      fCatalog := Catalog;
    end;
end;

procedure TZOleDBConnection.SetProviderProps(DBinit: Boolean);
const
  DBPROPSET_SQLSERVERDBINIT:      TGUID = '{5cf4ca10-ef21-11d0-97e7-00c04fc2ad98}';
  //{%H-}DBPROPSET_SQLSERVERDATASOURCE:  TGUID = '{28efaee4-2d2c-11d1-9807-00c04fc2ad98}'; unused
  SSPROP_INIT_PACKETSIZE	       = 9;
var
  DBProps: IDBProperties;
  rgDBPROPSET: array[0..10] of TDBProp;
  rgDBPROPSET_SQLSERVERDBINIT: TDBProp;
  rgDBPROPSET_DATASOURCE: TDBProp;
  PropertySets: array[0..2] of TDBPROPSET;
  cPropertySets: ULONG;
  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
begin
//some examples: https://blogs.msdn.microsoft.com/sqlnativeclient/2009/05/06/sql-server-native-client-connection-strings-and-ole-db/
  DBProps := nil; //init
  if Succeeded(FDBInitialize.QueryInterface(IID_IDBProperties, DBProps)) then
  begin
    if DBinit then
    begin
      cPropertySets := 2;
      PropertySets[0].cProperties     := 0; //init
      PropertySets[0].guidPropertySet := DBPROPSET_DBINIT;
      PropertySets[0].rgProperties    := @rgDBPROPSET[0];
      PropertySets[1].cProperties     := 0; //init
      PropertySets[1].guidPropertySet := DBPROPSET_DATASOURCE;
      PropertySets[1].rgProperties    := @rgDBPROPSET_DATASOURCE;
      PropertySets[2].cProperties     := 0; //init
      PropertySets[2].guidPropertySet := DBPROPSET_SQLSERVERDBINIT;
      PropertySets[2].rgProperties    := @rgDBPROPSET_SQLSERVERDBINIT;
      //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
      //Indicates the number of seconds before the source initialization times out
      SetProp(PropertySets[0], DBPROP_INIT_TIMEOUT,       StrToIntDef(Info.Values[ConnProps_Timeout], 0));
      //Indicates the number of seconds before a request and command execution, times out
      SetProp(PropertySets[0], DBPROP_INIT_GENERALTIMEOUT,StrToIntDef(Info.Values[ConnProps_Timeout], 0));
      //Force Multiple connections -> prevent transactional issues with IDBSchemaRowSet etc
      //http://support2.microsoft.com/default.aspx?scid=kb;en-us;272358
      SetProp(PropertySets[1], DBPROP_MULTIPLECONNECTIONS,VARIANT_TRUE);
      //supported for MSSQL only!!!
      if (Info.Values[ConnProps_TDSPacketSize] <> '') then
      begin
        SetProp(PropertySets[2], SSPROP_INIT_PACKETSIZE, StrToIntDef(Info.Values[ConnProps_TDSPacketSize], 0));
        cPropertySets := 3;
      end;
    end
    else
      // don't work? Bad sequence when to call?
      if (FServerProvider = spMSSQL) then
      begin
        PropertySets[0].cProperties     := 0; //init
        PropertySets[0].guidPropertySet := DBPROPSET_DATASOURCE;
        PropertySets[0].rgProperties    := @rgDBPROPSET[0];
        SetProp(PropertySets[0], DBPROP_MULTIPLECONNECTIONS,VARIANT_FALSE);
        cPropertySets := 1;
      end
      else
        cPropertySets := 0;
    try
      CheckError(DBProps.SetProperties(cPropertySets,@PropertySets[0]), ocaOther, False);
    finally
      DBProps := nil;
    end;
  end;
end;

const
  TIL: array[TZTransactIsolationLevel] of ISOLATIONLEVEL =
   ( ISOLATIONLEVEL_CHAOS,
     ISOLATIONLEVEL_READUNCOMMITTED,
     ISOLATIONLEVEL_READCOMMITTED,
     ISOLATIONLEVEL_REPEATABLEREAD,
     ISOLATIONLEVEL_SERIALIZABLE);

procedure TZOleDBConnection.StartTransaction;
var
  rgDBPROPSET_DBPROPSET_SESSION: TDBProp;
  prgPropertySets: TDBPROPSET;
  SessionProperties: ISessionProperties;
  Res: HResult;
begin
  if (not Closed) and Self.GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(TransactIsolationLevel) then
    if AutoCommit then begin
      SessionProperties := nil;
      if (FDBCreateCommand.QueryInterface(IID_ISessionProperties, SessionProperties) = S_OK) then begin
        prgPropertySets.cProperties     := 1;
        prgPropertySets.guidPropertySet := DBPROPSET_SESSION;
        prgPropertySets.rgProperties    := @rgDBPROPSET_DBPROPSET_SESSION;
        rgDBPROPSET_DBPROPSET_SESSION.dwPropertyID := DBPROP_SESS_AUTOCOMMITISOLEVELS;
        rgDBPROPSET_DBPROPSET_SESSION.dwOptions    := DBPROPOPTIONS_REQUIRED;
        rgDBPROPSET_DBPROPSET_SESSION.colid        := DB_NULLID;
        rgDBPROPSET_DBPROPSET_SESSION.vValue       := TIL[TransactIsolationLevel];
        CheckError(SessionProperties.SetProperties(1, @prgPropertySets), ocaOther, False);
      end;
    end else if not Assigned(fTransaction) and
       Succeeded(FDBCreateCommand.QueryInterface(IID_ITransactionLocal,fTransaction)) then begin
      Res := fTransaction.StartTransaction(TIL[TransactIsolationLevel],0,nil,@FpulTransactionLevel);
      if not Succeeded(Res) then begin
        Dec(FpulTransactionLevel);
        fTransaction := nil;
      end;
      CheckError(Res, ocaStartTransaction, True);
    end;
end;

// returns property value(-s) from Data Source Information group as string,
//where values are delimited using space
function TZOleDBConnection.OleDbGetDBPropValue(const APropIDs: array of DBPROPID): string;
var
  DBProperties: IDBProperties;
  PropIDSet: TDBPROPIDSET;
  prgPropertySets: PDBPropSet;
  PropSet: TDBPropSet;
  nPropertySets: ULONG;
  i: Integer;
  s: string;
begin
  Result := '';
  DBProperties := nil;
  CheckError(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties), ocaOther, False);
  try
    PropIDSet.rgPropertyIDs   := @APropIDs;
    PropIDSet.cPropertyIDs    := High(APropIDs)+1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    CheckError(DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ), ocaOther, False);
    Assert( nPropertySets = 1 );
    PropSet := prgPropertySets^;
    for i := 0 to PropSet.cProperties-1 do begin
      if PropSet.rgProperties^[i].dwStatus <> DBPROPSTATUS(DBPROPSTATUS_OK) then
        Continue;
      if Result <> '' then
        if PropSet.rgProperties^[i].dwPropertyID = DBPROP_DBMSVER then
          Result := Result + ' Release '
        else
          Result := Result + ' ';
      s := PropSet.rgProperties^[i].vValue;
      Result := Result + s;
    end;
    // free and clear elements of PropIDSet
    for i := 0 to PropSet.cProperties-1 do
      VariantClear(PropSet.rgProperties^[i].vValue);
    FMAlloc.Free(PropSet.rgProperties);
    FMAlloc.Free(prgPropertySets); //free prgPropertySets
  finally
    DBProperties := nil;
  end;
end;

{**
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to pre-detect it Result = BinaryString
  @param Value represents the Byte-Array
  @result the detectable Binary String
}
function TZOleDBConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  Result := GetSQLHexString(Pointer(Value), Length(Value), True);
end;

{**
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to pre-detect it Result = BinaryString
  @param Value represents the Binary-String
  @result the detectable Binary String
}
function TZOleDBConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  Result := GetSQLHexString(Pointer(Value), Length(Value), True);
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZOleDBConnection.GetCatalog: string;
begin
  Result := fCatalog;
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
function TZOleDBConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  Result := CreatePreparedStatement('', Info);
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
function TZOleDBConnection.CreatePreparedStatement(const SQL: string; Info: TStrings):
  IZPreparedStatement;
begin
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, SQL, Info);
end;

{**
  Returs the OleSession interface of current connection
}
function TZOleDBConnection.GetServerProvider: TZServerProvider;
begin
  Result := spMSSQL;
end;

function TZOleDBConnection.GetSession: IUnknown;
begin
  Result := FDBCreateCommand;
end;

{**
  Returs the Ole-ICommandText interface of current connection
}
function TZOleDBConnection.CreateCallableStatement(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  if (GetServerProvider = spMSSQL)
  then Result := TZOleDBCallableStatementMSSQL.Create(Self, SQL, Info)
  else Result := inherited CreateCallableStatement(SQL, Info);
end;

function TZOleDBConnection.CreateCommand: ICommandText;
begin
  Result := nil;
  CheckError(FDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(Result)), ocaOther, False);
end;

function TZOleDBConnection.GetMalloc: IMalloc;
begin
  Result := FMalloc;
end;

function TZOleDBConnection.SupportsMARSConnection: Boolean;
begin
  Result := FSupportsMARSConnnection;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZOleDBConnection.SetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  if (TransactIsolationLevel <> Level) and GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(Level) then begin
    StopTransaction;
    inherited SetTransactionIsolation(Level);
    StartTransaction;
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOleDBConnection.CheckError(Status: HResult; Action: TOleCheckAction;
  DoLog: Boolean);
begin
  if DoLog and (Action <> ocaOther) and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, ROleCheckActions[Action]);
  if Status <> S_OK then
    OleDbCheck(Status, {$IFDEF UNICODE}UOleCheckActions{$ELSE}ROleCheckActions{$ENDIF}[Action], Self, nil);
end;

procedure TZOleDBConnection.Commit;
begin
  if (FpulTransactionLevel > 0) and assigned(fTransaction) then begin
    CheckError(fTransaction.Commit(FRetaining,XACTTC_SYNC,0), ocaCommit, True);
    if not FRetaining then begin
      fTransaction := nil;
      Dec(FpulTransactionLevel);
      StartTransaction;
    end;
  end;
end;

procedure TZOleDBConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  FpulTransactionLevel := 0;
  fTransaction := nil;
  FMalloc := nil;
  FDBInitialize := nil;
  FDBCreateCommand := nil;
  inherited ReleaseImmediat(Sender);
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZOleDBConnection.Rollback;
begin
  if (FpulTransactionLevel > 0) and assigned(fTransaction) then begin
    CheckError(fTransaction.Abort(nil, FRetaining, False), ocaRollback, True);
    if not FRetaining then
    begin
      fTransaction := nil;
      Dec(FpulTransactionLevel);
      StartTransaction;
    end;
  end;
end;

function TZOleDBConnection.OleDbGetDBPropValue(APropID: DBPROPID): Integer;
var
  DBProperties: IDBProperties;
  PropIDSet: TDBPROPIDSET;
  prgPropertySets: PDBPropSet;
  PropSet: TDBPropSet;
  nPropertySets: ULONG;
  i: Integer;
begin
  Result := 0;
  DBProperties := nil;
  CheckError(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties), ocaOther, False);
  try
    PropIDSet.rgPropertyIDs   := @APropID;
    PropIDSet.cPropertyIDs    := 1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    CheckError(DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ), ocaOther, False);
    Assert( nPropertySets = 1 );
    PropSet := prgPropertySets^;
    for i := 0 to PropSet.cProperties-1 do begin
      if PropSet.rgProperties^[i].dwStatus <> DBPROPSTATUS(DBPROPSTATUS_OK) then
        Continue;
      Result := PropSet.rgProperties^[i].vValue;
    end;
    // free and clear elements of PropIDSet
    for i := 0 to PropSet.cProperties-1 do
      VariantClear(PropSet.rgProperties^[i].vValue);
    FMAlloc.Free(PropSet.rgProperties);
    FMAlloc.Free(prgPropertySets); //free prgPropertySets
  finally
    DBProperties := nil;
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZOleDBConnection.Open;
var
  DataInitialize : IDataInitialize;
  ConnectStrings: TStrings;
  ConnectString: ZWideString;
  FDBCreateSession: IDBCreateSession;
begin
  if not Closed then
    Exit;
  try
    // retrieve initialization parameters from connection string
    DataInitialize := CreateComObject(CLSID_DataLinks) as IDataInitialize;
    ConnectStrings := SplitString(DataBase, ';');
    //https://msdn.microsoft.com/de-de/library/ms131686%28v=sql.120%29.aspx
    FSupportsMARSConnnection := StrToBoolEx(ConnectStrings.Values[ConnProps_MarsConn]);
    if StrToBoolEx(ConnectStrings.Values[ConnProps_TrustedConnection]) then
      ConnectString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(DataBase)
    else
    begin
      ConnectStrings.Values[ConnProps_UserId] := User;
      ConnectStrings.Values[ConnProps_Password] := PassWord;
      ConnectString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(ComposeString(ConnectStrings, ';'));
    end;
    FServerProvider := ProviderNamePrefix2ServerProvider(ConnectStrings.Values[ConnProps_Provider]);
    fCatalog := ConnectStrings.Values[ConnProps_Initial_Catalog];
    ConnectStrings.Free;
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      Pointer(ConnectString), IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil; //no longer required!
    SetProviderProps(True); //set's timeout values
    if TransactIsolationLevel = tiNone then
      Inherited SetTransactionIsolation(tiReadCommitted);
    // open the connection to the DB
    CheckError(fDBInitialize.Initialize, ocaOther, False);
    OleCheck(fDBInitialize.QueryInterface(IID_IDBCreateSession, FDBCreateSession));
    //some Providers do NOT support commands, so let's check if we can use it
    OleCheck(FDBCreateSession.CreateSession(nil, IID_IDBCreateCommand, IUnknown(FDBCreateCommand)));
    FDBCreateSession := nil; //no longer required!
    //if FServerProvider = spMSSQL then
      //SetProviderProps(False); //provider properties -> don't work??
    inherited Open;
    (GetMetadata.GetDatabaseInfo as IZOleDBDatabaseInfo).InitilizePropertiesFromDBInfo(fDBInitialize, fMalloc);
    if (GetServerProvider = spMSSQL) and ((Info.Values[ConnProps_DateWriteFormat] = '') or (Info.Values[ConnProps_DateTimeWriteFormat] = '')) then begin
      if (Info.Values[ConnProps_DateWriteFormat] = '') then begin
        ConSettings^.WriteFormatSettings.DateFormat := 'YYYYMMDD';  //ISO format which always is accepted by SQLServer
        ConSettings^.WriteFormatSettings.DateFormatLen := 8;
      end;
      if (Info.Values[ConnProps_DateTimeWriteFormat] = '') then begin
        ConSettings^.WriteFormatSettings.DateTimeFormat := 'YYYY-MM-DDTHH:NN:SS'; //ISO format which always is accepted by SQLServer
        ConSettings^.WriteFormatSettings.DateTimeFormatLen := 19;
      end;
    end;
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
      'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"');
    StartTransaction;
  except
    on E: Exception do
    begin
      FDBCreateSession := nil; // mark not connected
      FDBCreateCommand := nil; // mark not connected
      fDBInitialize := nil;
      DataInitialize := nil;
      raise;
    end;
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
procedure TZOleDBConnection.InternalClose;
begin
  if Closed or not Assigned(fDBInitialize) then
    Exit;

  StopTransaction;
  FDBCreateCommand := nil;
  CheckError(fDBInitialize.Uninitialize, ocaOther, False);
  fDBInitialize := nil;
  DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol,
    'DISCONNECT FROM "'+ConSettings^.Database+'"');
end;

initialization
  OleDBDriver := TZOleDBDriver.Create;
  DriverManager.RegisterDriver(OleDBDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OleDBDriver);
  OleDBDriver := nil;
{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
