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

{$IFDEF ENABLE_OLEDB}
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  {$ifdef WITH_SYSTEM_PREFIX}System.Win.ComObj,{$else}ComObj,{$endif}
  ZDbcIntfs, ZDbcConnection, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZURL, ZCompatibility,
  ZOleDB, ZPlainOleDBDriver, ZOleDBToken;

type
  {** Implements OleDB Database Driver. }
  {$WARNINGS OFF}
  TZOleDBDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetTokenizer: IZTokenizer; override;
  end;
  {$WARNINGS ON}

  TServerProvider = (spUnkown, spMSSQL, spOracle);

  {** Defines a PostgreSQL specific connection. }
  IZOleDBConnection = interface(IZConnection)
    ['{35A72582-F758-48B8-BBF7-3267EEBC9750}']
    function GetSession: IUnknown;
    function CreateCommand: ICommandText;
    function GetMalloc: IMalloc;
    function SupportsMultipleResultSets: Boolean;
    function GetProvider: TServerProvider;
  end;

  {** Implements a generic OleDB Connection. }
  TZOleDBConnection = class(TZAbstractConnection, IZOleDBConnection)
  private
    FMalloc: IMalloc;
    FDBInitialize: IDBInitialize;
    FSession: IUnknown;
    FDBCreateCommand: IDBCreateCommand;
    FTransaction: ITransactionLocal;
    FRetaining: Boolean;
    FpulTransactionLevel: PULONG;
    FSupportsMultipleResultSets: Boolean;
    FServerProvider: TServerProvider;
    procedure StopTransaction;
  protected
    procedure StartTransaction;
    procedure InternalCreate; override;
    function OleDbGetDBPropValue(APropIDs: array of DBPROPID): string;
  public
    destructor Destroy; override;

    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    {function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;}

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure Close; override;

    {procedure SetReadOnly(ReadOnly: Boolean); override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;}
  public
    { OleDB speziffic }
    function GetSession: IUnknown;
    function CreateCommand: ICommandText;
    function GetMalloc: IMalloc;
    function SupportsMultipleResultSets: Boolean;
    function GetProvider: TServerProvider;
  end;

var
  {** The common driver manager object. }
  OleDBDriver: IZDriver;

implementation

uses ZDbcOleDBMetadata, ZDbcOleDBUtils, ZDbcOleDBStatement, ZSysUtils, ZDbcUtils,
  ZMessages, ZFastCode;

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
{$WARNINGS OFF}
function TZOleDBDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOleDBConnection.Create(Url);
end;
{$WARNINGS ON}

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZOleDBDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOleDBTokenizer.Create;
end;

threadvar
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
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOleDBConnection.Destroy;
begin
  try
    inherited Destroy; // call Disconnect;
    fMalloc := nil;
    CoUninit;
  except
  end;
end;

procedure TZOleDBConnection.StopTransaction;
begin
  if (FTransaction <> nil) and (TransactIsolationLevel <> tiNone) then
    if AutoCommit then
      OleDbCheck(fTransaction.Commit(False,XACTTC_SYNC,0))
    else
      OleDbCheck(fTransaction.Abort(nil, False, False));
end;

const
  TIL: array[TZTransactIsolationLevel] of ISOLATIONLEVEL =
   ( ISOLATIONLEVEL_CHAOS,
     ISOLATIONLEVEL_READUNCOMMITTED,
     ISOLATIONLEVEL_READCOMMITTED,
     ISOLATIONLEVEL_REPEATABLEREAD,
     ISOLATIONLEVEL_SERIALIZABLE);

procedure TZOleDBConnection.StartTransaction;
begin
  if (FTransaction <> nil) and (TransactIsolationLevel <> tiNone) then
  begin
    fTransaction.StartTransaction(TIL[TransactIsolationLevel],0,nil,FpulTransactionLevel);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'Restart Transaction support');
  end;
end;

// returns property value(-s) from Data Source Information group as string,
//where values are delimited using space
function TZOleDBConnection.OleDbGetDBPropValue(APropIDs: array of DBPROPID): string;
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
  OleDBCheck(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties) );
  try
    PropIDSet.rgPropertyIDs   := @APropIDs;
    PropIDSet.cPropertyIDs    := High(APropIDs)+1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    OleDBCheck( DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ) );
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
  if GetAutoEncodeStrings then
    Result := GetDriver.GetTokenizer.GetEscapeString(Result)
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
  if GetAutoEncodeStrings then
    Result := GetDriver.GetTokenizer.GetEscapeString(Result)
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
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, Info);
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
function TZOleDBConnection.GetSession: IUnknown;
begin
  Result := FSession;
end;

{**
  Returs the OleSession interface of current connection
}
function TZOleDBConnection.CreateCommand: ICommandText;
const
  SSPROP_DEFERPREPARE	= 13;
  DBPROPSET_SQLSERVERROWSET: TGUID 	= '{5cf4ca11-ef21-11d0-97e7-00c04fc2ad98}';
var
  FCmdProps: ICommandProperties;
  rgProperties: TDBProp;
  rgPropertySets: TDBPROPSET;
begin
  Result := nil;
  OleDbCheck(FDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(Result)));
  FCmdProps := nil; //init
  if (FServerProvider = spMSSQL) and
    Succeeded(Result.QueryInterface(IID_ICommandProperties, FCmdProps)) then
  begin
    //http://msdn.microsoft.com/de-de/library/ms130779.aspx
    rgPropertySets.rgProperties := @rgProperties;
    // initialize common property options
    rgProperties.dwOptions := DBPROPOPTIONS_REQUIRED;
    rgProperties.colid     := DB_NULLID;
    //VariantInit(rgProperties.vValue);
    // turn off deferred prepare -> raise exception now if command can't be executed!
    rgProperties.dwPropertyID := SSPROP_DEFERPREPARE;
    rgProperties.vValue       := False;

    rgPropertySets.guidPropertySet := DBPROPSET_SQLSERVERROWSET;
    rgPropertySets.cProperties := 1;
    try
      OleDBCheck(FCmdProps.SetProperties(1,rgPropertySets));
    finally
      FCmdProps := nil;
    end;
  end;
end;

function TZOleDBConnection.GetMalloc: IMalloc;
begin
  Result := FMalloc;
end;

function TZOleDBConnection.SupportsMultipleResultSets: Boolean;
begin
  Result := FSupportsMultipleResultSets;
end;

function TZOleDBConnection.GetProvider: TServerProvider;
begin
  Result := FServerProvider;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZOleDBConnection.SetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  if not Closed and (TransactIsolationLevel <> Level) then
    StopTransaction;
  TransactIsolationLevel := Level;
  StartTransaction;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOleDBConnection.Commit;
begin
  if assigned(fTransaction) and (TransactIsolationLevel <> tiNone) then
  begin
    OleDbCheck(fTransaction.Commit(FRetaining,XACTTC_SYNC,0));
    if not FRetaining then StartTransaction;
  end;
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'COMMIT');
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
  if assigned(fTransaction) and (TransactIsolationLevel <> tiNone) then
  begin
    OleDbCheck(fTransaction.Abort(nil, FRetaining, False));
    if not FRetaining then StartTransaction;
  end;
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'ROLLBACK');
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZOleDBConnection.Open;
var
  DataInitialize : IDataInitialize;
  ConnectStrings: TStrings;
  ConnectString: ZWideString;
  Tmp: String;
  FDBCreateSession: IDBCreateSession;
begin
  if not Closed then
    Exit;
  try
    // retrieve initialization parameters from connection string
    DataInitialize := CreateComObject(CLSID_DataLinks) as IDataInitialize;
    ConnectStrings := SplitString(DataBase, ';');
    if StrToBoolEx(ConnectStrings.Values['Trusted_Connection']) then
      ConnectString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(DataBase)
    else
    begin
      ConnectStrings.Values['User Id'] := User;
      ConnectStrings.Values['Password'] := PassWord;
      ConnectString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(ComposeString(ConnectStrings, ';'));
    end;
    ConnectStrings.Free;
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      Pointer(ConnectString), IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil; //no longer required!

    // open the connection to the DB
    OleDBCheck(fDBInitialize.Initialize);
    OleCheck(fDBInitialize.QueryInterface(IID_IDBCreateSession, FDBCreateSession));
    OleDBCheck(FDBCreateSession.CreateSession(nil, IID_IOpenRowset, FSession));
    FDBCreateSession := nil; //no longer required!
    //some Providers do NOT support commands, so let's check if we can use it
    OleDBCheck(FSession.QueryInterface(IID_IDBCreateCommand, FDBCreateCommand));
    // Now let's find out what current server supports:
    // Is IMultipleResults supported??
    FSupportsMultipleResultSets := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(OleDbGetDBPropValue([DBPROP_MULTIPLERESULTS]), 0 ) <> 0;
    // Now let's find which Server is used for optimal stms/parameters etc.
    Tmp := OleDbGetDBPropValue([DBPROP_PROVIDERFRIENDLYNAME]);
    { exact name leading to pain -> scan KeyWords instead! }
    if ZFastCode.Pos('Oracle', Tmp) > 0 then
      FServerProvider := spOracle
    else if (ZFastCode.Pos('Microsoft', Tmp) > 0 ) and (ZFastCode.Pos('SQL Server', Tmp) > 0 ) then
      FServerProvider := spMSSQL;

    // check if DB handle transactions
    if Failed(FSession.QueryInterface(IID_ITransactionLocal,fTransaction)) then
      fTransaction := nil;
    inherited Open;
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
      'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"');
    StartTransaction;
  except
    on E: Exception do
    begin
      FDBCreateSession := nil; // mark not connected
      FDBCreateCommand := nil; // mark not connected
      FSession := nil;
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
procedure TZOleDBConnection.Close;
begin
  if not Closed then
  begin
    StopTransaction;
    fTransaction := nil;
    FSession := nil;
    FDBCreateCommand := nil;
    (FMetadata as TOleDBDatabaseMetadata).ReleaseDBSchemaRowSet; //flush IDBSchemaRowSet
    OleDBCheck(fDBInitialize.Uninitialize);
    fDBInitialize := nil;
    DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol,
      'DISCONNECT FROM "'+ConSettings^.Database+'"');
    inherited Close;
  end;
end;

initialization
  OleDBDriver := TZOleDBDriver.Create;
  DriverManager.RegisterDriver(OleDBDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OleDBDriver);
  OleDBDriver := nil;

{$ELSE !ENABLE_OLEDB}
implementation
{$ENDIF ENABLE_OLEDB}

end.
