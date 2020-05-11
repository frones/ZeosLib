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
  ZGenericSqlAnalyser, ZCompatibility, ZDbcOleDBUtils,
  ZPlainOleDBDriver, ZOleDBToken;

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

  {** Implements a generic OleDB Connection. }
  TZOleDBConnection = class(TZAbstractDbcConnection, IZConnection,
    IZOleDBConnection)
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
    FSavePoints: TStrings;
    FAutoCommitTIL: ISOLATIONLEVEL;
    FRestartTransaction: Boolean;
    procedure SetProviderProps(DBinit: Boolean);
    procedure CheckError(Status: HResult; LoggingCateGory: TZLoggingCategory;
      const LogMsg: RawByteString);
  protected
    procedure InternalCreate; override;
    function OleDbGetDBPropValue(const APropIDs: array of DBPROPID): string; overload;
    function OleDbGetDBPropValue(APropID: DBPROPID): Integer; overload;
    procedure InternalSetTIL(Level: TZTransactIsolationLevel);
    procedure ExecuteImmediat(const SQL: UnicodeString; LoggingCategory: TZLoggingCategory); overload; override;
    procedure InternalClose; override;
  public
    destructor Destroy; override;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;

    procedure Open; override;
    procedure Commit;
    procedure Rollback;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function StartTransaction: Integer;

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;

    {procedure SetReadOnly(ReadOnly: Boolean); override; }

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    {function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;}

    function GetServerProvider: TZServerProvider; override;
  public { IZOleDBConnection }
    function GetSession: IUnknown;
    function CreateCommand: ICommandText;
    function GetMalloc: IMalloc;
    function SupportsMARSConnection: Boolean;
  end;

var
  {** The common driver manager object. }
  OleDBDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses ZDbcOleDBMetadata, ZDbcOleDBStatement, ZSysUtils, ZDbcUtils, ZEncoding,
  ZMessages, ZFastCode, ZDbcProperties;

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
  FSavePoints := TStringList.Create;
  Inherited SetAutoCommit(True);
  //Open;
end;

const
  TIL: array[TZTransactIsolationLevel] of ISOLATIONLEVEL =
   ( ISOLATIONLEVEL_CHAOS,
     ISOLATIONLEVEL_READUNCOMMITTED,
     ISOLATIONLEVEL_READCOMMITTED,
     ISOLATIONLEVEL_REPEATABLEREAD,
     ISOLATIONLEVEL_SERIALIZABLE);

procedure TZOleDBConnection.InternalSetTIL(Level: TZTransactIsolationLevel);
var
  rgDBPROPSET_DBPROPSET_SESSION: TDBProp;
  prgPropertySets: TDBPROPSET;
  SessionProperties: ISessionProperties;
begin
  SessionProperties := nil;
  if (FDBCreateCommand.QueryInterface(IID_ISessionProperties, SessionProperties) = S_OK) then begin
    prgPropertySets.cProperties     := 1;
    prgPropertySets.guidPropertySet := DBPROPSET_SESSION;
    prgPropertySets.rgProperties    := @rgDBPROPSET_DBPROPSET_SESSION;
    rgDBPROPSET_DBPROPSET_SESSION.dwPropertyID := DBPROP_SESS_AUTOCOMMITISOLEVELS;
    rgDBPROPSET_DBPROPSET_SESSION.dwOptions    := DBPROPOPTIONS_REQUIRED;
    rgDBPROPSET_DBPROPSET_SESSION.colid        := DB_NULLID;
    rgDBPROPSET_DBPROPSET_SESSION.vValue       := TIL[Level];
    CheckError(SessionProperties.SetProperties(1, @prgPropertySets), lcOther, EmptyRaw);
    FAutoCommitTIL := TIL[Level];
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOleDBConnection.Destroy;
begin
  try
    inherited Destroy; // call Disconnect;
  finally
    FreeAndNil(FSavePoints);
    FDBCreateCommand := nil;
    fDBInitialize := nil;
    fMalloc := nil;
    CoUninit;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "pParams" does not seem to be initialized} {$ENDIF}
procedure TZOleDBConnection.ExecuteImmediat(const SQL: UnicodeString;
  LoggingCategory: TZLoggingCategory);
var Cmd: ICommandText;
  pParams: TDBPARAMS;
  Status: HResult;
  procedure DoLog;
  begin
    DriverManager.LogMessage(LoggingCategory, ConSettings.Protocol, ZUnicodeToRaw(SQL, ConSettings.ClientCodePage^.CP));
  end;
  procedure CheckError;
  begin
    OleDbCheck(Status, {$IFNDEF UNICODE}ZUnicodeToRaw(SQL, ConSettings.CTRL_CP){$ELSE}SQL{$ENDIF}, Self, nil);
  end;
begin
  Cmd := CreateCommand;
  FillChar(pParams, SizeOf(TDBParams), #0);
  Status := Cmd.SetCommandText(DBGUID_DEFAULT, Pointer(SQL));
  if Status <> S_OK then
    CheckError;
  Status := Cmd.Execute(nil, DB_NULLGUID,pParams,nil,nil);
  if Status <> S_OK then
    CheckError;
  if DriverManager.HasLoggingListener then
    DoLog;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      while FpulTransactionLevel > 0 do begin
        CheckError(fTransaction.Abort(nil, FRetaining, False), lcTransaction, 'Rollback Transaction');
        Dec(FpulTransactionLevel);
      end;
      fTransaction := nil;
      if FAutoCommitTIL <> TIL[TransactIsolationLevel] then
        InternalSetTIL(TransactIsolationLevel);
      AutoCommit := True;
    end else
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
    if GetServerProvider in [spASE,spMSSQL, spMySQL] then begin
      CreateStatementWithParams(info).ExecuteUpdate('use '+Catalog);
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
      CheckError(DBProps.SetProperties(cPropertySets,@PropertySets[0]), lcTransaction, EmptyRaw);
    finally
      DBProps := nil;
    end;
  end;
end;

function TZOleDBConnection.StartTransaction: Integer;
var Res: HResult;
  S: String;
begin
  if Closed then
    Open;
  AutoCommit := False;
  if FpulTransactionLevel = 0 then begin
    if not Assigned(fTransaction) then
      OleCheck(FDBCreateCommand.QueryInterface(IID_ITransactionLocal,fTransaction));
    Res := fTransaction.StartTransaction(TIL[TransactIsolationLevel],0,nil,@FpulTransactionLevel);
    CheckError(Res, lcTransaction, 'Start Transaction');
    Result := FpulTransactionLevel;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    if cSavePointSyntaxW[fServerProvider][spqtSavePoint] = '' then
      raise EZSQLException.Create(SUnsupportedOperation);
    ExecuteImmediat(cSavePointSyntaxW[fServerProvider][spqtSavePoint]+ {$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S)+2;
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
  CheckError(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties), lcOther, EmptyRaw);
  try
    PropIDSet.rgPropertyIDs   := @APropIDs;
    PropIDSet.cPropertyIDs    := High(APropIDs)+1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    CheckError(DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ), lcOther, EmptyRaw);
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
function TZOleDBConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, '', Info);
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
function TZOleDBConnection.CreateCommand: ICommandText;
begin
  Result := nil;
  CheckError(FDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(Result)), lcOther, EmptyRaw);
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
  if (TransactIsolationLevel <> Level) then begin
    if not Closed then begin
      if not AutoCommit then
        raise EZSQLException.Create(SInvalidOpInNonAutoCommit);
      InternalSetTIL(Level);
    end;
    TransactIsolationLevel := Level;
  end;
end;

procedure TZOleDBConnection.CheckError(Status: HResult; LoggingCateGory: TZLoggingCategory;
  const LogMsg: RawByteString);
begin
  if (Pointer(LogMsg) <> nil) and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCateGory, ConSettings^.Protocol, LogMsg);
  if Status <> S_OK then
    OleDbCheck(Status, '', Self, nil);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOleDBConnection.Commit;
var S: UnicodeString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseCommit);
  if FSavePoints.Count > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtCommit];
    if S <> '' then begin
      S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    CheckError(fTransaction.Commit(FRetaining,XACTTC_SYNC,0), lcTransaction, 'Commit Transaction');
    Dec(FpulTransactionLevel);
    if (FpulTransactionLevel = 0) and not FRetaining then begin
      fTransaction := nil;
      AutoCommit := True;
      if FRestartTransaction then
        StartTransaction;
    end;
  end;
end;

procedure TZOleDBConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  FpulTransactionLevel := 0;
  fTransaction := nil;
  FMalloc := nil;
  FDBInitialize := nil;
  FDBCreateCommand := nil;
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZOleDBConnection.Rollback;
var S: UnicodeString;
begin
  if Closed then
    raise EZSQLException.Create(SConnectionIsNotOpened);
  if AutoCommit then
    raise EZSQLException.Create(SCannotUseRollback);
  if FSavePoints.Count > 0 then begin
    S := cSavePointSyntaxW[fServerProvider][spqtRollback];
    if S <> '' then begin
      S := S+{$IFNDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(FSavePoints[FSavePoints.Count-1]);
      ExecuteImmediat(S, lcTransaction);
    end;
    FSavePoints.Delete(FSavePoints.Count-1);
  end else begin
    CheckError(fTransaction.Abort(nil, FRetaining, False), lcTransaction, 'Rollback Transaction');
    Dec(FpulTransactionLevel);
    if (FpulTransactionLevel = 0) and not FRetaining then begin
      fTransaction := nil;
      AutoCommit := True;
      if FRestartTransaction then
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
  CheckError(FDBInitialize.QueryInterface(IID_IDBProperties, DBProperties), lcOther, EmptyRaw);
  try
    PropIDSet.rgPropertyIDs   := @APropID;
    PropIDSet.cPropertyIDs    := 1;
    PropIDSet.guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    CheckError(DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ), lcOther, EmptyRaw);
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
  ConnectString: UnicodeString;
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
      ConnectString := {$IFNDEF UNICODE}UnicodeString{$ENDIF}(DataBase)
    else
    begin
      ConnectStrings.Values[ConnProps_UserId] := User;
      ConnectStrings.Values[ConnProps_Password] := PassWord;
      ConnectString := {$IFNDEF UNICODE}UnicodeString{$ENDIF}(ComposeString(ConnectStrings, ';'));
    end;
    FServerProvider := ProviderNamePrefix2ServerProvider(ConnectStrings.Values[ConnProps_Provider]);
    fCatalog := ConnectStrings.Values[ConnProps_Initial_Catalog];
    ConnectStrings.Free;
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      Pointer(ConnectString), IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil; //no longer required!
    SetProviderProps(True); //set's timeout values
    // open the connection to the DB
    CheckError(fDBInitialize.Initialize, lcOther, EmptyRaw);
    OleCheck(fDBInitialize.QueryInterface(IID_IDBCreateSession, FDBCreateSession));
    //some Providers do NOT support commands, so let's check if we can use it
    OleCheck(FDBCreateSession.CreateSession(nil, IID_IDBCreateCommand, IUnknown(FDBCreateCommand)));
    FDBCreateSession := nil; //no longer required!
    //if FServerProvider = spMSSQL then
      //SetProviderProps(False); //provider properties -> don't work??
    inherited Open;
    if TransactIsolationLevel = tiNone then
      Inherited SetTransactionIsolation(GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation)
    else if TransactIsolationLevel <> GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation then
      InternalSetTIL(TransactIsolationLevel);
    FAutoCommitTIL := TIL[TransactIsolationLevel];
    CheckCharEncoding('CP_UTF16'); //do this by default!
    (GetMetadata.GetDatabaseInfo as IZOleDBDatabaseInfo).InitilizePropertiesFromDBInfo(fDBInitialize, fMalloc);
    if (GetServerProvider = spMSSQL) then begin
      if (Info.Values[ConnProps_DateWriteFormat] = '') or (Info.Values[ConnProps_DateTimeWriteFormat] = '') then begin
        if (Info.Values[ConnProps_DateWriteFormat] = '') then begin
          ConSettings^.WriteFormatSettings.DateFormat := 'YYYYMMDD';  //ISO format which always is accepted by SQLServer
          ConSettings^.WriteFormatSettings.DateFormatLen := 8;
        end;
        if (Info.Values[ConnProps_DateTimeWriteFormat] = '') then begin
          ConSettings^.WriteFormatSettings.DateTimeFormat := 'YYYY-MM-DDTHH:NN:SS'; //ISO format which always is accepted by SQLServer
          ConSettings^.WriteFormatSettings.DateTimeFormatLen := 19;
        end;
      end;
      { find out which encoding the raw columns do have }
      with CreateStatement.ExecuteQuery(
        'SELECT DATABASEPROPERTYEX('+QuotedStr(fCatalog)+', ''Collation'') as DatabaseCollation, '+
        '  COLLATIONPROPERTY(CAST(DATABASEPROPERTYEX('+QuotedStr(fCatalog)+', ''Collation'') as NVARCHAR(255)), ''Codepage'') as Codepage') do begin
        if Next and not IsNull(FirstDbcIndex) then begin
          ConSettings.ClientCodePage := New(PZCodePage);
          ConSettings.ClientCodePage.Encoding := ceUTF16;//well a "mixed" encoding i have not prepared yet...
          ConSettings.ClientCodePage.IsStringFieldCPConsistent := False;
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
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
      'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"');
    if not AutoCommit then begin
      AutoCommit := True;
      SetAutoCommit(False);;
    end;
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
{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZOleDBConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if (GetServerProvider = spMSSQL)
  then Result := TZOleDBCallableStatementMSSQL.Create(Self, Name, Info)
  else Raise EZUnsupportedException.Create(SUnsupportedOperation);
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
function TZOleDBConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if Closed then Open;
  Result := TZOleDBPreparedStatement.Create(Self, SQL, Info);
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

  FSavePoints.Clear;

  if not AutoCommit then begin
    SetAutoCommit(True); //close all pending transactions without restarting the TA
    AutoCommit := False; //remainder for reopen
  end;
  FDBCreateCommand := nil;
  CheckError(fDBInitialize.Uninitialize, lcOther, EmptyRaw);
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
