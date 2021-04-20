{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Connection Component              }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

(*
 Constributors:
  aehimself,
  cipto,
  EgonHugeist
  firmos
  HA,
  marsupilami97
  mdeams (Mark Deams)
  miab3
  mse
  and many others
*)

unit ZAbstractConnection;

interface

{$I ZComponent.inc}

uses
  Types,
{$IFDEF ENABLE_ADO}
  ZDbcAdo,
{$ENDIF}
{$IFDEF ENABLE_DBLIB}
  ZDbcDbLib,
{$ENDIF}
{$IFDEF ENABLE_MYSQL}
  ZDbcMySql,
{$ENDIF}
{$IFDEF ENABLE_POSTGRESQL}
  ZDbcPostgreSql,
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
  ZDbcInterbase6,
{$ENDIF}
{$IFDEF ENABLE_FIREBIRD}
  ZDbcFirebird,
{$ENDIF}
{$IFDEF ENABLE_SQLITE}
  ZDbcSqLite,
{$ENDIF}
{$IFDEF ENABLE_ORACLE}
  ZDbcOracle,
{$ENDIF}
{$IFDEF ENABLE_ASA}
  ZDbcASA,
{$ENDIF}
{$IFDEF ENABLE_SQLANY}
  ZDbcSQLAnywhere,
{$ENDIF}
{$IFDEF ENABLE_POOLED}
  ZDbcPooled,
{$ENDIF}
{$IFDEF ENABLE_OLEDB}
  ZDbcOleDB,
{$ENDIF}
{$IFDEF ENABLE_ODBC}
  ZDbcODBCCon,
{$ENDIF}
{$IFDEF ENABLE_PROXY}
  ZDbcProxy,
{$ENDIF}

  SysUtils, Classes, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  ZClasses, ZCompatibility, ZSysUtils,
  ZDbcIntfs,
  ZDatasetUtils, ZFormatSettings;

type
  /// <author>HA</author>
  /// <summary>Defines a login event.</summary>
  TZLoginEvent = procedure(Sender: TObject; var Username:string ; var Password: string) of object;

  /// <author>EgonHugeist</author>
  /// <summary>Forward declaration of the TZRawCharacterTransliterateOptions.</summary>
  TZRawCharacterTransliterateOptions = class;

  { TZAbstractConnection }

  /// <summary>Implements an abstract connection component which wraps a
  ///  connection to database.</summary>
  TZAbstractConnection = class(TComponent)
  private
    FUseMetaData: Boolean;
    FControlsCodePage: TZControlsCodePage;
    {$IFDEF ZEOS_TEST_ONLY}
    FTestMode: Byte;
    {$ENDIF}
    function GetVersion: string;
    procedure SetUseMetadata(AValue: Boolean);
    procedure SetCharacterFieldType(const Value: TZControlsCodePage);
  protected
    FURL: TZURL;
    FCatalog: string;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FConnection: IZConnection;
    FLinkedComponents: TZSortedList;
    FAddLogMsgToExceptionOrWarningMsg: Boolean;
    FRaiseWarningMessages: Boolean;
    FLoginPrompt: Boolean;
    FStreamedConnected: Boolean;
    FExplicitTransactionCounter: Integer; //this counter is required to find out setting autocommit mode back to True without loosing changes
    FTxnLevel: Integer; //the current nested_transaction/savepoint level
    FSQLHourGlass: Boolean;
    FDesignConnection: Boolean;

    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeReconnect: TNotifyEvent;
    FAfterReconnect: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnRollback, FOnLost: TNotifyEvent;
    FOnStartTransaction: TNotifyEvent;
    FOnLogin: TZLoginEvent;
    FClientCodepage: String;
    FTransactions: TZSortedList;
    FRawCharacterTransliterateOptions: TZRawCharacterTransliterateOptions;
    FFormatSettings: TZFormatSettings;
    procedure SetRawCharacterTransliterateOptions(Value: TZRawCharacterTransliterateOptions);
    procedure SetFormatSettings(const Value: TZFormatSettings);
    function GetFormatSettings: TZFormatSettings;
    function GetHostName: string;
    procedure SetHostName(const Value: String);
    function GetConnPort: Integer;
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: String);
    function GetUser: string;
    procedure SetUser(const Value: String);
    function GetPassword: string;
    procedure SetPassword(const Value: String);
    function GetLibLocation: String;
    procedure SetLibLocation(const Value: String);
    function GetProtocol: String;
    procedure SetProtocol(const Value: String);
    function GetProperties: TStrings;
    procedure SetAddLogMsgToExceptionOrWarningMsg(Value: Boolean);
    procedure SetRaiseWarningMessages(Value: Boolean);
    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    procedure SetProperties(Value: TStrings);
    procedure SetTransactIsolationLevel(Value: TZTransactIsolationLevel);
    procedure SetAutoCommit(Value: Boolean);
    /// <summary>Puts this connection in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    function GetDbcDriver: IZDriver;
    function GetInTransaction: Boolean;
    function GetClientVersion: Integer;
    function GetServerVersion: Integer;
    function GetClientVersionStr: String;
    function GetServerVersionStr: String;
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoBeforeDisconnect;
    procedure DoAfterDisconnect;
    procedure DoBeforeReconnect;
    procedure DoAfterReconnect;
    procedure DoCommit;
    procedure DoRollback;
    procedure DoStartTransaction;

    procedure CheckConnected;
    procedure CheckDisconnected;
    procedure CheckAutoCommitMode;
    procedure CheckNonAutoCommitMode;

    function ConstructURL(const UserName, Password: string): string;

    procedure CloseAllLinkedComponents;
    procedure UnregisterAllComponents;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;

    property StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;

    procedure SetClientCodePage(Const Value: String);
    procedure ConnectionLost(var AError: EZSQLConnectionLost);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Reconnect;
    function Ping: Boolean; virtual;

    function StartTransaction: Integer; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure PrepareTransaction(const transactionid: string); virtual;
    procedure CommitPrepared(const transactionid: string); virtual;
    procedure RollbackPrepared(const transactionid: string); virtual;
    function PingServer: Boolean; virtual;
    Function AbortOperation: Boolean; virtual;

    function ExecuteDirect(const SQL: string): boolean; overload;
    function ExecuteDirect(const SQL: string; out RowsAffected: integer): boolean; overload;

    procedure RegisterComponent(Value: TComponent);
    procedure UnregisterComponent(Value: TComponent);

    procedure GetProtocolNames(List: TStrings);
    procedure GetCatalogNames(List: TStrings);
    procedure GetSchemaNames(List: TStrings);
    procedure GetTableNames(const Pattern: string; List: TStrings);overload;
    procedure GetTableNames(const schemaPattern, tablePattern: string; List: TStrings);overload;
    procedure GetTableNames(const schemaPattern, tablePattern: string; const Types: TStringDynArray; List: TStrings);overload;
    procedure GetColumnNames(const TablePattern, ColumnPattern: string; List: TStrings);

    procedure GetStoredProcNames(const Pattern: string; List: TStrings);
    procedure GetTriggerNames(const TablePattern, SchemaPattern: string; List: TStrings);

    function GetURL: String;

    property InTransaction: Boolean read GetInTransaction;

    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetConnPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Protocol: string read GetProtocol write SetProtocol;
    property LibLocation: string read GetLibLocation write SetLibLocation;

    property DbcDriver: IZDriver read GetDbcDriver;
    property DbcConnection: IZConnection read FConnection;
    property ClientVersion: Integer read GetClientVersion;
    property ServerVersion: Integer read GetServerVersion;
    property ClientVersionStr: String read GetClientVersionStr;
    property ServerVersionStr: String read GetServerVersionStr;
    procedure ShowSQLHourGlass;
    procedure HideSQLHourGlass;
  published
    property FormatSettings: TZFormatSettings read GetFormatSettings write SetFormatSettings;
    property ControlsCodePage: TZControlsCodePage read FControlsCodePage write SetCharacterFieldType;
    property RawCharacterTransliterateOptions: TZRawCharacterTransliterateOptions read FRawCharacterTransliterateOptions write
      SetRawCharacterTransliterateOptions;
    property ClientCodepage: String read FClientCodepage write SetClientCodePage;
    property Catalog: string read FCatalog write FCatalog;
    property Properties: TStrings read GetProperties write SetProperties;
    property AutoCommit: Boolean read FAutoCommit write SetAutoCommit
      default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly
      default False;
    property UseMetadata: Boolean read FUseMetaData write SetUseMetadata default true;
    property TransactIsolationLevel: TZTransactIsolationLevel
      read FTransactIsolationLevel write SetTransactIsolationLevel
      default tiNone;
    property Connected: Boolean read GetConnected write SetConnected
      default False;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt
      default False;
    property Version: string read GetVersion stored False;
    property DesignConnection: Boolean read FDesignConnection
      write FDesignConnection default False;
    property AddLogMsgToExceptionOrWarningMsg: Boolean
      read fAddLogMsgToExceptionOrWarningMsg
      write SetAddLogMsgToExceptionOrWarningMsg default True;
    property RaiseWarningMessages: Boolean read fRaiseWarningMessages
      write SetRaiseWarningMessages default False;

    property BeforeConnect: TNotifyEvent
      read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent
      read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent
      read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent
      read FAfterDisconnect write FAfterDisconnect;
    property BeforeReconnect: TNotifyEvent
      read FBeforeReconnect write FBeforeReconnect;
    property AfterReconnect: TNotifyEvent
      read FAfterReconnect write FAfterReconnect;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass
      default False;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
    property OnLogin: TZLoginEvent read FOnLogin write FOnLogin;
    property OnStartTransaction: TNotifyEvent
      read FOnStartTransaction write FOnStartTransaction;
    property OnLost: TNotifyEvent read FOnLost write FOnLost;
    {$IFDEF ZEOS_TEST_ONLY}
    property TestMode : Byte read FTestMode write FTestMode;
    {$ENDIF}
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a raw to utf16 and vice verca setting object</summary>
  TZRawCharacterTransliterateOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[WEAK]{$ENDIF}FConnection: TZAbstractConnection;
    {$IFNDEF UNICODE}
    FSQL: Boolean;
    {$ENDIF}
    FEncoding: TZW2A2WEncodingSource;
    FParams: Boolean;
    FFields: Boolean;
    function GetSQL: Boolean;
    {$IFNDEF UNICODE}
    procedure SetSQL(Value: Boolean);
    {$ENDIF UNICODE}
    function GetEncoding: TZW2A2WEncodingSource;
    procedure SetEncoding(Value: TZW2A2WEncodingSource);
  public
    Constructor Create(AOwner: TZAbstractConnection);
    function GetRawTransliterateCodePage(Target: TZTransliterationType): Word;
  published
    property SQL: Boolean read GetSQL {$IFNDEF UNICODE}write SetSQL stored True{$ELSE}stored False{$ENDIF} default False;
    //this option is not reachable in Component-Layer for the Unicode-Compilers
    //it's always DB_CP!! But in DBC it's also interesting. Thus another enumerator
    property Encoding: TZW2A2WEncodingSource read GetEncoding
      write SetEncoding stored {$IFDEF UNCIODE}False{$ELSE}True{$ENDIF} default encDB_CP;
    property Params: Boolean read FParams write FParams stored true default False;
    property Fields: Boolean read FFields write FFields stored True default True;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract component which is linked with a
  ///  connection component</summary>
  TZAbstractConnectionLinkedComponent = Class(TComponent)
  protected
    FConnection: TZAbstractConnection;
    FActive: Boolean;
    procedure SetConnection(Value: TZAbstractConnection); virtual; abstract;
    /// <summary>Processes component notifications.</summary>
    /// <param>"AComponent" a changed component object.</summary>
    /// <param>"Operation" a component operation code.</summary>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  protected
    property Connection: TZAbstractConnection read FConnection write SetConnection;
  End;

  /// <author>EgonHugeist</author>
  /// <summary>Implements an abstract component which is linked with a
  ///  connection component and can be (de)activated</summary>
  TAbstractActiveConnectionLinkedComponent = Class(TZAbstractConnectionLinkedComponent)
  protected
    FActive: Boolean;
    procedure SetActive(Value: Boolean); virtual; abstract;
  protected
    property Active: Boolean read FActive write SetActive;
  End;

  /// <author>EgonHugeist</author>
  /// <summary>Test if the component is a zeos component</summary>
  function IsZeosConnectionComponent(AValue: TComponent): Boolean;

implementation

uses ZMessages, ZFastCode, ZEncoding,
  ZDbcProperties, ZDbcLogging,
  ZSequence, ZAbstractDataset, ZAbstractRODataset, ZMemTable, ZSqlProcessor,
  ZTransaction;

var
  SqlHourGlassLock: Integer;
  CursorBackup: TDBScreenCursor;

function IsZeosConnectionComponent(AValue: TComponent): Boolean;
begin
  Result := (AValue <> nil) and (AValue.InheritsFrom(TZAbstractRODataSet) or
      AValue.InheritsFrom(TZSQLProcessor) or AValue.InheritsFrom(TZSequence) or
      AValue.InheritsFrom(TZAbstractTransaction) or AValue.InheritsFrom(TZSqlProcessor) or
      AValue.InheritsFrom(TZAbstractConnectionLinkedComponent));
end;
{ TZAbstractConnection }

{**
  Constructs this component and assignes the main properties.
  @param AOwner an owner component.
}
constructor TZAbstractConnection.Create(AOwner: TComponent);
begin
  {$IFDEF UNICODE}
  FControlsCodePage := cCP_UTF16;
  {$ELSE}
    {$IFDEF FPC}
    FControlsCodePage := cCP_UTF8;
    {$ELSE}
    FControlsCodePage := cGET_ACP;
    {$ENDIF}
  {$ENDIF}
  FURL := TZURL.Create;
  inherited Create(AOwner);
  FRawCharacterTransliterateOptions := TZRawCharacterTransliterateOptions.Create(Self);
  FAutoCommit := True;
  FReadOnly := False;
  FTransactIsolationLevel := tiNone;
  FAddLogMsgToExceptionOrWarningMsg := True;
  FRaiseWarningMessages := False;
  FConnection := nil;
  FUseMetadata := True;
  FLinkedComponents := TZSortedList.Create;
  FTransactions := TZSortedList.Create;
  FLoginPrompt := False;
  FDesignConnection := False;
  FFormatSettings := TZFormatSettings.Create(Self);
end;

{**
  Destroys this component and cleanups the memory.
}
destructor TZAbstractConnection.Destroy;
begin
  Disconnect;
  UnregisterAllComponents;
  inherited Destroy; //calls Notification(self,opRemove)
  FURL.Free;
  FreeAndNil(FLinkedComponents);
  FreeAndNil(FTransactions);
  FreeAndNil(FRawCharacterTransliterateOptions);
  FreeAndNil(FFormatSettings);
end;

function TZAbstractConnection.GetHostName: string;
begin
  Result := FURL.HostName;
end;

procedure TZAbstractConnection.SetHostName(const Value: String);
begin
  FURL.HostName := Value;
end;

function TZAbstractConnection.GetConnPort: Integer;
begin
  Result := FURL.Port;
end;

procedure TZAbstractConnection.SetConnPort(const Value: Integer);
begin
  FURL.Port := Value;
end;

function TZAbstractConnection.GetDatabase: string;
begin
  Result := FURL.Database;
end;

procedure TZAbstractConnection.SetDatabase(const Value: String);
begin
  FURL.Database := Value;
end;

procedure TZAbstractConnection.SetFormatSettings(const Value: TZFormatSettings);
begin
  FFormatSettings.Assign(Value);
end;

function TZAbstractConnection.GetUser: string;
begin
  Result := FURL.UserName;
end;

procedure TZAbstractConnection.SetUser(const Value: String);
begin
  FURL.UserName := Value;
end;

function TZAbstractConnection.GetPassword: string;
begin
  Result := FURL.Password;
end;

procedure TZAbstractConnection.SetPassword(const Value: String);
begin
  FURL.Password := Value;
end;

function TZAbstractConnection.GetLibLocation: String;
begin
  Result := FURL.LibLocation;
end;

procedure TZAbstractConnection.SetLibLocation(const Value: String);
begin
  FURL.LibLocation := Value;
end;

function TZAbstractConnection.GetProtocol: String;
begin
  Result := FURL.Protocol;
end;

procedure TZAbstractConnection.SetProtocol(const Value: String);
begin
  FURL.Protocol := Value;
end;

function TZAbstractConnection.GetProperties: TStrings;
begin
  Result := FURL.Properties;
end;

{**
  This methode is required to support proper component initialization.
  Without it, the connection can start connecting before every property is loaded!
}
procedure TZAbstractConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then
      if (csDesigning in ComponentState) or not FDesignConnection then
        SetConnected(True);
  except
    if csDesigning in ComponentState then
      if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
  end;
end;

{**
  EgonHugeist:
  Sets the ClientCode-Page-Property and adds or corrects it in the
    Info(Properties)-Strings
  @param <code>string</code> the ClientCharacterSet
}
procedure TZAbstractConnection.SetClientCodePage(Const Value: String);
begin
  if ( Value = 'Not Connected!' ) or ( Value = 'Not implementet!' ) then
    //possible! -> result of PropertyEditor if not complete yet
    //Later we should remove this if the MeataData/Plaindriver-Informations
    //where complete
    FClientCodepage := Trim(FURL.Properties.Values[ConnProps_CodePage])
  else
    Self.FClientCodepage := Value;
  if ( Trim(FURL.Properties.Values[ConnProps_CodePage]) <> FClientCodepage ) then
    FURL.Properties.Values[ConnProps_CodePage] := FClientCodepage;
end;

{**
  Gets an open connection flag.
  @return <code>True</code> if the connection is open
    or <code>False</code> otherwise.
}
function TZAbstractConnection.GetConnected: Boolean;
begin
  Result := (FConnection <> nil) and not FConnection.IsClosed;
end;

{**
  Sets a new open connection flag.
  @param Value <code>True</code> to open the connection
    and <code>False</code> to close it.
}
procedure TZAbstractConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else if Value <> GetConnected then
    if Value
    then Connect
    else Disconnect
  else if Not Value And Assigned(FConnection) Then
    FConnection := nil; // Make sure to throw away FConnection to ensure reconnecting. Otherwise a next call to .Connect will not do anything!!!
end;

{**
  Sets a new connection properties.
  @param Value a list with new connection properties.
}
procedure TZAbstractConnection.SetProperties(Value: TStrings);
begin
  FURL.Properties.Assign(Value);
end;

{**
  Sets AddLogMsgToExceptionOrWarningMsg flag.
  @param Value <code>True</code> to turn message concatation.
}
procedure TZAbstractConnection.SetAddLogMsgToExceptionOrWarningMsg(
  Value: Boolean);
begin
  if FAddLogMsgToExceptionOrWarningMsg <> Value then begin
    if FConnection <> nil then
      FConnection.SetAddLogMsgToExceptionOrWarningMsg(Value);
    FAddLogMsgToExceptionOrWarningMsg := Value;
  end;
end;

{**
  Sets autocommit flag.
  @param Value <code>True</code> to turn autocommit on.
}
procedure TZAbstractConnection.SetAutoCommit(Value: Boolean);
begin
  if FAutoCommit <> Value then begin
    if FExplicitTransactionCounter > 0 then
      raise Exception.Create(SInvalidOperationInTrans);
    FAutoCommit := Value;
    if Value then
      FExplicitTransactionCounter := 0;
    if FConnection <> nil then begin
      if not FConnection.IsClosed then ShowSQLHourGlass;
      try
        FConnection.SetAutoCommit(Value);
      finally
        if not FConnection.IsClosed then HideSqlHourGlass
      end;
    end;
  end;
end;

{**
  Sets readonly flag.
  @param Value readonly flag.
}
procedure TZAbstractConnection.SetRaiseWarningMessages(Value: Boolean);
begin
  if FRaiseWarningMessages <> Value then begin
    if FConnection <> nil then
      FConnection.SetRaiseWarnings(Value);
    FRaiseWarningMessages := Value;
  end;
end;

procedure TZAbstractConnection.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then begin
    if FExplicitTransactionCounter > 0 then
      raise Exception.Create(SInvalidOperationInTrans);
    FReadOnly := Value;
    if FConnection <> nil then begin
      if not FConnection.IsClosed then ShowSQLHourGlass;
      try
        FConnection.SetReadOnly(Value);
      finally
        if not FConnection.IsClosed then HideSqlHourGlass
      end;
    end;
  end;
end;

{**
  Sets transact isolation level.
  @param Value a transact isolation level.
}
procedure TZAbstractConnection.SetTransactIsolationLevel(
  Value: TZTransactIsolationLevel);
begin
  if FTransactIsolationLevel <> Value then
  begin
    FTransactIsolationLevel := Value;
    if FConnection <> nil then begin
      if not FConnection.IsClosed then ShowSQLHourGlass;
      try
        FConnection.SetTransactionIsolation(Value);
      finally
        if not FConnection.IsClosed then HideSqlHourGlass
      end;
    end;
  end;
end;

{**
  Gets a ZDBC driver for the specified protocol.
  @returns a ZDBC driver interface.
}
function TZAbstractConnection.GetDbcDriver: IZDriver;
begin
  if FConnection <> nil then
    Result := FConnection.GetDriver
  else
    Result := DriverManager.GetDriver(ConstructURL('', ''));
end;

function TZAbstractConnection.GetFormatSettings: TZFormatSettings;
begin
  if FFormatSettings = nil then
    FFormatSettings := TZFormatSettings.Create(Self);
  Result := FFormatSettings;
end;

{**
  Checks is the connection started a transaction.
  @returns <code>True</code> if connection in manual transaction mode
    and transaction is started.
}
function TZAbstractConnection.GetInTransaction: Boolean;
begin
  CheckConnected;
  Result := not FAutoCommit or (FExplicitTransactionCounter > 0);
end;

{**
  Gets client's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractConnection.GetClientVersion: Integer;
begin
  if FConnection <> nil then
    Result := DbcConnection.GetClientVersion
  else
    Result := DriverManager.GetClientVersion(ConstructURL('', ''));
end;

{**
  Gets server's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractConnection.GetServerVersion: Integer;
begin
  CheckConnected;
  Result := DbcConnection.GetHostVersion;
end;

{**
  Gets client's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractConnection.GetClientVersionStr: String;
begin
  Result := FormatSQLVersion(GetClientVersion);
end;

{**
  Gets server's full version number.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractConnection.GetServerVersionStr: String;
begin
  Result := FormatSQLVersion(GetServerVersion);
end;

{**
  Constructs ZDBC connection URL string.
  @param UserName a name of the user.
  @param Password a user password.
  @returns a constructed connection URL.
}
function TZAbstractConnection.ConstructURL(const UserName, Password: string): string;
var AURL: TZURL;
begin
  AURL := TZURL.Create;
  Result := '';
  try
    //EH: Set the attachment charsset, AuotEncode, and ControlsCP again
    //if the user did clear the properties then this info is lost
    //See https://sourceforge.net/p/zeoslib/tickets/329/
    if (FURL.Properties.Values[ConnProps_CodePage] = '') and (FClientCodePage <> '') then
      FURL.Properties.Values[ConnProps_CodePage] := FClientCodePage;
    {$IFDEF UNICODE}
    FURL.Properties.Values[ConnProps_RawStringEncoding] := 'DB_CP';
    {$ELSE}
    case FRawCharacterTransliterateOptions.FEncoding of //automated check..
      encDB_CP: FURL.Properties.Values[ConnProps_RawStringEncoding] := 'DB_CP';
      encUTF8: FURL.Properties.Values[ConnProps_RawStringEncoding] := 'CP_UTF8';
      encDefaultSystemCodePage: FURL.Properties.Values[ConnProps_RawStringEncoding] := 'GET_ACP';
    end;
    {$ENDIF}
    AURL.Protocol := FURL.Protocol;
    AURL.HostName := FURL.HostName;
    AURL.Database := FURL.Database;
    AURL.UserName := UserName;
    AURL.Password := Password;
    AURL.Port := Port;
    AURL.Properties.AddStrings(FURL.Properties);
    AURL.LibLocation := FURL.LibLocation;
    Result := AURL.URL;
  finally
    FreeAndNil(AURL);
  end;
end;

{**
  Fires an event before connection open
}
procedure TZAbstractConnection.DoBeforeConnect;
begin
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);
end;

{**
  Fires an event after connection open
}
procedure TZAbstractConnection.DoAfterConnect;
begin
  if Assigned(FAfterConnect) then
    FAfterConnect(Self);
end;

{**
  Fires an event before connection close
}
procedure TZAbstractConnection.DoBeforeDisconnect;
begin
  if Assigned(FBeforeDisconnect) then
    FBeforeDisconnect(Self);
end;

{**
  Fires an event after connection close
}
procedure TZAbstractConnection.DoAfterDisconnect;
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

{**
  Fires an event before reconnect
}
procedure TZAbstractConnection.DoBeforeReconnect;
begin
  if Assigned(FBeforeReconnect) then
    FBeforeReconnect(Self);
end;

{**
  Fires an event after reconnect
}
procedure TZAbstractConnection.DoAfterReconnect;
begin
  if Assigned(FAfterReconnect) then
    FAfterReconnect(Self);
end;

{**
  Fires an event after transaction commit
}
procedure TZAbstractConnection.DoCommit;
begin
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

{**
  Fires an event after transaction rollback
}
procedure TZAbstractConnection.DoRollback;
begin
  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

{**
  Fires an event after transaction start
}
procedure TZAbstractConnection.DoStartTransaction;
begin
  if Assigned(FOnStartTransaction) then
    FOnStartTransaction(Self);
end;

{**
  Establish a connection with database.
}
procedure TZAbstractConnection.Connect;
var
//Local variables declared in order to preserve the original property value
//and to avoid the storage of password
  Username, Password: string;
begin
  if FConnection = nil then begin
    DoBeforeConnect;

    UserName := FURL.UserName;
    Password := FURL.Password;

    if FLoginPrompt then
      if Assigned(FOnLogin)
      then FOnLogin(Self, UserName, Password)
      else if Assigned(LoginDialogProc) then begin
        if not LoginDialogProc(FURL.Database, UserName, Password) then
          Exit;
      end else
        raise EZDatabaseError.Create(SLoginPromptFailure);

    ShowSqlHourGlass;
    try
      FConnection := DriverManager.GetConnectionWithParams(
        ConstructURL(UserName, Password), FURL.Properties);
      try
        with FConnection do begin
          SetOnConnectionLostErrorHandler(ConnectionLost);
          SetAutoCommit(FAutoCommit);
          SetReadOnly(FReadOnly);
          SetCatalog(FCatalog);
          SetTransactionIsolation(FTransactIsolationLevel);
          SetUseMetadata(FUseMetadata);
          SetAddLogMsgToExceptionOrWarningMsg(FAddLogMsgToExceptionOrWarningMsg);
          SetRaiseWarnings(FRaiseWarningMessages);
          Open;
          {$IFDEF ZEOS_TEST_ONLY}
          SetTestMode(FTestMode);
          {$ENDIF}
        end;
      finally
        if Assigned(FConnection) And FConnection.IsClosed then
          FConnection := nil;
      end;
    finally
      HideSqlHourGlass;
    end;
    FTxnLevel := Ord(not FAutoCommit);
    FExplicitTransactionCounter := FTxnLevel;
    if not FAutoCommit then
      DoStartTransaction;
    if Assigned(FConnection) And not FConnection.IsClosed then
      DoAfterConnect;
  end;
end;

procedure TZAbstractConnection.ConnectionLost(var AError: EZSQLConnectionLost);
var Err: EZSQLConnectionLost;
    EventErrorRaised: Boolean;
    ADatBaseError: EZDataBaseError;
begin
  Err := AError;
  AError := nil;
  try
    CloseAllLinkedComponents;
  except end;
  EventErrorRaised := True;
  try
    if Assigned(FOnLost) then
      FOnLost(Self);
    EventErrorRaised := False;
  finally
    if EventErrorRaised then begin
      if (Err <> nil) then
        FreeAndNil(Err);
    end else begin
      if Err = nil //should not happen
      then ADatBaseError := EZDataBaseError.Create('Connection lost.')
      else begin
        ADatBaseError := EZDataBaseError.CreateFromException(Err);
        FreeAndNil(Err);
      end;
      raise ADatBaseError;
    end;
  end;
end;

{**
  Closes and removes the connection with database
}
procedure TZAbstractConnection.Disconnect;
begin
  if FConnection <> nil then begin
    DoBeforeDisconnect;

    ShowSqlHourGlass;
    try
      CloseAllLinkedComponents;
      FConnection.Close;
      FConnection.SetOnConnectionLostErrorHandler(nil);
    finally
      FConnection := nil;
      HideSqlHourGlass;
    end;
    FTxnLevel := 0;
    FExplicitTransactionCounter := 0;
    DoAfterDisconnect;
  end;
end;


{**
  Sends a ping to the server.
}
function TZAbstractConnection.Ping: Boolean;
begin
  Result := (FConnection <> nil) and (FConnection.PingServer=0);
end;

{**
  Reconnect, doesn't destroy DataSets if successful.
}
procedure TZAbstractConnection.Reconnect;
var Succeeded: Boolean;
begin
  if FConnection <> nil then begin
    DoBeforeReconnect;
    ShowSqlHourGlass;
    Succeeded := False;
    try
      try
        FConnection.Close;
        FConnection.Open;
        Succeeded := True;
      finally
        if not Succeeded then
          CloseAllLinkedComponents;
      end;
    finally
      HideSqlHourGlass;
    end;
    DoAfterReconnect;
  end;
end;

{**  Checks if this connection is active.
}
procedure TZAbstractConnection.CheckConnected;
begin
  if (FConnection = nil) or FConnection.IsClosed then
    raise EZDatabaseError.Create(SConnectionIsNotOpened);
end;

{**  Checks if this connection is inactive.
}
procedure TZAbstractConnection.CheckDisconnected;
begin
  if (FConnection <> nil) and not FConnection.IsClosed then
    raise EZDatabaseError.Create(SConnectionIsNotOpened);
end;

{**
  Checks if this connection is in auto-commit mode.
}
procedure TZAbstractConnection.CheckNonAutoCommitMode;
begin
  if FAutoCommit then
    raise EZDatabaseError.Create(SInvalidOpInAutoCommit);
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
function TZAbstractConnection.AbortOperation: Boolean;
begin
 Result := FConnection.AbortOperation = 0;
end;

{**
  Checks if this connection is in auto-commit mode.
}
procedure TZAbstractConnection.CheckAutoCommitMode;
begin
  if not FAutoCommit and (FExplicitTransactionCounter = 0) then
    raise EZDatabaseError.Create(SInvalidOpInNonAutoCommit);
end;

{**
  Starts a new transaction or saves the transaction.
}
function TZAbstractConnection.StartTransaction: Integer;
begin
  CheckConnected;
  ShowSQLHourGlass;
  try
    FTxnLevel := FConnection.StartTransaction;
    FAutoCommit := False;
  finally
    HideSQLHourGlass;
  end;
  FExplicitTransactionCounter := FTxnLevel;
  Result := FTxnLevel;
  DoStartTransaction;
end;

{**
  Commits the current transaction.
}
type //To get protected methodes
  THack_ZAbstractDataset = Class(TZAbstractRWDataSet);
procedure TZAbstractConnection.Commit;
var i: Integer;
    AComp: TComponent;
begin
  CheckConnected;
  CheckNonAutoCommitMode;

  ShowSQLHourGlass;
  try
    for i := 0 to FLinkedComponents.Count -1 do begin
      AComp := TComponent(FLinkedComponents[i]);
      if Assigned(AComp) And (AComp is TZAbstractRWDataSet) and
         TZAbstractRWDataSet(AComp).CachedUpdates and
         TZAbstractRWDataSet(AComp).UpdatesPending then
          TZAbstractRWDataSet(AComp).ApplyUpdates;
    end;
    FConnection.Commit;
{ TODO -oEgonHugeist : Change this code sequence on 7.3! My automation idea simply is wrong! A commit vs. commitupdate(clear the cache) shouldn't be same! }
    //See: http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=19800
    for i := 0 to FLinkedComponents.Count -1 do begin
      AComp := TComponent(FLinkedComponents[i]);
      if Assigned(AComp) And (AComp is TZAbstractRWDataSet) and
         TZAbstractRWDataSet(AComp).UpdatesPending then
          THack_ZAbstractDataset(AComp).DisposeCachedUpdates;
    end;
  finally
    Dec(FExplicitTransactionCounter);
    HideSQLHourGlass;
    FAutoCommit := FConnection.GetAutoCommit;
    Dec(FTxnLevel);
    if FTxnLevel = 0 then
      FTxnLevel := Ord(not FAutoCommit);
  end;
  DoCommit;
end;

procedure TZAbstractConnection.CommitPrepared(const transactionid: string);
var
  oldlev: TZTransactIsolationLevel;
begin
  CheckAutoCommitMode;
  oldlev := TransactIsolationLevel;
  TransactIsolationLevel := tiNone;
  FConnection.CommitPrepared(transactionid);
  TransactIsolationLevel := oldLev;
end;

{**
  Rollbacks the current transaction.
}
procedure TZAbstractConnection.Rollback;
begin
  CheckConnected;
  CheckNonAutoCommitMode;

  ShowSQLHourGlass;
  try
    FConnection.RollBack;
  finally
    Dec(FExplicitTransactionCounter);
    HideSQLHourGlass;
    FAutoCommit := FConnection.GetAutoCommit;
    Dec(FTxnLevel);
    if FTxnLevel = 0 then
      FTxnLevel := Ord(not FAutoCommit);
  end;
  DoRollback;
end;

procedure TZAbstractConnection.RollbackPrepared(const transactionid: string);
var
  oldlev: TZTransactIsolationLevel;
begin
  CheckAutoCommitMode;
  oldlev := TransactIsolationLevel;
  TransactIsolationLevel := tiNone;
  FConnection.RollbackPrepared(transactionid);
  TransactIsolationLevel := oldLev;
end;

{**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZAbstractConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
var Idx: Integer;
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then begin
    Idx := FLinkedComponents.IndexOf(AComponent);
    if Idx <> -1 then
      FLinkedComponents.Remove(AComponent);
  end;
end;

Function TZAbstractConnection.PingServer: Boolean;
var
  LastState: boolean;
begin
  Result := false;
  // Check connection status
  LastState := GetConnected;
  If FConnection <> Nil Then
  try
    Result := (FConnection.PingServer=0);
    // Connection now is false but was true
    If (Not Result) And (LastState) Then
      // Generate OnDisconnect event
      SetConnected(Result);
  except
    on E:Exception do
    begin
      if not (E is EZUnsupportedException) and LastState then
        // Generate OnDisconnect event
        SetConnected(False);
      Raise;
    End;
  end
  else If LastState Then // Connection now is false but was true
    SetConnected(false);
end;

procedure TZAbstractConnection.PrepareTransaction(const transactionid: string);
{var
  ExplicitTran: Boolean;}
begin
  CheckConnected;
  CheckNonAutoCommitMode;
  if FExplicitTransactionCounter <> 1 then
    raise EZDatabaseError.Create(SInvalidOpPrepare);
  ShowSQLHourGlass;
  try
    try
      FConnection.PrepareTransaction(transactionid);
    finally
      FExplicitTransactionCounter := 0;
      AutoCommit := True;
    end;
  finally
    HideSQLHourGlass;
  end;
end;


type
  TZProtectedMethodTransaction = Class(TZAbstractTransaction);
  TZProtectedAbstractRODataset = class(TZAbstractRODataset);
  TZProtectedAbstractRWDataSet = Class(TZAbstractRWDataSet);

{**
  Closes all registered datasets.
}
procedure TZAbstractConnection.CloseAllLinkedComponents;
var
  I: Integer;
  AComp: TComponent;
begin
  for i := FLinkedComponents.Count -1 downto 0 do begin
    AComp := TComponent(FLinkedComponents[i]);
    if AComp.InheritsFrom(TZAbstractMemTable) then
      continue
    else begin
      if AComp.InheritsFrom(TZAbstractRODataset) then begin
        if not TZProtectedAbstractRODataset(AComp).Active or not TZProtectedAbstractRODataset(AComp).TryKeepDataOnDisconnect then try
          TZAbstractRODataset(AComp).Close;
          TZAbstractRODataset(AComp).UnPrepare;
        except {Ignore.} end else if AComp.InheritsFrom(TZAbstractRWDataSet) then begin
          TZProtectedAbstractRWDataSet(AComp).CachedResultSet.ClearStatementLink;
          TZProtectedAbstractRWDataSet(AComp).ResultSetMetadata := TZProtectedAbstractRWDataSet(AComp).CachedResultSet.GetMetadata;
          if TZProtectedAbstractRWDataSet(AComp).Statement <> nil then begin
            TZProtectedAbstractRWDataSet(AComp).Statement.Close;
            TZProtectedAbstractRWDataSet(AComp).Statement := nil;
          end;
          TZProtectedAbstractRWDataSet(AComp).ResultSetMetadata.SetMetadata(nil);
          if TZProtectedAbstractRWDataSet(AComp).CachedResolver <> nil then
            TZProtectedAbstractRWDataSet(AComp).CachedResolver.SetConnection(nil);
        end;
      end else if AComp.InheritsFrom(TZSequence) then try
        TZSequence(AComp).CloseSequence
      except end else if AComp.InheritsFrom(TZAbstractTransaction) then try
        if TZAbstractTransaction(AComp).Active then
          TZProtectedMethodTransaction(AComp).GetIZTransaction.Close;
      except end else if AComp.InheritsFrom(TAbstractActiveConnectionLinkedComponent) then try
        if TAbstractActiveConnectionLinkedComponent(AComp).Active then
          TAbstractActiveConnectionLinkedComponent(AComp).SetActive(False);
      except end;
    end;
  end;
end;

{**
  Registers a new dataset object.
  @param DataSet a new dataset to be registered.
}
procedure TZAbstractConnection.RegisterComponent(Value: TComponent);
var IDX: Integer;
begin
  if not IsZeosConnectionComponent(Value) then
    raise EZDatabaseError.Create('Invalid Component');
  IDX := FLinkedComponents.IndexOf(Value);
  if IDX = -1 then
    FLinkedComponents.Add(Pointer(Value));
end;

{**
  Unregisters a new dataset object.
  @param DataSet a new dataset to be unregistered.
}
procedure TZAbstractConnection.UnregisterComponent(Value: TComponent);
begin
  FLinkedComponents.Remove(Value);
end;

procedure TZAbstractConnection.UnregisterAllComponents;
var
  I: Integer;
  AComp: TComponent;
begin
  for i := FLinkedComponents.Count - 1 downto 0 do begin
    AComp := TComponent(FLinkedComponents[i]);
    if AComp.InheritsFrom(TZAbstractRODataset) then
      TZProtectedAbstractRODataset(AComp).Connection := nil
    else if AComp.InheritsFrom(TZSequence) then
      TZSequence(AComp).Connection := nil
    else if AComp.InheritsFrom(TZAbstractTransaction) then
      TZAbstractTransaction(AComp).Connection := nil
    else if AComp.InheritsFrom(TZSQLProcessor) then
      TZAbstractTransaction(TZSQLProcessor).Connection := nil
  end;
end;

{**
  Turn on sql hourglass cursor
}
procedure TZAbstractConnection.ShowSQLHourGlass;
begin
  if not FSqlHourGlass then
    Exit;

  if (SqlHourGlassLock = 0) and Assigned(DBScreen) then begin
    CursorBackup := DBScreen.Cursor;
    if CursorBackup <> dcrOther then
      DBScreen.Cursor := dcrSQLWait;
  end;
  Inc(SqlHourGlassLock);
end;

{**
  Turn off sql hourglass cursor
}
procedure TZAbstractConnection.HideSQLHourGlass;
begin
  if not FSqlHourGlass then
    Exit;

  if SqlHourGlassLock > 0 then
    Dec(SqlHourGlassLock);
  if SqlHourGlassLock = 0 then
  begin
    if CursorBackup <> dcrOther then
      if Assigned(DBScreen) then
        DBScreen.Cursor := CursorBackup;
  end;
end;

{**
  Fills string list with registered protocol names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetProtocolNames(List: TStrings);
var
  I, J: Integer;
  Drivers: IZCollection;
  Driver: IZDriver;
  Protocols: TStringDynArray;
begin
  List.Clear;
  Protocols := nil; // Makes compiler happy
  Drivers := DriverManager.GetDrivers;
  for I := 0 to Drivers.Count - 1 do
  begin
    Driver := Drivers[I] as IZDriver;
    Protocols := Driver.GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      List.Add(Protocols[J]);
  end;
end;

{**
  Fills string list with catalog names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetCatalogNames(List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetCatalogs;
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_CAT'));
end;

{**
  Fills string list with schema names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetSchemaNames(List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetSchemas;
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_SCHEM'));
end;

{**
  Fills string list with table names.
  @param Pattern a pattern for table names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetTableNames(const Pattern: string; List: TStrings);
begin
  GetTableNames('', Pattern, nil, List);
end;

{**
  Fills string list with table names.
  @param tablePattern a pattern for table names.
  @param schemaPattern a pattern for schema names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetTableNames(const schemaPattern, tablePattern: string; List: TStrings);
begin
  GetTableNames(schemaPattern, tablePattern, nil,List);
end;

{**
  Fills string list with table names.
  @param tablePattern a pattern for table names.
  @param schemaPattern a pattern for schema names.
  @param types a TStringDynArray specifying the table types to look for.
    possible values can be found by reading
     TZAbstractConnection.DbcConnection.GetMetadata.GetTableTypes
     eg. for PostGreSQL this includes :'TABLE', 'VIEW', 'INDEX', 'SEQUENCE',
                                       'SYSTEM TABLE', 'SYSTEM TOAST TABLE',
                                       'SYSTEM TOAST INDEX', 'SYSTEM VIEW',
                                       'SYSTEM INDEX', 'TEMPORARY TABLE',
                                       'TEMPORARY INDEX'
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetTableNames(const schemaPattern, tablePattern: string; const Types: TStringDynArray; List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetTables('', schemaPattern, tablePattern, types);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('TABLE_NAME'));
end;

{**
  Fills string list with column names.
  @param TablePattern a pattern for table names.
  @param ColumnPattern a pattern for column names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetColumnNames(const TablePattern, ColumnPattern: string; List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;
  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetColumns('', '', TablePattern, ColumnPattern);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('COLUMN_NAME'));
end;

{**
  Fills string list with stored procedure names.
  @param Pattern a pattern for table names.
  @param List a string list to fill out.
}
procedure TZAbstractConnection.GetStoredProcNames(const Pattern: string;
  List: TStrings);
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  CheckConnected;

  List.Clear;
  Metadata := DbcConnection.GetMetadata;
  ResultSet := Metadata.GetProcedures('', '', Pattern);
  while ResultSet.Next do
    List.Add(ResultSet.GetStringByName('PROCEDURE_NAME'));
end;

{**
  EgonHugeist Returns Database-Triggers
  @Param TablePattern is a "like"-pattern to get Triggers of specified Table
  @SchemaPattern is Pattern to filter Schema-Trigger
  @List the Result-Trigger-List
}
procedure TZAbstractConnection.GetTriggerNames(const TablePattern,
  SchemaPattern: string; List: TStrings);
begin
  CheckConnected;

  List.Clear;
  with DbcConnection.GetMetadata.GetTriggers('', SchemaPattern, TablePattern, '') do
  begin
    while Next do
     List.Add(GetStringByName('TRIGGER_NAME'));
    Close;
  end;
end;

function TZAbstractConnection.GetURL: String;
begin
  Result := ConstructURL(FURL.UserName, FURL.Password);
end;

{**
  Returns the current version of zeosdbo.
}
function TZAbstractConnection.GetVersion: string;
begin
  Result := ZEOS_VERSION;
end;

procedure TZAbstractConnection.SetUseMetadata(AValue: Boolean);
begin
  if FUseMetaData=AValue then Exit;
  FUseMetaData:=AValue;
  if FConnection <> nil then
    FConnection.SetUseMetadata(FUseMetadata);
end;

procedure TZAbstractConnection.SetCharacterFieldType(const Value: TZControlsCodePage);
begin
  if Value <> FControlsCodePage then
    FControlsCodePage := Value;
end;

procedure TZAbstractConnection.SetRawCharacterTransliterateOptions(
  Value: TZRawCharacterTransliterateOptions);
begin
  fRawCharacterTransliterateOptions.Assign(Value);
end;

{**
  Executes the SQL statement immediately without the need of a TZQuery component
  @param SQL the statement to be executed.
  Returns an indication if execution was succesfull.
}
function TZAbstractConnection.ExecuteDirect(const SQL: String): boolean;
begin
  CheckConnected;
  DbcConnection.ExecuteImmediat(SQL, lcExecute);
  Result := True;
end;

{**
  Executes the SQL statement immediately without the need of a TZQuery component
  @param SQL the statement to be executed.
  @param RowsAffected the number of rows that were affected by the statement.
  Returns an indication if execution was succesfull.
}
function TZAbstractConnection.ExecuteDirect(const SQL: string;
  out RowsAffected: integer): boolean;
begin
  RowsAffected := -1;
  try
    CheckConnected;
    RowsAffected := DbcConnection.CreateStatement.ExecuteUpdate(SQL);
  finally
    result := (RowsAffected <> -1);
  end;
end;

{ TZRawCharacterTransliterateOptions }
constructor TZRawCharacterTransliterateOptions.Create(
  AOwner: TZAbstractConnection);
begin
  FConnection := AOwner;
  FFields := True;
end;

function TZRawCharacterTransliterateOptions.GetEncoding: TZW2A2WEncodingSource;
begin
  {$IFDEF UNICODE}
  if FParams
  then Result := FEncoding
  else Result := encDB_CP;
  {$ELSE}
  Result := FEncoding;
  {$ENDIF}
end;

function TZRawCharacterTransliterateOptions.GetRawTransliterateCodePage(
  Target: TZTransliterationType): Word;
var Transliterate: Boolean;
  ConSettings: PZConSettings;
begin
  FConnection.CheckConnected;
  ConSettings := FConnection.FConnection.GetConSettings;
  if (ConSettings.ClientCodePage.Encoding = ceUTF16) then
    Transliterate := True
  else case Target of
    ttSQL:  Transliterate := {$IFDEF UNICODE}False{$ELSE}FSQL{$ENDIF};
    ttParam: Transliterate := FParams;
    else {ttField:} Transliterate := FFields;
  end;
  if Transliterate then
    case GetEncoding of
      encDefaultSystemCodePage: Result := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF};
      encDB_CP: if ConSettings.ClientCodePage.Encoding = ceUTF16
                then Result := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}{$IFDEF LCL}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF}{$ENDIF}
                else Result := ConSettings.ClientCodePage.CP;
      else {encUTF8:} Result := zCP_UTF8;
    end
  else Result := ConSettings.ClientCodePage.CP;
end;

function TZRawCharacterTransliterateOptions.GetSQL: Boolean;
begin
{$IFNDEF UNICODE}
  if FConnection.Connected and (FConnection.DbcConnection.GetConSettings.ClientCodePage.Encoding =ceUTF16)
  then Result := True
  else Result := FSQL;
{$ELSE UNICODE}
  Result := False;
{$ENDIF UNICODE}
end;

procedure TZRawCharacterTransliterateOptions.SetEncoding(
  Value: TZW2A2WEncodingSource);
begin
  {$IFDEF UNICODE}
  if FParams
  then FEncoding := Value
  else FEncoding := encDB_CP;
  {$ENDIF}
  FEncoding := Value;
end;

{$IFNDEF UNICODE}
procedure TZRawCharacterTransliterateOptions.SetSQL(Value: Boolean);
begin
  {$IFNDEF UNICODE}
  FSQL := Value;
  {$ENDIF}
end;
{$ENDIF UNICODE}

{ TZAbstractConnectionLinkedComponent }

procedure TZAbstractConnectionLinkedComponent.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    SetConnection(nil);
end;

initialization
  SqlHourGlassLock := 0;
end.

