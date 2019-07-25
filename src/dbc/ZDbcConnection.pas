{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcConnection;

interface

{$I ZDbc.inc}

uses
{$IFDEF WITH_LCONVENCODING}
  LConvEncoding,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF TLIST_IS_DEPRECATED}ZSysUtils,{$ENDIF}
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZClasses, ZDbcIntfs, ZTokenizer, ZCompatibility, ZGenericSqlToken,
  ZGenericSqlAnalyser, ZPlainDriver, ZURL, ZCollections, ZVariant;

type

  {** Implements Abstract Database Driver. }
  TZAbstractDriver = class(TInterfacedObject, IZDriver)
  protected
    FCachedPlainDrivers: IZHashMap;
    FSupportedProtocols: TStringDynArray;
    procedure AddSupportedProtocol(const AProtocol: String);
    function AddPlainDriverToCache(const PlainDriver: IZPlainDriver; const Protocol: string = ''; const LibLocation: string = ''): String;
    function GetPlainDriverFromCache(const Protocol, LibLocation: string): IZPlainDriver;
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetSupportedProtocols: TStringDynArray;
    function GetSupportedClientCodePages(const Url: TZURL;
      Const {$IFNDEF UNICODE}AutoEncode, {$ENDIF} SupportedsOnly: Boolean;
      CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
    function Connect(const Url: string; Info: TStrings = nil): IZConnection; overload; deprecated;
    function Connect(const {%H-}Url: TZURL): IZConnection; overload; virtual;
    function AcceptsURL(const Url: string): Boolean; virtual;

    function GetPropertyInfo(const {%H-}Url: string; {%H-}Info: TStrings): TStrings; virtual;
    function GetMajorVersion: Integer; virtual;
    function GetMinorVersion: Integer; virtual;
    function GetSubVersion: Integer; virtual;
    function GetTokenizer: IZTokenizer; virtual;
    function GetStatementAnalyser: IZStatementAnalyser; virtual;
    function GetClientVersion(const {%H-}Url: string): Integer; virtual;
  end;

  {** Implements Abstract Database Connection. }

  { TZAbstractDbcConnection }

  TZAbstractDbcConnection = class(TZCodePagedObject, IZConnection,
    IImmediatelyReleasable)
  private
    FOnConnectionLostError: TOnConnectionLostError; //error handle which can be registered
    FDriver: IZDriver;
    FDriverManager: IZDriverManager; //just keep refcount high until last conection is gone e.g. Logging
    FIZPlainDriver: IZPlainDriver;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FClosed: Boolean;
    FURL: TZURL;
    FUseMetadata: Boolean;
    FClientVarManager: IZClientVariantManager;
    fRegisteredStatements: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}; //weak reference to pending stmts
    function GetHostName: string;
    procedure SetHostName(const Value: String);
    function GetPort: Integer;
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: String);
    function GetUser: string;
    procedure SetUser(const Value: String);
    function GetPassword: string;
    procedure SetPassword(const Value: String);
    function GetInfo: TStrings;
  protected
    FDisposeCodePage: Boolean;
    FChunkSize: Integer; //indicates reading / writing lobs in Chunks of x Byte
    FClientCodePage: String;
    FMetadata: TContainedObject;
    {$IFDEF ZEOS_TEST_ONLY}
    FTestMode: Byte;
    {$ENDIF}
    procedure InternalCreate; virtual; abstract;
    procedure SetDateTimeFormatProperties(DetermineFromInfo: Boolean = True);
    procedure ResetCurrentClientCodePage(const Name: String);
    function GetEncoding: TZCharEncoding;
    function GetClientVariantManager: IZClientVariantManager;
    procedure CheckCharEncoding(const CharSet: String; const DoArrange: Boolean = False);
    function GetClientCodePageInformations: PZCodePage; //EgonHugeist
    function GetAutoEncodeStrings: Boolean; //EgonHugeist
    procedure SetAutoEncodeStrings(const Value: Boolean);
    procedure OnPropertiesChange({%H-}Sender: TObject); virtual;
    procedure RaiseUnsupportedException;

    procedure RegisterOnConnectionLostErrorHandler(Handler: TOnConnectionLostError);
    procedure RegisterStatement(const Value: IZStatement);
    procedure DeregisterStatement(const Value: IZStatement);
    procedure CloseRegisteredStatements;

    function CreateRegularStatement({%H-}Info: TStrings): IZStatement;
      virtual;
    function CreatePreparedStatement(const {%H-}SQL: string; {%H-}Info: TStrings):
      IZPreparedStatement; virtual;
    function CreateCallableStatement(const {%H-}SQL: string; {%H-}Info: TStrings):
      IZCallableStatement; virtual;
    property Driver: IZDriver read FDriver write FDriver;
    property PlainDriver: IZPlainDriver read FIZPlainDriver write FIZPlainDriver;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Info: TStrings read GetInfo;
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property URL: TZURL read FURL;
    property TransactIsolationLevel: TZTransactIsolationLevel
      read FTransactIsolationLevel write FTransactIsolationLevel;
  public
    constructor Create(const {%H-}Driver: IZDriver; const Url: string;
      const {%H-}PlainDriver: IZPlainDriver; const HostName: string; Port: Integer;
      const Database: string; const User: string; const Password: string;
      Info: TStrings); overload; deprecated;
    constructor Create(const ZUrl: TZURL); overload;
    destructor Destroy; override;

    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;

    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;

    function CreateNotification(const {%H-}Event: string): IZNotification; virtual;
    function CreateSequence(const {%H-}Sequence: string; {%H-}BlockSize: Integer):
      IZSequence; virtual;

    function NativeSQL(const SQL: string): string; virtual;

    procedure SetAutoCommit(Value: Boolean); virtual;
    function GetAutoCommit: Boolean; virtual;

    procedure Commit; virtual;
    procedure Rollback; virtual;

    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const {%H-}transactionid: string);virtual;
    procedure CommitPrepared(const {%H-}transactionid: string);virtual;
    procedure RollbackPrepared(const {%H-}transactionid: string);virtual;

    //Ping Support initially for MySQL 27032006 (firmos)
    function PingServer: Integer; virtual;
    function EscapeString(const Value: RawByteString): RawByteString; overload; virtual;

    procedure Open; virtual;
    procedure Close;
    procedure InternalClose; virtual; abstract;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
    function IsClosed: Boolean; virtual;

    function GetDriver: IZDriver;
    function GetIZPlainDriver: IZPlainDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; virtual;
    function GetHostVersion: Integer; virtual;
    {END ADDED by fduenas 15-06-2006}
    function GetDescription: String;
    procedure SetReadOnly(Value: Boolean); virtual;
    function IsReadOnly: Boolean; virtual;
    function GetURL: String;

    procedure SetCatalog(const {%H-}Catalog: string); virtual;
    function GetCatalog: string; virtual;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); virtual;
    function GetTransactionIsolation: TZTransactIsolationLevel; virtual;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;
    {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; virtual;
    {$ENDIF}
    function GetBinaryEscapeString(const Value: TBytes): String; overload; virtual;
    procedure GetBinaryEscapeString(Buf: Pointer; Len: LengthInt; out Result: RawByteString); overload; virtual;
    procedure GetBinaryEscapeString(Buf: Pointer; Len: LengthInt; out Result: ZWideString); overload; virtual;

    function GetEscapeString(const Value: ZWideString): ZWideString; overload; virtual;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; virtual;
    function GetEscapeString(Buf: PAnsichar; Len: LengthInt): RawByteString; overload; virtual;

    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString); overload; virtual;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; RawCP: Word; out Result: ZWideString); overload; virtual;
    procedure GetEscapeString(Buf: PWideChar; Len: LengthInt; RawCP: Word; out Result: RawByteString); overload; virtual;
    procedure GetEscapeString(Buf: PWideChar; Len: LengthInt; out Result: ZWideString); overload; virtual;

    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean);
    function GetServerProvider: TZServerProvider; virtual;
  protected
    property Closed: Boolean read IsClosed write FClosed;
  end;

  {** Implements Abstract Database notification. }
  TZAbstractNotification = class(TInterfacedObject, IZNotification)
  private
    FEventName: string;
    FConnection: IZConnection;
  protected
    property EventName: string read FEventName write FEventName;
    property Connection: IZConnection read FConnection write FConnection;
  public
    constructor Create(const Connection: IZConnection; const EventName: string);
    function GetEvent: string;
    procedure Listen; virtual;
    procedure Unlisten; virtual;
    procedure DoNotify; virtual;
    function CheckEvents: string; virtual;

    function GetConnection: IZConnection; virtual;
  end;

  {** Implements Abstract Sequence generator. }
  TZAbstractSequence = class(TInterfacedObject, IZSequence)
  private
    FName: string;
    FBlockSize: Integer;
    FConnection: IZConnection;
  protected
    function GetName: string; virtual;
    function GetBlockSize: Integer; virtual;
    procedure SetName(const Value: string); virtual;
    procedure SetBlockSize(const Value: Integer); virtual;
    property Connection: IZConnection read FConnection write FConnection;
  public
    constructor Create(const Connection: IZConnection; const Name: string;
      BlockSize: Integer);

    function GetCurrentValue: Int64; virtual;
    function GetNextValue: Int64; virtual;

    function GetCurrentValueSQL: string; virtual; abstract;
    function GetNextValueSQL: string; virtual; abstract;

    function GetConnection: IZConnection; virtual;

    property Name: string read GetName write SetName;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
  end;

implementation

uses ZMessages,{$IFNDEF TLIST_IS_DEPRECATED}ZSysUtils, {$ENDIF}
  ZDbcMetadata, ZDbcUtils, ZEncoding, ZConnProperties, StrUtils,
  ZDbcProperties, {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF}
  {$IFDEF WITH_INLINE},ZFastCode{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

{ TZAbstractDriver }

{**
  Constructs this object with default properties.
}
constructor TZAbstractDriver.Create;
begin
  FCachedPlainDrivers := TZHashMap.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractDriver.Destroy;
begin
  FCachedPlainDrivers.Clear;
  FCachedPlainDrivers := nil;
  inherited Destroy;
end;

function TZAbstractDriver.GetSupportedProtocols: TStringDynArray;
begin
  Result := FSupportedProtocols;
end;

{**
  EgonHugeist:
  Get names of the supported CharacterSets.
  For example: ASCII, UTF8...
}
function TZAbstractDriver.GetSupportedClientCodePages(const Url: TZURL;
  Const {$IFNDEF UNICODE}AutoEncode,{$ENDIF} SupportedsOnly: Boolean;
  CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
var
  Plain: IZPlainDriver;
begin
  Plain := GetPlainDriverFromCache(Url.Protocol, '');
  if Assigned(Plain) then
  Result := Plain.GetSupportedClientCodePages({$IFNDEF UNICODE}AutoEncode,{$ENDIF}
    not SupportedsOnly, CtrlsCPType);
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
function TZAbstractDriver.Connect(const Url: string; Info: TStrings): IZConnection;
var
  TempURL:  TZURL;
begin
  TempURL := TZURL.Create(Url, Info);
  try
    Result := Connect(TempURL);
  finally
    TempUrl.Free;
  end;
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

  @param url the TZURL of the database to which to connect
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZAbstractDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := nil;
end;

{**
  Returns true if the driver thinks that it can open a connection
  to the given URL.  Typically drivers will return true if they
  understand the subprotocol specified in the URL and false if
  they don't.
  @param url the URL of the database
  @return true if this driver can connect to the given URL
}
function TZAbstractDriver.AcceptsURL(const Url: string): Boolean;
var
  I: Integer;
  Protocols: TStringDynArray;
begin
  Result := False;
  Protocols := GetSupportedProtocols;
  for I := Low(Protocols) to High(Protocols) do
  begin
    Result := StartsWith(LowerCase(Url), Format('zdbc:%s:', [LowerCase(Protocols[I])]));
    if Result then
      Break;
  end;
end;

procedure TZAbstractDriver.AddSupportedProtocol(const AProtocol: String);
begin
  SetLength(FSupportedProtocols, Length(FSupportedProtocols)+1);
  FSupportedProtocols[High(FSupportedProtocols)] := AProtocol;
end;

function TZAbstractDriver.AddPlainDriverToCache(const PlainDriver: IZPlainDriver;
  const Protocol: string = ''; const LibLocation: string = ''): String;
var
  TempKey: IZAnyValue;
begin
  if Protocol = '' then
  begin
    Result := PlainDriver.GetProtocol;
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(PlainDriver.GetProtocol))
  end
  else
  begin
    Result := Protocol;
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol+LibLocation));
  end;
  FCachedPlainDrivers.Put(TempKey, PlainDriver);
end;

function TZAbstractDriver.GetPlainDriverFromCache(const Protocol, LibLocation: string): IZPlainDriver;
var
  TempKey: IZAnyValue;
  TempPlain: IZPlainDriver;
begin
  TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol+LibLocation));
  Result := FCachedPlainDrivers.Get(TempKey) as IZPlainDriver;
  if Result = nil then
  begin
    TempKey := TZAnyValue.CreateWithString(AnsiLowerCase(Protocol));
    TempPlain := FCachedPlainDrivers.Get(TempKey) as IZPlainDriver;
    if Assigned(TempPlain) then
    begin
      Result := TempPlain.Clone;
      AddPlainDriverToCache(Result, Protocol, LibLocation);
    end;
  end;
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected plaindriver.
}
function TZAbstractDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean): IZPlainDriver;
begin
  Result := GetPlainDriverFromCache(Url.Protocol, Url.LibLocation);
  if Assigned(Result) and InitDriver then begin
    GlobalCriticalSection.Enter;
    try
      Result.Initialize(Url.LibLocation);
    finally
      GlobalCriticalSection.Leave;
    end;
  end;
end;

{**
  Gets information about the possible properties for this driver.
  <p>The getPropertyInfo method is intended to allow a generic GUI tool to
  discover what properties it should prompt a human for in order to get
  enough information to connect to a database.  Note that depending on
  the values the human has supplied so far, additional values may become
  necessary, so it may be necessary to iterate though several calls
  to getPropertyInfo.

  @param url the URL of the database to which to connect
  @param info a proposed list of tag/value pairs that will be sent on
    connect open
  @return an array of DriverPropertyInfo objects describing possible
    properties.  This array may be an empty array if no properties
    are required.
}
function TZAbstractDriver.GetPropertyInfo(const Url: string; Info: TStrings): TStrings;
begin
  Result := nil;
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZAbstractDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZAbstractDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the driver's sub version (revision) number. Initially this should be 0.
  @return this driver's sub version number
}
function TZAbstractDriver.GetSubVersion: Integer;
begin
 Result := 0;
end;
{**
  Creates a generic statement analyser object.
  @returns a generic statement analyser object.
}
function TZAbstractDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZGenericStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{**
  Creates a generic tokenizer object.
  @returns a created generic tokenizer object.
}
function TZAbstractDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZGenericSQLTokenizer.Create;
end;

{**
  Returns the version of the plain driver library that will be used to open a connection
  to the given URL.
  @param url the URL of the database
  @return the version number of the plain driver library for the give URL
}
function TZAbstractDriver.GetClientVersion(const Url: string): Integer;
begin
  Result := 0;
end;

{ TZAbstractDbcConnection }

function TZAbstractDbcConnection.GetHostName: string;
begin
  Result := FURL.HostName;
end;

procedure TZAbstractDbcConnection.SetHostName(const Value: String);
begin
  FURL.HostName := Value;
end;

function TZAbstractDbcConnection.GetPort: Integer;
begin
  Result := FURL.Port;
end;

function TZAbstractDbcConnection.GetServerProvider: TZServerProvider;
begin
  Result := spUnknown;
end;

procedure TZAbstractDbcConnection.SetConnPort(const Value: Integer);
begin
  FURL.Port := Value;
end;

function TZAbstractDbcConnection.GetDatabase: string;
begin
  Result := FURL.Database;
end;

procedure TZAbstractDbcConnection.SetDatabase(const Value: String);
begin
  FURL.Database := Value;
end;

function TZAbstractDbcConnection.GetUser: string;
begin
  Result := FURL.UserName;
end;

procedure TZAbstractDbcConnection.SetUser(const Value: String);
begin
  FURL.UserName := Value;
end;

function TZAbstractDbcConnection.GetPassword: string;
begin
  Result := FURL.Password;
end;

procedure TZAbstractDbcConnection.SetPassword(const Value: String);
begin
  FURL.Password := Value;
end;

function TZAbstractDbcConnection.GetInfo: TStrings;
begin
  Result := FURL.Properties;
end;

procedure TZAbstractDbcConnection.SetDateTimeFormatProperties(DetermineFromInfo: Boolean);

  procedure SetNotEmptyFormat(const FmtFromValues, FmtDefault: string; out ResultFmt: string);
  begin
    if FmtFromValues = '' then
      ResultFmt := FmtDefault
    else
      ResultFmt := UpperCase(FmtFromValues);
  end;

begin
  if DetermineFromInfo then begin
    {date formats}
    SetNotEmptyFormat(Info.Values[ConnProps_DateWriteFormat],
      DefDateFormatYMD,
      ConSettings^.WriteFormatSettings.DateFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateReadFormat],
      DefDateFormatYMD,
      ConSettings^.ReadFormatSettings.DateFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateDisplayFormat],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat,
      ConSettings^.DisplayFormatSettings.DateFormat);

    {time formats}
    SetNotEmptyFormat(Info.Values[ConnProps_TimeWriteFormat],
      IfThen(GetMetaData.GetDatabaseInfo.SupportsMilliseconds, DefTimeFormatMsecs, DefTimeFormat),
      ConSettings^.WriteFormatSettings.TimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_TimeReadFormat],
      IfThen(GetMetaData.GetDatabaseInfo.SupportsMilliseconds, DefTimeFormatMsecs, DefTimeFormat),
      ConSettings^.ReadFormatSettings.TimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_TimeDisplayFormat],
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat,
      ConSettings^.DisplayFormatSettings.TimeFormat);

    {timestamp formats}
    SetNotEmptyFormat(Info.Values[ConnProps_DateTimeWriteFormat],
      ConSettings^.WriteFormatSettings.DateFormat+' '+ConSettings^.WriteFormatSettings.TimeFormat,
      ConSettings^.WriteFormatSettings.DateTimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateTimeReadFormat],
      ConSettings^.ReadFormatSettings.DateFormat+' '+ConSettings^.ReadFormatSettings.TimeFormat,
      ConSettings^.ReadFormatSettings.DateTimeFormat);

    SetNotEmptyFormat(Info.Values[ConnProps_DateTimeDisplayFormat],
      ConSettings^.DisplayFormatSettings.DateFormat+' '+ConSettings^.DisplayFormatSettings.TimeFormat,
      ConSettings^.DisplayFormatSettings.DateTimeFormat);
  end;

  ConSettings^.WriteFormatSettings.DateFormatLen := Length(ConSettings^.WriteFormatSettings.DateFormat);
  ConSettings^.ReadFormatSettings.DateFormatLen := Length(ConSettings^.ReadFormatSettings.DateFormat);
  ConSettings^.DisplayFormatSettings.DateFormatLen := Length(ConSettings^.DisplayFormatSettings.DateFormat);

  ConSettings^.WriteFormatSettings.TimeFormatLen := Length(ConSettings^.WriteFormatSettings.TimeFormat);
  ConSettings^.ReadFormatSettings.TimeFormatLen := Length(ConSettings^.ReadFormatSettings.TimeFormat);
  ConSettings^.DisplayFormatSettings.TimeFormatLen := Length(ConSettings^.DisplayFormatSettings.TimeFormat);

  ConSettings^.WriteFormatSettings.DateTimeFormatLen := Length(ConSettings^.WriteFormatSettings.DateTimeFormat);
  ConSettings^.ReadFormatSettings.DateTimeFormatLen := Length(ConSettings^.ReadFormatSettings.DateTimeFormat);
  ConSettings^.DisplayFormatSettings.DateTimeFormatLen := Length(ConSettings^.DisplayFormatSettings.DateTimeFormat);
end;

procedure TZAbstractDbcConnection.RegisterOnConnectionLostErrorHandler(
  Handler: TOnConnectionLostError);
begin
  if Assigned(FOnConnectionLostError) then
    FOnConnectionLostError := Handler
  else if Assigned(FOnConnectionLostError) then
    raise EZSQLException.Create('Error handler registered already!')
  else FOnConnectionLostError := Handler;
end;

procedure TZAbstractDbcConnection.RegisterStatement(
  const Value: IZStatement);
begin
  if fRegisteredStatements.IndexOf(Pointer(Value)) = -1 then
    fRegisteredStatements.Add(Pointer(Value))
end;

procedure TZAbstractDbcConnection.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var I: Integer;
  ImmediatelyReleasable: IImmediatelyReleasable;
  FError: EZSQLConnectionLost;
begin
  FAutoCommit := True;
  if not Closed and Assigned(FOnConnectionLostError) and Assigned(AError) then begin
    FError := AError;
    AError := nil;
  end else
    FError := nil; //satisfy compiler
  fClosed := True;
  for I := fRegisteredStatements.Count-1 downto 0 do
    If Supports(IZStatement(fRegisteredStatements[I]), IImmediatelyReleasable, ImmediatelyReleasable)
      and (Sender <> ImmediatelyReleasable) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
  if Assigned(FOnConnectionLostError) and (FError <> nil) then
    FOnConnectionLostError(FError);
end;

procedure TZAbstractDbcConnection.ResetCurrentClientCodePage(const Name: String);
var NewCP, tmp: PZCodePage;
begin
  FDisposeCodePage := True;
  Tmp := ConSettings^.ClientCodePage;
  ConSettings^.ClientCodePage := New(PZCodePage);
  NewCP := GetIZPlainDriver.ValidateCharEncoding(Name);
  ConSettings^.ClientCodePage^.Name := Tmp^.Name;
  ConSettings^.ClientCodePage^.ID := Tmp^.ID;
  ConSettings^.ClientCodePage^.CharWidth := Tmp^.CharWidth;
  ConSettings^.ClientCodePage^.Encoding := NewCP^.Encoding;
  ConSettings^.ClientCodePage^.CP := NewCP^.CP;
  ConSettings^.ClientCodePage^.ZAlias := '';
  ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := Tmp^.IsStringFieldCPConsistent;
  {Also reset the MetaData ConSettings}
  (FMetadata as TZAbstractDatabaseMetadata).ConSettings := ConSettings;
  {$IFDEF WITH_LCONVENCODING}
  SetConvertFunctions(ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP,
    ConSettings^.PlainConvertFunc, ConSettings^.DbcConvertFunc);
  {$ENDIF}
  ZEncoding.SetConvertFunctions(ConSettings);
  FClientVarManager := TZClientVariantManager.Create(ConSettings);
end;

function TZAbstractDbcConnection.GetEncoding: TZCharEncoding;
begin
  Result := ConSettings.ClientCodePage^.Encoding;
end;

function TZAbstractDbcConnection.GetEscapeString(Buf: PAnsichar;
  Len: LengthInt): RawByteString;
begin
  Result := SQLQuotedStr(Buf, Len, AnsiChar(#39));
end;

procedure TZAbstractDbcConnection.GetEscapeString(Buf: PAnsichar; Len: LengthInt;
  out Result: RawByteString);
begin
  Result := SQLQuotedStr(Buf, Len, AnsiChar(#39));
end;

procedure TZAbstractDbcConnection.GetEscapeString(Buf: PAnsichar; Len: LengthInt;
  RawCP: Word; out Result: ZWideString);
var RawTmp: RawByteString;
begin
  GetEscapeString(Buf, Len, RawTmp);
  Result := ZRawToUnicode(RawTmp, RawCP);
end;

function TZAbstractDbcConnection.GetClientVariantManager: IZClientVariantManager;
begin
  Result := TZClientVariantManager.Create(ConSettings);
end;

{**
  EgonHugeist: Check if the given Charset for Compiler/Database-Support!!
    Not supported means if there is a possible String-DataLoss.
    So it raises an Exception if case of settings. This handling
    is an improofment to inform Zeos-Users about the troubles the given
    CharacterSet may have.
  @param CharSet the CharacterSet which has to be proofed
  @param DoArrange represents a switch to check and set a aternative ZAlias as
    default. This means it ignores the choosen Client-CharacterSet and sets a
    "more" Zeos-Compatible Client-CharacterSet if known.
}
procedure TZAbstractDbcConnection.CheckCharEncoding(const CharSet: String;
  const DoArrange: Boolean = False);
begin
  ConSettings.ClientCodePage := GetIZPlainDriver.ValidateCharEncoding(CharSet, DoArrange);
  FClientCodePage := ConSettings.ClientCodePage^.Name; //resets the developer choosen ClientCodePage
  {$IFDEF WITH_LCONVENCODING}
  SetConvertFunctions(ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP,
    ConSettings^.PlainConvertFunc, ConSettings^.DbcConvertFunc);
  {$ENDIF}
  ZEncoding.SetConvertFunctions(ConSettings);
  FClientVarManager := TZClientVariantManager.Create(ConSettings);
end;


{**
  EgonHugeist: this is a compatibility-Option for exiting Applictions.
    Zeos is now able to preprepare direct insered SQL-Statements.
    Means do the UTF8-preparation if the CharacterSet was choosen.
    So we do not need to do the SQLString + UTF8Encode(Edit1.Test) for example.
  @result if AutoEncodeStrings should be used
}
function TZAbstractDbcConnection.GetAutoEncodeStrings: Boolean;
begin
  {$IFDEF UNICODE}
  Result := True;
  {$ELSE}
  Result := ConSettings.AutoEncode;
  {$ENDIF}
end;

procedure TZAbstractDbcConnection.GetBinaryEscapeString(Buf: Pointer;
  Len: LengthInt; out Result: ZWideString);
begin
  Result := GetSQLHexWideString(Buf, Len);
end;

procedure TZAbstractDbcConnection.GetBinaryEscapeString(Buf: Pointer;
  Len: LengthInt; out Result: RawByteString);
begin
  Result := GetSQLHexAnsiString(Buf, Len);
end;

procedure TZAbstractDbcConnection.SetAutoEncodeStrings(const Value: Boolean);
begin
  {$IFNDEF UNICODE}
  ConSettings.AutoEncode := Value;
  {$ENDIF}
end;

{**
  EgonHugeist and MDeams: The old deprecadet constructor which was used
  from the descendant classes. We left him here for compatibility reasons to
  exesting projects which using the DbcConnections directly

  Constructs this object and assignes the main properties.
  @param Driver the parent ZDBC driver.
  @param Url a connection URL.
  @param PlainDriver a versioned ZPlainDriver object interface.
  @param HostName a name of the host.
  @param Port a port number (0 for default port).
  @param Database a name pof the database.
  @param User a user name.
  @param Password a user password.
  @param Info a string list with extra connection parameters.
}
constructor TZAbstractDbcConnection.Create(const Driver: IZDriver; const Url: string;
  const PlainDriver: IZPlainDriver;
  const HostName: string; Port: Integer; const Database: string;
  const User: string; const Password: string; Info: TStrings);
var
  TempURL: TZURL;
begin
  TempURL := TZURL.Create(Url, HostName, Port, Database, User, Password, Info);
  Create(TempURL);
  TempURL.Free;
end;

{**
  Constructs this object and assignes the main properties.
  @param Url a connection ZURL-class which exports all connection parameters.
}
constructor TZAbstractDbcConnection.Create(const ZUrl: TZURL);
begin
  FClosed := True;
  FDisposeCodePage := False;
  if not assigned(ZUrl) then
    raise Exception.Create('ZUrl is not assigned!')
  else
    FURL := TZURL.Create();
  FDriverManager := DriverManager; //just keep refcount high
  FDriver := DriverManager.GetDriver(ZURL.URL);
  FIZPlainDriver := FDriver.GetPlainDriver(ZUrl);
  fRegisteredStatements := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FURL.OnPropertiesChange := OnPropertiesChange;
  FURL.URL := ZUrl.URL;

  FClientCodePage := Info.Values[ConnProps_CodePage];
  {CheckCharEncoding}
  ConSettings := New(PZConSettings);

  SetConSettingsFromInfo(Info);
  CheckCharEncoding(FClientCodePage, True);
  FAutoCommit := True;
  FReadOnly := False; //EH: Changed! We definitelly did newer ever open a ReadOnly connection by default!
  FTransactIsolationLevel := tiNone;
  FUseMetadata := True;
  // should be set BEFORE InternalCreate
  ConSettings^.Protocol := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FIZPlainDriver.GetProtocol);
  ConSettings^.Database := ConSettings^.ConvFuncs.ZStringToRaw(FURL.Database, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  ConSettings^.User := ConSettings^.ConvFuncs.ZStringToRaw(FURL.UserName, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  FChunkSize := StrToIntDef(Info.Values[DSProps_ChunkSize], 4096);
  // now InternalCreate will work, since it will try to Open the connection
  InternalCreate;

  {$IFDEF ZEOS_TEST_ONLY}
  FTestMode := 0;
  {$ENDIF}
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractDbcConnection.Destroy;
begin
  if not FClosed then
    Close;
  FreeAndNil(FMetadata);
  FreeAndNil(FURL);
  FreeAndNil(fRegisteredStatements);
  FIZPlainDriver := nil;
  FDriver := nil;
  if Assigned(ConSettings) then begin
    Dispose(ConSettings);
    ConSettings := nil;
  end;
  FClientVarManager := nil;
  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAbstractDbcConnection.Open;
begin
  FClosed := False;
  SetDateTimeFormatProperties;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractDbcConnection.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
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

  @return a new Statement object
}
function TZAbstractDbcConnection.CreateStatement: IZStatement;
begin
  Result := CreateStatementWithParams(nil);
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
function TZAbstractDbcConnection.CreateStatementWithParams(Info: TStrings):
  IZStatement;
var UsedInfo: TStrings;
begin
  UsedInfo := Info;
  If StrToBoolEx(GetInfo.Values[DSProps_PreferPrepared]) then
    If UsedInfo = nil then
    begin
      UsedInfo := TStringList.Create;
      UsedInfo.Values[DSProps_PreferPrepared] := 'true';
    end;
  Result := CreateRegularStatement(Info);
  if UsedInfo <> Info then UsedInfo.Free;
end;

{**
  Creates a regular statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractDbcConnection.CreateRegularStatement(
  Info: TStrings): IZStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
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
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZAbstractDbcConnection.PrepareStatement(const SQL: string): IZPreparedStatement;
begin
  Result := CreatePreparedStatement(SQL, nil);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  @param SQL a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZAbstractDbcConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
var UsedInfo: TStrings;
begin
  UsedInfo := Info;
  If StrToBoolEx(GetInfo.Values[DSProps_PreferPrepared]) then
    If UsedInfo = nil then
    begin
      UsedInfo := TStringList.Create;
      UsedInfo.Values[DSProps_PreferPrepared] := 'true';
    end;
  Result := CreatePreparedStatement(SQL, UsedInfo);
  if UsedInfo <> Info then UsedInfo.Free;
end;

procedure TZAbstractDbcConnection.PrepareTransaction(const transactionid: string);
begin
  RaiseUnsupportedException;
end;

{**
  Creates a prepared statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractDbcConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
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

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}

function TZAbstractDbcConnection.PrepareCall(
  const SQL: string): IZCallableStatement;
begin
  Result := CreateCallableStatement(SQL, nil);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  @param SQL a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZAbstractDbcConnection.PrepareCallWithParams(const SQL: string;
  Info: TStrings): IZCallableStatement;
var UsedInfo: TStrings;
begin
  UsedInfo := Info;
  If StrToBoolEx(GetInfo.Values[DSProps_PreferPrepared]) then
    If UsedInfo = nil then
    begin
      UsedInfo := TStringList.Create;
      UsedInfo.Values[DSProps_PreferPrepared] := 'true';
    end;
  Result := CreateCallableStatement(SQL, UsedInfo);
  if UsedInfo <> Info then UsedInfo.Free;
end;

{**
  Creates a callable statement object.
  @param SQL a SQL query string.
  @param Info a statement parameters.
  @returns a created statement.
}
function TZAbstractDbcConnection.CreateCallableStatement(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

{**
  Creates an object to send/recieve notifications from SQL server.
  @param Event an event name.
  @returns a created notification object.
}
function TZAbstractDbcConnection.CreateNotification(const Event: string): IZNotification;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZAbstractDbcConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := nil;
  RaiseUnsupportedException;
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
function TZAbstractDbcConnection.NativeSQL(const SQL: string): string;
begin
  Result := SQL;
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
procedure TZAbstractDbcConnection.SetAutoCommit(Value: Boolean);
begin
  FAutoCommit := Value;
end;

{**
  Gets the current auto-commit state.
  @return the current state of auto-commit mode
  @see #setAutoCommit
}
function TZAbstractDbcConnection.GetAutoCommit: Boolean;
begin
  Result := FAutoCommit;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZAbstractDbcConnection.Commit;
begin
  RaiseUnsupportedException;
end;

procedure TZAbstractDbcConnection.CommitPrepared(const transactionid: string);
begin
  RaiseUnsupportedException;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZAbstractDbcConnection.Rollback;
begin
  RaiseUnsupportedException;
end;

procedure TZAbstractDbcConnection.RollbackPrepared(const transactionid: string);
begin
  RaiseUnsupportedException;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZAbstractDbcConnection.PingServer: Integer;
begin
  Result := 1;
  RaiseUnsupportedException;
end;

{**
  Escape a string so it's acceptable for the Connection's server.
  @param value string that should be escaped
  @return Escaped string
}
function TZAbstractDbcConnection.EscapeString(const Value : RawByteString) : RawByteString;
begin
  Result := EncodeCString(Value);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}

procedure TZAbstractDbcConnection.Close;
var RefCountAdded: Boolean;
begin
  //while killing pending statements which keep the Connection.RefCount greater than 0
  //we need to take care about calling Destroy which calls Close again.
  DriverManager.ClearGarbageCollector;
  if RefCount > 0 then begin //manual close called
    RefCountAdded := True;
    _AddRef;
  end else
    RefCountAdded := False; //destructor did call close;
  try
    try
      CloseRegisteredStatements;
    finally
      InternalClose;
    end;
  finally
    FClosed := True;
    if FDisposeCodePage then
    begin
      Dispose(ConSettings^.ClientCodePage);
      ConSettings^.ClientCodePage := nil;
      FDisposeCodePage := False;
    end;
    if RefCountAdded then
      _Release; //destructor will call close again
  end;
end;

procedure TZAbstractDbcConnection.CloseRegisteredStatements;
var I: Integer;
begin
  for i := fRegisteredStatements.Count-1 downto 0 do begin
    //try
      IZStatement(fRegisteredStatements[i]).Close;
    //except end;
  end;
end;

{**
  Tests to see if a Connection is closed.
  @return true if the connection is closed; false if it's still open
}
function TZAbstractDbcConnection.IsClosed: Boolean;
begin
  Result := FClosed;
  DriverManager.ClearGarbageCollector;
end;

{**
  Gets the parent ZDBC driver.
  @returns the parent ZDBC driver interface.
}
function TZAbstractDbcConnection.GetDriver: IZDriver;
begin
  Result := FDriver;
end;

{**
  Gets the plain driver.
  @returns the plain driver interface.
}
function TZAbstractDbcConnection.GetIZPlainDriver: IZPlainDriver;
begin
  result := FIZPlainDriver;
end;

{**
  Gets the metadata regarding this connection's database.
  A Connection's database is able to provide information
  describing its tables, its supported SQL grammar, its stored
  procedures, the capabilities of this connection, and so on. This
  information is made available through a DatabaseMetaData
  object.

  @return a DatabaseMetaData object for this Connection
}
function TZAbstractDbcConnection.GetMetadata: IZDatabaseMetadata;
begin
  if Closed then
    Open;
  Result := FMetadata as IZDatabaseMetadata;
end;

{**
  Gets a connection parameters.
  @returns a list with connection parameters.
}
function TZAbstractDbcConnection.GetParameters: TStrings;
begin
  Result := Info;
end;

{**
  Gets the client's full version number. Initially this should be 0.
  The format of the version resturned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZAbstractDbcConnection.GetClientVersion: Integer;
begin
 Result := 0;
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZAbstractDbcConnection.GetHostVersion: Integer;
begin
 Result := 0;
end;

function TZAbstractDbcConnection.GetDescription: String;
begin
  Result := PlainDriver.GetDescription;
end;

{END ADDED by fduenas 15-06-2006}

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZAbstractDbcConnection.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

{**
  Tests to see if the connection is in read-only mode.
  @return true if connection is read-only and false otherwise
}
function TZAbstractDbcConnection.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

{**
  get current connection URL from TZURL. Nice to clone the connection by using
  the IZDriverManager
  @return true if connection is read-only and false otherwise
}
function TZAbstractDbcConnection.GetURL: String;
begin
  Result := FURL.URL
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZAbstractDbcConnection.SetCatalog(const Catalog: string);
begin
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAbstractDbcConnection.GetCatalog: string;
begin
  Result := '';
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZAbstractDbcConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  FTransactIsolationLevel := Level;
end;

{**
  Gets this Connection's current transaction isolation level.
  @return the current TRANSACTION_* mode value
}
function TZAbstractDbcConnection.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := FTransactIsolationLevel;
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZAbstractDbcConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZAbstractDbcConnection.ClearWarnings;
begin
end;

procedure TZAbstractDbcConnection.DeregisterStatement(
  const Value: IZStatement);
var
  I: Integer;
begin
  I := fRegisteredStatements.IndexOf(Pointer(Value));
  if I > -1 then fRegisteredStatements.Delete(I);
end;

function TZAbstractDbcConnection.UseMetadata: boolean;
begin
  result := FUseMetadata;
end;

procedure TZAbstractDbcConnection.SetUseMetadata(Value: Boolean);
begin
  FUseMetadata := Value;
end;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
function TZAbstractDbcConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  GetBinaryEscapeString(Pointer(Value), Length(Value), {$IFNDEF UNICODE}RawByteString{$ELSE}ZWideString{$ENDIF}(Result));
end;
{$ENDIF}

function TZAbstractDbcConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  GetBinaryEscapeString(Pointer(Value), Length(Value), {$IFNDEF UNICODE}RawByteString{$ELSE}ZWideString{$ENDIF}(Result));
end;

function TZAbstractDbcConnection.GetEscapeString(const Value: ZWideString): ZWideString;
begin
  GetEscapeString(Pointer(Value), Length(Value), Result);
end;

function TZAbstractDbcConnection.GetEscapeString(const Value: RawByteString): RawByteString;
var P: PAnsiChar;
begin
  P := Pointer(Value);
  if (P <> nil) and (Length(Value)>1) and (AnsiChar(P^)=AnsiChar(#39)) and (AnsiChar((P+Length(Value)-1)^)=AnsiChar(#39))
  then Result := Value
  else GetEscapeString(P, Length(Value), Result);
end;

{**
  Result 100% Compiler-Compatible
  And sets it Result to ClientCodePage by calling the
    PlainDriver.GetClientCodePageInformations function

  @param ClientCharacterSet the CharacterSet which has to be checked
  @result PZCodePage see ZCompatible.pas
}
function TZAbstractDbcConnection.GetClientCodePageInformations: PZCodePage; //EgonHugeist
begin
  Result := ConSettings.ClientCodePage
end;

procedure TZAbstractDbcConnection.OnPropertiesChange(Sender: TObject);
begin
  // do nothing in base class
end;

{ TZAbstractNotification }

{**
  Creates this object and assignes the main properties.
  @param Connection a database connection object.
  @param EventName a name of the SQL event.
}
constructor TZAbstractNotification.Create(const Connection: IZConnection;
  const EventName: string);
begin
  FConnection := Connection;
  FEventName := EventName;
end;

{**
  Gets an event name.
  @return an event name for this notification.
}
function TZAbstractNotification.GetEvent: string;
begin
  Result := FEventName;
end;

{**
  Sets a listener to the specified event.
}
procedure TZAbstractNotification.Listen;
begin
end;

{**
  Removes a listener to the specified event.
}
procedure TZAbstractNotification.Unlisten;
begin
end;

{**
  Checks for any pending events.
  @return a string with incoming events??
}
function TZAbstractNotification.CheckEvents: string;
begin
  Result := '';
end;

{**
  Sends a notification string.
}
procedure TZAbstractNotification.DoNotify;
begin
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractNotification.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{ TZAbstractSequence }

{**
  Creates this sequence object.
  @param Connection an SQL connection interface.
  @param Name a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to server.
}
constructor TZAbstractSequence.Create(const Connection: IZConnection;
  const Name: string; BlockSize: Integer);
begin
  FConnection := Connection;
  FName := Name;
  FBlockSize := BlockSize;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractSequence.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{**
  Returns a name of the sequence generator.
  @return a name of this sequence generator.
}
function TZAbstractSequence.GetName: string;
begin
  Result := FName;
end;

{**
  Returns the assigned block size for this sequence.
  @return the assigned block size.
}
function TZAbstractSequence.GetBlockSize: Integer;
begin
  Result := FBlockSize;
end;

{**
  Gets the current unique key generated by this sequence.
  @param the last generated unique key.
}
function TZAbstractSequence.GetCurrentValue: Int64;
begin
  Result := 0;
end;

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
function TZAbstractSequence.GetNextValue: Int64;
begin
  Result := 0;
end;

{**
  Sets the block size for this sequence.
  @param Value the block size.
}
procedure TZAbstractSequence.SetBlockSize(const Value: Integer);
begin
  FBlockSize := Value;
end;

{**
  Sets a name of the sequence generator.
  @param Value a name of this sequence generator.
}
procedure TZAbstractSequence.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TZAbstractDbcConnection.GetEscapeString(Buf: PWideChar; Len: LengthInt;
  RawCP: Word; out Result: RawByteString);
var RawTemp: RawByteString;
begin
  RawTemp := PUnicodeToRaw(Buf, Len, RawCP);
  GetEscapeString(Pointer(RawTemp), Length(RawTemp), Result);
end;

procedure TZAbstractDbcConnection.GetEscapeString(Buf: PWideChar; Len: LengthInt;
  out Result: ZWideString);
var RawTemp: RawByteString;
begin
  if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
    Result := SQLQuotedStr(Buf, Len, WideChar(#39))
  else begin
    RawTemp := PUnicodeToRaw(Buf, Len, ConSettings^.ClientCodePage^.CP);
    GetEscapeString(Pointer(RawTemp), Length(RawTemp), ConSettings^.ClientCodePage^.CP, Result);
  end;
end;

end.
