{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainMySqlDriver, ZPlainDriver,
  ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser,
  ZClasses;
type

  {** Implements MySQL Database Driver. }

  { TZMySQLDriver }
  TZMySQLDriver = class(TZAbstractDriver)
  private
    FServerArgs: array of PAnsiChar;
    FServerArgsRaw: array of RawByteString;
  protected
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; override;
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
    function GetClientVersion(const Url: string): Integer; override;
  end;

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']
    function GetConnectionHandle: PPMYSQL;
    function GetDatabaseName: String;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
    function GetPlainDriver: TZMySQLPlainDriver;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString);
  end;

  {** Implements MySQL Database Connection. }
  TZMySQLConnection = class(TZAbstractDbcConnection, IZConnection,
    IZMySQLConnection, IZTransaction)
  private
    FCatalog: string;
    FHandle: PMySQL;
    FDatabaseName: String;
    FIKnowMyDatabaseName, FMySQL_FieldType_Bit_1_IsBoolean,
    FSupportsBitType, FSupportsReadOnly: Boolean;
    FPlainDriver: TZMySQLPlainDriver;
    FSavePoints: TStrings;
  protected
    procedure InternalCreate; override;
    procedure InternalClose; override;
    procedure ExecuteImmediat(const SQL: RawByteString; LoggingCategory: TZLoggingCategory); override;
  public
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const Name: String; Info: TStrings):
      IZCallableStatement;

    procedure Commit;
    procedure Rollback;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetAutoCommit(Value: Boolean); override;
    function StartTransaction: Integer;

    destructor Destroy; override;
    procedure SetReadOnly(Value: Boolean); override;

    function PingServer: Integer; override;
    function AbortOperation: Integer; override;

    procedure Open; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    {END ADDED by fduenas 15-06-2006}
    function GetConnectionHandle: PPMYSQL;
    function EscapeString(const Value: RawByteString): RawByteString; overload; override;
    function GetEscapeString(const Value: ZWideString): ZWideString; overload; override;
    procedure GetEscapeString(Buf: PAnsichar; Len: LengthInt; out Result: RawByteString); overload;

    function GetDatabaseName: String;
    function GetServerProvider: TZServerProvider; override;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
    function GetPlainDriver: TZMySQLPlainDriver;
  end;

var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  ZMessages, ZSysUtils, ZDbcMySqlStatement, ZMySqlToken, ZFastCode,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser, TypInfo,
  ZEncoding, ZDbcProperties, ZDbcUtils,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF};

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLPlainDriver.Create));
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
function TZMySQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZMySQLConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZMySQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZMySQLDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZMySQLDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZMySQLTokenizer.Create;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZMySQLStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected plaindriver.
}
function TZMySQLDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean = True): IZPlainDriver;
var I: Integer;
  PlainDriver: TZMySQLPLainDriver;
  TmpList: TStrings;
  ErrorNo: Integer;
begin
  //if libLocation is not in cache the driver will return a clone
  if URL.Properties.Values[ConnProps_Library] <> '' then
    URL.LibLocation := URL.Properties.Values[ConnProps_Library];

  // added by tohenk, 2009-10-11
  // before PlainDriver is initialized, we can perform pre-library loading
  // requirement check here, e.g. Embedded server argument params
  Result := inherited GetPlainDriver(URL, True);
  if Assigned(Result) then begin
    PlainDriver := TZMySQLPLainDriver(Result.GetInstance);
    if (Assigned(PlainDriver.mysql_server_init) or Assigned(PlainDriver.mysql_library_init)) and
      not PlainDriver.IsInitialized and InitDriver and (Url.Properties.Count >0) then begin
      GlobalCriticalSection.Enter;
      TmpList := TStringList.Create;
      try
        if Url.Properties.Values[ConnProps_Datadir] = ''
        then TmpList.Add(EMBEDDED_DEFAULT_DATA_DIR)
        else Url.Properties.Values[ConnProps_Datadir];
        for i := 0 to Url.Properties.Count -1 do
          if StartsWith(SERVER_ARGUMENTS_KEY_PREFIX, Url.Properties[i]) and
            (Length(Url.Properties[i])>Length(SERVER_ARGUMENTS_KEY_PREFIX)+1) then
            TmpList.Add(Url.Properties.ValueFromIndex[i]);
        SetLength(FServerArgs, TmpList.Count);
        SetLength(FServerArgsRaw, TmpList.Count);
        for i := 0 to TmpList.Count - 1 do begin
          {$IFDEF UNICODE}
          FServerArgsRaw[i] := ZUnicodeToRaw(TmpList[i], ZOSCodePage);
          {$ELSE}
          FServerArgsRaw[i] := TmpList[i];
          {$ENDIF}
          FServerArgs[i] :=  Pointer(FServerArgsRaw[i]);
        end;
        I := TmpList.Count;
        if Assigned(PlainDriver.mysql_library_init)
        then ErrorNo := PlainDriver.mysql_library_init(i, Pointer(FServerArgs), @SERVER_GROUPS) //<<<-- Isn't threadsafe
        else ErrorNo := PlainDriver.mysql_server_init(I, Pointer(FServerArgs), @SERVER_GROUPS); //<<<-- Isn't threadsafe
        if ErrorNo <> 0 then
          raise Exception.Create('Could not initialize the MySQL / MariaDB client library. Error No: ' + ZFastCode.IntToStr(ErrorNo));  // The manual says nothing else can be called until this call succeeds. So lets just throw the error number...
        PlainDriver.IsInitialized := True;
      finally
        FreeAndNil(TmpList);
        GlobalCriticalSection.Leave;
      end;
    end;
  end
  else
    raise Exception.Create('Can''t receive Plaindriver!');
end;

{**
  Returns the version of the plain driver library that will be used to open a connection
  to the given URL.
  @param url the URL of the database
  @return the version number of the plain driver library for the give URL
}
function TZMySQLDriver.GetClientVersion(const Url: string): Integer;
var
  TempURL: TZURL;
begin
  TempURL := TZURL.Create(Url);
  Result := ConvertMySQLVersionToSQLVersion(TZMySQLPlainDriver(GetPlainDriver(TempUrl).GetInstance).mysql_get_client_version);
  TempUrl.Free
end;

{ TZMySQLConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZMySQLConnection.InternalCreate;
begin
  FPlainDriver := TZMySQLPlainDriver(PlainDriver.GetInstance);
  FIKnowMyDatabaseName := False;
  if Self.Port = 0 then
     Self.Port := MYSQL_PORT;
  inherited SetTransactionIsolation(tiRepeatableRead);
  FMetaData := TZMySQLDatabaseMetadata.Create(Self, Url);
  FSavePoints := TStringList.Create;
end;

const
  MySQLSessionTransactionIsolation: array[TZTransactIsolationLevel] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE');

  MySQLSessionTransactionReadOnly: array[Boolean] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION READ ONLY',
    'SET SESSION TRANSACTION READ WRITE');

  MySQLCommitMsg: array[Boolean] of RawByteString = (
    'Native SetAutoCommit False call', 'Native SetAutoCommit True call');

function TZMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean: Boolean;
begin
  Result := FMySQL_FieldType_Bit_1_IsBoolean;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  LogMessage: RawByteString;
  UIntOpt: UInt;
  MyBoolOpt: Byte;
  ClientFlag : Cardinal;
  SslCa, SslCaPath, SslKey, SslCert, SslCypher: PAnsiChar;
  myopt: TMySQLOption;
  sMyOpt: string;
  my_client_Opt:TMYSQL_CLIENT_OPTIONS;
  sMy_client_Opt, sMy_client_Char_Set:String;
  ClientVersion: Integer;
  SQL: RawByteString;
  MaxLobSize: ULong;
label setuint;
begin
  if not Closed then
    Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
  GlobalCriticalSection.Enter;
  try
    FHandle := FPlainDriver.mysql_init(FHandle); //is not threadsave!
  finally
    GlobalCriticalSection.Leave;
  end;
  {EgonHugeist: get current characterset first }
  if Assigned(FPlainDriver.mysql_character_set_name) then begin
    SslCa := FPlainDriver.mysql_character_set_name(FHandle);
    {$IFDEF UNICODE}
    sMy_client_Char_Set := ASCII7ToUnicodeString(SslCa, StrLen(SslCa));
    {$ELSE}
    System.SetString(sMy_client_Char_Set, SslCa, StrLen(SslCa));
    {$ENDIF}
    if (sMy_client_Char_Set <> '') {mysql 4down doesn't have this function } and
     (sMy_client_Char_Set <> FClientCodePage) then begin
      ConSettings^.ClientCodePage := FPlainDriver.ValidateCharEncoding(sMy_client_Char_Set);
      SetConvertFunctions(ConSettings);
    end;
  end;
  try
    { Sets a default port number. }
    if Port = 0 then
       Port := MYSQL_PORT;

    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values[ConnProps_Compress]) and
      (Info.Values[GetMySQLOptionValue(MYSQL_OPT_COMPRESS)] = '') then //check if user allready did set the value!
      Info.Values[GetMySQLOptionValue(MYSQL_OPT_COMPRESS)] := Info.Values[ConnProps_Compress];
    { Sets connection timeout. }
    if (StrToIntDef(Info.Values[ConnProps_Timeout], 0) >= 0) and
       (Info.Values[GetMySQLOptionValue(MYSQL_OPT_CONNECT_TIMEOUT)] = '') then //check if user allready did set the value!
      Info.Values[GetMySQLOptionValue(MYSQL_OPT_CONNECT_TIMEOUT)] := Info.Values[ConnProps_Timeout];

   (*Added lines to handle option parameters 21 november 2007 marco cotroneo*)
    ClientVersion := FPlainDriver.mysql_get_client_version;
    for myopt := low(TMySQLOption) to high(TMySQLOption) do
    begin
      sMyOpt:= GetMySQLOptionValue(myOpt);
      if ClientVersion >= TMySqlOptionMinimumVersion[myopt] then //version checked (:
        case myopt of
          {unsigned int options ...}
          MYSQL_OPT_CONNECT_TIMEOUT,
          MYSQL_OPT_PROTOCOL,
          MYSQL_OPT_READ_TIMEOUT,
          MYSQL_OPT_WRITE_TIMEOUT:
            if Info.Values[sMyOpt] <> '' then
            begin
setuint:      UIntOpt := StrToIntDef(Info.Values[sMyOpt], 0);
              FPlainDriver.mysql_options(FHandle, myopt, @UIntOpt);
            end;
          MYSQL_OPT_LOCAL_INFILE: {optional empty or unsigned int}
            if Info.Values[sMyOpt] <> '' then
              goto setuint
            else
              if Info.IndexOf(sMyOpt) > -1 then
                FPlainDriver.mysql_options(FHandle, myopt, nil);
          { no value options }
          MYSQL_OPT_COMPRESS,
          MYSQL_OPT_GUESS_CONNECTION,
          MYSQL_OPT_NAMED_PIPE,
          MYSQL_OPT_USE_REMOTE_CONNECTION,
          MYSQL_OPT_USE_EMBEDDED_CONNECTION,
          MYSQL_OPT_USE_RESULT,
          MYSQL_OPT_CONNECT_ATTR_RESET:
            if (Info.Values[sMyOpt] <> '') or (Info.IndexOf(sMyOpt) > -1) then
              FPlainDriver.mysql_options(FHandle, myopt, nil);
          { my_bool * options}
          MYSQL_REPORT_DATA_TRUNCATION,
          MYSQL_SECURE_AUTH,
          MYSQL_OPT_RECONNECT,
          MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
          MYSQL_ENABLE_CLEARTEXT_PLUGIN,
          MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
          MYSQL_OPT_SSL_ENFORCE:
            if Info.Values[sMyOpt] <> '' then
            begin
              MyBoolOpt := Ord(StrToBoolEx(Info.Values[sMyOpt]));
              FPlainDriver.mysql_options(FHandle, myopt, @MyBoolOpt);
            end;
          { unsigned char * options }
          MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT,
          MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER: ;//skip, processed down below
          else
            if Info.Values[sMyOpt] <> '' then
              FPlainDriver.mysql_options(FHandle, myopt, PAnsiChar(
                ConSettings^.ConvFuncs.ZStringToRaw(Info.Values[sMyOpt],
                  ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)));
        end;
    end;

    { Set ClientFlag }
    ClientFlag := 0;
    if not StrToBoolEx(Info.Values[ConnProps_DBLess]) then
      ClientFlag := (1 shl Integer(CLIENT_CONNECT_WITH_DB));

    for my_client_Opt := Low(TMYSQL_CLIENT_OPTIONS) to High(TMYSQL_CLIENT_OPTIONS) do
    begin
      sMy_client_Opt := GetEnumName(TypeInfo(TMYSQL_CLIENT_OPTIONS), Integer(my_client_Opt));
      if StrToBoolEx(Info.Values[sMy_client_Opt]) then
        ClientFlag := ClientFlag or Cardinal(1 shl Integer(my_client_Opt));
    end;

  { Set SSL properties before connect}
  SslKey := nil; SslCert := nil; SslCa := nil; SslCaPath := nil; SslCypher := nil;
  if StrToBoolEx(Info.Values[ConnProps_MYSQLSSL]) then
    begin                                          
       if Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_KEY)] <> '' then
          SslKey := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_KEY)], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CERT)] <> '' then
          SslCert := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CERT)], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CA)] <> '' then
          SslCa := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CA)], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CAPATH)] <> '' then
          SslCaPath := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CAPATH)], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CIPHER)] <> '' then
          SslCypher := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values[GetMySQLOptionValue(MYSQL_OPT_SSL_CIPHER)], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       FPlainDriver.mysql_ssl_set(FHandle, SslKey, SslCert, SslCa, SslCaPath,
          SslCypher);
       DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
          'SSL options set');
    end;

    { Connect to MySQL database. }
    {$IFDEF UNICODE}
    if FPlainDriver.mysql_real_connect(FHandle, PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(HostName, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
                              PAnsiChar(ConSettings^.User), PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(Password, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
    {$ELSE}
    if FPlainDriver.mysql_real_connect(FHandle, PAnsiChar(HostName),
                              PAnsiChar(ConSettings^.User), PAnsiChar(Password),
    {$ENDIF}
                              PAnsiChar(ConSettings^.Database), Port, nil,
                              ClientFlag) = nil then begin
      CheckMySQLError(FPlainDriver, FHandle, nil, lcConnect, LogMessage, Self);
      DriverManager.LogError(lcConnect, ConSettings^.Protocol, LogMessage,
        0, ConSettings.ConvFuncs.ZStringToRaw(SUnknownError,
          ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
      raise EZSQLException.Create(SCanNotConnectToServer);
    end;
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

    { Fix Bugs in certain Versions where real_conncet resets the Reconnect flag }
    if (Info.Values[GetMySQLOptionValue(MYSQL_OPT_RECONNECT)] <> '') and
      ((ClientVersion>=50013) and (ClientVersion<50019)) or
      ((ClientVersion>=50100) and (ClientVersion<50106)) then begin
      MyBoolOpt := Ord(StrToBoolEx(Info.Values[GetMySQLOptionValue(MYSQL_OPT_RECONNECT)]));
      FPlainDriver.mysql_options(FHandle, MYSQL_OPT_RECONNECT, @MyBoolOpt);
    end;
    if (FClientCodePage = '') and (sMy_client_Char_Set <> '') then
      FClientCodePage := sMy_client_Char_Set;

    //EH: MariaDB needs a explizit set of charset to be synced on Client<>Server!
    if (FClientCodePage <> sMy_client_Char_Set) or (FPlainDriver.IsMariaDBDriver and (FClientCodePage <> '')) then begin
      //http://dev.mysql.com/doc/refman/5.7/en/mysql-set-character-set.html
      //take care mysql_real_escape_string works like expected!
      SQL := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage);
      if not Assigned(FPlainDriver.mysql_set_character_set) or
            (FPlainDriver.mysql_set_character_set(FHandle, Pointer(SQL)) <> 0) then begin //failed? might be possible the function does not exists
        SQL := 'SET NAMES '+SQL;
        ExecuteImmediat(SQL, lcOther);
      end;
      CheckCharEncoding(FClientCodePage);
    end;

    MaxLobSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Info.Values[ConnProps_MaxLobSize], 0);
    if MaxLobSize <> 0 then begin
      SQL := 'SET GLOBAL max_allowed_packet='+IntToRaw(MaxLobSize);
      ExecuteImmediat(SQL, lcOther);
    end;
    inherited Open;
    //no real version check required -> the user can simply switch off treading
    //enum('Y','N')
    FMySQL_FieldType_Bit_1_IsBoolean := StrToBoolEx(Info.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]);
    FSupportsBitType := (
      (    FPlainDriver.IsMariaDBDriver and (ClientVersion >= 100109) ) or
      (not FPlainDriver.IsMariaDBDriver and (ClientVersion >=  50003) ) ) and (GetHostVersion >= EncodeSQLVersioning(5,0,3));
    //if not explizit !un!set -> assume as default since Zeos 7.3
    FMySQL_FieldType_Bit_1_IsBoolean := FMySQL_FieldType_Bit_1_IsBoolean or (FSupportsBitType and (Info.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean] = ''));
    (GetMetadata as IZMySQLDatabaseMetadata).SetMySQL_FieldType_Bit_1_IsBoolean(FMySQL_FieldType_Bit_1_IsBoolean);
    FSupportsReadOnly := (    FPlainDriver.IsMariaDBDriver and (GetHostVersion >= EncodeSQLVersioning(10,0,0))) or
                         (not FPlainDriver.IsMariaDBDriver and (GetHostVersion >= EncodeSQLVersioning( 5,6,0)));
    (GetMetadata as IZMySQLDatabaseMetadata).SetDataBaseName(GetDatabaseName);

    { Sets transaction isolation level. }
    if not (TransactIsolationLevel in [tiNone,tiRepeatableRead]) then
      ExecuteImmediat(MySQLSessionTransactionIsolation[TransactIsolationLevel], lcTransaction);
    if (FSupportsReadOnly and ReadOnly) then
      ExecuteImmediat(MySQLSessionTransactionReadOnly[ReadOnly], lcTransaction);

    { Sets an auto commit mode. }
    if not AutoCommit then begin
      AutoCommit := True;
      SetAutoCommit(False);
    end;
  except
    FPlainDriver.mysql_close(FHandle);
    FHandle := nil;
    raise;
  end;

  if FClientCodePage = '' then begin //workaround for MySQL 4 down
    with CreateStatement.ExecuteQuery('show variables like "character_set_database"') do begin
      if Next then
        FClientCodePage := GetString(FirstDbcIndex+1);
      Close;
    end;
    ConSettings^.ClientCodePage := FPlainDriver.ValidateCharEncoding(FClientCodePage);
    SetConvertFunctions(ConSettings);
  end;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZMySQLConnection.PingServer: Integer;
const
   PING_ERROR_ZEOSCONNCLOSED = -1;
begin
   if Closed or (FHandle = nil)
   then Result := PING_ERROR_ZEOSCONNCLOSED
   else Result := FPlainDriver.mysql_ping(FHandle);
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
function TZMySQLConnection.PrepareCallWithParams(const Name: String;
  Info: TStrings): IZCallableStatement;
begin
  if (FPLainDriver.IsMariaDBDriver and (FPLainDriver.mysql_get_client_version >= 100000)) or
     (not FPLainDriver.IsMariaDBDriver and (FPLainDriver.mysql_get_client_version >= 50608))
  then Result := TZMySQLCallableStatement56up.Create(Self, Name, Info)
  else Result := TZMySQLCallableStatement56down.Create(Self, Name, Info);
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
function TZMySQLConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLPreparedStatement.Create(Self, SQL, Info)
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
function TZMySQLConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLStatement.Create(Self, Info);
end;

destructor TZMySQLConnection.Destroy;
begin
  inherited Destroy;
  FSavePoints.Free;
end;

function TZMySQLConnection.EscapeString(
  const Value: RawByteString): RawByteString;
begin
  GetEscapeString(Pointer(Value), Length(Value), Result);
end;

procedure TZMySQLConnection.ExecuteImmediat(const SQL: RawByteString;
  LoggingCategory: TZLoggingCategory);
begin
  if FPlainDriver.mysql_real_query(FHandle,
    Pointer(SQL), Length(SQL){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}) <> 0 then
      CheckMySQLError(FPlainDriver, FHandle, nil, LoggingCategory, SQL, Self);
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, SQL);
end;

{**
  Attempts to kill a long-running operation on the database server
  side
}
Function TZMySQLConnection.AbortOperation: Integer;
Var
 killquery: SQLString;
 izc: IZConnection;
Begin
  // https://dev.mysql.com/doc/refman/5.7/en/mysql-kill.html
  killquery := 'KILL QUERY ' + IntToStr(FPlainDriver.mysql_thread_id(FHandle));
  izc := DriverManager.GetConnection(GetURL);
  Result := izc.CreateStatement.ExecuteUpdate(killquery);
End;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Commit;
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
    If FPlainDriver.mysql_commit(FHandle) <> 0 then
      CheckMySQLError(FPlainDriver, FHandle, nil, lcExecute, 'Native Commit call', Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, ConSettings.Protocol, 'Native Commit call');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
  end
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FHandle := nil;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZMySQLConnection.Rollback;
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
    If FPlainDriver.mysql_rollback(FHandle) <> 0 then
      CheckMySQLError(FPlainDriver, FHandle, nil, lcExecute, 'Native Rollback call', Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, ConSettings.Protocol, 'Native Rollback call');
    AutoCommit := True;
    if FRestartTransaction then
      StartTransaction;
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
procedure TZMySQLConnection.InternalClose;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  try
    if not AutoCommit then begin
      SetAutoCommit(True);
      AutoCommit := False;
    end;
  finally
    FPlainDriver.mysql_close(FHandle);
    FHandle := nil;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol,
        'DISCONNECT FROM "'+ConSettings^.Database+'"');
  end;
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZMySQLConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZMySQLConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZMySQLConnection.SetReadOnly(Value: Boolean);
begin
  if not FSupportsReadOnly then
    Value := False;
  if Value <> ReadOnly then begin
    if not Closed then
      ExecuteImmediat(MySQLSessionTransactionReadOnly[ReadOnly], lcTransaction);
    ReadOnly := Value;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level = tiNone then
    Level := tiRepeatableRead;
  if TransactIsolationLevel <> Level then begin
    if not Closed then
      ExecuteImmediat(MySQLSessionTransactionIsolation[Level], lcTransaction);
    TransactIsolationLevel := Level;
  end;
end;

function TZMySQLConnection.StartTransaction: Integer;
var S: String;
begin
  if Closed then
    Open;
  if AutoCommit then begin
    if FPlainDriver.mysql_autocommit(FHandle, 0) <> 0 then
      CheckMySQLError(FPlainDriver, FHandle, nil, lcExecute, MySQLCommitMsg[False], Self);
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, MySQLCommitMsg[False]);
    AutoCommit := False;
    Result := 1;
  end else begin
    S := 'SP'+ZFastCode.IntToStr(NativeUint(Self))+'_'+ZFastCode.IntToStr(FSavePoints.Count);
    ExecuteImmediat('SAVEPOINT '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S), lcTransaction);
    Result := FSavePoints.Add(S) +2;
  end;
end;

function TZMySQLConnection.SupportsFieldTypeBit: Boolean;
begin
  if Closed then
    Open;
  Result := FSupportsBitType;
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
procedure TZMySQLConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> AutoCommit then begin
    FRestartTransaction := AutoCommit;
    if Closed
    then AutoCommit := Value
    else if Value then begin
      FSavePoints.Clear;
      if FPlainDriver.mysql_autocommit(FHandle, 1) <> 0 then
        CheckMySQLError(FPlainDriver, FHandle, nil, lcExecute, MySQLCommitMsg[True], Self);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, MySQLCommitMsg[True]);
      AutoCommit := True;
    end else
      StartTransaction;
  end;
end;

{**
  Gets client's full version number.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetClientVersion: Integer;
begin
  Result := ConvertMySQLVersionToSQLVersion(FPlainDriver.mysql_get_client_version );
end;

{**
  Gets server's full version number.
  The format of the returned version must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetHostVersion: Integer;
begin
  Result := ConvertMySQLVersionToSQLVersion( FPlainDriver.mysql_get_server_version(FHandle) );
end;

function TZMySQLConnection.GetPlainDriver: TZMySQLPlainDriver;
begin
  Result := FPlainDriver;
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandle: PPMYSQL;
begin
  Result := @FHandle;
end;

function TZMySQLConnection.GetServerProvider: TZServerProvider;
begin
  Result := spMySQL;
end;

function TZMySQLConnection.GetDatabaseName: String;
var
  ResultSet: IZResultSet;
begin
  if not FIKnowMyDatabaseName then begin
    ResultSet := CreateStatement.ExecuteQuery('select database() as ''DATABASE''');
    if ResultSet.Next
    then FDatabaseName := ResultSet.GetStringByName('DATABASE');
    FIKnowMyDatabaseName := True;
    ResultSet.Close;
    ResultSet := nil;
  end;
  Result := FDatabaseName;
end;

function TZMySQLConnection.GetEscapeString(
  const Value: ZWideString): ZWideString;
var tmp: RawByteString;
begin
  tmp := ZUnicodeToRaw(Value, ConSettings.ClientcodePage.CP);
  GetEscapeString(Pointer(tmp), Length(Tmp), tmp);
  Result := ZRawToUnicode(tmp, ConSettings.ClientcodePage.CP);
end;

procedure TZMySQLConnection.GetEscapeString(Buf: PAnsichar; Len: LengthInt;
  out Result: RawByteString);
var
  StatBuf: array[0..2048] of AnsiChar;
  P: PAnsichar;
  EscapedLen: ULong;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  if ((Len+1) shl 1) > (SizeOf(Buf)-1) then begin
    SetLength(Result, (Len+1) shl 1);
    P := Pointer(Result);
  end else
    P := @StatBuf[0];
  PByte(P)^ := Ord(#39);
  if FHandle = nil
  then EscapedLen := FPlainDriver.mysql_escape_string(P+1, Buf, Len)
  else EscapedLen := FPlainDriver.mysql_real_escape_string(FHandle, P+1, Buf, Len);
  PByte(P+EscapedLen+1)^ := Ord(#39);
  if P = @StatBuf[0]
  then ZSetString(@StatBuf[0], EscapedLen+2, Result)
  else SetLength(Result, EscapedLen+2);
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;

  {$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.
