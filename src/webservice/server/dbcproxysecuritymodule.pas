{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                WebService Proxy Server                  }
{                                                         }
{         Originally written by Jan Baumgarten            }
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

unit DbcProxySecurityModule;

{$I dbcproxy.inc}

interface

uses
  Classes, SysUtils, IniFiles,
  ZDbcIntfs,
  dbcproxyconfigstore,
  yubiotp,
  GoogleOTP,
  md5crypt;

type
  TZAbstractSecurityModule = class
  public
    FModuleName: String;
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; virtual; abstract;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); virtual;
  end;

  TZYubiOtpSecurityModule = class(TZAbstractSecurityModule)
  protected
    FYubikeysFile: String;
    FAddDatabase: Boolean;
    FDatabaseSeparator: String;
    FBaseURL: String;
    FClientID: Integer;
    FSecretKey: String;
    FYubikeySQL: String;
    FDBUser: String;
    FDBPassword: String;
    FReplacementUserNameColumn: String;
    function CheckUserYubikeyFile(const UserName, Password, ConnectionName: String): Boolean;
    function CheckUserYubikeyDatabase(var UserName: String; const Password, ConnectionName: String): Boolean;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
  end;

  TZTotpSecurityModule = class(TZAbstractSecurityModule)
  protected
    FSecretsName: String;
    FAddDatabase: Boolean;
    FDatabaseSeparator: String;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
  end;

  TZIntegratedSecurityModule = class(TZAbstractSecurityModule)
  protected
    FDBUser: String;
    FDBPassword: String;
    FPasswordSQL: String;
    FReplacementUser: String;
    FReplacementPassword: String;
    FAddDatabaseToUserName: Boolean;
    FAuthDbName: String;
  public
    function CheckPassword(var XUserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
  end;

  {
  TZIntegratedSecurityModule = class(TZAbstractSecurityModule)
  protected
    FDBUser: String;
    FDBPassword: String;
    FPasswordSQL: String;
    FReplacementUser: String;
    FReplacementPassword: String;
    FAddDatabaseToUserName: Boolean;
    FAuthDbName: String;
  public
    function CheckPassword(var XUserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
  end;
  }
  TZChainedSecurityModule = class(TZAbstractSecurityModule)
  protected
    FModuleChain: Array of TZAbstractSecurityModule;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
    destructor Destroy; override;
  end;

  TZAlternateSecurityModule = class(TZAbstractSecurityModule)
  protected
    FModuleChain: Array of TZAbstractSecurityModule;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(Values: IZDbcProxyKeyValueStore); override;
    destructor Destroy; override;
  end;

function GetSecurityModule(TypeName: String): TZAbstractSecurityModule;

implementation

uses DbcProxyConfigManager, zeosproxy_imp, StrUtils, Types, ZExceptions
     {$IFDEF ENABLE_BCRYPT}, BCrypt{$ENDIF}
     {$IFDEF ENABLE_LDAP_SECURITY},DbcProxyLdapSecurityModule{$ENDIF}
     ;

function GetSecurityModule(TypeName: String): TZAbstractSecurityModule;
begin
  TypeName := LowerCase(TypeName);
  if TypeName = 'yubiotp' then
    Result := TZYubiOtpSecurityModule.Create
  else if TypeName = 'totp' then
    Result := TZTotpSecurityModule.Create
  else if TypeName = 'integrated' then
    Result := TZIntegratedSecurityModule.Create
  else if TypeName = 'chained' then
    Result := TZChainedSecurityModule.Create
  else if TypeName = 'alternate' then
    Result := TZAlternateSecurityModule.Create
  {$IFDEF ENABLE_LDAP_SECURITY}
  else if TypeName = 'ldap' then
    Result := TZLdapSecurityModule.Create
  {$ENDIF}
  else
    //raise EZSQLException.Create('Security module of type ' + TypeName + ' is unknown.');
    Result := nil;
end;

procedure TZAbstractSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
begin
  FModuleName := Values.GetName;
end;

{------------------------------------------------------------------------------}

function TZYubiOtpSecurityModule.CheckUserYubikeyFile(const UserName, Password, ConnectionName: String): Boolean;
var
  Yubikeys: TStringList;
  AllowedKeys: String;
  PublicIdentity: String;
  YubikeysUser: String;
begin
  Yubikeys := TStringList.Create;
  try
    Yubikeys.NameValueSeparator:=':';
    Yubikeys.LoadFromFile(FYubikeysFile, TEncoding.UTF8);
    YubikeysUser := UserName;
    if FAddDatabase then
      YubikeysUser := YubikeysUser + FDatabaseSeparator + ConnectionName;
    AllowedKeys := ':' + Yubikeys.Values[YubikeysUser] + ':';
    Logger.Debug('yubiotp: User: ' + YubikeysUser + ' Allowed keys: ' + AllowedKeys);
    if AllowedKeys = '::' then
      raise Exception.Create('No yubikeys found for user ' + YubikeysUser);
    PublicIdentity := GetYubikeyIdentity(Password);
    Result := Pos(':' + PublicIdentity + ':', AllowedKeys) > 0;
    if not Result then
      raise Exception.Create('The yubikey ' + PublicIdentity + ' cannot be used for the user ' + YubikeysUser + '.');
  finally
    FreeAndNil(Yubikeys);
  end;
end;

function TZYubiOtpSecurityModule.CheckUserYubikeyDatabase(var UserName: String; const Password, ConnectionName: String): Boolean;
var
  URL: String;
  Conn: IZConnection;
  Stmt: IZPreparedStatement;
  RS: IZResultSet;
  PropertiesList: TStringList;
  PublicIdentity: String;
  YubikeysUser: String;
begin
  Result := False;
  YubikeysUser := '';

  if FAddDatabase then
    YubikeysUser := YubikeysUser + FDatabaseSeparator + ConnectionName
  else
    YubikeysUser := UserName;

  PublicIdentity := GetYubikeyIdentity(Password);

  URL := ConfigManager.ConstructUrl(ConnectionName, FDBUser, FDBPassword, False);
  PropertiesList := TStringList.Create;
  try
    Conn := DriverManager.GetConnectionWithParams(Url, PropertiesList);
  finally
    FreeAndNil(PropertiesList);
  end;

  Stmt := Conn.PrepareStatement(FYubikeySQL);
  Stmt.SetResultSetConcurrency(rcReadOnly);
  Stmt.SetResultSetType(rtForwardOnly);
  Stmt.SetString(FirstDbcIndex, PublicIdentity);
  Stmt.SetString(FirstDbcIndex + 1, YubikeysUser);
  if Stmt.ExecutePrepared then begin
    RS := Stmt.GetResultSet;
    if Assigned(RS) and RS.IsBeforeFirst then begin
      if RS.Next then begin
        try
          if FReplacementUserNameColumn <> '' then
            UserName := RS.GetStringByName(FReplacementUserNameColumn);
          Result := True;
        finally
          RS.Close;
        end;
      end else begin
        raise Exception.Create('No record for user ' + UserName + ' and Yubikey ' + PublicIdentity + '  found.');
      end;
    end;
  end;
  RS := nil;
  Stmt := nil;
  Conn.Close;
  Conn := nil;
end;

function TZYubiOtpSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  YubiStatus: TYubiOtpStatus;
  RemainingPassword: String;
begin
  Result := false;
  if FYubikeysFile <> '' then
    Result := CheckUserYubikeyFile(UserName, Password, ConnectionName);
  if FYubikeySQL <> '' then
    Result := CheckUserYubikeyDatabase(UserName, Password, ConnectionName);

  if Result then begin
    YubiStatus := VerifyYubiOtp(FBaseURL, Password, RemainingPassword, FClientID, FSecretKey);
    Result := YubiStatus = yosOk;
    RaiseYubiOtpError(YubiStatus);
    Password := RemainingPassword;
  end;
  Logger.Debug('yubiotpresult for server ' + FBaseURL + ': ' + BoolToStr(Result, true));
end;

procedure TZYubiOtpSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
begin
  inherited;
  Logger.Debug('Initializing Security module ' + Values.GetName);
  FYubikeysFile := Values.ReadString('Yubikeys File', '');
  FAddDatabase := StrToBool(Values.ReadString('Add Database To Username', 'false'));
  FDatabaseSeparator := Values.ReadString('Database Separator', '@');
  FBaseURL := Values.ReadString('Base URL', 'https://api.yubico.com/wsapi/2.0/verify');
  FClientID := Values.ReadInteger('Client ID', 0);
  FSecretKey := Values.ReadString('Secret Key', '');
  FYubikeySQL := Values.ReadString('Yubikey SQL', '');
  FDBUser := Values.ReadString('DB User', '');
  FDBPassword := Values.ReadString('DB Password', '');
  FReplacementUserNameColumn := Values.ReadString('Replacement User Column', '');
end;

{------------------------------------------------------------------------------}

function TZTotpSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  Secrets: TStringList;
  Secret: String;
  SecretsUser: String;
  RemainingPassword: String;
  OTP: String;
begin
  Secrets := TStringList.Create;
  try
    Secrets.NameValueSeparator:=':';
    Secrets.LoadFromFile(FSecretsName, TEncoding.UTF8);
    SecretsUser := UserName;
    if FAddDatabase then
      SecretsUser := SecretsUser + FDatabaseSeparator + ConnectionName;
    Secret := UpperCase(Trim(Secrets.Values[SecretsUser]));
    Result := Secret <> '';
  finally
    FreeAndNil(Secrets);
  end;

  if Result then begin
    OTP := Copy(Password, Length(Password) - 6 + 1, 6);
    Result := GoogleOTP.ValidateTOPT(Secret, StrToIntDef(OTP, 0));
  end else
    raise Exception.Create('Could not find secret for user ' + SecretsUser);

  if not Result then
    raise Exception.Create('Could not validate username / OTP: ' + SecretsUser + ' / ' + OTP);

  if not Result then
    raise EZSQLException.Create('Could not validate username / password.');

  RemainingPassword := Copy(Password, 1, Length(Password) - 6);
  Password := RemainingPassword;
end;

procedure TZTotpSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
begin
  inherited;
  Logger.Debug('Initializing Security module ' + Values.GetName);
  FSecretsName := Values.ReadString('Secrets File', '');
  FAddDatabase := StrToBool(Values.ReadString('Add Database To Username', 'false'));
  FDatabaseSeparator := Values.ReadString('Database Separator', '@');
end;

{------------------------------------------------------------------------------}

function TZIntegratedSecurityModule.CheckPassword(var XUserName, Password: String; const ConnectionName: String): Boolean;
var
  URL: String;
  Conn: IZConnection;
  Stmt: IZPreparedStatement;
  RS: IZResultSet;
  PropertiesList: TStringList;
  CryptPwdDB: UTF8String;
  CryptPwdUser: UTF8String;
  pwdStart: String;
  DBUserName: String;
  PWUserName: String;
  Position: Integer;
  {$IFDEF ENABLE_BCRYPT}
  BCrypt: TBCryptHash;
  {$ENDIF}
begin
  Result := False;

  if FAddDatabaseToUserName and (Length(XUserName) > 0 ) then begin
    Position := Pos('@', XUserName);
    if Position = 0 then begin // no @ in the user name
      DBUserName := XUserName;
      if FAuthDbName = '' then
        PWUserName := XUserName + '@' + ConnectionName
      else
        PWUserName := XUserName + '@' + FAuthDbName;
    end else if Position = Length(XUserName) then begin // the user name ends with an @
      DBUserName := Copy(XUserName, 1, Length(XUserName) - 1);
      PWUserName := DBUserName;
    end else begin // the user name contains an @ but doesn't end with an @
      // just copy the username
      DBUserName := XUserName;
      PWUserName := XUserName;
    end;
  end else begin
    DBUserName := XUserName;
    PWUserName := XUserName;
  end;


  URL := ConfigManager.ConstructUrl(ConnectionName, FDBUser, FDBPassword, False);
  PropertiesList := TStringList.Create;
  try
    Conn := DriverManager.GetConnectionWithParams(Url, PropertiesList);
  finally
    FreeAndNil(PropertiesList);
  end;

  Stmt := Conn.PrepareStatement(FPasswordSQL);
  Stmt.SetResultSetConcurrency(rcReadOnly);
  Stmt.SetResultSetType(rtForwardOnly);
  Stmt.SetString(FirstDbcIndex, DBUserName);
  if Stmt.ExecutePrepared then begin
    RS := Stmt.GetResultSet;
    if Assigned(RS) and RS.IsBeforeFirst then begin
      if RS.Next then begin
        try
          CryptPwdDB := RS.GetUTF8String(FirstDbcIndex);
        finally
          RS.Close;
        end;
      end else begin
        raise Exception.Create('No record for user ' + XUserName + ' found.');
      end;
    end;
  end;
  RS := nil;
  Stmt := nil;
  Conn.Close;
  Conn := nil;


  pwdStart := Copy(CryptPwdDB, 1, 3);
  if pwdStart = '$1$' then //cryptmd5
    CryptPwdUser := crypt_md5(Password, CryptPwdDB)
  else if pwdStart = 'md5' then //md5 by PpostgreSQL
    CryptPwdUser := crypt_md5pg(Password, PWUserName)
  {$IFDEF ENABLE_BCRYPT}
  else if copy(CryptPwdDB, 1, 4) = '$2y$' then begin
    BCrypt := TBCryptHash.Create;
    try
      Result := BCrypt.VerifyHash(Password, CryptPwdDB);
    finally
      FreeAndNil(BCrypt);
    end;
  end
  {$ENDIF}
  else
    CryptPwdUser := '$$$$$$$$$$'; // $-Signs shouldn't make up a valid crypted password.

  if not Result then begin
    Result := CryptPwdDB = CryptPwdUser;
    Logger.Debug('Integrated security module: CryptPwdUser:' + CryptPwdUser + ' CryptPwdDB: ' + CryptPwdDB);
  end;

  if FReplacementUser <> '' then begin
    XUserName := FReplacementUser;
    Password := FReplacementPassword;
  end;
end;

procedure TZIntegratedSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
begin
  inherited;
  Logger.Debug('Initializing Security module ' + Values.GetName);
  FDBUser := Values.ReadString('DB User', '');
  FDBPassword := Values.ReadString('DB Password', '');
  FReplacementUser := Values.ReadString('Replacement User', '');
  FReplacementPassword := Values.ReadString('Replacement Password', '');
  FPasswordSQL := Values.ReadString('Password SQL', '');
  FAddDatabaseToUserName := Values.ReadBool('Add Database To Username', false);
  FAuthDbName := Values.ReadString('Auth DB Name', '');
end;

{------------------------------------------------------------------------------}

function TZChainedSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  x: Integer;
begin
  Logger.Debug('Checking chained module for ' + ConnectionName);
  Result := True;
  for x := 0 to Length(FModuleChain) - 1  do begin
    try
      Result := Result and FModuleChain[x].CheckPassword(UserName, Password, ConnectionName);
      Logger.Debug('Result for module ' + IntToStr(x + 1) + ': ' + BoolToStr(Result));
    except
      on E: Exception do begin
        Logger.Warning('Unexpected exception while calling an authentication module:  ' + E.Message);
        Result := false;
      end;
    end;
    if not Result then break;
  end;
end;

procedure TZChainedSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
var
  Modules: String;
  ModuleList: TStringDynArray;
  x: Integer;
  SubModuleName: String;
  SubModuleType: String;
  SubmoduleInfos: IZDbcProxyKeyValueStore;
begin
  inherited;
  Logger.Debug('Initializing Security module ' + Values.GetName);
  Modules := Values.ReadString('Module List', '');
  Logger.Debug(Format('Modules: %s', [Modules]));
  ModuleList := SplitString(Modules, ',');
  for x := Length(ModuleList) - 1 downto 0 do
    ModuleList[x] := Trim(ModuleList[x]);
  for x := Length(ModuleList) - 1 downto 0 do
    if ModuleList[x] = '' then
      Delete(ModuleList, x, 1)
    else
      Logger.Debug(Format('Module %d = %s', [x, ModuleList[x]]));
  if Length(ModuleList) = 0 then
    raise EZSQLException.Create('A chained security module may not have an empty Module List');

  SetLength(FModuleChain, Length(ModuleList));
  for x := 0 to Length(ModuleList) - 1 do begin
    SubModuleName := ModuleList[x];
    SubmoduleInfos := Values.GetConfigStore.GetSecurityConfig(SubModuleName);
    SubModuleType := SubmoduleInfos.ReadString('type', '');
    Logger.Debug(Format('Creating submodule %s of type %s', [SubModuleName, SubModuleType]));
    FModuleChain[x] := GetSecurityModule(SubModuleType);
    if not Assigned(FModuleChain[x]) then
      raise Exception.Create(Format('Could not create a security module for %s', [SubModuleName]));
    FModuleChain[x].LoadConfig(SubmoduleInfos);
  end;
end;

destructor TZChainedSecurityModule.Destroy;
var
  x: Integer;
begin
  for x := 0 to Length(FModuleChain) - 1 do
    if Assigned(FModuleChain[x]) then;
      FreeAndNil(FModuleChain[x]);
  inherited;
end;

{------------------------------------------------------------------------------}

function TZAlternateSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  x: Integer;
begin
  Result := False;
  for x := 0 to Length(FModuleChain) - 1  do begin
    try
      Result := Result or FModuleChain[x].CheckPassword(UserName, Password, ConnectionName);
    except
      on E: Exception do begin
        Logger.Warning('Unexpected exception while calling an authentication module:  ' + E.Message);
      end;
    end;
    if Result then break;
  end;
end;

procedure TZAlternateSecurityModule.LoadConfig(Values: IZDbcProxyKeyValueStore);
var
  Modules: String;
  ModuleList: TStringDynArray;
  x: Integer;
  SectionName: String;
  ModuleType: String;
  SubmoduleInfos: IZDbcProxyKeyValueStore;
begin
  inherited;
  Logger.Debug('Initializing Security module ' + Values.GetName);
  Modules := Values.ReadString('Module List', '');
  ModuleList := SplitString(Modules, ',');
  for x := Length(ModuleList) - 1 downto 0 do
    ModuleList[x] := Trim(ModuleList[x]);
  for x := Length(ModuleList) - 1 downto 0 do
    if ModuleList[x] = '' then
      Delete(ModuleList, x, 1);
  if Length(ModuleList) = 0 then
    raise EZSQLException.Create('An alternate security module may not have an empty Module List');

  Logger.Debug('Initializing alternate security module...');
  SetLength(FModuleChain, Length(ModuleList));
  for x := 0 to Length(ModuleList) - 1 do begin

    SectionName := ModuleList[x];
    SubmoduleInfos := Values.GetConfigStore.GetSecurityConfig(SectionName);
    ModuleType := SubmoduleInfos.ReadString('type', '');
    Logger.Debug('Initializing submodule ' + SectionName + ' of type ' + ModuleType);
    FModuleChain[x] := GetSecurityModule(ModuleType);
    if not assigned(FModuleChain[x]) then
      raise Exception.Create(Format('Could not create submodule %s of type %s', [SectionName, ModuleType]));
    FModuleChain[x].LoadConfig(SubmoduleInfos);
  end;
  Logger.Debug('Initialization of alternate module finished.');
end;

destructor TZAlternateSecurityModule.Destroy;
var
  x: Integer;
begin
  for x := 0 to Length(FModuleChain) - 1 do
    if Assigned(FModuleChain[x]) then;
      FreeAndNil(FModuleChain[x]);
  inherited;
end;

end.
