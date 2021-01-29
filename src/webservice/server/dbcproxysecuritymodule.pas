unit DbcProxySecurityModule;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  ZDbcIntfs,
  yubiotp;

type
  TZAbstractSecurityModule = class
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; virtual; abstract;
    procedure LoadConfig(IniFile: TIniFile; const Section: String); virtual; abstract;
  end;

  TZYubiOtpSecurityModule = class(TZAbstractSecurityModule)
  protected
    FYubikeysName: String;
    FAddDatabase: Boolean;
    FDatabaseSeparator: String;
    FBaseURL: String;
    FClientID: Integer;
    FSecretKey: String;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(IniFile: TIniFile; const Section: String); override;
  end;

implementation

function TZYubiOtpSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  Yubikeys: TStringList;
  AllowedKeys: String;
  PublicIdentity: String;
  YubikeysUser: String;
  YubiStatus: TYubiOtpStatus;
  RemainingPassword: String;
begin
  Yubikeys := TStringList.Create;
  try
    Yubikeys.NameValueSeparator:=':';
    Yubikeys.LoadFromFile(FYubikeysName, TEncoding.UTF8);
    YubikeysUser := UserName;
    if FAddDatabase then
      YubikeysUser := YubikeysUser + FDatabaseSeparator + ConnectionName;
    AllowedKeys := ':' + Yubikeys.Values[UserName] + ':';
    PublicIdentity := GetYubikeyIdentity(Password);
    Result := Pos(':' + PublicIdentity + ':', AllowedKeys) > 0;
    if not Result then
      raise Exception.Create('This yubikey cannot be used for this user.');
  finally
    FreeAndNil(Yubikeys);
  end;

  if Result then begin
    YubiStatus := VerifyYubiOtp(FBaseURL, Password, RemainingPassword, FClientID, FSecretKey);
    Result := YubiStatus = yosOk;
    RaiseYubiOtpError(YubiStatus);
    Password := RemainingPassword;
  end;
end;

procedure TZYubiOtpSecurityModule.LoadConfig(IniFile: TIniFile; const Section: String);
begin
  FYubikeysName := IniFile.ReadString(Section, 'Yubikeys File', '');
  FAddDatabase := IniFile.ReadBool(Section, 'Add Database To Username', false);
  FDatabaseSeparator := IniFile.ReadString(Section, 'Database Separator', '@');
  FBaseURL := IniFile.ReadString(Section, 'Base URL', 'https://api.yubico.com/wsapi/2.0/verify');
  FClientID := IniFile.ReadInteger(Section, 'Client ID', 0);
  FSecretKey := IniFile.ReadString(Section, 'Secret Key', '');
end;

end.

