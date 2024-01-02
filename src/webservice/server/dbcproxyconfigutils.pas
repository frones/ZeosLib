unit dbcproxyconfigutils;

{$I dbcproxy.inc}

interface

uses
  Classes, SysUtils, fpc_https_server, openssl;

// This procedure is used to do possible initializations on different OS
// types. Currently we only do that on Windows, so we can infuence, which
// OpenSSL libraries get used.
// We only can take influence on FPC. Synapse seems to initialize SSL
// libraries during initialization. There we only can influence things by
// changing the order of ssl_* units in the uses clause.
procedure InitializeSSLLibs;
// Copies the SSL configuration data from the configuration manager to the
// actual WST worker object.
procedure ConfigureSSL(AppObject: TwstFPHttpsListener);
// Constructs the Servers URL by inspecting the ConfigManager.
function ConstructServerURL: String;

implementation

uses zeosproxy_imp, dbcproxycertstore;

function ConstructServerURL: String;
begin
  if ConfigManager.UseSSL
     {$IFDEF ENABLE_TOFU_CERTIFICATES}or ConfigManager.UseTofuSSL{$ENDIF}
  then
    Result := 'https://'
  else
    Result := 'http://';
  Result := Result + ConfigManager.IPAddress+ ':'+ IntToStr(ConfigManager.ListeningPort)+ '/'
end;

procedure ConfigureSSL(AppObject: TwstFPHttpsListener);
{$IFDEF ENABLE_TOFU_CERTIFICATES}
var
  CertFile, KeyFile: String;
{$ENDIF}
begin
  if ConfigManager.UseSSL then begin
    AppObject.UseSSL := True;
    AppObject.HostName := ConfigManager.HostName;
    AppObject.CertificateFileName := ConfigManager.CertificateFile;
    AppObject.KeyFile := ConfigManager.KeyFile;
    AppObject.KeyPasswod := ConfigManager.KeyPasswod;
  end;
  {$IFDEF ENABLE_TOFU_CERTIFICATES}
  if ConfigManager.UseTofuSSL then begin
    TofuCertStore.GetCurrentCertificate(CertFile, KeyFile);
    AppObject.UseSSL := true;
    AppObject.HostName := TofuCertStore.HostName;
    AppObject.CertificateFileName := CertFile;
    AppObject.KeyFile := KeyFile;
  end;
  {$ENDIF}
end;

procedure InitializeSSLLibs;
var
  LibSslFile: String;
  LibCryptoFile: String;
begin
  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  LibSslFile := ExtractFilePath(ParamStr(0)) + 'libssl-1_1.dll';
  LibCryptoFile := ExtractFilePath(ParamStr(0)) + 'libcrypto-1_1.dll';
  {$ENDIF}
  {$IFDEF WIN64}
  LibSslFile := ExtractFilePath(ParamStr(0)) + 'libssl-1_1-x64.dll';
  LibCryptoFile := ExtractFilePath(ParamStr(0)) + 'libcrypto-1_1-x64.dll';
  {$ENDIF}
  {$ENDIF}
  if FileExists(LibSslFile) and FileExists(LibCryptoFile) then begin
    openssl.DLLSSLName := LibSslFile;
    openssl.DLLUtilName := LibCryptoFile;
  end;
end;

end.

