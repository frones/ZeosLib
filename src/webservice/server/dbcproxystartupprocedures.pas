unit DbcProxyStartupProcedures;

{$I dbcproxy.inc}

interface

uses
  Classes, SysUtils,
  DbcProxyConnectionManager, dbcproxycleanupthread, fpc_https_server
  {$IFDEF ENABLE_DNSSD}, mdnsService{$ENDIF};

procedure CreateConnectionManager;
procedure InitCleanupThread;
// Requires a working copnfig manager.
procedure InitTofuCerts;
procedure StopServer;
function CreateAppObject: TwstFPHttpsListener;
procedure RegisterMdns(ServiceType: String);

implementation

uses
  zeosproxy_imp, dbcproxycertstore, dbcproxyconfigutils;

var
  CleanupThread: TDbcProxyCleanupThread;
  {$IFDEF WITH_DNSSD}
  mdnsService: TMdnsService;
  {$ENDIF}

procedure CreateConnectionManager;
begin
  ConnectionManager := TDbcProxyConnectionManager.Create;
end;

procedure InitCleanupThread;
begin
  CleanupThread := TDbcProxyCleanupThread.Create(ConnectionManager, ConfigManager);
  CleanupThread.Start;
end;

procedure InitTofuCerts;
begin
  {$IFDEF ENABLE_TOFU_CERTIFICATES}
  if ConfigManager.UseTofuSSL then begin
    TofuCertStore := TDbcProxyCertificateStore.Create;
    zeosproxy_imp.Logger.Info('Certificate store: ' + TofuCertStore.CertificatesPath);
  end;
  {$ENDIF}
end;

procedure StopServer;
begin
  if Assigned(CleanupThread) then begin
    CleanupThread.Terminate;
    CleanupThread.WaitFor;
  end;
  if Assigned(ConnectionManager) then
    FreeAndNil(ConnectionManager);
  if Assigned(ConfigManager) then
    FreeAndNil(ConfigManager);
end;

function CreateAppObject: TwstFPHttpsListener;
begin
  Result := TwstFPHttpsListener.Create(ConfigManager.IPAddress, ConfigManager.ListeningPort);
  ConfigureSSL(Result);
  Result.Options := Result.Options + [loExecuteInThread];
  if ConfigManager.EnableThreading then begin
    Result.Options := Result.Options + [loHandleRequestInThread];
    Logger.Info('Threading is enabled.');
  end;
end;

procedure RegisterMdns(ServiceType: String);
begin
  {$IFDEF WITH_DNSSD}
  mdnsService := TMdnsService.Create(nil);
  mdnsService.PortNumber := ConfigManager.ListeningPort;
  mdnsService.ServiceName := ServiceType;
  mdnsService.RegisterService;
  {$ENDIF}
end;

end.

