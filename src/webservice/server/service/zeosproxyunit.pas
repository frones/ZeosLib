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

unit zeosproxyunit;

{$I ../dbcproxy.inc}
interface

uses
  Classes, SysUtils, DaemonApp, server_listener,
  // for including the Zeos drivers:
  ZDbcAdo, ZDbcASA, ZDbcDbLib, ZDbcFirebird, ZDbcInterbase6, ZDbcMySql, ZDbcODBCCon,
  ZDbcOleDB, ZDbcOracle, ZDbcPostgreSql, ZDbcSQLAnywhere, ZDbcSqLite, ZDbcProxyMgmtDriver,
  // wst
  fpc_https_server,
  //
  dbcproxycleanupthread, dbcproxyconfigutils, dbcproxycertstore
  ;

type

  { TZeosProxyDaemon }

  TZeosProxyDaemon = class(TDaemon)
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    AppObject: TwstFPHttpsListener;
    CleanupThread: TDbcProxyCleanupThread;
    procedure OnMessage(Sender : TObject; const AMsg : string);
  public

  end;

var
  ZeosProxyDaemon: TZeosProxyDaemon;

implementation

uses
  {wst}server_service_soap,
  {synapse}
  {local}zeosproxy, zeosproxy_binder, zeosproxy_imp, DbcProxyUtils,
  DbcProxyConnectionManager, DbcProxyConfigManager, ZDbcProxyManagement,
  DbcProxyFileLogger;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TZeosProxyDaemon);
end;

{$R *.lfm}

{ TZeosProxyDaemon }

procedure TZeosProxyDaemon.OnMessage(Sender : TObject; const AMsg : string);
begin
  zeosproxy_imp.Logger.Error(AMsg);
end;

procedure TZeosProxyDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
var
  configFile: String;
begin
  {$IFDEF LINUX}
  configFile := '/etc/zeosproxy.ini';
  {$ELSE}
  configFile := ExtractFilePath(ParamStr(0)) + 'zeosproxy.ini';
  {$ENDIF}
  zeosproxy_imp.Logger := TDbcProxyConsoleLogger.Create;
  ConfigManager := TDbcProxyConfigManager.Create;
  ConfigManager.LoadBaseConfig(configFile);
  if ConfigManager.LogFile <> '' then begin
    FreeAndNil(zeosproxy_imp.Logger);
    zeosproxy_imp.Logger := TDbcProxyFileLogger.Create(ConfigManager.LogFile);
  end;

  ConfigManager.LoadConnectionConfig(configFile);
  try
    zeosproxy_imp.Logger.Debug('Creating Connection Manager...');
    ConnectionManager := TDbcProxyConnectionManager.Create;

    //Server_service_RegisterBinaryFormat();
    zeosproxy_imp.Logger.Debug('Registering SOAP');
    Server_service_RegisterSoapFormat();
    //Server_service_RegisterXmlRpcFormat();

    RegisterZeosProxyImplementationFactory();
    Server_service_RegisterZeosProxyService();
    zeosproxy_imp.Logger.Debug('Creating Listener...');
    AppObject := TwstFPHttpsListener.Create(ConfigManager.IPAddress, ConfigManager.ListeningPort);
    InitializeSSLLibs;
    {$IFDEF ENABLE_TOFU_CERTIFICATES}
    if ConfigManager.UseTofuSSL then begin
      TofuCertStore := TDbcProxyCertificateStore.Create;
      zeosproxy_imp.Logger.Info('Certificate store: ' + TofuCertStore.CertificatesPath);
    end;
    {$ENDIF}
    ConfigureSSL(AppObject);
    AppObject.OnNotifyMessage := OnMessage;
    AppObject.Options := [loExecuteInThread];
    if ConfigManager.EnableThreading then begin
      AppObject.Options := AppObject.Options + [loHandleRequestInThread];
      zeosproxy_imp.Logger.Info('Handling requests in threads.');
    end;
    zeosproxy_imp.Logger.Debug('Starting the Proxy...');
    AppObject.Start();
    CleanupThread := TDbcProxyCleanupThread.Create(ConnectionManager, ConfigManager);
    CleanupThread.Start;
    zeosproxy_imp.Logger.Info('Zeos Proxy started.');
    OK := True;
  except
    on E: Exception do begin;
      OK := False;
      zeosproxy_imp.Logger.Error('Startup error: ' + E.Message);
    end;
  end;
end;

procedure TZeosProxyDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  CleanupThread.Terminate;
  CleanupThread.WaitFor;
  AppObject.Stop();
  FreeAndNil(AppObject);
  FreeAndNil(ConnectionManager);
  FreeAndNil(ConfigManager);
  zeosproxy_imp.Logger.Info('Zeos Proxy started.');
  OK := True;
end;


initialization
  RegisterDaemon;
end.

