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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, server_listener,
  // for including the Zeos drivers:
  ZDbcPostgreSql, ZDbcFirebird, ZDbcInterbase6;

type

  { TZeosProxyDaemon }

  TZeosProxyDaemon = class(TDaemon)
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    AppObject: TwstListener;
  public

  end;

var
  ZeosProxyDaemon: TZeosProxyDaemon;

implementation

uses
  {wst}fpc_http_server, server_service_soap,
  {synapse}
  {local}zeosproxy, zeosproxy_binder, zeosproxy_imp, DbcProxyUtils,
  DbcProxyConnectionManager, DbcProxyConfigManager, ZDbcProxyManagement;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TZeosProxyDaemon)
end;

{$R *.lfm}

{ TZeosProxyDaemon }

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

  ConnectionManager := TDbcProxyConnectionManager.Create;
  ConfigManager := TDbcProxyConfigManager.Create;
  ConfigManager.LoadConfigInfo(configFile);

  //Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  //Server_service_RegisterXmlRpcFormat();

  RegisterZeosProxyImplementationFactory();
  Server_service_RegisterZeosProxyService();
  AppObject := TwstFPHttpListener.Create(ConfigManager.IPAddress, ConfigManager.ListeningPort);
  (AppObject as  TwstFPHttpListener).Options := [loExecuteInThread];
  AppObject.Start();
  OK := True;
end;

procedure TZeosProxyDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  AppObject.Stop();
  FreeAndNil(AppObject);
  OK := True;
end;


initialization
  RegisterDaemon;
end.

