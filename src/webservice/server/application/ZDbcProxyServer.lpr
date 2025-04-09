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

program ZDbcProxyServer;

{$I ../dbcproxy.inc}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    cwstring,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  //{fpc}lazutils,
  {wst}server_listener, fpc_https_server, server_service_soap,
  {synapse}
  {local}zeosproxy, zeosproxy_binder, zeosproxy_imp, DbcProxyUtils,
  DbcProxyConnectionManager, DbcProxyConfigManager, ZDbcProxyManagement,
  dbcproxycleanupthread, dbcproxysecuritymodule, DbcProxyFileLogger,
  dbcproxyconfigutils, dbcproxycertstore, DbcProxyConfigStore,
  //Zeos drivers:
  ZDbcAdo, ZDbcASA, ZDbcDbLib, ZDbcFirebird, ZDbcInterbase6, ZDbcMySql,
  ZDbcODBCCon, ZDbcOleDB, ZDbcOracle, ZDbcPostgreSql, ZDbcSQLAnywhere,
  ZDbcSqLite, ZDbcProxyMgmtDriver, DbcProxyStartupProcedures;

type

  { TZDbcProxyServer }

  TZDbcProxyServer = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure OnMessage(Sender : TObject; const AMsg : string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteProtocols;
  end;

{ TZDbcProxyServer }

procedure TZDbcProxyServer.WriteProtocols;
begin

end;

procedure TZDbcProxyServer.OnMessage(Sender : TObject; const AMsg : string);
begin
  Logger.Error(AMsg);
end;

procedure TZDbcProxyServer.DoRun;
var
  ErrorMsg: String;
  //AppObject : TwstListener;
  AppObject : TwstFPHttpsListener;
  configFile: String;
begin
  {$IFDEF LINUX}
    {$IFDEF ENABLE_DEBUG_SETTINGS}
    configFile := ExtractFilePath(ParamStr(0)) + 'zeosproxy.ini';
    {$ELSE}
    configFile := '/etc/zeosproxy.ini';
    {$IFEND}
  {$ELSE}
  configFile := ExtractFilePath(ParamStr(0)) + 'zeosproxy.ini';
  {$ENDIF}

  // quick check parameters
  ErrorMsg:=String(CheckOptions('h', 'help'));
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(UTF8Encode(ErrorMsg)));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // register available formats
  //Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  //Server_service_RegisterXmlRpcFormat();

  RegisterZeosProxyImplementationFactory();
  Server_service_RegisterZeosProxyService();

  InitializeSSLLibs;

  // initialize configuration and logger -> this is server specific
  ConfigManager := TDbcProxyIniConfigManager.Create(ConfigFile) as IZDbcProxyConfigStore;
  WriteLn('Loading Base Config...');
  ConfigManager.LoadBaseConfig;
  Logger := TDbcProxyConsoleLogger.Create;
  WriteLn('Loading Connection Config...');
  ConfigManager.LoadConnectionConfig;

  InitTofuCerts;
  CreateConnectionManager;
  InitCleanupThread;
  RegisterMdns('_zeosdbo._tcp.local');

  try
    AppObject := CreateAppObject;
    AppObject.OnNotifyMessage := OnMessage;
    WriteLn('Zeos Proxy Server listening at:');
    WriteLn('');
    WriteLn(ConstructServerURL);
    WriteLn('');
    WriteLn('Press enter to quit.');

    AppObject.Start();

    Logger.Info('Proxy started.');
    ReadLn();
    WriteLn('Stopping the Server...');
    AppObject.Stop()
  finally
    if Assigned(AppObject) then
      FreeAndNil(AppObject);
  end;

  StopServer;

  // stop program loop
  Terminate;
end;

constructor TZDbcProxyServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TZDbcProxyServer.Destroy;
begin
  inherited Destroy;
end;

procedure TZDbcProxyServer.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TZDbcProxyServer;
begin
  {$IFDEF WINDOWS}
  SetMultiByteConversionCodePage(CP_UTF8);
  {$IFEND}
  Application:=TZDbcProxyServer.Create(nil);
  Application.Title:='Zeos DBC Proxy Server';
  Application.Run;
  Application.Free;
end.

