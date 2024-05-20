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

unit DbcProxyConfigManager;

{$I dbcproxy.inc}

interface

uses
  Classes, SysUtils, generics.collections, ZDbcIntfs, DbcProxySecurityModule,
  DbcProxyConfigStore, IniFiles;

type
  TDbcProxyConnConfig = record
    ConfigName: String;
    ClientCodepage: String;
    HostName: String;
    Database: String;
    Properties: String;
    LibraryLocation: String;
    Port: Integer;
    Protocol: String;
    SecurityModule: TZAbstractSecurityModule;
  end;

  TDbcProxyConnConfigList = TList<TDbcProxyConnConfig>;

  TDbcProxyBaseConfigManager = class(TInterfacedObject, IZDbcProxyConfigStore)
  protected
    // general stuff
    FListeningPort: Word;
    FIPAddress: String;
    FConnectionIdleTimeOut: Cardinal;
    FEnableThreading: Boolean;
    FLogFile: String;

    // SSL related stuff
    FUseSSL: Boolean;
    FHostName: String;
    FCertificateFile: String;
    FKeyFile: String;
    FKeyPasswod: String;
    {$IFDEF ENABLE_TOFU_CERTIFICATES}
    FUseTofuSSL: Boolean;
    {$ENDIF}
    function GetUseSSL: Boolean;
    function GetListeningPort: Word;
    function GetIPAddress: String;
    function GetConnectionIdleTimeout: Cardinal;
    function GetEnableThreading: Boolean;
    function GetLogFile: String;
    {$IFDEF ENABLE_TOFU_CERTIFICATES}
    function GetUseTofuSSL: Boolean;
    {$ENDIF}
    function GetHostName: String;
    function GetCertificateFile: String;
    function GetKeyFile: String;
    function GetKeyPassword: String;
  public
    property ListeningPort: Word read FListeningPort;
    property IPAddress: String read FIPAddress;
    property ConnectionIdleTimeout: Cardinal read FConnectionIdleTimeout;
    property EnableThreading: Boolean read FEnableThreading;
    property LogFile: String read FLogFile;
    property UseSSL: Boolean read FUseSSL;
    {$IFDEF ENABLE_TOFU_CERTIFICATES}
    property UseTofuSSL: Boolean read FUseTofuSSL;
    {$ENDIF}
    property HostName: String read FHostName;
    property CertificateFile: String read FCertificateFile;
    property KeyFile: String read FKeyFile;
    property KeyPasswod: String read FKeyPasswod;
    function ConstructUrl(ConfigName, UserName, Password: String; CheckSecurity: Boolean = True): String; virtual; abstract;
    procedure LoadBaseConfig; virtual; abstract;
    procedure LoadConnectionConfig; virtual; abstract;
    function GetSecurityConfig(const Name: String): IZDbcProxyKeyValueStore; virtual; abstract;
    function GetDatbaseConfig(const Name: String): IZDbcProxyKeyValueStore; virtual; abstract;
  end;

  TDbcProxyIniConfigManager = class;
  TDbcProxyIniKeyValueProvider = class(TInterfacedObject, IZDbcProxyKeyValueStore)
    protected
      FIniFile: TIniFile;
      FSectionName: String;
      FStore: TDbcProxyIniConfigManager;
    public
      function ReadString(const Key, DefaultValue: String): String;
      function ReadInteger(const Key: String; DefaultVaue: Integer): Integer;
      function ReadBool(const Key: String; DefaultValue: Boolean): Boolean;
      function GetName: String;
      function GetConfigStore: IZDbcProxyConfigStore;
      constructor Create(Store: TDbcProxyIniConfigManager; IniFile: TIniFile; SectionName: String);
  end;

  TDbcProxyIniConfigManager = class(TDbcProxyBaseConfigManager)
  protected
    // general stuff
    FConfigList: TDbcProxyConnConfigList;
    FDbPrefix: String;
    FSecurityPrefix: String;
    FIniFileName: String;
    FIniFile: TIniFile;
  public
    property DbPrefix: String read FDbPrefix;
    property SecurityPrefix: String read FSecurityPrefix;
    function ConstructUrl(ConfigName, UserName, Password: String; CheckSecurity: Boolean = True): String; override;
    procedure LoadBaseConfig; override;
    procedure LoadConnectionConfig; override;
    function GetSecurityConfig(const Name: String): IZDbcProxyKeyValueStore; override;
    function GetDatbaseConfig(const Name: String): IZDbcProxyKeyValueStore; override;
    constructor Create(IniFileName: String);
    destructor Destroy; override;
  end;

implementation

uses
  ZExceptions, zeosproxy_imp;


function TDbcProxyBaseConfigManager.GetUseSSL: Boolean;
begin
  Result := FUseSSL;
end;

function TDbcProxyBaseConfigManager.GetListeningPort: Word;
begin
  Result := FListeningPort;
end;

function TDbcProxyBaseConfigManager.GetIPAddress: String;
begin
  Result := FIPAddress;
end;

function TDbcProxyBaseConfigManager.GetConnectionIdleTimeout: Cardinal;
begin
  Result := FConnectionIdleTimeOut;
end;

function TDbcProxyBaseConfigManager.GetEnableThreading: Boolean;
begin
  Result := FEnableThreading;
end;

function TDbcProxyBaseConfigManager.GetLogFile: String;
begin
  Result := FLogFile;
end;

{$IFDEF ENABLE_TOFU_CERTIFICATES}
function TDbcProxyBaseConfigManager.GetUseTofuSSL: Boolean;
begin
  Result := FUseTofuSSL;
end;
{$ENDIF}

function TDbcProxyBaseConfigManager.GetHostName: String;
begin
  Result := FHostName;
end;

function TDbcProxyBaseConfigManager.GetCertificateFile: String;
begin
  Result := FCertificateFile;
end;

function TDbcProxyBaseConfigManager.GetKeyFile: String;
begin
  Result := FKeyFile;
end;

function TDbcProxyBaseConfigManager.GetKeyPassword: String;
begin
  Result := FKeyPasswod;
end;

{ TDbcProxyIniKeyValueProvider }

function TDbcProxyIniKeyValueProvider.ReadString(const Key, DefaultValue: String): String;
begin
  Result := FIniFile.ReadString(FSectionName, Key, DefaultValue);
end;

function TDbcProxyIniKeyValueProvider.ReadInteger(const Key: String; DefaultVaue: Integer): Integer;
begin
  Result := FIniFile.ReadInteger(FSectionName, Key, DefaultVaue);
end;

function TDbcProxyIniKeyValueProvider.ReadBool(const Key: String; DefaultValue: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(FSectionName, Key, DefaultValue);
end;

function TDbcProxyIniKeyValueProvider.GetName: String;
begin
  Result := FSectionName;
end;

function TDbcProxyIniKeyValueProvider.GetConfigStore: IZDbcProxyConfigStore;
begin
  Result := FStore as IZDbcProxyConfigStore;
end;

constructor TDbcProxyIniKeyValueProvider.Create(Store: TDbcProxyIniConfigManager; IniFile: TIniFile; SectionName: String);
begin
  inherited Create;
  FStore := Store;
  FIniFile := IniFile;
  FSectionName := SectionName;
end;

{ TDbcProxyConfigManager }

constructor TDbcProxyIniConfigManager.Create(IniFileName: String);
begin
  FIniFileName := IniFileName;
  FConfigList := TDbcProxyConnConfigList.Create;
end;

destructor TDbcProxyIniConfigManager.Destroy;
var
  x: Integer;
begin
  if Assigned(FConfigList) then begin
    for x := 0 to FConfigList.Count - 1 do
      if Assigned(FConfigList.Items[x].SecurityModule) then begin
        FConfigList.Items[x].SecurityModule.Free;
        //doesn't work, so don't even try:
        //FConfigList.Items[x].SecurityModule := nil;
      end;
    FreeAndNil(FConfigList);
  end;
  inherited;
end;

procedure TDbcProxyIniConfigManager.LoadBaseConfig;
var
  IniFile: TIniFile;
begin
  {$IFDEF LINUX}
  FLogFile := '/var/log/zeosproxy.log';
  {$ELSE}
  FLogFile := ExtractFilePath(ParamStr(0)) + 'zeosproxy.log';
  {$ENDIF}

  IniFile := TIniFile.Create(FIniFileName, TEncoding.UTF8);
  try
    FDbPrefix := IniFile.ReadString('general', 'Database Prefix', 'db.');
    FSecurityPrefix := IniFile.ReadString('general', 'Security Prefix', 'sec.');
    FListeningPort := IniFile.ReadInteger('general', 'Listening Port', 8000);
    FIPAddress := IniFile.ReadString('general', 'IP Address', '127.0.0.1');
    FConnectionIdleTimeout := IniFile.ReadInteger('general', 'Connection Idle Timeout', 86400); {Default to one day}
    FEnableThreading := IniFile.ReadBool('general', 'Enable Threading', false);
    FLogFile := IniFile.ReadString('general', 'Log File', FLogFile);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TDbcProxyIniConfigManager.LoadConnectionConfig;
var
  ConfigInfo: TDbcProxyConnConfig;
  Sections: TStringList;
  Section: String;
  ModuleType: String;
begin
  if not Assigned(Logger) then raise
    Exception.Create('Logger does not exist!');

  Sections := TStringList.Create;
  try
    FIniFile := TIniFile.Create(FIniFileName, TEncoding.UTF8);

    // load SSL configuration
    FUseSSL := FIniFile.ReadBool('general', 'use ssl', false);
    {$IFDEF ENABLE_TOFU_CERTIFICATES}
    FUseTofuSSL := FIniFile.ReadBool('general', 'use tofu ssl', false);
    if FUseSSL and FUseTofuSSL then begin
      zeosproxy_imp.Logger.Warning('SSL and TOFU SSL is enabled. Preferring SSL and disabling TOFU SSL.');
      FUseTofuSSL := false;
    end;
    {$ENDIF}

    if FUseSSL then begin
      FHostName := FIniFile.ReadString('general', 'host name', '');
      FCertificateFile := FIniFile.ReadString('general', 'Certificate File', '');
      FKeyFile := FIniFile.ReadString('general', 'Key File', '');
      FKeyPasswod := FIniFile.ReadString('general', 'Key Password', '');
    end;

    // load available connections
    FIniFile.ReadSections(Sections);
    while Sections.Count > 0 do begin
      Section := Sections.Strings[0];
      if LowerCase(Copy(Section, 1, Length(DbPrefix))) = DbPrefix then begin
        ConfigInfo.ConfigName := LowerCase(Copy(Section, Length(DbPrefix) + 1, Length(Section)));
        ConfigInfo.ClientCodepage := FIniFile.ReadString(Section, 'ClientCodepage', 'UTF8');
        ConfigInfo.Database := FIniFile.ReadString(Section, 'Database', '');
        ConfigInfo.HostName := FIniFile.ReadString(Section, 'HostName', '');
        ConfigInfo.LibraryLocation := FIniFile.ReadString(Section, 'LibraryLocation', '');
        ConfigInfo.Port := FIniFile.ReadInteger(Section, 'Port', 0);
        ConfigInfo.Protocol := FIniFile.ReadString(Section, 'Protocol', '');
        ConfigInfo.Properties := FIniFile.ReadString(Section, 'Properties', '');

        Section := FIniFile.ReadString(Section, 'Security Module', '');
        if Section <> '' then begin
          Section := SecurityPrefix + Section;
          ModuleType := FIniFile.ReadString(Section, 'Type', '');
          ConfigInfo.SecurityModule := GetSecurityModule(ModuleType);
          ConfigInfo.SecurityModule.LoadConfig(TDbcProxyIniKeyValueProvider.Create(Self, FIniFile, Section) as IZDbcProxyKeyValueStore);
        end else begin
          ConfigInfo.SecurityModule := nil;
        end;

        FConfigList.Add(ConfigInfo);
      end;
      Sections.Delete(0);
    end;
  finally
    if Assigned(Sections) then
      FreeAndNil(Sections);
    if assigned(FIniFile) then
      FreeAndNil(FIniFile);
  end;
end;

function TDbcProxyIniConfigManager.ConstructUrl(ConfigName, UserName, Password: String; CheckSecurity: Boolean = True): String;
var
  x: Integer;
  found: Boolean;
  Cfg: TDbcProxyConnConfig;
  Properties: TStringList;
begin
  ConfigName := LowerCase(ConfigName);
  found := false;
  for x := 0 to FConfigList.Count - 1 do begin
    if FConfigList.Items[x].ConfigName = ConfigName then begin
      Cfg := FConfigList.Items[x];
      found := true;
      break;
    end;
  end;

  if not found then raise EZSQLException.Create('No config named ' + ConfigName + ' was found.');

  if CheckSecurity and Assigned(Cfg.SecurityModule)
    then if not Cfg.SecurityModule.CheckPassword(UserName, Password, ConfigName) then
      raise Exception.Create('Could not validate username / password.');

  Properties := TStringList.Create;
  try
    Properties.Values['codepage'] := Cfg.ClientCodepage;
    Result := DriverManager.ConstructURL(Cfg.Protocol, Cfg.HostName, Cfg.Database, UserName, Password, Cfg.Port, Properties, Cfg.LibraryLocation);
  finally
    FreeAndNil(Properties);
  end;
end;

function TDbcProxyIniConfigManager.GetSecurityConfig(const Name: String): IZDbcProxyKeyValueStore;
begin
  Result := TDbcProxyIniKeyValueProvider.Create(self, FIniFile, FSecurityPrefix + '.' + Name) as IZDbcProxyKeyValueStore;
end;

function TDbcProxyIniConfigManager.GetDatbaseConfig(const Name: String): IZDbcProxyKeyValueStore;
begin
  Result := TDbcProxyIniKeyValueProvider.Create(self, FIniFile, FDbPrefix + '.' + Name) as IZDbcProxyKeyValueStore;
end;

end.
