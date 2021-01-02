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

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, generics.collections, ZDbcIntfs;

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
  end;

  TDbcProxyConnConfigList = TList<TDbcProxyConnConfig>;

  TDbcProxyConfigManager = class
  protected
    ConfigList: TDbcProxyConnConfigList;
  public
    function ConstructUrl(ConfigName, UserName, Password: String): String;
    procedure LoadConfigInfo(SourceFile: String);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  IniFiles;

constructor TDbcProxyConfigManager.Create;
begin
  ConfigList := TDbcProxyConnConfigList.Create;
end;

destructor TDbcProxyConfigManager.Destroy;
begin
  if Assigned(ConfigList) then
    FreeAndNil(ConfigList);
  inherited;
end;

procedure TDbcProxyConfigManager.LoadConfigInfo(SourceFile: String);
var
  ConfigInfo: TDbcProxyConnConfig;
  IniFile: TIniFile;
  Sections: TStringList;
  Section: String;
begin
  IniFile := TIniFile.Create(SourceFile);
  try
    Sections := TStringList.Create;
    try
      IniFile.ReadSections(Sections);
      while Sections.Count > 0 do begin
        Section := Sections.Strings[0];

        ConfigInfo.ConfigName := LowerCase(Section);
        ConfigInfo.ClientCodepage := IniFile.ReadString(Section, 'ClientCodepage', 'UTF8');
        ConfigInfo.Database := IniFile.ReadString(Section, 'Database', '');
        ConfigInfo.HostName := IniFile.ReadString(Section, 'HostName', '');
        ConfigInfo.LibraryLocation := IniFile.ReadString(Section, 'LibraryLocation', '');
        ConfigInfo.Port := IniFile.ReadInteger(Section, 'Port', 0);
        ConfigInfo.Protocol := IniFile.ReadString(Section, 'Protocol', '');
        ConfigInfo.Properties := IniFile.ReadString(Section, 'Properties', '');

        ConfigList.Add(ConfigInfo);
        Sections.Delete(0);
      end;
    finally
      FreeAndNil(Sections);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TDbcProxyConfigManager.ConstructUrl(ConfigName, UserName, Password: String): String;
var
  x: Integer;
  found: Boolean;
  Cfg: TDbcProxyConnConfig;
begin
  ConfigName := LowerCase(ConfigName);
  found := false;
  for x := 0 to ConfigList.Count - 1 do begin
    if ConfigList.Items[x].ConfigName = ConfigName then begin
      Cfg := ConfigList.Items[x];
      found := true;
      break;
    end;
  end;

  if not found then raise Exception.Create('No config named ' + ConfigName + ' was found.');

  Result := DriverManager.ConstructURL(Cfg.Protocol, Cfg.HostName, Cfg.Database, UserName, Password, Cfg.Port, nil, Cfg.LibraryLocation);
end;

end.

