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

unit dbcproxycleanupthread;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DbcProxyConnectionManager, DbcProxyConfigManager, DbcProxyConfigProvider;

type
  TDbcProxyCleanupThread = class(TThread)
    private
      FConnManager: TDbcProxyConnectionManager;
      FIdleTimeout: Integer;
    protected
      procedure Execute; override;
    public
      constructor Create(ConnManager: TDbcProxyConnectionManager; ConfigManager: IZDbcProxyConfigStore);
  end;

implementation

uses
  DateUtils, ZDbcProxyManagement, zeosproxy_imp;

constructor TDbcProxyCleanupThread.Create(ConnManager: TDbcProxyConnectionManager; ConfigManager: IZDbcProxyConfigStore);
begin
  inherited Create(True);
  FConnManager := ConnManager;
  FIdleTimeout := ConfigManager.ConnectionIdleTimeout;
end;

procedure TDbcProxyCleanupThread.Execute;
var
  MaxTime: TDateTime;
  x: SizeInt;
  Conn: TDbcProxyConnection;
  ConnID: String;
  y: Integer;
  ActionTime: Integer;
begin
  try
    y := 0;
    ActionTime := FIdleTimeout div 2;
    If ActionTime = 0 then
      ActionTime := 1;
    Logger.Info('Starting Cleanup Thread');
    Logger.Info('Idle Timeout: ' + IntToStr(FIdleTimeout));
    Logger.Info('Action Time: ' + IntToStr(ActionTime));
    while not Terminated do begin
      y := y mod ActionTime;
      if y = 0 then begin
        Logger.Info('Inspecting Connections');
        MaxTime := IncSecond(Now, FIdleTimeout * (-1));
        for X := FConnManager.GetConnectionCount - 1 downto 0 do begin
          Conn := FConnManager.GetConnection(x);
          if Assigned(Conn) then begin
            if Conn.LastAccessTime <= MaxTime then begin
              Conn.Lock;
              try
                ConnID := Conn.ID;
                Conn.ZeosConnection.Close;
                Logger.Info('Closed Connection ' + ConnID + ' to ' + Conn.ZeosConnection.GetURL);
              finally
                Conn.Unlock;
              end;
            end else begin
              ConnID := '';
            end;
            if ConnID <> '' then begin
              FConnManager.RemoveConnection(ConnID);
            end;
          end;
        end;
      end;
      inc(y);
      Sleep(1000);
    end;
  except
    on E: Exception do begin
      Logger.Error('Cleanup thread created exception: ' + E.Message);
    end;
  end;
end;

end.

