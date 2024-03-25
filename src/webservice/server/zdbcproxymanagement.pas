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

unit ZDbcProxyManagement;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ZDbcIntfs, SyncObjs;

type
  TDbcProxyConnection = class
  protected
    FID: String;
    FNr: Int64;
    FZeosConnection: IZConnection;
    FLastAccessTime: TDateTime;
    FCriticalSection: TCriticalSection;
    FCreationTime: TDateTime;
    FOriginalUser: String;
    FDatabaseName: String;
  public
    constructor Create(AConnection: IZConnection); virtual;
    destructor Destroy; override;
    property ZeosConnection: IZConnection read FZeosConnection;
    property ID: String read FID;
    property Nr: Int64 read FNr;
    property LastAccessTime: TDateTime read FLastAccessTime;
    property CreationTime: TDateTime read FCreationTime;
    property OriginalUser: String read FOriginalUser write FOriginalUser;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    procedure Lock;
    procedure Unlock;
  end;

procedure RaiseNotImplemented(FunctionName: String);

implementation

uses
  ZExceptions;

var
  NextSessionNr: Int64;

function GetNextSessionNr: Int64;
begin
  Result := NextSessionNr;
  Inc(NextSessionNr);
end;

procedure RaiseNotImplemented(FunctionName: String);
begin
  raise EZSQLException.Create('Function ' + FunctionName + ' is not implemented yet!');
end;

constructor TDbcProxyConnection.Create(AConnection: IZConnection);
var
  UUID: TGuid;
begin
  FCriticalSection := TCriticalSection.Create;
  CreateGUID(UUID);
  FID := GUIDToString(UUID);
  FZeosConnection := AConnection;
  FLastAccessTime := Now;
  FCreationTime := LastAccessTime;
  FNr := GetNextSessionNr;
end;

destructor TDbcProxyConnection.Destroy;
begin
  if Assigned(FCriticalSection) then
    FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TDbcProxyConnection.Lock;
begin
  FLastAccessTime := Now;
  FCriticalSection.Enter;
end;

procedure TDbcProxyConnection.Unlock;
begin
  FLastAccessTime := Now;
  FCriticalSection.Leave;
end;

initialization
  NextSessionNr := 1;

end.
