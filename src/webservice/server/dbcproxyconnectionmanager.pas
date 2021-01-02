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

unit DbcProxyConnectionManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ZDbcProxyManagement, ZDbcIntfs, generics.collections;

type
  TDbcProxyConnectionList = TList<TDbcProxyConnection>;

  TDbcProxyConnectionManager = class
  protected
    Synchronizer: TMultiReadExclusiveWriteSynchronizer;
    List: TDbcProxyConnectionList;
  public
    function GetConnectionCount: SizeInt;
    function FindConnection(ID: String): TDbcProxyConnection;
    function GetConnection(Index: SizeInt): TDbcProxyConnection;
    function AddConnection(Connection: IZConnection): String;
    procedure RemoveConnection(ID: String);
    function LockConnection(ID: String): TDbcProxyConnection; overload;
    function LockConnection(Index: SizeInt): TDbcProxyConnection; overload;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TDbcProxyConnectionManager.Create;
begin
  inherited;
  Synchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  List := TDbcProxyConnectionList.Create;
end;

destructor TDbcProxyConnectionManager.Destroy;
begin
  if Assigned(Synchronizer) then
    FreeAndNil(Synchronizer);
  if Assigned(List) then
    FreeAndNil(List);
  inherited;
end;

function TDbcProxyConnectionManager.FindConnection(ID: String): TDbcProxyConnection;
var
  x: Integer;
begin
  Result := nil;
  Synchronizer.Beginread;
  try
    for x := 0 to List.Count - 1 do begin
      if List.Items[x].ID = ID then begin
        Result := List.Items[x];
        break;
      end;
    end;
  finally
    Synchronizer.Endread;
  end;
end;

function TDbcProxyConnectionManager.GetConnection(Index: SizeInt): TDbcProxyConnection;
begin
  Result := Nil;
  Synchronizer.Beginread;
  try
    if List.Count >= Index then
      Result := Nil
    else
      Result := List.Items[Index];
  finally
    Synchronizer.Endread;
  end;
end;

function TDbcProxyConnectionManager.LockConnection(ID: String): TDbcProxyConnection;
begin
  Result := FindConnection(ID);
  if Assigned(Result) then Result.Lock else raise Exception.Create('No connection with ID ' + ID + ' was found!');
end;

function TDbcProxyConnectionManager.LockConnection(Index: SizeInt): TDbcProxyConnection;
begin
  Result := GetConnection(Index);
  if Assigned(Result) then Result.Lock else raise Exception.Create('No connection with Index ' + IntToStr(Index) + ' was found!');
end;

function TDbcProxyConnectionManager.AddConnection(Connection: IZConnection): String;
var
  ProxyConn: TDbcProxyConnection;
begin
  ProxyConn := TDbcProxyConnection.Create(Connection);
  Result := ProxyConn.ID;
  Synchronizer.Beginwrite;
  try
    List.Add(ProxyConn);
  finally
    Synchronizer.Endwrite;
  end;
end;

procedure TDbcProxyConnectionManager.RemoveConnection(ID: String);
var
  Conn: TDbcProxyConnection;
  x: Integer;
begin
  Synchronizer.Beginwrite;
  try
    for x := 0 to List.Count - 1 do begin
      if List.Items[x].ID = ID then begin
        Conn := List.Items[x];
        List.Delete(x);
        break;
      end;
    end;
  finally
    Synchronizer.Endwrite;
  end;

  if Assigned(Conn) then
    Conn := nil;
end;

function TDbcProxyConnectionManager.GetConnectionCount: SizeInt;
begin
  Synchronizer.Beginread;
  try
    Result := List.Count;
  finally
    Synchronizer.Endread;
  end;
end;

end.

