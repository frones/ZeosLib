{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 5.0                         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainMySql5;

interface

{$I ZPlain.inc}
{$DEFINE MYSQL_5_API}

{$J+}
{$Z4}
{$IFNDEF FPC}
{$A8}
{$ENDIF}

uses Classes, ZPlainLoader, ZCompatibility, ZPlainMySqlConstants;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS_50_DLL_LOCATION = 'libmysql50.dll';
  WINDOWS_51_DLL_LOCATION = 'libmysql51.dll';
  WINDOWS_50_DLL_LOCATION_EMBEDDED = 'libmysqld50.dll';
  WINDOWS_51_DLL_LOCATION_EMBEDDED = 'libmysqld51.dll';
  LINUX1_DLL_LOCATION  = 'libmysqlclient.so.15';

{ ****************** Plain API Types definition ***************** }

type

{ ************** Plain API Function types definition ************* }
{$I ZPlainMysqlFunc.inc}

{ ************** Collection of Plain API Function types definition ************* }
MYSQL5_API = record
{$I ZPlainMysqlVar.inc}
end;

type
  {** Implements a loader for MySQL native library. }
  TZMySQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    api_rec : MYSQL5_API;
    destructor Destroy; override;
    function Load: Boolean; override;
  end;

var

  LibraryLoader: TZMySQLNativeLibraryLoader;
  LibraryLoaderEmbedded: TZMySQLNativeLibraryLoader;

implementation

{ TZMySQLNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZMySQLNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

{ ************** Load adresses of API Functions ************* }
{$I ZPlainMysqlRec.inc}
end;

{**
  Destroys the library and cleanups the memory.
}
destructor TZMySQLNativeLibraryLoader.Destroy;
begin
  if (Loaded) and (@api_rec.mysql_server_end <> nil) then
    api_rec.mysql_server_end;
  inherited Destroy;
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [WINDOWS_51_DLL_LOCATION
    , WINDOWS_50_DLL_LOCATION
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION
{$ENDIF}
    ]);
  LibraryLoaderEmbedded := TZMySQLNativeLibraryLoader.Create(
    [WINDOWS_51_DLL_LOCATION_EMBEDDED
    , WINDOWS_50_DLL_LOCATION_EMBEDDED
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION_EMBEDDED
{$ENDIF}
    ]);
{$ELSE}
  LibraryLoader := TZMySQLNativeLibraryLoader.Create(
    [LINUX1_DLL_LOCATION,LINUX2_DLL_LOCATION]);
  LibraryLoaderEmbedded := TZMySQLNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION_EMBEDDED]);
{$ENDIF}
{$UNDEF MYSQL_5_API}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
  if Assigned(LibraryLoaderEmbedded) then
    LibraryLoaderEmbedded.Free;
end.
