{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainLoader;

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize(const Location: String = '');
    function Clone: IZPlainDriver;
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver. }

  { TZAbstractPlainDriver }

  TZAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
  protected
    FLoader: TZNativeLibraryLoader;
    function Clone: IZPlainDriver; reintroduce; virtual; abstract;
    procedure LoadApi; virtual;
  public
    constructor CreateWithLibrary(const LibName : String);
    property Loader: TZNativeLibraryLoader read FLoader;
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    procedure Initialize(const Location: String = ''); virtual;
    destructor Destroy; override;
  end;
  {END ADDED by fduenas 15-06-2006}
implementation
uses SysUtils, ZSysUtils;
procedure TZAbstractPlainDriver.LoadApi;
begin

end;

constructor TZAbstractPlainDriver.CreateWithLibrary(const LibName: String);
begin
  Inherited Create;
  Loader.ClearLocations;
  Loader.AddLocation(LibName);
end;

procedure TZAbstractPlainDriver.Initialize(const Location: String);
begin
  If Assigned(Loader) and not Loader.Loaded then
  begin
    if Location <> '' then
    begin
      Loader.ClearLocations;
      Loader.AddLocation(Location);
    end;
    If Loader.LoadNativeLibrary then
      LoadApi;
  end;
end;

destructor TZAbstractPlainDriver.Destroy;
begin
  FreeAndNil(FLoader);
  inherited Destroy;
end;

end.

