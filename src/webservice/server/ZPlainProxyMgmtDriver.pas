{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainProxyMgmtDriver;

interface

{$I ../../Zeos.inc}

uses SysUtils, Classes, ZCompatibility, ZPlainDriver;

  { ************* Plain API Function variables definition ************ }
type
  {** Represents a generic interface to DBC Proxy Managament native API. }
  IZProxyMgmtPlainDriver = interface (IZPlainDriver)
    ['{72325666-A8F8-403D-BBE9-C1030E941322}']
  end;

  {** Implements a base driver for DBC Proxy}
  TZProxyMgmtBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZProxyMgMtPlainDriver)
  private
  protected
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses ZPlainLoader, ZEncoding{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZProxyMgmtBaseDriver }

function TZProxyMgmtBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-16'
end;

procedure TZProxyMgmtBaseDriver.LoadCodePages;  //Egonhugeist
begin
  { MultiByte }
  AddCodePage('UTF-16', 4, ceUTF16, zCP_UTF16); //Setting this will be ignored by actual Excute of Plaindriver
end;

constructor TZProxyMgmtBaseDriver.Create;
begin
  inherited create;
  (* Not needed - we have no library to load *)
  (*{$IFNDEF ENABLE_INTERNAL_PROXY}
    FLoader := TZNativeLibraryLoader.Create([]);
    {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    FLoader.AddLocation(LINUX_DLL_LOCATION+'.0');
    {$ENDIF}
  {$ENDIF ENABLE_INTERNAL_PROXY}*)
end;

procedure TZProxyMgmtBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  // No need to load anything
end;

function TZProxyMgmtBaseDriver.GetProtocol: string;
begin
  Result := 'WebServiceProxyManagement';
end;

function TZProxyMgmtBaseDriver.GetDescription: string;
begin
  Result := 'Native management driver for Web Service based Proxy driver';
end;

function TZProxyMgmtBaseDriver.Clone: IZPlainDriver;
begin
  Result := TZProxyMgmtBaseDriver.Create;
end;

end.

