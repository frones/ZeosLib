{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses;

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    {
    function GetClientVersion: Integer;
    function GetServerVersion: Integer;
    procedure GetClientVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer);
    procedure GetServerVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer);
    }
    procedure Initialize;
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver. }
  TZAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    {
    function GetClientVersion: Integer; virtual;
    function GetServerVersion: Integer; virtual;
    procedure GetClientVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer); virtual;
    procedure GetServerVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer); virtual;
    }
    procedure Initialize; virtual; abstract;
  end;
  {END ADDED by fduenas 15-06-2006}
implementation
uses ZSysUtils;
{ TZAbstractPlainDriver }

{ADDED by fduenas 15-06-2006}
{**
  Gets the clients's full version number. Initially this should be 0.
  @return the clients's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    client_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetClientVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Client's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetClientVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetClientVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}
{**
  Gets the servers's full version number. Initially this should be 0.
  @return the server's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    server_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetServerVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Server's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetServerVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetServerVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}
end.

