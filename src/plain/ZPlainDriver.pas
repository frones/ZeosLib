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

uses ZClasses, ZPlainLoader
  {$IFDEF CHECK_CLIENT_CODE_PAGE}, ZCompatibility, Types{$ENDIF};

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    {$IFDEF CHECK_CLIENT_CODE_PAGE}
    {EgonHugeist:
      Why this here? -> No one else then Plaindriver knows which Characterset
      is supported. Here i've made a intervention in dependency of used Compiler..
    }
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage; //Egonhugeist
    {$ENDIF}
    procedure Initialize;
  end;

  {ADDED by EgonHugeist 20-01-2011}
  {** implements a generic base class of a generic plain driver.
   to make the CodePage-handling tranparency for all Plain-Drivers}

  {$IFDEF CHECK_CLIENT_CODE_PAGE}
  TZGenericAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
  private
    FClientCodePage: PZCodePage;
    FCodePages: array of TZCodePage;
  protected
    procedure LoadCodePages; virtual; abstract;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = 0; const ZAlias: String = '');
    procedure ResetCodePage(const OldID: Integer; const Name: String;
      const ID:  Integer; {may be an ordinal value of predefined Types...}
      Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = 0; const ZAlias: String = '');
  public
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetSupportedClientCodePages(const IgnoreUnsupported: Boolean): TStringDynArray;
    procedure Initialize; virtual; abstract;
    destructor Destroy; override;

    function GetClientCodePageInformations(const ClientCharacterSet: String): PZCodePage;
  end;
  {$ENDIF}

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver with TZNativeLibraryLoader-object. }

  { TZAbstractPlainDriver }

  TZAbstractPlainDriver = class({$IFDEF CHECK_CLIENT_CODE_PAGE}
    TZGenericAbstractPlainDriver{$ELSE}TZAbstractObject{$ENDIF}, IZPlainDriver)
  protected
    FLoader: TZNativeLibraryLoader;
    procedure LoadApi; virtual;
  public
    constructor CreateWithLibrary(const LibName : String);
    property Loader: TZNativeLibraryLoader read FLoader;
    function GetProtocol: string; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF}abstract;
    function GetDescription: string; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF} abstract;
    procedure Initialize; {$IFDEF CHECK_CLIENT_CODE_PAGE} override; {$ELSE}virtual; {$ENDIF}
    destructor Destroy; override;
  end;
  {END ADDED by fduenas 15-06-2006}
implementation

uses ZSysUtils{$IFDEF CHECK_CLIENT_CODE_PAGE}, SysUtils{$ENDIF};

{$IFDEF CHECK_CLIENT_CODE_PAGE}

{TZGenericAbstractPlainDriver}

procedure TZGenericAbstractPlainDriver.AddCodePage(const Name: String;
      const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = 0; const ZAlias: String = '');
begin
  SetLength(FCodePages, Length(FCodePages)+1);
  FCodePages[High(FCodePages)].Name := Name;
  FCodePages[High(FCodePages)].ID := ID;
  FCodePages[High(FCodePages)].Encoding := Encoding;
  FCodePages[High(FCodePages)].CP := CP;
  FCodePages[High(FCodePages)].ZAlias := ZAlias;
end;

procedure TZGenericAbstractPlainDriver.ResetCodePage(const OldID: Integer;
      const Name: String; const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = 0; const ZAlias: String = '');
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if OldID = FCodePages[I].ID then
    begin
      FCodePages[I].ID := ID;
      FCodePages[I].Name := Name;
      FCodePages[I].Encoding := Encoding;
      FCodePages[I].CP := CP;
      FCodePages[I].ZAlias := ZAlias;
      Break;
    end;
end;

function TZGenericAbstractPlainDriver.GetSupportedClientCodePages(
  const IgnoreUnsupported: Boolean): TStringDynArray;
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if (not (FCodePages[i].Encoding = ceUnsupported) )
      or ( IgnoreUnsupported ) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := FCodePages[i].Name;
    end;
end;

destructor TZGenericAbstractPlainDriver.Destroy;
begin
  SetLength(FCodePages, 0);
  inherited Destroy;
end;

{**
   Checks if the given CharacterSet is Unicode-Save!
   @param ClientCharacterSet the Value wich hast to be compared
   @result True if ClientCharacterSet supports Unicode
}
function TZGenericAbstractPlainDriver.GetClientCodePageInformations(
  const ClientCharacterSet: String): PZCodePage;
var
  I: Integer;
begin
  {now check for PlainDriver-Informations...}
  for i := Low(FCodePages) to high(FCodePages) do
    if UpperCase(FCodePages[i].Name) = UpperCase(ClientCharacterSet) then
    begin
      Result := @FCodePages[i];
      Exit;
    end;
  Result := @ClientCodePageDummy;
end;

{$ENDIF}

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

procedure TZAbstractPlainDriver.LoadApi;
begin

end;

constructor TZAbstractPlainDriver.CreateWithLibrary(const LibName: String);
begin
  Inherited Create;
  Loader.ClearLocations;
  Loader.AddLocation(LibName);
end;

procedure TZAbstractPlainDriver.Initialize;
begin
  If Assigned(Loader) and not Loader.Loaded then
    If Loader.LoadNativeLibrary then
      LoadApi;
end;

destructor TZAbstractPlainDriver.Destroy;
begin
  FLoader.Free;
  inherited Destroy;
end;

end.

