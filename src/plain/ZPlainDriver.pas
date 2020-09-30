{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainLoader, ZCompatibility, Types, ZEncoding;

type
  TZAbstractPlainDriver = class;

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    function GetInstance: TZAbstractPlainDriver;
    function GetClientCodePages: TStringDynArray;
    function ValidateCharEncoding(const CharacterSetName: String; const DoArrange: Boolean = False): PZCodePage; overload;
    function ValidateCharEncoding(const CharacterSetID: Integer; const DoArrange: Boolean = False): PZCodePage; overload;
    procedure Initialize(const Location: String = '');
    function Clone: IZPlainDriver;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = zCP_NONE;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver with TZNativeLibraryLoader-object. }

  TZAbstractPlainDriver = class(TInterfacedObject, IZPlainDriver)
  protected
    FCodePages: array of TZCodePage;
    FLoader: TZNativeLibraryLoader;
    procedure LoadApi; virtual;
    function Clone: IZPlainDriver; reintroduce; virtual; abstract;
    procedure LoadCodePages; virtual; abstract;
    function GetUnicodeCodePageName: String; virtual;
  public
    constructor Create;
    constructor CreateWithLibrary(const LibName : String);
    destructor Destroy; override;
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetInstance: TZAbstractPlainDriver;
    function GetClientCodePages: TStringDynArray;
    procedure Initialize(const Location: String = ''); virtual;
    function ValidateCharEncoding(const CharacterSetName: String; const DoArrange: Boolean = False): PZCodePage; overload;
    function ValidateCharEncoding(const CharacterSetID: Integer; const DoArrange: Boolean = False): PZCodePage; overload;

    property Loader: TZNativeLibraryLoader read FLoader;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = zCP_NONE;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
    procedure ResetCodePage(const OldID: Integer; const Name: String;
      const ID:  Integer; {may be an ordinal value of predefined Types...}
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = zCP_NONE;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
  end;
  {END ADDED by fduenas 15-06-2006}

implementation

uses SysUtils{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

const
  ClientCodePageDummy: TZCodepage =
    (Name: ''; ID: 0; CharWidth: 1; Encoding: ceAnsi; CP: zCP_NONE; ZAlias: ''; IsStringFieldCPConsistent: False);

{TZAbstractPlainDriver}

function TZAbstractPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := '';
end;


{**
   Checks if the given ClientCharacterSet and returns the PZCodePage
   @param CharacterSetName the Name wich has to be validated
   @param DoArrange means if the CharacterSet is empty or unsupported then find
          a supported CodePage
   @result the PZCodePage of the ClientCharacterSet
}
function TZAbstractPlainDriver.ValidateCharEncoding(const CharacterSetName: String;
  const DoArrange: Boolean = False): PZCodePage;

  function GetClientCodePageInformations(
    const ClientCharacterSet: String): PZCodePage;
  var
    I: Integer;
    S: String;
  begin
    {now check for PlainDriver-Informations...}
    {$IF defined(LCL) or defined(UNICODE) or not defined(MSWINDOWS)} //if the user didn't set it
    if ClientCharacterSet = '' then begin
      S := UpperCase(GetUnicodeCodePageName);
      for i := Low(FCodePages) to high(FCodePages) do
        if UpperCase(FCodePages[i].Name) = S then
        begin
          Result := @FCodePages[i];
          Exit;
        end;
    end else
    {$IFEND} begin
      S := UpperCase(ClientCharacterSet);
      for i := Low(FCodePages) to high(FCodePages) do
        if UpperCase(FCodePages[i].Name) = S then begin
          Result := @FCodePages[i];
          Exit;
        end;
    end;
    Result := @ClientCodePageDummy;
  end;
begin
  Result := GetClientCodePageInformations(CharacterSetName);
  if (DoArrange) and (Result^.ZAlias <> '' ) then
    Result := ValidateCharEncoding(Result^.ZAlias); //recalls em selves
end;

{**
   Checks if the given ClientCharacterSet and returns the PZCodePage
   @param CharacterSetID the ID wich has to be validated
   @param DoArrange means if the CharacterSet is empty or unsupported then find
          a supported CodePage
   @result the PZCodePage of the ClientCharacterSet
}
function TZAbstractPlainDriver.ValidateCharEncoding(const CharacterSetID: Integer;
  const DoArrange: Boolean = False): PZCodePage;

  function GetClientCodePageInformations(const ClientCharacterSetID: Word): PZCodePage;
  var
    I: Integer;
  begin
    {now check for PlainDriver-Informations...}
    for i := Low(FCodePages) to high(FCodePages) do
      if FCodePages[i].ID = ClientCharacterSetID then
      begin
        Result := @FCodePages[i];
        Exit;
      end;
    Result := @ClientCodePageDummy;
  end;
begin
  Result := GetClientCodePageInformations(CharacterSetID);

  if (DoArrange) and (Result^.ZAlias <> '' ) then
    ValidateCharEncoding(Result^.ZAlias); //recalls em selves
end;

procedure TZAbstractPlainDriver.AddCodePage(const Name: String;
      const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = zCP_NONE; const ZAlias: String = '';
      CharWidth: Integer = 1; const ConsistentCP: Boolean = True);
begin
  SetLength(FCodePages, Length(FCodePages)+1);
  FCodePages[High(FCodePages)].Name := Name;
  FCodePages[High(FCodePages)].ID := ID;
  FCodePages[High(FCodePages)].Encoding := Encoding;
  FCodePages[High(FCodePages)].CP := CP;
  FCodePages[High(FCodePages)].CharWidth := CharWidth;
  FCodePages[High(FCodePages)].ZAlias := ZAlias;
  FCodePages[High(FCodePages)].IsStringFieldCPConsistent := ConsistentCP;

  if CP = zCP_NONE then
    FCodePages[High(FCodePages)].ZAlias := GetUnicodeCodePageName;
end;

procedure TZAbstractPlainDriver.ResetCodePage(const OldID: Integer;
      const Name: String; const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = zCP_NONE;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
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
      FCodePages[I].CharWidth := CharWidth;
      FCodePages[I].IsStringFieldCPConsistent := ConsistentCP;

      if CP = zCP_NONE then
        FCodePages[I].ZAlias := GetUnicodeCodePageName;
      Break;
    end;
end;

function TZAbstractPlainDriver.GetClientCodePages: TStringDynArray;
var
  I: Integer;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
  SetLength(Result, Length(FCodePages));
  for i := low(FCodePages) to high(FCodePages) do
    Result[i] := FCodePages[i].Name;
end;

constructor TZAbstractPlainDriver.Create;
begin
  inherited Create;
end;

destructor TZAbstractPlainDriver.Destroy;
begin
  SetLength(FCodePages, 0);
  if Assigned(FLoader) then
    FreeAndNil(FLoader);
  inherited Destroy;
end;

function TZAbstractPlainDriver.GetInstance: TZAbstractPlainDriver;
begin
  Result := Self;
end;

procedure TZAbstractPlainDriver.LoadApi;
begin

end;

constructor TZAbstractPlainDriver.CreateWithLibrary(const LibName: String);
begin
  Inherited Create;
  if Assigned(FLoader) then
  begin
    Loader.ClearLocations;
    Loader.AddLocation(LibName);
  end;
end;

procedure TZAbstractPlainDriver.Initialize(const Location: String);
begin
  If Assigned(Loader) then
    if not Loader.Loaded then
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

end.

