{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for TZURL Class                    }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}
unit ZURL;

interface
{$I ZCore.inc}

uses
  Classes,
  SysUtils;

type
  TZURLStringList = Class(TStringList)
  protected
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
    function GetURLText: String;
  public
    property URLText: String read GetURLText;
  end;

  TZURL = class
  private
    FPrefix: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FLibLocation: String;
    FProperties: TZURLStringList;
    FOnPropertiesChange: TNotifyEvent;
    procedure SetPrefix(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetLibLocation: String;
    procedure SetLibLocation(const Value: String);
    function GetURL: string;
    procedure SetURL(const Value: string);
    procedure DoOnPropertiesChange(Sender: TObject);
  public
    constructor Create; overload;
    constructor Create(const AURL: String); overload;
    constructor Create(const AURL: String; Info: TStrings); overload;
    constructor Create(const AURL: TZURL); overload;
    constructor Create(Const AURL, AHostName: string; const APort: Integer;
      const ADatabase, AUser, APassword: string; Info: TStrings); overload;

    destructor Destroy; override;
    property Prefix: string read FPrefix write SetPrefix;
    property Protocol: string read FProtocol write SetProtocol;
    property HostName: string read FHostName write SetHostName;
    property Port: Integer read FPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property LibLocation: string read GetLibLocation write SetLibLocation;
    property Properties: TZURLStringList read FProperties;
    property URL: string read GetURL write SetURL;

    property OnPropertiesChange: TNotifyEvent read FOnPropertiesChange write FOnPropertiesChange;
  end;

implementation

uses ZCompatibility;

{TZURLStringList}
function TZURLStringList.GetTextStr: string;
begin
  Result := inherited GetTextStr;
  Result := StringReplace(Result, #9, ';', [rfReplaceAll]); //unescape the #9 char to ';'
end;

procedure TZURLStringList.SetTextStr(const Value: string);
begin
  inherited SetTextStr(StringReplace(Value, ';', #9, [rfReplaceAll])); //escape the ';' char to #9
end;

function TZURLStringList.GetURLText: String;
begin
  Result := StringReplace(GetTextStr, ';', #9, [rfReplaceAll]); //keep all ';' escaped
  Result := StringReplace(Result, LineEnding, ';', [rfReplaceAll]);  //return a URL-usable string
  if Result[Length(Result)] = ';' then
    Result := Copy(Result, 1, Length(Result)-1);
end;

{ TZURL }

constructor TZURL.Create;
begin
  inherited;

  FPrefix := 'zdbc';
  FProperties := TZURLStringList.Create;
  FProperties.CaseSensitive := False;
  FProperties.OnChange := DoOnPropertiesChange;
end;

constructor TZURL.Create(const AURL: String);
begin
  Create;
  Self.URL := AURL;
end;

constructor TZURL.Create(const AURL: String; Info: TStrings);
begin
  Create(AURL);
  if Assigned(Info) then
    Self.Properties.AddStrings(Info);
end;

constructor TZURL.Create(const AURL: TZURL);
begin
  Create(AURL.URL);
end;

constructor TZURL.Create(Const AURL, AHostName: string; const APort: Integer;
  const ADatabase, AUser, APassword: string; Info: TStrings);
begin
  Create(AURL);
  Self.HostName := AHostName;
  Self.Port := APort;
  Self.Database := ADataBase;
  Self.UserName := AUser;
  Self.Password := APassword;
  if Assigned(Info) then
    Self.Properties.AddStrings(Info);
end;

destructor TZURL.Destroy;
begin
  FProperties.Free;

  inherited;
end;

procedure TZURL.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TZURL.SetProtocol(const Value: string);
begin
  FProtocol := Value;
end;

procedure TZURL.SetHostName(const Value: string);
begin
  FHostName := StringReplace(Value, ';', #9, [rfReplaceAll]); //escape the ';' char to #9
end;

procedure TZURL.SetConnPort(const Value: Integer);
begin
  FPort := Value;
end;

function TZURL.GetDatabase: string;
begin
  Result := StringReplace(FDatabase, #9, ';', [rfReplaceAll]); //unescape the #9 char to ';'
end;

procedure TZURL.SetDatabase(const Value: string);
begin
  FDatabase := StringReplace(Value, ';', #9, [rfReplaceAll]); //escape the ';' char to #9
end;

function TZURL.GetUserName: string;
begin
  Result := StringReplace(FUserName, #9, ';', [rfReplaceAll]); //unescape the #9 char to ';'
end;

procedure TZURL.SetUserName(const Value: string);
begin
  FUserName := StringReplace(Value, ';', #9, [rfReplaceAll]); //escape the ';' char to #9
end;

function TZURL.GetPassword: string;
begin
  Result := StringReplace(FPassword, #9, ';', [rfReplaceAll]); //unescape the #9 char to ';'
end;

procedure TZURL.SetPassword(const Value: string);
begin
  FPassword := StringReplace(Value, ';', #9, [rfReplaceAll]); //escape the ';' char to #9
end;

function TZURL.GetLibLocation: String;
begin
  Result := StringReplace(FLibLocation, #9, ';', [rfReplaceAll]); //unescape the #9 char to ';'
end;

procedure TZURL.SetLibLocation(const Value: String);
begin
  FLibLocation := StringReplace(Value, ';', #9, [rfReplaceAll]); //escape the ';' char to #9
end;

function TZURL.GetURL: string;
var
  hasParamPart : boolean;
  procedure AddParamPart(const ParamPart: String);
  begin
    if hasParamPart then
      Result := Result + ';'
    else
      Result := Result + '?';
    Result := Result + ParamPart;
    hasParamPart := True;
  end;

begin
  Result := '';
  hasParamPart := false;

  // Prefix
  Result := Result + Prefix + ':';

  // Protocol
  Result := Result + Protocol + ':';

  Result := Result + '//'; //Allways set the doubleslash  to avoid unix '/' path issues if host is empty

  // HostName/Port
  if HostName <> '' then
  begin
    Result := Result + HostName;
    if Port <> 0 then
      Result := Result + ':' + IntToStr(Port);
  end;

  // Database
  if Database <> '' then
    Result := Result + '/' + FDatabase;

  // UserName
  if FUserName <> '' then
    AddParamPart('username=' + FUserName);

  // Password
  if FPassword <> '' then
    AddParamPart('password=' + FPassword);

  // Properties
  if Properties.Count > 0 then
    AddParamPart(Properties.GetURLText); //Adds the escaped string

  // LibLocation
    if FLibLocation <> '' then
      AddParamPart('LibLocation='+ FLibLocation);
end;

procedure TZURL.SetURL(const Value: string);
var
  APrefix: string;
  AProtocol: string;
  AHostName: string;
  APort: string;
  ADatabase: string;
  AUserName: string;
  APassword: string;
  AProperties: TStrings;
  AValue: string;
  I: Integer;
begin
  APrefix := '';
  AProtocol := '';
  AHostName := '';
  APort := '';
  ADatabase := '';
  AUserName := '';
  APassword := '';
  AProperties := TStringList.Create;

  try
    AValue := Value;

    // APrefix
    I := Pos(':', AValue);
    if I = 0 then
      raise Exception.Create('TZURL.SetURL - The prefix is missing');
    APrefix := Copy(AValue, 1, I - 1);
    Delete(AValue, 1, I);

    // AProtocol
    I := Pos(':', AValue);
    if I = 0 then
      raise Exception.Create('TZURL.SetURL - The protocol is missing');
    AProtocol := Copy(AValue, 1, I - 1);
    Delete(AValue, 1, I);

    // AHostName
    if Pos('//', AValue) = 1 then
    begin
      Delete(AValue, 1, 2);
      if (Pos(':', AValue) > 0) and ((Pos(':', AValue) < Pos('/', AValue)) or (Pos('/', AValue)=0)) then
        AHostName := Copy(AValue, 1, Pos(':', AValue) - 1)
      else if Pos('/', AValue) > 0 then
        AHostName := Copy(AValue, 1, Pos('/', AValue) - 1)
      else if Pos('?', AValue) > 0 then
        AHostName := Copy(AValue, 1, Pos('?', AValue) - 1)
      else
        AHostName := AValue;

      Delete(AValue, 1, Length(AHostName));

      // APort
      if Pos(':', AValue) = 1 then
      begin
        Delete(AValue, 1, 1);
        if Pos('/', AValue) > 0 then
          APort := Copy(AValue, 1, Pos('/', AValue) - 1)
        else if Pos('?', AValue) > 0 then
          APort := Copy(AValue, 1, Pos('?', AValue) - 1)
        else
          APort := AValue;

        Delete(AValue, 1, Length(APort));
      end;
    end;

    if Pos('/', AValue) = 1 then
      Delete(AValue, 1, 1);

    // ADatabase
    I := Pos('?', AValue);
    if I > 0 then
    begin
      ADatabase := Copy(AValue, 1, I - 1);
      Delete(AValue, 1, I);
      AProperties.Text := StringReplace(AValue, ';', LineEnding, [rfReplaceAll]);
    end
    else
      ADatabase := AValue;

    FPrefix := APrefix;
    FProtocol := AProtocol;
    FHostName := AHostName;
    FPort := StrToIntDef(APort, 0);
    FDatabase := ADatabase;
    FUserName := AUserName;
    FPassword := APassword;
    FProperties.Text := AProperties.Text;
  finally
    AProperties.Free;
  end;
end;

procedure TZURL.DoOnPropertiesChange(Sender: TObject);
begin
  FProperties.OnChange := nil;
  try
    if FProperties.Values['UID'] <> '' then
    begin
      UserName := FProperties.Values['UID'];
      FProperties.Delete(FProperties.IndexOfName('UID'));
    end;

    if FProperties.Values['PWD'] <> '' then
    begin
      Password := FProperties.Values['PWD'];
      FProperties.Delete(FProperties.IndexOfName('PWD'));
    end;

    if FProperties.Values['username'] <> '' then
    begin
      UserName := FProperties.Values['username'];
      FProperties.Delete(FProperties.IndexOfName('username'));
    end;

    if FProperties.Values['password'] <> '' then
    begin
      Password := FProperties.Values['password'];
      FProperties.Delete(FProperties.IndexOfName('password'));
    end;

    if FProperties.Values['LibLocation'] <> '' then
    begin
      LibLocation := FProperties.Values['LibLocation'];
      FProperties.Delete(FProperties.IndexOfName('LibLocation'));
    end;

  finally
    FProperties.OnChange := DoOnPropertiesChange;
  end;

  if Assigned(FOnPropertiesChange) then
    FOnPropertiesChange(Sender);
end;

end.

