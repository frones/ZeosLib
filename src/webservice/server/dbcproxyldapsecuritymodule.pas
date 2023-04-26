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

unit DbcProxyLdapSecurityModule;

{$I dbcproxy.inc}

interface

{$IFDEF ENABLE_LDAP_SECURITY}

uses
  Classes, SysUtils, DbcProxySecurityModule, IniFiles, ldapsend;

type
  TZLdapSecurityModule = class(TZAbstractSecurityModule)
  protected
    FHostName: String;
    FUserNameMask: String;
    FUserLookupExpression: String;
    FBaseDN: String;
    FReplacementUser: String;
    FReplacementPassword: String;
  public
    function CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean; override;
    procedure LoadConfig(IniFile: TIniFile; const Section: String); override;
  end;

{$ENDIF}

implementation

{$IFDEF ENABLE_LDAP_SECURITY}

function TZLdapSecurityModule.CheckPassword(var UserName, Password: String; const ConnectionName: String): Boolean;
var
  Ldap: TLdapSend;
  Filter: String;
  AttributeList: TStringList;
begin
  Result := False;
  Ldap := TLDAPSend.Create;
  try
    Ldap.TargetHost := FHostName;
    Ldap.UserName := Format(FUserNameMask, [UserName]);
    Ldap.Password := Password;
    if Ldap.Login then begin
      if Ldap.Bind then begin
        if FUserLookupExpression = '' then
          Result := True
        else begin
          Filter := Format(FUserLookupExpression, [UserName]);
          AttributeList := TStringList.Create;
          try
            AttributeList.Add('*');
            if Ldap.Search(FBaseDN, false, Filter, AttributeList) then
              Result := Ldap.SearchResult.Count > 0;
          finally
            FreeAndNil(AttributeList);
          end;
        end;
      end;
      Ldap.Logout;
    end;
  finally
    FreeAndNil(Ldap);
  end;

  if Result then begin
    if FReplacementUser <> '' then
      UserName := FReplacementUser;
    if FReplacementPassword <> '' then
      Password := FReplacementPassword;
  end;
end;

procedure TZLdapSecurityModule.LoadConfig(IniFile: TIniFile; const Section: String);
begin
  inherited;
  FHostName := IniFile.ReadString(Section, 'Host Name', '');
  FUserNameMask := IniFile.ReadString(Section, 'User Name Mask', '%s');
  FUserLookupExpression := IniFile.ReadString(Section, 'User Lookup Expression', '(sAMAccountName=%s)');
  FBaseDN := IniFile.ReadString(Section, 'Base DN', '');
  FReplacementUser := IniFile.ReadString(Section, 'Replacement User', '');
  FReplacementPassword := IniFile.ReadString(Section, 'Replacement Password', '');
end;

{$ENDIF}

end.

