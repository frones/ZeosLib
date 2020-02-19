{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for URL Classes                    }
{                                                         }
{         Originally written by Fabiano Bonin             }
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

unit ZTestURL;

interface

{$I ZCore.inc}

uses
  Classes,
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZTestCase, ZDbcIntfs;

type

  { TZURLTest }

  TZURLTest = class (TZGenericTestCase)
  published
    procedure TestAssignToUrl;
    procedure TestAssignToProperties;
    procedure TestAssignToUrl_DatabaseIsFile;
    procedure TestAssignToProperties_NoHostUserPwd;
    procedure TestAssignToProperties_NoUser;
    procedure TestAssignToProperties_Properties;
    procedure TestAssignToProperties_Properties2;
    procedure TestAssignToProperties_ProtocolOnly;
    procedure TestAssignToProperties_LibLocationOnly;
    procedure TestAssignToUrl_UID_PWD;
    procedure TestAssignToUrl_UID_PWD2;
    procedure TestEmpty;
    procedure TestAssignToUrl_NoHost;
    procedure TestAssignToProperties_Properties_NoHost;
    procedure TestSemicolons;
    procedure TestUnixPathDelimiter;
    procedure TestAssignToUrl_NoParams;
    procedure TestAssignToUrl_NoDatabase;
    procedure TestSFTicket8_HostPort_NoDB_Properties;
    procedure TestSFTicket158_LibLocWithSemicolon;
    procedure TestAssignWithOverwrite;
  end;

implementation

uses ZCompatibility, ZDbcProperties;

procedure TZURLTest.TestAssignToUrl;
var
  ZURL: TZURL;
begin
  // Test assignment to URL
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(3050, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties;
var
  ZURL: TZURL;
begin
  // Test assignement to properties
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'+LineEnding;
    CheckEquals('zdbc:oracle://127.0.0.1:3050/model?username=root;password=passwd;rolename=public', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_UID_PWD;
var
  ZURL: TZURL;
begin
  // Test assignment to URL using UID and PWD
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:odbc://localhost/model?UID=admin;PWD=pw;rolename=public';
    CheckEquals('zdbc',ZURL.Prefix);
    CheckEquals('odbc', ZURL.Protocol);
    CheckEquals('localhost', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('admin', ZURL.UserName);
    CheckEquals('pw', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_UID_PWD2;
var
  ZURL: TZURL;
begin
  // Test assignment to URL using UID and PWD in lower case and out of order
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?rolename=public;pwd=masterkey;uid=sysdba';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(3050, ZURL.Port);
    CheckEquals('model', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding,ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_NoUser;
var
  ZURL: TZURL;
begin
  // Test assignment to properties without port, user, password and properties
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'firebird-2.0';
    ZURL.HostName := '127.0.0.1';
    ZURL.Database := 'model';
    CheckEquals('zdbc:firebird-2.0://127.0.0.1/model', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_LibLocationOnly;
var
  ZURL: TZURL;
begin
  // Test assignment to properties without port, user, password and properties
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'firebird-2.0';
    ZURL.HostName := '127.0.0.1';
    ZURL.Database := 'model';
    ZURL.LibLocation := 'liblocpath';
    CheckEquals('zdbc:firebird-2.0://127.0.0.1/model?LibLocation=liblocpath', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_NoHostUserPwd;
var
  ZURL: TZURL;
begin
  // Test assignment to properties without hostname, port, user, password and properties
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'postgresql';
    ZURL.Database := 'model';
    CheckEquals('zdbc:postgresql:///model', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties;
var
  ZURL: TZURL;
begin
  // Test assignement to properties, setting user and password in properties
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'mysql';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.Properties.Values[ConnProps_Username] := 'admin';
    ZURL.Properties.Values[ConnProps_Password] := 'admin';
    ZURL.Properties.Values['prop1'] := 'prop1';
    ZURL.Properties.Values['prop2'] := 'prop2';
    CheckEquals('zdbc:mysql://127.0.0.1:3050/model?username=admin;password=admin;prop1=prop1;prop2=prop2', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties2;
var
  ZURL: TZURL;
begin
  // Test assignement to properties, setting user and password in properties as UID and PWD
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'ado';
    ZURL.HostName := 'localhost';
    ZURL.Database := 'database';
    ZURL.Properties.Values[ConnProps_UID] := 'admin';
    ZURL.Properties.Values[ConnProps_PWD] := '123';
    ZURL.Properties.Values['role'] := 'rolename';
    CheckEquals('admin', ZURL.UserName);
    CheckEquals('123', ZURL.Password);
    CheckEquals('zdbc:ado://localhost/database?username=admin;password=123;role=rolename', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestEmpty;
var
  ZURL: TZURL;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    CheckEquals('zdbc:://', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_ProtocolOnly;
var
  ZURL: TZURL;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Protocol := 'protocol';
    CheckEquals('zdbc:protocol://', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_DatabaseIsFile;
var
  ZURL: TZURL;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('127.0.0.1', ZURL.HostName);
    CheckEquals(0,ZURL.Port);
    CheckEquals('C:\database.fdb', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text,'11.8');
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_NoHost;
var
  ZURL: TZURL;
begin
  // Test assignement to URL without hostname
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('C:\database.fdb', ZURL.Database);
    CheckEquals('sysdba', ZURL.UserName);
    CheckEquals('masterkey', ZURL.Password);
    CheckEquals('rolename=public'+LineEnding, ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToProperties_Properties_NoHost;
var
  ZURL: TZURL;
begin
  // Test assignement to properties without hostname
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '';
    ZURL.Port := 0;
    ZURL.Database := 'C:\model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'+LineEnding;
    CheckEquals('zdbc:oracle:///C:\model?username=root;password=passwd;rolename=public', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestSemicolons;
var
  ZURLIn, ZURLOut: TZURL;
begin
  ZURLIn := nil;
  ZURLOut := nil;
  try
    ZURLIn := TZURL.Create;
    ZURLIn.Prefix := 'zdbc';
    ZURLIn.Protocol := 'ado';
    ZURLIn.HostName := 'localhost';
    ZURLIn.Port := 3050;
    ZURLIn.Database := 'data/;\base';
    ZURLIn.Password := 'pass/;\word';
    ZURLIn.Properties.Values[ConnProps_UID] := 'ad/;\min';
    ZURLIn.Properties.Values['role'] := 'role/;\name';
    CheckEquals('data/;\base', ZURLIn.Database);
    CheckEquals('ad/;\min', ZURLIn.UserName);
    CheckEquals('pass/;\word', ZURLIn.Password);
    CheckEquals('zdbc:ado://localhost:3050/data/'#9'\base?username=ad/'#9'\min;password=pass/'#9'\word;role=role/'#9'\name', ZURLIn.URL);

    ZURLOut := TZURL.Create;
    ZURLOut.URL := ZURLIn.URL;
    CheckEquals('zdbc', ZURLOut.Prefix);
    CheckEquals('ado', ZURLOut.Protocol);
    CheckEquals('localhost', ZURLOut.HostName);
    CheckEquals(3050, ZURLOut.Port);
    CheckEquals('data/;\base', ZURLOut.Database);
    CheckEquals('ad/;\min', ZURLOut.UserName);
    CheckEquals('pass/;\word', ZURLOut.Password);
    CheckEquals('role=role/;\name'+LineEnding, ZURLOut.Properties.Text);
    CheckEquals('role/;\name', ZURLOut.Properties.Values['role']);
  finally
    ZURLIn.Free;
    ZURLOut.Free;
  end;
end;

procedure TZURLTest.TestUnixPathDelimiter;
var
  ZURL: TZURL;
  Temp: String;
begin
  // Test assignement to properties without hostname and unix path delimiter
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '';
    ZURL.Port := 0;
    ZURL.Database := '/model/test/test.db';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'+LineEnding;
    temp := ZURL.URL;
    ZURL.URL := Temp;
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('oracle', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('/model/test/test.db', ZURL.Database);
    CheckEquals('root', ZURL.UserName);
    CheckEquals('passwd', ZURL.Password);
    CheckEquals('zdbc:oracle:////model/test/test.db?username=root;password=passwd;rolename=public', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_NoParams;
var
  ZURL: TZURL;
begin
  // Test assignment to URL without params
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:/database.fdb';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('database.fdb', ZURL.Database);
    CheckEquals('', ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
  // Test assignment to URL without params and ? sign
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:/database.fdb?';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('database.fdb', ZURL.Database);
    CheckEquals('', ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;


{ SourceForge.net Ticket #8:
TZURL.SetURL(const Value: string); maybe parse error.

e.g.
Value = 'localhost:3306?username=root;password=test'

HostName will be "localhost:3306" and Port will be 0
}
procedure TZURLTest.TestSFTicket8_HostPort_NoDB_Properties;
var
  ZURL: TZURL;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306?username=root;password=test';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('localhost', ZURL.HostName);
    CheckEquals(3306, ZURL.Port);
    CheckEquals('', ZURL.Database);
    CheckEquals('root', ZURL.UserName);
    CheckEquals('test', ZURL.Password);
    CheckEquals('zdbc:firebird-2.0://localhost:3306?username=root;password=test', ZURL.URL);
  finally
    ZURL.Free;
  end;
end;

procedure TZURLTest.TestAssignToUrl_NoDatabase;
var
  ZURL: TZURL;
begin
  // Test assignment to URL without params
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('', ZURL.Database);
    CheckEquals('', ZURL.Properties.Text);
  finally
    ZURL.Free;
  end;
end;

{ SourceForge.net Ticket #158:
  Liblocation with ":" breaks parsing
}
procedure TZURLTest.TestSFTicket158_LibLocWithSemicolon;
var
  ZURL: TZURL;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306/database?LibLocation=c:\fbclient.dll';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('localhost', ZURL.HostName);
    CheckEquals(3306, ZURL.Port);
    CheckEquals('database', ZURL.Database);
    CheckEquals('c:\fbclient.dll', ZURL.LibLocation);
  finally
    ZURL.Free;
  end;
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:///database?LibLocation=c:\fbclient.dll';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('database', ZURL.Database);
    CheckEquals('c:\fbclient.dll', ZURL.LibLocation);
  finally
    ZURL.Free;
  end;
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:///c:\test\database.fdb?LibLocation=c:\fbclient.dll';
    CheckEquals('zdbc', ZURL.Prefix);
    CheckEquals('firebird-2.0', ZURL.Protocol);
    CheckEquals('', ZURL.HostName);
    CheckEquals(0, ZURL.Port);
    CheckEquals('c:\test\database.fdb', ZURL.Database);
    CheckEquals('c:\fbclient.dll', ZURL.LibLocation);
  finally
    ZURL.Free;
  end;
end;

{ Test if assign overwrites previous values }
procedure TZURLTest.TestAssignWithOverwrite;
var
  ZURL: TZURL;
  Params: TStringList;
begin
  ZURL := nil;
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306/database';
    ZURL.UserName := 'user';
    ZURL.Password := 'pass';
    ZURL.LibLocation := 'c:\fbclient.dll';
    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306/database';
    // user/pass must be cleared
    CheckEquals('', ZURL.UserName);
    CheckEquals('', ZURL.Password);
    // lib loc must not be cleared
    CheckEquals('c:\fbclient.dll', ZURL.LibLocation);

    ZURL.UserName := 'user';
    ZURL.Password := 'pass';
    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306/database?UID=;PWD=';
    // user/pass must be cleared (empty values in props)
    CheckEquals('', ZURL.UserName);
    CheckEquals('', ZURL.Password);

    ZURL.URL := 'zdbc:firebird-2.0://localhost:3306/database?UID=user;PWD=pass;username=;password=;';
    // user/pass must have values ignoring empty values in props
    CheckEquals('user', ZURL.UserName);
    CheckEquals('pass', ZURL.Password);
  finally
    ZURL.Free;
  end;

  ZURL := nil;
  Params := TStringList.Create;
  try
    Params.Add('SomeValuelessParam');
    Params.Add('SomeEmptyParam=');
    ZURL := TZURL.Create('zdbc:firebird-2.0://localhost:3306/database?SomeValuelessParam', Params);
    // test adding duplicated parameters
    CheckEquals('zdbc:firebird-2.0://localhost:3306/database?SomeValuelessParam;SomeEmptyParam=', ZURL.URL);
  finally
    ZURL.Free;
    Params.Free;
  end;
end;

initialization
  RegisterTest('core',TZURLTest.Suite);

end.
