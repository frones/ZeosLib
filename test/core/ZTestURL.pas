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
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions,ZURL;

type
  TZURLTest = class (TZCoreGenericTestCase)
  published
    procedure Test;
  end;

implementation

procedure TZURLTest.Test;
var
  ZURL: TZURL;
begin
  // Test assignment to URL
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey;rolename=public';
    Check(ZURL.Prefix = 'zdbc', '1.1');
    Check(ZURL.Protocol = 'firebird-2.0', '1.2');
    Check(ZURL.HostName = '127.0.0.1', '1.3');
    Check(ZURL.Port = 3050, '1.4');
    Check(ZURL.Database = 'model', '1.5');
    Check(ZURL.UserName = 'sysdba', '1.6');
    Check(ZURL.Password = 'masterkey', '1.7');
    Check(ZURL.Properties.Text = 'rolename=public'#13#10, '1.8');
  finally
    ZURL.Free;
  end;

  // Test assignement to properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'#13#10;
    Check(ZURL.URL = 'zdbc:oracle://127.0.0.1:3050/model?username=root;password=passwd;rolename=public', '2.1');
  finally
    ZURL.Free;
  end;

  // Test assignment to URL using UID and PWD
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:odbc://localhost/model?UID=admin;PWD=pw;rolename=public';
    Check(ZURL.Prefix = 'zdbc', '3.1');
    Check(ZURL.Protocol = 'odbc', '3.2');
    Check(ZURL.HostName = 'localhost', '3.3');
    Check(ZURL.Port = 0, '3.4');
    Check(ZURL.Database = 'model', '3.5');
    Check(ZURL.UserName = 'admin', '3.6');
    Check(ZURL.Password = 'pw', '3.7');
    Check(ZURL.Properties.Text = 'rolename=public'#13#10, '3.8');
  finally
    ZURL.Free;
  end;

  // Test assignment to URL using UID and PWD in lower case and out of order
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1:3050/model?rolename=public;pwd=masterkey;uid=sysdba';
    Check(ZURL.Prefix = 'zdbc', '4.1');
    Check(ZURL.Protocol = 'firebird-2.0', '4.2');
    Check(ZURL.HostName = '127.0.0.1', '4.3');
    Check(ZURL.Port = 3050, '4.4');
    Check(ZURL.Database = 'model', '4.5');
    Check(ZURL.UserName = 'sysdba', '4.6');
    Check(ZURL.Password = 'masterkey', '4.7');
    Check(ZURL.Properties.Text = 'rolename=public'#13#10, '4.8');
  finally
    ZURL.Free;
  end;

  // Test assignment to properties without port, user, password and properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'firebird-2.0';
    ZURL.HostName := '127.0.0.1';
    ZURL.Database := 'model';
    Check(ZURL.URL = 'zdbc:firebird-2.0://127.0.0.1/model', '5.1');
  finally
    ZURL.Free;
  end;

  // Test assignment to properties without hostname, port, user, password and properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'postgresql';
    ZURL.Database := 'model';
    Check(ZURL.URL = 'zdbc:postgresql:/model', '6.1');
  finally
    ZURL.Free;
  end;

  // Test assignement to properties, setting user and password in properties
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'mysql';
    ZURL.HostName := '127.0.0.1';
    ZURL.Port := 3050;
    ZURL.Database := 'model';
    ZURL.Properties.Values['username'] := 'admin';
    ZURL.Properties.Values['password'] := 'admin';
    ZURL.Properties.Values['prop1'] := 'prop1';
    ZURL.Properties.Values['prop2'] := 'prop2';
    Check(ZURL.URL = 'zdbc:mysql://127.0.0.1:3050/model?username=admin;password=admin;prop1=prop1;prop2=prop2', '7.1');
  finally
    ZURL.Free;
  end;

  // Test assignement to properties, setting user and password in properties as UID and PWD
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'ado';
    ZURL.HostName := 'localhost';
    ZURL.Database := 'database';
    ZURL.Properties.Values['UID'] := 'admin';
    ZURL.Properties.Values['PWD'] := '123';
    ZURL.Properties.Values['role'] := 'rolename';
    Check(ZURL.UserName = 'admin', '8.1');
    Check(ZURL.Password = '123', '8.2');
    Check(ZURL.URL = 'zdbc:ado://localhost/database?username=admin;password=123;role=rolename', '8.1');
  finally
    ZURL.Free;
  end;

  try
    ZURL := TZURL.Create;
    Check(ZURL.URL = 'zdbc::', '9.1');
  finally
    ZURL.Free;
  end;

  try
    ZURL := TZURL.Create;
    ZURL.Protocol := 'protocol';
    Check(ZURL.URL = 'zdbc:protocol:', '10.1');
  finally
    ZURL.Free;
  end;

  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0://127.0.0.1/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    Check(ZURL.Prefix = 'zdbc', '11.1');
    Check(ZURL.Protocol = 'firebird-2.0', '11.2');
    Check(ZURL.HostName = '127.0.0.1', '11.3');
    Check(ZURL.Port = 0, '11.4');
    Check(ZURL.Database = 'C:\database.fdb', '11.5');
    Check(ZURL.UserName = 'sysdba', '11.6');
    Check(ZURL.Password = 'masterkey', '11.7');
    Check(ZURL.Properties.Text = 'rolename=public'#13#10, '11.8');
  finally
    ZURL.Free;
  end;

  // Test assignement to URL without hostname
  try
    ZURL := TZURL.Create;
    ZURL.URL := 'zdbc:firebird-2.0:/C:\database.fdb?username=sysdba;password=masterkey;rolename=public';
    Check(ZURL.Prefix = 'zdbc', '12.1');
    Check(ZURL.Protocol = 'firebird-2.0', '12.2');
    Check(ZURL.HostName = '', '12.3');
    Check(ZURL.Port = 0, '12.4');
    Check(ZURL.Database = 'C:\database.fdb', '12.5');
    Check(ZURL.UserName = 'sysdba', '12.6');
    Check(ZURL.Password = 'masterkey', '12.7');
    Check(ZURL.Properties.Text = 'rolename=public'#13#10, '12.8');
  finally
    ZURL.Free;
  end;

  // Test assignement to properties without hostname
  try
    ZURL := TZURL.Create;
    ZURL.Prefix := 'zdbc';
    ZURL.Protocol := 'oracle';
    ZURL.HostName := '';
    ZURL.Port := 0;
    ZURL.Database := 'C:\model';
    ZURL.UserName := 'root';
    ZURL.Password := 'passwd';
    ZURL.Properties.Text := 'rolename=public'#13#10;
    Check(ZURL.URL = 'zdbc:oracle:/C:\model?username=root;password=passwd;rolename=public', '13.1');
  finally
    ZURL.Free;
  end;
end;

initialization
  {$IFNDEF FPC}TestFramework.{$ENDIF}RegisterTest(TZURLTest.Suite);

end.
