{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Testing Framework              }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{              Written by Sergey Merkuriev                }
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

unit ZTestFramework;

interface

{$I ZCore.inc}

uses TestFramework, ZSysUtils, SysUtils, ZTestDefinitions, ZCompatibility;

type

  {** Implements a test case for TZPortableSQLTestCase. }
  TZTestPortableSQLTestCase = class(TZCorePortableSQLTestCase)
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

  {** Implements a test case for TZSpecificSQLTestCase. }
  TZTestSpecificSQLTestCase = class(TZCoreSpecificSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

implementation


{ TZTestPortableSQLTestCase }

{**
  Runs the first test.
}
procedure TZTestPortableSQLTestCase.TestOne;
begin
  Check(True);
  PrintLn('*** Test # 1 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestPortableSQLTestCase.TestTwo;
begin
  Check(True);
  PrintLn('*** Test # 2 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'postgresql') then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestPortableSQLTestCase.TestTree;
begin
  Check(True);
  PrintLn('*** Test # 3 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mssql') then
    PrintLn('--- Part specific for mssql');
  PrintLn;
end;

{ TZTestSpecificSQLTestCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestSpecificSQLTestCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,postgresql';
end;

{**
  Runs the first test.
}
procedure TZTestSpecificSQLTestCase.TestOne;
begin
  Check(True);
  PrintLn('### Test # 1 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestSpecificSQLTestCase.TestTwo;
begin
  Check(True);
  PrintLn('### Test # 2 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'postgresql') then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestSpecificSQLTestCase.TestTree;
begin
  Check(True);
  PrintLn('### Test # 3 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if StartsWith(Protocol, 'mysql') then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

initialization
  TestFramework.RegisterTest(TZTestPortableSQLTestCase.Suite);
  TestFramework.RegisterTest(TZTestSpecificSQLTestCase.Suite);
end.
