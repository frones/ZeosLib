{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Core Classes                }
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

program ZTestAll;

{$I ..\..\test\core\ZCore.inc}

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Types,
  SysUtils,
  TestFrameWork,
  GUITestRunner,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestCore in '..\..\test\core\ZTestCore.pas',
  ZTestParseSql in '..\..\test\parsesql\ZTestParseSql.pas',
  ZTestDbc in '..\..\test\dbc\ZTestDbc.pas',
  ZTestComponents in '..\..\test\component\ZTestComponents.pas',
  ZTestBugreports in '..\..\test\bugreport\ZTestBugreports.pas';

Var
  RunTests : ITestSuite;
  function CreateTestSuite(TestItems : TStringDynArray):ITestSuite;
  var
    TempRunTests : ITestSuite;
    I, J: integer;
    procedure CheckTestRegistry (test:ITest; ATestName:string);
    var s, c : string;
        I, p : integer;
    begin
      if Supports(test, ITestSuite) then
        begin
        p := pos ('.', ATestName);
        if p > 0 then
          begin
          s := copy (ATestName, 1, p-1);
          c := copy (ATestName, p+1, maxint);
          end
        else
          begin
          s := '';
          c := ATestName;
          end;
        if comparetext(c, test.Name) = 0 then
          begin
            TempRunTests.AddTest(test);
          end
        else if (CompareText( s, Test.Name) = 0) or (s = '') then
          for I := 0 to test.Tests.Count - 1 do
            CheckTestRegistry (ITest(test.Tests[I]), c)
        end
      else // if test is TTestCase then
        begin
        if comparetext(test.Name, ATestName) = 0 then
          begin
            TempRunTests.AddTest(test);
          end;
        end;
    end;
  begin
    TempRunTests := TTestSuite.Create('Suite');
    for J := 0 to High(CommandLineSwitches.suiteitems) do
      for I := 0 to RegisteredTests.Tests.count-1 do
        CheckTestRegistry (ITest(RegisteredTests.Tests[I]), CommandLineSwitches.suiteitems[J]);
    Result := TempRunTests;
  end;
begin
  TestGroup := COMMON_GROUP;
  If Not CommandLineSwitches.norebuild then
    RebuildTestDatabases;

  If CommandLineSwitches.Suite then
    RunTests := CreateTestSuite(CommandLineSwitches.SuiteItems)
  else
    Runtests := RegisteredTests;

  If CommandLineSwitches.batch then
    TextTestRunner.RunTest(RunTests)
  else
    GUITestRunner.RunTest(RunTests);
end.
