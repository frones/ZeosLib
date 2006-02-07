{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Core Classes                }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
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

program ZTestCoreAll;

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I ..\..\test\core\ZCore.inc}

uses
  TestFrameWork,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestSysUtils in '..\..\test\core\ZTestSysUtils.pas',
  ZTestList in '..\..\test\core\ZTestList.pas',
  ZTestFramework in '..\..\test\core\ZTestFramework.pas',
  ZTestVariant in '..\..\test\core\ZTestVariant.pas',
  ZTestExprToken in '..\..\test\core\ZTestExprToken.pas',
  ZTestTokenizer in '..\..\test\core\ZTestTokenizer.pas',
  ZTestExpression in '..\..\test\core\ZTestExpression.pas';

begin
  TestGroup := CORE_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
