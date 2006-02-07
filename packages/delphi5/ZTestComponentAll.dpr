{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Suite for Database Components            }
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

program ZTestComponentAll;

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I ..\..\test\component\ZComponent.inc}

uses
  TestFrameWork,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestSqlStrings in '..\..\test\component\ZTestSqlStrings.pas',
  ZTestSqlProcessor in '..\..\test\component\ZTestSqlProcessor.pas',
  ZTestStoredProcedure in '..\..\test\component\ZTestStoredProcedure.pas',
  ZTestExecuteSql in '..\..\test\component\ZTestExecuteSql.pas',
  ZTestSqlTypes in '..\..\test\component\ZTestSqlTypes.pas',
  ZTestDataSetGeneric in '..\..\test\component\ZTestDataSetGeneric.pas',
  ZTestMasterDetail in '..\..\test\component\ZTestMasterDetail.pas',
  ZTestData in '..\..\test\component\ZTestData.pas';

begin
  TestGroup := COMPONENT_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.

