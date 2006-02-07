{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Bug Reports                 }
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

program ZTestBugReport;

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I ..\..\test\bugreport\ZBugReport.inc}

uses
  TestFrameWork,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestDbcCore in '..\..\test\bugreport\ZTestDbcCore.pas',
  ZTestDbcMySql in '..\..\test\bugreport\ZTestDbcMySql.pas',
  ZTestDbcPostgreSql in '..\..\test\bugreport\ZTestDbcPostgreSql.pas',
  ZTestDbcInterbase in '..\..\test\bugreport\ZTestDbcInterbase.pas',
  ZTestDbcDbLib in '..\..\test\bugreport\ZTestDbcDbLib.pas',
  ZTestCompCore in '..\..\test\bugreport\ZTestCompCore.pas',
  ZTestCompMySql in '..\..\test\bugreport\ZTestCompMySql.pas',
  ZTestCompMSSql in '..\..\test\bugreport\ZTestCompMSSql.pas',
  ZTestCompPostgreSql in '..\..\test\bugreport\ZTestCompPostgreSql.pas',
  ZTestCompInterbase in '..\..\test\bugreport\ZTestCompInterbase.pas',
  ZTestCompDbLib in '..\..\test\bugreport\ZTestCompDbLib.pas';

begin
  TestGroup := BUGREPORT_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
