{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Suite for Parsing SQL Classes             }
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

program ZTestParseSqlAll;

{$I ..\..\test\parsesql\ZParseSql.inc}

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFrameWork,
{$IFDEF TESTGUI}
  GUITestRunner,
{$ELSE}
  TextTestRunner,
{$ENDIF}
  ZTestConfig,
  ZSqlTestCase,
  ZTestSqlAnalyser in '..\..\test\parsesql\ZTestSqlAnalyser.pas',
  ZTestSybaseToken in '..\..\test\parsesql\ZTestSybaseToken.pas',
  ZTestMySqlToken in '..\..\test\parsesql\ZTestMySqlToken.pas',
  ZTestPostgreSqlToken in '..\..\test\parsesql\ZTestPostgreSqlToken.pas',
  ZTestScriptParser in '..\..\test\parsesql\ZTestScriptParser.pas',
  ZTestInterbaseToken in '..\..\test\parsesql\ZTestInterbaseToken.pas',
  ZTestOracleToken in '..\..\test\parsesql\ZTestOracleToken.pas',
  ZTestSqLiteToken in '..\..\test\parsesql\ZTestSqLiteToken.pas';

begin
  TestGroup := PARSESQL_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
