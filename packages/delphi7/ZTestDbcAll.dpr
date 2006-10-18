{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Test Suite for Database Connectivity Classes       }
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

program ZTestDbcAll;

{$I ..\..\test\dbc\ZDbc.inc}

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
  ZTestDbcUtils in '..\..\test\dbc\ZTestDbcUtils.pas',
  ZTestDbcCache in '..\..\test\dbc\ZTestDbcCache.pas',
  ZTestDbcCachedResultSet in '..\..\test\dbc\ZTestDbcCachedResultSet.pas',
  ZTestDbcResultSet in '..\..\test\dbc\ZTestDbcResultSet.pas',
  ZTestDbcResultSetMetadata in '..\..\test\dbc\ZTestDbcResultSetMetadata.pas',
  ZTestDbcResolver in '..\..\test\dbc\ZTestDbcResolver.pas',
  ZTestDbcMetadata in '..\..\test\dbc\ZTestDbcMetadata.pas',
{$IFDEF ENABLE_INTERBASE}
  ZTestDbcInterbaseMetadata in '..\..\test\dbc\ZTestDbcInterbaseMetadata.pas',
  ZTestDbcInterbase in '..\..\test\dbc\ZTestDbcInterbase.pas',
{$ENDIF}
{$IFDEF ENABLE_MYSQL}
  ZTestDbcMySqlMetadata in '..\..\test\dbc\ZTestDbcMySqlMetadata.pas',
  ZTestDbcMySql in '..\..\test\dbc\ZTestDbcMySql.pas',
{$ENDIF}
{$IFDEF ENABLE_POSTGRESQL}
  ZTestDbcPostgreSqlMetadata in '..\..\test\dbc\ZTestDbcPostgreSqlMetadata.pas',
  ZTestDbcPostgreSql in '..\..\test\dbc\ZTestDbcPostgreSql.pas',
{$ENDIF}
  ZTestDbcMsSql in '..\..\test\dbc\ZTestDbcMsSql.pas',
{$IFDEF ENABLE_ORACLE}
  ZTestDbcOracle in '..\..\test\dbc\ZTestDbcOracle.pas',
{$ENDIF}
{$IFDEF ENABLE_SQLITE}
  ZTestDbcSqLite in '..\..\test\dbc\ZTestDbcSqLite.pas',
{$ENDIF}
  ZTestDbcGeneric in '..\..\test\dbc\ZTestDbcGeneric.pas';

begin
  TestGroup := DBC_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
