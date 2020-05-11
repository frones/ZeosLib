{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Core Classes                }
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

{$APPTYPE CONSOLE}

uses
  Types,
  SysUtils,
  TestFrameWork,
  GUITestRunner,
  TextTestRunner,
  XMLTestRunner2,
  ZTestConfig,
  ZSqlTestCase,
  ZTestPerformance in '..\..\test\performance\ZTestPerformance.pas',
  ZTestExpression in '..\..\test\core\ZTestExpression.pas',
  ZTestExprToken in '..\..\test\core\ZTestExprToken.pas',
  ZTestFramework in '..\..\test\core\ZTestFramework.pas',
  ZTestList in '..\..\test\core\ZTestList.pas',
  ZTestSysUtils in '..\..\test\core\ZTestSysUtils.pas',
  ZTestTokenizer in '..\..\test\core\ZTestTokenizer.pas',
  ZTestVariant in '..\..\test\core\ZTestVariant.pas',
  ZTestInterbaseToken in '..\..\test\parsesql\ZTestInterbaseToken.pas',
  ZTestMySqlToken in '..\..\test\parsesql\ZTestMySqlToken.pas',
  ZTestOracleToken in '..\..\test\parsesql\ZTestOracleToken.pas',
  ZTestPostgreSqlToken in '..\..\test\parsesql\ZTestPostgreSqlToken.pas',
  ZTestScriptParser in '..\..\test\parsesql\ZTestScriptParser.pas',
  ZTestSqlAnalyser in '..\..\test\parsesql\ZTestSqlAnalyser.pas',
  ZTestSqLiteToken in '..\..\test\parsesql\ZTestSqLiteToken.pas',
  ZTestSybaseToken in '..\..\test\parsesql\ZTestSybaseToken.pas',
  ZTestBugCompADO in '..\..\test\bugreport\ZTestBugCompADO.pas',
  ZTestBugCompASA in '..\..\test\bugreport\ZTestBugCompASA.pas',
  ZTestBugCompCore in '..\..\test\bugreport\ZTestBugCompCore.pas',
  ZTestBugCompDbLib in '..\..\test\bugreport\ZTestBugCompDbLib.pas',
  ZTestBugCompInterbase in '..\..\test\bugreport\ZTestBugCompInterbase.pas',
  ZTestBugCompMSSql in '..\..\test\bugreport\ZTestBugCompMSSql.pas',
  ZTestBugCompMySql in '..\..\test\bugreport\ZTestBugCompMySql.pas',
  ZTestBugCompOracle in '..\..\test\bugreport\ZTestBugCompOracle.pas',
  ZTestBugCompPostgreSql in '..\..\test\bugreport\ZTestBugCompPostgreSql.pas',
  ZTestBugCompSQLite in '..\..\test\bugreport\ZTestBugCompSQLite.pas',
  ZTestBugDbcASA in '..\..\test\bugreport\ZTestBugDbcASA.pas',
  ZTestBugDbcCore in '..\..\test\bugreport\ZTestBugDbcCore.pas',
  ZTestBugDbcDbLib in '..\..\test\bugreport\ZTestBugDbcDbLib.pas',
  ZTestBugDbcInterbase in '..\..\test\bugreport\ZTestBugDbcInterbase.pas',
  ZTestBugDbcMSSQL in '..\..\test\bugreport\ZTestBugDbcMSSQL.pas',
  ZTestBugDbcMySql in '..\..\test\bugreport\ZTestBugDbcMySql.pas',
  ZTestBugDbcOracle in '..\..\test\bugreport\ZTestBugDbcOracle.pas',
  ZTestBugDbcPostgreSql in '..\..\test\bugreport\ZTestBugDbcPostgreSql.pas',
  ZTestConnection in '..\..\test\component\ZTestConnection.pas',
  ZTestDataSetGeneric in '..\..\test\component\ZTestDataSetGeneric.pas',
  ZTestEvents in '..\..\test\component\ZTestEvents.pas',
  ZTestExecuteSql in '..\..\test\component\ZTestExecuteSql.pas',
  ZTestMasterDetail in '..\..\test\component\ZTestMasterDetail.pas',
  ZTestSorting in '..\..\test\component\ZTestSorting.pas',
  ZTestSqlMetadata in '..\..\test\component\ZTestSqlMetadata.pas',
  ZTestSqlProcessor in '..\..\test\component\ZTestSqlProcessor.pas',
  ZTestSqlStrings in '..\..\test\component\ZTestSqlStrings.pas',
  ZTestSqlTypes in '..\..\test\component\ZTestSqlTypes.pas',
  ZTestStoredProcedure in '..\..\test\component\ZTestStoredProcedure.pas',
  ZTestDbcADO in '..\..\test\dbc\ZTestDbcADO.pas',
  ZTestDbcASA in '..\..\test\dbc\ZTestDbcASA.pas',
  ZTestDbcASAMetadata in '..\..\test\dbc\ZTestDbcASAMetadata.pas',
  ZTestDbcCache in '..\..\test\dbc\ZTestDbcCache.pas',
  ZTestDbcCachedResultSet in '..\..\test\dbc\ZTestDbcCachedResultSet.pas',
  ZTestDbcDriver in '..\..\test\dbc\ZTestDbcDriver.pas',
  ZTestDbcGeneric in '..\..\test\dbc\ZTestDbcGeneric.pas',
  ZTestDbcInterbase in '..\..\test\dbc\ZTestDbcInterbase.pas',
  ZTestDbcInterbaseMetadata in '..\..\test\dbc\ZTestDbcInterbaseMetadata.pas',
  ZTestDbcMetadata in '..\..\test\dbc\ZTestDbcMetadata.pas',
  ZTestDbcMsSql in '..\..\test\dbc\ZTestDbcMsSql.pas',
  ZTestDbcMySql in '..\..\test\dbc\ZTestDbcMySql.pas',
  ZTestDbcMySqlMetadata in '..\..\test\dbc\ZTestDbcMySqlMetadata.pas',
  ZTestDbcODBc in '..\..\test\dbc\ZTestDbcODBc.pas',
  ZTestDbcOracle in '..\..\test\dbc\ZTestDbcOracle.pas',
  ZTestDbcPostgreSql in '..\..\test\dbc\ZTestDbcPostgreSql.pas',
  ZTestDbcPostgreSqlMetadata in '..\..\test\dbc\ZTestDbcPostgreSqlMetadata.pas',
  ZTestDbcResolver in '..\..\test\dbc\ZTestDbcResolver.pas',
  ZTestDbcResultSet in '..\..\test\dbc\ZTestDbcResultSet.pas',
  ZTestDbcResultSetMetadata in '..\..\test\dbc\ZTestDbcResultSetMetadata.pas',
  ZTestDbcSqLite in '..\..\test\dbc\ZTestDbcSqLite.pas',
  ZTestDbcURL in '..\..\test\dbc\ZTestDbcURL.pas',
  ZTestDbcUtils in '..\..\test\dbc\ZTestDbcUtils.pas';

begin
  TestGroup := COMMON_GROUP;
  ReportMemoryLeaksOnShutDown := CommandLineSwitches.memcheck;
  If Not CommandLineSwitches.norebuild then
    RebuildTestDatabases;

  If CommandLineSwitches.sqlmonitor then
    EnableZSQLMonitor;

  If CommandLineSwitches.batch then
    TextTestRunner.RunTest(CreateTestSuite).Free
  else if CommandLineSwitches.xml then
    XMLTestRunner2.RunTest(CreateTestSuite, CommandLineSwitches.xmlfilename).Free	
  else
    GUITestRunner.RunTest(CreateTestSuite);
end.
