{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Test Suite for Bug Reports                 }
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

program ZTestBugReport;

{$IFNDEF TESTGUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I ../../test/bugreport/ZBugReport.inc}

uses
  TestFrameWork,
  TextTestRunner,
  ZTestConfig,
  ZSqlTestCase,
  ZTestDbcCore in '../../test/bugreport/ZTestDbcCore.pas',
  ZTestDbcMySql in '../../test/bugreport/ZTestDbcMySql.pas',
  ZTestDbcPostgreSql in '../../test/bugreport/ZTestDbcPostgreSql.pas',
  ZTestDbcInterbase in '../../test/bugreport/ZTestDbcInterbase.pas',
  ZTestDbcDbLib in '../../test/bugreport/ZTestDbcDbLib.pas',
  ZTestCompCore in '../../test/bugreport/ZTestCompCore.pas',
  ZTestCompMySql in '../../test/bugreport/ZTestCompMySql.pas',
  ZTestCompPostgreSql in '../../test/bugreport/ZTestCompPostgreSql.pas',
  ZTestCompInterbase in '../../test/bugreport/ZTestCompInterbase.pas',
  ZTestCompDbLib in '../../test/bugreport/ZTestCompDbLib.pas';

begin
  TestGroup := BUGREPORT_TEST_GROUP;
  RebuildTestDatabases;
{$IFDEF TESTGUI}
  GUITestRunner.RunRegisteredTests;
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
