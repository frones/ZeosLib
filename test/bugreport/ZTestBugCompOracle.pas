{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Interbase Component Bug Reports    }
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

unit ZTestBugCompOracle;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZConnection, ZDbcIntfs, ZBugReport,
  {$IFNDEF LINUX}
    DBCtrls,
  {$ENDIF}
  ZCompatibility;
type

  {** Implements a bug report test case for Oracle components. }
  ZTestCompOracleBugReport = class(TZSpecificCompSQLBugReportTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestNum1;
    procedure TestNestedDataSetFields1;
    procedure TestNestedDataSetFields2;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestCase, ZTestConsts, ZSqlUpdate, ZSqlTestCase;

{ ZTestCompOracleBugReport }

function ZTestCompOracleBugReport.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

{**
  NUMBER must be froat
}
procedure ZTestCompOracleBugReport.TestNum1;
var
  Query: TZQuery;
begin
  if SkipTest then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM Table_Num1';
    Query.Open;
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType), 'id field type');
    CheckEquals(Ord(ftFloat), Ord(Query.Fields[1].DataType), 'Num field type');
    CheckEquals(1, Query.Fields[0].AsInteger, 'id value');
    CheckEquals(54321.0123456789, Query.Fields[1].AsFloat, 1E-11, 'Num value');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompOracleBugReport.TestNestedDataSetFields1;
var
  Query: TZQuery;
begin
  if SkipTest then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM SYSTEM.AQ$_QUEUES';
    Query.Open;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompOracleBugReport.TestNestedDataSetFields2;
var
  Query: TZQuery;
begin
  if SkipTest then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM customers';
    Query.Open;
  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompOracleBugReport.Suite);
end.
