{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for DBC DbLib Bug Reports            }
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

unit ZTestBugDbcMSSQL;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZCompatibility, ZSqlTestCase;

type

  {** Implements a DBC bug report test case for DB Lib. }
  ZTestDbcMSSQLBugReport = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestTicket375;
  end;

implementation

{ ZTestDbcMSSQLBugReport }

function ZTestDbcMSSQLBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,FreeTDS_MsSQL<=6.5,FreeTDS_MsSQL-7.0,FreeTDS_MsSQL-2000,FreeTDS_MsSQL>=2005,FreeTDS_Sybase<10,FreeTDS_Sybase-10+,odbc_a,odbc_w,OleDB,ado';
end;

(*
When you execute query without parameters the library uses sp_prepexec for the
  odbc and ado (probably oledb) protocols making it impossible to maintain the
  session context between queries in the same connection as it is bound to the
  scope of the parameterized statement.
*)
procedure ZTestDbcMSSQLBugReport.TestTicket375;
var
  Stmt: IZStatement;
  RS: IZResultSet;
begin
  Stmt := Connection.CreateStatement;
  Check(Stmt <> nil);
  RS := nil;
  try
    With Stmt do begin
      ExecuteUpdate('create table #t375 (i int)');
      CheckEquals(1, ExecuteUpdate('insert into #t375 values (0)'), 'UpdateCount');//Invalid object name #t
      RS := ExecuteQuery('select * from #t375');
      Check(RS.Next, 'there is a row in the tmp table');
      RS.Close;
      RS := nil;
      ExecuteUpdate('drop table #t375');
      Close;
    end;
  finally
    Stmt := nil
  end;
end;

initialization
  RegisterTest('bugreport',ZTestDbcMSSQLBugReport.Suite);
end.
