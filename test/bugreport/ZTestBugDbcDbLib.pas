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

unit ZTestBugDbcDbLib;

interface

{$I ZBugReport.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZCompatibility, ZSqlTestCase;

type

  {** Implements a DBC bug report test case for DB Lib. }
  ZTestDbcDbLibBugReport = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Mantis54Dbc;
  end;

implementation

{ ZTestDbcDbLibBugReport }

function ZTestDbcDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,FreeTDS_MsSQL<=6.5,FreeTDS_MsSQL-7.0,FreeTDS_MsSQL-2000,FreeTDS_MsSQL>=2005,FreeTDS_Sybase<10,FreeTDS_Sybase-10+';
end;

{ Mantis #54 }
{
The fields with data type "BigInt" in "MS-SQL" behave like "float" and not like Integer.
For example:
Suppose that 2 data bases are had. The one in MySQL and the other in MS-SQL Server, with a table each one.
The structure of the tables is the following one:

MS-SQL Server
CREATE TABLE Mantis54 (
    Key1 int NOT NULL ,
    BI bigint NULL ,
    F float NULL)

EgonHugeist:
  The resultset-Metadata returning SYBFLT8, which is probably a floating type.
  Reminder for ?missing? metadata processing.
}
procedure ZTestDbcDbLibBugReport.Mantis54Dbc;
const
  Key1_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  //BI_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  F_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
var stmnt: IZPreparedStatement;
begin
  stmnt := Connection.PrepareStatement('select * from mantis54');
  with Stmnt.ExecuteQueryPrepared do
  begin
    CheckEquals(Ord(stInteger), Ord(GetMetadata.GetColumnType(Key1_Index)));
    //CheckEquals(Ord(stLong), Ord(GetMetadata.GetColumnType(BI_Index)), 'Int64/LongInt expected'); //Not solvable
    CheckEquals(Ord(stDouble), Ord(GetMetadata.GetColumnType(F_Index)));
    Close;
  end;
  stmnt.Close;
end;

initialization
  RegisterTest('bugreport',ZTestDbcDbLibBugReport.Suite);
end.
