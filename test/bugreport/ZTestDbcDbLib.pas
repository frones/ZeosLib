{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for DBC DbLib Bug Reports            }
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

unit ZTestDbcDbLib;

interface

{$I ZBugReport.inc}

uses
  Classes, TestFramework, ZDbcIntfs, ZBugReport, ZCompatibility, ZDbcDbLib;

type

  {** Implements a DBC bug report test case for DB Lib. }
  ZTestDbcDbLibBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
  end;

implementation

{ ZTestDbcDbLibBugReport }

function ZTestDbcDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase';
end;

procedure ZTestDbcDbLibBugReport.SetUp;
begin
  Connection := CreateDbcConnection;
end;

procedure ZTestDbcDbLibBugReport.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

initialization
  TestFramework.RegisterTest(ZTestDbcDbLibBugReport.Suite);
end.
