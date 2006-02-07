{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for DbLib Component Bug Reports        }
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

unit ZTestCompDbLib;

interface

{$I ZBugReport.inc}

uses
  Classes, DB, TestFramework, ZDataset, ZConnection, ZDbcIntfs, ZBugReport,
  ZCompatibility;

type

  {** Implements a bug report test case for DbLib components. }
  ZTestCompDbLibBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: TZConnection read FConnection write FConnection;
  published
  end;

implementation

{ ZTestCompDbLibBugReport }

function ZTestCompDbLibBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase';
end;

procedure ZTestCompDbLibBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

procedure ZTestCompDbLibBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

initialization
  TestFramework.RegisterTest(ZTestCompDbLibBugReport.Suite);
end.
