{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{             Written by Sergey Seroukhov                 }
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

unit ZBugReport;

interface

uses ZCompatibility, ZSqlTestCase;

type

  {** Implements an abstract bug test case. }
  TZAbstractBugReportTestCase = class (TZAbstractSQLTestCase)
  protected
    function SkipClosed: Boolean;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZPortableSQLBugReportTestCase = class (TZAbstractBugReportTestCase)
  protected
    function IsProtocolValid(Name: string): Boolean; override;
    function GetSupportedProtocols: string; override;
  end;

  {**
    Implements a bug test case which runs only active protocols,
      specified by user.
  }
  TZSpecificSQLBugReportTestCase = class (TZAbstractBugReportTestCase);

implementation

uses ZSysUtils, ZTestConfig;

{ TZAbstractBugReportTestCase }

{**
  Checks is closed test cases should be skipped.
  @return <code>True</code> to skip closed test cases.
}
function TZAbstractBugReportTestCase.SkipClosed: Boolean;
begin
  Check(True);
  Result := StrToBoolEx(ReadInheritProperty(SKIP_CLOSED_KEY, FALSE_VALUE));
end;

{ TZPortableSQLBugReportTestCase }

{**
  Gets a comma separated list of all supported by this test protocols.
  @returns a list of all supported protocols.
}
function TZPortableSQLBugReportTestCase.GetSupportedProtocols: string;
begin
  Result := '';
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZPortableSQLBugReportTestCase.IsProtocolValid(Name: string): Boolean;
begin
  Result := True;
end;

end.

