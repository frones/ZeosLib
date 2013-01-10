{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZBugReport;

interface
{$I ZTestFramework.inc}

uses ZCompatibility, ZSqlTestCase, ZDbcIntfs, ZConnection, ZDataSet;

type

  {** Implements an abstract bug test case. }
  TZAbstractBugReportTestCase = class (TZAbstractSQLTestCase)
  protected
    function SkipClosed: Boolean;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZAbstractDbcSQLBugReportTestCase = class (TZAbstractBugReportTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetConnectionUrl(Param: String): string;

    property Connection: IZConnection read FConnection write FConnection;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZAbstractCompSQLBugReportTestCase = class (TZAbstractBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function CreateQuery: TZQuery;
    function CreateReadOnlyQuery: TZReadOnlyQuery;
    function CreateTable: TZTable;

    property Connection: TZConnection read FConnection write FConnection;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZPortableDbcSQLBugReportTestCase = class (TZAbstractDbcSQLBugReportTestCase)
  protected
    function IsProtocolValid(Name: string): Boolean; override;
    function GetSupportedProtocols: string; override;
  end;

  {** Implements a bug test case which runs all active protocols with MB-Chars }
  TZPortableDbcSQLBugReportTestCaseMBCs = class (TZPortableDbcSQLBugReportTestCase)
  protected
    function IsASCIITest: Boolean; override;
  end;

  {** Implements a bug test case which runs all active protocols. }
  TZPortableCompSQLBugReportTestCase = class (TZAbstractCompSQLBugReportTestCase)
  protected
    function IsProtocolValid(Name: string): Boolean; override;
    function GetSupportedProtocols: string; override;
  end;

  {** Implements a bug test case which runs all active protocols with MB-Chars }
  TZPortableCompSQLBugReportTestCaseMBCs = class (TZPortableCompSQLBugReportTestCase)
  protected
    function IsASCIITest: Boolean; override;
  end;

  {**
    Implements a dbc bug test case which runs only active protocols,
      specified by user.
  }
  TZSpecificDbcSQLBugReportTestCase = class(TZAbstractDbcSQLBugReportTestCase);

  {**
    Implements a dbc+MultiByte-chars bug test case which runs only active
      protocols, specified by user.
  }
  TZSpecificDbcSQLBugReportTestCaseMBCs = class(TZSpecificDbcSQLBugReportTestCase)
  protected
    function IsASCIITest: Boolean; override;
  end;

  {**
    Implements a dbc bug test case which runs only active protocols,
      specified by user.
  }
  TZSpecificCompSQLBugReportTestCase = class(TZAbstractCompSQLBugReportTestCase);

  {**
    Implements a dbc+MultiByte-chars bug test case which runs only active
      protocols, specified by user.
  }
  TZSpecificCompSQLBugReportTestCaseMBCs = class(TZSpecificCompSQLBugReportTestCase)
  protected
    function IsASCIITest: Boolean; override;
  end;

implementation

uses ZSysUtils, ZTestConfig, Classes, ZAbstractRODataset;

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

{ TZAbstractDbcSQLBugReportTestCase}

procedure TZAbstractDbcSQLBugReportTestCase.SetUp;
begin
  FConnection := CreateDbcConnection;
end;

procedure TZAbstractDbcSQLBugReportTestCase.TearDown;
begin
  FConnection.Close;
  FConnection := nil;
end;

function TZAbstractDbcSQLBugReportTestCase.GetConnectionUrl(Param: String): string;
var
  TempProperties: TStrings;
  I: Integer;
begin
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  TempProperties.Add(Param);
  Result := DriverManager.ConstructURL(Protocol, HostName, Database,
  UserName, Password, Port, TempProperties);
  TempProperties.Free;
end;

{ TZAbstractCompSQLBugReportTestCase }
procedure TZAbstractCompSQLBugReportTestCase.SetUp;
begin
  FConnection := CreateDatasetConnection;
end;

procedure TZAbstractCompSQLBugReportTestCase.TearDown;
begin
  FConnection.Disconnect;
  FConnection.Free;
end;

function TZAbstractCompSQLBugReportTestCase.CreateQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

function TZAbstractCompSQLBugReportTestCase.CreateReadOnlyQuery: TZReadOnlyQuery;
begin
  Result := TZReadOnlyQuery.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

function TZAbstractCompSQLBugReportTestCase.CreateTable: TZTable;
begin
  Result := TZTable.Create(nil);
  Result.Connection := FConnection;
  { do not check for Include_RealPrepared, because it's allways true if set! }
  if StrToBoolEx(FConnection.Properties.Values['preferprepared']) then
    Result.Options := Result.Options + [doPreferPrepared];
end;

{ TZPortableDbcSQLBugReportTestCase }

{**
  Gets a comma separated list of all supported by this test protocols.
  @returns a list of all supported protocols.
}
function TZPortableDbcSQLBugReportTestCase.GetSupportedProtocols: string;
begin
  Result := '';
end;

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZPortableDbcSQLBugReportTestCase.IsProtocolValid(Name: string): Boolean;
begin
  Result := True;
end;

{ TZPortableDbcSQLBugReportTestCaseMBCs }

function TZPortableDbcSQLBugReportTestCaseMBCs.IsASCIITest: Boolean;
begin
  Result := False;
end;

{ TZPortableCompSQLBugReportTestCase }

{**
  Function check name prototocol
  @param Name a protocol name
  @result true if protocol valid
}
function TZPortableCompSQLBugReportTestCase.IsProtocolValid(Name: string): Boolean;
begin
  Result := True;
end;

{**
  Gets a comma separated list of all supported by this test protocols.
  @returns a list of all supported protocols.
}
function TZPortableCompSQLBugReportTestCase.GetSupportedProtocols: string;
begin
  Result := '';
end;

{ TZPortableCompSQLBugReportTestCaseMBCs }
function TZPortableCompSQLBugReportTestCaseMBCs.IsASCIITest: Boolean;
begin
  Result := False;
end;

{ TZSpecificDbcSQLBugReportTestCaseMBCs }
function TZSpecificDbcSQLBugReportTestCaseMBCs.IsASCIITest: Boolean;
begin
  Result := False;
end;

{ TZSpecificComponentSQLBugReportTestCaseMBCs }
function TZSpecificCompSQLBugReportTestCaseMBCs.IsASCIITest: Boolean;
begin
  Result := False;
end;

end.

