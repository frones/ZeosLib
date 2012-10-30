{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Interbase Component Bug Reports    }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZTestBugCompSQLite;

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
  ZTestCompSQLiteBugReport = class(TZSpecificSQLBugReportTestCase)
  private
    FConnection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: TZConnection read FConnection write FConnection;

  published
    procedure Mantis248_TestNonASCIICharSelect;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestCase, ZTestConsts, ZSqlUpdate, ZSqlTestCase;

const

  Str1 = 'This license, the Lesser General Public License, applies to some specially designated software packages--typically libraries--of the Free Software Foundation and other authors who decide to use it.  You can use it too, but we suggest you first think ...';
  Str2 = 'ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...';
  Str3 = 'ќдной из наиболее';
  Str4 = 'тривиальных задач';
  Str5 = 'решаемых многими';
  Str6 = 'коллективами программистов';

{ ZTestCompSQLiteBugReport }

function ZTestCompSQLiteBugReport.GetSupportedProtocols: string;
begin
  Result := 'sqlite,sqlite-3';
end;

procedure ZTestCompSQLiteBugReport.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

procedure ZTestCompSQLiteBugReport.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  NUMBER must be froat
}
procedure ZTestCompSQLiteBugReport.Mantis248_TestNonASCIICharSelect;
const TestRowID = 248;
var
  Query: TZQuery;
  RowCounter: Integer;
  I: Integer;
  procedure InsertValues(s_char, s_varchar, s_nchar, s_nvarchar: String);
  begin
    if Connection.DbcConnection.AutoEncodeStrings or Connection.UTF8StringsAsWideField then
    begin
      Query.ParamByName('s_id').AsInteger := TestRowID+RowCounter;
      Query.ParamByName('s_char').AsString := s_char;
      Query.ParamByName('s_varchar').AsString := s_varchar;
      Query.ParamByName('s_nchar').AsString := s_nchar;
      Query.ParamByName('s_nvarchar').AsString := s_nvarchar;
    end
    else
    begin
      Query.ParamByName('s_id').AsInteger := TestRowID+RowCounter;
      Query.ParamByName('s_char').AsString := UTF8Encode(WideString(s_char));
      Query.ParamByName('s_varchar').AsString := UTF8Encode(WideString(s_varchar));
      Query.ParamByName('s_nchar').AsString := UTF8Encode(WideString(s_nchar));
      Query.ParamByName('s_nvarchar').AsString := UTF8Encode(WideString(s_nvarchar));
    end;
    Query.ExecSQL;
    inc(RowCounter);
  end;

begin
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Connection.Connect;
  try
    RowCounter := 0;
    Query.SQL.Text := 'Insert into string_values (s_id, s_char, s_varchar, s_nchar, s_nvarchar)'+
      ' values (:s_id, :s_char, :s_varchar, :s_nchar, :s_nvarchar)';
    InsertValues(str2, str2, str2, str2);
    InsertValues(str3, str3, str3, str3);
    InsertValues(str4, str4, str4, str4);
    InsertValues(str5, str5, str5, str5);
    InsertValues(str6, str6, str6, str6);

    if Connection.DbcConnection.AutoEncodeStrings or Connection.UTF8StringsAsWideField then
    begin
      Query.SQL.Text := 'select * from string_values where s_id > '+IntToStr(TestRowID-1);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 5);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Str2+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 1);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Str3+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Str4+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Str5+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Str6+'%', #39);
      Query.Open;
    end
    else
    begin
      Query.SQL.Text := 'select * from string_values where s_id > '+IntToStr(TestRowID-1);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 5);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Utf8Encode(Str2)+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 1);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Utf8Encode(Str3)+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Utf8Encode(Str4)+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Utf8Encode(Str5)+'%', #39);
      Query.Open;
      CheckEquals(True, Query.RecordCount = 2);
      Query.SQL.Text := 'select * from string_values where s_char like '+AnsiQuotedStr('%'+Utf8Encode(Str6)+'%', #39);
      Query.Open;
    end;
  finally
    for i := TestRowID to TestRowID+RowCounter do
    begin
      Query.SQL.Text := 'delete from string_values where s_id = '+IntToStr(i);
      Query.ExecSQL;
    end;
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompSQLiteBugReport.Suite);
end.
