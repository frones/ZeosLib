{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for SQL String Classes             }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{               Written by Sergey Seroukhov               }
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

unit ZTestSqlStrings;

interface

uses TestFramework, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken;

type

  {** Implements a test case for class TZSqlStrings. }
  TZTestSQLStringsCase = class(TTestCase)
  private
    SQLStrings: TZSQLStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStatements;
    procedure TestParams;
    procedure TestUncompleted;
  end;

implementation

uses Classes, ZDbcUtils;

{ TZTestSqlStringsCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLStringsCase.SetUp;
begin
  SQLStrings := TZSQLStrings.Create;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLStringsCase.TearDown;
begin
  SQLStrings.Free;
  SQLStrings := nil;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestSQLStringsCase.TestParams;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(4, SQLStrings.ParamCount);
  CheckEquals('ID', SQLStrings.ParamNames[0]);
  CheckEquals('NAME', SQLStrings.ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.ParamNames[2]);
  CheckEquals('NEW_NAME', SQLStrings.ParamNames[3]);

  SQLStrings.Clear;
  CheckEquals(0, SQLStrings.ParamCount);
end;

{**
  Runs a test for SQL statements.
}
procedure TZTestSQLStringsCase.TestStatements;
var
  SQLScript: string;
begin
  SQLScript := 'INSERT INTO department VALUES (:ID, :NAME, :NEW_ADDRESS);'
    + #10 + 'UPDATE department SET dep_name=:NEW_NAME, dep_address=:NEW_ADDRESS'
    + ' WHERE id_dep=:Id AND dep_name=:Name;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('INSERT INTO department VALUES (?, ?, ?)',
    SQLStrings.Statements[0].SQL);
  CheckEquals(3, SQLStrings.Statements[0].ParamCount);
  CheckEquals('ID', SQLStrings.Statements[0].ParamNames[0]);
  CheckEquals('NAME', SQLStrings.Statements[0].ParamNames[1]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[0].ParamNames[2]);

  CheckEquals('UPDATE department SET dep_name=?, dep_address=?'
    + ' WHERE id_dep=? AND dep_name=?', SQLStrings.Statements[1].SQL);
  CheckEquals(4, SQLStrings.Statements[1].ParamCount);
  CheckEquals('NEW_NAME', SQLStrings.Statements[1].ParamNames[0]);
  CheckEquals('NEW_ADDRESS', SQLStrings.Statements[1].ParamNames[1]);
  CheckEquals('ID', SQLStrings.Statements[1].ParamNames[2]);
  CheckEquals('NAME', SQLStrings.Statements[1].ParamNames[3]);
end;

{**
  Runs a test for uncompleted SQL statements.
}
procedure TZTestSQLStringsCase.TestUncompleted;
var
  SQLScript: string;
begin
  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo;';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);

  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo';
  SQLStrings.Text := SQLScript;

  CheckEquals(2, SQLStrings.StatementCount);
  CheckEquals('SELECT * FROM people', SQLStrings.Statements[0].SQL);
  CheckEquals('SELECT * FROM cargo', SQLStrings.Statements[1].SQL);
end;

initialization
  TestFramework.RegisterTest(TZTestSQLStringsCase.Suite);
end.
