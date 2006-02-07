{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Query Components               }
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

unit ZTestExecuteSql;

interface

uses
  TestFramework, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestExecSQLCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    Query: TZReadOnlyQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParams;
    procedure TestKeepParams;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestExecSQLCase }

{**
  Prepares initial data before each test.
}
procedure TZTestExecSQLCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  Query := TZReadOnlyQuery.Create(nil);
  Query.Connection := Connection;
  Query.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestExecSQLCase.TearDown;
begin
  Query.Close;
  Query.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestExecSQLCase.TestParams;
begin
  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;

  Query.SQL.Text := 'INSERT INTO department (dep_id, dep_name, dep_address)'
    + ' VALUES(:Id, :Name, :Address)';
  CheckEquals(3, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  CheckEquals('Name', Query.Params[1].Name);
  Query.Params[1].DataType := ftString;
  Query.Params[1].Value := 'AAA';
  CheckEquals('Address', Query.Params[2].Name);
  Query.Params[2].DataType := ftString;
  Query.Params[2].Value := 'BBB';
  Query.ExecSQL;
  CheckEquals(1, Query.RowsAffected);

  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;
  Query.ExecSQL;
  CheckEquals(1, Query.RowsAffected);
end;

{**
  Tests keeping parameters across changes in statements.
}
procedure TZTestExecSQLCase.TestKeepParams;
begin
  Query.SQL.Text := 'DELETE FROM department WHERE dep_id=:Id';
  CheckEquals(1, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  Query.Params[0].DataType := ftInteger;
  Query.Params[0].Value := TEST_ROW_ID;

  Query.SQL.Text := 'INSERT INTO department (dep_id, dep_name, dep_address)'
    + ' VALUES(:Id, :Name, :Address)';
  CheckEquals(3, Query.Params.Count);
  CheckEquals('Id', Query.Params[0].Name);
  CheckEquals(TEST_ROW_ID, Query.Params[0].Value);
  CheckEquals(Ord(ftInteger), Ord(Query.Params[0].DataType));
  CheckEquals('Name', Query.Params[1].Name);
  CheckEquals(True, Query.Params[1].IsNull);
  CheckEquals('Address', Query.Params[2].Name);
  CheckEquals(True, Query.Params[2].IsNull);
end;

initialization
  TestFramework.RegisterTest(TZTestExecSQLCase.Suite);
end.
