{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Case for Master-Detail Links             }
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

unit ZTestMasterDetail;

interface

uses
  TestFramework, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestMasterDetailCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    MasterDataSource: TDataSource;
    MasterQuery: TZQuery;
    DetailQuery: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDataSource;
    procedure TestMasterFields;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestMasterDetailCase }

{**
  Prepares initial data before each test.
}
procedure TZTestMasterDetailCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  MasterQuery := TZQuery.Create(nil);
  MasterQuery.Connection := Connection;

  MasterDataSource := TDataSource.Create(nil);
  MasterDataSource.DataSet := MasterQuery;

  DetailQuery := TZQuery.Create(nil);
  DetailQuery.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestMasterDetailCase.TearDown;
begin
  DetailQuery.Close;
  DetailQuery.Free;

  MasterQuery.Close;
  MasterQuery.Free;

  MasterDataSource.Free;

  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestMasterDetailCase.TestDataSource;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people WHERE p_dep_id=:dep_id';
  DetailQuery.DataSource := MasterDataSource;
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

{**
  Runs a test for master-detail links.
}
procedure TZTestMasterDetailCase.TestMasterFields;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'dep_id';
  DetailQuery.IndexFieldNames := 'p_dep_id';
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

initialization
  TestFramework.RegisterTest(TZTestMasterDetailCase.Suite);
end.
