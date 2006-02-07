{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 Test Case for Sorting                   }
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

unit ZTestSorting;

interface

uses
  TestFramework, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZDbcMySql, ZDbcPostgreSql, ZDbcDbLib,
  ZCompatibility;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestSortingCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckDatasetColumns(Dataset: TDataset; FieldIndex: Integer;
      Values: array of string);
  published
    procedure TestResultSetSort;
    procedure TestCachedResultSetSort;
  end;

implementation

uses Classes, ZDbcUtils, ZTestConsts, ZDbcIntfs;

{ TZTestSortingCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSortingCase.SetUp;
begin
  Connection := CreateDatasetConnection;
end;

{**
  Removes data after each test.
}
procedure TZTestSortingCase.TearDown;
begin
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Checks dataset field values.
  @param Dataset a dataset object.
  @param FieldNo a field index to check.
  @param Values a field expected values.
}
procedure TZTestSortingCase.CheckDatasetColumns(Dataset: TDataset;
  FieldIndex: Integer; Values: array of string);
var
  I: Integer;
begin
  Dataset.First;
  for I := Low(Values) to High(Values) do
  begin
    Check(not Dataset.Eof);
    CheckEquals(Values[I], Dataset.Fields[FieldIndex].AsString);
    Dataset.Next;
  end;
end;

{**
  Runs a test for sorting on the resultset level.
}
procedure TZTestSortingCase.TestResultSetSort;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from people where p_id < 6 order by p_id';
    Query.SortedFields := 'p_id Desc';
    Query.Open;

    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','3','2','1']);

    Query.RecNo := 1;
    Query.SortedFields := '';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['1','2','3','4','5']);

    Query.RecNo := 1;
    Query.SortedFields := 'p_dep_id DESC, p_name';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','2','3','1']);
    CheckDatasetColumns(Query, Query.FieldByName('p_dep_id').Index,
      ['3','2','2','1','1']);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for sorting on the cached resultset level.
}
procedure TZTestSortingCase.TestCachedResultSetSort;
var
  Query: TZQuery;
begin
  Query := TZQuery.Create(nil);
  try
    Query.Connection := Connection;
    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from people where p_id < 6 order by p_id';
    Query.SortedFields := 'p_id Desc';
    Query.Open;

    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','3','2','1']);

    Query.RecNo := 1;
    Query.SortedFields := '';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['1','2','3','4','5']);

    Query.RecNo := 1;
    Query.SortedFields := 'p_dep_id DESC, p_name';
    CheckEquals(5, Query.RecNo);
    CheckDatasetColumns(Query, Query.FieldByName('p_id').Index,
      ['5','4','2','3','1']);
    CheckDatasetColumns(Query, Query.FieldByName('p_dep_id').Index,
      ['3','2','2','1','1']);

    Query.Close;
  finally
    Query.Free;
  end;
end;

initialization
  TestFramework.RegisterTest(TZTestSortingCase.Suite);
end.
