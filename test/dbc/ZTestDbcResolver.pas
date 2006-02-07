{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Cached Resolver Classes          }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
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

unit ZTestDbcResolver;

interface

uses TestFramework, Classes, SysUtils, ZDbcIntfs, ZClasses, ZCompatibility,
  ZCollections, ZDbcPostgreSql, ZDbcMySql, ZDbcMySqlStatement,
  ZDbcMySqlResultSet, ZDbcGenericResolver, ZTestDefinitions;

type

 {** Implements a test case for CachedResolver classes. }
  TZTestCachedResolverCase = class(TZDbcPortableSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    property Connection: IZConnection read FConnection write FConnection;
  published
    procedure TestGenericResolver;
    procedure TestInterbaseResolver;
    procedure TestMySqlResolverPosts;
  end;

implementation

uses ZSysUtils, ZTestConsts;

{ TZTestCachedResolverCase }

{**
   Create objects and allocate memory for variables
}
procedure TZTestCachedResolverCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestCachedResolverCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Runs a test for GenericCachedResolver class.
}
procedure TZTestCachedResolverCase.TestGenericResolver;
(*
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Resolver: TZGenericCachedResolver;
  Columns: TObjectList;
*)
begin
  if StartsWith(Protocol, 'interbase')
    or StartsWith(Protocol, 'firebird') then
    Exit;
(*
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);
  Columns := TObjectList.Create;

  try
    CheckEquals('INSERT INTO department (dep_id,dep_name,dep_shname,dep_address)'
      + ' VALUES (?,?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(4, Resolver.GetInsertParams.Count);
    CheckEquals('UPDATE department SET dep_id=?,dep_name=?,dep_shname=?,'
      + 'dep_address=? WHERE dep_id=?', Resolver.GetUpdateSQL);
    CheckEquals(5, Resolver.GetUpdateParams.Count);
    CheckEquals('DELETE FROM department WHERE dep_id=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);

    ResultSet := Statement.ExecuteQuery('SELECT t.dep_id AS id, dep_name as name, '
      + ' t.dep_shname, 2+2 as dep_address FROM department as t where dep_id < 100');
    Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

    CheckEquals('INSERT INTO department (dep_id,dep_name,dep_shname)'
      + ' VALUES (?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(3, Resolver.GetInsertParams.Count);
    CheckEquals('UPDATE department SET dep_id=?,dep_name=?,dep_shname=?'
      + ' WHERE dep_id=?', Resolver.GetUpdateSQL);
    CheckEquals(4, Resolver.GetUpdateParams.Count);
    CheckEquals('DELETE FROM department WHERE dep_id=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);
  finally
    Resolver.Free;
    Columns.Free;
  end;
*)
end;

{**
  Runs a test for posts of Interbase CachedResolver class.
}
procedure TZTestCachedResolverCase.TestInterbaseResolver;
(*
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Resolver: TZGenericCachedResolver;
*)
begin
  if not StartsWith(Protocol, 'interbase') then Exit;
(*
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

  try
    CheckEquals('INSERT INTO DEPARTMENT (DEP_ID,DEP_NAME,DEP_SHNAME,DEP_ADDRESS)'
      + ' VALUES (?,?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(4, Resolver.GetInsertParams.Count);

    CheckEquals('UPDATE DEPARTMENT SET DEP_ID=?,DEP_NAME=?,DEP_SHNAME=?,'
      + 'DEP_ADDRESS=? WHERE DEP_ID=?', Resolver.GetUpdateSQL);
    CheckEquals(5, Resolver.GetUpdateParams.Count);

    CheckEquals('DELETE FROM DEPARTMENT WHERE DEP_ID=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);

    ResultSet := Statement.ExecuteQuery('SELECT t.dep_id AS id, dep_name as name, '
      + ' t.dep_shname, 2+2 as dep_address FROM DEPARTMENT t where DEP_ID < 100');
    Resolver := TZGenericCachedResolver.Create(Statement, ResultSet.GetMetadata);

    CheckEquals('INSERT INTO DEPARTMENT (DEP_ID,DEP_NAME,DEP_SHNAME)'
      + ' VALUES (?,?,?)', Resolver.GetInsertSQL);
    CheckEquals(3, Resolver.GetInsertParams.Count);

    CheckEquals('UPDATE DEPARTMENT SET DEP_ID=?,DEP_NAME=?,DEP_SHNAME=?'
      + ' WHERE DEP_ID=?', Resolver.GetUpdateSQL);
    CheckEquals(4, Resolver.GetUpdateParams.Count);

    CheckEquals('DELETE FROM DEPARTMENT WHERE DEP_ID=?', Resolver.GetDeleteSQL);
    CheckEquals(1, Resolver.GetDeleteParams.Count);
  finally
    Resolver.Free;
  end;
*)
end;

{**
  Runs a test for MySQL Resolver.
}
procedure TZTestCachedResolverCase.TestMySqlResolverPosts;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  WhereClause: string;
begin
  if not StartsWith(Protocol, 'mysql') then
    Exit;

  Statement := Connection.CreateStatement;
  WhereClause := 'WHERE dep_id=' + IntToStr(TEST_ROW_ID);

  { Deletes all existed rows. }
  Statement.ExecuteUpdate('DELETE FROM department ' + WhereClause);

  { Inserts a new row. }
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(False, ResultSet.Next);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(1, TEST_ROW_ID);
  ResultSet.UpdateString(2, 'AAA');
  ResultSet.UpdateString(3, 'BBB');
  ResultSet.UpdateString(4, 'XXX');
  ResultSet.InsertRow;

  { Updates the row. }
  ResultSet.UpdateString(4, 'CCC');
  ResultSet.UpdateRow;

  { Reads the row and removes it. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(True, ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetInt(1));
  CheckEquals('AAA', ResultSet.GetString(2));
  CheckEquals('BBB', ResultSet.GetString(3));
  CheckEquals('CCC', ResultSet.GetString(4));
  ResultSet.DeleteRow;

  { Checks the removed row. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(False, ResultSet.Next);
end;

initialization
  TestFramework.RegisterTest(TZTestCachedResolverCase.Suite);
end.
