{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Cached Resolver Classes          }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestDbcResolver;

interface
{$I ZDbc.inc}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Classes, SysUtils,
  ZDbcIntfs, ZCompatibility, ZSqlTestCase;

type

 {** Implements a test case for CachedResolver classes. }
  TZTestCachedResolverCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
  published
    procedure TestGenericResolver;
    {$IFDEF ENABLE_INTERBASE}procedure TestInterbaseResolver;{$ENDIF}
    {$IFDEF ENABLE_MYSQL}procedure TestMySqlResolverPosts;{$ENDIF}
  end;

implementation

uses ZSysUtils, ZTestConsts
     {$IFDEF ENABLE_POSTGRESQL}, ZDbcPostgreSql{$ENDIF}
     {$IFDEF ENABLE_MYSQL}, ZDbcMySql{$ENDIF};

{ TZTestCachedResolverCase }

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
  if ProtocolType in [protInterbase, protFirebird] then
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

{$IFDEF ENABLE_INTERBASE}
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
  if ProtocolType <> protInterbase then
    Exit;
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
{$ENDIF}

{$IFDEF ENABLE_MYSQL}
{**
  Runs a test for MySQL Resolver.
}
procedure TZTestCachedResolverCase.TestMySqlResolverPosts;
const
  department_dep_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  department_dep_name_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  department_dep_shname_index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  department_dep_address_index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  WhereClause: string;
begin
  if ProtocolType <> protMySQL then
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
  ResultSet.UpdateInt(department_dep_id_index, TEST_ROW_ID);
  ResultSet.UpdateString(department_dep_name_index, 'AAA');
  ResultSet.UpdateString(department_dep_shname_index, 'BBB');
  ResultSet.UpdateString(department_dep_address_index, 'XXX');
  ResultSet.InsertRow;

  { Updates the row. }
  ResultSet.UpdateString(department_dep_address_index, 'CCC');
  ResultSet.UpdateRow;

  { Reads the row and removes it. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(True, ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetInt(department_dep_id_index));
  CheckEquals('AAA', ResultSet.GetString(department_dep_name_index));
  CheckEquals('BBB', ResultSet.GetString(department_dep_shname_index));
  CheckEquals('CCC', ResultSet.GetString(department_dep_address_index));
  ResultSet.DeleteRow;

  { Checks the removed row. }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department ' + WhereClause);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;
end;
{$ENDIF}

initialization
  RegisterTest('dbc',TZTestCachedResolverCase.Suite);
end.
