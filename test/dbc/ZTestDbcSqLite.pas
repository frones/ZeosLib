{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for SQLite Database Connectivity Classes    }
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

unit ZTestDbcSqLite;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE}
uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZSqlTestCase, ZDbcSQLite,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcSQLiteCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestResultSet;
    procedure TestPreparedStatement;
    procedure TestAutoIncFields;
    procedure TestDefaultValues;
    procedure TestEmptyTypes;
    procedure TestReuseResultsetNative;
    procedure TestReuseResultsetCached;
    procedure TestUpdateOpenedTable;
  end;


{$IFNDEF ZEOS_DISABLE_SQLITE}
implementation
{$ENDIF ZEOS_DISABLE_SQLITE}

uses ZTestConsts;

{ TZTestDbcSQLiteCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcSQLiteCase.GetSupportedProtocols: string;
begin
  Result := pl_all_sqlite;
end;

{**
  Runs a test for SQLite database connection.
}
procedure TZTestDbcSQLiteCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiSerializable), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
//  Connection.Commit;
//  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.CreateStatement;
  Connection.SetAutoCommit(False);
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Runs a test for regular SQLite DBC Statement.
}
procedure TZTestDbcSQLiteCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment WHERE 1=0'));
  Statement.Close;
end;

{**
  Runs a test for SQLite DBC ResultSet with stored results.
}
procedure TZTestDbcSQLiteCase.TestResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZDatabaseMetadata;
  TableTypes: TStringDynArray;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Metadata := Connection.GetMetadata;
  ResultSet := Metadata.GetBestRowIdentifier('', '', 'people', 0, False);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  SetLength(TableTypes, 2);
  TableTypes[0] := 'TABLE';
  TableTypes[1] := 'VIEW';
  ResultSet := Metadata.GetTables('', '', '', TableTypes);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  with Metadata.GetTables('', '', '', TableTypes) do
  try
    while Next do
      if GetString(2) = '' then
        PrintLn('Table=' + GetString(3))
      else
        PrintLn('Table=' + GetString(2) + '.' + GetString(3));
  finally
    Close;
  end;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for SQLite DBC PreparedStatement.
}
procedure TZTestDbcSQLiteCase.TestPreparedStatement;
const
  department_dep_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  department_dep_name_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  department_dep_shname_index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  department_dep_address_index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZPreparedStatement;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
    + ' VALUES(?,?,?,?)');
  CheckNotNull(Statement);

  Statement.SetInt(department_dep_id_index, TEST_ROW_ID);
  Statement.SetString(department_dep_name_index, 'xyz');
  Statement.SetNull(department_dep_shname_index, stString);
  Stream := TStringStream.Create('abc'#10'def'#13'hg''i');
  try
    Statement.SetAsciiStream(department_dep_address_index, Stream);
  finally
    Stream.Free;
  end;
  CheckEquals(False, Statement.ExecutePrepared);
  CheckEquals(1, Statement.GetUpdateCount);

  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  CheckNotNull(Statement);

  Statement.SetInt(department_dep_id_index, TEST_ROW_ID);
  CheckEquals(1, Statement.ExecuteUpdatePrepared);
  Statement.ExecutePrepared;
  CheckEquals(0, Statement.GetUpdateCount);
end;

{**
  Runs a test for SQLite AutoIncremented fields.
}
procedure TZTestDbcSQLiteCase.TestAutoIncFields;
const
  cargo_c_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  cargo_c_name_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT c_id, c_name FROM cargo');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateString(cargo_c_name_index, 'xxx');
  CheckEquals(0, ResultSet.GetInt(cargo_c_id_index));
  CheckEquals('xxx', ResultSet.GetString(cargo_c_name_index));

  ResultSet.InsertRow;
  Check(ResultSet.GetInt(cargo_c_id_index) <> 0);
  CheckEquals('xxx', ResultSet.GetString(cargo_c_name_index));

  ResultSet.DeleteRow;

  ResultSet.Close;

  Statement.Close;
end;

{**
  Runs a test for SQLite default values.
}
procedure TZTestDbcSQLiteCase.TestDefaultValues;
const
  D_ID = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  D_FLD1 = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  D_FLD2 = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  D_FLD3 = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  D_FLD4 = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
  D_FLD5 = {$IFDEF GENERIC_INDEX}5{$ELSE}6{$ENDIF};
  D_FLD6 = {$IFDEF GENERIC_INDEX}6{$ELSE}7{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.Execute('delete from default_values');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(D_FLD4), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(D_FLD5), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(D_FLD6), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for SQLite empty types.
}
procedure TZTestDbcSQLiteCase.TestEmptyTypes;
const
  et_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  data1_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  data2_index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
var
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.Execute('delete from empty_types');

  PreparedStatement := Connection.PrepareStatement(
    'INSERT INTO empty_types(et_id, data1, data2) VALUES(?,?,?)');
  CheckNotNull(PreparedStatement);

  PreparedStatement.SetInt(et_id_index, 0);
  PreparedStatement.SetInt(data1_index, 1);
  PreparedStatement.SetString(data2_index, 'xyz');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.SetString(et_id_index, 'qwe');
  PreparedStatement.SetString(data1_index, 'asd');
  PreparedStatement.SetString(data2_index, 'xyzz');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.SetFloat(et_id_index, 1.25);
  PreparedStatement.SetFloat(data1_index, 2.25);
  PreparedStatement.SetString(data2_index, 'xyzzz');
  PreparedStatement.ExecuteUpdatePrepared;

  ResultSet := Statement.ExecuteQuery('SELECT et_id,data1,data2 FROM empty_types where data2=''xyz''');
  CheckNotNull(ResultSet);

  ResultSet.Next;
  CheckEquals(0, ResultSet.GetInt(et_id_index));
  CheckEquals(1, ResultSet.GetInt(data1_index));

  ResultSet := Statement.ExecuteQuery('SELECT et_id,data1,data2 FROM empty_types where data2="xyzz"');
  CheckNotNull(ResultSet);

  ResultSet.Next;
  CheckEquals('qwe', ResultSet.GetString(et_id_index));
  CheckEquals('asd', ResultSet.GetString(data1_index));

  ResultSet := Statement.ExecuteQuery('SELECT et_id,data1,data2 FROM empty_types where data2=''xyzzz''');
  CheckNotNull(ResultSet);

  ResultSet.Next;
  CheckEquals(1.25, ResultSet.GetFloat(et_id_index));
  CheckEquals(2.25, ResultSet.GetFloat(data1_index));

  ResultSet.Close;
  Statement.Close;
end;

procedure TZTestDbcSQLiteCase.TestReuseResultsetNative;
const
  p_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  PreparedStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  PreparedStatement := Connection.PrepareStatement(
    'SELECT * FROM PEOPLE WHERE p_id > ?');
  PreparedStatement.SetResultSetType(rtScrollInsensitive);
  try
    PreparedStatement.SetInt(p_id_index, 0); //expecting 5 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(1, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    CheckEquals(True, ResultSet.Next); //fetch third row.
    CheckEquals(True, ResultSet.Next); //fetch fourth row.
    {ignore last row}
    PreparedStatement.SetInt(p_id_index, 1); //expecting 4 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(2, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    CheckEquals(True, ResultSet.Next); //fetch third row.
    {ignore last row}

    PreparedStatement.SetInt(p_id_index, 2); //expecting 3 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(3, ResultSet.GetInt(p_id_index));
    while ResultSet.Next do; //full fetch automatically resets handle

    PreparedStatement.SetInt(p_id_index, 3); //expecting 2 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(4, ResultSet.GetInt(p_id_index));
    while ResultSet.Next do; //full fetch automatically resets handle

    PreparedStatement.SetInt(p_id_index, 10); //expecting !0! rows  e.g AB -> no metadata???
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckNotNull(ResultSet);
    CheckEquals(False, ResultSet.Next); //fetch first row.
  finally
    if Assigned(ResultSet) then
      ResultSet.Close;
    PreparedStatement.Close;
  end;
end;

procedure TZTestDbcSQLiteCase.TestReuseResultsetCached;
const
  p_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  PreparedStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
  RSPointer: Pointer;
begin
  PreparedStatement := Connection.PrepareStatement(
    'SELECT * FROM PEOPLE WHERE p_id > ?');
  PreparedStatement.SetResultSetConcurrency(rcUpdatable);
  PreparedStatement.SetResultSetType(rtScrollSensitive);
  try
    PreparedStatement.SetInt(p_id_index, 0); //expecting 5 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    RSPointer := Pointer(ResultSet);
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(1, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    CheckEquals(True, ResultSet.Next); //fetch third row.
    CheckEquals(True, ResultSet.Next); //fetch fourth row.
    {ignore last row}
    PreparedStatement.SetInt(p_id_index, 1); //expecting 4 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(NativeUInt(RSPointer), NativeUInt(Pointer(ResultSet)), 'First ResultSet is opened again');
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(2, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    CheckEquals(True, ResultSet.Next); //fetch third row.
    {ignore last row}

    PreparedStatement.SetInt(p_id_index, 2); //expecting 3 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(NativeUInt(RSPointer), NativeUInt(Pointer(ResultSet)), 'First ResultSet is opened again');
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(3, ResultSet.GetInt(p_id_index));
    while ResultSet.Next do; //full fetch automatically resets handle

    PreparedStatement.SetInt(p_id_index, 3); //expecting 2 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(NativeUInt(RSPointer), NativeUInt(Pointer(ResultSet)), 'First ResultSet is opened again');
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(4, ResultSet.GetInt(p_id_index));
    while ResultSet.Next do; //full fetch automatically resets handle
  finally
    if Assigned(ResultSet) then
      ResultSet.Close;
    PreparedStatement.Close;
  end;
end;

{test if table, we are working on, is blocked}
procedure TZTestDbcSQLiteCase.TestUpdateOpenedTable;
const
  p_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  PreparedStatement, InsertPreparedStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
  RSPointer: Pointer;
begin
  PreparedStatement := Connection.PrepareStatement(
    'SELECT * FROM PEOPLE WHERE p_id > ?');
  InsertPreparedStatement := Connection.PrepareStatement(
    'insert into PEOPLE(p_id) values (?)');
  try
    PreparedStatement.SetInt(p_id_index, 0); //expecting 5 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    RSPointer := Pointer(ResultSet);
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(1, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    //CheckEquals(True, ResultSet.Next); //fetch third row.
    //CheckEquals(True, ResultSet.Next); //fetch fourth row.
    InsertPreparedStatement.SetInt(p_id_index, 10);
    CheckEquals(1, InsertPreparedStatement.ExecuteUpdatePrepared, 'updatecount');

    {ignore last row}
    PreparedStatement.SetInt(p_id_index, 1); //expecting 4 rows
    ResultSet := PreparedStatement.ExecuteQueryPrepared;
    CheckEquals(NativeUInt(RSPointer), NativeUInt(Pointer(ResultSet)), 'First ResultSet is opened again');
    CheckEquals(True, ResultSet.Next); //fetch first row.
    CheckEquals(2, ResultSet.GetInt(p_id_index));
    CheckEquals(True, ResultSet.Next); //fetch second row.
    CheckEquals(True, ResultSet.Next); //fetch third row.
    {ignore last row}
    //connection.Commit;
  finally
    InsertPreparedStatement.ClearParameters;
    InsertPreparedStatement.ExecuteUpdate('delete from people where p_id=10');
    InsertPreparedStatement.Close;
    if Assigned(ResultSet) then
      ResultSet.Close;
    PreparedStatement.Close;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcSQLiteCase.Suite);
{$ENDIF ZEOS_DISABLE_SQLITE}
end.
