{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for MySql Database Connectivity Classes     }
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

unit ZTestDbcMySql;

interface

uses Classes, SysUtils, TestFramework, ZDbcIntfs, ZTestDefinitions, ZDbcMySql,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcMySQLCase = class(TZDbcSpecificSQLTestCase)
  private
    FConnection: IZConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;

    property Connection: IZConnection read FConnection write FConnection;

  published
    procedure TestConnection;
    procedure TestStoredResultSet;
    procedure TestUseResultSet;
    procedure TestPreparedStatement;
    procedure TestStatement;
    procedure TestAutoIncFields;
    procedure TestDefaultValues;
  end;


implementation

uses ZSysUtils, ZTestConsts;

{ TZTestDbcMySqlCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcMySQLCase.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-3.20,mysql-3.23,mysql-4.0,mysql-4.1';
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestDbcMySQLCase.SetUp;
begin
  Connection := CreateDbcConnection;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestDbcMySQLCase.TearDown;
begin
  Connection.Close;
  Connection := nil;
end;

{**
  Runs a test for MySQL database connection.
}
procedure TZTestDbcMySQLCase.TestConnection;
begin
  CheckEquals(True, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Runs a test for MySQL DBC PreparedStatement.
}
procedure TZTestDbcMySQLCase.TestPreparedStatement;
var
  Statement: IZPreparedStatement;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
    + ' VALUES(?,?,?,?)');
  try
    CheckNotNull(Statement);

    Statement.SetInt(1, TEST_ROW_ID);
    Statement.SetString(2, 'xyz');
    Statement.SetNull(3, stString);
    Stream := TStringStream.Create('abc'#10'def'#13'hgi');
    try
      Statement.SetAsciiStream(4, Stream);
    finally
      Stream.Free;
    end;
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
  finally
    Statement.Close;
  end;

  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  try
    CheckNotNull(Statement);

    Statement.SetInt(1, TEST_ROW_ID);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
    Statement.ExecutePrepared;
    CheckEquals(0, Statement.GetUpdateCount);
  finally
    Statement.Close;
  end;
end;

{**
  Runs a test for regular MySQL DBC Statement.
}
procedure TZTestDbcMySQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  try
    CheckNotNull(Statement);

    Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
    Statement.ExecuteUpdate('SELECT * FROM equipment');

    Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
    Check(Statement.Execute('SELECT * FROM equipment'));
  finally
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL DBC ResultSet with stored results.
}
procedure TZTestDbcMySQLCase.TestStoredResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for MySQL DBC ResultSet with use results.
}
procedure TZTestDbcMySQLCase.TestUseResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtForwardOnly);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, False);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for MySQL AutoIncremented fields.
}
procedure TZTestDbcMySQLCase.TestAutoIncFields;
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
  ResultSet.UpdateString(2, 'xxx');
  CheckEquals(0, ResultSet.GetInt(1));
  CheckEquals('xxx', ResultSet.GetString(2));

  ResultSet.InsertRow;
  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals('xxx', ResultSet.GetString(2));

  ResultSet.DeleteRow;

  ResultSet.Close;

  Statement.Close;
end;

{**
  Runs a test for MySQL default values.
}
procedure TZTestDbcMySQLCase.TestDefaultValues;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from default_values');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(1) <> 0);
  CheckEquals(123456, ResultSet.GetInt(2));
  CheckEquals(123.456, ResultSet.GetFloat(3), 0.001);
  CheckEquals('xyz', ResultSet.GetString(4));
  CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(5), 0);
  CheckEquals(EncodeTime(23, 12, 11, 0), ResultSet.GetTime(6), 3);
  CheckEquals(EncodeDate(2003, 12, 11) +
    EncodeTime(23, 12, 11, 0), ResultSet.GetTimestamp(7), 3);

  ResultSet.DeleteRow;

  ResultSet.Close;
  Statement.Close;
end;

initialization
  TestFramework.RegisterTest(TZTestDbcMySQLCase.Suite);
end.
