{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for MSSql Database Connectivity Classes     }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZTestDbcMsSql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MSSQL_SYBASE}

uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZDbcDbLib, ZSqlTestCase, ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcMSSqlCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestConnection;
    procedure TestStoredResultSet;
    procedure TestUseResultSet;
    procedure TestPreparedStatement;
    procedure TestStatement;
    procedure TestDefaultValues;
    procedure TestStoredprocedures;
  end;

{$ENDIF ZEOS_DISABLE_MSSQL_SYBASE}
implementation
{$IFNDEF ZEOS_DISABLE_MSSQL_SYBASE}

uses ZTestConsts;

{ TZTestDbcMSSqlCase classes }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcMSSqlCase.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,ado,odbc_w,odbc_a,OleDB';
end;

{**
  Runs a test for DBC connection.
}
procedure TZTestDbcMSSqlCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  Connection.SetAutoCommit(False);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  { Check without transactions }
  Connection.CreateStatement;
  CheckFalse(Connection.IsClosed);
  CheckFalse(Connection.GetAutoCommit);
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Check with transactions }
  Connection.Open;
  Connection.SetAutoCommit(False);
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
procedure TZTestDbcMsSQLCase.TestPreparedStatement;
const
  dep_id_index = FirstDbcIndex;
  dep_name_index = FirstDbcIndex+1;
  dep_shname_index = FirstDbcIndex+2;
  dep_address_index = FirstDbcIndex+3;
var
  Statement: IZPreparedStatement;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
    + ' VALUES(?,?,?,?)');
  CheckNotNull(Statement);
  try
    Statement.SetInt(dep_id_index, TEST_ROW_ID);
    Statement.SetString(dep_name_index, 'xyz');
    Statement.SetNull(dep_shname_index, stString);
    Stream := TStringStream.Create('abc'#10'def'#13'hgi');
    try
      Statement.SetAsciiStream(dep_address_index, Stream);
    finally
      Stream.Free;
    end;
    CheckEquals(1, Statement.ExecuteUpdatePrepared);

    Statement := Connection.PrepareStatement(
      'DELETE FROM department WHERE dep_id=?');
    CheckNotNull(Statement);

    Statement.SetInt(dep_id_index, TEST_ROW_ID);
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
procedure TZTestDbcMsSQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  try
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
procedure TZTestDbcMsSQLCase.TestStoredResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True);
    ResultSet.Close;
  finally
    Statement.Close;
    Connection.Close;
  end;
end;

{**
  Runs a test for MySQL DBC ResultSet with use results.
}
procedure TZTestDbcMsSQLCase.TestUseResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtForwardOnly);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, False);
    ResultSet.Close;
  finally
    Statement.Close;
    Connection.Close;
  end;
end;

{**
  Runs a test for MySQL default values.
}
procedure TZTestDbcMsSQLCase.TestDefaultValues;
const
  D_ID = FirstDbcIndex;
  D_FLD1 = FirstDbcIndex+1;
  D_FLD2 = FirstDbcIndex+2;
  D_FLD3 = FirstDbcIndex+3;
  D_FLD4 = FirstDbcIndex+4;
  D_FLD5 = FirstDbcIndex+5;
  D_FLD6 = FirstDbcIndex+6;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  try
    Statement.ExecuteUpdate('delete from default_values');

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
  finally
    ResultSet.Close;
    Statement.Close;
  end;
end;

{**
  Runs a test for Interbase stored procedures.
}
procedure TZTestDbcMSSqlCase.TestStoredprocedures;
const
  RETURN_VALUE_Index = FirstDbcIndex;
  P1_Index = FirstDbcIndex+1;
  R1_Index = FirstDbcIndex+2;
  eq_name_Index = FirstDbcIndex;
var
  ResultSet: IZResultSet;
  CallableStatement: IZCallableStatement;
begin
  CallableStatement := Connection.PrepareCallWithParams(
    'procedure1', nil);
  try
    with CallableStatement do
    begin
      RegisterOutParameter(RETURN_VALUE_Index, Ord(stInteger)); //stupid RETURN_VALUE
      SetInt(P1_Index, 12345);
      RegisterOutParameter(R1_Index, Ord(stInteger));
      ExecutePrepared;
      CheckEquals(12346, GetInt(R1_Index));
    end;
  finally
    CallableStatement.Close;
  end;
  CallableStatement := nil;
  CallableStatement := Connection.PrepareCallWithParams(
    'procedure2', nil);
  try
    ResultSet := CallableStatement.ExecuteQueryPrepared;
    with ResultSet do
    begin
      CheckEquals(True, Next);
      CheckEquals('Computer', GetString(eq_name_Index));
      CheckEquals(True, Next);
      CheckEquals('Laboratoy', GetString(eq_name_Index));
      CheckEquals(True, Next);
      CheckEquals('Radiostation', GetString(eq_name_Index));
      CheckEquals(True, Next);
      CheckEquals('Volvo', GetString(eq_name_Index));
      Close;
    end;
  finally
    CallableStatement.Close;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcMSSqlCase.Suite);
{$ENDIF ZEOS_DISABLE_MSSQL_SYBASE}
end.


