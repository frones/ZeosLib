{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for MySql Database Connectivity Classes     }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZTestDbcMySql;

interface
{$I ZDbc.inc}

uses Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZSqlTestCase, ZDbcMySql, ZDbcProperties, ZCompatibility, ZSysUtils;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcMySQLCase = class(TZAbstractDbcSQLTestCase)
  private
    procedure CheckBitFields(ResultSet: IZResultSet);
    procedure InternalTestSelectTwoQueriesGetMoreResults(const Statement: IZStatement);
    procedure InternalTestSelectTwoQueriesNoGetResults(const Statement: IZStatement);
    procedure InternalTestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS(const Statement: IZStatement);
    procedure InternalTestSelectThreeQueriesGetMoreResults(const Statement: IZStatement);
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestConnection;
    procedure TestStoredResultSet;
    procedure TestStoredResultSetPrepared;
    procedure TestStoredResultSetRealPrepared;
    procedure TestUseResultSetForwardOnly;
    procedure TestUseResultSetScrollable;
    procedure TestUseResultSetUpdateable;
    procedure TestPreparedStatement;
    procedure TestStatement;
    procedure TestAutoIncFields;
    procedure TestDefaultValues;
    procedure TestSelectTwoQueriesGetMoreResults;
    procedure TestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS;
    procedure TestSelectTwoQueriesNoGetResults;
    procedure TestSelectThreeQueriesGetMoreResults;
    procedure TestBitFields;
  end;


implementation

uses ZTestConsts;

{ TZTestDbcMySqlCase }
procedure TZTestDbcMySQLCase.CheckBitFields(ResultSet: IZResultSet);
const
 {Bit_id_Index = FirstDbcIndex;}
  Bit1_Index = FirstDbcIndex+1;
  Bit2_Index = FirstDbcIndex+2;
  Bit3_Index = FirstDbcIndex+3;
  Bit4_Index = FirstDbcIndex+4;
  Bit5_Index = FirstDbcIndex+5;
  Bit6_Index = FirstDbcIndex+6;
  Bit7_Index = FirstDbcIndex+7;
  Bit8_Index = FirstDbcIndex+8;
  Bit9_Index = FirstDbcIndex+9;
  Bit10_Index = FirstDbcIndex+10;
  Bit11_Index = FirstDbcIndex+11;
  Bit12_Index = FirstDbcIndex+12;
  Bit13_Index = FirstDbcIndex+13;
  Bit14_Index = FirstDbcIndex+14;
  Bit15_Index = FirstDbcIndex+15;
  Bit16_Index = FirstDbcIndex+16;
  Bit17_Index = FirstDbcIndex+17;
  Bit18_Index = FirstDbcIndex+18;
  Bit19_Index = FirstDbcIndex+19;
  Bit20_Index = FirstDbcIndex+20;
  Bit21_Index = FirstDbcIndex+21;
  Bit22_Index = FirstDbcIndex+22;
  Bit23_Index = FirstDbcIndex+23;
  Bit24_Index = FirstDbcIndex+24;
  Bit25_Index = FirstDbcIndex+25;
 {Bit26_Index = FirstDbcIndex+26;
  Bit27_Index = FirstDbcIndex+27;
  Bit28_Index = FirstDbcIndex+28;
  Bit29_Index = FirstDbcIndex+29;
  Bit30_Index = FirstDbcIndex+30;
  Bit31_Index = FirstDbcIndex+31;
  Bit32_Index = FirstDbcIndex+32;
  Bit33_Index = FirstDbcIndex+33;
  Bit34_Index = FirstDbcIndex+34;
  Bit35_Index = FirstDbcIndex+35;
  Bit36_Index = FirstDbcIndex+36;
  Bit37_Index = FirstDbcIndex+37;
  Bit38_Index = FirstDbcIndex+38;
  Bit39_Index = FirstDbcIndex+39;}
  Bit40_Index = FirstDbcIndex+40;
 {Bit41_Index = FirstDbcIndex+41;
  Bit42_Index = FirstDbcIndex+42;
  Bit43_Index = FirstDbcIndex+43;
  Bit44_Index = FirstDbcIndex+44;
  Bit45_Index = FirstDbcIndex+45;
  Bit46_Index = FirstDbcIndex+46;
  Bit47_Index = FirstDbcIndex+47;
  Bit48_Index = FirstDbcIndex+48;
  Bit49_Index = FirstDbcIndex+49;
  Bit50_Index = FirstDbcIndex+50;
  Bit51_Index = FirstDbcIndex+51;
  Bit52_Index = FirstDbcIndex+52;
  Bit53_Index = FirstDbcIndex+53;
  Bit54_Index = FirstDbcIndex+54;
  Bit55_Index = FirstDbcIndex+55;
  Bit56_Index = FirstDbcIndex+56;
  Bit57_Index = FirstDbcIndex+57;
  Bit58_Index = FirstDbcIndex+58;
  Bit59_Index = FirstDbcIndex+59;
  Bit60_Index = FirstDbcIndex+60;
  Bit61_Index = FirstDbcIndex+61;
  Bit62_Index = FirstDbcIndex+62;
  Bit63_Index = FirstDbcIndex+63;}
  Bit64_Index = FirstDbcIndex+64;
begin
  CheckNotNull(ResultSet);
  try
    CheckEquals(65, ResultSet.GetMetadata.GetColumnCount, 'ColumnCount of TEST_BIT_FIELDS');
    Check(ResultSet.Next, 'There is one row available');
    CheckEquals(1, ResultSet.GetByte(Bit1_Index), 'Bit1_Index');
    CheckEquals(3, ResultSet.GetByte(Bit2_Index), 'Bit2_Index');
    CheckEquals(7, ResultSet.GetByte(Bit3_Index), 'TEST_BIT_FIELDS');
    CheckEquals(15, ResultSet.GetByte(Bit4_Index), 'TEST_BIT_FIELDS');
    CheckEquals(31, ResultSet.GetByte(Bit5_Index), 'TEST_BIT_FIELDS');
    CheckEquals(63, ResultSet.GetByte(Bit6_Index), 'TEST_BIT_FIELDS');
    CheckEquals(127, ResultSet.GetByte(Bit7_Index), 'TEST_BIT_FIELDS');
    CheckEquals(255, ResultSet.GetByte(Bit8_Index), 'TEST_BIT_FIELDS');
    CheckEquals(511, ResultSet.GetWord(Bit9_Index), 'TEST_BIT_FIELDS');
    CheckEquals(1023, ResultSet.GetWord(Bit10_Index), 'TEST_BIT_FIELDS');
    CheckEquals(2047, ResultSet.GetWord(Bit11_Index), 'TEST_BIT_FIELDS');
    CheckEquals(4095, ResultSet.GetWord(Bit12_Index), 'TEST_BIT_FIELDS');
    CheckEquals(8191, ResultSet.GetWord(Bit13_Index), 'TEST_BIT_FIELDS');
    CheckEquals(16383, ResultSet.GetWord(Bit14_Index), 'TEST_BIT_FIELDS');
    CheckEquals(32767, ResultSet.GetWord(Bit15_Index), 'TEST_BIT_FIELDS');
    CheckEquals(65535, ResultSet.GetWord(Bit16_Index), 'TEST_BIT_FIELDS');
    CheckEquals(131071, ResultSet.GetUInt(Bit17_Index), 'TEST_BIT_FIELDS');
    CheckEquals(262143, ResultSet.GetUInt(Bit18_Index), 'TEST_BIT_FIELDS');
    CheckEquals(524287, ResultSet.GetUInt(Bit19_Index), 'TEST_BIT_FIELDS');
    CheckEquals(1048575, ResultSet.GetUInt(Bit20_Index), 'TEST_BIT_FIELDS');
    CheckEquals(2097151, ResultSet.GetUInt(Bit21_Index), 'Bit21_Index');
    CheckEquals(4194303, ResultSet.GetUInt(Bit22_Index), 'Bit22_Index');
    CheckEquals(8388607, ResultSet.GetUInt(Bit23_Index), 'Bit23_Index');
    CheckEquals(16777215, ResultSet.GetUInt(Bit24_Index), 'Bit24_Index');
    CheckEquals(33554431, ResultSet.GetUInt(Bit25_Index), 'Bit25_Index');
    // FPC can't determine here which overloaded method to call.
    // And if we add direct typecast, D7 fails to build with "Ambiguous overload" error
    CheckEquals({$IFDEF FPC}UInt64{$ENDIF}($FFFFFFFFFF), ResultSet.GetULong(Bit40_Index), 'TEST_BIT_FIELDS');
    CheckEquals({$IFDEF FPC}UInt64{$ENDIF}($FFFFFFFFFFFFFFFF), ResultSet.GetULong(Bit64_Index), 'TEST_BIT_FIELDS');
  finally
    ResultSet.Close;
  end;
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcMySQLCase.GetSupportedProtocols: string;
begin
  Result := pl_all_mysql;
end;

procedure TZTestDbcMySQLCase.InternalTestSelectThreeQueriesGetMoreResults(
  const Statement: IZStatement);
var
  ResultSet: IZResultSet;
begin
  CheckNotNull(Statement);
  try
    ResultSet := Statement.ExecuteQuery('call ThreeResultSets()');
    CheckNotNull(ResultSet);
    CheckEquals(8, ResultSet.GetMetadata.GetColumnCount, 'ColumnCount of people table');
    Check(ResultSet.Next, 'There should be a row retrieved');
    Check(Statement.GetMoreResults, 'There is a second resultset available!');
    CheckEquals(7, Statement.GetResultSet.GetMetadata.GetColumnCount, 'ColumnCount of string_values table');
    Check(Statement.GetResultSet.Next, 'There should be a row retrieved');
    Check(not ResultSet.Next, 'First ResultSet should be closed');
    ResultSet := Statement.GetResultSet;
    Check(Statement.GetMoreResults, 'There is a third resultset available!');
    Check(Statement.GetResultSet.Next, 'There should be a row retrieved');
    Check(not ResultSet.Next, 'Second ResultSet should be closed');
    Statement.GetResultSet.Close;
  finally
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.InternalTestSelectTwoQueriesGetMoreResults(
  const Statement: IZStatement);
var
  ResultSet: IZResultSet;
begin
  CheckNotNull(Statement);
  try
    ResultSet := Statement.ExecuteQuery('call TwoResultSets()');
    CheckNotNull(ResultSet);
    CheckEquals(8, ResultSet.GetMetadata.GetColumnCount, 'ColumnCount of people table');
    Check(ResultSet.Next, 'There should be a row retrieved');
    Check(Statement.GetMoreResults, 'There is a second resultset available!');
    CheckEquals(7, Statement.GetResultSet.GetMetadata.GetColumnCount, 'ColumnCount of string_values table');
    Check(Statement.GetResultSet.Next, 'There should be a row retrieved');
    Check(not Statement.GetMoreResults, 'There no more resultset available!');
    Check(not ResultSet.Next, 'Previous ResultSet should be closed');
    ResultSet.Close;
  finally
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.InternalTestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS(
  const Statement: IZStatement);
var
  ResultSet: IZResultSet;
begin
  CheckNotNull(Statement);
  try
    ResultSet := Statement.ExecuteQuery('call TwoResultSets()');
    CheckNotNull(ResultSet);
    CheckEquals(8, ResultSet.GetMetadata.GetColumnCount, 'ColumnCount of people table');
    Check(ResultSet.Next, 'There should be a row retrieved');
    ResultSet.Close;
    Check(Statement.GetMoreResults, 'There is a second resultset available!');
    CheckEquals(7, Statement.GetResultSet.GetMetadata.GetColumnCount, 'ColumnCount of string_values table');
    Check(Statement.GetResultSet.Next, 'There should be a row retrieved');
    ResultSet := Statement.GetResultSet;
    ResultSet.Close;
  finally
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.InternalTestSelectTwoQueriesNoGetResults(
  const Statement: IZStatement);
var
  ResultSet: IZResultSet;
begin
  CheckNotNull(Statement);
  try
    ResultSet := Statement.ExecuteQuery('call TwoResultSets()');
    CheckNotNull(ResultSet);
    CheckEquals(8, ResultSet.GetMetadata.GetColumnCount, 'ColumnCount of people table');
    ResultSet.Close;
    ResultSet := Statement.ExecuteQuery('call  TwoResultSets()');
    ResultSet.Close;
  finally
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL database connection.
}
procedure TZTestDbcMySQLCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiRepeatableRead), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  //Connection.Commit;
  //Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
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
procedure TZTestDbcMySQLCase.TestPreparedStatement;
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
  try
    CheckNotNull(Statement);

    Statement.SetInt(department_dep_id_index, TEST_ROW_ID);
    Statement.SetString(department_dep_name_index, 'xyz');
    Statement.SetNull(department_dep_shname_index, stString);
    Stream := TStringStream.Create('abc'#10'def'#13'hgi');
    try
      Statement.SetAsciiStream(department_dep_address_index, Stream);
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

    Statement.SetInt(department_dep_id_index, TEST_ROW_ID);
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
  ResultSet, ResultSet2: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    ResultSet2 := Statement.ExecuteQuery('SELECT * FROM department');
    Check(ResultSet = ResultSet2, 'same rs retrieved');
  finally
    if Assigned(ResultSet) then
      ResultSet.Close;
    if Assigned(ResultSet2) then
      ResultSet2.Close;
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.TestStoredResultSetPrepared;
var
  Statement: IZPreparedStatement;
  ResultSet, ResultSet2: IZResultSet;
begin
  Statement := Connection.PrepareStatement('SELECT * FROM department');
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQueryPrepared;
    CheckNotNull(ResultSet);
    ResultSet2 := Statement.ExecuteQueryPrepared;
    Check(ResultSet = ResultSet2, 'same rs retrieved');
    Check(ResultSet2.Next, 'There is a row');
    Check(ResultSet.Next, 'There is a row');
  finally
    if Assigned(ResultSet) then
      ResultSet.Close;
    if Assigned(ResultSet2) then
      ResultSet2.Close;
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.TestStoredResultSetRealPrepared;
var
  Statement: IZPreparedStatement;
  ResultSet, ResultSet2: IZResultSet;
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  Statement := Connection.PrepareStatementWithParams('SELECT * FROM department', Info);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQueryPrepared;
    CheckNotNull(ResultSet);
    ResultSet2 := Statement.ExecuteQueryPrepared;
    Check(ResultSet = ResultSet2, 'same rs retrieved');
    Check(ResultSet2.Next, 'There is a row');
    Check(ResultSet.Next, 'There is a row');
  finally
    Info.Free;
    if Assigned(ResultSet) then
      ResultSet.Close;
    if Assigned(ResultSet2) then
      ResultSet2.Close;
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL DBC ResultSet with use results.
}
procedure TZTestDbcMySQLCase.TestUseResultSetForwardOnly;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  Info.Add('useresult=true');
  Statement := Connection.CreateStatementWithParams(Info);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtForwardOnly);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, False);
  finally
    Info.Free;
    if Assigned(ResultSet) then
      ResultSet.Close;
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.TestUseResultSetScrollable;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Add('useresult=true');
  Statement := Connection.CreateStatementWithParams(Info);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    Check(ResultSet.Last);
    Check(ResultSet.First);
    Check(ResultSet.Next);
    Check(ResultSet.Next);
    Check(ResultSet.Last);
    Check(not ResultSet.Next);
    Check(ResultSet.IsAfterLast);
    ResultSet.BeforeFirst;
  finally
    Info.Free;
    if Assigned(ResultSet) then
      ResultSet.Close;
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.TestUseResultSetUpdateable;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Add('useresult=true');
  Statement := Connection.CreateStatementWithParams(Info);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
    CheckNotNull(ResultSet);
    Check(ResultSet.Last);
    Check(ResultSet.First);
    Check(ResultSet.Next);
    Check(ResultSet.Next);
    Check(ResultSet.Last);
    Check(not ResultSet.Next);
    Check(ResultSet.IsAfterLast);
    ResultSet.BeforeFirst;
  finally
    Info.Free;
    if Assigned(ResultSet) then
      ResultSet.Close;
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL AutoIncremented fields.
}
procedure TZTestDbcMySQLCase.TestAutoIncFields;
const
  c_id_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  c_name_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  try
    ResultSet := Statement.ExecuteQuery('SELECT c_id, c_name FROM cargo');
    CheckNotNull(ResultSet);

    ResultSet.MoveToInsertRow;
    ResultSet.UpdateString(c_name_Index, 'xxx');
    CheckEquals(0, ResultSet.GetInt(c_id_Index));
    CheckEquals('xxx', ResultSet.GetString(c_name_Index));

    ResultSet.InsertRow;
    Check(ResultSet.GetInt(c_id_Index) <> 0);
    CheckEquals('xxx', ResultSet.GetString(c_name_Index));

    ResultSet.DeleteRow;
  finally
    if Assigned(ResultSet) then
      ResultSet.Close;
    Statement.Close;
  end;
end;

{**
  Runs a test for MySQL default values.
}
procedure TZTestDbcMySQLCase.TestDefaultValues;
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
  try
    Statement.ExecuteUpdate('delete from default_values');

    ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3,d_fld4,d_fld5,d_fld6 FROM default_values');
    CheckNotNull(ResultSet);

    ResultSet.MoveToInsertRow;
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
    if Assigned(ResultSet) then
      ResultSet.Close;
    Statement.Close;
  end;
end;

procedure TZTestDbcMySQLCase.TestSelectThreeQueriesGetMoreResults;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  try
    InternalTestSelectThreeQueriesGetMoreResults(Connection.PrepareStatementWithParams('', Info));
    InternalTestSelectThreeQueriesGetMoreResults(Connection.CreateStatement);
    InternalTestSelectThreeQueriesGetMoreResults(Connection.PrepareStatement(''));
  finally
    Info.Free;
  end;
end;

procedure TZTestDbcMySQLCase.TestSelectTwoQueriesGetMoreResults;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  try
    InternalTestSelectTwoQueriesGetMoreResults(Connection.PrepareStatementWithParams('', Info));
    InternalTestSelectTwoQueriesGetMoreResults(Connection.CreateStatement);
    InternalTestSelectTwoQueriesGetMoreResults(Connection.PrepareStatement(''));
  finally
    Info.Free;
  end;
end;

procedure TZTestDbcMySQLCase.TestSelectTwoQueriesNoGetResults;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  try
    InternalTestSelectTwoQueriesNoGetResults(Connection.PrepareStatementWithParams('', Info));
    InternalTestSelectTwoQueriesNoGetResults(Connection.CreateStatement);
    InternalTestSelectTwoQueriesNoGetResults(Connection.PrepareStatement(''));
  finally
    Info.Free;
  end;
end;

procedure TZTestDbcMySQLCase.TestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS;
var
  Info: TStrings;
begin
  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  try
    InternalTestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS(Connection.PrepareStatementWithParams('', Info));
    InternalTestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS(Connection.CreateStatement);
    InternalTestSelectTwoQueriesGetMoreResultsWithCloseOfFirstRS(Connection.PrepareStatement(''));
  finally
    Info.Free;
  end;
end;

procedure TZTestDbcMySQLCase.TestBitFields;
var
  Statement: IZStatement;
  Info: TStrings;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  try
    CheckBitFields(Statement.ExecuteQuery('select * from TEST_BIT_FIELDS'));
  finally
    Statement.Close;
  end;

  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  try
    CheckBitFields(Statement.ExecuteQuery('select * from TEST_BIT_FIELDS'));
  finally
    Statement.Close;
  end;

  Info := TStringList.Create;
  Info.Values[DSProps_PreferPrepared] := StrTrue;
  try
    Statement := Connection.PrepareStatementWithParams('', Info);
    CheckNotNull(Statement);
    try
      CheckBitFields(Statement.ExecuteQuery('select * from TEST_BIT_FIELDS'));
    finally
      Statement.Close;
    end;

    Statement := Connection.PrepareStatementWithParams('', Info);
    CheckNotNull(Statement);
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcUpdatable);
    try
      CheckBitFields(Statement.ExecuteQuery('select * from TEST_BIT_FIELDS'));
    finally
      Statement.Close;
    end;
  finally
    FreeAndNil(Info);
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcMySQLCase.Suite);
end.
