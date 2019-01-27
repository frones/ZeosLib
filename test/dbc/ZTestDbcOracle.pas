{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for Oracle Database Connectivity Classes    }
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

unit ZTestDbcOracle;

interface
{$I ZDbc.inc}

uses Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZSqlTestCase, ZDbcOracle,
  ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcOracleCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
    function GetSupportedProtocols: string; override;
    //disabled tests:
    procedure TestStatement;
  published
    procedure TestConnection;
    procedure TestResultSet;
    procedure TestLongObjects;
    procedure TestPreparedStatement;
    procedure TestEmptyBlob;
    procedure TestNumbers;
    procedure TestLargeBlob;
    procedure TestDateWithTime;
    procedure TestFKError;
    procedure TestArrayBindings;
(*
    procedure TestDefaultValues;
*)
  end;


implementation

uses Types, ZTestConsts, ZTestCase, ZDbcResultSet, ZVariant, ZSysUtils;

{ TZTestDbcOracleCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcOracleCase.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

{**
  Runs a test for Oracle database connection.
}
procedure TZTestDbcOracleCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
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
  Runs a test for regular Oracle DBC Statement.
}
procedure TZTestDbcOracleCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  try
    Statement.ExecuteUpdate('SELECT * FROM equipment');
    Fail('Incorrect ExecuteUpdate behaviour');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
  Statement.Close;
end;

{**
  Runs a test for Oracle DBC ResultSet with stored results.
}
procedure TZTestDbcOracleCase.TestResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZDatabaseMetadata;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Metadata := Connection.GetMetadata;
  ResultSet := Metadata.GetPrimaryKeys('', 'ZEOSLIB', '');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM people');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment2');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM cargo');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery(
    'SELECT b_id, b_long, b_clob, b_blob FROM blob_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM binary_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for Oracle DBC PreparedStatement.
}
procedure TZTestDbcOracleCase.TestPreparedStatement;
const
  department_dep_id_Index = FirstDbcIndex;
  department_dep_name_Index = FirstDbcIndex+1;
  department_dep_shname_Index = FirstDbcIndex+2;
  department_dep_address_Index = FirstDbcIndex+3;
  people_p_id_Index = FirstDbcIndex;
  people_p_begin_work_Index = FirstDbcIndex+1;
var
  Statement: IZPreparedStatement;
  Statement1: IZPreparedStatement;
  ResultSet: IZResultSet;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'DELETE FROM department WHERE dep_id=?');
  CheckNotNull(Statement);
  try
    Statement.SetInt(department_dep_id_Index, TEST_ROW_ID);
    Statement.ExecuteUpdatePrepared;

    Statement1 := Connection.PrepareStatement(
      'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
      + ' VALUES(?,?,?,?)');
    CheckNotNull(Statement1);
    try
      Statement1.SetInt(department_dep_id_Index, TEST_ROW_ID);
      Statement1.SetString(department_dep_name_Index, 'xyz');
      Statement1.SetNull(department_dep_shname_Index, stString);
      Stream := TStringStream.Create('abc'#10'def'#13'hgi');
      try
        Statement1.SetAsciiStream(department_dep_address_Index, Stream);
      finally
        Stream.Free;
      end;
      CheckEquals(1, Statement1.ExecuteUpdatePrepared);
    finally
      Statement1.Close;
    end;

    Statement.SetInt(department_dep_id_Index, TEST_ROW_ID);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
    Statement.ExecutePrepared;
    CheckEquals(0, Statement.GetUpdateCount);

    Statement1 := Connection.PrepareStatement(
      'SELECT count(*) FROM people WHERE p_id<>? AND p_begin_work<>?');
    CheckNotNull(Statement1);
    try
      Statement1.SetNull(people_p_id_Index, stInteger);
      Statement1.SetNull(people_p_begin_work_Index, stTimestamp);
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
//        CheckEquals(5, ResultSet.GetInt(people_count_Index));
        Check(not ResultSet.Next);
      finally
        ResultSet.Close;
      end;
    finally
      Statement1.Close;
    end;

    Statement1 := Connection.PrepareStatement('UPDATE people SET p_resume=?');
    CheckNotNull(Statement1);
    try
      Statement1.SetNull(FirstDbcIndex, stAsciiStream);
      CheckEquals(5, Statement1.ExecuteUpdatePrepared);
    finally
      Statement1.Close;
    end;
  finally
    Statement.Close;
  end;
end;

{**
  Special blob case: blob is not null, but its empty by length
}

procedure TZTestDbcOracleCase.TestEmptyBlob;
const
  update_blob_values_b_blob_Index = FirstDbcIndex;
  select_blob_values_b_blob_Index = FirstDbcIndex+1;
var
  Statement: IZPreparedStatement;
  Statement1: IZPreparedStatement;
  ResultSet: IZResultSet;
  Stream: TStream;
begin
  Statement := Connection.PrepareStatement(
    'update blob_values set b_blob=? where b_id = 1');
  CheckNotNull(Statement);
  Stream := TMemoryStream.Create; // empty stream
  try
    Statement.SetBinaryStream(update_blob_values_b_blob_Index, Stream);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);

    Statement1 := Connection.PrepareStatement(
      'select b_id, b_blob from blob_values order by b_id');
    CheckNotNull(Statement1);
    try
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
        Check(not ResultSet.IsNull(select_blob_values_b_blob_Index));
        CheckEquals(0, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length');
        Check(ResultSet.Next);
        CheckEquals(20, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length (2)');
      finally
        ResultSet.Close;
      end;
    finally
      Statement1.Close;
    end;
  finally
    Statement.Close;
    Stream.Free;
  end;
end;



(*
{**
  Runs a test for Oracle default values.
}
procedure TZTestDbcOracleCase.TestDefaultValues;
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

*)

{**
  Runs a test for Oracle long objects.
}
procedure TZTestDbcOracleCase.TestLongObjects;
const
  blob_values_b_id_Index = FirstDbcIndex;
  blob_values_b_long_Index = FirstDbcIndex+1;
  blob_values_b_clob_Index = FirstDbcIndex+2;
  blob_values_b_blob_Index = FirstDbcIndex+3;
  binary_values_n_id_Index = FirstDbcIndex;
  binary_values_n_raw_Index = FirstDbcIndex+1;
  binary_values_n_longraw_Index = FirstDbcIndex+2;
  binary_values_n_blob_Index = FirstDbcIndex+3;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
//  Statement.SetResultSetType(rtScrollInsensitive);
//  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery(
    'SELECT b_id, b_long, b_clob, b_blob FROM blob_values ORDER BY b_id');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  CheckEquals(1, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_long_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_clob_Index));
  CheckEquals('', ResultSet.GetString(blob_values_b_blob_Index));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(blob_values_b_long_Index).GetString);
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(blob_values_b_clob_Index).GetString);
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 = String(ResultSet.GetBlob(blob_values_b_blob_Index).GetString), 'Comparision of binary strings failed.');

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(blob_values_b_id_Index));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(blob_values_b_long_Index).GetString));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(blob_values_b_clob_Index).GetString));
  Check(ResultSet.IsNull(blob_values_b_blob_Index));

  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery(
    'SELECT n_id, n_raw, n_longraw, n_blob FROM binary_values ORDER BY n_id');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  CheckEquals(1, ResultSet.GetInt(binary_values_n_id_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_raw_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_longraw_Index));
  CheckEquals('', ResultSet.GetString(binary_values_n_blob_Index));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(binary_values_n_id_Index));
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 =
    {$IFDEF UNICODE}Ascii7ToUnicodeString{$ENDIF}(BytesToStr(ResultSet.GetBytes(binary_values_n_raw_Index))), 'Second comparision of binary strings failed');
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 =
    String(ResultSet.GetBlob(binary_values_n_longraw_Index).GetString), 'Third comparision of binary strings failed');
  Check(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00 =
    String(ResultSet.GetBlob(binary_values_n_blob_Index).GetString), 'Fourth comparision of binary strings failed');

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(binary_values_n_id_Index));
  Check(ResultSet.IsNull(binary_values_n_raw_Index));
  Check(ResultSet.IsNull(binary_values_n_longraw_Index));
  Check(ResultSet.IsNull(binary_values_n_blob_Index));

  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Test number datatype reading
}

procedure TZTestDbcOracleCase.TestNumbers;
const
  number_values_n_id_Index = FirstDbcIndex;
  number_values_n_tint_Index = FirstDbcIndex+1;
  number_values_n_sint_Index = FirstDbcIndex+2;
  number_values_n_int_Index = FirstDbcIndex+3;
  number_values_n_bdecimal_Index = FirstDbcIndex+4;
  number_values_n_numeric_Index = FirstDbcIndex+5;
  number_values_n_float_Index = FirstDbcIndex+6;
  number_values_n_real_Index = FirstDbcIndex+7;
  number_values_n_dprecision_Index = FirstDbcIndex+8;
  number_values_n_money_Index = FirstDbcIndex+9;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery(
    'SELECT * FROM number_values order by 1');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  // 1, -128,-32768,-2147483648,-9223372036854775808, -99999.9999
  // -3.402823466E+38, -3.402823466E+38, -1.7976931348623157E+38, -21474836.48
  CheckEquals(1, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(-128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(-32768, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(Low(LongInt), ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(Low(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(Low(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(-99999.9999, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-99999.9999, ResultSet.GetCurrency(number_values_n_numeric_Index));
  {EH: oracle uses the number here, so we can't compare any other way except using a string or simply forget it
    it's officially documented the number is not accurate for FPU floats}
  CheckEquals('-3.402823466E38', ResultSet.GetString(number_values_n_float_Index));
  CheckEquals('-3.402823466E38', ResultSet.GetString(number_values_n_real_Index));
  //CheckEquals(-1.7976931348623157E38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-21474836.48, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //2,-128,-32768,-2147483648,-9223372036854775808, -11111.1111,
	//-1.175494351E-38, -1.175494351E-38, -2.2250738585072014E-38, 21474836.47
  CheckEquals(2, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(-128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(-32768, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(Low(LongInt), ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(Low(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(Low(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(-11111.1111, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-11111.1111, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(-1.175494351E-38, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(-1.175494351E-38, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(-2.2250738585072014E-38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(21474836.47, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //3, 0, 0, 0, 0, 0, 0, 0, 0, '0'
  CheckEquals(3, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(0, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(0, ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(0, ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(0, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(0, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(0, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(0, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(0, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(0, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //4, 128, 32767, 2147483647, 9223372036854775807, 11111.1111,
	//3.402823466E+38, 3.402823466E+38, 1.7976931348623157E+38, -922337203685477.5808
  CheckEquals(4, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(32767, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(2147483647, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(High(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(High(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(11111.1111, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(11111.1111, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals('3.402823466E38', ResultSet.GetString(number_values_n_float_Index));
  CheckEquals('3.402823466E38', ResultSet.GetString(number_values_n_real_Index));
  //CheckEquals(1.7976931348623157E+38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(-922337203685477.5808, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(ResultSet.Next);
  //5, 128, 32767, 147483647, 9223372036854775807,  99999.9999,
	//1.175494351E-38, 1.175494351E-38, 2.2250738585072014E-38, 922337203685477.5807
  CheckEquals(5, ResultSet.GetInt(number_values_n_id_Index));
  CheckEquals(128, ResultSet.GetInt(number_values_n_tint_Index));
  CheckEquals(32767, ResultSet.GetInt(number_values_n_sint_Index));
  CheckEquals(147483647, ResultSet.GetInt(number_values_n_int_Index));
  {$IFNDEF CPU64} //EH: FPU 64 does no longer support the 80bit 10byte real-> impossible to resolve!
  //this needs to be reviewed with true BCD record structs later on (7.3+)
  CheckEquals(High(Int64), ResultSet.GetBigDecimal(number_values_n_bdecimal_Index), 10000);
  {$ENDIF !CPU64}
  CheckEquals(High(Int64), ResultSet.GetLong(number_values_n_bdecimal_Index));
  CheckEquals(99999.9999, ResultSet.GetDouble(number_values_n_numeric_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(99999.9999, ResultSet.GetCurrency(number_values_n_numeric_Index));
  CheckEquals(1.175494351E-38, ResultSet.GetFloat(number_values_n_float_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(1.175494351E-38, ResultSet.GetFloat(number_values_n_real_Index), FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(2.2250738585072014E-38, ResultSet.GetDouble(number_values_n_dprecision_Index), FLOAT_COMPARE_PRECISION);
  CheckEquals(922337203685477.5807, ResultSet.GetCurrency(number_values_n_money_Index));
  Check(not ResultSet.Next);
end;

{**
  Test the large amount data in blob
}

procedure TZTestDbcOracleCase.TestLargeBlob;
const
  insert_blob_values_b_blob_Index = FirstDbcIndex;
  select_blob_values_b_blob_Index = FirstDbcIndex+1;
var
  InStm: TMemoryStream;
  OutBytes: TBytes;
  OutStr: AnsiString;
  i, TestSize: Integer;
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  InStm := TMemoryStream.Create;
  try
    TestSize := 1050 * 1024 + Random(100000); // relative big random size
    InStm.SetSize(TestSize);
    // randomizing content
    i := 0;
    while i < TestSize do begin
      {$R-} //EH range check does overrun the defined TByteArray = array[0..32767] of Byte range -> turn off!
      PByteArray(InStm.Memory)[i] := Random(256);
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      Inc(i, Random(1000));
    end;
    // inserting
    PStatement := Connection.PrepareStatement(
      Format('insert into blob_values(b_id, b_blob) values (%d, ?)', [TEST_ROW_ID]));
    CheckNotNull(PStatement);
    PStatement.SetBinaryStream(insert_blob_values_b_blob_Index, InStm);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');
    PStatement.Close;

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);

    ResultSet := Statement.ExecuteQuery(
      'SELECT b_id, b_blob FROM blob_values where b_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEquals(TestSize, ResultSet.GetBlob(select_blob_values_b_blob_Index).Length, 'Wrong blob length');
    OutBytes := ResultSet.GetBytes(select_blob_values_b_blob_Index);
    CheckEquals(TestSize, Length(OutBytes), 'Wrong blob bytes length');
    CheckEqualsMem(InStm.Memory, @OutBytes[0], TestSize, 'Wrong blob content (byte array)');
    OutStr := ResultSet.GetBinaryString(select_blob_values_b_blob_Index);
    CheckEquals(TestSize, Length(OutStr), 'Wrong blob string length');
    CheckEqualsMem(InStm.Memory, Pointer(OutStr), TestSize, 'Wrong blob content (string)');
  finally
    InStm.Free;

    PStatement := Connection.PrepareStatement(
    'DELETE FROM blob_values WHERE b_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{**
  Test oracle DATE type precission is 1 second
}

procedure TZTestDbcOracleCase.TestDateWithTime;
const
  param_d_date_Index = FirstDbcIndex;
  field_d_date_Index = FirstDbcIndex+1;
var
  TestDate: TDateTime;
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  TestDate := EncodeDate(2009, 12, 20) + EncodeTime(20, 09, 11, 0);
  try
    // inserting
    PStatement := Connection.PrepareStatement(
      Format('insert into date_values(d_id, d_date) values(%d, ?)', [TEST_ROW_ID]));
    CheckNotNull(PStatement);
    PStatement.SetTimestamp(param_d_date_Index, TestDate);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');
    PStatement.Close;

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    ResultSet := Statement.ExecuteQuery(
      'SELECT d_id, d_date FROM date_values where d_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEqualsDate(TestDate, ResultSet.GetTimestamp(field_d_date_Index), [dpYear..dpSec], 'DATE type must have 1 sec precission');
  finally
    PStatement := Connection.PrepareStatement(
    'DELETE FROM date_values WHERE d_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{**
  Test PK-error and possible prepared statement corruption after it
}

procedure TZTestDbcOracleCase.TestFKError;
const
  TestStr = 'The source code of the ZEOS Libraries and packages are distributed under the Library GNU General Public License';
  s_id_Index = FirstDbcIndex;
  s_varchar_Index = FirstDbcIndex+1;
var
  Statement: IZStatement;
  PStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  // inserting
  PStatement := Connection.PrepareStatement(
    'insert into string_values(s_id, s_varchar) values(?, ?)');
  CheckNotNull(PStatement);
  // making PK error
  PStatement.SetInt(s_id_Index, 1);
  PStatement.SetNull(s_varchar_Index, stString);  // null clears variable memory ref
  try
    PStatement.ExecuteUpdatePrepared;
    Fail('Primary key violation expected');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  // rerun with new value (and check, that prev error dont corrupt PStatement)
  try
    PStatement.SetInt(s_id_Index, TEST_ROW_ID);
    PStatement.SetString(s_varchar_Index, TestStr);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    ResultSet := Statement.ExecuteQuery(
      'SELECT s_id, s_varchar FROM string_values where s_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEquals(TestStr, ResultSet.GetString(s_varchar_Index));
  finally
    PStatement := Connection.PrepareStatement(
    'DELETE FROM string_values WHERE s_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{$WARNINGS OFF} //implizit string conversion of...
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZTestDbcOracleCase.TestArrayBindings;
const
  hl_id_Index           = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  stBooleanArray_Index  = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  stByte_Index          = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  stShort_Index         = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  stInteger_Index       = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
  stLong_Index          = {$IFDEF GENERIC_INDEX}5{$ELSE}6{$ENDIF};
  stFloat_Index         = {$IFDEF GENERIC_INDEX}6{$ELSE}7{$ENDIF};
  stDouble_Index        = {$IFDEF GENERIC_INDEX}7{$ELSE}8{$ENDIF};
  stBigDecimal_Index    = {$IFDEF GENERIC_INDEX}8{$ELSE}9{$ENDIF};
  stString_Index        = {$IFDEF GENERIC_INDEX}9{$ELSE}10{$ENDIF};
  stUnicode_Index       = {$IFDEF GENERIC_INDEX}10{$ELSE}11{$ENDIF};
  stBytes_Index         = {$IFDEF GENERIC_INDEX}11{$ELSE}12{$ENDIF};
  stDate_Index          = {$IFDEF GENERIC_INDEX}12{$ELSE}13{$ENDIF};
  stTime_Index          = {$IFDEF GENERIC_INDEX}13{$ELSE}14{$ENDIF};
  stTimeStamp_Index     = {$IFDEF GENERIC_INDEX}14{$ELSE}15{$ENDIF};
  stGUID_Index          = {$IFDEF GENERIC_INDEX}15{$ELSE}16{$ENDIF};
  stAsciiStream_Index   = {$IFDEF GENERIC_INDEX}16{$ELSE}17{$ENDIF};
  stUnicodeStream_Index = {$IFDEF GENERIC_INDEX}17{$ELSE}18{$ENDIF};
  stBinaryStream_Index  = {$IFDEF GENERIC_INDEX}18{$ELSE}19{$ENDIF};
var
  PStatement: IZPreparedStatement;
  hl_idArray: TIntegerDynArray;
  stBooleanArray: TBooleanDynArray;
  stByteArray: TByteDynArray;
  stShortArray: TShortIntDynArray;
  stLongArray: TInt64DynArray;
  stIntegerArray: TIntegerDynArray;
  stFloatArray: TSingleDynArray;
  stDoubleArray: TDoubleDynArray;
  stBigDecimalArray: TExtendedDynArray;
  stStringArray: TRawByteStringDynArray;
  stUnicodeStringArray: TUnicodeStringDynArray;
  stBytesArray: TBytesDynArray;
  stDateArray: TDateTimeDynArray;
  stTimeArray: TDateTimeDynArray;
  stTimeStampArray: TDateTimeDynArray;
  stGUIDArray: TGUIDDynArray;
  stAsciiStreamArray: TZCharRecDynArray;
  stUnicodeStreamArray: TUTF8StringDynArray;
  stBinaryStreamArray: TInterfaceDynArray;
  stBooleanNullArray: array of TBooleanDynArray;
  stByteNullArray: array of TByteDynArray;
  stShortNullArray: array of TShortIntDynArray;
  stWordNullArray: array of TWordDynArray;
  stSmallNullArray: array of TSmallIntDynArray;
  stLongWordNullArray: array of TLongWordDynArray;
  stIntegerNullArray: array of TIntegerDynArray;
  stULongNullArray: array of TUInt64DynArray;
  stLongNullArray: array of TInt64DynArray;
  stFloatNullArray: array of TSingleDynArray;
  stDoubleNullArray: array of TDoubleDynArray;
  stCurrencyNullArray: array of TCurrencyDynArray;
  stBigDecimalNullArray: array of TExtendedDynArray;
  stStringNullArray: array of TRawByteStringDynArray;
  stUnicodeStringNullArray: array of TUnicodeStringDynArray;
  I, J: Integer;

  procedure PrepareSomeData;
  var I: Integer;
  begin
    SetLength(hl_idArray, 50);
    SetLength(stBooleanArray, 50);
    SetLength(stByteArray, 50);
    SetLength(stShortArray, 50);
    SetLength(stLongArray, 50);
    SetLength(stIntegerArray, 50);
    SetLength(stFloatArray, 50);
    SetLength(stDoubleArray, 50);
    SetLength(stBigDecimalArray, 50);
    SetLength(stStringArray, 50);
    SetLength(stUnicodeStringArray, 50);
    SetLength(stBytesArray, 50);
    SetLength(stDateArray, 50);
    SetLength(stTimeArray, 50);
    SetLength(stTimeStampArray, 50);
    SetLength(stGUIDArray, 50);
    SetLength(stAsciiStreamArray, 50);
    SetLength(stUnicodeStreamArray, 50);
    SetLength(stBinaryStreamArray, 50);
    for i := 0 to 49 do
    begin
      hl_idArray[i] := I;
      stBooleanArray[i] := Boolean(Random(1));
      stByteArray[i] := Random(255);
      stShortArray[i] := I;
      stLongArray[I] := I;
      stIntegerArray[I] := I;
      stFloatArray[i] := RandomFloat(-5000, 5000);
      stDoubleArray[i] := RandomFloat(-5000, 5000);
      stBigDecimalArray[i] := RandomFloat(-5000, 5000);
      stStringArray[i] := RandomStr(Random(99)+1);
      stUnicodeStringArray[i] := RandomStr(Random(254+1));
      stBytesArray[i] := RandomBts(50);
      stDateArray[i] := Trunc(Now);
      stTimeArray[i] := Frac(Now);
      stTimeStampArray[i] := Now;
      stGUIDArray[i] := RandomGUID;
      stAsciiStreamArray[i].Len := Length(stStringArray[i]);
      stAsciiStreamArray[i].P := Pointer(stStringArray[i]);
      stAsciiStreamArray[i].CP := Connection.GetConSettings^.ClientCodePage^.CP; {safe we're passing ASCII7 only to the raws}
      stUnicodeStreamArray[i] := RandomStr(MaxPerformanceLobSize);
      stBinaryStreamArray[i] := TZAbstractBlob.Create;
      (stBinaryStreamArray[i] as IZBlob).SetBytes(RandomBts(MaxPerformanceLobSize));
    end;
  end;
begin
  Connection.PrepareStatement('delete from high_load').ExecutePrepared;
  PStatement := Connection.PrepareStatement(
  'insert into high_load(hl_id, stBoolean, stByte, stShort, stInteger, stLong, '+
    'stFloat, stDouble, stBigDecimal, stString, stUnicodeString, stBytes,'+
    'stDate, stTime, stTimestamp, stGUID, stAsciiStream, stUnicodeStream, '+
    'stBinaryStream) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)');
  CheckNotNull(PStatement);
  PrepareSomeData;
  PStatement.SetDataArray(hl_id_Index, hl_idArray, stInteger);
  PStatement.SetDataArray(stBooleanArray_Index, stBooleanArray, stBoolean);
  PStatement.SetDataArray(stByte_Index, stByteArray, stByte);
  PStatement.SetDataArray(stShort_Index, stShortArray, stShort);
  PStatement.SetDataArray(stInteger_Index, stIntegerArray, stInteger);
  PStatement.SetDataArray(stLong_Index, stLongArray, stLong);
  PStatement.SetDataArray(stFloat_Index, stFloatArray, stFloat);
  PStatement.SetDataArray(stDouble_Index, stDoubleArray, stDouble);
  PStatement.SetDataArray(stBigDecimal_Index, stBigDecimalArray, stBigDecimal);
  PStatement.SetDataArray(stString_Index, stStringArray, stString, vtRawByteString);
  PStatement.SetDataArray(stUnicode_Index, stUnicodeStringArray, stUnicodeString, vtUnicodeString);
  PStatement.SetDataArray(stBytes_Index, stBytesArray, stBytes);
  PStatement.SetDataArray(stDate_Index, stDateArray, stDate);
  PStatement.SetDataArray(stTime_Index, stTimeArray, stTime);
  PStatement.SetDataArray(stTimeStamp_Index, stTimeStampArray, stTimeStamp);
  PStatement.SetDataArray(stGUID_Index, stGUIDArray, stGUID);
  PStatement.SetDataArray(stAsciiStream_Index, stAsciiStreamArray, stString, vtCharRec);
  PStatement.SetDataArray(stUnicodeStream_Index, stUnicodeStreamArray, stString, vtUTF8String);
  PStatement.SetDataArray(stBinaryStream_Index, stBinaryStreamArray, stBinaryStream);

  for i := FirstDbcIndex to 19{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    case TZSQLType(Random(14)+1) of
      stBoolean:
        begin
          SetLength(stBooleanNullArray, Length(stBooleanNullArray) +1);
          SetLength(stBooleanNullArray[High(stBooleanNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stBooleanNullArray[High(stBooleanNullArray)][J] := False
            else
              stBooleanNullArray[High(stBooleanNullArray)][J] := Boolean(Random(1));
          PStatement.SetNullArray(I, stBoolean, stBooleanNullArray[High(stBooleanNullArray)]);
        end;
      stByte:
        begin
          SetLength(stByteNullArray, Length(stByteNullArray)+1);
          SetLength(stByteNullArray[High(stByteNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stByteNullArray[High(stByteNullArray)][J] := Ord(False)
            else
              stByteNullArray[High(stByteNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stByte, stByteNullArray[High(stByteNullArray)]);
        end;
      stShort:
        begin
          SetLength(stShortNullArray, Length(stShortNullArray)+1);
          SetLength(stShortNullArray[High(stShortNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stShortNullArray[High(stShortNullArray)][J] := 0
            else
              stShortNullArray[High(stShortNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stShort, stShortNullArray[High(stShortNullArray)]);
        end;
      stWord:
        begin
          SetLength(stWordNullArray, Length(stWordNullArray)+1);
          SetLength(stWordNullArray[High(stWordNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stWordNullArray[High(stWordNullArray)][j] := 0
            else
              stWordNullArray[High(stWordNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stWord, stWordNullArray[High(stWordNullArray)]);
        end;
      stSmall:
        begin
          SetLength(stSmallNullArray, Length(stSmallNullArray)+1);
          SetLength(stSmallNullArray[High(stSmallNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stSmallNullArray[High(stSmallNullArray)][J] := 0
            else
              stSmallNullArray[High(stSmallNullArray)][J] := -Random(2);
          PStatement.SetNullArray(I, stSmall, stSmallNullArray[High(stSmallNullArray)]);
        end;
      stLongWord:
        begin
          SetLength(stLongWordNullArray, Length(stLongWordNullArray)+1);
          SetLength(stLongWordNullArray[High(stLongWordNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stLongWordNullArray[High(stLongWordNullArray)][J] := 0
            else
              stLongWordNullArray[High(stLongWordNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stLongWord, stLongWordNullArray[High(stLongWordNullArray)]);
        end;
      stInteger:
        begin
          SetLength(stIntegerNullArray, Length(stIntegerNullArray)+1);
          SetLength(stIntegerNullArray[High(stIntegerNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stIntegerNullArray[High(stIntegerNullArray)][J] := 0
            else
              stIntegerNullArray[High(stIntegerNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stInteger, stIntegerNullArray[High(stIntegerNullArray)]);
        end;
      stULong:
        begin
          SetLength(stULongNullArray, Length(stULongNullArray)+1);
          SetLength(stULongNullArray[High(stULongNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stULongNullArray[High(stULongNullArray)][J] := 0
            else
              stULongNullArray[High(stULongNullArray)][J] := Random(2);
          PStatement.SetNullArray(I, stULong, stULongNullArray[High(stULongNullArray)]);
        end;
      stLong:
        begin
          SetLength(stLongNullArray, Length(stLongNullArray) +1);
          SetLength(stLongNullArray[High(stLongNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stLongNullArray[High(stLongNullArray)][J] := 0
            else
              stLongNullArray[High(stLongNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stLong, stLongNullArray[High(stLongNullArray)]);
        end;
      stFloat:
        begin
          SetLength(stFloatNullArray, Length(stFloatNullArray)+1);
          SetLength(stFloatNullArray[High(stFloatNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stFloatNullArray[High(stFloatNullArray)][J] := 0
            else
              stFloatNullArray[High(stFloatNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stFloat, stFloatNullArray[High(stFloatNullArray)]);
        end;
      stDouble:
        begin
          SetLength(stDoubleNullArray, Length(stDoubleNullArray)+1);
          SetLength(stDoubleNullArray[high(stDoubleNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stDoubleNullArray[high(stDoubleNullArray)][J] := 0
            else
              stDoubleNullArray[high(stDoubleNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stDouble, stDoubleNullArray[high(stDoubleNullArray)]);
        end;
      stCurrency:
        begin
          SetLength(stCurrencyNullArray, Length(stCurrencyNullArray)+1);
          SetLength(stCurrencyNullArray[High(stCurrencyNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stCurrencyNullArray[High(stCurrencyNullArray)][J] := 0
            else
              stCurrencyNullArray[High(stCurrencyNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stCurrency, stCurrencyNullArray[High(stCurrencyNullArray)]);
        end;
      stBigDecimal:
        begin
          SetLength(stBigDecimalNullArray, Length(stBigDecimalNullArray)+1);
          SetLength(stBigDecimalNullArray[High(stBigDecimalNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stBigDecimalNullArray[High(stBigDecimalNullArray)][J] := 0
            else
              stBigDecimalNullArray[High(stBigDecimalNullArray)][J] := Random(2)-1;
          PStatement.SetNullArray(I, stBigDecimal, stBigDecimalNullArray[High(stBigDecimalNullArray)]);
        end;
      {stString:
        begin
          SetLength(stStringNullArray, Length(stStringNullArray)+1);
          SetLength(stStringNullArray[High(stStringNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
              else
                stStringNullArray[High(stStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stString, stStringNullArray[High(stStringNullArray)], vtRawByteString);
        end;}
      stUnicodeString:
        begin
          SetLength(stUnicodeStringNullArray, Length(stUnicodeStringNullArray)+1);
          SetLength(stUnicodeStringNullArray[High(stUnicodeStringNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'FALSE'
              else
                stUnicodeStringNullArray[High(stUnicodeStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stUnicodeString, stUnicodeStringNullArray[High(stUnicodeStringNullArray)], vtUnicodeString);
        end;
      else
        begin
          SetLength(stStringNullArray, Length(stStringNullArray)+1);
          SetLength(stStringNullArray[High(stStringNullArray)], 50);
          for J := 0 to 49 do
            if I = FirstDbcIndex then
              stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
            else
              if Random(2) = 0 then
                stStringNullArray[High(stStringNullArray)][J] := 'FALSE'
              else
                stStringNullArray[High(stStringNullArray)][J] := 'TRUE';
          PStatement.SetNullArray(I, stString, stStringNullArray[High(stStringNullArray)], vtRawByteString);
        end;
      {stBytes:
      stGUID:
      stDate:
      stTime:
      stTimestamp:
      stArray:
      stDataSet:
      stAsciiStream:
      stUnicodeStream:
      stBinaryStream:}
    end;
  PStatement.ExecuteUpdatePrepared;
  //SetLength(stShortNullArray, 0);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
{$WARNINGS ON} //implizit string conversion of...

initialization
  RegisterTest('dbc',TZTestDbcOracleCase.Suite);
end.
