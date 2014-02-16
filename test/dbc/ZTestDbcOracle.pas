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
  published
    procedure TestConnection;
    procedure TestStatement;
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

uses Types, ZTestConsts, ZTestCase, ZDbcResultSet, ZVariant;

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
  except
    // Ignore.
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
    Statement.SetInt(1, TEST_ROW_ID);
    Statement.ExecuteUpdatePrepared;

    Statement1 := Connection.PrepareStatement(
      'INSERT INTO department(dep_id,dep_name,dep_shname,dep_address)'
      + ' VALUES(?,?,?,?)');
    CheckNotNull(Statement1);
    try
      Statement1.SetInt(1, TEST_ROW_ID);
      Statement1.SetString(2, 'xyz');
      Statement1.SetNull(3, stString);
      Stream := TStringStream.Create('abc'#10'def'#13'hgi');
      try
        Statement1.SetAsciiStream(4, Stream);
      finally
        Stream.Free;
      end;
      CheckEquals(1, Statement1.ExecuteUpdatePrepared);
    finally
      Statement1.Close;
    end;

    Statement.SetInt(1, TEST_ROW_ID);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);
    Statement.ExecutePrepared;
    CheckEquals(0, Statement.GetUpdateCount);

    Statement1 := Connection.PrepareStatement(
      'SELECT count(*) FROM people WHERE p_id<>? AND p_begin_work<>?');
    CheckNotNull(Statement1);
    try
      Statement1.SetNull(1, stInteger);
      Statement1.SetNull(2, stTimestamp);
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
//        CheckEquals(5, ResultSet.GetInt(1));
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
      Statement1.SetNull(1, stAsciiStream);
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
    Statement.SetBinaryStream(1, Stream);
    CheckEquals(1, Statement.ExecuteUpdatePrepared);

    Statement1 := Connection.PrepareStatement(
      'select b_id, b_blob from blob_values order by b_id');
    CheckNotNull(Statement1);
    try
      ResultSet := Statement1.ExecuteQueryPrepared;
      try
        Check(ResultSet.Next);
        Check(not ResultSet.IsNull(2));
        CheckEquals(0, ResultSet.GetBlob(2).Length, 'Wrong blob length');
        Check(ResultSet.Next);
        CheckEquals(20, ResultSet.GetBlob(2).Length, 'Wrong blob length (2)');
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
  CheckEquals(1, ResultSet.GetInt(1));
  CheckEquals('', ResultSet.GetString(2));
  CheckEquals('', ResultSet.GetString(3));
  CheckEquals('', ResultSet.GetString(4));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(1));
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(2).GetString);
  CheckEquals(RawByteString('Test string'), ResultSet.GetBlob(3).GetString);
  CheckEquals(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00,
    String(ResultSet.GetBlob(4).GetString));

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(1));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(2).GetString));
  CheckEquals('111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111',
    String(ResultSet.GetBlob(3).GetString));
  Check(ResultSet.IsNull(4));

  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery(
    'SELECT n_id, n_raw, n_longraw, n_blob FROM binary_values ORDER BY n_id');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);
  CheckEquals(1, ResultSet.GetInt(1));
  CheckEquals('', ResultSet.GetString(2));
  CheckEquals('', ResultSet.GetString(3));
  CheckEquals('', ResultSet.GetString(4));

  Check(ResultSet.Next);
  CheckEquals(2, ResultSet.GetInt(1));
  CheckEquals(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00,
    String(ResultSet.GetBlob(2).GetString));
  CheckEquals(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00,
    String(ResultSet.GetBlob(3).GetString));
  CheckEquals(#01#02#03#04#05#06#07#08#09#00#01#02#03#04#05#06#07#08#09#00,
    String(ResultSet.GetBlob(4).GetString));

  Check(ResultSet.Next);
  CheckEquals(3, ResultSet.GetInt(1));
  Check(ResultSet.IsNull(2));
  Check(ResultSet.IsNull(3));
  Check(ResultSet.IsNull(4));

  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

{**
  Test number datatype reading
}

procedure TZTestDbcOracleCase.TestNumbers;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery(
    'SELECT * FROM number_values where n_id = 1 ');
  CheckNotNull(ResultSet);

  Check(ResultSet.Next);

  // 1, -128,-32768,-2147483648,-9223372036854775808, -99999.9999
  CheckEquals(1, ResultSet.GetInt(1));
  CheckEquals(-128, ResultSet.GetInt(2));
  CheckEquals(-32768, ResultSet.GetInt(3));
{$IFDEF FPC}
  CheckEquals(-2147483648, ResultSet.GetInt(4));
  // !! in oracle we can only use double precission numbers now
  CheckEquals(-9223372036854775808, ResultSet.GetBigDecimal(5), 10000);
{$ENDIF}
  CheckEquals(-99999.9999, ResultSet.GetDouble(6), 0.00001);
end;

{**
  Test the large amount data in blob
}

procedure TZTestDbcOracleCase.TestLargeBlob;
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
      PByteArray(InStm.Memory)[i] := Random(256);
      Inc(i, Random(1000));
    end;
    // inserting
    PStatement := Connection.PrepareStatement(
      Format('insert into blob_values(b_id, b_blob) values (%d, ?)', [TEST_ROW_ID]));
    CheckNotNull(PStatement);
    PStatement.SetBinaryStream(1, InStm);
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
    CheckEquals(TestSize, ResultSet.GetBlob(2).Length, 'Wrong blob length');
    OutBytes := ResultSet.GetBytes(2);
    CheckEquals(TestSize, Length(OutBytes), 'Wrong blob bytes length');
    CheckEqualsMem(InStm.Memory, @OutBytes[0], TestSize, 'Wrong blob content (byte array)');
    OutStr := ResultSet.GetBinaryString(2);
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
    PStatement.SetTimestamp(1, TestDate);
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
    CheckEqualsDate(TestDate, ResultSet.GetTimestamp(2), [dpYear..dpSec], 'DATE type must have 1 sec precission');
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
  PStatement.SetInt(1, 1);
  PStatement.SetNull(2, stString);  // null clears variable memory ref
  try
    PStatement.ExecuteUpdatePrepared;
    Fail('Primary key violation expected');
  except
  end;
  // rerun with new value (and check, that prev error dont corrupt PStatement)
  try
    PStatement.SetInt(1, TEST_ROW_ID);
    PStatement.SetString(2, TestStr);
    CheckEquals(1, PStatement.ExecuteUpdatePrepared, 'Row insert');

    // selectiong
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    ResultSet := Statement.ExecuteQuery(
      'SELECT s_id, s_varchar FROM string_values where s_id = ' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);

    // checking value
    CheckEquals(TestStr, ResultSet.GetString(2));
  finally
    PStatement := Connection.PrepareStatement(
    'DELETE FROM string_values WHERE s_id=' + IntToStr(TEST_ROW_ID));
    PStatement.ExecuteUpdatePrepared;
  end;
end;

{$WARNINGS OFF} //implizit string conversion of...
procedure TZTestDbcOracleCase.TestArrayBindings;
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
  PStatement := Connection.PrepareStatement(
  'insert into high_load(hl_id, stBoolean, stByte, stShort, stInteger, stLong, '+
    'stFloat, stDouble, stBigDecimal, stString, stUnicodeString, stBytes,'+
    'stDate, stTime, stTimestamp, stGUID, stAsciiStream, stUnicodeStream, '+
    'stBinaryStream) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)');
  CheckNotNull(PStatement);
  PrepareSomeData;
  PStatement.SetDataArray(1, hl_idArray, stInteger);
  PStatement.SetDataArray(2, stBooleanArray, stBoolean);
  PStatement.SetDataArray(3, stByteArray, stByte);
  PStatement.SetDataArray(4, stShortArray, stShort);
  PStatement.SetDataArray(5, stIntegerArray, stInteger);
  PStatement.SetDataArray(6, stLongArray, stLong);
  PStatement.SetDataArray(7, stFloatArray, stFloat);
  PStatement.SetDataArray(8, stDoubleArray, stDouble);
  PStatement.SetDataArray(9, stBigDecimalArray, stBigDecimal);
  PStatement.SetDataArray(10, stStringArray, stString, vtRawByteString);
  PStatement.SetDataArray(11, stUnicodeStringArray, stUnicodeString, vtUnicodeString);
  PStatement.SetDataArray(12, stBytesArray, stBytes);
  PStatement.SetDataArray(13, stDateArray, stDate);
  PStatement.SetDataArray(14, stTimeArray, stTime);
  PStatement.SetDataArray(15, stTimeStampArray, stTimeStamp);
  PStatement.SetDataArray(16, stGUIDArray, stGUID);
  PStatement.SetDataArray(17, stAsciiStreamArray, stString, vtCharRec);
  PStatement.SetDataArray(18, stUnicodeStreamArray, stString, vtUTF8String);
  PStatement.SetDataArray(19, stBinaryStreamArray, stBinaryStream);

  for i := 1 to 19 do
    case TZSQLType(Random(14)+1) of
      stBoolean:
        begin
          SetLength(stBooleanNullArray, Length(stBooleanNullArray) +1);
          SetLength(stBooleanNullArray[High(stBooleanNullArray)], 50);
          for J := 0 to 49 do
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
            if I = 1 then
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
{$WARNINGS ON} //implizit string conversion of...

initialization
  RegisterTest('dbc',TZTestDbcOracleCase.Suite);
end.
