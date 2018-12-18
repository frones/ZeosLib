{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Test Case for Generic Database Connectivity Classes   }
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

unit ZTestDbcGeneric;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, ZDbcIntfs, ZSqlTestCase,
  ZCompatibility;

type
  {** Implements a test case for . }

  { TZGenericTestDbcResultSet }

  TZGenericTestDbcResultSet = class(TZAbstractDbcSQLTestCase)
  private
  protected
  published
    procedure TestConnection;
    procedure TestCommitBehavior;
    procedure TestStatement;
    procedure TestPreparedStatement;
    procedure TestStoredResultSetUpdate;
    procedure TestCaseSensitive;
    procedure TestAliases;
    procedure TestStoredResultSet;
    procedure TestLastQuery;
    procedure TestNotNullValues;
    procedure TestConcurrency;
    procedure TestStringGetter;
    procedure TestStringToSignedIntegerConversions;
    procedure TestStringToUnsignedIntegerConversions;
    procedure TestAfterLast;
    procedure TestQuestionMarks;
  end;

implementation

uses ZSysUtils, ZTestConsts, ZFastCode, Types, ZVariant;

{ TZGenericTestDbcResultSet }
procedure TZGenericTestDbcResultSet.TestAfterLast;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  Resultset := Statement.ExecuteQuery('select * from people');
  try
    while ResultSet.Next do;
    Check(ResultSet.IsAfterLast, 'Missing afterlast logic');
  finally
    ResultSet.Close;
    ResultSet := nil;
    Statement.Close;
    Statement := nil;
  end;
end;

{**
   Test table with aliases
}
procedure TZGenericTestDbcResultSet.TestAliases;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
//  StrStream, BinStream: TMemoryStream;
//  StrStream1, BinStream1: TStream;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  try
    Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);
    Sql := 'DELETE FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);

    { Tests the equipment table }
    Sql := 'SELECT a.eq_id as id, a.eq_name as name, a.eq_type as type1,'
      + ' a.eq_cost + 10 as cost FROM equipment a where a.eq_id = '
      + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    { Inserts test record to equipment }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '1. ' + Sql);
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateIntByName('id', TEST_ROW_ID);
      UpdateNullByName('name');
      UpdateNullByName('type1');
      UpdateNullByName('cost');
      InsertRow;
      Close;
    end;
    ResultSet := nil;

    { Updates row for equipment}
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '2. ' + Sql);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetIntByName('id'));
      CheckEquals(True, IsNullByName('name'));
      CheckEquals(True, IsNullByName('type1'));
      CheckEquals(True, IsNullByName('cost'));

      UpdateStringByName('name', 'The some thing');
      UpdateIntByName('type1', 1);
      UpdateDoubleByName('cost', 12345.678);
      UpdateRow;
      Close;
    end;

    { Checks previous updated row}
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '3. ' + Sql);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals('The some thing', GetStringByName('name'));
      CheckEquals(1, GetIntByName('type1'));
  // Column cost is calculated is can't be updated
  //    CheckEquals(12355.678, GetFloatByName('cost'), 0.01);
      DeleteRow;
      Close;
    end;

    { Checks what record deleted }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '4. ' + Sql);
    CheckEquals(False, ResultSet.Next);
  finally
    Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);
    Sql := 'DELETE FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);
  end;
end;

{**
  Tests case sensetive tables
}
procedure TZGenericTestDbcResultSet.TestCaseSensitive;
const
  cs_id_Index  = FirstDbcIndex + 0;
  field1_Index = FirstDbcIndex + 1;
  field2_Index = FirstDbcIndex + 2;
  field3_Index = FirstDbcIndex + 3;
var
  Sql: string;
  Statement: IZPreparedStatement;
  ResultSet: IZResultSet;
  Metadata: IZDatabaseMetadata;
begin
  if ProtocolType in [protMySQL, protSQLite, protFreeTDS, protMSSQL,
                      protADO, protSyBase, protASA] then
    Exit; //not in build sripts because they depend to locale settings

  Metadata := Connection.GetMetadata;
  if not Metadata.GetDatabaseInfo.SupportsMixedCaseQuotedIdentifiers then
    Exit;
  Sql := 'DELETE FROM '+MetaData.GetIdentifierConvertor.Quote('Case_Sensitive')+' where cs_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);

  Sql := 'SELECT * FROM '+MetaData.GetIdentifierConvertor.Quote('Case_Sensitive')+' WHERE cs_id = ?';

  { Inserts row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateInt(cs_id_Index, TEST_ROW_ID);
    UpdateInt(field1_Index, 10);
    UpdateInt(field2_Index, 11);
    UpdateNull(field3_Index);
    InsertRow;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks inserted row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(10, GetIntByName('Cs_Data1'));
    CheckEquals(11, GetIntByName('cs_data1'));
    CheckEquals(True, IsNullByName('cs data1'));

    UpdateInt(field1_Index, 101);
    UpdateNullByName('cs_data1');
    UpdateIntByName('cs data1', 12);
    ResultSet.UpdateRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks updated row from "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(101, GetIntByName('Cs_Data1'));
    CheckEquals(True, IsNullByName('cs_data1'));
    CheckEquals(12, GetIntByName('cs data1'));
    DeleteRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Deletes inserted,updated row in "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
  Statement := nil;

  Sql := 'SELECT * FROM case_sensitive WHERE cs_id = ?';

  { Inserts row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  CheckNotNull(Statement);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateInt(cs_id_Index, TEST_ROW_ID);
    UpdateNull(field1_Index);
    UpdateInt(field2_Index, 21);
    UpdateInt(field3_Index, 22);
    InsertRow;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks inserted row to "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(True, IsNullByName('CS_DATA1'));
    CheckEquals(21, GetIntByName('CS_Data2'));
    CheckEquals(22, GetIntByName('Cs_Data3'), 0);

    UpdateInt(field1_Index, 20);
    UpdateIntByName('CS_Data2', 212);
    UpdateNullByName('Cs_Data3');
    ResultSet.UpdateRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Checks updated row from "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('cs_id'));
    CheckEquals(20, GetIntByName('CS_DATA1'));
    CheckEquals(212, GetIntByName('CS_Data2'));
    CheckEquals(True, IsNullByName('Cs_Data3'));
    DeleteRow;
    Close;
  end;
  ResultSet := nil;
  Statement := nil;

  { Deletes inserted,updated row in "Case_Sensitive" table }
  Statement := Connection.PrepareStatement(Sql);
  Statement.SetResultSetConcurrency(rcUpdatable);
  CheckNotNull(Statement);
  Statement.SetInt(cs_id_Index, TEST_ROW_ID);
  ResultSet := Statement.ExecuteQueryPrepared;
  with ResultSet do
  begin
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
  Statement := nil;
end;

{**
  Tests the DBC connection.
}
procedure TZGenericTestDbcResultSet.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
//  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  Connection.SetAutoCommit(False);
  if Connection.GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(tiNone)
  then CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation))
  else CheckEquals(Ord(Connection.GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation), Ord(Connection.GetTransactionIsolation));

  { Checks without transactions. }
  CheckNotNull(Connection.CreateStatement);
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  CheckNotNull(Connection.CreateStatement);
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

{**
  Checks functionality prepared statement
}
procedure TZGenericTestDbcResultSet.TestPreparedStatement;
const
  Insert_eq_id_Index        = FirstDbcIndex + 0;
  Insert_eq_name_Index      = FirstDbcIndex + 1;
  Insert_eq_type_Index      = FirstDbcIndex + 2;
  Insert_eq_cost_Index      = FirstDbcIndex + 3;
  Insert_eq_date_Index      = FirstDbcIndex + 4;
  Insert_woff_date_Index    = FirstDbcIndex + 5;
  Select_eq_id_Index        = FirstDbcIndex + 0;
  Delete_eq_id_Index        = FirstDbcIndex + 0;
  Inserted_eq_name_Index    = FirstDbcIndex + 0;
  Inserted_eq_id_Index      = FirstDbcIndex + 1;

  Insert_p_id_Index         = FirstDbcIndex + 0;
  Insert_p_dep_id_Index     = FirstDbcIndex + 1;
  Insert_p_name_Index       = FirstDbcIndex + 2;
  Insert_p_begin_work_Index = FirstDbcIndex + 3;
  Insert_p_end_work_Index   = FirstDbcIndex + 4;
  Insert_p_picture_Index    = FirstDbcIndex + 5;
  Insert_p_resume_Index     = FirstDbcIndex + 6;
  Insert_p_redundant_Index  = FirstDbcIndex + 7;
  Select_p_id_Index         = FirstDbcIndex + 0;
  Delete_p_id_Index         = FirstDbcIndex + 0;
var
  Sql: string;
  Statement: IZPreparedStatement;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
  ResultSet: IZResultSet;
begin
  Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);
  try
    { The test for equipment table }

    { Creates prepared statement for equipment table }
    Statement := Connection.PrepareStatement(
      'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
      + ' woff_date) VALUES(?,?,?,?,?,?)');
    CheckNotNull(Statement);
    with Statement do
    begin
      SetInt(Insert_eq_id_Index, TEST_ROW_ID);
      SetString(Insert_eq_name_Index, 'xyz');
      SetInt(Insert_eq_type_Index, 7);
      SetDouble(Insert_eq_cost_Index, 1234.567);
      SetDate(Insert_eq_date_Index, EncodeDate(1999, 8, 5));
      SetNull(Insert_woff_date_Index, stDate);
      CheckEquals(False, ExecutePrepared);
      CheckEquals(1, GetUpdateCount);
    end;
    Statement := nil;

    { Checks inserted row from equipment table }
    Statement := Connection.PrepareStatement(
      'SELECT * FROM equipment WHERE eq_id = ?');
    CheckNotNull(Statement);
    Statement.SetInt(Select_eq_id_Index, TEST_ROW_ID);
    ResultSet := Statement.ExecuteQueryPrepared;
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals('xyz', GetStringByName('eq_name'));
      CheckEquals(7, GetIntByName('eq_type'));
      CheckEquals(1234.567, GetDoubleByName('eq_cost'), 0.001);
      CheckEquals(EncodeDate(1999, 8, 5), GetDateByName('eq_date'));
      CheckEquals(True, IsNullByName('woff_date'));
      Close;
    end;
    ResultSet := nil;

    { Updates inserted row from equipment table }
    Statement := Connection.PrepareStatement(
      'UPDATE equipment SET eq_name = ? WHERE eq_id = ?' );
    CheckNotNull(Statement);
    with Statement do
    begin
      SetString(Inserted_eq_name_Index, 'xyz1');
      SetInt(Inserted_eq_id_Index, TEST_ROW_ID);
      CheckEquals(1, ExecuteUpdatePrepared);
    end;
    Statement := nil;

    { Deletes inserted row from equipment table }
    Statement := Connection.PrepareStatement(
      'DELETE FROM equipment WHERE eq_id = ?');
    CheckNotNull(Statement);
    with Statement do
    begin
      SetInt(Delete_eq_id_Index, TEST_ROW_ID);
      CheckEquals(False, ExecutePrepared);
      CheckEquals(1, GetUpdateCount);
    end;
    Statement := nil;

    { The test for people table }

    { Creates prepared statement for people table }
    Statement := Connection.PrepareStatement(
      'INSERT INTO people (p_id, p_dep_id, p_name, p_begin_work, p_end_work,'
      + ' p_picture, p_resume, p_redundant) VALUES(?,?,?,?,?,?,?,?)');
    CheckNotNull(Statement);
    { Sets prepared statement parameters values. }
    with Statement do
    begin
      SetInt(Insert_p_id_Index, TEST_ROW_ID);
      SetInt(Insert_p_dep_id_Index, 2);
      SetString(Insert_p_name_Index, 'xyz');
      SetTime(Insert_p_begin_work_Index, EncodeTime(8, 0, 0, 0));
      SetTime(Insert_p_end_work_Index, EncodeTime(17, 30, 0, 0));

      BinStream := TMemoryStream.Create;
      BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
      SetBinaryStream(Insert_p_picture_Index, BinStream);

      StrStream := TMemoryStream.Create;
      StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
      SetAsciiStream(Insert_p_resume_Index, StrStream);
      if ProtocolType = protPostgre then //PQExecParams can't convert str to smallint
        SetNull(Insert_p_redundant_Index, stSmall)
      else
        SetNull(Insert_p_redundant_Index, stString);
      SetNull(Insert_p_redundant_Index, stString);
      CheckEquals(False, ExecutePrepared);
      CheckEquals(1, GetUpdateCount);
    end;
    Statement := nil;

    { Checks inserted row. }
    Statement := Connection.PrepareStatement(
      'SELECT * FROM people WHERE p_id = ?');
    CheckNotNull(Statement);
    Statement.SetInt(Select_p_id_Index, TEST_ROW_ID);
    ResultSet := Statement.ExecuteQueryPrepared;
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
      CheckEquals(2, GetIntByName('p_dep_id'));
      CheckEquals('xyz', GetStringByName('p_name'));
      CheckEquals(EncodeTime(8, 0, 0, 0), GetTimeByName('p_begin_work'), 0.0001);
      CheckEquals(EncodeTime(17, 30, 0, 0), GetTimeByName('p_end_work'), 0.0001);
      CheckEquals(False, IsNullByName('p_picture'));
      CheckEquals(False, IsNullByName('p_resume'));
      CheckEquals(0, GetIntByName('p_redundant'));

      { Compares aciistream }
      StrStream1 := GetAsciiStreamByName('p_resume');
      CheckEquals(StrStream, StrStream1, 'Ascii Stream');
      StrStream.Free;
      StrStream1.Free;

      { Compares BinaryStream }
      BinStream1 := GetBinaryStreamByName('p_picture');
      CheckEquals(BinStream, BinStream1, 'Binary Stream');
      BinStream.Free;
      BinStream1.Free;
      Close;
    end;
    ResultSet := nil;


    { Deletes the row. }
    Statement := Connection.PrepareStatement(
      'DELETE FROM people WHERE p_id=?');
    CheckNotNull(Statement);
    with Statement do
    begin
      SetInt(Delete_p_id_Index, TEST_ROW_ID);
      CheckEquals(False, ExecutePrepared);
      CheckEquals(1, GetUpdateCount);
    end;
    Statement := nil;
  finally
    Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);
    Sql := 'DELETE FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
    Connection.CreateStatement.ExecuteUpdate(Sql);
  end;
end;

//see: http://zeoslib.sourceforge.net/viewtopic.php?f=38&p=95669#p95669
procedure TZGenericTestDbcResultSet.TestQuestionMarks;
var Stmt: IZPreparedStatement;
begin
  Stmt := Connection.PrepareStatement(
    '/*?? do we find these ?question-marks? as parameter ? */'+
    'select * from people where p_id > ?'+LineEnding+
    '/* ? and those marks? Are they ignored too?'+LineEnding+
    '? Are they ignored on a multi-line comment as well?*/'+LineEnding+
    '-- ? and those marks? Are they ignored too? On a single line comment?');
  Check(Stmt <> nil, 'We got a stmt');
  if ProtocolType = protADO then //ado raises nice exceptions. The ms implementation seems buggy
    Exit; //we can't help -> skip it! Except we would force the tokenizer to scip the comments!
  Stmt.SetInt(FirstDbcIndex, 1);
  with stmt.ExecuteQueryPrepared do begin
    Check(Next, 'There is on row to see');
    Close;
  end;
  Stmt.Close;
end;

{**
  Checks functionality execute statement
}
procedure TZGenericTestDbcResultSet.TestStatement;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
//!! Oracle does not support such queries in ExecuteUpdate
//  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));

  Statement.ExecuteUpdate('DELETE FROM department where dep_id = ' +
    ZFastCode.IntToStr(Integer(TEST_ROW_ID)));

  { Inserts row to department table }
  Statement.Execute('INSERT INTO department VALUES (' +
    ZFastCode.IntToStr(Integer(TEST_ROW_ID)) + ',''Some agency'',''ENG'',''Some city'')');
  CheckEquals(1, Statement.GetUpdateCount);

  { Checks what row inserted }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM department where dep_id = ' +
    ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
  CheckNotNull(ResultSet);
  CheckEquals(True, ResultSet.Next);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;

  { Updates row in department table }
  Statement.ExecuteUpdate(
   'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
   ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
  { Checks what row updated }
  CheckEquals(1, Statement.GetUpdateCount);

  { Deletes value from department table }
  Statement.ExecuteUpdate('DELETE FROM department where dep_id = ' +
    ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
  CheckEquals(1, Statement.GetUpdateCount);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM department where dep_id = ' +
    ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
  CheckNotNull(ResultSet);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;
  ResultSet := nil;

  Statement.Close;
  Statement := nil;
end;

{**
  Checks the functionality ResultSet
}
procedure TZGenericTestDbcResultSet.TestStoredResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  { Creates resultset for equipment table }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment where eq_id > 100');
  CheckNotNull(ResultSet);
  CheckEquals(False, ResultSet.Next);
  ResultSet.Close;

  { Creates resultset for equipment table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment where eq_id = 1');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, GetIntByName('eq_id'));
    CheckEquals('Volvo', GetStringByName('eq_name'));
    CheckEquals(1, GetIntByName('eq_type'));
    CheckEquals(15000, GetFloatByName('eq_cost'));
    CheckEquals(EncodeDate(1998, 03, 04), GetDateByName('eq_date'));
    Check(IsNullByName('woff_date'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for people table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM people where p_id <= 2');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, GetIntByName('p_id'));
    CheckEquals(1, GetIntByName('p_dep_id'));
    CheckEquals('Vasia Pupkin', GetStringByName('p_name'));
    CheckEquals(EncodeTime(9, 0, 0, 0),
      Frac(Abs(GetTimeByName('p_begin_work'))), 0.0001);
    CheckEquals(EncodeTime(18, 0, 0, 0),
      Frac(Abs(GetTimeByName('p_end_work'))), 0.0001);
    Check(IsNullByName('p_picture'));
    Check(IsNullByName('p_resume'));
    CheckEquals(0, GetIntByName('p_redundant'));

    Check(Next);
    CheckEquals(2, GetIntByName('p_id'));
    CheckEquals(2, GetIntByName('p_dep_id'));
    CheckEquals('Andy Karto', GetStringByName('p_name'));
    CheckEquals(EncodeTime(8, 30, 0, 0),
      Frac(Abs(GetTimeByName('p_begin_work'))), 0.0001);
    CheckEquals(EncodeTime(17, 30, 0, 0),
      Frac(Abs(GetTimeByName('p_end_work'))), 0.0001);
    Check(IsNullByName('p_picture'));
    Check(IsNullByName('p_resume'));
    CheckEquals(0, GetIntByName('p_redundant'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for cargo table}
  ResultSet := Statement.ExecuteQuery('SELECT * FROM cargo where c_id = 2');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(2, GetIntByName('c_id'));
    CheckEquals(1, GetIntByName('c_dep_id'));
    CheckEquals('Paper', Trim(GetStringByName('c_name')));
    CheckEquals(2, GetIntByName('c_seal'));
    CheckEquals(EncodeDate(2002, 12, 19) + EncodeTime(14, 0, 0, 0),
      GetTimestampByName('c_date_came'), 0.0001);
    CheckEquals(EncodeDate(2002, 12, 23) + EncodeTime(0, 0, 0, 0),
      GetTimestampByName('c_date_out'), 0.0001);
    CheckEquals(1000, GetFloatByName('c_weight'));
    CheckEquals(10, GetIntByName('c_width'));
    CheckEquals(10, GetIntByName('c_height'));
    CheckEquals(986.47, GetFloatByName('c_cost'), 0.01);
    //CheckEquals('#14#17#Сорт2', GetStringByName('c_attributes'));
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for equipment table }
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    Check(Next);
    Check(Next);
    Check(Next);
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;

  { Creates resultset for equipment table with limit rows}
  Statement.SetMaxRows(2);
  ResultSet := Statement.ExecuteQuery('SELECT * FROM equipment');
  CheckNotNull(ResultSet);
  with ResultSet do
  begin
    Check(Next);
    Check(Next);
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;

  Statement.Close;
end;

procedure TZGenericTestDbcResultSet.TestStoredResultSetUpdate;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);
  Sql := 'DELETE FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);

  { Tests the equipment table }
  Sql := 'SELECT * FROM equipment where eq_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  { Inserts test record to equipment }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '1. ' + Sql);
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateIntByName('eq_id', TEST_ROW_ID);
    UpdateNullByName('eq_name');
    UpdateNullByName('eq_type');
    UpdateNullByName('eq_cost');
    UpdateNullByName('eq_date');
    UpdateNullByName('woff_date');
    InsertRow;
    Close;
  end;
  ResultSet := nil;

  { Updates row for equipment}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '2. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(TEST_ROW_ID, GetIntByName('eq_id'));
    CheckEquals(True, IsNullByName('eq_name'));
    CheckEquals(True, IsNullByName('eq_type'));
    CheckEquals(True, IsNullByName('eq_cost'));
    CheckEquals(True, IsNullByName('eq_date'));
    CheckEquals(True, IsNullByName('woff_date'));

    UpdateStringByName('eq_name', 'The some thing');
    UpdateIntByName('eq_type', 1);
    UpdateDoubleByName('eq_cost', 12345.678);
    UpdateDateByName('eq_date', EncodeDate(1989, 07, 07));
    UpdateDateByName('woff_date', EncodeDate(1998, 04, 24));
    UpdateRow;
    Close;
  end;

  { Checks previous updated row}
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '3. ' + Sql);
  with ResultSet do
  begin
    Check(Next);
    CheckEquals('The some thing', GetStringByName('eq_name'));
    CheckEquals(1, GetIntByName('eq_type'));
    CheckEquals(12345.678, GetFloatByName('eq_cost'), 0.01);
    CheckEquals(EncodeDate(1989, 07, 07), GetDateByName('eq_date'));
    CheckEquals(EncodeDate(1998, 04, 24), GetDateByName('woff_date'));
    DeleteRow;
    Close;
  end;

  { Checks what record deleted }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '4. ' + Sql);
  CheckEquals(False, ResultSet.Next);


  { Tests the people table }
  Sql := 'DELETE FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Statement.ExecuteUpdate(Sql);

  Sql := 'SELECT * FROM people where p_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  StrStream := TMemoryStream.Create;
  StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
  BinStream := TMemoryStream.Create;
  BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
  StrStream1 := nil;
  BinStream1 := nil;
  try
    { Inserts test record to people table }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '1. ' + Sql);
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateIntByName('p_id', TEST_ROW_ID);
      UpdateNullByName('p_dep_id');
      UpdateNullByName('p_name');
      UpdateNullByName('p_begin_work');
      UpdateNullByName('p_end_work');
      UpdateNullByName('p_resume');
      UpdateNullByName('p_picture');
      UpdateNullByName('p_redundant');
      InsertRow;
      Close;
    end;

     { Checks the previous inserted record }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '2. ' + Sql);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
      CheckEquals(True, IsNullByName('p_dep_id'));
      CheckEquals(True, IsNullByName('p_name'));
      CheckEquals(True, IsNullByName('p_begin_work'));
      CheckEquals(True, IsNullByName('p_end_work'));
      CheckEquals(True, IsNullByName('p_resume'));
      CheckEquals(True, IsNullByName('p_picture'));
      CheckEquals(True, IsNullByName('p_redundant'));

      CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
      CheckEquals(True, IsNullByName('p_dep_id'));
      CheckEquals(True, IsNullByName('p_name'));
      CheckEquals(True, IsNullByName('p_begin_work'));
      CheckEquals(True, IsNullByName('p_end_work'));
      CheckEquals(True, IsNullByName('p_resume'));
      CheckEquals(True, IsNullByName('p_picture'));
      CheckEquals(True, IsNullByName('p_redundant'));
      Close;
    end;

    { Creates and update resultset for people table for p_id = TEST_ROW_ID }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '3. ' + Sql);
    with ResultSet do
    begin
      Check(Next);
      UpdateIntByName('p_dep_id', 1);
      UpdateStringByName('p_name', 'Somebody');
      UpdateTimeByName('p_begin_work', EncodeTime(12, 11, 20, 0));
      UpdateTimeByName('p_end_work', EncodeTime(22, 36, 55, 0));
      UpdateAsciiStreamByName('p_resume', StrStream);
      UpdateBinaryStreamByName('p_picture', BinStream);
      UpdateIntByName('p_redundant', 1);
      UpdateRow;
      Close;
    end;

    { Creates and updates resultset for people table for p_id = TEST_ROW_ID }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    PrintResultSet(ResultSet, True, '4. ' + Sql);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(1, GetIntByName('p_dep_id'));
      CheckEquals('Somebody', GetStringByName('p_name'));
      CheckEquals(EncodeTime(12, 11, 20, 0), GetTimeByName('p_begin_work'), 0.0001);
      CheckEquals(EncodeTime(22, 36, 55, 0), GetTimeByName('p_end_work'), 0.0001);
      BinStream1 := GetBinaryStreamByName('p_picture');
      StrStream1 := GetAsciiStreamByName('p_resume');
      CheckEquals(BinStream, BinStream1);
      CheckEquals(StrStream, StrStream1);
      CheckEquals(1, GetIntByName('p_redundant'));
      DeleteRow;
    end;
  finally
    FreeAndNil(BinStream);
    FreeAndNil(BinStream1);
    FreeAndNil(StrStream);
    FreeAndNil(StrStream1);
  end;


  { Creates and updates resultset for equipment table for eq_id = TEST_ROW_ID }
  ResultSet := Statement.ExecuteQuery(Sql);
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True, '5. ' + Sql);
  CheckEquals(False, ResultSet.Next);
end;

{**
  Tests memory issues in Execute and GetLastQuery.
}
procedure TZGenericTestDbcResultSet.TestLastQuery;
var
  SQL: string;
  Statement: IZPreparedStatement;
  Statement1: IZStatement;
  ResultSet: IZResultSet;
begin
  SQL := 'UPDATE people SET p_id=p_id WHERE 1=0';
  Statement := Connection.PrepareStatement(SQL);
  try
    CheckNotNull(Statement);
    Check(not Statement.ExecutePrepared);
  finally
    Statement.Close;
  end;

  SQL := 'SELECT * FROM people';

  Statement1 := Connection.CreateStatement;
  try
    CheckNotNull(Statement1);
    Statement1.SetResultSetType(rtScrollInsensitive);
    Statement1.SetResultSetConcurrency(rcUpdatable);
    Check(Statement1.Execute(SQL));

    ResultSet := Statement1.GetResultSet;
    try
      ResultSet.BeforeFirst;
      ResultSet := nil;
      Statement1.GetConnection;

      ResultSet := Statement1.GetResultSet;
      ResultSet.BeforeFirst;
    finally
      ResultSet.Close;
    end;
  finally
    Statement1.Close;
  end;

  Statement := Connection.PrepareStatement(SQL);
  try
    CheckNotNull(Statement);
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcUpdatable);
    Check(Statement.ExecutePrepared);

    ResultSet := Statement.GetResultSet;
    try
      ResultSet.BeforeFirst;
      ResultSet := nil;
      Statement.GetConnection;

      ResultSet := Statement.GetResultSet;
      ResultSet.BeforeFirst;
    finally
      ResultSet.Close;
    end;
  finally
    Statement.Close;
  end;
end;

procedure TZGenericTestDbcResultSet.TestNotNullValues;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if ProtocolType = protOracle then Exit; //oracle doesnt allow '' values for not null columns

  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM not_null_values where n_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  Connection.CreateStatement.ExecuteUpdate(Sql);

  { Tests the equipment table }
  Sql := 'SELECT * FROM not_null_values where n_id = ' + ZFastCode.IntToStr(Integer(TEST_ROW_ID));
  { Inserts test record to equipment }
  try
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateIntByName('n_id', TEST_ROW_ID);
      UpdateStringByName('n_varchar', ''); //test empty string
      try
        InsertRow;
      except
        ResultSet := nil;
        Statement.Close;
        raise;
      end;
    end;
    ResultSet := Statement.ExecuteQuery(Sql); //load values from db
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      try
        Check(Next);
        CheckEquals(TEST_ROW_ID, GetIntByName('n_id'));
        CheckEquals(False, IsNullByName('n_varchar'));
        CheckEquals('', GetStringByName('n_varchar'));
      finally
        //do nothing here
        ResultSet := nil;
      end;
    end;
  finally
    if Assigned(Statement) then
      Statement.Close;
  end;
end;

procedure TZGenericTestDbcResultSet.TestCommitBehavior;
var Stmt: IZStatement;
  I: Integer;
begin
  CheckEquals(False, Connection.IsReadOnly);
  CheckEquals(True, Connection.GetAutoCommit);
  if Connection.GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(tiNone)
  then CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation))
  else CheckEquals(Ord(Connection.GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation), Ord(Connection.GetTransactionIsolation));

  Stmt := Connection.CreateStatement;
  CheckNotNull(Stmt);
  CheckEquals(False, Connection.IsClosed);
  for i := 0 to 10 do begin
    Stmt.ExecuteUpdate('insert into people(p_id, p_name) values (1000, ''miab3'')');
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(not Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.Close;
  end;

  Connection.SetAutoCommit(False);
  { Checks without transactions. }
  Stmt := Connection.CreateStatement;
  CheckNotNull(Stmt);
  CheckEquals(False, Connection.IsClosed);
  for i := 0 to 10 do begin
    Stmt.ExecuteUpdate('insert into people(p_id, p_name) values (1000, ''miab3'')');
    Connection.Commit;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    Connection.Rollback;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong rollback behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    Connection.Commit;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(not Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.Close;
  end;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.SetAutoCommit(True);
  Stmt := Connection.CreateStatement;
  CheckNotNull(Stmt);
  CheckEquals(False, Connection.IsClosed);
  for i := 0 to 10 do begin
    Stmt.ExecuteUpdate('insert into people(p_id, p_name) values (1000, ''miab3'')');
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(not Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.Close;
  end;
  for i := 0 to 100 do
    Connection.SetAutoCommit(i mod 2 = 0);
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  Connection.SetAutoCommit(False);
  { Checks without transactions. }
  Stmt := Connection.CreateStatement;
  CheckNotNull(Stmt);
  CheckEquals(False, Connection.IsClosed);
  for i := 0 to 10 do begin
    Stmt.ExecuteUpdate('insert into people(p_id, p_name) values (1000, ''miab3'')');
    Connection.Commit;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    Connection.Rollback;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(Next, 'wrong rollback behavior');
      Close;
    end;
    Stmt.ExecuteUpdate('delete from people where p_id = 1000');
    Connection.Commit;
    with Stmt.ExecuteQuery('select * from people where p_id = 1000') do begin
      Check(not Next, 'wrong commit behavior');
      Close;
    end;
    Stmt.Close;
  end;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

procedure TZGenericTestDbcResultSet.TestConcurrency;
var
  Statement: IZStatement;
  Statement2: IZStatement;
  ResultSet1: IZResultSet;
  ResultSet2: IZResultSet;
  ResultSet3: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  Statement2 := Connection.CreateStatement;
  CheckNotNull(Statement);

  try
    ResultSet1 := Statement.ExecuteQuery('select * from people');
    ResultSet2 := Statement.ExecuteQuery('select * from equipment');
    ResultSet3 := Statement2.ExecuteQuery('select * from people');
    try
      Check(not ResultSet1.Next, 'Resultset 1 should be closed');
      Check(ResultSet2.Next);
      Check(ResultSet3.Next);
      Check(ResultSet2.Next);
      Check(ResultSet3.Next);
      Check(ResultSet2.Next);
      Check(ResultSet3.Next);
    finally
      ResultSet1.Close;
      ResultSet2.Close;
      ResultSet3.Close;
    end;
  finally
    Statement.Close;
    Statement2.Close;
  end;
end;

procedure TZGenericTestDbcResultSet.TestStringGetter;
const
  p_name_Index = FirstDbcIndex + 2;
  SNames: array[0..4] of string =
    ('Vasia Pupkin', 'Andy Karto', 'Kristen Sato', 'Aleksey Petrov', 'Yan Pater');
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Len, name: NativeUInt;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  try
    ResultSet := Statement.ExecuteQuery('select * from people');
    try
      for name := Low(SNames) to High(SNames) do
      begin
        Check(ResultSet.Next);
        CheckEquals(SNames[name], ResultSet.GetString(p_name_Index));
        CheckEquals(AnsiString(SNames[name]), ResultSet.GetAnsiString(p_name_Index));
        CheckEquals(UTF8String(SNames[name]), ResultSet.GetUTF8String(p_name_Index));
        CheckEquals(RawByteString(SNames[name]), ResultSet.GetRawByteString(p_name_Index));
        CheckEquals(ZWideString(SNames[name]), ResultSet.GetUnicodeString(p_name_Index));
        CheckEquals(PAnsiChar(AnsiString(SNames[name])), ResultSet.GetPAnsiChar(p_name_Index));
        CheckEquals(PAnsiChar(RawByteString(SNames[name])), ResultSet.GetPAnsiChar(p_name_Index, Len), @Len);
        CheckEquals(PChar(SNames[name]), ResultSet.GetPChar(p_name_Index));
        CheckEquals(ZWideString(SNames[name]), ZWideString(ResultSet.GetPWideChar(p_name_Index)));
        CheckEquals(ZWideString(SNames[name]), ZWideString(ResultSet.GetPWideChar(p_name_Index, Len)));
      end;
    finally
      ResultSet.Close;
    end;
  finally
    Statement.Close;
  end;
end;

procedure TZGenericTestDbcResultSet.TestStringToSignedIntegerConversions;
const
  s_id_Index       = FirstDbcIndex + 0;
  s_char_Index     = FirstDbcIndex + 1;
  s_varchar_Index  = FirstDbcIndex + 2;
  s_nchar_Index    = FirstDbcIndex + 3;
  s_nvarchar_Index = FirstDbcIndex + 4;
  s_bit_Index      = FirstDbcIndex + 5;
  //s_varbit_Index  = FirstDbcIndex + 6;
var
  PStatement: IZPreparedStatement;
  Use_S_BIT: Boolean;

  function InsertTestString(ID: Integer; Const Str: RawByteString): Boolean;
  begin
    PStatement.SetInt(s_id_Index, ID);
    PStatement.SetRawByteString(s_char_Index, Str);
    PStatement.SetRawByteString(s_varchar_Index, Str);
    PStatement.SetRawByteString(s_nchar_Index, Str);
    PStatement.SetRawByteString(s_nvarchar_Index, Str);
    if Use_S_BIT then
      PStatement.SetRawByteString(s_bit_Index, Str);
    Result := PStatement.ExecuteUpdatePrepared = 1;
  end;
begin
  Use_S_BIT := Not (
    ProtocolType in [protSQLite, protADO, protMSSQL, protSyBase, protFreeTDS, protASA]
  );
  if Use_S_BIT then
    PStatement := Connection.PrepareStatement('insert into string_values(s_id,s_char,s_varchar,s_nchar,s_nvarchar,s_bit) values (?, ?, ?, ?, ?, ?)')
  else
    PStatement := Connection.PrepareStatement('insert into string_values(s_id,s_char,s_varchar,s_nchar,s_nvarchar) values (?, ?, ?, ?, ?)');
  CheckNotNull(PStatement);
  {Insert ShortInt test values}
  Check(InsertTestString(TEST_ROW_ID, IntToRaw(Low(ShortInt))));
  Check(InsertTestString(TEST_ROW_ID+1, IntToRaw(High(ShortInt))));
  {Insert SmallInt test values}
  Check(InsertTestString(TEST_ROW_ID+2, IntToRaw(Low(SmallInt))));
  Check(InsertTestString(TEST_ROW_ID+3, IntToRaw(High(SmallInt))));
  {Insert Integer test values}
  Check(InsertTestString(TEST_ROW_ID+4, IntToRaw(Low(Integer))));
  Check(InsertTestString(TEST_ROW_ID+5, IntToRaw(High(Integer))));
  {Insert Int64 test values}
  Check(InsertTestString(TEST_ROW_ID+6, IntToRaw(Low(Int64))));
  Check(InsertTestString(TEST_ROW_ID+7, IntToRaw(High(Int64))));

  PStatement := Connection.PrepareStatement('select * from string_values where s_id >= ? order by s_id');
  try
    PStatement.SetInt(FirstDbcIndex, TEST_ROW_ID);
    with PStatement.ExecuteQueryPrepared do
    begin
      { Test ShortInt getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetInt(s_id_Index));
      CheckEquals(Low(ShortInt), GetShort(s_char_Index));
      CheckEquals(Low(ShortInt), GetShort(s_varchar_Index));
      CheckEquals(Low(ShortInt), GetShort(s_nchar_Index));
      CheckEquals(Low(ShortInt), GetShort(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(ShortInt), GetShort(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+1, GetInt(s_id_Index));
      CheckEquals(High(ShortInt), GetShort(s_char_Index));
      CheckEquals(High(ShortInt), GetShort(s_varchar_Index));
      CheckEquals(High(ShortInt), GetShort(s_nchar_Index));
      CheckEquals(High(ShortInt), GetShort(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(ShortInt), GetShort(s_bit_Index));
      { Test SmallInt getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+2, GetInt(s_id_Index));
      CheckEquals(Low(SmallInt), GetSmall(s_char_Index));
      CheckEquals(Low(SmallInt), GetSmall(s_varchar_Index));
      CheckEquals(Low(SmallInt), GetSmall(s_nchar_Index));
      CheckEquals(Low(SmallInt), GetSmall(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(SmallInt), GetSmall(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+3, GetInt(s_id_Index));
      CheckEquals(High(SmallInt), GetSmall(s_char_Index));
      CheckEquals(High(SmallInt), GetSmall(s_varchar_Index));
      CheckEquals(High(SmallInt), GetSmall(s_nchar_Index));
      CheckEquals(High(SmallInt), GetSmall(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(SmallInt), GetSmall(s_bit_Index));
      { Test Integer getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+4, GetInt(s_id_Index));
      CheckEquals(Low(Integer), GetInt(s_char_Index));
      CheckEquals(Low(Integer), GetInt(s_varchar_Index));
      CheckEquals(Low(Integer), GetInt(s_nchar_Index));
      CheckEquals(Low(Integer), GetInt(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(Integer), GetInt(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+5, GetInt(s_id_Index));
      CheckEquals(High(Integer), GetInt(s_char_Index));
      CheckEquals(High(Integer), GetInt(s_varchar_Index));
      CheckEquals(High(Integer), GetInt(s_nchar_Index));
      CheckEquals(High(Integer), GetInt(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(Integer), GetInt(s_bit_Index));
      { Test Int64 getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+6, GetInt(s_id_Index));
      CheckEquals(Low(Int64), GetLong(s_char_Index));
      CheckEquals(Low(Int64), GetLong(s_varchar_Index));
      CheckEquals(Low(Int64), GetLong(s_nchar_Index));
      CheckEquals(Low(Int64), GetLong(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(Int64), GetLong(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+7, GetInt(s_id_Index));
      CheckEquals(High(Int64), GetLong(s_char_Index));
      CheckEquals(High(Int64), GetLong(s_varchar_Index));
      CheckEquals(High(Int64), GetLong(s_nchar_Index));
      CheckEquals(High(Int64), GetLong(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(Int64), GetLong(s_bit_Index));
      Close;
    end;
  finally
    Connection.CreateStatement.ExecuteUpdate('delete from string_values where s_id >= '+ ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
  end;
end;

procedure TZGenericTestDbcResultSet.TestStringToUnsignedIntegerConversions;
const
  s_id_Index       = FirstDbcIndex + 0;
  s_char_Index     = FirstDbcIndex + 1;
  s_varchar_Index  = FirstDbcIndex + 2;
  s_nchar_Index    = FirstDbcIndex + 3;
  s_nvarchar_Index = FirstDbcIndex + 4;
  s_bit_Index      = FirstDbcIndex + 5;
  //s_varbit_Index  = FirstDbcIndex + 6;
var
  PStatement: IZPreparedStatement;
  Info: TStrings;
  Use_S_BIT: Boolean;

  function InsertTestString(ID: Integer; Const Str: RawByteString): Boolean;
  begin
    PStatement.SetInt(s_id_Index, ID);
    PStatement.SetRawByteString(s_char_Index, Str);
    PStatement.SetRawByteString(s_varchar_Index, Str);
    PStatement.SetRawByteString(s_nchar_Index, Str);
    PStatement.SetRawByteString(s_nvarchar_Index, Str);
    if Use_S_BIT then
      PStatement.SetRawByteString(s_bit_Index, Str);
    Result := PStatement.ExecuteUpdatePrepared = 1;
  end;
begin
  Info := TStringList.Create;
  Info.Add('preferprepared=True');
  Use_S_BIT := Not (
    ProtocolType in [protSQLite, protADO, protMSSQL, protSyBase, protFreeTDS, protASA]
  );
  if Use_S_BIT then
    PStatement := Connection.PrepareStatement('insert into string_values(s_id,s_char,s_varchar,s_nchar,s_nvarchar,s_bit) values (?, ?, ?, ?, ?, ?)')
  else
    PStatement := Connection.PrepareStatement('insert into string_values(s_id,s_char,s_varchar,s_nchar,s_nvarchar) values (?, ?, ?, ?, ?)');
  CheckNotNull(PStatement);
  {Insert ShortInt test values}
  Check(InsertTestString(TEST_ROW_ID, IntToRaw(Low(Byte))));
  Check(InsertTestString(TEST_ROW_ID+1, IntToRaw(High(Byte))));
  {Insert SmallInt test values}
  Check(InsertTestString(TEST_ROW_ID+2, IntToRaw(Low(Word))));
  Check(InsertTestString(TEST_ROW_ID+3, IntToRaw(High(Word))));
  {Insert Integer test values}
  Check(InsertTestString(TEST_ROW_ID+4, IntToRaw(Low(LongWord))));
  Check(InsertTestString(TEST_ROW_ID+5, IntToRaw(High(LongWord))));
  {Insert Int64 test values}
  Check(InsertTestString(TEST_ROW_ID+6, IntToRaw(Low(UInt64))));
  {$IFDEF WITH_UINT64_C1118_ERROR}
  Check(InsertTestString(TEST_ROW_ID+7, IntToRaw(UInt64($FFFFFFFFFFFFFFFF)))); //D7 returns -1 als High value
  {$ELSE}
  Check(InsertTestString(TEST_ROW_ID+7, IntToRaw(High(UInt64))));
  {$ENDIF}

  PStatement := Connection.PrepareStatementWithParams('select * from string_values where s_id >= ? order by s_id', Info);
  try
    PStatement.SetInt(FirstDbcIndex, TEST_ROW_ID);
    with PStatement.ExecuteQueryPrepared do
    begin
      { Test Byte getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetInt(s_id_Index));
      CheckEquals(Low(Byte), GetByte(s_char_Index));
      CheckEquals(Low(Byte), GetByte(s_varchar_Index));
      CheckEquals(Low(Byte), GetByte(s_nchar_Index));
      CheckEquals(Low(Byte), GetByte(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(Byte), GetByte(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+1, GetInt(s_id_Index));
      CheckEquals(High(Byte), GetByte(s_char_Index));
      CheckEquals(High(Byte), GetByte(s_varchar_Index));
      CheckEquals(High(Byte), GetByte(s_nchar_Index));
      CheckEquals(High(Byte), GetByte(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(Byte), GetByte(s_bit_Index));
      { Test Word getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+2, GetInt(s_id_Index));
      CheckEquals(Low(Word), GetWord(s_char_Index));
      CheckEquals(Low(Word), GetWord(s_varchar_Index));
      CheckEquals(Low(Word), GetWord(s_nchar_Index));
      CheckEquals(Low(Word), GetWord(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(Word), GetWord(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+3, GetInt(s_id_Index));
      CheckEquals(High(Word), GetWord(s_char_Index));
      CheckEquals(High(Word), GetWord(s_varchar_Index));
      CheckEquals(High(Word), GetWord(s_nchar_Index));
      CheckEquals(High(Word), GetWord(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(Word), GetWord(s_bit_Index));
      { Test Longword/Cardinal getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+4, GetInt(s_id_Index));
      CheckEquals(Low(LongWord), GetUInt(s_char_Index));
      CheckEquals(Low(LongWord), GetUInt(s_varchar_Index));
      CheckEquals(Low(LongWord), GetUInt(s_nchar_Index));
      CheckEquals(Low(LongWord), GetUInt(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(LongWord), GetUInt(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+5, GetInt(s_id_Index));
      CheckEquals(High(LongWord), GetUInt(s_char_Index));
      CheckEquals(High(LongWord), GetUInt(s_varchar_Index));
      CheckEquals(High(LongWord), GetUInt(s_nchar_Index));
      CheckEquals(High(LongWord), GetUInt(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(LongWord), GetUInt(s_bit_Index));
      { Test UInt64 getter}
      Check(Next);
      CheckEquals(TEST_ROW_ID+6, GetInt(s_id_Index));
      CheckEquals(Low(UInt64), GetULong(s_char_Index));
      CheckEquals(Low(UInt64), GetULong(s_varchar_Index));
      CheckEquals(Low(UInt64), GetULong(s_nchar_Index));
      CheckEquals(Low(UInt64), GetULong(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(Low(UInt64), GetULong(s_bit_Index));
      Check(Next);
      CheckEquals(TEST_ROW_ID+7, GetInt(s_id_Index));
      CheckEquals(High(UInt64), GetULong(s_char_Index));
      CheckEquals(High(UInt64), GetULong(s_varchar_Index));
      CheckEquals(High(UInt64), GetULong(s_nchar_Index));
      CheckEquals(High(UInt64), GetULong(s_nvarchar_Index));
      if Use_S_BIT then
        CheckEquals(High(UInt64), GetULong(s_bit_Index));
      Close;
    end;
  finally
    Connection.CreateStatementWithParams(Info).ExecuteUpdate('delete from string_values where s_id >= '+ ZFastCode.IntToStr(Integer(TEST_ROW_ID)));
    FreeAndNil(Info);
  end;
end;

initialization
  RegisterTest('dbc',TZGenericTestDbcResultSet.Suite);
end.

