{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for Interbase Database Connectivity Classes   }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZTestDbcInterbase;

interface

{$I ZDbc.inc}

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcInterbase6, ZSqlTestCase,
  ZCompatibility, DateUtils;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcInterbaseCase = class(TZAbstractDbcSQLTestCase)
  protected
//    function GetSupportedProtocols: string; override;
    function SupportsConfig(Config: TZConnectionConfig): Boolean; override;
  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestRegularResultSet;
    procedure TestBlobs;
    procedure TestUpdateBlobs;
    procedure TestCaseSensitive;
    procedure TestDefaultValues;
    procedure TestDomainValues;
    procedure TestStoredprocedures;
    procedure TestMsec;
    procedure TestEmptyStrings;
    procedure TestInsertReturning;
    procedure TestClientVersionNumber;
    procedure TestDefaultReadCommittedMode;
    procedure FB_TestUpdateCounts;
    procedure FB_TestUpdateCounts_Returning;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_A;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_B;
    procedure FB_TestUpdateCounts_FromSuspendedProcedure_C;
    procedure TestLongStatements;
  end;

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

uses SysUtils, ZTestConsts, ZTestCase, ZVariant, ZMessages,
  ZDbcInterbaseFirebirdMetadata, ZDbcFirebirdInterbase;

{ TZTestDbcInterbaseCase }

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_A;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_A');
    //Check(Stmt.GetUpdateCount = 0, 'updatecount is not equal');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_B;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_B');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    //Check(Stmt.GetUpdateCount = Cnt, 'updatecount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_FromSuspendedProcedure_C;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);
    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    RS := Stmt.ExecuteQuery('select r1 from procedure_upd_people_B');
//    Check(Stmt.GetUpdateCount = Cnt, 'updatecount is not equal');
    I := 0;
    while RS.Next do
      Inc(I);
    Check(I = Cnt, 'rowcount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts_Returning;
var Stmt: IZStatement;
  Cnt: Integer;
  DbInfo: IZInterbaseDatabaseInfo;
begin
  Supports(Connection.GetMetadata.GetDatabaseInfo, IZInterbaseDatabaseInfo, DbInfo);

  if Assigned(DbInfo) and DbInfo.HostIsFireBird and (DbInfo.GetHostVersion >= 2000000) then begin
    Stmt := Connection.CreateStatement;
    try
      CheckNotNull(Stmt);
      Cnt := Stmt.ExecuteUpdate('update people set p_id = p_id where p_id = 5 returning p_id'); //fb does not support multiple rows yet
      Check(Cnt = 1, 'updatecount is not equal');
      Stmt.Close;
    finally
      Stmt.Close;
      Stmt := nil;
    end;
  end;
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
//function TZTestDbcInterbaseCase.GetSupportedProtocols: string;
//begin
//  Result := pl_all_interbase;
//end;

function TZTestDbcInterbaseCase.SupportsConfig(Config: TZConnectionConfig): Boolean;
begin
  Result := Config.Provider = spIB_FB;
end;

procedure TZTestDbcInterbaseCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiNone), Ord(Connection.GetTransactionIsolation));

  if ConnectionConfig.Transport = traNative then
    CheckEquals(3, (Connection as IZInterbaseFirebirdConnection).GetDialect);

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(SInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiSerializable);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(SInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.SetAutoCommit(False);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.CreateStatement;
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.Commit;
  Connection.Rollback;
  Connection.SetAutoCommit(True);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  try
    Connection.Commit;
    Connection.Rollback;
    Fail(SInvalidOpInAutoCommit);
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  Connection.SetTransactionIsolation(tiReadCommitted);
  Check(not Connection.IsClosed, 'Connection should not be closed');
  Connection.CreateStatement;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

procedure TZTestDbcInterbaseCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
  Statement.close;
end;

procedure TZTestDbcInterbaseCase.TestRegularResultSet;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM DEPARTMENT');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
end;

procedure TZTestDbcInterbaseCase.TestBlobs;
const
  B_ID_Index = FirstDbcIndex;
  B_TEXT_Index = FirstDbcIndex+1;
  B_IMAGE_Index = FirstDbcIndex+2;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := CreateDbcConnection;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM BLOB_VALUES WHERE B_ID='
    + IntToStr(TEST_ROW_ID));

  TempStream := nil;
  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/zapotec.bmp'));
  try
    PreparedStatement := Connection.PrepareStatement(
      'INSERT INTO BLOB_VALUES (B_ID, B_TEXT, B_IMAGE) VALUES(?,?,?)');
    PreparedStatement.SetInt(B_ID_Index, TEST_ROW_ID);
    PreparedStatement.SetAsciiStream(B_TEXT_Index, TextStream);
    PreparedStatement.SetBinaryStream(B_IMAGE_Index, ImageStream);
    CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
      + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);
    CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
    TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
    CheckEquals(TextStream, TempStream);
    TempStream.Free;
    TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
    CheckEquals(ImageStream, TempStream);
  finally
    FreeAndNil(TempStream);
    ResultSet.Close;

    TextStream.Free;
    ImageStream.Free;

    Statement.Close;
  end;
end;

procedure TZTestDbcInterbaseCase.TestUpdateBlobs;
const
  insert_B_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  insert_B_TEXT_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  insert_B_IMAGE_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  update_B_ID_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  update_B_TEXT_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  update_B_IMAGE_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
begin
  Connection := CreateDbcConnection;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcReadOnly);

  Statement.ExecuteUpdate('DELETE FROM BLOB_VALUES WHERE B_ID='
    + IntToStr(TEST_ROW_ID));

  TextStream := TStringStream.Create('ABCDEFG');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/zapotec.bmp'));

  PreparedStatement := Connection.PrepareStatement(
    'INSERT INTO BLOB_VALUES (B_ID, B_TEXT, B_IMAGE) VALUES(?,?,?)');
  PreparedStatement.SetInt(insert_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(insert_B_TEXT_Index, TextStream);
  PreparedStatement.SetBinaryStream(insert_B_IMAGE_Index, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;

// Update blob
  TextStream.Free;
  ImageStream.Free;
  TextStream := TStringStream.Create('GFEDCBA');
  ImageStream := TMemoryStream.Create;
  ImageStream.LoadFromFile(TestFilePath('images/dogs.jpg'));

  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(update_B_TEXT_Index, TextStream);
  PreparedStatement.SetBinaryStream(update_B_IMAGE_Index, ImageStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);
  ResultSet.Close;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  TempStream.Free;
  TempStream := ResultSet.GetBinaryStreamByName('B_IMAGE');
  CheckEquals(ImageStream, TempStream);
  TempStream.Free;

// Update null binary blob
  TextStream.Free;
  TextStream := TStringStream.Create('GFEDCBA');

  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetAsciiStream(update_B_TEXT_Index, TextStream);
  PreparedStatement.SetNull(update_B_IMAGE_Index,stBinaryStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet.Close;
  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  TempStream := ResultSet.GetAsciiStreamByName('B_TEXT');
  CheckEquals(TextStream, TempStream);
  CheckNull(ResultSet.GetBinaryStreamByName('B_IMAGE'));
  TempStream.Free;

// Update null ascii blob

  ResultSet.Close;
  PreparedStatement := Connection.PrepareStatement(
    'UPDATE BLOB_VALUES SET B_TEXT =?,B_IMAGE=? WHERE B_ID=?');
  PreparedStatement.SetInt(update_B_ID_Index, TEST_ROW_ID);
  PreparedStatement.SetNull(update_B_TEXT_Index,stAsciiStream);
  PreparedStatement.SetNull(update_B_IMAGE_Index,stBinaryStream);
  CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES'
    + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
  CheckNotNull(ResultSet);
  Check(ResultSet.Next);
  CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('B_ID'));
  CheckNull(ResultSet.GetAsciiStreamByName('B_TEXT'));
  CheckNull(ResultSet.GetBinaryStreamByName('B_IMAGE'));

  ResultSet.Close;

  TextStream.Free;
  ImageStream.Free;

  Statement.Close;
end;

procedure TZTestDbcInterbaseCase.FB_TestUpdateCounts;
var Stmt: IZStatement;
  RS: IZResultSet;
  I, Cnt: Integer;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  try
    CheckNotNull(Stmt);

    RS := Stmt.ExecuteQuery('select count(*) from people');
    Check(Rs <> nil, 'no resultset retieved');
    Check(Rs.Next, 'no count(*) retieved');
    Cnt := Rs.GetInt(FirstDbcIndex);
    I := Stmt.ExecuteUpdate('Update people set p_id = p_id');
    Check(I = Cnt, 'updatecount is not equal');
    Stmt.Close;
  finally
    Stmt.Close;
    Stmt := nil;
    RS := nil;
  end;
end;

procedure TZTestDbcInterbaseCase.TestCaseSensitive;
const
  CS_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  Cs_Data1_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  cs_data1_Index1 = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  cs_data1_Index2 = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM "Case_Sensitive"');
  CheckNotNull(ResultSet);
  Metadata := ResultSet.GetMetadata;
  CheckNotNull(Metadata);

  CheckEquals('CS_ID', Metadata.GetColumnName(CS_ID_Index));
  CheckEquals(False, Metadata.IsCaseSensitive(CS_ID_Index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(CS_ID_Index));

  CheckEquals('Cs_Data1', Metadata.GetColumnName(Cs_Data1_Index));
  CheckEquals(True, Metadata.IsCaseSensitive(Cs_Data1_Index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(Cs_Data1_Index));

  CheckEquals('cs_data1', Metadata.GetColumnName(cs_data1_Index1));
  CheckEquals(True, Metadata.IsCaseSensitive(cs_data1_Index1));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_Index1));

  CheckEquals('cs data1', Metadata.GetColumnName(cs_data1_Index2));
  CheckEquals(True, Metadata.IsCaseSensitive(cs_data1_Index2));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_Index2));

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase default values.
}
procedure TZTestDbcInterbaseCase.TestDefaultValues;
const
  D_ID = FirstDbcIndex;
  D_FLD1 = FirstDbcIndex +1;
  D_FLD2 = FirstDbcIndex +2;
  D_FLD3 = FirstDbcIndex +3;
  D_FLD4 = FirstDbcIndex +4;
  D_FLD5 = FirstDbcIndex +5;
  D_FLD6 = FirstDbcIndex +6;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DEFAULT_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT D_ID,D_FLD1,D_FLD2,D_FLD3,D_FLD4,D_FLD5,D_FLD6 FROM DEFAULT_VALUES');
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
  Runs a test for Interbase domain fields.
}
procedure TZTestDbcInterbaseCase.TestDomainValues;
const
  D_ID = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  D_FLD1 = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  D_FLD2 = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  D_FLD3 = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.ExecuteUpdate('delete from DOMAIN_VALUES');

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 1);
  ResultSet.InsertRow;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));

  ResultSet.Close;
  ResultSet := nil;

  ResultSet := Statement.ExecuteQuery('SELECT d_id,d_fld1,d_fld2,d_fld3 FROM DOMAIN_VALUES');
  CheckNotNull(ResultSet);

  ResultSet.Next;

  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(123456, ResultSet.GetInt(D_FLD1));
  CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
  CheckEquals('xyz', ResultSet.GetString(D_FLD3));

  ResultSet.Close;
  Statement.Close;
end;

{**
  Runs a test for Interbase stored procedures.
}
procedure TZTestDbcInterbaseCase.TestStoredprocedures;
var
  ResultSet: IZResultSet;
  CallableStatement: IZCallableStatement;
  DbInfo: IZInterbaseDatabaseInfo;
begin
  // Doesn't run with ExecutePrepared. RegisterOutParameter does also not work.
  // Has to be called with an ExecuteQueryPrepared, then has to be fetched and
  // afterwards the Resultes have to be retrieved via result set columns.
  // Resultset must only have one(!) line.
  Connection.Open;
  CallableStatement := Connection.PrepareCallWithParams(
    'PROCEDURE1', nil);
  with CallableStatement do begin
    SetInt(FirstDbcIndex, 12345);
    ResultSet := ExecuteQueryPrepared;
    with ResultSet do begin
      CheckEquals(True, Next);
      CheckEquals(True, (IsFirst() and IsLast()));
      CheckEquals(12346, GetInt(FirstDbcIndex));
    end;
  end;
  CallableStatement.Close;

  // this test can only be passed by Firebird 1.5+ For Interbase and older Firebird versions we cannot detect if a procedure is selectable.
  if Supports(Connection.GetMetadata.GetDatabaseInfo, IZInterbaseDatabaseInfo, DbInfo) then begin
    if DbInfo.HostIsFireBird and (DbInfo.GetHostVersion >= 1005000) then begin
      CallableStatement := Connection.PrepareCallWithParams(
        'PROCEDURE2', nil);
      ResultSet := CallableStatement.ExecuteQueryPrepared;
      with ResultSet do begin
        CheckEquals(True, Next);
        CheckEquals('Computer', GetString(FirstDbcIndex));
        CheckEquals(True, Next);
        CheckEquals('Laboratoy', GetString(FirstDbcIndex));
        CheckEquals(True, Next);
        CheckEquals('Radiostation', GetString(FirstDbcIndex));
        CheckEquals(True, Next);
        CheckEquals('Volvo', GetString(FirstDbcIndex));
        Close;
      end;
      CallableStatement.Close;
    end;
  end;
end;

procedure TZTestDbcInterbaseCase.TestMsec;
const
  D_ID = FirstDbcIndex;
  D_DATE = FirstDbcIndex+1;
  D_TIME = FirstDbcIndex+2;
  D_DATETIME = FirstDbcIndex+3;
  D_TIMESTAMP = FirstDbcIndex+4;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  ThisTime : TDateTime;
  oldTimeFormat: string;
  TS1, TS2: TZTimeStamp;
  procedure ToTS(const InValue: TDateTime; var ToVal:TZTimeStamp);
  begin
    ToVal.Fractions := 0;
    DecodeDateTime(InValue, ToVal.Year, ToVal.Month, ToVal.Day, ToVal.Hour,
      ToVal.Minute, ToVal.Second, PWord(@ToVal.Fractions)^);
  end;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  Statement.ExecuteUpdate('delete from DATE_VALUES where D_ID=4');
  ResultSet := Statement.ExecuteQuery('select D_ID, D_DATE, D_TIME, D_DATETIME, D_TIMESTAMP from DATE_VALUES');
  CheckNotNull(ResultSet);
  OldTimeFormat := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat := 'hh:mm:ss.zzz';
  ThisTime := DateUtils.EncodeDateTime(18,8,2, 13, 13, 13, 999);
  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(D_ID, 4);
  ResultSet.UpdateDate(D_DATE,ThisTime);
  ResultSet.UpdateTime(D_TIME,ThisTime);
  ResultSet.UpdateTimestamp(D_DATETIME,ThisTime);
  ResultSet.UpdateTimestamp(D_TIMESTAMP,ThisTime);
  ResultSet.InsertRow;
//  ResultSet.Last; // why do we do this in this test?
  Check(ResultSet.GetInt(D_ID) <> 0);
  CheckEquals(Int(ThisTime), ResultSet.GetDate(D_DATE),'Failure field 2');
  ToTS(Frac(ThisTime), TS1{%H-});
  ToTS(ResultSet.GetTime(D_TIME), Ts2{%H-});
  CheckEquals(EncodeTime(ts1.Hour, ts1.Minute, ts1.Second, 0), EncodeTime(ts2.Hour, ts2.Minute, ts2.Second, 0), 'time without fractions');
  CheckEquals(ts1.Fractions, ts2.Fractions, 'fractions');
  CheckEquals(ThisTime, ResultSet.GetTimeStamp(D_DATETIME),'Failure field 4');
  CheckEquals(ThisTime, ResultSet.GetTimeStamp(D_TIMESTAMP),'Failure field 5');
  ResultSet.DeleteRow;
  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat := OldTimeFormat;
  ResultSet.Close;
  Statement.Close;
end;

// There should be no
// SQL Error:  Dynamic SQL Error SQL error code = -804 Incorrect values within SQLDA structure.
procedure TZTestDbcInterbaseCase.TestEmptyStrings;
const
  CSQLd = 'delete from department where dep_id in (4,5)';
  CSQLi = 'insert into department (dep_id, dep_shname) values (?,?)';
  dep_id = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  dep_shname = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  PreparedStatement: IZPreparedStatement;
begin
  PreparedStatement := Connection.PrepareStatement(CSQLd);
  CheckNotNull(PreparedStatement);
  PreparedStatement.ExecutePrepared;
  PreparedStatement.Close;
  PreparedStatement := Connection.PrepareStatement(CSQLi);
  CheckNotNull(PreparedStatement);
  PreparedStatement.SetInt(dep_id, 4);
  PreparedStatement.SetString(dep_shname, '');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.SetInt(dep_id, 5);
  PreparedStatement.SetString(dep_shname, '');
  PreparedStatement.ExecuteUpdatePrepared;
  PreparedStatement.Close;
end;

procedure TZTestDbcInterbaseCase.TestInsertReturning;
const
  D_ID   = FirstDbcIndex + 0;
  D_FLD1 = FirstDbcIndex + 1;
  D_FLD2 = FirstDbcIndex + 2;
  D_FLD3 = FirstDbcIndex + 3;
  D_FLD4 = FirstDbcIndex + 4;

  procedure CheckValues(const ResultSet: IZResultSet);
  begin
    CheckEquals(1, ResultSet.GetInt(D_ID));
    CheckEquals(123456, ResultSet.GetInt(D_FLD1));
    CheckEquals(123.456, ResultSet.GetFloat(D_FLD2), 0.001);
    CheckEquals('xyz', ResultSet.GetString(D_FLD3));
    CheckEquals(EncodeDate(2003, 12, 11), ResultSet.GetDate(D_FLD4), 0);
  end;

const
  SQLDel = 'DELETE FROM DEFAULT_VALUES';
  SQLIns = 'INSERT INTO DEFAULT_VALUES(D_ID) VALUES(1) RETURNING D_ID,D_FLD1,D_FLD2,D_FLD3,D_FLD4';
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  ResCount: Integer;
  DbInfo: IZInterbaseDatabaseInfo;
begin
  Supports(Connection.GetMetadata.GetDatabaseInfo, IZInterbaseDatabaseInfo, DbInfo);

  if Assigned(DbInfo) and DbInfo.HostIsFireBird and (DbInfo.GetHostVersion >= 2000000) then begin
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcUpdatable);

    // Cleanup
    Statement.ExecuteUpdate(SQLDel);

    // Exec query
    ResultSet := Statement.ExecuteQuery(SQLIns);
    CheckNotNull(ResultSet);
    ResultSet.Next;
    CheckValues(ResultSet);
    ResultSet.Close;

    // Cleanup
    Statement.ExecuteUpdate(SQLDel);

    // Exec update
    ResCount := Statement.ExecuteUpdate(SQLIns);
    Check(ResCount > 0);
    ResultSet := Statement.GetResultSet;
    CheckNotNull(ResultSet);
    ResultSet.Next;
    CheckValues(ResultSet);
    ResultSet.Close;

    Statement.Close;
  end;
end;

procedure TZTestDbcInterbaseCase.TestClientVersionNumber;
var
  Version: Integer;
begin
  Version := Connection.GetClientVersion;

  CheckNotEquals(0, Version, 'Expected a client library version of anything but 0.');
end;

procedure TZTestDbcInterbaseCase.TestDefaultReadCommittedMode;
const
  IDX = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  IsFirebird: Boolean;
  ServerVersion: Integer;
  IsolationMode: Integer;
begin
  Connection.Close;
  Connection.SetTransactionIsolation(tiReadCommitted);
  Connection.Open;

  if ConnectionConfig.Transport <> traNative then
    Exit;

  with (Connection.GetMetadata.GetDatabaseInfo as IZInterbaseDatabaseInfo) do begin
    IsFirebird := HostIsFireBird;
    ServerVersion := GetHostVersion;
  end;

  if (not IsFirebird) or (ServerVersion < 2001000) then begin
    Check(true, 'This is a fake and cannot fail because this test can only be executed on Firebird 2.1+');
  end else begin
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement, 'Couldn''t get a valid statement.');
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcReadOnly);

    ResultSet := Statement.ExecuteQuery('select T.MON$ISOLATION_MODE from MON$TRANSACTIONS T where T.MON$TRANSACTION_ID = CURRENT_TRANSACTION');
    CheckNotNull(ResultSet);

    Check(ResultSet.Next, 'Couldn''t move to the first result row.');

    IsolationMode := ResultSet.GetInt(IDX);

    ResultSet.Close;
    Statement.Close;

    if ServerVersion >= 4000000 then
      CheckEquals(4, IsolationMode, 'Expected Isolation mode to be READ COMMITTED READ CONSISTENCY (4) but got something else.')
    else
      CheckEquals(2, IsolationMode, 'Expected Isolation mode to be READ COMMITTED RECORD VERSION (2) but got something else.');
  end;
end;


/// <summary>
///   This test tests if statements longer than 64 KB work as expected.
/// </summary>
procedure TZTestDbcInterbaseCase.TestLongStatements;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;

  SQL: String;
  Ctr: Integer;
  MinLen: Integer;
begin
  MinLen := Integer(High(Word)) + 1;
  if Connection.GetMetadata.GetDatabaseInfo.GetMaxStatementLength > MinLen then begin
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement, 'Couldn''t get a valid statement.');
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcReadOnly);

    //Build SQL:
    Ctr := FirstDbcIndex + 1;
    SQL := 'select cast(' + IntToStr(FirstDbcIndex) + ' as integer) as Field' + IntToStr(FirstDbcIndex);
    while Length(SQL) < MinLen do begin
      SQL := SQL + ', cast(' + IntToStr(Ctr) + ' as integer) as Field' + IntToStr(Ctr);
      Inc(Ctr);
    end;

    SQL := SQL + ' from RDB$DATABASE';

    ResultSet := Statement.ExecuteQuery(SQL);
    CheckNotNull(ResultSet);

    Check(ResultSet.Next, 'Couldn''t move to the first result row.');

    for Ctr := FirstDbcIndex to ResultSet.GetMetadata.GetColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      CheckEquals(Ctr, ResultSet.GetInt(Ctr), 'Expected the field to have its index number as its value.');
    end;

    ResultSet.Close;
    Statement.Close;
  end else begin
    Check(true, 'This is a fake and cannot fail because this test can only be executed on Firebird 3.0+');
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcInterbaseCase.Suite);
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
end.
