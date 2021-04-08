{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Test Case for PostgreSql Database Connectivity Classes  }
{                                                         }
{       Originally written by Sergey Seroukhov            }
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

unit ZTestDbcPostgreSql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcPostgreSql, ZSqlTestCase,
  ZCompatibility, ZDbcProperties;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcPostgreSQLCase = class(TZAbstractDbcSQLTestCase)
  private
    FEventName: String;
    procedure OnEvent(var Event: TZEventOrNotification; var StopListen: Boolean);
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestConnection;
    procedure TestStatement;
    procedure TestRegularResultSet;
    procedure TestBlobs;
    procedure TestCaseSensitive;
    procedure TestDefaultValues;
    procedure TestEnumValues;
    procedure TestGUIDs;
    procedure Test_GENERATED_ALWAYS_64;
    procedure Test_GENERATED_BY_DEFAULT_64;
    procedure Test_BatchDelete_equal_operator;
    procedure Test_BatchInsert_returning;
    procedure Test_BatchDelete_in_operator;
    procedure Test_TimezoneOffset;
    procedure Test_EventAlerter;
  end;

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
implementation
{$ENDIF ZEOS_DISABLE_POSTGRESQL}

uses Types, DateUtils,
  SysUtils, ZTestConsts, ZSysUtils, ZVariant,
  ZDbcUtils, ZDbcLogging;

{ TZTestDbcPostgreSQLCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbcPostgreSQLCase.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

procedure TZTestDbcPostgreSQLCase.TestConnection;
begin
  CheckEquals(False, Connection.IsReadOnly);
  CheckEquals(True, Connection.IsClosed);
  CheckEquals(True, Connection.GetAutoCommit);
  CheckEquals(Ord(tiReadCommitted), Ord(Connection.GetTransactionIsolation));

  CheckEquals('inet', (Connection as IZPostgreSQLConnection).
    GetTypeNameByOid(869));

  { Checks without transactions. }
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
//  Connection.Commit;
//  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);

  { Checks with transactions. }
  Connection.SetTransactionIsolation(tiSerializable);
  Connection.SetAutoCommit(false);
  Connection.CreateStatement;
  CheckEquals(False, Connection.IsClosed);
  Connection.Commit;
  Connection.Rollback;
  Connection.Close;
  CheckEquals(True, Connection.IsClosed);
end;

procedure TZTestDbcPostgreSQLCase.TestStatement;
var
  Statement: IZStatement;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  Statement.ExecuteUpdate('UPDATE equipment SET eq_name=eq_name');
  Statement.ExecuteUpdate('SELECT * FROM equipment');

  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  Check(Statement.Execute('SELECT * FROM equipment'));
  Statement.Close;
end;

procedure TZTestDbcPostgreSQLCase.Test_BatchDelete_equal_operator;
var
  Statement: IZPreparedStatement;
  IntArray: TIntegerDynArray;
begin
  IntArray := nil;
  Statement := Connection.PrepareStatement('delete from people where p_id = ?');
  CheckEquals(1, Connection.StartTransaction);
  try
    if Connection.GetHostVersion < ZSysUtils.EncodeSQLVersioning(8, 0, 0) then
      Exit;
    SetLength(IntArray, 4);
    IntArray[0] := 1;
    IntArray[1] := 2;
    IntArray[2] := 3;
    IntArray[3] := 4;
    CheckNotNull(Statement);
    Statement.SetDataArray(FirstDbcIndex, IntArray, stInteger);
    Statement.ExecuteUpdatePrepared;
  finally
    Connection.Rollback;
    Connection.Close;
  end;
end;

procedure TZTestDbcPostgreSQLCase.Test_BatchDelete_in_operator;
var
  Statement: IZPreparedStatement;
  IntArray: TIntegerDynArray;
begin
  IntArray := nil;
  Statement := Connection.PrepareStatement('delete from people where p_id in (?)');
  CheckEquals(1, Connection.StartTransaction);
  try
    if Connection.GetHostVersion < ZSysUtils.EncodeSQLVersioning(8, 0, 0) then
      Exit;
    SetLength(IntArray, 4);
    IntArray[0] := 1;
    IntArray[1] := 2;
    IntArray[2] := 3;
    IntArray[3] := 4;
    CheckNotNull(Statement);
    Statement.SetDataArray(FirstDbcIndex, IntArray, stInteger);
    Statement.ExecuteUpdatePrepared;
  finally
    Connection.Rollback;
    Connection.Close;
  end;
end;

procedure TZTestDbcPostgreSQLCase.Test_BatchInsert_returning;
var
  Statement: IZPreparedStatement;
  BoolArray: TBooleanDynArray;
  RS: IZResultSet;
  I: Cardinal;
begin
  BoolArray := nil;
  RS := nil;
  Statement := Connection.PrepareStatement('insert into high_load(stBoolean) VALUES (?) returning hl_id');
  CheckEquals(1, Connection.StartTransaction);
  try
    if Connection.GetHostVersion < ZSysUtils.EncodeSQLVersioning(8, 0, 0) then
      Exit;
    SetLength(BoolArray, 4);
    BoolArray[0] := True;
    BoolArray[1] := False;
    BoolArray[2] := True;
    BoolArray[3] := False;
    CheckNotNull(Statement);
    Statement.SetDataArray(FirstDbcIndex, BoolArray, stBoolean);
    I := 0;
    RS := Statement.ExecuteQueryPrepared;
    while RS.Next do begin
      CheckFalse(Rs.IsNull(FirstDbcIndex));
      Inc(I);
    end;
    Check(I = 4);
  finally
    Connection.Rollback;
    Connection.Close;
  end;
end;

procedure TZTestDbcPostgreSQLCase.Test_EventAlerter;
var Alerter: IZEventAlerter;
    EndTime: TDateTime;
begin
  Alerter := Connection.GetEventAlerter(OnEvent, False);
  Check(Alerter <> nil);
  FEventName := '';
  CheckFalse(Alerter.IsListening);
  Alerter.AddEvent('zeostest', nil);
  Check(Alerter.IsListening);
  EndTime := IncSecond(Now, 2);
  Connection.ExecuteImmediat('NOTIFY zeostest', lcExecute);
  while (FEventName = '') and (EndTime > Now) do begin
    //Application.ProcessMessages;
    Sleep(0);
  end;
  Check(FEventName = 'zeostest', 'Didn''t get PostgreSQL notification.');
  Alerter.ClearEvents;
  CheckFalse(Alerter.IsListening);
  Connection.CloseEventAlerter;
end;

procedure TZTestDbcPostgreSQLCase.Test_GENERATED_ALWAYS_64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  try
    CheckNotNull(Statement);
    if Connection.GetHostVersion < ZSysUtils.EncodeSQLVersioning(10, 0, 0) then
      Exit;
    ResultSet := nil;
    try
      Statement.ExecuteUpdate('drop table if exists GENERATED_ALWAYS_64');
      Statement.ExecuteUpdate('CREATE TABLE GENERATED_ALWAYS_64 ( '+
        'id BIGINT GENERATED ALWAYS AS IDENTITY NOT NULL, '+
        'value text, '+
        'CONSTRAINT pk_GENERATED_ALWAYS_BIG_64_id PRIMARY KEY(id)) '+
        'WITH (oids = false)');
      Statement.SetResultSetType(rtScrollInsensitive);
      Statement.SetResultSetConcurrency(rcUpdatable);

      ResultSet := Statement.ExecuteQuery('SELECT * FROM GENERATED_ALWAYS_64');
      try
        CheckNotNull(ResultSet);
        PrintResultSet(ResultSet, True);
        ResultSet.MoveToInsertRow;
        ResultSet.UpdateString(FirstDbcIndex+1, 'Test_GENERATED_ALWAYS_64_1');
        ResultSet.InsertRow;
        CheckFalse(ResultSet.IsNull(FirstDbcIndex), 'the generated always id should have a value');
        Check(ResultSet.GetLong(FirstDbcIndex) > 0, 'the generated always id should be greater then zero');
        ResultSet.MoveToInsertRow;
        ResultSet.UpdateString(FirstDbcIndex+1, 'Test_GENERATED_ALWAYS_64_2');
        ResultSet.InsertRow;
        CheckFalse(ResultSet.IsNull(FirstDbcIndex), 'the generated always id should have a value');
        Check(ResultSet.GetLong(FirstDbcIndex) > 0, 'the generated always id should be greater then zero');
      finally
        ResultSet.Close;
        ResultSet := nil;
      end;
    finally
      Statement.ExecuteUpdate('drop table if exists GENERATED_ALWAYS_64');
      Statement.Close;
    end;
  finally
    Connection.Close;
  end;
end;

procedure TZTestDbcPostgreSQLCase.Test_GENERATED_BY_DEFAULT_64;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  try
    CheckNotNull(Statement);
    if Connection.GetHostVersion < EncodeSQLVersioning(10, 0, 0) then
      Exit;
    ResultSet := nil;
    try
      Statement.ExecuteUpdate('drop table if exists GENERATED_BY_DEFAULT_64');
      Statement.ExecuteUpdate('CREATE TABLE GENERATED_BY_DEFAULT_64 ( '+
        'id BIGINT GENERATED BY DEFAULT AS IDENTITY (START WITH 2 INCREMENT BY 2), '+
        'value text, '+
        'CONSTRAINT pk_GENERATED_BY_DEFAULT_BIG_64_id PRIMARY KEY(id)) '+
        'WITH (oids = false)');
      Statement.SetResultSetType(rtScrollInsensitive);
      Statement.SetResultSetConcurrency(rcUpdatable);

      ResultSet := Statement.ExecuteQuery('SELECT * FROM GENERATED_BY_DEFAULT_64');
      try
        CheckNotNull(ResultSet);
        PrintResultSet(ResultSet, True);
        ResultSet.MoveToInsertRow;
        ResultSet.UpdateLong(FirstDbcIndex, 1);
        ResultSet.UpdateString(FirstDbcIndex+1, 'Test_GENERATED_BY_DEFAULT_64');
        ResultSet.InsertRow;
        CheckFalse(ResultSet.IsNull(FirstDbcIndex), 'the id should have a value');
        CheckEquals(1, ResultSet.GetLong(FirstDbcIndex), 'the id should be 1');
        ResultSet.MoveToInsertRow;
        ResultSet.UpdateString(FirstDbcIndex+1, 'Test_GENERATED_BY_DEFAULT_64_2');
        ResultSet.InsertRow;
        CheckFalse(ResultSet.IsNull(FirstDbcIndex), 'the generated by default id should have a value');
        CheckEquals(2, ResultSet.GetLong(FirstDbcIndex), 'the generated by default id should be 2');
      finally
        ResultSet.Close;
        ResultSet := nil;
      end;
    finally
      Statement.ExecuteUpdate('drop table if exists GENERATED_BY_DEFAULT_64');
      Statement.Close;
    end;
  finally
    Connection.Close;
  end;
end;

procedure TZTestDbcPostgreSQLCase.Test_TimezoneOffset;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  PGConn: IZPostgreSQLConnection;
  MyTimeZoneOffset: Double;
  Offset: Int64;
begin
  CheckEquals(S_OK, Connection.QueryInterface(IZPostgreSQLConnection, PGCOnn));

  Statement := PGCOnn.CreateStatement;
  CheckNotNull(Statement);
  if not Assigned(PGConn.GetPlainDriver.PQexecParams) or not StrToBoolEx(DefineStatementParameter(Statement, DSProps_BinaryWireResultMode, 'TRUE')) then
    Exit;
  Statement.ExecuteUpdate('SET TIME ZONE ''Europe/Rome''');
  ResultSet := Statement.ExecuteQuery('select extract(timezone from current_timestamp)');
  Check(ResultSet.Next);
  MyTimeZoneOffset := ResultSet.GetDouble(FirstDbcIndex);
  ResultSet.Close;
  Offset := Trunc(MyTimeZoneOffset);
  if PGCOnn.integer_datetimes
  then Offset := OffSet * 1000000
  else Offset := OffSet * 1000;
  CheckEquals(OffSet, PGConn.GetTimeZoneOffset);

  Statement.ExecuteUpdate('SET SESSION TIME ZONE ''America/New_York''');
  ResultSet := Statement.ExecuteQuery('select extract(timezone from current_timestamp)');
  Check(ResultSet.Next);
  MyTimeZoneOffset := ResultSet.GetDouble(FirstDbcIndex);
  ResultSet.Close;
  Offset := Trunc(MyTimeZoneOffset);
  if PGCOnn.integer_datetimes
  then Offset := OffSet * 1000000
  else Offset := OffSet * 1000;
  CheckEquals(OffSet, PGConn.GetTimeZoneOffset);

  Statement.ExecuteUpdate('SET TIME ZONE ''Asia/Tokyo''');
  ResultSet := Statement.ExecuteQuery('select extract(timezone from current_timestamp)');
  Check(ResultSet.Next);
  MyTimeZoneOffset := ResultSet.GetDouble(FirstDbcIndex);
  ResultSet.Close;
  Offset := Trunc(MyTimeZoneOffset);
  if PGCOnn.integer_datetimes
  then Offset := OffSet * 1000000
  else Offset := OffSet * 1000;
  CheckEquals(OffSet, PGConn.GetTimeZoneOffset);

  Statement.ExecuteUpdate('SET TIMEZONE=''GMT''');
  ResultSet := Statement.ExecuteQuery('select extract(timezone from current_timestamp)');
  Check(ResultSet.Next);
  MyTimeZoneOffset := ResultSet.GetDouble(FirstDbcIndex);
  ResultSet.Close;
  Offset := Trunc(MyTimeZoneOffset);
  if PGCOnn.integer_datetimes
  then Offset := OffSet * 1000000
  else Offset := OffSet * 1000;
  CheckEquals(OffSet, PGConn.GetTimeZoneOffset);

  CheckEquals(OffSet, PGConn.GetTimeZoneOffset);
  Statement.Close;
  Connection.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestRegularResultSet;
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

  ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, True);
  ResultSet.Close;

  Statement.Close;
  Connection.Close;
end;

procedure TZTestDbcPostgreSQLCase.OnEvent(var Event: TZEventOrNotification;
  var StopListen: Boolean);
begin
  FEventName := Event.Name;
end;

procedure TZTestDbcPostgreSQLCase.TestBlobs;
const
  b_id_index = FirstDbcIndex;
  b_text_index = FirstDbcIndex+1;
  b_image_index = FirstDbcIndex+2;
var
  Connection: IZConnection;
  PreparedStatement: IZPreparedStatement;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  TextStream: TStream;
  ImageStream: TMemoryStream;
  TempStream: TStream;
  Url: TZURL;
begin
  TextStream := nil;
  ImageStream := nil;
  TempStream := nil;
  Url := GetConnectionUrl(DSProps_OidAsBlob + '=' + StrTrue);
  try
    Connection := DriverManager.GetConnection(Url.URL);
    //Connection := DriverManager.GetConnectionWithLogin(
      //GetConnectionUrl + '?oidasblob=true', UserName, Password);
    Connection.SetTransactionIsolation(tiReadCommitted);
    Connection.SetAutoCommit(False);
    Statement := Connection.CreateStatement;
    CheckNotNull(Statement);
    Statement.SetResultSetType(rtScrollInsensitive);
    Statement.SetResultSetConcurrency(rcReadOnly);

    Statement.ExecuteUpdate('DELETE FROM blob_values WHERE b_id='
      + IntToStr(TEST_ROW_ID));

    TextStream := TStringStream.Create('ABCDEFG');
    ImageStream := TMemoryStream.Create;
    ImageStream.LoadFromFile(TestFilePath('images/zapotec.bmp'));

    PreparedStatement := Connection.PrepareStatement(
      'INSERT INTO blob_values (b_id,b_text,b_image) VALUES($1,$2,$3)');
    PreparedStatement.SetInt(b_id_index, TEST_ROW_ID);
    PreparedStatement.SetAsciiStream(b_text_index, TextStream);
    PreparedStatement.SetBinaryStream(b_image_index, ImageStream);
    CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared, 'UpateCount');

    ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values'
      + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next, 'ResultSet.Next');
    CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('b_id'));
    TempStream := ResultSet.GetAsciiStreamByName('b_text');
    try
      CheckEquals(TextStream, TempStream);
    finally
      FreeAndNil(TempStream);
    end;
    TempStream := ResultSet.GetBinaryStreamByName('b_image');
    try
      CheckEquals(ImageStream, TempStream);
    finally
      FreeAndNil(TempStream);
    end;
    ResultSet.Close;

    Statement.Close;
    Connection.Close;

  finally
    FreeAndNil(URL);
    FreeAndNil(TextStream);
    FreeAndNil(ImageStream);

  end;
end;

procedure TZTestDbcPostgreSQLCase.TestCaseSensitive;
const
  cs_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  Cs_Data1_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  cs_data1_2_index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  cs_data1_3_index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
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

  CheckEquals('cs_id', Metadata.GetColumnName(cs_id_index));
  CheckEquals(False, Metadata.IsCaseSensitive(cs_id_index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_id_index));

  CheckEquals('Cs_Data1', Metadata.GetColumnName(Cs_Data1_index));
  CheckEquals(True, Metadata.IsCaseSensitive(Cs_Data1_index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(Cs_Data1_index));

  CheckEquals('cs_data1', Metadata.GetColumnName(cs_data1_2_index));
  CheckEquals(False, Metadata.IsCaseSensitive(cs_data1_2_index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_2_index));

  CheckEquals('cs data1', Metadata.GetColumnName(cs_data1_3_index));
  CheckEquals(True, Metadata.IsCaseSensitive(cs_data1_3_index));
  CheckEquals('Case_Sensitive', Metadata.GetTableName(cs_data1_3_index));

  ResultSet.Close;
  Statement.Close;
  Connection.Close;
end;

{**
  Runs a test for PostgreSQL default values.
}
procedure TZTestDbcPostgreSQLCase.TestDefaultValues;
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

  ResultSet.Close;
  Statement.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestEnumValues;
const
  ext_id_index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  ext_enum_index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  // Select case
  ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1');
  CheckNotNull(ResultSet);
  ResultSet.First;
  Check(ResultSet.GetInt(ext_id_index) = 1);
  CheckEquals('Car', ResultSet.GetString(ext_enum_index));
  ResultSet.Close;
  Statement.Close;

  // Update case
  Statement.ExecuteQuery('UPDATE extension set ext_enum = ''House'' where ext_id = 1');

  ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1');
  CheckNotNull(ResultSet);
  ResultSet.First;
  Check(ResultSet.GetInt(ext_id_index) = 1);
  CheckEquals('House', ResultSet.GetString(ext_enum_index));
  ResultSet.Close;
  Statement.Close;

  // Insert case
  Statement.ExecuteQuery('DELETE FROM extension where ext_id = 1');

  Statement.ExecuteQuery('INSERT INTO extension VALUES(1,''Car'')');

  ResultSet := Statement.ExecuteQuery('SELECT * FROM extension where ext_id = 1');
  CheckNotNull(ResultSet);
  ResultSet.First;
  Check(ResultSet.GetInt(ext_id_index) = 1);
  CheckEquals('Car', ResultSet.GetString(ext_enum_index));
  ResultSet.Close;
  Statement.Close;
end;

procedure TZTestDbcPostgreSQLCase.TestGUIDs;
const
  ext_id_index = FirstDbcIndex+1;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  S: String;
begin
  if Connection.GetHostVersion < 9 then
    Exit;
  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);

  ResultSet := Statement.ExecuteQuery('SELECT id, guid FROM guid_test WHERE id = 1');
  try
    CheckNotNull(ResultSet);
    ResultSet.First;

    // Compare initial inserted value vs database read value from table
    S := ZSysUtils.GUIDToStr(ResultSet.GetBytes(ext_id_index));
    CheckEquals('{BAD51CFF-F21F-40E8-A9EA-838977A681BE}', s, 'UUID different');
    S := ResultSet.GetString(ext_id_index);
    //it's offical documented what PG returns:
    //https://www.postgresql.org/docs/9.1/static/datatype-uuid.html
    //so a native dbc user whould not agree if something else is returned
    CheckEquals(LowerCase('BAD51CFF-F21F-40E8-A9EA-838977A681BE'), s, 'UUID different');
  finally
    ResultSet.Close;
    Statement.Close;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcPostgreSQLCase.Suite);
{$ENDIF ZEOS_DISABLE_POSTGRESQL}
end.
