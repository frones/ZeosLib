{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Test Case for Database Connectivity Classes        }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZTestDataSetGeneric;

interface
{$I ZComponent.inc}

uses
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZDataset, ZConnection, ZDbcIntfs, ZTokenizer, ZSqlTestCase, ZCompatibility,
  ZAbstractRODataset, ZMessages;

type
  {** Implements a test case for . }

  { TZGenericTestDataSet }

  TZGenericTestDataSet = class(TZAbstractCompSQLTestCase)
  private
    FQuery: TZQuery;
    FFieldList: string;
    FTokenizer: IZTokenizer;
    procedure RunDefineFields;
    procedure RunDefineSortedFields;
    procedure TestReadCachedLobs(const BinLob: String; aOptions: TZDataSetOptions;
      BinStreamE: TMemoryStream; Query: TZAbstractRODataset);
  protected
    procedure TestQueryGeneric(Query: TDataset);
    procedure TestFilterGeneric(Query: TDataset);
    function IsRealPreparableTest: Boolean; override;
  published
    constructor Create(MethodName: string); override;
    procedure TestConnection;
    procedure TestReadOnlyQuery;
    procedure TestReadOnlyQueryUniDirectional;
    procedure TestQuery;
    procedure TestReadOnlyQueryExecSql;
    procedure TestQueryExecSql;
    procedure TestQueryUpdate;
    procedure TestPreparedStatement;
    procedure TestParamChar;
    procedure TestReadOnlyQueryFilter;
    procedure TestQueryFilter;
    procedure TestQueryLocate;
    procedure TestFilterExpression;
    procedure TestDecodingSortedFields;
    procedure TestSmartOpen;
    procedure TestPrepare;
    procedure TestTimeFilterExpression;
    procedure TestDateTimeFilterExpression;
    procedure TestTimeLocateExpression;
    procedure TestDateTimeLocateExpression;
    procedure TestInsertNumbers;
    procedure TestNullDateTimeValues;
    procedure TestDoubleFloatParams;
    procedure TestClobEmptyString;
    procedure TestLobModes;
    procedure TestSpaced_Names;
    procedure Test_doCachedLobs;
    procedure TestDefineFields;
    procedure TestDefineSortedFields;
    procedure TestEmptyMemoAfterFullMemo;
    procedure TestNullUnionNull;
    procedure TestVeryLargeBlobs;
    procedure TestKeyWordParams;
  end;


implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  {$IFDEF UNICODE}ZEncoding,{$ENDIF}ZFastCode,
  DateUtils, ZSysUtils, ZTestConsts, ZTestCase,
  ZDatasetUtils, strutils{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF},
  TypInfo;

{ TZGenericTestDataSet }

constructor TZGenericTestDataSet.Create(MethodName: string);
begin
  inherited;

  FTokenizer := TZTokenizer.Create as IZTokenizer;
end;

procedure TZGenericTestDataSet.TestConnection;
var
  MetadataList: TStrings;
  Query: TZQuery;
  Table: TZTable;
  ROQuery: TZReadOnlyQuery;
begin
  Query := TZQuery.Create(nil); // not CreateQuery because here we test unconnected queries
  Table := TZTable.Create(nil); // not CreateTable because here we test unconnected queries
  ROQuery := TZReadOnlyQuery.Create(nil); // not CreateReadOnlyQuery because here we test unconnected queries
  MetadataList := TStringList.Create;
  try
    CheckEquals(False, Connection.Connected);
    CheckEquals(False, Connection.ReadOnly);
    CheckEquals(True, Connection.AutoCommit);

    Query.Connection := Connection;
    Table.Connection := Connection;
    ROQuery.Connection := Connection;

    Connection.AutoCommit := False;
    CheckEquals(Ord(tiNone), Ord(Connection.TransactIsolationLevel));

    { Checks without transactions. }
    Connection.Connect;
    CheckEquals(True, Connection.Connected);
    Connection.Commit;
    Connection.Rollback;
    Connection.Disconnect;
    CheckEquals(False, Connection.Connected);

    { Checks with transactions. }
    Connection.TransactIsolationLevel := tiReadCommitted;
    Connection.Connect;
    CheckEquals(True, Connection.Connected);
    Connection.Commit;
    Connection.Rollback;
    Connection.Disconnect;
    CheckEquals(False, Connection.Connected);

    try
      Connection.GetProtocolNames(MetadataList);
      Check(MetadataList.Count > 0, 'GetProtocolNames returns an empty list');
      Check(MetadataList.IndexOf(Protocol) >= 0, 'Error in GetProtocolNames');

      try
        Connection.GetCatalogNames(MetadataList);
        Fail('On closed connection call should throw exception');
      except on E: Exception do
        CheckNotTestFailure(E);
      end;

      Connection.Connect;

      Connection.GetCatalogNames(MetadataList);
      Connection.GetSchemaNames(MetadataList);
      Connection.GetTableNames('', MetadataList);
      Check(MetadataList.Count > 0, 'Error in GetTableNames');
      Connection.GetStoredProcNames('', MetadataList);
    finally
      //nothing happens here
    end;
  finally
    //FreeMemory
    MetadataList.Free;
    Query.Free;
    Table.Free;
    ROQuery.Free;
  end;
end;

{**
  Check functionality prepared statement
}
procedure TZGenericTestDataSet.TestPreparedStatement;
var
  Ansi: AnsiString;
  WS: WideString;
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
  s:string;
begin
  Query := CreateQuery;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM people where p_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      SQL.Text := 'DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
    end;

    {
      The test for equipment table
    }
    with Query do
    begin
      { Create prepared statement for equipment table }
      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
      CheckEquals(6, Params.Count);

      Params[0].DataType := ftInteger;
      Params[1].DataType := ftString;
      Params[2].DataType := ftSmallint;
      Params[3].DataType := ftFloat;
      Params[4].DataType := ftDate;
      Params[5].DataType := ftDate;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsString := '\xyz\'+#13;
      Params[2].AsInteger := 7;
      Params[3].AsFloat := 1234.567;
      Params[4].AsDateTime := EncodeDate(1999, 8, 5);
      Params[5].Value := Null;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      { check inserted row from equipment table }
      SQL.Text := 'SELECT * FROM equipment WHERE eq_id = :eq_id';
      CheckEquals(1, Query.Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID, FieldByName('eq_id').AsInteger);
      s:=FieldByName('eq_name').AsString;
      CheckEquals('\xyz\'#13, s);
      CheckEquals(7, FieldByName('eq_type').AsInteger);
      CheckEquals(1234.567, FieldByName('eq_cost').AsFloat, 0.001);
      CheckEquals(EncodeDate(1999, 8, 5), FieldByName('eq_date').AsDateTime);
      CheckEquals(True, FieldByName('woff_date').IsNull);
      Close;

      { update inserted row from equipment table }
      SQL.Text := 'UPDATE equipment SET eq_name = :eq_name WHERE eq_id = :eq_id';
      CheckEquals(2, Params.Count);

      Params[0].DataType := ftString;
      Params[1].DataType := ftInteger;

      Params[0].AsString := 'xyz1';
      Params[1].AsInteger := TEST_ROW_ID;

      ExecSQL;
      CheckEquals(1, RowsAffected);

      { delete inserted row from equipment table }
      SQL.Text := 'DELETE FROM equipment WHERE eq_id = :eq_id';

      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;

    { The test for people table }
    with Query do
    begin
      { Create prepared statement for people table }
      SQL.Text := 'INSERT INTO people (p_id, p_dep_id, p_name, p_begin_work, p_end_work,' +
          ' p_picture, p_resume, p_redundant) VALUES(:p_id, :p_dep_id, :p_name, ' +
          ' :p_begin_work, :p_end_work, :p_picture, :p_resume, :p_redundant)';
      { Sets prepared statement parameters values. }
      CheckEquals(8, Params.Count);

      Params[0].DataType := ftInteger;
      Params[1].DataType := ftSmallint;
      Params[2].DataType := ftString;
      Params[3].DataType := ftDateTime;
      Params[4].DataType := ftDateTime;
      Params[5].DataType := ftBlob;
      Params[6].DataType := ftMemo;
      Params[7].DataType := ftSmallint;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsInteger := 2;
      Params[2].AsString := 'xyz';
      Params[3].AsDateTime := EncodeTime(8, 0, 0, 0);
      Params[4].AsDateTime := EncodeTime(17, 30, 0, 0);

      BinStream := TMemoryStream.Create;
      BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
      Params[5].LoadFromStream(BinStream, ftBlob);

      StrStream := TMemoryStream.Create;
      if ConnectionConfig.Transport = traWEBPROXY then
        StrStream.LoadFromFile(TestFilePath('text/lgpl without control characters.txt'))
      else
        StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
//      Params[6].LoadFromStream(StrStream, {$IFDEF UNICODE}ftWideMemo{$ELSE}ftMemo{$ENDIF});
      Params[6].LoadFromStream(StrStream, ftMemo);

      Params[7].Value := Null;
      ExecSql;
      CheckEquals(1, RowsAffected);

      { Checks inserted row. }
      SQL.Text := 'SELECT * FROM people WHERE p_id = :p_id';
      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ReadOnly:=True;
      Open;
      CheckEquals(TEST_ROW_ID, FieldByName('p_id').AsInteger);
      CheckEquals(False, FieldByName('p_id').IsNull);
      CheckEquals(2, FieldByName('p_dep_id').AsInteger);
      CheckEquals(False, FieldByName('p_dep_id').IsNull);
      CheckEquals('xyz', FieldByName('p_name').AsString);
      CheckEquals(False, FieldByName('p_name').IsNull);
      CheckEquals(EncodeTime(8, 0, 0, 0), FieldByName('p_begin_work').AsDateTime, 0.0001);
      CheckEquals(False, FieldByName('p_begin_work').IsNull);
      CheckEquals(EncodeTime(17, 30, 0, 0), FieldByName('p_end_work').AsDateTime, 0.0001);
      CheckEquals(False, FieldByName('p_end_work').IsNull);
      CheckEquals(False, FieldByName('p_picture').IsNull);
      CheckEquals(False, FieldByName('p_resume').IsNull);
      CheckEquals(0, FieldByName('p_redundant').AsInteger);
      CheckEquals(True, FieldByName('p_redundant').IsNull);

      { compare aciistream/unicodestream }
      //Modification by EgonHugeist: Different behavior for the Same Field
      //With dependencies on stUnicodeStream = CP_UTF8 for Delphi-compilers.
      //Now we read a none Wide-Stream in! What happens? Zeos is now able
      //to autodetect such strange things! But Zeos converts the Ansi-Stream to
      //a WiteString-Stream. So this test must be modified...
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
      begin
        StrStream.position := 0;
        SetLength(Ansi,StrStream.Size);
        StrStream.Read(PAnsiChar(Ansi)^, StrStream.Size);
        WS := {$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(Ansi);
        StrStream.Clear;
        StrStream.Write(PWideChar(WS)^, Length(WS)*2);
        StrStream.Position := 0;
      end;
      StrStream1 := TMemoryStream.Create;
      (FieldByName('p_resume') as TBlobField).SaveToStream(StrStream1);
      CheckEquals(StrStream, StrStream1, 'Ascii Stream');
      StrStream.Free;
      StrStream1.Free;

      { compare BinaryStream }
      BinStream1 := TMemoryStream.Create;
      (FieldByName('p_picture') as TBlobField).SaveToStream(BinStream1);
      CheckEquals(BinStream, BinStream1, 'Binary Stream');
      BinStream.Free;
      BinStream1.Free;
      Close;

      { Delete the row. }
      SQL.Text := 'DELETE FROM people WHERE p_id = :p_id';
      CheckEquals(1, Params.Count);

      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;
  finally
    Query.SQL.Text := 'DELETE FROM people where p_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
    Query.ExecSQL;
    Query.SQL.Text := 'DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
    Query.ExecSQL;
    Query.Free;
  end;
end;

{**
  Check functionality ParamChar
}
procedure TZGenericTestDataSet.TestParamChar;
var
  Query: TZQuery;
  s:string;
begin
  Query := CreateQuery;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
    end;
    //see http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
    Query.SQL.Text := 'select IsNull(B.X||B.y, '+QuotedStr('')+') as X_Y_Z from "My_Table" as B '+
    'where 1=1 and :user_ID = B.x and ((B.nazwa iLike :nazwaFiltr) or (B.skrot iLike :nazwaFiltr))';
    CheckEquals(2, Query.Params.Count);
    CheckEquals('user_ID', Query.Params[0].Name);
    CheckEquals('nazwaFiltr', Query.Params[1].Name);
    Query.SQL.Text := '';

    {
      The test for equipment table
    }
    with Query do
    begin
      { Create prepared statement for equipment table }
      ParamChar := '&';
      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(:q_id, :eq_name, :eq_type, :eq_cost, :eq_date, :woff_date)';
      CheckEquals(0, Params.Count);

      Sql.Text := 'INSERT INTO equipment (eq_id, eq_name, eq_type, eq_cost, eq_date, '
          + ' woff_date) VALUES(&q_id, &eq_name, &eq_type, &eq_cost, &eq_date, &woff_date)';
      CheckEquals(6, Params.Count);

      Params[0].DataType := ftInteger;
      Params[1].DataType := ftString;
      Params[2].DataType := ftSmallint;
      Params[3].DataType := ftFloat;
      Params[4].DataType := ftDate;
      Params[5].DataType := ftDate;

      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsString := '\xyz\'+#13;
      Params[2].AsInteger := 7;
      Params[3].AsFloat := 1234.567;
      Params[4].AsDateTime := EncodeDate(1999, 8, 5);
      Params[5].Value := Null;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      { check inserted row from equipment table }
      SQL.Text := 'SELECT * FROM equipment WHERE eq_id = &eq_id';
      CheckEquals(1, Query.Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID, FieldByName('eq_id').AsInteger);
      s:=FieldByName('eq_name').AsString;
      CheckEquals('\xyz\'#13, s);
      CheckEquals(7, FieldByName('eq_type').AsInteger);
      CheckEquals(1234.567, FieldByName('eq_cost').AsFloat, 0.001);
      CheckEquals(EncodeDate(1999, 8, 5), FieldByName('eq_date').AsDateTime);
      CheckEquals(True, FieldByName('woff_date').IsNull);
      Close;

      { delete inserted row from equipment table }
      SQL.TEXT := ''; // cleanup beacuse otherwise the previous select would be parsed
                      // resulting in an error because there's a space immediately after the '*' symbol
      ParamChar := '*';
      SQL.Text := 'DELETE FROM equipment WHERE eq_id = *eq_id';

      CheckEquals(1, Params.Count);
      Params[0].DataType := ftInteger;
      Params[0].AsInteger := TEST_ROW_ID;
      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;

  finally
    Query.Free;
  end;
end;

{**
  Check functionality of TZQuery
}
procedure TZGenericTestDataSet.TestQuery;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;


{**
  Check functionality execute statement for  TZQuery
}
procedure TZGenericTestDataSet.TestQueryExecSql;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    with Query do
    begin
      SQL.Text := 'UPDATE equipment SET eq_name=eq_name';
      ExecSQL;
      SQL.Text := 'SELECT * FROM equipment';
      ExecSQL;

  //  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  //  Check(Statement.Execute('SELECT * FROM equipment'));

      SQL.Text := 'DELETE FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;

      { insert row to department table }
      SQL.Text := 'INSERT INTO department VALUES (' +
        SysUtils.IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')';
      ExecSQL;
      CheckEquals(1, RowsAffected);

      { check what row inserted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(1, RecordCount);
      Close;

      { update row in department table }
      SQL.Text := 'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
      SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { delete value from department table }
      SQL.Text := 'DELETE FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { check what row deleted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Check functionality of TZReadOnlyQuery
}

type
  TZAbstractRODatasetHack = class(TZAbstractRODataset)
  end;

procedure TZGenericTestDataSet.TestReadCachedLobs(const BinLob: String;
  aOptions: TZDataSetOptions; BinStreamE: TMemoryStream; Query: TZAbstractRODataset);
var
  BinStreamA: TMemoryStream;
begin
  BinStreamA := nil;
  try
    with TZAbstractRODatasetHack(Query) do
    begin
      Options := aOptions;
      SQL.Text := 'SELECT * FROM blob_values where b_id >= '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      Open;
      CheckEquals(2, RecordCount, 'RecordCount');
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID-1, FieldByName('b_id').AsInteger);
      Next;
      CheckEquals(TEST_ROW_ID, FieldByName('b_id').AsInteger);
      BinStreamA := TMemoryStream.Create;
      BinStreamA.Position:=0;
      (FieldByName(BinLob) as TBlobField).SaveToStream(BinStreamA);
      CheckEquals(BinStreamE, BinStreamA, 'Binary Stream');
      FreeAndNil(BinStreamA);
      First;
      Refresh;
      CheckEquals(2, RecordCount, 'RecordCount');
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID-1, FieldByName('b_id').AsInteger);
      Next;
      CheckEquals(TEST_ROW_ID, FieldByName('b_id').AsInteger);
      BinStreamA := TMemoryStream.Create;
      BinStreamA.Position:=0;
      (FieldByName(BinLob) as TBlobField).SaveToStream(BinStreamA);
      CheckEquals(BinStreamE, BinStreamA, 'Binary Stream');
      FreeAndNil(BinStreamA);
      Close;
    end;
  finally
    FreeAndNil(BinStreamA);
  end;
end;

procedure TZGenericTestDataSet.TestReadOnlyQuery;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;

(**
check Unidirectional behavior of TZReadOnlyQuery
*)
procedure TZGenericTestDataSet.TestReadOnlyQueryUnidirectional;
var
  Query: TZReadOnlyQuery;
begin
(*  {$IFDEF FPC}
  if SkipForReason(srNonZeos) then Exit;
  {$ENDIF}
*)

  Query := CreateReadOnlyQuery;
  Query.IsUniDirectional := True;
  try
    TestQueryGeneric(Query);
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestQueryUpdate;
var
  Sql_: string;
  WS: ZWideString;
  Ansi: AnsiString;
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
begin
  Query := CreateQuery;
  { FPC init -> if test fails we've random addresses so we're trying to free a non created object -> PFC, puff puff puff}
  StrStream := nil;
  BinStream := nil;
  StrStream1 := nil;
  BinStream1 := nil;
  try
    Query.SQL.Text := 'DELETE FROM people where p_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
    Query.ExecSQL;

    Query.SQL.Text := 'DELETE FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
    Query.ExecSQL;

    {
      The test for equipment table
    }

    with Query do
    begin
      { insert test record to equipment }
      Sql_ := 'SELECT * FROM equipment where eq_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, IsEmpty);

      Append;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      CheckEquals(Ord(dsInsert), Ord(State));
      FieldByName('eq_id').AsInteger := TEST_ROW_ID;
      FieldByName('eq_name').Value := Null;
      FieldByName('eq_type').Value := Null;
      FieldByName('eq_cost').Value := Null;
      FieldByName('eq_date').Value := Null;
      FieldByName('woff_date').Value := Null;
      CheckEquals(True, Modified);
      Post;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      Close;

      { update row for equipment}
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);
      {$IFNDEF WITH_FPC_BOF_BUG}
      CheckEquals(True, Bof);
      {$ENDIF}

      Edit;
      CheckEquals(Ord(dsEdit), Ord(State));
      FieldByName('eq_name').AsString := 'The some thing5678901234567890';
      FieldByName('eq_type').AsInteger := 1;
      FieldByName('eq_cost').AsFloat := 12345.678;
      FieldByName('eq_date').AsDateTime := EncodeDate(1989, 07, 07);
      FieldByName('woff_date').AsDateTime := EncodeDate(1998, 04, 24);
      CheckEquals(True, Modified);
      Post;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      Close;

      { check previous updated row}
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);

      {$IFNDEF WITH_FPC_BOF_BUG}
      CheckEquals(True, Bof);
      {$ENDIF}
      CheckEquals(Ord(dsBrowse), Ord(State));
      CheckEquals('The some thing5678901234567890', FieldByName('eq_name').AsString);
      CheckEquals(1, FieldByName('eq_type').AsInteger);
      CheckEquals(12345.678, FieldByName('eq_cost').AsFloat, 0.01);
      CheckEquals(EncodeDate(1989, 07, 07), FieldByName('eq_date').AsDateTime);
      CheckEquals(EncodeDate(1998, 04, 24), FieldByName('woff_date').AsDateTime);
      Delete;
      Close;

      { check what record deleted }
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, IsEmpty);
    end;

      {
        The test for people table
      }
    with Query do
    begin
      Sql_ := 'DELETE FROM people where p_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      SQL.Text := Sql_;
      ExecSQL;

      Sql_ := 'SELECT * FROM people where p_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      StrStream := TMemoryStream.Create();
      if ConnectionConfig.Transport = traWEBPROXY then
        StrStream.LoadFromFile(TestFilePath('text/lgpl without control characters.txt'))
      else
        StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));

      //Modification by EgonHugeist: Different behavior for the Same Field
      //With dependencies on stUnicodeStream = CP_UTF8 for Delphi-compilers.
      //Now we read a none Wide-Stream in! What happens? Zeos is now able
      //to autodetect such strange things! But Zeos converts the Ansi-Stream to
      //a WiteString-Stream. So this test must be modified...
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
      begin
        SetLength(Ansi,StrStream.Size);
        StrStream.Read(PAnsiChar(Ansi)^, StrStream.Size);
        WS := {$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(Ansi);
        StrStream.Clear;
        StrStream.Write(PWideChar(WS)^, Length(WS)*2);
        StrStream.Position := 0;
      end;
      BinStream := TMemoryStream.Create();
      BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
      BinStream1 := TMemoryStream.Create;
      StrStream1 := TMemoryStream.Create;

      { insert test record to people table }
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, Query.IsEmpty);

      Append;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      CheckEquals(Ord(dsInsert), Ord(State));
      FieldByName('p_id').AsInteger := TEST_ROW_ID;
      FieldByName('p_dep_id').Value := Null;
      FieldByName('p_name').Value := Null;
      FieldByName('p_begin_work').Value := Null;
      FieldByName('p_end_work').Value := Null;
      FieldByName('p_resume').Value := Null;
      FieldByName('p_picture').Value := Null;
      FieldByName('p_redundant').Value := Null;
      CheckEquals(True, Modified);
      Post;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      Close;


      { check previous inserted record }
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);

      {$IFNDEF WITH_FPC_BOF_BUG}
      CheckEquals(True, Bof);
      {$ENDIF}
      CheckEquals(Ord(dsBrowse), Ord(State));
      CheckEquals(TEST_ROW_ID, FieldByName('p_id').AsInteger);
      CheckEquals(True, FieldByName('p_dep_id').IsNull);
      CheckEquals(True, FieldByName('p_name').IsNull);
      CheckEquals(True, FieldByName('p_begin_work').IsNull);
      CheckEquals(True, FieldByName('p_end_work').IsNull);
      CheckEquals(True, FieldByName('p_resume').IsNull);
      CheckEquals(True, FieldByName('p_picture').IsNull);
      CheckEquals(True, FieldByName('p_redundant').IsNull);
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}

      Edit;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      CheckEquals(Ord(dsEdit), Ord(State));
      FieldByName('p_dep_id').AsInteger := 1;
      FieldByName('p_name').AsString := 'Somebody';
      FieldByName('p_begin_work').AsDateTime := EncodeTime(12, 11, 20, 0);
      FieldByName('p_end_work').AsDateTime := EncodeTime(22, 36, 55, 0);

      (FieldByName('p_resume') as TBlobField).LoadFromStream(StrStream);
      (FieldByName('p_picture') as TBlobField).LoadFromStream(BinStream);

      FieldByName('p_redundant').AsInteger := 1;
      CheckEquals(True, Modified);
      Post;
      {$IFNDEF WITH_FPC_MODIFIED_BUG}
      CheckEquals(False, Modified);
      {$ENDIF}
      Close;

      { create and update resultset for people table for p_id = TEST_ROW_ID }
      SQL.Text := Sql_;
      Open;
      CheckEquals(False, IsEmpty);

      CheckEquals(1, FieldByName('p_dep_id').AsInteger);
      CheckEquals('Somebody', FieldByName('p_name').AsString);
      CheckEquals(EncodeTime(12, 11, 20, 0), FieldByName('p_begin_work').AsDateTime, 0.0001);
      CheckEquals(EncodeTime(22, 36, 55, 0), FieldByName('p_end_work').AsDateTime, 0.0001);

      (FieldByName('p_picture')as TBlobField).SaveToStream(BinStream1);
      (FieldByName('p_resume')as TBlobField).SaveToStream(StrStream1);

      CheckEquals(BinStream, BinStream1);
      CheckEquals(StrStream, StrStream1);
      CheckEquals(1, FieldByName('p_redundant').AsInteger);
      Delete;

      FreeAndNil(BinStream);
      FreeAndNil(BinStream1);
      FreeAndNil(StrStream);
      FreeAndNil(StrStream1);

      { create and update resultset for equipment table for eq_id = TEST_ROW_ID }
      SQL.Text := Sql_;
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    FreeAndNil(BinStream);
    FreeAndNil(BinStream1);
    FreeAndNil(StrStream);
    FreeAndNil(StrStream1);
    FreeAndNil(Query);
  end;
end;

procedure TZGenericTestDataSet.TestQueryGeneric(Query: TDataset);
var
  SQL: string;
  i: Integer;
begin
  { select equipment table }
  SQL := 'DELETE FROM equipment where eq_id > 100';
  if GetName = 'TestQuery' then
  begin
    (Query as TZquery).SQL.Text := SQL;
    (Query as TZquery).ExecSQL;
  end
  else
  begin
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
    (Query as TZReadOnlyQuery).ExecSQL;
  end;

  { select equipment table }
  SQL := 'SELECT * FROM equipment where eq_id > 100';
  if GetName = 'TestQuery' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(True, IsEmpty);
    Close;
  end;

  { select equipment table}
  SQL := 'SELECT * FROM equipment where eq_id = 1';
  if GetName = 'TestQuery' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(1,     RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(1,       FieldByName('eq_id').AsInteger);
    CheckEquals('Volvo', FieldByName('eq_name').AsString);
    CheckEquals(1,       FieldByName('eq_type').AsInteger);
    CheckEquals(15000,   FieldByName('eq_cost').AsFloat);
    CheckEquals(EncodeDate(1998, 03, 04), Trunc(FieldByName('eq_date').AsDateTime));
    CheckEquals(True,   FieldByName('woff_date').IsNull);
    Close;
  end;

  { select from people table two records }
  SQL := 'SELECT * FROM people where p_id <= 2';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(False, IsEmpty);
    if not Query.IsUnidirectional then
      CheckEquals(2, RecordCount);

    CheckEquals(1, RecNo);
    {$IFNDEF WITH_FPC_BOF_BUG}
    CheckEquals(True, Bof);
    {$ENDIF}
    CheckEquals(False, Eof);
    CheckEquals(1, FieldByName('p_id').AsInteger);
    CheckEquals(1, FieldByName('p_dep_id').AsInteger);
    CheckEquals('Vasia Pupkin', FieldByName('p_name').AsString);
    CheckEquals(EncodeTime(9, 0, 0, 0),
      Frac(Abs(FieldByName('p_begin_work').AsDateTime)), 0.0001);
    CheckEquals(EncodeTime(18, 0, 0, 0),
      Frac(Abs(FieldByName('p_end_work').AsDateTime)), 0.0001);
    CheckEquals(True, FieldByName('p_picture').IsNull);
    CheckEquals(True, FieldByName('p_resume').IsNull);
    CheckEquals(0, FieldByName('p_redundant').AsInteger);

    Next;
    Next; // just a check for current RecNo = 2 / we are one EOF here
    CheckEquals(2, RecNo);
    {$IFNDEF WITH_FPC_BOF_BUG}
    CheckEquals(False, Bof);
    {$ENDIF}
    CheckEquals(True, Eof);
    CheckEquals(2, FieldByName('p_id').AsInteger);
    CheckEquals(2, FieldByName('p_dep_id').AsInteger);
    CheckEquals('Andy Karto', FieldByName('p_name').AsString);
    CheckEquals(EncodeTime(8, 30, 0, 0),
      Frac(Abs(FieldByName('p_begin_work').AsDateTime)), 0.0001);
    CheckEquals(EncodeTime(17, 30, 0, 0),
      Frac(Abs(FieldByName('p_end_work').AsDateTime)), 0.0001);
    Check(FieldByName('p_picture').IsNull);
    Check(FieldByName('p_resume').IsNull);
    CheckEquals(0, FieldByName('p_redundant').AsInteger);
    Close;
  end;

  { select cargo table}
  SQL := 'SELECT * FROM cargo where c_id = 2';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(False, IsEmpty);
    CheckEquals(2, FieldByName('c_id').AsInteger);
    CheckEquals(1, FieldByName('c_dep_id').AsInteger);
    CheckEquals('Paper', Trim(FieldByName('c_name').AsString));
    CheckEquals(2, FieldByName('c_seal').AsInteger);
    CheckEquals(EncodeDate(2002, 12, 19) + EncodeTime(14, 0, 0, 0),
      FieldByName('c_date_came').AsDateTime, 0.001);
    CheckEquals(EncodeDate(2002, 12, 23) + EncodeTime(0, 0, 0, 0),
      FieldByName('c_date_out').AsDateTime, 0.001);
    CheckEquals(1000, FieldByName('c_weight').AsFloat);
    CheckEquals(10, FieldByName('c_width').AsInteger);
    CheckEquals(10, FieldByName('c_height').AsInteger);
    CheckEquals(986.47, FieldByName('c_cost').AsFloat, 0.001);
    //CheckEquals('#14#17#T���2', FieldByName('c_attributes').AsString);
    Close;
  end;

  { select cargo table date parameter checking}
  SQL := 'SELECT * FROM cargo where c_date_came = :DateCame';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    if GetName = 'TestQuery' then
      (Query as TZQuery).ParamByName('DateCame').AsDateTime := EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0)
    else
      (Query as TZReadOnlyQuery).ParamByName('DateCame').AsDateTime := EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0);
    Open;
    CheckEquals(1, RecordCount);
    CheckEquals(EncodeDate(2002, 12, 20) + EncodeTime(2, 0, 0, 0),
      FieldByName('c_date_out').AsDateTime, 0.001);
    Close;
  end;

  { select equipment table }
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    if not Query.IsUnidirectional then
      CheckEquals(4, RecordCount);
    CheckEquals(False, IsEmpty);

    {$IFNDEF WITH_FPC_BOF_BUG}
    CheckEquals(True, Bof);
    {$ENDIF}
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(True, Eof);

    if not Query.IsUnidirectional then
    begin
      First;
      {$IFNDEF WITH_FPC_BOF_BUG}
      CheckEquals(True, Bof);
      {$ENDIF}
      Last;
      CheckEquals(True, Eof);
    end;
    Close;
  end;

  { select equipment table }
  // test traversing through dataset with FindNext
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    i := 0;
    repeat
      Inc(i);
      CheckEquals(i, RecNo);
    until not Query.FindNext;
    CheckEquals(i, RecordCount);
    CheckEquals(4, RecordCount);
    Close;
  end;

  { select equipment table }
  // test traversing through dataset with Next
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
    (Query as TZQuery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    i := 0;
    while not EOF do
    begin
      Inc(i);
      CheckEquals(i, RecNo);
      Next;
    end;
    CheckEquals(i, RecordCount);
    CheckEquals(4, RecordCount);
    Close;
  end;

  { create resultset for equipment table with limit rows}
(*
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQuery' then
  begin
    (Query as TZQuery).DbcStatement.SetMaxRows(2);
    (Query as TZquery).SQL.Text := SQL;
  end
  else
  begin
    (Query as TZReadOnlyQuery).DbcStatement.SetMaxRows(2);
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  end;
  with Query do
  begin
    Open;
    CheckEquals(False, IsEmpty);
    CheckEquals(2, RecordCount);

    {$IFNDEF WITH_FPC_BOF_BUG}
    CheckEquals(True, Bof);
    {$ENDIF}
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, Eof);
    Next;
    CheckEquals(False, True);

    Close;
  end;
*)
end;

{**
   Check functionality execute statement for  TZReadOnlyQuery
}
procedure TZGenericTestDataSet.TestReadOnlyQueryExecSql;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    with Query do
    begin
      SQL.Text := 'UPDATE equipment SET eq_name=eq_name';
      ExecSQL;
      SQL.Text := 'SELECT * FROM equipment';
      ExecSQL;

  //  Check(not Statement.Execute('UPDATE equipment SET eq_name=eq_name'));
  //  Check(Statement.Execute('SELECT * FROM equipment'));

      SQL.Text := 'DELETE FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;

      { insert row to department table }
      SQL.Text := 'INSERT INTO department VALUES (' +
        SysUtils.IntToStr(TEST_ROW_ID) + ',''Some agency'',''ENG'',''Some city'')';
      ExecSQL;
      CheckEquals(1, RowsAffected);

      { check what row inserted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(1, RecordCount);
      Close;

      { update row in department table }
      SQL.Text :=
      'UPDATE department SET dep_name=NULL, dep_shname=NULL, dep_address=NULL WHERE dep_id = ' +
        SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { delete value from department table }
      SQL.Text := 'DELETE FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      CheckEquals(1, RowsAffected);
      Close;

      { chech what recird deleted }
      SQL.Text := 'SELECT * FROM department where dep_id = ' + SysUtils.IntToStr(TEST_ROW_ID);
      Open;
      CheckEquals(True, IsEmpty);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

{**
   Test for filtering recods in TZQuery
}
procedure TZGenericTestDataSet.TestQueryFilter;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    TestFilterGeneric(Query);
  finally
    Query.Free;
  end;
end;

{**
   Generic test for filtering recods in dataset
}
procedure TZGenericTestDataSet.TestFilterGeneric(Query: TDataset);
var
  SQL: string;
begin
  { select equipment table }
  SQL := 'SELECT * FROM equipment';
  if GetName = 'TestQueryFilter' then
    (Query as TZquery).SQL.Text := SQL
  else
    (Query as TZReadOnlyQuery).SQL.Text := SQL;
  with Query do
  begin
    Open;
    Filtered := True;
    CheckEquals(4, RecordCount, 'RecordCount');
    CheckEquals(False, IsEmpty, 'IsEmpty');

    //(*
    Filter := 'eq_date = ''' + DateToStr(Encodedate(2001, 10, 7)) + '''';
    CheckEquals(1, RecordCount);
    CheckEquals(2, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_date > ''' + DateToStr(Encodedate(2000, 1, 1)) + '''';
    CheckEquals(2, RecordCount);
    First;
    CheckEquals(2, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(4, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_id = 3';
    CheckEquals(1, RecordCount);
    CheckEquals('Computer', FieldByName('eq_name').AsString, 'Field eq_name');

    Filter := 'eq_name = ''Volvo''';
    CheckEquals(1, RecordCount);
    CheckEquals(1, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_cost > 1000';
    CheckEquals(2, RecordCount);
    First;
    CheckEquals(1, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(2, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_type <= 10';
    CheckEquals(3, RecordCount);
    First;
    CheckEquals(1, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(2, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(3, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_name like ''C*''';
    CheckEquals(1, RecordCount); //oracle, ADO, SQLite fails
    CheckEquals(3, FieldByName('eq_id').AsInteger, 'field eq_id');

    Filter := 'eq_name like ''*o*''';
    CheckEquals(4, RecordCount);
    {check what cursor save position}
    CheckEquals(3, FieldByName('eq_id').AsInteger, 'field eq_id');
    First;
    CheckEquals(1, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(2, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(3, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(4, FieldByName('eq_id').AsInteger, 'field eq_id');


    Filter := 'eq_date <= ''' + DateToStr(Encodedate(2000, 8, 7)) + '''';
    CheckEquals(3, RecordCount);
    First;
    CheckEquals(1, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(3, FieldByName('eq_id').AsInteger, 'field eq_id');
    Next;
    CheckEquals(4, FieldByName('eq_id').AsInteger, 'field eq_id');
    Filter := '';

    Filter := 'woff_date is null';  //SF.net ticket #229
    CheckEquals(4, RecordCount);

    Filter := 'woff_date is not null'; //SF.net ticket #229
    CheckEquals(0, RecordCount);

    Filter := '';
//    *)
    Close;

    //(*
    //SF.net ticket #200 ->
    CheckEquals(1, Connection.DbcConnection.CreateStatement.ExecuteUpdate('insert into equipment(eq_id, eq_name) values ('+
      SysUtils.IntToStr(5)+','''+DateToStr(Encodedate(2000, 8, 7))+''')'), 'inserted row');
    try
      Open;
      CheckEquals(5, RecordCount);

      Filter := 'eq_name = '''+DateToStr(Encodedate(2000, 8, 7))+'''';
      CheckEquals(1, RecordCount);
      CheckEquals(5, FieldByName('eq_id').AsInteger, 'field eq_id');

    finally
      Connection.DbcConnection.CreateStatement.ExecuteUpdate('delete from equipment where eq_id = '+SysUtils.IntToStr(5));
    end;
    //*)
  end;
end;

function TZGenericTestDataSet.IsRealPreparableTest: Boolean;
begin
  Result:= true;
end;

{**
  Test for locating recods in TZReadOnlyQuery
}
procedure TZGenericTestDataSet.TestQueryLocate;
var
  Query: TZReadOnlyQuery;
  ResData : boolean; 
begin
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Add('select * from cargo');
    Query.ExecSQL; 
    Query.Open; 
    Check(Query.RecordCount > 0, 'Query return no records'); 
    ResData := Query.Locate('C_DEP_ID;C_WIDTH;C_SEAL',VarArrayOf(['1','10','2']),[loCaseInsensitive]); 
    CheckEquals(true,ResData); 
    ResData := Query.Locate('C_DEP_ID,C_WIDTH,C_SEAL',VarArrayOf(['2',Null,'1']),[loCaseInsensitive]); 
    CheckEquals(true,ResData); 
  finally 
    Query.Free; 
  end; 
end; 

{**
  Test for filtering recods in TZReadOnlyQuery
}
procedure TZGenericTestDataSet.TestReadOnlyQueryFilter;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    TestFilterGeneric(Query);
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for filter expressions.
}
procedure TZGenericTestDataSet.TestFilterExpression;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Text := 'SELECT * FROM people';

    Query.Filter := 'p_id + 1 = 2';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(1, Query.FieldByName('p_id').AsInteger);

    Query.Filter := '"p_id" = 2';
    CheckEquals(1, Query.RecordCount);
    CheckEquals(2, Query.FieldByName('p_id').AsInteger);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for decoding sorted fields.
}
procedure TZGenericTestDataSet.TestDecodingSortedFields;
var
  Query: TZReadOnlyQuery;
  FieldRefs: TObjectDynArray;
  FieldComparsionKinds: TComparisonKindArray;
  OnlyDataFields: Boolean;
begin
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Text := 'SELECT p_id, p_dep_id, p_name FROM people';
    Query.Open;

    DefineSortedFields(Query, 'p_id', FTokenizer, FieldRefs, FieldComparsionKinds, OnlyDataFields);
    CheckEquals(1, Length(FieldRefs));
    CheckEquals(Integer(Query.Fields[0]), Integer(FieldRefs[0]));
    CheckEquals(1, Length(FieldComparsionKinds));
    CheckEquals(Ord(ckAscending), Ord(FieldComparsionKinds[0]));
    CheckEquals(True, OnlyDataFields);

    DefineSortedFields(Query, 'p_id ASC, p_name DESC', FTokenizer, FieldRefs,
      FieldComparsionKinds, OnlyDataFields);
    CheckEquals(2, Length(FieldRefs));
    CheckEquals(Integer(Query.Fields[0]), Integer(FieldRefs[0]));
    CheckEquals(Integer(Query.Fields[2]), Integer(FieldRefs[1]));
    CheckEquals(2, Length(FieldComparsionKinds));
    CheckEquals(Ord(ckAscending), Ord(FieldComparsionKinds[0]));
    CheckEquals(Ord(ckDescending), Ord(FieldComparsionKinds[1]));
    CheckEquals(True, OnlyDataFields);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for SmartOpen option.
}
procedure TZGenericTestDataSet.TestSmartOpen;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Text := 'select * from people';
    Check(not (doSmartOpen in Query.Options));

    Query.Open;
    Check(Query.Active);
    Query.Close;

    Query.Active := True;
    Check(Query.Active);
    Query.Active := False;

    Query.SQL.Text := 'update people set p_id=p_id where 1=0';
    try
      Query.Open;
      Fail('Wrong open behaviour without SmartOpen.');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

    try
      Query.Active := True;
      Fail('Wrong open behaviour without SmartOpen.');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

    Query.Options := Query.Options + [doSmartOpen];
    Query.SQL.Text := 'select * from people';

    Query.Open;
    Check(Query.Active);
    Query.Close;

    Query.Active := True;
    Check(Query.Active);
    Query.Active := False;

    Query.SQL.Text := 'update people set p_id=p_id where 1=0';
    try
      Query.Open;
    except
      Fail('Wrong open behaviour with SmartOpen.');
    end;
//    Check(not Query.Active);

    Query.Active := False;
    try
      Query.Active := True;
    except
      Fail('Wrong open behaviour with SmartOpen.');
    end;
//    Check(not Query.Active);

  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestPrepare;
var
  Query: TZReadOnlyQuery;
begin
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Text := 'select * from people';

    Query.Prepare;
    Check(Query.Prepared);
    Check(Not Query.Active);
    Query.Open;
    Check(Query.Active);
    Check(Query.Prepared);
    Query.Close;
    Query.Open;
    Check(Query.Active);
    Check(Query.Prepared);
    Query.Close;
    Check(Not Query.Active);
    Check(Query.Prepared);
    Query.UnPrepare;
    Check(Not Query.Prepared);

    Query.Active := True;
    Check(Query.Active);
    Check(Query.Prepared);
    Query.Unprepare;
    try
      Query.Prepare;
      Fail('Wrong prepare behaviour.');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
    Check(Not Query.Prepared);
    Query.Active := False;

  finally
    Query.Free;
  end;
end;


{**
Runs a test for time filter expressions.
}
procedure TZGenericTestDataSet.TestTimeFilterExpression;
var
  Query: TZQuery;
  DT: TDateTime;
begin
  Query := CreateQuery;
  try
    DT := EncodeTime(8,30,0,50);
    Query.SQL.Text := 'SELECT * FROM people';
    //!! Oracle: depend from local settings
    Query.Filter := 'p_begin_work >= "'+TimeToStr(DT)+'"';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    Query.Last;
    CheckEquals(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
    Query.Close;
    Query.Filter := '(p_begin_work > "'+TimeToStr(EncodeTime(8,0,0,0))+ '") AND (p_end_work < "'+TimeToStr(EncodeTime(18,0,0,0))+'")';
    Query.Open;
    CheckEquals(2, Query.RecordCount);
    Query.Last;
    CheckEquals(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
    CheckEquals(EncodeTime(17,30,0,0), Query.FieldByName('p_end_work').AsDateTime);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
Runs a test for Datetime filter expressions.
}
procedure TZGenericTestDataSet.TestDateTimeFilterExpression;
var
  Query: TZQuery;
  Date_came,Date_out : TDateTime;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM cargo';
    Date_came := EncodeDateTime(2002,12,19,18,30,0,0);
    Query.Filter := 'c_date_came >= "'+DateTimeToStr(Date_came)+'"';
    Query.Filtered := True;
    Query.Open;
    CheckEquals(3, Query.RecordCount);
    Query.Last;
    Date_came := EncodeDateTime(2002,12,21,10,20,0,0);
    CheckEquals(Date_Came, Query.FieldByName('c_date_came').AsDateTime);
    Query.Close;
    Date_came := EncodeDateTime(2002,12,19,14,30,0,0);
    Date_out := EncodeDateTime(2002,12,23,2,0,0,0);
    Query.Filter := '(c_date_came > "'+DateTimeToStr(Date_came)+ '") AND (c_date_out < "'+DateTimeToStr(Date_out)+'")';
    Query.Open;
    CheckEquals(2, Query.RecordCount);
    Query.First;
    Date_came := EncodeDateTime(2002,12,20,2,0,0,0);
    Date_out := EncodeDateTime(2002,12,20,2,0,0,0);
    CheckEquals(Date_came, Query.FieldByName('c_date_came').AsDateTime);
    CheckEquals(Date_out, Query.FieldByName('c_date_out').AsDateTime);
    Query.Close;
    Date_came := EncodeDateTime(2002,12,21,14,30,0,0); 
    Date_out := EncodeDateTime(2002,12,25,2,0,0,0); 
    Query.Filter := '(c_date_came < "'+DateTimeToStr(Date_came)+ '") AND (c_date_out > "'+DateTimeToStr(Date_out)+'")'; 
    Query.Open; 
    CheckEquals(1, Query.RecordCount); 
    Query.First; 
    Date_came := EncodeDateTime(2002,12,21,10,20,0,0); 
    Date_out := EncodeDateTime(2002,12,26,0,0,0,0); 
    CheckEquals(Date_came, Query.FieldByName('c_date_came').AsDateTime); 
    CheckEquals(Date_out, Query.FieldByName('c_date_out').AsDateTime); 
    Query.Close;
   finally 
    Query.Free;
  end;
end;

{**
Runs a test for time locate expressions.
}
procedure TZGenericTestDataSet.TestTimeLocateExpression;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    CheckEquals(true, Query.Locate('p_begin_work',EncodeTime(8,30,0,0),[]));
    CheckEqualsDate(EncodeTime(8,30,0,0), Query.FieldByName('p_begin_work').AsDateTime);
    CheckEqualsDate(EncodeTime(17,30,0,0), Query.FieldByName('p_end_work').AsDateTime);
    Query.Close;
    Query.Open;
    CheckEquals(false, Query.Locate('p_begin_work',EncodeTime(8,31,0,0),[]));
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.Test_doCachedLobs;
var
  Query: TZQuery;
  ROQuery: TZReadOnlyQuery;
  BinStreamE: TMemoryStream;
  BinLob: String;
  TempConnection: TZConnection;
  aOptions: TZDataSetOptions;
begin
  TempConnection := nil;
  BinStreamE:=nil;

  Query := CreateQuery;
  ROQuery := CreateReadOnlyQuery;
  aOptions := Query.Options;
  try
    if ProtocolType = protPostgre then
    begin
      TempConnection := TZConnection.Create(nil);
      TempConnection.HostName := Connection.HostName;
      TempConnection.Port     := Connection.Port;
      TempConnection.Database := Connection.Database;
      TempConnection.User     := Connection.User;
      TempConnection.Password := Connection.Password;
      TempConnection.Protocol := Connection.Protocol;
      TempConnection.Catalog  := Connection.Catalog;
      TempConnection.Properties.Text := Connection.Properties.Text;
      TempConnection.Properties.Add('oidasblob=true');
      TempConnection.TransactIsolationLevel := tiReadCommitted;
      TempConnection.AutoCommit := False;    //https://www.postgresql.org/message-id/002701c49d7e%240f059240%24d604460a%40zaphod
      TempConnection.Connect;
      Query.Connection := TempConnection;
      ROQuery.Connection := TempConnection;
      Connection.TransactIsolationLevel:=tiReadCommitted;
    end;
    with Query do
    begin
      Query.Options := aOptions;
      SQL.Text := 'DELETE FROM blob_values where b_id >= '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      ExecSQL;
      Sql.Text := 'INSERT INTO blob_values (b_id) values ('+SysUtils.IntToStr(TEST_ROW_ID-1)+')';
      ExecSQL;

      case ProtocolType of
        protOracle: BinLob := 'b_blob';
        protSQLite: BinLob := 'b_blob';
        else        BinLob := 'b_image';
      end;

      Sql.Text := 'INSERT INTO blob_values (b_id,'+BinLob+')'
        + ' VALUES (:b_id,:b_image)';
      CheckEquals(2, Params.Count);
      Params[0].DataType := ftInteger;
      Params[1].DataType := ftBlob;
      Params[0].AsInteger := TEST_ROW_ID;
      BinStreamE := TMemoryStream.Create;
      BinStreamE.LoadFromFile(TestFilePath('images/horse.jpg'));
      Params[1].LoadFromStream(BinStreamE, ftBlob);
      ExecSQL;
      CheckEquals(1, RowsAffected);
    end;
    TestReadCachedLobs(BinLob, aOptions, BinStreamE, Query);
    TestReadCachedLobs(BinLob, aOptions, BinStreamE, ROQuery);
    Include(aOptions, doCachedLobs);
    TestReadCachedLobs(BinLob, aOptions, BinStreamE, Query);
    TestReadCachedLobs(BinLob, aOptions, BinStreamE, ROQuery);
  finally
    FreeAndNil(BinStreamE);
    Query.SQL.Text := 'DELETE FROM blob_values where b_id >= '+ SysUtils.IntToStr(TEST_ROW_ID-1);
    try
      Query.ExecSQL;
      if Assigned(TempConnection) then
        TempConnection.Commit;
    finally
      Query.Free;
      try
        ROQuery.Free;
      finally
        FreeAndNil(TempConnection);
      end;
    end;
  end;
end;

{**
Runs a test for Datetime locate expressions.
}
procedure TZGenericTestDataSet.TestDateTimeLocateExpression;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM cargo';
    Query.Open;
    CheckEquals(true, Query.Locate('c_date_came',EncodeDateTime(2002,12,19,14,0,0,0),[]));
    CheckEquals(EncodeDateTime(2002,12,19,14,0,0,0), Query.FieldByName('c_date_came').AsDateTime);
    CheckEquals(EncodeDateTime(2002,12,23,0,0,0,0), Query.FieldByName('c_date_out').AsDateTime);
    Query.Close;
    Query.Open;
    CheckEquals(false, Query.Locate('c_date_came',EncodeDateTime(2002,12,19,0,0,0,0),[]));
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestNullDateTimeValues;
var
  Query: TZQuery;
  I, j, ID: Integer;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Could not establish a connection');
  Query := CreateQuery;
  try
    ID := TEST_ROW_ID;
    with Query do
    begin
      Sql.Text := 'select * from date_values';
      Open;
      Append;
      {$IFDEF WITH_TAUTOREFRESHFLAG}
      if (Fields[0].AutoGenerateValue <> arAutoInc) or not Fields[0].ReadOnly then
      {$ENDIF}
        Fields[0].AsInteger := ID;
      Post;
      {$IFDEF WITH_TAUTOREFRESHFLAG}
      if Fields[0].AutoGenerateValue = arAutoInc then
      {$ENDIF}
        ID := Fields[0].AsInteger;
      try
        for i := 1 to Fields.Count-1 do begin
          Check(Fields[i].IsNull, Fields[i].FieldName+' should be null');
          Check(Fields[i].AsString='', Fields[i].FieldName+' should be empty');
        end;
        for j := 0 to 4 do begin //emulate fall into realprepared mode for some drivers
          Close;
          Open;
          Last;
          Check(RecordCount > 0, 'table date_values is not empty');
          for i := 1 to Fields.Count-1 do begin
            Check(Fields[i].IsNull, Fields[i].FieldName+' should be null');
            Check(Fields[i].AsString='', Fields[i].FieldName+' should be empty');
          end;
          Close;
        end;
      finally
        Connection.ExecuteDirect('DELETE FROM date_values where d_id='+ZFastCode.IntToStr(ID));
      end;
      Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestDoubleFloatParams;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM number_values where n_id = '+ SysUtils.IntToStr(TEST_ROW_ID);
      ExecSQL;
      Sql.Text := 'INSERT INTO number_values (n_id,n_float,n_dprecission)'
          + ' VALUES (:n_id,:n_float,:n_real)';
      CheckEquals(3, Params.Count);
      Params[0].DataType := ftInteger;
      Params[1].DataType := ftFloat;
      Params[2].DataType := ftFloat;
      Params[0].AsInteger := TEST_ROW_ID;
      Params[1].AsFloat := 3.14159265358979323846;
      Params[2].AsFloat := 3.14159265358979323846;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      SQL.Text := 'SELECT * FROM number_values where n_id = '+ SysUtils.IntToStr(TEST_ROW_ID);
      CheckEquals(0, Query.Params.Count);

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID, FieldByName('n_id').AsInteger);
      CheckEquals(3.14159265358979323846, FieldByName('n_float').AsFloat,0.00001);
      CheckEquals(3.14159265358979323846, FieldByName('n_dprecission').AsFloat,0.0000000000001);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestNullUnionNull;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT null as col1 FROM people union SELECT null as col1 FROM people';
    Query.Open; //just take care we can open a cursor
    BlankCheck;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.TestClobEmptyString;
var
  Query: TZQuery;
  TextLob: String;
begin
  Query := CreateQuery;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-2);
      ExecSQL;
      if ProtocolType = protOracle then
      begin
        Sql.Text := 'INSERT INTO blob_values (b_id, b_clob)'
          + ' VALUES (:b_id, EMPTY_CLOB())';
        CheckEquals(1, Params.Count);
        Params[0].DataType := ftInteger;
        Params[0].AsInteger := TEST_ROW_ID-2;
        TextLob := 'b_clob';
      end
      else
      begin
        if ProtocolType = protSQLite then
          TextLob := 'b_text'
        else
          TextLob := 'b_text';
        Sql.Text := 'INSERT INTO blob_values (b_id,'+TextLob+')'
          + ' VALUES (:b_id,:b_text)';
        CheckEquals(2, Params.Count);
        ParamByName('b_id').DataType := ftInteger;
        Params[0].AsInteger := TEST_ROW_ID-2;
        {$IFDEF WITH_WIDEMEMO}
        if Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 then
        begin
          ParamByName('b_text').DataType := ftWideMemo;
          Params[1].AsWideString := '';
        end
        else
        {$ENDIF}
        begin
          ParamByName('b_text').DataType := ftMemo;
          Params[1].AsString := '';
        end;
      end;
      ExecSQL;

      CheckEquals(1, RowsAffected);

      SQL.Text := 'SELECT * FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-2);
      CheckEquals(0, Query.Params.Count);

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID-2, FieldByName('b_id').AsInteger);
      CheckEquals(False, FieldByName(TextLob).IsNull, 'Memo is not empty.');
      CheckEquals('', FieldByName(TextLob).AsString, 'Empty but not null String');
      Close;
    end;
  finally
    try
      Query.SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-2);
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  end;
end;

procedure TZGenericTestDataSet.TestLobModes;
const teststring = RawByteString('abcdefghijklmnopqrstuvwxyz');
var
  Query: TZQuery;
  BinStreamA, TextStreamA: TStream;
  BinStreamE, TextStreamE: TMemoryStream;
  TempA: RawByteString;
  {$IFDEF WITH_WIDEMEMO}
  TempU: ZWideString;
  {$ENDIF}

  TextLob, BinLob: String;
  TempConnection: TZConnection;
begin
  TempConnection := nil;
  BinStreamA:=nil;
  BinStreamE:=nil;
  TextStreamA:=nil;
  TextStreamE:=nil;

  Query := CreateQuery;
  try
    if ProtocolType = protPostgre then
    begin
      TempConnection := TZConnection.Create(nil);
      TempConnection.HostName := Connection.HostName;
      TempConnection.Port     := Connection.Port;
      TempConnection.Database := Connection.Database;
      TempConnection.User     := Connection.User;
      TempConnection.Password := Connection.Password;
      TempConnection.Protocol := Connection.Protocol;
      TempConnection.Catalog  := Connection.Catalog;
      TempConnection.Properties.Text := Connection.Properties.Text;
      TempConnection.Properties.Add('oidasblob=true');
      TempConnection.TransactIsolationLevel := tiReadCommitted;
      TempConnection.AutoCommit := False;    //https://www.postgresql.org/message-id/002701c49d7e%240f059240%24d604460a%40zaphod
      TempConnection.Connect;
      Query.Connection := TempConnection;
      Connection.TransactIsolationLevel:=tiReadCommitted;
    end;
    with Query do
    begin
      SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      ExecSQL;
      case ProtocolType of
        protOracle:
          begin
            TextLob := 'b_clob';
            BinLob := 'b_blob';
          end;
        protSQLite:
          begin
            TextLob := 'b_text';
            BinLob := 'b_blob';
          end;
        else
          begin
            TextLob := 'b_text';
            BinLob := 'b_image';
          end;
      end;
      BinStreamE := TMemoryStream.Create;
      BinStreamE.LoadFromFile(TestFilePath('images/horse.jpg'));
      BinStreamE.Position := 0;

      TextStreamE := TMemoryStream.Create;
      {$IFDEF WITH_WIDEMEMO}
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
        TextStreamE.Write(ZWideString(teststring)[1], Length(teststring)*2)
      else
      {$ENDIF}
        TextStreamE.Write(teststring[1], Length(teststring));
      TextStreamE.Position := 0;
      SQL.Text := 'SELECT * FROM blob_values';

      Open;
      Insert;
      FieldByName('b_id').AsInteger := TEST_ROW_ID-1;
      TextStreamA := Query.CreateBlobStream(Query.FieldByName(TextLob), bmWrite);
      {$IFDEF WITH_WIDEMEMO}
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
        TextStreamA.Write(ZWideString(teststring)[1], Length(teststring)*2)
      else
      {$ENDIF}
        TextStreamA.Write(teststring[1],length(teststring));
      TextStreamA.Free;
      BinStreamA := Query.CreateBlobStream(Query.FieldByName(BinLob), bmWrite);
      TMemoryStream(BinStreamA).LoadFromFile(TestFilePath('images/horse.jpg'));
      BinStreamA.Free;
      Post;

      SQL.Text := 'SELECT * FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      Open;
      TextStreamA := Query.CreateBlobStream(Query.FieldByName(TextLob), bmRead);
      BinStreamA := Query.CreateBlobStream(Query.FieldByName(BinLob), bmRead);

      CheckEquals(TextStreamE, TextStreamA, 'Text-Stream');
      CheckEquals(BinStreamE, BinStreamA, 'Bin-Stream');

      FreeAndNil(TextStreamA);
      FreeAndNil(BinStreamA);

      Edit;

      TextStreamA := Query.CreateBlobStream(Query.FieldByName(TextLob), bmReadWrite);
      BinStreamA := Query.CreateBlobStream(Query.FieldByName(BinLob), bmReadWrite);

      {$IFDEF WITH_WIDEMEMO}
      TextStreamA.Position := 0;
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
      begin
        SetLength(TempU, Length(TestString));
        TextStreamA.Read(PWideChar(TempU)^, Length(teststring)*2);
        CheckEquals(TempU, ZWideString(TestString));
      end
      else
      {$ENDIF}
      begin
        SetLength(TempA, Length(TestString));
        TextStreamA.Read(PAnsiChar(TempA)^, Length(teststring));
        CheckEquals(TempA, TestString);
      end;
      CheckEquals(BinStreamE, BinStreamA);
      CheckEquals(TextStreamE, TextStreamA);

      TextStreamE.Size := TextStreamE.Size div 2;
      BinStreamE.Size := 1024;

      TMemoryStream(TextStreamA).LoadFromStream(TextStreamE);
      TMemoryStream(BinStreamA).LoadFromStream(BinStreamE);
      FreeAndNil(TextStreamA);
      FreeAndnil(BinStreamA);

      Post;

      Close;
      Open;

      TextStreamA := Query.CreateBlobStream(Query.FieldByName(TextLob), bmRead);
      BinStreamA := Query.CreateBlobStream(Query.FieldByName(BinLob), bmRead);

      CheckEquals(TextStreamE, TextStreamA, 'Text-Stream');
      CheckEquals(BinStreamE, BinStreamA, 'Bin-Stream');

      {$IFDEF WITH_WIDEMEMO}
      if ( Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16 ) then
      begin
        SetLength(TempU, TextStreamA.Size div 2);
        TextStreamE.Read(PWideChar(TempU)^, TextStreamA.Size);
        CheckEquals(Copy(ZWideString(TestString), 1, Length(teststring) div 2), TempU);
      end
      else
      {$ENDIF}
      begin
        SetLength(TempA, TextStreamA.Size);
        TextStreamE.Read(PAnsiChar(TempA)^, TextStreamA.Size);
        CheckEquals(Copy(TestString, 1, Length(teststring) div 2), TempA);
      end;

      FreeAndNil(TextStreamA);
      FreeAndNil(BinStreamA);
      Close;
    end;
  finally
    FreeAndNil(BinStreamA);
    FreeAndNil(BinStreamE);
    FreeAndNil(TextStreamA);
    FreeAndNil(TextStreamE);
    Query.SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
    try
      Query.ExecSQL;
      if Assigned(TempConnection) then
        TempConnection.Commit;
    finally
      Query.Free;
      FreeAndNil(TempConnection);
    end;
  end;
end;

procedure TZGenericTestDataSet.TestSpaced_Names;
var
  Query: TZQuery;

  function GetNonQuotedAlias(const Value: String): String;
  begin
    case ConnectionConfig.Provider of
      spPostgreSQL:
        Result := LowerCase(Value);
      spOracle, spIB_FB:
        Result := UpperCase(Value);
      else
        Result := Value;
    end;
  end;
begin
  Query := CreateQuery;
  Connection.Connect;
  try
    { test generic field names }
    Query.SQL.Text := 'select * from '+Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Spaced Names');
    Query.Open;
    CheckEquals('Cs Data1', Query.Fields[1].DisplayName);
    CheckEquals('cs data2', Query.Fields[2].DisplayName);
    CheckEquals('cS data3', Query.Fields[3].DisplayName);
    Query.Insert;
    Query.Fields[0].AsInteger := TEST_ROW_ID;
    Query.Fields[1].AsInteger := TEST_ROW_ID+1;
    Query.Fields[2].AsInteger := TEST_ROW_ID+2;
    Query.Fields[3].AsInteger := TEST_ROW_ID+3;
    Query.Post;
    Query.Insert;
    Query.FieldByName('cs_id').AsInteger := TEST_ROW_ID+1;
    Query.FieldByName('Cs Data1').AsInteger := TEST_ROW_ID+1;
    Query.FieldByName('cs data2').AsInteger := TEST_ROW_ID+2;
    Query.FieldByName('cS data3').AsInteger := TEST_ROW_ID+3;
    Query.Post;
    { test alias names without spaces and quoting rules }
    Query.SQL.Text := 'select cs_id, '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Cs Data1')+' as CsData1, '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cs data2')+' as csdata2, '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cS data3')+' as cSdata3 from '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Spaced Names');
    Query.Open;
    CheckEquals(GetNonQuotedAlias('CsData1'), Query.Fields[1].DisplayName);
    CheckEquals(GetNonQuotedAlias('csdata2'), Query.Fields[2].DisplayName);
    CheckEquals(GetNonQuotedAlias('cSdata3'), Query.Fields[3].DisplayName);
    Query.Insert;
    Query.FieldByName('cs_id').AsInteger := TEST_ROW_ID+2;
    Query.FieldByName(GetNonQuotedAlias('CsData1')).AsInteger := TEST_ROW_ID+1;
    Query.FieldByName(GetNonQuotedAlias('csdata2')).AsInteger := TEST_ROW_ID+2;
    Query.FieldByName(GetNonQuotedAlias('cSdata3')).AsInteger := TEST_ROW_ID+3;
    Query.Post;
    { test alias names without spaces but with quoting rules }
    Query.SQL.Text := 'select cs_id, '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Cs Data1')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('CsData1')+', '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cs data2')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('csdata2')+', '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cS data3')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cSdata3')+
       ' from '+Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Spaced Names');
    Query.Open;
    CheckEquals('CsData1', Query.Fields[1].DisplayName);
    CheckEquals('csdata2', Query.Fields[2].DisplayName);
    CheckEquals('cSdata3', Query.Fields[3].DisplayName);
    Query.Insert;
    Query.FieldByName('cs_id').AsInteger := TEST_ROW_ID+3;
    Query.FieldByName('CsData1').AsInteger := TEST_ROW_ID+1;
    Query.FieldByName('csdata2').AsInteger := TEST_ROW_ID+2;
    Query.FieldByName('cSdata3').AsInteger := TEST_ROW_ID+3;
    Query.Post;
    { test alias names with spaces and quoting rules }
    Query.SQL.Text := 'select cs_id, '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Cs Data1')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Cs  Data1')+', '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cs data2')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cs  data2')+', '+
      Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cS data3')+' as '+
        Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('cS  data3')+
       ' from '+Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Spaced Names');
    Query.Open;
    CheckEquals('Cs  Data1', Query.Fields[1].DisplayName);
    CheckEquals('cs  data2', Query.Fields[2].DisplayName);
    CheckEquals('cS  data3', Query.Fields[3].DisplayName);
    Query.Insert;
    Query.FieldByName('cs_id').AsInteger := TEST_ROW_ID+4;
    Query.FieldByName('Cs  Data1').AsInteger := TEST_ROW_ID+1;
    Query.FieldByName('cs  data2').AsInteger := TEST_ROW_ID+2;
    Query.FieldByName('cS  data3').AsInteger := TEST_ROW_ID+3;
    Query.Post;
  finally
    Query.SQL.Text := 'delete from '+Connection.DbcConnection.GetMetadata.GetIdentifierConvertor.Quote('Spaced Names')+
      ' where cs_id > '+SysUtils.IntToStr(TEST_ROW_ID-1);
    Query.ExecSQL;
    Query.Free;
  end;
end;

procedure TZGenericTestDataSet.RunDefineFields;
var
  Bool: Boolean;
begin
  DefineFields(FQuery, FFieldList, Bool, FTokenizer);
end;

procedure TZGenericTestDataSet.RunDefineSortedFields;
var
  Bool: Boolean;
  CompareKinds: TComparisonKindArray;
  Fields: TObjectDynArray;
begin
  DefineSortedFields(FQuery, FFieldList, FTokenizer, Fields, CompareKinds, Bool);
end;

procedure TZGenericTestDataSet.TestDefineFields;

  procedure CheckFieldList(const FieldList: string; const Expect: array of TField);
  var
    Bool: Boolean;
    Fields: TObjectDynArray;
    i: Integer;
  begin
    Fields := DefineFields(FQuery, FieldList, Bool, FTokenizer);
    CheckEquals(Length(Expect), Length(Fields), 'FieldList "' + FieldList + '" - item count');
    for i := Low(Fields) to High(Fields) do
      CheckSame(Expect[i], Fields[i], 'FieldList "' + FieldList + '" - item #' + ZFastCode.IntToStr(i));
  end;

  procedure CheckExceptionRaised(const FieldList: string; Expect: ExceptClass; const ExpectMsg: string = '');
  begin
    FFieldList := FieldList;
    CheckException(RunDefineFields, Expect, ExpectMsg, 'FieldList "' + FieldList + '"');
  end;

var
  F1, F2: TStringField;
begin
  FQuery := TZQuery.Create(nil);
  try
    F1 := TStringField.Create(FQuery);
    F1.FieldName := 'Field1';
    FQuery.Fields.Add(F1);
    F2 := TStringField.Create(FQuery);
    F2.FieldName := 'Field2';
    FQuery.Fields.Add(F2);

    CheckFieldList('', []);
    CheckFieldList('Field1,Field2', [F1, F2]);
    CheckFieldList('Field1;Field2', [F1, F2]);
    CheckFieldList('Field1 Field2', [F1, F2]); // this works currently but not recommended
    CheckFieldList('"Field1", "Field2"', [F1, F2]);
    CheckFieldList('1,0', [F2, F1]);

    CheckExceptionRaised('-1,0', EZDatabaseError, Format(SIncorrectSymbol, ['-']));
    CheckExceptionRaised('Field1/Field2', EZDatabaseError, Format(SIncorrectSymbol, ['/']));
    CheckExceptionRaised('1,12345', EZDatabaseError, Format(SFieldNotFound2, [12345]));
    CheckExceptionRaised('foo,bar', EDatabaseError);
    CheckExceptionRaised('Field1,"not exists",Field2', EDatabaseError);
  finally
    FQuery.Free;
  end;
end;

procedure TZGenericTestDataSet.TestDefineSortedFields;

  procedure CheckFieldList(const FieldList: string; const ExpectFields: array of TField; const ExpectCompareKinds: array of TComparisonKind);
  var
    Bool: Boolean;
    Fields: TObjectDynArray;
    CompareKinds: TComparisonKindArray;
    i: Integer;
  begin
    DefineSortedFields(FQuery, FieldList, FTokenizer, Fields, CompareKinds, Bool);
    CheckEquals(Length(ExpectFields), Length(Fields), 'FieldList "' + FieldList + '" - item count');
    CheckEquals(Length(ExpectCompareKinds), Length(CompareKinds), 'FieldList "' + FieldList + '" - item count');
    for i := Low(Fields) to High(Fields) do
    begin
      CheckSame(ExpectFields[i], Fields[i], 'FieldList "' + FieldList + '" - item #' + ZFastCode.IntToStr(i));
      CheckEquals(Integer(ExpectCompareKinds[i]), Integer(CompareKinds[i]), 'FieldList "' + FieldList + '" - item #' + ZFastCode.IntToStr(i));
    end;
  end;

  procedure CheckExceptionRaised(const FieldList: string; Expect: ExceptClass; const ExpectMsg: string = '');
  begin
    FFieldList := FieldList;
    CheckException(RunDefineSortedFields, Expect, ExpectMsg, 'FieldList "' + FieldList + '"');
  end;

var
  F1, F2, F3: TStringField;
begin
  FQuery := TZQuery.Create(nil);
  try
    F1 := TStringField.Create(FQuery);
    F1.FieldName := 'Field1';
    FQuery.Fields.Add(F1);
    F2 := TStringField.Create(FQuery);
    F2.FieldName := 'Field2';
    FQuery.Fields.Add(F2);
    F3 := TStringField.Create(FQuery);
    F3.FieldName := 'Desc';
    FQuery.Fields.Add(F3);

    CheckFieldList('', [], []);
    CheckFieldList('Field1,Field2', [F1, F2], [ckAscending, ckAscending]);
    CheckFieldList('Field1;Field2', [F1, F2], [ckAscending, ckAscending]);
    CheckFieldList('"Field1", "Field2"', [F1, F2], [ckAscending, ckAscending]);
    CheckFieldList('1,0', [F2, F1], [ckAscending, ckAscending]);

    CheckFieldList('Field1 desc, Field2 desc', [F1, F2], [ckDescending, ckDescending]);
    CheckFieldList('Field1 desc, 1 asc', [F1, F2], [ckDescending, ckAscending]);
    CheckFieldList('Field1 desc, Field2 desc, Desc', [F1, F2, F3], [ckDescending, ckDescending, ckAscending]);
    CheckFieldList('Field1 desc, Field2 desc, Desc asc', [F1, F2, F3], [ckDescending, ckDescending, ckAscending]);
    CheckFieldList('Field1 desc  Field2 desc Desc', [F1, F2, F3], [ckDescending, ckDescending, ckAscending]);

    CheckExceptionRaised('-1,0', EZDatabaseError, Format(SIncorrectSymbol, ['-']));
    CheckExceptionRaised('Field1/Field2', EZDatabaseError, Format(SIncorrectSymbol, ['/']));
    CheckExceptionRaised('1,12345', EZDatabaseError, Format(SFieldNotFound2, [12345]));
    CheckExceptionRaised('foo,bar', EDatabaseError);
    CheckExceptionRaised('Field1,"not exists",Field2', EDatabaseError);

    CheckExceptionRaised('Field1 Field2', EZDatabaseError, Format(SIncorrectSymbol, ['Field2']));
    CheckExceptionRaised('Field1 desc,Field2 foo', EZDatabaseError, Format(SIncorrectSymbol, ['foo']));
  finally
    FQuery.Free;
  end;
end;

{** some people reporting problems that params are not found..

  http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
  http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=98333
}
procedure TZGenericTestDataSet.TestKeyWordParams;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  Query.Connection.Connect;
  try
    Query.SQL.Text := 'select IsNull(B.X||B.y, '+QuotedStr('')+') as X_Y_Z from "My_Table" as B '+
      'where 1=1 and :user_ID = B.x and ((B.nazwa iLike :nazwaFiltr) or (B.skrot iLike :nazwaFiltr))';
    CheckEquals(2, Query.Params.Count);
    CheckEquals('user_ID', Query.Params[0].Name);
    CheckEquals('nazwaFiltr', Query.Params[1].Name);

    Query.SQL.Text := 'SELECT * FROM sys_users WHERE login = :login';
    CheckEquals(1, Query.Params.Count);
    CheckEquals('login', Query.Params[0].Name);

    Query.SQL.Text := 'SELECT * FROM users WHERE username = :username;';
    CheckEquals(1, Query.Params.Count);
    CheckEquals('username', Query.Params[0].Name);

    Query.SQL.Text := 'SELECT * FROM sys_users WHERE login = :user_id';
    CheckEquals(1, Query.Params.Count);
    CheckEquals('user_id', Query.Params[0].Name);
  finally
    Query.Free;
  end;
end;

type
  THackQuery = class(TZAbstractRODataSet);
// Test if fields got the right types - the simplest method is to try
// assigning boundary values (range checking must be enabled)
procedure TZGenericTestDataSet.TestInsertNumbers;
var
  Query: TZQuery;
  i: Integer;
  Msg: string;

  procedure TestSetValue(Value: Integer); overload;
  begin
    Msg := Format('set value "%d" to field "%s"', [Value, Query.Fields[i].FieldName]);
    Query.Edit;
    Query.Fields[i].AsInteger := Value;
    // This has nothing to do with the test - Sybase and MSSQL just don't allow
    // setting bit fields to null.
    if ProtocolType in [protFreeTDS, protSybase, protMSSQL]
    then Query.FieldByName('stBoolean').AsBoolean := true;
    Query.Post;
    CheckEquals(Value, Query.Fields[i].AsInteger);
  end;

  procedure TestSetValue(Value: Int64); overload;
  var ActValue: Int64;
  begin
    Msg := Format('set value "%d" to field "%s"', [Value, Query.Fields[i].FieldName]);
    Query.Edit;
    Query.Fields[i].{$IFDEF TFIELD_HAS_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF} := Value;
    // This has nothing to do with the test - Sybase and MSSQL just don't allow
    // setting bit fields to null.
    if ProtocolType in [protFreeTDS, protSybase]
    then Query.FieldByName('stBoolean').AsBoolean := true;
    Query.Post;
    // D7 calls _VarToInteger instead of _VarToInt64 if used directly in CheckEquals so we need temp variable
    ActValue := Query.Fields[i].{$IFDEF TFIELD_HAS_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF};
    CheckEquals(Value, ActValue);
  end;

begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM high_load';
    Query.Open;
    try
      I := 0;
      TestSetValue(TEST_ROW_ID);
      for i := 1 to Query.Fields.Count - 1 do
        case Query.Fields[i].DataType of
          {$IFDEF WITH_FTSHORTINT}
          ftShortint:
            begin
              TestSetValue(Low(Shortint));
              TestSetValue(High(Shortint));
            end;
          {$ENDIF}
          {$IFDEF WITH_FTBYTE}
          ftByte:
            begin
              TestSetValue(Low(Byte));
              TestSetValue(High(Byte));
            end;
          {$ENDIF}
          ftSmallint:
            begin
            if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stSmall then begin
              TestSetValue(Low(SmallInt));
              TestSetValue(High(SmallInt));
            end else if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stByte then begin
              TestSetValue(Low(ShortInt));
              TestSetValue(High(ShortInt));
            end;
            end;
          ftWord:
            if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stWord then begin
              TestSetValue(Low(Word));
              TestSetValue(High(Word));
            end else if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stByte then begin
              TestSetValue(Low(Byte));
              TestSetValue(High(Byte));
            end;
          ftInteger:
            begin
              TestSetValue(Low(Integer));
              TestSetValue(High(Integer));
            end;
          {$IFDEF WITH_FTLONGWORD}
          ftLongWord:
            begin
              TestSetValue(Low(LongWord));
              TestSetValue(High(LongWord));
            end;
          {$ENDIF}
          ftLargeint:
            if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stLong then begin
              TestSetValue(Low(Int64));
              TestSetValue(High(Int64));
            end else if THackQuery(Query).ResultSet.GetMetadata.GetColumnType(FirstDbcIndex +1) = stLongWord then begin
              TestSetValue(Low(Cardinal));
              TestSetValue(High(Cardinal));
            end;
        end;
    except on E: Exception do
      Fail(Msg + ' raised exception ' + E.Message);
    end;
  finally
    Query.SQL.Text := 'delete from high_load';
    try
      Query.ExecSQL;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TZGenericTestDataSet.TestVeryLargeBlobs;
{$IFDEF MSWINDOWS}
const teststring: ZWideString = '123456'+Chr(192)+Chr(193)+Chr(194)+Chr(195);
{$ELSE}
const teststring: ZWideString = '123456\##/';
{$ENDIF}

var
  Query: TZQuery;
  BinStreamE,BinStreamA,TextStream: TMemoryStream;
  s:  RawByteString;
  TextLob, BinLob: String;
  W: ZWideString;
  TempConnection: TZConnection;
  ConSettings: PZConSettings;

  function WideDupeString(const AText: ZWideString; ACount: Integer): ZWideString;
  var i,l : integer;
  begin
    result:='';
    if aCount>=0 then
    begin
      l:=length(atext);
      SetLength(Result,aCount*l);
      for i:=0 to ACount-1 do
        move(atext[1],Result[l*i+1],l shl 1);
    end;
  end;
begin
  TempConnection := nil;
  BinStreamE:=nil;
  BinStreamA:=nil;
  TextStream:=nil;

  Query := CreateQuery;
  try
    if ProtocolType = protPostgre then
    begin
      TempConnection := TZConnection.Create(nil);
      TempConnection.HostName := Connection.HostName;
      TempConnection.Port     := Connection.Port;
      TempConnection.Database := Connection.Database;
      TempConnection.User     := Connection.User;
      TempConnection.Password := Connection.Password;
      TempConnection.Protocol := Connection.Protocol;
      TempConnection.Catalog  := Connection.Catalog;
      TempConnection.Properties.Text := Connection.Properties.Text;
      TempConnection.Properties.Add('oidasblob=true');
      TempConnection.TransactIsolationLevel := tiReadCommitted;
      TempConnection.AutoCommit := False;    //https://www.postgresql.org/message-id/002701c49d7e%240f059240%24d604460a%40zaphod
      TempConnection.Connect;
      Query.Connection := TempConnection;
      Connection.TransactIsolationLevel:=tiReadCommitted;
    end;
    if not Query.Connection.Connected then
      Query.Connection.Connect;

    ConSettings := Query.Connection.DbcConnection.GetConSettings;
    with Query do
    begin
      SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      ExecSQL;
      case ProtocolType of
        protOracle:
          begin
            TextLob := 'b_clob';
            BinLob := 'b_blob';
          end;
        protSQLite:
          begin
            TextLob := 'b_text';
            BinLob := 'b_blob';
          end;
        else
          begin
            TextLob := 'b_text';
            BinLob := 'b_image';
          end;
      end;
      Sql.Text := 'INSERT INTO blob_values (b_id,'+TextLob+','+BinLob+')'
        + ' VALUES (:b_id,:b_text,:b_image)';
      CheckEquals(3, Params.Count);
      Params[0].DataType := ftInteger;
      Params[1].DataType := ftMemo;
      Params[2].DataType := ftBlob;
      Params[0].AsInteger := TEST_ROW_ID-1;
      TextStream := TMemoryStream.Create;
      W := WideDupeString(teststring,6000);
      {$IFNDEF UNICODE}
      s:= GetDBTestString(W, ConSettings);
      {$ELSE}
      if ConSettings.AutoEncode or (ConSettings.ClientCodePage.Encoding = ceUTF16)
      then S := ZUnicodeToRaw(W, ConSettings.CTRL_CP)
      else S := ZUnicodeToRaw(W, ConSettings.ClientCodePage.CP);
      {$ENDIF}

      TextStream.Write(s[1],length(s));
      s := '';
      Params[1].LoadFromStream(TextStream, ftMemo);
      FreeAndNil(TextStream);
      BinStreamE := TMemoryStream.Create;
      BinStreamE.LoadFromFile(TestFilePath('images/horse.jpg'));
      setlength(s,BinStreamE.Size);
      BinStreamE.Read(s[1],length(s));
      s := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}DupeString(s, 10);
      CheckEquals(BinStreamE.Size * 10, length(s), 'Length of DupeString result');
      S := '';
      BinStreamE.Position := 0;
      // this operation is useless - s is empty so length(s) is zero - even worse - it generates a range overflow because s[1] doesn't exist.
      //BinStreamE.Write(s[1], length(s));
      Params[2].LoadFromStream(BinStreamE, ftBlob);
      ExecSQL;

      CheckEquals(1, RowsAffected);

      SQL.Text := 'SELECT * FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
      CheckEquals(0, Query.Params.Count);

      Open;
      CheckEquals(1, RecordCount);
      CheckEquals(False, IsEmpty);
      CheckEquals(TEST_ROW_ID-1, FieldByName('b_id').AsInteger);
      TextStream := TMemoryStream.Create;
      (FieldByName(TextLob) as TBlobField).SaveToStream(TextStream);
      CheckEquals(W, TextStream, Connection.DbcConnection.GetConSettings, 'Text-Stream');
      FreeAndNil(TextStream);
      BinStreamA := TMemoryStream.Create;
      BinStreamA.Position:=0;
      (FieldByName(BinLob) as TBlobField).SaveToStream(BinStreamA);
      CheckEquals(BinStreamE, BinStreamA, 'Binary Stream');
      Close;
    end;
  finally
    FreeAndNil(BinStreamE);
    FreeAndNil(BinStreamA);
    FreeAndNil(TextStream);
    Query.SQL.Text := 'DELETE FROM blob_values where b_id = '+ SysUtils.IntToStr(TEST_ROW_ID-1);
    try
      Query.ExecSQL;
      if Assigned(TempConnection) then
        TempConnection.Commit;
    finally
      Query.Free;
      FreeAndNil(TempConnection);
    end;
  end;
end;

procedure TZGenericTestDataSet.TestEmptyMemoAfterFullMemo;
var
  Query: TZQuery;
  TxtValue: String;
  ValueIsNull: Boolean;
begin
  Query := CreateQuery;
  try
    try
      Query.Connection.StartTransaction;
      try
        if ProtocolType = protOracle
        then Query.SQL.Text := 'insert into blob_values (b_id, b_clob) values (:id, :text)'
        else Query.SQL.Text := 'insert into blob_values (b_id, b_text) values (:id, :text)';
        Query.ParamByName('id').DataType := ftInteger;
        Query.ParamByName('text').DataType := ftMemo;

        Query.ParamByName('id').AsInteger := 2000;
        Query.ParamByName('text').AsMemo := '/* abc */';
        Query.ExecSQL;
        Query.ParamByName('id').AsInteger := 2001;
        Query.ParamByName('text').AsMemo := '';
        Query.ExecSQL;
        Connection.Commit;
      except
        Connection.Rollback;
      end;

      Query.SQL.Text := 'select * from blob_values where b_id = 2001';
      Query.Open;
      try
        if ProtocolType = protOracle then begin
          TxtValue := Query.FieldByName('b_clob').AsString;
          ValueIsNull := Query.FieldByName('b_clob').IsNull;
        end else begin
          TxtValue := Query.FieldByName('b_text').AsString;
          ValueIsNull := Query.FieldByName('b_text').IsNull;
        end;
      finally
        Query.Close;
      end;

      CheckEquals('', TxtValue, 'Tried to insert an empty clob from a paramater, after the parameter has been used before.');
      CheckEquals(false, ValueIsNull, 'Tried to insert an empty clob from a paramater, after the parameter has been used before. IsNull should return false.');
    finally
      Connection.ExecuteDirect('delete from blob_values where b_id in (2000, 2001)');
    end;
  finally
    FreeAndNil(Query);
  end;
end;

initialization
  RegisterTest('component',TZGenericTestDataSet.Suite);
end.
