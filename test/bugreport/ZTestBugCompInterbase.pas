{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Interbase Component Bug Reports    }
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

unit ZTestBugCompInterbase;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZDbcIntfs, ZSqlTestCase, ZCompatibility
  {$IFNDEF LINUX}
    {$IFDEF WITH_VCL_PREFIX}
    , Vcl.DBCtrls
    {$ELSE}
    , DBCtrls
    {$ENDIF}
  {$ENDIF};
type

  {** Implements a bug report test case for Interbase components. }
  ZTestCompInterbaseBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test750912;
    procedure Test789879D;
    procedure Test833489;
    procedure Test841559;
    procedure Test843655;
    procedure Test864622;
    procedure Test886194;
    procedure Test886854;
    procedure Test897631;
    procedure Test909181;
    procedure Test984305;
    procedure Test1021705;
    procedure Test_Decimal;
    procedure Test_Ticket54;
    procedure Test_Ticket63;
    procedure Test_Ticket67;
    procedure Test_Ticket228;
    procedure Test_SF249;
    procedure Test_SF287;
    procedure TestTicket363;
    procedure TestTicket376;
    procedure TestExecutBlockReturning_WithoutParamCheck;
    procedure TestExecutBlockReturning_WithParamCheck;
    procedure TestProcAbtest_WithParamCheck;
    procedure TestProcAbtest_WithoutParamCheck;
  end;

  ZTestCompInterbaseBugReportMBCs = class(TZAbstractCompSQLTestCaseMBCs)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test_Param_LoadFromStream_StringStream_ftBlob;
    procedure Test_Param_LoadFromStream_StringStream_ftMemo;
    procedure Test_Mantis214;
  end;
implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestCase, ZTestConsts, ZSqlUpdate, ZEncoding;

{ ZTestCompInterbaseBugReport }

function ZTestCompInterbaseBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_interbase;
end;

{**
   Test for Bug#1021705 - problem start transaction in non autocommit mode
}
procedure ZTestCompInterbaseBugReport.Test1021705;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'SELECT * FROM TABLE1021705';
    Query.OPEN;

    Query.Append;
    Query.FieldByName('ID').AsFloat := 1;
    Query.FieldByName('FLD1').AsFloat := 34257.346;
    Query.FieldByName('FLD2').AsFloat := 2.387;
    Query.Post;
    Query.Close;

    Query.Open;
    CheckEquals(1, Query.FieldByName('ID').AsInteger);
    CheckEquals(34257.346, Query.FieldByName('FLD1').AsFloat, 0.0001);
    CheckEquals(2.387, Query.FieldByName('FLD2').AsFloat, 0.0001);
    Query.Delete;
    Query.Close;

    try
      Query.Append;
      Query.FieldByName('ID').AsFloat := 1;
      Query.FieldByName('FLD1').AsFloat := 2.387;
      Query.FieldByName('FLD2').AsFloat := 34257.346;
      Query.Post;
      Fail('Inserted wrong value 34257.346 into TABLE1021705.FLD2 ');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

  finally
    Query.Free;
  end;
end;

{
  Rounding issues:
  http://zeos.firmos.at/viewtopic.php?t=3589
  http://zeos.firmos.at/viewtopic.php?t=3725
}
procedure ZTestCompInterbaseBugReport.Test_Decimal;
const
  Row1_Num2 = 541.77;
  Row1_Num3 = 541.777;
  Row1_Num4 = 541.7777;
  Row1_Num5 = 541.77777;
  Row1_Num6 = 541.777777;
  Row1_Num7 = 541.7777777;
  Row1_Num8 = 541.77777777;
  Row1_Num9 = 541.77777777;

  Row2_Num2 = 541.74;
  Row2_Num3 = 541.774;
  Row2_Num4 = 541.7774;
  Row2_Num5 = 541.77774;
  Row2_Num6 = 541.777774;
  Row2_Num7 = 541.7777774;
  Row2_Num8 = 541.77777774;

  Row3_Num2 = 23.45;
  Row3_Num3 = 23.445;
  Row3_Num4 = 23.4445;
  Row3_Num5 = 23.44445;
  Row3_Num6 = 23.444445;
  Row3_Num7 = 23.4444445;
  Row3_Num8 = 23.44444445;
var
  Table: TZTable;
begin
  if SkipForReason(srClosedBug) then Exit;

  Table := CreateTable;
  try
    // Query.RequestLive := True;
    Table.TableName := 'TEST_DECIMAL';
    Table.Open;

    Table.Append;
    Table.FieldByName('ID').AsInteger := 1;
    Table.FieldByName('NUM_2').AsString := FloatToStr(Row1_Num2);
    Table.FieldByName('NUM_3').AsString := FloatToStr(Row1_Num3);
    Table.FieldByName('NUM_4').AsString := FloatToStr(Row1_Num4);
    Table.FieldByName('NUM_5').AsString := FloatToStr(Row1_Num5);
    Table.FieldByName('NUM_6').AsString := FloatToStr(Row1_Num6);
    Table.FieldByName('NUM_7').AsString := FloatToStr(Row1_Num7);
    Table.FieldByName('NUM_8').AsString := FloatToStr(Row1_Num8);
    Table.Post;

    Table.Append;
    Table.FieldByName('ID').AsInteger := 2;
    Table.FieldByName('NUM_2').AsString := FloatToStr(Row2_Num2);
    Table.FieldByName('NUM_3').AsString := FloatToStr(Row2_Num3);
    Table.FieldByName('NUM_4').AsString := FloatToStr(Row2_Num4);
    Table.FieldByName('NUM_5').AsString := FloatToStr(Row2_Num5);
    Table.FieldByName('NUM_6').AsString := FloatToStr(Row2_Num6);
    Table.FieldByName('NUM_7').AsString := FloatToStr(Row2_Num7);
    Table.FieldByName('NUM_8').AsString := FloatToStr(Row2_Num8);
    Table.Post;

    Table.Append;
    Table.FieldByName('ID').AsInteger := 3;
    Table.FieldByName('NUM_2').AsString := FloatToStr(Row3_Num2);
    Table.FieldByName('NUM_3').AsString := FloatToStr(Row3_Num3);
    Table.FieldByName('NUM_4').AsString := FloatToStr(Row3_Num4);
    Table.FieldByName('NUM_5').AsString := FloatToStr(Row3_Num5);
    Table.FieldByName('NUM_6').AsString := FloatToStr(Row3_Num6);
    Table.FieldByName('NUM_7').AsString := FloatToStr(Row3_Num7);
    Table.FieldByName('NUM_8').AsString := FloatToStr(Row3_Num8);
    Table.Post;

    Table.Append; //now let's use oversized values and see what happens
    Table.FieldByName('ID').AsInteger := 4;
    Table.FieldByName('NUM_2').AsString := FloatToStr(Row1_Num3);
    Table.FieldByName('NUM_3').AsString := FloatToStr(Row1_Num4);
    Table.FieldByName('NUM_4').AsString := FloatToStr(Row1_Num5);
    Table.FieldByName('NUM_5').AsString := FloatToStr(Row1_Num6);
    Table.FieldByName('NUM_6').AsString := FloatToStr(Row1_Num7);
    Table.FieldByName('NUM_7').AsString := FloatToStr(Row1_Num8);
    Table.FieldByName('NUM_8').AsString := FloatToStr(Row1_Num9);
    Table.Post;

    Table.Close;


    Table.Open;

    CheckEquals(1, Table.FieldByName('ID').AsInteger);
    CheckEquals(FloatToStr(Row1_Num2), Table.FieldByName('NUM_2').AsString);
    CheckEquals(FloatToStr(Row1_Num3), Table.FieldByName('NUM_3').AsString);
    CheckEquals(FloatToStr(Row1_Num4), Table.FieldByName('NUM_4').AsString);
    CheckEquals(FloatToStr(Row1_Num5), Table.FieldByName('NUM_5').AsString);
    CheckEquals(FloatToStr(Row1_Num6), Table.FieldByName('NUM_6').AsString);
    CheckEquals(FloatToStr(Row1_Num7), Table.FieldByName('NUM_7').AsString);
    CheckEquals(FloatToStr(Row1_Num8), Table.FieldByName('NUM_8').AsString);

    Table.Next;

    CheckEquals(2, Table.FieldByName('ID').AsInteger);
    CheckEquals(FloatToStr(Row2_Num2), Table.FieldByName('NUM_2').AsString);
    CheckEquals(FloatToStr(Row2_Num3), Table.FieldByName('NUM_3').AsString);
    CheckEquals(FloatToStr(Row2_Num4), Table.FieldByName('NUM_4').AsString);
    CheckEquals(FloatToStr(Row2_Num5), Table.FieldByName('NUM_5').AsString);
    CheckEquals(FloatToStr(Row2_Num6), Table.FieldByName('NUM_6').AsString);
    CheckEquals(FloatToStr(Row2_Num7), Table.FieldByName('NUM_7').AsString);
    CheckEquals(FloatToStr(Row2_Num8), Table.FieldByName('NUM_8').AsString);

    Table.Next;

    CheckEquals(3, Table.FieldByName('ID').AsInteger);
    CheckEquals(FloatToStr(Row3_Num2), Table.FieldByName('NUM_2').AsString);
    CheckEquals(FloatToStr(Row3_Num3), Table.FieldByName('NUM_3').AsString);
    CheckEquals(FloatToStr(Row3_Num4), Table.FieldByName('NUM_4').AsString);
    CheckEquals(FloatToStr(Row3_Num5), Table.FieldByName('NUM_5').AsString);
    CheckEquals(FloatToStr(Row3_Num6), Table.FieldByName('NUM_6').AsString);
    CheckEquals(FloatToStr(Row3_Num7), Table.FieldByName('NUM_7').AsString);
    CheckEquals(FloatToStr(Row3_Num8), Table.FieldByName('NUM_8').AsString);

    Table.Next;

    CheckEquals(4, Table.FieldByName('ID').AsInteger);

    { rounding has no stable effect. random succes so let's forgett this checks
    fix it!!
    CheckEquals(FloatToStr(Row1_Num2+0.01), Table.FieldByName('NUM_2').AsString);
    CheckEquals(FloatToStr(Row1_Num3+0.001), Table.FieldByName('NUM_3').AsString);
    CheckEquals(FloatToStr(Row1_Num4+0.0001), Table.FieldByName('NUM_4').AsString);
    CheckEquals(FloatToStr(Row1_Num5+0.00001), Table.FieldByName('NUM_5').AsString);
    CheckEquals(FloatToStr(Row1_Num6+0.000001), Table.FieldByName('NUM_6').AsString);
    CheckEquals(FloatToStr(Row1_Num7+0.0000001), Table.FieldByName('NUM_7').AsString);
    CheckEquals(FloatToStr(Row1_Num8+0.00000001), Table.FieldByName('NUM_8').AsString);}

  finally
    try
      Table.Delete;
      Table.Delete;
      Table.Delete;
      Table.Delete;
    except
      // Do nothing here -> just have a clean table
    end;
    Table.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test750912;
{$IFNDEF LINUX}
var
  Query: TZQuery;
  ROQuery: TZReadOnlyQuery;
  DSQuery, DSROQuery: TDataSource;
  LookUp: TDBLookupComboBox;
{$ENDIF}
begin
  if SkipForReason(srClosedBug) then Exit;

{$IFNDEF LINUX}
  Query := CreateQuery;
  ROQuery := CreateReadOnlyQuery;
  DSQuery := TDataSource.Create(nil);
  DSROQuery := TDataSource.Create(nil);
  LookUp := TDBLookupComboBox.Create(nil);
  try
    Query.Connection := Connection;
    ROQuery.Connection := Connection;
    Query.SQL.Text := 'select * from people';
    ROQuery.SQL.Text := 'select * from department';
    DSQuery.DataSet := Query;
    DSROQuery.DataSet := ROQuery;
    LookUp.DataSource := DSQuery;
    LookUp.ListSource := DSROQuery;
    LookUp.DataField := 'p_dep_id';
    LookUp.KeyField := 'dep_id';
    LookUp.ListField := 'dep_name';
    Query.Open;
    ROQuery.Open;

    Query.First;
    CheckEquals(1, Query.FieldByName('p_id').AsInteger);
    CheckEquals(1, Query.FieldByName('p_dep_id').AsInteger);
    CheckEquals('Vasia Pupkin', Query.FieldByName('p_name').AsString);
    CheckEquals('Line agency', LookUp.Text);
    {$IFNDEF FPC}
    CheckEquals(1, Integer(LookUp.KeyValue));
    {$ENDIF}

    Query.Next;
    CheckEquals(2, Query.FieldByName('p_id').AsInteger);
    CheckEquals(2, Query.FieldByName('p_dep_id').AsInteger);
    CheckEquals('Andy Karto', Query.FieldByName('p_name').AsString);
    CheckEquals('Container agency', LookUp.Text);
    {$IFNDEF FPC}
    CheckEquals(2, Integer(LookUp.KeyValue));
    {$ENDIF}
  finally
    LookUp.Free;
    Query.Free;
    ROQuery.Free;
    DSQuery.Free;
    DSROQuery.Free;
  end;
{$ENDIF}
end;

{**
  Float->Numeric problem
}
procedure ZTestCompInterbaseBugReport.Test789879D;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'DELETE FROM TABLE789879';
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO TABLE789879(FLD) VALUES (:Anum_value)';
    Query.ParamByName('Anum_value').AsFloat := 1.14;
    Query.ExecSQL;

    Query.Sql.Text := 'SELECT * FROM TABLE789879';
    Query.Open;
    CheckEquals(1.14, Query.Fields[0].AsFloat, 0.001);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure ZTestCompInterbaseBugReport.Test833489;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
  Check(True);
end;

{**
   Runs a test for bug report #833489
   Can't show messages from triggers
}
procedure ZTestCompInterbaseBugReport.Test841559;
var
  Temp: boolean;
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Temp := False;
  Query := CreateQuery;
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM TABLE841559';
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO TABLE841559 (FLD1, FLD2) VALUES (:FLD1, :FLD2)';
    Query.ParamByName('FLD1').AsInteger := 1;
    Query.ParamByName('FLD2').AsString := '';
    Query.ExecSQL;
  except
    Temp := True;
  end;
  Query.Free;
  CheckEquals(True, Temp, 'Just exception EXCEPTION841559');
end;

{**
   Runs a test for bug report #843655
   Blob fields don't updates
}
procedure ZTestCompInterbaseBugReport.Test843655;
var
  Query: TZQuery;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TMemoryStream;
  {$IFDEF WITH_WIDEMEMO}
  WS: ZWideString;
  {$ENDIF}
  ConSettings: PZConSettings;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;

  // Query.RequestLive := True;
  Query.SQL.Text := 'DELETE FROM BLOB_VALUES';
  Query.ExecSQL;

  BinStream := TMemoryStream.Create;
  StrStream := TMemoryStream.Create;
  BinStream1 := TMemoryStream.Create;
  StrStream1 := TMemoryStream.Create;
  ConSettings := Connection.DbcConnection.GetConSettings;
  try
    { load data to the stream }
    BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
    StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
    {$IFDEF WITH_WIDEMEMO}
    if ( ConSettings.CPType = cCP_UTF16 ) then begin
      StrStream.Position := 0;
      WS := PRawToUnicode(StrStream.Memory, StrStream.Size, zCP_us_ascii);
      StrStream.Write(Pointer(WS)^, Length(WS)*2);
    end;
    {$ENDIF}

    { post empty row }
    Query.SQL.Text := 'SELECT * FROM BLOB_VALUES';
    Query.Open;
    Query.Append;
    Query.FieldByName('B_ID').AsInteger := 1;
    Query.Post;
    Query.Close;
    { update data }
    Query.Open;
    Query.Edit;
    (Query.FieldByName('B_TEXT') as TBlobField).LoadFromStream(StrStream);
    (Query.FieldByName('B_IMAGE') as TBlobField).LoadFromStream(BinStream);
    Query.Post;
    Query.Close;

    { check that data updated }
    Query.Open;
    (Query.FieldByName('B_TEXT') as TBlobField).SaveToStream(StrStream1);
    (Query.FieldByName('B_IMAGE') as TBlobField).SaveToStream(BinStream1);
    CheckEquals(StrStream, StrStream1);
    CheckEquals(BinStream, BinStream1);
    Query.Close;
  finally
    BinStream.Free;
    StrStream.Free;
    BinStream1.Free;
    StrStream1.Free;
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #865441
   ZeosLib reports Ex. numeric(3,1) as IntegerField
}
procedure ZTestCompInterbaseBugReport.Test864622;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'SELECT * FROM TABLE864622';
    Query.Open;
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBCD), Ord(Query.Fields[1].DataType));
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1.2, Query.Fields[1].AsCurrency);
  finally
    Query.Free;
  end;
end;

{
  Runs a test for bug report #886194
  ARITHMETIC EXCEPTION, NUMERIC OVERFLOW, OR STRING TRUNCATION
}
procedure ZTestCompInterbaseBugReport.Test886194;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'DELETE FROM TABLE886194';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM TABLE886194';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsString := 'ABCDEFG';
    Query.Fields[1].AsString := 'KLMNOPQ';
    Query.Post;
  finally
    Query.Free;
  end;

  Check(True);
end;

{**
   Runs a test for bug report #886854
   Incorrect field type
}
procedure ZTestCompInterbaseBugReport.Test886854;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.Connection := Connection;
  // Query.RequestLive := True;
  try
    Query.SQL.Text := 'select rc.rdb$relation_name as rel_name, ' +
    'rc.rdb$index_name as ind_name, rs.rdb$field_name as field_name, ' +
    'rs.rdb$field_position as field_pos from rdb$relation_constraints rc ' +
    'left join rdb$index_segments rs on rs.rdb$index_name=rc. '+
    'rdb$index_name where rs.rdb$field_name is not null and rs. '+
    'rdb$field_name<>''DEP_ID'' and '+
    'rc.rdb$constraint_type=''PRIMARY KEY'' and rc.rdb$relation_name=''PEOPLE'' ' +
    'order by rc.rdb$relation_name';
    Query.Open;
    //Client_Character_set sets column-type!!!!
    if (Connection.DbcConnection.GetConSettings.CPType = cCP_UTF16) then
    begin
      CheckEquals(ord(ftWideString), ord(Query.Fields[0].DataType));
      CheckEquals(ord(ftWideString), ord(Query.Fields[1].DataType));
      CheckEquals(ord(ftWideString), ord(Query.Fields[2].DataType));
    end
    else
    begin
      CheckEquals(ord(ftString), ord(Query.Fields[0].DataType));
      CheckEquals(ord(ftString), ord(Query.Fields[1].DataType));
      CheckEquals(ord(ftString), ord(Query.Fields[2].DataType));
    end;
    CheckEquals(ord(ftSmallint), ord(Query.Fields[3].DataType));

    CheckEquals('PEOPLE', Query.Fields[0].AsString);
    CheckEquals(Copy('RDB$PRIMARY2598', 1, Length('RDB$PRIMARY')),
      Copy(Query.Fields[1].AsString, 1, Length('RDB$PRIMARY')));
    CheckEquals('P_ID', Query.Fields[2].AsString);
    CheckEquals(0, Query.Fields[3].AsInteger);
  finally
    Query.Free;
  end;

end;

{**
    Numeric 15.2 field which displays as integer
}
procedure ZTestCompInterbaseBugReport.Test897631;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  // Query.RequestLive := True;

  try
    Query.SQL.Text := 'DELETE FROM TABLE897631';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO TABLE897631 VALUES (179.22)';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM TABLE897631';
    Query.Open;
    with Query do
    begin
      CheckEquals(179.22, Fields[0].AsFloat, 0.001);
      Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test909181;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  // Query.RequestLive := True;
  UpdateSQL := TZUpdateSQL.Create(nil);

  try
    Query.SQL.Text := 'DELETE FROM TABLE909181';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO TABLE909181 VALUES (''A'', 10, ''2004-03-11'')';
    Query.ExecSQL;

    Query.UpdateObject := UpdateSQL;
    UpdateSQL.ModifySQL.Text := ' UPDATE TABLE909181 SET' +
      ' FLD1 = :FLD1,' +
      ' FLD2 = :FLD2,' +
      ' FLD3 = :FLD3'+
      ' WHERE FLD1 = :OLD_FLD1';

    Query.SQL.Text := 'SELECT * FROM TABLE909181';
    // Query.RequestLive := True;
    Query.Open;
    with Query do
    begin
      Edit;
      Fields[0].Value := Null;
      Fields[1].Value := Null;
      Fields[2].Value := Null;
      Post;
      Close;
    end;
    Fail('Problems with set Null prametrs in SQLDA');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
  UpdateSQL.Free;
  Query.Free;
end;

{**
    Test for Bug#984305
}
procedure ZTestCompInterbaseBugReport.Test984305;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM PEOPLE';
    Query.Open;
    Connection.Disconnect;
    CheckEquals(False, Connection.Connected);
    CheckEquals(False, Query.Active);

    try
      Connection.User := '';
      Connection.Connect;
      Fail('Problems with change user name');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.TestExecutBlockReturning_WithoutParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.ParamCheck := False;
    SQL := 'execute block (val1 int = ?, val2 int = ?)'+LineEnding+
      'returns (largest int, smallest int, square bigint)'+LineEnding+
      'as begin '+LineEnding+
      '  if (val1 < val2) then begin'+LineEnding+
      '    largest = val2;' +LineEnding+
      '    smallest = val1;'+LineEnding+
      '  end else if (val1 > val2) then begin '+LineEnding+
      '    largest = val1;'+LineEnding+
      '    smallest = val2;'+LineEnding+
      '  end'+LineEnding+
      '  square = val1 * val2;'+LineEnding+
      '   suspend;'+LineEnding+
      'end';
    Query.SQL.Text := SQL;
    Query.Params.CreateParam(ftInteger, 'val1', ptInPut);
    Query.Params.CreateParam(ftInteger, 'val2', ptInPut);
    Query.Params.CreateParam(ftInteger, 'largest', ptOutPut);
    Query.Params.CreateParam(ftInteger, 'smallest', ptOutPut);
    {$IFDEF WITH_ASLARGEINT}
    Query.Params.CreateParam(ftLargeInt, 'square',   ptOutPut);
    {$ELSE WITH_ASLARGEINT}
    Query.Params.CreateParam(ftInteger, 'square',   ptOutPut);
    {$ENDIF}
    Query.Params[0].AsInteger := 10;
    Query.Params[1].AsInteger := 20;
    Query.ExecSQL;
    CheckEquals(20, Query.ParamByName('largest').AsInteger, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(10, Query.ParamByName('smallest').AsInteger, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(200, Query.ParamByName('square').AsInteger, 'The OutParam-Result of a anonymous execute block');
    Query.Params[1].AsInteger := 10;
    Query.ExecSQL;
    Check(Query.ParamByName('largest').IsNull, 'The OutParam-Result of a anonymous execute block');
    Check(Query.ParamByName('smallest').IsNull, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(100, Query.ParamByName('square').AsInteger, 'The OutParam-Result of a anonymous execute block');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.TestExecutBlockReturning_WithParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    SQL := 'execute block (val1 int = :val1, val2 int = :val2)'+LineEnding+
      'returns (largest int, smallest int, square bigint)'+LineEnding+
      'as begin '+LineEnding+
      '  if (val1 < val2) then begin'+LineEnding+
      '    largest = val2;' +LineEnding+
      '    smallest = val1;'+LineEnding+
      '  end else if (val1 > val2) then begin '+LineEnding+
      '    largest = val1;'+LineEnding+
      '    smallest = val2;'+LineEnding+
      '  end else begin'+Lineending+
      '    largest = null;'+LineEnding+
      '    smallest = null;'+LineEnding+
      '  end'+LineEnding+
      '  square = val1 * val2;'+LineEnding+
      '   suspend;'+LineEnding+
      'end';
    Query.SQL.Text := SQL;
    Query.Params.CreateParam(ftInteger, 'largest', ptOutPut);
    Query.Params.CreateParam(ftInteger, 'smallest', ptOutPut);
    {$IFDEF WITH_ASLARGEINT}
    Query.Params.CreateParam(ftLargeInt, 'square',   ptOutPut);
    {$ELSE WITH_ASLARGEINT}
    Query.Params.CreateParam(ftInteger, 'square',   ptOutPut);
    {$ENDIF WITH_ASLARGEINT}
    Query.Params[0].AsInteger := 10;
    Query.Params[1].AsInteger := 20;
    Query.ExecSQL;
    CheckEquals(20, Query.ParamByName('largest').AsInteger, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(10, Query.ParamByName('smallest').AsInteger, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(200, Query.ParamByName('square').AsInteger, 'The OutParam-Result of a anonymous execute block');
    Query.Params[1].AsInteger := 10;
    Query.ExecSQL;
    Check(Query.ParamByName('largest').IsNull, 'The OutParam-Result of a anonymous execute block');
    Check(Query.ParamByName('smallest').IsNull, 'The OutParam-Result of a anonymous execute block');
    CheckEquals(100, Query.ParamByName('square').AsInteger, 'The OutParam-Result of a anonymous execute block');
  finally
    Query.Free;
  end;
end;


procedure ZTestCompInterbaseBugReport.TestProcAbtest_WithoutParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.ParamCheck := False;
    SQL := 'EXECUTE PROCEDURE abtest(?, ?, ?)';
    Query.SQL.Text := SQL;
    Query.Params.CreateParam(ftInteger, 'P1', ptInPut);
    Query.Params[0].AsInteger := 10;
    Query.Params.CreateParam(ftInteger, 'P2', ptInPut);
    Query.Params[1].AsInteger := 20;
    Query.Params.CreateParam(ftString, 'P3', ptInPut);
    Query.Params[2].Precision := 20;
    Query.Params[2].AsString := 'xx';
    Query.Params.CreateParam(ftInteger, 'P4', ptOutPut);
    Query.Params.CreateParam(ftString, 'P5', ptOutPut);
    Query.Params[4].Precision := 20;
    Query.ExecSQL;
    CheckEquals(120, Query.ParamByName('P4').AsInteger, 'The OutParam-Result of a exec pro abtest');
    CheckEquals('xxxx', Query.ParamByName('P5').AsString, 'The OutParam-Result of a exec pro abtest');
    Query.Params[0].AsInteger := Query.Params[3].AsInteger;
    Query.Params[2].AsString := Query.Params[4].AsString;
    Query.ExecSQL;
    CheckEquals(1220, Query.ParamByName('P4').AsInteger, 'The OutParam-Result of a exec pro abtest');
    CheckEquals('xxxxxxxx', Query.ParamByName('P5').AsString, 'The OutParam-Result of a exec pro abtest');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.TestProcAbtest_WithParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    SQL := 'EXECUTE PROCEDURE abtest(:P1, :P2, :P3)';
    Query.SQL.Text := SQL;
    Query.Params[0].AsInteger := 10;
    Query.Params[1].AsInteger := 20;
    Query.Params[2].AsString := 'xx';
    Query.Params.CreateParam(ftInteger, 'P4', ptOutPut);
    Query.Params.CreateParam(ftString, 'P5', ptOutPut);
    Query.Params[4].Precision := 10;
    Query.ExecSQL;
    CheckEquals(120, Query.ParamByName('P4').AsInteger, 'The OutParam-Result of a exec pro abtest');
    CheckEquals('xxxx', Query.ParamByName('P5').AsString, 'The OutParam-Result of a exec pro abtest');
    Query.Params[0].AsInteger := Query.Params[3].AsInteger;
    Query.Params[2].AsString := Query.Params[4].AsString;
    Query.ExecSQL;
    CheckEquals(1220, Query.ParamByName('P4').AsInteger, 'The OutParam-Result of a exec pro abtest');
    CheckEquals('xxxxxxxx', Query.ParamByName('P5').AsString, 'The OutParam-Result of a exec pro abtest');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.TestTicket363;
var
  I: Integer;
  PStmt: IZPreparedStatement;
  RS: IZResultSet;
  Query: TZQuery;
begin
  Exit;
  Query := CreateQuery;
  // prepare sample table
  Query.SQL.Text:= 'create table test73 (id integer primary key, "value" BLOB SUB_TYPE TEXT)';
  Query.ExecSQL;
  try
    // generate test data
    for I:= 1 to 20 do begin
      Query.SQL.Text:= 'insert into test73 (id, "value") values (' + IntTostr(I) + ', ''testing #' + IntToStr(I) + ''')';
      Query.ExecSQL;
    end;
    // trigger fix for ticket #228
    Query.SQL.Text:= 'select * from test73';
    Query.Open;
    I := 1;
    while not Query.Eof do begin
      if Query.RecNo = 2 then begin // go
        Connection.StartTransaction;
        Connection.Commit;
      end;
      Query.Next; // it works here with a TZQuery
      Inc(I);
      if I > 20 then Break;
    end;
    Check(I = 20, 'duplicated rows retrieved');
    // direct approach (just like mORMot with SynDBZeos)
    PStmt:= Connection.DbcConnection.PrepareStatement('select * from test73 where id > ?');
    PStmt.SetInt(FirstDbcIndex, 1);
    RS:= PStmt.ExecuteQueryPrepared;
    I:= 0;
    while RS.Next do begin // BUG! duplicates the fetched row
      if I = 2 then begin // trigger
        Connection.StartTransaction;
        Connection.Commit;
      end;
      Inc(I);
      if I > 21 then Break;
    end;
    Check(I = 20, 'duplicated rows retrieved');
//  RS:= nil; // trigger stmt reuse (e.g. mORMot statement cache)
    PStmt.ClearParameters;
    // trigger fix for ticket #228
    Connection.StartTransaction;
    Connection.Commit;

    PStmt.SetInt(FirstDbcIndex, 0);
    RS:= PStmt.ExecuteQueryPrepared; // the transaction ID is not updated
    I:= 0;
    while RS.Next do begin // BUG! fails here as the fix is trying to open the resultset already opened in ExecuteQueryPrepared
      Inc(I);
      if I > 20 then
        Break;
    end;
    Check(I = 20, 'duplicated rows retrieved');
  finally
    Query.SQL.Text:= 'drop table test73';
    Query.ExecSQL;
    if Assigned(PStmt) then
      PStmt.Close;
    PStmt:= nil;
  end;
end;

(*
Hello,
today I noticed that when a prepared query is used multiple times
(e.g. cached by the SynDB of mORMot) it fails to return correct results.
Investigating a bit, I have found that a param value does not reset the
"NULL" flag correctly, i.e. when a param was set to NULL previously, setting a
value afterwards does not work (at least for some methods:
  SetString, SetAnsiString, SetUTF8String).
Attached is a sample project that currently fails.

Best regards,
Joe
*)
procedure ZTestCompInterbaseBugReport.TestTicket376;
var
  I: Integer;
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Connection.Connected:= True;

    // prepare sample table with integer key
    try
      Query.SQL.Text := 'delete from Ticket376a where 1=1';
      Query.ExecSQL;
      // generate test data
      for I:= 1 to 20 do begin
        Query.SQL.Text:= 'insert into Ticket376a (id, "value") values (' + IntToStr(I) + ', ''testing #' + IntToStr(I) + ''')';
        Query.ExecSQL;
      end;
      // check read
      Query.SQL.Text:= 'select * from Ticket376a where id=:id';
      Query.Params.ParamByName('id').Value:= Null;
      Query.Open;
      CheckEquals(0, Query.RecordCount, 'there is no row for null id');
      Query.Close;
      Query.Params.ParamByName('id').AsInteger:= 5;
      Query.Open;
      while not Query.Eof do begin
        Check(Query.Fields[0].AsString <> '', 'clob should not be empty');
        Query.Next; // it works here with a TZQuery
      end;
      CheckEquals(1, Query.RecordCount, 'the record count for id 5');
      Query.Close;
      Query.Params.ParamByName('id').Value:= Null;
      Query.Open;
      CheckEquals(0, Query.RecordCount, 'there is no row for null id');
      Query.Close;
    finally
      Query.SQL.Text := 'delete from Ticket376a where 1=1';
      Query.ExecSQL;
    end;

    // prepare sample table with varchar key
    try
      Query.SQL.Text := 'delete from Ticket376b where 1=1';
      Query.ExecSQL;
      // generate test data
      for I:= 1 to 20 do begin
        Query.SQL.Text:= 'insert into Ticket376b (id, "value") values (''' + IntToStr(I) + ''', ''testing #' + IntToStr(I) + ''')';
        Query.ExecSQL;
      end;
      // check read
      Query.SQL.Text:= 'select * from Ticket376b where id=:id';
      Query.Params.ParamByName('id').Value:= Null;
      Query.Open;
      CheckEquals(0, Query.RecordCount, 'there is no row for null id');
      Query.Close;
      Query.Params.ParamByName('id').AsString:= '5';
      Query.Open;
      while not Query.Eof do begin
        Check(Query.Fields[0].AsString <> '', 'clob should not be empty');
        Query.Next; // it works here with a TZQuery
      end;
      CheckEquals(1, Query.RecordCount, 'the record count for id 5');
      Query.Close;
      Query.Params.ParamByName('id').Value:= Null;
      Query.Open;
      CheckEquals(0, Query.RecordCount, 'there is no row for null id');
      Query.Close;
    finally
      Query.SQL.Text := 'delete from Ticket376b where 1=1';
      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test_Ticket228;
var
  Query: TZQuery;
  I, j: Integer;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT RDB$RELATION_NAME' +
      ' FROM RDB$RELATIONS' +
      ' WHERE RDB$SYSTEM_FLAG=0;';
    Connection.Connect;
    Connection.StartTransaction;
    Query.Open;
    Connection.Commit;
    I := Query.RecordCount; //<- did crash
    J := 0;
    while not Query.Eof do begin
      Check(Query.Fields[0].AsString <> '');
      Query.Next;
      Inc(J);
    end;
    Query.Close;
    Check(I = J, 'TestPassed');
  finally
    Query.Free;
  end;
end;

{
I'm using Firebird 2.5.2. This error appears for lazarus 1.0.12 (windows and linux) and delphi 7.
INSERT with RETURNING works when executed Query.SQL.Text. If you do the Close and Open, the error is "Error Code: -502. The cursor identified in an OPEN statement is already open.".
}
procedure ZTestCompInterbaseBugReport.Test_Ticket54;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'INSERT INTO Ticket54(Val) VALUES(:P) RETURNING ID';
    Query.ParamByName('P').AsString := 'Test';
    Query.ExecSQL;
    Query.Open;
    Query.Close;
    Query.Open;
  finally
    Query.Free;
  end;

  Check(True);
end;

{
}
procedure ZTestCompInterbaseBugReport.Test_Ticket63;
var
  Table: TZTable;
begin
  if SkipForReason(srClosedBug) then Exit;

  Table := CreateTable;
  try
    Table.TableName := 'PLUS__';
    Table.Open;
    CheckEquals(3, Table.Fields.Count);
    Table.Insert;
    Table.FieldByName('ID').AsInteger := TEST_ROW_ID;
    Table.Fields[1].AsString := 'A';
    Table.Fields[2].AsString := 'PLUSA';
    Table.Post;
    Table.Edit;
    Table.Fields[2].AsString := 'PLU__';
    Table.Post;

    Table.TableName := 'PLUSA';
    Table.Open;
    CheckEquals(3, Table.Fields.Count);
    Table.Insert;
    Table.FieldByName('ID').AsInteger := TEST_ROW_ID;
    Table.Fields[1].AsString := 'PLUS__';
    Table.Post;
    Table.Edit;
    Table.Fields[1].AsString := 'PLUSA';
    Table.Post;
  finally
    Table.Free;
    Connection.ExecuteDirect('Delete from PLUS__');
    Connection.ExecuteDirect('Delete from PLUSA');
  end;
end;

procedure ZTestCompInterbaseBugReport.Test_Ticket67;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  Query.ParamCheck := False;
  Query.SQL.Text := 'execute block (P1 integer = ?) returns (P2 integer) as begin select R1 from PROCEDURE1(:p1) into :P2; suspend; end';
  Query.Params.CreateParam(ftInteger, 'P1', ptInput);
  Query.Params[0].AsInteger := 10;
  try
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'the RecordCount');
    CheckEquals(11, Query.Fields[0].AsInteger, 'the returned value');
  finally
    Query.Free;
  end;
  Query := CreateQuery;
  Query.ParamChar := '&';
  Query.SQL.Text := 'execute block (P1 integer = &P) returns (P2 integer) as begin select R1 from PROCEDURE1(:p1) into :P2; suspend; end';
  CheckEquals(1, Query.Params.Count, 'the ParamCount');
  Query.Params[0].AsInteger := 10;
  try
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'the RecordCount');
    CheckEquals(11, Query.Fields[0].AsInteger, 'the returned value');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test_SF249;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from RDB$DATABASE where :TESTPARM=''''';
    Query.ParamByName('TESTPARM').AsString := 'xyz';
    try
      Query.Open;
      Fail('where clause evaulates to true although it shouldn''t: where :TESTPARM = ''''; testparm = ''xyz''');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

procedure ZTestCompInterbaseBugReport.Test_SF287;
const
  DateStr1 = '0018-07-02 19:00:00';
  DateStr2 = '0018-07-01 05:00:00';
  FormatStr = '';
var
  Query: TZQuery;
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FormatSettings.LongDateFormat := FormatSettings.ShortDateFormat;
  FormatSettings.ShortTimeFormat := 'hh:nn:ss';
  FormatSettings.LongTimeFormat := FormatSettings.ShortTimeFormat;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'insert into date_values (d_id, d_datetime, d_timestamp) values (:id, cast(cast(:date1 as varchar(50)) as timestamp), :date2)';
    Query.ParamByName('id').AsString := '1001';
    Query.ParamByName('date1').AsString := DateStr1;
    Query.ParamByName('date2').AsDateTime := StrToDateTime(DateStr1, FormatSettings);
    Query.ExecSQL;

    Query.SQL.Text := 'insert into date_values (d_id, d_datetime, d_timestamp) values (:id, cast(cast(:date1 as varchar(50)) as timestamp), :date2)';
    Query.ParamByName('id').AsString := '1002';
    Query.ParamByName('date1').AsString := DateStr2;
    Query.ParamByName('date2').AsDateTime := StrToDateTime(DateStr2, FormatSettings);
    Query.ExecSQL;

    Query.SQL.Text := 'select * from date_values where d_id in (1001, 1002) order by d_id';
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'Checking if the query returned two records.');
    Query.First;
    CheckEquals(DateStr1, DateTimeToStr(Query.FieldByName('d_datetime').AsDateTime, FormatSettings), 'Checking, if the first timestamp field has the expected value.');
    CheckEquals(DateStr1, DateTimeToStr(Query.FieldByName('d_timestamp').AsDateTime, FormatSettings), 'Checking, if the second field is as expected.');
    Query.Next;
    CheckEquals(DateStr2, DateTimeToStr(Query.FieldByName('d_datetime').AsDateTime, FormatSettings), 'Checking, if the first timestamp field has the expected value.');
    CheckEquals(DateStr2, DateTimeToStr(Query.FieldByName('d_timestamp').AsDateTime, FormatSettings), 'Checking, if the second timestamp field has the expected value.');
  finally
    try
      Query.SQL.Text := 'delete from date_values where d_id in (1001, 1002)';
      Query.ExecSQL;
    finally
      Query.Close;
      Query.Free;
    end;
  end;
end;

{ ZTestCompInterbaseBugReportMBCs }

function ZTestCompInterbaseBugReportMBCs.GetSupportedProtocols: string;
begin
  Result := pl_all_interbase;
end;

const
  Str2 = 'Одной из наиболее тривиальных задач, решаемых многими коллективами программистов, является построение информационной системы для автоматизации бизнес-деятельности предприятия. Все архитектурные компоненты (базы данных, сервера приложений, клиентское ...';
  Str3 = 'Одной из наиболее';

procedure ZTestCompInterbaseBugReportMBCs.Test_Param_LoadFromStream_StringStream_ftBlob;
var
  Query: TZQuery;
  StrStream1: TMemoryStream;
  SL: TStringList;
begin
//??  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  SL := TStringList.Create;
  StrStream1 := TMemoryStream.Create;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      //bugreport of mrLion

      SQL.Text := 'INSERT INTO people(P_ID, P_NAME, P_RESUME)'+
        ' VALUES (:P_ID, :P_NAME, :P_RESUME)';
      ParamByName('P_ID').AsInteger := TEST_ROW_ID;
      ParamByName('P_NAME').AsString := GetDBTestString(Str3, Connection.DbcConnection.GetConSettings);

      CheckEquals(3, Query.Params.Count, 'Param.Count');
      SL.Text := GetDBTestString(Str2, Connection.DbcConnection.GetConSettings);

      SL.SaveToStream(StrStream1);
      ParamByName('P_RESUME').LoadFromStream(StrStream1, ftBlob);
      try
        ExecSQL;
        SQL.Text := 'select * from people where p_id = ' + IntToStr(TEST_ROW_ID);
        StrStream1.Free;
        StrStream1 := TMemoryStream.Create;
        Open;

        (FieldByName('P_RESUME') as TBlobField).SaveToStream(StrStream1);
        CheckEquals(str2+LineEnding, StrStream1, Connection.DbcConnection.GetConSettings, 'Param().LoadFromStream(StringStream, ftBlob)');
        SQL.Text := 'DELETE FROM people WHERE p_id = :p_id';
        CheckEquals(1, Params.Count);
        Params[0].DataType := ftInteger;
        Params[0].AsInteger := TEST_ROW_ID;

        ExecSQL;
        CheckEquals(1, RowsAffected);
      except
        on E:Exception do
          Fail('Param().LoadFromStream(StringStream, ftBlob): '+E.Message);
      end;
    end;
  finally
    SL.free;
    FreeAndNil(StrStream1);
    Query.Free;
  end;
end;

//bugreport of mrLion
procedure ZTestCompInterbaseBugReportMBCs.Test_Param_LoadFromStream_StringStream_ftMemo;
var
  Query: TZQuery;
  StrStream1: TMemoryStream;
  SL: TStringList;
  {$IFDEF UNICODE}
  R: RawByteString;
  {$ENDIF}
  ConSettings: PZConSettings;
begin
//??  if SkipForReason(srClosedBug) then Exit;
  Query := CreateQuery;
  SL := TStringList.Create;
  StrStream1 := TMemoryStream.Create;
  try
    with Query do
    begin
      SQL.Text := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      ConSettings := Connection.DbcConnection.GetConSettings;


      SQL.Text := 'INSERT INTO people(P_ID, P_NAME, P_RESUME)'+
        ' VALUES (:P_ID, :P_NAME, :P_RESUME)';
      ParamByName('P_ID').AsInteger := TEST_ROW_ID;
      CheckEquals(3, Query.Params.Count, 'Param.Count');

      ParamByName('P_NAME').AsString := GetDBTestString(Str3, Connection.DbcConnection.GetConSettings);
      SL.Text := GetDBTestString(Str2, Connection.DbcConnection.GetConSettings);
      {$IFDEF UNICODE}
      if not ConSettings.AutoEncode then begin
        R := ZUnicodeToRaw(Str2+LineEnding, ConSettings^.ClientCodePage.CP);
        StrStream1.Write(Pointer(R)^, Length(R));
        StrStream1.Position := 0;
      end else
      {$ENDIF}
        SL.SaveToStream(StrStream1);
      ParamByName('P_RESUME').LoadFromStream(StrStream1, ftMemo);

      try
        ExecSQL;
        SQL.Text := 'select * from people where p_id = ' + IntToStr(TEST_ROW_ID);
        StrStream1.Free;
        StrStream1 := TMemoryStream.Create;
        Open;

        (FieldByName('P_RESUME') as TBlobField).SaveToStream(StrStream1);
        CheckEquals(Str2+LineEnding, StrStream1, ConSettings, 'Param().LoadFromStream(StringStream, ftMemo)');
        SQL.Text := 'DELETE FROM people WHERE p_id = :p_id';
        CheckEquals(1, Params.Count);
        Params[0].DataType := ftInteger;
        Params[0].AsInteger := TEST_ROW_ID;

        ExecSQL;
        CheckEquals(1, RowsAffected);
      except
        on E:Exception do
          Fail('Param().LoadFromStream(StringStream, ftMemo): '+E.Message);
      end;
    end;
  finally
    StrStream1.Free;
    SL.free;
    Query.Free;
  end;
end;

{**
Database: Firebird 2.1.3 (latest as of 2010-01-04)
ZEOSLib: 7.0.0 (latest from SVN trunk as of 2010-01-04)
System: Windows (english) mit CodeGear Delphi 2009

Situation:

I have a firebird-2.1 database with UTF-8 as default charset and a single table t with two columns i (integer) and s (string). I insert a row into the table via a prepared statement "insert into t(i,s) values (:i1,:s1)". Depending on the contents of the unicode string given as named parameter s1, I see the following behavior:

1. String contains german Umlauts: query fails with malformed string error
2. String contains ASCII chars and cyrillic letters: query succeeds but reading back the row via select gives the original string with the cyrillic letters replaced by question marks
3. String contains only ASCII chars: query succeeds but reading back the row via select gives the original string

I consider cases 1 and 2 as errors.

If no parameter is used for the insert query, i.e. using something like "insert into t(i,s) values (:i1,'+QuotedStr(s)+')'" all three cases are correct.

For sample code see Additional Information. Minimalistic program to reproduce the error promptly available on request.

Please let me know what I can do to track down the bug.

}
procedure ZTestCompInterbaseBugReportMBCs.Test_Mantis214;
const
  RowID = 214;
  { three cases }
  S1 = RawByteString('MГјller Г¤Г¶ГјГ„Г–ГњГџ'); // gives malformed expression error on ExecSQL
  S2 = RawByteString('000 РџРµС‚СЉСЂ 000'); // can be stored but cyrillic letters cannot be read back
  S3 = RawByteString('abc'); // can be written and reread
var
  iqry: TZQuery;
  ConSettings: PZConSettings;
  Procedure AddRecord(ID: Integer; WS: ZWideString);
  begin
    iqry.ParamByName('i1').AsInteger:= ID;
    {$IFDEF UNICODE}
    iqry.ParamByName('s1').AsString := WS;
    {$ELSE}
    iqry.ParamByName('s1').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := WS;
    {$ENDIF}
    iqry.ExecSQL;
  end;
begin
//??  if SkipForReason(srClosedBug) then Exit;

  { prepared insert statement }
  iqry:= CreateQuery;
  try
    Connection.Connect; // DbcConnection needed
    ConSettings := Connection.DbcConnection.GetConSettings;
    if ConSettings^.ClientCodePage^.Encoding = ceUTF8 then
    begin
      if ( ConSettings.CPType = cCP_UTF16 ) then
      begin
        iqry.SQL.Add('insert into string_values(s_id,s_varchar) values (:i1,:s1)');
        iqry.Prepare;
        AddRecord(RowID, {$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(S1));
        AddRecord(RowID+1,{$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(S2));
        AddRecord(RowID+2,{$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(S3));

        iqry.SQL.Text := 'select s_varchar from string_values where s_id > 213 and s_id < 217';
        iqry.open;

        CheckEquals(3, iqry.RecordCount, 'RecordCount');
        {$IFDEF WITH_FTWIDESTRING}
          {$IFDEF UNICODE}
          CheckEquals(UTF8ToString(S1), iqry.Fields[0].AsString);
          iqry.Next;
          CheckEquals(UTF8ToString(S2), iqry.Fields[0].AsString);
          iqry.Next;
          CheckEquals(UTF8ToString(S3), iqry.Fields[0].AsString);
          {$ELSE}
          CheckEquals(UTF8Decode(S1), iqry.Fields[0].AsWideString);
          iqry.Next;
          CheckEquals(UTF8Decode(S2), iqry.Fields[0].AsWideString);
          iqry.Next;
          CheckEquals(UTF8Decode(S3), iqry.Fields[0].AsWideString);
          {$ENDIF}
        {$ENDIF}
      end
      else
      begin
        iqry.SQL.Add('insert into string_values(s_id,s_varchar) values (:i1,:s1)');
        iqry.Prepare;
        iqry.ParamByName('i1').AsInteger:= RowID;
        iqry.ParamByName('s1').AsString:= GetDBTestString(S1, ConSettings, True);
        iqry.ExecSQL;
        iqry.ParamByName('i1').AsInteger:= RowID+1;
        iqry.ParamByName('s1').AsString:= GetDBTestString(S2, ConSettings, True);
        iqry.ExecSQL;
        iqry.ParamByName('i1').AsInteger:= RowID+2;
        iqry.ParamByName('s1').AsString:= GetDBTestString(S3, ConSettings, True);
        iqry.ExecSQL;
        iqry.Unprepare;

        iqry.SQL.Text := 'select s_varchar from string_values where s_id > 213 and s_id < 217';
        iqry.open;

        CheckEquals(3, iqry.RecordCount, 'RecordCount');
        { note GetDBTestString might have dataloss if AutoEncode is set and the os-code does not support cyrylic or Latin1 chars ...
          so we'll skip the check because we'll never be able to get a match in such cases }
        if ((ZOSCodePage = zCP_WIN1252) or (ZOSCodePage = zCP_UTF8)) or (not ConSettings^.AutoEncode and (ConSettings.CTRL_CP = zCP_UTF8)) then
           if (ConSettings.CTRL_CP = zCP_WIN1252) or (ConSettings.CTRL_CP = zCP_UTF8) then
              CheckEquals({$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(S1), iqry.Fields[0].AsString, ConSettings);
        iqry.Next;
        if ((ZOSCodePage = zCP_WIN1251) or (ZOSCodePage = zCP_UTF8)) or (not ConSettings^.AutoEncode and (ConSettings.CTRL_CP = zCP_UTF8)) then
           if (ConSettings.CTRL_CP = zCP_WIN1251) or (ConSettings.CTRL_CP = zCP_UTF8) then
              CheckEquals({$IFDEF FPC}UTF8Decode{$ELSE}UTF8ToString{$ENDIF}(S2), iqry.Fields[0].AsString, ConSettings);
        iqry.Next;
        CheckEquals(S3, iqry.Fields[0].AsString);
      end;
    end;
  finally
    iqry.SQL.Text := 'delete from string_values where s_id > 213 and s_id < 217';
    iqry.ExecSQL;
    iqry.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompInterbaseBugReport.Suite);
  RegisterTest('bugreport',ZTestCompInterbaseBugReportMBCs.Suite);
end.
