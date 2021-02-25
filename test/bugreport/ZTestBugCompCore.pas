{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Generic Component Bug Reports      }
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

unit ZTestBugCompCore;

interface

{$I ZBugReport.inc}

uses
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZConnection, ZDbcIntfs, ZSqlTestCase,
  ZCompatibility, ZSqlUpdate, ZSqlProcessor, ZSqlMetadata;

type

  {** Implements a bug report test case for core components. }
  ZTestCompCoreBugReport = class(TZAbstractCompSQLTestCase)
  private
    FUpdateCounter: Integer;
    FErrorCounter: Integer;
    procedure TestSF279CalcFields(DataSet: TDataSet);
  public
    procedure DataSetCalcFields(Dataset: TDataSet);
    procedure DataSetBeforeScroll({%H-}Dataset: TDataSet);
    procedure DataSetAfterScroll({%H-}Dataset: TDataSet);
    procedure DataSetOnError({%H-}DataSet: TDataSet;
      {%H-}E: EDatabaseError; var Action: TDataAction);
  published
    procedure Test707309;
    procedure Test707364;
    procedure Test000001;
    procedure Test000002;
    procedure Test000003;
    procedure Test715099;
    procedure Test722651;
    procedure Test773022;
    procedure Test772926;
    procedure Test793351;
    procedure Test804323;
    procedure Test804640;
    procedure Test802548;
    procedure Test795641;
    procedure Test826886;
    procedure Test000004;
    procedure Test832467;
    procedure Test830804;
    procedure Test833197;
    procedure Test834798;
    procedure Test839540;
    procedure Test840218;
    procedure Test842678;
    procedure Test846377;
    procedure Test880459;
    procedure Test000005;
    procedure Test887103;
    procedure Test919401;
    procedure Test926264;
    procedure Test953557;
    procedure Test966267;
    procedure Test985629;
    procedure TestFloatPrecision;
    procedure Test995080;
    procedure Test996283;
    procedure Test1004534;
    procedure Test1012751;
    procedure Test1022415;
    procedure Test1049821;
    procedure Test1045500;
    procedure Test1036916;
    procedure TestParamUx;
    procedure TestTicket228;
    procedure TestSF270_1;
    procedure TestSF270_2;
    procedure TestSF279;
    procedure TestSF286_getBigger;
    procedure TestSF286_getSmaller;
    procedure TestSF301;
    procedure TestSF238;
    procedure TestSF418_SortedFields;
    procedure TestSF418_IndexFieldNames;
    procedure TestSF434;
    procedure TestSF310_JoinedUpdate_SetReadOnly;
    procedure TestSF310_JoinedUpdate_ProviderFlags;
    procedure TestSF469;
    procedure TestSF493;
  end;

  {** Implements a bug report test case for core components with MBCs. }
  ZTestCompCoreBugReportMBCs = class(TZAbstractCompSQLTestCaseMBCs)
  private
  published
    {$IFDEF MSWINDOWS}
    //It will be next to impossible to get these tests working correctly on FPC.
    //On Linux there usually will not be a non-ASCII character set. Other tests
    //will test Unicode.
    procedure TestUnicodeBehavior;
    procedure TestNonAsciiChars;
    {$ENDIF}
    procedure TestUnicodeChars;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  SysUtils,
  ZSysUtils, ZEncoding,
  ZDbcMetadata,
  ZDatasetUtils, ZAbstractDataset, ZTestConsts, ZTestCase;

{ ZTestCompCoreBugReport }

{**
  Assignes values to dataset calculated fields.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetCalcFields(Dataset: TDataSet);
begin
  Dataset.FieldByName('p_calc').AsInteger := Dataset.RecNo + 100;
end;

{**
  Performs an event after dataset is scrolled.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetAfterScroll(Dataset: TDataSet);
begin
  FUpdateCounter := FUpdateCounter + 100;
end;

{**
  Performs an event before dataset is scrolled.
  @param DataSet a database object.
}
procedure ZTestCompCoreBugReport.DataSetBeforeScroll(Dataset: TDataSet);
begin
  FUpdateCounter := FUpdateCounter + 1;
end;

{**
  Performs an event when some error occures.
  @param DataSet a dataset object.
  @param E a database error.
  @param Action an action to be performed.
}
procedure ZTestCompCoreBugReport.DataSetOnError(DataSet: TDataSet;
  E: EDatabaseError; var Action: TDataAction);
begin
  FErrorCounter := FErrorCounter + 1;
  Action := daAbort;
end;

{**
  Bugs report #000002.

  The program will crash with access violation at the line
  if Length(FieldIndices) = 1 then
    KeyValues := KeyValues[0];
}
procedure ZTestCompCoreBugReport.Test000002;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    CheckEquals(False, Query.CachedUpdates);

    Query.SQL.Text := 'SELECT eq_id FROM equipment';
    Query.Open;
    Check(Query.RecordCount > 0);
    Query.Refresh;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #707309.

  Non-Cached mode doesn't post updates
}
procedure ZTestCompCoreBugReport.Test000001;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    CheckEquals(False, Query.CachedUpdates);

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Insert a new record }
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    Query.Append;
    Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
    Query.Post;
    Query.Close;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #707309.

  Query.Param.LoadFromStream
}
procedure ZTestCompCoreBugReport.Test707309;
var
  Query: TZQuery;
  TextStream: TMemoryStream;
  BinaryStream: TMemoryStream;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  TextStream := TMemoryStream.Create();
  BinaryStream := TMemoryStream.Create();
  try
    TextStream.LoadFromFile(TestFilePath('text/gnu.txt'));
    TextStream.Position := 0;

    BinaryStream.LoadFromFile(TestFilePath('images/coffee.bmp'));
    BinaryStream.Position := 0;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Creates a record }
    Query.SQL.Text := 'INSERT INTO people (p_id, p_resume, p_picture)'
      + ' VALUES(:id, :resume, :picture)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ParamByName('resume').LoadFromStream(TextStream, ftMemo);
    Query.ParamByName('picture').LoadFromStream(BinaryStream, ftBlob);
    Query.ExecSQL;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    Check(True);
  finally
    TextStream.Free;
    BinaryStream.Free;
    Query.Free;
  end;
end;

{**
  Bugs report #707364.

  Query.Param.LoadFromStream
}
procedure ZTestCompCoreBugReport.Test707364;
var
  Processor: TZSQLProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Connection := Connection;
    Processor.Script.Text := 'AAAAAAAAAAAA BBBBBBBBBBBBBBB CCCCCCCCCCCCCC';
    CheckException(Processor.Execute, EZSQLException, '', 'SQL Processor must throw exception on invalid script.');
  finally
    Processor.Free;
  end;
end;

procedure ZTestCompCoreBugReport.Test715099;
var
  Query: TZQuery;
  RecNo: Integer;
begin
  if SkipForReason([srClosedBug{$IFDEF FPC}, srNonZeos{$ENDIF}]) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM people';
    Query.Open;
    RecNo := 0;

    repeat
      Inc(RecNo);
      CheckEquals(Query.RecNo, RecNo, 'check Query.RecNo');
    until not Query.FindNext;

    CheckEquals(Query.RecordCount, RecNo, 'check Query.RecordCount');
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #722651.

  There are error with DateTime convertion when field is
  null since ZeosDBO 6.0.7. ( when you added GetChar and SetChar )
}
procedure ZTestCompCoreBugReport.Test722651;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    CheckEquals(False, Query.CachedUpdates);

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Insert a new record }
    Query.SQL.Text := 'INSERT INTO date_values(d_id) VALUES (:id)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);

    { Check for null fields }
    Query.SQL.Text := 'SELECT * FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.Open;
    Check(Query.FieldByName('d_date').IsNull);
    Check(Query.FieldByName('d_time').IsNull);
    Check(Query.FieldByName('d_datetime').IsNull);
  //  Check(Query.FieldByName('d_timestamp').IsNull);
    Query.Close;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM date_values WHERE d_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #000003.

  Problem related with quoted SQL parameters in TZQuery and TZUpdateSQL.
}
procedure ZTestCompCoreBugReport.Test000003;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM Table'
      + ' WHERE F1=:P_1 AND F2=:"P 2" AND F3=:''P 3''';
    CheckEquals(3, Query.Params.Count);
    CheckEquals('P_1', Query.Params[0].Name);
    CheckEquals('P 2', Query.Params[1].Name);
    CheckEquals('P 3', Query.Params[2].Name);
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #773022.

  RecordCount is not changed after Dataset modification
}
procedure ZTestCompCoreBugReport.Test773022;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.CachedUpdates := True;
    Query.SQL.Text := 'select * from people where 1=0';
    Query.Open;

    CheckEquals(0, Query.RecordCount);

    Query.Append;
    CheckEquals(0, Query.RecordCount);
    Query.Cancel;
    CheckEquals(0, Query.RecordCount);

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 0;
    Query.Post;
    CheckEquals(1, Query.RecordCount);
    Query.Append;
    Query.FieldByName('p_id').AsInteger := 0;
    Query.Post;
    CheckEquals(2, Query.RecordCount);

    Query.Edit;
    CheckEquals(0, Query.FieldByName('p_id').AsInteger);
    Query.FieldByName('p_id').AsInteger := 123;
    CheckEquals(123, Query.FieldByName('p_id').AsInteger);
    Query.Cancel;
    CheckEquals(0, Query.FieldByName('p_id').AsInteger);
    CheckEquals(2, Query.RecordCount);

    Query.Delete;
    CheckEquals(1, Query.RecordCount);
    Query.Delete;
    CheckEquals(0, Query.RecordCount);
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #772926.

  I've noticed that when entering invalid info into a field,
  then updating the database you get an error message,
  after this cancel the error and then press the Esc key,
  the invalid data stays in the field. I would assume it
  should revert back to the original data.
}
procedure ZTestCompCoreBugReport.Test772926;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO people (p_id) VALUES (:id)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM people WHERE p_id>=:id ORDER BY p_id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.Open;
    CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
    Query.Next;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);

    { Check for rollback. }
    try
      Query.Edit;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID - 1;
      CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
      Query.Post;
      Fail('Wrong behaviour with duplicated key.');
    except on E: Exception do
      begin
        CheckNotTestFailure(E);
        CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('p_id').AsInteger);
        Query.Cancel;
        CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
      end;
    end;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #772926.

  zquery1 has a working connection, fielddefs, and SQL
  statement, but zquery1.state=dsInactive (not opened).
  zquery1.fieldbyname('field1').asstring:='text' causes an
  access violation in ZAbstractRODataset, because it try
  to execute the assign.
  maybe it should check the state and throw an exception.
  (eg can't modify a closed dataset)

  when zquery1 is opened (state=dsBrowse), there is no
  error. ZAbstractRODataset (or else) should check for
  dsBrowse state too, and should throw an other
  exception.
}
procedure ZTestCompCoreBugReport.Test793351;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.ReadOnly := True;
    Query.SQL.Text := 'select p_id from people';
    Query.Open;

    try
      Query.Fields[0].AsInteger := 0;
      Fail('Wrong SetField behaviour');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

    Query.Close;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select p_id from people where 1=0';
    Query.Open;

    try
      Query.Fields[0].AsInteger := 0;
      Fail('Wrong SetField behaviour');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #804323.

  Calculated fields in read only datasets
  I get the following error, when I work with calculated fields,
  when I want to assign the value to one of those fields: "Operation not
  allowed in a read only dataset."
}
procedure ZTestCompCoreBugReport.Test804323;
var
  I: Integer;
  Query: TZQuery;
  FieldDefs: TFieldDefs;
  CalcField: TField;
begin
  if SkipForReason([srClosedBug{$IFDEF FPC}, srNonZeos{$ENDIF}]) then Exit;

  Query := CreateQuery;
  try
    CheckEquals(False, Query.CachedUpdates);
    Query.ReadOnly := True;
    Query.OnCalcFields := DataSetCalcFields;

    Query.SQL.Text := 'SELECT p_id FROM people';
    FieldDefs := Query.FieldDefs;
    FieldDefs.Update;

    for I := 0 to FieldDefs.Count - 1 do
      FieldDefs[I].CreateField(Query).DataSet := Query;

    CalcField := TIntegerField.Create(nil);
    CalcField.FieldName := 'p_calc';
    CalcField.FieldKind := fkCalculated;
    CalcField.Visible := True;
    CalcField.DataSet := Query;

    Query.Open;

    while not Query.Eof do
    begin
      CheckEquals(Query.RecNo + 100, Query.FieldByName('p_calc').AsInteger);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #804640.

  Calculated Fields of type TCurrencyField returns always 0.00.
}
procedure ZTestCompCoreBugReport.Test804640;
var
  I: Integer;
  Query: TZQuery;
  FieldDefs: TFieldDefs;
  CalcField: TField;
begin
  if SkipForReason([srClosedBug{$IFDEF FPC}, srNonZeos{$ENDIF}]) then Exit;

  Query := CreateQuery;
  try
    CheckEquals(False, Query.CachedUpdates);
    Query.ReadOnly := True;
    Query.OnCalcFields := DataSetCalcFields;

    Query.SQL.Text := 'SELECT p_id FROM people';
    FieldDefs := Query.FieldDefs;
    FieldDefs.Update;

    for I := 0 to FieldDefs.Count - 1 do
      FieldDefs[I].CreateField(Query).DataSet := Query;

    CalcField := TCurrencyField.Create(nil);
    CalcField.FieldName := 'p_calc';
    CalcField.FieldKind := fkCalculated;
    CalcField.Visible := True;
    CalcField.DataSet := Query;

    Query.Open;
    while not Query.Eof do
    begin
      Check(Query.FieldByName('p_calc').AsInteger <> 0);
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bugs report #802548.

  When I call the refresh method and the record with the
  pointer has been changed by outside source (e.g. MySQL
  Control Center) the error "List Index out of Bounds
  (-1)" appears. I try remove any DataSources and
  DataControls, keeping Zeos alone and the error occurs
  on same way. I try use ZReadOnlyQuery and ZQuery, with
  ReadOnly set False and True too, the error is same
  in all cases (with MySQL and MsSQL).
}
procedure ZTestCompCoreBugReport.Test802548;
var
  Query: TZQuery;
  RefreshQuery: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  RefreshQuery := CreateQuery;
  try
    // Query.RequestLive := True;
    // RefreshQuery.RequestLive := True;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    RefreshQuery.SQL.Text := 'SELECT * FROM people';
    RefreshQuery.Open;
    RefreshQuery.Last;
    RefreshQuery.Refresh;

    Query.SQL.Text := 'INSERT INTO people(p_id, p_name) VALUES('
      + IntToStr(TEST_ROW_ID) + ', ''abc'')';
    Query.ExecSQL;

    RefreshQuery.Refresh;
    RefreshQuery.Last;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    RefreshQuery.Refresh;
    RefreshQuery.Last;
    RefreshQuery.Close;

    Check(True);
  finally
    Query.SQL.Text := 'DELETE FROM people WHERE p_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
    Query.Free;
    RefreshQuery.Free;
  end;
end;

{**
  Bugs report #795641.

  AV when TZConnection component is destroyed before linked TDataset
}
procedure ZTestCompCoreBugReport.Test795641;
var
  Connection: TZConnection;     // Attention : local Connection
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;

  Connection.Free;
  Query.Free;

  Check(True);
end;

{**
  Bug report#826886.
  Access Violation with a simple query
}
procedure ZTestCompCoreBugReport.Test826886;
var
  Connection: TZConnection;     // Attention : local Connection
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection := Self.CreateDatasetConnection;
  Query := TZQuery.Create(nil);
  Query.Connection := Connection;
  Query.SQL.Text := 'update people set p_dep_id=p_dep_id where 1=0';

  try
    Connection.Connect;
    Query.ExecSQL;
    Connection.Disconnect;
    Connection.Connect;
    Query.ExecSQL;

    Check(True);
  finally
    Connection.Free;
    Query.Free;
  end;
end;

{**
  Bug report#000004.
  Duplications of commands in SQLProcessor.
}
procedure ZTestCompCoreBugReport.Test000004;
var
  Connection: TZConnection;       // Attention : local Connection
  SQLProcessor: TZSQLProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection := Self.CreateDatasetConnection;
  SQLProcessor := TZSQLProcessor.Create(nil);
  SQLProcessor.Connection := Connection;

  try
    SQLProcessor.Script.Text := 'update people set p_dep_id=p_dep_id where 1=0';
    SQLProcessor.Execute;

    SQLProcessor.Script.Text := 'update people set p_dep_id=p_dep_id where 1=0';
    SQLProcessor.Execute;

    Check(True);
  finally
    Connection.Free;
    SQLProcessor.Free;
  end;
end;

{**
  Bug report#832467.
  Problem with values, who contain ' symbol.
}
procedure ZTestCompCoreBugReport.Test832467;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  // Query.RequestLive := True;
  Query.CachedUpdates := True;
  Query.SQL.Text := 'select p_name from people where 1=0';

  try
    Query.Open;
    Query.Insert;
    Query.Fields[0].AsString := 'abc''def';
    Query.Post;
    Query.Insert;
    Query.Fields[0].AsString := 'abcdef';
    Query.Post;
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'P_Name=''abc''''def''';
    Query.Filtered := True;
    CheckEquals(1, Query.RecordCount);
    CheckEquals('abc''def', Query.Fields[0].AsString);

    Query.Filter := '"p_name"<>''abc''''def''';
//    Query.Filtered := True
    CheckEquals(1, Query.RecordCount);
    CheckEquals('abcdef', Query.Fields[0].AsString);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#830804.
  Problem with values, who contain ' symbol.
}
procedure ZTestCompCoreBugReport.Test830804;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select p_id, p_name, p_resume from people'
    + ' where p_id < 4 order by p_id';

  if ConnectionConfig.Provider in [spIB_FB, spOracle] then
  begin
    try
      Query.Open;
      CheckEquals('P_ID', Query.Fields[0].FieldName);
      CheckEquals('P_NAME', Query.Fields[1].FieldName);
      CheckEquals('P_RESUME', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Fields[0].Index := 1;
      CheckEquals('P_NAME', Query.Fields[0].FieldName);
      CheckEquals('P_ID', Query.Fields[1].FieldName);
      CheckEquals('P_RESUME', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Close;
    finally
      Query.Free;
    end
  end else begin
    try
      Query.Open;
      CheckEquals('p_id', Query.Fields[0].FieldName);
      CheckEquals('p_name', Query.Fields[1].FieldName);
      CheckEquals('p_resume', Query.Fields[2].FieldName);

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Fields[0].Index := 1;
      CheckEquals('p_name', Query.Fields[0].FieldName);
      CheckEquals('p_id', Query.Fields[1].FieldName);
      CheckEquals('p_resume', Query.Fields[2].FieldName);
      Query.Refresh;

      Query.First;
      CheckEquals(1, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(2, Query.FieldByName('p_id').AsInteger);
      Query.Next;
      CheckEquals(3, Query.FieldByName('p_id').AsInteger);

      Query.Close;
    finally
      Query.Free;
    end;
  end;  
end;

{**
  Bug report#833197.
  Refresh problem with filtered data.
}
procedure ZTestCompCoreBugReport.Test833197;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;
    Query.Filter := 'p_name=''Aleksey Petrov''';
    Query.Filtered := True;
    CheckEquals(1, Query.RecordCount);

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#834798.
  CaseInsensitive broken.
}
procedure ZTestCompCoreBugReport.Test834798;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;
    Query.Locate('p_name', 'Kristen Sato', []);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'KRISTEN sato', [loCaseInsensitive]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'Kristen', [loPartialKey]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.First;
    Query.Locate('p_name', 'KRISTEN', [loPartialKey, loCaseInsensitive]);
    Check(Query.Found);
    CheckEquals(3, Query.RecNo);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#839540.
  Logical operations don't work properly in filter expression.
}
procedure ZTestCompCoreBugReport.Test839540;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;
    Query.Filter := 'P_ID=1 OR p_id=2';
    Query.Filtered := True;

    CheckEquals(2, Query.RecordCount);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#840218.
  Strange behaviour with Lookup fields in ver. 6.1.1 alpha.
}
procedure ZTestCompCoreBugReport.Test840218;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from people order by p_id';

  try
    Query.Open;

    CheckEquals('Kristen Sato', VarToStr(Query.Lookup('p_id', 3, 'p_name')));

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#842678.
  AV with ZComponent.bpl when removing ZUpdateSQL from ZQuery.
}
procedure ZTestCompCoreBugReport.Test842678;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  UpdateSQL := TZUpdateSQL.Create(nil);

  try
    Query.UpdateObject := UpdateSQL;
    Query.UpdateObject := nil;

    Check(True);
  finally
    UpdateSQL.Free;
    Query.Free;
  end;
end;

{**
  Bug report#846377.
  EVariantTypeCastError in Locate with loCaseInsensitive when Query
  has NULL fields.
}
procedure ZTestCompCoreBugReport.Test846377;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select p_id, p_name from people where 1=0';

  try
    Query.Open;

    Query.Insert;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'abc';
    Query.Post;

    Query.Insert;
    Query.Fields[0].AsInteger := 2;
    Query.Post;

    Query.First;
    Query.Locate('p_name', 'xyz', [loCaseInsensitive]);

    Check(True);
  finally
    Query.Free;
  end;
end;

{**
  Bug report#880459.
  Access Violation in ZSQLProcessor.Execute method when Connection is not set.
}
procedure ZTestCompCoreBugReport.Test880459;
var
  Processor: TZSQLProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Script.Text := 'SELECT * FROM peoplt';
    try
      Processor.Execute;
      Fail('Processor without Connection must throw exception');
    except
      on E: Exception do
      begin
        Check(not (E is EAccessViolation), 'Exception shouldn''t be an Access Violation');
        CheckNotTestFailure(E);
      end;
    end;
  finally
    Processor.Free;
  end;
end;

{**
  Bug report#000005.
  Access Violation in Query.Open when statement does not return a Resultset.
}
procedure ZTestCompCoreBugReport.Test000005;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'update people set p_name=p_name where 1=0';

    try
      Query.Open;
      Fail('Should be an exception when DML statement is executed via Query.Open');
    except
      on E: Exception do
      begin
        Check(not (E is EAccessViolation), 'Query.Open for DML statement shouldn''t throw Access Violation');
        CheckNotTestFailure(E);
      end;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#887103.
  BeforeScroll and AfterScroll events are not working with SetRecNo.
}
procedure ZTestCompCoreBugReport.Test887103;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.BeforeScroll := DataSetBeforeScroll;
  Query.AfterScroll := DataSetAfterScroll;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;

    FUpdateCounter := 0;
    Query.RecNo := 3;
    CheckEquals(101, FUpdateCounter);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#919401.
  BeforeScroll and AfterScroll events are not working with SetRecNo.
}
procedure ZTestCompCoreBugReport.Test919401;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;

    CheckFalse(Query.UpdatesPending);
    Query.Edit;
    CheckEquals(False, Query.UpdatesPending);
    Query.FieldByName('p_id').AsInteger := 12345;
    Check(Query.UpdatesPending);
    Query.Post;
    Check(Query.UpdatesPending);
    Query.CancelUpdates;
    CheckFalse(Query.UpdatesPending);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#926264.
  TZAbstractRODataset.UpdateStatus bug.
}
procedure ZTestCompCoreBugReport.Test926264;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.CachedUpdates := True;
  // Query.RequestLive := True;
  Query.SQL.Text := 'select * from people';

  try
    Query.Open;
    Query.DisableControls;

    CheckEquals(5, Query.RecordCount);
    while not Query.Eof do
    begin
      CheckEquals(Ord(usUnmodified), Ord(Query.UpdateStatus));
      Query.Next;
    end;

    Query.First;
    Query.Edit;
    Query.FieldByName('p_id').AsInteger := 567;
    Query.FieldByName('p_name').AsString := 'fgh';
    Query.Post;

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 123;
    Query.FieldByName('p_name').AsString := 'abc';
    Query.Post;

    Query.Append;
    Query.FieldByName('p_id').AsInteger := 321;
    Query.FieldByName('p_name').AsString := 'xyz';
    Query.Post;

    Query.Delete;

    Query.ShowRecordTypes := [usModified,usInserted,usDeleted];
    Query.First;
    CheckEquals(Ord(usModified), Ord(Query.UpdateStatus));
    Query.Next;
    CheckEquals(Ord(usInserted), Ord(Query.UpdateStatus));
    Query.Next;
    CheckEquals(Ord(usDeleted), Ord(Query.UpdateStatus));
    Query.Next;
    Check(Query.Eof);
  finally
    Query.EnableControls;
    Query.Free;
  end;
end;

{**
  Bug report#953557.
  First and Last methods call afterscroll repeatedly.
}
procedure ZTestCompCoreBugReport.Test953557;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.BeforeScroll := DataSetBeforeScroll;
  Query.AfterScroll := DataSetAfterScroll;
  Query.SQL.Text := 'select * from people';

  try
    FUpdateCounter := 0;
    Query.Open;
    CheckEquals(100, FUpdateCounter);
    CheckEquals(1, Query.RecNo);
    CheckEquals(5, Query.RecordCount);

    FUpdateCounter := 0;
    Query.RecNo := 3;
    CheckEquals(101, FUpdateCounter);

    FUpdateCounter := 0;
    Query.First;
    CheckEquals(101, FUpdateCounter);
    CheckEquals(1, Query.RecNo);

    FUpdateCounter := 0;
    Query.Last;
    CheckEquals(101, FUpdateCounter);
    CheckEquals(5, Query.RecNo);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#966267.
  TZQuery.OnPostError event doesn't activate.
}
procedure ZTestCompCoreBugReport.Test966267;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipForReason([srClosedBug{$IFDEF FPC}, srNonZeos{$ENDIF}]) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from people';
  Query.OnDeleteError := DataSetOnError;
  Query.OnPostError := DataSetOnError;
  Query.OnEditError := DataSetOnError;
  Query.OnApplyUpdateError := DataSetOnError;

  UpdateSQL := TZUpdateSQL.Create(nil);
  UpdateSQL.InsertSQL.Text := 'xxxx';
  UpdateSQL.ModifySQL.Text := 'xxxx';
  UpdateSQL.DeleteSQL.Text := 'xxxx';
  Query.UpdateObject := UpdateSQL;

  try
    Query.Open;

    FErrorCounter := 0;
    Query.Edit;
    Query.Fields[0].AsInteger := 12345;
    try
      Query.Post;
      Fail('Wrong Error Processing');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
    Check(FErrorCounter > 0);
    Query.Cancel;

    FErrorCounter := 0;
    try
      Query.Delete;
      Fail('Wrong Error Processing');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
    Check(FErrorCounter > 0);

    FErrorCounter := 0;
    Query.Append;
    try
      Query.Post;
      Fail('Wrong Error Processing');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
    Check(FErrorCounter > 0);
    Query.Cancel;
{
    Query.Close;
    Query.CachedUpdates := True;
    Query.Open;

    Query.Append;
    Query.Post;

    FErrorCounter := 0;
    try
      Query.CommitUpdates;
      Fail('Wrong Error Processing');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
    Check(FErrorCounter > 0);
    Query.CancelUpdates;
}
    Query.Close;
  finally
    Query.Free;
    UpdateSQL.Free;
  end;
end;

{**
  Bug report#985629.
  Locate and Lookup don't find float fields.
}
procedure ZTestCompCoreBugReport.Test985629;
var
  Query: TZQuery;
begin
  if SkipForReason([srClosedBug]) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select c_cost from cargo order by c_id';

  try
    Query.Open;
    Query.Locate('c_cost', 643.11, []);
    Check(Query.Found, 'Query.Locate 643.11');
    CheckEquals(3, Query.RecNo);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report Float fields precision
}
procedure ZTestCompCoreBugReport.TestFloatPrecision;
var
  Query: TZQuery;
begin
  if SkipForReason([srClosedBug]) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'Insert into number_values(n_id, n_money) values(999999,643.11)';
    Query.ExecSQL;

    try
      Query.SQL.Text := 'select n_money from number_values where n_id=999999';
      Query.Open;
      // uses format to avoid local separator differences
      CheckEquals(trim(Format('%8.2f', [643.11])),Query.Fields[0].AsString);

      Query.Close;
    finally
      Query.SQL.Text := 'delete from number_values where n_id=999999';
      Query.execSql;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#995080.
  "List Index Out of Bounds" exception in Query.UpdateStatus.
}
procedure ZTestCompCoreBugReport.Test995080;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.CachedUpdates := True;
  Query.SQL.Text := 'select * from people where 1=0';

  try
    Query.Open;

    Check(Query.UpdateStatus = usUnmodified);

    Query.Append;
    Check(Query.UpdateStatus = usUnmodified);

    Query.Post;
    Check(Query.UpdateStatus = usInserted);

    Query.CancelUpdates;
    Check(Query.UpdateStatus = usUnmodified);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#996283.
  TZSQLStrings Text property problem.
}
procedure ZTestCompCoreBugReport.Test996283;
var
  Query: TZReadOnlyQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateReadOnlyQuery;
  try
    Check(Query.ParamCheck);
    CheckEquals(0, Query.Params.Count);

    Query.SQL.Text := 'xxx :aaa, :bbb, :ccc xxx';
    CheckEquals(3, Query.Params.Count);
    CheckEquals('aaa', Query.Params[0].Name);
    CheckEquals('bbb', Query.Params[1].Name);
    CheckEquals('ccc', Query.Params[2].Name);

    Query.SQL.Text := 'xxx :xyz xxx';
    CheckEquals(1, Query.Params.Count);
    CheckEquals('xyz', Query.Params[0].Name);

    Query.ParamCheck := False;
    CheckEquals(0, Query.Params.Count);

    Query.SQL.Text := 'xxx :aaa, :bbb, :ccc xxx';
    CheckEquals(0, Query.Params.Count);
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1004534.
  Oracle 9 SQL Syntax error when query contains 0x0A symbols.
}
procedure ZTestCompCoreBugReport.Test1004534;
var
  Query: TZReadOnlyQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateReadOnlyQuery;
  try
    Check(not Query.Active);
    try
      Query.RecNo := 1;
      Fail('SetRecNo must throw exception when Active=False');
    except
      on E: Exception do
      begin
        CheckNotTestFailure(E);
        Check(E is EDatabaseError);
      end;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1012751.
  ErrorHandling in Dataset.SetRecNo when Active=False.
}
procedure ZTestCompCoreBugReport.Test1012751;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * '#10'from cargo'#13' order by'#10#13' c_id';

  try
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1022415.
  Parameters in subquery doesn't exist.
}
procedure ZTestCompCoreBugReport.Test1022415;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'SELECT * FROM table1 WHERE field1 NOT IN ('
    + 'SELECT field1 FROM table2 WHERE field2 = :Param1)';

  try
    CheckEquals(1, Query.Params.Count);
    CheckEquals('Param1', Query.Params[0].Name);
  finally
    Query.Free;
  end;
end;

{**
  Bug report#1049821.
  ZSQLScriptParser drops last statement when no closing ';'.
}
procedure ZTestCompCoreBugReport.Test1049821;
var
  Processor: TZSQLProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Connection := Connection;
    Processor.Script.Text := 'SELECT * FROM people;'#10#13'SELECT * FROM cargo';
    Processor.Parse;
    CheckEquals(2, Processor.StatementCount);
    CheckEquals('SELECT * FROM people', Processor.Statements[0]);
    CheckEquals('SELECT * FROM cargo', Processor.Statements[1]);
    Processor.Execute;
  finally
    Processor.Free;
  end;
end;

{**
  Bug report#1045500.
  Exception when TZSQLMetadata is opened twice.
}
procedure ZTestCompCoreBugReport.Test1045500;
var
  Metadata: TZSQLMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  Metadata := TZSQLMetadata.Create(nil);
  try
    Metadata.Connection := Connection;
    Metadata.MetadataType := mdTables;
    Metadata.Active := True;
    Metadata.Active := False;
    Metadata.Active := True;

    Check(True);
  finally
    Metadata.Free;
  end;
end;

{**
  Bug report#1036916.
  Incorrect transfer \' to the Query params.
}
procedure ZTestCompCoreBugReport.Test1036916;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO equipment (eq_id, eq_name) VALUES (:id, :name)';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ParamByName('name').AsString := 'ab''cd''ef';
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 1;
    Query.ParamByName('name').AsString := 'ab\cd\ef';
    Query.ExecSQL;
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ParamByName('name').AsString := 'ab\''cd\''ef';
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM equipment WHERE eq_id>=:id ORDER BY eq_id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.Open;
    CheckEquals(TEST_ROW_ID - 2, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab''cd''ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\cd\ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\''cd\''ef', Query.FieldByName('eq_name').AsString);

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompCoreBugReport.TestParamUx;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO equipment (eq_id, eq_name) VALUES (:u, :u1)';
    //EH: i don't understand the string assignment here. however..
    if Query.Connection.DbcConnection.GetServerProvider = spASE
    then Query.ParamByName('u').AsInteger := TEST_ROW_ID - 2
    else Query.ParamByName('u').AsString := IntToStr(TEST_ROW_ID - 2);
    Query.ParamByName('u1').AsString := 'ab''cd''ef';
    Query.ExecSQL;
    Query.ParamByName('u').AsInteger := TEST_ROW_ID - 1;
    Query.ParamByName('u1').AsString := 'ab\cd\ef';
    Query.ExecSQL;
    Query.ParamByName('u').AsInteger := TEST_ROW_ID;
    Query.ParamByName('u1').AsString := 'ab\''cd\''ef';
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM equipment WHERE eq_id = :u or eq_id = :u +1 or eq_id = :u +2  ORDER BY eq_id';
    Query.ParamByName('u').AsString := IntToStr(TEST_ROW_ID-2);
    Query.Open;
    CheckEquals(TEST_ROW_ID - 2, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab''cd''ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID - 1, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\cd\ef', Query.FieldByName('eq_name').AsString);
    Query.Next;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('eq_id').AsInteger);
    CheckEquals('ab\''cd\''ef', Query.FieldByName('eq_name').AsString);

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM equipment WHERE eq_id>=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID - 2;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompCoreBugReport.TestTicket228;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.Connection.Connect;
    Query.SQL.Text := 'SELECT * from people';
    if (Connection.Protocol <> 'OleDB') then //not reolvable
      Connection.StartTransaction;
    Query.Open;
    //Connection.Commit; //<- this crash with FB/IB and MSSQL(oledb,odbc,ado) only
    Check(Query.RecordCount = 5);
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompCoreBugReport.TestSF238;
const TEST_ROW_ID = 1000;
var Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Query := CreateQuery;
  try
    CheckEquals(Ord(tiNone), Ord(Connection.TransactIsolationLevel));
    Connection.Disconnect;
    Connection.AutoCommit := True;
    Connection.TransactIsolationLevel := tiSerializable;
    Connection.Connect;
    Query.SQL.Text := 'select p_id, p_name from people where p_id > '+IntToStr(TEST_ROW_ID);
    Query.Open;
    CheckEquals(1, Connection.StartTransaction, 'The txn-level');
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID+1;
    Query.Fields[1].AsString := 'marsupilami';
    Query.Post;
    CheckEquals(2, Connection.StartTransaction, 'The txn-level');
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID+2;
    Query.Fields[1].AsString := 'FrOst';
    Query.Post;
    CheckEquals(3, Connection.StartTransaction, 'The txn-level');
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID+3;
    Query.Fields[1].AsString := 'Mark';
    Query.Post;
    CheckEquals(4, Connection.StartTransaction, 'The txn-level');
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID+4;
    Query.Fields[1].AsString := 'EgonHugeist';
    Query.Post;
    CheckEquals(4, Query.RecordCount, 'the record count after rollback');
    Query.Close;
    Query.Open;
    CheckEquals(4, Query.RecordCount, 'the record count after rollback');
    Query.Close;
    Connection.Rollback;
    Query.Open;
    CheckEquals(3, Query.RecordCount, 'the record count after rollback');
    Query.Close;
    Connection.Rollback;
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'the record count after rollback');
    Query.Close;
    Connection.Rollback;
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'the record count after rollback');
    Query.Close;
    Connection.Rollback;
    Query.Open;
    CheckEquals(0, Query.RecordCount, 'the record count after rollback');
    Query.Close;
  finally
    Query.SQL.Text := 'delete from people where p_id > '+IntToStr(TEST_ROW_ID);
    Query.ExecSQL;
    Query.Free;
  end;
end;

procedure ZTestCompCoreBugReport.TestSF418_SortedFields;
var
  Query: TZReadOnlyQuery;
begin
  Connection.Connect;
  try
    Connection.ExecuteDirect('delete from number_values where n_id in (1001, 1002, 1003, 1003, 1004, 1005)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1001, 0.0)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1002, 1.3376)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1003, 0.8246)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1004, 0.8459)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1005, 0.5684)');

    Query := TZReadOnlyQuery.Create(nil);
    try
      Query.Connection := Connection;
      Query.SQL.Text := 'select n_id, n_numeric from number_values where n_id in (1001, 1002, 1003, 1004, 1005) order by n_id';
      Query.Open;
      Query.SortedFields := 'n_numeric';

      Query.First;
      CheckEquals(1001, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1005, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1003, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1004, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1002, Query.FieldByName('n_id').AsInteger);
      Query.Next;
    finally
      FreeAndNil(Query);
    end;
  finally
    Connection.Disconnect;
  end;
end;

(* I use the MS SQL-Server 2014 and 2016. I work in the ZConnection with the
  protocol "ado". In the ZQuery, an SQL command with one parameter is executed.
  The query does not return any result (Recordcount = 0). Now ZQuery is closed,
  the parameter changed and ZQuery opened again. Again there is no result,
  although the query should result in several lines. When debugging,
  I noticed that the changed parameter was not adopted
  (down to the depth of the sources). It works perfectly with the
  "mssql" protocol. If the SQL command is set before the second
  parameter transfer, it also works.

Example:
ZQuery.SQL.Text: = 'select * from article where artno like :0';
ZQuery.Params [0] .asString: = 'A';
ZQuery.Open;
// ZQuery.Recordcount = 0 (OK)
...
ZQuery.Close;
ZQuery.Params [0] .asString: = 'B%';
ZQuery.Open;
// ZQuery.Recordcount = 0 (not OK => 'B%' was not set)
*)
procedure ZTestCompCoreBugReport.TestSF434;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.Connection.Connect;
  try
    CheckEquals(False, Query.CachedUpdates);

    Query.SQL.Text := 'SELECT * FROM people where p_name like :'+Char(Ord('0')+Ord(Connection.DbcConnection.GetServerProvider = spPostgreSQL));
    Query.Params[0].AsString := 'A';
    Query.Open;
    CheckEquals(0, Query.RecordCount, 'there is no Row');
    Query.Close;
    Query.Params[0].AsString := 'A%';
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'there are two rows');
  finally
    Query.Free;
  end;
end;

{Assume the following code:

Query.SQL.Text := 'select * from users where userid = :userid';
Query.ParamByName('userid').AsString := 'abc';
Query.SQL.Text := 'select * from users where username = :userid';

It seems that the third line reinitializes the parameter values to be empty.
This worked in the past. I am not sure, which revision introduced the change.
}
procedure ZTestCompCoreBugReport.TestSF469;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from users where userid = :userid';
    Query.ParamByName('userid').AsString := 'abc';
    CheckEquals('abc', Query.ParamByName('userid').AsString);
    Query.SQL.Text := 'select * from users where username = :userid';
    CheckEquals('abc', Query.ParamByName('userid').AsString);
  finally
    Query.Free;
  end;
end;

(* see https://sourceforge.net/p/zeoslib/tickets/493/
FPC/Lazarus

this code generate AV on FreeAndNil(FConnection1);

procedure TForm1.Button1Click(Sender: TObject);
var
FConnection1: TZConnection;
begin
FConnection1:=TZConnection.Create(Self);
FreeAndNil(FConnection1);
end;
*)
procedure ZTestCompCoreBugReport.TestSF493;
var FConnection1: TZConnection;
begin
  FConnection1 := TZConnection.Create(Connection);  //need an owner for this test
  FreeAndNil(FConnection1);
end;

procedure ZTestCompCoreBugReport.TestSF418_IndexFieldNames;
var
  Query: TZReadOnlyQuery;
begin
  Connection.Connect;
  try
    Connection.ExecuteDirect('delete from number_values where n_id in (1001, 1002, 1003, 1003, 1004, 1005)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1001, 0.0)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1002, 1.3376)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1003, 0.8246)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1004, 0.8459)');
    Connection.ExecuteDirect('insert into number_values (n_id, n_numeric) values (1005, 0.5684)');

    Query := TZReadOnlyQuery.Create(nil);
    try
      Query.Connection := Connection;
      Query.SQL.Text := 'select n_id, n_numeric from number_values where n_id in (1001, 1002, 1003, 1004, 1005) order by n_id';
      Query.Open;
      Query.IndexFieldNames := 'n_numeric';

      Query.First;
      CheckEquals(1001, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1005, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1003, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1004, Query.FieldByName('n_id').AsInteger);
      Query.Next;
      CheckEquals(1002, Query.FieldByName('n_id').AsInteger);
      Query.Next;
    finally
      FreeAndNil(Query);
    end;
  finally
    Connection.Disconnect;
  end;
end;

procedure ZTestCompCoreBugReport.TestSF270_1;
var
  Query: TZQuery;
  PersonName: String;
  Succeeded: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * from people';
    Query.Open;
    CheckEquals(5, Query.RecordCount, 'Expected to get exactly fife records from the people table.');
    PersonName := Query.FieldByName('p_name').AsString;
    Query.Edit;
    Query.FieldByName('p_name').AsString := '';
    Query.FieldByName('p_name').AsString := PersonName;
    Succeeded := False;
    try
      Query.Post;
      Succeeded := True;
    finally
      if not Succeeded then
        Query.Cancel;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompCoreBugReport.TestSF270_2;
var
  Query: TZQuery;
  PersonName: String;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    UpdateSQL := TZUpdateSQL.Create(nil);
    Query.UpdateObject := UpdateSQL;
    UpdateSQL.DeleteSQL.Text := 'delete from people where p_id = :old_p_id';
    UpdateSQL.InsertSQL.Text := 'insert into people (p_id, p_name) values (:new_p_id, :new_p_name)';
    UpdateSQL.ModifySQL.Text := 'update people set p_id = :new_p_id, p_name = :new_p_name where p_id = :old_p_id';
    Query.SQL.Text := 'SELECT p_id, p_name from people';
    Query.Open;
    CheckEquals(5, Query.RecordCount, 'Expected to get exactly fife records from the people table.');
    PersonName := Query.FieldByName('p_name').AsString;
    Query.Edit;
    Query.FieldByName('p_name').AsString := '';
    Query.FieldByName('p_name').AsString := PersonName;
    try
      Query.Post;
    except
      Query.Cancel;
      raise;
    end;
    Query.Close;
  finally
    Query.Free;
    FreeAndNil(UpdateSQL);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF279CalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('calculated').AsString :=
    DataSet.FieldByName('dep_name').AsString +
    ' ' +
    DataSet.FieldByName('dep_address').AsString;
end;

procedure ZTestCompCoreBugReport.TestSF279;
const
  FieldName = 'calculated';
var
  Query: TZQuery;
  FieldDef: TFieldDef;
  X: Integer;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from department';
    Query.FieldDefs.Clear;
    Query.Fields.Clear;
    Query.FieldDefs.Update;
    FieldDef := Query.FieldDefs.AddFieldDef;
    FieldDef.DataType := ftString;
    FieldDef.Size := 280;
    FieldDef.Name := FieldName;
    for x := 0 to Query.FieldDefs.Count - 1
    do Query.FieldDefs.Items[x].CreateField(Query);
    Query.FieldByName(FieldName).FieldKind := fkCalculated;
    Query.OnCalcFields := TestSF279CalcFields;
    Query.Open;
    Check(Assigned(Query.FindField(FieldName)), 'Checking, if the calculated field really exists.');
    try
      Query.Filter := 'calculated LIKE ' + QuotedStr('*Krasnodar*');
      Query.Filtered := True;
    finally
      Query.Close;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF286_getBigger;
var
 x: Integer;
 y: Integer;
 Metadata: TZSQLMetadata;
begin
  Metadata := TZSQLMetadata.Create(nil);
  try
    Metadata.Connection := Connection;
    // this part never should fail.
    Metadata.MetadataType := mdTables;
    Metadata.Open;
    CheckEquals(Length(TableColumnsDynArray), Metadata.FieldCount, 'Checking if Metadata object has the correct count of columns for mdTables.');
    y := Low(TableColumnsDynArray);
    for x := Low(TableColumnsDynArray) to High(TableColumnsDynArray)
    do CheckEquals(TableColumnsDynArray[x].Name, Metadata.Fields[x-y].FieldName, 'Checking if field name is as expected for mdTables.');
    Metadata.Close;

    // here it fails if we have a bug.
    Metadata.MetadataType := mdColumns;
    Metadata.Open;
    CheckEquals(Length(TableColColumnsDynArray), Metadata.FieldCount, 'Checking if Metadata object has the correct count of columns for mdTables.');
    y := Low(TableColColumnsDynArray);
    for x := Low(TableColColumnsDynArray) to High(TableColColumnsDynArray)
    do CheckEquals(TableColColumnsDynArray[x].Name, Metadata.Fields[x-y].FieldName, 'Checking if field name is as expected for mdColumns.');
    Metadata.Close;
  finally
    FreeAndNil(Metadata);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF286_getSmaller;
var
 x: Integer;
 y: Integer;
 Metadata: TZSQLMetadata;
begin
  Metadata := TZSQLMetadata.Create(nil);
  try
    Metadata.Connection := Connection;
    // this part never should fail.
    Metadata.MetadataType := mdColumns;
    Metadata.Open;
    CheckEquals(Length(TableColColumnsDynArray), Metadata.FieldCount, 'Checking if Metadata object has the correct count of columns for mdTables.');
    y := Low(TableColColumnsDynArray);
    for x := Low(TableColColumnsDynArray) to High(TableColColumnsDynArray)
    do CheckEquals(TableColColumnsDynArray[x].Name, Metadata.Fields[x-y].FieldName, 'Checking if field name is as expected for mdColumns.');
    Metadata.Close;

    // here it fails if we have a bug.
    Metadata.MetadataType := mdTables;
    Metadata.Open;
    CheckEquals(Length(TableColumnsDynArray), Metadata.FieldCount, 'Checking if Metadata object has the correct count of columns for mdTables.');
    y := Low(TableColumnsDynArray);
    for x := Low(TableColumnsDynArray) to High(TableColumnsDynArray)
    do CheckEquals(TableColumnsDynArray[x].Name, Metadata.Fields[x-y].FieldName, 'Checking if field name is as expected for mdTables.');
    Metadata.Close;
  finally
    FreeAndNil(Metadata);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF301;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from equipment';
    Query.SortedFields := 'eq_date';
    Query.Open;
    Check(true);
  finally
    FreeAndNil(Query);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF310_JoinedUpdate_ProviderFlags;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  Check(Query <> nil);
  try
    Query.Connection.Connect;
    Query.Connection.StartTransaction;
    //Exit;
    Query.SQL.Text := 'select c.c_id, c.c_name, c.c_seal, c.c_date_came, '+
      'c.c_date_out, c.c_weight, c.c_width, c.c_height, c.c_cost, '+
      'c.c_attributes, dep.dep_name, dep.dep_shname, dep_address '+
      'from cargo c left join department dep on dep.dep_id = c.c_dep_id '+
      'where c.c_id = '+IntToStr(TEST_ROW_ID);
    Query.Open;
    Query.FieldByName('dep_name').ReadOnly := True;
    Query.FieldByName('dep_shname').ReadOnly := True;
    Query.FieldByName('dep_address').ReadOnly := True;
    Query.Append;
    Query.FieldByName('c_id').AsInteger := TEST_ROW_ID;
    Query.Post;
    Query.Close;
    Query.Unprepare; //now test updates using provider flags -> Refresh fields
    Query.WhereMode := wmWhereAll; //to test if the joined fields are ignored in the where clause
    Query.Open;
    CheckFalse(Query.Eof, 'there is a row now');
    Query.Edit;
    Query.FieldByName('dep_name').ProviderFlags := [];
    Query.FieldByName('dep_shname').ProviderFlags := [];
    Query.FieldByName('dep_address').ProviderFlags := [];
    Query.FieldByName('c_name').AsString := 'Ticket310';
    Query.Post;
  finally
    Query.Connection.Rollback;
    FreeAndNil(Query);
  end;
end;

procedure ZTestCompCoreBugReport.TestSF310_JoinedUpdate_SetReadOnly;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  Check(Query <> nil);
  try
    Query.Connection.Connect;
    Query.Connection.StartTransaction;
    Query.SQL.Text := 'select c.c_id, c.c_name, c.c_seal, c.c_date_came, '+
      'c.c_date_out, c.c_weight, c.c_width, c.c_height, c.c_cost, '+
      'c.c_attributes, dep.dep_name, dep.dep_shname, dep_address '+
      'from cargo c left join department dep on dep.dep_id = c.c_dep_id '+
      'where c.c_id = '+IntToStr(TEST_ROW_ID);
    Query.Open;
    Query.FieldByName('dep_name').ReadOnly := True;
    Query.FieldByName('dep_shname').ReadOnly := True;
    Query.FieldByName('dep_address').ReadOnly := True;
    Query.Append;
    Query.FieldByName('c_id').AsInteger := TEST_ROW_ID;
    Query.Post;
  finally
    Query.Connection.Rollback;
    FreeAndNil(Query);
  end;
end;

const {Test Strings}
  Str1: UnicodeString = 'This license, the Lesser General Public License, applies to some specially designated software packages--typically libraries--of the Free Software Foundation and other authors who decide to use it.  You can use it too, but we suggest you first think ...';
  // some dull text in Russian
  Str2: UnicodeString = #$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430+
                      #$0438#$0431#$043E#$043B#$0435#$0435#$0020#$0442#$0440#$0438#$0432+
                      #$0438#$0430#$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434+
                      #$0430#$0447#$002C#$0020#$0440#$0435#$0448#$0430#$0435#$043C#$044B+
                      #$0445#$0020#$043C#$043D#$043E#$0433#$0438#$043C#$0438#$0020#$043A+
                      #$043E#$043B#$043B#$0435#$043A#$0442#$0438#$0432#$0430#$043C#$0438+
                      #$0020#$043F#$0440#$043E#$0433#$0440#$0430#$043C#$043C#$0438#$0441+
                      #$0442#$043E#$0432#$002C#$0020#$044F#$0432#$043B#$044F#$0435#$0442+
                      #$0441#$044F#$0020#$043F#$043E#$0441#$0442#$0440#$043E#$0435#$043D+
                      #$0438#$0435#$0020#$0438#$043D#$0444#$043E#$0440#$043C#$0430#$0446+
                      #$0438#$043E#$043D#$043D#$043E#$0439#$0020#$0441#$0438#$0441#$0442+
                      #$0435#$043C#$044B#$0020#$0434#$043B#$044F#$0020#$0430#$0432#$0442+
                      #$043E#$043C#$0430#$0442#$0438#$0437#$0430#$0446#$0438#$0438#$0020+
                      #$0431#$0438#$0437#$043D#$0435#$0441#$002D#$0434#$0435#$044F#$0442+
                      #$0435#$043B#$044C#$043D#$043E#$0441#$0442#$0438#$0020#$043F#$0440+
                      #$0435#$0434#$043F#$0440#$0438#$044F#$0442#$0438#$044F#$002E#$0020+
                      #$0412#$0441#$0435#$0020#$0430#$0440#$0445#$0438#$0442#$0435#$043A+
                      #$0442#$0443#$0440#$043D#$044B#$0435#$0020#$043A#$043E#$043C#$043F+
                      #$043E#$043D#$0435#$043D#$0442#$044B#$0020#$0028#$0431#$0430#$0437+
                      #$044B#$0020#$0434#$0430#$043D#$043D#$044B#$0445#$002C#$0020#$0441+
                      #$0435#$0440#$0432#$0435#$0440#$0430#$0020#$043F#$0440#$0438#$043B+
                      #$043E#$0436#$0435#$043D#$0438#$0439#$002C#$0020#$043A#$043B#$0438+
                      #$0435#$043D#$0442#$0441#$043A#$043E#$0435#$0020#$002E#$002E#$002E;
  Str3: UnicodeString = #$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430#$0438#$0431#$043E#$043B#$0435#$0435;
  Str4: UnicodeString = #$0442#$0440#$0438#$0432#$0438#$0430#$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434#$0430#$0447;
  Str5: UnicodeString = #$0440#$0435#$0448#$0430#$0435#$043C#$044B#$0445#$0020#$043C#$043D#$043E#$0433#$0438#$043C#$0438;
  (*Str6: UnicodeString = #$043A#$043E#$043B#$043B#$0435#$043A#$0442#$0438#$0432#$0430#$043C+
                      #$0438#$0020#$043F#$0440#$043E#$0433#$0440#$0430#$043C#$043C#$0438#$0441#$0442#$043E#$0432;*)

{$IFDEF MSWINDOWS}
procedure ZTestCompCoreBugReportMBCs.TestUnicodeBehavior;
var
  Query: TZQuery;
  StrStream1: TMemoryStream;
  SL: TStringList;
  ConSettings: PZConSettings;
  {Str1, }Str2, Str3{, Str4, Str5, Str6}: UnicodeString;
  CP: Word;
  {$IFDEF UNICODE}
  DSCCString: RawByteString;
  {$ENDIF}
begin
  (*Str1 := 'This is an ASCII text and should work on any database.';
  // This test requires the database to either use the same codepage as the
  // computer. The strings need to fit into unicode and the local codepage.
  // now let's create some strings that are not in the ASCII range
  // rules:
  // String 2 Starts with String 3
  // String 2 ends with String 4
  // String 5 is in the middle of String 2*)
  Str2 := Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199)+
          Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223)+
          Chr(200)+Chr(201)+Chr(202)+Chr(203)+Chr(204)+Chr(205)+Chr(206)+Chr(207)+
          Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215);
  Str3 := Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199);
  (*Str4 := Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215);
  Str5 := Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223);
  Str6 := Chr(232)+Chr(233)+Chr(234)+Chr(235)+Chr(236)+Chr(237)+Chr(238)+Chr(239);*)

  StrStream1 := TMemoryStream.Create;
  SL := TStringList.Create;
  Query := CreateQuery;
  try
    Query.Connection.Connect;
    Check(Query.Connection.Connected, 'Connected');
    ConSettings := connection.DbcConnection.GetConSettings;
    CP := ConSettings.ClientCodePage.CP;
    Check(CP <> 0, 'The Codepage can not be zero');
    with Query do
    begin
      SQL.Text := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
      ExecSQL;
      ConSettings := Connection.DbcConnection.GetConSettings;
      //bugreport of mrLion

      SQL.Text := 'INSERT INTO people(p_id, p_name, p_resume)'+
        ' VALUES (:P_ID, :P_NAME, :P_RESUME)';
      ParamByName('P_ID').AsInteger := TEST_ROW_ID;
      ParamByName('P_NAME').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF}  := Str3;
      CheckEquals(3, Query.Params.Count, 'Param.Count');
      SL.Text := GetDBTestString(Str2, ttParam);
      {$IFDEF UNICODE} //the unicode compiler are converting the streams into DefaultSystemCodePage
      if (CP <> DefaultSystemCodePage) and (ConSettings.ClientCodePage.Encoding <> ceUTF16) then begin
        DSCCString := ZUnicodeToRaw(SL.Text, CP);
        StrStream1.Write(Pointer(DSCCString)^, Length(DSCCString));
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

        CheckEquals(Str2+UnicodeString(LineEnding), FieldByName('P_RESUME'));
        {$IFDEF UNICODE}
        CheckEquals(Str3, FieldByName('P_NAME').AsString, 'Field(P_NAME) as String');
        {$ELSE}
        CheckEquals(Str3, FieldByName('P_NAME'));
        {$ENDIF}
      finally
        SQL.Text := 'DELETE FROM people WHERE p_id = :p_id';
        CheckEquals(1, Params.Count);
        Params[0].DataType := ftInteger;
        Params[0].AsInteger := TEST_ROW_ID;
        ExecSQL;
      end;
    end;
  finally
    FreeAndNil(Query);
    FreeAndNil(SL);
    FreeAndNil(StrStream1);
  end;
end;

procedure ZTestCompCoreBugReportMBCs.TestNonAsciiChars;
const TestRowID = 248;
var
  Query: TZQuery;
  RowCounter: Integer;
  I: Integer;
  Str1, Str2, Str3, Str4, Str5, Str6: UnicodeString;
  procedure InsertValues(s_char, s_varchar, s_nchar, s_nvarchar: UnicodeString);
  begin
    Query.ParamByName('s_id').AsInteger := TestRowID+RowCounter;
    if Query.Connection.ControlsCodePage = cCP_UTF16 then begin
      Query.ParamByName('s_char').{$IFDEF WITH_PARAM_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := s_char;
      Query.ParamByName('s_varchar').{$IFDEF WITH_PARAM_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := s_varchar;
      Query.ParamByName('s_nchar').{$IFDEF WITH_PARAM_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := s_nchar;
      Query.ParamByName('s_nvarchar').{$IFDEF WITH_PARAM_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := s_nvarchar;
    end else begin
      Query.ParamByName('s_char').AsString := GetDBTestString(s_char, ttParam);
      Query.ParamByName('s_varchar').AsString := GetDBTestString(s_varchar, ttParam);
      Query.ParamByName('s_nchar').AsString := GetDBTestString(s_nchar, ttParam);
      Query.ParamByName('s_nvarchar').AsString := GetDBTestString(s_nvarchar, ttParam);
    end;
    Query.ExecSQL;
    inc(RowCounter);
  end;
  function ConcatSQL(const Values: Array of SQLString): String;
  var I: Integer;
    L: Integer;
    S,D: PChar;
  begin
    L := 0;
    Result := '';
    for I := Low(Values) to High(Values) do
      Inc(L, Length(Values[i]));
    SetLength(Result, L);
    D := Pointer(Result);
    for I := Low(Values) to High(Values) do begin
      S := Pointer(Values[i]);
      L := Length(Values[i]);
      Move(S^, D^, L{$IFDEF UNICODE} shl 1{$ENDIF});
      Inc(D, L);
    end;
  end;

begin
  Str1 := 'This is an ASCII text and should work on any database.';
  // This test requires the database to either use the same codepage as the
  // computer. The strings need to fit into unicode and the local codepage.
  // now let's create some strings that are not in the ASCII range
  // rules:
  // String 2 Starts with String 3
  // String 2 ends with String 4
  // String 5 is in the middle of String 2
  Str2 := UnicodeString(AnsiString(Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199)+
          Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223)+
          Chr(200)+Chr(201)+Chr(202)+Chr(203)+Chr(204)+Chr(205)+Chr(206)+Chr(207)+
          Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215)));
  Str3 := UnicodeString(AnsiString(Chr(192)+Chr(193)+Chr(194)+Chr(195)+Chr(196)+Chr(197)+Chr(198)+Chr(199)));
  Str4 := UnicodeString(AnsiString(Chr(208)+Chr(209)+Chr(210)+Chr(211)+Chr(212)+Chr(213)+Chr(214)+Chr(215)));
  Str5 := UnicodeString(AnsiString(Chr(216)+Chr(217)+Chr(218)+Chr(219)+Chr(220)+Chr(221)+Chr(222)+Chr(223)));
  Str6 := UnicodeString(AnsiString(Chr(232)+Chr(233)+Chr(234)+Chr(235)+Chr(236)+Chr(237)+Chr(238)+Chr(239)));

  Query := CreateQuery;
  Connection.Connect;  //DbcConnection needed
  Check(Connection.Connected);
  try
    RowCounter := 0;
    Query.SQL.Text := 'Insert into string_values (s_id, s_char, s_varchar, s_nchar, s_nvarchar)'+
      ' values (:s_id, :s_char, :s_varchar, :s_nchar, :s_nvarchar)';
    InsertValues(str1, str2, str1, str2);
    InsertValues(str3, str3, str3, str3);
    InsertValues(str4, str4, str4, str4);
    InsertValues(str5, str5, str5, str5);
    InsertValues(str6, str6, str6, str6);

    Query.SQL.Text := 'select * from string_values where s_id > '+IntToStr(TestRowID-1);
    Query.Open;
    CheckEquals(True, Query.RecordCount = 5);
    if ProtocolType in [protASA, protASACAPI] then //ASA has a limitation of 125chars for like statements
      Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str2, ttSQL, 125),'%'''])
    else
      Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str2, ttSQL),'%''']);
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'RowCount of Str2 '+Protocol);
    Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str3, ttSQL),'%''']);
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'RowCount of Str3  '+Protocol);
    Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str4, ttSQL),'%''']);
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'RowCount of Str4 '+Protocol);
    Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str5, ttSQL),'%''']);
    Query.Open;
    CheckEquals(2, Query.RecordCount, 'RowCount of Str5 '+Protocol);
    Query.SQL.Text := ConcatSQL(['select * from string_values where s_varchar like ''%',GetDBTestString(Str6, ttSQL),'%''']);
    Query.Open;
  finally
    for i := TestRowID to TestRowID+RowCounter do
    begin
      Query.SQL.Text := 'delete from string_values where s_id = '+IntToStr(i);
      Query.ExecSQL;
    end;
    Query.Free;
  end;
end;
{$ENDIF}
procedure ZTestCompCoreBugReportMBCs.TestUnicodeChars;
var
  Query: TZQuery;
  CP: Word;
  ConSettings: PZConSettings;
  {$IFNDEF Unicode}
  Str6: UnicodeString;
  {$ENDIF}
const
  {$IFDEF Unicode}
  Str6: UnicodeString = #$5317#$4EAC#$0020#$6771#$4EAC; // Beijing + Space + Tokyo
  {$ELSE}
  Words: array[0..4] of Word = ($5317,$4EAC,$0020,$6771,$4EAC); // Beijing + Space + Tokyo
  {$ENDIF}

  procedure InsertValue(const id: Integer; const value: UnicodeString);
  begin
    Query.ParamByName('id').AsInteger := id;
    Query.ParamByName('string').{$IFDEF WITH_PARAM_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := value;
    Query.ExecSQL;
  end;
begin
  Query := CreateQuery;
  Query.Connection.Connect;
  Check(Query.Connection.Connected);
  ConSettings := Connection.DbcConnection.GetConSettings;

  {$IFNDEF Unicode}
  System.SetString(Str6, PWideChar(@Words[0]), 5);
  {$ENDIF}
  try
    {no unicode strings or utf8 allowed}
    if ((Connection.ControlsCodePage = cGET_ACP)
{$IF defined(MSWINDOWS) and not (defined(LCL) and defined(WITH_DEFAULTSYSTEMCODEPAGE))} //LCL is hacking the default-systemcodepage so they can pass this test pass nice (utf8 to Widcharmove)
          {$IFNDEF UNICODE}or (Connection.ControlsCodePage = cCP_UTF8){$ENDIF}
{$IFEND}
      ) and not ((ZOSCodePage = zCP_UTF8) or (ZOSCodePage = zCP_EUC_CN) or (ZOSCodePage = zCP_csISO2022JP)) then
      Exit;
    CP := ConSettings.ClientCodePage.CP;
    if not ((CP = zCP_UTF8) or (CP = zCP_UTF16) or (ZOSCodePage = zCP_EUC_CN) or (ZOSCodePage = zCP_csISO2022JP))
      {add some more if you run into same issue !!} then
      Exit;
    { firebird CS_NONE just support 1Byte/Char if CP is not native like russion single byte the test just would raise exception we are aware about -> skip}
    if (ProtocolType = protFirebird) and (ConSettings.ClientCodePage.ID = 0{CS_NONE}) and (ConSettings.ClientCodePage.CP = zCP_UTF8) then
      Exit;

    try
      Query.Close;
      Query.SQL.Text := 'delete from string_values where s_id in (1001, 1002, 1003, 1004, 1005, 1006)';
      Query.ExecSQL;
      Query.SQL.Text := 'insert into string_values (s_id, s_nvarchar) values (:id, :string)';
      InsertValue(1001, Str1);
      InsertValue(1002, Str2);
      InsertValue(1003, Str3);
      InsertValue(1004, Str4);
      InsertValue(1005, Str5);
      InsertValue(1006, Str6);
      Query.SQL.Text := 'select s_id, s_nvarchar from string_values where s_id in (1001, 1002, 1003, 1004, 1005, 1006) order by s_id';
      Query.Open;
      CheckEquals(1001, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str1, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Query.Next;
      CheckEquals(1002, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str2, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Query.Next;
      CheckEquals(1003, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str3, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Query.Next;
      CheckEquals(1004, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str4, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Query.Next;
      CheckEquals(1005, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str5, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Query.Next;
      CheckEquals(1006, Query.FieldByName('s_id').AsInteger);
      CheckEquals(Str6, Query.FieldByName('s_nvarchar').{$IFDEF WITH_VIRTUAL_TFIELD_ASWIDESTRING}AsWideString{$ELSE}Value{$ENDIF});
      Assert(CP <> 0);
      Assert(Consettings <> nil);
    finally
      Query.Close;
      Query.SQL.Text := 'delete from string_values where s_id in (1001, 1002, 1003, 1004, 1005, 1006)';
      Query.ExecSQL;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompCoreBugReport.Suite);
  RegisterTest('bugreport',ZTestCompCoreBugReportMBCs.Suite);
end.
