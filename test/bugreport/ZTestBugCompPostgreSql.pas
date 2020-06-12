{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Test Cases for PostgreSQL Component Bug Reports    }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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

unit ZTestBugCompPostgreSql;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
uses
  {$IFNDEF LINUX}
    {$IFDEF WITH_VCL_PREFIX}
    Vcl.DBCtrls,
    {$ELSE}
    DBCtrls,
    {$ENDIF}
  {$ENDIF}
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset, ZAbstractRODataset,
  ZConnection, ZDbcIntfs, ZSqlTestCase, ZSqlUpdate, ZDbcProperties,
  ZCompatibility, SysUtils, ZTestConsts, ZSqlProcessor, ZSqlMetadata;

type

  {** Implements a bug report test case for PostgreSQL components. }
  TZTestCompPostgreSQLBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  private
    TestSF274_GotNotified: Boolean;
    procedure InternalTestSF224(Query: TZAbstractRODataset);
    procedure TestSF274_OnNotify(Sender: TObject; Event: string;
        ProcessID: Integer; Payload: string);
  published
    procedure Test707339;
    procedure Test707337;
    procedure Test707338;
    procedure Test709879;
    procedure Test727373;
    procedure Test739519;
    procedure Test739444;
    procedure Test759184;
    procedure Test765111;
    procedure Test766053;
    procedure Test816846;
    procedure Test824780;
    procedure Test815854;
    procedure Test831559;
    procedure Test894367;
    procedure Test933623;
    procedure Test994562;
    procedure Test1043252;
    procedure TestMantis240;
    procedure TestMantis229;
    procedure TestLobTypeCast;
    procedure TestUnknowParam;
    procedure TestTicket44;
    procedure TestUnicodeEscape;
    procedure TestTicket51;
    procedure TestSF81;
    procedure TestSF218_royo;
    procedure TestSF218_kgizmo;
    procedure TestSF224;
    procedure TestSF266;
    procedure TestSF274;
    procedure TestMarsupilami1;
    {$IFDEF WITH_TDATASETPROVIDER}
    procedure TestSF331;
    {$ENDIF WITH_TDATASETPROVIDER}
    procedure TestSF354;
    procedure TestSF394;
  end;

  TZTestCompPostgreSQLBugReportMBCs = class(TZAbstractCompSQLTestCaseMBCs)
  protected
    function GetSupportedProtocols: string; override;
  public
    procedure TestStandartConfirmingStrings(Query: TZQuery; Connection: TZConnection);
  published
    procedure TestStandartConfirmingStringsOn;
    procedure TestStandartConfirmingStringsOff;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses ZSysUtils, ZTestCase, ZPgEventAlerter, DateUtils, ZEncoding,
  ZDbcPostgreSqlMetadata, ZPlainPostgreSqlDriver, ZDatasetUtils,
  (*{$IFDEF WITH_VCL_PREFIX}Vcl.Forms{$ELSE}Forms{$ENDIF}*)ZTestConfig
  {$IFDEF WITH_TDATASETPROVIDER},Provider, DBClient{$ENDIF};

{ TZTestCompPostgreSQLBugReport }

function TZTestCompPostgreSQLBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

{**
  Test the bug report #707339.

  create table booltest(
   colnn bool not null,
   col   bool null
  );

  insert into booltest( colnn, col ) values( true, true );
  insert into booltest( colnn, col ) values( false, false );

  select * from booltest;

  When I open this query and show all rows in DBGrid all is good,
  but when i read fields manualy every field value returns "True".
}
procedure TZTestCompPostgreSQLBugReport.Test707339;
(*
var
  Query: TZQuery;
*)
begin
  if SkipForReason(srClosedBug) then Exit;
(*
  Query := CreateQuery;
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT COLNN, COL FROM booltest';
    Query.Open;
    CheckEquals('True', Query.FieldByName('COLNN').AsString);
    CheckEquals('True', Query.FieldByName('COL').AsString);
    Query.Next;
    CheckEquals('False', Query.FieldByName('COLNN').AsString);
    CheckEquals('False', Query.FieldByName('COL').AsString);
    Query.Close;
  finally
    Query.Free;
  end;
*)
end;

{**
  Test the bug report #707337.

  Query:
  select idtab, 'value' as virt_col from tab;

  Column virt_col is not exist in resultset
}
procedure TZTestCompPostgreSQLBugReport.Test707337;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select p_id, ''value'' as virt_col from people';
    Query.Open;
    CheckEquals('value', Query.FieldByName('virt_col').AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #707338.

  Query:
  select idtab::text as txt from tab;

  ::text is type cast, not param
}
procedure TZTestCompPostgreSQLBugReport.Test707338;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.ParamCheck := False;
    Query.SQL.Text := 'select p_id::text as txt from people';
    Query.Open;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #709879.

  "Out of memory" when field is interval(n)
}
procedure TZTestCompPostgreSQLBugReport.Test709879;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select now() - now() as timediff';
    Query.Open;
    Check(StartsWith(Query.FieldByName('timediff').AsString, '00:00'));
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.Test727373;
var
  Query: TZQuery;
  UpdateSql: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    UpdateSql := TZUpdateSQL.Create(nil);
    try
      Query.SQL.Text := 'delete from people where p_id >= ' + IntToStr(TEST_ROW_ID);
      Query.ExecSQL;

      Query.SQL.Text := 'select p.*, d.dep_name from people p ' +
        ' left outer join department d on p.p_id = d.dep_id ' +
        ' where p_id = ' + IntToStr(TEST_ROW_ID);
      Query.UpdateObject := UpdateSql;
      UpdateSql.InsertSQL.Text := 'insert into people (p_id, p_name, p_begin_work, ' +
        ' p_end_work) values (:p_id, :p_name, :p_begin_work, :p_end_work)';
      UpdateSql.ModifySQL.Text := 'update people set p_id = :p_id, p_name = :p_name, ' +
        ' p_begin_work = :p_begin_work, p_end_work = :p_end_work where p_id = :OLD_p_id';
      UpdateSql.DeleteSQL.Text := 'delete from people where p_id = :OLD_p_id';
      UpdateSql.Params.ParamByName('p_id').DataType := ftInteger;
      UpdateSql.Params.ParamByName('p_id').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_name').DataType := ftString;
      UpdateSql.Params.ParamByName('p_name').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_begin_work').DataType := ftTime;
      UpdateSql.Params.ParamByName('p_begin_work').ParamType := ptInput;
      UpdateSql.Params.ParamByName('p_end_work').DataType := ftTime;
      UpdateSql.Params.ParamByName('p_end_work').ParamType := ptInput;
      UpdateSql.Params.ParamByName('OLD_p_id').DataType := ftInteger;
      UpdateSql.Params.ParamByName('OLD_p_id').ParamType := ptInput;

      Query.Open;
      Query.Append;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
      Query.FieldByName('p_name').AsString := 'Vasia';
      Query.FieldByName('p_begin_work').AsDateTime := EncodeTime(9, 30, 0, 0);
      Query.FieldByName('p_end_work').AsDateTime := EncodeTime(18, 30, 0, 0);
      Query.Post;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
      CheckEquals('Vasia', Query.FieldByName('p_name').AsString);
      CheckEquals(EncodeTime(9, 30, 0, 0), Query.FieldByName('p_begin_work').AsDateTime);
      CheckEquals(EncodeTime(18, 30, 0, 0), Query.FieldByName('p_end_work').AsDateTime);
      Query.Edit;
      Query.FieldByName('p_id').AsInteger := TEST_ROW_ID;
      Query.FieldByName('p_name').AsString := 'Petia';
      Query.FieldByName('p_begin_work').AsDateTime := EncodeTime(10, 0, 0, 0);
      Query.FieldByName('p_end_work').AsDateTime := EncodeTime(19, 0, 0, 0);
      Query.Post;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(TEST_ROW_ID, Query.FieldByName('p_id').AsInteger);
      CheckEquals('Petia', Query.FieldByName('p_name').AsString);
      CheckEquals(EncodeTime(10, 0, 0, 0), Query.FieldByName('p_begin_work').AsDateTime, 0.001);
      CheckEquals(EncodeTime(19, 0, 0, 0), Query.FieldByName('p_end_work').AsDateTime, 0.001);
      Query.Delete;
      Query.ApplyUpdates;
      Query.Close;

      Query.Open;
      CheckEquals(True, Query.IsEmpty);
    finally
      UpdateSql.Free;
    end;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #739519.

  After posting updates AffectedRows property is always 0.
}
procedure TZTestCompPostgreSQLBugReport.Test739519;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'delete from test739519';
    Query.ExecSQL;

    Query.SQL.Text := 'insert into test739519 (id, fld, fld1)'
      + ' values (1, ''aaa'', ''bbb'')';
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);

    Query.SQL.Text := 'insert into test739519 (id, fld, fld1)'
      + ' values (2, ''ccc'', ''ddd'')';
    Query.ExecSQL;
    CheckEquals(1, Query.RowsAffected);

    Query.SQL.Text := 'update test739519 set fld = ''xyz''';
    Query.ExecSQL;
    CheckEquals(2, Query.RowsAffected);

    Query.SQL.Text := 'delete from test739519';
    Query.ExecSQL;
    CheckEquals(2, Query.RowsAffected);
  finally
    Query.Free;
  end;

end;

{**
  Test the bug report #739444.

  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestCompPostgreSQLBugReport.Test739444;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'select count(*) as items, sum(c_weight) as total, '+
      ' AVG(c_width) as average from cargo ';
    Query.Open;

    CheckEquals('items', Query.Fields[0].FieldName);
    CheckEquals('total', Query.Fields[1].FieldName);
    CheckEquals('average', Query.Fields[2].FieldName);
    CheckEquals(4, Query.Fields[0].AsInteger);
    CheckEquals(8434, Query.Fields[1].AsInteger);
    CheckEquals(8.5, Query.Fields[2].AsFloat, 0.01);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #759184.

  Empty fields in string concatination expression.
}
procedure TZTestCompPostgreSQLBugReport.Test759184;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select p_id || p_name as expr from people where p_id=1';
    Query.Open;

    CheckEquals('expr', Query.Fields[0].FieldName);
    CheckEquals('1Vasia Pupkin', Query.Fields[0].AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.Test765111;
var
  Batch: TZSqlProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.AutoCommit := True;
  Batch := TZSqlProcessor.Create(nil);
  try
    Batch.Connection := Connection;
    Batch.Connection.AutoCommit := False;
    Batch.Script.Text := 'DELETE FROM people where p_id = '
      + IntToStr(TEST_ROW_ID);
    Batch.Execute;
    Connection.Rollback;
  finally
    Batch.Free;
  end;
end;

{**
  Test the bug report #766053.

  Invalid variant type conversion when using TDBLookupComboBox
}
procedure TZTestCompPostgreSQLBugReport.Test766053;
var CP: Word;
{$IFNDEF LINUX}
  Query1, Query2: TZQuery;
  DSQuery1, DSQuery2: TDataSource;
  LookUp: TDBLookupComboBox;
{$ENDIF}
begin
  Connection.Connect;
  if connection.ControlsCodePage = cGET_ACP {no unicode strings or utf8 allowed}
  then CP := ZOSCodePage
  else CP := connection.DbcConnection.GetConSettings.ClientCodePage.CP;
  //eh the russion abrakadabra can no be mapped to other charsets then:
  if not ((CP = zCP_UTF8) or (CP = zCP_WIN1251) or (CP = zcp_DOS855) or (CP = zCP_KOI8R))
    {add some more if you run into same issue !!} then begin
    BlankCheck;
    Exit;
  end;

{$IFNDEF LINUX}
  Query1 := CreateQuery;
  Query2 := CreateQuery;
  DSQuery1 := TDataSource.Create(nil);
  DSQuery2 := TDataSource.Create(nil);
  LookUp := TDBLookupComboBox.Create(nil);;
  try
    Query1.SQL.Text := 'select * from test766053a';
    Query2.SQL.Text := 'select * from test766053b';
    DSQuery1.DataSet := Query1;
    DSQuery2.DataSet := Query2;
    LookUp.DataSource := DSQuery1;
    LookUp.ListSource := DSQuery2;
    LookUp.DataField := 'id';
    LookUp.KeyField := 'id';
    LookUp.ListField := 'fld';
    Query1.Open;
    Query2.Open;
  finally
    LookUp.Free;
    Query1.Free;
    Query2.Free;
    DSQuery1.Free;
    DSQuery2.Free;
  end;
{$ENDIF}
end;

{**
  Test the bug report #816846.

  Bad update behavior when no primary key
}
procedure TZTestCompPostgreSQLBugReport.Test816846;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select fld1, fld2 from test816846 order by fld1';
    Query.Open;

    CheckEquals(2, Query.RecordCount);
    Query.Last;
    Query.Edit;
    Query.Fields[1].AsString := 'd';
    Query.Post;
    CheckEquals('d', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[1].AsString := 'ddd';
    Query.Post;
    Query.Refresh;
    Query.Last;
    CheckEquals('ddd', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[1].AsString := 'd';
    Query.Post;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #824780.

  TZMetadata does not show schemas.
}
procedure TZTestCompPostgreSQLBugReport.Test824780;
var
  Metadata: TZSQLMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  if Connection.Protocol <> 'postgresql-7' then
    Exit;

  Metadata := TZSQLMetadata.Create(nil);
  try
    Metadata.Connection := Connection;
    Metadata.MetadataType := mdSchemas;
    Metadata.Open;

    Check(Metadata.RecordCount > 0);
    Metadata.Locate('TABLE_SCHEM', 'xyz', []);
    Check(Metadata.Found);

    Metadata.Close;
  finally
    Metadata.Free;
  end;
end;

{**
  Test the bug report #Test815854.

  Problem with support for schemas.
}
procedure TZTestCompPostgreSQLBugReport.Test815854;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  if Connection.Protocol <> 'postgresql-7' then
    Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'delete from xyz.test824780';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select fld1, fld2 from xyz.test824780';
    Query.Open;

    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));

    Query.Insert;
    Query.Fields[0].AsInteger := 123456;
    Query.Fields[1].AsString := 'abcdef';
    Query.Post;

    Query.Refresh;
    Check(not Query.Eof);
    CheckEquals(1, Query.RecNo);
    CheckEquals(123456, Query.Fields[0].AsInteger);
    CheckEquals('abcdef', Query.Fields[1].AsString);

    Query.SQL.Text := 'delete from xyz.test824780';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #Test831559.

  Use keywords in column name.
}
procedure TZTestCompPostgreSQLBugReport.Test831559;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'delete from "insert"';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from "insert"';
    Query.Open;

    Query.Insert;
    Query.Fields[0].AsString := 'abcdef';
    Query.Fields[1].AsInteger := 123456;
    Query.Post;

    Query.Refresh;
    Check(not Query.Eof);
    CheckEquals(1, Query.RecNo);
    CheckEquals('abcdef', Query.Fields[0].AsString);
    CheckEquals(123456, Query.Fields[1].AsInteger);

    Query.SQL.Text := 'delete from "insert"';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #894367.

  Incorrect parsing of complex queries.
}
procedure TZTestCompPostgreSQLBugReport.Test894367;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'SELECT d65.f3 as a,t65.f2 as b,t65.f3 as c'
      + ' FROM test894367a as t65, test894367b as d65'
      + ' WHERE d65.f1=t65.f1';
    Query.Open;

    if (Connection.ControlsCodePage = cCP_UTF16 ) then
      CheckEquals(ord(ftWideString), Ord(Query.Fields[0].DataType))
    else
      CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[2].DataType));

    Query.Close;

    Query.SQL.Text := 'SELECT test894367b.f3,test894367a.f2,test894367a.f3'
      + ' FROM test894367a, test894367b'
      + ' WHERE test894367a.f1=test894367b.f1';
    Query.Open;

    if (Connection.ControlsCodePage = cCP_UTF16 ) then
      CheckEquals(ord(ftWideString), Ord(Query.Fields[0].DataType))
    else
      CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType));
    CheckEquals(Ord(ftBoolean), Ord(Query.Fields[2].DataType));

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #933623.
  Command is aborten until the next of transaction block.
}
procedure TZTestCompPostgreSQLBugReport.Test933623;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.AutoCommit := True;
  Connection.TransactIsolationLevel := tiReadCommitted;
  Query := CreateQuery;
  try
    try
      Query.SQL.Text := 'select * from people where xp_id=1';
      Query.Open;
      Fail('Incorrect syntax error processing');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

    Query.SQL.Text := 'select * from people where p_id=1';
    Query.Open;

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #894367.
  Incorrect parsing of complex queries.
}
procedure TZTestCompPostgreSQLBugReport.Test994562;
var
  Query: TZReadOnlyQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Clear;
    Query.SQL.Append('SELECT *');
    Query.SQL.Append('-- SQL Comment');
    Query.SQL.Append('FROM people');
    Query.SQL.Append('WHERE p_id=:p_id');
    Query.SQL.Append('-- SQL Comment');

    CheckEquals(1, Query.Params.Count);
    Query.Params[0].AsInteger := 1;

    Query.Open;
    CheckEquals(1, Query.RecordCount);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #1043252.
  "No Argument for format %s" exception.
}

type
  TZAbstractRODatasetHack = class(TZAbstractRODataset)
  end;

procedure TZTestCompPostgreSQLBugReport.InternalTestSF224(
  Query: TZAbstractRODataset);
var S: String;
begin
  with TZAbstractRODatasetHack(Query) do
  try
    SQL.Text := 'select * from guid_test';
    Open;
    Check(Fields[1].DataType = ftGUID);
    S := Fields[1].AsString;
    CheckEquals('{BAD51CFF-F21F-40E8-A9EA-838977A681BE}', s, 'UUID different');
    Close;
    SQL.Text := 'select * from guid_test where guid = :x';
    ParamByName('x').AsString := '{BAD51CFF-F21F-40E8-A9EA-838977A681BE}';
    Open;
    Check(Fields[1].DataType = ftGUID);
    S := Fields[1].AsString;
    CheckEquals('{BAD51CFF-F21F-40E8-A9EA-838977A681BE}', s, 'UUID not found');
  finally
    Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.Test1043252;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select p_name as " xxx xxx " from people where p_id=1';
    Query.Open;

    CheckEquals(1, Query.RecordCount);
    CheckEquals(' xxx xxx ', Query.FieldDefs[0].Name);
    CheckEquals('Vasia Pupkin', Query.Fields[0].AsString);
    CheckEquals('Vasia Pupkin', Query.FieldByName(' xxx xxx ').AsString);

    Query.Close;

    Query.SQL.Text := 'select p_name from people where p_id=1';
    Query.Open;
    Query.FieldDefs[0].DisplayName := ' xxx xxx '; //Changes nothing since which D-Version?

    CheckEquals(1, Query.RecordCount);
    {$IFDEF FPC}
    CheckEquals(' xxx xxx ', Query.FieldDefs[0].Name);
    {$ELSE}
    {$IFDEF VER150BELOW}
    CheckEquals(' xxx xxx ', Query.FieldDefs[0].Name); //EgonHugeist: Changed from ' xxx xxx ' -> select!
    {$ELSE}
    CheckEquals('p_name', Query.FieldDefs[0].Name); //EgonHugeist: Changed from ' xxx xxx ' -> select!
    {$ENDIF}
    {$ENDIF}
    //CheckEquals(' xxx xxx ', Query.FieldDefs[0].DisplayName); //EgonHugeist: The TFieldDef inherites from TCollectionItem! the DisplayName is'nt changeable there. This is only possible if we create a class of TFieldDef and override the SetDisplayName method
    CheckEquals('Vasia Pupkin', Query.Fields[0].AsString);
    CheckEquals('Vasia Pupkin', Query.FieldByName('p_name').AsString);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{** Matin#0000240
Hi!

I tried to run a ZQuery to get the table

"ntax_bejovo_konyvelesi_tipusok"

primary keys.

select r.relname as "Table", c.conname,
contype as "Constraint Type"
from pg_class r, pg_constraint c
where r.oid = c.conrelid;

In PGADMIN and EMS PG Manager I got good result, the:

"ntax_bejovo_konyvelesi_tipusok_pkey"


But Zeos Query is set the Field Size to 32, and I don't found the needed columns of the primary key in the next Query...


When I cast the field:

cast(c.conname as varchar(100)),

the field size is correctly 100.

Is this bug, or this based on "unknown field type" of PG that automatically set to 32... :-(
}
procedure TZTestCompPostgreSQLBugReport.TestMantis240;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select r.relname as "Table", c.conname, ' +
            ' contype as "Constraint Type" ' +
            ' from pg_class r, pg_constraint c ' +
            ' where r.oid = c.conrelid and r.relname = ''ntax_bejovo_konyvelesi_tipusok''; ';
    Query.Open;
    CheckEquals('ntax_bejovo_konyvelesi_tipusok_pkey', Query.FieldByName('conname').AsString);
  finally
    Query.Free;
  end;
end;

{**
0000229: postgresql varchar is badly interpreted
  In postgresql, varchar with no precision is equal to text (blob) type.
  In zeos, varchar is treated as stString with default precision 255.
  It means when we try to read data that are longer than 255 then they are
  automatically truncated.
}
procedure TZTestCompPostgreSQLBugReport.TestMantis229;
var
  Query: TZQuery;

  procedure TestMantis229_AsMemo;
  begin
    Query := CreateQuery;
    try
      Query.SQL.Text := 'select * from Mantis229';
      Query.Open;
      CheckMemoFieldType(Query.Fields[0].DataType, Connection.ControlsCodePage);
      CheckEquals('Mantis229', Query.Fields[0].AsString);
    finally
      Query.Free;
    end;
  end;

  procedure TestMantis229_AsString;
  begin
    Connection.Properties.Values[DSProps_UndefVarcharAsStringLength] := '255';
    Connection.Connected := False;
    Query := CreateQuery;
    try
      Query.SQL.Text := 'select * from Mantis229';
      Query.Open;
      CheckStringFieldType(Query.Fields[0].DataType, Connection.ControlsCodePage);
      CheckEquals('Mantis229', Query.Fields[0].AsString);
    finally
      Query.Free;
      Connection.Properties.Values[DSProps_UndefVarcharAsStringLength] := '';
    end;
  end;

begin
  if SkipForReason(srClosedBug) then Exit;

  TestMantis229_AsMemo;
  TestMantis229_AsString;
end;

{After introducing the Postgre real prepared statement we had a behaior change
 for indeterminable parameters types. handle_indeterminate_datatype should fix it}
procedure TZTestCompPostgreSQLBugReport.TestUnknowParam;
var Query: TZQuery;
  I: Integer;
  NowDate: TDateTime;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Connection.Connect;
  try
    NowDate := Now;
    Query.SQL.Text := 'select :p1 as Param1, :p2 as param2, :p3 as param3, :p4 as param4';
    Query.Params[1].AsTime := NowDate;
    Query.Params[2].AsDate := NowDate;
    Query.Params[3].AsDateTime := NowDate;
    for i := 0 to 5 do begin
      Query.Open;
      //Check(Query.Fields[1].AsDateTime = Frac(NowDate), 'time value passed by unknown param');
      //Check(Query.Fields[2].AsDateTime = Int(NowDate), 'date value passed by unknown param');
      //Check(Query.Fields[2].AsDateTime = NowDate, 'timestamp value passed by unknown param');
      Query.Close;
    end;
  finally
    Query.Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.TestTicket44;
var Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Connection.Connect;
  try
    Query.SQL.Text := 'select table_catalog from information_schema.columns';
    Query.Open;
    CheckEquals(UpperCase(DataBase), UpperCase(Query.Fields[0].AsString));
    Query.Close;
  finally
    Query.Free;
  end;
end;

(*
http://zeoslib.sourceforge.net/viewtopic.php?f=39&t=3968
I have delphi xe3 prof. and zeos 7.0.3-stable installed, DB is PostgreSQL 9, and I have a problem when I put by Param string value (contains \U escape character) to TZQuery.SQL.Text.
code:
q := TZQuery.Create(nil);
q.Connection := Conn;
q.SQL.Text := 'select :content';
q.ParamByName('content').AsString := sText;
q.ExecSQL; //exception
q.Free;
when for example sText := 'C:/User'; executing this query q raises exception class EZSQLException with message 'SQL Error: ERROR: invalid Unicode escape at character 31 HINT: Unicode escapes must be \uXXXX or \UXXXXXXXX.'.
*)
procedure TZTestCompPostgreSQLBugReport.TestUnicodeEscape;
var Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Connection.Connect;
  try
    Query.SQL.Text := 'select :content';
    Query.ParamByName('content').AsString := 'C:\User';
    Query.ExecSQL;
    Query.SQL.Text := 'select :content';
    Query.ParamByName('content').AsString := 'C:/User';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{
Column "Name"
-> done! Expected result: Name column shows "(Memo/WideMemo)"
-> resolved: Actual result: Name column is empty
}
procedure TZTestCompPostgreSQLBugReport.TestTicket51;
var Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Connection.Connect;
  try
    Query.SQL.Text := 'SELECT Ticket51_A.*, '+
      '(CASE WHEN (Ticket51_B."Name" IS NOT NULL) THEN Ticket51_B."Name" '+
      'ELSE ''Empty'' END) As "Name" FROM Ticket51_A '+
      'LEFT JOIN Ticket51_B ON Ticket51_B."t1_id" = Ticket51_A."ID" '+
      'WHERE Ticket51_A."order_id" = 2';
    Query.Open;
    CheckMemoFieldType(Query.Fields[2].DataType, Connection.ControlsCodePage);
    CheckEquals('MyName', Query.Fields[2].AsString);
  finally
    Query.Free;
  end;
end;

//since Pointer referencing by RowAccessor we've a pointer and GetBlob
//raises an exception if the pointer is a reference to PPAnsiChar or
//ZPPWideChar. if we execute a cast of a lob field the database meta-informtions
//assume a IZLob-Pointer. So let's prevent this case and check for
//stByte, stString, stUnicoeString first. If this type is returned from the
//ResultSet-Metadata we do NOT overwrite the column-type
//f.e. select cast( name as varchar(100)), cast(setting as varchar(100)) from pg_settings
procedure TZTestCompPostgreSQLBugReport.TestLobTypeCast;
var Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Query := CreateQuery;
  Query.SQL.Text := 'select cast( name as varchar(100)), cast(setting as varchar(100)) from pg_settings';
  Query.Open;
  CheckStringFieldType(Query.Fields[0].DataType, Connection.ControlsCodePage);
  CheckStringFieldType(Query.Fields[1].DataType, Connection.ControlsCodePage);
  Query.Close;
  Query.Free;
end;
{ TZTestCompPostgreSQLBugReportMBCs }
function TZTestCompPostgreSQLBugReportMBCs.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

procedure TZTestCompPostgreSQLBugReportMBCs.TestStandartConfirmingStrings(Query: TZQuery; Connection: TZConnection);
const
  QuoteString1 = '\'', 1 --''';
  QuoteString2: {$IFNDEF UNICIDE}WideString{$ELSE}UnicodeString{$ENDIF} = #$0422#$0435#$0441#$0442#$0401#$0419'\000'; // Test in Russian + a couple of random Cyrillic letters
var  CP: Word;
  ConSettings: PZConSettings;
begin
  Query.ParamChar := ':';
  Query.ParamCheck := True;
  Query.SQL.Text := 'select cast(:test as TEXT)';
  {$IFDEF UNICODE}
  Query.ParamByName('test').AsString := QuoteString1;
  {$ELSE}
  Query.ParamByName('test').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := QuoteString1;
  {$ENDIF}

  Query.Open;
  CheckEquals(QuoteString1, Query.Fields[0].AsString);
  Query.Close;

  ConSettings := connection.DbcConnection.GetConSettings;
  if (Connection.ControlsCodePage = cGET_ACP) and {no unicode strings or utf8 allowed}
    not ((ZOSCodePage = zCP_UTF8) or (ZOSCodePage = zCP_WIN1251) or (ZOSCodePage = zcp_DOS855) or (ZOSCodePage = zCP_KOI8R)) then
    Exit;
  CP := ConSettings.ClientCodePage.CP;
  //eh the russion abrakadabra can no be mapped to other charsets then:
  if not ((CP = zCP_UTF8) or (CP = zCP_WIN1251) or (CP = zcp_DOS855) or (CP = zCP_KOI8R))
    {add some more if you run into same issue !!} then
    Exit;
  {$IFDEF UNICODE}
  Query.ParamByName('test').AsString := QuoteString2;
  {$ELSE}
  Query.ParamByName('test').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := QuoteString2;
  {$ENDIF}
  Query.Open;
  {$IFDEF UNICODE}
  CheckEquals(QuoteString2, Query.Fields[0].AsString);
  {$ELSE}
  If Connection.ControlsCodePage = cCP_UTF16 then
    CheckEquals(QuoteString2, Query.Fields[0].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF})
  else if ConSettings.AutoEncode
    then CheckEquals(ZUnicodeToRaw(QuoteString2, ConSettings.CTRL_CP), Query.Fields[0].AsString)
    else CheckEquals(ZUnicodeToRaw(QuoteString2, CP), Query.Fields[0].AsString);
  {$ENDIF}
  Query.Close;
end;

procedure TZTestCompPostgreSQLBugReportMBCs.TestStandartConfirmingStringsOn;
var
  TempConnection: TZConnection;          // Attention : local Connection
  Query: TZQuery;
begin
//??  if SkipForReason(srClosedBug) then Exit;

  TempConnection := CreateDatasetConnection;
  TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'ON';
  Query := TZQuery.Create(nil);
  Query.Connection := TempConnection;
  try
    TestStandartConfirmingStrings(Query, TempConnection);
  finally
    Query.Free;
    TempConnection.Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReportMBCs.TestStandartConfirmingStringsOff;
var
  TempConnection: TZConnection;
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  TempConnection := CreateDatasetConnection;
  TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'OFF';
  Query := TZQuery.Create(nil);
  Query.Connection := TempConnection;
  try
    TestStandartConfirmingStrings(Query, TempConnection);
  finally
    Query.Free;
    TempConnection.Free;
  end;
end;

//see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
procedure TZTestCompPostgreSQLBugReport.TestSF218_kgizmo;
var
  TempConnection: TZConnection;
  Query: TZQuery;
const
  TestString = 'Test string'+#10+'Test string';
begin
  if SkipForReason(srClosedBug) then Exit;

  TempConnection := CreateDatasetConnection;
  TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'OFF';
  Query := TZQuery.Create(nil);
  TempConnection.Connect;
  Query.Connection := TempConnection;
  try
    Query.SQL.Text := 'select s_char||''\n''||s_varchar from string_values where 1=1 and (s_varbit iLike :param)';
    Query.ParamByName('param').AsString := '%'; //kgizmo reports: Everything is OK with Params but I get "\n" string in the result text.
    Query.Open;
    Query.Next;
    CheckEquals(TestString, Query.Fields[0].AsString, 'wrong string returned tested with pgadmin and standard_conforming_strings=OFF');
    Query.Close;

    Query.SQL.Text := 'select s_char||E''\n''||s_varchar from string_values where 1=1 and (s_varbit iLike :param)';
    Query.ParamByName('param').AsString := '%'; //kgizmo reports: Parameter not found.
    Query.Open;
    Query.Next;
    CheckEquals(TestString, Query.Fields[0].AsString, 'wrong string returned tested with pgadmin and standard_conforming_strings=OFF');
    Query.Close;

    TempConnection.Disconnect;
    TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'ON';
    TempConnection.Connect;

    Query.SQL.Text := 'select s_char||E''\n''||s_varchar from string_values where 1=1 and (s_varbit iLike :param)';
    Query.ParamByName('param').AsString := '%'; //kgizmo reports: Parameter not found.
    Query.Open;
    Query.Next;
    CheckEquals(TestString, Query.Fields[0].AsString, 'wrong string returned tested with pgadmin and standard_conforming_strings=OFF');
    Query.Close;
  finally
    Query.Free;
    TempConnection.Free;
  end;
end;

//see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=49966
procedure TZTestCompPostgreSQLBugReport.TestSF218_royo;
var
  TempConnection: TZConnection;
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  TempConnection := CreateDatasetConnection;
  TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'OFF';
  Query := TZQuery.Create(nil);
  TempConnection.Connect;
  Query.Connection := TempConnection;
  try
    Query.SQL.Text := 'select * from sys_user where user_id = :user_id';
    Query.ParamByName('user_id').AsInteger := 1; //royo reports: param not found???
    Query.SQL.Text := '';
    TempConnection.Disconnect;
    TempConnection.Properties.Values[ConnProps_StdConformingStrings] := 'ON';
    TempConnection.Connect;
    Query.Connection := TempConnection;
    Query.SQL.Text := 'select * from sys_user where user_id = :user_id';
    Query.ParamByName('user_id').AsInteger := 1; //royo reports: param not found???
    Query.SQL.Text := '';
  finally
    Query.Free;
    TempConnection.Free;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.TestSF224;
var
  Query: TZQuery;
  ROQuery: TZReadOnlyQuery;
begin
  Connection.Connect;
  if Connection.DbcConnection.GetHostVersion < 9 then
    Exit;
  Query := TZQuery.Create(nil);
  Query.ReadOnly := True;
  Query.Connection := Connection;
  InternalTestSF224(Query);
  Query := TZQuery.Create(nil);
  Query.ReadOnly := False;
  Query.Connection := Connection;
  InternalTestSF224(Query);
  ROQuery := TZReadOnlyQuery.Create(nil);
  ROQuery.Connection := Connection;
  InternalTestSF224(ROQuery);
  ROQuery := TZReadOnlyQuery.Create(nil);
  ROQuery.IsUniDirectional := True;
  ROQuery.Connection := Connection;
  InternalTestSF224(ROQuery);
end;

procedure TZTestCompPostgreSQLBugReport.TestSF81;
var
  TempConnection: TZConnection;
  Query: TZQuery;
begin
  TempConnection := CreateDatasetConnection;
  try
    Query := TZQuery.Create(nil);
    try
      Query.Connection := TempConnection;
      Query.SQL.Text := 'select * from public."RANMeter"';
      Query.Open;
      Query.Edit;
      try
        Query.FieldByName('MeterID').AsInteger := 32134;
        Query.Post;
      except
        Query.Cancel;
        raise;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
    FreeAndNil(TempConnection);
  end;
end;

procedure TZTestCompPostgreSQLBugReport.TestSF266;
var
  Query: TZQuery;
  B, X: Boolean;
begin
  Connection.Connect;
  X := (Connection.DbcConnection.GetMetadata.GetDatabaseInfo as IZPostgreSQLDatabaseInfo).HasMinimumServerVersion(8,4) and //just V3.up protocol supports the binary wire
    Assigned((Connection.DbcConnection.GetIZPlainDriver.GetInstance as TZPostgreSQLPlainDriver).PQexecParams);//pgbouncer does not support this function
  for B := False to X do
    try
      Query := TZQuery.Create(nil);
      Query.Properties.Values[DSProps_BinaryWireResultMode] := IntToStr(Ord(B));
      Query.Connection := Connection;
      Query.SQL.Append('CREATE OR REPLACE FUNCTION pc_chartoint(chartoconvert character varying)');
      Query.SQL.Append('  RETURNS integer AS');
      Query.SQL.Append('$BODY$');
      Query.SQL.Append('SELECT CASE WHEN trim($1) SIMILAR TO ''[0-9]+''');
      Query.SQL.Append('        THEN CAST(trim($1) AS integer)');
      Query.SQL.Append('    ELSE NULL END;');
      Query.SQL.Append('');
      Query.SQL.Append('$BODY$');
      Query.SQL.Append('  LANGUAGE ''sql'' IMMUTABLE STRICT;');
      Query.SQL.Append('');
      Query.SQL.Append('INSERT INTO blob_values(b_id, b_text) values (261,'''');');
      if B then
        try
          Query.ExecSQL;
          Check(False, 'Whoops Postgres has a changed behavior! Tag it as known!');
        except end
      else begin
        Query.ExecSQL;
        Connection.ExecuteDirect('drop function pc_chartoint(chartoconvert character varying)');
      end;
    finally
      Connection.ExecuteDirect('delete from blob_values where b_id = 261');
      FreeAndNil(Query);
      Connection.Disconnect;
    end;
end;

procedure TZTestCompPostgreSQLBugReport.TestSF274;
var
  Listener: TZPgEventAlerter;
  EndTime: TDateTime;
begin
  TestSF274_GotNotified := false;
  try
    Listener := TZPgEventAlerter.Create(nil);
    Listener.Events.Add('zeostest');
    Listener.Connection := Connection;
    Listener.OnNotify := TestSF274_OnNotify;
    Connection.Connect;
    Listener.Active := True;
    EndTime := IncSecond(Now, 2);
    Connection.ExecuteDirect('NOTIFY zeostest');
    while (not TestSF274_GotNotified) and (EndTime > Now) do begin
      //Application.ProcessMessages;
      Sleep(0);
    end;
    Listener.Active := false;
    Check(TestSF274_GotNotified, 'Didn''t get PostgreSQL notification.');
  finally
    FreeAndNil(Listener);
    Connection.Disconnect;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.TestSF274_OnNotify(Sender: TObject; Event: string;
        ProcessID: Integer; Payload: string);
begin
  TestSF274_GotNotified := true;
end;

procedure TZTestCompPostgreSQLBugReport.TestMarsupilami1;
var
  Query: TZQuery;
  String1, String2: String;
begin
  Connection.Connect;
  try
    Query := TZQuery.Create(nil);
    Query.Connection := Connection;
    Connection.ExecuteDirect('insert into bcd_values (id) values (0815)');
    Query.SQL.Text := 'select id, curr15_2 from bcd_values where id = 0815';
    Query.Open;
    Query.Edit;
    Query.FieldByName('curr15_2').AsFloat := 274.065;
    Query.Post;
    String1 := Query.FieldByName('curr15_2').AsString;
    Query.Close;
    Query.Open;
    String2 := Query.FieldByName('curr15_2').AsString;
    Query.Close;
    CheckEquals(String1, String2, 'Users expect these strings to be equal.');
  finally
    FreeAndNil(Query);
    Connection.ExecuteDirect('delete from bcd_values where id = 0815');
    Connection.Disconnect;
  end;
end;

{$IFDEF WITH_TDATASETPROVIDER}
procedure TZTestCompPostgreSQLBugReport.TestSF331;
var
  Query: TZQuery;
  Provider: TDataSetProvider;
  ClientDataSet: TClientDataSet;
begin
  Query := CreateQuery;
  Connection.Connect;
  Provider := TDataSetProvider.Create(Query);
  Provider.DataSet := Query;
  Provider.Name := 'TestSF331Provider';
  ClientDataSet := TClientDataSet.Create(Query);
  ClientDataSet.ProviderName := Provider.Name;
  try
    Query.SQL.Text := 'select * from TableSFTicket331 order by id';
    //Query.Open;
    ClientDataSet.Open;
    CheckFalse(ClientDataSet.EOF, 'ClientDataset should have a row');
    CheckEquals(1, ClientDataSet.Fields[0].AsInteger, 'First row, value of field id');
    CheckEquals(0.5214, ClientDataSet.Fields[1].AsCurrency, 'First row, value of field val1');
    CheckEquals(52.14, ClientDataSet.Fields[2].AsCurrency, 'First row, value of field val2');
    ClientDataSet.Next;
    CheckFalse(ClientDataSet.EOF, 'ClientDataset should have a row');
    CheckEquals(2, ClientDataSet.Fields[0].AsInteger, 'Second row, value of field id');
    CheckEquals(0.8358, ClientDataSet.Fields[1].AsCurrency, 'Second row, value of field val1');
    CheckEquals(83.58, ClientDataSet.Fields[2].AsCurrency, 'Second row, value of field val2');
    ClientDataSet.Next;
    CheckFalse(ClientDataSet.EOF, 'ClientDataset should have a row');
    CheckEquals(3, ClientDataSet.Fields[0].AsInteger, 'Third row, value of field id');
    CheckEquals(900, ClientDataSet.Fields[1].AsCurrency, 'Third row, value of field val1');
    CheckEquals(0.08, ClientDataSet.Fields[2].AsCurrency, 'Third row, value of field val2');
    ClientDataSet.Next;
    CheckFalse(ClientDataSet.EOF, 'ClientDataset should have a row');
    CheckEquals(4, ClientDataSet.Fields[0].AsInteger, 'Fourth row, value of field id');
    CheckEquals(9000, ClientDataSet.Fields[1].AsCurrency, 'Fourth row, value of field val1');
    CheckEquals(0.23, ClientDataSet.Fields[2].AsCurrency, 'Fourth row, value of field val2');
    Query.Close;
  finally
    FreeAndNil(ClientDataSet);
    FreeAndNil(Provider);
    FreeAndNil(Query);
  end;
end;
{$ENDIF WITH_TDATASETPROVIDER}

procedure TZTestCompPostgreSQLBugReport.TestSF354;
var
  Query: TZQuery;
  Field: TField;

  procedure CheckFieldExists(const FieldName, ErrorMessage: String);
  begin
    Field := Query.FindField(FieldName);
    Check(Assigned(Field), ErrorMessage);
  end;

begin
  Connection.Connect;
  try
    Query := TZQuery.Create(nil);
    Query.Connection := Connection;
    Query.SQL.Text := 'select * from sf354';
    Query.Open;
    try
      CheckFieldExists('timestamp_none', 'Could not find field timstamp_none of type timestamp');
      CheckFieldExists('timestamp0', 'Could not find field timstamp_none of type timestamp(0)');
      CheckFieldExists('timestamp1', 'Could not find field timstamp_none of type timestamp(1)');
      CheckFieldExists('timestamp2', 'Could not find field timstamp_none of type timestamp(2)');
      CheckFieldExists('timestamp3', 'Could not find field timstamp_none of type timestamp(3)');
      CheckFieldExists('timestamp4', 'Could not find field timstamp_none of type timestamp(4)');
      CheckFieldExists('timestamp5', 'Could not find field timstamp_none of type timestamp(5)');
      CheckFieldExists('timestamp6', 'Could not find field timstamp_none of type timestamp(6)');
    finally
      Query.Close;
    end;
  finally
    FreeAndNil(Query);
    Connection.Disconnect;
  end;
end;

procedure TZTestCompPostgreSQLBugReport.TestSF394;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select 1::BIGINT';
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'The record-count');
    Query.ParamCheck := False;
    Query.SQL.Text := 'select 1::BIGINT';
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'The record-count');
    Query.Close;
    Query.ParamChar := '$';
    Query.ParamCheck := True;
    Query.SQL.Text := 'select 1::BIGINT';
    Query.Open;
    CheckEquals(1, Query.RecordCount, 'The record-count');
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;

initialization
  RegisterTest('bugreport',TZTestCompPostgreSQLBugReport.Suite);
  RegisterTest('bugreport',TZTestCompPostgreSQLBugReportMBCs.Suite);
{$ENDIF ZEOS_DISABLE_POSTGRESQL}
end.
