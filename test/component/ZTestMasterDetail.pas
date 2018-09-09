{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Case for Master-Detail Links             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestMasterDetail;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, SysUtils,
  ZGenericSqlToken, ZDataset, ZSqlTestCase;

type

  {** Implements a test case for Master-Detail relations }
  TZTestMasterDetailCase = class(TZAbstractCompSQLTestCase)
  private
    MasterDataSource: TDataSource;
    MasterQuery: TZQuery;
    DetailQuery: TZQuery;
    DetailQuery2: TZQuery;
    DetailQuery3: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDataSource;
    procedure TestMasterFields;
    procedure TestClientDataset;
  end;

  TZTestMasterDetailCaseMBCs = class(TZAbstractCompSQLTestCaseMBCs)
  private
    MasterDataSource: TDataSource;
    MasterQuery: TZQuery;
    DetailQuery: TZQuery;
    DetailQuery2: TZQuery;
    DetailQuery3: TZQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestClientDatasetWithForeignKey_ApplyUpdates;
  published
    procedure TestClientDatasetWithForeignKey_doUpdateMasterFirst;
  end;

implementation

uses Classes, ZDbcIntfs, ZSqlMonitor, ZdbcLogging,
  ZAbstractRODataset, ZCompatibility, ZSysUtils;

const TestRowID = 1000;

{ TZTestMasterDetailCase }

{**
  Prepares initial data before each test.
}
procedure TZTestMasterDetailCase.SetUp;
begin
  inherited SetUp;

  MasterQuery := CreateQuery;
  MasterDataSource := TDataSource.Create(nil);
  MasterDataSource.DataSet := MasterQuery;

  DetailQuery := CreateQuery;
  DetailQuery2 := CreateQuery;
  DetailQuery3 := CreateQuery;
end;

{**
  Removes data after each test.
}
procedure TZTestMasterDetailCase.TearDown;
begin
  DetailQuery.Close;
  DetailQuery.Free;

  DetailQuery2.Close;
  DetailQuery2.Free;

  DetailQuery3.Close;
  DetailQuery3.Free;

  MasterQuery.Close;
  MasterQuery.Free;

  MasterDataSource.Free;

  inherited TearDown;
end;

{**
  Runs a test for SQL parameters.
}
procedure TZTestMasterDetailCase.TestDataSource;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people WHERE p_dep_id=:dep_id';
  DetailQuery.DataSource := MasterDataSource;
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

{**
  Runs a test for master-detail links.
}
procedure TZTestMasterDetailCase.TestMasterFields;
begin
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'dep_id';
  DetailQuery.LinkedFields := 'p_dep_id';
  DetailQuery.Open;

  MasterQuery.First;
  CheckEquals(1, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(1, DetailQuery.FieldByName('p_dep_id').AsInteger);

  MasterQuery.Next;
  CheckEquals(2, MasterQuery.FieldByName('dep_id').AsInteger);
  CheckEquals(2, DetailQuery.RecordCount);
  CheckEquals(2, DetailQuery.FieldByName('p_dep_id').AsInteger);
end;

{**
  Runs a test for in clientdatset rules
  All detail-queries should be updated in a single transaction.
}
procedure TZTestMasterDetailCase.TestClientDataset;
var
  SQLMonitor: TZSQLMonitor;
  CommitCount, I: Integer;
begin
  SQLMonitor := TZSQLMonitor.Create(nil);
  SQLMonitor.Active := True;
  MasterQuery.SQL.Text := 'SELECT * FROM default_values ORDER BY d_id';
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM date_values';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'd_id';
  DetailQuery.LinkedFields := 'd_id';
  DetailQuery.Open;

  DetailQuery2.SQL.Text := 'SELECT * FROM date_values';
  DetailQuery2.MasterSource := MasterDataSource;
  DetailQuery2.MasterFields := 'd_id';
  DetailQuery2.LinkedFields := 'd_id';
  DetailQuery2.Open;

  DetailQuery3.SQL.Text := 'SELECT * FROM date_values';
  DetailQuery3.MasterSource := MasterDataSource;
  DetailQuery3.MasterFields := 'd_id';
  DetailQuery3.LinkedFields := 'd_id';
  DetailQuery3.Open;

  CommitCount := 0;
  try
    MasterQuery.Append;
    MasterQuery.FieldByName('d_id').AsInteger := TestRowID;
    CheckEquals(True, (MasterQuery.State = dsInsert), 'MasterQuery Insert-State');

    DetailQuery.Append;
    DetailQuery.FieldByName('d_id').AsInteger := TestRowID;
    DetailQuery.FieldByName('d_date').AsDateTime := Date;
    DetailQuery.FieldByName('d_time').AsDateTime := Time;
    CheckEquals(True, (DetailQuery.State = dsInsert), 'MasterQuery Insert-State');

    DetailQuery2.Append;
    DetailQuery2.FieldByName('d_id').AsInteger := TestRowID+1;
    DetailQuery2.FieldByName('d_date').AsDateTime := Date;
    DetailQuery2.FieldByName('d_time').AsDateTime := Time;
    CheckEquals(True, (DetailQuery2.State = dsInsert), 'MasterQuery Insert-State');

    DetailQuery3.Append;
    DetailQuery3.FieldByName('d_id').AsInteger := TestRowID+2;
    DetailQuery3.FieldByName('d_date').AsDateTime := Date;
    DetailQuery3.FieldByName('d_time').AsDateTime := Time;
    CheckEquals(True, (DetailQuery3.State = dsInsert), 'MasterQuery Insert-State');

    MasterQuery.Post;

    CheckEquals(True, (MasterQuery.State = dsBrowse), 'MasterQuery Browse-State');
    CheckEquals(True, (DetailQuery.State = dsBrowse), 'DetailQuery Browse-State');
    CheckEquals(True, (DetailQuery2.State = dsBrowse), 'DetailQuery Browse-State');
    CheckEquals(True, (DetailQuery3.State = dsBrowse), 'DetailQuery Browse-State');

    for i := 0 to SQLMonitor.TraceCount -1 do
      if SQLMonitor.TraceList[i].Category = lcTransaction then
        if Pos('COMMIT', UpperCase(String(SQLMonitor.TraceList[i].Message))) > 0 then
          Inc(CommitCount);

    CheckEquals(CommitCount{fix it 1}, CommitCount, 'CommitCount');
  finally
    MasterQuery.SQL.Text := 'delete from default_values where d_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    MasterQuery.SQL.Text := 'delete from date_values where d_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    MasterQuery.SQL.Text := 'delete from date_values where d_id = '+IntToStr(TestRowID+1);
    MasterQuery.ExecSQL;
    MasterQuery.SQL.Text := 'delete from date_values where d_id = '+IntToStr(TestRowID+2);
    MasterQuery.ExecSQL;
    SQLMonitor.Free;
  end;
end;

{ TZTestMasterDetailCaseMBCs }
{**
  Prepares initial data before each test.
}
procedure TZTestMasterDetailCaseMBCs.SetUp;
begin
  inherited SetUp;

  MasterQuery := CreateQuery;
  MasterDataSource := TDataSource.Create(nil);
  MasterDataSource.DataSet := MasterQuery;

  DetailQuery := CreateQuery;
  DetailQuery2 := CreateQuery;
  DetailQuery3 := CreateQuery;
end;

{**
  Removes data after each test.
}
procedure TZTestMasterDetailCaseMBCs.TearDown;
begin
  DetailQuery.Close;
  DetailQuery.Free;

  DetailQuery2.Close;
  DetailQuery2.Free;

  DetailQuery3.Close;
  DetailQuery3.Free;

  MasterQuery.Close;
  MasterQuery.Free;

  MasterDataSource.Free;

  inherited TearDown;
end;

const
  Str1: ZWideString = #$0422#$0435#$0441#$0442; // "Test" in Cyrillic letters
  Str2: ZWideString = 'An address of ' + #$0422#$0435#$0441#$0442; // the same

procedure TZTestMasterDetailCaseMBCs.TestClientDatasetWithForeignKey_ApplyUpdates;
var
  SQLMonitor: TZSQLMonitor;
  procedure SetTheData(Index: Integer);
  begin
    MasterQuery.Append;
    MasterQuery.FieldByName('dep_id').AsInteger := TestRowID + Index;
    MasterQuery.FieldByName('dep_name').AsString := GetDBTestString(Str1, Connection.DbcConnection.GetConSettings);
    MasterQuery.FieldByName('dep_shname').AsString := 'abc';
    MasterQuery.FieldByName('dep_address').AsString := GetDBTestString(Str2, Connection.DbcConnection.GetConSettings);

    CheckEquals(True, (MasterQuery.State = dsInsert), 'MasterQuery Insert-State');

    DetailQuery.Append;
    DetailQuery.FieldByName('p_id').AsInteger := TestRowID + Index;
    DetailQuery.FieldByName('p_dep_id').AsInteger := TestRowID + Index;
    DetailQuery.FieldByName('p_name').AsString := GetDBTestString(Str1, Connection.DbcConnection.GetConSettings);
    DetailQuery.FieldByName('p_begin_work').AsDateTime := now;
    DetailQuery.FieldByName('p_end_work').AsDateTime := now;
    DetailQuery.FieldByName('p_picture').AsString := '';
    DetailQuery.FieldByName('p_resume').AsString := '';
    DetailQuery.FieldByName('p_redundant').AsInteger := 5;
    CheckEquals(True, (DetailQuery.State = dsInsert), 'MasterQuery Insert-State');
  end;
begin
  Connection.AutoCommit := False;
  //Connection.TransactIsolationLevel := tiReadCommitted;
  SQLMonitor := TZSQLMonitor.Create(nil);
  SQLMonitor.Active := True;
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.CachedUpdates := True;
  MasterQuery.Open;

  DetailQuery.SQL.Text := 'SELECT * FROM people';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'dep_id';
  DetailQuery.LinkedFields := 'p_dep_id';
  DetailQuery.CachedUpdates := True;
  DetailQuery.Open;
  SetTheData(0);
  try
    try
      DetailQuery.ApplyUpdates;
      MasterQuery.ApplyUpdates;
      Connection.Commit;
      Fail('Wrong ApplyUpdates behavior!');
    except on E: Exception do
      begin
        CheckNotTestFailure(E);
        DetailQuery.CancelUpdates;
        MasterQuery.CancelUpdates;
        SetTheData(1);
        //DetailQuery.Options := DetailQuery.Options + [doUpdateMasterFirst];
        MasterQuery.ApplyUpdates;
        DetailQuery.ApplyUpdates;
        Connection.Commit;
        CheckEquals(True, (MasterQuery.State = dsBrowse), 'MasterQuery Browse-State');
        CheckEquals(True, (DetailQuery.State = dsBrowse), 'DetailQuery Browse-State');
      end;
    end;
  finally
    MasterQuery.SQL.Text := 'delete from people where p_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    MasterQuery.SQL.Text := 'delete from department where dep_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    SQLMonitor.Free;
  end;
end;

{**
  Runs a test for in extendet clientdatset rules
  All detail-queries should be updated in a single transaction.
  But now the MasterTable should be updated first for an valid ForegnKey.
  Then all DetailTables should have been updated.
  Very tricky and has to deal with MetaData informations.
}
procedure TZTestMasterDetailCaseMBCs.TestClientDatasetWithForeignKey_doUpdateMasterFirst;
var
  SQLMonitor: TZSQLMonitor;
  //CommitCount, I: Integer;
begin
  SQLMonitor := TZSQLMonitor.Create(nil);
  SQLMonitor.Active := True;
  MasterQuery.SQL.Text := 'SELECT * FROM department ORDER BY dep_id';
  MasterQuery.Options := MasterQuery.Options + [doDontSortOnPost];
  MasterQuery.Open;

  CheckStringFieldType(MasterQuery.FieldByName('dep_name').DataType, Connection.DbcConnection.GetConSettings);
  CheckStringFieldType(MasterQuery.FieldByName('dep_shname').DataType, Connection.DbcConnection.GetConSettings);
    //ASA curiousity: if NCHAR and VARCHAR fields set to UTF8-CodePage we get the LONG_Char types as fieldTypes for !some! fields
  if (ProtocolType = protASA) and (Connection.DbcConnection.GetConSettings.ClientCodePage^.CP = 65001) then
    CheckMemoFieldType(MasterQuery.FieldByName('dep_address').DataType, Connection.DbcConnection.GetConSettings)
  else
    CheckStringFieldType(MasterQuery.FieldByName('dep_address').DataType, Connection.DbcConnection.GetConSettings);

  DetailQuery.SQL.Text := 'SELECT * FROM people';
  DetailQuery.MasterSource := MasterDataSource;
  DetailQuery.MasterFields := 'dep_id';
  DetailQuery.LinkedFields := 'p_dep_id';
  DetailQuery.Options := DetailQuery.Options + [doUpdateMasterFirst, doDontSortOnPost];
  DetailQuery.Open;
  //CommitCount := 0;
  try
    MasterQuery.Append;
    MasterQuery.FieldByName('dep_id').AsInteger := TestRowID;
    MasterQuery.FieldByName('dep_name').AsString := GetDBTestString(Str1, Connection.DbcConnection.GetConSettings);
    MasterQuery.FieldByName('dep_shname').AsString := 'abc';
    MasterQuery.FieldByName('dep_address').AsString := GetDBTestString(Str2, Connection.DbcConnection.GetConSettings);

    CheckEquals(True, (MasterQuery.State = dsInsert), 'MasterQuery Insert-State');

    DetailQuery.Append;
    DetailQuery.FieldByName('p_id').AsInteger := TestRowID;
    DetailQuery.FieldByName('p_dep_id').AsInteger := TestRowID;

    DetailQuery.FieldByName('p_begin_work').AsDateTime := now;
    DetailQuery.FieldByName('p_end_work').AsDateTime := now;
    DetailQuery.FieldByName('p_picture').AsString := '';
    DetailQuery.FieldByName('p_resume').AsString := '';
    DetailQuery.FieldByName('p_redundant').AsInteger := 5;
    CheckEquals(True, (DetailQuery.State = dsInsert), 'MasterQuery Insert-State');

    MasterQuery.Post;

    CheckEquals(True, (MasterQuery.State = dsBrowse), 'MasterQuery Browse-State');
    CheckEquals(True, (DetailQuery.State = dsBrowse), 'DetailQuery Browse-State');

    {fix it
    for i := 0 to SQLMonitor.TraceCount -1 do
      if SQLMonitor.TraceList[i].Category = lcTransaction then
        if Pos('COMMIT', UpperCase(SQLMonitor.TraceList[i].Message)) > 0 then
          Inc(CommitCount);
    CheckEquals(1, CommitCount, 'CommitCount'); }
  finally
    MasterQuery.SQL.Text := 'delete from people where p_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    MasterQuery.SQL.Text := 'delete from department where dep_id = '+IntToStr(TestRowID);
    MasterQuery.ExecSQL;
    SQLMonitor.Free;
  end;
end;


initialization
  RegisterTest('component',TZTestMasterDetailCase.Suite);
  RegisterTest('component',TZTestMasterDetailCaseMBCs.Suite);
end.
