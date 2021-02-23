{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for MSSql Component Bug Reports       }
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

unit ZTestBugCompMSSql;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_MSSQL_SYBASE}
uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Classes, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDataset,
  ZDbcIntfs, ZSqlTestCase,ZCompatibility, ZDbcDbLib;

type

  {** Implements a bug report test case for MSSql components. }
  TZTestCompMSSqlBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test959307; //wrong defined????
    procedure Test953072;
    procedure Test728955;
    procedure Test833489;
    procedure Test907497;
    procedure Mantis54;
    procedure Mantis164;
    procedure Test_SelectInformation_Schema;
    procedure TestSF306;
    procedure TestSF378;
    procedure TestSF380a;
    procedure TestSF380b;
    procedure TestSF382;
    procedure TestSF383;
    procedure TestSF391;
    procedure TestSF402;
    procedure TestSF421;
 end;

{$ENDIF ZEOS_DISABLE_MSSQL_SYBASE}
implementation
{$IFNDEF ZEOS_DISABLE_MSSQL_SYBASE}

uses SysUtils, Types, FmtBCD, DateUtils,
  ZTestConsts, ZSysUtils, ZTestCase, ZStoredProcedure, ZAbstractRODataset,
  ZSqlUpdate;

{ TZTestCompMSSqlBugReport }

function TZTestCompMSSqlBugReport.GetSupportedProtocols: string;
begin
  Result := 'mssql,sybase,OleDB,odbc_a,odbc_w,ado';
end;

{**
  Access Violation during ZReadOnlyQuery.Open
  In method TZAbstractRODataset.InternalInitFieldDefs:
}
procedure TZTestCompMSSqlBugReport.Test728955;
var
  Query: TZReadOnlyQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateReadOnlyQuery;
  try
    Query.SQL.Text := 'SELECT * FROM department';
    Query.Open;
    CheckEquals(4, Query.FieldCount);

    CheckEquals(1, Query.FieldByName('dep_id').AsInteger);
    CheckEquals('Line agency', Query.FieldByName('dep_name').AsString);
    Query.Next;
    CheckEquals(2, Query.FieldByName('dep_id').AsInteger);
    CheckEquals('Container agency', Query.FieldByName('dep_name').AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure TZTestCompMSSqlBugReport.Test833489;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
end;

procedure TZTestCompMSSqlBugReport.Test907497;
var
  StoredProc: TZStoredProc;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  StoredProc := TZStoredProc.Create(nil);
  try
    StoredProc.Connection := Connection;
    StoredProc.StoredProcName := 'proc907497';
    StoredProc.ParamByName('@zzz').AsInteger := 12345;
    StoredProc.ExecProc;
    CheckEquals(7890, StoredProc.ParamByName('@zzz').AsInteger);
  finally
    StoredProc.Free;
  end;
end;

{**
   test for Bug#953072 - problem with queries with empty owner name
}
procedure TZTestCompMSSqlBugReport.Test953072;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from master..sysobjects';
    Query.Open;
    CheckEquals(False, Query.IsEmpty);
  finally
    Query.Free;
  end;
end;

{**
  test for Bug#959307 - empty string parameter translate as null
}
procedure TZTestCompMSSqlBugReport.Test959307;
var
  Query: TZQuery;
  StoredProc: TZStoredProc;
  DblibConn: IZDBLibConnection;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  if Supports(Connection.DbcConnection, IZDBLibConnection, DblibConn) then begin
    if (DblibConn.GetHostVersion < 9000000) and (DblibConn.GetServerProvider = spMSSQL) then
      Fail('This test cannot succeed for MS SQL 2000 with DBLib. The dblib API (dbrpcparam) doesn''t allow to distinguish between empty strings and null.');
  end;

  {perfectly resolveable with ODBC, OleDB, ADO}
  StoredProc := TZStoredProc.Create(nil);
  Query := CreateQuery;
  try
    StoredProc.Connection := Connection;
    StoredProc.StoredProcName := 'proc959307';
    Query.SQL.Text := 'select * from table959307';

    StoredProc.ParamByName('@p').AsString := 'xyz';
    StoredProc.ExecProc;
    Query.Open;
    CheckEquals('xyz', Query.FieldByName('fld1').AsString);
    Query.Close;

    StoredProc.ParamByName('@p').AsString := '';
    StoredProc.ExecProc;
    Query.Open;
    CheckEquals('', Query.FieldByName('fld1').AsString);
    CheckEquals(False, Query.FieldByName('fld1').IsNull);
    Query.Close;

    StoredProc.ParamByName('@p').Value := Null;
    StoredProc.ExecProc;
    Query.Open;
    CheckEquals('', Query.FieldByName('fld1').AsString);
    CheckEquals(True, Query.FieldByName('fld1').IsNull);
    Query.Close;
  finally
    StoredProc.Free;
    Query.Free;
  end;
end;

(*
When we open a query on this table and uses TZQuery.Append to add a new row with NO VALUE for the testcol,
Zeos uses the Resultset-Metadata to find out the default value for the rows with no value set.
MSSQL delivers for the row testcol the default value '(NULL)' instead of 'NULL'.
Zeos interpretes this value correct as string and sends 'N'(NULL)'' to the database as default value.
The result is: The new value for testcol is '(NULL)' as string instead of NULL as default null value.
*)
procedure TZTestCompMSSqlBugReport.TestSF306;
var
  Query: TZQuery;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.Sql.Text := 'select * from TableTicked306';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID;
    Query.Post;
//    CheckEquals(1, Query.RowsAffected,'The updateCount');
    Query.Close;
    Query.Open;
    CheckEquals(1, Query.RecordCount,'The RecordCount');
    Check(Query.Fields[1].IsNull ,'val1 should be null');
    Check(Query.Fields[2].IsNull ,'val2 should be null');
  finally
    FreeAndNil(Query);
    Connection.ExecuteDirect('delete from TableTicked306 where 1=1');
  end;
end;

(**
  User:
    Is this the expected behaviour for summing numeric types using
      odbc or oledb protocol in mssql server?
*)
procedure TZTestCompMSSqlBugReport.TestSF378;
var
  Query: TZQuery;
  eBCD, aBCD: TBCD;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.ParamCheck := false;
    Query.Options := [doCalcDefaults]; //turn of doPreferPrepared
    Query.Sql.Text := 'create table #t (n numeric(16,2))';
    Query.ExecSQL;
    try
      if Protocol = 'ado' then
      try
        Query.Sql.Text := 'select * from  #t';
        Query.Open;
        Check(False, 'ado-behavior changed, change the test!');
      except
        Exit;
      end;
      Query.Sql.Text := 'select * from  #t';
      Query.Open;
      CheckEquals(Ord(ftBCD), Ord(Query.FieldDefs[0].DataType), 'The returned type for numeric-field of the tmp-table');
      Query.Close;
      Query.Sql.Text := 'insert into #t values (1)';
      Query.ExecSQL;
      Query.Sql.Text := 'select sum(n) from #t';
      Query.Open;
      CheckEquals('1', Query.Fields[0].AsString, 'The value of the Sum() of the tmp-table');
      CheckEquals(Ord(ftFmtBCD), Ord(Query.FieldDefs[0].DataType), 'The returned type for the Sum() of the tmp-table numeric type');
      aBCD := Query.Fields[0].AsBCD;
      eBCD := Str2BCD('1'{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}, FmtSettFloatDot{$ENDIF});
      CheckEquals(0, BcdCompare(eBCD, aBCD), Protocol+': BCD compare mismatch, for value: 1');
      Query.Close;
      Query.Sql.Text := 'insert into #t values (20)';
      Query.ExecSQL;
      Query.Sql.Text := 'select sum(n) from #t';
      Query.Open;
      CheckEquals(Ord(ftFmtBCD), Ord(Query.FieldDefs[0].DataType), 'The returned type for the Sum() of the tmp-table numeric type');
      aBCD := Query.Fields[0].AsBCD;
      eBCD := Str2BCD('21'{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}, FmtSettFloatDot{$ENDIF});
      CheckEquals(0, BcdCompare(eBCD, aBCD), Protocol+': BCD compare mismatch, for value: 21');
      Query.Close;
      Query.Sql.Text := 'insert into #t values (300)';
      Query.ExecSQL;
      Query.Sql.Text := 'select sum(n) from #t';
      Query.Open;
      aBCD := Query.Fields[0].AsBCD;
      eBCD := Str2BCD('321'{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}, FmtSettFloatDot{$ENDIF});
      CheckEquals(0, BcdCompare(eBCD, aBCD), Protocol+': BCD compare mismatch, for value: 321');
      Query.Close;
      Query.Sql.Text := 'insert into #t values (0.1)';
      Query.ExecSQL;
      Query.Sql.Text := 'select sum(n) from #t';
      Query.Open;
      aBCD := Query.Fields[0].AsBCD;
      eBCD := Str2BCD('321.1'{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}, FmtSettFloatDot{$ENDIF});
      CheckEquals(0, BcdCompare(eBCD, aBCD), Protocol+': BCD compare mismatch, for value: 321.1');
      Query.Close;
      Query.Sql.Text := 'insert into #t values (0.02)';
      Query.ExecSQL;
      Query.Sql.Text := 'select sum(n) from #t';
      Query.Open;
      aBCD := Query.Fields[0].AsBCD;
      eBCD := Str2BCD('321.12'{$IFDEF HAVE_BCDTOSTR_FORMATSETTINGS}, FmtSettFloatDot{$ENDIF});
      CheckEquals(0, BcdCompare(eBCD, aBCD), Protocol+': BCD compare mismatch, for value: 321.12');
    finally
      if Protocol <> 'ado' then begin
        Query.Sql.Text := 'drop table #t';
        Query.ExecSQL;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

(**
  User:
  Is this the correct syntax and expected behaviour for using TZTable for
    freetds, odbc, ado and oledb protocol in mssql server?
  Answer is !Yes!, we do not get metainformations from the server.
**)
procedure TZTestCompMSSqlBugReport.TestSF380a;
var
  Query: TZQuery;
  Table: TZTable;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Check(Connection.UseMetadata, 'UseMetadata should be true for this test.');
  Query := CreateQuery;
  try
    Query.ParamCheck := false;
    Query.Options := [doCalcDefaults];
    Query.Sql.Add('create table #t (i int)');
    Query.Sql.Add('insert into #t values (0)');
    Query.ExecSQL; //this now kills the #t table using ADO, even if !NO! prepare oslt is called by us
    Check((Query.RowsAffected = -1) or (Query.RowsAffected = 1), 'Rows affected of first command in batch');
    //end up the command batch:
    //Check(Query.NextRowsAffected, 'There is a second updatecount available');
    CheckEquals(1, Query.RowsAffected, 'Rows affected of second command in batch');
    //ado seems to execute some sp's in background, thus (MS-Bug skope of temp-table ends with SP's)
    //ado is not able to see the table any more
    Query.Sql.Text := 'select * from #t';
    Query.Open;
    CheckEquals(0, Query.Fields[0].AsInteger, 'The previously set value should be returned.');
    //the OleDB GetSchema() does not return any rows for the tempdb schema
    //reason is unkown, we need to document this on our side..
    try
      Query.Edit;
      Query.Fields[0].AsInteger := 1;
      Query.Post;
      Check(False, 'unexpected behaviour change! -> Change the test');
    except on E: Exception do
      CheckNotTestFailure(E, 'Expected beahvior');
    end;
    Query.Close;
  finally
    FreeAndNil(Query);
  end;

  Table := CreateTable;
  try
    Table.Options := [doCalcDefaults];
    Table.TableName := '#t';
    Table.Open;
    CheckEquals(0, Table.Fields[0].AsInteger, 'The previously set value should be returned.');
    try
      Table.Edit;
      Table.Fields[0].AsInteger := 1;
      Table.Post;
      Check(False, 'unexpected behaviour change! -> Change the test');
    except on E: Exception do
      CheckNotTestFailure(E, 'Expected beahvior');
    end;
    Table.Close;
  finally
    FreeAndNil(Table);
    Connection.ExecuteDirect('drop table #t');
  end;
end;

procedure TZTestCompMSSqlBugReport.TestSF380b;
var
  Query: TZQuery;
  Table: TZTable;
  UpdateSQL: TZUpdateSQL;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Check(Connection.UseMetadata, 'UseMetadata should be true for this test.');
  Query := CreateQuery;
  UpdateSQL := TZUpdateSQL.Create(nil);
  try
    Query.UpdateObject := UpdateSQL;
    UpdateSQL.DeleteSQL.Text := 'delete from #t where i= :old_i';
    UpdateSQL.InsertSQL.Text := 'insert into #t (i) values (:new_i)';
    UpdateSQL.ModifySQL.Text := 'update #t set i = :new_i where i = :old_i';
    try
      Query.ParamCheck := false;
      Query.Options := [doCalcDefaults];
      Query.Sql.Add('create table #t (i int)');
      Query.Sql.Add('insert into #t values (0)');
      Query.ExecSQL;
      CheckEquals(1, Query.RowsAffected, 'Rows affected of second command in batch');
      //ado seems to execute some sp's in background, thus (MS-Bug skope of temp-table ends with SP's)
      //ado is not able to see the table any more
      Query.Sql.Text := 'select * from #t';
      Query.Open;
      CheckEquals(0, Query.Fields[0].AsInteger, 'The previously set value should be returned.');
      //the OleDB GetSchema() does not return any rows for the tempdb schema
      //reason is unkown, we need to document this on our side..
      Query.Fields[0].ReadOnly := False;
      Query.Edit;
      Query.Fields[0].AsInteger := 2;
      Query.Post;
      Query.Close;
    finally
      FreeAndNil(Query);
    end;

    Table := CreateTable;
    try
      Table.Options := [doCalcDefaults];
      Table.UpdateObject := UpdateSQL;
      Table.TableName := '#t';
      Table.Open;
      CheckEquals(2, Table.Fields[0].AsInteger, 'The previously set value should be returned.');
      Table.Fields[0].ReadOnly := False;
      Table.Edit;
      Table.Fields[0].AsInteger := 1;
      Table.Post;
      Table.Close;
    finally
      FreeAndNil(Table);
      Connection.ExecuteDirect('drop table #t');
    end;
  finally
    UpdateSQL.Free;
  end;
end;

(*
  When you use TZQuery.Locate with TLocateOptions.loPartialKey specified and
  try to search for a whole string match it will not go past the first record or
  if you try to search for partial string match and there is empty string
  inbetween it will stop on the record with an empty string.
*)
procedure TZTestCompMSSqlBugReport.TestSF382;
var
  Query: TZQuery;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Check(Connection.UseMetadata, 'UseMetadata should be true for this test.');
  Query := CreateQuery;
  try
    Query.ParamCheck := false;
    Query.Options := [doCalcDefaults];
    Query.Sql.Text := 'select * from (values (''apple''), (''banana''), (''cherry'')) as x(fruit)';
    Query.Open;
    Query.Locate('fruit', 'cherry', [loPartialKey]);
    CheckEquals(3, Query.RecNo, 'Wrong record number located');
    Query.Close;
    Query.Sql.Text := 'select * from (values (''apple''), (''''), (''banana''), (''cherry'')) as x(fruit)';
    Query.Open;
    Query.Locate('fruit', 'che', [loPartialKey]);
    CheckEquals(4, Query.RecNo, 'Wrong record number located');
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;

(*
When I execute a query containing varchar(max) column with the odbc_w protocol
in mssql server I get 'Division by zero' at line 1803 in dbc\ZDbcODBCResultSet.pas
(fMaxFetchableRows := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(1, (Cardinal(fZBufferSize) div RowSize)*Byte(Ord(not LobsInResult)))).
I am using Zeos 7.3 6050, Delphi 10.2 25.0.26309.314,
Microsoft SQL Server 13.0.1601.5 and Windows 10 1903 18362.418.
*)
procedure TZTestCompMSSqlBugReport.TestSF383;
var
  Query: TZQuery;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Check(Connection.UseMetadata, 'UseMetadata should be true for this test.');
  Query := CreateQuery;
  try
    Query.ParamCheck := false;
    Query.Options := [doCalcDefaults];
    Query.Sql.Text := 'create table #t (fruit varchar(max))';
    Query.SQL.Add('insert into #t values (''apple''), (''banana''), (''cherry'')');
    Query.SQL.Add('select * from #t');
    Query.SQL.Add('drop table #t');
    Query.Open; //devision by zero
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TZTestCompMSSqlBugReport.TestSF391;
var
  Query: TZQuery;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.Sql.Text := 'select SUM(0.0)';
    Query.Open;
    CheckEquals(0, Query.Fields[0].AsFloat, 'SUM(0.0) should return a zero BCD');
  finally
    FreeAndNil(Query);
  end;
end;

{ Using Zeos revision 6213 when you try to convert a field of type
datetime to string you get only the time, but its not correct as well. }

procedure TZTestCompMSSqlBugReport.TestSF402;
var
  Query: TZQuery;
  dtE, dtA: TDateTime;
  S: String;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.Sql.Add('set dateformat mdy');
    Query.Sql.Add('select cast(''2020-01-01 08:30:45'' as datetime)');
    Query.Sql.Add('union all');
    Query.Sql.Add('select cast(''2020-01-01 00:00:00'' as datetime)');
    Query.Sql.Add('union all');
    Query.Sql.Add('select cast(''1899-12-30 10:00:00'' as datetime)');
    Query.Sql.Add('order by 1 Desc');
    Query.Open;
    dtE := EncodeDate(2020,01,01);
    dtE := dtE+EncodeTime(8,30,45,0);
    dtA := Query.Fields[0].AsDateTime;
    CheckEqualsDate(dtE, dtA, [dpYear, dpMonth, dpDay, dpHour, dpMin, dpSec], 'Should be "2020-01-01 08:30:45" ');
    CheckEquals(DateTimeToStr(dtE), query.Fields[0].AsString, 'Should be "2020-01-01 08:30:45" ');
    Query.Next;
    CheckFalse(Query.Eof);
    dtA := Query.Fields[0].AsDateTime;
    S := DateTimeToStr(dtA);
    CheckEquals(S, query.Fields[0].DisplayText, 'Should be "2020-01-01" ');
    S := DateToStr(dtA);
    CheckEquals(S, query.Fields[0].DisplayText, 'Should be "2020-01-01" ');
    CheckNotEquals(S, query.Fields[0].AsString, 'Should be "2020-01-01 10:00:00" ');
    Query.Next;
    CheckFalse(Query.Eof);
    dtA := Query.Fields[0].AsDateTime;
    S := TimeToStr(dtA);
    CheckEquals(S, query.Fields[0].DisplayText, 'Should be "10:00:00" ');
    CheckNotEquals(S, query.Fields[0].AsString, 'Should be "1899-12-30 10:00:00" ');
  finally
    FreeAndNil(Query);
  end;
end;

(*
When you try to read TField.AsBytes value of a binary column the data is missing.
Windows 10, Microsoft SQL Server 2016, Delphi 10.2 and Zeos 7.3 6427.
The problem was present in revision 6222 as well.
*)
procedure TZTestCompMSSqlBugReport.TestSF421;
var
  q: TZQuery;
begin
  Q := CreateQuery;
  try
    Check(Q <> nil);
    //ado does some own logic behind the scenes such as calling some sp's ->
    //#t doesn't exist any more
    if Protocol = 'ado' then
      Exit;
    q.Sql.Add('create table #t (b varbinary(128));');
    q.Sql.Add('insert into #t values (0x6170706c65), (0x62616e616e61), (0x636865727279);');
    q.ExecSql;
    q.Sql.Clear;
    q.Sql.Add('select * from #t');
    q.Open;
    while not q.Eof do begin
      CheckFalse(q.Fields[0].IsNull, 'the field is not null');
      {$IFDEF TFIELD_HAS_ASBYTES}
      Check(Length(q.Fields[0].AsBytes) > 0, 'there are some bytes in queue');
      {$ELSE}
      Check(Length(q.Fields[0].AsString) > 0, 'there are some bytes in queue');
      {$ENDIF}
      (* expected output:
      5 apple
      6 banana
      6 cherry
      *)
      q.Next;
    end;
    q.Close;
    q.Sql.Clear;
    q.Sql.Add('drop table #t');
    q.ExecSql;
  finally
    q.Free;
  end;
end;

{ Mantis #54 }
{
The fields with data type "BigInt" in "MS-SQL" behave like "float" and not like Integer.
For example:
Suppose that 2 data bases are had. The one in MySQL and the other in MS-SQL Server, with a table each one.
The structure of the tables is the following one:

MS-SQL Server
CREATE TABLE Mantis54 (
    Key1 int NOT NULL ,
    BI bigint NULL ,
    F float NULL)

EgonHugeist:
  The resultset-Metadata returning 8, which is probably a floating type..
}
procedure TZTestCompMSSqlBugReport.Mantis54;
var
  Query: TZQuery;
begin
//??  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from mantis54';
    Query.Open;
    CheckEquals(ord(ftInteger), ord(Query.Fields[0].DataType));
    CheckEquals(ord(ftLargeInt), ord(Query.Fields[1].DataType), 'Int64/LongInt expected');
    CheckEquals(ord(ftFloat), ord(Query.Fields[2].DataType));
  finally
    Query.Free;
  end;
end;

procedure TZTestCompMSSqlBugReport.Mantis164;
const
  sGUID1 = '{546ED716-BB88-468C-8CCE-D7111CF5E1EF}';
  sGUID2 = '{BAF24A92-C8CE-4AB4-AEBC-3D4A9BCB0946}';
var
  Query: TZQuery;
  GUID1, GUID2: TGUID;
  Bts1, Bts2: TByteDynArray;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from Mantis164';
    Query.Open;
    CheckEquals(9, Query.Fields.Count);
    CheckStringFieldType(Query.Fields[0], Connection.ControlsCodePage);
    CheckEquals(ord(ftSmallInt), ord(Query.Fields[1].DataType));
    CheckEquals(ord(ftDateTime), ord(Query.Fields[2].DataType));
    {$IFDEF WITH_FTGUID}
    CheckEquals(ord(ftGUID), ord(Query.Fields[3].DataType), 'uniqueidentifier (GUID)');
    CheckEquals(ord(ftGUID), ord(Query.Fields[4].DataType), 'uniqueidentifier (GUID)');
    {$ELSE}
    CheckEquals(ord(ftBytes), ord(Query.Fields[3].DataType), 'uniqueidentifier (GUID)');
    CheckEquals(ord(ftBytes), ord(Query.Fields[4].DataType), 'uniqueidentifier (GUID)');
    {$ENDIF}
    CheckEquals(ord(ftBoolean), ord(Query.Fields[5].DataType));
    //tds returns wrong flags for fixed types... so the test fails
    CheckEquals(ftBytes, Query.Fields[6].DataType, 'binary(16)'); //correct only with metadata
    CheckEquals(ftVarBytes, Query.Fields[7].DataType, 'varbinary(16)');
    CheckEquals(ftBlob, Query.Fields[8].DataType, 'Image');
    Query.Insert;
    Query.Fields[0].AsString := 'abc';
    Query.Fields[1].AsInteger := 1;
    Query.Fields[2].AsDateTime := Now;
    GUID1 := StringToGUID(sGUID1);
    GUID2 := StringToGUID(sGUID2);
    Bts1 := nil;
    System.SetLength(Bts1, 16);
    Bts2 := nil;
    System.SetLength(Bts2, 16);
    System.Move(Pointer(@GUID1)^, Pointer(Bts1)^, 16);
    System.Move(Pointer(@GUID2)^, Pointer(Bts2)^, 16);
    Query.Fields[6].Value := Bts1;
    Query.Fields[7].Value := Bts2;

    {$IFDEF WITH_FTGUID}
    Query.Fields[3].AsString := sGUID1;
    Query.Fields[4].AsString := sGUID2;
    {$ELSE}
    Query.Fields[3].Value := Bts1;
    Query.Fields[4].Value := Bts2;
    {$ENDIF}
    Query.Fields[5].AsBoolean := True;
    Query.Post;

    Query.Insert;
    Query.Fields[0].AsString := 'abc';
    Query.Fields[1].AsInteger := 2;
    Query.Fields[2].AsDateTime := Now;
    GUID1 := StringToGUID(sGUID1);
    GUID2 := StringToGUID(sGUID2);
    System.SetLength(Bts1, 16);
    System.Move(Pointer(@GUID1)^, Pointer(Bts1)^, 16);
    Query.Fields[6].Value := Bts1;

    {$IFDEF WITH_FTGUID}
    Query.Fields[3].AsString := sGUID1;
    {$ELSE}
    Query.Fields[3].Value := Bts1;
    {$ENDIF}
    Query.Fields[5].AsBoolean := True;
    Query.Post;

    Query.Close;
    Query.Open;
    CheckEquals('abc', Query.Fields[0].AsString);
    CheckEquals(1, Query.Fields[1].AsInteger);
    CheckEquals(True, Query.Fields[5].AsBoolean);
    {$IFDEF WITH_FTGUID}
    CheckEquals(sGUID1, Query.Fields[3].AsString);
    CheckEquals(sGUID2, Query.Fields[4].AsString);
    {$ELSE}
    Query.Fields[3].GetData(@GUID1);
    Query.Fields[4].GetData(@GUID2);
    CheckEquals(sGUID1, GUIDToString(GUID1));
    CheckEquals(sGUID2, GUIDToString(GUID2));
    {$ENDIF}
    Query.Delete;
    Query.Close;
  finally
    Query.SQL.Text := 'delete from Mantis164';
    Query.ExecSQL;
    Query.Free;
  end;
end;

procedure TZTestCompMSSqlBugReport.Test_SelectInformation_Schema;
var
  Query: TZQuery;
begin
  Connection.Connect;
  Check(Connection.Connected, 'Failed to establish a connection');
  if Connection.DbcConnection.GetServerProvider <> spMSSQL then
    Exit;
  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from INFORMATION_SCHEMA.TABLES';
    Query.Open;
    Check(Query.Active);
  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestCompMSSqlBugReport.Suite);
{$ENDIF ZEOS_DISABLE_MSSQL_SYBASE}
end.
