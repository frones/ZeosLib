{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for MySQL Component Bug Reports       }
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

unit ZTestBugCompMySql;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}
uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZDataSetUtils,
  ZSqlTestCase, ZCompatibility, ZTestConsts, ZSqlUpdate, ZSqlProcessor,
  ZAbstractRODataset;

type

  {** Implements a bug report test case for MySQL components. }
  TZTestCompMySQLBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test735226;
    procedure Test726788;
    procedure Test735299;
    procedure Test740144;
    procedure Test740899;
    procedure Test724542;
    procedure Test739444;
    procedure Test733236;
    procedure Test727373;
    procedure Test771576;
    procedure Test768163;
    procedure Test779520;
    procedure Test799863;
    procedure Test000001;
    procedure Test817607;
    procedure Test828147;
    procedure Test840608;
    procedure Test844415;
    procedure Test849723;
    procedure Test833489;
    procedure Test869609;
    procedure Test865564;
    procedure Test881634;
    procedure Test884135;
    procedure Test886841;
    procedure Test894367;
    procedure Test914436;
    procedure Test920589;
    procedure Test938705;
    procedure Test952976;
    procedure Test957126;
    procedure Test961337;
    procedure Test981208;
    procedure Test987022;
    procedure Test989474;
    procedure Test1045286;
    procedure Test1023149;
    procedure TestMantis220;
    procedure TestMantis235;
    procedure TestTicket52;
    procedure TestMS56OBER9357;
    procedure TestTicket186_MultipleResults;
    procedure TestBin_Collation;
    procedure TestEvalue2Params;
    procedure TestTicked240;
    procedure TestTicked389;
    procedure TestProcAbtest_WithParamCheck;
    procedure TestProcAbtest_WithoutParamCheck;
    procedure TestTicket304;
    procedure TestBigIntError;
    procedure TestBCD_Refresh_p156227;
    procedure TestTicket265;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL}

uses ZTestCase, ZDbcMySQL, ZSysUtils, ZDbcProperties, ZDbcMySqlMetadata;

{ TZTestCompMySQLBugReport }

function TZTestCompMySQLBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_mysql;
end;

{**
  Test the bug report #735226.

  "Duplicate field name" exception
  Initial Comment: When I use a query like this:

  SELECT * FROM TableA
  LEFT JOIN TableB ON TableB.TableAId = TableA.Id

  where both TableA and TableB have a column
  named 'Id', I get the exception:' "Duplicate field name 'Id' "
}
procedure TZTestCompMySQLBugReport.Test735226;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM table735226a'
      + ' LEFT JOIN table735226b ON table735226a.referenceid = table735226b.id';
    Query.Open;
    CheckEquals(3, Query.FieldCount);
    CheckEquals('id', Query.FieldDefs[0].Name);
    CheckEquals('referenceid', Query.FieldDefs[1].Name);
    CheckEquals('id_1', Query.FieldDefs[2].Name);
    Query.Last;
    Query.Close;

    Query.SQL.Text := 'SELECT table735226a.id as Table1Id, table735226b.id as Table2Id'
      + ' FROM table735226a LEFT JOIN table735226b ON table735226a.referenceid = table735226b.id';
    Query.Open;
    CheckEquals(2, Query.FieldCount);
    CheckEquals('Table1Id', Query.FieldDefs[0].Name);
    CheckEquals('Table2Id', Query.FieldDefs[1].Name);
    Query.Last;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #726788.

  Mixed case table problem (6.0.8)

  Initial Comment:
  When a table has got an mixed case tablename, there
  is an EZSQLException raised when the table is opened
  ('Row data is not accessable'). Making the tablename
  upper- or lowercase solves the problem, but isn't
  sollution
}
procedure TZTestCompMySQLBugReport.Test726788;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM Table726788';
    Query.Open;
    CheckEquals(1, Query.FieldCount);
    CheckEquals('FieldName', Query.FieldDefs[0].Name);
    Query.Last;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #735299.

  The commonly used MySQL Boolean fieldtype for
  MySQL, enum('Y','N'), is not working as expected in
  version 6.0.8. When you use this fieldtype, the values
  returned are strings, not booleans. The fieldtype is
  correct (Boolean) in version 5.4.1.

  EH: but this is not correct for all cases. loads users want to have it mapped
  as string. So since 2018 MySQL still not have a true bool type we'll
  map fieldtype bit(1) as Boolean which is the only type with just a 0/1
  switch. Keep hands far away from (un)signed tinyint(1) which has a range
  of shortint/byte
}
procedure TZTestCompMySQLBugReport.Test735299;
var
  Query: TZQuery;
  B: Boolean;
const
  tbl: array[Boolean] of String = ('table735299', 'table735299_bit');
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Connect;
  for B := (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    if Connection.Connected then
      Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Query := CreateQuery;
    try
      // Query.RequestLive := True;
      Query.SQL.Text := 'DELETE FROM '+tbl[B];
      Query.ExecSQL;

      Query.SQL.Text := 'SELECT * FROM '+tbl[B];
      Query.Open;
      CheckEquals(3, Query.FieldCount);
      CheckEquals(Ord(ftInteger), Ord(Query.FieldDefs[0].DataType));
      CheckEquals(Ord(ftBoolean), Ord(Query.FieldDefs[1].DataType));
      CheckEquals(Ord(ftBoolean), Ord(Query.FieldDefs[2].DataType));
      Query.Append;
      Query.Fields[0].AsInteger := 1;
      Query.Fields[1].AsBoolean := True;
      Query.Fields[2].AsBoolean := False;
      Query.Post;
      Query.Close;

      Query.SQL.Text := 'SELECT * FROM '+tbl[B];
      Query.Open;
      CheckEquals(1, Query.RecordCount);
      CheckEquals(1, Query.Fields[0].AsInteger);
      CheckEquals(True, Query.Fields[1].AsBoolean);
      CheckEquals(False, Query.Fields[2].AsBoolean);
      Query.Delete;
      Query.Close;

      Query.SQL.Text := 'SELECT * FROM '+tbl[B];
      Query.Open;
      CheckEquals(0, Query.RecordCount);
      Query.Close;
    finally
      Query.Free;
    end;
  end;
end;

{**
  Test the bug report #740144.

  Query.Locate('Boolean', VarArrayOf([True]), [])

  Gives the exception in :
  Could not convert variant of type (OleStr) into type
  (Boolean).
}
procedure TZTestCompMySQLBugReport.Test740144;
var
  Query: TZQuery;
  B: Boolean;
const
  tbl: array[Boolean] of String = ('table735299', 'table735299_bit');
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Connect;
  for B := (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    if Connection.Connected then
      Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Query := CreateQuery;
    try
      // Query.RequestLive := True;
      Query.SQL.Text := 'DELETE FROM '+tbl[B];
      Query.ExecSQL;

      Query.SQL.Text := 'SELECT * FROM '+tbl[B];
      Query.Open;
      Query.Append;
      Query.Fields[0].AsInteger := 1;
      Query.Fields[1].AsBoolean := True;
      Query.Fields[2].AsBoolean := False;
      Query.Post;
      Query.Append;
      Query.Fields[0].AsInteger := 2;
      Query.Fields[1].AsBoolean := False;
      Query.Fields[2].AsBoolean := True;

      Query.Post;
      Query.Append;
      Query.Fields[0].AsInteger := 3;
      Query.Post;
      Query.Close;

      Query.SQL.Text := 'SELECT * FROM '+tbl[B]+' ORDER BY id';
      Query.Open;
      CheckEquals(True, Query.Locate('fld1', VarArrayOf([True]), []));
      CheckEquals(1, Query.RecNo);
      CheckEquals(True, Query.Locate('fld1,fld2', VarArrayOf([Null, Null]), []));
      CheckEquals(3, Query.RecNo);
      Query.Close;
    finally
      Query.Free;
    end;
  end;
end;

{**
  Test the bug report #740899.

  PreparedStatement for Field=? (Null) generated
  Field=NULL instead Field IS NULL.
}
procedure TZTestCompMySQLBugReport.Test740899;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table740899';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id) VALUES(1)';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id) VALUES(2)';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id,fld) VALUES(3,''xxx'')';
    Query.ExecSQL;
    Query.SQL.Text := 'INSERT INTO table740899 (id,fld) VALUES(4,''yyy'')';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table740899 ORDER BY id';
    Query.Open;
    CheckEquals(4, Query.RecordCount);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Edit;
    Query.Fields[1].AsString := 'zzz';
    Query.Post;
    Query.Refresh;
    CheckEquals('zzz', Query.Fields[1].AsString);

    Query.Next;
    Query.Delete;
    Query.Refresh;
    CheckEquals(3, Query.RecordCount);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table740899';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #740899.

  Error posting data to update but no changes happened.
}
procedure TZTestCompMySQLBugReport.Test724542;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'DELETE FROM table724542';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM table724542';
    Query.Open;
    Query.Append;
    Query.Fields[0].AsInteger := TEST_ROW_ID;
    Query.Fields[1].AsString := 'abcdefghijklmnopq';
    Query.Fields[2].Value := Null;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table724542 where fld1 = ' + IntToStr(TEST_ROW_ID);
    Query.Open;
    Query.Edit;
    Query.Fields[2].AsString := 'abcdefg';
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'SELECT * FROM table724542 where fld1 = ' + IntToStr(TEST_ROW_ID);
    Query.Open;
    CheckEquals(TEST_ROW_ID, Query.Fields[0].AsInteger);
    CheckEquals('abcdefghijklmnopq', Query.Fields[1].AsString);
    CheckEquals('abcdefg', Query.Fields[2].AsString);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #739444.

  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestCompMySQLBugReport.Test739444;
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
  Test the bug report #733236.

  Locate for time fields because riased
  "Convertion is not possible for column 1 from Time to Timestamp"
}
procedure TZTestCompMySQLBugReport.Test733236;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from table733236';
    Query.Open;

    Query.Locate('time', EncodeTime(23, 59, 59, 0), []);
    Query.Locate('date', EncodeDate(2001, 01, 01), []);
    Query.Locate('datetime', EncodeDate(2004, 12, 31) +
      EncodeTime(23, 59, 59, 0), []);
    Query.Locate('timestamp', EncodeDate(2004, 12, 31) +
      EncodeTime(23, 59, 59, 0), []);

    Query.Close;
  finally
    Query.Free;
  end;

end;

{**
  Test the bug report #727373.

  The TZQuery with TZUpdateObject can't update records
  when left outer joins is used in sql query.
}
procedure TZTestCompMySQLBugReport.Test727373;
var
  Query: TZQuery;
  UpdateSql: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    UpdateSql := TZUpdateSQL.Create(nil);
    try
      // Query.RequestLive := True;

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
  Test the bug report #771576.

  Problem related to ':' character in SQL query.
}
procedure TZTestCompMySQLBugReport.Test771576;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'set @user_var=0';
    Query.ExecSQL;

    Query.SQL.Text := 'update people set p_id=(@user_var:=@user_var+1)'
      + ' where 1=0';
    Query.ExecSQL;

    Query.ParamCheck := False;
    Query.SQL.Text := 'update people set p_id=(@user_var:=@user_var+1)'
      + ' where 1=0';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #771576.

  unsigned int field problem.
}
procedure TZTestCompMySQLBugReport.Test768163;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'delete from table768163';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select * from table768163';
    Query.Open;
    Query.Append;
    {$IFDEF WITH_FTLONGWORD}
    TLongWordField(Query.Fields[0]).Value := 2147483647;
    {$ELSE}
    TLargeIntField(Query.Fields[0]).AsLargeInt := 2147483647;
    {$ENDIF}
    TLargeIntField(Query.Fields[1]).AsLargeInt := -5;//-2147483648;
    Query.Post;
    Query.Close;

    Query.SQL.Text := 'select * from table768163';
    Query.Open;
    {$IFDEF WITH_FTLONGWORD}
    CheckEquals(2147483647, TLongWordField(Query.Fields[0]).Value);
    {$ELSE}
    CheckEquals(2147483647, TLargeIntField(Query.Fields[0]).AsLargeInt);
    {$ENDIF}
    CheckEquals(-5{-2147483648}, TLargeIntField(Query.Fields[1]).AsLargeInt);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #779520.

  Invalid values of float fields when ReadOnly = False
}
procedure TZTestCompMySQLBugReport.Test779520;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select c_id, c_cost from cargo';
    Query.Open;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1769.4300, Query.Fields[1].AsFloat, 0.001);
    Query.Close;

    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals(1769.4300, Query.Fields[1].AsFloat, 0.001);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #799863.

  Cannot see YEAR type in Delphi.
}
procedure TZTestCompMySQLBugReport.Test799863;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from table799863';
    Query.Open;
    CheckEquals(Ord(ftWord), Ord(Query.Fields[0].DataType));
    CheckEquals(2, Query.RecordCount);
    CheckEquals(1940, Query.Fields[0].AsInteger);
    Query.Next;
    CheckEquals(2003, Query.Fields[0].AsInteger);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #000001

  ZQuery does not display MySQL fields with longtext type.
}
procedure TZTestCompMySQLBugReport.Test000001;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from table000001';
    Query.Open;
    CheckEquals(2, Query.Fields.Count);
    if ( Connection.ControlsCodePage = cCP_UTF16 ) then
    {$IFDEF WITH_WIDEMEMO}
      CheckEquals(Ord(ftWideMemo), Ord(Query.Fields[0].DataType))
    else
    {$ENDIF}
      CheckEquals(Ord(ftMemo), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBlob), Ord(Query.Fields[1].DataType));
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #817607

  Problem when refresh Query component.

  Problem when use Refresh() in Query component who have fields
  which name with space symbol. Ect. filed name : 'zeos cool component'
}
procedure TZTestCompMySQLBugReport.Test817607;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'select * from table817607';
    Query.Open;
    CheckEquals('id', Query.Fields[0].DisplayName);
    CheckEquals('fruit name', Query.Fields[1].DisplayName);
    Query.RecNo := 2;
    Query.Refresh;
    CheckEquals(2, Query.RecNo);
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #828147

  No automatic post of memo or blob field.
}
procedure TZTestCompMySQLBugReport.Test828147;
var
  Query: TZQuery;
  Temp: PChar;
  Stream: TStream;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'update table828147 set txt=''abc''';
    Query.ExecSQL;

    // Query.RequestLive := True;
    Query.SQL.Text := 'select id, txt from table828147';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    //Client_Character_set sets column-type!!!!
    {$IFDEF WITH_WIDEMEMO}
    if ( Connection.ControlsCodePage= cCP_UTF16 ) then
      CheckEquals(Ord(ftWideMemo), Ord(Query.Fields[1].DataType))
    else
    {$ENDIF}
      CheckEquals(Ord(ftMemo), Ord(Query.Fields[1].DataType));

    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Stream := Query.CreateBlobStream(Query.Fields[1], bmWrite);
    try
      Temp := 'xyz';
      {$IFDEF WITH_WIDEMEMO}
      if ( Connection.ControlsCodePage = cCP_UTF16 ) then
        Stream.Write(PWideChar(WideString(Temp))^, Length(Temp)*2)
      else
      {$ENDIF}
        Stream.Write(PAnsiChar(AnsiString(Temp))^, Length(Temp));
    finally
      Stream.Free;
    end;
    Query.Next;

    Query.Close;
    Query.Open;

    CheckEquals(1, Query.RecordCount);
    CheckEquals('xyz', Query.Fields[1].AsString);

    Query.Close;

    Query.SQL.Text := 'update table828147 set txt=''abc''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #840608

  Wrong =NULL in where clause of generated DML statements.
}
procedure TZTestCompMySQLBugReport.Test840608;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'update table840608 set name=''PARACETAMOL (GPO)''';
    Query.ExecSQL;

    Query.SQL.Text := 'select * from table840608';
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals('PARACETAMOL (GPO)', Query.FieldByName('name').AsString);

    Query.Edit;
    Query.FieldByName('name').AsString := 'xxx';
    Query.Post;

    Query.Refresh;

    CheckEquals(1, Query.RecordCount);
    CheckEquals('xxx', Query.FieldByName('name').AsString);

    Query.Close;

    Query.SQL.Text := 'update table840608 set name=''PARACETAMOL (GPO)''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #844415

  TZSQLStrings.RebuildAll makes invalid SQL req.
}
procedure TZTestCompMySQLBugReport.Test844415;
var
  Query: TZQuery;
  SQLProcessor: TZSQLProcessor;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'update people set p_name="\"\"\\" where 1=0';
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  SQLProcessor := TZSQLProcessor.Create(nil);
  try
    SQLProcessor.Connection := Connection;
    SQLProcessor.Script.Text := 'update people set p_name="\"\"\\" where 1=0;';
    SQLProcessor.Execute;
  finally
    SQLProcessor.Free;
  end;
end;

{**
  Runs a test for bug report #849723

  Update bug on time field bug.
}
procedure TZTestCompMySQLBugReport.Test849723;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'UPDATE table849723 SET fld2=''abc''';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT fld1, fld2 FROM table849723';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(0, Query.Fields[0].AsDateTime, 0);
    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[1].AsString := 'xyz';
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(0, Query.Fields[0].AsDateTime, 0);
    CheckEquals('xyz', Query.Fields[1].AsString);
    Query.Close;

    Query.SQL.Text := 'UPDATE table849723 SET fld2=''abc''';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #833489
   AutoCommit=FALSE starting a transaction causing an error
}
procedure TZTestCompMySQLBugReport.Test833489;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Disconnect;
  Connection.AutoCommit := False;
  Connection.Connect;
end;

{**
   Runs a test for bug report #869609
   Wrong behaviour of AutoIncremented Fields in MySQL.
}
procedure TZTestCompMySQLBugReport.Test869609;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'DELETE FROM table869609';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT id FROM table869609';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(0, Query.RecordCount);
    (* EH: just prepared ->uncomment this if the fieldtype
      ftAutoInc should be supported
    {$IFDEF WITH_TAUTOREFRESHFLAG}
    if (Query.Fields[0].AutoGenerateValue = arAutoInc) then
      CheckEquals(ftAutoInc, Query.Fields[0].DataType) else
    {$ENDIF}*)
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[0].DataType));

    Query.Append;
    Check(Query.Fields[0].IsNull);
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals(True, Query.Fields[0].AsInteger <> 0);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table869609';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #865564
  Incorrect precision of float fields in MySQL.
}
procedure TZTestCompMySQLBugReport.Test865564;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT fld1, fld2 FROM table865564';
    Query.Open;
    CheckEquals(1, Query.RecordCount);

    CheckEquals(Ord(ftFloat), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftBCD), Ord(Query.Fields[1].DataType));

//    CheckEquals(2, Query.FieldDefs[0].Precision);
//    CheckEquals(4, Query.FieldDefs[1].Precision);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Runs a test for bug report #881634
  Complex select statement returns wrong field types.
}
procedure TZTestCompMySQLBugReport.Test881634;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT idt2, ft2, table881634a.ft1'
      + ' FROM table881634b INNER JOIN table881634a'
      + ' ON (table881634b.ft1 = table881634a.idt1)';
    Query.Open;

    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
    if ( Connection.ControlsCodePage = cCP_UTF16 ) then
    begin
      CheckEquals(Ord(ftWideString), Ord(Query.Fields[1].DataType));
      CheckEquals(Ord(ftWideString), Ord(Query.Fields[2].DataType));
    end
    else
    begin
      CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));
      CheckEquals(Ord(ftString), Ord(Query.Fields[2].DataType));
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
   Runs a test for bug report #881634
   Problem in master-detail links with unsigned int keys.
}
procedure TZTestCompMySQLBugReport.Test884135;
var
  MasterQuery: TZQuery;
  MasterDataSource: TDataSource;
  DetailQuery: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  MasterQuery := CreateQuery;
  MasterDataSource := TDataSource.Create(nil);
  DetailQuery := CreateQuery;
  try
    MasterQuery.ReadOnly := True;
    MasterQuery.SQL.Text := 'SELECT * FROM table884135a';
    MasterDataSource.DataSet := MasterQuery;

    // DetailQuery.RequestLive := True;
    DetailQuery.CachedUpdates := True;
    DetailQuery.SQL.Text := 'SELECT * FROM table884135b';
    DetailQuery.MasterSource := MasterDataSource;
    DetailQuery.MasterFields := 'id';
    DetailQuery.IndexFieldNames := 'mid';
    DetailQuery.Options := DetailQuery.Options + [doAlwaysDetailResync];

    MasterQuery.Open;
    DetailQuery.Open;

    DetailQuery.Insert;
    DetailQuery.Fields[0].AsInteger := 12345;
    DetailQuery.Post;

    DetailQuery.Close;
    MasterQuery.Close;
  finally
    DetailQuery.Free;
    MasterDataSource.Free;
    MasterQuery.Free;
  end;
end;

{**
  Runs a test for bug report #886841
  Error in processing for default values for columns
  with type enum(y,n) in MySQL.
}
procedure TZTestCompMySQLBugReport.Test886841;
var
  Query: TZQuery;
  B: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Connect;
  for B := (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Query := CreateQuery;
    try
      Query.SQL.Text := 'DELETE FROM table886841';
      Query.ExecSQL;

      // Query.RequestLive := True;
      Query.SQL.Text := 'SELECT * FROM table886841';
      Query.Open;

      if not B
      then CheckEquals(Ord(ftBoolean), Ord(Query.Fields[0].DataType))
      else Check(Query.Fields[0].DataType in [ftString, ftWideString]);
      Query.Append;
      Query.Post;
      if not B
      then CheckEquals(True, Query.Fields[0].AsBoolean)
      else CheckEquals('y', LowerCase(Query.Fields[0].AsString));

      Query.Close;

      Query.SQL.Text := 'DELETE FROM table886841';
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  end;
end;

{**
  Runs a test for bug report #894367
  Wrong processing of queries with non-unique field names.
}
procedure TZTestCompMySQLBugReport.Test894367;
var
  Query: TZQuery;
  B: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Connect;
  for B := (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Query := CreateQuery;
    try
      Query.SQL.Text := 'SELECT a.fld1, b.fld2, 1 + 2 as fld2, a.fld2,'
        + ' c.fld1, b.fld1, c.fld2, ''xyz'' as fld1'
        + ' FROM table894367a as a, table894367b as b, table894367c as c';
      Query.Open;

      if ( Connection.ControlsCodePage = cCP_UTF16 ) then
        CheckEquals(Ord(ftWideString), Ord(Query.Fields[0].DataType))
      else
        CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
      CheckEquals(Ord(ftFloat), Ord(Query.Fields[1].DataType));
     // behavior inconsistency between mysql and mariadb:
     // mysql maps const ordinals to Largeint, mariadb to integer(if in range)
     // CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[2].DataType));
      if not B
      then CheckEquals(Ord(ftBoolean), Ord(Query.Fields[3].DataType))
      else Check(Query.Fields[3].DataType in [ftString, ftWideString]);
      CheckEquals(Ord(ftBlob), Ord(Query.Fields[4].DataType));
      CheckEquals(Ord(ftInteger), Ord(Query.Fields[5].DataType));
      CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[6].DataType));
      if ( Connection.ControlsCodePage = cCP_UTF16 ) then
        CheckEquals(Ord(ftWideString), Ord(Query.Fields[7].DataType))
      else
        CheckEquals(Ord(ftString), Ord(Query.Fields[7].DataType));

      Query.Close;

      Query.SQL.Text := 'SELECT a.*, 1 + 2 as fld2, b.*,'
        + ' c.fld1, c.fld2, ''xyz'' as fld1'
        + ' FROM table894367a as a, table894367b as b, table894367c as c';
      Query.Open;

      if ( Connection.ControlsCodePage = cCP_UTF16 ) then
        CheckEquals(Ord(ftWideString), Ord(Query.Fields[0].DataType))
      else
        CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
      if not B
      then CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType))
      else Check(Query.Fields[1].DataType in [ftString, ftWideString]);
     // behavior inconsistency between mysql and mariadb:
     // mysql maps const ordinals to Largeint, mariadb to integer(if in range)
     // CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[2].DataType));
      CheckEquals(Ord(ftInteger), Ord(Query.Fields[3].DataType));
      CheckEquals(Ord(ftFloat), Ord(Query.Fields[4].DataType));
      CheckEquals(Ord(ftBlob), Ord(Query.Fields[5].DataType));
      CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[6].DataType));
      if ( Connection.ControlsCodePage = cCP_UTF16 ) then
        CheckEquals(Ord(ftWideString), Ord(Query.Fields[7].DataType))
      else
        CheckEquals(Ord(ftString), Ord(Query.Fields[7].DataType));

      Query.Close;
    finally
      Query.Free;
    end;
  end;
end;

{**
  Runs a test for bug report #914436
  Bug in ZDbcMySqlUtils-TEXT Fields aren't correct identified.
}
procedure TZTestCompMySQLBugReport.Test914436;
{var
  Query: TZQuery;}
begin
  if SkipForReason(srClosedBug) then Exit;

  {Test914436Query := CreateQuery;
  try

    Query.SQL.Text := 'SELECT fld1, fld2 FROM table914436';
    Query.Open;

    CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));

    Query.Close;

  finally
    Query.Free;
  end;}
end;

{**
  Runs a test for bug report #920589
  Bug in method TZGenericSQLQuoteState.NextToken.
}
procedure TZTestCompMySQLBugReport.Test920589;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try

    Query.SQL.Text := 'SELECT "aa\"aa"';
    Query.Open;

    //Client_Character_set sets column-type!!!!
    if (Connection.ControlsCodePage = cCP_UTF16) then
      CheckEquals(Ord(ftWideString), Ord(Query.Fields[0].DataType))
    else
      CheckEquals(Ord(ftString), Ord(Query.Fields[0].DataType));
    CheckEquals('aa"aa', Query.Fields[0].AsString);

    Query.Close;

    Query.SQL.Text := 'insert delayed into log_sql'
      + ' (datum, uzivid, lockid, sqlcommand)'
      + ' values (now(), 3, 11952,'
      + ' "update global set value = \"21.3.2004 18:50:17\"'
      + ' where name = \"DAY_JOB_LAST_RUN\"")';
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #938705.
  ZUpdateSQL params behavior when fieldnames have blank spaces.
}
procedure TZTestCompMySQLBugReport.Test938705;
var
  Query: TZQuery;
  UpdateSql: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    UpdateSql := TZUpdateSQL.Create(nil);
    try
      Query.SQL.Text := 'delete from `Table 938705`';
      Query.ExecSQL;

      Query.SQL.Text := 'select * from `Table 938705`';
      Query.UpdateObject := UpdateSql;

      UpdateSql.InsertSQL.Text := 'insert into `Table 938705` (`Field First`,'
        + ' `Field Second`) values (:`Field First`, :`Field Second`)';
      UpdateSql.ModifySQL.Text := 'update `Table 938705`'
        + ' set `Field First`=:`Field First`, `Field Second`=:`Field Second`'
        + ' where `Field First`=:`OLD_Field First`';
      UpdateSql.DeleteSQL.Text := 'delete from `Table 938705`'
        + ' where `Field First`=:`OLD_Field First`';

      Query.Open;
      Query.Append;
      Query.FieldByName('Field First').AsInteger := 1;
      Query.FieldByName('Field Second').AsString := 'abc';
      Query.Post;
      Query.Close;

      Query.Open;
      CheckEquals(False, Query.IsEmpty);
      CheckEquals(1, Query.FieldByName('Field First').AsInteger);
      CheckEquals('abc', Query.FieldByName('Field Second').AsString);
      Query.Edit;
      Query.FieldByName('FIELD FIRST').AsInteger := TEST_ROW_ID;
      Query.FieldByName('FIELD SECOND').AsString := 'xyz';
      Query.Post;
      Query.Refresh;
      CheckEquals(TEST_ROW_ID, Query.FieldByName('Field FIRST').AsInteger);
      CheckEquals('xyz', Query.FieldByName('Field SECOND').AsString);
      Query.Delete;
      Query.Close;

      Query.Open;
      CheckEquals(True, Query.IsEmpty);
    finally
      UpdateSql.Free;
    end;
  finally
    Query.Connection.ExecuteDirect('delete from `Table 938705`');
    Query.Free;
  end;
end;

{**
  Test the bug report #952976.
  ExecSQL error doesn't raise exception with MySQL 4.1.
}
procedure TZTestCompMySQLBugReport.Test952976;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;

    Query.SQL.Text := 'INSERT INTO people(bad_column) VALUES(''abc'')';
    try
      Query.ExecSQL;

      Fail('Wrong exception handling in MySQL');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;
  finally
    Query.Free;
  end;
end;

{**
  Test the bug report #957126.
  Incorrect processing of empty strings in default values in MySQL driver.
}
procedure TZTestCompMySQLBugReport.Test957126;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'DELETE FROM table957126';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT fld1, fld2 FROM table957126';
    // Query.RequestLive := True;
    Query.Open;
    CheckEquals(0, Query.RecordCount);

    Query.Append;
    CheckEquals(True, Query.Fields[0].IsNull);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.RecordCount);
    CheckEquals(False, Query.Fields[0].IsNull);
    CheckEquals('', Query.Fields[0].AsString);
    CheckEquals(True, Query.Fields[1].IsNull);
    Query.Close;

    Query.SQL.Text := 'DELETE FROM table957126';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #961337
  ENUM('Y','N') is not recognized as Boolean when column name is renamed.

  EH: but this is not correct for all cases. loads users want to have it mapped
  as string. So since 2018 MySQL still not have a true bool type we'll
  map fieldtype bit(1) as Boolean which is the only type with just a 0/1
  switch. Keep hands far away from (un)signed tinyint(1) which has a range
  of shortint/byte
}
procedure TZTestCompMySQLBugReport.Test961337;
var
  Query: TZQuery;
  B: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;
  Connection.Connect;
  for B := (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Query := CreateQuery;
    try
      Query.SQL.Text := 'SELECT id, fld1, fld2, fld1 as fld3,'
        + ' fld2 as fld4 FROM table735299';
      // Query.RequestLive := True;
      Query.Open;
      if not B then begin
        CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
        CheckEquals(Ord(ftBoolean), Ord(Query.Fields[1].DataType));
        CheckEquals(Ord(ftBoolean), Ord(Query.Fields[2].DataType));
        CheckEquals(Ord(ftBoolean), Ord(Query.Fields[3].DataType));
        CheckEquals(Ord(ftBoolean), Ord(Query.Fields[4].DataType));
      end else begin
        CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType));
        Check(Query.Fields[1].DataType in [ftString, ftWideString]);
        Check(Query.Fields[2].DataType in [ftString, ftWideString]);
        Check(Query.Fields[3].DataType in [ftString, ftWideString]);
        Check(Query.Fields[4].DataType in [ftString, ftWideString]);
      end;
      Query.Close;
    finally
      Query.Free;
    end;
  end;
end;

{**
  Tests the bug report #981208
  SELECT * FROM mydb.mytable is not updateable.
}
procedure TZTestCompMySQLBugReport.Test981208;
var
  Query: TZQuery;
  DatabaseName: string;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Disconnect;
  DatabaseName := Connection.Database;
  Connection.Database := 'mysql';
  Connection.Connect;

  Query := CreateQuery;
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'DELETE FROM `' + DatabaseName + '`.table740899';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT * FROM `' + DatabaseName + '`.table740899';
    // Query.RequestLive := True;
    Query.Open;

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'abc';
    Query.Post;

    Query.Refresh;
    CheckEquals(1, Query.Fields[0].AsInteger);
    CheckEquals('abc', Query.Fields[1].AsString);

    Query.Edit;
    Query.Fields[0].AsInteger := 2;
    Query.Fields[1].AsString := 'xyz';
    Query.Post;

    Query.Close;
    Query.Open;
    CheckEquals(2, Query.Fields[0].AsInteger);
    CheckEquals('xyz', Query.Fields[1].AsString);

    Query.Delete;

    Query.Close;

    Query.SQL.Text := 'DELETE FROM `' + DatabaseName + '`.table740899';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #987022
  LargeInt fields are not filtered by Dataset.Filter.
}
procedure TZTestCompMySQLBugReport.Test987022;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT fld1, fld2 FROM table987022 WHERE 1=0';
    // Query.RequestLive := True;
    Query.CachedUpdates := True;

    Query.Open;
    CheckEquals(Ord(ftLargeInt), Ord(Query.Fields[0].DataType));
    if ( Connection.ControlsCodePage = cCP_UTF16 ) then
      CheckEquals(Ord(ftWideString), Ord(Query.Fields[1].DataType))
    else
      CheckEquals(Ord(ftString), Ord(Query.Fields[1].DataType));

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsString := 'aaa';
    Query.Post;

    Query.Append;
    TLargeIntField(Query.Fields[0]).AsLargeInt := Int64(MaxInt) + 1;
    Query.Fields[1].AsString := 'bbb';
    Query.Post;

    Query.Append;
    Query.Fields[0].AsInteger := 2;
    Query.Fields[1].AsString := 'ccc';
    Query.Post;

    Query.Append;
    TLargeIntField(Query.Fields[0]).AsLargeInt := Int64(MaxInt) + 2;
    Query.Fields[1].AsString := 'ddd';
    Query.Post;

    CheckEquals(4, Query.RecordCount);

    Query.Filter := 'fld1 > 100';
    Query.Filtered := True;
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'fld1 < ' + IntToStr(MaxInt);
    CheckEquals(2, Query.RecordCount);

    Query.Filter := 'fld1 > ' + IntToStr(MaxInt) + ' OR fld2 = ''aaa''';
    CheckEquals(3, Query.RecordCount);

    Query.Filtered := False;
    CheckEquals(4, Query.RecordCount);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #989474
  UpdateSQL doesn't process columns with duplicated names in queries like:
  SELECT T1.MyField, T2.MyField...
}
procedure TZTestCompMySQLBugReport.Test989474;
var
  Query: TZQuery;
  UpdateSQL: TZUpdateSQL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  UpdateSQL := TZUpdateSQL.Create(nil);
  try
    UpdateSQL.InsertSQL.Text :=
      'INSERT INTO table989474 VALUES(:CustID_1,:CreateDate_1)';

    Query.UpdateObject := UpdateSQL;
    // Query.RequestLive := True;

    Query.SQL.Text := 'DELETE FROM table989474';
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT A.CustID, A.CreateDate, B.CustID, B.CreateDate'
      + ' FROM table989474 A INNER JOIN table989474 B ON A.CustID=B.CustID';
    Query.Open;

    CheckEquals('CustID', Query.Fields[0].FieldName);
    CheckEquals('CreateDate', Query.Fields[1].FieldName);
    CheckEquals('CustID_1', Query.Fields[2].FieldName);
    CheckEquals('CreateDate_1', Query.Fields[3].FieldName);

    Query.Append;
    Query.Fields[0].AsInteger := 1;
    Query.Fields[1].AsDateTime := EncodeDate(1949, 07, 06);
    Query.Fields[2].AsInteger := 2;
    Query.Fields[3].AsDateTime := EncodeDate(1975, 04, 08);
    Query.Post;

    Query.Close;
    Query.Open;

    CheckEquals(1, Query.RecordCount);
    CheckEquals(2, Query.Fields[0].AsInteger);
    CheckEquals(EncodeDate(1975, 04, 08), Query.Fields[1].AsDateTime);
    CheckEquals(2, Query.Fields[2].AsInteger);
    CheckEquals(EncodeDate(1975, 04, 08), Query.Fields[3].AsDateTime);

    Query.Close;
  finally
    Query.Free;
    UpdateSQL.Free;
  end;
end;

procedure TZTestCompMySQLBugReport.TestBin_Collation;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM `mysql`.`user`';

    Query.Open;
    CheckStringFieldType(Query.Fields[0], Connection.ControlsCodePage);
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TZTestCompMySQLBugReport.TestEvalue2Params;
var
  Query: TZReadOnlyQuery;
  B: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Connect;
  if not (Connection.DbcConnection as IZMySQLConnection).SupportsFieldTypeBit then
    Exit;
  for B := True downto False do begin
    if Connection.Connected then
      Connection.Disconnect;
    Connection.Properties.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];

    Query := CreateReadOnlyQuery;
    Query.Connection.Connect;
    Query.SQL.Text := 'SELECT p_ID, p_begin_work, '+
      '/* IF condition, show SubName value*/ '+
      'IF (:ShowSubName = True, p_name, NULL) AS SubName '+
      'FROM people WHERE people.p_ID = :ID ';
    try
      CheckEquals(2, Query.Params.Count, 'Wrong param count');
      if B
      then Query.ParamByName('ShowSubName').AsBoolean := True
      else Query.ParamByName('ShowSubName').AsInteger := Ord(True);
      Query.ParamByName('ID').AsInteger := 1;
      Query.Open;
      Check(Query.RecordCount = 1, 'Wrong ReocrdCount');
      Check(not Query.FieldByName('SubName').IsNull, 'Should be null');
    finally
      Query.Free;
    end;
  end;
end;

{**
  Tests the bug report #1045286
  Improper IsNull method for Text columns.
}
procedure TZTestCompMySQLBugReport.Test1045286;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT fld FROM table1045286';

    Query.Open;
    CheckEquals(1, Query.RecordCount);
    CheckMemoFieldType(Query.Fields[0], Connection.ControlsCodePage);
    CheckEquals('', Query.Fields[0].AsString);
    CheckEquals(False, Query.Fields[0].IsNull);

    Query.Close;
  finally
    Query.Free;
  end;
end;

{**
  Tests the bug report #1023149
  Incorrect processing '\' characters in text fields.
}
procedure TZTestCompMySQLBugReport.Test1023149;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    // Query.RequestLive := True;
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'DELETE FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    Query.SQL.Text := 'INSERT INTO blob_values (b_id, b_text) VALUES (:id, ''c:\\test.jpg'')';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;

    { Opens a result set. }
    Query.SQL.Text := 'SELECT * FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.Open;
    CheckEquals(TEST_ROW_ID, Query.FieldByName('b_id').AsInteger);
    CheckEquals('c:\test.jpg', Query.FieldByName('b_text').AsString);
    Query.Close;

    { Remove newly created record }
    Query.SQL.Text := 'DELETE FROM blob_values WHERE b_id=:id';
    Query.ParamByName('id').AsInteger := TEST_ROW_ID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

{**
  0000220: MySQL: After next open of DataSet with Open() we get MySQL Error 2014:
  Commands out of sync; you can't run this command now

  Hint this happens on calling StoredProcedure. Fixed with Zeos7 R
}
procedure TZTestCompMySQLBugReport.TestMantis220;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'CALL SingleResultSet()';
    Query.Open;
    Query.Close;
    Query.Open; //Here the Exception was raised
    Query.Close;
  finally
    Query.Free;
  end;
end;

{** Mantis235
I've patched TZMySQLResultSet.Close as suggested in issue 000220.
(Thanks Shkil)
When I do that using TQuery.Open with a stored procedure works great the first time and all subsequent times.

HOWEVER: If I run TZQuery.ExecSQL (not valid for a stored proc with resultset, I know) I get "Commands out of sync..." error and MySQL/Zeos will not let me out of that one.

I don't mind getting an error, but I don't want to have to disconnect and reconnect to the database just to fix being stuck.
}
procedure TZTestCompMySQLBugReport.TestMantis235;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.Properties.Values[DSProps_ValidateUpdateCount] := 'False';
    Query.CachedUpdates := False;

    { Remove previously created record }
    Query.SQL.Text := 'CALL SingleResultSet()';
    Query.ExecSQL;
    Query.ExecSQL; //Here the Exception was raised
  finally
    Query.Free;
  end;
end;

{
I think, there is an error in MatchAfterStar function in ZMatchPattern.pas,
this block:
if TLen = 1 then
begin
Result := MATCH_VALID;
Exit;
end;
returns MATCH_VALID in case Pattern = '*any_pattern*' and Text = '1' (or another only one character)
May be there should be "if PLen = 1 then ...."?
....
than, connect to this table from ZQuery and set Filter property to '*ring*' and Filtered:=true. There should be only 2 records (id=1,6), but you will see 5 filtered records (id=1,2,3,4,6).
}
procedure TZTestCompMySQLBugReport.TestTicket52;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Query.SQL.Text := 'select * from TableTicket52';
  Query.Open;
  Query.Filtered := True;
  Query.Filter := 'filter_test like ''*tring*''';

  try
    CheckEquals(2, Query.RecordCount);
  finally
    Query.Free;
  end;
end;

procedure TZTestCompMySQLBugReport.TestTicked240;
var
  Query: TZQuery;
  I: Integer;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.CachedUpdates := False;
    Query.SQL.Text := 'select * from TableTicket240';
    Query.Open;
    for i := 0 to 9 do begin
      Query.Append;
      Query.Fields[1].AsString := 'aaa';
      Query.Post;
      {$IFDEF WITH_ASLARGEINT}
      if Query.Fields[0] Is TLargeIntField then
        Check(TLargeIntField(Query.Fields[0]).AsLargeInt <> 0, 'autoincrement of unsigned bigint is not retrieved')
      else
      Check(Query.Fields[0].AsInteger <> 0, 'autoincrement of unsigned bigint is not retrieved');
      {$ELSE}
      Check(Query.Fields[0].AsInteger <> 0, 'autoincrement of unsigned bigint is not retrieved');
      {$ENDIF}
    end;
    Query.Close;
  finally
    try
      Query.SQL.Text := 'delete from TableTicket240 where 1=1';
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  end;
end;

(*
I have quite a strange problem. (Mariadb 10.1.16, Dephi7)
I want to read a bigint from a table via dataset.FindField('fieldname').AsString;
With some numbers (0,1,2,3,4,5,10,...) I get the real value but for other numers (6,7,8,9,69,..,609) I get "0"
Any sugestions?
*)
procedure TZTestCompMySQLBugReport.TestTicked389;
const
  TestValues: array[0..10] of Integer = (6,7,8,9,69,77,78,88,96,99,609);
var
  Query: TZQuery;
  I, j: Integer;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.CachedUpdates := False;
    Query.SQL.Text := 'select * from TableTicked389 order by 1';
    Query.Open;
    for i := Low(TestValues) to High(TestValues) do begin
      Query.Append;
      Query.Fields[1].AsInteger := TestValues[i];
      Query.Post;
      {$IFDEF WITH_ASLARGEINT}
      if Query.Fields[0] Is TLargeIntField
      then Check(TLargeIntField(Query.Fields[0]).AsLargeInt > 0, 'autoincrement of unsigned bigint is not retrieved')
      else Check(Query.Fields[0].AsInteger > 0, 'autoincrement of unsigned bigint is not retrieved');
      {$ELSE}
      Check(Query.Fields[0].AsInteger > 0, 'autoincrement of unsigned bigint is not retrieved');
      {$ENDIF}
    end;
    Query.Close;
    for J := 0 to 4 do begin
      Query.Open;
      I := 0;
      while not Query.EOF do begin
        {$IFDEF WITH_ASLARGEINT}
        if Query.Fields[0] Is TLargeIntField
        then Check(TLargeIntField(Query.Fields[0]).AsLargeInt > 0, 'autoincrement of unsigned bigint is not retrieved')
        else Check(Query.Fields[0].AsInteger > 0, 'autoincrement of unsigned bigint is not retrieved');
        {$ELSE}
        Check(Query.Fields[0].AsInteger > 0, 'autoincrement of unsigned bigint is not retrieved');
        {$ENDIF}
        {$IFDEF WITH_ASLARGEINT}
        if Query.Fields[1] Is TLargeIntField
        then CheckEquals(TestValues[I], TLargeIntField(Query.Fields[1]).AsLargeInt, 'value signed bigint is not retrieved')
        else CheckEquals(TestValues[I], Query.Fields[1].AsInteger, 'value signed bigint is not retrieved');
        {$ELSE}
        CheckEquals(TestValues[I], Query.Fields[1].AsInteger, 'value signed is not retrieved');
        {$ENDIF}
        CheckEquals(IntToStr(TestValues[i]), Query.Fields[1].AsString, 'value as String');
        Query.Next;
        Inc(I);
      end;
      Query.Close;
    end;
  finally
    try
      Query.SQL.Text := 'delete from TableTicked389 where 1=1';
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  end;
end;

procedure TZTestCompMySQLBugReport.TestTicket186_MultipleResults;
var
  Query: TZReadOnlyQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateReadOnlyQuery;
  try
    { Remove previously created record }
    Query.SQL.Text := 'CALL ThreeResultSets()';
    Query.ExecSQL;
    Query.ExecSQL;
    Query.Open;
    Query.Close;
    Query.Open;
    CheckEquals(Query.FieldCount, 8,  'ColumnCount of people table');
    Query.Next;
    Check(Query.NextResultSet, 'There is a second resultset available!');
    CheckEquals(7, Query.FieldCount, 'ColumnCount of string_values table');
    Query.Next;
    Check(Query.NextResultSet, 'There is a third resultset available!');
    Query.Next;
    CheckEquals(6, Query.FieldCount, 'ColumnCount of equipment table');
    Query.Close;
  finally
    Query.Free;
  end;
end;

{Exists will return false even if the table exists when run on a table with upper-case name on MySQL.}

procedure TZTestCompMySQLBugReport.TestTicket265;
var tbl: TZTable;
begin
  tbl := CreateTable;
  Check(tbl <> nil);
  try
    tbl.Connection.Connect;
    tbl.TableName := 'PEOPLE';
    if (tbl.Connection.DbcConnection.GetMetadata as IZMySQLDatabaseMetadata).Get_lower_case_table_names > 0 then //not resolvable on case sensitive servers
      Check(tbl.Exists)
    else try
      Check(tbl.Exists)
    except
      on E:Exception do
        CheckNotTestFailure(E);
    end;
  finally
    FreeAndNil(tbl);
  end;
end;

procedure TZTestCompMySQLBugReport.TestTicket304;
var
  qy: TZquery;
begin
  qy := CreateQuery;
  try
    qy.SQL.Text := 'delete from TableTicket304';
    qy.ExecSQL;
    qy.SQL.Text := 'select * from TableTicket304';
    qy.Open;
    CheckEquals(Ord(ftFloat), Ord(qy.FieldByName('LATITUDE').DataType), 'datatype of LATITUDE');
    CheckEquals(Ord(ftFloat), Ord(qy.FieldByName('LONGITUDE').DataType), 'datatype of LONGITUDE');
    CheckEquals(Ord(ftBlob), Ord(qy.FieldByName('point_field').DataType), 'datatype of point_field');
    CheckEquals(Ord(ftBlob), Ord(qy.FieldByName('geometry_field').DataType), 'datatype of geometry_field');
    qy.Append;
    qy.Fields[0].AsInteger := 1;
    qy.FieldByName('LATITUDE').AsFloat := 10;
    qy.FieldByName('LONGITUDE').AsFloat := 20;
    qy.Post;
    qy.Close;
    qy.Open;
    CheckEquals(1, qy.RecordCount, 'there is a row');
    Check(qy.FieldByName('point_field').IsNull, 'The point_field should be null');
    Check(qy.FieldByName('geometry_field').IsNull, 'The geometry_field should be null');
    qy.Edit;
    qy.FieldByName('LATITUDE').AsFloat := 20;
    qy.FieldByName('LONGITUDE').AsFloat := 40;
    qy.Post;
    qy.Close;
    qy.Open;
    CheckEquals(1, qy.RecordCount, 'there is a row');
    CheckFalse(qy.FieldByName('point_field').IsNull, 'The point_field should not be null');
    CheckFalse(qy.FieldByName('geometry_field').IsNull, 'The geometry_field should not be null');
    qy.Edit;
    qy.FieldByName('point_field').Clear;
    try
      qy.Post;
      Check(False, 'The trigger should forbit this');
    except
    end;
    qy.Close;
    qy.Open;
    qy.Edit;
    qy.FieldByName('geometry_field').Clear;
    try
      qy.Post;
      Check(False, 'The trigger should forbit this');
    except
    end;
  finally
    qy.Connection.ExecuteDirect('delete from TableTicket304');
    qy.ExecSQL;
    qy.Free;
  end;
end;

{See:
http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=9357
}
procedure TZTestCompMySQLBugReport.TestMS56OBER9357;
var
  qy: TZquery;
begin
  qy := CreateQuery;
  qy.SQL.Text := 'select * from TableMS56OBER9357';
  qy.Open;
  try
    qy.Append;
    qy.FieldByName('keyfield').value := 1;
    qy.FieldByName('dtField').value := Date;
    qy.FieldByName('infofield').value := 'test';
    qy.Post;

    qy.Edit;
    qy.FieldByName('keyfield').Value := 1;
    qy.FieldByName('dtField').Value := NULL;
    qy.FieldByName('infofield').AsString := 'test';
    qy.Post;

    qy.Edit;
    qy.FieldByName('keyfield').value := 1;
    qy.FieldByName('dtField').Value := date;
    qy.FieldByName('infofield').value := 'test';
    qy.Post;
  finally
    qy.Connection.ExecuteDirect('delete from TableMS56OBER9357');
    qy.ExecSQL;
    qy.Free;
  end;
end;

procedure TZTestCompMySQLBugReport.TestProcAbtest_WithoutParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Check(Query <> nil);
  try
    {$IFNDEF CPU64}
    Connection.Connect;
    //there is no 32 bit dll for MySQL 8+, but the test can pass using libmariadb of v10+
    //otherwise using last 6.1 connection we cat a Maleformed communication error
    if (Query.Connection.DbcConnection.GetMetadata as IZMySQLDatabaseMetadata).IsMySQL and
       (Query.Connection.DbcConnection.GetHostVersion >= EncodeSQLVersioning(8,0,0)) and
       (Query.Connection.DbcConnection.GetClientVersion < EncodeSQLVersioning(7,0,0)) then
         Exit;
    {$ENDIF}
    SQL := 'CALL abtest(?, ?, ?, ?, ?)';
    Query.ParamCheck := False;
    Query.SQL.Text := SQL;
    Query.Params.CreateParam(ftInteger, 'P1', ptInPut);
    Query.Params[0].AsInteger := 10;
    Query.Params.CreateParam(ftInteger, 'P2', ptInPut);
    Query.Params[1].AsInteger := 20;
    Query.Params.CreateParam(ftString, 'P3', ptInPut);
    Query.Params[2].AsString := 'xx';
    Query.Params[2].Precision := 10;
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

procedure TZTestCompMySQLBugReport.TestProcAbtest_WithParamCheck;
var
  Query: TZQuery;
  SQL: String;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Check(Query <> nil);
  try
    Query.Connection.Connect;
    {$IFNDEF CPU64}
    //there is no 32 bit dll for MySQL 8+, but the test can pass using libmariadb of v10+
    //otherwise using last 6.1 connection we cat a Maleformed communication error
    if (Query.Connection.DbcConnection.GetMetadata as IZMySQLDatabaseMetadata).IsMySQL and
       (Query.Connection.DbcConnection.GetHostVersion >= EncodeSQLVersioning(8,0,0)) and
       (Query.Connection.DbcConnection.GetClientVersion < EncodeSQLVersioning(7,0,0)) then
         Exit;
    {$ENDIF}
    Query.ParamCheck := False;
    SQL := 'CALL abtest(?, ?, ?, ?, ?)';
    Query.SQL.Text := SQL;
    Query.Params.CreateParam(ftInteger, 'P1', ptInPut);
    Query.Params[0].AsInteger := 10;
    Query.Params.CreateParam(ftInteger, 'P2', ptInPut);
    Query.Params[1].AsInteger := 20;
    Query.Params.CreateParam(ftString, 'P3', ptInPut);
    Query.Params[2].Precision := 10;
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

(*
Hello,

I have a table in a mysql database with a field defined as Decimal(10,2). To connect to this table
I have a ZQuery component with the following SQL query:

SELECT DATE(SaleDateTime), SUM(VatLow)
FROM tblsale
GROUP BY DATE(SaleDateTime)

A DBGrid is connected to ZQuery.

I can open the query, but whenever I try to refresh the table an exception is raised of class 'External:SIGSEGV' in file
ZSysUtils.pas at line 7454. This occurs whenever SUM(VatLow) is 0.00.
I also noticed that after opening the query, the value 0.00 is displayed as 0000000.00 and other values are displayed
correctly.

Is there a way solve this?

My environment is:
- Lazarus/FPC in Windows 10 build with fpcupdeluxe
- ZEOS 7.3 installed with fpcdeluxe using testing branch

see: https://zeoslib.sourceforge.io/viewtopic.php?f=28&p=156227#p156227
*)
procedure TZTestCompMySQLBugReport.TestBCD_Refresh_p156227;
var Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.Connection.Connect;
    Query.Connection.ExecuteDirect('delete from table_p156227');
    Query.Connection.ExecuteDirect('insert into table_p156227 values '+
      '(1, Cast(''2020-09-09'' as date), 12345678.90), '+
      '(2, Cast(''2020-09-09'' as date), 12345678.90), '+
      '(3, Cast(''2020-09-09'' as date), 12345678.90)');
    Query.SQL.Text := 'select DATE(SaleDateTime), SUM(VatLow) '+
      'from table_p156227 '+
      'group by DATE(SaleDateTime)';
    Query.Open;
    Query.Connection.ExecuteDirect('update table_p156227 set VatLow = 0 where 1=1');
    try
      Query.Refresh;
    Finally
      Query.Connection.Disconnect;
      Query.Close;
    End;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TZTestCompMySQLBugReport.TestBigIntError;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.Connection.Connect;
    Query.Connection.ExecuteDirect('insert into biginterror values (18446744073709551615)');

    Query.SQL.Text := 'SELECT * FROM biginterror';
    Query.Open;
    try
      //writeln(Query.Fields[0].AsString);
      Query.Refresh; // Range check error pops up here
    Finally
      Query.Connection.Disconnect;
      Query.Close;
    End;
  finally
    FreeAndNil(Query);
  end;
end;

initialization
  RegisterTest('bugreport',TZTestCompMySQLBugReport.Suite);
{$ENDIF ZEOS_DISABLE_MYSQL}
end.
