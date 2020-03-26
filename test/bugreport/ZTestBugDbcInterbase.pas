{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for Interbase DBC Bug Reports         }
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

unit ZTestBugDbcInterbase;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZCompatibility, ZSqlTestCase;

type

  {** Implements a DBC bug report test case for Interbase. }
  TZTestDbcInterbaseBugReport = class(TZAbstractDbcSQLTestCase)
  protected
//    function GetSupportedProtocols: string; override;
    function SupportsConfig(Config: TZConnectionConfig): Boolean; override;
  published
    procedure Test789879D;
    procedure Test841559;
    procedure Test843655;
    procedure Test865441;
    procedure Test864622;
    procedure Test886914;
    procedure Test886854;
    procedure Test934253;
    procedure Test_SourceForge192;
    procedure TestTicket363;
    procedure TestTicket376_A;
    procedure TestTicket376_B;
  end;

implementation

uses ZTestCase, ZTestConsts, ZDbcMetadata, ZFastCode;

{ TZTestDbcInterbaseBugReport }

//function TZTestDbcInterbaseBugReport.GetSupportedProtocols: string;
//begin
//  Result := pl_all_interbase;
//end;

function TZTestDbcInterbaseBugReport.SupportsConfig(Config: TZConnectionConfig): Boolean;
begin
  Result := Config.Provider = spIB_FB;
end;

procedure TZTestDbcInterbaseBugReport.Test789879D;
const
  FLD_Index = FirstDbcIndex;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.Execute('DELETE FROM TABLE789879');
  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE789879');

  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateCurrency(FLD_Index, 1.14);
    InsertRow;
  end;

  ResultSet := nil;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE789879');
  with ResultSet do
  begin
    Next;
    CheckEquals(1.14, GetCurrency(FLD_Index));
  end;
  ResultSet := nil;
  Statement.Close;
end;

{**
   Runs a test for bug report #833489
   Can't show messages from triggers
}
procedure TZTestDbcInterbaseBugReport.Test841559;
var
  Statement: IZStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.Execute('DELETE FROM TABLE841559');
  try
    Statement.Execute('INSERT INTO TABLE841559 (FLD1, FLD2) VALUES (1, NULL)');
    Fail('Just exception EXCEPTION841559');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
end;

procedure TZTestDbcInterbaseBugReport.Test843655;
const
  B_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  B_TEXT_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  B_IMAGE_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
begin
  if SkipForReason(srClosedBug) then Exit;

  { load data to the stream }
  BinStream := TMemoryStream.Create;
  StrStream := TMemoryStream.Create;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  try
    BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
    BinStream.Size := 512;
    StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
    StrStream.Size := 512;

    Statement.Execute('DELETE FROM BLOB_VALUES');

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateInt(B_ID_Index, TEST_ROW_ID);
      UpdateAsciiStream(B_TEXT_Index, StrStream);
      UpdateBinaryStream(B_IMAGE_Index, BinStream);
      InsertRow;
      Close;
    end;

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
    with ResultSet do
    begin
      CheckEquals(True, Next);
      StrStream1 := GetAsciiStream(B_TEXT_Index);
      try
        CheckEquals(StrStream, StrStream1, '512 bytes string stream');
      finally
        FreeAndNil(StrStream1);
      end;
      BinStream1 := GetBinaryStream(B_IMAGE_Index);
      try
        CheckEquals(BinStream, BinStream1, '512 bytes binary stream');
      finally
        FreeAndNil(BinStream1);
      end;
      Close;
    end;

    BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));
    BinStream.Size := 1024;
    if ConnectionConfig.Transport = traWEBPROXY then
      StrStream.LoadFromFile(TestFilePath('text/lgpl without control characters.txt'))
    else
      StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
    StrStream.Size := 1024;

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
    with ResultSet do
    begin
      Next;
      UpdateAsciiStream(B_TEXT_Index, StrStream);
      UpdateBinaryStream(B_IMAGE_Index, BinStream);
      UpdateRow;
      Close;
    end;

    ResultSet := Statement.ExecuteQuery('SELECT * FROM BLOB_VALUES');
    with ResultSet do
    begin
      CheckEquals(True, Next);
      StrStream1 := GetAsciiStream(B_TEXT_Index);
      try
        CheckEquals(StrStream, StrStream1, '1024 bytes string stream');
      finally
        FreeAndNil(StrStream1);
      end;
      BinStream1 := GetBinaryStream(B_IMAGE_Index);
      try
        CheckEquals(BinStream, BinStream1, '1024 bytes binary stream');
      finally
        FreeAndNil(BinStream1);
      end;
      Close;
    end;
    Statement.Close;
  finally
    FreeAndNil(BinStream);
    FreeAndNil(StrStream);
  end;
end;

{**
   Runs a test for bug report #865441
   ZeosLib reports Ex. numeric(3,1) as IntegerField
}
procedure TZTestDbcInterbaseBugReport.Test864622;
const
  FLD1_Index = FirstDbcIndex;
  FLD2_Index = FirstDbcIndex+1;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE864622');
  with ResultSet do
  begin
    with GetMetadata do
    begin
      CheckEquals(ord(stInteger), Ord(GetColumnType(FLD1_Index)));
      CheckEquals(ord(stCurrency), Ord(GetColumnType(FLD2_Index)));
    end;
    CheckEquals(True, Next);
    CheckEquals(1, GetInt(FLD1_Index));
    CheckEquals(1.2, GetCurrency(FLD2_Index));
    Close;
  end;
end;

{**
   Runs a test for bug report #865441
   Error -104 with Field named PASSWORD in Firebird
}
procedure TZTestDbcInterbaseBugReport.Test865441;
const
  ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  PASSWORD_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.Execute('DELETE FROM TABLE865441');

  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE865441');
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateInt(ID_Index, TEST_ROW_ID);
    UpdateString(PASSWORD_Index, 'passwd');
    InsertRow;
    Close;
  end;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE865441');
  with ResultSet do
  begin
    Next;
    CheckEquals(TEST_ROW_ID, GetInt(ID_Index));
    CheckEquals('passwd', GetString(PASSWORD_Index));
    Close;
  end;

  Statement.Close;
end;

{**
   Runs a test for bug report #886854
   Incorrect field type
}
procedure TZTestDbcInterbaseBugReport.Test886854;
const
  rdb_relation_name = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  rdb_index_name = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  rdb_field_name = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  rdb_field_position = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('select rc.rdb$relation_name as rel_name, ' +
    'rc.rdb$index_name as ind_name, rs.rdb$field_name as field_name, ' +
    'rs.rdb$field_position as field_pos from rdb$relation_constraints rc ' +
    'left join rdb$index_segments rs on rs.rdb$index_name=rc. '+
    'rdb$index_name where rs.rdb$field_name is not null and rs. '+
    'rdb$field_name<>''DEP_ID'' and '+
    'rc.rdb$constraint_type=''PRIMARY KEY'' and rc.rdb$relation_name=''PEOPLE'' ' +
    'order by rc.rdb$relation_name');
  Metadata := ResultSet.GetMetadata;
  with Metadata do
  begin
    CheckEquals(4, GetColumnCount);
    //Client_Character_set sets column-type!!!!
    CheckEquals(ord(stString), ord(GetColumnType(rdb_relation_name)));
    CheckEquals(ord(stString), ord(GetColumnType(rdb_index_name)));
    CheckEquals(ord(stString), ord(GetColumnType(rdb_field_name)));
    CheckEquals(ord(stSmall), ord(GetColumnType(rdb_field_position)));
  end;

  with ResultSet do
  begin
    Next;
    CheckEquals('PEOPLE', GetString(rdb_relation_name));
    CheckEquals(Copy('RDB$PRIMARY2598', 1, Length('RDB$PRIMARY')),
      Copy(GetString(rdb_index_name), 1, Length('RDB$PRIMARY')));
    CheckEquals('P_ID', GetString(rdb_field_name));
    CheckEquals(0, GetInt(rdb_field_position));
    Close;
  end;
end;

{**
  Problem store data in database with character set DOS850
}
procedure TZTestDbcInterbaseBugReport.Test886914;
const
  TABLE886914_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  TABLE886914_DESCRIPTION_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  TABLE886914_FLAG_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  TABLE886914_ID_WhereIndex = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
var
  Statement: IZStatement;
  PreparedStatement: IZPreparedStatement;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Statement.Execute('DELETE FROM TABLE886914');
  Statement.Execute('INSERT INTO TABLE886914 VALUES (1, ''xyz'', ''RU'')');

  PreparedStatement := Connection.PrepareStatement(
    'UPDATE TABLE886914 SET ID=?, DESCRIPTION=?, FLAG=? WHERE ID=?;');
  with PreparedStatement do
  begin
    SetInt(TABLE886914_ID_Index, 2);
    SetString(TABLE886914_DESCRIPTION_Index, '');
    SetString(TABLE886914_FLAG_Index, '');
    SetInt(TABLE886914_ID_WhereIndex, 1);
    ExecuteUpdatePrepared;
    Close;
  end;
  PreparedStatement := nil;

  ResultSet := Statement.ExecuteQuery('SELECT * FROM TABLE886914');
  with ResultSet do
  begin
    Next;
    CheckEquals(2, GetInt(TABLE886914_ID_Index));
    CheckEquals('', GetString(TABLE886914_DESCRIPTION_Index));
    CheckEquals('', GetString(TABLE886914_FLAG_Index));
    Close;
  end;

  ResultSet := nil;
  Statement.Close;
  Statement := nil;
end;

procedure TZTestDbcInterbaseBugReport.Test934253;
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Metadata := Connection.GetMetadata;

  ResultSet := Metadata.GetTables('', '', 'DEPARTMENT', nil);
  with ResultSet do begin
    Check(Next);
    CheckEquals('', GetString(CatalogNameIndex));
    CheckEquals('', GetString(SchemaNameIndex));
    CheckEquals('DEPARTMENT', GetString(TableNameIndex));
    CheckEquals('TABLE', GetString(TableColumnsSQLType));
  end;
  ResultSet.Close;
  ResultSet := nil;

  ResultSet := Metadata.GetTables('', '', 'DEP_VIEW', nil);
  with ResultSet do begin
    Next;
    CheckEquals('', GetString(CatalogNameIndex));
    CheckEquals('', GetString(SchemaNameIndex));
    CheckEquals('DEP_VIEW', GetString(TableNameIndex));
    CheckEquals('VIEW', GetString(TableColumnsSQLType));
  end;
  ResultSet.Close;
  ResultSet := nil;
end;

(*
Hello,
I am trying to use SynDB of mORMot with statement caching.
Seems to be working fine, but the fix for ticket 228 causes error
"Dynamic SQL Error; SQL error code = -502; Attempt to reopen an open cursor.".
The problem is that reused statement opend the cursor in the
ExecuteQueryPrepared call, but the FISC_TR_HANDLE property of reused
resultset is not updated and TZInterbase6XSQLDAResultSet.Next() triggers the error.
I have prepared a sample project to investigate the issue (it is attached).
It also demonstrates duplicating of records described in ticket #362.

*)
procedure TZTestDbcInterbaseBugReport.TestTicket363;
var
  I: Integer;
  Stmt: IZStatement;
  LStmt: IZPreparedStatement;
  LSet: IZResultSet;
begin
  Connection.Open;
  // prepare sample table
  Stmt := Connection.CreateStatement;
  try
    // generate test data
    for I:= 1 to 10 do
      Stmt.ExecuteUpdate('insert into TestTicket363 (id, "value") values (' + ZFastCode.IntTostr(I) + ', ''testing #' + ZFastCode.IntToStr(I) + ''')');
    // direct approach (just like mORMot with SynDBZeos)
    LStmt:= Connection.PrepareStatement('select * from TestTicket363 where id > ?');
    LStmt.SetInt(FirstDbcIndex, 1);
    LSet:= LStmt.ExecuteQueryPrepared;
    I:= 1;
    while LSet.Next do begin // BUG! duplicates the fetched row
      if I = 2 then begin // trigger
        Connection.SetAutoCommit(False);
        Connection.Commit;
        Connection.SetAutoCommit(True);
      end;
      Inc(I);
      if I > 20 then
        Break;
    end;
    Check(I = 10, 'duplicate rows retrieved');
//  LSet:= nil; // trigger stmt reuse (e.g. mORMot statement cache)
    LStmt.ClearParameters;
    // trigger fix for ticket #228
    Connection.SetAutoCommit(False);
    Connection.Commit;
    Connection.SetAutoCommit(True);

    LStmt.SetInt(FirstDbcIndex, 0);
    LSet:= LStmt.ExecuteQueryPrepared; // the transaction ID is not updated
    I:= 1;
    while LSet.Next do begin // BUG! fails here as the fix is trying to open the resultset already opened in ExecuteQueryPrepared
      Inc(I);
      if I > 20 then
        Break;
    end;
    Check(I = 11, 'duplicate rows retrieved');

    LStmt.SetInt(FirstDbcIndex, 0);
    LSet:= LStmt.ExecuteQueryPrepared; // the transaction ID is not updated
    I:= 1;
    while LSet.Next do begin // BUG! fails here as the fix is trying to open the resultset already opened in ExecuteQueryPrepared
      Inc(I);
      Connection.SetAutoCommit(I and 1 = 0);
      if I > 20 then
        Break;
    end;
    Check(I = 11, 'duplicate rows retrieved');
  finally
    if Assigned(LStmt) then begin
      LStmt.Close;
      LStmt:= nil;
    end;
    Stmt.Close;
    Stmt := nil;
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
procedure TZTestDbcInterbaseBugReport.TestTicket376_A;
var
  I: Integer;
  PStmt: IZPreparedStatement;
  Stmt: IZStatement;
  RS: IZResultSet;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  PStmt := nil;
  CheckFalse(Connection.IsClosed, 'Could not establish a connection');
  // prepare sample table with integer key
  try
    Stmt.ExecuteUpdate('delete from Ticket376a where 1=1');
    PStmt := Connection.PrepareStatement('insert into Ticket376a (id, "value") values (?, ?)');
    // generate test data
    for I:= 1 to 4 do begin
      PStmt.SetInt(FirstDbcIndex, I);
      PStmt.SetRawByteString(FirstDbcIndex + 1, IntToRaw(I));
      CheckEquals(1, PStmt.ExecuteUpdatePrepared, 'the updatecount inserting a row');
    end;
    // check read
    // direct approach (just like mORMot with SynDBZeos)
    PStmt:= Connection.PrepareStatement('select * from Ticket376a where id = ?');
    PStmt.SetNull(FirstDbcIndex, stInteger);
    RS:= PStmt.ExecuteQueryPrepared;
    CheckFalse(RS.Next, 'there is no row for null id');
//  RS:= nil; // trigger stmt reuse (e.g. mORMot statement cache)
    PStmt.ClearParameters;

    PStmt.SetInt(FirstDbcIndex, 3);
    RS:= PStmt.ExecuteQueryPrepared; // the transaction ID is not updated
    Check(RS.Next, 'there is a row for id 3');
    CheckFalse(RS.Next, 'there is no second row for id 3');
  finally
    if Assigned(PStmt) then
      PStmt.Close;
    PStmt:= nil;
    Stmt.ExecuteUpdate('delete from Ticket376a where 1=1');
    Stmt := nil;
  end;
end;

procedure TZTestDbcInterbaseBugReport.TestTicket376_B;
var
  I: Integer;
  PStmt: IZPreparedStatement;
  Stmt: IZStatement;
  RS: IZResultSet;
begin
  Stmt := Connection.CreateStatement;
  RS := nil;
  PStmt := nil;
  CheckFalse(Connection.IsClosed, 'Could not establish a connection');
  // prepare sample table with integer key
  try
    Stmt.ExecuteUpdate('delete from Ticket376b where 1=1');
    PStmt := Connection.PrepareStatement('insert into Ticket376b (id, "value") values (?, ?)');
    // generate test data
    for I:= 1 to 4 do begin
      PStmt.SetRawByteString(FirstDbcIndex, IntToRaw(I));
      PStmt.SetRawByteString(FirstDbcIndex + 1, IntToRaw(I));
      CheckEquals(1, PStmt.ExecuteUpdatePrepared, 'the updatecount inserting a row');
    end;
    // check read
    // direct approach (just like mORMot with SynDBZeos)
    PStmt:= Connection.PrepareStatement('select * from Ticket376b where id = ?');
    PStmt.SetNull(FirstDbcIndex, stString);
    RS:= PStmt.ExecuteQueryPrepared;
    CheckFalse(RS.Next, 'there is no row for null id');
//  RS:= nil; // trigger stmt reuse (e.g. mORMot statement cache)
    PStmt.ClearParameters;

    PStmt.SetRawByteString(FirstDbcIndex, '3');
    RS:= PStmt.ExecuteQueryPrepared; // the transaction ID is not updated
    Check(RS.Next, 'there is a row for id 3');
    CheckFalse(RS.Next, 'there is no second row for id 3');
  finally
    if Assigned(PStmt) then
      PStmt.Close;
    PStmt:= nil;
    Stmt.ExecuteUpdate('delete from Ticket376b where 1=1');
    Stmt := nil;
  end;
end;

procedure TZTestDbcInterbaseBugReport.Test_SourceForge192;
var
  DbcCols: IZResultSet;
begin
  DbcCols := Connection.GetMetadata.GetColumns('', '', 'Ticket192', '');
  CheckTrue(Assigned(DbcCols), 'DbcCols is not assigned');
  CheckTrue(DbcCols.Next, 'Could not move to first row');
  CheckEquals('N51', DbcCols.GetString(ColumnNameIndex));
  CheckEquals('NUMERIC', DbcCols.GetString(TableColColumnTypeNameIndex));
  CheckEquals(5, DbcCols.GetInt(TableColColumnSizeIndex));
  CheckEquals(1, DbcCols.GetInt(TableColColumnDecimalDigitsIndex));
  CheckTrue(DbcCols.Next, 'Could not move to second row');
  CheckEquals('N41', DbcCols.GetString(ColumnNameIndex));
  CheckEquals('NUMERIC', DbcCols.GetString(TableColColumnTypeNameIndex));
  CheckEquals(4, DbcCols.GetInt(TableColColumnSizeIndex));
  CheckEquals(1, DbcCols.GetInt(TableColColumnDecimalDigitsIndex));
  CheckTrue(DbcCols.Next, 'Could not move to third row');
  CheckEquals('D51', DbcCols.GetString(ColumnNameIndex));
  CheckEquals('DECIMAL', DbcCols.GetString(TableColColumnTypeNameIndex));
  CheckEquals(5, DbcCols.GetInt(TableColColumnSizeIndex));
  CheckEquals(1, DbcCols.GetInt(TableColColumnDecimalDigitsIndex));
  CheckFalse(DbcCols.Next, 'Could move behind third row');
end;

initialization
  RegisterTest('bugreport',TZTestDbcInterbaseBugReport.Suite);
end.
