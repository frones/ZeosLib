{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for MySQL DBC Bug Reports            }
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

unit ZTestBugDbcMySql;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZDbcIntfs, ZCompatibility, ZDbcMySql, ZSqlTestCase, ZDbcProperties;

type
  {** Implements a DBC bug report test case for MySql. }
  TZTestDbcMySQLBugReport = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test924861;
    procedure Test702352;
    procedure Test739448;
    procedure Test739444;
    procedure Test768163;
    procedure Test816925;
    procedure Test881634;
    procedure Test961337;
    procedure TestBin_Collation;
    procedure TestTicket365;
    procedure TestTicket284;
    procedure TestTicket442;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL}

uses ZTestCase, ZSysUtils, ZPlainMySqlDriver, FmtBCD, ZDbcLogging;

{ TZTestDbcMySQLBugReport }

function TZTestDbcMySQLBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_mysql;
end;

{**
  Test Case for Bug Report #702352
  Access violation
  Server: MySQL 4.0.3 beta-nt,
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select * from mysql.user

  Access violation at address 006DE326 in module 'myprog.exe'.
}
procedure TZTestDbcMySQLBugReport.Test702352;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  PrStatement: IZPreparedStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select * from mysql.user');
  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('Host'));
  ResultSet.Close;
  Statement.Close;

  PrStatement := Connection.PrepareStatement('select * from mysql.user');
  ResultSet := PrStatement.ExecuteQuery('select * from mysql.user');

  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('Host'));
  ResultSet.Close;
  PrStatement.Close;
end;

{**
  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestDbcMySQLBugReport.Test739444;
const
  items_Index = FirstDbcIndex;
  total_Index = FirstDbcIndex+1;
  average_Index = FirstDbcIndex+2;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  MetaData: IZResultSetMetaData;
begin
  if SkipForReason(srClosedBug) then Exit;

  {test statement}
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select count(*) as items, sum(c_weight) as total, '+
    ' AVG(c_width) as average from cargo');

  MetaData := ResultSet.GetMetadata;
  CheckEquals(3, MetaData.GetColumnCount);
  CheckEquals('items', MetaData.GetColumnLabel(items_Index));
  CheckEquals('total', MetaData.GetColumnLabel(total_Index));
  CheckEquals('average', MetaData.GetColumnLabel(average_Index));

  ResultSet.Next;
  CheckEquals(4, ResultSet.GetInt(items_Index));
  CheckEquals(8434, ResultSet.GetInt(total_Index));
  CheckEquals(8.5, ResultSet.GetFloat(average_Index), 0.01);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
Dublicate field names. Do not show properly fields name
if query have two alike field names and different tables.
}
procedure TZTestDbcMySQLBugReport.Test739448;
const
  table739448a_fld1_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  table739448a_fld2_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  table739448a_fld3_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  table739448b_fld1_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  table739448b_fld2_Index = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
  table739448b_fld3_Index = {$IFDEF GENERIC_INDEX}5{$ELSE}6{$ENDIF};
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  MetaData: IZResultSetMetaData;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select table739448a.fld1, table739448a.fld2, table739448a.fld3, '+
    ' table739448b.fld1, table739448b.fld2, table739448b.fld3 from table739448a, table739448b where '+
    ' table739448a.fld1 = table739448b.fld1 ');

  MetaData := ResultSet.GetMetadata;
  CheckEquals(6, MetaData.GetColumnCount);
  CheckEquals('fld1', MetaData.GetColumnLabel(table739448a_fld1_Index));
  CheckEquals('fld2', MetaData.GetColumnLabel(table739448a_fld2_Index));
  CheckEquals('fld3', MetaData.GetColumnLabel(table739448a_fld3_Index));
  CheckEquals('fld1_1', MetaData.GetColumnLabel(table739448b_fld1_Index));
  CheckEquals('fld2_1', MetaData.GetColumnLabel(table739448b_fld2_Index));
  CheckEquals('fld3_1', MetaData.GetColumnLabel(table739448b_fld3_Index));
  CheckEquals('fld1', MetaData.GetColumnName(table739448a_fld1_Index));
  CheckEquals('fld2', MetaData.GetColumnName(table739448a_fld2_Index));
  CheckEquals('fld3', MetaData.GetColumnName(table739448a_fld3_Index));
  CheckEquals('fld1', MetaData.GetColumnName(table739448b_fld1_Index));
  CheckEquals('fld2', MetaData.GetColumnName(table739448b_fld2_Index));
  CheckEquals('fld3', MetaData.GetColumnName(table739448b_fld3_Index));
  ResultSet.Close;
end;

{**
  Test the bug report #771576.

  unsigned int field problem.
}
{$IFDEF FPC}{$PUSH} {$WARN 5057 off : Local variable "BCD" does not seem to be initialized}{$ENDIF}
procedure TZTestDbcMySQLBugReport.Test768163;
const
  fld1_Index = FirstDbcIndex;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  BCD: TBCD;
begin
  if SkipForReason(srClosedBug) then Exit;

  {create statement}
  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  { clear table }
  Statement.Execute('delete from table768163');
  { insert data }
  ResultSet := Statement.ExecuteQuery('select * from table768163');
  with ResultSet do
  begin
    MoveToInsertRow;
    UpdateBigDecimal(fld1_Index, StrToBCD('2147483648'));
    InsertRow;
    Close;
  end;
  ResultSet := nil;
  { check inserted data }
  ResultSet := Statement.ExecuteQuery('select * from table768163');
  with ResultSet do
  begin
    Next;
    GetBigDecimal(fld1_Index, BCD);
    CheckEquals('2147483648', BCDToStr(BCD));
    Close;
  end;
    ResultSet := nil;
end;
{$IFDEF FPC}{$POP}{$ENDIF}

{**
  Test the bug report #816925.

  Problems with ZeosDBO 6.0.2, Delphi 6 and Infopower 3000
}
procedure TZTestDbcMySQLBugReport.Test816925;
const
  fld1_Index = FirstDbcIndex;
  fld2_Index = FirstDbcIndex+1;
  fld3_Index = FirstDbcIndex+2;
  fld4_Index = FirstDbcIndex+3;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3, fld4 from table816925');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_Index)));
  CheckEquals(Ord(stCurrency), Ord(Metadata.GetColumnType(fld2_Index)));
  CheckEquals(Ord(stLong), Ord(Metadata.GetColumnType(fld3_Index)));
  CheckEquals(Ord(stCurrency), Ord(Metadata.GetColumnType(fld4_Index)));

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3, fld4 from table816925');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_Index)));
  CheckEquals(Ord(stCurrency), Ord(Metadata.GetColumnType(fld2_Index)));
  CheckEquals(Ord(stLong), Ord(Metadata.GetColumnType(fld3_Index)));
  CheckEquals(Ord(stCurrency), Ord(Metadata.GetColumnType(fld4_Index)));
end;

{**
   Runs a test for bug report #881634
   Complex select statement returns wrong field types.
}
procedure TZTestDbcMySQLBugReport.Test881634;
const
  idt2_Index = FirstDbcIndex;
  ft2_Index = FirstDbcIndex+1;
  ft1_Index = FirstDbcIndex+2;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('SELECT idt2, ft2, table881634a.ft1'
    + ' FROM table881634b INNER JOIN table881634a'
    + ' ON (table881634b.ft1 = table881634a.idt1)');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(idt2_Index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(ft2_Index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(ft1_Index)));
end;

{**
  Runs a test for bug report #924861
  Memory leak, when client cannot connect to server
}
procedure TZTestDbcMySQLBugReport.Test924861;
begin
  if SkipForReason(srClosedBug) then Exit;

  try
    DriverManager.GetConnection('zdbc:mysql://xxx:1234557/db').Open;
    Fail('Incorrect processing of wrong connection URL.');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;
end;

{**
  Runs a test for bug report #961337
  ENUM('Y','N') is not recognized as Boolean when column name is renamed.

  EH: but this is not correct for all cases. loads users want to have it mapped
  as string. So since 2018 MySQL still not have a true bool type we'll
  map fieldtype bit(1) as Boolean which is the only type with just a 0/1
  switch. Keep hands far away from (un)signed tinyint(1) which has a range
  of shortint/byte
}
procedure TZTestDbcMySQLBugReport.Test961337;
const
  id_Index = FirstDbcIndex;
  fld1_Index = FirstDbcIndex + 1;
  fld2_Index = FirstDbcIndex + 2;
  fld3_Index = FirstDbcIndex + 3;
  fld4_Index = FirstDbcIndex + 4;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
  B: Boolean;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.Open;
  for B := (Connection as IZMySQLConnection).SupportsFieldTypeBit downto False do begin
    if not Connection.IsClosed then
      Connection.Close;
    Connection.GetParameters.Values[ConnProps_MySQL_FieldType_Bit_1_IsBoolean]:= ZSysUtils.BoolStrs[B];
    Statement := Connection.CreateStatement;
    try
      Statement.SetResultSetConcurrency(rcUpdatable);
      ResultSet := Statement.ExecuteQuery('SELECT id, fld1, fld2, fld1 as fld3,'
        + ' fld2 as fld4 FROM table735299');
      try
        Metadata := ResultSet.GetMetadata;
        CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(id_Index)));
        if not b then begin
          CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(fld1_Index)));
          CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(fld2_Index)));
          CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(fld3_Index)));
          CheckEquals(Ord(stBoolean), Ord(Metadata.GetColumnType(fld4_Index)));
        end else begin
          Check(Metadata.GetColumnType(fld1_Index) in [stString, stUnicodeString]);
          Check(Metadata.GetColumnType(fld2_Index) in [stString, stUnicodeString]);
          Check(Metadata.GetColumnType(fld3_Index) in [stString, stUnicodeString]);
          Check(Metadata.GetColumnType(fld4_Index) in [stString, stUnicodeString]);
        end;
      finally
        ResultSet.Close;
      end;
    finally
      Statement.Close;
    end;
  end;
end;

procedure TZTestDbcMySQLBugReport.TestBin_Collation;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  try
    ResultSet := Statement.ExecuteQuery('SELECT * FROM `mysql`.`user`');
    try
      Metadata := ResultSet.GetMetadata;
      Check(Metadata.GetColumnType(FirstDbcIndex) in [stString, stUnicodeString], 'Wrong fieldtype');
    finally
      ResultSet.Close;
    end;
  finally
    Statement.Close;
  end;
end;

{TZMySQLDatabaseInfo.GetDatabaseProductName always returns MySQL - even if the server is MariaDB.}
procedure TZTestDbcMySQLBugReport.TestTicket284;
var Stmt: IZStatement;
  S: String;
  function GetFork(S: String): TMySQLFork;
  var F: String;
  begin
    S := LowerCase(S);
    for Result := Low(TMySQLFork) to high(TMySQLFork) do begin
      F := LowerCase(MySQLForkName[Result]);
      if Pos(F, S) > 0 then
        Break;
    end;
  end;
begin
  Stmt := Connection.CreateStatement;
  CheckFalse(Connection.IsClosed);
  S := '';
  try
    with Stmt.ExecuteQuery('show variables like ''version''') do try
      Check(Next, 'there is a row');
      S := GetString(FirstDbcIndex+1);
    finally
      Close;
    end;
    S := S+' ';
    with Stmt.ExecuteQuery('show variables like ''version_comment''') do try
      Check(Next, 'there is a row');
      S := S + GetString(FirstDbcIndex+1);
    finally
      Close;
    end;
    CheckEquals(S, Connection.GetMetadata.GetDatabaseInfo.GetDatabaseProductVersion, 'The product version');
    CheckEquals(MySQLForkName[GetFork(S)], Connection.GetMetadata.GetDatabaseInfo.GetDatabaseProductName);
  finally
    Stmt.Close;
    Stmt := nil;
  end;
end;

(*
INSERT/UPDATE via TZSQLQuery - got error under Win7x64
I have two operators in .SQL.Text and I want to get AUTOINCREMENT valua for 'ter_id':
INSERT INTO territory ( 'ter_name' ) VALUES ( 'aaa' ); SELECT LAST_INSERT_ID() as ter_id;
ERROR: SIGSEGV .\src\dbc\ZDbcStatement.pas 1809 *)
procedure TZTestDbcMySQLBugReport.TestTicket365;
var
  I: Integer;
  Stmt: IZStatement;
  PStmt: IZPreparedStatement;
  RS: IZResultSet;

begin
  Connection.GetParameters.Add('CLIENT_MULTI_STATEMENTS=1');
  // prepare sample table
  Stmt := Connection.CreateStatement;
  try
    Stmt.ExecuteUpdate('create table TestTicket365 (id int not null auto_increment,int_value int,primary key (id))');
    RS := Stmt.ExecuteQuery('insert into TestTicket365 (int_value) values (0); SELECT last_insert_id() as id;');
    Check(RS <> nil, 'No resultset retrived');
    Check(RS.Next, 'No row retrieved');
    Check(not RS.IsNull(FirstDbcIndex), 'No data retrieved');
    RS.Close;
    Check(Stmt.ExecuteUpdate('insert into TestTicket365 (int_value) values (1); SELECT last_insert_id() as id;') = 1,'UpdateCount');
    Check(Stmt.GetMoreResults, 'There should be an pending result');
    RS := Stmt.GetResultSet;
    Check(RS <> nil, 'No resultset retrived');
    Check(RS.Next, 'No row retrieved');
    Check(not RS.IsNull(FirstDbcIndex), 'No data retrieved');
    I := RS.GetInt(FirstDbcIndex);
    RS.Close;
    PStmt := Connection.PrepareStatement('insert into TestTicket365 (int_value) values (?); SELECT last_insert_id() as id;');

    for I:= I to 10 do begin
      PStmt.SetInt(FirstDbcIndex, I);
      Check(PStmt.ExecuteUpdatePrepared = 1,'UpdateCount');
      Check(PStmt.GetMoreResults, 'There should be an pending result');
      RS := PStmt.GetResultSet;
      Check(RS <> nil, 'No resultset retrived');
      Check(RS.Next, 'No row retrieved');
      Check(not RS.IsNull(FirstDbcIndex), 'No data retrieved');
      RS.Close;
    end;
  finally
    if Assigned(PStmt) then begin
      PStmt.Close;
      PStmt:= nil;
    end;
    Stmt.ExecuteUpdate('drop table TestTicket365');
    Stmt.Close;
    Stmt := nil;
  end;
end;

procedure TZTestDbcMySQLBugReport.TestTicket442;
const
  INS_DATA: array[0..1] of array[0..1] of string = (
    ('1112690','Label1'),
    ('1112656','Label2')
  );
var
  i: Integer;
  pStmt: IZPreparedStatement;
begin
  Connection.ExecuteImmediat('delete from table_ticket_442', lcExecute);
  pStmt := Connection.PrepareStatement('INSERT INTO table_ticket_442 VALUES (?, ?)');
  (*Q.SQL.Text := 'DROP TABLE IF EXISTS table_ticket_442';
  Q.ExecSQL;

  Q.SQL.Text := 'CREATE TABLE `table_ticket_442` (' +
  '`objectid` int(11) NOT NULL, ' +
  '`label` varchar(255) NOT NULL, ' +
  'PRIMARY KEY (`objectid`)' +
  ') ENGINE=InnoDB DEFAULT CHARSET=latin1';
  Q.ExecSQL;

  Q.SQL.Text := 'INSERT INTO table_ticket_442 VALUES (:id, :label)';
  *)
  for i := 0 to High(INS_DATA) do begin
    pStmt.SetInt(FirstDbcIndex, StrToInt(INS_DATA[i][0]));
    pStmt.SetString(FirstDbcIndex+1, INS_DATA[i][1]);
    pStmt.ExecuteUpdatePrepared;
  end;
  try
    pStmt.SetInt(FirstDbcIndex, StrToInt(INS_DATA[0][0]));
    pStmt.SetString(FirstDbcIndex+1, 'bar');
    pStmt.ExecuteUpdatePrepared;
    Fail('(?) 1112690 not in INS_DATA');
  except
    on E: Exception do
      CheckNotTestFailure(E, 'Expected behavior');
  end;
  pStmt.SetInt(FirstDbcIndex, StrToInt(INS_DATA[1][0])+1);
  pStmt.SetString(FirstDbcIndex+1, 'foot');
  pStmt.ExecuteUpdatePrepared;
end;

initialization
  RegisterTest('bugreport',TZTestDbcMySQLBugReport.Suite);
{$ENDIF ZEOS_DISABLE_MYSQL}
end.
