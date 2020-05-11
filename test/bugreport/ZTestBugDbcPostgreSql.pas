{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Cases for PostgreSql DBC Bug Reports       }
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

unit ZTestBugDbcPostgreSql;

interface

{$I ZBugReport.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
uses
  Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs,
  ZSqlTestCase, ZCompatibility, ZDbcPostgreSql, ZTestConsts, ZDbcProperties;

type

  {** Implements a DBC bug report test case for PostgreSQL. }
  TZTestDbcPostgreSQLBugReport = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test702361;
    procedure Test702365;
    procedure Test702368;
    procedure Test727385;
    procedure Test739444;
    procedure Test759184;
    procedure Test798336;
    procedure Test815852;
    procedure Test815854;
    procedure Test824786;
    procedure Test815861;
    procedure Test933623;
    procedure Test1014416;
    procedure Test_Mantis0000148;
    procedure Test_Mantis0000229;
    procedure Test_TrailingSpaces;
  end;

  TZTestDbcPostgreSQLBugReportMBCs = class(TZAbstractDbcSQLTestCaseMBCs)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test739514;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses SysUtils, ZSysUtils, ZTestCase, ZDbcPostgreSqlUtils, ZEncoding;

{ TZTestDbcPostgreSQLBugReport }

function TZTestDbcPostgreSQLBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

{**
  PostgreSQL - inv_open: large object
  Servet: PostgreSQL 7.3.1(NT/Cygwin),
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select * from pg_class

  Error:
  I receive msgbox error "inv_open: large object ..."
  when column in result set is OID datatype.
}
procedure TZTestDbcPostgreSQLBugReport.Test702361;
var
  Connection: IZConnection;
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Url: TZURL;
begin
  if SkipForReason(srClosedBug) then Exit;

  Url := GetConnectionUrl(DSProps_OidAsBlob + '=' + StrTrue);
  Connection := DriverManager.GetConnection(Url.URL);
  Url.Free;
  //Connection := DriverManager.GetConnectionWithLogin(
    //GetConnectionUrl + '?oidasblob=true', UserName, Password);
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select reltype from pg_class');
  CheckEquals(Ord(stBinaryStream), Ord(ResultSet.GetMetadata.GetColumnType(FirstDbcIndex)));
  ResultSet.Close;
  Statement.Close;

  Url := GetConnectionUrl(DSProps_OidAsBlob + '=' + StrFalse);
  Connection := DriverManager.GetConnection(Url.URL);
  Url.Free;
//  Connection := DriverManager.GetConnectionWithLogin(
  //  GetConnectionUrl + '?oidasblob=false', UserName, Password);
  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select reltype from pg_class');
  CheckEquals(Ord(stLongWord), Ord(ResultSet.GetMetadata.GetColumnType(FirstDbcIndex)));
  ResultSet.Close;
  Statement.Close;
end;

{**
  PostgreSQL - empty columns
  Server: PostgreSQL 7.3.1(NT/Cygwin)
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select contype from pg_constraint

  Error:
  I receive empty value in contype column, but this
  column have a value example.
}
procedure TZTestDbcPostgreSQLBugReport.Test702365;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select contype from pg_constraint');
  while ResultSet.Next do
    CheckNotEquals('', ResultSet.GetStringByName('contype'));
  ResultSet.Close;
  Statement.Close;
end;

{**
  PostgreSQL - bytea datatype
  Server: PostgreSQL 7.3.1(NT/Cygwin)
  Environment: Delphi6
  Components: ZeosDBO 6.0.4
  Sql: select probin from pg_proc

  Error:
  "... wrong size ..." when column in bytea datatype
}
procedure TZTestDbcPostgreSQLBugReport.Test702368;
{var
  ResultSet: IZResultSet;
  Statement: IZStatement;}
begin
  if SkipForReason(srClosedBug) then Exit;

  {Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select probin from pg_proc');
  CheckEquals(Ord(stBinaryStream), Ord(ResultSet.GetMetadata.GetColumnType(1)));
  while ResultSet.Next do
  begin
    CheckNotEquals(0, Length(ResultSet.GetString(1)));
    CheckNotEquals(0, Length(ResultSet.GetBytes(1)));
    CheckEquals(ResultSet.GetString(1), BytesToStr(ResultSet.GetBytes(1)));
  end;
  ResultSet.Close;
  Statement.Close;}
end;

{
  Server: PostgreSql 7.3.2
  Components: ZeosDBO 6.0.8
  The problem is in the

  Error:
  TZRowAccessor.GetBlobObject
  NullPtr^ resolves to zero, so Result is assigned to
  BlobPtr.
}
procedure TZTestDbcPostgreSQLBugReport.Test727385;
var
  Sql: string;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  StrStream, BinStream: TMemoryStream;
  StrStream1, BinStream1: TStream;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  CheckNotNull(Statement);
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  Sql := 'DELETE FROM people where p_id = ' + IntToStr(TEST_ROW_ID);
  Statement.ExecuteUpdate(Sql);

  StrStream := TMemoryStream.Create;
  StrStream.LoadFromFile(TestFilePath('text/lgpl.txt'));
  BinStream := TMemoryStream.Create;
  BinStream.LoadFromFile(TestFilePath('images/dogs.jpg'));

  StrStream1 := nil;
  BinStream1 := nil;

  try
    Sql := 'SELECT p_id, p_resume, p_picture FROM people where p_id = '
      + IntToStr(TEST_ROW_ID);
    { Inserts test record to equipment }
    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      MoveToInsertRow;
      UpdateIntByName('p_id', TEST_ROW_ID);
      UpdateAsciiStreamByName('p_resume', StrStream);
      UpdateBinaryStreamByName('p_picture', BinStream);
      InsertRow;
      Close;
    end;
    ResultSet := nil;

    ResultSet := Statement.ExecuteQuery(Sql);
    CheckNotNull(ResultSet);
    with ResultSet do
    begin
      Check(Next);
      CheckEquals(TEST_ROW_ID, GetIntByName('p_id'));
      BinStream1 := GetBinaryStreamByName('p_picture');
      try
        CheckEquals(BinStream, BinStream1);
      finally
        FreeAndNil(BinStream1);
      end;
      StrStream1 := GetAsciiStreamByName('p_resume');
      try
        CheckEquals(StrStream, StrStream1);
      finally
        FreeAndNil(StrStream1);
      end;
      DeleteRow;
      Close;
    end;
  finally
    FreeAndNil(BinStream);
    FreeAndNil(StrStream);
  end;
end;

{**
  Aliases for fields do not work. Result Set after
  execution SQL query do not contain the aliased fields.
}
procedure TZTestDbcPostgreSQLBugReport.Test739444;
const
  items_index = FirstDbcIndex;
  total_index = FirstDbcIndex+1;
  average_index = FirstDbcIndex+2;
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
  CheckEquals('items', MetaData.GetColumnLabel(items_index));
  CheckEquals('total', MetaData.GetColumnLabel(total_index));
  CheckEquals('average', MetaData.GetColumnLabel(average_index));

  ResultSet.Next;
  CheckEquals(4, ResultSet.GetInt(items_index));
  CheckEquals(8434, ResultSet.GetInt(total_index));
  CheckEquals(8.5, ResultSet.GetFloat(average_index), 0.01);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
  Test the bug report #759184.

  Empty fields in string concatination expression.
}
procedure TZTestDbcPostgreSQLBugReport.Test759184;
const
  expr_index = FirstDbcIndex;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select p_id || p_name as expr from people where p_id=1');

  with ResultSet do
  begin
    Next;
    CheckEquals('expr', GetMetadata.GetColumnLabel(expr_index));
    CheckEquals('1Vasia Pupkin', GetString(expr_index));
  end;
end;

{**
  Test the bug report #798336.

  Not passing large objects to Postgres DB.
}
procedure TZTestDbcPostgreSQLBugReport.Test798336;
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
  if SkipForReason(srClosedBug) then Exit;

  Url := GetConnectionUrl(DSProps_OidAsBlob + '=' + StrTrue);
  Connection := DriverManager.GetConnection(Url.URL);
  Url.Free;
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
  ImageStream.LoadFromFile(TestFilePath('images/horse.jpg'));

  try
    PreparedStatement := Connection.PrepareStatement(
      'INSERT INTO blob_values (b_id,b_text,b_image) VALUES(?,?,?)');
    PreparedStatement.SetInt(b_id_index, TEST_ROW_ID);
    PreparedStatement.SetAsciiStream(b_text_index, TextStream);
    PreparedStatement.SetBinaryStream(b_image_index, ImageStream);
    CheckEquals(1, PreparedStatement.ExecuteUpdatePrepared);

    ResultSet := Statement.ExecuteQuery('SELECT * FROM blob_values'
      + ' WHERE b_id=' + IntToStr(TEST_ROW_ID));
    CheckNotNull(ResultSet);
    Check(ResultSet.Next);
    CheckEquals(TEST_ROW_ID, ResultSet.GetIntByName('b_id'));

    TempStream := ResultSet.GetAsciiStreamByName('b_text');
    CheckEquals(TextStream, TempStream);
    TempStream.Free;

    TempStream := ResultSet.GetBinaryStreamByName('b_image');
    CheckEquals(ImageStream, TempStream);
    TempStream.Free;

    ResultSet.Close;
  finally
    TextStream.Free;
    ImageStream.Free;

    Statement.Close;
    Connection.Close;
  end;
end;

{**
  Test the bug report #815852.

  Metadata Query does not support Domains.
}
procedure TZTestDbcPostgreSQLBugReport.Test815852;
const
  fld1_index = FirstDbcIndex;
  fld2_index = FirstDbcIndex+1;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852 ');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));

  Statement.ExecuteUpdate('delete from test815852');

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(fld1_index, 123456);
  ResultSet.UpdateString(fld2_index, 'abcdef');
  ResultSet.InsertRow;

  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from test815852');
  Check(ResultSet.Next);
  CheckEquals(123456, ResultSet.GetInt(fld1_index));
  CheckEquals('abcdef', ResultSet.GetString(fld2_index));

  Statement.ExecuteUpdate('delete from test815852');
end;

{**
  Test the bug report #815854.

  Problem with support for schemas.
}
procedure TZTestDbcPostgreSQLBugReport.Test815854;
const
  fld1_index = FirstDbcIndex;
  fld2_index = FirstDbcIndex+1;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780 ');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));

  Statement.ExecuteUpdate('delete from xyz.test824780');

  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(fld1_index)));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));

  ResultSet.MoveToInsertRow;
  ResultSet.UpdateInt(fld1_index, 123456);
  ResultSet.UpdateString(fld2_index, 'abcdef');
  ResultSet.InsertRow;

  ResultSet := Statement.ExecuteQuery('select fld1, fld2 from xyz.test824780');
  Check(ResultSet.Next);
  CheckEquals(123456, ResultSet.GetInt(fld1_index));
  CheckEquals('abcdef', ResultSet.GetString(fld2_index));

  Statement.ExecuteUpdate('delete from xyz.test824780');
end;

{**
  Test the bug report #824786.
  TZMetadata shows PostgreSQL system tables.
}
procedure TZTestDbcPostgreSQLBugReport.Test824786;
var
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Metadata := Connection.GetMetadata;
  ResultSet := Metadata.GetTables('', '', '', nil);

  while ResultSet.Next do
  begin
    if StartsWith(ResultSet.GetStringByName('TABLE_NAME'), 'sql_') then
      CheckEquals('SYSTEM TABLE', ResultSet.GetStringByName('TABLE_TYPE'));
  end;
end;

{**
  Test the bug report #815861
  Problem is incorrect parsing of the Version#
}
procedure TZTestDbcPostgreSQLBugReport.Test815861;
const
  MinorVersion1: string = '4beta2';
  MinorVersion2: string = 'tst4beta2';
  MinorVersion3: string = '123beta2';
begin
  if SkipForReason(srClosedBug) then Exit;

  CheckEquals(4, GetMinorVersion(MinorVersion1));
  CheckEquals(0, GetMinorVersion(MinorVersion2));
  CheckEquals(123, GetMinorVersion(MinorVersion3));
end;

{**
  Test the bug report #933623.
  Command is aborten until the next of transaction block.
}
procedure TZTestDbcPostgreSQLBugReport.Test933623;
var
  Statement: IZStatement;
begin
  if SkipForReason(srClosedBug) then Exit;

  Connection.SetAutoCommit(True);
  Connection.SetTransactionIsolation(tiReadCommitted);
  Statement := Connection.CreateStatement;
  try
    Statement.ExecuteQuery('select * from people where xp_id=1');
    Fail('Incorrect syntax error processing');
  except on E: Exception do
    CheckNotTestFailure(E);
  end;

  Statement.ExecuteQuery('select * from people where p_id=1');
end;

{**
  Test the bug report #1014416
  Problem is incorrect parsing of the Version#
}
procedure TZTestDbcPostgreSQLBugReport.Test1014416;
const
  fld1_index = FirstDbcIndex;
  fld2_index = FirstDbcIndex+1;
  fld3_index = FirstDbcIndex+2;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
  Metadata: IZResultSetMetadata;
begin
  if SkipForReason(srClosedBug) then Exit;

  with (Connection as IZPostgreSQLConnection) do
  begin
    if (GetServerMajorVersion < 7 ) or
       ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
      Exit;
    if (Protocol = 'postgresql-6.5') or
      (Protocol = 'postgresql-7.2') then
      Exit;
  end;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetConcurrency(rcReadOnly);
  ResultSet := Statement.ExecuteQuery('select fld1, fld2, fld3 from test1014416');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));
  CheckEquals(100, Metadata.GetPrecision(fld1_index));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld2_index)));
  CheckEquals(100, Metadata.GetPrecision(fld2_index));
  CheckEquals(Ord(stString), Ord(Metadata.GetColumnType(fld3_index)));
  CheckEquals(17, Metadata.GetPrecision(fld3_index));

  Check(ResultSet.Next);
  CheckEquals('192.168.100.128/25', ResultSet.GetString(fld1_index));
  CheckEquals('192.168.100.128/25', ResultSet.GetString(fld2_index));
  CheckEquals('08:00:2b:01:02:03', ResultSet.GetString(fld3_index));

  Check(ResultSet.Next);
  CheckEquals('2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128', ResultSet.GetString(fld1_index));
  CheckEquals('2001:4f8:3:ba:2e0:81ff:fe22:d1f1', ResultSet.GetString(fld2_index));
  CheckEquals('08:00:2b:01:02:03', ResultSet.GetString(fld3_index));

  Check(not ResultSet.Next);
  Statement.Close;
end;

{ http://zeosbugs.firmos.at/view.php?id=148
	0000148: Access violation in TZRowAccessor.GetBlob
can't open table pg_class
}
procedure TZTestDbcPostgreSQLBugReport.Test_Mantis0000148;
const
  relacl_index = FirstDbcIndex;
var
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
//??  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  ResultSet := Statement.ExecuteQuery('select relacl from pg_class;');
  ///
  ResultSet.Next;
  ResultSet.GetBlob(relacl_index);
  Statement.Close;
end;

{**
0000229: postgresql varchar is badly interpreted
  In postgresql, varchar with no precision is equal to text (blob) type.
  In zeos, varchar is treated as stString with default precision 255.
  It means when we try to read data that are longer than 255 then they are
  automatically truncated.
}
procedure TZTestDbcPostgreSQLBugReport.Test_Mantis0000229;
var
  ResultSet: IZResultSet;
begin
  with Connection.PrepareStatement('select * from Mantis229') do
  begin
    ResultSet := ExecuteQueryPrepared;
    CheckEquals(Ord(stAsciiStream), Ord(ResultSet.GetMetadata.GetColumnType(FirstDbcIndex)));
    ResultSet := nil;
    Close;
  end;
end;

procedure TZTestDbcPostgreSQLBugReport.Test_TrailingSpaces;
var
  ResultSet: IZResultSet;
begin
  with Connection.PrepareStatement('select s_char from string_values') do
  begin
    ResultSet := ExecuteQueryPrepared;
    CheckEquals(Ord(stString), Ord(ResultSet.GetMetadata.GetColumnType(FirstDbcIndex)));
    ResultSet.Next;
    CheckEquals('', ResultSet.GetString(FirstDbcIndex));
    ResultSet.Next;
    CheckEquals('Test string', ResultSet.GetString(FirstDbcIndex));

    ResultSet := nil;
    Close;
  end;
end;

function TZTestDbcPostgreSQLBugReportMBCs.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

procedure TZTestDbcPostgreSQLBugReportMBCs.Test739514;
const
  id_index = FirstDbcIndex;
  fld_index = FirstDbcIndex+1;
  {$IFDEF UNICODE} //D7 wrongly assignes the cyrill chars ):
  Str1: UnicodeString = #$0410#$0431#$0440#$0430#$043a#$0430#$0434#$0430#$0431#$0440#$0430 {'Абракадабра'}; // Abrakadabra in Cyrillic letters
  Str2: UnicodeString = '\'#$041f#$043e#$0431#$0435#$0434#$0430'\' {'\Победа\'}; // victory / success in russian (according to leo.org)
  {$ELSE}
  Words1: array[0..10] of Word = ($0410,$0431,$0440,$0430,$043a,$0430,$0434,$0430,$0431,$0440,$0430);
  Words2: array[0.. 7] of Word = (92,$041f,$043e,$0431,$0435,$0434,$0430,92);
  {$ENDIF}
var
  {$IFNDEF UNICODE}
  Str1: UnicodeString;
  Str2: UnicodeString;
  {$ENDIF}
  CP: Word;
  ResultSet: IZResultSet;
  Statement: IZStatement;
begin
  Connection.Open;
  {$IFNDEF UNICODE}
  System.SetString(Str1, PWideChar(@Words1[0]), 11);
  System.SetString(Str2, PWideChar(@Words2[0]), 8);
  {$ENDIF}
  Check(not Connection.IsClosed, 'Connected'); //for FPC which marks tests as failed if not executed
  CP := connection.GetConSettings.ClientCodePage.CP;
  //eh the russion abrakadabra can no be mapped to other charsets then:
  if not ((CP = zCP_UTF8) or (CP = zCP_WIN1251) or (CP = zcp_DOS855) or (CP = zCP_KOI8R))
    {add some more if you run into same issue !!} then
    Exit;

  Statement := Connection.CreateStatement;
  Statement.ExecuteUpdate('delete from test739514 where id<>1');

  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);
  ResultSet := Statement.ExecuteQuery('select id, fld from test739514');
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, ResultSet.GetInt(id_index));
    CheckEquals(Str1, ResultSet.{$IFDEF UNICODE}GetString{$ELSE}GetUnicodeString{$ENDIF}(fld_index));
    MoveToInsertRow;
    UpdateIntByName('id', 2);

    UpdateUnicodeStringByName('fld', Str2);
    InsertRow;
    Close;
  end;

  ResultSet := Statement.ExecuteQuery('select id, fld from test739514 order by id');
  with ResultSet do
  begin
    Check(Next);
    CheckEquals(1, ResultSet.GetInt(id_index));
    CheckEquals(Str1, ResultSet.{$IFDEF UNICODE}GetString{$ELSE}GetUnicodeString{$ENDIF}(fld_index));

    Check(Next);
    CheckEquals(2, ResultSet.GetInt(id_index));
    CheckEquals(Str2, ResultSet.GetUnicodeString(fld_index));
    Close;
  end;

  Statement.ExecuteUpdate('delete from test739514 where id<>1');
  Statement.Close;
end;

initialization
  RegisterTest('bugreport',TZTestDbcPostgreSQLBugReport.Suite);
  RegisterTest('bugreport',TZTestDbcPostgreSQLBugReportMBCs.Suite);
{$ENDIF ZEOS_DISABLE_POSTGRESQL}
end.
