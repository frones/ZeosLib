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

unit ZTestBugCompOracle;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, DB, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZDbcIntfs, ZSqlTestCase,
  {$IFNDEF LINUX}
    {$IFDEF WITH_VCL_PREFIX}
    Vcl.DBCtrls,
    {$ELSE}
    DBCtrls,
    {$ENDIF}
  {$ENDIF}
  ZCompatibility;
type

  {** Implements a bug report test case for Oracle components. }
  ZTestCompOracleBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestNum1;
    procedure TestNestedDataSetFields1;
    procedure TestNestedDataSetFields2;
    procedure TestNCLOBValues;
    procedure TestTicket96;
    procedure TestOutParam1;
    procedure TestOutParam2;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZTestCase, ZSysUtils, ZEncoding;

{ ZTestCompOracleBugReport }

function ZTestCompOracleBugReport.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

{**
  NUMBER must be froat
}
procedure ZTestCompOracleBugReport.TestNum1;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM Table_Num1';
    Query.Open;
    CheckEquals(Ord(ftInteger), Ord(Query.Fields[0].DataType), 'id field type');
    CheckEquals(Ord(ftFmtBCD), Ord(Query.Fields[1].DataType), 'Num field type');
    CheckEquals(1, Query.Fields[0].AsInteger, 'id value');
    CheckEquals(54321.0123456789, Query.Fields[1].AsFloat, 1E-11, 'Num value');
  finally
    Query.Free;
  end;
end;

(* http://zeoslib.sourceforge.net/viewtopic.php?f=37&p=99195#p99195
*)
procedure ZTestCompOracleBugReport.TestOutParam1;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'begin :result ::= 42; end;';
    CheckEquals(1, Query.Params.Count, 'Param count');
    Query.Params[0].ParamType := ptOutPut;
    Query.Params[0].DataType := ftInteger;
    Query.ExecSQL;
    CheckEquals(42, Query.ParamByName('result').AsInteger, 'The OutParam-Result of a anonymous begin ... end block');
  finally
    Query.Free;
  end;
end;

(* http://zeoslib.sourceforge.net/viewtopic.php?f=37&p=99195#p99195
*)
procedure ZTestCompOracleBugReport.TestOutParam2;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.ParamCheck := False;
    Query.SQL.Text := 'begin :result := 42; end;';
    Query.Params.CreateParam(ftInteger, 'result', ptOutPut);
    Query.ExecSQL;
    CheckEquals(42, Query.ParamByName('result').AsInteger, 'The OutParam-Result of a anonymous begin ... end block');
  finally
    Query.Free;
  end;
end;

procedure ZTestCompOracleBugReport.TestNestedDataSetFields1;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM SYSTEM.AQ$_QUEUES';
    Query.Open;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompOracleBugReport.TestNestedDataSetFields2;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  try
    Query.SQL.Text := 'SELECT * FROM customers';
    Query.Open;
  finally
    Query.Free;
  end;
end;

procedure ZTestCompOracleBugReport.TestNCLOBValues;
const
  row_id = 1000;
  testString: ZWideString = #$0410#$0431#$0440#$0430#$043a#$0430#$0434#$0430#$0431#$0440#$0430; // Abrakadabra in Cyrillic letters
var
  Query: TZQuery;
  BinFileStream: TFileStream;
  BinaryStream: TStream;
  Dir: String;
  ConSettings: PZConSettings;
  CP: Word;
  function GetBFILEDir: String;
  var I: Integer;
  begin
    Result := '';
    for i := 0 to high(Properties) do
      if StartsWith(Properties[i], 'BFILE_DIR') then
        Result := Copy(Properties[i], Pos('=', Properties[i])+1, Length(Properties[i]));
  end;

begin
  if SkipForReason(srClosedBug) then Exit;

  Dir := '';
  Query := CreateQuery;
  BinaryStream := TMemoryStream.Create;
  BinFileStream := nil;
  try
    BinFileStream := TFileStream.Create(TestFilePath('images/horse.jpg'), fmOpenRead);
    Query.SQL.Text := 'select * from blob_values'; //NCLOB and BFILE is inlcuded
    Query.Open;
    ConSettings := Connection.DbcConnection.GetConSettings;
    CheckEquals(6, Query.Fields.Count);
    CheckEquals('', Query.Fields[2].AsString);
    CheckMemoFieldType(Query.Fields[2].DataType, ConSettings);
    CheckMemoFieldType(Query.Fields[3].DataType, ConSettings);
    Query.Next;
    CheckEquals('Test string', Query.Fields[2].AsString); //read ORA NCLOB
    Query.Next;
    Query.Insert;
    Query.FieldByName('b_id').AsInteger := row_id;
    Query.FieldByName('b_long').AsString := 'aaa';
    Query.FieldByName('b_nclob').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := testString+testString;
    Query.FieldByName('b_clob').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF} := testString+testString+testString;
    (Query.FieldByName('b_blob') as TBlobField).LoadFromStream(BinFileStream);
    Query.Post;
    Dir := GetBFILEDir;
    if not ( Dir = '') then
    begin
      Query.SQL.Text := 'CREATE OR REPLACE DIRECTORY IMG_DIR AS '''+Dir+'''';
      Query.ExecSQL;
      Query.SQL.Text := 'update blob_values set b_bfile = BFILENAME(''IMG_DIR'', ''horse.jpg'') where b_id = '+IntToStr(row_id);
      Query.ExecSQL;
      Query.SQL.Text := 'select * from blob_values where b_id = '+IntToStr(row_id);
      Query.Open;
      CheckEquals(6, Query.Fields.Count);
      CheckEquals('aaa', Query.FieldByName('b_long').AsString, 'value of b_long field');

      if ConSettings.CPType = cGET_ACP {no unicode strings or utf8 allowed}
      then CP := ZOSCodePage
      else CP := connection.DbcConnection.GetConSettings.ClientCodePage.CP;
      //eh the russion abrakadabra can no be mapped to other charsets then:
      if not ((CP = zCP_UTF8) or (CP = zCP_WIN1251) or (CP = zcp_DOS855) or (CP = zCP_KOI8R) or (CP = zCP_UTF16))
        {add some more if you run into same issue !!} then begin
        BlankCheck;
      end else begin
        CheckEquals(teststring+teststring, Query.FieldByName('b_nclob').AsString, ConSettings, 'value of b_nclob field');
        CheckEquals(teststring+teststring+teststring, Query.FieldByName('b_clob').AsString, ConSettings, 'value of b_clob field');
      end;

      (Query.FieldByName('b_blob') as TBlobField).SaveToStream(BinaryStream);
      CheckEquals(BinFileStream, BinaryStream, 'b_blob');
      BinaryStream.Position := 0;
      (Query.FieldByName('b_bfile') as TBlobField).SaveToStream(BinaryStream);
      CheckEquals(BinFileStream, BinaryStream, 'b_bfile');
    end;
    Query.Close;
  finally
    Query.SQL.Text := 'delete from blob_values where b_id ='+IntToStr(row_id);
    Query.ExecSQL;
    if not ( Dir = '') then
    begin
      Query.SQL.Text := 'DROP DIRECTORY IMG_DIR';
      try
        Query.ExecSQL;
      except
      end;
    end;
    FreeAndNil(Query);
    FreeAndNil(BinFileStream);
    FreeAndNil(BinaryStream);
  end;
end;

procedure ZTestCompOracleBugReport.TestTicket96;
var
  Query: TZQuery;
begin
  if SkipForReason(srClosedBug) then Exit;
  Query := CreateQuery;
  try
    Query.ParamCheck := True;
    Query.SQL.Text := 'begin ';
    Query.SQL.Add('  P_TICKET96.Update_Doc(:Param1);');
    Query.SQL.Add('end;');
    Query.ParamByName('Param1').AsInteger := 1;
    Query.ExecSQL;

    Query.SQL.Text := 'begin ';
    Query.SQL.Add('  P_TICKET96.Update_Doc(:Param1, :Param2, :Param3);');
    Query.SQL.Add('end;');
    Query.ParamByName('Param1').AsInteger := 1;
    Query.ParamByName('Param2').AsInteger := 2;
    Query.ParamByName('Param3').AsInteger := 3;
    Query.ExecSQL;

    Query.SQL.Text := 'begin ';
    Query.SQL.Add('  P_TICKET96.Update_Doc(:Param1);');
    Query.SQL.Add('end;');
    Query.ParamByName('Param1').AsInteger := 1;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport', ZTestCompOracleBugReport.Suite);
end.
