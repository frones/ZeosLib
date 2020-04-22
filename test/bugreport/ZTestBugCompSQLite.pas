{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Cases for Interbase Component Bug Reports    }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZTestBugCompSQLite;

interface

{$I ZBugReport.inc}

uses
  {$IF not defined(FPC) and defined(MSWINDOWS)}Windows,{$IFEND}//inlineWarning
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDataset, ZDbcIntfs, ZSqlTestCase,
  {$IFNDEF LINUX}
    {$IFDEF WITH_VCL_PREFIX}
    Vcl.DBCtrls,
    {$ELSE}
    DBCtrls,
    {$ENDIF}
  {$ENDIF}
  ZCompatibility, ZEncoding, ZDbcProperties;
type

  {** Implements a bug report test case for SQLite components. }
  ZTestCompSQLiteBugReport = class(TZAbstractCompSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestUndefined_Varchar_AsString_Length;
    procedure TestCompTicket386;
    procedure TestAttachAndDetach;
    procedure TestTicket405;
  end;

  {** Implements a MBC bug report test case for SQLite components. }
  ZTestCompSQLiteBugReportMBCs = class(TZAbstractCompSQLTestCaseMBCs)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Mantis248_TestNonASCIICharSelect;
  end;

implementation

uses
  Variants, DB, ZDatasetUtils, ZSqlProcessor;

{ ZTestCompSQLiteBugReport }

function ZTestCompSQLiteBugReport.GetSupportedProtocols: string;
begin
  Result := pl_all_sqlite;
end;

{ https://zeoslib.sourceforge.io/viewtopic.php?f=40&t=117755
I just migrated from ZeosDbo 7.0.6-stable to 7.2.6-stable.
My program uses SQlite3 and attaches and detaches the bases with:
TZConnection.ExecuteDirect ('Attach database "myBasefile.db3" as XXX ’)
TZConnection.ExecuteDirect ('Detach database XXX')

This has been working well for several years with ZeosDb 7.0 but with 7.2 I have the SQLITE_ERROR error when the line TZConnection.ExecuteDirect ('Detach database XXX') is executed.
Do you have an idea to solve this problem?

I noticed in ZeosDbo sources that in V7.2 it was the sqlite3_step command while in V7.0 it was sqlite3_exec.
sqlite3_exec is used in TZSQLiteBaseDriver.Execute but this function is not used afterwards and is not accessible from TZConnection now.
}
procedure ZTestCompSQLiteBugReport.TestAttachAndDetach;
var con2: IZConnection;
  URL: TZURL;
  DBName: String;
begin
  Connection.Connect;
  Check(Connection.Connected);
  URL := TZURL.Create;
  URL.URL := Connection.DbcConnection.GetURL;
  DBName := ExtractFilePAth(ParamStr(0))+'TestAttachAndDetach.db';
  URL.DataBase := 'TestAttachAndDetach.db';
  Con2 := nil;
  try
    Con2 := DriverManager.GetConnection(URL.URL); //Create new dbfile;
    Con2.Open;
    Con2.Close;
    Con2 := nil;
    Connection.ExecuteDirect('ATTACH '+QuotedStr(DBName)+' AS ''TEST''');
    Check(Connection.Connected);
    Connection.ExecuteDirect('DETACH DATABASE ''TEST''');
  finally
    FreeAndNil(URL);
    if Con2 <> nil then
      Con2.Close;
    Con2 := nil;
    if FileExists(DBName) then
      DeleteFile(DBName);
  end;
end;

procedure ZTestCompSQLiteBugReport.TestCompTicket386;
var SL: TStrings;
begin
  SL := TStringList.Create;
  try
    Connection.Connect;
    Check(Connection.Connected, 'Could not open a connection');
    Connection.GetColumnNames('table_ticket_386', '', SL);
    CheckEquals(3, SL.Count, 'the fieldcount of table table_ticket_386');
    Connection.GetColumnNames('table_ticket_386', 'id', SL);
    CheckEquals(1, SL.Count, 'the fieldcount of table table_ticket_386 and id');
    Connection.GetColumnNames('table_ticket_386', '%', SL);
    CheckEquals(3, SL.Count, 'the fieldcount of table table_ticket_386 and all fields matching %');
    Connection.GetColumnNames('table_ticket_386', 'Field_1', SL);
    CheckEquals(1, SL.Count, 'the fieldcount of table table_ticket_386 and Field_1');
    CheckEquals('Field_1', SL[0], 'the column_patternmatch of Field_1');
    Connection.GetColumnNames('table_ticket_386', 'Field_2', SL);
    CheckEquals(1, SL.Count, 'the fieldcount of table table_ticket_386 and Field_2');
    CheckEquals('Field_2', SL[0], 'the column_patternmatch of Field_2');
    Connection.GetColumnNames('table_ticket_386', 'Field%', SL);
    CheckEquals(2, SL.Count, 'the fieldcount of table_ticket_386 for columnpattern Field%');
  finally
    Sl.Free;
  end;
end;

{ See: https://sourceforge.net/p/zeoslib/tickets/405/
  when executing a certain SQL script and committing the work,
  Zeos will get an errror in SQLite when starting the next transaction. }
procedure ZTestCompSQLiteBugReport.TestTicket405;
var SQLProcessor: TZSQLProcessor;
begin
  SQLProcessor := TZSQLProcessor.Create(nil);
  SQLProcessor.Connection := Connection;
  SQLProcessor.Delimiter := ';';
  Connection.Connect;
  Check(Connection.Connected);
  try
    SQLProcessor.Script.Text :=
      'CREATE  TABLE IF NOT EXISTS `station` ('+LineEnding+
      '  `sindex` INTEGER PRIMARY KEY,'+LineEnding+
      '  `station_name` VARCHAR(50) NOT NULL,'+LineEnding+
      '  `station_code` VARCHAR(50) UNIQUE NULL,'+LineEnding+
      '  `sea_height` INT,'+LineEnding+
      '  `time_shift` INT'+LineEnding+
      ');'+LineEnding+
      'CREATE TEMPORARY TABLE IF NOT EXISTS `bufer` ('+LineEnding+
      '  `rowid` INTEGER PRIMARY KEY AUTOINCREMENT,'+LineEnding+
      '  `station` INT NOT NULL,'+LineEnding+
      '  `place` INT,'+LineEnding+
      '  `lat` REAL,'+LineEnding+
      '  `lon` REAL'+LineEnding+
      ');';
    // start transaction
    Connection.StartTransaction;
    // execute script
    SQLProcessor.Execute;
    // commit
    Connection.Commit; //thows an exception ?
    Check(Connection.Connected);
  finally
    SQLProcessor.Free;
    Connection.ExecuteDirect('DROP TABLE IF EXISTS `station`')
  end;
end;

procedure ZTestCompSQLiteBugReport.TestUndefined_Varchar_AsString_Length;
var
  Query: TZQuery;
begin
  Query := CreateQuery;
  try
    Query.Properties.Values[DSProps_UndefVarcharAsStringLength] := '255';
    Query.SQL.Text := 'select p_name ||'',''|| p_name from people';
    Query.Open;
    CheckEquals(1, Query.FieldCount);
    CheckStringFieldType(Query.Fields[0].DataType, Connection.ControlsCodePage);
    CheckEquals('Vasia Pupkin,Vasia Pupkin', Query.Fields[0].AsString, 'The SQLite concat');
    Query.Next;
    CheckEquals('Andy Karto,Andy Karto', Query.Fields[0].AsString, 'The SQLite concat');
    Query.Close;
  finally
    Query.Free;
  end;
end;

{ ZTestCompSQLiteBugReportMBCs }
const
  // some dull text in Russian
  Str2: ZWideString =
    #$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430#$0438+
    #$0431#$043E#$043B#$0435#$0435#$0020#$0442#$0440#$0438#$0432#$0438#$0430+
    #$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434#$0430#$0447#$002C+
    #$0020#$0440#$0435#$0448#$0430#$0435#$043C#$044B#$0445#$0020#$043C#$043D+
    #$043E#$0433#$0438#$043C#$0438#$0020#$043A#$043E#$043B#$043B#$0435#$043A+
    #$0442#$0438#$0432#$0430#$043C#$0438#$0020#$043F#$0440#$043E#$0433#$0440+
    #$0430#$043C#$043C#$0438#$0441#$0442#$043E#$0432#$002C#$0020#$044F#$0432+
    #$043B#$044F#$0435#$0442#$0441#$044F#$0020#$043F#$043E#$0441#$0442#$0440+
    #$043E#$0435#$043D#$0438#$0435#$0020#$0438#$043D#$0444#$043E#$0440#$043C+
    #$0430#$0446#$0438#$043E#$043D#$043D#$043E#$0439#$0020#$0441#$0438#$0441+
    #$0442#$0435#$043C#$044B#$0020#$0434#$043B#$044F#$0020#$0430#$0432#$0442+
    #$043E#$043C#$0430#$0442#$0438#$0437#$0430#$0446#$0438#$0438#$0020#$0431+
    #$0438#$0437#$043D#$0435#$0441#$002D#$0434#$0435#$044F#$0442#$0435#$043B+
    #$044C#$043D#$043E#$0441#$0442#$0438#$0020#$043F#$0440#$0435#$0434#$043F+
    #$0440#$0438#$044F#$0442#$0438#$044F#$002E#$0020#$0412#$0441#$0435#$0020+
    #$0430#$0440#$0445#$0438#$0442#$0435#$043A#$0442#$0443#$0440#$043D#$044B+
    #$0435#$0020#$043A#$043E#$043C#$043F#$043E#$043D#$0435#$043D#$0442#$044B+
    #$0020#$0028#$0431#$0430#$0437#$044B#$0020#$0434#$0430#$043D#$043D#$044B+
    #$0445#$002C#$0020#$0441#$0435#$0440#$0432#$0435#$0440#$0430#$0020#$043F+
    #$0440#$0438#$043B#$043E#$0436#$0435#$043D#$0438#$0439#$002C#$0020#$043A+
    #$043B#$0438#$0435#$043D#$0442#$0441#$043A#$043E#$0435#$0020#$002E#$002E#$002E;
  Str3: ZWideString = #$041E#$0434#$043D#$043E#$0439#$0020#$0438#$0437#$0020#$043D#$0430#$0438#$0431#$043E#$043B#$0435#$0435;
  Str4: ZWideString = #$0442#$0440#$0438#$0432#$0438#$0430#$043B#$044C#$043D#$044B#$0445#$0020#$0437#$0430#$0434#$0430#$0447;
  Str5: ZWideString = #$0440#$0435#$0448#$0430#$0435#$043C#$044B#$0445#$0020#$043C#$043D#$043E#$0433#$0438#$043C#$0438;
  Str6: ZWideString = #$043A#$043E#$043B#$043B#$0435#$043A#$0442#$0438#$0432#$0430#$043C#$0438#$0020#$043F#$0440#$043E#$0433#$0440#$0430#$043C#$043C#$0438#$0441#$0442#$043E#$0432;

function ZTestCompSQLiteBugReportMBCs.GetSupportedProtocols: string;
begin
  Result := pl_all_sqlite;
end;

{**
  NUMBER must be froat
}
procedure ZTestCompSQLiteBugReportMBCs.Mantis248_TestNonASCIICharSelect;
const TestRowID = 248;
var
  Query: TZQuery;
  RowCounter: Integer;
  I: Integer;
  ConSettings: PZConSettings;
  CP: Word;
  procedure InsertValues(TestString: ZWideString);
  begin
    Query.ParamByName('s_id').AsInteger := TestRowID+RowCounter;
    Query.ParamByName('s_char').AsString := GetDBTestString(TestString, ConSettings);
    Query.ParamByName('s_varchar').AsString := GetDBTestString(TestString, ConSettings);
    Query.ParamByName('s_nchar').AsString := GetDBTestString(TestString, ConSettings);
    Query.ParamByName('s_nvarchar').AsString := GetDBTestString(TestString, ConSettings);

    Query.ExecSQL;
    inc(RowCounter);
  end;

  procedure CheckColumnValues(const TestString: ZWideString);
  begin
    {$IFDEF UNICODE}
    CheckEquals(TestString, Query.FieldByName('s_char').AsString);
    CheckEquals(TestString, Query.FieldByName('s_varchar').AsString);
    CheckEquals(TestString, Query.FieldByName('s_nchar').AsString);
    CheckEquals(TestString, Query.FieldByName('s_nvarchar').AsString);
    {$ELSE}
    If Connection.ControlsCodePage = cCP_UTF16
    then CheckEquals(TestString, Query.FieldByName('s_char').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF}, 's_char')
    else CheckEquals(ZUnicodeToString(TestString, CP), Query.FieldByName('s_char').AsString, 's_char');
    If Connection.ControlsCodePage = cCP_UTF16
    then CheckEquals(TestString, Query.FieldByName('s_varchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF}, 's_varchar')
    else CheckEquals(ZUnicodeToString(TestString, CP), Query.FieldByName('s_varchar').AsString, 's_varchar');
    If Connection.ControlsCodePage = cCP_UTF16
    then CheckEquals(TestString, Query.FieldByName('s_nchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF}, 's_nchar')
    else CheckEquals(ZUnicodeToString(TestString, CP), Query.FieldByName('s_nchar').AsString, 's_nchar');
    If Connection.ControlsCodePage = cCP_UTF16
    then CheckEquals(TestString, Query.FieldByName('s_nvarchar').{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}Value{$ENDIF}, 's_nvarchar')
    else CheckEquals(ZUnicodeToString(TestString, CP), Query.FieldByName('s_nvarchar').AsString, 's_nvarchar');
    {$ENDIF}
  end;
begin
//??  if SkipForReason(srClosedBug) then Exit;

  Query := CreateQuery;
  Connection.Connect;
  Check(Connection.Connected);
  ConSettings := Connection.DbcConnection.GetConSettings;
  try
    RowCounter := 0;
    if ConSettings.AutoEncode and not ((ZOSCodePage = zCP_UTF8) or (ZOSCodePage = zCP_WIN1251) or (ZOSCodePage = zcp_DOS855) or (ZOSCodePage = zCP_KOI8R)) then
      Exit;
    if ConSettings.AutoEncode
    then CP := ConSettings.CTRL_CP
    else CP := ConSettings.ClientCodePage.CP;
    //eh the russion dull text can no be mapped to other charsets then:
    if not ((CP = zCP_UTF8) or (CP = zCP_WIN1251) or (CP = zcp_DOS855) or (CP = zCP_KOI8R))
      {add some more if you run into same issue !!} then
      Exit;
    Query.SQL.Text := 'Insert into string_values (s_id, s_char, s_varchar, s_nchar, s_nvarchar)'+
      ' values (:s_id, :s_char, :s_varchar, :s_nchar, :s_nvarchar)';
    InsertValues(str2);
    InsertValues(str3);
    InsertValues(str4);
    InsertValues(str5);
    InsertValues(str6);

    Query.SQL.Text := 'select * from string_values where s_id > '+IntToStr(TestRowID-1);
    Query.Open;
    CheckEquals(True, Query.RecordCount = 5);

    Query.SQL.Text := 'select * from string_values where s_char like ''%'+ZUnicodeToString(Str2, zCP_UTF8)+'%''';
    Query.Open;
    CheckEquals(True, Query.RecordCount = 1);
    CheckColumnValues(Str2);

    Query.SQL.Text := 'select * from string_values where s_char like ''%'+ZUnicodeToString(Str3, zCP_UTF8)+'%''';
    Query.Open;
    CheckEquals(True, Query.RecordCount = 2);
    CheckColumnValues(Str2);
    Query.Next;
    CheckColumnValues(Str3);

    Query.SQL.Text := 'select * from string_values where s_char like ''%'+ZUnicodeToString(Str4, zCP_UTF8)+'%''';
    Query.Open;
    CheckEquals(True, Query.RecordCount = 2);
    CheckColumnValues(Str2);
    Query.Next;
    CheckColumnValues(Str4);

    Query.SQL.Text := 'select * from string_values where s_char like ''%'+ZUnicodeToString(Str5, zCP_UTF8)+'%''';
    Query.Open;
    CheckEquals(True, Query.RecordCount = 2);
    CheckColumnValues(Str2);
    Query.Next;
    CheckColumnValues(Str5);

    Query.SQL.Text := 'select * from string_values where s_char like ''%'+ZUnicodeToString(Str6, zCP_UTF8)+'%''';
    Query.Open;
    CheckEquals(True, Query.RecordCount = 2);
    CheckColumnValues(Str2);
    Query.Next;
    CheckColumnValues(Str6);

  finally
    for i := TestRowID to TestRowID+RowCounter do
    begin
      Query.SQL.Text := 'delete from string_values where s_id = '+IntToStr(i);
      Query.ExecSQL;
    end;
    Query.Free;
  end;
end;

initialization
  RegisterTest('bugreport',ZTestCompSQLiteBugReport.Suite);
  RegisterTest('bugreport',ZTestCompSQLiteBugReportMBCs.Suite);
end.
