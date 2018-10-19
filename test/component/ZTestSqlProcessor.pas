{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for SQL Script Class              }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestSqlProcessor;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZSqlProcessor, ZSqlTestCase, ZScriptParser;

type

  {** Implements a test case for class TZSqlProcessor. }

  { TZTestSQLProcessorCase }

  TZTestSQLProcessorCase = class(TZAbstractCompSQLTestCase)
  private
    FProcessor: TZSQLProcessor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultProcessor;
    procedure TestGoProcessor;
    procedure TestEmptyLineProcessor;
    procedure TestSetTermProcessor;
    procedure TestUncompleted;
    procedure TestParamChar;
    procedure TestScriptParser;
  end;

  {** Implements a test case for class TZSqlProcessor. }
  TZTestSQLProcessorMysqlCase = class(TZAbstractCompSQLTestCase)
  private
    FProcessor: TZSQLProcessor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
  published
    procedure TestSpecialCommentProcessor;
    procedure TestMysqlCommentDefaultProcessor;
    procedure TestMysqlEmptyLineProcessor;
    procedure TestMysqlSetTermProcessor;
  end;

implementation

uses Classes,
  ZCompatibility, ZSysUtils;

{ TZTestSQLProcessorCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLProcessorCase.SetUp;
begin
  inherited SetUp;
  FProcessor := TZSQLProcessor.Create(nil);
  FProcessor.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLProcessorCase.TearDown;
begin
  FProcessor.Free;
  inherited TearDown;
end;

{**
  Runs a test for SQL Processor with default delimiters.
}
procedure TZTestSQLProcessorCase.TestDefaultProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);
//  FProcessor.DelimiterType = sdDefault;

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */';
  Delimiter := ';';

  Text := Comment + NewLine + Line + Delimiter + NewLine + '   ' + NewLine + Line +
    Comment + Delimiter + NewLine + Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[2]);
end;

{**
  Runs a test for SQL Processor with empty line delimiters.
}
procedure TZTestSQLProcessorCase.TestEmptyLineProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);
  FProcessor.DelimiterType := dtEmptyLine;

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */';
  Delimiter := NewLine + '  ';

  Text := Comment + NewLine + Line + Delimiter + NewLine + '   ' + NewLine + Line +
    Comment + Delimiter + NewLine + Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + ' ' + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment + ' ' + Line + ' ', FProcessor.Statements[2]);
end;

{**
  Runs a test for SQL Processor with GO delimiter.
}
procedure TZTestSQLProcessorCase.TestGoProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);
  FProcessor.DelimiterType := dtGo;

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';
  if ProtocolType = protMySQL then
    Comment := '# Comment...'
  else
    Comment := '/* Comment... */';
  Delimiter := NewLine + 'Go' + NewLine;

  Text := Comment + NewLine + Line + Delimiter + NewLine + '   ' + NewLine + Line +
    Comment + Delimiter + NewLine + Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line + NewLine, FProcessor.Statements[0]);
  CheckEquals(Line + Comment + NewLine, FProcessor.Statements[1]);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[2]);
end;

{**
  Runs a test for SQL Processor with SET TERM delimiter.
}
procedure TZTestSQLProcessorCase.TestSetTermProcessor;
var
  NewLine: String;
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FProcessor);
  FProcessor.DelimiterType := dtSetTerm;

  Line := '/AAA/ BBB CCC';
  NewLine := LineEnding;
  Comment := '/* Comment... */';
  Delimiter := '^';

  Text := Comment + NewLine +
          ' Set Term ' + Delimiter + ' ;' + NewLine +
          Comment + NewLine +
          Line + Delimiter + NewLine +
          '   ' + NewLine +
          Line + Comment + NewLine +
          Delimiter + NewLine +
          'Set Term ; ' + Delimiter +
          Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment  + NewLine + Line, FProcessor.Statements[2]);
end;

{**
  Runs a test for uncompleted SQL statements.
}
procedure TZTestSQLProcessorCase.TestUncompleted;
var
  SQLScript: string;
begin
  SQLScript := 'SELECT * FROM people;' + #10 + 'SELECT * FROM cargo;';
  FProcessor.Script.Text := SQLScript;
  FProcessor.Parse;

  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals('SELECT * FROM people', FProcessor.Statements[0]);
  CheckEquals('SELECT * FROM cargo', FProcessor.Statements[1]);

  SQLScript := 'SELECT * FROM people;' + #10#13 + 'SELECT * FROM cargo';
  FProcessor.Script.Text := SQLScript;
  FProcessor.Parse;

  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals('SELECT * FROM people', FProcessor.Statements[0]);
  CheckEquals('SELECT * FROM cargo', FProcessor.Statements[1]);
end;

procedure TZTestSQLProcessorCase.TestParamChar;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);
//  FProcessor.DelimiterType = sdDefault;

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */';
  Delimiter := ';';

  Text := Comment + NewLine + Line + Delimiter + Line;
  FProcessor.Script.Text := Text;
  FProcessor.ParamCheck := true;
  FProcessor.Parse;

  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line, FProcessor.Statements[1]);
  CheckEquals(0, FProcessor.Params.Count);

  Text := Comment + NewLine + Line + ' :a' + Delimiter + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;
  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line + ' :a', FProcessor.Statements[0]);
  CheckEquals(Line, FProcessor.Statements[1]);
  CheckEquals(1, FProcessor.Params.Count);
  CheckEquals('a', FProcessor.Params[0].Name);;

  FProcessor.ParamChar := '&';
  FProcessor.Parse;
  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals(0, FProcessor.Params.Count);

  Text := Comment + NewLine + Line + ' &b' + Delimiter + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;
  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line + ' &b', FProcessor.Statements[0]);
  CheckEquals(Line, FProcessor.Statements[1]);
  CheckEquals(1, FProcessor.Params.Count);
  CheckEquals('b', FProcessor.Params[0].Name);;
end;

procedure TZTestSQLProcessorCase.TestScriptParser;
const
  stmt1 = String(
    'create table domain_values'+LineEnding+'('+LineEnding+
    '   d_id                       INTEGER not null,'+LineEnding+
    '   d_fld1                     tinteger,'+LineEnding+
    '   d_fld2                     tfloat,'+LineEnding+
    '   d_fld3                     tstring,'+LineEnding+
    '   primary key (d_id)'+LineEnding+
    ')');

  stmt2 = String(
    '/*==============================================================*/'+LineEnding+
    '/* Stored procedure: procedure1                                 */'+LineEnding+
    '/*==============================================================*/'+LineEnding+
    LineEnding+
    'CREATE PROCEDURE PROCEDURE1(P1 INTEGER)'+LineEnding+
    '   RETURNS(R1 INTEGER)'+LineEnding+
    'AS'+LineEnding+
    'BEGIN'+LineEnding+
    '  R1 = P1 + 1;'+LineEnding+
    'SUSPEND;'+LineEnding+
    'END');

  stmt3 = String(
    '/*==============================================================*/'+LineEnding+
    '/* Stored procedure: procedure2                                 */'+LineEnding+
    '/*==============================================================*/'+LineEnding+
    LineEnding+
    'CREATE PROCEDURE PROCEDURE2'+LineEnding+
    '   RETURNS(R1 VARCHAR(30))'+LineEnding+
    'AS'+LineEnding+
    'BEGIN'+LineEnding+
    '  FOR SELECT eq_name FROM equipment ORDER BY eq_name INTO :R1'+LineEnding+
    '  DO'+LineEnding+
    '  SUSPEND;'+LineEnding+
    'END'+LineEnding+
    '/*==============================================================*/'+LineEnding+
    '/* Grant privileges to columns                                  */'+LineEnding+
    '/*==============================================================*/');
begin
  FProcessor.DelimiterType := dtSetTerm;
  Fprocessor.LoadFromFile(TestFilePath('text/TestSQLProcessor3Stmts.sql'));
  Fprocessor.Parse;
  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(stmt1,FProcessor.Statements[0]);
  CheckEquals(stmt2,FProcessor.Statements[1]);
  CheckEquals(stmt3,FProcessor.Statements[2]);
end;

{ TZTestSQLProcessorMysqlCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLProcessorMysqlCase.SetUp;
begin
  inherited SetUp;
  FProcessor := TZSQLProcessor.Create(nil);
  FProcessor.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLProcessorMysqlCase.TearDown;
begin
  FProcessor.Free;
  inherited TearDown;
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestSQLProcessorMysqlCase.GetSupportedProtocols: string;
begin
  Result := pl_all_mysql;
end;

{**
  Runs a test for uncompleted SQL statements.
}
procedure TZTestSQLProcessorMysqlCase.TestSpecialCommentProcessor;
var
  SQLScript: string;
begin
  SQLScript := '/*!SELECT * FROM people*/;' + #10 + 'SELECT * FROM cargo;';
  FProcessor.CleanupStatements := True;
  FProcessor.Script.Text := SQLScript;
  FProcessor.Parse;

  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals('/*!SELECT * FROM people*/', FProcessor.Statements[0]);
  CheckEquals('SELECT * FROM cargo', FProcessor.Statements[1]);

  SQLScript := '--SELECT * FROM people;' + #10 + 'SELECT * FROM cargo;';
  FProcessor.CleanupStatements := True;
  FProcessor.Script.Text := SQLScript;
  FProcessor.Parse;

  CheckEquals(1, FProcessor.StatementCount);
  CheckEquals('SELECT * FROM cargo', FProcessor.Statements[0]);

  SQLScript := '/*SELECT * FROM people*/;' + #10 + 'SELECT * FROM cargo;';
  FProcessor.CleanupStatements := True;
  FProcessor.Script.Text := SQLScript;
  FProcessor.Parse;

  CheckEquals(1, FProcessor.StatementCount);
  CheckEquals('SELECT * FROM cargo', FProcessor.Statements[0]);
end;

{**
  Runs a mysql specific test for SQL Processor with default line delimiters.
}
procedure TZTestSQLProcessorMysqlCase.TestMysqlCommentDefaultProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';
  Comment := '# Comment...';
  Delimiter := ';';

  Text := Comment + NewLine + Line + Delimiter + NewLine + '   ' + NewLine + Line +
    Comment + Delimiter + NewLine + Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(2, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment + Delimiter + NewLine + Comment + NewLine + Line, FProcessor.Statements[1]);
end;

{**
  Runs a mysql specific test for SQL Processor with empty line delimiters.
}
procedure TZTestSQLProcessorMysqlCase.TestMysqlEmptyLineProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
  NewLine: string;
begin
  CheckNotNull(FProcessor);
  FProcessor.DelimiterType := dtEmptyLine;

  NewLine := LineEnding;
  Line := '/AAA/ BBB CCC';

  Comment := '# Comment...'+NewLine;
  Delimiter := NewLine + '  ';
  Text := Comment + Line + Delimiter + NewLine + '   ' + NewLine + Line +
    Comment + Delimiter + NewLine + Comment + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment + Line + ' ', FProcessor.Statements[2]);
end;

procedure TZTestSQLProcessorMysqlCase.TestMysqlSetTermProcessor;
var
  NewLine: String;
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FProcessor);
  FProcessor.DelimiterType := dtSetTerm;

  Line := '/AAA/ BBB CCC';
  NewLine := LineEnding;
  Comment := '# Comment...'+NewLine;
  Delimiter := '^';

  Text := Comment + NewLine +
          ' Set Term ' + Delimiter + ' ;' + NewLine +
          Comment + NewLine +
          Line + Delimiter + NewLine +
          '   ' + NewLine +
          Line + Comment + NewLine +
          Delimiter + NewLine +
          'Set Term ; ' + Delimiter +
          Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment  + NewLine + Line, FProcessor.Statements[2]);
end;

initialization
  RegisterTest('component',TZTestSQLProcessorCase.Suite);
  RegisterTest('component',TZTestSQLProcessorMysqlCase.Suite);
end.
