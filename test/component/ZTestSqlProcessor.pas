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

uses
  TestFramework, ZSqlStrings, SysUtils, ZTokenizer, ZConnection, ZSqlProcessor,
  ZTestDefinitions, ZScriptParser;

type

  {** Implements a test case for class TZSqlProcessor. }
  TZTestSQLProcessorCase = class(TZComponentPortableSQLTestCase)
  private
    FConnection: TZConnection;
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
  end;

implementation

uses Classes, ZDbcUtils, ZSysUtils;

{ TZTestSQLProcessorCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLProcessorCase.SetUp;
begin
  FConnection := CreateDatasetConnection;
  FProcessor := TZSQLProcessor.Create(nil);
  FProcessor.Connection := FConnection;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLProcessorCase.TearDown;
begin
  FProcessor.Free;
  FConnection.Free;
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

  NewLine := #13#10;
  Line := '/AAA/ BBB CCC';
  if StartsWith(Protocol, 'mysql') then
    Comment := '# Comment...'
  else Comment := '/* Comment... */';
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

  NewLine := #13#10;
  Line := '/AAA/ BBB CCC';
  if StartsWith(Protocol, 'mysql') then
    Comment := '# Comment...'
  else Comment := '/* Comment... */';
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

  NewLine := #13#10;
  Line := '/AAA/ BBB CCC';
  if StartsWith(Protocol, 'mysql') then
    Comment := '# Comment...'
  else Comment := '/* Comment... */';
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
  NewLine := #13#10;
  if StartsWith(Protocol, 'mysql') then
    Comment := '# Comment...'
  else Comment := '/* Comment... */';
  Delimiter := '^';

  Text := ' Set Term ^ ;' + NewLine + Comment + NewLine + Line + Delimiter + NewLine +
    '   ' + NewLine + Line + Comment + NewLine + Delimiter + NewLine + 'Set Term ; ^' +
    Comment + NewLine + Line;
  FProcessor.Script.Text := Text;
  FProcessor.Parse;

  CheckEquals(3, FProcessor.StatementCount);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[0]);
  CheckEquals(Line + Comment, FProcessor.Statements[1]);
  CheckEquals(Comment + NewLine + Line, FProcessor.Statements[2]);
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

initialization
  TestFramework.RegisterTest(TZTestSQLProcessorCase.Suite);
end.
