{*********************************************************}
{                                                         }
{                     Zeos SQL Shell                      }
{             Test Case for SQL Script Class              }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{               Written by Sergey Seroukhov               }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZTestScriptParser;

interface

uses
  TestFramework, SysUtils, ZTokenizer, ZPostgreSqlToken, ZScriptParser,
  ZTestDefinitions;

type

  {** Implements a test case for class TZSQLScriptParser. }
  TZTestSQLScriptParserCase = class(TZParseSQLGenericTestCase)
  private
    FParser: TZSQLScriptParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultProcessor;
    procedure TestGoProcessor;
    procedure TestEmptyLineProcessor;
    procedure TestSetTermProcessor;
    procedure TestCleanup;
  end;

implementation

uses Classes, ZDbcUtils, ZSysUtils;

{ TZTestSQLScriptParserCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLScriptParserCase.SetUp;
begin
  FParser := TZSQLScriptParser.Create;
  FParser.Tokenizer := TZPostgreSQLTokenizer.Create;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLScriptParserCase.TearDown;
begin
  FParser.Free;
end;

{**
  Runs a test for cleaning up SQL statements.
}
procedure TZTestSQLScriptParserCase.TestCleanup;
var
  EndOfLine: string;
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtDefault;
  FParser.CleanupStatements := False;

  EndOfLine := #10#13;
  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */';
  Delimiter := ';';

  Text := ' ' + Comment  + EndOfLine + Line + Delimiter
    + EndOfLine + '   ' + EndOfLine + Line + Comment + EndOfLine + Delimiter
    + Comment + EndOfLine + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  //--> ms, 20/10/2005: changed due to new behaviour (ignore CRLF and empty
  //                    lines or lines with only spaces, preceding an SQL-
  //                    Statement. Trailing CRLFs will also be ignored)
  //
  //CheckEquals(' ' + Comment  + EndOfLine + Line, FParser.Statements[0]);
  //CheckEquals(EndOfLine + '   ' + EndOfLine + Line + Comment + EndOfLine,
  //  FParser.Statements[1]);
  //CheckEquals(Comment + EndOfLine + Line, FParser.UncompletedStatement);
  //
  CheckEquals(Comment  + EndOfLine + Line, FParser.Statements[0]);
  CheckEquals(Line + Comment, FParser.Statements[1]);
  CheckEquals(Comment + EndOfLine + Line, FParser.UncompletedStatement);
  // <-- ms
end;

{**
  Runs a test for SQL Parser with default delimiters.
}
procedure TZTestSQLScriptParserCase.TestDefaultProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtDefault;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := ';'#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + Comment + Delimiter
    + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);

  FParser.ClearCompleted;
  CheckEquals(0, FParser.StatementCount);
  CheckEquals(Line, FParser.UncompletedStatement);

  FParser.Delimiter := '*';
  FParser.Clear;
  CheckEquals(0, FParser.StatementCount);
  CheckEquals('', FParser.UncompletedStatement);
  CheckEquals(';', FParser.Delimiter);
end;

{**
  Runs a test for SQL Parser with empty line delimiters.
}
procedure TZTestSQLScriptParserCase.TestEmptyLineProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtEmptyLine;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := #10'  '#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + ';' + Comment
    + Delimiter + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);
end;

{**
  Runs a test for SQL Parser with GO delimiter.
}
procedure TZTestSQLScriptParserCase.TestGoProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtGo;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := #10#13'  Go  '#10#13;

  Text := Comment + Line + Delimiter + '   '#10#13 + Line + Comment + Delimiter
    + Comment + Line;
  FParser.ParseText(Text);

  CheckEquals(2, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.UncompletedStatement);
end;

{**
  Runs a test for SQL Parser with SET TERM delimiter.
}
procedure TZTestSQLScriptParserCase.TestSetTermProcessor;
var
  Line: string;
  Delimiter: string;
  Comment: string;
  Text: string;
begin
  CheckNotNull(FParser);
  FParser.DelimiterType := dtSetTerm;

  Line := '/AAA/ BBB CCC';
  Comment := '/* Comment... */'#10#13;
  Delimiter := '!!'#10#13;

  Text := ' Set Term !! ;'#10#13 + Comment + Line + Delimiter + '   '#10#13
    + Line + Comment + Delimiter + 'Set Term ; !!' + Comment + Line + ';';
  FParser.ParseText(Text);

  CheckEquals(3, FParser.StatementCount);
  CheckEquals(Line, FParser.Statements[0]);
  CheckEquals(Line, FParser.Statements[1]);
  CheckEquals(Line, FParser.Statements[2]);
end;

initialization
  TestFramework.RegisterTest(TZTestSQLScriptParserCase.Suite);
end.
