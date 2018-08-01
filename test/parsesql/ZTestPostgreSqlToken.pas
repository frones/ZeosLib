{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Case for PostgreSQL Tokenizer Classes       }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZTestPostgreSqlToken;

interface
{$I ZParseSql.inc}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTokenizer, ZPostgreSqlToken,
  ZTestTokenizer;

type

  {** Implements a test case for PostgreSqlTokenizer classes. }
  TZTestPostgreSQLTokenizer = class(TZAbstractTokenizerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestWhitespaceState;
    procedure TestQuoteState;
    procedure TestCommentState;
    procedure TestSymbolState;
    procedure TestWordState;
    procedure TestNumberState;
  end;

implementation

{ TZTestPostgreSQLTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestPostgreSQLTokenizer.SetUp;
begin
  Tokenizer := TZPostgreSQLTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestPostgreSQLTokenizer.TestCommentState;
const
  TokenString1: string = '-aaa/*bbb*/ccc--ddd'#10'/*eee/*eee*/eee*/';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttSymbol, ttWord, ttComment, ttWord, ttComment, ttComment);
  TokenValues1: array[0..5] of string = (
    '-', 'aaa', '/*bbb*/', 'ccc', '--ddd'#10, '/*eee/*eee*/eee*/');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestPostgreSQLTokenizer.TestQuoteState;
const
  //TokenString146: string = 'nazwa1 = ''\'''',';
  //TokenValues146Off: array[0..3] of string = ('nazwa1', '=', '''\''''',',');
  //TokenTypes146Off: array[0..3] of TZTokenType = (ttWord, ttSymbol, ttQuoted, ttSymbol);

  TokenString1: string = '"a\""\''aa" ''c\'' ''c''''c''';
  //TokenTypes1Off: array[0..1] of TZTokenType = (ttWord, ttQuoted);
  //TokenValues1Off: array[0..1] of string = ('"a\""\''aa"', '''c\'' ''c''''c''');

  TokenTypes1On: array[0..2] of TZTokenType = (ttWord, ttQuoted, ttQuoted);
  TokenValues1On: array[0..2] of string = ('"a\""\''aa"', '''c\''', '''c''''c''');

  TokenString2: string = '$aaa$bbb$$ccc$aaa$ddd$ $$eee$$';
  TokenTypes2: array[0..2] of TZTokenType = (
    ttQuoted, ttWord, ttQuoted);
  TokenValues2: array[0..2] of string = (
    '$aaa$bbb$$ccc$aaa$', 'ddd$', '$$eee$$');

  //TokenString3: string = 'E''eee'' B''bbb'' X''xxx'' U&''uuu'' U$''zzz''';
  //TokenTypes3: array[0..5] of TZTokenType = (
  //  ttQuoted, ttQuoted, ttQuoted, ttQuoted, ttWord, ttQuoted);
  //TokenValues3: array[0..5] of string = (
  //  'E''eee''', 'B''bbb''', 'X''xxx''', 'U&''uuu''', 'U$', '''zzz''');

  TokenString4: string = '$body$ $1,$2 $body$ $$ $1,$2 $$';
  TokenTypes4: array[0..1] of TZTokenType = (ttQuoted, ttQuoted);
  TokenValues4: array[0..1] of string = (
    '$body$ $1,$2 $body$', '$$ $1,$2 $$');

begin
  //test seems to be plain wrong: https://sourceforge.net/p/zeoslib/tickets/214/
//  (Tokenizer as TZPostgreSQLTokenizer).SetStandardConformingStrings(False);
//  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
//    [toSkipEOF, toSkipWhitespaces]), TokenTypes1Off, TokenValues1Off);

//  (Tokenizer as TZPostgreSQLTokenizer).SetStandardConformingStrings(False);
//  CheckTokens(Tokenizer.TokenizeBuffer(TokenString146,
//    [toSkipEOF, toSkipWhitespaces]), TokenTypes146Off, TokenValues146Off);

  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1On, TokenValues1On);

  CheckTokens(Tokenizer.TokenizeBuffer(TokenString2,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes2, TokenValues2);

//  CheckTokens(Tokenizer.TokenizeBuffer(TokenString3,
//    [toSkipEOF, toSkipWhitespaces]), TokenTypes3, TokenValues3);

  CheckTokens(Tokenizer.TokenizeBuffer(TokenString4,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes4, TokenValues4);
end;

{**
  Runs a test for symbols.
}
procedure TZTestPostgreSQLTokenizer.TestSymbolState;
const
  TokenString1: string = '=<>>=<< < <';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol);
  TokenValues1: array[0..5] of string = (
    '=', '<>', '>=', '<<', '<', '<');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for whitespaces.
}
procedure TZTestPostgreSQLTokenizer.TestWhitespaceState;
const
  TokenString1: string = 'aaa '#9'ccc'#10#13;
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttWhitespace, ttWord, ttWhitespace);
  TokenValues1: array[0..3] of string = (
    'aaa', ' '#9, 'ccc', #10#13);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for words.
}
procedure TZTestPostgreSQLTokenizer.TestWordState;
const
  TokenString1: string = ' _a_a. p2p b$b';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttWord);
  TokenValues1: array[0..3] of string = (
    '_a_a', '.', 'p2p', 'b$b');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestPostgreSQLTokenizer.TestNumberState;
const
  TokenString1: string = '.A .123 123.456a 123.456e10 2E-12c';
  TokenTypes1: array[0..7] of TZTokenType = (
    ttSymbol, ttWord, ttFloat, ttFloat, ttWord, ttFloat, ttFloat, ttWord);
  TokenValues1: array[0..7] of string = (
    '.', 'A', '.123', '123.456', 'a', '123.456e10', '2E-12', 'c');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

initialization
  RegisterTest('parsesql',TZTestPostgreSQLTokenizer.Suite);
end.

