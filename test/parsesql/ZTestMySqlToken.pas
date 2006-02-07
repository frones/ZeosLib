{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for MySQL Tokenizer Classes          }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
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

unit ZTestMySqlToken;

interface

uses TestFramework, ZTestCase, ZClasses, ZTokenizer, ZMySqlToken,
  ZTestTokenizer;

type

  {** Implements a test case for MySqlTokenizer classes. }
  TZTestMySQLTokenizer = class(TZAbstractTokenizerTestCase)
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

uses SysUtils;

{ TZTestMySQLTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestMySQLTokenizer.SetUp;
begin
  Tokenizer := TZMySQLTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestMySQLTokenizer.TestCommentState;
const
  TokenString1: string = 'aaa/*bbb*/ccc#ddd'#10;
  TokenTypes1: array[0..4] of TZTokenType = (
    ttWord, ttComment, ttWord, ttComment, ttWhitespace);
  TokenValues1: array[0..4] of string = (
    'aaa', '/*bbb*/', 'ccc', '#ddd', #10);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestMySQLTokenizer.TestQuoteState;
const
  TokenString1: string = '"aaa" `b``b`''c\''c''''c''';
  TokenTypes1: array[0..2] of TZTokenType = (
    ttQuoted, ttQuoted, ttQuoted);
  TokenValues1: array[0..2] of string = (
    '"aaa"', '`b``b`', '''c\''c''''c''');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for symbols.
}
procedure TZTestMySQLTokenizer.TestSymbolState;
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
procedure TZTestMySQLTokenizer.TestWhitespaceState;
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
procedure TZTestMySQLTokenizer.TestWordState;
const
  TokenString1: string = ' _a_a. $c$c p2p';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttWord);
  TokenValues1: array[0..3] of string = (
    '_a_a', '.', '$c$c', 'p2p');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestMySQLTokenizer.TestNumberState;
const
  TokenString1: string = 'A.E .123 123.456a 0xFfH 0x54BC 123.456e10 2E-12c';
  TokenTypes1: array[0..11] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttFloat, ttFloat, ttWord, ttHexDecimal, ttWord,
    ttHexDecimal, ttFloat, ttFloat, ttWord);
  TokenValues1: array[0..11] of string = (
    'A', '.', 'E', '.123', '123.456', 'a', '0xFf', 'H', '0x54BC', '123.456e10',
    '2E-12', 'c');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

initialization
  TestFramework.RegisterTest(TZTestMySQLTokenizer.Suite);
end.

