{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          String tokenizing classes for Sybase           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZSybaseToken;

interface

{$I ZParseSql.inc}

{$IF defined(ZEOS_DISABLE_DBLIB) and defined(ZEOS_DISABLE_ASA) and
     defined(ZEOS_DISABLE_SQLANY) and defined(ZEOS_DISABLE_ADO) and
     defined(ZEOS_DISABLE_OLEDB) and defined(ZEOS_DISABLE_ODBC) and defined(ZEOS_DISABLE_PROXY)}
  {$DEFINE EMPTY_ZSybaseToken}
{$IFEND}

{$IFNDEF DEFINE EMPTY_ZSybaseToken}

uses ZTokenizer, ZGenericSqlToken;

type
  /// <summary>Implements a Sybase-specific number state object.</summary>
  TZSybaseNumberState = TZGenericSQLHexNumberState;

  /// <summary>Implements a Sybase-specific quote string state object.</summary>
  TZSybaseQuoteState = TZGenericSQLBracketQuoteState;

  /// <summary>Implements a Sybase-specific a comment state object.</summary>
  TZSybaseCommentState = TZGenericSQLCommentState;

  /// <summary>Implements a Sybase-specific a symbol state object.</summary>
  TZSybaseSymbolState = class (TZSymbolState)
  public
    /// <summary>Creates this Sybase-specific symbol state object.</summary>
    constructor Create;
  end;

  /// <summary>Implements a Sybase-specific a word state object.</summary>
  TZSybaseWordState = class (TZGenericSQLWordState)
  public
    /// <summary>Constructs this Sybase-specific word state object.</summary>
    constructor Create;
  end;

  /// <summary>Implements a Sybase-specific a tokenize object.</summary>
  TZSybaseTokenizer = class (TZTokenizer)
  protected
    /// <summary>Constructs a default state table (as described in the class
    ///  comment).</summary>
    procedure CreateTokenStates; override;
  end;

{$ENDIF EMPTY_ZSybaseToken}
implementation
{$IFNDEF EMPTY_ZSybaseToken}

{ TZSybaseSymbolState }

constructor TZSybaseSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!<');
  Add('!>');
  Add('!=');
end;

{ TZSybaseWordState }

constructor TZSybaseWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
  SetWordChars('@', '@', True);
  SetWordChars('#', '#', True);
end;

{ TZSybaseTokenizer }

procedure TZSybaseTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZSybaseSymbolState.Create;
  NumberState := TZSybaseNumberState.Create;
  QuoteState := TZSybaseQuoteState.Create;
  WordState := TZSybaseWordState.Create;
  CommentState := TZSybaseCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);
  SetCharacterState('@', '@', WordState);
  SetCharacterState('#', '#', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState('''', '''', QuoteState);
  SetCharacterState('[', '[', QuoteState);
  SetCharacterState(']', ']', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{$ENDIF EMPTY_ZSybaseToken}
end.
