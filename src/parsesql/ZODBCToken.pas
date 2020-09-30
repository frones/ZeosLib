{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           String tokenizing classes for OleDB           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                                                         }
{         unit Originally written by EgonHugeist          }
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

unit ZODBCToken;

interface

{$I ZParseSql.inc}

{$IF defined(ZEOS_DISABLE_ODBC) and defined(ZEOS_DISABLE_ADO) and defined(ZEOS_DISABLE_OLEDB)}
  {$DEFINE ZEOS_DISABLE_ODBC}
{$ELSE}
  {$UNDEF ZEOS_DISABLE_ODBC}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ODBC}
uses
  Classes, SysUtils, ZTokenizer, ZGenericSqlToken;

type
  {** Implements a quote string state object. }
  TZODBCQuoteState = TZGenericSQLBracketQuoteState;

  {** Implements a default tokenizer object. }
  TZODBCTokenizer = class (TZGenericSQLTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

{$ENDIF ZEOS_DISABLE_ODBC}
implementation
{$IFNDEF ZEOS_DISABLE_ODBC}

{ TZODBCTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZODBCTokenizer.CreateTokenStates;
begin
  NumberState := TZNumberState.Create;
  QuoteState := TZODBCQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  CommentState := TZCppCommentState.Create;

  SymbolState := TZGenericSQLSymbolState.Create;
  WordState := TZGenericSQLWordState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);
  SetCharacterState('@', '@', WordState);
  SetCharacterState('#', '#', WordState); //added for mssql temp tables

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('`', '`', QuoteState); //MySQL
  SetCharacterState('"', '"', QuoteState); //standard SQL
  SetCharacterState(#39, #39, QuoteState); //standard SQL
  SetCharacterState('[', '[', QuoteState); //ODBC syntax
  SetCharacterState(']', ']', QuoteState); //ODBC syntax

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{$ENDIF ZEOS_DISABLE_ODBC}


end.
