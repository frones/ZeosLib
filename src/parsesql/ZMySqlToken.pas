{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           String tokenizing classes for MySQL           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZMySqlToken;

interface

{$I ZParseSql.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type

  {** Implements a MySQL-specific number state object. }
  TZMySQLNumberState = TZGenericSQLHexNumberState;

  {** Implements a MySQL-specific quote string state object. }
  TZMySQLQuoteState = class (TZQuoteState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeToken(const Value: TZToken; QuoteChar: Char): string; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZMySQLCommentState = class (TZCppCommentState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZMySQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZMySQLWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZMySQLTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}

implementation

{$IFNDEF ZEOS_DISABLE_MYSQL}

{$IFDEF FAST_MOVE}uses ZFastCode;{$ENDIF}

{ TZMySQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZMySQLQuoteState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
const BackSlash = Char('\');
var
  LastChar: Char;
begin
  Result.P := SPos;
  If SPos^ = '`'
  then Result.TokenType := ttQuotedIdentifier
  else Result.TokenType := ttQuoted;

  LastChar := #0;
  while SPos < NTerm do begin
    Inc(SPos);
    if ((LastChar = Result.P^) and (SPos^ <> Result.P^)) or (SPos = NTerm) then begin
      Dec(SPos);
      Break;
    end;
    if LastChar = BackSlash then
      LastChar := #0
    else if (LastChar = Result.P^) and (SPos^ = Result.P^) then
      LastChar := #0
    else
      LastChar := SPos^;
  end;
  Result.L := SPos-Result.P+1;
end;

{**
  Decodes a string value.
  @param Value a token value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZMySQLQuoteState.DecodeToken(const Value: TZToken;
  QuoteChar: Char): string;
begin
  if (Value.L >= 2) and (Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')])
    and (Value.p^ = QuoteChar) and ((Value.P+Value.L-1)^ = QuoteChar) then
  begin
    if Value.L > 2
    then DecodeCString(Value.L-2, Value.P+1, {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString(Result){$ELSE}Result{$IFEND})
    else Result := '';
  end
  else SetString(Result, Value.P, Value.L)
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZMySQLQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if (Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]) then
    Result := QuoteChar + EncodeCString(Value) + QuoteChar
  else Result := Value;
end;

{ TZMySQLCommentState }

{**
  Gets a MySQL specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZMySQLCommentState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttUnknown;
  Result.P := SPos;

  if SPos+1 < NTerm then
  case SPos^ of
    '-': begin
        Inc(SPos);
        //MySQL sees the -- only if it's followed by a whitespace
        //intentenion was substract a negative numer (: like
        //UPDATE account SET credit=credit--1 funny isn't it
        if (SPos^ = '-') and (SPos+1 < NTerm) and (Ord((SPos+1)^) <= Ord(' ')) then begin
          Result.TokenType := ttComment;
          GetSingleLineComment(SPos, NTerm);
        end else
          Dec(SPos);
      end;
    '#': begin
        Result.TokenType := ttComment;
        GetSingleLineComment(SPos, NTerm);
      end;
    '/': begin
        Inc(SPos);
        if SPos^ = '*' then begin
          Inc(Spos);
          // Don't treat '/*!' or '/*+'(optimizer hints) comments as normal comments!!
          if (SPos < NTerm) then begin
            if (SPos^ <> '!')
            then Result.TokenType := ttComment
            else Result.TokenType := ttSymbol;
            GetMultiLineComment(SPos, NTerm);
          end else
            Dec(SPos);
        end else
          Dec(SPos);
      end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil)
  then Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer)
  else Result.L := SPos-Result.P+1;
end;

{ TZMySQLSymbolState }

{**
  Creates this MySQL-specific symbol state object.
}
constructor TZMySQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
  {BEGIN PATCH: added by fduenas}
  Add(':=');
  {END PATCH: added by fduenas}
end;

{ TZMySQLWordState }

{**
  Constructs this MySQL-specific word state object.
}
constructor TZMySQLWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
end;

{ TZMySQLTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZMySQLTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZMySQLSymbolState.Create;
  NumberState := TZMySQLNumberState.Create;
  QuoteState := TZMySQLQuoteState.Create;
  WordState := TZMySQLWordState.Create;
  CommentState := TZMySQLCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('#', '#', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{$ENDIF ZEOS_DISABLE_MYSQL}

end.

