{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for PostgreSQL         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPostgreSqlToken;

interface

{$I ZParseSql.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZTokenizer, ZGenericSqlToken, ZMySqlToken;

type

  {** Implements a PostgreSQL-specific number state object. }
  TZPostgreSQLNumberState = TZGenericSQLNoHexNumberState;

  {** Implements a PostgreSQL-specific quote string state object. }
  TZPostgreSQLQuoteState = class (TZMySQLQuoteState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZPostgreSQLCommentState = class (TZGenericSQLCommentState)
  protected
    procedure GetMultiLineComment(var SPos: PChar; const NTerm: PChar); override;
  end;

  {** Implements a symbol state object. }
  TZPostgreSQLSymbolState = class (TZSymbolState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
    constructor Create;
  end;

  {** Implements a word state object. }
  TZPostgreSQLWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZPostgreSQLTokenizer = class (TZTokenizer)
  private
    FNormalizedParams: TStrings;
  protected
    procedure CreateTokenStates; override;
  public
    function NormalizeParamToken(const Token: TZToken; out ParamName: String): String; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}

implementation

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses ZCompatibility, ZFastCode;

const
  NameQuoteChar   = Char('"');
  SingleQuoteChar = Char('''');

{ TZPostgreSQLQuoteState }

{**
  Retrieves string modifier from quoted string.
  @return a string with modifier for valid quoted string with modifier
  or empty string otherwise.
}
function TZPostgreSQLQuoteState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
const BackSlash = Char('\');
var
  LastChar: Char;
  QuoteCount: Integer;
  LastWasEscapeChar: Boolean;
begin
  LastChar := #0;
  Result.P := SPos;
  if SPos^ = NameQuoteChar
  then Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
  QuoteCount := 1;

  LastWasEscapeChar := False;
  while SPos < NTerm do begin
    Inc(SPos);
    if SPos^ = Result.P^
    then Inc(QuoteCount)
    else LastWasEscapeChar := (SPos^=BackSlash) and (not LastWasEscapeChar); //False; //Kamil Giza comment False;

    if (LastChar = Result.P^) and (SPos^ <> Result.P^) then
      if QuoteCount mod 2 = 0 then begin
        Dec(SPos);
        Break;
      end;
    if (SPos^ = BackSlash) then
      LastChar := #0
      //LastWasEscapeChar := True; //Kamil Giza add comment
      //Dec(QuoteCount); nope that doesnt' work @all see the tests
    else if (LastChar = Result.P^) and (SPos^ = Result.P^) then
      LastChar := #0
    else
      LastChar := SPos^;
  end;
   Result.L := SPos-Result.P+1;
end;

{ TZPostgreSQLCommentState }

{**
  Ignore everything up to a last closing star and slash, and
  then return the tokenizer's next token.
  @return the tokenizer's next token
}
procedure TZPostgreSQLCommentState.GetMultiLineComment(var SPos: PChar;
  const NTerm: PChar);
var
  LastChar: Char;
  NestedLevel: Integer;
begin
  LastChar := #0;
  NestedLevel := 1;
  while SPos < NTerm do begin
    Inc(SPos);
    if (LastChar = '*') and (Spos^ = '/') then
    begin
      Dec(NestedLevel);
      if NestedLevel = 0 then
        Break;
    end
    else
    if (LastChar = '/') and (Spos^ = '*') then
      Inc(NestedLevel);
    LastChar := SPos^;
  end;
end;

{ TZPostgreSQLSymbolState }

{**
  Creates this PostgreSQL-specific symbol state object.
}
constructor TZPostgreSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
  Add('~*');
  Add('!~');
  Add('!~*');
  Add('::'); //parameter type marker such as 1::BIGINT
end;

function TZPostgreSQLSymbolState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
var
  DollarCount, BodyTagLen: Integer;
  TempTag: PChar;
begin
  Result := inherited NextToken(SPos, NTerm, Tokenizer);
  //detecting Postgre Parameters as one ttWordState:
  if (Result.P^ = '$') and (SPos < NTerm) then begin
    //detect body tags as ttQuoted
    //eg. $body$ .... $body$ or $$ .... $$
    DollarCount := 1;
    BodyTagLen := 1;
    TempTag := nil;
    while (SPos < NTerm) do begin
      Inc(SPos);
      if SPos^ = '$' then begin
        Inc(DollarCount);
        if DollarCount = 2 then
          BodyTagLen := SPos-Result.P
        else if DollarCount = 3 then
          TempTag := Spos
        else if (DollarCount = 4) then
          if ((Spos - TempTag) = BodyTagLen) and CompareMem(Result.P, TempTag, BodyTagLen*SizeOf(Char)) then begin
            Result.L := (Spos - Result.P)+1;
            Result.TokenType := ttQuoted;
            Exit;
          end else begin // $body$ .... $1, $2  .... $body$
            DollarCount := 3;
            TempTag := Spos;
          end;
      end;
    end;
    SPos := Result.P + Result.L-1;
  end;
end;

{ TZPostgreSQLWordState }

{**
  Constructs this PostgreSQL-specific word state object.
}
constructor TZPostgreSQLWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars('$', '$', True);
end;

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZPostgreSQLTokenizer.AfterConstruction;
begin
  inherited;
  FNormalizedParams := TStringList.Create
end;

procedure TZPostgreSQLTokenizer.BeforeDestruction;
begin
  inherited;
  FNormalizedParams.Free;
end;

procedure TZPostgreSQLTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZPostgreSQLSymbolState.Create;
  NumberState := TZPostgreSQLNumberState.Create;
  QuoteState := TZPostgreSQLQuoteState.Create;
  WordState := TZPostgreSQLWordState.Create;
  CommentState := TZPostgreSQLCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState(NameQuoteChar, NameQuoteChar, QuoteState);
  SetCharacterState(SingleQuoteChar, SingleQuoteChar, QuoteState);
 // SetCharacterState(DollarQuoteChar, DollarQuoteChar, QuoteState);
  //SetCharacterState(DollarQuoteChar, DollarQuoteChar, WordState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

function TZPostgreSQLTokenizer.NormalizeParamToken(const Token: TZToken;
  out ParamName: String): String;
var
  P: PChar;
  I: Integer;
  C: Cardinal;
  B: Byte;
label fill;
begin
  {postgres just understands numerical tokens only at least unti V12}
  if Token.TokenType = ttInteger then begin
    System.SetString(ParamName, Token.P, Token.L);
    System.SetString(Result, nil, Token.L+1);
    P := Pointer(Result);
    P^ := '$';
    Move(Token.P^, (P+1)^, Token.L*SizeOf(Char));
  end else begin
    if (Token.L >= 2) and (Ord(Token.P^) in [Ord(#39), Ord('`'), Ord('"'), Ord('[')])
    then ParamName := GetQuoteState.DecodeToken(Token, Token.P^)
    else System.SetString(ParamName, Token.P, Token.L);
    I := FNormalizedParams.IndexOf(ParamName);
    if I = -1 then begin
      C := FNormalizedParams.Count+1;
      FNormalizedParams.Add(ParamName);
      goto fill;
    end else begin
      C := I+1;
fill: B := GetOrdinalDigits(C);
      SetLength(Result, B+1);
      P := Pointer(Result);
      P^ := '$';
      {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(C, P+1, B);
    end;
  end;
end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}

end.

