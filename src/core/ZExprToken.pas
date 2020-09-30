{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for Expressions        }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZExprToken;

interface

{$I ZCore.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer;

type

  {** Implements an Expression-specific number state object. }
  TZExpressionNumberState = class (TZNumberState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements an Expression-specific quote string state object. }
  TZExpressionQuoteState = class (TZQuoteState)
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
  TZExpressionCommentState = class (TZCppCommentState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZExpressionSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZExpressionWordState = class (TZWordState)
  public
    constructor Create;
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a default tokenizer object. }
  TZExpressionTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

uses ZCompatibility{$IFDEF FAST_MOVE},ZFastCode{$ENDIF};

const
  {** List of keywords. }
  Keywords: array [0..8] of string = (
    'AND','OR','NOT','XOR','LIKE','IS','NULL','TRUE','FALSE'
  );

{ TZExpressionNumberState }


{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZExpressionNumberState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
var
  FloatPoint: Boolean;
  GotDecDigit: Boolean;
begin
  FloatPoint := SPos^ = '.';
  GotDecDigit := False;
  Result.P := SPos;
  Result.TokenType := ttUnknown;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then begin
    GotDecDigit := ReadDecDigits(SPos, NTerm);
    if GotDecDigit then
      FloatPoint := (SPos)^ = '.';
    if FloatPoint then
      Inc(SPos);
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then begin
    Inc(SPos, ord(not GotDecDigit));
    GotDecDigit := ReadDecDigits(Spos, NTerm);
  end;

  { Reads a power part of the number }
  if GotDecDigit and (Ord((SPos)^) or $20 = Ord('e')) then begin  //charinset('E','e')
    Inc(SPos, Ord(SPos < NTerm));
    FloatPoint := True;
    if ((Ord(SPos^) >= Ord('0')) and (Ord(SPos^) <= Ord('9'))) or
        (Ord(SPos^) = Ord('-')) or (Ord(SPos^) = Ord('+')) then begin
      Inc(SPos, Ord((Ord(SPos^) = Ord('-')) or (Ord(SPos^) = Ord('+'))));
      ReadDecDigits(SPos, NTerm)
    end else Dec(SPos, 2);
  end;

  Dec(SPos); //push back wrong result
  { Prepare the result }
  if (SPos^ = '.') and (SPos = Result.P) then begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer);
  end else begin
    Result.L := SPos-Result.P+1;
    if FloatPoint
    then Result.TokenType := ttFloat
    else Result.TokenType := ttInteger;
  end;
end;

{ TZExpressionSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZExpressionQuoteState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
var
  LastChar: Char;
begin
  if SPos^ = '"'
  then Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
  Result.P := SPos;
  LastChar := #0;

  while SPos < NTerm do begin
    Inc(SPos);
    if (LastChar = Result.P^) and (SPos^ <> Result.P^) then begin //read over boundaries?
      Dec(SPos);
      Break;
    end;
    if LastChar = '\' then
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
function TZExpressionQuoteState.DecodeToken(const Value: TZToken;
  QuoteChar: Char): string;
begin
  if (Value.L >= 2) and ((QuoteChar= '''') or (QuoteChar= '"')) and
     (Value.P^ = QuoteChar) and ((Value.P+Value.L-1)^ = QuoteChar)
  then DecodeCString(Value.L-2, Value.P+1, {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString(Result){$ELSE}Result{$IFEND})
  else SetString(Result, Value.P, Value.L);
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZExpressionQuoteState.EncodeString(const Value: string;
  QuoteChar: Char): string;
begin
  if (Ord(QuoteChar) in [Ord(''''), Ord('"')]) then
    Result := QuoteChar + EncodeCString(Value) + QuoteChar
  else
    Result := Value;
end;

{ TZExpressionCommentState }

{**
  Gets an Expression specific comments like /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZExpressionCommentState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttUnknown;
  Result.P := SPos;

  if SPos^ = '/' then begin
    Inc(SPos);
    if (SPos < NTerm) and (SPos^ = '*') then begin
      Result.TokenType := ttComment;
      GetMultiLineComment(SPos, NTerm);
    end else if (SPos < NTerm) then
      Dec(SPos);
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil)
  then Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer)
  else Result.L := SPos-Result.P+1;
end;

{ TZExpressionSymbolState }

{**
  Creates this Expression-specific symbol state object.
}
constructor TZExpressionSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
end;

{ TZExpressionWordState }

{**
  Constructs this Expression-specific word state object.
}
constructor TZExpressionWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
end;

{**
  Gets a word tokens or special operators.
  @return a processed token.
}
function TZExpressionWordState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
var
  I: Integer;
begin
  Result := inherited NextToken(SPos, NTerm, Tokenizer);
  for I := Low(Keywords) to High(Keywords) do
    if Result.L = Length(Keywords[I]) then
      if SameText(Result.P, Pointer(Keywords[i]), Result.L) then begin
        Result.TokenType := ttKeyword;
        Break;
      end;
end;

{ TZExpressionTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZExpressionTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZExpressionSymbolState.Create;
  NumberState := TZExpressionNumberState.Create;
  QuoteState := TZExpressionQuoteState.Create;
  WordState := TZExpressionWordState.Create;
  CommentState := TZExpressionCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState('''', '''', QuoteState);

  SetCharacterState('/', '/', CommentState);
end;

end.


