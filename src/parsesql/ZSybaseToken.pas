{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          String tokenizing classes for Sybase           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZTokenizer, ZCompatibility, ZGenericSqlToken;

type

  {** Implements a Sybase-specific number state object. }
  TZSybaseNumberState = class (TZNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a Sybase-specific quote string state object. }
  TZSybaseQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeString(const Value: string; QuoteChar: Char): string; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZSybaseCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZSybaseSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZSybaseWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZSybaseTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

{$IFDEF FAST_MOVE}
uses ZFastCode;
{$ENDIF}

{ TZSybaseNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZSybaseNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  HexDecimal: Boolean;
  FloatPoint: Boolean;
  LastChar: Char;

  procedure ReadHexDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
      if CharInSet(LastChar, ['0'..'9','a'..'f','A'..'F']) then begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end else begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
  end;

  procedure ReadDecDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
      if CharInSet(LastChar, ['0'..'9']) then begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end else begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
  end;

begin
  HexDecimal := False;
  FloatPoint := FirstChar = '.';
  LastChar := #0;

  Result.Value := '';
  InitBuf(FirstChar);
  Result.TokenType := ttUnknown;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then
  begin
    ReadDecDigits;
    FloatPoint := (LastChar = '.');
    if FloatPoint then
    begin
      Stream.Read(LastChar, SizeOf(Char));
      ToBuf(LastChar, Result.Value);
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    ReadDecDigits;

  { Reads a power part of the number }
  if (Ord(LastChar) or $20) = ord('e') then //CharInSet(LastChar, ['e','E']) then
  begin
    Stream.Read(LastChar, SizeOf(Char));
    ToBuf(LastChar, Result.Value);
    FloatPoint := True;

    Stream.Read(LastChar, SizeOf(Char));
    if CharInSet(LastChar, ['0'..'9','-','+']) then begin
      ToBuf(LastChar, Result.Value);
      ReadDecDigits;
    end else begin
      FlushBuf(Result.Value);
      Result.Value := Copy(Result.Value, 1, Length(Result.Value) - 1);
      Stream.Seek(-2*SizeOf(Char), soFromCurrent);
    end;
  end;

  { Reads the nexdecimal number }
  if (Result.Value = '') and (FirstChar = '0') and ((Ord(LastChar) or $20) = ord('x')) then //CharInSet(LastChar, ['x','X']) then
  begin
    Stream.Read(LastChar, SizeOf(Char));
    ToBuf(LastChar, Result.Value);
    ReadHexDigits;
    HexDecimal := True;
  end;
  FlushBuf(Result.Value);

  { Prepare the result }
  if Result.Value = '.' then begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end else if HexDecimal then
    Result.TokenType := ttHexDecimal
  else if FloatPoint then
    Result.TokenType := ttFloat
  else Result.TokenType := ttInteger;
end;

{ TZSybaseQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZSybaseQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := '';
  InitBuf(FirstChar);
  LastChar := #0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if ((LastChar = FirstChar) and (ReadChar <> FirstChar)
      and (FirstChar <> '[')) or ((FirstChar = '[') and (LastChar = ']')) then
    begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    ToBuf(ReadChar, Result.Value);
    if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);

  if CharInSet(FirstChar, ['"', '[']) then
    Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZSybaseQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if QuoteChar = '[' then
    Result := '[' + Value + ']'
  else if CharInSet(QuoteChar, [#39, '"']) then
    Result := QuoteChar + Value + QuoteChar
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZSybaseQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
begin
  Result := Value;
  if Length(Value) >= 2 then
  begin
    if CharInSet(QuoteChar, [#39, '"']) and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = QuoteChar) then
    begin
      if Length(Value) > 2 then
        Result := AnsiDequotedStr(Value, QuoteChar)
      else Result := '';
    end
    else if (QuoteChar = '[') and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = ']') then
      Result := Copy(Value, 2, Length(Value) - 2)
  end;
end;

{ TZSybaseCommentState }

{**
  Gets a Sybase specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZSybaseCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  InitBuf(FirstChar);
  Result.Value := '';
  Result.TokenType := ttUnknown;

  if FirstChar = '-' then
  begin
    ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '-') then begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetSingleLineComment(Stream, Result.Value);
    end else begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end else if FirstChar = '/' then begin
    ReadNum := Stream.Read(ReadChar, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '*') then begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetMultiLineComment(Stream, Result.Value);
    end else begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else
    FlushBuf(Result.Value);
end;

{ TZSybaseSymbolState }

{**
  Creates this Sybase-specific symbol state object.
}
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

{**
  Constructs this Sybase-specific word state object.
}
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

{**
  Constructs a default state table (as described in the class comment).
}
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

end.


