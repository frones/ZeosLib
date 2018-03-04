{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           String tokenizing classes for MySQL           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZMySqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type

  {** Implements a MySQL-specific number state object. }
  TZMySQLNumberState = class (TZNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a MySQL-specific quote string state object. }
  TZMySQLQuoteState = class (TZQuoteState)
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
  TZMySQLCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
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

implementation

{$IFDEF FAST_MOVE}
uses ZFastCode;
{$ENDIF}

{ TZMySQLNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZMySQLNumberState.NextToken(Stream: TStream; FirstChar: Char;
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
      end
      else
      begin
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
  Result.Value := '';
  InitBuf(FirstChar);
  Result.TokenType := ttUnknown;
  LastChar := #0;

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
  if (Result.Value = '') and (FirstChar = '0') and ((Ord(LastChar) or $20) = ord('x')) then // CharInSet(LastChar, ['x','X']) then
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

{ TZMySQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZMySQLQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
const BackSlash = Char('\');
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := '';
  InitBuf(FirstChar);

  If FirstChar = '`' then
    Result.TokenType := ttQuotedIdentifier
  else
    Result.TokenType := ttQuoted;

  LastChar := #0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    ToBuf(ReadChar, Result.Value);
    if LastChar = BackSlash then
      LastChar := #0
    else if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZMySQLQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if CharInSet(QuoteChar, [#39, '"', '`']) then
    Result := QuoteChar + EncodeCString(Value) + QuoteChar
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZMySQLQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
var
  Len: Integer;
begin
  Len := Length(Value);
  if (Len >= 2) and CharInSet(QuoteChar, [#39, '"', '`'])
    and (Value[1] = QuoteChar) and (Value[Len] = QuoteChar) then
  begin
    if Len > 2 then
      Result := DecodeCString(Copy(Value, 2, Len - 2))
    else Result := '';
  end
  else Result := Value;
end;

{ TZMySQLCommentState }

{**
  Gets a MySQL specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZMySQLCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum, ReadNum2: Integer;
begin
  Result.TokenType := ttUnknown;
  InitBuf(Firstchar);
  Result.Value := '';

  if FirstChar = '-' then begin
    ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '-') then begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetSingleLineComment(Stream, Result.Value);
    end else begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end else if FirstChar = '#' then begin
    Result.TokenType := ttComment;
    GetSingleLineComment(Stream, Result.Value);
  end else if FirstChar = '/' then begin
    ReadNum := Stream.Read(ReadChar, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '*') then
    begin
      ToBuf(ReadChar, Result.Value);
      ReadNum2 := Stream.Read(ReadChar, SizeOf(Char));
      // Don't treat '/*!' comments as normal comments!!
      if (ReadNum2 > 0) then begin
        ToBuf(ReadChar, Result.Value);
        if (ReadChar <> '!') then
          Result.TokenType := ttComment
        else
          Result.TokenType := ttSymbol;
        GetMultiLineComment(Stream, Result.Value);
      end;
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else
    FlushBuf(Result.Value);
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

end.

