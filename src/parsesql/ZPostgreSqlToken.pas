{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for PostgreSQL         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPostgreSqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZTokenizer, ZGenericSqlToken, ZMySqlToken;

type

  {** Implements a PostgreSQL-specific number state object. }
  TZPostgreSQLNumberState = class (TZNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a PostgreSQL-specific quote string state object. }
  TZPostgreSQLQuoteState = class (TZMySQLQuoteState)
  private
    FStandardConformingStrings: Boolean;
  protected
    function GetModifier(Stream: TStream; FirstChar: Char; ResetPosition: Boolean = True): String;
    procedure GetDollarQuotedString(Stream: TStream; QuoteChar: Char; var Result: String);
    procedure GetQuotedString(Stream: TStream; QuoteChar: Char; EscapeSyntax: Boolean; var Result: String);
    procedure GetQuotedStringWithModifier(Stream: TStream; FirstChar: Char; var Result: String);
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    procedure SetStandardConformingStrings(const Value: Boolean);
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZPostgreSQLCommentState = class (TZCppCommentState)
  protected
    procedure GetMultiLineComment(Stream: TStream; var Result: String); override;
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZPostgreSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZPostgreSQLWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  IZPostgreSQLTokenizer = interface (IZTokenizer)
    ['{82392175-9065-4048-9974-EE1253B921B4}']
    procedure SetStandardConformingStrings(const Value: Boolean);
  end;

  {** Implements a default tokenizer object. }
  TZPostgreSQLTokenizer = class (TZTokenizer, IZPostgreSQLTokenizer)
  protected
    procedure CreateTokenStates; override;
  public
    procedure SetStandardConformingStrings(const Value: Boolean);
  end;

implementation

uses ZCompatibility{$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

const
  NameQuoteChar   = Char('"');
  DollarQuoteChar = Char('$');
  SingleQuoteChar = Char('''');

{ TZPostgreSQLNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZPostgreSQLNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  FloatPoint: Boolean;
  LastChar: Char;

  procedure ReadDecDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
    begin
      if CharInSet(LastChar, ['0'..'9']) then begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end else begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
    end;
  end;

begin
  FloatPoint := FirstChar = '.';
  InitBuf(FirstChar);
  Result.Value := '';
  Result.TokenType := ttUnknown;
  LastChar := #0;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then
  begin
    ReadDecDigits;
    FloatPoint := LastChar = '.';
    if FloatPoint then
    begin
      Stream.Read(TempChar{%H-}, SizeOf(Char));
      ToBuf(TempChar, Result.Value);
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    ReadDecDigits;

  { Reads a power part of the number }
  if (Ord(LastChar) or $20) = ord('e') then //CharInSet(LastChar, ['e','E']) then
  begin
    Stream.Read(TempChar, SizeOf(Char));
    ToBuf(TempChar, Result.Value);
    FloatPoint := True;

    Stream.Read(TempChar, SizeOf(Char));
    if CharInSet(TempChar, ['0'..'9','-','+']) then begin
      ToBuf(TempChar, Result.Value);
      ReadDecDigits;
    end else begin
      FlushBuf(Result.Value);
      Result.Value := Copy(Result.Value, 1, Length(Result.Value) - 1);
      Stream.Seek(-2*SizeOf(Char), soFromCurrent);
    end;
  end;
  FlushBuf(Result.Value);

  { Prepare the result }
  if Result.Value = '.' then
  begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end else begin
    if FloatPoint then
      Result.TokenType := ttFloat
    else Result.TokenType := ttInteger;
  end;
end;

{ TZPostgreSQLQuoteState }

{**
  Retrieves string modifier from quoted string.
  @return a string with modifier for valid quoted string with modifier
  or empty string otherwise.
}
function TZPostgreSQLQuoteState.GetModifier(Stream: TStream;
  FirstChar: Char; ResetPosition: boolean = True): String;
var
  ReadChar: Char;
  Modifier: string;
  ReadNum: Integer;
begin
  Result := '';
  if CharInSet(FirstChar, ['E', 'e', 'B', 'b', 'X', 'x', 'U', 'u']) then
  begin
    Modifier := FirstChar;
    ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
    if ReadNum = SizeOf(Char) then begin
      if (UpCase(FirstChar) = 'U') and (ReadChar = '&') then // Check for U& modifier
      begin
        Modifier := Modifier + ReadChar;
        ReadNum := ReadNum + Stream.Read(ReadChar, SizeOf(Char));
      end;
      if (ReadChar = SingleQuoteChar) then
         Result := Modifier;
      if ResetPosition then
        Stream.Seek(-ReadNum, soFromCurrent);
    end;
  end;
end;

{**
  Returns a quoted string token from a reader. This method
  will get Tag from first char to QuoteChar and will collect
  characters until reaches same Tag.

  @return a quoted string token from a reader
}
procedure TZPostgreSQLQuoteState.GetDollarQuotedString(Stream: TStream;
  QuoteChar: Char; var Result: String);
var
  ReadChar: Char;
  Tag, TempTag: string;
  TagState: integer;
begin
  Result := '';
  InitBuf(QuoteChar);
  TagState := 0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if (ReadChar = QuoteChar) then
    begin
      if (TagState = 0) then begin
        TagState := 1;
        FlushBuf(Result);
        Tag := Result;
      end else if (TagState = 1) then
      begin
        TagState := 2;
        TempTag := '';
      end else if (TagState = 2) then
        if TempTag = Tag then
          TagState := 3
        else
          TempTag := '';
    end;
    ToBuf(ReadChar, Result);

    if TagState = 2 then
      TempTag := TempTag + ReadChar
    else if TagState = 3 then
      Break;
  end;
  FlushBuf(Result);
end;

{**
  Returns a quoted string token from a reader. This method
  will collect characters until it sees same QuoteChar,
  ommitting doubled chars

  @return a quoted string token from a reader
}
procedure TZPostgreSQLQuoteState.GetQuotedString(Stream: TStream; QuoteChar: Char;
  EscapeSyntax: Boolean; var Result: String);
const BackSlash = Char('\');
var
  ReadChar: Char;
  LastChar: Char;
  QuoteCount: Integer;
  LastWasEscapeChar: Boolean;
begin
  LastChar := #0;
  Result := '';
  InitBuf(QuoteChar);
  QuoteCount := 1;

  LastWasEscapeChar := False;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if ReadChar = QuoteChar then
      Inc(QuoteCount, Ord((not EscapeSyntax) or (not LastWasEscapeChar)))
    else
      LastWasEscapeChar :=(ReadChar=BackSlash) and (not LastWasEscapeChar); //False; //Kamil Giza comment False;

    if (LastChar = QuoteChar) and (ReadChar <> QuoteChar) then
      if QuoteCount mod 2 = 0 then begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
    ToBuf(ReadChar, Result);
    if (LastChar = BackSlash) and EscapeSyntax then begin
      LastChar := #0;
      //LastWasEscapeChar := True; //Kamil Giza add comment
      //Dec(QuoteCount); nope that doesnt' work @all see the tests
    end else if (LastChar = QuoteChar) and (ReadChar = QuoteChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result);
end;

{**
  Returns a quoted string token with leading modifier from a reader.
  @return a quoted string token from a reader
}
procedure TZPostgreSQLQuoteState.GetQuotedStringWithModifier(Stream: TStream;
  FirstChar: Char; var Result: String);
var
  Modifier: string;
begin
  Modifier := GetModifier(Stream, FirstChar, False);
  if (Modifier <> '') then
    FirstChar := SingleQuoteChar;
  GetQuotedString(Stream, FirstChar, (not FStandardConformingStrings and (Modifier = '')) or
    ((Modifier <> '') and (UpCase(Modifier[1]) = 'E')), Result);
  if (Modifier <> '') then
    Result := Modifier + Result;
end;

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZPostgreSQLQuoteState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
begin
  if FirstChar = NameQuoteChar then begin
    Result.TokenType := ttWord;
    GetQuotedString(Stream, FirstChar, False, Result.Value);
  end else if FirstChar = DollarQuoteChar then begin
    Result.TokenType := ttQuoted;
    GetDollarQuotedString(Stream, FirstChar, Result.Value);
  end else begin
    Result.TokenType := ttQuoted;
    GetQuotedStringWithModifier(Stream, FirstChar, Result.Value);
  end;
end;

{**
  Sets how backslashes in quoted strings are handled
  @param True means backslashes are escape characters
}
procedure TZPostgreSQLQuoteState.SetStandardConformingStrings(const Value:
    Boolean);
begin
  FStandardConformingStrings := Value;
end;

{ TZPostgreSQLCommentState }

{**
  Ignore everything up to a last closing star and slash, and
  then return the tokenizer's next token.
  @return the tokenizer's next token
}
procedure TZPostgreSQLCommentState.GetMultiLineComment(Stream: TStream; var Result: string);
var
  ReadChar, LastChar: Char;
  NestedLevel: Integer;
begin
  LastChar := #0;
  NestedLevel := 1;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    ToBuf(ReadChar, Result);
    if (LastChar = '*') and (ReadChar = '/') then
    begin
      Dec(NestedLevel);
      if NestedLevel = 0 then
        Break;
    end;
    if (LastChar = '/') and (ReadChar = '*') then
      Inc(NestedLevel);
    LastChar := ReadChar;
  end;
end;

{**
  Gets a PostgreSQL specific comments like -- or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZPostgreSQLCommentState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  Result.TokenType := ttUnknown;
  InitBuf(FirstChar);
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
  informs the Postgre Tokenizer '\' should be handled as Escape-char
  @param True means backslashes are quoted strings
}
procedure TZPostgreSQLTokenizer.SetStandardConformingStrings(
  const Value: Boolean);
begin
  (QuoteState as TZPostgreSQLQuoteState).SetStandardConformingStrings(Value);
end;

{**
  Constructs a default state table (as described in the class comment).
}
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
  SetCharacterState(DollarQuoteChar, DollarQuoteChar, QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

end.

