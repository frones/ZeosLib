{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       String tokenizing classes for Generic SQL         }
{                                                         }
{       Originally written by Sergey Seroukhov            }
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

unit ZGenericSqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer, ZCompatibility;

type
  {**
    Implements a number state object.
    Depending on the FSupportsHex flag it could read hex values.
    It is base abstract class that shouldn't be used.
  }
  TZGenericBaseNumberState = class (TZNumberState)
  private
    FSupportsHex: Boolean;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  TZGenericSQLNoHexNumberState = class (TZGenericBaseNumberState)
  public
    constructor Create;
  end;

  TZGenericSQLHexNumberState = class (TZGenericBaseNumberState)
  public
    constructor Create;
  end;

  {** Implements a symbol state object. }
  TZGenericSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZGenericSQLWordState = class (TZWordState)
  public
    constructor Create;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a quote string state object. }
  TZGenericSQLQuoteState = class (TZQuoteState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeToken(const Value: TZToken; QuoteChar: Char): string; override;
  end;

  {** Implements a quote string state object.
    Quote chars:
      ' - string literals
      " and [] - identifiers
   }
  TZGenericSQLBracketQuoteState = class (TZQuoteState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeToken(const Value: TZToken; QuoteChar: Char): string; override;
  end;

  {** Implements a comment state object.
    Processes common SQL comments like -- and /* */
  }
  TZGenericSQLCommentState = class (TZCppCommentState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a default tokenizer object. }
  TZGenericSQLTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

{$IFDEF FAST_MOVE}uses ZFastCode;{$ENDIF}

{ TZGenericBaseNumberState }

function TZGenericBaseNumberState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;

  procedure ReadExp;
  begin
    Inc(SPos, Ord((SPos < NTerm) and ((Ord(SPos^) = Ord('-')) or (Ord(SPos^) = Ord('+')))));
    ReadDecDigits(SPos, NTerm)
  end;

var
  HexDecimal: Boolean;
  FloatPoint: Boolean;
  GotDecDigit: Boolean;
begin
  HexDecimal := False;
  FloatPoint := SPos^ = '.';
  GotDecDigit := False;

  Result.P := SPos;
  Result.TokenType := ttUnknown;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then begin
    GotDecDigit := ReadDecDigits(SPos, NTerm);
    if GotDecDigit then
      FloatPoint := SPos^= '.';
    if FloatPoint then
      Inc(SPos); //roll forward to dot
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then begin
    Inc(Spos, Ord(not GotDecDigit));
    GotDecDigit := ReadDecDigits(SPos, NTerm);
  end;

  { Reads a power part of the number }
  if GotDecDigit and ((Ord((SPos)^) or $20) = ord('e')) then //CharInSet(LastChar, ['e','E']) then
  begin
    Inc(SPos); //skip exponent
    FloatPoint := True;
    ReadExp;
  end;

  { Reads the hexadecimal number }
  if FSupportsHex and GotDecDigit and not FloatPoint then begin
    if (SPos-1 = Result.P) and ((SPos-1)^ = '0') and
      ((Byte(Ord((SPos)^)) or $20) = ord('x')) then //CharInSet(LastChar, ['x','X']) then
    begin
      Inc(SPos, 1);  //skip x
      HexDecimal := ReadHexDigits(Spos, NTerm);
    end;
  end;

  Dec(SPos); //push back wrong result
  { Prepare the result }
  if (SPos^ = '.') and (SPos = Result.P) then begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer);
  end else begin
    Result.L := SPos-Result.P+1;
    if HexDecimal then
      Result.TokenType := ttHexDecimal
    else if FloatPoint then
      Result.TokenType := ttFloat
    else
      Result.TokenType := ttInteger;
  end;
end;

{ TZGenericSQLNoHexNumberState }

constructor TZGenericSQLNoHexNumberState.Create;
begin
  inherited;
  FSupportsHex := False;
end;

{ TZGenericSQLHexNumberState }

constructor TZGenericSQLHexNumberState.Create;
begin
  inherited;
  FSupportsHex := True;
end;

{ TZGenericSQLSymbolState }

{**
  Creates this SQL-specific symbol state object.
}
constructor TZGenericSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZGenericSQLWordState }

{**
  Constructs this SQL-specific word state object.
}
constructor TZGenericSQLWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
end;

const
  {** List of keywords. }
  Keywords: array [0..8] of string = (
    'AND','OR','NOT','XOR','LIKE','IS','NULL','TRUE','FALSE'
  );

{**
  Gets a word tokens or special operators.
  @return a processed token.
}
function TZGenericSQLWordState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
var
  I: Integer;
begin
  Result := inherited NextToken(SPos, NTerm, Tokenizer);
  for I := Low(Keywords) to High(Keywords) do
    if Result.L = Length(Keywords[i]) then
      if SameText(Result.P, Pointer(Keywords[i]), Result.L) then begin
        Result.TokenType := ttKeyword;
        Break;
      end;
end;

{ TZGenericSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZGenericSQLQuoteState.NextToken(var SPos: PChar;
  const NTerm: PChar;Tokenizer: TZTokenizer): TZToken;
var
  LastChar: Char;
  ReadCounter, NumericCounter, TimeSepCount, DateSepCount, SpaceCount: integer;
  Tmp: String;
begin
  Result.P := SPos;
  LastChar := #0;
  TimeSepCount := 0;
  DateSepCount := 0;
  SpaceCount := 0;
  ReadCounter := 0;
  NumericCounter := 0;

  while SPos < NTerm do
  begin
    Inc(SPos);
    if (LastChar = Result.P^) and (SPos^ <> Result.P^) then begin
      Dec(SPos);
      Break;
    end;
    inc(TimeSepCount, Ord(SPos^ = {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator));
    inc(DateSepCount, Ord(SPos^ = {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator));
    inc(SpaceCount, Ord(SPos^ = ' '));
    inc(NumericCounter, Ord(Ord(SPos^) in [Ord('0')..Ord('9')]));
    Inc(ReadCounter);

    if (LastChar = SPos^) and (SPos^ = Result.P^)
    then LastChar := #0
    else LastChar := SPos^;
  end;

  if SPos^ = '"'
  then Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
  Result.L := SPos-Result.P+1;
  if (TimeSepCount = 2) and (DateSepCount = 0) and // test Time constant
    ((NumericCounter + TimeSepCount) = ReadCounter-1) then
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
    //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      Tmp := DecodeToken(Result, Result.P^);
      if StrToTimeDef(Tmp, 0) = 0 then
        Exit;
      Result.TokenType := ttTime;
    except end
  else if (TimeSepCount = 0) and (DateSepCount = 2) and // test Date constant
    ((NumericCounter + DateSepCount) = ReadCounter-1) then
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
      //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      Tmp := DecodeToken(Result, Result.P^);
      if StrToDateDef(Tmp, 0) = 0 then
        Exit;
      Result.TokenType := ttDate;
    except end
  else if (TimeSepCount = 2) and (DateSepCount = 2) and // test DateTime constant
    ((NumericCounter + TimeSepCount + DateSepCount + SpaceCount) = ReadCounter-1) then
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
      //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      Tmp := DecodeToken(Result, Result.P^);
      if StrToDateTimeDef(Tmp, 0) = 0 then
        Exit;
      Result.TokenType := ttDateTime;
    except end
end;

{**
  Decodes a string value.
  @param Value a token value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZGenericSQLQuoteState.DecodeToken(const Value: TZToken;
  QuoteChar: Char): string;
begin
  if (Value.L >= 2) and (Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]) and
    (Value.P^ = QuoteChar) and ((Value.P+Value.L-1)^ = QuoteChar) then
    if Value.L > 2
      then Result := SQLDequotedStr(Value.P, Value.L, QuoteChar)
      else Result := ''
    else SetString(Result, Value.P, Value.L);
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZGenericSQLQuoteState.EncodeString(const Value: string;
  QuoteChar: Char): string;
begin
  if Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]
  then Result := SQLQuotedStr(Value, QuoteChar)
  else Result := Value;
end;

{ TZGenericSQLCommentState }

function TZGenericSQLCommentState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
begin
  Result.P := SPos;
  Result.TokenType := ttUnknown;

  case SPos^ of
    '-': if SPos+1 < NTerm then begin
          Inc(SPos);
          if SPos^ = '-' then begin
            Result.TokenType := ttComment;
            GetSingleLineComment(SPos, NTerm);
          end else
            Dec(SPos);
        end;
    '/': if SPos+1 < NTerm then begin
          Inc(SPos);
          if SPos^ = '*' then
          begin
            Result.TokenType := ttComment;
            GetMultiLineComment(SPos, NTerm);
          end else
            Dec(SPos);
      end;
  end;
  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer)
  else
    Result.L := SPos-Result.P+1;
end;

{ TZGenericSQLBracketQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZGenericSQLBracketQuoteState.NextToken(var SPos: PChar;
  const NTerm: PChar; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.P := SPos;
  LastChar := #0;
  while SPos < NTerm do begin
    Inc(SPos);
    ReadChar := SPos^;
    if ((LastChar = Result.P^) and (ReadChar <> Result.P^) and (Result.P^ <> '[')) or
      ((Result.P^ = '[') and (LastChar = ']')) then
    begin
      Dec(SPos);
      Break;
    end;
    if (LastChar = Result.P^) and (ReadChar = Result.P^)
    then LastChar := #0
    else LastChar := ReadChar;
  end;
  case Result.P^ of
    '"', '[': Result.TokenType := ttWord  //?? shouldn't the '[' be ttQuoted or ttQuotedIdentifier too?
    else          Result.TokenType := ttQuoted;
  end;
   Result.L := SPos-Result.P+1;
end;

{**
  Decodes a string value.
  @param Value a token value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZGenericSQLBracketQuoteState.DecodeToken(const Value: TZToken;
  QuoteChar: Char): string;
begin
  if Value.L >= 2 then
    case QuoteChar of
      #39, '"', '`':
        if (Value.P^ = QuoteChar) and ((Value.P+Value.L-1)^ = QuoteChar) then
          if Value.L > 2
          then Result := SQLDequotedStr(Value.P, Value.L, QuoteChar)
          else Result := '';
      '[':
        if (Value.P^ = QuoteChar) and ((Value.P+Value.L-1)^ = ']') then
          if Value.L > 2
            then SetString(Result, Value.P+1, Value.L-2)
            else Result := '';
      else
        SetString(Result, Value.P, Value.L)
    end
  else if Value.L = 1
    then SetString(Result, Value.P, Value.L)
    else Result := '';
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZGenericSQLBracketQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  case QuoteChar of
    '[':      Result := '[' + Value + ']';
    #39, '"', '`': Result := QuoteChar + Value + QuoteChar;
    else      Result := Value;
  end;
end;

{ TZGenericSQLTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZGenericSQLTokenizer.CreateTokenStates;
begin
  NumberState := TZNumberState.Create;
  QuoteState := TZGenericSQLQuoteState.Create;
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

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

end.

