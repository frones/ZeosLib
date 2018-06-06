{*********************************************************}
{                                                         }
{                     Zeos SQL Shell                      }
{                 Script Parsing Classes                  }
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

unit ZScriptParser;

interface

{$I ZParseSql.inc}

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZTokenizer;

type
  {** Defines a SQL delimiter type. }
  TZDelimiterType = (dtDefault, dtDelimiter, dtGo, dtSetTerm, dtEmptyLine);

  {** Implements a SQL script parser. }
  TZSQLScriptParser = class
  private
    FDelimiter{$IFDEF PCHAR_KUNGFU}, FParsedText{$ENDIF}: string;
    FDelimiterType: TZDelimiterType;
    FCleanupStatements: Boolean;
    FTokenizer: IZTokenizer;
    FUncompletedStatement: string;
    FStatements: TStrings;

    function GetStatementCount: Integer;
    function GetStatement(Index: Integer): string;

  public
    constructor Create;
    constructor CreateWithTokenizer(const Tokenizer: IZTokenizer);
    destructor Destroy; override;

    procedure Clear;
    procedure ClearCompleted;
    procedure ClearUncompleted;

    procedure ParseText(const Text: string);
    procedure ParseLine(const Line: string);

    property Delimiter: string read FDelimiter write FDelimiter;
    property DelimiterType: TZDelimiterType read FDelimiterType
      write FDelimiterType default dtDefault;
    property CleanupStatements: Boolean read FCleanupStatements
      write FCleanupStatements default True;
    property Tokenizer: IZTokenizer read FTokenizer write FTokenizer;
    property UncompletedStatement: string read FUncompletedStatement;
    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: string read GetStatement;
  end;

implementation

uses ZMessages, ZSysUtils, ZFastCode;

{ TZSQLScriptParser }

{**
  Constructs this script parser class.
}
constructor TZSQLScriptParser.Create;
begin
  FStatements := TStringList.Create;
  FDelimiter := ';';
  FDelimiterType := dtDefault;
  FCleanupStatements := True;
end;

{**
  Creates this object and assignes a tokenizer object.
  @param Tokenizer a tokenizer object.
}
constructor TZSQLScriptParser.CreateWithTokenizer(const Tokenizer: IZTokenizer);
begin
  Create;
  FTokenizer := Tokenizer;
end;

{**
  Destroys this class and cleanups the memory.
}
destructor TZSQLScriptParser.Destroy;
begin
  FreeAndNil(FStatements);
  FTokenizer := nil;
  inherited Destroy;
end;

{**
  Gets SQL statements number.
  @returns SQL statements number.
}
function TZSQLScriptParser.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

{**
  Gets a parsed SQL statement by it's index.
  @param Index a statement index.
  @returns a SQL statement string.
}
function TZSQLScriptParser.GetStatement(Index: Integer): string;
begin
  Result := FStatements[Index];
end;

{**
  Clears all completed and uncompleted statements and line delimiter.
}
procedure TZSQLScriptParser.Clear;
begin
  FStatements.Clear;
  FDelimiter := ';';
  FUncompletedStatement := '';
end;

{**
  Clears only completed statements.
}
procedure TZSQLScriptParser.ClearCompleted;
begin
  FStatements.Clear;
end;

{**
  Clears completed and uncompleted statements.
}
procedure TZSQLScriptParser.ClearUncompleted;
begin
  FStatements.Clear;
  FUncompletedStatement := '';
end;

{**
  Parses incrementaly only one single line.
  The line appends with EOL character.
  @param Line a line to be parsed.
}
procedure TZSQLScriptParser.ParseLine(const Line: string);
begin
  ParseText(#10 + Line + #10);
end;

{**
  Parses a complete text with several lines.
  @oaram Text a text of the SQL script to be parsed.
}
procedure TZSQLScriptParser.ParseText(const Text: string);
const SetTerm = String('SET TERM ');
{$IFNDEF PCHAR_KUNGFU}
var
  Tokens: TZTokenList;
  TokenType: TZTokenType;
  TokenValue: string;
  TokenIndex, iPos: Integer;
  SQL, Temp: string;
  EndOfStatement: Boolean;
  Extract: Boolean;
  LastComment: String;

  function CountChars(const Str: string; Chr: Char): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(Str) do
      if Str[I] = Chr then
        Inc(Result);
  end;

  procedure SetNextToken;
  begin
    TokenValue := Tokens.AsString(TokenIndex);
    TokenType := Tokens[TokenIndex]^.TokenType;
    Inc(TokenIndex);
  end;

begin
  if Tokenizer = nil then
    raise Exception.Create(STokenizerIsNotDefined);

  if CleanupStatements
  then Tokens := Tokenizer.TokenizeBufferToList(Text, [toSkipComments])
  else Tokens := Tokenizer.TokenizeBufferToList(Text, []);

  if ( (DelimiterType = dtDelimiter) or
       (DelimiterType = dtSetTerm) ) and
     ( Delimiter = '' ) then
    Delimiter := ';'; //use default delimiter

  if (DelimiterType = dtDefault) then
    Delimiter := ';'; //use default delimiter

  TokenIndex := 0;
  SQL := FUncompletedStatement;
  if SQL <> '' then
    if CleanupStatements
    then SQL := SQL + ' '
    else SQL := SQL + #10;
  FUncompletedStatement := '';
  FStatements.Clear;
  try
    repeat
      SetNextToken;

      case DelimiterType of
        dtGo:
          EndOfStatement := (UpperCase(TokenValue) = 'GO');
        dtEmptyLine:
          begin
            EndOfStatement := False;
            if TokenType = ttWhitespace then begin
              Temp := TokenValue;
              while (CountChars(Temp, #10) < 2) and (TokenType = ttWhitespace) do begin
                SetNextToken;
                if TokenType = ttWhitespace then
                  Temp := Temp + TokenValue;
              end;
              EndOfStatement := (TokenType = ttWhitespace) or EndsWith(Sql, #10);
              if not EndOfStatement then
                if SQL <> '' then
                  SQL := Trim(SQL) + ' ';
            end;
          end;
        dtDelimiter,
        dtDefault,
        dtSetTerm:
          begin
            EndOfStatement := False;
            if not (TokenType in [ttWhitespace, ttEOF]) then
            begin
              if (DelimiterType = dtDelimiter) and (Uppercase(TokenValue) = 'DELIMITER') then
              begin
                Delimiter := '';
                Temp := TokenValue; {process the DELIMITER}
                Temp := Temp + Tokens.AsString(TokenIndex); {process the first ' ' char}
                Inc(TokenIndex);
                while TokenType <> ttWhitespace do begin
                  SetNextToken;
                  if not (TokenType in [ttWhitespace, ttEOF]) then
                    Delimiter := Delimiter + TokenValue; //get the new delimiter
                end;
                SQL := SQL + Temp + Delimiter;
                EndOfStatement := True;
              end
              else
              begin
                Temp := TokenValue;
                Extract := True;
                while (Delimiter[1]=Temp[1]) and
                      (Length(Delimiter) > Length(Temp))
                       and not (TokenType in [ttWhitespace, ttEOF]) do
                begin
                  SetNextToken;

                  if not (TokenType in [ttWhitespace, ttEOF]) then
                  begin
                    Temp := Temp + TokenValue;
                    Extract := True;
                  end else
                    Extract := False;
                end;
                EndOfStatement := (Delimiter = Temp);
                if not EndOfStatement then
                begin
                  if Extract then
                    Temp := Copy(Temp, 1, Length(Temp) - Length(TokenValue));
                  SQL := SQL + Temp;
                end;
              end;
            end;
          end;
        else
          EndOfStatement := False;
      end;

      if TokenType = ttEOF then Break;

      { Processes the end of statements. }
      if EndOfStatement then
      begin
        if CleanupStatements then
          SQL := Trim(SQL);
        if SQL <> '' then
        begin
          if not CleanupStatements
          then Temp := Trim(SQL)
          else Temp := SQL;
          if (DelimiterType = dtSetTerm) and StartsWith(UpperCase(Temp), SetTerm) then
              Delimiter := Copy(Temp, 10, Length(Temp) - 9)
          else if (DelimiterType = dtSetTerm) and ( ZFastCode.Pos(SetTerm, UpperCase(Temp)) > 0) then begin
            iPos := ZFastCode.Pos(SetTerm, UpperCase(Temp))+8;
            Delimiter := Copy(Temp, iPos+1, Length(Temp) - iPos);
            LastComment := TrimRight(Copy(Temp, 1, iPos-9));
          end else if (DelimiterType = dtDelimiter) and StartsWith(UpperCase(Temp), 'DELIMITER ') then
            Delimiter := Copy(Temp, 11, Length(Temp) - 10)
          else begin
            if (DelimiterType = dtEmptyLine) and EndsWith(SQL, ';') then
              SQL := Copy(SQL, 1, Length(SQL) - 1);
            if LastComment <> '' then
              SQL := LastComment+#13#10+SQL;
            if CleanupStatements then
              SQL := Trim(SQL);
            FStatements.Add(SQL);
            LastComment := '';
          end;
        end;
        SQL := '';
      { Adds a whitespace token. }
      end else if CleanupStatements and (TokenType = ttWhitespace) then begin
        if SQL <> '' then
          SQL := Trim(SQL) + ' ';
      { Adds a default token. }
      end else begin
        // --> ms, 20/10/2005
        // TokenValue is not a ttWhitespace (#32)
        if (TokenType = ttWhitespace) and (TokenValue > '') then begin
          // SQL is not emtyp
          if (SQL <> '') then begin
            // is last token:
            if (Tokenindex = Tokens.count-1) then
              TokenValue := '';
            // next(!) token is also ttWhitespace or delimiter
            // (TokenIndex was already incremented!)
            if (Tokenindex < Tokens.Count-1) then
              if (Tokens[TokenIndex]^.TokenType = ttWhitespace) or Tokens.IsEqual(TokenIndex, Delimiter) then
                TokenValue := '';
          end else // SQL is empty
            TokenValue := '';
        end;
        if ((SQL = '') and (trim(TokenValue) = '')) then
          TokenValue := '';
        // <-- ms
        SQL := SQL + TokenValue;
      end;
    until TokenType = ttEOF;
    if ( LastComment <> '' ) and ( FStatements.Count > 0) then
      if CleanupStatements then
        FStatements[FStatements.Count-1] := FStatements[FStatements.Count-1]+' '+Trim(LastComment)
      else
        FStatements[FStatements.Count-1] := FStatements[FStatements.Count-1]+#13#10+LastComment;
  finally
    Tokens.Free;
  end;

  if CleanupStatements then
    SQL := Trim(SQL);
  if SQL <> '' then
    FUncompletedStatement := SQL;
{$ELSE}
var
  Tokens: TZTokenList;
  Token: PZToken;
  StartTokenIndex, TokenIndex, iPos, N, D: Integer;
  SQL, Temp: string;
  EndOfStatement: Boolean;
  LastComment: String;
  P: PChar;

  function CountChars(Token: PZToken; Chr: Char): Integer;
  var I: Cardinal;
  begin
    Result := 0;
    for i := 0 to Token.L do
      Inc(Result, Ord((Token.P+i)^ = Chr));
  end;

  procedure SetNextToken;
  begin
    Token := Tokens[TokenIndex];
    Inc(TokenIndex);
  end;

begin
  if Tokenizer = nil then
    raise Exception.Create(STokenizerIsNotDefined);
  FParsedText := Trim(Text);
  if CleanupStatements
  then Tokens := Tokenizer.TokenizeBufferToList(FParsedText, [toSkipComments])
  else Tokens := Tokenizer.TokenizeBufferToList(FParsedText, []);

  if ( (DelimiterType = dtDelimiter) or
       (DelimiterType = dtSetTerm) ) and
     ( Delimiter = '' ) then
    Delimiter := ';'; //use default delimiter

  if (DelimiterType = dtDefault) then
    Delimiter := ';'; //use default delimiter

  TokenIndex := 0;
  StartTokenIndex := 0;
  SQL := FUncompletedStatement;
  if SQL <> '' then
    if CleanupStatements
    then SQL := SQL + ' '
    else SQL := SQL + #10;
  FUncompletedStatement := '';
  FStatements.Clear;
  try
    repeat
      iPos := 0;
      SetNextToken;

      case DelimiterType of
        dtGo: begin
                if Token.TokenType = ttWord then begin
                  EndOfStatement := Tokens.IsEqual(TokenIndex-1, 'GO', tcInsensitive);
                  if EndOfStatement then
                    Inc(iPos, 2);
                end else
                  EndOfStatement := False;
              end;
        dtEmptyLine:
          begin
            EndOfStatement := False;
            if Token.TokenType = ttWhitespace then begin
              N := TokenIndex-1; //reminder
              D := 0;
              while (Token.TokenType = ttWhitespace) and (D < 2) do begin
                Inc(D, CountChars(Token, #10));
                if (D>=2) then
                  Break;
                SetNextToken;
              end;
              EndOfStatement := (Token.TokenType = ttWhitespace) and (D >= 2);
              if EndOfStatement then begin
                iPos := (TokenIndex-N)+1;
              end else begin
                Tokens[N].P := pSpace;
                Tokens[N].L := 1;
              end;
            end;
          end;
        dtDelimiter,
        dtDefault,
        dtSetTerm:
          begin
            EndOfStatement := False;
            if not (Token.TokenType in [ttWhitespace, ttEOF]) then begin
              if (DelimiterType = dtDelimiter) and Tokens.IsEqual(TokenIndex-1, 'DELIMITER', tcInsensitive) then begin
                iPos := TokenIndex -2;
                Delimiter := '';
                SetNextToken;
                N := TokenIndex;
                while Token.TokenType = ttWhitespace do begin
                  N := TokenIndex;
                  SetNextToken;
                end;
                while Token.TokenType <> ttWhitespace do begin
                  SetNextToken;
                  if (Token.TokenType in [ttWhitespace, ttEOF]) then
                    Delimiter := Tokens.AsString(N, TokenIndex - 2); //get the new delimiter
                end;
                iPos := TokenIndex- iPos;   //trim "delimiter xxy;"
                EndOfStatement := True;
              end else if not (Token.TokenType in [ttWhitespace, ttEOF]) then begin
                P := Pointer(Delimiter);
                N := 0;
                if (Token.P^ = P^) then begin
                  if (Length(Delimiter) = Token.L) then begin
                    EndOfStatement := True;
                    iPos := 2; //trim the delimiter
                  end else begin
                    D := TokenIndex;
                    repeat
                      if Token.L <= N then begin
                        N := 0;
                        SetNextToken;
                      end;
                      if not (P^ = (Token.P+N)^) then
                        Break;
                      Inc(P);
                      Inc(N);
                    until (P^ = #0);
                    if P^ = #0 then begin
                      EndOfStatement := True;
                      iPos := TokenIndex - D + 2; //trim the delimiter
                    end;
                  end;
                end;
              end;
            end;
          end;
        else
          EndOfStatement := False;
      end;

      //if Token.TokenType = ttEOF then Break;

      { Processes the end of statements. }
      if EndOfStatement then
      begin
        SQL := Tokens.AsString(StartTokenIndex, TokenIndex-iPos-Ord(Token.TokenType = ttEOF));
        StartTokenIndex := TokenIndex+1;

        if CleanupStatements then
          SQL := Trim(SQL);
        if SQL <> '' then begin
          if not CleanupStatements
          then Temp := Trim(SQL)
          else Temp := SQL;
          if Temp = '' then
            Continue;
          if (DelimiterType = dtSetTerm) and StartsWith(UpperCase(Temp), SetTerm) then
              Delimiter := Copy(Temp, 10, Length(Temp) - 9)
          else if (DelimiterType = dtSetTerm) and ( ZFastCode.Pos(SetTerm, UpperCase(Temp)) > 0) then begin
            iPos := ZFastCode.Pos(SetTerm, UpperCase(Temp))+8;
            Delimiter := Copy(Temp, iPos+1, Length(Temp) - iPos);
            LastComment := TrimRight(Copy(Temp, 1, iPos-9));
          end else begin
            if (DelimiterType = dtEmptyLine) and EndsWith(SQL, ';') then
              SQL := Copy(SQL, 1, Length(SQL) - 1);
            if LastComment <> '' then
              SQL := LastComment+#13#10+SQL;
            if CleanupStatements then
              SQL := Trim(SQL);
            FStatements.Add(SQL);
            LastComment := '';
          end;
        end;
        SQL := '';
      { Adds a whitespace token. }
      end (*else if CleanupStatements and (Token.TokenType = ttWhitespace) then begin
        if SQL <> '' then
          SQL := Trim(SQL) + ' ';
      { Adds a default token. }
      end else begin
        // --> ms, 20/10/2005
        // TokenValue is not a ttWhitespace (#32)
        if (Token.TokenType = ttWhitespace) and (Token.EOT >= Token.EOT) then begin
          // SQL is not emtyp
          if (SQL <> '') then begin
            // is last token:
            if (Tokenindex = Tokens.count-1) then
              TokenValue := '';
            // next(!) token is also ttWhitespace or delimiter
            // (TokenIndex was already incremented!)
            if (Tokenindex < Tokens.Count-1) then
              if (Tokens[TokenIndex]^.TokenType = ttWhitespace) or Tokens.IsEqual(TokenIndex, Delimiter) then
                TokenValue := '';
          end else // SQL is empty
            TokenValue := '';
        end;
        if ((SQL = '') and (trim(TokenValue) = '')) then
          TokenValue := '';
        // <-- ms
        SQL := SQL + TokenValue;
      end;  *)
    until Token.TokenType = ttEOF;
    if True then

    if ( LastComment <> '' ) and ( FStatements.Count > 0) then
      if CleanupStatements then
        FStatements[FStatements.Count-1] := FStatements[FStatements.Count-1]+' '+Trim(LastComment)
      else
        FStatements[FStatements.Count-1] := FStatements[FStatements.Count-1]+#13#10+LastComment;
    if StartTokenIndex < Tokens.Count then begin
      SQL := Tokens.AsString(StartTokenIndex, TokenIndex-1);
      if CleanupStatements then
        SQL := Trim(SQL);
      FUncompletedStatement := SQL;
    end;
  finally
    Tokens.Free;
  end;
{$ENDIF}
end;

end.
