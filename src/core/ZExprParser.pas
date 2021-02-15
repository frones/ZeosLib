{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Expression Parser classes and interfaces        }
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

unit ZExprParser;

interface

{$I ZCore.inc}

uses SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZClasses,
  ZCompatibility, ZVariant, ZTokenizer;

type
  /// <summary>Define types of expression tokens.</summary>
  TZExpressionTokenType = (
    ttUnknown, ttLeftBrace, ttRightBrace, ttLeftSquareBrace,
    ttRightSquareBrace, ttPlus, ttMinus, ttStar, ttSlash, ttProcent, ttPower,
    ttEqual, ttNotEqual, ttMore, ttLess, ttEqualMore, ttEqualLess,
    ttAnd, ttOr, ttXor, ttIs, ttNull, ttNot, ttLike, ttNotLike, ttIsNull,
    ttIsNotNull, ttComma, ttUnary, ttFunction, ttVariable, ttConstant
  );

  /// <author>EgonHugeist.</author>
  /// <summary>Defines a reference of an expression token.</summary>
  PZExpressionToken = ^TZExpressionToken;
  /// <summary>Defines an expression token record.</summary>
  TZExpressionToken = Record
    TokenType: TZExpressionTokenType;
    Value: TZVariant;
  end;

  /// <summary>Defines a parser exception.</summary>
  TZParseError = class (Exception);

  /// <author>EgonHugeist.</author>
  /// <summary>Implements a list of TZExpressionTokens.</summary>
  TZExpressionLokenList = Class(TZCustomElementList)
  protected
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    /// <summary>Adds an expression token into this list.</summary>
    /// <param>"TokenType" the expression token type to add.</param>
    /// <param>"Value" the expression token variable to add.</param>
    procedure Add(TokenType: TZExpressionTokenType;
      {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZVariant);
  End;

  {** Implements an expression parser class. }
  TZExpressionParser = class (TObject)
  private
    FTokenizer: IZTokenizer;
    FExpression: string;
    FInitialTokens: TZExpressionLokenList;
    FTokenIndex: Integer;
    FResultTokens: TZExpressionLokenList;
    FVariables: TStrings;
    /// <author>EgonHugeist.</author>
    /// <summary>Creates and syntax error. This moves the compiler
    ///  finalization code from mainmethod into submethod.</summary>
    /// <param>"Token" the expression token reference where the error happens.</param>
    /// <returns>The TZParseError abject.</returns>
    function CreateSyntaxErrorNear(Token: PZExpressionToken): TZParseError;
    /// <author>EgonHugeist.</author>
    /// <summary>Creates and unknown symbol error. This moves the compiler
    ///  finalization code from mainmethod into submethod.</summary>
    /// <param>"Token" then string token reference where the error happens.</param>
    /// <returns>The TZParseError abject.</returns>
    function CreateUnknownSymbolError(Token: PZToken): TZParseError;
    /// <summary>Checks are there more tokens for processing.</summary>
    /// <returns><c>TRUE</c> if some more tokens are present.</returns>
    function HasMoreTokens: Boolean;
    /// <summary>Gets the current expression token reference.</summary>
    /// <returns>the current token reference.</returns>
    function GetToken: PZExpressionToken;
    /// <summary>Gets the next expression token reference.</summary>
    /// <returns>the next token reference.</returns>
    function GetNextToken: PZExpressionToken;
    /// <summary>Shifts the current token object.</summary>
    procedure ShiftToken;
    /// <summary>Checks available token types with token types from the list.
    ///  If they match it shifts the tokens.</summary>
    /// <param>"TokenTypes" an array of token types to compare.</param>
    /// <returns><c>True</c> if token types match.</returns>
    function CheckTokenTypes(
      const TokenTypes: array of TZExpressionTokenType): Boolean;
    /// <summary>Tokenizes the given expression and prepares an initial tokens
    ///  list.</summary>
    procedure TokenizeExpression;
    /// <summary>Performs a syntax analyze at level 0.</summary>
    procedure SyntaxAnalyse;
    /// <summary>Performs a syntax analyze at level 1.</summary>
    procedure SyntaxAnalyse1;
    /// <summary>Performs a syntax analyze at level 2.</summary>
    procedure SyntaxAnalyse2;
    /// <summary>Performs a syntax analyze at level 3.</summary>
    procedure SyntaxAnalyse3;
    /// <summary>Performs a syntax analyze at level 4.</summary>
    procedure SyntaxAnalyse4;
    /// <summary>Performs a syntax analyze at level 5.</summary>
    procedure SyntaxAnalyse5;
    /// <summary>Performs a syntax analyze at level 6.</summary>
    procedure SyntaxAnalyse6;
  public
    /// <summary>Creates this expression parser object.</summary>
    /// <param>"Tokenizer" an expression tokenizer.</param>
    constructor Create(const Tokenizer: IZTokenizer);
    /// <summary>Destroyes this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>Sets a new expression string and parses it into internal byte
    ///  code.</summary>
    /// <param>"Expression" a new expression string.</param>
    procedure Parse(const Expression: string);
    /// <summary>Clears parsing result.</summary>
    procedure Clear;

    property Tokenizer: IZTokenizer read FTokenizer write FTokenizer;
    property Expression: string read FExpression write Parse;
    property ResultTokens: TZExpressionLokenList read FResultTokens;
    property Variables: TStrings read FVariables;
  end;

implementation

uses ZSysUtils, ZMessages;

{ TZExpressionToken }

const
  /// <summary>Defines a list of operators.</summary>
  OperatorTokens: array[0..24] of string = (
    '(', ')', '[', ']', '+', '-', '*', '/', '%', '^',
    '=', '<>', '!=', '>', '<', '>=', '<=',
    'AND', 'OR', 'XOR', 'NOT', 'IS', 'NULL', 'LIKE', ','
  );

  /// <summary>Defines a list of operator codes.</summary>
  OperatorCodes: array[0..24] of TZExpressionTokenType = (
    ttLeftBrace, ttRightBrace, ttLeftSquareBrace, ttRightSquareBrace,
    ttPlus, ttMinus, ttStar, ttSlash, ttProcent, ttPower, ttEqual, ttNotEqual,
    ttNotEqual, ttMore, ttLess, ttEqualMore, ttEqualLess, ttAnd, ttOr, ttXor,
    ttNot, ttIs, ttNull, ttLike, ttComma
  );

{ TZExpressionParser }

constructor TZExpressionParser.Create(const Tokenizer: IZTokenizer);
begin
  FTokenizer := Tokenizer;
  FExpression := '';
  FInitialTokens := TZExpressionLokenList.Create(SizeOf(TZExpressionToken), True);
  FTokenIndex := 0;
  FResultTokens := TZExpressionLokenList.Create(SizeOf(TZExpressionToken), True);
  FVariables := TStringList.Create;
end;

function TZExpressionParser.CreateSyntaxErrorNear(
  Token: PZExpressionToken): TZParseError;
begin
  Result := TZParseError.Create(Format(SSyntaxErrorNear,
    [SoftVarManager.GetAsString(Token.Value)]));
end;

function TZExpressionParser.CreateUnknownSymbolError(
  Token: PZToken): TZParseError;
var S: String;
begin
  S := '';
  System.SetString(S, Token.P, Token.L);
  Result := TZParseError.Create(Format(SUnknownSymbol, [S]));
end;

destructor TZExpressionParser.Destroy;
begin
  FreeAndNil(FInitialTokens);
  FreeAndNil(FResultTokens);
  FreeAndNil(FVariables);
  FTokenizer := Nil;
  inherited Destroy;
end;

procedure TZExpressionParser.Clear;
begin
  FExpression := '';
  FInitialTokens.Clear;
  FResultTokens.Clear;
  FTokenIndex := 0;
  FVariables.Clear;
end;

procedure TZExpressionParser.Parse(const Expression: string);
begin
  Clear;
  FExpression := Trim(Expression);
  if FExpression <> '' then begin
    TokenizeExpression;
    SyntaxAnalyse;
    if HasMoreTokens then
      raise CreateSyntaxErrorNear(GetToken);
  end;
end;

function TZExpressionParser.HasMoreTokens: Boolean;
begin
  Result := FTokenIndex < FInitialTokens.Count;
end;

function TZExpressionParser.GetToken: PZExpressionToken;
begin
  if FTokenIndex < FInitialTokens.Count
  then Result := FInitialTokens[FTokenIndex]
  else Result := nil;
end;

function TZExpressionParser.GetNextToken: PZExpressionToken;
begin
  if (FTokenIndex + 1) < FInitialTokens.Count
  then Result := FInitialTokens[FTokenIndex + 1]
  else Result := nil;
end;

procedure TZExpressionParser.ShiftToken;
begin
  Inc(FTokenIndex);
end;

function TZExpressionParser.CheckTokenTypes(
  const TokenTypes: array of TZExpressionTokenType): Boolean;
var
  I: Integer;
  Temp: PZExpressionToken;
begin
  Result := False;
  for I := Low(TokenTypes) to High(TokenTypes) do begin
    if (FTokenIndex + I) < FInitialTokens.Count then begin
      Temp := FInitialTokens[FTokenIndex + I];
      Result := Temp.TokenType = TokenTypes[I];
    end else
      Result := False;

    if not Result then
      Break;
  end;
  if Result then
    Inc(FTokenIndex, Length(TokenTypes));
end;

procedure TZExpressionParser.TokenizeExpression;
var
  I: Integer;
  TokenIndex: Integer;
  Temp: string;
  Tokens: TZTokenList;
  TokenType: TZExpressionTokenType;
  TokenValue: TZVariant;
  Token: PZToken;
begin
  Tokens := FTokenizer.TokenizeBufferToList(FExpression,
    [toSkipWhitespaces, toSkipComments, toSkipEOF]);
  try
    TokenIndex := 0;

    while TokenIndex < Tokens.Count do begin
      TokenType := ttUnknown;
      Token := Tokens[TokenIndex];
      case Token.TokenType of
        ttKeyword:
            if Tokens.IsEqual(Token, 'TRUE', tcInsensitive) then begin
              TokenType := ttConstant;
              TokenValue:= EncodeBoolean(True);
            end else if Tokens.IsEqual(Token, 'FALSE', tcInsensitive) then begin
              TokenType := ttConstant;
              TokenValue:= EncodeBoolean(False);
            end else for I := Low(OperatorTokens) to High(OperatorTokens) do
              if Tokens.IsEqual(Token, OperatorTokens[I], tcInsensitive) then begin
                TokenType := OperatorCodes[I];
                Break;
              end;
        ttWord: begin
            TokenType := ttVariable;
            Temp := Tokenizer.GetQuoteState.DecodeToken(Token^, Token.P^);
            if FVariables.IndexOf(Temp) < 0 then
              FVariables.Add(Temp);
            TokenValue:= EncodeString(Temp);
          end;
        ttInteger: begin
            TokenType := ttConstant;
            TokenValue:= EncodeInteger(Tokens.AsInt64(TokenIndex));
          end;
        ttFloat: begin
            TokenType := ttConstant;
            TokenValue:= EncodeDouble(Tokens.AsFloat(TokenIndex));
          end;
        ttQuoted: begin
            TokenType := ttConstant;
            TokenValue:= EncodeString(Tokenizer.GetQuoteState.DecodeToken(Token^, Token.P^));
          end;
        ttSymbol:
            for I := Low(OperatorTokens) to High(OperatorTokens) do
              if Tokens.IsEqual(Token, OperatorTokens[I], tcInsensitive) then begin
                TokenType := OperatorCodes[I];
                Break;
              end;
        ttTime,ttDate,ttDateTime: begin
            TokenType := ttConstant;
            Temp := Tokenizer.GetQuoteState.DecodeToken(Token^, Token.P^);
            TokenValue:= EncodeDateTime(StrToDateTime(Temp));
            TokenValue.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF} := Temp; //this conversion is not 100%safe so'll keep the native value by using advantages of the ZVariant
          end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
      if TokenType = ttUnknown then
        raise CreateUnknownSymbolError(Token);

      Inc(TokenIndex);
      FInitialTokens.Add(TokenType, TokenValue);
    end;
  finally
    Tokens.Free;
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse1;
  while HasMoreTokens do begin
    Token := GetToken;
    if not (Token.TokenType in [ttAnd, ttOr, ttXor]) then
      Break;
    ShiftToken;
    SyntaxAnalyse1;
    FResultTokens.Add(Token.TokenType, NullVariant);
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse1;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Token := GetToken;
  if Token.TokenType = ttNot then begin
    ShiftToken;
    SyntaxAnalyse2;
    FResultTokens.Add(Token.TokenType, NullVariant);
   end else
    SyntaxAnalyse2;
end;

procedure TZExpressionParser.SyntaxAnalyse2;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse3;
  while HasMoreTokens do begin
    Token := GetToken;
    if not (Token.TokenType in [ttEqual, ttNotEqual, ttMore, ttLess,
      ttEqualMore, ttEqualLess]) then
      Break;
    ShiftToken;
    SyntaxAnalyse3;
    FResultTokens.Add(Token.TokenType, NullVariant);
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse3;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse4;
  while HasMoreTokens do begin
    Token := GetToken;
    if Token.TokenType in [ttPlus, ttMinus, ttLike] then begin
      ShiftToken;
      SyntaxAnalyse4;
      FResultTokens.Add(Token.TokenType, NullVariant);
    end else if CheckTokenTypes([ttNot, ttLike]) then begin
      SyntaxAnalyse4;
      FResultTokens.Add(ttNotLike, NullVariant);
    end else if CheckTokenTypes([ttIs, ttNull]) then
      FResultTokens.Add(ttIsNull, NullVariant)
    else if CheckTokenTypes([ttIs, ttNot, ttNull]) then
      FResultTokens.Add(ttIsNotNull, NullVariant)
    else
      Break;
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse4;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse5;
  while HasMoreTokens do begin
    Token := GetToken;
    if not (Token.TokenType in [ttStar, ttSlash, ttProcent]) then
      Break;
    ShiftToken;
    SyntaxAnalyse5;
    FResultTokens.Add(Token.TokenType, NullVariant);
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse5;
var Token: PZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse6;
  while HasMoreTokens do begin
    Token := GetToken;
    if Token.TokenType <> ttPower then
      Break;
    ShiftToken;
    SyntaxAnalyse6;
    FResultTokens.Add(Token.TokenType, NullVariant);
  end;
end;

procedure TZExpressionParser.SyntaxAnalyse6;
var ParamsCount: Integer;
    Unary, Token: PZExpressionToken;
    Primitive, NextToken: PZExpressionToken;
    Temp: TZVariant;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Unary := GetToken;
  if Unary.TokenType = ttPlus then begin
    Unary := nil;
    ShiftToken;
  end else if Unary.TokenType = ttMinus then begin
    Unary.TokenType := ttUnary;
    ShiftToken;
   end else
    Unary := nil;

  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Primitive := GetToken;
  NextToken := GetNextToken;
  if (Primitive.TokenType = ttVariable) and (NextToken <> nil)
    and (NextToken.TokenType = ttLeftBrace) then
    Primitive.TokenType := ttFunction;

  if Primitive.TokenType in [ttConstant, ttVariable] then begin
    ShiftToken;
    FResultTokens.Add(Primitive.TokenType, Primitive.Value);
  end else if Primitive.TokenType = ttLeftBrace then begin
    ShiftToken;
    SyntaxAnalyse;
    if not HasMoreTokens then
      raise TZParseError.Create(SUnexpectedExprEnd);
    Primitive := GetToken;
    if Primitive.TokenType <> ttRightBrace then
      raise TZParseError.Create(SRightBraceExpected);
    ShiftToken;
  end else if Primitive.TokenType = ttFunction then begin
    ShiftToken;
    Token := GetToken;
    if Token.TokenType <> ttLeftBrace then
      raise TZParseError.Create(SInternalError);
    ParamsCount := 0;
    repeat
      ShiftToken;
      Token := GetToken;
      if (Token = nil) or (Token.TokenType = ttRightBrace) then
        Break;
      Inc(ParamsCount);
      SyntaxAnalyse;
      Token := GetToken;
    until (Token = nil) or (Token.TokenType <> ttComma);

    if not HasMoreTokens then
      raise TZParseError.Create(SUnexpectedExprEnd);
    if Token.TokenType <> ttRightBrace then
      raise TZParseError.Create(SRightBraceExpected);
    ShiftToken;

    Temp:= EncodeInteger(ParamsCount);
    FResultTokens.Add(ttConstant, Temp);
    FResultTokens.Add(Primitive.TokenType, Primitive.Value);
   end else
    raise TZParseError.Create(SSyntaxError);

  if Unary <> nil then
    FResultTokens.Add(Unary.TokenType, NullVariant);
end;

{ TZExpressionLokenList }

procedure TZExpressionLokenList.Add(TokenType: TZExpressionTokenType;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZVariant);
var ExpressionToken: PZExpressionToken;
    Idx: NativeInt;
begin
  ExpressionToken := inherited Add(Idx);
  ExpressionToken.TokenType := TokenType;
  ExpressionToken.Value := Value;
end;

procedure TZExpressionLokenList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then begin
    PZExpressionToken(Ptr).Value.VUnicodeString := '';
    PZExpressionToken(Ptr).Value.VRawByteString := EmptyRaw;
    PZExpressionToken(Ptr).Value.VInterface := nil;
  end;
end;

end.
