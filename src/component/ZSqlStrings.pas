{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               SQL Query Strings component               }
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

{ constributor(s)
  Alexs
  EgonHugeist
  Mark Deams
}
unit ZSqlStrings;

interface

{$I ZComponent.inc}

uses
  Types, Classes, SysUtils, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs, {$ENDIF}ZClasses,
  ZDbcIntfs, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type
  /// <summary>Implements a SQL statement description object.</summary>
  TZSQLStatement = class (TObject)
  private
    FSQL: string;
    FParamIndices: TIntegerDynArray;
    FParams: TStrings;
    FParamNamesArray: TStringDynArray;
    /// <summary>Gets a parameters count for this statement.</summary>
    /// <returns>a parameters count.</returns>
    function GetParamCount: Integer;
    /// <summary>Gets a parameter name by it's index inside the statement.</summary>
    /// <param>"Index" the parameter index.
    /// <returns>a parameter name.</returns>
    function GetParamName(Index: Integer): string;
    /// <summary>Gets an array of parameter names.</summary>
    /// <returns>an array of parameter names.</returns>
    function GetParamNamesArray: TStringDynArray;
  public
    /// <summary>Creates a SQL statement object and assignes the main properties.
    /// <param>"SQL" a SQL statement.</param>
    /// <param>"ParamIndices" an array of parameter indices.</param>
    /// <param>"Params" a list with all parameter names.</param>
    constructor Create(const SQL: string; const ParamIndices: TIntegerDynArray;
      Params: TStrings);
  public
    property SQL: string read FSQL;
    property ParamCount: Integer read GetParamCount;
    property ParamNames[Index: Integer]: string read GetParamName;
    property ParamIndices: TIntegerDynArray read FParamIndices;
    property ParamNamesArray: TStringDynArray read FParamNamesArray;
  end;

  /// <summary>Implements a string list with SQL statements.</summary>
  TZSQLStrings = class (TStringList)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FDataset: TComponent;
    FParamCheck: Boolean;
    FStatements: TObjectList;
    FParams: TStringList;
    FMultiStatements: Boolean;
    FParamChar: Char;
    FDoNotRebuildAll: Boolean;
    /// <summary>Gets the parameter count.</summary>
    /// <returns>a count of SQL parameters.</returns>
    function GetParamCount: Integer;
    /// <summary>Gets parameter name by it's index.</summary>
    /// <param>"Index" a parameter index.</param>
    /// <returns>a parameter name.</returns>
    function GetParamName(Index: Integer): string;
    /// <summary>Gets a SQL statement by it's index.</summary>
    /// <param>"Index" a SQL statement index.</param>
    /// <returns>a SQL statement object.</returns>
    function GetStatement(Index: Integer): TZSQLStatement;
    /// <summary>Gets a SQL statements count.</summary>
    /// <returns>a SQL statements count.</returns>
    function GetStatementCount: Integer;
    /// <summary>Gets a Tokenizer object from owners connection or an generic
    ///  tokenizer if not connected.</summary>
    /// <returns>a Tokenizer object.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Sets a new correspondent dataset or Processor component.</summary>
    /// <param>"Value" a new dataset object.</param>
    procedure SetDataset(Value: TComponent);
    /// <summary>Sets a new ParamCheck value.</summary>
    /// <param>"Value" a new ParamCheck value.</param>
    procedure SetParamCheck(Value: Boolean);
    /// <summary>Sets a new ParamChar value.</summary>
    /// <param>"Value" a new ParamCheck value.</param>
    procedure SetParamChar(Value: Char);
    /// <summary>Sets a new MultiStatements value.</summary>
    /// <param>"Value" a new MultiStatements value.</param>
    procedure SetMultiStatements(Value: Boolean);
  protected
    /// <summary>Performs action when the content of this string list is
    ///  changed.</summary>
    procedure Changed; override;
    /// <summary>Finds a parameter by it's name.</summary>
    /// <param>"ParamName" a parameter name.</param>
    /// <returns>an index of found parameters or -1 if nothing was found.</returns>
    function FindParam(const ParamName: string): Integer;
    /// <summary>Rebuilds all SQL statements.</summary>
    procedure RebuildAll;
    procedure SetTextStr(const Value: string); override;
  public
    /// <summary>Creates a SQL strings object and assigns the main properties.</summary>
    constructor Create;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Dataset: TComponent read FDataset write SetDataset;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck;
    property ParamCount: Integer read GetParamCount;
    property ParamChar: Char read FParamChar write SetParamChar;
    property ParamNames[Index: Integer]: string read GetParamName;
    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: TZSQLStatement read GetStatement;
    property MultiStatements: Boolean read FMultiStatements
      write SetMultiStatements;
  end;

implementation

uses ZMessages, ZAbstractRODataset, ZSqlProcessor;

{ TZSQLStatement }

constructor TZSQLStatement.Create(const SQL: string;
  const ParamIndices: TIntegerDynArray; Params: TStrings);
begin
  FSQL := SQL;
  FParamIndices := ParamIndices;
  FParams := Params;
  FParamNamesArray := GetParamNamesArray;
end;

function TZSQLStatement.GetParamCount: Integer;
begin
  if Assigned(FParamIndices) then
    Result := High(FParamIndices) - Low(FParamIndices) + 1
  else Result := 0;
end;

function TZSQLStatement.GetParamName(Index: Integer): string;
begin
  if Assigned(FParamIndices)
  then Result := FParams[FParamIndices[Index + Low(FParamIndices)]]
  else Result := '';
end;

function TZSQLStatement.GetParamNamesArray: TStringDynArray;
var
  I: Integer;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
  SetLength(Result, High(FParamIndices) - Low(FParamIndices) + 1);
  for I := Low(Result) to High(Result) do
    Result[I] := FParams[FParamIndices[I + Low(FParamIndices)]];
end;

{ TZSQLStrings }

constructor TZSQLStrings.Create;
begin
  inherited Create; { -> needed to run the TestSuite else Inheritance(Self).Methods fails}
  FParams := TStringList.Create;
  FParamCheck := True;
  FStatements := TObjectList.Create;
  FMultiStatements := True;
  FParamChar :=':';
end;

destructor TZSQLStrings.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FStatements);
  FDataSet := nil;
  inherited Destroy;
end;

function TZSQLStrings.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TZSQLStrings.GetParamName(Index: Integer): string;
begin
  Result := FParams[Index];
end;

function TZSQLStrings.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

type TZProtectedAbstractRODataset = class(TZAbstractRODataset);

{$WARN SYMBOL_DEPRECATED OFF}
function TZSQLStrings.GetTokenizer: IZTokenizer;
begin
  { Defines a SQL specific tokenizer object. }
  Result := nil;
  if (FDataset is TZAbstractRODataset) and Assigned(TZProtectedAbstractRODataset(FDataset).Connection) then begin
    if TZProtectedAbstractRODataset(FDataset).Connection.Connected
    then Result := TZProtectedAbstractRODataset(FDataset).Connection.DbcConnection.GetTokenizer
    else Result := TZProtectedAbstractRODataset(FDataset).Connection.DbcDriver.GetTokenizer;
  end else if (FDataset is TZSQLProcessor) and Assigned(TZSQLProcessor(FDataset).Connection) then
    if TZSQLProcessor(FDataset).Connection.Connected
    then Result := TZSQLProcessor(FDataset).Connection.DbcConnection.GetTokenizer
    else Result := TZSQLProcessor(FDataset).Connection.DbcDriver.GetTokenizer;
  if Result = nil then
    Result := TZGenericSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;
{$WARN SYMBOL_DEPRECATED ON}

function TZSQLStrings.GetStatement(Index: Integer): TZSQLStatement;
begin
  Result := TZSQLStatement(FStatements[Index]);
end;

procedure TZSQLStrings.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then begin
    FParamCheck := Value;
    RebuildAll;
  end;
end;

procedure TZSQLStrings.SetTextStr(const Value: string);
begin
  if Trim(Value) <> Trim(Text) then //prevent rebuildall if nothing changed
    inherited SetTextStr(Value);
end;

procedure TZSQLStrings.SetParamChar(Value: Char);
begin
  if FParamChar <> Value then begin
    If not(GetTokenizer.GetCharacterState(Value) is TZSymbolstate) Then
      raise EZDatabaseError.Create(SIncorrectParamChar+' : '+Value);
    FParamChar := Value;
    RebuildAll;
  end;
end;

procedure TZSQLStrings.SetMultiStatements(Value: Boolean);
begin
  if FMultiStatements <> Value then begin
    FMultiStatements := Value;
    RebuildAll;
  end;
end;

procedure TZSQLStrings.SetDataset(Value: TComponent);
begin
  if FDataset <> Value then begin
    FDataset := Value;
    RebuildAll;
  end;
end;

function TZSQLStrings.FindParam(const ParamName: string): Integer;
begin
  FParams.CaseSensitive := False;
  Result := FParams.IndexOf(ParamName);
end;

const
  cAssignLeft: array[0..1] of Char = (':','=');
var //endian save
  uAssignLeft: {$IFDEF UNICODE}Cardinal{$ELSE}Word{$ENDIF} absolute cAssignLeft;

procedure TZSQLStrings.RebuildAll;
var
  Tokens: TZTokenList;
  Token: PZToken;
  TokenIndex: Integer;
  ParamIndex: Integer;
  ParamIndices: TIntegerDynArray;
  ParamIndexCount: Integer;
  ParamName, S, NormalizedParam: string;
  SQL: SQLString;
  Tokenizer: IZTokenizer;
  SQLStringWriter: TZSQLStringWriter;
  IgnoreParam: Boolean;
  procedure NextToken;
  begin
    Token := Tokens[TokenIndex];
    Inc(TokenIndex);
  end;
begin
  { Optimization for empty query. }
  If FDoNotRebuildAll then begin
    FDoNotRebuildAll := False;
    Exit;
  end;
  if (FDataset = nil) or not (csLoading in FDataset.ComponentState) then
    FParams.Clear;
  S := Text;
  NormalizedParam := Trim(S);
  if NormalizedParam = '' then
    Exit;


  FStatements.Clear;
  SQL := '';
  ParamIndexCount := 0;
  {$IFDEF WITH_VAR_INIT_WARNING}ParamIndices := nil;{$ENDIF}
  SetLength(ParamIndices, ParamIndexCount);

  { Optimization for single query without parameters. }
  if (not FParamCheck or (Pos(FParamChar, S) = 0))
    and (not FMultiStatements or (Pos(';', S) = 0)) then
  begin
    FStatements.Add(TZSQLStatement.Create(S, ParamIndices, FParams));
    Exit;
  end;

  Tokenizer := GetTokenizer;
  SQLStringWriter := TZSQLStringWriter.Create(Length(S));
  Tokens := Tokenizer.TokenizeBufferToList(S, [toSkipComments, toUnifyWhitespaces]);
  try
    TokenIndex := 0;
    repeat
      NextToken;
      { Processes parameters. }
      if ParamCheck and (Token.P^ = FParamChar) and (Token.L = 1) then begin
        NextToken;
        if (Token.TokenType <> ttEOF) then begin
          if (Token.TokenType = ttSymbol) and (Token.L = 2) and
            ({$IFDEF UNICODE}PCardinal{$ELSE}PWord{$ENDIF}(Token.P)^ = uAssignLeft) then
            SQLStringWriter.AddText(Token.P, Token.L, SQL)
          else if not ((Token.P^ = FParamChar) and (Token.L = 1)) then begin //test unescape FParamChar
            { Check for correct parameter type. }
            if not (Token.TokenType in [ttWord, ttQuoted, ttQuotedIdentifier, ttKeyWord, ttInteger]) then
              raise EZDatabaseError.Create(SIncorrectToken);
            NormalizedParam := Tokenizer.NormalizeParamToken(Token^, ParamName, FParams, ParamIndex, IgnoreParam);
            SQLStringWriter.AddText(NormalizedParam, SQL);
            if not IgnoreParam then begin
              Inc(ParamIndexCount);
              SetLength(ParamIndices, ParamIndexCount);
              ParamIndices[ParamIndexCount - 1] := ParamIndex;
            end;
            Continue;
          end else
            SQLStringWriter.AddChar(FParamChar, SQL);
        end;
      end else
        SQLStringWriter.AddText(Token.P, Token.L, SQL);

      { Adds a DML statement. }
      if (Token.TokenType = ttEOF) or (FMultiStatements and ((Token.P^ = ';') and (Token.L = 1))) then begin
        SQLStringWriter.CancelLastCharIfExists(';', SQL);
        SQLStringWriter.Finalize(SQL);
        SQL := Trim(SQL);
        if SQL <> '' then
          FStatements.Add(TZSQLStatement.Create(SQL, ParamIndices, FParams));
        SQL := '';
        ParamIndexCount := 0;
        SetLength(ParamIndices, ParamIndexCount);
      end;
    until Token.TokenType = ttEOF;
  finally
    S := ''; //hooking compiler optimisation
    Tokens.Free;
    SQLStringWriter.Free;
  end;
end;

procedure TZSQLStrings.Assign(Source: TPersistent);
var Old, New: String;
begin
  if Source is TStrings then begin
    Old := Text;
    Old := Trim(Old);
    New := TStrings(Source).Text;
    New := Trim(New);
    FDoNotRebuildAll := New = Old;
  end;
  inherited Assign(Source);
end;

procedure TZSQLStrings.Changed;
begin
  if UpdateCount = 0 then
    RebuildAll;
  inherited Changed;
end;

end.
