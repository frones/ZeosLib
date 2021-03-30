{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            SQL Statements Analysing classes             }
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

unit ZGenericSqlAnalyser;

interface

{$I ZParseSql.inc}

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZClasses, ZTokenizer, ZSelectSchema;

type
  /// <summary>Implements a section of the parsed SQL statement.</summary>
  TZStatementSection = class (TObject)
  private
    FName: string;
    FTokens: TZTokenList;
  public
    /// <summary>Create SQL statement section object.</summary>
    /// <param>"Tokens" a list of tokens.</param>
    constructor Create(const Name: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}
      Tokens: TZTokenList);
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;

    /// <summary>Clones an object instance.</summary>
    /// <returns>a clonned object instance.<returns>
    function Clone: TZStatementSection;
  public
    property Name: string read FName write FName;
    property Tokens: TZTokenList read FTokens;
  end;

  /// <summary>Defines a publicly available interface to statement analyser.</summary>
  IZStatementAnalyser = interface(IZInterface)
    ['{967635B6-411B-4DEF-990C-9C6C01F3DC0A}']
    /// <summary>Tokenizes a given SQL query into a list of tokens with tokenizer.</summary>
    /// <param>"Tokenizer" a tokenizer object.</param>
    /// <param>"SQL" a SQL query to be tokenized.</param>
    /// <returns>a list with tokens.</returns>
    function TokenizeQuery(const Tokenizer: IZTokenizer; const SQL: string;
      Cleanup: Boolean): TZTokenList;
    /// <summary>Splits a given list of tokens into the list named sections.</summary>
    /// <param>"Tokens" a list of tokens.</param>
    /// <returns>a list of section names where object property contains a list
    ///  of tokens in the section. It initial list is not started with a section
    ///  name the first section is unnamed ('').</returns>
    function SplitSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList): TObjectList;
    /// <summary>Composes a string from the list of tokens.</summary>
    /// <param>"Tokens" a list of tokens.</param>
    /// <returns>a composes string.</returns>
    function ComposeTokens({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList): string;
    /// <summary>Composes a string from the list of statement sections.</summary>
    /// <param>"Tokens" a list of statement sections.</param>
    /// <returns>a composes string.</returns>
    function ComposeSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}Sections: TObjectList): string;
    /// <summary>Extracts a select schema from the specified parsed select statement.</summary>
    /// <param>"Sections" a list of sections.</param>
    /// <returns>a select statement schema.</returns>
    function DefineSelectSchemaFromSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      Sections: TObjectList): IZSelectSchema;
    /// <summary>Defines a select schema from the specified SQL query.</summary>
    /// <param>"Tokenizer" a tokenizer object.</param>
    /// <param>"SQL" a SQL query.</param>
    /// <returns>a select statement schema.</returns>
    function DefineSelectSchemaFromQuery(const Tokenizer: IZTokenizer;
      const SQL: string): IZSelectSchema;
  end;

  /// <summary>Implements an SQL statements analyser.</summary>
  TZGenericStatementAnalyser = class (TZAbstractObject, IZStatementAnalyser)
  private
    FSectionNames: TStrings;
    FSelectOptions: TStrings;
    FFromJoins: TStrings;
    FFromClauses: TStrings;
  protected
    /// <summary>Converts an array of strings into TStrings object.</summary>
    /// <param>"Value" an array of strings to be converted.</param>
    /// <returns>a TStrings object with specified strings.</returns>
    function ArrayToStrings(const Value: array of string): TStrings;
    /// <summary>Checks for keyword with one, two or three consisted words in
    ///  the list</summary>
    /// <param>"Tokens" a list or tokens</param>
    /// <param>"TokenIndex" an index of the current token</param>
    /// <param>"Keywords" a list of keywords (in uppers case delimited with '*')</param>
    /// <param>"Keyword" an out parameter with found keyword.</param>
    /// <param>"WordCount" a count of words in the found keyword.</param>
    /// <returns><c>True</c> if the check was successfull</returns>
    function CheckForKeyword({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList;
      TokenIndex: Integer; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Keywords: TStrings;
      out Keyword: string; out WordCount: Integer): Boolean;
    /// <summary>Finds a section by it's name.</summary>
    /// <param>"Sections" a list of sections.</param>
    /// <param>"Name" a name of the section to be found.</param>
    /// <returns>a list of section tokens or <c>null</c> if section is was not
    ///  found.</returns>
    function FindSectionTokens(Sections: TObjectList; const Name: string): TZTokenList;
    /// <summary>Fills select schema with field references.</summary>
    /// <param>"SelectSchema" a select schema object.</param>
    /// <param>"SelectTokens" a list of tokens in select section.</param>
    procedure FillFieldRefs(const SelectSchema: IZSelectSchema; SelectTokens: TZTokenList);
    /// <summary>Fills select schema with table references.</summary>
    /// <param>"SelectSchema" a select schema object.</param>
    /// <param>"FromTokens" a list of tokens in from section.</param>
    procedure FillTableRefs(const SelectSchema: IZSelectSchema; FromTokens: TZTokenList);
    /// <summary>Skips option tokens specified in the string list.</summary>
    /// <param>"Tokens" a list of tokens to scan.</param>
    /// <param>"TokenIndex" the index of the current token.</param>
    /// <param>"Options" a list of option keyword strings in the upper case.</param>
    /// <returns><c>true</c> if some tokens were skipped.</returns>
    function SkipOptionTokens(Tokens: TZTokenList; var TokenIndex: Integer;
      Options: TStrings): Boolean;
    /// <summary>Skips tokens inside brackets.</summary>
    /// <param>"Tokens" a list of tokens to scan.</param>
    /// <param>"TokenIndex" the index of the current token.</param>
    /// <returns><c>true</c> if some tokens were skipped.</returns>
    function SkipBracketTokens(Tokens: TZTokenList; var TokenIndex: Integer): Boolean;
  protected
    property SectionNames: TStrings read FSectionNames write FSectionNames;
    property SelectOptions: TStrings read FSelectOptions write FSelectOptions;
    property FromJoins: TStrings read FFromJoins write FFromJoins;
    property FromClauses: TStrings read FFromClauses write FFromClauses;
  public
    /// <summary>Creates the object and assignes the main properties.</summary>
    constructor Create;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>Tokenizes a given SQL query into a list of tokens with tokenizer.</summary>
    /// <param>"Tokenizer" a tokenizer object.</param>
    /// <param>"SQL" a SQL query to be tokenized.</param>
    /// <returns>a list with tokens.</returns>
    function TokenizeQuery(const Tokenizer: IZTokenizer; const SQL: string;
      Cleanup: Boolean): TZTokenList;
    /// <summary>Splits a given list of tokens into the list named sections.</summary>
    /// <param>"Tokens" a list of tokens.</param>
    /// <returns>a list of section names where object property contains a list
    ///  of tokens in the section. It initial list is not started with a section
    ///  name the first section is unnamed ('').</returns>
    function SplitSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList): TObjectList;
    /// <summary>Composes a string from the list of tokens.</summary>
    /// <param>"Tokens" a list of tokens.</param>
    /// <returns>a composes string.</returns>
    function ComposeTokens({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList): string;
    /// <summary>Composes a string from the list of statement sections.</summary>
    /// <param>"Tokens" a list of statement sections.</param>
    /// <returns>a composes string.</returns>
    function ComposeSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}Sections: TObjectList): string;
    /// <summary>Extracts a select schema from the specified parsed select statement.</summary>
    /// <param>"Sections" a list of sections.</param>
    /// <returns>a select statement schema.</returns>
    function DefineSelectSchemaFromSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      Sections: TObjectList): IZSelectSchema;
    /// <summary>Defines a select schema from the specified SQL query.</summary>
    /// <param>"Tokenizer" a tokenizer object.</param>
    /// <param>"SQL" a SQL query.</param>
    /// <returns>a select statement schema.</returns>
    function DefineSelectSchemaFromQuery(const Tokenizer: IZTokenizer;
      const SQL: string): IZSelectSchema;
  end;

implementation

uses SysUtils, ZSysUtils;

{ TZStatementSection }

constructor TZStatementSection.Create(const Name: string;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList);
begin
  FName := Name;
  FTokens := Tokens;
end;

destructor TZStatementSection.Destroy;
begin
  FreeAndNil(FTokens);
  inherited Destroy;
end;

function TZStatementSection.Clone: TZStatementSection;
var
  Temp: TZTokenList;
begin
  Temp := TZTokenList.Create;
  Temp.Assign(FTokens);
  Result := TZStatementSection.Create(FName, Temp);
end;

const
  {** The generic constants.}
  GenericSectionNames: array[0..12] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'FOR*UPDATE', 'LIMIT', 'OFFSET'
  );
  GenericSelectOptions: array[0..1] of string = (
    'DISTINCT', 'ALL'
  );
  GenericFromJoins: array[0..5] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'INNER', 'OUTER', 'JOIN'
  );
  GenericFromClauses: array[0..0] of string = (
    'ON'
  );

{ TZGenericStatementAnalyser }

constructor TZGenericStatementAnalyser.Create;
begin
  FSectionNames := ArrayToStrings(GenericSectionNames);
  FSelectOptions := ArrayToStrings(GenericSelectOptions);
  FFromJoins := ArrayToStrings(GenericFromJoins);
  FFromClauses := ArrayToStrings(GenericFromClauses);
end;

destructor TZGenericStatementAnalyser.Destroy;
begin
  FreeAndNil(FSectionNames);
  FreeAndNil(FSelectOptions);
  FreeAndNil(FFromJoins);
  FreeAndNil(FFromClauses);
  inherited Destroy;
end;

function TZGenericStatementAnalyser.ArrayToStrings(
  const Value: array of string): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Capacity := Length(Value);
  for I := Low(Value) to High(Value) do
    Result.Add(Value[I]);
end;

function TZGenericStatementAnalyser.CheckForKeyword({$IFDEF AUTOREFCOUNT}const{$ENDIF}Tokens: TZTokenList;
  TokenIndex: Integer; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Keywords: TStrings;
  out Keyword: string; out WordCount: Integer): Boolean;
var
  I: Integer;
begin
  WordCount := 0;
  Keyword := '';
  Result := False;

  for I := 1 to 3 do begin
    if (Tokens.Count <= TokenIndex) then
      Break;
    if Tokens[TokenIndex].TokenType <> ttWord then
      Break;
    if Keyword <> '' then
      Keyword := Keyword + '*';
    Keyword := Keyword + AnsiUpperCase(Tokens.AsString(TokenIndex));
    Inc(WordCount);
    if Keywords.IndexOf(Keyword) >= 0 then begin
      Result := True;
      Break;
    end;
    Inc(TokenIndex);
    { Skips whitespaces. }
    while Tokens.Count > TokenIndex do begin
      if not (Tokens[TokenIndex]^.TokenType in [ttWhitespace, ttComment]) then
        Break;
      Inc(TokenIndex);
      Inc(WordCount);
    end;
  end;

  if not Result then begin
    WordCount := 0;
    Keyword := '';
  end;
end;

function TZGenericStatementAnalyser.FindSectionTokens(
  Sections: TObjectList; const Name: string): TZTokenList;
var
  I: Integer;
  Current: TZStatementSection;
begin
  Result := nil;
  for I := 0 to Sections.Count - 1 do begin
    Current := TZStatementSection(Sections[I]);
    if Current.Name = Name then begin
      Result := Current.Tokens;
      Break;
    end;
  end;
end;

function TZGenericStatementAnalyser.TokenizeQuery(
  const Tokenizer: IZTokenizer; const SQL: string; Cleanup: Boolean): TZTokenList;
begin
  if Cleanup
  then Result := Tokenizer.TokenizeBufferToList(SQL,
      [toSkipEOF, toSkipComments, toUnifyWhitespaces])
  else Result := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
end;

function TZGenericStatementAnalyser.SplitSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}
  Tokens: TZTokenList): TObjectList;
var
  I: Integer;
  Keyword: string;
  WordCount: Integer;
  TokenIndex: Integer;
  Elements: TZTokenList;
  FoundSection: Boolean;
  BracketCount: Integer;
begin
  Result := TObjectList.Create;
  TokenIndex := 0;
  FoundSection := True;
  Elements := nil;
  CheckForKeyword(Tokens, TokenIndex, SectionNames, Keyword{%H-}, WordCount{%H-});

  while TokenIndex < Tokens.Count do begin
    if FoundSection then begin
      Elements := TZTokenList.Create;
      for I := 0 to WordCount - 1 do
        Elements.Add(Tokens[TokenIndex + I]^);
      Inc(TokenIndex, WordCount);
      Result.Add(TZStatementSection.Create(Keyword, Elements));
    end;
    FoundSection := CheckForKeyword(Tokens, TokenIndex, SectionNames,
      Keyword, WordCount);
    if not FoundSection and (TokenIndex < Tokens.Count) then begin
      BracketCount := 0;
      repeat
        Elements.Add(Tokens[TokenIndex]^);
        if (Tokens[TokenIndex].L = 1) then
          if (Tokens[TokenIndex].P^ = '(') then
            Inc(BracketCount)
          else if (Tokens[TokenIndex].P^ = ')') then
            Dec(BracketCount);
        Inc(TokenIndex);
      until (BracketCount <= 0) or (TokenIndex >= Tokens.Count);
    end;
  end;
end;

function TZGenericStatementAnalyser.ComposeTokens({$IFDEF AUTOREFCOUNT}const{$ENDIF}
  Tokens: TZTokenList): string;
begin
  Result := Tokens.AsString;
end;

function TZGenericStatementAnalyser.ComposeSections({$IFDEF AUTOREFCOUNT}const{$ENDIF}
  Sections: TObjectList): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Sections.Count - 1 do
    Result := Result + ComposeTokens(TZStatementSection(Sections[I]).Tokens);
end;

function TZGenericStatementAnalyser.SkipBracketTokens(Tokens: TZTokenList;
  var TokenIndex: Integer): Boolean;
var
  BracketCount: Integer;
begin
  { Checks for the start bracket. }
  if (TokenIndex < Tokens.Count) and not Tokens.IsEqual(TokenIndex, Char('(')) then begin
    Result := False;
    Exit;
  end;

  { Skips the expression in brackets. }
  Result := True;
  BracketCount := 1;
  Inc(TokenIndex);
  while (TokenIndex < Tokens.Count) and (BracketCount > 0) do begin
    if Tokens.IsEqual(TokenIndex, Char('(')) then
      Inc(BracketCount)
    else if Tokens.IsEqual(TokenIndex, Char(')')) then
      Dec(BracketCount);
    Inc(TokenIndex);
  end;
end;

function TZGenericStatementAnalyser.SkipOptionTokens(Tokens: TZTokenList;
  var TokenIndex: Integer; Options: TStrings): Boolean;
begin
  Result := False;
  while TokenIndex < Tokens.Count do begin
    if not (Tokens[TokenIndex]^.TokenType in [ttWhitespace, ttComment])
       and (Options.IndexOf(UpperCase(Tokens.AsString(TokenIndex))) < 0) then
      Break;
    Inc(TokenIndex);
    Result := True;
  end;
end;

procedure TZGenericStatementAnalyser.FillFieldRefs(
  const SelectSchema: IZSelectSchema; SelectTokens: TZTokenList);
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Field: string;
  Alias: string;
  CurrentValue: string;
  CurrentType: TZTokenType;
  CurrentUpper: string;
  ReadField: Boolean;
  HadWhitespace : Boolean;
  LastWasBracketSection, LastWasSymbol: Boolean;
  CurrentUpperIs_AS: Boolean; //place holder to avoid compare the token twice

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Field := '';
    Alias := '';
    ReadField := True;
    LastWasBracketSection := False;
    LastWasSymbol:= False;
  end;

  { improve fail of fieldname detection if whitespaces and non ttWord or ttQuotedIdentifier previously detected
    f.e.: select first 100 skip 10 field1, field2}
  function CheckNextTokenForCommaAndWhiteSpaces: Boolean;
  var
    CurrentValue: string;
    CurrentType: TZTokenType;
    I: Integer;
    LAllowWord, LHadWhiteSpace: Boolean;
  begin
    Result := False;
    I := 1;
    //Check to right side to avoid wrong alias detection
    LAllowWord:= False;
    LHadWhiteSpace:= False;
    while SelectTokens.Count > TokenIndex +i do begin
      CurrentValue := SelectTokens.AsString(TokenIndex+i);
      CurrentType  := SelectTokens[TokenIndex+i]^.TokenType;
      if CurrentType in [ttWhiteSpace, ttSymbol] then begin
        if (CurrentValue = ',') then begin
          Result := True;
          Break;
        end
        else if (CurrentValue = '.') and not LHadWhiteSpace then
          LAllowWord:= True // field
        else if CurrentType = ttWhitespace then begin
          if not LHadWhiteSpace then
            LAllowWord:= True; // alias
          LHadWhiteSpace:= True;
        end;
      end else if LAllowWord and (CurrentType in [ttWord, ttQuotedIdentifier]) then
        LAllowWord:= False
      else
        break;
      Inc(i);
    end;

    if Result then begin
      i := 1;
      while Tokenindex - i > 0 do
        if SelectTokens[TokenIndex-i]^.TokenType = ttWhiteSpace
        then Inc(i)
        else Break;
      Result := Result and (TokenIndex - I > 0) and
          not ( SelectTokens[TokenIndex-i]^.TokenType = ttWord );
    end;
  end;

begin
  TokenIndex := 1;
  SkipOptionTokens(SelectTokens, TokenIndex, Self.SelectOptions);

  ClearElements;
  while TokenIndex < SelectTokens.Count do
  begin
    CurrentValue := SelectTokens.AsString(TokenIndex);
    CurrentUpper := AnsiUpperCase(CurrentValue);
    CurrentType := SelectTokens[TokenIndex]^.TokenType;

    { Switches to alias part. }
    CurrentUpperIs_AS := (CurrentUpper = 'AS');
    if (CurrentType = ttWhitespace) or CurrentUpperIs_AS then
      ReadField := ReadField and (Field = '') and not CurrentUpperIs_AS
    { Reads field. }
    else if ReadField and ((CurrentType in [ttWord, ttQuotedIdentifier]) or
      (CurrentValue = '*')) then
    begin
      Catalog := Schema;
      Schema := Table;
      Table := Field;
      Field := CurrentValue;
    end
    { Skips a '.' in field part. }
    else if ReadField and (CurrentValue = '.') then
    begin
    end
    { Reads alias. }
    else if not ReadField and (CurrentType in [ttWord, ttQuotedIdentifier]) then
      Alias := CurrentValue
    { Ends field reading. }
    else if CurrentValue = ',' then begin
      if Field <> '' then
        SelectSchema.AddField(TZFieldRef.Create(True, Catalog, Schema, Table,
          Field, Alias, nil));
      ClearElements;
    end else begin { Skips till the next field. }
      ClearElements;
      HadWhitespace := False;
      while (TokenIndex < SelectTokens.Count) and (CurrentValue <> ',') do
      begin
        CurrentValue := SelectTokens.AsString(TokenIndex);
        if CurrentValue = '(' then begin
          SkipBracketTokens(SelectTokens, TokenIndex);
          LastWasBracketSection := True;
        end else begin
          CurrentType := SelectTokens[TokenIndex]^.TokenType;
          if HadWhitespace and (CurrentType in [ttWord, ttQuotedIdentifier]) then
            if not LastWasBracketSection and not LastWasSymbol and CheckNextTokenForCommaAndWhiteSpaces then
              Break
            else
              Alias := CurrentValue
          else if not (CurrentType in [ttWhitespace, ttComment]) and
              (CurrentValue <> ',') then begin
            Alias := '';
            if CurrentType = ttSymbol then
              LastWasSymbol:= True;
          end else if CurrentType = ttWhitespace then
            HadWhitespace := true;
          Inc(TokenIndex);
        end;
      end;
      if Alias <> '' then begin
        SelectSchema.AddField(TZFieldRef.Create(False, '', '', '', '', Alias, nil));
        ClearElements;
      end;
      Dec(TokenIndex); // go back 1 token(Because of Inc in next lines)
    end;
    Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Field <> '' then
    SelectSchema.AddField(TZFieldRef.Create(True, Catalog, Schema, Table,
      Field, Alias, nil));
end;

procedure TZGenericStatementAnalyser.FillTableRefs(
  const SelectSchema: IZSelectSchema; FromTokens: TZTokenList);
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Alias: string;
  CurrentValue: string;
  CurrentType: TZTokenType;
  CurrentUpper: string;
  ReadTable: Boolean;

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Alias := '';
    ReadTable := True;
  end;

begin
  TokenIndex := 1;

  ClearElements;
  while TokenIndex < FromTokens.Count do begin
    CurrentValue := FromTokens.AsString(TokenIndex);
    CurrentUpper := AnsiUpperCase(CurrentValue);
    CurrentType := FromTokens[TokenIndex]^.TokenType;

    { Processes from join keywords. }
    if FromJoins.IndexOf(CurrentUpper) >= 0 then begin
      if Table <> '' then
        SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
      ClearElements;
      SkipOptionTokens(FromTokens, TokenIndex, FromJoins);
      Continue;
    { Skips from clause keywords. }
    end else if FromClauses.IndexOf(CurrentUpper) >= 0 then begin
      Inc(TokenIndex);
      CurrentValue := FromTokens.AsString(TokenIndex);
      CurrentUpper := AnsiUpperCase(CurrentValue);
      while (TokenIndex < FromTokens.Count) and
            (FromJoins.IndexOf(CurrentUpper) < 0) and (CurrentUpper <> ',') do begin
        if CurrentUpper = '('
        then SkipBracketTokens(FromTokens, TokenIndex)
        else Inc(TokenIndex);
        if TokenIndex < FromTokens.Count then begin
          CurrentValue := FromTokens.AsString(TokenIndex);
          CurrentUpper := AnsiUpperCase(CurrentValue);
          //CurrentType := FromTokens[TokenIndex]^.TokenType;
        end;
      end;
      // We must jump 1 tokens back now when we stopped on a Join clause.
      // Otherwise the next table is skipped
      if FromJoins.IndexOf(CurrentUpper) >= 0 then begin
        Dec(TokenIndex);
        CurrentValue := FromTokens.AsString(TokenIndex);
        CurrentUpper := AnsiUpperCase(CurrentValue);
      end;
    end
    { Switches to alias part. }
    else if (CurrentType = ttWhitespace) or (CurrentUpper = 'AS') then
      ReadTable := ReadTable and (Table = '') and (CurrentUpper <> 'AS')
    { Reads table. }
    else if ReadTable and (CurrentType in [ttWord, ttQuotedIdentifier]) then begin
      {Catalog := Schema;
      Schema := Table;}
      Table := CurrentValue;
    { Skips a '.' in table part. }
    end else if ReadTable and (CurrentValue = '.') then begin
      if not ((Schema <> '') and (Table = '')) then begin //hide issue master..sysobjects
        Catalog := Schema;
        Schema := Table;
      end;
      Table := '';
    { Reads alias. }
    end else if not ReadTable and (CurrentType = ttWord) then
      Alias := CurrentValue;
    { Ends field reading. }
    if CurrentValue = ',' then begin
      if Table <> '' then
        SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
      ClearElements;
    end;
    { Skips till the next field. }
    if CurrentValue = '('
    then SkipBracketTokens(FromTokens, TokenIndex)
    else Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Table <> '' then
    SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
end;

function TZGenericStatementAnalyser.DefineSelectSchemaFromSections(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}Sections: TObjectList): IZSelectSchema;
var
  SelectTokens: TZTokenList;
  FromTokens: TZTokenList;
begin
  Result := nil;
  { Checks for the correct select statement. }
  if (Sections.Count < 2)
    or not ((TZStatementSection(Sections[0]).Name = 'SELECT')
    or ((TZStatementSection(Sections[0]).Name = '')
    and (TZStatementSection(Sections[1]).Name = 'SELECT'))) then
    Exit;

  { Defins sections. }
  SelectTokens := FindSectionTokens(Sections, 'SELECT');
  FromTokens := FindSectionTokens(Sections, 'FROM');
  if (SelectTokens = nil) or (FromTokens = nil) then
    Exit;

  { Creates and fills the result object. }
  Result := TZSelectSchema.Create;
  FillFieldRefs(Result, SelectTokens);
  FillTableRefs(Result, FromTokens);
end;

function TZGenericStatementAnalyser.DefineSelectSchemaFromQuery(
  const Tokenizer: IZTokenizer; const SQL: string): IZSelectSchema;
var
  Tokens: TZTokenList;
  Sections: TObjectList;
begin
  Tokens := TokenizeQuery(Tokenizer, SQL, True);
  Sections := SplitSections(Tokens);
  try
    Result := DefineSelectSchemaFromSections(Sections);
  finally
    Tokens.Free;
    Sections.Free;
  end;
end;

end.

