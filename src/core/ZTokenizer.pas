{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes and interfaces         }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTokenizer;

interface

{$I ZCore.inc}
{$Z-}

uses
   Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
   ZClasses, ZCompatibility;

type
  {**
    Objects of this class represent a type of token,
    such as "number", "symbol" or "word".
  }
  TZTokenType = (ttUnknown, ttEOF, ttFloat, ttInteger, ttHexDecimal,
    ttNumber, ttSymbol, ttQuoted, ttQuotedIdentifier, ttWord, ttKeyword,
    ttWhitespace, ttComment, ttSpecial, ttTime, ttDate, ttDateTime);

  {**
    Defines options for tokenizing strings.
  }
  TZTokenOption = (toSkipUnknown, toSkipWhitespaces, toSkipComments,
    toSkipEOF, toUnifyWhitespaces, toUnifyNumbers);
  TZTokenOptions = set of TZTokenOption;

  {**
    A token represents a logical chunk of a string. For
    example, a typical tokenizer would break the string
    <code>"1.23 <= 12.3"</code> into three tokens: the number
    1.23, a less-than-or-equal symbol, and the number 12.3. A
    token is a receptacle, and relies on a tokenizer to decide
    precisely how to divide a string into tokens.
  }
  PZToken = ^TZToken;
  TZToken = record
    P: PChar; //Begin of token value
    L: LengthInt; //Lengt of Token
    TokenType: TZTokenType;
  end;

  {** Defines a dynamic array of tokens. }
  TZTokenDynArray = array of TZToken;

  PZTokenArray = ^TZTokenArray;
  {** Defines a static array of tokens. }
  TZTokenArray = array[0..{$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF} - 1] of TZToken;

  TZTokenCase = (tcSensitive, tcInsensitive);
  TZTokenList = class
  private
    FTokens: PZTokenArray;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    {$IFNDEF DISABLE_CHECKING}
    class procedure Error(const Msg: string; Data: Integer);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(const Item: TZToken): Integer;
    procedure Insert(Index: Integer; const Item: TZToken);
    procedure Delete(Index: Integer);

    procedure Put(Index: Integer; const Item: TZToken);
    function Get(Index: Integer): TZToken;

    function GetToken(Index: Integer): PZToken;

    function AsString: String; overload;
    function AsString(Index: Integer): String; overload;
    function AsString(iStart, iEnd: Integer): String; overload;
    function AsFloat(Index: Integer): Extended;
    function AsInt64(Index: Integer): Int64;

    function IsEqual(Index: Integer; const Value: Char): Boolean; overload;
    function IsEqual(Index: Integer; const Value: String; TokenCase: TZTokenCase = tcSensitive): Boolean; overload;
    function IsEqual(Index: Integer; TokenType: TZTokenType; const Value: String;
      TokenCase: TZTokenCase = tcSensitive): Boolean; overload;

    procedure Clear;
    procedure Assign(Source: TZTokenList);

    property Count: Integer read FCount;
    property Items[Index: Integer]: TZToken read Get write Put;
    property Tokens[Index: Integer]: PZToken read GetToken; default;
  end;

  // Forward declaration
  TZTokenizer = class;

  {**
    A tokenizerState returns a token, given a reader, an initial character
    read from the reader, and a tokenizer that is conducting an overall
    tokenization of the reader. The tokenizer will typically have a character
    state table that decides which state to use, depending on an initial
    character. If a single character is insufficient, a state such
    as <code>SlashState</code> will read a second character, and may delegate
    to another state, such as <code>SlashStarState</code>. This prospect
    of delegation is the reason that the <code>nextToken()</code> method has a
    tokenizer argument.
  }
  TZTokenizerState = class (TObject)
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; virtual; abstract;
  end;

  {**
    A NumberState object returns a number from a reader. This
    state's idea of a number allows an optional, initial
    minus sign, followed by one or more digits. A decimal
    point and another string of digits may follow these digits.
  }
  TZNumberState = class (TZTokenizerState)
  protected
    function ReadDecDigits(var SPos: PChar; const NTerm: PChar): Boolean;
    function ReadHexDigits(var SPos: PChar; const NTerm: PChar): Boolean;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    A quoteState returns a quoted string token from a reader.
    This state will collect characters until it sees a match
    to the character that the tokenizer used to switch to
    this state. For example, if a tokenizer uses a double-
    quote character to enter this state, then <code>
    nextToken()</code> will search for another double-quote
    until it finds one or finds the end of the reader.
  }
  TZQuoteState = class (TZTokenizerState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    function EncodeString(const Value: string; QuoteChar: Char): string; virtual;
    function DecodeString(const Value: string; QuoteChar: Char): string; virtual; deprecated;
    function DecodeToken(const Value: TZToken; QuoteChar: Char): string; virtual;
  end;

  {**
    A CommentState object returns a comment from a reader.
  }
  TZCommentState = class (TZTokenizerState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCppCommentState = class (TZCommentState)
  protected
    procedure GetMultiLineComment(var SPos: PChar; const NTerm: PChar); virtual;
    procedure GetSingleLineComment(var SPos: PChar; const NTerm: PChar); virtual;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCCommentState = class (TZCppCommentState)
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {*Fix for C++ Builder hpp generation bug - #817612 *}
  (*$HPPEMIT 'namespace Ztokenizer {class DELPHICLASS TZSymbolNode;}' *)
  // Forward declaration
  TZSymbolNode = class;
  TZSymbolNodeArray = array of TZSymbolNode;

  {**
    A <code>SymbolNode</code> object is a member of a tree that
    contains all possible prefixes of allowable symbols. Multi-
    character symbols appear in a <code>SymbolNode</code> tree
    with one node for each character.

    For example, the symbol <code>=:~</code> will appear in a
    tree as three nodes. The first node contains an equals sign,
    and has a child; that child contains a colon and has a
    child; this third child contains a tilde, and has no
    children of its own. If the colon node had another child
    for a dollar sign character, then the tree would contain
    the symbol <code>=:$</code>.

    A tree of <code>SymbolNode</code> objects collaborate to
    read a (potentially multi-character) symbol from an input
    stream. A root node with no character of its own finds an
    initial node that represents the first character in the
    input. This node looks to see if the next character in the
    stream matches one of its children. If so, the node
    delegates its reading task to its child. This approach
    walks down the tree, pulling symbols from the input that
    match the path down the tree.

    When a node does not have a child that matches the next
    character, we will have read the longest possible symbol
    prefix. This prefix may or may not be a valid symbol.
    Consider a tree that has had <code>=:~</code> added and has
    not had <code>=:</code> added. In this tree, of the three
    nodes that contain <code>=:~</code>, only the first and
    third contain complete symbols. If, say, the input contains
    <code>=:a</code>, the colon node will not have a child that
    matches the 'a' and so it will stop reading. The colon node
    has to "unread": it must push back its character, and ask
    its parent to unread. Unreading continues until it reaches
    an ancestor that represents a valid symbol.
  }
  TZSymbolNode = class (TObject)
  private
    FCharacter: Char;
    FChildren: TZSymbolNodeArray;
    FValid: Boolean;
    FParent: TZSymbolNode;
  protected
    procedure AddDescendantLine(const Value: string);
    function DeepestRead(var SPos: PChar; const NTerm: PChar): TZSymbolNode;
    function UnreadToValid(var SPos: PChar; const NTerm: PChar): TZSymbolNode;
    function EnsureChildWithChar(Value: Char): TZSymbolNode;
    function FindChildWithChar(Value: Char): TZSymbolNode; virtual;
    function FindDescendant(const Value: string): TZSymbolNode;

    property Children: TZSymbolNodeArray read FChildren write FChildren;
    property Character: Char read FCharacter write FCharacter;
    property Valid: Boolean read FValid write FValid;
    property Parent: TZSymbolNode read FParent write FParent;
  public
    constructor Create(Parent: TZSymbolNode; Character: Char);
    destructor Destroy; override;

    function Ancestry: string; virtual;
  end;

  {**
    This class is a special case of a <code>SymbolNode</code>. A
    <code>SymbolRootNode</code> object has no symbol of its
    own, but has children that represent all possible symbols.
  }
  TZSymbolRootNode = class (TZSymbolNode)
  protected
    function FindChildWithChar(Value: Char): TZSymbolNode; override;
  public
    constructor Create;

    procedure Add(const Value: string);
    function Ancestry: string; override;
    function NextSymbol(var SPos: PChar; const NTerm: PChar): PChar;
  end;

  {**
    The idea of a symbol is a character that stands on its
    own, such as an ampersand or a parenthesis. For example,
    when tokenizing the expression <code>(isReady)&
    (isWilling) </code>, a typical tokenizer would return 7
    tokens, including one for each parenthesis and one for
    the ampersand. Thus a series of symbols such as
    <code>)&( </code> becomes three tokens, while a series
    of letters such as <code>isReady</code> becomes a single
    word token.
    <p>
    Multi-character symbols are an exception to the rule
    that a symbol is a standalone character.  For example, a
    tokenizer may want less-than-or-equals to tokenize as a
    single token. This class provides a method for
    establishing which multi-character symbols an object of
    this class should treat as single symbols. This allows,
    for example, <code>"cat <= dog"</code> to tokenize as
    three tokens, rather than splitting the less-than and
    equals symbols into separate tokens.
    <p>
    By default, this state recognizes the following multi-
    character symbols: <code>!=, :-, <=, >=</code>
  }
  TZSymbolState = class (TZTokenizerState)
  private
    FSymbols: TZSymbolRootNode;
  protected
    property Symbols: TZSymbolRootNode read FSymbols write FSymbols;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    procedure Add(const Value: string); virtual;
  end;

  {**
    A whitespace state ignores whitespace (such as blanks
    and tabs), and returns the tokenizer's next token. By
    default, all characters from 0 to 32 are whitespace.
  }
  TZWhitespaceState = class (TZTokenizerState)
  private
    FWhitespaceChars: array[0..ord(high(char))] of Boolean;
  public
    constructor Create;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    procedure SetWhitespaceChars(FromChar: Char; ToChar: Char; Enable: Boolean);
  end;

  {**
    A wordState returns a word from a reader. Like other
    states, a tokenizer transfers the job of reading to this
    state, depending on an initial character. Thus, the
    tokenizer decides which characters may begin a word, and
    this state determines which characters may appear as a
    second or later character in a word. These are typically
    different sets of characters; in particular, it is typical
    for digits to appear as parts of a word, but not as the
    initial character of a word.
    <p>
    By default, the following characters may appear in a word.
    The method <code>setWordChars()</code> allows customizing
    this.
    <blockquote><pre>
        From    To
         'a', 'z'
         'A', 'Z'
         '0', '9'

        as well as: minus sign, underscore, and apostrophe.
    </pre></blockquote>
  }
  TZWordState = class (TZTokenizerState)
  private
    FWordChars: array[0..ord(high(char))] of Boolean;
  public
    constructor Create;
  public
    function NextToken(var SPos: PChar; const NTerm: PChar;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;
    procedure SetWordChars(FromChar: Char; ToChar: Char; Enable: Boolean);
  end;

  {**
    A tokenizer divides a string into tokens. This class is
    highly customizable with regard to exactly how this division
    occurs, but it also has defaults that are suitable for many
    languages. This class assumes that the character values read
    from the string lie in the range 0-255. For example, the
    Unicode value of a capital A is 65, so
    <code> System.out.println((char)65); </code> prints out a
    capital A.
    <p>
    The behavior of a tokenizer depends on its character state
    table. This table is an array of 256 <code>TokenizerState
    </code>  states. The state table decides which state to
    enter upon reading a character from the input string.
    <p>
    For example, by default, upon reading an 'A', a tokenizer
    will enter a "word" state. This means the tokenizer will
    ask a <code>WordState</code> object to consume the 'A',
    along with the characters after the 'A' that form a word.
    The state's responsibility is to consume characters and
    return a complete token.
    <p>
    The default table sets a SymbolState for every character
    from 0 to 255, and then overrides this with:
    <blockquote><pre>
        From    To     State
          0     ' '    whitespaceState
         'a'    'z'    wordState
         'A'    'Z'    wordState
        160     255    wordState
         '0'    '9'    numberState
         '-'    '-'    numberState
         '.'    '.'    numberState
         '"'    '"'    quoteState
        '\''   '\''    quoteState
         '/'    '/'    slashState
    </pre></blockquote>
    In addition to allowing modification of the state table,
    this class makes each of the states above available. Some
    of these states are customizable. For example, wordState
    allows customization of what characters can be part of a
    word, after the first character.
  }
  IZTokenizer = interface (IZInterface)
    ['{C7CF190B-C45B-4AB4-A406-5999643DF6A0}']

    function TokenizeBufferToList(const Buffer: string; Options: TZTokenOptions): TZTokenList; overload;
    function TokenizeBufferToList(Buffer, NTerm: PChar; Options: TZTokenOptions): TZTokenList; overload;
    function TokenizeStreamToList(Stream: TStream; Options: TZTokenOptions): TZTokenList;

    function TokenizeBuffer(const Buffer: string; Options: TZTokenOptions): TZTokenDynArray; deprecated;
    function TokenizeStream(Stream: TStream; Options: TZTokenOptions): TZTokenDynArray; deprecated;

    function NormalizeParamToken(const Token: TZToken; out ParamName: String): String;

    function GetCommentState: TZCommentState;
    function GetNumberState: TZNumberState;
    function GetQuoteState: TZQuoteState;
    function GetSymbolState: TZSymbolState;
    function GetWhitespaceState: TZWhitespaceState;
    function GetWordState: TZWordState;
    function GetCharacterState(StartChar: Char): TZTokenizerState;
  end;

  {** Implements a default tokenizer object. }
  TZTokenizer = class (TZAbstractObject, IZTokenizer)
  private
    FCharacterStates: array[0..ord(high(char))] of TZTokenizerState;
    FCommentState: TZCommentState;
    FNumberState: TZNumberState;
    FQuoteState: TZQuoteState;
    FSymbolState: TZSymbolState;
    FWhitespaceState: TZWhitespaceState;
    FWordState: TZWordState;
    FBuffer: String;
  protected
    procedure CreateTokenStates; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function TokenizeBufferToList(const Buffer: string; Options: TZTokenOptions): TZTokenList; overload;
    function TokenizeBufferToList(Buffer, EOS: PChar; Options: TZTokenOptions): TZTokenList; overload;
    function TokenizeStreamToList(Stream: TStream; Options: TZTokenOptions): TZTokenList;

    function TokenizeBuffer(const Buffer: string; Options: TZTokenOptions):
      TZTokenDynArray;
    function TokenizeStream(Stream: TStream; Options: TZTokenOptions):
      TZTokenDynArray;

    function NormalizeParamToken(const Token: TZToken; out ParamName: String): String; virtual;

    function GetCharacterState(StartChar: Char): TZTokenizerState;
    procedure SetCharacterState(FromChar, ToChar: Char; State: TZTokenizerState);

    function GetCommentState: TZCommentState;
    function GetNumberState: TZNumberState;
    function GetQuoteState: TZQuoteState;
    function GetSymbolState: TZSymbolState;
    function GetWhitespaceState: TZWhitespaceState;
    function GetWordState: TZWordState;

    property CommentState: TZCommentState read FCommentState write FCommentState;
    property NumberState: TZNumberState read FNumberState write FNumberState;
    property QuoteState: TZQuoteState read FQuoteState write FQuoteState;
    property SymbolState: TZSymbolState read FSymbolState write FSymbolState;
    property WhitespaceState: TZWhitespaceState read FWhitespaceState
      write FWhitespaceState;
    property WordState: TZWordState read FWordState write FWordState;
  end;

function TokenAsString(const Value: TZToken): String;

const
  pSpace: PChar = ' ';
  pQuestionMark: PChar = '?';

implementation

uses
  ZFastCode, Math, ZSysUtils;

function TokenAsString(const Value: TZToken): String;
begin
  SetString(Result, Value.P, Value.L);
end;

{ TZNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZNumberState.ReadDecDigits(var SPos: PChar; const NTerm: PChar): Boolean;
begin
  Result := False;
  while (SPos < NTerm) and (Ord(SPos^) >= Ord('0')) and (Ord(SPos^) <= Ord('9')) do begin
    Inc(SPos);
    Result := True;
  end;
end;

function TZNumberState.ReadHexDigits(var SPos: PChar; const NTerm: PChar): Boolean;
begin
  Result := False;
  while (SPos < NTerm) and (((Ord(SPos^) >= Ord('0')) and (Ord(SPos^) <= Ord('9'))) or
       ((Ord(SPos^) or $20 >= Ord('a')) and (Ord(SPos^) or $20 <= Ord('f')))) do begin//lower case version
    Inc(SPos);
    Result := True;
  end;
end;

function TZNumberState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
var
  AbsorbedDot: Boolean;
  GotAdigit: Boolean;
begin
  { Initializes the process. }
  AbsorbedDot := False;

  Result.TokenType := ttUnknown;
  Result.P := SPos;

  { Parses left part of the number. }
  if (SPos^ = '-') or (SPos^ = '+') then
    Inc(SPos);
  GotAdigit := ReadDecDigits(SPos, NTerm);

  { Parses right part of the number. }
  if SPos^ = '.' then begin
    AbsorbedDot := True;
    Inc(SPos);
    GotAdigit := ReadDecDigits(SPos, NTerm);
  end;

  Dec(SPos); //push back wrong result

  { Gets a token result. }
  if not GotAdigit then begin
    SPos := Result.P;
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer);
  end else begin
    if AbsorbedDot
    then Result.TokenType := ttFloat
    else Result.TokenType := ttInteger;
    Result.L := SPos-Result.P+1;
  end;
end;

{ TZQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZQuoteState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.P := SPos;
  while SPos < NTerm do begin
    Inc(SPos);
    if SPos^ = Result.P^ then
      Break;
  end;
  Result.L := SPos-Result.P+1;
  Result.TokenType := ttQuoted;
end;

{**
  Decodes a string value.
  @param Value a token value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZQuoteState.DecodeToken(const Value: TZToken; QuoteChar: Char): string;
begin
  if (Value.L >= 2) and (Value.P^ = QuoteChar) and ((Value.P+Value.L-1)^ = QuoteChar)
  then SetString(Result, Value.P+1, Value.L-2)
  else SetString(Result, Value.P, Value.L)
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  Result := QuoteChar + Value + QuoteChar;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
var Token: TZToken;
begin
  Token.P := Pointer(Value);
  Token.L := Length(Value);
  Result := DecodeToken(Token, QuoteChar);
end;

{ TZCommentState }

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCommentState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.P := SPos;

  while (SPos < NTerm) and not (Ord(SPos^) in [Ord(#10), Ord(#13)]) do
    Inc(SPos);

  if (Ord(SPos^) in [Ord(#10), Ord(#13)]) then
    Dec(SPos);

  Result.L := SPos-Result.P+1;
  Result.TokenType := ttComment;
end;

{ TZCppCommentState }

{**
  Ignore everything up to a closing star and slash, and
  then return the tokenizer's next token.
  @return the tokenizer's next token
}
procedure TZCppCommentState.GetMultiLineComment(var SPos: PChar; const NTerm: PChar);
var
  LastChar: Char;
begin
  LastChar := SPos^;
  while SPos < NTerm do begin
    Inc(SPos);
    if (LastChar = '*') and (SPos^ = '/') then
      Break;
    LastChar := SPos^;
  end;
end;

{**
  Ignore everything up to an end-of-line and return the tokenizer's next token.
  @return the tokenizer's next token
}
procedure TZCppCommentState.GetSingleLineComment(var SPos: PChar; const NTerm: PChar);
begin
  // mdaems : for single line comments the line ending must be included
  // as it should never be stripped off or unified with other whitespace characters
  // ludob Linux line terminator is just LF, don't read further if we already have LF
  while (SPos < NTerm) and (Ord(SPos^) <> Ord(#10)) do
    Inc(SPos);
end;

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCppCommentState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttUnknown;
  Result.P := SPos;

  Inc(SPos);
  if SPos < NTerm then
    case SPos^ of
      '*':
        begin
          Result.TokenType := ttComment;
          GetMultiLineComment(SPos, NTerm);
          Result.L := SPos-Result.P+1;
          Exit;
        end;
      '/', '-':
        begin
          Result.TokenType := ttComment;
          GetSingleLineComment(SPos, NTerm);
          Result.L := SPos-Result.P+1;
          Exit;
        end;
      else
        Dec(SPos);
    end;

  if Tokenizer.SymbolState <> nil
  then Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer)
  else Result.L := SPos-Result.P+1;
end;

{ TZCCommentState }

{**
  Gets a C specific comments like /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCCommentState.NextToken(var SPos: PChar; const NTerm: PChar;
   Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttUnknown;
  Result.P := SPos;

  if SPos^ = '/' then
    if SPos < NTerm then begin
      Inc(SPos);
      if SPos^ = '*' then begin
        Result.TokenType := ttComment;
        GetMultiLineComment(SPos, NTerm);
      end else
        Dec(SPos);
    end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil)
  then Result := Tokenizer.SymbolState.NextToken(SPos, NTerm, Tokenizer)
  else Result.L := SPos-Result.P+1;
end;

{ TZSymbolNode }

{**
  Constructs a SymbolNode with the given parent, representing
  the given character.
  @param Parent this node's parent
  @param Character this node's character
}
constructor TZSymbolNode.Create(Parent: TZSymbolNode; Character: Char);
begin
  FParent := Parent;
  FCharacter := Character;
  FValid := False;
  SetLength(FChildren, 256);
end;

{**
  Destroys this symbol object and cleanups the memory.
}
destructor TZSymbolNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to 255 do
    if FChildren[I] <> nil
    then FreeAndNil(FChildren[I])
    else Break;
  SetLength(FChildren, 0);
  FParent := nil;
  inherited Destroy;
end;

{**
  Add a line of descendants that represent the characters in the given string.
}
procedure TZSymbolNode.AddDescendantLine(const Value: string);
var
  Node: TZSymbolNode;
  P: PChar absolute Value;
begin
  if P <> nil then begin
    Node := EnsureChildWithChar(P^);
    Node.AddDescendantLine(Copy(Value, 2, Length(Value) - 1));
  end;
end;

{**
  Show the symbol this node represents.
  @return the symbol this node represents
}
function TZSymbolNode.Ancestry: string;
begin
  Result := FParent.Ancestry + FCharacter;
end;

{**
  Find the descendant that takes as many characters as possible from the input.
}
function TZSymbolNode.DeepestRead(var SPos: PChar; const NTerm: PChar): TZSymbolNode;
var
  Node: TZSymbolNode;
begin
  Inc(SPos);
  if (SPos < NTerm)
  then Node := FindChildWithChar(SPos^)
  else Node := nil;

  if Node = nil then begin
    Dec(SPos);
    Result := Self;
  end else
    Result := Node.DeepestRead(SPos, NTerm);
end;

{**
  Find or create a child for the given character.
}
function TZSymbolNode.EnsureChildWithChar(Value: Char): TZSymbolNode;
var
  N: Integer;
begin
  Result := FindChildWithChar(Value);
  if Result = nil then begin
    N := 0;
    while (FChildren[N] <> nil) and (N <= 255) do
      Inc(N);
    if N <= 255 then begin
      Result := TZSymbolNode.Create(Self, Value);
      FChildren[N] := Result;
    end;
  end;
end;

{**
  Find a child with the given character.
}
function TZSymbolNode.FindChildWithChar(Value: Char): TZSymbolNode;
var
  I: Integer;
  Current: TZSymbolNode;
begin
  Result := nil;
  for I := 0 to 255 do begin
    Current := Children[I];
    if (Current = nil) or (Current.Character = Value) then begin
      Result := Current;
      Break;
    end;
  end;
end;

{**
  Find a descendant which is down the path the given string indicates.
}
function TZSymbolNode.FindDescendant(const Value: string): TZSymbolNode;
var
  TempChar: Char;
  P: PChar absolute Value;
begin
  if P <> nil
  then TempChar := P^
  else TempChar := #0;
  Result := FindChildWithChar(TempChar);
  if (Length(Value) > 1) and (Result <> nil) then
    Result := Result.FindDescendant(Copy(Value, 2, Length(Value) - 1));
end;

{**
  Unwind to a valid node; this node is "valid" if its
  ancestry represents a complete symbol. If this node is
  not valid, put back the character and ask the parent to unwind.
}
function TZSymbolNode.UnreadToValid(var SPos: PChar; const NTerm: PChar): TZSymbolNode;
begin
  if not FValid then begin
    Dec(SPos);
    Result := FParent.UnreadToValid(SPos, NTerm);
  end else
    Result := Self;
end;

{ TZSymbolRootNode }

{**
  Create and initialize a root node.
}
constructor TZSymbolRootNode.Create;
var
  I: Integer;
begin
  inherited Create(nil, #0);

  for I := 0 to 255 do
  begin
    FChildren[I] := TZSymbolNode.Create(Self, Chr(I));
    FChildren[I].Valid := True;
  end;
end;

{**
  Add the given string as a symbol.
  @param   String   the character sequence to add
}
procedure TZSymbolRootNode.Add(const Value: string);
var
  TempChar: Char;
  Node: TZSymbolNode;
  P: PChar absolute Value;
begin
  if P <> nil
  then TempChar := P^
  else TempChar := #0;
  Node := EnsureChildWithChar(TempChar);
  Node.AddDescendantLine(Copy(Value, 2, Length(Value) - 1));
  FindDescendant(Value).Valid := True;
end;

{**
  A root node has no parent and no character of its own, so its ancestry is "".
  @return an empty string
}
function TZSymbolRootNode.Ancestry: string;
begin
  Result := '';
end;

{**
  A root node maintains its children in an array instead of
  a Vector, to be faster.
}
function TZSymbolRootNode.FindChildWithChar(Value: Char): TZSymbolNode;
begin
  Result := FChildren[Ord(Value)];
end;

{**
  Return a symbol string from a reader.

  @param Stream a reader to read from
  @param FirstChar the first character of this symbol, already
    read from the reader
  @return a symbol string from a reader
}
function TZSymbolRootNode.NextSymbol(var SPos: PChar; const NTerm: PChar): PChar;
var
  Node: TZSymbolNode;
begin
  Node := FindChildWithChar(SPos^);
  Node := Node.DeepestRead(SPos, NTerm);
  Node.UnreadToValid(SPos, NTerm);
  Result := SPos;
end;

{ TZSymbolState }

{**
  Constructs a symbol state with a default idea of what
  multi-character symbols to accept (as described in the class comment).
}
constructor TZSymbolState.Create;
begin
  FSymbols := TZSymbolRootNode.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSymbolState.Destroy;
begin
  FSymbols.Free;
  inherited Destroy;
end;

{**
  Add a multi-character symbol.
  @param Value the symbol to add, such as "=:="
}
procedure TZSymbolState.Add(const Value: string);
begin
  FSymbols.Add(Value);
end;

{**
  Return a symbol token from a reader.
  @return a symbol token from a reader
}
function TZSymbolState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttSymbol;
  Result.P := SPos;
  Result.L := FSymbols.NextSymbol(SPos, NTerm)-Result.P+1;
end;

{ TZWhitespaceState }

{**
  Constructs a whitespace state with a default idea of what
  characters are, in fact, whitespace.
}
constructor TZWhitespaceState.Create;
begin
  SetWhitespaceChars(' ', high(char), False);
  SetWhitespaceChars(Chr(0), ' ', True);
end;

{**
  Ignore whitespace (such as blanks and tabs), and return
  the tokenizer's next token.
  @return the tokenizer's next token
}
function TZWhitespaceState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.P := SPos;
  while SPos < NTerm do begin
    Inc(SPos);
    if (SPos = NTerm) or not FWhitespaceChars[Ord(SPos^)] then
      Break;
  end;

  Dec(SPos);
  Result.L := SPos - Result.P +1;
  Result.TokenType := ttWhitespace;
end;

{**
  Establish the given characters as whitespace to ignore.
  @param FromChar first character index.
  @param ToChar last character index.
  @param Enable true, if this state should ignore characters in the given range
}
procedure TZWhitespaceState.SetWhitespaceChars(FromChar, ToChar: Char;
  Enable: Boolean);
var
  I: Integer;
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), 255]) do
    FWhitespaceChars[I] := Enable;
end;

{ TZWordState }

{**
  Constructs a word state with a default idea of what characters
  are admissible inside a word (as described in the class comment).
}
constructor TZWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('-', '-', True);
  SetWordChars('_', '_', True);
  SetWordChars('''', '''', True);
end;

{**
  Return a word token from a reader.
  @return a word token from a reader
}
function TZWordState.NextToken(var SPos: PChar; const NTerm: PChar;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.P := SPos;
  repeat
    Inc(SPos);
    if (SPos = NTerm) or not FWordChars[Ord(SPos^)] then
      Break;
  until False;

  Dec(SPos);
  Result.L := SPos - Result.P +1;
  Result.TokenType := ttWord;
end;

{**
  Establish characters in the given range as valid
  characters for part of a word after the first character.
  Note that the tokenizer must determine which characters
  are valid as the beginning character of a word.
  @param FromChar first character index.
  @param ToChar last character index.
  @param Enable true, if this state should ignore characters in the given range
}
procedure TZWordState.SetWordChars(FromChar, ToChar: Char; Enable: Boolean);
var
  I: Integer;
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), Ord(high(char)) ]) do
    FWordChars[I] := Enable;
end;

{ TZTokenizer }

{**
  Constructs a tokenizer with a default Stream reader).
}
constructor TZTokenizer.Create;
begin
  CreateTokenStates;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZTokenizer.Destroy;
begin
  FreeAndNil(FCommentState);
  FreeAndNil(FNumberState);
  FreeAndNil(FQuoteState);
  FreeAndNil(FSymbolState);
  FreeAndNil(FWhitespaceState);
  FreeAndNil(FWordState);

  inherited Destroy;
end;

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZTokenizer.CreateTokenStates;
begin
  FSymbolState := TZSymbolState.Create;
  with TZSymbolState(FSymbolState) do
  begin
    Add('<>');
    Add('<=');
    Add('>=');
  end;
  FNumberState := TZNumberState.Create;
  FQuoteState := TZQuoteState.Create;
  FWhitespaceState := TZWhitespaceState.Create;
  FWordState := TZWordState.Create;
  FCommentState := TZCppCommentState.Create;

  SetCharacterState(#0, #32, FWhitespaceState);
  SetCharacterState(#33, #191, FSymbolState);
  SetCharacterState(#192, High(Char), FWordState);

  SetCharacterState('a', 'z', FWordState);
  SetCharacterState('A', 'Z', FWordState);
  SetCharacterState('0', '9', FNumberState);
  SetCharacterState('-', '-', FNumberState);
  SetCharacterState('.', '.', FNumberState);
  SetCharacterState('"', '"', FQuoteState);
  SetCharacterState('''', '''', FQuoteState);
  SetCharacterState('/', '/', FCommentState);
end;
{**
  Gets an initial state object for the specified character.
  @return an initial state object for the character.
}
function TZTokenizer.GetCharacterState(StartChar: Char): TZTokenizerState;
begin
  Result := FCharacterStates[Ord(StartChar)];
end;

{**
  Change the state the tokenizer will enter upon reading
  any character between "from" and "to".

  @param FromChar first character index.
  @param ToChar last character index.
  @param State the state to enter upon reading a
    character between "fromChar" and "toChar"
}
procedure TZTokenizer.SetCharacterState(FromChar, ToChar: Char;
  State: TZTokenizerState);
var
  I: Integer;
const
  ORDMAXCHAR = ord(high(char));
begin
  for I := Ord(FromChar) to MinIntValue([Ord(ToChar), ORDMAXCHAR]) do
    FCharacterStates[I] := State;
end;

{**
  Tokenizes a string buffer into a dynamic array of tokens.
  @param Buffer a string buffer to be tokenized.
  @param Options a set of tokenizer options.
  @returns a dynamic array of tokens
}
function TZTokenizer.TokenizeBuffer(const Buffer: string;
  Options: TZTokenOptions): TZTokenDynArray;
var
  List: TZTokenList;
  I: Integer;
begin
  {$IFDEF FPC} Result := nil;{$ENDIF}
  FBuffer := Buffer;
  List := Self.TokenizeBufferToList(FBuffer, Options);
  try
    SetLength(Result, List.Count);
    for I := 0  to List.Count - 1 do
      Result[I] := List[I]^;
  finally
    List.Free;
  end;
end;

{**
  Tokenizes a string buffer into a list of tokens.
  @param Buffer a string buffer to be tokenized.
  @param Options a set of tokenizer options.
  @returns a string list where Items are tokens and
    Objects are token types.
}
function TZTokenizer.TokenizeBufferToList(const Buffer: string;
  Options: TZTokenOptions): TZTokenList;
var
  P: PChar;
begin
  FBuffer := Buffer;
  P := Pointer(FBuffer);
  Result := TokenizeBufferToList(P, p+Length(Buffer), Options);
end;

{**
  Tokenizes a string buffer into a list of tokens.
  @param Buffer a string buffer to be tokenized.
  @param EOS the end of the buffer. Usually the trailing #0 term.
  @param Options a set of tokenizer options.
  @returns a token list where Items are tokens and
    Objects are token types.
}
function TZTokenizer.TokenizeBufferToList(Buffer, EOS: PChar;
  Options: TZTokenOptions): TZTokenList;
var
  Token: TZToken;
  LastTokenType: TZTokenType;
  State: TZTokenizerState;
label EOL; //End Of Loop
begin
  Result := TZTokenList.Create;
  LastTokenType := ttUnknown;

  if Result.FCapacity < (EOS-Buffer) shr 5 then
    Result.SetCapacity((EOS-Buffer) shr 5);

  while Buffer < EOS do begin
    State := FCharacterStates[Ord(Buffer^)];
    if State <> nil then
    begin
      Token := State.NextToken(Buffer, EOS, Self);
      (*{ Decode strings.
      if (State is TZQuoteState) and (toDecodeStrings in Options) then begin
        Token.Value := (State as TZQuoteState).DecodeString(TokenAsString(Token), Token.P^);
        Token.P := Pointer(Token.Value);
        Token.L := Length(Token.Value);
      end;
      { Skips comments if option set. }*)
      if (Token.TokenType = ttComment) and (toSkipComments in Options) then
        Goto EOL;
      { Skips whitespaces if option set. }
      if (Token.TokenType = ttWhitespace) and (toSkipWhitespaces in Options) then
        goto EOL;
      { Unifies whitespaces if option set. }
      if (Token.TokenType = ttWhitespace) and (toUnifyWhitespaces in Options) then begin
        if LastTokenType = ttWhitespace then goto EOL;
        if (Token.P^ <> ' ') then
          Token.P := pSpace;
        Token.L := 1;
      end;
      { Unifies numbers if option set. }
      if (Token.TokenType in [ttInteger, ttFloat, ttHexDecimal]) and (toUnifyNumbers in Options) then
        Token.TokenType := ttNumber;
      { If an integer is immediately followed by a string they should be seen as one string}
      if (Token.TokenType = ttWord) and (LastTokenType = ttInteger) then begin
        Token.P := Result[Result.Count-1]^.P;
        Token.L := Token.L + Result[Result.Count-1]^.L;
        Result.Delete(Result.Count-1);
      end;
      { Add a read token. }
      LastTokenType := Token.TokenType;
      Result.Add(Token);
    { Skips unknown chars if option set. }
    end else if not (toSkipUnknown in Options) then begin
      Token.P := Buffer;
      Token.L := 0;
      Token.TokenType := ttUnknown;
      Result.Add(Token);
    end;
EOL:Inc(Buffer);
  end;
  { Adds an EOF if option is not set. }
  if not (toSkipEOF in Options) then begin
    Token.P := EOS;
    Token.L := 0;
    Token.TokenType := ttEOF;
    Result.Add(Token);
  end;
end;

{**
  Tokenizes a stream into a dynamic array of tokens.
  @param Stream a stream to be tokenized.
  @param Options a set of tokenizer options.
  @returns a dynamic array of tokens
}
function TZTokenizer.TokenizeStream(Stream: TStream;
  Options: TZTokenOptions): TZTokenDynArray;
var
  I: Integer;
  List: TZTokenList;
begin
  List := TokenizeStreamToList(Stream, Options);
  {$IFDEF FPC} Result := nil;{$ENDIF}
  try
    SetLength(Result, List.Count);
    for I := 0  to List.Count - 1 do
      Result[I] := List[I]^;
  finally
    List.Free;
  end;
end;

{**
  Tokenizes a stream into a string list of tokens.
  @param Stream a stream to be tokenized.
  @param Options a set of tokenizer options.
  @returns a string list where Items are tokens and
    Objects are token types.
}
function TZTokenizer.TokenizeStreamToList(Stream: TStream;
  Options: TZTokenOptions): TZTokenList;
begin
  Result := TokenizeBufferToList(PChar(TMemoryStream(Stream).Memory),
    PChar(TMemoryStream(Stream).Memory)+(Stream.Size div SizeOf(Char)), Options);
end;

{**
  Gets a tokenizer default comment state.
  @returns a tokenizer default comment state.
}
function TZTokenizer.GetCommentState: TZCommentState;
begin
  Result := CommentState;
end;

{**
  Gets a tokenizer default number state.
  @returns a tokenizer default number state.
}
function TZTokenizer.GetNumberState: TZNumberState;
begin
  Result := NumberState;
end;

{**
  Gets a tokenizer default quote state.
  @returns a tokenizer default quote state.
}
function TZTokenizer.GetQuoteState: TZQuoteState;
begin
  Result := QuoteState;
end;

{**
  Gets a tokenizer default symbol state.
  @returns a tokenizer default symbol state.
}
function TZTokenizer.GetSymbolState: TZSymbolState;
begin
  Result := SymbolState;
end;

{**
  Gets a tokenizer default whitespace state.
  @returns a tokenizer default whitespace state.
}
function TZTokenizer.GetWhitespaceState: TZWhitespaceState;
begin
  Result := WhitespaceState;
end;

{**
  Gets a tokenizer default word state.
  @returns a tokenizer default word state.
}
function TZTokenizer.GetWordState: TZWordState;
begin
  Result := WordState;
end;

{** EH: Noramlize the paremter token to a valid datbase SQL grammar
  Parameter
}
function TZTokenizer.NormalizeParamToken(const Token: TZToken;
  out ParamName: String): String;
begin
  Result := '?';
  if (Token.L >= 2) and (Ord(Token.P^) in [Ord(#39), Ord('`'), Ord('"'), Ord('[')])
  then ParamName := GetQuoteState.DecodeToken(Token, Token.P^)
  else System.SetString(ParamName, Token.P, Token.L);
end;

{ TZTokenList }

{**
  Adds a new token at the and of this collection.
  @param Item an object to be added.
  @return a position of the added object.
}
function TZTokenList.Add(const Item: TZToken): Integer;
begin
  Result := FCount;
  Insert(Result, Item);
end;

{**
  Assignes source elements to this collection.
}
procedure TZTokenList.Assign(Source: TZTokenList);
begin
  SetCount(Source.Count);
  Move(Source.FTokens^, FTokens^, SizeOf(TZToken)*FCount);
end;

function TZTokenList.AsString(iStart, iEnd: Integer): String;
var
  i: Integer;
  P: PChar;
begin
  P := nil;
  {$R-}
  for i := iStart to iEnd do
    Inc(P, FTokens^[I].L);
  {$IFDEF FPC} Result := '';{$ENDIF}
  SetLength(Result, P-PChar(nil));
  P := Pointer(Result);
  for i := iStart to iEnd do begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FTokens^[I].P^, P^, FTokens^[I].L * SizeOf(Char));
    Inc(P, FTokens^[I].L);
  end;
 {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Clears the content of this collection.
}
procedure TZTokenList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{**
  Creates this collection and assignes main properties.
}
constructor TZTokenList.Create;
begin
  SetCapacity(32);
end;

{**
  Deletes an object from the specified position.
}
procedure TZTokenList.Delete(Index: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  Dec(FCount);
  if Index < FCount then begin
    {$R-}
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FTokens^[Index + 1], FTokens^[Index],
      (FCount - Index) * SizeOf(TZToken));
    FillChar(FTokens^[FCount], SizeOf(TZToken), #0);
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Destroys this object and frees the memory.
}
destructor TZTokenList.Destroy;
begin
  Clear;
  inherited;
end;

function TZTokenList.IsEqual(Index: Integer; const Value: String;
  TokenCase: TZTokenCase = tcSensitive): Boolean;
var
  Token: PZToken;
  P: PChar;
begin
  Token := GetToken(Index);
  Result := False;
  if Length(Value) = Token.L then begin
    P := Pointer(Value);
    if TokenCase = tcSensitive
    then Result := CompareMem(Token.P, P, Token.L*SizeOf(Char))
    else Result := SameText(Token.P, P, Token.L);
  end;
end;

function TZTokenList.IsEqual(Index: Integer; TokenType: TZTokenType;
  const Value: String; TokenCase: TZTokenCase): Boolean;
begin
  if GetToken(Index)^.TokenType = TokenType
  then Result := IsEqual(Index, Value, TokenCase)
  else Result := False;
end;

{$IFNDEF DISABLE_CHECKING}
class procedure TZTokenList.Error(const Msg: string; Data: Integer);
begin
  {$IFDEF FPC}
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
  {$ELSE}
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress
  {$ENDIF}
end;
{$ENDIF}

function TZTokenList.IsEqual(Index: Integer; const Value: Char): Boolean;
var Token: PZToken;
begin
  Token := GetToken(Index);
  Result := (Token^.L = 1) and (Token^.P^ = Value);
end;

{**
  Gets a collection element from the specified position.
  @param Index a position index of the element.
  @return a requested element.
}
function TZTokenList.Get(Index: Integer): TZToken;
begin
  Result := GetToken(Index)^;
end;

{**
  Gets a collection element from the specified position.
  @param Index a position index of the element.
  @return a requested element.
}
function TZTokenList.GetToken(Index: Integer): PZToken;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  Result := @FTokens^[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Increases an element count.
}
procedure TZTokenList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

{**
  Inserts an object into specified position.
  @param Index a position index.
  @param Item an object to be inserted.
}
procedure TZTokenList.Insert(Index: Integer; const Item: TZToken);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
  {$R-}
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FTokens^[Index], FTokens^[Index + 1],
      (FCount - Index) * SizeOf(TZToken));
  Move(Item.P, FTokens^[Index].P, SizeOf(TZToken));
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  Inc(FCount);
end;

procedure TZTokenList.Put(Index: Integer; const Item: TZToken);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  FTokens^[Index] := Item;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Sets a new list capacity.
  @param NewCapacity a new list capacity.
}
procedure TZTokenList.SetCapacity(NewCapacity: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCapacity < FCount) or (NewCapacity > {$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF}) then
    Error(SListCapacityError, NewCapacity);
  {$ENDIF}
  if NewCapacity <> FCapacity then begin
    ReallocMem(FTokens, NewCapacity * SizeOf(TZToken));
    if NewCapacity > FCapacity then
  {$R-}
      FillChar(FTokens^[FCount], (NewCapacity - FCapacity) * SizeOf(TZToken), #0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    FCapacity := NewCapacity;
  end;
end;

{**
  Sets a new element count.
  @param NewCount a new element count.
}
procedure TZTokenList.SetCount(NewCount: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCount < 0) or (NewCount > {$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF}) then
    Error(SListCountError, NewCount);
  {$ENDIF}
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  FCount := NewCount;
end;

{**
  convert a Token to a floating-point value
  @param Index of element.
}
function TZTokenList.AsFloat(Index: Integer): Extended;
{var
  Token: PZToken;
  C: Char;}
begin
  {Token := GetToken(Index);
  C := (Token.P+Token.L)^;
  Result := 0;
  (Token.P+Token.L)^ := #0;
  try
    SQLStrToFloatDef(Token.P, 0, Result, Token.L);
  finally
    (Token.P+Token.L)^ := C;
  end;     }
  Result := SQLStrToFloat(AsString(Index));
end;

{**
  convert a Token to a Int64 value
  @param Index of element.
}
function TZTokenList.AsInt64(Index: Integer): Int64;
{var
  Token: PZToken;
  C: Char;}
begin
  (*Token := GetToken(Index);
  C := (Token.P+Token.L)^;
  (Token.P+Token.L)^ := #0;
  try
  {$IFDEF UNICODE}
    Result := UnicodeToInt64Def(Token.P, 0);
  {$ELSE}
    Result := RawToInt64Def(Token.P, 0);
  {$ENDIF}
  finally
    (Token.P+Token.L)^ := C;
  end;*)
  Result := StrToInt64(AsString(Index));
end;

{**
  convert a Token to a string
  @param Index of element.
  @param TokenCase the result case of the token.
}
function TZTokenList.AsString(Index: Integer): String;
var
  Token: PZToken;
begin
  Token := GetToken(Index);
  SetString(Result, Token.P, Token.L);
end;

{**
  compose all Tokens to a string
  @result composed string from tokens.
}
function TZTokenList.AsString: String;
var
  i: Integer;
  P: PChar;
begin
  P := nil;
  {$R-}
  for i := 0 to FCount - 1 do
    Inc(P, FTokens^[I].L);
  {$IFDEF FPC} Result := '';{$ENDIF}
  SetLength(Result, P-PChar(Nil));
  P := Pointer(Result);
  for i := 0 to FCount - 1 do begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FTokens^[I].P^, P^, FTokens^[I].L * SizeOf(Char));
    Inc(P, FTokens^[I].L);
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

end.



