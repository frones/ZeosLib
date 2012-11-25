{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes and interfaces         }
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

unit ZTokenizer;

interface

{$I ZCore.inc}

uses
   Classes, SysUtils, ZClasses, ZCompatibility;

type

  {**
    Objects of this class represent a type of token,
    such as "number", "symbol" or "word".
  }
  TZTokenType = (ttUnknown, ttEOF, ttFloat, ttInteger, ttHexDecimal,
    ttNumber, ttSymbol, ttQuoted, ttQuotedIdentifier, ttWord, ttKeyword,
    ttWhitespace, ttComment, ttSpecial, ttTime, ttDate, ttDateTime, ttEscape);

  {**
    Defines options for tokenizing strings.
  }
  TZTokenOption = (toSkipUnknown, toSkipWhitespaces, toSkipComments,
    toSkipEOF, toUnifyWhitespaces, toUnifyNumbers, toDecodeStrings);
  TZTokenOptions = set of TZTokenOption;

  {**
    A token represents a logical chunk of a string. For
    example, a typical tokenizer would break the string
    <code>"1.23 <= 12.3"</code> into three tokens: the number
    1.23, a less-than-or-equal symbol, and the number 12.3. A
    token is a receptacle, and relies on a tokenizer to decide
    precisely how to divide a string into tokens.
  }
  TZToken = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    Value: string;
    TokenType: TZTokenType;
  end;

  {** Defines a dynamic array of tokens. }
  TZTokenDynArray = array of TZToken;

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
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; virtual; abstract;
  end;

  {**
    EgonHugeist:
    A <code>EsacapeState</code> object returns bininary/String-data from a reader.
    This
    state's idea is save work-around of DataSet given binary/String-Data.
    So it has some requirements to pick out this data from the SQL-
    String:

      First: We have to define one or some Chars to detect this state.
        Example: If data data was given like;
        ~<|:%d|<~'...Binary/StringData...'~<|:%d|<~
        we are able to predetect this State.

      Second: The parameter d represents an Integer(Count of Chars)
        if we do not use this it's possible that the Tokenizer is
        vinny-nilly on getting binary-Data!

      Third: The GenerigResolver who assambles the insert/update
        Statements has to add this ass prefix and suffix.

      Fourth: The User of this Component has to know this too. So has to do this
        previously if he want to insert/update binary-data in a self
        assembled Query. So i think it would be better to add an published
        read-only Property like:
          EscapeChars: String;

    If we did this corectly we are able to disassemble all queries and
      do execute the nessesary UTF8Encoding of the TZQuoteState and
      TZWordState which represents either Quoted-String-Data or
      Catalog/Table/Alias/Field name-spaces.

    This State is only neccessary for <code>Delphi12_UP</code> ( 2009 and later)
    and results of it's mixing nByte-Chars and binary-Data 1Byte-Chars.
  }
  TZEscapeState = class (TZTokenizerState)
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;
  {**
    A NumberState object returns a number from a reader. This
    state's idea of a number allows an optional, initial
    minus sign, followed by one or more digits. A decimal
    point and another string of digits may follow these digits.
  }
  TZNumberState = class (TZTokenizerState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
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
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; virtual;
    function DecodeString(const Value: string; QuoteChar: Char): string; virtual;
  end;

  {**
    A CommentState object returns a comment from a reader.
  }
  TZCommentState = class (TZTokenizerState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCppCommentState = class (TZCommentState)
  protected
    function GetMultiLineComment(Stream: TStream): string; virtual;
    function GetSingleLineComment(Stream: TStream): string; virtual;
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZCCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
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
    function DeepestRead(Stream: TStream): TZSymbolNode;
    function EnsureChildWithChar(Value: Char): TZSymbolNode;
    function FindChildWithChar(Value: Char): TZSymbolNode; virtual;
    function FindDescendant(const Value: string): TZSymbolNode;
    function UnreadToValid(Stream: TStream): TZSymbolNode;

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
    function NextSymbol(Stream: TStream; FirstChar: Char): string;
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

    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
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

    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
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

    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
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

    function TokenizeBufferToList(const Buffer: string; Options: TZTokenOptions):
      TStrings;
    function TokenizeStreamToList(Stream: TStream; Options: TZTokenOptions):
      TStrings;

    function TokenizeBuffer(const Buffer: string; Options: TZTokenOptions):
      TZTokenDynArray;
    function TokenizeStream(Stream: TStream; Options: TZTokenOptions):
      TZTokenDynArray;

    function GetCommentState: TZCommentState;
    function GetNumberState: TZNumberState;
    function GetQuoteState: TZQuoteState;
    function GetSymbolState: TZSymbolState;
    function GetWhitespaceState: TZWhitespaceState;
    function GetWordState: TZWordState;
    function GetCharacterState(StartChar: Char): TZTokenizerState;
    function AnsiGetEscapeString(const Ansi: ZAnsiString): String;
    {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}
    function GetEscapeString(const EscapeString: ZAnsiString): ZAnsiString;
    {$ELSE}
    function GetEscapeString(const EscapeString: String): String;
    {$IFEND}
    function TokenizeEscapeBufferToList(const SQL: String): TZTokenDynArray;
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
    FEscapeState: TZEscapeState; //EgonHugeist
  protected
    function CheckEscapeState(const ActualState: TZTokenizerState;
      Stream: TStream; const FirstChar: Char): TZTokenizerState; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function AnsiGetEscapeString(const EscapeString: ZAnsiString): String; virtual;
    {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}
    function GetEscapeString(const EscapeString: ZAnsiString): ZAnsiString;
    {$ELSE}
    function GetEscapeString(const EscapeString: String): String;
    {$IFEND}
    function TokenizeEscapeBufferToList(const SQL: String): TZTokenDynArray; virtual;
    function TokenizeBufferToList(const Buffer: string; Options: TZTokenOptions):
      TStrings;
    function TokenizeStreamToList(Stream: TStream; Options: TZTokenOptions):
      TStrings;

    function TokenizeBuffer(const Buffer: string; Options: TZTokenOptions):
      TZTokenDynArray;
    function TokenizeStream(Stream: TStream; Options: TZTokenOptions):
      TZTokenDynArray;

    function GetCharacterState(StartChar: Char): TZTokenizerState;
    procedure SetCharacterState(FromChar, ToChar: Char; State: TZTokenizerState);

    function GetEscapeState: TZEscapeState;
    function GetCommentState: TZCommentState;
    function GetNumberState: TZNumberState;
    function GetQuoteState: TZQuoteState;
    function GetSymbolState: TZSymbolState;
    function GetWhitespaceState: TZWhitespaceState;
    function GetWordState: TZWordState;

    property EscapeState: TZEscapeState read FEscapeState write FEscapeState;
    property CommentState: TZCommentState read FCommentState write FCommentState;
    property NumberState: TZNumberState read FNumberState write FNumberState;
    property QuoteState: TZQuoteState read FQuoteState write FQuoteState;
    property SymbolState: TZSymbolState read FSymbolState write FSymbolState;
    property WhitespaceState: TZWhitespaceState read FWhitespaceState
      write FWhitespaceState;
    property WordState: TZWordState read FWordState write FWordState;
  end;

const
  EscapeMarkSequence = String('~<|');


implementation

uses
  Math, StrUtils;

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}

{ TZEscapeState } //EgonHugeist

{**
  Return a quoted Escape-data-string of token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZEscapeState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  TempStr, LenString: string;
  I, IReadCount: Integer;

  function ReadNextCharToTempChar: Boolean;
  begin
    Result := True;
    if Stream.Read(TempChar, 1 * SizeOf(Char)) = 0 then
    begin
      Result := False;
      TempChar := #0;
    end
    else
      Inc(iReadCount);
  end;

  procedure RollbackStream;
  begin
    Stream.Seek(-(iReadCount * SizeOf(Char)), soFromCurrent);
    Result.Value := '';
    IReadCount := 0;
  end;

  function CheckMarkChars(Marks: String): Boolean;
  var
    iMark: Integer;
  begin
    Result := False;

    if ( TempChar = Copy(Marks, 1, 1) ) then
      for iMark := 2 to Length(Marks) do  //First Char was predetected
      begin
        if ReadNextCharToTempChar then
        begin
          if not ( TempChar = Copy(Marks, iMark, 1) ) then
          begin
            RollbackStream;
            Exit;
          end;
        end else
        begin
          RollbackStream;
          Exit;
        end;
      end
    else
    begin
      RollbackStream;
      Exit;
    end;
    Result := True;
  end;

  function ReadLengthString: String;
  var
    B: Boolean;
  begin
    Result := ''; //init value
    repeat
      B := ReadNextCharToTempChar;
      if B then
        if CharInSet(TempChar, ['0'..'9']) then
          Result := Result+TempChar;
    until ( not CharInSet(TempChar, ['0'..'9'])) or ( not B );
  end;
begin
  Result.TokenType := ttUnknown;
  Result.Value := '';

  iReadCount := 0; //init Value
  TempStr := '';
  TempChar := FirstChar; //FirstChar: ~

  if not CheckMarkChars(EscapeMarkSequence) then Exit;

  //All inMark-Chars where test.
  //Now Check for Numeric Chars until MarkOut was found or #0 was Resulted
  LenString := ReadLengthString;
  if LenString = '' then
  begin
    RollbackStream;
    Exit;
  end;

  //Now Check the TempChar for it's hits on cBinDetectCharsOut
  if not CheckMarkChars(ReverseString(EscapeMarkSequence)) then Exit;

  //OutMarks where Found too. So let's read the BinarayData to the TempStr
  //Including the Quotes
  for i := 0 to StrToInt(LenString) do
  begin
    if not ReadNextCharToTempChar then
      Exit
    else
      TempStr := TempStr + TempChar;
  end;
  //Done and still in here! Post Data to Result!
  Result.Value := Copy(TempStr, 1, Length(TempStr)-1);

  //Now Check for in Chars again..
  if not CheckMarkChars(EscapeMarkSequence) then Exit;
  //MarkIn-Chars where found now compare the read-length
  TempStr := LenString; //Save to before compare
  LenString := ReadLengthString;
  if ( LenString = '' ) or ( LenString <> TempStr ) then
  begin
    RollbackStream;
    Exit;
  end;

  //Now Check the TempChar for it's hits on Escape-Detect-CharsOut again..
  if not CheckMarkChars(ReverseString(EscapeMarkSequence)) then Exit;
  //MarkOut-Chars where found again now we are ready here

  //everything was fine! Now we are sure Escape data was here
  Result.TokenType := ttEscape;
  //End..
end;

{ TZNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadNum: Integer;
  AbsorbedLeadingMinus: Boolean;
  AbsorbedDot: Boolean;
  GotAdigit: Boolean;

  function AbsorbDigits: string;
  begin
    Result := '';
    while CharInSet(FirstChar, ['0'..'9']) do
    begin
      GotAdigit := True;
      Result := Result + FirstChar;
      ReadNum := Stream.Read(FirstChar, 1 * SizeOf(Char));
      if ReadNum = 0 then
        Break;
    end;
  end;

begin
  { Initializes the process. }
  ReadNum := 0;
  AbsorbedLeadingMinus := False;
  AbsorbedDot := False;
  GotAdigit := False;

  Result.TokenType := ttUnknown;
  Result.Value := '';

  { Parses left part of the number. }
  if FirstChar = '-' then
  begin
    ReadNum := Stream.Read(FirstChar, 1 * SizeOf(Char));
    Result.Value := '-';
    AbsorbedLeadingMinus := True;
  end;
  Result.Value := Result.Value + AbsorbDigits;

  { Parses right part of the number. }
  if FirstChar = '.' then
  begin
    AbsorbedDot := True;
    Result.Value := Result.Value + '.';
    ReadNum := Stream.Read(FirstChar, 1 * SizeOf(Char));
    if ReadNum > 0 then
      Result.Value := Result.Value + AbsorbDigits;
  end;

  { Pushback wrong symbols. }
  Stream.Seek(-ReadNum, soFromCurrent);

  { Gets a token result. }
  if not GotAdigit then
  begin
    if AbsorbedLeadingMinus and AbsorbedDot then
    begin
      Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
      if Tokenizer.SymbolState <> nil then
        Result := Tokenizer.SymbolState.NextToken(Stream, '-', Tokenizer);
    end
    else if AbsorbedLeadingMinus then
    begin
      if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, '-', Tokenizer)
    end
    else if AbsorbedDot then
    begin
      if Tokenizer.SymbolState <> nil then
        Result := Tokenizer.SymbolState.NextToken(Stream, '.', Tokenizer);
    end;
  end
  else
  begin
    if AbsorbedDot then
      Result.TokenType := ttFloat
    else
      Result.TokenType := ttInteger;
  end;
end;

{ TZQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  TempStr: string;
begin
  TempStr := FirstChar;
  repeat
    if Stream.Read(TempChar, 1 * SizeOf(Char)) = 0 then
      TempChar := FirstChar;
    TempStr := TempStr + TempChar;
  until TempChar = FirstChar;

  Result.TokenType := ttQuoted;
  Result.Value := TempStr;
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
begin
  if (Length(Value) >= 2) and (Value[1] = QuoteChar)
    and (Value[Length(Value)] = Value[1]) then
    Result := Copy(Value, 2, Length(Value) - 2)
  else
    Result := Value;
end;

{ TZBasicCommentState }

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadStr: string;
begin
  ReadStr := FirstChar;
  while (Stream.Read(ReadChar, 1 * SizeOf(Char)) > 0) and not CharInSet(ReadChar, [#10, #13]) do
      ReadStr := ReadStr + ReadChar;
    if CharInSet(ReadChar, [#10, #13]) then
    Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);

  Result.TokenType := ttComment;
  Result.Value := ReadStr;
end;

{ TZCppCommentState }

{**
  Ignore everything up to a closing star and slash, and
  then return the tokenizer's next token.
  @return the tokenizer's next token
}
function TZCppCommentState.GetMultiLineComment(Stream: TStream): string;
var
  ReadChar, LastChar: Char;
begin
  LastChar := #0;
  Result := '';
  while Stream.Read(ReadChar, 1 * SizeOf(Char)) > 0 do
  begin
    Result := Result + ReadChar;
    if (LastChar = '*') and (ReadChar = '/') then
      Break;
    LastChar := ReadChar;
  end;
end;

{**
  Ignore everything up to an end-of-line and return the tokenizer's next token.
  @return the tokenizer's next token
}
function TZCppCommentState.GetSingleLineComment(Stream: TStream): string;
var
  ReadChar: Char;
begin
  Result := '';
  while (Stream.Read(ReadChar, 1 * SizeOf(Char)) > 0) and not CharInSet(ReadChar, [#10, #13]) do
      Result := Result + ReadChar;

  // mdaems : for single line comments the line ending must be included
  // as it should never be stripped off or unified with other whitespace characters
  if CharInSet(ReadChar, [#10, #13]) then
    begin
      Result := Result + ReadChar;
      // ludob Linux line terminator is just LF, don't read further if we already have LF
      if (ReadChar<>#10) and (Stream.Read(ReadChar, 1 * SizeOf(Char)) > 0) then
        if CharInSet(ReadChar, [#10, #13]) then
          Result := Result + ReadChar
        else
          Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
    end;
end;

{**
  Either delegate to a comment-handling state, or return a
  token with just a slash in it.

  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCppCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  Result.TokenType := ttUnknown;
  Result.Value := FirstChar;

  ReadNum := Stream.Read(ReadChar, 1 * SizeOf(Char));
  if (ReadNum > 0) and (ReadChar = '*') then
  begin
    Result.TokenType := ttComment;
    Result.Value := '/*' + GetMultiLineComment(Stream);
  end
  else if (ReadNum > 0) and (ReadChar = '/') then
  begin
    Result.TokenType := ttComment;
    Result.Value := '//' + GetSingleLineComment(Stream);
  end
  else
  begin
    if ReadNum > 0 then
      Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end;
end;

{ TZCCommentState }

{**
  Gets a C specific comments like /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZCCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  Result.TokenType := ttUnknown;
  Result.Value := FirstChar;

  if FirstChar = '/' then
  begin
    ReadNum := Stream.Read(ReadChar, 1 * SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '*') then
    begin
      Result.TokenType := ttComment;
      Result.Value := '/*' + GetMultiLineComment(Stream);
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
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
  begin
    if FChildren[I] <> nil then
      FChildren[I].Free
    else
         Break;
  end;
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
begin
  if Length(Value) > 0 then
  begin
    Node := EnsureChildWithChar(Value[1]);
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
function TZSymbolNode.DeepestRead(Stream: TStream): TZSymbolNode;
var
  TempChar: Char;
  Node: TZSymbolNode;
  ReadNum: Integer;
begin
  ReadNum := Stream.Read(TempChar, 1 * SizeOf(Char));
  if ReadNum > 0 then
    Node := FindChildWithChar(TempChar)
  else
    Node := nil;

  if Node = nil then
  begin
    Stream.Seek(-ReadNum, soFromCurrent);
    Result := Self;
  end
  else
    Result := Node.DeepestRead(Stream);
end;

{**
  Find or create a child for the given character.
}
function TZSymbolNode.EnsureChildWithChar(Value: Char): TZSymbolNode;
var
  N: Integer;
begin
  Result := FindChildWithChar(Value);
  if Result = nil then
  begin
    N := 0;
    while (FChildren[N] <> nil) and (N <= 255) do
      Inc(N);
    if N <= 255 then
    begin
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
  for I := 0 to 255 do
  begin
    Current := Children[I];
    if (Current = nil) or (Current.Character = Value) then
    begin
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
begin
  if Length(Value) > 0 then
    TempChar := Value[1]
  else
    TempChar := #0;
  Result := FindChildWithChar(TempChar);
  if (Length(Value) > 1) and (Result <> nil) then
    Result := Result.FindDescendant(Copy(Value, 2, Length(Value) - 1));
end;

{**
  Unwind to a valid node; this node is "valid" if its
  ancestry represents a complete symbol. If this node is
  not valid, put back the character and ask the parent to unwind.
}
function TZSymbolNode.UnreadToValid(Stream: TStream): TZSymbolNode;
begin
  if not FValid then
  begin
    Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
    Result := FParent.UnreadToValid(Stream);
  end
  else
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
begin
  if Length(Value) > 0 then
    TempChar := Value[1]
  else
     TempChar := #0;
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
function TZSymbolRootNode.NextSymbol(Stream: TStream; FirstChar: Char): string;
var
  Node: TZSymbolNode;
begin
  Node := FindChildWithChar(FirstChar);
  Node := Node.DeepestRead(Stream);
  Node := Node.UnreadToValid(Stream);
  Result := Node.Ancestry;
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
function TZSymbolState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
begin
  Result.TokenType := ttSymbol;
  Result.Value := FSymbols.NextSymbol(Stream, FirstChar);
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
function TZWhitespaceState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadNum: Integer;
  ReadChar: Char;
  ReadStr: string;
begin
  ReadStr := FirstChar;
  ReadNum := 0;
  while True do
  begin
    ReadNum := Stream.Read(ReadChar, 1 * SizeOf(Char));
    if (ReadNum = 0) or not FWhitespaceChars[Ord(ReadChar)] then
      Break;
    ReadStr := ReadStr + ReadChar;
  end;

  if ReadNum > 0 then
    Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
  Result.TokenType := ttWhitespace;
  Result.Value := ReadStr;
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
function TZWordState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  ReadNum: Integer;
  Value: string;
begin
  Value := FirstChar;
  repeat
    ReadNum := Stream.Read(TempChar, 1 * SizeOf(Char));
    if (ReadNum = 0) or not FWordChars[Ord(TempChar)] then
      Break;
    Value := Value + TempChar;
  until False;

  if ReadNum > 0 then
    Stream.Seek(-(1 * SizeOf(Char)), soFromCurrent);
  Result.TokenType := ttWord;
  Result.Value := Value;
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
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZTokenizer.Create;
begin
  FSymbolState := TZSymbolState.Create;
  with TZSymbolState(FSymbolState) do
  begin
    Add('<>');
    Add('<=');
    Add('>=');
  end;
  FEscapeState := TZEscapeState.Create;
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
  Destroys this object and cleanups the memory.
}
destructor TZTokenizer.Destroy;
begin
  if FEscapeState <> nil then
    FEscapeState.Free;
  if FCommentState <> nil then
    FCommentState.Free;
  if FNumberState <> nil then
    FNumberState.Free;
  if FQuoteState <> nil then
    FQuoteState.Free;
  if FSymbolState <> nil then
    FSymbolState.Free;
  if FWhitespaceState <> nil then
    FWhitespaceState.Free;
  if FWordState <> nil then
    FWordState.Free;

  inherited Destroy;
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
  Stream: TStream;
begin
  {$IFDEF DELPHI12_UP}
  Stream := TStringStream.Create(Buffer, TEncoding.Unicode);
  {$ELSE}
  Stream := TStringStream.Create(Buffer);
  {$ENDIF}
  try
    Result := TokenizeStream(Stream, Options);
  finally
    Stream.Free;
  end;
end;

function TZTokenizer.AnsiGetEscapeString(const EscapeString: ZAnsiString): String;
var
  Temp: String;
begin
  Temp := EscapeMarkSequence+IntToStr(Length(EscapeString))+ReverseString(EscapeMarkSequence);

  if Length(EscapeString) > 0 then
    Result := Temp+String(EscapeString)+Temp
  else
    Result := 'NULL';
end;

{$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}
function TZTokenizer.GetEscapeString(const EscapeString: ZAnsiString): ZAnsiString;
{$ELSE}
function TZTokenizer.GetEscapeString(const EscapeString: String): String;
{$IFEND}
var
  Temp: String;
begin
  Temp := EscapeMarkSequence+IntToStr(Length(EscapeString))+ReverseString(EscapeMarkSequence);

  if Length(EscapeString) > 0 then
    {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}
    Result := ZAnsiString(Temp)+EscapeString+ZAnsiString(Temp)
    {$ELSE}
    Result := Temp+EscapeString+Temp
    {$IFEND}
  else
    Result := '';
end;

{**
  Tokenizes a string buffer into a dynamic array of tokens and informs
  the Tokenizer and EscapeState about the used sequence
  @param SQL a string buffer to be tokenized.
  @param EscapeMarkSequence to detect preprepared data
  @returns a dynamic array of tokens
}
function TZTokenizer.TokenizeEscapeBufferToList(const SQL: String): TZTokenDynArray;
begin
  Result := TokenizeBuffer(SQL, [toSkipEOF]); //Disassembles the Query
end;

{**
  EgonHugeist:
  Checks if SymboState is EscapeState and sets it ...
  @param Stream the Read-Stream which has to checked for Next-Chars.
  @FirstChar The FirstChar which was readed and sets the Symbolstate
  @returns either the given SymbolState or the EscapeState
}
function TZTokenizer.CheckEscapeState(const ActualState: TZTokenizerState;
  Stream: TStream; const FirstChar: Char): TZTokenizerState;
var
  NextChar: Char;
  iReadCount, I: Integer;
begin
  Result := ActualState;
  iReadCount := 0;
  if  ( FirstChar = EscapeMarkSequence[1]) then //Token was set so check if its Escape
  begin
    for i := 2 to Length(EscapeMarkSequence) do
      if Stream.Read(NextChar, 1 * SizeOf(Char)) > 0  then //Read next Char
      begin
        Inc(IReadCount); //increment count of read-Chars
        if NextChar <> Copy(EscapeMarkSequence, i, 1) then //Compare Chars
        begin
          Stream.Seek(-(iReadCount * SizeOf(Char)), soFromCurrent); //Seek Stream back to starting Position
          Exit;
        end
      end
      else
        Exit;
  end
  else
    Exit;
  Stream.Seek(-(iReadCount * SizeOf(Char)), soFromCurrent); //Seek Stream back to starting Position
  Result := Self.EscapeState;
end;

{**
  Tokenizes a string buffer into a list of tokens.
  @param Buffer a string buffer to be tokenized.
  @param Options a set of tokenizer options.
  @returns a string list where Items are tokens and
    Objects are token types.
}
function TZTokenizer.TokenizeBufferToList(const Buffer: string;
  Options: TZTokenOptions): TStrings;
var
  Stream: TStream;
begin
  {$IFDEF DELPHI12_UP}
  Stream := TStringStream.Create(Buffer, TEncoding.Unicode);
  {$ELSE}
  Stream := TStringStream.Create(Buffer);
  {$ENDIF}
  try
    Result := TokenizeStreamToList(Stream, Options);
  finally
    Stream.Free;
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
  List: TStrings;
begin
  List := TokenizeStreamToList(Stream, Options);
  try
    SetLength(Result, List.Count);
    for I := 0  to List.Count - 1 do
    begin
      Result[I].Value := List[I];
      Result[I].TokenType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
        List.Objects[I]{$IFDEF FPC}){$ENDIF});
    end;
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
  Options: TZTokenOptions): TStrings;
var
  FirstChar: Char;
  Token: TZToken;
  LastTokenType: TZTokenType;
  State: TZTokenizerState;
begin
  Result := TStringList.Create;
  LastTokenType := ttUnknown;

  while Stream.Read(FirstChar, 1 * SizeOf(Char)) > 0 do
  begin
    State := FCharacterStates[Ord(FirstChar)];
    if State <> nil then
    begin
      State := CheckEscapeState(State, Stream, FirstChar);

      Token := State.NextToken(Stream, FirstChar, Self);
      { Decode strings. }
      if (State is TZQuoteState)
        and (toDecodeStrings in Options) then
      begin
        Token.Value := (State as TZQuoteState).DecodeString(
          Token.Value, FirstChar);
      end;
      { Skips comments if option set. }
      if (Token.TokenType = ttComment)
        and (toSkipComments in Options) then
        Continue;
      { Skips whitespaces if option set. }
      if (Token.TokenType = ttWhitespace)
        and (toSkipWhitespaces in Options) then
        Continue;
      { Unifies whitespaces if option set. }
      if (Token.TokenType = ttWhitespace)
        and (toUnifyWhitespaces in Options) then
      begin
        if LastTokenType = ttWhitespace then
          Continue;
        Token.Value := ' ';
      end;
      { Unifies numbers if option set. }
      if (Token.TokenType in [ttInteger, ttFloat, ttHexDecimal])
        and (toUnifyNumbers in Options) then
        Token.TokenType := ttNumber;
      { If an integer is immediately followed by a string they should be seen as one string}
      if ((Token.TokenType = ttWord)and(LastTokenType = ttInteger)) then
      begin
        Token.Value := Result[Result.Count-1] + Token.Value;
        Result.Delete(Result.Count-1);
      end;
      { Add a read token. }
      LastTokenType := Token.TokenType;
      Result.AddObject(Token.Value, TObject(Ord(Token.TokenType)));
    end
    { Skips unknown chars if option set. }
    else if not (toSkipUnknown in Options) then
      Result.AddObject(FirstChar, TObject(Ord(ttUnknown)));
  end;
  { Adds an EOF if option is not set. }
  if not (toSkipEOF in Options) then
    Result.AddObject('', TObject(Ord(ttEOF)));
end;

{**
  Gets a tokenizer default Escape state.
  @returns a tokenizer default Escape state.
}
function TZTokenizer.GetEscapeState: TZEscapeState;
begin
  Result := EscapeState;
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

end.

