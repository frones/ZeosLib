{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Expression classes and interfaces           }
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

unit ZExpression;

interface

{$I ZCore.inc}

uses SysUtils, Classes, {$IF not defined(NO_UNIT_CONTNRS) and not defined(FPC)}Contnrs,{$IFEND} //used for inline methods
  ZClasses, ZCompatibility, ZVariant, ZTokenizer, ZExprParser;

type
  /// <summary>Defines an expression exception.</summary>
  TZExpressionError = class (Exception);

  /// <summary>Implements an execution stack object.</summary>
  TZExecutionStack = class(TObject)
  private
    FValues: TZVariantDynArray;
    FCount: Integer;
    FCapacity: Integer;
    /// <summary>Gets a value from absolute position in the stack.</summary>
    /// <param>"Index" a value index.</param>
    /// <returns>a variant value from requested position.</returns>
    function GetValue(Index: Integer): TZVariant;
  public
    /// <summary>Creates this object.</summary>
    constructor Create;

    procedure DecStackPointer(const Value : integer);
    /// <summary>Gets a value from the top and removes it from the stack.</summary>
    /// <returns>a value from the top.</returns>
    function Pop: TZVariant;
    /// <summary>Gets a value from the top of the stack without removing it.</summary>
    /// <returns>a value from the top.</returns>
    function Peek: TZVariant;
    /// <summary>Puts a value to the top of the stack.</summary>
    /// <param>"Value" the value to put on top af stack.</summary>
    procedure Push(const Value: TZVariant);
    /// <summary>Gets a function parameter by index.</summary>
    /// <param>"Index" a function parameter index. 0 is used for parameter count.</param>
    /// <returns>a parameter value.</returns>
    function GetParameter(Index: Integer): TZVariant;
    /// <summary>Swaps two values on the top of the stack.</summary>
    procedure Swap;
    /// <summary>Clears this stack.</summary>
    procedure Clear;
    /// <summary>Represents the stack count.</summary>
    property Count: Integer read FCount;
    /// <summary>Access the values by index position.</summary>
    property Values[Index: Integer]: TZVariant read GetValue;
  end;

  /// <summary>Defines a list of variables interface.</summary>
  IZVariablesList = interface (IZInterface)
    ['{F4347F46-32F3-4021-B6DB-7A39BF171275}']
    /// <summary>Gets a number of registered variables.</summary>
    /// <returns>a number of all registered variables.</returns>
    function GetCount: Integer;
    /// <summary>Gets a variable name by it's index.</summary>
    /// <param>"Index" a variable index.</param>
    /// <returns>a variable name.</returns>
    function GetName(Index: Integer): string;
    /// <summary>Gets a variable value by it's index.</summary>
    /// <param>"Index" a variable index.</param>
    /// <returns>a variable value.</returns>
    function GetValue(Index: Integer): TZVariant;
    /// <summary>Sets a variable by it's index.</summary>
    /// <param>"Index" a variable index.</param>
    /// <param>"Value" a variable value.</param>
    procedure SetValue(Index: Integer; const Value: TZVariant);
    /// <summary>Gets a variable value by it's name.</summary>
    /// <param>"Name" a variable name.</param>
    /// <returns>a variable value.</returns>
    function GetValueByName(const Name: string): TZVariant;
    /// <summary>Sets a variable by it's name.</summary>
    /// <param>"Name" a variable name.</param>
    /// <param>"Value" a variable value.</param>
    procedure SetValueByName(const Name: string; const Value: TZVariant);
    /// <summary>Adds a new variable with value.</summary>
    /// <param>"Name" a name of the new variable.</param>
    /// <param>"Value" a value for the new variable.</param>
    procedure Add(const Name: string; const Value: TZVariant);
    /// <summary>Removes a variable by specified name.</summary>
    /// <param>"Name" a name of variable to be removed.</param>
    procedure Remove(const Name: string);
    /// <summary>Finds a variable by specified name.</summary>
    /// <param>"Name" a name of the variable.</param>
    /// <returns>a found variable index or <c>-1</c> otherwise.</returns>
    function FindByName(const Name: string): Integer;
    /// <summary>Clears only variable values.</summary>
    procedure ClearValues;
    /// <summary>Clears all variables.</summary>
    procedure Clear;

    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Values[Index: Integer]: TZVariant read GetValue write SetValue;
    property NamedValues[const Index: string]: TZVariant read GetValueByName
      write SetValueByName;
  end;

  /// <summary>Defines a function interface.</summary>
  IZFunction = interface (IZInterface)
    ['{E9B3AFF9-6CD9-49C8-AB66-C8CF60ED8686}']
    /// <summary>Gets the assigned function name.</summary>
    /// <returns>the assigned function name.</returns>
    function GetName: string;
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
    /// <summary>represents the name of the function</summary>
    property Name: string read GetName;
  end;

  /// <summary>Defines a list of functions interface.</summary>
  IZFunctionsList = interface (IZInterface)
    ['{54453054-F012-475B-84C3-7E5C46187FDB}']
    /// <summary>Gets a number of registered functions.</summary>
    /// <returns>a number of registered functions.</summary>
    function GetCount: Integer;
    /// <summary>Gets a name of the functions by it's index.</summary>
    /// <param>"Index" a functon index.</param>
    /// <returns>a name of the function.</returns>
    function GetName(Index: Integer): string;
    /// <summary>Gets a function reference by it's index.</summary>
    /// <param>"Index" a function index.</param>
    /// <returns>a function reference.</returns>
    function GetFunction(Index: Integer): IZFunction;
    /// <summary>Adds a new function to this list.</summary>
    /// <param>"Func" a function reference.</param>
    procedure Add(const Func: IZFunction);
    /// <summary>Removes a reference to a function by it's name.</summary>
    /// <param>"Name" a name of the function to be removed.</param>
    procedure Remove(const Name: string);
    /// <summary>Finds a function reference by it's name.</summary>
    /// <param>"Name" a name of the function to be found.</param>
    function FindByName(const Name: string): Integer;
    /// <summary>Cleans the list of registered functions.</summary>
    procedure Clear;
    /// <summary>Represents a number of registered functions.</summary>
    property Count: Integer read GetCount;
    /// <summary>Represents a name of the functions by it's index.</summary>
    property Names[Index: Integer]: string read GetName;
    /// <summary>Represents a function reference by it's index.</summary>
    property Functions[Index: Integer]: IZFunction read GetFunction;
  end;

  /// <summary>Defines an interface to expression calculator.</summary>
  IZExpression = interface (IZInterface)
    ['{26F9D379-5618-446C-8999-D50FBB2F8560}']
    /// <summary>Gets the current expression tokenizer.</summary>
    /// <returns>the current expression tokenizer.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Sets a new expression tokenizer.</summary>
    /// <param>"Value" a new expression tokenizer.</param>
    procedure SetTokenizer(const Value: IZTokenizer);
    /// <summary>Gets the current set expression string.</summary>
    /// <returns>the current expression string.</returns>
    function GetExpression: string;
    /// <summary>Sets a new expression string.</summary>
    /// <param>"Value" a new expression string.</param>
    procedure SetExpression(const Value: string);
    /// <summary>Gets a reference to the current variant manager.</summary>
    /// <returns>a reference to the current variant manager.</returns>
    function GetVariantManager: IZVariantManager;
    /// <summary>Sets a new variant manager.</summary>
    /// <param>"Value" a new variant manager.</param>
    procedure SetVariantManager(const Value: IZVariantManager);
    /// <summary>Gets a list of default variables.</summary>
    /// <returns>a list of default variables.</returns>
    function GetDefaultVariables: IZVariablesList;
    /// <summary>Sets a new list of variables.</summary>
    /// <param>"Value" a new list of variables.</param>
    procedure SetDefaultVariables(const Value: IZVariablesList);
    /// <summary>Gets a list of default functions.</summary>
    /// <returns>a list of default functions.</returns>
    function GetDefaultFunctions: IZFunctionsList;
    /// <summary>Sets a new list of functions.</summary>
    /// <param>"Value" a new list of functions.</param>
    procedure SetDefaultFunctions(const Value: IZFunctionsList);
    /// <summary>Gets the current auto variables create flag.</summary>
    /// <returns>the auto variables create flag.</returns>
    function GetAutoVariables: Boolean;
    /// <summary>Sets a new auto variables create flag.</summary>
    /// <param>"Value" a new auto variables create flag.</param>
    procedure SetAutoVariables(Value: Boolean);
    /// <summary>Evaluates this expression.</summary>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate: TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate2(const Variables: IZVariablesList): TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <param>"Functions" a list of functions.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate3(const Variables: IZVariablesList;
      const Functions: IZFunctionsList): TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <param>"Functions" a list of functions.</param>
    /// <param>"Stack" an execution stack.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate4(const Variables: IZVariablesList;
      const Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;

    /// <summary>Creates an empty variables.</summary>
    /// <param>"Variables" a list of variables.</param>
    procedure CreateVariables(const Variables: IZVariablesList);
    /// <summary>Clears this class from all data.</summary>
    procedure Clear;

    property Tokenizer: IZTokenizer read GetTokenizer write SetTokenizer;
    property Expression: string read GetExpression write SetExpression;
    property VariantManager: IZVariantManager read GetVariantManager
      write SetVariantManager;
    property DefaultVariables: IZVariablesList read GetDefaultVariables
      write SetDefaultVariables;
    property DefaultFunctions: IZFunctionsList read GetDefaultFunctions
      write SetDefaultFunctions;
    property AutoVariables: Boolean read GetAutoVariables
      write SetAutoVariables;
  end;

  /// <summary>Implements an expression calculator class.</summary>
  TZExpression = class (TInterfacedObject, IZExpression)
  private
    FTokenizer: IZTokenizer;
    FDefaultVariables: IZVariablesList;
    FDefaultFunctions: IZFunctionsList;
    FVariantManager: IZVariantManager;
    FParser: TZExpressionParser;
    FAutoVariables: Boolean;
    /// <summary>Gets the current expression tokenizer.</summary>
    /// <returns>the current expression tokenizer.</returns>
    function GetTokenizer: IZTokenizer;
    /// <summary>Sets a new expression tokenizer.</summary>
    /// <param>"Value" a new expression tokenizer.</param>
    procedure SetTokenizer(const Value: IZTokenizer);
    /// <summary>Gets the current set expression string.</summary>
    /// <returns>the current expression string.</returns>
    function GetExpression: string;
    /// <summary>Sets a new expression string.</summary>
    /// <param>"Value" a new expression string.</param>
    procedure SetExpression(const Value: string);
    /// <summary>Gets a reference to the current variant manager.</summary>
    /// <returns>a reference to the current variant manager.</returns>
    function GetVariantManager: IZVariantManager;
    /// <summary>Sets a new variant manager.</summary>
    /// <param>"Value" a new variant manager.</param>
    procedure SetVariantManager(const Value: IZVariantManager);
    /// <summary>Gets a list of default variables.</summary>
    /// <returns>a list of default variables.</returns>
    function GetDefaultVariables: IZVariablesList;
    /// <summary>Sets a new list of variables.</summary>
    /// <param>"Value" a new list of variables.</param>
    procedure SetDefaultVariables(const Value: IZVariablesList);
    /// <summary>Gets a list of default functions.</summary>
    /// <returns>a list of default functions.</returns>
    function GetDefaultFunctions: IZFunctionsList;
    /// <summary>Sets a new list of functions.</summary>
    /// <param>"Value" a new list of functions.</param>
    procedure SetDefaultFunctions(const Value: IZFunctionsList);
    /// <summary>Gets the current auto variables create flag.</summary>
    /// <returns>the auto variables create flag.</returns>
    function GetAutoVariables: Boolean;
    /// <summary>Sets a new auto variables create flag.</summary>
    /// <param>"Value" a new auto variables create flag.</param>
    procedure SetAutoVariables(Value: Boolean);
  protected
    function NormalizeValues(var Val1, Val2: TZVariant): Boolean;
  public
    /// <summary>Creates this expression calculator object.</summary>
    constructor Create;
    /// <summary>Creates this expression calculator and assignes expression
    ///  string.</summary>
    /// <param>"Expression" an expression string.</param>
    constructor CreateWithExpression(const Expression: string);
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>Evaluates this expression.</summary>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate: TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate2(const Variables: IZVariablesList): TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <param>"Functions" a list of functions.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate3(const Variables: IZVariablesList;
      const Functions: IZFunctionsList): TZVariant;
    /// <summary>Evaluates this expression.</summary>
    /// <param>"Variables" a list of variables.</param>
    /// <param>"Functions" a list of functions.</param>
    /// <param>"Stack" an execution stack.</param>
    /// <returns>an evaluated expression value.</returns>
    function Evaluate4(const Variables: IZVariablesList;
      const Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;
    /// <summary>Creates an empty variables.</summary>
    /// <param>"Variables" a list of variables.</param>
    procedure CreateVariables(const Variables: IZVariablesList);
    /// <summary>Clears this class from all data.</summary>
    procedure Clear;

    property Expression: string read GetExpression write SetExpression;
    property VariantManager: IZVariantManager read GetVariantManager
      write SetVariantManager;
    property DefaultVariables: IZVariablesList read GetDefaultVariables
      write SetDefaultVariables;
    property DefaultFunctions: IZFunctionsList read GetDefaultFunctions
      write SetDefaultFunctions;
    property AutoVariables: Boolean read GetAutoVariables
      write SetAutoVariables;
  end;

implementation

uses
  ZMessages, ZExprToken, ZVariables, ZFunctions, ZMatchPattern;

{ TZExecutionStack }

constructor TZExecutionStack.Create;
begin
  FCapacity := 100;
  SetLength(FValues, FCapacity);
  FCount := 0;
end;

function TZExecutionStack.GetValue(Index: Integer): TZVariant;
begin
  Result := FValues[Index];
end;

function TZExecutionStack.Peek: TZVariant;
begin
  if FCount > 0 then
    Result := FValues[FCount - 1]
  else Result := NullVariant;
end;

function TZExecutionStack.GetParameter(Index: Integer): TZVariant;
begin
  if FCount <= Index then
    raise TZExpressionError.Create(SStackIsEmpty);
  Result := FValues[FCount - Index - 1];
end;

procedure TZExecutionStack.DecStackPointer(const Value : integer);
begin
  Dec(FCount, Value);
  if FCount < 0 then begin
    FCount := 0;
    raise TZExpressionError.Create(SStackIsEmpty);
  end;
end;

function TZExecutionStack.Pop: TZVariant;
begin
  Result := NullVariant;
  if FCount <= 0 then
    raise TZExpressionError.Create(SStackIsEmpty);
  Dec(FCount);
  Result := FValues[FCount];
end;

procedure TZExecutionStack.Push(const Value: TZVariant);
begin
  if FCapacity = FCount then begin
    Inc(FCapacity, 64);
    SetLength(FValues, FCapacity);
  end;
  SoftVarManager.Assign(Value, FValues[FCount]);
  {$IFDEF UNICODE}
  if Value.VUnicodeString <> '' then
    FValues[FCount].VUnicodeString := Value.VUnicodeString; //keep parsed value alive
  {$ELSE}
  if Value.VRawByteString <> '' then
    FValues[FCount].VRawByteString := Value.VRawByteString; //keep parsed value alive
  {$ENDIF}
  Inc(FCount);
end;

procedure TZExecutionStack.Swap;
var
  Temp: TZVariant;
begin
  if FCount <= 1 then
    raise TZExpressionError.Create(SStackIsEmpty);

  Temp := FValues[FCount - 1];
  FValues[FCount - 1] := FValues[FCount - 2];
  FValues[FCount - 2] := Temp;
end;

procedure TZExecutionStack.Clear;
begin
  FCount := 0;
end;

{ TZExpression }

constructor TZExpression.Create;
begin
  FTokenizer := TZExpressionTokenizer.Create;
  FDefaultVariables := TZVariablesList.Create;
  FDefaultFunctions := TZDefaultFunctionsList.Create;
  FVariantManager := TZSoftVariantManager.Create;
  FParser := TZExpressionParser.Create(FTokenizer);
  FAutoVariables := True;
end;

constructor TZExpression.CreateWithExpression(const Expression: string);
begin
  Create;
  SetExpression(Expression);
end;

destructor TZExpression.Destroy;
begin
  FTokenizer := nil;
  FDefaultVariables := nil;
  FDefaultFunctions := nil;
  FVariantManager := nil;
  FParser.Free;

  inherited Destroy;
end;

function TZExpression.GetAutoVariables: Boolean;
begin
  Result := FAutoVariables;
end;

procedure TZExpression.SetAutoVariables(Value: Boolean);
begin
  FAutoVariables := Value;
end;

function TZExpression.GetDefaultFunctions: IZFunctionsList;
begin
  Result := FDefaultFunctions;
end;

procedure TZExpression.SetDefaultFunctions(const Value: IZFunctionsList);
begin
  FDefaultFunctions := Value;
end;

function TZExpression.GetDefaultVariables: IZVariablesList;
begin
  Result := FDefaultVariables;
end;

procedure TZExpression.SetDefaultVariables(const Value: IZVariablesList);
begin
  FDefaultVariables := Value;
end;

function TZExpression.GetExpression: string;
begin
  Result := FParser.Expression;
end;

procedure TZExpression.SetExpression(const Value: string);
begin
  FParser.Expression := Value;
  if FAutoVariables then
    CreateVariables(FDefaultVariables);
end;

function TZExpression.GetVariantManager: IZVariantManager;
begin
  Result := FVariantManager;
end;

Function TZExpression.NormalizeValues(var Val1, Val2: TZVariant): Boolean;
begin
  Result := (Val1.VType in [vtString..vtUnicodeString]) and
        not (Val2.VType in [vtString..vtUnicodeString]) and (Val2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF} <> '');
  if Result then
    Val2 := EncodeString(Val2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
end;

procedure TZExpression.SetVariantManager(const Value: IZVariantManager);
begin
  FVariantManager := Value;
end;

function TZExpression.GetTokenizer: IZTokenizer;
begin
  Result := FTokenizer;
end;

procedure TZExpression.SetTokenizer(const Value: IZTokenizer);
begin
  FTokenizer := Value;
  FParser.Tokenizer := Value;
end;

procedure TZExpression.Clear;
begin
  FParser.Clear;
  FDefaultVariables.Clear;
end;

procedure TZExpression.CreateVariables(const Variables: IZVariablesList);
var I: Integer;
    Name: string;
begin
  for I := 0 to FParser.Variables.Count - 1 do begin
    Name := FParser.Variables[I];
    if Variables.FindByName(Name) < 0 then
      Variables.Add(Name, NullVariant);
  end;
end;

function TZExpression.Evaluate: TZVariant;
begin
  Result := Evaluate3(FDefaultVariables, FDefaultFunctions);
end;

function TZExpression.Evaluate2(const Variables: IZVariablesList): TZVariant;
begin
  Result := Evaluate3(Variables, FDefaultFunctions);
end;

function TZExpression.Evaluate3(const Variables: IZVariablesList;
  const Functions: IZFunctionsList): TZVariant;
var
  Stack: TZExecutionStack;
begin
  Stack := TZExecutionStack.Create;
  try
    Result := Evaluate4(Variables, Functions, Stack);
  finally
    Stack.Free;
  end;
end;

function TZExpression.Evaluate4(const Variables: IZVariablesList;
  const Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;
var
  I, Index, ParamsCount: Integer;
  Current: PZExpressionToken;
  Value1, Value2: TZVariant;
  function CreateSyntaxErrorNear(Current: PZExpressionToken): TZExpressionError;
  begin //suppress unwanted _(U/A)StrClr the format leaves in main method
    Result := TZExpressionError.Create(
                Format(SSyntaxErrorNear, [SoftVarManager.GetAsString(Current.Value)]))
  end;
  function CreateFunctionWasNotFoundError(Current: PZExpressionToken): TZExpressionError;
  begin //suppress unwanted _(U/A)StrClr the format leaves in main method
    Result := TZExpressionError.Create( Format(SFunctionWasNotFound,
      [Current.Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}]))
  end;
  function CreateVariableWasNotFoundError(Current: PZExpressionToken): TZExpressionError;
  begin //suppress unwanted _(U/A)StrClr the format leaves in main method
    Result := TZExpressionError.Create( Format(SVariableWasNotFound,
      [Current.Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}]))
  end;
  function InternalIsMatch({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value1, Value2: TZVariant): Boolean;
  begin //suppress unwanted _(U/A)StrClr the GetAsString leaves in main method
    Result := IsMatch(FVariantManager.GetAsString(Value2),
      FVariantManager.GetAsString(Value1))
  end;
begin
  Stack.Clear;

  for I := 0 to FParser.ResultTokens.Count - 1 do begin
    Current := FParser.ResultTokens[I];
    case Current.TokenType of
      ttConstant: Stack.Push(Current.Value);
      ttVariable: begin
          if Current.Value.VType = vtString then begin
            Index := Variables.FindByName(Current.Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
            if Index < 0 then
              raise CreateVariableWasNotFoundError(Current);
           Current.Value := EncodeInteger(Index);
          end;
          if Current.Value.VType = vtInteger
          then Stack.Push(Variables.Values[Current.Value.VInteger])
          else raise CreateSyntaxErrorNear(Current);
        end;
      ttFunction: begin
          if Current.Value.VType = vtString then begin
            Index := Functions.FindByName(Current.Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
            if Index < 0 then
              raise CreateFunctionWasNotFoundError(Current);
            Current.Value := EncodeInterface(Functions.Functions[Index]);
          end;
          if Current.Value.VType = vtInterface then begin
            Value1 := IZFunction(Current.Value.VInterface).Execute(Stack, FVariantManager);
            ParamsCount := FVariantManager.GetAsInteger(Stack.Pop);
            Stack.DecStackPointer(ParamsCount);
            Stack.Push(Value1);
          end else
            raise CreateSyntaxErrorNear(Current);
        end;
      ttAnd: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpAnd(Value1, Value2));
        end;
      ttOr: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpOr(Value1, Value2));
        end;
      ttXor: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpXor(Value1, Value2));
        end;
      ttNot: Stack.Push(FVariantManager.OpNot(Stack.Pop));
      ttPlus: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpAdd(Value1, Value2));
        end;
      ttMinus: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpSub(Value1, Value2));
        end;
      ttStar: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMul(Value1, Value2));
        end;
      ttSlash: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpDiv(Value1, Value2));
        end;
      ttProcent: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMod(Value1, Value2));
        end;
      ttEqual: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          NormalizeValues(Value1, Value2);
          Stack.Push(FVariantManager.OpEqual(Value1, Value2));
        end;
      ttNotEqual: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          NormalizeValues(Value1, Value2);
          Stack.Push(FVariantManager.OpNotEqual(Value1, Value2));
        end;
      ttMore: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          NormalizeValues(Value1, Value2);
          Stack.Push(FVariantManager.OpMore(Value1, Value2));
        end;
      ttLess: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          NormalizeValues(Value1, Value2);
          Stack.Push(FVariantManager.OpLess(Value1, Value2));
        end;
      ttEqualMore: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMoreEqual(Value1, Value2));
        end;
      ttEqualLess: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpLessEqual(Value1, Value2));
        end;
      ttPower: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpPow(Value1, Value2));
        end;
      ttUnary: Stack.Push(FVariantManager.OpNegative(Stack.Pop));
      ttLike: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(EncodeBoolean(InternalIsMatch(Value1, Value2)));
        end;
      ttNotLike: begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(EncodeBoolean(not InternalIsMatch(Value1, Value2)));
        end;
      ttIsNull: begin
          Value1 := Stack.Pop;
          Stack.Push(EncodeBoolean(FVariantManager.IsNull(Value1)));
        end;
      ttIsNotNull: begin
          Value1 := Stack.Pop;
          Stack.Push(EncodeBoolean(not FVariantManager.IsNull(Value1)));
        end;
      else raise TZExpressionError.Create(SInternalError);
    end;
  end;

  if Stack.Count <> 1 then
    raise TZExpressionError.Create(SInternalError);
  Result := Stack.Pop;
end;

end.
