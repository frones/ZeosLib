{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctions;

interface

{$I ZCore.inc}

uses SysUtils, Classes, ZClasses, ZCompatibility, ZVariant,
  ZExpression;

type
  /// <author>EgonHugeist<author>
  /// <summary>Defines a reference of a TZKeyAndFunctionPair record.</summary>
  PZKeyAndFunctionPair = ^TZKeyAndFunctionPair;
  /// <author>EgonHugeist<author>
  /// <summary>Defines a TZKeyAndFunctionPair record which holds the key and function
  ///  interface</summary>
  TZKeyAndFunctionPair = record
    Key: Cardinal;
    Value: IZFunction;
  end;

  /// <author>EgonHugeist<author>
  /// <summary>Implements a list of TZKeyAndFunctionPairs.</summary>
  TZKeyAndFunctionPairList = Class(TZCustomElementList)
  protected
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    /// <summary>Get the address of an element in the list. It is an error
    ///  remembering the address while the element capacity changes. The address
    ///  might be invalid then.</summary>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    function Get(Index: NativeInt): PZKeyAndFunctionPair;
    /// <summary>represents TZKeyAndFunctionPair reference by index</summary>
    property Items[Index: NativeInt]: PZKeyAndFunctionPair read Get; default;
  End;


  { TZFunctionsList }

  /// <summary>Implements a list of functions.</summary>
  TZFunctionsList = class (TInterfacedObject, IZFunctionsList)
  private
    FFunctions: TZKeyAndFunctionPairList;
    /// <summary>Sets the capacity of the internal Keystorage.</summary>
    /// <param>"NewCapacity" the new capacity to be set.</param>
    procedure SetKeyCapacity(const NewCapacity: Integer);
    /// <summary>Sets a key to the Keystorage.</summary>
    /// <param>"aKey" the new key to be set.</param>
    /// <param>"aPosition" the position of the new key to be placed into the list.</param>
    procedure SetKey(const aKey: Cardinal; const aPosition : Integer);
    /// <summary>Regenerates a given key</summary>
    /// <param>"aPosition" the position of the key to be regenerated.</param>
    procedure RegenerateKey(const aPosition: Integer);
    /// <summary>Regenerates all keys</summary>
    procedure RegenerateKeys;
  protected
    /// <summary>Finds a function reference by its Name and Hashkey</summary>
    /// <param>"aKey" the new key we search for.</param>
    /// <param>"aPosition" the position of the new key to be placed into the list.</param>
    function FindByKeyAndName(const aKey: Cardinal; const aName: string): Integer;
    /// <summary>creates an Duplicate expression name Error. This method
    ///  just bypasses the _(U/A)StrClr generated for the formated String</summary>
    /// <param>"Name" duplicate name.</param>
    /// <returns>The TZExpressionError object.</returns>
    function CreateDuplicateException(const Name: string): TZExpressionError;
  public
    /// <summary>Constructs this object.</summary>
    constructor Create;
    /// <summary>Destroys this object and cleanup the memory.</summary>
    destructor Destroy; override;
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
    /// <returns>The Index of the function or -1 if not found.</returns>
    function FindByName(const Name: string): Integer;
    /// <summary>Cleans the list of registered functions.</summary>
    procedure Clear;
  end;

  /// <summary>Implements an abstract function.</summary>
  TZAbstractFunction = class (TInterfacedObject)
  private
    FName: string;
  protected
    /// <summary>Gets the assigned function name.</summary>
    /// <returns>the assigned function name.</returns>
    function GetName: string;
    /// <summary>Create an epression error. This method suppresses
    ///  unwanded _(U/A)StrClr calls for local variables of the format.</summary>
    /// <param>"Expected" the expected count.</param>
    /// <param>"Actual" the actual count.</param>
    /// <returns>The TZExpressionError to be raised.</returns>
    function CreatExpressionError(Expected, Actual: Integer): TZExpressionError;
    /// <summary>Checks the function parameter count number.</summary>
    /// <param>"Stack" a stack object.</param>
    /// <param>"ExpectedCount" a number of expected parameters.</param>
    /// <returns>a real number of parameters.</returns>
    function CheckParamsCount(Stack: TZExecutionStack;
      ExpectedCount: Integer): Integer;
  public
    /// <summary>Creates the function with a user defined name.</summary>
    constructor Create(const aName : string);
    /// <summary>represents the function name</summary>
    property Name: string read GetName;
  end;

  /// <summary>Implements a default function list.</summary>
  TZDefaultFunctionsList = class (TZFunctionsList)
  public
    /// <summary>Constructs a default functions list and adds all available
    ///  standard functions.<summary>
    constructor Create;
  end;

implementation

uses ZMessages, ZFunctionsMath, ZFunctionsDateTime, ZFunctionsStrings,
     ZFunctionsConvert, ZFunctionsOther;

{ TZFunctionsList }

constructor TZFunctionsList.Create;
begin
  FFunctions := TZKeyAndFunctionPairList.Create(SizeOf(TZKeyAndFunctionPair), True);
end;

function TZFunctionsList.CreateDuplicateException(
  const Name: string): TZExpressionError;
begin
  Result := TZExpressionError.Create('Function '+Name+' already defined!');
end;

destructor TZFunctionsList.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited Destroy;
end;

procedure TZFunctionsList.SetKeyCapacity(const NewCapacity : Integer);
begin
  FFunctions.Capacity := NewCapacity;
end;

procedure TZFunctionsList.SetKey(const aKey: Cardinal; const aPosition: Integer);
begin
  if aPosition >= FFunctions.Capacity then
    FFunctions.Capacity := FFunctions.Capacity + 16;
  FFunctions[aPosition].Key := aKey;
end;

procedure TZFunctionsList.RegenerateKey(const aPosition : Integer);
begin
  SetKey(Hash(FFunctions[aPosition].Value.Name), aPosition);
end;

procedure TZFunctionsList.RegenerateKeys;
var I: Integer;
begin
  SetKeyCapacity(0);
  for I := 0 to FFunctions.Count - 1 do
    RegenerateKey(i);
end;

function TZFunctionsList.FindByKeyAndName(const aKey : Cardinal; const aName: string): Integer;
var
  I: NativeInt;
  KeyAndFunctionPair: PZKeyAndFunctionPair;
begin
  Result := -1;
  for I := 0 to FFunctions.Count - 1 do begin
    KeyAndFunctionPair := FFunctions.Get(I);
    if aKey = KeyAndFunctionPair.Key then
      if aName = KeyAndFunctionPair.Value.Name then begin
        Result := I;
        Break;
      end;
  end;
end;

function TZFunctionsList.FindByName(const Name: string): Integer;
var aName: string;
begin
  aName := Uppercase(Name);
  Result := FindByKeyAndName(Hash(aName), aName);
end;

procedure TZFunctionsList.Add(const Func: IZFunction);
var Index: NativeInt;
    aKey : Cardinal;
    aName: string;
    KeyAndFunctionPair: PZKeyAndFunctionPair;
begin
  aName := Uppercase(Func.Name);
  aKey  := Hash(aName);
  Index := FindByKeyAndName(aKey, aName);
  if Index < 0 then begin
    KeyAndFunctionPair := FFunctions.Add(Index);
    KeyAndFunctionPair.Key := aKey;
    KeyAndFunctionPair.Value := Func;
  end else
    raise CreateDuplicateException(Func.Name);
end;

procedure TZFunctionsList.Remove(const Name: string);
var Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then begin
    FFunctions.Delete(Index);
    RegenerateKeys;
  end;
end;

procedure TZFunctionsList.Clear;
begin
  FFunctions.Clear;
  SetKeyCapacity(0);
end;

function TZFunctionsList.GetCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TZFunctionsList.GetFunction(Index: Integer): IZFunction;
begin
  Result := FFunctions[Index].Value;
end;

function TZFunctionsList.GetName(Index: Integer): string;
begin
  Result := FFunctions[Index].Value.Name;
end;

{ TZDefaultFunctionsList }

constructor TZDefaultFunctionsList.Create;
begin
  inherited Create;
  AddMathFunctions(Self);
  AddStringFunctions(Self);
  AddConvertFunctions(Self);
  AddOtherFunctions(Self);
  AddDateTimeFunctions(Self);
end;

{ TZAbstractFunction }

constructor TZAbstractFunction.Create(const aName : string);
begin
  inherited Create;
  FName := UpperCase(aName);
end;

function TZAbstractFunction.CreatExpressionError(Expected,
  Actual: Integer): TZExpressionError;
begin
  Result := TZExpressionError.Create(Format(SParametersError,
      [Expected, Actual]));
end;

function TZAbstractFunction.GetName: string;
begin
  Result := FName;
end;

function TZAbstractFunction.CheckParamsCount(Stack: TZExecutionStack;
  ExpectedCount: Integer): Integer;
begin
  Result := SoftVarManager.GetAsInteger(Stack.GetParameter(0));
  if Result <> ExpectedCount then
    raise CreatExpressionError(ExpectedCount, Result);
end;

{ TZKeyAndFunctionPairList }

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function TZKeyAndFunctionPairList.Get(Index: NativeInt): PZKeyAndFunctionPair;
begin
  {$IFNDEF DISABLE_CHECKING}
  if NativeUInt(Index) > Capacity then
    Error(@SListIndexError, Index);
  {$ENDIF DISABLE_CHECKING}
  Result := Pointer(NativeUInt(FElements)+(NativeUInt(Index)*ElementSize));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZKeyAndFunctionPairList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if Action = lnDeleted then
    PZKeyAndFunctionPair(Ptr).Value := nil
end;

end.

