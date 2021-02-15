{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{            Originally written by Sergey Seroukhov       }
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

unit ZVariables;

interface

{$I ZCore.inc}

uses SysUtils, Classes, ZClasses,
  ZCompatibility, ZVariant, ZExpression;

type
  /// <author>EgonHugeist<author>
  /// <summary>Defines a TZNameVariablePair reference</summary>
  PZNameAndVariablePair = ^TZNameAndVariablePair;
  /// <author>EgonHugeist<author>
  /// <summary>Defines a pair of a name and a variable value.</summary>
  TZNameAndVariablePair = record
    Name: string;
    Value: TZVariant;
  end;

  /// <author>EgonHugeist<author>
  /// <summary>Implements a list of TZNameVariablePairs.</summary>
  TZNameAndVariablePairList = class(TZCustomElementList)
  protected
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  /// <summary>Implements a variables list.</summary>
  TZVariablesList = class (TInterfacedObject, IZVariablesList)
  private
    FVariables: TZNameAndVariablePairList;
  public
    /// <summary>Creates this variable list object.</summary>
    constructor Create;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;

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
  end;

implementation

uses ZMessages;

{ TZVariablesList }

constructor TZVariablesList.Create;
begin
  FVariables := TZNameAndVariablePairList.Create(SizeOf(TZNameAndVariablePair), True);
end;

destructor TZVariablesList.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

function TZVariablesList.FindByName(const Name: string): Integer;
var
  I: Integer;
  Current: PZNameAndVariablePair;
  UpperName: string;
begin
  Result := -1;
  UpperName := UpperCase(Name);
  for I := 0 to FVariables.Count - 1 do begin
    Current := PZNameAndVariablePair(FVariables[I]);
    if Current.Name = UpperName then begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TZVariablesList.Add(const Name: string; const Value: TZVariant);
var Idx: NativeInt;
    NameAndVariablePair: PZNameAndVariablePair;
begin
  if FindByName(Name) >= 0 then
    raise Exception.Create(Format(SVariableAlreadyExists, [Name]));
  NameAndVariablePair := FVariables.Add(Idx);
  NameAndVariablePair.Name := UpperCase(Name);
  NameAndVariablePair.Value := Value;
end;

procedure TZVariablesList.Remove(const Name: string);
var Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    FVariables.Delete(Index);
end;

procedure TZVariablesList.Clear;
begin
  FVariables.Clear;
end;

procedure TZVariablesList.ClearValues;
var I: Integer;
    NameAndVariablePair: PZNameAndVariablePair;
begin
  for I := 0 to FVariables.Count - 1 do begin
    NameAndVariablePair := PZNameAndVariablePair(FVariables[I]);
    NameAndVariablePair.Value.VUnicodeString := '';
    NameAndVariablePair.Value.VRawByteString := {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}nil{$ELSE}''{$ENDIF};
    NameAndVariablePair.Value.VInterface := nil;
    NameAndVariablePair.Value.VType := vtNull;
  end;
end;

function TZVariablesList.GetCount: Integer;
begin
  Result := FVariables.Count;
end;

function TZVariablesList.GetName(Index: Integer): string;
begin
  Result := PZNameAndVariablePair(FVariables[Index]).Name;
end;

function TZVariablesList.GetValue(Index: Integer): TZVariant;
begin
  Result := PZNameAndVariablePair(FVariables[Index]).Value;
end;

function TZVariablesList.GetValueByName(const Name: string): TZVariant;
var Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0
  then Result := PZNameAndVariablePair(FVariables[Index]).Value
  else Result := NullVariant;
end;

procedure TZVariablesList.SetValue(Index: Integer; const Value: TZVariant);
begin
  PZNameAndVariablePair(FVariables[Index]).Value := Value;
end;

procedure TZVariablesList.SetValueByName(const Name: string; const Value: TZVariant);
var Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0
  then PZNameAndVariablePair(FVariables[Index]).Value := Value
  else Add(Name, Value);
end;

{ TZNameAndVariablePairList }

procedure TZNameAndVariablePairList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if Action = lnDeleted then begin
    PZNameAndVariablePair(Ptr).Name := '';
    PZNameAndVariablePair(Ptr).Value.VUnicodeString := '';
    PZNameAndVariablePair(Ptr).Value.VRawByteString := {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}nil{$ELSE}''{$ENDIF};
    PZNameAndVariablePair(Ptr).Value.VInterface := nil;
  end;
end;

end.

