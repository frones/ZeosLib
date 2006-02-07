{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Test Case for Variants                   }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{              Written by Sergey Seroukhov                }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZTestVariant;

interface

{$I ZCore.inc}

uses TestFramework, ZTestDefinitions, SysUtils, Classes, ZVariant;

type

  {** Implements a test case for Utilities. }
  TZTestVariantCase = class(TZCoreGenericTestCase)
  private
    FManager: IZVariantManager;
  protected
    property Manager: IZVariantManager read FManager write FManager;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNull;
    procedure TestBoolean;
    procedure TestInteger;
    procedure TestDouble;
    procedure TestString;
{$IFNDEF VER130BELOW}
    procedure TestArray;
{$ENDIF}
  end;

implementation

{ TZTestVariantCase }

{**
  Sets up the test environment before tests.
}
procedure TZTestVariantCase.SetUp;
begin
  Manager := DefVarManager;
end;

{**
  Cleans up the test environment after tests.
}
procedure TZTestVariantCase.TearDown;
begin
  Manager := nil;
end;

{**
  Runs a test for null variants.
}
procedure TZTestVariantCase.TestNull;
var
  Value: TZVariant;
begin
  Value := NullVariant;

  Check(Manager.IsNull(Value));
  CheckEquals(Ord(vtNull), Ord(Value.VType));

  CheckEquals(False, Manager.GetAsBoolean(Value));
  CheckEquals('', Manager.GetAsString(Value));
  CheckEquals(0, Manager.GetAsInteger(Value));
  CheckEquals(0.0, Manager.GetAsFloat(Value), 0.001);
end;

{**
  Runs a test for boolean variants.
}
procedure TZTestVariantCase.TestBoolean;
var
  Value: TZVariant;
begin
  Manager.SetAsBoolean(Value, True);
  CheckEquals(True, Value.VBoolean);
  CheckEquals(Ord(vtBoolean), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  CheckEquals(True, Manager.GetAsBoolean(Value));
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInt operation behaviour.');
  except
  end;
  try
    Manager.GetAsFloat(Value);
    Fail('Incorrect getDouble operation behaviour.');
  except
  end;
end;

{**
  Runs a test for integer variants.
}
procedure TZTestVariantCase.TestInteger;
var
  Value: TZVariant;
begin
  Manager.SetAsInteger(Value, 123);

  CheckEquals(123, Value.VInteger);
  CheckEquals(Ord(vtInteger), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getBoolean operation behaviour.');
  except
  end;
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  CheckEquals(123, Manager.GetAsInteger(Value));
  CheckEquals(123, Manager.GetAsFloat(Value), 0.1);
end;

{**
  Runs a test for string variants.
}
procedure TZTestVariantCase.TestString;
var
  Value: TZVariant;
begin
  Manager.SetAsString(Value, 'ABC');
  CheckEquals('ABC', Value.VString);
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Manager.SetAsString(Value, '123');
  CheckEquals('123', Manager.GetAsString(Value));
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInt operation behaviour.');
  except
  end;
  try
    Manager.GetAsFloat(Value);
    Fail('Incorrect getDouble operation behaviour.');
  except
  end;
end;

{**
  Runs a test for double variants.
}
procedure TZTestVariantCase.TestDouble;
var
  Value: TZVariant;
begin
  Manager.SetAsFloat(Value, 123.456);

  CheckEquals(123.456, Value.VFloat, 0.001);
  CheckEquals(Ord(vtFloat), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getBoolean operation behaviour.');
  except
  end;
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInteger operation behaviour.');
  except
  end;
  CheckEquals(123.456, Manager.GetAsFloat(Value), 0.001);
end;

{**
  Runs a test for variant arrays.
}
{$IFNDEF VER130BELOW}
procedure TZTestVariantCase.TestArray;
const
  MAX_ITEM_COUNT = 10;
var
  I: Integer;
  MaxInt64: Int64;
  ArrayValue: TZVariantDynArray;
  ArrayItem: TZVariant;
  Value: Variant;
begin
  MaxInt64 := High(Int64);
  SetLength(ArrayValue, MAX_ITEM_COUNT);
  for I := 0 to MAX_ITEM_COUNT - 1 do
  begin
    if I < MAX_ITEM_COUNT div 2 then
      Manager.SetAsInteger(ArrayItem, MaxInt64 - I)
    else
      Manager.SetAsInteger(ArrayItem, I);
    ArrayValue[I] := ArrayItem;
  end;
  Value := EncodeVariantArray(ArrayValue);
  for I := 0 to MAX_ITEM_COUNT - 1 do
  begin
    if I < MAX_ITEM_COUNT div 2 then
      Check((MaxInt64 - I) = Value[I], 'Incorrect Int64 array processing')
    else
      Check(I = Value[I], 'Incorrect Int64 array processing');
  end;
end;
{$ENDIF}

initialization
  TestFramework.RegisterTest(TZTestVariantCase.Suite);
end.
