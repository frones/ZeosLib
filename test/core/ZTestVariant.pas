{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Test Case for Variants                   }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestVariant;

interface

{$I ZCore.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  {$If defined(MSWINDOWS) and not defined(FPC)}
  Windows,
  {$IFEND}
  ZTestCase, SysUtils, Classes, ZVariant, ZCompatibility;

type

  {** Implements a test case for Utilities. }
  TZTestVariantCase = class(TZGenericTestCase)
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
    procedure TestArray;
  end;

  TZDefVarManagerConvertCase = class(TZGenericTestCase)
  private
    FManager: IZVariantManager;
    FConSettings: PZConSettings;
    function GetTestStringVar: TZVariant;
  protected
    FNotEqualClientCP: Word;
    property Manager: IZVariantManager read FManager write FManager;
    property ConSettings: PZConSettings read FConSettings write FConSettings;

    procedure SetUp; override;
    procedure TearDown; override;

    procedure Test_AnsiStringFromString;
    procedure Test_AnsiStringFromUTF8String;
    procedure Test_AnsiStringFromRawByteString;
    procedure Test_AnsiStringFromUnicodeString;

    procedure Test_UTF8StringFromString;
    procedure Test_UTF8StringFromAnsiString;
    procedure Test_UTF8StringFromRawByteString;
    procedure Test_UTF8StringFromUnicodeString;

    procedure Test_RawByteStringFromString;
    procedure Test_RawByteStringFromUnicodeString;
    procedure Test_StringFromUnicodeString;
  published
    procedure TestConvert; virtual;
  end;

  TZClientVarManagerConvertCase = class(TZDefVarManagerConvertCase)
  protected
    procedure SetUp; override;
    procedure SetupConSettings; virtual;
  published
    procedure TestConvert; override;
  end;

  TZClientVarManagerConvertCaseUTF8 = Class(TZClientVarManagerConvertCase)
  protected
    procedure SetupConSettings; override;
  End;

  TZClientVarManagerConvertCaseWin1252 = Class(TZClientVarManagerConvertCase)
  protected
    procedure SetupConSettings; override;
  End;

const
  UnicodeVar: TZVariant = (VType: vtUnicodeString; VUnicodeString: '¸‰ˆﬂ‚·‡');
  {$IFDEF UNICODE}
  UStringVar: TZVariant = (VType: vtString; VString: '¸‰ˆﬂ‚·‡');
  {$ELSE}
  String_CPUTF8_Var: TZVariant = (VType: vtString; VString: '√º√§√∂√ü√¢√°√†');
  String_CP1252_Var: TZVariant = (VType: vtString; VString: '¸‰ˆﬂ‚·‡');
  {$ENDIF}
  AnsiVar: TZVariant = (VType: vtAnsiString; VAnsiString: '¸‰ˆﬂ‚·‡');
  UTF8Var: TZVariant = (VType: vtUTF8String; VUTF8String: {$IFDEF WITH_RAWBYTESTRING}'¸‰ˆﬂ‚·‡'{$ELSE}'√º√§√∂√ü√¢√°√†'{$ENDIF});
  Raw_CPUTF8_Var: TZVariant = (VType: vtRawByteString; VRawByteString: RawByteString('√º√§√∂√ü√¢√°√†'));
  Raw_CP1252_Var: TZVariant = (VType: vtRawByteString; VRawByteString: RawByteString('¸‰ˆﬂ‚·‡'));

implementation

uses ZEncoding;

var
  TestVar1, TestVar2: TZVariant;
  TestConSettings: PZConSettings;

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

{ TZClientVarManagerConvertCase }

function TZDefVarManagerConvertCase.GetTestStringVar: TZVariant;
begin
  {$IFDEF UNICODE}
  Result := UStringVar;
  {$ELSE}
  if ConSettings^.AutoEncode then
    if ConSettings^.CTRL_CP = zCP_UTF8 then
      Result := String_CP1252_Var
    else
      Result := String_CPUTF8_Var
  else
    if ConSettings^.CTRL_CP = zCP_UTF8 then
      Result := String_CPUTF8_Var
    else
      Result := String_CP1252_Var;
  {$ENDIF}
end;

procedure TZDefVarManagerConvertCase.SetUp;
begin
  Manager := DefVarManager;
  ConSettings := TestConSettings;
  ConSettings^.CTRL_CP := ZDefaultSystemCodePage;
  ConSettings^.AutoEncode := False;
end;

procedure TZDefVarManagerConvertCase.TearDown;
begin
  Manager := nil;
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromString;
var
  StringVar: TZVariant;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiString(StringVar.VString)),
    PAnsiChar(TestVar1.VAnsiString), 'AnsiString from String');
  Manager.SetAsAnsiString(TestVar2, TestVar1.VAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiString(StringVar.VString)),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  CheckEquals(PAnsiChar(TestVar1.VAnsiString),
    PAnsiChar(TestVar2.VAnsiString), 'SetAsAnsiString');
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(StringVar.VString, TestVar1.VString, 'String');
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromUTF8String;
begin
  TestVar1 := Manager.Convert(UTF8Var, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertUTF8ToAnsi(UTF8Var.VUTF8String)),
    PAnsiChar(TestVar1.VAnsiString), 'AnsiString from UTF8String');
  Manager.SetAsAnsiString(TestVar2, TestVar1.VAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertUTF8ToAnsi(UTF8Var.VUTF8String)),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  CheckEquals(PAnsiChar(TestVar1.VAnsiString),
    PAnsiChar(TestVar2.VAnsiString), 'SetAsAnsiString');
  TestVar1 := Manager.Convert(TestVar2, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(UTF8Var.VUTF8String), PAnsichar(TestVar1.VUTF8String), 'UTF8String');
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromRawByteString;
var
  AManager: IZClientVariantManager;
  RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiVar.VAnsiString),
    PAnsiChar(TestVar1.VAnsiString), 'AnsiString from RawByteString');
  Manager.SetAsAnsiString(TestVar2, TestVar1.VAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiVar.VAnsiString),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  CheckEquals(PAnsiChar(TestVar1.VAnsiString),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(RawVar.VRawByteString, TestVar1.VRawByteString, 'RawByteStringString from AnsiString');
  if Supports(Manager, IZClientVariantManager, AManager) then
    CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, FNotEqualClientCP)),
      PAnsiChar(AManager.GetAsRawByteString(TestVar1, FNotEqualClientCP)),
        'GetAsRawByteString(TZVariant, CP: '+IntToStr(FNotEqualClientCP)+')');
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiString(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar1.VAnsiString), 'AnsiString');
  Manager.SetAsAnsiString(TestVar2, TestVar1.VAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(AnsiString(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  CheckEquals(PAnsiChar(TestVar1.VAnsiString),
    PAnsiChar(TestVar2.VAnsiString), 'AnsiString');
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from AnsiString');
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromString;
var
  StringVar: TZVariant;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertStringToUTF8(StringVar.VString, ConSettings^.CTRL_CP)),
    PAnsiChar(TestVar1.VUTF8String), 'UTF8String from String');
  Manager.SetAsUTF8String(TestVar2, TestVar1.VUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertStringToUTF8(StringVar.VString, ConSettings^.CTRL_CP)),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  CheckEquals(PAnsiChar(TestVar1.VUTF8String),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type');
  if Supports(Manager, IZClientVariantManager) then //only the ClientVarManager can convert other StringCP's than Get_ACP
    CheckEquals(StringVar.VString, TestVar1.VString, 'String from UTF8String');
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromAnsiString;
begin
  TestVar1 := Manager.Convert(AnsiVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertAnsiToUTF8(AnsiVar.VAnsiString)),
    PAnsiChar(TestVar1.VUTF8String), 'UTF8String from AnsiString');
  Manager.SetAsUTF8String(TestVar2, TestVar1.VUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertAnsiToUTF8(AnsiVar.VAnsiString)),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  CheckEquals(PAnsiChar(TestVar1.VUTF8String),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  TestVar1 := Manager.Convert(TestVar2, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(AnsiVar.VAnsiString, TestVar1.VAnsiString, 'AnsiString from UTF8String');
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromRawByteString;
var
  AManager: IZClientVariantManager;
  RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(UTF8Var.VUTF8String), PAnsiChar(TestVar1.VAnsiString), 'UTF8String from RawByteString');
  Manager.SetAsUTF8String(TestVar2, TestVar1.VUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(UTF8Var.VAnsiString),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  CheckEquals(PAnsiChar(TestVar1.VUTF8String),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(RawVar.VRawByteString, TestVar1.VRawByteString, 'RawByteStringString from UTf8String');
  if Supports(Manager, IZClientVariantManager, AManager) then
    CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, FNotEqualClientCP)),
      PAnsiChar(AManager.GetAsRawByteString(TestVar1, FNotEqualClientCP)),
        'GetAsRawByteString(TZVariant, CP: '+IntToStr(FNotEqualClientCP)+')');
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type');
  {$IFDEF WITH_RAWBYTESTRING}
  CheckEquals(PAnsiChar(UTF8String(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar1.VUTF8String), 'UTF8String from UnicodeString');
  {$ELSE}
  CheckEquals(PAnsiChar(UTF8Encode(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar1.VUTF8String), 'UTF8String from UnicodeString');
  {$ENDIF}
  Manager.SetAsUTF8String(TestVar2, TestVar1.VUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type');
  {$IFDEF WITH_RAWBYTESTRING}
  CheckEquals(PAnsiChar(UTF8String(UnicodeVar.VUnicodeString)), PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  {$ELSE}
  CheckEquals(PAnsiChar(UTF8Encode(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  {$ENDIF}
  CheckEquals(PAnsiChar(TestVar1.VUTF8String), PAnsiChar(TestVar2.VUTF8String), 'UTF8String');
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from UTF8String');
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromString;
var
  StringVar: TZVariant;
  AManager: IZClientVariantManager;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertStringToRaw(StringVar.VString, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString from String');
  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZConvertStringToRaw(StringVar.VString, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString');
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString');
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type');
  if Supports(Manager, IZClientVariantManager, AManager) then //only the ClientVarManager can convert other StringCP's than Get_ACP
  begin
    CheckEquals(StringVar.VString, TestVar1.VString, 'String from RawByteString');
    CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, FNotEqualClientCP)),
      PAnsiChar(AManager.GetAsRawByteString(TestVar1, FNotEqualClientCP)),
        'GetAsRawByteString(TZVariant, CP: '+IntToStr(FNotEqualClientCP)+')');
  end;
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromUnicodeString;
var
  AManager: IZClientVariantManager;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString CP:'+IntToStr(ConSettings^.ClientCodePage^.CP));
  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString');

  if Supports(Manager, IZClientVariantManager, AManager) then
    CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, FNotEqualClientCP)),
      PAnsiChar(AManager.GetAsRawByteString(TestVar1, FNotEqualClientCP)),
        'GetAsRawByteString(TZVariant, CP: '+IntToStr(FNotEqualClientCP)+')');
end;

procedure TZDefVarManagerConvertCase.Test_StringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(ZConvertUnicodeToString(UnicodeVar.VUnicodeString, ConSettings^.CTRL_CP), TestVar1.VString, 'String');
  Manager.SetAsString(TestVar2, TestVar1.VString);
  CheckEquals(Ord(vtString), Ord(TestVar2.VType), 'ZVariant-Type');
  CheckEquals(ZConvertUnicodeToString(UnicodeVar.VUnicodeString, ConSettings^.CTRL_CP), TestVar2.VString, 'String');
  CheckEquals(TestVar1.VString, TestVar2.VString, 'String');
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type');
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString');
end;

procedure TZDefVarManagerConvertCase.TestConvert;
begin
  Test_AnsiStringFromString;
  Test_AnsiStringFromUTF8String;
  try
    Test_AnsiStringFromRawByteString;
    Check(False, 'Wrong RawByteString behavior');
  except
    Check(True);
  end;
  Test_AnsiStringFromUnicodeString;
  Test_UTF8StringFromString;
  Test_UTF8StringFromAnsiString;
  Test_UTF8StringFromUnicodeString;
  Test_StringFromUnicodeString;
  try
    Test_RawByteStringFromString;
    Check(False, 'Wrong RawByteString behavior');
  except
    Check(True);
  end;
  try
    Test_RawByteStringFromUnicodeString;
    Check(False, 'Wrong RawByteString behavior');
  except
    Check(True);
  end;
end;

{ TZClientVarManagerConvertCase }

procedure TZClientVarManagerConvertCase.SetUp;
begin
  ConSettings := TestConSettings;
  SetupConSettings;
  SetConvertFunctions(TestConSettings);
  Manager := TZClientVariantManager.Create(ConSettings);
end;

procedure TZClientVarManagerConvertCase.SetupConSettings;
begin
  ConSettings^.AutoEncode := False;
  ConSettings^.CTRL_CP := ZDefaultSystemCodePage;
  ConSettings^.ClientCodePage^.CP := ZDefaultSystemCodePage;
  ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := True;
  if ConSettings^.CTRL_CP = zCP_UTF8 then
    FNotEqualClientCP := 1252
  else
    FNotEqualClientCP := zCP_UTF8;
end;

procedure TZClientVarManagerConvertCase.TestConvert;
begin
  Test_AnsiStringFromString;
  Test_AnsiStringFromUTF8String;
  Test_AnsiStringFromRawByteString;
  Test_AnsiStringFromUnicodeString;
  Test_UTF8StringFromString;
  Test_UTF8StringFromAnsiString;
  Test_UTF8StringFromUnicodeString;
  Test_StringFromUnicodeString;
  Test_RawByteStringFromString;
  Test_RawByteStringFromUnicodeString;
end;

{ TZClientVarManagerConvertCaseUTF8 }

procedure TZClientVarManagerConvertCaseUTF8.SetupConSettings;
begin
  inherited;
  ConSettings^.CTRL_CP := zCP_UTF8;
  ConSettings^.ClientCodePage^.CP := zCP_UTF8;
  FNotEqualClientCP := zCP_WIN1252;
end;

{ TZClientVarManagerConvertCaseWin1252 }

procedure TZClientVarManagerConvertCaseWin1252.SetupConSettings;
begin
  inherited;
  ConSettings^.CTRL_CP := zCP_WIN1252;
  ConSettings^.ClientCodePage^.CP := zCP_WIN1252;
  FNotEqualClientCP := zCP_UTF8;
end;

initialization
  TestConSettings := New(PZConSettings);
  TestConSettings^.ClientCodePage := New(PZCodePage);

  RegisterTest('core',TZTestVariantCase.Suite);
  RegisterTest('core',TZDefVarManagerConvertCase.Suite);
  RegisterTest('core',TZClientVarManagerConvertCaseUTF8.Suite);
  RegisterTest('core',TZClientVarManagerConvertCaseWin1252.Suite);

finalization
  Dispose(TestConSettings^.ClientCodePage);
  Dispose(TestConSettings);
end.
