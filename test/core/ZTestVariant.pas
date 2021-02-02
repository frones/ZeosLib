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
  ZTestCase, SysUtils, Classes, ZVariant, ZCompatibility, ZDbcUtils, ZDbcIntfs;

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
    FUniTempExp: UnicodeString;
    FRawTempExp: RawByteString;
    //
    FTestStringList: TStringList;
    function GetTestStringVar: TZVariant;
    function GetExpectedStringVar: TZVariant;
    function GetOptionString: String;
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
    procedure Test_RawByteStringFromAnsiString;
    procedure Test_RawByteStringFromUTF8String;
    procedure Test_RawByteStringFromUnicodeString;

    procedure Test_StringFromAnsiString;
    procedure Test_StringFromUTF8String;
    procedure Test_StringFromRawByteString;
    procedure Test_StringFromUnicodeString;

    procedure Test_UnicodeStringFromString;
    procedure Test_UnicodeStringFromAnsiString;
    procedure Test_UnicodeStringFromUTF8String;
    procedure Test_UnicodeStringFromRawByteString;

    procedure DoTestConvert; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  published
    procedure TestConvert;
  end;

  TZClientVarManagerConvertCase = class(TZDefVarManagerConvertCase)
  protected
    procedure SetUp; override;
    procedure SetupConSettings; virtual;

    procedure DoTestConvert; override;
  published
  end;

  TZClientVarManagerConvertCaseUTF8 = Class(TZClientVarManagerConvertCase)
  protected
    procedure SetupConSettings; override;
  End;

  TZClientVarManagerConvertCase_ControlsCP_UTF8_ClientCP_WIN1252 = Class(TZClientVarManagerConvertCaseUTF8)
  protected
    procedure SetupConSettings; override;
  End;

  TZClientVarManagerConvertCaseWin1252 = Class(TZClientVarManagerConvertCase)
  protected
    procedure SetupConSettings; override;
  public
    procedure AfterConstruction; override;
  End;

  TZClientVarManagerConvertCase_ControlsCP_WIN1252_ClientCP_UTF8 = Class(TZClientVarManagerConvertCaseWin1252)
  protected
    procedure SetupConSettings; override;
  End;

  TZClientVarManagerConvertCaseWin1250 = class(TZClientVarManagerConvertCase)
  protected
    procedure SetupConSettings; override;
  public
    procedure AfterConstruction; override;
  end;

  TZClientVarManagerConvertCase_ControlsCP_WIN1250_ClientCP_UTF8 = class(TZClientVarManagerConvertCaseWin1250)
  protected
    procedure SetupConSettings; override;
  end;

var
  UnicodeVar,
  {$IFDEF UNICODE}
  UStringVar,
  {$ELSE}
  String_CPUTF8_Var,
  String_CP1250_Var,
  String_CP1252_Var,
  {$ENDIF}
  AnsiVar,
  UTF8Var,
  Raw_CPUTF8_Var,
  Raw_CP1250_Var,
  Raw_CP1252_Var: TZVariant;

implementation

uses ZEncoding, ZDbcConnection, TypInfo;

var
  TestVar1, TestVar2: TZVariant;
  TestConSettings: PZConSettings;
  S: UnicodeString;

{ TZTestVariantCase }

{**
  Sets up the test environment before tests.
}
procedure TZTestVariantCase.SetUp;
begin
  Manager := {$IFDEF ZEOS_TEST_ONLY}DefVarManager{$ELSE}SoftVarManager{$ENDIF};
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
  CheckEquals(0.0, Manager.GetAsDouble(Value), 0.001);
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

  Check(not Manager.IsNull(Value));
  CheckEquals(True, Manager.GetAsBoolean(Value));
  CheckEquals(StrTrueUp, Manager.GetAsString(Value));
  CheckEquals(1, Manager.GetAsInteger(Value));
  CheckEquals(1, Manager.GetAsDouble(Value));
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

  Check(not Manager.IsNull(Value));
  CheckEquals(True, Manager.GetAsBoolean(Value));
  CheckEquals('123', Manager.GetAsString(Value));
  CheckEquals(123, Manager.GetAsInteger(Value));
  CheckEquals(123, Manager.GetAsDouble(Value), 0.1);
end;

{**
  Runs a test for string variants.
}
procedure TZTestVariantCase.TestString;
var
  Value: TZVariant;
begin
  Manager.SetAsString(Value, 'ABC');
  CheckEquals('ABC', Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Manager.SetAsString(Value, '123');
  CheckEquals('123', Manager.GetAsString(Value));
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Check(not Manager.IsNull(Value));
  CheckEquals(True, Manager.GetAsBoolean(Value));
  CheckEquals(123, Manager.GetAsInteger(Value));
  CheckEquals(123, Manager.GetAsDouble(Value), 0.1);
end;

{**
  Runs a test for double variants.
}
procedure TZTestVariantCase.TestDouble;
var
  Value: TZVariant;
begin
  Manager.SetAsDouble(Value, 123.456);
  CheckEquals(123.456, Value.VDouble, 0.001);
  CheckEquals(Ord(vtDouble), Ord(Value.VType));

  Check(not Manager.IsNull(Value));
  CheckEquals(True, Manager.GetAsBoolean(Value));
  CheckEquals('123.456', Manager.GetAsString(Value));
  CheckEquals(123, Manager.GetAsInteger(Value));
  CheckEquals(123.456, Manager.GetAsDouble(Value), 0.001);
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

procedure TZDefVarManagerConvertCase.AfterConstruction;
begin
  inherited;
  FTestStringList:= TStringList.Create;
  FTestStringList.Add(''); // empty
  FTestStringList.Add('pure ascii string');
  FTestStringList.Add('CaSeD ASCii String with symbols#@!');
  FTestStringList.Add('ascii start, then ' + Char($0E1)+Char($0C9)+Char($0ED)+Char($0D3)+Char($0FA));
  FTestStringList.Add(Char($0E1)+Char($0C9)+Char($0ED)+Char($0D3)+Char($0FA) + ' followed by ascii');
  FTestStringList.Add(Char($0E1)+Char($0C9)+Char($0ED)+Char($0D3)+Char($0FA) + ' %symbolic core followed by ' + Char($0E1)+Char($0C9)+Char($0ED)+Char($0D3)+Char($0FA));

  FTestStringList.Add(Chr($FC)+Chr($E4)+Chr($F6)+Chr($DF)+Chr($E2)+Chr($E1));
  FTestStringList.Add('testing ascii ' + Chr($FC)+Chr($E4)+Chr($F6)+Chr($DF)+Chr($E2)+Chr($E1) + ' enriched!');
end;

destructor TZDefVarManagerConvertCase.Destroy;
begin
  FreeAndNil(FTestStringList);
  inherited;
end;


function TZDefVarManagerConvertCase.GetTestStringVar: TZVariant;
begin
  {$IFDEF UNICODE}
  Result := UStringVar;
  {$ELSE}
  if ConSettings.W2A2WEncodingSource = encUTF8
  then Result := String_CPUTF8_Var
  else if ConSettings.W2A2WEncodingSource = encDB_CP then begin
    if (ConSettings.ClientCodePage.Encoding = ceUTF16) or (ConSettings.ClientCodePage.CP = zCP_UTF8)
    then Result := String_CPUTF8_Var
    else if ConSettings.ClientCodePage.CP = zCP_WIN1250
    then Result:= String_CP1250_Var
    else Result:= String_CP1252_Var;
  end else begin
    if ZOSCodePage = zCP_WIN1250
    then Result := String_CP1250_Var
    else Result := String_CP1252_Var;
  end;
  {$ENDIF}
end;

function TZDefVarManagerConvertCase.GetExpectedStringVar: TZVariant;
begin
  {$IFDEF UNICODE}
  Result := UStringVar;
  {$ELSE}
  if ConSettings.W2A2WEncodingSource = encUTF8
  then Result := String_CPUTF8_Var
  else if ConSettings.W2A2WEncodingSource = encDB_CP then begin
    if (ConSettings.ClientCodePage.Encoding = ceUTF16) or (ConSettings.ClientCodePage.CP = zCP_UTF8)
    then Result := String_CPUTF8_Var
    else if ConSettings.ClientCodePage.CP = zCP_WIN1250
    then Result:= String_CP1250_Var
    else Result:= String_CP1252_Var;
  end else begin
    if ZOSCodePage = zCP_WIN1250
    then Result := String_CP1250_Var
    else Result := String_CP1252_Var;
  end;
  {$ENDIF}
end;

function TZDefVarManagerConvertCase.GetOptionString: String;
begin
  Result := ' W2A2W_CPtype: '+ GetEnumName(TypeInfo(TZW2A2WEncodingSource), Ord(ConSettings^.W2A2WEncodingSource))+
    ', Client_RawCP: '+IntToStr(ConSettings^.ClientCodepage^.CP)+
    ', ClientEncoding: '+GetEnumName(TypeInfo(TZCharEncoding), Ord(ConSettings^.ClientCodePage.Encoding));
end;

procedure TZDefVarManagerConvertCase.SetUp;
begin
  Manager := {$IFDEF ZEOS_TEST_ONLY}DefVarManager{$ELSE}SoftVarManager{$ENDIF};

  TestConSettings^.W2A2WEncodingSource := encDefaultSystemCodePage;
  FillChar(TestConSettings^.ClientCodePage^, SizeOf(TZCodePage), #0);
  ConSettings := TestConSettings;
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
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(AnsiVar.VRawByteString),
    PAnsiChar(TestVar1.VRawByteString), 'AnsiString from String'+GetOptionString);
  Manager.SetAsAnsiString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(AnsiVar.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'AnsiString'+GetOptionString);
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'SetAsAnsiString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  // if OS codepage differs, ANSI may be unable to contain the characters
  if ZOSCodePage = ConSettings^.ClientCodePage^.CP then
    CheckEquals(Self.GetExpectedStringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF},
      TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromUTF8String;
begin
  FUniTempExp := ZRawToUnicode(UTF8Var.VRawByteString, ZCP_UTF8);
  FRawTempExp := ZUnicodeToRaw(FUniTempExp, ZOSCodePage);

  TestVar1 := Manager.Convert(UTF8Var, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar1.VRawByteString, 'AnsiString from UTF8String'+GetOptionString);

  Manager.SetAsAnsiString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar2.VRawByteString, 'SetAsAnsiString'+GetOptionString);

  TestVar1 := Manager.Convert(TestVar2, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  // if OS codepage differs, ANSI may be unable to contain the characters
  if ZOSCodePage = ConSettings^.ClientCodePage^.CP then
    CheckEquals(UTF8Var.VRawByteString, TestVar1.VRawByteString, 'UTF8String from AnsiString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromRawByteString;
var RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else if ConSettings^.ClientCodePage^.CP = zCP_WIN1250 then
    RawVar := Raw_CP1250_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(AnsiVar.VRawByteString),
    PAnsiChar(TestVar1.VRawByteString), 'AnsiString from RawByteString'+GetOptionString);
  Manager.SetAsAnsiString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(AnsiVar.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'AnsiString'+GetOptionString);
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'AnsiString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  // if OS codepage differs, ANSI may be unable to contain the characters
  if ZOSCodePage = ConSettings^.ClientCodePage^.CP then
    CheckEquals(RawVar.VRawByteString, TestVar1.VRawByteString, 'RawByteStringString from AnsiString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_AnsiStringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ZOSCodePage)),
    PAnsiChar(TestVar1.VRawByteString), 'AnsiString'+GetOptionString);
  Manager.SetAsAnsiString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ZOSCodePage)),
    PAnsiChar(TestVar2.VRawByteString), 'AnsiString'+GetOptionString);
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'AnsiString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  // if OS codepage differs, ANSI may be unable to contain the characters
  if ZOSCodePage = ConSettings^.ClientCodePage^.CP then
    CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from AnsiString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromString;
var
  StringVar: TZVariant;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(UTF8Var.VRawByteString), PAnsiChar(TestVar1.VRawByteString), 'UTF8String from String'+GetOptionString);
  Manager.SetAsUTF8String(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(UTF8Var.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(GetExpectedStringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String from UTF8String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromAnsiString;
begin
  FUniTempExp := ZRawToUnicode(AnsiVar.VRawByteString, ZOSCodePage);
  FRawTempExp := ZUnicodeToRaw(FUniTempExp, zCP_UTF8);

  TestVar1 := Manager.Convert(AnsiVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar1.VRawByteString, 'UTF8String from AnsiString'+GetOptionString);

  Manager.SetAsUTF8String(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar2.VRawByteString, 'UTF8String'+GetOptionString);

  TestVar1 := Manager.Convert(TestVar2, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(AnsiVar.VRawByteString, TestVar1.VRawByteString, 'AnsiString from UTF8String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromRawByteString;
var RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else if ConSettings^.ClientCodePage^.CP = zCP_WIN1250 then
    RawVar := Raw_CP1250_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(UTF8Var.VRawByteString), PAnsiChar(TestVar1.VRawByteString), 'UTF8String from RawByteString'+GetOptionString);
  Manager.SetAsUTF8String(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(UTF8Var.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(RawVar.VRawByteString, TestVar1.VRawByteString, 'RawByteStringString from UTf8String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UTF8StringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF WITH_RAWBYTESTRING}
  CheckEquals(PAnsiChar(UTF8String(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar1.VRawByteString), 'UTF8String from UnicodeString'+GetOptionString);
  {$ELSE}
  CheckEquals(PAnsiChar(UTF8Encode(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar1.VRawByteString), 'UTF8String from UnicodeString'+GetOptionString);
  {$ENDIF}
  Manager.SetAsUTF8String(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF WITH_RAWBYTESTRING}
  CheckEquals(PAnsiChar(UTF8String(UnicodeVar.VUnicodeString)), PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  {$ELSE}
  CheckEquals(PAnsiChar(UTF8Encode(UnicodeVar.VUnicodeString)),
    PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  {$ENDIF}
  CheckEquals(PAnsiChar(TestVar1.VRawByteString), PAnsiChar(TestVar2.VRawByteString), 'UTF8String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from UTF8String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromString;
var
  StringVar{$IFNDEF UNICODE}, RawVar{$ENDIF}: TZVariant;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(PAnsiChar(ZUnicodeToRaw(StringVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString from String'+GetOptionString);
  {$ELSE}
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else if ConSettings^.ClientCodePage^.CP = zCP_WIN1250 then
    RawVar := Raw_CP1250_Var
  else
    RawVar := Raw_CP1252_Var;
  CheckEquals(PAnsiChar(RawVar.VRawByteString),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString from String'+GetOptionString);
  {$ENDIF}
  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(PAnsiChar(ZUnicodeToRaw(GetExpectedStringVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString'+GetOptionString);
  {$ELSE}
  CheckEquals(PAnsiChar(RawVar.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString'+GetOptionString);
  {$ENDIF}
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(StringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String from RawByteString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromAnsiString;
begin
  FUniTempExp := ZRawToUnicode(AnsiVar.VRawByteString, ZOSCodePage);
  FRawTempExp := ZUnicodeToRaw(FUniTempExp, ConSettings^.ClientCodePage^.CP);

  TestVar1 := Manager.Convert(AnsiVar, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar1.VRawByteString, 'RawByteString CP:'+IntToStr(ConSettings^.ClientCodePage^.CP));

  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(FRawTempExp, TestVar2.VRawByteString, 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));

  TestVar1 := Manager.Convert(TestVar2, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(AnsiVar.VRawByteString, TestVar1.VRawByteString, 'AnsiString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromUTF8String;
begin
  TestVar1 := Manager.Convert(UTF8Var, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString CP:'+IntToStr(ConSettings^.ClientCodePage^.CP));

  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  ZEncoding.PRawToRawConvert(Pointer(UTF8Var.VRawByteString), Length(UTF8Var.VRawByteString), zCP_UTF8, ConSettings^.ClientCodePage^.CP, FRawTempExp);
  CheckEquals(FRawTempExp, TestVar2.VRawByteString, 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));

  TestVar1 := Manager.Convert(TestVar2, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UTF8Var.VRawByteString, TestVar1.VRawByteString, 'UTF8String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_RawByteStringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar1.VRawByteString), 'RawByteString CP:'+IntToStr(ConSettings^.ClientCodePage^.CP));
  Manager.SetAsRawByteString(TestVar2, TestVar1.VRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(ZUnicodeToRaw(UnicodeVar.VUnicodeString, ConSettings^.ClientCodePage^.CP)),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));
  CheckEquals(PAnsiChar(TestVar1.VRawByteString),
    PAnsiChar(TestVar2.VRawByteString), 'RawByteString CP: '+IntToStr(ConSettings^.ClientCodePage^.CP));
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_StringFromAnsiString;
begin
  TestVar1 := Manager.Convert(AnsiVar, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  FUniTempExp := ZRawToUnicode(AnsiVar.VRawByteString, zOSCodePage);
  CheckEquals(FUniTempExp, TestVar1.VUnicodeString, 'String from AnsiString'+GetOptionString);
  {$ELSE}
  PRawToRawConvert(Pointer(AnsiVar.VRawByteString), Length(AnsiVar.VRawByteString), zOSCodePage, GetW2A2WConversionCodePage(ConSettings), FRawTempExp);
  CheckEquals(FRawTempExp, TestVar1.VRawbyteString, 'String from AnsiString'+GetOptionString);
  {$ENDIF}
  Manager.SetAsString(TestVar2, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
  CheckEquals(Ord(vtString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(AnsiVar.VRawByteString, TestVar1.VRawByteString, 'AnsiString from String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_StringFromUTF8String;
begin
  TestVar1 := Manager.Convert(UTF8Var, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(GetExpectedStringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String from UTF8String'+GetOptionString);
  Manager.SetAsString(TestVar2, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
  CheckEquals(Ord(vtString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(GetExpectedStringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
  CheckEquals(TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(PAnsiChar(UTF8Var.VRawByteString), PAnsiChar(TestVar1.VRawByteString), 'UTF8String from String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_StringFromRawByteString;
var
  RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else if ConSettings^.ClientCodePage^.CP = zCP_WIN1250 then
    RawVar := Raw_CP1250_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(ZRawToUnicode(RawVar.VRawByteString, ConSettings^.ClientCodePage^.CP),
    TestVar1.VUnicodeString, 'String from RawByteString'+GetOptionString);
  {$ELSE}
  CheckEquals(GetExpectedStringVar.VRawByteString,
    TestVar1.VRawByteString, 'String from RawByteString'+GetOptionString);
  {$ENDIF}
  Manager.SetAsString(TestVar2, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
  CheckEquals(Ord(vtString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(ZRawToUnicode(RawVar.VRawByteString, ConSettings^.ClientCodePage^.CP),
    TestVar2.VUnicodeString, 'String'+GetOptionString);
  {$ELSE}
  CheckEquals(GetExpectedStringVar.VRawByteString,
    TestVar2.VRawByteString, 'String'+GetOptionString);
  {$ENDIF}

  CheckEquals(TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);

  CheckEquals(PAnsiChar(RawVar.VRawByteString), PAnsiChar(TestVar1.VRawByteString), 'RawByteString from String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_StringFromUnicodeString;
begin
  TestVar1 := Manager.Convert(UnicodeVar, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'String'+GetOptionString);
  {$ELSE}
  CheckEquals(ZUnicodeToRaw(UnicodeVar.VUnicodeString, GetW2A2WConversionCodePage(ConSettings)), TestVar1.VRawByteString, 'String'+GetOptionString);
  {$ENDIF}
  Manager.SetAsString(TestVar2, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
  CheckEquals(Ord(vtString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  {$IFDEF UNICODE}
  CheckEquals(UnicodeVar.VUnicodeString, TestVar2.VUnicodeString, 'String'+GetOptionString);
  {$ELSE}
  CheckEquals(ZUnicodeToRaw(UnicodeVar.VUnicodeString, GetW2A2WConversionCodePage(ConSettings)), TestVar2.VRawByteString, 'String'+GetOptionString);
  {$ENDIF}
  CheckEquals(TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar2.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'String'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from String'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UnicodeStringFromString;
var
  StringVar: TZVariant;
begin
  StringVar := GetTestStringVar;
  TestVar1 := Manager.Convert(StringVar, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UnicodeVar.VUnicodeString, TestVar1.VUnicodeString, 'UnicodeString from String'+GetOptionString);
  Manager.SetAsUnicodeString(TestVar2, TestVar1.VUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UnicodeVar.VUnicodeString, TestVar2.VUnicodeString, 'String'+GetOptionString);
  CheckEquals(TestVar1.VUnicodeString, TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtString);
  CheckEquals(Ord(vtString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(GetExpectedStringVar.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, TestVar1.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF}, 'AnsiString from UnicodeString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UnicodeStringFromAnsiString;
begin
  TestVar1 := Manager.Convert(AnsiVar, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(ZRawToUnicode(AnsiVar.VRawByteString, ZOSCodePage), TestVar1.VUnicodeString, 'UnicodeString from AnsiString'+GetOptionString);
  Manager.SetAsUnicodeString(TestVar2, TestVar1.VUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(ZRawToUnicode(AnsiVar.VRawByteString, ZOSCodePage), TestVar2.VUnicodeString, 'AnsiString'+GetOptionString);
  CheckEquals(TestVar1.VUnicodeString, TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtAnsiString);
  CheckEquals(Ord(vtAnsiString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(AnsiVar.VRawByteString, TestVar1.VRawByteString, 'AnsiString from UnicodeString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UnicodeStringFromUTF8String;
begin
  TestVar1 := Manager.Convert(UTF8Var, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals({$IFDEF WITH_RAWBYTESTRING}UnicodeString{$ELSE}UTF8ToString{$ENDIF}(UTF8Var.VRawByteString), TestVar1.VUnicodeString, 'UnicodeString from AnsiString'+GetOptionString);
  Manager.SetAsUnicodeString(TestVar2, TestVar1.VUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals({$IFDEF WITH_RAWBYTESTRING}UnicodeString{$ELSE}UTF8ToString{$ENDIF}(UTF8Var.VRawByteString), TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  CheckEquals(TestVar1.VUnicodeString, TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtUTF8String);
  CheckEquals(Ord(vtUTF8String), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(UTF8Var.VRawByteString, TestVar1.VRawByteString, 'UTF8String from UnicodeString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.Test_UnicodeStringFromRawByteString;
var
  RawVar: TZVariant;
begin
  if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
    RawVar := Raw_CPUTF8_Var
  else if ConSettings^.ClientCodePage^.CP = zCP_WIN1250 then
    RawVar := Raw_CP1250_Var
  else
    RawVar := Raw_CP1252_Var;
  TestVar1 := Manager.Convert(RawVar, vtUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(ZRawToUnicode(RawVar.VRawByteString, ConSettings^.ClientCodePage^.CP), TestVar1.VUnicodeString, 'UnicodeString from RawByteString'+GetOptionString);
  Manager.SetAsUnicodeString(TestVar2, TestVar1.VUnicodeString);
  CheckEquals(Ord(vtUnicodeString), Ord(TestVar2.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(ZRawToUnicode(RawVar.VRawByteString, ConSettings^.ClientCodePage^.CP), TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  CheckEquals(TestVar1.VUnicodeString, TestVar2.VUnicodeString, 'UnicodeString'+GetOptionString);
  TestVar1 := Manager.Convert(TestVar2, vtRawByteString);
  CheckEquals(Ord(vtRawByteString), Ord(TestVar1.VType), 'ZVariant-Type'+GetOptionString);
  CheckEquals(RawVar.VRawByteString, TestVar1.VRawByteString, 'RawByteString from UnicodeString'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.DoTestConvert;
begin
  Test_AnsiStringFromString;
  Test_AnsiStringFromUTF8String;
  CheckException(Test_AnsiStringFromRawByteString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  Test_AnsiStringFromUnicodeString;

  Test_UTF8StringFromString;
  Test_UTF8StringFromAnsiString;
  CheckException(Test_UTF8StringFromRawByteString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  Test_UTF8StringFromUnicodeString;

  Test_StringFromAnsiString;
  Test_StringFromUTF8String;
  CheckException(Test_StringFromRawByteString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  Test_StringFromUnicodeString;
  CheckException(Test_RawByteStringFromString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  CheckException(Test_RawByteStringFromAnsiString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  CheckException(Test_RawByteStringFromUTF8String, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  CheckException(Test_RawByteStringFromUnicodeString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
  Test_UnicodeStringFromString;
  Test_UnicodeStringFromAnsiString;
  Test_UnicodeStringFromUTF8String;
  CheckException(Test_UnicodeStringFromRawByteString, EZVariantException, '', 'Wrong RawByteString behavior'+GetOptionString);
end;

procedure TZDefVarManagerConvertCase.TestConvert;
var
  I: Integer;
begin
  for I:= 0 to FTestStringList.Count - 1 do begin
    // prepare test string
    S:= FTestStringList[I];

    UnicodeVar := EncodeUnicodeString(S);
    {$IFDEF UNICODE}
    AnsiVar := EncodeAnsiString(AnsiString(s));
    UStringVar := EncodeString(S);
    {$ELSE}
    String_CPUTF8_Var := EncodeString(ZEncoding.ZUnicodeToRaw(s, zCP_UTF8));
    String_CP1250_Var := EncodeString(ZEncoding.ZUnicodeToRaw(s, zCP_WIN1250));
    String_CP1252_Var := EncodeString(ZEncoding.ZUnicodeToRaw(s, zCP_WIN1252));
    AnsiVar := EncodeAnsiString(ZUnicodeToRaw(s, ZOSCodePage));
    {$ENDIF}
    UTF8Var := EncodeUTF8String({$IFDEF WITH_RAWBYTESTRING}UTF8String(s){$ELSE}UTF8Encode(S){$ENDIF});
    Raw_CPUTF8_Var := EncodeRawByteString(UTF8Encode(S));
    Raw_CP1250_Var := EncodeRawByteString(ZUnicodeToRaw(S, zCP_WIN1250));
    Raw_CP1252_Var := EncodeRawByteString(ZUnicodeToRaw(S, zCP_WIN1252));

    // run test
    DoTestConvert;
  end;
end;

{ TZClientVarManagerConvertCase }

procedure TZClientVarManagerConvertCase.SetUp;
begin
  FillChar(TestConSettings^.ClientCodePage^, SizeOf(TZCodePage), #0);
  ConSettings := TestConSettings;
  SetupConSettings;
  Manager := TZClientVariantManager.Create(ConSettings);
end;

procedure TZClientVarManagerConvertCase.SetupConSettings;
begin
  ConSettings^.W2A2WEncodingSource := encDefaultSystemCodePage;
  ConSettings^.ClientCodePage^.CP := ZOSCodePage;
  ConSettings^.ClientCodePage^.Encoding := {$IFDEF MSWINDOWS}ceAnsi{$ELSE}ceUTF8{$ENDIF};
  ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := True;
  if GetW2A2WConversionCodePage(ConSettings) = zCP_UTF8 then
    FNotEqualClientCP := 1252
  else
    FNotEqualClientCP := zCP_UTF8;
end;

procedure TZClientVarManagerConvertCase.DoTestConvert;
begin
  Test_AnsiStringFromString;
  Test_AnsiStringFromUTF8String;
  Test_AnsiStringFromRawByteString;
  Test_AnsiStringFromUnicodeString;

  Test_UTF8StringFromString;
  Test_UTF8StringFromAnsiString;
  Test_UTF8StringFromRawByteString;
  Test_UTF8StringFromUnicodeString;

  Test_RawByteStringFromString;
  Test_RawByteStringFromAnsiString;
  Test_RawByteStringFromUTF8String;
  Test_RawByteStringFromUnicodeString;

  Test_StringFromAnsiString;
  Test_StringFromUTF8String;
  Test_StringFromRawByteString;
  Test_StringFromUnicodeString;

  Test_UnicodeStringFromString;
  Test_UnicodeStringFromAnsiString;
  Test_UnicodeStringFromUTF8String;
  Test_UnicodeStringFromRawByteString;
end;

{ TZClientVarManagerConvertCaseUTF8 }

procedure TZClientVarManagerConvertCaseUTF8.SetupConSettings;
begin
  inherited;
  ConSettings^.W2A2WEncodingSource := encUTF8;
  ConSettings^.ClientCodePage^.CP := zCP_UTF8;
  ConSettings^.ClientCodePage.Encoding := ceUTF8;
  FNotEqualClientCP := zCP_WIN1252;
end;

{ TZClientVarManagerConvertCase_ControsCP_UTF8_ClientCP_WIN1252 }
procedure TZClientVarManagerConvertCase_ControlsCP_UTF8_ClientCP_WIN1252.SetupConSettings;
begin
  inherited;
  ConSettings^.ClientCodePage^.CP := zCP_WIN1252;
  ConSettings^.ClientCodePage.Encoding := ceAnsi;
  FNotEqualClientCP := zCP_UTF8;
End;

{ TZClientVarManagerConvertCaseWin1252 }

procedure TZClientVarManagerConvertCaseWin1252.AfterConstruction;
begin
  inherited;
{$IFDEF UNICODE}
  FTestStringList.Add(Char($0FC)+Char($0E4)+Char($0F6)+Char($0DF)+Char($0E2)+Char($0E1)+Char($0E0));
  FTestStringList.Add('extended '+Char($0FC)+Char($0E4)+Char($0F6)+Char($0DF)+Char($0E2)+Char($0E1)+Char($0E0));
{$ELSE}
  if ZOSCodePage = 1252 then begin
    FTestStringList.Add(Chr($FC)+Chr($E4)+Chr($F6)+Chr($DF)+Chr($E2)+Chr($E1)+Chr($E0));
    FTestStringList.Add('extended '+Chr($FC)+Chr($E4)+Chr($F6)+Chr($DF)+Chr($E2)+Chr($E1)+Chr($E0));
  end;
{$ENDIF}
end;

procedure TZClientVarManagerConvertCaseWin1252.SetupConSettings;
begin
  inherited;
  ConSettings^.W2A2WEncodingSource := encDB_CP;
  ConSettings^.ClientCodePage^.CP := zCP_WIN1252;
  ConSettings^.ClientCodePage.Encoding := ceAnsi;
  FNotEqualClientCP := zCP_UTF8;
end;

{ TZClientVarManagerConvertCase_ControlsCP_WIN1252_ClientCP_UTF8 }
procedure TZClientVarManagerConvertCase_ControlsCP_WIN1252_ClientCP_UTF8.SetupConSettings;
begin
  inherited;
  ConSettings^.ClientCodePage^.CP := zCP_UTF8;
  ConSettings^.ClientCodePage.Encoding := ceUTF8;
  FNotEqualClientCP := zCP_WIN1252;
end;

{ TZClientVarManagerConvertCaseWin1250 }

procedure TZClientVarManagerConvertCaseWin1250.AfterConstruction;
begin
  inherited;
{$IFDEF UNICODE}
  FTestStringList.Add('diacritic test ' + Char($010D)+Char($010E)+Char($011B)+Char($0148)+Char($0158)+Char($0161)+Char($0164)+Char($017E));
  FTestStringList.Add(Char($010D)+Char($010E)+Char($011B)+Char($0148)+Char($0158)+Char($0161)+Char($0164)+Char($017E) + ' after');
  FTestStringList.Add('test ' + Char($010D)+Char($010E)+Char($011B)+Char($0148)+' zulu ' +Char($0158)+Char($0161)+Char($0164)+Char($017E));
{$ELSE}
  if ZOSCodePage = 1250 then begin
    FTestStringList.Add('diacritic test ' + Chr($E8)+Chr($CF)+Chr($EC)+Chr($F2)+Chr($D8)+Chr($9A)+Chr($8D)+Chr($9E));
    FTestStringList.Add(Chr($E8)+Chr($CF)+Chr($EC)+Chr($F2)+Chr($D8)+Chr($9A)+Chr($8D)+Chr($9E) + ' after');
    FTestStringList.Add('test ' + Chr($E8)+Chr($CF)+Chr($EC)+Chr($F2)+' zulu ' +Chr($D8)+Chr($9A)+Chr($8D)+Chr($9E));
  end;
{$ENDIF}
end;

procedure TZClientVarManagerConvertCaseWin1250.SetupConSettings;
begin
  inherited;
  ConSettings^.W2A2WEncodingSource := encDB_CP;
  ConSettings^.ClientCodePage^.CP := zCP_WIN1250;
  ConSettings^.ClientCodePage.Encoding := ceAnsi;
  FNotEqualClientCP := zCP_UTF8;
end;

{ TZClientVarManagerConvertCase_ControlsCP_WIN1250_ClientCP_UTF8 }

procedure TZClientVarManagerConvertCase_ControlsCP_WIN1250_ClientCP_UTF8.SetupConSettings;
begin
  inherited;
  ConSettings^.ClientCodePage^.CP := zCP_UTF8;
  ConSettings^.ClientCodePage.Encoding := ceUTF8;
  FNotEqualClientCP := zCP_WIN1250;
end;

initialization
  TestConSettings := New(PZConSettings);
  TestConSettings^.ClientCodePage := New(PZCodePage);
  FillChar(TestConSettings^.ClientCodePage^, SizeOf(TZCodePage), #0);

  RegisterTest('core',TZTestVariantCase.Suite);
  RegisterTest('core',TZDefVarManagerConvertCase.Suite);
  RegisterTest('core',TZClientVarManagerConvertCaseUTF8.Suite);
  RegisterTest('core',TZClientVarManagerConvertCase_ControlsCP_UTF8_ClientCP_WIN1252.Suite);
  RegisterTest('core',TZClientVarManagerConvertCaseWin1252.Suite);
  RegisterTest('core',TZClientVarManagerConvertCase_ControlsCP_WIN1252_ClientCP_UTF8.Suite);
  RegisterTest('core',TZClientVarManagerConvertCaseWin1250.Suite);
  RegisterTest('core',TZClientVarManagerConvertCase_ControlsCP_WIN1250_ClientCP_UTF8.Suite);

finalization
  Dispose(TestConSettings^.ClientCodePage);
  Dispose(TestConSettings);
end.
