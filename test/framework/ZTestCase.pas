{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{ Originally written by Sergey Merkuriev, Sergey Seroukhov}
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

unit ZTestCase;

interface

{$I ZTestFramework.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, {$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF}, SysUtils, StrUtils,
  ZCompatibility;

type
  {$IFDEF FPC}
  CTZAbstractTestCase = Class of TZAbstractTestCase;
  TTestMethod  = TRunMethod;
  {$ENDIF}

  TDatePart = (dpYear, dpMonth, dpDay, dpHour, dpMin, dpSec, dpMSec);
  TDateParts = set of TDatePart;
  ZSkipReason = (srClosedBug,srNonZeos,srNoPerformance
                 //database dependent
                 ,srMysqlRealPreparedConnection
                );
  ZSkipReasons = set of ZSkipReason;

  {** Implements an abstract class for all test cases. }

  { TZAbstractTestCase }

  TZAbstractTestCase = class(TTestCase)
  private
    FDecimalSeparator: Char;
    FSuppressTestOutput: Boolean;
    FSkipClosed: Boolean;
    FSkipNonZeos: Boolean;
    FSkipPerformance: Boolean;
  protected
    {$IFDEF FPC}
    frefcount : longint;
    { implement methods of IUnknown }
    {$IFDEF FPC2_5UP}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid; out obj) : HResult;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function QueryInterface(const iid : tguid;out obj) : HResult;stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
    {$ENDIF}
    function GetMemDiffStr(Expected, Actual: Pointer; Size: Longword; const Msg: string = ''): string;
    {$ENDIF}
    property DecimalSeparator: Char read FDecimalSeparator
      write FDecimalSeparator;
    property SuppressTestOutput: Boolean read FSuppressTestOutput
      write FSuppressTestOutput;

    function SkipForReason(Reasons: ZSkipReasons): Boolean; overload; virtual;
    function SkipForReason(Reason: ZSkipReason): Boolean; overload;

    function TestFilePath(const RelFilePath: string): string;

    { Test configuration methods. }
    procedure LoadConfiguration; virtual;

    { Configuration properties accessing methods. }
    function ReadProperty(const Group, Key, Default: string): string;
    function ReadGroupProperty(const Key, Default: string): string;
    {$IFDEF WITH_CLASS_VARS}class{$ENDIF} function ReadInheritProperty(const Key, Default: string): string;

    { Visual output methods. }
    procedure Print(const Msg: string); virtual;
    procedure PrintLn(const Msg: string = ''); virtual;

    { Additional checking methods. }
    procedure CheckEquals(const Expected: RawByteString; ActualValue: PAnsiChar;
      ActualLen: PNativeUInt; const Msg: string = ''); overload;
    procedure CheckEquals(const Expected, Actual: TBytes;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: String; ConSettings: PZConSettings;
      const Msg: string = ''); overload;
    {$IFNDEF UNICODE}
    procedure CheckEquals(Expected: ZWideString; Actual: String; ConSettings: PZConSettings;
      const Msg: string = ''); overload;
    {$ENDIF UNICODE}
    procedure CheckEquals(OrgStr: ZWideString; ActualLobStream: TStream; ConSettings: PZConSettings;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: TStream;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: PAnsiChar;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: PAnsiChar;
      const Msg: string = ''); overload;
    {$IFDEF FPC}
    procedure CheckEqualsMem(Expected, Actual: Pointer; Size: Longword; const Msg: string = '');
    procedure CheckNotEqualsMem(Expected, Actual: Pointer; Size: Longword; const Msg: string = '');
    procedure CheckEquals(Expected, Actual: WideString;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: WideString;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: UInt64;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: UInt64;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: Int64;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: Int64;
      const Msg: string = ''); overload;
    {$ENDIF}
    {$IFDEF WITH_OVERLOAD_BUG}
    procedure CheckEquals(Expected, Actual: Word;
      const Msg: string = ''); overload;
    procedure CheckEquals(Expected, Actual: Byte;
      const Msg: string = ''); overload;
    procedure CheckNotEquals(Expected, Actual: Byte;
      const Msg: string = ''); overload;
    {$ENDIF}
    procedure CheckEqualsDate(const Expected, Actual: TDateTime;
      Parts: TDateParts = []; const Msg: string = '');
    procedure CheckException(AMethod: TTestMethod; AExceptionClass: ExceptClass;
      const ExpectExcMsg: string = ''; const Msg: string = ''); overload;
    procedure CheckNotTestFailure(E: Exception; const Msg: string = '');
    procedure BlankCheck;
    { Measurement methods. }
    function GetTickCount: Cardinal;
  public
    constructor Create(MethodName: string); {$IFNDEF FPC} override; {$ELSE} overload; virtual;{$ENDIF}
    destructor Destroy; override;
    {$IFDEF FPC}
    constructor Create; override; overload;
    class function Suite : CTZAbstractTestCase;
    property GetName: string read GetTestName;
    {$ENDIF}
  end;

  {** Implements a generic test case. }
  TZGenericTestCase = class (TZAbstractTestCase);

function AddToMsg(const Msg, Add: string): string;

implementation

uses
{$IFDEF FPC}
  LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF WITH_STRLEN_DEPRECATED}
  AnsiStrings,
{$ENDIF}
{$IFDEF WITH_INLINE}
  ZFastCode,
{$ENDIF}
  ZSysUtils, ZTestConfig, ZEncoding;

const
  SStringLengthsDiffer = 'string lengths differ';
  SStringDataDiffer = 'string data differ';
  SArrayLengthsDiffer = 'array lengths differ';
  SArrayDataDiffer = 'array data differ';
  SStreamSizesDiffer = 'stream sizes differ';
  SStreamDataDiffer = 'stream data differ';
  SExpectedException = 'expected exception but TestFailure raised';
{$IFDEF FPC}
  SIdenticalContent = 'memory content was identical';
{$ELSE}
  SNoException = 'no exception';
  SExceptionMsgDiffer = 'exception messages differ';
{$ENDIF}

function AddToMsg(const Msg, Add: string): string;
begin
  if Msg <> ''
    then Result := Msg + ', ' + Add
    else Result := Add;
end;

{$IFDEF FPC} // copy from DUnit to implement generic interface

{$IF NOT DECLARED(CallerAddr)} // for older FPC versions < 3.1
const
  CallerAddr = nil;
{$IFEND}

function ByteAt(p: pointer; const Offset: integer): byte;
begin
  Result:=(PByte(p)+Offset)^;
end;

function FirstByteDiff(p1, p2: pointer; size: longword; out b1, b2: byte): integer;
// Returns offset of first byte pair (left to right, incrementing address) that is unequal
// Returns -1 if no difference found, or if size=0
var
  i: integer;
begin
  Result:=-1;
  if size>0 then
  for i:=0 to size-1 do // Subject to optimisation for sure:
    if ByteAt(p1,i)<>ByteAt(p2,i) then
    begin
      Result:=i;
      b1:=ByteAt(p1,i);
      b2:=ByteAt(p2,i);
      break;
    end;
end;
{$ENDIF FPC}

{ TZAbstractTestCase }

{$IFDEF FPC}
function TZAbstractTestCase.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  if getinterface(iid,obj) then
   result:=0
  else
   result:=longint(E_NOINTERFACE);
end;

function TZAbstractTestCase._AddRef: longint;
begin
  _addref:=interlockedincrement(frefcount);
end;

function TZAbstractTestCase._Release: longint;
begin
  _Release:=interlockeddecrement(frefcount);
  if _Release=0 then self.destroy;
end;

class function TZAbstractTestCase.Suite: CTZAbstractTestCase;
begin
  Result := Self;
end;

// copy from DUnit
procedure TZAbstractTestCase.CheckEqualsMem(Expected, Actual: Pointer; Size: Longword; const Msg: string);
begin
  if not CompareMem(expected, actual, size) then
    {$IFDEF FPC2_6DOWN}
    Fail(GetMemDiffStr(expected, actual, size, msg))
    {$ELSE}
    Fail(GetMemDiffStr(expected, actual, size, msg), CallerAddr)
    {$ENDIF}
  else
    Check(True);
end;

procedure TZAbstractTestCase.CheckNotEqualsMem(Expected, Actual: Pointer; Size: Longword; const Msg: string);
begin
  if CompareMem(expected, actual, size) then
    {$IFDEF FPC2_6DOWN}
    Fail(AddToMsg(Msg, sIdenticalContent))
    {$ELSE}
    Fail(AddToMsg(Msg, sIdenticalContent), CallerAddr)
    {$ENDIF}
  else
    Check(True);
end;

constructor TZAbstractTestCase.Create;
begin
  inherited Create;
  LoadConfiguration;
end;

function TZAbstractTestCase.GetMemDiffStr(Expected, Actual: Pointer; Size: Longword; const Msg: string): string;
var
  db1, db2: byte;
  Offset: integer;
begin
  Offset:=FirstByteDiff(expected,actual,size,db1,db2);
  Result:=NotEqualsErrorMessage(IntToHex(db1,2),IntToHex(db2,2),msg);
  Result:=Result+' at Offset = '+IntToHex(Offset,4)+'h';
end;
{$ENDIF FPC}

function TZAbstractTestCase.SkipForReason(Reasons: ZSkipReasons): Boolean;
begin
  Result := (FSkipClosed and (srClosedBug in Reasons)) or
            (FSkipNonZeos and (srNonZeos in Reasons)) or
            (FSkipPerformance and (srNoPerformance in Reasons));
  if Result then
    BlankCheck; // avoids an empty test fail
end;

function TZAbstractTestCase.SkipForReason(Reason: ZSkipReason): Boolean;
begin
  Result := SkipForReason([Reason]);
end;

{**
  Creates the abstract test case and initialize global parameters.
  @param MethodName a name of the test case.
}
constructor TZAbstractTestCase.Create(MethodName: string);
begin
  {$IFNDEF FPC}
  inherited Create(MethodName);
  LoadConfiguration;
  {$ELSE}
  inherited CreateWithName(MethodName);
  {$ENDIF}
end;

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractTestCase.Destroy;
begin
  inherited Destroy;
end;

{**
  Returns absolute path to a test file located in DEFAULT_CONFIG_DIR
}
function TZAbstractTestCase.TestFilePath(const RelFilePath: string): string;
begin
  Result := TestConfig.PathFromConfig(RelFilePath);
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZAbstractTestCase.LoadConfiguration;
var
  Temp: string;
begin
  { Defines a decimal separator for the tests. }
  Temp := ReadInheritProperty(DECIMAL_SEPARATOR_KEY,
    DEFAULT_DECIMAL_SEPARATOR);
  if Temp <> '' then
    FDecimalSeparator := Temp[1]
  else FDecimalSeparator :=  DEFAULT_DECIMAL_SEPARATOR;
  {$IFDEF WITH_FORMATSETTINGS}Formatsettings.{$ENDIF}DecimalSeparator := FDecimalSeparator;

  FSuppressTestOutput := StrToBoolEx(ReadInheritProperty(SUPPRESS_TEST_OUTPUT_KEY, TRUE_VALUE));
  FSkipClosed := StrToBoolEx(ReadInheritProperty(SKIP_CLOSED_KEY, FALSE_VALUE));
  FSkipNonZeos := StrToBoolEx(ReadInheritProperty(SKIP_NON_ZEOS_ISSUES_KEY, FALSE_VALUE));
  FSkipPerformance := StrToBoolEx(ReadInheritProperty(SKIP_PERFORMANCE_KEY, TRUE_VALUE));
end;

{**
  Reads a configuration property from test config file.
  @param Key a property key.
  @param Default a property default value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZAbstractTestCase.ReadProperty(
  const Group, Key, Default: string): string;
begin
  Result := TestConfig.ReadProperty(Group, Key, Default);
end;

{**
  Reads a configuration property from test config file
  for the current test group defined by TestGroup global variable.
  @param Key a property key.
  @param Default a property default value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZAbstractTestCase.ReadGroupProperty(
  const Key, Default: string): string;
begin
  Result := TestConfig.ReadProperty(TestGroup, Key, Default);
end;

{**
  Reads a configuration property for the current test group
  defined by TestGroup global variable. If key is not defined
  in the current group it reads the same key from "common" group.
  @param Key a property key.
  @param Default a property default value.
  @returns an interited property value or default value if property
    was not found in the config file neither in the current
    nor in the "common" group.
}
{$IFDEF WITH_CLASS_VARS}class{$ENDIF} function TZAbstractTestCase.ReadInheritProperty(
  const Key, Default: string): string;
const
  UNKNOWN_VALUE = '';
begin
  Result := TestConfig.ReadProperty(TestGroup, Key, UNKNOWN_VALUE);
  if Result = UNKNOWN_VALUE then
    Result := TestConfig.ReadProperty(COMMON_GROUP, Key, Default);
end;

{**
  Function compare two string-values. If values not equals raise exception.
  @param the first string for compare
  @param the second pointer to a !possible! null terminated string
  @param the pointer to Length of second value
}
procedure TZAbstractTestCase.CheckEquals(const Expected: RawByteString;
  ActualValue: PAnsiChar; ActualLen: PNativeUInt; const Msg: string);
var Actual: RawByteString;
begin
  ZSetString(ActualValue, ActualLen^, Actual);
  CheckEquals(Expected, Actual, Msg);
end;

{**
  Function compare two arrays. If arrays not equals raise exception.
  @param the first array for compare
  @param the secon array for compare
}
procedure TZAbstractTestCase.CheckEquals(const Expected, Actual: TBytes;
  const Msg: string);
begin
  CheckEquals(Length(Expected), Length(Actual), AddToMsg(Msg, SArrayLengthsDiffer));
  CheckEqualsMem(Expected, Actual, Length(Actual), AddToMsg(Msg, SArrayDataDiffer) );
end;

{**
   Function compare two strings with depenedent to the ConnectionSettings.
   If strings not equals raise exception.
   @param Expected the first stream for compare
   @param Actual the second stream for compare
   @param ConSettings the Connection given settings
}
procedure TZAbstractTestCase.CheckEquals(Expected, Actual: String; ConSettings: PZConSettings;
  const Msg: string);
{$IFNDEF UNICODE}
var Temp: String;
{$ENDIF}
begin
  {$IFNDEF UNICODE}
  if ConSettings.ClientCodePage.Encoding = ceUTF8 then
    if (ConSettings.CPType = cCP_UTF8) then
      Temp := UTF8Encode(WideString(Expected))
    else //cGET_ACP / cCP_UTF16
      if Consettings.CTRL_CP = zCP_UTF8 then
        Temp := UTF8Encode(WideString(Expected))
      else
        if ConSettings.AutoEncode or ( ConSettings.CPType = cCP_UTF16 ) then
          Temp := Expected
        else
          Temp := UTF8Encode(WideString(Expected))
  else //ceAnsi
    if ( ConSettings.CPType = cGET_ACP ) or ( ConSettings.CPType = cCP_UTF16 ) then //ftWideString returns a decoded value
      if Consettings.CTRL_CP = zCP_UTF8 then
        Temp := UTF8Encode(WideString(Expected))
      else
        Temp := Expected
    else
      //cCP_UTF8
      if ConSettings.CPType = cCP_UTF16 then
        if Consettings.CTRL_CP = zCP_UTF8 then
          Temp := UTF8Encode(WideString(Expected))
        else
          Temp := Expected
      else
        if ConSettings.AutoEncode then
          Temp := UTF8Encode(WideString(Expected))
        else
          Temp := Expected;
  {$ENDIF}
  CheckEquals({$IFNDEF UNICODE}Temp{$ELSE}Expected{$ENDIF}, Actual, Msg)
end;

{**
   Function compare a Original-given String with then BlobStream dependend to the ConnectionSettings.
   If streams not equals raise exception.
   @param Expected the first stream for compare
   @param Actual the second stream for compare
   @param ConSettings the Connection given settings
}
procedure TZAbstractTestCase.CheckEquals(OrgStr: ZWideString; ActualLobStream: TStream;
  ConSettings: PZConSettings; const Msg: string);
var
  StrStream: TMemoryStream;
  procedure SetAnsiStream(Value: RawByteString);
  begin
    StrStream.Write(PAnsiChar(Value)^, Length(Value));
    StrStream.Position := 0;
  end;
begin
  StrStream := TMemoryStream.Create;
  case ConSettings.CPType of
    cGET_ACP, cCP_UTF8:
      if ConSettings.AutoEncode then
        SetAnsiStream(ZUnicodeToRaw(OrgStr, ConSettings.CTRL_CP))
      else
        SetAnsiStream(ZUnicodeToRaw(OrgStr, ConSettings^.ClientCodePage^.CP));
    cCP_UTF16:
      begin
        StrStream.Write(PWideChar(OrgStr)^, Length(OrgStr)*2);
        StrStream.Position := 0;
      end;
  end;
  try
    CheckEquals(StrStream, ActualLobStream, Msg);
  finally
    StrStream.Free;
  end;
end;

{**
   Function compare two streams. If streams not equals raise exception.
   @param Expected the first stream for compare
   @param Actual the second stream for compare
}
procedure TZAbstractTestCase.CheckEquals(Expected, Actual: TStream;
  const Msg: string);
var
  EBuf, ABuf: TBytes;
  Size, ERead, ARead: Integer;
begin
  if Expected = Actual then
  begin
    Check(True);
    Exit;
  end;
  if not Assigned(Actual) and Assigned(Expected) then
    Fail(NotEqualsErrorMessage('Not NIL', 'NIL', Msg));
  if Assigned(Actual) and not Assigned(Expected) then
    Fail(NotEqualsErrorMessage('NIL', 'Not NIL', Msg));
  CheckEquals(Expected.Size, Actual.Size, AddToMsg(Msg, SStreamSizesDiffer));
  Size := Expected.Size;
  SetLength(EBuf, Size);
  SetLength(ABuf, Size);
  Expected.Position := 0;
  Actual.Position := 0;
  ERead := Expected.Read(Pointer(EBuf)^, Size);
  ARead := Actual.Read(Pointer(ABuf)^, Size);
  Expected.Position := 0;
  Actual.Position := 0;
  CheckEquals(ERead, ARead, AddToMsg(Msg, SStreamSizesDiffer));
  CheckEqualsMem(EBuf, ABuf, Size, AddToMsg(Msg, SStreamDataDiffer));
end;

procedure TZAbstractTestCase.CheckEquals(Expected, Actual: PAnsiChar;
  const Msg: string);
var LenE, LenA: Integer;
begin
  LenE := {$IFDEF WITH_STRLEN_DEPRECATED}AnsiStrings.{$ENDIF}StrLen(Expected);
  LenA := {$IFDEF WITH_STRLEN_DEPRECATED}AnsiStrings.{$ENDIF}StrLen(Actual);
  CheckEquals(LenE, LenA, AddToMsg(Msg, SStringLengthsDiffer));
  CheckEqualsMem(Expected, Actual, LenE, AddToMsg(Msg, SStringDataDiffer));
end;

procedure TZAbstractTestCase.CheckNotEquals(Expected, Actual: PAnsiChar;
  const Msg: string);
var LenE, LenA: Integer;
begin
  LenE := {$IFDEF WITH_STRLEN_DEPRECATED}AnsiStrings.{$ENDIF}StrLen(Expected);
  LenA := {$IFDEF WITH_STRLEN_DEPRECATED}AnsiStrings.{$ENDIF}StrLen(Actual);
  if LenE = LenA then
    CheckNotEqualsMem(Expected, Actual, LenE, Msg)
  else
    Check(True);
end;

{$IFDEF FPC}
// Made in accordance with DUnitCompatibleInterface.inc
procedure TZAbstractTestCase.CheckEquals(Expected, Actual: WideString;
  const Msg: string);
begin
  {$IFDEF FPC2_6DOWN}
  AssertTrue(ComparisonMsg(Expected, Actual), Expected = Actual);
  {$ELSE}
  AssertTrue(ComparisonMsg(Msg, Expected, Actual), Expected = Actual, CallerAddr);
  {$ENDIF}
end;

procedure TZAbstractTestCase.CheckNotEquals(Expected, Actual: WideString;
  const Msg: string);
begin
  if (Expected = Actual) then
    Fail(Msg + ComparisonMsg(Expected, Actual, False))
  else
    Check(True);
end;

procedure TZAbstractTestCase.CheckEquals(Expected, Actual: UInt64;
  const Msg: string);
begin
  {$IFDEF FPC2_6DOWN}
  AssertTrue(ComparisonMsg(IntToStr(Expected), IntToStr(Actual)), Expected = Actual);
  {$ELSE}
  AssertTrue(ComparisonMsg(Msg, IntToStr(Expected), IntToStr(Actual)), Expected = Actual, CallerAddr);
  {$ENDIF}
end;

procedure TZAbstractTestCase.CheckNotEquals(Expected, Actual: UInt64;
  const Msg: string);
begin
  if (Expected = Actual) then
    Fail(Msg + ComparisonMsg(IntToStr(Expected), IntToStr(Actual), False))
  else
    Check(True);
end;

procedure TZAbstractTestCase.CheckEquals(Expected, Actual: Int64;
  const Msg: string);
begin
  AssertEquals(Msg, Expected, Actual);
end;

procedure TZAbstractTestCase.CheckNotEquals(Expected, Actual: Int64;
  const Msg: string);
begin
  if (Expected = Actual) then
    Fail(Msg + ComparisonMsg(IntToStr(Expected), IntToStr(Actual), False))
  else
    Check(True);
end;
{$ENDIF FPC}

{$IFDEF WITH_OVERLOAD_BUG}
procedure TZAbstractTestCase.CheckEquals(Expected, Actual: Word;
  const Msg: string);
begin
  inherited CheckEquals(Integer(Expected), Integer(Actual), Msg)
end;

procedure TZAbstractTestCase.CheckEquals(Expected, Actual: Byte;
  const Msg: string);
begin
  inherited CheckEquals(Integer(Expected), Integer(Actual), Msg)
end;

procedure TZAbstractTestCase.CheckNotEquals(Expected, Actual: Byte;
  const Msg: string = '');
begin
  CheckNotEquals(Integer(Expected), Integer(Actual), Msg)
end;
{$ENDIF WITH_OVERLOAD_BUG}

{$IFNDEF UNICODE}
procedure TZAbstractTestCase.CheckEquals(Expected: ZWideString; Actual: String;
  ConSettings: PZConSettings; const Msg: string);
begin
  if ConSettings^.AutoEncode or (ConSettings^.ClientcodePage^.Encoding = ceUTF16) or
     (not ConSettings^.ClientcodePage^.IsStringFieldCPConsistent) or
     (ConSettings^.CPType = cCP_UTF16) then
    CheckEquals(ZUnicodeToRaw(Expected, ConSettings^.CTRL_CP), Actual, Msg)
    //CheckEquals(Expected, ZRawToUnicode(Actual, ConSettings^.CTRL_CP), Msg)
  else
    CheckEquals(Expected, ZRawToUnicode(Actual, ConSettings^.ClientcodePage^.CP), Msg);
end;
{$ENDIF UNICODE}

procedure TZAbstractTestCase.CheckEqualsDate(const Expected, Actual: TDateTime;
  Parts: TDateParts; const Msg: string);
var
  EYear, EMonth, EDay, EHour, EMin, ESec, EMSec: Word;
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Word;
  s: string;
begin
  if Parts = [] then
    Parts := [dpYear..dpMSec];
  s := NotEqualsErrorMessage(FormatDateTime(DefDateTimeFormatMsecsYMD, Expected), FormatDateTime(DefDateTimeFormatMsecsYMD, Actual), Msg);
  DecodeDate(Expected, EYear, EMonth, EDay);
  DecodeTime(Expected, EHour, EMin, ESec, EMSec);
  DecodeDate(Actual, AYear, AMonth, ADay);
  DecodeTime(Actual, AHour, AMin, ASec, AMSec);
  if dpYear in Parts then CheckEquals(Cardinal(EYear), Cardinal(AYear), s);
  if dpMonth in Parts then CheckEquals(Cardinal(EMonth), Cardinal(AMonth), s);
  if dpDay in Parts then CheckEquals(Cardinal(EDay), Cardinal(ADay), s);
  if dpHour in Parts then CheckEquals(Cardinal(EHour), Cardinal(AHour), s);
  if dpMin in Parts then CheckEquals(Cardinal(EMin), Cardinal(AMin), s);
  if dpSec in Parts then CheckEquals(Cardinal(ESec), Cardinal(ASec), s);
  if dpMSec in Parts then CheckEquals(Cardinal(EMSec), Cardinal(AMSec), s);
end;

{**
   Checks if a method raises expected exception. Unlike TAbstractTest.CheckException,
   additionally checks for exception message.
   @param AMethod method to call
   @param AExceptionClass class of expected exception. If no exception expected,
     set this to nil. Then the function will fail if AMethod raises any exception.
   @param ExcMsg message of expected exception. If empty, exception message won't
     be checked.
   @param Msg message
}
procedure TZAbstractTestCase.CheckException(AMethod: TTestMethod;
  AExceptionClass: ExceptClass; const ExpectExcMsg, Msg: string);
begin
  {$IFDEF FPC}
    {$IFDEF FPC2_6DOWN}
    // Note: actually this call won't check exception message
    AssertException(Msg, AExceptionClass, AMethod);
    {$ELSE}
    AssertException(Msg, AExceptionClass, AMethod, ExpectExcMsg, 0, CallerAddr);
    {$ENDIF}
  {$ELSE}
  if ExpectExcMsg = '' then
    inherited CheckException(AMethod, AExceptionClass, Msg)
  else
  begin
    // run inherited method with AExceptionClass = nil - this will invoke
    // AMethod and passthrough any exception it could raise.
    try
      inherited CheckException(AMethod, nil);
    except on E: Exception do
      begin
        // exception raised but not expected at all
        if not Assigned(AExceptionClass) then
          raise;
        // exception raised is of other class than expected
        if not E.ClassType.InheritsFrom(AExceptionClass) and not (AExceptionClass.ClassName = E.ClassName) then
          FailNotEquals(AExceptionClass.ClassName, E.ClassName, Msg, ReturnAddress);
        // exception raised with message other than expected
        CheckEquals(E.Message, ExpectExcMsg, AddToMsg(Msg, SExceptionMsgDiffer));
        Exit; // OK
      end;
    end; // try
    Fail(NotEqualsErrorMessage(AExceptionClass.ClassName, SNoException, Msg));
  end;
  {$ENDIF}
end;

{**
  Checks that exception wasn't raised by Fail method. This allows to check against
  expected exception, like
    try
      DoSmthThatRaisesException;
      Fail('');
    except on E: Exception do
      CheckNotTestFailure(E, 'DoSmthThatRaisesException method');
    end;

    - or -

    try
      DoSmthThatRaisesException;
      Fail('DoSmthThatRaisesException method');
    except on E: Exception do
      CheckNotTestFailure(E);
    end;

  @param Exception an exception raised
  @param Msg message. If empty, E.Message will be used
}
procedure TZAbstractTestCase.CheckNotTestFailure(E: Exception; const Msg: string);
begin
  Check(not (E is {$IFDEF FPC} EAssertionFailedError {$ELSE} ETestFailure {$ENDIF}),
        AddToMsg( IfThen(Msg <> '', Msg, E.Message), SExpectedException ) );
end;

{**
   Just a check that always succeeds. Use it to avoid "test has no assertions"
   error in places where none of Check* methods is used.
}
procedure TZAbstractTestCase.BlankCheck;
begin
  Check(True);
end;

{**
  Prints a debug message to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.Print(const Msg: string);
begin
  if not SuppressTestOutput then
    System.Write(Msg);
end;

{**
  Prints a debug message ends with EOL to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.PrintLn(const Msg: string);
begin
  if not SuppressTestOutput then
    System.WriteLn(Msg);
end;

{**
  Gets a system specific number of ticks.
  @return the current number of system ticks.
}
function TZAbstractTestCase.GetTickCount: Cardinal;
begin
{$IFDEF FPC}
  Result := LCLIntf.GetTickCount;
{$ELSE}
  Result := Windows.GetTickCount;
{$ENDIF}
end;

end.

