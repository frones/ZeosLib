{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Utility Functions              }
{                                                         }
{         Originally written by Sergey Merkuriev          }
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

unit ZTestSysUtils;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils, Classes,
  ZTestCase, ZSysUtils, ZClasses, ZVariant, ZMatchPattern, ZCompatibility,
  ZDbcUtils;

type
  {$UNDEF WITH_UNICODEFROMLOCALECHARS}
  {** Implements a test case for Utilities. }
  TZTestSysUtilsCase = class(TZGenericTestCase)
  private
    FQuote: string;
    FSrc: string;
    procedure RunDequotedStr;
    procedure TestStringReplaceAll_CS_LToEQ_A;
    procedure TestStringReplaceAll_CS_LToEQ_W;
  published
    procedure TestBufferToStr;
    procedure TestFirstDelimiter;
    procedure TestLastDelimiter;
    procedure TestPutSplitStringEx;
    procedure TestStringReplaceAll_CS_LToEQ;
    {$IFDEF ENABLE_POSTGRESQL}
    procedure TestIsIpAddr;
    {$ENDIF}
    procedure TestSqlStrToFloatDef;
    procedure TestStrToBoolEx;
    procedure TestObjectComparison;
    procedure TestReplaceChar;
    procedure TestRemoveChar;
    procedure TestAppendSepString;
    procedure TestBreakString;
    procedure TestMatch;
    procedure TestQuotedStr;
    procedure TestQuotedStr2;
    procedure TestDequotedStr;
  {$IFDEF BENCHMARK}
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    procedure TestAnsiToUnicodePerformance;
    procedure TestUTF8ToUnicodePerformance;
    {$IFEND}
    procedure TestIntToRaw_VS_IntToStr;
    procedure TestIntToUnicode_VS_IntToStr;
    procedure TestInt64ToRaw_VS_IntToStr;
    procedure TestInt64ToUnicode_VS_IntToStr;
    {$IFDEF USE_FAST_TRUNC}
    procedure TestTrunc;
    {$ENDIF USE_FAST_TRUNC}
    {$IFDEF WITH_UNICODEFROMLOCALECHARS}
    procedure TestUnicodeFromLocalChars;
    procedure TestLocalCharsFromUnicode;
    {$ENDIF}
    procedure TestRawToInt;
    procedure TestRawToIntDef;
    procedure TestRawToInt64Def;
    procedure TestRawToUInt64Def;
    procedure TestUnicodeToInt;
    procedure TestUnicodeToIntDef;
    procedure TestUnicodeToInt64Def;
    procedure TestRawToFloat;
    procedure TestUnicodeToFloat;
    procedure BenchTestDateTimeToRawSQLDate;
    procedure BenchTestDateTimeToUnicodeSQLDate;
    procedure BenchTestDateTimeToRawSQLTime;
    procedure BenchTestDateTimeToUnicodeSQLTime;
    procedure BenchTestDateTimeToRawSQLTimeStamp;
    procedure BenchTestDateTimeToUnicodeSQLTimeStamp;
    procedure BenchBinToHexUnicode;
    procedure BenchBinToHexRaw;
  {$ENDIF}
  end;

implementation

uses ZEncoding {$IFDEF BENCHMARK},ZFastCode, Types, Classes{$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}, Windows{$IFEND}{$ENDIF};

{ TZTestSysUtilsCase }

{**
  Runs a test for BufferToStr function.
}
procedure TZTestSysUtilsCase.TestBufferToStr;
var
  Value: PChar;
begin
  Value := nil;
  CheckEquals('', BufferToStr(Value, 1000));
end;

{**
  Runs a test for first delimiter function.
}
procedure TZTestSysUtilsCase.TestFirstDelimiter;
var
  SourceStr: string;
  DelimiterStr: string;
begin
  { Position should exist }
  DelimiterStr := '098g';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(7, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 1');

  { Position should not exist }
  DelimiterStr := 'klmn';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 2');

  { Check with empty string }
  DelimiterStr := '';
  SourceStr := '';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 3');
end;

procedure TZTestSysUtilsCase.TestLastDelimiter;
var
  SourceStr: string;
  DelimiterStr: string;
begin
  { Position should exist }
  DelimiterStr := '098g';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(17, LastDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 1');

  { Position should not exist }
  DelimiterStr := 'klmn';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 2');

  { Check with empty string }
  DelimiterStr := '';
  SourceStr := '';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 3');

  { Position should exist }
  DelimiterStr := 'gac';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(7, LastDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 4');
end;

procedure TZTestSysUtilsCase.TestPutSplitStringEx;
var SL: TStrings;
  SourceStr: string;
  DelimiterStr: string;
  I: Integer;
begin
  SL := TStringList.Create;
  Check(SL <> nil);
  try
    SourceStr := 'aaaggaaaggaaaggaaagg';
    DelimiterStr := 'gg';
    PutSplitStringEx(SL, SourceStr, DelimiterStr);
    CheckEquals(4, SL.Count, 'split count 1');
    for I := 0 to SL.Count -1 do
      CheckEquals('aaa', SL[i], '1. Splitted String');
    SourceStr := 'aaaggaaaggaaaggaaa';
    DelimiterStr := 'gg';
    PutSplitStringEx(SL, SourceStr, DelimiterStr);
    CheckEquals(4, SL.Count, 'split count 2');
    for I := 0 to SL.Count -1 do
      CheckEquals('aaa', SL[i], '2. Splitted String');
    DelimiterStr := 'aaa';
    PutSplitStringEx(SL, SourceStr, DelimiterStr);
    CheckEquals(3, SL.Count, 'split count 3');
    for I := 0 to SL.Count -1 do
      CheckEquals('gg', SL[i], '2. Splitted String');
  finally
    SL.Free;
  end;
end;

procedure TZTestSysUtilsCase.TestStringReplaceAll_CS_LToEQ;
begin
  TestStringReplaceAll_CS_LToEQ_A;
  TestStringReplaceAll_CS_LToEQ_W;
end;

procedure TZTestSysUtilsCase.TestStringReplaceAll_CS_LToEQ_A;
var SourceStr, Res: RawBytestring;
begin
  SourceStr := 'aaaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('aa'), RawByteString(''));
  CheckEquals(RawByteString('a'), Res);
  SourceStr := 'aaaggaaaggaaaggaaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString('aa'));
  CheckEquals(RawByteString('aaaaaaaaaaaaaaaaaaaa'), Res);
  SourceStr := 'aaaggaaaggaaaggaaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString('y'));
  CheckEquals(RawByteString('aaayaaayaaayaaay'), Res);
  SourceStr := 'aaaggaaaggaaaggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString('y'));
  CheckEquals(RawByteString('aaayaaayaaayaaa'), Res);
  SourceStr := 'aaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString('N'));
  CheckEquals(RawByteString('aaaN'), Res);
  SourceStr := 'ggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString('N'));
  CheckEquals(RawByteString('Naaa'), Res);
  SourceStr := 'ggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString(''));
  CheckEquals(RawByteString('aaa'), Res);
  SourceStr := 'aaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('gg'), RawByteString(''));
  CheckEquals(RawByteString('aaa'), Res);
  SourceStr := 'aaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, RawByteString('aa'), RawByteString(''));
  CheckEquals(RawByteString(''), Res);
end;

procedure TZTestSysUtilsCase.TestStringReplaceAll_CS_LToEQ_W;
var SourceStr, Res: ZWideString;
begin
  SourceStr := 'aaaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('aa'), ZWideString(''));
  CheckEquals(ZWideString('a'), Res);
  SourceStr := 'aaaggaaaggaaaggaaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString('aa'));
  CheckEquals(ZWideString('aaaaaaaaaaaaaaaaaaaa'), Res);
  SourceStr := 'aaaggaaaggaaaggaaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString('y'));
  CheckEquals(ZWideString('aaayaaayaaayaaay'), Res);
  SourceStr := 'aaaggaaaggaaaggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString('y'));
  CheckEquals(ZWideString('aaayaaayaaayaaa'), Res);
  SourceStr := 'aaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString('N'));
  CheckEquals(ZWideString('aaaN'), Res);
  SourceStr := 'ggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString('N'));
  CheckEquals(ZWideString('Naaa'), Res);
  SourceStr := 'ggaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString(''));
  CheckEquals(ZWideString('aaa'), Res);
  SourceStr := 'aaagg';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('gg'), ZWideString(''));
  CheckEquals(ZWideString('aaa'), Res);
  SourceStr := 'aaaa';
  Res := StringReplaceAll_CS_LToEQ(SourceStr, ZWideString('aa'), ZWideString(''));
  CheckEquals(ZWideString(''), Res);
end;

{$IFDEF ENABLE_POSTGRESQL}
{**
  Runs a test for IsIpAddr function.
}
procedure TZTestSysUtilsCase.TestIsIpAddr;
var
  IP: string;
begin
 // Correct IP
 IP := '10.0.5.15';
 Check(IsIpAddr(IP), 'ip is 10.0.5.15');

 // Correct IP
 IP := '150.150.14.33';
 Check(IsIpAddr(IP), 'ip is 150.150.14.33');

 // Incorrect IP
 IP := '1001.2220.775.1544';
 Check(not IsIpAddr(IP), 'ip is 1001.2220.775.1544');

 // Incorrect IP
 IP := '10.0.5.15:3306';
 Check(not IsIpAddr(IP), 'ip is 10.0.5.15:3306');

 // Incorrect IP
 IP := '999.999.999.999';
 Check(not IsIpAddr(IP), 'ip is 999.999.999.999');

 // Incorrect IP
 IP := 'localhost';
 Check(not IsIpAddr(IP), 'ip is 10.0.5.15');

 // Incorrect IP - clear string
 IP := '';
 Check(not IsIpAddr(IP), 'ip is ''''');

 // Incorrect IP
 IP := 'abcd.ddns.com.br';
 Check(not IsIpAddr(IP), 'abcd.ddns.com.br');
end;
{$ENDIF}

{**
  Runs a test for object and interface comparison.
}
procedure TZTestSysUtilsCase.TestObjectComparison;
var
  Value1: TZAnyValue;
  Value2: IZAnyValue;
begin
  Value1 := TZAnyValue.CreateWithInteger(1);
  Value2 := TZAnyValue.CreateWithInteger(1);

  try
    Check(Value1 = Value1);
    Check(IZInterface(Value1) <> Value2);

    Check(Pointer(Value1) = Pointer(Value1));
    Check(IZInterface(Value1) = IZInterface(Value1));
    Check(Pointer(Value1) <> Pointer(Value2));
{
    Check((Value1 as IZInterface) = (Value1 as IZInterface));
    Check((Value1 as IZInterface) <> (Value2 as IZInterface));
}
  finally
    Value1.Free;
  end;
end;

{**
  Runs a test for ReplaceChar function.
}
procedure TZTestSysUtilsCase.TestReplaceChar;
const
  Source = '1234,7567,567';
  SourceChar = ',';
  Target = '1234.7567.567';
  TargetChar = '.';
begin
  CheckEquals(Target, ReplaceChar(SourceChar, TargetChar, Source));
end;

{**
  Runs a test for RemoveChar function.
}
procedure TZTestSysUtilsCase.TestRemoveChar;
begin
  CheckEquals('123456', RemoveChar('.', '.123.456.'));
  CheckEquals('123456', RemoveChar('.', '123456'));
  CheckEquals('', RemoveChar('.', ''));
  CheckEquals('', RemoveChar('.', '...'));
end;

{**
  Runs a test for AppendSepString function.
}
procedure TZTestSysUtilsCase.TestAppendSepString;
var
  Str: string;
begin
  Str := '';
  AppendSepString(Str, 'foo', ';');
  CheckEquals(Str, 'foo');
  AppendSepString(Str, 'bar', ';');
  CheckEquals(Str, 'foo;bar');
  AppendSepString(Str, '', ';');
  CheckEquals(Str, 'foo;bar');
end;


{**
  Runs a test for BreakString function.
}
procedure TZTestSysUtilsCase.TestBreakString;

var
  Delim, S, S1: string;

  procedure CheckBreakString(const Str, ExpLeft, ExpRight: string);
  var
    Left, Right: string;
  begin
    BreakString(Str, Delim, Left, Right);
    CheckEquals(ExpLeft, Left);
    CheckEquals(ExpRight, Right);
  end;

begin
  Delim := '=';
  CheckBreakString('', '', '');
  CheckBreakString(Delim, '', '');
  CheckBreakString('aa', 'aa', '');
  CheckBreakString('aa'+Delim, 'aa', '');
  CheckBreakString('aa'+Delim+'bb', 'aa', 'bb');
  CheckBreakString('aa'+Delim+'bb'+Delim, 'aa', 'bb'+Delim);
  CheckBreakString('aa'+Delim+Delim+'bb', 'aa', Delim+'bb');

  Delim := '==';
  CheckBreakString('aa'+Delim+'bb', 'aa', 'bb');

  S := 'aa'+Delim+'bb';
  BreakString(S, Delim, S1, S);
  CheckEquals('aa', S1);
  CheckEquals('bb', S);
  BreakString(S, Delim, S1, S);
  CheckEquals('bb', S1);
  CheckEquals('', S);
end;

{**
  Runs a test for StrToFloatDef function.
}
procedure TZTestSysUtilsCase.TestSqlStrToFloatDef;
begin
  CheckEquals(12.75, SqlStrToFloatDef(PAnsiChar('12,75'), 11.11));
  CheckEquals(12.75, SqlStrToFloatDef(PAnsiChar('12.75'), 11.11));
  CheckEquals(0.1275, SqlStrToFloatDef(PAnsiChar('12.75e-2'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('12.75float'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar(''), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('111,125.33'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef(PAnsiChar('$1.012,75'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef(PAnsiChar('А 1.012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('$1.0012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('А 1.0012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef(PAnsiChar('$1.012.012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef(PAnsiChar('А  1.012.012,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(PAnsiChar('$1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(PAnsiChar('А  1.012.012.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('$1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('А  1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('$1.012.0121.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(PAnsiChar('А  1.012.0121.111,75'), 11.11));
  CheckEquals(-1012.75, SqlStrToFloatDef(PAnsiChar('А -1.012,75'), 11.11));
  CheckEquals(-1012012111.75, SqlStrToFloatDef(PAnsiChar('$-1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(PAnsiChar('$+1.012.012.111,75'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef(PAnsiChar('А643,11'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef(PAnsiChar('643,11 А'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef(PAnsiChar('643,11 $'), 11.11));

  CheckEquals(12.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('12,75'), 11.11));
  CheckEquals(12.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('12.75'), 11.11));
  CheckEquals(0.1275, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('12.75e-2'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('12.75float'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}(''), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('111,125.33'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.012,75'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А 1.012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.0012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А 1.0012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.012.012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А  1.012.012,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А  1.012.012.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А  1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$1.012.0121.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А  1.012.0121.111,75'), 11.11));
  CheckEquals(-1012.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А -1.012,75'), 11.11));
  CheckEquals(-1012012111.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$-1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('$+1.012.012.111,75'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('А643,11'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('643,11 А'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef({$IFDEF PWIDECHAR_IS_PUNICODECHAR}PWideChar{$ELSE}ZWideString{$ENDIF}('643,11 $'), 11.11));
end;

{**
  Runs a test for StrToBoolEx function.
}
procedure TZTestSysUtilsCase.TestStrToBoolEx;
begin
  Check(StrToBoolEx(RawByteString('YES')));
  Check(StrToBoolEx(RawByteString('Yes')));
  Check(StrToBoolEx(RawByteString('Y')));
  Check(StrToBoolEx(RawByteString('TRUE')));
  Check(StrToBoolEx(RawByteString('True')));
  Check(StrToBoolEx(RawByteString('T')));
  CheckEquals(False, StrToBoolEx(RawByteString('FALSE')));
  CheckEquals(False, StrToBoolEx(RawByteString('False')));
  CheckEquals(False, StrToBoolEx(RawByteString('F')));
  CheckEquals(False, StrToBoolEx(RawByteString('NO')));
  CheckEquals(False, StrToBoolEx(RawByteString('No')));
  CheckEquals(False, StrToBoolEx(RawByteString('N')));

  Check(StrToBoolEx(ZWideString('YES')));
  Check(StrToBoolEx(ZWideString('Yes')));
  Check(StrToBoolEx(ZWideString('Y')));
  Check(StrToBoolEx(ZWideString('TRUE')));
  Check(StrToBoolEx(ZWideString('True')));
  Check(StrToBoolEx(ZWideString('T')));
  CheckEquals(False, StrToBoolEx(ZWideString('FALSE')));
  CheckEquals(False, StrToBoolEx(ZWideString('False')));
  CheckEquals(False, StrToBoolEx(ZWideString('F')));
  CheckEquals(False, StrToBoolEx(ZWideString('NO')));
  CheckEquals(False, StrToBoolEx(ZWideString('No')));
  CheckEquals(False, StrToBoolEx(ZWideString('N')));
end;

{**
  Runs a test for IsMatch function.
}
procedure TZTestSysUtilsCase.TestMatch;
begin
  CheckEquals(True, IsMatch('2*', '23'));
  CheckEquals(True, IsMatch('qwe*', 'qwerty'));
  CheckEquals(True, IsMatch('qwe*', 'qwe'));
  CheckEquals(False, IsMatch('qwe*', 'xyz'));
  CheckEquals(False, IsMatch('qwe*', 'xyzqweabc'));

  CheckEquals(True, IsMatch('*qwe', 'xyzqwe'));
  CheckEquals(True, IsMatch('*qwe', 'qwe'));
  CheckEquals(False, IsMatch('*qwe', 'xyz'));
  CheckEquals(False, IsMatch('*qwe', 'xyzqweabc'));

  CheckEquals(True, IsMatch('*qwe*', 'xyzqwe'));
  CheckEquals(True, IsMatch('*qwe*', 'qwe'));
  CheckEquals(False, IsMatch('*qwe*', 'xyz'));
  CheckEquals(True, IsMatch('*qwe*', 'xyzqweabc'));
end;

procedure TZTestSysUtilsCase.TestQuotedStr;
const
  // Quote; Unquoted; Quoted
  TestCases_SglQuote: array[0..2] of array[0..2] of string =
  (
    ('"', '', '""'),
//    ('"', '"', '""""'),
    ('"', 'no quote', '"no quote"'),
    ('"', 'single "quote', '"single ""quote"')
  );

  TestCases_DblQuote: array[0..7] of array[0..2] of string =
  (
    ('""', '', '""'),
    ('""', '"', '""""'),
    ('""', 'no quote', '"no quote"'),
    ('""', 'single "quote', '"single ""quote"'),
    ('[]', '', '[]'),
    ('[]', 'no quote', '[no quote]'),
    ('[]', 'single [quote', '[single [[quote]'),
    ('[]', 'double [1] quote', '[double [[1]] quote]')
  );

var i: Integer;
begin
  for i := Low(TestCases_SglQuote) to High(TestCases_SglQuote) do
    CheckEquals(TestCases_SglQuote[i][2], SQLQuotedStr(TestCases_SglQuote[i][1], TestCases_SglQuote[i][0][1]));
  for i := Low(TestCases_DblQuote) to High(TestCases_DblQuote) do
    CheckEquals(TestCases_DblQuote[i][2], SQLQuotedStr(TestCases_DblQuote[i][1], TestCases_DblQuote[i][0][1], TestCases_DblQuote[i][0][2]));
end;

// in the sybase driver this leads to an exception
// see SF#277
procedure TZTestSysUtilsCase.TestQuotedStr2;
var
  TableName: String;
  QuoteChar: Char;
begin
  QuoteChar := '''';
  TableName := 'TABLE';
  CheckEquals(QuotedStr(TableName), SQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
  //CheckEquals(QuotedStr(TableName), FailingSQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
  TableName := 'TA''BLE';
  CheckEquals(QuotedStr(TableName), SQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
  //CheckEquals(QuotedStr(TableName), FailingSQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
  TableName := '''TABLE''';
  CheckEquals(QuotedStr(TableName), SQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
  //CheckEquals(QuotedStr(TableName), FailingSQLQuotedStr(Pointer(TableName), Length(TableName), QuoteChar));
end;

procedure TZTestSysUtilsCase.RunDequotedStr;
begin
  case Length(FQuote) of
    1: SQLDequotedStr(FSrc, FQuote[1]);
    2: SQLDequotedStr(FSrc, FQuote[1], FQuote[2]);
    else Fail('Unexpected Quote length');
  end;
end;

procedure TZTestSysUtilsCase.TestDequotedStr;
const
  // Quote; Unquoted; Quoted
  TestCases_SglQuote: array[0..5] of array[0..2] of string =
  (
    ('"', '', '""'),
    ('"', '"', '""""'),
    ('"', 'no quote', 'no quote'),
    ('"', 'no "quote', 'no "quote'),
    ('"', 'no quote', '"no quote"'),
    ('"', 'single "quote', '"single ""quote"')
  );

  TestCases_DblQuote: array[0..9] of array[0..2] of string =
  (
    ('""', '', '""'),
    ('""', '"', '"'),
    ('""', 'no quote', 'no quote'),
    ('""', 'no "quote', 'no "quote'),
    ('""', 'no quote', '"no quote"'),
    ('""', 'single "quote', '"single ""quote"'),
    ('[]', '', '[]'),
    ('[]', 'no quote', '[no quote]'),
    ('[]', 'single [quote', '[single [[quote]'),
    ('[]', 'double [1] quote', '[double [[1]] quote]')
  );

  TestCases_WillRaise: array[0..3] of array[0..1] of string =
  (
    ('"',  '"single "quote"'),
    ('""', '"single "quote"'),
    ('[]', '[s]]'),
    ('[]', '[]]')
  );

var i: Integer;
begin
  for i := Low(TestCases_SglQuote) to High(TestCases_SglQuote) do
    CheckEquals(TestCases_SglQuote[i][1], SQLDequotedStr(TestCases_SglQuote[i][2], TestCases_SglQuote[i][0][1]));
  for i := Low(TestCases_DblQuote) to High(TestCases_DblQuote) do
    CheckEquals(TestCases_DblQuote[i][1], SQLDequotedStr(TestCases_DblQuote[i][2], TestCases_DblQuote[i][0][1], TestCases_DblQuote[i][0][2]));

  for i := Low(TestCases_WillRaise) to High(TestCases_WillRaise) do
  begin
    FQuote := TestCases_WillRaise[i][0];
    FSrc := TestCases_WillRaise[i][1];
    CheckException(RunDequotedStr, EArgumentException, '', 'Source: <'+FSrc+'>');
  end;
end;

{$IFDEF BENCHMARK}
{$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
procedure TZTestSysUtilsCase.TestAnsiToUnicodePerformance;
const CPArray: Array[0..42] of Word = (zCP_DOS437, zCP_DOS708, zCP_DOS720,
  zCP_DOS737, zCP_DOS775, zCP_DOS850, zCP_DOS852, zCP_DOS857, zCP_DOS858,
  zCP_DOS860, zCP_DOS861, zCP_DOS862, zCP_DOS863, zCP_DOS864, zCP_DOS865,
  zCP_DOS866, zCP_DOS869, zCP_WIN874, zCP_WIN1250, zCP_WIN1251, zCP_WIN1252,
  zCP_WIN1253, zCP_WIN1254, zCP_WIN1255, zCP_WIN1256, zCP_WIN1257, zCP_WIN1258,
  zCP_macintosh, zCP_x_mac_ce, {zCP_x_IA5_Swedish, }zCP_us_ascii, zCP_KOI8R,
  zCP_KOI8U, zCP_L1_ISO_8859_1, zCP_L2_ISO_8859_2, zCP_L3_ISO_8859_3,
  zCP_L4_ISO_8859_4, zCP_L5_ISO_8859_5, zCP_L6_ISO_8859_6, zCP_L7_ISO_8859_7,
  zCP_L8_ISO_8859_8, zCP_L5_ISO_8859_9, zCP_L7_ISO_8859_13, zCP_L9_ISO_8859_15);
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  RBS: RawByteString;
  Loop, J: Integer;
  I: Byte;
  P: PAnsiChar;

    procedure TestWinEncode(P: PAnsiChar; Len: NativeUInt; CP: Word; var Dest: ZWideString);
    begin
      Dest := '';
      SetLength(Dest, Len);
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      SetLength(Dest, UnicodeFromLocaleChars(CP, 0, P, Len, Pointer(Dest), Len));
      {$ELSE}
      SetLength(Dest, MultiByteToWideChar(CP, 0, P, Len, Pointer(Dest), Len)); //Convert Ansi to Wide with supported Chars
      {$ENDIF}
    end;

    procedure TestZEncode(P: PAnsiChar; Len: NativeUInt; CP: Word; var Result: ZWideString);
    var
      S: RawByteString;
      {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
      C: LongWord;
      Dest: PWideChar;
      wlen: LengthInt;
      {$IFEND}
    label A2U;
    begin
      if Len = 0 then
        Result := ''
      else
        case CP of
          zCP_NONE:
            case ZDetectUTF8Encoding(P, Len) of
              etUSASCII: Result := USASCII7ToUnicodeString(P, Len);
              etUTF8:
                begin
                  CP := zCP_UTF8;
                  goto A2U;
                end
              else
                if ZCompatibleCodePages(ZOSCodePage,zCP_UTF8) then
                begin
                  ZSetString(P, Len, S{%H-});
                  Result := ZWideString(S); //random success, we don't know ANY proper CP here
                end
                else
                begin
                  CP := ZOSCodePage; //still a random success here!
                  goto A2U;
                end;
            end;
          {$If (defined(FPC) and not defined(MSWINDOWS))}
          //FPC code is less optimal ... very slow. Wasn't able to get a asm code running
          //so i left the code for Non-Mindows OS's which will benefit here. No LIBICONV link is required.
          //let's see what comming FPC 2.7.1 is able to do (if they have it's own codepage maps)
          //Delphi7 needs 10% of time the FPC requires whereas the UnicodeIDE are 60% faster than D7
          //So i exclude this code for the Ansi-Delphi with WideString sh.. too.
          //here the code is fast as MultibyteToWideChar and we don't need this code
          zCP_DOS437:   AnsiSBCSToUCS2(P, Len, Result, @CP437ToUnicodeMap);
          zCP_DOS708:   AnsiSBCSToUCS2(P, Len, Result, @CP708ToUnicodeMap);
          zCP_DOS720:   AnsiSBCSToUCS2(P, Len, Result, @CP720ToUnicodeMap);
          zCP_DOS737:   AnsiSBCSToUCS2(P, Len, Result, @CP737ToUnicodeMap);
          zCP_DOS775:   AnsiSBCSToUCS2(P, Len, Result, @CP775ToUnicodeMap);
          zCP_DOS850:   AnsiSBCSToUCS2(P, Len, Result, @CP850ToUnicodeMap);
          zCP_DOS852:   AnsiSBCSToUCS2(P, Len, Result, @CP852ToUnicodeMap);
          zCP_DOS857:   AnsiSBCSToUCS2(P, Len, Result, @CP857ToUnicodeMap);
          zCP_DOS858:   AnsiSBCSToUCS2(P, Len, Result, @CP858ToUnicodeMap);
          zCP_DOS860:   AnsiSBCSToUCS2(P, Len, Result, @CP860ToUnicodeMap);
          zCP_DOS861:   AnsiSBCSToUCS2(P, Len, Result, @CP861ToUnicodeMap);
          zCP_DOS862:   AnsiSBCSToUCS2(P, Len, Result, @CP862ToUnicodeMap);
          zCP_DOS863:   AnsiSBCSToUCS2(P, Len, Result, @CP863ToUnicodeMap);
          zCP_DOS864:   AnsiSBCSToUCS2(P, Len, Result, @CP864ToUnicodeMap);
          zCP_DOS865:   AnsiSBCSToUCS2(P, Len, Result, @CP865ToUnicodeMap);
          zCP_DOS866:   AnsiSBCSToUCS2(P, Len, Result, @CP866ToUnicodeMap);
          zCP_DOS869:   AnsiSBCSToUCS2(P, Len, Result, @CP869ToUnicodeMap);
          zCP_WIN874:   AnsiSBCSToUCS2(P, Len, Result, @CP874ToUnicodeMap);
          zCP_WIN1250:  AnsiSBCSToUCS2(P, Len, Result, @CP1250ToUnicodeMap);
          zCP_WIN1251:  AnsiSBCSToUCS2(P, Len, Result, @CP1251ToUnicodeMap);
          zCP_WIN1252:  AnsiSBCSToUCS2(P, Len, Result, @CP1252ToUnicodeMap);
          zCP_WIN1253:  AnsiSBCSToUCS2(P, Len, Result, @CP1253ToUnicodeMap);
          zCP_WIN1254:  AnsiSBCSToUCS2(P, Len, Result, @CP1254ToUnicodeMap);
          zCP_WIN1255:  AnsiSBCSToUCS2(P, Len, Result, @CP1255ToUnicodeMap);
          cCP_WIN1256:  AnsiSBCSToUCS2(P, Len, Result, @CP1256ToUnicodeMap);
          zCP_WIN1257:  AnsiSBCSToUCS2(P, Len, Result, @CP1257ToUnicodeMap);
          zCP_WIN1258:  AnsiSBCSToUCS2(P, Len, Result, @CP1258ToUnicodeMap);
          zCP_macintosh: AnsiSBCSToUCS2(P, Len, Result, @CP10000ToUnicodeMap);
          zCP_x_mac_ce: AnsiSBCSToUCS2(P, Len, Result, @CP10029ToUnicodeMap);
          zCP_x_IA5_Swedish:  AnsiSBCSToUCS2(P, Len, Result, @CP20107ToUnicodeMap);
          zCP_KOI8R:  AnsiSBCSToUCS2(P, Len, Result, @CP20866ToUnicodeMap);
          zCP_us_ascii: AnsiSBCSToUCS2(P, Len, Result, @CP20127ToUnicodeMap);
          zCP_KOI8U:  AnsiSBCSToUCS2(P, Len, Result, @CP21866ToUnicodeMap);
          zCP_L1_ISO_8859_1: AnsiSBCSToUCS2(P, Len, MapByteToUCS2, Result);
          zCP_L2_ISO_8859_2:  AnsiSBCSToUCS2(P, Len, Result, @CP28592ToUnicodeMap);
          zCP_L3_ISO_8859_3:  AnsiSBCSToUCS2(P, Len, Result, @CP28593ToUnicodeMap);
          zCP_L4_ISO_8859_4:  AnsiSBCSToUCS2(P, Len, Result, @CP28594ToUnicodeMap);
          zCP_L5_ISO_8859_5:  AnsiSBCSToUCS2(P, Len, Result, @CP28595ToUnicodeMap);
          zCP_L6_ISO_8859_6:  AnsiSBCSToUCS2(P, Len, Result, @CP28596ToUnicodeMap);
          zCP_L7_ISO_8859_7:  AnsiSBCSToUCS2(P, Len, Result, @CP28597ToUnicodeMap);
          zCP_L8_ISO_8859_8:  AnsiSBCSToUCS2(P, Len, Result, @CP28598ToUnicodeMap);
          zCP_L5_ISO_8859_9:  AnsiSBCSToUCS2(P, Len, Result, @CP28599ToUnicodeMap);
          zCP_L7_ISO_8859_13:  AnsiSBCSToUCS2(P, Len, Result, @CP28603ToUnicodeMap);
          zCP_L9_ISO_8859_15:  AnsiSBCSToUCS2(P, Len, Result, @CP28605ToUnicodeMap);
          {$IFEND}
          (* remaing fast conversion for MBCS encodings
          zCP_MSWIN921 = 921;
          zCP_MSWIN923 = 923;
          zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
          zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
          zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
          zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}
          ZCP_JOHAB = 1361; {Korean (Johab)}
          zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}

          zCP_csISO2022JP = 50221;	{ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
          zCP_euc_JP_win = 51932; {EUC Japanese}
          zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
          zCP_euc_kr = 51949; {EUC Korean}
          zCP_GB18030 = 54936;	{Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
          zCP_UTF7 = 65000;
          *)
          {.$IFEND}
          zCP_UTF8: AnsiMBCSToUCS2(P, Len, UTF8ToWideChar, Result);
          {not supported codepages by Windows MultiByteToWideChar}
          zCP_L6_ISO_8859_10:  AnsiSBCSToUCS2(P, Len, Result, @CP28600ToUnicodeMap);
          zCP_L8_ISO_8859_14:  AnsiSBCSToUCS2(P, Len, Result, @CP28604ToUnicodeMap);
          zCP_L10_ISO_8859_16:  AnsiSBCSToUCS2(P, Len, Result, @CP28606ToUnicodeMap);
          else
      A2U:  {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
            begin
              wlen := Len;
              Result := ''; //speeds up SetLength x2
              SetLength(result, wlen);
              Dest := Pointer(Result);
              {first handle leading ASCII if possible }
              while (Len >= 4) and (PLongWord(P)^ and $80808080 = 0) do
              begin
                C := PLongWord(P)^;
                dec(Len,4);
                inc(P,4);
                PLongWord(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
                c := c shr 16;
                PLongWord(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
                inc(Dest,4);
              end;
              while (Len > 0) and (PByte(P)^ and $80 = 0) do
              begin
                dec(Len);
                PWord(Dest)^ := Byte(P^); //Shift Byte to Word
                inc(P);
                inc(Dest);
              end;
              if Len > 0 then //convert remaining characters with codepage agnostic
              begin
                wlen :=  wlen-LengthInt(Len); //elimainate already processed chars
                {$IFDEF WITH_UNICODEFROMLOCALECHARS}
                wlen := wlen + UnicodeFromLocaleChars(CP, 0, P, Len, Dest, Len);
                {$ELSE}
                Wlen := wlen + MultiByteToWideChar(CP, 0, P, Len, Dest, Len); //Convert Ansi to Wide with supported Chars
                {$ENDIF}
                SetLength(Result, Wlen); //return with expected length
              end;
            end;
            {$ELSE}
              {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
              WidestringManager.Ansi2UnicodeMoveProc(P, CP, Result, Len);
              {$ELSE}
              begin
                ZSetString(P, Len, S);
                if ZCompatibleCodePages(CP, zCP_UTF8) then
                  Result := UTF8Decode(S)
                else
                  Result := ZWideString(S); //random success
              end;
              {$ENDIF}
            {$IFEND}
        end;
      end;
begin
  SetLength(RBS, High(Byte));
  P := Pointer(RBS);
  for i := Low(Byte) to High(Byte)  do
    PByte(P+i)^ := i;
  RBS := RBS+RBS+RBS+RBS;
  for J := Low(CPArray) to High(CPArray) do
  begin
    Start := GetTickCount;
    for Loop := 0 to 5000000 do
      TestWinEncode(P, High(Byte), CPArray[j], S1);
    Stop := GetTickCount;
    Between1 := Stop - Start;
    Start := GetTickCount;
    for Loop := 0 to 5000000 do
      TestZEncode(P, High(Byte), CPArray[j], S2);
    Stop := GetTickCount;
    Between2 := Stop - Start;

    system.WriteLn('');
    system.WriteLn('Benchmarking(x 5.000.000): Ansi(CP '+IntToStr(CPArray[j])+') to Unicode');
    system.WriteLn(Format('TestWinEncode: %d ms VS. TestZEncode: %d ms', [Between1, Between2]));
    for i := low(Byte) + 1 to high(Byte) do
      if Word(S1[I]) = Word(S2[I]) then
        Check(True)
      else
        Check(True, 'Byte: '+IntToHex(I, 2)+' Expected('+IntToHex(Word(S1[I+1]), 4)+') was ('+IntToHex(Word(S2[I+1]), 4)+'(');
    CheckEquals(s1, s2, 'CP: '+IntToStr(CPArray[j])+' Results of WinEncoding VS. ZeosEncoding');
  end;
end;

procedure TZTestSysUtilsCase.TestUTF8ToUnicodePerformance;
const
  cS1 = ZWideString('Mьller, Maier, Dцdelц');
  cS2 = ZWideString('Muhende Mдддh''s');
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  RBS: RawByteString;
  P: PAnsiChar;
  Len: NativeUInt;
  Loop: Integer;
  I: Byte;

    procedure TestWinEncode(P: PAnsichar; Len: NativeUInt; CP: Word; var Dest: ZWideString);
    begin
      Dest := '';
      SetLength(Dest, Len);
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      SetLength(Dest, UnicodeFromLocaleChars(CP, 0, P, Len, Pointer(Dest), Len));
      {$ELSE}
      SetLength(Dest, MultiByteToWideChar(CP, 0, P, Len, Pointer(Dest), Len)); //Convert Ansi to Wide with supported Chars
      {$ENDIF}
    end;

    procedure TestZEncode(P: PAnsichar; Len: NativeUInt; CP: Word; var Dest: ZWideString);
    begin
      Dest := '';
      SetLength(Dest, Len);
      SetLength(Dest, UTF8ToWideChar(P, Len, Pointer(Dest)));
    end;
begin
  RBS := UTF8Encode(cS1);
  P := Pointer(RBS);
  Len := Length(RBS);
  Start := GetTickCount;
  for Loop := 0 to 10000000 do
    TestWinEncode(P, Len, 65001, S1);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  RBS := UTF8Encode(cS2);
  P := Pointer(RBS);
  Len := Length(RBS);
  Start := GetTickCount;
  for Loop := 0 to 10000000 do
    TestWinEncode(P, Len, 65001, S1);
  Stop := GetTickCount;
  Between1 := Between1+(Stop - Start);

  RBS := UTF8Encode(cS1);
  P := Pointer(RBS);
  Len := Length(RBS);
  Start := GetTickCount;
  for Loop := 0 to 10000000 do
    TestZEncode(P, Len, 65001, S2);
  Stop := GetTickCount;
  Between2 := Stop - Start;
  RBS := UTF8Encode(cS2);
  P := Pointer(RBS);
  Len := Length(RBS);
  Start := GetTickCount;
  for Loop := 0 to 10000000 do
    TestZEncode(P, Len, 65001, S2);
  Stop := GetTickCount;
  Between2 := Between2+(Stop - Start);

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 20.000.000): Ansi(CP UTF8 to Unicode');
  system.WriteLn(Format('TestWinEncode: %d ms VS. TestZEncode: %d ms', [Between1, Between2]));
  for i := 1 to Length(S1) do
    if Word(S1[I]) = Word(S2[I]) then
      Check(True)
    else
      Check(True, 'Pos: '+IntToHex(I, 2)+' Expected('+IntToHex(Word(S1[I]), 4)+') was ('+IntToHex(Word(S2[I]), 4)+')');
  CheckEquals(s1, s2, 'CP: UTF8 Results of WinEncoding VS. ZeosEncoding');
end;
{$IFEND}

procedure TZTestSysUtilsCase.TestIntToRaw_VS_IntToStr;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;

    function TestIntToRaw: RawByteString;
    var
      I: Integer;
    begin
      {$IFDEF HUGE_BENCHMARK}
      for i := Low(Integer) to High(Integer) do
      {$ELSE}
      for i := -1 to 10000000 do
      {$ENDIF}
        if I < 0 then
          if I < Low(ShortInt) then
            if i < Low(SmallInt) then
              Result := IntToRaw(I)
            else
              Result := IntToRaw(SmallInt(I))
          else
            Result := IntToRaw(ShortInt(I))
        else
          if I > High(Byte) then
            if I > High(Word) then
              Result := IntToRaw(I)
            else
              Result := IntToRaw(Word(I))
          else
            Result := IntToRaw(Byte(I))
    end;

    function TestIntToString: RawByteString;
    var
      I: Integer;
    begin
      {$IFDEF HUGE_BENCHMARK}
      for i := Low(Integer) to High(Integer) do
      {$ELSE}
      for i := -1 to 10000000 do
      {$ENDIF}
        if I < 0 then
          if I < Low(ShortInt) then
            if i < Low(SmallInt) then
              Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(I))
            else
              Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(SmallInt(I)))
          else
            Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(ShortInt(I)))
        else
          if I > High(Byte) then
            if I > High(Word) then
              Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(I))
            else
              Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(Word(I)))
          else
            Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SysUtils.IntToStr(Byte(I)));
    end;
begin
  Start := GetTickCount;
  S1 := TestIntToRaw;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestIntToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of IntToRaw VS. IntToStr');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Integer to RawByteString');
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.IntToStr+NotEmptyStringToASCII7: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestIntToUnicode_VS_IntToStr;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;

    function TestIntToRaw: ZWideString;
    var
      I: Integer;
    begin
      for i := -1 to 10000000 do
        Result := IntToUnicode(i);
    end;

    function TestIntToString: ZWideString;
    var
      I: Integer;
    begin
      for i := -1 to 10000000 do
        Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(SysUtils.IntToStr(i));
    end;
begin
  Start := GetTickCount;
  S1 := TestIntToRaw;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestIntToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of IntToUnicode VS. IntToStr');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Integer to UnicodeString');
  system.WriteLn(Format('Zeos: %d ms VS. ZWideString(SysUtils.IntToStr): %d ms', [Between1, Between2]));

end;

{$ifopt Q+}
  {$define OverflowCheckEnabled}
  {$Q-}
{$endif}
{$ifopt R+}
  {$define RangeCheckEnabled}
  {$R-}
{$endif}
procedure TZTestSysUtilsCase.TestInt64ToRaw_VS_IntToStr;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;

    function TestIntToRaw: RawByteString;
    var
      I: Integer;
      I64: Int64;
    begin
      for i := 0 to 10000000 do
      begin
        I64 := i * i;
        Result := IntToRaw(I64);
      end;
      Result := IntToRaw(High(Int64));
    end;

    function TestIntToString: RawByteString;
    var
      I: Integer;
      I64: Int64;
    begin
      for i := 0 to 10000000 do
      begin
        I64 := i * i;
        Result := {$ifdef unicode}UnicodeStringToASCII7{$endif}(SysUtils.IntToStr(I64));
      end;
      Result := {$ifdef unicode}UnicodeStringToASCII7{$endif}(SysUtils.IntToStr(High(Int64)));
    end;
begin
  Start := GetTickCount;
  S1 := TestIntToRaw;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestIntToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of IntToRaw VS. IntToStr');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Int64 to RawByteString');
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.IntToStr+{$ifdef unicode}UnicodeStringToASCII7{$endif}: %d ms', [Between1, Between2]));

end;
{$ifdef OverflowCheckEnabled}
  {$Q+}
{$endif}
{$ifdef RangeCheckEnabled}
  {$R+}
{$endif}

procedure TZTestSysUtilsCase.TestInt64ToUnicode_VS_IntToStr;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;

    function TestIntToRaw: ZWideString;
    var
      I: Integer;
      I64: Int64;
    begin
      for i := -10 to 10000000 do
      begin
        I64 := Int64(i) * Int64(i);
        Result := IntToUnicode(I64);
      end;
      Result := IntToUnicode(High(Int64)-1);
    end;

    function TestIntToString: ZWideString;
    var
      I: Integer;
      I64: Int64;
    begin
      for i := -10 to 10000000 do
      begin
        I64 := Int64(i) * Int64(i);
        Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(SysUtils.IntToStr(I64));
      end;
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(SysUtils.IntToStr(High(Int64)-1));
    end;
begin
  Start := GetTickCount;
  S1 := TestIntToRaw;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestIntToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of IntToRaw VS. IntToStr');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Int64 to UnicodeString');
  system.WriteLn(Format('Zeos: %d ms VS. ZWideString(SysUtils.IntToStr): %d ms', [Between1, Between2]));

end;

{$IFDEF USE_FAST_TRUNC}
procedure TZTestSysUtilsCase.TestTrunc;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Int64;

    function TestFastCodeTrunc: Int64;
    var
      I: Integer;
    begin
      for i := 1 to 10000000 do
        Result := ZFastCode.Trunc(1000.5/I);
    end;

    function TestSystemTrunc: Int64;
    var
      I: Integer;
    begin
      for i := 1 to 10000000 do
        Result := System.Trunc(1000.5/I);
    end;
begin
  Start := GetTickCount;
  S1 := TestFastCodeTrunc;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestSystemTrunc;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of ZFastCode.Trunc VS. System.Trunc');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Trunc');
  system.WriteLn(Format('Zeos: %d ms VS. System.Trunc: %d ms', [Between1, Between2]));
end;
{$ENDIF USE_FAST_TRUNC}

{$IFDEF WITH_UNICODEFROMLOCALECHARS}
procedure TZTestSysUtilsCase.TestLocalCharsFromUnicode;
const AnsiTestString: TBytes = 'ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...';
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;
  TestString: ZWideString;

    function TestUTF8EncodeOversized: RawByteString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        wlen := Length(TestString);
        setlength(Result, wlen*6);
        ulen := LocaleCharsFromUnicode(65001, 0, PWideChar(TestString), wlen, PAnsiChar(Result), wlen*6, NIL, NIL);
        setlength(Result, ulen);
      end;
    end;

    function TestUTF8EncodeTestSize: RawByteString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        wlen := Length(TestString);
        ulen := LocaleCharsFromUnicode(65001, 0, PWideChar(TestString), wlen, NIL, 0, NIL, NIL);
        setlength(Result, ulen);
        LocaleCharsFromUnicode(65001, 0, PWideChar(TestString), wlen, PAnsiChar(Result), ulen, NIL, NIL);
      end;
    end;

    function Test1252EncodeOversized: RawByteString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        wlen := Length(TestString);
        setlength(Result, wlen*4);
        ulen := LocaleCharsFromUnicode(1252, 0, PWideChar(TestString), wlen, PAnsiChar(Result), wlen*4, NIL, NIL);
        setlength(Result, ulen);
      end;
    end;

    function Test1252EncodeTestSize: RawByteString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        wlen := Length(TestString);
        ulen := LocaleCharsFromUnicode(1252, 0, PWideChar(TestString), wlen, NIL, 0, NIL, NIL);
        setlength(Result, ulen);
        LocaleCharsFromUnicode(1252, 0, PWideChar(TestString), wlen, PAnsiChar(Result), ulen, NIL, NIL);
      end;
    end;
begin
  Between1 := UnicodeFromLocaleChars(1251, 0, AnsiTestString, length(AnsiTestString), nil, 0);
  SetLength(TestString, Between1);
  Between1 := UnicodeFromLocaleChars(1251, 0, AnsiTestString, length(AnsiTestString), TestString, Length(TestString));
  if Between1 = 0 then RaiseLastOsError;

  Start := GetTickCount;
  S1 := TestUTF8EncodeOversized;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestUTF8EncodeTestSize;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of oversized VS. tested size');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Unicode to CPUTF8 RawByteString');
  system.WriteLn(Format('TestUTF8EncodeOversized: %d ms VS. TestUTF8EncodeTestSize: %d ms', [Between1, Between2]));

  Start := GetTickCount;
  S1 := Test1252EncodeOversized;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := Test1252EncodeTestSize;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of oversized VS. tested size');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Unicode to CP1252 RawByteString');
  system.WriteLn(Format('TestUTF8EncodeOversized: %d ms VS. TestUTF8EncodeTestSize: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestUnicodeFromLocalChars;
const AnsiTestString: TBytes = 'ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...';
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  RBS: RawByteString;
  TestString: ZWideString;

    function TestUTF8EncodeOversized: ZWideString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Ulen := Length(RBS);
        Wlen := Ulen * 2;
        SetLength(Result, Wlen);
        wlen := UnicodeFromLocaleChars(65001, 0, PAnsiChar(RBS), ulen, PWideChar(Result), Wlen); // wlen is the number of UCS2 without NULL terminater.
        if wlen = 0 then exit;
        SetLength(result, wlen);
      end;
    end;

    function TestUTF8EncodeTestSize: ZWideString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Ulen := Length(RBS);
        wlen := UnicodeFromLocaleChars(65001, 0, PAnsiChar(RBS), ulen, NIL, 0); // wlen is the number of UCS2 without NULL terminater.
        if wlen = 0 then exit;
        SetLength(result, wlen);
        UnicodeFromLocaleChars(65001, 0, PAnsiChar(RBS), ulen, PWideChar(Result), wlen);
      end;
    end;

    function Test1252EncodeOversized: ZWideString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Ulen := Length(RBS);
        Wlen := Ulen * 2;
        SetLength(Result, Wlen);
        wlen := UnicodeFromLocaleChars(1252, 1, PAnsiChar(RBS), ulen, PWideChar(Result), Wlen); // wlen is the number of UCS2 without NULL terminater.
        SetLength(result, wlen);
      end;
    end;

    function Test1252EncodeTestSize: ZWideString;
    var i, wlen, ulen: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Ulen := Length(RBS);
        wlen := UnicodeFromLocaleChars(1252, 1, PAnsiChar(RBS), ulen, NIL, 0); // wlen is the number of UCS2 without NULL terminater.
        if wlen = 0 then exit;
        SetLength(result, wlen);
        UnicodeFromLocaleChars(1252, 1, PAnsiChar(RBS), ulen, PWideChar(Result), wlen);
      end;
    end;
begin
  Between1 := UnicodeFromLocaleChars(1251, 0, AnsiTestString, length(AnsiTestString), nil, 0);
  SetLength(TestString, Between1);
  Between1 := UnicodeFromLocaleChars(1251, 0, AnsiTestString, length(AnsiTestString), TestString, Length(TestString));
  if Between1 = 0 then RaiseLastOsError;

  RBS := UTF8Encode(TestString);
  Start := GetTickCount;
  S1 := TestUTF8EncodeOversized;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TestUTF8EncodeTestSize;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of oversized VS. tested size');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Unicode to CPUTF8 RawByteString');
  system.WriteLn(Format('TestUTF8EncodeOversized: %d ms VS. TestUTF8EncodeTestSize: %d ms', [Between1, Between2]));

  RBS := AnsiString(TestString);
  Start := GetTickCount;
  S1 := Test1252EncodeOversized;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := Test1252EncodeTestSize;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of oversized VS. tested size');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): Unicode to CP1252 RawByteString');
  system.WriteLn(Format('TestWideToAnsiOversized: %d ms VS. TestWideToTestSize: %d ms', [Between1, Between2]));

end;
{$ENDIF}

procedure TZTestSysUtilsCase.TestRawToInt;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Integer;

    function TRawToInt: Integer;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := RawToInt(IntToRaw(i));
    end;

    function TStrToInt: Integer;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := SysUtils.StrToInt(SysUtils.IntToStr(i));
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToInt VS. StrToInt');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): RawToInt', [S1]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToInt: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestRawToIntDef;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Integer;

    function TRawToInt: Integer;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := RawToIntDef(IntToRaw(i), 1);
      Result := Result + RawToIntDef('test', -999);
    end;

    function TStrToInt: Integer;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := StrToIntDef(SysUtils.IntToStr(i), 1);
      Result := Result + SysUtils.StrToIntDef('test', -999);
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToIntDef VS. StrToIntDef');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): RawToIntDef', [10000000]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToIntDef: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestRawToInt64Def;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Int64;

    function TRawToInt: Int64;
    var
      I: Integer;
    begin
      CheckEquals(Low(Int64), RawToInt64Def(IntToRaw(Low(Int64)), 0), 'Results of RawToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), RawToInt64Def(IntToRaw(High(Int64)), 0), 'Results of RawToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := RawToInt64Def(IntToRaw(Int64(i)*Int64(i)), 1);
      Result := Result + RawToInt64Def('test', -999);
    end;

    function TStrToInt: Int64;
    var
      I: Integer;
    begin
      {$IFDEF UNICODE}
      CheckEquals(Low(Int64), StrToInt64Def(UnicodeString(IntToRaw(Low(Int64))), 0), 'Results of RawToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(UnicodeString(IntToRaw(High(Int64))), 0), 'Results of RawToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(UnicodeString(IntToRaw(Int64(i)*Int64(i))), 1);
      Result := Result + StrToInt64Def('test', -999);
      {$ELSE}
      CheckEquals(Low(Int64), StrToInt64Def(SysUtils.IntToStr(Low(Int64)), 0), 'Results of RawToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(SysUtils.IntToStr(High(Int64)), 0), 'Results of RawToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(SysUtils.IntToStr(Int64(i)*Int64(i)), 1);
      Result := Result + StrToInt64Def('test', -999);
      {$ENDIF}
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToInt64Def VS. StrToInt64Def');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): RawToInt64Def', [10000000]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToIntDef: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestRawToUInt64Def;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Int64;

    function TRawToInt: UInt64;
    var
      I: Integer;
    begin
      CheckEquals(Low(UInt64), RawToUInt64Def(IntToRaw(Low(UInt64)), 0), 'Results of RawToUInt64Def VS. Low(UInt64)');
      CheckEquals(High(UInt64), RawToUInt64Def(IntToRaw(High(UInt64)), 0), 'Results of RawToUInt64Def VS. High(UInt64)');
      CheckEquals(0, RawToUInt64Def('18446744073709551616', 0), 'Results of RawToUInt64Def VS. High(UInt64)');
      for i := 0 to 10000000 do
        Result := RawToUInt64Def(IntToRaw(UInt64(i)*UInt64(i)), 1);
      Result := Result + RawToUInt64Def('test', 999);
    end;

    function TStrToInt: UInt64;
    var
      I: Integer;
    begin
      CheckEquals(Low(UInt64), StrToInt64Def(SysUtils.IntToStr(Low(UInt64)), 0), 'Results of RawToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(SysUtils.IntToStr(High(Int64)), 0), 'Results of RawToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(SysUtils.IntToStr(UInt64(i)*UInt64(i)), 1);
      Result := Result + StrToInt64Def('test', 999);
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToUInt64Def VS. StrToInt64Def');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): RawToUInt64Def', [10000000]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToIntDef: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestUnicodeToInt;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Integer;

    function TRawToInt: Integer;
    var
      I: Integer;
    begin
      for i := -20 to 10000000 do
        Result := UnicodeToInt(IntToUnicode(I));
    end;

    function TStrToInt: Integer;
    var
      I: Integer;
    begin
      for i := -20 to 10000000 do
        Result :=
          {$IFDEF UNICODE}
            SysUtils.StrToInt(SysUtils.IntToStr(I));
          {$ELSE}
            SysUtils.StrToInt(String(ZWideString(SysUtils.IntToStr(I))));
          {$ENDIF}
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of UnicodeToInt VS. StrToInt');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): UnicodeToInt', [10000000+20]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToInt: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestUnicodeToIntDef;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Integer;

    function TRawToInt: Integer;
    var
      I: Integer;
    begin
      for i := -20 to 10000000 do
        Result := UnicodeToIntDef(IntToUnicode(I),1);
      Result := Result + UnicodeToIntDef(ZWideString('test'), -999);
    end;

    function TStrToInt: Integer;
    var
      I: Integer;
    begin
      for i := -20 to 10000000 do
        Result :=
          {$IFDEF UNICODE}
            StrToIntDef(SysUtils.IntToStr(I),1);
          {$ELSE}
            StrToIntDef(String(ZWideString(SysUtils.IntToStr(I))), 1);
          {$ENDIF}
        {$IFDEF UNICODE}
          Result := Result +  StrToIntDef('test', -999);
        {$ELSE}
          Result := Result + StrToIntDef(String(ZWideString('test')), -999);
        {$ENDIF}
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of UnicodeToIntDef VS. StrToIntDef');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): UnicodeToIntDef', [10000000+20]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToIntDef: %d ms', [Between1, Between2]));

end;

{$ifopt Q+}
  {$define OverflowCheckEnabled}
  {$Q-}
{$endif}
{$ifopt R+}
  {$define RangeCheckEnabled}
  {$R-}
{$endif}
procedure TZTestSysUtilsCase.TestUnicodeToInt64Def;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Int64;

    function TRawToInt: Int64;
    var
      I: Integer;
    begin
      CheckEquals(Low(Int64), UnicodeToInt64Def(IntToUnicode(Low(Int64)), 0), 'Results of UnicodeToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), UnicodeToInt64Def(IntToUnicode(High(Int64)), 0), 'Results of UnicodeToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := UnicodeToInt64Def(IntToUnicode(Int64(i*i)), 1);
      Result := Result + UnicodeToInt64Def(ZWideString('test'), -999);
    end;

    function TStrToInt: Int64;
    var
      I: Integer;
    begin
      {$IFDEF UNICODE}
      CheckEquals(Low(Int64), StrToInt64Def(SysUtils.IntToStr(Low(Int64)), 0), 'Results of StrToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(SysUtils.IntToStr(High(Int64)), 0), 'Results of StrToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(SysUtils.IntToStr(Int64(i*i)), 1);
      Result := Result + StrToInt64Def('test', -999);
      {$ELSE}
      CheckEquals(Low(Int64), StrToInt64Def(String(ZWideString(SysUtils.IntToStr(Low(Int64)))), 0), 'Results of StrToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(String(ZWideString(SysUtils.IntToStr(High(Int64)))), 0), 'Results of StrToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(String(ZWideString(SysUtils.IntToStr(Int64(i*i)))), 1);
      Result := Result + StrToInt64Def('test', -999);
      {$ENDif}
    end;
begin
  Start := GetTickCount;
  S1 := TRawToInt;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToInt;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToInt64Def VS. StrToInt64Def');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): UnicodeToInt64Def', [10000000]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToIntDef: %d ms', [Between1, Between2]));

end;
{$ifdef OverflowCheckEnabled}
  {$Q+}
{$endif}
{$ifdef RangeCheckEnabled}
  {$R+}
{$endif}

procedure TZTestSysUtilsCase.TestRawToFloat;
const sTestFloat = RawByteString('9876543210.0123456789');
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Extended;

    function TRawToFloat: Extended;
    var
      I,N: Integer;
    begin
      for N := 0 to 1000000 do
      begin
        for i := 0 to 20 do
          Result := RawToFloat(PAnsiChar(sTestFloat)+I, '.');
        for i := 1 to 10 do
          Result := Result + RawToFloat(PAnsiChar(AnsiString(sTestFloat[i])), '.');
      end;
      Result := Result + RawToFloatDef('test', '.', -999);
    end;

    function TStrToFloat: Extended;
    var
      I,N: Integer;
    begin
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ELSE}SysUtils.{$ENDIF}DecimalSeparator := '.';
      for N := 0 to 1000000 do
      begin
        for i := 0 to 20 do
          Result := StrToFloat(String(PAnsiChar(sTestFloat)+I));
        for i := 1 to 10 do
          Result := Result + StrToFloat(String(PAnsiChar(AnsiString(sTestFloat[i]))));
      end;
      Result := Result + StrToFloatDef('test', -999);
    end;
begin
  Start := GetTickCount;
  S1 := TRawToFloat;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToFloat;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of RawToFloatDef VS. StrToFloatDef');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): RawToFloatDef', [1000000*20*10]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToFloatDef: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.TestUnicodeToFloat;
const sTestFloat = ZWideString('9876543210.0123456789');
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: Extended;

    function TRawToFloat: Extended;
    var
      I,N: Integer;
    begin
      for N := 0 to 1000000 do
      begin
        for i := 0 to 20 do
          Result := UnicodeToFloat(PWideChar(sTestFloat)+I, WideChar('.'));
        for i := 1 to 10 do
          Result := Result + UnicodeToFloat(PWideChar(ZWideString(sTestFloat[i])), WideChar('.'));
      end;
      Result := Result + UnicodeToFloatDef(@ZWideString('test')[1], WideChar('.'), -999);
    end;

    function TStrToFloat: Extended;
    var
      I,N: Integer;
    begin
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
      for N := 0 to 1000000 do
      begin
        for i := 0 to 20 do
          Result := StrToFloat(String(PWideChar(sTestFloat)+I));
        for i := 1 to 10 do
          Result := Result + StrToFloat(String(PWideChar(ZWideString(sTestFloat[i]))));
      end;
      Result := Result + StrToFloatDef('test', -999);
    end;
begin
  Start := GetTickCount;
  S1 := TRawToFloat;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TStrToFloat;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of UnicodeToFloat VS. StrToFloatDef');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): UnicodeToFloat', [1000000*(20+10)]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.StrToFloatDef: %d ms', [Between1, Between2]));

end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToRawSQLDate;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;
  ADate: TDateTime;

  function TDateTimeToRawSQLDate(const Value: TDateTime): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLDate(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLDate(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLDate(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLDate(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ADate := now;
  Start := GetTickCount;
  S1 := TDateTimeToRawSQLDate(ADate);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ADate, String(ConSettingsDummy.ReadFormatSettings.DateFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToRawSQLDate VS. RawbyteString(FormatDateTime())');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToRawSQLDate', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToUnicodeSQLDate;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  ADate: TDateTime;

  function TDateTimeToUnicodeSQLDate(const Value: TDateTime): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLDate(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLDate(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLDate(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLDate(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ADate := now;
  Start := GetTickCount;
  S1 := TDateTimeToUnicodeSQLDate(ADate);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ADate, String(ConSettingsDummy.ReadFormatSettings.DateFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToUnicodeSQLDate VS. FormatDateTime');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToUnicodeSQLDate', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToRawSQLTime;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;
  ATime: TDateTime;

  function TDateTimeToRawSQLTime(const Value: TDateTime): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTime(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTime(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTime(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTime(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ATime := now;
  Start := GetTickCount;
  S1 := TDateTimeToRawSQLTime(ATime);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ATime, String(ConSettingsDummy.ReadFormatSettings.TimeFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToRawSQLTime VS. RawbyteString(FormatDateTime())');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToRawSQLTime', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToUnicodeSQLTime;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  ATime: TDateTime;

  function TDateTimeToUnicodeSQLTime(const Value: TDateTime): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTime(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTime(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTime(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTime(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ATime := now;
  Start := GetTickCount;
  S1 := TDateTimeToUnicodeSQLTime(ATime);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ATime, String(ConSettingsDummy.ReadFormatSettings.TimeFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToUnicodeSQLTime VS. FormatDateTime');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToUnicodeSQLTime', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToRawSQLTimeStamp;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;
  ADateTime: TDateTime;

  function TDateTimeToRawSQLTimeStamp(const Value: TDateTime): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToRawSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): RawByteString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFDEF UNICODE}RawByteString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ADateTime := now;
  Start := GetTickCount;
  S1 := TDateTimeToRawSQLTimeStamp(ADateTime);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ADateTime, String(ConSettingsDummy.ReadFormatSettings.DateTimeFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToRawSQLTimeStamp VS. RawbyteString(FormatDateTime())');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToRawSQLTimeStamp', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchTestDateTimeToUnicodeSQLTimeStamp;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  ADateTime: TDateTime;

  function TDateTimeToUnicodeSQLTimeStamp(const Value: TDateTime): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, False);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, True);
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, False, 'suffix');
    for i := 0 to 2500000 do
      Result := DateTimeToUnicodeSQLTimeStamp(Value, ConSettingsDummy.ReadFormatSettings, True, 'suffix');
  end;

  function TFormatDateTime(const Value: TDateTime; const Format: String): ZWideString;
  var
    I: Integer;
  begin
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value));
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39);
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(FormatDateTime(Format, Value)+'suffix');
    for i := 0 to 2500000 do
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(#39+FormatDateTime(Format, Value)+#39'suffix');
  end;
begin
  ADateTime := now;
  Start := GetTickCount;
  S1 := TDateTimeToUnicodeSQLTimeStamp(ADateTime);
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TFormatDateTime(ADateTime, String(ConSettingsDummy.ReadFormatSettings.DateTimeFormat));
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of DateTimeToUnicodeSQLTime VS. FormatDateTime');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): DateTimeToUnicodeSQLTimeStamp', [2500000*4]));
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.FormatDateTime: %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchBinToHexUnicode;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  Bin: TByteDynArray;
  I, L: Cardinal;

  function TZBinToHex: ZWideString;
  var
    I: Integer;
  begin
    Result := '';
    SetLength(Result, L*2);
    for i := 0 to 1000000 do
      ZBinToHex(PAnsiChar(Pointer(Bin)), PWideChar(Pointer(Result)), L);
  end;

  function TBinToHex: ZWideString;
  var
    I: Integer;
    Tmp: RawByteString;
  begin
    Tmp := '';
    SetLength(Tmp, L*2);
    for i := 0 to 1000000 do
    begin
      BinToHex(PAnsiChar(Pointer(Bin)), PAnsiChar(Pointer(Tmp)), L);
      Result := ZWideString(Tmp);
    end;

  end;
begin
  L := 10000;
  SetLength(Bin, L);
  for i := 0 to L-1 do
    Bin[i] := Byte(Random(255));
  Start := GetTickCount;
  S1 := TZBinToHex;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TBinToHex;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of ZBinToHex VS. BinToHex(10000 Bytes)');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): ZBinToHex', [1000000]));
  system.WriteLn(Format('Zeos: %d ms VS. ZWideString(SysUtils.BinToHex): %d ms', [Between1, Between2]));
end;

procedure TZTestSysUtilsCase.BenchBinToHexRaw;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;
  Bin: TByteDynArray;
  I, L: Cardinal;

  function TZBinToHex: RawByteString;
  var
    I: Integer;
  begin
    Result := '';
    SetLength(Result, L*2);
    for i := 0 to 1000000 do
      ZBinToHex(PAnsiChar(Pointer(Bin)), PAnsiChar(Pointer(Result)), L);
  end;

  function TBinToHex: RawByteString;
  var
    I: Integer;
  begin
    Result := '';
    SetLength(Result, L*2);
    for i := 0 to 1000000 do
      BinToHex(PAnsiChar(Pointer(Bin)), PAnsiChar(Pointer(Result)), L);
  end;
begin
  L := 10000;
  SetLength(Bin, L);
  for i := 0 to L-1 do
    Bin[i] := Byte(Random(255));
  Start := GetTickCount;
  S1 := TZBinToHex;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := TBinToHex;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of ZBinToHex VS. BinToHex(10000 Bytes)');

  system.WriteLn('');
  system.WriteLn(Format('Benchmarking(x %d): ZBinToHex', [1000000]));
  system.WriteLn(Format('Zeos: %d ms VS. RawByteString(SysUtils.BinToHex): %d ms', [Between1, Between2]));
end;

{$ENDIF}

initialization
  RegisterTest('core',TZTestSysUtilsCase.Suite);
end.
