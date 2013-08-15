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

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZTestCase, ZSysUtils, ZClasses, ZVariant, ZMatchPattern, ZCompatibility;

type

  {** Implements a test case for Utilities. }
  TZTestSysUtilsCase = class(TZGenericTestCase)
  published
    procedure TestBufferToStr;
    procedure TestFirstDelimiter;
    procedure TestIsIpAddr;
    procedure TestSqlStrToFloatDef;
    procedure TestStrToBoolEx;
    procedure TestObjectComparison;
    procedure TestReplaceChar;
    procedure TestMatch;
    procedure TestRawSQLDateToDateTime;
    procedure TestRawSQLTimeToDateTime;
    procedure TestRawSQLTimeStampToDateTime;
    {$IFDEF BENCHMARK}
    procedure TestASCII7ToString_VS_RawByteToString;
    procedure TestIntToRaw_VS_IntToStr;
    procedure TestIntToUnicode_VS_IntToStr;
    procedure TestInt64ToRaw_VS_IntToStr;
    procedure TestInt64ToUnicode_VS_IntToStr;
    {$IFDEF WITH_UNICODEFROMLOCALECHARS}
    procedure TestUnicodeFromLocalChars;
    procedure TestLocalCharsFromUnicode;
    {$ENDIF}
    procedure TestRawToInt;
    procedure TestRawToIntDef;
    procedure TestRawToInt64Def;
    procedure TestUnicodeToInt;
    procedure TestUnicodeToIntDef;
    procedure TestUnicodeToInt64Def;
    procedure TestRawToFloat;
    {$ENDIF}
  end;

implementation

uses ZEncoding;

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
  Runs a test for StrToFloatDef function.
}
procedure TZTestSysUtilsCase.TestSqlStrToFloatDef;
begin
  CheckEquals(12.75, SqlStrToFloatDef(RawByteString('12,75'), 11.11));
  CheckEquals(12.75, SqlStrToFloatDef(RawByteString('12.75'), 11.11));
  CheckEquals(0.1275, SqlStrToFloatDef(RawByteString('12.75e-2'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('12.75float'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString(''), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('111,125.33'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef(RawByteString('$1.012,75'), 11.11));
  CheckEquals(1012.75, SqlStrToFloatDef(RawByteString('А 1.012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('$1.0012,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('А 1.0012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef(RawByteString('$1.012.012,75'), 11.11));
  CheckEquals(1012012.75, SqlStrToFloatDef(RawByteString('А  1.012.012,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(RawByteString('$1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(RawByteString('А  1.012.012.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('$1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('А  1.012.012.1119,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('$1.012.0121.111,75'), 11.11));
  CheckEquals(11.11, SqlStrToFloatDef(RawByteString('А  1.012.0121.111,75'), 11.11));
  CheckEquals(-1012.75, SqlStrToFloatDef(RawByteString('А -1.012,75'), 11.11));
  CheckEquals(-1012012111.75, SqlStrToFloatDef(RawByteString('$-1.012.012.111,75'), 11.11));
  CheckEquals(1012012111.75, SqlStrToFloatDef(RawByteString('$+1.012.012.111,75'), 11.11));
  CheckEquals(643.11, SqlStrToFloatDef(RawByteString('А643,11'), 11.11))
end;

{**
  Runs a test for StrToBoolEx function.
}
procedure TZTestSysUtilsCase.TestStrToBoolEx;
begin
  CheckEquals(True, StrToBoolEx('YES'));
  CheckEquals(True, StrToBoolEx('Yes'));
  CheckEquals(True, StrToBoolEx('Y'));
  CheckEquals(True, StrToBoolEx('TRUE'));
  CheckEquals(True, StrToBoolEx('True'));
  CheckEquals(True, StrToBoolEx('T'));
  CheckEquals(False, StrToBoolEx('FALSE'));
  CheckEquals(False, StrToBoolEx('False'));
  CheckEquals(False, StrToBoolEx('F'));
  CheckEquals(False, StrToBoolEx('NO'));
  CheckEquals(False, StrToBoolEx('No'));
  CheckEquals(False, StrToBoolEx('N'));
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

procedure TZTestSysUtilsCase.TestRawSQLDateToDateTime;
const
  Date1_0 = RawByteString('1999-12-31');
  Date2_0 = RawByteString('1999/12/31');
  Date3_0 = RawByteString('1999\12\31');
  Date4_0 = RawByteString('1999-12-31');
  Date5_0 = RawByteString('19991231');

  Date1_1 = RawByteString('99-12-31');
  Date2_1 = RawByteString('99/12/31');
  Date3_1 = RawByteString('99\12\31');
  Date4_1 = RawByteString('99-12-31');
  Date5_1 = RawByteString('991231');

  Date1_2 = RawByteString('199a-12-31');
  Date2_2 = RawByteString('1999/1a/31');
  Date3_2 = RawByteString('1999\12\3a');
  Date4_2 = RawByteString('0000-00-00');
  Date5_2 = RawByteString('00000000');

  Date1_3 = RawByteString('36525');
  Date2_3 = RawByteString('36525.0');
  Date3_3 = RawByteString('36525a');
  Date4_3 = RawByteString('36525.0a');

  procedure TestRawSQLDateToDateTime(Value: RawByteString;
    const Expected: TDateTime; const ExpFailed: Boolean; const DateFormat: RawByteString = '');
  var Failed: Boolean;
  begin
    CheckEquals(Expected, ZSysUtils.RawSQLDateToDateTime(PAnsiChar(Value), PAnsiChar(DateFormat), Length(Value), Length(DateFormat), Failed), 'Expected Date');
    CheckEquals(ExpFailed, Failed, 'Fail value');
  end;

begin
  TestRawSQLDateToDateTime(Date1_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date2_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date3_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date4_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date5_0, EncodeDate(1999, 12, 31), False, 'YYYYMMDD');

  TestRawSQLDateToDateTime(Date1_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLDateToDateTime(Date2_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLDateToDateTime(Date3_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLDateToDateTime(Date4_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLDateToDateTime(Date5_1, EncodeDate(99, 12, 31), False, 'YYMMDD');

  TestRawSQLDateToDateTime(Date1_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date2_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date3_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date4_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLDateToDateTime(Date5_2, 0, True, 'YYYYMMDD');

  TestRawSQLDateToDateTime(Date1_3, EncodeDate(1999, 12, 31), False, 'FLOAT');
  TestRawSQLDateToDateTime(Date2_3, EncodeDate(1999, 12, 31), False, 'FLOAT');
  TestRawSQLDateToDateTime(Date3_3, 0, True, 'FLOAT');
  TestRawSQLDateToDateTime(Date4_3, 0, True, 'FLOAT');

  TestRawSQLDateToDateTime(Date1_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date2_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date3_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date4_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date5_0, EncodeDate(1999, 12, 31), False);

  TestRawSQLDateToDateTime(Date1_1, EncodeDate(99, 12, 31), False);
  TestRawSQLDateToDateTime(Date2_1, EncodeDate(99, 12, 31), False);
  TestRawSQLDateToDateTime(Date3_1, EncodeDate(99, 12, 31), False);
  TestRawSQLDateToDateTime(Date4_1, EncodeDate(99, 12, 31), False);
  TestRawSQLDateToDateTime(Date5_1, EncodeDate(99, 12, 31), False);

  TestRawSQLDateToDateTime(Date1_2, 0, True);
  TestRawSQLDateToDateTime(Date2_2, 0, True);
  TestRawSQLDateToDateTime(Date3_2, 0, True);
  TestRawSQLDateToDateTime(Date4_2, 0, True);
  TestRawSQLDateToDateTime(Date5_2, 0, True);

  TestRawSQLDateToDateTime(Date1_3, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date2_3, EncodeDate(1999, 12, 31), False);
  TestRawSQLDateToDateTime(Date3_3, 0, True);
  TestRawSQLDateToDateTime(Date4_3, 0, True);
end;

procedure TZTestSysUtilsCase.TestRawSQLTimeToDateTime;
const
  Time1_0 = RawByteString('23:59:59.999');
  Time2_0 = RawByteString('23/59/59/999');
  Time3_0 = RawByteString('23-59-59-999');
  Time4_0 = RawByteString('23-59/59\999');
  Time5_0 = RawByteString('235959999');
  Time6_0 = RawByteString('23:59:59.99');

  Time1_1 = RawByteString('23:59:59');
  Time2_1 = RawByteString('23/59/59');
  Time3_1 = RawByteString('23-59-59');
  Time4_1 = RawByteString('23-59/59');
  Time5_1 = RawByteString('235959');

  Time1_2 = RawByteString('2a:59:59.999');
  Time2_2 = RawByteString('23/5a/59/999');
  Time3_2 = RawByteString('23-59-5a-999');
  Time4_2 = RawByteString('23-59/59\9a9');
  Time5_2 = RawByteString('235959z99');

  Time1_3 = RawByteString('0.999999988425926');
  Time2_3 = RawByteString('00.999999988425926');
  Time3_3 = RawByteString('a.999999988425926');
  Time4_3 = RawByteString('0.9a9999988425926');
  Time5_3 = RawByteString('00000000000.999999988425926');

  procedure TestRawSQLTimeToDateTime(Value: RawByteString;
    const Expected: TDateTime; const ExpFailed: Boolean; const DateFormat: RawByteString = '');
  var Failed: Boolean;
  begin
    CheckEquals(Expected, ZSysUtils.RawSQLTimeToDateTime(PAnsiChar(Value), PAnsiChar(DateFormat), Length(Value), Length(DateFormat), Failed), 'Expected Date');
    CheckEquals(ExpFailed, Failed, 'Fail value');
  end;
begin
  TestRawSQLTimeToDateTime(Time1_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeToDateTime(Time2_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeToDateTime(Time3_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeToDateTime(Time4_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeToDateTime(Time5_0, EncodeTime(23, 59, 59, 999), False, 'HHNNSSZZZ');
  TestRawSQLTimeToDateTime(Time6_0, EncodeTime(23, 59, 59, 99), False, 'HH:NN:SS.ZZZ');

  TestRawSQLTimeToDateTime(Time1_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeToDateTime(Time2_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeToDateTime(Time3_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeToDateTime(Time4_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeToDateTime(Time5_1, EncodeTime(23, 59, 59, 0), False, 'HHNNSS');

  TestRawSQLTimeToDateTime(Time1_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeToDateTime(Time2_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeToDateTime(Time3_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeToDateTime(Time4_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeToDateTime(Time5_2, 0, True, 'HHNNSSZZZ');

  TestRawSQLTimeToDateTime(Time1_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeToDateTime(Time2_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeToDateTime(Time3_3, 0, True, 'FLOAT');
  TestRawSQLTimeToDateTime(Time4_3, 0, True, 'FLOAT');
  TestRawSQLTimeToDateTime(Time5_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');

  TestRawSQLTimeToDateTime(Time1_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time2_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time3_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time4_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time5_0, EncodeTime(23, 59, 59, 999), False);

  TestRawSQLTimeToDateTime(Time1_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeToDateTime(Time2_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeToDateTime(Time3_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeToDateTime(Time4_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeToDateTime(Time5_1, EncodeTime(23, 59, 59, 0), False);

  TestRawSQLTimeToDateTime(Time1_2, 0, True);
  TestRawSQLTimeToDateTime(Time2_2, 0, True);
  TestRawSQLTimeToDateTime(Time3_2, 0, True);
  TestRawSQLTimeToDateTime(Time4_2, 0, True);
  TestRawSQLTimeToDateTime(Time5_2, 0, True);

  TestRawSQLTimeToDateTime(Time1_3, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time2_3, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeToDateTime(Time3_3, 0, True);
  TestRawSQLTimeToDateTime(Time4_3, 0, True);
  TestRawSQLTimeToDateTime(Time5_3, EncodeTime(23, 59, 59, 999), False);
end;

procedure TZTestSysUtilsCase.TestRawSQLTimeStampToDateTime;
const
  Date1_0 = RawByteString('1999-12-31');
  Date2_0 = RawByteString('1999/12/31');
  Date3_0 = RawByteString('1999\12\31');
  Date4_0 = RawByteString('1999-12-31');
  Date5_0 = RawByteString('19991231');

  Date1_1 = RawByteString('99-12-31');
  Date2_1 = RawByteString('99/12/31');
  Date3_1 = RawByteString('99\12\31');
  Date4_1 = RawByteString('99-12-31');
  Date5_1 = RawByteString('991231');

  Date1_2 = RawByteString('199a-12-31');
  Date2_2 = RawByteString('1999/1a/31');
  Date3_2 = RawByteString('1999\12\3a');
  Date4_2 = RawByteString('0000-00-00');
  Date5_2 = RawByteString('00000000');

  Date1_3 = RawByteString('36525');
  Date2_3 = RawByteString('36525.0');
  Date3_3 = RawByteString('36525a');
  Date4_3 = RawByteString('36525.0a');

  Time1_0 = RawByteString('23:59:59.999');
  Time2_0 = RawByteString('23/59/59/999');
  Time3_0 = RawByteString('23-59-59-999');
  Time4_0 = RawByteString('23-59/59\999');
  Time5_0 = RawByteString('235959999');

  Time1_1 = RawByteString('23:59:59');
  Time2_1 = RawByteString('23/59/59');
  Time3_1 = RawByteString('23-59-59');
  Time4_1 = RawByteString('23-59/59');
  Time5_1 = RawByteString('235959');

  Time1_2 = RawByteString('2a:59:59.999');
  Time2_2 = RawByteString('23/5a/59/999');
  Time3_2 = RawByteString('23-59-5a-999');
  Time4_2 = RawByteString('23-59/59\9a9');
  Time5_2 = RawByteString('235959z99');

  Time1_3 = RawByteString('0.999999988425926');
  Time2_3 = RawByteString('00.999999988425926');
  Time3_3 = RawByteString('a.999999988425926');
  Time4_3 = RawByteString('0.9a9999988425926');
  Time5_3 = RawByteString('00000000000.999999988425926');

  TimeStamp1_0 = RawByteString('1999-12-31 23:59:59.999');
  TimeStamp2_0 = RawByteString('1999/12/31 23/59/59/999');
  TimeStamp3_0 = RawByteString('1999\12\31 23-59-59-999');
  TimeStamp4_0 = RawByteString('1999-12-31 23-59/59\999');
  TimeStamp5_0 = RawByteString('19991231235959999');

  TimeStamp1_1 = RawByteString('99-12-31 23:59:59');
  TimeStamp2_1 = RawByteString('99/12/31-23/59/59');
  TimeStamp3_1 = RawByteString('99\12\31-23-59-59');
  TimeStamp4_1 = RawByteString('99-12-31-23-59/59');
  TimeStamp5_1 = RawByteString('991231 235959');
  TimeStamp6_1 = RawByteString('99-12-31 23:59:59.999');
  TimeStamp7_1 = RawByteString('991231 235959999');

  TimeStamp1_2 = RawByteString('199a-12-31 2a:59:59.999');
  TimeStamp2_2 = RawByteString('1999/1a/31 23/5a/59/999');
  TimeStamp3_2 = RawByteString('1999\12\3a 23-59-5a-999');
  TimeStamp4_2 = RawByteString('0000-00-00 00-00-00-000');
  TimeStamp5_2 = RawByteString('00000000000000000');

  TimeStamp1_3 = RawByteString('36525.999999988425926');
  TimeStamp2_3 = RawByteString('36525.999999988425926');
  TimeStamp3_3 = RawByteString('36525a.999999988425926');
  TimeStamp4_3 = RawByteString('36525.99u999988425926');
  TimeStamp5_3 = RawByteString('a6525.99u999988425926');

  procedure TestRawSQLTimeStampToDateTime(Value: RawByteString;
    const Expected: TDateTime; const ExpFailed: Boolean; const DateFormat: RawByteString = '');
  var Failed: Boolean;
  begin
    CheckEquals(Expected, ZSysUtils.RawSQLTimeStampToDateTime(PAnsiChar(Value), PAnsiChar(DateFormat), Length(Value), Length(DateFormat), Failed), 'Expected Date');
    CheckEquals(ExpFailed, Failed, 'Fail value');
  end;

begin
  TestRawSQLTimeStampToDateTime(Date1_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date2_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date3_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date4_0, EncodeDate(1999, 12, 31), False, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date5_0, EncodeDate(1999, 12, 31), False, 'YYYYMMDD');

  TestRawSQLTimeStampToDateTime(Date1_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date2_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date3_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date4_1, EncodeDate(99, 12, 31), False, 'YY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date5_1, EncodeDate(99, 12, 31), False, 'YYMMDD');

  TestRawSQLTimeStampToDateTime(Date1_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date2_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date3_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date4_2, 0, True, 'YYYY-MM-DD');
  TestRawSQLTimeStampToDateTime(Date5_2, 0, True, 'YYYYMMDD');

  TestRawSQLTimeStampToDateTime(Date1_3, EncodeDate(1999, 12, 31), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Date2_3, EncodeDate(1999, 12, 31), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Date3_3, 0, True, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Date4_3, 0, True, 'FLOAT');

  TestRawSQLTimeStampToDateTime(Date1_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date2_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date3_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date4_0, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date5_0, EncodeDate(1999, 12, 31), False);

  TestRawSQLTimeStampToDateTime(Date1_1, EncodeDate(99, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date2_1, EncodeDate(99, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date3_1, EncodeDate(99, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date4_1, EncodeDate(99, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date5_1, EncodeDate(99, 12, 31), False);

  TestRawSQLTimeStampToDateTime(Date1_2, 0, True);
  TestRawSQLTimeStampToDateTime(Date2_2, 0, True);
  TestRawSQLTimeStampToDateTime(Date3_2, 0, True);
  TestRawSQLTimeStampToDateTime(Date4_2, 0, True);
  TestRawSQLTimeStampToDateTime(Date5_2, 0, True);

  TestRawSQLTimeStampToDateTime(Date1_3, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date2_3, EncodeDate(1999, 12, 31), False);
  TestRawSQLTimeStampToDateTime(Date3_3, 0, True);
  TestRawSQLTimeStampToDateTime(Date4_3, 0, True);

  TestRawSQLTimeStampToDateTime(Time1_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time2_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time3_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time4_0, EncodeTime(23, 59, 59, 999), False, 'HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time5_0, EncodeTime(23, 59, 59, 999), False, 'HHNNSSZZZ');

  TestRawSQLTimeStampToDateTime(Time1_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeStampToDateTime(Time2_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeStampToDateTime(Time3_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeStampToDateTime(Time4_1, EncodeTime(23, 59, 59, 0), False, 'HH-NN-SS');
  TestRawSQLTimeStampToDateTime(Time5_1, EncodeTime(23, 59, 59, 0), False, 'HHNNSS');

  TestRawSQLTimeStampToDateTime(Time1_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time2_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time3_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time4_2, 0, True, 'HH-NN-SS.ZZZ');
  TestRawSQLTimeStampToDateTime(Time5_2, 0, True, 'HHNNSSZZZ');

  TestRawSQLTimeStampToDateTime(Time1_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Time2_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Time3_3, 0, True, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Time4_3, 0, True, 'FLOAT');
  TestRawSQLTimeStampToDateTime(Time5_3, EncodeTime(23, 59, 59, 999), False, 'FLOAT');

  TestRawSQLTimeStampToDateTime(Time1_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(Time2_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(Time3_0, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(Time4_0, EncodeTime(23, 59, 59, 999), False);
//  TestRawSQLTimeStampToDateTime(Time5_0, EncodeTime(23, 59, 59, 999), False);

  TestRawSQLTimeStampToDateTime(Time1_1, EncodeTime(23, 59, 59, 0), False);
  {TestRawSQLTimeStampToDateTime(Time2_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(Time3_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(Time4_1, EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(Time5_1, EncodeTime(23, 59, 59, 0), False); }

  TestRawSQLTimeStampToDateTime(Time1_2, 0, True);
  TestRawSQLTimeStampToDateTime(Time2_2, 0, True);
  TestRawSQLTimeStampToDateTime(Time3_2, 0, True);
  TestRawSQLTimeStampToDateTime(Time4_2, 0, True);
  TestRawSQLTimeStampToDateTime(Time5_2, 0, True);

  TestRawSQLTimeStampToDateTime(Time1_3, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(Time2_3, EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(Time3_3, 0, True);
  TestRawSQLTimeStampToDateTime(Time4_3, 0, True);
  TestRawSQLTimeStampToDateTime(Time5_3, EncodeTime(23, 59, 59, 999), False);

  TestRawSQLTimeStampToDateTime(TimeStamp1_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp2_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp3_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp4_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp5_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'YYYYMMDDHHNNSSZZZ');

  TestRawSQLTimeStampToDateTime(TimeStamp1_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False, 'YY-MM-DD HH:NN:SS');
  TestRawSQLTimeStampToDateTime(TimeStamp2_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False, 'YY-MM-DD HH:NN:SS');
  TestRawSQLTimeStampToDateTime(TimeStamp3_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False, 'YY-MM-DD HH:NN:SS');
  TestRawSQLTimeStampToDateTime(TimeStamp4_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False, 'YY-MM-DD HH:NN:SS');
  TestRawSQLTimeStampToDateTime(TimeStamp5_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False, 'YYMMDD HHNNSS');

  TestRawSQLTimeStampToDateTime(TimeStamp1_2, 0, True, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp2_2, 0, True, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp3_2, 0, True, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp4_2, 0, True, 'YYYY-MM-DD HH:NN:SS.ZZZ');
  TestRawSQLTimeStampToDateTime(TimeStamp5_2, 0, True, 'YYYYMMDDHHNNSSZZZ');

  TestRawSQLTimeStampToDateTime(TimeStamp1_3, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(TimeStamp2_3, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False, 'FLOAT');
  TestRawSQLTimeStampToDateTime(TimeStamp3_3, 0, True, 'FLOAT');
  TestRawSQLTimeStampToDateTime(TimeStamp4_3, 0, True, 'FLOAT');

  TestRawSQLTimeStampToDateTime(TimeStamp1_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp2_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp3_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp4_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp5_0, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);

  TestRawSQLTimeStampToDateTime(TimeStamp1_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(TimeStamp2_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(TimeStamp3_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(TimeStamp4_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(TimeStamp5_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 0), False);
  TestRawSQLTimeStampToDateTime(TimeStamp6_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp7_1, EncodeDate(99, 12, 31)-EncodeTime(23, 59, 59, 999), False);

  TestRawSQLTimeStampToDateTime(TimeStamp1_2, 0, True);
  TestRawSQLTimeStampToDateTime(TimeStamp2_2, 0, True);
  TestRawSQLTimeStampToDateTime(TimeStamp3_2, 0, True);
  TestRawSQLTimeStampToDateTime(TimeStamp4_2, 0, True);
  TestRawSQLTimeStampToDateTime(TimeStamp5_2, 0, True);

  TestRawSQLTimeStampToDateTime(TimeStamp1_3, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp2_3, EncodeDate(1999, 12, 31)+EncodeTime(23, 59, 59, 999), False);
  TestRawSQLTimeStampToDateTime(TimeStamp3_3, 0, True);
  TestRawSQLTimeStampToDateTime(TimeStamp4_3, 0, True);
end;

{$IFDEF BENCHMARK}
procedure TZTestSysUtilsCase.TestASCII7ToString_VS_RawByteToString;
var
  tm: RawByteString;
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: String;

    function TestNotEmptyASCII7ToString: String;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Result := Result + NotEmptyASCII7ToString(tm);
      end;
    end;

    function TestPosEmptyASCII7ToString: String;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Result := Result + PosEmptyASCII7ToString(tm);
      end;
    end;

    function RawByteToString: String;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
      begin
        Result := '';
        Result := Result + String(tm);
      end;
    end;
begin
  ZSetString(PAnsiChar(RawByteString(Format('''%s''::timestamp', [FormatDateTime('yyyy-mm-dd hh":"mm":"ss"."zzz', now)]))), tm);

  Start := GetTickCount;
  S1 := TestNotEmptyASCII7ToString;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := RawByteToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of NotEmptyASCII7ToString VS. String(RawByteString)');

  system.WriteLn('');
  system.WriteLn('Benchmarking(x 10.000.000): RawByteString To UnicodeString');
  system.WriteLn('NotEmptyASCII7ToString from filled RawByteString:');
  system.WriteLn(Format('Zeos: %d ms VS. System String cast: %d ms', [Between1, Between2]));

  Start := GetTickCount;
  S1 := TestPosEmptyASCII7ToString;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := RawByteToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of NotEmptyASCII7ToString VS. String(RawByteString)');

  system.WriteLn('PosEmptyASCII7ToString from filled RawByteString:');
  system.WriteLn(Format('Zeos: %d ms VS. System String cast: %d ms', [Between1, Between2]));

  tm := '';

  Start := GetTickCount;
  S1 := TestNotEmptyASCII7ToString;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := RawByteToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of NotEmptyASCII7ToString VS. String(RawByteString)');

  system.WriteLn('NotEmptyASCII7ToString from empty RawByteString:');
  system.WriteLn(Format('Zeos: %d ms VS. System String cast: %d ms', [Between1, Between2]));

  Start := GetTickCount;
  S1 := TestPosEmptyASCII7ToString;
  Stop := GetTickCount;
  Between1 := Stop - Start;
  Start := GetTickCount;
  S2 := RawByteToString;
  Stop := GetTickCount;
  Between2 := Stop - Start;

  CheckEquals(s1, s2, 'Results of NotEmptyASCII7ToString VS. String(RawByteString)');

  system.WriteLn('PosEmptyASCII7ToString from empty RawByteString:');
  system.WriteLn(Format('Zeos: %d ms VS. system String cast: %d ms', [Between1, Between2]));
  system.WriteLn('');
end;

procedure TZTestSysUtilsCase.TestIntToRaw_VS_IntToStr;
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;

    function TestIntToRaw: RawByteString;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := IntToRaw(i);
    end;

    function TestIntToString: RawByteString;
    var
      I: Integer;
    begin
      for i := 0 to 10000000 do
        Result := NotEmptyStringToASCII7(IntToStr(i));
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
        Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(IntToStr(i));
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
        Result := NotEmptyStringToASCII7(IntToStr(I64));
      end;
      Result := NotEmptyStringToASCII7(IntToStr(High(Int64)));
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
  system.WriteLn(Format('Zeos: %d ms VS. SysUtils.IntToStr+NotEmptyStringToASCII7: %d ms', [Between1, Between2]));

end;

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
        Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(IntToStr(I64));
      end;
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(IntToStr(High(Int64)-1));
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

{$IFDEF WITH_UNICODEFROMLOCALECHARS}
procedure TZTestSysUtilsCase.TestLocalCharsFromUnicode;
const TestString = ZWideString('ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...');
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: RawByteString;

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
const TestString = ZWideString('ќдной из наиболее тривиальных задач, решаемых многими коллективами программистов, €вл€етс€ построение информационной системы дл€ автоматизации бизнес-де€тельности предпри€ти€. ¬се архитектурные компоненты (базы данных, сервера приложений, клиентское ...');
var
  Between1, Between2: Cardinal;
  Start, Stop: Cardinal;
  S1, S2: ZWideString;
  RBS: RawByteString;

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
  system.WriteLn(Format('TestUTF8EncodeOversized: %d ms VS. TestUTF8EncodeTestSize: %d ms', [Between1, Between2]));

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
        Result := StrToInt(IntToStr(i));
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
        Result := StrToIntDef(IntToStr(i), 1);
      Result := Result + StrToIntDef('test', -999);
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
      CheckEquals(Low(Int64), StrToInt64Def(IntToStr(Low(Int64)), 0), 'Results of RawToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(IntToStr(High(Int64)), 0), 'Results of RawToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(IntToStr(Int64(i)*Int64(i)), 1);
      Result := Result + StrToInt64Def('test', -999);
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
            StrToInt(IntToStr(I));
          {$ELSE}
            StrToInt(String(ZWideString(IntToStr(I))));
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
            StrToIntDef(IntToStr(I),1);
          {$ELSE}
            StrToIntDef(String(ZWideString(IntToStr(I))), 1);
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
        Result := UnicodeToInt64Def(IntToUnicode(Int64(i)*Int64(i)), 1);
      Result := Result + UnicodeToInt64Def('test', -999);
    end;

    function TStrToInt: Int64;
    var
      I: Integer;
    begin
      {$IFDEF UNICODE}
      CheckEquals(Low(Int64), StrToInt64Def(IntToStr(Low(Int64)), 0), 'Results of StrToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(IntToStr(High(Int64)), 0), 'Results of StrToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(IntToStr(Int64(i)*Int64(i)), 1);
      Result := Result + StrToInt64Def('test', -999);
      {$ELSE}
      CheckEquals(Low(Int64), StrToInt64Def(String(ZWideString(IntToStr(Low(Int64)))), 0), 'Results of StrToInt64Def VS. Low(Int64)');
      CheckEquals(High(Int64), StrToInt64Def(String(ZWideString(IntToStr(High(Int64)))), 0), 'Results of StrToInt64Def VS. High(Int64)');
      for i := 0 to 10000000 do
        Result := StrToInt64Def(String(ZWideString(IntToStr(Int64(i)*Int64(i)))), 1);
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
      {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
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
{$ENDIF}

initialization
  RegisterTest('core',TZTestSysUtilsCase.Suite);
end.
