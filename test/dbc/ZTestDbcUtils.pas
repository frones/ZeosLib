{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Case for Database Connectivity Classes       }
{                                                         }
{ Originally written by Sergey Seroukhov, Sergey Merkuriev}
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

unit ZTestDbcUtils;

interface
{$I ZDbc.inc}
uses {$IFDEF FPC}testregistry,fpcunit{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, SysUtils, ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcUtilsCase = class(TTestCase)
  published
    procedure TestCheckConvertion;
    procedure TestDefineColumnTypeName;
    procedure TestAnsiSqlDateToDateTime;
  end;

implementation

uses Classes, TypInfo, ZSysUtils, ZDbcUtils;

{ TZTestDbcUtilsCase }

{**
  Test for function AnsiSqlDateToDateTime
}
procedure TZTestDbcUtilsCase.TestAnsiSqlDateToDateTime;
var
  Value: string;
  Default: TDateTime;
begin
  Value := '';
  CheckEquals(0, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal time. }
  Value := '22:01:31';
  Default := EncodeTime(22, 01, 31, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests incorrect time. }
  Value := '99:77:99';
  CheckEquals(0, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal date. }
  Value := '1999-01-31';
  Default := EncodeDate(1999, 01, 31);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal 2k date. }
  Value := '2001-07-22';
  Default := EncodeDate(2001, 07, 22);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Test normal datetime time. }
  Value := '1999-01-31 02:20:42';
  Default := EncodeDate(1999, 01, 31) + EncodeTime(02, 20, 42, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal 2k datetimetime. }
  Value := '2001-07-22 02:20:42';
  Default := EncodeDate(2001, 07, 22) + EncodeTime(02, 20, 42, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);
end;

{**
  Tests function CheckConvertion.
}
procedure TZTestDbcUtilsCase.TestCheckConvertion;

  procedure CheckConvertionTrue(Type1, Type2: TZSQLType);
  begin
    Check(CheckConvertion(Type1, Type2),
      GetEnumName(TypeInfo(TZSQLType), Ord(Type1))+
      ' to '+
      GetEnumName(TypeInfo(TZSQLType), Ord(Type2)));
  end;

  procedure CheckConvertionFalse(Type1, Type2: TZSQLType);
  begin
    Check(not CheckConvertion(Type1, Type2),
      GetEnumName(TypeInfo(TZSQLType), Ord(Type1))+
      ' to '+
      GetEnumName(TypeInfo(TZSQLType), Ord(Type2)));
  end;

begin
  {check Boolean convertion}
  CheckConvertionTrue(stBoolean,stBoolean);
  CheckConvertionTrue(stBoolean,stByte);
  CheckConvertionTrue(stBoolean,stShort);
  CheckConvertionTrue(stBoolean,stSmall);
  CheckConvertionTrue(stBoolean,stInteger);
  CheckConvertionTrue(stBoolean,stLong);
  CheckConvertionTrue(stBoolean,stFloat);
  CheckConvertionTrue(stBoolean,stDouble);
  CheckConvertionTrue(stBoolean,stBigDecimal);
  CheckConvertionTrue(stBoolean,stString);
  CheckConvertionFalse(stBoolean,stBytes);
  CheckConvertionFalse(stBoolean,stAsciiStream);
  CheckConvertionFalse(stBoolean,stUnicodeStream);
  CheckConvertionFalse(stBoolean,stBinaryStream);
  CheckConvertionFalse(stBoolean,stDate);
  CheckConvertionFalse(stBoolean,stTime);
  CheckConvertionFalse(stBoolean,stTimestamp);
  {check Byte convertion}
  CheckConvertionTrue(stByte,stBoolean);
  CheckConvertionTrue(stByte,stByte);
  CheckConvertionTrue(stByte,stShort);
  CheckConvertionTrue(stByte,stSmall);
  CheckConvertionTrue(stByte,stInteger);
  CheckConvertionTrue(stByte,stLong);
  CheckConvertionTrue(stByte,stFloat);
  CheckConvertionTrue(stByte,stDouble);
  CheckConvertionTrue(stByte,stBigDecimal);
  CheckConvertionTrue(stByte,stString);
  CheckConvertionFalse(stByte,stBytes);
  CheckConvertionFalse(stByte,stAsciiStream);
  CheckConvertionFalse(stByte,stUnicodeStream);
  CheckConvertionFalse(stByte,stBinaryStream);
  CheckConvertionFalse(stByte,stDate);
  CheckConvertionFalse(stByte,stTime);
  CheckConvertionFalse(stByte,stTimestamp);
  {check Short convertion}
  CheckConvertionTrue(stShort,stBoolean);
  CheckConvertionTrue(stShort,stShort);
  CheckConvertionTrue(stShort,stSmall);
  CheckConvertionTrue(stShort,stByte);
  CheckConvertionTrue(stShort,stInteger);
  CheckConvertionTrue(stShort,stLong);
  CheckConvertionTrue(stShort,stFloat);
  CheckConvertionTrue(stShort,stDouble);
  CheckConvertionTrue(stShort,stBigDecimal);
  CheckConvertionTrue(stShort,stString);
  CheckConvertionFalse(stShort,stBytes);
  CheckConvertionFalse(stShort,stAsciiStream);
  CheckConvertionFalse(stShort,stUnicodeStream);
  CheckConvertionFalse(stShort,stBinaryStream);
  CheckConvertionFalse(stShort,stDate);
  CheckConvertionFalse(stShort,stTime);
  CheckConvertionFalse(stShort,stTimestamp);
  {check Small convertion}
  CheckConvertionTrue(stSmall,stBoolean);
  CheckConvertionTrue(stSmall,stByte);
  CheckConvertionTrue(stSmall,stSmall);
  CheckConvertionTrue(stSmall,stSmall);
  CheckConvertionTrue(stSmall,stInteger);
  CheckConvertionTrue(stSmall,stLong);
  CheckConvertionTrue(stSmall,stFloat);
  CheckConvertionTrue(stSmall,stDouble);
  CheckConvertionTrue(stSmall,stBigDecimal);
  CheckConvertionTrue(stSmall,stString);
  CheckConvertionFalse(stSmall,stBytes);
  CheckConvertionFalse(stSmall,stAsciiStream);
  CheckConvertionFalse(stSmall,stUnicodeStream);
  CheckConvertionFalse(stSmall,stBinaryStream);
  CheckConvertionFalse(stSmall,stDate);
  CheckConvertionFalse(stSmall,stTime);
  CheckConvertionFalse(stSmall,stTimestamp);
  {check Integer convertion}
  CheckConvertionTrue(stInteger,stBoolean);
  CheckConvertionTrue(stInteger,stByte);
  CheckConvertionTrue(stInteger,stShort);
  CheckConvertionTrue(stInteger,stSmall);
  CheckConvertionTrue(stInteger,stInteger);
  CheckConvertionTrue(stInteger,stLong);
  CheckConvertionTrue(stInteger,stFloat);
  CheckConvertionTrue(stInteger,stDouble);
  CheckConvertionTrue(stInteger,stBigDecimal);
  CheckConvertionTrue(stInteger,stString);
  CheckConvertionFalse(stInteger,stBytes);
  CheckConvertionFalse(stInteger,stAsciiStream);
  CheckConvertionFalse(stInteger,stUnicodeStream);
  CheckConvertionFalse(stInteger,stBinaryStream);
  CheckConvertionFalse(stInteger,stDate);
  CheckConvertionFalse(stInteger,stTime);
  CheckConvertionFalse(stInteger,stTimestamp);
  {check Long convertion}
  CheckConvertionTrue(stLong,stBoolean);
  CheckConvertionTrue(stLong,stByte);
  CheckConvertionTrue(stLong,stByte);
  CheckConvertionTrue(stLong,stSmall);
  CheckConvertionTrue(stLong,stInteger);
  CheckConvertionTrue(stLong,stLong);
  CheckConvertionTrue(stLong,stFloat);
  CheckConvertionTrue(stLong,stDouble);
  CheckConvertionTrue(stLong,stBigDecimal);
  CheckConvertionTrue(stLong,stString);
  CheckConvertionFalse(stLong,stBytes);
  CheckConvertionFalse(stLong,stAsciiStream);
  CheckConvertionFalse(stLong,stBinaryStream);
  CheckConvertionFalse(stLong,stDate);
  CheckConvertionFalse(stLong,stTime);
  CheckConvertionFalse(stLong,stTimestamp);
  CheckConvertionFalse(stLong,stUnicodeStream);
  {check Float convertion}
  CheckConvertionTrue(stFloat,stBoolean);
  CheckConvertionTrue(stFloat,stByte);
  CheckConvertionTrue(stFloat,stShort);
  CheckConvertionTrue(stFloat,stSmall);
  CheckConvertionTrue(stFloat,stInteger);
  CheckConvertionTrue(stFloat,stLong);
  CheckConvertionTrue(stFloat,stFloat);
  CheckConvertionTrue(stFloat,stDouble);
  CheckConvertionTrue(stFloat,stBigDecimal);
  CheckConvertionTrue(stFloat,stString);
  CheckConvertionFalse(stFloat,stBytes);
  CheckConvertionFalse(stFloat,stAsciiStream);
  CheckConvertionFalse(stFloat,stBinaryStream);
  CheckConvertionFalse(stFloat,stDate);
  CheckConvertionFalse(stFloat,stTime);
  CheckConvertionFalse(stFloat,stTimestamp);
  CheckConvertionFalse(stFloat,stUnicodeStream);
  {check Double convertion}
  CheckConvertionTrue(stDouble,stBoolean);
  CheckConvertionTrue(stDouble,stByte);
  CheckConvertionTrue(stDouble,stShort);
  CheckConvertionTrue(stDouble,stSmall);
  CheckConvertionTrue(stDouble,stInteger);
  CheckConvertionTrue(stDouble,stLong);
  CheckConvertionTrue(stDouble,stFloat);
  CheckConvertionTrue(stDouble,stDouble);
  CheckConvertionTrue(stDouble,stBigDecimal);
  CheckConvertionTrue(stDouble,stString);
  CheckConvertionFalse(stDouble,stBytes);
  CheckConvertionFalse(stDouble,stAsciiStream);
  CheckConvertionFalse(stDouble,stBinaryStream);
  CheckConvertionTrue(stDouble,stDate);
  CheckConvertionTrue(stDouble,stTime);
  CheckConvertionTrue(stDouble,stTimestamp);
  CheckConvertionFalse(stDouble,stUnicodeStream);
  {check BigDecimal convertion}
  CheckConvertionTrue(stBigDecimal,stBoolean);
  CheckConvertionTrue(stBigDecimal,stByte);
  CheckConvertionTrue(stBigDecimal,stShort);
  CheckConvertionTrue(stBigDecimal,stSmall);
  CheckConvertionTrue(stBigDecimal,stInteger);
  CheckConvertionTrue(stBigDecimal,stLong);
  CheckConvertionTrue(stBigDecimal,stFloat);
  CheckConvertionTrue(stBigDecimal,stDouble);
  CheckConvertionTrue(stBigDecimal,stBigDecimal);
  CheckConvertionTrue(stBigDecimal,stString);
  CheckConvertionFalse(stBigDecimal,stBytes);
  CheckConvertionFalse(stBigDecimal,stAsciiStream);
  CheckConvertionFalse(stBigDecimal,stBinaryStream);
  CheckConvertionFalse(stBigDecimal,stDate);
  CheckConvertionFalse(stBigDecimal,stTime);
  CheckConvertionFalse(stBigDecimal,stTimestamp);
  CheckConvertionFalse(stBigDecimal,stUnicodeStream);
  {check String convertion}
  CheckConvertionTrue(stString,stBoolean);
  CheckConvertionTrue(stString,stByte);
  CheckConvertionTrue(stString,stShort);
  CheckConvertionTrue(stString,stSmall);
  CheckConvertionTrue(stString,stInteger);
  CheckConvertionTrue(stString,stLong);
  CheckConvertionTrue(stString,stFloat);
  CheckConvertionTrue(stString,stDouble);
  CheckConvertionTrue(stString,stBigDecimal);
  CheckConvertionTrue(stString,stString);
  CheckConvertionTrue(stString,stBytes);
  CheckConvertionTrue(stString,stAsciiStream);
  CheckConvertionFalse(stString,stBinaryStream);
  CheckConvertionTrue(stString,stDate);
  CheckConvertionTrue(stString,stTime);
  CheckConvertionTrue(stString,stTimestamp);
  CheckConvertionTrue(stString,stUnicodeStream);
  {check Bytes convertion}
  CheckConvertionFalse(stBytes,stBoolean);
  CheckConvertionFalse(stBytes,stByte);
  CheckConvertionFalse(stBytes,stSmall);
  CheckConvertionFalse(stBytes,stInteger);
  CheckConvertionFalse(stBytes,stLong);
  CheckConvertionFalse(stBytes,stFloat);
  CheckConvertionFalse(stBytes,stDouble);
  CheckConvertionFalse(stBytes,stBigDecimal);
  CheckConvertionTrue(stBytes,stString);
  CheckConvertionTrue(stBytes,stBytes);
  CheckConvertionFalse(stBytes,stAsciiStream);
  CheckConvertionTrue(stBytes,stBinaryStream);
  CheckConvertionFalse(stBytes,stDate);
  CheckConvertionFalse(stBytes,stTime);
  CheckConvertionFalse(stBytes,stTimestamp);
  CheckConvertionFalse(stBytes,stUnicodeStream);
  {check Date convertion}
  CheckConvertionFalse(stDate,stBoolean);
  CheckConvertionFalse(stDate,stByte);
  CheckConvertionFalse(stDate,stShort);
  CheckConvertionFalse(stDate,stSmall);
  CheckConvertionFalse(stDate,stInteger);
  CheckConvertionFalse(stDate,stLong);
  CheckConvertionFalse(stDate,stFloat);
  CheckConvertionTrue(stDate,stDouble);
  CheckConvertionFalse(stDate,stBigDecimal);
  CheckConvertionTrue(stDate,stString);
  CheckConvertionFalse(stDate,stBytes);
  CheckConvertionFalse(stDate,stAsciiStream);
  CheckConvertionFalse(stDate,stBinaryStream);
  CheckConvertionTrue(stDate,stDate);
  CheckConvertionFalse(stDate,stTime);
  CheckConvertionTrue(stDate,stTimestamp);
  CheckConvertionFalse(stDate,stUnicodeStream);
  {check Time convertion}
  CheckConvertionFalse(stTime,stBoolean);
  CheckConvertionFalse(stTime,stByte);
  CheckConvertionFalse(stTime,stShort);
  CheckConvertionFalse(stTime,stSmall);
  CheckConvertionFalse(stTime,stInteger);
  CheckConvertionFalse(stTime,stLong);
  CheckConvertionFalse(stTime,stFloat);
  CheckConvertionTrue(stTime,stDouble);
  CheckConvertionFalse(stTime,stBigDecimal);
  CheckConvertionTrue(stTime,stString);
  CheckConvertionFalse(stTime,stBytes);
  CheckConvertionFalse(stTime,stAsciiStream);
  CheckConvertionFalse(stTime,stBinaryStream);
  CheckConvertionFalse(stTime,stDate);
  CheckConvertionTrue(stTime,stTime);
  CheckConvertionTrue(stTime,stTimestamp);
  CheckConvertionFalse(stTime,stUnicodeStream);
  {check TimeStamp convertion}
  CheckConvertionFalse(stTimestamp,stBoolean);
  CheckConvertionFalse(stTimestamp,stByte);
  CheckConvertionFalse(stTimestamp,stShort);
  CheckConvertionFalse(stTimestamp,stSmall);
  CheckConvertionFalse(stTimestamp,stInteger);
  CheckConvertionFalse(stTimestamp,stLong);
  CheckConvertionFalse(stTimestamp,stFloat);
  CheckConvertionTrue(stTimestamp,stDouble);
  CheckConvertionFalse(stTimestamp,stBigDecimal);
  CheckConvertionTrue(stTimestamp,stString);
  CheckConvertionFalse(stTimestamp,stBytes);
  CheckConvertionFalse(stTimestamp,stAsciiStream);
  CheckConvertionFalse(stTimestamp,stBinaryStream);
  CheckConvertionTrue(stTimestamp,stDate);
  CheckConvertionTrue(stTimestamp,stTime);
  CheckConvertionTrue(stTimestamp,stTimestamp);
  CheckConvertionFalse(stTimestamp,stUnicodeStream);
  {check AsciiStream convertion}
  CheckConvertionFalse(stAsciiStream,stBoolean);
  CheckConvertionFalse(stAsciiStream,stByte);
  CheckConvertionFalse(stAsciiStream,stShort);
  CheckConvertionFalse(stAsciiStream,stSmall);
  CheckConvertionFalse(stAsciiStream,stInteger);
  CheckConvertionFalse(stAsciiStream,stLong);
  CheckConvertionFalse(stAsciiStream,stFloat);
  CheckConvertionFalse(stAsciiStream,stDouble);
  CheckConvertionFalse(stAsciiStream,stBigDecimal);
  CheckConvertionTrue(stAsciiStream,stString);
  CheckConvertionTrue(stAsciiStream,stBytes);
  CheckConvertionTrue(stAsciiStream,stAsciiStream);
  CheckConvertionFalse(stAsciiStream,stBinaryStream);
  CheckConvertionFalse(stAsciiStream,stDate);
  CheckConvertionFalse(stAsciiStream,stTime);
  CheckConvertionFalse(stAsciiStream,stTimestamp);
  CheckConvertionTrue(stAsciiStream, stUnicodeStream);
  {check BinaryStream convertion}
  CheckConvertionFalse(stBinaryStream,stBoolean);
  CheckConvertionFalse(stBinaryStream,stByte);
  CheckConvertionFalse(stBinaryStream,stShort);
  CheckConvertionFalse(stBinaryStream,stSmall);
  CheckConvertionFalse(stBinaryStream,stInteger);
  CheckConvertionFalse(stBinaryStream,stLong);
  CheckConvertionFalse(stBinaryStream,stFloat);
  CheckConvertionFalse(stBinaryStream,stDouble);
  CheckConvertionFalse(stBinaryStream,stBigDecimal);
  CheckConvertionTrue(stBinaryStream,stString);
  CheckConvertionTrue(stBinaryStream,stBytes);
  CheckConvertionFalse(stBinaryStream,stAsciiStream);
  CheckConvertionTrue(stBinaryStream,stBinaryStream);
  CheckConvertionFalse(stBinaryStream,stDate);
  CheckConvertionFalse(stBinaryStream,stTime);
  CheckConvertionFalse(stBinaryStream,stTimestamp);
  CheckConvertionFalse(stBinaryStream,stUnicodeStream);
  {check unknown convertion}
  CheckConvertionFalse(stUnknown,stBoolean);
  CheckConvertionFalse(stUnknown,stByte);
  CheckConvertionFalse(stUnknown,stShort);
  CheckConvertionFalse(stUnknown,stSmall);
  CheckConvertionFalse(stUnknown,stInteger);
  CheckConvertionFalse(stUnknown,stLong);
  CheckConvertionFalse(stUnknown,stFloat);
  CheckConvertionFalse(stUnknown,stDouble);
  CheckConvertionFalse(stUnknown,stBigDecimal);
  CheckConvertionTrue(stUnknown,stString);
  CheckConvertionFalse(stUnknown,stBytes);
  CheckConvertionFalse(stUnknown,stAsciiStream);
  CheckConvertionFalse(stUnknown,stBinaryStream);
  CheckConvertionFalse(stUnknown,stDate);
  CheckConvertionFalse(stUnknown,stTime);
  CheckConvertionFalse(stUnknown,stTimestamp);
  CheckConvertionFalse(stUnknown,stUnknown);
  {check UnicodeStream convertion}
  CheckConvertionFalse(stUnicodeStream,stBoolean);
  CheckConvertionFalse(stUnicodeStream,stByte);
  CheckConvertionFalse(stUnicodeStream,stShort);
  CheckConvertionFalse(stUnicodeStream,stSmall);
  CheckConvertionFalse(stUnicodeStream,stInteger);
  CheckConvertionFalse(stUnicodeStream,stLong);
  CheckConvertionFalse(stUnicodeStream,stFloat);
  CheckConvertionFalse(stUnicodeStream,stDouble);
  CheckConvertionFalse(stUnicodeStream,stBigDecimal);
  CheckConvertionTrue(stUnicodeStream,stString);
  CheckConvertionTrue(stUnicodeStream,stBytes);
  CheckConvertionTrue(stUnicodeStream,stAsciiStream);
  CheckConvertionFalse(stUnicodeStream,stBinaryStream);
  CheckConvertionFalse(stUnicodeStream,stDate);
  CheckConvertionFalse(stUnicodeStream,stTime);
  CheckConvertionFalse(stUnicodeStream,stTimestamp);
  CheckConvertionFalse(stUnicodeStream,stUnknown);
  CheckConvertionTrue(stUnicodeStream,stUnicodeStream);
end;

{**
  Test for function DefineColumnTypeName
}
procedure TZTestDbcUtilsCase.TestDefineColumnTypeName;
begin
  CheckEquals('Boolean', DefineColumnTypeName(stBoolean));
  CheckEquals('Byte', DefineColumnTypeName(stByte));
  CheckEquals('Short', DefineColumnTypeName(stShort));
  CheckEquals('Small', DefineColumnTypeName(stSmall));
  CheckEquals('Integer', DefineColumnTypeName(stInteger));
  CheckEquals('Long', DefineColumnTypeName(stLong));
  CheckEquals('Float', DefineColumnTypeName(stFloat));
  CheckEquals('Double', DefineColumnTypeName(stDouble));
  CheckEquals('BigDecimal', DefineColumnTypeName(stBigDecimal));
  CheckEquals('String', DefineColumnTypeName(stString));
  CheckEquals('Bytes', DefineColumnTypeName(stBytes));
  CheckEquals('AsciiStream', DefineColumnTypeName(stAsciiStream));
  CheckEquals('BinaryStream', DefineColumnTypeName(stBinaryStream));
  CheckEquals('Date', DefineColumnTypeName(stDate));
  CheckEquals('Time', DefineColumnTypeName(stTime));
  CheckEquals('Timestamp', DefineColumnTypeName(stTimestamp));
  CheckEquals('Unknown', DefineColumnTypeName(stUnknown));
  CheckEquals('UnicodeStream', DefineColumnTypeName(stUnicodeStream));
end;

initialization
  RegisterTest('dbc',TZTestDbcUtilsCase.Suite);
end.
