{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Testing Framework              }
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

unit ZTestFramework;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  StrUtils, Classes, SysUtils, DateUtils,
  ZSysUtils, ZTestCase, ZSqlTestCase, ZCompatibility;

type

  {** Implements a test case for TZAbstractSQLTestCase. }
  TZTestPortableSQLTestCase = class(TZAbstractSQLTestCase)
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

  {** Implements a test case for TZAbstractSQLTestCase. }
  TZTestSpecificSQLTestCase = class(TZAbstractSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestTree;
  end;

  TZTestFramework = class(TZAbstractTestCase)
  protected
    ExcClass: ExceptClass;
    ExcMessage: string;
    procedure RaiseExc;
  published
    procedure TestCheckEqual;
    procedure TestExceptions;
  end;

implementation


{ TZTestPortableSQLTestCase }

{**
  Runs the first test.
}
procedure TZTestPortableSQLTestCase.TestOne;
begin
  BlankCheck;
  PrintLn('*** Test # 1 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protMySQL then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestPortableSQLTestCase.TestTwo;
begin
  BlankCheck;
  PrintLn('*** Test # 2 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protPostgre then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestPortableSQLTestCase.TestTree;
begin
  BlankCheck;
  PrintLn('*** Test # 3 ***');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protMSSQL then
    PrintLn('--- Part specific for mssql');
  PrintLn;
end;

{ TZTestSpecificSQLTestCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestSpecificSQLTestCase.GetSupportedProtocols: string;
begin
  Result := pl_all_mysql+','+pl_all_postgresql;
end;

{**
  Runs the first test.
}
procedure TZTestSpecificSQLTestCase.TestOne;
begin
  BlankCheck;
  PrintLn('### Test # 1 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protMySQL then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{**
  Runs the second test.
}
procedure TZTestSpecificSQLTestCase.TestTwo;
begin
  BlankCheck;
  PrintLn('### Test # 2 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protPostgre then
    PrintLn('--- Part specific for postgresql');
  PrintLn;
end;

{**
  Runs the third test.
}
procedure TZTestSpecificSQLTestCase.TestTree;
begin
  BlankCheck;
  PrintLn('### Test # 3 ###');
  PrintLn('Active Protocol: ' + Protocol);
  PrintLn('HostName: ' + HostName + ' Port: ' + IntToStr(Port)
    + ' Database: ' + Database + ' UserName: ' + UserName
    + ' Password: ' + Password);
  if ProtocolType = protMySQL then
    PrintLn('--- Part specific for mysql');
  PrintLn;
end;

{ TZTestFramework }

const
  SExpectedTestFailFmt = 'Expected test fail, got %s with message "%s"';
  SFailExpected = 'Test fail expected but wasn''t raised';

procedure TZTestFramework.TestCheckEqual;

  function Bytes(const Arr: array of Byte): TBytes;
  begin
    {$IFDEF WITH_VAR_INIT_WARNING}
    Result := nil;
    {$ENDIF}
    SetLength(Result, Length(Arr));
    if Length(Arr) > 0 then
      Move(Arr[0], Pointer(Result)^, Length(Arr));
  end;

  procedure CheckArraysSucceed(const Arr1, Arr2: array of Byte; const Msg: string);
  begin
    CheckEquals(Bytes(Arr1), Bytes(Arr2), 'Arrays succeed. ' + Msg);
  end;

  procedure CheckArraysFail(const Arr1, Arr2: array of Byte; const Msg: string);
  begin
    try
      CheckEquals(Bytes(Arr1), Bytes(Arr2), 'Arrays fail. ' + Msg);
    except on E: Exception do
      begin
        PrintLn(Format(SExpectedTestFailFmt, [E.ClassName, E.Message]));
        Exit;
      end;
    end;
    Fail( IfThen(Msg <> '', Msg + ', ') + SFailExpected);
  end;

  function Stream(const Arr: array of Byte): TMemoryStream;
  begin
    Result := nil;
    if Length(Arr) = 0 then Exit;
    Result := TMemoryStream.Create;
    Result.WriteBuffer(Arr[0], Length(Arr));
  end;

  procedure CheckStreamsSucceed(const Arr1, Arr2: array of Byte; const Msg: string);
  var Stm1, Stm2: TMemoryStream;
  begin
    Stm1 := Stream(Arr1);
    Stm2 := Stream(Arr2);
    CheckEquals(Stm1, Stm2, 'Streams succeed. ' + Msg);
    Stm1.Free;
    Stm2.Free;
  end;

  procedure CheckStreamsFail(const Arr1, Arr2: array of Byte; const Msg: string);
  var Stm1, Stm2: TMemoryStream;
  begin
    Stm1 := Stream(Arr1);
    Stm2 := Stream(Arr2);
    try
      CheckEquals(Stm1, Stm2, 'Streams fail. ' + Msg);
    except on E: Exception do
      begin
        PrintLn(Format(SExpectedTestFailFmt, [E.ClassName, E.Message]));
        Stm1.Free;
        Stm2.Free;
        Exit;
      end;
    end;
    Stm1.Free;
    Stm2.Free;
    Fail( IfThen(Msg <> '', Msg + ', ') + SFailExpected);
  end;

  procedure CheckDatesFail(Dt1, Dt2: TDateTime; const Msg: string);
  begin
    try
      CheckEqualsDate(Dt1, Dt2, [], 'Dates fail. ' + Msg);
    except on E: Exception do
      begin
        PrintLn(Format(SExpectedTestFailFmt, [E.ClassName, E.Message]));
        Exit;
      end;
    end;
    Fail( IfThen(Msg <> '', Msg + ', ') + SFailExpected);
  end;

var
  DtAct, DtExp: TDateTime;
begin
  PrintLn('### TestChecks ###');

  // Arrays
  CheckArraysSucceed([], [], 'empty');
  CheckArraysSucceed([1,2], [1,2], 'not empty');
  CheckArraysFail([], [1,2], 'empty and not empty');
  CheckArraysFail([1,3], [1,2], 'diff not empty');

  // Streams
  CheckStreamsSucceed([], [], 'empty');
  CheckStreamsSucceed([1,2], [1,2], 'not empty');
  CheckStreamsFail([], [1,2], 'empty and not empty');
  CheckStreamsFail([1,2], [], 'not empty and empty');
  CheckStreamsFail([1,3], [1,2], 'diff not empty');
  CheckStreamsFail([1,2,3], [1,2], 'diff sizes not empty');

  // Date
  DtExp := EncodeDateTime(2000, 1, 1, 1, 1, 1, 1);
  DtAct := DtExp;

  CheckEqualsDate(DtExp, DtAct, [], 'equal');
  CheckDatesFail(DtExp, RecodeYear(DtAct, 2001), 'not equal Y');
  CheckDatesFail(DtExp, RecodeMonth(DtAct, 2), 'not equal M');
  CheckDatesFail(DtExp, RecodeDay(DtAct, 2), 'not equal D');
  CheckDatesFail(DtExp, RecodeHour(DtAct, 2), 'not equal H');
  CheckDatesFail(DtExp, RecodeMinute(DtAct, 2), 'not equal M');
  CheckDatesFail(DtExp, RecodeSecond(DtAct, 2), 'not equal S');
  CheckDatesFail(DtExp, RecodeMilliSecond(DtAct, 2), 'not equal Ms');
end;

procedure TZTestFramework.RaiseExc;
begin
  if Assigned(ExcClass) then
    raise ExcClass.Create(ExcMessage);
end;

procedure TZTestFramework.TestExceptions;

  procedure ExceptionSuccess(AExceptClass: ExceptClass; const ExpectExcMsg, Msg: string);
  begin
    ExcClass := AExceptClass;
    ExcMessage := ExpectExcMsg;
    CheckException(RaiseExc, ExcClass, ExpectExcMsg, 'Exception success. ' + Msg);
  end;

  procedure ExceptionFail(AExceptClass: ExceptClass; const ExpectExcMsg, Msg: string);
  begin
    ExcClass := EAssertionFailed;
    ExcMessage := 'exc message';
    try
      CheckException(RaiseExc, AExceptClass, ExpectExcMsg, 'Exception fail. ' + Msg);
    except on E: Exception do
      begin
        PrintLn(Format(SExpectedTestFailFmt, [E.ClassName, E.Message]));
        Exit;
      end;
    end;
    Fail( IfThen(Msg <> '', Msg + ', ') + SFailExpected);
  end;

begin
  PrintLn('### TestExceptions ###');

  // Exception in method - Success
  ExceptionSuccess(EAssertionFailed, '', 'exception by class');
  ExceptionSuccess(EAssertionFailed, 'exc message', 'exception by message');

  // Exception in method - Fail
  ExceptionFail(EAbort, '', 'exception by class');
  ExceptionFail(EAssertionFailed, 'other exc message', 'exception by message');

  // Exception in code - Success
  try
    RaiseExc; // will raise
    Fail('');
  except on E: Exception do
    CheckNotTestFailure(E, 'exception raised in code');
  end;

  // Exception in code - Fail
  ExcClass := nil;
  try
    RaiseExc; // will NOT raise
    Fail('');
  except on E: Exception do
    begin
      try
        CheckNotTestFailure(E, 'exception not raised in code');
      except on E: Exception do
        begin
          PrintLn(Format(SExpectedTestFailFmt, [E.ClassName, E.Message]));
          Exit;
        end;
      end; // try
      Fail(SFailExpected);
    end;
  end;
end;

initialization
  RegisterTest('core',TZTestPortableSQLTestCase.Suite);
  RegisterTest('core',TZTestSpecificSQLTestCase.Suite);
  RegisterTest('core',TZTestFramework.Suite);
end.
