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
  Classes, TestFramework, ZCompatibility;

type

  {** Implements an abstract class for all test cases. }
  TZAbstractTestCase = class(TTestCase)
  private
    FDecimalSeparator: Char;
    FSuppressTestOutput: Boolean;

  protected
    property DecimalSeparator: Char read FDecimalSeparator
      write FDecimalSeparator;
    property SuppressTestOutput: Boolean read FSuppressTestOutput
      write FSuppressTestOutput;

    { Test configuration methods. }
    procedure LoadConfiguration; virtual;

    { Configuration properties accessing methods. }
    function ReadProperty(const Group, Key, Default: string): string;
    function ReadGroupProperty(const Key, Default: string): string;
    function ReadInheritProperty(const Key, Default: string): string;

    { Visual output methods. }
    procedure Print(_Message: string); virtual;
    procedure PrintLn(_Message: string = ''); virtual;

    { Additional checking methods. }
    procedure CheckEquals(Array1, Array2: TByteDynArray;
      _Message: string = ''); overload;
    procedure CheckEquals(Stream1, Stream2: TStream;
      _Message: string = ''); overload;

    { Measurement methods. }
    function GetTickCount: Cardinal;

  public
    constructor Create(MethodName: string); override;
    destructor Destroy; override;
  end;

  {** Implements a generic test case. }
  TZGenericTestCase = class (TZAbstractTestCase);

implementation

uses
{$IFDEF LINUX}
  IdGlobal,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, ZSysUtils, ZTestConfig;

{ TZAbstractTestCase }

{**
  Creates the abstract test case and initialize global parameters.
  @param MethodName a name of the test case.
}
constructor TZAbstractTestCase.Create(MethodName: string);
begin
  inherited Create(MethodName);
  LoadConfiguration;
end;

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractTestCase.Destroy;
begin
  inherited Destroy;
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
  SysUtils.DecimalSeparator := FDecimalSeparator;

  { Defines a 'suppress test output' setting. }
  Temp := ReadInheritProperty(SUPPRESS_TEST_OUTPUT_KEY, TRUE_VALUE);
  FSuppressTestOutput := StrToBoolEx(Temp);
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
function TZAbstractTestCase.ReadInheritProperty(
  const Key, Default: string): string;
const
  UNKNOWN_VALUE = '';
begin
  Result := TestConfig.ReadProperty(TestGroup, Key, UNKNOWN_VALUE);
  if Result = UNKNOWN_VALUE then
    Result := TestConfig.ReadProperty(COMMON_GROUP, Key, Default);
end;

{**
  Function compare two arrays. If arrays not equals raise exception.
  @param the first array for compare
  @param the secon array for compare
}
procedure TZAbstractTestCase.CheckEquals(Array1, Array2: TByteDynArray;
  _Message: string = '');
var
  Size1, Size2: Integer;
  P1, P2: Pointer;
begin
  Size1 := Length(Array1);
  Size2 := Length(Array2);
  if Size1 <> Size2 then
    FailNotEquals(IntToStr(Size1), IntToStr(Size1), _Message, CallerAddr);

  P1 := Addr(Array1);
  P2 := Addr(Array2);
  if CompareMem(P1, P2, High(Array1)) then
    Fail('Arrays not equal.' +  _Message)
end;

{**
   Function compare two streams. If streams not equals raise exception.
   @param the first array for compare
   @param the secon array for compare
}
procedure TZAbstractTestCase.CheckEquals(Stream1, Stream2: TStream;
  _Message: string = '');
var
  Buffer1, Buffer2: Pointer;
  Size1, Size2: Integer;
  ReadNum1, ReadNum2: Integer;
  ResultCompareMem: Boolean;
begin
  ResultCompareMem := False;

  CheckNotNull(Stream1, 'Stream #1 is null. ' + _Message);
  CheckNotNull(Stream2, 'Stream #2 is null. ' + _Message);
{$IFNDEF LINUX}
  CheckEquals(Stream1.Size, Stream2.Size, 'Stream sizes are not equal. '
    + _Message);
{$ELSE}
  if Stream1.Size <> Stream2.Size then
   Fail('Stream sizes are not equal. ' + _Message);
{$ENDIF}

  Size1 := Stream1.Size;
  Size2 := Stream1.Size;
  Buffer1 := AllocMem(Size1);
  Buffer2 := AllocMem(Size2);
  Stream1.Position := 0;
  Stream2.Position := 0;

  try
    ReadNum1 := Stream1.Read(Buffer1^, Size1);
    ReadNum2 := Stream2.Read(Buffer2^, Size2);
    if ReadNum1 = ReadNum2 then
      ResultCompareMem := CompareMem(Buffer1, Buffer2, ReadNum1);
  finally
    FreeMem(Buffer1, Size1);
    FreeMem(Buffer2, Size2);
  end;

  CheckEquals(ReadNum1, ReadNum2, 'Read sizes are not equal.' + _Message);
  Check(ResultCompareMem, 'Read sizes are not equal.' + _Message);
end;

{**
  Prints a debug message to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.Print(_Message: string);
begin
  if not SuppressTestOutput then
    System.Write(_Message);
end;

{**
  Prints a debug message ends with EOL to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.PrintLn(_Message: string);
begin
  if not SuppressTestOutput then
    System.WriteLn(_Message);
end;

{**
  Gets a system specific number of ticks.
  @return the current number of system ticks.
}
function TZAbstractTestCase.GetTickCount: Cardinal;
begin
{$IFDEF LINUX}
  Result := IdGlobal.GetTickCount;
{$ELSE}
  Result := Windows.GetTickCount;
{$ENDIF}
end;

end.

