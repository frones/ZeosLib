{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Utility Functions              }
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

unit ZTestList;

interface

{$I ZCore.inc}

uses TestFramework, ZTestDefinitions, SysUtils, Classes;

type

  {** Implements a test case for Utilities. }
  TZTestListCase = class(TZCoreGenericTestCase)
  published
    procedure TestPerformanceStandardList;
    procedure TestPerformanceItemList;
  end;

implementation

uses ZCompatibility;

const
  ITEM_COUNT = 10000;
  ITEM_SIZE = 512;

{ TZTestListCase }

{**
  Runs a performance test for TZItemList
}
procedure TZTestListCase.TestPerformanceItemList;
var
  Buffer: Pointer;
  StartTicks: Cardinal;
begin
  StartTicks := GetTickCount;
  Buffer := AllocMem(ITEM_COUNT * ITEM_SIZE);
  PrintLn(Format('Creating buffer, Time: %d', [GetTickCount - StartTicks]));

  StartTicks := GetTickCount;
  FreeMem(Buffer, ITEM_COUNT * ITEM_SIZE);
  PrintLn(Format('Removing buffer, Time: %d', [GetTickCount - StartTicks]));
end;

{**
  Runs a performance test for standard TList
}
procedure TZTestListCase.TestPerformanceStandardList;
var
  I: Integer;
  Buffer: Pointer;
  StartTicks: Cardinal;
  List: TList;
begin
  List := TList.Create;

  { Fills the list }
  StartTicks := GetTickCount;
  for I := 0 to ITEM_COUNT - 1 do
  begin
    Buffer := AllocMem(ITEM_SIZE);
    List.Add(Buffer);
  end;
  PrintLn(Format('Creating standard list, Time: %d',
    [GetTickCount - StartTicks]));

  { Removes the list }
  StartTicks := GetTickCount;
  for I := 0 to ITEM_COUNT - 1 do
  begin
    Buffer := List[I];
    FreeMem(Buffer, ITEM_SIZE);
  end;
  List.Clear;
  PrintLn(Format('Removing standard list, Time: %d',
    [GetTickCount - StartTicks]));

  List.Free;
end;

initialization
  TestFramework.RegisterTest(TZTestListCase.Suite);
end.
