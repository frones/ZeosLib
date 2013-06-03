{#(@)$Id: FastMMMemLeakMonitor.pas 23 2008-08-26 04:42:20Z judc $ }
{  DUnit: An XTreme testing framework for Delphi programs. }
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 * Peter McNab <>
 *
 *******************************************************************************
 * Code to provide Memory Leak Detection at the test case level.
 * This code makes use of FastMM4.pas available from
 * http://fastmm.sourceforge.net
 *
 * FastMM is a fast replacement memory manager for Borland Delphi Win32
 * applications that scales well under multi-threaded usage, is not prone to
 * memory fragmentation, and supports shared memory without the use of
 * external .DLL files.

 * To use FastMM in DUnit for memory leak detection it is necessary to download
 * the latest stable release of FastMM from "fastmm.sourceforge.net"
 * Then add the path of the folder containing the FastMM4 source code to the
 * DUnit project's search path.
 *
 * Generally it should not be necessary to change FastMMOptions.inc settings.
 * However read the notes in FastMMOptions.inc carefully to understand the
 * effect of each option and set accordingly to best meet your testing
 * environment.
 *
 * Finally, select
 *   "Project, Option, Directories/Conditionals, Conditional Defines"
 * in the Delphi IDE and add the conditional define
 *   FASTMM (prefix with an extra ; if there are other defines)
 * to allow DUnit to use the FASTMM specific code.
 *
*)

unit FastMMMemLeakMonitor;

interface
uses
  SysUtils,
{$IFNDEF CLR}
  {$IFDEF FASTMM}
    FastMM4,
  {$ENDIF}
{$ENDIF}
  TestFrameWork;

type
  TMemLeakMonitor = class(TInterfacedObject, IMemLeakMonitor)
  protected
    FMS1: TMemoryManagerState;
    FMS2: TMemoryManagerState;

    function MemLeakDetected(out LeakSize: Integer): boolean; overload;
  public
    constructor Create;
  end;

  TDUnitMemLeakMonitor = class(TMemLeakMonitor, IDUnitMemLeakMonitor)

    procedure MarkMemInUse;
    function MemLeakDetected(const AllowedLeakSize: Integer;
                             const FailOnMemoryRecovery: boolean;
                             out   LeakSize: Integer): boolean; overload;
    function MemLeakDetected(const AllowedValuesGetter: TListIterator;
                             const FailOnMemoryRecovery: Boolean;
                             out   LeakIndex: integer;
                             out   LeakSize: Integer): Boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                             const TestProcChangedMem: Integer;
                             out   ErrorMsg: string): boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                             const TestSetupChangedMem: Integer;
                             const TestProcChangedMem: Integer;
                             const TestTearDownChangedMem: Integer;
                             const TestCaseChangedMem: Integer;
                             out   ErrorMsg: string): boolean; overload;
  end;

implementation

{ TMemLeakMonitor }

constructor TMemLeakMonitor.Create;
begin
  inherited;
  GetMemoryManagerState(FMS1);
end;

function TMemLeakMonitor.MemLeakDetected(out LeakSize: Integer): boolean;
var
  I: Integer;
  SMBSize1,
  SMBSize2: Int64;

begin
  LeakSize  := 0;
  SMBSize1 := 0;
  SMBSize2 := 0;
  GetMemoryManagerState(FMS2);

  for I := 0 to NumSmallBlockTypes - 1 do // Iterate through the blocks
  begin
    Inc(SMBSize1, (FMS1.SmallBlockTypeStates[i].InternalBlockSize *
                   FMS1.SmallBlockTypeStates[i].AllocatedBlockCount));
    Inc(SMBSize2, (FMS2.SmallBlockTypeStates[i].InternalBlockSize *
                   FMS2.SmallBlockTypeStates[i].AllocatedBlockCount));
  end;

  LeakSize := (SMBSize2 - SMBSize1);

  LeakSize := LeakSize +
    (Int64(FMS2.TotalAllocatedMediumBlockSize) - Int64(FMS1.TotalAllocatedMediumBlockSize)) +
    (Int64(FMS2.TotalAllocatedLargeBlockSize) - Int64(FMS1.TotalAllocatedLargeBlockSize));

  Result := LeakSize <> 0;
end;

// May be called after detecting memory use change at Test Procedure level
function TDUnitMemLeakMonitor.GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                                              const TestProcChangedMem: Integer;
                                              out   ErrorMsg: string): boolean;
begin
  ErrorMsg := '';

  if (TestProcChangedMem > 0) then
    ErrorMsg := IntToStr(TestProcChangedMem) +
      ' Bytes Memory Leak in Test Procedure'
  else
  if (TestProcChangedMem  < 0) and (FailOnMemoryRecovery) then
    ErrorMsg := IntToStr(Abs(TestProcChangedMem)) +
     ' Bytes Memory Recovered in Test Procedure';

  Result := (Length(ErrorMsg) = 0);
end;

function TDUnitMemLeakMonitor.MemLeakDetected(const AllowedLeakSize: Integer;
                                              const FailOnMemoryRecovery: boolean;
                                              out   LeakSize: Integer): boolean;
begin
  LeakSize := 0;
  inherited MemLeakDetected(LeakSize);
  Result := ((LeakSize > 0) and (LeakSize <> AllowedLeakSize)) or
    ((LeakSize < 0) and (FailOnMemoryRecovery) and (LeakSize <> AllowedLeakSize));
end;

procedure TDUnitMemLeakMonitor.MarkMemInUse;
begin
  GetMemoryManagerState(FMS1);
end;

function TDUnitMemLeakMonitor.MemLeakDetected(const AllowedValuesGetter: TListIterator;
                                              const FailOnMemoryRecovery: Boolean;
                                              out   LeakIndex: integer;
                                              out   LeakSize: Integer): Boolean;
var
  AllowedLeakSize: Integer;
begin
  LeakIndex := 0;
  LeakSize  := 0;
  Result := False;
  inherited MemLeakDetected(LeakSize);
  if (LeakSize = 0) then
    exit;

  // Next line access value stored via SetAllowedLeakSize, if any
  if LeakSize = AllowedValuesGetter then
    Exit;

  repeat // loop over values stored via SetAllowedLeakArray
    inc(LeakIndex);
    AllowedLeakSize := AllowedValuesGetter;
    if (LeakSize = AllowedLeakSize) then
      Exit;
  until (AllowedLeakSize = 0);
  Result := (LeakSize > 0) or ((LeakSize < 0) and FailOnMemoryRecovery);
end;

// Expanded message generation for detected leak isolation
// Use additional knowledge of when Setup and or TearDown have nor run.

function TDUnitMemLeakMonitor.GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                                              const TestSetupChangedMem: integer;
                                              const TestProcChangedMem: Integer;
                                              const TestTearDownChangedMem: integer;
                                              const TestCaseChangedMem: Integer;
                                              out   ErrorMsg: string): boolean;
var
  Location: string;
begin
  Result := False;
  ErrorMsg := '';

  if (TestSetupChangedMem = 0) and (TestProcChangedMem = 0) and
     (TestTearDownChangedMem = 0) and (TestCaseChangedMem <> 0) then
  begin
    ErrorMsg :=
      'Error in TestFrameWork. No leaks in Setup, TestProc or Teardown but '+
      IntToStr(TestCaseChangedMem) +
      ' Bytes Memory Leak reported across TestCase';
    Exit;
  end;

  if (TestSetupChangedMem + TestProcChangedMem + TestTearDownChangedMem) <>
    TestCaseChangedMem then
  begin
    ErrorMsg :=
      'Error in TestFrameWork. Sum of Setup, TestProc and Teardown leaks <> '+
      IntToStr(TestCaseChangedMem) +
      ' Bytes Memory Leak reported across TestCase';
    Exit;
  end;

  Result := True;
  if TestCaseChangedMem = 0 then
    Exit;  // Dont waste further time here

  if (TestCaseChangedMem < 0) and not FailOnMemoryRecovery then
    Exit;     // Dont waste further time here


// We get to here because there is a memory use imbalance to report.
  if (TestCaseChangedMem > 0) then
    ErrorMsg := IntToStr(TestCaseChangedMem) + ' Bytes memory leak  ('
  else
    ErrorMsg := IntToStr(TestCaseChangedMem) + ' Bytes memory recovered  (';

  Location := '';

  if (TestSetupChangedMem <> 0) then
    Location := 'Setup= ' + IntToStr(TestSetupChangedMem) + '  ';
  if (TestProcChangedMem <> 0) then
    Location := Location + 'TestProc= ' + IntToStr(TestProcChangedMem) + '  ';
  if (TestTearDownChangedMem <> 0) then
    Location := Location + 'TearDown= ' + IntToStr(TestTearDownChangedMem) + '  ';

  ErrorMsg := ErrorMsg + Location + ')';
  Result := (Length(ErrorMsg) = 0);
end;

end.
