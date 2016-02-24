{#(@)$Id: TestFramework.pas 41 2011-04-16 01:13:25Z medington $ }
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
 * Portions created by The DUnit Group are Copyright (C) 2000-2008.
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
 *
 *)

{$IFDEF CLR}
  {$UNSAFECODE ON}
{$ENDIF}
{$BOOLEVAL OFF}
unit TestFramework;


{ The following is for C++ Support }
(*$HPPEMIT '#pragma link "dunitrtl.lib"' *)

interface
uses
{$IFDEF CLR}
  System.Reflection, System.Diagnostics, System.IO, System.Collections.Specialized,
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  TypInfo,
  DUnitConsts;

const
  rcs_id: string = '#(@)$Id: TestFramework.pas 41 2011-04-16 01:13:25Z medington $';
  rcs_version : string = '$Revision: 41 $';

type
{$IFDEF MSWINDOWS}
  {$DEFINE MSWINDOWS_OR_CLR}
  {$DEFINE MSWINDOWS_OR_POSIX}
{$ENDIF}
{$IFDEF POSIX}
  {$DEFINE MSWINDOWS_OR_POSIX}
{$ENDIF}
{$IFDEF CLR}
//  Pointer = Borland.Delphi.System.Pointer;
  IUnknown = interface(IInterface)
  end;

  TestAttribute = class(TCustomAttribute)
  end;

  {$DEFINE MSWINDOWS_OR_CLR}
  {$DEFINE PUREPASCAL}
{$ENDIF}
{$IFNDEF CPU386}
  {$DEFINE PUREPASCAL}
{$ENDIF !CPU386}

{$IFDEF CLR}
  TTestMethod  = string;
{$ELSE}
  TTestMethod  = procedure of object;
{$ENDIF}
  TTestProc    = procedure;

  TTestCaseClass  = class of TTestCase;

  ITestListener   = interface;
  IStatusListener = interface;

  TTestResult   = class;
{$M+}
  TAbstractTest = class;
{$M-}
  TTestCase     = class;
  TTestSuite    = class;
  TTestFailure  = class;

  ExceptionClass = class of Exception;

  ETestFailure = class(EAbort)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;

  EDunitException = class(Exception);
  ETestError = class(EDunitException);
  EStopTestsFailure = class(ETestFailure);
  EPostTestFailure = class(ETestFailure);

  TAllowedLeakArray = array[0..3] of integer;
  TListIterator = function: integer of object;

  IMemLeakMonitor = interface(IUnknown)
  ['{041368CC-5B04-4111-9E2E-05A5908B3A58}']

    function MemLeakDetected(out LeakSize: Integer): Boolean;
  end;

  IDUnitMemLeakMonitor = interface(IMemLeakMonitor)
  ['{45466FCA-1ADC-4457-A41C-88FA3F8D23F7}']

    function MemLeakDetected(const AllowedLeakSize: Integer;
                             const FailOnMemoryRecovery: Boolean;
                             out   LeakSize: Integer): Boolean; overload;
    function MemLeakDetected(const AllowedValuesGetter: TListIterator;
                             const FailOnMemoryRecovery: Boolean;
                             out   LeakIndex: integer;
                             out   LeakSize: Integer): Boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: Boolean;
                             const TestProcChangedMem: Integer;
                             out   ErrorMsg: string): Boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                             const TestSetupChangedMem: Integer;
                             const TestProcChangedMem: Integer;
                             const TestTearDownChangedMem: Integer;
                             const TestCaseChangedMem: Integer;
                             out   ErrorMsg: string): boolean; overload;
    procedure MarkMemInUse;
  end;


  { thrown to force a debugger break on a test failure }
  EBreakingTestFailure = class(EDunitException)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;


  ITest = interface(IUnknown)
    ['{89CCD557-7DE1-4814-B033-ABAFE0870EC7}']
    function GetName: string;

    function  CountTestCases: integer;
    function  CountEnabledTestCases: integer;
    function  Tests: IInterfaceList;


    procedure SetUp;
    procedure TearDown;

    function  Run : TTestResult;  overload;
    procedure Run(testResult: TTestResult); overload;

    procedure RunWithFixture(testResult: TTestResult);
    procedure RunTest(testResult: TTestResult);

    function  GetTestMethodInvoked: Boolean;
    procedure SetTestMethodInvoked(const Value: Boolean);
    property  TestMethodInvoked: Boolean read  GetTestMethodInvoked
                                         write SetTestMethodInvoked;

    function  GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);

    procedure SetStartTime(Value :Int64);
    function  GetStartTime : Int64;

    procedure SetStopTime(Value :Int64);
    function  GetStopTime : Int64;
    function  ElapsedTestTime: Cardinal;


    procedure SetStatusListener(Listener :IStatusListener);
    function  GetStatus :string;

    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;
    procedure LoadConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean); overload;

    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;
    procedure SaveConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean); overload;

    procedure SetGUIObject(const guiObject: TObject);
    function  GetGUIObject: TObject;

    property Name:    string  read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property GUIObject: TObject read GetGUIObject write SetGUIObject;
    property Status:  string  read GetStatus;

    property StartTime: Int64 read GetStartTime write SetStartTime;
    property StopTime:  Int64 read GetStopTime  write SetStopTime;

    procedure SetFailsOnNoChecksExecuted(const Value: Boolean);
    function  GetFailsOnNoChecksExecuted: Boolean;
    property  FailsOnNoChecksExecuted: Boolean
                read GetFailsOnNoChecksExecuted
                write SetFailsOnNoChecksExecuted;

    function  GetFailsOnMemoryLeak: Boolean;
    procedure SetFailsOnMemoryLeak(const Value: Boolean);
    property  FailsOnMemoryLeak: Boolean
                read GetFailsOnMemoryLeak
                write SetFailsOnMemoryLeak;
    function  GetAllowedMemoryLeakSize: Integer;
    procedure SetAllowedMemoryLeakSize(const NewSize: Integer);
    property  AllowedMemoryLeakSize: Integer read GetAllowedMemoryLeakSize
                                           write SetAllowedMemoryLeakSize;
    function  GetFailsOnMemoryRecovery: Boolean;
    procedure SetFailsOnMemoryRecovery(const Value: Boolean);
    property  FailsOnMemoryRecovery: Boolean
                read GetFailsOnMemoryRecovery
                write SetFailsOnMemoryRecovery;
    procedure SetAllowedLeakArray(AllowedList: array of Integer);

    function  GetIgnoreSetUpTearDownLeaks: Boolean;
    procedure SetIgnoreSetUpTearDownLeaks(const Value: Boolean);
    property  IgnoreSetUpTearDownLeaks: Boolean
                read GetIgnoreSetUpTearDownLeaks
                write SetIgnoreSetUpTearDownLeaks;

    function  GetAllowedLeak: Integer;
    function  GetAllowedLeaksIterator: TListIterator;
    property  AllowedLeaksIterator: TListIterator read GetAllowedLeaksIterator;
  end;


  {: General interface for test decorators}
  ITestDecorator = interface(ITest)
    ['{8B3FC229-3033-4590-AD5C-01914C6E2C9F}']
    {: Get the decorated test
    @return The decorated test }
    function GetTest: ITest;
    property Test: ITest read GetTest;
  end;

  { IStatusListeners are notified of test status messages }
  IStatusListener = interface
  ['{8681DC88-033C-4A42-84F4-4C52EF9ABAC0}']
    procedure Status(test :ITest; const Msg :string);
  end;

  { ITestListeners get notified of testing events.
    See TTestResult.AddListener()
  }
  ITestListener = interface(IStatusListener)
    ['{114185BC-B36B-4C68-BDAB-273DBD450F72}']

    procedure TestingStarts;
    procedure StartTest(test: ITest);

    procedure AddSuccess(test: ITest);
    procedure AddError(error: TTestFailure);
    procedure AddFailure(Failure: TTestFailure);

    procedure EndTest(test: ITest);
    procedure TestingEnds(testResult :TTestResult);

    function  ShouldRunTest(test :ITest):Boolean;
  end;


  ITestListenerX = interface(ITestListener)
    ['{5C28B1BE-38B5-4D6F-AA96-A04E9302C317}']

    procedure StartSuite(suite: ITest);
    procedure EndSuite(suite: ITest);
  end;

  // a named collection of tests
  ITestSuite = interface(ITest)
    ['{C20E38EF-7369-44D9-9D84-08E84EC1DCF0}']

    procedure AddTest(test: ITest);
    procedure AddSuite(suite : ITestSuite);
  end;

  {  Adapter to allow a TTestResult to receive status messages
     from the running test }
  TStatusToResultAdapter = class(TInterfacedObject, IStatusListener)
  protected
    FTestResult :TTestResult;
  public
    constructor Create(TestResult :TTestResult);
    procedure   Status(Test :ITest; const Msg :string);
  end;

  { A TTestResult collects the results of executing a test case.
  And notifies registered ITestListener of testing events. }
  TTestResult = class(TObject)
  private
    FTotalTime: Int64;
  protected
    FFailures: TList;
    FErrors: TList;
    FOverRides: Integer;
    FListeners: IInterfaceList;
    FRootTest: ITest;
    FRunTests: integer;
    FStop: Boolean;
    FBreakOnFailures :Boolean;
    FFailsIfNoChecksExecuted: Boolean;
    FIgnoresMemoryLeakInSetUpTearDown: Boolean;
    FMemoryLeakIgnoredInSetupOrTearDown: boolean;
    FFailsIfMemoryLeaked: Boolean;
    FMethodPtr: Pointer;

    FStatusAdapter :IStatusListener;

    procedure Run(test: ITest); virtual;
    function  RunTestSetup(test: ITest):Boolean; virtual;
    procedure RunTestTearDown(test: ITest); virtual;
    function  RunTestRun(test: ITest) : Boolean; virtual;

    procedure TestingStarts;                           virtual;
    procedure StartSuite(suite: ITest);                virtual;
    procedure StartTest(test: ITest);                  virtual;
    function  ShouldRunTest(test :ITest) :Boolean;     virtual;
    procedure Status(test :ITest; const Msg :string);  virtual;
    procedure EndSuite(suite: ITest);                  virtual;
    procedure EndTest(test: ITest);                    virtual;
    procedure TestingEnds;                             virtual;
  public

    constructor Create;
    destructor  Destroy; override;

    procedure AddListener(listener: ITestListener); virtual;

    procedure RunSuite(test: ITest);  overload;
    procedure AddSuccess(test: ITest); virtual;
    function  AddFailure(test: ITest; e: Exception; Addrs :Pointer): TTestFailure; overload;
    function  AddFailure(test: ITest; Addrs :Pointer; msg :string = ''): TTestFailure; overload;
    function  AddError(test: ITest; e: Exception; Addrs :Pointer; msg :string = ''): TTestFailure; virtual;

    procedure Stop; virtual;
    function  ShouldStop: Boolean; virtual;

    function RunCount: integer;     virtual;
    function ErrorCount: integer;   virtual;
    function FailureCount: integer; virtual;

    function  GetError(idx :Integer) :TTestFailure;
    function  GetFailure(idx :Integer) :TTestFailure;

    function  WasStopped :Boolean; virtual;
    function  WasSuccessful: Boolean; virtual;

    property  BreakOnFailures :Boolean read  FBreakOnFailures write FBreakOnFailures;
    property  FailsIfNoChecksExecuted :Boolean read  fFailsIfNoChecksExecuted
                                               write fFailsIfNoChecksExecuted;
    property  FailsIfMemoryLeaked :Boolean read  fFailsIfMemoryLeaked
                                           write fFailsIfMemoryLeaked;
    property  IgnoresMemoryLeakInSetUpTearDown: Boolean
                read FIgnoresMemoryLeakInSetUpTearDown
                write FIgnoresMemoryLeakInSetUpTearDown;
    property  MemoryLeakIgnoredInSetupOrTearDown: Boolean
                read FMemoryLeakIgnoredInSetupOrTearDown
                write FMemoryLeakIgnoredInSetupOrTearDown;
    property  TotalTime: Int64 read FTotalTime;

    property Errors[i :Integer] :TTestFailure read GetError;
    property Failures[i :Integer] :TTestFailure read GetFailure;
    property OverRides: integer read FOverRides write FOverRides;
  end;

  TAbstractTest = class(TInterfacedObject, ITest)
  protected
    FTestName: string;
    FEnabled: Boolean;

    FStartTime: Int64;
    FStopTime:  Int64;

    FStatusListener :IStatusListener;
    FStatusStrings  :TStrings;

    FExpectedException: ExceptionClass;
    FCheckCalled: Boolean;
    FFailsOnNoChecksExecuted: Boolean;
    FFailsOnMemoryLeak: Boolean;
    FTestMethodInvoked: Boolean;
    FFailsOnMemoryRecovery: Boolean;
    FIgnoreSetUpTearDownLeaks: Boolean;
    FMemoryLeakIgnoredInSetupTearDown: boolean;
    FAllowedLeakList: TAllowedLeakArray;
    FAllowedLeakListIndex: Word;

    // Object used by the GUI to map the test onto a GUI object such as a tree node
    FGUIObject: TObject;

    procedure Invoke(AMethod: TTestMethod); virtual;
    procedure RunWithFixture(testResult: TTestResult); virtual;
    procedure RunTest(testResult: TTestResult); virtual;

    procedure SetUp; virtual;
    procedure TearDown; virtual;

    procedure SetStartTime(Value :Int64); virtual;
    function  GetStartTime : Int64;       virtual;

    procedure SetStopTime(Value :Int64);  virtual;
    function  GetStopTime : Int64;        virtual;

    procedure SetGUIObject(const guiObject: TObject);
    function  GetGUIObject: TObject;

    procedure SetFailsOnNoChecksExecuted(const Value: Boolean);
    function  GetFailsOnNoChecksExecuted: Boolean;

    function  GetFailsOnMemoryLeak: Boolean;
    procedure SetFailsOnMemoryLeak(const Value: Boolean);

    {$IFNDEF CLR} // related to Check(Not)EqualsMem, pointer based, unsuitable for .NET
    function GetMemDiffStr(expected, actual: pointer; size:longword; msg:string):string;
    {$ENDIF}

    function  GetAllowedMemoryLeakSize: Integer;
    procedure SetAllowedMemoryLeakSize(const NewSize: Integer);
    function  GetFailsOnMemoryRecovery: Boolean;
    procedure SetFailsOnMemoryRecovery(const Value: Boolean);
    function  GetIgnoreSetUpTearDownLeaks: Boolean;
    procedure SetIgnoreSetUpTearDownLeaks(const Value: Boolean);
    function  GetMemoryLeakIgnoredInSetupTearDown: Boolean;
    procedure SetMemoryLeakIgnoredInSetupTearDown(const Value: Boolean);

    procedure SetAllowedLeakArray(AllowedList: array of Integer);
    function  GetAllowedLeak: Integer; // Is the iterator returned below
    function  GetAllowedLeaksIterator: TListIterator;
  public
    constructor Create(AName: string);
    destructor Destroy; override;

    function  GetName: string; virtual;

    function  GetEnabled: Boolean; virtual;
    procedure SetEnabled(value: Boolean); virtual;

    function  Tests: IInterfaceList; virtual;

    function  CountTestCases: integer; virtual;
    function  CountEnabledTestCases: integer; virtual;

    function  Run: TTestResult; overload;
    procedure Run(testResult: TTestResult); overload;

    function  GetTestMethodInvoked: Boolean;
    procedure SetTestMethodInvoked(const Value: Boolean);

    function  ElapsedTestTime: Cardinal; virtual;

    procedure SetStatusListener(Listener :IStatusListener);
    procedure Status(const Msg :string);
    function  GetStatus :string;

    procedure LoadConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean); overload;
    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;
    procedure SaveConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean); overload;
    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;

    property Name:    string  read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function  BoolToStr(ABool: Boolean): string;

    procedure Check(condition: Boolean; msg: string = ''); virtual;
    procedure CheckTrue(condition: Boolean; msg: string = ''); virtual;
    procedure CheckFalse(condition: Boolean; msg: string = ''); virtual;
    procedure CheckEquals(expected, actual: extended; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: extended; delta: extended; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: Cardinal; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: int64; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: uint64; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: ShortString; msg: string = ''); overload; virtual;
    procedure CheckEqualsString(expected, actual: string; msg: string = ''); virtual;
{$IFNDEF CLR}
{$IFDEF UNICODE}
    procedure CheckEquals(expected, actual: RawByteString; msg: string = ''); overload; virtual;
{$ELSE}
    procedure CheckEquals(expected, actual: WideString; msg: string = ''); overload; virtual;
{$ENDIF}
    procedure CheckEqualsWideString(expected, actual: WideString; msg: string = ''); virtual;
    procedure CheckEqualsMem(expected, actual: pointer; size:longword; msg:string=''); virtual;
{$ELSE}
    procedure CheckEquals(expected, actual: AnsiString; msg: string = ''); overload; virtual;
{$ENDIF}
    procedure CheckEquals(expected, actual: Boolean; msg: string = ''); overload; virtual;
    procedure CheckEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: Cardinal; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: int64; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: ShortString; msg: string = ''); overload; virtual;
    procedure CheckNotEqualsString(expected, actual: string; msg: string = ''); virtual;
{$IFNDEF CLR}
{$IFDEF UNICODE}
    procedure CheckNotEquals(expected, actual: RawByteString; msg: string = ''); overload; virtual;
{$ELSE}
    procedure CheckNotEquals(const expected, actual: WideString; msg: string = ''); overload; virtual;
{$ENDIF}
    procedure CheckNotEqualsWideString(const expected, actual: WideString; msg: string = ''); virtual;
    procedure CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string=''); virtual;
{$ENDIF}
    procedure CheckNotEquals(expected, actual: Boolean; msg: string = ''); overload; virtual;
    procedure CheckNotEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckNotEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotNull(obj :IUnknown; msg: string = ''); overload; virtual;
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: TObject; msg: string = ''); overload; virtual;

    procedure CheckNotNull(obj: TObject; msg: string = ''); overload; virtual;
    procedure CheckNull(obj: TObject; msg: string = ''); overload; virtual;

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg: string = '');
    procedure CheckEquals(expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckIs(AObject: TObject; AClass: TClass; msg: string = ''); overload; virtual;

    procedure Fail(msg: string; ErrorAddrs: Pointer = nil); overload; virtual;
    procedure FailEquals(expected, actual: WideString; msg: string = ''; ErrorAddrs: Pointer = nil); overload; virtual;
    procedure FailNotEquals(expected, actual: WideString; msg: string = ''; ErrorAddrs: Pointer = nil); overload; virtual;
    procedure FailNotSame(expected, actual: WideString; msg: string = ''; ErrorAddrs: Pointer = nil); virtual;

    function EqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
    function NotEqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
    function NotSameErrorMessage(expected, actual, msg: string): WideString;

    procedure StopTests(msg: string = ''); virtual;

{$IFNDEF CLR}
    procedure CheckMethodIsNotEmpty(MethodPointer: pointer);
{$ENDIF}

    procedure StartExpectingException(e: ExceptionClass);
    procedure StopExpectingException(msg :string = '');

    property ExpectedException :ExceptionClass
      read  fExpectedException
      write StartExpectingException;

  published
    property FailsOnNoChecksExecuted: Boolean
      read GetFailsOnNoChecksExecuted
      write SetFailsOnNoChecksExecuted;

    property FailsOnMemoryLeak: Boolean
      read GetFailsOnMemoryLeak
      write SetFailsOnMemoryLeak;

    property TestMethodInvoked: Boolean
      read GetTestMethodInvoked
      write SetTestMethodInvoked;

    property AllowedMemoryLeakSize: Integer
      read GetAllowedMemoryLeakSize
      write SetAllowedMemoryLeakSize;

    property AllowedLeaksIterator: TListIterator read GetAllowedLeaksIterator;

    property FailsOnMemoryRecovery: Boolean
      read GetFailsOnMemoryRecovery
      write SetFailsOnMemoryRecovery;

    property IgnoreSetUpTearDownLeaks: Boolean
      read GetIgnoreSetUpTearDownLeaks
      write SetIgnoreSetUpTearDownLeaks;
  end;

  TTestCase = class(TAbstractTest, ITest)
  protected
    fMethod:    TTestMethod;

    procedure Invoke(AMethod: TTestMethod); override;
    procedure RunWithFixture(testResult: TTestResult); override;
    procedure RunTest(testResult: TTestResult); override;
  public
    constructor Create(MethodName: string); virtual;
    class function Suite: ITestSuite; virtual;

    procedure Run(testResult: TTestResult); overload;
  published
  end;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (DEFINED(MSWINDOWS) OR DEFINED(POSIX)) AND (CompilerVersion >= 21.0)}
    {$DEFINE GENERICS} // Requires generics and RTTI (Delphi 2010)
    {$DEFINE RTTI}     // RTTI supported
  {$ELSEIF DEFINED(CLR) AND (CompilerVersion >= 19.0)}
    {$DEFINE GENERICS} // Requires generics (Delphi 2007)
  {$IFEND}
{$ENDIF}

{$IFDEF GENERICS}

{$IF DEFINED(MSWINDOWS) OR DEFINED(POSIX)}
  TValueData = Pointer;
  PObject = ^TObject;
{$ELSE IF DEFINED(CLR)}
  TValueData = TObject;
{$IFEND}

  TConverter<T> = class
  private
    class function ValueToString(Info: PTypeInfo; Size: Cardinal; Value: TValueData): string;
  public
    class function ToString(Value: T): string; reintroduce;
  end;

  TGenericTestCase = class(TTestCase, ITest)
  private
    class function Compare<T>(const Expected, Actual: T): Integer;
  public
    procedure CheckEquals<T>(Expected, Actual: T; Msg: string = ''); overload;
    procedure CheckNotEquals<T>(Expected, Actual: T; Msg: string = ''); overload;

    procedure FailEquals<T>(Expected, Actual: T; Msg: string = ''; ErrorAddrs: Pointer = nil); overload;
    procedure FailNotEquals<T>(Expected, Actual: T; Msg: string = ''; ErrorAddrs: Pointer = nil); overload;

    function EqualsErrorMessage<T>(Expected, Actual: T; Msg: string): string; overload;
    function NotEqualsErrorMessage<T>(Expected, Actual: T; Msg: string): string; overload;
  end;

{$ENDIF GENERICS}

  TTestSuite = class(TAbstractTest, ITestSuite, ITest)
  protected
    FTests: IInterfaceList;
    procedure RunTest(testResult: TTestResult); override;
  public
    constructor Create; overload;
    constructor Create(AName: string); overload;
    constructor Create(TestClass: TTestCaseClass); overload;
    constructor Create(AName: string; const Tests: array of ITest); overload;
    function CountTestCases: integer;         override;
    function CountEnabledTestCases: integer;  override;

    function Tests: IInterfaceList;                 override;
    procedure AddTest(ATest: ITest);                virtual;
    procedure AddTests(testClass: TTestCaseClass);  virtual;
    procedure AddSuite(suite:  ITestSuite);         virtual;


    procedure LoadConfiguration(const iniFile: TCustomIniFile; const section: string);  override;
    procedure SaveConfiguration(const iniFile: TCustomIniFile; const section: string);  override;
  end;


  TTestFailure = class(TObject)
  protected
    FFailedTest: ITest;
    FThrownExceptionClass: TClass;
    FThrownExceptionMessage: string;
    FThrownExceptionAddress: Pointer;
    FStackTrace:             string;

    function CaptureStackTrace(ThrownException: Exception; ThrownExceptionAddress: Pointer): string;
  public
    constructor Create(FailedTest: ITest; ThrownException: Exception; Addrs: Pointer; msg: string = ''); overload;
    constructor Create(FailedTest: ITest; Addrs: Pointer; msg: string); overload;
    function FailedTest: ITest; virtual;
    function ThrownExceptionClass: TClass; virtual;
    function ThrownExceptionName: string; virtual;
    function ThrownExceptionMessage: string; virtual;
    function ThrownExceptionAddress: pointer; virtual;

    function LocationInfo: string; virtual;
    function AddressInfo:  string; virtual;

    function StackTrace:   string; virtual;
  end;


  TMethodEnumerator = class
  protected
{$IFDEF CLR}
    FMethodNameList: StringCollection;
{$ELSE}
    FMethodNameList:  array of string;
{$ENDIF}
    function GetNameOfMethod(idx: integer):  string;
    function GetMethodCount: Integer;
  public
    constructor Create(AClass: TClass);
    property MethodCount: integer read GetMethodCount;
    property NameOfMethod[idx:  integer]: string read GetNameOfMethod;
  end;


// creating suites
function  TestSuite(AName: string; const Tests: array of ITest): ITestSuite;

// test registry
procedure RegisterTest(SuitePath: string; test: ITest); overload;
procedure RegisterTest(test: ITest);                    overload;
procedure RegisterTests(SuitePath: string; const Tests: array of ITest);  overload;
procedure RegisterTests(const Tests: array of ITest);                     overload;
function  RegisteredTests: ITestSuite;
procedure ClearRegistry;

// running tests
function RunTest(suite: ITest; const listeners: array of ITestListener): TTestResult; overload;
function RunRegisteredTests(const listeners: array of ITestListener): TTestResult;

// utility routines
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (NOT DEFINED(CLR)) AND (CompilerVersion >= 23.0) }
      {$DEFINE HAS_BUILTIN_RETURNADDRESS} // Requires ReturnAddress intrinsic function(Delphi XE2)
  {$IFEND}
{$ENDIF}

{$IFNDEF HAS_BUILTIN_RETURNADDRESS}
function ReturnAddress: Pointer; {$IFNDEF CLR} assembler; {$ENDIF}
{$ENDIF}
function CallerAddr: Pointer; {$IFNDEF CLR} assembler; {$ENDIF} 
  {$IFDEF HAS_BUILTIN_RETURNADDRESS}deprecated 'Use ReturnAddress';{$ENDIF}
function PtrToStr(p: Pointer): string;
function PointerToLocationInfo(Addrs: Pointer): string;
function PointerToAddressInfo(Addrs: Pointer):  string;
function IsTestMethod(aTest: ITest): Boolean;
function IsDecorator(aTest: ITest): Boolean;
function GetDUnitRegistryKey: string;
procedure SetDUnitRegistryKey(const NewKey: string);
{$IFNDEF CLR}  // - unsuitable for .NET, pointer magic
function FirstByteDiff(p1, p2: pointer; size: longword; out b1, b2: byte): integer;
{$ENDIF}

function MemLeakMonitor: IMemLeakMonitor;

//  strings, used in TAbstractTestCase.EqualsErrorMessage etc.:
const sExpButWasFmt    = '%sexpected: <%s> but was: <%s>';
      sExpAndActualFmt = '%sexpected and actual were: <%s>';

{$UNDEF DETECTMEMLEAKS}
{$IFDEF FASTMM}
  {$DEFINE DETECTMEMLEAKS} // Only check for memory leaks if FASTMM is specifically enabled
{$ENDIF}
///////////////////////////////////////////////////////////////////////////
implementation

uses
{$IFDEF GENERICS}
{$IFDEF MSWINDOWS_OR_POSIX}
  Rtti, Generics.Defaults,
{$ENDIF MSWINDOWS_OR_POSIX}
{$IFDEF CLR}
  System.Collections.Generic,
{$ENDIF CLR}
{$ENDIF GENERICS}
{$IFDEF USE_JEDI_JCL}
  JclDebug,
{$ENDIF}
{$IFDEF DETECTMEMLEAKS}
  FastMMMemLeakMonitor,
{$ENDIF}
{$IFDEF madExcept}
  madStackTrace,
{$ENDIF}
{$IFDEF LINUX}
  Libc;
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysTime;
{$ENDIF}
{$IFDEF MSWINDOWS_OR_CLR}
  Windows,
  Registry;
{$ENDIF}

{$STACKFRAMES ON} // Required to retrieve caller's address

{$IFNDEF DETECTMEMLEAKS}
// Stub code for use when Memory Leak detection is disabled.
// See FastMMMemLeakMonitor.pas notes for invoking memory leak detection.

type
  TMemLeakMonitor = class(TInterfacedObject, IMemLeakMonitor)
  private
    function MemLeakDetected(out LeakSize: Integer): Boolean; overload;
  end;

{ TMemMonitor }

function TMemLeakMonitor.MemLeakDetected(out LeakSize: Integer): Boolean;
begin
  LeakSize := 0;
  Result := False;
end;

type
  TDUnitMemLeakMonitor = class(TMemLeakMonitor, IDUnitMemLeakMonitor)
    function MemLeakDetected(const AllowedLeakSize: Integer;
                             const FailOnMemoryRecovery: Boolean;
                             out   LeakSize: Integer): Boolean; overload;
    function MemLeakDetected(const AllowedValuesGetter: TListIterator;
                             const FailOnMemoryRecovery: Boolean;
                             out   LeakIndex: integer;
                             out   LeakSize: Integer): Boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: Boolean;
                             const TestProcChangedMem: Integer;
                             out   ErrorMsg: string): Boolean; overload;
    function GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                             const TestSetupChangedMem: Integer;
                             const TestProcChangedMem: Integer;
                             const TestTearDownChangedMem: Integer;
                             const TestCaseChangedMem: Integer;
                             out   ErrorMsg: string): boolean; overload;
    procedure MarkMemInUse;
  end;

{ TDUnitMemMonitor }

procedure TDUnitMemLeakMonitor.MarkMemInUse;
begin
// Do Nothing
end;

function TDUnitMemLeakMonitor.MemLeakDetected(const AllowedLeakSize: Integer;
                                              const FailOnMemoryRecovery: Boolean;
                                              out   LeakSize: Integer): Boolean;
begin
  inherited MemLeakDetected(LeakSize);
  Result := (AllowedLeakSize <> 0);
end;

function TDUnitMemLeakMonitor.MemLeakDetected(const AllowedValuesGetter: TListIterator;
                                              const FailOnMemoryRecovery: Boolean;
                                              out   LeakIndex: integer;
                                              out   LeakSize: Integer): Boolean;
begin
  inherited MemLeakDetected(LeakSize);
  LeakIndex := 0;
  Result := (AllowedValuesGetter <> 0);
end;

function TDUnitMemLeakMonitor.GetMemoryUseMsg(const FailOnMemoryRecovery: Boolean;
                                              const TestProcChangedMem: Integer;
                                              out   ErrorMsg: string): Boolean;
begin
  ErrorMsg := '';
  Result := True;
end;

function TDUnitMemLeakMonitor.GetMemoryUseMsg(const FailOnMemoryRecovery: boolean;
                                              const TestSetupChangedMem: Integer;
                                              const TestProcChangedMem: Integer;
                                              const TestTearDownChangedMem: Integer;
                                              const TestCaseChangedMem: Integer;
                                              out   ErrorMsg: string): boolean;
begin
  ErrorMsg := '';
  Result := True;
end;
{$ENDIF}

function MemLeakMonitor: IMemLeakMonitor;
begin
  Result := TMemLeakMonitor.Create;
end;

type
  TMemIniFileTrimmed = class(TMemIniFile)
  public
    // Override the read string method to trim the string for compatibility with TIniFile
    function ReadString(const Section, Ident, DefaultStr: string): string; override;
  end;

var
  // SubKey of HKEY_CURRENT_USER for storing configurations in the registry (end with \)
  DUnitRegistryKey: string = ''; // How about 'Software\DUnitTests\';

{$IFNDEF MSWINDOWS_OR_CLR}

var
  PerformanceCounterInitValue: Int64;

procedure InitPerformanceCounter;
var
  TV : timeval;
begin
  gettimeofday(TV, nil);
  PerformanceCounterInitValue :=
    LongWord(TV.tv_sec mod (24*60*60) * 1000) + (LongWord(TV.tv_usec) div 1000);
end;

function QueryPerformanceCounter(var PerformanceCounter: Int64): LongBool;
var
  TV : timeval;
begin
  gettimeofday(TV, nil);
  PerformanceCounter := (TV.tv_sec mod (24*60*60) * 1000) +
            (TV.tv_usec div 1000);
  PerformanceCounter := PerformanceCounter - PerformanceCounterInitValue;
  Result := true;
end;

function QueryPerformanceFrequency(var Frequency: Int64): LongBool;
begin
  Frequency := 1000;
  Result := true;
end;
{$ENDIF MSWINDOWS_OR_CLR}

{: Convert a pointer into its string representation }
function PtrToStr(p: Pointer): string;
begin
   Result := Format('%p', [p])
end;

function IsBadPointer(P: Pointer):Boolean; {$IFNDEF CLR} register; {$ENDIF}
begin
  try
    Result  := (p = nil)
{$IFNDEF CLR}
              or ((Pointer(P^) <> P) and (Pointer(P^) = P));
{$ENDIF}
  except
    Result := true;
  end
end;

                                                                                                 
{$IFNDEF HAS_BUILTIN_RETURNADDRESS}
function ReturnAddress: Pointer; {$IFNDEF PUREPASCAL} assembler; {$ENDIF}
{$IF defined(CPUX86) and defined(MSWINDOWS) }
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ELSE}
begin
  Result := nil;
end;
{$IFEND}
{$ENDIF HAS_BUILTIN_RETURNADDRESS}

function CallerAddr: Pointer; {$IFNDEF PUREPASCAL} assembler; {$ENDIF}
{$IF defined(CPUX86) and defined(MSWINDOWS) }
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ELSE}
begin
  Result := nil;
end;
{$IFEND}

{$IFNDEF USE_JEDI_JCL}

{$IFNDEF madExcept}
function PointerToLocationInfo(Addrs: Pointer): string;
begin
 Result := ''
end;

function PointerToAddressInfo(Addrs: Pointer): string;
begin
  if Assigned(Addrs) then
    Result := '$'+PtrToStr(Addrs)
  else
    Result := 'n/a';
end;
{$ELSE}
function PointerToLocationInfo(Addrs: Pointer): string;
begin
  Result := string(StackAddrToStr(Addrs));
end;

function PointerToAddressInfo(Addrs: Pointer): string;
begin
  Result := string(StackAddrToStr(Addrs));
end;
{$ENDIF}
{$ELSE}
function PointerToLocationInfo(Addrs: Pointer): string;
var
  _file,
  _module,
  _proc: string;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);

  if _file <> '' then
    Result   := Format('%s:%d', [_file, _line])
  else
    Result   := _module;
end;

function PointerToAddressInfo(Addrs: Pointer): string;
var
  _file,
  _module,
  _proc: string;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);
  Result := Format('%s$%p', [_proc, Addrs]);
end;
{$ENDIF}

function IsTestMethod(aTest: ITest): Boolean;
var
  aTestSuite: ITestSuite;
  aTestDecorator: ITestDecorator;
begin
  Assert(Assigned(aTest));

  // Initialize to be sure
  aTestSuite := nil;
  aTestDecorator := nil;

  { The test should be a normal testmethod
    when the testcount = 1 }
  Result := (aTest.CountTestCases = 1);

  // But not when the test is a suite? (It could have one test.)
{$IFDEF CLR}
  if Supports(aTest, ITestSuite) or Supports(aTest, ITestDecorator) then
    Result := false;
{$ELSE}
  aTest.QueryInterface(ITestSuite, aTestSuite);
  if Assigned(aTestSuite) then
    Result := false;

  // And not when the test is a decorator?
  aTest.QueryInterface(ITestDecorator, aTestDecorator);
  if Assigned(aTestDecorator) then
    Result := false;
{$ENDIF}
end;

function IsDecorator(aTest: ITest): Boolean;
var
  aTestDecorator: ITestDecorator;
begin
  Assert(Assigned(aTest));

  // Initialize to be sure
  aTestDecorator := nil;

{$IFDEF CLR}
  Result := Supports(aTest, ItestDecorator);
{$ELSE}
  aTest.QueryInterface(ITestDecorator, aTestDecorator);
  Result := Assigned(aTestDecorator);
{$ENDIF}
end;

function GetDUnitRegistryKey: string;
begin
  Result := DUnitRegistryKey;
end;

procedure SetDUnitRegistryKey(const NewKey: string);
begin
  DUnitRegistryKey := NewKey;
end;

{$IFNDEF CLR} // KGS: not expected to work in .NET, pointer magic follows
function ByteAt(p: pointer; const Offset: integer): byte;
begin
  Result:=pByte(NativeInt(p)+Offset)^;
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
{$ENDIF}


{ TTestResult }

constructor TTestResult.Create;
begin
  inherited Create;
  FFailures := TList.Create;
  FErrors := TList.Create;
  FListeners := TInterfaceList.Create;
  FRunTests := 0;
  FStop := false;
  FStatusAdapter := TStatusToResultAdapter.Create(Self);
end;

destructor TTestResult.destroy;
var
  i: Integer;
begin
  for i := 0 to fErrors.Count - 1 do
  begin
    TTestFailure(fErrors[i]).Free;
  end;
  FErrors.Free;
  for i := 0 to fFailures.Count - 1 do
  begin
    TTestFailure(fFailures[i]).Free;
  end;
  FFailures.Free;
  inherited;
end;

procedure TTestResult.AddSuccess(test: ITest);
var
  i: integer;
begin
  assert(assigned(test));
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddSuccess(test);
  end;
end;

function TTestResult.AddError(test: ITest; e: Exception; Addrs: Pointer; msg: string): TTestFailure;
var
  i: integer;
  TestFailureError:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fErrors));

  TestFailureError := TTestFailure.Create(test, e, Addrs, msg);
  FErrors.add(TestFailureError);
  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).AddError(TestFailureError);
  end;

  assert(assigned(TestFailureError));
  Result := TestFailureError;
end;

function TTestResult.AddFailure(test: ITest; e: Exception; Addrs: Pointer): TTestFailure;
var
  i: integer;
  Failure:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fFailures));

  Failure := TTestFailure.Create(test, e, Addrs);
  FFailures.add(Failure);
  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).AddFailure(Failure);
  end;

  assert(assigned(Failure));
  Result := Failure;
end;

procedure TTestResult.addListener(listener: ITestListener);
begin
  assert(assigned(listener), sNilListener);
  FListeners.add(listener);
end;

procedure TTestResult.EndTest(test: ITest);
var
  i: integer;
begin
  assert(assigned(FListeners));

  try
    for i := 0 to FListeners.count - 1 do
    begin
      (FListeners[i] as ITestListener).EndTest(test);
    end;
  finally
    test.SetStatusListener(nil);
  end;
end;

procedure TTestResult.Status(test: ITest; const Msg: string);
var
  i: integer;
begin
  assert(assigned(FListeners));

  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).Status(test, Msg);
  end;
end;

function TTestResult.GetError(idx :Integer): TTestFailure;
begin
  Result := TObject(FErrors[idx]) as TTestFailure;
end;

function TTestResult.GetFailure(idx :Integer): TTestFailure;
begin
  Result := TObject(FFailures[idx]) as TTestFailure;
end;

function TTestResult.RunTestSetup(test: ITest):Boolean;
var
  LTime :Int64;
begin
  Result := false;
  try
    test.StopTime := 0;
    QueryPerformanceCounter(LTime);
    test.StartTime := LTime;
    test.FailsOnNoChecksExecuted := FFailsIfNoChecksExecuted;
    test.FailsOnMemoryLeak := FFailsIfMemoryLeaked;
    test.IgnoreSetUpTearDownLeaks:= FIgnoresMemoryLeakInSetUpTearDown;
    test.SetUp;
    Result := true;
  except
    on e: Exception do
    begin
      AddError(test, e, ExceptAddr, sFailedSetup);
    end;
  end;
end;

procedure TTestResult.RunTestTearDown(test: ITest);
var
  LTime :Int64;
begin
  try
    test.TearDown;
  except
    on e: Exception do
      AddError(test, e, ExceptAddr, sFailedTearDown);
  end;
  QueryPerformanceCounter(LTime);
  test.StopTime := LTime;
end;

function TTestResult.RunTestRun(test: ITest) : Boolean;
var
  failure: TTestFailure;
begin
  Result := false;
  test.TestMethodInvoked := False;
  failure := nil;
  try
    test.RunTest(self);
    if not Assigned(FRootTest) then
      FRootTest := test;
    FTotalTime := FRootTest.ElapsedTestTime;
    Result := true;
  except
    on e: EStopTestsFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
      FStop := True;
    end;
    on e: ETestFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
    end;
    on e: EBreakingTestFailure do
    begin
      failure := AddFailure(test, e, ExceptAddr);
    end;
    on e: Exception do
    begin
      failure := AddError(test, e, ExceptAddr);
    end;
  end;
  if BreakOnFailures
  and (failure <> nil)
  and (failure.FThrownExceptionClass.InheritsFrom(ETestFailure))
  then
  begin
    try
       raise EBreakingTestFailure.Create(failure.ThrownExceptionMessage)
          {$IFNDEF CLR}at failure.ThrownExceptionAddress{$ENDIF};
    except
    end;
  end;
end;

procedure TTestResult.Run(test: ITest);
var
  TestProcExecuted: Boolean;
  TestCaseMemLeakMonitor : IDUnitMemLeakMonitor;
  TestProcMemLeakMonitor : IDUnitMemLeakMonitor;
  TestProcMemdiff        : Integer;
  TestCaseMemdiff        : Integer;
  TestCasePassed         : Boolean;
  ErrorMessage           : string;
  MemImbalance           : Boolean;
  SetupMemDiff           : Integer;
  TearDownMemDiff        : Integer;
  LeakIndex              : Integer;
begin
  assert(assigned(test));
  if not ShouldStop and ShouldRunTest(test) then
  begin
    StartTest(test);
    test.AllowedMemoryLeakSize := 0;
    try
      TestProcExecuted := False;
      TestCasePassed := False;
      TestCaseMemdiff := 0;
      TestProcMemdiff := 0;
      ErrorMessage := '';

      // Start monitoring memory allocation before Setup.
      TestProcMemLeakMonitor := TDUnitMemLeakMonitor.Create;
      TestCaseMemLeakMonitor := TDUnitMemLeakMonitor.Create;
      if RunTestSetUp(test) then
      begin
        // See if Setup Leaked.
        (TestCaseMemLeakMonitor as IMemLeakMonitor).MemLeakDetected(SetupMemDiff);
        {$IFDEF USE_JEDI_JCL}
        { JclClearGlobalStackData is called before and after the test to make sure
          that global stack data, which was generated by a trapped exception, wont
          be detected as a memory leak. }
        JclClearGlobalStackData; // requires JCL >= 2.2
        {$ENDIF}
        TestProcMemLeakMonitor.MarkMemInUse;
        TestCasePassed := RunTestRun(test);
        {$IFDEF USE_JEDI_JCL}
        JclClearGlobalStackData; // requires JCL >= 2.2
        {$ENDIF}
        //Not all calls to RunTestRun call Test Procedures so check if this was.
        TestProcExecuted := isTestMethod(test);

        if TestProcExecuted and test.FailsOnMemoryLeak then
          (TestProcMemLeakMonitor as IMemLeakMonitor).MemLeakDetected(TestProcMemdiff);
      end;
      TestProcMemLeakMonitor.MarkMemInUse;
      RunTestTearDown(test);
      (TestProcMemLeakMonitor as IMemLeakMonitor).MemLeakDetected(TearDownMemDiff);

      // Reporting of test success is delayed from within RunTestRun so mem leak
      // can be flagged as failure at testcase level encompasing all of
      // SetUp, Run and TearDown.

      if TestCasePassed then
      begin
        if not TestProcExecuted or not test.FailsOnMemoryLeak then
          // Show non Test Procedure call success or TestCase success if not
          // showing failure on memory status change
          AddSuccess(test)
        else
        begin
          MemImbalance := TestCaseMemLeakMonitor.MemLeakDetected(test.AllowedLeaksIterator,
                                                                 test.FailsOnMemoryRecovery,
                                                                 LeakIndex,
                                                                 TestCaseMemdiff);
          FMemoryLeakIgnoredInSetupOrTearDown := IgnoresMemoryLeakInSetUpTearDown and
            (TestProcMemdiff = 0) and MemImbalance;
          if not FMemoryLeakIgnoredInSetupOrTearDown and (MemImbalance and
            (not TestCaseMemLeakMonitor.GetMemoryUseMsg(test.FailsOnMemoryRecovery,
                                                        SetupMemDiff,
                                                        TestProcMemdiff,
                                                        TearDownMemDiff,
                                                        TestCaseMemdiff,
                                                        ErrorMessage))) then
            AddFailure(test, FMethodPtr, ErrorMessage)
          else
          begin
            AddSuccess(test);
            //Report back allowed leak size actually used from list of sizes
            if (TestCaseMemdiff <> 0) then
             Test.AllowedMemoryLeakSize := TestCaseMemdiff;
          end;
        end;
      end;

    finally
      EndTest(test);
      TestProcMemLeakMonitor := nil;
      TestCaseMemLeakMonitor := nil;
    end;
  end;
end;

function TTestResult.RunCount: integer;
begin
  Result := FRunTests;
end;

function TTestResult.ShouldStop: Boolean;
begin
  Result := FStop;
end;

procedure TTestResult.StartTest(test: ITest);
var
  i: integer;
begin
  assert(assigned(test));
  assert(assigned(FListeners));

  test.SetStatusListener(FStatusAdapter);

  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).StartTest(test);
  end;
end;

procedure TTestResult.Stop;
begin
  FStop := true;
end;

function TTestResult.ErrorCount: integer;
begin
  assert(assigned(FErrors));

  Result := FErrors.count;
end;

function TTestResult.FailureCount: integer;
begin
  assert(assigned(FFailures));

  Result := FFailures.count;
end;

function TTestResult.WasSuccessful: Boolean;
begin
  Result := (FailureCount = 0) and (ErrorCount() = 0) and not WasStopped;
end;

procedure TTestResult.TestingStarts;
var
  i: Integer;
begin
  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).TestingStarts;
  end;
end;

procedure TTestResult.TestingEnds;
var
  i: Integer;
begin
  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).TestingEnds(self);
  end;
end;

function TTestResult.ShouldRunTest(test: ITest): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FListeners.count - 1 do
  begin
    if not (FListeners[i] as ITestListener).ShouldRunTest(test) then
    begin
      Result := false;
      break;
    end;
  end;
end;


function TTestResult.WasStopped: Boolean;
begin
  Result := FStop;
end;

procedure TTestResult.RunSuite(test: ITest);
begin
  TestingStarts;
  try
  {$IFDEF USE_JEDI_JCL}
  try
    JclStartExceptionTracking;
  {$ENDIF}
    FRootTest := test;
    test.RunWithFixture(self);
  {$IFDEF USE_JEDI_JCL}
  finally
    JclStopExceptionTracking;
  end;
  {$ENDIF}
  finally
    TestingEnds
  end
end;

procedure TTestResult.EndSuite(suite: ITest);
var
  i: Integer;
  l: ITestListenerX;
begin
  for i := 0 to FListeners.count - 1 do
  begin
{$IFDEF CLR}
    if Supports(FListeners[i], ITestListenerX, l) then
{$ELSE}
    if FListeners[i].QueryInterface(ITestListenerX, l) = 0 then
{$ENDIF}
       l.EndSuite(suite);
  end;
end;

procedure TTestResult.StartSuite(suite: ITest);
var
  i: Integer;
  l: ITestListenerX;
begin
  for i := 0 to FListeners.count - 1 do
  begin
{$IFDEF CLR}
    if Supports(FListeners[i], ITestListenerX, l) then
{$ELSE}
    if FListeners[i].QueryInterface(ITestListenerX, l) = 0 then
{$ENDIF}
      l.StartSuite(suite);
  end;
end;

function TTestResult.AddFailure(test: ITest; Addrs: Pointer; msg :string = ''): TTestFailure;
var
  i: integer;
  Failure:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(FFailures));

  Failure := TTestFailure.Create(test, Addrs, msg);
  FFailures.add(Failure);
  for i := 0 to FListeners.count - 1 do
  begin
    (FListeners[i] as ITestListener).AddFailure(Failure);
  end;
  assert(assigned(Failure));
  Result := Failure;
end;

{ TStatusToResultAdapter }

constructor TStatusToResultAdapter.Create(TestResult: TTestResult);
begin
  Assert(TestResult <> nil, sNonNiltestresult);
  inherited Create;

  FTestResult := TestResult;
end;

procedure TStatusToResultAdapter.Status(Test: ITest; const Msg: string);
begin
  FTestResult.Status(Test, Msg);
end;

{ TAbstractTest }

constructor TAbstractTest.Create(AName: string);
var
  I: Integer;
begin
  inherited Create;
  FTestName := AName;
  FEnabled  := true;
  for I := 0 to Length(FAllowedLeakList) - 1 do    // Iterate
    FAllowedLeakList[I] := 0;
end;

destructor TAbstractTest.Destroy;
begin
  FStatusStrings.Free;
  FStatusListener := nil;
  inherited;
end;

procedure TAbstractTest.Invoke(AMethod: TTestMethod);
begin
  FTestMethodInvoked := False;
end;

procedure TAbstractTest.Run(testResult: TTestResult);
begin
  FailsOnNoChecksExecuted := testResult.FailsIfNoChecksExecuted;
  FailsOnMemoryLeak := testResult.FailsIfMemoryLeaked;
  IgnoreSetUpTearDownLeaks := testResult.IgnoresMemoryLeakInSetUpTearDown;
  testResult.RunSuite(self);
end;

function TAbstractTest.CountEnabledTestCases: integer;
begin
  if GetEnabled then
    Result := 1
  else
    Result := 0
end;

function TAbstractTest.CountTestCases: integer;
begin
  Result := 1;
end;

function TAbstractTest.getEnabled: Boolean;
begin
  Result := FEnabled
end;

function TAbstractTest.GetName: string;
begin
  Result := FTestName
end;

procedure TAbstractTest.LoadConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean);
var
  f: TCustomIniFile;
begin
{$IFDEF MSWINDOWS_OR_CLR}
  if useRegistry then
    f := TRegistryIniFile.Create(DUnitRegistryKey + fileName)
  else
{$ENDIF}
    if useMemIni then
      f := TMemIniFileTrimmed.Create(fileName)
    else
      f := TIniFile.Create(fileName);

  try
    LoadConfiguration(f, 'Tests')
  finally
    f.free
  end
end;

procedure TAbstractTest.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  self.setEnabled(iniFile.readBool(section, self.GetName, True));
end;

procedure TAbstractTest.SaveConfiguration(const fileName: string; const useRegistry, useMemIni: Boolean);
var
  f: TCustomIniFile;
begin
{$IFDEF MSWINDOWS_OR_CLR}
  if useRegistry then
    f := TRegistryIniFile.Create(DUnitRegistryKey + fileName)
  else
{$ENDIF}
    if useMemIni then
      f := TMemIniFileTrimmed.Create(fileName)
    else
      f := TIniFile.Create(fileName);

  try
    SaveConfiguration(f, sTests);
    f.UpdateFile;
  finally
    f.free
  end
end;

procedure TAbstractTest.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  if self.GetEnabled then
    iniFile.deleteKey(section, self.GetName)
  else
    iniFile.writeBool(section, self.GetName, False);
end;

function TAbstractTest.Run: TTestResult;
var
  testResult:  TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.FailsIfNoChecksExecuted := self.FailsOnNoChecksExecuted;
    testResult.FailsIfMemoryLeaked := self.FailsOnMemoryLeak;
    testResult.IgnoresMemoryLeakInSetUpTearDown := IgnoreSetUpTearDownLeaks;
    testResult.RunSuite(self);
  except
    testResult.Free;
    raise;
  end;
  Result := testResult;
end;

procedure TAbstractTest.setEnabled(value: Boolean);
begin
  FEnabled := value;
end;

var
  EmptyTestList: IInterfaceList = nil;

function TAbstractTest.Tests: IInterfaceList;
begin
  if EmptyTestList = nil then
    EmptyTestList := TInterfaceList.Create;
  Result := EmptyTestList;
end;


function TAbstractTest.GetStartTime: Int64;
begin
  Result := FStartTime
end;

procedure TAbstractTest.SetStartTime(Value: Int64);
begin
  FStartTime := Value;
end;

procedure TAbstractTest.SetStopTime(Value: Int64);
begin
  FStopTime := Value;
end;

function TAbstractTest.GetStopTime: Int64;
begin
  Result := FStopTime;
end;

procedure TAbstractTest.SetUp;
begin
 // do nothing
end;

procedure TAbstractTest.TearDown;
begin
  // do nothing
end;

procedure TAbstractTest.RunTest(testResult: TTestResult);
begin
  // do nothing
end;

function TAbstractTest.ElapsedTestTime: Cardinal;
var
  Freq, LTime: Int64;
begin
  // returns TestTime in millisecs
  if fStopTime > 0 then
    LTime := FStopTime
  else if FStartTime > 0 then
    QueryPerformanceCounter(LTime)
  else
    LTime := 0;

  LTime := LTime - FStartTime;

  if QueryPerformanceFrequency(Freq) then
    Result := (1000*LTime) div Freq
  else
    Result := 0;
end;


procedure TAbstractTest.SetStatusListener(Listener: IStatusListener);
begin
  FStatusListener := Listener;
end;

function TAbstractTest.GetStatus: string;
begin
  if FStatusStrings = nil then
    Result := ''
  else
    Result := FStatusStrings.Text;
end;

procedure TAbstractTest.Status(const Msg: string);
begin
  if FStatusStrings = nil then
    FStatusStrings := TStringList.Create;
  FStatusStrings.Add(Msg);
  if FStatusListener <> nil then
    FStatusListener.Status(self, Msg);
end;

procedure TAbstractTest.RunWithFixture(testResult: TTestResult);
begin
  assert(assigned(testResult));
  if testResult.ShouldRunTest(self) then
    testResult.Run(self);
end;

procedure TAbstractTest.Check(condition: Boolean; msg: string);
begin
  FCheckCalled := True;
  if (not condition) then
    Fail(msg, ReturnAddress);
end;

procedure TAbstractTest.CheckTrue(condition: Boolean; msg: string);
begin
  FCheckCalled := True;
  if (not condition) then
    FailNotEquals(BoolToStr(true), BoolToStr(false), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckFalse(condition: Boolean; msg: string);
begin
  FCheckCalled := True;
  if (condition) then
    FailNotEquals(BoolToStr(false), BoolToStr(true), msg, ReturnAddress);
end;


procedure TAbstractTest.Fail(msg: string; ErrorAddrs: Pointer = nil);
begin
{$IFDEF CLR}
  raise ETestFailure.Create(msg);
{$ELSE}
  if ErrorAddrs = nil then
    raise ETestFailure.Create(msg) at ReturnAddress
  else
    raise ETestFailure.Create(msg) at ErrorAddrs;
{$ENDIF}
end;

procedure TAbstractTest.StopTests(msg: string);
begin
  raise EStopTestsFailure.Create(msg);
end;

procedure TAbstractTest.FailNotEquals( expected,
                                       actual   : WideString;
                                       msg      : string = '';
                                       ErrorAddrs: Pointer = nil);
begin
  Fail(notEqualsErrorMessage(expected, actual, msg), ErrorAddrs);
end;

procedure TAbstractTest.FailEquals(       expected,
                                          actual   : WideString;
                                          msg      : string = '';
                                          ErrorAddrs: Pointer = nil);
begin
  Fail(EqualsErrorMessage(expected, actual, msg), ErrorAddrs);
end;

procedure TAbstractTest.FailNotSame( expected,
                                     actual   : WideString;
                                     msg      : string = '';
                                     ErrorAddrs: Pointer = nil);
begin
  Fail(NotSameErrorMessage(expected, actual, msg), ErrorAddrs);
end;

procedure TAbstractTest.CheckEquals( expected,
                                     actual   : extended;
                                     delta    : extended;
                                     msg      : string = '');
const
  Infinity    =  1.0 / 0.0;
begin
  FCheckCalled := True;
  if not ((expected = Infinity) and (actual = Infinity)) then
    if ((expected = Infinity) and (actual <> Infinity)) or
       ((expected <> Infinity) and (actual = Infinity)) or
       (abs(expected-actual) > delta) then
      FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: extended; msg: string);
begin
  CheckEquals(expected, actual, 0, msg);
end;

procedure TAbstractTest.CheckNotNull(obj: IUnknown; msg: string);
begin
  FCheckCalled := True;
  if obj = nil then
    Fail(msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNull(obj: IUnknown; msg: string);
begin
  FCheckCalled := True;
  if obj <>  nil then
    Fail(msg, ReturnAddress);
end;

procedure TAbstractTest.CheckSame(expected, actual: IUnknown; msg: string = '');
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: string; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(expected, actual, msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: ShortString; msg: string = '');
begin
  FCheckCalled := True;
{$IFDEF CLR}
  if string(expected) <> string(actual) then // Avoid warning from DCCIL.
{$ELSE}
  if expected <> actual then
{$ENDIF}
    FailNotEquals(WideString(expected), WideString(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEqualsString(expected, actual: string; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(expected, actual, msg, ReturnAddress);
end;

{$IFNDEF CLR}
{$IFDEF UNICODE}
procedure TAbstractTest.CheckEquals(expected, actual: RawByteString; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(WideString(expected), WideString(actual), msg, ReturnAddress);
end;
{$ELSE}
procedure TAbstractTest.CheckEquals(expected, actual: WideString; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(expected, actual, msg, ReturnAddress);
end;
{$ENDIF}

procedure TAbstractTest.CheckEqualsWideString(expected, actual: WideString; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(expected, actual, msg, ReturnAddress);
end;

function TAbstractTest.GetMemDiffStr(expected, actual: pointer; size:longword; msg:string):string;
var
  db1, db2: byte;
  Offset: integer;
begin
  Offset:=FirstByteDiff(expected,actual,size,db1,db2);
  Result:=NotEqualsErrorMessage(IntToHex(db1,2),IntToHex(db2,2),msg);
  Result:=Result+' at Offset = '+IntToHex(Offset,4)+'h';
end;

procedure TAbstractTest.CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  FCheckCalled := True;
  if not CompareMem(expected, actual, size) then
    Fail(GetMemDiffStr(expected, actual, size, msg), ReturnAddress);
end;
{$ELSE}
procedure TAbstractTest.CheckEquals(expected, actual: AnsiString; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(WideString(expected), WideString(actual), msg, ReturnAddress);
end;
{$ENDIF}

procedure TAbstractTest.CheckNotEquals(expected, actual: string; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(expected, actual, msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: ShortString; msg: string = '');
begin
  FCheckCalled := True;
{$IFDEF CLR}
  if string(expected) = string(actual) then // Avoid warning from DCCIL.
{$ELSE}
  if expected = actual then
{$ENDIF}
    FailEquals(WideString(expected), WideString(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEqualsString(expected, actual: string; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(expected, actual, msg, ReturnAddress);
end;

{$IFNDEF CLR}
{$IFDEF UNICODE}
procedure TAbstractTest.CheckNotEquals(expected, actual: RawByteString; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(WideString(expected), WideString(actual), msg, ReturnAddress);
end;
{$ELSE}
procedure TAbstractTest.CheckNotEquals(const expected, actual: WideString; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(expected, actual, msg, ReturnAddress);
end;
{$ENDIF}

procedure TAbstractTest.CheckNotEqualsWideString(const expected, actual: WideString; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(expected, actual, msg, ReturnAddress);
end;

// Expected not to work under CLR (pointer based) - KGS
procedure TAbstractTest.CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  FCheckCalled := True;
  if CompareMem(expected, actual, size) then
  begin
    if msg <>'' then msg := msg + ', ';
    Fail(sIdenticalContent + msg, ReturnAddress);
  end;
end;
{$ENDIF}

procedure TAbstractTest.CheckEquals(expected, actual: integer; msg: string);
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: Cardinal; msg: string = '');
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: int64; msg: string);
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: uint64; msg: string);
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;


procedure TAbstractTest.CheckNotEquals(expected, actual: integer; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: Cardinal; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: int64; msg: string = '');
begin
  FCheckCalled := True;
  if expected = actual then
    FailEquals(IntToStr(expected), IntToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = '');
begin
  FCheckCalled := True;
  if (abs(expected-actual) <= delta) then
    FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEquals(expected, actual: Boolean; msg: string);
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotEquals(BoolToStr(expected), BoolToStr(actual), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: Boolean; msg: string);
begin
  FCheckCalled := True;
  if (expected = actual) then
    FailEquals(BoolToStr(expected), BoolToStr(actual), msg, ReturnAddress);
end;

{ [KGS] IntToBin: Elected not to add to TestFrameWork interface,
        many people already have a self made version: }
function IntToBin(const value, digits: longword): string;
const
  ALL_32_BIT_0 = '00000000000000000000000000000000';
var
  counter: integer;
  pow:     integer;
begin
  Result := ALL_32_BIT_0;
  SetLength(Result, digits);
  pow := 1 shl (digits - 1);
  if value <> 0 then
  for counter := 0 to digits - 1 do
  begin
    if (value and (pow shr counter)) <> 0 then
      Result[counter+1] := '1';
  end;
end;

procedure TAbstractTest.CheckEqualsBin(expected, actual: longword;
                                       msg: string = ''; digits: integer=32);
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(IntToBin(expected, digits), IntToBin(actual, digits), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEqualsBin(expected, actual: longword;
                                       msg: string = ''; digits: integer=32);
begin
  FCheckCalled := True;
  if (expected = actual) then
    FailEquals(IntToBin(expected, digits), IntToBin(actual, digits), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckEqualsHex(expected, actual: longword;
                                       msg: string = ''; digits: integer=8);
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotEquals(IntToHex(expected, digits), IntToHex(actual, digits), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotEqualsHex(expected, actual: longword;
                                       msg: string = ''; digits: integer=8);
begin
  FCheckCalled := True;
  if (expected = actual) then
    FailEquals(IntToHex(expected, digits), IntToHex(actual, digits), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckSame(expected, actual: TObject; msg: string);
begin
  FCheckCalled := True;
  if (expected <> actual) then
    FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNotNull(obj: TObject; msg: string);
begin
  FCheckCalled := True;
  if obj = nil then
    FailNotSame('object', PtrToStr(Pointer(obj)), msg, ReturnAddress);
end;

procedure TAbstractTest.CheckNull(obj: TObject; msg: string);
begin
  FCheckCalled := True;
  if obj <> nil then
    FailNotSame('nil', PtrToStr(Pointer(obj)), msg, ReturnAddress);
end;

function TAbstractTest.NotEqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
begin
  if (msg <> '') then
    msg := msg + ', ';
  Result := Format( sExpButWasFmt , [msg, expected, actual])
end;

function TAbstractTest.EqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
begin
  if (msg <> '') then
    msg := msg + ', ';
  Result := Format( sExpAndActualFmt, [msg, expected])
end;

function TAbstractTest.NotSameErrorMessage(expected, actual, msg: string): WideString;
begin
  if (msg <> '') then
    msg := msg + ', ';
  Result := Format( sExpButWasFmt, [msg, expected, actual])
end;

function TAbstractTest.BoolToStr(ABool: Boolean): string;
begin
  Result := BooleanIdents[aBool];
end;

procedure TAbstractTest.StartExpectingException(e: ExceptionClass);
begin
  StopExpectingException;
  FExpectedException := e;
end;

procedure TAbstractTest.StopExpectingException(msg :string);
begin
  try
    if FExpectedException <> nil then
    begin
      Fail( Format( sExpectedException,
                                        [FExpectedException.ClassName,
                                        Msg]),
                                        ReturnAddress);
    end;
  finally
    FExpectedException := nil;
  end;
end;

{$IFNDEF CLR}
procedure TAbstractTest.CheckMethodIsNotEmpty(MethodPointer: pointer);
const
  AssemblerRet = $C3;
begin
  if byte(MethodPointer^) = AssemblerRet then
    Fail(sEmptyTest, MethodPointer);
end;
{$ENDIF}

procedure TAbstractTest.CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string);
begin
  FCheckCalled := True;
  try
    Invoke(AMethod);
  except
    on e :Exception do
    begin
      if  not Assigned(AExceptionClass) then
        raise
      else if not e.ClassType.InheritsFrom(AExceptionClass) then
        FailNotEquals(AExceptionClass.ClassName, e.ClassName, msg, ReturnAddress)
      else
        AExceptionClass := nil;
    end;
  end;
  if Assigned(AExceptionClass) then
    FailNotEquals(AExceptionClass.ClassName, sExceptionNothig, msg, ReturnAddress)
end;

procedure TAbstractTest.CheckEquals(expected, actual: TClass; msg: string);
begin
  FCheckCalled := True;
 if expected <> actual then
 begin
   if expected = nil then
     FailNotEquals('nil', actual.ClassName, msg, ReturnAddress)
   else if actual = nil then
     FailNotEquals(expected.ClassName, 'nil', msg, ReturnAddress)
   else
     FailNotEquals(expected.ClassName, actual.ClassName, msg, ReturnAddress)
 end;
end;

procedure TAbstractTest.CheckInherits(expected, actual: TClass; msg: string);
begin
  FCheckCalled := True;
  if expected = nil then
    FailNotEquals('nil', actual.ClassName, msg, ReturnAddress)
  else if actual = nil then
    FailNotEquals(expected.ClassName, 'nil', msg, ReturnAddress)
  else if not actual.InheritsFrom(expected) then
    FailNotEquals(expected.ClassName, actual.ClassName, msg, ReturnAddress)
end;

procedure TAbstractTest.CheckIs(AObject: TObject; AClass: TClass; msg: string);
begin
  FCheckCalled := True;
  Assert(AClass <> nil);
  if AObject = nil then
    FailNotEquals(AClass.ClassName, 'nil', msg, ReturnAddress)
  else if not AObject.ClassType.InheritsFrom(AClass) then
    FailNotEquals(AClass.ClassName, AObject.ClassName, msg, ReturnAddress)
end;

function TAbstractTest.GetGUIObject: TObject;
begin
  Result := FGUIObject;
end;

procedure TAbstractTest.SetGUIObject(const guiObject: TObject);
begin
  FGUIObject := guiObject;
end;

function TAbstractTest.GetFailsOnNoChecksExecuted: Boolean;
begin
  Result := FFailsOnNoChecksExecuted;
end;

procedure TAbstractTest.SetFailsOnNoChecksExecuted(const Value: Boolean);
begin
  FFailsOnNoChecksExecuted := Value;
end;

function TAbstractTest.GetFailsOnMemoryLeak: Boolean;
begin
  Result := FFailsOnMemoryLeak;
end;

procedure TAbstractTest.SetFailsOnMemoryLeak(const Value: Boolean);
begin
  FFailsOnMemoryLeak := Value;
end;

function TAbstractTest.GetTestMethodInvoked: Boolean;
begin
  Result := FTestMethodInvoked;
end;

procedure TAbstractTest.SetTestMethodInvoked(const Value: Boolean);
begin
  FTestMethodInvoked := True;
end;

function TAbstractTest.GetAllowedMemoryLeakSize: Integer;
// Array[0] reserved for property AllowedLeakSize and remainder for values entered by SetAllowedLeakArray
var
  i: Integer;
begin
  Result := FAllowedLeakList[0];
  if (result = 0) then
  begin   // The user may have set the values using SetAllowedLeakArray
    for I := 0 to Length(FAllowedLeakList) - 1 do    // Iterate
    begin
      if FAllowedLeakList[0] <> 0 then
      begin
        result := FAllowedLeakList[i];
        break;
      end;
    end;    // for
  end;
end;

procedure TAbstractTest.SetAllowedMemoryLeakSize(const NewSize: Integer);
begin
  FAllowedLeakList[0] := NewSize;
end;

function TAbstractTest.GetFailsOnMemoryRecovery: Boolean;
begin
  Result := FFailsOnMemoryRecovery;
end;

procedure TAbstractTest.SetFailsOnMemoryRecovery(const Value: Boolean);
begin
  FFailsOnMemoryRecovery := Value;
end;

procedure TAbstractTest.SetAllowedLeakArray(AllowedList: array of Integer);
var
  I: Integer;
begin // Note the 0th element is reserved for old code value.
  if Length(AllowedList) >= Length(FAllowedLeakList) then
    fail( sAllowLeakArrayValues +
      IntToStr(Length(FAllowedLeakList) - 1));
  for I := 1 to Length(FAllowedLeakList) - 1 do
  begin
    if I <= Length(AllowedList) then
      FAllowedLeakList[I] := AllowedList[I-1]
    else
      FAllowedLeakList[I] := 0;
  end;
end;

function  TAbstractTest.GetAllowedLeaksIterator: TListIterator;
begin
  FAllowedLeakListIndex := 0;
  result := GetAllowedLeak;
end;

function TAbstractTest.GetAllowedLeak: Integer;
begin // Auto Iterator
  if FAllowedLeakListIndex >= Length(FAllowedLeakList) then
    Result := 0
  else
  begin
    Result := FAllowedLeakList[FAllowedLeakListIndex];
    Inc(FAllowedLeakListIndex);
  end;
end;

function TAbstractTest.GetIgnoreSetUpTearDownLeaks: Boolean;
begin
  Result := FIgnoreSetUpTearDownLeaks;
end;

procedure TAbstractTest.SetIgnoreSetUpTearDownLeaks(const Value: Boolean);
begin
  FIgnoreSetUpTearDownLeaks := Value;
end;

function TAbstractTest.GetMemoryLeakIgnoredInSetupTearDown: Boolean;
begin
  Result := FMemoryLeakIgnoredInSetupTearDown;
end;

procedure TAbstractTest.SetMemoryLeakIgnoredInSetupTearDown(
  const Value: Boolean);
begin
  FMemoryLeakIgnoredInSetupTearDown := Value;
end;

{ TTestCase }

constructor TTestCase.Create(MethodName: string);
{$IFNDEF CLR}
var
  RunMethod: TMethod;
{$ENDIF}
begin
  assert(length(MethodName) >0);
{$IFNDEF CLR}
  assert(assigned(MethodAddress(MethodName)));
{$ELSE}
  assert(MethodName <> '');
{$ENDIF}

  inherited Create(MethodName);
{$IFDEF CLR}
  FMethod := MethodName;
{$ELSE}
  RunMethod.code := MethodAddress(MethodName);
  RunMethod.Data := self;
  FMethod := TTestMethod(RunMethod);

  assert(assigned(FMethod));
{$ENDIF}
end;

procedure TTestCase.Invoke(AMethod: TTestMethod);
begin
  FTestMethodInvoked := True;
{$IFDEF CLR}
  try
    GetType.InvokeMember(AMethod, BindingFlags.Public or BindingFlags.Instance or BindingFlags.InvokeMethod, nil, Self, nil);
  except
    on E:TargetInvocationException do
      raise E.InnerException;
  end;
{$ELSE}
  AMethod;
{$ENDIF}
end;

procedure TTestCase.RunWithFixture(testResult: TTestResult);
begin
  assert(assigned(testResult));
  FFailsOnNoChecksExecuted := testResult.FailsIfNoChecksExecuted;
  FFailsOnMemoryLeak := testResult.FailsIfMemoryLeaked;
  FIgnoreSetUpTearDownLeaks := testResult.IgnoresMemoryLeakInSetUpTearDown;
  if testResult.ShouldRunTest(self) then
  begin
    inc(testResult.FRunTests);
    inherited;
  end;
end;

procedure TTestCase.RunTest(testResult: TTestResult);
begin
  assert(assigned(FMethod), sMethodNotFound + FTestName + '" ');
  FExpectedException := nil;
  try
    try
{$IFDEF CLR}
      testResult.FMethodPtr := nil;
{$ELSE}
      CheckMethodIsNotEmpty(tMethod(FMethod).Code);
      testResult.FMethodPtr := tMethod(FMethod).Code;
{$ENDIF}
      FCheckCalled := False;
      Invoke(FMethod);
      if FFailsOnNoChecksExecuted and (not FCheckCalled) then
        Fail(sNoChecksExecuted, testResult.FMethodPtr);
      StopExpectingException;
    except
      on E: ETestFailure  do
      begin
        raise;
      end;
      on E: Exception  do
      begin
        if  not Assigned(FExpectedException) then
          raise
        else if not E.ClassType.InheritsFrom(fExpectedException) then
          FailNotEquals(fExpectedException.ClassName, E.ClassName, sExceptionUnexpected, ExceptAddr);
      end;
    end;
  finally
    FExpectedException := nil;
  end;
end;

procedure TTestCase.Run(testResult: TTestResult);
begin
  testResult.RunSuite(self);
end;

class function TTestCase.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(self);
end;

{ TTestFailure }

constructor TTestFailure.Create(FailedTest: ITest; ThrownException: Exception; Addrs: Pointer; msg: string);
begin
  assert(assigned(ThrownException));

  inherited Create;
  FFailedTest := FailedTest;
  FThrownExceptionClass := ThrownException.ClassType;
  FThrownExceptionMessage := msg + ThrownException.message;
  FThrownExceptionAddress := Addrs;

  FStackTrace := CaptureStackTrace(ThrownException, FThrownExceptionAddress);
end;

constructor TTestFailure.Create(FailedTest: ITest; Addrs: Pointer; msg: string);
begin
  inherited Create;
  FFailedTest := FailedTest;
  FThrownExceptionClass := EPostTestFailure;
  FThrownExceptionMessage := msg;
  FThrownExceptionAddress := Addrs;
  FStackTrace := '';
end;

function TTestFailure.FailedTest: ITest;
begin
  Result := FFailedTest;
end;

function TTestFailure.ThrownExceptionName: string;
begin
  Result := FThrownExceptionClass.ClassName;
end;

function TTestFailure.ThrownExceptionMessage: string;
begin
  Result := FThrownExceptionMessage;
end;

function TTestFailure.ThrownExceptionAddress: pointer;
begin
  Result := FThrownExceptionAddress;
end;

function TTestFailure.ThrownExceptionClass: TClass;
begin
  Result := FThrownExceptionClass;
end;

function TTestFailure.LocationInfo: string;
begin
  Result := PointerToLocationInfo(ThrownExceptionAddress);
end;

function TTestFailure.AddressInfo: string;
begin
  Result := PointerToAddressInfo(ThrownExceptionAddress);
end;

function TTestFailure.StackTrace: string;
begin
  Result := FStackTrace;
end;

function TTestFailure.CaptureStackTrace(ThrownException: Exception; ThrownExceptionAddress: Pointer): string;
{$IFDEF USE_JEDI_JCL}
var
  Trace: TStrings;
{$ENDIF}
begin
{$IFDEF CLR}
  Result := thrownException.StackTrace;
{$ELSE}
{$IFDEF USE_JEDI_JCL}
  Trace := TStringList.Create;
  try
    JclDebug.JclLastExceptStackListToStrings(Trace, true);
    Result := Trace.Text;
  finally
    Trace.Free;
  end;
{$ELSE}
{$IFDEF madExcept}
  Result := madStackTrace.StackTrace( false, false, false, nil, nil,
                                           ThrownExceptionAddress, false,
                                           false, 0, 0, nil,
                                           @ThrownExceptionAddress );
{$ELSE}
  Result := '';
{$ENDIF madExcept}
{$ENDIF USE_JEDI_JCL}
{$ENDIF CLR}
end;

{ TTestSuite }

constructor TTestSuite.Create;
begin
  Create(TObject.ClassName);
end;

constructor TTestSuite.Create(AName: string);
begin
  assert(length(AName) > 0);

  inherited Create(AName);

  FTests := TInterfaceList.Create;
end;

constructor TTestSuite.Create( testClass: TTestCaseClass);
begin
  self.Create(testClass.ClassName);
  AddTests(testClass);
end;

constructor TTestSuite.Create(AName: string; const Tests: array of ITest);
var
  i: Integer;
begin
  self.Create(AName);
  for i := Low(Tests) to High(Tests) do begin
    Self.addTest(Tests[i])
  end;
end;

procedure TTestSuite.AddTest(ATest: ITest);
begin
  Assert(Assigned(ATest));

  FTests.Add(ATest);
end;

procedure TTestSuite.AddSuite(suite: ITestSuite);
begin
  AddTest(suite);
end;


procedure TTestSuite.AddTests(testClass: TTestCaseClass);
var
  MethodIter     :  Integer;
  NameOfMethod   :  string;
  MethodEnumerator:  TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount-1 do
      begin
        NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
        self.addTest(testClass.Create(NameOfMethod) as ITest);
      end;
  finally
    MethodEnumerator.free;
  end;
end;

function TTestSuite.CountTestCases: integer;
var
  test: ITest;
  i: Integer;
  Total:  integer;
begin
  assert(assigned(FTests));

  Total := 0;
  for i := 0 to FTests.Count - 1 do
  begin
    test := FTests[i] as ITest;
    Total := Total + test.CountTestCases;
  end;
  Result := Total;
end;

function TTestSuite.CountEnabledTestCases: integer;
var
  i: Integer;
  test: ITest;
  Total:  Integer;
begin
  assert(assigned(FTests));

  Total := 0;
  if getEnabled then
  begin
    for i := 0 to FTests.Count - 1 do
    begin
      test := FTests[i] as ITest;
      Total := Total + test.CountEnabledTestCases;
    end;
  end;
  Result := Total;
end;

procedure TTestSuite.RunTest(testResult: TTestResult);
var
  i: Integer;
  test: ITest;
begin
  assert(assigned(testResult));
  assert(assigned(FTests));

  testResult.StartSuite(self);
  for i := 0 to FTests.Count - 1 do
  begin
    if testResult.ShouldStop then
      BREAK;
    test := FTests[i] as ITest;
    test.RunWithFixture(testResult);
  end;
  testResult.EndSuite(self);
end;

function TTestSuite.Tests: IInterfaceList;
begin
  Result := FTests;
end;

procedure TTestSuite.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  LTests: IInterfaceList;
  TestSection: string;
begin
  inherited LoadConfiguration(iniFile, section);
  LTests := self.Tests;
  TestSection := section + '.' + self.GetName;
  for i := 0 to LTests.count-1 do
    (LTests[i] as ITest).LoadConfiguration(iniFile, TestSection);
end;

procedure TTestSuite.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  LTests: IInterfaceList;
  TestSection: string;
begin
  inherited SaveConfiguration(iniFile, section);
  LTests := self.Tests;
  TestSection := section + '.' + self.GetName;
  for i := 0 to LTests.count-1 do
    (LTests[i] as ITest).SaveConfiguration(iniFile, TestSection);
end;

{ ETestFailure }

constructor ETestFailure.Create;
begin
   inherited Create('')
end;

constructor ETestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ EBreakingTestFailure }

constructor EBreakingTestFailure.Create;
begin
   inherited Create('')
end;

constructor EBreakingTestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ TMemIniFileTrimmed }

function TMemIniFileTrimmed.ReadString(const Section, Ident,
  DefaultStr: string): string;
begin
  // Trim the result for compatibility with TIniFile
  Result := Trim(inherited ReadString(Section, Ident, DefaultStr));
end;

{ TMethodEnumerator }

constructor TMethodEnumerator.Create(AClass: TClass);
{$IFDEF CLR}
var
  I: integer;
  Methods: array of MethodInfo;

  function IsTest(AMethod: MethodInfo): Boolean;
  var
    CustomAttr: array of System.Object;
  begin
    if AMethod.IsPublic then
    begin
      CustomAttr := AMethod.GetCustomAttributes(typeof(TestAttribute), false);
      Result :=  Length(CustomAttr) > 0;
    end
    else
      Result := false;
  end;

begin
  inherited Create;
  FMethodNameList := StringCollection.Create;
  Methods := AClass.ClassInfo.GetMethods();
  for I := 0 to System.Array(Methods).Length - 1 do
    if IsTest(Methods[I]) then
      FMethodNameList.Add(Methods[I].Name);
end;
{$ELSE}
                                                            
{$IF DEFINED(CPUX64) AND DEFINED(RTTI)}
var
  I: Integer;
  LMethod: TRttiMethod;
begin
  inherited Create;
  if AClass <> nil then
    for LMethod in TRttiContext.Create.GetType(AClass).GetMethods do
      if LMethod.Visibility = mvPublished then
        if LMethod.VirtualIndex >= 0 then
        begin
          I := Low(FMethodNameList);
          while (I <= High(FMethodNameList)) and (LMethod.Name <> FMethodNameList[I]) do
            Inc(I);
          if I > High(FMethodNameList) then
          begin
            SetLength(FMethodNameList, Length(FMethodNameList) + 1);
            FMethodNameList[Length(FMethodNameList) - 1] := LMethod.Name;
          end;
        end
        else
        begin
          SetLength(FMethodNameList, Length(FMethodNameList) + 1);
          FMethodNameList[Length(FMethodNameList) - 1] := LMethod.Name;
        end;
end;
{$ELSE}
type
  TMethodTable = packed record
    count: SmallInt;
  //[...methods...]
  end;
  TMethodEntry = packed record
    Len: Word;
    Code: Pointer;
    Name: ShortString;
   {Optional Data}
  end;
var
  table: ^TMethodTable;
  entry: ^TMethodEntry;
  AName:  ShortString;
  i, j:  Integer;
begin
  inherited Create;
  while aclass <> nil do
  begin
    table := PPointer(PAnsiChar(aclass) + vmtMethodTable)^;
    if table <> nil then
    begin
      entry := Pointer(PAnsiChar(table) + 2);
      for i := 1 to table.count do
      begin
        AName := entry^.Name;
        // check if we've seen the method name
        j := Low(FMethodNameList);
        while (j <= High(FMethodNameList))
        and (string(AName) <> FMethodNameList[j]) do
          inc(j);
        // if we've seen the name, then the method has probably been overridden
        if j > High(FMethodNameList) then
        begin
          SetLength(FMethodNameList,length(FMethodNameList)+1);
          FMethodNameList[j] := string(AName);
        end;
        entry := Pointer(PAnsiChar(entry) + entry^.Len);
      end;
    end;
    aclass := aclass.ClassParent;
  end;
end;
{$IFEND}
{$ENDIF CLR}

function TMethodEnumerator.GetMethodCount: Integer;
begin
{$IFDEF CLR}
  Result := FMethodNameList.Count;
{$ELSE}
  Result := Length(FMethodNameList);
{$ENDIF}
end;

function TMethodEnumerator.GetNameOfMethod(idx: integer): string;
begin
  Result := FMethodNameList[idx];
end;

{ Convenience routines }

function  TestSuite(AName: string; const Tests: array of ITest): ITestSuite;
begin
   Result := TTestSuite.Create(AName, Tests);
end;

{ test registry }

var
  __TestRegistry: ITestSuite = nil;

procedure RegisterTestInSuite(rootSuite: ITestSuite; path: string; test: ITest);
var
  pathRemainder:  string;
  suiteName:  string;
  targetSuite:  ITestSuite;
  suite:  ITestSuite;
  currentTest:  ITest;
  Tests:  IInterfaceList;
  dotPos:  Integer;
  i: Integer;
begin
  if (path = '') then
  begin
    // End any recursion
    rootSuite.addTest(test);
  end
  else
  begin
    // Split the path on the dot (.)
    dotPos := Pos('.', Path);
    if (dotPos <= 0) then dotPos := Pos('\', Path);
    if (dotPos <= 0) then dotPos := Pos('/', Path);
    if (dotPos > 0) then
    begin
      suiteName := Copy(path, 1, dotPos - 1);
      pathRemainder := Copy(path, dotPos + 1, length(path) - dotPos);
    end
    else
    begin
      suiteName := path;
      pathRemainder := '';
    end;
    Tests := rootSuite.Tests;

    // Check to see if the path already exists
    targetSuite := nil;
    Tests := rootSuite.Tests;
    for i := 0 to Tests.count -1 do
    begin
      currentTest := Tests[i] as ITest;
{$IFDEF CLR}
      if Supports(currentTest, ITestSuite, suite) then
{$ELSE}
      currentTest.queryInterface(ITestSuite, suite);
      if Assigned(suite) then
{$ENDIF}
      begin
        if (currentTest.GetName = suiteName) then
        begin
          targetSuite := suite;
          break;
        end;
      end;
    end;

    if not assigned(targetSuite) then
    begin
      targetSuite := TTestSuite.Create(suiteName);
      rootSuite.addTest(targetSuite);
    end;

    RegisterTestInSuite(targetSuite, pathRemainder, test);
  end;
end;

procedure CreateRegistry;
var
  MyName : string;
begin
{$IFDEF CLR}
  MyName := ExtractFileName(ParamStr(0));
{$ELSE}
  SetLength(MyName, 1024);
  GetModuleFileName(hInstance, PChar(MyName), Length(MyName));
  MyName := Trim(PChar(MyName));
  MyName := ExtractFileName(MyName);
{$ENDIF}
  __TestRegistry := TTestSuite.Create(MyName);
end;

procedure RegisterTest(SuitePath: string; test: ITest);
begin
  assert(assigned(test));
  if __TestRegistry = nil then CreateRegistry;
  RegisterTestInSuite(__TestRegistry, SuitePath, test);
end;

procedure RegisterTest(test: ITest);
begin
  RegisterTest('', test);
end;

procedure RegisterTests(SuitePath: string; const Tests: array of ITest);
var
  i: Integer;
begin
  for i := Low(Tests) to High(Tests) do begin
    TestFramework.RegisterTest(SuitePath, Tests[i])
  end
end;

procedure RegisterTests(const Tests: array of ITest);
begin
  RegisterTests('', Tests);
end;

function RegisteredTests: ITestSuite;
begin
  Result := __TestRegistry;
end;

function RunTest(suite: ITest; const listeners: array of ITestListener): TTestResult; overload;
var
  i        : Integer;
begin
  Result := TTestResult.Create;
  for i := low(listeners) to high(listeners) do
      result.addListener(listeners[i]);
  if suite <> nil then
    suite.Run(result);
end;

function RunRegisteredTests(const listeners: array of ITestListener): TTestResult;
begin
  Result := RunTest(RegisteredTests, listeners);
end;

procedure ClearRegistry;
begin
  __TestRegistry := nil;
end;

{$IFDEF GENERICS}

{ TConverter<T> }

class function TConverter<T>.ValueToString(Info: PTypeInfo; Size: Cardinal;
  Value: TValueData): string;
{$IF DEFINED(MSWINDOWS) OR DEFINED(POSIX)}
var
  I, MinValue: Integer;
  LType: TRttiType;
  LContext: TRttiContext;
  LField: TRttiField;
  LValue: TValue;
  LTypeData: PTypeData;
  Buffer: Pointer;
  Fmt: string;
begin
  if Info = nil then
    Exit(sUnsupportedTypeInfo);

  case Info.Kind of
    tkInteger:
      case GetTypeData(Info).OrdType of
        otSByte, otUByte: Result := IntToStr(PByte(Value)^);
        otSWord, otUWord: Result := IntToStr(PWord(Value)^);
        otSLong, otULong: Result := IntToStr(PInteger(Value)^);
      end;
    tkInt64: Result := IntToStr(PInt64(Value)^);
    tkPointer,
    tkInterface:
      Result := '$' + IntToHex(NativeInt(PPointer(Value)^), SizeOf(Pointer) * 2);
    tkString: Result := string(PShortString(Value)^);
    tkLString: Result := string(PAnsiString(Value)^);
    tkUString: Result := PUnicodeString(Value)^;
    tkWString: Result := string(PWideString(Value)^);
    tkChar: Result := Char(PAnsiChar(Value)^);
    tkWChar: Result := PWideChar(Value)^;
    tkEnumeration: Result := GetEnumName(Info, PByte(Value)^);
    tkFloat:
      case GetTypeData(Info).FloatType of
        ftSingle: Result := FloatToStr(PSingle(Value)^);
        ftDouble: Result := FloatToStr(PDouble(Value)^);
        ftExtended: Result := FloatToStr(PExtended(Value)^);
        ftComp: Result := FloatToStr(PComp(Value)^);
        ftCurr: Result := FloatToStr(PCurrency(Value)^);
      end;
    tkSet:
      begin
        I := 0;
        Move(Value^, I, Size);
        Result := SetToString(Info, I, True);
      end;
    tkClass: Result := '$' + IntToHex(NativeInt(PPointer(Value)^), SizeOf(Pointer) * 2) +
      ' [' + PObject(Value)^.ClassName + ']';
    tkVariant: Result := PVariant(Value)^;
    tkDynArray:
      begin
        Result := '(';
        LTypeData := GetTypeData(Info);
        for I := 0 to DynArraySize(PPointer(Value)^) - 1 do
        begin
          Result := Format('%s%s;', [Result,
            ValueToString(LTypeData^.elType2^, LTypeData^.elSize,
            Pointer((NativeInt(PPointer(Value)^) + LTypeData^.elSize * I)))]);
        end;
        if Length(Result) > 0 then
          SetLength(Result, Length(Result) - 1);
        Result := Result + ')';
      end;
    tkRecord:
      begin
        Result := '(';
        LType := LContext.GetType(Info);
        if LType <> nil then
        begin
          for LField in LType.AsRecord.GetFields do
          begin
            LValue := LField.GetValue(Value);
            case LValue.Kind of
              tkString, tkLString, tkWString, tkUString: Fmt := '%s%s="%s";'
            else
              Fmt := '%s%s=%s;';
            end;
            Result := Format(Fmt, [Result, LField.Name,
              ValueToString(LValue.TypeInfo, LValue.DataSize, LValue.GetReferenceToRawData)]);
          end;
          if Length(Result) > 0 then
            SetLength(Result, Length(Result) - 1);
          Result := Result + ')';
        end
        else
          Result := string(Info.Name);
      end;
  else
    Result := string(Info.Name);
  end;
end;
{$ELSE IF DEFINED(CLR)}
var
  LField: FieldInfo;
  LValue: TObject;
  Fmt: string;
begin
  if Info = nil then
  begin
    Result := sUnsupportedTypeInfo;
    Exit;
  end;

  case Info.Kind of
    tkInteger,
    tkInt64: Result := IntToStr(Convert.ToInt64(Value));
    tkString: Result := string(ShortString(Value));
    tkLString: Result := string(AnsiString(Value));
    tkWString: Result := string(WideString(Value));
    tkChar: Result := Char(AnsiChar(Value));
    tkWChar: Result := WideChar(Value);
    tkEnumeration: Result := GetEnumName(Info, Byte(Value));
    tkFloat:
      case GetTypeData(Info).FloatType of
        ftSingle: Result := FloatToStr(Single(Value));
        ftDouble: Result := FloatToStr(Double(Value));
        ftExtended: Result := FloatToStr(Extended(Value));
        ftComp: Result := FloatToStr(Comp(Value));
        ftCurr: Result := FloatToStr(Currency(Value));
      end;
    tkSet: Result := GetSetNames(Info, Convert.ToInt32(Value), True);
    tkClass,
    tkInterface:
      begin
        try
          Result := '$' + IntToHex(Convert.ToInt64(Value), IntPtr.Size * 2) +
            ' [' + Value.ClassName + ']';
        except
          Result := 'HashCode = $' + IntToHex(Value.GetHashCode, IntPtr.Size * 2) +
            ' [' + Value.ClassName + ']';
        end;
      end;
    tkRecord:
      begin
        Result := '(';
        begin
          for LField in Info.GetFields do
          begin
            if System.Type.GetTypeCode(LField.FieldType) = TypeCode.String then
              Fmt := '%s%s="%s";'
            else
              Fmt := '%s%s=%s;';
            LValue := LField.GetValue(Value);
            if LValue <> nil then
              Result := Format(Fmt, [Result, LField.Name,
                ValueToString(LValue.GetType, SizeOf(LValue.GetType), LValue)])
            else
              Result := Format(Fmt, [Result, LField.Name, sUnkownFieldType])
          end;
          if Length(Result) > 0 then
            SetLength(Result, Length(Result) - 1);

          Result := Result + ')';
        end
      end;
  else
    Result := string(Info.Name);
  end;
end;
{$IFEND}

class function TConverter<T>.ToString(Value: T): string;
begin
  Result := ValueToString(TypeInfo(T), SizeOf(T), {$IFNDEF CLR}@{$ENDIF}Value);
end;

{ TGenericTestCase }

{$IFDEF CLR}
type
  TComparer<T> = class(Comparer<T>);
{$ENDIF CLR}

class function TGenericTestCase.Compare<T>(const Expected, Actual: T): Integer;
var
  FComparer: IComparer<T>;
begin
  FComparer := TComparer<T>.Default;

{$IF DEFINED(MSWINDOWS) OR DEFINED(POSIX)}
  Result := FComparer.Compare(Expected, Actual);
{$ELSE IF DEFINED(CLR)}
  try
    Result := FComparer.Compare(Expected, Actual);
  except
    on E: ArgumentException do
    begin
      if Expected.Equals(Actual) then
        Result := 0
      else
        Result := 1;
    end;
  end;
{$IFEND}
end;

procedure TGenericTestCase.CheckEquals<T>(Expected, Actual: T; Msg: string = '');
begin
  FCheckCalled := True;
  if Compare<T>(Expected, Actual) <> 0 then
    FailNotEquals<T>(Expected, Actual, Msg);
end;

procedure TGenericTestCase.CheckNotEquals<T>(Expected, Actual: T; Msg: string = '');
begin
  FCheckCalled := True;
  if Compare<T>(Expected, Actual) = 0 then
    FailEquals<T>(Expected, Actual, Msg);
end;

procedure TGenericTestCase.FailEquals<T>(Expected, Actual: T; Msg: string;
  ErrorAddrs: Pointer);
begin
  Fail(EqualsErrorMessage<T>(Expected, Actual, Msg), ErrorAddrs);
end;

procedure TGenericTestCase.FailNotEquals<T>(Expected, Actual: T; Msg: string;
  ErrorAddrs: Pointer);
begin
  Fail(NotEqualsErrorMessage<T>(Expected, Actual, Msg), ErrorAddrs);
end;

function TGenericTestCase.EqualsErrorMessage<T>(Expected, Actual: T;
  Msg: string): string;
begin
  if (Msg <> '') then
    Msg := Msg + ', ';
  Result := Format(sExpAndActualFmt, [Msg, TConverter<T>.ToString(Expected)])
end;

function TGenericTestCase.NotEqualsErrorMessage<T>(Expected, Actual: T;
  Msg: string): string;
begin
  if (Msg <> '') then
    Msg := Msg + ', ';
  Result := Format(sExpButWasFmt ,
    [Msg, TConverter<T>.ToString(Expected), TConverter<T>.ToString(Actual)]);
end;

{$ENDIF GENERICS}

initialization
{$IFDEF LINUX}
  InitPerformanceCounter;
{$ENDIF}
finalization
  ClearRegistry;
end.
