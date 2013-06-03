
unit HeidiTestRunner;

interface
uses
  SysUtils,
  Classes,
  XMLTestRunner,
  TestFramework,
  TestLogMgr,
  WebReport,
  BDSUtils;

type
  THeidiTestListener = class(TXMLTestListener)
  private
    function MakeUTCTime(DateTime: TDateTime): TDateTime;
  public
    procedure TestingEnds(testResult: TTestResult); override;
  end;

  TFileLoadFormater = class(TLogFormatter)
  public
    function FormatOutput(T: TEventType; const Desc, Expect, Recv: string; const Expected: Boolean=true): string; override;
  end;

  TDUnitTestManager = record
    Author: String;
    TestingArea: String;
    Context: String;
  end;

  HeidiReportException = class(Exception);

{: Run the given test suite
}
function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;
function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;

var
  TestManager: TDUnitTestManager;

implementation

uses MSXML, ActiveX, Windows;

{ THeidiTestListener }

function THeidiTestListener.MakeUTCTime(DateTime: TDateTime): TDateTime;
var
  TZI: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD:
      Result := DateTime + (TZI.Bias/60/24);
    TIME_ZONE_ID_DAYLIGHT:
      Result := DateTime + (TZI.Bias/60/24) + TZI.DaylightBias;
    else
      Result := DateTime;
  end;
end;

procedure THeidiTestListener.TestingEnds(testResult: TTestResult);
var
  LogManager: TTestLogManager;
  aHandler : TLogHandler;
begin
  inherited;

  LogManager := TTestLogManager.Create(nil);
  try
    LogManager.Handlers[0].Active := False;

    LogManager.Product.Sku := BDSUtils.GetSKU;
    LogManager.Product.Build := BDSUtils.GetVersion;
    LogManager.Machine.OSVersion := BDSUtils.GetOSVersion;
    LogManager.Machine.Locale := BDSUtils.GetBDSLocale;

    LogManager.Suite.TestingArea := TestManager.TestingArea;
    LogManager.Suite.Name        := ExtractFileName(ParamStr(0));
    LogManager.Suite.Author      := TestManager.Author;
    LogManager.Suite.LogFile     := FileName;
    LogManager.Suite.Context     := TestManager.Context;

    with LogManager.Stats do
    begin
      Failed  := testResult.failureCount;
      Aborted := testResult.errorCount;
      Passed  := testResult.runCount - Failed - Aborted;
      Ended   := MakeUTCTime(now);
      Started := MakeUTCTime(Self.StartTime);
    end;

    aHandler := LogManager.Handlers.Add(TWebServiceLogHandler);
    aHandler.Formatter := LogManager.Formatters.Add(TFileLoadFormater);

    try
      LogManager.BeginLogging;
      LogManager.EndLogging;
    except
      on e: exception do
        raise HeidiReportException.CreateFmt('Failed to report test result to Heidi: %s', [e.Message]);
    end;
  finally
    LogManager.Free;
  end;

end;

function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [THeidiTestListener.Create(outputFile)]);
end;

function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := HeidiTestRunner.RunTest(registeredTests, outputFile);
end;


{ TFileLoadFormater }

function TFileLoadFormater.FormatOutput(T: TEventType; const Desc, Expect,
  Recv: string; const Expected: Boolean): string;
var
  fileContents : TStringList;
begin
  if FileExists(Desc) then
  begin
    fileContents := TStringList.Create;
    try
      fileContents.LoadFromFile(Desc);
      Result := fileContents.Text;
    finally
      fileContents.Free;
    end;
  end;
end;


initialization

  TestManager.Author      := 'Unknown Author';
  TestManager.TestingArea := 'Unknown DUnit';
  TestManager.Context     := '';
end.
