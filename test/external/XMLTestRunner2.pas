// This file was found at http://cc.embarcadero.com/Item/28239

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
 * and Juancarlo Aez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Aez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{
 Contributor : Laurent Laffont <llaffont@altaiire.fr>
                2011-02-23, Mark Pickersgill: Updated report output to be more like NUnit reports.
}

unit XMLTestRunner2;

interface
uses
  SysUtils,
  Classes,
  TestFramework;

const
   DEFAULT_FILENAME = 'dunit-report.xml';

type
  TXMLTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  private
     FOutputFile : TextFile;
     FFileName : String;
     
  protected
     startTime : Cardinal;
     dtStartTime : TDateTime;

     testStart : TDateTime;
     FSuiteStack : TStringList;
     
     procedure writeReport(str: String);

     function GetCurrentSuiteName : string;
     function GetCurrentCaseName: string;
  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    constructor Create; overload;
    constructor Create(outputFile : String); overload;
    destructor Destroy; override;
    
    class function RunTest(suite: ITest; outputFile:String): TTestResult; overload;
    class function RunRegisteredTests(outputFile:String): TTestResult;
    class function text2sgml(text : String) : String;
    class function StringReplaceAll (text,byt,mot : string ) :string;
    
    //:Report filename. If an empty string, then standard output is used (compile with -CC option)
    property FileName : String read FFileName write FFileName;
  end;

{: Run the given test suite
}
function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;
function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;

implementation

uses Forms, Windows;

const
   CRLF = #13#10;
   MAX_DEEP = 5;

{ TXMLTestListener }
   
constructor TXMLTestListener.Create;
begin
   Create(DEFAULT_FILENAME);
end;

constructor TXMLTestListener.Create(outputFile : String);
begin
   inherited Create;
   FileName := outputFile;
   FSuiteStack := TStringList.Create;
end;

{:
 Write F in the report file or on standard output if none specified
}
procedure TXMLTestListener.writeReport(str : String);
begin
   if TTextRec(FOutputFile).Mode = fmOutput then begin
      writeln(FOutputFile, str);
      Flush(FOutputFile)
   end else
      writeln(str);
end;

const
  TrueFalse : array[Boolean] of string = ('False', 'True');

procedure TXMLTestListener.AddSuccess(test: ITest);
begin
   if test.tests.Count<=0 then
   begin
      writeReport(Format('    <test-case name="%s%s" executed="%s" success="True" time="%1.3f" result="Pass"/>',
                         [GetCurrentCaseName, test.GetName, TrueFalse[test.Enabled], test.ElapsedTestTime / 1000]));
   end;
end;

procedure TXMLTestListener.AddError(error: TTestFailure);
begin
   writeReport(Format('    <test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Error">',
                      [GetCurrentCaseName, error.FailedTest.GetName, TrueFalse[error.FailedTest.Enabled], error.FailedTest.ElapsedTestTime / 1000]));
   writeReport('      <failure>');
   writeReport(Format('        <message><![CDATA[Exception: [%s] %s', [error.ThrownExceptionName, error.ThrownExceptionMessage]));
   writeReport(Format('At: %s]]></message>', [text2sgml(error.AddressInfo)]));
   writeReport(Format('        <stack-trace><![CDATA[%s]]></stack-trace>', [error.StackTrace]));
   writeReport('      </failure>');
   writeReport('    </test-case>');
end;

procedure TXMLTestListener.AddFailure(failure: TTestFailure);
begin
   writeReport(Format('    <test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Failure">',
                      [GetCurrentCaseName, failure.FailedTest.GetName, TrueFalse[failure.FailedTest.Enabled], failure.FailedTest.ElapsedTestTime / 1000]));
   writeReport('      <failure>');
   writeReport(Format('        <message><![CDATA[Exception: [%s] %s', [failure.ThrownExceptionName, failure.ThrownExceptionMessage]));
   writeReport(Format('At: %s]]></message>', [text2sgml(failure.AddressInfo)]));
   writeReport(Format('        <stack-trace><![CDATA[%s]]></stack-trace>', [failure.StackTrace]));
   writeReport('      </failure>');
   writeReport('    </test-case>');
end;


procedure TXMLTestListener.StartTest(test: ITest);
begin
  if test.CountTestCases <= 1
  then Write(test.Name + '...');
end;

procedure TXMLTestListener.EndTest(test: ITest);
begin
  if test.CountTestCases <= 1
  then Writeln(' finished');
end;

procedure TXMLTestListener.TestingStarts;
begin
   startTime := GetTickCount;
   dtStartTime := Now;
   
   if FFileName<>'' then
   begin
     AssignFile(FOutputFile, FFileName);
     Rewrite(FOutputFile);
   end;
   
   writeReport('<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>');
   writeReport(Format('<test-results name="%s" total="%d" not-run="%d" date="%s" time="%s">',
                      [RegisteredTests.Name, RegisteredTests.CountTestCases,
                        RegisteredTests.CountTestCases - RegisteredTests.CountEnabledTestCases,
                          DateToStr(Now),
                            TimeToStr(Now)]));
end;

procedure TXMLTestListener.TestingEnds(testResult: TTestResult);
var
   runTime : Double;
   successRate : Integer;
begin
   runtime := (GetTickCount - startTime) / 1000;
   if testResult.RunCount > 0 then
     successRate :=  Trunc(
        ((testResult.runCount - testResult.failureCount - testResult.errorCount)
         /testResult.runCount)
        *100)
   else
     successRate := 100;

   writeReport('<statistics>'+CRLF+
                  '<stat name="tests" value="'+intToStr(testResult.runCount)+'" />'+CRLF+
                  '<stat name="failures" value="'+intToStr(testResult.failureCount)+'" />'+CRLF+
                  '<stat name="errors" value="'+intToStr(testResult.errorCount)+'" />'+CRLF+
                  '<stat name="success-rate" value="'+intToStr(successRate)+'%" />'+CRLF+
                  '<stat name="started-at" value="'+DateTimeToStr(dtStartTime)+'" />'+CRLF+
                  '<stat name="finished-at" value="'+DateTimeToStr(now)+'" />'+CRLF+
                  Format('<stat name="runtime" value="%1.3f"/>', [runtime])+CRLF+
                  '</statistics>'+CRLF+
              '</test-results>');
   
   if TTextRec(FOutputFile).Mode = fmOutput then
      Close(FOutputFile);
end;

class function TXMLTestListener.RunTest(suite: ITest; outputFile:String): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

class function TXMLTestListener.RunRegisteredTests(outputFile:String): TTestResult;
begin
  Result := RunTest(registeredTests, outputFile);
end;

function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := RunTest(registeredTests, outputFile);
end;


procedure TXMLTestListener.Status(test: ITest; const Msg: string);
begin
  writeReport(Format('INFO: %s: %s', [test.Name, Msg]));
end;

procedure TXMLTestListener.Warning(test :ITest; const Msg :string);
begin
  writeReport(Format('WARNING: %s: %s', [test.Name, Msg]));
end;

function TXMLTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
  if not Result then
    writeReport(Format('    <test-case name="%s%s" executed="False"/>',
                       [GetCurrentCaseName, test.GetName]));
end;

procedure TXMLTestListener.EndSuite(suite: ITest);
begin
     if CompareText(suite.Name, ExtractFileName(Application.ExeName)) = 0 then
       Exit;
     writeReport('  </results>');
     writeReport('</test-suite>');
     FSuiteStack.Delete(0);
end;

procedure TXMLTestListener.StartSuite(suite: ITest);
var
  s : string;
begin
   if CompareText(suite.Name, ExtractFileName(Application.ExeName)) = 0 then
     Exit;
   s := GetCurrentSuiteName + suite.Name;
   writeReport(Format('<test-suite name="%s" total="%d" not-run="%d">', [s, suite.CountTestCases, suite.CountTestCases - suite.CountEnabledTestCases]));
   FSuiteStack.Insert(0, suite.getName);
   writeReport('  <results>');
end;

{:
 Replace byt string by mot in text string
 }
class function TXMLTestListener.StringReplaceAll (text,byt,mot : string ) :string;
var
   plats : integer;
begin
While pos(byt,text) > 0 do
      begin
      plats := pos(byt,text);
      delete (text,plats,length(byt));
      insert (mot,text,plats);
      end;
result := text;
end;

{:
 Replace special character by sgml compliant characters
 }
class function TXMLTestListener.text2sgml(text : String) : String;
begin
  text := stringreplaceall (text,'<','&lt;');
  text := stringreplaceall (text,'>','&gt;');
  result := text;
end;

destructor TXMLTestListener.Destroy;
begin
  FreeAndNil(FSuiteStack);
  inherited Destroy;
end;

function TXMLTestListener.GetCurrentSuiteName: string;
var
  c : Integer;
begin
  Result := '';
  for c := 0 to FSuiteStack.Count - 1 do
    Result := FSuiteStack[c] + '.' + Result;
end;

function TXMLTestListener.GetCurrentCaseName: string;
var
  c : Integer;
begin
  Result := '';
  for c := 0 to FSuiteStack.Count - 1 do
    Result := FSuiteStack[c] + '.' + Result;
  Result := RegisteredTests.Name + '.' + Result;
end;

end.
