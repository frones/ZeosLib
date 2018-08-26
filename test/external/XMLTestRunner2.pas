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
  TestFramework, XMLIntf, XMLDoc;

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
     FNextId: Integer;

     testStart : TDateTime;
     FSuiteStack : TInterfaceList;

     FXmlDocument: IXMLDocument;
     FDocNode: IXMLNode;

     procedure writeReport(str: String);
     function GetCurrentSuiteName : string;
     function GetCurrentCaseName: string;
     function GetNextId: Integer;
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
var
  Doc: TXMLDocument;
begin
  inherited Create;
  FSuiteStack := TInterfaceList.Create;
  Doc := TXmlDocument.Create(nil);
  FXmlDocument := (Doc as IXMLDocument);
  FXmlDocument.Options := FXmlDocument.Options + [doNodeAutoIndent];
  FXmlDocument.Active := true;
  FXmlDocument.Version := '1.0';
  FXmlDocument.Encoding := 'UTF-8';
  FXmlDocument.StandAlone := 'yes';
  FFileName := outputFile;
  FNextId := 3;
end;

{:
 Write F in the report file or on standard output if none specified
}
procedure TXMLTestListener.writeReport(str : String);
begin
   if TTextRec(FOutputFile).Mode = fmOutput then
      writeln(FOutputFile, str)
   else
      writeln(str);
end;

const
  TrueFalse : array[Boolean] of string = ('False', 'True');

function TXMLTestListener.GetNextId: Integer;
begin
  Result := FNextId;
  Inc(FNextId);
end;

procedure TXMLTestListener.AddSuccess(test: ITest);
var
  SuiteN: IXMLNode;
  TestN: IXMLNode;
begin
  if test.tests.Count<=0 then begin
    SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode);
    SuiteN.Attributes['type'] := 'TestFixture';
    TestN := SuiteN.AddChild('test-case');
    TestN.Attributes['id'] := IntToStr(GetNextId);
    TestN.Attributes['name'] :=  test.GetName;
    TestN.Attributes['fullname'] := GetCurrentCaseName + test.GetName;
    TestN.Attributes['methodname'] := test.GetName;
    TestN.Attributes['classname'] := GetCurrentCaseName;
    if test.Enabled
    then TestN.Attributes['runstate'] := 'Runnable'
    else TestN.Attributes['runstate'] := 'Skipped';
    TestN.Attributes['result'] := 'Passed';
    //TestN.Attributes['start-time'] :=
    //TestN.Attributes['end-time'] :=
    TestN.Attributes['duration'] := test.ElapsedTestTime;

//      writeReport(Format('    <test-case name="%s%s" executed="%s" success="True" time="%1.3f" result="Pass"/>',
//                         [GetCurrentCaseName, test.GetName, TrueFalse[test.Enabled], test.ElapsedTestTime]));
 end;
end;

procedure TXMLTestListener.AddError(error: TTestFailure);
var
  SuiteN: IXMLNode;
  TestN: IXMLNode;
  test: ITest;
  FailureN: IXMLNode;
  MessageN: IXMLNode;
  MessageStr: String;
  StackN: IXMLNode;
begin
//   writeReport(Format('    <test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Failed">',
//                      [GetCurrentCaseName, error.FailedTest.GetName, TrueFalse[error.FailedTest.Enabled], error.FailedTest.ElapsedTestTime]));
    Test := error.FailedTest;
    SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode);
    SuiteN.Attributes['type'] := 'TestFixture';
    TestN := SuiteN.AddChild('test-case');
    TestN.Attributes['id'] := IntToStr(GetNextId);
    TestN.Attributes['name'] :=  test.GetName;
    TestN.Attributes['fullname'] := GetCurrentCaseName + test.GetName;
    TestN.Attributes['methodname'] := test.GetName;
    TestN.Attributes['classname'] := GetCurrentCaseName;
    if test.Enabled
    then TestN.Attributes['runstate'] := 'Runnable'
    else TestN.Attributes['runstate'] := 'Skipped';
    TestN.Attributes['result'] := 'Failed';
    //TestN.Attributes['start-time'] :=
    //TestN.Attributes['end-time'] :=
    TestN.Attributes['duration'] := test.ElapsedTestTime;

//   writeReport('      <failure>');
   FailureN := TestN.AddChild('failure');

//   writeReport(Format('        <message><![CDATA[Exception: [%s] %s', [error.ThrownExceptionName, error.ThrownExceptionMessage]));
//   writeReport(Format('At: %s]]></message>', [text2sgml(error.AddressInfo)]));
   MessageN := FailureN.AddChild('message');
   MessageStr := Format('Exception: [%s] %s'#13'at: %s', [error.ThrownExceptionName, error.ThrownExceptionMessage, text2sgml(error.AddressInfo)]);
   MessageN.ChildNodes.Add(FXmlDocument.CreateNode(MessageStr, ntCData));

//   writeReport(Format('        <stack-trace><![CDATA[%s]]></stack-trace>', [error.StackTrace]));
   StackN := FailureN.AddChild('stack-trace');
   StackN.ChildNodes.Add(FXmlDocument.CreateNode(error.StackTrace, ntCData))

//   writeReport('      </failure>');
//   writeReport('    </test-case>');
end;

procedure TXMLTestListener.AddFailure(failure: TTestFailure);
var
  SuiteN: IXMLNode;
  TestN: IXMLNode;
  test: ITest;
  FailureN: IXMLNode;
  MessageN: IXMLNode;
  MessageStr: String;
  StackN: IXMLNode;
begin
//   writeReport(Format('    <test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Failed">',
//                      [GetCurrentCaseName, failure.FailedTest.GetName, TrueFalse[failure.FailedTest.Enabled], failure.FailedTest.ElapsedTestTime / 1000]));
    Test := failure.FailedTest;
    SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode);
    SuiteN.Attributes['type'] := 'TestFixture';
    TestN := SuiteN.AddChild('test-case');
    TestN.Attributes['id'] := IntToStr(GetNextId);
    TestN.Attributes['name'] :=  test.GetName;
    TestN.Attributes['fullname'] := GetCurrentCaseName + test.GetName;
    TestN.Attributes['methodname'] := test.GetName;
    TestN.Attributes['classname'] := GetCurrentCaseName;
    if test.Enabled
    then TestN.Attributes['runstate'] := 'Runnable'
    else TestN.Attributes['runstate'] := 'Skipped';
    TestN.Attributes['result'] := 'Failed';
    //TestN.Attributes['start-time'] :=
    //TestN.Attributes['end-time'] :=
    TestN.Attributes['duration'] := test.ElapsedTestTime;

//   writeReport('      <failure>');
   FailureN := TestN.AddChild('failure');

//   writeReport(Format('        <message><![CDATA[Exception: [%s] %s', [failure.ThrownExceptionName, failure.ThrownExceptionMessage]));
//   writeReport(Format('At: %s]]></message>', [text2sgml(failure.AddressInfo)]));
   MessageN := FailureN.AddChild('message');
   MessageStr := Format('Exception: [%s] %s'#13'at: %s', [failure.ThrownExceptionName, failure.ThrownExceptionMessage, text2sgml(failure.AddressInfo)]);
   MessageN.ChildNodes.Add(FXmlDocument.CreateNode(MessageStr, ntCData));

//   writeReport(Format('        <stack-trace><![CDATA[%s]]></stack-trace>', [failure.StackTrace]));
   StackN := FailureN.AddChild('stack-trace');
   StackN.ChildNodes.Add(FXmlDocument.CreateNode(failure.StackTrace, ntCData))

//   writeReport('      </failure>');
//   writeReport('    </test-case>');
end;


procedure TXMLTestListener.StartTest(test: ITest);
begin
end;

procedure TXMLTestListener.EndTest(test: ITest);
begin
end;

procedure TXMLTestListener.TestingStarts;
begin
   startTime := GetTickCount;
   dtStartTime := Now;
   
//   if FFileName<>'' then
//   begin
//     AssignFile(FOutputFile, FFileName);
//     Rewrite(FOutputFile);
//   end;

//   writeReport('<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>');
//   writeReport(Format('<test-run id="2" name="%s" total="%d" date="%s" time="%s">',
//                      [RegisteredTests.Name, RegisteredTests.CountTestCases,
//                        DateToStr(Now),
//                          TimeToStr(Now)]));
  FDocNode := FXmlDocument.CreateNode('test-run', ntElement);
  FXmlDocument.DocumentElement := FDocNode;
  FDocNode.Attributes['id'] := '2';
end;

procedure TXMLTestListener.TestingEnds(testResult: TTestResult);
var
   runTime : Double;
   successRate : Integer;
begin
//   runtime := (GetTickCount - startTime);
//   if testResult.RunCount > 0 then
//     successRate :=  Trunc(
//        ((testResult.runCount - testResult.failureCount - testResult.errorCount)
//         /testResult.runCount)
//        *100)
//   else
//     successRate := 100;
(*
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
*)
//   if TTextRec(FOutputFile).Mode = fmOutput then
//      Close(FOutputFile);
  FXmlDocument.SaveToFile(FFileName);
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
var
  SuiteN: IXMLNode;
  TestN: IXMLNode;
begin
  Result := test.Enabled;
  if not Result then begin
//    writeReport(Format('    <test-case name="%s%s" executed="False"/>',
//                       [GetCurrentCaseName, test.GetName]));
    SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode);
    SuiteN.Attributes['type'] := 'TestFixture';
    TestN := SuiteN.AddChild('test-case');
    TestN.Attributes['id'] := IntToStr(GetNextId);
    TestN.Attributes['name'] :=  test.GetName;
    TestN.Attributes['fullname'] := GetCurrentCaseName + test.GetName;
    TestN.Attributes['methodname'] := test.GetName;
    TestN.Attributes['classname'] := GetCurrentCaseName;
    TestN.Attributes['runstate'] := 'Skipped';
    TestN.Attributes['result'] := 'Skipped';
    TestN.Attributes['duration'] := '0';
  end;
end;

procedure TXMLTestListener.EndSuite(suite: ITest);
var
  SuiteN: IXMLNode;
begin
  SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode);
  SuiteN.Attributes['duration'] := FloatToStr(suite.ElapsedTestTime / 1000);
  SuiteN.Attributes['end-time'] := FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', now);
  FSuiteStack.Delete(FSuiteStack.Count - 1);
end;

procedure TXMLTestListener.StartSuite(suite: ITest);
var
  s : string;
  SuiteN: IXMLNode;
begin
//   writeReport(Format('<test-suite name="%s" total="%d" not-run="%d">', [s, suite.CountTestCases, suite.CountTestCases - suite.CountEnabledTestCases]));
//   FSuiteStack.AddObject(suite.getName);
  if FSuiteStack.Count = 0
  then SuiteN := FDocNode.AddChild('test-suite')
  else SuiteN := (FSuiteStack.Items[FSuiteStack.Count - 1] as IXMLNode).AddChild('test-suite');
  SuiteN.Attributes['id'] := IntToStr(GetNextId);
  SuiteN.Attributes['name'] := suite.Name;
  SuiteN.Attributes['type'] := 'TestSuite';
  SuiteN.Attributes['total'] := IntToStr(suite.CountTestCases);
  SuiteN.Attributes['failed'] := '0';
  SuiteN.Attributes['passed'] := IntToStr(suite.CountTestCases);
  SuiteN.Attributes['result'] := 'Inconclusive';
  SuiteN.Attributes['skipped'] := '0';
  SuiteN.Attributes['duration'] := '0';
  SuiteN.Attributes['end-time'] := FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', now);
  SuiteN.Attributes['fullname'] := suite.Name;
  if suite.Enabled
  then SuiteN.Attributes['runstate'] := 'Runnable'
  else SuiteN.Attributes['runstate'] := 'Skipped';
  SuiteN.Attributes['classname'] := GetCurrentSuiteName + suite.Name;
  SuiteN.Attributes['start-time'] := FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', now);
  SuiteN.Attributes['inconclusive'] := '0';
  SuiteN.Attributes['testcasecount'] := IntToStr(suite.CountTestCases);
  SuiteN.Attributes['total'] := IntToStr(suite.CountTestCases);
  FSuiteStack.Add(SuiteN);
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
  if Assigned(FSuiteStack) then FreeAndNil(FSuiteStack);
  if Assigned(FXmlDocument) then FXmlDocument := nil;

  inherited Destroy;
end;

function TXMLTestListener.GetCurrentSuiteName: string;
var
  c : Integer;
begin
  Result := '';
  for c := FSuiteStack.Count - 1 downto 0 do
    Result := (FSuiteStack.Items[c] as IXmlNode).Attributes['name']  + '.' + Result;
end;

function TXMLTestListener.GetCurrentCaseName: string;
var
  c : Integer;
begin
  Result := GetCurrentSuiteName;
  Result := RegisteredTests.Name + '.' + Result;
end;

end.
