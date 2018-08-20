{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2006 by Dean Zobec, Graeme Geldenhuys

    An example of an XML report writer for FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
  

  Purpose:
    This unit contains a XML TestListener for use with the fpcUnit testing
    framework. It uses the XMLWrite unit (part of FPC) to generate
    the XML document. The benefit of using XMLWrite is that the data generated
    is valid XML, with reserved characters correctly escaped.
    This allows the XML document to be further processed with XSLT etc without
    any issues.

  Notes:
    Specify 'null' as the filename if you don't want to output to file (e.g.
    used by the GUI test runner which instead reads the Document property).

}

unit zxmltestreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpcunit, fpcunitreport, testutils, dom, XMLWrite;
  

type

  { TZXMLResultsWriter }

  TZXMLResultsWriter = class(TCustomResultsWriter)
  private
    FDoc: TXMLDocument;
    FResults{, FListing}: TDOMElement;
    FSuitePath: TFPList;
    FResultsPath: TFPList;
    FCurrentTest: TDOMElement;
  protected
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; 
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; 
      ANumFailures: integer; ANumIgnores: integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure StartTest(ATest: TTest); override;
    procedure EndTest(ATest: TTest); override;
    procedure WriteResult(aResult: TTestResult); override;
    { A public property to the internal XML document }
    property Document: TXMLDocument read FDoc;
  end;

//function GetSuiteAsXML(aSuite: TTestSuite): string;
//function TestSuiteAsXML(n: TDOMElement; FDoc: TXMLDocument; aSuite:TTestSuite): string;

implementation

function TimeToSeconds(ATime: TTime): single;
var
  h, m, s, ms: Word;
begin
  DecodeTime(ATime, h, m, s, ms);
  result := 3600 * h + 60 * m + s + ms / 1000;
end;

(*
function GetSuiteAsXML(aSuite: TTestSuite): string;
var
  FDoc: TXMLDocument;
  n: TDOMElement;
  stream : TStringStream;
begin
  Result := '';

  if aSuite <> nil then
  begin
    FDoc:= TXMLDocument.Create;

    n := FDoc.CreateElement('TestSuites');
    FDoc.AppendChild(n);

    TestSuiteAsXML(n, FDoc, aSuite);

    stream := TStringStream.Create('');
    WriteXMLFile(FDoc, stream);
    writeln(stream.DataString);
    stream.Free;
  end;
end;

function TestSuiteAsXML(n: TDOMElement; FDoc: TXMLDocument; aSuite:TTestSuite): string;
var
  i: integer;
  E,T : TDomElement;
  
begin
  if aSuite.TestName<>'' then
    begin
    E:=FDoc.CreateElement('Suite');
    E['Name']:=aSuite.TestName;
    N.AppendChild(E);
    end
  else
    E:=N;
  for i:=0 to Pred(aSuite.ChildTestCount) do
    if TTest(aSuite.Test[i]) is TTestSuite then
      TestSuiteAsXML(E, FDoc, TTestSuite(aSuite.Test[i]))
    else
      if TTest(aSuite.Test[i]) is TTestCase then
        begin
        T:=FDoc.CreateElement('Test');
        T['name']:=TTestCase(aSuite.Test[i]).TestName;
        E.AppendChild(T);
        end;
end;
*)

{ TZXMLResultsWriter }

const
  strTrue = 'True';
  strFalse = 'False';

procedure TZXMLResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := FDoc.CreateElement('test-case');
  n['name'] := ATest.TestName;
  n['executed'] := strTrue;
  n['success'] := strFalse;
  if FResultsPath.Count > 0 then
  //test is included in a suite
    TDOMElement(FResultsPath[FResultsPath.Count -1]).AppendChild(n)
  else
  //no suite to append so append directly to the listing node
    FResults.AppendChild(n);
  FCurrentTest := n;
end;

procedure TZXMLResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);
begin
  inherited;
  if not SkipTiming then
    FCurrentTest['time'] := FormatFloat('0.###', ATiming);
end;


procedure TZXMLResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
var
  n: TDOMElement;
  results: TDomElement;
begin
  inherited;
  n := FDoc.CreateElement('test-suite');
  FSuitePath.Add(n); 
  n['name'] := ATestSuite.TestName;

  if FSuitePath.Count = 1 then
    FResults.AppendChild(n)
  else
    TDOMElement(FResultsPath[FResultsPath.Count - 1]).AppendChild(n);

  results := FDoc.CreateElement('results');
  FResultsPath.Add(results);
  n.AppendChild(results);
end;


procedure TZXMLResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; 
  ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer;
  ANumIgnores: integer);
var
  n: TDOMElement;
begin
  inherited;
  n := TDomElement(FSuitePath[FSuitePath.Count -1]);
  //if not SkipTiming then
  //  n['ElapsedTime'] := FormatDateTime('hh:nn:ss.zzz', ATiming);
  n['total'] := IntToStr(ANumRuns);
  //n['NumberOfErrors'] := IntToStr(ANumErrors);
  //n['NumberOfFailures'] := IntToStr(ANumFailures);
  //n['NumberOfIgnoredTests'] := IntToStr(ANumIgnores);
  n['not-run'] := '0';
  FSuitePath.Delete(FSuitePath.Count - 1);
  FResultsPath.Delete(FResultsPath.Count - 1);
end;

constructor TZXMLResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDoc:= TXMLDocument.Create;
  FSuitePath := TFPList.Create;
  FResultsPath := TFPList.Create;
  FResults := nil;
  //FListing := nil;
end;

destructor  TZXMLResultsWriter.Destroy;
begin
  FResults := nil;
  //FListing := nil;
  FSuitePath.Free;
  FResultsPath.Free;
  FDoc.Free;
  inherited Destroy;
end;


procedure TZXMLResultsWriter.WriteHeader;
begin
  inherited;
  FResults := FDoc.CreateElement('test-results');
  FResults.AppendChild(FDoc.CreateComment(' Generated using FPCUnit on '
    + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) ));
  FDoc.AppendChild(FResults);
  FResults['name'] := ParamStr(0);
//  FListing := FDoc.CreateElement('TestListing');
//  FResults.AppendChild(FListing);
end;


procedure TZXMLResultsWriter.WriteFooter;
begin
  inherited;
end;

procedure TZXMLResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  failure, message: TDOMElement;
begin
  inherited;
  if AFailure.IsIgnoredTest then
    FCurrentTest['result'] := 'Ignored'
  else
    FCurrentTest['result'] := 'Failure';
    //FCurrentTest.AppendChild(FDoc.CreateElement('message')).AppendChild
    //  (FDoc.CreateTextNode(AFailure.AsString));
    //FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionClass')).AppendChild
    //  (FDoc.CreateTextNode(AFailure.ExceptionClassName));
    //FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild
    //  (FDoc.CreateTextNode(AFailure.ExceptionMessage));
    failure := FDoc.CreateElement('failure');
    FCurrentTest.AppendChild(failure);
    message := FDoc.CreateElement('message');
    failure.AppendChild(message);
    message.AppendChild(FDoc.CreateTextNode(AFailure.ExceptionClassName + #13 + AFailure.ExceptionMessage + #13 + AFailure.AsString));
end;

procedure TZXMLResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
var
  error, message: TDOMElement;
begin
  inherited;
  FCurrentTest['result'] := 'Error';
  //FCurrentTest.AppendChild(FDoc.CreateElement('Message')).AppendChild
  //  (FDoc.CreateTextNode(AError.AsString));
  //FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionClass')).AppendChild
  //  (FDoc.CreateTextNode(AError.ExceptionClassName));
  //FCurrentTest.AppendChild(FDoc.CreateElement('ExceptionMessage')).AppendChild
  //  (FDoc.CreateTextNode(AError.ExceptionMessage));
  //FCurrentTest.AppendChild(FDoc.CreateElement('SourceUnitName')).AppendChild
  //  (FDoc.CreateTextNode(AError.SourceUnitName));
  //FCurrentTest.AppendChild(FDoc.CreateElement('LineNumber')).AppendChild
  //  (FDoc.CreateTextNode(IntToStr(AError.LineNumber)));
  //FCurrentTest.AppendChild(FDoc.CreateElement('FailedMethodName')).AppendChild
  //  (FDoc.CreateTextNode(AError.FailedMethodName));
  error := FDoc.CreateElement('failure');
  FCurrentTest.AppendChild(error);
  message := FDoc.CreateElement('message');
  error.AppendChild(message);
  message.AppendChild(FDoc.CreateTextNode(AError.ExceptionClassName + #13 + AError.ExceptionMessage + #13 + AError.SourceUnitName + #13 + IntToStr(AError.LineNumber) + #13 + AError.FailedMethodName + #13 + AError.AsString));
end;


procedure TZXMLResultsWriter.StartTest(ATest: TTest);
begin
  inherited;
end;


procedure TZXMLResultsWriter.EndTest(ATest: TTest);
begin
  inherited;
end;

procedure TZXMLResultsWriter.WriteResult(aResult: TTestResult);
var
  n, lResults: TDOMNode;
  f: text;
begin
  lResults := FDoc.FindNode('TestResults');

  //n := FDoc.CreateElement('NumberOfRunTests');
  //n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.RunTests)));
  //lResults.AppendChild(n);
  FResults['total'] := IntToStr(aResult.RunTests + aResult.NumberOfIgnoredTests + aResult.NumberOfSkippedTests);
  FResults['not-run'] := IntToStr(aResult.NumberOfIgnoredTests + aResult.NumberOfSkippedTests);
  FResults['date'] := FormatDateTime('DD/MM/YYYY', Now);
  FResults['time'] := FormatDateTime('HH:NN:SS', Now);

  //n := FDoc.CreateElement('NumberOfErrors');
  //n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfErrors)));
  //lResults.AppendChild(n);

  //n := FDoc.CreateElement('NumberOfFailures');
  //n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfFailures)));
  //lResults.AppendChild(n);
  FResults['failures'] := IntToStr(aResult.NumberOfErrors + aResult.NumberOfFailures);

  n := FDoc.CreateElement('NumberOfIgnoredTests');
  n.AppendChild(FDoc.CreateTextNode(IntToStr(aResult.NumberOfIgnoredTests)));
  lResults.AppendChild(n);

  //if not SkipTiming then
  //begin
  //  n := FDoc.CreateElement('TotalElapsedTime');
  //  n.AppendChild(FDoc.CreateTextNode(FormatDateTime('hh:nn:ss.zzz',
  //    Now - aResult.StartingTime)));
  //  lResults.AppendChild(n);
  //end;

  { Summary of ISO 8601  http://www.cl.cam.ac.uk/~mgk25/iso-time.html }
  //n := FDoc.CreateElement('DateTimeRan');
  //n.AppendChild(FDoc.CreateTextNode(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)));
  //lResults.AppendChild(n);

  // This is so that the GUI Test Runner doesn't output text as well.
  if FileName <> 'null' then
  begin
    system.Assign(f, FileName);
    rewrite(f);
    WriteXMLFile(FDoc, f);
    close(f);
  end;
end;

end.



