program ztestall;

{$mode objfpc}{$H+}

uses
  heaptrc,custapp, sysutils, comctrls, types,
  Interfaces, Forms, GuiTestRunner, LResources,
  Classes, consoletestrunner, fpcunit, fpcunitreport, testregistry,
  plaintestreport,latextestreport, xmltestreport,
  ZTestConfig,
  ZSqlTestCase,
  //core
  ZTestCore,
  //parsesql
  ZTestParseSql,
  //dbc
  ZTestDbc,
  //component
  ZTestComponents,
  //bugreport
  ZTestBugReports
  ;

type

  TTreeNodeState=(tsUnChecked, tsChecked);

  { TLazTestRunner }

  { TMyResultsWriter }

  TMyResultsWriter = class(TPlainResultsWriter)
  protected
  // override the protected methods of TCustomResultsWriter to customize its behavior
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
      ANumFailures: integer; ANumIgnores: integer); override;
  public
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
    procedure WriteCustomHelp; override;
    function GetShortOpts: string; override;
    function GetResultsWriter: TCustomResultsWriter; override;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TMyGUITestRunner }

  TMyGUITestRunner = class(TGUITestRunner)
  protected
  // override the protected methods of TGUITestRunner to customize its behavior
  public
    constructor Create(TheOwner: TComponent); override;
    function FindNode(NodeText : String):TTreeNode;
  end;

constructor TMyGUITestRunner.Create(TheOwner: TComponent);
var
  Suite : String;
  SuiteNode : TTreeNode;
  procedure ChangeCheck(aNode: TTreeNode; aCheck: TTreeNodeState);
  var
    i: integer;
    n: TTreeNode;
  begin
    if Assigned(aNode) then
    begin
      aNode.StateIndex := ord(aCheck);
      if (TTest(aNode.Data) is TTestSuite) then
        for i := 0 to aNode.Count - 1 do
        begin
          n := aNode.Items[i];
          ChangeCheck(n, aCheck);
        end;
    end;
  end;
begin
  inherited Create(TheOwner);
  if Application.HasOption('suite') then
    begin
      Suite := Application.GetOptionValue('suite');
      ActUncheckAllExecute(Self);
      SuiteNode := FindNode(suite);
      If SuiteNode <> nil then
        begin
          SuiteNode.selected := true;
          ChangeCheck(SuiteNode,tsChecked);
        end;
    end;
end;

function TMyGUITestRunner.FindNode(NodeText: String): TTreeNode;
var
  i: integer;
  Function CheckNodes (node:TTreeNode; ATestName:string):TTreeNode;
  var s, c : string;
      I, p : integer;
  begin
    result := nil;
      begin
      p := pos ('.', ATestName);
      if p > 0 then
        begin
        s := copy (ATestName, 1, p-1);
        c := copy (ATestName, p+1, maxint);
        end
      else
        begin
        s := '';
        c := ATestName;
        end;
      if comparetext(c, node.Text) = 0 then
        result := node
      else if (CompareText( s, node.Text) = 0) or (s = '') then
        for I := 0 to node.Count - 1 do
          begin
            result := CheckNodes(node.items[I], c);
            if result <> nil then exit;
          end;
      end
  end;
begin
  for i := 0 to TestTree.Items.Count -1 do
    begin
      Result := CheckNodes(TestTree.Items[i],NodeText);
      if result <> nil then exit;
    end;
end;

procedure TMyResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer;
  ATiming: TDateTime);
begin
  { //don't write the verbose test footer information
  inherited WriteTestFooter(ATest, ALevel, ATiming);}
end;

procedure TMyResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite;
  ALevel: integer);
begin
  { //don't write the verbose suite header information
  inherited WriteSuiteHeader(ATestSuite, ALevel);}
end;

procedure TMyResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite;
  ALevel: integer; ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
  ANumFailures: integer; ANumIgnores: integer);
begin
  { //don't write the verbose suite footer information
  inherited WriteSuiteFooter(ATestSuite, ALevel, ATiming, ANumRuns, ANumErrors,
    ANumFailures, ANumIgnores); }
end;

{ TMyTestRunner }

procedure TMyTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('  -c <filename>             custom config file name');
  writeln('  -b or --batch             don''t run the GUI interface');
  writeln('  -v or --verbose           show full output (otherwise compact report is used)');
  writeln('  -n or --norebuild         don''t rebuild the databases');
end;

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'bvcn';
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if (FormatParam = fPlain) and not Application.HasOption('v', 'verbose') then
    Result := TMyResultsWriter.Create(nil)
  else
    Result:=inherited GetResultsWriter;
end;

procedure TMyTestRunner.DoRun;
  Var tempTestSuite : TTestSuite;
    procedure CheckTestRegistry (test:TTest; ATestName:string);
    var s, c : string;
        I, p : integer;
    begin
      if test is TTestSuite then
        begin
        p := pos ('.', ATestName);
        if p > 0 then
          begin
          s := copy (ATestName, 1, p-1);
          c := copy (ATestName, p+1, maxint);
          end
        else
          begin
          s := '';
          c := ATestName;
          end;
        if comparetext(c, test.TestName) = 0 then
          begin
//            Writeln('Adding Suite : '+test.TestName);
            tempTestSuite.AddTest(test);
          end
        else if (CompareText( s, Test.TestName) = 0) or (s = '') then
          for I := 0 to TTestSuite(test).Tests.Count - 1 do
            CheckTestRegistry (TTest(TTestSuite(test).Tests[I]), c)
        end
      else // if test is TTestCase then
        begin
        if comparetext(test.TestName, ATestName) = 0 then
          begin
//            Writeln('Adding Test : '+test.TestName);
            tempTestSuite.AddTest(test);
          end;
        end;
    end;

  var
    I, J: integer;
    S: string;
    SuiteTests: TStringDynArray;
  begin
    S := CheckOptions(GetShortOpts, LongOpts);
    if (S <> '') then
      Writeln(S);

    ParseOptions;

    //get a list of all registed tests
    if HasOption('l', 'list') then
      case FormatParam of
        fLatex: Write(GetSuiteAsLatex(GetTestRegistry));
        fPlain: Write(GetSuiteAsPlain(GetTestRegistry));
      else
        Write(GetSuiteAsLatex(GetTestRegistry));;
      end;

    //run the tests
    if HasOption('suite') then
    begin
      S := '';
      S := GetOptionValue('suite');
      if S = '' then
        for I := 0 to GetTestRegistry.Tests.Count - 1 do
          writeln(GetTestRegistry[i].TestName)
      else
        begin
          tempTestSuite := TTestSuite.Create('CustomTestSuite');
          SuiteTests := SplitStringToArray(S, LIST_DELIMITERS);
          for J := 0 to High(SuiteTests) do
            for I := 0 to GetTestRegistry.Tests.count-1 do
              CheckTestRegistry (GetTestregistry[I], SuiteTests[J]);
          DoTestRun(tempTestSuite);
        end;
    end
    else if HasOption('a', 'all') or (DefaultRunAllTests and Not HasOption('l','list')) then
      DoTestRun(GetTestRegistry) ;
    Terminate;
  end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
  longopts.Add('norebuild');
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  SetHeapTraceOutput('heaptrc.log');
  TestGroup := COMMON_GROUP;
  If Not Application.HasOption('h', 'help') and
     Not Application.HasOption('n', 'norebuild')then
    RebuildTestDatabases;

 If Application.HasOption('b', 'batch') then
  begin
    Applicationc := TMyTestRunner.Create(nil);
    Applicationc.Initialize;
    Applicationc.Run;
    Applicationc.Free;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TMyGuiTestRunner, TestRunner);
    Application.Run;
  end;
end.
