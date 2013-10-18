program ztestall;

{$mode objfpc}{$H+}

uses
  heaptrc,custapp, sysutils,
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
  ZTestBugReports,
  //performance
  ZTestPerformance
  ;

type
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
    FullRegistryItems: TFPList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyGUITestRunner.Create(TheOwner: TComponent);
var
  tempTestSuite :TTestSuite;
begin
  // Dirty Workaround to make sure all tests can be destroyed in the destructor
  If CommandLineSwitches.Suite then
    begin
      FullRegistryItems := TFPList.Create;
      FullRegistryItems.Assign(GetTestRegistry.Tests);
      tempTestSuite := CreateTestSuite;
      GetTestRegistry.Tests.Assign(tempTestSuite.Tests);
    end;
  inherited Create(TheOwner);
end;

destructor TMyGUITestRunner.Destroy;
begin
  If CommandLineSwitches.Suite then
    begin
      GetTestRegistry.Tests.Assign(FullRegistryItems);
      FullRegistryItems.Free;
    end;
  inherited Destroy;
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
  writeln('  -c <filename>                        custom config file name');
  writeln('  -b or --batch                        don''t run the GUI interface');
  writeln('  -v or --verbose                      show full output (otherwise compact report is used)');
  writeln('  -n or --norebuild                    don''t rebuild the databases');
  writeln('  -m <filename> or -monitor <filename> sqlmonitor file name');
end;

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'bvcnm:';
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if (FormatParam = fPlain) and not CommandLineSwitches.verbose then
    Result := TMyResultsWriter.Create(nil)
  else
    Result:=inherited GetResultsWriter;
end;

procedure TMyTestRunner.DoRun;
  var
    tempTestSuite : TTestSuite;
    S: string;
  begin
    S := CheckOptions(GetShortOpts, LongOpts);
    if (S <> '') then
      Writeln(S);

    ParseOptions;

    tempTestSuite := CreateTestSuite;

    //get a list of all registed tests
    if CommandLineSwitches.list then
      case FormatParam of
        fLatex: Write(GetSuiteAsLatex(tempTestSuite));
        fPlain: Write(GetSuiteAsPlain(tempTestSuite));
        fXML: Write(GetSuiteAsXML(tempTestSuite));
      else
        Write(GetSuiteAsLatex(tempTestSuite));;
      end;

    //run the tests
    if CommandLineSwitches.runall or (DefaultRunAllTests and Not CommandLineSwitches.list) then
      DoTestRun(tempTestSuite) ;
    Terminate;
  end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
  longopts.Add('norebuild');
  longopts.Add('monitor:');
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  SetHeapTraceOutput('heaptrc.log');
  TestGroup := COMMON_GROUP;

  If Not CommandLineSwitches.help and
     Not CommandLineSwitches.norebuild then
    RebuildTestDatabases;

  If CommandLineSwitches.sqlmonitor then
    EnableZSQLMonitor;

  If CommandLineSwitches.batch then
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
