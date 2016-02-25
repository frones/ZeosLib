program ztestall;

{$mode objfpc}{$H+}

uses
  {$IF FPC_FULLVERSION<30000} //use compiler option -gh instead
  heaptrc,
  {$IFEND}
  custapp, sysutils,
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
    procedure WriteTestFooter({%H-}ATest: TTest; {%H-}ALevel:{%H-} integer; {%H-}ATiming: TDateTime); override;
    procedure WriteSuiteHeader({%H-}ATestSuite: TTestSuite; {%H-}ALevel: integer); override;
    procedure WriteSuiteFooter({%H-}ATestSuite: TTestSuite; {%H-}ALevel: integer;
      {%H-}ATiming: TDateTime; {%H-}ANumRuns:{%H-} integer; {%H-}ANumErrors: integer;
      {%H-}ANumFailures: integer; {%H-}ANumIgnores: integer); override;
  public
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  private
    {$IFNDEF FPC}
    FullRegistryItems: TFPList;
    CurrentRegistryItems: TFPList;
    {$ENDIF}
  protected
  // override the protected methods of TTestRunner to customize its behavior
    procedure WriteCustomHelp; override;
    function GetShortOpts: string; override;
    function GetResultsWriter: TCustomResultsWriter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TMyGUITestRunner }

  TMyGUITestRunner = class(TGUITestRunner)
  private
    {$IFNDEF FPC}
    FullRegistryItems: TFPList;
    CurrentRegistryItems: TFPList;
    {$ENDIF}
  protected
  // override the protected methods of TGUITestRunner to customize its behavior
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyGUITestRunner.Create(TheOwner: TComponent);
begin
  {$IFNDEF FPC}
  // Dirty Workaround to make sure ALL tests can be destroyed in the destructor
  FullRegistryItems := TFPList.Create;
  CurrentRegistryItems := GetExecutedTests;
  FullRegistryItems.Assign(GetTestRegistry.Tests); //save old list
  GetTestRegistry.Tests.Assign(CurrentRegistryItems); //assign new or same list
  {$ENDIF}
  inherited Create(TheOwner);
end;

destructor TMyGUITestRunner.Destroy;
begin
  {$IFNDEF FPC}
  GetTestRegistry.Tests.Assign(FullRegistryItems); //assign old list again -> destroy the tests
  FreeAndNil(FullRegistryItems); //free old list
  FreeAndNil(CurrentRegistryItems); //free possible changed list
  {$ENDIF}
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
  writeln(' --suite="<layer>.<optional testname>.<optional MethodeName>"');
  writeln(' --memcheck <filename>')
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

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  {$IFNDEF FPC}
  // Dirty Workaround to make sure ALL tests can be destroyed in the destructor
  FullRegistryItems := TFPList.Create;
  CurrentRegistryItems := GetExecutedTests;
  FullRegistryItems.Assign(GetTestRegistry.Tests); //save old list
  GetTestRegistry.Tests.Assign(CurrentRegistryItems); //assign new or same list
  {$ENDIF}
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
  longopts.Add('norebuild');
  longopts.Add('monitor:');
  longopts.Add('suite:');
end;

destructor TMyTestRunner.Destroy;
begin
  {$IFNDEF FPC}
  GetTestRegistry.Tests.Assign(FullRegistryItems); //assign old list again -> destroy the tests
  FreeAndNil(FullRegistryItems); //free old list
  FreeAndNil(CurrentRegistryItems); //free possible changed list
  {$ENDIF}
  inherited Destroy;
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  if CommandLineSwitches.memcheck and (CommandLineSwitches.memcheck_file <> '') then
  begin
    if FileExists(CommandLineSwitches.memcheck_file) then
      DeleteFile(CommandLineSwitches.memcheck_file);
    {$IF FPC_FULLVERSION<30000}
    SetHeapTraceOutput(CommandLineSwitches.memcheck_file);
    {$IFEND}
  end;
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
