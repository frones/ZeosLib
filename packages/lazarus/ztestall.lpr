program ztestall;

{$mode objfpc}{$H+}

uses
  custapp,
  Interfaces, Forms, GuiTestRunner, LResources,
  Classes, consoletestrunner, fpcunit, fpcunitreport, plaintestreport,
  //core
  ZTestSysUtils, ZTestVariant, ZTestTokenizer,
  ZTestList, ZTestFramework, ZTestExprToken, ZTestExpression, ZTestURL,
  //parsesql
  ZTestSybaseToken, ZTestSqLiteToken,
  ZTestSqlAnalyser, ZTestScriptParser, ZTestPostgreSqlToken, ZTestOracleToken,
  ZTestMySqlToken, ZTestInterbaseToken,
  //dbc
  ZTestDbcResultSet, ZTestDbcUtils, ZTestDbcCache,
  ZTestDbcCachedResultSet, ZTestDbcMetadata,ZTestDbcResultSetMetadata, ZTestDbcResolver,
  ZTestDbcSqLite, ZTestDbcPostgreSqlMetadata,  ZTestDbcPostgreSql, ZTestDbcOracle,
  ZTestDbcMySqlMetadata, ZTestDbcMySql, ZTestDbcMsSql, ZTestDbcInterbaseMetadata,
  ZTestDbcInterbase, ZTestDbcASA, ZTestDbcASAMetadata,
  //component
  ZTestSqlTypes, ZTestSqlStrings, ZTestSqlProcessor,
  ZTestSqlMetadata, ZTestSorting, ZTestMasterDetail, ZTestExecuteSql,
  ZTestDataSetGeneric, ZTestData, ZTestConnection,
  //bugreport
  ZTestBugDbcCore,
  ZTestBugDbcMySql, ZTestBugCompMySql,
  ZTestBugDbcPostgreSql, //ZTestBugCompPostgreSql,
  //ZTestBugDbcInterbase, //ZTestBugCompInterbase,
  ZTestBugDbcDbLib, ZTestBugCompDbLib,
  ZTestBugCompMSSql
;

type

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
  public
    constructor Create(AOwner: TComponent); override;
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
  writeln('  -b or --batch             don''t run the GUI interface');
  writeln('  -v or --verbose           show full output (otherwise compact report is used)');
end;

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'bv';
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if (FormatParam = fPlain) and not Application.HasOption('v', 'verbose') then
    Result := TMyResultsWriter.Create(nil)
  else
    Result:=inherited GetResultsWriter;
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  If Application.HasOption('b', 'batch') or Application.HasOption('h', 'help')then
  begin
    Applicationc := TMyTestRunner.Create(nil);
    Applicationc.Initialize;
    Applicationc.Run;
    Applicationc.Free;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
  end;
end.
