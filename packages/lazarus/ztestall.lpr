program ztestall;

{$mode objfpc}{$H+}

uses
  custapp,
  Interfaces, Forms, GuiTestRunner, LResources,
  Classes, consoletestrunner,
  //core
  ZTestSysUtils, ZTestVariant, ZTestTokenizer,
  ZTestList, ZTestFramework, ZTestExprToken, ZTestExpression, ZTestURL,
  //parsesql
  ZTestSybaseToken, ZTestSqLiteToken,
  ZTestSqlAnalyser, ZTestScriptParser, ZTestPostgreSqlToken, ZTestOracleToken,
  ZTestMySqlToken, ZTestInterbaseToken,
  //dbc
  ZTestDbcResultset, ZTestDbcUtils, ZTestDbcCache,
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

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
    function GetShortOpts: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMyTestRunner }

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'b';
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  longopts.Add('batch');
end;

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
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
    Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
  end;
end.
