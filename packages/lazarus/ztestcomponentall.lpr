program ztestcomponentall;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ZTestSqlTypes, ZTestSqlStrings, ZTestSqlProcessor,
  ZTestSqlMetadata, ZTestSorting, ZTestMasterDetail, ZTestExecuteSql,
  ZTestDataSetGeneric, ZTestData, ZTestConnection;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestcomponentall.rc}{$ENDIF}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
