program ztestdbcall;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ZTestDbcResultset, ZTestDbcUtils, ZTestDbcCache,
  ZTestDbcCachedResultSet, ZTestDbcMetadata,ZTestDbcResultSetMetadata, ZTestDbcResolver,
  ZTestDbcSqLite, ZTestDbcPostgreSqlMetadata,  ZTestDbcPostgreSql, ZTestDbcOracle,
  ZTestDbcMySqlMetadata, ZTestDbcMySql, ZTestDbcMsSql, ZTestDbcInterbaseMetadata,
  ZTestDbcInterbase, ZTestDbcASA, ZTestDbcASAMetadata;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestdbcall.rc}{$ENDIF}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
