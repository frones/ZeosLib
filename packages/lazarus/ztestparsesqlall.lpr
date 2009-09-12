program ztestparsesqlall;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ZTestSybaseToken, ZTestSqLiteToken,
  ZTestSqlAnalyser, ZTestScriptParser, ZTestPostgreSqlToken, ZTestOracleToken,
  ZTestMySqlToken, ZTestInterbaseToken ;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestparsesqlall.rc}{$ENDIF}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
