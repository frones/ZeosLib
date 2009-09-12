program ztestcoreall;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ZTestSysUtils, ZTestVariant, ZTestTokenizer,
  ZTestList, ZTestFramework, ZTestExprToken, ZTestExpression, ZTestURL;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestcoreall.rc}{$ENDIF}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
