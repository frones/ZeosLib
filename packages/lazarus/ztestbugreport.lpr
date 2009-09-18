program ztestbugreport;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, ZTestDbcCore,
  ZTestDbcMySql, ZTestCompMySql,
  ZTestDbcPostgreSql, //ZTestCompPostgreSql,
  //ZTestDbcInterbase, //ZTestCompInterbase,
  ZTestDbcDbLib, ZTestCompDbLib,
  ZTestCompMSSql
;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestbugreport.rc}{$ENDIF}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
