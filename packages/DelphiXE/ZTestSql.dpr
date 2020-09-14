program ZTestSql;

{$APPTYPE CONSOLE}

uses
  Forms,
  frmMain in '..\..\test\zsql\frmMain.pas' {MainForm},
  ZTestConfig in '..\..\test\framework\ZTestConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
