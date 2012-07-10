program ZeosBugTemplate;

uses
  Forms,
  uBugTemplate in 'uBugTemplate.pas' {frmBugTemplate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Bug Template';
  Application.CreateForm(TfrmBugTemplate, frmBugTemplate);
  Application.Run;
end.
