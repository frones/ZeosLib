program ZeosBugTemplate;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  uBugTemplate in 'uBugTemplate.pas' {frmBugTemplate};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bug Template';
  Application.CreateForm(TfrmBugTemplate, frmBugTemplate);
  Application.Run;
end.
