{ $Id: dunit.dpr 36 2011-04-15 19:26:16Z medington $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 36 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

program DUnit;

uses
{$IFNDEF CLR}
  {$IFDEF FASTMM}
    FastMM4,
  {$ELSE}
    ShareMem,
  {$ENDIF}
{$ENDIF}
  Windows,
  SysUtils,
  Forms,
  Dialogs,
  GUITestRunner in 'GUITestRunner.pas' {GUITestRunner},
  TestFramework in 'TestFramework.pas',
  TextTestRunner in 'TextTestRunner.pas',
  DUnitMainForm in 'DUnitMainForm.pas',
  DUnitAbout in 'DUnitAbout.pas' {DUnitAboutBox},
  TestModules in 'TestModules.pas',
  TestExtensions in 'TestExtensions.pas',
  DUnitConsts in 'DUnitConsts.pas';

{$R *.RES}
{$R versioninfo.res }

const
  rcs_id :string = '#(@)$Id: dunit.dpr 36 2011-04-15 19:26:16Z medington $';
  SwitchChars = ['-','/'];

procedure RunInConsoleMode;
var
  i :Integer;
begin
  try
    if not IsConsole then
      Windows.AllocConsole;
    for i := 1 to ParamCount do
    begin
      if not (AnsiChar(ParamStr(i)[1]) in SwitchChars) then
         RegisterModuleTests(ParamStr(i));
    end;
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
{$IFDEF VER140}
  {$IFDEF FASTMM}
    // It is Delphi 6 and FASTMM so register its known memory leaks
    RegisterExpectedMemoryLeak(36, 1); // TWinHelpViewer x 1
    RegisterExpectedMemoryLeak(20, 3); // TObjectList x 3
    RegisterExpectedMemoryLeak(20, 3); // Unknown x 3
    RegisterExpectedMemoryLeak(52, 1); // THelpManager x 1
  {$ENDIF}
{$ENDIF}

  if FindCmdLineSwitch('c', SwitchChars, true) then
    RunInConsoleMode
  else
  begin
    Application.Initialize;
    Application.Title := sTitle;
    if not SysUtils.FindCmdLineSwitch('nologo', ['/','-'], true) then
      DUnitAbout.Splash;
    Application.CreateForm(TDUnitForm, DUnitForm);
  try
      Application.Run;
    except
       on e:Exception do
         ShowMessage(e.Message);
    end;
  end;
end.
