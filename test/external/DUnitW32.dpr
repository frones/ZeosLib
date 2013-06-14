{ $Id: DUnitW32.dpr 23 2008-08-26 04:42:20Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 23 $
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

program DUnitW32;

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
  TestExtensions in 'TestExtensions.pas';

{$R *.RES}
{$R versioninfo.res }

const
  rcs_id :string = '#(@)$Id: DUnitW32.dpr 23 2008-08-26 04:42:20Z judc $';
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
      if not (ParamStr(i)[1] in SwitchChars) then
         RegisterModuleTests(ParamStr(i));
    end;
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  except
    on e:Exception do
      Writeln(Format('%s: %s', [e.ClassName, e.Message]));
  end;
end;

begin
  if FindCmdLineSwitch('c', SwitchChars, true) then
    RunInConsoleMode
  else
  begin
    Application.Initialize;
    Application.Title := 'DUnit - An Extreme Testing Framework';
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
