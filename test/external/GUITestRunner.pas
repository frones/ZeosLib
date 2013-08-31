{ $Id: GUITestRunner.pas 37 2011-04-15 19:43:36Z medington $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 37 $ 2001/03/08 uberto
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
 * and Juancarlo A±ez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kenneth Semeijn <dunit@designtime.demon.nl>
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit GUITestRunner;
interface

  {$I ..\..\Src\Zeos.inc}
uses
  TestFramework,

  Windows,
  Math,
  {$IFDEF WITH_VCL_PREFIX}
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.Buttons, Vcl.Menus, Vcl.ActnList, Vcl.ToolWin,
  {$ELSE}
  Graphics, Controls, Forms,
  ComCtrls, ExtCtrls, StdCtrls, ImgList, Buttons, Menus, ActnList, ToolWin,
  {$ENDIF}
  Classes, IniFiles, DUnitConsts;



const
  {: Section of the dunit.ini file where GUI information will be stored }
  cnConfigIniSection = 'GUITestRunner Config';

  {: Color constants for the progress bar and failure details panel }
  clOK      = clGreen;
  clFAILURE = clFuchsia;
  clERROR   = clRed;

  {: Indexes of the color images used in the test tree and failure list }
  imgNONE     = 0;
  imgRUNNING  = 1;
  imgRUN      = 2;
  imgHASPROPS = 3;
  imgFAILED   = 4;
  imgERROR    = 5;

  {: Indexes of the images used for test tree checkboxes }
  imgDISABLED        = 1;
  imgPARENT_DISABLED = 2;
  imgENABLED         = 3;

type
  {: Function type used by the TDUnitDialog.ApplyToTests method
     @param item  The ITest instance on which to act
     @return true if processing should continue, false otherwise
  }
  TTestFunc = function (item :ITest):Boolean of object;

  TGUITestRunner = class(TForm, ITestListener, ITestListenerX)
    StateImages: TImageList;
    RunImages: TImageList;
    DialogActions: TActionList;
    SelectAllAction: TAction;
    DeselectAllAction: TAction;
    SelectFailedAction: TAction;
    MainMenu: TMainMenu;
    TestTreeMenu: TMenuItem;
    SelectAllItem: TMenuItem;
    DeselectAllItem: TMenuItem;
    SelectFailedItem: TMenuItem;
    FileMenu: TMenuItem;
    SaveConfigurationAction: TAction;
    AutoSaveAction: TAction;
    SaveConfigurationItem: TMenuItem;
    AutoSaveItem: TMenuItem;
    RestoreSavedAction: TAction;
    RestoreSavedConfigurationItem: TMenuItem;
    ViewMenu: TMenuItem;
    HideErrorBoxItem: TMenuItem;
    BodyPanel: TPanel;
    ErrorBoxVisibleAction: TAction;
    TopPanel: TPanel;
    TreePanel: TPanel;
    TestTree: TTreeView;
    ResultsPanel: TPanel;
    ProgressPanel: TPanel;
    ResultsView: TListView;
    FailureListView: TListView;
    ErrorBoxPanel: TPanel;
    ErrorBoxSplitter: TSplitter;
    ResultsSplitter: TSplitter;
    AutoChangeFocusItem: TMenuItem;
    TopProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    pnlProgresslabel: TPanel;
    ScorePanel: TPanel;
    ScoreLabel: TPanel;
    ScoreBar: TProgressBar;
    pmTestTree: TPopupMenu;
    pmiSelectAll: TMenuItem;
    pmiDeselectAll: TMenuItem;
    pmiSelectFailed: TMenuItem;
    HideTestNodesAction: TAction;
    CollapseLowestSuiteNodesItem: TMenuItem;
    CollapseLowestSuiteNodes1: TMenuItem;
    HideTestNodesOnOpenAction: TAction;
    HideTestNodesItem: TMenuItem;
    ExpandAllNodesAction: TAction;
    TestTreeMenuSeparator: TMenuItem;
    ExpandAllItem: TMenuItem;
    TestTreeLocalMenuSeparator: TMenuItem;
    ExpandAll2: TMenuItem;
    lblTestTree: TLabel;
    RunAction: TAction;
    ExitAction: TAction;
    BreakOnFailuresAction: TAction;
    BreakonFailuresItem: TMenuItem;
    ShowTestedNodeAction: TAction;
    SelectTestedNodeItem: TMenuItem;
    ErrorMessagePopup: TPopupMenu;
    CopyFailureMessage: TMenuItem;
    CopyMessageToClipboardAction: TAction;
    ActionsMenu: TMenuItem;
    CopyMessagetoCllipboardItem: TMenuItem;
    LbProgress: TLabel;
    UseRegistryAction: TAction;
    UseRegistryItem: TMenuItem;
    ErrorMessageRTF: TRichEdit;
    SelectCurrentAction: TAction;
    DeselectCurrentAction: TAction;
    SelectCurrent1: TMenuItem;
    DeselectCurrent1: TMenuItem;
    ActionsImages: TImageList;
    CloseItem: TMenuItem;
    RunItem: TMenuItem;
    StopAction: TAction;
    StopActionItem: TMenuItem;
    ToolBar1: TToolBar;
    SelectAllButton: TToolButton;
    DeselectAllButton: TToolButton;
    ToolButton1: TToolButton;
    SelectFailedButton: TToolButton;
    ToolButton2: TToolButton;
    SelectCurrentButton: TToolButton;
    DeselectCurrentButton: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Alt_R_RunAction: TAction;
    Alt_S_StopAction: TAction;
    N1: TMenuItem;
    DeselectCurrent2: TMenuItem;
    SelectCurrent2: TMenuItem;
    N2: TMenuItem;
    CopyProcnameToClipboardAction: TAction;
    N3: TMenuItem;
    Copytestnametoclipboard1: TMenuItem;
    N4: TMenuItem;
    Copytestnametoclipboard2: TMenuItem;
    RunSelectedTestAction: TAction;
    N5: TMenuItem;
    Runcurrenttest1: TMenuItem;
    N6: TMenuItem;
    Runcurrenttest2: TMenuItem;
    RunSelectedTestItem: TMenuItem;
    RunSelectedTestButton: TToolButton;
    GoToNextSelectedTestAction: TAction;
    GoToPrevSelectedTestAction: TAction;
    N7: TMenuItem;
    GoToNextSelectedNode1: TMenuItem;
    GoToPreviousSelectedNode1: TMenuItem;
    N8: TMenuItem;
    GoToNextSelectedNode2: TMenuItem;
    GoToPreviousSelectedNode2: TMenuItem;
    FailIfNoChecksExecuted: TMenuItem;
    FailIfNoChecksExecutedAction: TAction;
    FailTestCaseIfMemoryLeaked: TMenuItem;
    FailTestCaseIfMemoryLeakedAction: TAction;
    TestCaseProperty: TPopupMenu;
    TestCaseProperties: TMenuItem;
    N10: TMenuItem;
    FailNoCheckExecutedMenuItem: TMenuItem;
    FailsOnMemoryLeakMenuItem: TMenuItem;
    N11: TMenuItem;
    TestCasePopup: TMenuItem;
    FailsOnMemoryRecoveryMenuItem: TMenuItem;
    AllowedLeakSizeMemuItem: TMenuItem;
    ShowTestCaseswithRunTimeProperties: TMenuItem;
    ShowTestCasesWithRunTimePropertiesAction: TAction;
    N9: TMenuItem;
    WarnOnFailTestOverride: TMenuItem;
    WarnOnFailTestOverrideAction: TAction;
    N12: TMenuItem;
    TestCasePropertiesAction: TAction;
    PropertyPopUpAction: TAction;
    N13: TMenuItem;
    Previous1: TMenuItem;
    Next1: TMenuItem;
    RunSelectedTest1: TMenuItem;
    RunSelectedTestAltAction: TAction;
    N14: TMenuItem;
    ReportMemoryLeakTypeOnShutdown: TMenuItem;
    IgnoreMemoryLeakInSetUpTearDown: TMenuItem;
    IgnoreMemoryLeakInSetUpTearDownAction: TAction;
    ReportMemoryLeakTypeOnShutdownAction: TAction;
    TestCaseIgnoreSetUpTearDownLeaksMenuItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TestTreeClick(Sender: TObject);
    procedure FailureListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FailureListViewClick(Sender: TObject);
    procedure TestTreeKeyPress(Sender: TObject; var Key: Char);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure DeselectAllActionExecute(Sender: TObject);
    procedure SelectFailedActionExecute(Sender: TObject);
    procedure SaveConfigurationActionExecute(Sender: TObject);
    procedure RestoreSavedActionExecute(Sender: TObject);
    procedure AutoSaveActionExecute(Sender: TObject);
    procedure ErrorBoxVisibleActionExecute(Sender: TObject);
    procedure ErrorBoxSplitterMoved(Sender: TObject);
    procedure ErrorBoxPanelResize(Sender: TObject);
    procedure HideTestNodesActionExecute(Sender: TObject);
    procedure HideTestNodesOnOpenActionExecute(Sender: TObject);
    procedure ExpandAllNodesActionExecute(Sender: TObject);
    procedure RunActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure BreakOnFailuresActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowTestedNodeActionExecute(Sender: TObject);
    procedure CopyMessageToClipboardActionExecute(Sender: TObject);
    procedure UseRegistryActionExecute(Sender: TObject);
    procedure RunActionUpdate(Sender: TObject);
    procedure CopyMessageToClipboardActionUpdate(Sender: TObject);
    procedure SelectCurrentActionExecute(Sender: TObject);
    procedure DeselectCurrentActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CopyProcnameToClipboardActionExecute(Sender: TObject);
    procedure CopyProcnameToClipboardActionUpdate(Sender: TObject);
    procedure RunSelectedTestActionExecute(Sender: TObject);
    procedure RunSelectedTestActionUpdate(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GoToNextSelectedTestActionExecute(Sender: TObject);
    procedure GoToPrevSelectedTestActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FailIfNoChecksExecutedActionExecute(Sender: TObject);
    procedure FailTestCaseIfMemoryLeakedActionExecute(Sender: TObject);
    procedure ShowTestCasesWithRunTimePropertiesActionExecute(
      Sender: TObject);
    procedure WarnOnFailTestOverrideActionExecute(Sender: TObject);
    procedure TestCasePropertiesActionExecute(Sender: TObject);
    procedure Previous1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure TestCasePropertiesMeasureItem(Sender: TObject;
      ACanvas: TCanvas; var Width, Height: Integer);
    procedure TestCasePropertiesDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure FailNoCheckExecutedMenuItemDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure AllowedLeakSizeMemuItemDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure FailsOnMemoryRecoveryMenuItemDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure FailsOnMemoryLeakMenuItemDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure pmTestTreePopup(Sender: TObject);
    procedure FailNoCheckExecutedMenuItemClick(Sender: TObject);
    procedure AllowedLeakSizeMemuItemClick(Sender: TObject);
    procedure FailsOnMemoryLeakMenuItemClick(Sender: TObject);
    procedure FailsOnMemoryRecoveryMenuItemClick(Sender: TObject);
    procedure RunSelectedTestAltActionExecute(Sender: TObject);
    procedure Previous1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure RunSelectedTest1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure Next1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure ReportMemoryLeakTypeOnShutdownActionExecute(Sender: TObject);
    procedure IgnoreMemoryLeakInSetUpTearDownActionExecute(
      Sender: TObject);
    procedure TestCaseIgnoreSetUpTearDownLeaksMenuItemClick(Sender: TObject);
    procedure TestCaseIgnoreSetUpTearDownLeaksMenuItemDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
  private
    FNoCheckExecutedPtyOverridden: Boolean;
    FMemLeakDetectedPtyOverridden: Boolean;
    FIgnoreSetUpTearDownLeakPtyOverridden: Boolean;
    FPopupY: Integer;
    FPopupX: Integer;
    procedure ResetProgress;
    procedure MenuLooksInactive(ACanvas: TCanvas; ARect: TRect; Selected: Boolean;
      ATitle: string; TitlePosn: UINT; PtyOveridesGUI: boolean);
    procedure MenuLooksActive(ACanvas: TCanvas; ARect: TRect; Selected: Boolean;
      ATitle: string; TitlePosn: UINT);
    function  GetPropertyName(const Caption: string): string;
  protected
    FSuite:         ITest;
    FTestResult:    TTestResult;
    FRunning:       Boolean;
    FTests:         TInterfaceList;
    FSelectedTests: TInterfaceList;
    FTotalTime:     Int64;
    FRunTimeStr:    string;
    FNoChecksStr:   string;
    FMemLeakStr:    string;
    FMemGainStr:    string;
    FMemBytesStr:   string;
    FIgnoreLeakStr: string;
    FBytes:         string;
    FErrorCount:    Integer;
    FFailureCount:  Integer;
    FStrMaxLen:     Integer;
    FValMaxLen:     Integer;
    FUpdateTimer:   TTimer;
    FTimerExpired:  Boolean;
    FTotalTestsCount: Integer;

    procedure Setup;
    procedure SetUpStateImages;
    procedure SetSuite(value: ITest);
    procedure ClearResult;
    procedure DisplayFailureMessage(Item :TListItem);
    procedure ClearFailureMessage;

    function  AddFailureItem(failure: TTestFailure): TListItem;
    procedure UpdateStatus(const fullUpdate:Boolean);

    procedure FillTestTree(RootNode: TTreeNode; ATest: ITest); overload;
    procedure FillTestTree(ATest: ITest);                      overload;

    procedure UpdateNodeImage(node: TTreeNode);
    procedure UpdateNodeState(node: TTreeNode);
    procedure SetNodeState(node: TTreeNode; enabled :boolean);
    procedure SwitchNodeState(node: TTreeNode);
    procedure UpdateTestTreeState;

    procedure MakeNodeVisible(node :TTreeNode);
    procedure SetTreeNodeImage(Node :TTReeNode; imgIndex :Integer);
    procedure SelectNode(node: TTreeNode);

    function  NodeToTest(node :TTreeNode) :ITest;
    function  TestToNode(test :ITest) :TTreeNode;
    function  SelectedTest :ITest;
    procedure ListSelectedTests;

    function  EnableTest(test :ITest) : boolean;
    function  DisableTest(test :ITest) : boolean;
    procedure ApplyToTests(root :TTreeNode; const func :TTestFunc);

    procedure EnableUI(enable :Boolean);
    procedure RunTheTest(aTest: ITest);

    procedure InitTree; virtual;

    function  IniFileName :string;
    function  GetIniFile( const FileName : string ) : tCustomIniFile;

    procedure LoadRegistryAction;
    procedure SaveRegistryAction;

    procedure LoadFormPlacement;
    procedure SaveFormPlacement;

    procedure SaveConfiguration;
    procedure LoadConfiguration;

    procedure LoadSuiteConfiguration;
    procedure AutoSaveConfiguration;

    function NodeIsGrandparent(ANode: TTreeNode): boolean;
    procedure CollapseNonGrandparentNodes(RootNode: TTreeNode);

    procedure ProcessClickOnStateIcon;
    procedure ClearStatusMessage;

    procedure CopyTestNametoClipboard(ANode: TTreeNode);

    procedure SetupCustomShortcuts;
    procedure SetupGUINodes;

    function SelectNodeIfTestEnabled(ANode: TTreeNode): boolean;

    procedure OnUpdateTimer(Sender: TObject);
  public
    {: implement the ITestListener interface }
    procedure AddSuccess(test: ITest);
    procedure AddError(failure: TTestFailure);
    procedure AddFailure(failure: TTestFailure);
    function  ShouldRunTest(test :ITest):boolean;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts;
    procedure TestingEnds(TestResult :TTestResult);
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    {: The number of errors in the last test run }
    property ErrorCount: Integer read FErrorCount;
    {: The number of failures in the last test run }
    property FailureCount: Integer read FFailureCount;
    {: The test suite to be run in this runner }
    property Suite: ITest read FSuite write SetSuite;
    {: The result of the last test run }
    property TestResult : TTestResult read FTestResult write FTestResult;

    class procedure RunTest(test: ITest);
    class procedure RunRegisteredTests;
  end;

procedure RunTest(test: ITest);
procedure RunRegisteredTests;

procedure RunTestModeless(test: ITest);
procedure RunRegisteredTestsModeless;

// Run all tests in unattended mode, i.e. automatically
function RunRegisteredTestsModelessUnattended: Integer;

implementation

uses
{$IFDEF FASTMM}
  {$IFNDEF CLR}
    {$IFNDEF ManualLeakReportingControl}
      {$I FastMM4Options.inc}
    {$ENDIF}
    FastMM4,
  {$ENDIF}
{$ENDIF}
  Registry,
  SysUtils,
  {$IFDEF WITH_VCL_PREFIX}
  Vcl.Clipbrd
  {$ELSE}
  Clipbrd
  {$ENDIF};

{$BOOLEVAL OFF}  // Required or you'll get an AV
{$R *.DFM}

type
  TProgressBarCrack = class(TProgressBar);

procedure RunTest(test: ITest);
begin
  with TGUITestRunner.Create(nil) do
  begin
    try
      Suite := test;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure RunTestModeless(test: ITest);
var
  GUI :TGUITestRunner;
begin
  Application.CreateForm(TGUITestRunner, GUI);
  GUI.Suite := test;
  GUI.Show;
end;

procedure RunRegisteredTests;
begin
   RunTest(registeredTests)
end;

procedure RunRegisteredTestsModeless;
begin
   RunTestModeless(registeredTests)
end;

// Run all tests in unattended mode, i.e. automatically
function RunRegisteredTestsModelessUnattended: Integer;
var
  GUI :TGUITestRunner;
begin
  // Create and show the GUI runner form

  Application.CreateForm(TGUITestRunner, GUI);
  GUI.Suite := registeredTests;
  GUI.Show;

  GUI.RunActionExecute(GUI.RunItem);

  // Process messages until the tests have finished

  repeat
    try
      Application.HandleMessage;
    except
      Application.HandleException(Application);
    end;
  until TGUITestRunner(Application.MainForm).RunAction.Enabled;

  // Return the number of errors and failures and free the runner form

  Result := GUI.ErrorCount + GUI.FailureCount;

  GUI.Free;
end;

{ TGUITestRunner }

procedure TGUITestRunner.InitTree;
begin
  FTests.Clear;
  FillTestTree(Suite);
  Setup;
  if HideTestNodesOnOpenAction.Checked then
    HideTestNodesAction.Execute
  else
    ExpandAllNodesAction.Execute;
  TestTree.Selected := TestTree.Items.GetFirstNode;
end;

function TGUITestRunner.NodeToTest(Node: TTreeNode): ITest;
var
  idx: Integer;
begin
  assert(assigned(Node));

  idx  := NativeInt(Node.data);
  assert((idx >= 0) and (idx < FTests.Count));
  result := FTests[idx] as ITest;
end;

procedure TGUITestRunner.OnUpdateTimer(Sender: TObject);
begin
  FTimerExpired := True;
  FUpdateTimer.Enabled := False;
end;

function TGUITestRunner.TestToNode(test: ITest): TTreeNode;
begin
  assert(assigned(test));

  Result := test.GUIObject as TTreeNode;

  assert(assigned(Result));
end;

function TGUITestRunner.ShouldRunTest(test: ITest): boolean;
begin
  if FSelectedTests = nil then
    Result := test.Enabled
  else
    Result := FSelectedTests.IndexOf(test as ITest) >= 0;
end;

procedure TGUITestRunner.StartTest(test: ITest);
var
  node :TTreeNode;
begin
  assert(assigned(TestResult));
  assert(assigned(test));
  node := TestToNode(test);
  assert(assigned(node));
  SetTreeNodeImage(node, imgRunning);
  if ShowTestedNodeAction.Checked then
  begin
    MakeNodeVisible(node);
    TestTree.Update;
  end;
  ClearStatusMessage;
  UpdateStatus(False);
end;

procedure TGUITestRunner.EndTest(test: ITest);
begin
  UpdateStatus(False);
end;

procedure TGUITestRunner.TestingStarts;
begin
  FTotalTime := 0;
  UpdateStatus(True);
  TProgressBarCrack(ScoreBar).Color := clOK;
  TProgressBarCrack(ScoreBar).RecreateWnd;
end;

procedure TGUITestRunner.AddSuccess(test: ITest);
var
  OverridesGUI: Boolean;
  HasRunTimePropsSet: Boolean;
begin
  assert(assigned(test));
  if not IsTestMethod(test) then
    SetTreeNodeImage(TestToNode(Test), imgRun)
  else
  begin
    OverridesGUI :=
      ((FailIfNoChecksExecuted.Checked and not Test.FailsOnNoChecksExecuted) or
       (FailTestCaseIfMemoryLeaked.Checked and not Test.FailsOnMemoryLeak)) or
       (FailTestCaseIfMemoryLeaked.Checked and Test.IgnoreSetUpTearDownLeaks and
         not IgnoreMemoryLeakInSetUpTearDown.Checked);
    HasRunTimePropsSet :=
      ((Test.FailsOnNoChecksExecuted and not FailIfNoChecksExecuted.Checked) or
       (Test.FailsOnMemoryLeak and not FailTestCaseIfMemoryLeaked.Checked) or
       (FailTestCaseIfMemoryLeaked.Checked and Test.IgnoreSetUpTearDownLeaks) or
       (Test.AllowedMemoryLeakSize <> 0));

    if OverridesGUI then
      FTestResult.Overrides := FTestResult.Overrides + 1;

    if (WarnOnFailTestOverride.Checked and OverridesGUI) or
       (ShowTestCaseswithRunTimeProperties.Checked and HasRunTimePropsSet) then
      SetTreeNodeImage(TestToNode(Test), imgHASPROPS)
    else
      SetTreeNodeImage(TestToNode(Test), imgRun);
  end;
end;

procedure TGUITestRunner.AddError(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgERROR;
  TProgressBarCrack(ScoreBar).Color := clERROR;
  TProgressBarCrack(ScoreBar).RecreateWnd;

  SetTreeNodeImage(TestToNode(failure.failedTest), imgERROR);
  UpdateStatus(False);
end;

procedure TGUITestRunner.AddFailure(failure: TTestFailure);
var
  ListItem: TListItem;
begin
  ListItem := AddFailureItem(failure);
  ListItem.ImageIndex := imgFAILED;
  if TestResult.errorCount = 0 then
  begin
    TProgressBarCrack(ScoreBar).Color := clFAILURE;
    TProgressBarCrack(ScoreBar).RecreateWnd;
  end;
  SetTreeNodeImage(TestToNode(failure.failedTest), imgFAILED);
  UpdateStatus(False);
end;

function TGUITestRunner.IniFileName: string;
const
  TEST_INI_FILE = 'dunit.ini';
begin
    result := ExtractFilePath(Application.ExeName) + TEST_INI_FILE
end;

procedure TGUITestRunner.LoadFormPlacement;
begin
  with GetIniFile( IniFileName ) do
  try
    Self.SetBounds(
                   ReadInteger(cnConfigIniSection, 'Left',   Left),
                   ReadInteger(cnConfigIniSection, 'Top',    Top),
                   ReadInteger(cnConfigIniSection, 'Width',  Width),
                   ReadInteger(cnConfigIniSection, 'Height', Height)
                   );
    if ReadBool(cnConfigIniSection, 'Maximized', False ) then
      WindowState := wsMaximized;
  finally
    Free;
  end;
end;

procedure TGUITestRunner.SaveFormPlacement;
begin
  with GetIniFile(IniFileName) do
    try
      WriteBool(cnConfigIniSection, 'AutoSave', AutoSaveAction.Checked);

      if WindowState <> wsMaximized then
      begin
        WriteInteger(cnConfigIniSection, 'Left',   Left);
        WriteInteger(cnConfigIniSection, 'Top',    Top);
        WriteInteger(cnConfigIniSection, 'Width',  Width);
        WriteInteger(cnConfigIniSection, 'Height', Height );
      end;

      WriteBool(cnConfigIniSection, 'Maximized', WindowState = wsMaximized );
    finally
      Free
    end;
end;

procedure TGUITestRunner.LoadConfiguration;
var
  i :Integer;
begin
  LoadRegistryAction;
  LoadFormPlacement;
  LoadSuiteConfiguration;
  with GetIniFile(IniFileName) do
  try
    with AutoSaveAction do
      Checked := ReadBool(cnConfigIniSection, 'AutoSave', Checked);

    { center splitter location }
    with ResultsPanel do
      Height := ReadInteger(cnConfigIniSection, 'ResultsPanel.Height', Height);

    { error splitter location }
    with ErrorBoxPanel do
      Height := ReadInteger(cnConfigIniSection, 'ErrorMessage.Height', Height);
    with ErrorBoxVisibleAction do
      Checked := ReadBool(cnConfigIniSection, 'ErrorMessage.Visible', Checked);

    ErrorBoxSplitter.Visible := ErrorBoxVisibleAction.Checked;
    ErrorBoxPanel.Visible    := ErrorBoxVisibleAction.Checked;

    { failure list configuration }
    with FailureListView do begin
      for i := 0 to Columns.Count-1 do
      begin
        Columns[i].Width := Max(4, ReadInteger(cnConfigIniSection,
                                        Format('FailureList.ColumnWidth[%d]', [i]),
                                        Columns[i].Width)
                                        );
      end;
    end;

    { other options }
    HideTestNodesOnOpenAction.Checked := ReadBool(cnConfigIniSection,
      'HideTestNodesOnOpen', HideTestNodesOnOpenAction.Checked);
    BreakOnFailuresAction.Checked := ReadBool(cnConfigIniSection,
      'BreakOnFailures', BreakOnFailuresAction.Checked);
    FailIfNoChecksExecutedAction.Checked := ReadBool(cnConfigIniSection,
      'FailOnNoChecksExecuted', FailIfNoChecksExecutedAction.Checked);
    FailTestCaseIfMemoryLeakedAction.Checked := ReadBool(cnConfigIniSection,
      'FailOnMemoryLeaked', FailTestCaseIfMemoryLeakedAction.Checked);
    IgnoreMemoryLeakInSetUpTearDownAction.Checked := ReadBool(cnConfigIniSection,
      'IgnoreSetUpTearDownLeaks', IgnoreMemoryLeakInSetUpTearDownAction.Checked);
    ReportMemoryLeakTypeOnShutdownAction.Checked := ReadBool(cnConfigIniSection,
      'ReportMemoryLeakTypes', ReportMemoryLeakTypeOnShutdownAction.Checked);
    WarnOnFailTestOverrideAction.Checked := ReadBool(cnConfigIniSection,
      'WarnOnFailTestOverride', WarnOnFailTestOverrideAction.Checked);
    ShowTestedNodeAction.Checked := ReadBool(cnConfigIniSection,
      'SelectTestedNode', ShowTestedNodeAction.Checked);
    FPopupX := ReadInteger(cnConfigIniSection,'PopupX', 350);
    FPopupY := ReadInteger(cnConfigIniSection,'PopupY', 30);
  finally
    Free;
  end;

  if Suite <> nil then
    UpdateTestTreeState;
end;

procedure TGUITestRunner.AutoSaveConfiguration;
begin
  if AutoSaveAction.Checked then
    SaveConfiguration;
end;

procedure TGUITestRunner.SaveConfiguration;
var
  i :Integer;
begin
  if Suite <> nil then
    Suite.SaveConfiguration(IniFileName, UseRegistryAction.Checked, True);

  SaveFormPlacement;
  SaveRegistryAction;

  with GetIniFile(IniFileName) do
  try
    { center splitter location }
    WriteInteger(cnConfigIniSection, 'ResultsPanel.Height',
      ResultsPanel.Height);

    { error box }
    WriteInteger(cnConfigIniSection, 'ErrorMessage.Height',
      ErrorBoxPanel.Height);
    WriteBool(cnConfigIniSection, 'ErrorMessage.Visible',
      ErrorBoxVisibleAction.Checked);

    { failure list configuration }
    with FailureListView do begin
      for i := 0 to Columns.Count-1 do
      begin
       WriteInteger( cnConfigIniSection,
                     Format('FailureList.ColumnWidth[%d]', [i]),
                     Columns[i].Width);
      end;
    end;

    { other options }
    WriteBool(cnConfigIniSection, 'HideTestNodesOnOpen',      HideTestNodesOnOpenAction.Checked);
    WriteBool(cnConfigIniSection, 'BreakOnFailures',          BreakOnFailuresAction.Checked);
    WriteBool(cnConfigIniSection, 'FailOnNoChecksExecuted',   FailIfNoChecksExecutedAction.Checked);
    WriteBool(cnConfigIniSection, 'FailOnMemoryLeaked',       FailTestCaseIfMemoryLeakedAction.Checked);
    WriteBool(cnConfigIniSection, 'IgnoreSetUpTearDownLeaks', IgnoreMemoryLeakInSetUpTearDownAction.Checked);
    WriteBool(cnConfigIniSection, 'ReportMemoryLeakTypes',    ReportMemoryLeakTypeOnShutdownAction.Checked);
    WriteBool(cnConfigIniSection, 'SelectTestedNode',         ShowTestedNodeAction.Checked);
    WriteBool(cnConfigIniSection, 'WarnOnFailTestOverride',   WarnOnFailTestOverrideAction.Checked);
    WriteInteger(cnConfigIniSection, 'PopupX',                FPopupX);
    WriteInteger(cnConfigIniSection, 'PopupY',                FPopupY);

  finally
    Free;
  end;
end;

procedure TGUITestRunner.TestingEnds(TestResult :TTestResult);
begin
  FTotalTime := TestResult.TotalTime;
end;

procedure TGUITestRunner.UpdateNodeState(node: TTreeNode);
var
  test: ITest;
begin
  assert(assigned(node));
  test := NodeToTest(node);
  assert(assigned(test));

  UpdateNodeImage(node);

  if node.HasChildren then
  begin
    node := node.getFirstChild;
    while node <> nil do
    begin
      UpdateNodeState(node);
      node := node.getNextSibling;
    end;
  end;
end;

procedure TGUITestRunner.SetNodeState(node: TTreeNode; enabled :boolean);
var
  MostSeniorChanged :TTReeNode;
begin
   assert(node <> nil);

   // update ancestors if enabling
   NodeToTest(Node).Enabled := enabled;

   MostSeniorChanged := Node;
   if enabled then
   begin
     while Node.Parent <> nil do
     begin
       Node := Node.Parent;
       if not NodeToTest(Node).Enabled then
       begin // changed
          NodeToTest(Node).Enabled := true;
          MostSeniorChanged := Node;
          UpdateNodeImage(Node);
       end
     end;
   end;
   TestTree.Items.BeginUpdate;
   try
     UpdateNodeState(MostSeniorChanged);
   finally
     TestTree.Items.EndUpdate;
   end
end;

procedure TGUITestRunner.SwitchNodeState(node: TTreeNode);
begin
   assert(node <> nil);

   SetNodeState(node, not NodeToTest(node).enabled);
end;

procedure TGUITestRunner.UpdateTestTreeState;
var
  node :TTreeNode;
begin
  if TestTree.Items.Count > 0 then
  begin
    TestTree.Items.BeginUpdate;
    try
      node := TestTree.Items.GetFirstNode;
      while node <> nil do
      begin
        UpdateNodeState(node);
        node := node.getNextSibling;
      end
    finally
      TestTree.Items.EndUpdate;
    end;
  end;
end;

procedure TGUITestRunner.UpdateStatus(const fullUpdate:Boolean);
var
  i :Integer;
  TestNumber: Integer;

   function FormatElapsedTime(milli: Int64):string;
   var
     h,nn,ss,zzz: Cardinal;
   begin
     h := milli div 3600000;
     milli := milli mod 3600000;
     nn := milli div 60000;
     milli := milli mod 60000;
     ss := milli div 1000;
     milli := milli mod 1000;
     zzz := milli;
     Result := Format('%d:%2.2d:%2.2d.%3.3d', [h, nn, ss, zzz]);
   end;
begin
  if ResultsView.Items.Count = 0 then
    Exit;

  if fullUpdate then
  begin
    FTotalTestsCount := Suite.countEnabledTestCases;
    if Assigned(Suite) then
      ResultsView.Items[0].SubItems[0] := IntToStr(FTotalTestsCount)
    else
      ResultsView.Items[0].SubItems[0] := '';
  end;
  
  if TestResult <> nil then
  begin
    // Save the test number as we use it a lot
    TestNumber := TestResult.runCount;

    if fullUpdate or FTimerExpired or ((TestNumber and 15) = 0) then
    begin
      with ResultsView.Items[0] do
      begin
        SubItems[1] := IntToStr(TestNumber);
        SubItems[2] := IntToStr(TestResult.failureCount);
        SubItems[3] := IntToStr(TestResult.errorCount);
        SubItems[4] := IntToStr(TestResult.Overrides);
        SubItems[5] := FormatElapsedTime(TestResult.TotalTime);
        SubItems[6] := FormatElapsedTime(max(TestResult.TotalTime, FTotalTime));
      end;
      with TestResult do
      begin
        ScoreBar.Position  := TestNumber - (failureCount + errorCount);
        ProgressBar.Position := TestNumber;

        // There is a possibility for zero tests
        if (TestNumber = 0) and (Suite.CountEnabledTestCases = 0) then
          LbProgress.Caption := '100%'
        else
          LbProgress.Caption := IntToStr((100 * ScoreBar.Position) div ScoreBar.Max) + '%';
      end;
      if FTimerExpired and (TestNumber < FTotalTestsCount) then
      begin
        FTimerExpired := False;
        FUpdateTimer.Enabled := True;
      end;
    end;
    // Allow just the results pane to catch up

    ResultsPanel.Update;
  end
  else
  begin
    with ResultsView.Items[0] do
    begin
      if (SubItems[0] = '0') or (subItems[0] = '') then
      begin
        for i := 1 to 6 do
          SubItems[i] := ''
      end
      else
      begin
        if SubItems[0] <> subItems[1] then
          for i := 1 to 6 do
            SubItems[i] := ''
        else
        begin
          SubItems[5] := FormatElapsedTime(SelectedTest.ElapsedTestTime);
          SubItems[6] := FormatElapsedTime(Max(SelectedTest.ElapsedTestTime, FTotalTime));
        end;
      end;
    end;

    ResetProgress;
  end;

  if fullUpdate then
  begin
    // Allow the whole display to catch up and check for key strokes

    Update;
    Application.ProcessMessages;
  end;
end;

procedure TGUITestRunner.ResetProgress;
begin
  TProgressBarCrack(ScoreBar).ParentColor := True;
  TProgressBarCrack(ScoreBar).RecreateWnd;
  ScoreBar.Position := 0;
  ProgressBar.Position := 0;
  LbProgress.Caption := '';
end;

function TGUITestRunner.AddFailureItem(failure: TTestFailure): TListItem;
var
  item : TListItem;
  node : TTreeNode;
begin
  assert(assigned(failure));
  item := FailureListView.Items.Add;
  item.data := Pointer(TestToNode(failure.failedTest));
  item.Caption := failure.failedTest.Name;
  item.SubItems.Add(failure.thrownExceptionName);
  item.SubItems.Add(failure.thrownExceptionMessage);
  item.SubItems.Add( failure.LocationInfo
                     + ' ' +
                     failure.AddressInfo
                     );
  item.SubItems.Add(failure.StackTrace);

  node := testToNode(failure.failedTest);
  while node <> nil do
  begin
    node.Expand(false);
    node := node.Parent;
  end;

  Result := item;
end;

procedure TGUITestRunner.FillTestTree(RootNode: TTreeNode; ATest: ITest);
var
  TestTests: IInterfaceList;
  i: Integer;
begin
  if ATest = nil then
    EXIT;

  RootNode := TestTree.Items.AddChild(RootNode, ATest.Name);
  RootNode.data := TObject(FTests.Add(ATest));

  TestTests := ATest.Tests;
  for i := 0 to TestTests.count - 1 do
  begin
    FillTestTree(RootNode, TestTests[i] as ITest);
  end;
end;

procedure TGUITestRunner.FillTestTree(ATest: ITest);
begin
  TestTree.Items.Clear;
  FTests.Clear;
  fillTestTree(nil, Suite);
end;

procedure TGUITestRunner.SetTreeNodeImage(Node :TTReeNode; imgIndex :Integer);
begin
  while Node <> nil do
  begin
    if imgIndex > Node.ImageIndex then
    begin
       Node.ImageIndex    := imgIndex;
       Node.SelectedIndex := imgIndex;
    end;
    if imgIndex = imgRunning then
      Node := nil
    else
      Node := Node.Parent;
  end;
end;

procedure TGUITestRunner.SetSuite(value: ITest);
begin
  FSuite := value;
  if FSuite <> nil then
  begin
    LoadSuiteConfiguration;
    EnableUI(True);
    InitTree;
  end
  else
    EnableUI(False)
end;

procedure TGUITestRunner.DisplayFailureMessage(Item: TListItem);
var
  hlColor :TColor;
  Test    :ITest;
  Status  :string;
begin
  TestTree.Selected := TTreeNode(Item.data);
  Test := NodeToTest(TestTree.Selected);
  hlColor := clFAILURE;
  if Item.ImageIndex >= imgERROR then
     hlColor := clERROR;
  with ErrorMessageRTF do
    begin
      Clear;
      SelAttributes.Size  := self.Font.Size;
      SelAttributes.Style := [fsBold];
      SelText := Item.Caption + ': ';

      SelAttributes.Color := hlColor;
      SelAttributes.Style := [fsBold];
      SelText := Item.SubItems[0];

      Lines.Add('');
      SelAttributes.Color := clWindowText;
      SelAttributes.Style := [];
      SelText := 'at ' + Item.SubItems[2];

      if Item.SubItems[1] <> '' then
      begin
        SelAttributes.Color := clWindowText;
        Lines.Add('');
        SelAttributes.Size  := 12;
        SelAttributes.Style := [];
        SelText := Item.SubItems[1];
        SelAttributes.Size  := self.Font.Size;
      end;

      Status := Test.Status;
      if Status <> '' then
      begin
        Lines.Add('');
        Lines.Add('');
        SelAttributes.Style := [fsBold];
        Lines.Add('Status Messages');
        SelAttributes.Style := [];
        Lines.Add(Status);
      end;

      if Item.SubItems[3] <> '' then
      begin
        Lines.Add('');
        SelAttributes.Style := [fsBold];
        Lines.Add('StackTrace');
        SelAttributes.Style := [];
        SelText := Item.SubItems[3];
      end;
    end
end;

procedure TGUITestRunner.ClearFailureMessage;
begin
  ErrorMessageRTF.Clear;
end;

procedure TGUITestRunner.ClearResult;
begin
  if FTestResult <> nil then
  begin
    FTestResult.Free;
    FTestResult := nil;
    ClearFailureMessage;
  end;
end;

procedure TGUITestRunner.SetUp;
var
  i: Integer;
  node: TTreeNode;
begin
  FailureListView.Items.Clear;
  ResetProgress;
  Update;

  with ResultsView.Items[0] do
  begin
    if Suite <> nil then
    begin
      i := Suite.countEnabledTestCases;
      SubItems[0] := IntToStr(i);
      ProgressBar.Max := i
    end
    else
    begin
      SubItems[0] := '';
      ProgressBar.Max:= 10000;
    end;
    ScoreBar.Max := ProgressBar.Max;

    SubItems[1] := '';
    SubItems[2] := '';
    SubItems[3] := '';
    SubItems[4] := '';
    SubItems[5] := '';
    SubItems[6] := '';
  end;

  for i := 0 to TestTree.Items.Count - 1 do
  begin
    node := TestTree.Items[i];
    node.ImageIndex    := imgNONE;
    node.SelectedIndex := imgNONE;
  end;
  UpdateTestTreeState;
end;

procedure TGUITestRunner.EnableUI(enable: Boolean);
begin
  SelectAllAction.Enabled    := enable;
  DeselectAllAction.Enabled  := enable;
  SelectFailedAction.Enabled := enable;
  SelectCurrentAction.Enabled := enable;
  DeselectCurrentAction.Enabled := enable;
  HideTestNodesAction.Enabled   := enable;
  ExpandAllNodesAction.Enabled  := enable;
end;

procedure TGUITestRunner.FormCreate(Sender: TObject);
begin
  inherited;
  FTests := TInterfaceList.Create;
  LoadConfiguration;

  {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator := ':';
  SetUpStateImages;
  SetupCustomShortcuts;
  TestTree.Items.Clear;
  EnableUI(false);
  ClearFailureMessage;
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 200;
  FUpdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := OnUpdateTimer;
  Setup;

  {$IFDEF FASTMM}
    FailTestCaseIfMemoryLeakedAction.Enabled := True;
    {$IFDEF ManualLeakReportingControl}
      ReportMemoryLeaksOnShutdown := ReportMemoryLeakTypeOnShutdownAction.Checked;
    {$ELSE}
      ReportMemoryLeakTypeOnShutdownAction.Checked := False;
      ReportMemoryLeakTypeOnShutdownAction.Enabled := False;
    {$ENDIF}
  {$ELSE}
    FailTestCaseIfMemoryLeakedAction.Enabled := False;
    ReportMemoryLeakTypeOnShutdownAction.Checked := False;
    ReportMemoryLeakTypeOnShutdownAction.Enabled := False;
  {$ENDIF}

  if not FailTestCaseIfMemoryLeakedAction.Enabled then
    FailTestCaseIfMemoryLeakedAction.Checked := False;
  IgnoreMemoryLeakInSetUpTearDownAction.Enabled :=
    FailTestCaseIfMemoryLeakedAction.Checked;
  if not IgnoreMemoryLeakInSetUpTearDownAction.Enabled then
    IgnoreMemoryLeakInSetUpTearDownAction.Checked := False;
end;

procedure TGUITestRunner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUpdateTimer);
  ClearResult;
  AutoSaveConfiguration;
  Suite := nil;
  FTests.Free;
  FTests := nil;
  inherited;
end;

procedure TGUITestRunner.FormShow(Sender: TObject);
begin
  { Set up the GUI nodes in the test nodes. We do it here because the form,
    the tree and all its tree nodes get recreated in TCustomForm.ShowModal
    in D8+ so we cannot do it sooner. }

  SetupGUINodes;
end;

procedure TGUITestRunner.TestTreeClick(Sender: TObject);
begin
  if FRunning then
    EXIT;

  ProcessClickOnStateIcon;
  TestTreeChange(Sender, TestTree.Selected);
end;

procedure TGUITestRunner.TestTreeChange(Sender: TObject; Node: TTreeNode);
var
  i : Integer;
begin
  if (Node <> nil) and (Node = TestTree.Selected) then
  begin
    FailureListView.Selected := nil;
    for i := 0 to FailureListView.Items.count - 1 do
    begin
      if TTreeNode(FailureListView.Items[i].Data) = Node then
      begin
        FailureListView.Selected := FailureListView.Items[i];
        break;
      end;
    end;
    UpdateStatus(True);
  end;
end;

procedure TGUITestRunner.FailureListViewClick(Sender: TObject);
begin
  if FailureListView.Selected <> nil then
  begin
    TestTree.Selected := TTreeNode(FailureListView.Selected.data);
  end;
end;

procedure TGUITestRunner.FailureListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    ClearFailureMessage
  else
    DisplayFailureMessage(Item);
end;

function TGUITestRunner.DisableTest(test: ITest): boolean;
begin
  test.enabled := false;
  result := true;
end;

function TGUITestRunner.EnableTest(test: ITest): boolean;
begin
  test.enabled := true;
  result := true;
end;

procedure TGUITestRunner.ApplyToTests(root :TTreeNode; const func :TTestFunc);

  procedure DoApply(rootnode :TTreeNode);
  var
    test: ITest;
    node: TTreeNode;
  begin
    if rootnode <> nil then
    begin
      test := NodeToTest(rootnode);
      if func(test) then
      begin
        node := rootnode.getFirstChild;
        while node <> nil do
        begin
          DoApply(node);
          node := node.getNextSibling;
        end;
      end;
    end;
  end;
begin
  TestTree.Items.BeginUpdate;
  try
    DoApply(root)
  finally
    TestTree.Items.EndUpdate
  end;
  UpdateTestTreeState;
end;

procedure TGUITestRunner.TestTreeKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ' ') and (TestTree.Selected <> nil) then
  begin
    SwitchNodeState(TestTree.Selected);
    UpdateStatus(True);
    Key := #0
  end;
end;

procedure TGUITestRunner.SelectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, EnableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.DeselectAllActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Items.GetFirstNode, DisableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.SelectFailedActionExecute(Sender: TObject);
var
  i: integer;
  ANode: TTreeNode;
begin
  { deselect all }
  ApplyToTests(TestTree.Items[0], DisableTest);

  { select failed }
  for i := 0 to FailureListView.Items.Count - 1 do
  begin
    ANode := TTreeNode(FailureListView.Items[i].Data);
    SetNodeState(ANode, true);
  end;
  UpdateStatus(True);
end;

procedure TGUITestRunner.SaveConfigurationActionExecute(Sender: TObject);
begin
  SaveConfiguration
end;

procedure TGUITestRunner.RestoreSavedActionExecute(Sender: TObject);
begin
  LoadConfiguration
end;

procedure TGUITestRunner.AutoSaveActionExecute(Sender: TObject);
begin
  with AutoSaveAction do
  begin
    Checked := not Checked
  end;
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.ErrorBoxVisibleActionExecute(Sender: TObject);
begin
   with ErrorBoxVisibleAction do
   begin
     Checked := not Checked;
     ErrorBoxSplitter.Visible := Checked;
     ErrorBoxPanel.Visible    := Checked;
     if Checked then
     begin
      // Solve bugs with Delphi4 resizing with constraints
       ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
     end
   end;
end;

procedure TGUITestRunner.ErrorBoxSplitterMoved(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
  self.Update;
end;

procedure TGUITestRunner.ErrorBoxPanelResize(Sender: TObject);
begin
  // Solve bugs with Delphi4 resizing with constraints
  ErrorBoxSplitter.Top := ErrorBoxPanel.Top-8;
end;

function TGUITestRunner.NodeIsGrandparent(ANode: TTreeNode): boolean;
var
  AChildNode: TTreeNode;
begin
  Result := false;
  if ANode.HasChildren then
  begin
    AChildNode := ANode.GetFirstChild;
    while AChildNode <> nil do
    begin
      Result := AChildNode.HasChildren or Result;
      AChildNode := ANode.GetNextChild(AChildNode);
    end;
  end;
end;

procedure TGUITestRunner.CollapseNonGrandparentNodes(RootNode: TTreeNode);
var
  AChildNode: TTreeNode;
begin
  if not NodeIsGrandparent(RootNode) then
    RootNode.Collapse(false);

  AChildNode := RootNode.GetFirstChild;
  while AChildNode <> nil do
  begin
    CollapseNonGrandparentNodes(AChildNode);
    AChildNode := RootNode.GetNextChild(AChildNode);
  end;
end;

procedure TGUITestRunner.HideTestNodesActionExecute(Sender: TObject);
var
  ANode: TTreeNode;
begin
  inherited;
  if TestTree.Items.Count = 0 then
    EXIT;

  TestTree.Items.BeginUpdate;
  try
    ANode := TestTree.Items[0];
    if ANode <> nil then
    begin
      ANode.Expand(true);
      CollapseNonGrandparentNodes(ANode);
      SelectNode(ANode);
    end;
  finally
    TestTree.Items.EndUpdate;
  end;
end;

procedure TGUITestRunner.HideTestNodesOnOpenActionExecute(Sender: TObject);
begin
  HideTestNodesOnOpenAction.Checked := not HideTestNodesOnOpenAction.Checked;
end;

procedure TGUITestRunner.ExpandAllNodesActionExecute(Sender: TObject);
begin
  TestTree.FullExpand;
  if (TestTree.Selected <> nil) then
    MakeNodeVisible(TestTree.Selected)
  else if(TestTree.Items.Count > 0) then
    TestTree.Selected := TestTree.Items[0];
end;

procedure TGUITestRunner.RunTheTest(aTest : ITest);
begin
  if aTest = nil then
    EXIT;
  if FRunning then
  begin
    // warning: we're reentering this method if FRunning is true
    assert(FTestResult <> nil);
    FTestResult.Stop;
    EXIT;
  end;

  FRunning := true;
  try
    RunAction.Enabled  := False;
    StopAction.Enabled := True;

    CopyMessageToClipboardAction.Enabled := false;

    EnableUI(false);
    AutoSaveConfiguration;
    ClearResult;
    TestResult := TTestResult.create;
    try
      TestResult.addListener(self);
      TestResult.BreakOnFailures := BreakOnFailuresAction.Checked;
      TestResult.FailsIfNoChecksExecuted := FailIfNoChecksExecutedAction.Checked;
      TestResult.FailsIfMemoryLeaked := FailTestCaseIfMemoryLeakedAction.Checked;
      TestResult.IgnoresMemoryLeakInSetUpTearDown :=
        IgnoreMemoryLeakInSetUpTearDownAction.Checked;
      aTest.run(TestResult);
    finally
      FErrorCount := TestResult.ErrorCount;
      FFailureCount := TestResult.FailureCount;
      TestResult.Free;
      TestResult := nil;
    end;
  finally
      FRunning := false;
      EnableUI(true);
  end;
end;

procedure TGUITestRunner.RunActionExecute(Sender: TObject);
begin
  if Suite = nil then
    EXIT;

  Setup;
  RunTheTest(Suite);
end;

procedure TGUITestRunner.ExitActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
     FTestResult.stop;
  self.ModalResult := mrCancel;
  Close;
end;

procedure TGUITestRunner.BreakOnFailuresActionExecute(Sender: TObject);
begin
  with BreakOnFailuresAction do
   Checked := not Checked;
end;

procedure TGUITestRunner.FailIfNoChecksExecutedActionExecute(Sender: TObject);
begin
  with FailIfNoChecksExecutedAction do
    Checked := not Checked;
end;

procedure TGUITestRunner.FailTestCaseIfMemoryLeakedActionExecute(Sender: TObject);
begin
  with FailTestCaseIfMemoryLeakedAction do
  begin
    Checked := not Checked;
    IgnoreMemoryLeakInSetUpTearDownAction.Enabled := Checked;
    if not Checked then
      IgnoreMemoryLeakInSetUpTearDownAction.Checked := False;
  end;
end;

procedure TGUITestRunner.ShowTestCasesWithRunTimePropertiesActionExecute(
  Sender: TObject);
begin
  with ShowTestCasesWithRunTimePropertiesAction do
  begin
    Checked := not Checked;
    if Checked then
      WarnOnFailTestOverrideAction.Checked := False;
  end;
end;

procedure TGUITestRunner.WarnOnFailTestOverrideActionExecute(
  Sender: TObject);
begin
  with WarnOnFailTestOverrideAction do
  begin
    Checked := not Checked;
    if Checked then
      ShowTestCasesWithRunTimePropertiesAction.Checked := False;
  end;
end;

procedure TGUITestRunner.ShowTestedNodeActionExecute(Sender: TObject);
begin
  with ShowTestedNodeAction do
    Checked := not Checked;
end;

procedure TGUITestRunner.SetUpStateImages;
begin
    TestTree.Images             := RunImages;
    TestTree.StateImages        := StateImages;
    FailureListView.SmallImages := RunImages;
end;

procedure TGUITestRunner.LoadSuiteConfiguration;
begin
  if Suite <> nil then
    Suite.LoadConfiguration(IniFileName, UseRegistryAction.Checked, True);
end;

procedure TGUITestRunner.MakeNodeVisible(node: TTreeNode);
begin
  node.MakeVisible
end;

procedure TGUITestRunner.ProcessClickOnStateIcon;
var
  HitInfo: THitTests;
  node: TTreeNode;
  PointPos: TPoint;
begin
  GetCursorPos(PointPos);
  PointPos := TestTree.ScreenToClient(PointPos);
  with PointPos do
  begin
    HitInfo := TestTree.GetHitTestInfoAt(X, Y);
    node := TestTree.GetNodeAt(X, Y);
  end;
  if (node <> nil) and (HtOnStateIcon in HitInfo) then
  begin
    SwitchNodeState(node);
  end;
end;

procedure TGUITestRunner.UpdateNodeImage(node: TTreeNode);
var
  test :ITest;
begin
  test := NodeToTest(node);
  if not test.enabled then
  begin
    node.StateIndex := imgDISABLED;
  end
  else if (node.Parent <> nil)
  and (node.Parent.StateIndex <= imgPARENT_DISABLED) then
  begin
    node.StateIndex := imgPARENT_DISABLED;
  end
  else
  begin
    node.StateIndex := imgENABLED;
  end;
end;

procedure TGUITestRunner.CopyMessageToClipboardActionExecute(Sender: TObject);
begin
  ErrorMessageRTF.SelectAll;
  ErrorMessageRTF.CopyToClipboard;
end;

procedure TGUITestRunner.UseRegistryActionExecute(Sender: TObject);
begin
  with UseRegistryAction do
    Checked := not Checked;
end;

function TGUITestRunner.GetIniFile(const FileName: string) : tCustomIniFile;
begin
  if UseRegistryAction.Checked then
    Result := tRegistryIniFile.Create( GetDUnitRegistryKey + FileName )
  else
    Result := tIniFile.Create( FileName );
end;

procedure TGUITestRunner.LoadRegistryAction;
begin
  with TIniFile.Create(IniFileName) do
  try
    UseRegistryAction.Checked := ReadBool(cnConfigIniSection,
      'UseRegistry', UseRegistryAction.Checked);
  finally
    Free;
  end;
end;

procedure TGUITestRunner.SaveRegistryAction;
begin
  if UseRegistryAction.Checked then
    DeleteFile( IniFileName );

  with TIniFile.Create(IniFileName) do
  try
    WriteBool(cnConfigIniSection, 'UseRegistry', UseRegistryAction.Checked);
  finally
    Free;
  end;
end;

procedure TGUITestRunner.RunActionUpdate(Sender: TObject);
begin
  RunAction.Enabled := not FRunning and assigned( Suite ) and (Suite.countEnabledTestCases > 0);
end;

procedure TGUITestRunner.CopyMessageToClipboardActionUpdate(Sender: TObject);
begin
  CopyMessageToClipboardAction.Enabled := FailureListView.Selected <> nil;
end;

procedure TGUITestRunner.SelectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, EnableTest);
  SetNodeState(TestTree.Selected, true);
  UpdateStatus(True);
end;

procedure TGUITestRunner.DeselectCurrentActionExecute(Sender: TObject);
begin
  ApplyToTests(TestTree.Selected, DisableTest);
  UpdateStatus(True);
end;

procedure TGUITestRunner.StopActionExecute(Sender: TObject);
begin
  if FTestResult <> nil then
     FTestResult.stop;
end;

procedure TGUITestRunner.StopActionUpdate(Sender: TObject);
begin
  StopAction.Enabled := FRunning and (FTestResult <> nil);
end;

procedure TGUITestRunner.Status(test: ITest; const Msg: string);
begin
  if ErrorMessageRTF.Lines.Count = 0 then
    ErrorMessageRTF.Lines.Add(test.Name + ':');

  ErrorMessageRTF.Lines.Add(Msg);

  ErrorMessageRTF.Update;
end;

procedure TGUITestRunner.Warning(test: ITest; const Msg: string);
begin
  if ErrorMessageRTF.Lines.Count = 0 then
    ErrorMessageRTF.Lines.Add(test.Name + ':');

  ErrorMessageRTF.Lines.Add(Msg);

  ErrorMessageRTF.Update;
end;

procedure TGUITestRunner.ClearStatusMessage;
begin
  ErrorMessageRTF.Lines.Clear;
end;

procedure TGUITestRunner.CopyProcnameToClipboardActionExecute(
  Sender: TObject);
begin
  CopyTestNametoClipboard(TestTree.Selected);
end;

procedure TGUITestRunner.CopyTestNametoClipboard(ANode: TTreeNode);
begin
  if Assigned(ANode) then
  begin
    Clipboard.AsText := ANode.Text;
  end;
end;

procedure TGUITestRunner.CopyProcnameToClipboardActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TestTree.Selected)
                                 and isTestMethod(NodeToTest(TestTree.Selected));
end;

function TGUITestRunner.SelectedTest: ITest;
begin
  if TestTree.Selected = nil then
    Result := nil
  else
    Result := NodeToTest(TestTree.Selected);
end;

procedure TGUITestRunner.ListSelectedTests;
var
  aTest: ITest;
  aNode: TTreeNode;
begin
  FSelectedTests.Free;
  FSelectedTests := nil;
  FSelectedTests := TInterfaceList.Create;

  aNode := TestTree.Selected;

  while Assigned(aNode) do
  begin
    aTest := NodeToTest(aNode);
    FSelectedTests.Add(aTest as ITest);
    aNode := aNode.Parent;
  end;
end;

procedure TGUITestRunner.RunSelectedTestActionExecute(Sender: TObject);
begin
  Setup;
  ListSelectedTests;
  ProgressBar.Max := 1;
  ScoreBar.Max    := 1;
  RunTheTest(Suite);
  {$IFDEF VER130}
    FreeAndNil(FSelectedTests);
  {$ELSE}
    FSelectedTests.Free;
    FSelectedTests := nil;
  {$ENDIF}
end;

procedure TGUITestRunner.RunSelectedTestActionUpdate(Sender: TObject);
var
  aTest :ITest;
begin
  ATest := SelectedTest;
  RunSelectedTestAction.Enabled := (aTest <> nil) and (aTest.CountTestCases = 1);
end;

class procedure TGUITestRunner.RunTest(test: ITest);
var
  myform: TGUITestRunner;
begin
  Application.CreateForm(TGUITestRunner, MyForm);
  with MyForm do
  begin
    try
      suite := test;
      ShowModal;
    finally
      MyForm.Free;
    end;
  end;
end;

class procedure TGUITestRunner.RunRegisteredTests;
begin
  RunTest(RegisteredTests);
end;

procedure TGUITestRunner.EndSuite(suite: ITest);
begin
  UpdateStatus(True);
end;

procedure TGUITestRunner.StartSuite(suite: ITest);
begin
end;

procedure TGUITestRunner.TestTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewNode: TTreeNode;
begin
  { a version of this code was in the pmTestTreePopup event, but it created
    an intermittent bug. OnPopup is executed if any of the ShortCut keys
    belonging to items on the popup menu are used. This caused weird behavior,
    with the selected node suddenly changing to whatever was under the mouse
    cursor (or AV-ing if the mouse cursor wasn't over the DUnit form) when
    the user executed one of the keyboard shortcuts.

    It was intermittent most likely because the ShortCuts belonged to
    Main Menu items as well (shared from the Action.ShortCut), and the bug
    dependended on the Popup menu items receiving the ShortCut Windows message
    first.

    This code ensures that node selection occurs prior to the popup menu
    appearing when the user right-clicks on a non-selected tree node. }

  if (Button = mbRight) and (htOnItem in TestTree.GetHitTestInfoAt(X, Y)) then
  begin
    NewNode := TestTree.GetNodeAt(X, Y);
    if TestTree.Selected <> NewNode then
      TestTree.Selected := NewNode;
  end;
end;

procedure TGUITestRunner.GoToNextSelectedTestActionExecute(
  Sender: TObject);
var
  aNode: TTreeNode;
begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected.GetNext;
    while aNode <> nil do
    begin
      if SelectNodeIfTestEnabled(aNode) then
        break
      else
        aNode := aNode.GetNext;
    end;
  end;
end;

function TGUITestRunner.SelectNodeIfTestEnabled(ANode: TTreeNode): boolean;
var
  ATest: ITest;
begin
  ATest := NodeToTest(ANode);
  if (ATest.Enabled) and (IsTestMethod(ATest)) then
  begin
    Result := true;
    SelectNode(ANode);
  end
  else
    Result := false;
end;

procedure TGUITestRunner.GoToPrevSelectedTestActionExecute(
  Sender: TObject);
var
  aNode: TTreeNode;
begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected.GetPrev;
    while aNode <> nil do
    begin
      if SelectNodeIfTestEnabled(aNode) then
        break
      else
        aNode := aNode.GetPrev;
    end;
  end;
end;

procedure TGUITestRunner.SelectNode(node: TTreeNode);
begin
  node.Selected := true;
  MakeNodeVisible(node);
end;

procedure TGUITestRunner.SetupCustomShortcuts;
begin
  { the following shortcuts are not offered as an option in the
    form designer, but can be set up here }
  GoToNextSelectedTestAction.ShortCut := ShortCut(VK_RIGHT, [ssCtrl]);
  GoToPrevSelectedTestAction.ShortCut := ShortCut(VK_LEFT, [ssCtrl]);
end;

procedure TGUITestRunner.SetupGUINodes;
var
  node: TTreeNode;
  test: ITest;
begin
  { Set up the GUI nodes in the test nodes. We do it here because the form,
    the tree and all its tree nodes get recreated in TCustomForm.ShowModal
    in D8+ so we cannot do it sooner.
    This method is also called after loading test libraries }

  node := TestTree.Items.GetFirstNode;
  while assigned(node) do
  begin
    // Get and check the test for the tree node

    test := NodeToTest(node);
    assert(Assigned(test));

    // Save the tree node in the test and get the next tree node

    test.GUIObject := node;

    node := node.GetNext;
  end;
end;

const
  NoChecksStrT = ' FailsOnNoChecksExecuted  := True ';
  NoChecksStrF = ' FailsOnNoChecksExecuted  := False';
  MemLeakStrT  = ' FailsOnMemoryLeak        := True ';
  MemLeakStrF  = ' FailsOnMemoryLeak        := False';
  MemGainStrT  = ' FailsOnMemoryRecovery    := True ';
  MemGainStrF  = ' FailsOnMemoryRecovery    := False';
  MemBytesStr0 = ' AllowedMemoryLeakSize '           ;
  IgnoreStrT   = ' IgnoreSetUpTearDownLeaks := True ';
  IgnoreStrF   = ' IgnoreSetUpTearDownLeaks := False';

procedure TGUITestRunner.TestCasePropertiesActionExecute(Sender: TObject);
var
  aNode: TTreeNode;
  ATest: ITest;

begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected;
    if (aNode <> nil) then
    begin
      ATest := NodeToTest(ANode);
      if IsTestMethod(ATest) then
      begin
        if ATest.FailsOnNoChecksExecuted then
          FNoChecksStr := NoChecksStrT
        else
          FNoChecksStr := NoChecksStrF;
        fNoCheckExecutedPtyOverridden := FailIfNoChecksExecutedAction.Checked and
          (not ATest.FailsOnNoChecksExecuted);

        if ATest.FailsOnMemoryLeak then
          FMemLeakStr := MemLeakStrT
        else
          FMemLeakStr := MemLeakStrF;
        fMemLeakDetectedPtyOverridden := FailTestCaseIfMemoryLeakedAction.Checked and
          (not ATest.FailsOnMemoryLeak);
        if (ATest.FailsOnMemoryLeak and ATest.FailsOnMemoryRecovery) then
          FMemGainStr := MemGainStrT
        else
          FMemGainStr := MemGainStrF;

        if (ATest.IgnoreSetUpTearDownLeaks) and ATest.FailsOnMemoryLeak then
          FIgnoreLeakStr := IgnoreStrT
        else
          FIgnoreLeakStr := IgnoreStrF;
        FIgnoreSetUpTearDownLeakPtyOverridden := ATest.IgnoreSetUpTearDownLeaks and
          ATest.FailsOnMemoryLeak and (not IgnoreMemoryLeakInSetUpTearDownAction.Checked);

        FBytes := ':= ' + IntToStr(Atest.AllowedMemoryLeakSize) + ' Bytes';
        FMemBytesStr := MemBytesStr0 + FBytes;
        TestCaseProperty.Popup(Self.Left + FPopupX,Self.Top + FPopupY);
      end;
    end;
    ATest := nil;
  end;
end;

procedure TGUITestRunner.Previous1Click(Sender: TObject);
begin
  GoToPrevSelectedTestActionExecute(Self);
  TestCasePropertiesActionExecute(self);
end;

procedure TGUITestRunner.Next1Click(Sender: TObject);
begin
  GoToNextSelectedTestActionExecute(Self);
  TestCasePropertiesActionExecute(self);
end;

procedure TGUITestRunner.TestCasePropertiesMeasureItem(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
var
  ImageSize: TSize;
begin
  if GetTextExtentPoint32(ACanvas.Handle,
                          PChar(sPopupTitle),
                          Length(sPopupTitle),
                          ImageSize) then
  begin
    Width  := ImageSize.cx + 60;
    Height := ImageSize.cy + 4;
  end;
end;

procedure TGUITestRunner.MenuLooksInactive(ACanvas: TCanvas;
                                           ARect: TRect;
                                           Selected: Boolean;
                                           ATitle: string;
                                           TitlePosn: UINT;
                                           PtyOveridesGUI: boolean);
var
  Count: integer;
  SecondPart: string;
  SecondRect: TRect;
begin
  if TitlePosn = DT_CENTER then
    ACanvas.Font.Style := [fsBold];
  if Selected then
    ACanvas.Font.Color := clBlack;
  if PtyOveridesGUI then
    ACanvas.Brush.Color := clYellow
  else
    ACanvas.Brush.Color := TColor($C0FCC0);  //Sort of Moneygreen
  ACanvas.FillRect(ARect);
  Count := Pos(':=', ATitle);
  if Count = 0 then
    DrawText(ACanvas.Handle,
             PChar(ATitle),
             Length(ATitle),
             ARect,
             DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX or TitlePosn)
  else
  begin
    DrawText(ACanvas.Handle,
             PChar(ATitle),
             Count-1,
             ARect,
             DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX or TitlePosn);

    SecondPart := Copy(ATitle, Count, Length(ATitle));
    SecondRect := ARect;
    SecondRect.Left := 5 * ((ARect.Right - ARect.Left) div 8);
    DrawText(ACanvas.Handle,
             PChar(SecondPart),
             Length(SecondPart),
             SecondRect,
             DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX or TitlePosn)
  end;
end;

procedure TGUITestRunner.MenuLooksActive(ACanvas: TCanvas;
                                         ARect: TRect;
                                         Selected: Boolean;
                                         ATitle: string;
                                         TitlePosn: UINT);
begin
  ACanvas.FillRect(ARect);
  DrawText(ACanvas.Handle,
           PChar(ATitle),
           Length(ATitle),
           ARect,
           DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX or TitlePosn);
end;

procedure TGUITestRunner.TestCasePropertiesDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, sPopupTitle, DT_CENTER, False);
end;

procedure TGUITestRunner.Previous1DrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksActive(ACanvas, ARect, Selected, sPopupPrevious, DT_LEFT);
end;

procedure TGUITestRunner.RunSelectedTest1DrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksActive(ACanvas, ARect, Selected, sPopupRun, DT_LEFT);
end;

procedure TGUITestRunner.Next1DrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
begin
  MenuLooksActive(ACanvas, ARect, Selected, sPopupNext, DT_LEFT);
end;

procedure TGUITestRunner.FailNoCheckExecutedMenuItemDrawItem(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, FNoChecksStr,
    DT_LEFT, fNoCheckExecutedPtyOverridden);
end;

procedure TGUITestRunner.FailsOnMemoryLeakMenuItemDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, FMemLeakStr,
    DT_LEFT, fMemLeakDetectedPtyOverridden);
end;

procedure TGUITestRunner.FailsOnMemoryRecoveryMenuItemDrawItem(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, FMemGainStr,
    DT_LEFT, False);
end;

procedure TGUITestRunner.AllowedLeakSizeMemuItemDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, FMemBytesStr, DT_LEFT, False);
end;

procedure TGUITestRunner.TestCaseIgnoreSetUpTearDownLeaksMenuItemDrawItem(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
begin
  MenuLooksInactive(ACanvas, ARect, Selected, FIgnoreLeakStr,
    DT_LEFT, FIgnoreSetUpTearDownLeakPtyOverridden);
end;

procedure TGUITestRunner.pmTestTreePopup(Sender: TObject);
var
  aNode: TTreeNode;
  ATest: ITest;

begin
  if TestTree.Selected <> nil then
  begin
    aNode := TestTree.Selected;
    if (aNode <> nil) then
    begin
      ATest := NodeToTest(ANode);
      TestCasePopup.Enabled := IsTestMethod(ATest);
    end;
    ATest := nil;
  end;
end;

function TGUITestRunner.GetPropertyName(const Caption: string): string;
var
  TempStr: string;
  PosSpace: integer;
begin
  TempStr := Trim(Caption);
  PosSpace := Pos(' ',TempStr);
  if (PosSpace > 1)  then
    result := Copy(TempStr, 1, PosSpace-1);
end;

procedure TGUITestRunner.FailNoCheckExecutedMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := GetPropertyName(NoChecksStrT);
end;

procedure TGUITestRunner.FailsOnMemoryLeakMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := GetPropertyName(MemLeakStrT);
end;

procedure TGUITestRunner.AllowedLeakSizeMemuItemClick(Sender: TObject);
begin
  Clipboard.AsText := GetPropertyName(MemBytesStr0);
end;

procedure TGUITestRunner.FailsOnMemoryRecoveryMenuItemClick(
  Sender: TObject);
begin
  Clipboard.AsText := GetPropertyName(MemGainStrT);
end;

procedure TGUITestRunner.TestCaseIgnoreSetUpTearDownLeaksMenuItemClick(
  Sender: TObject);
begin
  Clipboard.AsText := GetPropertyName(IgnoreStrT);
end;

procedure TGUITestRunner.RunSelectedTestAltActionExecute(Sender: TObject);
begin
  RunSelectedTestActionExecute(Self);
  TestCasePropertiesActionExecute(Self);
end;

procedure TGUITestRunner.IgnoreMemoryLeakInSetUpTearDownActionExecute(
  Sender: TObject);
begin
  with IgnoreMemoryLeakInSetUpTearDownAction do
    Checked := not Checked;
end;

procedure TGUITestRunner.ReportMemoryLeakTypeOnShutdownActionExecute(
  Sender: TObject);
begin
  with ReportMemoryLeakTypeOnShutdownAction do
  begin
    Checked := not Checked;
{$IFDEF FASTMM}
  {$IFDEF ManualLeakReportingControl}
    ReportMemoryLeaksOnShutdown := Checked;
  {$ENDIF}
{$ENDIF}
  end;    // with
end;

end.
