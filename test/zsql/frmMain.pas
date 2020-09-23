unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ExtCtrls, StdCtrls, ToolWin, Grids, DBGrids, DB,
  ZAbstractRODataset, ZAbstractDataset, ZDataset, ZAbstractConnection,
  ZConnection;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    tcGroup: TTabControl;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ToolBar1: TToolBar;
    tbConnect: TToolButton;
    mmSQL: TMemo;
    btnOpen: TButton;
    btnExecSQL: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnExecSQLClick(Sender: TObject);
    procedure tcGroupChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tbConnectClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TSQLTestConfig = record  { Names of the connection configuration keys. }
    DATABASE_LIBLOCATION    : String;
    DATABASE_ALIAS          : String;
    DATABASE_PROTOCOL       : String;
    DATABASE_HOST           : String;
    DATABASE_PORT           : Integer;
    DATABASE_NAME           : String;
    DATABASE_USER           : String;
    DATABASE_PASSWORD       : String;
    DATABASE_PROPERTIES     : String;
  end;
  TSQLTestConfigDynArray = array of TSQLTestConfig;

var
  MainForm: TMainForm;
  Configs: TSQLTestConfigDynArray;

implementation

uses ZTestConfig, ZSysUtils;
{$R *.dfm}

procedure TMainForm.btnExecSQLClick(Sender: TObject);
begin
  ZQuery1.ExecSQL;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  ZQuery1.Active := False;
  ZQuery1.SQL.Assign(mmSQL.Lines);
  ZQuery1.Open;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var S: String;
  SL: TStrings;
  I: Integer;
  X: Boolean;
begin
  S := ZTestConfig.TestConfig.ReadProperty(COMMON_GROUP, ACTIVE_CONNECTIONS_KEY, '');
  SL := SplitString(S, ',;');
  SetLength(Configs, SL.Count);
  for i := 0 to SL.Count -1 do begin
    S := SL[i];
    tcGroup.Tabs.Add(S);
    with Configs[i] do begin
      DATABASE_LIBLOCATION := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_LIBLOCATION_KEY, '');
      DATABASE_ALIAS := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_ALIAS_KEY, '');
      DATABASE_PROTOCOL := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_PROTOCOL_KEY, '');
      DATABASE_HOST := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_HOST_KEY, '');
      DATABASE_PORT := StrToInt(ZTestConfig.TestConfig.ReadProperty(S, DATABASE_PORT_KEY, '0'));
      DATABASE_NAME := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_NAME_KEY, '');
      DATABASE_USER := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_USER_KEY, '');
      DATABASE_PASSWORD := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_PASSWORD_KEY, '');
      DATABASE_PROPERTIES := ZTestConfig.TestConfig.ReadProperty(S, DATABASE_PROPERTIES_KEY, '');
    end;
  end;
  tcGroupChanging(Sender, X);
end;

procedure TMainForm.tbConnectClick(Sender: TObject);
begin
  ZConnection1.Connected := not ZConnection1.Connected;
end;

procedure TMainForm.tcGroupChanging(Sender: TObject; var AllowChange: Boolean);
var Props: TStrings;
begin
  AllowChange := True;
  if ZConnection1.Connected then
    ZConnection1.Disconnect;
  with Configs[tcGroup.TabIndex] do begin
    ZConnection1.LibraryLocation := DATABASE_LIBLOCATION;
    ZConnection1.Protocol := DATABASE_PROTOCOL;
    ZConnection1.HostName := DATABASE_HOST;
    ZConnection1.Port := DATABASE_PORT;
    ZConnection1.Database := DATABASE_NAME;
    ZConnection1.User := DATABASE_USER;
    ZConnection1.Password := DATABASE_PASSWORD;
    Props := SplitString(DATABASE_PROPERTIES, ';');
    ZConnection1.Properties.Assign(Props);
    FreeAndNil(Props);
  end;
end;

end.
