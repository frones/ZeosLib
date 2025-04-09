unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, ZAbstractConnection, ZConnection,
  Data.DB, ZAbstractRODataset, ZAbstractDataset, ZDataset, ZSqlProcessor,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, Data.Bind.Controls, FMX.Layouts,
  Fmx.Bind.Navigator, FMX.Edit, ZSequence, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.DBScope;

type
  TForm1 = class(TForm)
    LoadDataBtn: TButton;
    LocalConn: TZConnection;
    ZQuery1: TZQuery;
    StructureProc: TZSQLProcessor;
    ButtonsP: TPanel;
    Button2: TButton;
    DataProc: TZSQLProcessor;
    BindNavigator1: TBindNavigator;
    NameEdt: TEdit;
    SurnameEdt: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PhoneEdt: TEdit;
    Label3: TLabel;
    EmailEdt: TEdit;
    Label4: TLabel;
    StreetEdt: TEdit;
    Label5: TLabel;
    CityEdt: TEdit;
    Label6: TLabel;
    ZipCodeEdt: TEdit;
    Label7: TLabel;
    BindSourceDB12: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    LinkControlToField7: TLinkControlToField;
    procedure LoadDataBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ZQuery1BeforePost(DataSet: TDataSet);
  private
    { Private-Deklarationen }
    procedure createDatabase;
    procedure openDatabase;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses IOUtils, FireDAC.Stan.Util, Posix.Unistd, ZExceptions;

const
  ClientLib =    PathDelim + 'sqlite' + PathDelim + 'libsqliteX.so';

procedure TForm1.Button2Click(Sender: TObject);
begin
  LocalConn.Disconnect;
  DeleteFile(TPath.GetHomePath + PathDelim + 'contacts.db');
end;

procedure TForm1.createDatabase;
begin
  StructureProc.Execute;
  DataProc.Execute;
end;

procedure TForm1.openDatabase;
var
  HomePath: String;
  //TmpPath: String;
begin
  HomePath := TPath.GetHomePath;

  LocalConn.LibraryLocation := HomePath + ClientLib;
  LocalConn.Database := HomePath + PathDelim + 'contacts.db';
  LocalConn.Protocol := 'sqlite';
  LocalConn.ClientCodepage := 'UTF-8';
  if FileExists(LocalConn.Database) then
    LocalConn.Connect
  else
    CreateDatabase;
end;

procedure TForm1.ZQuery1BeforePost(DataSet: TDataSet);
begin
  if DataSet.FieldByName('id').IsNull then
    DataSet.FieldByName('id').AsInteger := Random(High(Integer));
end;

procedure TForm1.LoadDataBtnClick(Sender: TObject);
begin
  OpenDatabase;
  ZQuery1.Open;
end;

end.
