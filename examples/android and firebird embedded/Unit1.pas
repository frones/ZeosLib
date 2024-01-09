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
    GenericS: TZSequence;
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
  private
    { Private-Deklarationen }
    procedure prepareFirebird(FirebirdBase: String);
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
  FirebirdBase = PathDelim + 'firebird';
  ClientLib =    PathDelim + 'firebird' + PathDelim + 'lib' + PathDelim + 'libfbclient.so.4.0.4';
  LogFile =      PathDelim + 'firebird' + PathDelim + 'firebird.log';

procedure CreateSymlinkIfNecessary(Const SymLink, Target: String);
begin
  if not FileExists(SymLink) then
    if not TFile.CreateSymLink(SymLink, Target) then
      raise EZSQLException.Create('Could not create Symlink to ' + Target + ' from ' + SymLink);
end;

procedure TForm1.prepareFirebird(FirebirdBase: String);
begin
  CreateSymlinkIfNecessary(FirebirdBase + PathDelim + 'lib' + PathDelim + 'libfbclient.so.2', FirebirdBase + PathDelim + 'lib' + PathDelim + 'libfbclient.so.4.0.4');
  CreateSymlinkIfNecessary(FirebirdBase + PathDelim + 'lib' + PathDelim + 'libfbclient.so',   FirebirdBase + PathDelim + 'lib' + PathDelim + 'libfbclient.so.2');
  CreateSymlinkIfNecessary(FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicudata.so',    FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicudata.so.63');
  CreateSymlinkIfNecessary(FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicui18n.so',    FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicui18n.so.63');
  CreateSymlinkIfNecessary(FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicuuc.so',      FirebirdBase + PathDelim + 'lib' + PathDelim + 'libicuuc.so.63');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  LocalConn.Disconnect;
  DeleteFile(TPath.GetHomePath + PathDelim + 'contacts.fdb');
end;

procedure TForm1.createDatabase;
begin
  LocalConn.Properties.Add(
    'CreateNewDatabase=CREATE DATABASE ' + QuotedStr(LocalConn.Database) +
    ' USER ' + QuotedStr ('sysdba') +
    ' PASSWORD ' + QuotedStr ('masterkey') +
    ' DEFAULT CHARACTER SET UTF8');
  LocalConn.Connect;
  StructureProc.Execute;
  DataProc.Execute;
end;

procedure TForm1.openDatabase;
var
  HomePath: String;
  TmpPath: String;
begin
  HomePath := TPath.GetHomePath;
  PrepareFirebird(HomePath + FirebirdBase);
  TmpPath := HomePath + PathDelim + 'tmp';
  if not DirectoryExists(TmpPath) then
    if not ForceDirectories(TmpPath) then
      raise EZSQLException.Create('Could not create tmp dir.');
  FDSetEnv('FIREBIRD_LOCK', TmpPath);

  LocalConn.LibraryLocation := HomePath + ClientLib;
  LocalConn.User := 'sysdba';
  LocalConn.HostName := '';
  LocalConn.Database := HomePath + PathDelim + 'contacts.fdb';
  LocalConn.Protocol := 'interbase';
  LocalConn.ClientCodepage := 'UTF8';
  if FileExists(LocalConn.Database) then
    LocalConn.Connect
  else
    CreateDatabase;
end;

procedure TForm1.LoadDataBtnClick(Sender: TObject);
begin
  OpenDatabase;
  ZQuery1.Open;
end;

end.
