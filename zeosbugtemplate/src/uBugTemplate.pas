unit uBugTemplate;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  ZAbstractRODataset, ZAbstractDataset, ZDataset,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ZConnection, ZSqlMonitor, DB;

type

  { TfrmBugTemplate }

  TfrmBugTemplate = class(TForm)
    memProgress: TMemo;
    edtBugTitle: TEdit;
    btnRunMe: TButton;
    btnClose: TButton;
    procedure btnRunMeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure ZSQLMonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
  private
    { Private declarations }
  public
    { Public declarations }
    sUsername : String;
    sPassword : String;
    procedure Log (sString: String);
  end;

var
  frmBugTemplate: TfrmBugTemplate;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

//Path to where you have Zeos downloaded, please set in project options -> directories / conditionals -> Search Path
{ Here below for easy replacement
  c:\Temp\zeos-testing\src\component;c:\Temp\zeos-testing\src\plain;c:\Temp\zeos-testing\src\core;c:\Temp\zeos-testing\src\dbc;c:\Temp\zeos-testing\src\parsesql
}

procedure TfrmBugTemplate.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmBugTemplate.ZSQLMonitorLogTrace(Sender: TObject;     Event: TZLoggingEvent);
begin
  Log (Event.Message);
end;

procedure TfrmBugTemplate.btnRunMeClick(Sender: TObject);
var
  Connection : TZConnection;
  Monitor : TZSQLMonitor;
  Query : TZQuery;

begin
  //Name of Bug Template
  Caption := 'Example Bug';

  //Create the Database Object
  Connection := TZConnection.Create(Self);
  Monitor := TZSQLMonitor.Create(Self);
  Monitor.OnLogTrace := ZSQLMonitorLogTrace;
  Monitor.Active := True;
  
  try
    //Try do some stuff - catch errors
    try
      //Connect to the Database/ please create an empty one in database folder if possible
      Connection.Protocol := 'firebird-2.5'; //A valid protocol
      Connection.HostName := '127.0.0.1';
      Connection.Database := ExtractFileDir(paramstr(0))+'/database/TESTDB.FDB';

      //Get username & password
      if (sUsername = '') then sUsername := InputBox ('Database User', 'Username', '');
      if (sPassword = '') then sPassword := InputBox ('Database Password', 'Password', '');

      Connection.User := sUsername;
      Connection.Password := sPassword;
      //Connect to the database
      Log ('Connecting to the database '+Connection.Database);
      Connection.Connect;
      //HERE IS WHERE YOU DO YOUR THINGS - USE Log to make logs

      Query := TZQuery.Create(Self);
      Query.Connection := Connection;

      Query.SQL.Text := 'create table tblnumerictest (id integer default 0 not null, nnumber numeric(18,10) default 0, primary key (id))';

      Query.SQL.Text := 'delete from tblnumerictest';
      Query.ExecSQL;


      Log ('Adding');
      //Works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (0, 8.9399999999)';
      Query.ExecSQL;

      Log ('8.9399999999');

      //Works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (1, :param)';
      Query.ParamByName ('param').AsCurrency := 8.9399999999;
      Query.ExecSQL;

      Log ('8.9399999999');

      //Works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (2, :param)';
      Query.ParamByName ('param').AsFloat := 8.9399999999;
      Query.ExecSQL;

      Log ('8.9399999999');

       //Works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (3, :param)';
      Query.ParamByName ('param').AsString := '8.9399999999';
      Query.ExecSQL;

      Log ('8.9399999999');

      //works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (4, 8.7982991199)';
      Query.ExecSQL;

      Log ('8.7982991199');

      //Does not work ??
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (5, :param)';
      Query.ParamByName ('param').AsFloat := 8.7982991199;
      Query.ExecSQL;

      Log ('8.7982991199');

      //Does not work ??
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (6, :param)';
      Query.ParamByName ('param').AsCurrency := 8.7982991199;
      Query.ExecSQL;

      Log ('8.7982991199');

      //Works
      Query.Close;
      Query.SQL.Text := 'insert into tblnumerictest values (7, :param)';
      Query.ParamByName ('param').AsString := '8.7982991199';
      Query.ExecSQL;

      Log ('8.7982991199');


      //And now for the results
      Query.Close;
      Query.SQL.Text := 'select * from tblnumerictest order by id';
      Query.Open;

      while not Query.Eof do
      begin
        Log (Query.FieldByName ('NNumber').AsString);
        Query.Next;
      end;

      Log ('Whoops, look at the last bit of data, Is this consistent');

      Query.Free;
      //END OF WHERE YOU DO YOUR THINGS
      Log('Done!');
    except
      on e:exception do
      begin
        Log (e.Message);
      end;
    end;
  finally
    //Free up all the components here
    Monitor.Free;
    Connection.Free;
  end;
end;

//Adds error logs to to the memo box
procedure TfrmBugTemplate.Log(sString: String);
begin
  memProgress.Lines.Add(DateTimeToStr(Now)+': '+sString);
end;

end.
