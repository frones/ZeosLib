//-- Ivan Rog - 2010
//-- Tested only for 8.3.10 PostgreSQL!
//-- MUST work with all version ??? Can`t test this! Sory
//-- Ported to Lazarus by Silvio Clecio - http://silvioprog.com.br
//-- Tested on FPC-2.4.0-2/Lazarus-0.9.29(Win32/Linux)

unit ZPgEventAlerter;

interface

uses
  SysUtils, Classes, ExtCtrls,
{$IFNDEF UNIX} 
  Windows,
{$ELSE} 
  {$IFNDEF FPC} 
    libc, Math,
  {$ENDIF} 
{$ENDIF}
  ZDbcPostgreSql, ZPlainPostgreSqlDriver, ZConnection;

//****************************************************************************//
//                  TZPgEventAlerter Object                                   //
//                  Asynchronous notifying                                    //
//****************************************************************************//
type

  TZPgNotifyEvent = procedure (Sender: TObject; Event: string; ProcessID : Integer) of object;

  TZPgEventAlerter = class (TComponent)
  private
    FActive      : Boolean;
    FEvents      : TStrings;
    FTimer       : TTimer;
    FConnection  : TZConnection;   //-- соединение
    FNotifyFired : TZPgNotifyEvent;
  protected
    procedure SetActive     (Value: Boolean);
    function  GetInterval   : Cardinal;
    procedure SetInterval   (Value: Cardinal);
    procedure SetEvents     (Value: TStrings);
    procedure SetConnection (Value: TZConnection);
    procedure TimerTick     (Sender: TObject);
    procedure CheckEvents;
    procedure OpenNotify;
    procedure CloseNotify;
  public
    constructor Create     (AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Connection: TZConnection     read FConnection   write SetConnection;
    property Active:     Boolean          read FActive       write SetActive;
    property Events:     TStrings         read FEvents       write SetEvents;
    property Interval:   Cardinal         read GetInterval   write SetInterval    default 250;
    property OnNotify:   TZPgNotifyEvent  read FNotifyFired  write FNotifyFired;
  end;

implementation

constructor TZPgEventAlerter.Create(AOwner: TComponent);
var I: integer;
begin
  inherited Create(AOwner);
  FEvents := TStringList.Create;
  with TStringList(FEvents) do
  begin
    Duplicates := dupIgnore;
  end;
  FTimer         := TTimer.Create(Self);
  FTimer.Enabled := False;
  SetInterval(250);
  FTimer.OnTimer := TimerTick;
  FActive        := False;
  if (csDesigning in ComponentState) and Assigned(AOwner) then
   for I := AOwner.ComponentCount - 1 downto 0 do
    if AOwner.Components[I] is TZConnection then
     begin
      Connection := AOwner.Components[I] as TZConnection;
      Break;
     end;
end;

destructor TZPgEventAlerter.Destroy;
begin
  CloseNotify;
  FEvents.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TZPgEventAlerter.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TZPgEventAlerter.GetInterval;
begin
  Result := FTimer.Interval;
end;

procedure TZPgEventAlerter.SetEvents(Value: TStrings);
var
  I: Integer;
begin
  FEvents.Assign(Value);
  for I := 0 to FEvents.Count -1 do
    FEvents[I] := Trim(FEvents[I]);
end;

procedure TZPgEventAlerter.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then OpenNotify
    else CloseNotify;
  end;
end;

procedure TZPgEventAlerter.SetConnection(Value: TZConnection);
begin
//  if Value=nil then
//  begin
//   if FConnection <>nil then
//   Begin
//    CloseNotify;
//    FConnection := Value;
//   End;
//   exit;
//  end;
//  if (FConnection.Protocol<>'postgresql-8') or (FConnection.Protocol<>'postgresql-7') then
//  begin
//   raise EZDatabaseError.Create('Ivalid connection protocol! Need <postgres>, get'+FConnection.Protocol);
//   Exit;
//  end;
  if FConnection <> Value then
  begin
    CloseNotify;
    FConnection := Value;
  end;
end;

procedure TZPgEventAlerter.TimerTick(Sender: TObject);
begin
  if not FActive then
   FTimer.Enabled := False
  else
   CheckEvents;
end;

procedure TZPgEventAlerter.OpenNotify;
var
  I        : Integer;
  tmp      : array [0..255] of AnsiChar;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
  res      : PGresult; //-- результат выполнения команд сервера
begin
  if FActive then Exit;
  if not Assigned(FConnection) then Exit;
  if ((csLoading in ComponentState) or (csDesigning in ComponentState)) then Exit;
  if not FConnection.Connected then Exit;
  ICon     := (FConnection.DbcConnection as IZPostgreSQLConnection);
  Handle   := ICon.GetConnectionHandle;
  PlainDRV := ICon.GetPlainDriver;
  if Handle=nil then Exit;

  for I := 0 to FEvents.Count-1 do
  begin
   StrPCopy(tmp, 'listen '+FEvents.Strings[i]);
   res:=PlainDRV.ExecuteQuery(Handle,tmp);
   if (PlainDRV.GetResultStatus(res) <> TZPostgreSQLExecStatusType(PGRES_COMMAND_OK))
   then
   begin
    //-- произошла ошибка! Как обрабатывать, пока не понятно....
//    raise EZDatabaseError.Create('LISTEN command failed: '+PlainDRV.GetErrorMessage(Handle));
    PlainDRV.Clear(res);
    Exit;
   end;
   PlainDRV.Clear(res);
  end;
 FActive        := True;
 FTimer.Enabled := True; //-- запуск таймера опроса
end;

procedure TZPgEventAlerter.CloseNotify;
var
  I        : Integer;
  tmp      : array [0..255] of AnsiChar;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
  res      : PGresult; //-- результат выполнения команд сервера

begin
  if not FActive then Exit;
  FActive        := False;
  FTimer.Enabled := False;
  ICon           := (FConnection.DbcConnection as IZPostgreSQLConnection);
  Handle         := ICon.GetConnectionHandle;
  PlainDRV       := ICon.GetPlainDriver;

  if Handle=nil then Exit;
  for I := 0 to FEvents.Count-1 do
  begin
   StrPCopy(tmp, 'unlisten '+FEvents.Strings[i]);
   res:=PlainDRV.ExecuteQuery(Handle,tmp);
   if (PlainDRV.GetResultStatus(res) <> TZPostgreSQLExecStatusType(PGRES_COMMAND_OK))
   then
   begin
    //-- произошла ошибка! Как обрабатывать, пока не понятно....
//    raise EZDatabaseError.Create('UNLISTEN command failed: '+PlainDRV.GetErrorMessage(Handle));
    PlainDRV.Clear(res);
    Exit;
   end;
   PlainDRV.Clear(res);
  end;
end;

procedure TZPgEventAlerter.CheckEvents;
var
  notify   : PZPostgreSQLNotify;
  Handle   : PZPostgreSQLConnect;
  ICon     : IZPostgreSQLConnection;
  PlainDRV : IZPostgreSQLPlainDriver;
begin

 ICon      := (FConnection.DbcConnection as IZPostgreSQLConnection);
 Handle    := ICon.GetConnectionHandle;
 if Handle=nil then
 begin
  //-- произошло внезапное отсоединение от БД
  FTimer.Enabled := false;
  FActive        := false;
  Exit;
 end;
 if not FConnection.Connected then
 begin
  CloseNotify;
  Exit;
 end;
 PlainDRV  := ICon.GetPlainDriver;

 if PlainDRV.ConsumeInput(Handle)=1 then
 begin
  while true do
  begin
   notify:=PlainDRV.Notifies(Handle);
   if notify=nil then break;
   if Assigned(FNotifyFired) then FNotifyFired(Self, Notify.relname, Notify.be_pid);
   PlainDRV.FreeNotify(notify);
  end;
 end;
end;

end.

