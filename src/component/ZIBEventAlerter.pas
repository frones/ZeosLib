{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Merkuriev                  }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZIBEventAlerter;

{$I ZComponent.inc}

interface

uses
  SysUtils, Classes,
{$IF defined(MSWINDOWS)and not defined(FPC)}
  Windows,
{$IFEND}
  ZDbcInterbase6, ZConnection, ZDbcIntfs, ZFastCode,
  ZPlainFirebirdDriver, ZPlainFirebirdInterbaseConstants;

type

  TEventAlert = procedure(Sender: TObject; EventName: string; EventCount: longint;
    var CancelAlerts: boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: integer) of object;

  TZIBEventAlerter = class(TComponent)
  private
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FThreads: TList;
    FNativeHandle: PISC_DB_HANDLE;
    ThreadException: boolean;
    FConnection: TZConnection;
    FOnError: TErrorEvent;
    FAutoRegister: boolean;
    FRegistered: boolean;

    procedure SetConnection(Value: TZConnection);
    procedure SetEvents(Value: TStrings);
    function GetRegistered: boolean;
    procedure SetRegistered(const Value: boolean);
    function GetPlainDriver: IZInterbasePlainDriver;
  protected
    { Protected declarations }
    function GetNativeHandle: PISC_DB_HANDLE; virtual;
    procedure EventChange(Sender: TObject); virtual;
    procedure ThreadEnded(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    property NativeHandle: PISC_DB_HANDLE read GetNativeHandle;
    property PlainDriver: IZInterbasePlainDriver read GetPlainDriver;
    procedure SetAutoRegister(const Value: boolean);
    function GetAutoRegister: boolean;
  published
    { Published declarations }
    property AutoRegister: boolean read GetAutoRegister write SetAutoRegister;
    property Connection: TZConnection read FConnection write SetConnection;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: boolean read GetRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  SyncObjs;

const
  IB_MAX_EVENT_BLOCK = 15;   // maximum events handled per block by InterBase
  IB_MAX_EVENT_LENGTH = 64;  // maximum event name length
threadvar
  FStatusVector: TARRAY_ISC_STATUS;

type

  { TIBEventThread }
  TIBEventThread = class(TThread)
  private
    // IB API call parameters
    WhichEvent: integer;
    EventID: ISC_LONG;
    EventBuffer: PAnsiChar;
    EventBufferLen: Short;
    ResultBuffer: PAnsiChar;
    // Local use variables
    Signal: TSimpleEvent;
    EventsReceived,
    FirstTime: boolean;
    EventGroup,
    EventCount: integer;
    Parent: TZIBEventAlerter;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts: boolean;
  protected
    procedure Execute; override;
    procedure SignalEvent; virtual;
    procedure SignalTerminate; virtual;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    procedure QueueEvents; virtual;
    procedure SQueEvents;
    procedure ProcessEvents; virtual;
    procedure DoEvent;
    procedure DoHandleException;
    function HandleException: boolean; virtual;
    procedure UpdateResultBuffer(Length: UShort; Updated: PAnsiChar);
  public
    constructor Create(Owner: TZIBEventAlerter; EventGrp: integer;
      TermEvent: TNotifyEvent); virtual;
    destructor Destroy; override;
  end;

  Tsib_event_block = function(EventBuffer, ResultBuffer: PPAnsiChar; IDCount: UShort;
    Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,
    Event10, Event11, Event12, Event13, Event14, Event15: PAnsiChar): ISC_LONG;
  cdecl;

function TZIBEventAlerter.GetNativeHandle: PISC_DB_HANDLE;
begin
  Result := (FConnection.DbcConnection as IZInterbase6Connection).GetDBHandle;
end;

function StatusVector: PISC_STATUS;
begin
  Result := @FStatusVector;
end;

function StatusVectorArray: TARRAY_ISC_STATUS;
begin
  Result := FStatusVector;
end;

{ TZIBEventAlerter }

constructor TZIBEventAlerter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ThreadException := False;
  FOnEventAlert := nil;
  FNativeHandle := nil;
  FConnection := nil;
  FAutoRegister := False;
  FEvents := TStringList.Create;
  with TStringList(FEvents) do
  begin
    Sorted := True;  // dupIgnore only works when the TStringList is sorted
    OnChange := EventChange; // assign the routine which validates the event lenghts
    Duplicates := dupIgnore; // don't allow duplicate events
  end;
  FThreads := TList.Create;
end;

destructor TZIBEventAlerter.Destroy;
begin
  try
    if Registered then
      UnRegisterEvents;
  except
    // silence any exceptions which might be raised
    // by UnRegisterEvents during destruction
  end;

{  If Assigned(FConnection) then
    FConnection.RemoveEventNotifier(Self);
}

  FThreads.Free;
  FEvents.Free;

  inherited Destroy;
end;

procedure TZIBEventAlerter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    if Registered then
      UnRegisterEvents;
    FConnection := nil;
  end;
end;


// -> ms, 18/08/2004:
//    Modified so that now the DB connection will be made when events are registered
//    this is because the method UnregisterEvents of TIBEventThread needs a native
//    DB handle that can only be retrieved when DB connection is active. If the events
//    are registered correctly the DB connection must be established. If it is not
//    established this will be done here. This means that whenever events are registered
//    (by setting AutoRegister := True or calling RegisterEvents explicitly) and the
//    DB connection ist not established, this will be done here automatically (including
//    the retrieval of the native DB handle).
Procedure TZIBEventAlerter.RegisterEvents;
Var i: Integer;
Begin
   If (not (csDesigning in ComponentState)) and (Assigned(FConnection)) Then Begin
      Try
         If (FThreads.Count = 0) Then Begin
            If (FEvents.Count > 0) Then Begin
               For i := 0 To ((FEvents.Count - 1) div IB_MAX_EVENT_BLOCK) Do
                 FThreads.Add(TIBEventThread.Create(Self, i, ThreadEnded));
            End;
         End;
      Finally
         FRegistered := FThreads.Count <> 0;
         If FRegistered Then Begin
            If not FConnection.Connected Then
               FConnection.Connect;
            FNativeHandle := GetNativeHandle;
         End;
      End;
   End;
End; // RegisterEvents


// -> ms, 18/08/2004:
//    Modified so that the native DB handle will now be retrieved by
//    method RegisterEvents. Retrieving it here caused an Exception
//    even if DB was connected.
Procedure TZIBEventAlerter.SetConnection(Value: TZConnection);
Var
  WasRegistered: boolean;
Begin
   If (Value <> FConnection) Then Begin
      If (csDesigning in ComponentState) Then
         FConnection := Value
      Else Begin
         WasRegistered := Registered;
         If WasRegistered Then
            UnRegisterEvents;
         FConnection := Value;
         If WasRegistered Then
            RegisterEvents;
      End;
   End;
End; // SetConnection


procedure TZIBEventAlerter.SetEvents(Value: TStrings);
begin
  FEvents.Assign(Value);
end;

procedure TZIBEventAlerter.SetRegistered(const Value: boolean);
begin
  FRegistered := Value;
  if csDesigning in ComponentState then
    exit;
  if Value then
    RegisterEvents
  else
    UnRegisterEvents;
end;

procedure TZIBEventAlerter.UnregisterEvents;
var
  i: integer;
  Temp: TIBEventThread;
begin
  if csDesigning in ComponentState then
    exit;
  if (FThreads.Count > 0) then
  begin
    for i := (FThreads.Count - 1) downto 0 do
    begin
      Temp := TIBEventThread(FThreads[i]);
      FThreads.Delete(i);

      Temp.SignalTerminate;
      Temp.WaitFor;
      Temp.Free;
    end;
  end;
  FRegistered := FThreads.Count <> 0;
end;

function TZIBEventAlerter.GetPlainDriver: IZInterbasePlainDriver;
begin
  Result := (FConnection.DbcConnection as IZInterbase6Connection).GetPlainDriver;
end;

{ TIBEventThread }

procedure EventCallback(P: Pointer; Length: Short; Updated: PAnsiChar); cdecl;
begin
  if (Assigned(P) and Assigned(Updated)) then
  begin
    TIBEventThread(P).UpdateResultBuffer(Length, Updated);
    TIBEventThread(P).SignalEvent;
  end;
end;

procedure TIBEventThread.DoEvent;
begin
  Parent.FOnEventAlert(Parent, Parent.FEvents[((EventGroup * IB_MAX_EVENT_BLOCK) + WhichEvent)],
    StatusVectorArray[WhichEvent], FCancelAlerts)
end;

procedure TIBEventThread.UpdateResultBuffer(Length: UShort; Updated: PAnsiChar);
begin
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Updated^, ResultBuffer^, Length);
end;

procedure TIBEventThread.QueueEvents;
begin
  EventsReceived := False;
  Signal.ResetEvent;
  Synchronize(SQueEvents);
end;

procedure TIBEventThread.ProcessEvents;
var
  i: integer;
begin
  Parent.PlainDriver.isc_event_counts(StatusVector, EventBufferLen,
    EventBuffer, ResultBuffer);
  if (Assigned(Parent.FOnEventAlert) and (not FirstTime)) then
  begin
    FCancelAlerts := False;
    for i := 0 to (EventCount - 1) do
    begin
      if (StatusVectorArray[i] <> 0) then
      begin
        WhichEvent := i;
        Synchronize(DoEvent)
      end;
    end;
  end;
  FirstTime := False;
end;

procedure TIBEventThread.UnRegisterEvents;
begin
  Parent.PlainDriver.isc_cancel_events(StatusVector, Parent.FNativeHandle, @EventID);
  Parent.PlainDriver.isc_free(EventBuffer);
  EventBuffer := nil;
  Parent.PlainDriver.isc_free(ResultBuffer);
  ResultBuffer := nil;
end;

procedure TIBEventThread.RegisterEvents;
var
  sib_event_block: Tsib_event_block;
  {$IFDEF UNICODE}
  // Holder for ANSI strings converted from Unicode items of FEvents.
  // Obligatory! Otherwise pointer returned from EBP will point to
  // invalid (released) memory.
  EBPArray: array[1..IB_MAX_EVENT_BLOCK] of AnsiString;
  {$ENDIF}

  function EBP(Index: integer): PAnsiChar;
  var EvListIndex: Integer;
  begin
    // Index is 1-based, FEvents is 0-based
    EvListIndex := Index + (EventGroup * IB_MAX_EVENT_BLOCK) - 1;
    if (EvListIndex >= Parent.FEvents.Count) then
      Result := nil
    else
    {$IFDEF UNICODE}
    begin
      EBPArray[Index] := AnsiString(Parent.FEvents[EvListIndex]);
      Result := PAnsiChar(EBPArray[Index]);
    end;
    {$ELSE}
    Result := PAnsiChar(Parent.FEvents[EvListIndex]);
    {$ENDIF}
  end;

begin
  EventBuffer := nil;
  ResultBuffer := nil;
  EventBufferLen := 0;
  FirstTime := True;
  EventCount := (Parent.FEvents.Count - (EventGroup * IB_MAX_EVENT_BLOCK));
  if (EventCount > IB_MAX_EVENT_BLOCK) then
    EventCount := IB_MAX_EVENT_BLOCK;

{
  if Parent.Connection.Protocol='interbase-6' then
    sib_event_block := Tsib_event_block(ZPlainInterbase6.isc_event_block)
    else if Parent.Connection.Protocol='firebird-1.0' then
      sib_event_block := Tsib_event_block(ZPlainFirebird10.isc_event_block)
    else if Parent.Connection.Protocol='firebird-1.5' then
      sib_event_block := Tsib_event_block(ZPlainFirebird15.isc_event_block)
    else if Parent.Connection.Protocol='firebirdd-1.5' then
      sib_event_block := Tsib_event_block(ZPlainFirebird15.isc_event_block)
    else if Parent.Connection.Protocol='firebird-2.0' then
      sib_event_block := Tsib_event_block(ZPlainFirebird20.isc_event_block)
    else if Parent.Connection.Protocol='firebirdd-2.0' then
      sib_event_block := Tsib_event_block(ZPlainFirebird20.isc_event_block)
    else if Parent.Connection.Protocol='firebird-2.1' then
      sib_event_block := Tsib_event_block(ZPlainFirebird21.isc_event_block)
    else if Parent.Connection.Protocol='firebirdd-2.1' then
      sib_event_block := Tsib_event_block(ZPlainFirebird21.isc_event_block)

  else
    sib_event_block := Tsib_event_block(ZPlainInterbase6.isc_event_block);
  }
  sib_event_block := Tsib_event_block(Parent.GetPlainDriver.GetFirebirdAPI.isc_event_block);
  EventBufferLen := sib_event_block(@EventBuffer,
    @ResultBuffer, EventCount,
    EBP(1), EBP(2),  EBP(3),  EBP(4),  EBP(5),  EBP(6),  EBP(7), EBP(8),
    EBP(9), EBP(10), EBP(11), EBP(12), EBP(13), EBP(14), EBP(15));


end;

procedure TIBEventThread.SignalEvent;
begin
  EventsReceived := True;
  Signal.SetEvent;
end;

procedure TIBEventThread.SignalTerminate;
begin
  if not Terminated then
  begin
    Terminate;
    Signal.SetEvent;
  end;
end;

procedure TIBEventThread.DoHandleException;
begin
  SysUtils.ShowException(FExceptObject, FExceptAddr);
end;

function TIBEventThread.HandleException: boolean;
begin
  if not Parent.ThreadException then
  begin
    Result := True;
    Parent.ThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        Synchronize(DoHandleException);
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end
  else
    Result := False;
end;

procedure TIBEventThread.Execute;
begin
  RegisterEvents;
  QueueEvents;
  try
    repeat
      Signal.WaitFor(INFINITE);
      if EventsReceived then
      begin
        ProcessEvents;
        QueueEvents;
      end;
    until Terminated;
    ReturnValue := 0;
  except
    if HandleException then
      ReturnValue := 1
    else
      ReturnValue := 0;
  end;
end;

{$WARNINGS OFF}
constructor TIBEventThread.Create(Owner: TZIBEventAlerter;
  EventGrp: integer; TermEvent: TNotifyEvent);
begin
  inherited Create(True);
  FCancelAlerts := False;
  Signal := TSimpleEvent.Create;
  Parent := Owner;
  EventGroup := EventGrp;
  OnTerminate := TermEvent;
  Resume;
end;
{$WARNINGS ON}

destructor TIBEventThread.Destroy;
begin
  try
    UnRegisterEvents;
  except
    if HandleException then
      ReturnValue := 1
    else
      ReturnValue := 0;
  end;
  Signal.Free;
  inherited Destroy;
end;

procedure TZIBEventAlerter.EventChange(Sender: TObject);
var
  i: integer;
  WasRegistered: boolean;
begin
  WasRegistered := Registered;
  try
    if WasRegistered then
      UnRegisterEvents;
    TStringList(FEvents).OnChange := nil;
    try
      for i := (FEvents.Count - 1) downto 0 do
      begin
        if (FEvents[i] = EmptyStr) then
        begin
          FEvents.Delete(i);
        end
        else if (Length(FEvents[i]) > (IB_MAX_EVENT_LENGTH - 1)) then
        begin
          FEvents[i] := Copy(FEvents[i], 1, (IB_MAX_EVENT_LENGTH - 1));
        end;
      end;
    finally
      TStringList(FEvents).OnChange := EventChange;
    end;
  finally
    if WasRegistered then
      RegisterEvents;
  end;
end;

function TZIBEventAlerter.GetRegistered: boolean;
begin
  Result := FRegistered;
end;

procedure TZIBEventAlerter.ThreadEnded(Sender: TObject);
var
  ThreadIdx: integer;
begin
  if (Sender is TIBEventThread) then
  begin
    ThreadIdx := FThreads.IndexOf(Sender);
    if (ThreadIdx > -1) then
      FThreads.Delete(ThreadIdx);
    if (TIBEventThread(Sender).ReturnValue = 1) then
    begin
      if Registered then
        UnRegisterEvents;
      ThreadException := False;
    end
  end;
end;

procedure TZIBEventAlerter.SetAutoRegister(const Value: boolean);
begin
  if FAutoRegister <> Value then
  begin
    FAutoRegister := Value;
    if FAutoRegister and (not Registered) and
      Assigned(FConnection) and FConnection.Connected then
      RegisterEvents;
  end;
end;

function TZIBEventAlerter.GetAutoRegister: boolean;
begin
  Result := FAutoRegister;
end;

procedure TIBEventThread.SQueEvents;
var
  Status: ISC_STATUS;
begin
  Status := -999999;
  try
    Status := Parent.PlainDriver.isc_que_events(StatusVector,
      Parent.FNativeHandle, @EventID, EventBufferLen,
      EventBuffer, TISC_CALLBACK(@EventCallback), PVoid(Self));
  except
    on E: Exception do
      if Status <> -999999 then
        if Assigned(Parent.OnError) then
          if E is EZSQLException then
            Parent.OnError(Parent, EZSQLException(E).ErrorCode)
          else
            Parent.OnError(Parent, 0);
  end;
end;

end.

