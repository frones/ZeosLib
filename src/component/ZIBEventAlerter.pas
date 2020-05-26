{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{            Written by Sergey Merkuriev                  }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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

{ Constributors:
  MS (Michael Seeger),
  NB
  EgonHugeist
}

unit ZIBEventAlerter;

{$I ZComponent.inc}

interface

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
uses
  SysUtils, Classes,
{$IF defined(MSWINDOWS)and not defined(FPC)}
  Windows,
{$IFEND}
  {$IFNDEF ZEOS_DISABLE_INTERBASE}ZDbcInterbase6, {$ENDIF}
  {$IFNDEF ZEOS_DISABLE_FIREBIRD}ZPlainFirebird, ZDbcFirebird,{$ENDIF}
  ZDbcInterbase6Utils, ZConnection, ZDbcIntfs, ZFastCode,
  ZPlainFirebirdInterbaseDriver
  {$IFDEF TLIST_IS_DEPRECATED}, ZSysUtils, ZClasses{$ENDIF};

type

  TEventAlert = procedure(Sender: TObject; EventName: string; EventCount: longint;
    var CancelAlerts: boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: integer) of object;

  TZIBEventAlerter = class(TComponent)
  private
    FEvents: TStrings;
    FOnEventAlert: TEventAlert;
    FThreads: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    {$IFNDEF ZEOS_DISABLE_INTERBASE}
    FIBConnection: IZInterbase6Connection;
    FNativeHandle: PISC_DB_HANDLE;
    {$ENDIF}
    {$IFNDEF ZEOS_DISABLE_FIREBIRD}
    FFBConnection: IZFirebirdConnection;
    FAttachment: IAttachment;
    FStatus: IStatus;
    {$ENDIF}
    ThreadException: boolean;
    FConnection: TZConnection;
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FOnError: TErrorEvent;
    FAutoRegister: boolean;
    FRegistered: boolean;

    procedure SetConnection({$IFDEF AUTOREFCOUNT}const{$ENDIF}Value: TZConnection);
    procedure SetEvents({$IFDEF AUTOREFCOUNT}const{$ENDIF}Value: TStrings);
    function GetRegistered: boolean;
    procedure SetRegistered(const Value: boolean);
  protected
    { Protected declarations }
    procedure EventChange({%H-}Sender: TObject); virtual;
    procedure ThreadEnded(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    procedure SetAutoRegister(const Value: boolean);
    function GetAutoRegister: boolean;
    property PlainDriver: TZInterbaseFirebirdPlainDriver read FPlainDriver;
  published
    { Published declarations }
    property AutoRegister: boolean read GetAutoRegister write SetAutoRegister;
    property Connection: TZConnection read FConnection write SetConnection;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: boolean read GetRegistered write SetRegistered;
    property OnEventAlert: TEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit

uses
  SyncObjs, ZDbcLogging{.$IFDEF UNICODE}, ZCompatibility{.$ENDIF}
  {$IFDEF UNICODE}, ZEncoding{$ENDIF};

const
  IB_MAX_EVENT_BLOCK = 15;   // maximum events handled per block by InterBase
  IB_MAX_EVENT_LENGTH = 64;  // maximum event name length

type
  TAbstractInterbaseFirebirdEventThread = class(TThread)
    // IB API call parameters
    WhichEvent: integer;
    CountForEvent: longint;
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
    FParent: TZIBEventAlerter;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts: boolean;
    {$IFDEF UNICODE}
    FCodePage: Word;
    {$ENDIF}
  protected
    procedure Execute; override;
    procedure SignalEvent;
    procedure SignalTerminate;
    procedure RegisterEvents;
    procedure UnRegisterEvents; virtual;
    procedure QueueEvents;
    procedure AsyncQueEvents; virtual; abstract;
    procedure ProcessEvents; virtual;
    procedure DoEvent;
    procedure DoHandleException;
    function HandleException: boolean;
    procedure UpdateResultBuffer(Length: Integer; Updated: Pointer);
  public
    constructor Create(Owner: TZIBEventAlerter; EventGrp: integer;
      TermEvent: TNotifyEvent);
    destructor Destroy; override;
  end;

  {$IFNDEF ZEOS_DISABLE_INTERBASE}
  { TIBEventThread }
  TIBEventThread = class(TAbstractInterbaseFirebirdEventThread)
  private
    StatusVector: TARRAY_ISC_STATUS;
    // Local use variables
  protected
    procedure UnRegisterEvents; override;
    procedure AsyncQueEvents; override;
  end;
  {$ENDIF ZEOS_DISABLE_INTERBASE}

  {$IFNDEF ZEOS_DISABLE_FIREBIRD}

  TFBEventThread = class;

  TFBEventCallback = class(IEventCallbackImpl)
  private
    FOwner: TFBEventThread;
    FName: RawByteString;
    FRefCnt: integer; //for refcounting
  public
    constructor Create(aOwner: TFBEventThread; const aName: RawByteString);
    procedure addRef;  override;
    function release: Integer; override;
    procedure eventCallbackFunction(length: Cardinal; events: BytePtr); override;
    procedure WaitForEvent;
    procedure CancelEvent;
 end;


  { TFBEventThread }
  TFBEventThread = class(TAbstractInterbaseFirebirdEventThread)
  private
    // IB API call parameters
    FEvents: IEvents;
    FEventCallback: TFBEventCallback;
  protected
    procedure UnRegisterEvents; override;
    procedure AsyncQueEvents; override;
  end;
  {$ENDIF ZEOS_DISABLE_FIREBIRD}

{ TZIBEventAlerter }

constructor TZIBEventAlerter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEvents := TStringList.Create;
  with TStringList(FEvents) do begin
    Sorted := True;  // dupIgnore only works when the TStringList is sorted
    OnChange := EventChange; // assign the routine which validates the event lenghts
    Duplicates := dupIgnore; // don't allow duplicate events
  end;
  FThreads := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
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
  {$IFNDEF ZEOS_DISABLE_INTERBASE}
  FIBConnection := nil;
  {$ENDIF ZEOS_DISABLE_INTERBASE}
  {$IFNDEF ZEOS_DISABLE_FIREBIRD}
  FFBConnection := nil;
  {$ENDIF ZEOS_DISABLE_FIREBIRD}
  If (not (csDesigning in ComponentState)) and (Assigned(FConnection)) then
  try
    {$IFNDEF ZEOS_DISABLE_FIREBIRD}
    Connection.DbcConnection.QueryInterface(IZFirebirdConnection, FFBConnection);
    {$ENDIF ZEOS_DISABLE_FIREBIRD}
    {$IFNDEF ZEOS_DISABLE_INTERBASE}
    Connection.DbcConnection.QueryInterface(IZInterbase6Connection, FIBConnection);
    {$ENDIF ZEOS_DISABLE_INTERBASE}

    If (FThreads.Count = 0) Then
      If (FEvents.Count > 0) Then
        For i := 0 To ((FEvents.Count - 1) div IB_MAX_EVENT_BLOCK) Do
          {$IFNDEF ZEOS_DISABLE_FIREBIRD}
            {$IFNDEF ZEOS_DISABLE_INTERBASE}
            if FFBConnection <> nil then
            {$ENDIF ZEOS_DISABLE_INTERBASE}
            FThreads.Add(TFBEventThread.Create(Self, i, ThreadEnded))
            {$IFNDEF ZEOS_DISABLE_INTERBASE}
          else
            FThreads.Add(TIBEventThread.Create(Self, i, ThreadEnded));
            {$ENDIF ZEOS_DISABLE_INTERBASE}
          {$ELSE ZEOS_DISABLE_FIREBIRD}
            FThreads.Add(TIBEventThread.Create(Self, i, ThreadEnded));
          {$ENDIF ZEOS_DISABLE_FIREBIRD}

  Finally
    FRegistered := FThreads.Count <> 0;
    If FRegistered Then Begin
      If not FConnection.Connected Then
        FConnection.Connect;
      {$IFNDEF ZEOS_DISABLE_FIREBIRD}
      if (FFBConnection <> nil) then begin
        FAttachment := FFBConnection.GetAttachment;
        FStatus := FFBConnection.GetStatus;
      end else begin
        FAttachment := nil;
        FStatus := nil;
      end;
      {$ENDIF ZEOS_DISABLE_FIREBIRD}
      {$IFNDEF ZEOS_DISABLE_INTERBASE}
      if FIBConnection <> nil
      then FNativeHandle := FIBConnection.GetDBHandle
      else FNativeHandle := nil;
      {$ENDIF ZEOS_DISABLE_INTERBASE}
    End;
  End;
End; // RegisterEvents


// -> ms, 18/08/2004:
//    Modified so that the native DB handle will now be retrieved by
//    method RegisterEvents. Retrieving it here caused an Exception
//    even if DB was connected.
Procedure TZIBEventAlerter.SetConnection({$IFDEF AUTOREFCOUNT}const{$ENDIF}Value: TZConnection);
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
    if Value <> nil then
      FPlainDriver := FConnection.DbcConnection.GetIZPlainDriver.GetInstance as TZInterbaseFirebirdPlainDriver;
  End;
End; // SetConnection

procedure TZIBEventAlerter.SetEvents({$IFDEF AUTOREFCOUNT}const{$ENDIF}Value: TStrings);
begin
  FEvents.Assign(Value);
end;

procedure TZIBEventAlerter.SetRegistered(const Value: boolean);
begin
  FRegistered := Value;
  if csDesigning in ComponentState then
    exit;
  if Value
  then RegisterEvents
  else UnRegisterEvents;
end;

procedure TZIBEventAlerter.UnregisterEvents;
var
  i: integer;
  Temp: TAbstractInterbaseFirebirdEventThread;
begin
  if csDesigning in ComponentState then
    exit;
  if (FThreads.Count > 0) then
  begin
    for i := (FThreads.Count - 1) downto 0 do
    begin
      Temp := TAbstractInterbaseFirebirdEventThread(FThreads[i]);
      FThreads.Delete(i);

      Temp.SignalTerminate;
      Temp.WaitFor;
      Temp.Free;
    end;
  end;
  FRegistered := FThreads.Count <> 0;
end;

{ TAbstractInterbaseFirebirdEventThread }

constructor TAbstractInterbaseFirebirdEventThread.Create(
  Owner: TZIBEventAlerter; EventGrp: integer; TermEvent: TNotifyEvent);
begin
  // NB: we call inherited constructor after custom stuff because thread can't
  // start itself from within constructor (it gets started 2nd time in AfterConstruction
  // thus raising exception)
  FCancelAlerts := False;
  Signal := TSimpleEvent.Create;
  FParent := Owner;
  EventGroup := EventGrp;
  OnTerminate := TermEvent;
  {$IFDEF UNICODE}
  FCodePage := Owner.Connection.DbcConnection.GetConSettings.ClientCodePage.CP;
  {$ENDIF}
  inherited Create(False);
end;

destructor TAbstractInterbaseFirebirdEventThread.Destroy;
begin
  try
    UnRegisterEvents;
  except
    ReturnValue := Ord(HandleException);
  end;
  Signal.Free;
  inherited Destroy;
end;

procedure TAbstractInterbaseFirebirdEventThread.DoEvent;
begin
  FParent.FOnEventAlert(FParent, FParent.FEvents[((EventGroup * IB_MAX_EVENT_BLOCK) + WhichEvent)],
    CountForEvent, FCancelAlerts)
end;

procedure TAbstractInterbaseFirebirdEventThread.DoHandleException;
begin
  SysUtils.ShowException(FExceptObject, FExceptAddr);
end;

procedure TAbstractInterbaseFirebirdEventThread.Execute;
begin
  RegisterEvents;
  QueueEvents;
  try
    repeat
      Signal.WaitFor(INFINITE);
      if EventsReceived then begin
        ProcessEvents;
        QueueEvents;
      end;
    until Terminated;
    ReturnValue := 0;
  except
    ReturnValue := Ord(HandleException);
  end;
end;

function TAbstractInterbaseFirebirdEventThread.HandleException: boolean;
begin
  if not FParent.ThreadException then begin
    Result := True;
    FParent.ThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        Synchronize(DoHandleException);
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end else
    Result := False;
end;

procedure TAbstractInterbaseFirebirdEventThread.ProcessEvents;
var
  i: integer;
  EventCounts: TARRAY_ISC_EVENTCOUNTS;
begin
  FParent.PlainDriver.isc_event_counts(@EventCounts, EventBufferLen,
    EventBuffer, ResultBuffer);
  if (Assigned(FParent.FOnEventAlert) and (not FirstTime)) then begin
    FCancelAlerts := False;
    for i := 0 to (EventCount - 1) do
      if (EventCounts[i] <> 0) then begin
        WhichEvent := i;
        CountForEvent := EventCounts[i];
        Synchronize(DoEvent)
      end;
  end;
  FirstTime := False;
end;

procedure TAbstractInterbaseFirebirdEventThread.QueueEvents;
begin
  EventsReceived := False;
  Signal.ResetEvent;
  Synchronize(AsyncQueEvents);
end;

procedure TAbstractInterbaseFirebirdEventThread.RegisterEvents;
{$IFDEF UNICODE}
var
  // Holder for ANSI strings converted from Unicode items of FEvents.
  // Obligatory! Otherwise pointer returned from EBP will point to
  // invalid (released) memory.
  EBPArray: array[1..IB_MAX_EVENT_BLOCK] of RawByteString;
{$ENDIF}

  function EBP(Index: integer): PAnsiChar;
  var EvListIndex: Integer;
  begin
    // Index is 1-based, FEvents is 0-based
    EvListIndex := Index + (EventGroup * IB_MAX_EVENT_BLOCK) - 1;
    if (EvListIndex >= FParent.FEvents.Count) then
      Result := nil
    else
    {$IFDEF UNICODE}
    begin
      EBPArray[Index] := ZUnicodeToRaw(FParent.FEvents[EvListIndex], FCodePage);
      Result := Pointer(EBPArray[Index]);
    end;
    {$ELSE}
    Result := Pointer(FParent.FEvents[EvListIndex]);
    {$ENDIF}
  end;

begin
  EventBuffer := nil;
  ResultBuffer := nil;
  EventBufferLen := 0;
  FirstTime := True;
  EventCount := (FParent.FEvents.Count - (EventGroup * IB_MAX_EVENT_BLOCK));
  if (EventCount > IB_MAX_EVENT_BLOCK) then
    EventCount := IB_MAX_EVENT_BLOCK;

  EventBufferLen := FParent.PlainDriver.isc_event_block(@EventBuffer,
    @ResultBuffer, EventCount,
    EBP(1), EBP(2),  EBP(3),  EBP(4),  EBP(5),  EBP(6),  EBP(7), EBP(8),
    EBP(9), EBP(10), EBP(11), EBP(12), EBP(13), EBP(14), EBP(15));
end;

procedure TAbstractInterbaseFirebirdEventThread.SignalEvent;
begin
  EventsReceived := True;
  Signal.SetEvent;
end;

procedure TAbstractInterbaseFirebirdEventThread.SignalTerminate;
begin
  if not Terminated then begin
    Terminate;
    Signal.SetEvent;
  end;
end;

procedure TAbstractInterbaseFirebirdEventThread.UnRegisterEvents;
begin
  FParent.PlainDriver.isc_free(EventBuffer);
  EventBuffer := nil;
  FParent.PlainDriver.isc_free(ResultBuffer);
  ResultBuffer := nil;
end;

procedure TAbstractInterbaseFirebirdEventThread.UpdateResultBuffer(
  Length: Integer; Updated: Pointer);
begin
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Updated^, ResultBuffer^, Length);
end;

{$IFNDEF ZEOS_DISABLE_INTERBASE}

{ TIBEventThread }

procedure EventCallback(UserData: PVoid; Length: ISC_USHORT; Updated: PISC_UCHAR); cdecl;
begin
  if (Assigned(UserData) and Assigned(Updated) and (Length > 0)) then begin
    TIBEventThread(UserData).UpdateResultBuffer(Length, Updated);
    TIBEventThread(UserData).SignalEvent;
  end;
end;

procedure TIBEventThread.UnRegisterEvents;
begin
  FParent.PlainDriver.isc_cancel_events(@StatusVector, FParent.FNativeHandle, @EventID);
  inherited UnRegisterEvents;
end;

procedure TIBEventThread.AsyncQueEvents;
begin
  if FParent.PlainDriver.isc_que_events(@StatusVector,
    FParent.FNativeHandle, @EventID, EventBufferLen,
    EventBuffer, TISC_CALLBACK(@EventCallback), PVoid(Self)) <> 0 then
    if Assigned(FParent.OnError) then // only if someone handles errors
      // Very Ugly! OnError should accept Exception as parameter.
      // But we keep backward compatibility here
      try
        CheckInterbase6Error(FParent.PlainDriver, StatusVector, nil);
      except on E: Exception do
        if E is EZSQLException then
          FParent.OnError(FParent, EZSQLException(E).ErrorCode)
        else
          FParent.OnError(FParent, 0);
      end;
end;

{$ENDIF ZEOS_DISABLE_INTERBASE}

{$IFNDEF ZEOS_DISABLE_FIREBIRD}

{ TFBEventThread }

procedure TFBEventThread.AsyncQueEvents;
begin
  if FEventCallback <> nil then begin
    FEventCallback.release;
    FEventCallback := nil;
  end;
  FEventCallback := TFBEventCallback.Create(Self, 'EventCallback');
  FEvents := fParent.FAttachment.queEvents(fParent.FStatus, FEventCallback,
    EventBufferLen, BytePtr(EventBuffer));
end;

procedure TFBEventThread.UnRegisterEvents;
begin
  if FEvents <> nil then
  try
    FEvents.Cancel(FParent.FStatus);
    if ((FParent.FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
      FParent.FFBConnection.HandleError(FParent.FStatus, 'IAttachment.queEvents', FParent.FFBConnection, lcOther);
  finally
    FEvents.release;
    FEvents := nil;
    if FEventCallback <> nil then begin
      FEventCallback.release;
      FEventCallback := nil;
    end;
  end;
end;

{ TFBEventCallback }

procedure TFBEventCallback.addRef;
begin
  Inc(FRefCnt);
end;

procedure TFBEventCallback.CancelEvent;
begin
  FOwner.Signal.SetEvent;
end;

constructor TFBEventCallback.Create(aOwner: TFBEventThread;
  const aName: RawByteString);
begin
  inherited Create;
  FName := aName;
  FOwner := aOwner;
end;

procedure TFBEventCallback.eventCallbackFunction(length: Cardinal;
  events: BytePtr);
begin
  FOwner.UpdateResultBuffer(Length, events);
  FOwner.SignalEvent;
end;

function TFBEventCallback.release: Integer;
begin
  Dec(FRefCnt);
  if FRefCnt = 0 then
    Free;
  Result := FRefCnt;
end;

procedure TFBEventCallback.WaitForEvent;
begin
  FOwner.Signal.WaitFor(INFINITE)
end;

{$ENDIF ZEOS_DISABLE_FIREBIRD}

{ TZIBEventAlerter }

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
        if (FEvents[i] = EmptyStr) then
          FEvents.Delete(i)
        else if (Length(FEvents[i]) > (IB_MAX_EVENT_LENGTH - 1)) then
          FEvents[i] := Copy(FEvents[i], 1, (IB_MAX_EVENT_LENGTH - 1));
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
  if (Sender is TAbstractInterbaseFirebirdEventThread) then begin
    ThreadIdx := FThreads.IndexOf(Sender);
    if (ThreadIdx > -1) then
      FThreads.Delete(ThreadIdx);
    if (TAbstractInterbaseFirebirdEventThread(Sender).ReturnValue = 1) then begin
      if Registered then
        UnRegisterEvents;
      ThreadException := False;
    end
  end;
end;

procedure TZIBEventAlerter.SetAutoRegister(const Value: boolean);
begin
  if FAutoRegister <> Value then begin
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

{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD} //if set we have an empty unit
end.
