Unit ZMethodInThread;

Interface

Uses SysUtils, ZAbstractRODataSet, DB, ZAbstractConnection;

Type
 TProcedureOfObject = Procedure Of Object;
 TErrorEvent = Procedure(Sender: TObject; Error: Exception) Of Object;

 TZMethodInThread = Class
 private
  _dataset: TZAbstractRODataset;
  _afteropen: TDataSetNotifyEvent;
  _methodthread: TObject;
  _methoderror: TErrorEvent;
  _threaderror: Exception;
  Procedure CheckRunning;
  Procedure InternalOpen;
  Procedure SetOnError(Const inErrorEvent: TErrorEvent);
  Procedure Sync_MethodError;
  Procedure Sync_AfterOpen;
  Procedure ThreadError(Sender: TObject; Error: Exception);
  Function ThreadRunning: Boolean;
 public
  Constructor Create;
  Destructor Destroy; Override;
  Property IsRunning: Boolean Read ThreadRunning;
  Property OnError: TErrorEvent Read _methoderror Write SetOnError;
  Procedure Connect(inConnection: TZAbstractConnection);
  Procedure Open(inDataSet: TZAbstractRODataset);
  Procedure WaitFor;
 End;

Implementation

Uses Classes, ZClasses, ZMessages;

Type
 TZMethodThread = Class(TThread)
 private
  _runmethod: TProcedureOfObject;
  _onerror: TErrorEvent;
 protected
  Procedure Execute; Override;
 public
  Constructor Create(inMethod: TProcedureOfObject; inErrorEvent: TErrorEvent); ReIntroduce;
 End;

Constructor TZMethodThread.Create(inMethod: TProcedureOfObject; inErrorEvent: TErrorEvent);
Begin
 inherited Create(True);
 _runmethod := inMethod;
 _onerror := inErrorEvent;
End;

Procedure TZMethodThread.Execute;
Begin
 Try
  If Assigned(_runmethod) Then _runmethod;
 Except
  On E:Exception Do If Assigned(_onerror) Then _onerror(Self, E);
 End;
End;

//
// TZMethodInThread
//

Procedure TZMethodInThread.CheckRunning;
Begin
 If Self.ThreadRunning Then Raise EZSQLException.Create(SBackgroundOperationStillRunning)
   Else
 If Assigned(_methodthread) Then FreeAndNil(_methodthread);
End;

Procedure TZMethodInThread.Connect(inConnection: TZAbstractConnection);
Begin
 CheckRunning;
 _methodthread := TZMethodThread.Create(inConnection.Connect, ThreadError);
 (_methodthread As TZMethodThread).NameThreadForDebugging('TZMethodInThread_Connect');
 (_methodthread As TZMethodThread).Start;
End;

Constructor TZMethodInThread.Create;
Begin
 inherited;
 _methodthread := nil;
 _methoderror := nil;
 _afteropen := nil;
 _threaderror := nil;
End;

Destructor TZMethodInThread.Destroy;
Begin
 Self.WaitFor;
 If Assigned(_methodthread) Then FreeAndNil(_methodthread);
 inherited;
End;

Procedure TZMethodInThread.InternalOpen;
Begin
 _afteropen := _dataset.AfterOpen;
 _dataset.AfterOpen := nil;
 Try
  _dataset.Open;
  _dataset.FetchAll;
  If Assigned(_afteropen) Then TThread.Synchronize(nil, Sync_AfterOpen);
 Finally
  _dataset.AfterOpen := _afteropen;
  _afteropen := nil;
  _dataset := nil;
 End;
End;

Procedure TZMethodInThread.Open(inDataSet: TZAbstractRODataset);
Begin
 CheckRunning;
 _dataset := inDataSet;
 _methodthread := TZMethodThread.Create(InternalOpen, ThreadError);
 (_methodthread As TZMethodThread).NameThreadForDebugging('TZMethodInThread_Open');
 (_methodthread As TZMethodThread).Start;
End;

Procedure TZMethodInThread.SetOnError(Const inErrorEvent: TErrorEvent);
Begin
 Self.CheckRunning;
 _methoderror := inErrorEvent;
End;

Procedure TZMethodInThread.Sync_AfterOpen;
Begin
 _afteropen(_dataset);
End;

Procedure TZMethodInThread.Sync_MethodError;
Begin
 _methoderror(Self, _threaderror);
End;

Procedure TZMethodInThread.ThreadError(Sender: TObject; Error: Exception);
Begin
 If Assigned(_methoderror) Then Begin
                                _threaderror := Error;
                                TThread.Synchronize(nil, Self.Sync_MethodError);
                                _threaderror := nil;
                                End;
End;

Function TZMethodInThread.ThreadRunning: Boolean;
Begin
 Result := Assigned(_methodthread) And Not (_methodthread As TZMethodThread).Finished;
End;

Procedure TZMethodInThread.WaitFor;
Begin
 If Assigned(_methodthread) Then (_methodthread As TZMethodThread).WaitFor;
End;

End.