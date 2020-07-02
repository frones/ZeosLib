Unit ZMethodInThread;

Interface

Uses SysUtils, ZAbstractRODataSet, DB, ZAbstractConnection, Classes, ZAbstractDataSet;

Type
 TProcedureOfObject = Procedure Of Object;
 TErrorEvent = Procedure(Sender: TObject; Error: Exception) Of Object;

 TZMethodInThread = Class
 private
  _connection: TZAbstractConnection;
  _dataset: TZAbstractRODataset;
  _methodthread: TThread;
  _methoderror: TErrorEvent;
  _methodfinish: TNotifyEvent;
  _threaderror: Exception;
  Procedure CheckRunning;
  Procedure InternalOpen;
  Procedure InternalStartTransaction;
  Procedure SetOnError(Const inErrorEvent: TErrorEvent);
  Procedure SetOnFinish(Const inFinishEvent: TNotifyEvent);
  Procedure StartMethodThread(inMethod: TProcedureOfObject);
  Procedure ThreadError(Sender: TObject; Error: Exception);
  Procedure ThreadFinish(Sender: TObject);
  Function GetThreadID: Cardinal;
  Function GetThreadRunning: Boolean;
 public
  Constructor Create;
  Destructor Destroy; Override;
  Property IsRunning: Boolean Read GetThreadRunning;
  Property OnError: TErrorEvent Read _methoderror Write SetOnError;
  Property OnFinish: TNotifyEvent Read _methodfinish Write SetOnFinish;
  Property ThreadID: Cardinal Read GetThreadId;
  Procedure ApplyUpdates(inDataSet: TZAbstractDataset);
  Procedure Connect(inConnection: TZAbstractConnection);
  Procedure Commit(inConnection: TZAbstractConnection);
  Procedure Disconnect(inConnection: TZAbstractConnection);
  Procedure ExecSQL(inDataSet: TZAbstractRODataSet);
  Procedure Open(inDataSet: TZAbstractRODataset);
  Procedure Post(inDataSet: TZAbstractRODataset);
  Procedure Refresh(inDataSet: TZAbstractRODataset);
  Procedure Rollback(inConnection: TZAbstractConnection);
  Procedure StartTransaction(inConnection: TZAbstractConnection);
  Procedure WaitFor;
  Procedure Kill(inAreYouSure: Boolean);
 End;

Implementation

Uses ZDbcIntfs, ZMessages{$IF defined(MSWINDOWS)}, Windows{$ELSE IF defined(LINUX)}, Linux{$ENDIF};

{$IF defined(LINUX)}
Function pthread_kill(ThreadID: TThreadID; SigNum: Integer): Integer; Cdecl; External 'libpthread.so.0' name 'pthread_kill';
{$ENDIF}

Type
 TZMethodThread = Class(TThread)
 private
  _runmethod: TProcedureOfObject;
  _onerror: TErrorEvent;
 protected
  Procedure Execute; Override;
 public
  Constructor Create(inMethod: TProcedureOfObject; inErrorEvent: TErrorEvent; inFinishEvent: TNotifyEvent); ReIntroduce;
 End;

Constructor TZMethodThread.Create(inMethod: TProcedureOfObject; inErrorEvent: TErrorEvent; inFinishEvent: TNotifyEvent);
Begin
 inherited Create(False);
 _runmethod := inMethod;
 _onerror := inErrorEvent;
 Self.OnTerminate := inFinishEvent;
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

Procedure TZMethodInThread.ApplyUpdates(inDataSet: TZAbstractDataset);
Begin
 StartMethodThread(inDataSet.ApplyUpdates);
End;

Procedure TZMethodInThread.CheckRunning;
Begin
 If Self.IsRunning Then Raise EZSQLException.Create(SBackgroundOperationStillRunning)
   Else
 If Assigned(_methodthread) Then FreeAndNil(_methodthread);
End;

Procedure TZMethodInThread.Commit(inConnection: TZAbstractConnection);
Begin
 StartMethodThread(inConnection.Commit);
End;

Procedure TZMethodInThread.Connect(inConnection: TZAbstractConnection);
Begin
 StartMethodThread(inConnection.Connect);
End;

Constructor TZMethodInThread.Create;
Begin
 inherited;
 _methodthread := nil;
 _methoderror := nil;
 _threaderror := nil;
End;

Destructor TZMethodInThread.Destroy;
Begin
 Self.WaitFor;
 If Assigned(_methodthread) Then FreeAndNil(_methodthread);
 inherited;
End;

Procedure TZMethodInThread.Disconnect(inConnection: TZAbstractConnection);
Begin
 StartMethodThread(inConnection.Disconnect);
End;

Procedure TZMethodInThread.ExecSQL(inDataSet: TZAbstractRODataSet);
Begin
 StartMethodThread(inDataSet.ExecSQL);
End;

Procedure TZMethodInThread.InternalOpen;
Var
 afteropen, afterscroll: TDataSetNotifyEvent;
Begin
 afteropen := _dataset.AfterOpen;
 afterscroll := _dataset.AfterScroll;
 _dataset.AfterOpen := nil;
 _dataset.AfterScroll := nil;
 Try
  _dataset.Open;
  _dataset.FetchAll;
  If Assigned(afteropen) Then afteropen(_dataset);
  If Assigned(afterscroll) Then afterscroll(_dataset);
 Finally
  _dataset.AfterOpen := afteropen;
  _dataset.AfterScroll := afterscroll;
  _dataset := nil;
 End;
End;

Procedure TZMethodInThread.InternalStartTransaction;
Begin
 Try
  _connection.StartTransaction;
 Finally
  _connection := nil;
 End;
End;

Procedure TZMethodInThread.Kill(inAreYouSure: Boolean);
Begin
 // Confirmation parameter is included because terminating a thread like
 // this is potentially dangerous and can cause application instability.
 // Normally noone should use this ever, just wait for the database server
 // to interrupt the action after calling .AbortOperation
 If Not inAreYouSure Then Exit;
 {$IF defined(MSWINDOWS)}
  If Self.IsRunning Then TerminateThread(_methodthread.Handle, 0);
 {$ELSE IF defined(LINUX)}
  If Self.IsRunning Then pthread_kill(_methodthread.Handle, SIGSTOP);
 {$ENDIF}
End;

Procedure TZMethodInThread.Open(inDataSet: TZAbstractRODataset);
Begin
 Self.CheckRunning;
 _dataset := inDataSet;
 StartMethodThread(InternalOpen);
End;

Procedure TZMethodInThread.Post(inDataSet: TZAbstractRODataset);
Begin
 StartMethodThread(inDataSet.Post);
End;

Procedure TZMethodInThread.Refresh(inDataSet: TZAbstractRODataset);
Begin
 StartMethodThread(inDataSet.Refresh);
End;

Procedure TZMethodInThread.Rollback(inConnection: TZAbstractConnection);
Begin
 StartMethodThread(inConnection.Rollback);
End;

Procedure TZMethodInThread.SetOnError(Const inErrorEvent: TErrorEvent);
Begin
 Self.CheckRunning;
 _methoderror := inErrorEvent;
End;

Procedure TZMethodInThread.SetOnFinish(Const inFinishEvent: TNotifyEvent);
Begin
 Self.CheckRunning;
 _methodfinish := inFinishEvent;
End;

Procedure TZMethodInThread.StartMethodThread(inMethod: TProcedureOfObject);
Begin
 Self.CheckRunning;
 _methodthread := TZMethodThread.Create(inMethod, ThreadError, ThreadFinish);
End;

Procedure TZMethodInThread.StartTransaction(inConnection: TZAbstractConnection);
Begin
 Self.CheckRunning;
 _connection := inConnection;
 StartMethodThread(InternalStartTransaction);
End;

Procedure TZMethodInThread.ThreadError(Sender: TObject; Error: Exception);
Begin
 If Assigned(_methoderror) Then Begin
                                _threaderror := Error;
                                _methoderror(Self, _threaderror);
                                _threaderror := nil;
                                End;
End;

Procedure TZMethodInThread.ThreadFinish(Sender: TObject);
Begin
 If Assigned(_methodfinish) Then _methodfinish(Self);
End;

Function TZMethodInThread.GetThreadID: Cardinal;
Begin
 If Self.IsRunning Then Result := _methodthread.ThreadID
   Else Result := 0;
End;

Function TZMethodInThread.GetThreadRunning: Boolean;
Begin
 Result := Assigned(_methodthread) And Not _methodthread.Finished;
End;

Procedure TZMethodInThread.WaitFor;
Begin
 If Self.IsRunning Then _methodthread.WaitFor;
End;

End.