{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{     Launch a database operation in the background       }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

Unit ZMethodInThread;

Interface

Uses SysUtils, ZAbstractRODataSet, DB, ZAbstractConnection, Classes, ZAbstractDataSet;

Type
 TProcedureOfObject = Procedure Of Object;
 TErrorEvent = Procedure(Sender: TObject; Error: Exception) Of Object;

 /// <summary>Execute database component operations in a background thread, leaving the VCL thread responsive. Background operations can be
 /// gracefully terminated (where supported) with TZConnection.AbortOperation.</summary>
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
  Procedure InternalRefresh;
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
  /// <summary>Gives information about the status of the background thread</summary>
  /// <returns>True if there is a background operation in progress, false if not</returns>
  Property IsRunning: Boolean Read GetThreadRunning;
  /// <summary>The OnError event fires if an exception was raised during the background operation</summary>
  /// <remarks>The event handler is executed in the background thread's context. Always pay attention for proper synchronization!</remarks>
  Property OnError: TErrorEvent Read _methoderror Write SetOnError;
  /// <summary>The OnFinish event fires when the background operation ended. The OnFinish event always fires, even if an error happened (except if killed)</summary>
  /// <remarks>The event handles is executed in the background thread's context. Always pay attention for proper synchronization!</remarks>
  Property OnFinish: TNotifyEvent Read _methodfinish Write SetOnFinish;
  /// <summary>Returns the thread ID of the backgroun operation</summary>
  /// <returns>The thread ID of the background operation, or 0 if there is none</returns>
  Property ThreadID: Cardinal Read GetThreadId;
  /// <summary>Apply the changes of a dataset to the database in the background</summary>
  /// <param name="inDataSet">The dataset where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure ApplyUpdates(inDataSet: TZAbstractDataset);
  /// <summary>Connect to a database in the background</summary>
  /// <param name="inConnection">The connection where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Connect(inConnection: TZAbstractConnection);
  /// <summary>Commit the current transaction to the database in the background</summary>
  /// <param name="inConnection">The connection where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Commit(inConnection: TZAbstractConnection);
  /// <summary>Disconnect from a database in the background</summary>
  /// <param name="inConnection">The connection where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Disconnect(inConnection: TZAbstractConnection);
  /// <summary>Execute an INSERT/UPDATE/DELETE query in the background</summary>
  /// <param name="inDataSet">The dataset where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure ExecSQL(inDataSet: TZAbstractRODataSet);
  /// <summary>Load the results of a SELECT query in the background</summary>
  /// <param name="inDataSet">The dataset where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Open(inDataSet: TZAbstractRODataset);
  /// <summary>Post changes made in the dataset to the database in the background</summary>
  /// <param name="inDataSet">The dataset where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Post(inDataSet: TZAbstractRODataset);
  /// <summary>Reload the results of a SELECT query in the background</summary>
  /// <param name="inDataSet">The dataset where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Refresh(inDataSet: TZAbstractRODataset);
  /// <summary>Roll back the current transaction in the background</summary>
  /// <param name="inConnection">The connection where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure Rollback(inConnection: TZAbstractConnection);
  /// <summary>Start a new transaction in the background</summary>
  /// <param name="inConnection">The connection where the method should be executed</param>
  /// <remarks>This method returns immediately. Use the event handlers (or poll the .IsRunning property) to react when the operation finishes.</remarks>
  Procedure StartTransaction(inConnection: TZAbstractConnection);
  /// <summary>Pause the program flow until the background operation finishes</summary>
  /// <remarks>This method is blocking, causing the calling thread to hang. If called from the VCL thread, the application will appear frozen.</remarks>
  Procedure WaitFor;
  /// <summary>Ungracefully terminate the background operation</summary>
  /// <param name="inAreYouSure">Are you sure you want to immediately terminate the background operation?</param>
  /// <remarks>Terminating a thread is unsafe and can cause appication instability, data corruption and memory leaks. Don't use this in normal circumstances.</remarks>
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
 // Disable event handlers
 afteropen := _dataset.AfterOpen;
 afterscroll := _dataset.AfterScroll;
 _dataset.AfterOpen := nil;
 _dataset.AfterScroll := nil;
 Try
  // First, open the dataset
  _dataset.Open;
  // We also have to call .FetchAll to make sure everything is downloaded. Not doing this will cause the application to fetch all data even
  // if we just want to access .RecordCount.
  _dataset.FetchAll;
  // Now that all data is set and ready, execute the event handlers - if any
  If Assigned(afteropen) Then afteropen(_dataset);
  If Assigned(afterscroll) Then afterscroll(_dataset);
 Finally
  // Reassign event handles
  _dataset.AfterOpen := afteropen;
  _dataset.AfterScroll := afterscroll;
  _dataset := nil;
 End;
End;

Procedure TZMethodInThread.InternalRefresh;
Var
 afteropen, afterscroll: TDataSetNotifyEvent;
Begin
 // Disable event handlers
 afteropen := _dataset.AfterOpen;
 afterscroll := _dataset.AfterScroll;
 _dataset.AfterOpen := nil;
 _dataset.AfterScroll := nil;
 Try
  // First, refresh the dataset
  _dataset.Refresh;
  // We also have to call .FetchAll to make sure everything is downloaded. Not doing this will cause the application to fetch all data even
  // if we just want to access .RecordCount.
  _dataset.FetchAll;
  // Now that all data is set and ready, execute the event handlers - if any
  If Assigned(afteropen) Then afteropen(_dataset);
  If Assigned(afterscroll) Then afterscroll(_dataset);
 Finally
  // Reassign event handles
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
 // Confirmation parameter is included because terminating a thread like this is potentially dangerous and can cause application instability.
 // Normally noone should use this ever, just wait for the database server to interrupt the action after calling .AbortOperation
 If inAreYouSure And Self.IsRunning Then
 {$IF defined(MSWINDOWS)}
   TerminateThread(_methodthread.Handle, 0);
 {$ELSE IF defined(LINUX)}
   pthread_kill(_methodthread.Handle, SIGSTOP);
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
 Self.CheckRunning;
 _dataset := inDataSet;
 StartMethodThread(InternalRefresh);
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