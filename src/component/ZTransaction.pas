{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Transaction classes                    }
{                                                         }
{            Originally written by EgonHugeist            }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTransaction;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes,
  ZClasses,
  ZDbcIntfs,
  ZAbstractConnection;

type
  /// <summary>Implements an abstract transaction component.</summary>
  TZAbstractTransaction = class(TZAbstractConnectionLinkedComponent)
  private
    FApplyPendingUpdatesOnCommit: Boolean;
    FDisposePendingUpdatesOnRollback: Boolean;
    FLinkedComponents: TZSortedList;
    FParams: TStrings;
    FAutoCommit: Boolean;
    FReadOnly: Boolean;
    FTransactIsolationLevel: TZTransactIsolationLevel;
    FBeforeStartTransaction: TNotifyEvent;
    FAfterStartTransaction: TNotifyEvent;
    FBeforeCommit: TNotifyEvent;
    FAfterCommit: TNotifyEvent;
    FBeforeRollback: TNotifyEvent;
    FAfterRollback: TNotifyEvent;
    FConnection: TZAbstractConnection;
    FTransaction: IZTransaction;
    FTxnLevel: Integer;
    FSupportsOpenCursorsAcrossRollback,
    FSupportsOpenCursorsAcrossCommit: Boolean;
    function GetActive: Boolean;
    function GetAutoCommit: Boolean;
    procedure SetAutoCommit(Value: Boolean);
    /// <summary>Puts this transaction in read-only mode as a hint to enable
    ///  database optimizations. Note: This method cannot be called while in the
    ///  middle of a transaction.</summary>
    /// <param>"value" true enables read-only mode; false disables read-only
    ///  mode.</param>
    procedure SetReadOnly(Value: Boolean);
    procedure SetTransactIsolationLevel(Value: TZTransactIsolationLevel);
    procedure SetParams(Value: TStrings);
    function GetTransactionManager: IZTransactionManager;
    procedure TxnPropertiesChanged;
    procedure CheckConnected;
  protected
    procedure SetConnection(Value: TZAbstractConnection); override;
    function GetIZTransaction: IZTransaction;
  public
    constructor Create(AOnwer: TComponent); override;
    procedure BeforeDestruction; override;
  public
    procedure RegisterComponent(Value: TComponent);
    procedure UnregisterComponent(Value: TComponent);
  public
    function StartTransaction: Integer;
    procedure Commit;
    procedure Rollback;
    property Active: Boolean read GetActive;
    property Connection: TZAbstractConnection read FConnection write SetConnection;
    property BeforeStartTransaction: TNotifyEvent read FBeforeStartTransaction
      write FBeforeStartTransaction;
    property AfterStartTransaction: TNotifyEvent read FAfterStartTransaction
      write FAfterStartTransaction;
    property BeforeCommit: TNotifyEvent read FBeforeCommit write FBeforeCommit;
    property AfterCommit: TNotifyEvent read FAfterCommit write FAfterCommit;
    property BeforeRollback: TNotifyEvent read FBeforeRollback write FBeforeRollback;
    property AfterRollback: TNotifyEvent read FAfterRollback write FAfterRollback;

    property TransactIsolationLevel: TZTransactIsolationLevel read
      FTransactIsolationLevel write SetTransactIsolationLevel default tiReadCommitted;
    property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Properties: TStrings read FParams write SetParams;
    property ApplyPendingUpdatesOnCommit: Boolean read FApplyPendingUpdatesOnCommit
      write FApplyPendingUpdatesOnCommit default True;
    property DisposePendingUpdatesOnRollback: Boolean read FDisposePendingUpdatesOnRollback
      write FDisposePendingUpdatesOnRollback default True;
  end;

implementation

uses ZAbstractDataset, ZAbstractRODataset, ZSqlProcessor, ZMessages;
type
  TZProtectedAbstractRWTxnSeqDataSet = Class(TZAbstractRWTxnSeqDataSet);
  TZProtectedAbstractRODataset = Class(TZAbstractRODataset);
{ TZAbstractTransaction }

procedure TZAbstractTransaction.BeforeDestruction;
var I: Integer;
  Comp: TComponent;
begin
  for i := FLinkedComponents.Count -1 downto 0 do begin
    Comp := TComponent(FLinkedComponents[i]);
    if Comp.InheritsFrom(TZAbstractRWTxnSeqDataSet) then
      if TZProtectedAbstractRWTxnSeqDataSet(Comp).UpdateTransaction = Self then
        TZProtectedAbstractRWTxnSeqDataSet(Comp).UpdateTransaction := nil;
    if Comp.InheritsFrom(TZAbstractRODataset) then
      if TZProtectedAbstractRODataset(Comp).Transaction = Self then
        TZProtectedAbstractRODataset(Comp).Transaction := nil;
    if Comp.InheritsFrom(TZSQLProcessor) then
      {if TZSQLProcessor(Comp).Transaction = Self then
        TZSQLProcessor(Comp).Transaction := nil;}
  end;
  FreeAndNil(FParams);
  FreeAndNil(FLinkedComponents);
  if (FTransaction <> nil) then begin
    with GetTransactionManager do
      if IsTransactionValid(FTransaction) then ReleaseTransaction(FTransaction);
    FTransaction := nil;
  end;
  if Connection <> nil then
    SetConnection(nil);
  inherited BeforeDestruction;
end;

procedure TZAbstractTransaction.CheckConnected;
begin
  if (FConnection = nil) or (not FConnection.Connected) then
    raise EZDatabaseError.Create(SConnectionIsNotOpened);
end;

type //To get protected methodes
  THack_ZAbstractDataset = Class(TZAbstractRWTxnSeqDataSet);
procedure TZAbstractTransaction.Commit;
var I: Integer;
    Row: NativeInt;
    AComp: TComponent;
begin
  if (FTransaction = nil) or FTransaction.GetAutoCommit then
    raise EZDatabaseError.Create(SInvalidOpInAutoCommit);
  if Assigned(FBeforeCommit) then
    FBeforeCommit(Self);
  try
    if FApplyPendingUpdatesOnCommit or FSupportsOpenCursorsAcrossCommit then
      for I := FLinkedComponents.Count -1 downto 0 do begin
        AComp := TComponent(FLinkedComponents[i]);
        if (AComp is TZAbstractRWTxnSeqDataSet) and THack_ZAbstractDataset(AComp).IsCursorOpen then begin
          if FDisposePendingUpdatesOnRollback and THack_ZAbstractDataset(AComp).UpdatesPending then
            THack_ZAbstractDataset(AComp).ApplyUpdates;
          if not FSupportsOpenCursorsAcrossCommit and not THack_ZAbstractDataset(AComp).LastRowFetched then begin
            Row := THack_ZAbstractDataset(AComp).CurrentRow;
            THack_ZAbstractDataset(AComp).DisableControls;
            try
              THack_ZAbstractDataset(AComp).FetchRows(0);
            finally
              THack_ZAbstractDataset(AComp).GotoRow(Row);
              THack_ZAbstractDataset(AComp).EnableControls;
            end;
          end;
        end;
      end;
  finally
    FTransaction.Commit;
  end;
  if Assigned(FAfterCommit) then
    FAfterCommit(Self);
end;

constructor TZAbstractTransaction.Create(AOnwer: TComponent);
begin
  inherited;
  FParams := TStringList.Create;
  FLinkedComponents := TZSortedList.Create;
  FDisposePendingUpdatesOnRollback := True;
  FApplyPendingUpdatesOnCommit := True;
  FTransactIsolationLevel := tiReadCommitted;
  FAutoCommit := True;
end;

function TZAbstractTransaction.GetActive: Boolean;
var TxnMngr: IZTransactionManager;
begin
  if (FConnection <> nil) and FConnection.Connected and (FTransaction <> nil) and
     (FConnection.DbcConnection.QueryInterface(IZTransactionManager, TxnMngr) = S_OK)
  then Result := TxnMngr.IsTransactionValid(FTransaction)
  else Result := False;
end;

function TZAbstractTransaction.GetAutoCommit: Boolean;
begin
  if GetActive
  then Result := FTransaction.GetAutoCommit
  else Result := FAutoCommit;
end;

function TZAbstractTransaction.GetIZTransaction: IZTransaction;
var TxnManager: IZTransactionManager;
begin
  TxnManager := GetTransactionManager;
  if (FTransaction = nil) or (not TxnManager.IsTransactionValid(FTransaction)) then
    Result := TxnManager.CreateTransaction(FAutoCommit,
      FReadOnly, FTransactIsolationLevel, FParams)
  else Result := FTransaction;
  with Connection.DbcConnection.GetMetadata.GetDatabaseInfo do begin
    FSupportsOpenCursorsAcrossRollback := SupportsOpenCursorsAcrossRollback;
    FSupportsOpenCursorsAcrossCommit := FSupportsOpenCursorsAcrossCommit;
  end;
end;

function TZAbstractTransaction.GetTransactionManager: IZTransactionManager;
begin
  CheckConnected;
  if FConnection.DbcConnection.QueryInterface(IZTransactionManager, Result) <> S_OK then
    raise EZDatabaseError.Create(SUnsupportedOperation);
end;

procedure TZAbstractTransaction.RegisterComponent(Value: TComponent);
var IDX: Integer;
begin
  if (Value = nil) or not (Value.InheritsFrom(TZAbstractRODataSet) or Value.InheritsFrom(TZSQLProcessor)) then
    raise EZDatabaseError.Create('Invalid Component');
  IDX := FLinkedComponents.IndexOf(Value);
  if IDX = -1 then
    FLinkedComponents.Add(Pointer(Value));
end;

procedure TZAbstractTransaction.Rollback;
var I: Integer;
    Row: NativeInt;
    AComp: TComponent;
begin
  if (FTransaction = nil) then
    raise EZDatabaseError.Create(SInvalidOpInAutoCommit);
  if Assigned(FBeforeCommit) then
    FBeforeRollback(Self);
  try
    if FDisposePendingUpdatesOnRollback or FSupportsOpenCursorsAcrossRollback then
      for i := 0 to FLinkedComponents.Count -1 do  begin
        AComp := TComponent(FLinkedComponents[i]);
        if (AComp is TZAbstractRWTxnSeqDataSet) and THack_ZAbstractDataset(AComp).IsCursorOpen then begin
          if FDisposePendingUpdatesOnRollback and TZAbstractRWTxnSeqDataSet(AComp).UpdatesPending then
            THack_ZAbstractDataset(AComp).DisposeCachedUpdates;
          if not FSupportsOpenCursorsAcrossRollback and not THack_ZAbstractDataset(AComp).LastRowFetched then begin
            Row := THack_ZAbstractDataset(AComp).CurrentRow;
            THack_ZAbstractDataset(AComp).DisableControls;
            try
              THack_ZAbstractDataset(AComp).FetchRows(0);
            finally
              THack_ZAbstractDataset(AComp).GotoRow(Row);
              THack_ZAbstractDataset(AComp).EnableControls;
            end;
          end;
        end;
      end;
  finally
    FTransaction.Rollback;
  end;
  if Assigned(FAfterRollback) then
    FAfterRollback(Self);
end;

procedure TZAbstractTransaction.SetAutoCommit(Value: Boolean);
begin
  if Value <> FAutoCommit then begin
    FAutoCommit := Value;
    TxnPropertiesChanged;
  end;
end;

procedure TZAbstractTransaction.SetConnection(Value: TZAbstractConnection);
begin
  if Value <> FConnection then begin
    if (Value<> nil) and GetActive then
      raise EZDatabaseError.Create(SInvalidOpInNonAutoCommit);
    if (Value = nil) then
      FConnection.UnregisterComponent(Self);
    FConnection := Value;
    if (FConnection <> nil) then
      FConnection.RegisterComponent(Self);
  end;
end;

procedure TZAbstractTransaction.SetParams(Value: TStrings);
begin
  FParams.Clear;
  FParams.AddStrings(Value);
  TxnPropertiesChanged;
end;

procedure TZAbstractTransaction.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    TxnPropertiesChanged;
  end;
end;

procedure TZAbstractTransaction.SetTransactIsolationLevel(
  Value: TZTransactIsolationLevel);
begin
  if Value <> FTransactIsolationLevel then begin
    FTransactIsolationLevel := Value;
    TxnPropertiesChanged;
  end;
end;

function TZAbstractTransaction.StartTransaction: Integer;
var AutoCommit: Boolean;
begin
  AutoCommit := FAutoCommit;
  if not GetActive then try
    FAutoCommit := False;
    FTransaction := GetIZTransaction;
  finally
    FAutoCommit := AutoCommit;
  end;
  if Assigned(FBeforeStartTransaction) then
    FBeforeStartTransaction(Self);
  Result := FTransaction.StartTransaction;
  FTxnLevel := Result;
  if Assigned(FAfterStartTransaction) then
    FAfterStartTransaction(Self);
end;

procedure TZAbstractTransaction.TxnPropertiesChanged;
begin
  if GetActive then
    raise EZDatabaseError.Create(SInvalidOpInNonAutoCommit);
end;

procedure TZAbstractTransaction.UnregisterComponent(Value: TComponent);
var IDX: Integer;
begin
  IDX := FLinkedComponents.IndexOf(Value);
  if IDX <> -1 then
    FLinkedComponents.Delete(IDX);
end;

end.
