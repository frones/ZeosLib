{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Read/Write Dataset component          }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZAbstractDataset;

interface

{$I ZComponent.inc}

uses
  Variants,
  SysUtils,  Classes, {$IFDEF MSEgui}mdb, mclasses{$ELSE}DB{$ENDIF},
  ZSqlUpdate, ZDbcIntfs, ZVariant, ZDbcCache, ZDbcCachedResultSet,
  ZAbstractRODataset, ZCompatibility, ZSequence, ZAbstractConnection,
  ZTransaction
  {$IFDEF TLIST_IS_DEPRECATED}, ZSysUtils, ZClasses{$ENDIF};

type
  {$IFDEF oldFPC} // added in 2006, probably pre 2.2.4
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
  {$ENDIF}

  {** Update Event type. }
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

  {** Defines update modes for the resultsets. }
  TZUpdateMode = (umUpdateChanged, umUpdateAll);

  {** Defines where form types for resultsets. }
  TZWhereMode = (wmWhereKeyOnly, wmWhereAll);

  {**
    Abstract dataset component which supports read/write access and
    cached updates.
  }
  TZAbstractRWDataSet = class(TZAbstractRODataset)
  private
    FCachedUpdatesBeforeMasterUpdate: Boolean;
    FCachedUpdates: Boolean;
    FCachedResultSet: IZCachedResultSet;
    FCachedResolver: IZCachedResolver;
    FGenDMLResolver: IZGenerateSQLCachedResolver;
    FOnApplyUpdateError: TDataSetErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    FUpdateMode: TZUpdateMode;
    FWhereMode: TZWhereMode;
    FBeforeApplyUpdates: TNotifyEvent; {bangfauzan addition}
    FAfterApplyUpdates: TNotifyEvent; {bangfauzan addition}
    FDetailDataSets: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FDetailCachedUpdates: array of Boolean;
  private
    function GetUpdatesPending: Boolean;
    /// <summary>Sets a new CachedUpdates property value.</summary>
    /// <param>"Value" a new CachedUpdates value.</param>
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetWhereMode(Value: TZWhereMode);
    procedure SetUpdateMode(Value: TZUpdateMode);
  protected
    property CachedResultSet: IZCachedResultSet read FCachedResultSet
      write FCachedResultSet;
    property CachedResolver: IZCachedResolver read FCachedResolver
      write FCachedResolver;
    property UpdateMode: TZUpdateMode read FUpdateMode write SetUpdateMode
      default umUpdateChanged;
    property WhereMode: TZWhereMode read FWhereMode write SetWhereMode
      default wmWhereKeyOnly;

    procedure SetTxns2Resolver(const Resolver: IZCachedResolver); virtual;
    procedure InternalClose; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalUnPrepare; override;
    {$IFNDEF WITH_InternalAddRecord_TRecBuf}
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    {$ELSE}
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
    {$ENDIF}
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure InternalUpdate;
    procedure InternalCancel; override;

    procedure DOBeforeApplyUpdates; {bangfauzan addition}
    procedure DOAfterApplyUpdates; {bangfauzan addition}

    /// <summary>Creates a DBC resultset for the query.</summary>
    /// <param>"SQL" an SQL query.</param>
    /// <param>"MaxRows" a maximum rows number (-1 for all).</param>
    /// <returns>a created DBC resultset.</returns>
    function CreateResultSet(const SQL: string; MaxRows: Integer):
      IZResultSet; override;
    {$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}
    function GetUpdatable: Boolean; override;
    {$ENDIF}
  {$IFDEF WITH_IPROVIDER}
    function PSUpdateRecord(UpdateKind: TUpdateKind;
      Delta: TDataSet): Boolean; override;
  {$ENDIF}
    procedure RegisterDetailDataSet(Value: TZAbstractRWDataSet; CachedUpdates: Boolean);
    procedure DisposeCachedUpdates;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RevertRecord;
    procedure RefreshCurrentRow(const RefreshDetails:Boolean); //FOS+ 07112006

    procedure EmptyDataSet; {bangfauzan addition}

  public
    property UpdatesPending: Boolean read GetUpdatesPending;
  published
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates
      default False;

    property OnApplyUpdateError: TDataSetErrorEvent read FOnApplyUpdateError
      write FOnApplyUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord
      write FOnUpdateRecord;

    property BeforeApplyUpdates: TNotifyEvent read FBeforeApplyUpdates
      write FBeforeApplyUpdates; {bangfauzan addition}
    property AfterApplyUpdates: TNotifyEvent read FAfterApplyUpdates
      write FAfterApplyUpdates; {bangfauzan addition}
  published
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property OnDeleteError;
    property OnEditError;
    property OnPostError;
    property OnNewRecord;
    property Options default [doCalcDefaults, doPreferPrepared];
  end;

  TZAbstractRWTxnSeqDataSet = class(TZAbstractRWDataSet)
  private
    FUpdateTransaction: TZAbstractTransaction;
    FSequence: TZSequence;
    FSequenceField: string;
  protected
    procedure SetTxns2Resolver(const Resolver: IZCachedResolver); override;
    procedure SetTransaction(Value: TZAbstractTransaction); override;
    procedure SetUpdateTransaction(Value: TZAbstractTransaction);
    /// <summary>Processes component notifications.</summary>
    /// <param>"AComponent" a changed component object.</summary>
    /// <param>"Operation" a component operation code.</summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure InternalPost; override;
  public
    destructor Destroy; override;
  published
    property Sequence: TZSequence read FSequence write FSequence;
    property SequenceField: string read FSequenceField write FSequenceField;
    property UpdateTransaction: TZAbstractTransaction read FUpdateTransaction
      write SetUpdateTransaction;
    property Transaction;
  end;

  TZAbstractRWTxnUpdateObjDataSet = Class(TZAbstractRWTxnSeqDataSet)
  private
    FUpdateObject: TZUpdateSQL;
    /// <summary>Sets a new UpdateSQL object.</summary>
    /// <param>"Value" a new TZUpdateSQL object.</summary>
    procedure SetUpdateObject(Value: TZUpdateSQL);
  protected
    /// <summary>Processes component notifications.</summary>
    /// <param>"AComponent" a changed component object.</summary>
    /// <param>"Operation" a component operation code.</summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    /// <summary>Creates a DBC resultset for the query.</summary>
    /// <param>"SQL" an SQL query.</param>
    /// <param>"MaxRows" a maximum rows number (-1 for all).</param>
    /// <returns>a created DBC resultset.</returns>
    function CreateResultSet(const SQL: string; MaxRows: Integer):
      IZResultSet; override;
  public
    destructor Destroy; override;
  published
    property UpdateObject: TZUpdateSQL read FUpdateObject write SetUpdateObject;
  End;
  //EH: left for compatibility
  TZAbstractDataset = TZAbstractRWDataSet; 


implementation

uses Math, ZMessages, ZDatasetUtils, ZDbcProperties;

{ TZAbstractRWDataSet }

{**
  Constructs this object and assignes the mail properties.
  @param AOwner a component owner.
}
constructor TZAbstractRWDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWhereMode := wmWhereKeyOnly;
  FUpdateMode := umUpdateChanged;
  RequestLive := True;
  FDetailDataSets := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractRWDataSet.Destroy;
begin
  AfterCancel := nil;
  BeforeCancel := nil;
  if State in [dsEdit, dsInsert]
  then Cancel;

  FreeAndNil(FDetailDataSets);
  inherited Destroy;
end;

type
  HackTZAbstractTransaction = class(TZAbstractTransaction);

procedure TZAbstractRWDataSet.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then begin
    FCachedUpdates := Value;
    if Active and (CachedResultSet <> nil) then
      CachedResultSet.SetCachedUpdates(Value);
  end;
end;

procedure TZAbstractRWDataSet.SetTxns2Resolver(const Resolver: IZCachedResolver);
begin
  if Resolver = nil then Exit;
  if FTransaction <> nil then
    Resolver.SetTransaction(HackTZAbstractTransaction(FTransaction).GetIZTransaction);
end;

{**
  Sets a new UpdateMode property value.
  @param Value a new UpdateMode value.
}
procedure TZAbstractRWDataSet.SetUpdateMode(Value: TZUpdateMode);
begin
  if FUpdateMode <> Value then
  begin
    FUpdateMode := Value;
    if FGenDMLResolver <> nil then
      FGenDMLResolver.SetUpdateAll(FUpdateMode = umUpdateAll);
    {if Active then
      Close;}
  end;
end;

{**
  Sets a new WhereMode property value.
  @param Value a new WhereMode value.
}
procedure TZAbstractRWDataSet.SetWhereMode(Value: TZWhereMode);
begin
  if FWhereMode <> Value then begin
    FWhereMode := Value;
    if FGenDMLResolver <> nil then
      FGenDMLResolver.SetWhereAll(FWhereMode = wmWhereAll);
    {if Active then
      Close;}
  end;
end;

function TZAbstractRWDataSet.CreateResultSet(const SQL: string; MaxRows: Integer):
  IZResultSet;
begin
  Result := inherited CreateResultSet(SQL, MaxRows);

  if not Assigned(Result) then
    Exit;

  if Result.QueryInterface(IZCachedResultSet, FCachedResultSet) = 0 then begin
    FCachedResultSet := Result as IZCachedResultSet;
    FCachedResolver := CachedResultSet.GetResolver;
    FCachedResultSet.SetCachedUpdates(CachedUpdates);
    if (FCachedResolver <> nil) and (FCachedResolver.QueryInterface(IZGenerateSQLCachedResolver, FGenDMLResolver) = S_OK) then begin
      FGenDMLResolver.SetUpdateAll(FUpdateMode = umUpdateAll);
      FGenDMLResolver.SetWhereAll(FWhereMode = wmWhereAll);
      FGenDMLResolver.SetCalcDefaults(doCalcDefaults in Options);
    end;
  end;
end;

{$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}
function TZAbstractRWDataSet.GetUpdatable: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{**
  Performs internal query closing.
}
procedure TZAbstractRWDataSet.InternalClose;
begin
  inherited InternalClose;

  if not ResultSetWalking then begin
    {if Assigned(CachedResultSet) then begin
      CachedResultSet.Close;
      CachedResultSet := nil;
    end;}
    FCachedResolver := nil;
    FGenDMLResolver := nil;
  end;
end;

{**
  Performs an internal action before switch into edit mode.
}
procedure TZAbstractRWDataSet.InternalEdit;
var
  RowNo: NativeInt;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then begin
    RowNo := {%H-}NativeInt (CurrentRows[CurrentRow - 1]);
    CachedResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := RowBuffer;
  end;
end;

{**
  Performs an internal action before switch into insert mode.
}
procedure TZAbstractRWDataSet.InternalInsert;
{$IFDEF HAVE_INSERT_BOOKMARK_BUG}
var RowBuffer: PZRowBuffer;
{$ENDIF}
begin
  ResultSet.MoveToInsertRow;
  //EH: the FPC does not set bfInserted if we Insert the row
  {$IFDEF HAVE_INSERT_BOOKMARK_BUG}
  if GetActiveBuffer(RowBuffer) then
     RowBuffer.BookmarkFlag := Byte(bfInserted)
  {$ENDIF}
  //on append the FPC initializes the bookmark correctly afterwards}
end;

{**
  Performs an internal record updates.
}
procedure TZAbstractRWDataSet.InternalUnPrepare;
begin
  if Assigned(CachedResultSet) then begin
    CachedResultSet.Close;
    CachedResultSet := nil;
  end;
  inherited InternalUnPrepare;

end;

procedure TZAbstractRWDataSet.InternalUpdate;
var
  RowNo: NativeInt;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then begin
    RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
    try
      CachedResultSet.UpdateRow;
    except on E: Exception do
      if E is EZSQLThrowable
      then raise EZDatabaseError.CreateFromException(E as EZSQLThrowable)
      else raise Exception.Create(E.Message);
    end;

    { Filters the row }
    if not FilterRow(RowNo) then begin
      CurrentRows.Delete(CurrentRow - 1);
      CurrentRow := Min(CurrentRows.Count, CurrentRow);
    end;
  end;
end;

{**
  Performs an internal adding a new record.
  @param Buffer a buffer of the new adding record.
  @param Append <code>True</code> if record should be added to the end
    of the result set.
}
{$IFNDEF WITH_InternalAddRecord_TRecBuf}
procedure TZAbstractRWDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
{$ELSE}
procedure TZAbstractRWDataSet.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
{$ENDIF}
var
  RowNo: NativeInt;
  RowBuffer: PZRowBuffer;
begin
{$IFNDEF WITH_InternalAddRecord_TRecBuf}
  if not GetActiveBuffer(RowBuffer) or (RowBuffer <> Buffer) then
{$ELSE}
  if not GetActiveBuffer(RowBuffer) or (TRecBuf(RowBuffer) <> Buffer) then
{$ENDIF}
    raise EZDatabaseError.Create(SInternalError);

  if Append then
    FetchRows(0);

  if CachedResultSet <> nil then begin
    RowAccessor.RowBuffer := RowBuffer;
    CachedResultSet.InsertRow;
    RowNo := CachedResultSet.GetRow;
    FetchCount := FetchCount + 1;

    { Filters the row }
    if FilterRow(RowNo) then
      if Append then begin
        CurrentRows.Add({%H-}Pointer(RowNo));
        CurrentRow := CurrentRows.Count;
      end else begin
        CurrentRow := Max(CurrentRow, 1);
        CurrentRows.Insert(CurrentRow - 1, {%H-}Pointer(RowNo));
      end;
  end;
end;

{**
  Performs an internal post updates.
}
procedure TZAbstractRWDataSet.InternalPost;
var
  RowBuffer: PZRowBuffer;
  {$IFDEF WITH_TBOOKMARK}
  BM: TBookMark;
  {$ELSE}
  BM:TBookMarkStr{%H-};
  {$ENDIF}
  I, j: Integer;
begin
  //inherited;  //AVZ - Firebird defaults come through when this is commented out


  if not GetActiveBuffer(RowBuffer) then
    raise EZDatabaseError.Create(SInternalError);
  if Connection <> nil then
    Connection.ShowSqlHourGlass;
  try
    //revert Master Detail updates makes it possible to update
    // with ForeignKey contraints
    if Assigned(MasterLink.DataSet) then
      if (TDataSet(MasterLink.DataSet) is TZAbstractRWDataSet) then
        if ( doUpdateMasterFirst in TZAbstractRWDataSet(MasterLink.DataSet).Options )
         or ( doUpdateMasterFirst in Options ) then
        begin //This is an detail-table
          FCachedUpdatesBeforeMasterUpdate := CachedUpdates; //buffer old value
          if not(CachedUpdates) then
            CachedUpdates := True; //Execute without writing
          TZAbstractRWDataSet(MasterLink.DataSet).RegisterDetailDataSet(Self,
            TZAbstractRWDataSet(MasterLink.DataSet).CachedUpdates);
        end;

    if FGenDMLResolver <> nil then
      for i := 0 to Fields.Count -1 do
        if not (pfInUpdate in Fields[i].ProviderFlags) or not (pfInWhere in Fields[i].ProviderFlags) then
          for j := 0 to high(FieldsLookupTable) do
            if (FieldsLookupTable[j].Field = Fields[i]) and (FieldsLookupTable[j].DataSource = dltResultSet) then begin
              FGenDMLResolver.SetReadOnly(FieldsLookupTable[j].Index, Fields[i].ReadOnly or not (pfInUpdate in Fields[i].ProviderFlags));
              FGenDMLResolver.SetSearchable(FieldsLookupTable[j].Index, (pfInWhere in Fields[i].ProviderFlags));
            end;
    if State = dsInsert then
      {$IFNDEF WITH_InternalAddRecord_TRecBuf}
      InternalAddRecord(RowBuffer, False)
      {$ELSE}
      InternalAddRecord(TRecBuf(RowBuffer), False)
      {$ENDIF}
    else
      InternalUpdate;

    // Apply Detail updates now
    if FDetailDataSets.Count > 0 then
      for i := 0 to FDetailDataSets.Count -1 do
        if (TDataSet(FDetailDataSets.Items[i]) is TZAbstractRWDataSet) then
          begin
            if not (Self.FDetailCachedUpdates[I]) then
              TZAbstractRWDataSet(TDataSet(FDetailDataSets.Items[i])).ApplyUpdates;
            TZAbstractRWDataSet(TDataSet(FDetailDataSets.Items[i])).CachedUpdates := Self.FDetailCachedUpdates[I];
          end;
    FDetailDataSets.Clear;
    SetLength(FDetailCachedUpdates, 0);

    {BUG-FIX: bangfauzan addition}
    if (SortedFields <> '') and not (doDontSortOnPost in Options) then
    begin
      FreeFieldBuffers;
      SetState(dsBrowse);
      Resync([]);
      BM := Bookmark;
      if BookmarkValid({$IFDEF WITH_TBOOKMARK}BM{$ELSE}@BM{$ENDIF}) Then
      begin
        InternalGotoBookmark({$IFDEF WITH_TBOOKMARK}BM{$ELSE}@BM{$ENDIF});
        Resync([rmExact, rmCenter]);
      end;
      DisableControls;
      InternalSort;
      BookMark:=BM;
      UpdateCursorPos;
      EnableControls;
    end;
    {end of bangfauzan addition}
  finally
    if Connection <> nil then
      Connection.HideSqlHourGlass;
    //DetailLinks.Free;
  end;
end;

{**
  Performs an internal record removing.
}
procedure TZAbstractRWDataSet.InternalDelete;
var
  RowNo: NativeInt;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then begin
    Connection.ShowSqlHourGlass;
    try
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
      CachedResultSet.MoveAbsolute(RowNo);
      try
        CachedResultSet.DeleteRow;
      except on E: Exception do
        if E is EZSQLThrowable
        then raise EZDatabaseError.CreateFromException(E as EZSQLThrowable)
        else raise Exception.Create(E.Message);
      end;

      { Filters the row }
      if not FilterRow(RowNo) then begin
        CurrentRows.Delete(CurrentRow - 1);
        if not FetchRows(CurrentRow) then
          CurrentRow := Min(CurrentRows.Count, CurrentRow);
      end;
    finally
      Connection.HideSQLHourGlass;
    end;
  end;
end;

{**
  Performs an internal cancel updates.
}
procedure TZAbstractRWDataSet.InternalCancel;
var
  RowNo: NativeInt;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then
    if (CurrentRow > 0) and (State = dsEdit) then begin
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
      CachedResultSet.MoveAbsolute(RowNo);
      RowAccessor.RowBuffer := RowBuffer;
      CachedResultSet.RevertRecord;
    end
    else if (State = dsInsert) then
      CachedResultSet.RevertRecord;
end;

{**
   Applies all cached updates stored in the resultset.
}
procedure TZAbstractRWDataSet.ApplyUpdates;
begin
  if not Active then
    Exit;

  Connection.ShowSQLHourGlass;
  try
    if State in [dsEdit, dsInsert] then
       Post;

    DoBeforeApplyUpdates; {bangfauzan addition}

    if CachedResultSet <> nil then
      if Connection.AutoCommit and
        not ( Connection.TransactIsolationLevel in [tiReadCommitted, tiSerializable] ) then
        CachedResultSet.PostUpdates
      else
        CachedResultSet.PostUpdatesCached;
    UpdateCursorPos;
    if not (State in [dsInactive]) then
      Resync([]);

  DOAfterApplyUpdates; {bangfauzan addition}

  finally
    Connection.HideSqlHourGlass;
  end;
end;

{**
   Dispose all cached updates stored in the resultset.
}
procedure TZAbstractRWDataSet.DisposeCachedUpdates;
begin
  if Active then
    if Assigned(CachedResultSet) then
      CachedResultSet.DisposeCachedUpdates;
end;

{**
  Clears cached updates buffer.
}
procedure TZAbstractRWDataSet.CommitUpdates;
begin
  CheckBrowseMode;
  if (CachedResultSet <> nil) and CachedResultSet.IsPendingUpdates then
    CachedResultSet.DisposeCachedUpdates;
end;

{**
  Cancels all cached updates and clears the buffer.
}
procedure TZAbstractRWDataSet.CancelUpdates;
begin
  if State in [dsEdit, dsInsert] then
    Cancel;
  if CachedResultSet <> nil then
    CachedResultSet.CancelUpdates;
  if not (State in [dsInactive]) then
    RereadRows;
end;

{**
  Reverts the previous status for the current row.
}
procedure TZAbstractRWDataSet.RefreshCurrentRow(const RefreshDetails:Boolean);
var RowNo: NativeInt;
    i: Integer;
    ostate: TDataSetState;
begin
  if State=dsBrowse then begin
    if CachedResultSet <> nil then begin
      UpdateCursorPos;
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
      CachedResultSet.MoveAbsolute(RowNo);
      CachedResultSet.RefreshRow;
      if not (State in [dsInactive]) then
      begin
        if RefreshDetails then
          Resync([])
        else begin
          ostate:=State;
          SetTempState(dsInternalCalc);
          try
            for I := 0 to Fields.Count - 1 do
              DataEvent(deFieldChange, NativeInt(Fields[i]));
          finally
            RestoreState(ostate);
          end;
        end;
      end;
    end;
  end else
    raise EZDatabaseError.Create(SInternalError);
end;


procedure TZAbstractRWDataSet.RevertRecord;
begin
  if State in [dsInsert] then
  begin
    Cancel;
    Exit;
  end;
  if State in [dsEdit] then
    Cancel;

  if CachedResultSet <> nil then
    CachedResultSet.RevertRecord;

  if not (State in [dsInactive]) then
    Resync([]);
end;

{**
  Checks is there cached updates pending in the buffer.
  @return <code>True</code> if there some pending cached updates.
}
function TZAbstractRWDataSet.GetUpdatesPending: Boolean;
begin
  if State = dsInactive then
    Result := False
  else if (CachedResultSet <> nil) and CachedResultSet.IsPendingUpdates then
    Result := True
  else if (State in [dsInsert, dsEdit]) then
    Result := Modified
  else
    Result := False;
end;

{$IFDEF WITH_IPROVIDER}

{**
  Applies a single update to the underlying database table or tables.
  @param UpdateKind an update type.
  @param Delta a dataset where the current position shows the row to update.
  @returns <code>True</code> if updates were successfully applied.
}
function TZAbstractRWDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;

var
  Bookmark: TBookmark;
  ActiveMode: Boolean;
  UpdateMode: Boolean;

  function LocateRecord: Boolean;
  var
    I: Integer;
    KeyFields: string;
    Temp: Variant;
    SrcField: TField;
    KeyValues: Variant;
    FieldRefs: TZFieldsLookUpDynArray;
    OnlyDataFields: Boolean;
  begin
    if Properties.Values[DSProps_KeyFields] <> '' then
      KeyFields := Properties.Values[DSProps_KeyFields]
    else
      KeyFields := DefineKeyFields(Fields, Connection.DbcConnection.GetMetadata.GetIdentifierConverter);
    FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields,
      Connection.DbcConnection.GetDriver.GetTokenizer);
    Temp := VarArrayCreate([0, Length(FieldRefs) - 1], varVariant);

    for I := 0 to Length(FieldRefs) - 1 do
    begin
      SrcField := Delta.FieldByName(TField(FieldRefs[I].Field).FieldName);
      if SrcField <> nil
      then Temp[I] := SrcField.OldValue
      else Temp[I] := Null;
    end;

    if Length(FieldRefs) = 1 then
      KeyValues := Temp[0]
    else
      KeyValues := Temp;

    if KeyFields <> '' then
      Result := Locate(KeyFields, KeyValues, [])
    else
      Result := False;
  end;

  procedure CopyRecord(SrcDataset: TDataset; DestDataset: TDataset);
  var
    I: Integer;
    SrcField: TField;
    DestField: TField;
    SrcStream: TStream;
    DestStream: TStream;
  begin
    for I := 0 to DestDataset.FieldCount - 1 do
    begin
      DestField := DestDataset.Fields[I];
      SrcField := SrcDataset.FieldByName(DestField.FieldName);
      if (SrcField = nil) or VarIsEmpty(SrcField.NewValue) then
        Continue;

      if SrcField.IsNull then
      begin
        DestField.Clear;
        Continue;
      end;

      case DestField.DataType of
        ftLargeInt:
            if SrcField.DataType = ftLargeInt
            then TLargeIntField(DestField).AsLargeInt := TLargeIntField(SrcField).AsLargeInt
            else DestField.AsInteger := SrcField.AsInteger;
        ftBlob, ftMemo {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
          if SrcField.DataType in [ftBlob, ftMemo {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}] then begin
            SrcStream := SrcDataset.CreateBlobStream(SrcField, bmRead);
            try
              DestStream := DestDataset.CreateBlobStream(DestField, bmWrite);
              try
                DestStream.CopyFrom(SrcStream, 0);
              finally
                DestStream.Free;
              end;
            finally
              SrcStream.Free;
            end;
          end else
            DestField.AsVariant := SrcField.AsVariant;
        else
          DestField.AsVariant := SrcField.AsVariant;
      end;
    end;
  end;

begin
  Result := False;
  ActiveMode := Self.Active;
  UpdateMode := Self.RequestLive;

  if Self.RequestLive = False then
    Self.RequestLive := True;
  if Self.Active = False then
    Self.Open;

  CheckBrowseMode;
  try
    Self.DisableControls;

    { Saves the current position. }
    Bookmark := Self.GetBookmark;

    { Applies updates. }
    try
      case UpdateKind of
        ukModify: if LocateRecord then begin
              Self.Edit;
              CopyRecord(Delta, Self);
              Self.Post;
              Result := True;
            end;
        ukInsert: begin
            Self.Append;
            CopyRecord(Delta, Self);
            Self.Post;
            Result := True;
          end;
        ukDelete: if LocateRecord then begin
              Self.Delete;
              Result := True;
            end;
      end;
    except
      Result := False;
    end;

    { Restores the previous position. }
    try
      Self.GotoBookmark(Bookmark);
    except
      Self.First;
    end;
    Self.FreeBookmark(Bookmark);
  finally
    EnableControls;
    Self.RequestLive := UpdateMode;
    Self.Active := ActiveMode;
  end;
end;

{$ENDIF}

procedure TZAbstractRWDataSet.RegisterDetailDataSet(Value: TZAbstractRWDataSet;
  CachedUpdates: Boolean);
begin
  FDetailDataSets.Add(Value);
  SetLength(Self.FDetailCachedUpdates, Length(FDetailCachedUpdates)+1);
  FDetailCachedUpdates[High(FDetailCachedUpdates)] := CachedUpdates;
end;

{============================bangfauzan addition===================}

procedure TZAbstractRWDataSet.DOBeforeApplyUpdates;
begin
  if assigned(BeforeApplyUpdates) then
    FBeforeApplyUpdates(Self);
end;

procedure TZAbstractRWDataSet.DOAfterApplyUpdates;
begin
  if assigned(AfterApplyUpdates) then
    FAfterApplyUpdates(Self);
end;

procedure TZAbstractRWDataSet.EmptyDataSet;
begin
  if Active then
  begin
    Self.CancelUpdates;
    Self.CurrentRows.Clear;
    Self.CurrentRow:=0;
    CachedResultSet.ResetCursor;
    Resync([]);
    InitRecord(ActiveBuffer);
  end;
end;

{========================end of bangfauzan addition================}

{ TZAbstractRWTxnSeqDataSet }

destructor TZAbstractRWTxnSeqDataSet.Destroy;
begin
  AfterCancel := nil;
  BeforeCancel := nil;
  if State in [dsEdit, dsInsert] then
    Cancel;
  if Assigned(FSequence) then
    FSequence := nil;
  if (FUpdateTransaction <> nil) then
    SetUpdateTransaction(nil); //unregister
  inherited Destroy;
end;

procedure TZAbstractRWTxnSeqDataSet.InternalPost;
begin
  if (FSequenceField <> '') and Assigned(FSequence) then
    if FieldByName(FSequenceField).IsNull then
      FieldByName(FSequenceField).Value := FSequence.GetNextValue;
  inherited InternalPost;

end;

procedure TZAbstractRWTxnSeqDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = FUpdateTransaction) then
      SetUpdateTransaction(nil);
    if (AComponent = FSequence) then
      FSequence := nil;
  end;
end;

procedure TZAbstractRWTxnSeqDataSet.SetTransaction(Value: TZAbstractTransaction);
begin
  if Value <> FTransaction then begin
    if (FTransaction <> nil) and (FTransaction <> FUpdateTransaction) then
      FTransaction.UnregisterComponent(Self);
    FTransaction := Value;
    if FTransaction <> nil then
      FTransaction.RegisterComponent(Self);
  end;
end;

procedure TZAbstractRWTxnSeqDataSet.SetTxns2Resolver(
  const Resolver: IZCachedResolver);
begin
  if Resolver = nil then Exit;
  if FUpdateTransaction = nil
  then inherited SetTxns2Resolver(Resolver)
  else Resolver.SetTransaction(HackTZAbstractTransaction(FUpdateTransaction).GetIZTransaction);
end;

procedure TZAbstractRWTxnSeqDataSet.SetUpdateTransaction(
  Value: TZAbstractTransaction);
var Txn: IZTransaction;
begin
  if Value <> FUpdateTransaction then begin
    if (FTransaction <> nil) and (FTransaction <> FUpdateTransaction) and (FUpdateTransaction <> nil) then
      FUpdateTransaction.UnregisterComponent(Self);
    FUpdateTransaction := Value;
    if FUpdateTransaction <> nil then
      FUpdateTransaction.RegisterComponent(Self);
    if (Value<> nil) and Value.Active
    then Txn := HackTZAbstractTransaction(Value).GetIZTransaction
    else Txn := nil;
    if CachedResolver <> nil then
      CachedResolver.SetTransaction(Txn);
  end;
end;

{ TZAbstractRWTxnUpdateObjDataSet }

function TZAbstractRWTxnUpdateObjDataSet.CreateResultSet(const SQL: string;
  MaxRows: Integer): IZResultSet;
begin
  Result := inherited CreateResultSet(SQL, MaxRows);
  if (CachedResultSet <> nil) and (FUpdateObject <> nil) then begin
    CachedResultSet.SetResolver(FUpdateObject);
    SetTxns2Resolver(FUpdateObject);
  end else SetTxns2Resolver(CachedResolver);
end;

destructor TZAbstractRWTxnUpdateObjDataSet.Destroy;
begin
  AfterCancel := nil;
  BeforeCancel := nil;
  if State in [dsEdit, dsInsert] then
    Cancel;
  if Assigned(FUpdateObject) then begin
    FUpdateObject.DataSet := nil;
    SetUpdateObject(nil);
  end;
  inherited Destroy;
end;

procedure TZAbstractRWTxnUpdateObjDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = FUpdateObject) then begin
      Close;
      SetUpdateObject(nil);
    end;
    if (AComponent = FSequence) then
      FSequence := nil;
  end;
end;

procedure TZAbstractRWTxnUpdateObjDataSet.SetUpdateObject(Value: TZUpdateSQL);
var TempResolver: IZCachedResolver; //need a temporay interface to compare the resolvers
begin
  if FUpdateObject <> Value then begin
    TempResolver := nil; //init
    if Assigned(FUpdateObject) then begin
      FUpdateObject.RemoveFreeNotification(Self);
      { get a local interface ptr of old update object for comparesion below }
      FUpdateObject.GetInterface(IZCachedResolver, TempResolver);
    end;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then begin
      FUpdateObject.FreeNotification(Self);
      FUpdateObject.DataSet := Self;
    end;
    if Active and (CachedResultSet <> nil) then
      if FUpdateObject <> nil then begin
        { get a local interface of the component }
        FUpdateObject.GetInterface(IZCachedResolver, TempResolver);
        CachedResultSet.SetResolver(TempResolver);
        SetTxns2Resolver(TempResolver);
        FGenDMLResolver := nil;
      end else begin
        {EH: now test if the old FUpdateObject intf equals with current cached resolver }
        if FCachedResolver = TempResolver then
          { do not use this interface any more. Use the native resolver of
            the cached RS instead. Otherwise on freeing (self) the compiler
            attaches dead memory later on (this is hidded with FastMM in our tests f.e.)
            -> Component interfaces are not refcounted by default}
          FCachedResolver := CachedResultSet.GetNativeResolver;
        CachedResultSet.SetResolver(FCachedResolver);
        FCachedResolver.QueryInterface(IZGenerateSQLCachedResolver, FGenDMLResolver);
        SetTxns2Resolver(FCachedResolver);
      end;
  end;
end;

end.

