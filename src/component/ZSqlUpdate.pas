{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Unidatabase UpdateSQL component             }
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

{
  constributor(s):
  EgonHugeist
  fduenas
  FOS
}

unit ZSqlUpdate;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  {$IFDEF TEST_TZPARAM}ZDatasetParam,{$ENDIF}
  ZDbcIntfs, ZDbcCachedResultSet, ZDbcCache, ZSqlStrings, ZClasses;

type
  TZBeforeSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer; out Execute: Boolean ) of object;

  TZAfterSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer) of object;

  TZAfterInsertSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer; out UpdateAutoIncFields: Boolean ) of object;

  {**
    Implements an object which manages SQL DML statements to update TDatasets.
  }
  TZUpdateSQL = class(TComponent, IZCachedResolver)
  private
    FTransaction: IZTransaction;
    FDataSet: TDataSet;

    FDeleteSQL: TZSQLStrings;
    FInsertSQL: TZSQLStrings;
    FModifySQL: TZSQLStrings;
    FRefreshSQL: TZSQLStrings;
    FStmts: Array[utModified..utDeleted] of IZCollection;
    FRefreshRS: IZResultSet;
    FRefreshStmt: IZPreparedStatement;

    FParamCheck: Boolean;
    FParams: {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF};
    FMultiStatements: Boolean;
    FBeforeDeleteSQL: TNotifyEvent;
    FBeforeInsertSQL: TNotifyEvent;
    FBeforeModifySQL: TNotifyEvent;
    FAfterDeleteSQL: TNotifyEvent;
    FAfterInsertSQL: TNotifyEvent;
    FAfterModifySQL: TNotifyEvent;
    FUseSequenceFieldForRefreshSQL: Boolean;
    FBeforeDeleteSQLStatement: TZBeforeSQLStatementEvent;
    FAfterDeleteSQLStatement: TZAfterSQLStatementEvent;
    FBeforeInsertSQLStatement: TZBeforeSQLStatementEvent;
    FAfterInsertSQLStatement: TZAfterInsertSQLStatementEvent;
    FBeforeModifySQLStatement: TZBeforeSQLStatementEvent;
    FAfterModifySQLStatement: TZAfterSQLStatementEvent;

    procedure SetUseSequenceFieldForRefreshSQL(const Value: Boolean);
    procedure SetDataset(Value: TDataset);
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    function GetParamsCount: Word;
    procedure SetParamsList(Value: {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF});
    procedure SetParamCheck(Value: Boolean);
    procedure SetMultiStatements(Value: Boolean);

    function GetDeleteSQL: TStrings;
    procedure SetDeleteSQL(Value: TStrings);
    function GetInsertSQL: TStrings;
    procedure SetInsertSQL(Value: TStrings);
    function GetModifySQL: TStrings;
    procedure SetModifySQL(Value: TStrings);

    function GetRefreshSQL: TStrings;
    procedure SetRefreshSQL(Value: TStrings);

    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);


  protected
    procedure Apply_RefreshResultSet(const Sender: IZCachedResultSet;
      const RefreshResultSet: IZResultSet; const RefreshRowAccessor: TZRowAccessor);

    procedure DefineProperties(Filer: TFiler); override;

    procedure SetTransaction(const Value: IZTransaction);
    function HasAutoCommitTransaction: Boolean;
    procedure CalculateDefaults(const Sender: IZCachedResultSet;
      const RowAccessor: TZRowAccessor);
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      Const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);

    procedure RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);

    procedure Rebuild(SQLStrings: TZSQLStrings);
    procedure RebuildAll;
    procedure FillStatement(const ResultSet: IZCachedResultSet;
      const Statement: IZPreparedStatement; Config: TZSQLStatement;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure UpdateParams({%H-}Sender: TObject);

    procedure DoBeforeDeleteSQL;
    procedure DoBeforeInsertSQL;
    procedure DoBeforeModifySQL;
    procedure DoAfterDeleteSQL;
    procedure DoAfterInsertSQL;
    procedure DoAfterModifySQL;

    procedure DoBeforeDeleteSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoBeforeInsertSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoBeforeModifySQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoAfterDeleteSQLStatement(const Sender: TObject;
      StatementIndex: Integer);
    procedure DoAfterInsertSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out UpdateAutoIncFields: Boolean) ;
    procedure DoAfterModifySQLStatement(const Sender: TObject;
      StatementIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
    property ParamCount: Word read GetParamsCount;
    property DataSet: TDataSet read FDataSet write SetDataSet;

  published
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    property RefreshSQL: TStrings read GetRefreshSQL write SetRefreshSQL;
    property UseSequenceFieldForRefreshSQL:Boolean read FUseSequenceFieldForRefreshSQL write SetUseSequenceFieldForRefreshSQL;


    property Params: {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF} read FParams write SetParamsList stored False;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck default True;
    property MultiStatements: Boolean read FMultiStatements write SetMultiStatements default True;

    property BeforeDeleteSQL: TNotifyEvent
      read FBeforeDeleteSQL write FBeforeDeleteSQL;
    property BeforeInsertSQL: TNotifyEvent
      read FBeforeInsertSQL write FBeforeInsertSQL;
    property BeforeModifySQL: TNotifyEvent
      read FBeforeModifySQL write FBeforeModifySQL;
    property AfterDeleteSQL: TNotifyEvent
      read FAfterDeleteSQL write FAfterDeleteSQL;
    property AfterInsertSQL: TNotifyEvent
      read FAfterInsertSQL write FAfterInsertSQL;
    property AfterModifySQL: TNotifyEvent
      read FAfterModifySQL write FAfterModifySQL;

    {New Events Fired by executed Statement}
    property BeforeDeleteSQLStatement: TZBeforeSQLStatementEvent
      read FBeforeDeleteSQLStatement write FBeforeDeleteSQLStatement;
    property BeforeInsertSQLStatement: TZBeforeSQLStatementEvent
      read FBeforeInsertSQLStatement write FBeforeInsertSQLStatement;
    property BeforeModifySQLStatement: TZBeforeSQLStatementEvent
      read FBeforeModifySQLStatement write FBeforeModifySQLStatement;
    property AfterDeleteSQLStatement: TZAfterSQLStatementEvent
      read FAfterDeleteSQLStatement write FAfterDeleteSQLStatement;
    property AfterInsertSQLStatement: TZAfterInsertSQLStatementEvent
      read FAfterInsertSQLStatement write FAfterInsertSQLStatement;
    property AfterModifySQLStatement: TZAfterSQLStatementEvent
      read FAfterModifySQLStatement write FAfterModifySQLStatement;
  end;

implementation

uses FmtBCD,
  ZGenericSqlToken, ZDatasetUtils, ZAbstractRODataset, ZAbstractDataset,
  ZSysUtils, ZDbcUtils, ZMessages, ZCompatibility, ZDbcProperties, ZCollections,
  ZEncoding;

{ TZUpdateSQL }

{**
  Constructs this object and assignes main properties.
  @param AOwner a component owner.
}
constructor TZUpdateSQL.Create(AOwner: TComponent);
var RowUpdateType: TZRowUpdateType;
begin
  inherited Create(AOwner);

  FDeleteSQL := TZSQLStrings.Create;
  FDeleteSQL.OnChange := UpdateParams;
  FInsertSQL := TZSQLStrings.Create;
  FInsertSQL.OnChange := UpdateParams;
  FModifySQL := TZSQLStrings.Create;
  FModifySQL.OnChange := UpdateParams;

  FRefreshSQL := TZSQLStrings.Create;
  FRefreshSQL.OnChange:= UpdateParams;

  FParams := {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF}.Create(Self);
  FParamCheck := True;
  FMultiStatements := True;
  for RowUpdateType := utModified to utDeleted do
    FStmts[RowUpdateType] := TZCollection.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZUpdateSQL.Destroy;
begin
  FParams.Free;
  FDeleteSQL.Free;
  FInsertSQL.Free;
  FModifySQL.Free;
  FRefreshSQL.Free;
  {keep track we notify a possible opened DataSet.CachedResultSet about destruction
   else IntfAssign of FPC fails to clear the cached resolver of the CachedResultSet}
  if Assigned(FDataSet) and (FDataSet is TZAbstractDataset) then
    TZAbstractDataset(DataSet).UpdateObject := nil;
  inherited Destroy;
end;

{**
  Store the related dataset object for update sql editor
}
procedure TZUpdateSQL.SetDataset(Value: TDataset);
begin
  FDataSet := Value;
  FDeleteSQL.Dataset := Value;
  FInsertSQL.Dataset := Value;
  FModifySQL.Dataset := Value;
end;

{**
  Gets a DML statements for specified action.
  @param UpdateKind a type of the DML statements.
  @return a stored DML statement.
}
function TZUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  case UpdateKind of
    ukModify: Result := FModifySQL;
    ukInsert: Result := FInsertSQL;
  else
    Result := FDeleteSQL;
  end;
end;

function TZUpdateSQL.HasAutoCommitTransaction: Boolean;
begin
  if (FTransaction <> nil)
  then Result := FTransaction.GetAutoCommit
  else Result := TZAbstractRODataset(Dataset).Connection.AutoCommit;
end;

{**
  Sets a DML statements for specified action.
  @param UpdateKind a type of the DML statements.
  @param Value a DML statements to be set.
}
procedure TZUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  case UpdateKind of
    ukModify: FModifySQL.Assign(Value);
    ukInsert: FInsertSQL.Assign(Value);
    ukDelete: FDeleteSQL.Assign(Value);
  end;
end;

{**
  Get parameters count.
  @return a parameters count.
}
function TZUpdateSQL.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TZUpdateSQL.GetRefreshSQL: TStrings;
begin
  Result := FRefreshSQL;
end;

{**
  Sets parameters checking flag.
  @param Value a new parameters checking flag.
}
procedure TZUpdateSQL.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then begin
    FParamCheck := Value;
    FModifySQL.ParamCheck := Value;
    FInsertSQL.ParamCheck := Value;
    FDeleteSQL.ParamCheck := Value;
    RebuildAll;
  end;
end;

{**
  Sets multiple statements flag.
  @param Value a new multiple statements flag.
}
procedure TZUpdateSQL.SetMultiStatements(Value: Boolean);
begin
  if FMultiStatements <> Value then begin
    FMultiStatements := Value;
    FModifySQL.MultiStatements := Value;
    FInsertSQL.MultiStatements := Value;
    FDeleteSQL.MultiStatements := Value;
    RebuildAll;
  end;
end;

{**
  Set a new list of SQL parameters.
  @param Value a new list of SQL parameters.
}
procedure TZUpdateSQL.SetParamsList(Value: {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF});
begin
  FParams.AssignValues(Value);
end;

procedure TZUpdateSQL.SetTransaction(const Value: IZTransaction);
var Stmt: IZStatement;
  ut: TZRowUpdateType;
begin
  if FTransaction <> Value then begin
    Stmt := nil;
    for ut := utModified to utDeleted do
      if (FTransaction <> nil) and
         (FStmts[ut].Count > 0) and (FStmts[ut][0] <> nil) and
         (FStmts[ut][0].QueryInterface(IZStatement, Stmt) = S_OK) and
         (Stmt.GetConnection <> FTransaction.GetConnection)
      then Break
      else Stmt := nil;
    if (Stmt <> nil) then
      for ut := utModified to utDeleted do
        FStmts[ut].Clear;
  end;
end;

procedure TZUpdateSQL.SetRefreshSQL(Value: TStrings);
begin
  FRefreshSQL.Assign(Value);
  if (FRefreshRS <> nil) then begin
    FRefreshRS.Close;
    FRefreshRS := nil;
  end;
  FRefreshStmt := nil;
end;

procedure TZUpdateSQL.SetUseSequenceFieldForRefreshSQL(const Value: Boolean);
begin
  FUseSequenceFieldForRefreshSQL := Value;
end;

{**
  Defines a persistent dataset properties.
  @param Filer a persistent manager object.
}
procedure TZUpdateSQL.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZUpdateSQL(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{**
  Reads parameter data from persistent storage.
  @param Reader an input data stream.
}
procedure TZUpdateSQL.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{**
  Writes parameter data from persistent storage.
  @param Writer an output data stream.
}
procedure TZUpdateSQL.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{**
  Gets strings with Delete statements.
  @return strings with Delete statements.
}
function TZUpdateSQL.GetDeleteSQL: TStrings;
begin
  Result := FDeleteSQL;
end;

{**
  Sets a new Delete SQL statement.
  @param Value a new Delete SQL statement.
}
procedure TZUpdateSQL.SetDeleteSQL(Value: TStrings);
begin
  FDeleteSQL.Assign(Value);
  FStmts[utDeleted].Clear;
end;

{**
  Gets strings with Insert statements.
  @return strings with Insert statements.
}
function TZUpdateSQL.GetInsertSQL: TStrings;
begin
  Result := FInsertSQL;
end;

{**
  Sets a new Insert SQL statement.
  @param Value a new Insert SQL statement.
}
procedure TZUpdateSQL.SetInsertSQL(Value: TStrings);
begin
  FInsertSQL.Assign(Value);
  FStmts[utInserted].Clear;
end;

{**
  Gets strings with Modify statements.
  @return strings with Modify statements.
}
function TZUpdateSQL.GetModifySQL: TStrings;
begin
  Result := FModifySQL;
end;

{**
  Sets a new  Modify SQL statement.
  @param Value a new Modify SQL statement.
}
procedure TZUpdateSQL.SetModifySQL(Value: TStrings);
begin
  FModifySQL.Assign(Value);
  FStmts[utModified].Clear;
end;

{**
  Updates all parameters.
  @param Sender an event sender object.
}
procedure TZUpdateSQL.UpdateParams(Sender: TObject);
begin
  RebuildAll;
end;

{**
  Rebuilds parameters and inserts a new one from specified sql statements.
  @param SQLStrings a strings with SQL statements.
}
procedure TZUpdateSQL.Rebuild(SQLStrings: TZSQLStrings);
var
  I: Integer;
begin
  for I := 0 to SQLStrings.ParamCount - 1 do
  begin
    if FParams.FindParam(SQLStrings.ParamNames[I]) = nil then
      FParams.CreateParam(ftUnknown, SQLStrings.ParamNames[I], ptUnknown);
  end;
end;

{**
  Rebuilds all internal structures including parameters from SQL statements.
}
procedure TZUpdateSQL.RebuildAll;
var
  OldParams: {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF};
begin
  OldParams := {$IFDEF TEST_TZPARAM}TZParams{$ELSE}TParams{$ENDIF}.Create;
  OldParams.Assign(FParams);
  FParams.Clear;
  try
    Rebuild(FModifySQL);
    Rebuild(FInsertSQL);
    Rebuild(FDeleteSQL);
//FOSPATCH
    Rebuild(FRefreshSQL);
//FOSPATCH
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

procedure TZUpdateSQL.RefreshCurrentRow(const Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
var
    Config: TZSQLStrings;
    Statement: IZPreparedStatement;
    RefreshResultSet: IZResultSet;
begin
 Config:=FRefreshSQL;
 if CONFIG.StatementCount=1 then
 begin
  Statement := Sender.GetStatement.GetConnection.PrepareStatement(Config.Statements[0].SQL);
  FillStatement(Sender, Statement, Config.Statements[0],RowAccessor, RowAccessor);
  RefreshResultSet:=Statement.ExecuteQueryPrepared;
  Apply_RefreshResultSet(Sender,RefreshResultSet,RowAccessor);
 end;
end;

{**
  Fills the specified statement with stored or given parameters.
  @param ResultSet a source result set object.
  @param Statement a DBC statement object.
  @param Config a SQLStatement configuration.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZUpdateSQL.FillStatement(const ResultSet: IZCachedResultSet;
  const Statement: IZPreparedStatement; Config: TZSQLStatement;
  OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I, ColumnIndex: Integer;
  ParamValue: {$IFDEF TEST_TZPARAM}TZParam{$ELSE}TParam{$ENDIF};
  ParamName: string;
  OldParam: Boolean;
  WasNull: Boolean;
  RowAccessor: TZRowAccessor;
  TempBlob: IZBlob;
  BCD: TBCD; //one val on stack 4 all
  UID: TGUID absolute BCD;
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
  Metadata: IZResultSetMetadata;
begin
  WasNull := False;
  Metadata := ResultSet.GetMetadata;
  for I := 0 to Config.ParamCount - 1 do begin
    ParamValue := Params.FindParam(Config.ParamNames[I]);
    ParamName := Config.ParamNames[I];
    OldParam := False;{Seqparam:=False;}
    if StrLIComp(PChar(ParamName), 'NEW_', 4) = 0 then
      ParamName := Copy(ParamName, 5, Length(ParamName) - 4)
    else if StrLIComp(PChar(ParamName), 'OLD_', 4) = 0 then begin
      ParamName := Copy(ParamName, 5, Length(ParamName) - 4);
      OldParam := True;
    end;

    ColumnIndex := ResultSet.FindColumn(ParamName);
    if ColumnIndex >= FirstDbcIndex then begin
      if OldParam
      then RowAccessor := OldRowAccessor
      else RowAccessor := NewRowAccessor;

      if RowAccessor.IsNull(ColumnIndex) then
        Statement.SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          Metadata.GetColumnType(ColumnIndex))
      else case Metadata.GetColumnType(ColumnIndex) of
        stBoolean:
          Statement.SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
            RowAccessor.GetBoolean(ColumnIndex, WasNull));
        stByte:
          Statement.SetByte(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetByte(ColumnIndex, WasNull));
        stShort:
          Statement.SetShort(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetShort(ColumnIndex, WasNull));
        stWord:
          Statement.SetWord(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetWord(ColumnIndex, WasNull));
        stSmall:
          Statement.SetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetSmall(ColumnIndex, WasNull));
        stLongWord:
          Statement.SetUInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetUInt(ColumnIndex, WasNull));
        stInteger:
          Statement.SetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetInt(ColumnIndex, WasNull));
        stULong:
          Statement.SetULong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetULong(ColumnIndex, WasNull));
        stLong:
          Statement.SetLong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetLong(ColumnIndex, WasNull));
        stFloat:
          Statement.SetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetFloat(ColumnIndex, WasNull));
        stDouble:
          Statement.SetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetDouble(ColumnIndex, WasNull));
        stCurrency:
          Statement.SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetCurrency(ColumnIndex, WasNull));
        stBigDecimal:
          begin
            RowAccessor.GetBigDecimal(ColumnIndex, BCD{%H-}, WasNull);
            Statement.SetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BCD);
          end;
        stGUID: begin
            RowAccessor.GetGUID(ColumnIndex, UID, WasNull);
            Statement.SetGUID(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, UID);
          end;
        stString, stUnicodeString:
          Statement.SetCharRec(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetCharRec(ColumnIndex, WasNull));
        stBytes:
          Statement.SetBytes(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetBytes(ColumnIndex, WasNull));
        stDate: begin
            RowAccessor.GetDate(ColumnIndex, WasNull, D);
            Statement.SetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, D);
          end;
        stTime: begin
            RowAccessor.GetTime(ColumnIndex, WasNull, T);
            Statement.SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
          end;
        stTimestamp: begin
            RowAccessor.GetTimestamp(ColumnIndex, WasNull, TS);
            Statement.SetTimestamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS);
          end;
        stAsciiStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stAsciiStream, TempBlob)
            else
              Statement.SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stAsciiStream);
          end;
        stUnicodeStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnicodeStream, TempBlob)
            else
              Statement.SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnicodeStream);
          end;
        stBinaryStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBinaryStream, TempBlob)
            else
              Statement.SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBinaryStream);
          end;
        else raise ZDbcUtils.CreateUnsupportedParameterTypeException(ColumnIndex,
             Metadata.GetColumnType(ColumnIndex));
      end;
      if WasNull then
      begin
        Statement.SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          ResultSet.GetMetadata.GetColumnType(ColumnIndex))
      end;
    end
    else
      SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, ParamValue);
  end;
end;

{**
  Apply the Refreshed values.
  @param RefreshResultSet a result set object.
  @param RefreshRowAccessor an accessor object to column values.
}
procedure TZUpdateSQL.Apply_RefreshResultSet(const Sender:IZCachedResultSet;
  const RefreshResultSet: IZResultSet; const RefreshRowAccessor: TZRowAccessor);
var
  I: Integer;
  RefreshColumnIndex: integer;
  RefreshColumnName: String;
  RefreshColumnType: TZSQLType;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  UID: TGUID absolute BCD;
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
Label CheckColumnType;
begin
  if Assigned(RefreshResultSet) then begin
    if (RefreshResultSet.GetType = rtForwardOnly) then begin
      if not RefreshResultSet.Next then
        raise EZDatabaseError.Create(SUpdateSQLNoResult);
    end else if not (RefreshResultSet.GetType = rtForwardOnly) and  not RefreshResultSet.First then
      raise EZDatabaseError.Create(SUpdateSQLNoResult);
    for I := FirstDbcIndex to RefreshResultSet.GetMetadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      RefreshColumnName := RefreshResultSet.GetMetadata.GetColumnLabel(I); // What Column from Resultset should be updated
      RefreshColumnIndex := Sender.FindColumn(RefreshColumnName); // Is the Column available in the select ?
      if RefreshColumnIndex = InvalidDbcIndex then continue; // Column not found in Select from Dataset
      if RefreshResultSet.IsNull(I) then
        RefreshRowAccessor.SetNull(RefreshColumnIndex)
      else begin
        RefreshColumnType  := RefreshResultSet.GetMetadata.GetColumnType(I); // Type of Column ?
CheckColumnType:
        case RefreshColumnType of
          stBoolean: RefreshRowAccessor.SetBoolean(RefreshColumnIndex, RefreshResultSet.GetBoolean(I));
          stByte: RefreshRowAccessor.SetByte(RefreshColumnIndex, RefreshResultSet.GetByte(I));
          stShort: RefreshRowAccessor.SetShort(RefreshColumnIndex, RefreshResultSet.GetShort(I));
          stWord: RefreshRowAccessor.SetWord(RefreshColumnIndex, RefreshResultSet.GetWord(I));
          stSmall: RefreshRowAccessor.SetSmall(RefreshColumnIndex, RefreshResultSet.GetSmall(I));
          stLongWord: RefreshRowAccessor.SetUInt(RefreshColumnIndex, RefreshResultSet.GetUInt(I));
          stInteger: RefreshRowAccessor.SetInt(RefreshColumnIndex, RefreshResultSet.GetInt(I));
          stULong: RefreshRowAccessor.SetULong(RefreshColumnIndex, RefreshResultSet.GetULong(I));
          stLong: RefreshRowAccessor.SetLong(RefreshColumnIndex, RefreshResultSet.GetLong(I));
          stFloat: RefreshRowAccessor.SetFloat(RefreshColumnIndex, RefreshResultSet.GetFloat(I));
          stDouble: RefreshRowAccessor.SetDouble(RefreshColumnIndex, RefreshResultSet.GetDouble(I));
          stCurrency: RefreshRowAccessor.SetCurrency(RefreshColumnIndex, RefreshResultSet.GetCurrency(I));
          stBigDecimal: begin
                          RefreshResultSet.GetBigDecimal(I, BCD{%H-});
                          RefreshRowAccessor.SetBigDecimal(RefreshColumnIndex, BCD);
                        end;
          stGUID: begin
              RefreshResultSet.GetGUID(I, UID);
              RefreshRowAccessor.SetGUID(RefreshColumnIndex, UID);
            end;
          stString, stUnicodeString:
            if RefreshRowAccessor.GetColumnCodePage(RefreshColumnIndex) = zCP_UTF16
            then RefreshRowAccessor.SetPWideChar(RefreshColumnIndex, RefreshResultSet.GetPWideChar(I, Len), Len)
            else RefreshRowAccessor.SetPAnsiChar(RefreshColumnIndex, RefreshResultSet.GetPAnsiChar(I, Len), Len);
          stBytes: RefreshRowAccessor.SetBytes(RefreshColumnIndex, RefreshResultSet.GetBytes(I, Len), Len);
          stDate: begin
              RefreshResultSet.GetDate(I, D);
              RefreshRowAccessor.SetDate(RefreshColumnIndex, D);
            end;
          stTime: begin
              RefreshResultSet.GetTime(I, T);
              RefreshRowAccessor.SetTime(RefreshColumnIndex, T);
            end;
          stTimestamp: begin
              RefreshResultSet.GetTimestamp(I, TS);
              RefreshRowAccessor.SetTimestamp(RefreshColumnIndex, TS);
            end;
          stAsciiStream, stUnicodeStream, stBinaryStream:
            {handle possible different column_type using a native RS
             e.g. SQLite with joins we get stream types for string/bytes etc. coulmns
             because SQLite sadly doesn't retrieve ColunmType infos
             All conversion can be made by RowAccessor but not the lob-columns!}
            if RefreshRowAccessor.GetColumnType(RefreshColumnIndex) in [stAsciiStream, stUnicodeStream, stBinaryStream] then
              RefreshRowAccessor.SetBlob(RefreshColumnIndex, RefreshResultSet.GetBlob(I))
            else
            begin
              RefreshColumnType := RefreshRowAccessor.GetColumnType(RefreshColumnIndex);
              goto CheckColumnType;
            end;
          else raise ZDbcUtils.CreateUnsupportedParameterTypeException(RefreshColumnIndex,
               RefreshColumnType);
        end;
      end;
    end;
  end;
end;

{**
  Calculate default values for the fields.
  @param Sender a cached result set object.
  @param RowAccessor an accessor object to column values.
}
procedure TZUpdateSQL.CalculateDefaults(const Sender: IZCachedResultSet;
  const RowAccessor: TZRowAccessor);
begin
 {BEGIN PATCH [1214009] TZUpdateSQL - implemented feature to Calculate default values}
 Sender.GetNativeResolver.CalculateDefaults(Sender, RowAccessor);
 {END PATCH [1214009] TZUpdateSQL - implemented feature to Calculate default values}
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZUpdateSQL.PostUpdates(const Sender: IZCachedResultSet;
 UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I: Integer;
  Statement: IZPreparedStatement;
  Config: TZSQLStrings;
  CalcDefaultValues,
  ExecuteStatement,
  UpdateAutoIncFields: Boolean;
  Tmp:String;
  lValidateUpdateCount : Boolean;
  lUpdateCount : Integer;

  function SomethingChanged: Boolean;
  var I, J: Integer;
  begin
    Result := False;
    J := 0;
    for I := 0 to DataSet.Fields.Count -1 do
      if DataSet.Fields[0].FieldKind = fkData then begin
        if OldRowAccessor.CompareBuffer(OldRowAccessor.RowBuffer,
           NewRowAccessor.RowBuffer, I+FirstDbcIndex, NewRowAccessor.GetCompareFunc(J+FirstDbcIndex, ckEquals))  <> 0 then begin
          Result := True;
          Break;
        end;
        Inc(J);
      end;
  end;
  {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
  function CreateInvalidUpdateCountException: EZSQLException; //suppress _U/LStrArrClear
  begin
    Result := EZSQLException.Create(Format(SInvalidUpdateCount, [lUpdateCount]));
  end;
  {$ENDIF WITH_VALIDATE_UPDATE_COUNT}
begin
  if (UpdateType = utDeleted)
    and (OldRowAccessor.RowBuffer.UpdateType = utInserted) then
    Exit;

  case UpdateType of
    utInserted:
      Config := FInsertSQL;
    utDeleted:
      Config := FDeleteSQL;
    utModified: if SomethingChanged
                then Config := FModifySQL
                else Exit;
    else
      Exit;
  end;

  case UpdateType of
    utInserted:
      DoBeforeInsertSQL;
    utDeleted:
      DoBeforeDeleteSQL;
    utModified:
      DoBeforeModifySQL;
    {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}// do nothing
  end;

  if (Dataset is TZAbstractRODataset) then
    (Dataset as TZAbstractRODataset).Connection.ShowSqlHourGlass;
  if (Dataset is TZAbstractDataset) then
    CalcDefaultValues := doCalcDefaults in (Dataset as TZAbstractDataset).Options
  else CalcDefaultValues := False;
    //(Dataset as TZAbstractRODataset). ZSysUtils.StrToBoolEx(DefineStatementParameter(Sender.GetStatement, DSProps_Defaults, 'true'));
  try
    for I := 0 to Config.StatementCount - 1 do begin
      if (FStmts[UpdateType].Count <= i) or not (FStmts[UpdateType][i].QueryInterface(IZPreparedStatement, Statement) = S_OK) or
         Statement.IsClosed or (Sender.GetStatement.GetParameters.Text <> Statement.GetParameters.Text) then begin
        Statement := Sender.GetStatement.GetConnection.PrepareStatementWithParams(
          Config.Statements[I].SQL, Sender.GetStatement.GetParameters);
        if (FStmts[UpdateType].Count <= i)
        then FStmts[UpdateType].Add(Statement)
        else FStmts[UpdateType][i] := Statement;
      end;
      FillStatement(Sender, Statement, Config.Statements[I],
        OldRowAccessor, NewRowAccessor);
      {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
      {Update AutoInc Field Tasks will be only executed if the UpdateAutoIncFields
       in the AfterInsertSQLStatement event returns true
      }
      ExecuteStatement := true;
      UpdateAutoIncFields := false;
      case UpdateType of
        utDeleted: DoBeforeDeleteSQLStatement(Self, I, ExecuteStatement);
        utInserted: DoBeforeInsertSQLStatement(Self, I, ExecuteStatement);
        utModified: DoBeforeModifySQLStatement(Self, I, ExecuteStatement);
        {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}// do nothing
      end;
      if ExecuteStatement then begin
        // if Property ValidateUpdateCount isn't set : assume it's true
        Tmp := Sender.GetStatement.GetParameters.Values[DSProps_ValidateUpdateCount];
        lValidateUpdateCount := (Tmp = '') or StrToBoolEx(Tmp);

        lUpdateCount := Statement.ExecuteUpdatePrepared;
        {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
        if  (lValidateUpdateCount) and (lUpdateCount <> 1   ) then
          raise CreateInvalidUpdateCountException;
        {$ENDIF}

        case UpdateType of
          utDeleted: DoAfterDeleteSQLStatement(Self, I);
          utInserted: begin
             DoAfterInsertSQLStatement(Self, I, UpdateAutoIncFields);
             if CalcDefaultValues and UpdateAutoIncFields then
                UpdateAutoIncrementFields(Sender, UpdateType,
                                          OldRowAccessor, NewRowAccessor, Self);
            end;
          utModified: DoAfterModifySQLStatement(Self,I);
          {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}// do nothing
        end;
      end;
      {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    end;
//FOSPATCH
    case UpdateType of
      utInserted, utModified:
        if FRefreshSql.Text <> '' then begin
          Tmp := FRefreshSql.Text;
          try
            Config:=FRefreshSQL;
            if (UpdateType = utInserted) then
              if (Dataset is TZAbstractDataset) then
                if FUseSequenceFieldForRefreshSQL then
                  if (TZAbstractDataset(DataSet).Sequence <> nil) and
                     (TZAbstractDataset(DataSet).SequenceField<>'') then
                    Config.Text := StringReplace(UpperCase(Config.Text),
                      ':OLD_'+UpperCase(TZAbstractDataset(DataSet).SequenceField),
                      TZAbstractDataset(DataSet).Sequence.GetCurrentValueSQL,[rfReplaceAll]);
            if CONFIG.StatementCount = 1 then begin
              if (FRefreshStmt = nil) or FRefreshStmt.IsClosed
              then Statement := Sender.GetStatement.GetConnection.PrepareStatement(Config.Statements[0].SQL)
              else Statement := FRefreshStmt;
              FillStatement(Sender, Statement, Config.Statements[0],OldRowAccessor, NewRowAccessor);
              FRefreshRS := Statement.ExecuteQueryPrepared;
              Apply_RefreshResultSet(Sender,FRefreshRS,NewRowAccessor);
            end;
          finally
            FRefreshSQL.Text:=Tmp;
          end;
        end;
      {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}// do nothing
    end; {case... }
//FOSPATCH

  finally
    if Dataset is TZAbstractRODataset then
      (Dataset as TZAbstractRODataset).Connection.HideSQLHourGlass;
  end;

  case UpdateType of
    utInserted: DoAfterInsertSQL;
    utDeleted: DoAfterDeleteSQL;
    utModified: DoAfterModifySQL;
    {$IFDEF WITH_CASE_WARNING}else;{$ENDIF}// do nothing
  end;
end;

{**
  Fires an event before delete Statement
}
procedure TZUpdateSQL.DoBeforeDeleteSQL;
begin
  if Assigned(FBeforeDeleteSQL) then
    FBeforeDeleteSQL(Self);
end;

{**
  Fires an event before insert Statement
}
procedure TZUpdateSQL.DoBeforeInsertSQL;
begin
  if Assigned(BeforeInsertSQL) then
    FBeforeInsertSQL(Self);
end;

{**
  Fires an event before modify Statement
}
procedure TZUpdateSQL.DoBeforeModifySQL;
begin
  if Assigned(FBeforeModifySQL) then
    FBeforeModifySQL(Self);
end;

{**
  Fires an event after delete Statement
}
procedure TZUpdateSQL.DoAfterDeleteSQL;
begin
  if Assigned(FAfterDeleteSQL) then
    FAfterDeleteSQL(Self);
end;

{**
  Fires an event after insert Statement
}
procedure TZUpdateSQL.DoAfterInsertSQL;
begin
  if Assigned(FAfterInsertSQL) then
    FAfterInsertSQL(Self);
end;

{**
  Fires an event after modify Statement
}
procedure TZUpdateSQL.DoAfterModifySQL;
begin
  if Assigned(FAfterModifySQL) then
    FAfterModifySQL(Self);
end;

{BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
procedure TZUpdateSQL.UpdateAutoIncrementFields(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor,
  NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
begin
 with Sender.GetNativeResolver do
   UpdateAutoIncrementFields(Sender, UpdateType,
     OldRowAccessor, NewRowAccessor, Resolver);
end;
{END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }

{NEW Methods for Events to validate at Statement level }
procedure TZUpdateSQL.DoAfterDeleteSQLStatement(const Sender: TObject;
  StatementIndex: Integer);
begin
 if Assigned(FAfterDeleteSQLStatement) then
    FAfterDeleteSQLStatement(Sender, StatementIndex);
end;

procedure TZUpdateSQL.DoAfterInsertSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out UpdateAutoIncFields: Boolean);
begin
 if Assigned(FAfterInsertSQLStatement) then
    FAfterInsertSQLStatement(Sender, StatementIndex, UpdateAutoIncFields);
end;

procedure TZUpdateSQL.DoAfterModifySQLStatement(const Sender: TObject;
  StatementIndex: Integer);
begin
 if Assigned(FAfterModifySQLStatement) then
    FAfterModifySQLStatement(Sender, StatementIndex);
end;

procedure TZUpdateSQL.DoBeforeDeleteSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeDeleteSQLStatement) then
    FBeforeDeleteSQLStatement(Sender, StatementIndex, Execute);
end;

procedure TZUpdateSQL.DoBeforeInsertSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeInsertSQLStatement) then
    FBeforeInsertSQLStatement(Sender, StatementIndex, Execute);
end;

procedure TZUpdateSQL.DoBeforeModifySQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeModifySQLStatement) then
    FBeforeModifySQLStatement(Sender, StatementIndex, Execute);
end;

end.
