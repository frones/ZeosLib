{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Abstract MemTable component               }
{                                                         }
{          Originally written by EgonHugeist              }
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

unit ZMemTable;

{$I ZComponent.inc}

interface

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZCompatibility, ZClasses,
  ZDbcIntfs, ZDbcResultSetMetadata,
  ZAbstractDataset, ZAbstractRODataset, ZAbstractConnection, ZDatasetUtils;

type
  TZAbstractMemTable = class(TZAbstractRWDataSet)
  private
    FColumnsInfo: TObjectList;
    procedure ConvertFiedDefsToColumnsInfo(const Source: TFieldDefs);
    function ConvertFiedDefToColumnsInfo(const Source: TFieldDef): TZColumnInfo;
    function StoreControlsCodepage: Boolean;
    function GetControlsCodePage: TZControlsCodePage;
    procedure SetControlsCodePage(const Value: TZControlsCodePage);
  protected
    FLocalConSettings: TZConSettings;
    FCharacterSet: TZCodePage;
    function CreateResultSet(const SQL: string; MaxRows: Integer):
      IZResultSet; override;
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    procedure CheckSQLQuery; override;
    procedure CheckConnected; override;
    procedure InternalRefresh; override;
    procedure InternalPrepare; override;
    procedure InternalInitFieldDefs; override;
    /// <summary>Sets database connection object.</summary>
    /// <param>"Value" a database connection object.</param>
    procedure SetConnection(Value: TZAbstractConnection); override;
    function GetTryKeepDataOnDisconnect: Boolean; override;
    function PSIsSQLBased: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    /// <summary>Clones data structure and copies the data from a source zeos dataset</summary>
    /// <param>"Source" a TZAbstractRODataset or descendant we clone from</param>
    procedure CloneDataFrom(Source: TZAbstractRODataset);
    /// <summary>Assigns the data from a source zeos dataset. The
    ///  FieldDefs are used to find matching fields by it's name. All matches
    ///  get a copy as given by Source.</summary>
    /// <param>"Source" a TZAbstractRODataset or descendant we clone from</param>
    procedure AssignDataFrom(Source: TZAbstractRODataset);
    procedure Clear;
    procedure Empty;
  published
    /// <summary>represents the codpage the character fieldtypes are mapped to.
    ///  The value is ignored if a connection is assigned</summary>
    property ControlsCodePage: TZControlsCodePage read GetControlsCodePage write SetControlsCodePage stored StoreControlsCodepage;
  end;

implementation

uses ZMessages, ZEncoding,
  ZDbcStatement, ZDbcMetadata, ZDbcUtils, ZDbcCache,
  ZDbcCachedResultSet;

type
  TZMemResultSetPreparedStatement = Class(TZBeginnerPreparedStatement,
    IZPreparedStatement)
  private
    FColumnList: TObjectList;
  public
    constructor Create(ConSettings: PZConSettings;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}AColumnList: TObjectList;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
    destructor Destroy; override;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  End;

{ TZMemResultSetPreparedStatement }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Info" not used} {$ENDIF}
constructor TZMemResultSetPreparedStatement.Create(
  ConSettings: PZConSettings;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}AColumnList: TObjectList;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
begin
  Self.ConSettings := ConSettings;
  FColumnList := TObjectList.Create;
  if AColumnList <> nil then
    CopyColumnsInfo(AColumnList, FColumnList);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZMemResultSetPreparedStatement.Destroy;
begin
  inherited;
  FreeAndNil(FColumnList);
end;

function TZMemResultSetPreparedStatement.ExecutePrepared: Boolean;
begin
  LastResultSet := ExecuteQueryPrepared;
  Result := True;
end;

function TZMemResultSetPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var VirtualResultSet: TZVirtualResultSet;
begin
  VirtualResultSet := TZVirtualResultSet.CreateWithColumns(FColumnList, '', ConSettings);
  Result := VirtualResultSet;
  VirtualResultSet.SetType(GetResultSetType);
  VirtualResultSet.SetConcurrency(GetResultSetConcurrency);
end;

function TZMemResultSetPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := 0;
end;

type
  TZProtectedAbstractRODataset = Class(TZAbstractRODataset);

{ TZAbstractMemTable }

procedure TZAbstractMemTable.AssignDataFrom(Source: TZAbstractRODataset);
var Rows: TZSortedList;
    FieldPairs: TZIndexPairList;
    Current: TFieldDef;
    I, ColumnIndex, Idx, SkipCount: Integer;
    RS: IZResultSet;
    CS: IZCachedResultSet;
    Metadata: IZResultSetMetadata;
    VirtualResultSet: TZVirtualResultSet;
    ColumnInfo: TZColumnInfo;
    ColumnsInfo: TObjectList;
    ReInitFieldDefs: Boolean;
    SourceCodePage, DestCodePage: Word;
begin
  if (Source = nil) or (not Source.Active) then Exit;
  if FieldDefs.Count = 0 then begin
    CloneDataFrom(Source);
    Exit;
  end;
  if Active then Close;
  if not TZProtectedAbstractRODataset(Source).IsUniDirectional and TZProtectedAbstractRODataset(Source).LastRowFetched
  then Rows := TZProtectedAbstractRODataset(Source).CurrentRows
  else Rows := nil;
  Metadata := TZProtectedAbstractRODataset(Source).ResultSetMetadata;
  { we can't judge if a user did change the field order, thus create a new lookup}
  FieldPairs := TZIndexPairList.Create;
  ColumnsInfo := TObjectList.Create(True);
  ReInitFieldDefs := False;
  try
    FieldPairs.Capacity := FieldDefs.Count;
    SkipCount := 0;
    Idx := FirstDbcIndex;
    for i := 0 to FieldDefs.Count -1 do begin
      Current := FieldDefs[i];
      ColumnIndex := MetaData.FindColumn(Current.Name);
      if (not Current.InternalCalcField) then begin
        if (ColumnIndex <> InvalidDbcIndex) then begin
          FieldPairs.Add(ColumnIndex, (Idx-SkipCount){$IFNDEF GENERIC_INDEX}+1{$ENDIF});
          ColumnInfo := TZColumnInfo.Create;
          ColumnInfo.Currency := Metadata.IsCurrency(ColumnIndex);
          ColumnInfo.Signed := Metadata.IsSigned(ColumnIndex);
          ColumnInfo.ColumnLabel := Metadata.GetOrgColumnLabel(ColumnIndex);
          ColumnInfo.ColumnType := Metadata.GetColumnType(ColumnIndex);
          ReInitFieldDefs := ReInitFieldDefs or not Current.InheritsFrom(TZFieldDef);
          ColumnInfo.Precision := Metadata.GetPrecision(ColumnIndex);
          if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then begin
            SourceCodePage := Metadata.GetColumnCodePage(ColumnIndex);
            DestCodePage := GetTransliterateCodePage(FControlsCodePage);
            if (ColumnInfo.ColumnType in [stString, stAsciiStream]) and
               ((SourceCodePage <> DestCodePage) or
                (Current.DataType in [{$IFDEF WITH_FTWIDEMEMO}ftWideMemo, {$ENDIF}
                ftWideString{$IFDEF WITH_FTFIXEDWIDECHAR}, ftFixedWideChar{$ENDIF}])) then begin
              ColumnInfo.ColumnType := TZSQLType(Byte(ColumnInfo.ColumnType)+1);
              ColumnInfo.ColumnCodePage := zCP_UTF16;
            end else ColumnInfo.ColumnCodePage := SourceCodePage;
          end;
          ColumnInfo.Scale := Metadata.GetScale(ColumnIndex);
          Inc(Idx);
        end else
          ColumnInfo := ConvertFiedDefToColumnsInfo(Current);
        ColumnsInfo.Add(ColumnInfo);
      end else Inc(SkipCount);
    end;
    FLocalConSettings.ClientCodePage := @FCharacterSet;
    FConSettings := @FLocalConSettings;
    Statement := TZMemResultSetPreparedStatement.Create(FConSettings, ColumnsInfo, Properties);
    VirtualResultSet := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', FConSettings);
    VirtualResultSet.CopyFrom(TZProtectedAbstractRODataset(Source).ResultSet, Rows, FieldPairs);
    RS := VirtualResultSet;
    if RequestLive
    then VirtualResultSet.SetConcurrency(rcUpdatable)
    else VirtualResultSet.SetConcurrency(rcReadOnly);
    {$IFDEF FPC}
    SetDefaultFields(True);
    {$ENDIF}
    SetAnotherResultset(RS);
    RS.QueryInterface(IZCachedResultSet, CS);
    CachedResultSet := CS;
  finally
    FreeAndNil(FieldPairs);
    if FColumnsInfo <> nil then
      FreeAndNil(FColumnsInfo);
    if ReInitFieldDefs then
      InternalInitFieldDefs;
    FColumnsInfo := ColumnsInfo;
  end;
end;

procedure TZAbstractMemTable.CheckConnected;
begin
  // NOOP
end;

procedure TZAbstractMemTable.CheckSQLQuery;
begin
  if FieldDefs.Count = 0 then
    raise EZDataBaseError.Create(SQueryIsEmpty);
end;

procedure TZAbstractMemTable.Clear;
begin
  Close;
  FieldDefs.Clear;
  Fields.Clear;
end;

procedure TZAbstractMemTable.CloneDataFrom(Source: TZAbstractRODataset);
var Rows: TZSortedList;
    FieldPairs: TZIndexPairList;
    Field: TField;
    I, ColumnIndex, Idx, SkipCount: Integer;
    RS: IZResultSet;
    CS: IZCachedResultSet;
    Metadata: IZResultSetMetadata;
    VirtualResultSet: TZVirtualResultSet;
begin
  if (Source = nil) or (not Source.Active) then Exit;
  if Active then Close;
  if not TZProtectedAbstractRODataset(Source).IsUniDirectional and TZProtectedAbstractRODataset(Source).LastRowFetched
  then Rows := TZProtectedAbstractRODataset(Source).CurrentRows
  else Rows := nil;
  Metadata := TZProtectedAbstractRODataset(Source).ResultSetMetadata;
  if Metadata = nil then CheckActive;

  { we can't judge if a user did change the field order, thus create a new lookup}
  FieldPairs := TZIndexPairList.Create;
  try
    FieldPairs.Capacity := Source.Fields.Count;
    SkipCount := 0;
    FieldDefs.BeginUpdate;
    try
      FieldDefs.Clear;
      for i := 0 to Source.Fields.Count -1 do begin
        Field := Source.Fields[i];
        if Field.Visible and (Field.FieldKind = fkData) then begin
          IDX := Field.Index;
          ColumnIndex := DefineFieldIndex(TZProtectedAbstractRODataset(Source).FieldsLookupTable, Field);
          FieldPairs.Add(ColumnIndex, (Idx-SkipCount){$IFNDEF GENERIC_INDEX}+1{$ENDIF});
          AddFieldDefFromMetadata(ColumnIndex, Metadata, Field.FieldName);
        end else Inc(SkipCount);
      end;
    finally
      FieldDefs.EndUpdate;
    end;
    FLocalConSettings.ClientCodePage := @FCharacterSet;
    FConSettings := @FLocalConSettings;
    Statement := TZMemResultSetPreparedStatement.Create(FConSettings, nil, Properties);
    VirtualResultSet := TZVirtualResultSet.CreateFrom(TZProtectedAbstractRODataset(Source).ResultSet, Rows, FieldPairs, FConSettings);
    RS := VirtualResultSet;
    if RequestLive
    then VirtualResultSet.SetConcurrency(rcUpdatable)
    else VirtualResultSet.SetConcurrency(rcReadOnly);
    {$IFDEF FPC}
    SetDefaultFields(True);
    {$ENDIF}
    SetAnotherResultset(RS);
    RS.QueryInterface(IZCachedResultSet, CS);
    CachedResultSet := CS;
    ConvertFiedDefsToColumnsInfo(FieldDefs);
  finally
    FreeAndNil(FieldPairs);
  end;
end;

procedure TZAbstractMemTable.ConvertFiedDefsToColumnsInfo(const Source: TFieldDefs);
var I: Integer;
    Current: TFieldDef;
    ColumnInfo: TZColumnInfo;
begin
  if FColumnsInfo = nil
  then FColumnsInfo := TObjectList.Create(True)
  else FColumnsInfo.Clear;
  for I := 0 to Source.Count - 1 do begin
    Current := Source[I];
    if not Current.InternalCalcField then begin
      ColumnInfo := ConvertFiedDefToColumnsInfo(Current);
      FColumnsInfo.Add(ColumnInfo);
    end;
  end;
end;

function TZAbstractMemTable.ConvertFiedDefToColumnsInfo(
  const Source: TFieldDef): TZColumnInfo;
begin
  Result := TZColumnInfo.Create;
  Result.ColumnType := ConvertDatasetToDbcType(Source.DataType);
  if Result.ColumnType in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
    Result.Precision := 0;
    Result.ColumnType := TZSQLType(Byte(Result.ColumnType)-3);
  end else
    Result.Precision := Source.Size;
  Result.ColumnName := Source.Name;
  Result.Writable := RequestLive;
  Result.ReadOnly := not RequestLive;
  if Source.DataType in [ftBCD, ftFmtBCD, ftTime, ftDateTime] then begin
    Result.Scale := Source.Size;
    Result.Precision := Source.Precision;
  end else if Result.ColumnType = stUnicodeString then
    Result.ColumnCodePage := zCP_UTF16
  else if Result.ColumnType = stString then
    {$IFDEF FPC}
    Result.ColumnCodePage := {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}DefaultSystemCodePage{$ELSE}zCP_UTF8{$ENDIF};
    {$ELSE}
    Result.ColumnCodePage := {$IFDEF UNICODE}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF};
    {$ENDIF}
  Result.ColumnLabel := Source.DisplayName;
end;

constructor TZAbstractMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlsCodePage := cDynamic;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "SQL" not used} {$ENDIF}
function TZAbstractMemTable.CreateResultSet(const SQL: string;
  MaxRows: Integer): IZResultSet;
var RS: IZCachedResultSet;
begin
  if (FColumnsInfo = nil) or (FColumnsInfo.Count = 0) then
    Statement := CreateStatement(SQL, Properties);
  if (FConnection <> nil) then
    FControlsCodePage := Connection.ControlsCodePage;
  FCharacterSet.Encoding := {$IFDEF UNICODE}ceUTF16{$ELSE}{$IFDEF FPC}ceUTF8{$ELSE}ceAnsi{$ENDIF}{$ENDIF};
  {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
  FCharacterSet.CP := {$IFDEF UNICODE}zCP_UTF8{$ELSE}DefaultSystemCodePage{$ENDIF};
  {$ELSE}
  FCharacterSet.CP := {$IFDEF FPC}zCP_UTF8{$ELSE}ZOSCodePage{$ENDIF};
  {$ENDIF}
  Statement := CreateStatement('', Properties);
  if RequestLive
  then Statement.SetResultSetConcurrency(rcUpdatable)
  else Statement.SetResultSetConcurrency(rcReadOnly);
  Statement.SetFetchDirection(fdForward);
  Statement.SetResultSetType(rtScrollInsensitive);
  if MaxRows > 0 then
    Statement.SetMaxRows(MaxRows);
  Result := Statement.ExecuteQueryPrepared;
  Result.QueryInterface(IZCachedResultSet, RS);
  CachedResultSet := RS;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "SQL" not used} {$ENDIF}
function TZAbstractMemTable.CreateStatement(const SQL: string;
  Properties: TStrings): IZPreparedStatement;
begin
  FConSettings := @FLocalConSettings;
  if (FColumnsInfo = nil) or (FColumnsInfo.Count = 0) then
    ConvertFiedDefsToColumnsInfo(FieldDefs);
  FLocalConSettings.ClientCodePage := @FCharacterSet;
  FLocalConSettings.W2A2WEncodingSource := encDB_CP;
  Result := TZMemResultSetPreparedStatement.Create(@FLocalConSettings, FColumnsInfo, Properties);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZAbstractMemTable.Destroy;
begin
  if FColumnsInfo <> nil then
    FreeAndNil(FColumnsInfo);
  inherited;
end;

procedure TZAbstractMemTable.Empty;
begin
  CheckActive;
  EmptyDataSet;
end;

function TZAbstractMemTable.GetTryKeepDataOnDisconnect: Boolean;
begin
  Result := True;
end;

function TZAbstractMemTable.GetControlsCodePage: TZControlsCodePage;
begin
  if FConnection = nil
  then Result := FControlsCodePage
  else Result := FConnection.ControlsCodePage;
end;

procedure TZAbstractMemTable.InternalInitFieldDefs;
var FieldDefsCopy: TFieldDefs;
    ADefCopy, Current: TFieldDef;
    I: Integer;
    SQLType: TZSQLType;
begin
  if (FColumnsInfo <> nil) then
    FColumnsInfo.Clear;
  if not DisableZFields then begin
    FieldDefsCopy := TFieldDefs.Create(Self);
    try
      //FieldDefsCopy.Capacity := FieldDefs.Count;
      FieldDefsCopy.Assign(FieldDefs);
      FieldDefs.Clear;
      for i := 0 to FieldDefsCopy.Count -1 do begin
        Current := FieldDefsCopy[i];
        SQLType := ConvertDatasetToDbcType(Current.DataType);
        if Current.InternalCalcField or not (SQLType in [stBoolean..stBinaryStream])
        then ADefCopy := FieldDefs.AddFieldDef
        else ADefCopy := TZFieldDef.Create(FieldDefs, Current.Name, Current.DataType,
            SQLType, Current.Size, Current.Required, Current.FieldNo
            {$IFDEF WITH_CODEPAGE_AWARE_FIELD}, Current.CodePage{$ENDIF});
        ADefCopy.Assign(Current);
      end;
    finally
      FreeAndNil(FieldDefsCopy);
    end;
  end;
end;

procedure TZAbstractMemTable.InternalPrepare;
begin
  //NOOP
end;

procedure TZAbstractMemTable.InternalRefresh;
begin
  //NOOP
end;

function TZAbstractMemTable.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

procedure TZAbstractMemTable.SetConnection(Value: TZAbstractConnection);
begin
  if FConnection <> Value then begin
    if Value = nil then begin
      FConnection.UnregisterComponent(Self);
      FormatSettings.SetParent(nil);
    end else begin
      FormatSettings.SetParent(Value.FormatSettings);
      Value.RegisterComponent(Self);
    end;
    FConnection := Value;
  end;
end;

procedure TZAbstractMemTable.SetControlsCodePage(
  const Value: TZControlsCodePage);
begin
  if (Value <> FControlsCodePage) then begin
    if Active then Close;
    FControlsCodePage := Value;
  end;
end;

function TZAbstractMemTable.StoreControlsCodepage: Boolean;
begin
  Result := FConnection = nil;
end;

end.
