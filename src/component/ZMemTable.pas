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
  ZDbcIntfs, ZDbcResultSetMetadata, ZTokenizer,
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
    FClientVariantManager: IZClientVariantManager;
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
    function GetTokenizer: IZTokenizer; override;
    function GetClientVariantManager: IZClientVariantManager; override;
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
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
    Procedure SaveToStream(AStream: TStream);
    Procedure LoadFromStream(AStream: TStream);
    {$ENDIF}
  end;

implementation

uses ZMessages, ZEncoding,
  ZDbcStatement, ZDbcMetadata, ZDbcUtils, ZDbcCache, ZDbcConnection,
  ZDbcCachedResultSet, ZGenericSqlToken;

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

  TZMemTableResultSet = class(TZVirtualResultSet)
  protected
    procedure PostRowUpdates(OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

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
var VirtualResultSet: TZMemTableResultSet;
begin
  VirtualResultSet := TZMemTableResultSet.CreateWithColumns(FColumnList, '', ConSettings);
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
    VirtualResultSet: TZMemTableResultSet;
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
    VirtualResultSet := TZMemTableResultSet.CreateWithColumns(ColumnsInfo, '', FConSettings);
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
    VirtualResultSet: TZMemTableResultSet;
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
    VirtualResultSet := TZMemTableResultSet.CreateFrom(TZProtectedAbstractRODataset(Source).ResultSet, Rows, FieldPairs, FConSettings);
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
  if Source.Required
  then Result.Nullable := ntNoNulls
  else Result.Nullable := ntNullableUnknown;
end;

constructor TZAbstractMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlsCodePage := cDynamic;
  Options := [doCheckRequired];
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
  If Self.Active Then
    Self.Close;
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

{$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
Procedure TZAbstractMemTable.LoadFromStream(AStream: TStream);

 Function ReadBool: Boolean;
 Begin
  AStream.Read(Result, SizeOf(Boolean));
 End;

 Function ReadInt: Integer;
 Begin
  AStream.Read(Result, SizeOf(Integer));
 End;

 Function ReadString(Const inSize: Integer = -1): String;
 Begin
  If inSize = -1 Then
    SetLength(Result, ReadInt)
  Else
    SetLength(Result, inSize);
  AStream.Read(Pointer(Result)^, Length(Result) * SizeOf(Char));
 End;

Var
 a, b, len, ftype, fsize{$IFDEF WITH_TVALUEBUFFER}, desiredlen{$ENDIF}: Integer;
 fname: String;
 buf: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
 ms: TMemoryStream;
 field: TField;
Begin
 Self.CheckInactive;

 Self.FieldDefs.Clear;

 Self.DisableControls;
 Try
   // Recreate FieldDefs
   len := ReadInt;
   For a := 0 To len - 1 Do
   Begin
     // These local variables are needed to make sure that data is read out from the stream in proper order
     fname := ReadString;
     ftype := ReadInt;
     fsize := ReadInt;
     Self.FieldDefs.Add(fname, TFieldType(ftype), fsize, ReadBool);
   End;

   // Activate the MemTable so we can write the data back
   Self.Open;

   // Now read each field of each record, one by one
   len := ReadInt;
   For a := 0 To len - 1 Do
   Begin
     Self.Append;

     For b := 0 To Self.FieldCount - 1 Do
     Begin
       // Use a local variable to spare the getter calls
       field := Self.Fields[b];

       fsize := ReadInt;

       // Size is zero if the field was null. Even with full null values, SetData will actually set .IsNull to false.
       // To preserve .IsNull, don't touch the field at all!
       If fsize = 0 Then Continue
         Else
       Case field.DataType Of
         ftMemo{$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}: field.AsString := ReadString(fsize);
         ftBlob:
           Begin
             ms := TMemoryStream.Create;
             Try
               ms.CopyFrom(AStream, fsize);
               ms.Position := 0;
               (field As TBlobField).LoadFromStream(ms);
             Finally
               FreeAndNil(ms);
             End;
           End;
         Else
           Begin
             {$IFDEF WITH_TVALUEBUFFER}
             // Due to "compression" the data written to the stream might be less than it should be. If this is the case,
             // initialize the buffer to our final data size to spare a slow SetLength operation.

             // Bugfix: TZAbstractRODataSet.GetData handles buffer as PDateTime in most of the cases. As SizeOf(Double) = 8 and
             // SizeOf(Integer) = 4, it's better to reserve more to avoid memory and / or data corruption
             If (field Is TDateField) Or (field Is TTimeField) Then
               desiredlen := SizeOf(TDateTime)
             Else
               desiredlen := field.DataSize;

             // ToDo: In theory data written in the stream must never be larger than the field's DataSize. As this shows
             // a clear sign of corruption, throwing an exception might be a good idea...?
             If fsize < desiredlen Then
               SetLength(buf, desiredlen)
             Else
               SetLength(buf, fsize);
             AStream.Read(buf, fsize);

             // If less data was read from the stream we must make sure to zero out the rest to avoid value corruption!
             If fsize < desiredlen Then
               FillChar(buf[fsize], Length(buf) - fsize, #0);

             field.SetData(buf);
             {$ELSE}
             GetMem(buf, fsize);
             Try
               AStream.Read(buf^, fsize);

               // ToDo: Someone with some pointer magic knowledge to implement "decompression" for older Delphis

               field.SetData(buf);
             Finally
               FreeMem(buf);
             End;
             {$ENDIF}
           End;
       End;
     End;

     Self.Post;
   End;

   Self.First;
 Finally
   Self.EnableControls;
 End;
End;
{$ENDIF}

function TZAbstractMemTable.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

function TZAbstractMemTable.GetTokenizer: IZTokenizer;
begin
  Result := TZGenericSQLTokenizer.Create as IZTokenizer;
end;

function TZAbstractMemTable.GetClientVariantManager: IZClientVariantManager;
begin
  if not assigned (FClientVariantManager) then
    FClientVariantManager := TZClientVariantManager.Create(@FLocalConSettings) as IZClientVariantManager;

  Result := FClientVariantManager;
end;

{$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
Procedure TZAbstractMemTable.SaveToStream(AStream: TStream);

 Procedure WriteBool(Const ABoolean: Boolean);
 Begin
  AStream.Write(ABoolean, SizeOf(Boolean));
 End;

 Procedure WriteInt(Const ANumber: Integer);
 Begin
  AStream.Write(ANumber, SizeOf(Integer));
 End;

 Procedure WriteString(Const AText: String);
 Begin
  WriteInt(Length(AText));
  AStream.Write(Pointer(AText)^, Length(AText) * SizeOf(Char));
 End;

Var
 bm: TBookMark;
 a{$IFDEF WITH_TVALUEBUFFER}, b{$ENDIF}, fsize: Integer;
 buf: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
 ms: TMemoryStream;
 fdef: TFieldDef;
 field: TField;
Begin
 Self.CheckActive;

 bm := Self.GetBookmark;
 Try
   Self.DisableControls;
   Try
     // Write all FieldDefs
     WriteInt(Self.FieldDefs.Count);
     For a := 0 To Self.FieldDefs.Count - 1 Do
     Begin
       // Use a local variable to spare the getter calls
       fdef := Self.FieldDefs[a];

       WriteString(fdef.Name);
       WriteInt(Integer(fdef.DataType));
       WriteInt(fdef.Size);
       WriteBool(fdef.Required);
     End;

     // Write the number of records the MemTable holds
     WriteInt(Self.RecordCount);

     // Write each field of each record, one by one
     Self.First;
     While Not Self.Eof Do
     Begin
       For a := 0 To Self.FieldCount - 1 Do
       Begin
         // Use a local variable to spare the getter calls
         field := Self.Fields[a];

         // If the field is null, save no data to the stream, only the zero length. As loading skips setting the data of zero-length
         // data, Field.IsNull will be properly preserved.
         If field.IsNull Then
           WriteInt(0)
         Else
         Case field.DataType Of
           ftMemo {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}: WriteString(field.AsString);
           ftBlob:
             Begin
               ms := TMemoryStream.Create;
               Try
                (field As TBlobField).SaveToStream(ms);
                ms.Position := 0;
                WriteInt(ms.Size);
                AStream.CopyFrom(ms, ms.Size);
              Finally
                FreeAndNil(ms);
              End;
             End;
           Else
             Begin
               {$IFDEF WITH_TVALUEBUFFER}
               // Bugfix: TZAbstractRODataSet.GetData handles buffer as PDateTime in most of the cases. As SizeOf(Double) = 8 and
               // SizeOf(Integer) = 4, it's better to reserve more to avoid memory and / or data corruption
               If (field Is TDateField) Or (field Is TTimeField) Then
                 SetLength(buf, SizeOf(TDateTime))
               Else
                 SetLength(buf, field.DataSize);

               field.GetData(buf);

               fsize := Length(buf);

               // Attempt compression - simply cut down all trailing zeroes to make the output stream smaller
               If buf[High(buf)] = 0 Then
               Begin
                 For b := High(buf) - 1 DownTo Low(buf) Do
                   If buf[b] <> 0 Then
                   Begin
                     fsize := b + 1;
                     Break;
                   End;

                 // The field is NOT NULL, leave 1 null-byte as data so the loading will actually
                 // modify the field value. This is needed to properly preserve Field.IsNull
                 // property!
                 If b = -1 Then
                   fsize := 1;
               End;

               WriteInt(fsize);
               AStream.Write(buf, fsize);
               {$ELSE}
               // Bugfix: TZAbstractRODataSet.GetData handles buffer as PDateTime in most of the cases. As SizeOf(Double) = 8 and
               // SizeOf(Integer) = 4, it's better to reserve more to avoid memory and / or data corruption
               If (field Is TDateField) Or (field Is TTimeField) Then
                 fsize := SizeOf(TDateTime)
               Else
                 fsize := field.DataSize;

               GetMem(buf, fsize);
               Try
                 field.GetData(buf);

                 // ToDo: Someone with some pointer magic knowledge to implement "compression" for older Delphis

                 WriteInt(fsize);
                 AStream.Write(buf^, fsize);
               Finally
                 FreeMem(buf);
               End;
               {$ENDIF}
             End;
         End;
       End;

       Self.Next;
     End;
   Finally
     Self.EnableControls;
   End;
 Finally
   Self.GotoBookmark(bm);
 End;
End;
{$ENDIF}

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

{ TZMemTableResultSet }

procedure TZMemTableResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
var c_idx: Integer;
    ColInfo: TZColumnInfo;
begin
  for c_idx := 0 to ColumnsInfo.Count -1 do begin
    ColInfo := TZColumnInfo(ColumnsInfo[c_idx]);
    if (ColInfo.Nullable = ntNoNulls) and NewRowAccessor.IsNull(c_idx + FirstDbcIndex) then
      raise CreateFieldRequired(ColInfo.ColumnLabel);
  end;
end;

end.
