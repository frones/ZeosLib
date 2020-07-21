{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Ado Resultset common functionality              }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcAdoResultSet;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS} Contnrs,{$ENDIF} Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZDbcGenericResolver, ZClasses,
  ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet, ZDbcResultsetMetadata, ZCompatibility, ZPlainAdo;

type
  {** Implements SQLite ResultSet Metadata. }
  TZADOResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  TZADOColumInfo = class(TZColumnInfo)
  public
    ADOColumnType: TDataTypeEnum;
  end;
  { ADO Error Class}
  EZADOConvertError = class(EZSQLException);

  {** Implements Ado ResultSet. }
  TZAdoResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    AdoColumnCount: Integer;
    FFirstFetch: Boolean;
    FAdoRecordSet: ZPlainAdo.RecordSet;
    FFields: Fields15;
    FField20: Field20;
    FValueAddr: Pointer;
    FValueType: Word;
    FColValue: OleVariant;
    FByteBuffer: PByteBuffer;
    function CreateAdoConvertError(ColumnIndex: Integer; DataType: Word): EZADOConvertError;
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      const AdoRecordSet: ZPlainAdo.RecordSet);
    procedure AfterClose; override;
    procedure ResetCursor; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function GetRow: NativeInt; override;
    function IsNull(ColumnIndex: Integer): Boolean;
    function GetString(ColumnIndex: Integer): String;
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject]);
    {$ENDIF USE_SYNCOMMONS}
  end;

  {** Implements a cached resolver with Ado specific functionality. }
  TZAdoCachedResolver = class (TZGenerateSQLCachedResolver, IZCachedResolver)
  private
    FHandle: ZPlainAdo.Command;
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Handle: ZPlainAdo.Connection;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

  TZADOCachedResultSet = Class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZADORowAccessor }

  TZADORowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, {$IFDEF FPC}ZPlainOleDBDriver{$ELSE}OleDB{$ENDIF}, ActiveX,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZDbcAdoUtils, ZEncoding, ZFastCode, ZDbcUtils, ZDbcLogging, ZDbcAdo;

{$IFDEF USE_SYNCOMMONS}
procedure TZAdoResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var Len, C, H, I: Integer;
    P: PWideChar;
    BCD: TBCD;
begin
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if IsNull(C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then begin
      if JSONWriter.Expand then begin
        if not (jcsSkipNulls in JSONComposeOptions) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,');
    end else with FField20 do begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case FValueType of
        VT_BOOL:        JSONWriter.AddShort(JSONBool[PWordBool(FValueAddr)^]);
        VT_UI1:         JSONWriter.AddU(PByte(FValueAddr)^);
        VT_UI2:         JSONWriter.AddU(PWord(FValueAddr)^);
        VT_UI4:         JSONWriter.AddU(PCardinal(FValueAddr)^);
        VT_UINT:        JSONWriter.AddU(PLongWord(FValueAddr)^);
        VT_I1:          JSONWriter.Add(PShortInt(FValueAddr)^);
        VT_I2:          JSONWriter.Add(PSmallInt(FValueAddr)^);
        VT_ERROR,
        VT_I4:          JSONWriter.Add(PInteger(FValueAddr)^);
        VT_INT:         JSONWriter.Add(PLongInt(FValueAddr)^);
        VT_HRESULT:     JSONWriter.Add(PHResult(FValueAddr)^);
        VT_UI8:         JSONWriter.AddQ(PUInt64(FValueAddr)^);
        VT_I8:          JSONWriter.Add(PInt64(FValueAddr)^);
        VT_CY:          JSONWriter.AddCurr64(PCurrency(FValueAddr)^);
        VT_DECIMAL:     begin
                          P := @FColValue;
                          if PDecimal(P).scale > 0 then begin
                            ScaledOrdinal2Bcd(UInt64(PDecimal(P).Lo64), PDecimal(P).scale, BCD, PDecimal(P).sign > 0);
                            Len := ZSysUtils.BcdToRaw(BCd, PAnsiChar(FByteBuffer), '.');
                            JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer), Len);
                          end else if PDecimal(P).sign > 0 then
                            JSONWriter.Add(Int64(-UInt64(PDecimal(P).Lo64)))
                          else
                            JSONWriter.AddQ(UInt64(PDecimal(P).Lo64));
                        end;
        VT_R4:          JSONWriter.AddSingle(PSingle(FValueAddr)^);
        VT_R8:          JSONWriter.AddDouble(PDouble(FValueAddr)^);
      else case Type_ of {ADO uses its own DataType-mapping different to System tagVariant type mapping}
          adGUID:             begin
                                JSONWriter.Add('"');
                                JSONWriter.AddNoJSONEscapeW(Pointer(PWideChar(FValueAddr)+1), 36);
                                JSONWriter.Add('"');
                              end;
          adDBTime:           if (jcoMongoISODate in JSONComposeOptions) then begin
                                JSONWriter.AddShort('ISODate("0000-00-00');
                                JSONWriter.AddDateTime(PDateTime(FValueAddr)^, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.AddShort('Z")');
                              end else begin
                                if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                                JSONWriter.AddDateTime(PDateTime(FValueAddr)^, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.Add('"');
                              end;
          adDate,
          adDBDate,
          adDBTimeStamp:      if (jcoMongoISODate in JSONComposeOptions) then begin
                                JSONWriter.AddShort('ISODate("');
                                JSONWriter.AddDateTime(PDateTime(FValueAddr)^, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.AddShort('Z")');
                              end else begin
                                if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                                JSONWriter.AddDateTime(PDateTime(FValueAddr)^, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.Add('"');
                              end;
          adChar:             begin
                                JSONWriter.Add('"');
                                JSONWriter.AddJSONEscapeW(FValueAddr, ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWideChar(FValueAddr), ActualSize));
                                JSONWriter.Add('"');
                              end;
          adWChar:            begin
                                JSONWriter.Add('"');
                                JSONWriter.AddJSONEscapeW(FValueAddr, ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWideChar(FValueAddr), ActualSize shr 1));
                                JSONWriter.Add('"');
                              end;
          adVarChar,
          adLongVarChar:      begin
                                JSONWriter.Add('"');
                                JSONWriter.AddJSONEscapeW(FValueAddr, ActualSize);
                                JSONWriter.Add('"');
                              end;
          adVarWChar,
          adLongVarWChar:     begin
                                JSONWriter.Add('"');
                                JSONWriter.AddJSONEscapeW(FValueAddr, ActualSize shr 1);
                                JSONWriter.Add('"');
                              end;
          adBinary,
          adVarBinary,
          adLongVarBinary:    JSONWriter.WrBase64(TVarData(Value).VArray.Data, ActualSize, True);
        end;
      end;
      JSONWriter.Add(',');
    end;
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZAdoResultSet.Create(const Statement: IZStatement; const SQL: string; const AdoRecordSet: ZPlainAdo.RecordSet);
begin
  inherited Create(Statement, SQL,
    TZADOResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FAdoRecordSet := AdoRecordSet;
  FByteBuffer := (Statement.GetConnection as IZAdoConnection).GetByteBufferAddress;
  Open;
end;

function TZAdoResultSet.CreateAdoConvertError(ColumnIndex: Integer;
  DataType: Word): EZADOConvertError;
begin
  Result := EZADOConvertError.Create(Format(SErrorConvertionField,
        [TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnLabel,
        IntToStr(DataType)]));
end;

{**
  Opens this recordset and initializes the Column information.
}
procedure TZAdoResultSet.Open;
var
  OleDBRowset: IUnknown;
  OleDBColumnsInfo: IColumnsInfo;
  pcColumns: NativeUInt;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I,j: Integer;
  FieldSize: Integer;
  ColumnInfo: TZADOColumInfo;
  ColName: string;
  ColType: Integer;
  F: ZPlainAdo.Field20;
  Prop: Property_;
  function StringFromVar(const V: OleVariant): String;
  begin
    if not VarIsStr(V)
    then Result := ''
    else Result := V;
  end;
begin
//Check if the current statement can return rows
  if not Assigned(FAdoRecordSet) or (FAdoRecordSet.State = adStateClosed) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  (FAdoRecordSet as ADORecordsetConstruction).Get_Rowset(OleDBRowset);
  OleDBRowset.QueryInterface(IColumnsInfo, OleDBColumnsInfo);

  OleDBColumnsInfo.GetColumnInfo(pcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo;

  { Fills the column info }
  ColumnsInfo.Clear;
  AdoColumnCount := FAdoRecordSet.Fields.Count;

  if Assigned(prgInfo) then
    if prgInfo.iOrdinal = 0 then
      Inc({%H-}NativeInt(prgInfo), SizeOf(TDBColumnInfo));

  for I := 0 to AdoColumnCount - 1 do
  begin
    ColumnInfo := TZADOColumInfo.Create;

    F := FAdoRecordSet.Fields.Item[I];
    {$IFDEF UNICODE}
    ColName := F.Name;
    {$ELSE}
    ColName := PUnicodeToRaw(Pointer(F.Name), Length(F.Name), ConSettings.CTRL_CP);
    {$ENDIF}
    ColType := F.Type_;
    ColumnInfo.ColumnLabel := ColName;

    for j := 0 to F.Properties.Count -1 do begin
      Prop := F.Properties.Item[j];
      if Prop.Name = 'BASECOLUMNNAME' then
        ColumnInfo.ColumnName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASETABLENAME' then
        ColumnInfo.TableName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASECATALOGNAME' then
        ColumnInfo.CatalogName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASESCHEMANAME' then
        ColumnInfo.SchemaName := StringFromVar(Prop.Value)
      else if (Prop.Name = 'ISAUTOINCREMENT') and not (TVarData(Prop.Value).VType in [varEmpty, varNull]) then
        ColumnInfo.AutoIncrement := Prop.Value
    end;

    ColumnInfo.ColumnType := ConvertAdoToSqlType(ColType, F.Precision, F.NumericScale);
    FieldSize := F.DefinedSize;
    if FieldSize < 0 then
      FieldSize := 0;
    if F.Type_ = adGuid
    then ColumnInfo.Precision := 38
    else if ColType = adCurrency then begin
      ColumnInfo.Precision := 19;
      ColumnInfo.Scale := 4;
      ColumnInfo.Currency := True;
    end else if ColType in [adDecimal, adNumeric] then begin
      ColumnInfo.Precision := F.Precision;
      ColumnInfo.Scale := F.NumericScale;
    end else begin
      ColumnInfo.Precision := FieldSize;
    end;
    ColumnInfo.Signed := ColType in [adTinyInt, adSmallInt, adInteger, adBigInt, adDouble, adSingle, adCurrency, adDecimal, adNumeric, adBinary];
    ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0) and (F.Properties.Item['BASECOLUMNNAME'].Value <> null) and not ColumnInfo.AutoIncrement;
    ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0) or ColumnInfo.AutoIncrement;
    ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
    if (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) <> 0 then
    case ColumnInfo.ColumnType of
      stString: ColumnInfo.ColumnType := stAsciiStream;
      stUnicodeString: ColumnInfo.ColumnType := stUnicodeStream;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;

    ColumnInfo.ADOColumnType := ColType;
    ColumnsInfo.Add(ColumnInfo);
    Inc({%H-}NativeInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
  end;
  if Assigned(ppStringsBuffer) then ZAdoMalloc.Free(ppStringsBuffer);
  if Assigned(OriginalprgInfo) then ZAdoMalloc.Free(OriginalprgInfo);
  FFirstFetch := True;
  inherited;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  ADO resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAdoResultSet.AfterClose;
begin
  FAdoRecordSet := nil;
  inherited AfterClose;
end;

procedure TZAdoResultSet.ResetCursor;
begin
  { Resync the Adorecordsets leads to pain with huge collection of Data !!}
  FFields := nil;
  FField20 := nil;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZAdoResultSet.Next: Boolean;
begin
  Result := False;
  FField20 := nil;
  FFields := nil;
  if (FAdoRecordSet = nil) or (FAdoRecordSet.BOF and FAdoRecordSet.EOF) then
    Exit;
  if FAdoRecordSet.BOF then
    FAdoRecordSet.MoveFirst
  else if not FAdoRecordSet.EOF and not FFirstFetch then
    FAdoRecordSet.MoveNext;
  FFirstFetch := False;
  Result := not FAdoRecordSet.EOF;
  if Result then FFields := FAdoRecordSet.Fields;
  RowNo := RowNo +1;
  if Result then
    LastRowNo := RowNo;
  if not Result and not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZAdoResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  FField20 := nil;
  FFields := nil;
  if FAdoRecordSet.CursorType = adUseClient then begin
    if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
       FAdoRecordSet.MoveFirst;
    if Row > 0 then
      FAdoRecordSet.Move(Row - 1, adBookmarkFirst)
    else
      FAdoRecordSet.Move(Abs(Row) - 1, adBookmarkLast);
    Result := not (FAdoRecordSet.EOF or FAdoRecordSet.BOF);
    if Result
    then FFields := FAdoRecordSet.Fields
    else if not LastRowFetchLogged and DriverManager.HasLoggingListener and FAdoRecordSet.EOF then
      DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
  end else
    Result := inherited MoveAbsolute(Row);
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAdoResultSet.GetRow: NativeInt;
begin
  if FAdoRecordSet.CursorType = adUseClient then
    if FAdoRecordSet.EOF or FAdoRecordSet.BOF
    then Result := -1
    else Result := FAdoRecordSet.AbsolutePosition
  else Result := RowNo;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAdoResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  if (FFields = nil) then begin
    Result := True;
    FValueAddr := nil;
    FField20 := nil;
  end else begin
    FField20 := FFields.Get_Item(ColumnIndex);
    FColValue := FField20.Value;
    FValueType := tagVariant(FColValue).vt;
    if (FValueType = VT_NULL) or (FValueType = VT_EMPTY) then begin
      Result := True;
      FValueAddr := nil;
    end else begin
      Result := False;
      if FValueType and VT_BYREF = VT_BYREF then begin
        FValueType := FValueType xor VT_BYREF;
        FValueAddr := tagVariant(FColValue).unkVal;
      end else if FValueType = VT_DECIMAL
        then FValueAddr := (@FColValue)
        else if (FValueType = VT_BSTR)
          then FValueAddr := tagVariant(FColValue).bstrVal
          else FValueAddr := @tagVariant(FColValue).bVal;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetString(ColumnIndex: Integer): String;
var P: {$IFDEF UNICODE}PWidechar{$ELSE}PAnsiChar{$ENDIF};
  L: NativeUInt;
begin
  {$IFDEF UNICODE}
  P := GetPWideChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else System.SetString(Result, P, L)
  else Result := '';
  {$ELSE}
  if ConSettings.AutoEncode then
    if ConSettings.CTRL_CP = zCP_UTF8
    then Result := GetUTF8String(ColumnIndex)
    else Result := GetAnsiString(ColumnIndex)
  else begin
    P := GetPAnsiChar(ColumnIndex, L);
    if (P <> nil) and (L > 0) then
      if P = Pointer(FRawTemp)
      then Result := FRawTemp
      else System.SetString(Result, P, L)
    else Result := '';
  end;
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var Len: NativeUInt;
  PW: PWideChar;
  PA: PAnsiChar absolute PW;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
     Result := ''
  else with FField20 do
    case Type_ of
      adChar, adWChar, adVarChar, adLongVarChar, adBSTR, adVarWChar,
      adLongVarWChar: begin
                        PW := GetPWidechar(ColumnIndex, Len);
                        Result := PUnicodeToRaw(PW, Len, ZOSCodePage);
                      end;
      else            begin
                        PA := GetPAnsiChar(ColumnIndex, Len);
                        ZSetString(PA, Len, Result);
                      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var Len: NativeUInt;
  PW: PWideChar;
  PA: PAnsiChar absolute PW;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
     Result := ''
  else with FField20 do
    case Type_ of
      adChar, adWChar, adVarChar, adLongVarChar, adBSTR, adVarWChar,
      adLongVarWChar: begin
                        PW := GetPWidechar(ColumnIndex, Len);
                        Result := PUnicodeToRaw(PW, Len, zCP_UTF8);
                      end;
      else            begin
                        PA := GetPAnsiChar(ColumnIndex, Len);
                        ZSetString(PA, Len, Result);
                      end;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
var P: PAnsiChar;
  L: NativeUInt;
begin
  P := GetPAnsiChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FRawTemp)
    then Result := FRawTemp
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    else ZSetString(P, L, Result)
    {$ELSE}
    else System.SetString(Result, P, L)
    {$ENDIF}
  else Result := EmptyRaw;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
var BCD: TBCD;
  PW: PWideChar;
label Set_From_Buf;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with FField20 do begin
    case FValueType of
      VT_BOOL:        if PWordBool(FValueAddr)^ then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      VT_UI1, VT_UI2, VT_UI4, VT_UINT: begin
                        IntToRaw(GetUInt(ColumnIndex), PAnsiChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: begin
                        IntToRaw(GetInt(ColumnIndex), PAnsiChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_UI8:         begin
                        IntToRaw(PUInt64(FValueAddr)^, PAnsiChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_I8:          begin
                        IntToRaw(PInt64(FValueAddr)^, PAnsiChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_CY:          begin
                        CurrToRaw(PCurrency(FValueAddr)^, PAnsiChar(fByteBuffer), @Result);
Set_From_Buf:           Len := Result - PAnsiChar(fByteBuffer);
                        Result := PAnsiChar(fByteBuffer);
                      end;
      VT_DECIMAL:     begin
                        Result := @FColValue;
                        ScaledOrdinal2Bcd(UInt64(PDecimal(Result).Lo64), PDecimal(Result).scale, BCD{%H-}, PDecimal(Result).sign > 0);
                        Result := PAnsiChar(fByteBuffer);
                        Len := ZSysUtils.BcdToRaw(BCd, Result, '.');
                      end;
      VT_R4:          begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PSingle(FValueAddr)^, Result);
                      end;
      VT_R8:          begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PDouble(FValueAddr)^, Result);
                      end;
      VT_DATE:        case Type_ of
                        adDate, adDBDate: begin
                            Result := PAnsiChar(fByteBuffer);
                            Len := DateTimeToRawSQLDate(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        adDBTime: begin
                            Result := PAnsiChar(fByteBuffer);
                            Len := DateTimeToRawSQLTime(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        else begin
                            Result := PAnsiChar(fByteBuffer);
                            Len := DateTimeToRawSQLTimeStamp(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                      end;
      else case Type_ of
        adVarBinary,
        adLongVarBinary,
        adBinary:     begin
                        Result := TVarData(FColValue).VArray.Data;
                        Len := ActualSize;
                      end;
        else begin
          PW := GetPWideChar(ColumnIndex, Len);
          FRawTemp := PUnicodeToRaw(PW, Len, ConSettings.CTRL_CP);
          Result := Pointer(FRawTemp);
          Len := Length(FRawTemp);
        end;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
var BCD: TBCD;
label Set_From_Buf;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with FField20 do begin
    case FValueType of
      VT_BOOL:        if PWordBool(FValueAddr)^ then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      VT_UI1, VT_UI2, VT_UI4, VT_UINT: begin
                        IntToUnicode(GetUInt(ColumnIndex), PWideChar(fByteBuffer), @Result);
                        goto Set_From_Buf; end;
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: begin
                        IntToUnicode(GetInt(ColumnIndex), PWideChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_UI8:         begin
                        IntToUnicode(PUInt64(FValueAddr)^, PWideChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_I8:          begin
                        IntToUnicode(PInt64(FValueAddr)^, PWideChar(fByteBuffer), @Result);
                        goto Set_From_Buf;
                      end;
      VT_CY:          begin
                        CurrToUnicode(PCurrency(FValueAddr)^, PWideChar(fByteBuffer), @Result);
Set_From_Buf:           Len := Result - PWideChar(fByteBuffer);
                        Result := PWideChar(fByteBuffer);
                      end;
      VT_DECIMAL:     begin
                        Result := @FColValue;
                        ScaledOrdinal2Bcd(UInt64(PDecimal(Result).Lo64), PDecimal(Result).scale, BCD{%H-}, PDecimal(Result).sign > 0);
                        Result := PWideChar(fByteBuffer);
                        Len := ZSysUtils.BcdToUni(BCd, Result, '.');
                      end;
      VT_R4:          begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PSingle(FValueAddr)^, Result);
                      end;
      VT_R8:          begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PDouble(FValueAddr)^, Result);
                      end;
      VT_DATE:        case Type_ of
                        adDate, adDBDate: begin
                            Result := PWideChar(fByteBuffer);
                            Len := DateTimeToUnicodeSQLDate(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        adDBTime: begin
                            Result := PWideChar(fByteBuffer);
                            Len := DateTimeToUnicodeSQLTime(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        else begin
                            Result := PWideChar(fByteBuffer);
                            Len := DateTimeToUnicodeSQLTimeStamp(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                      end;
      else case Type_ of
        adGUID:       begin
                        Result := FValueAddr;
                        Len := 38;
                      end;
        adChar:       begin
                        Result := FValueAddr;
                        Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, ActualSize);
                      end;
        adVarChar,
        adLongVarChar: begin
                        Result := FValueAddr;
                        Len := ActualSize;
                      end;
        adWChar:      begin
                        Result := FValueAddr;
                        Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, ActualSize shr 1);
                      end;
        adLongVarWChar,
        adVarWChar:   begin
                        Result := FValueAddr;
                        Len := ActualSize shr 1;
                      end;
        else          try
                        FUniTemp := FColValue;
                        Len := Length(FUniTemp);
                        Result := Pointer(FUniTemp);
                      except
                        Len := 0;
                        Result := nil;
                        LastWasNull := True;
                      end;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var P: PWideChar;
  L: NativeUInt;
begin
  P := GetPWideChar(ColumnIndex, L);
  if LastWasNull or (L = 0) then
    Result := ''
  else if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else System.SetString(Result, P, L);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAdoResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := False;
  end else case FValueType of
    VT_BOOL:        Result := PWordBool(FValueAddr)^;
    VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex) <> 0;
    VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex) <> 0;
    VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^ <> 0;
    VT_UI8:         Result := PUInt64(FValueAddr)^ <> 0;
    VT_I8:          Result := PInt64(FValueAddr)^ <> 0;
    VT_CY:          Result := PCurrency(FValueAddr)^ <> 0;
    VT_DECIMAL:     Result := PDecimal(@FColValue).Lo64 <> 0;
    VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex)) <> 0;
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := StrToBoolEx(P, P+Len, True, False);
    end;
  end;
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with FField20 do
    case FValueType of
      VT_ARRAY or VT_UI1: begin
            Result := TVarData(FColValue).VArray.Data;
            Len := ActualSize;
          end;
      else if Type_ = adGUID then begin
          SetLength(FRawTemp, SizeOf(TGUID));
          Result := Pointer(FRawTemp);
          Len := SizeOf(TGUID);
          ValidGUIDToBinary(PWideChar(FValueAddr), Pointer(Result));
        end else raise Self.CreateAdoConvertError(ColumnIndex, Type_);
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetInt(ColumnIndex: Integer): Integer;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else case FValueType of
    VT_BOOL:        Result := Ord(PWord(FValueAddr)^ <> 0);
    VT_UI1:         Result := PByte(FValueAddr)^;
    VT_UI2:         Result := PWord(FValueAddr)^;
    VT_UI4:         Result := PInteger(FValueAddr)^;
    VT_UINT:        Result := PCardinal(FValueAddr)^;
    VT_I1:          Result := PShortInt(FValueAddr)^;
    VT_I2:          Result := PSmallInt(FValueAddr)^;
    VT_I4:          Result := Pinteger(FValueAddr)^;
    VT_INT:         Result := tagVARIANT(FColValue).intVal;
    VT_HRESULT,
    VT_ERROR:       Result := PHResult(FValueAddr)^;
    VT_UI8, VT_I8, VT_CY, VT_DECIMAL, VT_R4, VT_R8, VT_DATE: Result := GetLong(ColumnIndex);
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := UnicodeToIntDef(P, P+Len, 0);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetLong(ColumnIndex: Integer): Int64;
var P: PWideChar;
  PD: PDecimal absolute P;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else with FField20 do begin
    case FValueType of
      VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
      VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex);
      VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^;
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_CY:          Result := PInt64(FValueAddr)^ div 10000;
      VT_DECIMAL: begin
                    PD := @FColValue;
                    Result := Int64(PD.Lo64);
                    if PD.scale > 0 then
                      Result := Result div Int64Tower[PD.scale];
                    if PD.sign > 0 then
                      Result := -Result;
                  end;
      VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex));
      else begin
        P := GetPWideChar(ColumnIndex, Len);
        Result := UnicodeToInt64Def(P, P+Len, 0);
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else case FValueType of
    VT_BOOL:        Result := Ord(PWord(FValueAddr)^ <> 0);
    VT_UI1:         Result := PByte(FValueAddr)^;
    VT_UI2:         Result := PWord(FValueAddr)^;
    VT_UI4:         Result := PInteger(FValueAddr)^;
    VT_UINT:        Result := PCardinal(FValueAddr)^;
    VT_I1:          Result := PShortInt(FValueAddr)^;
    VT_I2:          Result := PSmallInt(FValueAddr)^;
    VT_I4:          Result := PInteger(FValueAddr)^;
    VT_INT:         Result := tagVARIANT(FColValue).intVal;
    VT_HRESULT,
    VT_ERROR:       Result := PHResult(FValueAddr)^;
    VT_UI8, VT_I8, VT_CY, VT_DECIMAL, VT_R4, VT_R8, VT_DATE: Result := GetULong(ColumnIndex);
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := UnicodeToUInt64Def(P, P+Len, 0);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAdoResultSet.GetULong(ColumnIndex: Integer): UInt64;
var P: PWideChar;
  PD: PDecimal absolute P;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else case FValueType of
    VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
    VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex);
    VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^;
    VT_UI8:         Result := PUInt64(FValueAddr)^;
    VT_I8:          Result := PInt64(FValueAddr)^;
    VT_CY:          Result := PInt64(FValueAddr)^ div 10000;
    VT_DECIMAL: begin
                  PD := @FColValue;
                  Result := UInt64(PD.Lo64);
                  if PD.scale > 0 then
                    Result := Result div Uint64(Int64Tower[PD.scale]);
                end;
    VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex));
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := UnicodeToUInt64Def(P, P+Len, 0);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := GetDouble(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZAdoResultSet.GetGUID(ColumnIndex: Integer; var Result: TGUID);
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    FillChar(Result, SizeOf(TGUID), #0)
  else with FField20 do
    if Type_ = adGUID then
      ZSysUtils.ValidGUIDToBinary(PWideChar(FValueAddr), @Result.D1)
    else  raise CreateAdoConvertError(ColumnIndex, Type_);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetDouble(ColumnIndex: Integer): Double;
var PD: PDecimal;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    Result := 0
  else with FField20 do begin
    case FValueType of
      VT_R8:          Result := PDouble(FValueAddr)^;
      VT_R4:          Result := PSingle(FValueAddr)^;
      VT_DECIMAL:     begin
                        PD := @FColValue;
                        Result := Uint64(PDecimal(PD).Lo64) / ZFastCode.Int64Tower[PD.Scale];
                        if PD.sign > 0 then
                          Result := -Result;
                      end;
      VT_DATE:        Result := PDouble(FValueAddr)^;
      VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: Result := GetInt(ColumnIndex);
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_CY:          Result := PCurrency(FValueAddr)^;
      else  UnicodeToFloatDef(GetPWideChar(ColumnIndex, Len), WideChar('.'), 0, Result)
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAdoResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var
  Len: NativeUint;
  P: PWideChar;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := NullBCD
  else case FValueType of
    VT_BOOL:        ScaledOrdinal2Bcd(Word(Ord(PWord(FValueAddr)^ <> 0)), 0, Result);
    VT_UI1:         ScaledOrdinal2Bcd(Word(PByte(FValueAddr)^), 0, Result, False);
    VT_UI2:         ScaledOrdinal2Bcd(PWord(FValueAddr)^, 0, Result, False);
    VT_UI4:         ScaledOrdinal2Bcd(PCardinal(FValueAddr)^, 0, Result, False);
    VT_UINT:        ScaledOrdinal2Bcd(PLongWord(FValueAddr)^, 0, Result, False);
    VT_UI8:         ScaledOrdinal2Bcd(PUInt64(FValueAddr)^, 0, Result, False);
    VT_I1:          ScaledOrdinal2Bcd(SmallInt(PShortInt(FValueAddr)^), 0, Result);
    VT_I2:          ScaledOrdinal2Bcd(PSmallInt(FValueAddr)^, 0, Result);
    VT_HRESULT,
    VT_ERROR,
    VT_I4:          ScaledOrdinal2Bcd(PInteger(FValueAddr)^, 0, Result);
    VT_I8:          ScaledOrdinal2Bcd(PInt64(FValueAddr)^, 0, Result);
    VT_INT:         ScaledOrdinal2Bcd(PLongInt(FValueAddr)^, 0, Result);
    VT_CY:          ScaledOrdinal2Bcd(PInt64(FValueAddr)^, 4, Result);
    VT_DECIMAL:     ScaledOrdinal2Bcd(UInt64(PDecimal(FValueAddr)^.Lo64), PDecimal(FValueAddr)^.Scale, Result, PDecimal(FValueAddr)^.Sign > 0);
    VT_R4:          Double2BCD(PSingle(FValueAddr)^, Result);
    VT_R8, VT_DATE: Double2BCD(PDouble(FValueAddr)^, Result);
    else begin
        P := GetPWideChar(ColumnIndex, Len);
        if not ZSysUtils.TryUniToBcd(P, Len, Result, '.') then
          Result := NullBCD;
      end;
  end;
end;

function TZAdoResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  Len: NativeUint;
  P: PWideChar;
  PD: PDecimal absolute P;
  i64: Int64 absolute Result;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else case FValueType of
    VT_UI1, VT_I1, VT_UI2, VT_I2, VT_UI4, VT_I4, VT_UI8, VT_I8, VT_INT,
    VT_UINT, VT_HRESULT, VT_ERROR, VT_BOOL: Result := GetLong(FirstDbcIndex);
    VT_CY: Result := PCurrency(FValueAddr)^;
    VT_DECIMAL: begin
                  PD := PDecimal(@FColValue);
                  i64 := UInt64(PD.Lo64);
                  if PD.sign > 0 then
                    i64 := -i64;
                  if PD.scale < 4 then
                    i64 := i64 * ZFastCode.Int64Tower[4-PD.scale]
                  else if PD.scale > 4 then
                    i64 := i64 div ZFastCode.Int64Tower[PD.scale-4];
                end;
    VT_R4:      Result := PSingle(FValueAddr)^;
    VT_R8, VT_DATE: Result := PDouble(FValueAddr)^;
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  UnicodeToFloatDef(P, WideChar('.'), Len);
                end;
    else raise CreateAdoConvertError(ColumnIndex, FField20.Type_);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAdoResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var P: PWideChar;
  Len: NativeUint;
label Fill;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
Fill: PInt64(@Result.Year)^ := 0;
  end else case FValueType of
    VT_DATE: DecodeDateTimeToDate(PDateTime(FValueAddr)^, Result);
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result);
                  if LastWasNull then
                    goto fill;
                end;
    else DecodeDateTimeToDate(GetDouble(ColumnIndex), Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAdoResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var P: PWideChar;
  Len: NativeUint;
Label Fill;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
Fill: PCardinal(@Result.Hour)^ := 0;
    PInt64(@Result.Second)^ := 0;
  end else case FValueType of
    VT_DATE:  DecodeDateTimeToTime(PDateTime(FValueAddr)^, Result);
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
                  if LastWasNull then
                    goto fill;
                end;
    else     DecodeDateTimeToTime(GetDouble(ColumnIndex), Result);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
procedure TZAdoResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var P: PWideChar;
  Len: NativeUint;
Label Fill;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
Fill: FillChar(Result, SizeOf(TZTimeStamp), #0);
  end else case FValueType of
    VT_DATE:    DecodeDateTimeToTimeStamp(PDateTime(FValueAddr)^, Result);
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
                  if LastWasNull then
                    goto fill;
                end;
    else     DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex), Result);
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAdoResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var L: LengthInt;
label CLOB;
begin
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    Result := nil
  else with FField20 do
    case Type_ of
      adGUID:   begin L := 38; goto CLOB; end;
      adChar:   begin
                  L := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWidechar(FValueAddr), ActualSize);
                  goto CLOB; end;
      adWChar:  begin
                  L := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWidechar(FValueAddr), ActualSize shr 1);
                  goto CLOB; end;
      adVarChar,
      adLongVarChar: begin
                  L := ActualSize;
                  goto CLOB; end;
      adBSTR,
      adLongVarWChar,
      adVarWChar: begin
                  L := ActualSize shr 1;
CLOB:             Result := TZLocalMemCLob.CreateWithData(PWidechar(FValueAddr), L, ConSettings, FOpenLobStreams);
                end;

      adVarBinary,
      adLongVarBinary,
      adBinary:   Result := TZLocalMemBLob.CreateWithData(TVarData(FColValue).VArray.Data, ActualSize, FOpenLobStreams);
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType);
    end;
end;


{ TZAdoCachedResolver }

{**
  Creates a Ado specific cached resolver object.
  @param PlainDriver a native Ado plain driver.
  @param Handle a Ado specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZAdoCachedResolver.Create(const Handle: ZPlainAdo.Connection;
  const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FHandle := ZPlainAdo.CoCommand.Create;
  FHandle._Set_ActiveConnection(Handle);
  FHandle.CommandText := 'SELECT @@IDENTITY';
  FHandle.CommandType := adCmdText;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZAdoCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Recordset: ZPlainAdo.Recordset;
  RA: OleVariant;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Recordset := FHandle.Execute(RA, null, 0);
    if Recordset.RecordCount > 0 then
      NewRowAccessor.SetLong(FAutoColumnIndex, Recordset.Fields.Item[0].Value);
  end;
end;

{ TZADOResultSetMetadata }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZADOResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  inherited ClearColumn(ColumnInfo);
  {ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;}
end;
{ TZADOCachedResultSet }

class function TZADOCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZADORowAccessor;
end;

{ TZADORowAccessor }

constructor TZADORowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  CachedLobs: WordBool);
var TempColumns: TObjectList;
  I: Integer;
  Current: TZColumnInfo;
begin
  {EH: usually this code is NOT nessecary if we would handle the types as the
  providers are able to. But in current state we just copy all the incompatibilities
  from the DataSets into dbc... grumble.}
  TempColumns := TObjectList.Create(True);
  CopyColumnsInfo(ColumnsInfo, TempColumns);
  for I := 0 to TempColumns.Count -1 do begin
    Current := TZColumnInfo(TempColumns[i]);
    if Current.ColumnType in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-3); // no streams available using ADO
      Current.Precision := -1;
    end;
    if Current.ColumnType = stString then begin
      Current.ColumnType := stUnicodeString; // no raw chars in ADO
      Current.ColumnCodePage := zCP_UTF16;
    end else if Current.ColumnType = stBytes then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, CachedLobs);
  TempColumns.Free;
end;

{$ENDIF ZEOS_DISABLE_ADO}
end.
