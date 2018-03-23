{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Ado Resultset common functionality              }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
{.$DEFINE ENABLE_ADO}
{$IFDEF ENABLE_ADO}

uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF OLD_FPC}ZClasses, {$ENDIF}ZSysUtils, ZDbcIntfs, ZDbcGenericResolver,
  ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet, ZDbcResultsetMetadata, ZCompatibility, ZPlainAdo;

type
  {** Implements SQLite ResultSet Metadata. }
  TZADOResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  {** Implements Ado ResultSet. }
  TZAdoResultSet = class(TZAbstractResultSet)
  private
    AdoColTypeCache: TIntegerDynArray;
    AdoColumnCount: Integer;
    FFirstFetch: Boolean;
    FAdoRecordSet: ZPlainAdo.RecordSet;
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      const AdoRecordSet: ZPlainAdo.RecordSet);
    procedure Close; override;
    procedure ResetCursor; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function GetRow: NativeInt; override;
    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetRawByteString(ColumnIndex: Integer): RawByteString; override;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
  end;

  {** Implements a cached resolver with Ado specific functionality. }
  TZAdoCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: ZPlainAdo.Command;
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Handle: ZPlainAdo.Connection;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

implementation

uses
  Variants, {$IFDEF FPC}ZOleDB{$ELSE}OleDB{$ENDIF},
  ZMessages, ZDbcAdoUtils, ZEncoding, ZFastCode;

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
  Open;
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
  ColumnInfo: TZColumnInfo;
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
  SetLength(AdoColTypeCache, AdoColumnCount);

  if Assigned(prgInfo) then
    if prgInfo.iOrdinal = 0 then
      Inc({%H-}NativeInt(prgInfo), SizeOf(TDBColumnInfo));

  for I := 0 to AdoColumnCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;

    F := FAdoRecordSet.Fields.Item[I];
    {$IFDEF UNICODE}
    ColName := F.Name;
    {$ELSE}
    ColName := PUnicodeToString(Pointer(F.Name), Length(F.Name), ConSettings.CTRL_CP);
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

    ColumnInfo.ColumnType := ConvertAdoToSqlType(ColType, ConSettings.CPType);
    FieldSize := F.DefinedSize;
    if FieldSize < 0 then
      FieldSize := 0;
    if F.Type_ = adGuid then
      ColumnInfo.ColumnDisplaySize := 38
    else
      ColumnInfo.ColumnDisplaySize := FieldSize;
    ColumnInfo.Precision := FieldSize;
    ColumnInfo.Currency := ColType = adCurrency;
    ColumnInfo.Signed := False;
    if ColType in [adTinyInt, adSmallInt, adInteger, adBigInt, adCurrency, adDecimal, adDouble, adNumeric, adSingle] then
      ColumnInfo.Signed := True;

    ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0) and (F.Properties.Item['BASECOLUMNNAME'].Value <> null) and not ColumnInfo.AutoIncrement;
    ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0) or ColumnInfo.AutoIncrement;
    ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
    if (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) <> 0 then
    case ColumnInfo.ColumnType of
      stString: ColumnInfo.ColumnType := stAsciiStream;
      stUnicodeString: ColumnInfo.ColumnType := stUnicodeStream;
    end;

    ColumnsInfo.Add(ColumnInfo);

    AdoColTypeCache[I] := ColType;
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
procedure TZAdoResultSet.Close;
begin
  FAdoRecordSet := nil;
  inherited Close;
end;

procedure TZAdoResultSet.ResetCursor;
begin
  { Resync the Adorecordsets leads to pain with huge collection of Data !!}
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
  if (FAdoRecordSet = nil) or (FAdoRecordSet.BOF and FAdoRecordSet.EOF) then
    Exit;
  if FAdoRecordSet.BOF then
    FAdoRecordSet.MoveFirst
  else
    if not FAdoRecordSet.EOF and not FFirstFetch then
      FAdoRecordSet.MoveNext;
  FFirstFetch := False;
  Result := not FAdoRecordSet.EOF;
  RowNo := RowNo +1;
  if Result then
    LastRowNo := RowNo;
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
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
     FAdoRecordSet.MoveFirst;
  if Row > 0 then
    FAdoRecordSet.Move(Row - 1, adBookmarkFirst)
  else
    FAdoRecordSet.Move(Abs(Row) - 1, adBookmarkLast);
  Result := not (FAdoRecordSet.EOF or FAdoRecordSet.BOF);
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAdoResultSet.GetRow: NativeInt;
begin
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
    Result := -1
  else
    Result := FAdoRecordSet.AbsolutePosition;
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
  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
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
var
  P: PWidechar;
  Len: NativeUInt;
Label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
     Result := ''
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           Result := {$IFDEF UNICODE}FloatToSQLUnicode{$ELSE}FloatToSQLRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := {$IFDEF UNICODE}FloatToSQLUnicode{$ELSE}FloatToSQLRaw{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean: Result := {$IFDEF UNICODE}BoolToUnicodeEx{$ELSE}BoolToRawEx{$ENDIF}(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID:
        {$IFDEF UNICODE}
        System.SetString(Result, TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38);
        {$ELSE}
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38, ConSettings^.CTRL_CP);
        {$ENDIF}
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VString;
          while (P+Len-1)^ = ' ' do dec(Len);
          {$IFDEF UNICODE}
          System.SetString(Result, P, Len);
          {$ELSE}
          Result := PUnicodeToRaw(P, Len, ConSettings^.CTRL_CP);
          {$ENDIF}
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        {$IFDEF UNICODE}
        System.SetString(Result,
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
        {$ELSE}
          Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize, ConSettings^.CTRL_CP);
        {$ENDIF}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        {$IFDEF UNICODE}
        System.SetString(Result,
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1); //shr 1 = div 2 but faster, OleDb returns size in Bytes!
        {$ELSE}
          Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1, //shr 1 = div 2 but faster, OleDb returns size in Bytes!
            ConSettings^.CTRL_CP);
        {$ENDIF}
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := '';
        end;
    end;
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
var
  P: PWideChar;
  Len: NativeUInt;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
     Result := ''
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := {$IFDEF UNICODE}AnsiString{$ENDIF}(CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency));
      adBoolean:          Result := BoolToRawEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID:
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38, ZOSCodePage);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          Result := PUnicodeToRaw(P, Len, ZOSCodePage);
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize, ZOSCodePage);
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1, //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          ZOSCodePage);
      else
        try
          Result := AnsiString(FAdoRecordSet.Fields.Item[ColumnIndex].Value);
        except
          Result := '';
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
var
  P: PWideChar;
  Len: NativeUInt;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
     Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := {$IFDEF UNICODE}UTF8String{$ENDIF}(CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency));
      adBoolean:          Result := BoolToRawEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID:
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38, zCP_US_ASCII);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          Result := PUnicodeToRaw(P, Len, zCP_UTF8);
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize, zCP_UTF8);
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        begin
          Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1, //shr 1 = div 2 but faster, OleDb returns size in Bytes!
            zCP_UTF8);
        end;
      else
        try
          {$IFDEF FPC}
          Result := UTF8String(WideString(FAdoRecordSet.Fields.Item[ColumnIndex].Value));
          {$ELSE}
          Result := UTF8String(FAdoRecordSet.Fields.Item[ColumnIndex].Value);
          {$ENDIF}
        except
          Result := '';
        end;
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
var
  P: PWideChar;
  Len: NativeUInt;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
     Result := ''
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   Result := IntToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := FloatToSQLRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := {$IFDEF UNICODE}AnsiString{$ENDIF}(CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency));
      adBoolean:          Result := BoolToRawEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID:
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38, zCP_US_ASCII);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          Result := PUnicodeToRaw(P, Len, GetACP);
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize, GetACP);
      adVarWChar,
      adLongVarWChar: {varying char fields}
          Result := PUnicodeToRaw(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1, //shr 1 = div 2 but faster, OleDb returns size in Bytes!
              GetACP);
      else
        try
          {$IFDEF FPC}
          Result := RawByteString(WideString(FAdoRecordSet.Fields.Item[ColumnIndex].Value));
          {$ELSE}
          Result := RawByteString(FAdoRecordSet.Fields.Item[ColumnIndex].Value);
          {$ENDIF}
        except
          Result := '';
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
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   FUniTemp := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           FUniTemp := FloatToSQLUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           FUniTemp := FloatToSQLUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         FUniTemp := {$IFNDEF UNICODE}ZWideString{$ENDIF}(CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency));
      adBoolean: FUniTemp := BoolToUnicodeEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID:
        begin
          Len := 38;
          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          Exit;
        end;
      adChar:
        begin
          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          while (Result+Len-1)^ = ' ' do dec(Len);
          Exit;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (Result+Len-1)^ = ' ' do dec(Len);
          Exit;
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        begin
          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          Exit;
        end;
      adVarWChar,
      adLongVarWChar: {varying char fields}
        begin
          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          Exit;
        end;
      else
        try
          FUniTemp := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
        end;
    end;
    Len := Length(FUniTemp);
    Result := Pointer(FUniTemp);
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
var
  L: LengthInt;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := ''
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt);
      adSmallInt:         Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt);
      adInteger, adError: Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger);
      adBigInt:           Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      adUnsignedTinyInt:  Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte);
      adUnsignedSmallInt: Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord);
      adUnsignedInt:      Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord);
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64);
      {$ELSE}
      adUnsignedBigInt:   Result := IntToUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64);
      {$ENDIF}
      adSingle:           Result := FloatToSQLUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := FloatToSQLUnicode(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := ZWideString(CurrToStr(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency));
      adBoolean: Result := BoolToUnicodeEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adGUID: System.SetString(Result, TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38);
      adChar:
        System.SetString(Result, TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
      adWChar: {fixed char fields}
        begin
          L := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          while (TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr+L-1)^ = ' ' do dec(L);
          System.SetString(Result, TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, L);
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        System.SetString(Result,
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
      adVarWChar,
      adLongVarWChar: {varying char fields}
        System.SetString(Result,
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1); //shr 1 = div 2 but faster, OleDb returns size in Bytes!
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := '';
        end;
    end;
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
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := False
   else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt <> 0;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt  <> 0;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger  <> 0;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64  <> 0;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte <> 0;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord  <> 0;
      adUnsignedInt:      Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord <> 0;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64 <> 0;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64 <> 0;
      {$ENDIF}
      adSingle:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle <> 0;
      adDouble:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble <> 0;
      adCurrency:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency <> 0;
      adBoolean:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean;
      adGUID:             Result := False;
      adChar,
      adWChar: {fixed char fields}
                          Result := StrToBoolEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr);
      adVarChar,
      adLongVarChar,
      adVarWChar,
      adLongVarWChar: {varying char fields}
                          Result := StrToBoolEx(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, True, False);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := False;
        end;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetByte(ColumnIndex: Integer): Byte;
var
  Len: NativeUInt;
  P: PWideChar;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt:      Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean:          Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          System.SetString(FUniTemp, P, Len);
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      adBSTR,
      adVarChar,
      adLongVarChar: {varying char fields}
        begin
          System.SetString(FUniTemp,
            TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      adVarWChar,
      adLongVarWChar: {varying char fields}
        begin
          System.SetString(FUniTemp,
            TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1); //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
        end;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt:      Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean:          Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          System.SetString(FUniTemp, P, Len);
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      adVarChar,
      adLongVarChar: {varying char fields}
        begin
          System.SetString(FUniTemp,
            TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        begin
          System.SetString(FUniTemp,
            TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1); //shr 1 = div 2 but faster, OleDb returns size in Bytes!
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
        end;
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
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {ADO uses its own DataType-mapping different to System Variant type mapping}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt:      Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean:          Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          System.SetString(FUniTemp, P, Len);
          Result := UnicodeToIntDef(FUniTemp, 0);
        end;
      adBSTR,
      adVarChar,
      adLongVarChar, {varying char fields}
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToIntDef(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
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
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adEmpty:            Result := 0;
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt:      Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle: Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble: Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency: Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean: Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
  ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          System.SetString(FUniTemp, P, Len);
          Result := UnicodeToInt64Def(FUniTemp, 0);
        end;
      adVarChar,
      adLongVarChar, {varying char fields}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToInt64Def(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
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
function TZAdoResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adEmpty: Result := 0;
      adTinyInt:          Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt:         Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt:           Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt:  Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle);
      adDouble:           Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble);
      adCurrency:         Result := Trunc(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency);
      adBoolean:          Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
  ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          System.SetString(FUniTemp, P, Len);
          Result := UnicodeToUInt64Def(FUniTemp, 0);
        end;
      adVarChar,
      adLongVarChar, {varying char fields}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToUInt64Def(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
        end;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle;
      adDouble: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble;
      adCurrency: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency;
      adBoolean: Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adDate,
      adDBDate,
      adDBTime,
      adDBTimeStamp: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDate;
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
  ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
        end;
      adVarChar,
      adLongVarChar, {varying char fields}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToFloatDef(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, WideChar('.'), 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
        end;
    end;
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
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle;
      adDouble: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble;
      adCurrency: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency;
      adBoolean: Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adDate,
      adDBDate,
      adDBTime,
      adDBTimeStamp: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDate;
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
  ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
        end;
      adVarChar,
      adLongVarChar, {varying char fields}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToFloatDef(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, WideChar('.'), 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
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
function TZAdoResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  Len: NativeUint;
  P: PWideChar;
label ProcessFixedChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VShortInt;
      adSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSmallInt;
      adInteger, adError: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInteger;
      adBigInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      adUnsignedTinyInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VByte;
      adUnsignedSmallInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VWord;
      adUnsignedInt: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VLongWord;
      {$IFDEF WITH_VARIANT_UINT64}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VUInt64;
      {$ELSE}
      adUnsignedBigInt:   Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VInt64;
      {$ENDIF}
      adSingle: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VSingle;
      adDouble: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDouble;
      adCurrency: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VCurrency;
      adBoolean: Result := Ord(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VBoolean);
      adDate,
      adDBDate,
      adDBTime,
      adDBTimeStamp: Result := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VDate;
      adChar:
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          goto ProcessFixedChar;
        end;
      adWChar: {fixed char fields}
        begin
          Len := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1; //shr 1 = div 2 but faster, OleDb returns size in Bytes!
  ProcessFixedChar:
          P := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr;
          while (P+Len-1)^ = ' ' do dec(Len);
          ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
        end;
      adVarChar,
      adLongVarChar, {varying char fields}
      adBSTR,
      adVarWChar,
      adLongVarWChar: {varying char fields}
        Result := UnicodeToFloatDef(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, WideChar('.'), 0);
      else
        try
          Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
        except
          Result := 0;
        end;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  V: Variant;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := nil
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adGUID:
        begin
          SetLength(Result, 16);
          ValidGUIDToBinary(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, Pointer(Result));
        end;
      adBinary,
      adVarBinary,
      adLongVarBinary:
        begin
          Result := BufferToBytes(
            TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VArray.Data,
            FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
        end;
      else
        begin
          V := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
          try
            Result := V
          except
            Result := nil;
          end;
        end;
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
function TZAdoResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    try
      Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
    except
      Result := 0;
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
function TZAdoResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    try
      Result := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
    except
      Result := 0;
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
function TZAdoResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var V: Variant;
Failed: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := 0
  else
    try
      V := FAdoRecordSet.Fields.Item[ColumnIndex].Value;
      if VarIsStr(V) then
        Result := UnicodeSQLTimeStampToDateTime(PWideChar(ZWideString(V)),
          Length(V), ConSettings^.ReadFormatSettings, Failed{%H-})
      else
        Result := V;
    except
      Result := 0;
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
function TZAdoResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VType in [varNull, varEmpty];
  if LastWasNull then
    Result := nil
  else
    case FAdoRecordSet.Fields.Item[ColumnIndex].Type_ of
      adGUID:
        Result := TZAbstractClob.CreateWithData(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, 38, ConSettings);
      adChar: { fixed char columns }
        begin
          L := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize;
          while (TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr+L-1)^ = ' ' do dec(L);
          Result := TZAbstractClob.CreateWithData( TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, L, ConSettings);
        end;
      adWChar: { fixed wchar columns }
        begin
          L := FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1;
          while (TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr+L-1)^ = ' ' do dec(L);
          Result := TZAbstractClob.CreateWithData( TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr, L, ConSettings);
        end;
      adVarChar,
      adLongVarChar:
        Result := TZAbstractClob.CreateWithData(
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize, ConSettings);
      adBSTR, adVarWChar,
      adLongVarWChar:
        Result := TZAbstractClob.CreateWithData(
          TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VOleStr,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize shr 1, ConSettings);
      adBinary,
      adVarBinary,
      adLongVarBinary:
        Result := TZAbstractBlob.CreateWithData(TVarData(FAdoRecordSet.Fields.Item[ColumnIndex].Value).VArray.Data,
          FAdoRecordSet.Fields.Item[ColumnIndex].ActualSize);
      else
        Result := nil;
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
procedure TZAdoCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
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

{$ELSE}
implementation
{$ENDIF ENABLE_ADO}

end.



