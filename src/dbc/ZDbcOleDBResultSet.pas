{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcOleDBResultSet;

interface

{$I ZDbc.inc}
{.$IFDEF ENABLE_OLEDB}

uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF OLD_FPC}ZClasses, {$ENDIF}ZSysUtils, ZDbcIntfs, ZDbcGenericResolver,
  ZOleDB, ZDbcOleDBUtils,
  ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet, ZDbcResultsetMetadata, ZCompatibility;

type
  IZOleDBResultSet = Interface(IZResultSet)
    ['{7824F384-D4CB-479A-ABF7-87A64B8FD76D}']
    procedure RestartPosition;
  End;
  {** Implements Ado ResultSet. }
  TZOleDBResultSet = class(TZAbstractResultSet, IZOleDBResultSet)
  private
    FEnhancedColInfo: Boolean;
    FRowSet: IRowSet;
    FZBufferSize: Integer;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FRowSize: NativeInt;
    FAccessor:HACCESSOR;
    FRowCount: DBROWCOUNT;
    FCurrentBufRowNo: DBROWOFFSET;
    FRowsObtained: DBCOUNTITEM;
    FHROWS: PHROWS_Array;
    FColBuffer: TByteDynArray;
  protected
    procedure Open; override;
    procedure RestartPosition;
  public
    constructor Create(Statement: IZStatement; SQL: string; RowSet: IRowSet;
      ZBufferSize: Integer; const EnhancedColInfo: Boolean = True);
    procedure Close; override;
    procedure ResetCursor; override;
    function Next: Boolean; override;
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
  TZOleDBMSSQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
    FDBParams: TDBParams;
    FRowCount: DBROWCOUNT;
    FResultSet: IZOleDBResultSet;
  public
    constructor Create(Statement: IZStatement; Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

function GetCurrentResultSet(RowSet: IRowSet; Statement: IZStatement;
  Const SQL: String; ConSettings: PZConSettings; BuffSize: Integer;
  EnhancedColInfo: Boolean; var PCurrRS: Pointer): IZResultSet;

{.$ENDIF ENABLE_OLEDB}
implementation
{.$IFDEF ENABLE_OLEDB}

uses
  Variants, Math,
  ZDbcOleDB, ZMessages, ZEncoding, ZFastCode;

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZOleDBResultSet.Create(Statement: IZStatement; SQL: string;
  RowSet: IRowSet; ZBufferSize: Integer; const EnhancedColInfo: Boolean = True);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);
  FRowSet := RowSet;
  FZBufferSize := ZBufferSize;
  FEnhancedColInfo := EnhancedColInfo;
  FAccessor := 0;
  FCurrentBufRowNo := 0;
  FRowsObtained := 0;
  Open;
end;

{**
  Opens this recordset and initializes the Column information.
}
procedure TZOleDBResultSet.Open;
var
  OleDBColumnsInfo: IColumnsInfo;
  pcColumns: DBORDINAL;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
  //HasAutoIncProp: Boolean;
  S: string;
begin
//Check if the current statement can return rows
  if not Assigned(FRowSet) {or (FAdoRecordSet.State = adStateClosed) }then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  FRowSet.QueryInterface(IColumnsInfo, OleDBColumnsInfo);
  OleDBColumnsInfo.GetColumnInfo(pcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo; //save pointer for Malloc.Free
  try
    SetLength(FDBBINDSTATUSArray, pcColumns);
    FRowSize := PrepareOleColumnDBBindings(pcColumns, FDBBindingArray, prgInfo);
    FRowCount := Max(1, FZBufferSize div NativeInt(FRowSize));
    if (MaxRows > 0) and (FRowCount > MaxRows) then
      FRowCount := MaxRows; //fetch only wanted count of rows
    OleDBCheck((FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA or DBACCESSOR_OPTIMIZED,
      pcColumns, Pointer(FDBBindingArray), FRowSize, @FAccessor,
      Pointer(FDBBINDSTATUSArray)), FDBBINDSTATUSArray);


    { Fills the column info }
    ColumnsInfo.Clear;
    {HasAutoIncProp := False;
    if AdoColumnCount > 0 then
      for I := 0 to FAdoRecordSet.Fields.Item[0].Properties.Count - 1 do
        if FAdoRecordSet.Fields.Item[0].Properties.Item[I].Name = 'ISAUTOINCREMENT' then
        begin
          HasAutoIncProp := True;
          Break;
        end;}

    if Assigned(prgInfo) then
      if prgInfo.iOrdinal = 0 then // skip possible bookmark column
        Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));

    for I := prgInfo.iOrdinal-1 to pcColumns-1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      if (prgInfo^.pwszName=nil) or (prgInfo^.pwszName^=#0) then
        ColumnInfo.ColumnLabel := 'col_'+ZFastCode.IntToStr(i)
      else
        ColumnInfo.ColumnLabel := String(prgInfo^.pwszName);
      ColumnInfo.ColumnName := ColumnInfo.ColumnLabel;
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(prgInfo^.wType,
        ConSettings.CPType, (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) <> 0);

      FieldSize := prgInfo^.ulColumnSize;
      if FieldSize < 0 then
        FieldSize := 0;
      if ColumnInfo.ColumnType = stGUID then
        ColumnInfo.ColumnDisplaySize := 38
      else
        ColumnInfo.ColumnDisplaySize := FieldSize;
      ColumnInfo.Precision := FieldSize;
      ColumnInfo.Currency := ColumnInfo.ColumnType = stCurrency;
      S := '';
      {for J := 0 to F.Properties.Count - 1 do
        S := S+F.Properties.Item[J].Name + '=' + VarToStr(F.Properties.Item[J].Value) + ', ';
      if HasAutoIncProp then
        ColumnInfo.AutoIncrement := F.Properties.Item['ISAUTOINCREMENT'].Value;}

      ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stDouble, stBigDecimal];
      ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0);// and (F.Properties.Item['BASECOLUMNNAME'].Value <> null) and not ColumnInfo.AutoIncrement;
      ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0);// or ColumnInfo.AutoIncrement;
      ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
      ColumnsInfo.Add(ColumnInfo);
      Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
    end;
  finally
    if Assigned(ppStringsBuffer) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(ppStringsBuffer);
    if Assigned(OriginalprgInfo) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(OriginalprgInfo);
  end;
  inherited Open;
end;

procedure TZOleDBResultSet.RestartPosition;
begin
  if RowNo > 0 then
  begin
    {release old rows}
    OleDBCheck(fRowSet.ReleaseRows(FRowsObtained,FHROWS,nil,nil,nil));
    FRowSet.RestartPosition(DB_NULL_HCHAPTER)
  end;
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
procedure TZOleDBResultSet.Close;
var
  FAccessorRefCount: DBREFCOUNT;
begin
  if Assigned(fRowSet) then
    OleDBCheck(fRowSet.ReleaseRows(FRowsObtained,FHROWS,nil,nil,nil));
  if FAccessor > 0 then
    OleDBCheck((fRowSet As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount));

  FRowSet := nil;
  FAccessor := 0;
  inherited Close;
end;

procedure TZOleDBResultSet.ResetCursor;
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
function TZOleDBResultSet.Next: Boolean;
var I: NativeInt;
label Success, NoSuccess;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (FRowSet = nil) then
    Exit;

  if (RowNo = 0) then //fetch Iteration count of rows
  begin
    OleDBCheck(fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS));
    if FRowsObtained > 0 then
    begin
      if DBROWCOUNT(FRowsObtained) < FRowCount then
      begin //reserve required mem only
        SetLength(FColBuffer, NativeInt(FRowsObtained) * FRowSize);
        MaxRows := FRowsObtained;
      end
      else //reserve full allowed mem
        SetLength(FColBuffer, FRowCount * FRowSize);
      {fetch data into the buffer}
      for i := 0 to FRowsObtained -1 do
        OleDBCheck(fRowSet.GetData(FHROWS[i], FAccessor, @FColBuffer[I*FRowSize]));
      goto success;
    end
    else //we do NOT need a buffer here!
      goto NoSuccess;
  end
  else
    if FCurrentBufRowNo < DBROWCOUNT(FRowsObtained) then
    begin
      Inc(FCurrentBufRowNo);
      goto Success;
    end
    else
    begin
      {release old rows}
      OleDBCheck(fRowSet.ReleaseRows(FRowsObtained,FHROWS,nil,nil,nil));
      OleDBCheck(fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS));
      if DBROWCOUNT(FRowsObtained) < FCurrentBufRowNo then
        MaxRows := RowNo+Integer(FRowsObtained);  //this makes Exit out in first check on next fetch
      FCurrentBufRowNo := 0; //reset Buffer offsett
      if FRowsObtained > 0 then
      begin
        {fetch data into the buffer}
        for i := 0 to FRowsObtained -1 do
          OleDBCheck(fRowSet.GetData(FHROWS[i], FAccessor, @FColBuffer[I*FRowSize]));
        goto Success;
      end else goto NoSuccess;
    end;

Success:
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
    Exit;
NoSuccess:
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZOleDBResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(FRowSize*FCurrentBufRowNo)])^ = DBSTATUS_S_ISNULL;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetString(ColumnIndex: Integer): String;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if LastWasNull then
    Result := ''
  else
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_EMPTY:     Result := '';
      DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToStr(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        Result := ZFastCode.IntToStr(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        Result := FloatToSQLStr(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := FloatToSQLStr(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLTime{$ELSE}DateTimeToRawSQLTime{$ENDIF}(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLDate{$ELSE}DateTimeToRawSQLDate{$ENDIF}(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        {$IFDEF UNICODE}
        System.SetString(Result, PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      DBTYPE_BSTR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToStr(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      Result := {$IFDEF UNICODE}BoolToUnicodeEx{$ELSE}BoolToRawEx{$ENDIF}(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToStr(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        Result := ZFastCode.IntToStr(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       Result := ZFastCode.IntToStr(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       Result := ZFastCode.IntToStr(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        Result := ZFastCode.IntToStr(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       Result := ZFastCode.IntToStr(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      Result := {$IFDEF UNICODE}GuidToUnicode{$ELSE}GuidToRaw{$ENDIF}(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        {$IFDEF UNICODE}
          Result := PRawToUnicode(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
        {$ELSE}
          System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        {$ENDIF}
      DBTYPE_STR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
          Result := PRawToUnicode(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
        {$ELSE}
          System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        {$ENDIF}
      DBTYPE_WSTR:
        {$IFDEF UNICODE}
        System.SetString(Result, PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      DBTYPE_WSTR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToStr(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
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
function TZOleDBResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if LastWasNull then
    Result := ''
  else
    case FDBBindingArray[ColumnIndex].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := AnsiString(CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^));
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := DateTimeToRawSQLTime(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: Result := DateTimeToRawSQLDate(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            Result := DateTimeToRawSQLTimeStamp(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, GetACP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, GetACP);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   Result := AnsiString(POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, GetACP);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, GetACP);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
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
function TZOleDBResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if LastWasNull then
    Result := ''
  else
    case FDBBindingArray[ColumnIndex].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := UTF8String(CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^));
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := DateTimeToRawSQLTime(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: Result := DateTimeToRawSQLDate(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            Result := DateTimeToRawSQLTimeStamp(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, zCP_UTF8);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(PPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, zCP_UTF8);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   Result := UTF8String(POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, zCP_UTF8);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, zCP_UTF8);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
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
function TZOleDBResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if LastWasNull then
    Result := ''
  else
    case FDBBindingArray[ColumnIndex].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := RawByteString(CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^));
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := DateTimeToRawSQLTime(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: Result := DateTimeToRawSQLDate(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            Result := DateTimeToRawSQLTimeStamp(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   Result := RawByteString(POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
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
function TZOleDBResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  Len := 0;
  if LastWasNull then
    Result := nil
  else
  begin
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        FUniTemp := ZFastCode.IntToUnicode(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        FUniTemp := ZFastCode.IntToUnicode(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        FUniTemp := FloatToSQLUnicode(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        FUniTemp := FloatToSQLUnicode(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        FUniTemp := CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: FUniTemp := DateTimeToUnicodeSQLTime(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: FUniTemp := DateTimeToUnicodeSQLDate(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            FUniTemp := DateTimeToUnicodeSQLTimeStamp(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        begin
          Result := PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]);
          Len := PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1;
          Exit;
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        begin
          Result := ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
          Len := PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1;
          Exit;
        end;
      DBTYPE_ERROR:     FUniTemp := ZFastCode.IntToUnicode(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      FUniTemp := BoolToUnicodeEx(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   FUniTemp := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI1:       FUniTemp := ZFastCode.IntToUnicode(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        FUniTemp := ZFastCode.IntToUnicode(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       FUniTemp := ZFastCode.IntToUnicode(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       FUniTemp := ZFastCode.IntToUnicode(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        FUniTemp := ZFastCode.IntToUnicode(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       FUniTemp := ZFastCode.IntToUnicode(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      FUniTemp := GuidToUnicode(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR:
          FUniTemp := PRawToUnicode(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
      DBTYPE_STR or DBTYPE_BYREF:
          FUniTemp := PRawToUnicode(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR:
        begin
          Result := PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]);
          Len := PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1;
          Exit;
        end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        begin
          Result := ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
          Len := PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1;
          Exit;
        end;
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  FUniTemp := ZFastCode.IntToUnicode(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else FUniTemp := '';
    end;
    Len := Length(FUniTemp);
    if Len = 0 then
      Result := PEmptyUnicodeString
    else
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
function TZOleDBResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if LastWasNull then
    Result := ''
  else
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_EMPTY:     Result := '';
      DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToUnicode(PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I4:        Result := ZFastCode.IntToUnicode(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R4:        Result := FloatToSQLUnicode(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := FloatToSQLUnicode(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := CurrToStr(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := DateTimeToUnicodeSQLTime(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          stDate: Result := DateTimeToUnicodeSQLDate(
            PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
              ConSettings.ReadFormatSettings, False);
          else
            Result := DateTimeToUnicodeSQLTimeStamp(
              PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
                ConSettings.ReadFormatSettings, False);
        end;
      DBTYPE_BSTR:
        System.SetString(Result, PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
      DBTYPE_BSTR or DBTYPE_BYREF:
        System.SetString(Result, ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToUnicode(PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BOOL:      Result := BoolToUnicodeEx(PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToUnicode(PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I1:        Result := ZFastCode.IntToUnicode(PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI2:       Result := ZFastCode.IntToUnicode(PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI4:       Result := ZFastCode.IntToUnicode(PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_I8:        Result := ZFastCode.IntToUnicode(PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_UI8:       Result := ZFastCode.IntToUnicode(PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_GUID:      Result := GuidToUnicode(PGUID(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
          Result := PRawToUnicode(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
      DBTYPE_STR or DBTYPE_BYREF:
          Result := PRawToUnicode(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
            PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR:
        System.SetString(Result, PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
      DBTYPE_WSTR or DBTYPE_BYREF:
        System.SetString(Result, ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToUnicode(PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
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
function TZOleDBResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := False;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_R4:        Result := PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^  <> 0;
      DBTYPE_R8:        Result := PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_CY:        Result := PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_DATE:      Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_BSTR:
        Result := StrToBoolEx(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]));
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := StrToBoolEx(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_BOOL:      Result := PWordBool(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
      DBTYPE_STR:
        Result := StrToBoolEx(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]));
      DBTYPE_STR or DBTYPE_BYREF:
        Result := StrToBoolEx(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := StrToBoolEx(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]));
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := StrToBoolEx(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^ <> 0;
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
function TZOleDBResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := Trunc(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := Trunc(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := Trunc(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToIntDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_WSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := Trunc(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := Trunc(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := Trunc(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToIntDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      DBTYPE_WSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := Trunc(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := Trunc(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := Trunc(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToIntDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      DBTYPE_WSTR:
        Result := UnicodeToIntDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := Trunc(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := Trunc(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := Trunc(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := UnicodeToInt64Def(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToInt64Def(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToInt64Def(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToInt64Def(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_WSTR:
        Result := UnicodeToInt64Def(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToInt64Def(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := Trunc(PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_R8:        Result := Trunc(PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_CY:        Result := Trunc(PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := UnicodeToUInt64Def(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToUInt64Def(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToUInt64Def(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToUInt64Def(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      DBTYPE_WSTR:
        Result := UnicodeToUInt64Def(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),0 );
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToUInt64Def(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R8:        Result := PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_CY:        Result := PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_DATE:      Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), WideChar('.'), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'), 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToFloatDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),'.', 0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToFloatDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,'.', 0);
      DBTYPE_WSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),WideChar('.'),0 );
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R8:        Result := PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_CY:        Result := PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_DATE:      Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), WideChar('.'), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'), 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToFloatDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),'.', 0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToFloatDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,'.', 0);
      DBTYPE_WSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),WideChar('.'),0 );
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_I2:        Result := PSmallInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I4:        Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R4:        Result := PSingle(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_R8:        Result := PDouble(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_CY:        Result := PCurrency(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_DATE:      Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), WideChar('.'), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'), 0);
      DBTYPE_ERROR:     Result := PLongInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_BOOL:      Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I1:        Result := PShortInt(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI2:       Result := PWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI4:       Result := PLongWord(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_I8:        Result := PInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_UI8:       Result := PUInt64(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        Result := RawToFloatDef(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),'.', 0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToFloatDef(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,'.', 0);
      DBTYPE_WSTR:
        Result := UnicodeToFloatDef(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),WideChar('.'),0 );
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToFloatDef(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,WideChar('.'),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
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
function TZOleDBResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := nil;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_GUID:
        begin
          SetLength(Result, 16);
          System.Move(Pointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, Pointer(Result)^, 16);
        end;
      DBTYPE_GUID or DBTYPE_BYREF:
        begin
          SetLength(Result, 16);
          System.Move(PPointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^^, Pointer(Result)^, 16);
        end;
      DBTYPE_BYTES:
        begin
          SetLength(Result, PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
          System.Move(Pointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
            Pointer(Result)^, PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        end;
      DBTYPE_BYTES or DBTYPE_BYREF:
        begin
          SetLength(Result, PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
          System.Move(PPointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^^,
            Pointer(Result)^, PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        end;
      else LastWasNull := True;
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
function TZOleDBResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := 0;
          stDate: Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
          else
            Result := Trunc(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        end;
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
*)      else LastWasNull := True;
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
function TZOleDBResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
          stDate: Result := 0;
          else
            Result := Frac(PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
        end;
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
*)      else LastWasNull := True;
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
function TZOleDBResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_DATE:
        Result := PDateTime(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_WSTR:
        Result := PUnicodeToRaw(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings^.ClientCodePage^.CP);
*)      else LastWasNull := True;
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
function TZOleDBResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := nil;
  LastWasNull := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(NativeUInt(FRowSize*FCurrentBufRowNo))])^ = DBSTATUS_S_ISNULL;
  if not LastWasNull then
    case FDBBindingArray[ColumnIndex].wType of
      DBTYPE_GUID:
        Result := TZAbstractBlob.CreateWithData(Pointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]), 16);
      DBTYPE_GUID or DBTYPE_BYREF:
        Result := TZAbstractBlob.CreateWithData(PPointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^, 16);
      DBTYPE_BYTES:
        Result := TZAbstractBlob.CreateWithData(Pointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BYTES or DBTYPE_BYREF:
        Result := TZAbstractBlob.CreateWithData(PPointer(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^);
      DBTYPE_BSTR:
        Result := TZAbstractClob.CreateWithData(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := TZAbstractClob.CreateWithData(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings);
      DBTYPE_STR:
        Result := TZAbstractClob.CreateWithData(PAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP, ConSettings);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := TZAbstractClob.CreateWithData(PPAnsiChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^, ConSettings^.ClientCodePage^.CP, ConSettings);
      DBTYPE_WSTR:
        Result := TZAbstractClob.CreateWithData(PWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]),
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := TZAbstractClob.CreateWithData(ZPPWideChar(@FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)])^,
          PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^ shr 1, ConSettings);
      else LastWasNull := True;
    end;
end;


{ TZOleDBMSSQLCachedResolver }

{**
  Creates a OleDB specific cached resolver object.
}
constructor TZOleDBMSSQLCachedResolver.Create(Statement: IZStatement;
  Metadata: IZResultSetMetadata);
const GetScope = ZWideString('SELECT SCOPE_IDENTITY()');
var
  I: Integer;
  FOlePrepareCommand: ICommandPrepare;
  FCommandText: ICommandText;
  RowSet: IRowSet;
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;

  OleDBCheck((Statement.GetConnection as IZOleDBConnection).GetIDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(FCommandText)));
  if Supports(FCommandText, IID_ICommandPrepare, FOlePrepareCommand) then
  begin
    OleDBCheck(FCommandText.SetCommandText(DBGUID_DEFAULT, PWideChar(GetScope)));
    FOlePrepareCommand.Prepare(0); //unknown count of executions
    (FCommandText as ICommand).Execute(nil, IID_IRowset, FDBParams, @FRowCount, @RowSet);
    if Assigned(RowSet) then
      FResultSet := TZOleDBResultSet.Create(Statement, 'SELECT SCOPE_IDENTITY()', RowSet, 0, False);
  end
  else
    raise EZSQLException.Create('OleCommand does not support Prepared mode!');
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZOleDBMSSQLCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    FResultSet.RestartPosition;
    NewRowAccessor.SetLong(FAutoColumnIndex, FResultSet.GetLong(FAutoColumnIndex));
  end;
end;

function GetCurrentResultSet(RowSet: IRowSet; Statement: IZStatement;
  Const SQL: String; ConSettings: PZConSettings; BuffSize: Integer;
  EnhancedColInfo: Boolean; var PCurrRS: Pointer): IZResultSet;
var
  NativeResultSet: IZResultSet;
begin
  Result := nil;
  if Assigned(RowSet) then
    //if (AdoRecordSet.State and adStateOpen) = adStateOpen then
    begin
      NativeResultSet := TZOleDBResultSet.Create(Statement, SQL, RowSet,
        BuffSize, EnhancedColInfo);
      if (Statement.GetResultSetConcurrency = rcUpdatable) or
         (Statement.GetResultSetType <> rtForwardOnly) then
        Result := TZCachedResultSet.Create(NativeResultSet, SQL,
          TZOleDBMSSQLCachedResolver.Create(Statement,
            NativeResultSet.GetMetaData), ConSettings)
      else
        Result := NativeResultSet;
    end;
  PCurrRS := Pointer(Result);
end;
{.$ENDIF ENABLE_OLEDB}
end.



