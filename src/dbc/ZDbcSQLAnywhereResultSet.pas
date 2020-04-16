{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
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

unit ZDbcSQLAnywhereResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}

uses FmtBCD, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
  System.Types, System.Contnrs
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} Types
  {$ENDIF},
  ZPlainSQLAnywhere, ZCompatibility, ZDbcCache, ZClasses,
  ZDbcIntfs, ZDbcResultSet, ZDbcCachedResultSet, ZDbcResultSetMetadata,
  ZDbcSQLAnywhere;

type
  TZSQLAnywhereColumnInfo = class(TZColumnInfo)
  public
    NativeType: Ta_sqlany_native_type;
    Bind: Pa_sqlany_data_valueV4up;
    IsBound: Boolean;
  end;

  {** Implements SQL Anywhere ResultSet. }
  TZSQLAnywhereResultSet = class(TZAbstractReadOnlyResultSet_A, IZResultSet)
  private
    Fa_sqlany_stmt: Pa_sqlany_stmt;
    FPLainDriver: TZSQLAnywherePlainDriver;
    FSQLAnyConnection: IZSQLAnywhereConnection;
    FClientCP, FRawStrCP: Word;
    Fnum_cols: Tsacapi_i32; //kept for index check
    FZBufferSize: Cardinal; //max size for multiple rows. If Row > Value ignore it!
    Fdata_info: Pa_sqlany_data_info; //used for the LOB's
    FCurrentRowOffset, FIteration: Cardinal; //in array value bindings we need an offset
    FData: Pointer; //just a temporary buffer
    FDataValues: Pa_sqlany_data_valueArray; //just one of both is used
    FDataValuesV4up: Pa_sqlany_data_valueV4upArray; //just one of both is used
    FNullArray: Psacapi_i32Array; //one buffer for all null indicator
    FColumnData: Pointer; //one buffer for all column data fields
    procedure CheckRange(const Index: Integer);
    function FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer): Boolean;
    function CreateConversionError(Index: Integer): EZSQLException;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      a_sqlany_stmt: Pa_sqlany_stmt);
    destructor Destroy; override;

    procedure AfterClose; override;
    procedure ResetCursor; override;
  protected
    procedure Open; override;
  public //Getter implementation
    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
    function Next: Boolean; reintroduce;
  end;

  { TZSQLAnywhereCachedResultSet }

  TZSQLAnywhereCachedResultSet = class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZSQLAnywhereRowAccessor }

  TZSQLAnywhereRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
    procedure FetchLongData(AsStreamedType: TZSQLType; const ResultSet: IZResultSet;
      ColumnIndex: Integer; Data: PPZVarLenData); override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses SysUtils, TypInfo,
  ZFastCode, ZSysUtils, ZEncoding,  ZMessages, ZDbcLogging, ZDbcUtils,
  ZDbcSQLAnywhereUtils, ZDbcProperties;

{ TZSQLAnywhereResultSet }

procedure TZSQLAnywhereResultSet.AfterClose;
begin
  inherited;
  if (FColumnData <> nil) then begin
    FreeMem(FColumnData);
    FColumnData := nil;
  end;
  if (FNullArray <> nil) then begin
    FreeMem(FNullArray);
    FNullArray := nil;
  end;
  if (Fdata_info <> nil) then begin
    FreeMem(Fdata_info);
    Fdata_info := nil;
  end;
  if (FDataValues <> nil) then begin
    FreeMem(FDataValues);
    FDataValues := nil;
  end;
end;

procedure TZSQLAnywhereResultSet.CheckRange(const Index: Integer);
begin

end;

constructor TZSQLAnywhereResultSet.Create(const Statement: IZStatement;
  const SQL: string; a_sqlany_stmt: Pa_sqlany_stmt);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConSettings);
  Fa_sqlany_stmt := a_sqlany_stmt;
  FSQLAnyConnection := Statement.GetConnection as IZSQLAnywhereConnection;
  FPlainDriver := FSQLAnyConnection.GetPlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FClientCP := ConSettings.ClientCodePage.CP;
  FRawStrCP := ConSettings.CTRL_CP;
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Statement, DSProps_InternalBufSize, ''), 131072);
  Open;
end;

function TZSQLAnywhereResultSet.CreateConversionError(
  Index: Integer): EZSQLException;
begin
  with TZSQLAnywhereColumnInfo(ColumnsInfo[Index]) do
    Result := EZSQLException.Create(Format(SErrorConvertionField,
      [ColumnLabel, TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(ColumnType))]));
end;

destructor TZSQLAnywhereResultSet.Destroy;
begin
  inherited;
end;

function TZSQLAnywhereResultSet.FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer): Boolean;
var ABind: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  {$IFDEF DEBUG}
  Assert((Index >= 0) and (Index < Fnum_cols), 'Out of Range.');
  {$ENDIF}
  with TZSQLAnywhereColumnInfo(ColumnsInfo[Index]) do begin
    ABind := Pa_sqlany_data_value(Bind);
    if IsBound then begin
      {$R-}
      Result := Bind.is_null[FCurrentRowOffset] = 0;
      FData := ABind.buffer+(FCurrentRowOffset*Bind.buffer_size);
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end else if (Ord(ColumnType) >= Ord(stAsciiStream)) then begin
      if FplainDriver.sqlany_get_data_info(Fa_sqlany_stmt, Index, Fdata_info) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data_info', Self);
      Result := Fdata_info.is_null = 0;
    end else begin
      if FplainDriver.sqlany_get_column(Fa_sqlany_stmt, Index, Pointer(ABind)) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_column', Self);
      Result := ABind.is_null^ = 0;
      FData :=  ABind.buffer;
    end;
  end;
  LastWasNull := not Result;
end;

procedure TZSQLAnywhereResultSet.GetBigDecimal(ColumnIndex: Integer;
  var Result: TBCD);
begin

end;

function TZSQLAnywhereResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
begin
  CheckRange(ColumnIndex);
end;

function TZSQLAnywhereResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^ <> 0;
      DT_BIT         : Result := PByte(FData)^ <> 0;
      DT_SMALLINT    : Result := PSmallint(FData)^ <> 0;
      DT_UNSSMALLINT : Result := PWord(FData)^ <> 0;
      DT_INT         : Result := PInteger(FData)^ <> 0;
      DT_UNSINT      : Result := PCardinal(FData)^ <> 0;
      DT_BIGINT      : Result := PInt64(FData)^ <> 0;
      DT_UNSBIGINT   : Result := PUInt64(FData)^ <> 0;
      DT_FLOAT       : Result := PSingle(FData)^ <> 0;
      DT_DOUBLE      : Result := PDouble(FData)^ <> 0;
      DT_NVARCHAR,
      DT_VARCHAR     :Result := StrToBoolEx(PAnsiChar(@PZSQLAnyString(FData).data[0]), PAnsiChar(@PZSQLAnyString(FData).data[0])+PZSQLAnyString(FData).length, True);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

function TZSQLAnywhereResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  Result := Pointer(GetPAnsiChar(ColumnIndex, Len));
end;

function TZSQLAnywhereResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := PSingle(FData)^;
      DT_DOUBLE      : Result := PDouble(FData)^;
      DT_NVARCHAR,
      DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZSQLAnyString(FData).data[0]),
        0, Result, PZSQLAnyString(FData).length);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

procedure TZSQLAnywhereResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
begin

end;

function TZSQLAnywhereResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := PSingle(FData)^;
      DT_DOUBLE      : Result := PDouble(FData)^;
      DT_NVARCHAR,
      DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZSQLAnyString(FData).data[0]),
        0, Result, PZSQLAnyString(FData).length);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

function TZSQLAnywhereResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := PSingle(FData)^;
      DT_DOUBLE      : Result := PDouble(FData)^;
      DT_NVARCHAR,
      DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZSQLAnyString(FData).data[0]),
        0, Result, PZSQLAnyString(FData).length);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

procedure TZSQLAnywhereResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
begin

end;

function TZSQLAnywhereResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FData)^);
      DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FData)^);
      DT_NVARCHAR,
      DT_VARCHAR     : Result := RawToIntDef(@PZSQLAnyString(FData).data[0],
        PAnsiChar(@PZSQLAnyString(FData).data[0])+PZSQLAnyString(FData).length, 0);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

function TZSQLAnywhereResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongLong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FData)^);
      DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FData)^);
      DT_NVARCHAR,
      DT_VARCHAR     : Result := RawToInt64Def(@PZSQLAnyString(FData).data[0],
        PAnsiChar(@PZSQLAnyString(FData).data[0])+PZSQLAnyString(FData).length, 0);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length in bytes of the raw String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLAnywhereResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
  procedure FromLob;
  var Lob: IZBlob;
  begin
    Lob := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmRead);
    Result := Lob.GetBuffer(fRawTemp, Len);
  end;
label set_Results;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT    : begin
                        IntToRaw(Cardinal(PByte(FData)^), PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_BIT        : if PByte(FData)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      DT_SMALLINT   : begin
                        IntToRaw(Integer(PSmallInt(FData)^), PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSSMALLINT: begin
                        IntToRaw(Cardinal(PWord(FData)^), PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_INT        : begin
                        IntToRaw(PInteger(FData)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSINT     : begin
                        IntToRaw(PCardinal(FData)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_BIGINT     : begin
                        IntToRaw(PInt64(FData)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSBIGINT  : begin
                        IntToRaw(PUInt64(FData)^, PAnsiChar(@FTinyBuffer[0]), @Result);
set_Results:            Len := Result - PAnsiChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_FLOAT      : begin
                        Len := FloatToSQLRaw(PSingle(FData)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_DOUBLE     : begin
                        Len := FloatToSQLRaw(PDouble(FData)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_VARCHAR,
      DT_NVARCHAR,
      DT_BINARY     : begin
                        Result := @PZSQLAnyString(FData).data[0];
                        Len := PZSQLAnyString(FData).length;
                      end;
      DT_LONGBINARY,
      DT_LONGNVARCHAR,
      DT_LONGVARCHAR: FromLob;
      DT_TIMESTAMP_STRUCT : begin
                      Result := @FTinyBuffer[0];
                      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType of
                        stDate: Len := DateToRaw(PZSQLAnyDateTime(FData).Year,
                                  PZSQLAnyDateTime(FData).Month +1, PZSQLAnyDateTime(FData).Day,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        stTime: Len := TimeToRaw(PZSQLAnyDateTime(FData).Hour,
                                  PZSQLAnyDateTime(FData).Minute, PZSQLAnyDateTime(FData).Second,
                                  PZSQLAnyDateTime(FData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        else    Len := DateTimeToRaw(PZSQLAnyDateTime(FData).Year,
                                  PZSQLAnyDateTime(FData).Month +1, PZSQLAnyDateTime(FData).Day,
                                  PZSQLAnyDateTime(FData).Hour, PZSQLAnyDateTime(FData).Minute,
                                  PZSQLAnyDateTime(FData).Second, PZSQLAnyDateTime(FData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        end;
                      end;
    else begin
        Result := nil;
        Len := 0;
        raise CreateConversionError(ColumnIndex);
      end;
    end;
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of UTF16 string in word count
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLAnywhereResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
  procedure FromLob;
  var Lob: IZBlob;
  begin
    Lob := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, lsmRead);
    Result := Lob.GetPWideChar(fUniTemp, Len);
  end;
label set_Results, set_from_uni;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT    : begin
                        IntToUnicode(Cardinal(PByte(FData)^), PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_BIT        : if PByte(FData)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      DT_SMALLINT   : begin
                        IntToUnicode(Integer(PSmallInt(FData)^), PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSSMALLINT: begin
                        IntToUnicode(Cardinal(PWord(FData)^), PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_INT        : begin
                        IntToUnicode(PInteger(FData)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSINT     : begin
                        IntToUnicode(PCardinal(FData)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_BIGINT     : begin
                        IntToUnicode(PInt64(FData)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end;
      DT_UNSBIGINT  : begin
                        IntToUnicode(PUInt64(FData)^, PWideChar(@FTinyBuffer[0]), @Result);
set_Results:            Len := Result - PWideChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_FLOAT      : begin
                        Len := FloatToSQLUnicode(PSingle(FData)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_DOUBLE     : begin
                        Len := FloatToSQLUnicode(PDouble(FData)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      DT_NVARCHAR,
      DT_VARCHAR    : begin
                        if IsBound then begin
                          Len := PZSQLAnyString(FData).length;
                          Result := @PZSQLAnyString(FData).data[0];
                        end else begin
                          Result := FData;
                          Len := Bind.length[0];
                        end;
                        fUniTemp := PRawToUnicode(PAnsiChar(Result), Len, ColumnCodePage);
                        goto set_from_uni;
                      end;
      DT_BINARY     : begin
                        fUniTemp := Ascii7ToUnicodeString(@PZSQLAnyString(FData).data[0],
                          PZSQLAnyString(FData).length);
set_from_uni:           Len := Length(FUniTemp);
                        if Len = 0
                        then Result := PEmptyUnicodeString
                        else Result := Pointer(FUniTemp);
                      end;
      DT_LONGNVARCHAR,
      DT_LONGVARCHAR: FromLob;
      DT_TIMESTAMP_STRUCT : begin
                      Result := @FTinyBuffer[0];
                      case ColumnType of
                        stDate: Len := DateToUni(Abs(PZSQLAnyDateTime(FData).Year),
                                  PZSQLAnyDateTime(FData).Month +1, PZSQLAnyDateTime(FData).Day,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, PZSQLAnyDateTime(FData).Year < 0);
                        stTime: Len := TimeToUni(PZSQLAnyDateTime(FData).Hour,
                                  PZSQLAnyDateTime(FData).Minute, PZSQLAnyDateTime(FData).Second,
                                  PZSQLAnyDateTime(FData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        else    Len := DateTimeToUni(Abs(PZSQLAnyDateTime(FData).Year),
                                  PZSQLAnyDateTime(FData).Month +1, PZSQLAnyDateTime(FData).Day,
                                  PZSQLAnyDateTime(FData).Hour, PZSQLAnyDateTime(FData).Minute,
                                  PZSQLAnyDateTime(FData).Second, PZSQLAnyDateTime(FData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, PZSQLAnyDateTime(FData).Year < 0);
                        end;
                      end;

    else begin
        Result := nil;
        Len := 0;
        raise CreateConversionError(ColumnIndex);
      end;
    end;
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

procedure TZSQLAnywhereResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
begin

end;

procedure TZSQLAnywhereResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
begin

end;

function TZSQLAnywhereResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FData)^);
      DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FData)^);
      DT_NVARCHAR,
      DT_VARCHAR     : Result := RawToUInt64Def(@PZSQLAnyString(FData).data[0],
        PAnsiChar(@PZSQLAnyString(FData).data[0])+PZSQLAnyString(FData).length, 0);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

function TZSQLAnywhereResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if FillData(ColumnIndex) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    case NativeType of
      DT_TINYINT     : Result := PShortInt(FData)^;
      DT_BIT         : Result := PByte(FData)^;
      DT_SMALLINT    : Result := PSmallint(FData)^;
      DT_UNSSMALLINT : Result := PWord(FData)^;
      DT_INT         : Result := PInteger(FData)^;
      DT_UNSINT      : Result := PCardinal(FData)^;
      DT_BIGINT      : Result := PInt64(FData)^;
      DT_UNSBIGINT   : Result := PUInt64(FData)^;
      DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FData)^);
      DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FData)^);
      DT_NVARCHAR,
      DT_VARCHAR     : Result := RawToUInt64Def(@PZSQLAnyString(FData).data[0],
        PAnsiChar(@PZSQLAnyString(FData).data[0])+PZSQLAnyString(FData).length, 0);
      else raise CreateConversionError(ColumnIndex);
    end;
  end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZSQLAnywhereResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Assert((ColumnIndex >= 0) and (ColumnIndex < Fnum_cols), 'Out of Range.');
  with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    if (Bind <> nil) then
      {$R-}
      Result := Bind.is_null[FCurrentRowOffset] <> 0
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    else begin
      if FplainDriver.sqlany_get_data_info(Fa_sqlany_stmt, ColumnIndex, Fdata_info) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data_info', Self);
      Result := Fdata_info.is_null <> 0;
    end;
  end;
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
function TZSQLAnywhereResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (Fa_sqlany_stmt = nil) then
    Exit;
  Result := FPlainDriver.sqlany_fetch_next(Fa_sqlany_stmt) = 1;
  RowNo := RowNo + 1;
  if Result then
    LastRowNo := RowNo;
end;

{**
  Opens this recordset.
}
procedure TZSQLAnywhereResultSet.Open;
var i, info_size: Tsacapi_i32;
  RowSize: Tsize_t;
  sqlany_column_info: Pa_sqlany_column_info;
  ColumnInfo: TZSQLAnywhereColumnInfo;
  function GetStringProp(Buf: PAnsiChar): String;
  var L: LengthInt;
  begin
    Result := '';
    if Buf <> nil then begin
      L := ZFastCode.StrLen(Buf);
      {$IFDEF UNICODE}
      Result := PRawToUnicode(Buf, L, FClientCP);
      {$ELSE}
      System.SetString(Result, Buf, L);
      {$ENDIF}
    end;
  end;
begin
  Fnum_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt);
  if Fnum_cols < 1 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := Fnum_cols;
  if FSQLAnyConnection.Get_api_version >= SQLANY_API_VERSION_4 then begin
    info_size := SizeOf(Ta_sqlany_column_infoV4up);
    GetMem(FDataValuesV4up, SizeOf(Ta_sqlany_data_valueV4up)*Fnum_cols);
  end else begin
    info_size := SizeOf(Ta_sqlany_column_info);
    GetMem(FDataValues, SizeOf(Ta_sqlany_data_value)*Fnum_cols);
  end;
  RowSize := 0;
  GetMem(sqlany_column_info, info_size);
  try
    for i := 0 to Fnum_cols -1 do begin
      ColumnInfo := TZSQLAnywhereColumnInfo.Create;
      ColumnsInfo.Add(ColumnInfo);
      if FPlainDriver.sqlany_get_column_info(Fa_sqlany_stmt, I, sqlany_column_info) <> 1 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_column_info', Self);
      ColumnInfo.NativeType := sqlany_column_info.native_type;
      ColumnInfo.Nullable := TZColumnNullableType(sqlany_column_info.nullable <> 0);
      ColumnInfo.ReadOnly := True;
      if Ord(ColumnInfo.ColumnType) < Ord(stAsciiStream) then
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;

      ColumnInfo.ColumnLabel := GetStringProp(sqlany_column_info.name);
      if FSQLAnyConnection.Get_api_version >= SQLANY_API_VERSION_4 then begin
        ColumnInfo.TableName := GetStringProp(Pa_sqlany_column_infoV4up(sqlany_column_info).table_name);
        ColumnInfo.Writable := ColumnInfo.TableName <> '';
        ColumnInfo.Bind := Pointer(PAnsiChar(FDataValuesV4up)+(SizeOf(Ta_sqlany_data_valueV4up)*I));
        ColumnInfo.Bind.buffer_size := sqlany_column_info.max_size;
        ColumnInfo.Bind._type := Pa_sqlany_column_infoV4up(sqlany_column_info)._type;
      end else begin
        ColumnInfo.Bind := Pointer(PAnsiChar(FDataValues)+(SizeOf(Ta_sqlany_data_value)*I));
        Pa_sqlany_data_value(ColumnInfo.Bind)._type := sqlany_column_info._type;
        Pa_sqlany_data_value(ColumnInfo.Bind).buffer_size := sqlany_column_info.max_size;
      end;
      if Ord(ColumnInfo.ColumnType) < Ord(stAsciiStream) then
        RowSize := RowSize + sqlany_column_info.max_size
      else if Fdata_info = nil then
        GetMem(Fdata_info, SizeOf(Ta_sqlany_data_info));

      if (sqlany_column_info.native_type = DT_VARCHAR) or
         (sqlany_column_info.native_type = DT_NVARCHAR) or
         (sqlany_column_info.native_type = DT_BINARY)
      then RowSize := RowSize +2;
      ColumnInfo.ColumnType := ConvertSQLAnyTypeToSQLType(ColumnInfo.NativeType);
      if ColumnInfo.ColumnType in [stUnicodeString, stUnicodeStream] then begin
        ColumnInfo.ColumnCodePage := zCP_UTF8;
        if ColumnInfo.ColumnType = stUnicodeString then begin//ASA calcutates the n column different
          ColumnInfo.Precision := sqlany_column_info.max_size shr 2; //default UTF8 has 3 bytes only whereas n-cols have 4 bytes
          ColumnInfo.Signed := ColumnInfo.NativeType = DT_NFIXCHAR;
        end;
      end else if ColumnInfo.ColumnType in [stString, stAsciiStream] then begin
        ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        if ColumnInfo.ColumnType = stString then begin
          ColumnInfo.Precision := ColumnInfo.CharOctedLength div ConSettings^.ClientCodePage^.CharWidth;
          ColumnInfo.Signed := ColumnInfo.NativeType = DT_FIXCHAR;
        end;
      end else if ColumnInfo.ColumnType = stBytes then
        ColumnInfo.Precision := sqlany_column_info.max_size
      else if ColumnInfo.ColumnType = stBigDecimal then begin
        ColumnInfo.Precision := sqlany_column_info.precision;
        ColumnInfo.Scale := sqlany_column_info.scale;
        if (ColumnInfo.Scale <= 4) and (ColumnInfo.Precision <= sAlignCurrencyScale2Precision[ColumnInfo.Scale]) then
          ColumnInfo.ColumnType := stCurrency;
        ColumnInfo.Signed := True;
      end else if ColumnInfo.ColumnType in [stTime, stTimeStamp] then begin
        ColumnInfo.Precision := sqlany_column_info.precision;
        ColumnInfo.Scale := sqlany_column_info.scale;
      end else
        ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stCurrency, stBigDecimal];

    end;
  finally
    FreeMem(sqlany_column_info);
  end;
  if (Fdata_info <> nil) or (FSQLAnyConnection.Get_api_version < SQLANY_API_VERSION_4)
  then FIteration := 1
  else begin
    FIteration := FZBufferSize div RowSize;
    if FIteration = 0 then
      FIteration := 1
  end;
  { allocate just two blocks for the data buffers }
  GetMem(FNullArray, SizeOf(Tsacapi_i32) * FIteration);
  GetMem(FColumnData, RowSize * FIteration);
  RowSize := 0;
  for i := 0 to Fnum_cols -1 do with TZSQLAnywhereColumnInfo(ColumnsInfo[i]) do begin
    if Ord(ColumnType) < Ord(stAsciiStream)
    then Bind.buffer := PAnsiChar(FColumnData)+RowSize
    else Bind.buffer := nil;
    Bind.buffer_size := CharOctedLength;
    Bind.is_null := Pointer(PAnsiChar(FColumnData)+(Cardinal(I) * FIteration)*SizeOf(Tsacapi_i32));
    RowSize := RowSize + (Cardinal(CharOctedLength)*FIteration);
    if (NativeType = DT_VARCHAR) or (NativeType = DT_NVARCHAR) or (NativeType = DT_BINARY) then
      RowSize := RowSize + FIteration shl 1;
  end;
  inherited Open;
end;

{**
  Resets the cursor position to initialstate and releases all resources.
}
procedure TZSQLAnywhereResultSet.ResetCursor;
begin
  inherited;

end;

{ TZSQLAnywhereCachedResultSet }

class function TZSQLAnywhereCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZSQLAnywhereRowAccessor
end;

{ TZSQLAnywhereRowAccessor }

constructor TZSQLAnywhereRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  CachedLobs: WordBool);
var TempColumns: TObjectList;
  I: Integer;
  Current: TZColumnInfo;
begin
  TempColumns := TObjectList.Create(True);
  CopyColumnsInfo(ColumnsInfo, TempColumns);
  for I := 0 to TempColumns.Count -1 do begin
    Current := TZColumnInfo(TempColumns[i]);
    if Current.ColumnType in [stUnicodeString, stUnicodeStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no national chars in 4 asa
   { eh: we can stream the data with asa, but we need always to fetch the
     current row and we have no descriptor so we cache the data }
   if Current.ColumnType in [stAsciiStream, stBinaryStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-3);
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, False);
  TempColumns.Free;
end;

procedure TZSQLAnywhereRowAccessor.FetchLongData(AsStreamedType: TZSQLType;
  const ResultSet: IZResultSet; ColumnIndex: Integer; Data: PPZVarLenData);
begin
  inherited;

end;

initialization
{$ENDIF ZEOS_DISABLE_ASA}
end.
