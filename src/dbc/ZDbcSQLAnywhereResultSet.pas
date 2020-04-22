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
    Fa_sqlany_stmt: PPa_sqlany_stmt;
    FPLainDriver: TZSQLAnywherePlainDriver;
    FSQLAnyConnection: IZSQLAnywhereConnection;
    FClientCP, FRawStrCP: Word;
    Fnum_cols: Tsacapi_i32; //kept for index check
    FZBufferSize: Cardinal; //max size for multiple rows. If Row > Value ignore it!
    Fdata_info: Pa_sqlany_data_info; //used for the LOB's
    FCurrentRowOffset, FIteration: Cardinal; //in array value bindings we need an offset
    FData: Pointer; //just a temporary pointer to a buffer
    FDataLen: TSize_T; //just current datalen of buffer
    FDataValues: Pa_sqlany_data_valueArray; //just one of both is used
    FDataValuesV4up: Pa_sqlany_data_valueV4upArray; //just one of both is used
    FColumnData: Pointer; //one buffer for all column data fields
    function FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer;
      out native_type: Ta_sqlany_native_type): Boolean;
    function CreateConversionError(Index: Integer): EZSQLException;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      a_sqlany_stmt: PPa_sqlany_stmt);

    procedure BeforeClose; override;
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

    function MoveAbsolute(Row: Integer): Boolean; override;
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

  TZSQLAnyLob = class;

  TZSQLAnyStream = class(TZImmediatelyReleasableLobStream)
  private
    FPlainDriver: TZSQLAnywherePlainDriver;
    FOwnerLob: TZSQLAnyLob;
    FPosition: TSize_t;
  protected
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  public
    constructor Create(const OwnerLob: TZSQLAnyLob);
    destructor Destroy; override;
  end;

  TZSQLAnyLob = Class(TZAbstractStreamedLob, IZLob, IZBlob, IImmediatelyReleasable)
  private
    Fa_sqlany_data_info: Ta_sqlany_data_info;
    Fa_sqlany_stmt: Pa_sqlany_stmt;
    FConnection: IZSQLAnywhereConnection;
    FColumnIndex: Integer;
    FReleased: Boolean;
    FCurrentRowAddr: PInteger;
    FLobRowNo: Integer;
    FPlainDriver: TZSQLAnywherePlainDriver;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    function GetConSettings: PZConSettings;
  public
    constructor Create(const Connection: IZSQLAnywhereConnection;
      a_sqlany_data_info: Pa_sqlany_data_info; a_sqlany_stmt: Pa_sqlany_stmt;
      ColumnIndex: Integer; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList; CurrentRowAddr: PInteger);
  public
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
  public
    function Length: Integer; override;
  End;

  TZSQLAnyBLob = class(TZSQLAnyLob);

  TZSQLAnyCLob = class(TZSQLAnyLob, IZClob);

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
  PPointer(@fTinyBuffer[0])^ := nil;
  Fa_sqlany_stmt := @fTinyBuffer[0];
  if (FColumnData <> nil) then begin
    FreeMem(FColumnData);
    FColumnData := nil;
  end;
  if (Fdata_info <> nil) then begin
    FreeMem(Fdata_info);
    Fdata_info := nil;
  end;
  if (FDataValues <> nil) then begin
    FreeMem(FDataValues);
    FDataValues := nil;
  end;
  if (FDataValuesV4up <> nil) then begin
    FreeMem(FDataValuesV4up);
    FDataValuesV4up := nil;
  end;
end;

procedure TZSQLAnywhereResultSet.BeforeClose;
begin
  inherited;
  if (FDataValuesV4up <> nil) and (Fa_sqlany_stmt^ <> nil) then
    FplainDriver.sqlany_clear_column_bindings(Fa_sqlany_stmt^);
end;

constructor TZSQLAnywhereResultSet.Create(const Statement: IZStatement;
  const SQL: string; a_sqlany_stmt: PPa_sqlany_stmt);
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
var ColumnInfo: TZSQLAnywhereColumnInfo;
begin
  ColumnInfo := TZSQLAnywhereColumnInfo(ColumnsInfo[Index]);
  //with TZSQLAnywhereColumnInfo(ColumnsInfo[Index]) do
    Result := EZSQLException.Create(Format(SErrorConvertionField,
      [ColumnInfo.ColumnLabel, TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(ColumnInfo.ColumnType))]));
end;

function TZSQLAnywhereResultSet.FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer; out native_type: Ta_sqlany_native_type): Boolean;
var ABind: Pa_sqlany_data_value;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  {$IFDEF DEBUG}
  Assert((Index >= 0) and (Index < Fnum_cols), 'Out of Range.');
  {$ENDIF}
  if Fa_sqlany_stmt^ = nil then
    Result := False
  else with TZSQLAnywhereColumnInfo(ColumnsInfo[Index]) do begin
    native_type := NativeType;
    ABind := Pa_sqlany_data_value(Bind);
    if IsBound then begin
      {$R-}
      Result := Bind.is_null[FCurrentRowOffset] = 0;
      FData := ABind.buffer+(FCurrentRowOffset*Bind.buffer_size);
      FDataLen := Bind.length[FCurrentRowOffset];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end else if (Ord(ColumnType) >= Ord(stAsciiStream)) then begin
      if FplainDriver.sqlany_get_data_info(Fa_sqlany_stmt^, Index, Fdata_info) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data_info', Self);
      Result := (Fdata_info.is_null = 0);
    end else begin
      if FplainDriver.sqlany_get_column(Fa_sqlany_stmt^, Index, Pointer(ABind)) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_column', Self);
      Result := (ABind.is_null^ = 0);
      FData :=  ABind.buffer;
      FDataLen := ABind.length^;
    end;
  end;
  LastWasNull := not Result;
end;

procedure TZSQLAnywhereResultSet.GetBigDecimal(ColumnIndex: Integer;
  var Result: TBCD);
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_TINYINT     : ScaledOrdinal2BCD(SmallInt(PShortInt(FData)^), 0, Result);
    //DT_BIT         : Result := PByte(FData)^ <> 0;
    DT_SMALLINT    : ScaledOrdinal2BCD(PSmallInt(FData)^, 0, Result);
    DT_UNSSMALLINT : ScaledOrdinal2BCD(PWord(FData)^, 0, Result, False);
    DT_INT         : ScaledOrdinal2BCD(PInteger(FData)^, 0, Result);
    DT_UNSINT      : ScaledOrdinal2BCD(PCardinal(FData)^, 0, Result, False);
    DT_BIGINT      : ScaledOrdinal2BCD(PInt64(FData)^, 0, Result);
    DT_UNSBIGINT   : ScaledOrdinal2BCD(PUInt64(FData)^, 0, Result, False);
    DT_FLOAT       : Double2BCD(PSingle(FData)^, Result);
    DT_DOUBLE      : Double2BCD(PDouble(FData)^, Result);
    DT_NFIXCHAR,
    DT_FIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : if not TryRawToBcd(FData, FDataLen, Result, '.') then
                        PCardinal(@Result.Precision)^ := 0;
    else raise CreateConversionError(ColumnIndex);
  end else
    PCardinal(@Result.Precision)^ := 0;
end;

function TZSQLAnywhereResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var native_type: Ta_sqlany_native_type;
begin
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  if FillData(ColumnIndex, native_type) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do
    case native_type of
      DT_LONGBINARY: Result := TZSQLAnyBLob.Create(FSQLAnyConnection, Fdata_info, Fa_sqlany_stmt^,
        ColumnIndex, lsmRead, zCP_Binary, FOpenLobStreams, @FRowNo);
      DT_LONGVARCHAR,
      DT_LONGNVARCHAR: Result := TZSQLAnyCLob.Create(FSQLAnyConnection, Fdata_info, Fa_sqlany_stmt^,
        ColumnIndex, lsmRead, ColumnCodePage, FOpenLobStreams, @FRowNo);
      else raise CreateConversionError(ColumnIndex);
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
function TZSQLAnywhereResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR     : Result := StrToBoolEx(PAnsiChar(FData), PAnsiChar(FData)+FDataLen, True);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := False;
end;

function TZSQLAnywhereResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
var native_type: Ta_sqlany_native_type;
  function FromLob(ColumnCodePage: Word; out Len: NativeUint): PByte;
  var Lob: IZBlob;
  begin
    if native_type = DT_LONGBINARY then
      Lob := TZSQLAnyBLob.Create(FSQLAnyConnection, Fdata_info, Fa_sqlany_stmt^,
        ColumnIndex, lsmRead, zCP_Binary, FOpenLobStreams, @FRowNo)
    else Lob := TZSQLAnyBLob.Create(FSQLAnyConnection, Fdata_info, Fa_sqlany_stmt^,
        ColumnIndex, lsmRead, ColumnCodePage, FOpenLobStreams, @FRowNo);
    Result := Lob.GetBuffer(fRawTemp, Len);
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Result := FData;
    Len := CharOctedLength;
    case native_type of
      DT_TINYINT,
      DT_BIT,
      DT_SMALLINT,
      DT_UNSSMALLINT,
      DT_INT,
      DT_UNSINT,
      DT_BIGINT,
      DT_UNSBIGINT,
      DT_FLOAT,
      DT_DOUBLE: ;
      DT_FIXCHAR,
      DT_NFIXCHAR,
      DT_NVARCHAR,
      DT_VARCHAR,
      DT_BINARY     : Len := FDataLen;
      DT_LONGBINARY,
      DT_LONGVARCHAR,
      DT_LONGNVARCHAR: Result := FromLob(ColumnCodePage, Len);
      else raise CreateConversionError(ColumnIndex);
    end;
  end else begin
    Len := 0;
    Result := nil;
  end;
end;

function TZSQLAnywhereResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FDataLen);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywhereResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
label jmpFail;
var TS: TZTimeStamp;
    native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DATE: if not TryRawToDate(FData, FDataLen, ConSettings.ReadFormatSettings.DateFormat, Result) then
              goto jmpFail;
    DT_TIME: PInt64(@Result.Year)^ := 0;
    DT_TIMESTAMP: if not TryRawToTimeStamp(FData, FDataLen, ConSettings.ReadFormatSettings.DateFormat, TS)
      then goto jmpFail
      else begin
        PInt64(@Result.Year)^ := PInt64(@TS.Year)^;
        Result.IsNegative := TS.IsNegative;
      end;
    else DecodeDateTimeToDate(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
  end else begin
jmpFail:
    PInt64(@Result.Year)^ := 0;
    LastWasNull := True;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZSQLAnywhereResultSet.GetDouble(ColumnIndex: Integer): Double;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FDataLen);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;

function TZSQLAnywhereResultSet.GetFloat(ColumnIndex: Integer): Single;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FDataLen);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;

procedure TZSQLAnywhereResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var native_type: Ta_sqlany_native_type;
label fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_BINARY       : if FDataLen = SizeOf(TGUID)
                      then Result := PGUID(FData)^
                      else goto fail;
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR      : if (FDataLen = 36) or (FDataLen = 38)
                      then ValidGUIDToBinary(PAnsiChar(FData), @Result.D1)
                      else goto fail;
    else
fail: raise CreateConversionError(ColumnIndex);
  end else begin
    PInt64(@Result.D1)^ := 0;
    PInt64(@Result.D4)^ := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>signed 32bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLAnywhereResultSet.GetInt(ColumnIndex: Integer): Integer;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : Result := RawToIntDef(PAnsiChar(FData), PAnsiChar(FData)+FDataLen, 0);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>signed 64bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLAnywhereResultSet.GetLong(ColumnIndex: Integer): Int64;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : Result := RawToInt64Def(PAnsiChar(FData), PAnsiChar(FData)+FDataLen, 0);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
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
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_TIME,
    DT_DATE,
    DT_TIMESTAMP,
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_VARCHAR,
    DT_NVARCHAR,
    DT_BINARY     : begin
                      Result := FData;
                      Len := FDataLen;
                    end;
    DT_LONGBINARY,
    DT_LONGNVARCHAR,
    DT_LONGVARCHAR: if Fdata_info.data_size < SizeOf(FTinyBuffer) then begin
                      Result := @FTinyBuffer[0];
                      ColumnIndex := FplainDriver.sqlany_get_data(Fa_sqlany_stmt^, ColumnIndex,
                        0, Result, SizeOf(FTinyBuffer));
                      if ColumnIndex < 0 then
                        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data', Self);
                      Len := Cardinal(ColumnIndex);
                    end else FromLob;
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
    else begin      Result := nil;
                    Len := 0;
                    raise CreateConversionError(ColumnIndex);
                  end
    end
  else begin
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
label set_Results, set_from_uni, convert;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then with TZSQLAnywhereColumnInfo(ColumnsInfo[ColumnIndex]) do case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR    : begin
                      Result := FData;
                      Len := FDataLen;
convert:              fUniTemp := PRawToUnicode(PAnsiChar(Result), Len, ColumnCodePage);
                      goto set_from_uni;
                    end;
    DT_BINARY     : begin
                      fUniTemp := Ascii7ToUnicodeString(FData, FDataLen);
set_from_uni:         Len := Length(FUniTemp);
                      if Len = 0
                      then Result := PEmptyUnicodeString
                      else Result := Pointer(FUniTemp);
                    end;
    DT_LONGNVARCHAR,
    DT_LONGVARCHAR: if Fdata_info.data_size < SizeOf(FTinyBuffer) then begin
                      Result := @FTinyBuffer[0];
                      ColumnIndex := FplainDriver.sqlany_get_data(Fa_sqlany_stmt^, ColumnIndex,
                        0, Result, SizeOf(FTinyBuffer));
                      if ColumnIndex < 0 then
                        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data', Self);
                      Len := Cardinal(ColumnIndex);
                      goto Convert;
                    end else FromLob;
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
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywhereResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
label jmpFail, jmpFill;
var TS: TZTimeStamp;
    native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_TIME: if not TryRawToTime(FData, FDataLen, ConSettings.ReadFormatSettings.TimeFormat, Result) then
              goto jmpFail;
    DT_DATE: goto jmpFill;
    DT_TIMESTAMP: if not TryRawToTimeStamp(FData, FDataLen, ConSettings.ReadFormatSettings.DateFormat, TS)
      then goto jmpFail
      else begin
        PInt64(@Result.Hour)^ := PInt64(@TS.Hour)^;
        Result.Fractions := TS.Fractions;
        Result.IsNegative := TS.IsNegative;
      end;
    else DecodeDateTimeToTime(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
  end else begin
jmpFail:
    LastWasNull := True;
jmpFill:
    PInt64(@Result.Hour)^ := 0;
    PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TM" does not seem to be initialized} {$ENDIF}
procedure TZSQLAnywhereResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
var TM: TZTime;
    DT: TZDate absolute TM;
    native_type: Ta_sqlany_native_type;
label jmpFail, jmpFill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_TIMESTAMP: if not TryRawToTimeStamp(FData, FDataLen, ConSettings.ReadFormatSettings.DateTimeFormat, Result) then
              goto jmpFail;
    DT_TIME: if not TryRawToTime(FData, FDataLen, ConSettings.ReadFormatSettings.TimeFormat, TM)
        then goto jmpFail
        else begin
          PInt64(@Result.Year)^ := 0;
          PInt64(@Result.Hour)^ := PInt64(@Tm.Hour)^;
          PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
          Result.Fractions := TM.Fractions;
          Result.IsNegative := TM.IsNegative;
        end;
    DT_DATE: if not TryRawToDate(FData, FDataLen, ConSettings.ReadFormatSettings.DateFormat, DT)
        then goto jmpFail
        else begin
          PInt64(@Result.Year)^ := PInt64(@DT.Year)^;
          PInt64(@Result.Hour)^ := 0;
          PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
          Result.IsNegative := DT.IsNegative;
        end;
    else DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
  end else begin
jmpFail:
    LastWasNull := True;
jmpFill:
    PInt64(@Result.Hour)^ := 0;
    PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>unsigned 32bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLAnywhereResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_TINYINT     : Result := PShortInt(FData)^;
    DT_BIT         : Result := PByte(FData)^;
    DT_SMALLINT    : Result := PSmallint(FData)^;
    DT_UNSSMALLINT : Result := PWord(FData)^;
    DT_INT         : Result := PInteger(FData)^;
    DT_UNSINT      : Result := PCardinal(FData)^;
    DT_BIGINT      : Result := PInt64(FData)^;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    DT_UNSBIGINT   : Result := PUInt64(FData)^;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FData)^);
    DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FData)^);
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    DT_DECIMAL     : Result := RawToUInt64Def(PAnsiChar(FData), PAnsiChar(FData)+FDataLen, 0);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>unsigned 64bit integer</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZSQLAnywhereResultSet.GetULong(ColumnIndex: Integer): UInt64;
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
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
    DT_FIXCHAR,
    DT_NFIXCHAR,
    DT_NVARCHAR,
    DT_VARCHAR,
    DT_DECIMAL     : Result := RawToUInt64Def(PAnsiChar(FData), PAnsiChar(FData)+FDataLen, 0);
    else raise CreateConversionError(ColumnIndex);
  end else
    Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

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
    if (Bind <> nil) and IsBound then
      {$R-}
      Result := Bind.is_null[FCurrentRowOffset] <> 0
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    else begin
      if FplainDriver.sqlany_get_data_info(Fa_sqlany_stmt^, ColumnIndex, Fdata_info) = 0 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_data_info', Self);
      Result := Fdata_info.is_null <> 0;
    end
  end;
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
function TZSQLAnywhereResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  if Closed or ((Row < 1) or ((MaxRows > 0) and (Row >= MaxRows))) then
    Exit;
  Result := FPlainDriver.sqlany_fetch_absolute(Fa_sqlany_stmt^, Row) = 1;
  if Result then begin
    RowNo := Row;
    if Row > LastRowNo then
      LastRowNo := Row;
  end else
    FSQLAnyConnection.HandleError(lcExecute, 'sqlany_fetch_absolute', Self)
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
var fetched_rows: Tsacapi_i32;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (Fa_sqlany_stmt^ = nil) then
    Exit;
  if (FIteration > 1) then begin
    Inc(FCurrentRowOffset, Cardinal(RowNo > 0));
    if (FCurrentRowOffset = FIteration) or (RowNo = 0)  then begin
      FCurrentRowOffset := 0;
      Result := FPlainDriver.sqlany_fetch_next(Fa_sqlany_stmt^) = 1;
      if Result then begin
        fetched_rows := FPlainDriver.sqlany_fetched_rows(Fa_sqlany_stmt^);
        if fetched_rows < 0
        then FSQLAnyConnection.HandleError(lcOther, 'sqlany_fetched_rows', Self)
        else if Cardinal(fetched_rows) < FIteration then begin
          LastRowNo := RowNo+fetched_rows;
          if fetched_rows > 0 then
            Result := True
        end;
      end;
    end;
  end else
    Result := FPlainDriver.sqlany_fetch_next(Fa_sqlany_stmt^) = 1;
  RowNo := RowNo + 1;
  if Result then
    LastRowNo := RowNo;
end;

{**
  Opens this recordset.
}
procedure TZSQLAnywhereResultSet.Open;
var i: Tsacapi_i32;
  ApiVersion: Tsacapi_u32;
  RowSize: Tsize_t;
  sqlany_column_info: Pa_sqlany_column_info;
  ColumnInfo: TZSQLAnywhereColumnInfo;
  P: PAnsiChar;
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
  Fnum_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt^);
  if Fnum_cols < 1 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := Fnum_cols;
  ApiVersion := FSQLAnyConnection.Get_api_version;
  if ApiVersion >= SQLANY_API_VERSION_4 then begin
    RowSize := SizeOf(Ta_sqlany_data_valueV4up);
    P := @FDataValuesV4up;
  end else begin
    RowSize := SizeOf(Ta_sqlany_column_info);
    P := @FDataValues;
  end;
  RowSize := RowSize * Cardinal(Fnum_cols);
  GetMem(PPointer(P)^, RowSize);
  FillChar(PPointer(P)^^, RowSize, #0);
  sqlany_column_info := @FTinyBuffer[0];
  RowSize := 0;
  for i := 0 to Fnum_cols -1 do begin
    ColumnInfo := TZSQLAnywhereColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);
    if FPlainDriver.sqlany_get_column_info(Fa_sqlany_stmt^, I, sqlany_column_info) <> 1 then
      FSQLAnyConnection.HandleError(lcOther, 'sqlany_get_column_info', Self);
    ColumnInfo.NativeType := sqlany_column_info.native_type;
    ColumnInfo.Nullable := TZColumnNullableType(sqlany_column_info.nullable <> 0);
    ColumnInfo.ReadOnly := True;
    ColumnInfo.ColumnType := ConvertSQLAnyTypeToSQLType(ColumnInfo.NativeType);
    if Ord(ColumnInfo.ColumnType) < Ord(stAsciiStream) then begin
      if ColumnInfo.ColumnType = stString then begin
        ColumnInfo.Precision := sqlany_column_info.max_size;
        sqlany_column_info.max_size := (sqlany_column_info.max_size + 2) * Byte(ConSettings.ClientCodePage^.CharWidth);
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;
        ColumnInfo.ColumnCodePage := FClientCP;
        ColumnInfo.Signed := ColumnInfo.NativeType = DT_FIXCHAR;
      end else if ColumnInfo.ColumnType = stUnicodeString then begin
        ColumnInfo.Precision := sqlany_column_info.max_size;
        sqlany_column_info.max_size := sqlany_column_info.max_size shl 2;
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;
        ColumnInfo.ColumnCodePage := zCP_UTF8;
        ColumnInfo.Signed := ColumnInfo.NativeType = DT_NFIXCHAR;
      end else if ColumnInfo.ColumnType = stBytes then begin
        ColumnInfo.Precision := sqlany_column_info.max_size;
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;
        ColumnInfo.ColumnCodePage := zCP_Binary;
      end else if ColumnInfo.ColumnType = stBigDecimal then begin
        ColumnInfo.Precision := sqlany_column_info.precision;
        sqlany_column_info.max_size := sqlany_column_info.precision + 2; //sign, dot
        if (FDataValuesV4up <> nil) then
          sqlany_column_info.native_type := DT_VARCHAR;
        ColumnInfo.Scale := sqlany_column_info.scale;
        if (ColumnInfo.Scale <= 4) and (ColumnInfo.Precision <= sAlignCurrencyScale2Precision[ColumnInfo.Scale]) then
          ColumnInfo.ColumnType := stCurrency;
        ColumnInfo.Signed := True;
      end else if ColumnInfo.ColumnType in [stTime, stTimeStamp] then begin
        ColumnInfo.Precision := sqlany_column_info.precision;
        ColumnInfo.Scale := sqlany_column_info.scale;
      end else begin
        ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stDouble];
        if (ColumnInfo.ColumnType = stFloat) {and (ApiVersion < SQLANY_API_VERSION_4) }then begin
          { sybase overruns our buffer even if maxsize is SizeOf(Single) }
          sqlany_column_info.max_size := SizeOf(Double);
          ColumnInfo.NativeType := DT_DOUBLE;
        end;
      end;
    end else begin
      sqlany_column_info.max_size := 0;
      if ColumnInfo.ColumnType = stUnicodeStream then
        ColumnInfo.ColumnCodePage := zCP_UTF8
      else if ColumnInfo.ColumnType = stAsciiStream then
        ColumnInfo.ColumnCodePage := FClientCP
      else ColumnInfo.ColumnCodePage := zCP_Binary;
      if Fdata_info = nil then
        GetMem(Fdata_info, SizeOf(Ta_sqlany_data_info));
    end;
    ColumnInfo.ColumnLabel := GetStringProp(sqlany_column_info.name);
    if (FDataValuesV4up <> nil) then begin
      ColumnInfo.TableName := GetStringProp(Pa_sqlany_column_infoV4up(sqlany_column_info).table_name);
      ColumnInfo.Writable := ColumnInfo.TableName <> '';
      ColumnInfo.Bind := Pointer(PAnsiChar(FDataValuesV4up)+(SizeOf(Ta_sqlany_data_valueV4up)*I));
    end else
      ColumnInfo.Bind := Pointer(PAnsiChar(FDataValues)+(SizeOf(Ta_sqlany_data_value)*I));
    ColumnInfo.Bind.buffer_size := sqlany_column_info.max_size;
    ColumnInfo.Bind._type := sqlany_column_info._type;
    RowSize := RowSize + SizeOf(Tsacapi_bool);
    RowSize := RowSize + SizeOf(Tsize_t);
  end;
  if (Fdata_info <> nil) or (FSQLAnyConnection.Get_api_version < SQLANY_API_VERSION_4)
  then FIteration := 1
  else begin
    FIteration := FZBufferSize div RowSize;
    if FIteration = 0 then
      FIteration := 1
  end;
  FIteration := 1;
  if FIteration > 1 then
    if FplainDriver.sqlany_set_rowset_size(Fa_sqlany_stmt^, FIteration) <> 1 then
      FSQLAnyConnection.HandleError(lcOther, 'sqlany_set_rowset_size', Self);

  { allocate just one block of memory for the data buffers }
  GetMem(FColumnData, (RowSize * FIteration));
  P := FColumnData;
  for i := 0 to Fnum_cols -1 do with TZSQLAnywhereColumnInfo(ColumnsInfo[i]) do begin
    Bind.is_null := FColumnData;
    Inc(PAnsiChar(FColumnData), SizeOf(Tsacapi_i32)*FIteration);
    Bind.length := FColumnData;
    Inc(PAnsiChar(FColumnData), SizeOf(Tsize_t)*FIteration);
    if Ord(ColumnType) < Ord(stAsciiStream) then begin
      Bind.buffer := PAnsiChar(FColumnData);
      Inc(PAnsiChar(FColumnData), Bind.buffer_size*FIteration);
    end else
      Bind.buffer := nil;
    {if (FIteration > 1) or (FDataValuesV4up <> nil) then begin
      Bind.is_address := 0;
      if FplainDriver.sqlany_bind_column(Fa_sqlany_stmt^, Cardinal(I), Bind) <> 1 then
        FSQLAnyConnection.HandleError(lcOther, 'sqlany_bind_column', Self);
      IsBound := True;
    end;}
    if not IsBound and (Fdata_info = nil) then //need that for IsNull()
      GetMem(Fdata_info, SizeOf(Ta_sqlany_data_info));
  end;
  FColumnData := P;
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
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no national chars in 4 SQLAny
   { eh: we can stream the data with SQLAny, but we need always to fetch the
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

{ TZSQLAnyStream }

constructor TZSQLAnyStream.Create(const OwnerLob: TZSQLAnyLob);
begin
  inherited Create(OwnerLob, OwnerLob.FConnection, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  FPlainDriver := OwnerLob.FConnection.GetPlainDriver;
end;

destructor TZSQLAnyStream.Destroy;
begin
  inherited Destroy;
end;

function TZSQLAnyStream.GetSize: Int64;
begin
  if FReleased
  then Result := 0
  else Result := FOwnerLob.Fa_sqlany_data_info.data_size;
end;

function TZSQLAnyStream.Read(var Buffer; Count: Longint): Longint;
begin
  if FReleased or (Count = 0) or (FOwnerLob.Fa_sqlany_data_info.data_size = 0)
  then Result := 0
  else begin
    Result := FPlainDriver.sqlany_get_data(FOwnerLob.Fa_sqlany_stmt, FOwnerLob.FColumnIndex,
      FPosition, @Buffer, Count);
    if Result < 0 then
      FOwnerLob.FConnection.HandleError(lcOther, 'sqlany_get_data', Self);
    FPosition := FPosition+TSize_t(Result);
  end;
end;

function TZSQLAnyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soEnd then
    Result := 0
  else if Origin = soCurrent then
    Result := FPosition + OffSet
  else
    Result := OffSet;
  if Result <> FPosition then
    FPosition := Result;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZSQLAnyStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZSQLAnyLob }

procedure TZSQLAnyLob.Clear;
begin
  raise CreateReadOnlyException;
end;

function TZSQLAnyLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
begin
  Result := nil;
end;

constructor TZSQLAnyLob.Create(const Connection: IZSQLAnywhereConnection;
  a_sqlany_data_info: Pa_sqlany_data_info; a_sqlany_stmt: Pa_sqlany_stmt;
  ColumnIndex: Integer; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList; CurrentRowAddr: PInteger);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  FConnection := Connection;
  FColumnIndex := ColumnIndex;
  FLobStreamMode := LobStreamMode;
  FCurrentRowAddr := CurrentRowAddr;
  FLobRowNo := CurrentRowAddr^;
  FPlainDriver := Connection.GetPlainDriver;
  FConSettings := Connection.GetConSettings;
  Fa_sqlany_stmt := a_sqlany_stmt;
  Fa_sqlany_data_info := a_sqlany_data_info^;
end;

function TZSQLAnyLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  if FReleased
  then Result := nil
  else begin
    if FCurrentRowAddr^ <> FLobRowNo then begin
    end;
    Result := TZSQLAnyStream.Create(Self);
    if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
      Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
  end;
end;

function TZSQLAnyLob.GetConSettings: PZConSettings;
begin
  if FConnection = nil
  then Result := nil
  else Result := FConnection.GetConSettings;
end;

function TZSQLAnyLob.IsEmpty: Boolean;
begin
  Result := False;
end;

function TZSQLAnyLob.Length: Integer;
begin
  if FReleased
  then Result := -1
  else Result := Fa_sqlany_data_info.data_size;
end;

procedure TZSQLAnyLob.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FConnection <> nil) and (FConnection.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then
    Imm.ReleaseImmediat(Sender, AError);
  FConnection := nil;
  FReleased := true;
end;

initialization
{$ENDIF ZEOS_DISABLE_ASA}
end.
