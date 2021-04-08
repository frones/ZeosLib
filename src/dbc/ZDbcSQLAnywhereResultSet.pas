{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
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
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcSQLAnywhereResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLANY}

uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  FmtBCD, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
  System.Types{$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} Types
  {$ENDIF},
  ZPlainSQLAnywhere, ZCompatibility, ZDbcCache, ZClasses, ZDbcStatement,
  ZDbcIntfs, ZDbcResultSet, ZDbcCachedResultSet, ZDbcResultSetMetadata,
  ZDbcSQLAnywhere;

type
  TZSQLAnywhereColumnInfo = class(TZColumnInfo)
  public
    NativeType: Ta_sqlany_native_type;
    Bind: Pa_sqlany_data_valueV4up;
    IsBound: Boolean;
  end;

  {** Implements and abstract SQL Anywhere ResultSet. }
  TZAbstractSQLAnywhereResultSet = class(TZAbstractReadOnlyResultSet_A)
  private
    Fa_sqlany_stmt: PPa_sqlany_stmt;
    FPLainDriver: TZSQLAnywherePlainDriver;
    FSQLAnyConnection: IZSQLAnywhereConnection;
    FBindValueSize: NativeInt;
    FClientCP: Word;
    Fnum_cols: Tsacapi_i32; //kept for index check
    FFetchedMinRowNo, FFetchedMaxRowNo: NativeInt; //required for OffSetCalculation of MoveAbsolut
    FZBufferSize: Cardinal; //max size for multiple rows. If Row > Value ignore it!
    Fdata_info: Pa_sqlany_data_info; //used for the LOB's
    FCurrentRowOffset, FIteration, FMaxBufIndex: Cardinal; //in array value bindings we need an offset
    FData: Pointer; //just a temporary pointer to a buffer
    Fapi_version, FDataLen: TSize_T; //just current datalen of buffer (if variable type)
    FDataValues: Pa_sqlany_data_valueArray;
    FColumnData: Pointer; //one buffer for all column data fields
    FByteBuffer: PByteBuffer;
    function FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer;
      out native_type: Ta_sqlany_native_type): Boolean;
    function CreateConversionError(Index: Integer): EZSQLException;
    function GetStringProp(Buf: PAnsiChar): String;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      a_sqlany_stmt: PPa_sqlany_stmt);

    procedure AfterClose; override;
    procedure ResetCursor; override;
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
    {$IFDEF WITH_COLUMNS_TO_JSON}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF WITH_COLUMNS_TO_JSON}
  end;

  { TZSQLAnywhereResultSet }

  TZSQLAnywhereResultSet = class(TZAbstractSQLAnywhereResultSet, IZResultSet)
  protected
    procedure Open; override;
  public
    function Next: Boolean; override;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  { TZSQLAynwhereOutParamResultSet }

  TZSQLAynwhereOutParamResultSet = class(TZAbstractSQLAnywhereResultSet, IZResultSet)
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      a_sqlany_stmt: PPa_sqlany_stmt; a_sqlany_bind_paramArray: Pa_sqlany_bind_paramArray;
      BindList: TZBindList);
  public
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  { TZSQLAnywhereCachedResultSet }

  TZSQLAnywhereCachedResultSet = class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZSQLAnywhereRowAccessor }

  TZSQLAnywhereRowAccessor = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; LobCacheMode: TLobCacheMode); override;
    procedure FetchLongData(AsStreamedType: TZSQLType; const ResultSet: IZResultSet;
      ColumnIndex: Integer; Data: PPZVarLenData); override;
  end;


  TZSQLAnyLob = class; //forward

  {** Implements SQLAnywhere lob stream object. }
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

  {** Implements SQLAnywhere lob descriptor object. }
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
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
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

  {** Implements SQLAnywhere Blob object. }
  TZSQLAnyBLob = class(TZSQLAnyLob);

  {** Implements SQLAnywhere Clob object. }
  TZSQLAnyCLob = class(TZSQLAnyLob, IZClob);

  {** Implements SQLAnywhere ResultSetMetadata object. }
  TZSQLAnyWhereResultSetMetadataV4Up = Class(TZAbstractResultSetMetadata)
  protected
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  public
    function GetTableName(ColumnIndex: Integer): string; override;
  End;


{$ENDIF ZEOS_DISABLE_SQLANY}
implementation
{$IFNDEF ZEOS_DISABLE_SQLANY}

uses SysUtils, TypInfo,
  ZFastCode, ZSysUtils, ZEncoding,  ZMessages, ZDbcLogging, ZDbcUtils,
  ZDbcSQLAnywhereUtils, ZDbcProperties;

{ TZAbstractSQLAnywhereResultSet }

procedure TZAbstractSQLAnywhereResultSet.AfterClose;
begin
  inherited;
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
  Fa_sqlany_stmt := @FDataValues;
end;

constructor TZAbstractSQLAnywhereResultSet.Create(const Statement: IZStatement;
  const SQL: string; a_sqlany_stmt: PPa_sqlany_stmt);
var ResultSetMetadata: TContainedObject;
begin
  FSQLAnyConnection := Statement.GetConnection as IZSQLAnywhereConnection;
  FByteBuffer := FSQLAnyConnection.GetByteBufferAddress;
  Fapi_version := FSQLAnyConnection.Get_api_version;
  if Fapi_version >= SQLANY_API_VERSION_4
  then ResultSetMetadata := TZSQLAnyWhereResultSetMetadataV4Up.Create(FSQLAnyConnection.GetMetadata, SQL, Self)
  else ResultSetMetadata := nil;
  inherited Create(Statement, SQL, ResultSetMetadata, Statement.GetConSettings);
  Fa_sqlany_stmt := a_sqlany_stmt;
  FPlainDriver := FSQLAnyConnection.GetPlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FClientCP := ConSettings.ClientCodePage.CP;
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Statement, DSProps_InternalBufSize, ''), 131072);
  if Fapi_version >= SQLANY_API_VERSION_4
  then FBindValueSize := SizeOf(Ta_sqlany_data_valueV4up)
  else FBindValueSize := SizeOf(Ta_sqlany_data_value);
  Open;
end;

function TZAbstractSQLAnywhereResultSet.CreateConversionError(
  Index: Integer): EZSQLException;
var ColumnInfo: TZSQLAnywhereColumnInfo;
begin
  ColumnInfo := TZSQLAnywhereColumnInfo(ColumnsInfo[Index]);
  //with TZSQLAnywhereColumnInfo(ColumnsInfo[Index]) do
    Result := EZSQLException.Create(Format(SErrorConvertionField,
      [ColumnInfo.ColumnLabel, TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(ColumnInfo.ColumnType))]));
end;

function TZAbstractSQLAnywhereResultSet.FillData({$IFNDEF GENERIC_INDEX}var{$ENDIF} Index: Integer; out native_type: Ta_sqlany_native_type): Boolean;
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
      if Bind.length = nil
      then FDataLen := 0
      else FDataLen := Bind.length[FCurrentRowOffset];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end else if (Ord(ColumnType) >= Ord(stAsciiStream)) then begin
      if FplainDriver.sqlany_get_data_info(Fa_sqlany_stmt^, Index, Fdata_info) = 0 then
        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_data_info', Self);
      Result := (Fdata_info.is_null = 0);
    end else begin
      if FplainDriver.sqlany_get_column(Fa_sqlany_stmt^, Index, Pointer(ABind)) = 0 then
        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_column', Self);
      Result := (ABind.is_null^ = 0);
      FData :=  ABind.buffer;
      if ABind.length = nil
      then FDataLen := 0
      else FDataLen := ABind.length^;
    end;
  end;
  LastWasNull := not Result;
end;

procedure TZAbstractSQLAnywhereResultSet.GetBigDecimal(ColumnIndex: Integer;
  var Result: TBCD);
var native_type: Ta_sqlany_native_type;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  if FillData(ColumnIndex, native_type) then case native_type of
    DT_TINYINT     : ScaledOrdinal2BCD(SmallInt(PShortInt(FData)^), 0, Result);
    DT_BIT         : ScaledOrdinal2BCD(Word(PByte(FData)^ <> 0), 0, Result, False);
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

function TZAbstractSQLAnywhereResultSet.GetBlob(ColumnIndex: Integer;
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
function TZAbstractSQLAnywhereResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
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
function TZAbstractSQLAnywhereResultSet.GetBytes(ColumnIndex: Integer;
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

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractSQLAnywhereResultSet.GetCurrency(ColumnIndex: Integer): Currency;
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
procedure TZAbstractSQLAnywhereResultSet.GetDate(ColumnIndex: Integer;
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

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractSQLAnywhereResultSet.GetDouble(ColumnIndex: Integer): Double;
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

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractSQLAnywhereResultSet.GetFloat(ColumnIndex: Integer): Single;
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

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>TGUID</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>zero padded</code>
}
procedure TZAbstractSQLAnywhereResultSet.GetGUID(ColumnIndex: Integer;
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
function TZAbstractSQLAnywhereResultSet.GetInt(ColumnIndex: Integer): Integer;
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
function TZAbstractSQLAnywhereResultSet.GetLong(ColumnIndex: Integer): Int64;
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

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TZAbstractSQLAnywhereResultSet.ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
var L: NativeUInt;
    P: Pointer;
    C, H, I: SmallInt;
    native_type: Ta_sqlany_native_type;
    procedure AddClob(ColumnCodePage: Word);
    var Clob: IZCLob;
    begin
      JSONWriter.Add('"');
      Clob := TZSQLAnyCLob.Create(FSQLAnyConnection, Fdata_info, Fa_sqlany_stmt^,
        C, lsmRead, ColumnCodePage, FOpenLobStreams, @FRowNo);
      P := Clob.GetPAnsiChar(zCP_UTF8, FRawTemp, L);
      JSONWriter.AddJSONEscape(P, L);
      JSONWriter.Add('"');
      Clob := nil;
    end;
    procedure AddBlob;
    var Blob: IZBLob;
    begin
      JSONWriter.Add('"');
      Blob := TZSQLAnyBLob.Create(FSQLAnyConnection,
        Fdata_info, Fa_sqlany_stmt^, C, lsmRead, zCP_Binary, FOpenLobStreams, @FRowNo);
      P := Blob.GetBuffer(FRawTemp, L);
      JSONWriter.WrBase64(P, L, True);
      JSONWriter.Add('"');
      Blob := nil;
    end;
begin
  //init
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    {$R-}
    if FillData(C, native_type) then with TZSQLAnywhereColumnInfo(ColumnsInfo[C]) do begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case native_type of
        DT_NOTYPE           : JSONWriter.AddShort('""');
        DT_SMALLINT         : JSONWriter.Add(PSmallint(FData)^);
        DT_INT              : JSONWriter.Add(PInteger(FData)^);
        //DT_DECIMAL bound to double
        DT_FLOAT            : JSONWriter.AddSingle(PSingle(FData)^);
        DT_DOUBLE           : JSONWriter.AddDouble(PDouble(FData)^);
        //DT_DATE bound to TIMESTAMP_STRUCT
        DT_STRING,
        DT_NSTRING,
        DT_FIXCHAR,
        DT_NFIXCHAR,
        DT_VARCHAR,
        DT_NVARCHAR         : begin
                                JSONWriter.Add('"');
                                if ColumnCodePage = zCP_UTF8 then
                                  JSONWriter.AddJSONEscape(FData, FDataLen)
                                else begin
                                  PRawToUnicode(FData, FDataLen, ColumnCodePage, FUniTemp);
                                  JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
                                end;
                                JSONWriter.Add('"');
                              end;
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR      : AddClob(ColumnCodePage);
        DT_DATE,
        DT_TIME,
        DT_TIMESTAMP,
        DT_TIMESTAMP_STRUCT : begin
                                if jcoMongoISODate in JSONComposeOptions then
                                  JSONWriter.AddShort('ISODate("')
                                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                  {$IFDEF MORMOT2}
                                  JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                                  {$ELSE}
                                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                  {$ENDIF}
                                else
                                  JSONWriter.Add('"');
                                if native_type = DT_TIMESTAMP_STRUCT then begin
                                  if PZSQLAnyDateTime(FData).Year < 0 then
                                    JSONWriter.Add('-');
                                  if (TZColumnInfo(ColumnsInfo[C]).ColumnType <> stTime) then begin
                                    DateToIso8601PChar(Pointer(fByteBuffer), True, Abs(PZSQLAnyDateTime(FData).Year),
                                    PZSQLAnyDateTime(FData).Month + 1, PZSQLAnyDateTime(FData).Day);
                                    JSONWriter.AddNoJSONEscape(Pointer(fByteBuffer),10);
                                  end else if jcoMongoISODate in JSONComposeOptions then
                                    JSONWriter.AddShort('0000-00-00');
                                  if (TZColumnInfo(ColumnsInfo[C]).ColumnType <> stDate) then begin
                                    TimeToIso8601PChar(Pointer(fByteBuffer), True, PZSQLAnyDateTime(FData).Hour,
                                    PZSQLAnyDateTime(FData).Minute, PZSQLAnyDateTime(FData).Second,
                                    PZSQLAnyDateTime(FData).MicroSecond div 1000, 'T', jcoMilliseconds in JSONComposeOptions);
                                    JSONWriter.AddNoJSONEscape(Pointer(fByteBuffer),9 + (4*Ord(jcoMilliseconds in JSONComposeOptions)));
                                  end;
                                end else
                                  JSONWriter.AddNoJSONEscape(FData, FDataLen);
                                if jcoMongoISODate in JSONComposeOptions
                                then JSONWriter.AddShort('Z)"')
                                else JSONWriter.Add('"');
                              end;
        DT_BINARY           : JSONWriter.WrBase64(FData, FDataLen, True);
        DT_LONGBINARY       : AddBlob;
        //DT_VARIABLE: ?
        DT_TINYINT          : JSONWriter.Add(PByte(FData)^);
        DT_BIGINT           : JSONWriter.Add(PInt64(FData)^);
        DT_UNSINT           : JSONWriter.AddU(PCardinal(FData)^);
        DT_UNSSMALLINT      : JSONWriter.AddU(PWord(FData)^);
        DT_UNSBIGINT        : JSONWriter.AddQ(PUInt64(FData)^);
        DT_BIT              : JSONWriter.AddShort(JSONBool[PByte(FData)^ <> 0]);
        else raise CreateConversionError(C);
      end;
        JSONWriter.Add(',');
    end else if JSONWriter.Expand then begin
      if not (jcsSkipNulls in JSONComposeOptions) then begin
        JSONWriter.AddString(JSONWriter.ColNames[I]);
        JSONWriter.AddShort('null,')
      end;
    end else JSONWriter.AddShort('null,')
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF WITH_COLUMNS_TO_JSON}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length in bytes of the raw String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractSQLAnywhereResultSet.GetPAnsiChar(ColumnIndex: Integer;
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
                      IntToRaw(Cardinal(PByte(FData)^), PAnsiChar(FByteBuffer), @Result);
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
                      IntToRaw(Integer(PSmallInt(FData)^), PAnsiChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSSMALLINT: begin
                      IntToRaw(Cardinal(PWord(FData)^), PAnsiChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_INT        : begin
                      IntToRaw(PInteger(FData)^, PAnsiChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSINT     : begin
                      IntToRaw(PCardinal(FData)^, PAnsiChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_BIGINT     : begin
                      IntToRaw(PInt64(FData)^, PAnsiChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSBIGINT  : begin
                      IntToRaw(PUInt64(FData)^, PAnsiChar(FByteBuffer), @Result);
set_Results:            Len := Result - PAnsiChar(FByteBuffer);
                      Result := PAnsiChar(FByteBuffer);
                    end;
    DT_FLOAT      : begin
                      Len := FloatToSQLRaw(PSingle(FData)^, PAnsiChar(FByteBuffer));
                      Result := PAnsiChar(FByteBuffer);
                    end;
    DT_DOUBLE     : begin
                      Len := FloatToSQLRaw(PDouble(FData)^, PAnsiChar(FByteBuffer));
                      Result := PAnsiChar(FByteBuffer);
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
    DT_LONGVARCHAR: if Fdata_info.data_size < SizeOf(TByteBuffer) then begin
                      Result := PAnsiChar(FByteBuffer);
                      ColumnIndex := FplainDriver.sqlany_get_data(Fa_sqlany_stmt^, ColumnIndex,
                        0, Result, SizeOf(TByteBuffer)-1);
                      if ColumnIndex < 0 then
                        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_data', Self);
                      Len := Cardinal(ColumnIndex);
                    end else FromLob;
    DT_TIMESTAMP_STRUCT : begin
                    Result := PAnsiChar(FByteBuffer);
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
function TZAbstractSQLAnywhereResultSet.GetPWideChar(ColumnIndex: Integer;
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
                      IntToUnicode(Cardinal(PByte(FData)^), PWideChar(FByteBuffer), @Result);
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
                      IntToUnicode(Integer(PSmallInt(FData)^), PWideChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSSMALLINT: begin
                      IntToUnicode(Cardinal(PWord(FData)^), PWideChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_INT        : begin
                      IntToUnicode(PInteger(FData)^, PWideChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSINT     : begin
                      IntToUnicode(PCardinal(FData)^, PWideChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_BIGINT     : begin
                      IntToUnicode(PInt64(FData)^, PWideChar(FByteBuffer), @Result);
                      goto set_Results;
                    end;
    DT_UNSBIGINT  : begin
                      IntToUnicode(PUInt64(FData)^, PWideChar(FByteBuffer), @Result);
set_Results:            Len := Result - PWideChar(FByteBuffer);
                      Result := PWideChar(FByteBuffer);
                    end;
    DT_FLOAT      : begin
                      Len := FloatToSQLUnicode(PSingle(FData)^, PWideChar(FByteBuffer));
                      Result := PWideChar(FByteBuffer);
                    end;
    DT_DOUBLE     : begin
                      Len := FloatToSQLUnicode(PDouble(FData)^, PWideChar(FByteBuffer));
                      Result := PWideChar(FByteBuffer);
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
    DT_LONGVARCHAR: if Fdata_info.data_size < SizeOf(TByteBuffer) then begin
                      Result := PWideChar(FByteBuffer);
                      ColumnIndex := FplainDriver.sqlany_get_data(Fa_sqlany_stmt^, ColumnIndex,
                        0, Result, SizeOf(TByteBuffer)-1);
                      if ColumnIndex < 0 then
                        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_data', Self);
                      Len := Cardinal(ColumnIndex);
                      goto Convert;
                    end else FromLob;
    DT_TIMESTAMP_STRUCT : begin
                    Result := PWideChar(FByteBuffer);
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

function TZAbstractSQLAnywhereResultSet.GetStringProp(Buf: PAnsiChar): String;
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

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTime</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero padded</code>
  @exception SQLException if a database access error occurs
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZAbstractSQLAnywhereResultSet.GetTime(ColumnIndex: Integer;
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


{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTimestamp</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero padded</code>
  @exception SQLException if a database access error occurs
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TM" does not seem to be initialized} {$ENDIF}
procedure TZAbstractSQLAnywhereResultSet.GetTimestamp(ColumnIndex: Integer;
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
function TZAbstractSQLAnywhereResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
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
function TZAbstractSQLAnywhereResultSet.GetULong(ColumnIndex: Integer): UInt64;
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
function TZAbstractSQLAnywhereResultSet.IsNull(ColumnIndex: Integer): Boolean;
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
        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_data_info', Self);
      Result := Fdata_info.is_null <> 0;
    end
  end;
end;

{**
  Resets the cursor position to initialstate and releases all resources.
}
procedure TZAbstractSQLAnywhereResultSet.ResetCursor;
begin
  inherited;
  FMaxBufIndex := FIteration;
  FFetchedMinRowNo := 0;
  FFetchedMaxRowNo := 0;
end;

{ TZSQLAnywhereResultSet }

function TZSQLAnywhereResultSet.MoveAbsolute(Row: Integer): Boolean;
var fetched_rows: Tsacapi_i32;
label jmpErr;
begin
  Result := False;
  if Closed or ((Row < 0) or ((MaxRows > 0) and (Row >= MaxRows))) or (Fa_sqlany_stmt^ = nil) then
    Exit;
  if Row = 0 //beforefirst state
  then Result := True
  else if (FIteration > 1) then begin
    if (Row < FFetchedMinRowNo) or (Row > FFetchedMaxRowNo) then begin
      Result := FPlainDriver.sqlany_fetch_absolute(Fa_sqlany_stmt^, Row) = 1;
      if Result then begin
        FCurrentRowOffset := 0;
        fetched_rows := FPlainDriver.sqlany_fetched_rows(Fa_sqlany_stmt^);
        if fetched_rows < 0 then
          goto jmpErr;
        FMaxBufIndex := Cardinal(fetched_rows);
        FFetchedMinRowNo := Row;
        FFetchedMaxRowNo := Row+fetched_rows-1;
        if LastRowNo < FFetchedMaxRowNo then
          LastRowNo := FFetchedMaxRowNo;
      end else goto jmpErr;
    end else begin //align to buffer offsets
      Result := True;
      fetched_rows := Row-FFetchedMinRowNo;
      FCurrentRowOffset := Cardinal(fetched_rows);
    end;
  end else begin
    Result := FPlainDriver.sqlany_fetch_absolute(Fa_sqlany_stmt^, Row) = 1;
    if Result then begin
      FCurrentRowOffset := 0;
      if Row > LastRowNo then
        LastRowNo := Row;
    end else
jmpErr: FSQLAnyConnection.HandleErrorOrWarning(lcFetch, 'sqlany_fetch_absolute', Self)
  end;
  RowNo := Row;
  if not Result and not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
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
label jmpErr;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (Fa_sqlany_stmt^ = nil) then
    Exit;
  RowNo := RowNo + 1;
  if (FIteration > 1) then begin
    Inc(FCurrentRowOffset);
    if (RowNo = 1) or ((FCurrentRowOffset = FMaxBufIndex) and (FMaxBufIndex = FIteration)) then begin
      FCurrentRowOffset := 0;
      Result := FPlainDriver.sqlany_fetch_next(Fa_sqlany_stmt^) = 1;
      if Result then begin
        fetched_rows := FPlainDriver.sqlany_fetched_rows(Fa_sqlany_stmt^);
        if fetched_rows < 0 then
          goto jmpErr;
        FMaxBufIndex := Cardinal(fetched_rows);
        FFetchedMinRowNo := RowNo;
        FFetchedMaxRowNo := RowNo + fetched_rows-1;
        Dec(fetched_rows);
        fetched_rows := RowNo+fetched_rows;
        if LastRowNo < fetched_rows then
          LastRowNo := fetched_rows;
      end else goto jmpErr;
    end else begin
      Result := FCurrentRowOffset < FMaxBufIndex;
      if not Result then
        Dec(FCurrentRowOffset);
    end;
  end else begin
    Result := FPlainDriver.sqlany_fetch_next(Fa_sqlany_stmt^) = 1;
    if Result then begin
      if (LastRowNo < RowNo) then
        LastRowNo := RowNo;
    end else begin
jmpErr:if (RowNo = 1) then
      FSQLAnyConnection.HandleErrorOrWarning(lcFetch, 'sqlany_fetch_next', Self)
    end;
  end;
  if not Result and not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
end;

{**
  Opens this recordset.
}
procedure TZSQLAnywhereResultSet.Open;
var i: Tsacapi_i32;
  RowSize: Tsize_t;
  sqlany_column_info: Pa_sqlany_column_info;
  ColumnInfo: TZSQLAnywhereColumnInfo;
  P: PAnsiChar;
var ABind: Pa_sqlany_data_value;
begin
  Fnum_cols := FplainDriver.sqlany_num_cols(Fa_sqlany_stmt^);
  if Fnum_cols < 1 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := Fnum_cols;
  RowSize := FBindValueSize * Fnum_cols;
  GetMem(FDataValues, RowSize);
  FillChar(FDataValues^, RowSize, #0);
  sqlany_column_info := Pointer(FByteBuffer);
  RowSize := 0;
  for i := 0 to Fnum_cols -1 do begin
    ColumnInfo := TZSQLAnywhereColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);
    if FPlainDriver.sqlany_get_column_info(Fa_sqlany_stmt^, I, sqlany_column_info) <> 1 then
      FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_column_info', Self);
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
        if ColumnInfo.NativeType = DT_FIXCHAR then
          ColumnInfo.Scale := ColumnInfo.Precision;
      end else if ColumnInfo.ColumnType = stUnicodeString then begin
        ColumnInfo.Precision := sqlany_column_info.max_size;
        sqlany_column_info.max_size := sqlany_column_info.max_size shl 2;
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;
        ColumnInfo.ColumnCodePage := zCP_UTF8;
        if ColumnInfo.NativeType = DT_NFIXCHAR then
          ColumnInfo.Scale := ColumnInfo.Precision;
      end else if ColumnInfo.ColumnType = stBytes then begin
        ColumnInfo.Precision := sqlany_column_info.max_size;
        ColumnInfo.CharOctedLength := sqlany_column_info.max_size;
        ColumnInfo.ColumnCodePage := zCP_Binary;
      end else if ColumnInfo.ColumnType = stBigDecimal then begin
        ColumnInfo.Precision := sqlany_column_info.precision;
        sqlany_column_info.max_size := sqlany_column_info.precision + 2; //sign, dot
        if (Fapi_version >= SQLANY_API_VERSION_4) then
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
        if (ColumnInfo.ColumnType = stFloat) {and (ApiVersion < SQLANY_API_VERSION_4)} then begin
          { sybase overruns our buffer even if maxsize is SizeOf(Single) }
          sqlany_column_info.max_size := SizeOf(Double);
          ColumnInfo.NativeType := DT_DOUBLE;
        end;
      end;
      if Ord(ColumnInfo.ColumnType) >= Ord(stCurrency) then
        RowSize := RowSize + SizeOf(Tsize_t); //lengthes are required for varible types only
      RowSize := RowSize + SizeOf(Tsacapi_bool); //add nullable row
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
    if (Fapi_version >= SQLANY_API_VERSION_4) then begin
      ColumnInfo.TableName := GetStringProp(Pa_sqlany_column_infoV4up(sqlany_column_info).table_name);
      ColumnInfo.Writable := ColumnInfo.TableName <> '';
    end;
    ABind := Pointer(PAnsiChar(FDataValues)+(FBindValueSize*I));
    ABind.buffer_size := sqlany_column_info.max_size;
    ABind._type := sqlany_column_info._type;
    ColumnInfo.Bind := Pointer(ABind);
    RowSize := RowSize + sqlany_column_info.max_size;
  end;
  if (Fapi_version < SQLANY_API_VERSION_4)
  then FIteration := 0
  else begin
    FIteration := FZBufferSize div RowSize;
    if (FIteration = 0) or (Fdata_info <> nil) then
      FIteration := 1
  end;
  if FIteration > 1 then
    if FplainDriver.sqlany_set_rowset_size(Fa_sqlany_stmt^, FIteration) <> 1 then
      FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_set_rowset_size', Self);
  FMaxBufIndex := FIteration;
  { allocate just one block of memory for the data buffers }
  GetMem(FColumnData, (RowSize * FIteration));
  P := FColumnData;
  for i := 0 to Fnum_cols -1 do with TZSQLAnywhereColumnInfo(ColumnsInfo[i]) do begin
    if Ord(ColumnType) >= Ord(stAsciiStream) then continue; //null buffers can't be bound
    ABind := Pointer(Bind);
    ABind.is_null := FColumnData;
    if Ord(ColumnType) >= Ord(stCurrency) then begin
      Inc(PAnsiChar(FColumnData), SizeOf(Tsacapi_i32)*FIteration);
      ABind.length := FColumnData;
    end else ABind.length := nil;
    Inc(PAnsiChar(FColumnData), SizeOf(Tsize_t)*FIteration);
      ABind.buffer := PAnsiChar(FColumnData);
      Inc(PAnsiChar(FColumnData), ABind.buffer_size*FIteration);
    if ((FIteration > 1) or (Fapi_version >= SQLANY_API_VERSION_4)) then begin
      Bind.is_address := 0;
      if FplainDriver.sqlany_bind_column(Fa_sqlany_stmt^, Cardinal(I), Pointer(ABind)) <> 1 then
        FSQLAnyConnection.HandleErrorOrWarning(lcOther, 'sqlany_bind_column', Self);
      IsBound := True;
    end;
    if not IsBound and (Fdata_info = nil) then //need that for IsNull()
      GetMem(Fdata_info, SizeOf(Ta_sqlany_data_info));
  end;
  FColumnData := P;
  inherited Open;
  FCursorLocation := rctServer;
end;

{ TZSQLAnywhereCachedResultSet }

class function TZSQLAnywhereCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZSQLAnywhereRowAccessor
end;

{ TZSQLAnywhereRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobCacheMode" not used} {$ENDIF}
constructor TZSQLAnywhereRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  LobCacheMode: TLobCacheMode);
begin
  inherited Create(ColumnsInfo, ConSettings, OpenLobStreams, lcmNone);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZSQLAnywhereRowAccessor.FetchLongData(AsStreamedType: TZSQLType;
  const ResultSet: IZResultSet; ColumnIndex: Integer; Data: PPZVarLenData);
const
  MaxBufSize = $F000;
var Lob: IZBlob;
  Stream: TStream;
  Len, BufSize, ReadBytes: Cardinal;
  buf: PAnsiChar;
begin
  if Data^ <> nil then
    FreeMem(Data^);
  Lob := ResultSet.GetBlob(ColumnIndex);
  Stream := Lob.GetStream;
  try
    Len := Stream.Size;
    if Len = 0 then Exit;
    if Len > MaxBufSize
    then BufSize := MaxBufSize
    else BufSize := Len;
    GetMem(Data^, SizeOf(Cardinal)+Len+Byte(AsStreamedType=stAsciiStream));
    Data^.Len := Len;
    buf := @Data^.Data;
    while Len <> 0 do begin
      ReadBytes := Stream.Read(Buf^, BufSize);
      Dec(Len, ReadBytes);
      Inc(Buf, readBytes);
      if Len < BufSize then
        BufSize := Len;
    end;
    if AsStreamedType = stAsciiStream then
      PByte(Buf)^ := 0;
  finally
    Stream.Free;
    Lob := nil;
  end;
end;

class function TZSQLAnywhereRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings;
  Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stUnicodeString, stUnicodeStream] then
    Result := TZSQLType(Byte(Result)-1); // no national chars 4 SQLAny
  { eh: we can stream the data with SQLAny, but we need always to fetch the
    current row and we have no descriptor so we cache the data }
  if (Result in [stAsciiStream, stBinaryStream]) then
    Result := TZSQLType(Byte(Result)-3);
  if Result = stString then
    ColumnCodePage := ConSettings.ClientCodePage.CP;
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
      FOwnerLob.FConnection.HandleErrorOrWarning(lcOther, 'sqlany_get_data', Self);
    FPosition := FPosition+TSize_t(Result);
  end;
end;

function TZSQLAnyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soEnd then
    Result := 0
  else if Origin = soCurrent then
    Result := Int64(FPosition) + OffSet
  else
    Result := OffSet;
  if Result <> FPosition then
    FPosition := Result;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "Buffer/Count" not used}
{$ENDIF}
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

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZSQLAnyLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
    if LobStreamMode <> lsmRead then
      raise CreateReadOnlyException;
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

const
  a_sqlany_data_type2SQLType: array[Ta_sqlany_data_type] of TZSQLType = (
    {A_INVALID_TYPE}  stUnknown,
    {A_BINARY}        stBytes,
    {A_STRING}        stString,
    {A_DOUBLE}        stDouble,
    {A_VAL64}         stLong,
    {A_UVAL64}        stULong,
    {A_VAL32}         stInteger,
    {A_UVAL32}        stLongWord,
    {A_VAL16}         stSmall,
    {A_UVAL16}        stWord,
    {A_VAL8}          stShort,
    {A_UVAL8}         stByte,
    {A_FLOAT}         stBigDecimal
    );
const
  SQLType2a_sqlany_data_type: array[TZSQLType] of Ta_sqlany_native_type = (
    {stUnknown}       DT_NOTYPE,
    {stBoolean}       DT_BIT,
    {stByte}          DT_TINYINT,
    {stShort}         DT_TINYINT,
    {stWord}          DT_UNSSMALLINT,
    {stSmall}         DT_SMALLINT,
    {stLongWord}      DT_UNSINT,
    {stInteger}       DT_INT,
    {stULong}         DT_UNSBIGINT,
    {stLong}          DT_BIGINT,
    {stFloat}         DT_FLOAT,
    {stDouble}        DT_DOUBLE,
    {stCurrency}      DT_DECIMAL,
    {stBigDecimal}    DT_DECIMAL,
    {stDate}          DT_DATE,
    {stTime}          DT_TIME,
    {stTimestamp}     DT_TIMESTAMP,
    {stGUID}          DT_VARCHAR,
    {stString}        DT_VARCHAR,
    {stUnicodeString} DT_NVARCHAR,
    {stBytes}         DT_BINARY,
    {stAsciiStream}   DT_LONGVARCHAR,
    {stUnicodeStream} DT_LONGNVARCHAR,
    {stBinaryStream}  DT_LONGBINARY,
    {stArray}         DT_NOTYPE,
    {stDataSet}       DT_NOTYPE
    );

{ TZSQLAynwhereOutParamResultSet }

constructor TZSQLAynwhereOutParamResultSet.Create(const Statement: IZStatement;
  const SQL: string; a_sqlany_stmt: PPa_sqlany_stmt;
  a_sqlany_bind_paramArray: Pa_sqlany_bind_paramArray; BindList: TZBindList);
var I: Cardinal;
    Bind: Pa_sqlany_bind_param;
    BindV4: Pa_sqlany_bind_paramV4Up absolute Bind;
    ColumnInfo: TZSQLAnywhereColumnInfo;
    BindParamSize: NativeUint;
begin
  inherited Create(Statement, SQL, a_sqlany_stmt);
  if Fapi_version >= SQLANY_API_VERSION_4
  then BindParamSize := SizeOf(Ta_sqlany_bind_paramV4up)
  else BindParamSize := SizeOf(Ta_sqlany_bind_param);
  for i := 0 to BindList.Count -1 do begin
    Bind := Pointer(PAnsiChar(a_sqlany_bind_paramArray) + (BindParamSize * I));
    if Ord(Bind.direction) >= Ord(DD_OUTPUT) then begin
      Inc(Fnum_cols);
      ColumnInfo := TZSQLAnywhereColumnInfo.Create;
      ColumnsInfo.Add(ColumnInfo);
      if Fapi_version >= SQLANY_API_VERSION_4
      then ColumnInfo.ColumnLabel := GetStringProp(BindV4.name)
      else ColumnInfo.ColumnLabel := GetStringProp(Bind.name);
      ColumnInfo.Precision := Bind.value.buffer_size;
      ColumnInfo.ColumnType := BindList[i].SQLType;
      if ColumnInfo.ColumnType = stUnknown then
        ColumnInfo.ColumnType := a_sqlany_data_type2SQLType[Bind.value._type];
      ColumnInfo.NativeType := SQLType2a_sqlany_data_type[ColumnInfo.ColumnType];
      ColumnInfo.Bind := Pointer(@Bind.Value);
      ColumnInfo.IsBound := True;
    end;
  end;
  LastRowNo := 1;
  FCursorLocation := rctClient;
end;

function TZSQLAynwhereOutParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  if (Row >= 0) and (Row <= 2) then
    RowNo := Row;
end;

function TZSQLAynwhereOutParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 2; //set AfterLast
end;

{ TZSQLAnyWhereResultSetMetadataV4Up }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZSQLAnyWhereResultSetMetadataV4Up.ClearColumn(
  ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZSQLAnyWhereResultSetMetadataV4Up.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

initialization
{$ENDIF ZEOS_DISABLE_SQLANY}
end.
