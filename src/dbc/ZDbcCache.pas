{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Caching Classes and Interfaces               }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcCache;

interface

{$I ZDbc.inc}


uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Contnrs,
  ZClasses, ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZVariant,
  ZCompatibility;

type

  {** Defines a row status type. }
  TZRowUpdateType = (utUnmodified, utModified, utInserted, utDeleted);
  TZRowUpdateTypes = set of TZRowUpdateType;

  TZByteArray = array[0..4096 * SizeOf(Pointer)] of Byte;
  {** Defines a header for row buffer. }
  {ludob. Notes on alignment:
  Columns contains a record per field with the structure
    null:byte;
    fielddata:anything;
  field records are addressed through offsets in Columns stored in FColumnOffsets.
  Since anything can be stored as fielddata including pointers, fielddata needs
  to be aligned to pointer. To do this Columns is aligned to pointer and
  FColumnOffsets is aligned to pointer - 1 (the null:byte). The latter is
  done in TZRowAccessor.Create where FColumnOffsets is filled in.
  FPC_REQUIRES_PROPER_ALIGNMENT is a fpc build in define}
  TZRowBuffer = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    Index: Integer;
    UpdateType: TZRowUpdateType;
    BookmarkFlag: Byte;
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    dummyalign:pointer;
    {$endif}
    Columns: TZByteArray;
  end;
  PZRowBuffer = ^TZRowBuffer;

  {** Implements a abstract column buffer accessor. }
  TZRowAccessor = class(TObject)
  private
    FRawTemp: RawByteString;
    FUniTemp: ZWideString;
    FRowSize: Integer;
    FColumnsSize: Integer;
    FColumnCount: Integer;
    FColumnNames: array of string;
    FColumnCases: array of Boolean;
    FColumnTypes: array of TZSQLType;
    FColumnLengths: array of Integer;
    FColumnOffsets: array of Integer;
    FColumnDefaultExpressions: array of string;
    FColumnCodePages: array of Word;
    FBuffer: PZRowBuffer;
    FHasBlobs: Boolean;
    FConSettings: PZConSettings;

    function GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
    function GetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer): IZBlob;
    procedure SetBlobObject(const Buffer: PZRowBuffer; const ColumnIndex: Integer;
      const Value: IZBlob);
    function InternalGetBytes(const Buffer: PZRowBuffer; const ColumnIndex: Integer): TByteDynArray; {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetBytes(const Buffer: PZRowBuffer; const ColumnIndex: Integer;
      const Value: TByteDynArray; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetString(const Buffer: PZRowBuffer; const ColumnIndex: Integer;
      const Value: RawByteString; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetUnicodeString(const Buffer: PZRowBuffer; const ColumnIndex: Integer;
      const Value: ZWideString; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPAnsiChar(const Buffer: PZRowBuffer;
      const ColumnIndex: Integer; const Value: PAnsiChar;
      Const Len: Cardinal; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPWideChar(const Buffer: PZRowBuffer;
      const ColumnIndex: Integer; const Value: PWideChar;
      Const Len: Cardinal; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
  protected
    procedure CheckColumnIndex(Const ColumnIndex: Integer);
    procedure CheckColumnConvertion(Const ColumnIndex: Integer; ResultType: TZSQLType);
    function CompareString(Value1, Value2: Pointer): Integer; virtual; abstract;
    property ConSettings: PZConSettings read FConSettings;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings);

    function AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
    procedure InitBuffer(Buffer: PZRowBuffer);
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); virtual; abstract;
    procedure MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); virtual; abstract;
    procedure ClearBuffer(Buffer: PZRowBuffer);
    procedure DisposeBuffer(Buffer: PZRowBuffer);

    function CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
      ColumnIndices: TIntegerDynArray; ColumnDirs: TBooleanDynArray): Integer;

    function Alloc: PZRowBuffer;
    procedure Init;
    procedure CopyTo(DestBuffer: PZRowBuffer);
    procedure CopyFrom(SrcBuffer: PZRowBuffer);
    procedure MoveTo(DestBuffer: PZRowBuffer);
    procedure MoveFrom(SrcBuffer: PZRowBuffer);
    procedure CloneTo(DestBuffer: PZRowBuffer);
    procedure CloneFrom(SrcBuffer: PZRowBuffer);
    procedure Clear;
    procedure Dispose;

    function GetColumnData(Const ColumnIndex: Integer; var IsNull: Boolean): Pointer;
    function GetColumnDataSize(Const ColumnIndex: Integer): Integer;

    function GetColumnName(Const ColumnIndex: Integer): string;
    function GetColumnCase(Const ColumnIndex: Integer): Boolean;
    function GetColumnType(Const ColumnIndex: Integer): TZSQLType;
    function GetColumnLength(Const ColumnIndex: Integer): Integer;
    function GetColumnOffSet(Const ColumnIndex: Integer): Integer;
    function GetColumnDefaultExpression(Const ColumnIndex: Integer): string;
    procedure SetColumnDefaultExpression(Const ColumnIndex: Integer; const Value: string);
    procedure SetColumnCodePage(Const ColumnIndex: Integer; const Value: Word);

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(Const ColumnIndex: Integer): Boolean;
    function GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec; virtual;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; virtual; abstract;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; virtual;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; virtual;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; virtual;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; virtual;
    function GetWideRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZWideRec; virtual;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; virtual;
    function GetBoolean(Const ColumnIndex: Integer; var IsNull: Boolean): Boolean; virtual;
    function GetByte(Const ColumnIndex: Integer; var IsNull: Boolean): Byte; virtual;
    function GetShort(Const ColumnIndex: Integer; var IsNull: Boolean): ShortInt; virtual;
    function GetSmall(Const ColumnIndex: Integer; var IsNull: Boolean): SmallInt; virtual;
    function GetInt(Const ColumnIndex: Integer; var IsNull: Boolean): Integer; virtual;
    function GetLong(Const ColumnIndex: Integer; var IsNull: Boolean): Int64; virtual;
    function GetFloat(Const ColumnIndex: Integer; var IsNull: Boolean): Single; virtual;
    function GetDouble(Const ColumnIndex: Integer; var IsNull: Boolean): Double; virtual;
    function GetBigDecimal(Const ColumnIndex: Integer; var IsNull: Boolean): Extended; virtual;
    function GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean): TByteDynArray; virtual;
    function GetDate(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime; virtual;
    function GetTime(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime; virtual;
    function GetTimestamp(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime; virtual;
    function GetAsciiStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetUnicodeStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBinaryStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBlob(Const ColumnIndex: Integer; var IsNull: Boolean): IZBlob;
    function GetDataSet(Const ColumnIndex: Integer; var IsNull: Boolean): IZDataSet;
    function GetValue(Const ColumnIndex: Integer): TZVariant;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetNotNull(Const ColumnIndex: Integer);
    procedure SetNull(Const ColumnIndex: Integer);
    procedure SetBoolean(Const ColumnIndex: Integer; const Value: Boolean); virtual;
    procedure SetByte(Const ColumnIndex: Integer; const Value: Byte); virtual;
    procedure SetShort(Const ColumnIndex: Integer; const Value: ShortInt); virtual;
    procedure SetSmall(Const ColumnIndex: Integer; const Value: SmallInt); virtual;
    procedure SetInt(Const ColumnIndex: Integer; const Value: Integer); virtual;
    procedure SetLong(Const ColumnIndex: Integer; const Value: Int64); virtual;
    procedure SetFloat(Const ColumnIndex: Integer; const Value: Single); virtual;
    procedure SetDouble(Const ColumnIndex: Integer; const Value: Double); virtual;
    procedure SetBigDecimal(Const ColumnIndex: Integer; const Value: Extended); virtual;
    procedure SetString(Const ColumnIndex: Integer; const Value: String); virtual;
    procedure SetAnsiRec(Const ColumnIndex: Integer; const Value: TZAnsiRec);virtual;
    procedure SetPAnsiChar(Const ColumnIndex: Integer; const Value: PAnsiChar); virtual;
    procedure SetWideRec(Const ColumnIndex: Integer; const Value: TZWideRec);virtual;
    procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); virtual;
    procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); virtual;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); virtual;
    procedure SetBytes(Const ColumnIndex: Integer; const Value: TByteDynArray); virtual;
    procedure SetDate(Const ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTime(Const ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTimestamp(Const ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure SetAsciiStream(Const ColumnIndex: Integer; const Value: TStream);
    procedure SetUnicodeStream(Const ColumnIndex: Integer; const Value: TStream);
    procedure SetBinaryStream(Const ColumnIndex: Integer; const Value: TStream);
    procedure SetBlob(Const ColumnIndex: Integer; const Value: IZBlob);
    procedure SetDataSet(Const ColumnIndex: Integer; const Value: IZDataSet);
    procedure SetValue(Const ColumnIndex: Integer; const Value: TZVariant);

    property ColumnsSize: Integer read FColumnsSize;
    property RowSize: Integer read FRowSize;
    property RowBuffer: PZRowBuffer read FBuffer write FBuffer;
  end;

  {** Implements a raw-string based column buffer accessor. }
  TZRawRowAccessor = class(TZRowAccessor)
  protected
    function CompareString(ValuePtr1, ValuePtr2: Pointer): Integer; override;
  public
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); override;
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec; override;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; override;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; override;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; override;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; override;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; override;
    function GetWideRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZWideRec; override;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetString(Const ColumnIndex: Integer; const Value: String); override;
    procedure SetAnsiRec(Const ColumnIndex: Integer; const Value: TZAnsiRec); override;
    procedure SetWideRec(Const ColumnIndex: Integer; const Value: TZWideRec); override;
    //procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); override;
    //procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); override;
  end;

  {** Implements a unicode-string based column buffer accessor. }
  TZUnicodeRowAccessor = class(TZRowAccessor)
  protected
    function CompareString(ValuePtr1, ValuePtr2: Pointer): Integer; override;
  public
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); override;
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer); override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec; override;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; override;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; override;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; override;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; override;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; override;
    function GetWideRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZWideRec; override;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetString(Const ColumnIndex: Integer; const Value: String); override;
    procedure SetAnsiRec(Const ColumnIndex: Integer; const Value: TZAnsiRec); override;
    procedure SetWideRec(Const ColumnIndex: Integer; const Value: TZWideRec); override;
    //procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); override;
    //procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); override;
  end;

const
  RowHeaderSize = SizeOf(TZRowBuffer) - SizeOf(TZByteArray);

implementation

uses ZFastcode, Math, ZMessages, ZSysUtils, ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

const
  PAnsiInc = SizeOf(Cardinal);
  PWideInc = SizeOf(Word); //PWide inc assumes allways two byte

{ TZRowAccessor }

{**
  Creates this object and assignes the main properties.
  @param ColumnsInfo a collection with column information.
}
constructor TZRowAccessor.Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings);
var
  I: Integer;
  Current: TZColumnInfo;
begin
  FConSettings := ConSettings;
  FBuffer := nil;
  FColumnCount := ColumnsInfo.Count;
  FColumnsSize := 0;
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  FColumnsSize:=align(FColumnsSize+1,sizeof(pointer))-1;
  {$endif}
  SetLength(FColumnNames, FColumnCount);
  SetLength(FColumnCases, FColumnCount);
  SetLength(FColumnTypes, FColumnCount);
  SetLength(FColumnLengths, FColumnCount);
  SetLength(FColumnOffsets, FColumnCount);
  SetLength(FColumnDefaultExpressions, FColumnCount);
  SetLength(FColumnCodePages, FColumnCount);
  FHasBlobs := False;

  for I := 0 to FColumnCount - 1 do
  begin
    Current := TZColumnInfo(ColumnsInfo[I]);
    FColumnNames[I] := Current.ColumnName;
    FColumnCases[I] := Current.CaseSensitive;
    FColumnTypes[I] := Current.ColumnType;
    FColumnLengths[I] := GetColumnSize(Current);
    FColumnOffsets[I] := FColumnsSize;
    FColumnDefaultExpressions[I] := Current.DefaultExpression;
    FColumnCodePages[I] := Current.ColumnCodePage;
    Inc(FColumnsSize, FColumnLengths[I] + 1);
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    FColumnsSize:=align(FColumnsSize+1,sizeof(pointer))-1;
    {$endif}
    if Current.ColumnType in [stBytes, stString, stUnicodeString] then
      FColumnLengths[I] := Current.Precision;
    if Current.ColumnType = stGUID then
      FColumnLengths[I] := 16;
    if FColumnsSize > SizeOf(TZByteArray)-1 then
      raise EZSQLException.Create(SRowBufferWidthExceeded);
    FHasBlobs := FHasBlobs
      or (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream]);
  end;
  FRowSize := FColumnsSize + RowHeaderSize;
end;

{**
  Checks is the column index correct and row buffer is available.
  @param ColumnIndex an index of column.
}
procedure TZRowAccessor.CheckColumnIndex(Const ColumnIndex: Integer);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
  @return <code>true</code> if convertion is allowed or throw exception
    otherwise.
}
procedure TZRowAccessor.CheckColumnConvertion(Const ColumnIndex: Integer;
  ResultType: TZSQLType);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
  end;

  if not CheckConvertion(FColumnTypes[ColumnIndex - 1], ResultType) then
  begin
    raise EZSQLException.Create(
      Format(SConvertionIsNotPossible, [ColumnIndex,
      DefineColumnTypeName(FColumnTypes[ColumnIndex - 1]),
      DefineColumnTypeName(ResultType)]));
  end;
end;

{**
  Gets a size of column with the specified type.
  @param ColumnInfo a column information struct.
  @return a size for the column with the specified type.
}
function TZRowAccessor.GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
begin
  case ColumnInfo.ColumnType of
    stBoolean:
      Result := SizeOf(WordBool);
    stByte:
      Result := SizeOf(Byte);
    stShort:
      Result := SizeOf(ShortInt);
    stSmall:
      Result := SizeOf(SmallInt);
    stInteger:
      Result := SizeOf(Integer);
    stLong:
      Result := SizeOf(Int64);
    stFloat:
      Result := SizeOf(Single);
    stDouble:
      Result := SizeOf(Double);
    stBigDecimal:
      Result := SizeOf(Extended);
    stString:
      Result := SizeOf(Pointer);
    stUnicodeString:
      Result := SizeOf(Pointer);
    stBytes, stGUID:
      Result := SizeOf(Pointer) + SizeOf(SmallInt);
    stDate, stTime, stTimestamp:
      Result := SizeOf(TDateTime);
    stAsciiStream, stUnicodeStream, stBinaryStream, stDataSet:
      Result := SizeOf(Pointer);
    else
      Result := 0;
  end;
end;

{**
  Gets a stream from the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
}
function TZRowAccessor.GetBlobObject(Buffer: PZRowBuffer;
  ColumnIndex: Integer): IZBlob;
var
  BlobPtr: PPointer;
  NullPtr: {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF};
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  {$IFNDEF FPC}
  if NullPtr^ = {$IFDEF WIN64}false{$ELSE}0{$ENDIF} then  //M.A. if NullPtr^ = 0 then
  {$ELSE}
  if NullPtr^ = 0 then
  {$ENDIF}
    Result := IZBlob(BlobPtr^)
  else
    Result := nil;
end;

{**
  Sets a blob into the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
  @param Value a stream object to be set.
}
procedure TZRowAccessor.SetBlobObject(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: IZBlob);
var
  BlobPtr: PPointer;
  NullPtr: {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF};
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  {$IFNDEF FPC}
  if NullPtr^ = {$IFDEF WIN64}false{$ELSE}0{$ENDIF} then  //M.A. if NullPtr^ = 0 then
  {$ELSE}
  if NullPtr^ = 0 then
  {$ENDIF}
    IZBlob(BlobPtr^) := nil
  else
    BlobPtr^ := nil;

  IZBlob(BlobPtr^) := Value;

  if Value <> nil then
  {$IFNDEF FPC}
    NullPtr^ := {$IFDEF WIN64}false{$ELSE}0{$ENDIF}  //M.A. NullPtr^ := 0
  else
    NullPtr^ := {$IFDEF WIN64}true{$ELSE}1{$ENDIF};  //M.A. NullPtr^ := 1;
  {$ELSE}
    NullPtr^ := 0
  else
    NullPtr^ := 1;
  {$ENDIF}
end;

function TZRowAccessor.InternalGetBytes(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer): TByteDynArray;
var
  P: PPointer;
  L: SmallInt;
begin
  Result := nil;
  if ( Buffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 )then
  begin
    L := PSmallInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1 + SizeOf(Pointer)])^;
    SetLength(Result, L);
    if L > 0 then
    begin
      P := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
      Move(P^^, Pointer(Result)^, L);
    end;
  end;
end;

procedure TZRowAccessor.InternalSetBytes(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: TByteDynArray;
  const NewPointer: Boolean = False);
var
  P: PPointer;
  L: SmallInt;
begin
  if Assigned(Buffer) then
  begin
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
    P := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
    L := Min(Length(Value), FColumnLengths[ColumnIndex - 1]);
    PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1 + SizeOf(Pointer)])^ := L;
    if L > 0 then
    begin
      ReallocMem(P^, L);
      System.Move(Pointer(Value)^, P^^, L);
    end
    else
      if PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ > 0 then
      begin
        System.FreeMem(P^);
        PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
      end;
  end;
end;

procedure TZRowAccessor.InternalSetString(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: RawByteString;
  const NewPointer: Boolean = False);
var
  C: PPAnsiChar;
  L: Cardinal;
begin
  if Buffer <> nil then
  begin
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
    C := PPAnsiChar(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
    L := Length(Value);
    ReallocMem(C^, L +SizeOf(Cardinal)+1);
    System.Move(Value[1], (C^+PAnsiInc)^, L);
    PCardinal(C^)^ := L;
    (C^+PAnsiInc+L)^ := #0; //set #0 terminator if a truncation is required e.g. FireBird Char columns with trailing spaces
  end;
end;

procedure TZRowAccessor.InternalSetUnicodeString(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: ZWideString;
  const NewPointer: Boolean = False);
var
  W: ZPPWideChar;
  LStr, LMem: Cardinal;
begin
  if Buffer <> nil then
  begin
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
    W := ZPPWideChar(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
    LStr := Length(Value);
    LMem := LStr * 2;
    ReallocMem(W^, LMem+SizeOf(Cardinal)+2); //including #0#0 terminator
    System.Move(Value[1], (W^+PWideInc)^, LMem);
    PCardinal(W^)^ := LStr;
    (W^+PWideInc+LStr)^ := WideChar(#0);
  end;
end;

procedure TZRowAccessor.InternalSetPWideChar(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: PWideChar; const Len: Cardinal;
  const NewPointer: Boolean = False);
var
  W: ZPPWideChar;
  LMem: Cardinal;
begin
  if Buffer <> nil then
  begin
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
    W := ZPPWideChar(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
    LMem := Len * 2;
    ReallocMem(W^, LMem+SizeOf(Cardinal)+2); //including #0#0 terminator
    System.Move(Value^, (W^+PWideInc)^, LMem);
    PCardinal(W^)^ := Len;
    (W^+PWideInc+Len)^ := WideChar(#0);
  end;
end;


procedure TZRowAccessor.InternalSetPAnsiChar(const Buffer: PZRowBuffer;
  const ColumnIndex: Integer; const Value: PAnsiChar; const Len: Cardinal;
  const NewPointer: Boolean = False);
var
  C: PPAnsiChar;
begin
  if Buffer <> nil then
  begin
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
    C := PPAnsiChar(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
    ReallocMem(C^, Len+SizeOf(Cardinal)+1);
    Move(Value^, (C^+PAnsiInc)^, Len);
    PCardinal(C^)^ := Len;
    (C^+PAnsiInc+Len)^ := #0; //set #0 terminator if a truncation is required e.g. FireBird Char columns with trailing spaces
  end;
end;

{**
  Allocates a new row buffer and sets it into the variable.
  @param Buffer a pointer to row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
begin
  GetMem(Buffer, FRowSize);
  InitBuffer(Buffer);
  Result := Buffer;
end;

{**
  Disposes the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.DisposeBuffer(Buffer: PZRowBuffer);
begin
  if Assigned(Buffer) then
  begin
    ClearBuffer(Buffer);
    FreeMem(Buffer);
  end;
end;

{**
  Initializes the row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.InitBuffer(Buffer: PZRowBuffer);
var
  I : Integer;
begin
  if Assigned(Buffer) then
    with Buffer^ do
    begin
      Index := 0;
      BookmarkFlag := 0;//bfCurrent;
      UpdateType := utUnmodified;
      FillChar(Columns, FColumnsSize, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
      for I := 0 to FColumnCount - 1 do Columns[FColumnOffsets[I]] := 1;
    end;
end;

{**
  Moves the row buffer from source to destination row.
  Source buffer is cleaned up after the operation.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, DestBuffer);
  ClearBuffer(SrcBuffer);
end;

{**
  Compares fields from two row buffers.
  @param Buffer1 the first row buffer to compare.
  @param Buffer2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZRowAccessor.CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
  ColumnIndices: TIntegerDynArray; ColumnDirs: TBooleanDynArray): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  Length1, Length2: SmallInt;
  ValuePtr1, ValuePtr2: Pointer;
  Blob1, Blob2: IZBlob;
  BlobEmpty1, BlobEmpty2: Boolean;
  Bts1, Bts2: TByteDynArray;

  function CompareFloat(Value1, Value2: Extended): Integer;
  begin
    Value1 := Value1 - Value2;
    if Value1 > 0 then
      Result := 1
    else if Value1 < 0 then
      Result := -1
    else
      Result := 0;
  end;

  function CompareBool(Value1, Value2: Boolean): Integer;
  begin
    if Value1 = Value2 then
      Result := 0
    else if Value1 then
      Result := 1
    else
      Result := -1;
  end;

  function CompareInt64(Value1, Value2: Int64): Integer;
  begin
    Value1 := Value1 - Value2;
    if Value1 > 0 then
      Result := 1
    else if Value1 < 0 then
      Result := -1
    else
      Result := 0;
  end;

begin
  Result := 0;
  for I := Low(ColumnIndices) to High(ColumnIndices) do
  begin
    ColumnIndex := ColumnIndices[I] - 1;
    { Checks for both Null columns. }
    if (Buffer1.Columns[FColumnOffsets[ColumnIndex]] = 1) and
      (Buffer2.Columns[FColumnOffsets[ColumnIndex]] = 1) then
      Continue;
    { Checks for not-Null and Null columns. }
    if Buffer1.Columns[FColumnOffsets[ColumnIndex]] <>
      Buffer2.Columns[FColumnOffsets[ColumnIndex]] then
    begin
      if not (FColumnTypes[ColumnIndex]
        in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
      begin
        Result := Buffer2.Columns[FColumnOffsets[ColumnIndex]] -
          Buffer1.Columns[FColumnOffsets[ColumnIndex]];
        if not ColumnDirs[I] then
          Result := -Result;
        Break;
      end;
    end;
    { Compares column values. }
    ValuePtr1 := @Buffer1.Columns[FColumnOffsets[ColumnIndex] + 1];
    ValuePtr2 := @Buffer2.Columns[FColumnOffsets[ColumnIndex] + 1];
    case FColumnTypes[ColumnIndex] of
      stByte:
        Result := PShortInt(ValuePtr1)^ - PShortInt(ValuePtr2)^;
      stSmall:
        Result := PSmallInt(ValuePtr1)^ - PSmallInt(ValuePtr2)^;
      stInteger:
        Result := PInteger(ValuePtr1)^ - PInteger(ValuePtr2)^;
      stLong:
        Result := CompareInt64(PInt64(ValuePtr1)^, PInt64(ValuePtr2)^);
      stFloat:
        Result := CompareFloat(PSingle(ValuePtr1)^, PSingle(ValuePtr2)^);
      stDouble:
        Result := CompareFloat(PDouble(ValuePtr1)^, PDouble(ValuePtr2)^);
      stBigDecimal:
        Result := CompareFloat(PExtended(ValuePtr1)^, PExtended(ValuePtr2)^);
      stBoolean:
        Result := CompareBool(PWordBool(ValuePtr1)^, PWordBool(ValuePtr2)^);
      stDate, stTime, stTimestamp:
        Result := CompareFloat(PDateTime(ValuePtr1)^, PDateTime(ValuePtr2)^);
      stUnicodeString, stString:
        Result := CompareString(ValuePtr1, ValuePtr2);
      stBytes,stGUID:
        begin
          Length1 := PSmallInt(@Buffer1.Columns[FColumnOffsets[ColumnIndex]
            + 1 + SizeOf(Pointer)])^;
          Length2 := PSmallInt(@Buffer2.Columns[FColumnOffsets[ColumnIndex]
            + 1 + SizeOf(Pointer)])^;
          Result := Length1 - Length2;
          if Result = 0 then
          begin
            Bts1 := InternalGetBytes(Buffer1, ColumnIndex+1);
            Bts2 := InternalGetBytes(Buffer2, ColumnIndex+1);
            if (Assigned(Bts1) and Assigned(Bts2)) then
              Result := ZMemLComp(Pointer(Bts1), Pointer(Bts2), Length1)
            else if not Assigned(Bts1) and not Assigned(Bts2) then
              Result := 0
            else if Assigned(Bts1) then
              Result := 1
            else
              Result := -1;
          end;
        end;
      stAsciiStream, stBinaryStream, stUnicodeStream:
        begin
          Blob1 := GetBlobObject(Buffer1, ColumnIndex + 1);
          BlobEmpty1 := (Blob1 = nil) or (Blob1.IsEmpty);
          Blob2 := GetBlobObject(Buffer2, ColumnIndex + 1);
          BlobEmpty2 := (Blob2 = nil) or (Blob2.IsEmpty);
          if BlobEmpty1 and BlobEmpty2 then
            Continue
          else if (BlobEmpty1 <> BlobEmpty2) then
            if BlobEmpty1 then
              Result := -1
            else
              Result := 1
          else
            if Blob1.IsUpdated or Blob2.IsUpdated then
              if FColumnTypes[ColumnIndex] = stBinaryStream then
              begin
                Result := Blob1.Length - Blob2.Length;
                if Result = 0 then //possible same lenngth but data diffs
                  Result := ZMemLComp(Blob1.GetBuffer, Blob2.GetBuffer, Blob1.Length);
              end
              else
                if Blob1.IsClob and Blob2.IsClob then
                  case ConSettings^.CPType of
                    cCP_UTF16:
                      begin
                        {$IFDEF MSWINDOWS}
                        ValuePtr1 := Blob1.GetPWideChar;
                        ValuePtr2 := Blob2.GetPWideChar;
                        SetLastError(0);
                        Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                          ValuePtr1, Blob1.Length, ValuePtr2, Blob1.Length) - 2{CSTR_EQUAL};
                        if GetLastError <> 0 then RaiseLastOSError;
                        {$ELSE}
                        WideCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
                        {$ENDIF}
                      end;
                    cCP_UTF8:
                      begin
                        ValuePtr1 := Blob1.GetPAnsiChar(zCP_UTF8);
                        ValuePtr2 := Blob2.GetPAnsiChar(zCP_UTF8);
                        Result := ZMemLComp(ValuePtr1, ValuePtr2, Blob1.Length);
                      end;
                    else
                      begin
                        {$IFDEF MSWINDOWS}
                        ValuePtr1 := Blob1.GetPAnsiChar(ConSettings^.CTRL_CP);
                        ValuePtr2 := Blob2.GetPAnsiChar(ConSettings^.CTRL_CP);
                          Result := CompareStringA(LOCALE_USER_DEFAULT, 0, ValuePtr1, Blob1.Length,
                            ValuePtr2, Blob2.Length) - 2;
                        {$ELSE}
                          Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString);
                        {$ENDIF}
                      end;
                  end
                else
                  Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString)
        end;
    end;
    if Result <> 0 then
    begin
      if not ColumnDirs[I] then
        Result := -Result;
      Break;
    end;
  end;
end;

{**
  Cleans the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.ClearBuffer(Buffer: PZRowBuffer);
var
  I: Integer;
  P: PPointer;
begin
  with Buffer^ do
  begin
    Index := -1;
    UpdateType := utUnmodified;
    BookmarkFlag := 0;
    for I := 0 to FColumnCount - 1 do
      case FColumnTypes[I] of
        stAsciiStream, stUnicodeStream, stBinaryStream:
          if (Columns[FColumnOffsets[I]] = 0) then
            SetBlobObject(Buffer, I + 1, nil);
        stBytes,stGUID,stString, stUnicodeString:
          if PNativeUInt(@Columns[FColumnOffsets[I] +1])^ > 0 then
          begin
            P := PPointer(@Columns[FColumnOffsets[I] +1]);
            System.FreeMem(P^);
          end;
      end;
    FillChar(Columns, FColumnsSize, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
    for I := 0 to FColumnCount - 1 do Columns[FColumnOffsets[I]] := 1;
  end;
end;

{**
  Allocates a new row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.Alloc: PZRowBuffer;
begin
  Result := AllocBuffer(FBuffer);
end;

{**
  Disposes an associated row buffer.
}
procedure TZRowAccessor.Dispose;
begin
  DisposeBuffer(FBuffer);
  FBuffer := nil;
end;

{**
  Initializes the associated row buffer.
}
procedure TZRowAccessor.Init;
begin
  InitBuffer(FBuffer);
end;

{**
  Copies the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.CopyTo(DestBuffer: PZRowBuffer);
begin
  CopyBuffer(FBuffer, DestBuffer);
end;

{**
  Copies the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.CopyFrom(SrcBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, FBuffer);
end;

{**
  Moves the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.MoveTo(DestBuffer: PZRowBuffer);
begin
  MoveBuffer(FBuffer, DestBuffer);
end;

{**
  Moves the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.MoveFrom(SrcBuffer: PZRowBuffer);
begin
  MoveBuffer(SrcBuffer, FBuffer);
end;

{**
  Clones the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.CloneTo(DestBuffer: PZRowBuffer);
begin
  CloneBuffer(FBuffer, DestBuffer);
end;

{**
  Clones the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.CloneFrom(SrcBuffer: PZRowBuffer);
begin
  CloneBuffer(SrcBuffer, FBuffer);
end;

{**
  Cleans the associated row buffer.
}
procedure TZRowAccessor.Clear;
begin
  ClearBuffer(FBuffer);
end;

{**
  Gets the case sensitive flag of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the case sensitive flag of the column data buffer.
}
function TZRowAccessor.GetColumnCase(Const ColumnIndex: Integer): Boolean;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnCases[ColumnIndex-1];
end;

{**
  Gets a pointer to the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a pointer to the column data buffer.
}
function TZRowAccessor.GetColumnData(Const ColumnIndex: Integer;
  var IsNull: Boolean): Pointer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
end;

{**
  Gets a size of the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a size of the column data buffer.
}
function TZRowAccessor.GetColumnDataSize(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FColumnLengths[ColumnIndex - 1];
end;

{**
  Gets then length of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the length of the column data buffer.
}
function TZRowAccessor.GetColumnLength(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnLengths[ColumnIndex-1];
end;

{**
  Gets then name of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the name of the column data buffer.
}
function TZRowAccessor.GetColumnName(Const ColumnIndex: Integer): string;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnNames[ColumnIndex-1];
end;

{**
  Gets then offset of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return then offset of the column data buffer.
}
function TZRowAccessor.GetColumnOffSet(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnOffSets[ColumnIndex-1];
end;

{**
  Gets then SQLType of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the SQLType of the column data buffer.
}
function TZRowAccessor.GetColumnType(Const ColumnIndex: Integer): TZSQLType;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnTypes[ColumnIndex-1];
end;

function TZRowAccessor.GetColumnDefaultExpression(Const ColumnIndex: Integer): string;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnDefaultExpressions[ColumnIndex-1];
end;

procedure TZRowAccessor.SetColumnDefaultExpression(Const ColumnIndex: Integer; const Value: string);
begin
  FColumnDefaultExpressions[ColumnIndex-1] := Value;
end;

procedure TZRowAccessor.SetColumnCodePage(Const ColumnIndex: Integer; const Value: Word);
begin
  FColumnCodePages[ColumnIndex-1] := Value;
end;

//
//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZRowAccessor.IsNull(Const ColumnIndex: Integer): Boolean;
var
  TempBlob: IZBlob;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
  if not Result and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream,
    stBinaryStream, stUnicodeStream]) then
  begin
    TempBlob := GetBlobObject(FBuffer, ColumnIndex);
    Result := (TempBlob = nil) or TempBlob.IsEmpty;
  end;
end;

function TZRowAccessor.GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec;
var
  Blob: IZBlob;
  GUID: TGUID;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      if GetBoolean(ColumnIndex, IsNull) then
        FRawTemp := 'True'
      else
        FRawTemp := 'False';
    stByte: FRawTemp := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: FRawTemp := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: FRawTemp := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: FRawTemp := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: FRawTemp := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: FRawTemp := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: FRawTemp := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: FRawTemp := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: FRawTemp := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID:
      begin
        System.Move(GetBytes(ColumnIndex, IsNull)[0], GUID, 16);
        {$IFDEF UNICODE}
        FRawTemp := ConSettings^.ConvFuncs.ZStringToRaw(GUIDToString(GUID), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
        {$ELSE}
        FRawTemp := GUIDToString(GUID);
        {$ENDIF}
      end;
    stDate: FRawTemp := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: FRawTemp := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: FRawTemp := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlobObject(FBuffer, ColumnIndex);
        if (Blob <> nil) and not Blob.IsEmpty then
          if Blob.IsClob then
          begin
            if ConSettings^.AutoEncode then
              Result.P := Blob.GetPAnsiChar(ConSettings^.CTRL_CP)
            else
              Result.P := Blob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
            Result.Len := Blob.Length;
          end
          else
          begin
            Result.P := Blob.GetBuffer;
            Result.Len := Blob.Length;
          end;
        Exit;
      end;
    stBinaryStream:
      begin
        Blob := GetBlobObject(FBuffer, ColumnIndex);
        if (Blob <> nil) and not Blob.IsEmpty then
        begin
          Result.P := Blob.GetBuffer;
          Result.Len := Blob.Length;
        end;
        Exit;
      end;
    else
      FRawTemp := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(GetString(ColumnIndex, IsNull));
  end;
  Result.Len := Length(FRawTemp);
  Result.P := PAnsiChar(FRawTemp);
end;

function TZRowAccessor.GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String;
var
  TempBlob: IZBlob;
  GUID: TGUID;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: Result := {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: Result := {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: Result := {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: Result := {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: Result := FloatToSQLStr(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: Result := FloatToSQLStr(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: Result := FloatToSQLStr(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
            {$IFDEF UNICODE}
            Result := TempBlob.GetUnicodeString
            {$ELSE}
            Result := TempBlob.GetPAnsiChar(ConSettings^.CTRL_CP)
            {$ENDIF}
          else
            Result := {$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(TempBlob.GetString);
      end;
    stBytes: Result := {$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(BytesToStr(GetBytes(ColumnIndex, IsNull)));
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        Result := GUIDToString(GUID);
      end;
    stDate: Result := FormatDateTime('yyyy-mm-dd',
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stTime: Result := FormatDateTime('hh:mm:ss',
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stTimestamp: Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stAsciiStream, stBinaryStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          Result := {$IFDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(TempBlob.GetString);
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Ansi</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString;
var
  TempBlob: IZBlob;
  GUID: TGUID;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        {$IFDEF UNICODE}
        Result := AnsiString(GUIDToString(GUID));
        {$ELSE}
        Result := GUIDToString(GUID);
        {$ENDIF}
      end;
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
            Result := TempBlob.GetAnsiString
          else
            Result := TempBlob.GetString;
      end;
    stBinaryStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          Result := TempBlob.GetString;
      end;
    else
      Result := ConSettings^.ConvFuncs.ZStringToAnsi(GetString(ColumnIndex, IsNull), ConSettings^.CTRL_CP);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String;
var
  TempBlob: IZBlob;
  GUID: TGUID;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        Result := ConSettings^.ConvFuncs.ZStringToUTF8(GUIDToString(GUID), ConSettings^.CTRL_CP);
      end;
    //stString, stUnicodeString: do not handle here!
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
            Result := TempBlob.GetUTF8String
          else
            Result := TempBlob.GetString;
      end;
    stBinaryStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          Result := TempBlob.GetString;
      end;
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    else
      Result := ConSettings^.ConvFuncs.ZStringToUTF8(GetString(ColumnIndex, IsNull), ConSettings^.CTRL_CP);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString;
var
  TempBlob: IZBlob;
  GUID: TGUID;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        Result := ConSettings^.ConvFuncs.ZStringToRaw(GUIDToString(GUID), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
      end;
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          Result := TempBlob.GetString;
      end;
    else
      Result := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(GetString(ColumnIndex, IsNull));
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZWideRec/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetWideRec(Const ColumnIndex: Integer;
  var IsNull: Boolean): TZWideRec;
var
  TempBlob: IZBlob;
  GUID: TGUID;
  Bts: TByteDynArray;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stByte: FUniTemp := IntToUnicode(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: FUniTemp := IntToUnicode(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: FUniTemp := IntToUnicode(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: FUniTemp := IntToUnicode(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: FUniTemp := IntToUnicode(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: FUniTemp := FloatToSqlUnicode(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: FUniTemp := FloatToSqlUnicode(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: FUniTemp := FloatToSqlUnicode(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stUnicodeString, stString: do not handle here!
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
          begin
            Result.P := TempBlob.GetPWideChar;
            Result.Len := TempBlob.Length div 2;
            Exit;
          end
          else
            FUniTemp := NotEmptyASCII7ToUnicodeString(TempBlob.GetString);
      end;
    stBytes, stBinaryStream:
      begin
        Bts := GetBytes(ColumnIndex, IsNull);
        FUniTemp := NotEmptyASCII7ToUnicodeString(PAnsiChar(Bts), Length(Bts));
      end;
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        FUniTemp := {$IFNDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(GUIDToString(GUID));
      end;
    stDate: FUniTemp := DateTimeToUnicodeSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: FUniTemp := DateTimeToUnicodeSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: FUniTemp := DateTimeToUnicodeSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    else
      FUniTemp := {$IFNDEF UNICODE}ZWideString{$ENDIF}(GetString(ColumnIndex, IsNull));
  end;
  Result.P := PWideChar(FUniTemp);
  Result.Len := Length(FuniTemp);
end;
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeString(Const ColumnIndex: Integer;
  var IsNull: Boolean): ZWideString;
var
  TempBlob: IZBlob;
  GUID: TGUID;
  Bts: TByteDynArray;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stByte: Result := IntToUnicode(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stShort: Result := IntToUnicode(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stSmall: Result := IntToUnicode(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stInteger: Result := IntToUnicode(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stLong: Result := IntToUnicode(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stFloat: Result := FloatToSqlUnicode(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stDouble: Result := FloatToSqlUnicode(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    stBigDecimal: Result := FloatToSqlUnicode(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
    //stUnicodeString, stString: do not handle here!
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
            Result := TempBlob.GetUnicodeString
          else
            Result := NotEmptyASCII7ToUnicodeString(TempBlob.GetString);
      end;
    stBytes, stBinaryStream:
      begin
        Bts := GetBytes(ColumnIndex, IsNull);
        Result := NotEmptyASCII7ToUnicodeString(PAnsiChar(Bts), Length(Bts));
      end;
    stGUID:
      begin
        System.Move(Pointer(GetBytes(ColumnIndex, IsNull))^, GUID, 16);
        Result := {$IFNDEF UNICODE}NotEmptyASCII7ToUnicodeString{$ENDIF}(GUIDToString(GUID));
      end;
    stDate: Result := DateTimeToUnicodeSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToUnicodeSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToUnicodeSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    else
      Result := {$IFNDEF UNICODE}ZWideString{$ENDIF}(GetString(ColumnIndex, IsNull));
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
function TZRowAccessor.GetBoolean(Const ColumnIndex: Integer; var IsNull: Boolean): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean: Result := PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ <> 0;
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := StrToBoolEx(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, False)
        else
          Result := StrToBoolEx(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, False);
      stUnicodeStream, stAsciiStream:
        Result := StrToBoolEx(GetBlob(ColumnIndex, IsNull).GetString);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetByte(Const ColumnIndex: Integer; var IsNull: Boolean): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0);
      stUnicodeStream, stAsciiStream: Result := RawToIntDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetShort(Const ColumnIndex: Integer; var IsNull: Boolean): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0);
      stUnicodeStream, stAsciiStream: Result := RawToIntDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetSmall(Const ColumnIndex: Integer; var IsNull: Boolean): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0);
      stAsciiStream, stUnicodeStream: Result := RawToIntDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetInt(Const ColumnIndex: Integer; var IsNull: Boolean): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0);
      stAsciiStream, stUnicodeStream: Result := RawToIntDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetLong(Const ColumnIndex: Integer; var IsNull: Boolean): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetBigDecimal(ColumnIndex, IsNull));
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := RawToInt64Def(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToInt64Def(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0);
      stAsciiStream, stUnicodeStream: Result := RawToInt64Def(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetFloat(Const ColumnIndex: Integer; var IsNull: Boolean): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat:
        Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stDouble: Result := GetDouble(ColumnIndex, IsNull);
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
      stAsciiStream, stUnicodeStream: Result := SQLStrToFloatDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetDouble(Const ColumnIndex: Integer; var IsNull: Boolean): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := GetFloat(ColumnIndex, IsNull);
      stDouble:
        Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0,
           PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0,
           PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
      stAsciiStream, stUnicodeStream: Result := SQLStrToFloatDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
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
function TZRowAccessor.GetBigDecimal(Const ColumnIndex: Integer; var IsNull: Boolean): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else
          Result := 0;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := GetFloat(ColumnIndex, IsNull);
      stDouble: Result := GetDouble(ColumnIndex, IsNull);
      stBigDecimal:
        Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc, 0,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc, 0,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
      stAsciiStream, stUnicodeStream: Result := SQLStrToFloatDef(GetBlob(ColumnIndex, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
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
function TZRowAccessor.GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBytes,stGUID:
        Result := InternalGetBytes(FBuffer, ColumnIndex);
      stBinaryStream:
        Result := GetBlob(ColumnIndex, IsNull).GetBytes;
      else
        Result := StrToBytes(GetRawByteString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetDate(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
var
  Failed: Boolean;
  AnsiBuffer: PAnsiChar;
  WideBuffer: PWideChar;
  TempBlob: IZBlob;
  BufLen: Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Int(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        begin
          AnsiBuffer := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          BufLen := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZSysUtils.RawSQLDateToDateTime(AnsiBuffer, BufLen,
           ConSettings^.ReadFormatSettings, Failed);
          if Failed then
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ZSysUtils.RawSQLTimeStampToDateTime(
              AnsiBuffer, BufLen, ConSettings^.ReadFormatSettings, Failed));
        end
        else
        begin
          WideBuffer := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          BufLen := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZSysUtils.UnicodeSQLDateToDateTime(WideBuffer,
            BufLen, ConSettings^.ReadFormatSettings, Failed);
          if Failed then
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ZSysUtils.UnicodeSQLTimeStampToDateTime(
              WideBuffer, BufLen, ConSettings^.ReadFormatSettings, Failed));
        end;
      stAsciiStream, stUnicodeStream:
        begin
          TempBlob := GetBlob(ColumnIndex, IsNull);
          if TempBlob.IsClob then
          begin
            AnsiBuffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
            Result := ZSysUtils.RawSQLDateToDateTime(AnsiBuffer, TempBlob.Length,
              ConSettings^.ReadFormatSettings, Failed);
            if Failed then
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                ZSysUtils.RawSQLTimeStampToDateTime(AnsiBuffer, TempBlob.Length,
                ConSettings^.ReadFormatSettings, Failed));
          end;
        end;
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetTime(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
var
  Failed: Boolean;
  AnsiBuffer: PAnsiChar;
  WideBuffer: PWideChar;
  TempBlob: IZBlob;
  BufLen: Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Frac(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        begin
          AnsiBuffer := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          BufLen := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZSysUtils.RawSQLTimeToDateTime(AnsiBuffer, BufLen,
            ConSettings^.ReadFormatSettings, Failed);
          if Failed then
            Result := Frac(ZSysUtils.RawSQLTimeStampToDateTime(AnsiBuffer,
              BufLen, ConSettings^.ReadFormatSettings, Failed));
        end
        else
        begin
          WideBuffer := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          BufLen := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZSysUtils.UnicodeSQLTimeToDateTime(WideBuffer, BufLen,
            ConSettings^.ReadFormatSettings, Failed);
          if Failed then
            Result := Frac(ZSysUtils.UnicodeSQLTimeStampToDateTime(WideBuffer,
              BufLen, ConSettings^.ReadFormatSettings, Failed));
        end;
      stAsciiStream, stUnicodeStream:
        begin
          TempBlob := GetBlob(ColumnIndex, IsNull);
          if TempBlob.IsClob then
          begin
            AnsiBuffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
            Result := ZSysUtils.RawSQLTimeToDateTime(AnsiBuffer, TempBlob.Length,
              ConSettings^.ReadFormatSettings, Failed);
            if Failed then
              Result := Frac(ZSysUtils.RawSQLTimeStampToDateTime(AnsiBuffer,
                TempBlob.Length, ConSettings^.ReadFormatSettings, Failed));
          end;
        end;
    end;
    IsNull := False;
  end
  else
    IsNull := True;
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
function TZRowAccessor.GetTimestamp(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
var
  Failed: Boolean;
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          Result := ZSysUtils.RawSQLTimeStampToDateTime(
            PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^,
                ConSettings^.ReadFormatSettings, Failed)
        else
          Result := ZSysUtils.UnicodeSQLTimeStampToDateTime(
            PPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^,
                ConSettings^.ReadFormatSettings, Failed);
      stAsciiStream, stUnicodeStream:
        begin
          TempBlob := GetBlob(ColumnIndex, IsNull);
          if TempBlob.IsClob then
            Result := ZSysUtils.RawSQLTimeStampToDateTime(TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP),
              TempBlob.Length, ConSettings^.ReadFormatSettings, Failed);
        end;
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a stream of ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <char>LONGVARCHAR</char> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetAsciiStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else
    Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  as a stream of Unicode characters.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large<code>LONGVARCHAR</code>values.  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as specified in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream in Java UTF-8 byte format; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    if TempBlob.IsClob then
      Result := TempBlob.GetUnicodeStream
    else
      Result := TempBlob.GetStream
  else
    Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a binary stream of
  uninterpreted bytes. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code> values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetBinaryStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else
    Result := nil;
  IsNull := Result = nil;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZRowAccessor.GetBlob(Const ColumnIndex: Integer; var IsNull: Boolean): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  Result := GetBlobObject(FBuffer, ColumnIndex);
  IsNull := Result = nil;
  if Result = nil then
  begin
    if (FColumnTypes[ColumnIndex - 1] = stBinaryStream) then
      Result := TZAbstractBlob.CreateWithStream(nil)
    else
      Result := TZAbstractClob.CreateWithData(nil, 0, ConSettings^.ClientCodePage^.CP, ConSettings);
    SetBlobObject(FBuffer, ColumnIndex, Result);
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>ResultSet</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>ResultSet</code> object representing the SQL
    <code>ResultSet</code> value in the specified column
}
function TZRowAccessor.GetDataSet(Const ColumnIndex: Integer; var IsNull: Boolean): IZDataSet;
var
  Ptr: PPointer;
  NullPtr: {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF};
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] = stDataSet) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  Ptr := PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  {$IFNDEF FPC}
  if NullPtr^ = {$IFDEF WIN64}false{$ELSE}0{$ENDIF} then
  {$ELSE}
  if NullPtr^ = 0 then
  {$ENDIF}
    Result := IZDataSet(Ptr^)
  else
    Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> value.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
}
function TZRowAccessor.GetValue(Const ColumnIndex: Integer): TZVariant;
var
  ValuePtr: Pointer;
  IsNull: Boolean;
begin
  IsNull := False;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    ValuePtr := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
    case FColumnTypes[ColumnIndex - 1] of
      stByte:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PByte(ValuePtr)^;
        end;
      stShort:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PShortInt(ValuePtr)^;
        end;
      stSmall:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PSmallInt(ValuePtr)^;
        end;
      stInteger:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PInteger(ValuePtr)^;
        end;
      stLong:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PInt64(ValuePtr)^;
        end;
      stFloat:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PSingle(ValuePtr)^;
        end;
      stDouble:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PDouble(ValuePtr)^;
        end;
      stBigDecimal:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PExtended(ValuePtr)^;
        end;
      stBoolean:
        begin
          Result.VType := vtBoolean;
          Result.VBoolean := PWordBool(ValuePtr)^;
        end;
      stDate, stTime, stTimestamp:
        begin
          Result.VType := vtDateTime;
          Result.VDateTime := PDateTime(ValuePtr)^;
        end;
      stString:
        begin
          Result.VType := vtString;
          Result.VString := GetString(ColumnIndex, IsNull);
        end;
      stUnicodeString:
        begin
          Result.VType := vtUnicodeString;
          Result.VUnicodeString := GetUnicodeString(ColumnIndex, IsNull);
        end;
      stBytes, stGUID, stBinaryStream:
        begin
          Result.VType := vtBytes;
          Result.VBytes := GetBytes(ColumnIndex, IsNull);
        end;
      stAsciiStream:
        begin
          Result.VType := vtString;
          Result.VString := GetString(ColumnIndex, IsNull);
        end;
      stUnicodeStream:
        begin
          Result.VType := vtUnicodeString;
          Result.VUnicodeString := GetUnicodeString(ColumnIndex, IsNull);
        end;
      stDataSet:
        begin
          Result.VType := vtInterface;
          Result.VInterface := GetDataSet(ColumnIndex, IsNull);
        end;
      else
        Result.VType := vtNull;
    end;
  end
  else
    Result.VType := vtNull;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a not nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNotNull(Const ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1)
    and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
end;

{**
  Gives a nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNull(Const ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0) then
    case FColumnTypes[ColumnIndex - 1] of
      stAsciiStream, stBinaryStream, stUnicodeStream:
        SetBlobObject(FBuffer, ColumnIndex, nil);
      stBytes,stGUID, stString, stUnicodeString:
        if PNativeUInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ > 0 then
        begin
          System.FreeMem(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
          PNativeUInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := 0;
        end;
    end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1;
end;

{**
  Sets the designated column with a <code>boolean</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBoolean(Const ColumnIndex: Integer; const Value: Boolean);
var
  TempInt: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  if Value then
     TempInt := 1
  else
     TempInt := 0;

  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := TempInt;
    stString, stUnicodeString:
       if Value then
          SetString(ColumnIndex, 'True')
       else
          SetString(ColumnIndex, 'False');
    else
      FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1; //set null
  end;
end;

{**
  Sets the designated column with a <code>byte</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetByte(Const ColumnIndex: Integer;
  const Value: Byte);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>byte</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetShort(Const ColumnIndex: Integer;
  const Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>short</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetSmall(Const ColumnIndex: Integer; const Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with an <code>int</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetInt(Const ColumnIndex: Integer; const Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>long</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetLong(Const ColumnIndex: Integer; const Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>float</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetFloat(Const ColumnIndex: Integer; const Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, FloatToSQLRaw(Value))
      else
        SetUnicodeString(ColumnIndex, FloatToSQLUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>double</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDouble(Const ColumnIndex: Integer; const Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, FloatToSQLRaw(Value))
      else
        SetUnicodeString(ColumnIndex, FloatToSQLUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBigDecimal(Const ColumnIndex: Integer; const Value: Extended);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
    stString, stUnicodeString:
      if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
        SetRawByteString(ColumnIndex, FloatToSQLRaw(Value))
      else
        SetUnicodeString(ColumnIndex, FloatToSQLUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetString(Const ColumnIndex: Integer; const Value: String);
var
  IsNull: Boolean;
  Bts: TByteDynArray;
  GUID: TGUID;
  TempBlob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    //stString, stUnicodeString: do not handle here!
    stBytes: SetBytes(ColumnIndex, StrToBytes(Value));
    stGUID:
      if (Value = '') or (Length(Value) <> 38) then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID(Value);
        SetLength(Bts, 16);
        System.Move(Pointer(@GUID)^, Pointer(Bts)^, 16);
        SetBytes(ColumnIndex, Bts);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF UNICODE}UnicodeSQLDateToDateTime{$ELSE}RawSQLDateToDateTime{$ENDIF}(
            PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            {$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
              PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        {$IFDEF UNICODE}UnicodeSQLTimeToDateTime{$ELSE}RawSQLTimeToDateTime{$ENDIF}(
          PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            Frac({$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
              PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
            PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed);
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlob(ColumnIndex, IsNull);
        if TempBlob.IsClob then
          {$IFDEF UNICODE}
          TempBlob.SetUnicodeString(Value)
          {$ELSE}
          TempBlob.SetUnicodeString(ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings.CTRL_CP, ConSettings^.ClientCodePage^.CP))
          {$ENDIF}
        else
          GetBlob(ColumnIndex, IsNull).SetBytes(StrToBytes(Value));
      end;
    stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetBytes(StrToBytes(Value));
  end;
end;

{**
  Sets the designated column with a <code>PAnsiChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
  @param Len the length of the String
}
procedure TZRowAccessor.SetAnsiRec(Const ColumnIndex: Integer; const Value: TZAnsiRec);
var
  IsNull: Boolean;
  GUID: TGUID;
  Bts: TByteDynArray;
  Blob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := StrToBoolEx(Value.P, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value.P, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value.P, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value.P, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value.P, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToInt64Def(Value.P, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    //stString, stUnicodeString: do not handle here!
    stBytes:
      begin
        SetLength(Bts, Value.Len);
        System.Move(Value.P^, Pointer(Bts)^, Value.Len);
        SetBytes(ColumnIndex, Bts);
      end;
    stGUID:
      if Value.P = nil then
        SetNull(ColumnIndex)
      else
      begin
        {$IFDEF UNICODE}
        GUID := StringToGUID(NotEmptyASCII7ToString(Value.P, Value.Len));
        {$ELSE}
        GUID := StringToGUID(Value.P);
        {$ENDIF}
        SetLength(Bts, 16);
        System.Move(Pointer(@GUID)^, Pointer(Bts)^, 16);
        SetBytes(ColumnIndex, Bts);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            RawSQLDateToDateTime (Value.P, Value.Len,
              ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            RawSQLTimeStampToDateTime(Value.P, Value.Len,
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        RawSQLTimeToDateTime(Value.P, Value.Len,
          ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            Frac(RawSQLTimeStampToDateTime(Value.P, Value.Len,
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          RawSQLTimeStampToDateTime(Value.P, Value.Len,
            ConSettings^.DisplayFormatSettings, Failed);
    stUnicodeStream, stAsciiStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull);
        if Blob.IsClob then
          Blob.SetPAnsiChar(Value.P, ConSettings^.ClientCodePage^.CP, Value.Len)
        else
          Blob.SetBuffer(Value.P, Value.Len);
      end;
    stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetBuffer(Value.P, Value.Len);
  end;
end;

{**
  Sets the designated column with a <code>PAnsiChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
}
procedure TZRowAccessor.SetPAnsiChar(Const ColumnIndex: Integer; const Value: PAnsiChar);
var AnsiRec: TZAnsiRec;
begin
  AnsiRec.P := Value;
  AnsiRec.Len := ZFastCode.StrLen(Value);
  SetAnsiRec(ColumnIndex, AnsiRec);
end;

{**
  Sets the designated column with a <code>TZWideRec</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
}
procedure TZRowAccessor.SetWideRec(Const ColumnIndex: Integer; const Value: TZWideRec);
var
  IsNull: Boolean;
  GUID: TGUID;
  Bts: TByteDynArray;
  Blob: IZBlob;
  Failed: Boolean;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := StrToBoolEx(Value.P, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value.P, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value.P, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value.P, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value.P, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToInt64Def(Value.P, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value.P, 0, Value.Len);
    //stUnicodeString, stString: do not handle here
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull);
        if Blob.IsClob then
          Blob.SetPWideChar(Value.P, Value.Len)
        else
          Blob.SetBuffer(Value.P, Value.Len*2);
      end;
    stBytes:
      SetBytes(ColumnIndex, StrToBytes(ZWideString(Value.P)));
    stGUID:
      if Value.P = nil  then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID({$IFDEF UNICODE}Value.P{$ELSE}NotEmptyUnicodeStringToASCII7(Value.P, Value.Len){$ENDIF});
        SetLength(Bts, 16);
        System.Move(Pointer(@GUID)^, Pointer(Bts)^, 16);
        SetBytes(ColumnIndex, Bts);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          UnicodeSQLDateToDateTime(Value.P, Value.Len, ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            UnicodeSQLTimeStampToDateTime(Value.P, Value.Len,
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        UnicodeSQLTimeToDateTime(Value.P, Value.Len, ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            Frac(UnicodeSQLTimeStampToDateTime(Value.P, Value.Len, ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        UnicodeSQLTimeStampToDateTime(Value.P, Value.Len, ConSettings^.DisplayFormatSettings, Failed);
    else
      SetString(ColumnIndex, ConSettings^.ConvFuncs.ZUnicodeToString(Value.P, ConSettings^.CTRL_CP));
  end;
end;

{**
  Sets the designated column with a <code>AnsiString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString);
begin
  if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
    SetRawByteString(ColumnIndex, ConSettings^.ConvFuncs.ZAnsiToRaw(Value, ConSettings^.ClientCodePage^.CP))
  else
    SetUnicodeString(ColumnIndex, ZWideString(Value));
end;

{**
  Sets the designated column with a <code>UTF8String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String);
begin
  if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
    SetRawByteString(ColumnIndex, ConSettings^.ConvFuncs.ZUTF8ToRaw(Value, ConSettings^.ClientCodePage^.CP))
  else
    SetUnicodeString(ColumnIndex, {$IFDEF WITH_RAWBYTESTRING}ZWideString{$ELSE}UTF8Decode{$ENDIF}(Value))
end;

{**
  Sets the designated column with a <code>RawByteString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString);
var
  IsNull: Boolean;
  GUID: TGUID;
  Bts: TByteDynArray;
  Failed: Boolean;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToIntDef(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := RawToInt64Def(Value, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    //stString, stUnicodeString: do not handle here!
    stBytes: SetBytes(ColumnIndex, StrToBytes(Value));
    stGUID:
      if Value = '' then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID({$IFDEF UNICODE}NotEmptyASCII7ToString{$ENDIF}(Value));
        SetLength(Bts, 16);
        System.Move(Pointer(@GUID)^, Pointer(Bts)^, 16);
        SetBytes(ColumnIndex, Bts);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            RawSQLDateToDateTime (PAnsiChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            RawSQLTimeStampToDateTime(PAnsiChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        RawSQLTimeToDateTime(PAnsiChar(Value), Length(Value),
          ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            Frac(RawSQLTimeStampToDateTime(PAnsiChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          RawSQLTimeStampToDateTime(PAnsiChar(Value), Length(Value),
            ConSettings^.DisplayFormatSettings, Failed);
    stUnicodeStream, stAsciiStream, stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetString(Value);
  end;
end;

{**
  Sets the designated column with a <code>WideString/UnicodeString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString);
var
  IsNull: Boolean;
  GUID: TGUID;
  Bts: TByteDynArray;
  Blob: IZBlob;
  Failed: Boolean;
begin
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToIntDef(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := UnicodeToInt64Def(Value, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := SQLStrToFloatDef(Value, 0);
    //stUnicodeString, stString: do not handle here
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull);
        if Blob.IsClob then
          Blob.SetUnicodeString(Value)
        else
          Blob.SetString(RawByteString(Value));
      end;
    stBytes:
      SetBytes(ColumnIndex, StrToBytes(Value));
    stGUID:
      if Value = '' then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID({$IFNDEF UNICODE}NotEmptyUnicodeStringToASCII7{$ENDIF}(Value));
        SetLength(Bts, 16);
        System.Move(Pointer(@GUID)^, Pointer(Bts)^, 16);
        SetBytes(ColumnIndex, Bts);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          UnicodeSQLDateToDateTime(PWideChar(Value), Length(Value),
            ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            UnicodeSQLTimeStampToDateTime(PWideChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        UnicodeSQLTimeToDateTime(PWideChar(Value), Length(Value),
          ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            Frac(UnicodeSQLTimeStampToDateTime(PWideChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
        UnicodeSQLTimeStampToDateTime(PWideChar(Value), Length(Value),
          ConSettings^.DisplayFormatSettings, Failed);
    else
      SetString(ColumnIndex, ConSettings^.ConvFuncs.ZUnicodeToString(Value, ConSettings^.CTRL_CP));
  end;
end;

{**
  Sets the designated column with a <code>byte</code> array value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBytes(Const ColumnIndex: Integer; const Value: TByteDynArray);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if Value <> nil then
  begin
    FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
    case FColumnTypes[ColumnIndex - 1] of
      stBytes,stGUID: InternalSetBytes(FBuffer, ColumnIndex, Value);
      stBinaryStream: GetBlob(ColumnIndex, IsNull).SetBytes(Value);
      else
        SetString(ColumnIndex, String(BytesToStr(Value)));
    end;
  end
  else
    SetNull(ColumnIndex);
end;

{**
  Sets the designated column with a <code>java.sql.Date</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDate(Const ColumnIndex: Integer; const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Value);
    stString, stUnicodeString: SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Time</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTime(Const ColumnIndex: Integer; const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stTime:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          Frac(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Frac(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTimestamp(Const ColumnIndex: Integer; const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate: SetDate(ColumnIndex, Value);
    stTime: SetTime(ColumnIndex, Value);
    stTimestamp:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with an ascii stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetAsciiStream(Const ColumnIndex: Integer; const Value: TStream);
var
  IsNull: Boolean;
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  IsNull := False;
  Blob := GetBlob(ColumnIndex, IsNull);
  if Blob.IsClob then
    if ConSettings^.AutoEncode then
      Blob.SetStream(Value)
    else
      Blob.SetStream(Value, ConSettings^.ClientCodePage^.CP)
  else
    GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a binary stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZRowAccessor.SetBinaryStream(Const ColumnIndex: Integer; const Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  IsNull := False;
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a character stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeStream(Const ColumnIndex: Integer;
  const Value: TStream);
var
  IsNull: Boolean;
  Blob: IZBlob;
begin
  IsNull := False;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Blob := GetBlob(ColumnIndex, IsNull);
  if Blob.IsClob then
    Blob.SetStream(Value, zCP_UTF16)
  else
    Blob.SetStream(Value);
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a blob wrapper object to be set.
}
procedure TZRowAccessor.SetBlob(Const ColumnIndex: Integer; const Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  SetBlobObject(FBuffer, ColumnIndex, Value);
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a ResultSet wrapper object to be set.
}
procedure TZRowAccessor.SetDataSet(Const ColumnIndex: Integer; const Value: IZDataSet);
var
  Ptr: PPointer;
  NullPtr: {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF};
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] = stDataSet) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  Ptr := PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  {$IFNDEF FPC}
  if NullPtr^ = {$IFDEF WIN64}false{$ELSE}0{$ENDIF} then  //M.A. if NullPtr^ = 0 then
  {$ELSE}
  if NullPtr^ = 0 then
  {$ENDIF}
    IZDataSet(Ptr^) := nil
  else
    Ptr^ := nil;

  IZDataSet(Ptr^) := Value;

  if Value <> nil then
  {$IFNDEF FPC}
    NullPtr^ := {$IFDEF WIN64}false{$ELSE}0{$ENDIF}  //M.A. NullPtr^ := 0
  else
    NullPtr^ := {$IFDEF WIN64}true{$ELSE}1{$ENDIF};  //M.A. NullPtr^ := 1;
  {$ELSE}
    NullPtr^ := 0
  else
    NullPtr^ := 1;
  {$ENDIF}
end;
{**
  Sets the designated column with a <code>Variant</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetValue(Const ColumnIndex: Integer; const Value: TZVariant);
begin
  case Value.VType of
    vtNull: SetNull(ColumnIndex);
    vtBoolean: SetBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: SetLong(ColumnIndex, Value.VInteger);
    vtFloat: SetBigDecimal(ColumnIndex, Value.VFloat);
    vtBytes: SetBytes(ColumnIndex, Value.VBytes);
    vtString: SetString(ColumnIndex, Value.VString);
    vtAnsiString: SetAnsiString(ColumnIndex, Value.VAnsiString);
    vtUTF8String: SetUTF8String(ColumnIndex, Value.VUTF8String);
    vtRawByteString: SetRawByteString(ColumnIndex, Value.VRawByteString);
    vtUnicodeString: SetUnicodeString(ColumnIndex, Value.VUnicodeString);
    vtDateTime: SetTimestamp(ColumnIndex, Value.VDateTime);
  end;
end;

{ TZRawRowAccessor }

function TZRawRowAccessor.CompareString(ValuePtr1, ValuePtr2: Pointer): Integer;
begin
  if Assigned(PPAnsichar(ValuePtr1)^) and Assigned(PPAnsiChar(ValuePtr2)^) then
    {$IFDEF MSWINDOWS}
    Result := CompareStringA(LOCALE_USER_DEFAULT, 0,
      PAnsiChar(ValuePtr1^)+PAnsiInc, PLongWord(ValuePtr1^)^,
      PAnsiChar(ValuePtr2^)+PAnsiInc, PLongWord(ValuePtr2^)^) - 2{CSTR_EQUAL}
    {$ELSE}
      Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}
        AnsiStrComp(PPAnsiChar(ValuePtr1)^+PAnsiInc, PPAnsiChar(ValuePtr2)^+PAnsiInc)
    {$ENDIF}
  else
    if not Assigned(PPAnsichar(ValuePtr1)^) and not Assigned(PPAnsiChar(ValuePtr2)^) then
      Result := 0
    else
      Result := -1
end;

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRawRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    for I := 0 to FColumnCount - 1 do
      case FColumnTypes[I] of
        stAsciiStream, stUnicodeStream, stBinaryStream:
          if (Columns[FColumnOffsets[I]] = 0) then
          begin
            Columns[FColumnOffsets[I]] := 1;
            SetBlobObject(DestBuffer, I + 1, GetBlobObject(SrcBuffer, I + 1));
          end;
        stString, stUnicodeString:
          if Columns[FColumnOffsets[I]] = 0 then
            InternalSetPAnsiChar(DestBuffer, I +1,
              PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^+PAnsiInc,
              PLongWord(PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^)^, True);
        stBytes,stGUID: InternalSetBytes(DestBuffer, I +1, InternalGetBytes(SrcBuffer, I +1), True);
      end;
  end;
end;

{**
  Clones the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRawRowAccessor.CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
  Blob: IZBlob;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    for I := 0 to FColumnCount - 1 do
      case FColumnTypes[I] of
        stAsciiStream, stUnicodeStream, stBinaryStream:
          if (Columns[FColumnOffsets[I]] = 0) then
          begin
            Columns[FColumnOffsets[I]] := 1;
            Blob := GetBlobObject(SrcBuffer, I + 1);
            if Blob <> nil then
              Blob := Blob.Clone;
            SetBlobObject(DestBuffer, I + 1, Blob);
          end;
        stString, stUnicodeString:
          if (Columns[FColumnOffsets[I]] = 0) then
            InternalSetPAnsiChar(DestBuffer, I +1,
              PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^+PAnsiInc,
              PLongWord(PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^)^, True);
        stBytes,stGUID: InternalSetBytes(DestBuffer, I +1, InternalGetBytes(SrcBuffer, I +1), True);
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          Result.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          Result.Len := PCardinal(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
        end;
      else
        Result := inherited GetAnsiRec(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
  begin
    Result.Len := 0;
    Result.P := nil;
    IsNull := True;
  end;
end;

function TZRawRowAccessor.GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec;
var AnsiRec: TZAnsiRec;
begin
  AnsiRec := GetAnsiRec(ColumnIndex, IsNull);
  Result.Len := AnsiRec.Len;
  Result.P := AnsiRec.P;
  Result.CP := ConSettings^.ClientCodePage^.CP;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String;
{$IFDEF UNICODE}
var AnsiRec: TZAnsiRec;
{$ENDIF}
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        {$IFDEF UNICODE}
        begin
          AnsiRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          AnsiRec.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          Result := ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP);
        end;
        {$ELSE}
        if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) or not ConSettings^.AutoEncode then
          System.SetString(Result, PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        else
          Result := ConSettings^.ConvFuncs.ZRawToString(
            PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc,
            ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
        {$ENDIF}
      else
        Result := inherited GetString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Ansi</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString;
var AnsiRec: TZAnsiRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          if ZCompatibleCodePages(ZDefaultsystemCodePage, ConSettings^.ClientCodePage^.CP) then
            System.SetString(Result, PAnsiChar(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc),
              PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
          else
          begin
            AnsiRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
            AnsiRec.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
            FUniTemp := ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP);
            Result := AnsiString(FUniTemp);
          end;
        end;
      else
        Result := inherited GetAnsiString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String;
var AnsiRec: TZAnsiRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        if ZCompatibleCodePages(zCP_UTF8, ConSettings^.ClientCodePage^.CP) then
        {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
        begin
          SetLength(Result, PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
          System.Move(PAnsiChar(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc)^,
            PAnsiChar(Result)^, PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
        end
        {$ELSE}
          System.SetString(Result, PAnsiChar(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc),
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        {$ENDIF}
        else
        begin
          AnsiRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          AnsiRec.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          FUniTemp := ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP); //localize the vals to avoid buffer overrun for WideStrings
          Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(FUniTemp);
        end;
      else
        Result := inherited GetUTF8String(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
        ZSetString(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc,
          PCardinal(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^, Result);
        {$ELSE}
        System.SetString(Result, PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc,
          PCardinal(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
        {$ENDIF}
      else
        Result := Inherited GetRawByteString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetWideRec(Const ColumnIndex: Integer;
  var IsNull: Boolean): TZWideRec;
var AnsiRec: TZAnsiRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString, stString:
        begin
          AnsiRec.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          AnsiRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          FUniTemp := ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP);
          Result.P := PWideChar(FUniTemp);
          Result.Len := Length(FUniTemp);
        end
      else
        Result := inherited GetWideRec(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
  begin
    Result.P := nil;
    Result.Len := 0;
    IsNull := True;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetUnicodeString(Const ColumnIndex: Integer;
  var IsNull: Boolean): ZWideString;
var AnsiRec: TZAnsiRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString, stString:
        begin
          AnsiRec.P := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PAnsiInc;
          AnsiRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP);
        end;
      else
        Result := inherited GetUnicodeString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRawRowAccessor.SetString(Const ColumnIndex: Integer; const Value: String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString, stUnicodeString:
      begin
        InternalSetString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else
      Inherited SetString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>PAnsiChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
  @param Len the Length of the new column value
}
procedure TZRawRowAccessor.SetAnsiRec(Const ColumnIndex: Integer;
  const Value: TZAnsiRec);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value.P = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          InternalSetPAnsiChar(FBuffer, ColumnIndex, Value.P, Value.Len);
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        end;
      else inherited SetAnsiRec(ColumnIndex, Value)
    end;
end;

{**
  Sets the designated column with a <code>TZWideRec</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRawRowAccessor.SetWideRec(Const ColumnIndex: Integer; const Value: TZWideRec);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value.P = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          InternalSetString(FBuffer, ColumnIndex, ZWideRecToRaw(Value, ConSettings^.ClientCodePage^.CP));
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        end;
      else inherited SetWideRec(ColumnIndex, Value)
    end;
end;

{**
  Sets the designated column with a <code>RawByteString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRawRowAccessor.SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString, stUnicodeString:
      begin
        InternalSetString(FBuffer, ColumnIndex, Value);
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else inherited SetRawByteString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>WideString/UnicodeString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRawRowAccessor.SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stUnicodeString, stString:
      begin
        InternalSetString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else inherited SetUnicodeString(ColumnIndex, Value);
  end;
end;

{ TZUnicodeRowAccessor }

function TZUnicodeRowAccessor.CompareString(ValuePtr1, ValuePtr2: Pointer): Integer;
{$IFDEF MSWINDOWS}
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
    PWideChar(ValuePtr1^)+PWideInc, PCardinal(ValuePtr1^)^,
    PWideChar(ValuePtr2^)+PWideInc, PCardinal(ValuePtr2^)^) - 2{CSTR_EQUAL};
  if GetLastError <> 0 then
    RaiseLastOSError;
end;
{$ELSE}
var S1, S2: ZWideString;
begin
  System.SetString(S1, PWideChar(ValuePtr1^)+PWideInc, PCardinal(ValuePtr1^)^);
  System.SetString(S2, PWideChar(ValuePtr2^)+PWideInc, PCardinal(ValuePtr2^)^);
  Result := WideCompareStr(S1, S2);
end;
{$ENDIF}

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZUnicodeRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    for I := 0 to FColumnCount - 1 do
      case FColumnTypes[I] of
        stAsciiStream, stUnicodeStream, stBinaryStream:
          if (Columns[FColumnOffsets[I]] = 0) then
          begin
            Columns[FColumnOffsets[I]] := 1;
            SetBlobObject(DestBuffer, I + 1, GetBlobObject(SrcBuffer, I + 1));
          end;
      stString, stUnicodeString:
        if (Columns[FColumnOffsets[I]] = 0) then
          InternalSetPWideChar(DestBuffer, I +1,
            ZPPWideChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^+PWideInc,
            PCardinal(PPointer(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^)^,
            True);
      stBytes,stGUID: InternalSetBytes(DestBuffer, I +1, InternalGetBytes(SrcBuffer, I +1), True);
    end;
  end;
end;

{**
  Clones the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZUnicodeRowAccessor.CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
  Blob: IZBlob;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, FColumnsSize);
    for I := 0 to FColumnCount - 1 do
      case FColumnTypes[I] of
        stAsciiStream, stUnicodeStream, stBinaryStream:
          if (Columns[FColumnOffsets[I]] = 0) then
          begin
            Columns[FColumnOffsets[I]] := 1;
            Blob := GetBlobObject(SrcBuffer, I + 1);
            if Blob <> nil then
              Blob := Blob.Clone;
            SetBlobObject(DestBuffer, I + 1, Blob);
          end;
        stString, stUnicodeString:
          if (Columns[FColumnOffsets[I]] = 0) then
            InternalSetPWideChar(DestBuffer, I +1,
              ZPPWideChar(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^+PWideInc,
              PCardinal(PPointer(@SrcBuffer.Columns[FColumnOffsets[I] + 1])^)^,
              True);
        stBytes,stGUID: InternalSetBytes(DestBuffer, I +1, InternalGetBytes(SrcBuffer, I +1), True);
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
function TZUnicodeRowAccessor.GetAnsiRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZAnsiRec;
var ZWideRec: TZWideRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          ZWideRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          ZWideRec.P := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          FRawTemp := ZWideRecToRaw(ZWideRec, ConSettings^.ClientCodePage^.CP);
          Result.Len := Length(FRawTemp);
          Result.P := PAnsiChar(FRawTemp);
        end
      else
        Result := Inherited GetAnsiRec(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
  begin
    Result.P := nil;
    Result.Len := 0;
    IsNull := True;
  end;
end;

function TZUnicodeRowAccessor.GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec;
var WideRec: TZWideRec;
begin
  WideRec := GetWideRec(ColumnIndex, IsNull);
  Result.Len := WideRec.Len;
  Result.CP := zCP_UTF16;
  Result.P := WideRec.P;
end;
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String;
{$IFNDEF UNICODE}
var WideRec: TZWideRec;
{$ENDIF}
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc,
                          PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^)
        {$ELSE}
        begin
          WideRec.P := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          WideRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          if ConSettings^.AutoEncode then
            Result := ZWideRecToString(WideRec, ConSettings^.CTRL_CP)
          else
            Result := ZWideRecToString(WideRec, ConSettings^.ClientCodePage^.CP)
        end;
        {$ENDIF}
      else Result := Inherited GetString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Ansi</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString;
var US: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          System.SetString(US, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
          Result := AnsiString(US);
        end;
      else
        Result := inherited GetAnsiString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String;
var US: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          System.SetString(US, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc,
            PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
          Result := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(US);
        end;
      else
        Result := inherited GetUTF8String(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString;
var WideRec: TZWideRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          WideRec.P := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          WideRec.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
          Result := ZWideRecToRaw(WideRec, ConSettings^.ClientCodePage^.CP);
        end;
      else
        Result := Inherited GetRawByteString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetWideRec(Const ColumnIndex: Integer;
  var IsNull: Boolean): TZWideRec;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString, stString:
        begin
          Result.P := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc;
          Result.Len := PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^;
        end;
      else
        Result := inherited GetWideRec(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
  begin
    Result.P := nil;
    Result.Len := 0;
    IsNull := True;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean):
   ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString, stString:
        System.SetString(Result, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^+PWideInc,
          PCardinal(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^)^);
      else
        Result := inherited GetUnicodeString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZUnicodeRowAccessor.SetString(Const ColumnIndex: Integer;
  const Value: String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString, stUnicodeString:
      begin
        {$IFDEF UNICODE}
        InternalSetUnicodeString(FBuffer, ColumnIndex, Value);
        {$ELSE}
        InternalSetUnicodeString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZStringToUnicode(Value, ConSettings^.CTRL_CP));
        {$ENDIF}
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else inherited SetString(ColumnIndex, Value)
  end;
end;

{**
  Sets the designated column with a <code>TZAnsiRec</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
}
procedure TZUnicodeRowAccessor.SetAnsiRec(Const ColumnIndex: Integer;
  const Value: TZAnsiRec);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value.P = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          InternalSetUnicodeString(FBuffer, ColumnIndex, ZAnsiRecToUnicode(Value, ConSettings^.ClientCodePage^.CP));
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        end;
      else inherited SetAnsiRec(ColumnIndex, Value)
    end;
end;

{**
  Sets the designated column with a <code>TZWideRec</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
}
procedure TZUnicodeRowAccessor.SetWideRec(Const ColumnIndex: Integer;
  const Value: TZWideRec);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value.P = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex - 1] of
      stString, stUnicodeString:
        begin
          InternalSetPWideChar(FBuffer, ColumnIndex, Value.P, Value.Len);
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        end;
      else inherited SetWideRec(ColumnIndex, Value)
    end;
end;

{**
  Sets the designated column with a <code>RawByteString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZUnicodeRowAccessor.SetRawByteString(Const ColumnIndex: Integer;
  const Value: RawByteString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString, stUnicodeString:
      begin
        InternalSetUnicodeString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZRawToUnicode(Value, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else inherited SetRawByteString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>WideString/UnicodeString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZUnicodeRowAccessor.SetUnicodeString(Const ColumnIndex: Integer;
  const Value: ZWideString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stUnicodeString, stString:
      begin
        InternalSetUnicodeString(FBuffer, ColumnIndex, Value);
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
      end;
    else inherited SetUnicodeString(ColumnIndex, Value);
  end;
end;

end.


