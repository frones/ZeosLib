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
  {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZDbcIntfs, ZDbcResultSet,
  ZDbcResultSetMetadata, ZVariant, ZCompatibility;

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

    {store columnswhere mem-deallocation/copy needs an extra sequence of code}
    FHasBytes, FHasStrings, FHasArrays, FHasLobs, FHasDataSets: Boolean;
    FHighBytesCols, FHighStringCols, FHighArrayCols, FHighLobCols, FHighDataSetCols: Integer;
    FBytesCols, FStringCols, FArrayCols, FLobCols, FDataSetCols: array of Integer;
    FConSettings: PZConSettings;

    function GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
    function GetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer): IZBlob;
    procedure SetBlobObject(const Buffer: PZRowBuffer; ColumnIndex: Integer;
      const Value: IZBlob);
    function InternalGetBytes(const Buffer: PZRowBuffer; ColumnIndex: Integer): TBytes; overload; {$IFDEF WITHINLINE} inline; {$ENDIF}
    function InternalGetBytes(const Buffer: PZRowBuffer; ColumnIndex: Integer; var Len: Word): Pointer; overload; {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetBytes(const Buffer: PZRowBuffer; ColumnIndex: Integer;
      const Value: TBytes; const NewPointer: Boolean = False); overload; {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetBytes(const Buffer: PZRowBuffer; ColumnIndex: Integer;
      Buf: Pointer; Len: Word; const NewPointer: Boolean = False); overload; {$IFDEF WITHINLINE} inline; {$ENDIF}
   procedure InternalSetString(const Buffer: PZRowBuffer; ColumnIndex: Integer;
      const Value: RawByteString; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetUnicodeString(const Buffer: PZRowBuffer; ColumnIndex: Integer;
      const Value: ZWideString; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPAnsiChar(const Buffer: PZRowBuffer;
      ColumnIndex: Integer; const Value: PAnsiChar;
      Const Len: Cardinal; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPWideChar(const Buffer: PZRowBuffer;
      ColumnIndex: Integer; const Value: PWideChar;
      Const Len: Cardinal; const NewPointer: Boolean = False); {$IFDEF WITHINLINE} inline; {$ENDIF}
  protected
    procedure CheckColumnIndex(Const ColumnIndex: Integer);
    procedure CheckColumnConvertion(Const ColumnIndex: Integer; ResultType: TZSQLType);
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings);

    function AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
    procedure InitBuffer(Buffer: PZRowBuffer);
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer; const CloneLobs: Boolean = False); virtual; abstract;
    procedure MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure ClearBuffer(Buffer: PZRowBuffer; const WithFillChar: Boolean = True);
    procedure DisposeBuffer(Buffer: PZRowBuffer);

    function CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
      const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
    function GetCompareFunc(ColumnIndex: Integer; const CompareKind: TComparisonKind): TCompareFunc;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs;

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
    function GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PAnsiChar; virtual;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; virtual; abstract;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; virtual;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; virtual;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; virtual;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; virtual;
    function GetPWideChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PWideChar; virtual;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; virtual;
    function GetBoolean(Const ColumnIndex: Integer; var IsNull: Boolean): Boolean;
    function GetByte(Const ColumnIndex: Integer; var IsNull: Boolean): Byte;
    function GetShort(Const ColumnIndex: Integer; var IsNull: Boolean): ShortInt;
    function GetWord(Const ColumnIndex: Integer; var IsNull: Boolean): Word;
    function GetSmall(Const ColumnIndex: Integer; var IsNull: Boolean): SmallInt;
    function GetUInt(Const ColumnIndex: Integer; var IsNull: Boolean): Cardinal;
    function GetInt(Const ColumnIndex: Integer; var IsNull: Boolean): Integer;
    function GetULong(Const ColumnIndex: Integer; var IsNull: Boolean): UInt64;
    function GetLong(Const ColumnIndex: Integer; var IsNull: Boolean): Int64;
    function GetFloat(Const ColumnIndex: Integer; var IsNull: Boolean): Single;
    function GetDouble(Const ColumnIndex: Integer; var IsNull: Boolean): Double;
    function GetCurrency(Const ColumnIndex: Integer; var IsNull: Boolean): Currency;
    function GetBigDecimal(Const ColumnIndex: Integer; var IsNull: Boolean): Extended;
    function GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean): TBytes; overload;
    function GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: Word): Pointer; overload;
    function GetDate(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTime(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTimestamp(Const ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetAsciiStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetUnicodeStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBinaryStream(Const ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBlob(Const ColumnIndex: Integer; var IsNull: Boolean): IZBlob;
    function GetDataSet(Const ColumnIndex: Integer; var {%H-}IsNull: Boolean): IZDataSet;
    function GetValue(Const ColumnIndex: Integer): TZVariant;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetNotNull(Const ColumnIndex: Integer);
    procedure SetNull(Const ColumnIndex: Integer);
    procedure SetBoolean(Const ColumnIndex: Integer; const Value: Boolean);
    procedure SetByte(Const ColumnIndex: Integer; const Value: Byte);
    procedure SetShort(Const ColumnIndex: Integer; const Value: ShortInt);
    procedure SetWord(Const ColumnIndex: Integer; const Value: Word);
    procedure SetSmall(Const ColumnIndex: Integer; const Value: SmallInt);
    procedure SetUInt(Const ColumnIndex: Integer; const Value: Cardinal);
    procedure SetInt(Const ColumnIndex: Integer; const Value: Integer);
    procedure SetULong(Const ColumnIndex: Integer; const Value: UInt64);
    procedure SetLong(Const ColumnIndex: Integer; const Value: Int64);
    procedure SetFloat(Const ColumnIndex: Integer; const Value: Single);
    procedure SetDouble(Const ColumnIndex: Integer; const Value: Double);
    procedure SetCurrency(Const ColumnIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(Const ColumnIndex: Integer; const Value: Extended);
    procedure SetString(Const ColumnIndex: Integer; const Value: String); virtual;
    procedure SetPAnsiChar(Const ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUInt); overload; virtual;
    procedure SetPAnsiChar(Const ColumnIndex: Integer; const Value: PAnsiChar); overload; virtual;
    procedure SetPWideChar(Const ColumnIndex: Integer; Value: PWideChar; Len: PNativeUInt); virtual;
    procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); virtual;
    procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); virtual;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); virtual;
    procedure SetBytes(Const ColumnIndex: Integer; const Value: TBytes); overload; virtual;
    procedure SetBytes(Const ColumnIndex: Integer; Buf: Pointer; Len: Word); overload; virtual;
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
    property ConSettings: PZConSettings read FConSettings;
  end;

  {** Implements a raw-string based column buffer accessor. }
  TZRawRowAccessor = class(TZRowAccessor)
  public
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer; const CloneLobs: Boolean = False); override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PAnsiChar; override;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; override;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; override;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; override;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; override;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; override;
    function GetPWideChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PWideChar; override;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetString(Const ColumnIndex: Integer; const Value: String); override;
    procedure SetPAnsiChar(Const ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUInt); override;
    procedure SetPWideChar(Const ColumnIndex: Integer; Value: PWideChar; Len: PNativeUInt); override;
    //procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); override;
    //procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); override;
  end;

  {** Implements a unicode-string based column buffer accessor. }
  TZUnicodeRowAccessor = class(TZRowAccessor)
  public
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer; const CloneLobs: Boolean = False); override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PAnsiChar; override;
    function GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec; override;
    function GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String; override;
    function GetAnsiString(Const ColumnIndex: Integer; var IsNull: Boolean): AnsiString; override;
    function GetUTF8String(Const ColumnIndex: Integer; var IsNull: Boolean): UTF8String; override;
    function GetRawByteString(Const ColumnIndex: Integer; var IsNull: Boolean): RawByteString; override;
    function GetPWideChar(Const ColumnIndex: Integer; var IsNull: Boolean; out Len: NativeUInt): PWideChar; override;
    function GetUnicodeString(Const ColumnIndex: Integer; var IsNull: Boolean): ZWideString; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetString(Const ColumnIndex: Integer; const Value: String); override;
    procedure SetPAnsiChar(Const ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUInt); override;
    procedure SetPWideChar(Const ColumnIndex: Integer; Value: PWideChar; Len: PNativeUInt); override;
    //procedure SetAnsiString(Const ColumnIndex: Integer; const Value: AnsiString); override;
    //procedure SetUTF8String(Const ColumnIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(Const ColumnIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(Const ColumnIndex: Integer; const Value: ZWideString); override;
  end;

const
  RowHeaderSize = SizeOf(TZRowBuffer) - SizeOf(TZByteArray);
  {we revert the normal Boolean anlogy. We Use True = 0 and False = 1! Beacuse:
    This avoids an extra Setting of Null bytes after calling FillChar(x,y,#0)}
  bIsNull = Byte(0);
  bIsNotNull = Byte(1);

implementation

uses ZFastcode, Math, ZMessages, ZSysUtils, ZDbcUtils, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

const
  PAnsiInc = SizeOf(Cardinal);
  PWideInc = SizeOf(Word); //PWide inc assumes allways two byte

function CompareNothing(const {%H-}Null1, {%H-}Null2: Boolean; const {%H-}V1, {%H-}V2): Integer; //emergency exit for types we can't sort like arrays, dataset ...
begin
  Result := 0;
end;

function CompareBoolean_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PWordBool(V1)^) - Ord(PWordBool(V2)^); //overflow safe
end;

function CompareBoolean_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBoolean_Asc(Null1, Null2, V1, V2);
end;

function CompareBoolean_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PWordBool(V1)^ <> PWordBool(V2)^);
end;

function CompareByte_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := PByte(V1)^ - PByte(V2)^; //overflow safe
end;

function CompareByte_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareByte_Asc(Null1, Null2, V1, V2);
end;

function CompareByte_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PByte(V1)^ <> PByte(V2)^);
end;

function CompareShort_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := PShortInt(V1)^ - PShortInt(V2)^; //overflow safe
end;

function CompareShort_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareShort_Asc(Null1, Null2, V1, V2);
end;

function CompareShort_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PShortInt(V1)^ <> PShortInt(V2)^);
end;

function CompareWord_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := PWord(V1)^ - PWord(V2)^; //overflow safe
end;

function CompareWord_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareWord_Asc(Null1, Null2, V1, V2);
end;

function CompareWord_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PWord(V1)^ <> PWord(V2)^);
end;

function CompareSmallInt_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := PSmallInt(V1)^ - PSmallInt(V2)^; //overflow safe
end;

function CompareSmallInt_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareSmallInt_Asc(Null1, Null2, V1, V2);
end;

function CompareSmallInt_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PSmallInt(V1)^ <> PSmallInt(V2)^);
end;

function CompareLongWord_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PLongWord(V1)^ > PLongWord(V2)^)-Ord(PLongWord(V1)^ < PLongWord(V2)^);
end;

function CompareLongWord_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareLongWord_Asc(Null1, Null2, V1, V2);
end;

function CompareLongWord_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PLongWord(V1)^ <> PLongWord(V2)^);
end;

function CompareInteger_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else
  //function ShaCompareInt(Item1, Item2: Pointer): Integer;
  begin //on 100 mio execs 200ms faster
    Result := PInteger(V1)^;
    if Result xor PInteger(V2)^>=0
      then Result:=Result-PInteger(V2)^
      else Result:=Result or 1;
    end; //Than My (EH) overflow save idea
    //Result := Ord(PLongInt(V1)^ > PLongInt(V2)^)-Ord(PLongInt(V1)^ < PLongInt(V2)^);
end;

function CompareInteger_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInteger_Asc(Null1, Null2, V1, V2);
end;

function CompareInteger_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PLongInt(V1)^ <> PLongInt(V2)^);
end;

function CompareInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PInt64(V1)^ > PInt64(V2)^)-Ord(PInt64(V1)^ < PInt64(V2)^);
end;

function CompareInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareInt64_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PInt64(V1)^ <> PInt64(V2)^);
end;

function CompareUInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PUInt64(V1)^ > PUInt64(V2)^)-Ord(PUInt64(V1)^ < PUInt64(V2)^);
end;

function CompareUInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareUInt64_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PUInt64(V1)^ <> PUInt64(V2)^);
end;

function CompareSingle_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PSingle(V1)^ > PSingle(V2)^)-Ord(PSingle(V1)^ < PSingle(V2)^);
end;

function CompareSingle_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareSingle_Asc(Null1, Null2, V1, V2);
end;

function CompareSingle_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PSingle(V1)^ <> PSingle(V2)^);
end;

function CompareDouble_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PDouble(V1)^ > PDouble(V2)^)-Ord(PDouble(V1)^ < PDouble(V2)^);
end;

function CompareDouble_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDouble_Asc(Null1, Null2, V1, V2);
end;

function CompareDouble_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PDouble(V1)^ <> PDouble(V2)^);
end;

function CompareCurrency_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PCurrency(V1)^ > PCurrency(V2)^)-Ord(PCurrency(V1)^ < PCurrency(V2)^);
end;

function CompareCurrency_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDouble_Asc(Null1, Null2, V1, V2);
end;

function CompareCurrency_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PCurrency(V1)^ <> PCurrency(V2)^);
end;

function CompareExtended_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PExtended(V1)^ > PExtended(V2)^)-Ord(PExtended(V1)^ < PExtended(V2)^);
end;

function CompareExtended_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareExtended_Asc(Null1, Null2, V1, V2);
end;

function CompareExtended_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PExtended(V1)^ <> PExtended(V2)^);
end;

function CompareDateTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(PDateTime(V1)^ > PDateTime(V2)^)-Ord(PDateTime(V1)^ < PDateTime(V2)^);
end;

function CompareDateTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDateTime_Asc(Null1, Null2, V1, V2);
end;

function CompareDateTime_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := Ord(PDateTime(V1)^ <> PDateTime(V2)^);
end;

function CompareGUID_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZMemLComp(Pointer(V1), Pointer(V2), 16); //Not a endversion! It would be nice to compare field-by-field of TGUID
end;

function CompareGUID_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareGUID_Asc(Null1, Null2, V1, V2);
end;

function CompareGUID_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else Result := ZMemLComp(Pointer(V1), Pointer(V2), 16);
end;

function CompareBytes_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else begin
    Result := PWord(PAnsiChar(V1)+SizeOf(Pointer))^ - PWord(PAnsiChar(V1)+SizeOf(Pointer))^;
    if Result = 0 then
      Result := ZMemLComp(PPointer(V1)^, PPointer(V2)^, PWord(PAnsiChar(V1)+SizeOf(Pointer))^)
  end;
end;

function CompareRaw_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else begin
    Result := Ord(PLongWord(Pointer(V1)^)^ <> PLongWord(Pointer(V2)^)^);
    if Result = 0 then
       Result := ZMemLComp(PAnsiChar(Pointer(V1)^)+PAnsiInc,
                           PAnsiChar(Pointer(V2)^)+PAnsiInc,
                           PLongWord(Pointer(V1)^)^);
  end;
end;

function CompareNativeRaw_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else
    {$IFDEF MSWINDOWS}
    Result := CompareStringA(LOCALE_USER_DEFAULT, 0,
      PAnsiChar(Pointer(V1)^)+PAnsiInc, PLongWord(Pointer(V1)^)^,
      PAnsiChar(Pointer(V2)^)+PAnsiInc, PLongWord(Pointer(V2)^)^) - 2;{CSTR_EQUAL}
    {$ELSE}
      if Assigned(PPAnsichar(Pointer(V1)^)) and Assigned(PPAnsiChar(Pointer(V2)^)) then
        Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}
          AnsiStrComp(PPAnsiChar(V1)^+PAnsiInc, PPAnsiChar(V2)^+PAnsiInc)
      else //we've found out on other (FPC)OS's a nil compare crahs!
        if Assigned(PPAnsichar(Pointer(V1)^))
        then Result := 1
        else Result := -1;
    {$ENDIF}
end;

function CompareNativeRaw_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareNativeRaw_Asc(Null1, Null2, V1, V2);
end;

function CompareUnicodeFromUTF8_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  S1, S2: ZWideString;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else begin
    S1 := PRawToUnicode(PAnsiChar(Pointer(V1)^)+PAnsiInc, PLongWord(Pointer(V1)^)^, zCP_UTF8);
    S2 := PRawToUnicode(PAnsiChar(Pointer(V2)^)+PAnsiInc, PLongWord(Pointer(V2)^)^, zCP_UTF8);
    Result := WideCompareStr(S1, S2);
  end;
end;

function CompareUnicodeFromUTF8_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicodeFromUTF8_Asc(Null1, Null2, V1, V2);
end;

function CompareUnicode_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
{$IFNDEF MSWINDOWS}
var S1, S2: ZWideString;
{$ENDIF}
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else begin
    {$IFDEF MSWINDOWS}
    SetLastError(0);
    Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
      PWideChar(Pointer(V1)^)+PWideInc, PCardinal(Pointer(V1)^)^,
      PWideChar(Pointer(V2)^)+PWideInc, PCardinal(Pointer(V2)^)^) - 2{CSTR_EQUAL};
    if GetLastError <> 0 then
      RaiseLastOSError;
    {$ELSE}
    System.SetString(S1, PWideChar(Pointer(V1)^)+PWideInc, PCardinal(Pointer(V1)^)^);
    System.SetString(S2, PWideChar(Pointer(V2)^)+PWideInc, PCardinal(Pointer(V2)^)^);
    Result := WideCompareStr(S1, S2);
    {$ENDIF}
  end;
end;

function CompareUnicode_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicode_Asc(Null1, Null2, V1, V2);
end;

function CompareUnicode_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 <> Null2 then Result := 1
  else begin
    Result := Ord(PLongWord(Pointer(V1)^)^ <> PLongWord(Pointer(V2)^)^);
    if Result = 0 then
       Result := ZMemLComp(Pointer(PWideChar(Pointer(V1)^)+PWideInc),
                           Pointer(PWideChar(Pointer(V2)^)+PWideInc),
                           PLongWord(Pointer(V1)^)^ shl 1);
  end;
end;


function CompareNativeCLob_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
  BlobEmpty1, BlobEmpty2: Boolean;
begin
  Blob1 := IZBlob(PPointer(V1)^);
  BlobEmpty1 := (Blob1 = nil) or (Blob1.IsEmpty);
  Blob2 := IZBlob(PPointer(V2)^);
  BlobEmpty2 := (Blob2 = nil) or (Blob2.IsEmpty);
  if BlobEmpty1 and BlobEmpty2 then Result := 0
  else if (BlobEmpty1 <> BlobEmpty2) then Result := 1
  else if Blob1.IsUpdated or Blob2.IsUpdated then
    Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString)
  else Result := 0;
end;

function CompareUnicodeCLob_Equals(const {%H-}Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
  BlobEmpty1, BlobEmpty2: Boolean;
  {$IFDEF MSWINDOWS}
  ValuePtr1, ValuePtr2: Pointer;
  {$ENDIF}
begin
  Blob1 := IZBlob(PPointer(V1)^);
  BlobEmpty1 := (Blob1 = nil) or (Blob1.IsEmpty);
  Blob2 := IZBlob(PPointer(V2)^);
  BlobEmpty2 := (Blob2 = nil) or (Blob2.IsEmpty);
  if BlobEmpty1 and BlobEmpty2 then Result := 0
  else if (BlobEmpty1 <> BlobEmpty2) then Result := 1
  else if Blob1.IsUpdated or Blob2.IsUpdated then
    if Blob1.IsClob and Blob2.IsClob then
    begin
      {$IFDEF MSWINDOWS}
      ValuePtr1 := Blob1.GetPWideChar;
      ValuePtr2 := Blob2.GetPWideChar;
      SetLastError(0);
      Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
        ValuePtr1, Blob1.Length, ValuePtr2, Blob2.Length) - 2{CSTR_EQUAL};
      if GetLastError <> 0 then RaiseLastOSError;
      {$ELSE}
      Result := WideCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
      {$ENDIF}
    end
    else
      Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString)
  else Result := 0;
end;

function CompareBlob_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
  BlobEmpty1, BlobEmpty2: Boolean;
begin
  Blob1 := IZBlob(PPointer(V1)^);
  BlobEmpty1 := (Blob1 = nil) or (Blob1.IsEmpty);
  Blob2 := IZBlob(PPointer(V2)^);
  BlobEmpty2 := (Blob2 = nil) or (Blob2.IsEmpty);
  if BlobEmpty1 and BlobEmpty2 then Result := 0
  else if (BlobEmpty1 <> BlobEmpty2) then Result := 1
  else if Blob1.IsUpdated or Blob2.IsUpdated then
    if Blob1.Length <> Blob2.Length then Result := 1
    else Result := ZMemLComp(Blob1.GetBuffer, Blob2.GetBuffer, Blob1.Length)
  else Result := 1;
end;

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
    FColumnsSize := align(FColumnsSize+1,sizeof(pointer))-1;
    {$endif}
    if Current.ColumnType = stBytes then
    begin
      FColumnLengths[I] := Current.Precision;
      SetLength(FBytesCols, Length(FBytesCols)+1);
      FBytesCols[High(FBytesCols)] := I;
    end
    else if Current.ColumnType in [stString, stUnicodeString] then
    begin
      FColumnLengths[I] := Current.Precision;
      SetLength(FStringCols, Length(FStringCols)+1);
      FStringCols[High(FStringCols)] := I;
    end
    else if Current.ColumnType in [stAsciiStream, stUnicodeStream, stBinaryStream]then
    begin
      SetLength(FLobCols, Length(FLobCols)+1);
      FLobCols[High(FLobCols)] := I;
    end
    else if Current.ColumnType = stArray then
    begin
      SetLength(FArrayCols, Length(FArrayCols)+1);
      FArrayCols[High(FArrayCols)] := I;
    end
    else if Current.ColumnType = stDataSet then
    begin
      SetLength(FDataSetCols, Length(FDataSetCols)+1);
      FDataSetCols[High(FDataSetCols)] := I;
    end;
  end;
  FHighBytesCols := Length(FBytesCols)-1;
  FHighStringCols := Length(FStringCols)-1;
  FHighArrayCols := Length(FArrayCols)-1;
  FHighLobCols := Length(FLobCols)-1;
  FHighDataSetCols := Length(FDataSetCols)-1;
  FHasBytes := FHighBytesCols > -1;
  FHasStrings := FHighStringCols > -1;
  FHasArrays := FHighArrayCols > -1;
  FHasLobs := FHighLobCols > -1;
  FHasDataSets := FHighDataSetCols > -1;
  FRowSize := FColumnsSize + RowHeaderSize;
end;

{**
  Checks is the column index correct and row buffer is available.
  @param ColumnIndex an index of column.
}
procedure TZRowAccessor.CheckColumnIndex(Const ColumnIndex: Integer);
begin
  if FBuffer = nil then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex {$IFDEF GENERIC_INDEX}<{$ELSE}<={$ENDIF}0) or
     (ColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF}FColumnCount) then
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

  if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > FColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  if not CheckConvertion(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}], ResultType) then
    raise EZSQLException.Create(
      Format(SConvertionIsNotPossible, [ColumnIndex,
      DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]),
      DefineColumnTypeName(ResultType)]));
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
    stWord:
      Result := SizeOf(Word);
    stSmall:
      Result := SizeOf(SmallInt);
    stLongWord:
      Result := SizeOf(LongWord);
    stInteger:
      Result := SizeOf(Integer);
    stULong:
      Result := SizeOf(UInt64);
    stLong:
      Result := SizeOf(Int64);
    stFloat:
      Result := SizeOf(Single);
    stDouble:
      Result := SizeOf(Double);
    stCurrency:
      Result := SizeOf(Currency);
    stBigDecimal:
      Result := SizeOf(Extended);
    stString, stUnicodeString,
    stAsciiStream, stUnicodeStream, stBinaryStream,
    stDataSet, stArray:
      Result := SizeOf(Pointer);
    stGUID: Result := 16;
    stBytes:
      Result := SizeOf(Pointer) + SizeOf(Word);
    stDate, stTime, stTimestamp:
      Result := SizeOf(TDateTime);
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
begin
  if Buffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
    Result := IZBlob(PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^)
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
  ColumnIndex: Integer; const Value: IZBlob);
var LobPtr: PPointer;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}

  LobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
  if Buffer.Columns[FColumnOffsets[ColumnIndex]] = bIsNotNull then
    IZBlob(LobPtr^) := nil //Free Interface reference
  else
    LobPtr^ := nil; //nil Pointer reference without Interface ref -> do NOT dec refcount

  IZBlob(LobPtr^) := Value; //set new value

  if Value = nil then
    Buffer.Columns[FColumnOffsets[ColumnIndex]] := bIsNull  //Set null byte
  else
    Buffer.Columns[FColumnOffsets[ColumnIndex]] := bIsNotNull; //Set not null byte
end;

function TZRowAccessor.InternalGetBytes(const Buffer: PZRowBuffer;
  ColumnIndex: Integer): TBytes;
var
  P: Pointer;
  L: Word;
begin
  P := InternalGetBytes(Buffer, ColumnIndex, L{%H-});
  if P <> nil then begin
    SetLength(Result, L);
    Move(P^, Pointer(Result)^, L);
  end else
    Result := nil;
end;

function TZRowAccessor.InternalGetBytes(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; var Len: Word): Pointer;
begin
  Result := nil;
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  if ( Buffer.Columns[FColumnOffsets[ColumnIndex]] = bIsNotNull )then
  begin
    Len := PWord(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1 + SizeOf(Pointer)])^;
    if Len > 0 then
      Result := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^;
  end else Len := 0;
end;

procedure TZRowAccessor.InternalSetBytes(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; const Value: TBytes;
  const NewPointer: Boolean = False);
begin
  InternalSetBytes(Buffer, ColumnIndex, Pointer(Value), Length(Value), NewPointer);
end;

procedure TZRowAccessor.InternalSetBytes(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; Buf: Pointer; Len: Word;
  const NewPointer: Boolean = False);
var
  P: PPointer;
begin
  if Buffer <> nil then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
    P := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
    Len := Min(Len, FColumnLengths[ColumnIndex]);
    PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex] + 1 + SizeOf(Pointer)])^ := Len;
    if Len > 0 then
    begin
      ReallocMem(P^, Len);
      System.Move(Buf^, P^^, Len);
    end
    else
      if PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ > 0 then
      begin
        System.FreeMem(P^);
        PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
      end;
  end;
end;

procedure TZRowAccessor.InternalSetString(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; const Value: RawByteString;
  const NewPointer: Boolean = False);
var
  C: PPAnsiChar;
  L: Cardinal;
begin
  if Buffer <> nil then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
    C := PPAnsiChar(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
    L := Length(Value);
    ReallocMem(C^, L +SizeOf(Cardinal)+1); //add space for leading #0
    System.Move(Pointer(Value)^, (C^+PAnsiInc)^, L);
    PLongWord(C^)^ := L;
    (C^+PAnsiInc+L)^ := #0; //set #0 terminator if a truncation is required e.g. FireBird Char columns with trailing spaces
  end;
end;

procedure TZRowAccessor.InternalSetUnicodeString(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; const Value: ZWideString;
  const NewPointer: Boolean = False);
var
  W: ZPPWideChar;
  LStr, LMem: Cardinal;
begin
  if Buffer <> nil then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
    W := ZPPWideChar(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
    LStr := Length(Value);
    LMem := LStr shl 1;
    ReallocMem(W^, LMem+SizeOf(Cardinal)+2); //including #0#0 terminator
    System.Move(Pointer(Value)^, (W^+PWideInc)^, LMem);
    PLongWord(W^)^ := LStr;
    (W^+PWideInc+LStr)^ := WideChar(#0);
  end;
end;

procedure TZRowAccessor.InternalSetPWideChar(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; const Value: PWideChar; const Len: Cardinal;
  const NewPointer: Boolean = False);
var
  W: ZPPWideChar;
  LMem: Cardinal;
begin
  if Buffer <> nil then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
    W := ZPPWideChar(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
    LMem := Len shl 1;
    ReallocMem(W^, LMem+SizeOf(Cardinal)+2); //including #0#0 terminator
    System.Move(Value^, (W^+PWideInc)^, LMem);
    PLongWord(W^)^ := Len;
    (W^+PWideInc+Len)^ := WideChar(#0);
  end;
end;


procedure TZRowAccessor.InternalSetPAnsiChar(const Buffer: PZRowBuffer;
  ColumnIndex: Integer; const Value: PAnsiChar; const Len: Cardinal;
  const NewPointer: Boolean = False);
var
  C: PPAnsiChar;
begin
  if Buffer <> nil then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    if NewPointer then
      PNativeUInt(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1])^ := 0;
    C := PPAnsiChar(@Buffer.Columns[FColumnOffsets[ColumnIndex] + 1]);
    ReallocMem(C^, Len+SizeOf(LongWord)+1);
    Move(Value^, (C^+PAnsiInc)^, Len);
    PLongWord(C^)^ := Len;
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
  if Buffer <> nil then
  begin
    ClearBuffer(Buffer, False);
    FreeMem(Buffer);
  end;
end;

{**
  Initializes the row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.InitBuffer(Buffer: PZRowBuffer);
begin
  if Buffer <> nil then
  begin
    Buffer^.Index := 0;
    Buffer^.BookmarkFlag := 0;//bfCurrent;
    Buffer^.UpdateType := utUnmodified;
    FillChar(Buffer^.Columns, FColumnsSize, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
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
  ClearBuffer(SrcBuffer, True);
end;

{**
  Clones the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, DestBuffer, True);
end;

{**
  Compares fields from two row buffers.
  @param Buffer1 the first row buffer to compare.
  @param Buffer2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZRowAccessor.CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  ValuePtr1, ValuePtr2: Pointer;
begin
  Result := 0; //satisfy compiler
  for I := Low(ColumnIndices) to High(ColumnIndices) do
  begin
    ColumnIndex := ColumnIndices[I]{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
    { Compares column values. }
    ValuePtr1 := @Buffer1.Columns[FColumnOffsets[ColumnIndex] + 1];
    ValuePtr2 := @Buffer2.Columns[FColumnOffsets[ColumnIndex] + 1];
    Result := CompareFuncs[i](
      (Buffer1.Columns[FColumnOffsets[ColumnIndex]] = bIsNull),
      (Buffer2.Columns[FColumnOffsets[ColumnIndex]] = bIsNull),
        ValuePtr1, ValuePtr2);
    if Result <> 0 then
      Break;
  end;
end;

{**
  Return an array of Compare funtions
  @param ColumnIndex the columnIndex first is 1, second is 2 ....
  @param Ascending indicate if a Ascending compare should be used
  @param EqualsCompare indicate if we string comparison should check equals mem(update comparison) or human(sorts f.e.) kind
  returns the array of "best fit" compare functions
}
function TZRowAccessor.GetCompareFunc(ColumnIndex: Integer;
  const CompareKind: TComparisonKind): TCompareFunc;
begin
  Result := CompareNothing;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] of
    stBoolean:
      case CompareKind of
        ckAscending:  Result := CompareBoolean_Asc;
        ckDescending: Result := CompareBoolean_Desc;
        ckEquals:     Result := CompareBoolean_Equals;
      end;
    stShort:
      case CompareKind of
        ckAscending:  Result := CompareShort_Asc;
        ckDescending: Result := CompareShort_Desc;
        ckEquals:     Result := CompareShort_Equals;
      end;
    stByte:
      case CompareKind of
        ckAscending:  Result := CompareByte_Asc;
        ckDescending: Result := CompareByte_Desc;
        ckEquals:     Result := CompareByte_Equals;
      end;
    stSmall:
      case CompareKind of
        ckAscending:  Result := CompareSmallInt_Asc;
        ckDescending: Result := CompareSmallInt_Desc;
        ckEquals:     Result := CompareSmallInt_Equals;
      end;
    stWord:
      case CompareKind of
        ckAscending:  Result := CompareWord_Asc;
        ckDescending: Result := CompareWord_Desc;
        ckEquals:     Result := CompareWord_Equals;
      end;
    stInteger:
      case CompareKind of
        ckAscending:  Result := CompareInteger_Asc;
        ckDescending: Result := CompareInteger_Desc;
        ckEquals:     Result := CompareInteger_Equals;
      end;
    stLongWord:
      case CompareKind of
        ckAscending:  Result := CompareLongWord_Asc;
        ckDescending: Result := CompareLongWord_Desc;
        ckEquals:     Result := CompareLongWord_Equals;
      end;
    stLong:
      case CompareKind of
        ckAscending:  Result := CompareInt64_Asc;
        ckDescending: Result := CompareInt64_Desc;
        ckEquals:     Result := CompareInt64_Equals;
      end;
    stULong:
      case CompareKind of
        ckAscending:  Result := CompareUInt64_Asc;
        ckDescending: Result := CompareUInt64_Desc;
        ckEquals:     Result := CompareUInt64_Equals;
      end;
    stFloat:
      case CompareKind of
        ckAscending:  Result := CompareSingle_Asc;
        ckDescending: Result := CompareSingle_Desc;
        ckEquals:     Result := CompareSingle_Equals;
      end;
    stDouble:
      case CompareKind of
        ckAscending:  Result := CompareDouble_Asc;
        ckDescending: Result := CompareDouble_Desc;
        ckEquals:     Result := CompareDouble_Equals;
      end;
    stCurrency:
      case CompareKind of
        ckAscending:  Result := CompareCurrency_Asc;
        ckDescending: Result := CompareCurrency_Desc;
        ckEquals:     Result := CompareCurrency_Equals;
      end;
    stBigDecimal:
      case CompareKind of
        ckAscending:  Result := CompareExtended_Asc;
        ckDescending: Result := CompareExtended_Desc;
        ckEquals:     Result := CompareExtended_Equals;
      end;
    stDate, stTime, stTimestamp:
      case CompareKind of
        ckAscending:  Result := CompareDateTime_Asc;
        ckDescending: Result := CompareDateTime_Desc;
        ckEquals:     Result := CompareDateTime_Equals;
      end;
    stGUID:
      case CompareKind of
        ckAscending:  Result := CompareGUID_Asc;
        ckDescending: Result := CompareGUID_Desc;
        ckEquals:     Result := CompareGUID_Equals;
      end;
    stBytes:
      if CompareKind = ckEquals then Result := CompareBytes_Equals;
    stBinaryStream:
      if CompareKind = ckEquals then
        Result := CompareBLob_Equals;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        case CompareKind of
          ckAscending:
            if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
              Result := CompareUnicodeFromUTF8_Asc
            else
              Result := CompareNativeRaw_Asc;
          ckDescending:
            if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
              Result := CompareUnicodeFromUTF8_Desc
            else
              Result := CompareNativeRaw_Desc;
          ckEquals:     Result := CompareRaw_Equals;
        end
      else
        case CompareKind of
          ckAscending:  Result := CompareUnicode_Asc;
          ckDescending: Result := CompareUnicode_Desc;
          ckEquals:     Result := CompareUnicode_Equals;
        end;
    stAsciiStream, stUnicodeStream:
      if CompareKind = ckEquals then
        if ConSettings^.CPType in [cCP_UTF16, cCP_UTF8] then
          Result := CompareUnicodeCLob_Equals
        else
          Result := CompareNativeCLob_Equals;
  end;
end;

function TZRowAccessor.GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
  const CompareKinds: TComparisonKindArray): TCompareFuncs;
var I: Integer;
begin
  SetLength(Result, Length(ColumnIndices));
  for i := low(ColumnIndices) to high(ColumnIndices) do
    Result[i] := GetCompareFunc(ColumnIndices[I], CompareKinds[i]);
end;

{**
  Cleans the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.ClearBuffer(Buffer: PZRowBuffer; const WithFillChar: Boolean = True);
var
  I: Integer;
  P: PPointer;
begin
  Buffer^.Index := -1;
  Buffer^.UpdateType := utUnmodified;
  Buffer^.BookmarkFlag := 0;
  for I := 0 to FColumnCount - 1 do
    case FColumnTypes[I] of
      stAsciiStream, stUnicodeStream, stBinaryStream:
        if (Buffer^.Columns[FColumnOffsets[I]] = bIsNotNull) then
          SetBlobObject(Buffer, I {$IFNDEF GENERIC_INDEX}+ 1{$ENDIF}, nil);
      stBytes,stString, stUnicodeString:
        if PNativeUInt(@Buffer^.Columns[FColumnOffsets[I] +1])^ > 0 then
        begin
          P := PPointer(@Buffer^.Columns[FColumnOffsets[I] +1]);
          System.FreeMem(P^);
        end;
    end;
  if WithFillChar then
    FillChar(Buffer^.Columns, FColumnsSize, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
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
  ClearBuffer(FBuffer, True);
end;

{**
  Gets the case sensitive flag of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the case sensitive flag of the column data buffer.
}
function TZRowAccessor.GetColumnCase(Const ColumnIndex: Integer): Boolean;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnCases[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets a pointer to the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a pointer to the column data buffer.
}
function TZRowAccessor.GetColumnData(Const ColumnIndex: Integer;
  var IsNull: Boolean): Pointer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex {$IFNDEF GENERIC_INDEX}- 1{$ENDIF}] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull;
end;

{**
  Gets a size of the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a size of the column data buffer.
}
function TZRowAccessor.GetColumnDataSize(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}];
end;

{**
  Gets then length of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the length of the column data buffer.
}
function TZRowAccessor.GetColumnLength(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then name of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the name of the column data buffer.
}
function TZRowAccessor.GetColumnName(Const ColumnIndex: Integer): string;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnNames[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then offset of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return then offset of the column data buffer.
}
function TZRowAccessor.GetColumnOffSet(Const ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnOffSets[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then SQLType of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the SQLType of the column data buffer.
}
function TZRowAccessor.GetColumnType(Const ColumnIndex: Integer): TZSQLType;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

function TZRowAccessor.GetColumnDefaultExpression(Const ColumnIndex: Integer): string;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnDefaultExpressions[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

procedure TZRowAccessor.SetColumnDefaultExpression(Const ColumnIndex: Integer; const Value: string);
begin
  FColumnDefaultExpressions[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

procedure TZRowAccessor.SetColumnCodePage(Const ColumnIndex: Integer; const Value: Word);
begin
  FColumnCodePages[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
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
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > FColumnCount {$IFDEF GENERIC_INDEX} -1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  Result := FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull;
  if not Result and (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stAsciiStream,
    stBinaryStream, stUnicodeStream]) then
  begin
    TempBlob := GetBlobObject(FBuffer, ColumnIndex);
    Result := (TempBlob = nil) or TempBlob.IsEmpty;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the string in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean;
  out Len: NativeUInt): PAnsiChar;
var
  Blob: IZBlob;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then
        FRawTemp := 'True'
      else
        FRawTemp := 'False';
    stByte: FRawTemp := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: FRawTemp := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: FRawTemp := IntToRaw(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: FRawTemp := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: FRawTemp := IntToRaw(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: FRawTemp := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: FRawTemp := IntToRaw(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: FRawTemp := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: FRawTemp := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: FRawTemp := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: FRawTemp := FloatToSqlRaw(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: FRawTemp := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: FRawTemp := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID: FRawTemp := GUIDToRaw(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: FRawTemp := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: FRawTemp := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: FRawTemp := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlobObject(FBuffer, ColumnIndex);
        if (Blob <> nil) and not Blob.IsEmpty then
          if Blob.IsClob then
          begin
            if ConSettings^.AutoEncode then
              Result := Blob.GetPAnsiChar(ConSettings^.CTRL_CP)
            else
              Result := Blob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
            Len := Blob.Length;
          end
          else
          begin
            Result := Blob.GetBuffer;
            Len := Blob.Length;
          end
        else
        begin
          Result := nil;
          Len := 0;
        end;
        Exit;
      end;
    stBinaryStream:
      begin
        Blob := GetBlobObject(FBuffer, ColumnIndex);
        if (Blob <> nil) and not Blob.IsEmpty then
        begin
          Result := Blob.GetBuffer;
          Len := Blob.Length;
        end
        else
        begin
          Result := nil;
          Len := 0;
        end;
        Exit;
      end;
    else
      FRawTemp := {$IFDEF WITH_RAWBYTESTRING}RawByteString{$ENDIF}(GetString(ColumnIndex, IsNull));
  end;
  Len := Length(FRawTemp);
  Result := Pointer(FRawTemp);
end;

function TZRowAccessor.GetString(Const ColumnIndex: Integer; var IsNull: Boolean): String;
var
  TempBlob: IZBlob;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := ZFastCode.IntToStr(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: Result := ZFastCode.IntToStr(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: Result := ZFastCode.IntToStr(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: Result := ZFastCode.IntToStr(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: Result := ZFastCode.IntToStr(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: Result := ZFastCode.IntToStr(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: Result := ZFastCode.IntToStr(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: Result := ZFastCode.IntToStr(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: Result := FloatToSQLStr(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: Result := FloatToSQLStr(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: Result := FloatToSQLStr(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: Result := FloatToSQLStr(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
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
            Result := {$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(TempBlob.GetString);
      end;
    stBytes: Result := {$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(BytesToStr(GetBytes(ColumnIndex, IsNull)));
    stGUID: Result := {$IFDEF UNICODE}GUIDToUnicode{$ELSE}GUIDToRaw{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: Result := FormatDateTime('yyyy-mm-dd',
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stTime: Result := FormatDateTime('hh:mm:ss',
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stTimestamp: Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stAsciiStream, stBinaryStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          Result := {$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(TempBlob.GetString);
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
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: Result := IntToRaw(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: Result := IntToRaw(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: Result := IntToRaw(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: Result := FloatToSqlRaw(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID: Result := GUIDToRaw(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
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
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: Result := IntToRaw(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: Result := IntToRaw(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: Result := IntToRaw(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: Result := FloatToSqlRaw(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID: Result := GUIDToRaw(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
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
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
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
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean:
      if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then
        Result := 'True'
      else
        Result := 'False';
    stByte: Result := IntToRaw(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: Result := IntToRaw(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: Result := IntToRaw(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: Result := IntToRaw(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: Result := IntToRaw(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: Result := IntToRaw(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: Result := IntToRaw(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: Result := IntToRaw(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: Result := FloatToSqlRaw(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: Result := FloatToSqlRaw(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: Result := FloatToSqlRaw(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: Result := FloatToSqlRaw(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    //stString, stUnicodeString: do not handle here!
    stBytes: Result := BytesToStr(GetBytes(ColumnIndex, IsNull));
    stGUID: Result := GUIDToRaw(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: Result := DateTimeToRawSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToRawSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToRawSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
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
  a <code>PWideChar</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetPWideChar(Const ColumnIndex: Integer;
  var IsNull: Boolean; out Len: NativeUInt): PWideChar;
var
  TempBlob: IZBlob;
  Bts: TBytes;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stByte: FUniTemp := IntToUnicode(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: FUniTemp := IntToUnicode(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: FUniTemp := IntToUnicode(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: FUniTemp := IntToUnicode(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: FUniTemp := IntToUnicode(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: FUniTemp := IntToUnicode(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: FUniTemp := IntToUnicode(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: FUniTemp := IntToUnicode(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: FUniTemp := FloatToSqlUnicode(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: FUniTemp := FloatToSqlUnicode(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: FUniTemp := FloatToSqlUnicode(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: FUniTemp := FloatToSqlUnicode(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    //stUnicodeString, stString: do not handle here!
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
          begin
            Result := TempBlob.GetPWideChar;
            Len := TempBlob.Length shr 1;
            Exit;
          end
          else
            FUniTemp := ASCII7ToUnicodeString(TempBlob.GetString);
      end;
    stBytes, stBinaryStream:
      begin
        Bts := GetBytes(ColumnIndex, IsNull);
        FUniTemp := ASCII7ToUnicodeString(Pointer(Bts), Length(Bts));
      end;
    stGUID: FUniTemp := GUIDToUnicode(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: FUniTemp := DateTimeToUnicodeSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: FUniTemp := DateTimeToUnicodeSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: FUniTemp := DateTimeToUnicodeSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    else
      FUniTemp := {$IFNDEF UNICODE}ZWideString{$ENDIF}(GetString(ColumnIndex, IsNull));
  end;
  Result := Pointer(FUniTemp);
  Len := Length(FuniTemp);
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
  Bts: TBytes;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stByte: Result := IntToUnicode(PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stShort: Result := IntToUnicode(PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stWord: Result := IntToUnicode(PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stSmall: Result := IntToUnicode(PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLongWord: Result := IntToUnicode(PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stInteger: Result := IntToUnicode(PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stULong: Result := IntToUnicode(PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stLong: Result := IntToUnicode(PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stFloat: Result := FloatToSqlUnicode(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stDouble: Result := FloatToSqlUnicode(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stCurrency: Result := FloatToSqlUnicode(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    stBigDecimal: Result := FloatToSqlUnicode(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
    //stUnicodeString, stString: do not handle here!
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlobObject(FBuffer, ColumnIndex);
        if (TempBlob <> nil) and not TempBlob.IsEmpty then
          if TempBlob.IsClob then
            Result := TempBlob.GetUnicodeString
          else
            Result := ASCII7ToUnicodeString(TempBlob.GetString);
      end;
    stBytes, stBinaryStream:
      begin
        Bts := GetBytes(ColumnIndex, IsNull);
        Result := ASCII7ToUnicodeString(Pointer(Bts), Length(Bts));
      end;
    stGUID: Result := GUIDToUnicode(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
    stDate: Result := DateTimeToUnicodeSQLDate(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTime: Result := DateTimeToUnicodeSQLTime(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
      ConSettings^.DisplayFormatSettings, False);
    stTimestamp: Result := DateTimeToUnicodeSQLTimeStamp(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^,
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: Result := PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stCurrency: Result := PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ <> 0;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := StrToBoolEx(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, False)
        else
          Result := StrToBoolEx(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, False);
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
      stUnicodeStream, stAsciiStream: Result := RawToIntDef(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, IsNull).GetString, 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetShort(Const ColumnIndex: Integer; var IsNull: Boolean): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
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
  a <code>Word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetWord(Const ColumnIndex: Integer; var IsNull: Boolean): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
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
  a <code>small</code> in the Java programming language.

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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
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
  an <code>Cardinal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetUInt(Const ColumnIndex: Integer; var IsNull: Boolean): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToUInt64Def(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToUInt64Def(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToIntDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToIntDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
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
  a <code>Ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetULong(Const ColumnIndex: Integer; var IsNull: Boolean): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToUInt64Def(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToUInt64Def(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
      stAsciiStream: Result := RawToUInt64Def(GetBlob(ColumnIndex, IsNull).GetPAnsiChar(zCP_us_ascii), 0);
      stUnicodeStream: Result := UnicodeToUInt64Def(GetBlob(ColumnIndex, IsNull).GetPWideChar, 0);
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stCurrency: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stBigDecimal: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := RawToInt64Def(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0)
        else
          Result := UnicodeToInt64Def(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0);
      stAsciiStream: Result := RawToInt64Def(GetBlob(ColumnIndex, IsNull).GetPAnsiChar(zCP_us_ascii), 0);
      stUnicodeStream: Result := UnicodeToInt64Def(GetBlob(ColumnIndex, IsNull).GetPWideChar, 0);
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
var TempLob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stCurrency: Result := PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0, Result,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
      stAsciiStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
        end;
      stUnicodeStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPWideChar, 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
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
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetDouble(Const ColumnIndex: Integer; var IsNull: Boolean): Double;
var TempLob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stCurrency: Result := PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stTime, stDate, stTimeStamp: Result := PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0, Result,
           PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0,
           PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
      stAsciiStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
        end;
      stUnicodeStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPWideChar, 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
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
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetCurrency(Const ColumnIndex: Integer; var IsNull: Boolean): Currency;
var TempLob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stCurrency: Result := PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0, Result,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        else
          SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0, Result,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
      stAsciiStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
        end;
      stUnicodeStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPWideChar, 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
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
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetBigDecimal(Const ColumnIndex: Integer; var IsNull: Boolean): Extended;
var
  TempLob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ then Result := 1;
      stByte: Result := PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stShort: Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stWord: Result := PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stSmall: Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLongWord: Result := PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stInteger: Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stULong: Result := PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stLong: Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stFloat: Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stDouble: Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stCurrency: Result := PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stBigDecimal: Result := PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          SQLStrToFloatDef(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc, 0, Result,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        else
          Result := SQLStrToFloatDef(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc, 0,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
      stAsciiStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
        end;
      stUnicodeStream: begin
          TempLob := GetBlob(ColumnIndex, IsNull);
          if TempLob.IsClob then
            SQLStrToFloatDef(TempLob.GetPWideChar, 0, Result)
          else
            SQLStrToFloatDef(PAnsiChar(TempLob.GetBuffer), 0, Result, TempLob.Length)
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
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stGUID:
        begin
          SetLength(Result, 16);
          System.Move(FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], Result[0], 16);
        end;
      stBytes:
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
  a <code>Pointer</code> reference.

  @param columnIndex the first column is 1, the second is 2, ...
  @param IsNull if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
  @param Len return the count of Bytes for the value
  @return the column value
}
function TZRowAccessor.GetBytes(Const ColumnIndex: Integer; var IsNull: Boolean;
  out Len: Word): Pointer;
var Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  Len := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stGUID:
        begin
          Len := 16;
          Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
        end;
      stBytes:
        Result := InternalGetBytes(FBuffer, ColumnIndex, Len);
      stBinaryStream: begin
          Blob := GetBlob(ColumnIndex, IsNull);
          if (Blob <> nil) and (not Blob.IsEmpty) then begin
            Result := Blob.GetBuffer;
            Len := Blob.Length;
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
  BufLen: LongWord;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stDate, stTime, stTimestamp:
        Result := Int(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
        begin
          AnsiBuffer := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc;
          BufLen := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
          Result := ZSysUtils.RawSQLDateToDateTime(AnsiBuffer, BufLen,
           ConSettings^.ReadFormatSettings, Failed{%H-});
          if Failed then
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ZSysUtils.RawSQLTimeStampToDateTime(
              AnsiBuffer, BufLen, ConSettings^.ReadFormatSettings, Failed));
        end
        else
        begin
          WideBuffer := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc;
          BufLen := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
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
  BufLen: LongWord;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stDate, stTime, stTimestamp:
        Result := Frac(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
        begin
          AnsiBuffer := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc;
          BufLen := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
          Result := ZSysUtils.RawSQLTimeToDateTime(AnsiBuffer, BufLen,
            ConSettings^.ReadFormatSettings, Failed{%H-});
          if Failed then
            Result := Frac(ZSysUtils.RawSQLTimeStampToDateTime(AnsiBuffer,
              BufLen, ConSettings^.ReadFormatSettings, Failed));
        end
        else
        begin
          WideBuffer := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc;
          BufLen := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stDate, stTime, stTimestamp:
        Result := PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^;
      stString, stUnicodeString:
        if Self is TZRawRowAccessor then
          Result := ZSysUtils.RawSQLTimeStampToDateTime(
            PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
                ConSettings^.ReadFormatSettings, Failed{%H-})
        else
          Result := ZSysUtils.UnicodeSQLTimeStampToDateTime(
            PPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
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
  if not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
{$ENDIF}

  Result := GetBlobObject(FBuffer, ColumnIndex);
  IsNull := Result = nil;
  if Result = nil then
  begin
    if (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stBinaryStream) then
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
  if not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stDataSet) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
{$ENDIF}

  Ptr := PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]]);

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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    ValuePtr := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stByte:
        begin
          Result.VType := vtUInteger;
          Result.VUInteger := PByte(ValuePtr)^;
        end;
      stShort:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PShortInt(ValuePtr)^;
        end;
      stWord:
        begin
          Result.VType := vtUInteger;
          Result.VUInteger := PWord(ValuePtr)^;
        end;
      stSmall:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PSmallInt(ValuePtr)^;
        end;
      stLongWord:
        begin
          Result.VType := vtUInteger;
          Result.VUInteger := PLongWord(ValuePtr)^;
        end;
      stInteger:
        begin
          Result.VType := vtInteger;
          Result.VInteger := PInteger(ValuePtr)^;
        end;
      stULong:
        begin
          Result.VType := vtUInteger;
          Result.VUInteger := PUInt64(ValuePtr)^;
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
      stCurrency:
        begin
          Result.VType := vtFloat;
          Result.VFloat := PCurrency(ValuePtr)^;
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
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull)
    and (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
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
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull) then
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stAsciiStream, stBinaryStream, stUnicodeStream:
        SetBlobObject(FBuffer, ColumnIndex, nil);
      stBytes, stString, stUnicodeString:
        if PNativeUInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ > 0 then
        begin
          System.FreeMem(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^);
          PNativeUInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := 0;
        end;
    end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNull;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Ord(Value);
    stString, stUnicodeString:
       if Value then
          SetString(ColumnIndex, 'True')
       else
          SetString(ColumnIndex, 'False');
    else
      FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNull; //set null
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>shortint</code> value.
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>shortint</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetWord(Const ColumnIndex: Integer;
  const Value: Word);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>smallint</code> value.
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        SetRawByteString(ColumnIndex, IntToRaw(Value))
      else
        SetUnicodeString(ColumnIndex, IntToUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>smallint</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUInt(Const ColumnIndex: Integer;
  const Value: Cardinal);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
procedure TZRowAccessor.SetULong(Const ColumnIndex: Integer; const Value: UInt64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
        SetRawByteString(ColumnIndex, FloatToSQLRaw(Value))
      else
        SetUnicodeString(ColumnIndex, FloatToSQLUnicode(Value));
  end;
end;

{**
  Sets the designated column with a <code>currency</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetCurrency(Const ColumnIndex: Integer; const Value: Currency);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value <> 0;
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
    stString, stUnicodeString:
      if Self is TZRawRowAccessor then
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
  GUID: TGUID;
  TempBlob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Value, 0);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(PChar(Value), 0, Length(Value));
    //stString, stUnicodeString: do not handle here!
    stBytes: SetBytes(ColumnIndex, StrToBytes(Value));
    stGUID:
      if (Value = '') or (Length(Value) <> 38) then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID(Value);
        System.Move(GUID.D1, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF UNICODE}UnicodeSQLDateToDateTime{$ELSE}RawSQLDateToDateTime{$ENDIF}(
            PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed{%H-});
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            {$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
              PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        {$IFDEF UNICODE}UnicodeSQLTimeToDateTime{$ELSE}RawSQLTimeToDateTime{$ENDIF}(
          PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            Frac({$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
              PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF UNICODE}UnicodeSQLTimeStampToDateTime{$ELSE}RawSQLTimeStampToDateTime{$ENDIF}(
            PChar(Value), Length(Value), ConSettings^.DisplayFormatSettings, Failed);
    stAsciiStream, stUnicodeStream:
      begin
        TempBlob := GetBlob(ColumnIndex, IsNull{%H-});
        if TempBlob.IsClob then
          {$IF defined(DELPHI) or defined(UNICODE)}
          TempBlob.SetUnicodeString(Value)
          {$ELSE}
          TempBlob.SetUTF8String(ConSettings^.ConvFuncs.ZStringToUTF8(Value, ConSettings^.CTRL_CP))
          {$IFEND}
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
  @param Len the pointer to the length of the String in bytes
}
procedure TZRowAccessor.SetPAnsiChar(Const ColumnIndex: Integer; Value: PAnsichar;
  Len: PNativeUInt);
var
  IsNull: Boolean;
  GUID: TGUID;
  Blob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stFloat: SQLStrToFloatDef(Value, 0, PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Len^);
    stDouble: SQLStrToFloatDef(Value, 0, PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Len^);
    stCurrency: SQLStrToFloatDef(Value, 0, PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Len^);
    stBigDecimal: SQLStrToFloatDef(Value, 0, PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Len^);
    //stString, stUnicodeString: do not handle here!
    stBytes: SetBytes(ColumnIndex, Value, len^);
    stGUID:
      if Value = nil then
        SetNull(ColumnIndex)
      else
      begin
        {$IFDEF UNICODE}
        GUID := StringToGUID(ASCII7ToUnicodeString(Value, Len^));
        {$ELSE}
        GUID := StringToGUID(Value);
        {$ENDIF}
        System.Move(GUID.D1, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            RawSQLDateToDateTime (Value, Len^,
              ConSettings^.DisplayFormatSettings, Failed{%H-});
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            RawSQLTimeStampToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        RawSQLTimeToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            Frac(RawSQLTimeStampToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          RawSQLTimeStampToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed);
    stUnicodeStream, stAsciiStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull{%H-});
        if Blob.IsClob then
          Blob.SetPAnsiChar(Value, ConSettings^.ClientCodePage^.CP, Len^)
        else
          Blob.SetBuffer(Value, Len^);
      end;
    stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetBuffer(Value, Len^);
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
var Len: NativeUInt;
begin
  Len := ZFastCode.StrLen(Value);
  SetPAnsiChar(ColumnIndex, Value, @Len);
end;

{**
  Sets the designated column with a <code>PWideChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the pointer to the length of the string in codepoints
  @param Value the new column value
}
procedure TZRowAccessor.SetPWideChar(Const ColumnIndex: Integer;
  Value: PWideChar; Len: PNativeUInt);
var
  IsNull: Boolean;
  GUID: TGUID;
  Blob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stFloat: PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(Value, 0, Len^);
    stDouble: PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(Value, 0, Len^);
    stCurrency: PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(Value, 0, Len^);
    stBigDecimal: PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := SQLStrToFloatDef(Value, 0, Len^);
    //stUnicodeString, stString: do not handle here
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull{%H-});
        if Blob.IsClob then
          Blob.SetPWideChar(Value, Len^)
        else
          Blob.SetBuffer(Value, Len^*2);
      end;
    stBytes:
      SetBytes(ColumnIndex, StrToBytes(ZWideString(Value)));
    stGUID:
      if Value = nil  then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID({$IFDEF UNICODE}Value{$ELSE}UnicodeStringToASCII7(Value, Len^){$ENDIF});
        System.Move(GUID.D1, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          UnicodeSQLDateToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed{%H-});
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            UnicodeSQLTimeStampToDateTime(Value, Len^,
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        UnicodeSQLTimeToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            Frac(UnicodeSQLTimeStampToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        UnicodeSQLTimeStampToDateTime(Value, Len^, ConSettings^.DisplayFormatSettings, Failed);
    else
      SetString(ColumnIndex, ConSettings^.ConvFuncs.ZUnicodeToString(Value, ConSettings^.CTRL_CP));
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
  if Self is TZRawRowAccessor then
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
  if Self is TZRawRowAccessor then
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
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToIntDef(Value, 0);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := RawToInt64Def(Value, 0);
    stFloat: SQLStrToFloatDef(PAnsiChar(Pointer(Value)), 0, PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stDouble: SQLStrToFloatDef(PAnsiChar(Pointer(Value)), 0, PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stCurrency: SQLStrToFloatDef(PAnsiChar(Pointer(Value)), 0, PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stBigDecimal: SQLStrToFloatDef(PAnsiChar(Pointer(Value)), 0, PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    //stString, stUnicodeString: do not handle here!
    stBytes: SetBytes(ColumnIndex, StrToBytes(Value));
    stGUID:
      if Value = '' then
        SetNull(ColumnIndex)
      else
      begin
        GUID := StringToGUID({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(Value));
        System.Move(GUID.D1, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            RawSQLDateToDateTime (Pointer(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed{%H-});
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            RawSQLTimeStampToDateTime(Pointer(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        RawSQLTimeToDateTime(Pointer(Value), Length(Value),
          ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            Frac(RawSQLTimeStampToDateTime(Pointer(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          RawSQLTimeStampToDateTime(Pointer(Value), Length(Value),
            ConSettings^.DisplayFormatSettings, Failed);
    stUnicodeStream, stAsciiStream, stBinaryStream:
      GetBlob(ColumnIndex, IsNull{%H-}).SetString(Value);
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
  Blob: IZBlob;
  Failed: Boolean;
begin
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := StrToBoolEx(Value, False);
    stByte: PByte(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stShort: PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stWord: PWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stSmall: PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stLongWord: PCardinal(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stInteger: PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToIntDef(Value, 0);
    stULong: PUInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stLong: PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := UnicodeToInt64Def(Value, 0);
    stFloat: SQLStrToFloatDef(PWideChar(Pointer(Value)), 0, PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stDouble: SQLStrToFloatDef(PWideChar(Pointer(Value)), 0, PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stCurrency: SQLStrToFloatDef(PWideChar(Pointer(Value)), 0, PCurrency(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    stBigDecimal: SQLStrToFloatDef(PWideChar(Pointer(Value)), 0, PExtended(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^, Length(Value));
    //stUnicodeString, stString: do not handle here
    stAsciiStream, stUnicodeStream:
      begin
        Blob := GetBlob(ColumnIndex, IsNull{%H-});
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
        GUID := StringToGUID({$IFNDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Value));
        System.Move(GUID.D1, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      end;
    stDate:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          UnicodeSQLDateToDateTime(PWideChar(Value), Length(Value),
            ConSettings^.DisplayFormatSettings, Failed{%H-});
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
          {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
            UnicodeSQLTimeStampToDateTime(PWideChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTime:
      begin
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
        UnicodeSQLTimeToDateTime(PWideChar(Value), Length(Value),
          ConSettings^.DisplayFormatSettings, Failed);
        if Failed then
          PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
            Frac(UnicodeSQLTimeStampToDateTime(PWideChar(Value), Length(Value),
              ConSettings^.DisplayFormatSettings, Failed));
      end;
    stTimestamp:
      PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
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
procedure TZRowAccessor.SetBytes(Const ColumnIndex: Integer; const Value: TBytes);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if Value <> nil then
  begin
    FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stGUID: System.Move(Value[0], FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      stBytes: InternalSetBytes(FBuffer, ColumnIndex, Value);
      stBinaryStream: GetBlob(ColumnIndex, IsNull{%H-}).SetBytes(Value);
      else
        SetString(ColumnIndex, String(BytesToStr(Value)));
    end;
  end
  else
    SetNull(ColumnIndex);
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
procedure TZRowAccessor.SetBytes(Const ColumnIndex: Integer; Buf: Pointer; Len: Word);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if (Buf <> nil) and (Len > 0) then
  begin
    FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stGUID: System.Move(Buf^, FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1], 16);
      stBytes: InternalSetBytes(FBuffer, ColumnIndex, Buf, Len);
      stBinaryStream: GetBlob(ColumnIndex, IsNull{%H-}).SetBuffer(Buf, Len);
      else
        raise EZSQLException.Create(cSConvertionIsNotPossible);
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stDate:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stTime:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ :=
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stDate: SetDate(ColumnIndex, Value);
    stTime: SetTime(ColumnIndex, Value);
    stTimestamp:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
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
  if not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
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
  if not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stDataSet) then
  begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
{$ENDIF}

  Ptr := PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]]);

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
var
  Len: NativeUInt;
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
    vtCharRec:
      if ZCompatibleCodePages(zCP_UTF16, Value.VCharRec.CP) then
      begin
        Len := Value.VCharRec.Len;
        SetPWideChar(ColumnIndex, Value.VCharRec.P, @Len);
      end
      else
      begin
        if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, Value.VCharRec.CP) then
        begin
          Len := Value.VCharRec.Len;
          SetPAnsiChar(ColumnIndex, Value.VCharRec.P, @Len)
        end
        else
          SetUnicodeString(ColumnIndex, PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
      end;
  end;
end;

{ TZRawRowAccessor }

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRawRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer; const CloneLobs: Boolean = False);
var
  I: Integer;
  Blob: IZBlob;
begin
  ClearBuffer(DestBuffer, False);
  DestBuffer^.Index := SrcBuffer^.Index;
  DestBuffer^.UpdateType := SrcBuffer^.UpdateType;
  DestBuffer^.BookmarkFlag := SrcBuffer^.BookmarkFlag;
  System.Move(SrcBuffer^.Columns, DestBuffer^.Columns, FColumnsSize);
  if FHasBytes then
    for i := 0 to FHighBytesCols do
      if SrcBuffer^.Columns[FColumnOffsets[FBytesCols[i]]] = bIsNotNull then
        InternalSetBytes(DestBuffer, FBytesCols[i] {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          PPointer(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1])^,
          PWord(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1+SizeOf(Pointer)])^, True);
  if FHasStrings then
    for i := 0 to FHighStringCols do
      if SrcBuffer^.Columns[FColumnOffsets[FStringCols[i]]] = bIsNotNull then
        InternalSetPAnsiChar(DestBuffer, FStringCols[i]{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^+PAnsiInc,
          PPLongWord(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^^, True);
  if FHasArrays then
    for I := 0 to FHighArrayCols - 1 do
      ; //currently NOT implemented
  if FHasLobs then
    for i := 0 to FHighLobCols do
      if (SrcBuffer^.Columns[FColumnOffsets[FLobCols[i]]] = bIsNotNull) then
      begin
        DestBuffer^.Columns[FColumnOffsets[FLobCols[i]]] := bIsNull; //init Destbuffer flag to avoid IZLob() := nil;
        Blob := GetBlobObject(SrcBuffer, FLobCols[i] {$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        if CloneLobs and (Blob <> nil) then
          Blob := Blob.Clone;
        SetBlobObject(DestBuffer, FLobCols[i] {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Blob); //this line increments RefCount but doesn't move data
      end;
  if FHasDataSets then
    for i := 0 to FHighDataSetCols do
      ; //currently NOT implemented
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean;
  out Len: NativeUInt): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          Result := PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc;
          Len := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
        end;
      else
        Result := inherited GetPAnsiChar(ColumnIndex, IsNull, Len);
    end;
    IsNull := False;
  end
  else
  begin
    Len := 0;
    Result := nil;
    IsNull := True;
  end;
end;

function TZRawRowAccessor.GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec;
var Len: NativeUint;
begin
  Result.P := GetPAnsiChar(ColumnIndex, IsNull, Len);
  Result.Len := Len;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        {$IFDEF UNICODE}
        begin
          Result := PRawToUnicode(
            PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
            ConSettings^.ClientCodePage^.CP);
        end;
        {$ELSE}
        if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) or not ConSettings^.AutoEncode then
          System.SetString(Result, PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        else
          Result := ConSettings^.ConvFuncs.ZRawToString(
            PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          if ZCompatibleCodePages(ZOSCodePage, ConSettings^.ClientCodePage^.CP) then
            System.SetString(Result, PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
              PLongWord(PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^)^)
          else
          begin
            FUniTemp := PRawToUnicode(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
              PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
              ConSettings^.ClientCodePage^.CP);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        if ZCompatibleCodePages(zCP_UTF8, ConSettings^.ClientCodePage^.CP) then
        {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
        begin
          SetLength(Result, PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
          System.Move((PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc)^,
            Pointer(Result)^, PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
        end
        {$ELSE}
          System.SetString(Result, PAnsiChar(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc),
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        {$ENDIF}
        else
        begin
          FUniTemp := PRawToUnicode(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
              ConSettings^.ClientCodePage^.CP); //localize the vals to avoid buffer overrun for WideStrings
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
        ZSetString(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^, Result);
        {$ELSE}
        System.SetString(Result, PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
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
  @param Len the length of the string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRawRowAccessor.GetPWideChar(Const ColumnIndex: Integer;
  var IsNull: Boolean; out Len: NativeUInt): PWideChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stUnicodeString, stString:
        begin
          if PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^ = 0 then //avoid all conversions
          begin
            Len := 0;
            Result := PEmptyUnicodeString;
          end
          else
          begin
            FUniTemp := PRawToUnicode(
              PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
              PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
              ConSettings^.ClientCodePage^.CP);
            Result := Pointer(FUniTemp);
            {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
            Len := {%H-}PLengthInt(NativeUInt(FUniTemp) - StringLenOffSet)^;
            {$ELSE}
            Len := Length(FUniTemp);
            {$ENDIF}
          end;
        end
      else
        Result := inherited GetPWideChar(ColumnIndex, IsNull, Len);
    end;
    IsNull := False;
  end
  else
  begin
    Result := nil;
    Len := 0;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stUnicodeString, stString:
        begin
          Result := PRawToUnicode(PPAnsiChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PAnsiInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
            ConSettings^.ClientCodePage^.CP);
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString, stUnicodeString:
      begin
        InternalSetString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
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
  @param Len pointer to the Length of the new column value in bytes
}
procedure TZRawRowAccessor.SetPAnsiChar(Const ColumnIndex: Integer;
  Value: PAnsiChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          InternalSetPAnsiChar(FBuffer, ColumnIndex, Value, Len^);
          FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        end;
      else inherited SetPAnsiChar(ColumnIndex, Value, Len)
    end;
end;

{**
  Sets the designated column with a <code>PWideChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the pointer to the length of the string in codepoints
  @param x the new column value
}
procedure TZRawRowAccessor.SetPWideChar(Const ColumnIndex: Integer;
  Value: PWideChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          InternalSetString(FBuffer, ColumnIndex, PUnicodeToRaw(Value, Len^, ConSettings^.ClientCodePage^.CP));
          FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        end;
      else inherited SetPWideChar(ColumnIndex, Value, Len)
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString, stUnicodeString:
      begin
        InternalSetString(FBuffer, ColumnIndex, Value);
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stUnicodeString, stString:
      begin
        InternalSetString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
      end;
    else inherited SetUnicodeString(ColumnIndex, Value);
  end;
end;

{ TZUnicodeRowAccessor }

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZUnicodeRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer;
  const CloneLobs: Boolean = False);
var
  I: Integer;
  Blob: IZBlob;
begin
  ClearBuffer(DestBuffer, False);
  DestBuffer^.Index := SrcBuffer^.Index;
  DestBuffer^.UpdateType := SrcBuffer^.UpdateType;
  DestBuffer^.BookmarkFlag := SrcBuffer^.BookmarkFlag;
  System.Move(SrcBuffer^.Columns, DestBuffer^.Columns, FColumnsSize);
  if FHasBytes then
    for i := 0 to FHighBytesCols do
      if SrcBuffer^.Columns[FColumnOffsets[FBytesCols[i]]] = bIsNotNull then
        InternalSetBytes(DestBuffer, FBytesCols[i] {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          PPointer(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1])^,
          PWord(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1+SizeOf(Pointer)])^, True);
  if FHasStrings then
    for i := 0 to FHighStringCols do
      if SrcBuffer^.Columns[FColumnOffsets[FStringCols[i]]] = bIsNotNull then
        InternalSetPWideChar(DestBuffer, FStringCols[i]{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          ZPPWideChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^+PWideInc,
          PPLongWord(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^^, True);
  if FHasArrays then
    for I := 0 to FHighArrayCols - 1 do
      ; //currently NOT implemented
  if FHasLobs then
    for i := 0 to FHighLobCols do
      if (SrcBuffer^.Columns[FColumnOffsets[FLobCols[i]]] = bIsNotNull) then
      begin
        DestBuffer^.Columns[FColumnOffsets[FLobCols[i]]] := bIsNull; //init Destbuffer flag to avoid IZLob() := nil;
        Blob := GetBlobObject(SrcBuffer, FLobCols[i] {$IFNDEF GENERIC_INDEX}+ 1{$ENDIF});
        if CloneLobs and (Blob <> nil) then
          Blob := Blob.Clone;
        SetBlobObject(DestBuffer, FLobCols[i] {$IFNDEF GENERIC_INDEX}+ 1{$ENDIF}, Blob); //this line increments RefCount but doesn't move data
      end;
  if FHasDataSets then
    for i := 0 to FHighDataSetCols do
      ; //currently NOT implemented
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetPAnsiChar(Const ColumnIndex: Integer; var IsNull: Boolean;
  out Len: NativeUInt): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          if PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^ = 0 then //avoid all conversions
          begin
            Len := 0;
            Result := PEmptyAnsiString;
          end
          else
          begin
            FRawTemp := PUnicodeToRaw(
              ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
              PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
              ConSettings^.ClientCodePage^.CP);
            Len := {%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^;
            Result := Pointer(FRawTemp);
          end;
        end
      else
        Result := Inherited GetPAnsiChar(ColumnIndex, IsNull, Len);
    end;
    IsNull := False;
  end
  else
  begin
    Result := nil;
    Len := 0;
    IsNull := True;
  end;
end;

function TZUnicodeRowAccessor.GetCharRec(Const ColumnIndex: Integer; var IsNull: Boolean): TZCharRec;
var Len: NativeUInt;
begin
  Result.P := GetPWideChar(ColumnIndex, IsNull, Len);
  Result.Len := Len;
  Result.CP := zCP_UTF16;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
                          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^)
        {$ELSE}
        if ConSettings^.AutoEncode then
          Result := PUnicodeToString(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^, ConSettings^.CTRL_CP)
        else
          Result := PUnicodeToString(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^, ConSettings^.ClientCodePage^.CP)
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        Result := PUnicodeToRaw(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^, ZOSCodePage);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        Result := PUnicodeToRaw(ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^, zCP_UTF8);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
          Result := PUnicodeToRaw(
            ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
            PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^,
            ConSettings^.ClientCodePage^.CP);
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
  @param Len the length of the string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZUnicodeRowAccessor.GetPWideChar(Const ColumnIndex: Integer;
  var IsNull: Boolean; out Len: NativeUInt): PWideChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stUnicodeString, stString:
        begin
          Result := ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc;
          Len := PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^;
        end;
      else
        Result := inherited GetPWideChar(ColumnIndex, IsNull, Len);
    end;
    IsNull := False;
  end
  else
  begin
    Result := nil;
    Len := 0;
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
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stUnicodeString, stString:
        System.SetString(Result, ZPPWideChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^+PWideInc,
          PPLongWord(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^^);
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString, stUnicodeString:
      begin
        {$IFDEF UNICODE}
        InternalSetUnicodeString(FBuffer, ColumnIndex, Value);
        {$ELSE}
        InternalSetUnicodeString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZStringToUnicode(Value, ConSettings^.CTRL_CP));
        {$ENDIF}
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
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
procedure TZUnicodeRowAccessor.SetPAnsiChar(Const ColumnIndex: Integer;
  Value: PAnsiChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          InternalSetUnicodeString(FBuffer, ColumnIndex, PRawToUnicode(Value,
            Len^, ConSettings^.ClientCodePage^.CP));
          FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        end;
      else inherited SetPAnsiChar(ColumnIndex, Value, Len)
    end;
end;

{**
  Sets the designated column with a <code>PWideChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param len the pointer to the length of the string in codepoints
  @param Value the new column value
}
procedure TZUnicodeRowAccessor.SetPWideChar(Const ColumnIndex: Integer;
  Value: PWideChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if Value = nil then
    SetNull(ColumnIndex)
  else
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString:
        begin
          InternalSetPWideChar(FBuffer, ColumnIndex, Value, Len^);
          FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        end;
      else inherited SetPWideChar(ColumnIndex, Value, Len)
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString, stUnicodeString:
      begin
        InternalSetUnicodeString(FBuffer, ColumnIndex, ConSettings^.ConvFuncs.ZRawToUnicode(Value, ConSettings^.ClientCodePage^.CP));
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
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
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stUnicodeString, stString:
      InternalSetUnicodeString(FBuffer, ColumnIndex, Value);
    else inherited SetUnicodeString(ColumnIndex, Value);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
end;

end.



