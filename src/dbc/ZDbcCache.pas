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
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}FmtBcd,
  {$IF defined(OLDFPC) or defined(NO_UNIT_CONTNRS)}ZClasses,{$IFEND} ZDbcIntfs,
  ZDbcResultSet, ZDbcResultSetMetadata, ZVariant, ZCompatibility;

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
    FRaw: Boolean;
    FClientCP: Word;

    {store columnswhere mem-deallocation/copy needs an extra sequence of code}
    FHighBytesCols, FHighStringCols, FHighArrayCols, FHighLobCols, FHighDataSetCols: Integer;
    FBytesCols, FStringCols, FArrayCols, FLobCols, FDataSetCols: array of Integer;
    FConSettings: PZConSettings;
    function GetColumnSize(ColumnInfo: TZColumnInfo): Integer;
    function InternalGetInt(ColumnIndex: Integer; out IsNull: Boolean): Integer; {$IFDEF WITHINLINE} inline; {$ENDIF}
    function InternalGetULong(ColumnIndex: Integer; out IsNull: Boolean): UInt64; {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetInt(ColumnIndex: Integer; Value: Integer); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetULong(ColumnIndex: Integer; const Value: UInt64); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetBytes(BuffAddr: PPointer; Buf: Pointer; Len: Word); overload; {$IFDEF WITHINLINE} inline; {$ENDIF}
   procedure InternalSetString(BuffAddr: PPointer;
      const Value: RawByteString); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPAnsiChar(BuffAddr: PPointer;
      Value: PAnsiChar; Len: Cardinal); {$IFDEF WITHINLINE} inline; {$ENDIF}
    procedure InternalSetPWideChar(BuffAddr: PPointer; Value: PWideChar;
      Len: Cardinal); {$IFDEF WITHINLINE} inline; {$ENDIF}
  protected
    procedure CheckColumnIndex(ColumnIndex: Integer);
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);
  public
    TinyBuffer: array[Byte] of Byte;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings);

    function AllocBuffer: PZRowBuffer;
    procedure InitBuffer(Buffer: PZRowBuffer);
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer; const CloneLobs: Boolean = False);
    procedure MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure CloneBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure ClearBuffer(Buffer: PZRowBuffer; const WithFillChar: Boolean = True);
    procedure DisposeBuffer(Buffer: PZRowBuffer);

    function CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
      const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
    function CompareBuffer(Buffer1, Buffer2: PZRowBuffer;
      ColumnIndex: Integer; CompareFunc: TCompareFunc): Integer;
    function GetCompareFunc(ColumnIndex: Integer; const CompareKind: TComparisonKind): TCompareFunc;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs;

    procedure Alloc;
    procedure Init;
    procedure CopyTo(DestBuffer: PZRowBuffer);
    procedure CopyFrom(SrcBuffer: PZRowBuffer);
    procedure MoveTo(DestBuffer: PZRowBuffer);
    procedure MoveFrom(SrcBuffer: PZRowBuffer);
    procedure CloneTo(DestBuffer: PZRowBuffer);
    procedure CloneFrom(SrcBuffer: PZRowBuffer);
    procedure Clear;
    procedure Dispose;

    function GetColumnData(ColumnIndex: Integer; out IsNull: Boolean): Pointer;
    function GetColumnDataSize(ColumnIndex: Integer): Integer;

    function GetColumnName(ColumnIndex: Integer): string;
    function GetColumnCase(ColumnIndex: Integer): Boolean;
    function GetColumnType(ColumnIndex: Integer): TZSQLType;
    function GetColumnLength(ColumnIndex: Integer): Integer;
    function GetColumnOffSet(ColumnIndex: Integer): Integer;
    function GetColumnDefaultExpression(ColumnIndex: Integer): string;
    procedure SetColumnDefaultExpression(ColumnIndex: Integer; const Value: string);
    procedure SetColumnCodePage(ColumnIndex: Integer; const Value: Word);

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out IsNull: Boolean; out Len: NativeUInt): PAnsiChar;
    function GetCharRec(ColumnIndex: Integer; out IsNull: Boolean): TZCharRec;
    function GetString(ColumnIndex: Integer; out IsNull: Boolean): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer; out IsNull: Boolean): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer; out IsNull: Boolean): UTF8String;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer; out IsNull: Boolean): RawByteString;
    function GetPWideChar(ColumnIndex: Integer; out IsNull: Boolean; out Len: NativeUInt): PWideChar;
    function GetUnicodeString(ColumnIndex: Integer; out IsNull: Boolean): ZWideString;
    function GetBoolean(ColumnIndex: Integer; out IsNull: Boolean): Boolean;
    function GetByte(ColumnIndex: Integer; out IsNull: Boolean): Byte;
    function GetShort(ColumnIndex: Integer; out IsNull: Boolean): ShortInt;
    function GetWord(ColumnIndex: Integer; out IsNull: Boolean): Word;
    function GetSmall(ColumnIndex: Integer; out IsNull: Boolean): SmallInt;
    function GetUInt(ColumnIndex: Integer; out IsNull: Boolean): Cardinal;
    function GetInt(ColumnIndex: Integer; out IsNull: Boolean): Integer;
    function GetULong(ColumnIndex: Integer; out IsNull: Boolean): UInt64;
    function GetLong(ColumnIndex: Integer; out IsNull: Boolean): Int64;
    function GetFloat(ColumnIndex: Integer; out IsNull: Boolean): Single;
    function GetDouble(ColumnIndex: Integer; out IsNull: Boolean): Double;
    function GetCurrency(ColumnIndex: Integer; out IsNull: Boolean): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD; out IsNull: Boolean);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID; out IsNull: Boolean);
    function GetBytes(ColumnIndex: Integer; out IsNull: Boolean): TBytes; overload;
    function GetBytes(ColumnIndex: Integer; out IsNull: Boolean; out Len: Word): Pointer; overload;
    procedure GetDate(ColumnIndex: Integer; out IsNull: Boolean; Var Result: TZDate);
    procedure GetTime(ColumnIndex: Integer; out IsNull: Boolean; Var Result: TZTime);
    procedure GetTimestamp(ColumnIndex: Integer; out IsNull: Boolean; var Result: TZTimeStamp);
    function GetAsciiStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
    function GetUnicodeStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
    function GetBinaryStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
    function GetBlob(ColumnIndex: Integer; out IsNull: Boolean): IZBlob;
    function GetDataSet(ColumnIndex: Integer; out IsNull: Boolean): IZDataSet;
    function GetValue(ColumnIndex: Integer): TZVariant;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetNotNull(ColumnIndex: Integer);
    procedure SetNull(ColumnIndex: Integer);
    procedure SetBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure SetByte(ColumnIndex: Integer; Value: Byte);
    procedure SetShort(ColumnIndex: Integer; Value: ShortInt);
    procedure SetWord(ColumnIndex: Integer; Value: Word);
    procedure SetSmall(ColumnIndex: Integer; Value: SmallInt);
    procedure SetUInt(ColumnIndex: Integer; Value: Cardinal);
    procedure SetInt(ColumnIndex: Integer; Value: Integer);
    procedure SetULong(ColumnIndex: Integer; const Value: UInt64);
    procedure SetLong(ColumnIndex: Integer; const Value: Int64);
    procedure SetFloat(ColumnIndex: Integer; Value: Single);
    procedure SetDouble(ColumnIndex: Integer; const Value: Double);
    procedure SetCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(ColumnIndex: Integer; const Value: TBCD);
    procedure SetGUID(ColumnIndex: Integer; const Value: TGUID);
    procedure SetString(ColumnIndex: Integer; const Value: String);
    procedure SetPAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; var Len: NativeUInt);
    procedure SetPWideChar(ColumnIndex: Integer; Value: PWideChar; var Len: NativeUInt);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ColumnIndex: Integer; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ColumnIndex: Integer; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure SetRawByteString(ColumnIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(ColumnIndex: Integer; const Value: ZWideString); virtual;
    procedure SetBytes(ColumnIndex: Integer; const Value: TBytes); overload; virtual;
    procedure SetBytes(ColumnIndex: Integer; Buf: Pointer; Len: Word); overload; virtual;
    procedure SetDate(ColumnIndex: Integer; const Value: TZDate); virtual;
    procedure SetTime(ColumnIndex: Integer; const Value: TZTime); virtual;
    procedure SetTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp); virtual;
    procedure SetAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure SetUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    procedure SetBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure SetBlob(ColumnIndex: Integer; const Value: IZBlob);
    procedure SetDataSet(ColumnIndex: Integer; const Value: IZDataSet);
    procedure SetValue(ColumnIndex: Integer; const Value: TZVariant);

    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}

    property ColumnsSize: Integer read FColumnsSize;
    property RowSize: Integer read FRowSize;
    property RowBuffer: PZRowBuffer read FBuffer write FBuffer;
    property ConSettings: PZConSettings read FConSettings;
    property IsRaw: Boolean read fRaw;
  end;

const
  RowHeaderSize = SizeOf(TZRowBuffer) - SizeOf(TZByteArray);
  {EH: we revert the normal Boolean anlogy. We Use True = 0 and False = 1! Beacuse:
    This avoids an extra Setting of Null bytes after calling FillChar(x,y,#0)}
  bIsNull = Byte(0);
  bIsNotNull = Byte(1);

implementation

uses ZFastcode, Math, ZMessages, ZSysUtils, ZDbcUtils, ZEncoding
  {$IF not defined(OLDFPC) and not defined(NO_UNIT_CONTNRS)}, ZClasses{$IFEND}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

const
  PAnsiInc = SizeOf(Cardinal);
  PWideInc = SizeOf(Word); //PWide inc assumes allways two byte
  BothNotNull = Low(Integer);
  // Results of Asc comparation of Null1, Null2 flags.
  // If both flags are False, comparation of values is required
  NullsCompareMatrix: array[Boolean] of array[Boolean] of Integer =
    (
      (BothNotNull, 1),
      (-1, 0)
    );
  // Results of equality comparation of Null1, Null2 flags.
  // If both flags are False, comparation of values is required
  NullsEqualMatrix: array[Boolean] of array[Boolean] of Integer =
    (
      (BothNotNull, bIsNotNull),
      (bIsNotNull, bIsNull)
    );

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // empty function - parameter not used intentionally
function CompareNothing(const Null1, Null2: Boolean; const V1, V2): Integer; //emergency exit for types we can't sort like arrays, dataset ...
begin
  Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function CompareBoolean_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PWordBool(V1)^) - Ord(PWordBool(V2)^); //overflow safe
end;

function CompareBoolean_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBoolean_Asc(Null1, Null2, V1, V2);
end;

function CompareBoolean_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PWordBool(V1)^ <> PWordBool(V2)^);
end;

function CompareByte_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := PByte(V1)^ - PByte(V2)^; //overflow safe
end;

function CompareByte_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareByte_Asc(Null1, Null2, V1, V2);
end;

function CompareByte_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PByte(V1)^ <> PByte(V2)^);
end;

function CompareShort_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := PShortInt(V1)^ - PShortInt(V2)^; //overflow safe
end;

function CompareShort_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareShort_Asc(Null1, Null2, V1, V2);
end;

function CompareShort_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PShortInt(V1)^ <> PShortInt(V2)^);
end;

function CompareWord_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := PWord(V1)^ - PWord(V2)^; //overflow safe
end;

function CompareWord_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareWord_Asc(Null1, Null2, V1, V2);
end;

function CompareWord_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PWord(V1)^ <> PWord(V2)^);
end;

function CompareSmallInt_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := PSmallInt(V1)^ - PSmallInt(V2)^; //overflow safe
end;

function CompareSmallInt_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareSmallInt_Asc(Null1, Null2, V1, V2);
end;

function CompareSmallInt_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PSmallInt(V1)^ <> PSmallInt(V2)^);
end;

function CompareLongWord_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PLongWord(V1)^ > PLongWord(V2)^)-Ord(PLongWord(V1)^ < PLongWord(V2)^);
end;

function CompareLongWord_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareLongWord_Asc(Null1, Null2, V1, V2);
end;

function CompareLongWord_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PLongWord(V1)^ <> PLongWord(V2)^);
end;

function CompareInteger_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  //function ShaCompareInt(Item1, Item2: Pointer): Integer;
  begin //on 100 mio execs 200ms faster
    Result := PInteger(V1)^;
    if Result xor PInteger(V2)^>=0
      then Result:=Result-PInteger(V2)^
      else Result:=Result or 1;
  end; //Than My (EH) overflow save idea
  //Result := Ord(PInteger(V1)^ > PInteger(V2)^)-Ord(PInteger(V1)^ < PInteger(V2)^);
end;

function CompareInteger_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInteger_Asc(Null1, Null2, V1, V2);
end;

function CompareInteger_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PInteger(V1)^ <> PInteger(V2)^);
end;

function CompareInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PInt64(V1)^ > PInt64(V2)^)-Ord(PInt64(V1)^ < PInt64(V2)^);
end;

function CompareInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareInt64_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PInt64(V1)^ <> PInt64(V2)^);
end;

function CompareUInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PUInt64(V1)^ > PUInt64(V2)^)-Ord(PUInt64(V1)^ < PUInt64(V2)^);
end;

function CompareUInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareUInt64_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PUInt64(V1)^ <> PUInt64(V2)^);
end;

function CompareSingle_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PSingle(V1)^ > PSingle(V2)^)-Ord(PSingle(V1)^ < PSingle(V2)^);
end;

function CompareSingle_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareSingle_Asc(Null1, Null2, V1, V2);
end;

function CompareSingle_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PSingle(V1)^ <> PSingle(V2)^);
end;

function CompareDouble_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PDouble(V1)^ > PDouble(V2)^)-Ord(PDouble(V1)^ < PDouble(V2)^);
end;

function CompareDouble_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDouble_Asc(Null1, Null2, V1, V2);
end;

function CompareDouble_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PDouble(V1)^ <> PDouble(V2)^);
end;

function CompareCurrency_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PCurrency(V1)^ > PCurrency(V2)^)-Ord(PCurrency(V1)^ < PCurrency(V2)^);
end;

function CompareCurrency_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDouble_Asc(Null1, Null2, V1, V2);
end;

function CompareCurrency_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PCurrency(V1)^ <> PCurrency(V2)^);
end;

function CompareBigDecimal_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := BCDCompare(PBCD(V1)^, PBCD(V2)^);
end;

function CompareBigDecimal_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBigDecimal_Asc(Null1, Null2, V1, V2);
end;

function CompareBigDecimal_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := BCDCompare(PBCD(V1)^, PBCD(V2)^);
end;

function CompareDateTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZSysUtils.ZCompareDateTime(PDateTime(V1)^, PDateTime(V2)^)
end;

function CompareDateTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDateTime_Asc(Null1, Null2, V1, V2);
end;

function CompareDateTime_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZSysUtils.ZCompareDateTime(PDateTime(V1)^, PDateTime(V2)^)
end;

function CompareZDate_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := Ord(PInt64(V1)^ <> PInt64(V2)^)
end;

function CompareZDate_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZCompareDate(PZDate(V1)^, PZDate(V2)^)
end;

function CompareZDate_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := -ZCompareDate(PZDate(V1)^, PZDate(V2)^)
end;

function CompareZTime_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then begin
    Result := Ord(PCardinal(V1)^ <> PCardinal(V2)^);
    if Result = 0 then
        Result := Ord(PInt64(PAnsiChar(V1)+2)^ <> PInt64(PAnsiChar(V2)+2)^);
  end;
end;

function CompareZTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZCompareTime(PZTime(V1)^, PZTime(V2)^)
end;

function CompareZTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := -ZCompareTime(PZTime(V1)^, PZTime(V2)^)
end;

function CompareZTimeStamp_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then begin
    Result := Ord(PInt64(V1)^ <> PInt64(V2)^);
    if Result = 0 then begin
      Result := Ord(PInt64(PAnsiChar(V1)+8)^ <> PInt64(PAnsiChar(V2)+8)^);
      if Result = 0 then
        Result := Ord(PInt64(PAnsiChar(V1)+14)^ <> PInt64(PAnsiChar(V2)+14)^);
    end;
  end;
end;

function CompareZTimeStamp_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZCompareTimeStamp(PZTimeStamp(V1)^, PZTimeStamp(V2)^)
end;

function CompareZTimeStamp_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := -ZCompareTimeStamp(PZTimeStamp(V1)^, PZTimeStamp(V2)^)
end;

function CompareGUID_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZMemLComp(Pointer(V1), Pointer(V2), 16); //Not a endversion! It would be nice to compare field-by-field of TGUID
end;

function CompareGUID_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareGUID_Asc(Null1, Null2, V1, V2);
end;

function CompareGUID_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
    Result := ZMemLComp(Pointer(V1), Pointer(V2), 16);
end;

function CompareBytes_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    Result := NullsEqualMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
    if PWord(PAnsiChar(V1)+SizeOf(Pointer))^ <> PWord(PAnsiChar(V1)+SizeOf(Pointer))^ then Result := 1//length different?
    else Result := ZMemLComp(PPointer(V1)^, PPointer(V2)^, PWord(PAnsiChar(V1)+SizeOf(Pointer))^)
  end;
end;

function CompareRaw_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    Result := NullsEqualMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
    if PLongWord(Pointer(V1)^)^ <> PLongWord(Pointer(V2)^)^ then Result := 1//length different?
    else Result := ZMemLComp(PAnsiChar(Pointer(V1)^)+PAnsiInc,
                         PAnsiChar(Pointer(V2)^)+PAnsiInc,
                         PLongWord(Pointer(V1)^)^);
  end;
end;

{$IFNDEF WITH_USC2_ANSICOMPARESTR_ONLY}
function CompareNativeRaw_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    Result := NullsCompareMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
    {$IFDEF MSWINDOWS}
    Result := CompareStringA(LOCALE_USER_DEFAULT, 0,
      PAnsiChar(Pointer(V1)^)+PAnsiInc, PLongWord(Pointer(V1)^)^,
      PAnsiChar(Pointer(V2)^)+PAnsiInc, PLongWord(Pointer(V2)^)^) - 2;{CSTR_EQUAL}
    {$ELSE}
    Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}
      AnsiStrComp(PPAnsiChar(V1)^+PAnsiInc, PPAnsiChar(V2)^+PAnsiInc)
    {$ENDIF}
  end;
end;

function CompareNativeRaw_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareNativeRaw_Asc(Null1, Null2, V1, V2);
end;
{$ENDIF}

function CompareUnicodeFromUTF8_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  S1, S2: ZWideString;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    Result := NullsCompareMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
    S1 := PRawToUnicode(PAnsiChar(Pointer(V1)^)+PAnsiInc, PLongWord(Pointer(V1)^)^, zCP_UTF8);
    S2 := PRawToUnicode(PAnsiChar(Pointer(V2)^)+PAnsiInc, PLongWord(Pointer(V2)^)^, zCP_UTF8);
    {$IFDEF UNICODE}
    Result := AnsiCompareStr(S1, S2);
    {$ELSE}
    Result := WideCompareStr(S1, S2);
    {$ENDIF}
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
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    Result := NullsCompareMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
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
    {$IFDEF UNICODE}
    Result := AnsiCompareStr(S1, S2);
    {$ELSE}
    Result := WideCompareStr(S1, S2);
    {$ENDIF}
    {$ENDIF}
  end;
end;

function CompareUnicode_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicode_Asc(Null1, Null2, V1, V2);
end;

function CompareUnicode_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Result := NullsEqualMatrix[(PPointer(V1)^ = nil), (PPointer(V2)^ = nil)];
    if Result <> BothNotNull then Exit;
    Result := Ord(PLongWord(Pointer(V1)^)^ <> PLongWord(Pointer(V2)^)^);
    if Result = 0 then
       Result := ZMemLComp(Pointer(PWideChar(Pointer(V1)^)+PWideInc),
                           Pointer(PWideChar(Pointer(V2)^)+PWideInc),
                           PLongWord(Pointer(V1)^)^ shl 1);
  end;
end;

function CompareNativeCLob_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Blob1 := IZBlob(PPointer(V1)^);
    Blob2 := IZBlob(PPointer(V2)^);
    Result := NullsCompareMatrix[(Blob1 = nil) or (Blob1.IsEmpty), (Blob2 = nil) or (Blob2.IsEmpty)];
    if Result <> BothNotNull then Exit;
    {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
    Result := AnsiCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
    {$ELSE}
    Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString);
    {$ENDIF}
  end;
end;

function CompareNativeCLob_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareNativeCLob_Asc(Null1, Null2, V1, V2);
end;

function CompareNativeCLob_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Blob1 := IZBlob(PPointer(V1)^);
    Blob2 := IZBlob(PPointer(V2)^);
    Result := NullsEqualMatrix[(Blob1 = nil) or (Blob1.IsEmpty), (Blob2 = nil) or (Blob2.IsEmpty)];
    if Result <> BothNotNull then Exit;
    if Blob1.IsUpdated or Blob2.IsUpdated then
      {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
      Result := AnsiCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString)
      {$ELSE}
      Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Blob1.GetString, Blob2.GetString)
      {$ENDIF}
    else Result := 0;
  end;
end;

function CompareUnicodeCLob_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
  {$IFDEF MSWINDOWS}
  ValuePtr1, ValuePtr2: Pointer;
  {$ENDIF}
begin
  Result := NullsCompareMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Blob1 := IZBlob(PPointer(V1)^);
    Blob2 := IZBlob(PPointer(V2)^);
    Result := NullsCompareMatrix[(Blob1 = nil) or (Blob1.IsEmpty), (Blob2 = nil) or (Blob2.IsEmpty)];
    if Result <> BothNotNull then Exit;
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
        {$IFDEF UNICODE}
        Result := AnsiCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
        {$ELSE}
        Result := WideCompareStr(Blob1.GetUnicodeString, Blob2.GetUnicodeString);
        {$ENDIF}
      {$ENDIF}
    end
    else
      Result := ZMemLComp(Blob1.GetBuffer, Blob2.GetBuffer, Max(Blob1.Length, Blob2.Length));
  end;
end;

function CompareUnicodeCLob_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicodeClob_Asc(Null1,Null2,V1,V2);
end;

function CompareUnicodeCLob_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
  ValuePtr1, ValuePtr2: Pointer;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Blob1 := IZBlob(PPointer(V1)^);
    Blob2 := IZBlob(PPointer(V2)^);
    Result := NullsEqualMatrix[(Blob1 = nil) or (Blob1.IsEmpty), (Blob2 = nil) or (Blob2.IsEmpty)];
    if Result <> BothNotNull then Exit;
    if Blob1.IsUpdated or Blob2.IsUpdated then
      if Blob1.IsClob and Blob2.IsClob then
      begin
        ValuePtr1 := Blob1.GetPWideChar;
        ValuePtr2 := Blob2.GetPWideChar;
        if Blob1.Length <> Blob2.Length then
          Result := 1 else
          Result := ZMemLComp(ValuePtr1, ValuePtr2, Blob1.Length  shl 1);
      end else begin
        ValuePtr1 := Blob1.GetBuffer;
        ValuePtr2 := Blob2.GetBuffer;
        if Blob1.Length <> Blob2.Length then
          Result := 1 else
          Result := ZMemLComp(ValuePtr1, ValuePtr2, Blob1.Length);
      end
    else Result := 0;
  end;
end;

function CompareBlob_Equals(const Null1, Null2: Boolean; const V1, V2): Integer;
var
  Blob1, Blob2: IZBlob;
begin
  Result := NullsEqualMatrix[Null1, Null2];
  if Result = BothNotNull then
  begin
    // Both values not null
    Blob1 := IZBlob(PPointer(V1)^);
    Blob2 := IZBlob(PPointer(V2)^);
    Result := NullsEqualMatrix[(Blob1 = nil) or (Blob1.IsEmpty), (Blob2 = nil) or (Blob2.IsEmpty)];
    if Result <> BothNotNull then Exit;
    if Blob1.IsUpdated or Blob2.IsUpdated then
      if Blob1.Length <> Blob2.Length then Result := 1
      else Result := ZMemLComp(Blob1.GetBuffer, Blob2.GetBuffer, Blob1.Length)
    else Result := 1;
  end;
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
  FRaw := ConSettings^.ClientCodePage^.IsStringFieldCPConsistent
          and (ConSettings^.ClientCodePage^.Encoding in [ceAnsi, ceUTF8]);
  FClientCP := ConSettings^.ClientCodePage^.CP;
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
  FRowSize := FColumnsSize + RowHeaderSize;
end;

{**
  Checks is the column index correct and row buffer is available.
  @param ColumnIndex an index of column.
}
procedure TZRowAccessor.CheckColumnIndex(ColumnIndex: Integer);
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
procedure TZRowAccessor.CheckColumnConvertion(ColumnIndex: Integer;
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
    stBoolean:    Result := SizeOf(WordBool);
    stByte:       Result := SizeOf(Byte);
    stShort:      Result := SizeOf(ShortInt);
    stWord:       Result := SizeOf(Word);
    stSmall:      Result := SizeOf(SmallInt);
    stLongWord:   Result := SizeOf(Cardinal);
    stInteger:    Result := SizeOf(Integer);
    stULong:      Result := SizeOf(UInt64);
    stLong:       Result := SizeOf(Int64);
    stFloat:      Result := SizeOf(Single);
    stDouble:     Result := SizeOf(Double);
    stCurrency:   Result := SizeOf(Currency);
    stBigDecimal: Result := SizeOf(TBCD);
    stString, stUnicodeString,
    stAsciiStream, stUnicodeStream, stBinaryStream,
    stDataSet, stArray:
      Result := SizeOf(Pointer);
    stGUID: Result := SizeOf(TGUID);
    stBytes:      Result := SizeOf(Pointer) + SizeOf(Word);
    stDate:       Result := SizeOf(TZDate);
    stTime:       Result := SizeOf(TZTime);
    stTimeStamp:  Result := SizeOf(TZTimeStamp);
    else
      Result := 0;
  end;
end;

function TZRowAccessor.InternalGetInt(ColumnIndex: Integer; out IsNull: Boolean): Integer;
var
  Data: PPointer;
begin
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    {$Q-}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := Integer(PInt64(Data)^);
      stFloat: Result := Integer({$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(Data)^));
      stDouble: Result := Integer({$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(Data)^));
      stCurrency: Result := Integer(PInt64(Data)^ div 10000);
      stBigDecimal: Result := BCD2Int64(PBCD(Data)^);
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then Result := RawToIntDef(PPAnsiChar(Data)^+PAnsiInc, 0)
        else Result := UnicodeToIntDef(ZPPWideChar(Data)^+PWideInc, 0);
      stUnicodeStream:
        if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
            then Result := UnicodeToIntDef(PIZlob(Data)^.GetPWideChar, 0)
            else Result := RawToIntDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
      stAsciiStream, stBinaryStream:
        if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
            then Result := RawToIntDef(PIZlob(Data)^.GetPAnsiChar(ConSettings.ClientCodePage.CP), 0)
            else Result := RawToIntDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
    end;
    {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
    IsNull := False;
  end
  else
    IsNull := True;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZRowAccessor.InternalGetULong(ColumnIndex: Integer; out IsNull: Boolean): UInt64;
var
  Data: PPointer;
begin
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := PInt64(Data)^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(Data)^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(Data)^);
      stCurrency: Result := (PInt64(Data)^ div 10000);
      stBigDecimal: Result := BCD2UInt64(PBCD(Data)^);
      stString, stUnicodeString: if Data^ <> nil then
          if fRaw
          then Result := RawToUInt64Def(PPAnsiChar(Data)^+PAnsiInc, 0)
          else Result := UnicodeToUInt64Def(ZPPWideChar(Data)^+PWideInc, 0);
      stUnicodeStream:
        if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
            then Result := UnicodeToUInt64Def(PIZlob(Data)^.GetPWideChar, 0)
            else Result := RawToUInt64Def(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
      stAsciiStream, stBinaryStream:
        if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
            then Result := RawToUInt64Def(PIZlob(Data)^.GetPAnsiChar(ConSettings.ClientCodePage.CP), 0)
            else Result := RawToUInt64Def(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
    end;
    IsNull := False;
  end
  else
    IsNull := True;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZRowAccessor.InternalSetInt(ColumnIndex, Value: Integer);
var
  Data: PPointer;
  L: NativeUint;
begin
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte: PByte(Data)^ := Byte(Value);
    stShort: PShortInt(Data)^ := ShortInt(Value);
    stWord: PWord(Data)^ := Word(Value);
    stSmall: PSmallInt(Data)^ := SmallInt(Value);
    stLongWord: PCardinal(Data)^ := Cardinal(Value);
    stInteger: PInteger(Data)^ := Value;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := Uint64(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(Data)^ := Value;
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      if fRaw then begin
        IntToRaw(Value, @TinyBuffer[0], @Data);
        L := PAnsiChar(Data) - PAnsiChar(@TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        IntToUnicode(Value, @TinyBuffer[0], @Data);
        L := PWideChar(Data) - PWideChar(@TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
  end;
end;

procedure TZRowAccessor.InternalSetULong(ColumnIndex: Integer; const Value: UInt64);
var
  Data: PPointer;
  L: NativeUint;
begin
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte: PByte(Data)^ := Value;
    stShort: PShortInt(Data)^ := Value;
    stWord: PWord(Data)^ := Value;
    stSmall: PSmallInt(Data)^ := Value;
    stLongWord: PCardinal(Data)^ := Value;
    stInteger: PInteger(Data)^ := Value;
    stULong: PUInt64(Data)^ := Value;
    stLong: PInt64(Data)^ := Value;
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(Data)^, False);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      if fRaw then begin
        IntToRaw(Value, @TinyBuffer[0], @Data);
        L := PAnsiChar(Data) - PAnsiChar(@TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        IntToUnicode(Value, @TinyBuffer[0], @Data);
        L := PWideChar(Data) - PWideChar(@TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
  end;
end;

procedure TZRowAccessor.InternalSetBytes(BuffAddr: PPointer; Buf: Pointer; Len: Word);
begin
  if (BuffAddr^ <> nil) and (Len <> PWord(PAnsiChar(BuffAddr)+SizeOf(Pointer))^) then begin
    FreeMem(BuffAddr^);
    BuffAddr^ := nil;
  end;
  PWord(PAnsiChar(BuffAddr)+SizeOf(Pointer))^ := Len;
  if (Len > 0) and (Buf <> nil) then begin
    if BuffAddr^ = nil then
      GetMem(BuffAddr^, Len);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, BuffAddr^^, Len);
  end;
end;

procedure TZRowAccessor.InternalSetString(BuffAddr: PPointer;
  const Value: RawByteString);
begin
  InternalSetPAnsiChar(BuffAddr, Pointer(Value), Length(Value));
end;

procedure TZRowAccessor.InternalSetPWideChar(BuffAddr: PPointer;
  Value: PWideChar; Len: Cardinal);
var
  LMem: Cardinal;
begin
  if (BuffAddr^ <> nil) and (Len <> PLongWord(BuffAddr^)^) then begin
    FreeMem(BuffAddr^);
    BuffAddr^ := nil;
  end;
  if (Len > 0) and (Value <> nil) then begin
    LMem := Len*SizeOf(WideChar);
    if BuffAddr^ = nil then
      GetMem(BuffAddr^, LMem+SizeOf(LongWord)+SizeOf(WideChar)); //including #0#0 terminator
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, (PWideChar(BuffAddr^)+PWideInc)^, LMem);
    PLongWord(BuffAddr^)^ := Len;
    (PWideChar(BuffAddr^)+PWideInc+Len)^ := WideChar(#0);
  end;
end;


procedure TZRowAccessor.InternalSetPAnsiChar(BuffAddr: PPointer;
  Value: PAnsiChar; Len: Cardinal);
begin
  if (BuffAddr^ <> nil) and (Len <> PLongWord(BuffAddr^)^) then begin
    System.FreeMem(BuffAddr^);
    BuffAddr^ := nil;
  end;
  if (Len > 0) and (Value <> nil) then begin
    if BuffAddr^ = nil then
      GetMem(BuffAddr^, Len+SizeOf(LongWord)+SizeOf(AnsiChar)); //including #0 terminator
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, (PPAnsiChar(BuffAddr)^+PAnsiInc)^, Len);
    PLongWord(BuffAddr^)^ := Len;
    AnsiChar((PPAnsiChar(BuffAddr)^+PAnsiInc+Len)^) := AnsiChar(#0); //set #0 terminator if a truncation is required e.g. FireBird Char columns with trailing spaces
  end;
end;

{**
  Allocates a new row buffer and sets it into the variable.
  @param Buffer a pointer to row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.AllocBuffer: PZRowBuffer;
begin
  GetMem(Result, FRowSize);
  InitBuffer(Result);
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

{$IFDEF USE_SYNCOMMONS}
procedure TZRowAccessor.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var Data: PPointer;
    I, H, C: SmallInt;
    Blob: IZBlob;
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
    {$R-}
    Data := @FBuffer.Columns[FColumnOffsets[C] + 1];
    if FBuffer.Columns[FColumnOffsets[C]] = bIsNull then begin
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      if JSONWriter.Expand then begin
        if not (jcsSkipNulls in JSONComposeOptions) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,');
    end else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case FColumnTypes[C] of
        stBoolean       : JSONWriter.AddShort(JSONBool[PWord(Data)^ <> 0]);
        stByte          : JSONWriter.AddU(PByte(Data)^);
        stShort         : JSONWriter.Add(PShortInt(Data)^);
        stWord          : JSONWriter.AddU(PWord(Data)^);
        stSmall         : JSONWriter.Add(PSmallInt(Data)^);
        stLongWord      : JSONWriter.AddU(PCardinal(Data)^);
        stInteger       : JSONWriter.Add(PInteger(Data)^);
        stULong         : JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PUInt64(Data)^));
        stLong          : JSONWriter.Add(PInt64(Data)^);
        stFloat         : JSONWriter.AddSingle(PSingle(Data)^);
        stDouble        : JSONWriter.AddDouble(PDouble(Data)^);
        stCurrency      : JSONWriter.AddCurr64(PCurrency(Data)^);
        stBigDecimal    : JSONWriter.AddNoJSONEscape(@TinyBuffer[0], BCDToRaw(PBCD(Data)^, @TinyBuffer[0], '.'));
        stString,
        stUnicodeString : begin
                            JSONWriter.Add('"');
                            if (Data^ <> nil) then begin
                              if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or
                                 (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) then
                                JSONWriter.AddJSONEscapeW(Pointer(ZPPWideChar(Data)^+PWideInc),
                                  PCardinal(PPointer(Data)^)^)
                              else if FClientCP = zCP_UTF8 then
                                JSONWriter.AddJSONEscape(PPAnsiChar(Data)^+PAnsiInc,
                                  PCardinal(PPointer(Data)^)^)
                              else begin
                                FUniTemp := PRawToUnicode(PPAnsiChar(Data)^+PAnsiInc,
                                  PCardinal(PPointer(Data)^)^, FClientCP);
                                JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
                              end;
                            end;
                            JSONWriter.Add('"');
                          end;
        stBytes         : JSONWriter.WrBase64(Data^, PWord(PAnsiChar(Data)+SizeOf(Pointer))^, True);
        stGUID          : begin
                            JSONWriter.Add('"');
                            JSONWriter.Add(PGUID(Data)^);
                            JSONWriter.Add('"');
                          end;
        stTime          : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            end else
                              JSONWriter.Add('"');
                            TimeToIso8601PChar(@TinyBuffer[0], True, PZTime(Data)^.hour,
                              PZTime(Data)^.Minute, PZTime(Data)^.second, PZTime(Data)^.Fractions div NanoSecsPerMSec,
                                'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@TinyBuffer[0],8+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
        stDate          : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            if PZDate(Data)^.IsNegative then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(@TinyBuffer[0], True, PZDate(Data)^.Year,
                              PZDate(Data)^.Month, PZDate(Data)^.Day);
                            JSONWriter.AddNoJSONEscape(@TinyBuffer[0],10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('T00:00:00Z")')
                            else JSONWriter.Add('"');
                          end;
        stTimestamp     : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            if PZTimeStamp(Data)^.IsNegative then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(@TinyBuffer[0], True, PZTimeStamp(Data)^.Year,
                               PZTimeStamp(Data)^.Month, PZTimeStamp(Data)^.Day);
                            TimeToIso8601PChar(@TinyBuffer[10], True, PZTimeStamp(Data)^.Hour,
                              PZTimeStamp(Data)^.Minute, PZTimeStamp(Data)^.Second, PZTimeStamp(Data)^.Fractions div NanoSecsPerMSec,
                                'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@TinyBuffer[0],19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
        stAsciiStream, stUnicodeStream:
          begin
            Blob := IZBlob(Data^);
            if Blob.IsEmpty then
              JSONWriter.AddShort('null')
            else begin
              if Blob.IsClob then
                PAnsiChar(Data) := Blob.GetPAnsiChar(zCP_UTF8) else
                Data := Blob.GetBuffer;
              JSONWriter.Add('"');
              JSONWriter.AddJSONEscape(Data, Blob.Length);
              JSONWriter.Add('"');
            end;
          end;
        stBinaryStream:
          begin
            Blob := IZBlob(PPointer(Data)^);
            if Blob.IsEmpty then
              JSONWriter.AddShort('null')
            else
              JSONWriter.WrBase64(Blob.GetBuffer, Blob.Length, True);
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
  Compares fields from two row buffers.
  @param Buffer1 the first row buffer to compare.
  @param Buffer2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZRowAccessor.CompareBuffer(Buffer1, Buffer2: PZRowBuffer;
  ColumnIndex: Integer; CompareFunc: TCompareFunc): Integer;
var ValuePtr1, ValuePtr2: Pointer;
begin
  {$IFNDEF GENERIC_INDEX}ColumnIndex := ColumnIndex-1{$ENDIF};
  { Compares column values. }
  ValuePtr1 := @Buffer1.Columns[FColumnOffsets[ColumnIndex] + 1];
  ValuePtr2 := @Buffer2.Columns[FColumnOffsets[ColumnIndex] + 1];
  if @CompareFunc = @CompareNothing
  then Result := -1
  else Result := CompareFunc(
    (Buffer1.Columns[FColumnOffsets[ColumnIndex]] = bIsNull),
    (Buffer2.Columns[FColumnOffsets[ColumnIndex]] = bIsNull),
      ValuePtr1, ValuePtr2);
end;

function TZRowAccessor.CompareBuffers(Buffer1, Buffer2: PZRowBuffer;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var I: Integer;
begin
  Result := 0; //satisfy compiler
  for I := Low(ColumnIndices) to High(ColumnIndices) do begin
    Result := CompareBuffer(Buffer1, Buffer2, ColumnIndices[I], CompareFuncs[i]);
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
        ckAscending:  Result := CompareBigDecimal_Asc;
        ckDescending: Result := CompareBigDecimal_Desc;
        ckEquals:     Result := CompareBigDecimal_Equals;
      end;
    stDate: case CompareKind of
              ckAscending:  Result := CompareZDate_Asc;
              ckDescending: Result := CompareZDate_Desc;
              ckEquals:     Result := CompareZDate_Equals;
            end;
    stTime: case CompareKind of
              ckAscending:  Result := CompareZTime_Asc;
              ckDescending: Result := CompareZTime_Desc;
              ckEquals:     Result := CompareZTime_Equals;
            end;
    stTimestamp: case CompareKind of
              ckAscending:  Result := CompareZTimeStamp_Asc;
              ckDescending: Result := CompareZTimeStamp_Desc;
              ckEquals:     Result := CompareZTimeStamp_Equals;
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
      if fRaw then
        case CompareKind of
          ckAscending:
            {$IFNDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            if FClientCP = zCP_UTF8 then
              Result := CompareUnicodeFromUTF8_Asc
            else
              Result := CompareNativeRaw_Asc;
            {$ELSE}
            Result := CompareUnicodeFromUTF8_Asc;
            {$ENDIF}
          ckDescending:
            {$IFNDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            if FClientCP = zCP_UTF8 then
              Result := CompareUnicodeFromUTF8_Desc
            else
              Result := CompareNativeRaw_Desc;
            {$ELSE}
            Result := CompareUnicodeFromUTF8_Desc;
            {$ENDIF}
          ckEquals:     Result := CompareRaw_Equals;
        end
      else
        case CompareKind of
          ckAscending:  Result := CompareUnicode_Asc;
          ckDescending: Result := CompareUnicode_Desc;
          ckEquals:     Result := CompareUnicode_Equals;
        end;
    stAsciiStream, stUnicodeStream:
      if ConSettings^.CPType in [cCP_UTF16, cCP_UTF8] then
        case CompareKind of
          ckAscending: Result := CompareUnicodeCLob_Asc;
          ckDescending: Result := CompareUnicodeCLob_Desc;
          ckEquals: Result := CompareUnicodeCLob_Equals;
        end
      else
        case CompareKind of
          ckAscending: Result := CompareNativeCLob_Asc;
          ckDescending: Result := CompareNativeCLob_Desc;
          ckEquals: Result := CompareNativeCLob_Equals;
        end;
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
begin
  Buffer^.Index := -1;
  Buffer^.UpdateType := utUnmodified;
  Buffer^.BookmarkFlag := 0;
  {$R-}
  for I := 0 to High(FBytesCols) do
    if (Buffer^.Columns[FColumnOffsets[FBytesCols[i]]] = bIsNotNull) then
      if PPointer(@Buffer^.Columns[FColumnOffsets[FBytesCols[i]] +1])^ <> nil then begin
        System.FreeMem(PPointer(@Buffer^.Columns[FColumnOffsets[FBytesCols[i]] +1])^);
        PPointer(@Buffer^.Columns[FColumnOffsets[FBytesCols[i]] +1])^ := nil;
      end;
  for I := 0 to High(FStringCols) do
    if (Buffer^.Columns[FColumnOffsets[FStringCols[i]]] = bIsNotNull) then
      if PPointer(@Buffer^.Columns[FColumnOffsets[FStringCols[i]] +1])^ <> nil then begin
        System.FreeMem(PPointer(@Buffer^.Columns[FColumnOffsets[FStringCols[i]] +1])^);
        PPointer(@Buffer^.Columns[FColumnOffsets[FStringCols[i]] +1])^ := nil;
      end;
  for I := 0 to High(FLobCols) do
    if (Buffer^.Columns[FColumnOffsets[FLobCols[I]]] = bIsNotNull) then
      PIZLob(@Buffer^.Columns[FColumnOffsets[FLobCols[I]] +1])^ := nil;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if WithFillChar then
    FillChar(Buffer^.Columns, FColumnsSize, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
end;

{**
  Allocates a new row buffer and saves it as internal field (externally visible as RowBuffer).
}
procedure TZRowAccessor.Alloc;
begin
  FBuffer := AllocBuffer;
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
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.CopyBuffer(SrcBuffer, DestBuffer: PZRowBuffer;
  const CloneLobs: Boolean);
var
  I: Integer;
  DestAddress: PPointer;
begin
  ClearBuffer(DestBuffer, False);
  DestBuffer^.Index := SrcBuffer^.Index;
  DestBuffer^.UpdateType := SrcBuffer^.UpdateType;
  DestBuffer^.BookmarkFlag := SrcBuffer^.BookmarkFlag;
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(SrcBuffer^.Columns, DestBuffer^.Columns, FColumnsSize);
{$R-}
  for i := 0 to FHighBytesCols do
    if (SrcBuffer^.Columns[FColumnOffsets[FBytesCols[i]]] = bIsNotNull) and
       (PPointer(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1])^ <> nil) then begin
      DestAddress := @DestBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1];
      DestAddress^ := nil;
      InternalSetBytes(DestAddress,
        PPointer(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1])^,
        PWord(@SrcBuffer.Columns[FColumnOffsets[FBytesCols[i]]+1+SizeOf(Pointer)])^);
    end;
  if fRaw then begin
    for i := 0 to FHighStringCols do
      if (SrcBuffer^.Columns[FColumnOffsets[FStringCols[i]]] = bIsNotNull) and
         (PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^ <> nil) then begin
        DestAddress := @DestBuffer.Columns[FColumnOffsets[FStringCols[i]]+1];
        DestAddress^ := nil;
        InternalSetPAnsiChar(DestAddress,
          PPAnsiChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^+PAnsiInc,
          PCardinal(PPointer(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^)^);
      end;
  end else begin
    for i := 0 to FHighStringCols do
      if (SrcBuffer^.Columns[FColumnOffsets[FStringCols[i]]] = bIsNotNull) and
         (ZPPWideChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^ <> nil) then begin
        DestAddress := @DestBuffer.Columns[FColumnOffsets[FStringCols[i]]+1];
        DestAddress^ := nil;
        InternalSetPWideChar(DestAddress,
          ZPPWideChar(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^+PWideInc,
          PCardinal(PPointer(@SrcBuffer.Columns[FColumnOffsets[FStringCols[i]]+1])^)^);
      end;
  end;
  for i := 0 to FHighLobCols do
    if (SrcBuffer^.Columns[FColumnOffsets[FLobCols[i]]] = bIsNotNull) then begin
      PPointer(@DestBuffer.Columns[FColumnOffsets[FLobCols[i]]+1])^ := nil; //init to avoid refcounting
      if CloneLobs
      then PIZLob(@DestBuffer.Columns[FColumnOffsets[FLobCols[i]]+1])^ := PIZLob(@SrcBuffer.Columns[FColumnOffsets[FLobCols[i]]+1])^.Clone
      else PIZLob(@DestBuffer.Columns[FColumnOffsets[FLobCols[i]]+1])^ := PIZLob(@SrcBuffer.Columns[FColumnOffsets[FLobCols[i]]+1])^;
    end;
{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
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

function TZRowAccessor.GetCharRec(ColumnIndex: Integer;
  out IsNull: Boolean): TZCharRec;
var Len: NativeUint;
begin
  if fRaw then begin
    Result.P := GetPAnsiChar(ColumnIndex, IsNull, Len);
    Result.CP := FClientCP;
  end else begin
    Result.P := GetPWideChar(ColumnIndex, IsNull, Len);
    Result.CP := zCP_UTF16;
  end;
  Result.Len := Len;
end;

{**
  Gets the case sensitive flag of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the case sensitive flag of the column data buffer.
}
function TZRowAccessor.GetColumnCase(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  Result := FColumnCases[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets a pointer to the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a pointer to the column data buffer.
}
function TZRowAccessor.GetColumnData(ColumnIndex: Integer;
  out IsNull: Boolean): Pointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  {$R-}
  Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex {$IFNDEF GENERIC_INDEX}- 1{$ENDIF}] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets a size of the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a size of the column data buffer.
}
function TZRowAccessor.GetColumnDataSize(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  Result := FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}];
end;

{**
  Gets then length of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the length of the column data buffer.
}
function TZRowAccessor.GetColumnLength(ColumnIndex: Integer): Integer;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then name of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the name of the column data buffer.
}
function TZRowAccessor.GetColumnName(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  Result := FColumnNames[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then offset of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return then offset of the column data buffer.
}
function TZRowAccessor.GetColumnOffSet(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  Result := FColumnOffSets[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

{**
  Gets then SQLType of a column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the SQLType of the column data buffer.
}
function TZRowAccessor.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin
  CheckColumnIndex(ColumnIndex);
  Result := FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

function TZRowAccessor.GetColumnDefaultExpression(ColumnIndex: Integer): string;
begin
//{$IFNDEF DISABLE_CHECKING}
//  CheckColumnIndex(ColumnIndex);
//{$ENDIF}
  Result := FColumnDefaultExpressions[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
end;

procedure TZRowAccessor.SetColumnDefaultExpression(ColumnIndex: Integer; const Value: string);
begin
//{$IFNDEF DISABLE_CHECKING}
//  CheckColumnIndex(ColumnIndex);
//{$ENDIF}
  FColumnDefaultExpressions[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

procedure TZRowAccessor.SetColumnCodePage(ColumnIndex: Integer; const Value: Word);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
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
function TZRowAccessor.IsNull(ColumnIndex: Integer): Boolean;
var TempBlob: PIZLob;
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create(SRowBufferIsNotAssigned);

  if (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > FColumnCount {$IFDEF GENERIC_INDEX} -1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  {$R-}
  Result := FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull;
  if not Result and (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stUnicodeStream, stAsciiStream, stBinaryStream]) then begin
    TempBlob := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] +1];
    Result := (TempBlob^ = nil) or TempBlob.IsEmpty;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
function TZRowAccessor.GetPAnsiChar(ColumnIndex: Integer; out IsNull: Boolean;
  out Len: NativeUInt): PAnsiChar;
var Data: PPointer;
label Set_Results, SetEmpty;
begin
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    IsNull := False;
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean:  if PWordBool(Data)^ then begin
                    Result := Pointer(BoolStrsRaw[True]);
                    Len := 4
                  end else begin
                    Result := Pointer(BoolStrsRaw[False]);
                    Len := 5;
                  end;
      stByte:     begin
                    IntToRaw(Cardinal(PByte(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stShort:    begin
                    IntToRaw(Integer(PShortInt(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stWord:     begin
                    IntToRaw(Cardinal(PWord(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stSmall:    begin
                    IntToRaw(Integer(PSmallInt(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stLongWord: begin
                    IntToRaw(PCardinal(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stInteger:  begin
                    IntToRaw(PInteger(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stULong:    begin
                    IntToRaw(PUInt64(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stLong:     begin
                    IntToRaw(PInt64(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stFloat:    begin
                    Len := FloatToSqlRaw(PSingle(Data)^, @TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stDouble:   begin
                    Len := FloatToSqlRaw(PDouble(Data)^, @TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stCurrency: begin
                    CurrToRaw(PCurrency(Data)^, @TinyBuffer[0], @Result);
Set_Results:        Len := Result - PAnsiChar(@TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stBigDecimal: begin
                    Result := @TinyBuffer[0];
                    Len := ZSysUtils.BcdToRaw(PBCD(Data)^, Result, '.')
                  end;
      stString,
      stUnicodeString: if (Data^ = nil) then
                    goto SetEmpty
                  else if fRaw then begin
                    Result := PPAnsiChar(Data)^+PAnsiInc;
                    Len := PCardinal(PPointer(Data)^)^;
                  end else begin
                    FRawTemp := PUnicodeToRaw(ZPPWideChar(Data)^+PWideInc,
                      PCardinal(PPointer(Data)^)^, FClientCP);
                    Len := Length(FRawTemp);
                    if Len > 0
                    then Result := Pointer(FRawTemp)
                    else Result := PEmptyAnsiString;
                  end;
      stBytes:    if Data^ <> nil then begin
                    Len := PWord(PAnsiChar(Data)+SizeOf(Pointer))^;
                    Result := Data^;
                  end else
                    goto SetEmpty;
      stGUID:     begin
                    GUIDToBuffer(Data, PAnsiChar(@TinyBuffer[0]), [guidWithBrackets]);
                    Result := @TinyBuffer[0];
                    Len := 38;
                  end;
      stDate:     begin
                    Result := @TinyBuffer[0];
                    Len := DateToRaw(PZDate(Data)^.Year, PZDate(Data)^.Month, PZDate(Data)^.Day,
                      Result, ConSettings^.DisplayFormatSettings.DateFormat, False, PZDate(Data)^.IsNegative);
                  end;
      stTime:     begin
                    Result := @TinyBuffer[0];
                    Len := TimeToRaw(PZTime(Data)^.Hour, PZTime(Data)^.Minute, PZTime(Data)^.Second,
                      PZTime(Data)^.Fractions, Result, ConSettings^.DisplayFormatSettings.TimeFormat, False, PZTime(Data)^.IsNegative);
                  end;
      stTimestamp:begin
                    Result := @TinyBuffer[0];
                    Len := DateTimeToRaw(PZTimeStamp(Data)^.Year, PZTimeStamp(Data)^.Month,
                      PZTimeStamp(Data)^.Day, PZTimeStamp(Data)^.Hour, PZTimeStamp(Data)^.Minute,
                      PZTimeStamp(Data)^.Second, PZTimeStamp(Data)^.Fractions, Result,
                      ConSettings^.DisplayFormatSettings.DateTimeFormat, False, PZTimeStamp(Data)^.IsNegative);
                  end;
      stAsciiStream, stUnicodeStream:
                  if (Data^ <> nil) and not PIZLob(Data)^.IsEmpty then begin
                    if PIZLob(Data)^.IsClob then begin
                      if ConSettings^.ClientCodePage.Encoding = ceUTF16// ConSettings^.AutoEncode
                      then Result := PIZLob(Data)^.GetPAnsiChar(ConSettings^.CTRL_CP)
                      else Result := PIZLob(Data)^.GetPAnsiChar(FClientCP);
                      Len := PIZLob(Data)^.Length;
                    end else begin
                      Result := PIZLob(Data)^.GetBuffer;
                      Len := PIZLob(Data)^.Length;
                    end;
                  end else
                    goto SetEmpty;
      else        begin
SetEmpty:           Len := 0;
                    Result := PEmptyAnsiString;
                  end;
    end;
  end else begin
    Len := 0;
    Result := nil;
    IsNull := True;
  end;
end;

function TZRowAccessor.GetString(ColumnIndex: Integer; out IsNull: Boolean): String;
var P: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF};
  Len: NativeUInt;
begin
  {$IFDEF UNICODE}
  P := GetPWideChar(ColumnIndex, IsNull, Len);
  if P = Pointer(FUniTemp)
  then Result := FUniTemp
  else System.SetString(Result, P, Len);
  {$ELSE}
  {$R-}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString, stUnicodeString:
      if fRaw then begin
        P := GetPAnsiChar(ColumnIndex, IsNull, Len);
        if not ConSettings^.AutoEncode or (ConSettings^.CTRL_CP = FClientCP) or (Len=0) then
          System.SetString(Result, P, Len)
        else begin
          fUniTemp := PRawToUnicode(P, Len, FClientCP);
          Result := PUnicodeToRaw(Pointer(fUniTemp), Length(FUniTemp), ConSettings^.CTRL_CP)
        end;
      end else begin
        PWideChar(P) := GetPWideChar(ColumnIndex, IsNull, Len);
        if ConSettings^.AutoEncode
        then Result := PUnicodeToRaw(PWideChar(P), Len, ConSettings^.CTRL_CP)
        else Result := PUnicodeToRaw(PWideChar(P), Len, FClientCP);
      end;
    else begin
      P := GetPAnsiChar(ColumnIndex, IsNull, Len);
      System.SetString(Result, P, Len);
    end;
  end;
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Ansi</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZRowAccessor.GetAnsiString(ColumnIndex: Integer; out IsNull: Boolean): AnsiString;
var P: Pointer;
  L: NativeUInt;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString,
    stUnicodeString:  if fRaw then begin
                        P := GetPAnsiChar(ColumnIndex, IsNull, L);
                        if L > 0 then
                          if fClientCP = zCP_UTF8 then
                            System.SetString(Result, PAnsiChar(P), L)
                          else begin
                            FUniTemp := PRawToUnicode(P, L, fClientCP);
                            Result := PUnicodeToRaw(Pointer(FUniTemp), Length(fUniTemp), zOSCodePage);
                          end
                        else Result := '';
                      end else begin
                        P := GetPWideChar(ColumnIndex, IsNull, L);
                        if L > 0
                        then Result := PUnicodeToRaw(P, L, zOSCodePage)
                        else Result := '';
                      end;
    stAsciiStream,
    stUnicodeStream: {$R-}
        if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
          P := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
          if (PPointer(P)^ <> nil) and not PIZLob(P)^.IsEmpty then
            if PIZLob(P)^.IsClob
            then Result := PIZLob(P)^.GetAnsiString
            else Result := PIZLob(P)^.GetString
          else Result := '';
        end else Result := '';
    else begin
      P := GetPAnsiChar(ColumnIndex, IsNull, L);
      System.SetString(Result, PAnsiChar(P), L);
    end;
  end;
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZRowAccessor.GetUTF8String(ColumnIndex: Integer; out IsNull: Boolean): UTF8String;
var P: Pointer;
  L: NativeUInt;
begin
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stString,
    stUnicodeString:  if fRaw then begin
                        P := GetPAnsiChar(ColumnIndex, IsNull, L);
                        if L > 0 then
                          if fClientCP = zCP_UTF8 then
                            ZSetString(PAnsiChar(P), L, Result)
                          else begin
                            FUniTemp := PRawToUnicode(P, L, fClientCP);
                            Result := PUnicodeToRaw(Pointer(FUniTemp), Length(fUniTemp), zCP_UTF8);
                          end
                        else Result := '';
                      end else begin
                        P := GetPWideChar(ColumnIndex, IsNull, L);
                        if L > 0
                        then Result := PUnicodeToRaw(P, L, zCP_UTF8)
                        else Result := '';
                      end;
    stAsciiStream,
    stUnicodeStream: {$R-}
        if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
          P := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
          if (PPointer(P)^ <> nil) and not PIZLob(P)^.IsEmpty then
            if PIZLob(P)^.IsClob
            then Result := PIZLob(P)^.GetUTF8String
            else Result := PIZLob(P)^.GetString
          else Result := '';
        end else Result := '';
    else begin
      P := GetPAnsiChar(ColumnIndex, IsNull, L);
      ZSetString(PAnsiChar(P), L, Result);
    end;
  end;
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetRawByteString(ColumnIndex: Integer; out IsNull: Boolean): RawByteString;
var P: PAnsichar;
  L: NativeUInt;
begin
  P := GetPAnsiChar(ColumnIndex, IsNull, L);
  ZSetString(P, L, Result);
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
function TZRowAccessor.GetPWideChar(ColumnIndex: Integer;
  out IsNull: Boolean; out Len: NativeUInt): PWideChar;
var Data: PPointer;
label Set_Results, SetEmpty, Set_From_Temp;
begin
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    IsNull := False;
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean:  if PWordBool(Data)^ then begin
                    Result := Pointer(BoolStrsW[True]);
                    Len := 4
                  end else begin
                    Result := Pointer(BoolStrsW[False]);
                    Len := 5;
                  end;
      stByte:     begin
                    IntToUnicode(Cardinal(PByte(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stShort:    begin
                    IntToUnicode(Integer(PShortInt(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stWord:     begin
                    IntToUnicode(Cardinal(PWord(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stSmall:    begin
                    IntToUnicode(Integer(PSmallInt(Data)^), @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stLongWord: begin
                    IntToUnicode(PCardinal(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stInteger:  begin
                    IntToUnicode(PInteger(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stULong:    begin
                    IntToUnicode(PUInt64(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stLong:     begin
                    IntToUnicode(PInt64(Data)^, @TinyBuffer[0], @Result);
                    goto Set_Results;
                  end;
      stFloat:    begin
                    Len := FloatToSqlUnicode(PSingle(Data)^, @TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stDouble:   begin
                    Len := FloatToSqlUnicode(PDouble(Data)^, @TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stCurrency: begin
                    CurrToUnicode(PCurrency(Data)^, @TinyBuffer[0], @Result);
Set_Results:        Len := Result - PWideChar(@TinyBuffer[0]);
                    Result := @TinyBuffer[0];
                  end;
      stBigDecimal: begin
                    Result := @TinyBuffer[0];
                    Len := ZSysUtils.BcdToUni(PBCD(Data)^, Result, '.')
                  end;
      stString,
      stUnicodeString: if (Data^ = nil) then
                    goto SetEmpty
                  else if fRaw then begin
                    FUniTemp := PRawToUnicode(PPAnsiChar(Data)^+PAnsiInc, PCardinal(PPointer(Data)^)^, FClientCP);
                    goto Set_From_Temp;
                  end else begin
                    Result := ZPPWideChar(Data)^+PWideInc;
                    Len := PLongWord(Data^)^;
                  end;
      stBytes:    if Data^ <> nil then begin
                    fUniTemp := Ascii7ToUnicodeString(Data^, PWord(PAnsiChar(Data)+SizeOf(Pointer))^);
                    goto Set_From_Temp;
                  end else
                    goto SetEmpty;
      stGUID:     begin
                    GUIDToBuffer(Data, PWideChar(@TinyBuffer[0]), [guidWithBrackets]);
                    Result := @TinyBuffer[0];
                    Len := 38;
                  end;
      stDate:     begin
                    Result := @TinyBuffer[0];
                    Len := DateToUni(PZDate(Data)^.Year, PZDate(Data)^.Month, PZDate(Data)^.Day,
                      Result, ConSettings^.DisplayFormatSettings.DateFormat, False, PZDate(Data)^.IsNegative);
                  end;
      stTime:     begin
                    Result := @TinyBuffer[0];
                    Len := TimeToUni(PZTime(Data)^.Hour, PZTime(Data)^.Minute, PZTime(Data)^.Second,
                      PZTime(Data)^.Fractions, Result, ConSettings^.DisplayFormatSettings.TimeFormat, False, PZTime(Data)^.IsNegative);
                  end;
      stTimestamp:begin
                    Result := @TinyBuffer[0];
                    Len := DateTimeToUni(PZTimeStamp(Data)^.Year, PZTimeStamp(Data)^.Month,
                      PZTimeStamp(Data)^.Day, PZTimeStamp(Data)^.Hour, PZTimeStamp(Data)^.Minute,
                      PZTimeStamp(Data)^.Second, PZTimeStamp(Data)^.Fractions, Result,
                      ConSettings^.DisplayFormatSettings.DateTimeFormat, False, PZTimeStamp(Data)^.IsNegative);
                  end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
                  if (Data^ <> nil) and not PIZLob(Data)^.IsEmpty then begin
                    if PIZLob(Data)^.IsClob then begin
                      Result := PIZLob(Data)^.GetPWideChar;
                      Len := PIZLob(Data)^.Length shr 1;
                    end else begin
                      FUniTemp := Ascii7ToUnicodeString(PIZLob(Data)^.GetBuffer, PIZLob(Data)^.Length);
Set_From_Temp:        Len := Length(FUniTemp);
                      if Len > 0
                      then Result := Pointer(FUniTemp)
                      else Result := PEmptyUnicodeString;
                    end;
                  end else
                    goto SetEmpty;
      else        begin
SetEmpty:           Len := 0;
                    Result := PEmptyUnicodeString;
                  end;
    end;
  end else begin
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
function TZRowAccessor.GetUnicodeString(ColumnIndex: Integer;
  out IsNull: Boolean): ZWideString;
var P: PWideChar;
  L: NativeUInt;
begin
  P := GetPWideChar(ColumnIndex, IsNull, L);
  if P = Pointer(FUniTemp)
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
function TZRowAccessor.GetBoolean(ColumnIndex: Integer; out IsNull: Boolean): Boolean;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: Result := PWordBool(Data)^;
      stByte: Result := PByte(Data)^ <> 0;
      stShort: Result := PShortInt(Data)^ <> 0;
      stWord: Result := PWord(Data)^ <> 0;
      stSmall: Result := PSmallInt(Data)^ <> 0;
      stLongWord: Result := PCardinal(Data)^ <> 0;
      stInteger: Result := PInteger(Data)^ <> 0;
      stULong: Result := PUInt64(Data)^ <> 0;
      stLong: Result := PInt64(Data)^ <> 0;
      stFloat: Result := PSingle(Data)^ <> 0;
      stDouble: Result := PDouble(Data)^ <> 0;
      stCurrency: Result := PCurrency(Data)^ <> 0;
      stBigDecimal: Result := BCDCompare(NullBCD, PBCD(Data)^) <> 0;
      stDate: Result := (PZDate(Data)^.Year or PZDate(Data)^.Month or PZDate(Data)^.Day) <> 0;
      stTime: Result := (PZTime(Data)^.Hour or PZTime(Data)^.Minute or PZTime(Data)^.Second or PZTime(Data)^.Fractions) <> 0;
      stTimeStamp: Result := (PZTimeStamp(Data)^.Year or PZTimeStamp(Data)^.Month or PZTimeStamp(Data)^.Day or
        PZTimeStamp(Data)^.Hour or PZTimeStamp(Data)^.Minute or PZTimeStamp(Data)^.Second or PZTimeStamp(Data)^.Fractions) <> 0;
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then Result := StrToBoolEx(PPAnsiChar(Data)^+PAnsiInc, False)
        else Result := StrToBoolEx(ZPPWideChar(Data)^+PWideInc, False);
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then Result := StrToBoolEx(PIZlob(Data)^.GetPWideChar)
          else Result := StrToBoolEx(PAnsiChar(PIZlob(Data)^.GetBuffer));
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then Result := StrToBoolEx(PIZlob(Data)^.GetPAnsiChar(ConSettings.ClientCodePage.CP))
          else Result := StrToBoolEx(PAnsiChar(PIZlob(Data)^.GetBuffer));
    end;
    IsNull := False;
  end else
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
function TZRowAccessor.GetByte(ColumnIndex: Integer; out IsNull: Boolean): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := Byte(InternalGetInt(ColumnIndex, IsNull));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetShort(ColumnIndex: Integer; out IsNull: Boolean): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := ShortInt(InternalGetInt(ColumnIndex, IsNull));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetWord(ColumnIndex: Integer; out IsNull: Boolean): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  Result := Word(InternalGetInt(ColumnIndex, IsNull));
end;
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>small</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetSmall(ColumnIndex: Integer; out IsNull: Boolean): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  Result := SmallInt(InternalGetInt(ColumnIndex, IsNull));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>Cardinal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetUInt(ColumnIndex: Integer; out IsNull: Boolean): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  Result := Cardinal(InternalGetULong(ColumnIndex, IsNull));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetInt(ColumnIndex: Integer; out IsNull: Boolean): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := InternalGetInt(ColumnIndex, IsNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetULong(ColumnIndex: Integer; out IsNull: Boolean): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  Result := InternalGetULong(ColumnIndex, IsNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetLong(ColumnIndex: Integer; out IsNull: Boolean): Int64;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$Q-}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := PInt64(Data)^;
      stFloat: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(Data)^);
      stDouble: Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(Data)^);
      stCurrency: Result := PInt64(Data)^ div 10000;
      stBigDecimal: Result := BCD2Int64(PBCD(Data)^);
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then Result := RawToInt64Def(PPAnsiChar(Data)^+PAnsiInc, 0)
        else Result := UnicodeToInt64Def(ZPPWideChar(Data)^+PWideInc, 0);
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then Result := UnicodeToInt64Def(PIZlob(Data)^.GetPWideChar, 0)
          else Result := RawToInt64Def(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then Result := RawToInt64Def(PIZlob(Data)^.GetPAnsiChar(ConSettings.ClientCodePage.CP), 0)
          else Result := RawToInt64Def(PAnsiChar(PIZlob(Data)^.GetBuffer), 0);
    end;
    {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
    IsNull := False;
  end else
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
function TZRowAccessor.GetFloat(ColumnIndex: Integer; out IsNull: Boolean): Single;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$Q-}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := PInt64(Data)^;
      stFloat: Result := PSingle(Data)^;
      stDouble: Result := PDouble(Data)^;
      stCurrency: Result := PCurrency(Data)^;
      stBigDecimal: Result := BCDToDouble(PBCD(Data)^);
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then SQLStrToFloatDef(PPAnsiChar(Data)^+PAnsiInc, 0, Result, PCardinal(PPointer(Data)^)^)
        else SQLStrToFloatDef(ZPPWideChar(Data)^+PWideInc, 0, Result, PCardinal(PPointer(Data)^)^);
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPAnsiChar(FClientCP), 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPWideChar, 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
    end;
    {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>GUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Result the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null-guid</code>
}
procedure TZRowAccessor.GetGUID(ColumnIndex: Integer; var Result: TGUID;
  out IsNull: Boolean);
var Data: PPointer;
label zero_guid;
  procedure FromString;
  begin
    Result := StringToGUID(GetString(ColumnIndex, IsNull))
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stString, stUnicodeString,
      stAsciiStream, stUnicodeStream: FromString;
      stBytes:    if PWord(PAnsiChar(Data)+SizeOf(Pointer))^ = SizeOf(TGUID)
                  then Result := PGUID(Data^)^
                  else goto zero_guid;
      stGUID:     Result := PGUID(Data)^;
      else
zero_guid: FillChar(Result.D1, SizeOf(TGUID), #0);
    end;
    IsNull := False;
  end else begin
    IsNull := True;
    FillChar(Result.D1, SizeOf(TGUID), #0);
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
function TZRowAccessor.GetDouble(ColumnIndex: Integer; out IsNull: Boolean): Double;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$Q-}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := PInt64(Data)^;
      stFloat: Result := PSingle(Data)^;
      stDouble: Result := PDouble(Data)^;
      stCurrency: Result := PCurrency(Data)^;
      stBigDecimal: Result := BCDToDouble(PBCD(Data)^);
      stTime: IsNull := not TryTimeToDateTime(PZTime(Data)^, TDateTime(Result));
      stDate: IsNull := not TryDateToDateTime(PZDate(Data)^, TDateTime(Result));
      stTimeStamp: IsNull := not TryTimeStampToDateTime(PZTimeStamp(Data)^, TDateTime(Result));
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then SQLStrToFloatDef(PPAnsiChar(Data)^+PAnsiInc, 0, Result, PCardinal(PPointer(Data)^)^)
        else SQLStrToFloatDef(ZPPWideChar(Data)^+PWideInc, 0, Result, PCardinal(PPointer(Data)^)^);
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPAnsiChar(FClientCP), 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPWideChar, 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
    end;
    {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
    IsNull := False;
  end else
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
function TZRowAccessor.GetCurrency(ColumnIndex: Integer; out IsNull: Boolean): Currency;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  Result := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    {$Q-}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: if PWordBool(Data)^ then Result := 1;
      stByte: Result := PByte(Data)^;
      stShort: Result := PShortInt(Data)^;
      stWord: Result := PWord(Data)^;
      stSmall: Result := PSmallInt(Data)^;
      stLongWord: Result := PCardinal(Data)^;
      stInteger: Result := PInteger(Data)^;
      stULong: Result := PUInt64(Data)^;
      stLong: Result := PInt64(Data)^;
      stFloat: Result := PSingle(Data)^;
      stDouble: Result := PDouble(Data)^;
      stCurrency: Result := PCurrency(Data)^;
      stBigDecimal: BCDToCurr(PBCD(Data)^, Result);
      stString, stUnicodeString: if Data^ <> nil then
        if fRaw
        then SQLStrToFloatDef(PPAnsiChar(Data)^+PAnsiInc, 0, Result, PCardinal(PPointer(Data)^)^)
        else SQLStrToFloatDef(ZPPWideChar(Data)^+PWideInc, 0, Result, PCardinal(PPointer(Data)^)^);
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPAnsiChar(FClientCP), 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then
          if PIZlob(Data)^.IsClob
          then SQLStrToFloatDef(PIZlob(Data)^.GetPWideChar, 0, Result)
          else SQLStrToFloatDef(PAnsiChar(PIZlob(Data)^.GetBuffer), 0, Result, PIZlob(Data)^.Length);
    end;
    {$IFDEF OverFlowCheckEnabled}{$Q+}{$ENDIF}
    IsNull := False;
  end else
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
procedure TZRowAccessor.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD; out IsNull: Boolean);
var Data: PPointer;
label NBCD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBoolean: ScaledOrdinal2BCD(Word(PWord(Data)^ <> 0), 0, Result);
      stByte: ScaledOrdinal2BCD(Word(PByte(Data)^), 0, Result);
      stShort: ScaledOrdinal2BCD(SmallInt(PShortInt(Data)^), 0, Result);
      stWord: ScaledOrdinal2BCD(PWord(Data)^, 0, Result);
      stSmall: ScaledOrdinal2BCD(PSmallInt(Data)^, 0, Result);
      stLongWord: ScaledOrdinal2BCD(PCardinal(Data)^, 0, Result);
      stInteger: ScaledOrdinal2BCD(PInteger(Data)^, 0, Result);
      stULong: ScaledOrdinal2BCD(PUInt64(Data)^,0, Result, False);
      stLong: ScaledOrdinal2BCD(PInt64(Data)^, 0, Result);
      stFloat: Double2BCD(PSingle(Data)^, Result);
      stDouble: Double2BCD(PDouble(Data)^,Result);
      stCurrency: Currency2Bcd(PCurrency(Data)^, Result);
      stBigDecimal: Result := PBCD(Data)^;
      stString, stUnicodeString: if fRaw
          then TryRawToBcd(PAnsiChar(Data^)+PAnsiInc, PLongWord(Data^)^, Result, '.')
          else TryUniToBcd(PWideChar(Data^)+PWideInc, PLongWord(Data^)^, Result, '.');
      stAsciiStream, stBinaryStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then begin
            if PIZlob(Data)^.IsClob then
              PIZlob(Data)^.GetPAnsiChar(FClientCP);
            TryRawToBcd(PIZlob(Data)^.GetBuffer, PIZlob(Data)^.Length, Result, '.');
          end else goto NBCD;
      stUnicodeStream: if (Data^ <> nil) and not PIZlob(Data)^.IsEmpty then begin
          if PIZlob(Data)^.IsClob then
            PIZlob(Data)^.GetPWideChar;
          TryUniToBcd(PIZlob(Data)^.GetBuffer, PIZlob(Data)^.Length, Result, '.');
        end else goto NBCD;
      else
NBCD:   Result := NullBcd;
    end;
    IsNull := False;
  end else
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
function TZRowAccessor.GetBytes(ColumnIndex: Integer; out IsNull: Boolean): TBytes;
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBytes:
        Result := BufferToBytes(Data^, PWord(PAnsiChar(Data)+SizeOf(Pointer))^);
      stBinaryStream, stAsciiStream, stUnicodeStream:
        if (Data^ <> nil) and not PIZlob(Data^)^.IsEmpty
          then Result := PIZlob(Data^)^.GetBytes
          else Result := nil;
      stString, stUnicodeString:
        if Data^ <> nil then
          if fRaw then begin
            Result := BufferToBytes( (PPAnsiChar(Data)^+PAnsiInc), PCardinal(PPointer(Data)^)^ )
          end else begin
            FRawTemp := UnicodeStringToASCII7(ZPPWideChar(Data)^+PWideInc, PCardinal(PPointer(Data)^)^);
            Result := BufferToBytes( Pointer(FRawTemp), Length(FRawTemp) );
          end
        else
          Result := nil;
      else
        Result := BufferToBytes(Data, FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]);
    end;
    IsNull := False;
  end else
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
function TZRowAccessor.GetBytes(ColumnIndex: Integer; out IsNull: Boolean;
  out Len: Word): Pointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  Len := 0;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stBytes: if (PPointer(Result)^ <> nil) then begin
          Len := PWord(PAnsiChar(Result)+SizeOf(Pointer))^;
          Result := PPointer(Result)^; //from address to data
        end;
      stBinaryStream, stAsciiStream, stUnicodeStream:
        if (PIZLob(Result)^ <> nil) and not PIZLob(Result)^.IsEmpty then begin
          Len := PIZLob(Result)^.Length;
          Result := PIZLob(Result)^.GetBuffer;
        end;
      stString, stUnicodeString: if (PPointer(Result)^ <> nil) then
          if fRaw then begin
            Len := PCardinal(PPointer(Result)^)^;
            Result := PPAnsiChar(Result)^+PAnsiInc;
          end else begin
            FRawTemp := UnicodeStringToASCII7(ZPPWideChar(Result)^+PWideInc, PCardinal(PPointer(Result)^)^);
            Len := Length(FRawTemp);
            Result := Pointer(FRawTemp);
          end;
      else //all other types have fixed length and points to data in buffer
        Len := FColumnLengths[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}];
    end;
    IsNull := False;
  end else
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
procedure TZRowAccessor.GetTime(ColumnIndex: Integer; out IsNull: Boolean; var Result: TZTime);
var
  Data: Pointer;
  Len: NativeUint;
label Fill, Dbl;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then
  begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    IsNull := False;
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stTime: Result := PZTime(Data)^;
      stDate: goto Fill;
      stTimestamp:  begin
                      Result.Hour := PZTimeStamp(Data)^.Hour;
                      Result.Minute := PZTimeStamp(Data)^.Minute;
                      Result.Second := PZTimeStamp(Data)^.Second;
                      Result.Fractions := PZTimeStamp(Data)^.Fractions;
                      Result.IsNegative := False;
                    end;
      stString, stUnicodeString, stAsciiStream, stUnicodeStream: begin
          if fRaw then begin
            Data := GetPAnsiChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToTime(PAnsiChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end else begin
            Data := GetPWideChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToTime(PWideChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end;
          if IsNull then goto Dbl;
        end;
      else
Dbl:    DecodeDateTimeToTime(GetDouble(ColumnIndex, IsNull), Result);
    end;
  end else begin
    IsNull := True;
Fill: PCardinal(@Result.Hour)^ := 0;
    PInt64(@Result.Second)^ := 0;
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
procedure TZRowAccessor.GetTimestamp(ColumnIndex: Integer; out IsNull: Boolean; var Result: TZTimeStamp);
var
  Data: Pointer;
  Len: NativeUInt;
label Dbl;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    IsNull := False;
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stDate: begin
           PInt64(@Result.Hour)^ := 0;
           PInt64(@Result.Fractions)^ := 0;
           Result.Year := PZDate(Data)^.Year;
           Result.Month := PZDate(Data)^.Month;
           Result.Day := PZDate(Data)^.Day;
           PCardinal(@Result.TimeZoneHour)^ := 0;
           Result.IsNegative := PZDate(Data)^.IsNegative;
        end;
      stTime: begin
           PInt64(@Result.Year)^ := 0;
           Result.Hour := PZTime(Data)^.Hour;
           Result.Minute := PZTime(Data)^.Minute;
           Result.Second := PZTime(Data)^.Second;
           Result.Fractions := PZTime(Data)^.Fractions;
           PCardinal(@Result.TimeZoneHour)^ := 0;
           Result.IsNegative := PZTime(Data)^.IsNegative;
        end;
      stTimestamp: Result := PZTimeStamp(Data)^;
      stString, stUnicodeString, stAsciiStream, stUnicodeStream: begin
          if fRaw then begin
            Data := GetPAnsiChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToTimeStamp(PAnsiChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end else begin
            Data := GetPWideChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToTimeStamp(PWideChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end;
          if IsNull then goto Dbl;
        end;
      else
Dbl:    DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, IsNull), Result);
    end;
  end else begin
    IsNull := True;
    FillChar(Result, SizeOf(TZTimeStamp), #0);
  end;
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
function TZRowAccessor.GetAsciiStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
var TempBlob: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  Result := nil;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    TempBlob := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stUnicodeStream, stAsciiStream, stBinaryStream] then
      if (TempBlob^ <> nil) and not TempBlob^.IsEmpty then
        Result := TempBlob^.GetStream;
  end;
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
function TZRowAccessor.GetUnicodeStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
var TempBlob: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  {$R-}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    TempBlob := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stUnicodeStream, stAsciiStream, stBinaryStream] then
      if (TempBlob^ <> nil) and not TempBlob^.IsEmpty then
        if TempBlob^.IsClob then
          Result := TempBlob^.GetUnicodeStream
        else
          Result := TempBlob^.GetStream
  end;
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
function TZRowAccessor.GetBinaryStream(ColumnIndex: Integer; out IsNull: Boolean): TStream;
var TempBlob: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  Result := nil;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    TempBlob := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stUnicodeStream, stAsciiStream, stBinaryStream] then
      if (TempBlob^ <> nil) and not TempBlob^.IsEmpty then
        Result := TempBlob^.GetStream;
  end;
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
function TZRowAccessor.GetBlob(ColumnIndex: Integer; out IsNull: Boolean): IZBlob;
var TempBlob: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  Result := nil;
  {$R-}
  TempBlob := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in [stUnicodeStream, stAsciiStream, stBinaryStream] then begin
    if (TempBlob^ <> nil) then
      Result := TempBlob^
    else if (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stBinaryStream)
      then Result := TZAbstractBlob.CreateWithStream(nil)
      else Result := TZAbstractClob.CreateWithData(nil, 0, FClientCP, ConSettings);
  end else
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>ResultSet</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>ResultSet</code> object representing the SQL
    <code>ResultSet</code> value in the specified column
}
function TZRowAccessor.GetDataSet(ColumnIndex: Integer; out IsNull: Boolean): IZDataSet;
begin
  Result := nil;
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
procedure TZRowAccessor.GetDate(ColumnIndex: Integer; out IsNull: Boolean;
  var Result: TZDate);
var
  Data: Pointer;
  Len: NativeUInt;
label Fill, Dbl;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    IsNull := False;
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stTime: goto Fill;
      stDate: Result := PZDate(Data)^;
      stTimestamp: begin
          Result.Year := PZTimeStamp(Data).Year;
          Result.Month := PZTimeStamp(Data).Month;
          Result.Day := PZTimeStamp(Data).Day;
          Result.IsNegative := PZTimeStamp(Data).IsNegative;
        end;
      stString, stUnicodeString, stAsciiStream, stUnicodeStream: begin
          if fRaw then begin
            Data := GetPAnsiChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToDate(PAnsiChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end else begin
            Data := GetPWideChar(ColumnIndex, IsNull, Len);
            IsNull := not TryPCharToDate(PWideChar(Data), Len, ConSettings^.ReadFormatSettings, Result);
          end;
          if IsNull then goto Dbl;
        end;
      else
Dbl:    DecodeDateTimeToDate(GetDouble(ColumnIndex, IsNull), Result);
    end;
  end else begin
    IsNull := True;
Fill: PInt64(@Result.Year)^ := 0
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> value.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
}
function TZRowAccessor.GetValue(ColumnIndex: Integer): TZVariant;
var
  ValuePtr: Pointer;
  IsNull: Boolean;
begin
  IsNull := False;
  {$R-}
  if FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull then begin
    ValuePtr := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stByte:       Result := EncodeUInteger(PByte(ValuePtr)^);
      stShort:      Result := EncodeInteger(PShortInt(ValuePtr)^);
      stWord:       Result := EncodeUInteger(PWord(ValuePtr)^);
      stSmall:      Result := EncodeInteger(PSmallInt(ValuePtr)^);
      stLongWord:   Result := EncodeUInteger(PCardinal(ValuePtr)^);
      stInteger:    Result := EncodeInteger(PInteger(ValuePtr)^);
      stULong:      Result := EncodeUInteger(PUInt64(ValuePtr)^);
      stLong:       Result := EncodeInteger(PInt64(ValuePtr)^);
      stFloat:      Result := EncodeDouble(PSingle(ValuePtr)^);
      stDouble:     Result := EncodeDouble(PDouble(ValuePtr)^);
      stCurrency:   Result := EncodeCurrency(PCurrency(ValuePtr)^);
      stBigDecimal: Result := EncodeBigDecimal(PBCD(ValuePtr)^);
      stBoolean:    Result := EncodeBoolean(PWordBool(ValuePtr)^);
      stDate:       Result := EncodeZDate(PZDate(ValuePtr)^);
      stTime:       Result := EncodeZTime(PZTime(ValuePtr)^);
      stTimestamp:  Result := EncodeZTimeStamp(PZTimeStamp(ValuePtr)^);
      stString,
      stAsciiStream:Result := EncodeString(GetString(ColumnIndex, IsNull));
      stUnicodeString,
      stUnicodeStream: Result := EncodeUnicodeString(GetUnicodeString(ColumnIndex, IsNull));
      stBytes, stBinaryStream: Result := EncodeBytes(GetBytes(ColumnIndex, IsNull));
      stGUID:       Result := EncodeGUID(PGUID(ValuePtr)^);
      else
        Result.VType := vtNull;
    end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  end
  else
    Result.VType := vtNull;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a nullable column a not null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNotNull(ColumnIndex: Integer);
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  {$R-}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNull) then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stAsciiStream, stBinaryStream, stUnicodeStream:
        if PIZLob(Data)^ = nil
        then if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stBinaryStream
          then PIZLob(Data)^ := TZAbstractBlob.Create
          else PIZLob(Data)^ := TZAbstractClob.CreateWithData(PEmptyUnicodeString, 0, Consettings);
    end;
  end;
end;

{**
  Gives a nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNull(ColumnIndex: Integer);
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  {$R-}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] = bIsNotNull) then begin
    Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
    FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNull;
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
      stAsciiStream, stBinaryStream, stUnicodeStream:
          PIZLob(Data)^ := nil;
      stBytes, stString, stUnicodeString:
        if Data^ <> nil then begin
          System.FreeMem(Data^);
          Data^ := nil;
        end;
    end;
  end;
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
procedure TZRowAccessor.SetBoolean(ColumnIndex: Integer; Value: Boolean);
var Data: PPointer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value;
    stByte: PByte(Data)^ := Ord(Value);
    stShort: PShortInt(Data)^ := Ord(Value);
    stWord: PWord(Data)^ := Ord(Value);
    stSmall: PSmallInt(Data)^ := Ord(Value);
    stLongWord: PCardinal(Data)^ := Ord(Value);
    stInteger: PInteger(Data)^ := Ord(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := Ord(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(Data)^ := Ord(Value);
    stFloat: PSingle(Data)^ := Ord(Value);
    stDouble: PDouble(Data)^ := Ord(Value);
    stCurrency: PCurrency(Data)^ := Ord(Value);
    stBigDecimal: ScaledOrdinal2BCD(Word(Value), 0, PBCD(Data)^);
    stString, stUnicodeString: SetString(ColumnIndex, BoolStrs[Value]);
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
procedure TZRowAccessor.SetByte(ColumnIndex: Integer; Value: Byte);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  InternalSetInt(ColumnIndex, Value);
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
procedure TZRowAccessor.SetShort(ColumnIndex: Integer; Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  InternalSetInt(ColumnIndex, Value);
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
procedure TZRowAccessor.SetWord(ColumnIndex: Integer; Value: Word);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  InternalSetInt(ColumnIndex, Value);
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
procedure TZRowAccessor.SetSmall(ColumnIndex: Integer; Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  InternalSetInt(ColumnIndex, Value);
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
procedure TZRowAccessor.SetUInt(ColumnIndex: Integer; Value: Cardinal);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
  InternalSetULong(ColumnIndex, Value);
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
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
procedure TZRowAccessor.SetInt(ColumnIndex: Integer; Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  InternalSetInt(ColumnIndex, Value);
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
procedure TZRowAccessor.SetULong(ColumnIndex: Integer; const Value: UInt64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  InternalSetULong(ColumnIndex, Value);
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
procedure TZRowAccessor.SetLong(ColumnIndex: Integer; const Value: Int64);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte: PByte(Data)^ := Value;
    stShort: PShortInt(Data)^ := Value;
    stWord: PWord(Data)^ := Value;
    stSmall: PSmallInt(Data)^ := Value;
    stLongWord: PCardinal(Data)^ := Value;
    stInteger: PInteger(Data)^ := Value;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := Value;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(Data)^ := Value;
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: ScaledOrdinal2BCD(Value, 0, PBCD(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      if fRaw then begin
        IntToRaw(Value, @TinyBuffer[0], @Data);
        L := PAnsiChar(Data) - PAnsiChar(@TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        IntToUnicode(Value, @TinyBuffer[0], @Data);
        L := PWideChar(Data) - PWideChar(@TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
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
procedure TZRowAccessor.SetFloat(ColumnIndex: Integer; Value: Single);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong,
    stLong: SetLong(ColumnIndex, {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value));
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: Double2BCD(Value,PBCD(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: begin
      if fRaw then begin
        L := FloatToSQLRaw(Value, @TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        L := FloatToSQLUnicode(Value, @TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
    end;
  end;
end;

procedure TZRowAccessor.SetGUID(ColumnIndex: Integer; const Value: TGUID);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stGUID: PGUID(Data)^ := Value;
    stBytes: InternalSetBytes(Data, @Value.D1, 16);
    stBinaryStream: if Data^ = nil
        then PIZLob(Data)^ := TZAbstractBlob.CreateWithData(@Value.D1, 16)
        else PIZLob(Data)^.SetBuffer(@Value.D1, 16);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: begin
        L := 38;
        if fRaw then begin
          GUIDToBuffer(@Value.D1, PAnsiChar(@TinyBuffer[0]), [guidWithBrackets]);
          SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
        end else begin
          GUIDToBuffer(@Value.D1, PWideChar(@TinyBuffer[0]), [guidWithBrackets]);
          SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
        end;
    end else
      raise EZSQLException.Create(SConvertionIsNotPossible);
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
procedure TZRowAccessor.SetDouble(ColumnIndex: Integer; const Value: Double);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong,
    stLong: SetLong(ColumnIndex, {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value));
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: Double2BCD(Value,PBCD(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      if fRaw then begin
        L := FloatToSQLRaw(Value, @TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        L := FloatToSQLUnicode(Value, @TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
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
procedure TZRowAccessor.SetCurrency(ColumnIndex: Integer; const Value: Currency);
var Data: PPointer;
  I64: Int64 absolute Value;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := Value <> 0;
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong,
    stLong: SetLong(ColumnIndex, I64 div 10000);
    stFloat: PSingle(Data)^ := Value;
    stDouble: PDouble(Data)^ := Value;
    stCurrency: PCurrency(Data)^ := Value;
    stBigDecimal: Currency2Bcd(Value, PBCD(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      if fRaw then begin
        CurrToRaw(Value, @TinyBuffer[0], @Data);
        L := PAnsiChar(Data)-PAnsiChar(@TinyBuffer[0]);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        CurrToUnicode(Value, @TinyBuffer[0], @Data);
        L := PWideChar(Data)-PWideChar(@TinyBuffer[0]);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
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
procedure TZRowAccessor.SetBigDecimal(ColumnIndex: Integer; const Value: TBCD);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := BcdCompare(Value, NullBCD) <> 0;
    stByte: PByte(Data)^ := BCDToInteger(Value);
    stShort: PShortInt(Data)^ := BCDToInteger(Value);
    stWord: PWord(Data)^ := BCDToInteger(Value);
    stSmall: PSmallInt(Data)^ := BCDToInteger(Value);
    stLongWord: PCardinal(Data)^ := Bcd2UInt64(Value);
    stInteger: PInteger(Data)^ := BCD2Int64(Value);
    stLong: PInt64(Data)^ := Bcd2Int64(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := Bcd2UInt64(Value);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stFloat: PSingle(Data)^ := BcdToDouble(Value);
    stDouble: PDouble(Data)^ := BcdToDouble(Value);
    stCurrency: BCDToCurr(Value, PCurrency(Data)^);
    stBigDecimal: PBCD(Data)^ := Value;
    stString, stUnicodeString:
      if fRaw then begin
        L := ZSysUtils.BcdToRaw(Value, @TinyBuffer[0], '.');
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], L);
      end else begin
        L := ZSysUtils.BcdToUni(Value, @TinyBuffer[0], '.');
        SetPWideChar(ColumnIndex, @TinyBuffer[0], L);
      end;
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
procedure TZRowAccessor.SetString(ColumnIndex: Integer; const Value: String);
var Len: NativeUInt;
begin
  {$IFDEF UNICODE}
  Len := Length(Value);
  SetPWideChar(ColumnIndex, Pointer(Value), Len);
  {$ELSE}
  if (fRaw and not ConSettings.AutoEncode) or
     not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in
          [stString, stUnicodeString, stAsciiStream, stUnicodeStream])
  then begin
    Len := Length(Value);
    SetPAnsiChar(ColumnIndex, Pointer(Value), Len)
  end else if fRaw then begin
    fRawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, FClientCP);
    Len := Length(fRawTemp);
    SetPAnsiChar(ColumnIndex, Pointer(fRawTemp), Len);
  end else begin
    fUniTemp := ConSettings^.ConvFuncs.ZStringToUnicode(Value, ConSettings^.CTRL_CP); //localize Value becuse of WideString overrun
    Len := Length(fUniTemp);
    SetPWideChar(ColumnIndex, Pointer(fUniTemp), Len);
  end;
  {$ENDIF}
end;

{**
  Sets the designated column with a <code>PAnsiChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
  @param Len then length of the String in bytes
}
procedure TZRowAccessor.SetPAnsiChar(ColumnIndex: Integer; Value: PAnsichar;
  var Len: NativeUInt);
var
  Data: PPointer;
  TS: TZTimeStamp;
begin
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, False);
    stByte, stShort, stWord, stSmall,
    stInteger: SetInt(ColumnIndex, RawToInt64Def(Value, Value+Len, 0));
    stLongWord: PCardinal(Data)^ := RawToInt64Def(Value, 0);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := RawToInt64Def(Value, 0);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(Data)^ := RawToInt64Def(Value, 0);
    stFloat: SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
    stDouble: SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
    stCurrency: SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
    stBigDecimal: ZSysUtils.TryRawToBcd(Value, Len, PBCD(Data)^, '.');
    stString, stUnicodeString: if fRaw then
        InternalSetPAnsiChar(Data, Value, Len)
      else begin
        fUniTemp := PRawToUnicode(Value, Len, FClientCP); //localize because of WideString overrun
        InternalSetPWideChar(Data, Pointer(fUniTemp), Length(fUniTemp));
      end;
    stBytes: InternalSetBytes(Data, Value, len);
    stGUID:
      if (Value <> nil) and ((Len = 36) or (Len = 38))
        then ValidGUIDToBinary(Value, PAnsiChar(Data))
        else SetNull(ColumnIndex);
    stDate: if not ZSysUtils.TryRawToDate(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZDate(Data)^) then
              if ZSysUtils.TryRawToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, TS{%H-}) then begin
                PZDate(Data)^.Year := TS.Year;
                PZDate(Data)^.Month := TS.Month;
                PZDate(Data)^.Day := TS.Day;
                PZDate(Data)^.IsNegative := TS.IsNegative;
              end else
                PInt64(Data)^ := 0;
    stTime: if not ZSysUtils.TryRawToTime(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZTime(Data)^) then
              if ZSysUtils.TryRawToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, TS) then begin
                PZTime(Data)^ := PZTime(@TS.Hour)^;
                PZTime(Data)^.IsNegative := False;
              end;
    stTimestamp: ZSysUtils.TryRawToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZTimeStamp(Data)^);
    stUnicodeStream, stAsciiStream:
      if (Data^ = nil) or not PIZLob(Data^)^.IsClob
      then PIZLob(Data)^ := TZAbstractCLob.CreateWithData(Value, Len, FClientCP, ConSettings)
      else PIZLob(Data)^.SetPAnsiChar(Value, FClientCP, Len);
    stBinaryStream:
      if (Data^ = nil)
      then PIZLob(Data)^ := TZAbstractBLob.CreateWithData(Value, Len)
      else PIZLob(Data)^.SetBuffer(Value, Len);
  end;
end;

{**
  Sets the designated column with a <code>PWideChar</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the new column value
  @param Len the length of the string in codepoints
}
procedure TZRowAccessor.SetPWideChar(ColumnIndex: Integer;
  Value: PWideChar; var Len: NativeUInt);
var
  Data: PPointer;
  TS: TZTimeStamp;
begin
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stBoolean: PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, False);
    stByte, stShort, stWord, stSmall,
    stInteger: SetInt(ColumnIndex, UnicodeToIntDef(Value, Value+Len, 0));
    stLongWord: PCardinal(Data)^ := UnicodeToInt64Def(Value, Value+Len, 0);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    stULong: PUInt64(Data)^ := UnicodeToInt64Def(Value, Value+Len, 0);
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    stLong: PInt64(Data)^ := UnicodeToInt64Def(Value, Value+Len, 0);
    stFloat: SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
    stDouble: SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
    stCurrency: SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
    stBigDecimal: ZSysUtils.TryUniToBcd(Value, Len, PBCD(Data)^, '.');
    stUnicodeString, stString: if fRaw
      then InternalSetString(Data, PUnicodeToRaw(Value, Len, FClientCP))
      else InternalSetPWideChar(Data, Value, Len);
    stAsciiStream, stUnicodeStream:
      if (Data^ = nil) or not (PIZLob(Data^)^.IsClob)
      then PIZLob(Data)^ := TZAbstractCLob.CreateWithData(Value, Len, ConSettings)
      else PIZLob(Data)^.SetPWideChar(Value, Len);
    stBytes, stBinaryStream: begin
        fRawTemp := UnicodeStringToAscii7(Value, Len);
        SetBytes(ColumnIndex, Pointer(fRawTemp), Length(fRawTemp));
      end;
    stGUID:
      if (Value <> nil) and ((Len = 36) or (Len = 38))
        then ValidGUIDToBinary(Value, PAnsiChar(Data))
        else SetNull(ColumnIndex);
    stDate: if not ZSysUtils.TryUniToDate(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZDate(Data)^) then
              if ZSysUtils.TryUniToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, TS{%H-}) then begin
                PZDate(Data)^.Year := TS.Year;
                PZDate(Data)^.Month := TS.Month;
                PZDate(Data)^.Day := TS.Day;
                PZDate(Data)^.IsNegative := TS.IsNegative;
              end;
    stTime: if not ZSysUtils.TryUniToTime(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZTime(Data)^) then
              if ZSysUtils.TryUniToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, TS) then begin
                PZTime(Data)^ := PZTime(@TS.Hour)^;
                PZTime(Data)^.IsNegative := False;
              end;
    stTimestamp: ZSysUtils.TryUniToTimeStamp(Value, Len, ConSettings^.ReadFormatSettings.DateFormat, PZTimeStamp(Data)^);
    else
      raise EZSQLException.Create(SConvertionIsNotPossible);
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
{$IFNDEF NO_ANSISTRING}
procedure TZRowAccessor.SetAnsiString(ColumnIndex: Integer; const Value: AnsiString);
var Len: NativeUInt;
begin
  if (fRaw and (FClientCP = ZOSCodePage)) or
     not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in
          [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
    Len := Length(Value);
    SetPAnsiChar(ColumnIndex, Pointer(Value), Len)
  end else if fRaw then begin
    FUniTemp := PRawToUnicode(Pointer(Value), Length(Value), ZOSCodePage);
    fRawTemp := ZUnicodeToRaw(FUniTemp, FClientCP);
    Len := Length(fRawTemp);
    SetPAnsiChar(ColumnIndex, Pointer(fRawTemp), Len);
  end else begin
    fUniTemp := ZRawToUnicode(Value, ZOSCodePage); //localize Value becuse of WideString overrun
    Len := Length(fUniTemp);
    SetPWideChar(ColumnIndex, Pointer(fUniTemp), Len);
  end;
end;
{$ENDIF}

{**
  Sets the designated column with a <code>UTF8String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZRowAccessor.SetUTF8String(ColumnIndex: Integer; const Value: UTF8String);
var Len: NativeUInt;
begin
  if (fRaw and (FClientCP = zCP_UTF8)) or
     not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] in
          [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
    Len := Length(Value);
    SetPAnsiChar(ColumnIndex, Pointer(Value), Len)
  end else if fRaw then begin
    ZEncoding.PRawToRawConvert(Pointer(Value), Length(Value), zCP_UTF8, FClientCP, fRawTemp);
    Len := Length(fRawTemp);
    SetPAnsiChar(ColumnIndex, Pointer(fRawTemp), Len);
  end else begin
    fUniTemp := ZRawToUnicode(Value, zCP_UTF8); //localize Value becuse of WideString overrun
    Len := Length(fUniTemp);
    SetPWideChar(ColumnIndex, Pointer(fUniTemp), Len);
  end;
end;
{$ENDIF}

{**
  Sets the designated column with a <code>RawByteString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetRawByteString(ColumnIndex: Integer; const Value: RawByteString);
var Len: NativeUInt;
begin
  Len := Length(Value);
  SetPAnsiChar(ColumnIndex, Pointer(Value), Len);
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
procedure TZRowAccessor.SetUnicodeString(ColumnIndex: Integer; const Value: ZWideString);
var Len: NativeUInt;
begin
  Len := Length(Value);
  SetPWideChar(ColumnIndex, Pointer(Value), Len);
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
procedure TZRowAccessor.SetBytes(ColumnIndex: Integer; const Value: TBytes);
begin
  SetBytes(ColumnIndex, Pointer(Value), Length(Value));
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
procedure TZRowAccessor.SetBytes(ColumnIndex: Integer; Buf: Pointer; Len: Word);
var Data: PPointer;
  L: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stGUID:
      if Len = 16 then
        PGUID(Data)^ := PGUID(Buf)^
      else
        SetNull(ColumnIndex);
    stBytes: InternalSetBytes(Data, Buf, Len);
    stBinaryStream:
        if Data^ = nil then
          PIZLob(Data)^ := TZAbstractBlob.CreateWithData(Buf, Len)
        else
          PIZLob(Data)^.SetBuffer(Buf, Len);
    stString, stUnicodeString: begin
        L := Len;
        SetPAnsiChar(ColumnIndex, Buf, L);
      end;
    else
      raise EZSQLException.Create(SConvertionIsNotPossible);
  end;
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
procedure TZRowAccessor.SetDate(ColumnIndex: Integer; const Value: TZDate);
var Data: PPointer;
  Len: NativeUInt;
  DT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stTime: if SizeOf(TZTime) = 12 then begin
              PCardinal(Data)^ := 0;
              PInt64(@PZTime(Data).Second)^ := 0;
            end else FillChar(Data^, SizeOf(TZTime), #0);
    stDate: PZDate(Data)^ := Value;
    stTimestamp: TimeStampFromDate(Value, PZTimeStamp(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: if fRaw then begin
        Len := DateToRaw(Value.Year, Value.Month, Value.Day, @TinyBuffer[0],
          ConSettings^.ReadFormatSettings.DateFormat, False, Value.IsNegative);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], Len);
      end else begin
        Len := DateToUni(Value.Year, Value.Month, Value.Day, @TinyBuffer[0],
          ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], Len);
      end;
    else if TryDateToDateTime(Value, DT{%H-})
      then SetDouble(ColumnIndex, DT)
      else SetNull(ColumnIndex);
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
procedure TZRowAccessor.SetTime(ColumnIndex: Integer; const Value: TZTime);
var Data: PPointer;
  Len: NativeUInt;
  DT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stDate: if SizeOf(TZDate) = SizeOf(Int64)
            then PInt64(Data)^ := 0
            else FillChar(Data^, SizeOf(TZDate), #0);
    stTime: PZTime(Data)^ := Value;
    stTimestamp: TimeStampFromTime(Value, PZTimeStamp(Data)^);
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: if fRaw then begin
        Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
          @TinyBuffer[0], ConSettings^.ReadFormatSettings.TimeFormat, False, Value.IsNegative);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], Len);
      end else begin
        Len := TimeToUni(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
          @TinyBuffer[0], ConSettings^.ReadFormatSettings.TimeFormat, False, Value.IsNegative);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], Len);
      end;
    else if TryTimeToDateTime(Value, DT{%H-})
      then SetDouble(ColumnIndex, DT)
      else SetNull(ColumnIndex);
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
procedure TZRowAccessor.SetTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp);
var Data: PPointer;
  Len: NativeUInt;
  DT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$R-}
  FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stDate: DateFromTimeStamp(Value, PZDate(Data)^);
    stTime: TimeFromTimeStamp(Value, PZTime(Data)^);
    stTimestamp: PZTimeStamp(Data)^ := Value;
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: if fRaw then begin
        Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
          Value.Hour, Value.Minute, Value.Second, Value.Fractions,
          @TinyBuffer[0], ConSettings^.ReadFormatSettings.DateTimeFormat, False, Value.IsNegative);
        SetPAnsiChar(ColumnIndex, @TinyBuffer[0], Len);
      end else begin
        Len := DateTimeToUni(Value.Year, Value.Month, Value.Day,
          Value.Hour, Value.Minute, Value.Second, Value.Fractions,
          @TinyBuffer[0], ConSettings^.ReadFormatSettings.DateTimeFormat, False, Value.IsNegative);
        SetPWideChar(ColumnIndex, @TinyBuffer[0], Len);
      end;
    else if TryTimeStampToDateTime(Value, DT{%H-})
      then SetDouble(ColumnIndex, DT)
      else SetNull(ColumnIndex);
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
procedure TZRowAccessor.SetAsciiStream(ColumnIndex: Integer; const Value: TStream);
var Data: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  {$R-}
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stAsciiStream, stUnicodeStream, stBinaryStream: begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        if Data^ = nil then
          if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stBinaryStream
          then Data^ := TZAbstractBlob.Create
          else  Data^ := TZAbstractClob.CreateWithData(nil, 0, ConSettings);
        if Data^.IsClob then
          if ConSettings^.AutoEncode
          then Data^.SetStream(Value)
          else Data^.SetStream(Value, FClientCP)
        else Data^.SetStream(Value);
      end;
    else
      raise EZSQLException.Create( Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
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
procedure TZRowAccessor.SetBinaryStream(ColumnIndex: Integer; const Value: TStream);
var Data: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  {$R-}
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stAsciiStream, stUnicodeStream, stBinaryStream: begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        if Data^ = nil
        then Data^ := TZAbstractBlob.CreateWithStream(Value)
        else Data^.SetStream(Value);
      end;
    else
      raise EZSQLException.Create( Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
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
procedure TZRowAccessor.SetUnicodeStream(ColumnIndex: Integer;
  const Value: TStream);
var Data: PIZLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  {$R-}
  Data := @FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1];
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stAsciiStream, stUnicodeStream, stBinaryStream: begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
        {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        if Data^ = nil then
          if FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stBinaryStream
          then Data^ := TZAbstractBlob.Create
          else  Data^ := TZAbstractClob.CreateWithData(nil, 0, ConSettings);
        if Data^.IsClob
        then Data^.SetStream(Value, zCP_UTF16)
        else Data^.SetStream(Value);
      end;
    else
      raise EZSQLException.Create( Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a blob wrapper object to be set.
}
procedure TZRowAccessor.SetBlob(ColumnIndex: Integer; const Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
{$ENDIF}
  {$R-}
  case FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] of
    stAsciiStream, stUnicodeStream, stBinaryStream: begin
        PIZLob(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1])^ := Value;
        if Value = nil
        then FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNull
        else FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]] := bIsNotNull;
      end;
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    else
      raise EZSQLException.Create( Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a ResultSet wrapper object to be set.
}
procedure TZRowAccessor.SetDataSet(ColumnIndex: Integer; const Value: IZDataSet);
var
  Ptr: PPointer;
  NullPtr: {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF};
begin
{$R-}
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] = stDataSet) then begin
    raise EZSQLException.Create(
      Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}])]));
  end;
{$ENDIF}

  Ptr := PPointer(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}] + 1]);
  NullPtr := {$IFDEF WIN64}PBoolean{$ELSE}PByte{$ENDIF}(@FBuffer.Columns[FColumnOffsets[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]]);
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}

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
procedure TZRowAccessor.SetValue(ColumnIndex: Integer; const Value: TZVariant);
var
  Len: NativeUInt;
  Lob: IZBLob;
  TS: TZTimeStamp;
begin
  case Value.VType of
    vtNull: SetNull(ColumnIndex);
    vtBoolean: SetBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: SetLong(ColumnIndex, Value.VInteger);
    vtUInteger: SetULong(ColumnIndex, Value.VUInteger);
    vtBigDecimal: SetBigDecimal(ColumnIndex, Value.VBigDecimal);
    vtGUID:  SetGUID(ColumnIndex, Value.VGUID);
    vtBytes: begin
              Len := Length(Value.VRawByteString);
              SetPAnsichar(ColumnIndex, Pointer(Value.VRawByteString), Len);
            end;
    vtString: SetString(ColumnIndex, Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: SetAnsiString(ColumnIndex, Value.VRawByteString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: SetUTF8String(ColumnIndex, Value.VRawByteString);
    {$ENDIF}
    vtRawByteString: SetRawByteString(ColumnIndex, Value.VRawByteString);
    vtUnicodeString: SetUnicodeString(ColumnIndex, Value.VUnicodeString);
    vtDate: SetDate(ColumnIndex, Value.VDate);
    vtDateTime: begin
        DecodeDateTimeToTimeStamp(Value.VDateTime, TS{%H-});
        SetTimestamp(ColumnIndex, TS);
      end;
    vtTime: SetTime(ColumnIndex, Value.VTime);
    vtTimeStamp: SetTimeStamp(ColumnIndex, Value.VTimeStamp);
    vtInterface: if Value.VInterface.QueryInterface(IZBLob, Lob) = S_OK then
                  SetBlob(ColumnIndex, Lob);
    vtCharRec:
      if ZCompatibleCodePages(zCP_UTF16, Value.VCharRec.CP) then begin
        Len := Value.VCharRec.Len;
        SetPWideChar(ColumnIndex, Value.VCharRec.P, Len);
      end else if ZCompatibleCodePages(FClientCP, Value.VCharRec.CP) then begin
        Len := Value.VCharRec.Len;
        SetPAnsiChar(ColumnIndex, Value.VCharRec.P, Len)
      end else
        SetUnicodeString(ColumnIndex, PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
  end;
end;

end.
