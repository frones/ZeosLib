{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for Caching Classes               }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestDbcCache;

interface

{$I ZDbc.inc}

uses
{$IFDEF VER120BELOW}
  DateUtils,
{$ENDIF}
  Contnrs, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcCache, {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZSysUtils,
  ZDbcIntfs, SysUtils, Classes, ZDbcResultSetMetadata,
  ZCompatibility, ZTestCase, FmtBCD;

type

  {** Implements a test case for TZRowAccessor. }
  TZTestRowAccessorCase = class(TZGenericTestCase)
  private
    FRowAccessor: TZRowAccessor;
    FBoolean: Boolean;
    FByte: Byte;
    FShort: ShortInt;
    FSmall: SmallInt;
    FInt: Integer;
    FLong: LongInt;
    FFloat: Single;
    FDouble: Double;
    FBigDecimal: TBCD;
    FString: string;
    FDate: TZDate;
    FTime: TZTime;
    FTimeStamp: TZTimeStamp;
    FAsciiStream: TStream;
    FUnicodeStream: TStream;
    FBinaryStream: TStream;
    FByteArray: TBytes;
    FAsciiStreamData: Ansistring;
    FUnicodeStreamData: WideString;
    FBinaryStreamData: Pointer;
    FConSettings: TZConSettings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetColumnsInfo(Index: Integer; ColumnType: TZSqlType;
      Nullable: TZColumnNullableType; ReadOnly: Boolean;
      Writable: Boolean): TZColumnInfo;
    function GetColumnsInfoCollection: TObjectList;
    function GetRowAccessor: TZRowAccessor;
    procedure FillRowAccessor(RowAccessor: TZRowAccessor);
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
  published
    procedure TestFillRowAccessor;
    procedure TestRowAccesorNull;
    procedure TestRowAccessorBoolean;
    procedure TestRowAccessorByte;
    procedure TestRowAccessorShort;
    procedure TestRowAccessorSmall;
    procedure TestRowAccessorInteger;
    procedure TestRowAccessorLong;
    procedure TestRowAccessorFloat;
    procedure TestRowAccessorDouble;
    procedure TestRowAccessorBigDecimal;
    procedure TestRowAccessorDate;
    procedure TestRowAccessorTime;
    procedure TestRowAccessorTimestamp;
    procedure TestRowAccessorString;
    procedure TestRowAccessor;
    procedure TestRowAccessorBytes;
    procedure TestRowAccesorBlob;
    procedure TestRowAccessorAsciiStream;
    procedure TestRowAccessorUnicodeStream;
    procedure TestRowAccessorBinaryStream;
    procedure TestRowAccessorReadonly;
  end;

implementation

uses ZTestConsts;

const
  stBooleanIndex        = FirstDbcIndex + 0;
  stByteIndex           = FirstDbcIndex + 1;
  stShortIndex          = FirstDbcIndex + 2;
  stSmallIndex          = FirstDbcIndex + 3;
  stIntegerIndex        = FirstDbcIndex + 4;
  stLongIndex           = FirstDbcIndex + 5;
  stFloatIndex          = FirstDbcIndex + 6;
  stDoubleIndex         = FirstDbcIndex + 7;
  stBigDecimalIndex     = FirstDbcIndex + 8;
  stStringIndex         = FirstDbcIndex + 9;
  stBytesIndex          = FirstDbcIndex + 10;
  stDateIndex           = FirstDbcIndex + 11;
  stTimeIndex           = FirstDbcIndex + 12;
  stTimestampIndex      = FirstDbcIndex + 13;
  stAsciiStreamIndex    = FirstDbcIndex + 14; // stream indexes
  stUnicodeStreamIndex  = FirstDbcIndex + 15; //   must be kept
  stBinaryStreamIndex   = FirstDbcIndex + 16; //   together

  FirstIndex = stBooleanIndex;
  LastIndex = stBinaryStreamIndex;

  FieldTypes: array[FirstIndex..LastIndex] of string =
  (
    'Boolean',
    'Byte',
    'Short',
    'Small',
    'Integer',
    'Long',
    'Float',
    'Double',
    'BigDecimal',
    'String',
    'Bytes',
    'Date',
    'Time',
    'Timestamp',
    'AsciiStream',
    'UnicodeStream',
    'BinaryStream'
  );

{ TZTestRowAccessorCase }

function TZTestRowAccessorCase.GetColumnsInfo(Index: Integer;
  ColumnType: TZSqlType; Nullable: TZColumnNullableType; ReadOnly: Boolean;
  Writable: Boolean): TZColumnInfo;
begin
  Result := TZColumnInfo.Create;

  Result.AutoIncrement := True;
  Result.CaseSensitive := True;
  Result.Searchable := True;
  Result.Currency := True;
  Result.Nullable := Nullable;
  Result.Signed := True;
  Result.ColumnLabel := 'Test Label'+IntToStr(Index);
  Result.ColumnName := 'TestName'+IntToStr(Index);
  Result.SchemaName := 'TestSchemaName';
  case ColumnType of
    stString, stUnicodeString: Result.Precision := 255;
    stBytes: Result.Precision := 5;
  else
    Result.Precision := 0;
  end;
  Result.Scale := 5;
  Result.TableName := 'TestTableName';
  Result.CatalogName := 'TestCatalogName';
  Result.ColumnType := ColumnType;
  Result.ReadOnly := ReadOnly;
  Result.Writable := Writable;
  Result.DefinitelyWritable := Writable;
end;

{**
  Create IZCollection and fill it by ZColumnInfo objects
  @return the ColumnInfo object
}
function TZTestRowAccessorCase.GetColumnsInfoCollection: TObjectList;
begin
  Result := TObjectList.Create;
  with Result do
  begin
    Add(GetColumnsInfo(stBooleanIndex, stBoolean, ntNullable, False, True));
    Add(GetColumnsInfo(stByteIndex, stByte, ntNullable, False, True));
    Add(GetColumnsInfo(stShortIndex, stShort, ntNullable, False, True));
    Add(GetColumnsInfo(stSmallIndex, stSmall, ntNullable, False, True));
    Add(GetColumnsInfo(stIntegerIndex, stInteger, ntNullable, False, True));
    Add(GetColumnsInfo(stLongIndex, stLong, ntNullable, False, True));
    Add(GetColumnsInfo(stFloatIndex, stFloat, ntNullable, False, True));
    Add(GetColumnsInfo(stDoubleIndex, stDouble, ntNullable, False, True));
    Add(GetColumnsInfo(stBigDecimalIndex, stBigDecimal, ntNullable, False, True));
    Add(GetColumnsInfo(stStringIndex, stString, ntNullable, False, True));
    Add(GetColumnsInfo(stBytesIndex, stBytes, ntNullable, False, True));
    Add(GetColumnsInfo(stDateIndex, stDate, ntNullable, False, True));
    Add(GetColumnsInfo(stTimeIndex, stTime, ntNullable, False, True));
    Add(GetColumnsInfo(stTimestampIndex, stTimestamp, ntNullable, False, True));
    Add(GetColumnsInfo(stAsciiStreamIndex, stAsciiStream, ntNullable, False, True));
    Add(GetColumnsInfo(stUnicodeStreamIndex, stUnicodeStream, ntNullable, False, True));
    Add(GetColumnsInfo(stBinaryStreamIndex, stBinaryStream, ntNullable, False, True));
  end;
end;

{**
  Create TZRowAccessor object and allocate it buffer
  @return the TZRowAccessor object
}
function TZTestRowAccessorCase.GetRowAccessor: TZRowAccessor;
var
  ColumnsInfo: TObjectList;
begin
  ColumnsInfo := GetColumnsInfoCollection;
  try
    Result := TZRowAccessor.Create(ColumnsInfo, @FConSettings);  //dummy cp: Stringfield cp is inconsistent
    Result.Alloc;
  finally
    ColumnsInfo.Free;
  end;
end;

{**
  Setup paramters for test such as variables, stream datas and streams
}
procedure TZTestRowAccessorCase.SetUp;
begin
  DecodeDateTimeToDate(SysUtils.Date, FDate);
  DecodeDateTimeToTime(SysUtils.Time, FTime);
  DecodeDateTimeToTimeStamp(SysUtils.Now, FTimeStamp);

  FAsciiStreamData := 'Test Ascii Stream Data';
  FAsciiStream := StreamFromData(FAsciiStreamData);

  FUnicodeStreamData := 'Test Unicode Stream Data';
  FUnicodeStream := StreamFromData(FUnicodeStreamData);

  FBinaryStreamData := AllocMem(BINARY_BUFFER_SIZE);
  FillChar(FBinaryStreamData^, BINARY_BUFFER_SIZE, 55);
  FBinaryStream := StreamFromData(FBinaryStreamData, BINARY_BUFFER_SIZE);

  FBoolean := true;
  FByte := 255;
  FShort := 127;
  FSmall := 32767;
  FInt := 2147483647;
  FLong := 1147483647;
  FFloat := 3.4E-38;
  FDouble := 1.7E-308;
  FBigDecimal := StrToBCD('9223372036854775807');
  FString := '0123456789';

  SetLength(FByteArray, 5);
  FByteArray[0] := 0;
  FByteArray[1] := 1;
  FByteArray[2] := 2;
  FByteArray[3] := 3;
  FByteArray[4] := 4;

  FConSettings := ConSettingsDummy;
  FConSettings.DisplayFormatSettings.DateFormat := DefDateFormatYMD;
  FConSettings.ReadFormatSettings.DateFormat := DefDateFormatYMD;
  FConSettings.WriteFormatSettings.DateFormat := DefDateFormatYMD;

  FConSettings.DisplayFormatSettings.DateTimeFormat := DefDateFormatYMD + ' ' + DefTimeFormatMsecs;
  FConSettings.ReadFormatSettings.DateTimeFormat := DefDateFormatYMD + ' ' + DefTimeFormatMsecs;
  FConSettings.WriteFormatSettings.DateTimeFormat := DefDateFormatYMD + ' ' + DefTimeFormatMsecs;

  RowAccessor := GetRowAccessor;
  FillRowAccessor(RowAccessor);
end;

{**
  Free parameters for test such as stream datas and streams
}
procedure TZTestRowAccessorCase.TearDown;
begin
  RowAccessor.Dispose;
  RowAccessor.Free;
  RowAccessor := nil;

  FAsciiStream.Free;
  FUnicodeStream.Free;
  FBinaryStream.Free;
  FreeMem(FBinaryStreamData);
end;

{**
  Test for blob filed
}
procedure TZTestRowAccessorCase.TestRowAccesorBlob;
var
  Blob: IZBlob;
  WasNull: Boolean;
  Index: Integer;
begin
  with RowAccessor do
    for Index := stAsciiStreamIndex to stBinaryStreamIndex do
    begin
      Blob := GetBlob(Index, WasNull);
      CheckNotNull(Blob, 'Not Null blob from ' + FieldTypes[Index] + ' field');
      Check(not Blob.IsEmpty, 'Blob from ' + FieldTypes[Index] + ' empty');
      Blob := nil;
    end;
end;

{**
  Test for setup to null fields and check it on correspondence to null
}
procedure TZTestRowAccessorCase.TestRowAccesorNull;
var
  Index: Integer;
begin
  with RowAccessor do
    for Index := FirstIndex to LastIndex do
    begin
      Check(not IsNull(Index), 'Not Null ' + FieldTypes[Index] + ' column');

      try
        SetNull(Index);
      except
        Fail('Incorrect ' + FieldTypes[Index] + ' method behavior');
      end;

      Check(IsNull(Index), 'Null ' + FieldTypes[Index] + ' column');
    end;
end;

{**
  Test for general TestZRowAccessor functions
}
procedure TZTestRowAccessorCase.TestRowAccessor;
var
  RowBuffer1: PZRowBuffer;
  RowBuffer2: PZRowBuffer;
begin
  {$IFNDEF NO_COLUMN_LIMIT}
  RowBuffer1 := AllocMem(RowAccessor.RowSize);
  RowBuffer2 := AllocMem(RowAccessor.RowSize);
  {$ELSE}
  RowBuffer1 := RowAccessor.AllocBuffer;
  RowBuffer2 := RowAccessor.AllocBuffer;
  {$ENDIF}
  RowAccessor.InitBuffer(RowBuffer1);
  RowAccessor.InitBuffer(RowBuffer2);

  RowBuffer1^.Index := 100;
  RowBuffer1^.UpdateType := utModified;
  RowBuffer1^.BookmarkFlag := 2;

  with RowAccessor do
  begin
   {check Copy method}
    try
      RowAccessor.CopyBuffer(RowBuffer1, RowBuffer2);
    except
      Fail('Incorrect Copy method behavior');
    end;
    Check(Assigned(RowBuffer2),'Copy. The RowBuffer2 assigned )');
    CheckEquals(100, RowBuffer2^.Index, 'Copy. Buffer2 Index');
    CheckEquals(ord(utModified), ord(RowBuffer2^.UpdateType),
        'Copy. Buffer2 UpdateType');
    CheckEquals(2, RowBuffer2^.BookmarkFlag, 'Copy. Buffer2 BookmarkFlag');

    {check CopyTo method}
    try
      RowAccessor.CopyTo(RowBuffer1);
    except
      Fail('Incorrect CopyTo method behavior');
    end;
    Check(Assigned(RowBuffer1),'CopyTo. The RowBuffer1 assigned )');
    CheckEquals(stStringIndex, RowBuffer1^.Index, 'CopyTo. The RowBuffer1 Index');
    CheckEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'CopyTo. The RowBuffer1 UpdateType');
    CheckEquals(1, RowBuffer1^.BookmarkFlag,
        'CopyTo. The RowBuffer1 BookmarkFlag');

    {check Clear method}
    try
      RowAccessor.ClearBuffer(RowBuffer1);
    except
      Fail('Incorrect ClearBuffer method behavior');
    end;
    Check(Assigned(RowBuffer1),'Clear. The RowBuffer1 assigned )');
    CheckNotEquals(stStringIndex, RowBuffer1^.Index, 'Clear. The RowBuffer1 Index');
    CheckNotEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'Clear. The RowBuffer1 UpdateType');
    CheckNotEquals(1, RowBuffer1^.BookmarkFlag,
        'Clear. The RowBuffer1 BookmarkFlag');

    {check Moveto method}
    try
      RowAccessor.MoveTo(RowBuffer1);
    except
      Fail('Incorrect MoveTo method behavior');
    end;
    Check(Assigned(RowBuffer1), 'MoveTo. The RowBuffer1 assigned');
    Check(Assigned(RowBuffer1),'MoveTo. The RowBuffer1 assigned )');
    CheckEquals(stStringIndex, RowBuffer1^.Index, 'MoveTo. The RowBuffer1 Index');
    CheckEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'MoveTo. The RowBuffer1 UpdateType');
    CheckEquals(1, RowBuffer1^.BookmarkFlag,
        'MoveTo. The RowBuffer1 BookmarkFlag');

    CheckNotEquals(stStringIndex, RowBuffer^.Index, 'MoveTo. The RowBuffer Index');
    CheckNotEquals(ord(utInserted), ord(RowBuffer^.UpdateType),
        'MoveTo. The RowBuffer UpdateType');
    CheckNotEquals(1, RowBuffer^.BookmarkFlag,
        'MoveTo. The RowBuffer BookmarkFlag');

    {check CopyFrom method}
    try
      RowAccessor.CopyFrom(RowBuffer2);
    except
      Fail('Incorrect CopyFrom method behavior');
    end;
    CheckEquals(100, RowBuffer^.Index, 'CopyFrom. The RowBuffer2 Index');
    CheckEquals(ord(utModified), ord(RowBuffer^.UpdateType),
        'CopyFrom. The RowBuffer2 UpdateType');
    CheckEquals(2, RowBuffer^.BookmarkFlag,
        'CopyFrom. The RowBuffer2 BookmarkFlag');

    {check Clear method}
    try
      RowAccessor.Clear;
    except
      Fail('Incorrect Clear method behavior');
    end;
    Check(Assigned(RowAccessor.RowBuffer), 'Clear. The RowBuffer assigned');
    CheckNotEquals(stStringIndex, RowBuffer^.Index, 'Clear. The RowBuffer Index');
    CheckNotEquals(ord(utInserted), ord(RowBuffer^.UpdateType),
        'Clear. The RowBuffer UpdateType');
    CheckNotEquals(1, RowBuffer^.BookmarkFlag,
        'Clear. The RowBuffer BookmarkFlag');

    {check  dispose}
    try
      RowAccessor.Dispose;
    except
      Fail('Incorrect Dispose method behavior');
    end;
    Check(not Assigned(RowAccessor.RowBuffer), 'The not RowAccessor.RowBuffer assigned');
  end;

  RowAccessor.DisposeBuffer(RowBuffer1);
  RowAccessor.DisposeBuffer(RowBuffer2);
end;

procedure TZTestRowAccessorCase.TestRowAccessorAsciiStream;
var
  Stream: TStream;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    try
      Stream := GetAsciiStream(stAsciiStreamIndex, WasNull);
      CheckEquals(Stream, FAsciiStream, 'AsciiStream');
      Stream.Free;
    except
      Fail('Incorrect GetAsciiStream method behavior');
    end;
  end;
end;

{**
  Test for BigDecimal field
}
procedure TZTestRowAccessorCase.TestRowAccessorBigDecimal;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stBigDecimalIndex, WasNull), 'GetBoolean');
//    CheckEquals(FBigDecimal, GetFloat(stDoubleIndex, WasNull), 0.001, 'GetFloat');
//    CheckEquals(FBigDecimal, GetDouble(stDoubleIndex, WasNull), 0.001, 'GetDouble');
    GetBigDecimal(stBigDecimalIndex, BCD, WasNull);
    CheckEquals(0, BCDCompare(FBigDecimal, BCD), 'GetBigDecimal');
    CheckEquals(BCDToStr(FBigDecimal), GetString(stBigDecimalIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for BinaryStream field
}
procedure TZTestRowAccessorCase.TestRowAccessorBinaryStream;
var
  Stream: TStream;
  ReadNum: Integer;
  Buffer: array[0..BINARY_BUFFER_SIZE] of Byte;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    try
      Stream := GetBinaryStream(stBinaryStreamIndex, WasNull);
      CheckNotNull(Stream, 'BinaryStream');
      CheckEquals(Stream, FBinaryStream, 'BinaryStream');
      Stream.Position := 0;
      ReadNum := Stream.Read(Buffer, BINARY_BUFFER_SIZE);
      Stream.Free;
      CheckEquals(ReadNum, BINARY_BUFFER_SIZE);
      CheckEqualsMem(@Buffer, FBinaryStreamData, BINARY_BUFFER_SIZE);
    except
      Fail('Incorrect GetBinaryStream method behavior');
    end;
  end;
end;

{**
  Test for Boolean field
}
procedure TZTestRowAccessorCase.TestRowAccessorBoolean;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stBooleanIndex, WasNull), 'GetBoolean');
    CheckEquals(1, GetByte(stBooleanIndex, WasNull), 0, 'GetByte');
    CheckEquals(1, GetShort(stBooleanIndex, WasNull), 0, 'GetShort');
    CheckEquals(1, GetSmall(stBooleanIndex, WasNull), 0, 'GetSmall');
    CheckEquals(1, GetInt(stBooleanIndex, WasNull), 0, 'GetInt');
    CheckEquals(1, GetLong(stBooleanIndex, WasNull), 0, 'GetLong');
    CheckEquals(1, GetFloat(stBooleanIndex, WasNull), 0, 'GetFloat');
    CheckEquals(1, GetDouble(stBooleanIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stBooleanIndex, BCD, WasNull);
    CheckEquals(Integer(1), BCDToInteger(BCD), 'GetBigDecimal');
    CheckEquals('True', GetString(stBooleanIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for Byte field
}
procedure TZTestRowAccessorCase.TestRowAccessorByte;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stByteIndex, WasNull), 'GetBoolean');
    CheckEquals(FByte, GetByte(stByteIndex, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FByte), GetShort(stByteIndex, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FByte), GetSmall(stByteIndex, WasNull), 0, 'GetSmall');
    CheckEquals(FByte, GetInt(stByteIndex, WasNull), 0, 'GetInt');
    CheckEquals(FByte, GetLong(stByteIndex, WasNull), 0, 'GetLong');
    CheckEquals(FByte, GetFloat(stByteIndex, WasNull), 0, 'GetFloat');
    CheckEquals(FByte, GetDouble(stByteIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stByteIndex, BCD, WasNull);
    CheckEquals(Integer(FByte), BCDToInteger(BCD), 'GetBigDecimal');
    CheckEquals(IntToStr(FByte), GetString(stByteIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for Bytes field
}
procedure TZTestRowAccessorCase.TestRowAccessorBytes;

  function  ArrayToString(BytesArray: TBytes): string;
  var
    I: Integer;
  begin
    for I := 0 to High(BytesArray) do
       Result := Result + Char(BytesArray[I]);
  end;

var
  ByteArray: TBytes;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    ByteArray := GetBytes(stBytesIndex, WasNull);
    CheckNotEquals(0, High(ByteArray));
    CheckEquals(ArrayToString(FByteArray), GetString(stBytesIndex, WasNull),
      'strings from bytearray equals');
    CheckEquals(FByteArray, ByteArray);
  end;
end;

{**
  Test for Date field
}
procedure TZTestRowAccessorCase.TestRowAccessorDate;
var
  WasNull: Boolean;
  DT: TDateTime;
  TS: TZTimeStamp;
  D: TZDate;
begin
  with RowAccessor do
  begin
    TryDateToDateTime(FDate, DT);
    CheckEquals(FormatDateTime(ConSettings^.DisplayFormatSettings.DateFormat, DT),
      GetString(stDateIndex, WasNull), 'GetString');
    GetDate(stDateIndex, WasNull, D);
    Check(ZCompareDate(FDate, D)= 0, 'GetDate');
    GetTimestamp(stDateIndex, WasNull, TS);
    DateFromTimeStamp(Ts, D);
    Check(ZCompareDate(FDate, D)= 0, 'GetTimestamp');
  end;
end;

{**
  Test for Double field
}
procedure TZTestRowAccessorCase.TestRowAccessorDouble;
var
  WasNull: Boolean;
  BCD, BCD2: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stDoubleIndex, WasNull), 'GetBoolean');
    CheckEquals(Byte(Trunc(FDouble)), GetByte(stDoubleIndex, WasNull), 0, 'GetByte');
    CheckEquals(Trunc(FDouble), GetShort(stDoubleIndex, WasNull), 0, 'GetShort');
    CheckEquals(Trunc(FDouble), GetSmall(stDoubleIndex, WasNull), 0, 'GetSmall');
    CheckEquals(Trunc(FDouble), GetInt(stDoubleIndex, WasNull), 0, 'GetInt');
    CheckEquals(Trunc(FDouble), GetLong(stDoubleIndex, WasNull), 0, 'GetLong');
    CheckEquals(FDouble, GetFloat(stDoubleIndex, WasNull), 0.001, 'GetFloat');
    CheckEquals(FDouble, GetDouble(stDoubleIndex, WasNull), 0.001, 'GetDouble');
    GetBigDecimal(stDoubleIndex, BCD, WasNull);
    Double2BCD(FDouble, BCD2);
    CheckEquals(0, BCDCompare(BCD2, BCD), 'GetBigDecimal');
    CheckEquals(FloatToSQLStr(FDouble), GetString(stDoubleIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for fill all fileds by their values
}
procedure TZTestRowAccessorCase.TestFillRowAccessor;
var
  RowAccessor: TZRowAccessor;
begin
  RowAccessor := GetRowAccessor;
  FillRowAccessor(RowAccessor);
  RowAccessor.Dispose;
  RowAccessor.Free;
end;

{**
  Test for Float field
}
procedure TZTestRowAccessorCase.TestRowAccessorFloat;
var
  BCD, BCD2: TBCD;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stFloatIndex, WasNull), 'GetBoolean');
    CheckEquals(Trunc(FFloat), GetByte(stFloatIndex, WasNull), 0, 'GetByte');
    CheckEquals(Trunc(FFloat), GetShort(stFloatIndex, WasNull), 0, 'GetShort');
    CheckEquals(Trunc(FFloat), GetSmall(stFloatIndex, WasNull), 0, 'GetSmall');
    CheckEquals(Trunc(FFloat), GetInt(stFloatIndex, WasNull), 0, 'GetInt');
    CheckEquals(Trunc(FFloat), GetLong(stFloatIndex, WasNull), 0, 'GetLong');
    CheckEquals(FFloat, GetFloat(stFloatIndex, WasNull), 0.001, 'GetFloat');
    CheckEquals(FFloat, GetDouble(stFloatIndex, WasNull), 0.001, 'GetDouble');
    GetBigDecimal(stFloatIndex, BCD, WasNull);
    Double2BCD(FFloat, BCD2);
    CheckEquals(0, BCDCompare(BCD2, BCD), 'GetBigDecimal');
    CheckEquals(FloatToSQLStr(FFloat), GetString(stFloatIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for Integer field
}
procedure TZTestRowAccessorCase.TestRowAccessorInteger;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stIntegerIndex, WasNull), 'GetBoolean');
    CheckEquals(Byte(FInt), GetByte(stIntegerIndex, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FInt), GetShort(stIntegerIndex, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FInt), GetSmall(stIntegerIndex, WasNull), 0, 'GetSmall');
    CheckEquals(FInt, GetInt(stIntegerIndex, WasNull), 0, 'GetInt');
    CheckEquals(FInt, GetLong(stIntegerIndex, WasNull), 0, 'GetLong');
    CheckEquals(FInt, GetFloat(stIntegerIndex, WasNull), 1, 'GetFloat');
    CheckEquals(FInt, GetDouble(stIntegerIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stIntegerIndex, BCD, WasNull);
    CheckEquals(FInt, BCDToInteger(BCD), 'GetBigDecimal');
    CheckEquals(IntToStr(FInt), GetString(stIntegerIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for Long field
}
procedure TZTestRowAccessorCase.TestRowAccessorLong;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stLongIndex, WasNull), 'GetBoolean');
    CheckEquals(Byte(FLong), GetByte(stLongIndex, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FLong), GetShort(stLongIndex, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FLong), GetSmall(stLongIndex, WasNull), 0, 'GetSmall');
    CheckEquals(FLong, GetInt(stLongIndex, WasNull), 0, 'GetInt');
    CheckEquals(FLong, GetLong(stLongIndex, WasNull), 0, 'GetLong');
    CheckEquals(FLong, GetFloat(stLongIndex, WasNull), 1, 'GetFloat');
    CheckEquals(FLong, GetDouble(stLongIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stLongIndex, BCD, WasNull);
    CheckEquals(FLong, BCD2Int64(BCD), 'GetBigDecimal');
    CheckEquals(IntToStr(FLong), GetString(stLongIndex, WasNull), 'GetString');
  end;
end;

// Fr0sT: this method exists from the beginning but does some magic.
// Just added blank check here to remove "no checks" warning
procedure TZTestRowAccessorCase.TestRowAccessorReadonly;
var
  Collection: TObjectList;
  RowAccessor: TZRowAccessor;
begin
  Collection := GetColumnsInfoCollection;
  try
    RowAccessor := TZRowAccessor.Create(Collection, @ConSettingsDummy); //dummy cp: Stringfield cp is inconsistent
    try
      RowAccessor.Dispose;
    finally
      RowAccessor.Free;
    end;
  finally
    Collection.Free;
  end;
  BlankCheck;
end;

{**
  Test for Short field
}
procedure TZTestRowAccessorCase.TestRowAccessorShort;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stShortIndex, WasNull), 'GetBoolean');
    CheckEquals(Byte(FShort), GetByte(stShortIndex, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FShort), GetShort(stShortIndex, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FShort), GetSmall(stShortIndex, WasNull), 0, 'GetSmall');
    CheckEquals(FShort, GetInt(stShortIndex, WasNull), 0, 'GetInt');
    CheckEquals(FShort, GetLong(stShortIndex, WasNull), 0, 'GetLong');
    CheckEquals(FShort, GetFloat(stShortIndex, WasNull), 0, 'GetFloat');
    CheckEquals(FShort, GetDouble(stShortIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stShortIndex, BCD, WasNull);
    CheckEquals(Integer(FShort), BCDToInteger(BCD), 'GetBigDecimal');
    CheckEquals(IntToStr(FShort), GetString(stShortIndex, WasNull), 'GetString');
  end;
end;

procedure TZTestRowAccessorCase.TestRowAccessorSmall;
var
  WasNull: Boolean;
  BCD: TBCD;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(stSmallIndex, WasNull), 'GetBoolean');
    CheckEquals(Byte(FSmall), GetByte(stSmallIndex, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FSmall), GetShort(stSmallIndex, WasNull), 0, 'GetShort');
    CheckEquals(FSmall, GetSmall(stSmallIndex, WasNull), 0, 'GetSmall');
    CheckEquals(FSmall, GetInt(stSmallIndex, WasNull), 0, 'GetInt');
    CheckEquals(FSmall, GetLong(stSmallIndex, WasNull), 0, 'GetLong');
    CheckEquals(FSmall, GetFloat(stSmallIndex, WasNull), 0, 'GetFloat');
    CheckEquals(FSmall, GetDouble(stSmallIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stSmallIndex, BCD, WasNull);
    CheckEquals(Integer(FSmall), BCDToInteger(BCD), 'GetBigDecimal');
    CheckEquals(IntToStr(FSmall), GetString(stSmallIndex, WasNull), 'GetString');
  end;
end;

{**
  Test for String field
}
procedure TZTestRowAccessorCase.TestRowAccessorString;
var
  WasNull: Boolean;
  BCD: TBCD;
  TS: TZTimeStamp;
  T: TZTime absolute BCD;
  D: TZDate absolute BCD;
  DT: TDateTime;
begin
  with RowAccessor do
  begin
    CheckEquals(False, GetBoolean(stStringIndex, WasNull), 'GetBoolean');
    CheckEquals(ShortInt(StrToIntDef(FString, 0)), GetByte(stStringIndex, WasNull), 0, 'GetByte');
    CheckEquals(SmallInt(StrToIntDef(FString, 0)), GetSmall(stStringIndex, WasNull), 0, 'GetSmall');
    CheckEquals(Integer(StrToIntDef(FString, 0)), GetInt(stStringIndex, WasNull), 0, 'GetInt');
    CheckEquals(LongInt(StrToIntDef(FString, 0)), GetLong(stStringIndex, WasNull), 0, 'GetLong');
    CheckEquals(StrToFloatDef(FString, 0), GetFloat(stStringIndex, WasNull), 100, 'GetFloat');
    CheckEquals(StrToFloatDef(FString, 0), GetDouble(stStringIndex, WasNull), 0, 'GetDouble');
    GetBigDecimal(stStringIndex, BCd, WasNull);
    CheckEquals(StrToInt64(FString), BCD2Int64(BCD), 'GetBigDecimal');
    CheckEquals(FString, GetString(stStringIndex, WasNull), 'GetString');
{    Check(ArraysComapre(GetByteArrayFromString(FString),
       GetBytes(stStringIndex, WasNull)));}

    {test time convertion}
    SetString(stStringIndex, '1999-01-02 12:01:02');
    GetDate(stStringIndex, WasNull, D);
    Check(TryDateToDateTime(D, DT), 'DateConvert');
    CheckEquals(EncodeDate(1999, 01, 02), DT, 0, 'GetDate');
    GetTime(stStringIndex, WasNull, T);
    Check(TryTimeToDateTime(T, DT), 'TimeConvert');
    CheckEquals(EncodeTime(12, 01, 02, 0), DT, stShortIndex, 'GetTime');
    GetTimestamp(stStringIndex, WasNull, TS);
    Check(TryTimeStampToDateTime(TS, DT), 'TimeStampConvert');
    CheckEquals(EncodeDate(1999, 01, 02)+EncodeTime(12,01,02, 0), DT, stShortIndex, 'GetTimestamp');
    SetString(stStringIndex, '');
    CheckEquals('', GetString(stStringIndex, WasNull));
    SetString(stStringIndex, FString);
  end;
end;

{**
  Test for Time field
}
procedure TZTestRowAccessorCase.TestRowAccessorTime;
var
  WasNull: Boolean;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  DT, DT2: TDateTime;
begin
  with RowAccessor do
  begin
    Check(TryTimeToDateTime(FTime, DT), 'TimeConvert');
    CheckEquals(FormatDateTime(ConSettings^.DisplayFormatSettings.TimeFormat, DT),
      GetString(stTimeIndex, WasNull), 'GetString');
    GetTime(stTimeIndex, WasNull, T);
    Check(TryTimeToDateTime(T, DT2), 'TimeConvert');
    CheckEqualsDate(DT, DT2, [], 'GetTime');
    GetTimestamp(stTimeIndex, WasNull, TS);
    Check(TryTimeStampToDateTime(TS, DT2), 'TimeStampConvert');
    CheckEqualsDate(DT, DT2, [], 'GetTimestamp');
  end;
end;

{**
  Test for Timestamp field
}
procedure TZTestRowAccessorCase.TestRowAccessorTimestamp;
var
  WasNull: Boolean;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  DT, DT2: TDateTime;
begin
  with RowAccessor do
  begin
    Check(TryTimeStampToDateTime(FTimeStamp, DT), 'TimeStampConvert');
    CheckEquals(FormatDateTime(ConSettings^.DisplayFormatSettings.DateTimeFormat, DT),
      GetString(stTimestampIndex, WasNull), 'GetString');
    GetDate(stTimestampIndex, WasNull, D);
    Check(TryDateToDateTime(FDate, DT2), 'DateConvert');
    CheckEqualsDate(DT, DT2, [dpYear..dpDay], 'GetDate');
    GetTime(stTimestampIndex, WasNull, T);
    Check(TryTimeToDateTime(T, DT2), 'TimeConvert');
    CheckEqualsDate(DT, DT2, [dpHour..dpMSec], 'GetTime');
    GetTimestamp(stTimestampIndex, WasNull, TS);
    Check(TryTimeStampToDateTime(TS, DT2), 'TimeStampConvert');
    CheckEqualsDate(DT, DT2, [], 'GetTimestamp');
  end;
end;

{**
  Test for UnicodeStream field
}
procedure TZTestRowAccessorCase.TestRowAccessorUnicodeStream;
var
  Stream: TStream;
  ReadNum: Integer;
  BufferWideChar: array[0..100] of Char;
  ResultString: string;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    try
      Stream := GetUnicodeStream(stUnicodeStreamIndex, WasNull);
      CheckNotNull(Stream, 'UnicodeStream');
      CheckEquals(Stream, FUnicodeStream, 'UnicodeStream');
      Stream.Position := 0;
      ReadNum := Stream.Read(BufferWideChar, 100);
      Stream.Free;
      ResultString := WideCharLenToString(@BufferWideChar, ReadNum div 2);
      CheckEquals(FUnicodeStreamData, ResultString);
    except
      Fail('Incorrect GetUnicodeStream method behavior');
    end;
  end;
end;

{**
  Fill fields by it values
}
procedure TZTestRowAccessorCase.FillRowAccessor(RowAccessor: TZRowAccessor);
var
  Index: Integer;
begin
  with RowAccessor do
  begin
    for Index := FirstIndex to LastIndex do
    begin
      try
        case Index of
          stBooleanIndex:       SetBoolean(Index, True);
          stByteIndex:          SetByte(Index, FByte);
          stShortIndex:         SetShort(Index, FShort);
          stSmallIndex:         SetSmall(Index, FSmall);
          stIntegerIndex:       SetInt(Index, FInt);
          stLongIndex:          SetLong(Index, FLong);
          stFloatIndex:         SetFloat(Index, FFloat);
          stDoubleIndex:        SetDouble(Index, FDouble);
          stBigDecimalIndex:    SetBigDecimal(Index, FBigDecimal);
          stStringIndex:        SetString(Index, FString);
          stBytesIndex:         SetBytes(Index, FByteArray);
          stDateIndex:          SetDate(Index, FDate);
          stTimeIndex:          SetTime(Index, FTime);
          stTimestampIndex:     SetTimestamp(Index, FTimeStamp);
          stAsciiStreamIndex:   SetAsciiStream(Index, FAsciiStream);
          stUnicodeStreamIndex: SetUnicodeStream(Index, FUnicodeStream);
          stBinaryStreamIndex:  SetBinaryStream(Index, FBinaryStream);
        end;
        Check(not IsNull(Index));
      except
        Fail('Incorrect Set' + FieldTypes[Index] + ' method behavior');
      end;
    end;
    RowBuffer^.Index := stStringIndex;
    RowBuffer^.UpdateType := utInserted;
    RowBuffer^.BookmarkFlag := 1;
  end;
end;

initialization
  RegisterTest('dbc',TZTestRowAccessorCase.Suite);
end.

