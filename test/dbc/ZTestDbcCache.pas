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
  Types, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcCache, ZClasses, ZSysUtils, ZCollections, ZDbcResultSet,
  ZDbcIntfs, ZDbcUtils, SysUtils, Classes, ZDbcResultSetMetadata,
  Contnrs, ZCompatibility, ZTestCase;

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
    FBigDecimal: Int64;
    FString: string;
    FDate: TDateTime;
    FTime: TDateTime;
    FTimeStamp: TDateTime;
    FAsciiStream: TStream;
    FUnicodeStream: TStream;
    FBinaryStream: TStream;
    FByteArray: TByteDynArray;
    FAsciiStreamData: Ansistring;
    FUnicodeStreamData: WideString;
    FBinaryStreamData: Pointer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetColumnsInfo(Index: Integer; ColumnType: TZSqlType;
      Nullable: TZColumnNullableType; ReadOnly: Boolean;
      Writable: Boolean): TZColumnInfo;
    function GetColumnsInfoCollection: TObjectList;
    function GetRowAccessor: TZRowAccessor;
    function CompareArrays(Array1, Array2: TByteDynArray): Boolean;
    procedure FillRowAccessor(RowAccessor: TZRowAccessor);
    function CompareStreams(Stream1: TStream; Stream2: TStream): Boolean; overload;

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

{ TZTestRowAccessorCase }

{**
  Compares two byte arrays
  @param Array1 the first array to compare.
  @param Array2 the second array to compare.
  @return <code>True</code> if arrays are equal.
}
function TZTestRowAccessorCase.CompareArrays(Array1, Array2: TByteDynArray):
  Boolean;
var
  I: Integer;
begin
  Result := False;
  if High(Array2) <> High(Array1) then Exit;
  for I := 0 to High(Array1) do
    if Array1[I] <> Array2[I] then Exit;
  Result := True;
end;

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
  Result.ColumnDisplaySize := 32;
  Result.ColumnLabel := 'Test Labe'+IntToStr(Index);
  Result.ColumnName := 'TestName'+IntToStr(Index);
  Result.SchemaName := 'TestSchemaName';
  case ColumnType of
    stString: Result.Precision := 255;
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
    Add(GetColumnsInfo(1, stBoolean, ntNullable, False, True));
    Add(GetColumnsInfo(2, stByte, ntNullable, False, True));
    Add(GetColumnsInfo(3, stShort, ntNullable, False, True));
    Add(GetColumnsInfo(4, stSmall, ntNullable, False, True));
    Add(GetColumnsInfo(5, stInteger, ntNullable, False, True));
    Add(GetColumnsInfo(6, stLong, ntNullable, False, True));
    Add(GetColumnsInfo(7, stFloat, ntNullable, False, True));
    Add(GetColumnsInfo(8, stDouble, ntNullable, False, True));
    Add(GetColumnsInfo(9, stBigDecimal, ntNullable, False, True));
    Add(GetColumnsInfo(10, stString, ntNullable, False, True));
    Add(GetColumnsInfo(11, stBytes, ntNullable, False, True));
    Add(GetColumnsInfo(12, stDate, ntNullable, False, True));
    Add(GetColumnsInfo(13, stTime, ntNullable, False, True));
    Add(GetColumnsInfo(14, stTimestamp, ntNullable, False, True));
    Add(GetColumnsInfo(15, stAsciiStream, ntNullable, False, True));
    Add(GetColumnsInfo(16, stUnicodeStream, ntNullable, False, True));
    Add(GetColumnsInfo(17, stBinaryStream, ntNullable, False, True));
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
    Result := TZUnicodeRowAccessor.Create(ColumnsInfo, @ConSettingsDummy);  //dummy cp: Stringfield cp is inconsistent
    Result.Alloc;
  finally
    ColumnsInfo.Free;
  end;
end;

{**
  Setup paramters for test such as variables, stream datas and streams
}
procedure TZTestRowAccessorCase.SetUp;
var
  BufferChar: PAnsiChar;
  BufferWideChar: PWideChar;
begin
  FDate := SysUtils.Date;
  FTime := SysUtils.Time;
  FTimeStamp := SysUtils.Now;

  FAsciiStreamData := 'Test Ascii Stream Data';
  FAsciiStream := TMemoryStream.Create;
  BufferChar := PAnsiChar(FAsciiStreamData);
  FAsciiStream.Write(BufferChar^, Length(FAsciiStreamData));

  FUnicodeStreamData := 'Test Unicode Stream Data';
  FUnicodeStream := TMemoryStream.Create;
  BufferWideChar := PWideChar(FUnicodeStreamData);
  FUnicodeStream.Write(BufferWideChar^, Length(FUnicodeStreamData) * 2);

  FBinaryStream := TMemoryStream.Create;
  FBinaryStreamData := AllocMem(BINARY_BUFFER_SIZE);
  FillChar(FBinaryStreamData^, BINARY_BUFFER_SIZE, 55);
  FBinaryStream.Write(FBinaryStreamData^, BINARY_BUFFER_SIZE);

  FBoolean := true;
  FByte := 255;
  FShort := 127;
  FSmall := 32767;
  FInt := 2147483647;
  FLong := 1147483647;
  FFloat := 3.4E-38;
  FDouble := 1.7E-308;
  FBigDecimal := 9223372036854775807;
  FString := '0123456789';

  SetLength(FByteArray, 5);
  FByteArray[0] := 0;
  FByteArray[1] := 1;
  FByteArray[2] := 2;
  FByteArray[3] := 3;
  FByteArray[4] := 4;

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
begin
  with RowAccessor do
  begin
   Blob := GetBlob(15, WasNull);
   CheckNotNull(Blob, 'Not Null blob from asciistream field');
   Check(not Blob.IsEmpty, 'Blob from asciistream empty');
   Blob := nil;

   Blob := GetBlob(16, WasNull);
   CheckNotNull(Blob, 'Not Null blob from unicodestream field');
   Check(not Blob.IsEmpty, 'Blob from unicodestream empty');
   Blob := nil;

   Blob := GetBlob(17, WasNull);
   CheckNotNull(Blob, 'Not Null blob from binarystream field');
   Check(not Blob.IsEmpty, 'Blob from binarystream empty');
   Blob := nil;
  end;
end;

{**
  Test for setup to null fields and check it on correspondence to null
}
procedure TZTestRowAccessorCase.TestRowAccesorNull;
begin
  with RowAccessor do
  begin
   Check(not IsNull(1), 'Not Null boolen column');
   Check(not IsNull(2), 'Not Null byte column');
   Check(not IsNull(3), 'Not Null short column');
   Check(not IsNull(4), 'Not Null small column');
   Check(not IsNull(5), 'Not Null integer column');
   Check(not IsNull(6), 'Not Null longint column');
   Check(not IsNull(7), 'Not Null float column');
   Check(not IsNull(8), 'Not Null double column');
   Check(not IsNull(9), 'Not Null bigdecimal column');
   Check(not IsNull(10), 'Not Null srting column');
   Check(not IsNull(11), 'Not Null bytearray column');
   Check(not IsNull(12), 'Not Null date column');
   Check(not IsNull(13), 'Not Null time column');
   Check(not IsNull(14), 'Not Null timestamp column');
   Check(not IsNull(15), 'Not Null aciistream column');
   Check(not IsNull(16), 'Not Null unicodestream column');
   Check(not IsNull(17), 'Not Null binarystream column');

   try
     SetNull(1);
   except
     Fail('Incorrect boolean method behavior');
   end;
   Check(IsNull(1), 'Null boolean column');
   try
     SetNull(2);
   except
     Fail('Incorrect byte method behavior');
   end;
   Check(IsNull(2), 'Null byte column');
   try
     SetNull(3);
   except
     Fail('Incorrect short method behavior');
   end;
   Check(IsNull(3), 'Null short column');
   try
     SetNull(4);
   except
     Fail('Incorrect small method behavior');
   end;
   Check(IsNull(4), 'Null small column');
   try
     SetNull(5);
   except
     Fail('Incorrect integer method behavior');
   end;
   Check(IsNull(5), 'Null integer column');
   try
     SetNull(6);
   except
     Fail('Incorrect longint method behavior');
   end;
   Check(IsNull(6), 'Null longint column');
   try
     SetNull(7);
   except
     Fail('Incorrect float method behavior');
   end;
   Check(IsNull(7), 'Null float column');
   try
     SetNull(8);
   except
     Fail('Incorrect double method behavior');
   end;
   Check(IsNull(8), 'Null double column');
   try
     SetNull(10);
   except
     Fail('Incorrect bigdecimal method behavior');
   end;
   Check(IsNull(10), 'Null bigdecimal column');
   try
     SetNull(11);
   except
     Fail('Incorrect string method behavior');
   end;
   Check(IsNull(11), 'Null string column');
   try
     SetNull(12);
   except
   Fail('Incorrect bytearray method behavior');
   end;
   Check(IsNull(12), 'Null bytearray column');
   try
     SetNull(13);
   except
     Fail('Incorrect date method behavior');
   end;
   Check(IsNull(13), 'Null date column');
   try
     SetNull(14);
   except
   Fail('Incorrect time method behavior');
   end;
   Check(IsNull(14), 'Null time column');
   try
     SetNull(15);
   except
     Fail('Incorrect timestamp method behavior');
   end;
   Check(IsNull(15), 'Null timestamp column');
   try
     SetNull(16);
   except
     Fail('Incorrect asciisreeam method behavior');
   end;
   Check(IsNull(16), 'Null asciisreeam column');
   try
     SetNull(17);
   except
     Fail('Incorrect unicodestream method behavior');
   end;
   Check(IsNull(17), 'Null unicodestream column');
   try
   SetNull(17);
   except
     Fail('Incorrect bytestream method behavior');
   end;
   Check(IsNull(17), 'Null bytestream column');
   try
     SetBinaryStream(17, FBinaryStream);
   except
     Fail('Incorrect SetBinaryStream method behavior');
   end;
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
  RowBuffer1 := AllocMem(RowAccessor.RowSize);
  RowBuffer2 := AllocMem(RowAccessor.RowSize);
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
    CheckEquals(10, RowBuffer1^.Index, 'CopyTo. The RowBuffer1 Index');
    CheckEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'CopyTo. The RowBuffer1 UpdateType');
    CheckEquals(1, RowBuffer1^.BookmarkFlag,
        'CopyTo. The RowBuffer1 BookmarkFlag');

    {check Clear method}
    try
      RowAccessor.ClearBuffer(RowBuffer1);
    except
       Fail('Incorrect CopyTo method behavior');
    end;
    Check(Assigned(RowBuffer1),'Clear. The RowBuffer1 assigned )');
    CheckNotEquals(10, RowBuffer1^.Index, 'Clear. The RowBuffer1 Index');
    CheckNotEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'Clear. The RowBuffer1 UpdateType');
    CheckNotEquals(1, RowBuffer1^.BookmarkFlag,
        'Clear. The RowBuffer1 BookmarkFlag');

    {check Moveto method}
    try
      RowAccessor.MoveTo(RowBuffer1);
    except
      Fail('Incorrect CopyTo method behavior');
    end;
    Check(Assigned(RowBuffer1), 'MoveTo. The RowBuffer1 assigned');
    Check(Assigned(RowBuffer1),'MoveTo. The RowBuffer1 assigned )');
    CheckEquals(10, RowBuffer1^.Index, 'MoveTo. The RowBuffer1 Index');
    CheckEquals(ord(utInserted), ord(RowBuffer1^.UpdateType),
        'MoveTo. The RowBuffer1 UpdateType');
    CheckEquals(1, RowBuffer1^.BookmarkFlag,
        'MoveTo. The RowBuffer1 BookmarkFlag');

    CheckNotEquals(10, RowBuffer^.Index, 'MoveTo. The RowBuffer Index');
    CheckNotEquals(ord(utInserted), ord(RowBuffer^.UpdateType),
        'MoveTo. The RowBuffer UpdateType');
    CheckNotEquals(1, RowBuffer^.BookmarkFlag,
        'MoveTo. The RowBuffer BookmarkFlag');

    {check CopyFrom method}
    try
      RowAccessor.CopyFrom(RowBuffer2);
    except
      Fail('Incorrect CopyTo method behavior');
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
    CheckNotEquals(10, RowBuffer^.Index, 'Clear. The RowBuffer Index');
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
  ReadNum: Integer;
  BufferChar: array[0..100] of AnsiChar;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    try
      Stream := GetAsciiStream(15, WasNull);
      CheckNotNull(Stream, 'AsciiStream');
      Check(CompareStreams(Stream, FAsciiStream), 'AsciiStream');
      Stream.Position := 0;
      ReadNum := Stream.Read(BufferChar, 101);
      Stream.Free;
      CheckEquals(String(FAsciiStreamData), BufferToStr(BufferChar, ReadNum));
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
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(9, WasNull), 'GetBoolean');
    CheckEquals(Byte(FBigDecimal), GetByte(9, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FBigDecimal), GetShort(9, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FBigDecimal), GetSmall(9, WasNull), 0, 'GetSmall');
    CheckEquals(Integer(FBigDecimal), GetInt(9, WasNull), 0, 'GetInt');
{$IFNDEF VER130BELOW}
    CheckEquals(Int64(FBigDecimal), GetLong(9, WasNull), 0, 'GetLong');
{$ENDIF}
//    CheckEquals(FBigDecimal, GetFloat(8, WasNull), 0.001, 'GetFloat');
//    CheckEquals(FBigDecimal, GetDouble(8, WasNull), 0.001, 'GetDouble');
    CheckEquals(FBigDecimal, GetBigDecimal(9, WasNull), 0.001, 'GetBigDecimal');
    CheckEquals(FloatToSQLStr(FBigDecimal), GetString(9, WasNull), 'GetString');
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
      Stream := GetBinaryStream(17, WasNull);
      CheckNotNull(Stream, 'BinaryStream');
      Check(CompareStreams(Stream, FBinaryStream), 'BinaryStream');
      Stream.Position := 0;
      ReadNum := Stream.Read(Buffer, BINARY_BUFFER_SIZE);
      Stream.Free;
      CheckEquals(ReadNum, BINARY_BUFFER_SIZE);
      Check(CompareMem(@Buffer, FBinaryStreamData, BINARY_BUFFER_SIZE));
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
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(1, WasNull), 'GetBoolean');
    CheckEquals(1, GetByte(1, WasNull), 0, 'GetByte');
    CheckEquals(1, GetShort(1, WasNull), 0, 'GetShort');
    CheckEquals(1, GetSmall(1, WasNull), 0, 'GetSmall');
    CheckEquals(1, GetInt(1, WasNull), 0, 'GetInt');
    CheckEquals(1, GetLong(1, WasNull), 0, 'GetLong');
    CheckEquals(1, GetFloat(1, WasNull), 0, 'GetFloat');
    CheckEquals(1, GetDouble(1, WasNull), 0, 'GetDouble');
    CheckEquals(1, GetBigDecimal(1, WasNull), 0, 'GetBigDecimal');
    CheckEquals('True', GetString(1, WasNull), 'GetString');
  end;
end;

{**
  Test for Byte field
}
procedure TZTestRowAccessorCase.TestRowAccessorByte;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(2, WasNull), 'GetBoolean');
    CheckEquals(FByte, GetByte(2, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FByte), GetShort(2, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FByte), GetSmall(2, WasNull), 0, 'GetSmall');
    CheckEquals(FByte, GetInt(2, WasNull), 0, 'GetInt');
    CheckEquals(FByte, GetLong(2, WasNull), 0, 'GetLong');
    CheckEquals(FByte, GetFloat(2, WasNull), 0, 'GetFloat');
    CheckEquals(FByte, GetDouble(2, WasNull), 0, 'GetDouble');
    CheckEquals(FByte, GetBigDecimal(2, WasNull), 0, 'GetBigDecimal');
    CheckEquals(IntToStr(FByte), GetString(2, WasNull), 'GetString');
  end;
end;

{**
  Test for Bytes field
}
procedure TZTestRowAccessorCase.TestRowAccessorBytes;

  function  ArrayToString(BytesArray: TByteDynArray): string;
  var
    I: Integer;
  begin
    for I := 0 to High(BytesArray) do
       Result := Result + Char(BytesArray[I]);
  end;

var
  I: Integer;
  ByteArray: TByteDynArray;
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    ByteArray := GetBytes(11, WasNull);
    CheckNotEquals(0, High(ByteArray));
    CheckEquals(ArrayToString(FByteArray), GetString(11, WasNull),
      'strings from bytearray equals');

    if High(ByteArray) <> High(FByteArray) then
      Fail('Size two array diffrent');
    for I := 0 to High(ByteArray) do
      if ByteArray[I] <> FByteArray[I] then
        Fail('Array have different values');
  end;
end;

{**
  Test for Date field
}
procedure TZTestRowAccessorCase.TestRowAccessorDate;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(FDate, AnsiSqlDateToDateTime(GetString(12, WasNull)), 0);
    CheckEquals(FDate, GetDate(12, WasNull), 0);
    CheckEquals(FDate, GetTimestamp(12, WasNull), 0);
  end;
end;

{**
  Test for Double field
}
procedure TZTestRowAccessorCase.TestRowAccessorDouble;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(8, WasNull), 'GetBoolean');
    CheckEquals(Byte(Trunc(FDouble)), GetByte(8, WasNull), 0, 'GetByte');
    CheckEquals(Trunc(FDouble), GetShort(8, WasNull), 0, 'GetShort');
    CheckEquals(Trunc(FDouble), GetSmall(8, WasNull), 0, 'GetSmall');
    CheckEquals(Trunc(FDouble), GetInt(8, WasNull), 0, 'GetInt');
    CheckEquals(Trunc(FDouble), GetLong(8, WasNull), 0, 'GetLong');
    CheckEquals(FDouble, GetFloat(8, WasNull), 0.001, 'GetFloat');
    CheckEquals(FDouble, GetDouble(8, WasNull), 0.001, 'GetDouble');
    CheckEquals(FDouble, GetBigDecimal(8, WasNull), 0.001, 'GetBigDecimal');
    CheckEquals(FloatToSQLStr(FDouble), GetString(8, WasNull), 'GetString');
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
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(7, WasNull), 'GetBoolean');
    CheckEquals(Trunc(FFloat), GetByte(7, WasNull), 0, 'GetByte');
    CheckEquals(Trunc(FFloat), GetShort(7, WasNull), 0, 'GetShort');
    CheckEquals(Trunc(FFloat), GetSmall(7, WasNull), 0, 'GetSmall');
    CheckEquals(Trunc(FFloat), GetInt(7, WasNull), 0, 'GetInt');
    CheckEquals(Trunc(FFloat), GetLong(7, WasNull), 0, 'GetLong');
    CheckEquals(FFloat, GetFloat(7, WasNull), 0.001, 'GetFloat');
    CheckEquals(FFloat, GetDouble(7, WasNull), 0.001, 'GetDouble');
    CheckEquals(FFloat, GetBigDecimal(7, WasNull), 0.001, 'GetBigDecimal');
    CheckEquals(FloatToSQLStr(FFloat), GetString(7, WasNull), 'GetString');
  end;
end;

{**
  Test for Integer field
}
procedure TZTestRowAccessorCase.TestRowAccessorInteger;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(5, WasNull), 'GetBoolean');
    CheckEquals(Byte(FInt), GetByte(5, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FInt), GetShort(5, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FInt), GetSmall(5, WasNull), 0, 'GetSmall');
    CheckEquals(FInt, GetInt(5, WasNull), 0, 'GetInt');
    CheckEquals(FInt, GetLong(5, WasNull), 0, 'GetLong');
    CheckEquals(FInt, GetFloat(5, WasNull), 1, 'GetFloat');
    CheckEquals(FInt, GetDouble(5, WasNull), 0, 'GetDouble');
    CheckEquals(FInt, GetBigDecimal(5, WasNull), 0, 'GetBigDecimal');
    CheckEquals(IntToStr(FInt), GetString(5, WasNull), 'GetString');
  end;
end;

{**
  Test for Long field
}
procedure TZTestRowAccessorCase.TestRowAccessorLong;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(6, WasNull), 'GetBoolean');
    CheckEquals(Byte(FLong), GetByte(6, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FLong), GetShort(6, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FLong), GetSmall(6, WasNull), 0, 'GetSmall');
    CheckEquals(FLong, GetInt(6, WasNull), 0, 'GetInt');
    CheckEquals(FLong, GetLong(6, WasNull), 0, 'GetLong');
    CheckEquals(FLong, GetFloat(6, WasNull), 1, 'GetFloat');
    CheckEquals(FLong, GetDouble(6, WasNull), 0, 'GetDouble');
    CheckEquals(FLong, GetBigDecimal(6, WasNull), 0, 'GetBigDecimal');
    CheckEquals(IntToStr(FLong), GetString(6, WasNull), 'GetString');
  end;
end;

{**
  Test for Short field
}
procedure TZTestRowAccessorCase.TestRowAccessorReadonly;
var
  Collection: TObjectList;
  RowAccessor: TZRowAccessor;
begin
  Collection := GetColumnsInfoCollection;
  try
    RowAccessor := TZUnicodeRowAccessor.Create(Collection, @ConSettingsDummy); //dummy cp: Stringfield cp is inconsistent
    try
      RowAccessor.Dispose;
    finally
      RowAccessor.Free;
    end;
  finally
    Collection.Free;
  end;
end;

procedure TZTestRowAccessorCase.TestRowAccessorShort;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(3, WasNull), 'GetBoolean');
    CheckEquals(Byte(FShort), GetByte(3, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FShort), GetShort(3, WasNull), 0, 'GetShort');
    CheckEquals(SmallInt(FShort), GetSmall(3, WasNull), 0, 'GetSmall');
    CheckEquals(FShort, GetInt(3, WasNull), 0, 'GetInt');
    CheckEquals(FShort, GetLong(3, WasNull), 0, 'GetLong');
    CheckEquals(FShort, GetFloat(3, WasNull), 0, 'GetFloat');
    CheckEquals(FShort, GetDouble(3, WasNull), 0, 'GetDouble');
    CheckEquals(FShort, GetBigDecimal(3, WasNull), 0, 'GetBigDecimal');
    CheckEquals(IntToStr(FShort), GetString(3, WasNull), 'GetString');
  end;
end;

procedure TZTestRowAccessorCase.TestRowAccessorSmall;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(True, GetBoolean(4, WasNull), 'GetBoolean');
    CheckEquals(Byte(FSmall), GetByte(4, WasNull), 0, 'GetByte');
    CheckEquals(ShortInt(FSmall), GetShort(4, WasNull), 0, 'GetShort');
    CheckEquals(FSmall, GetSmall(4, WasNull), 0, 'GetSmall');
    CheckEquals(FSmall, GetInt(4, WasNull), 0, 'GetInt');
    CheckEquals(FSmall, GetLong(4, WasNull), 0, 'GetLong');
    CheckEquals(FSmall, GetFloat(4, WasNull), 0, 'GetFloat');
    CheckEquals(FSmall, GetDouble(4, WasNull), 0, 'GetDouble');
    CheckEquals(FSmall, GetBigDecimal(4, WasNull), 0, 'GetBigDecimal');
    CheckEquals(IntToStr(FSmall), GetString(4, WasNull), 'GetString');
  end;
end;

{**
  Test for String field
}
procedure TZTestRowAccessorCase.TestRowAccessorString;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(False, GetBoolean(10, WasNull), 'GetBoolean');
    CheckEquals(ShortInt(StrToIntDef(FString, 0)), GetByte(10, WasNull), 0, 'GetByte');
    CheckEquals(SmallInt(StrToIntDef(FString, 0)), GetSmall(10, WasNull), 0, 'GetSmall');
    CheckEquals(Integer(StrToIntDef(FString, 0)), GetInt(10, WasNull), 0, 'GetInt');
    CheckEquals(LongInt(StrToIntDef(FString, 0)), GetLong(10, WasNull), 0, 'GetLong');
    CheckEquals(StrToFloatDef(FString, 0), GetFloat(10, WasNull), 100, 'GetFloat');
    CheckEquals(StrToFloatDef(FString, 0), GetDouble(10, WasNull), 0, 'GetDouble');
    CheckEquals(Int64(StrToInt(FString)), GetBigDecimal(10, WasNull), 0, 'GetBigDecimal');
    CheckEquals(FString, GetString(10, WasNull), 'GetString');
{    Check(ArraysComapre(GetByteArrayFromString(FString),
       GetBytes(8, WasNull)));}

    {test time convertion}
    SetString(10, '1999-01-02 12:01:02');
    CheckEquals(EncodeDate(1999, 01, 02), GetDate(10, WasNull), 0, 'GetDate');
    CheckEquals(EncodeTime(12, 01, 02, 0), GetTime(10, WasNull), 3, 'GetTime');
    CheckEquals(EncodeDate(1999, 01, 02)+EncodeTime(12,01,02, 0),
      GetTimestamp(10, WasNull), 3, 'GetTimestamp');
    SetString(10, FString);
  end;
end;

{**
  Test for Time field
}
procedure TZTestRowAccessorCase.TestRowAccessorTime;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(FTime, AnsiSqlDateToDateTime(GetString(13, WasNull)), 3, 'GetString');
    CheckEquals(FTime, GetTime(13, WasNull), 3, 'Getime');
//    CheckEquals(FTime, GetTimestamp(13, WasNull), 3, 'GetTimestamp');
  end;
end;

{**
  Test for Timestamp field
}
procedure TZTestRowAccessorCase.TestRowAccessorTimestamp;
var
  WasNull: Boolean;
begin
  with RowAccessor do
  begin
    CheckEquals(FormatDateTime('yyyy-mm-dd hh:mm:ss', FTimeStamp),
        GetString(14, WasNull), 'GetString');
//!!! Rwrite    CheckEquals(DateOf(FTimeStamp), GetDate(14, WasNull), 3, 'GetDate');
    CheckEquals(FTimeStamp, GetTimestamp(14, WasNull), 3, 'GetTimestamp');
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
      Stream := GetUnicodeStream(16, WasNull);
      CheckNotNull(Stream, 'UnicodeStream');
      Check(CompareStreams(Stream, FUnicodeStream), 'UnicodeStream');
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
begin
  with RowAccessor do
  begin
    try
      SetBoolean(1, true);
      Check(not IsNull(1));
    except
      Fail('Incorrect SetBoolean method behavior');
    end;
    try
      SetByte(2, FByte);
    except
      Fail('Incorrect SetByte method behavior');
    end;
    try
      SetShort(3, FShort);
    except
      Fail('Incorrect SetShort method behavior');
    end;
    try
      SetSmall(4, FSmall);
    except
      Fail('Incorrect SetSmall method behavior');
    end;
    try
      SetInt(5, FInt);
    except
      Fail('Incorrect SetInt method behavior');
    end;
    try
      SetLong(6, FLong);
    except
      Fail('Incorrect SetLong method behavior');
    end;
    try
      SetFloat(7, FFloat);
    except
      Fail('Incorrect SetFloat method behavior');
    end;
    try
      SetDouble(8, FDouble);
    except
      Fail('Incorrect SetDouble method behavior');
    end;
    try
      SetBigDecimal(9, FBigDecimal);
    except
      Fail('Incorrect SetBigDecimal method behavior');
    end;
    try
      SetString(10, FString);
    except
      Fail('Incorrect SetString method behavior');
    end;
    try
      SetBytes(11, FByteArray);
    except
      Fail('Incorrect SetBytes method behavior');
    end;
    try
      SetDate(12, FDate);
    except
      Fail('Incorrect SetDate method behavior');
    end;
    try
      SetTime(13, FTime);
    except
      Fail('Incorrect SetTime method behavior');
    end;
    try
      SetTimestamp(14, FTimeStamp);
    except
      Fail('Incorrect SetTimestamp method behavior');
    end;
    try
      SetAsciiStream(15, FAsciiStream);
    except
      Fail('Incorrect SetAsciiStream method behavior');
    end;
    try
      SetUnicodeStream(16, FUnicodeStream);
    except
      Fail('Incorrect SetUnicodeStream method behavior');
    end;
    try
      SetBinaryStream(17, FBinaryStream);
    except
      Fail('Incorrect SetBinaryStream method behavior');
    end;
    RowBuffer^.Index := 10;
    RowBuffer^.UpdateType := utInserted;
    RowBuffer^.BookmarkFlag := 1;
  end;
end;

function TZTestRowAccessorCase.CompareStreams(Stream1, Stream2: TStream):
  Boolean;
var
  Buffer1, Buffer2: array[0..1024] of Char;
  ReadNum1, ReadNum2: Integer;
begin
  CheckNotNull(Stream1, 'Stream #1 is null');
  CheckNotNull(Stream2, 'Stream #2 is null');
  CheckEquals(Stream1.Size, Stream2.Size, 'Stream sizes are not equal');

  Stream1.Position := 0;
  ReadNum1 := Stream1.Read(Buffer1, 1024);
  Stream2.Position := 0;
  ReadNum2 := Stream2.Read(Buffer2, 1024);

  CheckEquals(ReadNum1, ReadNum2, 'Read sizes are not equal.');
  Result := CompareMem(@Buffer1, @Buffer2, ReadNum1);
end;

initialization
  RegisterTest('dbc',TZTestRowAccessorCase.Suite);
end.
