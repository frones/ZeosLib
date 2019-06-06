{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Case for Generic ResultSet Classes        }
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

unit ZTestDbcResultSet;

interface
{$I ZDbc.inc}
uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Classes, SysUtils,
  ZDbcIntfs, {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZSysUtils,
  ZDbcResultSet, ZCompatibility, ZTestConsts, ZTestCase;

type

 {** Implements a test case for TZAbstractBlob. }
  TZTestAbstractBlobCase = class(TZGenericTestCase)
  private
    FString: string;
    FBuffer: Pointer;
    FBytes: TBytes;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBlob;
    procedure TestBlobNil;
    procedure TestBlobClone;
  end;

  {** Implements a test case for TZColumnInfo. }
  TZTestColumnInfoCase = class(TZGenericTestCase)
  published
    procedure TestColumnInfo;
  end;

implementation

uses ZDbcResultSetMetadata;

{ TZTestAbstractBlobCase }

procedure TZTestAbstractBlobCase.SetUp;
var
  I: integer;
begin
  FBuffer := AllocMem(BINARY_BUFFER_SIZE);
  FString := 'String for test blob working';
  SetLength(FBytes, BYTES_LEN);
  for I := 0 to BYTES_LEN-1 do
    FBytes[I] := I;
end;

procedure TZTestAbstractBlobCase.TearDown;
begin
  FreeMem(FBuffer, BINARY_BUFFER_SIZE);
end;

procedure TZTestAbstractBlobCase.TestBlob;
var
  Blob: IZBlob;
  StreamIn: TStream;
  StreamOut: TStream;
  ResultString: string;
  ResultBytes: TBytes;
  ReadNum: integer;
  Buffer: array[0..BINARY_BUFFER_SIZE] of Byte;
begin
  StreamIn := StreamFromData(FBuffer, BINARY_BUFFER_SIZE);

  {Test with defined constructor}
  Blob := TZAbstractBlob.CreateWithStream(StreamIn);
  Check(not Blob.IsEmpty, 'IsEmpty');
  Check(not Blob.IsUpdated, 'IsUpdated');
  CheckEquals(BINARY_BUFFER_SIZE, Blob.Length, 'Length');

  StreamOut := Blob.GetStream;
  CheckEquals(StreamIn, StreamOut, 'StreamIn = StreamOut');
  ReadNum := StreamOut.Read(Buffer, BINARY_BUFFER_SIZE);
  StreamOut.Free;
  StreamIn.Free;

  CheckEquals(ReadNum, BINARY_BUFFER_SIZE);
  CheckEqualsMem(@Buffer, FBuffer, BINARY_BUFFER_SIZE);

  {string test}
  Blob.SetString(RawByteString(FString));
  Check(not Blob.IsEmpty, 'IsEmpty');
  Check(Blob.IsUpdated, 'IsUpdated');
  CheckEquals(Length(FString), Blob.Length, 'Length');
  ResultString := String(Blob.GetString);
  CheckEquals(FString, ResultString, 'Strings compare');

  { bytes test}
  Blob.SetBytes(FBytes);
  Check(not Blob.IsEmpty, 'IsEmpty');
  Check(Blob.IsUpdated, 'IsUpdated');
  CheckEquals(BYTES_LEN, Blob.Length, 'Length');
  ResultBytes := Blob.GetBytes;
  CheckEquals(FBytes, ResultBytes, 'Compare arrays');

  Blob := nil;
end;

procedure TZTestAbstractBlobCase.TestBlobClone;
var
  Blob: IZBlob;
  BlobClone: IZBlob;
  StreamIn: TStream;
  StreamOut: TStream;
  ReadNum: integer;
  Buffer: array[0..BINARY_BUFFER_SIZE] of Byte;
begin
  StreamIn := StreamFromData(FBuffer, BINARY_BUFFER_SIZE);
  Blob := TZAbstractBlob.CreateWithStream(StreamIn);

 {Test clone blob}
  BlobClone := Blob.Clone;
  Blob := nil;
  Check(not BlobClone.IsEmpty, 'IsEmpty');
  Check(not BlobClone.IsUpdated, 'IsUpdated');
  CheckEquals(BINARY_BUFFER_SIZE, BlobClone.Length, 'Length');

  StreamOut := BlobClone.GetStream;
  CheckEquals(StreamIn, StreamOut, 'StreamIn = StreamOut');
  ReadNum := StreamOut.Read(Buffer, BINARY_BUFFER_SIZE);
  StreamOut.Free;
  StreamIn.Free;

  CheckEquals(ReadNum, BINARY_BUFFER_SIZE);
  CheckEqualsMem(@Buffer, FBuffer, BINARY_BUFFER_SIZE);
  BlobClone := nil;
end;

procedure TZTestAbstractBlobCase.TestBlobNil;
var
  Blob: IZBlob;
  StreamIn: TStream;
  StreamOut: TStream;
  ResultString: string;
  ResultBytes: TBytes;
  ReadNum: integer;
  Buffer: array[0..BINARY_BUFFER_SIZE] of Byte;
begin
  StreamIn := StreamFromData(FBuffer, BINARY_BUFFER_SIZE);

  {Test with nil constructor}
  Blob := TZAbstractBlob.CreateWithStream(nil);
  Check(Blob.IsEmpty, 'IsEmpty');
  Check(not Blob.IsUpdated, 'IsUpdated');
  CheckEquals(-1, Blob.Length, 'Length');

  Blob.SetStream(StreamIn);
  Check(Blob.IsUpdated, 'IsUpdated');
  Check(not Blob.IsEmpty, 'IsEmpty');
  CheckEquals(BINARY_BUFFER_SIZE, Blob.Length, 'Length');

  StreamOut := Blob.GetStream;
  CheckEquals(StreamIn, StreamOut, 'StreamIn = StreamOut');
  ReadNum := StreamOut.Read(Buffer, BINARY_BUFFER_SIZE);
  StreamIn.Free;
  StreamOut.Free;

  CheckEquals(ReadNum, BINARY_BUFFER_SIZE);
  CheckEqualsMem(@Buffer, FBuffer, BINARY_BUFFER_SIZE);
  Blob := nil;

  {string test}
  Blob := TZAbstractBlob.CreateWithStream(nil);
  Blob.SetString(RawByteString(FString));
  Check(not Blob.IsEmpty, 'IsEmpty');
  Check(Blob.IsUpdated, 'IsUpdated');
  CheckEquals(Length(FString), Blob.Length, 'Length');
  ResultString := String(Blob.GetString);
  CheckEquals(FString, ResultString, 'Strings comapre');
  Blob := nil;

  { bytes test}
  Blob := TZAbstractBlob.CreateWithStream(nil);
  Blob.SetBytes(FBytes);
  Check(not Blob.IsEmpty, 'IsEmpty');
  Check(Blob.IsUpdated, 'IsUpdated');
  CheckEquals(BYTES_LEN, Blob.Length, 'Length');
  ResultBytes := Blob.GetBytes;
  CheckEquals(FBytes, ResultBytes, 'Compare arrays');
end;

{ TZTestColumnInfoCase }

procedure TZTestColumnInfoCase.TestColumnInfo;
var
 ColumnInfo: TZColumnInfo;
 ColumnInfo1: TZColumnInfo;
begin
  ColumnInfo := TZColumnInfo.Create;
  ColumnInfo1 := TZColumnInfo.Create;

  with ColumnInfo do
  begin
    AutoIncrement := True;
    CaseSensitive := True;
    Searchable := True;
    Currency := True;
    Nullable := ntNoNulls;
    Signed := True;
    ColumnLabel := 'Test Label';
    ColumnName := 'Test name';
    SchemaName := 'Test schema';
    Precision := 99;
    Scale := 88;
    TableName := 'Test table name';
    CatalogName := 'Test catalog name';
    ColumnType := stBoolean;
    ReadOnly := False;
    Writable := True;
    DefinitelyWritable := True;

   Check(AutoIncrement, 'IsAutoIncrement');
   Check(CaseSensitive, 'IsCaseSensitive');
   Check(Searchable, 'IsSearchable');
   Check(Currency, 'IsCurrency');
   CheckEquals(Ord(ntNoNulls), Ord(Nullable), 'IsNullable');
   Check(Signed, 'IsSigned');
   CheckEquals('Test Label',  ColumnLabel, 'GetColumnLabel');
   CheckEquals('Test name', ColumnName, 'GetColumnName');
   CheckEquals('Test schema', SchemaName, 'GetSchemaName');
   CheckEquals(99, Precision, 'GetPrecision');
   CheckEquals(88, Scale, 'GetScale');
   CheckEquals('Test table name', TableName, 'GetTableName');
   CheckEquals('Test catalog name', CatalogName, 'GetCatalogName');
   CheckEquals(ord(stBoolean), Ord(ColumnType), 'GetColumnType');
   CheckEquals('Boolean', GetColumnTypeName, 'GetColumnTypeName');
   CheckEquals(False, ReadOnly, 'IsReadOnly');
   CheckEquals(True, Writable, 'IsWritable');
   CheckEquals(True, DefinitelyWritable, 'IsDefinitelyWritable');

  end;
  with ColumnInfo1 do
  begin
   AutoIncrement := True;
   CaseSensitive := True;
   Searchable := True;
   Currency := True;
   Nullable := ntNoNulls;
   Signed := True;
   ColumnLabel := 'Test Label';
   ColumnName := 'Test name';
   SchemaName := 'Test schema';
   Precision := 99;
   Scale := 88;
   TableName := 'Test table name';
   CatalogName := 'Test catalog name';
   ColumnType := stBoolean;
   ReadOnly := False;
   Writable := True;
   DefinitelyWritable := True;
  end;
//!!  Check(not ColumnInfo.Equals(nil), 'Equals');
//!!  Check(ColumnInfo.Equals(ColumnInfo1), 'Equals');
  ColumnInfo.Free;
  ColumnInfo1.Free;
end;

initialization
  RegisterTest('dbc',TZTestAbstractBlobCase.Suite);

end.
