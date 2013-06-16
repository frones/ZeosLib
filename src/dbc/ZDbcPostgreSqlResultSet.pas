{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, Types, ZSysUtils, ZDbcIntfs, ZDbcResultSet,
  ZPlainPostgreSqlDriver, ZDbcResultSetMetadata, ZDbcLogging, ZCompatibility;

type

  {** Implements PostgreSQL ResultSet. }
  TZPostgreSQLResultSet = class(TZAbstractResultSet)
  private
    FHandle: PZPostgreSQLConnect;
    FQueryHandle: PZPostgreSQLResult;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
  protected
    function InternalGetString(ColumnIndex: Integer): ZAnsiString; override;
    procedure Open; override;
    procedure DefinePostgreSQLToSQLType(ColumnIndex: Integer;
      ColumnInfo: TZColumnInfo; TypeOid: Oid);
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Statement: IZStatement; SQL: string; Handle: PZPostgreSQLConnect;
      QueryHandle: PZPostgreSQLResult; Chunk_Size: Integer);
    destructor Destroy; override;

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetUnicodeStream(ColumnIndex: Integer): TStream; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  {** Represents an interface, specific for PostgreSQL blobs. }
  IZPostgreSQLBlob = interface(IZBlob)
    ['{BDFB6B80-477D-4CB1-9508-9541FEA6CD72}']
    function GetBlobOid: Oid;
    procedure ReadBlob;
    procedure WriteBlob;
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZPostgreSQLBlob = class(TZAbstractBlob, IZPostgreSQLBlob)
  private
    FHandle: PZPostgreSQLConnect;
    FBlobOid: Oid;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver; Data: Pointer;
      Size: Integer; Handle: PZPostgreSQLConnect; BlobOid: Oid; Chunk_Size: Integer);

    destructor Destroy; override;

    function GetBlobOid: Oid;
    procedure ReadBlob;
    procedure WriteBlob;

    function IsEmpty: Boolean; override;
    function Clone: IZBlob; override;

    function GetStream: TStream; override;
  end;

implementation

uses
  Math, ZMessages, ZMatchPattern, ZDbcPostgreSql, ZDbcUtils, ZEncoding,
  ZDbcPostgreSqlUtils{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZPostgreSQLResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a PostgreSQL plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a PostgreSQL specific query handle.
}
constructor TZPostgreSQLResultSet.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Statement: IZStatement; SQL: string; Handle: PZPostgreSQLConnect;
  QueryHandle: PZPostgreSQLResult; Chunk_Size: Integer);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  FQueryHandle := QueryHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FChunk_Size := Chunk_Size; //size of red/write lob chunks

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLResultSet.Destroy;
begin
  inherited Destroy;
end;

{**
  Converts a PostgreSQL native types into ZDBC SQL types.
  @param ColumnIndex a column index.
  @param ColumnInfo a column description object.
  @param TypeOid a type oid.
  @return a SQL undepended type.
}
procedure TZPostgreSQLResultSet.DefinePostgreSQLToSQLType(ColumnIndex: Integer;
  ColumnInfo: TZColumnInfo; TypeOid: Oid);
var
  SQLType: TZSQLType;
  Connection: IZPostgreSQLConnection;
begin
  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  case TypeOid of
    790: ColumnInfo.Currency := True; { money }
    19: if (Connection.GetServerMajorVersion < 7) or
           ((Connection.GetServerMajorVersion = 7) and (Connection.GetServerMinorVersion < 3)) then
          ColumnInfo.Precision := 32
        else
          ColumnInfo.Precision := 64; { name }
    650: ColumnInfo.Precision := 100; { cidr }
    869: ColumnInfo.Precision := 100; { inet }
    829: ColumnInfo.Precision := 17; { macaddr }
    1186: ColumnInfo.Precision := 32; { interval }
    24: ColumnInfo.Precision := 64; { regproc } // M.A. was 10
    17:{ bytea }
      if Connection.IsOidAsBlob then
        ColumnInfo.Precision := 256;
  end;

  SQLType := PostgreSQLToSQLType(Connection, TypeOid);

  if SQLType <> stUnknown then
    ColumnInfo.ColumnType := SQLType
  else
  begin
    ColumnInfo.ColumnType := stString;
    ColumnInfo.Precision := 255;
    ColumnInfo.ReadOnly := True;
  end;
end;

{**
  Opens this recordset.
}
procedure TZPostgreSQLResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FQueryHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  LastRowNo := FPlainDriver.GetRowCount(FQueryHandle);

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FPlainDriver.GetFieldCount(FQueryHandle) - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';
      {$IFDEF DELPHI18_UP}
      ColumnLabel := ZDbcString(SysUtils.StrPas(FPlainDriver.GetFieldName(FQueryHandle, I)));
      {$ELSE}
      ColumnLabel := ZDbcString(StrPas(FPlainDriver.GetFieldName(FQueryHandle, I)));
      {$ENDIF}

      ColumnDisplaySize := 0;
      Scale := 0;
      Precision := 0;

      AutoIncrement := False;
      Signed := False;
      Nullable := ntNullable;

      DefinePostgreSQLToSQLType(I, ColumnInfo,
        FPlainDriver.GetFieldType(FQueryHandle, I));

      if Precision = 0 then
      begin
        Precision := Max(Max(FPlainDriver.GetFieldMode(FQueryHandle, I) - 4,
          FPlainDriver.GetFieldSize(FQueryHandle, I)), 0);

        if ColumnType in [stString, stUnicodeString] then
          if ( (ColumnLabel = 'expr') or ( Precision = 0 ) ) then
            Precision := GetFieldSize(ColumnType, ConSettings, 255,
              ConSettings.ClientCodePage^.CharWidth, nil, True)
          else
            Precision := GetFieldSize(ColumnType, ConSettings, Precision,
              ConSettings.ClientCodePage^.CharWidth, @ColumnDisplaySize);
      end;
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZPostgreSQLResultSet.Close;
begin
  if FQueryHandle <> nil then
    FPlainDriver.Clear(FQueryHandle);
  FHandle := nil;
  FQueryHandle := nil;
  inherited Close;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZPostgreSQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Result := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1,
    ColumnIndex - 1) <> 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.InternalGetString(ColumnIndex: Integer): ZAnsiString;
{$IFDEF WITH_RAWBYTESTRING}
var Len: Integer;
{$ENDIF}
begin
  ColumnIndex := ColumnIndex - 1;
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1,
    ColumnIndex) <> 0;
  {$IFDEF WITH_RAWBYTESTRING}
  Len := FPlainDriver.GetLength(FQueryHandle, RowNo - 1, ColumnIndex);
  SetLength(Result, Len);
  Move(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex)^, PAnsiChar(Result)^, Len);
  {$ELSE}
  SetString(Result, FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex),
    FPlainDriver.GetLength(FQueryHandle, RowNo - 1, ColumnIndex));
  {$ENDIF}
  if FPlainDriver.GetFieldType(FQueryHandle, ColumnIndex) = 1042 then
    Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}TrimRight(Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZPostgreSQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Temp := UpperCase(String(InternalGetString(ColumnIndex)));
  Result := (Temp = 'Y') or (Temp = 'YES') or (Temp = 'T') or
    (Temp = 'TRUE') or (StrToIntDef(String(Temp), 0) <> 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := Byte(StrToIntDef(String(InternalGetString(ColumnIndex)), 0));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := SmallInt(StrToIntDef(String(InternalGetString(ColumnIndex)), 0));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := StrToIntDef(String(InternalGetString(ColumnIndex)), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := StrToInt64Def(String(InternalGetString(ColumnIndex)), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := SQLStrToFloatDef(InternalGetString(ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := SQLStrToFloatDef(InternalGetString(ColumnIndex), 0);
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
function TZPostgreSQLResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := SQLStrToFloatDef(InternalGetString(ColumnIndex), 0);
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
function TZPostgreSQLResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(DecodeString(InternalGetString(ColumnIndex)));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Value: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Value := String(InternalGetString(ColumnIndex));
  if IsMatch('????-??-??*', Value) then
    Result := Trunc(AnsiSQLDateToDateTime(Value))
  else
    Result := Trunc(TimestampStrToDateTime(Value));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Value: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Value := String(InternalGetString(ColumnIndex));
  if IsMatch('*??:??:??*', Value) then
    Result := Frac(AnsiSQLDateToDateTime(Value))
  else
    Result := Frac(TimestampStrToDateTime(Value));
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
function TZPostgreSQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Value: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Value := String(InternalGetString(ColumnIndex));
  if IsMatch('????-??-??*', Value) then
    Result := AnsiSQLDateToDateTime(Value)
  else
    Result := TimestampStrToDateTime(Value);
end;

function TZPostgreSQLResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := TStringStream.Create(InternalGetString(ColumnIndex));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZPostgreSQLResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobOid: Oid;
  Stream: TStream;
  Connection: IZConnection;
  WS: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Connection := Statement.GetConnection;
  if (GetMetadata.GetColumnType(ColumnIndex) = stBinaryStream)
    and (Statement.GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
  begin
    if FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex - 1) = 0 then
      BlobOid := StrToIntDef(String(InternalGetString(ColumnIndex)), 0)
    else
      BlobOid := 0;

    Result := TZPostgreSQLBlob.Create(FPlainDriver, nil, 0, FHandle, BlobOid, FChunk_Size);
  end
  else
  begin
    if FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex - 1) = 0 then
    begin
      Stream := nil;
      try
        case GetMetadata.GetColumnType(ColumnIndex) of
          stBinaryStream:
            Stream := TStringStream.Create(FPlainDriver.DecodeBYTEA(InternalGetString(ColumnIndex), Self.FHandle));
          stAsciiStream:
            Stream := TStringStream.Create(GetValidatedAnsiString(InternalGetString(ColumnIndex), ConSettings, True));
          else
            begin
              WS := ZDbcUnicodeString(InternalGetString(ColumnIndex));
              Stream := WideStringStream(Ws);
            end;
        end;
        Result := TZAbstractBlob.CreateWithStream(Stream, GetStatement.GetConnection,
          GetMetadata.GetColumnType(ColumnIndex) = stUnicodeStream);
      finally
        if Assigned(Stream) then
          Stream.Free;
      end;
    end
    else
      Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection);
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
function TZPostgreSQLResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}

  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  { Processes negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if ResultSetType <> rtForwardOnly then
  begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end
    else
      Result := False;
  end
  else
    RaiseForwardOnlyException;
end;

{ TZPostgreSQLBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a PostgreSQL connection reference.
}
constructor TZPostgreSQLBlob.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Data: Pointer; Size: Integer; Handle: PZPostgreSQLConnect; BlobOid: Oid;
  Chunk_Size: Integer);
begin
  inherited CreateWithData(Data, Size, nil);
  FHandle := Handle;
  FBlobOid := BlobOid;
  FPlainDriver := PlainDriver;
  FChunk_Size := Chunk_Size;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLBlob.Destroy;
begin
  inherited Destroy;
end;

{**
  Gets the blob handle oid.
  @return the blob handle oid.
}
function TZPostgreSQLBlob.GetBlobOid: Oid;
begin
  Result := FBlobOid;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZPostgreSQLBlob.ReadBlob;
var
  BlobHandle: Integer;
  Buffer: array[0..1024] of AnsiChar;
  ReadNum: Integer;
  ReadStream: TMemoryStream;
begin
  if not Updated and (FBlobOid > 0) then
  begin
    BlobHandle := FPlainDriver.OpenLargeObject(FHandle, FBlobOid, INV_READ);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Read Large Object',nil);
    ReadStream := nil;
    if BlobHandle >= 0 then
    begin
      ReadStream := TMemoryStream.Create;
      repeat
        ReadNum := FPlainDriver.ReadLargeObject(FHandle, BlobHandle,
          Buffer, 1024);
        if ReadNum > 0 then
        begin
          ReadStream.SetSize(ReadStream.Size + ReadNum);
          ReadStream.Write(Buffer, ReadNum);
        end;
      until ReadNum < 1024;
      FPlainDriver.CloseLargeObject(FHandle, BlobHandle);
      ReadStream.Position := 0;
    end;
    SetStream(ReadStream);
    if ReadStream <> nil then
      ReadStream.free;
  end;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZPostgreSQLBlob.WriteBlob;
var
  BlobHandle: Integer;
  Position: Integer;
  Size: Integer;
begin
  { Checks for empty blob. }
  if IsEmpty then
  begin
    FBlobOid := 0;
    Exit;
  end;

  { Creates a new large object. }
  if FBlobOid = 0 then
  begin
    FBlobOid := FPlainDriver.CreateLargeObject(FHandle, INV_WRITE);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Create Large Object',nil);
  end;

  { Opens and writes a large object. }
  BlobHandle := FPlainDriver.OpenLargeObject(FHandle, FBlobOid, INV_WRITE);
  CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Open Large Object',nil);

  Position := 0;
  while Position < BlobSize do
  begin
    if (BlobSize - Position) < FChunk_Size then
      Size := BlobSize - Position
    else
      Size := FChunk_Size;
    FPlainDriver.WriteLargeObject(FHandle, BlobHandle,
      Pointer(NativeUInt(BlobData) + NativeUInt(Position)), Size);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Write Large Object',nil);
    Inc(Position, Size);
  end;

  FPlainDriver.CloseLargeObject(FHandle, BlobHandle);
  CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Close Large Object',nil);
end;

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZPostgreSQLBlob.IsEmpty: Boolean;
begin
  ReadBlob;
  Result := inherited IsEmpty;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZPostgreSQLBlob.Clone: IZBlob;
begin
  Result := TZPostgreSQLBlob.Create(FPlainDriver, BlobData, BlobSize,
    FHandle, FBlobOid, FChunk_Size);
end;

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZPostgreSQLBlob.GetStream: TStream;
begin
  ReadBlob;
  Result := inherited GetStream;
end;

end.
