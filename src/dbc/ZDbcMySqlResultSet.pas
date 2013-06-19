{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, Types, ZSysUtils, ZDbcIntfs,
  Contnrs, ZDbcResultSet, ZDbcResultSetMetadata,
  ZCompatibility, ZDbcCache, ZDbcCachedResultSet, ZDbcGenericResolver,
  ZDbcMySqlStatement, ZPlainMySqlDriver, ZPlainMySqlConstants;

type
  {** Implements MySQL ResultSet Metadata. }
  TZMySQLResultSetMetadata = class(TZAbstractResultSetMetadata)
  public
    function GetColumnType(Column: Integer): TZSQLType; override;
  end;

  {** Implements MySQL ResultSet. }
  TZMySQLResultSet = class(TZAbstractResultSet)
  private
    FHandle: PZMySQLConnect;
    FQueryHandle: PZMySQLResult;
    FRowHandle: PZMySQLRow;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;
    FIgnoreUseResult: Boolean;
    TempStr: String;
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): ZAnsistring; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver; Statement: IZStatement;
      SQL: string; Handle: PZMySQLConnect; UseResult: Boolean;
      AffectedRows: PInteger; IgnoreUseResult: Boolean = False);
    destructor Destroy; override;

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPChar(ColumnIndex: Integer): PChar; override;
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
    function Next: Boolean; override;
    procedure ReleaseHandle;
  end;

  {** Implements Prepared MySQL ResultSet. }
  TZMySQLPreparedResultSet = class(TZAbstractResultSet)
  private
    FHandle: PZMySQLConnect;
    FPrepStmt: PZMySqlPrepStmt;
    FResultMetaData : PZMySQLResult;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult: Boolean;
    FColumnArray: TZMysqlColumnBuffer;
    FBindBuffer: TZMySqlResultSetBindBuffer;
    function bufferasint64(ColumnIndex: Integer): Int64;
    function bufferasextended(ColumnIndex: Integer): Extended;

  protected
    function InternalGetString(ColumnIndex: Integer): ZAnsistring; override;
    procedure Open; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver; Statement: IZStatement;
      SQL: string; Handle: PZMySQLConnect; UseResult: Boolean);
    destructor Destroy; override;

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
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
    function GetAsciiStream(ColumnIndex: Integer): TStream; override;
    function GetUnicodeStream(ColumnIndex: Integer): TStream; override;
    function GetBinaryStream(ColumnIndex: Integer): TStream; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function MoveAbsolute(Row: Integer): Boolean; override;
    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with MySQL specific functionality. }
  TZMySQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
    FAutoColumnIndex: Integer;
    FStatement: IZMysqlStatement;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver; Handle: PZMySQLConnect;
      Statement: IZMysqlStatement; Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    // --> ms, 31/10/2005
    function FormCalculateStatement(Columns: TObjectList): string; override;
    // <-- ms
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver); override;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
  end;

implementation

uses
  Math, ZMessages, ZDbcMySqlUtils, ZMatchPattern, ZDbcMysql, ZEncoding;

{ TZMySQLResultSetMetadata }

{**
  Retrieves the designated column's SQL type.
  @param column the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZMySQLResultSetMetadata.GetColumnType(Column: Integer): TZSQLType;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[Column - 1]).ColumnType;
end;

{ TZMySQLResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZMySQLResultSet.Create(PlainDriver: IZMySQLPlainDriver;
  Statement: IZStatement; SQL: string; Handle: PZMySQLConnect;
  UseResult: Boolean; AffectedRows: PInteger; IgnoreUseResult: Boolean = False);
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
      Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  FQueryHandle := nil;
  FRowHandle := nil;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FUseResult := UseResult;
  FIgnoreUseResult := IgnoreUseResult;

  Open;
  if Assigned(AffectedRows) then
    AffectedRows^ := LastRowNo;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLResultSet.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens this recordset.
}
procedure TZMySQLResultSet.Open;
var
  I: Integer;
  FieldHandle: PZMySQLField;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if FUseResult and (not FIgnoreUseResult) then
  begin
    FQueryHandle := FPlainDriver.UseResult(FHandle);
    LastRowNo := 0;
  end
  else
  begin
    FQueryHandle := FPlainDriver.StoreResult(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.GetRowCount(FQueryHandle)
    else
      LastRowNo := 0;
  end;

  if not Assigned(FQueryHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FPlainDriver.GetFieldCount(FQueryHandle) - 1 do
  begin
    FPlainDriver.SeekField(FQueryHandle, I);
    FieldHandle := FPlainDriver.FetchField(FQueryHandle);
    if FieldHandle = nil then
      Break;

    ColumnsInfo.Add(GetMySQLColumnInfoFromFieldHandle(FPlainDriver,
     FieldHandle, ConSettings, FUseResult));
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
procedure TZMySQLResultSet.Close;
begin
  if FQueryHandle <> nil then
  begin
    FPlainDriver.FreeResult(FQueryHandle);
    while(FPlainDriver.RetrieveNextRowset(FHandle) = 0) do
    begin
      FQueryHandle := FPlainDriver.StoreResult(FHandle);
      if FQueryHandle <> nil then
      begin
        FPlainDriver.FreeResult(FQueryHandle);
      end;
    end;
  end;
  FQueryHandle := nil;
  FRowHandle := nil;
  inherited Close;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZMySQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
var
   Temp: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Temp := FPlainDriver.GetFieldData(FRowHandle, ColumnIndex - 1);
  Result := (Temp = nil);
  if not Result and (TZAbstractResultSetMetadata(Metadata).
    GetColumnType(ColumnIndex) in [stDate, stTimestamp]) then
  begin
    Result := (AnsiSQLDateToDateTime(String(Temp)) = 0)
      and (TimestampStrToDateTime(String(Temp)) = 0);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.GetPChar(ColumnIndex: Integer): PChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  TempStr := ZDbcString(FPlainDriver.GetFieldData(FRowHandle, ColumnIndex - 1));
  Result := PChar(TempStr);
  LastWasNull := Result = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.InternalGetString(ColumnIndex: Integer): ZAnsistring;
var
  LengthPointer: PULong;
  Length: ULong;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  ColumnIndex := ColumnIndex - 1;
  LengthPointer := FPlainDriver.FetchLengths(FQueryHandle);
  if LengthPointer <> nil then
    Length  := PULong(NativeUint(LengthPointer) + NativeUInt(ColumnIndex) * SizeOf(ULOng))^
  else
    Length := 0;
  Buffer := FPlainDriver.GetFieldData(FRowHandle, ColumnIndex);
  LastWasNull := Buffer = nil;
  Result := '';
  if not LastWasNull then
  {$IFDEF WITH_RAWBYTESTRING}
  begin
    SetLength(Result, Length);
    Move(Buffer^, PAnsiChar(Result)^, Length);
  end;
  {$ELSE}
    SetString(Result, Buffer, Length);
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZMySQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Temp := UpperCase(String(InternalGetString(ColumnIndex)));
  Result := (Temp = 'Y') or (Temp = 'YES') or (Temp = 'T') or
    (Temp = 'TRUE') or (StrToIntDef(Temp, 0) <> 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLResultSet.GetByte(ColumnIndex: Integer): Byte;
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
function TZMySQLResultSet.GetShort(ColumnIndex: Integer): SmallInt;
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
function TZMySQLResultSet.GetInt(ColumnIndex: Integer): Integer;
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
function TZMySQLResultSet.GetLong(ColumnIndex: Integer): Int64;
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
function TZMySQLResultSet.GetFloat(ColumnIndex: Integer): Single;
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
function TZMySQLResultSet.GetDouble(ColumnIndex: Integer): Double;
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
function TZMySQLResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
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
function TZMySQLResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(InternalGetString(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Value: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Value := String(InternalGetString(ColumnIndex));

  LastWasNull := (LastWasNull or (Copy(Value, 1, 10)='0000-00-00'));
  if LastWasNull then
  begin
    Result := 0;
    Exit;
  end;

  if IsMatch('????-??-??*', Value) then
    Result := Trunc(AnsiSQLDateToDateTime(Value))
  else
    Result := Trunc(TimestampStrToDateTime(Value));
  LastWasNull := Result = 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Value: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Value := String(InternalGetString(ColumnIndex));

  LastWasNull := (LastWasNull or (Copy(Value, 1, 8)='00:00:00'));
  if LastWasNull then
  begin
    Result := 0;
    Exit;
  end;

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
function TZMySQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Temp := String(GetPChar(ColumnIndex));

  if LastWasNull then
  begin
    Result := 0;
    Exit;
  end;

  if IsMatch('????-??-??*', Temp) or IsMatch('??:??:??*', Temp) then
    Result := AnsiSQLDateToDateTime(Temp)
  else
    Result := TimestampStrToDateTime(Temp);
  LastWasNull := Result = 0;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZMySQLResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Stream: TStream;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Stream := nil;
  try
    if not IsNull(ColumnIndex) then
    begin
      case GetMetaData.GetColumnType(ColumnIndex) of
        stAsciiStream: Stream := TStringStream.Create(GetValidatedAnsiString(InternalGetString(ColumnIndex), ConSettings, True));
        stUnicodeStream: Stream := GetValidatedUnicodeStream(InternalGetString(ColumnIndex), ConSettings, True);
        else
          Stream := TStringStream.Create(InternalGetString(ColumnIndex));
      end;
      if not Assigned(Stream) then //improve TZTestCompMySQLBugReport.Test1045286
        Stream := TMemoryStream.Create;
      Result := TZAbstractBlob.CreateWithStream(Stream, GetStatement.GetConnection,
        GetMetaData.GetColumnType(ColumnIndex) = stUnicodeStream);
    end
    else
      Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection);
  finally
    if Assigned(Stream) then
      Stream.Free;
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
function TZMySQLResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  CheckClosed;

  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  if not FUseResult then
  begin
    { Process negative rows. }
    if Row < 0 then
    begin
      Row := LastRowNo - Row + 1;
      if Row < 0 then
         Row := 0;
    end;

    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      if (Row >= 1) and (Row <= LastRowNo) then
      begin
        FPlainDriver.SeekData(FQueryHandle, RowNo - 1);
        FRowHandle := FPlainDriver.FetchRow(FQueryHandle);
      end
      else
        FRowHandle := nil;
    end;
    Result := FRowHandle <> nil;
  end
  else
    RaiseForwardOnlyException;
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
function TZMySQLResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (RowNo >= MaxRows) then
    Exit;
  if Assigned(FQueryHandle) then
    FRowHandle := FPlainDriver.FetchRow(FQueryHandle);
  if FRowHandle <> nil then
  begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end
  else
  begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;
end;

procedure TZMySQLResultSet.ReleaseHandle;
begin
  if FQueryHandle <> nil then
    FPlainDriver.FreeResult(FQueryHandle);
  FQueryHandle := nil;
end;
{ TZMySQLPreparedResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZMySQLPreparedResultSet.Create(PlainDriver: IZMySQLPlainDriver;
  Statement: IZStatement; SQL: string; Handle: PZMySQLConnect;
  UseResult: Boolean);
var
  tempPrepStmt : IZMysqlPreparedStatement;
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  tempPrepStmt := Statement as IZMysqlPreparedStatement;
  FPrepStmt:= tempPrepStmt.GetStmtHandle;
  FResultMetaData := nil;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FUseResult := UseResult;

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLPreparedResultSet.Destroy;
begin
  inherited Destroy;
end;

{**
  Opens this recordset.
}
procedure TZMySQLPreparedResultSet.Open;
const one = AnsiString('1');
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldHandle: PZMySQLField;
  FieldCount: Integer;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  FieldCount := FPlainDriver.GetPreparedFieldCount(FPrepStmt);
  if FieldCount = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  FResultMetaData := FPlainDriver.GetPreparedMetaData(FPrepStmt);
  if not Assigned(FResultMetaData) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  if FUseResult then
    LastRowNo := 0
  else
  begin
    FPlainDriver.StmtAttrSet(FPrepStmt,STMT_ATTR_UPDATE_MAX_LENGTH,PAnsiChar(one));
    if (FPlainDriver.StorePreparedResult(FPrepStmt)=0) then
      LastRowNo := FPlainDriver.GetPreparedNumRows(FPrepStmt)
    else
      LastRowNo := 0;
  end;

    { Initialize Bind Array and Column Array }
  FBindBuffer := TZMySqlResultSetBindBuffer.Create(FPlainDriver,FieldCount,FColumnArray);

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FPlainDriver.GetFieldCount(FResultMetaData) - 1 do
  begin
    FPlainDriver.SeekField(FResultMetaData, I);
    FieldHandle := FPlainDriver.FetchField(FResultMetaData);
    if FieldHandle = nil then
      Break;

    ColumnInfo := GetMySQLColumnInfoFromFieldHandle(FPlainDriver,
     FieldHandle, GetStatement.GetConnection.GetConSettings, FUseResult);

    ColumnsInfo.Add(ColumnInfo);

    FBindBuffer.AddColumn(FPlainDriver, FieldHandle);
  end;
  FPlainDriver.FreeResult(FResultMetaData);
  FResultMetaData := nil;

  if (FPlainDriver.BindResult(FPrepStmt,FBindBuffer.GetBufferAddress)<>0) then
    raise EZSQLException.Create(SFailedToBindResults);

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
procedure TZMySQLPreparedResultSet.Close;
begin
  if Assigned(FResultMetaData) then
    FPlainDriver.FreeResult(FResultMetaData);
  FResultMetaData := nil;
  if Assigned(FBindBuffer) then
    FreeAndNil(FBindBuffer);
  if Assigned(FPrepStmt) then
    begin
      FPlainDriver.FreePreparedResult(FPrepStmt);
      while(FPlainDriver.GetPreparedNextResult(FPrepStmt) = 0) do
        FPlainDriver.FreePreparedResult(FPrepStmt);
    end;
  inherited Close;

end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZMySQLPreparedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}

  Result := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLPreparedResultSet.InternalGetString(ColumnIndex: Integer): ZAnsiString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  Result := StrPas(PAnsiChar(FColumnArray[ColumnIndex - 1].buffer));
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZMySQLPreparedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
   Temp := UpperCase(String(InternalGetString(ColumnIndex)));
  Result := (Temp = 'Y') or (Temp = 'YES') or (Temp = 'T') or
    (Temp = 'TRUE') or (StrToIntDef(Temp, 0) <> 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetByte(ColumnIndex: Integer): Byte;
var
   full64: Int64;
   bitmask: Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  full64 := bufferasInt64(ColumnIndex);
  bitmask := $FF;
  Result := Byte(full64 and bitmask);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetShort(ColumnIndex: Integer): SmallInt;
var
    full64: Int64;
    bitmask: Int64;
Begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
 full64 := bufferasInt64(ColumnIndex);
 bitmask := $FFFFFFFF;
 Result := Integer(full64 and bitmask);

 LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := bufferasInt64(ColumnIndex);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  full64, bitmask: Int64;
Begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
 full64 := bufferasInt64(ColumnIndex);
 bitmask := $FFFFFFFF;
 Result := Int64(full64 and bitmask);
 LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := BufferAsExtended(ColumnIndex);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := BufferAsExtended(ColumnIndex);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := BufferAsExtended(ColumnIndex);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(InternalGetString(ColumnIndex));
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLPreparedResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  if not sysUtils.TryEncodeDate(PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Year,
                                PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Month,
                                PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Day,
                                Result) then
            Result := encodeDate(1900, 1, 1);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLPreparedResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  if not sysUtils.TryEncodeTime(PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Hour,
                                PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Minute,
                                PMYSQL_TIME(FColumnArray[ColumnIndex - 1].buffer)^.Second,
                                0,
                                Result) then
      Result :=  encodeTime(0,0,0,0);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := GetDate(ColumnIndex) + GetTime(ColumnIndex);
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetAsciiStream(ColumnIndex: Integer): TStream;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  Result := TStringStream.Create(InternalGetString(ColumnIndex));
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
var
  WS: ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  WS := ZDbcUnicodeString(InternalGetString(ColumnIndex));
  Result := TMemoryStream.Create;
  Result.Write(PWideChar(WS)^, Length(WS) *2);
  Result.Position := 0;
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
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
function TZMySQLPreparedResultSet.GetBinaryStream(ColumnIndex: Integer): TStream;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  Result := TMemoryStream.Create;
  Result.Write(FColumnArray[ColumnIndex - 1].buffer[0], FColumnArray[ColumnIndex - 1].length);
  Result.Position := 0;
  LastWasNull := FColumnArray[ColumnIndex-1].is_null =1;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZMySQLPreparedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Stream: TStream;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  Stream := nil;
  try
    if not LastWasNull then
    begin
      case GetMetadata.GetColumnType(ColumnIndex) of
        stBinaryStream: Stream := GetBinaryStream(ColumnIndex);
        stUnicodeStream: Stream := GetUnicodeStream(ColumnIndex);
      else
        Stream := TStringStream.Create(GetValidatedAnsiString(InternalGetString(ColumnIndex), ConSettings, True));
      end;
      Result := TZAbstractBlob.CreateWithStream(Stream, GetStatement.GetConnection,
        GetMetadata.GetColumnType(ColumnIndex) = stUnicodeStream)
    end
    else
      Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection);
  finally
    if Assigned(Stream) then
      Stream.Free;
  end;
end;

function TZMySQLPreparedResultSet.bufferasint64(ColumnIndex: Integer): Int64;
begin
   Case FColumnArray[ColumnIndex-1].length of
   1: Result := pshortint(FColumnArray[ColumnIndex-1].buffer)^;
   2: Result := psmallint(FColumnArray[ColumnIndex-1].buffer)^;
   4: Result := plongint(FColumnArray[ColumnIndex-1].buffer)^;
   8: Result := pint64(FColumnArray[ColumnIndex-1].buffer)^;
   else
    Result := 0;
   End
end;

function TZMySQLPreparedResultSet.bufferasextended(ColumnIndex: Integer): Extended;
begin
   Case FBindBuffer.GetBufferType(ColumnIndex) of
     FIELD_TYPE_FLOAT: Result := psingle(FColumnArray[ColumnIndex-1].buffer)^;
     FIELD_TYPE_DOUBLE: Result := pdouble(FColumnArray[ColumnIndex-1].buffer)^;
   else
      Result := SQLStrToFloatDef(InternalGetString(ColumnIndex), 0);
   End
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
function TZMySQLPreparedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  CheckClosed;

  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  if not FUseResult then
  begin
    { Process negative rows. }
    if Row < 0 then
    begin
      Row := LastRowNo - Row + 1;
      if Row < 0 then
         Row := 0;
    end;

    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      if (Row >= 1) and (Row <= LastRowNo) then
      begin
        FPlainDriver.SeekPreparedData(FPrepStmt, RowNo - 1);
        Result := (FPlainDriver.FetchBoundResults(FPrepStmt) =0);
      end;
    end;
  end
  else
    RaiseForwardOnlyException;
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
function TZMySQLPreparedResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (RowNo >= MaxRows) then
    Exit;

  if FPlainDriver.FetchBoundResults(FPrepStmt) in [0, MYSQL_DATA_TRUNCATED] then
  begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end
  else
  begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;
end;

{ TZMySQLCachedResolver }

{**
  Creates a MySQL specific cached resolver object.
  @param PlainDriver a native MySQL plain driver.
  @param Handle a MySQL specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZMySQLCachedResolver.Create(PlainDriver: IZMySQLPlainDriver;
  Handle: PZMySQLConnect; Statement: IZMysqlStatement; Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FStatement := Statement as IZMysqlStatement;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := 0;
  for I := 1 to Metadata.GetColumnCount do
  begin
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stInteger, stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZMySQLCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);
  if (UpdateType = utInserted) then
  begin
   UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
  end;
end;

{**
 Do Tasks after Post updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZMySQLCachedResolver.UpdateAutoIncrementFields(
  Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
var
  Plaindriver : IZMysqlPlainDriver;
begin
  inherited UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Resolver);
  if not ((FAutoColumnIndex > 0) and
          (OldRowAccessor.IsNull(FAutoColumnIndex) or (OldRowAccessor.GetValue(FAutoColumnIndex).VInteger=0))) then
     exit;
  Plaindriver := (Connection as IZMysqlConnection).GetPlainDriver;
  // THIS IS WRONG, I KNOW (MDAEMS) : which function to use depends on the insert statement, not the resultset statement
  {  IF FStatement.IsPreparedStatement  then
    NewRowAccessor.SetLong(FAutoColumnIndex, PlainDriver.GetPreparedInsertID(FStatement.GetStmtHandle))
  else}
    NewRowAccessor.SetLong(FAutoColumnIndex, PlainDriver.GetLastInsertID(FHandle));

end;

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZMySQLCachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
var
  I: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  if Columns.Count = 0 then
     Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    Current := TZResolverParameter(Columns[I]);
    if Result <> '' then
      Result := Result + ',';
    if Current.DefaultValue <> '' then
      Result := Result + Current.DefaultValue
    else
      Result := Result + 'NULL';
  end;
  Result := 'SELECT ' + Result;
end;

end.
