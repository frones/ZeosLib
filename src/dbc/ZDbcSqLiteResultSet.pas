{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
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

unit ZDbcSqLiteResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, Types, ZSysUtils, ZDbcIntfs,
  Contnrs, ZDbcResultSet, ZDbcResultSetMetadata, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcCache, ZDbcCachedResultSet, ZDbcGenericResolver;

type

  {** Implements SQLite ResultSet Metadata. }
  TZSQLiteResultSetMetadata = class(TZAbstractResultSetMetadata)
  public
//    function IsAutoIncrement(Column: Integer): Boolean; override;
    function IsNullable(Column: Integer): TZColumnNullableType; override;
  end;

  {** Implements SQLite ResultSet. }
  TZSQLiteResultSet = class(TZAbstractResultSet)
  private
    FHandle: Psqlite;
    FStmtHandle: Psqlite_vm;
    FColumnCount: Integer;
    FColumnNames: PPAnsiChar;
    FColumnValues: PPAnsiChar;
    FPlainDriver: IZSQLitePlainDriver;
  protected
    procedure Open; override;
    procedure FreeHandle;
    function InternalGetString(ColumnIndex: Integer): ZAnsiString; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver; Statement: IZStatement;
      SQL: string; Handle: Psqlite; StmtHandle: Psqlite_vm;
      ColumnCount: Integer; ColumnNames: PPAnsiChar; ColumnValues: PPAnsiChar);
    destructor Destroy; override;

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPChar(ColumnIndex: Integer): PChar; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): ShortInt; override;
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

    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with SQLite specific functionality. }
  TZSQLiteCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
    FAutoColumnIndex: Integer;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver; Handle: Psqlite;
      Statement: IZStatement; Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    function FormCalculateStatement(Columns: TObjectList): string; override;

    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver); override;
  end;

implementation

uses
  ZMessages, ZDbcSqLite, ZDbcSQLiteUtils, ZMatchPattern, ZEncoding,
  ZDbcLogging;

{ TZSQLiteResultSetMetadata }

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
{
function TZSQLiteResultSetMetadata.IsAutoIncrement(Column: Integer): Boolean;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[Column - 1]).AutoIncrement;
end;
}

{**
  Indicates the nullability of values in the designated column.
  @param column the first column is 1, the second is 2, ...
  @return the nullability status of the given column; one of <code>columnNoNulls</code>,
    <code>columnNullable</code> or <code>columnNullableUnknown</code>
}
function TZSQLiteResultSetMetadata.IsNullable(Column: Integer):
  TZColumnNullableType;
begin
  if IsAutoIncrement(Column) then
    Result := ntNullable
  else
    Result := inherited IsNullable(Column);
end;

{ TZSQLiteResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native SQLite plain driver.
  @param Statement a related SQL statement object.
  @param Handle a SQLite specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZSQLiteResultSet.Create(PlainDriver: IZSQLitePlainDriver;
  Statement: IZStatement; SQL: string; Handle: Psqlite;
  StmtHandle: Psqlite_vm; ColumnCount: Integer; ColumnNames: PPAnsiChar;
  ColumnValues: PPAnsiChar);
begin
  inherited Create(Statement, SQL, TZSQLiteResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  FStmtHandle := StmtHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FColumnCount := ColumnCount;
  FColumnNames := ColumnNames;
  FColumnValues := ColumnValues;

  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLiteResultSet.Destroy;
begin
  //ZPlainSQLLiteDriver.Step : AllocMem(SizeOf(PPAnsiChar)*(pN+1)); // Leak, if not freed ! [HD, 05.10.2007]
  if FColumnValues <> nil then
    FreeMem(FColumnValues, Sizeof(PPAnsiChar) * (fColumnCount + 1));
  FColumnValues := nil;

   //ZPlainSQLLiteDriver.Step : AllocMem(SizeOf(PPAnsiChar)*(pN+1)*2); // Leak, if not freed ! [HD, 05.10.2007]
  if FColumnNames <> nil then
    FreeMem(FColumnNames, Sizeof(PPAnsiChar) * (fColumnCount + 1) * 2);
  FColumnNames := nil;

  inherited Destroy;
end;

{**
  Opens this recordset.
}
procedure TZSQLiteResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldName: PPAnsiChar;
  FieldPrecision: Integer;
  FieldDecimals: Integer;
  TypeName: PPAnsiChar;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  LastRowNo := 0;

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldName := FColumnNames;
  TypeName := FColumnNames;
  Inc(TypeName, FColumnCount);
  for I := 1 to FColumnCount do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      ColumnLabel := ZDbcString(StrPas(FieldName^));
      Inc(FieldName);
      TableName := '';
      ReadOnly := False;
      if TypeName^ <> nil then
      begin
        ColumnType := ConvertSQLiteTypeToSQLType(ZDbcString(TypeName^),
          FieldPrecision, FieldDecimals, ConSettings.CPType);
        Inc(TypeName);
      end
      else
      begin
        ColumnType := ConvertSQLiteTypeToSQLType(FPlainDriver.GetColumnDataType(FStmtHandle,I-1),
          FieldPrecision, FieldDecimals, ConSettings.CPType);
      end;
      if ColumnType = stString then
        if Zencoding.ZDefaultSystemCodePage = zCP_UTF8 then
          ColumnDisplaySize := FieldPrecision div 4
        else
          ColumnDisplaySize := FieldPrecision div 2;

      if ColumnType = stUnicodeString then
        ColumnDisplaySize := FieldPrecision div 2;

      AutoIncrement := False;
      Precision := FieldPrecision;
      Scale := FieldDecimals;
      Signed := True;
      Nullable := ntNullable;
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Frees statement handle.
}
procedure TZSQLiteResultSet.FreeHandle;
var
  ErrorCode: Integer;
  ErrorMessage: PAnsiChar;
begin
  ErrorMessage := nil;
  if Assigned(FStmtHandle) then
    ErrorCode := FPlainDriver.Finalize(FStmtHandle, ErrorMessage)
  else
    ErrorCode := SQLITE_OK;
  FStmtHandle := nil;
  CheckSQLiteError(FPlainDriver, ErrorCode, ErrorMessage,
    lcOther, 'FINALIZE SQLite VM');
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
procedure TZSQLiteResultSet.Close;
begin
  inherited Close;
  FreeHandle;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZSQLiteResultSet.IsNull(ColumnIndex: Integer): Boolean;
var
  Temp: PPAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (LastRowNo = 0) or (FColumnValues = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Temp := FColumnValues;
  Inc(Temp, ColumnIndex - 1);
  Result := (Temp^ = nil);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetPChar(ColumnIndex: Integer): PChar;
var
  TempStr: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (LastRowNo = 0) or (FColumnValues = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  TempStr := GetString(ColumnIndex);
  Result := PChar(TempStr);
  LastWasNull := Result = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.InternalGetString(ColumnIndex: Integer): ZAnsiString;
var
  Temp: PPAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (LastRowNo = 0) or (FColumnValues = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Temp := FColumnValues;
  Inc(Temp, ColumnIndex - 1);
  Result := Temp^;
  LastWasNull := Result = '';
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZSQLiteResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
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
function TZSQLiteResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := ShortInt(StrToIntDef(String(InternalGetString(ColumnIndex)), 0));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLiteResultSet.GetShort(ColumnIndex: Integer): SmallInt;
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
function TZSQLiteResultSet.GetInt(ColumnIndex: Integer): Integer;
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
function TZSQLiteResultSet.GetLong(ColumnIndex: Integer): Int64;
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
function TZSQLiteResultSet.GetFloat(ColumnIndex: Integer): Single;
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
function TZSQLiteResultSet.GetDouble(ColumnIndex: Integer): Double;
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
function TZSQLiteResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
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
function TZSQLiteResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
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
function TZSQLiteResultSet.GetDate(ColumnIndex: Integer): TDateTime;
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
function TZSQLiteResultSet.GetTime(ColumnIndex: Integer): TDateTime;
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
function TZSQLiteResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Temp: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Temp := String(InternalGetString(ColumnIndex));
  if IsMatch('????-??-??*', Temp) then
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
function TZSQLiteResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
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
      case TZAbstractResultSetMetadata(Metadata).GetColumnType(ColumnIndex) of
        stAsciiStream:
          if ConSettings.AutoEncode then
            Stream := TStringStream.Create(GetValidatedAnsiString(InternalGetString(ColumnIndex), ConSettings, True))
          else
            Stream := TStringStream.Create(InternalGetString(ColumnIndex));
        stUnicodeStream: Stream := GetValidatedUnicodeStream(InternalGetString(ColumnIndex), ConSettings, True);
      else
        {introduced the old Zeos6 blob-encoding cause of compatibility reasons}
        if (Statement.GetConnection as IZSQLiteConnection).UseOldBlobEncoding then
          Stream := TStringStream.Create(DecodeString(InternalGetString(ColumnIndex)))
        else
          Stream := FPlaindriver.getblob(FStmtHandle,columnIndex);
      end;
      Result := TZAbstractBlob.CreateWithStream(Stream, GetStatement.GetConnection);
    end
    else
      Result := TZAbstractBlob.CreateWithStream(nil, GetStatement.GetConnection);
  finally
    if Assigned(Stream) then
      Stream.Free;
  end;
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
function TZSQLiteResultSet.Next: Boolean;
var
  ErrorCode: Integer;
begin
  { Checks for maximum row. }
  Result := False;

  if (MaxRows > 0) and (RowNo >= MaxRows) then
    Exit;

  if LastRowNo = 0 then
  begin
    Result := FColumnValues <> nil;
    if Result then
    begin
      LastRowNo := LastRowNo + 1;
      RowNo := RowNo + 1;
    end
    else
    begin
      if RowNo <= LastRowNo then
        RowNo := LastRowNo + 1;
    end;
  end
  else
  begin
      //ZPlainSQLLiteDriver.Step : AllocMem(SizeOf(PPAnsiChar)*(pN+1)); // Leak, if not freed ! [HD, 05.10.2007]
    if FColumnValues <> nil then
      FreeMem(FColumnValues, Sizeof(PPAnsiChar) * (fColumnCount + 1));
    FColumnValues := nil;
    if Assigned(FStmtHandle) then
    begin
     //ZPlainSQLLiteDriver.Step : AllocMem(SizeOf(PPAnsiChar)*(pN+1)*2); // Leak, if not freed [HD, 05.10.2007]
      if FColumnNames <> nil then
        FreeMem(FColumnNames, Sizeof(PPAnsiChar) * (fColumnCount + 1) * 2);
      FColumnNames := nil;
      ErrorCode := FPlainDriver.Step(FStmtHandle, FColumnCount,
        FColumnValues, FColumnNames);
      CheckSQLiteError(FPlainDriver, ErrorCode, nil, lcOther, 'FETCH');
    end;

    if FColumnValues <> nil then
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

  { Frees handle when reads to the end. }
  if not Result and Assigned(FStmtHandle) then
    FreeHandle;
end;

{ TZSQLiteCachedResolver }

{**
  Creates a SQLite specific cached resolver object.
  @param PlainDriver a native SQLite plain driver.
  @param Handle a SQLite specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZSQLiteCachedResolver.Create(PlainDriver: IZSQLitePlainDriver;
  Handle: Psqlite; Statement: IZStatement; Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := PlainDriver;
  FHandle := Handle;

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
procedure TZSQLiteCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) then
    UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
end;

{**
 Do Tasks after Post updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZSQLiteCachedResolver.UpdateAutoIncrementFields(
  Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
var
  PlainDriver: IZSQLitePlainDriver;
begin
  inherited;

  if (FAutoColumnIndex > 0) and
     (OldRowAccessor.IsNull(FAutoColumnIndex) or (OldRowAccessor.GetValue(FAutoColumnIndex).VInteger = 0)) then
  begin
    PlainDriver := (Connection as IZSQLiteConnection).GetPlainDriver;

    NewRowAccessor.SetLong(FAutoColumnIndex, PlainDriver.LastInsertRowId(FHandle));
  end;
end;

// --> ms, 02/11/2005
{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZSQLiteCachedResolver.FormCalculateStatement(
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
// <-- ms

end.
