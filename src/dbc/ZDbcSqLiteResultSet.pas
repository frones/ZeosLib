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
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types, Contnrs{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcCache, ZDbcCachedResultSet, ZDbcGenericResolver;

{$IF NOT DECLARED(JulianEpoch)} // sysutils/datih.inc
const
  JulianEpoch = -2415018.5; // "julian day 0" is January 1, 4713 BC 12:00AM
{$IFEND}
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
    FErrorCode: Integer;
    FHandle: Psqlite;
    FStmtHandle: Psqlite_vm;
    FColumnCount: Integer;
    FPlainDriver: IZSQLitePlainDriver;
    FFreeHandle: Boolean;
    FFirstRow: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
  protected
    procedure Open; override;
    procedure FreeHandle;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(PlainDriver: IZSQLitePlainDriver; Statement: IZStatement;
      SQL: string; const Handle: Psqlite; const StmtHandle: Psqlite_vm;
      const ErrorCode: Integer; const AllowFreeHandle: Boolean = True); overload;

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetAnsiRec(ColumnIndex: Integer): TZAnsiRec; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
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
  ZMessages, ZDbcSqLite, ZDbcSQLiteUtils, ZEncoding, ZDbcLogging, ZFastCode,
  ZVariant
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

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
  Statement: IZStatement; SQL: string; const Handle: Psqlite;
  const StmtHandle: Psqlite_vm; const ErrorCode: Integer;
  const AllowFreeHandle: Boolean = True);
begin
  inherited Create(Statement, SQL, TZSQLiteResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  FStmtHandle := StmtHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FFreeHandle := AllowFreeHandle;
  FErrorCode := ErrorCode;
  FUndefinedVarcharAsStringLength := StrToIntDef(Statement.GetConnection.GetParameters.Values['Undefined_Varchar_AsString_Length'], 0);
  FFirstRow := True;

  Open;
end;

{**
  Opens this recordset.
}
procedure TZSQLiteResultSet.Open;
var
  I, UndefinedVarcharAsStringLength: Integer;
  ColumnInfo: TZColumnInfo;
  FieldPrecision: Integer;
  FieldDecimals: Integer;
  TypeName: PAnsiChar;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  UndefinedVarcharAsStringLength := (GetStatement.GetConnection as IZSQLiteConnection).GetUndefinedVarcharAsStringLength;
  FColumnCount := FPlainDriver.column_count(FStmtHandle);

  LastRowNo := 0;
  //MaxRows := FPlainDriver.data_count(FStmtHandle) +1; {first ResultSetRow = 1}

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FColumnCount-1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(FPlainDriver.column_name(FStmtHandle, i),
        ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
      TableName := '';
      ReadOnly := False;
      TypeName := FPlainDriver.column_decltype(FStmtHandle, i);
      if TypeName = nil then
        ColumnType := ConvertSQLiteTypeToSQLType(FPlainDriver.column_type_AsString(FStmtHandle, i),
          UndefinedVarcharAsStringLength, FieldPrecision, FieldDecimals,
          ConSettings.CPType)
      else
        ColumnType := ConvertSQLiteTypeToSQLType(TypeName,
          UndefinedVarcharAsStringLength, FieldPrecision, FieldDecimals,
          ConSettings.CPType);


      if ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnCodePage := zCP_UTF8;
        if ColumnType = stString then
          if Zencoding.ZDefaultSystemCodePage = zCP_UTF8 then
            ColumnDisplaySize := FieldPrecision div 4
          else
            ColumnDisplaySize := FieldPrecision div 2;

        if ColumnType = stUnicodeString then
          ColumnDisplaySize := FieldPrecision div 2;
      end
      else
        ColumnCodePage := zCP_NONE;

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
begin
  if FFreeHandle then
  begin
    if Assigned(FStmtHandle) then
      ErrorCode := FPlainDriver.Finalize(FStmtHandle)
    else
      ErrorCode := SQLITE_OK;
    FStmtHandle := nil;
    CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil,
      lcOther, 'FINALIZE SQLite VM', ConSettings);
  end
  else
  begin
    ErrorCode := FPlainDriver.reset(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil, lcBindPrepStmt, 'Reset Prepared Stmt', ConSettings);
    FErrorCode := SQLITE_DONE;
  end;
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
begin
  Result := FPlainDriver.column_type(FStmtHandle, ColumnIndex -1) = SQLITE_NULL;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetAnsiRec(ColumnIndex: Integer): TZAnsiRec;
begin
  Result.P := FPlainDriver.column_text(FStmtHandle, ColumnIndex -1);
  Result.Len := FPlainDriver.column_bytes(FStmtHandle, ColumnIndex -1);
  LastWasNull := Result.P = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := FPlainDriver.column_text(FStmtHandle, ColumnIndex -1);
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
function TZSQLiteResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Buffer := FPlainDriver.column_text(FStmtHandle, ColumnIndex-1);
  LastWasNull := Buffer = nil;
  if LastWasNull then
    Result := ''
  else
    Result := Buffer;
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
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := False
  else
    case ColType of
      SQLITE_INTEGER:
        Result := FPlainDriver.column_int(FStmtHandle, ColumnIndex) <> 0;
      SQLITE_FLOAT:
        Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex) <> 0;
      SQLITE3_TEXT:
        Result := StrToBoolEx(FPlainDriver.column_text(FStmtHandle, ColumnIndex));
      else Result := False; {SQLITE_BLOB}
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLiteResultSet.GetByte(ColumnIndex: Integer): Byte;
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := Byte(FPlainDriver.column_int(FStmtHandle, ColumnIndex));
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := SmallInt(FPlainDriver.column_int(FStmtHandle, ColumnIndex));
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.column_int(FStmtHandle, ColumnIndex);
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.column_int64(FStmtHandle, ColumnIndex);
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex);
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex);
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex);
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
var
  ColType: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := nil
  else
    Result := FPlainDriver.column_blob_AsBytes(FStmtHandle, ColumnIndex);
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
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: Cardinal;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER, SQLITE_FLOAT:
        Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex);
      else
        begin
          Buffer := FPlainDriver.column_text(FStmtHandle, ColumnIndex);
          Len := FPlainDriver.column_bytes(FStmtHandle, ColumnIndex);

          if (Len = ConSettings^.ReadFormatSettings.DateFormatLen) then
            Result := RawSQLDateToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed)
          else
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
              RawSQLTimeStampToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed));
        end;
      LastWasNull := Result = 0;
    end;
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
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: Cardinal;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER, SQLITE_FLOAT:
        Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex)+JulianEpoch;
      else
        begin
          Buffer := FPlainDriver.column_text(FStmtHandle, ColumnIndex);
          Len := FPlainDriver.column_bytes(FStmtHandle, ColumnIndex);

          if ((Buffer)+2)^ = ':' then //possible date if Len = 10 then
            Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
          else
            Result := Frac(RawSQLTimeStampToDateTime(Buffer, Len,
              ConSettings^.ReadFormatSettings, Failed));
        end;
      LastWasNull := Result = 0;
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
function TZSQLiteResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: Cardinal;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  ColumnIndex := ColumnIndex -1;
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER, SQLITE_FLOAT:
        Result := FPlainDriver.column_double(FStmtHandle, ColumnIndex);
      else
        begin
          Buffer := FPlainDriver.column_text(FStmtHandle, ColumnIndex);
          Len := FPlainDriver.column_bytes(FStmtHandle, ColumnIndex);

          Result := RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed);
        end;
      LastWasNull := Result = 0;
    end;
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
  ColType: Integer;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  ColType := FPlainDriver.column_type(FStmtHandle, ColumnIndex-1);
  ColumnIndex := ColumnIndex -1;

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Exit
  else
    try
      case GetMetadata.GetColumnType(ColumnIndex+1) of
        stAsciiStream, stUnicodeStream:
          Result := TZAbstractClob.CreateWithData(
            FPlainDriver.column_text(FStmtHandle, ColumnIndex),
            FPlainDriver.column_bytes(FStmtHandle, ColumnIndex),
            zCP_UTF8, ConSettings);
        stBinaryStream:
           Result := TZAbstractBlob.CreateWithData(FPlaindriver.column_blob(FStmtHandle,ColumnIndex), FPlainDriver.column_bytes(FStmtHandle, ColumnIndex));
        else
          Result := TZAbstractBlob.CreateWithStream(nil);
      end;
    finally
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
begin
  { Checks for maximum row. }
  Result := False;
  if ((MaxRows > 0) and (RowNo >= MaxRows)) or (FErrorCode = SQLITE_DONE) then //previously set by stmt or Next
  begin
    { Free handle when EOF. }
    FreeHandle;
    Exit;
  end;

  if ((MaxRows > 0) and (RowNo >= MaxRows)) then
    Exit;

  if Assigned(FStmtHandle) and not FFirstRow then
  begin
    FErrorCode := FPlainDriver.Step(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, nil, lcOther, 'FETCH', ConSettings);
  end;

  if FFirstRow then //avoid incrementing issue on fetching since the first row is allready fetched by stmt
  begin
    FFirstRow := False;
    Result := (FErrorCode = SQLITE_ROW);
    RowNo := 1;
  end
  else
    if (FErrorCode = SQLITE_ROW) then
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

  { Free handle when EOF. }
  if not Result then
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
