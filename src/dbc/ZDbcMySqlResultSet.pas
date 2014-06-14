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
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, Contnrs,
  ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZCompatibility, ZDbcCache,
  ZDbcCachedResultSet, ZDbcGenericResolver, ZDbcMySqlStatement,
  ZPlainMySqlDriver, ZPlainMySqlConstants;

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
    FCachedLob: Boolean;
    FLengthArray: PMySQLLengthArray;
    function GetBufferAndLength(ColumnIndex: Integer; var Len: ULong): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
    function GetBuffer(ColumnIndex: Integer): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Statement: IZStatement; const SQL: string;
      const Handle: PZMySQLConnect; const UseResult: Boolean;
      AffectedRows: PInteger; const CachedLob: Boolean;
      const IgnoreUseResult: Boolean = False);
    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetAnsiRec(ColumnIndex: Integer): TZAnsiRec; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
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
    FMysqlFieldTypes: array of TMysqlFieldTypes;
    FMySQLSignedFlags: TBooleanDynArray;
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    procedure Open; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver; Statement: IZStatement;
      SQL: string; Handle: PZMySQLConnect; UseResult: Boolean);

    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetAnsiRec(ColumnIndex: Integer): TZAnsiRec; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetWord(ColumnIndex: Integer): Word; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetUInt(ColumnIndex: Integer): LongWord; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
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

    function FormWhereClause(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string; override;
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

  TZMySQLUncachedBlob = class(TZAbstractUnCachedBlob)
  private
    FPlainDriver: IZMySQLPlainDriver;
    FColumnIndex: Integer;
    FRowNo: Integer;
    FQueryHandle: PZMySQLResult;
    FSize: ULong;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; const Size: ULong;
      const ColumnIndex, RowNo: Integer; const QueryHandle: PZMySQLResult);
  end;

  TZMySQLUncachedClob = class(TZAbstractUnCachedClob)
  private
    FPlainDriver: IZMySQLPlainDriver;
    FColumnIndex: Integer;
    FRowNo: Integer;
    FQueryHandle: PZMySQLResult;
    FSize: ULong;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; const Size: ULong;
      const ColumnIndex, RowNo: Integer; const QueryHandle: PZMySQLResult;
      Const ConSettings: PZConSettings);
  end;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} ZFastCode,
  ZSysUtils, ZMessages, ZDbcMySqlUtils, ZDbcMysql, ZEncoding, ZDbcUtils;

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
  Result := TZColumnInfo(ResultSet.ColumnsInfo[Column{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType;
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
constructor TZMySQLResultSet.Create(const PlainDriver: IZMySQLPlainDriver;
  const Statement: IZStatement; const SQL: string;
  const Handle: PZMySQLConnect; const UseResult: Boolean;
  AffectedRows: PInteger; const CachedLob: Boolean;
  const IgnoreUseResult: Boolean = False);
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
  FCachedLob := CachedLob;

  Open;
  if Assigned(AffectedRows) then
    AffectedRows^ := LastRowNo;
end;

function TZMySQLResultSet.GetBufferAndLength(ColumnIndex: Integer; var Len: ULong): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  Len := FLengthArray^[ColumnIndex];
  Result := FPlainDriver.GetFieldData(FRowHandle, ColumnIndex);
  LastWasNull := Result = nil;
end;

function TZMySQLResultSet.GetBuffer(ColumnIndex: Integer): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  Result := FPlainDriver.GetFieldData(FRowHandle, ColumnIndex);
  LastWasNull := Result = nil;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := (GetBuffer(ColumnIndex) = nil);
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
function TZMySQLResultSet.GetAnsiRec(ColumnIndex: Integer): TZAnsiRec;
begin
  Result.P := GetBufferAndLength(ColumnIndex, Result{%H-}.Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var
  Len: ULong;
begin
  Result := GetBufferAndLength(ColumnIndex, Len{%H-});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: ULong;
  Buffer: PAnsiChar;
begin
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  if LastWasNull then
    Result := ''
  else
    ZSetString(Buffer, Len, Result);
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
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := False
  else
    Result := StrToBoolEx(Buffer, True, False);
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
var
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else
    Result := RawToIntDef(Buffer, 0);
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
var
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else
    Result := RawToInt64Def(Buffer, 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else
    Result := RawToUInt64Def(Buffer, 0);
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
var
  Len: ULong;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
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
var
  Len: ULong;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
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
var
  Len: ULong;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
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
function TZMySQLResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  Len: ULong;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  SetLength(Result, Len);
  if Len > 0 then
    System.Move(Buffer^, Pointer(Result)^, Len);
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
  Len: ULong;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
  begin
    if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
      Result := RawSQLDateToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
        RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZMySQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Len: ULong;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
  begin
    if (Buffer+2)^ = ':' then //possible date if Len = 10 then
      Result := RawSQLTimeToDateTime(Buffer,Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      Result := Frac(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZMySQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Len: ULong;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    if (Buffer+2)^ = ':' then
      Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
        Result := RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
      else
        Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed);
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
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  if LastWasNull then
    Result := nil
  else
    case GetMetaData.GetColumnType(ColumnIndex) of
      stBytes, stBinaryStream:
        if FCachedLob then
          Result := TZAbstractBlob.CreateWithData(Buffer, Len)
        else
          Result := TZMySQLUncachedBlob.Create(FPlainDriver, Len,
            ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}, Rowno-1, FQueryHandle);
      else
        if FCachedLob then
          Result := TZAbstractClob.CreateWithData(Buffer, Len,
            ConSettings^.ClientCodePage^.CP, ConSettings)
        else
          Result := TZMySQLUncachedClob.Create(FPlainDriver, Len,
            ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}, Rowno-1,
            FQueryHandle, ConSettings);
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
  if Result then
    FLengthArray := FPlainDriver.FetchLengths(FQueryHandle)
  else
    FLengthArray := nil;
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
  if FQueryHandle <> nil then
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
  if Result then
    FLengthArray := FPlainDriver.FetchLengths(FQueryHandle)
  else
    FLengthArray := nil;
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
  SetLength(FMysqlFieldTypes, FieldCount);
  SetLength(FMySQLSignedFlags, FieldCount);

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
    FMysqlFieldTypes[I] := FPlainDriver.GetFieldType(FieldHandle); //save exact MySQL type
    FMySQLSignedFlags[i] := ColumnInfo.Signed;
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
  Result := FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null =1;
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
function TZMySQLPreparedResultSet.GetAnsiRec(ColumnIndex: Integer): TZAnsiRec;
var
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
  begin
    Result.P := nil;
    Result.Len := 0;
  end
  else
  begin
    case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          FRawTemp := IntToRaw(PShortInt(FColumnArray[ColumnIndex].buffer)^)
        else
          FRawTemp := IntToRaw(PByte(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          FRawTemp := IntToRaw(PSmallInt(FColumnArray[ColumnIndex].buffer)^)
        else
          FRawTemp := IntToRaw(PWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          FRawTemp := IntToRaw(PLongInt(FColumnArray[ColumnIndex].buffer)^)
        else
          FRawTemp := IntToRaw(PLongWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_FLOAT:
        FRawTemp := FloatToSQLRaw(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:
        FRawTemp := FloatToSQLRaw(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:
        FRawTemp := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          FRawTemp := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          FRawTemp := IntToRaw(PInt64(FColumnArray[ColumnIndex].buffer)^)
        else
          FRawTemp := IntToRaw(PUInt64(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint have 2Byte, integer have 4Byte but int24 have 3Byte!
        if FMySQLSignedFlags[ColumnIndex] then
          FRawTemp := IntToRaw(PInteger(FColumnArray[ColumnIndex].buffer)^)
        else
          FRawTemp := IntToRaw(PLongWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          FRawTemp := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME:
        begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          FRawTemp := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        FRawTemp := IntToRaw(PWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL, FIELD_TYPE_VARCHAR,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB,
      FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_GEOMETRY:
        begin
          Result.P := PAnsiChar(FColumnArray[ColumnIndex].buffer);
          Result.Len := FColumnArray[ColumnIndex].length;
          Exit;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end;
    Result.Len := {$IFDEF WITH_INLINE}Length(FRawTemp){$ELSE}{%H-}PLongInt(NativeUInt(FRawTemp) - 4)^{$ENDIF};
    Result.P := Pointer(FRawTemp);
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
function TZMySQLPreparedResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := GetAnsiRec(ColumnIndex).P;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZMySQLPreparedResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  TmpDateTime, TmpDateTime2: TDateTime;
  Signed: Boolean;
begin
  Signed := FBindBuffer.GetBufferIsSigned(ColumnIndex);

  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := ''
  else
  begin
    case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_TINY:
        if Signed then
          Result := IntToRaw(PShortInt(FColumnArray[ColumnIndex].buffer)^)
        else
          Result := IntToRaw(PByte(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_SHORT:
        if Signed then
          Result := IntToRaw(PSmallInt(FColumnArray[ColumnIndex].buffer)^)
        else
          Result := IntToRaw(PWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_LONG:
        if Signed then
          Result := IntToRaw(PLongInt(FColumnArray[ColumnIndex].buffer)^)
        else
          Result := IntToRaw(PLongWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_FLOAT:
        Result := FloatToSQLRaw(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:
        Result := FloatToSQLRaw(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:
        Result := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          Result := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if Signed then
          Result := IntToRaw(PInt64(FColumnArray[ColumnIndex].buffer)^)
        else
          Result := IntToRaw(PUInt64(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint have 2Byte, integer have 4Byte but int24 have 3Byte!
        if Signed then
          Result := IntToRaw(PInteger(FColumnArray[ColumnIndex].buffer)^)
        else
          Result := IntToRaw(PLongWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          Result := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME:
        begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          Result := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        Result := IntToRaw(PWord(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_BIT:
        Result := IntToRaw(PByte(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL, FIELD_TYPE_VARCHAR,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB,
      FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_GEOMETRY:
        ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer),
          FColumnArray[ColumnIndex].length, Result);
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end;
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
function TZMySQLPreparedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := False
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0)) <> 0;
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^ <> 0
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^ <> 0
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^ <> 0
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^) <> 0;
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^) <> 0;
      FIELD_TYPE_NULL:      Result := False;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := False;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^ <> 0
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^ <> 0
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^ <> 0;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := StrToBoolEx(PAnsiChar(FColumnArray[ColumnIndex].buffer), True, False);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 12{Max Int32 Length = 11} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := StrToBoolEx(FRawTemp)
        end
        else //avoid senceless processing
          Result := False;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 4{max Length = 3} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToIntDef(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 4{Max ShortInt Length = 3} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToIntDef(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetWord(ColumnIndex: Integer): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 6{Max Word Length = 5} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToIntDef(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>SmallInt</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 7{Max SmallInt Length = 6} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToIntDef(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>LongWord</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetUInt(ColumnIndex: Integer): LongWord;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 11{Max LongWord Length = 10} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToUInt64Def(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 12{Max Int32 Length = 11} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToIntDef(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZMySQLPreparedResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 21{Max UInt64 Length = 20} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToUInt64Def(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0));
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColumnArray[ColumnIndex].buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToInt64Def(PAnsiChar(FColumnArray[ColumnIndex].buffer), 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 21{Max Int64 Length = 20} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToInt64Def(FRawTemp, 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 29{Max Extende Length = 28 ??} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToFloatDef(FRawTemp, '.', 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 29{Max Extended Length = 28 ??} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToFloatDef(FRawTemp, '.', 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColumnArray[ColumnIndex].length > 0 ) and
           (FColumnArray[ColumnIndex].length < 29{Max Extended Length = 28 ??} ) then
        begin
          ZSetString(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, FRawTemp);
          Result := RawToFloatDef(FRawTemp, '.', 0)
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
function TZMySQLPreparedResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := nil
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_TINY,
      FIELD_TYPE_SHORT,
      FIELD_TYPE_LONG,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE,
      FIELD_TYPE_NULL,
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE,
      FIELD_TYPE_LONGLONG,
      FIELD_TYPE_INT24,
      FIELD_TYPE_YEAR: Result := nil;
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET,
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          SetLength(Result, FColumnArray[ColumnIndex].length);
          System.Move(Pointer(FColumnArray[ColumnIndex].buffer)^, Pointer(Result)^, FColumnArray[ColumnIndex].length);
        end
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
var
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME: Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColumnArray[ColumnIndex].buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET,
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          if FColumnArray[ColumnIndex].length = ConSettings^.ReadFormatSettings.DateFormatLen then
            Result := RawSQLDateToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer),
              FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
              RawSQLTimeStampToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer),
                FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
var
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME, FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part}, Result) then
          Result := 0;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColumnArray[ColumnIndex].buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET,
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          if (PAnsiChar(FColumnArray[ColumnIndex].buffer)+2)^ = ':' then //possible date if Len = 10 then
            Result := RawSQLTimeToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer),
              FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := Frac(RawSQLTimeStampToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer),
              FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
var
  Failed: Boolean;
  tmp: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FMysqlFieldTypes[ColumnIndex] of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
        Result := RawToFloatDef(PAnsiChar(FColumnArray[ColumnIndex].buffer), '.', 0);
      FIELD_TYPE_TINY:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PShortInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PByte(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_SHORT:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PSmallInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_LONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
            PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
            0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part}, Result) then
          Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Year,
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Month,
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Day, tmp) then
            tmp := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Hour,
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Minute,
              PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.Second,
              0{PMYSQL_TIME(FColumnArray[ColumnIndex].buffer)^.second_part}, Result) then
            Result := 0;
          Result := Result + tmp;
        end;
      FIELD_TYPE_LONGLONG:
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PInt64(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PUInt64(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_INT24: //warning Delphi deosn't have a 24 bit float -> samllint is a 2Byte, integer is a 4Byte but int24 is a 3Byte value!
        if FMySQLSignedFlags[ColumnIndex] then
          Result := PLongInt(FColumnArray[ColumnIndex].buffer)^
        else
          Result := PLongWord(FColumnArray[ColumnIndex].buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColumnArray[ColumnIndex].buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_VARCHAR, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET,
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          if (PAnsiChar(FColumnArray[ColumnIndex].buffer)+2)^ = ':' then
            Result := RawSQLTimeToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer),
              FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - FColumnArray[ColumnIndex].length) <= 4 then
              Result := RawSQLTimeStampToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := RawSQLTimeToDateTime(PAnsiChar(FColumnArray[ColumnIndex].buffer), FColumnArray[ColumnIndex].length, ConSettings^.ReadFormatSettings, Failed);
          LastWasNull := Result = 0;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
  LastWasNull := FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null =1;
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
  WS := ConSettings^.ConvFuncs.ZRawToUnicode(InternalGetString(ColumnIndex), ConSettings^.ClientCodePage^.CP);
  Result := TMemoryStream.Create;
  Result.Write(PWideChar(WS)^, Length(WS) *2);
  Result.Position := 0;
  LastWasNull := FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null =1;
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Result := TMemoryStream.Create;
  LastWasNull := FColumnArray[ColumnIndex].is_null =1;
  if not LastWasNull then
  begin
    Result.Write(FColumnArray[ColumnIndex].buffer[0], FColumnArray[ColumnIndex].length);
    Result.Position := 0;
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
function TZMySQLPreparedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  RawTemp: RawByteString;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
    case GetMetadata.GetColumnType(ColumnIndex) of
      stBinaryStream, stBytes:
        Result := TZAbstractBlob.CreateWithData(Pointer(FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].buffer),
          FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].length);
      stAsciiStream, stUnicodeStream, stString, stUnicodeString:
        Result := TZAbstractClob.CreateWithData(PAnsichar(FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].buffer),
          FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].length, ConSettings^.ClientCodePage^.CP, ConSettings);
      else
        begin
          RawTemp := InternalGetString(ColumnIndex);
          Result := TZAbstractClob.CreateWithData(PAnsiChar(RawTemp), Length(RawTemp),
            ConSettings^.ClientCodePage^.CP, ConSettings);
        end;
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
  FAutoColumnIndex := {$IFDEF GENERIC_INDEX}-1{$ELSE}0{$ENDIF};
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  end;
end;

{**
  Forms a where clause for UPDATE or DELETE DML statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZMySQLCachedResolver.FormWhereClause(Columns: TObjectList;
  OldRowAccessor: TZRowAccessor): string;
var
  I, N: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  N := Columns.Count - WhereColumns.Count;

  for I := 0 to WhereColumns.Count - 1 do
  begin
    Current := TZResolverParameter(WhereColumns[I]);
    if Result <> '' then
      Result := Result + ' AND ';

    Result := Result + IdentifierConvertor.Quote(Current.ColumnName);
    if OldRowAccessor.IsNull(Current.ColumnIndex) then
    begin
      if not (Metadata.IsNullable(Current.ColumnIndex) = ntNullable) then
        case OldRowAccessor.GetColumnType(Current.ColumnIndex) of
          stDate:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''0000-00-00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          stTime:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''00:00:00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          stTimeStamp:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''0000-00-00 00:00:00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          else
            Result := Result + ' IS NULL';
        end
      else
        Result := Result + ' IS NULL ';
      Columns.Delete(N);
    end
    else
    begin
      Result := Result + '=?';
      Inc(N);
    end;
  end;

  if Result <> '' then
    Result := ' WHERE ' + Result;
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
    UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
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
  if not ((FAutoColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF} 0) and
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

{ TZMySQLUncachedBlob }

procedure TZMySQLUncachedBlob.ReadLob;
var
  RowHandle: PZMySQLRow;
  Buffer, Data: Pointer;
begin
  FPlainDriver.SeekData(FQueryHandle, FRowNo);
  RowHandle := FPlainDriver.FetchRow(FQueryHandle);
  Data := FPlainDriver.GetFieldData(RowHandle, FColumnIndex);
  FBlobSize  := FSize;
  Buffer := AllocMem(FBlobSize);
  System.Move(Data^, Buffer^, FBlobSize);
  FBlobData := Buffer;
  inherited ReadLob;
end;

{**
  Constructs this class and assignes the main properties.
  @param MySQL plaindriver.
  @ColumnIndex the ColumnIndex.
  @param RowNo the RowNo of the Lob.
  @param QueryHandle the PZMySQLResult.
}
constructor TZMySQLUncachedBlob.Create(const PlainDriver: IZMySQLPlainDriver;
  const Size: ULong; const ColumnIndex, RowNo: Integer;
  const QueryHandle: PZMySQLResult);
begin
  FPlainDriver := PlainDriver;
  FColumnIndex := ColumnIndex;
  FSize := Size;
  FRowNo := RowNo;
  FQueryHandle := QueryHandle;
end;

{ TZMySQLUncachedClob }

procedure TZMySQLUncachedClob.ReadLob;
var
  RowHandle: PZMySQLRow;
  Buffer: PAnsiChar;
begin
  FPlainDriver.SeekData(FQueryHandle, FRowNo);
  RowHandle := FPlainDriver.FetchRow(FQueryHandle);
  Buffer := FPlainDriver.GetFieldData(RowHandle, FColumnIndex);
  InternalSetPAnsiChar(Buffer, FConSettings^.ClientCodePage^.CP, FSize);
  inherited ReadLob;
end;

{**
  Constructs this class and assignes the main properties.
  @param MySQL plaindriver.
  @ColumnIndex the ColumnIndex.
  @param RowNo the RowNo of the Lob.
  @param QueryHandle the PZMySQLResult.
}
constructor TZMySQLUncachedClob.Create(const PlainDriver: IZMySQLPlainDriver;
  const Size: ULong; const ColumnIndex, RowNo: Integer;
  const QueryHandle: PZMySQLResult; const ConSettings: PZConSettings);
begin
  FPlainDriver := PlainDriver;
  FColumnIndex := ColumnIndex;
  FRowNo := RowNo;
  FQueryHandle := QueryHandle;
  FConSettings := ConSettings;
  FSize := Size;
end;

end.
