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

{EH: First of all read this:
  http://blog.ulf-wendel.de/2008/pdo_mysqlnd-prepared-statements-again/}

unit ZDbcMySqlResultSet;

interface

{$I ZDbc.inc}

{.$DEFINE USE_SYNCOMMONS}
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons,
{$ENDIF USE_SYNCOMMONS}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, Contnrs,
  ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZCompatibility, ZDbcCache,
  ZDbcCachedResultSet, ZDbcGenericResolver, ZDbcMySqlStatement,
  ZPlainMySqlDriver, ZPlainMySqlConstants, ZSelectSchema;

type
  {** Implements MySQL ResultSet Metadata. }
  TZMySQLResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetColumnType(ColumnIndex: Integer): TZSQLType; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  end;

  {** Implements MySQL ResultSet. }
  TZAbstractMySQLResultSet = class(TZAbstractResultSet)
  private
    FHandle: PMySQL;
    FQueryHandle: PZMySQLResult;
    FRowHandle: PZMySQLRow;
    FPlainDriver: TZMySQLPlainDriver;
    FLengthArray: PULongArray;
    FMySQLTypes: array of TMysqlFieldType;
    fServerCursor: Boolean;
    {$IFDEF USE_SYNCOMMONS}
    fBinaryResults: TBooleanDynArray;
    {$ENDIF}
    function GetBufferAndLength(ColumnIndex: Integer; var Len: NativeUInt): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
    function GetBuffer(ColumnIndex: Integer): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver;
      const Statement: IZStatement; const SQL: string; Handle: PMySQL;
      AffectedRows: PInteger; out OpenCursorCallback: TOpenCursorCallback);
    procedure Close; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
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

    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; EndJSONObject: Boolean = True;
      With_DATETIME_MAGIC: Boolean = False; SkipNullFields: Boolean = False); override;
    {$ENDIF USE_SYNCOMMONS}
    //EH: keep that override 4 all descendants: seek_data is dead slow in a forward only mode
    function Next: Boolean; override;

    procedure ResetCursor; override;
    procedure OpenCursor; virtual; abstract;
  end;

  TZMySQL_Store_ResultSet = class(TZAbstractMySQLResultSet)
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
    procedure ResetCursor; override;
    procedure OpenCursor; override;
  end;

  TZMySQL_Use_ResultSet = class(TZAbstractMySQLResultSet)
  public
    procedure ResetCursor; override;
    procedure OpenCursor; override;
  end;

  {** Implements Prepared MySQL ResultSet. }
  TZAbstractMySQLPreparedResultSet = class(TZAbstractResultSet)
  private
    FMysQL: PMySQL;
    FPrepStmt: PMYSQL;
    FPPMYSQL: PPMYSQL;
    FPlainDriver: TZMySQLPlainDriver;
    FTempBlob: IZBlob;
    fServerCursor: Boolean;
    FFetchStatus: Integer; //if FFetchStatus = MYSQL_DATA_TRUNCATED we need to read lob's with mysql_stmt_fetch_column
    FColBuffer: TBytes; //the buffer mysql writes in
    FMYSQL_aligned_BINDs: TMYSQL_aligned_BINDDynArray; //offset descriptor structures
    FSmallLobBuffer: array[Byte] of Byte; //for tiny reads of unbound col-buffers
    procedure InitColumnBinds(Bind: PMYSQL_aligned_BIND; MYSQL_FIELD: PMYSQL_FIELD;
      ColumnIndex: Integer; BindOffsets: PMYSQL_BINDOFFSETS);
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    procedure Open; override;
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; const Statement: IZStatement;
      const SQL: string; MySQL: PMySQL; MySQL_Stmt: PPMYSQL;
      out OpenCursorCallback: TOpenCursorCallback);

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
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
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Next: Boolean; override;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; EndJSONObject: Boolean = True;
      With_DATETIME_MAGIC: Boolean = False; SkipNullFields: Boolean = False); override;
    {$ENDIF USE_SYNCOMMONS}
    procedure OpenCursor; virtual;
    procedure ResetCursor; override;
  end;

  TZMySQL_Store_PreparedResultSet = class(TZAbstractMySQLPreparedResultSet)
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
    procedure ResetCursor; override;
    procedure OpenCursor; override;
  end;

  TZMySQL_Use_PreparedResultSet = class(TZAbstractMySQLPreparedResultSet)
  public
    procedure ResetCursor; override;
  end;

  {** Implements a cached resolver with MySQL specific functionality. }
  TZMySQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FMySQL: PMySQL;
    FMYSQL_STMT: PMYSQL_STMT;
    FPlainDriver: TZMySQLPlainDriver;
    FAutoColumnIndex: Integer;
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; MySQL: PMySQL;
      MYSQL_STMT: PMYSQL_STMT; const Statement: IZStatement;
      const Metadata: IZResultSetMetadata);

    function FormWhereClause(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string; override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    // --> ms, 31/10/2005
    function FormCalculateStatement(Columns: TObjectList): string; override;
    // <-- ms
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
  end;

  TZMySQLPreparedClob = Class(TZAbstractClob)
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; Bind: PMYSQL_aligned_BIND;
      StmtHandle: PMySql_Stmt; ColumnIndex: Cardinal; ConSettings: PZConSettings);
  End;

  TZMySQLPreparedBlob = Class(TZAbstractBlob)
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; Bind: PMYSQL_aligned_BIND;
      StmtHandle: PMySql_Stmt; ColumnIndex: Cardinal; ConSettings: PZConSettings);
  End;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF}
  ZFastCode, ZSysUtils, ZMessages, ZEncoding,
  ZDbcMySqlUtils, ZDbcUtils, ZDbcMetadata, ZDbcLogging;

{$IFOPT R+}
  {$DEFINE RangeCheckEnabled}
{$ENDIF}

{ TZMySQLResultSetMetadata }

procedure TZMySQLResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).CatalogName;
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZMySQLResultSetMetadata.GetColumnName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnName;
end;

{**
  Retrieves the designated column's SQL type.
  @param column the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZMySQLResultSetMetadata.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin {EH: does anyone know why the LoadColumns was made? Note the column-types are perfect determinable on MySQL}
  //if not Loaded then
    // LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetSchemaName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).SchemaName;
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).AutoIncrement;
end;

{**
  Initializes columns with additional data.
}
procedure TZMySQLResultSetMetadata.LoadColumns;
{$IFNDEF ZEOS_TEST_ONLY}
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
{$ENDIF}
begin
  {$IFDEF ZEOS_TEST_ONLY}
  inherited LoadColumns;
  {$ELSE}
  if Metadata.GetConnection.GetDriver.GetStatementAnalyser.DefineSelectSchemaFromQuery(Metadata.GetConnection.GetDriver.GetTokenizer, SQL) <> nil then
    for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
      Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
      ClearColumn(Current);
      if Current.TableName = '' then
        continue;
      TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(Metadata.GetIdentifierConvertor.Quote(Current.TableName)),'');
      if TableColumns <> nil then begin
        TableColumns.BeforeFirst;
        while TableColumns.Next do
          if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
            FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
            Break;
          end;
      end;
    end;
  Loaded := True;
  {$ENDIF}
end;

{ TZAbstractMySQLResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractMySQLResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  EndJSONObject: Boolean; With_DATETIME_MAGIC: Boolean; SkipNullFields: Boolean);
var
  C: Cardinal;
  H, I: Integer;
  P: PAnsiChar;
begin
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  {$R-}
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    P := PMYSQL_ROW(FRowHandle)[C];
    if P = nil then
      if JSONWriter.Expand then begin
        if (not SkipNullFields) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case FMySQLTypes[c] of
        FIELD_TYPE_DECIMAL,
        FIELD_TYPE_TINY,
        FIELD_TYPE_SHORT,
        FIELD_TYPE_LONG,
        FIELD_TYPE_FLOAT,
        FIELD_TYPE_DOUBLE,
        FIELD_TYPE_LONGLONG,
        FIELD_TYPE_INT24,
        FIELD_TYPE_YEAR,
        FIELD_TYPE_NEWDECIMAL : JSONWriter.AddNoJSONEscape(P, FLengthArray^[C]);
        FIELD_TYPE_NULL       : JSONWriter.AddShort('null');
        FIELD_TYPE_TIMESTAMP,
        FIELD_TYPE_DATETIME   : begin
                                  JSONWriter.Add('"');
                                  if PWord(P)^ < ValidCenturyMagic then //Year below 1900
                                    inc(P, 11)
                                  else begin
                                    JSONWriter.AddNoJSONEscape(P, 10);
                                    inc(P, 11);
                                  end;
                                  if PInt64(P)^ <> ZeroTimeMagic then begin //not 00:00:00 ?
                                    JSONWriter.Add('T');
                                    JSONWriter.AddNoJSONEscape(P, 8);
                                  end;
                                  JSONWriter.Add('"');
                                end;
        FIELD_TYPE_DATE,
        FIELD_TYPE_NEWDATE    : begin
                                  JSONWriter.Add('"');
                                  if not PWord(P)^ < ValidCenturyMagic then //Year below 1900 then
                                    JSONWriter.AddNoJSONEscape(P, 10);
                                  JSONWriter.Add('"');
                                end;
        FIELD_TYPE_TIME       : begin
                                  JSONWriter.Add('"');
                                  if PInt64(P)^ <> ZeroTimeMagic then begin //not 00:00:00 ?
                                    JSONWriter.Add('T');
                                    JSONWriter.AddNoJSONEscape(P, 8);
                                  end;
                                  JSONWriter.Add('"');
                                end;
        FIELD_TYPE_BIT        : if FLengthArray^[C] = 1 then
                                  JSONWriter.AddShort(JSONBool[PByte(P)^ <> 0]) else
                                  JSONWriter.WrBase64(P, FLengthArray^[C], True);
        FIELD_TYPE_ENUM       : if TZColumnInfo(ColumnsInfo[C]).ColumnType = stBoolean then
                                  JSONWriter.AddShort(JSONBool[UpCase(P^) = 'Y'])
                                else begin
                                  JSONWriter.Add('"');
                                  JSONWriter.AddJSONEscape(P, FLengthArray^[C]);
                                  JSONWriter.Add('"');
                                end;
        FIELD_TYPE_SET        : begin
                                  JSONWriter.Add('"');
                                  JSONWriter.AddJSONEscape(P, FLengthArray^[C]);
                                  JSONWriter.Add('"');
                                end;
        FIELD_TYPE_VARCHAR,
        FIELD_TYPE_TINY_BLOB,
        FIELD_TYPE_MEDIUM_BLOB,
        FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB,
        FIELD_TYPE_VAR_STRING,
        FIELD_TYPE_STRING     : if not fBinaryResults[c] then begin
                                  JSONWriter.Add('"');
                                  JSONWriter.AddJSONEscape(P, FLengthArray^[C]);
                                  JSONWriter.Add('"');
                                end else
                                  JSONWriter.WrBase64(P, FLengthArray^[C], True);
        FIELD_TYPE_GEOMETRY   : JSONWriter.WrBase64(P, FLengthArray^[C], True);
      end;
      JSONWriter.Add(',');
    end;
  end;
  if EndJSONObject then
  begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZAbstractMySQLResultSet.Create(const PlainDriver: TZMySQLPlainDriver;
  const Statement: IZStatement; const SQL: string; Handle: PMySQL;
  AffectedRows: PInteger; out OpenCursorCallback: TOpenCursorCallback);
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
      Statement.GetConnection.GetConSettings);
  fServerCursor := Self is TZMySQL_Use_ResultSet;
  FHandle := Handle;
  FQueryHandle := nil;
  FRowHandle := nil;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  OpenCursorCallback := OpenCursor;
  Open;
  if Assigned(AffectedRows) then
    AffectedRows^ := LastRowNo;
end;

function TZAbstractMySQLResultSet.GetBufferAndLength(ColumnIndex: Integer; var Len: NativeUInt): PAnsiChar;
var
  x: ULong;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  {$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  {$R-}
  x := FLengthArray^[ColumnIndex];
  Result := PMYSQL_ROW(FRowHandle)[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Len := x;
  LastWasNull := Result = nil;
end;

function TZAbstractMySQLResultSet.GetBuffer(ColumnIndex: Integer): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  {$R-}
  Result := PMYSQL_ROW(FRowHandle)[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := Result = nil;
end;

{**
  Opens this recordset.
}
procedure TZAbstractMySQLResultSet.Open;
var
  I: Integer;
  FieldHandle: PMYSQL_FIELD;
begin
  OpenCursor;
  if not Assigned(FQueryHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info. }
  ColumnsInfo.Clear;
  SetLength(FMySQLTypes, FPlainDriver.mysql_num_fields(FQueryHandle));
  {$IFDEF USE_SYNCOMMONS}
  SetLength(fBinaryResults, Length(FMySQLTypes));
  {$ENDIF}
  for I := 0 to High(FMySQLTypes) do begin
    FPlainDriver.mysql_field_seek(FQueryHandle, I);
    FieldHandle := FPlainDriver.mysql_fetch_field(FQueryHandle);
    if FieldHandle = nil then
      Break;
    FMySQLTypes[i] := FieldHandle^._type;
    {$IFDEF USE_SYNCOMMONS}
    fBinaryResults[i] := FieldHandle^.charsetnr = 63;
    {$ENDIF}

    ColumnsInfo.Add(GetMySQLColumnInfoFromFieldHandle(FieldHandle, ConSettings,
      fServerCursor));
  end;

  inherited Open;
end;

procedure TZAbstractMySQLResultSet.ResetCursor;
begin
  if (FQueryHandle <> nil) and (FPlainDriver.mysql_more_results(FHandle) = 1) then begin
    FQueryHandle := nil;
    Close;
  end else
    inherited ResetCursor;
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
procedure TZAbstractMySQLResultSet.Close;
begin
  inherited Close;
  FQueryHandle := nil;
  FRowHandle := nil;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractMySQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := (GetBuffer(ColumnIndex) = nil);
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
function TZAbstractMySQLResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (Closed) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;
  if (FQueryHandle = nil) then begin
    FQueryHandle := FPlainDriver.mysql_store_result(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.mysql_num_rows(FQueryHandle);
  end;
  FRowHandle := FPlainDriver.mysql_fetch_row(FQueryHandle);
  if FRowHandle <> nil then begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end else begin
    if fServerCursor then begin
      LastRowNo := RowNo;
      RowNo := RowNo+1;
    end else
      RowNo := RowNo+1;
    Exit;
  end;
  FLengthArray := FPlainDriver.mysql_fetch_lengths(FQueryHandle)
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
function TZAbstractMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  Result := GetBufferAndLength(ColumnIndex, Len{%H-});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := GetBuffer(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: NativeUInt;
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
function TZAbstractMySQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := False
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^ <> 0;
      2: Result := ReverseWordBytes(Buffer) <> 0;
      3, 4: Result := ReverseLongWordBytes(Buffer, Len) <> 0;
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len) <> 0;
    end
  end else
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
function TZAbstractMySQLResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  Buffer: PAnsiChar;
  Len: Ulong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
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
function TZAbstractMySQLResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
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
function TZAbstractMySQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
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
function TZAbstractMySQLResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
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
function TZAbstractMySQLResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
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
function TZAbstractMySQLResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  Result := BufferToBytes(Buffer, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
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
      Result := Int(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZAbstractMySQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
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
function TZAbstractMySQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
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
        Result := RawSQLDateToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed);
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
function TZAbstractMySQLResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Buffer: PAnsiChar;
  Len: NativeUInt;
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
        Result := TZAbstractBlob.CreateWithData(Buffer, Len)
      else
        Result := TZAbstractClob.CreateWithData(Buffer, Len,
          ConSettings^.ClientCodePage^.CP, ConSettings)
    end;
end;

{ TZMySQL_Store_ResultSet }

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
function TZMySQL_Store_ResultSet.MoveAbsolute(Row: Integer): Boolean;
var OffSet: ULongLong;  //local value required because of the subtraction
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or ((MaxRows > 0) and (Row > MaxRows)) then
    Exit;

  if (FQueryHandle = nil) then begin
    FQueryHandle := FPlainDriver.mysql_store_result(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.mysql_num_rows(FQueryHandle);
  end;

  { Process negative rows. }
  if Row < 0 then begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (Row >= 0) and (Row <= LastRowNo + 1) then begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then begin
      OffSet := RowNo - 1;
      FPlainDriver.mysql_data_seek(FQueryHandle, OffSet);
      FRowHandle := FPlainDriver.mysql_fetch_row(FQueryHandle);
    end else
      FRowHandle := nil;
  end;

  Result := FRowHandle <> nil;

  if Result
  then FLengthArray := FPlainDriver.mysql_fetch_lengths(FQueryHandle)
  else FLengthArray := nil;
end;

procedure TZMySQL_Store_ResultSet.OpenCursor;
begin
  FQueryHandle := FPlainDriver.mysql_store_result(FHandle);
  if Assigned(FQueryHandle) then
    LastRowNo := FPlainDriver.mysql_num_rows(FQueryHandle)
end;

procedure TZMySQL_Store_ResultSet.ResetCursor;
begin
  if FQueryHandle <> nil then begin
    FPlainDriver.mysql_free_result(FQueryHandle);
    FQueryHandle := nil;
  end;
  inherited ResetCursor;
end;

{ TZAbstractMySQLPreparedResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractMySQLPreparedResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  EndJSONObject: Boolean; With_DATETIME_MAGIC: Boolean; SkipNullFields: Boolean);
var
  C: Cardinal;
  H, I: SmallInt;
  Date1, Date2: TDateTime;
  Blob: IZBlob;
  Bind: PMYSQL_aligned_BIND;
  P: Pointer;
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
    Bind := @FMYSQL_aligned_BINDs[C];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    with Bind^ do
    if is_null[0] = 1 then
      if JSONWriter.Expand then begin
        if (not SkipNullFields) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case buffer_type_address^ of
        //FIELD_TYPE_DECIMAL,
        FIELD_TYPE_TINY       : if is_unsigned_address^ = 0 then
                                  JSONWriter.Add(PShortInt(Buffer)^) else
                                  JSONWriter.AddU(PByte(Buffer)^);
        FIELD_TYPE_SHORT      : if is_unsigned_address^ = 0 then
                                  JSONWriter.Add(PSmallInt(Buffer)^) else
                                  JSONWriter.AddU(PWord(Buffer)^);
        FIELD_TYPE_LONG       : if is_unsigned_address^ = 0 then
                                  JSONWriter.Add(PInteger(Buffer)^) else
                                  JSONWriter.AddU(PLongWord(Buffer)^);
        FIELD_TYPE_FLOAT      : JSONWriter.AddSingle(PSingle(Buffer)^);
        FIELD_TYPE_DOUBLE     : JSONWriter.AddDouble(PDouble(Buffer)^);
        FIELD_TYPE_LONGLONG   : if is_unsigned_address^ = 0 then
                                  JSONWriter.Add(PInt64(Buffer)^) else
                                  JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PUInt64(Buffer)^));
        //FIELD_TYPE_INT24,
        FIELD_TYPE_YEAR       : JSONWriter.AddU(PWord(Buffer)^);
        //FIELD_TYPE_NEWDECIMAL,
        FIELD_TYPE_NULL       : JSONWriter.AddShort('null');
        FIELD_TYPE_TIMESTAMP,
        FIELD_TYPE_DATETIME   : begin
                                  if PMYSQL_TIME(Buffer)^.Year >= 1900 then begin
                                    if not sysUtils.TryEncodeDate(
                                        PMYSQL_TIME(Buffer)^.Year,
                                        PMYSQL_TIME(Buffer)^.Month,
                                        PMYSQL_TIME(Buffer)^.Day, Date1) then
                                      Date1 := encodeDate(1900, 1, 1)
                                  end else
                                    Date1 := 0;
                                  if not sysUtils.TryEncodeTime(
                                      PMYSQL_TIME(Buffer)^.Hour,
                                      PMYSQL_TIME(Buffer)^.Minute,
                                      PMYSQL_TIME(Buffer)^.Second,
                                      0{PMYSQL_TIME(Buffer)^.second_part}, Date2) then
                                    Date2 := 0;
                                  Date1 := Date1+Date2;
                                  JSONWriter.AddDateTime(@Date1, 'T', '"');
                                end;
        FIELD_TYPE_DATE,
        FIELD_TYPE_NEWDATE    : begin
                                  if not sysUtils.TryEncodeDate(
                                      PMYSQL_TIME(Buffer)^.Year,
                                      PMYSQL_TIME(Buffer)^.Month,
                                      PMYSQL_TIME(Buffer)^.Day, Date1) then
                                    Date1 := encodeDate(1900, 1, 1);
                                  JSONWriter.AddDateTime(@Date1, 'T', '"');
                                end;
        FIELD_TYPE_TIME       : begin
                                  if not sysUtils.TryEncodeTime(
                                      PMYSQL_TIME(Buffer)^.Hour,
                                      PMYSQL_TIME(Buffer)^.Minute,
                                      PMYSQL_TIME(Buffer)^.Second,
                                      0{PMYSQL_TIME(Buffer)^.second_part}, Date1) then
                                    Date1 := 0;
                                  JSONWriter.AddDateTime(@Date1, 'T', '"');
                                end;
        FIELD_TYPE_BIT        : if Length[0] = 1
                                then JSONWriter.AddShort(JSONBool[PByte(Buffer)^ <> 0])
                                else JSONWriter.WrBase64(Pointer(Buffer), Length[0], True);
        FIELD_TYPE_ENUM,
        FIELD_TYPE_SET,
        FIELD_TYPE_VARCHAR,
        FIELD_TYPE_VAR_STRING,
        FIELD_TYPE_STRING,
        FIELD_TYPE_TINY_BLOB,
        FIELD_TYPE_MEDIUM_BLOB,
        FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB,
        FIELD_TYPE_GEOMETRY   : begin
                      if (Buffer <> nil) then
                        if Binary then
                          JSONWriter.WrBase64(Pointer(Buffer), Length[0], True)
                        else begin
                          JSONWriter.Add('"');
                          JSONWriter.AddJSONEscape(Pointer(Buffer), Length[0]);
                          JSONWriter.Add('"');
                        end
                      else if Length[0] < SizeOf(FSmallLobBuffer) then begin
                        buffer_address^ := @FSmallLobBuffer[0];
                        buffer_Length_address^ := SizeOf(FSmallLobBuffer);
                        FPlainDriver.mysql_stmt_fetch_column(FPrepStmt, mysql_bind, C, 0);
                        buffer_address^ := nil;
                        if binary then
                          JSONWriter.WrBase64(@FSmallLobBuffer[0], Length[0], True)
                        else begin
                          JSONWriter.Add('"');
                          JSONWriter.AddJSONEscape(@FSmallLobBuffer[0], Length[0]);
                          JSONWriter.Add('"');
                        end;
                      end else if binary then begin
                        Blob := TZMySQLPreparedBlob.Create(FplainDriver,
                          Bind, FPrepStmt, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, ConSettings);
                        JSONWriter.WrBase64(Blob.GetBuffer, Blob.Length, True)
                      end else begin
                        JSONWriter.Add('"');
                        Blob := TZMySQLPreparedClob.Create(FplainDriver,
                          Bind, FPrepStmt, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, ConSettings);
                        P := Blob.GetPAnsiChar(zCP_UTF8);
                        JSONWriter.AddJSONEscape(P, Blob.Length);
                        JSONWriter.Add('"');
                      end;
                    end;
      end;
      JSONWriter.Add(',');
    end;
  end;
  if EndJSONObject then
  begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZAbstractMySQLPreparedResultSet.Create(
  const PlainDriver: TZMySQLPlainDriver; const Statement: IZStatement;
  const SQL: string; MySQL: PMySQL; MySQL_Stmt: PPMYSQL;
  out OpenCursorCallback: TOpenCursorCallback);
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  fServerCursor := Self is TZMySQL_Use_PreparedResultSet;
  FMysQL := MySQL;
  FPPMYSQL := MySQL_Stmt;
  FPrepStmt := MySQL_Stmt^;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  OpenCursorCallback := OpenCursor;
  Open;
end;

{**
  Opens this recordset.
}
procedure TZAbstractMySQLPreparedResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldHandle: PMYSQL_FIELD;
  FieldCount: Integer;
  FResultMetaData : PZMySQLResult;
  BindOffsets: PMYSQL_BINDOFFSETS;
begin
  FieldCount := FPlainDriver.mysql_stmt_field_count(FPrepStmt);
  if FieldCount = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  FResultMetaData := FPlainDriver.mysql_stmt_result_metadata(FPrepStmt);

  if not Assigned(FResultMetaData) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  BindOffsets := GetBindOffsets(FPlainDriver.IsMariaDBDriver, FPlainDriver.mysql_get_client_version);

  if BindOffsets.buffer_type=0 then
    raise EZSQLException.Create('Unknown dll version : '+ZFastCode.IntToStr(FPlainDriver.mysql_get_client_version));


  { We use the refetch logic of
  https://bugs.mysql.com/file.php?id=12361&bug_id=33086 }

  { Fills the column info. }
  try
    ReAllocMySQLBindBuffer(FColBuffer, FMYSQL_aligned_BINDs,
      FPlainDriver.mysql_num_fields(FResultMetaData), 1, BindOffsets);
    for I := 0 to FPlainDriver.mysql_num_fields(FResultMetaData) - 1 do begin
      FPlainDriver.mysql_field_seek(FResultMetaData, I);
      FieldHandle := FPlainDriver.mysql_fetch_field(FResultMetaData);
      if FieldHandle = nil then
        Break;

      ColumnInfo := GetMySQLColumnInfoFromFieldHandle(FieldHandle,
        ConSettings, fServerCursor);

      ColumnsInfo.Add(ColumnInfo);
      {$R-}
      InitColumnBinds(@FMYSQL_aligned_BINDs[I], FieldHandle, i, BindOffSets);
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end;
  finally
    FPlainDriver.mysql_free_result(FResultMetaData);
  end;

  OpenCursor;
  inherited Open;
end;

procedure TZAbstractMySQLPreparedResultSet.OpenCursor;
begin
  if FPPMYSQL <> nil then
    FPrepStmt := FPPMYSQL^;
  if (FPlainDriver.mysql_stmt_bind_result(FPrepStmt,FColBuffer)<>0) then
    raise EZSQLException.Create(SFailedToBindResults);
end;

procedure TZAbstractMySQLPreparedResultSet.ResetCursor;
begin
  if (FPrepStmt <> nil) then begin
    FPrepStmt := nil; //else infinate loop
    //test if more results are pendding
    if not FPlainDriver.IsMariaDBDriver and (Assigned(FPlainDriver.mysql_stmt_more_results) and (FPlainDriver.mysql_stmt_more_results(FPPMYSQL^) = 1))
    then Close
    else inherited ResetCursor;
  end else
    inherited ResetCursor;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractMySQLPreparedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  {$R-}
  Result := FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null[0] = 1;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the String in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsichar;
var
  TmpDateTime, TmpDateTime2: TDateTime;
  ColBind: PMYSQL_aligned_BIND;
begin
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else begin
    case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0
        then FRawTemp := IntToRaw(PShortInt(ColBind^.buffer)^)
        else FRawTemp := IntToRaw(PByte(ColBind^.buffer)^);
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0
        then FRawTemp := IntToRaw(PSmallInt(ColBind^.buffer)^)
        else FRawTemp := IntToRaw(PWord(ColBind^.buffer)^);
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0
        then FRawTemp := IntToRaw(PLongInt(ColBind^.buffer)^)
        else FRawTemp := IntToRaw(PLongWord(ColBind^.buffer)^);
      FIELD_TYPE_FLOAT:
        FRawTemp := FloatToSQLRaw(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:
        FRawTemp := FloatToSQLRaw(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:
        FRawTemp := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          FRawTemp := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0
        then FRawTemp := IntToRaw(PInt64(ColBind^.buffer)^)
        else FRawTemp := IntToRaw(PUInt64(ColBind^.buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          FRawTemp := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME: begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          FRawTemp := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        FRawTemp := IntToRaw(PWord(ColBind^.buffer)^);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_STRING:
        begin
          Result := Pointer(ColBind^.buffer);
          Len := ColBind^.length[0];
          Exit;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          FTempBlob := GetBlob(ColumnIndex);
          Len := FTempBlob.Length;
          Result := FTempBlob.GetBuffer;
          Exit;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end;
    Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
    Result := Pointer(FRawTemp);
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
function TZAbstractMySQLPreparedResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
end;

procedure TZAbstractMySQLPreparedResultSet.InitColumnBinds(Bind: PMYSQL_aligned_BIND;
  MYSQL_FIELD: PMYSQL_FIELD; ColumnIndex: Integer; BindOffsets: PMYSQL_BINDOFFSETS);
begin
  Bind^.is_unsigned_address^ := Ord(MYSQL_FIELD.flags and UNSIGNED_FLAG <> 0);
  bind^.buffer_type_address^ := MYSQL_FIELD^._type; //safe initialtype
  bind^.binary := (MYSQL_FIELD^.charsetnr = 63);

  case MYSQL_FIELD^._type of
    FIELD_TYPE_BIT: case MYSQL_FIELD^.length of
                      0..8  : Bind^.Length[0] := SizeOf(Byte);
                      9..16 : Bind^.Length[0] := SizeOf(Word);
                      17..32: Bind^.Length[0] := SizeOf(LongWord);
                      else    Bind^.Length[0] := SizeOf(UInt64);
                    end;
    FIELD_TYPE_DATE,
    FIELD_TYPE_TIME,
    FIELD_TYPE_DATETIME,
    FIELD_TYPE_TIMESTAMP:   Bind^.Length[0] := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TINY:        Bind^.Length[0] := 1;
    FIELD_TYPE_SHORT:       Bind^.Length[0] := 2;
    FIELD_TYPE_LONG:        Bind^.Length[0] := 4;
    FIELD_TYPE_LONGLONG:    Bind^.Length[0] := 8;
    FIELD_TYPE_INT24: begin//we've no 3Byte integers... so let's convert them
        Bind^.Length[0] := 4;
        bind^.buffer_type_address^ := FIELD_TYPE_LONG;
      end;
    FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE: begin
        if MYSQL_FIELD^.length = 12 then begin
          Bind^.Length[0] := SizeOf(Single);
          bind^.buffer_type_address^ := FIELD_TYPE_FLOAT
        end else begin
          Bind^.Length[0] := SizeOf(Double);
          bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        end;
        bind^.decimals := MYSQL_FIELD^.decimals;
      end;
    FIELD_TYPE_BLOB,
    FIELD_TYPE_TINY_BLOB,
    FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB,
    FIELD_TYPE_GEOMETRY:    Bind^.Length[0] := 0;//http://bugs.mysql.com/file.php?id=12361&bug_id=33086
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_VAR_STRING,
    FIELD_TYPE_STRING,
    FIELD_TYPE_ENUM, FIELD_TYPE_SET: begin
        bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        Bind^.Length[0] := MYSQL_FIELD^.length;//+Byte(Ord(not bind^.binary));
      end;
    FIELD_TYPE_NEWDECIMAL,
    FIELD_TYPE_DECIMAL:
      if MYSQL_FIELD^.decimals = 0 then begin
        if MYSQL_FIELD^.length <= Byte(2+(Ord(MYSQL_FIELD^.flags and UNSIGNED_FLAG <> 0))) then begin
          bind^.buffer_type_address^ := FIELD_TYPE_TINY;
          Bind^.Length[0] := 1;
        end else if MYSQL_FIELD^.length <= Byte(4+(Ord(MYSQL_FIELD^.flags and UNSIGNED_FLAG <> 0))) then begin
          Bind^.Length[0] := 2;
          bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
        end else if MYSQL_FIELD^.length <= Byte(9+(Ord(MYSQL_FIELD^.flags and UNSIGNED_FLAG <> 0))) then begin
          bind^.buffer_type_address^ := FIELD_TYPE_LONG;
          Bind^.Length[0] := 4;
        end else begin
          bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
          Bind^.Length[0] := 8;
        end
      end else begin //force binary conversion to double values!
        bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        Bind^.Length[0] := 8;
        bind^.decimals := MYSQL_FIELD^.decimals;
      end;
    FIELD_TYPE_NULL: Bind^.Length[0] := 8;
    else
      Bind^.Length[0] := (((MYSQL_FIELD^.length) shr 3)+1) shl 3; //8Byte Aligned
    //Length := MYSQL_FIELD^.length;
  end;
  if bind^.Length[0] = 0
  then Bind^.Buffer := nil
  else SetLength(Bind^.Buffer, bind^.Length[0]+Byte(Ord(bind^.buffer_type_address^ = FIELD_TYPE_STRING)));
  Bind^.buffer_address^ := Pointer(Bind^.buffer);
  Bind^.buffer_length_address^ := bind^.Length[0];
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  TmpDateTime, TmpDateTime2: TDateTime;
  ColBind: PMYSQL_aligned_BIND;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := ''
  else
  begin
    case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := IntToRaw(PShortInt(ColBind^.buffer)^)
        else
          Result := IntToRaw(PByte(ColBind^.buffer)^);
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := IntToRaw(PSmallInt(ColBind^.buffer)^)
        else
          Result := IntToRaw(PWord(ColBind^.buffer)^);
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := IntToRaw(PLongInt(ColBind^.buffer)^)
        else
          Result := IntToRaw(PLongWord(ColBind^.buffer)^);
      FIELD_TYPE_FLOAT:
        Result := FloatToSQLRaw(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:
        Result := FloatToSQLRaw(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:
        Result := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          Result := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := IntToRaw(PInt64(ColBind^.buffer)^)
        else
          Result := IntToRaw(PUInt64(ColBind^.buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          Result := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME:
        begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          Result := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        Result := IntToRaw(PWord(ColBind^.buffer)^);
      FIELD_TYPE_BIT:
        Result := IntToRaw(PByte(ColBind^.buffer)^);
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB,
      FIELD_TYPE_STRING, FIELD_TYPE_GEOMETRY:
        if (Pointer(ColBind^.buffer) = nil) then
          if ColBind^.Length[0]  < SizeOf(FSmallLobBuffer) then begin
            Colbind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer);
            FPlainDriver.mysql_stmt_fetch_column(FPrepStmt, Colbind^.mysql_bind, ColumnIndex, 0);
            Colbind^.buffer_address^ := nil;
            ZSetString(PAnsiChar(@FSmallLobBuffer[0]), ColBind^.Length[0] , Result);
          end else
            Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString
        else ZSetString(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , Result);
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
function TZAbstractMySQLPreparedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := False
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^ <> 0
        else
          Result := PByte(ColBind^.buffer)^ <> 0;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^ <> 0
        else
          Result := PWord(ColBind^.buffer)^ <> 0;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^ <> 0
        else
          Result := PLongWord(ColBind^.buffer)^ <> 0;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^) <> 0;
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^) <> 0;
      FIELD_TYPE_NULL:      Result := False;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := False;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^ <> 0
        else
          Result := PUInt64(ColBind^.buffer)^ <> 0;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^ <> 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := StrToBoolEx(PAnsiChar(ColBind^.buffer), True, False);
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^ <> 0;
          2: Result := ReverseWordBytes(ColBind^.buffer)  <> 0;
          3, 4: Result := ReverseLongWordBytes(ColBind^.buffer, ColBind^.Length[0] ) <> 0;
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(ColBind^.buffer, ColBind^.Length[0] ) <> 0;
          end;
          //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 12{Max Int32 Length = 11} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          Result := StrToBoolEx(PAnsiChar(FTempBlob.GetBuffer));
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetByte(ColumnIndex: Integer): Byte;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT:
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
          //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 4{max Length = 3} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetShort(ColumnIndex: Integer): ShortInt;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 5{Max ShortInt Length = 3+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetWord(ColumnIndex: Integer): Word;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 7{Max Word Length = 5+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 8{Max SmallInt Length = 6+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetUInt(ColumnIndex: Integer): LongWord;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 12{Max LongWord Length = 10+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToUInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToUInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetInt(ColumnIndex: Integer): Integer;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 13{Max Int32 Length = 11+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetULong(ColumnIndex: Integer): UInt64;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 22{Max UInt64 Length = 20+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToUInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToUInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetLong(ColumnIndex: Integer): Int64;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToInt64Def(PAnsiChar(ColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case ColBind^.Length[0]  of
          1: Result := PByte(ColBind^.buffer)^;
          2: Result := ReverseWordBytes(Pointer(ColBind^.buffer));
          3, 4: Result := ReverseLongWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 22{Max Int64 Length = 20+#0}) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if ColBind^.binary then
            Result := RawToInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetFloat(ColumnIndex: Integer): Single;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     if ColBind^.decimals < 20
                            then Result := RoundTo(PSingle(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    if ColBind^.decimals < 20
                            then Result := RoundTo(PDouble(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(ColBind^.buffer), 0, Result, ColBind^.Length[0] );
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 30{Max Extended Length = 28 ??} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, '.', 0, Result)
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
function TZAbstractMySQLPreparedResultSet.GetDouble(ColumnIndex: Integer): Double;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     if ColBind^.decimals < 20
                            then Result := RoundTo(PSingle(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    if ColBind^.decimals < 20
                            then Result := RoundTo(PDouble(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(ColBind^.buffer), 0, Result, ColBind^.Length[0] );
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 30{Max Extended Length = 28 ??+#0} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, '.', 0, Result)
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
function TZAbstractMySQLPreparedResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     if ColBind^.decimals < 20
                            then Result := RoundTo(PSingle(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    if ColBind^.decimals < 20
                            then Result := RoundTo(PDouble(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(ColBind^.buffer), 0, Result, ColBind^.Length[0] );
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( ColBind^.Length[0]  > 0 ) and
           (ColBind^.Length[0]  < 29{Max Extended Length = 28 ??+#0} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, '.', 0, Result)
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
function TZAbstractMySQLPreparedResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := nil
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_TINY,
      FIELD_TYPE_SHORT,
      FIELD_TYPE_LONG,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE,
      FIELD_TYPE_NULL,
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE,
      FIELD_TYPE_LONGLONG,
      FIELD_TYPE_YEAR: Result := nil;
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := BufferToBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          FTempBlob := GetBlob(ColumnIndex);
          result := FTempBlob.GetBytes;
          FTempBlob := nil;
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
function TZAbstractMySQLPreparedResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME: Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if ColBind^.Length[0]  = ConSettings^.ReadFormatSettings.DateFormatLen then
            Result := RawSQLDateToDateTime(PAnsiChar(ColBind^.buffer),
              ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
              RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer),
                ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
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
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME, FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part div 1000}, Result) then
          Result := 0;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if (PAnsiChar(ColBind^.buffer)+2)^ = ':' then //possible date if Len = 10 then
            Result := RawSQLTimeToDateTime(PAnsiChar(ColBind^.buffer),
              ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := Frac(RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer),
              ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
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
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZAbstractMySQLPreparedResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  tmp: TDateTime;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := ColBind^.is_null[0] =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case ColBind^.buffer_type_address^ of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PShortInt(ColBind^.buffer)^
        else
          Result := PByte(ColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PSmallInt(ColBind^.buffer)^
        else
          Result := PWord(ColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PLongInt(ColBind^.buffer)^
        else
          Result := PLongWord(ColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(ColBind^.buffer)^.Year,
            PMYSQL_TIME(ColBind^.buffer)^.Month,
            PMYSQL_TIME(ColBind^.buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(ColBind^.buffer)^.Hour,
            PMYSQL_TIME(ColBind^.buffer)^.Minute,
            PMYSQL_TIME(ColBind^.buffer)^.Second,
            0{PMYSQL_TIME(ColBind^.buffer)^.second_part div 1000}, Result) then
          Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
              PMYSQL_TIME(ColBind^.buffer)^.Year,
              PMYSQL_TIME(ColBind^.buffer)^.Month,
              PMYSQL_TIME(ColBind^.buffer)^.Day, tmp) then
            tmp := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
              PMYSQL_TIME(ColBind^.buffer)^.Hour,
              PMYSQL_TIME(ColBind^.buffer)^.Minute,
              PMYSQL_TIME(ColBind^.buffer)^.Second,
              0{PMYSQL_TIME(ColBind^.buffer)^.second_part div 1000}, Result) then
            Result := 0;
          Result := Result + tmp;
        end;
      FIELD_TYPE_LONGLONG:
        if ColBind^.is_unsigned_address^ = 0 then
          Result := PInt64(ColBind^.buffer)^
        else
          Result := PUInt64(ColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if (PAnsiChar(ColBind^.buffer)+2)^ = ':' then
            Result := RawSQLTimeToDateTime(PAnsiChar(ColBind^.buffer),
              ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - ColBind^.Length[0] ) <= 4 then
              Result := RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed)
            else
              Result := RawSQLDateToDateTime(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed);
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractMySQLPreparedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  ColBind: PMYSQL_aligned_BIND;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if not LastWasNull then
    case ColBind^.buffer_type_address^ of
      FIELD_TYPE_NULL: Result := nil;
      FIELD_TYPE_BLOB,
      FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB,
      FIELD_TYPE_LONG_BLOB:
        if ColBind^.binary then
          Result := TZMySQLPreparedBlob.Create(FplainDriver,
            ColBind, FPrepStmt, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, ConSettings)
        else
          Result := TZMySQLPreparedClob.Create(FplainDriver,
            ColBind, FPrepStmt, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, ConSettings);
      else begin
          FRawTemp := InternalGetString(ColumnIndex);
          Result := TZAbstractClob.CreateWithData(PAnsiChar(FRawTemp), Length(FRawTemp),
            ConSettings^.ClientCodePage^.CP, ConSettings);
        end;
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
function TZAbstractMySQLPreparedResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or not Assigned(FPrepStmt) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;

  FFetchStatus := FPlainDriver.mysql_stmt_fetch(FPrepStmt);
  if FFetchStatus in [STMT_FETCH_OK, MYSQL_DATA_TRUNCATED] then begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end else if FFetchStatus = STMT_FETCH_ERROR then
    checkMySQLPrepStmtError(FPlainDriver,FPrepStmt, lcOther, '', ConSettings)
  else if FFetchStatus = MYSQL_NO_DATA then begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;
end;

{ TZMySQL_Store_PreparedResultSet }

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
function TZMySQL_Store_PreparedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  CheckClosed;

  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  { Process negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (Row >= 0) and (Row <= LastRowNo + 1) then begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then begin
      FPlainDriver.mysql_stmt_data_seek(FPrepStmt, RowNo - 1);
      Result := FPlainDriver.mysql_stmt_fetch(FPrepStmt) = 0;
    end;
  end;
end;

procedure TZMySQL_Store_PreparedResultSet.OpenCursor;
begin
  inherited OpenCursor;
  if FPlainDriver.mysql_stmt_store_result(FPrepStmt)=0 then
    LastRowNo := FPlainDriver.mysql_stmt_num_rows(FPrepStmt);
end;

procedure TZMySQL_Store_PreparedResultSet.ResetCursor;
begin
  if Assigned(FPrepStmt) then
    if FPlainDriver.mysql_stmt_free_result(FPrepStmt) <> 0 then
      checkMySQLPrepStmtError(FPlainDriver,FPrepStmt, lcOther, '', ConSettings);
  inherited ResetCursor;
end;

{ TZMySQL_Use_PreparedResultSet }

procedure TZMySQL_Use_PreparedResultSet.ResetCursor;
begin
  if FPrepStmt <> nil then
    {need to fetch all temporary until handle = nil else all other queries are out of sync
     see: http://dev.mysql.com/doc/refman/5.0/en/mysql-use-result.html}
    while FPlainDriver.mysql_stmt_fetch(FPrepStmt) in [0, MYSQL_DATA_TRUNCATED] do;
  inherited ResetCursor;
end;

{ TZMySQLCachedResolver }

{**
  Creates a MySQL specific cached resolver object.
  @param PlainDriver a native MySQL plain driver.
  @param Handle a MySQL specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZMySQLCachedResolver.Create(const PlainDriver: TZMySQLPlainDriver;
  MySQL: PMySQL; MYSQL_STMT: PMYSQL_STMT; const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := PlainDriver;
  FMySQL := MySQL;
  FMYSQL_STMT := MYSQL_STMT;
  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong]) then begin
      FAutoColumnIndex := I;
      Break;
    end;
end;

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
procedure TZMySQLCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
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
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
var LastWasNull: Boolean;
begin
  if ((FAutoColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF} 0) and
          (OldRowAccessor.IsNull(FAutoColumnIndex) or
          (OldRowAccessor.GetLong(FAutoColumnIndex, LastWasNull)=0)))
  then {if FMYSQL_STMT <> nil
    then NewRowAccessor.SetLong(FAutoColumnIndex, FPlainDriver.mysql_stmt_insert_id(FMYSQL_STMT))  //EH: why does it not work!?
    else }NewRowAccessor.SetLong(FAutoColumnIndex, FPlainDriver.mysql_insert_id(FMySQL)); //and this also works with the prepareds??!
end;

{ TZMySQLPreparedClob }
constructor TZMySQLPreparedClob.Create(const PlainDriver: TZMySQLPlainDriver;
  Bind: PMYSQL_aligned_BIND; StmtHandle: PMySql_Stmt;
  ColumnIndex: Cardinal; ConSettings: PZConSettings);
var
  offset: ULong;
  Status: Integer;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := ConSettings^.ClientCodePage^.CP;
  FBlobSize := Bind^.Length[0]+1; //MySQL sets a trailing #0 on top of data
  GetMem(FBlobData, FBlobSize);
  offset := 0;
  Bind^.buffer_Length_address^ := Bind^.Length[0]; //indicate size of Buffer
  Bind^.buffer_address^ := FBlobData;
  Status := PlainDriver.mysql_stmt_fetch_column(StmtHandle, bind^.mysql_bind, ColumnIndex, offset); //move data to buffer
  Bind^.buffer_address^ := nil; //set nil again
  Bind^.buffer_Length_address^ := 0;
  if Status = 1 then
    checkMySQLPrepStmtError(PlainDriver,StmtHandle, lcOther, '', ConSettings);
  (PAnsiChar(FBlobData)+FBlobSize-1)^ := #0;
End;

{ TZMySQLPreparedBlob }
constructor TZMySQLPreparedBlob.Create(const PlainDriver: TZMySQLPlainDriver;
  Bind: PMYSQL_aligned_BIND; StmtHandle: PMySql_Stmt;
  ColumnIndex: Cardinal; ConSettings: PZConSettings);
var
  offset: ULong;
  Status: Integer;
begin
  inherited Create;
  FBlobSize := Bind^.Length[0];
  GetMem(FBlobData, FBlobSize);
  offset := 0;
  Bind^.buffer_Length_address^ := Bind^.Length[0]; //indicate size of Buffer
  Bind^.buffer_address^ := FBlobData;
  Status := PlainDriver.mysql_stmt_fetch_column(StmtHandle, bind^.mysql_bind, ColumnIndex, offset); //move data to buffer
  Bind^.buffer_address^ := nil; //set nil again
  Bind^.buffer_Length_address^ := 0;
  if Status = 1 then
    checkMySQLPrepStmtError(PlainDriver,StmtHandle, lcOther, '', ConSettings);
End;

{ TZMySQL_Use_ResultSet }

procedure TZMySQL_Use_ResultSet.OpenCursor;
begin
  FQueryHandle := FPlainDriver.mysql_use_result(FHandle)
end;

procedure TZMySQL_Use_ResultSet.ResetCursor;
begin
  if FQueryHandle <> nil then begin
    {need to fetch all temporary until handle = nil else all other queries are out of sync
     see: http://dev.mysql.com/doc/refman/5.0/en/mysql-use-result.html}
    while FPlainDriver.mysql_fetch_row(FQueryHandle) <> nil do;
    FQueryHandle := nil;
  end;
  inherited ResetCursor;
  if FPlainDriver.mysql_more_results(FHandle) = 1 then
    Close;

end;

end.
