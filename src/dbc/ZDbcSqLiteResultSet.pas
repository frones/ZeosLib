{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
    System.Types{$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF},
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS} Contnrs,{$ENDIF}
  {$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcCache, ZDbcCachedResultSet, ZDbcGenericResolver,
  ZDbcSQLite, ZSelectSchema;

type
  {** Implements SQLite ResultSet Metadata. }

  { TZSQLiteResultSetMetadata }

  TZSQLiteResultSetMetadata = class(TZAbstractResultSetMetadata)
  private
    FHas_ExtendedColumnInfos: Boolean;
  protected
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
  public
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsNullable(Column: Integer): TZColumnNullableType; override;
  end;

  TResetCallBack = procedure of Object;
  {** Implements SQLite ResultSet. }
  TZSQLiteResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    FStmtErrorCode: PInteger;
    FPsqlite3_stmt: PPsqlite3_stmt;
    Fsqlite3_stmt: Psqlite3_stmt;
    FColumnCount: Integer;
    FPlainDriver: TZSQLitePlainDriver;
    FFirstRow, FSQLiteIntAffinity: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FResetCallBack: TResetCallBack;
    FCurrDecimalSep: Char;
    FByteBuffer: PByteBuffer;
    FSQLiteConnection: IZSQLiteConnection;
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement;
      const SQL: string; Psqlite3_stmt: PPsqlite3_stmt;
      PErrorCode: PInteger; UndefinedVarcharAsStringLength: Integer;
      ResetCallBack: TResetCallback; SQLiteIntAffinity: Boolean);

    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; Var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); reintroduce; overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    function Next: Boolean; reintroduce;
    {$IFDEF WITH_COLUMNS_TO_JSON}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF WITH_COLUMNS_TO_JSON}
  end;

  {** Implements a cached resolver with SQLite specific functionality. }
  TZSQLiteCachedResolver = class (TZGenerateSQLCachedResolver, IZCachedResolver)
  private
    FHandle: Psqlite;
    FPlainDriver: TZSQLitePlainDriver;
    FAutoColumnIndex: Integer;
  public
    constructor Create(Handle: Psqlite;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;

    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor;
      const Resolver: IZCachedResolver); override;
  end;

  { TZSQLiteCachedResultSet }

  TZSQLiteCachedResultSet = Class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZSQLiteRowAccessor }

  TZSQLiteRowAccessor = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses {$IFDEF WITH_COLUMNS_TO_JSON}Math, {$ENDIF}
  ZMessages, ZTokenizer, ZVariant, ZEncoding, ZFastCode,
  ZGenericSqlAnalyser,
  ZDbcSQLiteUtils, ZDbcLogging, ZDbcUtils, ZDbcMetadata
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLiteCachedResultSet }

class function TZSQLiteCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZSQLiteRowAccessor;
end;

{ TZSQLiteRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ConSettings" not used} {$ENDIF}
class function TZSQLiteRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
    Result := TZSQLType(Byte(Result)-3); // no streams 4 sqlite
    ColumnInfo.Precision := 0;
  end;
  if Result = stUnicodeString then
    Result := stString; // no national chars in sqlite
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZSQLiteResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZSQLiteResultSetMetadata.Create(const Metadata: IZDatabaseMetadata;
  const SQL: string; ParentResultSet: TZAbstractResultSet);
var PD: TZSQLitePlainDriver;
begin
  inherited Create(Metadata, SQL, ParentResultSet);
  PD := TZSQLitePlainDriver(MetaData.GetConnection.GetIZPlainDriver.GetInstance);
  FHas_ExtendedColumnInfos := Assigned(PD.sqlite3_column_table_name);
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZSQLiteResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CatalogName;
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZSQLiteResultSetMetadata.GetColumnName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZSQLiteResultSetMetadata.GetSchemaName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).SchemaName;
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZSQLiteResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

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

{**
  Initializes columns with additional data.
}
procedure TZSQLiteResultSetMetadata.LoadColumns;
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  Driver: IZDriver;
  IdentifierConverter: IZIdentifierConverter;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
begin
  if not FHas_ExtendedColumnInfos
  then inherited LoadColumns
  else begin
    Connection := Metadata.GetConnection;
    Analyser := Connection.GetStatementAnalyser;
    Tokenizer := Connection.GetTokenizer;
    IdentifierConverter := Metadata.GetIdentifierConverter;
    try
      if Analyser.DefineSelectSchemaFromQuery(Tokenizer, SQL) <> nil then
        for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
          Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
          ClearColumn(Current);
          if Current.TableName = '' then
            continue;
          TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(IdentifierConverter.Quote(Current.TableName, iqTable)),'');
          if TableColumns <> nil then begin
            TableColumns.BeforeFirst;
            while TableColumns.Next do
              if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
                FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
                Break;
              end;
          end;
        end;
    finally
      Driver := nil;
      Connection := nil;
      Analyser := nil;
      Tokenizer := nil;
      IdentifierConverter := nil;
    end;
  end;
  Loaded := True;
end;

{ TZSQLiteResultSet }

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TZSQLiteResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var
  C, H, I, ColType: Integer;
  P: PAnsiChar;
  i64: Int64;
  D: Double absolute i64;
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
    ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, C);
    if ColType = SQLITE_NULL then
      if JSONWriter.Expand then begin
        if not (jcsSkipNulls in JSONComposeOptions) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else with TZColumnInfo(ColumnsInfo[c]) do begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[i]);
      case ColType of
        SQLITE_BLOB: JSONWriter.WrBase64(FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt,C),
                        FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, C), True);
        SQLITE_INTEGER: begin
                        I64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, C);
                        case ColumnType of
                          stBoolean: JSONWriter.AddShort(JSONBool[I64 <> 0]);
                          stCurrency: JSONWriter.AddCurr64({$IFDEF MORMOT2}@{$ENDIF}i64);
                          {stTime, stDate, stTimeStamp:
                            todo: add implementation for unix timestamp
                            JSONWriter.Add(FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, C));}
                          else JSONWriter.Add(i64);
                        end;
                      end;
        SQLITE_FLOAT:   begin
                          D := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C);
                          case ColumnType of
                            stBoolean: JSONWriter.AddShort(JSONBool[D <> 0]);
                            stTime: begin
                                if jcoMongoISODate in JSONComposeOptions then
                                  JSONWriter.AddShort('ISODate("0000-00-00')
                                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                  {$IFDEF MORMOT2}
                                  JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                                  {$ELSE}
                                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                  {$ENDIF}
                                else
                                  JSONWriter.Add('"');
                                d := Frac(D+JulianEpoch);
                                JSONWriter.AddDateTime(D, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.Add('"');
                              end;
                            stDate: begin
                                if jcoMongoISODate in JSONComposeOptions then
                                  JSONWriter.AddShort('ISODate("')
                                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                  {$IFDEF MORMOT2}
                                  JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                                  {$ELSE}
                                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                  {$ENDIF}
                                else
                                  JSONWriter.Add('"');
                                D := Int(D+JulianEpoch);
                                JSONWriter.AddDateTime(D);
                                JSONWriter.Add('"');
                              end;
                            stTimeStamp: begin
                                if jcoMongoISODate in JSONComposeOptions then
                                  JSONWriter.AddShort('ISODate("')
                                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                  {$IFDEF MORMOT2}
                                  JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                                  {$ELSE}
                                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                  {$ENDIF}
                                else
                                  JSONWriter.Add('"');
                                D := D+JulianEpoch;
                                JSONWriter.AddDateTime(D, jcoMilliseconds in JSONComposeOptions);
                                JSONWriter.Add('"');
                              end;
                            else
                              JSONWriter.AddDouble(D);
                          end;
                        end;
        SQLITE3_TEXT: begin
            P := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, C);
            case ColumnType of
              stBoolean: JSONWriter.AddShort(JSONBool[StrToBoolEx(P)]);
              stTime: begin
                  if jcoMongoISODate in JSONComposeOptions then
                    JSONWriter.AddShort('ISODate("0000-00-00')
                  else if jcoDATETIME_MAGIC in JSONComposeOptions then
                    {$IFDEF MORMOT2}
                    JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                    {$ELSE}
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                    {$ENDIF}
                  else
                    JSONWriter.Add('"');
                  JSONWriter.AddNoJSONEscape(P, Min(StrLen(P), 8+(4*Ord(jcoMilliseconds in JSONComposeOptions))));
                  if jcoMongoISODate in JSONComposeOptions
                  then JSONWriter.AddShort('Z)"')
                  else JSONWriter.Add('"');
                end;
              stDate: begin
                  if jcoMongoISODate in JSONComposeOptions then
                    JSONWriter.AddShort('ISODate("')
                  else if jcoDATETIME_MAGIC in JSONComposeOptions then
                    {$IFDEF MORMOT2}
                    JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                    {$ELSE}
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                    {$ENDIF}
                  else
                    JSONWriter.Add('"');
                  JSONWriter.AddNoJSONEscape(P, Min(StrLen(P), 10));
                  if jcoMongoISODate in JSONComposeOptions
                  then JSONWriter.AddShort('Z)"')
                  else JSONWriter.Add('"');
                end;
              stTimeStamp: begin
                  if jcoMongoISODate in JSONComposeOptions then
                    JSONWriter.AddShort('ISODate("')
                  else if jcoDATETIME_MAGIC in JSONComposeOptions then
                    {$IFDEF MORMOT2}
                    JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                    {$ELSE}
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                    {$ENDIF}
                  else
                    JSONWriter.Add('"');
                  JSONWriter.AddNoJSONEscape(P, Min(StrLen(P), 19+(4*Ord(jcoMilliseconds in JSONComposeOptions))));
                  if jcoMongoISODate in JSONComposeOptions
                  then JSONWriter.AddShort('Z)"')
                  else JSONWriter.Add('"');
                end;
              else begin
                JSONWriter.Add('"');
                JSONWriter.AddJSONEscape(P);
                JSONWriter.Add('"');
              end;
            end;
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
{$ENDIF WITH_COLUMNS_TO_JSON}

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native SQLite plain driver.
  @param Statement a related SQL statement object.
  @param Handle a SQLite specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZSQLiteResultSet.Create(const Statement: IZStatement;
  const SQL: string; Psqlite3_stmt: PPsqlite3_stmt;
  PErrorCode: PInteger; UndefinedVarcharAsStringLength: Integer;
  ResetCallBack: TResetCallback; SQLiteIntAffinity: Boolean);
var Metadata: TContainedObject;
begin
  FSQLiteConnection := Statement.GetConnection as IZSQLiteConnection;
  FByteBuffer := FSQLiteConnection.GetByteBufferAddress;
  FPlainDriver := FSQLiteConnection.GetPlainDriver;
  if Assigned(FPlainDriver.sqlite3_column_table_name) and Assigned(FPlainDriver.sqlite3_column_name)
  then MetaData := TZSQLiteResultSetMetadata.Create(FSQLiteConnection.GetMetadata, SQL, Self)
  else MetaData := TZAbstractResultSetMetadata.Create(FSQLiteConnection.GetMetadata, SQL, Self);
  inherited Create(Statement, SQL, MetaData, FSQLiteConnection.GetConSettings);

  FPsqlite3_stmt := Psqlite3_stmt;
  Fsqlite3_stmt := Psqlite3_stmt^;
  FStmtErrorCode := PErrorCode;

  ResultSetConcurrency := rcReadOnly;
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FFirstRow := True;
  FResetCallBack := ResetCallBack;
  FSQLiteIntAffinity := SQLiteIntAffinity;

  Open;
end;

{**
  Opens this recordset.
}
procedure TZSQLiteResultSet.Open;
const
  NativeSQLite3Types: array[Boolean, SQLITE_INTEGER..SQLITE_NULL] of RawByteString =
    (('BIGINT','DOUBLE','CHAR','BLOB',''),
    ('BIGINT','DOUBLE','TEXT','BLOB',''));
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldPrecision: Integer;
  FieldDecimals: Integer;
  P: PAnsiChar;
  tmp: RawByteString;
  function ColAttributeToStr(P: PAnsichar): String;
  begin
    if P = nil then
      Result := ''
    else
      {$IFDEF UNICODE}
      Result := PRawToUnicode(P, ZFastCode.StrLen(P), zCP_UTF8);
      {$ELSE}
      Result := BufferToStr(P, ZFastCode.StrLen(P));
      {$ENDIF}
  end;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  FColumnCount := FPlainDriver.sqlite3_column_count(Fsqlite3_stmt);

  LastRowNo := 0;
  //MaxRows := FPlainDriver.data_count(Fsqlite3_stmt) +1; {first ResultSetRow = 1}

  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FColumnCount-1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do begin
      if Assigned(FPlainDriver.sqlite3_column_origin_name) then
        ColumnName := ColAttributeToStr(FPlainDriver.sqlite3_column_origin_name(Fsqlite3_stmt, i));
      ColumnLabel := ColAttributeToStr(FPlainDriver.sqlite3_column_name(Fsqlite3_stmt, i));
      if Assigned(FPlainDriver.sqlite3_column_table_name) then
        TableName := ColAttributeToStr(FPlainDriver.sqlite3_column_table_name(Fsqlite3_stmt, i));
      if Assigned(FPlainDriver.sqlite3_column_database_name) then
        CatalogName := ColAttributeToStr(FPlainDriver.sqlite3_column_database_name(Fsqlite3_stmt, i));
      ReadOnly := TableName <> '';
      P := FPlainDriver.sqlite3_column_decltype(Fsqlite3_stmt, i);
      if P = nil then begin
        tmp := NativeSQLite3Types[FUndefinedVarcharAsStringLength = 0][FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, i)]
      end else begin
        tmp := '';
        ZSetString(P, ZFastCode.StrLen(P), tmp);
      end;
      ColumnType := ConvertSQLiteTypeToSQLType(tmp, FUndefinedVarcharAsStringLength,
        FieldPrecision, FieldDecimals, FSQLiteIntAffinity);

      if ColumnType in [stString, stAsciiStream] then begin
        ColumnCodePage := zCP_UTF8;
        if ColumnType = stString then
          CharOctedLength := FieldPrecision shl 2;
      end else if ColumnType = stBytes then
        CharOctedLength := FieldPrecision;
      AutoIncrement := False;
      Precision := FieldPrecision;
      Scale := FieldDecimals;
      Writable := True;
      DefinitelyWritable := True;
      Signed := True;
      Nullable := ntNullable;  //sqlite just uses affinities .. all columns are nullable
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
  FCursorLocation := rctServer;
end;

{**
  Resets cursor position of this recordset and
  reset the prepared handles.
}
procedure TZSQLiteResultSet.ResetCursor;
begin
  if not Closed then begin
    FFirstRow := True;
    if Fsqlite3_stmt <> nil then begin
      FResetCallBack;
      Fsqlite3_stmt := nil;
    end;
    inherited ResetCursor;
  end;
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
  Result := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}) = SQLITE_NULL;
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
function TZSQLiteResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var ColType: Integer;
  I64: Int64;
  C: Currency absolute i64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  if ColType = SQLITE_NULL then begin
    LastWasNull := True;
    Result := nil;
    Len := 0;
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    LastWasNull := False;
    case ColType of
      SQLITE_INTEGER: begin
          i64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
          if ColumnType = stCurrency
          then CurrToRaw(C, '.', PAnsiChar(FByteBuffer), @Result)
          else IntToRaw(I64, PAnsiChar(FByteBuffer), @Result);
          Len := Result - PAnsiChar(FByteBuffer);
          Result := PAnsiChar(FByteBuffer);
        end;
      SQLITE_FLOAT: begin
          Result := PAnsiChar(FByteBuffer);
          Len := FloatToRaw(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex),
            Result);
        end;
      SQLITE3_TEXT: begin
          Result := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
          Len := ZFastCode.StrLen(Result);
        end;
      SQLITE_BLOB: begin
          Result := FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex);
          Len := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
        end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of UCS2 string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var
  ColType: Integer;
  i64: Int64;
  C: Currency absolute i64;
label set_From_tmp;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  if ColType = SQLITE_NULL then begin
    LastWasNull := True;
    Result := nil;
    Len := 0;
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    LastWasNull := False;
    case ColType of
      SQLITE_INTEGER: begin
          i64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
          if ColumnType = stCurrency
          then CurrToUnicode(C, '.', PWideChar(FByteBuffer), @Result)
          else IntToUnicode(I64, PWideChar(FByteBuffer), @Result);
          Len := Result - PWideChar(FByteBuffer);
          Result := PWideChar(FByteBuffer);
        end;
      SQLITE_FLOAT: begin
          Result := PWideChar(FByteBuffer);
          Len := FloatToUnicode(FPlainDriver.sqlite3_column_Double(Fsqlite3_stmt, ColumnIndex), Result);
        end;
      SQLITE3_TEXT: begin
          PAnsiChar(Result) := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
          FUniTemp := PRawToUnicode(PAnsiChar(Result), ZFastCode.StrLen(PAnsiChar(Result)), zCP_UTF8);
          goto set_From_tmp;
        end;
      SQLITE_BLOB: begin
          FUniTemp := Ascii7ToUnicodeString(FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex),
            FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex));
set_From_tmp:
          Len := Length(FUniTemp);
          if Len = 0
          then Result := PEmptyUnicodeString
          else Result := Pointer(FUniTemp);
        end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZSQLiteResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: PAnsiChar;
  Len: NativeUint;
begin
  P := GetPAnsiChar(ColumnIndex, Len);
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
  if P <> nil
  {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
  then ZSetString(P, Len, result)
  {$ELSE}
  then System.SetString(Result, P, Len)
  {$ENDIF}
  {$IFNDEF WITH_VAR_INIT_WARNING}
  else Result := '';
  {$ENDIF}
end;
{$ENDIF}

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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := False
  else case ColType of
    SQLITE_INTEGER:
      Result := FPlainDriver.sqlite3_column_int(Fsqlite3_stmt, ColumnIndex) <> 0;
    SQLITE_FLOAT:
      Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex) <> 0;
    SQLITE3_TEXT:
      Result := StrToBoolEx(FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex), True, False);
    else
      Result := False; {SQLITE_BLOB}
  end;
end;

{**
  Gets the address of value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len return the length of the addressed buffer
  @return the adressed column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) = SQLITE_NULL;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else begin
    Result :=  FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex);
    Len := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
  end;
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do case ColType of
    SQLITE_INTEGER: begin
                      Result := FPlainDriver.sqlite3_column_int(Fsqlite3_stmt, ColumnIndex);
                      if columnType = stCurrency then
                        Result := Result div 10000;
                    end;
    SQLITE_FLOAT:
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex));
    SQLITE3_TEXT:
      Result := RawToIntDef(FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex), 0);
    else
      Result := 0; {SQLITE_BLOB}
  end;
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
  I64: Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do case ColType of
    SQLITE_INTEGER: begin
                      i64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
                      if ColumnType = stCurrency
                      then Result := 164 div 10000
                      else Result := i64;
                    end;
    SQLITE_FLOAT:
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex));
    SQLITE3_TEXT:
      Result := RawToInt64Def(FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex), 0);
    else
      Result := 0; {SQLITE_BLOB}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLiteResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZSQLiteResultSet.GetULong(ColumnIndex: Integer): UInt64;
var ColType: Integer;
  i64: Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do case ColType of
    SQLITE_INTEGER: begin
                      i64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
                      if ColumnType = stCurrency
                      then Result := 164 div 10000
                      else Result := i64;
                    end;
    SQLITE_FLOAT:
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex));
    SQLITE3_TEXT:
      Result := RawToUInt64Def(FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex), 0);
    else
      Result := 0;
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
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
  Result := GetDouble(ColumnIndex);
end;

procedure TZSQLiteResultSet.GetGUID(ColumnIndex: Integer; var Result: TGUID);
var ColType, l: Integer;
  Buf: PAnsiChar;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then begin
Fill:FillChar(Result, SizeOf(TGUID), #0);
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do case ColType of
    SQLITE3_TEXT:    begin
                      Buf := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
                      L := ZFastCode.StrLen(Buf);
                      if (L = 36) or (L = 38)
                      then ZSysUtils.ValidGUIDToBinary(Buf, @Result.D1)
                      else goto Fill;
                    end;
    SQLITE_BLOB: begin
      L := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
      if L = SizeOf(TGUID) then begin
        Buf := FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex);
        Move(Buf^, Result.D1, SizeOf(TGUID));
      end else
        goto Fill;
    end;
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
function TZSQLiteResultSet.GetDouble(ColumnIndex: Integer): Double;
var ColType: Integer;
  I64: Int64;
  C: Currency absolute i64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do
    case ColType of
    SQLITE_INTEGER: begin
                      i64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
                      if ColumnType = stCurrency
                      then Result := c
                      else Result := i64;
                  end;
    SQLITE_FLOAT:   Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex);
    SQLITE3_TEXT:    SQLStrToFloatDef(FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex), 0, Result);
    else Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.
  the encoding is the encoding of the OS

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZSQLiteResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var P: PAnsiChar;
  L: NativeUInt;
begin
  P := GetPAnsiChar(ColumnIndex, L);
  if LastWasNull then
    Result := ''
  else if (FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) <> SQLITE3_TEXT) or
          (ZOSCodePage = zCP_UTF8) then
    System.SetString(Result, P, L)
  else begin
    FUniTemp := PRawToUnicode(P, ZFastCode.StrLen(P), zCP_UTF8);
    Result := ZUnicodeToRaw(FUniTemp, ZOSCodePage);
  end
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
const BCDScales: array[Boolean] of Byte = (0,4);
procedure TZSQLiteResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var ColType, l: Integer;
  Buf: PAnsiChar;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then begin
Fill:FillChar(Result, SizeOf(TBCD), #0);
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex]) do case ColType of
    SQLITE_INTEGER: ScaledOrdinal2BCD(FPlainDriver.sqlite3_column_Int64(Fsqlite3_stmt, ColumnIndex), BCDScales[ColumnType = stCurrency], Result);
    SQLITE_FLOAT:   ZSysUtils.Double2BCD(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex), Result);
    SQLITE3_TEXT:    begin
                      Buf := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
                      if not TryRawToBcd(Buf, ZFastCode.StrLen(Buf), Result, '.') then begin
                        LastWasNull := True;
                        goto Fill;
                      end;
                    end;
    else begin
      Buf := FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex);
      L := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
      if (ColumnType = stBigDecimal) and (L = SizeOf(TBCD))
      then Move(Buf^, Result, SizeOf(TBCD))
      else goto Fill;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZSQLiteResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var ColType: Integer;
  P: PAnsiChar;
  I64: Int64 absolute Result;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else case ColType of
    SQLITE_INTEGER: I64 := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
    SQLITE_FLOAT:   Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex);
    SQLITE3_TEXT:   begin
                      P := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
                      SQLStrToFloatDef(P, 0, FCurrDecimalSep, Result, ZFastcode.StrLen(P));
                    end;
    else Result := 0;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZSQLiteResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: Cardinal;
Label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    goto Fill
  else case ColType of
    SQLITE_INTEGER, SQLITE_FLOAT:
      DecodeDateTimeToDate(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch, Result);
    else begin
      Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
      Len := ZFastCode.StrLen(Buffer);
      LastWasNull := not TryPCharToDate(Buffer, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then
Fill:   PInt64(@Result.Year)^ := 0;
    end;
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
procedure TZSQLiteResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: Cardinal;
Label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    goto Fill
  else case ColType of
    SQLITE_INTEGER, SQLITE_FLOAT:
      DecodeDateTimeToTime(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch, Result);
    else begin
      Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
      Len := ZFastCode.StrLen(Buffer);
      LastWasNull := not TryPCharToTime(Buffer, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then begin
Fill:   PCardinal(@Result.Hour)^ := 0;
        PInt64(@Result.Second)^ := 0;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTimestamp</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero</code>
  @exception SQLException if a database access error occurs
}
procedure TZSQLiteResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
var
  ColType: Integer;
  Buffer: PAnsiChar;
  Len: LengthInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    goto Fill
  else case ColType of
    SQLITE_INTEGER,
    SQLITE_FLOAT:
      DecodeDateTimeToTimeStamp(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch, Result);
    else begin
      Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
      Len := StrLen(Buffer);
      LastWasNull := not TryPCharToTimeStamp(Buffer, Len, ConSettings^.ReadFormatSettings, Result);
      if LastWasNull then
Fill:  FillChar(Result, SizeOf(TZTimeStamp), #0);
    end;
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
function TZSQLiteResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  ColType: Integer;
  Buffer: PAnsiChar;
  L: NativeUInt;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if not LastWasNull then
    case ColType of
      SQLITE_BLOB: begin
          Buffer := FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt,ColumnIndex);
          L := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
          Result := TZLocalMemBLob.CreateWithData(Buffer, L);
        end;
      SQLITE3_TEXT: begin
        Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
        L := ZFastCode.StrLen(Buffer);
        Result := TZLocalMemCLob.CreateWithData(Buffer, L, zCP_UTF8, ConSettings);
      end;
      SQLITE_NULL: ;
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
        TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType);
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
var ErrorCode: Integer;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed then exit;
  if FFirstRow then begin
    ErrorCode := FStmtErrorCode^;
    Fsqlite3_stmt := FPsqlite3_stmt^;
  end else begin
    if ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then //previously set by stmt or Next
      Exit;
    ErrorCode := FPlainDriver.sqlite3_Step(Fsqlite3_stmt);
    if not (ErrorCode in [SQLITE_OK, SQLITE_ROW, SQLITE_DONE]) then
      FSQLiteConnection.HandleErrorOrWarning(lcFetch, ErrorCode, 'FETCH',
        IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;

  if FFirstRow then begin//avoid incrementing issue on fetching since the first row is allready fetched by stmt
    FFirstRow := False;
    Result := (ErrorCode = SQLITE_ROW);
    RowNo := 1;
    LastRowNo := Ord(Result);
  end else if (ErrorCode = SQLITE_ROW) then begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end else begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;

  { Free handle when EOF. }
  if not Result and Assigned(Fsqlite3_stmt) then begin
    FResetCallBack;
    Fsqlite3_stmt := nil;
    if not LastRowFetchLogged and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
  end;
end;

{ TZSQLiteCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZSQLiteCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex) in [stUnknown, stBinaryStream]);
end;

{**
  Creates a SQLite specific cached resolver object.
  @param PlainDriver a native SQLite plain driver.
  @param Handle a SQLite specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZSQLiteCachedResolver.Create(
  Handle: Psqlite; const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := TZSQLitePlainDriver(Statement.GetConnection.GetIZPlainDriver.GetInstance);
  FHandle := Handle;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := 0;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX} - 1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stSmall, stLongWord,
        stInteger, stUlong, stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZSQLiteCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
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
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; const
  OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
begin
  inherited;

  if (FAutoColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF} 0) and
     (OldRowAccessor.IsNull(FAutoColumnIndex) or (OldRowAccessor.GetValue(FAutoColumnIndex).VInteger = 0)) then
    NewRowAccessor.SetLong(FAutoColumnIndex, FPlainDriver.sqlite3_last_insert_rowid(FHandle));
end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.
