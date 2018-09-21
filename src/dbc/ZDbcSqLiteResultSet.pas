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
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
    System.Types, System.Contnrs
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS} Contnrs{$ELSE}ZClasses{$ENDIF}
  {$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZPlainSqLiteDriver,
  ZCompatibility, ZDbcCache, ZDbcCachedResultSet, ZDbcGenericResolver,
  ZSelectSchema;

type
  {** Implements SQLite ResultSet Metadata. }

  { TZSQLiteResultSetMetadata }

  TZSQLiteResultSetMetadata = class(TZAbstractResultSetMetadata)
  private
    FHas_ExtendedColumnInfos: Boolean;
  protected
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
  TZSQLiteResultSet = class(TZAbstractResultSet)
  private
    FStmtErrorCode: PInteger;
    FPsqlite: PPsqlite;
    FPsqlite3_stmt: PPsqlite3_stmt;
    Fsqlite3_stmt: Psqlite3_stmt;
    FColumnCount: Integer;
    FPlainDriver: TZSQLitePlainDriver;
    FFirstRow: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FResetCallBack: TResetCallBack;
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement;
      const SQL: string; Psqlite: PPsqlite; Psqlite3_stmt: PPsqlite3_stmt;
      PErrorCode: PInteger; UndefinedVarcharAsStringLength: Integer;
      ResetCallBack: TResetCallback);

    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
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

    function Next: Boolean; override;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions); override;
    {$ENDIF USE_SYNCOMMONS}
  end;

  {** Implements a cached resolver with SQLite specific functionality. }
  TZSQLiteCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: Psqlite;
    FPlainDriver: TZSQLitePlainDriver;
    FAutoColumnIndex: Integer;
  public
    constructor Create(Handle: Psqlite;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    function FormCalculateStatement(Columns: TObjectList): string; override;

    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
  end;

implementation

uses
  ZMessages, ZDbcSQLiteUtils, ZEncoding, ZDbcLogging, ZFastCode,
  ZVariant, ZDbcMetadata {$IFNDEF NO_UNIT_CONTNRS},ZClasses{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

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
begin
  if not FHas_ExtendedColumnInfos
  then inherited LoadColumns
  else if Metadata.GetConnection.GetDriver.GetStatementAnalyser.DefineSelectSchemaFromQuery(Metadata.GetConnection.GetDriver.GetTokenizer, SQL) <> nil then
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
end;

{ TZSQLiteResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZSQLiteResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var
  C, H, I, ColType: Integer;
  P: PAnsiChar;
label ProcBts;
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
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[i]);
      case ColType of
        SQLITE_BLOB: JSONWriter.WrBase64(FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt,C),
                        FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, C), True);
        SQLITE_INTEGER:
          case TZColumnInfo(ColumnsInfo[c]).ColumnType of
            stBoolean: JSONWriter.AddShort(JSONBool[FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, C) <> 0]);
            {stTime, stDate, stTimeStamp:
              todo: add implementation for unix timestamp
              JSONWriter.Add(FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, C));}
            else
              JSONWriter.Add(FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, C));
          end;
        SQLITE_FLOAT:
          case TZColumnInfo(ColumnsInfo[c]).ColumnType of
            stBoolean: JSONWriter.AddShort(JSONBool[FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C) <> 0]);
            stTime: begin
                if jcoMongoISODate in JSONComposeOptions then
                  JSONWriter.AddShort('ISODate("0000-00-00')
                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                else
                  JSONWriter.Add('"');
                JSONWriter.AddDateTime(Frac(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C)+JulianEpoch), jcoMilliseconds in JSONComposeOptions);
                JSONWriter.Add('"');
              end;
            stDate: begin
                if jcoMongoISODate in JSONComposeOptions then
                  JSONWriter.AddShort('ISODate("')
                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                else
                  JSONWriter.Add('"');
                JSONWriter.AddDateTime(Int(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C)+JulianEpoch));
                JSONWriter.Add('"');
              end;
            stTimeStamp: begin
                if jcoMongoISODate in JSONComposeOptions then
                  JSONWriter.AddShort('ISODate("')
                else if jcoDATETIME_MAGIC in JSONComposeOptions then
                  JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                else
                  JSONWriter.Add('"');
                JSONWriter.AddDateTime(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C)+JulianEpoch, jcoMilliseconds in JSONComposeOptions);
                JSONWriter.Add('"');
              end;
            else
              JSONWriter.AddDouble(FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, C));
          end;
        SQLITE3_TEXT: begin
            P := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, C);
            case TZColumnInfo(ColumnsInfo[c]).ColumnType of
              stBoolean: JSONWriter.AddShort(JSONBool[StrToBoolEx(P)]);
              stTime: begin
                  if jcoMongoISODate in JSONComposeOptions then
                    JSONWriter.AddShort('ISODate("0000-00-00')
                  else if jcoDATETIME_MAGIC in JSONComposeOptions then
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
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
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
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
                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
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
{$ENDIF USE_SYNCOMMONS}

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
  const SQL: string; Psqlite: PPsqlite; Psqlite3_stmt: PPsqlite3_stmt;
  PErrorCode: PInteger; UndefinedVarcharAsStringLength: Integer;
  ResetCallBack: TResetCallback);
begin
  inherited Create(Statement, SQL, TZSQLiteResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FPsqlite := Psqlite;
  FPsqlite3_stmt := Psqlite3_stmt;
  Fsqlite3_stmt := Psqlite3_stmt^;
  FStmtErrorCode := PErrorCode;

  FPlainDriver := TZSQLitePlainDriver(Statement.GetConnection.GetIZPlainDriver.GetInstance);
  ResultSetConcurrency := rcReadOnly;
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FFirstRow := True;
  FResetCallBack := ResetCallBack;

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
      Result := PRawToUnicode(P, ZFastCode.StrLen(P), ConSettings^.ClientCodePage^.CP);
      {$ELSE}
      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
        Result := BufferToStr(P, ZFastCode.StrLen(P))
      else
        Result := ZUnicodeToString(PRawToUnicode(P, ZFastCode.StrLen(P), ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
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
      if P = nil then
        tmp := NativeSQLite3Types[FUndefinedVarcharAsStringLength = 0][FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, i)]
      else
        ZSetString(P, ZFastCode.StrLen(P), tmp);
      ColumnType := ConvertSQLiteTypeToSQLType(tmp, FUndefinedVarcharAsStringLength,
        FieldPrecision, FieldDecimals, ConSettings.CPType);

      if ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnCodePage := zCP_UTF8;
        if ColumnType = stString then begin
          ColumnDisplaySize := FieldPrecision;
          CharOctedLength := FieldPrecision shl 2;
          Precision := FieldPrecision;
        end else if ColumnType = stUnicodeString then begin
          ColumnDisplaySize := FieldPrecision;
          CharOctedLength := FieldPrecision shl 1;
          Precision := FieldPrecision;
        end;
      end else
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
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);
  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
    if ColType <> SQLITE_BLOB then
    begin
      Result := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
      Len := ZFastCode.StrLen(Result);
    end
    else
    begin
      Result := FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex);
      Len := FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex);
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
function TZSQLiteResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF});
  LastWasNull := Result = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZSQLiteResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: PAnsiChar;
  Len: NativeUint;
begin //rewritten because of performance reasons to avoid localized the RBS before
  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}) = SQLITE_NULL;
  if LastWasNull then
    Result := ''
  else
  begin
    P := GetPAnsiChar(ColumnIndex, Len);
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    ZSetString(P, Len, result);
    {$ELSE}
    System.SetString(Result, P, Len);
    {$ENDIF}
  end;
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
  Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := False
  else
    case ColType of
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
  else
    case ColType of
      SQLITE_INTEGER:
        Result := FPlainDriver.sqlite3_column_int(Fsqlite3_stmt, ColumnIndex);
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
  else
    case ColType of
      SQLITE_INTEGER:
        Result := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
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
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZSQLiteResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  ColType: Integer;
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
  else
    case ColType of
      SQLITE_INTEGER:
        Result := FPlainDriver.sqlite3_column_int64(Fsqlite3_stmt, ColumnIndex);
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
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex);
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}

  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex);
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}

  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    { sqlite does the conversion if required
      http://www.sqlite.org/c3ref/column_blob.html }
     Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex);
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
function TZSQLiteResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}

  LastWasNull := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex) = SQLITE_NULL;
  if LastWasNull then
    Result := nil
  else
    Result :=  BufferToBytes(FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt, ColumnIndex), FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex));
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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER, SQLITE_FLOAT:
        Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch;
      else
      begin
        Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
        Len := ZFastCode.StrLen(Buffer);

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
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if LastWasNull then
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER, SQLITE_FLOAT:
        Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch;
      else
      begin
        Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
        Len := ZFastCode.StrLen(Buffer);

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
  Failed: Boolean;
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
    Result := 0
  else
    case ColType of
      SQLITE_INTEGER,
      SQLITE_FLOAT:
        Result := FPlainDriver.sqlite3_column_double(Fsqlite3_stmt, ColumnIndex)+JulianEpoch;
      else
      begin
        Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
        Result := RawSQLTimeStampToDateTime(Buffer, ZFastCode.StrLen(Buffer), ConSettings^.ReadFormatSettings, Failed);
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
  Buffer: PAnsiChar;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ColType := FPlainDriver.sqlite3_column_type(Fsqlite3_stmt, ColumnIndex);

  LastWasNull := ColType = SQLITE_NULL;
  if not LastWasNull then
    if ColType = SQLITE_BLOB then
      Result := TZAbstractBlob.CreateWithData(FPlainDriver.sqlite3_column_blob(Fsqlite3_stmt,ColumnIndex),
        FPlainDriver.sqlite3_column_bytes(Fsqlite3_stmt, ColumnIndex))
    else begin
      Buffer := FPlainDriver.sqlite3_column_text(Fsqlite3_stmt, ColumnIndex);
      Result := TZAbstractClob.CreateWithData( Buffer,
        ZFastCode.StrLen(Buffer), zCP_UTF8, ConSettings);
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
      CheckSQLiteError(FPlainDriver, Fsqlite3_stmt, ErrorCode, lcOther, 'FETCH', ConSettings);
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
  end;
end;

{ TZSQLiteCachedResolver }

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
  begin
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stSmall, stLongWord,
        stInteger, stUlong, stLong]) then
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
procedure TZSQLiteCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
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
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
begin
  inherited;

  if (FAutoColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF} 0) and
     (OldRowAccessor.IsNull(FAutoColumnIndex) or (OldRowAccessor.GetValue(FAutoColumnIndex).VInteger = 0)) then
  begin
    NewRowAccessor.SetLong(FAutoColumnIndex, FPlainDriver.sqlite3_last_insert_rowid(FHandle));
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
